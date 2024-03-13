use std::sync::{Arc, Mutex};

use hvmc::{
  ast, dispatch_dyn_net,
  host::Host,
  run::{LabSet, Port, Trg, Wire},
  stdlib::{ArcDef, HostedDef, IDENTITY},
};
use term_to_net::term_to_compat_net;

use crate::{
  builtins::util::{AsDefFunction, FunctionLike, FunctionLikeHosted},
  net::net_to_hvmc::net_to_hvmc,
  readback_hvmc,
  term::{
    term_to_net::{self, Labels},
    AdtEncoding, Book, Term,
  },
};

#[derive(Clone)]
struct ReadbackData {
  book: Arc<Book>,
  host: Arc<Mutex<Host>>,
  labels: Arc<Labels>,
  adt_encoding: AdtEncoding,
}

/// Adds the filesystem definitions (`HVM.store` and `HVM.load`) to the book
pub(crate) fn add_fs_defs(
  book: Arc<Book>,
  host: Arc<Mutex<Host>>,
  labels: Arc<Labels>,
  adt_encoding: AdtEncoding,
) {
  #[derive(Clone)]
  struct Fs0 {
    readback_data: ReadbackData,
    save: bool,
  }
  impl AsDefFunction for Fs0 {
    fn call<M: hvmc::run::Mode>(&self, net: &mut hvmc::run::Net<M>, input: Wire, output: Wire) {
      let (wire, port) = net.create_wire();
      let slf = self.clone();
      let readback_node = hvmc::stdlib::readback(slf.readback_data.host.clone(), port, move |net, tree| {
        dispatch_dyn_net!(net => {
          net.link_wire_port(wire, Port::ERA);
          let (term, _errs) = readback_hvmc(&ast::Net { root: tree,redexes: vec![]} , &slf.readback_data.book, &slf.readback_data.labels, false, slf.readback_data.adt_encoding);
          let filename = if let Term::Str { ref val } = term {
            Some(val.to_string())
          } else {
            None
          };
          net.link_wire_port(output, ArcDef::new_arc_port(LabSet::ALL, FunctionLike(Fs1 { readback_data: slf.readback_data, save: slf.save, filename  })));
        })
      });
      net.link_wire_port(input, readback_node);
    }
  }

  #[derive(Clone)]
  struct Fs1 {
    readback_data: ReadbackData,
    save: bool,
    filename: Option<String>,
  }
  impl AsDefFunction for Fs1 {
    fn call<M: hvmc::run::Mode>(&self, net: &mut hvmc::run::Net<M>, input: Wire, output: Wire) {
      if self.save {
        let (wire, port) = net.create_wire();
        let slf = self.clone();
        let readback_node = hvmc::stdlib::readback(
          self.readback_data.host.clone(),
          port,
          move |net, tree| {
            dispatch_dyn_net!(net => {
              let (term, _errs) = readback_hvmc(&ast::Net { root: tree,redexes: vec![]} , &slf.readback_data.book, &slf.readback_data.labels, false, slf.readback_data.adt_encoding);
              let contents = if let Term::Str { ref val } = term {
                Some(val.to_string())
              } else {
                None
              };
              // Save file
              if let (Some(filename), Some(contents)) = (slf.filename, contents) {
                let _ = std::fs::write(filename, contents);
              }
              net.link_wire_port(wire, Port::ERA);
              net.link_wire_port(output, Port::new_ref(unsafe { IDENTITY.as_ref().unwrap() }));
            })
          },
        );
        net.link_wire_port(input, readback_node);
      } else {
        let app = net.create_node(hvmc::run::Tag::Ctr, 0);
        let contents = self
          .filename
          .clone()
          .map(|filename| std::fs::read(filename).ok())
          .flatten()
          .map(|x| std::str::from_utf8(&x).map(|x| x.to_string()).ok())
          .flatten()
          .unwrap_or(String::from(""));
        let contents = Term::encode_str(&contents);
        let mut labels = (*self.readback_data.labels).clone();
        let contents = term_to_compat_net(&contents, &mut labels);
        if let Ok(contents) = net_to_hvmc(&contents) {
          self.readback_data.host.lock().unwrap().encode_net(net, Trg::port(app.p1), &contents);
        } else {
          net.link_port_port(Port::ERA, app.p1);
        }
        net.link_wire_port(output, app.p2);
        net.link_wire_port(input, app.p0);
      }
    }
  }
  let readback_data = ReadbackData { book, host: host.clone(), labels, adt_encoding };
  host.lock().unwrap().insert_def("HVM.store", unsafe {
    HostedDef::new_hosted(
      LabSet::ALL,
      FunctionLikeHosted(Fs0 { readback_data: readback_data.clone(), save: true }),
    )
  });
  host.lock().unwrap().insert_def("HVM.load", unsafe {
    HostedDef::new_hosted(
      LabSet::ALL,
      FunctionLikeHosted(Fs0 { readback_data: readback_data.clone(), save: false }),
    )
  });
}
