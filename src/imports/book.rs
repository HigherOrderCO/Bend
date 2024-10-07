use super::{BindMap, ImportsMap, PackageLoader};
use crate::{
  diagnostics::{Diagnostics, DiagnosticsConfig},
  fun::{
    parser::ParseBook, Adt, AdtCtr, Book, Definition, HvmDefinition, Name, Pattern, Source, SourceKind, Term,
  },
  imp::{self, Expr, MatchArm, Stmt},
  imports::packages::Packages,
  maybe_grow,
};
use indexmap::{map::Entry, IndexMap};
use itertools::Itertools;

impl ParseBook {
  /// Loads and applies imports recursively to a ParseBook,
  /// transforming definitions and ADTs to a canonical name,
  /// and adding `use` binds so that names are accessible by their alias.
  ///
  /// # Details
  ///
  /// The process involves:
  ///
  /// 1. Loading imports recursively using the provided `loader`.
  /// 2. Transforming definitions and ADTs with naming transformations.
  /// 3. Adding binds for aliases and old names in their respective definitions.
  /// 4. Converting the ParseBook into its functional form.
  /// 5. Perform any necessary post-processing.
  pub fn load_imports(
    self,
    mut loader: impl PackageLoader,
    diag_config: DiagnosticsConfig,
  ) -> Result<Book, Diagnostics> {
    let diag = &mut Diagnostics::new(diag_config);
    let pkgs = &mut Packages::new(self);
    // Load all the imports recursively, saving them in `pkgs`.
    // `book` is the root book with the entry point.
    let mut book = pkgs.load_imports(&mut loader, diag)?;

    // Apply the imports to the book
    book.apply_imports(None, diag, pkgs)?;
    diag.fatal(())?;
    eprint!("{}", diag);

    // Convert the parse-level AST into the internal functional representation.
    let mut book = book.to_fun()?;

    // Process terms that contains constructors names and can't be updated by `desugar_use`.
    book.desugar_ctr_use();

    Ok(book)
  }

  /// Loads the imported books recursively into the importing book,
  /// then apply imported names or aliases binds to its definitions.
  fn apply_imports(
    &mut self,
    main_imports: Option<&ImportsMap>,
    diag: &mut Diagnostics,
    pkgs: &mut Packages,
  ) -> Result<(), Diagnostics> {
    self.load_packages(main_imports, diag, pkgs)?;
    self.apply_import_binds(main_imports, pkgs);
    Ok(())
  }

  /// Consumes the book imported packages,
  /// applying the imports recursively of every nested book.
  fn load_packages(
    &mut self,
    main_imports: Option<&ImportsMap>,
    diag: &mut Diagnostics,
    pkgs: &mut Packages,
  ) -> Result<(), Diagnostics> {
    let sources = self.import_ctx.sources().into_iter().cloned().collect_vec();

    for src in sources {
      let Some(package) = pkgs.books.swap_remove(&src) else { continue };
      let mut package = package.into_inner();

      // Can not be done outside the loop/function because of the borrow checker.
      // Just serves to pass only the import map of the first call to `apply_imports_go`.
      let main_imports = main_imports.unwrap_or(&self.import_ctx.map);

      package.apply_imports(Some(main_imports), diag, pkgs)?;

      // Rename ADTs and defs, applying binds from old names to new names
      package.apply_adts(&src, main_imports);
      package.apply_defs(&src, main_imports);

      let Book { defs, hvm_defs, adts, .. } = package.to_fun()?;

      // Add the ADTs to the importing book,
      // saving the constructors names to be used when applying ADTs binds.
      for (name, adt) in adts {
        let adts = pkgs.loaded_adts.entry(src.clone()).or_default();
        adts.insert(name.clone(), adt.ctrs.keys().cloned().collect_vec());
        self.add_imported_adt(name, adt, diag);
      }

      // The names on the indexmap are the original ones, so we ignore them
      for def in defs.into_values() {
        self.add_imported_def(def, diag);
      }

      // The names on the indexmap are the original ones, so we ignore them
      for def in hvm_defs.into_values() {
        self.add_imported_hvm_def(def, diag);
      }
    }
    Ok(())
  }

  /// Applies a chain of `use bind = src` to every local definition.
  ///
  /// Must be used after `load_packages`
  fn apply_import_binds(&mut self, main_imports: Option<&ImportsMap>, pkgs: &Packages) {
    // Can not be done outside the function because of the borrow checker.
    // Just serves to pass only the import map of the first call to `apply_imports_go`.
    let main_imports = main_imports.unwrap_or(&self.import_ctx.map);
    let mut local_imports = BindMap::new();
    let mut adt_imports = BindMap::new();

    // Collect local imports binds, starting with `__` if not imported by the main book.
    'outer: for (bind, src) in self.import_ctx.map.binds.iter().rev() {
      if self.contains_def(bind) | self.ctrs.contains_key(bind) | self.adts.contains_key(bind) {
        // TODO: Here we should show warnings for shadowing of imported names by local def/ctr/adt
        // It can be done, but when importing with `ImportType::Single` files in the same folder,
        // it gives a false positive warning
        continue;
      }

      let nam = if main_imports.contains_source(src) { src.clone() } else { Name::new(format!("__{}", src)) };

      // Checks if the bind is an loaded ADT name,
      // If so, add the constructors binds as `bind/ctr` instead.
      for pkg in self.import_ctx.sources() {
        if let Some(book) = pkgs.loaded_adts.get(pkg) {
          if let Some(ctrs) = book.get(&nam) {
            for ctr in ctrs.iter().rev() {
              let full_ctr_name = ctr.split("__").nth(1).unwrap_or(ctr.as_ref());
              let ctr_name = full_ctr_name.strip_prefix(src.as_ref()).unwrap();
              let bind = Name::new(format!("{}{}", bind, ctr_name));
              local_imports.insert(bind, ctr.clone());
            }
            // Add a mapping of the ADT name
            adt_imports.insert(bind.clone(), nam.clone());
            continue 'outer;
          }
        }
      }

      // Not a constructor, so just insert the bind.
      local_imports.insert(bind.clone(), nam);
    }

    for (_, def) in self.local_defs_mut() {
      def.apply_binds(true, &local_imports);
      def.apply_type_binds(&adt_imports);
    }
  }

  /// Applying the necessary naming transformations to the book ADTs,
  /// adding `use ctr = ctr_src` chains to every local definition and
  /// substituting the name of type ctrs for the canonical ones.
  fn apply_adts(&mut self, src: &Name, main_imports: &ImportsMap) {
    let adts = std::mem::take(&mut self.adts);
    let mut new_adts = IndexMap::new();
    let mut adts_map = vec![];
    let mut ctrs_map = IndexMap::new();
    let mut new_ctrs = IndexMap::new();

    // Rename the ADTs and constructors to their canonical name,
    // starting with `__` if not imported by the main book.
    for (mut name, mut adt) in adts {
      if adt.source.is_local() {
        adt.source.kind = SourceKind::Imported;
        let old_name = name.clone();
        name = Name::new(format!("{}/{}", src, name));

        let mangle_name = !main_imports.contains_source(&name);
        let mut mangle_adt_name = mangle_name;

        for (old_nam, ctr) in std::mem::take(&mut adt.ctrs) {
          let mut ctr_name = Name::new(format!("{}/{}", src, old_nam));

          let mangle_ctr = mangle_name && !main_imports.contains_source(&ctr_name);

          if mangle_ctr {
            mangle_adt_name = true;
            ctr_name = Name::new(format!("__{}", ctr_name));
          }

          let ctr = AdtCtr { name: ctr_name.clone(), ..ctr };
          new_ctrs.insert(ctr_name.clone(), name.clone());
          ctrs_map.insert(old_nam, ctr_name.clone());
          adt.ctrs.insert(ctr_name, ctr);
        }

        if mangle_adt_name {
          name = Name::new(format!("__{}", name));
        }

        adt.name = name.clone();
        adts_map.push((old_name, name.clone()));
      }

      new_adts.insert(name.clone(), adt);
    }

    // Apply the binds for the type constructors in the constructor types
    for (_, adt) in &mut new_adts {
      for (_, ctr) in &mut adt.ctrs {
        for (from, to) in &adts_map {
          ctr.typ.subst_ctr(from, to);
        }
      }
    }

    let adts_map = adts_map.into_iter().collect::<IndexMap<_, _>>();
    for (_, def) in self.local_defs_mut() {
      // Applies the binds for the new constructor names for every definition.
      def.apply_binds(true, &ctrs_map);

      // Apply the binds for the type constructors in the def types and in the `def` terms.
      def.apply_type_binds(&adts_map);
    }

    self.adts = new_adts;
    self.ctrs = new_ctrs;
  }

  /// Apply the necessary naming transformations to the book definitions,
  /// adding `use def = def_src` chains to every local definition.
  fn apply_defs(&mut self, src: &Name, main_imports: &ImportsMap) {
    let mut canonical_map: IndexMap<_, _> = IndexMap::new();

    // Rename the definitions to their canonical name
    // Starting with `__` if not imported by the main book.
    for (_, def) in self.local_defs_mut() {
      def.canonicalize_name(src, main_imports, &mut canonical_map);
    }

    // Applies the binds for the new names for every definition
    for (_, def) in self.local_defs_mut() {
      def.apply_binds(false, &canonical_map);
      def.source_mut().kind = SourceKind::Imported;
    }
  }
}

/// Helper functions
impl ParseBook {
  pub fn top_level_names(&self) -> impl Iterator<Item = &Name> {
    let imp_defs = self.imp_defs.keys();
    let fun_defs = self.fun_defs.keys();
    let hvm_defs = self.hvm_defs.keys();
    let adts = self.adts.keys();
    let ctrs = self.ctrs.keys();

    imp_defs.chain(fun_defs).chain(hvm_defs).chain(adts).chain(ctrs)
  }

  fn add_imported_adt(&mut self, nam: Name, adt: Adt, diag: &mut Diagnostics) {
    if self.adts.get(&nam).is_some() {
      let err = format!("The imported datatype '{nam}' conflicts with the datatype '{nam}'.");
      diag.add_book_error(err);
    } else {
      for ctr in adt.ctrs.keys() {
        if self.contains_def(ctr) {
          let err = format!("The imported constructor '{ctr}' conflicts with the definition '{ctr}'.");
          diag.add_book_error(err);
        }
        match self.ctrs.entry(ctr.clone()) {
          Entry::Vacant(e) => _ = e.insert(nam.clone()),
          Entry::Occupied(e) => {
            let ctr = e.key();
            let err = format!("The imported constructor '{ctr}' conflicts with the constructor '{ctr}'.");
            diag.add_book_error(err);
          }
        }
      }
      self.adts.insert(nam, adt);
    }
  }

  fn add_imported_def(&mut self, def: Definition, diag: &mut Diagnostics) {
    if !self.has_def_conflict(&def.name, diag) {
      self.fun_defs.insert(def.name.clone(), def);
    }
  }

  fn add_imported_hvm_def(&mut self, def: HvmDefinition, diag: &mut Diagnostics) {
    if !self.has_def_conflict(&def.name, diag) {
      self.hvm_defs.insert(def.name.clone(), def);
    }
  }

  fn has_def_conflict(&mut self, name: &Name, diag: &mut Diagnostics) -> bool {
    if self.contains_def(name) {
      let err = format!("The imported definition '{name}' conflicts with the definition '{name}'.");
      diag.add_book_error(err);
      true
    } else if self.ctrs.contains_key(name) {
      let err = format!("The imported definition '{name}' conflicts with the constructor '{name}'.");
      diag.add_book_error(err);
      true
    } else {
      false
    }
  }

  fn local_defs_mut(&mut self) -> impl Iterator<Item = (&Name, &mut dyn Def)> {
    let fun = self.fun_defs.iter_mut().map(|(nam, def)| (nam, def as &mut dyn Def));
    let imp = self.imp_defs.iter_mut().map(|(nam, def)| (nam, def as &mut dyn Def));
    let hvm = self.hvm_defs.iter_mut().map(|(nam, def)| (nam, def as &mut dyn Def));
    fun.chain(imp).chain(hvm).filter(|(_, def)| def.source().is_local())
  }
}

/// Common functions for the different definition types
trait Def {
  fn canonicalize_name(&mut self, src: &Name, main_imports: &ImportsMap, binds: &mut BindMap) {
    let def_name = self.name_mut();
    let mut new_name = Name::new(format!("{}/{}", src, def_name));

    if !main_imports.contains_source(&new_name) {
      new_name = Name::new(format!("__{}", new_name));
    }

    binds.insert(def_name.clone(), new_name.clone());
    *def_name = new_name;
  }

  /// Applies the binds for definition names by placing `use` terms.
  ///
  /// If we know that the bind map doesn't contain any constructor names,
  /// we skip renaming rule patterns.
  fn apply_binds(&mut self, maybe_constructor: bool, binds: &BindMap);

  fn apply_type_binds(&mut self, binds: &BindMap);

  fn source(&self) -> &Source;
  fn source_mut(&mut self) -> &mut Source;
  fn name_mut(&mut self) -> &mut Name;
}

impl Def for Definition {
  fn apply_binds(&mut self, maybe_constructor: bool, binds: &BindMap) {
    fn rename_ctr_pattern(pat: &mut Pattern, binds: &BindMap) {
      for pat in pat.children_mut() {
        rename_ctr_pattern(pat, binds);
      }
      match pat {
        Pattern::Ctr(nam, _) => {
          if let Some(alias) = binds.get(nam) {
            *nam = alias.clone();
          }
        }
        Pattern::Var(Some(nam)) => {
          if let Some(alias) = binds.get(nam) {
            *nam = alias.clone();
          }
        }
        _ => {}
      }
    }

    for rule in &mut self.rules {
      if maybe_constructor {
        for pat in &mut rule.pats {
          rename_ctr_pattern(pat, binds);
        }
      }
      let bod = std::mem::take(&mut rule.body);
      rule.body = bod.fold_uses(binds.iter().rev());
    }
  }

  fn apply_type_binds(&mut self, binds: &BindMap) {
    for (from, to) in binds.iter().rev() {
      self.typ.subst_ctr(from, to);
      for rule in &mut self.rules {
        rule.body.subst_type_ctrs(from, to);
      }
    }
  }

  fn source(&self) -> &Source {
    &self.source
  }

  fn source_mut(&mut self) -> &mut Source {
    &mut self.source
  }

  fn name_mut(&mut self) -> &mut Name {
    &mut self.name
  }
}

impl Def for imp::Definition {
  fn apply_binds(&mut self, _maybe_constructor: bool, binds: &BindMap) {
    let bod = std::mem::take(&mut self.body);
    self.body = bod.fold_uses(binds.iter().rev());
  }

  fn apply_type_binds(&mut self, binds: &BindMap) {
    fn subst_type_ctrs_stmt(stmt: &mut Stmt, from: &Name, to: &Name) {
      maybe_grow(|| match stmt {
        Stmt::Assign { nxt, .. } => {
          if let Some(nxt) = nxt {
            subst_type_ctrs_stmt(nxt, from, to);
          }
        }
        Stmt::InPlace { nxt, .. } => {
          subst_type_ctrs_stmt(nxt, from, to);
        }
        Stmt::If { then, otherwise, nxt, .. } => {
          subst_type_ctrs_stmt(then, from, to);
          subst_type_ctrs_stmt(otherwise, from, to);
          if let Some(nxt) = nxt {
            subst_type_ctrs_stmt(nxt, from, to);
          }
        }
        Stmt::Match { arms, nxt, .. } => {
          for MatchArm { lft: _, rgt } in arms {
            subst_type_ctrs_stmt(rgt, from, to);
          }
          if let Some(nxt) = nxt {
            subst_type_ctrs_stmt(nxt, from, to);
          }
        }
        Stmt::Switch { arms, nxt, .. } => {
          for arm in arms {
            subst_type_ctrs_stmt(arm, from, to);
          }
          if let Some(nxt) = nxt {
            subst_type_ctrs_stmt(nxt, from, to);
          }
        }
        Stmt::Bend { step, base, nxt, .. } => {
          subst_type_ctrs_stmt(step, from, to);
          subst_type_ctrs_stmt(base, from, to);
          if let Some(nxt) = nxt {
            subst_type_ctrs_stmt(nxt, from, to);
          }
        }
        Stmt::Fold { arms, nxt, .. } => {
          for MatchArm { lft: _, rgt } in arms {
            subst_type_ctrs_stmt(rgt, from, to);
          }
          if let Some(nxt) = nxt {
            subst_type_ctrs_stmt(nxt, from, to);
          }
        }
        Stmt::With { typ, bod, nxt } => {
          if typ == from {
            *typ = to.clone();
          }
          subst_type_ctrs_stmt(bod, from, to);
          if let Some(nxt) = nxt {
            subst_type_ctrs_stmt(nxt, from, to);
          }
        }
        Stmt::Ask { nxt, .. } => {
          if let Some(nxt) = nxt {
            subst_type_ctrs_stmt(nxt, from, to);
          }
        }
        Stmt::Return { .. } => {}
        Stmt::Open { typ, nxt, .. } => {
          if typ == from {
            *typ = to.clone();
          }
          subst_type_ctrs_stmt(nxt, from, to);
        }
        Stmt::Use { nxt, .. } => {
          subst_type_ctrs_stmt(nxt, from, to);
        }
        Stmt::LocalDef { def, nxt } => {
          def.apply_type_binds(&[(from.clone(), to.clone())].into_iter().collect());
          subst_type_ctrs_stmt(nxt, from, to);
        }
        Stmt::Err => {}
      })
    }
    for (from, to) in binds.iter().rev() {
      self.typ.subst_ctr(from, to);
      subst_type_ctrs_stmt(&mut self.body, from, to);
    }
  }

  fn source(&self) -> &Source {
    &self.source
  }

  fn source_mut(&mut self) -> &mut Source {
    &mut self.source
  }

  fn name_mut(&mut self) -> &mut Name {
    &mut self.name
  }
}

impl Def for HvmDefinition {
  /// Do nothing, can not apply binds to a HvmDefinition.
  fn apply_binds(&mut self, _maybe_constructor: bool, _binds: &BindMap) {}

  fn apply_type_binds(&mut self, binds: &BindMap) {
    for (from, to) in binds.iter().rev() {
      self.typ.subst_ctr(from, to);
    }
  }

  fn source(&self) -> &Source {
    &self.source
  }

  fn source_mut(&mut self) -> &mut Source {
    &mut self.source
  }

  fn name_mut(&mut self) -> &mut Name {
    &mut self.name
  }

  fn canonicalize_name(&mut self, src: &Name, main_imports: &ImportsMap, binds: &mut BindMap) {
    let def_name = self.name_mut();
    let mut new_name = Name::new(std::format!("{}/{}", src, def_name));

    if !main_imports.contains_source(&new_name) {
      new_name = Name::new(std::format!("__{}", new_name));
    }

    binds.insert(def_name.clone(), new_name.clone());
    *def_name = new_name;
  }
}

impl Term {
  fn fold_uses<'a>(self, map: impl Iterator<Item = (&'a Name, &'a Name)>) -> Self {
    map.fold(self, |acc, (bind, nam)| Self::Use {
      nam: Some(bind.clone()),
      val: Box::new(Self::Var { nam: nam.clone() }),
      nxt: Box::new(acc),
    })
  }
}

impl Stmt {
  fn fold_uses<'a>(self, map: impl Iterator<Item = (&'a Name, &'a Name)>) -> Self {
    map.fold(self, |acc, (bind, nam)| Self::Use {
      nam: bind.clone(),
      val: Box::new(Expr::Var { nam: nam.clone() }),
      nxt: Box::new(acc),
    })
  }
}
