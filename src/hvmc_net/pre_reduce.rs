use crate::{diagnostics::Diagnostics, term::Name};
use hvmc::{ast::Book, transform::pre_reduce::PreReduceStats};

const MAX_REWRITES_DEFAULT: u64 = 100;

pub fn pre_reduce(
  book: &mut Book,
  entrypoint: &str,
  max_rwts: Option<u64>,
  check_only: bool,
  diags: &mut Diagnostics,
) -> Result<(), Diagnostics> {
  diags.start_pass();

  let max_rwts = max_rwts.unwrap_or(MAX_REWRITES_DEFAULT);

  // It would be even better if we could also set a memory limit and
  // catch the cases where the limit is broken.
  // However, the allocator just panics, and catching it is a mess.
  // For now, we just choose a reasonable amount.
  // 100 nodes per max_rwts.
  let max_memory = max_rwts as usize * 8 * 1000;
  let max_memory = Some(max_memory);

  let orig_book = if check_only { Some(book.clone()) } else { None };

  let PreReduceStats { not_normal, .. } = book.pre_reduce(&|x| x == entrypoint, max_memory, max_rwts);

  for not_normal in not_normal {
    // TODO: Reverse the generated names to get actual function names.
    diags.add_rule_error(
      format!("Unable to normalize function {not_normal}, it is likely that it will loop in strict mode. If this function is doing heavy processing that you're sure will terminate, we recommend moving its data to the main function."),
      Name::new(not_normal),
    );
  }

  if let Some(orig_book) = orig_book {
    *book = orig_book;
  }

  diags.fatal(())
}
