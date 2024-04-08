use crate::{
  diagnostics::{Diagnostics, WarningType},
  term::display::DisplayJoin,
};
use hvmc::{ast::Book, transform::pre_reduce::PreReduceStats};

pub const MAX_REWRITES_DEFAULT: u64 = 100_000;

pub fn pre_reduce(
  book: &mut Book,
  entrypoint: &str,
  max_rewrites: u64,
  max_memory: Option<usize>,
  check_only: bool,
  diags: &mut Diagnostics,
) -> Result<(), Diagnostics> {
  diags.start_pass();

  // It would be even better if we could also set a memory limit and
  // catch the cases where the limit is broken.
  // However, the allocator just panics, and catching it is a mess.
  // For now, we just choose a reasonable amount.
  // 1000 nodes per max_rwts (800MB for 100k reductions).
  let max_memory = max_memory.or(Some(max_rewrites as usize * 8 * 1000));

  let orig_book = if check_only { Some(book.clone()) } else { None };

  let PreReduceStats { not_normal, .. } = book.pre_reduce(&|x| x == entrypoint, max_memory, max_rewrites);

  if !not_normal.is_empty() {
    let msg = format!(
      include_str!("pre_reduce.message"),
      // TODO: Reverse the generated names to get actual function names.
      not_normal = DisplayJoin(|| &not_normal, ", "),
      max_rewrites = max_rewrites
    );
    diags.add_book_warning(msg, WarningType::RecursionPreReduce);
  }

  if let Some(orig_book) = orig_book {
    *book = orig_book;
  }

  diags.fatal(())
}
