use crate::{
  fun::{
    display::DisplayFn, Adt, AdtCtr, Adts, Constructors, CtrField, FanKind, HvmDefinition, HvmDefinitions,
    MatchRule, Name, Num, Op, Pattern, Rule, Source, SourceKind, Tag, Term, Type, STRINGS,
  },
  imp::parser::ImpParser,
  imports::{Import, ImportCtx, ImportType},
  maybe_grow,
};
use highlight_error::highlight_error;
use indexmap::IndexMap;
use itertools::Itertools;
use std::ops::Range;
use TSPL::{ParseError, Parser};

type FunDefinition = super::Definition;
type ImpDefinition = crate::imp::Definition;

/// Intermediate representation of a program.
#[derive(Debug, Clone, Default)]
pub struct ParseBook {
  /// The `functional` function definitions.
  pub fun_defs: IndexMap<Name, FunDefinition>,

  /// The `imperative` function definitions.
  pub imp_defs: IndexMap<Name, ImpDefinition>,

  /// HVM native function definitions.
  pub hvm_defs: HvmDefinitions,

  /// The algebraic datatypes defined by the program
  pub adts: Adts,

  /// To which type does each constructor belong to.
  pub ctrs: Constructors,

  /// Imported packages to be loaded in the program
  pub import_ctx: ImportCtx,

  /// File path that the book was loaded from.
  pub source: Name,
}

impl ParseBook {
  pub fn contains_def(&self, name: &Name) -> bool {
    self.fun_defs.contains_key(name) || self.imp_defs.contains_key(name) || self.hvm_defs.contains_key(name)
  }

  pub fn contains_builtin_def(&self, name: &Name) -> Option<bool> {
    self
      .fun_defs
      .get(name)
      .map(|d| d.is_builtin())
      .or_else(|| self.imp_defs.get(name).map(|d| d.source.is_builtin()))
      .or_else(|| self.hvm_defs.get(name).map(|d| d.source.is_builtin()))
  }
}

pub type ParseResult<T> = std::result::Result<T, ParseError>;

pub struct FunParser<'i> {
  file: Name,
  input: &'i str,
  index: usize,
  builtin: bool,
}

impl<'a> FunParser<'a> {
  pub fn new(file: Name, input: &'a str, builtin: bool) -> Self {
    Self { file, input, index: 0, builtin }
  }

  /* AST parsing functions */

  pub fn parse_book(&mut self, default_book: ParseBook) -> ParseResult<ParseBook> {
    let mut book = default_book;
    let mut indent = self.advance_newlines()?;
    while !self.is_eof() {
      // Record type definition
      if self.starts_with_keyword("object") {
        let ini_idx = *self.index();
        let mut prs = ImpParser {
          file: self.file.clone(),
          input: self.input,
          index: *self.index(),
          builtin: self.builtin,
        };
        let (adt, nxt_indent) = prs.parse_object(indent)?;
        self.index = prs.index;
        let end_idx = *self.index();
        self.add_type_def(adt, &mut book, ini_idx..end_idx)?;
        indent = nxt_indent;
        continue;
      }

      // Imp function definition
      if self.starts_with_keyword("def") {
        let ini_idx = *self.index();
        let mut prs =
          ImpParser { file: self.file.clone(), input: self.input, index: ini_idx, builtin: self.builtin };
        let (def, nxt_indent) = prs.parse_function_def(indent)?;
        self.index = prs.index;
        let end_idx = *self.index();
        self.add_imp_def(def, &mut book, ini_idx..end_idx)?;
        indent = nxt_indent;
        continue;
      }

      // Fun/Imp type definition
      if self.starts_with_keyword("type") {
        fn starts_with_imp_type(p: &mut FunParser) -> ParseResult<()> {
          p.parse_keyword("type")?;
          p.skip_trivia_inline()?;
          p.parse_top_level_name()?;
          p.skip_trivia_inline()?;
          if p.starts_with(":") || p.starts_with("(") {
            Ok(())
          } else {
            Err(ParseError::new((0, 0), ""))
          }
        }

        let ini_idx = *self.index();
        let is_imp = starts_with_imp_type(self).is_ok();
        self.index = ini_idx;
        if is_imp {
          // Imp type definition
          let mut prs = ImpParser {
            file: self.file.clone(),
            input: self.input,
            index: *self.index(),
            builtin: self.builtin,
          };
          let (adt, nxt_indent) = prs.parse_type_def(indent)?;
          self.index = prs.index;
          let end_idx = *self.index();
          self.add_type_def(adt, &mut book, ini_idx..end_idx)?;
          indent = nxt_indent;
          continue;
        } else {
          // Fun type definition
          let adt = self.parse_type_def()?;
          let end_idx = *self.index();
          self.add_type_def(adt, &mut book, ini_idx..end_idx)?;
          indent = self.advance_newlines()?;
          continue;
        }
      }

      // HVM native function definition
      if self.starts_with_keyword("hvm") {
        let ini_idx = self.index;
        let mut prs =
          ImpParser { file: self.file.clone(), input: self.input, index: self.index, builtin: self.builtin };
        let (def, nxt_indent) = prs.parse_hvm()?;
        *self.index() = prs.index;
        let end_idx = *self.index();
        self.add_hvm(def, &mut book, ini_idx..end_idx)?;
        indent = nxt_indent;
        continue;
      }

      // Import declaration
      if self.starts_with_keyword("from") {
        let import = self.parse_from_import()?;
        book.import_ctx.add_import(import);
        indent = self.advance_newlines()?;
        continue;
      }

      if self.starts_with_keyword("import") {
        let imports = self.parse_import()?;
        for imp in imports {
          book.import_ctx.add_import(imp);
        }
        indent = self.advance_newlines()?;
        continue;
      }

      // Fun function definition
      let ini_idx = *self.index();
      let def = self.parse_fun_def()?;
      let end_idx = *self.index();

      self.add_fun_def(def, &mut book, ini_idx..end_idx)?;
      indent = self.advance_newlines()?;
    }

    Ok(book)
  }

  fn parse_type_def(&mut self) -> ParseResult<Adt> {
    // type (name var1 ... varN) = ctr (| ctr)*
    let ini_idx = self.index;
    self.parse_keyword("type")?;
    self.skip_trivia();

    let name;
    let vars;
    if self.try_consume("(") {
      // parens around name and vars
      self.skip_trivia();
      name = self.parse_top_level_name()?;
      vars = self.list_like(|p| p.parse_var_name(), "", ")", "", false, 0)?;
      self.consume("=")?;
    } else {
      // no parens
      name = self.parse_top_level_name()?;
      vars = self.list_like(|p| p.parse_var_name(), "", "=", "", false, 0)?;
    }

    let mut ctrs = vec![self.parse_type_ctr(&name, &vars)?];
    while self.try_consume("|") {
      ctrs.push(self.parse_type_ctr(&name, &vars)?);
    }
    let ctrs = ctrs.into_iter().map(|ctr| (ctr.name.clone(), ctr)).collect::<IndexMap<_, _>>();

    let end_idx = *self.index();
    let source = Source::from_file_span(&self.file, self.input, ini_idx..end_idx, self.builtin);
    let adt = Adt { name, vars, ctrs, source };
    Ok(adt)
  }

  fn parse_type_ctr(&mut self, type_name: &Name, type_vars: &[Name]) -> ParseResult<AdtCtr> {
    // '(' name (( '~'? field) | ('~'? '('field (':' type)? ')') )* ')'
    // name
    self.skip_trivia();
    let ini_idx = *self.index();
    if self.try_consume("(") {
      // name and optionally fields

      self.skip_trivia();
      let ctr_name = self.parse_top_level_name()?;
      let ctr_name = Name::new(format!("{type_name}/{ctr_name}"));

      let fields = self.list_like(|p| p.parse_type_ctr_field(), "", ")", "", false, 0)?;
      let field_types = fields.iter().map(|f| f.typ.clone()).collect::<Vec<_>>();
      let end_idx = *self.index();
      self.check_repeated_ctr_fields(&fields, &ctr_name, ini_idx..end_idx)?;

      let typ = make_ctr_type(type_name.clone(), &field_types, type_vars);
      let ctr = AdtCtr { name: ctr_name, typ, fields };
      Ok(ctr)
    } else {
      // just name
      let name = self.labelled(|p| p.parse_top_level_name(), "datatype constructor name")?;
      let name = Name::new(format!("{type_name}/{name}"));
      let typ = make_ctr_type(type_name.clone(), &[], type_vars);
      let ctr = AdtCtr { name, typ, fields: vec![] };
      Ok(ctr)
    }
  }

  fn parse_type_ctr_field(&mut self) -> ParseResult<CtrField> {
    let rec = self.try_consume("~");

    let nam;
    let typ;
    if self.try_consume("(") {
      nam = self.parse_var_name()?;
      if self.try_consume(":") {
        typ = self.parse_type_term()?;
      } else {
        typ = Type::Any;
      }
      self.consume(")")?;
    } else {
      nam = self.parse_var_name()?;
      typ = Type::Any;
    }
    Ok(CtrField { nam, typ, rec })
  }

  fn parse_fun_def(&mut self) -> ParseResult<FunDefinition> {
    let ini_idx = *self.index();

    // Try to parse signature
    if let Ok((name, args, typ)) = self.parse_def_sig() {
      if self.try_consume("=") {
        // Single rule with signature
        let body = self.parse_term()?;
        let pats = args.into_iter().map(|nam| Pattern::Var(Some(nam))).collect();
        let rules = vec![Rule { pats, body }];
        let end_idx = *self.index();
        let source = Source::from_file_span(&self.file, self.input, ini_idx..end_idx, self.builtin);
        let def = FunDefinition { name, typ, check: true, rules, source };
        Ok(def)
      } else {
        // Multiple rules with signature
        let mut rules = vec![];
        let (_, rule) = self.parse_rule()?;
        rules.push(rule);
        while self.starts_with_rule(&name) {
          let (_, rule) = self.parse_rule()?;
          rules.push(rule);
        }
        let end_idx = *self.index();
        let source = Source::from_file_span(&self.file, self.input, ini_idx..end_idx, self.builtin);
        let def = FunDefinition { name, typ, check: true, rules, source };
        Ok(def)
      }
    } else {
      // Was not a signature, backtrack and read the name from the first rule
      self.index = ini_idx;
      let mut rules = vec![];
      let (name, rule) = self.parse_rule()?;
      rules.push(rule);
      while self.starts_with_rule(&name) {
        let (_, rule) = self.parse_rule()?;
        rules.push(rule);
      }
      let end_idx = *self.index();
      let source = Source::from_file_span(&self.file, self.input, ini_idx..end_idx, self.builtin);
      let def = FunDefinition { name, typ: Type::Any, check: true, rules, source };
      Ok(def)
    }
  }

  /// Parses a function definition signature.
  /// Returns the name, name of the arguments and the type of the function.
  fn parse_def_sig(&mut self) -> ParseResult<(Name, Vec<Name>, Type)> {
    // '(' name ((arg | '(' arg (':' type)? ')'))* ')' ':' type
    //     name ((arg | '(' arg (':' type)? ')'))*     ':' type
    let (name, args, typ) = if self.try_consume("(") {
      let name = self.parse_top_level_name()?;
      let args = self.list_like(|p| p.parse_def_sig_arg(), "", ")", "", false, 0)?;
      self.consume(":")?;
      let typ = self.parse_type_term()?;
      (name, args, typ)
    } else {
      let name = self.parse_top_level_name()?;
      let args = self.list_like(|p| p.parse_def_sig_arg(), "", ":", "", false, 0)?;
      let typ = self.parse_type_term()?;
      (name, args, typ)
    };
    let (args, arg_types): (Vec<_>, Vec<_>) = args.into_iter().unzip();
    let typ = make_fn_type(&arg_types, typ);
    Ok((name, args, typ))
  }

  fn parse_def_sig_arg(&mut self) -> ParseResult<(Name, Type)> {
    // name
    // '(' name ')'
    // '(' name ':' type ')'
    if self.try_consume("(") {
      let name = self.parse_var_name()?;
      let typ = if self.try_consume(":") { self.parse_type_term()? } else { Type::Any };
      self.consume(")")?;
      Ok((name, typ))
    } else {
      let name = self.parse_var_name()?;
      Ok((name, Type::Any))
    }
  }

  fn parse_from_import(&mut self) -> ParseResult<Import> {
    // from path import package
    // from path import (a, b)
    // from path import *
    self.parse_keyword("from")?;
    self.skip_trivia_inline()?;

    let path = self.parse_restricted_name("Path")?;
    self.skip_trivia_inline()?;

    self.consume("import")?;
    self.skip_trivia_inline()?;

    let relative = path.starts_with("./") | path.starts_with("../");

    if self.try_consume("*") {
      return Ok(Import::new(path, ImportType::Glob, relative));
    }

    if self.try_consume("(") {
      let sub = self.list_like(|p| p.parse_name_maybe_alias("Name"), "", ")", ",", false, 1)?;
      return Ok(Import::new(path, ImportType::List(sub), relative));
    }

    let (import, alias) = self.parse_name_maybe_alias("Import")?;
    Ok(Import::new(path, ImportType::Single(import, alias), relative))
  }

  fn parse_import(&mut self) -> ParseResult<Vec<Import>> {
    // import path
    // import (path/a, path/b)
    self.parse_keyword("import")?;
    self.skip_trivia_inline()?;

    let new_import = |import: Name, alias: Option<Name>, relative: bool| -> Import {
      let (path, import) = match import.rsplit_once('/') {
        Some((start, end)) => (Name::new(start), Name::new(end)),
        None => (Name::default(), import),
      };

      Import::new(path, ImportType::Single(import, alias), relative)
    };

    if self.try_consume("(") {
      let list = self.list_like(|p| p.parse_import_name("Name"), "", ")", ",", false, 1)?;
      let imports = list.into_iter().map(|(a, b, c)| new_import(a, b, c)).collect_vec();
      return Ok(imports);
    }

    let (import, alias, relative) = self.parse_import_name("Import")?;
    let import = new_import(import, alias, relative);
    Ok(vec![import])
  }

  fn parse_rule_lhs(&mut self) -> ParseResult<(Name, Vec<Pattern>)> {
    if self.try_consume_exactly("(") {
      self.skip_trivia();
      let name = self.labelled(|p| p.parse_top_level_name(), "function name")?;
      let pats = self.list_like(|p| p.parse_pattern(false), "", ")", "", false, 0)?;
      Ok((name, pats))
    } else {
      // Rule without parens
      // Here we use a different label for the error because this is
      // the last alternative case for top-level definitions.
      let name = self.labelled(|p| p.parse_top_level_name(), "top-level definition")?;
      let mut pats = vec![];
      self.skip_trivia();
      while !self.starts_with("=") {
        pats.push(self.parse_pattern(false)?);
        self.skip_trivia();
      }
      Ok((name, pats))
    }
  }

  fn parse_rule(&mut self) -> ParseResult<(Name, Rule)> {
    let (name, pats) = self.parse_rule_lhs()?;

    self.consume("=")?;

    let body = self.parse_term()?;

    let rule = Rule { pats, body };
    Ok((name, rule))
  }

  fn starts_with_rule(&mut self, expected_name: &Name) -> bool {
    self.skip_trivia();
    let ini_idx = *self.index();
    let res = self.parse_rule_lhs();
    self.index = ini_idx;
    if let Ok((name, _)) = res {
      if &name == expected_name {
        // Found rule with the expected name
        true
      } else {
        // Found rule with a different name
        false
      }
    } else {
      // Not a rule
      false
    }
  }

  fn parse_pattern(&mut self, simple: bool) -> ParseResult<Pattern> {
    maybe_grow(|| {
      let (tag, unexpected_tag) = self.parse_tag()?;
      self.skip_trivia();

      // Ctr or Tup
      if self.starts_with("(") {
        self.advance_one();
        let head_ini_idx = *self.index();
        let head = self.parse_pattern(simple)?;
        let head_end_idx = *self.index();

        // Tup
        self.skip_trivia();
        if self.starts_with(",") || simple {
          self.consume(",")?;
          let mut els = self.list_like(|p| p.parse_pattern(simple), "", ")", ",", true, 1)?;
          els.insert(0, head);
          return Ok(Pattern::Fan(FanKind::Tup, tag.unwrap_or(Tag::Static), els));
        }

        // Ctr
        unexpected_tag(self)?;
        let Pattern::Var(Some(name)) = head else {
          return self.expected_spanned("constructor name", head_ini_idx..head_end_idx);
        };
        let els = self.list_like(|p| p.parse_pattern(simple), "", ")", "", false, 0)?;
        return Ok(Pattern::Ctr(name, els));
      }

      // Dup
      if self.starts_with("{") {
        let els = self.list_like(|p| p.parse_pattern(simple), "{", "}", ",", false, 0)?;
        return Ok(Pattern::Fan(FanKind::Dup, tag.unwrap_or(Tag::Auto), els));
      }

      // List
      if self.starts_with("[") && !simple {
        unexpected_tag(self)?;
        let els = self.list_like(|p| p.parse_pattern(simple), "[", "]", ",", false, 0)?;
        return Ok(Pattern::Lst(els));
      }

      // String
      if self.starts_with("\"") && !simple {
        unexpected_tag(self)?;
        let str = self.parse_quoted_string()?;
        return Ok(Pattern::Str(STRINGS.get(str)));
      }

      // Char
      if self.starts_with("'") {
        unexpected_tag(self)?;
        let char = self.parse_quoted_char()?;
        return Ok(Pattern::Num(char as u32));
      }

      // Number
      if self.peek_one().map_or(false, |c| c.is_ascii_digit()) {
        unexpected_tag(self)?;
        let num = self.parse_u32()?;
        return Ok(Pattern::Num(num));
      }

      // Channel
      if self.starts_with("$") {
        unexpected_tag(self)?;
        self.advance_one();
        self.skip_trivia();
        let name = self.parse_var_name()?;
        return Ok(Pattern::Chn(name));
      }

      // Var
      if self.starts_with("*")
        || self
          .peek_one()
          .is_some_and(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '-' || c == '/')
      {
        unexpected_tag(self)?;
        let nam = self.parse_name_or_era()?;
        return Ok(Pattern::Var(nam));
      }

      let ini_idx = *self.index();
      while !(self.is_eof() || self.starts_with("=")) {
        self.advance_one();
      }
      let cur_idx = *self.index();

      self.expected_spanned("pattern or '='", ini_idx..cur_idx)
    })
  }

  pub fn parse_term(&mut self) -> ParseResult<Term> {
    maybe_grow(|| {
      let (tag, unexpected_tag) = self.parse_tag()?;
      self.skip_trivia();

      // Lambda, unscoped lambda
      if self.starts_with("λ") || self.starts_with("@") {
        self.advance_one();
        let tag = tag.unwrap_or(Tag::Static);
        let pat = self.parse_pattern(true)?;
        let bod = self.parse_term()?;
        return Ok(Term::Lam { tag, pat: Box::new(pat), bod: Box::new(bod) });
      }

      // App, Tup, Num Op
      if self.starts_with("(") {
        self.advance_one();

        // Opr but maybe a tup
        self.skip_trivia();
        if let Some(opr) = self.try_parse_oper() {
          self.skip_trivia();

          // jk, actually a tuple
          if self.starts_with(",") && opr == Op::MUL {
            self.consume_exactly(",")?;
            let tail = self.list_like(|p| p.parse_term(), "", ")", ",", true, 1)?;
            let els = [Term::Era].into_iter().chain(tail).collect();
            return Ok(Term::Fan { fan: FanKind::Tup, tag: tag.unwrap_or(Tag::Static), els });
          }

          if opr == Op::MUL && self.try_consume(")") {
            return Ok(Term::Era);
          }

          // Opr
          unexpected_tag(self)?;
          let fst = self.parse_term()?;
          let snd = self.parse_term()?;
          self.consume(")")?;
          return Ok(Term::Oper { opr, fst: Box::new(fst), snd: Box::new(snd) });
        }

        // Tup or App
        let head = self.parse_term()?;

        // Tup
        self.skip_trivia();
        if self.starts_with(",") {
          let mut els = vec![head];
          while self.try_consume(",") {
            els.push(self.parse_term()?);
          }
          self.consume(")")?;
          return Ok(Term::Fan { fan: FanKind::Tup, tag: tag.unwrap_or(Tag::Static), els });
        }

        // App
        let els = self.list_like(|p| p.parse_term(), "", ")", "", false, 0)?;
        let term = els.into_iter().fold(head, |fun, arg| Term::App {
          tag: tag.clone().unwrap_or(Tag::Static),
          fun: Box::new(fun),
          arg: Box::new(arg),
        });
        return Ok(term);
      }

      // List
      if self.starts_with("[") {
        unexpected_tag(self)?;
        let els = self.list_like(|p| p.parse_term(), "[", "]", ",", false, 0)?;
        return Ok(Term::List { els });
      }

      // Tree Node
      if self.starts_with("![") {
        self.advance_one();
        self.advance_one();
        unexpected_tag(self)?;
        let lft = self.parse_term()?;
        self.try_consume(",");
        let rgt = self.parse_term()?;
        self.labelled(|p| p.consume("]"), "Only two children in a Tree/Node")?;
        return Ok(Term::call(Term::r#ref("Tree/Node"), [lft, rgt]));
      }

      // Tree Leaf
      if self.starts_with("!") {
        self.advance_one();
        unexpected_tag(self)?;
        let val = self.parse_term()?;
        return Ok(Term::app(Term::r#ref("Tree/Leaf"), val));
      }

      // Sup
      if self.starts_with("{") {
        let els = self.list_like(|p| p.parse_term(), "{", "}", ",", false, 2)?;
        return Ok(Term::Fan { fan: FanKind::Dup, tag: tag.unwrap_or(Tag::Auto), els });
      }

      // Unscoped var
      if self.starts_with("$") {
        self.advance_one();
        unexpected_tag(self)?;
        self.skip_trivia();
        let nam = self.parse_var_name()?;
        return Ok(Term::Link { nam });
      }

      // Era
      if self.starts_with("*") {
        self.advance_one();
        unexpected_tag(self)?;
        return Ok(Term::Era);
      }

      // Nat
      if self.starts_with("#") {
        self.advance_one();
        unexpected_tag(self)?;
        let val = self.parse_u32()?;
        return Ok(Term::Nat { val });
      }

      // String
      if self.starts_with("\"") {
        unexpected_tag(self)?;
        let str = self.parse_quoted_string()?;
        return Ok(Term::Str { val: STRINGS.get(str) });
      }

      // Char
      if self.starts_with("'") {
        unexpected_tag(self)?;
        let char = self.parse_quoted_char()?;
        return Ok(Term::Num { val: Num::U24(char as u32 & 0x00ff_ffff) });
      }

      // Symbol
      if self.starts_with("`") {
        unexpected_tag(self)?;
        let val = self.parse_quoted_symbol()?;
        return Ok(Term::Num { val: Num::U24(val) });
      }

      // Native Number
      if self.peek_one().map_or(false, is_num_char) {
        unexpected_tag(self)?;
        let num = self.parse_number()?;
        return Ok(Term::Num { val: num });
      }

      // Use
      if self.try_parse_keyword("use") {
        unexpected_tag(self)?;
        self.skip_trivia();
        let nam = self.parse_var_name()?;
        self.consume("=")?;
        let val = self.parse_term()?;
        self.try_consume(";");
        let nxt = self.parse_term()?;
        return Ok(Term::Use { nam: Some(nam), val: Box::new(val), nxt: Box::new(nxt) });
      }

      // Let
      if self.try_parse_keyword("let") {
        unexpected_tag(self)?;
        let pat = self.parse_pattern(true)?;
        self.consume("=")?;
        let val = self.parse_term()?;
        self.try_consume(";");
        let nxt = self.parse_term()?;
        return Ok(Term::Let { pat: Box::new(pat), val: Box::new(val), nxt: Box::new(nxt) });
      }

      // Ask (monadic operation)
      if self.try_parse_keyword("ask") {
        unexpected_tag(self)?;
        let pat = self.parse_pattern(true)?;
        self.consume("=")?;
        let val = self.parse_term()?;
        self.try_consume(";");
        let nxt = self.parse_term()?;
        return Ok(Term::Ask { pat: Box::new(pat), val: Box::new(val), nxt: Box::new(nxt) });
      }

      // Def
      if self.try_parse_keyword("def") {
        self.skip_trivia();
        let mut def = self.parse_fun_def()?;
        def.source.kind = SourceKind::Generated;
        let nxt = self.parse_term()?;
        return Ok(Term::Def { def, nxt: Box::new(nxt) });
      }

      // If
      if self.try_parse_keyword("if") {
        let mut chain = Vec::new();
        let cnd = self.parse_term()?;
        self.consume("{")?;
        let thn = self.parse_term()?;
        self.consume("}")?;

        chain.push((cnd, thn));

        self.skip_trivia_inline()?;
        while self.try_parse_keyword("elif") {
          let cnd = self.parse_term()?;
          self.consume("{")?;
          let thn = self.parse_term()?;
          self.consume("}")?;
          self.skip_trivia_inline()?;
          chain.push((cnd, thn));
        }

        self.consume("else")?;
        self.consume("{")?;
        let els = self.parse_term()?;
        self.consume("}")?;
        let els = chain.into_iter().rfold(els, |acc, (cnd, thn)| Term::Swt {
          bnd: Some(Name::new("%cond")),
          arg: Box::new(cnd),
          with_bnd: Vec::new(),
          with_arg: Vec::new(),
          pred: Some(Name::new("%cond-1")),
          arms: vec![acc, thn],
        });
        return Ok(els);
      }

      // Match
      if self.try_parse_keyword("match") {
        unexpected_tag(self)?;
        let (bnd, arg) = self.parse_match_arg()?;
        let (with_bnd, with_arg) = self.parse_with_clause()?;
        let arms = self.list_like(|p| p.parse_match_arm(), "", "}", ";", false, 1)?;
        return Ok(Term::Mat { arg: Box::new(arg), bnd, with_bnd, with_arg, arms });
      }

      // Switch
      if self.try_parse_keyword("switch") {
        unexpected_tag(self)?;
        let (bnd, arg) = self.parse_match_arg()?;
        let (with_bnd, with_arg) = self.parse_with_clause()?;

        self.try_consume("|");
        self.consume("0")?;
        self.consume(":")?;
        let zero = self.parse_term()?;
        self.try_consume(";");

        let mut arms = vec![zero];
        let mut expected_num = 1;
        loop {
          self.try_consume("|");
          // case _
          if self.try_consume("_") {
            self.consume(":")?;
            arms.push(self.parse_term()?);
            self.try_consume(";");
            self.consume("}")?;
            break;
          }
          // case num
          let val = self.parse_u32()?;
          if val != expected_num {
            return self.expected(&format!("'{}'", &expected_num.to_string()));
          }
          expected_num += 1;
          self.consume(":")?;
          arms.push(self.parse_term()?);
          self.try_consume(";");
        }
        let pred = Some(Name::new(format!("{}-{}", bnd.as_ref().unwrap(), arms.len() - 1)));
        return Ok(Term::Swt { arg: Box::new(arg), bnd, with_bnd, with_arg, pred, arms });
      }

      // With (monadic block)
      if self.try_parse_keyword("with") {
        unexpected_tag(self)?;
        let typ = self.parse_name()?;
        self.consume("{")?;
        let bod = self.parse_term()?;
        self.consume("}")?;
        return Ok(Term::With { typ: Name::new(typ), bod: Box::new(bod) });
      }

      // Fold
      if self.try_parse_keyword("fold") {
        unexpected_tag(self)?;
        let (bnd, arg) = self.parse_match_arg()?;
        let (with_bnd, with_arg) = self.parse_with_clause()?;
        let arms = self.list_like(|p| p.parse_match_arm(), "", "}", ";", false, 1)?;
        return Ok(Term::Fold { arg: Box::new(arg), bnd, with_bnd, with_arg, arms });
      }

      // Bend
      if self.try_parse_keyword("bend") {
        unexpected_tag(self)?;
        let args = self.list_like(
          |p| {
            let bind = p.parse_var_name()?;
            let init = if p.try_consume("=") { p.parse_term()? } else { Term::Var { nam: bind.clone() } };
            Ok((bind, init))
          },
          "",
          "{",
          ",",
          false,
          0,
        )?;
        let (bind, init): (Vec<_>, Vec<_>) = args.into_iter().unzip();
        let bind = bind.into_iter().map(Some).collect::<Vec<_>>();
        self.skip_trivia();
        self.parse_keyword("when")?;
        let cond = self.parse_term()?;
        self.consume(":")?;
        let step = self.parse_term()?;
        self.skip_trivia();
        self.parse_keyword("else")?;
        self.consume(":")?;
        let base = self.parse_term()?;
        self.consume("}")?;
        return Ok(Term::Bend {
          bnd: bind,
          arg: init,
          cond: Box::new(cond),
          step: Box::new(step),
          base: Box::new(base),
        });
      }

      // Open
      if self.try_parse_keyword("open") {
        unexpected_tag(self)?;
        self.skip_trivia();
        let typ = self.parse_top_level_name()?;
        self.skip_trivia();
        let var = self.parse_var_name()?;
        self.try_consume(";");
        let bod = self.parse_term()?;
        return Ok(Term::Open { typ, var, bod: Box::new(bod) });
      }

      // Var
      unexpected_tag(self)?;
      let nam = self.labelled(|p| p.parse_var_name(), "term")?;
      Ok(Term::Var { nam })
    })
  }

  fn parse_name_or_era(&mut self) -> ParseResult<Option<Name>> {
    self.labelled(
      |p| {
        if p.try_consume_exactly("*") {
          Ok(None)
        } else {
          let nam = p.parse_var_name()?;
          Ok(Some(nam))
        }
      },
      "name or '*'",
    )
  }

  /// Parses a tag where it may or may not be valid.
  ///
  /// If it is not valid, the returned callback can be used to issue an error.
  fn parse_tag(&mut self) -> ParseResult<(Option<Tag>, impl FnOnce(&mut Self) -> Result<(), ParseError>)> {
    let index = self.index;
    self.skip_trivia();
    let tag = if self.peek_one() == Some('#')
      && !self.peek_many(2).is_some_and(|x| x.chars().nth(1).unwrap().is_ascii_digit())
    {
      let msg = "Tagged terms not supported for hvm32.".to_string();
      return self.with_ctx(Err(msg), index..index + 1);
    } else {
      None
    };
    let end_index = self.index;
    let has_tag = tag.is_some();
    Ok((tag, move |slf: &mut Self| {
      if has_tag {
        let msg = "\x1b[1m- unexpected tag:\x1b[0m".to_string();
        slf.with_ctx(Err(msg), index..end_index)
      } else {
        Ok(())
      }
    }))
  }

  // A named arg with optional name.
  fn parse_match_arg(&mut self) -> ParseResult<(Option<Name>, Term)> {
    let ini_idx = *self.index();
    let mut arg = self.parse_term()?;
    let end_idx = *self.index();

    self.skip_trivia();
    match (&mut arg, self.starts_with("=")) {
      (Term::Var { nam }, true) => {
        self.consume("=")?;
        Ok((Some(std::mem::take(nam)), self.parse_term()?))
      }
      (Term::Var { nam }, false) => Ok((Some(nam.clone()), Term::Var { nam: std::mem::take(nam) })),
      (_, true) => self.expected_spanned("argument name", ini_idx..end_idx),
      (arg, false) => Ok((Some(Name::new("%arg")), std::mem::take(arg))),
    }
  }

  /// A named arg with non-optional name.
  fn parse_named_arg(&mut self) -> ParseResult<(Option<Name>, Term)> {
    let nam = self.parse_var_name()?;
    self.skip_trivia();
    if self.starts_with("=") {
      self.advance_one();
      let arg = self.parse_term()?;
      Ok((Some(nam), arg))
    } else {
      let arg = Term::Var { nam: nam.clone() };
      Ok((Some(nam), arg))
    }
  }

  fn parse_with_clause(&mut self) -> ParseResult<(Vec<Option<Name>>, Vec<Term>)> {
    self.skip_trivia();
    let res = if self.try_parse_keyword("with") {
      self.list_like(|p| p.parse_named_arg(), "", "{", ",", false, 1)?.into_iter().unzip()
    } else {
      self.consume_exactly("{")?;
      (vec![], vec![])
    };
    Ok(res)
  }

  fn parse_match_arm(&mut self) -> ParseResult<MatchRule> {
    self.try_consume("|");
    self.skip_trivia();
    let nam = self.parse_name_or_era()?;
    self.consume(":")?;
    let bod = self.parse_term()?;
    Ok((nam, vec![], bod))
  }

  fn parse_type_term(&mut self) -> ParseResult<Type> {
    let mut left = self.parse_type_atom()?;
    self.skip_trivia();
    while self.try_consume_exactly("->") {
      let right = self.parse_type_term()?;
      left = Type::Arr(Box::new(left), Box::new(right));
    }
    Ok(left)
  }

  /// Parses a type without an ending arrow.
  /// Either an atom, a tuple, a ctr or a parenthesized type.
  fn parse_type_atom(&mut self) -> ParseResult<Type> {
    self.skip_trivia();
    if self.try_parse_keyword("Any") {
      Ok(Type::Any)
    } else if self.try_parse_keyword("None") {
      Ok(Type::None)
    } else if self.try_parse_keyword("u24") {
      Ok(Type::U24)
    } else if self.try_parse_keyword("i24") {
      Ok(Type::I24)
    } else if self.try_parse_keyword("f24") {
      Ok(Type::F24)
    } else if self.try_consume_exactly("(") {
      // Tuple, constructor or parenthesized expression
      let ini_idx = *self.index();
      let head = self.parse_type_term()?;
      self.skip_trivia();
      if self.try_consume_exactly(")") {
        // Parens
        Ok(head)
      } else if self.try_consume_exactly(",") {
        // Tuple
        let mut types = vec![head];
        loop {
          types.push(self.parse_type_term()?);
          self.skip_trivia();
          if !self.try_consume_exactly(",") {
            break;
          }
        }
        self.consume(")")?;
        Ok(Type::Tup(types))
      } else {
        // Constructor
        let Type::Var(nam) = head else {
          let end_idx = *self.index();
          // TODO: This is not a good error message
          return self.expected_spanned("type constructor", ini_idx..end_idx);
        };
        let mut args = vec![];
        // We know there's at least one argument, otherwise it would go in the parens case.
        while !self.try_consume(")") {
          args.push(self.parse_type_term()?);
          self.skip_trivia();
        }
        Ok(Type::Ctr(nam, args))
      }
    } else {
      // Variable
      // TODO: This will show "expected Name" instead of "expected type"
      let nam = self.parse_var_name()?;
      Ok(Type::Var(nam))
    }
  }

  fn add_fun_def(&mut self, def: FunDefinition, book: &mut ParseBook, span: Range<usize>) -> ParseResult<()> {
    self.check_top_level_redefinition(&def.name, book, span)?;
    book.fun_defs.insert(def.name.clone(), def);
    Ok(())
  }

  fn add_imp_def(
    &mut self,
    def: crate::imp::Definition,
    book: &mut ParseBook,
    span: Range<usize>,
  ) -> ParseResult<()> {
    self.check_top_level_redefinition(&def.name, book, span)?;
    book.imp_defs.insert(def.name.clone(), def);
    Ok(())
  }

  fn add_hvm(&mut self, def: HvmDefinition, book: &mut ParseBook, span: Range<usize>) -> ParseResult<()> {
    self.check_top_level_redefinition(&def.name, book, span)?;
    book.hvm_defs.insert(def.name.clone(), def);
    Ok(())
  }

  fn add_type_def(&mut self, adt: Adt, book: &mut ParseBook, span: Range<usize>) -> ParseResult<()> {
    self.check_type_redefinition(&adt.name, book, span.clone())?;
    for ctr in adt.ctrs.keys() {
      if let Some(builtin) = book.contains_builtin_def(ctr) {
        let msg = FunParser::redefinition_of_function_msg(builtin, ctr);
        return self.with_ctx(Err(msg), span);
      }
      match book.ctrs.entry(ctr.clone()) {
        indexmap::map::Entry::Vacant(e) => _ = e.insert(adt.name.clone()),
        indexmap::map::Entry::Occupied(e) => {
          let msg = FunParser::redefinition_of_constructor_msg(e.key());
          return self.with_ctx(Err(msg), span);
        }
      }
    }
    book.adts.insert(adt.name.clone(), adt);
    Ok(())
  }

  fn check_top_level_redefinition(
    &mut self,
    name: &Name,
    book: &mut ParseBook,
    span: Range<usize>,
  ) -> ParseResult<()> {
    if let Some(builtin) = book.contains_builtin_def(name) {
      let msg = Self::redefinition_of_function_msg(builtin, name);
      return self.with_ctx(Err(msg), span);
    }
    if book.ctrs.contains_key(name) {
      let msg = Self::redefinition_of_constructor_msg(name);
      return self.with_ctx(Err(msg), span);
    }
    if book.hvm_defs.contains_key(name) {
      let msg = Self::redefinition_of_hvm_msg(false, name);
      return self.with_ctx(Err(msg), span);
    }
    Ok(())
  }

  fn check_type_redefinition(
    &mut self,
    name: &Name,
    book: &mut ParseBook,
    span: Range<usize>,
  ) -> ParseResult<()> {
    if book.adts.contains_key(name) {
      let msg = Self::redefinition_of_type_msg(name);
      return self.with_ctx(Err(msg), span);
    }
    Ok(())
  }
}

impl<'a> Parser<'a> for FunParser<'a> {
  fn input(&mut self) -> &'a str {
    self.input
  }

  fn index(&mut self) -> &mut usize {
    &mut self.index
  }

  /// Generates an error message for parsing failures, including the highlighted context.
  ///
  /// Override to have our own error message.
  fn expected<T>(&mut self, exp: &str) -> ParseResult<T> {
    let ini_idx = *self.index();
    let end_idx = *self.index() + 1;
    self.expected_spanned(exp, ini_idx..end_idx)
  }

  /// Generates an error message with an additional custom message.
  ///
  /// Override to have our own error message.
  fn expected_and<T>(&mut self, exp: &str, msg: &str) -> ParseResult<T> {
    let ini_idx = *self.index();
    let end_idx = *self.index() + 1;
    self.expected_spanned_and(exp, msg, ini_idx..end_idx)
  }

  /// Consumes an instance of the given string, erroring if it is not found.
  ///
  /// Override to have our own error message.
  fn consume(&mut self, text: &str) -> ParseResult<()> {
    self.skip_trivia();
    if self.input().get(*self.index()..).unwrap_or_default().starts_with(text) {
      *self.index() += text.len();
      Ok(())
    } else {
      self.expected(format!("'{text}'").as_str())
    }
  }

  fn skip_trivia(&mut self) {
    while let Some(c) = self.peek_one() {
      if c.is_ascii_whitespace() {
        self.advance_one();
        continue;
      }
      if c == '#' {
        self.advance_one();
        if let Some(c) = self.peek_one() {
          if c == '{' {
            self.advance_one();
            while let Some(c) = self.peek_one() {
              self.advance_one();
              if c == '#' {
                if let Some('}') = self.peek_one() {
                  self.advance_one();
                  break;
                } else {
                  self.advance_one();
                }
              }
            }
          } else {
            while let Some(c) = self.peek_one() {
              if c != '\n' {
                self.advance_one();
              } else {
                break;
              }
            }
          }
        }
        continue;
      }
      break;
    }
  }
}

pub fn is_name_char(c: char) -> bool {
  c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '-' || c == '/'
}

pub fn is_num_char(c: char) -> bool {
  "0123456789+-".contains(c)
}

pub fn make_fn_type(args: &[Type], ret: Type) -> Type {
  let typ = ret;
  let typ = args.iter().rfold(typ, |acc, typ| Type::Arr(Box::new(typ.clone()), Box::new(acc)));
  typ
}

pub fn make_ctr_type(type_name: Name, fields: &[Type], vars: &[Name]) -> Type {
  let typ = Type::Ctr(type_name, vars.iter().cloned().map(Type::Var).collect());
  let typ = fields.iter().rfold(typ, |acc, typ| Type::Arr(Box::new(typ.clone()), Box::new(acc)));
  let typ = vars.iter().rfold(typ, |acc, typ| Type::All(typ.clone(), Box::new(acc)));
  typ
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Indent {
  Val(isize),
  Eof,
}

impl Indent {
  pub fn new(val: isize) -> Self {
    Indent::Val(val)
  }

  pub fn enter_level(&mut self) {
    if let Indent::Val(val) = self {
      *val += 2;
    }
  }

  pub fn exit_level(&mut self) {
    if let Indent::Val(val) = self {
      *val -= 2;
    }
  }
}

impl<'a> ParserCommons<'a> for FunParser<'a> {}

pub trait ParserCommons<'a>: Parser<'a> {
  /// Generates an error message that does not print expected terms.
  fn expected_message<T>(&mut self, msg: &str) -> ParseResult<T> {
    let ini_idx = *self.index();
    let end_idx = *self.index() + 1;

    let is_eof = self.is_eof();
    let detected = DisplayFn(|f| if is_eof { write!(f, " end of input") } else { Ok(()) });
    let msg = format!("\x1b[1m- information:\x1b[0m {}\n\x1b[1m- location:\x1b[0m{}", msg, detected);
    self.with_ctx(Err(msg), ini_idx..end_idx)
  }

  fn labelled<T>(&mut self, parser: impl Fn(&mut Self) -> ParseResult<T>, label: &str) -> ParseResult<T> {
    match parser(self) {
      Ok(val) => Ok(val),
      Err(_) => self.expected(label),
    }
  }

  fn parse_restricted_name(&mut self, kind: &str) -> ParseResult<Name> {
    let ini_idx = *self.index();
    let name = self.take_while(is_name_char);
    if name.is_empty() {
      self.expected(&format!("{kind} name"))?
    }
    let name = Name::new(name.to_owned());
    let end_idx = *self.index();
    if name.contains("__") {
      let msg = format!("{kind} names are not allowed to contain \"__\".");
      self.with_ctx(Err(msg), ini_idx..end_idx)
    } else if name.starts_with("//") {
      let msg = format!("{kind} names are not allowed to start with \"//\".");
      self.with_ctx(Err(msg), ini_idx..end_idx)
    } else {
      Ok(name)
    }
  }

  fn parse_top_level_name(&mut self) -> ParseResult<Name> {
    self.parse_restricted_name("Top-level")
  }

  fn parse_var_name(&mut self) -> ParseResult<Name> {
    self.parse_restricted_name("Variable")
  }

  fn parse_name_maybe_alias(&mut self, label: &str) -> ParseResult<(Name, Option<Name>)> {
    let name = self.parse_restricted_name(label)?;

    if self.try_consume("as") {
      self.skip_trivia();
      let alias = self.parse_restricted_name("Alias")?;
      Ok((name, Some(alias)))
    } else {
      Ok((name, None))
    }
  }

  fn parse_import_name(&mut self, label: &str) -> ParseResult<(Name, Option<Name>, bool)> {
    let (import, alias) = self.parse_name_maybe_alias(label)?;
    let relative = import.starts_with("./") | import.starts_with("../");
    Ok((import, alias, relative))
  }

  /// Consumes exactly the text without skipping.
  fn consume_exactly(&mut self, text: &str) -> ParseResult<()> {
    if self.input().get(*self.index()..).unwrap_or_default().starts_with(text) {
      *self.index() += text.len();
      Ok(())
    } else {
      self.expected(format!("'{text}'").as_str())
    }
  }

  fn consume_new_line(&mut self) -> ParseResult<()> {
    self.skip_trivia_inline()?;
    self.try_consume_exactly("\r");
    self.labelled(|p| p.consume_exactly("\n"), "newline")
  }

  /// Skips trivia, returns the number of trivia characters skipped in the last line.
  fn advance_newlines(&mut self) -> ParseResult<Indent> {
    loop {
      let num_spaces = self.advance_trivia_inline()?;
      if self.peek_one() == Some('\r') {
        self.advance_one();
      }
      if self.peek_one() == Some('\n') {
        self.advance_one();
      } else if self.is_eof() {
        return Ok(Indent::Eof);
      } else {
        return Ok(Indent::Val(num_spaces));
      }
    }
  }

  /// Advances the parser to the next non-trivia character in the same line.
  /// Returns how many characters were advanced.
  fn advance_trivia_inline(&mut self) -> ParseResult<isize> {
    let mut char_count = 0;
    while let Some(c) = self.peek_one() {
      if c == '\t' {
        let idx = *self.index();
        return self.with_ctx(Err("Tabs are not accepted for indentation.".to_string()), idx..idx);
      }
      if " ".contains(c) {
        self.advance_one();
        char_count += 1;
        continue;
      }
      if c == '#' {
        self.advance_one();
        char_count += 1;
        if let Some(c) = self.peek_one() {
          if c == '{' {
            self.advance_one();
            char_count += 1;
            while let Some(c) = self.peek_one() {
              self.advance_one();
              char_count += 1;
              if c == '#' {
                if let Some('}') = self.peek_one() {
                  self.advance_one();
                  char_count += 1;
                  break;
                } else {
                  self.advance_one();
                  char_count += 1;
                }
              }
            }
          } else {
            while let Some(c) = self.peek_one() {
              if c != '\n' {
                self.advance_one();
                char_count += 1;
              } else {
                break;
              }
            }
          }
        }
        continue;
      }
      break;
    }
    Ok(char_count)
  }

  /// Skips until the next non-trivia character in the same line.
  fn skip_trivia_inline(&mut self) -> ParseResult<()> {
    self.advance_trivia_inline()?;
    Ok(())
  }

  fn expected_spanned<T>(&mut self, exp: &str, span: Range<usize>) -> ParseResult<T> {
    let is_eof = self.is_eof();
    let detected = DisplayFn(|f| if is_eof { write!(f, " end of input") } else { Ok(()) });
    let msg = format!("\x1b[1m- expected:\x1b[0m {}\n\x1b[1m- detected:\x1b[0m{}", exp, detected);
    self.with_ctx(Err(msg), span)
  }

  /// Same as `expected_spanned` but adds an information message before the expected message.
  fn expected_spanned_and<T>(&mut self, exp: &str, msg: &str, span: Range<usize>) -> ParseResult<T> {
    let is_eof = self.is_eof();
    let detected = DisplayFn(|f| if is_eof { write!(f, " end of input") } else { Ok(()) });
    let msg = format!(
      "\x1b[1m- information:\x1b[0m {}\n\x1b[1m- expected:\x1b[0m {}\n\x1b[1m- detected:\x1b[0m{}",
      msg, exp, detected,
    );
    self.with_ctx(Err(msg), span)
  }

  fn with_ctx<T>(&mut self, res: Result<T, impl std::fmt::Display>, span: Range<usize>) -> ParseResult<T> {
    res.map_err(|msg| {
      let ctx = highlight_error(span.start, span.end, self.input());
      let msg = format!("{msg}\n{ctx}");
      ParseError::new((span.start, span.end), msg)
    })
  }

  /// Consumes text if the input starts with it or trivia. Otherwise, do nothing.
  fn try_consume(&mut self, text: &str) -> bool {
    self.skip_trivia();
    if self.starts_with(text) {
      self.consume(text).unwrap();
      true
    } else {
      false
    }
  }

  /// Consumes text if the input starts exactly with it. Otherwise, do nothing.
  fn try_consume_exactly(&mut self, text: &str) -> bool {
    if self.starts_with(text) {
      self.consume_exactly(text).unwrap();
      true
    } else {
      false
    }
  }

  fn try_parse_keyword(&mut self, keyword: &str) -> bool {
    if self.starts_with_keyword(keyword) {
      self.consume_exactly(keyword).unwrap();
      true
    } else {
      false
    }
  }

  fn parse_keyword(&mut self, keyword: &str) -> ParseResult<()> {
    let ini_idx = *self.index();
    self.consume_exactly(keyword)?;
    let end_idx = *self.index();
    let input = &self.input()[*self.index()..];
    let next_is_name = input.chars().next().map_or(false, is_name_char);
    if !next_is_name {
      Ok(())
    } else {
      self.expected_spanned(&format!("keyword '{keyword}'"), ini_idx..end_idx + 1)
    }
  }

  fn starts_with_keyword(&mut self, keyword: &str) -> bool {
    if self.starts_with(keyword) {
      let input = &self.input()[*self.index() + keyword.len()..];
      let next_is_name = input.chars().next().map_or(false, is_name_char);
      !next_is_name
    } else {
      false
    }
  }

  /// Parses a list-like structure like "[x1, x2, x3,]".
  /// Since a list is always well terminated, we consume newlines.
  ///
  /// `parser` is a function that parses an element of the list.
  ///
  /// If `hard_sep` the separator between elements is mandatory.
  /// Always accepts trailing separators.
  ///
  /// `min_els` determines how many elements must be parsed at minimum.
  fn list_like<T>(
    &mut self,
    mut parser: impl FnMut(&mut Self) -> ParseResult<T>,
    start: &str,
    end: &str,
    sep: &str,
    hard_sep: bool,
    min_els: usize,
  ) -> ParseResult<Vec<T>> {
    self.consume_exactly(start)?;

    let mut els = vec![];
    // Consume the minimum number of elements
    for i in 0..min_els {
      self.skip_trivia();
      els.push(parser(self)?);
      self.skip_trivia();
      if hard_sep && !(i == min_els - 1 && self.starts_with(end)) {
        self.consume(sep)?;
      } else {
        self.try_consume(sep);
      }
    }

    // Consume optional elements
    while !self.try_consume(end) {
      els.push(parser(self)?);
      self.skip_trivia();
      if hard_sep && !self.starts_with(end) {
        self.consume(sep)?;
      } else {
        self.try_consume(sep);
      }
    }
    Ok(els)
  }

  fn try_parse_oper(&mut self) -> Option<Op> {
    let opr = if self.try_consume_exactly("+") {
      Op::ADD
    } else if self.try_consume_exactly("-") {
      Op::SUB
    } else if self.try_consume_exactly("**") {
      Op::POW
    } else if self.try_consume_exactly("*") {
      Op::MUL
    } else if self.try_consume_exactly("/") {
      Op::DIV
    } else if self.try_consume_exactly("%") {
      Op::REM
    } else if self.try_consume_exactly("<<") {
      Op::SHL
    } else if self.try_consume_exactly(">>") {
      Op::SHR
    } else if self.try_consume_exactly("<=") {
      Op::LE
    } else if self.try_consume_exactly(">=") {
      Op::GE
    } else if self.try_consume_exactly("<") {
      Op::LT
    } else if self.try_consume_exactly(">") {
      Op::GT
    } else if self.try_consume_exactly("==") {
      Op::EQ
    } else if self.try_consume_exactly("!=") {
      Op::NEQ
    } else if self.try_consume_exactly("&") {
      Op::AND
    } else if self.try_consume_exactly("|") {
      Op::OR
    } else if self.try_consume_exactly("^") {
      Op::XOR
    } else {
      return None;
    };
    Some(opr)
  }

  fn peek_oper(&mut self) -> Option<Op> {
    let opr = if self.starts_with("+") {
      Op::ADD
    } else if self.starts_with("-") {
      Op::SUB
    } else if self.starts_with("**") {
      Op::POW
    } else if self.starts_with("*") {
      Op::MUL
    } else if self.starts_with("/") {
      Op::DIV
    } else if self.starts_with("%") {
      Op::REM
    } else if self.starts_with("<<") {
      Op::SHL
    } else if self.starts_with(">>") {
      Op::SHR
    } else if self.starts_with("<=") {
      Op::LE
    } else if self.starts_with(">=") {
      Op::GE
    } else if self.starts_with("<") {
      Op::LT
    } else if self.starts_with(">") {
      Op::GT
    } else if self.starts_with("==") {
      Op::EQ
    } else if self.starts_with("!=") {
      Op::NEQ
    } else if self.starts_with("&") {
      Op::AND
    } else if self.starts_with("|") {
      Op::OR
    } else if self.starts_with("^") {
      Op::XOR
    } else {
      return None;
    };
    Some(opr)
  }

  fn parse_u32(&mut self) -> ParseResult<u32> {
    let radix = match self.peek_many(2) {
      Some("0x") => {
        self.advance_many(2);
        Radix::Hex
      }
      Some("0b") => {
        self.advance_many(2);
        Radix::Bin
      }
      _ => Radix::Dec,
    };
    let num_str = self.take_while(move |c| c.is_digit(radix as u32) || c == '_');
    let num_str = num_str.chars().filter(|c| *c != '_').collect::<String>();

    let next_is_hex = self.peek_one().map_or(false, |c| "0123456789abcdefABCDEF".contains(c));
    if next_is_hex || num_str.is_empty() {
      self.expected(format!("valid {radix} digit").as_str())
    } else {
      u32::from_str_radix(&num_str, radix as u32)
        .map_err(|e| self.expected_and::<u64>("integer", &e.to_string()).unwrap_err())
    }
  }

  fn u32_with_radix(&mut self, radix: Radix) -> ParseResult<u32> {
    let num_str = self.take_while(move |c| c.is_digit(radix as u32) || c == '_');
    let num_str = num_str.chars().filter(|c| *c != '_').collect::<String>();
    let next_is_hex = self.peek_one().map_or(false, |c| "0123456789abcdefABCDEF".contains(c));
    if next_is_hex || num_str.is_empty() {
      self.expected(format!("valid {radix} digit").as_str())
    } else {
      u32::from_str_radix(&num_str, radix as u32)
        .map_err(|e| self.expected_and::<u64>("integer", &e.to_string()).unwrap_err())
    }
  }

  fn parse_number(&mut self) -> ParseResult<Num> {
    let ini_idx = *self.index();
    let sign = if self.try_consume_exactly("+") {
      Some(1)
    } else if self.try_consume_exactly("-") {
      Some(-1)
    } else {
      None
    };
    let radix = match self.peek_many(2) {
      Some("0x") => {
        self.advance_many(2);
        Radix::Hex
      }
      Some("0b") => {
        self.advance_many(2);
        Radix::Bin
      }
      _ => Radix::Dec,
    };
    let num = self.u32_with_radix(radix)?;
    let frac = if let Some('.') = self.peek_one() {
      self.advance_one();
      let fra_str = self.take_while(|c| c.is_digit(radix as u32) || c == '_');
      let fra_str = fra_str.chars().filter(|c| *c != '_').collect::<String>();
      let fra = u32::from_str_radix(&fra_str, radix as u32)
        .map_err(|e| self.expected_and::<u64>("integer", &e.to_string()).unwrap_err())?;
      let fra = fra as f32 / (radix.to_f32()).powi(fra_str.len() as i32);
      Some(fra)
    } else {
      None
    };

    if let Some(frac) = frac {
      let sign = sign.unwrap_or(1);
      return Ok(Num::F24(sign as f32 * (num as f32 + frac)));
    }

    if let Some(sign) = sign {
      let num = sign * num as i32;
      if !(-0x00800000..=0x007fffff).contains(&num) {
        return self.num_range_err(ini_idx, "I24");
      }
      Ok(Num::I24(num))
    } else {
      if num >= 1 << 24 {
        return self.num_range_err(ini_idx, "U24");
      }
      Ok(Num::U24(num))
    }
  }

  fn num_range_err<T>(&mut self, ini_idx: usize, typ: &str) -> ParseResult<T> {
    let msg = format!("\x1b[1mNumber literal outside of range for {}.\x1b[0m", typ);
    let end_idx = *self.index();
    self.with_ctx(Err(msg), ini_idx..end_idx)
  }

  /// Parses up to 4 base64 characters surrounded by "`".
  /// Joins the characters into a u24 and returns it.
  fn parse_quoted_symbol(&mut self) -> ParseResult<u32> {
    self.consume_exactly("`")?;
    let mut result = 0;
    let mut count = 0;
    while count < 4 {
      if self.starts_with("`") {
        break;
      }
      count += 1;
      let Some(c) = self.advance_one() else { self.expected("base_64 character")? };
      let c = c as u8;
      let nxt = match c {
        b'A'..=b'Z' => c - b'A',
        b'a'..=b'z' => c - b'a' + 26,
        b'0'..=b'9' => c - b'0' + 52,
        b'+' => 62,
        b'/' => 63,
        _ => return self.expected("base64 character"),
      };
      result = (result << 6) | nxt as u32;
    }
    self.consume_exactly("`")?;
    Ok(result)
  }

  fn check_repeated_ctr_fields(
    &mut self,
    fields: &[CtrField],
    ctr_name: &Name,
    span: Range<usize>,
  ) -> ParseResult<()> {
    for i in 0..fields.len() {
      let field = &fields[i];
      if fields.iter().skip(i + 1).any(|a: &CtrField| a.nam == field.nam) {
        let msg = format!("Found a repeated field '{}' in constructor {}.", field.nam, ctr_name);
        return self.with_ctx(Err(msg), span);
      }
    }
    Ok(())
  }

  fn redefinition_of_function_msg(builtin: bool, function_name: &str) -> String {
    if builtin {
      format!("Redefinition of builtin (function) '{function_name}'.")
    } else {
      format!("Redefinition of function '{function_name}'.")
    }
  }

  fn redefinition_of_hvm_msg(builtin: bool, function_name: &str) -> String {
    if builtin {
      format!("Redefinition of builtin (native HVM function) '{function_name}'.")
    } else {
      format!("Redefinition of native HVM function '{function_name}'.")
    }
  }

  fn redefinition_of_constructor_msg(constructor_name: &str) -> String {
    if crate::fun::builtins::BUILTIN_CTRS.contains(&constructor_name) {
      format!("Redefinition of builtin (constructor) '{constructor_name}'.")
    } else {
      format!("Redefinition of constructor '{constructor_name}'.")
    }
  }

  fn redefinition_of_type_msg(type_name: &str) -> String {
    if crate::fun::builtins::BUILTIN_TYPES.contains(&type_name) {
      format!("Redefinition of builtin (type) '{type_name}'.")
    } else {
      format!("Redefinition of type '{type_name}'.")
    }
  }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Radix {
  Bin = 2,
  Dec = 10,
  Hex = 16,
}

impl Radix {
  fn to_f32(self) -> f32 {
    match self {
      Radix::Bin => 2.,
      Radix::Dec => 10.,
      Radix::Hex => 16.,
    }
  }
}

impl std::fmt::Display for Radix {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Radix::Bin => write!(f, "binary"),
      Radix::Dec => write!(f, "decimal"),
      Radix::Hex => write!(f, "hexadecimal"),
    }
  }
}
