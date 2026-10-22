// The effects of lint.bend. bend-lint puts its channel on
// globalThis.BEND_LINT before it runs a rule, and removes it after. Bool
// and U32 are native JS values here; a String is Bend's text, so host text
// goes in with str_in and comes out with str_out; other data is
// {$: CID(Name), ...}.

const LINT_SEVERITY = {
  [CID(Error)]: "error",
  [CID(Warning)]: "warning",
  [CID(Information)]: "information",
  [CID(Hint)]: "hint",
};
const LINT_QUANTITY = { erased: CID(Erased), once: CID(Once), many: CID(Many) };
const LINT_APPLICABILITY = {
  [CID(Safe)]: "safe",
  [CID(Suggested)]: "suggested",
  [CID(Dangerous)]: "dangerous",
};

function lint_host() {
  const host = globalThis.BEND_LINT;
  if (host === undefined) {
    throw new Error(
      "this program is a rule; run it with bun tools/bend-lint/src/lint.ts <file.bend> --rules <rule.bend>",
    );
  }
  return host;
}

function lint_list(xs) {
  return xs.reduceRight((tail, head) => ({ $: CID(Con), head, tail }), { $: CID(Nil) });
}

function lint_unlist(xs) {
  const out = [];
  for (; xs.$ === CID(Con); xs = xs.tail) {
    out.push(xs.head);
  }
  return out;
}

function lint_maybe(x) {
  return x === undefined ? { $: CID(None) } : { $: CID(Some), value: x };
}

function lint_span(s) {
  return { $: CID(Span), path: str_in(s.path), beg: s.beg, end: s.end };
}

function lint_spot(s) {
  return { path: str_out(s.path), beg: s.beg, end: s.end };
}

function lint_value(v) {
  return typeof v === "number"
    ? { $: CID(OptionNum), value: v }
    : typeof v === "boolean"
      ? { $: CID(OptionFlag), value: v }
      : { $: CID(OptionText), value: str_in(v) };
}

function lint_term(id) {
  return { $: CID(Term), id };
}

function lint_node(id) {
  return { $: CID(Node), id };
}

function lint_fact(id) {
  return lint_maybe(id === undefined ? undefined : { $: CID(Fact), id });
}

function lint_input() {
  const { sources, options } = lint_host().input();
  return {
    $: CID(Input),
    sources: lint_list(
      sources.map((s) => ({
        $: CID(Source),
        path: str_in(s.path),
        text: str_in(s.text),
        root: s.root,
      })),
    ),
    options: lint_list(
      Object.entries(options).map(([key, v]) => ({
        $: CID(Option),
        key: str_in(key),
        value: lint_value(v),
      })),
    ),
  };
}

function lint_report(diags) {
  lint_host().report(
    lint_unlist(diags).map((d) => ({
      severity: LINT_SEVERITY[d.severity.$],
      message: str_out(d.message),
      span: d.span.$ === CID(Some) ? lint_spot(d.span.value) : undefined,
      fixes: lint_unlist(d.fixes).map((f) => ({
        title: str_out(f.title),
        applicability: LINT_APPLICABILITY[f.applicability.$],
        edits: lint_unlist(f.edits).map((e) => ({
          span: lint_spot(e.span),
          text: str_out(e.text),
        })),
      })),
    })),
  );
  return { $: CID(Unit) };
}

function lint_view(fact) {
  const v = lint_host().view(fact.id);
  return {
    $: CID(View),
    owner: str_in(v.owner),
    inst: v.inst,
    kind: str_in(v.kind),
    name: str_in(v.name),
    quantity: { $: LINT_QUANTITY[v.quantity] },
    span: lint_maybe(v.span && lint_span(v.span)),
    inner: lint_maybe(v.inner && lint_span(v.inner)),
  };
}

io_eff(CID(input), lint_input);
io_eff(CID(report), lint_report);
io_eff(CID(next_fact), () => lint_fact(lint_host().next()));
io_eff(CID(view), lint_view);
io_eff(CID(type_of), (fact) => lint_term(lint_host().type(fact.id)));
io_eff(CID(binder), (fact) => {
  const id = lint_host().binder(fact.id);
  return lint_maybe(id === undefined ? undefined : lint_term(id));
});
io_eff(CID(same), (fact, a, b) => lint_host().same(fact.id, a.id, b.id));
io_eff(CID(show), (fact, t) => str_in(lint_host().show(fact.id, t.id)));
io_eff(CID(normal), (fact, t) => lint_term(lint_host().normal(fact.id, t.id)));
io_eff(CID(uses), (fact) =>
  lint_list(
    lint_host()
      .uses(fact.id)
      .map((u) => ({
        $: CID(Use),
        name: str_in(u.name),
        quantity: { $: LINT_QUANTITY[u.quantity] },
      })),
  ),
);
io_eff(CID(text), (span) => str_in(lint_host().text(lint_spot(span))));
io_eff(CID(body), (name) => {
  const id = lint_host().body(str_out(name));
  return lint_maybe(id === undefined ? undefined : lint_node(id));
});
io_eff(CID(node), (fact) => lint_node(lint_host().node(fact.id)));
io_eff(CID(shape), (n) => {
  const s = lint_host().shape(n.id);
  return {
    $: CID(Shape),
    kind: str_in(s.kind),
    name: str_in(s.name),
    span: lint_maybe(s.span && lint_span(s.span)),
    children: lint_list(s.children.map(lint_node)),
  };
});
io_eff(CID(fact), (n) => lint_fact(lint_host().fact(n.id)));
