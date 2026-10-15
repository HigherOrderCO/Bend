// The effects of lint.bend. bend-lint puts its channel on
// globalThis.BEND_LINT before it runs a rule, and removes it after. Bool,
// String and U32 are native JS values here; other data is {$: CID(Name), ...}.

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
  return { $: CID(Span), path: s.path, beg: s.beg, end: s.end };
}

function lint_spot(s) {
  return { path: s.path, beg: s.beg, end: s.end };
}

function lint_value(v) {
  return typeof v === "number"
    ? { $: CID(Num), value: v }
    : typeof v === "boolean"
      ? { $: CID(Flag), value: v }
      : { $: CID(Text), value: v };
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
      sources.map((s) => ({ $: CID(Source), path: s.path, text: s.text, root: s.root })),
    ),
    options: lint_list(
      Object.entries(options).map(([key, v]) => ({ $: CID(Option), key, value: lint_value(v) })),
    ),
  };
}

function lint_report(diags) {
  lint_host().report(
    lint_unlist(diags).map((d) => ({
      severity: LINT_SEVERITY[d.severity.$],
      message: d.message,
      span: d.span.$ === CID(Some) ? lint_spot(d.span.value) : undefined,
      fixes: lint_unlist(d.fixes).map((f) => ({
        title: f.title,
        applicability: LINT_APPLICABILITY[f.applicability.$],
        edits: lint_unlist(f.edits).map((e) => ({ span: lint_spot(e.span), text: e.text })),
      })),
    })),
  );
  return { $: CID(Unit) };
}

function lint_view(fact) {
  const v = lint_host().view(fact.id);
  return {
    $: CID(View),
    owner: v.owner,
    inst: v.inst,
    kind: v.kind,
    name: v.name,
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
io_eff(CID(show), (fact, t) => lint_host().show(fact.id, t.id));
io_eff(CID(normal), (fact, t) => lint_term(lint_host().normal(fact.id, t.id)));
io_eff(CID(uses), (fact) =>
  lint_list(
    lint_host()
      .uses(fact.id)
      .map((u) => ({ $: CID(Use), name: u.name, quantity: { $: LINT_QUANTITY[u.quantity] } })),
  ),
);
io_eff(CID(text), (span) => lint_host().text(lint_spot(span)));
io_eff(CID(body), (name) => {
  const id = lint_host().body(name);
  return lint_maybe(id === undefined ? undefined : lint_node(id));
});
io_eff(CID(node), (fact) => lint_node(lint_host().node(fact.id)));
io_eff(CID(shape), (n) => {
  const s = lint_host().shape(n.id);
  return {
    $: CID(Shape),
    kind: s.kind,
    name: s.name,
    span: lint_maybe(s.span && lint_span(s.span)),
    children: lint_list(s.children.map(lint_node)),
  };
});
io_eff(CID(fact), (n) => lint_fact(lint_host().fact(n.id)));
