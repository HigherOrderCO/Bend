io_eff(CID(verify), (ops, p, q, b, c, right, r, i, e) => {
  const check = (ok) => {
    if (!ok) throw new Error("incorrect JS record reuse");
  };
  [p, q, b, c, right, r, i, e].forEach(Object.freeze);
  check(ops.keep(p) === p);
  check(ops.nested(b) === b);
  check(ops.choice(c) === c);
  check(ops.hold(p).f(0) === p);
  check(ops.hold2(p).f(0).f(0) === p);
  check(ops.hold_box(b).f(0) === b);
  check(ops.indexed(r) === r);
  check(ops.item(i) === i);
  check(ops.empty(e) === e);
  const changed = ops.changed(p);
  check(changed !== p && changed.x === 4 && changed.y === 9);
  const swapped = ops.swapped(p);
  check(swapped !== p && swapped.x === 9 && swapped.y === 3);
  const cross = ops.cross(p)(q);
  check(cross !== p && cross !== q && cross.x === 3 && cross.y === 20);
  const nested = ops.nested_change(b);
  check(nested !== b && nested.point !== p && nested.point.y === 10);
  const retag = ops.retag(c);
  check(retag !== c && retag.$ === right.$ && retag.point === p);
  return { $: CID(Unit) };
});
