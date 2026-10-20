#!/usr/bin/env python3
"""Reproduce independent destination optimizations in isolated compiler copies."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import time

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
OPTIONS = dict(fused=0, reuse=1, specialize=2, return_=4, join=8, all=15,
               reuse_spec=3, reuse_return=5, reuse_spec_return=7,
               join_once=16, reuse_join=17, all_once=23)
OPTIONS['return'] = OPTIONS.pop('return_')


def run(cmd, **kwargs):
    r = subprocess.run(cmd, capture_output=True, text=True, timeout=240, **kwargs)
    if r.returncode:
        raise RuntimeError(f'{cmd}\n{r.stdout}\n{r.stderr}')
    return r


def instrument(source):
    fids = re.findall(r'^#define (FID_\w+) (\d+)$', source, re.M)
    text = 'static __thread u64 dp_calls[' + str(max(int(i) for _, i in fids)+1) + '];\n'
    text += 'static __thread u64 dp_writes, dp_alloc, dp_free, dp_root, dp_join, dp_dest;\n'
    text += '#define PUTSTK(I) (*(dp_writes++, &STK(I)))\n'
    text += '__attribute__((destructor)) static void dp_report(void) {\n'
    for name, i in fids:
        text += f'if(dp_calls[{i}]) fprintf(stderr,"{name}=%llu\\n",(unsigned long long)dp_calls[{i}]);\n'
    for name in ['writes', 'alloc', 'free', 'root', 'join', 'dest']:
        text += f'fprintf(stderr,"{name}=%llu\\n",(unsigned long long)dp_{name});\n'
    text += '}\n'
    at = source.index('typedef struct {')
    source = source[:at]+text+source[at:]
    source = re.sub(r'^(  WL_CASE\((FID_\w+)\)\n)', r'\1    dp_calls[\2]++;\n', source, flags=re.M)
    source = re.sub(r'STK\(([^)]+)\)(\s*=\s*(?!=))', r'PUTSTK(\1)\2', source)
    source = source.replace('INLINE u64 heap_alloc(Env e, u32 cls) {',
                            'INLINE u64 heap_alloc(Env e, u32 cls) { dp_alloc++;')
    source = source.replace('INLINE void heap_free(Env e, u32 cls, u64 loc) {',
                            'INLINE void heap_free(Env e, u32 cls, u64 loc) { dp_free++;')
    source = source.replace('e.mem[STK(0) >> 16] = r0;', 'dp_root++; e.mem[STK(0) >> 16] = r0;')
    source = source.replace('if (STK(-1) >> 16) {', 'if ((dp_dest++, STK(-1) >> 16)) {')
    for value in ['cont', 'next']:
        line = f'u64 loc = term_loc({value}), tl = task_tail({value});'
        source = source.replace(line, 'dp_join++; '+line)
    return source


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--phase', choices=['phase1', 'phase1-measured', 'phase2', 'selected', 'pr'], default='phase1')
    p.add_argument('--variants', help='Comma-separated variants; base is the original compiler')
    p.add_argument('--bench', action='append')
    p.add_argument('--backend', choices=['cpu', 'gpu'], default='cpu')
    p.add_argument('--threads', type=int, default=1)
    p.add_argument('--repetitions', type=int, default=5)
    p.add_argument('--mode', choices=['runtime', 'compiler', 'profile'], default='runtime')
    a = p.parse_args()
    if a.mode == 'profile' and (a.backend != 'cpu' or a.threads != 1):
        p.error('Profile counters require CPU execution with one thread')
    if a.repetitions < 1:
        p.error('--repetitions must be positive')
    chosen = a.phase in {'selected', 'pr'}
    vs = a.variants.split(',') if a.variants else (['base', 'selected'] if chosen
        else ['base', 'fused', 'reuse', 'specialize', 'return', 'join', 'all'])
    allowed = {'base', 'selected'} if chosen else {'base', *OPTIONS}
    if not set(vs) <= allowed or (a.phase.startswith('phase1') and any(OPTIONS.get(v, 0) & 16 for v in vs)):
        p.error('Variant is not supported by this phase')
    meta = json.loads((HERE/'metadata.json').read_text())
    work = Path(tempfile.mkdtemp(prefix='bend-dps-options-'))
    print(f'Artifacts: {work}', flush=True)
    (work/'base').mkdir()
    archive = subprocess.run(['git', 'archive', meta['baseline_git_commit'], 'bend2'],
                             cwd=ROOT, capture_output=True, check=True).stdout
    subprocess.run(['tar', '-xf', '-', '-C', str(work/'base')],
                   input=archive, check=True)
    if hashlib.sha256((work/'base/bend2/comp.ts').read_bytes()).hexdigest() != meta['baseline_comp_sha256']:
        p.error('Recorded baseline compiler hash differs')
    shutil.copytree(ROOT/'bend2' if a.phase == 'pr' else work/'base/bend2',
                    work/'experimental/bend2',
                    ignore=shutil.ignore_patterns('docs', 'pack', 'node_modules'))
    if a.phase != 'pr':
        run(['patch', '-p1', '-i', str(HERE/(a.phase+'.patch'))], cwd=work/'experimental')
    if a.mode == 'compiler':
        for v in ['base', 'experimental']:
            comp = work/v/'bend2/comp.ts'
            source = comp.read_text().replace('export function compile_book(book: Bend.Book): string {',
                                              'function compile_book_inner(book: Bend.Book): string {')
            source += '\nexport function compile_book(book: Bend.Book): string { const t=performance.now(); const c=compile_book_inner(book); process.stderr.write("COMPILE_MS="+(performance.now()-t)+"\\n"); return c; }\n'
            comp.write_text(source)
    run(['clang', '-O2', str(HERE.parent/'tm.c'), '-o', str(work/'tm')])
    sources = {}
    for n in a.bench or ['tree-matmul', 'kmeans', 'tree-bitonic', 'tree-radix']:
        sources[n] = next((s for s in [ROOT/'bench/runtime'/n/'main.bend', ROOT/'demos'/n/'main.bend'] if s.is_file()), None)
        if sources[n] is None:
            p.error('Unknown benchmark/demo: '+n)
    flags = ['-O3', '-std=c11']
    libs = ['-lpthread', '-lm']
    if a.backend == 'gpu':
        flags += ['-DBEND_METAL=1', '-x', 'objective-c', '-fobjc-arc']
        libs += ['-framework', 'Metal', '-framework', 'Foundation']

    def build(n, v):
        binary = work/(n+'-'+v)
        c = binary.with_suffix('.c')
        env = dict(os.environ, BEND_DPS='1', BEND_DPS_ASYNC='1', BEND_DPS_OPT=str(OPTIONS.get(v, 0)))
        t = time.perf_counter()
        r = run(['bun', str(work/('base' if v == 'base' else 'experimental')/'bend2/main.ts'),
                 str(sources[n]), '-o', str(c)], env=env)
        cli_ms = 1000*(time.perf_counter()-t)
        if a.mode == 'profile':
            c.write_text(instrument(c.read_text()))
        t = time.perf_counter()
        run(['clang', *flags, str(c), *libs, '-o', str(binary)])
        row = dict(cli_ms=cli_ms, native_ms=1000*(time.perf_counter()-t), c_bytes=c.stat().st_size,
                   binary_bytes=binary.stat().st_size)
        if a.mode == 'compiler':
            row['emit_ms'] = float(re.search(r'COMPILE_MS=([\d.]+)', r.stderr)[1])
        if a.backend == 'gpu':
            t = time.perf_counter()
            run([str(binary), '--gpu-build'])
            row['metal_ms'] = 1000*(time.perf_counter()-t)
        return row

    if a.mode != 'compiler':
        for n in sources:
            for v in vs:
                build(n, v)
    rows = []
    reps = [0] if a.mode == 'profile' else range(-1, a.repetitions)
    for rep in reps:
        for n in sources:
            outs = []
            order = vs[rep % len(vs):]+vs[:rep % len(vs)]
            if rep % 2:
                order.reverse()
            for v in order:
                row = dict(name=n, variant=v, rep=rep, threads=a.threads)
                if a.mode == 'compiler':
                    row.update(build(n, v))
                else:
                    span = '4GB' if n in ['tree-matmul', 'tree-radix'] else '1GB'
                    r = run([str(work/'tm'), str(work/'time.txt'), str(work/(n+'-'+v)),
                             '--threads', str(a.threads), '--gpu', span if a.backend == 'gpu' else 'off'])
                    code, seconds, rss = (work/'time.txt').read_text().split()
                    row.update(code=int(code), seconds=float(seconds), rss=int(rss), out=r.stdout.strip(), err=r.stderr.strip())
                    if row['code']:
                        raise RuntimeError(row)
                    outs.append(row['out'])
                    if a.mode == 'profile':
                        row['counts'] = {k: int(v) for k, v in re.findall(r'^(\w+)=(\d+)$', r.stderr, re.M)}
                rows.append(row)
                (work/'results.json').write_text(json.dumps(rows, indent=2)+'\n')
                print(json.dumps(row), flush=True)
            if outs and len(set(outs)) != 1:
                raise RuntimeError('Output mismatch: '+n)


if __name__ == '__main__':
    main()
