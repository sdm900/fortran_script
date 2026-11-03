# AGENTS.md

Legacy Fortran scripting-language interpreter. No CI, no linter/formatter, no package manager. Three binaries, two test runners.

## Build (gfortran, verified)

- `make` builds all three binaries with system gfortran (15.x): plain `make` (or `make check` / `make shell` / `make embedcheck`), `make test` runs both suites, `make clean` removes objects, binaries and generated deps. `F90COMPILER`/`F90FLAGS` can be overridden, e.g. `make F90FLAGS="-O0 -g -fcheck=all -Wall"`.
- `mkdir -p obj bin` is handled by the `Makefile` via order-only prerequisites.
- `obj/check.o`, `obj/shell.o` and `obj/embedcheck.o` each define `main` — never link `obj/*.o` together; use the three link lines (`bin/check` needs `checkscrpt.o`, `bin/shell` links `shell.o` instead of `check.o`, `bin/embedcheck` links `embedcheck.o` instead of `check.o`).
- `src/*.f90` use `#include "macros.h"`; gfortran handles this directly with `-cpp -I./src` (no external `cpp -P -C -traditional-cpp` staging step).

## Generated source / deps

- `src/params.f90` is generated: `m4 src/params_m4.f90 > src/params.f90`. Edit `params_m4.f90`, not `params.f90`. (m4 is installed; the Makefile rule regenerates automatically.) Note m4 expands `__I`/`__II` even inside Fortran `!` comments, so keep those identifiers out of comment text.
- `./build_deps` regenerates `src/.*.dep` make dependencies from `src/LIST` plus `check.f90`/`shell.f90`/`embedcheck.f90`; run from repo root. Its `use`-regex also matches the word `use` inside `!` comments, so never write `<x>use <word>` prose in a comment (it invents a phantom `src/<word>.f90` prerequisite and breaks the build).

## Tests

- Runners are `bin/check` (sources `src/check.f90` + `src/checkscrpt.f90`, 63 script tests) and `bin/embedcheck` (sources `src/embedcheck.f90`, 26 multi-call embedding checks: sequential `interpret()` calls in one process, exactly the host-use pattern); `bin/shell` (`src/shell.f90`) is the interactive interpreter, not a test. `make test` runs both runners.
- Must run from repo root: `checkscrpt.f90` hardcodes `tests/<name>.in` / `.out` / `.exp` relative paths. `bin/embedcheck` uses only `STATUS='SCRATCH'` files, so it runs anywhere.
- `bin/check` overwrites `tests/*.out` on every run, then diffs against `tests/*.exp`. Restore unrelated churn after running (`git checkout -- tests/`), keeping only intended `.exp`/test changes.
- Not every test has an `.exp` file. Inline expectations live in `src/check.f90`: error-string checks (test1-2nd call, test6, test17, test30-35, test38-40, test42-44, test47-54, test57, test59, test60, test62, test63), scalar/array API checks (test23/24 use `get_scalar`/`get_array`), count checks (test25/26, no `.in` files). Tests without `.exp` (17, 23, 24, 30–35, 38–40, 42–44, 47–54, 57, 59, 60, 62, 63) are expected.
- Subroutines/functions are implemented (tests 28/29/36/37/41/45/46/55/56/58/61 are output tests; 34/35/38/39/40/42/43/44/47–54/57/59/60/62/63 assert error paths). The suite runs to completion (63 checks); a hang means a new regression, not a known issue.
- Run wrapped: `timeout -s KILL 60 ./bin/check || true` (bare `timeout` + `set -e` aborts the shell on 124).

## Architecture

- Entry: `fscript%interpret` (`src/fscript.f90`, also exposes the embed API: `get_scalar`, `get_array*`, `number_scalars/arrays`) → `parse%parsedriver` (`src/parse.f90`).
- Module compile order = `src/LIST` (params → globals → error → programnodes → string → variables → extrafunc → interpreter → progstack → callstack → evalexec → parse → fscript → checkscrpt).
- `src/interpreter.f90` (~1070 lines) and `src/extrafunc.f90` (~2580 lines, intrinsic function table) are the bulk; start there, not leaf files.
- The expression parser/evaluator in `src/interpreter.f90` (`s_readnumber`, `s_parse*`, `s_evaluate`, Bessel/`gamma` domain guards) shares lineage with `../fortran_parser/interpreter.f90`. Parser fixes already ported here: IOSTAT number parsing (`'Not a number'`), `parsedepth` cap (1000, `'Maximum nesting depth exceeded'`), `**` huge-exponent guard, `besjn/besy0/besy1/besyn/besin/besk0/besk1/beskn` domain guards, `gamma` pole/magnitude guards. Not portable (script lacks these features): `//`/`%`/`floor`/`ceiling`/`mod` guards, varvals arity tracking, `s_variableerr` MERGE fix, error-propagation stability check. Script-specific adaptations: sticky `seterror` (error module, line-number prefix) instead of `s_seterror`, `endfunc_` instead of `end_` sentinel.
- Follow-up hardening (no parser equivalent): `parsedriver` block-depth cap (1000) plus `parseline` restoring `progp` past multi-line constructs so later lines never reuse stale unevaluated body nodes (this was the `test28` hang); `DO` rejects zero increment and respects negative steps; expression array subscripts use `nint` consistently with writes; array auto-growth is capped at `maxarrayelems` (params) with doubling realloc, `stat=` checks, and `mask=.false.` on fresh/moved cells; `cachedfuncs` slots are recycled via freelist across `interpret` calls (entries must live for the whole session since `DO`/`IF` re-evaluate stored functions); `createarr` initialises `mask`.
- Subroutines/functions (`src/evalexec.f90` `driveroutine`/`drive_function`/`execcall`, `src/variables.f90` child namespaces + routine registry, `src/parse.f90` `parsefunction`/`parsedummies`/`parseendfunction`/`parsereturn`, `src/interpreter.f90` userfn expression parsing via `userfn_hook` set by `fscript`): scalar variable actuals alias the caller slot (pass-by-reference); array elements are copy-in/copy-out with frozen indices; other actuals are temps; function args are always copies. Routine bodies bind assignment targets by *name* (`parsevar`/`parsearr` store the name via `pushvarstr`, `evalvar`/`evalarr` resolve with `varnum`/`arraynum` in the call-time child namespace) — never by definition-time index, which would misresolve once dummies shift the namespace layout. Each call re-evaluates fresh scratch-node copies of the body (parsed once into templates), with a `progp_bias` so stored jump targets land in the scratch range and error lines map back to template lines; scratch ranges are cached per routine across calls (high-water-mark, invalidated by `invalidatescratch()` from `resetprogstack`/`clearsubs` — validity also rests on no `truncnodes` of a live range) and only refreshed via `copynodes` (O(body), not O(script)); teardown destroys the call's cachedfuncs but keeps the nodes, with `fn_exists()` guarding the destroy scan against stale template references. `progp_bias` lives in `globals` so `error%seterror` can un-bias line numbers. Error line numbers print with `(i0)` (no column shift past 99999 lines); `TRIM(txtnum)` at the concat site, since `txtnum` is now wider than the number. Gotchas fixed along the way: zero-length `func(:2)` substring (Fortran `.and.` does not short-circuit — use nested `if`s, same for the `fn_exists` bounds/associated guard), `lenfunc` exceeding the actual buffer in `finduserfn` (clamp with `len()`), shared `components` counter clobbered by nested userfn parses (use a local).
- Expression IDs (`variablestart+i`, `arraystart+i` in `s_readtext`, `s_evaluate`) only span their 1001-wide windows, so namespace size is capped to match: `createvar`/`createarr` reject past `variableend-variablestart+1` / `arrayend-arraystart+1` (`'Too many variables'`/`'Too many arrays'`, tests 62/63), and `childnamespace` re-checks after inheriting (a child can exceed the window even when the parent did not). Live expression slots are capped by the `functionstart..functionend` window in `getfnslot` (`'Too many expressions'`). `createfn`/`createfnstr` share that allocator.
- Cross-call state (the embedding pattern: many sequential `interpret()` calls in one process, guarded by `bin/embedcheck`'s 26 checks): all pools are high-water-mark arenas, not leaks — `resetvarstack` zeroes `nvars`/`narrs` so `varvals`/`arrvals` slots are recycled (components `deallocate`d *and* `nullify`d), `cachedfuncs` slots recycle via freelist, `subs`/`nodes`/`cstack` only grow to the per-process maximum; routine scratch ranges persist across calls by design. RSS is flat over 300 mixed array/routine calls. `initfscript` resets unconditionally (never gated on `calldepth == 1`), plus `resetsubdepth()`/`resetblockdepth()`, so one bad call cannot poison the next.

## Git

Only commit when the user explicitly asks for a commit **after a change**. Never commit, amend, or push without explicit permission. A plan or proposed work must never include a commit step; if a commit is needed, ask for permission separately.

Every commit made by an AI assistant MUST include a trailer noting OpenCode authorship and the active model, e.g.:

```
Committed-by: OpenCode (model: <provider>/<model>)
```
