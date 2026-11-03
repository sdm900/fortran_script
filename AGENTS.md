# AGENTS.md

Legacy Fortran scripting-language interpreter. No CI, no linter/formatter, no package manager. Two binaries, one test runner.

## Build (gfortran, verified)

- `make` builds both binaries with system gfortran (15.x): plain `make` (or `make check` / `make shell`), `make test` runs the suite, `make clean` removes objects, binaries and generated deps. `F90COMPILER`/`F90FLAGS` can be overridden, e.g. `make F90FLAGS="-O0 -g -fcheck=all -Wall"`.
- `mkdir -p obj bin` is handled by the `Makefile` via order-only prerequisites.
- `obj/check.o` and `obj/shell.o` each define `main` — never link `obj/*.o` together; use the two link lines (`bin/check` needs `checkscrpt.o`, `bin/shell` links `shell.o` instead of `check.o`).
- `src/*.f90` use `#include "macros.h"`; gfortran handles this directly with `-cpp -I./src` (no external `cpp -P -C -traditional-cpp` staging step).

## Generated source / deps

- `src/params.f90` is generated: `m4 src/params_m4.f90 > src/params.f90`. Edit `params_m4.f90`, not `params.f90`. Note `m4` is not installed in this container.
- `./build_deps` regenerates `src/.*.dep` make dependencies from `src/LIST` plus `check.f90`/`shell.f90`; run from repo root.

## Tests

- Runner is `bin/check` (sources `src/check.f90` + `src/checkscrpt.f90`); `bin/shell` (`src/shell.f90`) is the interactive interpreter, not a test.
- Must run from repo root: `checkscrpt.f90` hardcodes `tests/<name>.in` / `.out` / `.exp` relative paths.
- `bin/check` overwrites `tests/*.out` on every run, then diffs against `tests/*.exp`. Restore unrelated churn after running (`git checkout -- tests/`), keeping only intended `.exp`/test changes.
- Not every test has an `.exp` file. Inline expectations live in `src/check.f90`: error-string checks (test1-2nd call, test6, test17, test30-35, test38-44), scalar/array API checks (test23/24 use `get_scalar`/`get_array`), count checks (test25/26, no `.in` files). Tests without `.exp` (17, 23, 24, 30, 31, 32, 33, 34, 35, 38, 39, 40, 42, 43, 44) are expected.
- Subroutines/functions are implemented (tests 28/29/36/37/41 are output tests; 34/35/38/39/40/42/43/44 assert error paths). The suite runs to completion (44 checks); a hang means a new regression, not a known issue.
- Run wrapped: `timeout -s KILL 60 ./bin/check || true` (bare `timeout` + `set -e` aborts the shell on 124).

## Architecture

- Entry: `fscript%interpret` (`src/fscript.f90`, also exposes the embed API: `get_scalar`, `get_array*`, `number_scalars/arrays`) → `parse%parsedriver` (`src/parse.f90`).
- Module compile order = `src/LIST` (params → globals → error → programnodes → string → variables → extrafunc → interpreter → progstack → callstack → evalexec → parse → fscript → checkscrpt).
- `src/interpreter.f90` (~1070 lines) and `src/extrafunc.f90` (~2580 lines, intrinsic function table) are the bulk; start there, not leaf files.
- The expression parser/evaluator in `src/interpreter.f90` (`s_readnumber`, `s_parse*`, `s_evaluate`, Bessel/`gamma` domain guards) shares lineage with `../fortran_parser/interpreter.f90`. Parser fixes already ported here: IOSTAT number parsing (`'Not a number'`), `parsedepth` cap (1000, `'Maximum nesting depth exceeded'`), `**` huge-exponent guard, `besjn/besy0/besy1/besyn/besin/besk0/besk1/beskn` domain guards, `gamma` pole/magnitude guards. Not portable (script lacks these features): `//`/`%`/`floor`/`ceiling`/`mod` guards, varvals arity tracking, `s_variableerr` MERGE fix, error-propagation stability check. Script-specific adaptations: sticky `seterror` (error module, line-number prefix) instead of `s_seterror`, `endfunc_` instead of `end_` sentinel.
- Follow-up hardening (no parser equivalent): `parsedriver` block-depth cap (1000) plus `parseline` restoring `progp` past multi-line constructs so later lines never reuse stale unevaluated body nodes (this was the `test28` hang); `DO` rejects zero increment and respects negative steps; expression array subscripts use `nint` consistently with writes; array auto-growth is capped at `maxarrayelems` (params) with doubling realloc, `stat=` checks, and `mask=.false.` on fresh/moved cells; `cachedfuncs` slots are recycled via freelist across `interpret` calls (entries must live for the whole session since `DO`/`IF` re-evaluate stored functions); `createarr` initialises `mask`.
- Subroutines/functions (`src/evalexec.f90` `driveroutine`/`drive_function`/`execcall`, `src/variables.f90` child namespaces + routine registry, `src/parse.f90` `parsefunction`/`parsedummies`/`parseendfunction`/`parsereturn`, `src/interpreter.f90` userfn expression parsing via `userfn_hook` set by `fscript`): scalar variable actuals alias the caller slot (pass-by-reference); array elements are copy-in/copy-out with frozen indices; other actuals are temps; function args are always copies. Routine bodies bind assignment targets by *name* (`parsevar`/`parsearr` store the name via `pushvarstr`, `evalvar`/`evalarr` resolve with `varnum`/`arraynum` in the call-time child namespace) — never by definition-time index, which would misresolve once dummies shift the namespace layout. Each call drives fresh scratch-node copies of the body (parsed once into templates), with a `progp_bias` so stored jump targets land in the scratch range and error lines map back to template lines; scratch nodes are destroyed and truncated on teardown. An `active` flag rejects (mutual) recursion; `subcalldepth` (1000) is the backstop. `progp_bias` lives in `globals` so `error%seterror` can un-bias line numbers. Gotchas fixed along the way: zero-length `func(:2)` substring (Fortran `.and.` does not short-circuit — use nested `if`s), `lenfunc` exceeding the actual buffer in `finduserfn` (clamp with `len()`), shared `components` counter clobbered by nested userfn parses (use a local).

## Git

Only commit when the user explicitly asks for a commit **after a change**. Never commit, amend, or push without explicit permission. A plan or proposed work must never include a commit step; if a commit is needed, ask for permission separately.

Every commit made by an AI assistant MUST include a trailer noting OpenCode authorship and the active model, e.g.:

```
Committed-by: OpenCode (model: <provider>/<model>)
```
