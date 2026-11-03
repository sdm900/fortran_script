# AGENTS.md

Legacy Fortran scripting-language interpreter. No CI, no linter/formatter, no package manager. Two binaries, one test runner.

## Build (gfortran, verified)

- The `Makefile` is g95-only and stale: `F90COMPILER = ~/bin/g95` (not installed) plus g95-only flags (`-fpointer=invalid -freal=nan -ftrace=full`). Do not run `make` as-is.
- Verified working build with system gfortran (15.x): compile in `src/LIST` order with `-cpp -I./src -J./obj`:
  ```
  mkdir -p obj bin
  for f in $(cat src/LIST); do b=$(basename $f .f90); gfortran -cpp -I./src -J./obj -c $f -o obj/$b.o; done
  gfortran -cpp -I./src -J./obj -c src/check.f90 -o obj/check.o
  gfortran -cpp -I./src -J./obj -c src/shell.f90 -o obj/shell.o
  OBJS="obj/params.o obj/globals.o obj/error.o obj/programnodes.o obj/string.o obj/variables.o obj/extrafunc.o obj/interpreter.o obj/progstack.o obj/callstack.o obj/evalexec.o obj/parse.o obj/fscript.o"
  gfortran $OBJS obj/checkscrpt.o obj/check.o -o bin/check
  gfortran $OBJS obj/shell.o -o bin/shell
  ```
- `mkdir -p obj bin cpp` is required first; the `Makefile` never creates these dirs.
- `check.o` and `shell.o` each define `main` — never link `obj/*.o` together; use the two link lines above (`bin/check` needs `checkscrpt.o`, `bin/shell` must exclude it).
- `src/*.f90` use `#include "macros.h"`; the `Makefile` handles this via external `cpp -P -C -traditional-cpp` staged into `cpp/`. With gfortran, `-cpp -I./src` replaces that pipeline.

## Generated source / deps

- `src/params.f90` is generated: `m4 src/params_m4.f90 > src/params.f90`. Edit `params_m4.f90`, not `params.f90`. Note `m4` is not installed in this container.
- `./build_deps` regenerates `src/.*.dep` make dependencies from `src/LIST` plus `check.f90`/`shell.f90`; run from repo root.

## Tests

- Runner is `bin/check` (sources `src/check.f90` + `src/checkscrpt.f90`); `bin/shell` (`src/shell.f90`) is the interactive interpreter, not a test.
- Must run from repo root: `checkscrpt.f90` hardcodes `tests/<name>.in` / `.out` / `.exp` relative paths.
- `bin/check` overwrites `tests/*.out` on every run, then diffs against `tests/*.exp`. Restore after running: `git checkout -- tests/`.
- Not every test has an `.exp` file. Inline expectations live in `src/check.f90`: error-string checks (test1-2nd call, test6, test17, test28-33), scalar/array API checks (test23/24 use `get_scalar`/`get_array`), count checks (test25/26, no `.in` files). Tests without `.exp` (17, 23, 24, 28, 29, 30, 31, 32, 33) are expected.
- Subroutines are unimplemented: `subroutine`/`call`/`endsubroutine` eval+exec stubs report `Subroutines not implemented` (tests 28/29 assert this). The suite runs to completion (33 checks); a hang means a new regression, not a known issue.
- Run wrapped: `timeout -s KILL 60 ./bin/check || true` (bare `timeout` + `set -e` aborts the shell on 124).

## Architecture

- Entry: `fscript%interpret` (`src/fscript.f90`, also exposes the embed API: `get_scalar`, `get_array*`, `number_scalars/arrays`) → `parse%parsedriver` (`src/parse.f90`).
- Module compile order = `src/LIST` (params → globals → error → programnodes → string → variables → extrafunc → interpreter → progstack → callstack → evalexec → parse → fscript → checkscrpt).
- `src/interpreter.f90` (~1070 lines) and `src/extrafunc.f90` (~2580 lines, intrinsic function table) are the bulk; start there, not leaf files.
- The expression parser/evaluator in `src/interpreter.f90` (`s_readnumber`, `s_parse*`, `s_evaluate`, Bessel/`gamma` domain guards) shares lineage with `../fortran_parser/interpreter.f90`. Parser fixes already ported here: IOSTAT number parsing (`'Not a number'`), `parsedepth` cap (1000, `'Maximum nesting depth exceeded'`), `**` huge-exponent guard, `besjn/besy0/besy1/besyn/besin/besk0/besk1/beskn` domain guards, `gamma` pole/magnitude guards. Not portable (script lacks these features): `//`/`%`/`floor`/`ceiling`/`mod` guards, varvals arity tracking, `s_variableerr` MERGE fix, error-propagation stability check. Script-specific adaptations: sticky `seterror` (error module, line-number prefix) instead of `s_seterror`, `endfunc_` instead of `end_` sentinel.
- Follow-up hardening (no parser equivalent): `parsedriver` block-depth cap (1000) plus `parseline` restoring `progp` past multi-line constructs so later lines never reuse stale unevaluated body nodes (this was the `test28` hang); `DO` rejects zero increment and respects negative steps; expression array subscripts use `nint` consistently with writes; array auto-growth is capped at `maxarrayelems` (params) with doubling realloc, `stat=` checks, and `mask=.false.` on fresh/moved cells; `cachedfuncs` slots are recycled via freelist across `interpret` calls (entries must live for the whole session since `DO`/`IF` re-evaluate stored functions); `createarr` initialises `mask`.

## Git

Only commit when the user explicitly asks for a commit **after a change**. Never commit, amend, or push without explicit permission. A plan or proposed work must never include a commit step; if a commit is needed, ask for permission separately.

Every commit made by an AI assistant MUST include a trailer noting OpenCode authorship and the active model, e.g.:

```
Committed-by: OpenCode (model: <provider>/<model>)
```
