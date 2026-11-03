# fortran_script — User Guide

A small Fortran-like scripting language, written in Fortran.
It evaluates double-precision expressions, stores variables and arrays,
runs `do` loops and `if` branches, and supports user-defined subroutines
and functions. Think "programmable calculator", not a general-purpose
language: there is no file I/O.

## Running scripts

Build with `make` (needs `gfortran`), then:

```sh
./bin/shell < script.in     # run a script file
./bin/shell                 # interactive prompt
make test                   # run the regression suite
```

- In a script, one statement per line. Blank lines are fine.
- `#` starts a comment (rest of the line is ignored).
- Numbers print in an `f12.5` field, e.g. `    3.14000`.
- On the first error the script stops. The message is prefixed with the
  offending line number, e.g. `3 Array value used before it is initialised`.
  In interactive mode the error state resets after each line.

## Case rules

- Commands must be lowercase: `print`, `if`, `do`, …
- Function names accept any case: `sin(x)`, `SIN(x)`, `BesJ0(x)` all work.
- Variable and array names are **case-sensitive**: `A` and `a` are different.

## Variables

Assign with `=`. A variable springs into existence on first assignment;
there are no declarations or types — everything is double precision.

```
x = 2.0d0
y = 3
z = x**y + 1      # 9.0
print z
```

- Number formats: `3`, `3.14`, `.5`, `1e3`, `1.5e-2`, `3.14159d0`.
- Names may contain letters and underscores only (no digits),
  up to 32 characters.
- Reassigning is fine: `x = 1` … `x = x + 1`.
- Using a name that was never assigned is an error (`Unknown text: …`).

## Operators

| Operator | Meaning |
|---|---|
| `**` or `^` | power, highest precedence |
| `*` `/` | multiply, divide |
| `+` `-` | add, subtract |
| `==` `!=` `>` `<` `>=` `=>` `<=` `=<` | comparisons, yield `1` (true) or `0` (false) |

- All arithmetic is floating point: `10/4` is `2.5`. There is no integer
  division, remainder, or string-concatenation operator.
- Unary `+`/`-` is allowed at the start of an expression, just after `(`,
  and just after `,` in argument lists: `print -5+2`, `print max(-1,2)`.
  It is *not* allowed directly after another operator: `2*-3` is an error,
  write `2*(-3)` instead.
- Division by zero, `0**0`, and a negative base with a non-integer
  exponent (`(-2)**0.5`) are errors. A huge exponent on a negative base
  (`(-2)**3000000000`) is rejected as too large.

## Printing

`print` takes a comma-separated list of quoted strings and expressions.
Both single and double quotes work.

```
print "x =", x, 'done'
```

## Control flow

### IF blocks

```
if (x > 1) then
  print "big"
else
  print "small"
endif
```

- The condition is in parentheses; any positive value counts as true,
  zero and negative values count as false.
- `else` is optional. `endif` (or `end if`) closes the block.
- One-line form: `if (a(i) == 6) print "got it", i`
  (a false condition just skips the line — no `else` on one-liners).

### DO loops

```
do i = 1, 10
  print i
enddo

do i = 10, 0, -2
  print i
enddo
```

- `do var = start, end` steps by 1; `do var = start, end, step`
  uses the given step. Negative steps count down; a zero step is an error.
- The loop variable is an ordinary variable: it keeps its last value
  after the loop.
- `exit` leaves the loop; `cycle` skips to the next iteration.
  `exit`/`cycle` outside a loop is an error.
- A bare `do` … `enddo` with no bounds loops forever until `exit` or `stop`.
- `enddo` may be written `end do`, and `endif` as `end if`.
  (A bare `end` alone is an error — always name the block type.)
  Loops nest.

### Stopping

- `stop` ends the whole script (and the shell).
- `help` prints the list of commands.
- `enddo`/`endif` (or `end do` / `end if`) close the matching block;
  `end subroutine` / `end function` close routine definitions.
  (`elseif` is not implemented as a standalone command.)
- `return` exits the current subroutine or function early.
  It is an error outside a routine.

## Subroutines

```
subroutine swap(a, b)
  c = a
  a = b
  b = c
end subroutine swap

x = 1
y = 2
call swap(x, y)     # x = 2, y = 1
```

- Invoke with `call name(arguments)` — parentheses are required,
  even when empty (`call init()`).
- Dummy arguments that match caller *variables* are passed
  by reference: assigning to them inside the routine modifies the
  caller's variable (as `swap` does above).
- Anything else (literals, expressions) is evaluated first and passed
  as a temporary, so assignments to it are lost. Array *elements* are
  copied in and written back, so `call bump(a(2))` updates `a(2)`.
  Whole arrays cannot be passed — pass elements instead.
- Dummy names shadow caller variables of the same name. Other caller
  variables and arrays are visible and shared — more precisely, the
  routine sees the caller's *global* variables and arrays; names created
  (or shadowed) inside an enclosing routine are *not* visible to routines
  it calls. Names created inside the routine itself are local and
  disappear on return.
- `return` exits the routine early. `exit`/`cycle` may not cross
  a routine boundary.
- Recursion (direct or mutual) is rejected with an error.
- Names follow the variable rules (letters and underscores, no digits,
  32 characters max). Redefining a routine, or giving two dummies the
  same name, is an error.

## User-defined functions

```
function sq(x)
  sq = x*x
end function

print sq(3)          # 9
print sq(2) + sq(4)  # 20
```

- A function returns its value by assigning to its own name inside
  the body. Leaving without assigning is an error
  (`Function did not assign a return value`).
- Functions are called inside expressions, with any nesting:
  `sq(sq(2))`, `sq(x) + 1`, even `f(g(x))`.
- Unlike subroutines, functions cannot modify their caller's variables:
  arguments are always passed by value (a local copy).
- A no-argument function may be called with or without parentheses:
  `ten` or `ten()`.
- A function name must not collide with an intrinsic (`sin`, `max`, …;
  the check is case-insensitive), with an existing variable or array, or
  with one of its own dummy arguments — all are definition-time errors.
  `call` on a function is an error, and a subroutine name used inside
  an expression is rejected (`Unknown text`). Recursion is rejected,
  as for subroutines.

## Arrays

Assign to an element and the array is created automatically with that
shape; assigning outside the current bounds grows it (lower bounds may be
zero or negative). Indices are rounded to the nearest integer, so `a(1.5)`
and `a(1.5) = 7` both mean `a(2)`.

```
do i = 1, 5
  a(i) = -1 + i
enddo
print a(3)

a(0,-1) = 1          # 2-D, arbitrary lower bounds
print a(0,-1)
```

- Up to 10 dimensions per array; up to 10,000,000 elements total
  (larger requests fail with `Array exceeds maximum size`).
- Reading an element that was never assigned is an error
  (`Array value used before it is initialised`); reading outside the
  bounds is an error too.
- Dimension mismatches (e.g. using a 1-D array with two subscripts) are errors.

## Intrinsic functions

One argument unless noted. All angles are in radians.

| Group | Functions |
|---|---|
| Roots, powers | `sqrt`, `cbrt`, `exp`, `log`, `log10`, `logn(a,b)`¹ |
| Trigonometry | `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sinh`, `cosh`, `tanh`, `sinc`², `csch`, `sech`, `coth` |
| Rounding/parts | `aint` (truncate), `anint` (nearest whole), `abs` |
| Min/max/merge | `max(a,b)`, `min(a,b)`, `merge(x,y,z)`³ |
| Steps | `step(x)`⁴, `delta(x)`⁵, `hat(x)`⁶, `gauss(x)`⁷ |
| Error & gamma | `erf`, `erfc`, `ierf`, `ierfc`, `gamma`, `lgamma`, `expi`, `sini`, `cosi`, `logi`, `fresc`, `fress` |
| Bessel J/Y | `besj0`, `besj1`, `besjn(n,x)`, `besy0`, `besy1`, `besyn(n,x)` |
| Bessel I/K | `besi0`, `besi1`, `besin(n,x)`, `besk0`, `besk1`, `beskn(n,x)` |
| Elliptic | `elle`, `ellk`, `ielle(x,phi)`, `iellf(x,phi)` |

¹ `logn(a, b)` is log of `b` to base `a` (`logn(10,100)` = 2).
² `sinc(x)` = `sin(x)/x`, with `sinc(0)` = 1.
³ `merge(x, y, z)` returns `z` if `x == 0`, else `y`.
⁴ `step(x)` = 0 for `x < 0`, else 1. ⁵ `delta(x)` = 1 for `x == 0`, else 0.
⁶ `hat(x)` = 1 for `|x| <= 0.5`, else 0. ⁷ `gauss(x)` = `exp(-x**2)`.

Domain rules are enforced and reported, e.g. `log`/`log10`/`logi` need
positive arguments, `sqrt` needs a non-negative one, `acos`/`asin` need
`|x| <= 1`, `besy0`/`besy1`/`besk0`/`besk1` need `x > 0`, the `n` in
`besjn`/`besyn`/`besin`/`beskn` must be an integer with `|n| <= 1000000`,
`gamma` rejects zero/negative integers and `|x| > 200`, and
`ierf`/`elle`/`ellk` need `|x| < 1`.

## Restrictions and limits

- Lines are cut at 1024 characters; names at 32 characters.
- Expression nesting is capped (1000 deep) and block nesting too.
- Call depth is capped (1000 deep); recursion is not supported.
- Whole arrays cannot be passed to subroutines — pass elements instead.
- **Not implemented:** `open` / `close` (no file I/O), standalone `elseif`.
- No integer type: every value is double precision, comparisons included.
- Output formatting is fixed (`f12.5` per number); there is no formatted I/O.

## Complete example

```
# sum of squares with a loop and an array
n = 10
do i = 1, n
  sq(i) = i**2
enddo

total = 0
do i = 1, n
  total = total + sq(i)
enddo

print "sum of squares 1..", n, " = ", total
if (total == 385) print "correct"
```

## Embedding (for developers)

The interpreter is also a library. `src/fscript.f90` exposes
`interpret(infile, outfile, err)` plus inspection routines
(`number_scalars`, `scalars`, `get_scalar`, `number_arrays`, `arrays`,
`get_array_dims`, `get_array_lb/ub`, `get_array`) so a host Fortran
program can run a script and read its variables back. See
`src/checkscrpt.f90` for a worked example and `src/check.f90` for the
regression suite (run it with `make test` from the repo root).
