#include "macros.h"

! Multi-call embedding check: exercises fscript the way a host program
! would, with many sequential interpret() calls in one process, and
! verifies there is no cross-call state contamination. Wired into
! `make test` (see Makefile). Exits nonzero on any failure.
program embedcheck
  use fscript
  implicit none

  integer :: npass = 0, nfail = 0
  character(1024) :: err
  real(kind=8) :: x
  real(kind=8), dimension(:), allocatable :: a
  integer, dimension(:), allocatable :: lb, ub
  integer :: n

  ! T1: populate an array, read it back via the API
  call run_script((/ character(len=64) :: &
       'a(1) = 10', &
       'a(2) = 20', &
       'a(3) = 30' /), err)
  call check(trim(err) == 'OK', 'T1 interpret ok', err)
  call get_array_dims('a', n, err)
  call check(n == 1 .and. trim(err) == 'OK', 'T1 dims', err)
  allocate(lb(n), ub(n))
  call get_array_lb('a', lb, n, err)
  call get_array_ub('a', ub, n, err)
  call check(all(lb == 1) .and. all(ub == 3), 'T1 bounds')
  allocate(a(product(ub - lb + 1)))
  call get_array('a', a, size(a), err)
  call check(all(a == (/ 10.0d0, 20.0d0, 30.0d0 /)), 'T1 values')
  deallocate(a, lb, ub)

  ! T2: a fresh interpret() call starts clean; the previous array is
  ! gone (namespace reset), so the redefined array is 1..2
  call run_script((/ character(len=64) :: &
       'a(1) = 1', &
       'a(2) = 2' /), err)
  call check(trim(err) == 'OK', 'T2 interpret ok', err)
  call get_array_dims('a', n, err)
  allocate(lb(n), ub(n))
  call get_array_lb('a', lb, n, err)
  call get_array_ub('a', ub, n, err)
  call check(all(lb == 1) .and. all(ub == 2), 'T2 bounds')
  allocate(a(product(ub - lb + 1)))
  call get_array('a', a, size(a), err)
  call check(all(a == (/ 1.0d0, 2.0d0 /)), 'T2 values')
  deallocate(a, lb, ub)

  ! T3: error mid-script, then a clean script must still work
  call run_script((/ character(len=64) :: 'call nosuch(1)' /), err)
  call check(trim(err) == '1 Unknown subroutine: nosuch', 'T3 error reported', err)
  call run_script((/ character(len=64) :: 'b = 5' /), err)
  call check(trim(err) == 'OK', 'T4 clean after error', err)
  call get_scalar('b', x, err)
  call check(x == 5.0d0 .and. trim(err) == 'OK', 'T4 value after error')

  ! T5: same script twice (idempotence)
  call run_script((/ character(len=64) :: &
       'c(0,0) = 7', &
       'c(1,1) = 9' /), err)
  call run_script((/ character(len=64) :: &
       'c(0,0) = 7', &
       'c(1,1) = 9' /), err)
  call check(trim(err) == 'OK', 'T5 repeat ok', err)
  call get_array_dims('c', n, err)
  call check(n == 2, 'T5 dims')
  allocate(lb(n), ub(n))
  call get_array_lb('c', lb, n, err)
  call get_array_ub('c', ub, n, err)
  call check(all(lb == 0) .and. all(ub == 1), 'T5 bounds')
  allocate(a(product(ub - lb + 1)))
  call get_array('c', a, size(a), err)
  call check(abs(sum(a) - 16.0d0) < 1.0d-9, 'T5 values')
  deallocate(a, lb, ub)

  ! T6: subroutines defined and used across separate calls, writing
  ! through to a caller global (locals do not survive the call)
  call run_script((/ character(len=64) :: &
       'w = 0', &
       'subroutine s(v)', &
       '  w = v * 2', &
       'end subroutine s', &
       'call s(21)' /), err)
  call check(trim(err) == 'OK', 'T6 call1 ok', err)
  call get_scalar('w', x, err)
  call check(x == 42.0d0, 'T6 call1 value')
  call run_script((/ character(len=64) :: &
       'w = 0', &
       'subroutine s(v)', &
       '  w = v * 3', &
       'end subroutine s', &
       'call s(21)' /), err)
  call check(trim(err) == 'OK', 'T6 call2 ok', err)
  call get_scalar('w', x, err)
  call check(x == 63.0d0, 'T6 call2 value (registry cleared)')

  ! T7: no leakage of scalars between calls
  call run_script((/ character(len=64) :: 'z = 123' /), err)
  call run_script((/ character(len=64) :: 'y = z' /), err)
  call check(trim(err) == '1 Unknown text: z', 'T7 no scalar leakage', err)

  ! T8: error inside a called routine, then clean reuse
  call run_script((/ character(len=64) :: &
       'subroutine bad(v)', &
       '  call nosuch(v)', &
       'end subroutine bad', &
       'call bad(1)' /), err)
  call check(index(trim(err), 'Unknown subroutine: nosuch') > 0, 'T8 routine error', err)
  call run_script((/ character(len=64) :: 'q = 7' /), err)
  call check(trim(err) == 'OK', 'T8 clean after routine error', err)
  call get_scalar('q', x, err)
  call check(x == 7.0d0, 'T8 value after routine error')

  ! T9: initialisation mask distinguishes assigned from never-assigned
  ! elements (get_array returns 0.0 for both)
  call run_script((/ character(len=64) :: &
       'm(1) = 5', &
       'm(3) = 7' /), err)
  call check(trim(err) == 'OK', 'T9 interpret ok', err)
  call get_array_dims('m', n, err)
  allocate(lb(n), ub(n))
  call get_array_lb('m', lb, n, err)
  call get_array_ub('m', ub, n, err)
  allocate(a(product(ub - lb + 1)))
  call get_array('m', a, size(a), err)
  call check(all(a == (/ 5.0d0, 0.0d0, 7.0d0 /)), 'T9 values')
  deallocate(a)
  allocate(a(product(ub - lb + 1)))
  block
    logical, dimension(:), allocatable :: m
    logical, dimension(1) :: m1
    allocate(m(size(a)))
    call get_array_mask('m', m, size(m), err)
    call check(trim(err) == 'OK' .and. all(m .eqv. (/ .true., .false., .true. /)), &
         'T9 mask')
    deallocate(m)
    call get_array_mask('nosuch', m1, 1, err)
    call check(trim(err) == 'nosuch: array variable does not exist', 'T9 missing')
  end block
  deallocate(a, lb, ub)

  write(*, '(a,i0,a,i0,a)') 'EmbedCheck: ', npass, ' passed, ', nfail, ' failed'
  if (nfail > 0) stop 1

contains

  subroutine run_script(lines, err)
    character(*), dimension(:), intent(in) :: lines
    character(*), intent(out) :: err
    integer :: u, i
    open(newunit=u, status='scratch', action='readwrite')
    do i = 1, size(lines)
       write(u, '(a)') trim(lines(i))
    end do
    rewind(u)
    call interpret(u, err=err)
    close(u)
  end subroutine run_script

  subroutine check(ok, name, extra)
    logical, intent(in) :: ok
    character(*), intent(in) :: name
    character(*), intent(in), optional :: extra
    if (ok) then
       npass = npass + 1
       write(*, '(a,a)') 'pass: ', name
    else
       nfail = nfail + 1
       if (present(extra)) then
          write(*, '(a,a,a,a)') 'FAIL: ', name, ' [', trim(extra), ']'
       else
          write(*, '(a,a)') 'FAIL: ', name
       end if
    end if
  end subroutine check

end program embedcheck
