#include "macros.h"

module callstack
  use error

  implicit none

  private :: printcalls, cstack

  integer, dimension(:), pointer :: cstack
  integer :: calldepth = 1

contains

  subroutine resetcallstack()
    cstack = 0
    calldepth = 1
  end subroutine resetcallstack



  subroutine initcallstack()
    allocate(cstack(allocsize))
    call resetcallstack()
  end subroutine initcallstack



  subroutine printcalls()
    integer :: i

    do i=1, calldepth
       if (cstack(i) > commandstart .and. cstack(i) < commandend) write(*,*) TRIM(commands(cstack(i)))
    end do
  end subroutine printcalls



  function searchcall(a)
    integer, intent(in) :: a
    integer :: searchcall
    integer :: i

    searchcall = 0

    do i = calldepth, 1, -1
       if (cstack(i) == a) then
          searchcall = i
          exit
       end if
    end do
  end function searchcall



  subroutine setcalldepth(a)
    integer, intent(in) :: a

    calldepth = a
  end subroutine setcalldepth
    

  subroutine pushcall(i)
    integer, intent(in) :: i
    integer :: n
    integer, dimension(:), pointer :: tmpcstack

    n = size(cstack)
    calldepth = calldepth+1

    if (calldepth > n) then
       allocate(tmpcstack(calldepth+allocsize))
       tmpcstack(:n) = cstack
       tmpcstack(n+1:) = 0
       deallocate(cstack)
       cstack => tmpcstack
    end if

    cstack(calldepth) = i
  end subroutine pushcall



  subroutine popcall(i)
    integer, intent(out) :: i

    i = 0

    if (calldepth > 1) then
       i = cstack(calldepth)
       calldepth = calldepth - 1
    else
       call seterror('Unmatched end of section')
    end if
  end subroutine popcall



  subroutine getcall(i)
    integer, intent(out) :: i

    i = cstack(calldepth)
  end subroutine getcall

end module callstack
