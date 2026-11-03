#include "macros.h"

module fscript
  use parse
  use evalexec

  implicit none

  private
  public :: interpret, number_scalars, scalars, get_scalar,&
       & number_arrays, arrays, get_array_dims, get_array_lb,&
       & get_array_ub, get_array, get_array_mask

contains

  subroutine initfscript()
    logical, save :: firstrun = .true.

    if (firstrun) then
       call initnamespaces()
       call initsubs()
       call initvarstack()
       call initprogstack()
       call initcallstack()
       firstrun = .false.
    end if
    
    ! Unconditional: a previous interpret() must never poison the next
    ! one, even if an error unwind left calldepth/blockdepth/subcalldepth
    ! away from their rest values (all push/pop paths are balanced, but
    ! the next call must not depend on that).
    call resetvarstack()
    call clearsubs()
    call resetprogstack()
    call resetcallstack()
    call reseterror()
    call resetsubdepth()
    call resetblockdepth()
    progp = 0
    gotstop = .false.
    gotreturn = .false.
    stdin = -1
    stdout = -1

    call set_userfn_hook(drive_function)
  end subroutine initfscript



  !
  !  Top level driver routine
  !
  subroutine interpret(infile, outfile, err)
    integer, intent(in), optional :: infile, outfile
    character(*), intent(inout), optional :: err

    call initfscript()

    if (present(infile)) stdin = infile
    if (present(outfile)) stdout = outfile

    call parsedriver()

    if (present(err)) call geterror(err)
  end subroutine interpret



  !
  !  Call in routine to get arrays and scalars
  !
  subroutine number_scalars(n)
    integer, intent(out) :: n
    
    n = numvars()
  end subroutine number_scalars



  subroutine scalars(s, n)
    integer, intent(in) :: n
    character(*), dimension(n), intent(out) :: s
    integer :: i
    
    s = ""

    do i = 1, min(n, numvars())
       s(i) = TRIM(var(i))
    end do
  end subroutine scalars



  subroutine get_scalar(s, x, err)
    character(*), intent(in) :: s
    character(*), optional, intent(out) :: err
    real(fdp), intent(out) :: x
    character(len(s)) :: tmp
    integer :: i, l

    tmp = TRIM(s)
    l = len_trim(tmp)
    i = varstrnum(tmp, l)

    if (i == 0) then
       x = 0.0d0
       if (present(err)) err = trim(tmp)//': scalar variable does not exist'
    else
       x = varval(i)
       if (present(err)) err = 'OK'
    end if
  end subroutine get_scalar



  subroutine number_arrays(n)
    integer, intent(out) :: n

    n = numarrays()
  end subroutine number_arrays



  subroutine arrays(s, n)
    integer, intent(in) :: n
    character(*), dimension(n), intent(out) :: s
    integer :: i

    do i = 1, min(n, numarrays())
       s(i) = TRIM(arr(i))
    end do
  end subroutine arrays
    


  subroutine get_array_dims(s, n, err)
    character(*), intent(in) :: s
    character(*), optional, intent(out) :: err
    integer, intent(out) :: n
    character(len(s)) :: tmp
    integer :: i, l

    tmp = TRIM(s)
    l = len_trim(tmp)
    i = arrstrnum(tmp, l)

    if (i == 0) then
       n = 0
       if (present(err)) err = trim(tmp)//': array variable does not exist'
    else
       n = arraydims(i)
       if (present(err)) err = 'OK'
    end if
  end subroutine get_array_dims



  subroutine get_array_lb(s, lb, n, err)
    character(*), intent(in) :: s
    character(*), optional, intent(out) :: err
    integer, intent(in) :: n
    integer, dimension(n), intent(out) :: lb
    character(len(s)) :: tmp
    integer :: i, l

    tmp = TRIM(s)
    l = len_trim(tmp)
    i = arrstrnum(tmp, l)

    if (i == 0) then
       lb = 0
       if (present(err)) err = trim(tmp)//': array variable does not exist'
    elseif (n == arraydims(i)) then
       call getarraylb(i, lb, n)
       if (present(err)) err = 'OK'
    else
       lb = 0
       if (present(err)) err = trim(tmp)//': number of array dimensions mismatch'
    end if
  end subroutine get_array_lb



  subroutine get_array_ub(s, ub, n, err)
    character(*), intent(in) :: s
    character(*), optional, intent(out) :: err
    integer, intent(in) :: n
    integer, dimension(n), intent(out) :: ub
    character(len(s)) :: tmp
    integer :: i, l

    tmp = TRIM(s)
    l = len_trim(tmp)
    i = arrstrnum(tmp, l)

    if (i == 0) then
       ub = 0
       if (present(err)) err = trim(tmp)//': array variable does not exist'
    elseif (n == arraydims(i)) then
       call getarrayub(i, ub, n)
       if (present(err)) err = 'OK'
    else
       ub = 0
       if (present(err)) err = trim(tmp)//': number of array dimensions mismatch'
    end if
  end subroutine get_array_ub



  subroutine get_array(s, a, n, err)
    character(*), intent(in) :: s
    character(*), optional, intent(out) :: err
    integer, intent(in) :: n
    real(fdp), dimension(n), intent(out) :: a
    character(len(s)) :: tmp
    integer :: i, l

    tmp = TRIM(s)
    l = len_trim(tmp)
    i = arrstrnum(tmp, l)

    if (i == 0) then
       a = 0.0d0
       if (present(err)) err = trim(tmp)//': array variable does not exist'
    elseif (n == arraysize(i)) then
       call getarray(i, a, n)
       if (present(err)) err = 'OK'
    elseif (present(err)) then
       err = trim(tmp)//': array size different to requested array size'
    end if
  end subroutine get_array



  ! Initialisation mask: mask(k) is .false. where the element was never
  ! assigned (get_array returns 0.0 there, indistinguishable otherwise).
  subroutine get_array_mask(s, mask, n, err)
    character(*), intent(in) :: s
    logical, dimension(n), intent(out) :: mask
    integer, intent(in) :: n
    character(*), optional, intent(out) :: err
    character(len(s)) :: tmp
    integer :: i, l

    tmp = TRIM(s)
    l = len_trim(tmp)
    i = arrstrnum(tmp, l)

    if (i == 0) then
       mask = .false.
       if (present(err)) err = trim(tmp)//': array variable does not exist'
    elseif (n == arraysize(i)) then
       call getarraymask(i, mask, n)
       if (present(err)) err = 'OK'
    elseif (present(err)) then
       err = trim(tmp)//': array size different to requested array size'
    end if
  end subroutine get_array_mask
end module fscript
