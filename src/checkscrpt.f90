#include "macros.h"

module checkscrpt
  use fscript
  implicit none

  integer, save :: count
  integer, parameter :: fdp=8

contains

  subroutine checkscript(script, err)
    character(*), intent(in) :: script
    character(*), intent(in), optional :: err
    character(1024) :: tmperr, line1, line2
    logical :: pass
    integer :: linen, istat

    count = count + 1
    write(*, '(i4.4)', advance='no') count

    open(unit=11, file="tests/"//TRIM(script)//".in", action="read")
    open(unit=12, file="tests/"//TRIM(script)//".out", action="write")
    call interpret(11, 12, tmperr)
    close(12)
    close(11)

    if (present(err)) then
       if (TRIM(err) == TRIM(tmperr)) then
          write(*,'(2x,a)')     'Testing(Error)           : PASSED'
       else
          write(*, '(2x,a)')    'Testing(Error)           : FAILED'
          write(*,'(6x,a,a)')   'Script:            tests/'//TRIM(script)//".in"
          write(*,'(6x,a)')     'Execution error:   '//TRIM(tmperr)
          write(*,'(6x,a)')     'Expecting:         '//TRIM(err)
       end if
    else
       open(unit=12, file="tests/"//TRIM(script)//".out", action="read")
       open(unit=13, file="tests/"//TRIM(script)//".exp", action="read")
       pass = .true.
       linen = 0
       istat = 0
       do while (pass .and. istat == 0)
          linen = linen + 1
          line1 = ""
          line2 = ""
          read(12, '(a)', iostat=istat) line1
          read(13, '(a)', iostat=istat) line2
          if (TRIM(line1) /= TRIM(line2)) pass = .false.
       end do
       close(13)
       close(12)
       
       if (pass) then
          write(*,'(2x,a)')      'Testing(Output)          : PASSED'
       else
          write(*, '(2x,a)')     'Testing(Output)          : FAILED'
          write(*,'(6x,a)')      'Script:           tests/'//TRIM(script)//'.in'
          write(*,'(6x,a,i4.4)') 'Line number:      ', linen
          write(*,'(6x,a)')      'Output line:      '//TRIM(line1)
          write(*,'(6x,a)')      'Expecting line:   '//TRIM(line2)
       end if
    end if
  end subroutine checkscript



  subroutine checkscalar(script, var, val)
    character(*), intent(in) :: script, var
    real(fdp), intent(in) :: val
    character(1024) :: tmperr
    real(fdp) :: tmp

    count = count + 1
    write(*, '(i4.4)', advance='no') count

    open(unit=11, file="tests/"//TRIM(script)//".in", action="read")
    open(unit=12, file="tests/"//TRIM(script)//".out", action="write")
    call interpret(11, 12, tmperr)
    call get_scalar(var, tmp, tmperr)
    close(12)
    close(11)

    if (tmp == val) then
       write(*,'(2x,a)')      'Testing(GetScalar)       : PASSED'
    else
       write(*, '(2x,a)')     'Testing(GetScalar)       : FAILED'
       write(*, '(6x,a)')     'Script:           tests/'//TRIM(script)//'.in'
       write(*, '(6x,a,f10.3)')'Expecting:              ', val
       write(*, '(6x,a,f10.3)')'Got:                    ', tmp
    end if
  end subroutine checkscalar



  subroutine checkscalars(script, n, vars)
    character(*), intent(in) :: script, vars
    integer, intent(in) :: n
    character(1024) :: tmperr
    integer :: tmp1, i, ltmp2, ltmp3
    character(32), dimension(:), allocatable :: v
    character(3*len(vars)) :: tmp2, tmp3

    count = count + 1
    write(*, '(i4.4)', advance='no') count

    open(unit=11, file="tests/"//TRIM(script)//".in", action="read")
    open(unit=12, file="tests/"//TRIM(script)//".out", action="write")
    call interpret(11, 12, tmperr)
    call number_scalars(tmp1)
    allocate(v(tmp1))
    call scalars(v, tmp1)
    close(12)
    close(11)

    tmp2 = ""
    do i = 1, tmp1
       tmp2 = trim(adjustl(tmp2))//" "//trim(adjustl(v(i)))
    end do

    tmp3 = trim(adjustl(vars))
    ltmp2 = len_trim(tmp2)
    ltmp3 = len_trim(tmp3)
 
    if (n == tmp1 .and. tmp3(:ltmp3) == tmp2(:ltmp2)) then
       write(*,'(2x,a)')      'Testing(NumberScalars)   : PASSED'
    else
       write(*, '(2x,a)')     'Testing(NumberScalars)   : FAILED'
       write(*, '(6x,a)')     'Script:           tests/'//TRIM(script)//'.in'
       write(*, '(6x,a,i3,2a)')   'Expecting:              ', n, ' ', trim(adjustl(vars))
       write(*, '(6x,a,i3,2a)')   'Got:                    ', tmp1, ' ', trim(adjustl(tmp2))
    end if
  end subroutine checkscalars



  subroutine checkarrays(script, n, vars)
    character(*), intent(in) :: script, vars
    integer, intent(in) :: n
    character(1024) :: tmperr
    integer :: tmp1, i
    character(32), dimension(:), allocatable :: v
    character(len(vars)+2) :: tmp2

    count = count + 1
    write(*, '(i4.4)', advance='no') count

    open(unit=11, file="tests/"//TRIM(script)//".in", action="read")
    open(unit=12, file="tests/"//TRIM(script)//".out", action="write")
    call interpret(11, 12, tmperr)
    call number_arrays(tmp1)
    allocate(v(tmp1))
    call arrays(v, tmp1)
    close(12)
    close(11)

    tmp2 = ""
    do i = 1, tmp1
       tmp2 = trim(adjustl(tmp2))//" "//trim(adjustl(v(i)))
    end do

    if (n == tmp1 .and. trim(adjustl(vars)) == trim(adjustl(tmp2))) then
       write(*,'(2x,a)')      'Testing(NumberArrays)    : PASSED'
    else
       write(*, '(2x,a)')     'Testing(NumberArrays)    : FAILED'
       write(*, '(6x,a)')     'Script:           tests/'//TRIM(script)//'.in'
       write(*, '(6x,a,i3,2a)')   'Expecting:              ', n, ' ', trim(adjustl(vars))
       write(*, '(6x,a,i3,2a)')   'Got:                    ', tmp1, ' ', trim(adjustl(tmp2))
    end if
  end subroutine checkarrays



  subroutine checkarray(script, var, vals)
    character(*), intent(in) :: script, var
    real(fdp), dimension(6), intent(in) :: vals
    character(1024) :: tmperr
    real(fdp), dimension(6) :: tmp
    real(fdp), dimension(:), allocatable :: a
    integer, dimension(:), allocatable :: lb, ub
    integer :: n

    count = count + 1
    write(*, '(i4.4)', advance='no') count

    open(unit=11, file="tests/"//TRIM(script)//".in", action="read")
    open(unit=12, file="tests/"//TRIM(script)//".out", action="write")
    call interpret(11, 12, tmperr)
    call get_array_dims(var, n)
    allocate(lb(n), ub(n))
    call get_array_lb(var, lb, n)
    call get_array_ub(var, ub, n)
    allocate(a(product(ub-lb+1)))
    call get_array(var, a, size(a))
    close(12)
    close(11)
    
    tmp(1) = a(1)
    tmp(2) = a(size(a))
    tmp(3) = minval(a)
    tmp(4) = maxval(a)
    tmp(5) = sum(a)
    tmp(6) = product(a)
    

    if (all(abs(tmp-vals) < 1.0d-6)) then
       write(*,'(2x,a)')        'Testing(GetArray)        : PASSED'
    else
       write(*, '(2x,a)')       'Testing(GetArray)        : FAILED'
       write(*, '(6x,a)')       'Script:           tests/'//TRIM(script)//'.in'
       write(*, '(6x,a,6f10.3)') 'Expecting:        ', vals
       write(*, '(6x,a,6f10.3)') 'Got:              ', tmp
    end if

    deallocate(lb, ub, a)
  end subroutine checkarray

end module checkscrpt


