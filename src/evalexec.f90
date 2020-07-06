#include "macros.h"

module evalexec
  use callstack
  use progstack

  implicit none

  private :: evalexecdriver, evalcmd, evalprint, evalvar, evalarr,&
       & evaldo, evalif, evalinlineif, execline, execcmd, exechelp,&
       & execprint, execvar, execarr, execdo, execif, execinlineif,&
       & execenddo, execthen, execelse, execendif, execexit, execcycle

contains
  !
  !  Driver routine
  !
  recursive subroutine evalexecdriver()
    logical :: cont
    integer :: i

    cont = .true.

    do while(cont .and. (.not. gotstop) .and. errorok)
       call getcall(i)

       if (i == cycle_ .or. i == exit_) then
          cont = .false.
          exit
       end if

       progp = progp + 1
       
       call resetstkp()

       if (evaluated()) then
          call execline(cont)
       else
          call setevaluated()
          call evalline(cont)
       end if

       if ((.not. errorok) .and. calldepth == 1 .and. stdin == -1) then
          call reseterror()
       end if
    end do
  end subroutine evalexecdriver



  !
  !  The evaluation engine
  !
  recursive subroutine evalline(cont)
    logical, intent(inout) :: cont
    integer :: cmd

    if (.not. errorok) return

    call popcmd(cmd)
    call evalcmd(cmd, cont)
    call resetstkp()
    call execline(cont)
  end subroutine evalline



  recursive subroutine evalcmd(cmd, cont)
    integer, intent(in) :: cmd
    logical, intent(in) :: cont

    select case(cmd)
    case(do_)
       call evaldo()

    case(inlineif_)
       call evalinlineif()

    case(if_)
       call evalif()
       
    case(print_)
       call evalprint()
       
    case(var_)
       call evalvar()
       
    case(arr_)
       call evalarr()

    case(subroutine_)
       call evalsubroutine()

    case(call_)
       call evalcall()

    case(enddo_, then_, else_, endif_, exit_, cycle_, stop_, help_, nop_, endsubroutine_)

    case default
       call seterror('Illegal command: '//TRIM(commands(cmd)))

    end select
  end subroutine evalcmd



  !
  !  Routine to evaluate a parsed line
  !
  subroutine evalprint()
    type(str) :: s
    character :: natm
    integer :: func
    
    call getnatm(natm)
    
    do while (natm /= blank_)
       select case(natm)
       case(string_)
          call popstr(s)
       case(functionstr_)
          call remfnstr(s)
          call createfn(s, func)
          call insfn(func)
       end select

       call getnatm(natm)
   end do
  end subroutine evalprint


  
  subroutine evalvar()
    type(str) :: s
    integer :: var, func

    call remint(var)
    call remstr(s)
    call createfn(s, func)

    if (var == udef_) then
       call remstr(s)
       call createvar(s)
       var = varnum(s)
    end if

    call pushint(var)
    call pushfn(func)
  end subroutine evalvar



  subroutine evalarr()
    type(str) :: s
    integer :: arr, ndims, i, func
    integer, dimension(:), allocatable :: funcs

    call remint(arr)
    call remint(ndims)
    allocate(funcs(ndims))

    do i = 1, ndims
       call remstr(s)
       call createfn(s, funcs(i))
    end do

    call remstr(s)
    call createfn(s, func)

    if (arr == udef_) then
       call remstr(s)
       call createarr(s, ndims)
       arr = arraynum(s)
    end if

    call pushint(arr)

    do i = 1, ndims
       call pushfn(funcs(i))
    end do

    call pushfn(func)
    deallocate(funcs)
  end subroutine evalarr



  recursive subroutine evaldo()
    character :: c
    integer :: dovar, dostart, doend, doinc, endprogp, cll
    logical :: gotdoinc
    type(str) :: s
    
    call getnatm(c)
    
    if (c /= progp_) then
       call remstr(s)
       if (varnum(s) == 0) call createvar(s)
       dovar = varnum(s)

       call remstr(s)
       call createfn(s, dostart)
       call remstr(s)
       call createfn(s, doend)
       call getnatm(c)
       gotdoinc = .false.

       if (c /= progp_) then
          call remstr(s)
          call createfn(s, doinc)
          gotdoinc = .true.
       end if

       call remprogp(endprogp, cll)
       call pushvar(dovar)
       call pushfn(dostart)
       call pushfn(doend)

       if (gotdoinc) call pushfn(doinc)

       call pushprogp(endprogp, cll)
    end if
  end subroutine evaldo



  recursive subroutine evalif()
    type(str) :: s
    integer :: func, progp1, cll1

    call remstr(s)
    call createfn(s, func)
    call remprogp(progp1, cll1)
    call pushfn(func)
    call pushprogp(progp1, cll1)
  end subroutine evalif



  subroutine evalinlineif()
    type(str) :: s
    integer :: func, cmd
    logical :: cont

    call remstr(s)
    call createfn(s, func)
    call insfn(func)
    call popcmd(cmd)
    cont = .true.
    call evalcmd(cmd, cont)
  end subroutine evalinlineif



  subroutine evalsubroutine()
  end subroutine evalsubroutine



  subroutine evalcall()
  end subroutine evalcall

 

  !
  !  The execution engine
  !
  recursive subroutine execline(cont)
    logical, intent(inout) :: cont
    integer :: command

    if (.not. errorok) return

    call popcmd(command)
    call execcmd(command, cont)
  end subroutine execline



  recursive subroutine execcmd(command, cont)
    integer, intent(in) :: command
    logical, intent(inout) :: cont

    
    select case(command)
    case(do_)
       call execdo()
       
    case(enddo_)
       call execenddo(cont)

    case(inlineif_)
       call execinlineif()

    case(if_)
       call execif()
       
    case(then_)
       call execthen()
       
    case(else_)
       call execelse(cont)
       
    case(endif_)
       call execendif(cont)
       
    case(print_)
       call execprint()
       
    case(exit_)
       call execexit(cont)
       
    case(cycle_)
       call execcycle(cont)
       
    case(stop_)
       gotstop = .true.
       
    case(help_)
       call exechelp()
       
    case(var_)
       call execvar()

    case(arr_)
       call execarr()

    case(subroutine_)
       call execsubroutine()

    case(endsubroutine_)
       call execendsubroutine()

    case(call_)
       call execcall()

    case(nop_)

    case default
       call seterror('Illegal command: '//TRIM(commands(command)))
       
    end select
  end subroutine execcmd


  !
  !  Routine to execute an evaluated line
  !
  subroutine exechelp()
    write(*,*) "Allowed intrinsic procedures are:"
    write(*,*) "do/enddo/cycle/exit      Do loops"
    write(*,*) "if/then/else/endif       Conditional control"
    write(*,*) "print                    Print variables and strings"
    write(*,*) "stop                     Stop and exit entire shell"
    write(*,*) "help                     Display this help message"
  end subroutine exechelp



  subroutine execprint()
    type(str) :: atom
    integer :: func
    real(fdp) :: x
    logical :: printed
    character :: natm

    x = 0.0d0
    printed = .false.
    
    call getnatm(natm)

    do while(natm /= blank_)
       select case(natm)
       case(string_)
          call popstr(atom)
          if (errorok) then
             if (stdout /= -1) then
                write(stdout, '(a)', advance='no') linestr(atom)
                printed = .true.
             else
                write(*, '(a)', advance='no') linestr(atom)
                printed = .true.
             end if
          end if
          
       case(function_)
          call popfn(func)
          call evaluatefn(func, x)
          if (errorok) then
             if (stdout /= -1) then
                write(stdout, '(f12.5)', advance='no') x
                printed = .true.
             else
                write(*, '(f12.5)', advance='no') x
                printed = .true.
             end if
          end if
         
       end select

       call getnatm(natm)
    end do

    if (printed) then
       if (stdout /= -1) then
          write(stdout,*) ""
       else
          write(*,*) ""
       end if
    end if
  end subroutine execprint



  subroutine execvar()
    integer :: var, func
    real(fdp) :: x

    x = 0
    call popint(var)
    call popfn(func)
    call evaluatefn(func, x)
    call setvarval(var, x)
  end subroutine execvar



  subroutine execarr()
    integer :: arr, ndims, func, i
    integer, dimension(:), allocatable :: dims
    real(fdp) :: x, tmp

    x = 0
    call popint(arr)
    ndims = arraydims(arr)
    allocate(dims(ndims))
    
    do i = 1, ndims
       call popfn(func)
       call evaluatefn(func, tmp)
       dims(i) = nint(tmp)
    end do

    call popfn(func)
    call evaluatefn(func, x)
    call setarrayval(arr, dims, ndims, x)
    deallocate(dims)
  end subroutine execarr



  recursive subroutine execdo()
    real(fdp) :: x, dostart, doend, doinc
    integer :: dovar, progp1, progp2, func, i

    call pushcall(do_)

    if (numfns() > 0) then
       x = 0
       dostart = 0.0d0
       doend = 1.0d0
       doinc = 0.0d0
       dovar = 0

       call popvar(dovar)
       call popfn(func)
       call evaluatefn(func, dostart)
       call popfn(func)
       call evaluatefn(func, doend)

       if (numfns() > 2) then
          call popfn(func)
          call evaluatefn(func, doinc)
       else
          doinc = 1.0d0
       end if

       progp1 = progp
       call popprogp(progp2, i)
       x = dostart

       do while(x <= doend)
          call pushcall(do_)
          progp = progp1
          call setvarval(dovar, x)
          call evalexecdriver()
          call popcall(i)

          if (i == exit_) exit
          
          x = x + doinc
       end do
       
       progp = progp2
       
       call setvarval(dovar, x)
    else
       progp1 = progp
       call popprogp(progp2, i)
       
       do 
          call pushcall(do_)
          progp = progp1
          call evalexecdriver()
          call popcall(i)
          
          if (i == exit_) exit
       end do
       
       progp = progp2
    end if

    call popcall(i)
  end subroutine execdo



  recursive subroutine execif()
    integer :: func, i
    real(fdp) :: x

    call pushcall(if_)
    x = 0.0d0
    call popfn(func)
    call evaluatefn(func, x)

    if (x > 0) then
       call evalexecdriver()
       call popcall(i)
       if (i == else_) call popprogp(progp, i)
    else
       call popcall(i)
       call popprogp(progp, i)

       if (i == else_) then
          call pushcall(i)
          call evalexecdriver()
          call popcall(i)
       end if
    end if

    if (i == cycle_ .or. i == exit_) then
       call pushcall(i)
    end if
  end subroutine execif



  recursive subroutine execinlineif()
    integer :: func, i, cmd
    real(fdp) :: x
    logical :: cont

    cont = .true.
    
    call pushcall(inlineif_)
    x = 0.0d0
    call popfn(func)
    call evaluatefn(func, x)

    if (x > 0) then
       call popcmd(cmd)
       call execcmd(cmd, cont)
    end if

    call popcall(i)
  end subroutine execinlineif



  subroutine execenddo(cont)
    logical, intent(out) :: cont
    integer :: i

    call popcall(i)
    cont = .false.
    call pushcall(enddo_)
  end subroutine execenddo



  subroutine execthen()
    integer :: i

    call popcall(i)
    call pushcall(then_)
  end subroutine execthen



  subroutine execelse(cont)
    logical, intent(out) :: cont
    integer :: i
    
    call popcall(i)
    cont = .false.
    call pushcall(else_)
  end subroutine execelse



  subroutine execendif(cont)
    logical, intent(out) :: cont
    integer :: i
    
    call popcall(i)
    cont = .false.
    call pushcall(endif_)
  end subroutine execendif
  


  subroutine execexit(cont)
    logical, intent(out) :: cont
    integer :: i

    i = searchcall(do_)

    call setcalldepth(i)
    call popcall(i)
    call pushcall(exit_)
    cont = .false.
  end subroutine execexit



  subroutine execcycle(cont)
    logical, intent(out) :: cont
    integer :: i
    
    i = searchcall(do_)
    call setcalldepth(i)
    call popcall(i)
    call pushcall(cycle_)
    cont = .false.
  end subroutine execcycle



  subroutine execsubroutine()
  end subroutine execsubroutine

  

  subroutine execendsubroutine()
  end subroutine execendsubroutine



  subroutine execcall()
  end subroutine execcall

end module evalexec
