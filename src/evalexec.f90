#include "macros.h"

module evalexec
  use callstack
  use progstack

  implicit none

  private :: evalexecdriver, evalcmd, evalprint, evalvar, evalarr,&
       & evaldo, evalif, evalinlineif, execline, execcmd, exechelp,&
       & execprint, execvar, execarr, execdo, execif, execinlineif,&
       & execenddo, execthen, execelse, execendif, execexit, execcycle

  integer, save :: subcalldepth = 0
  integer, parameter :: maxsubcalldepth = 1000
  integer, parameter :: argvar = 1, argarr = 2, argexpr = 3

contains
  !
  !  Driver routine
  !
  recursive subroutine evalexecdriver()
    logical :: cont
    integer :: i

    cont = .true.

    do while(cont .and. (.not. gotstop) .and. errorok .and. .not. gotreturn)
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

    case(functiondef_)
       call evalfunction()

    case(call_)
       call evalcall()

    case(enddo_, then_, else_, endif_, exit_, cycle_, stop_, help_, nop_, endsubroutine_, endfunction_, return_)

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
    type(str) :: s, name
    integer :: var, func

    call remstr(name)
    call remstr(s)
    call createfn(s, func)

    if (varnum(name) == 0) call createvar(name)
    var = varnum(name)

    call pushvar(var)
    call pushfn(func)
  end subroutine evalvar



  subroutine evalarr()
    type(str) :: s, name
    integer :: arr, ndims, i, func
    integer, dimension(:), allocatable :: funcs

    call remstr(name)
    call remint(ndims)
    allocate(funcs(ndims))

    do i = 1, ndims
       call remstr(s)
       call createfn(s, funcs(i))
    end do

    call remstr(s)
    call createfn(s, func)

    if (arraynum(name) == 0) call createarr(name, ndims)
    arr = arraynum(name)

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



  subroutine evalfunction()
  end subroutine evalfunction



  subroutine evalcall()
    type(str) :: s, args(maxinds)
    integer :: subidx, nargs, i, slot, arr, nd, j, func
    integer, dimension(maxinds) :: lifns
    character(namelength) :: name, head
    character(linelength) :: text
    integer :: l, lp, p, depth, k

    call remstr(s)
    call remint(nargs)

    do i = 1, nargs
       call remstr(args(i))
    end do

    if (nargs == 1) then
       text = linestr(args(1))

       if (len_trim(text) == 0) nargs = 0
    end if

    name = linestr(s)
    subidx = findroutine(name)

    if (subidx == 0) then
       call seterror('Unknown subroutine: '//trim(name))
       return
    end if

    if (routinekind(subidx) /= subroutinekind) then
       call seterror('Cannot CALL a function: '//trim(name))
       return
    end if

    if (nargs /= routinenargs(subidx)) then
       call seterror('Wrong number of arguments in call to: '//trim(name))
       return
    end if

    call pushint(subidx)

    do i = 1, nargs
       text = linestr(args(i))
       text = adjustl(text)
       l = len_trim(text)

       if (l == 0) then
          call seterror('Missing argument in call to: '//trim(name))
          return
       end if

       if (index(text(:l), '(') == 0 .and. index(text(:l), ')') == 0) then
          slot = 0
          if (varstrnum(text, l) > 0) slot = varslot(varstrnum(text, l))

          if (slot > 0) then
             call pushint(argvar)
             call pushint(slot)
             cycle
          end if

          call createfnstr(text(:l), func)
          call pushint(argexpr)
          call pushfn(func)
          cycle
       end if

       lp = index(text(:l), '(')

       if (lp > 1) then
          head = text(:lp-1)
       else
          head = ''
       end if

       arr = 0

       if (lp > 1) arr = arrposbyname(trim(adjustl(head)))

       if (arr > 0 .and. text(l:l) == ')') then
          depth = 0
          k = 0
          p = lp

          do while (p <= l)
             if (text(p:p) == '(') depth = depth + 1
             if (text(p:p) == ')') depth = depth - 1
             if (depth == 0) exit
             p = p + 1
          end do

          if (p == l) then
             nd = 0
             k = lp + 1
             depth = 1

             do j = lp + 1, l - 1
                if (text(j:j) == '(') depth = depth + 1
                if (text(j:j) == ')') depth = depth - 1

                if (text(j:j) == ',' .and. depth == 1) then
                   if (len_trim(text(k:j-1)) == 0) then
                      call seterror('Missing array index in call to: '//trim(name))
                      return
                   end if

                   call createfnstr(text(k:j-1), func)
                   nd = nd + 1
                   lifns(nd) = func
                   k = j + 1
                end if
             end do

             if (len_trim(text(k:l-1)) == 0) then
                call seterror('Missing array index in call to: '//trim(name))
                return
             end if

             call createfnstr(text(k:l-1), func)
             nd = nd + 1
             lifns(nd) = func

             if (nd /= arraydims(arr)) then
                call seterror('Incorrect number of array dimensions')
                return
             end if

             call pushint(argarr)
             call pushint(arr)
             call pushint(nd)

             do j = 1, nd
                call pushfn(lifns(j))
             end do

             cycle
          end if
       end if

       call createfnstr(text(:l), func)
       call pushint(argexpr)
       call pushfn(func)
    end do
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

    case(functiondef_)
       call execfunction()

    case(endfunction_)
       call execendfunction()

    case(call_)
       call execcall()

    case(return_)
       call execreturn()

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
    call popvar(var)
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

       if (.not. errorok) then
          progp = progp2
          call popcall(i)
          return
       end if

       if (doinc == 0.0d0) then
          call seterror('DO increment is zero')
          progp = progp2
          call popcall(i)
          return
       end if

       x = dostart

       do while(errorok .and. .not. gotreturn .and. ((doinc > 0.0d0 .and. x <= doend) .or. &
            (doinc < 0.0d0 .and. x >= doend)))
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



  subroutine execfunction()
  end subroutine execfunction



  subroutine execendfunction()
  end subroutine execendfunction



  subroutine execreturn()
    integer :: i

    i = searchcall(subroutine_)

    if (i == 0) i = searchcall(functiondef_)

    if (i == 0) then
       call seterror('RETURN outside a subroutine or function')
    else
       gotreturn = .true.
    end if
  end subroutine execreturn



  recursive subroutine execcall()
    integer :: subidx, nargs, i, j
    integer, dimension(maxinds) :: kinds, slots, arrs, nds, vfns
    integer, dimension(maxinds, maxinds) :: ifns, fidx
    integer, dimension(maxinds) :: tmpinds
    real(fdp), dimension(maxinds) :: argvals, tempvals
    real(fdp) :: x, result

    call popint(subidx)
    nargs = routinenargs(subidx)

    do i = 1, nargs
       call popint(kinds(i))

       if (kinds(i) == argvar) then
          call popint(slots(i))
       else if (kinds(i) == argarr) then
          call popint(arrs(i))
          call popint(nds(i))

          do j = 1, nds(i)
             call popfn(ifns(i, j))
          end do
       else
          call popfn(vfns(i))
          slots(i) = 0
       end if
    end do

    do i = 1, nargs
       if (kinds(i) == argarr) then
          do j = 1, nds(i)
             call evaluatefn(ifns(i, j), x)
             fidx(i, j) = nint(x)
             tmpinds(j) = fidx(i, j)
          end do

          argvals(i) = arrayval(arrs(i), tmpinds, nds(i))
          slots(i) = 0
       else if (kinds(i) == argexpr) then
          call evaluatefn(vfns(i), argvals(i))
          slots(i) = 0
       else
          argvals(i) = 0.0d0
       end if
    end do

    if (.not. errorok) return

    call driveroutine(subidx, slots, argvals, nargs, .false., result, tempvals)

    if (.not. errorok .or. gotstop) return

    do i = 1, nargs
       if (kinds(i) == argarr) then
          do j = 1, nds(i)
             tmpinds(j) = fidx(i, j)
          end do

          call setarrayval(arrs(i), tmpinds, nds(i), tempvals(i))
       end if
    end do
  end subroutine execcall


  recursive subroutine driveroutine(subidx, slots, vals, nargs, want_result, result, tempvals)
    integer, intent(in) :: subidx, nargs
    integer, dimension(*), intent(in) :: slots
    real(fdp), dimension(*), intent(in) :: vals
    logical, intent(in) :: want_result
    real(fdp), intent(out) :: result
    real(fdp), dimension(*), intent(out) :: tempvals
    integer :: i, progp_save, progp1, bodyend, depth0, marker, fi, base, nbody, oldbias
    logical :: cont
    character(namelength) :: fname
    character(namelength), dimension(maxinds) :: dnames

    result = 0.0d0

    if (nargs > 0) tempvals(1:nargs) = 0.0d0

    subcalldepth = subcalldepth + 1

    if (subcalldepth > maxsubcalldepth) then
       call seterror('Maximum call depth exceeded')
       subcalldepth = subcalldepth - 1
       return
    end if

    if (.not. errorok) then
       subcalldepth = subcalldepth - 1
       return
    end if

    if (routineactive(subidx) /= 0) then
       call seterror('Recursive call is not supported: '//trim(routinename(subidx)))
       subcalldepth = subcalldepth - 1
       return
    end if

    call setactive(subidx, 1)

    progp_save = progp
    depth0 = calldepth
    oldbias = progp_bias

    if (routinekind(subidx) == functionkind) then
       marker = functiondef_
    else
       marker = subroutine_
    end if

    call pushcall(marker)

    do i = 1, routinenargs(subidx)
       dnames(i) = routinedummy(subidx, i)
    end do

    call childnamespace(dnames, slots, vals, nargs)

    progp1 = routineprogp(subidx)
    bodyend = routineend(subidx) - 1
    nbody = bodyend - progp1

    if (nbody > 0) then
       base = allocnodes(nbody)

       do i = 0, nbody - 1
          call copynodes(progp1+1+i, base+i)
       end do

       progp_bias = base - 1 - progp1
       progp = base - 1
       bodyend = base + nbody - 1
       cont = .true.

       do while (cont .and. .not. gotstop .and. errorok .and. .not. gotreturn .and. progp < bodyend)
          call getcall(i)

          if (i == cycle_ .or. i == exit_) then
             cont = .false.
             exit
          end if

          progp = progp + 1
          call resetstkp()
          call evalline(cont)
       end do
    end if

    call getcall(i)

    if (i /= marker) then
       if (errorok) call seterror('EXIT or CYCLE may not cross a subroutine or function boundary')
    end if

    call setcalldepth(depth0)

    if (errorok .and. .not. gotstop) then
       if (want_result) then
          fname = routinename(subidx)
          fi = varstrnum(fname, len_trim(fname))

          if (fi == 0) then
             call seterror('Function did not assign a return value: '//trim(fname))
          else
             result = varval(fi)
          end if
       end if

       do i = 1, nargs
          tempvals(i) = varval(i)
       end do
    end if

    progp = progp_save

    if (nbody > 0) then
       do i = 0, nbody - 1
          call destroynodefn(base+i)
       end do

       call truncnodes(base-1)
    end if

    progp_bias = oldbias
    call dropchild()
    gotreturn = .false.
    call setactive(subidx, 0)
    subcalldepth = subcalldepth - 1
  end subroutine driveroutine


  recursive subroutine drive_function(subidx, argvals, nargs, result)
    integer, intent(in) :: subidx, nargs
    real(fdp), dimension(*), intent(in) :: argvals
    real(fdp), intent(out) :: result
    integer, dimension(maxinds) :: slots
    real(fdp), dimension(maxinds) :: tempvals
    integer :: i

    do i = 1, maxinds
       slots(i) = 0
    end do

    call driveroutine(subidx, slots, argvals, nargs, .true., result, tempvals)
  end subroutine drive_function

end module evalexec
