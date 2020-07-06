#include "macros.h"

module parse
  use evalexec

  implicit none

  private :: parseline, parsecmd, equalfunc, parsevar,&
       & parseinds, parsearr, parsehelp, parseprint, parsedo, parseif,&
       & parseenddo, parsethen, parseelse, parseendif, parseend,&
       & parseexit, parsecycle

contains
  !
  ! Driver routine
  !
  recursive subroutine parsedriver()
    character(linelength) :: line
    character(20) :: fmt
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
       
       if (stdin /= -1) then
          read(stdin, '(a)', end=1000, err=1000) line
       else
          write(fmt,'(a,i5,a)') '(',calldepth*2-1,'x,a)'
          write(*, fmt, advance='no') prompt(min(calldepth, size(prompt)))//' '
          read(*, '(a)', end=1000, err=1000) line
       end if
       
       call pushline(line)
       call parseline(cont)

       if ((.not. errorok) .and. calldepth == 1 .and. stdin == -1) then
          call reseterror()
       end if
    end do

    1000 return
  end subroutine parsedriver



  !
  !  The parse engine
  !
  recursive subroutine parseline(cont)
    logical, intent(inout) :: cont
    type(str) :: atom
    integer :: command, obj, oldprogp

    oldprogp = progp
    call getatom(' (=', atom)
    call getcommand(atom, command, obj)
    call pushcmd(command)
    call parsecmd(atom, command, obj, cont)

    if (calldepth == 1) then
       progp = oldprogp
       call resetstkp()
       call evalline(cont)
    end if
  end subroutine parseline



  recursive subroutine parsecmd(atom, command, obj, cont)
    type(str), intent(in) :: atom
    integer, intent(in) :: command, obj
    logical, intent(inout) :: cont
    integer :: cstk

    call getcall(cstk)

    select case(command)
    case(do_)
       if (cstk == inlineif_) then
          call seterror('Invalid command in an inline IF statement')
          return
       end if

       call parsedo()
       
    case(enddo_)
       call parseenddo(cont)
       
    case(if_)
       if (cstk == inlineif_) then
          call seterror('Invalid command in an inline IF statement')
          return
       end if

       call parseif()
       
    case(then_)
       call parsethen()
       
    case(else_)
       call parseelse(cont)
       
    case(endif_)
       call parseendif(cont)
       
    case(end_)
       if (cstk == inlineif_) then
          call seterror('Invalid command in an inline IF statement')
          return
       end if

       call parseend(cont)
       
    case(print_)
       call parseprint()
       
    case(exit_)
       call parseexit()
       
    case(cycle_)
       call parsecycle()
       
    case(help_)
       if (cstk == inlineif_) then
          call seterror('Invalid command in an inline IF statement')
          return
       end if

      call parsehelp()

   case(call_)
      call parsecall()

   case(subroutine_)
      call parsesubroutine()
       
    case(var_)
       call parsevar(obj, atom)

    case(arr_)
       call parsearr(obj, atom)

    case(stop_, nop_)
       
    case default
       call seterror('Unknown command: '//linestr(atom))
       return
       
    end select
  end subroutine parsecmd



  !
  !  Routines to parse a command from a text string
  !
  subroutine equalfunc()
    type(str) :: s
    character :: c

    call skipspace()
    c = getchar()

    if (c /= '=') then
       call seterror('Malformed assignment statement')
       return
    end if
    
    call skipspace()
    
    s%i = linep()
    s%e = lenline()
    
    if (s%i > s%e) then
       call seterror('Malformed assignment statement')
       return
    end if

    call trimstr(s)
    call pushstr(s)
  end subroutine equalfunc



  subroutine parsevar(var, atom)
    integer, intent(in) :: var
    type(str), intent(in) :: atom
    type(str) :: s
    
    call pushint(var)
    if (errorok) call equalfunc()

    if (var == udef_) then
       s = atom
       call trimstr(s)
       call pushstr(s)
    end if
  end subroutine parsevar



  function parseinds() result(ninds)
    character :: c
    integer :: ninds, i
    type(str) :: s
    type(str), dimension(maxinds) :: inds

    call skipspace()
    c = getchar()

    if (c /= '(') then
       call seterror('Malformed index')
       return
    end if

    call getatom(',)', s)
    c = getchar()

    if (c /= ')' .and. c /= ',') then
       call seterror('Malformed index')
       return
    end if

    call trimstr(s)
    ninds = 1
    inds(ninds) = s

    do while (c /= ')')
       call getatom(',)', s)
       c = getchar()
       ninds = ninds + 1

       if (c /= ')' .and. c /= ',') then
          call seterror('Malformed index')
          return
       end if
       
       if (ninds > maxinds) then
          call seterror('Maximum number of indicies exceeded')
          return
       end if
       
       call trimstr(s)
       inds(ninds) = s
    end do

    call pushint(ninds)
    
    do i = 1, ninds
       call pushstr(inds(i))
    end do
  end function parseinds



  subroutine parsearr(arr, atom)
    integer, intent(in) :: arr
    type(str), intent(in) :: atom
    type(str) :: s
    integer :: ninds

    call pushint(arr)
    if (errorok) ninds = parseinds()
    if (errorok) call equalfunc()

    if (arr == udef_) then
       s = atom
       call trimstr(s)
       call pushstr(s)
    end if
  end subroutine parsearr



  subroutine parsehelp()
    if (linep() < lenline()) call seterror('Extraneous characters after HELP')
  end subroutine parsehelp



  subroutine parseprint()
    type(str) :: s
    character :: c

    do while(linep() <= lenline())
       call skipspace()
       call getatom(',', s)
       call trimstr(s)

       if (s%i > s%e) then
          call seterror('Malformed print statement')
          return
       end if

       if ((linechar(s%i) == "'" .and. linechar(s%e) == "'") .or. (linechar(s%i) == '"' .and. linechar(s%e) == '"')) then
          s%i = s%i + 1
          s%e = s%e - 1
          call pushstr(s)
       else
          call pushfnstr(s)
       end if

       c = getchar()

       if (c == ',' .and. linep() > lenline()) then
          call seterror('Premature end of print')
          return
       else if (c /= ',' .and. linep() <= lenline()) then
          call seterror('Invalid character: '//c)
          return
       end if
    end do
  end subroutine parseprint



  recursive subroutine parsedo()
    type(str) :: s
    integer :: i, progp1, progp2

    call pushcall(do_)
    call skipspace()
    progp1 = progp
    
    if (linep() <= lenline()) then
       call getatom('=', s)

       if (getchar() /= '=') then
          call seterror('Syntax error in DO statement')
          call popcall(i)
          return
       end if
       
       call trimstr(s)
       call pushvarstr(s)
       call getatom(',', s)
       
       if (getchar() /= ',') then
          call seterror('Syntax error in DO statement')
          call popcall(i)
          return
       end if
       
       call trimstr(s)
       call pushstr(s)
       call getatom(',', s)
       
       if (s%i>s%e) then
          call seterror('Syntax error in DO statement')
          call popcall(i)
          return
       end if
       
       call trimstr(s)
       call pushstr(s)
       
       if (getchar() == ',') then
          s%i = linep()
          s%e = lenline()
          call trimstr(s)
          call pushstr(s)
       end if
    end if

    call parsedriver()
    call popcall(i)
    
    if (i /= enddo_) call seterror('Premature end of DO statement')
    
    progp2 = progp
    progp = progp1
    call pushprogp(progp2, i)
    progp = progp2
  end subroutine parsedo



  recursive subroutine parseif()
    type(str) :: s
    integer :: progp1, progp2, cmd, obj, i
    logical :: cont

    call pushcall(if_)
    call skipspace()

    if (linep() > lenline()) then
       call seterror('Malformed IF statement')
       call popcall(i)
       return
    end if

    if( getchar() /= '(') then
       call seterror('Malformed IF statement')
       call popcall(i)
       return
    end if

    call getatom(')', s)

    if (getchar() /= ')') then
       call seterror('Malformed IF statement')
       call popcall(i)
       return
    end if

    call trimstr(s)
    call pushstr(s)
    call skipspace()
    call getatom(' ', s)
    call trimstr(s)

    if (linestr(s) == 'then') then
       call popcall(i)
       call pushcall(then_)
       progp1 = progp
       call parsedriver()
       call popcall(i)
       progp2 = progp
       progp = progp1
       call pushprogp(progp2, i)
       progp = progp2

       if (i == else_) then
          call pushcall(i)
          progp1 = progp
          call parsedriver()
          call popcall(i)
          progp2 = progp
          progp = progp1
          call pushprogp(progp2, i)
          progp = progp2
       end if

       if (i /= endif_) call seterror('Premature end of IF statement')
    elseif (s%e >= s%i) then
       call popcall(i)
       call pushcall(inlineif_)
       call modifycmd(inlineif_)
       call getcommand(s, cmd, obj)
       call pushcmd(cmd)
       cont = .true.
       call parsecmd(s, cmd, obj, cont)
       call popcall(i)

       if (i /= inlineif_) call seterror('Incorrect syntax for an inline IF statement')
    end if
  end subroutine parseif



  subroutine parseenddo(cont)
    logical, intent(out) :: cont
    integer :: i

    call popcall(i)

    if (linep() < lenline()) call seterror('Extraneous characters after ENDDO')

    if (i == do_) then
       cont = .false.
       call pushcall(enddo_)
    else
       call seterror('No matching DO statement')
    end if
  end subroutine parseenddo



  subroutine parsethen()
    integer :: i

    call popcall(i)

    if (linep() < lenline()) call seterror('Extraneous characters after THEN')

    if (i == if_) then
       call pushcall(then_)
    else
       call seterror('Malformed IF-THEN statement')
    end if
  end subroutine parsethen



  subroutine parseelse(cont)
    logical, intent(out) :: cont
    integer :: i
    
    call popcall(i)

    if (i == then_) then
       cont = .false.
       call pushcall(else_)
    else
       call seterror('Malformed IF-THEN-ELSE statement')
    end if
  end subroutine parseelse



  subroutine parseendif(cont)
    logical, intent(out) :: cont
    integer :: i
    
    call popcall(i)

    if (linep() < lenline()) call seterror('Extraneous characters after ENDIF')

    if (i == then_ .or. i == else_) then
       cont = .false.
       call pushcall(endif_)
    else
       call seterror('Malformed IF-ENDIF statement')
    end if
  end subroutine parseendif



  subroutine parseendsubroutine(cont)
    logical, intent(out) :: cont
    integer :: i

    call popcall(i)
    cont = .false.
    call pushcall(endsubroutine_)
  end subroutine parseendsubroutine
  


  subroutine parseend(cont)
    logical, intent(out) :: cont
    type(str) :: s

    call skipspace()
    call getatom(' ', s)

    select case (linestr(s))
    case(commands(do_))
       call modifycmd(enddo_)
       call parseenddo(cont)

    case(commands(if_))
       call modifycmd(endif_)
       call parseendif(cont)

    case(commands(subroutine_))
       call modifycmd(endsubroutine_)
       call parseendsubroutine(cont)

    case default
       call seterror('Mismatched END')
    end select
  end subroutine parseend



  subroutine parseexit()
    integer :: i

    if (linep() < lenline()) call seterror('Extraneous characters after CYCLE')

    i = searchcall(do_)

    if (i == 0) then
       call seterror('EXIT not within a DO loop')
    end if
  end subroutine parseexit



  subroutine parsecycle()
    integer :: i
    
    if (linep() < lenline()) call seterror('Extraneous characters after CYCLE')
    
    i = searchcall(do_)
    
    if (i == 0) then
       call seterror('CYCLE not within a DO loop')
    end if
  end subroutine parsecycle



  subroutine parsecall()
    type(str) :: s
    integer :: ninds

    call skipspace()

    if (linep() > lenline()) then
       call seterror('Malformed call declaration')
       return
    end if

    call getatom('(', s)

    if (s%e < s%i .or. .not. strname(s)) then
       call seterror('Illegal subroutine name: '//linestr(s))
       return
    end if

    call skipspace()

    if (nextchar() /= '(' .and. linep() <= lenline()) then
       call seterror('Malformed call declatation')
       return
    end if
         
    call trimstr(s)
    call pushstr(s)
    ninds = parseinds()
  end subroutine parsecall



  subroutine parsesubroutine()
    type(str) :: s
    integer :: i, progp1, progp2, ninds
    character :: c

    call pushcall(subroutine_)
    call skipspace()

    if (linep() > lenline()) then
       call seterror('Malformed subroutine declaration')
       call popcall(i)
       return
    end if

    call getatom('(', s)

    if (s%e < s%i .or. .not. strname(s)) then
       call seterror('Illegal subroutine name: '//linestr(s))
       call popcall(i)
       return
    end if

    call skipspace()

    if (nextchar() /= '(' .and. linep() <= lenline()) then
       call seterror('Malformed subroutine declatation')
       call popcall(i)
       return
    end if
         
    call trimstr(s)
    call pushstr(s)
    progp1 = progp
    ninds = parseinds()
    call parsedriver()
    call popcall(i)

    if (i /= endsubroutine_) call seterror('Malformed subroutine declaration')

    progp2 = progp
    progp = progp1
    call pushprogp(progp2, i)
    call registersub(s, ninds)
    progp = progp2
  end subroutine parsesubroutine

end module parse
