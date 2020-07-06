#include "macros.h"

module progstack
  use error
  use interpreter

  implicit none

  private :: printprogstack, pushstk, popstk,&
       & remstk, insstk, pushints, popints, remints, insints,&
       & pushstrs, popstrs, remstrs , popfnstr, insint, popvarstr

contains

  subroutine printprogstack()
    write(*,'(a,i4,a)') '====',progp,'===='
    write(*,*) nodes(progp)%stkp, nodes(progp)%linep, nodes(progp)%intp
    write(*,*) nodes(progp)%stk
    write(*,*) nodes(progp)%ints
    write(*,'(a)') '============'
  end subroutine printprogstack



  subroutine resetprogstack()
    integer :: i, j

    nnodes = 0

    do i=1, size(nodes)
       do j = 1, size(nodes(i)%ints)
          if (nodes(i)%ints(j) > functionstart .and. nodes(i)%ints(j) < functionend) &
               call destroyfn(nodes(i)%ints(j))
       end do

       call initnode(i)
    end do
  end subroutine resetprogstack



  subroutine initprogstack()
    integer :: i

    allocate(nodes(allocsize))

    do i=1, size(nodes)
       allocate(nodes(i)%ints(allocsize), nodes(i)%stk(allocsize))
    end do

    call resetprogstack()
  end subroutine initprogstack



  subroutine resetstkp()
    nodes(progp)%stkp = 0
    nodes(progp)%linep = 0
    nodes(progp)%intp = 0
  end subroutine resetstkp



  function evaluated()
    logical :: evaluated

    evaluated = nodes(progp)%evaluated
  end function evaluated



  subroutine setevaluated()
    nodes(progp)%evaluated = .true.
  end subroutine setevaluated
    


  subroutine pushline(line)
    character(*), intent(in) :: line
    integer :: i, l

    call checknnodes()
    nodes(progp)%line = TRIM(line)
    l = len_trim(nodes(progp)%line)
    i = scan(nodes(progp)%line(:l), commentchars)
    l = merge(l, i-1, i == 0)
    nodes(progp)%lenline = len_trim(nodes(progp)%line(:l))
    nodes(progp)%linep = 1
  end subroutine pushline



  subroutine getatom(sep, atom)
    character(*), intent(in) :: sep
    type(str), intent(out) :: atom
    integer :: i, bstack
    character :: c, quote
    
    bstack = 0
    quote = ''
    atom%i = linep()
    atom%e = 0

    if (lenline() == 0) return

    do  i = linep(), lenline()
       c = nodes(progp)%line(i:i)

       if (quote == '') then
          if (bstack == 0 .and. verify(c, sep) == 0) exit
          
          if (c == '"' .or. c == "'") then
             quote = c
          else if (c == '(') then
             bstack = bstack + 1
          else if (bstack > 0 .and. c == ')') then
             bstack = bstack - 1
          end if
       else if (quote == c) then
          quote = ''
       end if
    end do

    atom%e = i-1
    nodes(progp)%linep = i
  end subroutine getatom



  subroutine getcommand(atom, command, obj)
    type(str), intent(in) :: atom
    integer, intent(out) :: command, obj
    integer :: j

    command = nop_
    obj = udef_

    if (atom%i > 0 .and. atom%e >= atom%i) then
       do j = commandstart, commandend
          if (linestr(atom) == commands(j)(:lencommands(j))) then
             command = j
             return
          end if
       enddo

       do j = 1, numvars()
          if (linestr(atom) == var(j)) then
             command = var_
             obj = j
             return
          end if
       end do

       do j = 1, numarrays()
          if (linestr(atom) == arr(j)) then
             command = arr_
             obj = j
             return
          end if
       end do

       if (command == nop_) then
          if (lenline() > 0) then
             call skipspace()
             select case(nextchar())
             case('=')
                if (strname(atom)) then
                   command = var_
                else
                   call seterror('Illegal variable name: '//linestr(atom))
                end if
                
             case('(')
                if (strname(atom)) then
                   command = arr_
                else
                   call seterror('Illegal array name: '//linestr(atom))
                end if
                
             case default
                call seterror('Unknown command: '//linestr(atom))
                
             end select
          else
             call seterror('Unknown command: '//linestr(atom))
          end if
       end if
    end if
  end subroutine getcommand



  subroutine pushstk(c)
    character, intent(in) :: c
    character, dimension(:), pointer :: stk
    integer :: n, stkp

    n = size(nodes(progp)%stk)
    stkp = nodes(progp)%stkp + 1

    if (stkp >= n) then
       allocate(stk(stkp+allocsize))
       stk(:n) = nodes(progp)%stk
       stk(n+1:) = blank_
       deallocate(nodes(progp)%stk)
       nodes(progp)%stk => stk
    end if

    nodes(progp)%stk(stkp) = c
    nodes(progp)%stkp = stkp
  end subroutine pushstk



  subroutine popstk(c)
    character, intent(out) :: c
    integer :: stkp

    stkp = nodes(progp)%stkp + 1
    c = nodes(progp)%stk(stkp)
    nodes(progp)%stkp = stkp
  end subroutine popstk



  subroutine remstk(c)
    character, intent(out) :: c
    integer :: stkp, n, i

    stkp = nodes(progp)%stkp + 1
    c = nodes(progp)%stk(stkp)
    n = size(nodes(progp)%stk)

    do i = stkp, n-1
       nodes(progp)%stk(i) = nodes(progp)%stk(i+1)
    end do

    nodes(progp)%stk(n) = blank_
  end subroutine remstk



  subroutine insstk(c)
    character, intent(in) :: c
    character, dimension(:), pointer :: stk
    integer :: n, stkp, i

    n = size(nodes(progp)%stk)
    stkp = nodes(progp)%stkp + 1

    if (nodes(progp)%stk(n) /= blank_) then
       allocate(stk(stkp+allocsize))
       stk(:n) = nodes(progp)%stk
       stk(n+1:) = blank_
       deallocate(nodes(progp)%stk)
       nodes(progp)%stk => stk
    end if

    n = size(nodes(progp)%stk)

    do i = n, stkp+1, -1
       nodes(progp)%stk(i) = nodes(progp)%stk(i-1)
    end do

    nodes(progp)%stk(stkp) = c
    nodes(progp)%stkp = stkp
  end subroutine insstk



  subroutine getnatm(c)
    character, intent(out) :: c
    integer :: stkp

    stkp = nodes(progp)%stkp+1
    c = nodes(progp)%stk(stkp)
  end subroutine getnatm



  subroutine pushints(int)
    integer, intent(in) :: int
    integer, dimension(:), pointer :: ints
    integer :: n, intp

    n = size(nodes(progp)%ints)
    intp = nodes(progp)%intp + 1

    if (intp >= n) then
       allocate(ints(intp+allocsize))
       ints(:n) = nodes(progp)%ints
       ints(n+1:) = udef_
       deallocate(nodes(progp)%ints)
       nodes(progp)%ints => ints
    end if

    nodes(progp)%ints(intp) = int
    nodes(progp)%intp = intp
  end subroutine pushints



  subroutine popints(int)
    integer, intent(out) :: int
    integer :: intp

    intp = nodes(progp)%intp+1
    nodes(progp)%intp = intp
    int = nodes(progp)%ints(intp)
  end subroutine popints



  subroutine remints(int)
    integer, intent(out) :: int
    integer :: intp, n, i

    intp = nodes(progp)%intp+1
    int = nodes(progp)%ints(intp)
    n = size(nodes(progp)%ints)

    do i = intp, n-1
       nodes(progp)%ints(i) = nodes(progp)%ints(i+1)
    end do

    nodes(progp)%ints(n) = udef_
  end subroutine remints



  subroutine insints(int)
    integer, intent(in) :: int
    integer, dimension(:), pointer :: ints
    integer :: intp, n, i

    n = size(nodes(progp)%ints)
    intp = nodes(progp)%intp+1

    if (nodes(progp)%ints(n) /= udef_) then
       allocate(ints(n+allocsize))
       ints(:n) = nodes(progp)%ints
       ints(n+1:) = udef_
       deallocate(nodes(progp)%ints)
       nodes(progp)%ints => ints
    end if

    n = size(nodes(progp)%ints)

    do i = n, intp+1, -1
       nodes(progp)%ints(i) = nodes(progp)%ints(i-1)
    end do

    nodes(progp)%ints(intp) = int
    nodes(progp)%intp = intp
  end subroutine insints



  subroutine pushstrs(s)
    type(str), intent(in) :: s

    call pushints(s%i)
    call pushints(s%e)
  end subroutine pushstrs



  subroutine popstrs(s)
    type(str), intent(out) :: s

    call popints(s%i)
    call popints(s%e)
  end subroutine popstrs



  subroutine remstrs(s)
    type(str), intent(out) :: s

    call remints(s%i)
    call remints(s%e)
  end subroutine remstrs



  subroutine pushcmd(cmd)
    integer, intent(in) :: cmd

    call pushstk(command_)
    call pushints(cmd)
  end subroutine pushcmd



  subroutine popcmd(cmd)
    integer, intent(out) :: cmd
    character :: c

    call popstk(c)
    call popints(cmd)
  end subroutine popcmd


  
  subroutine pushstr(s)
    type(str), intent(in) :: s

    call pushstk(string_)
    call pushstrs(s)
  end subroutine pushstr



  subroutine popstr(s)
    type(str), intent(out) :: s
    character :: c

    call popstk(c)
    call popstrs(s)
  end subroutine popstr



  subroutine remstr(s)
    type(str), intent(out) :: s
    character :: c

    call remstk(c)
    call remstrs(s)
  end subroutine remstr



  subroutine pushfnstr(s)
    type(str), intent(in) :: s

    call pushstk(functionstr_)
    call pushstrs(s)
  end subroutine pushfnstr



  subroutine popfnstr(s)
    type(str), intent(out) :: s
    character :: c

    call popstk(c)
    call popstrs(s)
  end subroutine popfnstr



  subroutine remfnstr(s)
    type(str), intent(out) :: s
    character :: c

    call remstk(c)
    call remstrs(s)
  end subroutine remfnstr



  subroutine pushint(int)
    integer, intent(in) :: int

    call pushstk(int_)
    call pushints(int)
  end subroutine pushint



  subroutine popint(int)
    integer, intent(out) :: int
    character :: c

    call popstk(c)
    call popints(int)
  end subroutine popint



  subroutine remint(int)
    integer, intent(out) :: int
    character :: c
    
    call remstk(c)
    call remints(int)
  end subroutine remint



  subroutine insint(int)
    integer, intent(in) :: int

    call insstk(int_)
    call insints(int)
  end subroutine insint



  subroutine pushfn(fn)
    integer, intent(in) :: fn

    call pushstk(function_)
    call pushints(fn)
  end subroutine pushfn



  subroutine popfn(fn)
    integer, intent(out) :: fn
    character :: c
    
    call popstk(c)
    call popints(fn)
  end subroutine popfn



  subroutine insfn(fn)
    integer, intent(in) :: fn

    call insstk(function_)
    call insints(fn)
  end subroutine insfn



  function numfns()
    integer :: numfns, i

    numfns = 0

    do i = 1, size(nodes(progp)%stk)
       if (nodes(progp)%stk(i) == function_) numfns = numfns + 1
    end do
  end function numfns



  subroutine pushvar(var)
    integer, intent(in) :: var

    call pushstk(variable_)
    call pushints(var)
  end subroutine pushvar



  subroutine popvar(var)
    integer, intent(out) :: var
    character :: c

    call popstk(c)
    call popints(var)
  end subroutine popvar



  subroutine pushvarstr(s)
    type(str), intent(in) :: s

    call pushstk(variablestr_)
    call pushstrs(s)
  end subroutine pushvarstr



  subroutine popvarstr(s)
    type(str), intent(out) :: s
    character :: c

    call popstk(c)
    call popstrs(s)
  end subroutine popvarstr



  subroutine pushprogp(prgp, cll)
    integer, intent(in) :: prgp, cll

    call pushstk(progp_)
    call pushints(prgp)
    call pushints(cll)
  end subroutine pushprogp



  subroutine popprogp(prgp, cll)
    integer, intent(out) :: prgp, cll
    integer :: tmp
    character :: c

    call popstk(c)
    call popints(tmp)
    call popints(cll)
    prgp = tmp
  end subroutine popprogp



  subroutine remprogp(prgp, cll)
    integer, intent(out) :: prgp, cll
    integer :: tmp
    character :: c

    call remstk(c)
    call remints(tmp)
    call remints(cll)
    prgp = tmp
  end subroutine remprogp



  subroutine modifycmd(cmd)
    integer, intent(in) :: cmd
    integer :: i

    do i=1, size(nodes(progp)%stk)
       if (nodes(progp)%stk(i) == command_) exit
    end do

    nodes(progp)%ints(i) = cmd
  end subroutine modifycmd



end module progstack
