#include "macros.h"

module interpreter
  use error
  use extrafunc
  use variables

  implicit none

  private :: s_firstrun, s_funcpush, s_checkoperations,&
       & s_checkcomponents, s_readnumber, s_readtext, s_readoperator,&
       & s_readbracket, s_readcomma, s_parseatom, s_getnextatom,&
       & s_peeknextatom, s_parseblock, s_parsefn, s_evaluate,&
       & f_clean, f_lower,&
       & f_numbervars
  private :: cachefn, cachedfuncs, cfnp

  type cachefn
     real(fdp), dimension(:), pointer :: numbers
     integer, dimension(:), pointer :: commands
  end type cachefn

  type(cachefn), dimension(:), pointer, save :: cachedfuncs
  integer, save :: cfnp

contains

  subroutine s_firstrun()
    logical, save :: firstrun = .true.
    integer :: i

    if (firstrun) then
       allocate(cachedfuncs(allocsize))
       cfnp = 0

       do i=1, size(cachedfuncs)
          allocate(cachedfuncs(i)%numbers(allocsize), cachedfuncs(i)%commands(allocsize))
          cachedfuncs(i)%numbers = 0.0d0
          cachedfuncs(i)%commands = endfunc_
       end do

       firstrun = .false.
    end if
  end subroutine s_firstrun


  
  function f_clean(func) result(tmpfunc)
    character(len=*), intent(in) :: func
    character(len=len(func)) :: tmpfunc
    integer :: i
    
    tmpfunc = TRIM(func)
    i = verify(trim(tmpfunc), allowedchars)

    do while (i /= 0)
       if (tmpfunc(i:i) == ' ') then
          tmpfunc(i:) = tmpfunc(i+1:)
       else
          call seterror('Illegal character: '//tmpfunc(i:))
          tmpfunc = ''
          exit
       end if

       i = verify(trim(tmpfunc), allowedchars)
    end do
  end function f_clean



  function f_lower(s, n) result(res)
    integer, intent(in) :: n
    character(len=n), intent(in) :: s
    character(len=n) :: res
    integer :: i, c
    
    res = s
    i = scan(res, upperchars)

    do while (i > 0)
       c = ichar(res(i:i))

       if ((c <= ichar('Z')) .and. (c >= ichar('A'))) res(i:i) = achar(c-ichar('A')+ichar('a'))

       i = scan(res, upperchars)
    end do
  end function f_lower



  function f_numbervars(vars) result(numvars)
    character(len=*), intent(in) :: vars
    integer :: numvars
    character(len=len(vars)) :: tmpvars
    character(len=namelength) :: tmpvar

    tmpvars = TRIM(vars)
    numvars = 0

    do while (len_trim(tmpvars) > 0)
       read(tmpvars, *) tmpvar
       numvars = numvars + 1
       tmpvars = tmpvars(index(tmpvars, trim(tmpvar))+len_trim(tmpvar):)
    end do
  end function f_numbervars



  subroutine s_funcpush(funcstack, funcstackn, command, number)
    type(cachefn), intent(inout) :: funcstack
    integer, intent(inout) :: funcstackn
    real(fdp), intent(in) :: number
    integer, intent(in) :: command
    type(cachefn) :: tmpstack
    integer :: n

    funcstackn = funcstackn + 1
    n = size(funcstack%numbers)

    if (funcstackn > n-2) then
       allocate(tmpstack%numbers(funcstackn+allocsize), tmpstack%commands(funcstackn+allocsize))
       tmpstack%numbers(:n) = funcstack%numbers(:n)
       tmpstack%commands(:n) = funcstack%commands(:n)
       tmpstack%numbers(n+1:) = 0.0d0
       tmpstack%commands(n+1:) = endfunc_
       deallocate(funcstack%numbers, funcstack%commands)
       funcstack%numbers => tmpstack%numbers
       funcstack%commands => tmpstack%commands
    end if

    funcstack%commands(funcstackn) = command
    funcstack%numbers(funcstackn) = number
  end subroutine s_funcpush



  subroutine s_checkoperations(func, lenfunc, command, nextcommand)
    integer, intent(in) :: lenfunc
    character(lenfunc), intent(in) :: func
    integer, intent(in) :: command, nextcommand

    select case(command)

    case(number_)
       select case(nextcommand)

       case(number_)
          call seterror('Number following number: '//func)

       case(variablestart:variableend)
          call seterror('Variable following number: '//func)

       case(bracket_)
          call seterror('Bracket following number: '//func)

       case(expressionstart:expressionend)
          call seterror('Expression following number: '//func)

       end select
       
    case(variablestart:variableend)
       select case(nextcommand)
          
       case(number_)
          call seterror('Number following variable: '//func)
          
       case(variablestart:variableend)
          call seterror('Variable following variable: '//func)
          
       case(bracket_)
          call seterror('Bracket following variable: '//func)
          
       case(expressionstart:expressionend)
          call seterror('Expression following variable: '//func)
          
       end select
       
    case(operatorstart:operatorend)
       select case(nextcommand)

       case(operatorstart:operatorend)
          call seterror('Operator following operator: '//func)

       case(endfunc_)
          call seterror('Premature end of function')

       end select
    end select
  end subroutine s_checkoperations



  subroutine s_checkcomponents(components, command)
    integer, intent(in) :: components, command
    
    if (components /= commandcomps(command)) call seterror('Incorrect components for: '//expressions(command))
  end subroutine s_checkcomponents



  subroutine s_readnumber(func, lenfunc, command, number)
    integer, intent(inout) :: lenfunc
    character(lenfunc), intent(inout) :: func
    integer, intent(out) :: command
    real(fdp), intent(out) :: number
    integer :: pos
    logical :: dflag, exponent, finish

    exponent = .false.
    dflag = .false.
    finish = .false.
    pos = 0
    number = 0.0d0
    command = nop_

    do while(pos < lenfunc .and. .not. finish)
       pos = pos + 1

       select case(func(pos:pos))

       case ('0':'9')
          if (dflag) then
             exponent = .true.
             dflag = .false.
          end if

       case ('d','e','D','E')
          if (dflag .or. exponent) then
             finish = .true.
          else
             dflag = .true.
          end if

       case('.')
          if (dflag .or. exponent) finish = .true.

       case('+', '-')
          if (dflag) then
             dflag = .false.
             exponent = .true.
          else
             finish = .true.
          end if

       case default
          finish = .true.

       end select
    end do

    if (pos == lenfunc .and. .not. finish) then
       read(func, *) number

       command = number_

       func = ''
       lenfunc = 0
    else
       read(func(:pos-1), *) number

       command = number_

       func = func(pos:)
       lenfunc = lenfunc - pos + 1
    end if

  end subroutine s_readnumber



  subroutine s_readtext(func, lenfunc, command, number)
    integer, intent(inout) :: lenfunc
    character(lenfunc), intent(inout) :: func
    integer, intent(out) :: command
    real(fdp), intent(out) :: number
    integer :: i, funccut, lvar

    funccut = 0
    command = nop_
    number = 0.0d0

    do i = expressionstart, expressionend
       if (expressionslen(i) < lenfunc) then
          if ((f_lower(func(:expressionslen(i)), expressionslen(i)) == &
               expressions(i)(:expressionslen(i))) .and. expressionslen(i) > funccut ) then
             command = i
             funccut = expressionslen(i)-1
          end if
       end if
    end do

    if (command == nop_) then
       do i = 1, numvars()
          lvar = lenvar(i)
          if (func(:min(lvar, lenfunc)) == trim(var(i)) .and. lvar > funccut) then
             command = variablestart+i
             funccut = lvar
          end if
       end do
    end if

    if (command == nop_) then
       do i = 1, numarrays()
          lvar = lenarray(i)
          if (func(:min(lvar+1, lenfunc)) == trim(arr(i))//'(' .and. lvar > funccut) then
             command = arraystart+i
             funccut = lvar
          end if
       end do
    end if

    if (command == nop_) then
       call seterror('Unknown text: '//func)

       func = ''
       lenfunc = 0
    end if
    
    if (lenfunc > funccut) then
       func = func(funccut+1:)
       lenfunc = lenfunc - funccut
    else
       func = ''
       lenfunc = 0
    end if
  end subroutine s_readtext



  subroutine s_readoperator(func, lenfunc, command, order)
    integer, intent(inout) :: lenfunc
    character(lenfunc), intent(inout) :: func
    integer, intent(out) :: command, order
    integer :: i, funccut

    funccut = 0
    command = nop_
    order = huge(order)

    do i = operatorstart, operatorend
       if (func(:min(operatorslen(i), lenfunc)) == operators(i) .and. &
            &operatorslen(i) > funccut ) then
          command = i
          order = operatorsorder(i)
          funccut = operatorslen(i)
       end if
    end do

    if (command == nop_) then
       call seterror('Unknown operator: '//func)

       func = ''
       lenfunc = 0
    end if

    if (lenfunc > funccut) then
       func = func(funccut+1:)
       lenfunc = lenfunc - funccut
    else
       func = ''
       lenfunc = 0
    end if
  end subroutine s_readoperator



  subroutine s_readbracket(func, lenfunc, subfunc, lensubfunc)
    integer, intent(inout) :: lenfunc
    character(lenfunc), intent(inout) :: func
    character(len=len(func)), intent(out) :: subfunc
    integer, intent(out) :: lensubfunc
    integer :: pos, count

    count = 0
    subfunc = ''
    
    do pos = 1, lenfunc
       select case(func(pos:pos))
          
       case('(')
          count = count + 1
          
       case(')')
          count = count - 1
          
       end select
       
       if (count == 0) exit
    end do
    
    subfunc = func(2:pos-1)
    lensubfunc = pos - 2

    if (count /= 0) then
       call seterror('Unbalanced brackets: '//func)
       
       func = ''
       lenfunc = 0
    end if
    
    if (lensubfunc == 0) then
       call seterror('Empty brackets encountered: '//func)
       
       func = ''
       lenfunc = 0
    end if
    
    if (lenfunc < pos+1) then
       func = ''
       lenfunc = 0
    else
       func = func(pos+1:)
       lenfunc = lenfunc - pos
    end if
  end subroutine s_readbracket



  subroutine s_readcomma(func, lenfunc, command, order)
    integer, intent(inout) :: lenfunc
    character(lenfunc), intent(inout) :: func
    integer, intent(out) :: command, order
    
    command = comma_
    order = 0
    
    if (lenfunc < 2) then
       func = ''
       lenfunc = 0
    else
       func = func(2:)
       lenfunc = lenfunc - 1
    end if
  end subroutine s_readcomma



  recursive subroutine s_parseatom(func, lenfunc, funcstack, funcstackn, command, number, order, components)
    type(cachefn), intent(inout) :: funcstack
    integer, intent(inout) :: funcstackn
    integer, intent(inout) :: lenfunc, components
    character(lenfunc), intent(inout) :: func
    integer, intent(in) :: command, order
    real(fdp), intent(in) :: number
    character(len=len(func)) :: tmpfunc
    integer :: lentmpfunc, nextcommand, nextorder
    real(fdp) :: nextnumber

    select case(command)

    case(number_, variablestart:variableend)
       call s_funcpush(funcstack, funcstackn, command, number)

    case(nop_, endfunc_)

    case(comma_)
       call s_parseblock(command, func, lenfunc, funcstack, funcstackn, order, components)
       components = components + 1
       
    case(bracket_)
       call s_readbracket(func, lenfunc, tmpfunc, lentmpfunc)
       call s_parsefn(tmpfunc, lentmpfunc, funcstack, funcstackn, components)

    case(operatorstart:operatorend)
       call s_parseblock(command, func, lenfunc, funcstack, funcstackn, order, components)
       call s_funcpush(funcstack, funcstackn, command, number)

    case(arraystart:arrayend)
       call s_getnextatom(func, lenfunc, nextcommand, nextnumber, nextorder)
       components = 1
       call s_parseatom(func, lenfunc, funcstack, funcstackn, nextcommand, nextnumber, nextorder, components)
       call checkarray(command-arraystart, components)
       call s_funcpush(funcstack, funcstackn, command, number)

    case(expressionstart:expressionend)
       call s_getnextatom(func, lenfunc, nextcommand, nextnumber, nextorder)
       components = 1
       call s_parseatom(func, lenfunc, funcstack, funcstackn, nextcommand, nextnumber, nextorder, components)
       call s_checkcomponents(components, command)
       call s_funcpush(funcstack, funcstackn, command, number)
       
    case default
       call seterror('Unknown input: '//func)
       
       func = ''
       lenfunc = 0

    end select
  end subroutine s_parseatom



  subroutine s_getnextatom(func, lenfunc, command, number, order)
    integer, intent(inout) :: lenfunc
    character(lenfunc), intent(inout) :: func
    integer, intent(out) :: command, order
    real(fdp), intent(out) :: number

    command = nop_
    order = huge(order)
    number = 0.0d0

    if (lenfunc > 0) then
       select case(func(1:1))
          
       case('0':'9', '.')
          call s_readnumber(func, lenfunc, command, number)
          
       case('+', '-', '/', '*', '^', '>', '!', '<', '=')
          call s_readoperator(func, lenfunc, command, order)
          
       case('a':'z','A':'Z')
          call s_readtext(func, lenfunc, command, number)
          
       case('(')
          command = bracket_

       case(',')
          call s_readcomma(func, lenfunc, command, order)
          
       case default
          if (lenfunc > 1) call seterror('Unknown input: '//func)
          
          func = ''
          lenfunc = 0
          command = endfunc_
          
       end select
    else
       command = endfunc_
    end if
  end subroutine s_getnextatom
  


  subroutine s_peeknextatom(func, lenfunc, command, number, order)
    integer, intent(in) :: lenfunc
    character(lenfunc), intent(in) :: func
    integer, intent(out) :: command, order
    real(fdp), intent(out) :: number
    character(len=lenfunc) :: tmpfunc
    integer :: lentmpfunc

    command = nop_
    order = huge(order)
    tmpfunc = func
    lentmpfunc = lenfunc

    call s_getnextatom(tmpfunc, lentmpfunc, command, number, order)
  end subroutine s_peeknextatom



  recursive subroutine s_parseblock(command, func, lenfunc, funcstack, n, blockorder, components)
    integer, intent(inout) :: lenfunc
    character(lenfunc), intent(inout) :: func
    type(cachefn), intent(inout) :: funcstack
    integer, intent(inout) :: n, components
    integer, intent(in) :: command, blockorder
    integer :: tcommand, nextcommand, nextorder
    real(fdp) :: nextnumber

    tcommand = command
    nextcommand = nop_
    nextorder = huge(nextorder)

    call s_peeknextatom(func, lenfunc, nextcommand, nextnumber, nextorder)
    call s_checkoperations(func, lenfunc, tcommand, nextcommand)

    if ((nextcommand == plus_ .or. nextcommand == minus_) .and. command == comma_) call s_funcpush(funcstack, n, number_, 0.0d0)

    do while(nextorder > blockorder .and. nextcommand /= endfunc_)
       call s_getnextatom(func, lenfunc, nextcommand, nextnumber, nextorder)
       call s_parseatom(func, lenfunc, funcstack, n, nextcommand, nextnumber, nextorder, components)
       tcommand = nextcommand
       call s_peeknextatom(func, lenfunc, nextcommand, nextnumber, nextorder)
    end do
  end subroutine s_parseblock



  recursive subroutine s_parsefn(func, lenfunc, funcstack, funcstackn, components)
    integer, intent(inout) :: lenfunc
    character(lenfunc), intent(inout) :: func
    type(cachefn), intent(inout) :: funcstack
    integer, intent(inout) :: funcstackn, components
    integer :: order, command
    real(fdp) :: number

    call s_getnextatom(func, lenfunc, command, number, order)
    
    if (command == plus_ .or. command == minus_) call s_funcpush(funcstack, funcstackn, number_, 0.0d0)

    do while(command /= endfunc_)
       call s_parseatom(func, lenfunc, funcstack, funcstackn, command, number, order, components)
       call s_getnextatom(func, lenfunc, command, number, order)
    end do
  end subroutine s_parsefn



  subroutine s_evaluate(funcstack, funcstackn, result)
    type(cachefn), intent(in) :: funcstack
    integer, intent(inout) :: funcstackn
    real(fdp), intent(out) :: result
    real(fdp), dimension(:), save, allocatable :: stack
    real(fdp), dimension(maxcommandcomps) :: tmp
    integer :: command, stackn, array, ndims, i
    integer, dimension(maxinds) :: inds
    real(fdp) :: number

    if (.not. allocated(stack)) then
       allocate(stack(0:size(funcstack%numbers)))
       stack = 0.0d0
    elseif (size(stack) < size(funcstack%numbers)+1) then
       deallocate(stack)
       allocate(stack(0:size(funcstack%numbers)))
       stack = 0.0d0
    end if

    command = nop_
    number = 0.0d0
    result = 0.0d0
    stackn = 0

    do while(command /= endfunc_)
       FUNCPOP(funcstack,funcstackn,command,number)

       select case(command)

       case(number_)
          PUSH(stack,stackn,result)
          result = number

       case(variablestart:variableend)
          PUSH(stack,stackn,result)
          result = varval(command-variablestart)

       case(arraystart:arrayend)
          array = command-arraystart
          ndims = arraydims(array)
          inds(ndims) = result
          
          do i = 1, ndims-1
             POP(stack,stackn,tmp(1))
             inds(i) = nint(tmp(1))
          end do

          result = arrayval(array, inds(:ndims), ndims)
             
       case(pow_, caret_)
          POP(stack,stackn,tmp(1))

          if (tmp(1) > 0.0d0) then
             result = tmp(1) ** result
          elseif (tmp(1) < 0.0d0) then
             if (aint(result) == result) then
                result = tmp(1) ** nint(result)
             else
                call seterror('x**y, x<0 and y not integer')
             end if
          elseif (result /= 0.0d0) then
             result = 0.0d0
          else             
             call seterror('Undefined result 0**0')
          end if
          
       case(neq_)
          POP(stack,stackn,tmp(1))
          result = merge(1.0d0, 0.0d0, tmp(1) /= result)

       case(eqeq_)
          POP(stack,stackn,tmp(1))
          result = merge(1.0d0, 0.0d0, tmp(1) == result)

       case(gteq_, eqgt_)
          POP(stack,stackn,tmp(1))
          result = merge(1.0d0, 0.0d0, tmp(1) >= result) 

       case(lteq_, eqlt_)
          POP(stack,stackn,tmp(1))
          result = merge(1.0d0, 0.0d0, tmp(1) <= result)

       case(gt_)
          POP(stack,stackn,tmp(1))
          result = merge(1.0d0, 0.0d0, tmp(1) > result)

       case(lt_)
          POP(stack,stackn,tmp(1))
          result = merge(1.0d0, 0.0d0, tmp(1) < result)

       case(mult_)
          POP(stack,stackn,tmp(1))
          result = tmp(1) * result

       case(div_)
          POP(stack,stackn,tmp(1))

          if (result /= 0.0d0) then
             result = tmp(1) / result
          else
             call seterror('x/y, y=0')
          end if

       case(plus_)
          POP(stack,stackn,tmp(1))
          result = tmp(1) + result

       case(minus_)
          POP(stack,stackn,tmp(1))
          result = tmp(1) - result

       case(cos_)
          result = cos(result)

       case(sin_)
          result = sin(result)

       case(tan_)
          result = tan(result)

       case(exp_)
          result = exp(result)

       case(log_)
          if (result > 0) then
             result = log(result)
          else
             call seterror('log(x), x<=0')
          end if

       case(log10_)
          if (result > 0) then
             result = log10(result)
          else
             call seterror('log10(x), x<=0')
          end if

       case(sqrt_)
          if (result >= 0) then
             result = sqrt(result)
          else
             call seterror('sqrt(x), x<0')
          end if

       case(cbrt_)
          result = f_cbrt(result)

       case(acos_)
          if (abs(result) <= 1) then
             result = acos(result)
          else
             call seterror('acos(x), |x|>1')
          end if

       case(asin_)
          if (abs(result) <= 1) then
             result = asin(result)
          else
             call seterror('asin(x), |x|>1')
          end if

       case(atan_)
          result = atan(result)

       case(cosh_)
          result = cosh(result)

       case(sinh_)
          result = sinh(result)

       case(tanh_)
          result = tanh(result)

       case(anint_)
          result = anint(result)

       case(aint_)
          result = aint(result)

       case(abs_)
          result = abs(result)

       case(delta_)
          result = merge(1.0d0, 0.0d0, result == 0.0d0)

       case(step_)
          result = merge(0.0d0, 1.0d0, result < 0.0d0)

       case(hat_)
          result = merge(1.0d0, 0.0d0, result <= 0.5d0 .and. result >= -0.5d0)

       case(max_)
          POP(stack,stackn,tmp(1))
          result = max(result, tmp(1))

       case(min_)
          POP(stack,stackn,tmp(1))
          result = min(result, tmp(1))

       case(besj0_)
          result = f_besj0(result)

       case(besj1_)
          result = f_besj1(result)

       case(besjn_)
          POP(stack,stackn,tmp(1))
          if (aint(tmp(1)) == tmp(1)) then
             result = f_besjn(tmp(1), result)
          else
             call seterror('besjn(n, x), n not integer')
          end if

       case(besy0_)
          result = f_besy0(result)

       case(besy1_)
          result = f_besy1(result)

       case(besyn_)
          POP(stack,stackn,tmp(1))
          if (aint(tmp(1)) == tmp(1) .and. result > 0.0d0) then
             result = f_besyn(tmp(1), result)
          else
             call seterror('besyn(n, x), n not integer or x/>0')
          end if
          
       case(logn_)
          POP(stack,stackn,tmp(1))

          if (result > 0.0d0 .and. tmp(1) > 0.0d0) then
             result = log(result)/log(tmp(1))
          else
             call seterror('logn(x, y), x/>0 or y/>0')
          end if

       case(erf_)
          result = f_erf(result)

       case(erfc_)
          result = 1.0d0 - f_erf(result)

       case(lgamma_)
          if (result > 0.0d0) then
             result = f_lgamma(result)
          else
             call seterror('lgamma(x), x<=0')
          end if

       case(gamma_)
          result = f_gamma(result)

       case(csch_)
          result = sinh(result)

          if (result == 0.0d0) then
             call seterror('1/sinh(x), sinh(x) = 0')
          else
             result = 1.0d0/result
          end if

       case(sech_)
          result = 1.0d0/cosh(result)

       case(coth_)
          result = tanh(result)

          if (result == 0.0d0) then
             call seterror('1/tanh(x), tanh(x) = 0')
          else
             result = 1.0d0/result
          end if

       case(merge_)
          POP(stack,stackn,tmp(1))
          POP(stack,stackn,tmp(2))

          result = merge(result, tmp(1), tmp(2) == 0)

       case(gauss_)
          result = exp(-result**2)

       case(sinc_)
          result = merge(1.0d0, sin(result)/result, result == 0.0d0)

       case(besi0_)
          result = f_besi0(result)

       case(besi1_)
          result = f_besi1(result)
          
       case(besin_)
          POP(stack,stackn,tmp(1))
          if (aint(tmp(1)) == tmp(1)) then
             result = f_besin(tmp(1), result)
          else
             call seterror('besin(n, x), n not integer')
          end if

       case(besk0_)
          result = f_besk0(result)
          
       case(besk1_)
          result = f_besk1(result)
          
       case(beskn_)
          POP(stack,stackn,tmp(1))
          if (aint(tmp(1)) == tmp(1)) then
             result = f_beskn(tmp(1), result)
          else
             call seterror('besjk(n, x), n not integer')
          end if

       case(ierfc_)
          if (abs(result-1.0d0) < 1.0d0) then
             result = f_ierfc(result)
          else
             call seterror('ierfc(x), 0 < x < 2')
          end if

       case(fresc_)
          result = f_fresc(result)

       case(fress_)
          result = f_fress(result)

       case(expi_)
          result = f_expi(result)

       case(sini_)
          result = f_sini(result)

       case(cosi_)
          result = f_cosi(result)

       case(logi_)
          if (result > 0.0d0) then
             result = f_logi(result)
          else
             call seterror('logi(x), x > 0')
          end if

       case(ierf_)
          if (abs(result) < 1.0d0) then
             result = f_ierf(result)
          else
             call seterror('ierf(x), -1 < x < 1')
          end if

       case(elle_)
          if (abs(result) < 1.0d0) then
             result = f_elle(result)
          else
             call seterror('elle(x), -1 < x < 1')
          end if

       case(ellk_)
          if (abs(result) < 1.0d0) then
             result = f_ellk(result)
          else
             call seterror('ellk(x), -1 < x < 1')
          end if

       case(ielle_)
          POP(stack,stackn,tmp(1))
          if (abs(tmp(1)) < 1.0d0 .and. abs(result) < pi2) then
             result = f_ielle(tmp(1), result)
          else
             call seterror('ielle(x, phi), -1 < x < 1, -pi/2 < phi < pi/2')
          end if

       case(iellf_)
          POP(stack,stackn,tmp(1))
          if (abs(tmp(1)) < 1.0d0 .and. abs(result) < pi2) then
             result = f_iellf(tmp(1), result)
          else
             call seterror('iellf(x, phi), -1 < x < 1, -pi/2 < phi < pi/2')
          end if

       end select
    end do

    if (stackn > 1) call seterror('Evaluation stack not empty')
  end subroutine s_evaluate



  subroutine createfn(func, funcnumber)
    type(str) :: func
    integer, intent(out) :: funcnumber
    type(cachefn), pointer :: funcstack
    type(cachefn), dimension(:), pointer :: tmpcachedfuncs
    integer :: funcstackn, i, lentmpfunc, components, n
    character(func%e-func%i+1) :: tmpfunc
    
    funcnumber = 0
    call s_firstrun()
       
    if (errorok) then
       n = size(cachedfuncs)
       cfnp = cfnp + 1

       if (cfnp > n) then
          allocate(tmpcachedfuncs(cfnp+allocsize))
          tmpcachedfuncs(:n) = cachedfuncs
          deallocate(cachedfuncs)
          cachedfuncs => tmpcachedfuncs

          do i = n+1, size(cachedfuncs)
             allocate(cachedfuncs(i)%commands(allocsize), cachedfuncs(i)%numbers(allocsize))
             cachedfuncs(i)%commands = endfunc_
             cachedfuncs(i)%numbers = 0.0d0
          end do
       end if

       funcnumber = functionstart+cfnp
       funcstack => cachedfuncs(cfnp)
       funcstackn = 0
       tmpfunc = linestr(func)
       tmpfunc = f_clean(tmpfunc)
       lentmpfunc = len_trim(tmpfunc)
       funcstackn = 0
       components = 0

       call s_parsefn(tmpfunc(:lentmpfunc), lentmpfunc, funcstack, funcstackn, components)
    end if
  end subroutine createfn



  subroutine destroyfn(funcnumber)
    integer, intent(in) :: funcnumber
    integer :: tmp

    call s_firstrun()
    tmp = funcnumber - functionstart

    if (tmp < 1 .or. tmp > size(cachedfuncs) .or. functionstart+tmp > functionend) then
       call seterror('Function does not exist')
    else if (associated(cachedfuncs(tmp)%commands)) then
       deallocate(cachedfuncs(tmp)%numbers, cachedfuncs(tmp)%commands)
       nullify(cachedfuncs(tmp)%numbers, cachedfuncs(tmp)%commands)
    else
       call seterror('Function does not exist')
    end if
  end subroutine destroyfn
  
  
  
  subroutine evaluatefn(funcnumber, number)
    integer, intent(in) :: funcnumber
    real(fdp), intent(out) :: number
    type(cachefn) :: funcstack
    integer :: funcstackn, tmp

    call s_firstrun()
    tmp = funcnumber - functionstart
    number = 0.0d0
    
    if (tmp > size(cachedfuncs) .or. tmp < 1 .or. functionstart+tmp > functionend) then
       write(*,*) funcnumber, tmp
       call seterror('Function does not exist')
    else if (associated(cachedfuncs(tmp)%commands)) then
       funcstack = cachedfuncs(tmp)
       funcstackn = 0

       call s_evaluate(funcstack, funcstackn, number)
    else
       call seterror('Function does not exist')
    end if
  end subroutine evaluatefn
  
end module interpreter
