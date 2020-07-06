#include "macros.h"
define(`__II',`10')
define(`__I',`ifelse($#,0,`__II',`__II define(`__II', eval(__II + $1))')')

module params
  implicit none

  !  Useful constants
  integer, parameter :: &
       fdp=8, &
       linelength=1024, &
       errorlength=1024, &
       atomlength=linelength, &
       varlistlength=4096, &
       udef_ = -1, &
       allocsize = 10

  real(fdp), parameter :: pi=3.141592653589793238d0, pi2 = pi/2.0d0, &
       pi4 = pi/4.0d0, eps = 1.0d-16, inf = huge(inf), &
       el=0.5772156649015329d0, sqrt2 = sqrt(2.0d0), zero=tiny(zero)

  character(len=*), parameter :: &
       operatorchars='+-/*^><=!', &
       numberchars='1234567890.', &
       exponentchars='ed', &
       lowerchars='abcdefghijklmnopqrstuvwxyz', &
       upperchars='ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
       bracketchars='()', &
       commachars=',', &
       underchars='_', &
       spacechars=' ', &
       quotechars='"'//"'", &
       commentchars='#', &
       namechars=lowerchars//upperchars//underchars, &
       allowedchars=operatorchars//numberchars//namechars//bracketchars//commachars

  !  Prompt characters
  character, dimension(4), parameter :: &
       prompt = (/ '>', '+', '-', '.' /)

  !  Controlling definitions
  integer, parameter :: &
       number_= __I(1), &
       bracket_= __I(1), &
       nop_= __I(1), &
       comma_= __I(1), &
       endfunc_= __I(10)

  character, parameter :: &
       command_ = 'a', &
       function_ = 'b', &
       functionstr_ = 'c', &
       variable_ = 'd', &
       variablestr_ = 'e', &
       string_ = 'f', &
       progp_ = 'g', &
       int_ = 'h', &
       blank_ = 'i'

  !  Bracket definitions
  integer, parameter :: &
       bracketstart=__I, &
       roundbracket_=__I, &
       bracketend=__I(10)

  !  Expression definitions
  integer, parameter :: &
       expressionlength=8, &
       expressionstart=__I, &
       anint_=__I(1), &
       log10_=__I(1), &
       sqrt_=__I(1), &
       acos_=__I(1), &
       asin_=__I(1), &
       atan_=__I(1), &
       cosh_=__I(1), &
       sinh_=__I(1), &
       tanh_=__I(1), &
       aint_=__I(1), &
       cos_=__I(1), &
       sin_=__I(1), &
       tan_=__I(1), &
       exp_=__I(1), &
       log_=__I(1), &
       abs_=__I(1), &
       delta_=__I(1), &
       step_=__I(1), &
       hat_=__I(1), &
       max_=__I(1), &
       min_=__I(1), &
       besj0_=__I(1), &
       besj1_=__I(1), &
       besjn_=__I(1), &
       besy0_=__I(1), &
       besy1_=__I(1), &
       besyn_=__I(1), &
       logn_=__I(1), &
       erf_=__I(1), &
       erfc_=__I(1), &
       lgamma_=__I(1), &
       gamma_=__I(1), &
       csch_=__I(1), &
       sech_=__I(1), &
       coth_=__I(1), &
       merge_=__I(1), &
       gauss_=__I(1), &
       sinc_=__I(1), &
       besi0_=__I(1), &
       besi1_=__I(1), &
       besk0_=__I(1), &
       besk1_=__I(1), &
       ierfc_=__I(1), &
       besin_=__I(1), &
       beskn_=__I(1), &
       cbrt_=__I(1), &
       fresc_=__I(1), &
       fress_=__I(1), &
       expi_=__I(1), &
       sini_=__I(1), &
       cosi_=__I(1), &
       logi_=__I(1), &
       ierf_=__I(1), &
       elle_=__I(1), &
       ellk_=__I(1), &
       ielle_=__I(1), &
       iellf_=__I, &
       expressionend=__I(10)

  character(len=expressionlength), dimension(expressionstart:expressionend), parameter :: &
       expressions=(/ &
       'anint(  ', &
       'log10(  ', &
       'sqrt(   ', &
       'acos(   ', &
       'asin(   ', &
       'atan(   ', &
       'cosh(   ', &
       'sinh(   ', &
       'tanh(   ', &
       'aint(   ', &
       'cos(    ', &
       'sin(    ', &
       'tan(    ', &
       'exp(    ', &
       'log(    ', &
       'abs(    ', &
       'delta(  ', &
       'step(   ', &
       'hat(    ', &
       'max(    ', &
       'min(    ', &
       'besj0(  ', &
       'besj1(  ', &
       'besjn(  ', &
       'besy0(  ', &
       'besy1(  ', &
       'besyn(  ', &
       'logn(   ', &
       'erf(    ', &
       'erfc(   ', &
       'lgamma( ', &
       'gamma(  ', &
       'csch(   ', &
       'sech(   ', &
       'coth(   ', &
       'merge(  ', &
       'gauss(  ', &
       'sinc(   ', &
       'besi0(  ', &
       'besi1(  ', &
       'besk0(  ', &
       'besk1(  ', &
       'ierfc(  ', &
       'besin(  ', &
       'beskn(  ', &
       'cbrt(   ', &
       'fresc(  ', &
       'fress(  ', &
       'expi(   ', &
       'sini(   ', &
       'cosi(   ', &
       'logi(   ', &
       'ierf(   ', &
       'elle(   ', &
       'ellk(   ', &
       'ielle(  ', &
       'iellf(  '  &
       /)
      

  integer, dimension(expressionstart:expressionend), parameter :: &
       expressionslen=len_trim(expressions), &
       commandcomps=(/ &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       2, &
       2, &
       1, &
       1, &
       2, &
       1, &
       1, &
       2, &
       2, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       3, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       2, &
       2, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       1, &
       2, &
       2 &
       /)
  integer, parameter :: maxcommandcomps=4

  !  Operators definitions
  integer, parameter :: &
       operatorlength=5, &
       operatorstart=__I, &
       pow_=__I(1), &
       neq_=__I(1), &
       eqeq_=__I(1), &
       gteq_=__I(1), &
       eqgt_=__I(1), &
       lteq_=__I(1), &
       eqlt_=__I(1), &
       gt_=__I(1), &
       lt_=__I(1), &
       caret_=__I(1), &
       mult_=__I(1), &
       div_=__I(1), &
       plus_=__I(1), &
       minus_=__I, &
       operatorend=__I(10)

  character(len=operatorlength), dimension(operatorstart:operatorend), parameter :: &
       operators=(/ &
       '**', &
       '!=', &
       '==', &
       '>=', &
       '=>', &
       '<=', &
       '=<', &
       '> ', &
       '< ', &
       '^ ', &
       '* ', &
       '/ ', &
       '+ ', &
       '- ' &
       /)

  integer, dimension(operatorstart:operatorend), parameter :: &
       operatorsorder=(/ &
       10000, &
       10, &
       10, &
       10, &
       10, &
       10, &
       10, &
       10, &
       10, &
       10000, &
       1000, &
       1000, &
       100, &
       100 &
       /), &
       operatorslen=len_trim(operators)

  !  Variable, array and indicie definitions
  integer, parameter :: &
       namelength=32, &
       variablestart=__I(1000), &
       variableend=__I(10), &
       arraystart=__I(1000), &
       arrayend=__I(10), &
       subroutinestart=__I(1000), &
       subroutineend=__I(10), &
       maxinds = 10

  ! Function definitions
  integer, parameter :: &
       functionstart = __I(50000), &
       functionend = __I(10)

  !  Commands definitions
  integer, parameter :: &
       commandstart = __I, &
       do_ =__I(1), &
       enddo_=__I(1), &
       if_=__I(1), &
       then_=__I(1), &
       else_=__I(1), &
       endif_=__I(1), &
       end_=__I(1), &
       print_=__I(1), &
       exit_=__I(1), &
       cycle_=__I(1), &
       stop_=__I(1), &
       help_=__I(1), &
       elseif_=__I(1), &
       open_=__I(1), &
       close_=__I(1), &
       call_=__I(1), &
       subroutine_=__I(1), &
       return_=__I, &
       commandend=__I(10), &
       inlineif_=__I(1), &
       endsubroutine_=__I(1), &
       var_=__I(1), &
       arr_=__I(10)

  character(atomlength), dimension(commandstart:commandend), parameter :: &
       commands = (/ &
       'do         ', &
       'enddo      ', &
       'if         ', &
       'then       ', &
       'else       ', &
       'endif      ', &
       'end        ', &
       'print      ', &
       'exit       ', &
       'cycle      ', &
       'stop       ', &
       'help       ', &
       'elseif     ', &
       'open       ', &
       'close      ', &
       'call       ', &
       'subroutine ', &
       'return     ' &
       /)

  integer, dimension(commandstart:commandend), parameter :: &
       lencommands = len_trim(commands)

end module params
