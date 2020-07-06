#include "macros.h"



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
       number_= 10 , &
       bracket_= 11 , &
       nop_= 12 , &
       comma_= 13 , &
       endfunc_= 14 

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
       bracketstart=24, &
       roundbracket_=24, &
       bracketend=24 

  !  Expression definitions
  integer, parameter :: &
       expressionlength=8, &
       expressionstart=34, &
       anint_=34 , &
       log10_=35 , &
       sqrt_=36 , &
       acos_=37 , &
       asin_=38 , &
       atan_=39 , &
       cosh_=40 , &
       sinh_=41 , &
       tanh_=42 , &
       aint_=43 , &
       cos_=44 , &
       sin_=45 , &
       tan_=46 , &
       exp_=47 , &
       log_=48 , &
       abs_=49 , &
       delta_=50 , &
       step_=51 , &
       hat_=52 , &
       max_=53 , &
       min_=54 , &
       besj0_=55 , &
       besj1_=56 , &
       besjn_=57 , &
       besy0_=58 , &
       besy1_=59 , &
       besyn_=60 , &
       logn_=61 , &
       erf_=62 , &
       erfc_=63 , &
       lgamma_=64 , &
       gamma_=65 , &
       csch_=66 , &
       sech_=67 , &
       coth_=68 , &
       merge_=69 , &
       gauss_=70 , &
       sinc_=71 , &
       besi0_=72 , &
       besi1_=73 , &
       besk0_=74 , &
       besk1_=75 , &
       ierfc_=76 , &
       besin_=77 , &
       beskn_=78 , &
       cbrt_=79 , &
       fresc_=80 , &
       fress_=81 , &
       expi_=82 , &
       sini_=83 , &
       cosi_=84 , &
       logi_=85 , &
       ierf_=86 , &
       elle_=87 , &
       ellk_=88 , &
       ielle_=89 , &
       iellf_=90, &
       expressionend=90 

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
       operatorstart=100, &
       pow_=100 , &
       neq_=101 , &
       eqeq_=102 , &
       gteq_=103 , &
       eqgt_=104 , &
       lteq_=105 , &
       eqlt_=106 , &
       gt_=107 , &
       lt_=108 , &
       caret_=109 , &
       mult_=110 , &
       div_=111 , &
       plus_=112 , &
       minus_=113, &
       operatorend=113 

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
       variablestart=123 , &
       variableend=1123 , &
       arraystart=1133 , &
       arrayend=2133 , &
       subroutinestart=2143 , &
       subroutineend=3143 , &
       maxinds = 10

  ! Function definitions
  integer, parameter :: &
       functionstart = 3153 , &
       functionend = 53153 

  !  Commands definitions
  integer, parameter :: &
       commandstart = 53163, &
       do_ =53163 , &
       enddo_=53164 , &
       if_=53165 , &
       then_=53166 , &
       else_=53167 , &
       endif_=53168 , &
       end_=53169 , &
       print_=53170 , &
       exit_=53171 , &
       cycle_=53172 , &
       stop_=53173 , &
       help_=53174 , &
       elseif_=53175 , &
       open_=53176 , &
       close_=53177 , &
       call_=53178 , &
       subroutine_=53179 , &
       return_=53180, &
       commandend=53180 , &
       inlineif_=53190 , &
       endsubroutine_=53191 , &
       var_=53192 , &
       arr_=53193 

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
