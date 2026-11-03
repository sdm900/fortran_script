#include "macros.h"

module globals
  use params

  implicit none

  character(5) :: txtnum = ''
  integer :: progp = 0, stdin = -1, stdout = -1
  integer :: progp_bias = 0
  logical :: gotstop = .false.
  logical :: gotreturn = .false.

end module globals
