#include "macros.h"

module globals
  use params

  implicit none

  character(5) :: txtnum = ''
  integer :: progp = 0, stdin = -1, stdout = -1
  logical :: gotstop = .false.

end module globals
