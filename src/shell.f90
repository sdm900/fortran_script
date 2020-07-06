program shell
  use fscript
  implicit none

  character(1000) :: error

  call interpret(err=error)
end program shell
