#include "macros.h"

module error
  use globals

  implicit none

  private :: perror
  
  logical, save :: errorok = .true.
  character(errorlength) :: perror = 'OK'

contains

  subroutine reseterror()
    perror = 'OK'
    errorok = .true.
  end subroutine reseterror



  subroutine seterror(terror)
    character(*), intent(in) :: terror

    if (errorok .and. LEN_TRIM(terror) > 0) then
       write(txtnum, '(i5)') progp
       perror = txtnum//' '//TRIM(terror)

       if (stdout /= -1) then
          write(stdout, *) TRIM(perror)
       else
          write(*,*) TRIM(perror)
       end if

       errorok = .false.
    end if
  end subroutine seterror



  subroutine geterror(terror)
    character(*), intent(out) :: terror

    terror = TRIM(perror)
  end subroutine geterror



  subroutine printerror()
    write(*,*) TRIM(perror)
  end subroutine printerror

end module error
