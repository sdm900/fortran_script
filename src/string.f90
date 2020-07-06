module string
  use programnodes

  implicit none

  type str
     integer :: i, e
  end type str

contains

  subroutine trimstr(s)
    type(str), intent(inout) :: s
    integer :: i

    if (s%i > s%e) return

    do i = s%i, s%e
       if (linechar(i) /= ' ') exit
    end do
    
    s%i = merge(i, s%e, i<=s%e)

    do i = s%e, s%i, -1
       if (linechar(i) /= ' ') exit
    end do

    s%e = merge(i, s%i, i >= s%i)
  end subroutine trimstr



  subroutine skipspace()
    integer :: i

    do i = linep(), lenline()
       if (nodes(progp)%line(i:i) /= ' ') exit
    end do

    nodes(progp)%linep = i
  end subroutine skipspace



  function nextchar()
    character :: nextchar
    integer :: i

    i = linep()
    nextchar = nodes(progp)%line(i:i)
  end function nextchar



  function getchar()
    character :: getchar
    integer :: i

    getchar = ''
    i = linep()

    if (i <= lenline()) then
       getchar = nodes(progp)%line(i:i)
       nodes(progp)%linep = i+1
    end if
  end function getchar



  function linechar(i)
    integer, intent(in) :: i
    character :: linechar

    linechar = ''
    if (i <= lenline()) linechar = nodes(progp)%line(i:i)
  end function linechar



  function linestr(s) result(l)
    type(str), intent(in) :: s
    character(s%e-s%i+1) :: l
    
    l = ''
    if (s%i>0 .and. s%e<= lenline()) l=nodes(progp)%line(s%i:s%e)
  end function linestr



  function strname(s)
    type(str), intent(in) :: s
    logical :: strname
    integer :: i

    strname = verify(nodes(progp)%line(s%i:s%e), namechars)==0
  end function strname

end module string
