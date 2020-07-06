module programnodes
  use globals

  implicit none

  type prognode
     logical :: evaluated
     integer :: stkp, linep, intp, lenline
     character(linelength) :: line
     integer, dimension(:), pointer :: ints
     character, dimension(:), pointer :: stk
  end type prognode

  integer :: nnodes
  type(prognode), dimension(:), pointer :: nodes

contains

  subroutine initnode(i)
    integer, intent(in) :: i
    
    nodes(i)%evaluated = .false.
    nodes(i)%stkp = 0
    nodes(i)%linep = 0
    nodes(i)%intp = 0
    nodes(i)%ints = udef_
    nodes(i)%line = 'Message from Stu: What the bloody hell is going on?'
    nodes(i)%lenline = 0
    nodes(i)%stk = blank_
  end subroutine initnode



  subroutine checknnodes()
    type(prognode), dimension(:), pointer :: tmpnodes
    integer :: i, n

    n = size(nodes)
    nnodes = max(nnodes, progp)

    if (progp < n) return

    allocate(tmpnodes(progp+allocsize))
    tmpnodes(:n) = nodes
    deallocate(nodes)
    nodes => tmpnodes
    
    do i = n+1, size(nodes)
       allocate(nodes(i)%ints(allocsize), nodes(i)%stk(allocsize))
       call initnode(i)
    end do
  end subroutine checknnodes



  function lenline()
    integer :: lenline

    lenline = nodes(progp)%lenline
  end function lenline



  function linep()
    integer :: linep

    linep = nodes(progp)%linep
  end function linep

end module programnodes
