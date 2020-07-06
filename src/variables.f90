#include "macros.h"

module variables
  use error
  use string

  implicit none

  private :: array_, namespace_, sub_, nss, ns, nns, lb, ub, strd, varvals, arrvals, nvars, narrs, &
       subs, nsubs

  type array_
     integer :: ndims
     integer, dimension(:), pointer :: lb, ub, strd
     real(fdp), dimension(:), pointer :: vals
     logical, dimension(:), pointer :: mask
  end type array_

  type namespace_
     integer :: nvars, narrs, nvarinds, narrinds
     integer, dimension(:), pointer :: vars, arrs, lenvarnames, lenarrnames
     character(namelength), dimension(:), pointer :: varnames, arrnames
  end type namespace_

  type sub_
     integer :: progp, ninds
     character(namelength) :: name
     integer :: lenname
  end type sub_

  ! Name spaces
  integer :: nns
  type(namespace_), dimension(:), pointer :: nss
  type(namespace_), pointer :: ns

  ! Arrays and variables
  integer :: nvars, narrs
  real(fdp), dimension(:), pointer :: varvals
  type(array_), dimension(:), pointer :: arrvals

  ! Subroutines
  type(sub_), dimension(:), pointer :: subs
  integer :: nsubs

  ! Temp variables declared once for performance
  integer, dimension(maxinds) :: lb, ub, strd

contains

  !
  ! Namespace routines
  !



  subroutine resetnamespace()
    ns%nvars = 0
    ns%narrs = 0
    ns%nvarinds = 0
    ns%narrinds = 0
    ns%vars = udef_
    ns%arrs = udef_
    ns%varnames = ''
    ns%arrnames = ''
    ns%lenvarnames = 0
    ns%lenarrnames = 0
  end subroutine resetnamespace



  subroutine initnamespace()
    allocate(ns%vars(allocsize), ns%arrs(allocsize), ns%lenvarnames(allocsize), &
         ns%lenarrnames(allocsize), ns%varnames(allocsize), ns%arrnames(allocsize))

    call resetnamespace()
  end subroutine initnamespace



  subroutine createnamespace()
    type(namespace_), dimension(:), pointer :: tnss
    integer  :: tnns

    tnns = nns
    nns = nns + 1
    
    if (nns > size(nss)) then
       allocate(tnss(nns+allocsize))
       tnss(:tnns) = nss
       deallocate(nss)
       nss => tnss
    end if

    ns => nss(nns)
    call initnamespace()
  end subroutine createnamespace



  subroutine destroynamespace()
    deallocate(ns%vars, ns%arrs, ns%lenvarnames, ns%lenarrnames, ns%varnames, ns%arrnames)
    nns = nns -1
    ns => nss(nns)
  end subroutine destroynamespace


 
  subroutine initnamespaces()
    allocate(nss(allocsize))
    nns = 0
    call createnamespace()
  end subroutine initnamespaces



  !
  ! Subroutine routines
  !



  subroutine initsubs()
    allocate(subs(allocsize))
    nsubs = 0
  end subroutine initsubs

  

  subroutine registersub(s, ninds)
    type(str), intent(in) :: s
    integer, intent(in) :: ninds
    type(sub_), dimension(:), pointer :: tsubs
    integer :: tnsubs

    tnsubs = nsubs
    nsubs = nsubs + 1

    if (nsubs > size(subs)) then
       allocate(tsubs(nsubs+allocsize))
       tsubs(:tnsubs) = subs
       deallocate(subs)
       subs => tsubs
    end if

    subs(nsubs)%progp = progp
    subs(nsubs)%ninds = ninds
    subs(nsubs)%name = linestr(s)
    subs(nsubs)%lenname = s%e-s%i+1
  end subroutine registersub



  subroutine clearsubs()
    nsubs = 0
  end subroutine clearsubs



  function subroutinenum(s)
    type(str), intent(in) :: s
    integer :: i, subroutinenum

    subroutinenum = 0

    do i = 1, nsubs
       if (subs(i)%name(:subs(i)%lenname) == linestr(s)) then
          subroutinenum = i
          exit
       end if
    end do
  end function subroutinenum



  !
  ! Varstack routines
  !



  subroutine resetvarstack()
    integer :: i, var, arr

    nvars = 0
    narrs = 0

    do i = 1, ns%nvarinds
       ns%varnames(i) = ''
       ns%lenvarnames(i) = 0
    end do
       
    do i = ns%nvarinds+1, ns%nvars
       varvals(ns%vars(i)) = 0.0d0
       ns%varnames(i) = ''
       ns%lenvarnames(i) = 0
    end do

    do i = 1, ns%narrinds
       ns%arrnames(i) = ''
       ns%lenarrnames(i) = 0
    end do

    do i = ns%narrinds+1, ns%narrs
       arr = ns%arrs(i)
       arrvals(arr)%ndims = 0
       if (associated(arrvals(arr)%lb)) deallocate(arrvals(arr)%lb, arrvals(arr)%ub, &
            arrvals(arr)%strd, arrvals(arr)%vals, arrvals(arr)%mask)
       ns%arrnames(i) = ''
       ns%lenarrnames(i) = 0
    end do

    ns%nvars = 0
    ns%narrs = 0
    ns%nvarinds = 0
    ns%narrinds = 0
  end subroutine resetvarstack



  subroutine initvarstack()
    integer :: i

    allocate(varvals(allocsize), arrvals(allocsize))

    do i = 1, size(arrvals)
       arrvals(i)%ndims = 0
       nullify(arrvals(i)%vals, arrvals(i)%ub, arrvals(i)%lb, arrvals(i)%strd, arrvals(i)%mask)
    end do

    call resetvarstack()
  end subroutine initvarstack



  !
  ! Variable routines
  !



  function varnum(s)
    type(str), intent(in) :: s
    integer :: i, varnum

    varnum = 0

    do i = 1, ns%nvars
       if (ns%varnames(i)(:ns%lenvarnames(i)) == linestr(s)) then
          varnum = i
          exit
       end if
    end do
  end function varnum



  function varstrnum(s, l)
    integer, intent(in) :: l
    character(l), intent(in) :: s
    integer :: i, varstrnum

    varstrnum = 0

    do i = 1, ns%nvars
       if (ns%varnames(i)(:ns%lenvarnames(i)) == s(:l)) then
          varstrnum = i
          exit
       end if
    end do
  end function varstrnum



  function numvars()
    integer :: numvars

    numvars = ns%nvars
  end function numvars



  function var(i)
    integer, intent(in) :: i
    character(namelength) :: var

    var = ns%varnames(i)(:ns%lenvarnames(i))
  end function var



  function lenvar(i)
    integer, intent(in) :: i
    integer :: lenvar

    lenvar = ns%lenvarnames(i)
  end function lenvar



  subroutine createvar(var)
    type(str), intent(in) :: var
    integer :: nvarvals, nnsvars
    real(fdp), dimension(:), pointer :: tvarvals
    character(namelength), dimension(:), pointer :: tvarnames
    integer, dimension(:), pointer :: tlenvarnames, tvars

    if (varnum(var) /= 0) return
    
    ns%nvars = ns%nvars + 1
    nvars = nvars + 1
    nvarvals = size(varvals)
    nnsvars = size(ns%vars)
    
    
    if (nvars > nvarvals) then
       allocate(tvarvals(nvarvals+allocsize))
       tvarvals(:nvarvals) = varvals
       deallocate(varvals)
       varvals => tvarvals
    end if
    
    if (ns%nvars > nnsvars) then
       allocate(tvars(nnsvars+allocsize), tvarnames(nnsvars+allocsize), tlenvarnames(nnsvars+allocsize))
       tvars(:nnsvars) = ns%vars
       tvarnames(:nnsvars) = ns%varnames
       tlenvarnames(:nnsvars) = ns%lenvarnames
       deallocate(ns%vars, ns%varnames, ns%lenvarnames)
       ns%vars => tvars
       ns%varnames => tvarnames
       ns%lenvarnames => tlenvarnames
    end if
    
    ns%vars(ns%nvars) = nvars
    ns%varnames(ns%nvars) = linestr(var)
    ns%lenvarnames(ns%nvars) = var%e-var%i+1
    varvals(nvars) = 0.0d0
  end subroutine createvar



  subroutine setvarval(var, val)
    integer, intent(in) :: var
    real(fdp), intent(in) :: val
    integer :: v

    v = ns%vars(var)
    varvals(v) = val
  end subroutine setvarval



  function varval(var)
    integer, intent(in) :: var
    real(fdp) :: varval
    integer :: v

    v = ns%vars(var)
    varval = varvals(v)
  end function varval



  !
  ! Array routines
  ! 



  function arraynum(array)
    type(str), intent(in) :: array
    integer :: i, arraynum

    arraynum = 0

    do i = 1, ns%narrs
       if (ns%arrnames(i)(:ns%lenarrnames(i)) == linestr(array)) then
          arraynum = i
          exit
       end if
    end do
  end function arraynum



  function arrstrnum(s, l)
    integer, intent(in) :: l
    character(l) :: s
    integer :: i, arrstrnum

    arrstrnum = 0

    do i = 1, ns%narrs
       if (ns%arrnames(i)(:ns%lenarrnames(i)) == s(:l)) then
          arrstrnum = i
          exit
       end if
    end do
  end function arrstrnum



  function numarrays()
    integer :: numarrays

    numarrays = ns%narrs
  end function numarrays



  function arr(i)
    integer, intent(in) :: i
    character(namelength) :: arr

    arr = ns%arrnames(i)(:ns%lenarrnames(i))
  end function arr



  function lenarray(i)
    integer, intent(in) :: i
    integer :: lenarray

    lenarray = ns%lenarrnames(i)
  end function lenarray
     


  function arraydims(array)
    integer, intent(in) :: array
    integer :: arraydims, a
    
    a = ns%arrs(array)
    arraydims = arrvals(a)%ndims
  end function arraydims



  function arraysize(array)
    integer, intent(in) :: array
    integer :: arraysize, a

    a = ns%arrs(array)
    arraysize = product(arrvals(a)%ub-arrvals(a)%lb+1)
  end function arraysize



  subroutine createarr(array, ndims)
    integer, intent(in) :: ndims
    type(str), intent(in) :: array
    integer :: i, narrvals, nnsarrs
    type(array_), dimension(:), pointer :: tarrvals
    character(namelength), dimension(:), pointer :: tarrnames
    integer, dimension(:), pointer :: tlenarrnames, tarrs

    if (ndims > maxinds) then
       write(txtnum, '(i5)') maxinds
       call seterror('Maximum number of array dimensions is '//txtnum)
       return
    end if

    if (arraynum(array) /= 0) return

    ns%narrs = ns%narrs + 1
    narrs = narrs + 1
    narrvals = size(arrvals)
    nnsarrs = size(ns%arrs)
    
    if (narrs > narrvals) then
       allocate(tarrvals(narrvals+allocsize))
       tarrvals(:narrvals) = arrvals
       deallocate(arrvals)
       arrvals => tarrvals
    end if
    
    if (ns%narrs > nnsarrs) then
       allocate(tarrs(nnsarrs+allocsize), tarrnames(nnsarrs+allocsize), tlenarrnames(nnsarrs+allocsize))
       tarrs(:nnsarrs) = ns%arrs
       tarrnames(:nnsarrs) = ns%arrnames
       tlenarrnames(:nnsarrs) = ns%lenarrnames
       deallocate(ns%arrs, ns%arrnames, ns%lenarrnames)
       ns%arrs => tarrs
       ns%arrnames => tarrnames
       ns%lenarrnames => tlenarrnames
    end if
    
    ns%arrs(ns%narrs) = narrs
    ns%arrnames(ns%narrs) = linestr(array)
    ns%lenarrnames(ns%narrs) = array%e-array%i+1
    arrvals(narrs)%ndims = ndims
    i = allocsize+2**ndims
    
    allocate(arrvals(narrs)%vals(i), arrvals(narrs)%mask(i), arrvals(narrs)%lb(ndims), &
         arrvals(narrs)%ub(ndims), arrvals(narrs)%strd(ndims))

    arrvals(narrs)%vals = 0.0d0
    arrvals(narrs)%lb = 1
    arrvals(narrs)%ub = 1
    arrvals(narrs)%strd = 1
  end subroutine createarr



  subroutine setarrayval(array, inds, ndims, val)
    integer, intent(in) :: array, ndims
    integer, dimension(ndims), intent(in) :: inds
    real(fdp), intent(in) :: val
    integer :: i, j, isize, nsize, a
    real(fdp) :: tmp
    real(fdp), dimension(:), pointer :: v
    logical, dimension(:), pointer :: m

    a = ns%arrs(array)

    if (any(inds<arrvals(a)%lb .or. inds>arrvals(a)%ub)) then
       isize = product(arrvals(a)%ub-arrvals(a)%lb+1)
       lb(:ndims) = min(inds, arrvals(a)%lb)
       ub(:ndims) = max(inds, arrvals(a)%ub)
       nsize = product(ub(:ndims)-lb(:ndims)+1)
       strd(1) = 1
       
       do i=2, ndims
          strd(i) = strd(i-1)*(ub(i-1)-lb(i-1)+1)
       end do
       
       if (nsize > size(arrvals(a)%vals)) then
          nsize = product(ub(:ndims)-lb(:ndims)+3)
          allocate(v(nsize), m(nsize))
          v(:isize) = arrvals(a)%vals(:isize)
          m(:isize) = arrvals(a)%mask(:isize)
          v(isize+1:) = 0.0d0
          m(isize+1:) = .true.
          deallocate(arrvals(a)%vals, arrvals(a)%mask)
          arrvals(a)%vals => v
          arrvals(a)%mask => m
       end if
       
       do i = isize, 1, -1
          j = 1+sum(strd(:ndims)*(modulo((i-1)/arrvals(a)%strd, &
               arrvals(a)%ub-arrvals(a)%lb+1)-lb(:ndims)+1))
          tmp = arrvals(a)%vals(i)
          arrvals(a)%vals(i) = 0.0d0
          arrvals(a)%mask(i) = .false.
          arrvals(a)%vals(j) = tmp
          arrvals(a)%mask(j) = .true.
       end do
       
       arrvals(a)%lb = lb(:ndims)
       arrvals(a)%ub = ub(:ndims)
       arrvals(a)%strd = strd(:ndims)
    end if
    
    j = 1+sum((inds-arrvals(a)%lb)*arrvals(a)%strd)
    arrvals(a)%vals(j) = val
    arrvals(a)%mask(j) = .true.
  end subroutine setarrayval



  function arrayval(array, inds, ndims)
    integer, intent(in) :: array, ndims
    integer, dimension(ndims), intent(in) :: inds
    character(5) :: txtnum
    real(fdp) :: arrayval
    integer :: j, a

    a = ns%arrs(array)

    if (any(arrvals(a)%lb>inds .or. inds>arrvals(a)%ub)) then
       do j = 1, ndims
          if (arrvals(a)%lb(j)>inds(j) .or. inds(j)>arrvals(a)%ub(j)) then
             write(txtnum, '(i5)') j
             exit
          end if
       end do
       
       call seterror('Array out of bounds for index'//txtnum)
       arrayval = 0.0d0
       return
    end if

    j = 1+sum((inds-arrvals(a)%lb)*arrvals(a)%strd)

    if (.not. arrvals(a)%mask(j)) then
       call seterror('Array value used before it is initialised')
       arrayval = 0.0d0
       return
    end if

    arrayval = arrvals(a)%vals(j)
  end function arrayval



  subroutine getarraylb(array, lb, n)
    integer, intent(in) :: array
    integer, intent(in) :: n
    integer, dimension(n) :: lb
    integer :: a

    a = ns%arrs(array)
    lb = arrvals(a)%lb
  end subroutine getarraylb



  subroutine getarrayub(array, ub, n)
    integer, intent(in) :: array
    integer, intent(in) :: n
    integer, dimension(n) :: ub
    integer :: a

    a = ns%arrs(array)
    ub = arrvals(a)%ub
  end subroutine getarrayub



  subroutine getarray(array, val, n)
    integer, intent(in) :: array
    integer, intent(in) :: n
    real(fdp), dimension(n) :: val
    integer :: a

    a = ns%arrs(array)
    val = arrvals(a)%vals(:n)
  end subroutine getarray



  subroutine checkarray(array, ndims)
    integer, intent(in) :: array, ndims
    integer :: a

    a = ns%arrs(array)
    if (ndims /= arrvals(a)%ndims) call seterror('Incorrect number of array dimensions')
  end subroutine checkarray

end module variables
