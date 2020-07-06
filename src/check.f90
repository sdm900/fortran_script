#include "macros.h"

program check
  use checkscrpt
  implicit none

  !
  ! Test 1
  !
  call checkscript("test1")

  !
  ! Test 2
  !
  call checkscript("test1", "OK")

  !
  ! Test 3
  !
  call checkscript("test3")

  !
  ! Test 4
  !
  call checkscript("test4")

  !
  ! Test 5
  !
  call checkscript("test5")

  !
  ! Test 6
  !
  call checkscript("test6", "6 No matching DO statement")

  !
  ! Test 7
  !
  call checkscript("test7")

  !
  ! Test 8
  !
  call checkscript("test8")

  !
  ! Test 9
  !
  call checkscript("test9")

  !
  ! Test 10
  !
  call checkscript("test10")

  !
  ! Test 11
  !
  call checkscript("test11")

  !
  ! Test 12
  !
  call checkscript("test12")

  !
  ! Test 13
  !
  call checkscript("test13")

  !
  ! Test 14
  !
  call checkscript("test14")

  !
  ! Test 15
  !
  call checkscript("test15")

  !
  ! Test 16
  !
  call checkscript("test16")

  !
  ! Test 17
  !
  call checkscript("test17", "1 Unknown text: i+1")

  !
  ! Test 18
  !
  call checkscript("test18")

  !
  ! Test 19
  !
  call checkscript("test19")

  !
  ! Test 20
  !
  call checkscript("test20")

  !
  ! Test 21
  !
  call checkscript("test21")

  !
  ! Test 22
  !
  call checkscript("test22")

  !
  ! Test 23
  !
  call checkscalar("test23", "a", 2.0d0)

  !
  ! Test 24
  !
  call checkarray("test24", "a", &
       (/ 4.20735492403948d0, -8.38246494596778d0, &
       -15.1360499061586d0, 9.09297426825682d0, &
       0.991588729324784d0, 93429.7577076496d0 /) )

  !
  ! Test 25
  !
  call checkscalars("test24", 3, "j i k")

  !
  ! Test 26
  !
  call checkarrays("test24", 2, "a b")

  !
  ! Test 27
  !
  call checkscript("test27")

  !
  ! Test 28
  !
  call checkscript("test28")

  !
  ! Test 29
  !
  call checkscript("test29")

end program check
