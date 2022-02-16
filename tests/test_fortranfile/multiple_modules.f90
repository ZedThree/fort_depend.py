program progA
  use modA
  use modB
  use modC
  use modD
end program progA

module modA
  use, intrinsic :: iso_c_binding
end module modA

module modB
  use modA
end module modB

module modC
  use modA
  use::modB
end module modC

module modD
contains
  subroutine foo
    character(40) :: xyz, module_short_code
    xyz = "this &
         & module_short_code"
    print*, &
         xyz, &
         module_short_code       ! this is picked up as a beginning
  end subroutine foo
end module modD
