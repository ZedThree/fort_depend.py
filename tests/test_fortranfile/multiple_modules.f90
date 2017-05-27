program progA
  use modA
  use modB
  use modC
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
