module modF
  use, intrinsic :: iso_c_binding
end module modF

module modG
  use modF
end module modG

module modH
  use modF
  use::modG
end module modH

program progA
  use modF
  use modG
  use modH
end program progA
