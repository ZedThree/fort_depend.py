module modB
  use modA
  implicit none
  integer :: bar
contains
  subroutine foo
    use modA, only : subA
  end subroutine foo
end module modB
