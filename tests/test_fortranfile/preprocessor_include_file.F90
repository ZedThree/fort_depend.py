program cpp_include
#include "some_include.inc"
#ifdef CAT
  use cat
#else
!#ifdef BAR
  use dog
!#else
  use goat
!#endif
#endif
end program cpp_include
