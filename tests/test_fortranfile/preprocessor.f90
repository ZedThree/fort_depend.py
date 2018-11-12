program CPP
#ifdef FOO
  use foo
#else
!#ifdef BAR
  use bar
!#else
  use rawr
!#endif
#endif
end program CPP
