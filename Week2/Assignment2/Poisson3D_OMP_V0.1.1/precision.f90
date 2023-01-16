module precision

  implicit none

  integer, parameter :: sp = selected_real_kind(6,30)
  integer, parameter :: dp = selected_real_kind(14,100)

  integer, parameter :: lp = selected_int_kind(18)

end module precision
