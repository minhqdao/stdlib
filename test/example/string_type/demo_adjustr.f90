program demo_adjustr
  use stdlib_string_type
  implicit none
  type(string_type) :: string

  string = "Whitespace                            "
  string = adjustr(string)
! char(string) == "                            Whitespace"
end program demo_adjustr
