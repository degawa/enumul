program ex_openclose
    use, intrinsic :: iso_fortran_env
    use :: enumul_open
    use :: enumul_close
    implicit none

    integer(int32) :: unit
    open (newunit=unit, &
          action=open_action%write%expr, &
          form=open_form%formatted%expr, &
          decimal=open_decimal%comma%expr, &
          status=open_status%scratch%expr)
    close (unit, status=close_status%delete%expr)
end program ex_openclose
