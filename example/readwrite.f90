program ex_readwrite
    use, intrinsic :: iso_fortran_env
    use :: enumul_open
    use :: enumul_close
    use :: enumul_read
    use :: enumul_write
    implicit none

    integer(int32) :: unit
    character(:), allocatable :: char

    open (newunit=unit, file="sequential.tmp", &
          form=open_form%formatted%expr, &
          action=open_action%write%expr, &
          status=open_status%replace%expr)
    write (unit, '(A)', advance=write_advance%no%trim()) "🚀"
    write (unit, '(A)', advance=write_advance%yes%trim()) "🌏"
    close (unit, status=close_status%keep%expr)

    char = "🚀🌏" ! allocate an character string to determine the storage size
    char(:) = ""
    open (newunit=unit, file="sequential.tmp", &
          form=open_form%formatted%expr, &
          action=open_action%read%expr, &
          status=open_status%old%expr)

    read (unit, '(A)') char
    close (unit, status=close_status%delete%expr)

    print '(A)', char ! 🚀🌏
end program ex_readwrite
