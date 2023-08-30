module test_open_unitTests_pad
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_pad
    implicit none
    private
    public :: enum_open_pad_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_pad_assigns_enum_and_char_expr
    public :: default_open_pad_enum_is_yes
    public :: inquire_returns_default_char_expr_when_open_unit_wo_pad_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: inquire_returns_undefined_when_conn_is_not_for_formatted
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_pad_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_pad%yes%expr), "YES", &
                          "character expression of yes should be 'YES'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_pad%no%expr), "NO", &
                          "character expression of no should be 'NO'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_pad%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_pad_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_pad_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_pad) :: pad

        pad = open_pad%no

        call expect_equal(pad%enum, open_pad%no%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(pad%expr), trim(open_pad%no%expr), &
                          "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_pad_assigns_enum_and_char_expr

    subroutine default_open_pad_enum_is_yes(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_pad) :: default

        call expect_equal(default_open_pad%enum, open_pad%yes%enum, &
                          "enum of default pad specifier shoud equal to that of the yes", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default_open_pad%expr), trim(open_pad%yes%expr), &
                          "character expression of the default pad specifier should equal to that of the yes", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_pad_default()

        call expect_equal(default%enum, default_open_pad%enum, &
                          "enum of return value of `get_open_pad_default` should equal to that of `default_open_pad`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(default_open_pad%expr), &
                         "character expression of return value of `get_open_pad_default` &
                         &should equal to that of `default_open_pad`", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_pad_enum_is_yes

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_pad_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: pad
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, pad=pad)
        close (unit)

        call expect_equal(trim(pad), trim(default_open_pad%expr), &
                          "`inquire` returns the default character expression when open an unit without pad specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_pad_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: pad
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, pad=pad)

        call expect_equal(trim(pad), trim(open_pad%undefined%expr), &
                          "`inquire` returns 'UNDEFINED' when there is no connection", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_undefined_when_there_is_no_connection

    subroutine inquire_returns_undefined_when_conn_is_not_for_formatted(error)
        use :: enumul_open_form
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: pad
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr, form=open_form%unformatted%expr)
        inquire (unit, pad=pad)
        close (unit)

        call expect_equal(trim(pad), trim(open_pad%undefined%expr), &
                          "`inquire` returns 'UNDEFINED' when the connection is not for formatted", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_undefined_when_conn_is_not_for_formatted

    subroutine optval_returns_x_when_x_is_presented(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_pad) :: x, y

        x = open_pad%no
        y = optval(x, default=default_open_pad)

        call expect_equal(y%enum, x%enum, &
                          "enum of y should equal to that of x", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(x%expr), &
                          "character expression of y should equal to that of x", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_x_when_x_is_presented

    subroutine optval_returns_default_when_x_is_not_presented(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_pad) :: y

        y = optval(default=default_open_pad)

        call expect_equal(y%enum, default_open_pad%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(default_open_pad%expr), &
                          "character expression of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_pad
