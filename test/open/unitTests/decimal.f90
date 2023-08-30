module test_open_unitTests_decimal
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_decimal
    implicit none
    private
    public :: enum_open_decimal_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_decimal_assigns_enum_and_char_expr
    public :: default_open_decimal_enum_is_point
    public :: inquire_returns_default_char_expr_when_open_wo_decimal_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_decimal_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_decimal%comma%expr), "COMMA", &
                          "character expression of comma should be 'COMMA'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_decimal%point%expr), "POINT", &
                          "character expression of point should be 'POINT'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_decimal%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_decimal_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_decimal_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_decimal) :: decimal

        decimal = open_decimal%comma

        call expect_equal(decimal%enum, open_decimal%comma%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(decimal%expr), trim(open_decimal%comma%expr), &
                          "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_decimal_assigns_enum_and_char_expr

    subroutine default_open_decimal_enum_is_point(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_decimal) :: default

        call expect_equal(default_open_decimal%enum, open_decimal%point%enum, &
                          "enum of default decimal specifier shoud equal to that of the point", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default_open_decimal%expr), trim(open_decimal%point%expr), &
                          "character expression of the default decimal specifier should equal to that of the point", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_decimal_default()

        call expect_equal(default%enum, default_open_decimal%enum, &
                          "enum of return value of `get_open_decimal_default` should equal to that of `default_open_decimal`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(default_open_decimal%expr), &
                         "character expression of return value of `get_open_decimal_default` &
                         &should equal to that of `default_open_decimal`", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_decimal_enum_is_point

    subroutine inquire_returns_default_char_expr_when_open_wo_decimal_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: decimal
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, decimal=decimal)
        close (unit)

        call expect_equal(trim(decimal), trim(default_open_decimal%expr), &
                          "`inquire` returns the default character expression when open an unit without decimal specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_wo_decimal_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: decimal
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, decimal=decimal)

        call expect_equal(trim(decimal), trim(open_decimal%undefined%expr), &
                          "`inquire` returns 'UNDEFINED' when there is no connection", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_undefined_when_there_is_no_connection

    subroutine optval_returns_x_when_x_is_presented(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_decimal) :: x, y

        x = open_decimal%comma
        y = optval(x, default=default_open_decimal)

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

        type(enum_open_decimal) :: y

        y = optval(default=default_open_decimal)

        call expect_equal(y%enum, default_open_decimal%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(default_open_decimal%expr), &
                          "character expression of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_decimal
