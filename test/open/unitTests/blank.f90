module test_open_unitTests_blank
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_blank
    implicit none
    private
    public :: enum_open_blank_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_blank_assigns_enum_and_char_expr
    public :: default_open_blank_enum_is_null
    public :: inquire_returns_default_char_expr_when_open_unit_wo_blank_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_blank_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_blank%null%expr), trim("NULL"), &
                          "character expression of null should be 'NULL'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_blank%zero%expr), trim("ZERO"), &
                          "character expression of zero should be 'ZERO'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_blank%undefined%expr), trim("UNDEFINED"), &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_blank_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_blank_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_blank) :: blank

        blank = open_blank%zero

        call expect_equal(blank%enum, open_blank%zero%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(blank%expr), trim(open_blank%zero%expr), &
                          "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_blank_assigns_enum_and_char_expr

    subroutine default_open_blank_enum_is_null(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_blank) :: default

        call expect_equal(default_open_blank%enum, open_blank%null%enum, &
                          "enum of default blank specifier shoud equal to that of the null", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default_open_blank%expr), trim(open_blank%null%expr), &
                          "character expression of the default blank specifier should equal to that of the null", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_blank_default()

        call expect_equal(default%enum, default_open_blank%enum, &
                          "enum of return value of `get_open_blank_default` should equal to that of `default_open_blank`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(default_open_blank%expr), &
                         "character expression of return value of `get_open_blank_default` &
                         &should equal to that of `default_open_blank`", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_blank_enum_is_null

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_blank_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: blank
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, blank=blank)
        close (unit)

        call expect_equal(trim(blank), trim(default_open_blank%expr), &
                          "`inquire` returns the default character expression when open an unit without blank specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_blank_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: blank
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, blank=blank)

        call expect_equal(trim(blank), trim(open_blank%undefined%expr), &
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

        type(enum_open_blank) :: x, y

        x = open_blank%zero
        y = optval(x, default=default_open_blank)

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

        type(enum_open_blank) :: y

        y = optval(default=default_open_blank)

        call expect_equal(y%enum, default_open_blank%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(default_open_blank%expr), &
                          "character expression of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_blank
