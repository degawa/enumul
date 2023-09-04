module test_open_unitTests_form
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_form
    implicit none
    private
    public :: enum_open_form_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_form_assigns_enum_and_char_expr
    public :: open_form_default_returns_unformatted_for_direct_or_stream
    public :: open_form_default_returns_formatted_for_sequential_or_default
    public :: inquire_returns_default_char_expr_when_open_unit_wo_form_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_form_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_form%formatted%expr), "FORMATTED", &
                          "character expression of formatted should be 'FORMATTED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_form%unformatted%expr), "UNFORMATTED", &
                          "character expression of unformatted should be 'UNFORMATTED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_form%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_form_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_form_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_form) :: form

        form = open_form%unformatted

        call expect_equal(form%enum, open_form%unformatted%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(form%expr), trim(open_form%unformatted%expr), &
                          "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_form_assigns_enum_and_char_expr

    subroutine open_form_default_returns_unformatted_for_direct_or_stream(error)
        use :: enumul_open_access
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_form) :: default

        default = get_open_form_default(open_access%direct)
        call expect_equal(default%enum, open_form%unformatted%enum, &
                          "enum of return value of `get_open_form_default(direct)` should be &
                          &that of the `UNFORMATTED`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(open_form%unformatted%expr), &
                          "character expression of return value of `get_open_form_default(direct)` should be &
                          &that of the `UNFORMATTED`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_form_default(open_access%stream)
        call expect_equal(default%enum, open_form%unformatted%enum, &
                          "enum of return value of `get_open_form_default(stream)` should be &
                          &that of the `UNFORMATTED`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(open_form%unformatted%expr), &
                          "character expression of return value of `get_open_form_default(stream)` should be &
                          &that of the `UNFORMATTED`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_form_default_returns_unformatted_for_direct_or_stream

    subroutine open_form_default_returns_formatted_for_sequential_or_default(error)
        use :: enumul_open_access
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_form) :: default

        default = get_open_form_default()
        call expect_equal(default%enum, open_form%formatted%enum, &
                          "enum of return value of `get_open_form_default()` should be &
                          &that of the `FORMATTED`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(open_form%formatted%expr), &
                          "character expression of return value of `get_open_form_default()` should be &
                          &that of the `FORMATTED`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_form_default(open_access%sequential)
        call expect_equal(default%enum, open_form%formatted%enum, &
                          "enum of return value of `get_open_form_default(sequential)` should be &
                          &that of the `UNFORMATTED`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(open_form%formatted%expr), &
                          "character expression of return value of `get_open_form_default(sequential)` should be &
                          &that of the `UNFORMATTED`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_form_default_returns_formatted_for_sequential_or_default

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_form_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: form
        integer(int32) :: unit
        type(enum_open_form) :: default

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, form=form)
        close (unit)

        default = get_open_form_default()
        call expect_equal(trim(form), trim(default%expr), &
                          "`inquire` returns the default character expression when open an unit without form specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_form_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: form
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, form=form)

        call expect_equal(trim(form), trim(open_form%undefined%expr), &
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

        type(enum_open_form) :: x, y

        x = open_form%formatted
        y = optval(x, default=get_open_form_default())

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

        type(enum_open_form) :: y, default

        default = get_open_form_default()
        y = optval(default=default)

        call expect_equal(y%enum, default%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(default%expr), &
                          "character expression of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_form
