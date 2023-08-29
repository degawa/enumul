module test_open_unitTests_access
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_access
    implicit none
    private
    public :: enum_open_access_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_access_assigns_enum_and_char_expr
    public :: default_open_access_enum_is_sequential
    public :: inquire_returns_char_expr_when_open_unit_wo_access_spec
    public :: inqure_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_access_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_true(open_access%sequential%expr == "SEQUENTIAL", &
                         "character expression of sequential should be 'SEQUENTIAL'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(open_access%direct%expr == "DIRECT", &
                         "character expression of direct should be 'DIRECT'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(open_access%stream%expr == "STREAM", &
                         "character expression of stream should be 'STREAM'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(open_access%undefined%expr == "UNDEFINED", &
                         "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_access_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_access_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_access) :: access

        access = open_access%sequential

        call expect_equal(access%enum, open_access%sequential%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(access%expr == open_access%sequential%expr, &
                         "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_access_assigns_enum_and_char_expr

    subroutine default_open_access_enum_is_sequential(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_access) :: default

        call expect_equal(default_open_access%enum, open_access%sequential%enum, &
                          "enum of default access specifier shoud equal to that of the sequential", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(default_open_access%expr == open_access%sequential%expr, &
                         "character expression of the default access specifier should equal to that of the sequential", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_access_default()

        call expect_equal(default%enum, default_open_access%enum, &
                          "enum of return value of `get_open_access_default` should equal to that of `default_open_access`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(default%expr == default_open_access%expr, &
                         "character expression of return value of `get_open_access_default` &
                         &should equal to that of `default_open_access`", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_access_enum_is_sequential

    subroutine inquire_returns_char_expr_when_open_unit_wo_access_spec(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: access
        integer(int32) :: unit

        open (newunit=unit, status="scratch")
        inquire (unit, access=access)
        close (unit)

        call expect_true(access == default_open_access%expr, &
                         "`inquire` returns the default character expression when open an unit without access specifier", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_char_expr_when_open_unit_wo_access_spec

    subroutine inqure_returns_undefined_when_there_is_no_connection(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: access
        integer(int32) :: unit
        unit = -huge(unit)
        inquire (unit, access=access)

        call expect_true(access == open_access%undefined%expr, &
                         "`inquire` returns 'UNDEFINED' when there is no connection", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inqure_returns_undefined_when_there_is_no_connection

    subroutine optval_returns_x_when_x_is_presented(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_access) :: x, y

        x = open_access%stream
        y = optval(x, default=default_open_access)

        call expect_equal(y%enum, x%enum, &
                          "enum of y should equal to that of x", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(y%expr == x%expr, &
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

        type(enum_open_access) :: y

        y = optval(default=default_open_access)

        call expect_equal(y%enum, default_open_access%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(y%expr == default_open_access%expr, &
                         "character expression of y should equal to that of default", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_access
