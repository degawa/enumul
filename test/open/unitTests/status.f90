module test_open_unitTests_status
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_status
    implicit none
    private
    public :: enum_open_status_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_status_assigns_enum_and_char_expr
    public :: default_open_status_enum_is_unknown
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_status_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_status%old%expr), "OLD", &
                          "character expression of old should be 'OLD'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_status%new%expr), "NEW", &
                          "character expression of new should be 'NEW'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_status%scratch%expr), "SCRATCH", &
                          "character expression of scratch should be 'SCRATCH'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_status%replace%expr), "REPLACE", &
                          "character expression of replace should be 'REPLACE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_status%unknown%expr), "UNKNOWN", &
                          "character expression of unknown should be 'UNKNOWN'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_status_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_status_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_status) :: status

        status = open_status%replace

        call expect_equal(status%enum, open_status%replace%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(status%expr), trim(open_status%replace%expr), &
                          "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_status_assigns_enum_and_char_expr

    subroutine default_open_status_enum_is_unknown(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_status) :: default

        call expect_equal(default_open_status%enum, open_status%unknown%enum, &
                          "enum of default status specifier shoud equal to that of the unknown", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default_open_status%expr), trim(open_status%unknown%expr), &
                          "character expression of the default status specifier should equal to that of the unknown", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_status_default()

        call expect_equal(default%enum, default_open_status%enum, &
                          "enum of return value of `get_open_status_default` should equal to that of `default_open_status`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(default_open_status%expr), &
                         "character expression of return value of `get_open_status_default` &
                         &should equal to that of `default_open_status`", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_status_enum_is_unknown

    subroutine optval_returns_x_when_x_is_presented(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_status) :: x, y

        x = open_status%old
        y = optval(x, default=default_open_status)

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

        type(enum_open_status) :: y

        y = optval(default=default_open_status)

        call expect_equal(y%enum, default_open_status%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(default_open_status%expr), &
                          "character expression of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_status
