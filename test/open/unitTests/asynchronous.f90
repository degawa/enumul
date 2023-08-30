module test_open_unitTests_asynchronous
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_asynchronous
    implicit none
    private
    public :: enum_open_async_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_async_assigns_enum_and_char_expr
    public :: default_open_async_enum_is_no
    public :: inquire_returns_default_char_expr_when_open_unit_wo_async_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_async_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_asynchronous%yes%expr), "YES", &
                          "character expression of yes should be 'YES'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_asynchronous%no%expr), "NO", &
                          "character expression of no should be 'NO'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_asynchronous%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_async_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_async_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_asynchronous) :: async

        async = open_asynchronous%no

        call expect_equal(async%enum, open_asynchronous%no%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(async%expr), trim(open_asynchronous%no%expr), &
                          "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_async_assigns_enum_and_char_expr

    subroutine default_open_async_enum_is_no(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_asynchronous) :: default

        call expect_equal(default_open_asynchronous%enum, open_asynchronous%no%enum, &
                          "enum of default asynchronous specifier shoud equal to that of the no", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default_open_asynchronous%expr), trim(open_asynchronous%no%expr), &
                          "character expression of the default asynchronous specifier should equal to that of the no", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_asynchronous_default()

        call expect_equal(default%enum, default_open_asynchronous%enum, &
                          "enum of return value of `get_open_asynchronous_default` should &
                          &equal to that of `default_open_asynchronous`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(default_open_asynchronous%expr), &
                         "character expression of return value of `get_open_asynchronous_default` &
                         &should equal to that of `default_open_asynchronous`", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_async_enum_is_no

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_async_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: async
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, asynchronous=async)
        close (unit)

        call expect_equal(trim(async), trim(default_open_asynchronous%expr), &
                          "`inquire` returns the default character expression when open an unit without asynchronous specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_async_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: async
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, asynchronous=async)

        call expect_equal(trim(async), trim(open_asynchronous%undefined%expr), &
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

        type(enum_open_asynchronous) :: x, y

        x = open_asynchronous%yes
        y = optval(x, default=default_open_asynchronous)

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

        type(enum_open_asynchronous) :: y

        y = optval(default=default_open_asynchronous)

        call expect_equal(y%enum, default_open_asynchronous%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(default_open_asynchronous%expr), &
                          "character expression of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_asynchronous
