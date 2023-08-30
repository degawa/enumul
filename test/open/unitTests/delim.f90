module test_open_unitTests_delim
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_delim
    implicit none
    private
    public :: enum_open_delim_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_delim_assigns_enum_and_char_expr
    public :: default_open_delim_enum_is_none
    public :: inquire_returns_default_char_expr_when_open_unit_wo_delim_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_delim_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_delim%apostrophe%expr), "APOSTROPHE", &
                          "character expression of apostrophe should be 'APOSTROPHE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_delim%quote%expr), "QUOTE", &
                          "character expression of quote should be 'QUOTE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_delim%none%expr), "NONE", &
                          "character expression of none should be 'NONE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_delim%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_delim_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_delim_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_delim) :: delim

        delim = open_delim%apostrophe

        call expect_equal(delim%enum, open_delim%apostrophe%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(delim%expr), trim(open_delim%apostrophe%expr), &
                          "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_delim_assigns_enum_and_char_expr

    subroutine default_open_delim_enum_is_none(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_delim) :: default

        call expect_equal(default_open_delim%enum, open_delim%none%enum, &
                          "enum of default delim specifier shoud equal to that of the none", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default_open_delim%expr), trim(open_delim%none%expr), &
                          "character expression of the default delim specifier should equal to that of the none", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_delim_default()

        call expect_equal(default%enum, default_open_delim%enum, &
                          "enum of return value of `get_open_delim_default` should equal to that of `default_open_delim`", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(default%expr), trim(default_open_delim%expr), &
                         "character expression of return value of `get_open_delim_default` &
                         &should equal to that of `default_open_delim`", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_delim_enum_is_none

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_delim_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: delim
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, delim=delim)
        close (unit)

        call expect_equal(trim(delim), trim(default_open_delim%expr), &
                          "`inquire` returns the default character expression when open an unit without delim specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_delim_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: delim
        integer(int32) :: unit
        unit = get_newunit_number()
        inquire (unit, delim=delim)

        call expect_equal(trim(delim), trim(open_delim%undefined%expr), &
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

        type(enum_open_delim) :: x, y

        x = open_delim%quote
        y = optval(x, default=default_open_delim)

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

        type(enum_open_delim) :: y

        y = optval(default=default_open_delim)

        call expect_equal(y%enum, default_open_delim%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(default_open_delim%expr), &
                          "character expression of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_delim
