module test_open_unitTests_action
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_action
    implicit none
    private
    public :: enum_open_action_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_action_assigns_enum_and_char_expr
    public :: open_action_default_returns_one_of_enum_defined_in_std
    public :: inquire_returns_char_expr_when_open_unit_wo_action_spec
    public :: inqure_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_action_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_true(open_action%read%expr == "READ", &
                         "character expression of read should be 'READ'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(open_action%write%expr == "WRITE", &
                         "character expression of write should be 'WRITE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(open_action%readwrite%expr == "READWRITE", &
                         "character expression of readwrite should be 'READWRITE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(open_action%undefined%expr == "UNDEFINED", &
                         "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_action_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_action_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_action) :: access

        access = open_action%readwrite

        call expect_equal(access%enum, open_action%readwrite%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(access%expr == open_action%readwrite%expr, &
                         "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_action_assigns_enum_and_char_expr

    subroutine open_action_default_returns_one_of_enum_defined_in_std(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_action) :: default

        default = get_open_action_default()

        call expect_true(any([default%enum == open_action%read%enum, &
                              default%enum == open_action%write%enum, &
                              default%enum == open_action%readwrite%enum]), &
                         "enum of return value of `get_open_action_default` should be &
                         &one of the enum having expr in the Fortran standard", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(any([default%expr == open_action%read%expr, &
                              default%expr == open_action%write%expr, &
                              default%expr == open_action%readwrite%expr]), &
                         "character expression of return value of `get_open_action_default` &
                         &should be one of the enum having expr in the Fortran standard", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_action_default_returns_one_of_enum_defined_in_std

    subroutine inquire_returns_char_expr_when_open_unit_wo_action_spec(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: action
        integer(int32) :: unit
        type(enum_open_action) :: default

        open (newunit=unit, status="scratch")
        inquire (unit, action=action)
        close (unit)

        default = get_open_action_default()
        call expect_true(action == default%expr, &
                         "`inquire` returns the default character expression when open an unit without action specifier", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_char_expr_when_open_unit_wo_action_spec

    subroutine inqure_returns_undefined_when_there_is_no_connection(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: action
        integer(int32) :: unit
        unit = -huge(unit)
        inquire (unit, action=action)

        call expect_true(action == open_action%undefined%expr, &
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

        type(enum_open_action) :: x, y

        x = open_action%write
        y = optval(x, default=get_open_action_default())

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

        type(enum_open_action) :: y, default

        default = get_open_action_default()
        y = optval(default=default)

        call expect_equal(y%enum, default%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(y%expr == default%expr, &
                         "character expression of y should equal to that of default", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_action
