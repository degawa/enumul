module fassert_open_action
    use :: fassert_kit
    use :: enumul_open_action
    implicit none
    private
    public :: is_equal_enum_open_action
    public :: output_on_failure_enum_open_action_to_str

    character(*), parameter :: fmt = '('//fmt_indent//',A,A)'
    character(*), parameter :: type_mismatch_expected = "Type mismatch: `expected` is not enum_open_action"
    character(*), parameter :: type_mismatch_actual = "Type mismatch: `actual` is not enum_open_action"

contains
    pure logical function is_equal_enum_open_action(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_enum_open_action = .false.
        select type (actual); type is (enum_open_action)
            select type (expected); type is (enum_open_action)

                is_equal_enum_open_action = &
                    all([actual%enum == expected%enum, &
                         actual%expr == expected%expr])

            end select
        end select
    end function is_equal_enum_open_action

    pure subroutine output_on_failure_enum_open_action_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        select type (actual); type is (enum_open_action)
            select type (expected); type is (enum_open_action)

                write (buffer, fmt) "Expected: ", expected%expr
                call append(output_message, trim(buffer))
                write (buffer, fmt) "Actual  : ", actual%expr
                call append(output_message, trim(buffer))

            class default
                call append(output_message, type_mismatch_expected)
            end select
        class default
            call append(output_message, type_mismatch_actual)
        end select
    end subroutine output_on_failure_enum_open_action_to_str
end module fassert_open_action

module test_open_unitTests_action
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: fassert_open_action
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_action
    implicit none
    private
    public :: enum_open_action_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_action_assigns_enum_and_char_expr
    public :: open_action_default_returns_one_of_enum_defined_in_std
    public :: inquire_returns_default_char_expr_when_open_unit_wo_action_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_action_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_action%read%expr), "READ", &
                          "character expression of read should be 'READ'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_action%write%expr), "WRITE", &
                          "character expression of write should be 'WRITE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_action%readwrite%expr), "READWRITE", &
                          "character expression of readwrite should be 'READWRITE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_action%undefined%expr), "UNDEFINED", &
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
        type(enum_open_action) :: action

        action = open_action%readwrite

        call expect_equal(action, open_action%readwrite, &
                          "assigned enum should equal to rhs", &
                          comparator=is_equal_enum_open_action, &
                          verbose_message_writer=output_on_failure_enum_open_action_to_str, &
                          stat=stat, output_message=msg)
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

        call expect_true(any([is_equal_enum_open_action(default, open_action%read), &
                              is_equal_enum_open_action(default, open_action%write), &
                              is_equal_enum_open_action(default, open_action%readwrite)]), &
                         "`get_open_action_default` should retrun &
                         &one of the enum for those specified in the Fortran standard", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_action_default_returns_one_of_enum_defined_in_std

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_action_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: action
        integer(int32) :: unit
        type(enum_open_action) :: default

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, action=action)
        close (unit)

        default = get_open_action_default()
        call expect_equal(trim(action), trim(default%expr), &
                          "`inquire` returns the default character expression when open an unit without action specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_action_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: action
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, action=action)

        call expect_equal(trim(action), trim(open_action%undefined%expr), &
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

        type(enum_open_action) :: x, y

        x = open_action%write
        y = optval(x, default=get_open_action_default())

        call expect_equal(y, x, &
                          "y should equal to x", &
                          comparator=is_equal_enum_open_action, &
                          verbose_message_writer=output_on_failure_enum_open_action_to_str, &
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

        call expect_equal(y, default, &
                          "y should equal to default", &
                          comparator=is_equal_enum_open_action, &
                          verbose_message_writer=output_on_failure_enum_open_action_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_action
