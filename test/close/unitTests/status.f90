module fassert_close_status
    use :: fassert_kit
    use :: enumul_close_status
    implicit none
    private
    public :: is_equal_enum_close_status
    public :: output_on_failure_enum_close_status_to_str

    character(*), parameter :: fmt = '('//fmt_indent//',A,A)'
    character(*), parameter :: type_mismatch_expected = "Type mismatch: `expected` is not enum_close_status"
    character(*), parameter :: type_mismatch_actual = "Type mismatch: `actual` is not enum_close_status"

contains
    pure logical function is_equal_enum_close_status(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_enum_close_status = .false.
        select type (actual); type is (enum_close_status)
            select type (expected); type is (enum_close_status)

                is_equal_enum_close_status = &
                    all([actual%enum == expected%enum, &
                         actual%expr == expected%expr])

            end select
        end select
    end function is_equal_enum_close_status

    pure subroutine output_on_failure_enum_close_status_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        select type (actual); type is (enum_close_status)
            select type (expected); type is (enum_close_status)

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
    end subroutine output_on_failure_enum_close_status_to_str
end module fassert_close_status

module test_close_unitTests_status
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: fassert_close_status
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_close_status
    implicit none
    private
    public :: enum_close_status_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_close_status_assigns_enum_and_char_expr
    public :: default_close_status_enum_is_keep
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_close_status_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(close_status%keep%expr), "KEEP", &
                          "character expression of keep should be 'KEEP'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(close_status%delete%expr), "DELETE", &
                          "character expression of delete should be 'DELETE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_close_status_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_close_status_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_close_status) :: status

        status = close_status%delete

        call expect_equal(status, close_status%delete, &
                          "assigned enum should equal to rhs", &
                          comparator=is_equal_enum_close_status, &
                          verbose_message_writer=output_on_failure_enum_close_status_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_close_status_assigns_enum_and_char_expr

    subroutine default_close_status_enum_is_keep(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_close_status) :: default

        call expect_equal(default_close_status, close_status%keep, &
                          "default status specifier shoud equal to the keep", &
                          comparator=is_equal_enum_close_status, &
                          verbose_message_writer=output_on_failure_enum_close_status_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_close_status_default()

        call expect_equal(default, default_close_status, &
                          "`get_close_status_default` should return `default_close_status`", &
                          comparator=is_equal_enum_close_status, &
                          verbose_message_writer=output_on_failure_enum_close_status_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_close_status_enum_is_keep

    subroutine optval_returns_x_when_x_is_presented(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_close_status) :: x, y

        x = close_status%delete
        y = optval(x, default=default_close_status)

        call expect_equal(y, x, &
                          "y should equal to x", &
                          comparator=is_equal_enum_close_status, &
                          verbose_message_writer=output_on_failure_enum_close_status_to_str, &
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

        type(enum_close_status) :: y

        y = optval(default=default_close_status)

        call expect_equal(y, default_close_status, &
                          "y should equal to default", &
                          comparator=is_equal_enum_close_status, &
                          verbose_message_writer=output_on_failure_enum_close_status_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_close_unitTests_status
