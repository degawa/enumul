module fassert_write_asynchronous
    use :: fassert_kit
    use :: enumul_write_asynchronous
    implicit none
    private
    public :: is_equal_enum_write_asynchronous
    public :: output_on_failure_enum_write_asynchronous_to_str

    character(*), parameter :: fmt = '('//fmt_indent//',A,A)'
    character(*), parameter :: type_mismatch_expected = "Type mismatch: `expected` is not enum_write_asynchronous"
    character(*), parameter :: type_mismatch_actual = "Type mismatch: `actual` is not enum_write_asynchronous"

contains
    pure logical function is_equal_enum_write_asynchronous(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_enum_write_asynchronous = .false.
        select type (actual); type is (enum_write_asynchronous)
            select type (expected); type is (enum_write_asynchronous)

                is_equal_enum_write_asynchronous = &
                    all([actual%enum == expected%enum, &
                         actual%expr == expected%expr])

            end select
        end select
    end function is_equal_enum_write_asynchronous

    pure subroutine output_on_failure_enum_write_asynchronous_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        select type (actual); type is (enum_write_asynchronous)
            select type (expected); type is (enum_write_asynchronous)

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
    end subroutine output_on_failure_enum_write_asynchronous_to_str
end module fassert_write_asynchronous

module test_write_unitTests_asynchronous
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: fassert_write_asynchronous
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_write_asynchronous
    implicit none
    private
    public :: enum_write_async_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_write_async_assigns_enum_and_char_expr
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_write_async_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(write_asynchronous%yes%expr), "YES", &
                          "character expression of yes should be 'YES'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(write_asynchronous%no%expr), "NO", &
                          "character expression of no should be 'NO'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_write_async_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_write_async_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_write_asynchronous) :: async

        async = write_asynchronous%no

        call expect_equal(async, write_asynchronous%no, &
                          "assigned enum should equal to rhs", &
                          comparator=is_equal_enum_write_asynchronous, &
                          verbose_message_writer=output_on_failure_enum_write_asynchronous_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_write_async_assigns_enum_and_char_expr

    subroutine optval_returns_x_when_x_is_presented(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_write_asynchronous) :: x, y

        x = write_asynchronous%yes
        y = optval(x, default=write_asynchronous%no)

        call expect_equal(y, x, &
                          "y should equal to x", &
                          comparator=is_equal_enum_write_asynchronous, &
                          verbose_message_writer=output_on_failure_enum_write_asynchronous_to_str, &
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

        type(enum_write_asynchronous) :: y, default

        default = write_asynchronous%no
        y = optval(default=default)

        call expect_equal(y, default, &
                          "y should equal to default", &
                          comparator=is_equal_enum_write_asynchronous, &
                          verbose_message_writer=output_on_failure_enum_write_asynchronous_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_write_unitTests_asynchronous
