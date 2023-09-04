module fassert_open_sign
    use :: fassert_kit
    use :: enumul_open_sign
    implicit none
    private
    public :: is_equal_enum_open_sign
    public :: output_on_failure_enum_open_sign_to_str

    character(*), parameter :: fmt = '('//fmt_indent//',A,A)'
    character(*), parameter :: type_mismatch_expected = "Type mismatch: `expected` is not enum_open_sign"
    character(*), parameter :: type_mismatch_actual = "Type mismatch: `actual` is not enum_open_sign"

contains
    pure logical function is_equal_enum_open_sign(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_enum_open_sign = .false.
        select type (actual); type is (enum_open_sign)
            select type (expected); type is (enum_open_sign)

                is_equal_enum_open_sign = &
                    all([actual%enum == expected%enum, &
                         actual%expr == expected%expr])

            end select
        end select
    end function is_equal_enum_open_sign

    pure subroutine output_on_failure_enum_open_sign_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        select type (actual); type is (enum_open_sign)
            select type (expected); type is (enum_open_sign)

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
    end subroutine output_on_failure_enum_open_sign_to_str
end module fassert_open_sign

module test_open_unitTests_sign
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: fassert_open_sign
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_sign
    implicit none
    private
    public :: enum_open_sign_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_sign_assigns_enum_and_char_expr
    public :: default_open_sign_enum_is_processor_defined
    public :: inquire_returns_default_char_expr_when_open_unit_wo_sign_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_sign_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_sign%plus%expr), "PLUS", &
                          "character expression of plus should be 'PLUS'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_sign%suppress%expr), "SUPPRESS", &
                          "character expression of suppress should be 'SUPPRESS'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_sign%processor_defined%expr), "PROCESSOR_DEFINED", &
                          "character expression of processor_defined should be 'PROCESSOR_DEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_sign%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_sign_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_sign_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_sign) :: sign

        sign = open_sign%suppress

        call expect_equal(sign, open_sign%suppress, &
                          "assigned enum should equal to rhs", &
                          comparator=is_equal_enum_open_sign, &
                          verbose_message_writer=output_on_failure_enum_open_sign_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_sign_assigns_enum_and_char_expr

    subroutine default_open_sign_enum_is_processor_defined(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_sign) :: default

        call expect_equal(default_open_sign, open_sign%processor_defined, &
                          "default sign specifier shoud equal to the processor_defined", &
                          comparator=is_equal_enum_open_sign, &
                          verbose_message_writer=output_on_failure_enum_open_sign_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_sign_default()

        call expect_equal(default, default_open_sign, &
                          "`get_open_sign_default` should return `default_open_sign`", &
                          comparator=is_equal_enum_open_sign, &
                          verbose_message_writer=output_on_failure_enum_open_sign_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_sign_enum_is_processor_defined

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_sign_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: sign
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, sign=sign)
        close (unit)

        call expect_equal(trim(sign), trim(default_open_sign%expr), &
                          "`inquire` returns the default character expression when open an unit without sign specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_sign_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: sign
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, sign=sign)

        call expect_equal(trim(sign), trim(open_sign%undefined%expr), &
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

        type(enum_open_sign) :: x, y

        x = open_sign%plus
        y = optval(x, default=default_open_sign)

        call expect_equal(y, x, &
                          "y should equal to x", &
                          comparator=is_equal_enum_open_sign, &
                          verbose_message_writer=output_on_failure_enum_open_sign_to_str, &
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

        type(enum_open_sign) :: y

        y = optval(default=default_open_sign)

        call expect_equal(y, default_open_sign, &
                          "y should equal to default", &
                          comparator=is_equal_enum_open_sign, &
                          verbose_message_writer=output_on_failure_enum_open_sign_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_sign
