module fassert_open_encoding
    use :: fassert_kit
    use :: enumul_open_encoding
    implicit none
    private
    public :: is_equal_enum_open_encoding
    public :: output_on_failure_enum_open_encoding_to_str

    character(*), parameter :: fmt = '('//fmt_indent//',A,A)'
    character(*), parameter :: type_mismatch_expected = "Type mismatch: `expected` is not enum_open_encoding"
    character(*), parameter :: type_mismatch_actual = "Type mismatch: `actual` is not enum_open_encoding"

contains
    pure logical function is_equal_enum_open_encoding(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_enum_open_encoding = .false.
        select type (actual); type is (enum_open_encoding)
            select type (expected); type is (enum_open_encoding)

                is_equal_enum_open_encoding = &
                    all([actual%enum == expected%enum, &
                         actual%expr == expected%expr])

            end select
        end select
    end function is_equal_enum_open_encoding

    pure subroutine output_on_failure_enum_open_encoding_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        select type (actual); type is (enum_open_encoding)
            select type (expected); type is (enum_open_encoding)

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
    end subroutine output_on_failure_enum_open_encoding_to_str
end module fassert_open_encoding

module test_open_unitTests_encoding
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: fassert_open_encoding
    use :: testdrive, only:error_type, check, skip_test
    use :: testdrive_util, only:occurred
    use :: enumul_open_encoding
    implicit none
    private
    public :: enum_open_encoding_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_encoding_assigns_enum_and_char_expr
    public :: open_encoding_default_returns_one_of_enum_defined_in_std
    public :: open_encoding_default_returns_UNDEFINED_for_unformatted
    public :: open_encoding_default_returns_UNKNOWN_for_formatted
    public :: inquire_returns_utf8_when_conn_is_for_formatted_w_utf8
    public :: inquire_returns_unknown_when_unable_to_determine_encoding
    public :: inquire_returns_undefined_when_conn_is_for_unformatted
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_encoding_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_encoding%utf8%expr), "UTF-8", &
                          "character expression of utf8 should be 'UTF-8'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_encoding%default%expr), "DEFAULT", &
                          "character expression of default should be 'DEFAULT'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_encoding%unknown%expr), "UNKNOWN", &
                          "character expression of unknown should be 'UNKNOWN'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_encoding%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_encoding_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_encoding_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_encoding) :: encoding

        encoding = open_encoding%utf8

        call expect_equal(encoding, open_encoding%utf8, &
                          "assigned enum should equal to rhs", &
                          comparator=is_equal_enum_open_encoding, &
                          verbose_message_writer=output_on_failure_enum_open_encoding_to_str, &
                          stat=stat, output_message=msg)
    end subroutine assignment_op_for_enum_open_encoding_assigns_enum_and_char_expr

    subroutine open_encoding_default_returns_one_of_enum_defined_in_std(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_encoding) :: default

        default = get_open_encoding_default()
        call expect_true(any([is_equal_enum_open_encoding(default, open_encoding%utf8), &
                              is_equal_enum_open_encoding(default, open_encoding%unknown), &
                              is_equal_enum_open_encoding(default, open_encoding%undefined)]), &
                             "`get_open_encoding_default` should retrun &
                             &one of the enum for those specified in the Fortran standard", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_encoding_default_returns_one_of_enum_defined_in_std

    subroutine open_encoding_default_returns_UNDEFINED_for_unformatted(error)
        use :: enumul_open_form
        use :: enumul_open_access
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_encoding) :: default

        ! unformatted
        default = get_open_encoding_default(form=open_form%unformatted)

        call expect_equal(default, open_encoding%undefined, &
                          "`get_open_encoding_default` for `UNFORMATTED` should return the `UNDEFINED`", &
                          comparator=is_equal_enum_open_encoding, &
                          verbose_message_writer=output_on_failure_enum_open_encoding_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! direct access
        default = get_open_encoding_default(access=open_access%direct)

        call expect_equal(default, open_encoding%undefined, &
                          "`get_open_encoding_default` for `DIRECT` access should return the `UNDEFINED`", &
                          comparator=is_equal_enum_open_encoding, &
                          verbose_message_writer=output_on_failure_enum_open_encoding_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! stream access
        default = get_open_encoding_default(access=open_access%stream)

        call expect_equal(default, open_encoding%undefined, &
                          "`get_open_encoding_default` for `STREAM` access should return the `UNDEFINED`", &
                          comparator=is_equal_enum_open_encoding, &
                          verbose_message_writer=output_on_failure_enum_open_encoding_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_encoding_default_returns_UNDEFINED_for_unformatted

    subroutine open_encoding_default_returns_UNKNOWN_for_formatted(error)
        use :: enumul_open_form
        use :: enumul_open_access
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_encoding) :: default

        ! unformatted
        default = get_open_encoding_default(form=open_form%formatted)

        call expect_equal(default, open_encoding%unknown, &
                          "`get_open_encoding_default` for `FORMATTED` should return the `UNKNOWN`", &
                          comparator=is_equal_enum_open_encoding, &
                          verbose_message_writer=output_on_failure_enum_open_encoding_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! sequential access
        default = get_open_encoding_default(access=open_access%sequential)

        call expect_equal(default, open_encoding%unknown, &
                          "`get_open_encoding_default` for `SEQUENTIAL` access should return the `UNKNOWN`", &
                          comparator=is_equal_enum_open_encoding, &
                          verbose_message_writer=output_on_failure_enum_open_encoding_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_encoding_default_returns_UNKNOWN_for_formatted

    subroutine inquire_returns_utf8_when_conn_is_for_formatted_w_utf8(error)
        use :: enumul_open_status
        use :: enumul_open_form
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: encoding
        integer(int32) :: unit, iostat

        open (newunit=unit, &
              status=open_status%scratch%expr, &
              form=open_form%formatted%expr, &
              encoding=open_encoding%utf8%expr, &
              iostat=iostat)
        if (iostat /= 0) then
            call skip_test(error, "test skipped due to unsupport of UTF-8")
            return
        end if

        inquire (unit, encoding=encoding)
        close (unit)

        call expect_equal(trim(encoding), trim(open_encoding%utf8%expr), &
                         "`inquire` returns 'UTF-8' when &
                         &the connection is for formatted I/O with an encoding form of UTF-8", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_utf8_when_conn_is_for_formatted_w_utf8

    subroutine inquire_returns_unknown_when_unable_to_determine_encoding(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: encoding
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, encoding=encoding)
        close (unit)
        call expect_equal(trim(encoding), trim(open_encoding%unknown%expr), &
                          "`inquire` returns 'UNKOWN' when there is no connection", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_unknown_when_unable_to_determine_encoding

    subroutine inquire_returns_undefined_when_conn_is_for_unformatted(error)
        use :: enumul_open_status
        use :: enumul_open_form
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: encoding
        integer(int32) :: unit
        open (newunit=unit, status=open_status%scratch%expr, form=open_form%unformatted%expr)
        inquire (unit, encoding=encoding)
        close (unit)

        call expect_equal(trim(encoding), trim(open_encoding%undefined%expr), &
                         "`inquire` returns 'UNDEFINED' when &
                         &the connection is for unformatted I/O", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_undefined_when_conn_is_for_unformatted

    subroutine optval_returns_x_when_x_is_presented(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_encoding) :: x, y

        x = open_encoding%utf8
        y = optval(x, default=get_open_encoding_default())

        call expect_equal(y, x, &
                          "y should equal to x", &
                          comparator=is_equal_enum_open_encoding, &
                          verbose_message_writer=output_on_failure_enum_open_encoding_to_str, &
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

        type(enum_open_encoding) :: y, default

        default = get_open_encoding_default()
        y = optval(default=default)

        call expect_equal(y, default, &
                          "y should equal to default", &
                          comparator=is_equal_enum_open_encoding, &
                          verbose_message_writer=output_on_failure_enum_open_encoding_to_str, &
                          stat=stat, output_message=msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_encoding
