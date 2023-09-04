module fassert_open_form
    use :: fassert_kit
    use :: enumul_open_form
    implicit none
    private
    public :: is_equal_enum_open_form
    public :: output_on_failure_enum_open_form_to_str

    character(*), parameter :: fmt = '('//fmt_indent//',A,A)'
    character(*), parameter :: type_mismatch_expected = "Type mismatch: `expected` is not enum_open_form"
    character(*), parameter :: type_mismatch_actual = "Type mismatch: `actual` is not enum_open_form"

contains
    pure logical function is_equal_enum_open_form(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_enum_open_form = .false.
        select type (actual); type is (enum_open_form)
            select type (expected); type is (enum_open_form)

                is_equal_enum_open_form = &
                    all([actual%enum == expected%enum, &
                         actual%expr == expected%expr])

            end select
        end select
    end function is_equal_enum_open_form

    pure subroutine output_on_failure_enum_open_form_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        select type (actual); type is (enum_open_form)
            select type (expected); type is (enum_open_form)

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
    end subroutine output_on_failure_enum_open_form_to_str
end module fassert_open_form

module test_open_unitTests_form
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: fassert_open_form
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_form
    implicit none
    private
    public :: enum_open_form_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_form_assigns_enum_and_char_expr
    public :: open_form_default_returns_unformatted_for_direct_or_stream
    public :: open_form_default_returns_formatted_for_sequential_or_default
    public :: inquire_returns_default_char_expr_when_open_unit_wo_form_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_form_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_form%formatted%expr), "FORMATTED", &
                          "character expression of formatted should be 'FORMATTED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_form%unformatted%expr), "UNFORMATTED", &
                          "character expression of unformatted should be 'UNFORMATTED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_form%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_form_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_form_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_form) :: form

        form = open_form%unformatted

        call expect_equal(form, open_form%unformatted, &
                          "assigned enum should equal to rhs", &
                          comparator=is_equal_enum_open_form, &
                          verbose_message_writer=output_on_failure_enum_open_form_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_form_assigns_enum_and_char_expr

    subroutine open_form_default_returns_unformatted_for_direct_or_stream(error)
        use :: enumul_open_access
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_form) :: default

        ! direct access
        default = get_open_form_default(open_access%direct)

        call expect_equal(default, open_form%unformatted, &
                          "`get_open_form_default(direct)` should return the `UNFORMATTED`", &
                          comparator=is_equal_enum_open_form, &
                          verbose_message_writer=output_on_failure_enum_open_form_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! stream access
        default = get_open_form_default(open_access%stream)

        call expect_equal(default, open_form%unformatted, &
                          "`get_open_form_default(stream)` should return the `UNFORMATTED`", &
                          comparator=is_equal_enum_open_form, &
                          verbose_message_writer=output_on_failure_enum_open_form_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_form_default_returns_unformatted_for_direct_or_stream

    subroutine open_form_default_returns_formatted_for_sequential_or_default(error)
        use :: enumul_open_access
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        type(enum_open_form) :: default

        ! default (no argument)
        default = get_open_form_default()

        call expect_equal(default, open_form%formatted, &
                          "`get_open_form_default()` should return the `FORMATTED`", &
                          comparator=is_equal_enum_open_form, &
                          verbose_message_writer=output_on_failure_enum_open_form_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        ! sequential access
        default = get_open_form_default(open_access%sequential)

        print *, default%expr
        call expect_equal(default, open_form%formatted, &
                          "`get_open_form_default(sequential)` should return the `FORMATTED`", &
                          comparator=is_equal_enum_open_form, &
                          verbose_message_writer=output_on_failure_enum_open_form_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_form_default_returns_formatted_for_sequential_or_default

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_form_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: form
        integer(int32) :: unit
        type(enum_open_form) :: default

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, form=form)
        close (unit)

        default = get_open_form_default()
        call expect_equal(trim(form), trim(default%expr), &
                          "`inquire` returns the default character expression when open an unit without form specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_form_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: form
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, form=form)

        call expect_equal(trim(form), trim(open_form%undefined%expr), &
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

        type(enum_open_form) :: x, y

        x = open_form%formatted
        y = optval(x, default=get_open_form_default())

        call expect_equal(y, x, &
                          "y should equal to x", &
                          comparator=is_equal_enum_open_form, &
                          verbose_message_writer=output_on_failure_enum_open_form_to_str, &
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

        type(enum_open_form) :: y, default

        default = get_open_form_default()
        y = optval(default=default)

        call expect_equal(y, default, &
                          "y should equal to default", &
                          comparator=is_equal_enum_open_form, &
                          verbose_message_writer=output_on_failure_enum_open_form_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_form
