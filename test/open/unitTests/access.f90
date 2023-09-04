module fassert_open_access
    use :: fassert_kit
    use :: enumul_open_access
    implicit none
    private
    public :: is_equal_enum_open_access
    public :: output_on_failure_enum_open_access_to_str

    character(*), parameter :: fmt = '('//fmt_indent//',A,A)'
    character(*), parameter :: type_mismatch_expected = "Type mismatch: `expected` is not enum_open_access"
    character(*), parameter :: type_mismatch_actual = "Type mismatch: `actual` is not enum_open_access"

contains
    pure logical function is_equal_enum_open_access(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_enum_open_access = .false.
        select type (actual); type is (enum_open_access)
            select type (expected); type is (enum_open_access)

                is_equal_enum_open_access = &
                    all([actual%enum == expected%enum, &
                         actual%expr == expected%expr])

            end select
        end select
    end function is_equal_enum_open_access

    pure subroutine output_on_failure_enum_open_access_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        select type (actual); type is (enum_open_access)
            select type (expected); type is (enum_open_access)

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
    end subroutine output_on_failure_enum_open_access_to_str
end module fassert_open_access

module test_open_unitTests_access
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: fassert_open_access
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_access
    implicit none
    private
    public :: enum_open_access_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_access_assigns_enum_and_char_expr
    public :: default_open_access_enum_is_sequential
    public :: inquire_returns_default_char_expr_when_open_unit_wo_access_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_access_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_access%sequential%expr), "SEQUENTIAL", &
                          "character expression of sequential should be 'SEQUENTIAL'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_access%direct%expr), "DIRECT", &
                          "character expression of direct should be 'DIRECT'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_access%stream%expr), "STREAM", &
                          "character expression of stream should be 'STREAM'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_access%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_access_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_access_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_access) :: access

        access = open_access%sequential

        call expect_equal(access, open_access%sequential, &
                          "assigned enum should equal to rhs", &
                          comparator=is_equal_enum_open_access, &
                          verbose_message_writer=output_on_failure_enum_open_access_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_access_assigns_enum_and_char_expr

    subroutine default_open_access_enum_is_sequential(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_access) :: default

        call expect_equal(default_open_access, open_access%sequential, &
                          "default access specifier shoud equal to the sequential", &
                          comparator=is_equal_enum_open_access, &
                          verbose_message_writer=output_on_failure_enum_open_access_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_access_default()

        call expect_equal(default, default_open_access, &
                          "`get_open_access_default` should return `default_open_access`", &
                          comparator=is_equal_enum_open_access, &
                          verbose_message_writer=output_on_failure_enum_open_access_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_access_enum_is_sequential

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_access_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: access
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, access=access)
        close (unit)

        call expect_equal(trim(access), trim(default_open_access%expr), &
                          "`inquire` returns the default character expression when open an unit without access specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_access_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: access
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, access=access)

        call expect_equal(trim(access), trim(open_access%undefined%expr), &
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

        type(enum_open_access) :: x, y

        x = open_access%stream
        y = optval(x, default=default_open_access)

        call expect_equal(y, x, &
                          "y should equal to x", &
                          comparator=is_equal_enum_open_access, &
                          verbose_message_writer=output_on_failure_enum_open_access_to_str, &
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

        type(enum_open_access) :: y

        y = optval(default=default_open_access)

        call expect_equal(y, default_open_access, &
                          "y should equal to the default", &
                          comparator=is_equal_enum_open_access, &
                          verbose_message_writer=output_on_failure_enum_open_access_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_access
