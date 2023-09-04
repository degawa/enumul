module fassert_open_position
    use :: fassert_kit
    use :: enumul_open_position
    implicit none
    private
    public :: is_equal_enum_open_position
    public :: output_on_failure_enum_open_position_to_str

    character(*), parameter :: fmt = '('//fmt_indent//',A,A)'
    character(*), parameter :: type_mismatch_expected = "Type mismatch: `expected` is not enum_open_position"
    character(*), parameter :: type_mismatch_actual = "Type mismatch: `actual` is not enum_open_position"

contains
    pure logical function is_equal_enum_open_position(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_enum_open_position = .false.
        select type (actual); type is (enum_open_position)
            select type (expected); type is (enum_open_position)

                is_equal_enum_open_position = &
                    all([actual%enum == expected%enum, &
                         actual%expr == expected%expr])

            end select
        end select
    end function is_equal_enum_open_position

    pure subroutine output_on_failure_enum_open_position_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        select type (actual); type is (enum_open_position)
            select type (expected); type is (enum_open_position)

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
    end subroutine output_on_failure_enum_open_position_to_str
end module fassert_open_position

module test_open_unitTests_position
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: fassert_open_position
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_position
    implicit none
    private
    public :: enum_open_position_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_position_assigns_enum_and_char_expr
    public :: default_open_position_enum_is_asis
    public :: inquire_returns_default_char_expr_when_open_wo_position_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_position_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_position%asis%expr), "ASIS", &
                          "character expression of asis should be 'ASIS'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_position%rewind%expr), "REWIND", &
                          "character expression of rewind should be 'REWIND'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_position%append%expr), "APPEND", &
                          "character expression of append should be 'APPEND'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_position%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_position_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_position_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_position) :: position

        position = open_position%append

        call expect_equal(position, open_position%append, &
                          "assigned enum should equal to rhs", &
                          comparator=is_equal_enum_open_position, &
                          verbose_message_writer=output_on_failure_enum_open_position_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_position_assigns_enum_and_char_expr

    subroutine default_open_position_enum_is_asis(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_position) :: default

        call expect_equal(default_open_position, open_position%asis, &
                          "default position specifier shoud equal to the asis", &
                          comparator=is_equal_enum_open_position, &
                          verbose_message_writer=output_on_failure_enum_open_position_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        default = get_open_position_default()

        call expect_equal(default_open_position, open_position%asis, &
                          "`get_open_position_default` should return `default_open_position`", &
                          comparator=is_equal_enum_open_position, &
                          verbose_message_writer=output_on_failure_enum_open_position_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine default_open_position_enum_is_asis

    subroutine inquire_returns_default_char_expr_when_open_wo_position_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: position
        integer(int32) :: unit

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, position=position)
        close (unit)

        call expect_equal(trim(position), trim(default_open_position%expr), &
                          "`inquire` returns the default character expression when open an unit without position specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_wo_position_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: position
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, position=position)

        call expect_equal(trim(position), trim(open_position%undefined%expr), &
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

        type(enum_open_position) :: x, y

        x = open_position%rewind
        y = optval(x, default=default_open_position)

        call expect_equal(y, x, &
                          "y should equal to x", &
                          comparator=is_equal_enum_open_position, &
                          verbose_message_writer=output_on_failure_enum_open_position_to_str, &
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

        type(enum_open_position) :: y

        y = optval(default=default_open_position)

        call expect_equal(y, default_open_position, &
                          "y should equal to default", &
                          comparator=is_equal_enum_open_position, &
                          verbose_message_writer=output_on_failure_enum_open_position_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_position
