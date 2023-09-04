module fassert_open_round
    use :: fassert_kit
    use :: enumul_open_round
    implicit none
    private
    public :: is_equal_enum_open_round
    public :: output_on_failure_enum_open_round_to_str

    character(*), parameter :: fmt = '('//fmt_indent//',A,A)'
    character(*), parameter :: type_mismatch_expected = "Type mismatch: `expected` is not enum_open_round"
    character(*), parameter :: type_mismatch_actual = "Type mismatch: `actual` is not enum_open_round"

contains
    pure logical function is_equal_enum_open_round(actual, expected)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected

        is_equal_enum_open_round = .false.
        select type (actual); type is (enum_open_round)
            select type (expected); type is (enum_open_round)

                is_equal_enum_open_round = &
                    all([actual%enum == expected%enum, &
                         actual%expr == expected%expr])

            end select
        end select
    end function is_equal_enum_open_round

    pure subroutine output_on_failure_enum_open_round_to_str(actual, expected, output_message)
        implicit none
        class(*), intent(in) :: actual
        class(*), intent(in) :: expected
        character(:), allocatable, intent(inout) :: output_message
        character(64) :: buffer

        select type (actual); type is (enum_open_round)
            select type (expected); type is (enum_open_round)

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
    end subroutine output_on_failure_enum_open_round_to_str
end module fassert_open_round

module test_open_unitTests_round
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: fassert_open_round
    use :: testdrive, only:error_type, check
    use :: testdrive_util, only:occurred
    use :: enumul_open_round
    implicit none
    private
    public :: enum_open_round_list_has_char_expr_specified_in_standard
    public :: assignment_op_for_enum_open_round_assigns_enum_and_char_expr
    public :: open_round_default_returns_one_of_enum_defined_in_std
    public :: inquire_returns_default_char_expr_when_open_unit_wo_round_spec
    public :: inquire_returns_undefined_when_there_is_no_connection
    public :: optval_returns_x_when_x_is_presented
    public :: optval_returns_default_when_x_is_not_presented

contains
    subroutine enum_open_round_list_has_char_expr_specified_in_standard(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg

        call expect_equal(trim(open_round%up%expr), "UP", &
                          "character expression of up  should be 'UP'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_round%down%expr), "DOWN", &
                          "character expression of down should be 'DOWN'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_round%zero%expr), "ZERO", &
                          "character expression of zero should be 'ZERO'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_round%nearest%expr), "NEAREST", &
                          "character expression of nearest should be 'NEAREST'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_round%compatible%expr), "COMPATIBLE", &
                          "character expression of compatible should be 'COMPATIBLE'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_round%processor_defined%expr), "PROCESSOR_DEFINED", &
                          "character expression of processor_defined should be 'PROCESSOR_DEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(open_round%undefined%expr), "UNDEFINED", &
                          "character expression of undefined should be 'UNDEFINED'", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine enum_open_round_list_has_char_expr_specified_in_standard

    subroutine assignment_op_for_enum_open_round_assigns_enum_and_char_expr(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_round) :: round

        round = open_round%compatible

        call expect_equal(round, open_round%compatible, &
                          "assigned enum should equal to rhs", &
                          comparator=is_equal_enum_open_round, &
                          verbose_message_writer=output_on_failure_enum_open_round_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine assignment_op_for_enum_open_round_assigns_enum_and_char_expr

    subroutine open_round_default_returns_one_of_enum_defined_in_std(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        type(enum_open_round) :: default

        default = get_open_round_default()

        call expect_true(any([is_equal_enum_open_round(default, open_round%up) &
                              , is_equal_enum_open_round(default, open_round%down) &
                              , is_equal_enum_open_round(default, open_round%zero) &
                              , is_equal_enum_open_round(default, open_round%nearest) &
                              , is_equal_enum_open_round(default, open_round%compatible) &
#if defined(__GFORTRAN__) || defined(NAGFOR)
                              , is_equal_enum_open_round(default, open_round%processor_defined) &
#endif
                              ]), &
                         "`get_open_round_default` should return &
                         &one of the enum for those specified in the Fortran standard", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine open_round_default_returns_one_of_enum_defined_in_std

    subroutine inquire_returns_default_char_expr_when_open_unit_wo_round_spec(error)
        use :: enumul_open_status
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: round
        integer(int32) :: unit
        type(enum_open_round) :: default

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, round=round)
        close (unit)

        default = get_open_round_default()

        call expect_equal(trim(round), trim(default%expr), &
                          "`inquire` returns the default character expression when open an unit without round specifier", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine inquire_returns_default_char_expr_when_open_unit_wo_round_spec

    subroutine inquire_returns_undefined_when_there_is_no_connection(error)
        use :: newunit
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        logical :: stat
        character(:), allocatable :: msg
        character(32) :: round
        integer(int32) :: unit

        unit = get_newunit_number()
        inquire (unit, round=round)

        call expect_equal(trim(round), trim(open_round%undefined%expr), &
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

        type(enum_open_round) :: x, y

        x = open_round%down
        y = optval(x, default=open_round%nearest)

        call expect_equal(y, x, &
                          "y should equal to x", &
                          comparator=is_equal_enum_open_round, &
                          verbose_message_writer=output_on_failure_enum_open_round_to_str, &
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

        type(enum_open_round) :: y, default

        default = get_open_round_default()
        y = optval(default=default)

        call expect_equal(y, default, &
                          "y should equal to default", &
                          comparator=is_equal_enum_open_round, &
                          verbose_message_writer=output_on_failure_enum_open_round_to_str, &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_round
