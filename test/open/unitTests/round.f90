module test_open_unitTests_round
    use, intrinsic :: iso_fortran_env
    use :: fassert
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

        call expect_equal(round%enum, open_round%compatible%enum, &
                          "assigned enum should have the same enum value of the rhs", stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(round%expr), trim(open_round%compatible%expr), &
                          "assigned enum should have the same char-expr of the rhs", stat=stat, output_message=msg)
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

        call expect_true(any([default%enum == open_round%up%enum &
                              , default%enum == open_round%down%enum &
                              , default%enum == open_round%zero%enum &
                              , default%enum == open_round%nearest%enum &
                              , default%enum == open_round%compatible%enum &
#if defined(__GFORTRAN__)
                              , default%enum == open_round%processor_defined%enum &
#endif
                              ]), &
                         "enum of return value of `get_open_round_default` should be &
                         &one of the enum having expr in the Fortran standard", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_true(any([default%expr == open_round%up%expr &
                              , default%expr == open_round%down%expr &
                              , default%expr == open_round%zero%expr &
                              , default%expr == open_round%nearest%expr &
                              , default%expr == open_round%compatible%expr &
#if defined(__GFORTRAN__)
                              , default%expr == open_round%processor_defined%expr &
#endif
                              ]), &
                         "character expression of return value of `get_open_round_default` &
                         &should be one of the enum having expr in the Fortran standard", &
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

        call expect_equal(y%enum, x%enum, &
                          "enum of y should equal to that of x", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(x%expr), &
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

        type(enum_open_round) :: y

        y = optval(default=open_round%nearest)

        call expect_equal(y%enum, open_round%nearest%enum, &
                          "enum of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        call expect_equal(trim(y%expr), trim(open_round%nearest%expr), &
                          "character expression of y should equal to that of default", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine optval_returns_default_when_x_is_not_presented
end module test_open_unitTests_round
