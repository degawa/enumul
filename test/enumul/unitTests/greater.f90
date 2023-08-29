module test_enumul_unitTests_greater
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: test_enumul_unitTests_common
    implicit none
    private
    public :: gt_returns_true_when_lhs_has_greater_enum_than_rhs
    public :: ge_returns_true_when_lhs_has_greater_or_same_enum_than_rhs

contains
    subroutine gt_returns_true_when_lhs_has_greater_enum_than_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(enum_test_type) :: a, b
        logical :: stat
        character(:), allocatable :: msg

        a = enum_test_type(2)
        b = enum_test_type(1)
        call expect_true(a > b, &
                         "> operator returns `.true.` &
                         &when lhs and rhs have 2 and 1", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(-1)
        b = enum_test_type(-2)
        call expect_true(a > b, &
                         "> operator returns `.true.` &
                         &when lhs and rhs have -1 and -2", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(1)
        b = enum_test_type(2)
        call expect_true(a > b, &
                         "> operator does not returns `.true.` &
                         &when lhs and rhs have 1 and 2", &
                         expected_failure=.true., stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(1)
        b = enum_test_type(1)
        call expect_true(a > b, &
                         "> operator does not returns `.true.` &
                         &when lhs and rhs have the same IDs", &
                         expected_failure=.true., stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine gt_returns_true_when_lhs_has_greater_enum_than_rhs

    subroutine ge_returns_true_when_lhs_has_greater_or_same_enum_than_rhs(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(enum_test_type) :: a, b
        logical :: stat
        character(:), allocatable :: msg

        a = enum_test_type(2)
        b = enum_test_type(1)
        call expect_true(a >= b, &
                         ">= operator returns `.true.` &
                         &when lhs and rhs have 2 and 1", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(-1)
        b = enum_test_type(-2)
        call expect_true(a >= b, &
                         ">= operator returns `.true.` &
                         &when lhs and rhs have -1 and -2", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(1)
        b = enum_test_type(1)
        call expect_true(a >= b, &
                         ">= operator does not returns `.true.` &
                         &when lhs and rhs have the same IDs", &
                          stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(1)
        b = enum_test_type(2)
        call expect_true(a >= b, &
                         ">= operator does not returns `.true.` &
                         &when lhs and rhs have 1 and 2", &
                         expected_failure=.true., stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine ge_returns_true_when_lhs_has_greater_or_same_enum_than_rhs
end module test_enumul_unitTests_greater
