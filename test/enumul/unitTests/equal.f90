module test_enumul_unitTests_equal
    use, intrinsic :: iso_fortran_env
    use :: fassert
    use :: testdrive, only:error_type, check, to_string
    use :: testdrive_util, only:occurred
    use :: test_enumul_unitTests_common
    implicit none
    private
    public :: eq_returns_true_when_two_vars_have_the_same_enum
    public :: eq_returns_false_when_two_vars_have_different_enums
    public :: ne_returns_true_when_two_vars_have_different_enums
    public :: ne_returns_false_when_two_vars_have_the_same_enum

contains
    subroutine eq_returns_true_when_two_vars_have_the_same_enum(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(enum_test_type) :: a, b
        logical :: stat
        character(:), allocatable :: msg

        a = enum_test_type(1)
        b = enum_test_type(1)
        call expect_true(a == b, &
                         "== operator returns `.true.` &
                         &when lhs and rhs have 1", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(-1)
        b = enum_test_type(-1)
        call expect_true(a == b, &
                         "== operator returns `.true.` &
                         &when lhs and rhs have -1", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(3)
        b = a
        call expect_true(a == b, &
                         "== operator returns `.true.` &
                         &when lhs and rhs have the same ID", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(0)
        b = enum_test_type(1)
        call expect_true(a == b, &
                         "== operator does not return `.true.` &
                         &when lhs has 0 and rhs has 1", &
                         expected_failure=.true., stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine eq_returns_true_when_two_vars_have_the_same_enum

    subroutine eq_returns_false_when_two_vars_have_different_enums(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(enum_test_type) :: a, b
        logical :: stat
        character(:), allocatable :: msg

        a = enum_test_type(1)
        b = enum_test_type(0)
        call expect_false(a == b, &
                         "== operator returns `.false.` &
                         &when lhs has 1 and rhs has 0", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(3)
        b = a
        call expect_false(a == b, &
                         "== operator does not return `.false.` &
                         &when lhs and rhs have the same ID", &
                         expected_failure=.true., stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine eq_returns_false_when_two_vars_have_different_enums

    subroutine ne_returns_true_when_two_vars_have_different_enums(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(enum_test_type) :: a, b
        logical :: stat
        character(:), allocatable :: msg

        a = enum_test_type(1)
        b = enum_test_type(0)
        call expect_true(a /= b, &
                         "/= operator returns `.true.` &
                         &when lhs has 1 and rhs has 0", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(3)
        b = a
        call expect_true(a /= b, &
                         "/= operator does not return `.true.` &
                         &when lhs and rhs have the same ID", &
                         expected_failure=.true., stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine ne_returns_true_when_two_vars_have_different_enums

    subroutine ne_returns_false_when_two_vars_have_the_same_enum(error)
        implicit none
        type(error_type), allocatable, intent(out) :: error
            !! error handler

        type(enum_test_type) :: a, b
        logical :: stat
        character(:), allocatable :: msg

        a = enum_test_type(1)
        b = enum_test_type(1)
        call expect_false(a /= b, &
                         "/= operator returns `.false.` &
                         &when lhs and rhs have 1", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(-1)
        b = enum_test_type(-1)
        call expect_false(a /= b, &
                         "/= operator returns `.false.` &
                         &when lhs and rhs have -1", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(3)
        b = a
        call expect_false(a /= b, &
                         "/= operator returns `.false.` &
                         &when lhs and rhs have the same ID", &
                         stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return

        a = enum_test_type(0)
        b = enum_test_type(1)
        call expect_false(a /= b, &
                         "/= operator does not return `.false.` &
                         &when lhs have 0 and rhs have 1", &
                         expected_failure=.true., stat=stat, output_message=msg)
        call check(error, stat, msg)
        if (occurred(error)) return
    end subroutine ne_returns_false_when_two_vars_have_the_same_enum
end module test_enumul_unitTests_equal
