module test_enumul_collection
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_enumul_unitTests_equal
    use :: test_enumul_unitTests_greater
    use :: test_enumul_unitTests_less
    implicit none
    private
    public :: collect_enumul

contains
    subroutine collect_enumul(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("== operator returns true when 2 enum vars have the same enum", &
                                  eq_returns_true_when_two_vars_have_the_same_enum) &
                     , new_unittest("== operator returns false when 2 enum vars have different enums", &
                                    eq_returns_false_when_two_vars_have_different_enums) &
                     , new_unittest("/= operator returns true when 2 enum vars have different enums", &
                                    ne_returns_true_when_two_vars_have_different_enums) &
                     , new_unittest("/= operator returns false when 2 enum vars have the same enums", &
                                    ne_returns_false_when_two_vars_have_the_same_enum) &
                     , new_unittest("> operator returns true when lhs has greater enum than rhs", &
                                    gt_returns_true_when_lhs_has_greater_enum_than_rhs) &
                     , new_unittest(">= operator returns true when lhs has greater or the same enum than rhs", &
                                    ge_returns_true_when_lhs_has_greater_or_same_enum_than_rhs) &
                     , new_unittest("< operator returns true when lhs has greater enum than rhs", &
                                    lt_returns_true_when_lhs_has_greater_enum_than_rhs) &
                     , new_unittest("<= operator returns true when lhs has greater or the same enum than rhs", &
                                    le_returns_true_when_lhs_has_greater_or_same_enum_than_rhs) &
                     ]
    end subroutine collect_enumul
end module test_enumul_collection
