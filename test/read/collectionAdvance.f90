module test_read_collection_advance
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_read_unitTests_advance
    implicit none
    private
    public :: collect_read_advance

contains
    subroutine collect_read_advance(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("enum_read_adv_list should have char-expr specified in the Fortran standard", &
                                  enum_read_adv_list_has_char_expr_specified_in_standard) &
                     , new_unittest("= for enum_read_adv should assign enum value and char-expr", &
                                    assignment_op_for_enum_read_adv_assigns_enum_and_char_expr) &
                     , new_unittest("optval() should return x when x is presented", &
                                    optval_returns_x_when_x_is_presented) &
                     , new_unittest("optval() should return default when x is not presented", &
                                    optval_returns_default_when_x_is_not_presented) &
                     ]
    end subroutine collect_read_advance
end module test_read_collection_advance
