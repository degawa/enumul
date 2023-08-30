module test_open_collection_status
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_open_unitTests_status
    implicit none
    private
    public :: collect_open_status

contains
    subroutine collect_open_status(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("enum_open_status_list should have char-expr specified in the Fortran standard", &
                                  enum_open_status_list_has_char_expr_specified_in_standard) &
                     , new_unittest("= for enum_open_status should assigns enum value and char-expr", &
                                    assignment_op_for_enum_open_status_assigns_enum_and_char_expr) &
                     , new_unittest("enum of the default open status specifier should be UNKNOWN", &
                                    default_open_status_enum_is_unknown) &
                     , new_unittest("optval() should return x when x is presented", &
                                    optval_returns_x_when_x_is_presented) &
                     , new_unittest("optval() should return default when x is not presented", &
                                    optval_returns_default_when_x_is_not_presented) &
                     ]
    end subroutine collect_open_status
end module test_open_collection_status
