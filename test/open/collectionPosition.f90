module test_open_collection_position
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_open_unitTests_position
    implicit none
    private
    public :: collect_open_position

contains
    subroutine collect_open_position(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("enum_open_position_list should have char-expr specified in the Fortran standard", &
                                  enum_open_position_list_has_char_expr_specified_in_standard) &
                     , new_unittest("= for enum_open_position should assign enum value and char-expr", &
                                    assignment_op_for_enum_open_position_assigns_enum_and_char_expr) &
                     , new_unittest("the default open position specifier should be ASIS", &
                                    default_open_position_enum_is_asis) &
                     , new_unittest("inquire() should return the default char-expr &
                                    &when open an unit without the position specifier", &
                                    inquire_returns_default_char_expr_when_open_wo_position_spec) &
                     , new_unittest("inquire() should return 'UNDEFINED' when there is no connection", &
                                    inquire_returns_undefined_when_there_is_no_connection) &
                     , new_unittest("optval() should return x when x is presented", &
                                    optval_returns_x_when_x_is_presented) &
                     , new_unittest("optval() should return default when x is not presented", &
                                    optval_returns_default_when_x_is_not_presented) &
                     ]
    end subroutine collect_open_position
end module test_open_collection_position
