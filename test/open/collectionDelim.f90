module test_open_collection_delim
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_open_unitTests_delim
    implicit none
    private
    public :: collect_open_delim

contains
    subroutine collect_open_delim(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("enum_open_delim_list should have char-expr specified in the Fortran standard", &
                                  enum_open_delim_list_has_char_expr_specified_in_standard) &
                     , new_unittest("= for enum_open_delim should assign enum value and char-expr", &
                                    assignment_op_for_enum_open_delim_assigns_enum_and_char_expr) &
                     , new_unittest("enum of the default open delim specifier should be NONE", &
                                    default_open_delim_enum_is_none) &
                     , new_unittest("inquire() should return the default char-expr &
                                    &when open an unit without the delim specifier", &
                                    inquire_returns_default_char_expr_when_open_unit_wo_delim_spec) &
                     , new_unittest("inquire() should return 'UNDEFINED' when there is no connection", &
                                    inquire_returns_undefined_when_there_is_no_connection) &
                     , new_unittest("optval() should return x when x is presented", &
                                    optval_returns_x_when_x_is_presented) &
                     , new_unittest("optval() should return default when x is not presented", &
                                    optval_returns_default_when_x_is_not_presented) &
                     ]
    end subroutine collect_open_delim
end module test_open_collection_delim
