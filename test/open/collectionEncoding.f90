module test_open_collection_encoding
    use, intrinsic :: iso_fortran_env
    use :: testdrive, only:new_unittest, unittest_type
    use :: test_open_unitTests_encoding
    implicit none
    private
    public :: collect_open_encoding

contains
    subroutine collect_open_encoding(test_suite)
        implicit none
        type(unittest_type), allocatable, intent(out) :: test_suite(:)
            !! collection of tests

        test_suite = [ &
                     new_unittest("enum_open_encoding_list should have char-expr specified in the Fortran standard", &
                                  enum_open_encoding_list_has_char_expr_specified_in_standard) &
                     , new_unittest("= for enum_open_encoding should assign enum value and char-expr", &
                                    assignment_op_for_enum_open_encoding_assigns_enum_and_char_expr) &
                     , new_unittest("get_open_encoding_default() should return &
                                    &one of the enum having expr in the Fortran standard", &
                                    open_encoding_default_returns_one_of_enum_defined_in_std) &
                     , new_unittest("get_open_encoding_default() should return &
                                    &UNDEFINED for UNFORMATTED or for DIRECT/STREAM access", &
                                    open_encoding_default_returns_UNDEFINED_for_unformatted) &
                     , new_unittest("get_open_encoding_default() should return &
                                    &UNKNOWN for FORMATTED or for SEQUENTIAL access", &
                                    open_encoding_default_returns_UNKNOWN_for_formatted) &
                     , new_unittest("inquire() should return 'UTF-8' when the connection is for formatted IO &
                                    &with an encoding form of UTF-8", &
                                    inquire_returns_utf8_when_conn_is_for_formatted_w_utf8) &
                     , new_unittest("inquire() should return 'UNKNOWN' when the compiler is unable to determine encoding", &
                                    inquire_returns_unknown_when_unable_to_determine_encoding) &
                     , new_unittest("inquire() should return 'UNDEFINED' when the connection is for unformatted I/O", &
                                    inquire_returns_undefined_when_conn_is_for_unformatted) &
                     , new_unittest("optval() should return x when x is presented", &
                                    optval_returns_x_when_x_is_presented) &
                     , new_unittest("optval() should return default when x is not presented", &
                                    optval_returns_default_when_x_is_not_presented) &
                     ]
    end subroutine collect_open_encoding
end module test_open_collection_encoding
