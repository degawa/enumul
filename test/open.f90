program test_open
    use :: test_open_collection_access
    use :: test_open_collection_action
    use :: test_open_collection_asynchronous
    use :: test_open_collection_blank
    use :: test_open_collection_decimal
    use :: test_open_collection_delim
    use :: test_open_collection_form
    use :: test_open_collection_pad
    use :: test_open_collection_position
    use :: test_open_collection_sign
    use :: test_open_collection_status
    use :: test_open_collection_encoding
    use :: test_open_collection_round
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("open/status", collect_open_status) &
                  , new_testsuite("open/form", collect_open_form) &
                  , new_testsuite("open/access", collect_open_access) &
                  , new_testsuite("open/action", collect_open_action) &
                  , new_testsuite("open/asynchronous", collect_open_asynchronous) &
                  , new_testsuite("open/blank", collect_open_blank) &
                  , new_testsuite("open/decimal", collect_open_decimal) &
                  , new_testsuite("open/delim", collect_open_delim) &
                  , new_testsuite("open/pad", collect_open_pad) &
                  , new_testsuite("open/position", collect_open_position) &
                  , new_testsuite("open/sign", collect_open_sign) &
                  , new_testsuite("open/encoding", collect_open_encoding) &
                  , new_testsuite("open/round", collect_open_round) &
                  ]
    call run_test(test_suites)
end program test_open
