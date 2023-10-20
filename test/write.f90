program test_write
    use :: test_write_collection_advance
    use :: test_write_collection_asynchronous
    use :: test_write_collection_decimal
    use :: test_write_collection_delim
    use :: test_write_collection_round
    use :: test_write_collection_sign
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("write/advance", collect_write_advance) &
                  , new_testsuite("write/asynchronous", collect_write_asynchronous) &
                  , new_testsuite("write/decimal", collect_write_decimal) &
                  , new_testsuite("write/delim", collect_write_delim) &
                  , new_testsuite("write/round", collect_write_round) &
                  , new_testsuite("write/sign", collect_write_sign) &
                  ]
    call run_test(test_suites)
end program test_write
