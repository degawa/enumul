program test_read
    use :: test_read_collection_advance
    use :: test_read_collection_asynchronous
    use :: test_read_collection_blank
    use :: test_read_collection_decimal
    use :: test_read_collection_pad
    use :: test_read_collection_round
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("read/advance", collect_read_advance) &
                  , new_testsuite("read/asynchronous", collect_read_asynchronous) &
                  , new_testsuite("read/blank", collect_read_blank) &
                  , new_testsuite("read/decimal", collect_read_decimal) &
                  , new_testsuite("read/pad", collect_read_pad) &
                  , new_testsuite("read/round", collect_read_round) &
                  ]
    call run_test(test_suites)
end program test_read
