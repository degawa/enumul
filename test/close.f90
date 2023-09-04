program test_close
    use :: test_close_collection_status
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("close/status", collect_close_status) &
                  ]
    call run_test(test_suites)
end program test_close
