program test_open
    use :: test_open_collection_access
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("open/access", collect_open_access) &
                  ]
    call run_test(test_suites)
end program test_open
