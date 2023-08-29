program test_enumuml
    use :: test_enumul_collection
    use :: testdrive, only:new_testsuite, testsuite_type
    use :: testdrive_util, only:run_test
    implicit none

    type(testsuite_type), allocatable :: test_suites(:)
    test_suites = [ &
                  new_testsuite("enumul", collect_enumul) &
                  ]
    call run_test(test_suites)
end program test_enumuml
