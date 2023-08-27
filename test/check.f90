module enumul_test
    use :: enumul
    implicit none
    private

    type, public, extends(enum_atype) :: enum_test_type
    end type enum_test_type
end module enumul_test

program check
    use, intrinsic :: iso_fortran_env
    use :: enumul_test
    use :: fassert
    implicit none

    type(enum_test_type) :: a, b, c, d

    a = enum_test_type(1)
    b = enum_test_type(2)
    c = a
    d = enum_test_type(0)

    call assert_true(a == c, "== operator test between variables with the same item id")
    call assert_false(a == b, "== operator test between variables with different item id")
    call assert_true(a /= b, "/= operator test between variables with different item id")
    call assert_false(a /= c, "/= operator test between variables with the same item id")

    call assert_true(b > a, "> operator test between variables with different item id")
    call assert_false(d > a, "> operator test between variables with different item id")

    call assert_true(a < b, "< operator test between variables with different item id")
    call assert_false(a < d, "< operator test between variables with different item id")

    call assert_true(c >= a, ">= operator test between variables with the same item id")
    call assert_true(c <= a, "<= operator test between variables with the same item id")
    call assert_true(b >= a, ">= operator test between variables with different item id")
    call assert_true(d <= a, "<= operator test between variables with different item id")
end program check
