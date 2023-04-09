module enumul
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !>This abstract data type provides a component and operators
    !>for representing a enumerator type.
    type, public, abstract :: enum_atype
        integer(int32), public :: enum = 0
            !! enumerator<br>
            !! open to public for declaring as the parameter
            !! and use in select-case statement.
    contains
        !&<
        procedure, private, pass, non_overridable :: are_equal
        !* overloaded as `==` operator
        procedure, private, pass, non_overridable :: are_not_equal
        !* overloaded as `/=` operator
        procedure, private, pass, non_overridable :: is_greater_than
        !* overloaded as `>` operator
        procedure, private, pass, non_overridable :: is_less_than
        !* overloaded as `<` operator
        procedure, private, pass, non_overridable :: is_greater_than_or_equal_to
        !* overloaded as `>=` operator
        procedure, private, pass, non_overridable :: is_less_than_or_equal_to
        !* overloaded as `<=` operator
        procedure, private, pass, non_overridable :: assign
        !* overloaded as `==` operator
        generic :: operator(==)  => are_equal
        generic :: operator(/=)  => are_not_equal
        generic :: operator(>)   => is_greater_than
        generic :: operator(>=)  => is_greater_than_or_equal_to
        generic :: operator(<)   => is_less_than
        generic :: operator(<=)  => is_less_than_or_equal_to
        generic :: assignment(=) => assign
        !&>
    end type enum_atype

contains
    !>returns `.true.` when `lhs` and `rhs` are the same type
    !>and have the same enum value,
    !>and returns `.false.` elsewhere.
    !>
    !>This procedure will be overloaded as `==` operator.
    pure logical function are_equal(lhs, rhs)
        implicit none
        class(enum_atype), intent(in) :: lhs
            !! left-hand side of the `==` operator
        class(enum_atype), intent(in) :: rhs
            !! right-hand side of the `==` operator

        if (.not. same_type_as(lhs, rhs)) then
            are_equal = .false.
            return
        end if

        are_equal = (lhs%enum == rhs%enum)
    end function are_equal

    !>returns `.true.` when `lhs` and `rhs` are the same type
    !>and have different enum values,
    !>and returns `.false.` elsewhere.
    !>
    !>This procedure will be overloaded as `/=` operator.
    pure logical function are_not_equal(lhs, rhs)
        implicit none
        class(enum_atype), intent(in) :: lhs
            !! left-hand side of the `/=` operator
        class(enum_atype), intent(in) :: rhs
            !! right-hand side of the `/=` operator

        if (.not. same_type_as(lhs, rhs)) then
            are_not_equal = .true.
            return
        end if

        are_not_equal = .not. are_equal(lhs, rhs)
    end function are_not_equal

    !>returns `.true.` when `lhs` and `rhs` are the same type
    !>and `lhs` has an enum value greater than `rhs` one,
    !>and returns `.false.` elsewhere.
    !>
    !>This procedure will be overloaded as `>` operator.
    pure logical function is_greater_than(lhs, rhs)
        implicit none
        class(enum_atype), intent(in) :: lhs
            !! left-hand side of the `>` operator
        class(enum_atype), intent(in) :: rhs
            !! right-hand side of the `>` operator

        if (.not. same_type_as(lhs, rhs)) then
            is_greater_than = .false.
            return
        end if

        is_greater_than = (lhs%enum > rhs%enum)
    end function is_greater_than

    !>returns `.true.` when `lhs` and `rhs` are the same type
    !>and `lhs` has an enum value greater than or equal to `rhs` one,
    !>and returns `.false.` elsewhere.
    !>
    !>This procedure will be overloaded as `>=` operator.
    pure logical function is_greater_than_or_equal_to(lhs, rhs)
        implicit none
        class(enum_atype), intent(in) :: lhs
            !! left-hand side of the `>=` operator
        class(enum_atype), intent(in) :: rhs
            !! right-hand side of the `>=` operator

        if (.not. same_type_as(lhs, rhs)) then
            is_greater_than_or_equal_to = .false.
            return
        end if

        is_greater_than_or_equal_to = ((lhs%enum > rhs%enum) .or. (lhs%enum == rhs%enum))
    end function is_greater_than_or_equal_to

    !>returns `.true.` when `lhs` and `rhs` are the same type
    !>and `lhs` has an enum value less than `rhs` one,
    !>and returns `.false.` elsewhere.
    !>
    !>This procedure will be overloaded as `<` operator.
    pure logical function is_less_than(lhs, rhs)
        implicit none
        class(enum_atype), intent(in) :: lhs
            !! left-hand side of the `<` operator
        class(enum_atype), intent(in) :: rhs
            !! right-hand side of the `<` operator

        if (.not. same_type_as(lhs, rhs)) then
            is_less_than = .false.
            return
        end if

        is_less_than = (lhs%enum < rhs%enum)
    end function is_less_than

    !>returns `.true.` when `lhs` and `rhs` are the same type
    !>and `lhs` has an enum value less than or equal to `rhs` one,
    !>and returns `.false.` elsewhere.
    !>
    !>This procedure will be overloaded as `<=` operator.
    pure logical function is_less_than_or_equal_to(lhs, rhs)
        implicit none
        class(enum_atype), intent(in) :: lhs
            !! left-hand side of the `<=` operator
        class(enum_atype), intent(in) :: rhs
            !! right-hand side of the `<=` operator

        if (.not. same_type_as(lhs, rhs)) then
            is_less_than_or_equal_to = .false.
            return
        end if

        is_less_than_or_equal_to = ((lhs%enum < rhs%enum) .or. (lhs%enum == rhs%enum))
    end function is_less_than_or_equal_to

    !>assigns the enum value of `lhs` to `rhs` when both are the same type.
    !>The enum value of `rhs` is undefined when `lhs` and `rhs` are different types.
    !>
    !>This procedure will be overloaded as `=` operator.
    pure subroutine assign(lhs, rhs)
        implicit none
        class(enum_atype), intent(inout) :: lhs
            !! left-hand side of the `=` operator
        class(enum_atype), intent(in) :: rhs
            !! right-hand side of the `=` operator

        if (.not. same_type_as(lhs, rhs)) then
            return
        end if

        lhs%enum = rhs%enum
    end subroutine assign
end module enumul
