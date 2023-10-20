module enumul_write_advance
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Write_Advance_Yes = 1
        enumerator :: Write_Advance_No
    end enum

    character(*), parameter, private :: advance(*) = ["YES", &
                                                      "NO "]
        !! The possible character expressions
        !! for the `advance` specifier in the `write` statement.

    !>The enumerator for the `advance` specifier
    !>and expression in the `write` statement.
    type, public, extends(enum_atype) :: enum_write_advance
        character(len=len(advance)), public :: expr
            !! An expression for the `advance` specifier.
            !! Open to public to pass the `write` statement.
    contains
        procedure, public, pass :: trim => trim_expr
        !* returns expr with trailing blank spaces removed
    end type enum_write_advance

    type :: enum_write_advance_list
        type(enum_write_advance), public :: yes
            !! The enumerator to represent the `YES`
        type(enum_write_advance), public :: no
            !! The enumerator to represent the `NO`
    end type enum_write_advance_list

    type(enum_write_advance_list), public, parameter :: &
        write_advance = &
            enum_write_advance_list( &
                yes = enum_write_advance(Write_Advance_Yes, advance(Write_Advance_Yes)), &
                no  = enum_write_advance(Write_Advance_No , advance(Write_Advance_No)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `advance` specifier.

    interface optval
        procedure :: optval_write_advance
    end interface
contains
    !>Returns character expression with trailing blank spaces removed
    pure function trim_expr(this) result(expr)
        implicit none
        class(enum_write_advance), intent(in) :: this
        character(:), allocatable :: expr

        expr = trim(this%expr)
    end function trim_expr

    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_write_advance(x, default) result(y)
        implicit none
        type(enum_write_advance), intent(in), optional :: x
        type(enum_write_advance), intent(in) :: default

        type(enum_write_advance) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_write_advance
end module enumul_write_advance
