module enumul_read_advance
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Read_Advance_Yes = 1
        enumerator :: Read_Advance_No
    end enum

    character(*), parameter, private :: advance(*) = ["YES", &
                                                      "NO "]
        !! The possible character expressions
        !! for the `advance` specifier in the `read` statement.

    !>The enumerator for the `advance` specifier
    !>and expression in the `read` statement.
    type, public, extends(enum_atype) :: enum_read_advance
        character(len=len(advance)), public :: expr
            !! An expression for the `advance` specifier.
            !! Open to public to pass the `read` statement.
    contains
        procedure, public, pass :: trim => trim_expr
        !* returns expr with trailing blank spaces removed
    end type enum_read_advance

    !>The possible expressions for the `advance` specifier
    type :: enum_read_advance_list
        type(enum_read_advance), public :: yes
            !! The enumerator to represent the `YES`
        type(enum_read_advance), public :: no
            !! The enumerator to represent the `NO`
    end type enum_read_advance_list

    type(enum_read_advance_list), public, parameter :: &
        read_advance = &
            enum_read_advance_list( &
                yes = enum_read_advance(Read_Advance_Yes, advance(Read_Advance_Yes)), &
                no  = enum_read_advance(Read_Advance_No , advance(Read_Advance_No)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `advance` specifier.

    interface optval
        procedure :: optval_read_advance
    end interface
contains
    !>Returns character expression with trailing blank spaces removed
    pure function trim_expr(this) result(expr)
        implicit none
        class(enum_read_advance), intent(in) :: this
        character(:), allocatable :: expr

        expr = trim(this%expr)
    end function trim_expr

    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_read_advance(x, default) result(y)
        implicit none
        type(enum_read_advance), intent(in), optional :: x
        type(enum_read_advance), intent(in) :: default

        type(enum_read_advance) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_read_advance
end module enumul_read_advance
