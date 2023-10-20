module enumul_write_asynchronous
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Write_Asynchronous_Yes = 1
        enumerator :: Write_Asynchronous_No
    end enum

    character(*), parameter, private :: asynchronous(*) = ["YES", &
                                                           "NO "]
        !! The possible character expressions
        !! for the `asynchronous` specifier in the `write` statement.

    !>The enumerator for the `asynchronous` specifier
    !>and expression in the `write` statement.
    type, public, extends(enum_atype) :: enum_write_asynchronous
        character(len=len(asynchronous)), public :: expr
            !! An expression for the `asynchronous` specifier.
            !! Open to public to pass the `write` statement.
    end type enum_write_asynchronous

    !>The possible expressions for the `asynchronous` specifier
    type :: enum_write_asynchronous_list
        type(enum_write_asynchronous), public :: yes
            !! The enumerator to represent the `YES`
        type(enum_write_asynchronous), public :: no
            !! The enumerator to represent the `NO`
    end type enum_write_asynchronous_list

    type(enum_write_asynchronous_list), public, parameter :: &
        write_asynchronous = &
            enum_write_asynchronous_list( &
                yes = enum_write_asynchronous(Write_Asynchronous_Yes, asynchronous(Write_Asynchronous_Yes)), &
                no  = enum_write_asynchronous(Write_Asynchronous_No , asynchronous(Write_Asynchronous_No)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `asynchronous` specifier.

    interface optval
        procedure :: optval_write_asynchronous
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_write_asynchronous(x, default) result(y)
        implicit none
        type(enum_write_asynchronous), intent(in), optional :: x
        type(enum_write_asynchronous), intent(in) :: default

        type(enum_write_asynchronous) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_write_asynchronous
end module enumul_write_asynchronous
