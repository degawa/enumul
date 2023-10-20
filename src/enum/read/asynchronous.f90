module enumul_read_asynchronous
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Read_Asynchronous_Yes = 1
        enumerator :: Read_Asynchronous_No
    end enum

    character(*), parameter, private :: asynchronous(*) = ["YES", &
                                                           "NO "]
        !! The possible character expressions
        !! for the `asynchronous` specifier in the `read` statement.

    !>The enumerator for the `asynchronous` specifier
    !>and expression in the `read` statement.
    type, public, extends(enum_atype) :: enum_read_asynchronous
        character(len=len(asynchronous)), public :: expr
            !! An expression for the `asynchronous` specifier.
            !! Open to public to pass the `read` statement.
    end type enum_read_asynchronous

    !>The possible expressions for the `asynchronous` specifier
    type :: enum_read_asynchronous_list
        type(enum_read_asynchronous), public :: yes
            !! The enumerator to represent the `YES`
        type(enum_read_asynchronous), public :: no
            !! The enumerator to represent the `NO`
    end type enum_read_asynchronous_list

    type(enum_read_asynchronous_list), public, parameter :: &
        read_asynchronous = &
            enum_read_asynchronous_list( &
                yes = enum_read_asynchronous(Read_Asynchronous_Yes, asynchronous(Read_Asynchronous_Yes)), &
                no  = enum_read_asynchronous(Read_Asynchronous_No , asynchronous(Read_Asynchronous_No)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `asynchronous` specifier.

    interface optval
        procedure :: optval_read_asynchronous
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_read_asynchronous(x, default) result(y)
        implicit none
        type(enum_read_asynchronous), intent(in), optional :: x
        type(enum_read_asynchronous), intent(in) :: default

        type(enum_read_asynchronous) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_read_asynchronous
end module enumul_read_asynchronous
