module enumul_read_pad
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Read_Pad_Yes = 1
        enumerator :: Read_Pad_No
    end enum

    character(*), parameter, private :: pad(*) = ["YES", &
                                                  "NO "]
        !! The possible character expressions
        !! for the `pad` specifier in the `read` statement.

    !>The enumerator for the `pad` specifier
    !>and expression in the `read` statement.
    type, public, extends(enum_atype) :: enum_read_pad
        character(len=len(pad)), public :: expr
            !! An expression for the `pad` specifier.
            !! Open to public to pass the `read` statement.
    end type enum_read_pad

    !>The possible expressions for the `pad` specifier
    type :: enum_read_pad_list
        type(enum_read_pad), public :: yes
            !! The enumerator to represent the `YES`
        type(enum_read_pad), public :: no
            !! The enumerator to represent the `NO`
    end type enum_read_pad_list

    type(enum_read_pad_list), public, parameter :: &
        read_pad = &
            enum_read_pad_list( &
                yes = enum_read_pad(Read_Pad_Yes, pad(Read_Pad_Yes)), &
                no  = enum_read_pad(Read_Pad_No , pad(Read_Pad_No)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `pad` specifier.

    interface optval
        procedure :: optval_read_pad
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_read_pad(x, default) result(y)
        implicit none
        type(enum_read_pad), intent(in), optional :: x
        type(enum_read_pad), intent(in) :: default

        type(enum_read_pad) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_read_pad
end module enumul_read_pad
