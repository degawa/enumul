module enumul_write_delim
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Write_Delim_Apostrophe = 1
        enumerator :: Write_Delim_Quote
        enumerator :: Write_Delim_None
    end enum

    character(*), parameter, private :: delim(*) = ["APOSTROPHE", &
                                                    "QUOTE     ", &
                                                    "NONE      "]
        !! The possible character expressions
        !! for the `delim` specifier in the `write` statement.

    !>The enumerator for the `delim` specifier
    !>and expression in the `write` statement.
    type, public, extends(enum_atype) :: enum_write_delim
        character(len=len(delim)), public :: expr
            !! An expression for the `delim` specifier.
            !! Open to public to pass the `write` statement.
    end type enum_write_delim

    !>The possible expressions for the `delim` specifier
    type :: enum_write_delim_list
        type(enum_write_delim), public :: apostrophe
            !! The enumerator to represent the `APOSTROPHE`
        type(enum_write_delim), public :: quote
            !! The enumerator to represent the `QUOTE`
        type(enum_write_delim), public :: none
            !! The enumerator to represent the `NONE`
    end type enum_write_delim_list

    type(enum_write_delim_list), public, parameter :: &
        write_delim = &
            enum_write_delim_list( &
                apostrophe = enum_write_delim(Write_Delim_Apostrophe, delim(Write_Delim_Apostrophe)), &
                quote      = enum_write_delim(Write_Delim_Quote, delim(Write_Delim_Quote)), &
                none       = enum_write_delim(Write_Delim_None, delim(Write_Delim_None)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `delim` specifier.

    interface optval
        procedure :: optval_write_delim
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_write_delim(x, default) result(y)
        implicit none
        type(enum_write_delim), intent(in), optional :: x
        type(enum_write_delim), intent(in) :: default

        type(enum_write_delim) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_write_delim
end module enumul_write_delim
