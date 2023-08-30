module enumul_open_pad
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_pad_default

    enum, bind(c)
        enumerator :: Open_Pad_Undefined = 0
        enumerator :: Open_Pad_Yes
        enumerator :: Open_Pad_No
    end enum

    character(*), parameter, private :: pad(0:*) = ["UNDEFINED", &
                                                    "YES      ", &
                                                    "NO       "]
        !! The possible character expressions
        !! for the `pad` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statement assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection.
        !!@endnote

    !>The enumerator for the `pad` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_pad
        character(len=len(pad)), public :: expr
            !! An expression for the `pad` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_pad

    type :: enum_open_pad_list
        type(enum_open_pad), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_pad), public :: yes
            !! The enumerator to represent the `YES`
        type(enum_open_pad), public :: no
            !! The enumerator to represent the `NO`
    end type enum_open_pad_list

    type(enum_open_pad_list), public, parameter :: &
        open_pad = &
            enum_open_pad_list( &
                undefined = enum_open_pad(Open_Pad_Undefined, pad(Open_Pad_Undefined)), &
                yes       = enum_open_pad(Open_Pad_Yes      , pad(Open_Pad_Yes)), &
                no        = enum_open_pad(Open_Pad_No       , pad(Open_Pad_No)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `pad` specifier.

    type(enum_open_pad), public, parameter :: &
        default_open_pad = open_pad%yes
        !! The default value of the `pad` specifier.
        !! It is `"YES"`.

    interface optval
        procedure :: optval_open_pad
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_pad(x, default) result(y)
        implicit none
        type(enum_open_pad), intent(in), optional :: x
        type(enum_open_pad), intent(in) :: default

        type(enum_open_pad) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_pad

    !>Returns the enumerator representing the default
    !>character-expression for the `pad` specifier
    !>in the `open` statement.
    !>The default value is `"YES"`.
    function get_open_pad_default() result(default)
        implicit none
        type(enum_open_pad) :: default
            !! The enumerator for default value of `pad` specifier
            !! in `open`

        default = default_open_pad
    end function get_open_pad_default
end module enumul_open_pad
