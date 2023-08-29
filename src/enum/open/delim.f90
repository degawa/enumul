module enumul_open_delim
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_delim_default

    enum, bind(c)
        enumerator :: Open_Delim_Undefined = 0
        enumerator :: Open_Delim_Apostrophe
        enumerator :: Open_Delim_Quote
        enumerator :: Open_Delim_None
    end enum

    character(*), parameter, private :: delim(0:*) = ["UNDEFINED ", &
                                                      "APOSTROPHE", &
                                                      "QUOTE     ", &
                                                      "NONE      "]
        !! The possible character expressions
        !! for the `delim` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statment assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection.
        !!@endnote

    !>The enumerator for the `delim` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_delim
        character(len=len(delim)), public :: expr
            !! An expression for the `delim` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_delim

    !>The possible expressions for the `delim` specifier
    type :: enum_open_delim_list
        type(enum_open_delim), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_delim), public :: apostrophe
            !! The enumerator to represent the `APOSTROPHE`
        type(enum_open_delim), public :: quote
            !! The enumerator to represent the `QUOTE`
        type(enum_open_delim), public :: none
            !! The enumerator to represent the `NONE`
    end type enum_open_delim_list

    type(enum_open_delim_list), public, parameter :: &
        open_delim = &
            enum_open_delim_list( &
                undefined  = enum_open_delim(Open_Delim_Undefined , delim(Open_Delim_Undefined)), &
                apostrophe = enum_open_delim(Open_Delim_Apostrophe, delim(Open_Delim_Apostrophe)), &
                quote      = enum_open_delim(Open_Delim_Quote, delim(Open_Delim_Quote)), &
                none       = enum_open_delim(Open_Delim_None, delim(Open_Delim_None)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `delim` specifier.

    type(enum_open_delim), public, parameter :: &
        default_open_delim = open_delim%none
        !! The default value of the `delim` specifier.
        !! It is `"NONE"`.

    interface optval
        procedure :: optval_open_delim
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_delim(x, default) result(y)
        implicit none
        type(enum_open_delim), intent(in), optional :: x
        type(enum_open_delim), intent(in) :: default

        type(enum_open_delim) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_delim

    !>Returns the enumerator representing the default
    !>character-expression for the `delim` specifier
    !>in the `open` statement.
    !>The default value is `"NONE"`.
    function get_open_delim_default() result(default)
        implicit none
        type(enum_open_delim) :: default
            !! The enumerator for default value of `delim` specifier
            !! in `open`

        default = default_open_delim
    end function get_open_delim_default
end module enumul_open_delim
