module enumul_open_blank
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_blank_default

    enum, bind(c)
        enumerator :: Open_Blank_Undefined = 0
        enumerator :: Open_Blank_Null
        enumerator :: Open_Blank_Zero
    end enum

    character(*), parameter, private :: blank(0:*) = ["UNDEFINED", &
                                                      "NULL     ", &
                                                      "ZERO     "]
        !! The possible character expressions
        !! for the `blank` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statment assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection.
        !!@endnote

    !>The enumerator for the `blank` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_blank
        character(len=len(blank)), public :: expr
            !! An expression for the `blank` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_blank

    !>The possible expressions for the `blank` specifier
    type :: enum_open_blank_list
        type(enum_open_blank), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_blank), public :: null
            !! The enumerator to represent the `NULL`
        type(enum_open_blank), public :: zero
            !! The enumerator to represent the `ZEOR`
    end type enum_open_blank_list

    type(enum_open_blank_list), public, parameter :: &
        open_blank = &
            enum_open_blank_list( &
                undefined = enum_open_blank(Open_Blank_Undefined, blank(Open_Blank_Undefined)), &
                null      = enum_open_blank(Open_Blank_Null     , blank(Open_Blank_Null)), &
                zero      = enum_open_blank(Open_Blank_Zero     , blank(Open_Blank_Zero)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `blank` specifier.

    type(enum_open_blank), public, parameter :: &
        default_open_blank = open_blank%null
        !! The default value of the `blank` specifier.
        !! It is `"NULL"`.

    interface optval
        procedure :: optval_open_blank
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_blank(x, default) result(y)
        implicit none
        type(enum_open_blank), intent(in), optional :: x
        type(enum_open_blank), intent(in) :: default

        type(enum_open_blank) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_blank

    !>Returns the enumerator representing the default
    !>character-expression for the `blank` specifier
    !>in the `open` statement.
    !>The default value is `"NULL"`.
    function get_open_blank_default() result(default)
        implicit none
        type(enum_open_blank) :: default
            !! The enumerator for default value of `blank` specifier
            !! in `open`

        default = default_open_blank
    end function get_open_blank_default
end module enumul_open_blank
