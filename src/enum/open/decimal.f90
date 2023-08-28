module enumul_open_decimal
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_decimal_default

    enum, bind(c)
        enumerator :: Open_Decimal_Undefined = 0
        enumerator :: Open_Decimal_Comma
        enumerator :: Open_Decimal_Point
    end enum

    character(*), parameter, private :: deciaml(0:*) = ["UNDEFINED", &
                                                        "COMMA    ", &
                                                        "POINT    "]
        !! The possible character expressions
        !! for the `decimal` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statment assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection.
        !!@endnote

    !>The enumerator for the `decimal` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_decimal
        character(len=len(deciaml)), public :: expr
            !! An expression for the `decimal` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_decimal

    !>The possible expressions for the `decimal` specifier
    type :: enum_open_decimal_list
        type(enum_open_decimal), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_decimal), public :: comma
            !! The enumerator to represent the `COMMA`
        type(enum_open_decimal), public :: point
            !! The enumerator to represent the `POINT`
    end type enum_open_decimal_list

    type(enum_open_decimal_list), public, parameter :: &
        open_decimal = &
            enum_open_decimal_list( &
                undefined  = enum_open_decimal(Open_Decimal_Undefined , deciaml(Open_Decimal_Undefined)), &
                comma      = enum_open_decimal(Open_Decimal_Comma     , deciaml(Open_Decimal_Comma)), &
                point      = enum_open_decimal(Open_Decimal_Point     , deciaml(Open_Decimal_Point)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `decimal` specifier.

    type(enum_open_decimal), public, parameter :: &
        default_open_decimal = open_decimal%point
        !! The default value of the `decimal` specifier.
        !! It is `"POINT"`.

    interface optval
        procedure :: optval_open_decimal
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_decimal(x, default) result(y)
        implicit none
        type(enum_open_decimal), intent(in), optional :: x
        type(enum_open_decimal), intent(in) :: default

        type(enum_open_decimal) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_decimal

    !>Returns the enumerator representing the default
    !>character-expression for the `decimal` specifier
    !>in the `open` statement.
    !>The default value is `"POINT"`.
    function get_open_decimal_default() result(default)
        implicit none
        type(enum_open_decimal) :: default
            !! The enumerator for default value of `decimal` specifier
            !! in `open`

        default = default_open_decimal
    end function get_open_decimal_default
end module enumul_open_decimal
