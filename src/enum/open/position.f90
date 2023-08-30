module enumul_open_position
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_position_default

    enum, bind(c)
        enumerator :: Open_Position_Undefined = 0
        enumerator :: Open_Position_Asis
        enumerator :: Open_Position_Rewind
        enumerator :: Open_Position_Append
    end enum

    character(*), parameter, private :: position(0:*) = ["UNDEFINED", &
                                                         "ASIS     ", &
                                                         "REWIND   ", &
                                                         "APPEND   "]
        !! The possible character expressions
        !! for the `position` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statement assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection or
        !! the file is connected for direct position.
        !!@endnote

    !>The enumerator for the `position` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_position
        character(len=len(position)), public :: expr
            !! An expression for the `position` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_position

    !>The possible expressions for the `position` specifier
    type :: enum_open_position_list
        type(enum_open_position), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_position), public :: asis
            !! The enumerator to represent the `ASIS`
        type(enum_open_position), public :: rewind
            !! The enumerator to represent the `REWIND`
        type(enum_open_position), public :: append
            !! The enumerator to represent the `APPEND`
    end type enum_open_position_list

    type(enum_open_position_list), public, parameter :: &
        open_position = &
            enum_open_position_list( &
                undefined = enum_open_position(Open_Position_Undefined, position(Open_Position_Undefined)), &
                asis      = enum_open_position(Open_Position_Asis     , position(Open_Position_Asis)), &
                rewind    = enum_open_position(Open_Position_Rewind   , position(Open_Position_Rewind)), &
                append    = enum_open_position(Open_Position_Append   , position(Open_Position_Append)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `position` specifier.

    type(enum_open_position), public, parameter :: &
        default_open_position = open_position%asis
        !! The default value of the `position` specifier.
        !! It is `"ASIA"`.

    interface optval
        procedure :: optval_open_position
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_position(x, default) result(y)
        implicit none
        type(enum_open_position), intent(in), optional :: x
        type(enum_open_position), intent(in) :: default

        type(enum_open_position) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_position

    !>Returns the enumerator representing the default
    !>character-expression for the `position` specifier
    !>in the `open` statement.
    !>The default value is `"ASIS"`.
    function get_open_position_default() result(default)
        implicit none
        type(enum_open_position) :: default
            !! The enumerator for default value of `position` specifier
            !! in `open`

        default = default_open_position
    end function get_open_position_default
end module enumul_open_position
