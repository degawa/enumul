module enumul_open_sign
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_sign_default

    enum, bind(c)
        enumerator :: Open_Sign_Undefined = 0
        enumerator :: Open_Sign_Plus
        enumerator :: Open_Sign_Suppress
        enumerator :: Open_Sign_ProcessorDefined
    end enum

    character(*), parameter, private :: sign(0:*) = ["UNDEFINED        ", &
                                                     "PLUS             ", &
                                                     "SUPPRESS         ", &
                                                     "PROCESSOR_DEFINED"]
        !! The possible character expressions
        !! for the `sign` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statment assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection or
        !! if the connection is not for formatted I/O.
        !!@endnote

    !>The enumerator for the `sign` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_sign
        character(len=len(sign)), public :: expr
            !! An expression for the `sign` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_sign

    !>The possible expressions for the `sign` specifier
    type :: enum_open_sign_list
        type(enum_open_sign), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_sign), public :: plus
            !! The enumerator to represent the `PLUS`
        type(enum_open_sign), public :: suppress
            !! The enumerator to represent the `SUPPRESS`
        type(enum_open_sign), public :: processor_defined
            !! The enumerator to represent the `PROCESSOR_DEPENDENT`
    end type enum_open_sign_list

    type(enum_open_sign_list), public, parameter :: &
        open_sign = &
            enum_open_sign_list( &
                undefined         = enum_open_sign(Open_Sign_Undefined       , sign(Open_Sign_Undefined)), &
                plus              = enum_open_sign(Open_Sign_Plus            , sign(Open_Sign_Plus)), &
                suppress          = enum_open_sign(Open_Sign_Suppress        , sign(Open_Sign_Suppress)), &
                processor_defined = enum_open_sign(Open_Sign_ProcessorDefined, sign(Open_Sign_ProcessorDefined)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `sign` specifier.

    type(enum_open_sign), public, parameter :: &
        default_open_sign = open_sign%processor_defined
        !! The default value of the `sign` specifier.
        !! It is `"PROCESSOR_DEFINED"`.

    interface optval
        procedure :: optval_open_sign
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_sign(x, default) result(y)
        implicit none
        type(enum_open_sign), intent(in), optional :: x
        type(enum_open_sign), intent(in) :: default

        type(enum_open_sign) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_sign

    !>Returns the enumerator representing the default
    !>character-expression for the `sign` specifier
    !>in the `open` statement.
    !>The default value is `"PROCESSOR_DEFINED"`.
    function get_open_sign_default() result(default)
        implicit none
        type(enum_open_sign) :: default
            !! The enumerator for default value of `sign` specifier
            !! in `open`

        default = default_open_sign
    end function get_open_sign_default
end module enumul_open_sign
