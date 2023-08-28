module enumul_open_encoding
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_encoding_default

    enum, bind(c)
        enumerator :: Open_Encoding_UNKNOWN = -1
        enumerator :: Open_Encoding_UNDEFINED
        enumerator :: Open_Encoding_UTF8
        enumerator :: Open_Encoding_Default
    end enum

    character(*), parameter, private :: encoding(-1:*) = ["UNKNOWN  ", &
                                                          "UNDEFINED", &
                                                          "UTF-8    ", &
                                                          "DEFAULT  "]
        !! The possible character expressions
        !! for the `encoding` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` and `"UNKNOWN"` are not
        !! the possible character-expressions.
        !! But the `inquire` statment assigns `"UNDEFINED"`
        !! to the specified variable if the connection for
        !! formatted I/O with an encoding form of UTF-8.
        !! The `inquire` statment assigns `"UNKNOWN"`
        !! to the specified variable if the processor is unable to
        !! determine the encoding form of the file or
        !! the unit is not conneted to a file.
        !!@endnote

    !>The enumerator for the `encoding` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_encoding
        character(len=len(encoding)), public :: expr
            !! An expression for the `encoding` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_encoding

    type :: enum_open_encoding_list
        type(enum_open_encoding), public :: unknown
            !! The enumerator to represent the `UNKNOWN`
        type(enum_open_encoding), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_encoding), public :: utf8
            !! The enumerator to represent the `UTF-8`
        type(enum_open_encoding), public :: default
            !! The enumerator to represent the `DEFAULT`
    end type enum_open_encoding_list

    type(enum_open_encoding_list), public, parameter :: &
        open_encoding = &
            enum_open_encoding_list( &
                unknown   = enum_open_encoding(Open_Encoding_UNKNOWN,   encoding(Open_Encoding_UNKNOWN)), &
                undefined = enum_open_encoding(Open_Encoding_UNDEFINED, encoding(Open_Encoding_UNDEFINED)), &
                utf8      = enum_open_encoding(Open_Encoding_UTF8,      encoding(Open_Encoding_UTF8)), &
                default   = enum_open_encoding(Open_Encoding_Default,   encoding(Open_Encoding_Default)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `encoding` specifier.

    type(enum_open_encoding), public, parameter :: &
        default_open_encoding = open_encoding%default
        !! The default value of the `encoding` specifier.
        !! It is `"DEFAULT"`.

    interface optval
        procedure :: optval_open_encoding
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_encoding(x, default) result(y)
        implicit none
        type(enum_open_encoding), intent(in), optional :: x
        type(enum_open_encoding), intent(in) :: default

        type(enum_open_encoding) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_encoding

    !>Returns the enumerator representing the default
    !>character-expression for the `encoding` specifier
    !>in the `open` statement.
    !>The default value is `"DEFAULT"`.
    function get_open_encoding_default() result(default)
        implicit none
        type(enum_open_encoding) :: default
            !! The enumerator for default value of `encoding` specifier
            !! in `open`

        default = default_open_encoding
    end function get_open_encoding_default
end module enumul_open_encoding
