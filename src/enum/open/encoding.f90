module enumul_open_encoding
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_encoding_default

    enum, bind(c)
        enumerator :: Open_Encoding_Default = 0
        enumerator :: Open_Encoding_UTF8
        enumerator :: Open_Encoding_UNKNOWN
        enumerator :: Open_Encoding_UNDEFINED
    end enum

    character(*), parameter, private :: encoding(0:*) = ["DEFAULT  ", &
                                                         "UTF-8    ", &
                                                         "UNKNOWN  ", &
                                                         "UNDEFINED"]
        !! The possible character expressions
        !! for the `encoding` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` and `"UNKNOWN"` are not
        !! the possible character-expressions.
        !! But the `inquire` statement assigns `"UNDEFINED"`
        !! to the specified variable if the connection is for
        !! unformatted I/O.
        !! The `inquire` statement assigns `"UNKNOWN"`
        !! to the specified variable if the processor is unable to
        !! determine the encoding form of the file or
        !! the unit is not conneted to a file.
        !!
        !! If there is no connection to a unit, the result of
        !! the `inquire` is compiler-dependent.
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
                default   = enum_open_encoding(Open_Encoding_Default,   encoding(Open_Encoding_Default)), &
                utf8      = enum_open_encoding(Open_Encoding_UTF8,      encoding(Open_Encoding_UTF8)), &
                unknown   = enum_open_encoding(Open_Encoding_UNKNOWN,   encoding(Open_Encoding_UNKNOWN)), &
                undefined = enum_open_encoding(Open_Encoding_UNDEFINED, encoding(Open_Encoding_UNDEFINED)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `encoding` specifier.

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
    function get_open_encoding_default(form, access) result(default)
        use, intrinsic :: iso_fortran_env
        use :: enumul_open_form
        use :: enumul_open_access
        use :: enumul_open_status
        implicit none
        type(enum_open_form), intent(in), optional :: form
            !! FORM specifier determining the file format
        type(enum_open_access), intent(in), optional :: access
            !! ACCESS specifier specifing the access method
        type(enum_open_encoding) :: default
            !! The enumerator for default value of `encoding` specifier
            !! in `open`

        type(enum_open_form) :: form_
        character(32) :: encoding_expr
        integer(int32) :: unit

        form_ = optval(form, get_open_form_default(access))

        open (newunit=unit, status=open_status%scratch%expr, form=form_%expr)
        inquire (unit, encoding=encoding_expr)
        close (unit)

        select case (encoding_expr)
        case (open_encoding%utf8%expr)
            default = open_encoding%utf8

        case (open_encoding%unknown%expr)
            default = open_encoding%unknown

        case (open_encoding%undefined%expr)
            default = open_encoding%undefined

        case default
            default = open_encoding%undefined
        end select
    end function get_open_encoding_default
end module enumul_open_encoding
