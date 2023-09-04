module enumul_open_form
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_form_default

    enum, bind(c)
        enumerator :: Open_Form_Undefined = 0
        enumerator :: Open_Form_Formatted
        enumerator :: Open_Form_Unformatted
    end enum

    character(*), parameter, private :: form(0:*) = ["UNDEFINED  ", &
                                                     "FORMATTED  ", &
                                                     "UNFORMATTED"]
        !! The possible character expressions
        !! for the `form` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statement assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection.
        !!@endnote

    !>The enumerator for the `form` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_form
        character(len=len(form)), public :: expr
            !! An expression for the `form` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_form

    !>The possible expressions for the `form` specifier
    type :: enum_open_form_list
        type(enum_open_form), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_form), public :: formatted
            !! The enumerator to represent the `FORMATTED`
        type(enum_open_form), public :: unformatted
            !! The enumerator to represent the `UNFORMATTED`
    end type enum_open_form_list

    type(enum_open_form_list), public, parameter :: &
        open_form = &
            enum_open_form_list( &
                undefined   = enum_open_form(Open_Form_Undefined  , form(Open_Form_Undefined)), &
                formatted   = enum_open_form(Open_Form_Formatted  , form(Open_Form_Formatted)), &
                unformatted = enum_open_form(Open_Form_Unformatted, form(Open_Form_Unformatted)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `form` specifier.

    interface optval
        procedure :: optval_open_form
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_form(x, default) result(y)
        implicit none
        type(enum_open_form), intent(in), optional :: x
        type(enum_open_form), intent(in) :: default

        type(enum_open_form) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_form

    !>Returns the enumerator representing the default
    !>character-expression for the `form` specifier
    !>in the `open` statement.
    !>
    !>The default value is `"UNFORMATTED"` if the file is
    !>being connected for direct or stream access,
    !>and is `"FORMATTED"` for sequential access.
    function get_open_form_default(access) result(default)
        use :: enumul_open_access
        implicit none
        type(enum_open_access), intent(in), optional :: access
            !! ACCESS specifier specifing the access method
        type(enum_open_form) :: default
            !! The enumerator for default value of `form` specifier
            !! in `open`

        type(enum_open_access) :: access_
        access_ = optval(access, default_open_access)

        select case (access_%expr)
        case (open_access%direct%expr, open_access%stream%expr)
            default = open_form%unformatted

        case (open_access%sequential%expr)
            default = open_form%formatted

        case default
            default = open_form%undefined
        end select
    end function get_open_form_default
end module enumul_open_form
