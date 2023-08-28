module enumul_open_asynchronous
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_asynchronous_default

    enum, bind(c)
        enumerator :: Open_Asynchronous_Undefined = 0
        enumerator :: Open_Asynchronous_Yes
        enumerator :: Open_Asynchronous_No
    end enum

    character(*), parameter, private :: asynchronous(0:*) = ["UNDEFINED ", &
                                                             "YES       ", &
                                                             "NO        "]
        !! The possible character expressions
        !! for the `asynchronous` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statment assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection.
        !!@endnote

    !>The enumerator for the `asynchronous` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_asynchronous
        character(len=len(asynchronous)), public :: expr
            !! An expression for the `asynchronous` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_asynchronous

    !>The possible expressions for the `asynchronous` specifier
    type :: enum_open_asynchronous_list
        type(enum_open_asynchronous), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_asynchronous), public :: yes
            !! The enumerator to represent the `YES`
        type(enum_open_asynchronous), public :: no
            !! The enumerator to represent the `NO`
    end type enum_open_asynchronous_list

    type(enum_open_asynchronous_list), public, parameter :: &
        open_asynchronous = &
            enum_open_asynchronous_list( &
                undefined = enum_open_asynchronous(Open_Asynchronous_Undefined, asynchronous(Open_Asynchronous_Undefined)), &
                yes       = enum_open_asynchronous(Open_Asynchronous_Yes, asynchronous(Open_Asynchronous_Yes)), &
                no        = enum_open_asynchronous(Open_Asynchronous_No , asynchronous(Open_Asynchronous_No)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `asynchronous` specifier.

    type(enum_open_asynchronous), public, parameter :: &
        default_open_asynchronous = open_asynchronous%no
        !! The default value of the `asynchronous` specifier.
        !! It is `"NO"`.

    interface optval
        procedure :: optval_open_asynchronous
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_asynchronous(x, default) result(y)
        implicit none
        type(enum_open_asynchronous), intent(in), optional :: x
        type(enum_open_asynchronous), intent(in) :: default

        type(enum_open_asynchronous) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_asynchronous

    !>Returns the enumerator representing the default
    !>character-expression for the `asynchronous` specifier
    !>in the `open` statement.
    !>The default value is `"NO"`.
    function get_open_asynchronous_default() result(default)
        implicit none
        type(enum_open_asynchronous) :: default
            !! The enumerator for default value of `asynchronous` specifier
            !! in `open`

        default = default_open_asynchronous
    end function get_open_asynchronous_default
end module enumul_open_asynchronous
