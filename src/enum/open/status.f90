module enumul_open_status
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_status_default

    enum, bind(c)
        enumerator :: Open_Round_Old = 1
        enumerator :: Open_Round_New
        enumerator :: Open_Round_Scratch
        enumerator :: Open_Round_Replace
        enumerator :: Open_Round_Unknown
    end enum

    character(*), parameter, private :: status(*) = ["OLD    ", &
                                                     "NEW    ", &
                                                     "SCRATCH", &
                                                     "REPLACE", &
                                                     "UNKNOWN"]
        !! The possible character expressions
        !! for the `status` specifier in the `open` statement.

    !>The enumerator for the `status` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_status
        character(len=len(status)), public :: expr
            !! An expression for the `status` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_status

    !>The possible expressions for the `status` specifier
    type :: enum_open_status_list
        type(enum_open_status), public :: old
            !! The enumerator to represent the `OLD`
        type(enum_open_status), public :: new
            !! The enumerator to represent the `NEW`
        type(enum_open_status), public :: scratch
            !! The enumerator to represent the `SCRATCH`
        type(enum_open_status), public :: replace
            !! The enumerator to represent the `REPLACE`
        type(enum_open_status), public :: unknown
            !! The enumerator to represent the `UNKNOWN`
    end type enum_open_status_list

    type(enum_open_status_list), public, parameter :: &
        open_status = &
            enum_open_status_list( &
                old     = enum_open_status(Open_Round_Old    , status(Open_Round_Old)), &
                new     = enum_open_status(Open_Round_New    , status(Open_Round_New)), &
                scratch = enum_open_status(Open_Round_Scratch, status(Open_Round_Scratch)), &
                replace = enum_open_status(Open_Round_Replace, status(Open_Round_Replace)), &
                unknown = enum_open_status(Open_Round_Unknown, status(Open_Round_Unknown)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `status` specifier.

    type(enum_open_status), public, parameter :: &
        default_open_status = open_status%unknown
        !! The default value of the `status` specifier.
        !! It is `"UNKNOWN"`.

    interface optval
        procedure :: optval_open_status
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_status(x, default) result(y)
        implicit none
        type(enum_open_status), intent(in), optional :: x
        type(enum_open_status), intent(in) :: default

        type(enum_open_status) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_status

    !>Returns the enumerator representing the default
    !>character-expression for the `status` specifier
    !>in the `open` statement.
    !>The default value is `"UNKNOWN"`.
    function get_open_status_default() result(default)
        implicit none
        type(enum_open_status) :: default
            !! The enumerator for default value of `status` specifier
            !! in `open`

        default = default_open_status
    end function get_open_status_default
end module enumul_open_status
