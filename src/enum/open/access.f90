module enumul_open_access
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_access_default_expr

    enum, bind(c)
        enumerator :: Open_Access_Sequential = 1
        enumerator :: Open_Access_Direct
        enumerator :: Open_Access_Stream
    end enum

    character(*), parameter, private :: access(*) = ["SEQUENTIAL", &
                                                     "DIRECT    ", &
                                                     "STREAM    "]
        !! The possible character expressions
        !! for the `access` specifier in the `open` statement.

    !>The enumerator for the `access` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_access
        character(len=len(access)), public :: expr
            !! An expression for the `access` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_access

    !>The possible expressions for the `access` specifier
    type :: enum_open_access_list
        type(enum_open_access), public :: sequential
            !! The enumerator to represent the `SEQUENTIAL`
        type(enum_open_access), public :: direct
            !! The enumerator to represent the `DIRECT`
        type(enum_open_access), public :: stream
            !! The enumerator to represent the `STREAM`
    end type enum_open_access_list

    type(enum_open_access_list), public, parameter :: &
        open_access = &
            enum_open_access_list( &
                sequential = enum_open_access(Open_Access_Sequential, access(Open_Access_Sequential)), &
                direct     = enum_open_access(Open_Access_Direct    , access(Open_Access_Direct)), &
                stream     = enum_open_access(Open_Access_Stream    , access(Open_Access_Stream)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `access` specifier.

    type(enum_open_access), public, parameter :: &
        default_open_access = open_access%sequential
        !! The default value of the `access` specifier.
        !! It is `"SEQUENTIAL"`.

    interface optval
        procedure :: optval_open_access
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_access(x, default) result(y)
        implicit none
        type(enum_open_access), intent(in), optional :: x
        type(enum_open_access), intent(in) :: default

        type(enum_open_access) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_access

    !>Returns the enumerator representing the default
    !>character-expression for the `access` specifier
    !>in the `open` statement.
    !>The default value is `"SEQUENTIAL"`.
    function get_open_access_default_expr() result(default)
        implicit none
        type(enum_open_access) :: default
            !! The enumerator for default value of `access` specifier
            !! in `open`

        default = default_open_access
    end function get_open_access_default_expr
end module enumul_open_access
