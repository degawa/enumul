module enumul_close_status
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_close_status_default

    enum, bind(c)
        enumerator :: Close_Status_Keep = 1
        enumerator :: Close_Status_Delete
    end enum

    character(*), parameter, private :: status(*) = ["KEEP  ", &
                                                     "DELETE"]
        !! The possible character expressions
        !! for the `status` specifier in the `close` statement.

    !>The enumerator for the `status` specifier
    !>and expression in the `close` statement.
    type, public, extends(enum_atype) :: enum_close_status
        character(len=len(status)), public :: expr
            !! An expression for the `status` specifier.
            !! Open to public to pass the `close` statement.
    end type enum_close_status

    !>The possible expressions for the `status` specifier
    type :: enum_close_status_list
        type(enum_close_status), public :: keep
            !! The enumerator to represent the `KEEP`
        type(enum_close_status), public :: delete
            !! The enumerator to represent the `DELETE`
    end type enum_close_status_list

    type(enum_close_status_list), public, parameter :: &
        close_status = &
            enum_close_status_list( &
                keep   = enum_close_status(Close_Status_Keep  , status(Close_Status_Keep)), &
                delete = enum_close_status(Close_Status_Delete, status(Close_Status_Delete)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `status` specifier.

    type(enum_close_status), public, parameter :: &
        default_close_status = close_status%keep
        !! The default value of the `status` specifier.
        !! It is `"DELETE"`.

    interface optval
        procedure :: optval_close_status
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_close_status(x, default) result(y)
        implicit none
        type(enum_close_status), intent(in), optional :: x
        type(enum_close_status), intent(in) :: default

        type(enum_close_status) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_close_status

    !>Returns the enumerator representing the default
    !>character-expression for the `status` specifier
    !>in the `close` statement.
    !>The default value is `"DELETE"`.
    function get_close_status_default() result(default)
        implicit none
        type(enum_close_status) :: default
            !! The enumerator for default value of `status` specifier
            !! in `close`

        default = default_close_status
    end function get_close_status_default
end module enumul_close_status
