module enumul_open_action
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_action_default

    enum, bind(c)
        enumerator :: Open_Action_Undefined = 0
        enumerator :: Open_Action_Read
        enumerator :: Open_Action_Write
        enumerator :: Open_Action_ReadWrite
    end enum

    character(*), parameter, private :: action(0:*) = ["UNDEFINED", &
                                                       "READ     ", &
                                                       "WRITE    ", &
                                                       "READWRITE"]
        !! The possible character expressions
        !! for the `action` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statement assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection.
        !!@endnote

    !>The enumerator for the `action` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_action
        character(len=len(action)), public :: expr
            !! An expression for the `action` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_action

    !>The possible expressions for the `action` specifier
    type :: enum_open_action_list
        type(enum_open_action), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_action), public :: read
            !! The enumerator to represent the `READ`
        type(enum_open_action), public :: write
            !! The enumerator to represent the `WRITE`
        type(enum_open_action), public :: readwrite
            !! The enumerator to represent the `READWRITE`
    end type enum_open_action_list

    type(enum_open_action_list), public, parameter :: &
        open_action = &
            enum_open_action_list( &
                undefined = enum_open_action(Open_Action_Undefined, action(Open_Action_Undefined)), &
                read      = enum_open_action(Open_Action_Read     , action(Open_Action_Read)), &
                write     = enum_open_action(Open_Action_Write    , action(Open_Action_Write)), &
                readwrite = enum_open_action(Open_Action_ReadWrite, action(Open_Action_ReadWrite)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `action` specifier.

    interface optval
        procedure :: optval_open_action
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_action(x, default) result(y)
        implicit none
        type(enum_open_action), intent(in), optional :: x
        type(enum_open_action), intent(in) :: default

        type(enum_open_action) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_action

    !>Returns the enumerator representing the default
    !>character-expression for the `action` specifier
    !>in the `open` statement.
    !>The default value is compiler-dependent.
    function get_open_action_default() result(default)
        use, intrinsic :: iso_fortran_env
        use :: enumul_open_status
        implicit none
        type(enum_open_action) :: default
            !! The enumerator for default value of `action` specifier
            !! in `open`

        integer(int32) :: unit
        character(len=len(action)) :: action_expr

        ! open scratch file without action specifier
        ! to get compiler- or environment-dependent
        ! default value
        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, action=action_expr)
        close (unit)

        select case (action_expr)
        case (open_action%read%expr)
            default = open_action%read

        case (open_action%write%expr)
            default = open_action%write

        case (open_action%readwrite%expr)
            default = open_action%readwrite

        case default
            default = open_action%undefined
        end select
    end function get_open_action_default
end module enumul_open_action
