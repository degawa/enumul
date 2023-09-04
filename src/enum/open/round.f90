module enumul_open_round
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval
    public :: get_open_round_default

    enum, bind(c)
        enumerator :: Open_Round_Undefined = 0
        enumerator :: Open_Round_Up
        enumerator :: Open_Round_Down
        enumerator :: Open_Round_Zero
        enumerator :: Open_Round_Nearest
        enumerator :: Open_Round_Compatible
        enumerator :: Open_Round_ProcessorDefined
    end enum

    character(*), parameter, private :: round(0:*) = ["UNDEFINED        ", &
                                                      "UP               ", &
                                                      "DOWN             ", &
                                                      "ZERO             ", &
                                                      "NEAREST          ", &
                                                      "COMPATIBLE       ", &
                                                      "PROCESSOR_DEFINED"]
        !! The possible character expressions
        !! for the `round` specifier in the `open` statement.
        !!
        !!@note
        !! `"UNDEFINED"` is not the possible character-expression
        !! but the `inquire` statement assigns `"UNDEFINED"`
        !! to the specified variable if there is no connection or
        !! if the connection is not for formatted I/O.
        !!
        !! The `inquire` statement assigns `"PROCESSOR_DEFINED"`
        !! only if the behavior of the rounding mode is different from
        !! that of the UP, DOWN, ZERO, NEAREST, and COMPATIBLE.
        !!@endnote
        !!@warning
        !!Some version of gfortran compiler returns `"PROCESSOR_DEFINED"`
        !!even if the default rounding mode follows the ieee rounding mode
        !!@endwarning

    !>The enumerator for the `round` specifier
    !>and expression in the `open` statement.
    type, public, extends(enum_atype) :: enum_open_round
        character(len=len(round)), public :: expr
            !! An expression for the `round` specifier.
            !! Open to public to pass the `open` statement.
    end type enum_open_round

    !>The possible expressions for the `round` specifier
    type :: enum_open_round_list
        type(enum_open_round), public :: undefined
            !! The enumerator to represent the `UNDEFINED`
        type(enum_open_round), public :: up
            !! The enumerator to represent the `UP`
        type(enum_open_round), public :: down
            !! The enumerator to represent the `DOWN`
        type(enum_open_round), public :: zero
            !! The enumerator to represent the `ZERO`
        type(enum_open_round), public :: nearest
            !! The enumerator to represent the `NEAREST`
        type(enum_open_round), public :: compatible
            !! The enumerator to represent the `COMPATIBLE`
        type(enum_open_round), public :: processor_defined
            !! The enumerator to represent the `PROCESSOR_DEFINED`
    end type enum_open_round_list

    type(enum_open_round_list), public, parameter :: &
        open_round = &
            enum_open_round_list( &
                undefined         = enum_open_round(Open_Round_Undefined       , round(Open_Round_Undefined)), &
                up                = enum_open_round(Open_Round_Up              , round(Open_Round_Up)), &
                down              = enum_open_round(Open_Round_Down            , round(Open_Round_Down)), &
                zero              = enum_open_round(Open_Round_Zero            , round(Open_Round_Zero)), &
                nearest           = enum_open_round(Open_Round_Nearest         , round(Open_Round_Nearest)), &
                compatible        = enum_open_round(Open_Round_Compatible      , round(Open_Round_Compatible)), &
                processor_defined = enum_open_round(Open_Round_ProcessorDefined, round(Open_Round_ProcessorDefined)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `round` specifier.

    interface optval
        procedure :: optval_open_round
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_open_round(x, default) result(y)
        implicit none
        type(enum_open_round), intent(in), optional :: x
        type(enum_open_round), intent(in) :: default

        type(enum_open_round) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_open_round

    !>Returns the enumerator representing the default
    !>character-expression for the `round` specifier
    !>in the `open` statement.
    !>The default value is compiler- or environment-dependent.
    function get_open_round_default() result(default)
        use, intrinsic :: iso_fortran_env
        use :: enumul_open_status
        implicit none
        type(enum_open_round) :: default
            !! The enumerator for default value of `round` specifier
            !! in `open`

        integer(int32) :: unit
        character(len=len(round)) :: default_round_expr

        open (newunit=unit, status=open_status%scratch%expr)
        inquire (unit, round=default_round_expr)
        close (unit)

        select case (default_round_expr)
        case (open_round%up%expr)
            default = open_round%up

        case (open_round%down%expr)
            default = open_round%down

        case (open_round%zero%expr)
            default = open_round%zero

        case (open_round%nearest%expr)
            default = open_round%nearest

        case (open_round%compatible%expr)
            default = open_round%compatible

        case (open_round%processor_defined%expr)
            default = open_round%processor_defined

        case default
            default = open_round%undefined
        end select
    end function get_open_round_default
end module enumul_open_round
