module enumul_read_round
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Read_Round_Up = 1
        enumerator :: Read_Round_Down
        enumerator :: Read_Round_Zero
        enumerator :: Read_Round_Nearest
        enumerator :: Read_Round_Compatible
        enumerator :: Read_Round_ProcessorDefined
    end enum

    character(*), parameter, private :: round(*) = ["UP               ", &
                                                    "DOWN             ", &
                                                    "ZERO             ", &
                                                    "NEAREST          ", &
                                                    "COMPATIBLE       ", &
                                                    "PROCESSOR_DEFINED"]
        !! The possible character expressions
        !! for the `round` specifier in the `read` statement.

    !>The enumerator for the `round` specifier
    !>and expression in the `read` statement.
    type, public, extends(enum_atype) :: enum_read_round
        character(len=len(round)), public :: expr
            !! An expression for the `round` specifier.
            !! Open to public to pass the `read` statement.
    end type enum_read_round

    !>The possible expressions for the `round` specifier
    type :: enum_read_round_list
        type(enum_read_round), public :: up
            !! The enumerator to represent the `UP`
        type(enum_read_round), public :: down
            !! The enumerator to represent the `DOWN`
        type(enum_read_round), public :: zero
            !! The enumerator to represent the `ZERO`
        type(enum_read_round), public :: nearest
            !! The enumerator to represent the `NEAREST`
        type(enum_read_round), public :: compatible
            !! The enumerator to represent the `COMPATIBLE`
        type(enum_read_round), public :: processor_defined
            !! The enumerator to represent the `PROCESSOR_DEFINED`
    end type enum_read_round_list

    type(enum_read_round_list), public, parameter :: &
        read_round = &
            enum_read_round_list( &
                up                = enum_read_round(Read_Round_Up              , round(Read_Round_Up)), &
                down              = enum_read_round(Read_Round_Down            , round(Read_Round_Down)), &
                zero              = enum_read_round(Read_Round_Zero            , round(Read_Round_Zero)), &
                nearest           = enum_read_round(Read_Round_Nearest         , round(Read_Round_Nearest)), &
                compatible        = enum_read_round(Read_Round_Compatible      , round(Read_Round_Compatible)), &
                processor_defined = enum_read_round(Read_Round_ProcessorDefined, round(Read_Round_ProcessorDefined)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `round` specifier.

    interface optval
        procedure :: optval_read_round
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_read_round(x, default) result(y)
        implicit none
        type(enum_read_round), intent(in), optional :: x
        type(enum_read_round), intent(in) :: default

        type(enum_read_round) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_read_round
end module enumul_read_round
