module enumul_write_round
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Write_Round_Up = 1
        enumerator :: Write_Round_Down
        enumerator :: Write_Round_Zero
        enumerator :: Write_Round_Nearest
        enumerator :: Write_Round_Compatible
        enumerator :: Write_Round_ProcessorDefined
    end enum

    character(*), parameter, private :: round(*) = ["UP               ", &
                                                    "DOWN             ", &
                                                    "ZERO             ", &
                                                    "NEAREST          ", &
                                                    "COMPATIBLE       ", &
                                                    "PROCESSOR_DEFINED"]
        !! The possible character expressions
        !! for the `round` specifier in the `write` statement.

    !>The enumerator for the `round` specifier
    !>and expression in the `write` statement.
    type, public, extends(enum_atype) :: enum_write_round
        character(len=len(round)), public :: expr
            !! An expression for the `round` specifier.
            !! Open to public to pass the `write` statement.
    end type enum_write_round

    !>The possible expressions for the `round` specifier
    type :: enum_write_round_list
        type(enum_write_round), public :: up
            !! The enumerator to represent the `UP`
        type(enum_write_round), public :: down
            !! The enumerator to represent the `DOWN`
        type(enum_write_round), public :: zero
            !! The enumerator to represent the `ZERO`
        type(enum_write_round), public :: nearest
            !! The enumerator to represent the `NEAREST`
        type(enum_write_round), public :: compatible
            !! The enumerator to represent the `COMPATIBLE`
        type(enum_write_round), public :: processor_defined
            !! The enumerator to represent the `PROCESSOR_DEFINED`
    end type enum_write_round_list

    type(enum_write_round_list), public, parameter :: &
        write_round = &
            enum_write_round_list( &
                up                = enum_write_round(Write_Round_Up              , round(Write_Round_Up)), &
                down              = enum_write_round(Write_Round_Down            , round(Write_Round_Down)), &
                zero              = enum_write_round(Write_Round_Zero            , round(Write_Round_Zero)), &
                nearest           = enum_write_round(Write_Round_Nearest         , round(Write_Round_Nearest)), &
                compatible        = enum_write_round(Write_Round_Compatible      , round(Write_Round_Compatible)), &
                processor_defined = enum_write_round(Write_Round_ProcessorDefined, round(Write_Round_ProcessorDefined)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `round` specifier.

    interface optval
        procedure :: optval_write_round
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_write_round(x, default) result(y)
        implicit none
        type(enum_write_round), intent(in), optional :: x
        type(enum_write_round), intent(in) :: default

        type(enum_write_round) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_write_round
end module enumul_write_round
