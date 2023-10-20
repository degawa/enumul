module enumul_write_sign
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Write_Sign_Plus = 1
        enumerator :: Write_Sign_Suppress
        enumerator :: Write_Sign_ProcessorDefined
    end enum

    character(*), parameter, private :: sign(*) = ["PLUS             ", &
                                                   "SUPPRESS         ", &
                                                   "PROCESSOR_DEFINED"]
        !! The possible character expressions
        !! for the `sign` specifier in the `write` statement.

    !>The enumerator for the `sign` specifier
    !>and expression in the `write` statement.
    type, public, extends(enum_atype) :: enum_write_sign
        character(len=len(sign)), public :: expr
            !! An expression for the `sign` specifier.
            !! Open to public to pass the `write` statement.
    end type enum_write_sign

    !>The possible expressions for the `sign` specifier
    type :: enum_write_sign_list
        type(enum_write_sign), public :: plus
            !! The enumerator to represent the `PLUS`
        type(enum_write_sign), public :: suppress
            !! The enumerator to represent the `SUPPRESS`
        type(enum_write_sign), public :: processor_defined
            !! The enumerator to represent the `PROCESSOR_DEPENDENT`
    end type enum_write_sign_list

    type(enum_write_sign_list), public, parameter :: &
        write_sign = &
            enum_write_sign_list( &
                plus              = enum_write_sign(Write_Sign_Plus            , sign(Write_Sign_Plus)), &
                suppress          = enum_write_sign(Write_Sign_Suppress        , sign(Write_Sign_Suppress)), &
                processor_defined = enum_write_sign(Write_Sign_ProcessorDefined, sign(Write_Sign_ProcessorDefined)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `sign` specifier.

    interface optval
        procedure :: optval_write_sign
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_write_sign(x, default) result(y)
        implicit none
        type(enum_write_sign), intent(in), optional :: x
        type(enum_write_sign), intent(in) :: default

        type(enum_write_sign) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_write_sign
end module enumul_write_sign
