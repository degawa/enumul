module enumul_write_decimal
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Write_Decimal_Comma = 1
        enumerator :: Write_Decimal_Point
    end enum

    character(*), parameter, private :: deciaml(*) = ["COMMA", &
                                                      "POINT"]
        !! The possible character expressions
        !! for the `decimal` specifier in the `write` statement.

    !>The enumerator for the `decimal` specifier
    !>and expression in the `write` statement.
    type, public, extends(enum_atype) :: enum_write_decimal
        character(len=len(deciaml)), public :: expr
            !! An expression for the `decimal` specifier.
            !! Open to public to pass the `write` statement.
    end type enum_write_decimal

    !>The possible expressions for the `decimal` specifier
    type :: enum_write_decimal_list
        type(enum_write_decimal), public :: comma
            !! The enumerator to represent the `COMMA`
        type(enum_write_decimal), public :: point
            !! The enumerator to represent the `POINT`
    end type enum_write_decimal_list

    type(enum_write_decimal_list), public, parameter :: &
        write_decimal = &
            enum_write_decimal_list( &
                comma = enum_write_decimal(Write_Decimal_Comma, deciaml(Write_Decimal_Comma)), &
                point = enum_write_decimal(Write_Decimal_Point, deciaml(Write_Decimal_Point)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `decimal` specifier.

    interface optval
        procedure :: optval_write_decimal
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_write_decimal(x, default) result(y)
        implicit none
        type(enum_write_decimal), intent(in), optional :: x
        type(enum_write_decimal), intent(in) :: default

        type(enum_write_decimal) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_write_decimal
end module enumul_write_decimal
