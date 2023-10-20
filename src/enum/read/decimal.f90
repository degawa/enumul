module enumul_read_decimal
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Read_Decimal_Comma = 1
        enumerator :: Read_Decimal_Point
    end enum

    character(*), parameter, private :: deciaml(*) = ["COMMA", &
                                                      "POINT"]
        !! The possible character expressions
        !! for the `decimal` specifier in the `read` statement.

    !>The enumerator for the `decimal` specifier
    !>and expression in the `read` statement.
    type, public, extends(enum_atype) :: enum_read_decimal
        character(len=len(deciaml)), public :: expr
            !! An expression for the `decimal` specifier.
            !! Open to public to pass the `read` statement.
    end type enum_read_decimal

    type :: enum_read_decimal_list
        type(enum_read_decimal), public :: comma
            !! The enumerator to represent the `COMMA`
        type(enum_read_decimal), public :: point
            !! The enumerator to represent the `POINT`
    end type enum_read_decimal_list

    type(enum_read_decimal_list), public, parameter :: &
        read_decimal = &
            enum_read_decimal_list( &
                comma = enum_read_decimal(Read_Decimal_Comma, deciaml(Read_Decimal_Comma)), &
                point = enum_read_decimal(Read_Decimal_Point, deciaml(Read_Decimal_Point)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `decimal` specifier.

    interface optval
        procedure :: optval_read_decimal
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_read_decimal(x, default) result(y)
        implicit none
        type(enum_read_decimal), intent(in), optional :: x
        type(enum_read_decimal), intent(in) :: default

        type(enum_read_decimal) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_read_decimal
end module enumul_read_decimal
