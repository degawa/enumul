module enumul_read_blank
    use, intrinsic :: iso_c_binding
    use :: enumul, only:enum_atype
    implicit none
    private
    public :: optval

    enum, bind(c)
        enumerator :: Read_Blank_Null = 1
        enumerator :: Read_Blank_Zero
    end enum

    character(*), parameter, private :: blank(*) = ["NULL", &
                                                    "ZERO"]
        !! The possible character expressions
        !! for the `blank` specifier in the `read` statement.

    !>The enumerator for the `blank` specifier
    !>and expression in the `read` statement.
    type, public, extends(enum_atype) :: enum_read_blank
        character(len=len(blank)), public :: expr
            !! An expression for the `blank` specifier.
            !! Open to public to pass the `read` statement.
    end type enum_read_blank

    type :: enum_read_blank_list
        type(enum_read_blank), public :: null
            !! The enumerator to represent the `NULL`
        type(enum_read_blank), public :: zero
            !! The enumerator to represent the `ZEOR`
    end type enum_read_blank_list

    type(enum_read_blank_list), public, parameter :: &
        read_blank = &
            enum_read_blank_list( &
                null = enum_read_blank(Read_Blank_Null, blank(Read_Blank_Null)), &
                zero = enum_read_blank(Read_Blank_Zero, blank(Read_Blank_Zero)) &
            ) !&
        !! The enumerators of possible expressions
        !! for the `blank` specifier.

    interface optval
        procedure :: optval_read_blank
    end interface
contains
    !>Returns `x` if it is presented, and
    !>returns `default` if the `x` is not presented.
    function optval_read_blank(x, default) result(y)
        implicit none
        type(enum_read_blank), intent(in), optional :: x
        type(enum_read_blank), intent(in) :: default

        type(enum_read_blank) :: y

        if (present(x)) then
            y = x
        else
            y = default
        end if
    end function optval_read_blank
end module enumul_read_blank
