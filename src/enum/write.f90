module enumul_write
    use :: enumul_write_advance, only:enum_write_advance, write_advance, optval
    use :: enumul_write_asynchronous, only:enum_write_asynchronous, write_asynchronous, optval
    use :: enumul_write_decimal, only:enum_write_decimal, write_decimal, optval
    use :: enumul_write_delim, only:enum_write_delim, write_delim, optval
    use :: enumul_write_round, only:enum_write_round, write_round, optval
    use :: enumul_write_sign, only:enum_write_sign, write_sign, optval
    implicit none
    private

    ! enum types
    public :: enum_write_advance
    public :: enum_write_asynchronous
    public :: enum_write_decimal
    public :: enum_write_delim
    public :: enum_write_round
    public :: enum_write_sign

    ! enum lists
    public :: write_advance
    public :: write_asynchronous
    public :: write_decimal
    public :: write_delim
    public :: write_round
    public :: write_sign

    ! optval procedures
    public :: optval
end module enumul_write
