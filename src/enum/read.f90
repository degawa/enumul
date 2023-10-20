module enumul_read
    use :: enumul_read_advance, only:enum_read_advance, read_advance, optval
    use :: enumul_read_asynchronous, only:enum_read_asynchronous, read_asynchronous, optval
    use :: enumul_read_blank, only:enum_read_blank, read_blank, optval
    use :: enumul_read_decimal, only:enum_read_decimal, read_decimal, optval
    use :: enumul_read_pad, only:enum_read_pad, read_pad, optval
    use :: enumul_read_round, only:enum_read_round, read_round, optval
    implicit none
    private

    ! enum types
    public :: enum_read_advance
    public :: enum_read_asynchronous
    public :: enum_read_blank
    public :: enum_read_decimal
    public :: enum_read_pad
    public :: enum_read_round

    ! enum lists
    public :: read_advance
    public :: read_asynchronous
    public :: read_blank
    public :: read_decimal
    public :: read_pad
    public :: read_round

    ! optval procedures
    public :: optval
end module enumul_read
