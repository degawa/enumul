module enumul_close
    use :: enumul_close_status, &
        only:enum_close_status, close_status, default_close_status, get_close_status_default, optval
    implicit none
    private

    ! enum types
    public :: enum_close_status

    ! enum lists
    public :: close_status

    ! default values
    public :: get_close_status_default, default_close_status

    ! optval procedures
    public :: optval
end module enumul_close
