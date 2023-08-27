module enumul_open
    use :: enumul_open_access, &
        only:enum_open_access, open_access, default_open_access, get_open_access_default_expr, optval
    implicit none
    private

    ! enum types
    public :: enum_open_access

    ! enum lists
    public :: open_access

    ! default values
    public :: default_open_access
    public :: get_open_access_default_expr

    ! optval procedures
    public :: optval
end module enumul_open
