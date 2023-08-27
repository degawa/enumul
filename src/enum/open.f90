module enumul_open
    use :: enumul_open_access, &
        only:enum_open_access, open_access, default_open_access, get_open_access_default, optval
    use :: enumul_open_action, &
        only:enum_open_action, open_action, get_open_action_default, optval
    use :: enumul_open_status, &
        only:enum_open_status, open_status, default_open_status, get_open_status_default, optval
    implicit none
    private

    ! enum types
    public :: enum_open_access
    public :: enum_open_action
    public :: enum_open_status

    ! enum lists
    public :: open_access
    public :: open_action
    public :: open_status

    ! default values
    public :: default_open_access
    public :: get_open_access_default
    public :: get_open_action_default
    public :: default_open_status
    public :: get_open_status_default

    ! optval procedures
    public :: optval
end module enumul_open
