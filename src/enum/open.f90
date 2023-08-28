module enumul_open
    use :: enumul_open_access, &
        only:enum_open_access, open_access, default_open_access, get_open_access_default, optval
    use :: enumul_open_action, &
        only:enum_open_action, open_action, get_open_action_default, optval
    use :: enumul_open_asynchronous, &
        only:enum_open_asynchronous, open_asynchronous, default_open_asynchronous, get_open_asynchronous_default, optval
    use :: enumul_open_blank, &
        only:enum_open_blank, open_blank, default_open_blank, get_open_blank_default, optval
    use :: enumul_open_status, &
        only:enum_open_status, open_status, default_open_status, get_open_status_default, optval
    implicit none
    private

    ! enum types
    public :: enum_open_access
    public :: enum_open_action
    public :: enum_open_asynchronous
    public :: enum_open_blank
    public :: enum_open_status

    ! enum lists
    public :: open_access
    public :: open_action
    public :: open_asynchronous
    public :: open_blank
    public :: open_status

    ! default values
    public :: get_open_access_default, default_open_access
    public :: get_open_action_default
    public :: get_open_asynchronous_default, default_open_asynchronous
    public :: get_open_blank_default, default_open_blank
    public :: get_open_status_default, default_open_status

    ! optval procedures
    public :: optval
end module enumul_open
