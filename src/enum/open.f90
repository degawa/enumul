module enumul_open
    use :: enumul_open_access, &
        only:enum_open_access, open_access, default_open_access, get_open_access_default, optval
    use :: enumul_open_action, &
        only:enum_open_action, open_action, get_open_action_default, optval
    use :: enumul_open_asynchronous, &
        only:enum_open_asynchronous, open_asynchronous, default_open_asynchronous, get_open_asynchronous_default, optval
    use :: enumul_open_blank, &
        only:enum_open_blank, open_blank, default_open_blank, get_open_blank_default, optval
    use :: enumul_open_decimal, &
        only:enum_open_decimal, open_decimal, default_open_decimal, get_open_decimal_default, optval
    use :: enumul_open_delim, &
        only:enum_open_delim, open_delim, default_open_delim, get_open_delim_default, optval
    use :: enumul_open_encoding, &
        only:enum_open_encoding, open_encoding, default_open_encoding, get_open_encoding_default, optval
    use :: enumul_open_form, &
        only:enum_open_form, open_form, get_open_form_default, optval
    use :: enumul_open_pad, &
        only:enum_open_pad, open_pad, default_open_pad, get_open_pad_default, optval
    use :: enumul_open_position, &
        only:enum_open_position, open_position, default_open_position, get_open_position_default, optval
    use :: enumul_open_round, &
        only:enum_open_round, open_round, get_open_round_default, optval
    use :: enumul_open_sign, &
        only:enum_open_sign, open_sign, default_open_sign, get_open_sign_default, optval
    use :: enumul_open_status, &
        only:enum_open_status, open_status, default_open_status, get_open_status_default, optval
    implicit none
    private

    ! enum types
    public :: enum_open_access
    public :: enum_open_action
    public :: enum_open_asynchronous
    public :: enum_open_blank
    public :: enum_open_decimal
    public :: enum_open_delim
    public :: enum_open_encoding
    public :: enum_open_form
    public :: enum_open_pad
    public :: enum_open_position
    public :: enum_open_round
    public :: enum_open_sign
    public :: enum_open_status

    ! enum lists
    public :: open_access
    public :: open_action
    public :: open_asynchronous
    public :: open_blank
    public :: open_decimal
    public :: open_delim
    public :: open_encoding
    public :: open_form
    public :: open_pad
    public :: open_position
    public :: open_round
    public :: open_sign
    public :: open_status

    ! default values
    public :: get_open_access_default, default_open_access
    public :: get_open_action_default
    public :: get_open_asynchronous_default, default_open_asynchronous
    public :: get_open_blank_default, default_open_blank
    public :: get_open_decimal_default, default_open_decimal
    public :: get_open_delim_default, default_open_delim
    public :: get_open_encoding_default, default_open_encoding
    public :: get_open_form_default
    public :: get_open_pad_default, default_open_pad
    public :: get_open_position_default, default_open_position
    public :: get_open_round_default
    public :: get_open_sign_default, default_open_sign
    public :: get_open_status_default, default_open_status

    ! optval procedures
    public :: optval
end module enumul_open
