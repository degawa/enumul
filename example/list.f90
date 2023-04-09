module logging_level_enum_list
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: enumul
    implicit none
    private
    public :: logging_level_type
    public :: logging_levels

    enum, bind(c)
        !&<
        enumerator :: LoggingLevel_Debug
        enumerator :: LoggingLevel_Info
        enumerator :: LoggingLevel_Warning
        enumerator :: LoggingLevel_Error
        enumerator :: LoggingLevel_Critical
        !&>
    end enum

    type, extends(enum_atype) :: logging_level_type
    end type logging_level_type

    type :: logging_level_list
        type(logging_level_type) :: Debug
        type(logging_level_type) :: Info
        type(logging_level_type) :: Warning
        type(logging_level_type) :: Error
        type(logging_level_type) :: critical
    end type logging_level_list

    type(logging_level_list), parameter :: &
        logging_levels = logging_level_list(Debug   =logging_level_type(LoggingLevel_Debug), &
                                            Info    =logging_level_type(LoggingLevel_Info), &
                                            Warning =logging_level_type(LoggingLevel_Warning), &
                                            Error   =logging_level_type(LoggingLevel_Error), &
                                            critical=logging_level_type(LoggingLevel_Critical)) !&
end module logging_level_enum_list

program main
    use :: enumul
    use :: logging_level_enum_list
    implicit none

    type(logging_level_type) :: thr

    thr = logging_levels%Info

    select case (thr%enum)
    case (logging_levels%Debug%enum)
        print *, "debug"
    case (logging_levels%Info%enum)
        print *, "information"
    case (logging_levels%Warning%enum)
        print *, "warning"
    case (logging_levels%Error%enum)
        print *, "error"
    case (logging_levels%critical%enum)
        print *, "critical"
    end select
end program main
