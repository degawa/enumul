module logging_level_enum
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: enumul
    implicit none
    private
    public :: logging_level_type

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

    !&<
    type(logging_level_type), public, parameter :: &
        Debug    = logging_level_type(LoggingLevel_Debug)
    type(logging_level_type), public, parameter :: &
        Info     = logging_level_type(LoggingLevel_Info)
    type(logging_level_type), public, parameter :: &
        Warning  = logging_level_type(LoggingLevel_Warning)
    type(logging_level_type), public, parameter :: &
        Error    = logging_level_type(LoggingLevel_Error)
    type(logging_level_type), public, parameter :: &
        critical = logging_level_type(LoggingLevel_Critical)
    !&>
end module logging_level_enum

program main
    use :: enumul
    use :: logging_level_enum
    implicit none

    type(logging_level_type) :: thr

    thr = Info

    if (thr < Warning) then
        print *, "logging level is less than warning"
    end if

    if (thr == Info) then
        print *, "logging level is information"
    end if
end program main
