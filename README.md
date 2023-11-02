# enumul
an incomplete typed enumerator emulator for Fortran.

## Motivation
Fortran has the enumerator for interoperability with C, but it cannot be typed. A typed enumerator is highly demanded and has been discussed for 20 years. A typed enumerator with limited functionality, `enumerator type`, will be added to [Fortran 202y](https://wg5-fortran.org/N2151-N2200/N2194.pdf). However, [users have pointed out the lack of functionality](https://fortran-lang.discourse.group/t/flaw-with-typed-enumerators-in-f202x/3429).

This library, enumul, provides a typed enumerator as an abstract data type, although it is somewhat underpowered to be called an emulator.

## Getting started
### Requirements
enumul has been tested only on Windows 10 but may also work on Linux/Mac OS.
Due to the use of relatively new features, including object-oriented programming, a recent compiler is required to use enumul. The compilers and versions listed below have been used to develop enumul.

- Modern Fortran compiler
    - gfortran 11.2 bundled with [quickstart Fortran on Windows](https://github.com/LKedward/quickstart-fortran)
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.7.0 alpha

### Get the code
To get the code, execute the following commnad:

```console
git clone https://github.com/degawa/enumul.git
cd enumul
```

### Reference from your project
Add the following `use` statement to modules or procedures calling par-funnel.

```Fortran
use :: enumul
```

### Reference as a fpm project's dependency
To use enumul in your fpm project, add the following to the fpm.toml.

```TOML
[dependencies]
enumul = {git = "https://github.com/degawa/enumul.git"}
```

### extends abstract data type to define a typed enumerator
Extends the abstract data type, `enum_atype`, to define a typed enumerator and then declares enumerators as the parameters.
For example,

```Fortran
use :: enumul

enum, bind(c)
    enumerator :: LoggingLevel_Debug
    enumerator :: LoggingLevel_Info
    enumerator :: LoggingLevel_Warning
    enumerator :: LoggingLevel_Error
    enumerator :: LoggingLevel_Critical
end enum

type, extends(enum_atype) :: logging_level_type
end type logging_level_type

type(logging_level_type), parameter :: Debug    = logging_level_type(LoggingLevel_Debug)
type(logging_level_type), parameter :: Info     = logging_level_type(LoggingLevel_Info)
type(logging_level_type), parameter :: Warning  = logging_level_type(LoggingLevel_Warning)
type(logging_level_type), parameter :: Error    = logging_level_type(LoggingLevel_Error)
type(logging_level_type), parameter :: critical = logging_level_type(LoggingLevel_Critical)

type(logging_level_type) :: thr

thr = Info

if (thr == Info) then
    print *, "logging level is Information"
end if
```

or

```Fortran
use :: enumul

enum, bind(c)
    enumerator :: LoggingLevel_Debug
    enumerator :: LoggingLevel_Info
    enumerator :: LoggingLevel_Warning
    enumerator :: LoggingLevel_Error
    enumerator :: LoggingLevel_Critical
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
```

## examples
- typed enumerators for specifiers in `open` and `close` statement
    - Extended the abstract data type `enum_atype` to add a component `expr` containing the character expression of a specifier in the `open` and `close` statement.
    - Added a list of possible values for each enumerator as a parameter named `{open|close}_<specifier-name>`.
    - Added a procedure `get_{open|close}_<specifier-name>_default` that returns the default value of each specifier as the enumerator.

```Fortran
program ex_openclose
    use, intrinsic :: iso_fortran_env
    use :: enumul_open
    use :: enumul_close
    implicit none

    integer(int32) :: unit
    open (newunit=unit, &
          action =open_action%write%expr, &
          form   =open_form%formatted%expr, &
          decimal=open_decimal%comma%expr, &
          status =open_status%scratch%expr)

    close (unit, status=close_status%delete%expr)
end program ex_openclose
```

- typed enumerators for specifiers in `read` and `write` statement
    - Extended the abstract data type `enum_atype` to add a component `expr` containing the character expression of a specifier in the `read` and `write` statement.
        - added a type-bound procedure `trim` to remove trailing whitespace.
    - Added a list of possible values for each enumerator as a parameter named `{read|write}_<specifier-name>`.

```Fortran
program ex_openclose
    use, intrinsic :: iso_fortran_env
    use :: enumul_open
    use :: enumul_close
    use :: enumul_write
    use :: enumul_read
    implicit none

    integer(int32) :: unit
    open (newunit=unit, file="sequential.tmp", &
          form=open_form%formatted%expr, &
          action=open_action%write%expr, &
          status=open_status%replace%expr)
    write (unit, '(A)', advance=write_advance%no%trim()) "🚀"
    write (unit, '(A)', advance=write_advance%yes%trim()) "🌏"
    close (unit, status=close_status%keep%expr)
    ! 🚀🌏 is written in sequential.tmp
end program ex_openclose
```

## Todo
- To simplify typed enumerator definitions and declarations.