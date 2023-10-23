MODULE screen

    implicit none

    INTERFACE

        MODULE PURE FUNCTION getlen( string ) result( out )

            character(len=*), intent(in) :: string
            integer :: out

        END FUNCTION getlen

        MODULE SUBROUTINE print_hello
        END SUBROUTINE print_hello

    END INTERFACE

END MODULE

MODULE common

    USE screen, only : getlen

    implicit none

    character(len=5), parameter :: hello = "hello"

    CONTAINS

        SUBROUTINE foo

            character(len=:), allocatable :: tmp
            write(*, *) getlen("test")

        END SUBROUTINE

END MODULE

SUBMODULE (screen) screen_submodule

    USE common, only : hello

    implicit none

    CONTAINS

        PURE FUNCTION getlen( string ) result( out )

            character(len=*), intent(in) :: string
            integer :: out

            out = len(trim(string))

        END FUNCTION getlen

        SUBROUTINE print_hello

            write(*, *) getlen( hello )

        END SUBROUTINE print_hello

END SUBMODULE screen_submodule

PROGRAM Test

    USE screen, only : print_hello

    implicit none

    call print_hello()

END PROGRAM Test
