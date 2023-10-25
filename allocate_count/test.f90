PROGRAM test

    implicit none

    integer, allocatable :: nc(:)
    integer :: n

    ! KO
    allocate(nc(2), source=count((/ 65, 65, 65 /) == 65))

    ! OK
    !allocate(nc(2), source=3)

    ! OK
    !n = count((/ 65, 65, 65 /) == 65)
    !allocate(nc(2), source=n)

    ! OK
    !allocate(nc(2), source=get_count((/ 65, 65, 65 /), 65))

    write(*, *) nc

CONTAINS

    FUNCTION get_count(arr, val) result(out)

        integer, dimension(:), intent(in) :: arr
        integer, intent(in) :: val
        integer :: out

        out = count(arr == val)

    END FUNCTION get_count


END PROGRAM test 
