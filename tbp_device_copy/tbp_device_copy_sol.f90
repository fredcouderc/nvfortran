MODULE m_sw

   USE iso_fortran_env, only: ip => INT32, rp => REAL64

   implicit none

   TYPE :: unk

      real(rp), allocatable :: h(:)
      real(rp), allocatable :: hold(:)
      real(rp), allocatable :: u(:)
      real(rp), allocatable :: v(:)

   CONTAINS

      procedure, pass(sw) :: init => init_sw
      procedure, pass(sw) :: run => run_sw

   END TYPE unk

CONTAINS

   SUBROUTINE init_sw(sw, n)

      class(unk), intent(inout) :: sw
      integer(ip), intent(in) :: n

      allocate (sw%h(n), source=1._rp)
      allocate (sw%hold(n), source=1._rp)
      allocate (sw%u(n), source=4._rp)
      allocate (sw%v(n), source=3._rp)

   END SUBROUTINE

   SUBROUTINE run_sw(sw, n, nt)

      class(unk), intent(inout) :: sw
      integer(ip), intent(in) :: n , nt

      integer(ip) :: ite , ic
!$omp target data map(to:sw,sw%u) map(tofrom:sw%h) map(alloc:sw%hold)
!$acc data copyin(sw,sw%u) copy(sw%h) create(sw%hold)
      do ite = 1,nt
         sw%hold=sw%h
!$omp target update to(sw%hold)
!$acc update device(sw%hold)
         !$omp target teams loop
         !$acc parallel loop
         do ic = 2,n-1
            sw%h(ic) = sw%h(ic) - ( sw%hold(ic+1) * sw%u(ic+1) )
         end do
      end do
!$omp end target data
!$acc end data

   END SUBROUTINE

END MODULE m_sw

PROGRAM test

   USE m_sw

   implicit none

   type(unk) :: sw

   integer(ip), parameter :: n = 100
   integer(ip), parameter :: nt = 1

   !call init_sw( sw , n )
   call sw%init(  n )

   !call run_sw( sw , n , nt )
   call sw%run( n , nt )
   print *, sw%h(1:10)

END PROGRAM test

