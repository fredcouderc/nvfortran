MODULE m_sw

   USE iso_fortran_env, only: ip => INT32, rp => REAL64

   implicit none

   TYPE :: unk

      real(rp), allocatable :: h(:)
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
      allocate (sw%u(n), source=0._rp)
      allocate (sw%v(n), source=0._rp)

      !$omp target enter data map(to: sw, sw%h, sw%u, sw%v)

   END SUBROUTINE

   SUBROUTINE run_sw(sw, n, nt)

      class(unk), intent(inout) :: sw
      integer(ip), intent(in) :: n , nt

      integer(ip) :: ite , ic

      do ite = 1,nt
         !$omp target teams distribute parallel do
         do ic = 2,n-1
            sw%h(ic) = sw%h(ic) - ( sw%h(ic+1) * sw%u(ic+1) )
         end do
      end do
      !$omp exit target data map(delete: sw, sw%h, sw%u, sw%v)

   END SUBROUTINE

END MODULE m_sw

PROGRAM test

   USE m_sw

   implicit none

   type(unk) :: sw

   integer(ip), parameter :: n = 100
   integer(ip), parameter :: nt = 1000

   call sw%init( sw , n )

   call sw%run( sw , n , nt )

END PROGRAM test

