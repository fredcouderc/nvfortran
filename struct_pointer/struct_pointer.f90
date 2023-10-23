MODULE tt

   USE iso_fortran_env

   implicit none

   integer, parameter, public :: ip = int32
   integer, parameter, public :: rp = real64

   TYPE :: t1

      integer(ip) :: i
      real(rp)    :: r

   END TYPE t1

   TYPE :: t2

      type(t1), pointer :: p => null()

   END TYPE 

END MODULE tt

PROGRAM test
	
   USE tt

   implicit none

   type(t2) :: toto

END PROGRAM
