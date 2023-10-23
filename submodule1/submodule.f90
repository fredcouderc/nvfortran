MODULE mod

   USE, intrinsic :: iso_fortran_env, only : ip => INT32, rp => REAL64

   implicit none

   integer(ip), parameter :: lp = 4

   INTERFACE

      MODULE FUNCTION withopt( arg , default ) RESULT( opt )

         logical(lp), intent(in), optional :: default
         logical(lp), intent(in), optional :: arg
         logical(lp)                       :: opt

      END FUNCTION withopt

   END INTERFACE

END MODULE mod

SUBMODULE (mod) submod

   implicit none

CONTAINS

   MODULE FUNCTION withopt( arg , default ) RESULT( opt )

      logical(lp), intent(in), optional :: default
      logical(lp), intent(in), optional :: arg
      logical(lp)                       :: opt

      opt = .false.

      if( present( default ) ) opt = default
      if( present( arg     ) ) opt = arg

   END FUNCTION withopt

END SUBMODULE submod

MODULE func

   USE mod, only : ip , rp , lp , withopt

   implicit none

   INTERFACE

      MODULE SUBROUTINE totest( opt )

         logical(lp), intent(in), optional :: opt

      END SUBROUTINE totest

   END INTERFACE

END MODULE func

SUBMODULE (func) subfunc

   ! USE mod, only : withopt

   implicit none

CONTAINS

   MODULE SUBROUTINE totest( opt )

      logical(lp), intent(in), optional :: opt

      if (       withopt( opt ) ) write(6,*) 'true'
      if ( .not. withopt( opt ) ) write(6,*) 'false'

   END SUBROUTINE totest

END SUBMODULE subfunc

PROGRAM test

   USE func, only : totest

   call totest( opt=.true.  )
   call totest( opt=.false. )

END PROGRAM test
