MODULE MyStatsModule


CONTAINS

	!--------------------------------------------------------------------
	!  Downloaded from http://www.cs.mtu.edu/~shene/COURSES/cs201/NOTES/chap08/median.f90
	!  on Feb 11, 2016.
	!  Modified from a program into a subroutine by Tim Hunter.
	!
	!  This is an internal REAL function for computing the
	!  median of a set of input.  The median of a set of N data values is
	!  defined as follows.  First, the data values must be sorted.  Then,
	!  the median is the middle value X(N/2+1) if N is odd; otherwise, the
	!  median is the average of the middle two values (i.e., (X(n)+X(N/2+1))/2).
	!  For example, the median of 4, 2, 3, 1 is 2.5 since the sorted data
	!  values are 1, 2, 3 and 4 and the average of the middle two data
	!  values is (2+3)/2.  The median of 5, 3, 4, 1, 2 is 3 since 3 is the
	!  middle value of the sorted data 1, 2, 3, 4, 5.
	!
	!  We shall use the sorting subroutine discussed earlier.
   !
	!  REAL FUNCTION  Median() :
	!    This function receives an array X of N entries, copies its value
	!    to a local array Temp(), sorts Temp() and computes the median.
	!    The returned value is of REAL type.
	! --------------------------------------------------------------------
   REAL FUNCTION  Median(X, N)
      IMPLICIT  NONE
      REAL, DIMENSION(1:), INTENT(IN) :: X
      INTEGER, INTENT(IN)             :: N

      INTEGER :: I
      REAL, DIMENSION(1:N) :: Temp

      DO I = 1, N                       ! make a copy
         Temp(I) = X(I)
      END DO
      CALL Sort(Temp, N)               ! sort the copy
      IF (MOD(N,2) == 0) THEN          ! compute the median
         Median = (Temp(N/2) + Temp(N/2+1)) / 2.0
      ELSE
         Median = Temp(N/2+1)
      END IF
   END FUNCTION  Median
	
	! --------------------------------------------------------------------
	! INTEGER FUNCTION  FindMinimum():
	!    This function returns the location of the minimum in the section
	! between Start and End.
	! --------------------------------------------------------------------
   INTEGER FUNCTION  FindMinimum(x, Start, End)
      IMPLICIT  NONE
      REAL, DIMENSION(1:), INTENT(IN) :: x
      INTEGER, INTENT(IN)             :: Start, End
      INTEGER :: I, Location
      REAL    :: Minimum

      Minimum  = x(Start)          ! assume the first is the min
      Location = Start             ! record its position
      DO I = Start+1, End          ! start with next elements
         IF (x(I) < Minimum) THEN  !   if x(i) less than the min?
            Minimum  = x(I)        !      Yes, a new minimum found
            Location = I           !      record its position
         END IF
      END DO
      FindMinimum = Location            ! return the position
   END FUNCTION  FindMinimum
	
	! --------------------------------------------------------------------
	! SUBROUTINE  Swap():
	!    This subroutine swaps the values of its two formal arguments.
	! --------------------------------------------------------------------
   SUBROUTINE  Swap(a, b)
      IMPLICIT  NONE
      REAL, INTENT(INOUT) :: a, b
      REAL                :: Temp

      Temp = a
      a    = b
      b    = Temp
   END SUBROUTINE  Swap

	! --------------------------------------------------------------------
	! SUBROUTINE  Sort():
	!    This subroutine receives an array x() and sorts it into ascending
	! order.
	! --------------------------------------------------------------------
   SUBROUTINE  Sort(x, Size)
      IMPLICIT  NONE
      REAL, DIMENSION(1:), INTENT(INOUT) :: x
      INTEGER, INTENT(IN)                :: Size
      INTEGER :: I, Location

      DO I = 1, Size-1                       ! except for the last
         Location = FindMinimum(x, I, Size)  ! find min from this to last
         CALL  Swap(x(i), x(Location))       ! swap this and the minimum
      END DO
   END SUBROUTINE  Sort
	

END MODULE MyStatsModule







