MODULE like

use params
use polynomialmod

implicit none
      
contains
      
!=======================================================================

SUBROUTINE slikelihood(R,slhood)
         
	implicit none
      
	REAL(8), DIMENSION(nest_nPar) :: R
        REAL(8) :: slhood, loglike
	INTEGER :: i

        ! Scaling of the parameters from hypercube
	DO i = 1, sdim
          IF( Rflag(i) .EQ. 0 .OR. Rflag(i) .EQ. 2 ) THEN ! Modes 0 and 2
            ! Uniform: Rmax = max, Rmin = min
	    R(i) = Rmin(i) + (Rmax(i)-Rmin(i))*R(i)
          ELSE IF( Rflag(i) .EQ. 3 ) THEN ! Mode 3
            ! Gaussian: Rmax = mean, Rmin = stdev
            R(i) = Rmax(i) + DSQRT(2.0D0)*Rmin(i)*inverf(-1.0D0+2.0D0*R(i))
          ELSE IF( Rflag(i) .EQ. 4 ) THEN ! Mode 4
            ! Jeffrey's: Rmax = max, Rmin = min
            R(i) = ( Rmax(i)**R(i) )*( Rmin(i)**(1.0D0-R(i)) )
          ELSE IF( Rflag(i) .EQ. 5 ) THEN ! Mode 5
            ! Modified Jefrey's: Rmax = max, Rmin = inflection point
            R(i) = -( Rmin(i)**(1.0D0-R(i)) )*( Rmin(i)**R(i) - ( Rmin(i)+Rmax(i) )**R(i) )
          END IF
	END DO

        ! Call models to get likelihood
        call models(R,loglike)

	slhood = loglike
        !!write(*,*) 'R(1) = ',R(1),' yielding chi2 = ',chi2

END SUBROUTINE slikelihood
      
!=======================================================================

! ======================================================================
SUBROUTINE models(Rvec,loglike)

 implicit none

 REAL(8), DIMENSION(nest_nPar), INTENT(IN) :: Rvec   ! Fitted-parameter vector
 REAL(8) :: bperp, theta, V
 REAL(8) :: denom, loglikep, loglikes, loglike ! Log-likelihoods
 INTEGER :: i

 bperp = Rvec(1)
 theta = Rvec(2)
 V = Rvec(3)

 loglikep = 0.0D0
 loglikes = 0.0D0
 DO i=1,np
	 denom = V + (sigx(i)*DSIN(theta))**2 - sigx(i)*sigy(i)*DSIN(2.0D0*theta) + (sigy(i)*DCOS(theta))**2
	 loglikep = loglikep + ( bperp + x(i)*DSIN(theta) - y(i)*DCOS(theta) )**2 / denom
	 loglikes = loglikes + DLOG( denom )
 END DO
 loglike = -0.5D0*loglikep - 0.5D0*loglikes

END SUBROUTINE models
! ======================================================================

! ======================================================================
FUNCTION inverf(x)

 implicit none

 REAL(8) :: x
 REAL(8), PARAMETER :: awil = 0.14001228868666646D0
 REAL(8), PARAMETER :: bwil = 4.546884979448289D0
 REAL(8) :: factor, xsq, inverf

 IF( x .LT. 0.0D0 ) THEN
  factor = -1.0D0
 ELSE
  factor = 1.0D0
 END IF

 xsq = 1.0D0 - x**2
 x = bwil + 0.5D0*DLOG(xsq)
 x = DSQRT( x**2 - (DLOG(xsq)/awil) ) - x
 inverf = factor*DSQRT(x)

END FUNCTION
! ======================================================================

END MODULE like

