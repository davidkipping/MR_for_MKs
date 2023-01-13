MODULE polynomialmod

 implicit none

 CONTAINS

 SUBROUTINE polynomial(sdim,Rvec,nTlen,t,obs,res)

 implicit none

 INTEGER, INTENT(IN) :: sdim, nTlen
 REAL(8), DIMENSION(sdim), INTENT(IN) :: Rvec
 REAL(8), DIMENSION(nTlen), INTENT(IN) :: t, obs
 REAL(8), DIMENSION(nTlen) :: model
 REAL(8), DIMENSION(nTlen), INTENT(OUT) :: res
 REAL(8) :: tau0, Pp, nuttv, TTVamp, phi
 REAL(8), PARAMETER :: twopi = 6.283185307179586
 INTEGER :: i

 Pp = Rvec(1)
 tau0 = Rvec(2)
 nuttv = Rvec(3)
 TTVamp = Rvec(4)
 phi = Rvec(5)

 DO i=1,nTlen
   model(i) = tau0 + Pp*t(i) + TTVamp*DSIN( twopi*t(i)*nuttv + phi )
   res(i) = obs(i) - model(i)	! y residuals
 END DO
 !write(*,*) res

 END SUBROUTINE polynomial

END MODULE polynomialmod
 
