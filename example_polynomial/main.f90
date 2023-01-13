PROGRAM main

	use params
	use nestwrapper
      
	implicit none
	
	INTEGER :: i, j

      	!no parameters to wrap around
        DO j=1,sdim
          nest_pWrap(j) = 0
        END DO
		    !nest_pWrap(5) = 1

        ! OBTAIN PRIORS
        !--------------------------------------------------------------------------
        ! Open up jammi_in.txt and read file
        open(1,FILE='example_polynomial/jammi_in.txt',FORM='FORMATTED',STATUS='UNKNOWN')
        DO j=1,sdim
          read(1,*) Rsol(j),Rdel(j),Rmin(j),Rmax(j),Rflag(j)
          write(*,*) Rsol(j),Rdel(j),Rmin(j),Rmax(j),Rflag(j)
        END DO
        close(1)

        ! Mode 0 override
        DO j=1,sdim
          IF( Rflag(j) .EQ. 0 ) THEN
            Rmin(j) = Rsol(j) !- 1.0D-9
            Rmax(j) = Rsol(j) !+ 1.0D-9
          END IF
        END DO

        ! Read-in the data.dat
        write(*,*) 'Reading in primary data...'
        open(unit=30301,file='example_polynomial/datain.dat')
        DO i = 1,np
          read(30301,*) x(i),sigx(i),y(i),sigy(i)
        END DO
        close(30301)

      	call nest_Sample
END
