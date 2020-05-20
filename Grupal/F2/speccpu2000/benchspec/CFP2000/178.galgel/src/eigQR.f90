C ***************************************************************
C * This is home-made driver for LAPACK QR eigenvalue solver    *
C ***************************************************************


      Subroutine Eigr (A, N, IA, EV, ier)
       Implicit  Real*8 (A-H,O-Z)

       Dimension A(IA,N), Z(1,1)
       Complex*16 EV(N)

       Real*8, Allocatable, Dimension(:) :: WK, EVR, EVI

       Allocate ( WK(N), EVR(N), EVI(N) )

C ========================================================

	C a l l   DGEBAL ('S', N, A, N, ILO, IHI,
     *                     WK, info)

	C a l l   DGEHRD (N, ILO, IHI, A, N, WK,
     *                    EVR, N, info)

	Lwork = N
	C a l l   DHSEQR ('E', 'N', N, ILO, IHI, 
     *                     A, N, EVR, EVI, Z, 1,
     *                     WK, Lwork, ierr)

        If (ierr .NE. 0) then
                              ier = 128 + ierr
                              Write (*,129) ierr
                              Return
        Else
              EV(1:N) = dCmplx( EVR(1:N), EVI(1:N) )
              ier = 0
        End If

           Deallocate ( WK, EVR, EVI, stat=jerr)
       Return
129      Format (' Error return from DHSEQR called by Eigr: ierr=', I5)
      End
