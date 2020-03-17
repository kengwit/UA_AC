C %%%%%%%%%% NWTN &&&&&&&&&&&&&&&&&&&&&
c
c   Newton solver for stady states


      Subroutine  NWTN( Fname, X, N, Anu, Eps, 
     *                          Nsig, Xref, ItMax, ier)
      Implicit Real*8 (A-H,O-Z)

       Dimension X(N), Xref(N)
       External  Fname

       Real*8, Allocatable :: Y(:), W(:)
       Real*8, Allocatable, Dimension (:,:) :: A
       Integer, Allocatable :: IPIV(:)

C ===========================================================
       Allocate (A(N,N), Y(N), W(N), IPIV(N), stat=jerr)
C ===========================================================

C ........ Check input data ......

          If (N. LT. 1) then
                             ier = 129
                             Write (*,129)
                             goto 999
CLAB                             Return
          End If

          p = MinVal( Abs( Xref(1:N) ) )

          If (p .LE. Eps) then
                               ier = 130
                               Write (*,130)
                               goto 999
CLAB                               Return
          End If

          If (ItMax .LE. 0) then
                                ier = 131
                                Write (*,131)
                                goto 999
CLAB                                Return
          End If

C *****  Start the process *****

          Write (*,100)

          Nit = 0

         Eps2   = Eps**2
         EpsN2  = 10.0D0**(-2*Nsig)
C        EpsGAU = 1.D-08
         EpsGAU = 1.D-10

         C a l l    Fname(X, N, Anu, Y, A, 0)

             DxMax = MaxVal( Abs ( Y(1:N) ) )
             Write (*,101) Nit, DxMax
C   DEBUG RAB
C            Write(2,*) "top: ItMax= ",ItMax,
C     &                 "    DPYY= ",DOT_PRODUCT(Y,Y),
C     &                 "    eps2= ",eps2
C   DEBUG RAB

         If (DOT_PRODUCT(Y,Y) .LT. eps2) then
            ier   = 0
            itMax = 0
            Write (*,102) ItMax
            goto 999
CLAB            Return
         End If

         C a l l   Fname(X, N, Anu, Y, A, 1)

C  ********  Start Iterations *************

         K = 0
         Do While (K .LE. ItMax)
                   K = K + 1
                 Nit = K

!LAB           C a l l   DGETRF (N, N, A, N, W, info)
           C a l l   DGETRF (N, N, A, N, IPIV, info)
             If (info .NE. 0) then
                                   ier = 132
                                   Write (*,132) info
                                   goto 999
CLAB                                   Return
             End If

!LAB           C a l l   DGETRS ('N', N, 1, A, N,  W, Y, N, info)
           C a l l   DGETRS ('N', N, 1, A, N, IPIV, Y, N, info)
             If (info .NE. 0) then
                                  ier = 133
                                   Write (*,133) info
                                   goto 999
CLAB                                   Return
             End If

C  ........... Change X ...........

             X(1:N) = X(1:N) - Y(1:N)
             W(1:N) = Y(1:N) / Xref(1:N)

C    .........Convergence ?????? .........


             C a l l    Fname(X, N, Anu, Y, A, 0)

                 DxMax = MaxVal( Abs ( Y(1:N) ) )
                 If (DxMax .LE. Eps) DxMax = 0.D0
                 Write (*,101) Nit, DxMax
C   DEBUG RAB
C            Write(2,*) "bottom: ItMax= ",ItMax,
C     &                 "    DPYY= ",DOT_PRODUCT(Y,Y),
C     &                 "    eps2= ",eps2
C   DEBUG RAB
  
             If (DOT_PRODUCT( W, W ) .LT. EPSN2) Exit
             If (DOT_PRODUCT( Y, Y ) .LT.  Eps2) Exit

             C a l l    Fname(X, N, Anu, Y, A, 1)

            End Do

C  *******   Exit  NWTN  *****

          If (K .GT. ItMax) then
                                ier   = 134
                                ItMax = K
                                Write (*,134)
                                goto 999
CLAB                                Return
          End If

          ItMax = Nit
          ier   = 0

          Write (*,102) ItMax

999       continue
          Deallocate (A, Y, W, IPIV, stat=jerr)
      Return
100    Format (/,8X,' Iterations of Newton method started')
101    Format (10X,'  Newton iteration # ',I2,
     *             '    Maximal derivative =', G11.4)
102    Format (8X,' Newton method converged in ',I2, ' iterations')
129    Format(' !!! Error in NWTN:   N < 0')
130    Format(' !!! Error in NWTN:   Xref = 0')
131    Format(' !!! Error in NWTN:   ItMax <= 0')
132    Format(' !!! Error in DGETRF called by NWTN: info=', I5)
133    Format(' !!! Error in DGETRS called by NWTN: info=', I5)
134    Format(' !!! No convergence in NWTN !!!')
      End

