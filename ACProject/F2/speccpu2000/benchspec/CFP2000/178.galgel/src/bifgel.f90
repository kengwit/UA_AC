      Subroutine  BifGel (Fname, XS, N, Anu, U, Mth, Jjob , Iprint ,
     1     Par , V1, Err , ier )

       Implicit  Real*8(A-H,O-Z)
C .......................................................
C
C    Jjob  = 0 - everything without extimation of error 
C          = 1 - everything with estimation of error
C          = 2 - only critical point
C          = 3 - only eigenvector
C          = 4 - eigenvector and nonlinear decomposition
C .......................................................

        Dimension  XS(N), Par(10), Err(7)
        Complex*16 V1(N)
        Complex*16 EV1, C1, C1Err, Dev, DevErr 

C .......... Dynamic data .................................

        Real*8,     Allocatable, Dimension(:)   :: WX, Xref
        Real*8,     Allocatable, Dimension(:,:) :: AAA
        Complex*16, Allocatable, Dimension(:)   :: Uvec, Vvec

        NameList /BifGelInput/ EpsR, Eps, EpsErr, Nsig, ItMax
        NameList /BifGelStep3/ EpsR, Eps, Nsig, UU

C .......... External routines ............................


       External  Fname, Evals, Eval1

C ============= Allocate data ===================================
       Allocate ( WX(N), Xref(N), Uvec(N), Vvec(N), stat=jerr )
C ===============================================================

C ......... Check input data ..............................

      C1Err = (0.0D0,0.0D0)

      If (N .LT. 2) then 
                         ier = 129
                         Write (*,129) N
                         goto 999
CLAB                         Return
      End If

      If (U .LE. 0.D0) then 
                           ier = 130
                           Write (*,130) U
                           goto 999
CLAB                           Return
      End If

      If (Mth.LT.1 .OR. Mth.GT.2) then
                                      ier = 131
                                      Write (*,131) Mth
                                      goto 999
CLAB                                      Return
      End If

      If (Jjob  .GT. 5) then
                            ier = 132
                            Write (*,132) Jjob
                            goto 999
CLAB                            Return
      End If

      If (Iprint .LT.0 .OR. Iprint .GT.2) then
                                      ier = 133
                                      Write (*,133) Iprint
                                      goto 999
CLAB                                      Return
      End If
      

C ########## Initial data ######################################
      EpsR   = 1.0D-06
      Eps    = 1.0D-06
      EpsErr = 1.D-04

      Nsig   = 5
      ItMax  = 15

!LAB  namelist read input to foil aggressive optimizers
      read(5,nml=BifGelInput,end=991,err=991)
  991 continue

      NitAnu = ItMax
      N2     = N**2
      NC     = 2*N

      Do  i=1,N
          Xref(i) = Ref( XS(i) )
      End Do

      AnuREF = Ref( Anu )
      AnuC   = Anu

      WX(1:N) = XS(1:N)


C ############ Start linear stability analysis ######################

	If (Jjob  .LE. 3) then

C ......... Calculation of critical point .......................

           Write (*,101)

           If (Mth .EQ. 1) Call AnuCRT(Fname,  Evals,  WX,     N,
     1                                 AnuC,   Omega,  Eps,    Nsig,
     2                                 Xref,   AnuRef, NitAnu, U,
     3                                 Iprint, ier )    
               If (ier  .NE. 0) then
                                    ier = 135
                                    Write (*,135) ier
                                    goto 999
CLAB                                    Return
                          
               End If

           If (Mth .EQ. 2) Call AnuCRT(Fname, Eval1,  WX,     N,
     1                                AnuC,   Omega,  Eps,    Nsig,
     2                                Xref,   AnuREF, NITAnu, U,
     3                                Iprint, ier )  
               If (ier  .NE. 0) then
                                    ier = 136
                                    Write (*,135) ier
                                    goto 999
CLAB                                    Return
                          
               End If

           XS(1:N) = WX(1:N)

           Anu    = AnuC
           Par(8) = Omega

           Write (*,102)

C ................................................................

        Else
                    AnuC  = Anu
	          Omega = Par(8)
        End If

        If ( Jjob .EQ. 2 ) goto 999
CLAB        If (Jjob  .EQ. 2) Return

C ######### Start weakly nonlinear analysis #######################

           Write (*,103)

C .......... Calculation of the eigenvector and C1 ............

                EpsR  = 1.0D-06
                Eps   = 1.0D-06
                Nsig  = 3
                UU    = 1.D-04
!LAB  namelist read input to foil aggressive optimizers
                read(5,nml=BifGelStep3,end=901,err=901)
901             continue
                U=UU
                NitC1 = ItMax 

        If  (Jjob  .LE. 5) then
                                Write (*,104)

           C a l l  C1PNF (Fname, XS,     N,      Anu,  Omega, U, 
     1                     Jjob,  C1,     C1Err,  Nsig, NitC1, Uvec, 
     2                     Vvec,  EnormX, EnormV, ier )

             If (ier .NE. 0) then
                                  ier = 137
                                  Write (*,137) ier
                                  goto 999
CLAB                                  Return
             End If

               Write (*,105) C1

               Beta2   = 2.D0*Real(C1)
               Par(3)  = Beta2
               Par(4)  = Real(C1)
               Par(5)  = Aimag(C1)
               Par(9)  = EnormX
               Par(10) = EnormV

               V1(1:N) = Vvec(1:N)

C ........ Check eigenvectors ...............................

          If (Jjob  .EQ. 1) then

               Allocate ( AAA(N,N), stat=jerr )

               C a l l   Fname(XS, N, Anu, WX, AAA, 0)
               C a l l   Fname(XS, N ,Anu, WX, AAA, 1)

               C a l l   ChEig (AAA, Uvec, Vvec, N, Omega)

               Deallocate ( AAA, stat=jerr)
          End If

        Else
                Vvec(1:N) = V1(1:N)
        End If 

        If ( Jjob .EQ. 3) goto 999
CLAB        If (Jjob  .EQ. 3) Return

C ........ Calculation of Amu2 and Tau2 ...........................

             EV1    = Dcmplx(0.0D0, Omega)
             NitDev = ItMax*2

                WX(1:N) = XS(1:N)
              Xref(1:N) = 1.D0
              
              Write (*,106)

             C a l l   DEval1 (Fname,  WX,   N,      Anu,    Eps,
     1                         Nsig,   Xref, AnuREF, NitDev, U,
     2                         Vvec,   EV1,  Jjob,   Dev,  
     3                         DevErr, ier      )

                If (ier  .NE. 0) then 
                                      ier = 138
                                     Write (*,138)
                                     goto 999
CLAB                                     Return
                End If

                Write (*,107) Dev

              Dalpha = Real(Dev)
              Domega = Aimag(Dev)
              Par(6) = Dalpha
              Par(7) = Domega

              If (Dalpha .EQ. 0.D0) then
                                        ier=139
                                        Write (*,139)
                                        Write (*,150)
                                        goto 999
CLAB                                        Return
              End If

              Amu2   = -Real(C1)/Dalpha
              Par (1) = Amu2

              If (Omega .EQ. 0.D0) then
                                       ier=140
                                       Write (*,140)
                                       Write (*,150)
                                       goto 999
CLAB                                       Return
              End If
             
              Tau2   = - ( Aimag(C1) + Amu2*Domega ) / Omega
              Par(2) = Tau2

          If (Jjob  .NE. 0) then

               Err(4) = Abs( Real(C1Err  ))
               Err(5) = Abs(Aimag(C1Err  ))
               Err(6) = Abs( Real(DevErr ))
               Err(7) = Abs(Aimag(DevErr ))
               Err(3) = 2.D0*Err(4)

               Err(1) = 0.D0

               Do i=1,3,2
                  Dalph = Dalpha + Dble(i-2)*Err(6)

                  If (Dalph .EQ. 0.D0) then
                                           ier=139
                                           Write (*,139)
                                           Write (*,150)
                                           goto 999
CLAB                                           Return
                  End If

                  Do j=1,3,2
                    Amu = -(Real(C1) + Dble(j-2)*Err (4))/ Dalph
                    Err(1) = Dmax1( Err(1), Abs(Amu2-Amu) )
                  End Do

               End Do

              Err (2) = 0.0D0

              Do i=1,3,2
                    Amu = Amu2 + Dble(i-2)*Err (1)

                    Do j=1,3,2
                       Domeg = Domega + Dble(j-2)*Err(7)

                       Do k=1,3,2
                         Tau = -(Aimag(C1) + Dble(k-2)*Err(5)
     1                                       + Amu*DoMEG ) / Omega
                         Err(2) = Dmax1( Err(2), Abs(Tau2-Tau) )
                       End Do

                    End Do
              End Do

	    If ( Abs(Amu2) .GT. 1.D0) Err(1) = Err(1) / Amu2

	    p = Abs( Amu2*Domega / Omega )
	    If (p .GT. 1.D0) Err (2) = Err(2) /p

          End If

          If (Abs(Dalpha) .LT. EpsR) then
                                         ier=139
                                         Write (*,139)
                                         Write (*,150)
                                         goto 999
CLAB                                         Return
          End If
          If (Abs(Omega) .LT. EpsR) then
                                         ier=140
                                         Write (*,140)
                                         Write (*,150)
                                         goto 999
CLAB                                         Return
          End If

          Do i=1,7
            If ( Abs( Err(i) ) .LE. EpsErr) Err(i) = 0.D0
          End Do


          Write (*,114)
          Write (*,108) Err(7)
          Write (*,109) Err(1)
          Write (*,110) Err(2)
          Write (*,111) Err(3)
          Write (*,112) Err(6)                                                   
          Write (*,113) Err(4), Err(5)
          
          Write (*,200)
999       continue
          DeAllocate ( WX, Xref, Uvec, Vvec, stat=jerr )
          Return
101     Format (4X,' Iterations of linear stability analysis started',/)
102     Format (/,' Step 2 is successfully completed',/, 80('_'),//)
103     Format (/,' Step 3: Perform weakly nonlinear analysis of the',
     *            ' Hopf bifurction',/,
     *          '         using projection on the central manifold',// ) 
104     Format (5X,' ........ Calculate left and right eigenvectors',
     *             ' and coefficient C1 ..........',/)
105     Format (5X,'          C1 = (  ',G11.4,', ', G11.4, ')',/ )
106     Format (//,5X,' ........ Calculate derivative of the dominant',
     *             ' eigenvalue ..................',/)
107     Format (//,5X,'          D(Lambda)/D(Gr) = (  ',
     *                             G11.4,', ', G11.4, ')',/ )
108     Format (15X,'Error in calculation of Omega       =', G11.4)
109     Format (15X,'Error in calculation of Amu2        =', G11.4)
110     Format (15X,'Error in calculation of Tau2        =', G11.4)
111     Format (15X,'Error in calculation of Beta        =', G11.4)
112     Format (15X,'Error in calculation of Dalpha/DGr  =', G11.4)
113     Format (15X,'Error in calculation of C1          =', 2G11.4)
114     Format (/,10X,'Calculated estimations of errors:')
200     Format (/,' Step 3 is successfully completed',/, 80('_'),//)

129     Format (' Error in BifGel: N=', I5, ' is wrong (N>2)')
130     Format (' Error in BifGel: U=', G15.8,' is wrong (U>0)')
131     Format (' Error in BifGel: Mth=',I5,'  is wrong')
132     Format (' Error in BifGel: Jjob=',I5,'  is wrong')
133     Format (' Error in BifGel: Iprint=',I5,'  is wrong')
135     Format (' Error return from AnuCrt called by BifGel: ier=', I3)
137     Format (' Error return from C1PNF called by BifGel: ier=', I3)
138     Format (' Error return from Deval1 called by BifGel: ier=', I3)
139     Format (' Error in BifGel: D(alpha)/D(Anu) = 0')
140     Format (' Problem in BifGel: Omega = 0',/, 
     *            '     This is not Hopf bifurcation')
150     Format (' BifGel: weakly nonlinear analysis failed')
      End


C ******* Subroutine to check eigenvectors ***************************

	Subroutine ChEig (A, U, V, N, Omega)
	 Implicit Real*8 (a-h, o-z)

	  Dimension A(N,N)
	  Complex*16  U(N), V(N)
	  
	  Complex*16, Allocatable, Dimension(:) :: SC, Omg 

C =====================================================================
             Allocate ( SC(N), Omg(N), stat=jerr)
C =====================================================================

             EpsErr =1.D-04

C .......... Check left eigenvector ..................

                SC(1:N) = MATMUL( Transpose( A(1:N,1:N) ), U(1:N) )
               Omg(1:N) = Omega * Dcmplx(0.D0, 1.D0) * U(1:N)

               Omgm = MaxVal( Abs( SC(1:N) - Omg(1:N) ) )
               Vmod = MaxVal( Abs( U(1:N) ) )

               If (Vmod .GT. 1.D0)   Omgm = Omgm / Vmod
               If (Omgm .LE. EpsErr) Omgm = 0.D0
 
               Write (*,101) Omgm

C .......... Check right eigenvector ..................


	     SC(1:N) = MATMUL( A(1:N,1:N), V(1:N) )
              Omg(1:N) = Omega * Dcmplx(0.D0, 1.D0) * V(1:N)

              Omgm = MaxVal( Abs( SC(1:N) - Omg(1:N) ) )
              Vmod = MaxVal( Abs( U(1:N) ) )

              If (Vmod .GT. 1.D0)   Omgm = Omgm / Vmod
              If (Omgm .LE. EpsErr) Omgm = 0.D0
 
              Write (*,102) Omgm

C ..........................................
              DeAllocate ( SC, Omg, stat=jerr)
	 Return
101         Format (15X,'Error in left  eigenvector is ',G11.4)
102         Format (15X,'Error in right eigenvector is ',G11.4)
	End


C ********** Calculate eigenvalues using QR decomposition *************

      Subroutine EVALS (Fname, XS, N,   Anu,    Eps, Nsig, Xref, 
     1                  ItMax, EV, ev1, Iprint, ier         )
       Implicit Real*8 (A-H,O-Z)

        Dimension XS(N), Xref(N)
        Complex*16 EV(N), ev1, evi
        External Fname

         Real*8, Allocatable, Dimension(:)   :: WF
         Real*8, Allocatable, Dimension(:,:) :: A

C ============ Allocate data  ==============================
         NW = N * (N + 2)
         Allocate ( WF(N), A(N,N), stat=jerr )
C ==========================================================

         If (N .LT. 1) then 
                           ier = 129
                            Write (*,129) N
                            goto 999
CLAB                            Return
         End If


C ....... Get stationary point .................................

         NitXS = ItMax

         C a l l   NWTN (Fname, XS,   N,     Anu, Eps, 
     1                   Nsig,  Xref, NitXS, ier      )

             If (ier .NE. 0) then
                                 ier = 130
                                 Write (*,130) ier
                                 goto 999
CLAB                                 Return
             End If   

C ....... Get Jacobian matrix ..............................

         C a l l   Fname (XS, N, Anu, WF, A, 1)

C ....... Get eigenvalues .................................

         C a l l   EIGR (A, N, N, EV, ier)

             If (ier .NE. 0) then
                                 ier = 131
                                 Write (*,131) ier
                                 goto 999
CLAB                                 Return
             End If   

C ........ Choose dominant eigenvalue .......................

         ev1 = EV(1)

         If (N .GT. 1) then
                           Do I=2,N
                              REev1 = Real(ev1)
                              evi   = EV(i)
                              REevi =  Real(evi)

                             If ( (REevi .GT. REev1)        .OR.
     1                            ( (REevi      .EQ. REev1) .AND.
     2                              (Aimag(evi) .GT. Aimag(ev1)) )
     3                           ) ev1 = evi
                           End Do
          End If

C ....... Print the result .......................................

          If (Iprint .GT. 1) then
                                  Write (*,102) Anu, ev1
          End If

C ....... Normal return ...........................................
   
        ItMax = NitXS
999     continue
        DeAllocate ( WF, A, stat=jerr )

       Return
102     Format (/,10X, 'Grashof number=', G15.8,/,10X, 'Eigenvalue:',
     *         5X,'  Re(Lambda)=',G11.4, 5X, 'Im(Lambda)=', G11.4)  
129     Format (' Error in Evals: N=', I5, ' is wrong (N>1)')
130     Format (' Error return from NWTN called by Evals: ier=', I3)
131     Format (' Error return from Eigr called by Evals: ier=', I3)
      End

C ****** Inverse iteration algorithm ********************************
C  to calculate eigenvalue together with left and right eigenvectors

      Subroutine INITER (AC, N, EVAL, Nsig, ItMax, Ijob, V, U, ier)

       Implicit Real*8(A-H,O-Z)

        Complex*16 AC(N,N), EVAL, V(N), U(N)
        Complex*16 EVAL9, Cfac, Cfac1, ZDUM

        Complex*16, Allocatable, Dimension(:) :: WC
        Integer,    Allocatable, Dimension(:) :: IPIV

        Cabs2(ZDUM) =  Real(ZDUM)**2 + Aimag(ZDUM)**2

C ============= Allocate data =====================================
         Allocate ( WC(N), IPIV(N), stat=jerr )
C =================================================================

C ....... Check input data ................................

           If (N .LT. 1) then 
                             ier = 129
                             Write (*,129) N
                             goto 999
CLAB                             Return
           End If   

           If (ItMax .LT. 1) then
                                 ier = 130
                                 Write (*,130) ItMax
                                 goto 999
CLAB                                 Return
           End If   

           If ((Ijob.LT.1) .OR. (Ijob.GT.2)) then
                                 ier = 131
                                 Write (*,131) Ijob
                                 goto 999
CLAB                                 Return
           End If   

C ............. Prepare initial data .......................           
    
           EpsN2 = 10.0D0**(-2*Nsig)
           EVAL9 = 0.99999D0 * EVAL

           N2  = N**2
           NP1 = N + 1

           Do  ii=1,N
               AC(ii,ii) = AC(ii,ii) - EVAL9
           End Do 

C ......... Zero itration for V ...............................

           C a l l   Zgetrf (N, N, AC, N, IPIV, info)
                If (info .NE. 0) then
                                      ier = 132
                                      Write (*,132) info
                                      goto 999
CLAB                                      Return
                End If

           C a l l   Enrml(V,N,Cfac,ier)
               If (ier .NE. 0) then
                                    ier = 133
                                    Write (*,133) ier
                                    goto 999
CLAB                                    Return
               End If

           WC(1:N) = V(1:N)
 
C ........ Start iterations for V ...................................

           K = 1

          IterV: Do While (K .LE. ItMax)

            C a l l    Zgetrs ('N', N, 1, AC, N, IPIV, V, N, info)

	  Cfac1 = Cfac

            C a l l    Enrml (V, N, Cfac, ier)

               If (ier .NE. 0) then
                                    ier = 134
                                     Write (*,133) ier
                                     goto 999
CLAB                                     Return
               End If

            Enorm1 = Abs(Cfac1 - Cfac)

            Enorm2 = 0.0D0

            Do i=1,N
		  Vmod = Abs(V(i))
		  Vdif = Cabs2(V(i)-WC(i))

		  If (Vmod .GE. EpsN2) then
                                        Enorm2 = Enorm2 + Vdif/Vmod
				    else
			           Enorm2 = Enorm2 + Vdif
		  End If

                     WC(i) = V(i)
            End Do
            
            
            NITV = K

C ........ Convergence ??? .........................................

            If (Enorm2 .LE. EpsN2 .AND. Enorm1 .LE. EpsN2) Exit

                             K = K + 1
          End Do IterV

          If (ier .NE. 0) then
                              ier = 135
                              Write (*,135) ier
                              goto 999
CLAB                              Return
          End If

 

	Eval = Eval9 + Cfac

          If (Ijob .EQ. 1) then
                                 Itmax = NITV
                                 goto 999
CLAB                                 Return
          End If                       

C ......... Zero itration for U ...............................

         C a l l   Enrml (U, N, Cfac, ier)
               If (ier .NE. 0) then
                                    ier = 136
                                    Write (*,133) ier
                                    goto 999
CLAB                                    Return
               End If

         U(1:N)  = Conjg( U(1:N) )
         WC(1:N) =        U(1:N)

C ........ Start iterations for V ...................................

           K = 1

          IterU: Do While (K .LE. ItMax)

              C a l l    Zgetrs ('C', N, 1, AC, N, IPIV, U, N, info)

	    Cfac1 = Cfac

              C a l l   Enrml (U, N, Cfac, ier)
               If (ier .NE. 0) then
                                    ier = 137
                                    Write (*,133) ier
                                    goto 999
CLAB                                    Return
               End If

               Enorm1 = Abs(Cfac1 - Cfac)
               Enorm2 = 0.D0

               Do I=1,N
		Vmod = Abs( U(i) )
		Vdif = Cabs2( U(i) - WC(i) )

		If (Vmod .GE. EpsN2) then
				Enorm2 = Enorm2 + Vdif/Vmod
				     else
				Enorm2 = Enorm2 + Vdif
		End If

                   WC(i) = U(i)
               End Do

               NITU = K

C ........ Convergence ??? .........................................

              If (Enorm2 .LE. EpsN2 .AND. Enorm1 .LE. EpsN2) Exit

                             K = K + 1
          End Do IterU

          If (ier .NE. 0) then
                              ier = 138
                              Write (*,135) ier
                              goto 999
CLAB                              Return
          End If

         U(1:N) = Conjg( U(1:N) )

C ......... Normal Return .......................................

         ItMax = NITV + NITU
999    continue
       Deallocate ( IPIV, WC, stat=jerr )
       Return
129     Format (' Error in Initer: N=', I5, ' is wrong (N>1)')
130     Format (' Error in Initer: ItMax=', I2, ' is wrong (ItMax>0)')
131     Format (' Error in Initer: Ijob=', I2, ' is wrong ')
132     Format (' Error return from Zgetrf called by Initer: info=', I5)
133     Format (' Error return from Enmrl called by Initer: ier=', I3)
135     Format (' Initer: iterations do not converge: ier=', I3)
      End


C ****** Calculate numbers  G20, G11, G02, and vectors H20, H11 **********

      Subroutine PRJCT2(U, V, N, F20, F11, G20, G11, G02,
     1                                          H20, H11 )
       Implicit Real*8(A-H,O-Z)

        Dimension F11(N), H11(N)
        Complex*16 U(N), V(N), F20(N), G20, G11, G02, H20(N)
        Complex*16 UI, F20I, VI

C ================================================================

         S1 = 2.D0 * DOT_PRODUCT(  Real( U(1:N) ),  Real( F20(1:N) ) )
         S2 = 2.D0 * DOT_PRODUCT( Aimag( U(1:N) ),  Real( F20(1:N) ) )
         S3 = 2.D0 * DOT_PRODUCT(  Real( U(1:N) ), Aimag( F20(1:N) ) )
         S4 = 2.D0 * DOT_PRODUCT( Aimag( U(1:N) ), Aimag( F20(1:N) ) )
         S5 = 2.D0 * DOT_PRODUCT(  Real( U(1:N) ),        F11(1:N)   )
         S6 = 2.D0 * DOT_PRODUCT( Aimag( U(1:N) ),        F11(1:N)   )

         G20 =  Dcmplx( S1-S4, S2+S3 )
         G11 =  Dcmplx(  S5,    S6   )
         G02 =  Dcmplx( S1+S4, S2-S3 )

         H20(1:N) = F20(1:N) - 
     -               Dcmplx( S1*Real( V(1:N) ) - S2*Aimag( V(1:N) ),
     ,                      S3*Real( V(1:N) ) - S4*Aimag( V(1:N) )  )
         
         H11(1:N) = F11(1:N) - S5*Real( V(1:N) ) + S6*Aimag( V(1:N) )

       Return
      End

C ********** Calculate vectors W20 and W11 ****************************8

      Subroutine CMAN2 (W, N, NW, Omega, H20, H11, W20, W11, WK, ier)
       Implicit Real*8 (A-H,O-Z)
 
        Dimension W(NW), H11(N), W11(N), WK(N)
        Complex*16 H20(N), W20(N)

C ===================================================================

C .......... Initial data ..........................................

           N2   = N**2
           La   = 1
           LWK1 = La + N2
           Lac  = 1

           W(LWK1:LWK1+N2-1) = W(La:La+N2-1)

c        C a l l   Dcopy (N2, W(La), 1, W(LWK1), 1)
 
C ........ Solve A W11 = - H11 ..........

          W11(1:N) = - H11(1:N)

         C a l l   Dgetrf (N, N, W(La), N, WK, info)
            If (info .NE. 0) then
                                  ier = 129
                                  Write (*,129) info
                                  Return
            End If
            
         C a l l    Dgetrs ('N', N, 1, W(La), N, WK, W11, N, info)

C ...... Store A as a complex matrix AC ...........
 
         Do i=1,N2
           W(Lac+2*i-2) = W(LWK1+i-1)
           W(Lac+2*i-1) = 0.D0
         End Do

C ...... Setup R.H.S and matrix ...................
C         W = A - 2*I*Omega*IDENTITY

         ii     = 2
         IINC   = 2*(N+1)
         Omega2 = 2.0D0*Omega

         Do i=1,N
            W20(i) = -H20(i)
            W(ii)  = W(ii) - Omega2
            ii     = ii + IINC
         End Do

C ......... Solve the system W W20 = -H20 ..........

         C a l l   Zgetrf (N, N, W(Lac), N, WK, info)
            If (info .NE. 0) then
                                  ier = 130
                                  Write (*,130) info
                                  Return
            End If

         C a l l    Zgetrs ('N', N, 1, W(Lac), N, WK, W20, N, info)
 
       Return
129     Format (' Error return from Dgetrf called by CMAN2: info=', I5)
130     Format (' Error return from Zgetrf called by CMAN2: info=', I5)
      End

C ********* Scaling of X **********************************

      Function Ref(X)
       Implicit Real*8 (A-H,O-Z)

        Ref =  Abs(X)
        If (Ref .LT. 1.D0) Ref = 1.D0

       Return
      End

C ********** Subroutine to find a critical value ********************

      Subroutine AnuCRT(Fname, EvName, X,    N,      Anu,   Omega, 
     1                  Eps,   Nsig,   XRef, AnuRef, ItMax, U, 
     2                  Iprint, ier)

       Implicit Real*8 (A-H,O-Z)

        Dimension X(N), XRef(N)
        Complex*16 ev1, ev1L, EvDif
        External Fname, EvName

        Real*8    , Allocatable, Dimension(:) :: WX
        Complex*16, Allocatable, Dimension(:) :: EV, EVL

C =============== Allocate data =============================
        Allocate ( WX(N), EV(N), EVL(N), stat=jerr)
C ===========================================================

C ........ Check input data .....................................

         If (N .LT. 2) then 
                           ier = 129
                            Write (*,129) N
                            goto 999
CLAB                            Return
         End If

         Do i=1,N
            If (XRef(i) .EQ. 0.D0) then
                                       ier=130
                                       Write (*,130) i
                                       goto 999
CLAB                                       Return
            End If
         End Do

         If (AnuRef .LE. 0.0D0) then
                                    ier = 131
                                    Write (*,131) 
                                    goto 999
CLAB                                    Return
         End If

         If (U .LE. 0.D0) then
                              ier = 132
                              Write (*,132) 
                              goto 999
CLAB                              Return
         End If

C .......... Initial data .............................

      EpsN = 10.D0**(-Nsig)

      NitAnu = 0
      NC     = 2*N
      Nit    = ItMax

C ....... Dominant eigenvalue for Anu .........................

      Write (*,101) NitAnu

      C a l l    Evals ( Fname, X,   N,   Anu,    Eps, Nsig, XRef, 
     1                   Nit,   EV,  ev1, Iprint, ier            )

       If (ier .NE. 0) then
                           ier = 133
                           Write (*,133) ier
                           goto 999
CLAB                           Return
       End If

       REev1 =  Real(ev1)

       If ( Abs(REev1) .LT. Eps) then
          Omega = Aimag(ev1)
          ItMax = NitAnu
          ier   = 0
          
          goto 999
CLAB          Return
       End If

C ........ Dominant eigenvalue for next giess of Anu ...................

       DAnu   =  Sqrt(U) * AnuRef
       AnuL   = Anu + DAnu
       ev1L   = ev1
       Nit    = ItMax
       NitAnu = 1

        WX(1:N) = X(1:N)
       EVL(1:N) = Dcmplx(1.D0,0.D0)

       Write (*,101) NitAnu

       C a l l    EvName ( Fname, WX,  N,    AnuL,   Eps, Nsig, XRef, 
     1                     Nit,   EVL, ev1L, Iprint, ier             )

       If (ier .NE. 0) then
                           ier = 134
                           Write (*,134) ier
                           goto 999
CLAB                           Return
       End If

       REev1L =  Real(ev1L)

       EV(1:N) = EVL(1:N)

C ..... Iterations to find critical value of Anu ...................

       K = 2
       
       Do While ( K .LE. ItMax)

         If (REev1 .EQ. REev1L) then
                                     ier = 135
                                     Write (*,135) 
                                     goto 999
CLAB                                     Return
         End If

         Fac  = REev1 / ( REev1 - REev1L )
         Dif  = Anu - AnuL
         AnuL = Anu
         Anu  = Anu - Dif * Fac

         NitAnu = K

         C a l l    Extrap ( WX,   X, Fac, N)
         C a l l    Extrap ( EVL, EV, Fac, NC)

         EvDif  = ev1 - ev1L
         ev1L   = ev1
         REev1L = REev1
         ev1    = ev1 - EvDif * Fac

         Nit = ItMax

         Write (*,101) NitAnu
 
         C a l l    EvName(Fname, X,  N,   Anu,    Eps, Nsig, XRef, 
     1                     Nit,   EV, ev1, Iprint, ier         )

           If (ier .NE. 0) then
                               ier = 136
                               Write (*,134) ier
                               goto 999
CLAB                               Return
           End If

         REev1 =  Real(ev1)

C ............. Convergence ??? ............................

         If ( Abs(REev1) .LT. Eps) Exit
         If ( Abs((Anu-AnuL)/AnuRef) .LT. EpsN) Exit

         K = K + 1
       End Do
        
        If (K .GT. ItMax) then
                              ier = 137
                               Write (*,137) ier
                               goto 999
CLAB                               Return
        End If

C ............. Normal return ...............................

        Omega = Aimag(ev1)
        ItMax = NitAnu

999     continue
        Deallocate ( WX, EV, EVL, stat=jerr )
        Return
101     Format (/, 85('.'),/,
     *          5X,'Iteration of linear stability analysis # ', I2,/)
129     Format (' Error in AnuCrt: N=', I5, ' is wrong (N>2)')
130     Format (' Error in AnuCrt: Xref(',I5,')=0  is wrong')
131     Format (' Error in AnuCrt:  AnuRef = 0  is wrong ')
132     Format (' Error in AnuCrt: U=', G15.8,' is wrong (U>0)')
133     Format (' Error return from Evals called by AnuCrt: ier=', I3)
134     Format (' Error return from EvName called by AnuCrt: ier=', I3)
135     Format (' Error in AnuCrt: D(alpha)/D(Anu) = 0')
137     Format (' AnuCrt: Convergence of critical value is not reached',
     *              '   ier=', I3)
      End


C ******** Extrapolation to the next initial guess *******************

      Subroutine Extrap (AL, A, Fac, N)

       Implicit Real*8(A-H,O-Z)

        Dimension AL(N),A(N)
C =================================================================

         If (N .LT. 1) Return

         Do i=1,N
            AI    = A(i)  
            Dif   = AI - AL(i)  
            AL(i) = AI
            A(i)  = AI - Dif*Fac
         End Do

       Return
      End

C ******** Calculation of an eigenvalue by inverse iteration ************

        Subroutine EVAL1(Fname, XS,    N,   Anu,    Eps, Nsig, XRef, 
     1                   ItMax, V1,    ev1, Iprint, ier           )
         Implicit Real*8(A-H,O-Z)

          Dimension XS(N), XRef(N) 
          Complex*16 V1(N), ev1, Udum
          External Fname

          Real*8,     Allocatable, Dimension(:)   :: WF
          Real*8,     Allocatable, Dimension(:,:) :: A
          Complex*16, Allocatable, Dimension(:,:) :: AC

C ============== Allocate data =============================
         Allocate ( WF(N), A(N,N), AC(N,N), stat=jerr )
C ==========================================================

C ......... Initial preparations ..............................

           If (N .LT. 1) then 
                             ier = 129
                             Write (*,129) N
                             goto 999
CLAB                             Return
           End If   

          N2    = N**2
          NitXS = ItMax

C ....... Get stationary point .................................

          C a l l    NWTN (Fname, XS, N, Anu, Eps, Nsig, 
     *                                     XRef, NitXS, ier)
             If (ier .NE. 0) then
                                 ier = 130
                                 Write (*,130) ier
                                 goto 999
CLAB                                 Return
             End If   
          
C ....... Get Jacobian matrix ..............................

          C a l l    Fname(XS, N, Anu, WF, A, 1)

          AC(1:N,1:N) = Dcmplx( A(1:N,1:N), 0.D0)

C ....... Call Initer .......................................

          NitEv = ItMax

          C a l l    Initer (AC, N, ev1, Nsig, NitEv, 1, V1, Udum, ier)

             If (ier .NE. 0) then
                                 ier = 131
                                 Write (*,131) ier
                                 goto 999
CLAB                                 Return
             End If   

C ....... Print the result ..................................

          If (Iprint .GT. 1) then
                                  Write (*,102) Anu, ev1
          End If

C ......... Normal Return ..........................................

          ItMax = NitXS + NitEV

999       continue
          Deallocate ( WF, A, AC, stat=jerr )
       Return
102     Format (/,10X, ' Grashof number=', G15.8,/,10X, ' Eigenvalue:',
     *         5X,'  Re(Lambda)=',G11.4, 5X, 'Im(Lambda)=', G11.4)  
129     Format (' Error in Eval1: N=', I5, ' is wrong (N>1)')
130     Format (' Error return from NWTN called by Eval1: ier=', I3)
131     Format (' Error return from Initer called by Eval1: ier=', I3)
      End
      

C *********** Calculation of the derivative of the dominant eigenvalue ****** 

       Subroutine Deval1 (Fname,  X,     N,  Anu, Eps,  Nsig, XRef,   
     1                    AnuRef, ItMax, U,  V1,  ev1,  Jjob, Dev,  
     2                    DevErr, ier    )
        Implicit Real*8(A-H,O-Z)

         Dimension X(N), XRef(N)
         Complex*16 V1(N), ev1, Dev, DevErr, Evpm(2), DevS(2)
         External Fname

C =================================================================

C ......... Check input data ......................................

            If (N .LT. 0) then 
                               ier = 129
                               Write (*,129) N
                               Return
            End If

            If (U .LE. 0.D0) then 
                                 ier = 130
                                 Write (*,130) U
                                  Return
            End If

            Do i=1,N
                  If (XRef(i) .EQ. 0.D0) then
                                             ier=131
                                             Write (*,131) i
                                             Return
                 End If
            End Do

            If (AnuRef .LE. 0.0D0) then
                                       ier = 132
                                        Write (*,132) 
                                        Return
            End If
   
C ........... Start calculations .............................

          NitSum = 0
	
	If (Jjob .EQ. 1) then
	                     Jjob1 = 2
	                 else
	                     Jjob1 = 1
	End If

          Do j=1,Jjob1
               If (J .EQ. 1) UU = U
               If (J .EQ. 2) UU = 2.0D0*U

               DAnu = UU * AnuRef

               Do ipm=1,2
                  If (ipm .EQ. 1) AnuPM = Anu + DAnu
                  If (ipm .EQ. 2) AnuPM = Anu - DAnu

                  Evpm(ipm) = ev1

                  Nit    = ItMax
                  Iprint = 0

                  Write (*,101) AnuPM

                  C a l l    Eval1 (Fname, X,    N,   AnuPM, Eps,
     1                              Nsig,  XRef, Nit,    V1,
     2                              Evpm(ipm),   Iprint, ier )
                    If (ier .NE. 0) then
                                         ier = 133
                                         Write (*,133) ier
                                         Return
                    End If

                    NitSum = NitSum + Nit
   
               End Do 

              Dev     = (Evpm(1) - Evpm(2))/(2.0D0*DAnu)
              DevS(j) = Dev

          End Do 

          Dev = DevS(1)

          If(Jjob .EQ. 1) then
                          p = Abs( DevS(1) )
                          DevErr = (DevS(2) - DevS(1))
                          If (p .GT. 1.D0) DevErr = DevErr /p
          End If                              

C ......... Normal return .......................................

          ItMax = NitSum

       Return
101     Format (//,10X,'Calculate dominant eigenvalue at Gr=', G15.8)
129     Format (' Error in Deval1: N=', I5, ' is wrong (N>0)')
130     Format (' Error in BifGel: U=', G15.8,' is wrong (U>0)')
131     Format (' Error in Deval1: Xref(',I5,')=0  is wrong')
132     Format (' Error in Deval1:  AnuRef = 0  is wrong ')
133     Format (' Error return from Eval1 called by Deval1: ier=', I3)
      End


C *********** Normalize a vector ***********************

      Subroutine ENRML (V, N, CFac, ier)
       Implicit Real*8 (A-H,O-Z)

        Complex*16 V(N), CFac
        Complex*16 vi

C ======================================================

          If (N .LT. 1) then 
                            ier = 129
                            Write (*,129) N
                            Return
          End If


          Amx    = 0.0D0
          Imax   = 0
          Enorm2 = 0.0D0

          Do i=1,N
                Avi2 =  Real(V(i))**2 + Aimag(V(i))**2
                Enorm2 = Enorm2 + Avi2

                If (Amx .LT. Avi2) then
                                       Imax = i
                                       Amx = Avi2
                End If
          End Do

          If (Imax .EQ. 0) then
                                ier=130
                                Write (*,130)
                                Return
          End If

          vi   = V(Imax)
          Fac  = 1.D0 / Sqrt( Amx * Enorm2 )
          CFac =  Dcmplx ( Real(vi)*Fac, -Aimag(vi)*Fac )

          V(1:N) = Cfac * V(1:N)

         ier = 0

       Return
129     Format (' Error in Enrml: N=', I5, ' is wrong (N>2)')
130     Format (' Error in Enrml: zero vector is supplied')
      End


C ********* Calculate constant C1 and the Poincare normal form *********

      Subroutine C1PNF (Fname,  XS,     N,    Anu,   Omega, UU, Jjob, 
     1                  C1,     C1Err,  Nsig, ItMax, U,     V,
     2                  EnormX, EnormV, ier)
        Implicit Real*8(A-H,O-Z)

         Dimension XS(N)

         Complex*16 C1, C1Err, U(N), V(N)
         Complex*16 EVAL, CFac, G20, G11, G02, G21, CSum, C1S(2), zdum

         External Fname

         Cabs2(zdum) =  Real(zdum)**2 + Aimag(zdum)**2

         Real*8,     Allocatable, Dimension(:)   :: WF
         Real*8,     Allocatable, Dimension(:)   :: F11, H11
         Real*8,     Allocatable, Dimension(:,:) :: A
         Complex*16, Allocatable, Dimension(:,:) :: AC
         Complex*16, Allocatable, Dimension(:)   :: F20, H20

C ============== Allocate data ===========================================
         Allocate (  WF(N), A(N,N), AC(N,N), F11(N), F20(N), stat=jerr )
         Allocate ( H11(N), H20(N), stat=jerr )
C ========================================================================

C ........ Check initial data ....................................

            If (N .LT. 2) then 
                              ier = 129
                              Write (*,129) N
                              goto 999
CLAB                              Return
            End If

            If (Omega .EQ. 0.D0) then 
                                     ier = 130
                                     Write (*,130) 
                                     goto 999
CLAB                                     Return
            End If
   
            If (UU .LE. 0.D0) then 
                                  ier = 131
                                  Write (*,131) UU
                                  goto 999
CLAB                                  Return
            End If

            If (Jjob .GT. 5) then 
                                  ier = 132
                                  Write (*,132) Jjob
                                  goto 999
CLAB                                  Return
            End If
 
C ######## Find right eigenvector V and left      #############
C          eigenvector U for eigenvalue I*Omega

            C a l l    Fname(XS, N, Anu, WF, A, 0)
            C a l l    Fname(XS, N, Anu, WF, A, 1)

            AC(1:N,1:N) = Dcmplx( A(1:N,1:N), 0.D0  )

            EVAL =  Dcmplx (0.0D0, Omega)

            V(1:N) = Dcmplx( 1.D0, 0.D0 )
            U(1:N) = Dcmplx( 1.D0, 0.D0 )

            C a l l    Initer (AC, N, Eval, Nsig, ItMax, 2, V, U, ier)

               If (ier .NE. 0) then
                                  ier = 133
                                  Write (*,133) ier
                                  goto 999
CLAB                                  Return
               End If
 
C ....... Normalizing of V and U .................

           C a l l    BFNRML (V, N ,CFac, ier)
             If (ier .NE. 0) then 
                                 ier = 134
                                 Write (*,134) ier
                                 goto 999
CLAB                                 Return
             End If

           C a l l    RLNRML (U, V, N, CFac, ier)
             If (ier .NE. 0) then 
                                 ier = 135
                                 Write (*,134) ier
                                 goto 999
CLAB                                 Return
             End If
          
             If (Jjob .EQ. 3) goto 999
CLAB           If (Jjob .EQ. 3) Return
  
C ....... Compute weight factor for differencing .......
          
           EnormX =  Sqrt( DOT_PRODUCT(XS,XS)  )
           EnormV =  Sqrt( Real( DOT_PRODUCT(V,V) ) )

           If (EnormV .EQ. 0.D0) then
                                     ier = 136
                                     Write (*,136) ier
                                     Return
           End If   
           
           Fac = Ref( EnormX / EnormV )

C ######### Calculate second order derivatives of r.h.s. ###########

           C a l l   BifoAG (F11, F20, V, N)

           C a l l   PRJCT2 (U,   V,   N,   F20, F11,
     1                           G20, G11, G02, H20, H11 )

           C a l l    Fname (XS, N, Anu, WF, AC, 0)
           C a l l    Fname (XS, N, Anu, WF, AC, 1)
 
           NW = 2*N**2

           C a l l    CMAN2 (AC, N, NW, Omega, H20, H11, 
     *                                         F20, F11, WF, ier)
             If (ier .NE. 0) then
                                  ier = 137
                                  Write (*,137) ier
                                  goto 999
CLAB                                  Return
             End If   

C ######### Calculate third order derivatives of r.h.s. ###########
 
           C a l l    BifG21 (G21, F11, F20, V, U, N)
 
C ........ Calculate C1 ...........................................

           CSum = G20 * G11 - 2.0D0 * Cabs2(G11) - Cabs2(G02) / 3.0D0
           C1   = Dcmplx( 0.0D0, 0.5D0/Omega ) * CSum 
           C1   = C1 + Dcmplx( 0.5D0, 0.0D0 ) * G21

C ....... Normal return ...........................................
999      continue
         Deallocate (  WF, A, AC, F11, F20, H11, H20, stat=jerr )
       Return
129     Format (' Error in C1PNF: N=', I5, ' is wrong (N>2)')
130     Format (' Error in C1PNF: Omega=', G15.8,' is wrong (Omega<>0')
131     Format (' Error in C1PNF: UU=',G15.8,'  is wrong (UU>0)')
132     Format (' Error in C1PNF: Jjob=',I5,'  is wrong')
133     Format (' Error return from Initer called by C1PNF: ier=', I3)
134     Format (' Error return from BFNRML called by C1PNF: ier=', I3)
136     Format (' Error in C1PNF: Vnorm=0:   ier=', I3)
137     Format (' Error return from CMAN2 called by C1PNF: ier=', I3)
      End


C *********** Normalize a vector *********************************

      Subroutine BFNRML(V, N, CFac, ier)

       Implicit Real*8(A-H,O-Z)

        Complex*16 V(N)

C ===================================================================

          If (N .LT. 1) then 
                            ier = 129
                            Write (*,129) N
                            Return
          End If

          Vmax = 0.D0
          Imax = 0

          Do i=1,N
	      Vcur = Abs( V(i) )

	      If (Vcur .GT. Vmax) then
				  Vmax = Vcur
				  Imax = i
	     End If
          End Do      

          If (Vmax .EQ. 0.D0) then
                                   ier = 130
                                   Write (*,130) 
                                   Return
          End If
          
C .......... Normalize .........................

         V(1:N)   = V(1:N)  / V(Imax)

       Return
129     Format (' Error in BFNRML: N=', I5, ' is wrong (N>1)')
130     Format (' Error in BFNRML: zero vector is supplied')
      End


C *********** Normalize a vector *********************************

      Subroutine RLNRML(U, V, N, CFac, ier)
       Implicit Real*8(A-H,O-Z)

        Complex*16 U(N), V(N), UV

C ================================================================

          If (N .LT. 1) then 
                            ier = 129
                            Write (*,129) N
                            Return
          End If

          UV = DOT_PRODUCT( Conjg( U(1:N) ),  V(1:N) )


          If ( Abs(Real(UV))+ Abs(Aimag(UV)) .EQ. 0.D0) then

                            ier = 130
                            Write (*,130)
                            Return
          End If

C ......... Normalize ..................................

         U(1:N)   = U(1:N)  / UV

         ier = 0

       Return
129     Format (' Error in RLNRML: N=', I5, ' is wrong (N>1)')
130     Format (' Error in RLNRML: U and V are orthogonal')
      End

