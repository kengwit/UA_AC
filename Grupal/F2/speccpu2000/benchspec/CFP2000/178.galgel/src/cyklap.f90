C %%%%   Cyklap &&&&&&&&&&&&&&&&&& 
c
c Driver for bifurcation process
                         

      Subroutine  EvCykl (NS, NX, NY, NKX, NKY, X, ier)

        Use   razmer
        Use   Parameters

         Implicit Real*8 (A-H,O-Z)                                         
                                                                      
C .......................................................
C
C     JJOB = 0 - everything without estimation of error 
C          = 1 - everything with estimation of error
C          = 2 - only critical point
C          = 3 - critical point and eigenvector 
C          = 4 - eigenvector and nonlinear decomposition
C .......................................................

          Dimension X(NS ), PAR(10), Err(7)
          Complex*16 V1(M11), C1

          External   Diffun
!LAB
          namelist /EvCyklInput/ Mth, Jjob, Itype, U, Ieigr
          logical first
          save first
          data first /.true./
!LAB
C ================================================================

C  **** Definition of parameters of BIFOR2 ******
!LAB
         if(first) then
           first = .false.
           Mth    = 1
           Jjob   = 1
           Itype  = 2
           U      = 0.001d0
           Anu    = Gr
           Ieigr  = 1
           read(5,nml=EvCyklInput,end=901,err=901) 
901        continue
         endif
!LAB
           C a l l    BifGel (Diffun, X, NS, Anu, U, Mth, Jjob,
     *                                   Itype, PAR, V1, Err, ier)

            If (ier .NE. 0) then
                                Write (*,129) ier                                
                                Return
            End If

           Amu2  = PAR(1)
           Tau2  = PAR(2)
           Beta  = PAR(3)
           Omega = Par(8)


               Write (*,100)
               Write (*,101)  Anu, Omega, Amu2, Tau2, Beta
               
           GrN = Anu / W
           OmN = Omega / W

               Write (*,102)
               Write (*,103)  GrN, OmN

       Return
100      Format (' Final result:')
101      Format (/,'    Critical Grashof number = ',  G11.4 ,/,
     *             '    Critical Frequency      = ', G11.4,/,
     *             '    Parameter Mu            = ', G11.4,/,
     *             '    Parameter Tau           = ', G11.4,/,
     *             '    Floquet exponent        = ', G11.4 )
102      Format (//,' In the case of scaling of the temperature by ',
     *            /,'     temperature difference * height / length   :')
103      Format (/,'    Critical Grashof number = ',  G11.4 ,/,
     *             '    Critical Frequency      = ', G11.4     )

129      Format (' Error return from BifGel called by Cyklap: ier=',I5)
      End
