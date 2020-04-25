C %%%%%%%%%%%%%  NSXYNL  %%%%%%%%%%%%%%%%%%%
c
c Galerkin projection of convective terms of Navier-Stokes equation

       Subroutine  NsxyNL ( NX ,NY )

        Use razmer
        Use Parameters
        Use FNS
        Use ProdN
        Use NsNel

         Implicit Real*8 (A-H,O-Z)                                      

C ================================================================

C ******* Calculation of inner products **************************

         Do i=1,NX
          Do j=1,NX
           Do L=1,NX
              VXXX(i,j,L) = XXXYYY(i-1,j-1,L-1, F1FF,A,F1,F2,F3,F4,
     *                                                 F1,F2,F3,F4,
     *                                                 F1,F2,F3,F4)
              VXYX(i,j,L) = XYXYXY(i-1,j-1,L-1, FWF, A,F1,F2,F3,F4,
     *                                                 F1,F2,F3,F4,
     *                                                 F1,F2,F3,F4)
              VYXX(i,j,L) = XYYYXX(i-1,j-1,L-1, W1FW,A,F1,F2,F3,F4,
     *                                                 F1,F2,F3,F4,
     *                                                 F1,F2,F3,F4)
              VYYX(i,j,L) = XXYYYX(i-1,j-1,L-1, WWW, A,F1,F2,F3,F4,
     *                                                 F1,F2,F3,F4,
     *                                                 F1,F2,F3,F4)
          End Do		
         End Do		
        End Do		

              VXYX(1:NX,1:NX,1:NX) = VXYX(1:NX,1:NX,1:NX) * W
              VYYX(1:NX,1:NX,1:NX) = VYYX(1:NX,1:NX,1:NX) * W

C .......................................................

        Do i=1,NY
         Do j=1,NY
          Do L=1,NY
             VYYY(i,j,L) = XXXYYY(i-1,j-1,L-1, F1FF,B,G1,G2,G3,G4,
     *                                                G1,G2,G3,G4,
     *                                                G1,G2,G3,G4)
             VXXY(i,j,L) = XXYYYX(i-1,j-1,L-1, WWW, B,G1,G2,G3,G4,
     *                                                G1,G2,G3,G4,
     *                                                G1,G2,G3,G4)
             VXYY(i,j,L) = XYYYXX(i-1,j-1,L-1, W1FW,B,G1,G2,G3,G4,
     *                                                G1,G2,G3,G4,
     *                                                G1,G2,G3,G4)
             VYXY(i,j,L) = XYXYXY(i-1,j-1,L-1, FWF, B,G1,G2,G3,G4,
     *                                                G1,G2,G3,G4,
     *                                                G1,G2,G3,G4)
          End Do		
         End Do		
        End Do		

       Return
      End
