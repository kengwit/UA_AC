C %%%%%%%%%%%%%%%   NSXYL  %%%%%%%%%%%%%%%%%%%%%%%
c
c Galerkin projection of Lapalcian of velocity


       Subroutine  NsxyL ( NX, NY )

        Use razmer
        Use FNS
        Use NsLin
        Use ProdL

         Implicit  Real*8 (A-H,O-Z)                                      

C ==================================================================

C ******* Calculation of inner products ***************************

          Do i=1,NX
           Do j=1,NX
              VXX(i,j)  = XXYY(i-1,j-1,FF,  A,F1,F2,F3,F4,
     *                                        F1,F2,F3,F4)
              VXX2(i,j) = XXYY(i-1,j-1,F2F, A,F1,F2,F3,F4,
     *                                        F1,F2,F3,F4)
              VYX(i,j)  = XYYX(i-1,j-1,WW,  A,F1,F2,F3,F4,
     *                                        F1,F2,F3,F4)
              VYX2(i,j) = XYYX(i-1,j-1,W2W, A,F1,F2,F3,F4,
     *                                        F1,F2,F3,F4)
           End Do		
          End Do		

C .......................

          Do i=1,NY
           Do j=1,NY
              VYY(i,j)  = XXYY(i-1,j-1,FF,  B,G1,G2,G3,G4,
     *                                        G1,G2,G3,G4)
              VYY2(i,j) = XXYY(i-1,j-1,F2F, B,G1,G2,G3,G4,
     *                                        G1,G2,G3,G4)
              VXY(i,j)  = XYYX(i-1,j-1,WW,  B,G1,G2,G3,G4,
     *                                        G1,G2,G3,G4)
              VXY2(i,j) = XYYX(i-1,j-1,W2W, B,G1,G2,G3,G4,
     *                                        G1,G2,G3,G4)
           End Do		
          End Do		

       Return
      End
