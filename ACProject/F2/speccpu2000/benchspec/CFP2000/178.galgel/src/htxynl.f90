C %%%%%%%%%%%%%%  HTXYNL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                        
c
c Galerkin projection of convective terms of the energy equation

       Subroutine  HtxyNL ( NX, NY, NKX ,NKY )                                

        Use razmer
        Use Parameters
        Use FNS
        Use FHT
        Use ProdN
        Use HtNel

         Implicit  Real*8 (A-H,O-Z)                                      

C =================================================================

C ******* Calculation of inner products ***************************

         Do i=1,NX
          Do j=1,NKX
           Do L=1,NKX
             WXPX(i,j,L) = VPXXYY(i-1,j-1,L-1,F1FF,A,F1,F2,F3,F4,V1,V2)
             WXTX(i,j,L) = VTXXYY(i-1,j-1,L-1,F1FF,A,F1,F2,F3,F4,V1,V2)
             WYPX(i,j,L) = VPXYYX(i-1,j-1,L-1,FWF, A,F1,F2,F3,F4,V1,V2)
             WYTX(i,j,L) = VTXYYX(i-1,j-1,L-1,FWF, A,F1,F2,F3,F4,V1,V2)
           End Do		
          End Do		
         End Do		

             WYTX(1:NX,1:NKX,1:NKX) = WYTX(1:NX,1:NKX,1:NKX) * W
             WYPX(1:NX,1:NKX,1:NKX) = WYPX(1:NX,1:NKX,1:NKX) * W

C .................................................................

         Do i=1,NY
          Do j=1,NKY
           Do L=1,NKY
             WYPY(i,j,L) = VPXXYY(i-1,j-1,L-1, F1FF,B,G1,G2,G3,G4,T1,T2)
             WYTY(i,j,L) = VTXXYY(i-1,j-1,L-1, F1FF,B,G1,G2,G3,G4,T1,T2)
             WXPY(i,j,L) = VPXYYX(i-1,j-1,L-1, FWF, B,G1,G2,G3,G4,T1,T2)
             WXTY(i,j,L) = VTXYYX(i-1,j-1,L-1, FWF, B,G1,G2,G3,G4,T1,T2)
           End Do		
          End Do		
         End Do		

       Return
      End
