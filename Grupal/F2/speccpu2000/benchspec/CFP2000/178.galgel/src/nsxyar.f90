C %%%%%%%%%%%%%%%%   NSXYAR  %%%%%%%%%%%%%%%%%%%%%
c
c  Galerkin projection of buoyancy force 

       Subroutine  NsxyAR (NX, NY, NKX, NKY)

        Use razmer
        Use FNS
        Use FHT
        Use ProdL
        Use NsArh

         Implicit  Real*8 (A-H,O-Z)                                      

C ===============================================================

C ******* Calculation of the inner products ***************

         Do i=1,NX
          Do j=1,NKX
            WTX(i,j) = VTX(i-1,j-1, FBW, F1,F2,F3,F4,V1,V2)
            QXT(i,j) = YXT(i-1,j-1, FBW, F1,F2,F3,F4)
          End Do		
         End Do		

         Do i=1,NY
          Do j=1,NKY
            WTY(i,j) = VTY(i-1,j-1, FF,B,G1,G2,G3,G4,T1,T2)
            QYT(i,j) = YYT(i-1,j-1, FF,B,G1,G2,G3,G4)
          End Do		
         End Do		

C ...............................................

       Return
      End
