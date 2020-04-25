C %%%%%%%%%%%%%%  HTXYL  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                       
C
C Galerkin projection of Laplacian of the temperature

       Subroutine  HtxyL (NKX,NKY)                                       

        Use razmer
        Use FHT
        Use ProdL
        Use HtLin

         Implicit  Real*8 (A-H,O-Z)                                      

C =================================================================                             

C ******* Calculation of inner products ***************************

         Do i=1,NKX
          Do j=1,NKX
            TXX(i,j)  = PTXY(i-1,j-1,FF,  V1,V2)
            TXX2(i,j) = PTXY(i-1,j-1,F2F, V1,V2)
            PXX(i,j)  = PPXY(i-1,j-1,FF,  V1,V2)
            PXX2(i,j) = PPXY(i-1,j-1,F2F, V1,V2)
          End Do		
         End Do		

         Do i=1,NKY
          Do j=1,NKY
            TYY(i,j)  = PTXY(i-1,j-1,FF,  T1,T2)
            TYY2(i,j) = PTXY(i-1,j-1,F2F, T1,T2)
            PYY(i,j)  = PPXY(i-1,j-1,FF,  T1,T2)
            PYY2(i,j) = PPXY(i-1,j-1,F2F, T1,T2)
          End Do		
         End Do		

       Return
      End
