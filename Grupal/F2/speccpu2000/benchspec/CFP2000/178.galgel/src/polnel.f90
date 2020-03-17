C %%%%%%%%%%%%%%%%   POLNEL  %%%%%%%%%%%%%%%%
c
c Bilinear inner products of polynomials

       Subroutine  PolNel (NX, NY, NKX, NKY)

        Use razmer
        Use  ProdN

         Implicit  Real*8 (A-H,O-Z)                                      

C =======================================================================

C ***** Calculation of inner products of polynomials *********************

         NM = Max0(NX, NY, NKX, NKY) + 3

         Do i=0,NM
          Do j=0,NM
           Do L=0,NM
              F1FF(i,j,L) = T1TT(i,j,L)
           End Do		
          End Do		
         End Do		
 
         Do i=0,NM
          Do j=-1,NM
           Do L=0,NM
              FWF(i,j,L)  = TUT(i,j,L)
           End Do		
          End Do		
         End Do		

         Do i=-1,NM
          Do j=-1,NM
           Do L=-1,NM
              WWW(i,j,L)  = UUU(i,j,L)
           End Do		
          End Do		
         End Do		

         Do i=-1,NM
          Do j= 0,NM
           Do L=-1,NM
              W1FW(i,j,L) = U1TU(i,j,L)
           End Do		
          End Do		
         End Do		

       Return
      End
