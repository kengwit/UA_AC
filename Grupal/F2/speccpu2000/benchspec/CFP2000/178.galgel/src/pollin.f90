C %%%%%%%%%%%%%   POLLiN  %%%%%%%%%%%%%%%%%%%%%%%%%
c
c  Linear inner products of polynomials

       Subroutine  PolLin (NX, NY, NKX, NKY)

        Use   razmer
        Use   ProdL

         Implicit  Real*8 (A-H,O-Z)                                      

C ==============================================================

C ***** Calculation of inner products of polynomials ************

          NM = Max0(NX, NY, NKX, NKY) + 3

          Do i=0,NM
           Do j=0,NM
              FF(i,j) =  TT(i,j)
              F2F(i,j) = T2T(i,j)
           End Do		
          End Do		

          Do i=-1,NM
           Do j=-1,NM
              WW(i,j) =  UU(i,j)
             W2W(i,j) = U2U(i,j)
           End Do		
          End Do		

          Do i= 0,NM
           Do j=-1,NM
              FBW(i,j) = TBU(i,j)
           End Do		
          End Do		

       Return
      End
