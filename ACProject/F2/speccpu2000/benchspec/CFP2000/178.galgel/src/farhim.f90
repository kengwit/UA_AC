C %%%%%%%%%%%%%%   FARHIM %%%%%%%%%%%%%%%%%%%%%%%%
C
C       Galerkin projection of buoyancy force 


      Subroutine Farhim (N, NX, NY, K, NKX, NKY)

        Use   razmer
        Use   Parameters
        Use   NsArh
        Use   ENS
        Use   EHT
        Use   Ttime

         Implicit Real*8 (A-H,O-Z)                                         

C ================================================================

C  ****** Initial distribution of the temperature ******                    

          C a l l   Tminit (BT, NKX, NKY)

C **** Coefficients describing the buoyancy force *************

         Ext1: Do L=1,NX
          Ext2: Do M=1,NY

           Do i=1,NKX
            Do j=1,NKY
               EnsArh(NY*(L-1) + M,NKY*(i-1) + j) = 
     =                              WTX(L,i) * WTY(M,j) * W
            End Do
           End Do

              S = 0.D0

           Do i=1,NKX
            Do j=1,NKY
              S = S  +  BT(i,j) * QXT(L,i) * QYT(M,j) * W
            End Do
           End Do

             EnsFbt(NY*(L-1) + M) = S

          End Do Ext2
         End Do Ext1

C *** Mutiplication by the inversed matrix.******

         EnsArh(1:N,1:K) = MATMUL( EnsTim(1:N,1:N), EnsArh(1:N,1:K) )
         EnsFbt(1:N)     = MATMUL( EnsTim(1:N,1:N), EnsFbt(1:N) )

       Return
      End
