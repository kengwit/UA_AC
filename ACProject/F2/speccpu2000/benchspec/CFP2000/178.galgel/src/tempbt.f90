C %%%%%%   TEMPBT  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c  Convective terms of energy equation

      Subroutine TempBT (N, NX, NY, K, NKX, NKY)   

        Use razmer
        Use Parameters
        Use HtLin
        Use HtNel
        Use EHT
        Use Ttime

         Implicit  Real*8 (A-H,O-Z)                                         

C =============================================================

C  ***** Initial distribution of the temperature ******                 

          C a l l     TmInit (BT, NKX, NKY)

C  ***** Coefficients with BT(NKX,NKY) *******

          Ext1: Do L=1,NKX                                                   
           Ext2: Do M=1,NKY                                                   

             Inn1: Do IL=1,NX
              Inn2: Do JL=1,NY

                           S = 0.D0
                Do i=1,NKX
                 Do j=1,NKY
                     S = S + BT(i,j)* ( WXPX(IL,i,L)*WXPY(JL,j,M) +
     +                                  WYPX(IL,i,L)*WYPY(JL,j,M)  )
                 End Do		
                End Do		

               HtVbt(NKY*(L-1) + M,NY*(IL-1) + JL) = S                                                

             End Do Inn2
            End Do Inn1


                           S = 0.D0
                Do i=1,NKX
                 Do j=1,NKY
                     S = S + BT(i,j)*( PXX2(i,L) *  PYY(j,M) / W +
     +                                  PXX(i,L) * PYY2(j,M) * W   )
                 End Do		
                End Do		

               HtLbt(NKY*(L-1) + M) = S                                                   

           End Do Ext2
          End Do Ext1

C **** Multiplication by the inversed matrix ******

         HtVbt(1:K,1:N) = MATMUL( HtTim(1:K,1:K), HtVbt(1:K,1:N) )
         HtLbt(1:K)     = MATMUL( HtTim(1:K,1:K), HtLbt(1:K) )
 
       Return
      End
