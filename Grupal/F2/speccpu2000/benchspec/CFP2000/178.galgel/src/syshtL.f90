C ###############  SYSHT_L  #####################
c
c   Linear terms of energy equation

      Subroutine SysHTL (K, NKX, NKY, N, NX, NY)    

        Use razmer
        Use Parameters
        Use HtLin
        Use EHT
        Use Ttime
        Use DumBilin

         Implicit  Real*8 (A-H,O-Z)                                         

         Integer, Allocatable, Dimension(:) :: IPIV

C =============================================================
         Allocate ( IPIV(K), stat=jerr )
C =============================================================

C **** Coefficients in the heat transfer equation ***************
      
         Do L=1,NKX
          Do M=1,NKY
           Do i=1,NKX
            Do j=1,NKY

             HtTim(NKY*(L-1) + M,NKY*(i-1) + j) =  
     =                                TXX(i,L) *  TYY(j,M) * W                             
 
             HtLap(NKY*(L-1) + M,NKY*(i-1) + j) = 
     =                               TXX2(i,L) *  TYY(j,M) / W +
     +                                TXX(i,L) * TYY2(j,M) * W    
            End Do		
           End Do		
          End Do		
         End Do		

C **** Inverse the matrix ***********************************

         C a l l    Dgetrf (K, K, HtTim(1:K,1:K), K, IPIV, info)
               If (info .NE. 0) then
                       Write (*,*) 'syshtL(Dgetrf): info=',info
                       Stop
               End If

         C a l l     Dgetri (K, HtTim(1:K,1:K), K, IPIV, POP, K, info)
                If (info .NE. 0) then
                       Write (*,*) 'syshtL(Dgetri): info=',info
                       Stop
               End If

C **** Multiplication by inversed matrix ********

         HtLap(1:K,1:K) = MATMUL( HtTim(1:K,1:K), HtLap(1:K,1:K) )

         Deallocate ( IPIV, stat=jerr)
       Return
      End
