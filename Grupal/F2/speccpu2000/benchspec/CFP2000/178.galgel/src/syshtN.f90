C ###############  SYSHT_N  #####################

      Subroutine SysHTN (HtTim, Y, DX, A,
     *                           NS, K, NKX, NKY, N, NX, NY, Ind0)    

        Use   razmer
        Use   HtNel
        Use   DumBilin

         Implicit  Real*8 (A-H,O-Z)                                         

          Dimension HtTim(N11,N11), A(NS,NS)            
          Dimension Y(NS), DX(NS), Xp(nn)

C =============================================================

C **** Coefficients in the heat transfer equation ***************

           If (Ind0 .NE. 1) then
                                   ! Calculate r.h.s.
      
C ++++++ - HtCon(i,j,l)*Z(j)*X(l) ++++++++++++++++++++++++++++++++++++

                Ext1: Do L=1,NKX                                                   
                 Ext2: Do M=1,NKY                                                   

                   Do IL=1,NX
                    Do JL=1,NY
                     Do i=1,NKX
                      Do j=1,NKY

                         POP( NKY*(i-1)+j, NY*(IL-1)+JL ) = 
     =                             WXTX(IL,i,L) * WXTY(JL,j,M) +              
     +                             WYTX(IL,i,L) * WYTY(JL,j,M)
                      End Do		
                     End Do		
                    End Do		
                   End Do		

C .............. POP1(i) = POP(i,j)*X(j) ............................

                   POP1(1:K) = MATMUL( POP(1:K,1:N), Y(K+1:K+N) )

C .............. Poj3 = POP1 .......................................

                   Poj3( NKY*(L-1)+M, 1:K) = POP1(1:K)

C ............... Xp = <POP1,Z> ....................................

	         Xp(NKY*(L-1)+M) =  DOT_PRODUCT (Y(1:K), POP1(1:K) )

C ............... Poj4(*,i) = POP(j,i)*Z(j) .........................

                   Poj4( NKY*(L-1)+M,1:N) =
     =                     MATMUL( TRANSPOSE( POP(1:K,1:N) ), Y(1:K) )

                  End Do Ext2
                 End Do Ext1

C ............ DX = DX - HtTim*Xp ...........................

                 DX(1:K) = DX(1:K) - MATMUL( HtTim(1:K,1:K), Xp(1:K) )

           Else               

C ************ Jacobian ***************************************

C ...........A = A - HtTim * Poj3 .......................
 
                 A(1:K,1:K) = A(1:K,1:K) - 
     -                    MATMUL( HtTim(1:K,1:K), Poj3(1:K,1:K) )

C ...........A = A - HtTim * Poj4 .......................

                 A(1:K,K+1:K+N) = A(1:K,K+1:K+N) -
     -                     MATMUL( HtTim(1:K,1:K), Poj4(1:K,1:N) )

           End If              

       Return
      End
