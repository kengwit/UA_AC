C %%%%%%% SysNS_N %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c  Convective terms of Navier-Stokes equation

      Subroutine  SysNSN (EnsTim, Y, DX, A, NS, N, K, NX, NY, Ind0)

        Use razmer
        Use NsNel
        Use DumBilin

         Implicit  Real*8 (A-H,O-Z)                                         

          Dimension EnsTim(N11,N11), A(NS,NS)
          Dimension Y(NS), DX(NS), Xp(nn)

C ===================================================================

           If (Ind0 .NE. 1) then

C ++++++ -EnsCon(i,j,l)*X(j)*X(l) ++++++++++++++++++++++++++++++++++++

               Ext1: Do L=1,NX
                Ext2: Do M=1,NY

                 Do i=1,NX
                  Do j=1,NY
                   Do IL=1,NX
                    Do JL=1,NY
                    
                   POP( NY*(i-1)+j, NY*(IL-1)+JL ) = 
     =                                   VXXX(i,IL,L) * VXXY(j,JL,M) +
     +                                   VYXX(i,IL,L) * VYXY(j,JL,M) +
     +                                   VXYX(i,IL,L) * VXYY(j,JL,M) +
     +                                   VYYX(i,IL,L) * VYYY(j,JL,M)
                    End Do		
                   End Do		
                  End Do		
                 End Do		

C .............. POP1(i) = POP(i,j)*X(j) ............................

                 POP1(1:N) = MATMUL( POP(1:N,1:N), Y(K+1:K+N)  )

C ............... Xp = <POP1,X> ....................................

	       Xp(NY*(L-1)+M) =  DOT_PRODUCT( Y(K+1:K+N), POP1(1:N) )

C ............... Poj1(i) = POP1(i) + POP(j,i)*X(j) .........................

                Poj1( NY*(L-1)+M,1:N) = POP1(1:N) + 
     +                MATMUL( TRANSPOSE( POP(1:N,1:N) ), Y(K+1:K+N) )

               End Do Ext2
              End Do Ext1

C ............ DX = DX - EnsTim*Xp ...........................

              DX(K+1:K+N) = DX(K+1:K+N) - 
     -                          MATMUL( EnsTim(1:N,1:N), Xp(1:N) )

           Else

C ************ Jacobian ***************************************

C ...........A = A - EnsTim * Poj1 .......................

              A(K+1:K+N,K+1:K+N) =  A(K+1:K+N,K+1:K+N) -
     -                  MATMUL( EnsTim(1:N,1:N), Poj1(1:N,1:N) )
 
           End If

      Return
      End
