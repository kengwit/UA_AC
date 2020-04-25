C ******************************************************
C *      Calculation of vectors F11 and F20            *
C *      for subroutine BifGel                        *
C *      in the case of dynamic system                 *
C *      with quadratic nonlinearity                   *
C *                                                    *
C *  F11(NS) - Output array. Contains vector f11       *
C *  F20(NS) - Output array. Contains vector f20       *                           
C *    X(NS) - Input  array. Contains stationary point *
C *    V(NS) - Input  array. Contains eigenvector      *
C *                                                    *
C *  VR(NS), VI(NS) - Working area                     *
C *  Other variables define the system                 *                         C *                                                    *
C * A.Gelfgat 16.12.96                                 *
C ******************************************************

      Subroutine BifoAG (F11, F20, V, NS)

       Use   razmer
       Use   Parameters
       Use   Numbers
       Use   Sizes
       Use   NsNel
       Use   HtNel
       Use   Ttime
       Use   DumBilin

        Implicit real*8 (a-h,o-z)

         Dimension F11(NS)
         Complex*16 F20(NS), V(NS), Imnim

         Real*8, Allocatable, Dimension(:) :: POP2, VR, VI

C =========== Allocate variables ==============================

          Np = Max0(N,K)
           Allocate ( POP2(Np), VR(NS), VI(NS) )
C ============================================================

	Imnim = Dcmplx(0.D0, 1.D0)

C ..... Separate real and imaginary parts ...........
C               of the eigenvector

          VR(1:NS) = Dreal( V(1:NS) )
          VI(1:NS) = Dimag( V(1:NS) )

C ********* Calculate bilinear term in the **********
C              Navier-Stokes equation

C  #######  - EnsCon(i,j,l)*X(j)*X(l) ###############

      Ext1: Do L=1,NX
       Ext2: Do M=1,NY

        Do i=1,NX
         Do j=1,NY
          Do IL=1,NX
           Do JL=1,NY

	     POP(NY*(I-1)+j, NY*(IL-1)+JL ) =
     =                      VXXX(i,IL,L) * VXXY(j,JL,M) +                 
     +                      VYXX(i,IL,L) * VYXY(j,JL,M) +  
     +                      VXYX(i,IL,L) * VXYY(j,JL,M) +                 
     +                      VYYX(i,IL,L) * VYYY(j,JL,M)        
		
           End Do		
          End Do		
         End Do		
        End Do		

C ....... Poj1(L,j) = - B(L,i,j)*VR(i+K) .................

           Poj1(NY*(L-1)+M,1:N) = - MATMUL( POP(1:N,1:N), VR(K+1:K+N) )

C ....... Poj2(L,j) = B(L,i,j)*VI(i+K) .................

           Poj2(NY*(L-1)+M,1:N) = - MATMUL( POP(1:N,1:N), VI(K+1:K+N) )

       End Do Ext2
      End Do Ext1

C .... POP2 = Poj1(L,j)*VR(j+K) + Poj2(L,j)*VI(j+K) ................
C .... POP1 = Poj1(L,j)*VR(j+K) - Poj2(L,j)*VI(j+K) ................

        Poj3(1:N,1) = MATMUL( Poj1(1:N,1:N), VR(K+1:K+N) )
        Poj3(1:N,2) = MATMUL( Poj2(1:N,1:N), VI(K+1:K+N) )

        POP2(1:N) = Poj3(1:N,1) + Poj3(1:N,2)
        POP1(1:N) = Poj3(1:N,1) - Poj3(1:N,2)

C ######## Finite result for F11 (POP) ###################
C ######## and real part of F20 (POP1) ###################

C ... POP2(I) = 0.5* EnsTim(i,L)*POP2(L) ........
C ... POP1(I) = 0.5* EnsTim(i,L)*POP1(L) ........

        POP1(1:N) = 0.5D0 * MATMUL( EnsTim(1:N,1:N), POP1(1:N) )

        F11(K+1:K+N) = 0.5D0 * MATMUL( EnsTim(1:N,1:N), POP2(1:N) )

        F20(K+1:K+N) = Dcmplx( POP1(1:N), 0.D0 )

C ####### Start imaginary part of F20 ###################

C .... POP1 = Poj1(L,j)*VR(j+K) + Poj2(L,j)*VI(j+K) ................

        POP1(1:N) = MATMUL( Poj1(1:N,1:N), VI(K+1:K+N) ) +
     +              MATMUL( Poj2(1:N,1:N), VR(K+1:K+N) )

C ########## Finite result for Im(F20) (POP1) ##############

C ...  POP1(I) = 0.5* EnsTim(i,L)*POP1(L) ........

        POP1(1:N) = 0.5D0 * MATMUL( EnsTim(1:N,1:N), POP1(1:N) )

        F20(K+1:K+N) = F20(K+1:K+N)  +  Imnim * POP1(1:N)

C *******************************************************

C  ..... - HtCon(i,j,l)*Vf(j)*X(l) ...........................

      Ext3: Do L=1,NKX                                                   
       Ext4: Do M=1,NKY                                                   

        Do IL=1,NX
         Do JL=1,NY
          Do i=1,NKX                                                   
           Do j=1,NKY                                                   

	   POP(NKY*(I-1)+j, NY*(IL-1)+JL ) =
     =                 WXTX(IL,i,L) * WXTY(JL,j,M) +              
     +                 WYTX(IL,i,L) * WYTY(JL,j,M)        
           End Do		
          End Do		
         End Do		
        End Do		

C ....... Poj1(L,j) = - B(L,i,j)*VR(i) .................

          Poj1(NKY*(L-1)+M,1:N) = 
     =                   - MATMUL( TRANSPOSE( POP(1:K,1:N) ), VR(1:K) )

C ....... Poj2(L,j) = - B(L,i,j)*VI(i) .................

          Poj2(NKY*(L-1)+M,1:N) = 
     =                   - MATMUL( TRANSPOSE( POP(1:K,1:N) ), VI(1:K) )

       End Do Ext4
      End Do Ext3

C .... POP2 = Poj1(L,j)*VR(j+K) + Poj2(L,j)*VI(j+K) ................
C .... POP1 = Poj1(L,j)*VR(j+K) - Poj2(L,j)*VI(j+K) ................

        Poj3(1:K,1) = MATMUL( Poj1(1:K,1:N), VR(K+1:K+N) )
        Poj3(1:K,2) = MATMUL( Poj2(1:K,1:N), VI(K+1:K+N) )

        POP2(1:K) = Poj3(1:K,1) + Poj3(1:K,2)
        POP1(1:K) = Poj3(1:K,1) - Poj3(1:K,2)

C ######## Finite result for F11 (POP) ###################
C ######## and real part of F20 (POP1) ###################

C ...  POP(I) = 0.5* EnsTim(i,L)*POP(L) ........
C ... POP1(I) = 0.5* EnsTim(i,L)*POP1(L) ........

        POP1(1:K) = 0.5D0 * MATMUL( HtTim(1:K,1:K), POP1(1:K) )
         F11(1:K) = 0.5D0 * MATMUL( HtTim(1:K,1:K), POP2(1:K) )

         F20(1:K)  = Dcmplx( POP1(1:K), 0.D0 )

C ####### Start imaginary part of F20 ###################

C .... POP1 = Poj1(L,j)*VR(j+K) + Poj2(L,j)*VI(j+K) ................

       POP1(1:K) = MATMUL( Poj1(1:K,1:N), VI(K+1:K+N) ) +
     +             MATMUL( Poj2(1:K,1:N), VR(K+1:K+N) )

C ########## Finite result for Im(F20) (POP1) ##############

C ...  POP1(I) = 0.5* HtTim(i,L)*POP1(L) ........

       POP1(1:K) = 0.5D0 * MATMUL( HtTim(1:K,1:K), POP1(1:K) )

       F20(1:K) = F20(1:K)  +  Imnim * POP1(1:K)


           Deallocate ( POP2, VR, Vi, stat=jerr )
       Return
      End
