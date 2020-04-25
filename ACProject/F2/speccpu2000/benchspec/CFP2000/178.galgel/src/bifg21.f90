C ******************************************************
C *      Calculation of the number G21                 *
C *      for subroutine BifGel                         *
C *      in the case of dynamic system                 *
C *      with quadratic nonlinearity                   *
C *                                                    *
C *  W11(NS) - Intput array. Contains vector W11       *
C *  W20(NS) - Intput array. Contains vector W20       *                           
C *    X(NS) - Input  array. Contains left eigenvector *
C *    V(NS) - Input  array. Contains right eigenvector*
C *                                                    *
C *  Q1(NS), Q2(NS), Q3(NS),                           * 
C *                          VR, VI - Working area     *
C *  Other variables define the system                 *
C *                                                    *
C * A.Gelfgat 14.12.96                                 *
C ******************************************************

      Subroutine BifG21 (G21, W11, W20, V, U, NS)

       Use   razmer
       Use   Parameters
       Use   Numbers
       Use   Sizes
       Use   NsNel
       Use   HtNel
       Use   Ttime
       Use   DumBilin

        Implicit real*8 (a-h,o-z)

        Dimension W11(NS)
        Complex*16 W20(NS), V(NS), U(NS), G21, gp

        Real*8, Allocatable, Dimension(:) :: POP2, Q1, Q2, Q3, VR, VI

C =========== Allocate arrays ========================================

           Np = Max0(N,K)
           Allocate( POP2(Np), Q1(NS), Q2(NS), Q3(NS), VR(NS), VI(NS) )
C ====================================================================

	G21   = Dcmplx (0.D0, 0.D0)
	Pmax  = 0.D0

C #### Separate real and imaginary parts #############
C    of the eigenvector and create working arrays

C ........Q1 = 2*W11 + Real(W20)  .....................
C ........Q2 = 2*W11 - Real(W20)  .....................
C ........Q3 = Imag(W20)       .....................
C ........Q3 = Imag(W20)       .....................

          VR(1:NS) = DReal( V(1:NS) )
          VI(1:NS) = DImag( V(1:NS) )

          Q1(1:NS) = 2.D0 * W11(1:NS) + DReal( W20(1:NS) )
          Q2(1:NS) = 2.D0 * W11(1:NS) - DReal( W20(1:NS) )
          Q3(1:NS) = DImag( W20(1:NS) )

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
     =                   VXXX(i,IL,L) * VXXY(j,JL,M) +                 
     +                   VYXX(i,IL,L) * VYXY(j,JL,M) +  
     +                   VXYX(i,IL,L) * VXYY(j,JL,M) +                 
     +                   VYYX(i,IL,L) * VYYY(j,JL,M)        
           End Do		
          End Do		
         End Do		
        End Do		
 
C ............ Symmetrization ....................

        POP(1:N,1:N) = POP(1:N,1:N) + TRANSPOSE( POP(1:N,1:N) )

C ....... Poj1(L,j) = - B(L,i,j)*VR(i+K) .................

        POP1(1:N) = - MATMUL( POP(1:N,1:N), VR(K+1:K+N) )

        Poj1(NY*(L-1)+M,1:N) = POP1(1:N)

C ....... Poj2(L,j) = - B(L,i,j)*VI(i+K) .................

        POP1(1:N) = - MATMUL( POP(1:N,1:N), VI(K+1:K+N) )

        Poj2(NY*(L-1)+M,1:N) = POP1(1:N)

       End Do Ext2
      End Do Ext1

C %%%%%%%%%%%%%%%%%%%% Real part %%%%%%%%%%%%%%%%%%%%%%%

C ... POP2(L) = Poj1(L,j)*Q1(j+K) + Poj2(L,j)*Q3(j+K) .....

           POP2(1:N) = MATMUL( Poj1(1:N,1:N), Q1(K+1:K+N) ) +
     +                 MATMUL( Poj2(1:N,1:N), Q3(K+1:K+N) )

C ......... POP2(i) = EnsTim(i,j)*POP2(j) .................

           POP2(1:N) = MATMUL( EnsTim(1:N,1:N), POP2(1:N) )

C %%%%%%%%%%%%% Imaginary part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

C ... POP1(L) = Poj1(L,j)*Q3(j+K) + Poj2(L,j)*Q2(j+K) .....

           POP1(1:N) = MATMUL( Poj1(1:N,1:N), Q3(K+1:K+N) ) +
     +                 MATMUL( Poj2(1:N,1:N), Q2(K+1:K+N) )

C ......... POP1(i) = EnsTim(i,j)*POP1(j) .................

           POP1(1:N) = MATMUL( EnsTim(1:N,1:N), POP1(1:N) )

C ######## V(i) = Cmplx(POP2,POP1) ###################

          V(K+1:K+N) = Dcmplx( POP2(1:N), POP1(1:N) )

C *******************************************************

C  ..... - HtCon(i,j,l)*Y(j)*X(l) ...........................

      Ext3: Do L=1,NKX                                                   
       Ext4: Do M=1,NKY                                                   

        Do IL=1,NX
         Do JL=1,NY
          Do i=1,NKX                                                   
           Do j=1,NKY                                                   

	    POP(NKY*(I-1)+j, NY*(IL-1)+JL ) =
     =                  WXTX(IL,i,L) * WXTY(JL,j,M) +              
     +                  WYTX(IL,i,L) * WYTY(JL,j,M)        

           End Do		
          End Do		
         End Do		
        End Do		

C ....... Poj1(L,j) = - B(L,i,j)*VR(i) .................

          Poj1(NKY*(L-1)+M,1:N) = 
     =                 - MATMUL( TRANSPOSE( POP(1:K,1:N) ), VR(1:K) )

C ....... Poj2(L,j) = B(L,i,j)*VI(i) .................

          Poj2(NKY*(L-1)+M,1:N) = 
     =                 - MATMUL( TRANSPOSE( POP(1:K,1:N) ), VI(1:K) )

       End Do Ext4
      End Do Ext3

C %%%%%%%%%%%%%%%%%%%% Real part %%%%%%%%%%%%%%%%%%%%%%%

C ... POP2(L) = Poj1(L,j)*Q1(j+K) + Poj2(L,j)*Q3(j+K) .....

           POP2(1:K) = MATMUL( Poj1(1:K,1:N), Q1(K+1:K+N) ) +
     +                 MATMUL( Poj2(1:K,1:N), Q3(K+1:K+N) )

C ......... POP2(i) = HtTim(i,j)*POP2(j) .................

           POP2(1:K) = MATMUL( HtTim(1:K,1:K), POP2(1:K) )

C %%%%%%%%%%%%% Imaginary part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%

C ... POP1(L) = Poj1(L,j)*Q3(j+K) + Poj2(L,j)*Q2(j+K) .....

           POP1(1:K) = MATMUL( Poj1(1:K,1:N), Q3(K+1:K+N) ) +
     +                 MATMUL( Poj2(1:K,1:N), Q2(K+1:K+N) )

C ......... POP1(i) = HtTim(i,j)*POP1(j) .................

           POP1(1:K) = MATMUL( HtTim(1:K,1:K), POP1(1:K) )

C ######## V(i+N) = Cmplx(POP2,POP1) ###################

           V(1:K) = Dcmplx( POP2(1:K), POP1(1:K) )

C *******************************************************

C  ..... - HtCon(i,j,l)*Y(l)*X(j) ...........................

      Ext5: Do L=1,NKX                                                   
       Ext6: Do M=1,NKY                                                   

        Do IL=1,NX
         Do JL=1,NY
          Do i=1,NKX                                                   
           Do j=1,NKY                                                   

	   POP(NKY*(I-1)+j, NY*(IL-1)+JL ) =
     =                  WXTX(IL,i,L) * WXTY(JL,j,M) +              
     +                  WYTX(IL,i,L) * WYTY(JL,j,M)        
           End Do		
          End Do		
         End Do		
        End Do		

C ....... Poj1(L,j) = - B(L,i,j)*VR(j+K) .................

           Poj1(NKY*(L-1)+M,1:K) = - MATMUL( POP(1:K,1:N), VR(K+1:K+N) )

C ....... Poj2(L,j) = - B(L,i,j)*VI(j+K) .................

           Poj2(NKY*(L-1)+M,1:K) = - MATMUL( POP(1:K,1:N), VI(K+1:K+N) )

       End Do Ext6
      End Do Ext5

C %%%%%%%%%%%%%%%%%%%% Real part %%%%%%%%%%%%%%%%%%%%%%%

C ... POP2(L) = Poj1(L,j)*Q1(j) + Poj2(L,j)*Q3(j) .....

           POP2(1:K) = MATMUL( Poj1(1:K,1:K), Q1(1:K) ) +
     +                 MATMUL( Poj2(1:K,1:K), Q3(1:K) )

C ......... POP2(i) = HtTim(i,j)*POP2(j) .................

           POP2(1:N) = MATMUL( HtTim(1:K,1:K), POP2(1:K) )

C %%%%%%%%%%%%% Imaginary part %%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

C ... POP1(L) = Poj1(L,j)*Q3(j) + Poj2(L,j)*Q2(j) .....

           POP1(1:K) = MATMUL( Poj1(1:K,1:K), Q3(1:K) ) +
     +                 MATMUL( Poj2(1:K,1:K), Q2(1:K) )

C ......... POP1(i) = HtTim(i,j)*POP1(j) .................

           POP1(1:K) = MATMUL( HtTim(1:K,1:K), POP1(1:K) )

C ######## V(i+N) = V(i+N) + Cmplx(POP2,POP1) ###################

           V(1:K) = V(1:K) + Dcmplx( POP2(1:K), POP1(1:K) )

C ######## G21 = U(i)*V(i) ####################

	Do 85 i=1,NS
		gp  = U(i) * V(i)
		G21 = G21 + gp
85	Continue


          V(1:NS) = Dcmplx( VR(1:NS), VI(1:NS) )

 
           Deallocate( POP2, Q1, Q2, Q3, VR, Vi, stat=jerr )
       Return
      End
