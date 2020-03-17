C %%%%%%%    SysXYR   %%%%%%%%%

        Use   razmer
        Use   Numbers
        Use   Parameters
        Use   Sizes
        Use   FHT
        Use   FNS
        Use   NsLin
        Use   NsArh
        Use   NsNel
        Use   NsMF
        Use   HtLin
        Use   HtNel
        Use   ENS
        Use   Fmag
        Use   EHT
        Use   Ttime
        Use   DumBilin
!LAB
        Use   DynamParamM
!LAB
        Implicit Real*8 (A-H,O-Z)                                      
                
C ==================================================================

        NameList /GalGelInput/ Nx, Ny, W, Pr, NKx, NKy, DynamParam

!            Opening the files 

!       Open (1,  file='Conv.dat', status='old')

!           Input of parameters 

!        Read (1,101) Nx, NY
!LAB Set defaults.  Compiler cannot optimize with these since they are
!LAB                potentially read in from input file below
        DynamParam%GrStrt = 10.D0
        DynamParam%GrEnd  = 1.D+5
        DynamParam%GrMul  = 10.D0
         W  = 4.D0
         Pr = 0.015d0
!LAB     from EvNewt
        DynamParam%Eps   = 1.D-06
        DynamParam%Nsig  = 6
        DynamParam%ItMax = 20

!LAB  namelist read input to foil aggressive optimizers
         Read (5,nml=GalGelInput)

!       Print the title 

           Write (*,200)
           Write (*,201)
           Write (*,202) W, Pr, Nx, Ny

!         Define necesary integers

!LAB done in input file now            NKx = Nx
!LAB done in input file now            NKy = Ny
            N   = Nx  * Ny
            K   = NKx * NKy

            N2  = N  * N
            K2  = K  * K
            KN  = K  * N
            NS  = N  + K
            NS2 = NS * NS

! *****  Begin forming the Galerkin system ***********************

! .....  Evaluation of inner products of polynomials ............      

        C a l l     PolLin (Nx, Ny, NKx, NKy) 
        C a l l     PolNel (Nx, Ny, NKx, NKy)

! ....  Introducing of the boundary conditions ..................

        C a l l     FunNS  (Nx,  Ny)
        C a l l     FunHT  (NKx, NKy)

! .... Evaluation of inner products of basis functions .........               

!                            Navier-Stokes equation

        C a l l     NsxyL  (Nx, Ny)
        C a l l     NsxyNL (Nx, Ny)
        C a l l     NsxyAR (Nx, Ny, NKx, NKy)

!                            Heat transfer equation

        C a l l     HtxyL  (NKx,NKy)                                    
        C a l l     HtxyNL (Nx, Ny, NKx, NKy)                           

!  ****** Complete the Galerkin process ***********************         

!                            Navier-Stokes equation

        C a l l   SysNSL (N, Nx, Ny)

!                            Heat transfer equation

        C a l l   SysHTL (K, NKx, NKy, N, Nx, Ny)    

!                            Buoyancy force

        C a l l   Farhim (N, Nx, Ny, K, NKx, NKy)

!                            Initial distribution of the temperature        

        C a l l   TempBT (N, Nx, Ny, K, NKx, NKy)  


! *********** Evaluation of the dynamical system ********

	C a l l   Dynam (Nx, Ny, NKx, NKy)

      Stop

101      Format (I5)
200      Format (//,'  GalGel behnchmark submitted to SPEC2000 ',
     *              'search programm',/,80('_'),//)
201      Format (' This is Ra-Fa case of the GAMM benchmark on the',
     *           ' oscillatory instability',/,' of convection in a',
     *           ' laterally heated rectangular cavity.',//,
     *           ' Solution by global spectral Galerkin method.',/,
     *           80('_'),//,' Parameters:',/)
202      Format (' Aspect ratio (length/height) =', G15.8,/,
     *           ' Prandtl number               =', G15.8,//,
     *           ' Number of basis functions in x-direction =', I4,/,
     *           ' Number of basis functions in z-direction =', I4,
     *           /, 80('_'),//)
      End

C +++++++++++++++++++++++++++++++++++++++++++++++++++++++

      Subroutine   Dynam (Nx, Ny, NKx, NKy)

        Use   razmer
        Use   Numbers
        Use   Parameters
        Use   ENS
        Use   Fmag
        Use   EHT
!LAB
        Use   DynamParamM
!LAB
        Implicit Real*8 (A-H,O-Z)                                      


C  ***** Working area ******************                 

       Dimension   X(M11)

C ______________________________________________________

!          Initial parameters 

!LAB       GrStrt = 10.D0
!LAB       GrEnd  = 1.D+5
!LAB       GrMul  = 10.D0
       GrStrt = DynamParam%GrStrt
       GrEnd  = DynamParam%GrEnd
       GrMul  = DynamParam%GrMul
           

                   Write (*,101)                                   

              NS  = N + K                                          

!    Laplacian of the temperature is divided by the Prandtl number                        

           HtLbt(1:K)     = HtLbt(1:K)     / Pr
           HtLap(1:K,1:N) = HtLap(1:K,1:N) / Pr

!    Zero initial guess              

           X(1:NS) = 0.D0
           
! ****  Calculate initial steady state *********************      

                 GrOld = 1.D0
                 Gr    = GrStrt 

        AdjustGr: Do While (Gr .LE. GrEnd)

		Write (*,104)  Gr
		pgr = Gr / GrOld

!         Buoyancy terms are multiplied by the Grashof number

                  EnsFbt(1:N)     = EnsFbt(1:N)     * pgr
                  EnsArh(1:N,1:K) = EnsArh(1:N,1:K) * pgr

!         Call Newton method 

              C a l l   EvNewt (NS, Nx, Ny, NKx, NKy, X, ier)
     &                          
                 If  (ier .NE. 0) Stop

!         Change the Grashof number 

              GrOld = Gr
              Gr    = Gr * GrMul
	
        End Do AdjustGr
        
        Write (*,102)

! ********* Do stability analisys **********************

        Write (*,103)

        C a l l    EvCykl (NS, Nx, Ny, NKx, NKy, X, ier)  

      Stop
101     Format (/,' Step 1: Calculate steady states sequentially for',/,
     *            '         Grashof numbers = 1.E1, 1.E2, 1.E3', 
     *                      ' 1.E4, and 1.E5,',/, 
     *            '         using zero initial guess',/)
102     Format (/,' Step 1 is successfully completed',/, 80('_'),//)
103     Format (/,' Step 2: Perform linear stability analysis'/, 
     *            '         and calculate critical Grashof number ',
     *                     'and critical frequency.',/,
     *   '         Steady state at Gr=1.E5 is used as initial guess',//)
104     Format (//,'        Current Grashof number = ',G15.8,/)
      End
