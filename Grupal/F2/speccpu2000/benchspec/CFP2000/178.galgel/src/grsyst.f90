C %%%%%%%%%%%%%%  GRSYST  %%%%%%%%%%%%%%%%%

      Subroutine  Diffun (Y, NS, Grc, DX, A, ind)

       Use   razmer
       Use   Parameters
       Use   Numbers
       Use   Sizes
       Use   ENS
       Use   EHT
       Use   Ttime

        Implicit Real*8 (A-H,O-Z)                                        

         Dimension Y(NS), DX(NS), A(NS,NS)

C ================================================================

C  ***** Correction of the critical parameter *******************

      If (Grc .NE. Gr) then
                            sp = Grc / Gr                                                   
                            Gr = Grc                                                      

                 EnsArh(1:N,1:K) = sp * EnsArh(1:N,1:K)
                 EnsFbt(1:N)     = sp * EnsFbt(1:N)
      End If

C  **** What to compute ? ***************************************

          If (ind. NE. 1) then

C  +++++ Evaluate heat transfer equation +++++++++++++++++++++++++

              DX(1:K) = HtLbt(1:K) + 
     +                      MATMUL( HtLap(1:K,1:K), Y( 1 : K ) ) -
     -                      MATMUL( HtVbt(1:K,1:N), Y(K+1:K+N) )

              If (Pr .NE. 0.D0) C a l l   SysHTN (HtTim, Y, DX, A,
     *                               NS, K, NKX, NKY, N, NX, NY, 0)    

C  +++++ Evaluate Navier-Stokes equation +++++++++++++++++++++++++++   

              DX(K+1:K+N) = EnsFbt(1:N) +
     +                       MATMUL( EnsLap(1:N,1:N), Y(K+1:K+N) ) +
     +                       MATMUL( EnsArh(1:N,1:K), Y( 1 : K ) ) 

             C a l l   SysNSN (EnsTim, Y, DX, A, NS, N, K, NX, NY, 0)

C .................................................................

             Return

          Else

C  +++++++  Calculate the Jacobian +++++++++++++++++++++++++++++++++++

              A(  1:K,    1:K)   =   HtLap(1:K,1:K)
              A(K+1:K+N,K+1:K+N) =  EnsLap(1:N,1:N)
              A(K+1:K+N,  1:K  ) =  EnsArh(1:N,1:K)
              A(  1:K  ,K+1:K+N) = - HtVbt(1:K,1:N)

              C a l l   SysNSN (EnsTim, Y, DX, A, NS, N, K, NX, NY, 1)
 
              If (Pr .NE. 0.D0) C a l l   SysHTN (HtTim, Y, DX, A,
     *                                 NS, K, NKX, NKY, N, NX, NY, 1)    

          End If

         Return
        End

