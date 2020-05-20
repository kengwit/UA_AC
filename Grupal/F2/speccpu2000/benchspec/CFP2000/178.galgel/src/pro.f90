C %%%%%%%%%%%%%%%%  PRO  %%%%%%%%%%%%%%%%%%%
c
c  Inner products of Chebyshev polynomials

C    ******* Calculation of (T,T) **********

       Real*8 Function TT (NN,MM)
        Implicit Real*8 (a-h, o-z)

         N = NN
         M = MM

         TT = 0.D0

         If (N .LT. 0) N = -N
         If (M .LT. 0) M = -M

         K = (N+M)/2
         If (K*2 .NE. N+M) Return

         TT = ( 1.D0/(N+M+1)-1.D0/(N+M-1)+1.D0/(N-M+1)-1.D0/(N-M-1) )/4.D0

        Return
       End

C    ******* Calculation of (T,U) **********

       Real*8 Function TBU (NN,MM)
        Implicit Real*8 (a-h, o-z)

         N = NN
         M = MM

         TBU = 0.D0

         IM = 1
         If (M .EQ. -1) Return
         If (N .LT.  0)  N = -N
         If (M .LT. -1) IM = -1
         If (M .LT. -1)  M = -(M+2)

         K = (N+M)/2
         If (K*2 .NE. M+N) Return

         TBU = IM * DBLE(M+1) / DBLE( (M+1)**2-N*N )

        Return
       End


C    ******* Calculation of (U,U) **********

       Real*8 Function UU (NN,MM)
        Implicit Real*8 (a-h, o-z)

         N = NN
         M = MM

         UU = 0.D0

         IN = 1
         IM = 1
         If (N .EQ. -1) Return
         If (M .EQ. -1) Return
         If (N .LT. -1) IN = -1
         If (M .LT. -1) IM = -1
         If (N .LT. -1)  N = -(N+2)
         If (M .LT. -1)  M = -(M+2)

         K=N/2+1

         S = 0.D0

         Do j=0,K-1
c            J = II-1
            L = N-2*J
            P = 2.D0
            If (L .EQ. 0) P = 1.D0

            S = S + TBU(L,M) * P
         End Do

         UU = S * IN * IM

        Return
       End

C    ******* Calculation of (U'',U) ********

       Real*8 Function U2U (NN,MM)
        Implicit Real*8 (a-h, o-z)

         N = NN
         M = MM

         U2U = 0.D0

         IN = 1
         IM = 1
         If (N .EQ. -1) Return
         If (M .EQ. -1) Return
         If (N .LT. -1) IN = -1
         If (M .LT. -1) IM = -1
         If (N .LT. -1)  N = -(N+2)
         If (M .LT. -1)  M = -(M+2)
         If (N .LT.  2) Return

         K = (N-2)/2+1

         S = 0.D0

         Do j=0,K-1
            L = N-2*J-2
            P = 16.D0
            If (L .EQ. 0) P = 8.D0

            S = S + P * (J+1) * (J+2) * (N-J) * (N-J-1) * TBU(L,M)
         End Do

         U2U = S * IN * IM

        Return
       End

C    ******* Calculation of (T'',T) ********

       Real*8 Function T2T (NN,MM)
        Implicit Real*8 (a-h, o-z)

         N = NN
         M = MM

         T2T = 0.D0

         If (N .LT. 0) N=-N
         If (M .LT. 0) M=-M
         If (N .LT. 2) Return

         K = (N-2)/2+1

         S = 0.D0

         Do j=0,K-1
            L = N-2*J-2
            P = 16.D0
            If (L .EQ. 0) P = 8.D0

            S = S + P * (J+1) * (N-J-1) * TT(L,M)
         End Do

       T2T = N * S

       Return
       End

C    ******* Calculation of (T*U,T) ********

       Real*8 Function TUT (NN,MM,LL)
        Implicit Real*8 (a-h, o-z)

         N = NN
         M = MM
         L = LL

         TUT = 0.D0

         IM = 1
         If (M .EQ. -1) Return
         If (M .LT. -1) IM = -1
         If (M .LT. -1)  M = -(M+2)
         K = Abs(N-L)

         TUT = 0.5D0 * IM * (TBU(N+L,M) + TBU(K,M))

        Return
       End

C    ******* Calculation of (U*U,U) ********

       Real*8 Function UUU (NN,MM,LL)
        Implicit Real*8 (a-h, o-z)

         N = NN
         M = MM
         L = LL

         UUU = 0.D0

         IN = 1
         IM = 1
         IL = 1
         If (N .EQ. -1) Return
         If (M .EQ. -1) Return
         If (L .EQ. -1) Return
         If (N .LT. -1) IN = -1
         If (M .LT. -1) IM = -1
         If (L .LT. -1) IL = -1
         If (N .LT. -1) N = -(N+2)
         If (M .LT. -1) M = -(M+2)
         If (L .LT. -1) L = -(L+2)

         K  = N+M+L
         K2 = K/2
         If (K2*2 .NE. K) Return
         L0 = L/2+1
         N0 = MIN (N,M)+1
         NM = IABS(N-M)

         S = 0.D0

         Do K=0,N0-1

          Do J=0,L0-1
             J1 = L-2*J
             P  = 1.D0
             If (J1 .EQ. 0) P = 0.5D0

             S = S + P * TBU(J1,NM+2*K)
          End Do
         End Do

         UUU = 2.D0*S*IN*IM*IL

        Return
       End

C    ******* Calculation of (U'*T,U) *******

       Real*8 Function U1TU (NN,MM,LL)
        Implicit Real*8 (a-h, o-z)

         N = NN
         M = MM
         L = LL

         U1TU = 0.D0

         IN = 1
         IL = 1
         If (L .EQ. -1) Return
         If (N .EQ. -1) Return
         If (N .LT. -1) IN = -1
         If (L .LT. -1) IL = -1
         If (N .LT. -1) N = -(N+2)
         If (L .LT. -1) L = -(L+2)
         If (N .EQ. 0)  Return

         K  = N+M+L
         K2 = K/2
         If (K2*2 .EQ. K) Return
         N0 = (N-1)/2+1

         S  = 0.D0

         Do JJ=1,N0
            J = JJ-1
            K = N-2*J-1
            P = 1.D0
            If (K .EQ. 0) P=0.5D0

            S = S +  P * (J+1)*(N-J) * (TBU(K+M,L)+TBU(IABS(K-M),L) )
         End Do

         U1TU = 4.D0 * S * IN * IL

        Return
       End

C    ******* Calculation of (T'*T,T) *******

       Real*8 Function T1TT (NN,MM,LL)
        Implicit Real*8 (a-h, o-z)

         N = NN
         M = MM
         L = LL
         If (N .LT. 0) N = -N
         If (M .LT. 0) M =- M
         If (L .LT. 0) L = -L

         T1TT = 2.D0 * N * TUT(M,N-1,L)

        Return
       End

C   ******** Calculation of  (U',T)  *********

      Real*8 Function U1T (NN,MM)
       Implicit Real*8 (a-h, o-z)

        N = NN
        M = MM

        U1T = 0.D0

        IN = 1
        If (N .LT. -1) IN = -1
        If (N .LT. -1) N = -(N+2)
        If (M .LT. 0)  M = -M
        If (N .LE. 0)  Return

        U1T = IN * T2T(N+1,M) / (2.D0*(N+1))

       Return
      End

C  ******  Calculation of  (TT,T) *******

      Real*8 Function TTT (NN,MM,LL)
       Implicit Real*8 (a-h, o-z)

        N = NN
        M = MM
        L = LL
        If (N .LT. 0) N = -N
        If (M .LT. 0) M = -M
        If (L .LT. 0) L = -L

        TTT = 0.5D0 * ( TT(N,M+L) + TT(N,M-L) )

       Return
      End

