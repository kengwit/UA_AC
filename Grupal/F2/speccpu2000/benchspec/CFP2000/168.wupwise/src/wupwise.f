C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012



      PROGRAM WUPWISE
C     ===============

C --------------------------------------------------------------------
C
C     SIMPLE BICGSTAB SOLVER FOR ME(U)*X = PHI
C
C --------------------------------------------------------------------

      IMPLICIT NONE

C     %---------%
C     | SCALARS |
C     %---------%

      INTEGER           NITER

      INTEGER           SOURCE

      INTEGER           IX,IY,IZ,IT,IC,ID

      INTEGER           ITER

      REAL*8            EPS,TRUEPS

      REAL*8            R0NRM2,RNRM2

      COMPLEX*16        KAPPA

      COMPLEX*16        ALPH,ALPHA,BETA,DELTA,RHO,OMEGA,ST,TT

C     %------------%
C     | PARAMETERS |
C     %------------%

      INTEGER           N1,N2,N3,N4

      PARAMETER        (N1=16,N2=20,N3=20,N4=20)

      INTEGER           SIZE

      PARAMETER        (SIZE=6*N1*N2*N3*N4)

      COMPLEX*16        ZERO,ONE

      PARAMETER        (ZERO=(0.0D+0,0.0D+0),ONE=(1.0D+0,0.0D+0))

C     %--------%
C     | ARRAYS |
C     %--------%

      INTEGER           SEED(4)

      COMPLEX*16          X(12,N1/2,N2,N3,N4)
      COMPLEX*16        PHI(12,N1/2,N2,N3,N4)
      COMPLEX*16          R(12,N1/2,N2,N3,N4)
      COMPLEX*16         RD(12,N1/2,N2,N3,N4)
      COMPLEX*16          P(12,N1/2,N2,N3,N4)
      COMPLEX*16         UD(12,N1/2,N2,N3,N4)
      COMPLEX*16          S(12,N1/2,N2,N3,N4)
      COMPLEX*16          T(12,N1/2,N2,N3,N4)
      COMPLEX*16        AUX(12,N1/2,N2,N3,N4)

      COMPLEX*16        U(3,3,4,N1,N2,N3,N4)

C     %----------------------%
C     | EXTERNAL SUBROUTINES |
C     %----------------------%

      EXTERNAL INIT,UINITH,PHINIT

      EXTERNAL MATMUL

      EXTERNAL ZCOPY,ZAXPY,ZSCAL

C     %--------------------%
C     | EXTERNAL FUNCTIONS |
C     %--------------------%

      EXTERNAL        DZNRM2
      REAL*8          DZNRM2

      EXTERNAL         ZDOTC
      COMPLEX*16       ZDOTC

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%

C     %---------------------------------------%
C     | INITIALIZATION OF PROJECT ENVIRONMENT |
C     %---------------------------------------%

      CALL INIT(SOURCE,IX,IY,IZ,IT,IC,ID,SEED,NITER,KAPPA)

      CALL UINITH(N1,N2,N3,N4,SEED,U)

      CALL PHINIT(N1/2,N2,N3,N4,SOURCE,IX,IY,IZ,IT,IC,ID,SEED,PHI)

c     %---------------%
c     | INITIAL GUESS |
c     %---------------%

      CALL ZCOPY(SIZE,ZERO,0,X,1)

c     %------------------%
c     | INITIAL RESIDUAL |
c     %------------------%

      CALL ZCOPY(SIZE,PHI,1,R,1)
      CALL MATMUL(U,N1,N2,N3,N4,KAPPA,X,AUX,P)
      CALL ZAXPY(SIZE,-ONE,P,1,R,1)

      R0NRM2 = DZNRM2(SIZE,R,1)

c     %-------%
c     | P = R |
c     %-------%

      CALL ZCOPY(SIZE,R,1,P,1)

c     %-----------%
c     | RD RANDOM |
c     %-----------%

      CALL RNDPHI(RD,2*SIZE,SEED)

c     %----------------%
c     | ALPHA = <R,RD> |
c     %----------------%

      ALPHA = ZDOTC(SIZE,RD,1,R,1)

C     %-------------------------------%
C     | LOOP OVER BICGSTAB ITERATIONS |
C     %-------------------------------%

      DO 100 ITER = 1,NITER

c         %----------%
c         | UD = A P |
c         %----------%

          CALL MATMUL(U,N1,N2,N3,N4,KAPPA,P,AUX,UD)

c         %----------------%
c         | BETA = <UD,RS> |
c         %----------------%

          BETA  = ZDOTC(SIZE,RD,1,UD,1)

c         %--------------------%
c         | DELTA = ALPHA/BETA |
c         %--------------------%

          DELTA = ALPHA/BETA

c         %------------------%
c         | S = R - DELTA UD |
c         %------------------%

          CALL ZCOPY(SIZE,R,1,S,1)
          CALL ZAXPY(SIZE,-DELTA,UD,1,S,1)

c         %---------%
c         | T = A S |
c         %---------%

          CALL MATMUL(U,N1,N2,N3,N4,KAPPA,S,AUX,T)

c         %---------------------%
c         | OMEGA = <S,T>/<T,T> |
c         %---------------------%

          ST = ZDOTC(SIZE,T,1,S,1)
          TT = ZDOTC(SIZE,T,1,T,1)

          OMEGA = ST/TT

c         %-----------------%
c         | R = S - OMEGA T |
c         %-----------------%

          CALL ZCOPY(SIZE,S,1,R,1)
          CALL ZAXPY(SIZE,-OMEGA,T,1,R,1)

c         %---------------------------%
c         | X = X + DELTA P + OMEGA S |
c         %---------------------------%

          CALL ZAXPY(SIZE,DELTA,P,1,X,1)
          CALL ZAXPY(SIZE,OMEGA,S,1,X,1)

c         %---------------%
c         | ALPH = <R,RD> |
c         %---------------%

          ALPH  = ZDOTC(SIZE,RD,1,R,1)

c         %--------------------------------%
c         | RHO = ALPH/ALPHA * DELTA/OMEGA |
c         %--------------------------------%

          RHO   = ALPH/ALPHA * DELTA/OMEGA

c         %--------------%
c         | ALPHA = ALPH |
c         %--------------%

          ALPHA = ALPH

c         %------------------------------%
c         | P = R + RHO ( P - OMEGA UD ) |
c         %------------------------------%

          CALL ZAXPY(SIZE,-OMEGA,UD,1,P,1)
          CALL ZSCAL(SIZE,RHO,P,1)
          CALL ZAXPY(SIZE,ONE,R,1,P,1)

C        %------------------------------%
C        | CHECK MODULUS OF REST VECTOR |
C        %------------------------------%

         RNRM2  = DZNRM2(SIZE,R,1)

         EPS    = RNRM2 / R0NRM2

CKJS     PRINT *, ITER, EPS
         WRITE(*,992) ITER, EPS

992      FORMAT(I8,D15.8)

C     %-----------------------------------%
C     | END LOOP OVER BICGSTAB ITERATIONS |
C     %-----------------------------------%

 100  CONTINUE

c     %----------------%
c     | FINAL RESIDUAL |
c     %----------------%

      CALL ZCOPY(SIZE,PHI,1,R,1)
      CALL MATMUL(U,N1,N2,N3,N4,KAPPA,X,AUX,P)
      CALL ZAXPY(SIZE,-ONE,P,1,R,1)

      RNRM2 = DZNRM2(SIZE,R,1)

c     %-------------------------%
c     | TRUEPS = RNRM2 / R0NRM2 |
c     %-------------------------%

      TRUEPS = RNRM2 / R0NRM2

c     %---------------%
c     | PRINT RESULTS |
c     %---------------%

      PRINT *,' '
      PRINT *,'BICGSTAB RESULTS:'
      PRINT *,'---------------------'
      PRINT *,'ITER         : ',ITER-1
      PRINT *,'EPSILON      : ',EPS

C     PRINT *,'TRUE EPSILON : ',TRUEPS
C     For the SPEC CPU2000 version, we write TRUEPS to a different file
C     because the accuracy (relative tolerance) requirements for this value
C     are different from those for the other values.

      OPEN (8,FILE='te.out',STATUS='UNKNOWN')
      WRITE (UNIT=8,FMT=10) TRUEPS
10    FORMAT ('True epsilon: ', D15.8)

C     %----------------%
C     | END OF WUPWISE |
C     %----------------%

      STOP
      END

