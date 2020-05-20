      SUBROUTINE DINV(N,A,IDIN,R,IFAIL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER R(N)
      REAL*8 T1,T2,T3
      REAL*8 A(IDIN,N),DET,TEMP,S,
     $                 C11,C12,C13,C21,C22,C23,C31,C32,C33
         CHARACTER*6 HNAME
      save
      DATA HNAME/'DINV  '/,KPRNT/0/
C
C ******************************************************************
C
C     REPLACES A BY ITS INVERSE.
C
C     (PARAMETERS AS FOR DEQINV.)
C
C     CALLS ... DFACT, DFINV, F010PR, ABEND.
C
C ******************************************************************
C
C  TEST FOR PARAMETER ERRORS.
C
      IF((N.LT.1).OR.(N.GT.IDIN)) GO TO 7
C
C  TEST FOR N.LE.3.
C
      IF(N.GT.3) GO TO 6
      IFAIL=0
      IF(N.LT.3) GO TO 4
C
C  N=3 CASE.
C
C     COMPUTE COFACTORS.
      C11=A(2,2)*A(3,3)-A(2,3)*A(3,2)
      C12=A(2,3)*A(3,1)-A(2,1)*A(3,3)
      C13=A(2,1)*A(3,2)-A(2,2)*A(3,1)
      C21=A(3,2)*A(1,3)-A(3,3)*A(1,2)
      C22=A(3,3)*A(1,1)-A(3,1)*A(1,3)
      C23=A(3,1)*A(1,2)-A(3,2)*A(1,1)
      C31=A(1,2)*A(2,3)-A(1,3)*A(2,2)
      C32=A(1,3)*A(2,1)-A(1,1)*A(2,3)
      C33=A(1,1)*A(2,2)-A(1,2)*A(2,1)
      T1=ABS(A(1,1))
      T2=ABS(A(2,1))
      T3=ABS(A(3,1))
C
C     (SET TEMP=PIVOT AND DET=PIVOT*DET.)
      IF(T1.GE.T2) GO TO 1
         IF(T3.GE.T2) GO TO 2
C        (PIVOT IS A21)
            TEMP=A(2,1)
            DET=C13*C32-C12*C33
            GO TO 3
    1 IF(T3.GE.T1) GO TO 2
C     (PIVOT IS A11)
         TEMP=A(1,1)
         DET=C22*C33-C23*C32
         GO TO 3
C     (PIVOT IS A31)
    2    TEMP=A(3,1)
         DET=C23*C12-C22*C13
C
C     SET ELEMENTS OF INVERSE IN A.
    3 IF(DET.EQ.0D0) GO TO 8
      S=TEMP/DET
      A(1,1)=S*C11
      A(1,2)=S*C21
      A(1,3)=S*C31
      A(2,1)=S*C12
      A(2,2)=S*C22
      A(2,3)=S*C32
      A(3,1)=S*C13
      A(3,2)=S*C23
      A(3,3)=S*C33
      RETURN
C
    4 IF(N.LT.2) GO TO 5
C
C  N=2 CASE BY CRAMERS RULE.
C
      DET=A(1,1)*A(2,2)-A(1,2)*A(2,1)
      IF(DET.EQ.0D0) GO TO 8
      S=1D0/DET
      C11   =S*A(2,2)
      A(1,2)=-S*A(1,2)
      A(2,1)=-S*A(2,1)
      A(2,2)=S*A(1,1)
      A(1,1)=C11
      RETURN
C
C  N=1 CASE.
C
    5 IF(A(1,1).EQ.0D0) GO TO 8
      A(1,1)=1D0/A(1,1)
      RETURN
C
C  N.GT.3 CASES.  FACTORIZE MATRIX AND INVERT.
C
    6 CALL DFACT(N,A,IDIN,R,IFAIL,DET,JFAIL)
      IF(IFAIL.NE.0) RETURN
      CALL DFINV(N,A,IDIN,R)
      RETURN
C
C  ERROR EXITS.
C
    7 IFAIL=+1
      CALL F010PR(HNAME,N,IDIN,K,KPRNT)
      RETURN
C
    8 IFAIL=-1
      RETURN
C
      END
          SUBROUTINE          DFACT(N,A,IDIN,IR,IFAIL,DET,JFAIL)
          IMPLICIT REAL*8 (A-H,O-Z)
          INTEGER             IR(9),    IPAIRF
      REAL*8    A(IDIN,9),DET,      ZERO,     ONE,X,Y,TF
          REAL*8              G1,       G2
      REAL*8                PIVOTF,   P,        Q,        SIZEF,  T
          REAL*8    S11, S12, DOTF
          CHARACTER*6          HNAME
          save
          IPAIRF(J,K)  =  J*2**12 + K
          PIVOTF(X)    =  ABS(X)
          SIZEF(X)     =  ABS(X)
          DOTF(X,Y,S11)  =  X * Y + S11
          DATA      G1, G2              /  1.E-37,  1.E37  /
          DATA      HNAME               /'DFACT '/
          DATA      ZERO, ONE           /  0.D0, 1.D0  /
          DATA      NORMAL, IMPOSS      /  0, -1  /
          DATA      JRANGE, JOVER, JUNDER  /  0, +1, -1  /
          IF(IDIN .GE. N  .AND.  N .GT. 0)  GOTO 110
             CALL TMPRNT(HNAME,N,IDIN,0)
             RETURN
 110      IFAIL  =  NORMAL
          JFAIL  =  JRANGE
          NXCH   =  0
          DET    =  ONE
          DO 144    J  =  1, N
 120         K  =  J
             P  =  PIVOTF(A(J,J))
             IF(J .EQ. N)  GOTO 122
             JP1  =  J+1
             DO 121    I  =  JP1, N
                Q  =  PIVOTF(A(I,J))
                IF(Q .LE. P)  GOTO 121
                   K  =  I
                   P  =  Q
 121            CONTINUE
             IF(K .NE. J)  GOTO 123
 122         IF(P .GT. 0.)  GOTO 130
                DET    =  ZERO
                IFAIL  =  IMPOSS
                JFAIL  =  JRANGE
                RETURN
 123         DO 124    L  =  1, N
                TF      =  A(J,L)
                A(J,L)  =  A(K,L)
                A(K,L)  =  TF
 124            CONTINUE
             NXCH      =  NXCH + 1
             IR(NXCH)  =  IPAIRF(J,K)
 130         DET     =  DET * A(J,J)
             A(J,J)  =  ONE / A(J,J)
             T  =  SIZEF(DET)
             IF(T .LT. G1)  GOTO 132
             IF(T .GT. G2)  GOTO 133
 131         IF(J .EQ. N)  GOTO 144
             GOTO 140
 132         DET    =  ZERO
             IF(JFAIL .EQ. JRANGE)  JFAIL  =  JUNDER
             GOTO 131
 133         DET    =  ONE
             IF(JFAIL .EQ. JRANGE)  JFAIL  =  JOVER
             GOTO 131
 140         JM1  =  J-1
             JP1  =  J+1
             DO 143   K  =  JP1, N
                S11  =  -A(J,K)
                S12  =  -A(K,J+1)
                IF(J .EQ. 1)  GOTO 142
                DO 141  I  =  1, JM1
                   S11  =  DOTF(A(I,K),A(J,I),S11)
                   S12  =  DOTF(A(I,J+1),A(K,I),S12)
 141               CONTINUE
 142            A(J,K)    =  -S11 * A(J,J)
                A(K,J+1)  =  -DOTF(A(J,J+1),A(K,J),S12)
 143            CONTINUE
 144         CONTINUE
 150      IF(MOD(NXCH,2) .NE. 0)  DET  =  -DET
          IF(JFAIL .NE. JRANGE)   DET  =  ZERO
          IR(N)  =  NXCH
          RETURN
          END
          SUBROUTINE          DFINV(N,A,IDIN,IR)
          IMPLICIT REAL*8 (A-H,O-Z)
          INTEGER             IR(9)
          REAL*8    A(IDIN,9),ZERO,     X, Y, TI
          REAL*8    S31, S32, S33, S34, DOTF
          CHARACTER*6    HNAME
          save
          DATA      HNAME               /'DFINV '/
          DATA      ZERO      /  0.D0  /
          DOTF(X,Y,S31)  =  X*Y + S31
          IF(IDIN .GE. N  .AND.  N .GT. 0)  GOTO 310
             CALL TMPRNT(HNAME,N,IDIN,0)
             RETURN
 310      IF(N .EQ. 1)  RETURN
          A(2,1)  =  -A(2,2) * DOTF(A(1,1),A(2,1),ZERO)
          A(1,2)  =  -A(1,2)
          IF(N .EQ. 2)  GOTO 330
          DO 314    I  =  3, N
             IM2  =  I-2
             DO 312 J  =  1, IM2
                S31  =  ZERO
                S32  =  A(J,I)
                DO 311  K  =  J, IM2
                   S31  =  DOTF(A(K,J),A(I,K),S31)
                   S32  =  DOTF(A(J,K+1),A(K+1,I),S32)
 311               CONTINUE
                A(I,J)  =  -A(I,I) * DOTF(A(I-1,J),A(I,I-1),S31)
                A(J,I)  =  -S32
 312            CONTINUE
             A(I,I-1)  =  -A(I,I) * DOTF(A(I-1,I-1),A(I,I-1),ZERO)
             A(I-1,I)  =  -A(I-1,I)
 314         CONTINUE
 330      NM1  =  N-1
          DO 335   I  =  1, NM1
             NMI  =  N-I
             DO 332   J  =  1, I
                S33  =  A(I,J)
                DO 331   K  =  1, NMI
                   S33  =  DOTF(A(I+K,J),A(I,I+K),S33)
 331               CONTINUE
                A(I,J)  =  S33
 332            CONTINUE
             DO 334   J  =  1, NMI
                S34  =  ZERO
                DO 333   K  =  J, NMI
                   S34  =  DOTF(A(I+K,I+J),A(I,I+K),S34)
 333               CONTINUE
                A(I,I+J)  =  S34
 334            CONTINUE
 335         CONTINUE
          NXCH  =  IR(N)
          IF(NXCH .EQ. 0)  RETURN
            DO 342 M  =  1, NXCH
             K   =  NXCH - M+1
             IJ  =  IR(K)
             I   =  IJ / 4096
             J   =  MOD(IJ,4096)
             DO 341  K  =  1, N
                TI      =  A(K,I)
                A(K,I)  =  A(K,J)
                A(K,J)  =  TI
 341            CONTINUE
 342         CONTINUE
          RETURN
          END
      SUBROUTINE F010PR(NAME,N,IDIN,K,KPRNT)
      IMPLICIT REAL*8 (A-H,O-Z)
          CHARACTER*(*) NAME
      LOGICAL MFLAG,RFLAG
      save
C
C     ******************************************************************
C
C     PRINT ROUTINE FOR PARAMETER ERRORS IN MATRIX SUBROUTINES $EQINV,
C     $EQN, $INV (WHERE $ IS A LETTER SPECIFYING THE ARITHMETIC TYPE).
C
C     NAME         ("CHARACTER"*6) NAME OF THE CALLING ROUTINE.
C
C     N,IDIN,K     PARAMETERS OF THE CALLING ROUTINE (WITH K=0 IF K IS
C                  NOT TO BE PRINTED).
C
C     KPRNT        PRINT FLAG FOR K (K IS NOT PRINTED IF KPRNT=0).
C
C     ******************************************************************
C
C  START.
      CALL KERMTR('F010.1',LGFILE,MFLAG,RFLAG)
      IF(MFLAG) THEN
         IF(LGFILE.EQ.0)  THEN
            IF(KPRNT.EQ.0) WRITE(*,2000) NAME,N,IDIN
            IF(KPRNT.NE.0) WRITE(*,2001) NAME,N,IDIN,K
         ELSE
            IF(KPRNT.EQ.0) WRITE(LGFILE,2000) NAME,N,IDIN
            IF(KPRNT.NE.0) WRITE(LGFILE,2001) NAME,N,IDIN,K
         ENDIF
      ENDIF
      IF(.NOT. RFLAG) CALL ABEND
      RETURN
C
 2000 FORMAT( 7X, 'SUBROUTINE ', A6, ' ... PARAMETER',
     *        ' ERROR (N.LT.1 OR N.GT.IDIN).',
     *        6X, 'N =', I4, 6X, 'IDIN =', I4, '.' )
 2001 FORMAT( 7X, 'SUBROUTINE ', A6, '  ... PARAMETER',
     *        ' ERROR (N.LT.1 OR N.GT.IDIN OR K.LT.1).',
     *        6X, 'N =', I4, 6X, 'IDIN =', I4, 6X, 'K =', I4, '.' )
      END
          SUBROUTINE          TMPRNT(HNAME,N,IDIN,K)
          IMPLICIT REAL*8 (A-H,O-Z)
          CHARACTER*(*)       HNAME
          LOGICAL             MFLAG,    RFLAG
          save
             CALL KERMTR('F11/12',LGFILE,MFLAG,RFLAG)
          IF(MFLAG) THEN
             IF(LGFILE .EQ. 0) THEN
                   WRITE(*,1002) HNAME, N, IDIN, K
             ELSE
                   WRITE(LGFILE,1002) HNAME, N, IDIN, K
             ENDIF
          ENDIF
          IF(.NOT. RFLAG) CALL ABEND
          RETURN
1002      FORMAT(7X, ' PARAMETER ERROR IN SUBROUTINE ', A6,
     +            ' ... (N.LT.1 OR IDIN.LT.N OR K.LT.1).',
     +             5X, 'N =', I4, 5X, 'IDIN =', I4, 5X, 'K =', I4,'.')
          END
          SUBROUTINE KERSET(ERCODE,LGFILE,LIMITM,LIMITR)
          IMPLICIT REAL*8 (A-H,O-Z)
                    PARAMETER(KOUNTE  =  28)
          CHARACTER*6         ERCODE,   CODE(KOUNTE)
          LOGICAL             MFLAG,    RFLAG,    ERFLAG
          INTEGER             KNTM(KOUNTE),       KNTR(KOUNTE)
          save 
          DATA      LOGF      /  0  /
          DATA      CODE(1), KNTM(1), KNTR(1)  / 'C204.1', 100, 100 /
          DATA      CODE(2), KNTM(2), KNTR(2)  / 'C204.2', 100, 100 /
          DATA      CODE(3), KNTM(3), KNTR(3)  / 'C204.3', 100, 100 /
          DATA      CODE(4), KNTM(4), KNTR(4)  / 'C205.1', 100, 100 /
          DATA      CODE(5), KNTM(5), KNTR(5)  / 'C205.2', 100, 100 /
          DATA      CODE(6), KNTM(6), KNTR(6)  / 'C205.3', 100, 100 /
          DATA      CODE(7), KNTM(7), KNTR(7)  / 'C305.1', 100, 100 /
          DATA      CODE(8), KNTM(8), KNTR(8)  / 'C308.1', 100, 100 /
          DATA      CODE(9), KNTM(9), KNTR(9)  / 'C312.1', 100, 100 /
          DATA      CODE(10),KNTM(10),KNTR(10) / 'C313.1', 100, 100 /
          DATA      CODE(11),KNTM(11),KNTR(11) / 'C336.1', 100, 100 /
          DATA      CODE(12),KNTM(12),KNTR(12) / 'C337.1', 100, 100 /
          DATA      CODE(13),KNTM(13),KNTR(13) / 'C341.1', 100, 100 /
          DATA      CODE(14),KNTM(14),KNTR(14) / 'D103.1', 100, 100 /
          DATA      CODE(15),KNTM(15),KNTR(15) / 'D106.1', 100, 100 /
          DATA      CODE(16),KNTM(16),KNTR(16) / 'D209.1', 100, 100 /
          DATA      CODE(17),KNTM(17),KNTR(17) / 'D509.1', 100, 100 /
          DATA      CODE(18),KNTM(18),KNTR(18) / 'E100.1', 100, 100 /
          DATA      CODE(19),KNTM(19),KNTR(19) / 'E104.1', 100, 100 /
          DATA      CODE(20),KNTM(20),KNTR(20) / 'E105.1', 100, 100 /
          DATA      CODE(21),KNTM(21),KNTR(21) / 'E208.1', 100, 100 /
          DATA      CODE(22),KNTM(22),KNTR(22) / 'E208.2', 100, 100 /
          DATA      CODE(23),KNTM(23),KNTR(23) / 'F010.1', 100,   0 /
          DATA      CODE(24),KNTM(24),KNTR(24) / 'F011.1', 100,   0 /
          DATA      CODE(25),KNTM(25),KNTR(25) / 'F012.1', 100,   0 /
          DATA      CODE(26),KNTM(26),KNTR(26) / 'F406.1', 100,   0 /
          DATA      CODE(27),KNTM(27),KNTR(27) / 'G100.1', 100, 100 /
          DATA      CODE(28),KNTM(28),KNTR(28) / 'G100.2', 100, 100 /
          LOGF  =  LGFILE
          ERFLAG  =  .FALSE.
          DO 14     I  =  1, KOUNTE
             IF(CODE(I) .NE. ERCODE)  GOTO 14
             KNTM(I)  =  LIMITM
             KNTR(I)  =  LIMITR
             ERFLAG  =   .TRUE.
  14         CONTINUE
          IF (.NOT.ERFLAG) THEN
             DO 13    I  =   1,  KOUNTE
             KNTM(I)  =  LIMITM
  13         KNTR(I)  =  LIMITR
          ENDIF
          RETURN
          ENTRY KERMTR(ERCODE,LOG,MFLAG,RFLAG)
          LOG  =  LOGF
          DO 20     I  =  1, KOUNTE
             IF(ERCODE .EQ. CODE(I))  GOTO 21
  20         CONTINUE
          WRITE(*,1000)  ERCODE
          CALL ABEND
          RETURN
  21      RFLAG  =  KNTR(I) .GE. 1
          IF(RFLAG  .AND.  (KNTR(I) .LT. 100))  KNTR(I)  =  KNTR(I) - 1
          MFLAG  =  KNTM(I) .GE. 1
          IF(MFLAG  .AND.  (KNTM(I) .LT. 100))  KNTM(I)  =  KNTM(I) - 1
          IF(.NOT. RFLAG)  THEN
             IF(LOGF .LT. 1)  THEN
                WRITE(*,1001)  CODE(I)
             ELSE
                WRITE(LOGF,1001)  CODE(I)
             ENDIF
          ENDIF
          IF(MFLAG .AND. RFLAG)  THEN
             IF(LOGF .LT. 1)  THEN
                WRITE(*,1002)  CODE(I)
             ELSE
                WRITE(LOGF,1002)  CODE(I)
             ENDIF
          ENDIF
          RETURN
1000      FORMAT(' KERNLIB LIBRARY ERROR. ' /
     +           ' ERROR CODE ',A6,' NOT RECOGNIZED BY KERMTR',
     +           ' ERROR MONITOR. RUN ABORTED.')
1001      FORMAT(/' ***** RUN TERMINATED BY CERN LIBRARY ERROR ',
     +           'CONDITION ',A6)
1002      FORMAT(/' ***** CERN LIBRARY ERROR CONDITION ',A6)
          END
