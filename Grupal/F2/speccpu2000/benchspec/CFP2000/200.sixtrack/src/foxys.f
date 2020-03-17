C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPRI6(INA,RESULT,IEN,I56)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *************************************
*
*     THIS SUBROUTINE IS FOR REDUCED STORAGE DA VERSION JULY 91
*     RESULT CONTAINS THE (IEN-1) TH DERIVATIVE TO ENERGY AFTER EXECUTION
*     I56 SAYS WHETHER THE 5TH OR THE 6TH COORDINATE IS THE ENERGY
*     AND MUST HAVE THE VALUE 5 OR 6 ACCORDINGLY
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*
      DIMENSION J(LNV)
      save
*
*FRA
      RESULT=0.
*FRA
      IF(INA.LT.1.OR.INA.GT.NDA) THEN
         PRINT*,'ERROR IN DAPRI6, INA = ',INA
C         X = SQRT(-ONE)
C        PRINT*,X
         STOP
      ENDIF
*
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)
*
*
      IOUT = 0
*
      IF(NOMAX.EQ.1) THEN
         DO 90 I=1,ILLA
C90      WRITE(IUNIT,'(6X,2X,G20.14,I5)') CC(IPOA+I-1),I-1
      IF(IEN.EQ.1) THEN
      IF(I-1.NE.0) GOTO 90
      RESULT = CC(IPOA+I-1)
      RETURN
      ENDIF
      IF(IEN.EQ.2) THEN
      IF(I-1.NE.I56) GOTO 90
      RESULT = CC(IPOA+I-1)
      RETURN
      ENDIF
 90      CONTINUE
      ELSE
      DO 100 IOA = 0,INOA
      DO 100 II=IPOA,IPOA+ILLA-1
      IF(IEO(IA1(I1(II))+IA2(I2(II))).NE.IOA) GOTO 100
*
      IOUT = IOUT+1
      CALL DANCD(I1(II),I2(II),J)
*
CETIENNE
C     IF(ABS(CC(II)).GT.EPS) THEN
C     WRITE(IUNIT,'(I6,2X,G20.14,I5,4X,18(2I2,1X))')
C    *      IOUT,CC(II),IOA,(J(III),III=1,NVMAX)
*
      IF(I56.EQ.6) THEN
      DO 55 IHP=1,5
      IF(J(IHP).NE.0) GOTO 100
  55  CONTINUE
      IF(J(6).EQ.(IEN-1)) THEN
      RESULT = CC(II)
      RETURN
      ENDIF
      ELSE IF (I56.EQ.5) THEN
      DO 56 IHP=1,4
      IF(J(IHP).NE.0) GOTO 100
  56  CONTINUE
      IF(J(5).EQ.(IEN-1)) THEN
      RESULT = CC(II)
      RETURN
      ENDIF
      ENDIF
*
C     ENDIF
*
 100  CONTINUE
      ENDIF
*
*
      RETURN
      END
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAREA6(INA,ZFELD,I56)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *************************************
*
*     THIS SUBROUTINE IS FOR REDUCED STORAGE DA VERSION JULY 91
*     I56 SAYS WHETHER THE 5TH OR THE 6TH COORDINATE IS THE ENERGY
*     AND MUST HAVE THE VALUE 5 OR 6 ACCORDINGLY
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
      DIMENSION ZFELD(100)
*-----------------------------------------------------------------------------3
*
C     CHARACTER C10*10
      DIMENSION J(LNV)
      save
*
      IF(INA.LT.1.OR.INA.GT.NDA) THEN
         PRINT*,'ERROR IN DAREA6, INA = ',INA
C         X = SQRT(-ONE)
C        PRINT*,X
         STOP
      ENDIF
*
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)
*
      DO 5 I=1,LNV
   5  J(I) = 0
*
      CALL DACLR(1)
*
      IC = 0
*
      IWARNO = 0
      IWARNV = 0
      IWARIN = 0
*
      IIN = 0
*
      IF(NOMAX.EQ.1) THEN
         DO 90 I=1,ILLA
C        READ(IUNIT,'(6X,2X,G20.14,I5)',ERR=95) CC(IPOA+I-1),II
         IF (I-1.EQ.0) THEN
         CC(IPOA+I-1) = ZFELD(1)
         ELSE IF (I-1.EQ.I56) THEN
         CC(IPOA+I-1) = ZFELD(2)
         ENDIF
  90     CONTINUE
         RETURN
      ENDIF
*
      DO 8 IP=1,INVA
      J(IP)=0
   8  CONTINUE
      IO = 0
  10  CONTINUE
      IIN = IIN + 1
C     READ(IUNIT,'(I6,2X,G20.14,I5,4X,18(2I2,1X))')
C    *             II,C,IO,(J(I),I=1,INVA)
*
C     IF(II.EQ.0) GOTO 20
CETIENNE
C     READ(IUNIT,*) C
CETIENNE
C     IF(II.NE.IIN) THEN
C        IF(IWARIN.EQ.0) PRINT*,'WARNING IN DAREA, FILE ',
C    *              'NUMBERING OUT OF ORDER '
C        IWARIN = 1
C     ENDIF
      IO1 = 0
      DO 15 I=1,INVA
  15  IO1 = IO1 + J(I)
*
      IF(IO1.NE.IO) THEN
         IF(IWARNV.EQ.0) PRINT*,'WARNING IN DAREA6, FILE ',
     *              'CONTAINS MORE VARIABLES THAN VECTOR'
         IWARNV = 1
         GOTO 10
      ENDIF
      IF(IO.GT.INOA) THEN
C        IF(IWARNO.EQ.0) PRINT*,'WARNING IN DAREA6, FILE ',
C    *              'CONTAINS HIGHER ORDERS THAN VECTOR '
         IWARNO = 1
         GOTO 10
      ENDIF
*
      IC = IC + 1
      CALL DADCD(J,II1,II2)
      IC = IA1(II1) + IA2(II2)
      CC(IC) = ZFELD(IO+1)
      J(I56)=J(I56)+1
      IO=IO+1
      IF (IO.GT.INOA) GOTO 20
      GOTO 10
*
  20  CONTINUE
*
      CALL DAPAC(INA,1)
*
      RETURN
      END
*ETIENNE & FRANK & FRANK
C ANFANG UNTERPROGRAMM
      SUBROUTINE DALYA(INA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***************************
*
*     THIS SUBROUTINE PRINTS THE DA VECTOR INA TO UNIT IUNIT.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      PARAMETER(NLDI=6)
      COMMON/LYA/SGMA,AML(NLDI,NLDI),AM(NLDI,NLDI),EVAL,DLYA,XLYA(NLDI)
      COMMON/LYA6/AML6(6,6),XLYA6(6)
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
      DIMENSION IFL(NLDI)
*-----------------------------------------------------------------------------3
*
      DIMENSION J(LNV)
      save
      IF(INA.LT.1.OR.INA.GT.NDA) THEN
         PRINT*,'ERROR IN DALYA, INA = ',INA
         STOP
      ENDIF
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)
      IOA = 1
      DO 40 JR=1,NLDI
      IFL(JR)=0
 40   CONTINUE
      DO 100 II=IPOA,IPOA+ILLA-1
      IF(IEO(IA1(I1(II))+IA2(I2(II))).NE.IOA) GOTO 100
      CALL DANCD(I1(II),I2(II),J)
      DO 110 JJ=1,NLDI
      IF(J(JJ).EQ.1) THEN
      XLYA6(JJ)=CC(II)
      IFL(JJ)=1
      ENDIF
 110  CONTINUE
 100  CONTINUE
      DO 45 JR=1,NLDI
      IF(IFL(JR).EQ.0) XLYA6(JR)=ZERO
 45   CONTINUE
      RETURN
      END
C
C ANFANG UNTERPROGRAMM
      SUBROUTINE ETY(NM,N,LOW,IGH,A,ORT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
C
      INTEGER I,J,M,N,II,JJ,LA,MP,NM,IGH,KP1,LOW
      REAL*8 A(NM,N),ORT(IGH)
      REAL*8 F,G,H,SCALE
C     REAL*8 SQRT,ABS,SIGN
      save
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTHES,
C     NUM. MATH. 12, 349-368(1968) BY MARTIN AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 339-358(1971).
C
C     GIVEN A REAL GENERAL MATRIX, THIS SUBROUTINE
C     REDUCES A SUBMATRIX SITUATED IN ROWS AND COLUMNS
C     LOW THROUGH IGH TO UPPER HESSENBERG FORM BY
C     ORTHOGONAL SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
C          SET LOW=1, IGH=N,
C
C        A CONTAINS THE INPUT MATRIX.
C
C     ON OUTPUT-
C
C        A CONTAINS THE HESSENBERG MATRIX.  INFORMATION ABOUT
C          THE ORTHOGONAL TRANSFORMATIONS USED IN THE REDUCTION
C          IS STORED IN THE REMAINING TRIANGLE UNDER THE
C          HESSENBERG MATRIX,
C
C        ORT CONTAINS FURTHER INFORMATION ABOUT THE TRANSFORMATIONS.
C          ONLY ELEMENTS LOW THROUGH IGH ARE USED.
C
C     FORTRAN ROUTINE BY B. S. GARBOW
C     MODIFIED BY FILIPPO NERI.
C
C
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA .LT. KP1) GO TO 200
C
      DO 180 M = KP1, LA
         H = 0.0
         ORT(M) = 0.0
         SCALE = 0.0
C     ********** SCALE COLUMN (ALGOL TOL THEN NOT NEEDED) **********
         DO 90 I = M, IGH
   90    SCALE = SCALE + ABS(A(I,M-1))
C
         IF (SCALE .EQ. 0.0) GO TO 180
         MP = M + IGH
C     ********** FOR I=IGH STEP -1 UNTIL M DO -- **********
         DO 100 II = M, IGH
            I = MP - II
            ORT(I) = A(I,M-1) / SCALE
            H = H + ORT(I) * ORT(I)
  100    CONTINUE
C
         G = -SIGN(SQRT(H),ORT(M))
         H = H - ORT(M) * G
         ORT(M) = ORT(M) - G
C     ********** FORM (I-(U*UT)/H) * A **********
         DO 130 J = M, N
            F = 0.0
C     ********** FOR I=IGH STEP -1 UNTIL M DO -- **********
            DO 110 II = M, IGH
               I = MP - II
               F = F + ORT(I) * A(I,J)
  110       CONTINUE
C
            F = F / H
C
            DO 120 I = M, IGH
  120       A(I,J) = A(I,J) - F * ORT(I)
C
  130    CONTINUE
C     ********** FORM (I-(U*UT)/H)*A*(I-(U*UT)/H) **********
         DO 160 I = 1, IGH
            F = 0.0
C     ********** FOR J=IGH STEP -1 UNTIL M DO -- **********
            DO 140 JJ = M, IGH
               J = MP - JJ
               F = F + ORT(J) * A(I,J)
  140       CONTINUE
C
            F = F / H
C
            DO 150 J = M, IGH
  150       A(I,J) = A(I,J) - F * ORT(J)
C
  160    CONTINUE
C
         ORT(M) = SCALE * ORT(M)
         A(M,M-1) = SCALE * G
  180 CONTINUE
C
  200 RETURN
C     ********** LAST CARD OF ETY **********
      END
C     ******************************************************************
C
C ANFANG UNTERPROGRAMM
      SUBROUTINE ETYT(NM,N,LOW,IGH,A,ORT,Z)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
C
      INTEGER I,J,N,KL,MM,MP,NM,IGH,LOW,MP1
      REAL*8 A(NM,IGH),ORT(IGH),Z(NM,N)
      REAL*8 G
      save
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE ORTRANS,
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
C
C     THIS SUBROUTINE ACCUMULATES THE ORTHOGONAL SIMILARITY
C     TRANSFORMATIONS USED IN THE REDUCTION OF A REAL GENERAL
C     MATRIX TO UPPER HESSENBERG FORM BY  ETY.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
C          SET LOW=1, IGH=N,
C
C        A CONTAINS INFORMATION ABOUT THE ORTHOGONAL TRANS-
C          FORMATIONS USED IN THE REDUCTION BY  ORTHES
C          IN ITS STRICT LOWER TRIANGLE,
C
C          ORT CONTAINS FURTHER INFORMATION ABOUT THE TRANS-
C          FORMATIONS USED IN THE REDUCTION BY  ETY.
C          ONLY ELEMENTS LOW THROUGH IGH ARE USED.
C
C     ON OUTPUT-
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED IN THE
C          REDUCTION BY  ETY,
C
C        ORT HAS BEEN ALTERED.
C
C     FORTRAN ROUTINE BY B. S. GARBOW.
C     MODIFIED BY F. NERI.
C
C
C     ********** INITIALIZE Z TO IDENTITY MATRIX **********
      DO 80 I = 1, N
C
         DO 60 J = 1, N
   60    Z(I,J) = 0.0
C
         Z(I,I) = 1.0
   80 CONTINUE
C
      KL = IGH - LOW - 1
      IF (KL .LT. 1) GO TO 200
C     ********** FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- **********
      DO 140 MM = 1, KL
         MP = IGH - MM
         IF (A(MP,MP-1) .EQ. 0.0) GO TO 140
         MP1 = MP + 1
C
         DO 100 I = MP1, IGH
  100    ORT(I) = A(I,MP-1)
C
         DO 130 J = MP, IGH
            G = 0.0
C
            DO 110 I = MP, IGH
  110       G = G + ORT(I) * Z(I,J)
C     ********** DIVISOR BELOW IS NEGATIVE OF H FORMED IN ORTHES.
C                DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW **********
            G = (G / ORT(MP)) / A(MP,MP-1)
C
            DO 120 I = MP, IGH
  120       Z(I,J) = Z(I,J) + G * ORT(I)
C
  130    CONTINUE
C
  140 CONTINUE
C
  200 RETURN
C     ********** LAST CARD OF ETYT **********
      END
C     ******************************************************************
C
C ANFANG UNTERPROGRAMM
      SUBROUTINE ETY2(NM,N,LOW,IGH,H,WR,WI,Z,IERR)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
C
      INTEGER I,J,K,L,M,N,EN,II,JJ,LL,MM,NA,NM,NN,
     X        IGH,ITS,LOW,MP2,ENM2,IERR
      REAL*8 H(NM,N),WR(N),WI(N),Z(NM,N)
      REAL*8 P,Q,R,S,T,W,X,Y,RA,SA,VI,VR,ZZ,NORM,MACHEP
C     REAL*8 SQRT,ABS,SIGN
C     INTEGER MIN0
      LOGICAL NOTLAS
C     COMPLEX Z3
      REAL*8 Z3R,Z3I
C     COMPLEX CMPLX
C     REAL*8 REAL,AIMAG
      save
C
C
C
C     THIS SUBROUTINE IS A TRANSLATION OF THE ALGOL PROCEDURE HQR2,
C     NUM. MATH. 16, 181-204(1970) BY PETERS AND WILKINSON.
C     HANDBOOK FOR AUTO. COMP., VOL.II-LINEAR ALGEBRA, 372-395(1971).
C
C     THIS SUBROUTINE FINDS THE EIGENVALUES AND EIGENVECTORS
C     OF A REAL UPPER HESSENBERG MATRIX BY THE QR METHOD.  THE
C     EIGENVECTORS OF A REAL GENERAL MATRIX CAN ALSO BE FOUND
C     IF  ELMHES  AND  ELTRAN  OR  ORTHES  AND  ORTRAN  HAVE
C     BEEN USED TO REDUCE THIS GENERAL MATRIX TO HESSENBERG FORM
C     AND TO ACCUMULATE THE SIMILARITY TRANSFORMATIONS.
C
C     ON INPUT-
C
C        NM MUST BE SET TO THE ROW DIMENSION OF TWO-DIMENSIONAL
C          ARRAY PARAMETERS AS DECLARED IN THE CALLING PROGRAM
C          DIMENSION STATEMENT,
C
C        N IS THE ORDER OF THE MATRIX,
C
C        LOW AND IGH ARE INTEGERS DETERMINED BY THE BALANCING
C          SUBROUTINE  BALANC.  IF  BALANC  HAS NOT BEEN USED,
C          SET LOW=1, IGH=N,
C
C        H CONTAINS THE UPPER HESSENBERG MATRIX,
C
C        Z CONTAINS THE TRANSFORMATION MATRIX PRODUCED BY  ELTRAN
C          AFTER THE REDUCTION BY  ELMHES, OR BY  ORTRAN  AFTER THE
C          REDUCTION BY  ORTHES, IF PERFORMED.  IF THE EIGENVECTORS
C          OF THE HESSENBERG MATRIX ARE DESIRED, Z MUST CONTAIN THE
C          IDENTITY MATRIX.
C
C     ON OUTPUT-
C
C        H HAS BEEN DESTROYED,
C
C        WR AND WI CONTAIN THE REAL AND IMAGINARY PARTS,
C          RESPECTIVELY, OF THE EIGENVALUES.  THE EIGENVALUES
C          ARE UNORDERED EXCEPT THAT COMPLEX CONJUGATE PAIRS
C          OF VALUES APPEAR CONSECUTIVELY WITH THE EIGENVALUE
C          HAVING THE POSITIVE IMAGINARY PART FIRST.  IF AN
C          ERROR EXIT IS MADE, THE EIGENVALUES SHOULD BE CORRECT
C          FOR INDICES IERR+1,...,N,
C
C        Z CONTAINS THE REAL AND IMAGINARY PARTS OF THE EIGENVECTORS.
C          IF THE I-TH EIGENVALUE IS REAL, THE I-TH COLUMN OF Z
C          CONTAINS ITS EIGENVECTOR.  IF THE I-TH EIGENVALUE IS COMPLEX
C          WITH POSITIVE IMAGINARY PART, THE I-TH AND (I+1)-TH
C          COLUMNS OF Z CONTAIN THE REAL AND IMAGINARY PARTS OF ITS
C          EIGENVECTOR.  THE EIGENVECTORS ARE UNNORMALIZED.  IF AN
C          ERROR EXIT IS MADE, NONE OF THE EIGENVECTORS HAS BEEN FOUND,
C
C        IERR IS SET TO
C          ZERO       FOR NORMAL RETURN,
C          J          IF THE J-TH EIGENVALUE HAS NOT BEEN
C                     DETERMINED AFTER 30 ITERATIONS.
C
C     ARITHMETIC IS REAL*8. COMPLEX DIVISION
C     IS SIMULATED BY ROUTIN ETDIV.
C
C     FORTRAN ROUTINE BY B. S. GARBOW.
C     MODIFIED BY F. NERI.
C
C
C     ********** MACHEP IS A MACHINE DEPENDENT PARAMETER SPECIFYING
C                THE RELATIVE PRECISION OF REALING POINT ARITHMETIC.
C
C                **********
      MACHEP = C1M17
C     MACHEP = R1MACH(4)
C
      IERR = 0
      NORM = 0.0
      K = 1
C     ********** STORE ROOTS ISOLATED BY BALANC
C                AND COMPUTE MATRIX NORM **********
      DO 50 I = 1, N
C
         DO 40 J = K, N
   40    NORM = NORM + ABS(H(I,J))
C
         K = I
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 50
         WR(I) = H(I,I)
         WI(I) = 0.0
   50 CONTINUE
C
      EN = IGH
      T = 0.0
C     ********** SEARCH FOR NEXT EIGENVALUES **********
   60 IF (EN .LT. LOW) GO TO 340
      ITS = 0
      NA = EN - 1
      ENM2 = NA - 1
C     ********** LOOK FOR SINGLE SMALL SUB-DIAGONAL ELEMENT
C                FOR L=EN STEP -1 UNTIL LOW DO -- **********
   70 DO 80 LL = LOW, EN
         L = EN + LOW - LL
         IF (L .EQ. LOW) GO TO 100
         S = ABS(H(L-1,L-1)) + ABS(H(L,L))
         IF (S .EQ. 0.0) S = NORM
         IF (ABS(H(L,L-1)) .LE. MACHEP * S) GO TO 100
   80 CONTINUE
C     ********** FORM SHIFT **********
  100 X = H(EN,EN)
      IF (L .EQ. EN) GO TO 270
      Y = H(NA,NA)
      W = H(EN,NA) * H(NA,EN)
      IF (L .EQ. NA) GO TO 280
      IF (ITS .EQ. 30) GO TO 1000
      IF (ITS .NE. 10 .AND. ITS .NE. 20) GO TO 130
C     ********** FORM EXCEPTIONAL SHIFT **********
      T = T + X
C
      DO 120 I = LOW, EN
  120 H(I,I) = H(I,I) - X
C
      S = ABS(H(EN,NA)) + ABS(H(NA,ENM2))
      X = 0.75 * S
      Y = X
      W = -0.4375 * S * S
  130 ITS = ITS + 1
C     ********** LOOK FOR TWO CONSECUTIVE SMALL
C                SUB-DIAGONAL ELEMENTS.
C                FOR M=EN-2 STEP -1 UNTIL L DO -- **********
      DO 140 MM = L, ENM2
         M = ENM2 + L - MM
         ZZ = H(M,M)
         R = X - ZZ
         S = Y - ZZ
         P = (R * S - W) / H(M+1,M) + H(M,M+1)
         Q = H(M+1,M+1) - ZZ - R - S
         R = H(M+2,M+1)
         S = ABS(P) + ABS(Q) + ABS(R)
         P = P / S
         Q = Q / S
         R = R / S
         IF (M .EQ. L) GO TO 150
         IF (ABS(H(M,M-1)) * (ABS(Q) + ABS(R)) .LE. MACHEP * ABS(P)
     X    * (ABS(H(M-1,M-1)) + ABS(ZZ) + ABS(H(M+1,M+1)))) GO TO 150
  140 CONTINUE
C
  150 MP2 = M + 2
C
      DO 160 I = MP2, EN
         H(I,I-2) = 0.0
         IF (I .EQ. MP2) GO TO 160
         H(I,I-3) = 0.0
  160 CONTINUE
C     ********** DOUBLE QR STEP INVOLVING ROWS L TO EN AND
C                COLUMNS M TO EN **********
      DO 260 K = M, NA
         NOTLAS = K .NE. NA
         IF (K .EQ. M) GO TO 170
         P = H(K,K-1)
         Q = H(K+1,K-1)
         R = 0.0
         IF (NOTLAS) R = H(K+2,K-1)
         X = ABS(P) + ABS(Q) + ABS(R)
         IF (X .EQ. 0.0) GO TO 260
         P = P / X
         Q = Q / X
         R = R / X
  170    S = SIGN(SQRT(P*P+Q*Q+R*R),P)
         IF (K .EQ. M) GO TO 180
         H(K,K-1) = -S * X
         GO TO 190
  180    IF (L .NE. M) H(K,K-1) = -H(K,K-1)
  190    P = P + S
         X = P / S
         Y = Q / S
         ZZ = R / S
         Q = Q / P
         R = R / P
C     ********** ROW MODIFICATION **********
         DO 210 J = K, N
            P = H(K,J) + Q * H(K+1,J)
            IF (.NOT. NOTLAS) GO TO 200
            P = P + R * H(K+2,J)
            H(K+2,J) = H(K+2,J) - P * ZZ
  200       H(K+1,J) = H(K+1,J) - P * Y
            H(K,J) = H(K,J) - P * X
  210    CONTINUE
C
         J = MIN(EN,K+3)
C     ********** COLUMN MODIFICATION **********
         DO 230 I = 1, J
            P = X * H(I,K) + Y * H(I,K+1)
            IF (.NOT. NOTLAS) GO TO 220
            P = P + ZZ * H(I,K+2)
            H(I,K+2) = H(I,K+2) - P * R
  220       H(I,K+1) = H(I,K+1) - P * Q
            H(I,K) = H(I,K) - P
  230    CONTINUE
C     ********** ACCUMULATE TRANSFORMATIONS **********
         DO 250 I = LOW, IGH
            P = X * Z(I,K) + Y * Z(I,K+1)
            IF (.NOT. NOTLAS) GO TO 240
            P = P + ZZ * Z(I,K+2)
            Z(I,K+2) = Z(I,K+2) - P * R
  240       Z(I,K+1) = Z(I,K+1) - P * Q
            Z(I,K) = Z(I,K) - P
  250    CONTINUE
C
  260 CONTINUE
C
      GO TO 70
C     ********** ONE ROOT FOUND **********
  270 H(EN,EN) = X + T
      WR(EN) = H(EN,EN)
      WI(EN) = 0.0
      EN = NA
      GO TO 60
C     ********** TWO ROOTS FOUND **********
  280 P = (Y - X) / 2.0
      Q = P * P + W
      ZZ = SQRT(ABS(Q))
      H(EN,EN) = X + T
      X = H(EN,EN)
      H(NA,NA) = Y + T
      IF (Q .LT. 0.0) GO TO 320
C     ********** REAL PAIR **********
      ZZ = P + SIGN(ZZ,P)
      WR(NA) = X + ZZ
      WR(EN) = WR(NA)
      IF (ZZ .NE. 0.0) WR(EN) = X - W / ZZ
      WI(NA) = 0.0
      WI(EN) = 0.0
      X = H(EN,NA)
      S = ABS(X) + ABS(ZZ)
      P = X / S
      Q = ZZ / S
      R = SQRT(P*P+Q*Q)
      P = P / R
      Q = Q / R
C     ********** ROW MODIFICATION **********
      DO 290 J = NA, N
         ZZ = H(NA,J)
         H(NA,J) = Q * ZZ + P * H(EN,J)
         H(EN,J) = Q * H(EN,J) - P * ZZ
  290 CONTINUE
C     ********** COLUMN MODIFICATION **********
      DO 300 I = 1, EN
         ZZ = H(I,NA)
         H(I,NA) = Q * ZZ + P * H(I,EN)
         H(I,EN) = Q * H(I,EN) - P * ZZ
  300 CONTINUE
C     ********** ACCUMULATE TRANSFORMATIONS **********
      DO 310 I = LOW, IGH
         ZZ = Z(I,NA)
         Z(I,NA) = Q * ZZ + P * Z(I,EN)
         Z(I,EN) = Q * Z(I,EN) - P * ZZ
  310 CONTINUE
C
      GO TO 330
C     ********** COMPLEX PAIR **********
  320 WR(NA) = X + P
      WR(EN) = X + P
      WI(NA) = ZZ
      WI(EN) = -ZZ
  330 EN = ENM2
      GO TO 60
C     ********** ALL ROOTS FOUND.  BACKSUBSTITUTE TO FIND
C                VECTORS OF UPPER TRIANGULAR FORM **********
  340 IF (NORM .EQ. 0.0) GO TO 1001
C     ********** FOR EN=N STEP -1 UNTIL 1 DO -- **********
      DO 800 NN = 1, N
         EN = N + 1 - NN
         P = WR(EN)
         Q = WI(EN)
         NA = EN - 1
         IF (Q) 710, 600, 800
C     ********** REAL VECTOR **********
  600    M = EN
         H(EN,EN) = 1.0
         IF (NA .EQ. 0) GO TO 800
C     ********** FOR I=EN-1 STEP -1 UNTIL 1 DO -- **********
         DO 700 II = 1, NA
            I = EN - II
            W = H(I,I) - P
            R = H(I,EN)
            IF (M .GT. NA) GO TO 620
C
            DO 610 J = M, NA
  610       R = R + H(I,J) * H(J,EN)
C
  620       IF (WI(I) .GE. 0.0) GO TO 630
            ZZ = W
            S = R
            GO TO 700
  630       M = I
            IF (WI(I) .NE. 0.0) GO TO 640
            T = W
            IF (W .EQ. 0.0) T = MACHEP * NORM
            H(I,EN) = -R / T
            GO TO 700
C     ********** SOLVE REAL EQUATIONS **********
  640       X = H(I,I+1)
            Y = H(I+1,I)
            Q = (WR(I) - P) * (WR(I) - P) + WI(I) * WI(I)
            T = (X * S - ZZ * R) / Q
            H(I,EN) = T
            IF (ABS(X) .LE. ABS(ZZ)) GO TO 650
            H(I+1,EN) = (-R - W * T) / X
            GO TO 700
  650       H(I+1,EN) = (-S - Y * T) / ZZ
  700    CONTINUE
C     ********** END REAL VECTOR **********
         GO TO 800
C     ********** COMPLEX VECTOR **********
  710    M = NA
C     ********** LAST VECTOR COMPONENT CHOSEN IMAGINARY SO THAT
C                EIGENVECTOR MATRIX IS TRIANGULAR **********
         IF (ABS(H(EN,NA)) .LE. ABS(H(NA,EN))) GO TO 720
         H(NA,NA) = Q / H(EN,NA)
         H(NA,EN) = -(H(EN,EN) - P) / H(EN,NA)
         GO TO 730
C 720    Z3 = CMPLX(0.0,-H(NA,EN)) / CMPLX(H(NA,NA)-P,Q)
C        H(NA,NA) = REAL(Z3)
C        H(NA,EN) = AIMAG(Z3)
  720    CALL ETDIV(Z3R,Z3I,ZERO,-H(NA,EN),H(NA,NA)-P,Q)
         H(NA,NA) = Z3R
         H(NA,EN) = Z3I
  730    H(EN,NA) = 0.0
         H(EN,EN) = 1.0
         ENM2 = NA - 1
         IF (ENM2 .EQ. 0) GO TO 800
C     ********** FOR I=EN-2 STEP -1 UNTIL 1 DO -- **********
         DO 790 II = 1, ENM2
            I = NA - II
            W = H(I,I) - P
            RA = 0.0
            SA = H(I,EN)
C
            DO 760 J = M, NA
               RA = RA + H(I,J) * H(J,NA)
               SA = SA + H(I,J) * H(J,EN)
  760       CONTINUE
C
            IF (WI(I) .GE. 0.0) GO TO 770
            ZZ = W
            R = RA
            S = SA
            GO TO 790
  770       M = I
            IF (WI(I) .NE. 0.0) GO TO 780
C           Z3 = CMPLX(-RA,-SA) / CMPLX(W,Q)
C           H(I,NA) = REAL(Z3)
C           H(I,EN) = AIMAG(Z3)
            CALL ETDIV(Z3R,Z3I,-RA,-SA,W,Q)
            H(I,NA) = Z3R
            H(I,EN) = Z3I
            GO TO 790
C     ********** SOLVE COMPLEX EQUATIONS **********
  780       X = H(I,I+1)
            Y = H(I+1,I)
            VR = (WR(I) - P) * (WR(I) - P) + WI(I) * WI(I) - Q * Q
            VI = (WR(I) - P) * 2.0 * Q
            IF (VR .EQ. 0.0 .AND. VI .EQ. 0.0) VR = MACHEP * NORM
     X       * (ABS(W) + ABS(Q) + ABS(X) + ABS(Y) + ABS(ZZ))
C           Z3 = CMPLX(X*R-ZZ*RA+Q*SA,X*S-ZZ*SA-Q*RA) / CMPLX(VR,VI)
C           H(I,NA) = REAL(Z3)
C           H(I,EN) = AIMAG(Z3)
            CALL ETDIV(Z3R,Z3I,X*R-ZZ*RA+Q*SA,X*S-ZZ*SA-Q*RA,VR,VI)
            H(I,NA) = Z3R
            H(I,EN) = Z3I
            IF (ABS(X) .LE. ABS(ZZ) + ABS(Q)) GO TO 785
            H(I+1,NA) = (-RA - W * H(I,NA) + Q * H(I,EN)) / X
            H(I+1,EN) = (-SA - W * H(I,EN) - Q * H(I,NA)) / X
            GO TO 790
C 785       Z3 = CMPLX(-R-Y*H(I,NA),-S-Y*H(I,EN)) / CMPLX(ZZ,Q)
C           H(I+1,NA) = REAL(Z3)
C           H(I+1,EN) = AIMAG(Z3)
  785       CALL ETDIV(Z3R,Z3I,-R-Y*H(I,NA),-S-Y*H(I,EN),ZZ,Q)
            H(I+1,NA) = Z3R
            H(I+1,EN) = Z3I
  790    CONTINUE
C     ********** END COMPLEX VECTOR **********
  800 CONTINUE
C     ********** END BACK SUBSTITUTION.
C                VECTORS OF ISOLATED ROOTS **********
      DO 840 I = 1, N
         IF (I .GE. LOW .AND. I .LE. IGH) GO TO 840
C
         DO 820 J = I, N
  820    Z(I,J) = H(I,J)
C
  840 CONTINUE
C     ********** MULTIPLY BY TRANSFORMATION MATRIX TO GIVE
C                VECTORS OF ORIGINAL FULL MATRIX.
C                FOR J=N STEP -1 UNTIL LOW DO -- **********
      DO 880 JJ = LOW, N
         J = N + LOW - JJ
         M = MIN(J,IGH)
C
         DO 880 I = LOW, IGH
            ZZ = 0.0
C
            DO 860 K = LOW, M
  860       ZZ = ZZ + Z(I,K) * H(K,J)
C
            Z(I,J) = ZZ
  880 CONTINUE
C
      GO TO 1001
C     ********** SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS **********
 1000 IERR = EN
 1001 RETURN
C     ********** LAST CARD OF ETY2 **********
      END
C
*********************************************************
C
C ANFANG UNTERPROGRAMM
      SUBROUTINE ETDIV(A,B,C,D,E,F)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
C   COMPUTES THE COMPLEX DIVISION
C     A + IB = (C + ID)/(E + IF)
C  VERY SLOW, BUT TRIES TO BE AS ACCURATE AS
C  POSSIBLE BY CHANGING THE ORDER OF THE
C  OPERATIONS, SO TO AVOID UNDER(OVER)FLOW
C  PROBLEMS.
C  WRITTEN BY F. NERI FEB. 12 1986
C
      REAL*8 A,B,C,D,E,F
      REAL*8 S,T
      REAL*8 CC,DD,EE,FF
      REAL*8 TEMP
      INTEGER FLIP
      save
      FLIP = 0
      CC = C
      DD = D
      EE = E
      FF = F
      IF( ABS(F).GE.ABS(E) ) THEN
        EE = F
        FF = E
        CC = D
        DD = C
        FLIP = 1
      ENDIF
      S = ONE/EE
      T = ONE/(EE+ FF*(FF*S))
      IF ( ABS(FF) .GE. ABS(S) ) THEN
        TEMP = FF
        FF = S
        S = TEMP
      ENDIF
      IF( ABS(DD) .GE. ABS(S) ) THEN
        A = T*(CC + S*(DD*FF))
      ELSE IF ( ABS(DD) .GE. ABS(FF) ) THEN
        A = T*(CC + DD*(S*FF))
      ELSE
        A = T*(CC + FF*(S*DD))
      ENDIF
      IF ( ABS(CC) .GE. ABS(S)) THEN
        B = T*(DD - S*(CC*FF))
      ELSE IF ( ABS(CC) .GE. ABS(FF)) THEN
        B = T*(DD - CC*(S*FF))
      ELSE
        B = T*(DD - FF*(S*CC))
      ENDIF
      IF (FLIP.NE.0 ) THEN
        B = -B
      ENDIF
      RETURN
      END
C**********************************************************
C
C    SYMPL3
C
C**********************************************************
C
C  WRITTEN BY F. NERI  FEB 7 1986
C
C ANFANG UNTERPROGRAMM
      SUBROUTINE SYMPL3(M)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
      INTEGER N
      PARAMETER ( N = 3 )
      REAL*8 M(2*N,2*N)
C
C   ON RETURN ,THE MATRIX M(*,*), SUPPOSED TO BE ALMOST
C   SYMPLECTIC ON ENTRY IS MADE EXACTLY SYMPLECTIC BY
C   USING A NON ITERATIVE, CONSTRUCTIVE METHOD.
C
      INTEGER KP,KQ,LP,LQ,JP,JQ,I
      save
C
      DO 100 KP=2,2*N,2
        KQ = KP-1
        DO 200 LP=2,KP-2,2
          LQ = LP-1
          QQ = ZERO
          PQ = ZERO
          QP = ZERO
          PP = ZERO
          DO 300 JP=2,2*N,2
            JQ = JP-1
            QQ = QQ + M(LQ,JQ)*M(KQ,JP) - M(LQ,JP)*M(KQ,JQ)
            PQ = PQ + M(LP,JQ)*M(KQ,JP) - M(LP,JP)*M(KQ,JQ)
            QP = QP + M(LQ,JQ)*M(KP,JP) - M(LQ,JP)*M(KP,JQ)
            PP = PP + M(LP,JQ)*M(KP,JP) - M(LP,JP)*M(KP,JQ)
  300     CONTINUE
C         WRITE(6,*) QQ,PQ,QP,PP
          DO 400 I=1,2*N
            M(KQ,I) = M(KQ,I) - QQ*M(LP,I) + PQ*M(LQ,I)
            M(KP,I) = M(KP,I) - QP*M(LP,I) + PP*M(LQ,I)
  400     CONTINUE
  200   CONTINUE
        QP = ZERO
        DO 500 JP=2,2*N,2
          JQ = JP-1
          QP = QP + M(KQ,JQ)*M(KP,JP) - M(KQ,JP)*M(KP,JQ)
  500   CONTINUE
C       WRITE(6,*) QP
        DO 600 I=1,2*N
          M(KP,I) = M(KP,I)/QP
  600   CONTINUE
C
C  MAYBE THE FOLLOWING IS A BETTER IDEA ( USES SQRT AND IS SLOWER )
C       SIGN = ONE
C       IF ( QP.LT.ZERO ) SIGN = -ONE
C  OR, BETTER:
C       IF ( QP.LE.ZERO ) THEN COMPLAIN
C       QP = ABS(QP)
C       QP = SQRT(QP)
C       DO 600 I=1,2*N
C         M(KQ,I) = M(KQ,I)/QP
C         M(KP,I) = SIGN*M(KP,I)/QP
C 600   CONTINUE
  100 CONTINUE
      RETURN
      END
*******************************************************************************
*                                                                             *
*                                                                             *
*                                                                             *
*                                                                             *
*                     DIFFERENTIAL ALGEBRA PACKAGE                            *
*                     ****************************                            *
*                                                                             *
*                                                                             *
*                          M. BERZ, 1987, 1988                                *
*                                                                             *
*                                                                             *
*         VERSION FOR MACHINE IN LINE THAT IS NOT COMMENTED OFF               *
  
*SUN                                                                     *SUN *
*        TO CREATE DIFFERENT VERSIONS, USE THE PROGRAM 'VERSION'              *
*                                                                             *
*                      RULES OF THE GAME (EXCERPT)                            *
*                      ***************************                            *
*                                                                             *
*     THIS DIFFERENTIAL ALGEBRA PACKAGE MAY BE OBTAINED ONLY FROM THE AUTHOR. *
*     BY USING THIS PACKAGE, THE USER AGREES NOT TO GIVE THE PROGRAM TO       *
*     OTHER INDIVIDUALS. IF SOMEBODY ELSE IS INTERESTED IN THE CODE, THEY     *
*     SHOULD CONTACT THE AUTHOR AT ANY OF THE ABOVE ADRESSES, STATING FOR     *
*     WHAT PURPOSE THE PACKAGE SHALL BE USED AND AFFIRMING THAT IT WILL NOT   *
*     BE GIVEN TO OTHERS.                                                     *
*                                                                             *
*     THIS VERSION IS CURRENT AS OF 11/07/88                                  *
*                                                                             *
*******************************************************************************
*
*
*     THIS FILE CONTAINS ROUTINES TO PERFORM DIFFERENTIAL ALGEBRA (DA)
*     AS AN OPTION, ALSO COMPONENTWISE ALGEBRA (CA) CAN BE PERFORMED.
*     A DESCRIPTION OF THE INTERNAL ARRAYS USED BY THE ROUTINES CAN
*     BE FOUND IN BLOCKDATA DABLD.
*
*
*     SHORT REFERENCE CHART
*     *********************
*
*     THE PARAMETERS USED BELOW HAVE THE FOLLOWING MEANING:
*
*     A,B:                NAME OF INPUT DA VECTORS   (INTEGER)
*     C:                  NAME OF OUTPUT DA VECTOR   (INTEGER)
*     X,Y:                NAME OF INPUT DA MATRIX    (INTEGER(...))
*     Z:                  NAME OF OUTPUT DA MATRIX   (INTEGER(...))
*
*     F:                  NAME OF A DA FUNCTION      (CHARACTER*4)
*     G:                  NAME OF EXTERNAL FUNCTION  (REAL*8)
*     JJ:                 ARRAY OF EXPONENTS         (INTEGER(LNV))
*     O:                  ORDER                      (INTEGER)
*     N:                  NUMBER OF VARIABLES        (INTEGER)
*     I,J,K:              INTEGER NUMBER             (INTEGER
*     R,RA,RB:            REAL NUMBERS               (REAL*8)
*     H:                  ARRAY OF LENGTH LH         (REAL*8)
*     U:                  OUTPUT UNIT NUMBER         (INTEGER)
*     T:                  COMMENT TEXT               (CHARACTER*10)
*
*
*               SUBROUTINES AND THEIR CALLING PARAMETERS
*               ****************************************
*
*     DAINI(O,N,U):       INITIALIZES CONTROL ARRAYS AND SETS MAX. ORDER O AND
*                         MAX. NUMBER OF VARIABLES N. MUST BE CALLED BEFORE ANY
*                         OTHER DA ROUTINE CAN BE USED.
*
*     DAALL(A,I,T,O,N):   ALLOCATES SPACE FOR I VECTORS A. T: CHARACTER NAME
*     DADAL(A,I):         DEALLOCATES THE I VECTORS A.
*     DAVAR(A,R,I):       MAKES A INDEPENDENT VARIABLE # I WITH INITIAL VALUE R
*     DACON(A,R):         SETS A TO CONSTANT R
*     DANOT(O):           SETS NEW TRUNCATION ORDER O FOR DA OPERATIONS
*     DAEPS(R):           SETS NEW ZERO TOLERANCE EPSILON
*
*     DAPEK(A,JJ,R):      RETURNS COEF R OF MONOMIAL WITH EXPONENTS JJ OF A
*     DAPOK(A,JJ,R):      SETS COEF OF MONOMIAL WITH EXPONENTS JJ OF A TO R
*
*     DACOP(A,C):         PERFORMS C = A
*     DAADD(A,B,C):       PERFORMS C = A + B
*     DASUB(A,B,C):       PERFORMS C = A - B
*     DAMUL(A,B,C):       PERFORMS C = A * B
*     DADIV(A,B,C):       PERFORMS C = A / B
*     DASQR(A,C):         PERFORMS C = A^2           (SQUARE OF A)
*
*     DACAD(A,RA,C):      PERFORMS C = A + RA
*     DACSU(A,RA,C):      PERFORMS C = A - RA
*     DASUC(A,RA,C):      PERFORMS C = RA - A
*     DACMU(A,RA,C):      PERFORMS C = A * RA
*     DACDI(A,RA,C):      PERFORMS C = A / RA
*     DADIC(A,RA,C):      PERFORMS C = RA / A
*     DACMA(A,B,RB,C):    PERFORMS C = A + RB*B
*     DALIN(A,RA,B,RB,C): PERFORMS C = A*RA + B*RB
*     DAFUN(F,A,C):       PERFORMS C = F(A)          (DA FUNCTION)
*
*     DAABS(A,R):         PERFORMS R = |A|           (NORM OF A)
*     DACOM(A,B,R):       PERFORMS R = |A-B|         (NORM OF A-B)
*     DAPOS(A,C):         PERFORMS C(I) = |A(I)|     (MAKE SIGNS POSITIVE)
*
*     DACCT(X,I,Y,J,Z,K)  CONCATENATES Z = X O Y;   I,J,K: # OF VECTORS IN X,Y,Z
  
*     DAINV(X,I,Z,K)      INVERTS Z = X^-1;           I,J: # OF VECTORS IN X,Y
*     DAPIN(X,I,Z,K,JJ)   PARTIALLY INVERTS Z = X^-1; I,J: # OF VECTORS IN X,Y,
*                         JJ: ARRAY; NONZERO ENTRIES DENOTE TO BE INVERTED LINES
  
*     DATRK(X,I,Y,J,N,Z,K)TRACKS CA VECTOR Y N TIMES THROUGH DA MATRIX X
*
*     DADER(I,A,C):       PERFORMS C = DA/DI (DERIV. WITH RESPECT TO VARIABLE I)
  
*     DAPOI(A,B,C,I):     PERFORMS C = [A,B] (POISSON BRACKET, 2*I: # PHASEVARS
*     DACFU(A,G,C):       MULTIPLIES COEFFICIENTS WITH FUNCTION G(JJ)
*     DAMUF(G):           FILTER FOR DAMUL: ONLY TERMS WITH G(JJ).NE.0 ARE KEPT
*
*     DAIMP(H,LH,A):      "IMPORTS" THE ARRAY H WITH LENGTH LH TO DA VAR A
*     DAEXP(H,LH,A):      "EXPORTS" THE DA VAR A TO ARRAY H WITH LENGTH LH
*     DAPRI(A,U):         PRINTS DA VECTOR A TO UNIT U
*     DAREA(A,U):         READS DA VECTOR A FROM UNIT U
*     DADEB(U,T,I):       DEBUGGER, DUMPS TO U. T: MEMO, I=0: RETURN, I=1:STOP
*     DARAN(A,R):         FILLS A WITH RANDOM NUMBERS. R: FILLFACTOR
*     DANUM(O,N,I):       COMPUTES NUMBER OF MONOMIALS IN N VAR THROUGH ORDER O
*FF
*
*     ADDITIONAL ROUTINES THE USER DOES NOT NEED TO CALL:
*
*     DAINF: RETURNS INFOS ABOUT A DA VECTOR PREVIOUSLY DECLARED
*     DAPAC: PACKS DA VECTORS
*     DAUNP: UNPACKS DA VECTOR
*     DACHK: CHECKS IF DA VECTORS HAVE COMPATIBLE ATTRIBUTES
*     DCODE: TRANSFORMS DIGITS IN A CERTAIN BASE TO A DECIMAL INTEGER
*     NCODE: EXTRACTS DIGITS IN A CERTAIN BASE FROM A DECIMAL INTEGER
*
*
*     FURTHER WISHES
*     **************
*
*     - CHECK DAREA AND DAPRI FOR CA VECTORS
*     - MAKE DAFUN USE DASQR
*
*FF
*FRS  BLOCKDATA DABLD
*     ***************
*
*     THIS SUBROUTINE CONTAINS ALL THE PARAMETERS AND COMMON BLOCKS
*     NEEDED BY THE DIFFERENTIAL ALGEBRA PACKAGE AND INITIALIZES THE ARRAYS.
*
*     PARAMETERS:
*
*     LDA: MAXIMUM NUMBER OF DA-VECTORS;    CAN BE CHANGED QUITE ARBITRARILY
*     LST: LENGTH OF MAIN STORAGE STACK;    CAN BE CHANGED QUITE ARBITRARILY
*     LEA: MAXIMUM NUMBER OF MONOMIALS;     CAN BE INCREASED FOR LARGE NO,NV
*     LIA: DIMENSION OF IA1,IA2;            CAN BE INCREASED FOR LARGE NO,NV
*     LNO: MAXIMUM ORDER;                   CAN BE INCREASED TO ABOUT 1000
*     LNV: MAXIMUM NUMBER OF VARIABLES;     CAN BE INCREASED TO ABOUT 1000
*
*     ALL THE CHANGES IN THE VALUES OF PARAMETERS HAVE TO BE MADE BY GLOBAL
*     SUBSTITUTIONS IN ALL SUBROUTINES.
*
*     DANAME:   NAME OF DA VECTOR
*
*     CC:       STACK OF DOUBLE PRECISON COEFFICIENTS
*     I1:       FIRST CHARACTERISTIC INTEGER (CF DAINI)
*     I2:       SECOND CHARACTERISTIC INTEGER (CF DAINI)
*
*     IE1:      CHARACTERISTIC INTEGER 1 OF UNPACKED REPRESENTATION (CF DAINI)
*     IE2:      CHARACTERISTIC INTEGER 2 OF UNPACKED REPRESENTATION (CF DAINI)
*     IEO:      ORDER OF ENTRY IN UNPACKED REPRESENTATION
*     IA1:      REVERSE TO IE1 (CF DAINI)
*     IA2:      REVERSE TO IE2 (CF DAINI)
*     IFI:      FILTERING ARRAY. CONTAINS ZERO IF ENTRY IS IGNORED (CF DAMUF)
*
*     IDANO:    ORDER OF DA VECTOR; IN CA, NUMBER OF COMPONENTS
*     IDANV:    NUMBER OF VARIABLES; IF 0, INDICATES CA VECTOR
*     IDAPO:    FIRST ADDRESS IN STACK
*     IDALM:    NUMBER OF RESERVED STACK POSITIONS
*     IDALL:    NUMBER OF MOMENTARILY REQUIRED STACK POSITIONS
*
*     NDA:      NUMBER OF DA VECTORS MOMENTARILY DEFINED
*     NST:      NUMBER OF STACK POSITIONS MOMENTARILY ALLOCATED
*     NOMAX:    MAXIMUM REQUESTED ORDER  (CF DAINI)
*     NVMAX:    MAXIMUM REQUESTED NUMBER OF VARIABLES (CF DAINI)
*     NMMAX:    MAXIMUM NUMBER OF MONOMIALS FOR NOMAX, NVMAX (CF DAINI)
*     NOCUT:    MOMENTARY TRUNCATION ORDER
*     EPS:      TRUNCATION ACCURACY (CAN BE SET BY USER)
*     EPSMAC:   MANTISSA LENGTH OF MACHINE (PESSIMISTIC ESTIMATE)
*     LFI:      1 IF FILTERING IS IN EFFECT (CF DAMUF)
*
*-----------------------------------------------------------------------------1
*-----------------------------------------------------------------------------9
*FRS  CHARACTER DANAME(LDA)*10                                                1
*FRS  COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*FRS  COMMON/DASCR/ISCRDA(100),RSCRRI(100),ISCRRI(100),IDAO
*-----------------------------------------------------------------------------2
*
*FF
*
*     INITIALIZING THE VARIABLES IN THE COMMON BLOCK
*     **********************************************
*
*FRS  DATA EPS    / C1M14/
CFRANK
*FRS  DATA EPS    / C1M38/
CFRANK
*SUN  DATA EPSMAC / C1M6  /                                               *SUN
*
*FRS  END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAKEY(C)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *******************
*
      CHARACTER C*(*)
      save
*
      IF(C.NE.'FOX V2.1') THEN
         PRINT*,'ERROR, WRONG VERSION OF DA PACKAGE'
         STOP
      ENDIF
      RETURN
*
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE FACT(N,DFACT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE COMPUTES THE FACTORIAL OF N
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      TFACT=1.D0
      DO 11 JF=1,N
          TFACTT=JF          
          TFACT=TFACT*TFACTT
  11  CONTINUE
      DFACT=TFACT
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAINI(NO,NV,IUNIT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE SETS UP THE MAJOR ORDERING AND ADDRESSING ARRAYS IN
*     COMMON BLOCK DAINI. IF IUNIT > 0, THE ARRAYS WILL BE PRINTED TO UNIT
*     NUMBER IUNIT. AN EXAMPLE FOR THE ARRAYS GENERATED BY DAINI CAN BE
*     FOUND AFTER THE ROUTINE.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      INTEGER          ISCRDA
      REAL*8 RSCRRI
      COMMON/DASCR/ISCRDA(100),RSCRRI(100),ISCRRI(100),IDAO
*-----------------------------------------------------------------------------2
*
      CHARACTER AA*10
      DIMENSION N(LNV+1),K(0:LNV),J(LNV),JJ(LNV)
      save
*
      IF(NV.EQ.0) RETURN
C     EPS    = C1M14
CFRANK
      EPS    = C1M38
      EPSMAC = C1M7
CFRANK
*
*     INITIALIZING VARIABLES IN COMMON / DA /
*     ***************************************
*
      NDA   = 0
      NST   = 0
      NOMAX = NO
      NVMAX = NV
      CALL DANUM(NO,NV,NMMAX)
      NOCUT = NO
      LFI   = 0
*
CWF--start
C     DO 6 I=1,LST
C        CC(I)=ZERO
C        I1(I)=0
C        I2(I)=0
C6    CONTINUE
C     DO 7 I=1,LEA
C        IE1(I)=0
C        IE2(I)=0
C        IEO(I)=0
C        IFI(I)=0
C7    CONTINUE
C     DO 8 I=1,LDA
C        IDANO(I)=0
C        IDANV(I)=0
C        IDAPO(I)=0
C        IDALM(I)=0
C        IDALL(I)=0
C8    CONTINUE
CWF--end
      DO 10 I=0,LIA
      IA1(I) = 0
      IA2(I) = 0
  10  CONTINUE
*
      DO 20 I=1,100
  20  ISCRDA(I) = 0
*
      IF(NV.GT.LNV) THEN
        PRINT*,'THE NUMBER OF VARIABLES IS TOO BIG'
        PRINT*,'TO FIX IT INCREASE THE VALUE OF LNV SO THAT LNV > ',NV
         STOP
      ELSE IF(NO.GT.LNO) THEN
        PRINT*,'THE ORDER SPECIFIED IS TOO BIG!'
        PRINT*,'TO FIX IT INCREASE THE VALUE OF LNO SO THAT LNO > ',NO
         STOP
      ENDIF
*
      IBASE = NO+1
      JS    = NV/2
      IF(DBLE(IBASE)**((NV+1)/2).GT.DBLE(LIA)) THEN
         PRINT*,'THE PARAMETER LIA IS NOT BIG ENOUGH'
         PRINT*,'TO FIX IT REPLACE THE OLD VALUE WITH LIA > ',
     *   DBLE(IBASE)**((NV+1)/2)
         STOP
      ENDIF
*
      ICMAX = 0
      NN    = 0
      K(0)  = 0
*
      DO 100 IO2=0,NO
*     ***************
*
      N(1)  = IO2
      JL    = 0
      JD    = 1
*
  50  JL    = JL + JD
*
      IF(JL.EQ.0) THEN
         GOTO 100
      ELSEIF(JD.EQ.1) THEN
         J(JL) = 0
      ELSE
         J(JL) = J(JL) + 1
      ENDIF
*
      K(JL)    = K(JL-1)*IBASE + J(JL)
      N(JL+1)  = N(JL) - J(JL)
*
      IF(J(JL).GT.N(JL)) THEN
         JD    = -1
         GOTO 50
      ELSEIF(JL.LT.JS) THEN
         JD    = 1
         GOTO 50
      ELSE
         J(JL) = N(JL)
         K(JL) = K(JL-1)*IBASE + J(JL)
         IC2   = K(JL)
         ICMAX = MAX(ICMAX,IC2)
         K(JL) = 0
*
         IA2(IC2) = NN
*
         DO 80 IO1=0,NO-IO2
*        ******************
*
         N(JS+1) = IO1
         JD      = 1
*
  70     JL      = JL + JD
*
         IF(JL.EQ.JS) THEN
            GOTO 80
         ELSEIF(JD.EQ.1) THEN
            J(JL) = 0
         ELSE
            J(JL) = J(JL) + 1
         ENDIF
*
         K(JL)    = K(JL-1)*IBASE + J(JL)
         N(JL+1)  = N(JL) - J(JL)
*
         IF(J(JL).GT.N(JL)) THEN
            JD    = -1
            GOTO 70
         ELSEIF(JL.LT.NV) THEN
            JD    = 1
            GOTO 70
         ELSE
            JD    = -1
            J(JL) = N(JL)
            K(JL) = K(JL-1)*IBASE + J(JL)
            IC1   = K(JL)
            ICMAX = MAX(ICMAX,IC1)
            NN = NN + 1
*
            IE1(NN) = IC1
            IE2(NN) = IC2
            I1 (NN) = IC1
            I2 (NN) = IC2
            IF(IC2.EQ.0) IA1(IC1) = NN
            IEO(NN) = IO1 + IO2
*
            GOTO 70
         ENDIF
*
   80    CONTINUE
*
         JD = -1
         GOTO 50
      ENDIF
*
  100 CONTINUE
  110 CONTINUE
*
      IF(NN.GT.LEA) THEN
         PRINT*,'THE PARAMETER LEA IS NOT BIG ENOUGH'
         CF=0.D0
         CALL FACT(NV-1,FAC2)
         DO 120 JORD=1,NO
            CALL FACT(NV+JORD-1,FAC1)
            CALL FACT(JORD,FAC3)
            CF=CF+(FAC1/FAC2/FAC3)
 120     CONTINUE
         NEWLEA=CF+1
         PRINT*,'TO FIX IT REPLACE THE OLD VALUE WITH LEA > ',
     *   NEWLEA
         STOP
      ENDIF
*
*     ALLOCATING SCRATCH VARIABLES
*     ****************************
*
      IALL = 0
      CALL DAALL(IALL,1,'$$UNPACK$$',NOMAX,NVMAX)
*
      DO 150 I=0,NOMAX
      AA = '$$MUL   $$'
      WRITE(AA(6:10),'(I5)') I
      IALL = 0
      CALL DAALL(IALL,1,AA,I,NVMAX)
 150  CONTINUE
*
      IDALL(1) = NMMAX
*
*     DOUBLE CHECKING ARRAYS IE1,IE2,IA1,IA2
*     **************************************
*
      DO 300 I=1,NMMAX
*
      JJJ = IA1(IE1(I)) + IA2(IE2(I))
      IF(JJJ.NE.I) THEN
         PRINT*,'ERROR IN DAINI IN ARRAYS IE1,IE2,IA1,IA2 AT I = ',I
C        CALL DADEB(31,'ERR DAINI4',1)
      ENDIF
*
 300  CONTINUE
*
      IF(IUNIT.EQ.0) RETURN
*
      PRINT*,'ARRAY SETUP DONE, BEGIN PRINTING'
*
CFRS  IOUT = 32
*
CFRS  WRITE(IOUT,'(/A/A/)') ' ARRAYS I1 THROUGH I20, IE1,IE2,IEO',
CFRS *                       ' **********************************'
      DO 200 I=1,NMMAX
      CALL DANCD(IE1(I),IE2(I),JJ)
CFRS  WRITE(IOUT,'(1X,I5,2X,4(5I2,1X),3I6)') I,(JJ(JJJJ),JJJJ=1,20),
CFRS *                     IE1(I),IE2(I),IEO(I)
 200  CONTINUE
*
CFRS  WRITE(IOUT,'(/A/A/)') ' ARRAYS IA1,IA2',' **************'
CFRS  DO 210 I=0,ICMAX
CFRS  WRITE(IOUT,'(3I10)') I,IA1(I),IA2(I)
CFRS 210  CONTINUE
*
      RETURN
      END
*
* EXAMPLE: ARRAYS I1 THROUGH I20, IE1,IE2,IEO (NOMAX=3,NVMAX=4)
* *************************************************************
*     I   I1               THROUGH               I20     IE1   IE2   IEO
*     1   0 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0      0     0     0
*     2   1 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0      1     0     1
*     3   0 1 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0      4     0     1
*     4   2 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0      2     0     2
*     5   1 1 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0      5     0     2
*     6   0 2 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0      8     0     2
*     7   3 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0      3     0     3
*     8   2 1 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0      6     0     3
*     9   1 2 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0      9     0     3
*    10   0 3 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0 0     12     0     3
*    11   0 0 0 0 0  0 0 0 0 0  1 0 0 0 0  0 0 0 0 0      0     1     1
*    12   1 0 0 0 0  0 0 0 0 0  1 0 0 0 0  0 0 0 0 0      1     1     2
*    13   0 1 0 0 0  0 0 0 0 0  1 0 0 0 0  0 0 0 0 0      4     1     2
*    14   2 0 0 0 0  0 0 0 0 0  1 0 0 0 0  0 0 0 0 0      2     1     3
*    15   1 1 0 0 0  0 0 0 0 0  1 0 0 0 0  0 0 0 0 0      5     1     3
*    16   0 2 0 0 0  0 0 0 0 0  1 0 0 0 0  0 0 0 0 0      8     1     3
*    17   0 0 0 0 0  0 0 0 0 0  0 1 0 0 0  0 0 0 0 0      0     4     1
*    18   1 0 0 0 0  0 0 0 0 0  0 1 0 0 0  0 0 0 0 0      1     4     2
*    19   0 1 0 0 0  0 0 0 0 0  0 1 0 0 0  0 0 0 0 0      4     4     2
*    20   2 0 0 0 0  0 0 0 0 0  0 1 0 0 0  0 0 0 0 0      2     4     3
*    21   1 1 0 0 0  0 0 0 0 0  0 1 0 0 0  0 0 0 0 0      5     4     3
*    22   0 2 0 0 0  0 0 0 0 0  0 1 0 0 0  0 0 0 0 0      8     4     3
*    23   0 0 0 0 0  0 0 0 0 0  2 0 0 0 0  0 0 0 0 0      0     2     2
*    24   1 0 0 0 0  0 0 0 0 0  2 0 0 0 0  0 0 0 0 0      1     2     3
*    25   0 1 0 0 0  0 0 0 0 0  2 0 0 0 0  0 0 0 0 0      4     2     3
*    26   0 0 0 0 0  0 0 0 0 0  1 1 0 0 0  0 0 0 0 0      0     5     2
*    27   1 0 0 0 0  0 0 0 0 0  1 1 0 0 0  0 0 0 0 0      1     5     3
*    28   0 1 0 0 0  0 0 0 0 0  1 1 0 0 0  0 0 0 0 0      4     5     3
*    29   0 0 0 0 0  0 0 0 0 0  0 2 0 0 0  0 0 0 0 0      0     8     2
*    30   1 0 0 0 0  0 0 0 0 0  0 2 0 0 0  0 0 0 0 0      1     8     3
*    31   0 1 0 0 0  0 0 0 0 0  0 2 0 0 0  0 0 0 0 0      4     8     3
*    32   0 0 0 0 0  0 0 0 0 0  3 0 0 0 0  0 0 0 0 0      0     3     3
*    33   0 0 0 0 0  0 0 0 0 0  2 1 0 0 0  0 0 0 0 0      0     6     3
*    34   0 0 0 0 0  0 0 0 0 0  1 2 0 0 0  0 0 0 0 0      0     9     3
*    35   0 0 0 0 0  0 0 0 0 0  0 3 0 0 0  0 0 0 0 0      0    12     3
*
*    ARRAYS IA1,IA2
*    **************
*    I        IA1       IA2
*    0         1         0   IE1,IE2 AND IA1,IA2 ALLOW THE EASY COMPUTATION
*    1         2        10   OF THE ADDRESS OF THE PRODUCT OF TWO MONOMIALS.
*    2         4        22   LET IX AND IY BE THE POSITIONS OF THE TWO
*    3         7        31   FACTORS. THEN THE POSITION IZ OF THE PRODUCT OF
*    4         3        16   THE TWO FACTORS IS GIVEN BY
*    5         5        25
*    6         8        32   IZ = IA1(IE1(IX)+IE1(IY)) + IA2(IE2(IX)+IE2(IY))
*    7         0         0
*    8         6        28
*    9         9        33   THE OTHER VARIABLES SET BY DAINI WOULD HAVE THE
*   10         0         0   VALUES
*   11         0         0
*   12        10        34   NOMAX = 3,  NVMAX = 4, NMMAX = 35
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAALL(IC,L,CCC,NO,NV)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ********************************
*
*     THIS SUBROUTINE ALLOCATES STORAGE FOR A DA VECTOR WITH
*     ORDER NO AND NUMBER OF VARIABLES NV
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*
      INTEGER IC(*)
      CHARACTER C*10,CCC*10
      save
*
      DO 10 I=1,L
*
      IF(IC(I).GT.0.AND.IC(I).LE.NDA) THEN
*        DANAME(IC(I)) = C
*        IF(IDANO(IC(I)).EQ.NO.AND.IDANV(IC(I)).EQ.NV) THEN
      ELSE
         IF(NV.NE.0.AND.(NO.GT.NOMAX.OR.NV.GT.NVMAX)) THEN
            PRINT*,'ERROR IN DAALL, VECTOR ',C,' HAS NO, NV = ',
     *              NO,NV,' NOMAX, NVMAX = ',NOMAX,NVMAX
C           CALL DADEB(31,'ERR DAALL1',1)
         ENDIF
*
         NDA = NDA + 1
*
         IF(NDA.GT.LDA) THEN
           PRINT*,'THE PARAMETER LDA IS NOT BIG ENOUGH'
           PRINT*,'TO FIX IT REPLACE THE OLD VALUE WITH LDA > ',
     *   NDA
           STOP
         ENDIF
*
         IC(I) = NDA
*
         IF(NV.NE.0) THEN
            CALL DANUM(NO,NV,NDANUM)
         ELSE
            NDANUM = NO
         ENDIF
*
         C = CCC
         IF(L.NE.1) WRITE(C(6:10),'(I5)') I
         DANAME(NDA) = C
         IDANO(NDA) = NO
         IDANV(NDA) = NV
         IDAPO(NDA) = NST + 1
         IDALM(NDA) = NDANUM
         IDALL(NDA) = 0
*
         NST = NST + NDANUM
*
         IF(NST.GT.LST) THEN
           PRINT*,'THE PARAMETER LST IS NOT BIG ENOUGH'
           PRINT*,'TO FIX IT REPLACE THE OLD VALUE WITH LST > ',
     *   NST
           STOP
         ENDIF
*
         IF(NV.EQ.0.OR.NOMAX.EQ.1) THEN
            CALL DACLR(IC(I))
            IDALL(IC(I)) = IDALM(IC(I))
         ENDIF
      ENDIF
*
  10  CONTINUE
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DADAL(IDAL,L)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ************************
*
*     THIS SUBROUTINE DEALLOCATES THE VECTORS IDAL
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      INTEGER IDAL(*)
      save
*
      DO 10 I=L,1,-1
*
      IF(IDAL(I).LE.NOMAX+2.OR.IDAL(I).GT.NDA) THEN
         PRINT*,'ERROR IN ROUTINE DADAL, IDAL(I),NDA = ',IDAL(I),NDA
C        CALL DADEB(31,'ERR DADAL1',1)
      ENDIF
*
      IF(IDAL(I).EQ.NDA) THEN
         NDA = NDA - 1
         NST = IDAPO(NDA+1) - 1
      ENDIF
*
      IDANO(IDAL(I)) = 0
      IDANV(IDAL(I)) = 0
      IDAPO(IDAL(I)) = 0
      IDALM(IDAL(I)) = 0
      IDALL(IDAL(I)) = 0
*
      IDAL(I) = 0
*
  10  CONTINUE
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAVAR(INA,CKON,I)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ****************************
*
*     THIS SUBROUTINE DECLARES THE DA VECTOR
*     AS THE INDEPENDENT VARIABLE NUMBER I.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
*
      IF(INVA.EQ.0) THEN
         PRINT*,'ERROR IN DAVAR, ',INA,' IS A DECLARED CA VARIABLE'
C        CALL DADEB(31,'ERR DAVAR1',1)
      ENDIF
*
      IF(I.GT.NVMAX) THEN
         PRINT*,'ERROR IN DAVAR, I = ',I,' EXCEEDS NVMAX = ',NVMAX
C        CALL DADEB(31,'ERR DAVAR2',1)
      ENDIF
*
      IF(NOMAX.EQ.1) THEN
         IF(I.GT.INVA) THEN
            PRINT*,'ERROR IN DAVAR, I = ',I,' EXCEEDS INVA = ',INVA
C           CALL DADEB(31,'ERR DAVAR3',1)
         ENDIF
         CALL DACLR(INA)
         CC(IPOA) = CKON
         CC(IPOA+I) = ONE
         RETURN
      ENDIF
  
      IBASE = NOMAX+1
*
      IF(I.GT.(NVMAX+1)/2) THEN
        IC1 = 0
        IC2 = IBASE**(I-(NVMAX+1)/2-1)
      ELSE
        IC1 = IBASE**(I-1)
        IC2 = 0
      ENDIF
*
      IF(ABS(CKON).GT.EPS) THEN
         IDALL(INA) = 2
         CC(IPOA) = CKON
         I1(IPOA) = 0
         I2(IPOA) = 0
*
         CC(IPOA+1) = ONE
         I1(IPOA+1) = IC1
         I2(IPOA+1) = IC2
      ELSE
         IDALL(INA) = 1
         CC(IPOA) = ONE
         I1(IPOA) = IC1
         I2(IPOA) = IC2
      ENDIF
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACON(INA,CKON)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **************************
*
*     THIS SUBROUTINE SETS THE VECTOR C TO THE CONSTANT CKON
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
*
      IF(INVA.EQ.0) THEN
         DO 10 I=IPOA,IPOA+ILLA-1
  10     CC(I) = CKON
         RETURN
      ENDIF
*
      IF(NOMAX.EQ.1) THEN
         CALL DACLR(INA)
         CC(IPOA) = CKON
         RETURN
      ENDIF
  
      IDALL(INA) = 1
      CC(IPOA) = CKON
      I1(IPOA) = 0
      I2(IPOA) = 0
      IF(ABS(CKON).LT.EPS) IDALL(INA) = 0
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DANOT(NOT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *********************
*
*     THIS SUBROUTINE RESETS THE TRUNCATION ORDER NOCUT TO A NEW VALUE
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      IF(NOT.GT.NOMAX) THEN
         PRINT*,'ERROR, NOCUT = ',NOCUT,' EXCEEDS NOMAX = ',NOMAX
C        CALL DADEB(31,'ERR DANOT1',1)
      ENDIF
*
      NOCUT = NOT
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAEPS(DEPS)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **********************
*
*     THIS SUBROUTINE RESETS THE TRUNCATION ORDER NOCUT TO A NEW VALUE
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      EPS = DEPS
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPEK(INA,JJ,CJJ)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ****************************
*
*     THIS SUBROUTINE DETERMINES THE COEFFICIENT OF THE ARRAY
*     OF EXPONENTS JJ AND RETURNS IT IN CJJ
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION JJ(*)
      save
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
*
      JJ1 = 1
      IF(INVA.EQ.0.OR.NOMAX.EQ.1) THEN
         IF(INVA.NE.0.AND.NOMAX.EQ.1) THEN
            IF(ILLA.GE.2) THEN
               DO 115 I=1,ILLA - 1
                  IF(JJ(I).EQ.1) JJ1 = I + 1
 115           CONTINUE
            ELSE
               JJ1 = JJ(1) + 1
            ENDIF
         ELSE
            JJ1 = JJ(1)
         ENDIF
         IF(JJ1.LT.1.OR.JJ1.GT.ILLA) THEN
            PRINT*,'ERROR IN DAPEK, INDEX OUTSIDE RANGE, JJ(1) = ',JJ1
C           CALL DADEB(31,'ERR DAPEK1',1)
         ENDIF
         IPEK = IPOA + JJ1 - 1
         CJJ = CC(IPEK)
         RETURN
      ENDIF
*
      II1 = (NVMAX+1)/2
      IBASE = NOMAX+1
*
*     FOR GENERAL CASE, FIRST DETERMINE INDEX TO BE SEARCHED FOR
*     **********************************************************
*
      CALL DADCD(JJ,IC1,IC2)
*
      IC = IA1(IC1) + IA2(IC2)
*
*     DETERMINE IF MONOMIAL TO BE POKED CONFORMS WITH INOA, INVA,NOCUT
*     ****************************************************************
*
CFRS  IF(ICO.GT.INOA.OR.ICV.GT.INVA.OR.ICO.GT.NOCUT) THEN
CFRS     CJJ = 0
CFRS     RETURN
CFRS  ENDIF
*
*     DETERMINE IF MONOMIAL IS INSIDE FIRST AND LAST MONOMIALS OF A
*     *************************************************************
*
      IU = IPOA
      IZ = IPOA + ILLA - 1
      ICU = IA1(I1(IU))+IA2(I2(IU))
      ICZ = IA1(I1(IZ))+IA2(I2(IZ))
*
      IF(ILLA.EQ.0) THEN
         CJJ = 0
         RETURN
      ELSEIF(IC.EQ.ICU) THEN
         CJJ = CC(IU)
         RETURN
      ELSEIF(IC.EQ.ICZ) THEN
         CJJ = CC(IZ)
         RETURN
      ELSEIF(IC.LT.ICU.OR.IC.GT.ICZ) THEN
         CJJ = 0
         RETURN
      ENDIF
*
*     SEARCHING PROPER MONOMIAL
*     *************************
*
 10   CONTINUE
      IF(IZ-IU.LE.1) THEN
         CJJ = 0
         RETURN
      ENDIF
      I = (IU+IZ)/2
*
      IF(IA1(I1(I))+IA2(I2(I)) - IC) 20,30,40
 20   IU = I
      GOTO 10
 30   CJJ = CC(I)
      RETURN
 40   IZ = I
      GOTO 10
*
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPOK(INA,JJ,CJJ)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ****************************
*
*     THIS SUBROUTINE SETS THE COEFFICIENT OF THE ARRAY
*     OF EXPONENTS JJ TO THE VALUE CJJ
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION JJ(*)
      save
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
*
*     SPECIAL CASES OF CA VECTORS AND FIRST ORDER
*     *******************************************
*
      JJ1 = 1
      IF(INVA.EQ.0.OR.NOMAX.EQ.1) THEN
         IF(INVA.NE.0.AND.NOMAX.EQ.1) THEN
            IF(ILLA.GE.2) THEN
               DO 115 I=1,ILLA - 1
                  IF(JJ(I).EQ.1) JJ1 = I + 1
 115           CONTINUE
            ELSE
               JJ1 = JJ(1) + 1
            ENDIF
         ELSE
            JJ1 = JJ(1)
         ENDIF
         IF(JJ1.LT.1.OR.JJ1.GT.ILLA) THEN
            PRINT*,'ERROR IN DAPOK, INDEX OUTSIDE RANGE, JJ(1) = ',JJ1
C           CALL DADEB(31,'ERR DAPOK1',1)
         ENDIF
         IPOK = IPOA + JJ1 - 1
         CC(IPOK) = CJJ
         RETURN
      ENDIF
*
*     FOR GENERAL CASE, FIRST DETERMINE INDEX TO BE SEARCHED FOR
*     **********************************************************
*
      CALL DADCD(JJ,IC1,IC2)
*
      IC = IA1(IC1) + IA2(IC2)
*
*     DETERMINE IF MONOMIAL TO BE POKED CONFORMS WITH INOA, INVA,NOCUT
*     ****************************************************************
*
CFRS  IF(ICO.GT.INOA.OR.ICV.GT.INVA) THEN
CFRS     PRINT*,'ERROR IN DAPOK, MONOMIAL NOT ALLOWED FOR ',INA
CFRS     CALL DADEB(31,'ERR DAPOK2',1)
CFRS  ENDIF
CFRS  IF(ICO.GT.NOCUT) RETURN
*
      IU = IPOA
      IZ = IPOA + ILLA - 1
*
*     DETERMINE IF MONOMIAL IS INSIDE FIRST AND LAST MONOMIALS OF A
*     *************************************************************
*
      ICU = IA1(I1(IU))+IA2(I2(IU))
      ICZ = IA1(I1(IZ))+IA2(I2(IZ))
      IF(ILLA.EQ.0) THEN
         I = IPOA
         GOTO 100
      ELSEIF(IC.EQ.ICU) THEN
         CC(IU) = CJJ
         I = IU
         GOTO 200
      ELSEIF(IC.EQ.ICZ) THEN
         CC(IZ) = CJJ
         I = IZ
         GOTO 200
      ELSEIF(IC.LT.ICU) THEN
         I = IU
         GOTO 100
      ELSEIF(IC.GT.ICZ) THEN
         I = IZ + 1
         GOTO 100
      ENDIF
*
*
*     SEARCHING PLACE TO POKE INTO OR BEFORE WHICH TO POKE
*     ****************************************************
*
      IU = IPOA
      IZ = IPOA + ILLA
*
 10   CONTINUE
      IF(IZ-IU.LE.1) THEN
         I = IZ
         GOTO 100
      ENDIF
      I = (IU+IZ)/2
*
      IF(IA1(I1(I))+IA2(I2(I)) - IC) 20,30,40
 20   IU = I
      GOTO 10
 30   CC(I) = CJJ
      GOTO 200
 40   IZ = I
      GOTO 10
*
*     INSERTING THE MONOMIAL, MOVING THE REST
*     ***************************************
*
 100  CONTINUE
*
      IF(ABS(CJJ).LT.EPS) RETURN
*
      DO 110 II=IPOA+ILLA,I+1,-1
      CC(II) = CC(II-1)
      I1(II) = I1(II-1)
      I2(II) = I2(II-1)
 110  CONTINUE
*
      CC(I) = CJJ
      I1(I) = IC1
      I2(I) = IC2
*
      IDALL(INA) = ILLA + 1
      IF(IDALL(INA).GT.IDALM(INA)) THEN
         PRINT*,'ERROR IN DAPOK, INA =  ',INA
C        CALL DADEB(31,'ERR DAPOK3',1)
      ENDIF
*
      RETURN
*
*     CASE OF CJJ = 0 WHICH MEANS MOVING THE REST
*     *********************************************
*
 200  CONTINUE
      IF(ABS(CJJ).LT.EPS) THEN
         DO 210 II=I,IPOA+ILLA-2
         CC(II) = CC(II+1)
         I1(II) = I1(II+1)
         I2(II) = I2(II+1)
 210     CONTINUE
         IDALL(INA) = ILLA - 1
      ENDIF
      RETURN
*
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACLR(INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *********************
*
*     THIS SUBROUTINE SETS ALL THE STACK SPACE RESERVED FOR VARIABLE
*     C TO ZERO
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      DO 100 I=IPOC,IPOC+ILMC-1
*
      CC(I) = ZERO
*
 100  CONTINUE
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACOP(INA,INB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *************************
*
*     THIS SUBROUTINE COPIES THE DA VECTOR A TO THE DA VECTOR B
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
*
      CALL DACHK(INA,INOA,INVA,0,-1,-1,INB,INOB,INVB)
*
      IB = IPOB - 1
*
      IIF = 0
      IF(NOMAX.EQ.1.OR.INVA.EQ.0) IIF = 1
  
      DO 100 IA = IPOA,IPOA+ILLA-1
*
      IF(IIF.EQ.0) THEN
         IF(IEO(IA1(I1(IA))+IA2(I2(IA))).GT.NOCUT) GOTO 100
      ENDIF
      IB = IB + 1
      CC(IB) = CC(IA)
      I1(IB) = I1(IA)
      I2(IB) = I2(IA)
*
 100  CONTINUE
*
      IDALL(INB) = IB - IPOB + 1
      IF(IDALL(INB).GT.IDALM(INB)) THEN
         PRINT*,'ERROR IN DACOP'
C        CALL DADEB(31,'ERR DACOP1',1)
      ENDIF
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAADD(INA,INB,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
      DIMENSION I_SPEC_DUMMY(1)
      save
*     *****************************
*
*     THIS SUBROUTINE PERFORMS A DA ADDITION OF THE DA VECTORS A AND B.
*     THE RESULT IS STORED IN C.
*
      IF(INA.NE.INC.AND.INB.NE.INC) THEN
         CALL DALIN(INA,ONE,INB,ONE,INC)
      ELSE
         IDAADD = 0
         CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
         I_SPEC_DUMMY(1) = IDAADD
         CALL DAALL(I_SPEC_DUMMY,1,'$$DAADD $$',INOC,INVC)
         IDAADD = I_SPEC_DUMMY(1)
         CALL DALIN(INA,ONE,INB,ONE,IDAADD)
         CALL DACOP(IDAADD,INC)
         I_SPEC_DUMMY(1) = IDAADD
         CALL DADAL(I_SPEC_DUMMY,1)
         IDAADD = I_SPEC_DUMMY(1)
      ENDIF
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DASUB(INA,INB,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
      DIMENSION I_SPEC_DUMMY(1)
      save
*     *****************************
*
*     THIS SUBROUTINE PERFORMS A DA SUBTRACTION OF THE DA VECTORS A AND B.
*     THE RESULT IS STORED IN C.
*
      IF(INA.NE.INC.AND.INB.NE.INC) THEN
         CALL DALIN(INA,ONE,INB,ONEM,INC)
      ELSE
         IDASUB = -1
         CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
         I_SPEC_DUMMY(1) = IDASUB
         CALL DAALL(I_SPEC_DUMMY,1,'$$DASUB $$',INOC,INVC)
         IDASUB = I_SPEC_DUMMY(1)
         CALL DALIN(INA,ONE,INB,ONEM,IDASUB)
         CALL DACOP(IDASUB,INC)
         I_SPEC_DUMMY(1) = IDASUB
         CALL DADAL(I_SPEC_DUMMY,1)
         IDASUB = I_SPEC_DUMMY(1)
      ENDIF
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAMUL(INA,INB,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE PERFORMS A DA MULTIPLICATION OF THE DA VECTORS A AND B.
*     THE RESULT IS STORED IN C. AS TEMPORARY STORAGE, THE STACK SPACE
*     OF THE (NOMAX+2) SCRATCH VARIABLES ALLOCATED BY DAINI IS USED.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      DIMENSION I_SPEC_DUMMY(1)
      save
*-----------------------------------------------------------------------------9
*
*
  
      IF(INA.EQ.INC.OR.INB.EQ.INC) THEN
        CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
        INCC=0
        I_SPEC_DUMMY(1) = INCC
        CALL DAALL(I_SPEC_DUMMY,1,'$$DAJUNK$$',INOC,INVC)
        INCC = I_SPEC_DUMMY(1)
        CALL DAMULT(INA,INB,INCC)
        CALL DACOP(INCC,INC)
        I_SPEC_DUMMY(1) = INCC
        CALL DADAL(I_SPEC_DUMMY,1)
        INCC = I_SPEC_DUMMY(1)
      ELSE
        CALL DAMULT(INA,INB,INC)
      ENDIF
  
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAMULT(INA,INB,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE PERFORMS A DA MULTIPLICATION OF THE DA VECTORS A AND B.
*     THE RESULT IS STORED IN C. AS TEMPORARY STORAGE, THE STACK SPACE
*     OF THE (NOMAX+2) SCRATCH VARIABLES ALLOCATED BY DAINI IS USED.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION IPNO(0:LNO),NOFF(0:LNO)
      save
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      CALL DACHK(INA,INOA,INVA, INB,INOB,INVB, INC,INOC,INVC)
*
      IF(INVA+INVB+INVC.EQ.0) THEN
         DO 5 I=0,MIN(ILLA,ILLB)-1
  5      CC(IPOC+I) = CC(IPOA+I) * CC(IPOB+I)
         IDALL(INC) = MIN(ILLA,ILLB)
         IF(IDALL(INC).GT.IDALM(INC)) THEN
            PRINT*,'ERROR IN DAMUL '
C           CALL DADEB(31,'ERR DAMUL ',1)
         ENDIF
         RETURN
      ENDIF
*
*     CASE OF FIRST ORDER ONLY
*     ************************
  
      IF(NOMAX.EQ.1) THEN
         MINV = MIN(INVA,INVB,INVC)
         CCIPOA = CC(IPOA)
         CCIPOB = CC(IPOB)
         CC(IPOC) = CCIPOA*CCIPOB
         DO 20 I=1,MINV
  20     CC(IPOC+I) = CCIPOA*CC(IPOB+I) + CCIPOB*CC(IPOA+I)
         DO 30 I=IPOC+MINV+1,IPOC+INVC
  30     CC(I) = ZERO
         RETURN
      ENDIF
*
*     GENERAL CASE
*     ************
*
      DO 10 I=0,NOMAX
      NOFF(I) = IDAPO(I+2)
  10  IPNO(I) = 0
*
      CALL DACLR(1)
*
*     RESORTING THE VECTOR B INTO PIECES THAT ARE OF ONLY ONE ORDER
*     *************************************************************
*
      DO 50 IB=IPOB,IPOB+ILLB-1
*
      NOIB = IEO(IA1(I1(IB))+IA2(I2(IB)))
      IPOS = IPNO(NOIB) + 1
      IPNO(NOIB) = IPOS
      INOB = NOFF(NOIB) + IPOS
*
      CC(INOB) = CC(IB)
      I1(INOB) = I1(IB)
      I2(INOB) = I2(IB)
*
  50  CONTINUE
*
      DO 60 I=0,NOMAX
  60  IDALL(I+2) = IPNO(I)
*
*     PERFORMING ACTUAL MULTIPLICATION
*     ********************************
*
      NOM = MIN(NOCUT,INOC)
*
      DO 100 IA=IPOA,IPOA+ILLA-1
*
      I1IA = I1(IA)
      I2IA = I2(IA)
      CCIA = CC(IA)
*
      DO 100 NOIB = 0,NOM-IEO(IA1(I1(IA))+IA2(I2(IA)))
*
      IOFFB = NOFF(NOIB)
*
      DO 100 IB = IOFFB+1,IOFFB+IPNO(NOIB)
*
      IC = IA2(I2IA+I2(IB)) + IA1(I1IA + I1(IB))
      CC(IC) = CC(IC) + CCIA*CC(IB)
*
 100  CONTINUE
*
      CALL DAPAC(INC,1)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DADIV(INA,INB,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
      DIMENSION I_SPEC_DUMMY(1)
      save
*     *****************************
*
*     THIS SUBROUTINE PERFORMS A DA DIVISION OF THE DA VECTORS A AND B.
*     THE RESULT IS STORED IN C.
*
      IDADIV = 0
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
      I_SPEC_DUMMY(1) = IDADIV
      CALL DAALL(I_SPEC_DUMMY,1,'$$DADIV $$',INOC,INVC)
      IDADIV = I_SPEC_DUMMY(1)
      CALL DAFUN('INV   ',INB,IDADIV)
      CALL DAMUL(INA,IDADIV,INC)
      I_SPEC_DUMMY(1) = IDADIV
      CALL DADAL(I_SPEC_DUMMY,1)
      IDADIV = I_SPEC_DUMMY(1)
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DASQR(INA,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *************************
*
*     THIS SUBROUTINE SQUARES THE VECTOR A AND STORES THE RESULT IN C.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      DIMENSION I_SPEC_DUMMY(1)
      save
*-----------------------------------------------------------------------------9
*
  
      IF(INA.EQ.INC) THEN
        CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
        INCC=0
        I_SPEC_DUMMY(1) = INCC
        CALL DAALL(I_SPEC_DUMMY,1,'$$DAJUNK$$',INOC,INVC)
        INCC = I_SPEC_DUMMY(1)
        CALL DASQRT(INA,INCC)
        CALL DACOP(INCC,INC)
        I_SPEC_DUMMY(1) = INCC
        CALL DADAL(I_SPEC_DUMMY,1)
        INCC = I_SPEC_DUMMY(1)
      ELSE
        CALL DASQRT(INA,INC)
      ENDIF
  
      RETURN
      END
  
C ANFANG UNTERPROGRAMM
      SUBROUTINE DASQRT(INA,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *************************
*
*     THIS SUBROUTINE SQUARES THE VECTOR A AND STORES THE RESULT IN C.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      DIMENSION IPNO(0:LNO),NOFF(0:LNO)
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      CALL DACHK(INA,INOA,INVA,0,-1,-1,INC,INOC,INVC)
*
      IF(INVA+INVC.EQ.0) THEN
         DO 5 I=0,ILLA-1
  5      CC(IPOC+I) = CC(IPOA+I) * CC(IPOA+I)
         IDALL(INC) = IDALL(INA)
         IF(IDALL(INC).GT.IDALM(INC)) THEN
            PRINT*,'ERROR IN DASQR '
C           CALL DADEB(31,'ERR DASQR ',1)
         ENDIF
         RETURN
      ENDIF
*
*     CASE OF FIRST ORDER ONLY
*     ************************
  
      IF(NOMAX.EQ.1) THEN
         MINV = MIN(INVA,INVC)
         CCIPOA = CC(IPOA)
         CC(IPOC) = CCIPOA*CCIPOA
         DO 20 I=1,MINV
  20     CC(IPOC+I) = TWO*CCIPOA*CC(IPOA+I)
         DO 30 I=IPOC+MINV+1,IPOC+INVC
  30     CC(I) = ZERO
         RETURN
      ENDIF
*
*     GENERAL CASE
*     ************
*
      DO 10 I=0,NOMAX
      NOFF(I) = IDAPO(I+2)
  10  IPNO(I) = 0
*
      CALL DACLR(1)
*
*     RESORTING THE VECTOR A INTO PIECES THAT ARE OF ONLY ONE ORDER
*     *************************************************************
*
      DO 50 IA=IPOA,IPOA+ILLA-1
*
      NOIA = IEO(IA1(I1(IA))+IA2(I2(IA)))
      IPOS = IPNO(NOIA) + 1
      IPNO(NOIA) = IPOS
      INOA = NOFF(NOIA) + IPOS
*
      CC(INOA) = CC(IA)
      I1(INOA) = I1(IA)
      I2(INOA) = I2(IA)
*
  50  CONTINUE
*
      DO 60 I=0,NOMAX
  60  IDALL(I+2) = IPNO(I)
*
*     PERFORMING ACTUAL MULTIPLICATION
*     ********************************
*
      NOM = MIN(NOCUT,INOC)
*
      DO 100 NOIA = 0,NOM/2
*
      IOFFA = NOFF(NOIA)
*
      DO 100 IA=IOFFA+1,IOFFA+IPNO(NOIA)
*
      I1IA = I1(IA)
      I2IA = I2(IA)
      CCIA = CC(IA)
*
      IC = IA2(I2IA+I2IA) + IA1(I1IA+I1IA)
      CC(IC) = CC(IC) + CCIA*CCIA
      CCIA = CCIA + CCIA
*
      DO 100 NOIB = NOIA,NOM-NOIA
*
      IOFFB = NOFF(NOIB)
      IF(NOIB.EQ.NOIA) THEN
         IB1 = IA + 1
      ELSE
         IB1 = IOFFB + 1
      ENDIF
*
      DO 100 IB = IB1,IOFFB+IPNO(NOIB)
*
      IC = IA2(I2IA+I2(IB)) + IA1(I1IA + I1(IB))
      CC(IC) = CC(IC) + CCIA*CC(IB)
*
 100  CONTINUE
*
      CALL DAPAC(INC,1)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACAD(INA,CKON,INB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE ADDS THE CONSTANT CKON TO THE VECTOR A
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      INTEGER JJ(LNV)
      save
      DATA JJ / LNV*0 /
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
*
      IF(INVA.EQ.0) THEN
         DO 10 I=0,ILLB-1
  10     CC(I+IPOB) = CC(I+IPOA) + CKON
         IDALL(INB) = ILLA
         IF(IDALL(INB).GT.IDALM(INB)) THEN
            PRINT*,'ERROR IN DACAD'
C           CALL DADEB(31,'ERR DACAD1',1)
         ENDIF
         RETURN
      ENDIF
*
      CALL DACOP(INA,INB)
*
      IF(NOMAX.EQ.1) THEN
         CC(IPOB) = CC(IPOB) + CKON
         RETURN
      ENDIF
*
      CALL DAPEK(INB,JJ,CONST)
      CALL DAPOK(INB,JJ,CONST+CKON)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACSU(INA,CKON,INB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE SUBTRACTS THE CONSTANT CKON FROM THE VECTOR A
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      INTEGER JJ(LNV)
      save
      DATA JJ / LNV*0 /
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
*
      IF(INVA.EQ.0) THEN
         DO 10 I=0,ILLA-1
  10     CC(I+IPOB) = CC(I+IPOA) - CKON
         IDALL(INB) = ILLA
         IF(IDALL(INB).GT.IDALM(INB)) THEN
            PRINT*,'ERROR IN DACSU'
C           CALL DADEB(31,'ERR DACSU1',1)
         ENDIF
         RETURN
      ENDIF
*
      CALL DACOP(INA,INB)
*
      IF(NOMAX.EQ.1) THEN
         CC(IPOB) = CC(IPOB) - CKON
         RETURN
      ENDIF
*
      CALL DAPEK(INB,JJ,CONST)
      CALL DAPOK(INB,JJ,CONST-CKON)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DASUC(INA,CKON,INB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE SUBTRACTS THE VECTOR INA FROM THE CONSTANT CKON
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
*
      IF(INVA.EQ.0) THEN
         DO 10 I=0,ILLA-1
  10     CC(I+IPOB) = -CC(I+IPOA) + CKON
         IDALL(INB) = ILLA
         IF(IDALL(INB).GT.IDALM(INB)) THEN
            PRINT*,'ERROR IN DASUC'
C           CALL DADEB(31,'ERR DASUC1',1)
         ENDIF
         RETURN
      ENDIF
*
      CALL DACSU(INA,CKON,INB)
      CALL DACMU(INB,ONEM,INB)
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACMU(INA,CKON,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE MULTIPLIES THE DA VECTOR DENOTED BY THE
*     THE INTEGER A WITH THE CONSTANT C AND STORES THE RESULT IN
*     THE DA VECTOR DENOTED WITH THE INTEGER E.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      DIMENSION I_SPEC_DUMMY(1)
      save
*-----------------------------------------------------------------------------9
*
  
      IF(INA.EQ.INC) THEN
        CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
        INCC=0
        I_SPEC_DUMMY(1) = INCC
        CALL DAALL(I_SPEC_DUMMY,1,'$$DAJUNK$$',INOC,INVC)
        INCC = I_SPEC_DUMMY(1)
        CALL DACMUT(INA,CKON,INCC)
        CALL DACOP(INCC,INC)
        I_SPEC_DUMMY(1) = INCC
        CALL DADAL(I_SPEC_DUMMY,1)
        INCC = I_SPEC_DUMMY(1)
      ELSE
        CALL DACMUT(INA,CKON,INC)
      ENDIF
  
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACMUT(INA,CKON,INB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE MULTIPLIES THE DA VECTOR DENOTED BY THE
*     THE INTEGER A WITH THE CONSTANT C AND STORES THE RESULT IN
*     THE DA VECTOR DENOTED WITH THE INTEGER E.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
*
      IF(INVA.EQ.0) THEN
         DO 10 I=0,ILLA-1
  10     CC(I+IPOB) = CC(I+IPOA) * CKON
         IDALL(INB) = ILLA
         IF(IDALL(INB).GT.IDALM(INB)) THEN
            PRINT*,'ERROR IN DACAD'
C           CALL DADEB(31,'ERR DACAD1',1)
         ENDIF
         RETURN
      ENDIF
*
      CALL DACHK(INA,INOA,INVA,0,-1,-1,INB,INOB,INVB)
*
      IF(NOMAX.EQ.1) THEN
         MINV = MIN(INVA,INVB)
         DO 20 I=0,MINV
  20     CC(IPOB+I) = CC(IPOA+I) * CKON
         DO 30 I=IPOB+MINV+1,IPOB+INVB
  30     CC(I) = ZERO
         RETURN
      ENDIF
*
      IF(DABS(CKON).LT.EPS) THEN
         IDALL(INB) = 0
         RETURN
      ENDIF
*
      IB = IPOB - 1
*
      DO 100 IA=IPOA,IPOA+ILLA-1
*
      IF(IEO(IA1(I1(IA))+IA2(I2(IA))).GT.NOCUT) GOTO 100
      IB = IB + 1
      CC(IB) = CC(IA)*CKON
      I1(IB) = I1(IA)
      I2(IB) = I2(IA)
*
 100  CONTINUE
*
      IDALL(INB) = IB-IPOB+1
      IF(IDALL(INB).GT.IDALM(INB)) THEN
         PRINT*,'ERROR IN DACMU '
C        CALL DADEB(31,'ERR DACMU ',1)
      ENDIF
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACDI(INA,CKON,INB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE DIVIDES THE VECTOR INA BY THE CONSTANT CKON
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      IF(CKON.EQ.ZERO) THEN
         PRINT*,'ERROR IN DACDI, CKON IS ZERO'
C        CALL DADEB(31,'ERR DACDI1',1)
      ENDIF
*
      CALL DACMU(INA,ONE/CKON,INB)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DADIC(INA,CKON,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE DIVIDES THE CONSTANT CKON BY THE VECTOR INA
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      DIMENSION I_SPEC_DUMMY(1)
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      IF(ABS(CKON).LT.EPS) THEN
         CALL DACON(INC,ZERO)
         RETURN
      ENDIF
*
      IDADIC = 0
*
      I_SPEC_DUMMY(1) = IDADIC
      CALL DAALL(I_SPEC_DUMMY,1,'$$DADIC $$',INOC,INVC)
      IDADIC = I_SPEC_DUMMY(1)
      CALL DACDI(INA,CKON,IDADIC)
      CALL DAFUN('INV   ',IDADIC,INC)
      I_SPEC_DUMMY(1) = IDADIC
      CALL DADAL(I_SPEC_DUMMY,1)
      IDADIC = I_SPEC_DUMMY(1)
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACMA(INA,INB,BFAC,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **********************************
*
*     THIS SUBROUTINE PERFORMS THE OPERATIONS C = A + B*BFAC, WHERE A,B,C ARE
*     DA VECTORS AND BFAC IS A REAL*8. A AND C CAN BE IDENTICAL.
*     CAN LATER BE REPLACED BY SOMETHING LIKE DAADD WITH MINOR CHANGES.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      DIMENSION I_SPEC_DUMMY(1)
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
      IDACMA = 0
      I_SPEC_DUMMY(1) = IDACMA
      CALL DAALL(I_SPEC_DUMMY,1,'$$DACMA $$',INOC,INVC)
      IDACMA = I_SPEC_DUMMY(1)
      CALL DALIN(INA,ONE,INB,BFAC,IDACMA)
      CALL DACOP(IDACMA,INC)
      I_SPEC_DUMMY(1) = IDACMA
      CALL DADAL(I_SPEC_DUMMY,1)
      IDACMA = I_SPEC_DUMMY(1)
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAEXC(INA,CKON,INB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE EXPONENTIATES INE WITH THE CONSTANT CKON
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      INTEGER JJ(LNV)
      DIMENSION I_SPEC_DUMMY(1)
      save
      DATA JJ / LNV*0 /
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
*
      IDAEXC = 0
      I_SPEC_DUMMY(1) = IDAEXC
      CALL DAALL(I_SPEC_DUMMY,1,'$$DAEXC $$',INOB,INVB)
      IDAEXC = I_SPEC_DUMMY(1)
      CALL DAFUN('LOG   ',INA,INB)
      CALL DACMU(INB,CKON,IDAEXC)
      CALL DAFUN('EXP   ',IDAEXC,INB)
      I_SPEC_DUMMY(1) = IDAEXC
      CALL DADAL(I_SPEC_DUMMY,1)
      IDAEXC = I_SPEC_DUMMY(1)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACEX(INA,CKON,INB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE EXPONENTIATES THE CONSTANT CKON WITH INA
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      INTEGER JJ(LNV)
      DIMENSION I_SPEC_DUMMY(1)
      save
      DATA JJ / LNV*0 /
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
*
      IF(CKON.LE.0) THEN
         PRINT*,'ERROR IN DACEX, CKON NOT POSITIVE'
C        CALL DADEB(31,'ERR DACEX1',1)
      ENDIF
*
      IDACEX = 0
      I_SPEC_DUMMY(1) = IDACEX
      CALL DAALL(I_SPEC_DUMMY,1,'$$DACEX $$',INOB,INVB)
      IDACEX = I_SPEC_DUMMY(1)
      CKON = LOG(CKON)
      CALL DACMU(INA,CKON,IDACEX)
      CALL DAFUN('EXP   ',IDACEX,INB)
      I_SPEC_DUMMY(1) = IDACEX
      CALL DADAL(I_SPEC_DUMMY,1)
      IDACEX = I_SPEC_DUMMY(1)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAEXX(INA,INB,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE EXPONENTIATES INA WITH INB
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      INTEGER JJ(LNV)
      DIMENSION I_SPEC_DUMMY(1)
      save
      DATA JJ / LNV*0 /
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      IDAEXX = 0
      I_SPEC_DUMMY(1) = IDAEXX
      CALL DAALL(I_SPEC_DUMMY,1,'$$DAEXX $$',INOC,INVC)
      IDAEXX = I_SPEC_DUMMY(1)
      CALL DAFUN('LOG   ',INA,INC)
      CALL DAMUL(INB,INC,IDAEXX)
      CALL DAFUN('EXP   ',IDAEXX,INC)
      I_SPEC_DUMMY(1) = IDAEXX
      CALL DADAL(I_SPEC_DUMMY,1)
      IDAEXX = I_SPEC_DUMMY(1)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DALIN(INA,AFAC,INB,BFAC,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***************************************
*
*     THIS SUBROUTINE COMPUTES THE LINEAR COMBINATION
*     C = AFAC*A + BFAC*B. IT IS ALSO USED TO ADD AND SUBTRACT.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      DIMENSION I_SPEC_DUMMY(1)
      save
*-----------------------------------------------------------------------------9
*
  
      IF(INA.EQ.INC.OR.INB.EQ.INC) THEN
        CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
        INCC=0
        I_SPEC_DUMMY(1) = INCC
        CALL DAALL(I_SPEC_DUMMY,1,'$$DAJUNK$$',INOC,INVC)
        INCC = I_SPEC_DUMMY(1)
        CALL DALINT(INA,AFAC,INB,BFAC,INCC)
        CALL DACOP(INCC,INC)
        I_SPEC_DUMMY(1) = INCC
        CALL DADAL(I_SPEC_DUMMY,1)
        INCC = I_SPEC_DUMMY(1)
      ELSE
        CALL DALINT(INA,AFAC,INB,BFAC,INC)
      ENDIF
  
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DALINT(INA,AFAC,INB,BFAC,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***************************************
*
*     THIS SUBROUTINE COMPUTES THE LINEAR COMBINATION
*     C = AFAC*A + BFAC*B. IT IS ALSO USED TO ADD AND SUBTRACT.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      CALL DACHK(INA,INOA,INVA, INB,INOB,INVB, INC,INOC,INVC)
*
      IF(INVA+INVB+INVC.EQ.0) THEN
         DO 5 I=0,MIN(ILLA,ILLB)-1
         CC(IPOC+I) = CC(IPOA+I) * AFAC + CC(IPOB+I) * BFAC
 5       CONTINUE
         IDALL(INC) = MIN(ILLA,ILLB)
         IF(IDALL(INC).GT.IDALM(INC)) THEN
            PRINT*,'ERROR IN DALIN '
C           CALL DADEB(31,'ERR DALIN ',1)
         ENDIF
         RETURN
      ENDIF
*
      IF(NOMAX.EQ.1) THEN
         MINV = MIN(INVA,INVB,INVC)
         DO 7 I=0,MINV
 7       CC(IPOC+I) = CC(IPOA+I) * AFAC + CC(IPOB+I) * BFAC
         DO 8 I=IPOC+MINV+1,IPOC+INVC
 8       CC(I) = ZERO
         RETURN
      ENDIF
  
      IA = IPOA
      IB = IPOB
      IC = IPOC - 1
      IAMAX = IPOA+ILLA-1
      IBMAX = IPOB+ILLB-1
      ICMAX = IPOC+ILMC-1
      JA = IA1(I1(IA)) + IA2(I2(IA))
      JB = IA1(I1(IB)) + IA2(I2(IB))
*
      IF(IA.GT.IAMAX) THEN
         ISMIN = IB
         ISMAX = IBMAX
         COPF  = BFAC
         GOTO 50
      ENDIF
      IF(IB.GT.IBMAX) THEN
         ISMIN = IA
         ISMAX = IAMAX
         COPF  = AFAC
         GOTO 50
      ENDIF
*
*     COMPARING
*     *********
*
  10  CONTINUE
      IF(JA-JB) 30,20,40
*
*     ADDING TWO TERMS
*     ****************
*
  20  CONTINUE
      CCC = CC(IA)*AFAC + CC(IB)*BFAC
      IF(DABS(CCC).LT.EPS) GOTO 25
      IF(IEO(IA1(I1(IA))+IA2(I2(IA))).GT.NOCUT) GOTO 25
      IC = IC + 1
      CC(IC) = CCC
      I1(IC) = I1(IA)
      I2(IC) = I2(IA)
  25  CONTINUE
      IA = IA + 1
      IB = IB + 1
      IF(IA.GT.IAMAX) THEN
         ISMIN = IB
         ISMAX = IBMAX
         COPF  = BFAC
         GOTO 50
      ENDIF
      IF(IB.GT.IBMAX) THEN
         ISMIN = IA
         ISMAX = IAMAX
         COPF  = AFAC
         GOTO 50
      ENDIF
      JA = IA1(I1(IA)) + IA2(I2(IA))
      JB = IA1(I1(IB)) + IA2(I2(IB))
      GOTO 10
*
*     STORING TERM A
*     **************
*
  30  CONTINUE
      IF(IEO(IA1(I1(IA))+IA2(I2(IA))).GT.NOCUT) GOTO 35
      CCC = CC(IA)*AFAC
      IF(DABS(CCC).LT.EPS) GOTO 35
      IC = IC + 1
      CC(IC) = CCC
      I1(IC) = I1(IA)
      I2(IC) = I2(IA)
  35  CONTINUE
      IA = IA + 1
      IF(IA.GT.IAMAX) THEN
         ISMIN = IB
         ISMAX = IBMAX
         COPF  = BFAC
         GOTO 50
      ENDIF
      JA = IA1(I1(IA)) + IA2(I2(IA))
      GOTO 10
*
*     STORING TERM B
*     **************
*
  40  CONTINUE
      IF(IEO(IA1(I1(IB))+IA2(I2(IB))).GT.NOCUT) GOTO 45
      CCC = CC(IB)*BFAC
      IF(DABS(CCC).LT.EPS) GOTO 45
      IC = IC + 1
      CC(IC) = CCC
      I1(IC) = I1(IB)
      I2(IC) = I2(IB)
  45  CONTINUE
      IB = IB + 1
      IF(IB.GT.IBMAX) THEN
         ISMIN = IA
         ISMAX = IAMAX
         COPF  = AFAC
         GOTO 50
      ENDIF
      JB = IA1(I1(IB)) + IA2(I2(IB))
      GOTO 10
*
*     COPYING THE REST
*     ****************
*
  50  CONTINUE
      DO 60 IS=ISMIN,ISMAX
      IF(IEO(IA1(I1(IS))+IA2(I2(IS))).GT.NOCUT) GOTO 60
      CCC = CC(IS)*COPF
      IF(DABS(CCC).LT.EPS) GOTO 60
      IC = IC + 1
      CC(IC) = CCC
      I1(IC) = I1(IS)
      I2(IC) = I2(IS)
  60  CONTINUE
*
      IDALL(INC) = IC - IPOC + 1
*
      IF(IDALL(INC).GT.IDALM(INC)) THEN
         PRINT*,'ERROR IN DALIN, RESULT HAS TOO MANY TERMS '
C        CALL DADEB(31,'ERR DALIN ',1)
      ENDIF
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAFUN2(CF,INA,INB,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***********************************
*
*     THIS SUBROUTINE COMPUTES THE FUNCTION CF OF THE DA VECTORS A AND B
*     AND STORES THE RESULT IN C.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      CHARACTER CF*6,CFH*6,ABCS*30,ABCC*30
      DIMENSION JJ(LNV)
      save
*
      DATA JJ /LNV*0/
      DATA ABCS /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA ABCC /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
*
      IF(CF(1:1).EQ.' '.OR.CF(1:1).EQ.'D') THEN
         CFH(1:5) = CF(2:6)
         CFH(6:6) = ' '
         CF = CFH
      ENDIF
*
      DO 5 I=1,6
      IND = INDEX(ABCS,CF(I:I))
      IF(IND.NE.0) CF(I:I) = ABCC(IND:IND)
  5   CONTINUE
*
      IF((CF.EQ.'MAX   ').OR.(CF.EQ.'MAX0  ').OR.(CF.EQ.'AMAX1 ')
     * .OR.(CF.EQ.'MAX1  ')) THEN
         XA = DARE(INA)
         XB = DARE(INB)
         IF(ABS(XA-XB).LT.C1M12) THEN
            WRITE(*,1000) CF,INA,INB,XA,XB
            STOP
         ELSEIF(XA.GT.XB) THEN
            CALL DACOP(INA,INC)
         ELSE
            CALL DACOP(INB,INC)
         ENDIF
      ELSEIF((CF.EQ.'MIN   ').OR.(CF.EQ.'MIN0  ').OR.(CF.EQ.'AMIN1 ')
     * .OR.(CF.EQ.'MIN1  ')) THEN
         XA = DARE(INA)
         XB = DARE(INB)
         IF(ABS(XA-XB).LT.C1M12) THEN
            WRITE(*,1000) CF,INA,INB,XA,XB
            STOP
         ELSEIF(XA.LT.XB) THEN
            CALL DACOP(INA,INC)
         ELSE
            CALL DACOP(INB,INC)
         ENDIF
      ELSEIF((CF.EQ.'DIM ').OR.(CF.EQ.'IDIM ')) THEN
         XA = DARE(INA)
         XB = DARE(INB)
         IF(ABS(XA-XB).LT.C1M12) THEN
            WRITE(*,1000) CF,INA,INB,XA,XB
            STOP
         ELSEIF(XA.GT.XB) THEN
            CALL DASUB(INA,INB,INC)
         ELSE
            CALL DACON(INC,ZERO)
         ENDIF
      ELSEIF(CF.EQ.'PROD  ') THEN
         CALL DAMUL(INA,INB,INC)
      ELSEIF(CF.EQ.'ATAN2') THEN
         CALL DADIV(INA,INB,INC)
         CALL DAFUN('ATAN  ',INC,INC)
      ELSEIF((CF.EQ.'MOD   ').OR.(CF.EQ.'AMOD  ')) THEN
         CALL DADIV(INA,INB,INC)
         CALL DAFUN('INT   ',INC,INC)
         CALL DAMUL(INC,INB,INC)
         CALL DASUB(INA,INC,INC)
      ELSEIF((CF.EQ.'SIGN  ').OR.(CF.EQ.'ISIGN ')) THEN
         XA = DARE(INA)
         XB = DARE(INB)
         CALL DAFUN('ABS   ',INA,INC)
         IF(ABS(XB).LT.C1M12) THEN
            WRITE(*,1000) CF,INA,INB,XA,XB
            STOP
         ELSEIF(XB.LT.0) THEN
            CALL DACMU(INC,ONEM,INC)
         ENDIF
      ELSE
         WRITE(*,*) 'ERROR IN DAFUN2, UNKNOWN FUNCTION ',CF
      ENDIF
*
 1000 FORMAT('ERROR IN DAFUN, ',A6,' DOES NOT EXIST FOR VECTORS ',A10,
     *       A10, 'CONST TERMS  = ',2E12.5)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAFUN(MYCF,INA,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ****************************
*
*     THIS SUBROUTINE COMPUTES THE FUNCTION CF OF THE DA VECTOR A
*     AND STORES THE RESULT IN C.
*     AT PRESENT, SOME FUNCTIONS CAN BE COMPUTED ONLY TO FIFTH ORDER.
*     THIS HAS TO BE FIXED IN THE FUTURE.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      CHARACTER MYCF*6,CF*6,ABCS*26,ABCC*26
C     CHARACTER CFH*6
      DIMENSION JJ(LNV)
C     DIMENSION XF(0:LNO)
      DIMENSION I_SPEC_DUMMY(1)
      save
*
      DATA JJ /LNV*0/
      DATA ABCS /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA ABCC /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      CF=MYCF
      IF(INA.EQ.INC) THEN
       CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
        INCC=0
        I_SPEC_DUMMY(1) = INCC
       CALL DAALL(I_SPEC_DUMMY,1,'$$DAJUNK$$',INOC,INVC)
        INCC = I_SPEC_DUMMY(1)
       CALL DAFUNT(CF,INA,INCC)
       CALL DACOP(INCC,INC)
        I_SPEC_DUMMY(1) = INCC
       CALL DADAL(I_SPEC_DUMMY,1)
        INCC = I_SPEC_DUMMY(1)
      ELSE
       CALL DAFUNT(CF,INA,INC)
      ENDIF
  
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAFUNT(CF,INA,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ****************************
*
*     THIS SUBROUTINE COMPUTES THE FUNCTION CF OF THE DA VECTOR A
*     AND STORES THE RESULT IN C.
*     AT PRESENT, SOME FUNCTIONS CAN BE COMPUTED ONLY TO FIFTH ORDER.
*     THIS HAS TO BE FIXED IN THE FUTURE.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      CHARACTER CF*6,CFH*6,ABCS*30,ABCC*30
      DIMENSION XF(0:LNO),JJ(LNV)
      DIMENSION I_SPEC_DUMMY(1)
      save
*
      DATA JJ /LNV*0/
      DATA ABCS /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA ABCC /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
*
      IF(CF(1:1).EQ.' '.OR.CF(1:1).EQ.'D') THEN
         CFH(1:5) = CF(2:6)
         CFH(6:6) = ' '
         CF = CFH
      ENDIF
*
      DO 5 I=1,6
      IND = INDEX(ABCS,CF(I:I))
      IF(IND.NE.0) CF(I:I) = ABCC(IND:IND)
  5   CONTINUE
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
*     CASE OF NV = 0 WHICH MEANS COORDINATEWISE OPERATION
*     ***************************************************
*
      IF(INVA.EQ.0) THEN
            IF(CF.EQ.'INV   ') THEN
            DO 501 I=IPOA,IPOA+ILLA-1
  501         CC(I+IPOC-IPOA) = ONE/CC(I)
            RETURN
         ELSEIF(CF.EQ.'SQRT  ') THEN
            DO 502 I=IPOA,IPOA+ILLA-1
  502         CC(I+IPOC-IPOA) = SQRT(CC(I))
            RETURN
         ELSEIF(CF.EQ.'ISRT  ') THEN
            DO 503 I=IPOA,IPOA+ILLA-1
  503         CC(I+IPOC-IPOA) = ONE/SQRT(CC(I))
            RETURN
         ELSEIF(CF.EQ.'EXP   ') THEN
            DO 504 I=IPOA,IPOA+ILLA-1
  504         CC(I+IPOC-IPOA) = EXP(CC(I))
            RETURN
         ELSEIF(CF.EQ.'LOG   ') THEN
            DO 505 I=IPOA,IPOA+ILLA-1
  505         CC(I+IPOC-IPOA) = LOG(CC(I))
            RETURN
         ELSEIF(CF.EQ.'SIN   ') THEN
            DO 506 I=IPOA,IPOA+ILLA-1
  506         CC(I+IPOC-IPOA) = SIN(CC(I))
            RETURN
         ELSEIF(CF.EQ.'COS   ') THEN
            DO 507 I=IPOA,IPOA+ILLA-1
  507         CC(I+IPOC-IPOA) = COS(CC(I))
            RETURN
         ELSEIF(CF.EQ.'TAN   ') THEN
            DO 508 I=IPOA,IPOA+ILLA-1
  508         CC(I+IPOC-IPOA) = TAN(CC(I))
            RETURN
         ELSEIF(CF.EQ.'COT   ') THEN
            DO 509 I=IPOA,IPOA+ILLA-1
  509         CC(I+IPOC-IPOA) = COS(CC(I))/SIN(CC(I))
            RETURN
         ELSEIF(CF.EQ.'ASIN  ') THEN
            DO 510 I=IPOA,IPOA+ILLA-1
  510         CC(I+IPOC-IPOA) = ASIN(CC(I))
            RETURN
         ELSEIF(CF.EQ.'ACOS  ') THEN
            DO 511 I=IPOA,IPOA+ILLA-1
  511         CC(I+IPOC-IPOA) = ACOS(CC(I))
            RETURN
         ELSEIF(CF.EQ.'ATAN  ') THEN
            DO 512 I=IPOA,IPOA+ILLA-1
  512         CC(I+IPOC-IPOA) = ATAN(CC(I))
            RETURN
         ELSEIF(CF.EQ.'ACOT  ') THEN
            X1 = TWO*ATAN(ONE)
            DO 513 I=IPOA,IPOA+ILLA-1
  513         CC(I+IPOC-IPOA) = X1-ATAN(CC(I))
            RETURN
         ELSEIF(CF.EQ.'SINH  ') THEN
            DO 514 I=IPOA,IPOA+ILLA-1
  514         CC(I+IPOC-IPOA) = SINH(CC(I))
            RETURN
         ELSEIF(CF.EQ.'COSH  ') THEN
            DO 515 I=IPOA,IPOA+ILLA-1
  515         CC(I+IPOC-IPOA) = COSH(CC(I))
            RETURN
         ELSEIF(CF.EQ.'TANH  ') THEN
            DO 516 I=IPOA,IPOA+ILLA-1
  516         CC(I+IPOC-IPOA) = TANH(CC(I))
            RETURN
         ELSEIF(CF.EQ.'COTH  ') THEN
            DO 517 I=IPOA,IPOA+ILLA-1
  517         CC(I+IPOC-IPOA) = ONE/TANH(CC(I))
            RETURN
         ELSEIF(CF.EQ.'ASNH  ') THEN
            DO 518 I=IPOA,IPOA+ILLA-1
              AA = CC(I)
  518         CC(I+IPOC-IPOA) = LOG(AA+SQRT(AA*AA+ONE))
            RETURN
         ELSEIF(CF.EQ.'ACSH  ') THEN
            DO 519 I=IPOA,IPOA+ILLA-1
              AA = CC(I)
  519         CC(I+IPOC-IPOA) = LOG(AA+SQRT(AA*AA-ONE))
            RETURN
         ELSEIF(CF.EQ.'ATNH  ') THEN
            DO 520 I=IPOA,IPOA+ILLA-1
              AA = CC(I)
  520         CC(I+IPOC-IPOA) = HALF*LOG((1+AA)/(1-AA))
            RETURN
         ELSEIF(CF.EQ.'ACTH  ') THEN
            DO 521 I=IPOA,IPOA+ILLA-1
              AA = CC(I)
  521         CC(I+IPOC-IPOA) = HALF*LOG((AA+1)/(AA-1))
            RETURN
         ELSEIF(CF.EQ.'ABF   ') THEN
            DO 522 I=IPOA,IPOA+ILLA-1
  522         CC(I+IPOC-IPOA) = ONE/(ONE+EXP(CC(I)))
            RETURN
         ELSEIF(CF.EQ.'GAUS  ') THEN
            DO 523 I=IPOA,IPOA+ILLA-1
              AA = CC(I)
  523         CC(I+IPOC-IPOA) = EXP(-AA*AA)
            RETURN
         ELSEIF(CF.EQ.'ERF   ') THEN
            A1  = .254829592D0
            A2  = -.284496736D0
            A3  = 1.421413741D0
            A4  = -1.453152027D0
            A5  = 1.061405429D0
            P   = .3275911D0
            PI  = FOUR*ATAN(ONE)
            SPI = SQRT(PI)/TWO
            DO 524 I=IPOA,IPOA+ILLA-1
              AA  = CC(I)
              IF(ABS(AA).LT.ONE) THEN
                XN   = AA
                FAK  = ONE
                L    = 1
                X1   = L*XN/FAK
                DO 341 KI = 1,20
                  L   = L *(-1)
                  XN  = XN*AA*AA
                  FAK = FAK*KI
                  D   = 2*KI+1
                  X1  = X1 + L*XN/FAK/D
  341           CONTINUE
                E2  = X1 / SPI
              ELSE
                E1 = EXP(-AA*AA)
                T  = ONE/(1+P*AA)
                E2 = 1-T*(A1+T*(A2+T*(A3+T*(A4+T*A5))))*E1
              END IF
  524         CC(I+IPOC-IPOA) = E2
            RETURN
         ELSEIF(CF.EQ.'CORX  ') THEN
            DO 527 I=IPOA,IPOA+ILLA-1
              AA = CC(I)
              IF (AA.EQ.ZERO) THEN
                 CC(I)=ONE
              ELSE
                 AA=SQRT(AA)
                 CC(I) = COS(AA)
              ENDIF
  527       CONTINUE
            RETURN
         ELSEIF(CF.EQ.'SIDX  ') THEN
            DO 528 I=IPOA,IPOA+ILLA-1
              AA = CC(I)
              IF (AA.EQ.ZERO) THEN
                 CC(I)=ONE
              ELSE
                 CC(I) = SIN(AA)/AA
              ENDIF
  528       CONTINUE
            RETURN
         ELSEIF(CF.EQ.'SQR   ') THEN
            DO 529 I=IPOA,IPOA+ILLA-1
  529         CC(I+IPOC-IPOA) = CC(I)*CC(I)
            RETURN
         ELSEIF(CF.EQ.'LOG2  ') THEN
            DO 530 I=IPOA,IPOA+ILLA-1
  530         CC(I+IPOC-IPOA) = LOG(CC(I))/LOG(TWO)
            RETURN
         ELSEIF(CF.EQ.'LOG10 ') THEN
            DO 531 I=IPOA,IPOA+ILLA-1
  531         CC(I+IPOC-IPOA) = LOG(CC(I))/LOG(TEN)
            RETURN
*
         ELSE
            PRINT*,'ERROR, UNSUPPORTED FUNCTION ',CF
         ENDIF
      ENDIF
*
*     CASE OF NV > 0 WHICH MEANS DIFFERENTIAL ALGEBRAIC OPERATION
*     ***********************************************************
*
      IF(CF.EQ.'SQR   ') THEN
         CALL DASQR(INA,INC)
         RETURN
      ELSEIF(CF.EQ.'REAL  ') THEN
         CALL DACOP(INA,INC)
         RETURN
      ELSEIF(CF.EQ.'FLOAT') THEN
         CALL DACOP(INA,INC)
         RETURN
      ELSEIF(CF.EQ.'SNGL  ') THEN
         CALL DACOP(INA,INC)
         RETURN
      ELSEIF(CF.EQ.'DBLE  ') THEN
         CALL DACOP(INA,INC)
         RETURN
      ENDIF
*
*     ALLOCATE VARIABLES, PICK ZEROTH ORDER TERM
*     ******************************************
*
      IPOW = 0
      INON = 0
      ISCR = 0
*
      I_SPEC_DUMMY(1) = IPOW
      CALL DAALL(I_SPEC_DUMMY,1,'$$DAFUN1$$',INOC,INVC)
      IPOW = I_SPEC_DUMMY(1)
      I_SPEC_DUMMY(1) = INON
      CALL DAALL(I_SPEC_DUMMY,1,'$$DAFUN2$$',INOC,INVC)
      INON = I_SPEC_DUMMY(1)
      I_SPEC_DUMMY(1) = ISCR
      CALL DAALL(I_SPEC_DUMMY,1,'$$DAFUN3$$',INOC,INVC)
      ISCR = I_SPEC_DUMMY(1)
*
      CALL DACHK(INA,INOA,INVA,0,-1,-1,INC,INOC,INVC)
*
      CALL DAPEK(INA,JJ,A0)
*
      NO = MIN(NOCUT,INOA,INOC)
*
*     BRANCHING TO DIFFERENT FUNCTIONS
*     ********************************
*
      IF(CF.EQ.'INV   ') THEN
*        1/(A0+P) = 1/A0*(1-(P/A0)+(P/A0)**2-...)
         IF(A0.EQ.0) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUN1',1)
            LFUN = 0
            RETURN
         ENDIF
         XF(0) = ONE/A0
         DO 601 I=1,NO
  601    XF(I) = -XF(I-1)/A0
*
      ELSEIF(CF.EQ.'SQRT  ') THEN
*        SQRT(A0+P) = SQRT(A0)*(1+1/2(P/A0)-1/8*(P/A0)**2+...)
         IF(A0.LE.0) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUN2',1)
            LFUN = 0
            RETURN
         ENDIF
         RA = SQRT(A0)
         XF(0) = RA
         DO 602 I=1,NO
  602    XF(I) = -XF(I-1)/A0/DBLE(2*I)*(2*I-3)
*
      ELSEIF(CF.EQ.'ISRT  ') THEN
*        1/SQRT(A0+P) = 1/SQRT(A0)*(1-1/2(P/A0)+3/8*(P/A0)**2-...)
         IF(A0.LE.0) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUN3',1)
            LFUN = 0
            RETURN
         ENDIF
         ERA = ONE/SQRT(A0)
         XF(0) = ERA
         DO 603 I=1,NO
  603    XF(I) = -XF(I-1)/A0/DBLE(2*I)*(2*I-1)
*
      ELSEIF(CF.EQ.'EXP   ') THEN
*        EXP(A0+P) = EXP(A0)*(1+P+P**2/2!+...)
         EA  = EXP(A0)
         XF(0) = EA
         DO 604 I=1,NO
  604    XF(I) = XF(I-1)/DBLE(I)
*
      ELSEIF((CF.EQ.'LOG   ').OR.(CF.EQ.'ALOG  ')) THEN
*        LOG(A0+P) = LOG(A0) + (P/A0) - 1/2*(P/A0)**2 + 1/3*(P/A0)**3 - ...)
         IF(A0.LE.0) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUN4',1)
            LFUN = 0
            RETURN
         ENDIF
         EA  = LOG(A0)
         XF(0) = EA
         XF(1) = ONE/A0
         DO 605 I=2,NO
  605    XF(I) = -XF(I-1)/A0/DBLE(I)*(I-1)
*
      ELSEIF(CF.EQ.'SIN   ') THEN
*        SIN(A0+P) = SIN(A0)*(1-P**2/2!+P**4/4!) + COS(A0)*(P-P**3/3!+P**5/5!)
         SA  = SIN(A0)
         CA  = COS(A0)
         XF(0) = SA
         XF(1) = CA
         DO 606 I=2,NO
  606    XF(I) = -XF(I-2)/DBLE(I*(I-1))
*
      ELSEIF(CF.EQ.'COS   ') THEN
*        COS(A0+P) = COS(A0)*(1-P**2/2!+P**4/4!) - SIN(A0)*(P-P**3/3!+P**5/5!)
         SA  = SIN(A0)
         CA  = COS(A0)
         XF(0) = CA
         XF(1) = -SA
         DO 607 I=2,NO
  607    XF(I) = -XF(I-2)/DBLE(I*(I-1))
*
      ELSEIF(CF.EQ.'SIRX  ') THEN
*        SIN(SQRT(P))/SQRT(P) = 1 - P/3! + P**2/5! - P**3/7! + ...
         IF(A0.NE.0) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUN5',1)
            LFUN = 0
            RETURN
         ENDIF
         XF(0)=ONE
         DO 608 I=1,NO
  608    XF(I) = -XF(I-1)/DBLE(2*I*(2*I+1))
*
      ELSEIF(CF.EQ.'CORX  ') THEN
*        COS(SQRT(P)) = 1 - P/2! + P**2/4! - P**3/6! + ...
         IF(A0.NE.0) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUN6',1)
            LFUN = 0
            RETURN
         ENDIF
         XF(0)=ONE
         DO 609 I=1,NO
  609    XF(I) = -XF(I-1)/DBLE(2*I*(2*I-1))
*
      ELSEIF(CF.EQ.'SIDX  ') THEN
*        SIN(P)/P = 1 - P**2/3! + P**4/5! - P**6/7! + ...
         IF(A0.NE.0) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUN7',1)
            LFUN = 0
            RETURN
         ENDIF
         XF(0)=ONE
         XF(1)=ZERO
         DO 610 I=2,NO
  610    XF(I) = -XF(I-2)/DBLE(I*(I+1))
*
      ELSEIF(CF.EQ.'TAN   ') THEN
         IF(ABS(COS(A0)).LT.EPSMAC) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUN8',1)
            LFUN = 0
            RETURN
         ENDIF
         SA  = SIN(A0)
         CA  = COS(A0)
         XF(0) = SA/CA
         XF(1) = ONE/CA/CA
         XF(2) = TWO*SA/CA/CA/CA/TWO
         XF(3) = (TWO*CA*CA+SIX*SA*SA)/CA/CA/CA/CA/SIX
         XF(4) = (16*SA+EIGHT*SA*SA*SA)/CA/CA/CA/CA/CA/C24E0
         XF(5) = (C16E0*CA*CA+C24E0*CA*CA*SA*SA+C80E0*SA*SA+
     *          C40E0*SA*SA*SA*SA)/CA/CA/CA/CA/CA/CA/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUN9',1)
            STOP
         ENDIF
      ELSEIF(CF.EQ.'COT   ') THEN
         IF(ABS(SIN(A0)).LT.EPSMAC) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUN0',1)
            LFUN = 0
            RETURN
         ENDIF
         SA  = SIN(A0)
         CA  = COS(A0)
         XF(0) = CA/SA
         XF(1) = -ONE/SA/SA
         XF(2) = TWO*CA/SA/SA/SA/TWO
         XF(3) = -(TWO*SA*SA+SIX*CA*CA)/SA/SA/SA/SA/SIX
         XF(4) = (16*CA+EIGHT*CA*CA*CA)/SA/SA/SA/SA/SA/C24E0
         XF(5) = -(C16E0*SA*SA+C24E0*SA*SA*CA*CA+C80E0*CA*CA+
     *           C40E0*CA*CA*CA*CA)/SA/SA/SA/SA/SA/SA/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNA',1)
            STOP
         ENDIF
      ELSEIF(CF.EQ.'ASIN  ') THEN
         IF((ONE-ABS(A0)).LE.ZERO) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNB',1)
            LFUN = 0
            RETURN
         ENDIF
         XF(0) = ASIN(A0)
         XF(1) = (ONE-A0*A0)**(-HALF)
         XF(2) = A0*XF(1)**THREE/TWO
         XF(3) = (1+TWO*A0*A0)*XF(1)**FIVE/SIX
         XF(4) = (NINE*A0+SIX*A0*A0*A0)*XF(1)**SEVEN/C24E0
         XF(5) = (NINE+C72E0*A0*A0+C24E0*A0*A0*A0*A0)*XF(1)**NINE/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
            STOP
         ENDIF
      ELSEIF(CF.EQ.'ACOS  ')THEN
         IF((ONE-ABS(A0)).LE.ZERO) THEN
C           CALL DADEB(31,'ERR DAFUNC',1)
            WRITE(*,1000) CF,INA,A0
            LFUN = 0
            RETURN
         ENDIF
         XF(0) =  ACOS(A0)
         SCR =  (ONE-A0*A0)**(-HALF)
         XF(1) =  -SCR
         XF(2) = -A0*SCR**THREE/TWO
         XF(3) = -(1+TWO*A0*A0)*SCR**FIVE/SIX
         XF(4) = -(NINE*A0+SIX*A0*A0*A0)*SCR**SEVEN/C24E0
         XF(5) = -(NINE+C72E0*A0*A0+C24E0*A0*A0*A0*A0)*SCR**NINE/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUND',1)
         ENDIF
      ELSEIF(CF.EQ.'ATAN  ') THEN
*        ATAN(A0+P) = ATAN(A0)+1/(1+A0**2)*P-A0/(1+A0**2)**2*P**2+....)
         XF(0) = ATAN(A0)
         XF(1) = ONE/(ONE+A0*A0)
         XF(2) = -A0*(XF(1)*XF(1))
         XF(3) = (A0*A0-ONE/THREE)*XF(1)**3
         XF(4) = (A0-A0*A0*A0)*XF(1)**4
         XF(5) = (ONE/FIVE+A0**4-TWO*A0*A0)*XF(1)**5
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNE',1)
         ENDIF
      ELSEIF(CF.EQ.'ACOT  ') THEN
         XF(0) = TWO*ATAN(ONE)-ATAN(A0)
         SCR = ONE/(ONE+A0*A0)
         XF(1) = -SCR
         XF(2) = A0*(SCR*SCR)
         XF(3) = -(A0*A0-ONE/THREE)*SCR**3
         XF(4) = -(A0-A0*A0*A0)*SCR**4
         XF(5) = -(ONE/FIVE+A0**4-TWO*A0*A0)*SCR**5
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNF',1)
         ENDIF
      ELSEIF(CF.EQ.'SINH  ') THEN
         SA  = SINH(A0)
         CA  = COSH(A0)
         XF(0) = SA
         XF(1) = CA
         XF(2) = SA/TWO
         XF(3) = CA/SIX
         XF(4) = SA/C24E0
         XF(5) = CA/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNG',1)
         ENDIF
      ELSEIF(CF.EQ.'COSH  ') THEN
         SA  = SINH(A0)
         CA  = COSH(A0)
         XF(0) = CA
         XF(1) = SA
         XF(2) = CA/TWO
         XF(3) = SA/SIX
         XF(4) = CA/C24E0
         XF(5) = SA/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNH',1)
         ENDIF
      ELSEIF(CF.EQ.'TANH  ') THEN
         SA  = SINH(A0)
         CA  = COSH(A0)
         XF(0) = SA/CA
         XF(1) = ONE/CA/CA
         XF(2) = -TWO*SA/CA/CA/CA/TWO
         XF(3) = (-TWO*CA*CA+SIX*SA*SA)/CA/CA/CA/CA/SIX
         XF(4) = (C16E0*SA-EIGHT*SA*SA*SA)/CA/CA/CA/CA/CA/C24E0
         XF(5) = (C16E0*CA*CA-C24E0*CA*CA*SA*SA-C80E0*SA*SA+
     *          C40E0*SA*SA*SA*SA)/CA/CA/CA/CA/CA/CA/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNI',1)
         ENDIF
      ELSEIF(CF.EQ.'COTH  ') THEN
         IF(ABS(SINH(A0)).LT.EPSMAC) THEN
            LFUN = 0
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNJ',1)
            RETURN
         ENDIF
         SA  = SINH(A0)
         CA  = COSH(A0)
         XF(0) = CA/SA
         XF(1) = -ONE/SA/SA
         XF(2) =  TWO*CA/SA/SA/SA/TWO
         XF(3) = (TWO*SA*SA-SIX*CA*CA)/SA/SA/SA/SA/SIX
         XF(4) = (16*CA+EIGHT*CA*CA*CA)/SA/SA/SA/SA/SA/C24E0
         XF(5) = (C16E0*SA*SA+C24E0*SA*SA*CA*CA-C80E0*CA*CA-
     *          C40E0*CA*CA*CA*CA)/SA/SA/SA/SA/SA/SA/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNK',1)
         ENDIF
      ELSEIF(CF.EQ.'ASNH  ') THEN
         XF(0) = LOG(A0+SQRT(A0*A0+ONE))
         XF(1) = (ONE+A0*A0)**(-HALF)
         XF(2) = -A0*XF(1)**THREE/TWO
         XF(3) = (TWO*A0*A0-ONE)*XF(1)**FIVE/SIX
         XF(4) = (NINE*A0-SIX*A0*A0*A0)*XF(1)**SEVEN/C24E0
         XF(5) = (NINE-C72E0*A0*A0+C24E0*A0*A0*A0*A0)*XF(1)**NINE/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNL',1)
         ENDIF
      ELSEIF(CF.EQ.'ACSH  ') THEN
         IF((ONE-A0).GE.ZERO) THEN
            LFUN = 0
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNM',1)
            RETURN
         ENDIF
         XF(0) = LOG(A0+SQRT(A0*A0-ONE))
         XF(1) = (A0*A0-ONE)**(-HALF)
         XF(2) = -A0*XF(1)**THREE/TWO
         XF(3) = (TWO*A0*A0+ONE)*XF(1)**FIVE/SIX
         XF(4) = (-NINE*A0-SIX*A0*A0*A0)*XF(1)**SEVEN/C24E0
         XF(5) = (NINE+C72E0*A0*A0+C24E0*A0*A0*A0*A0)*XF(1)**NINE/C120E0
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNN',1)
         ENDIF
      ELSEIF(CF.EQ.'ATNH  ') THEN
         IF((ABS(A0)-ONE).GE.ZERO) THEN
            LFUN = 0
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNO',1)
            RETURN
         ENDIF
         XF(0) =  HALF*LOG((1+A0)/(1-A0))
         XF(1) =  ONE/(ONE-A0*A0)
         XF(2) =  A0*(XF(1)*XF(1))
         XF(3) = (A0*A0+ONE/THREE)*XF(1)**3
         XF(4) = (A0+A0*A0*A0)*XF(1)**4
         XF(5) = (ONE/FIVE+A0**4+TWO*A0*A0)*XF(1)**5
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNP',1)
         ENDIF
      ELSEIF(CF.EQ.'ACTH  ') THEN
         IF(ONE-ABS(A0).GE.ZERO) THEN
            LFUN = 0
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNQ',1)
            RETURN
         ENDIF
         XF(0) =  HALF*LOG((A0+1)/(A0-1))
         SCR =  ONE/(-ONE+A0*A0)
         XF(1) = -SCR
         XF(2) =  A0*(SCR*SCR)
         XF(3) = (-A0*A0-ONE/THREE)*SCR**THREE
         XF(4) = (A0+A0*A0*A0)*SCR**FOUR
         XF(5) = (-ONE/FIVE-A0**4-TWO*A0*A0)*SCR**FIVE
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNR',1)
         ENDIF
      ELSEIF(CF.EQ.'ABF   ') THEN
*
*     DIESE FUNKTION BESCHREIBT DEN FELDABFALL BEI IONENOPTISCHEN ELEMENTEN
*     ABF=1/(1+EXP(A0+X))
*        =1/(1+EXP(A0)*(1-EXP(A0)/(1+EXP(A0))*X+....)
         XF(0) = ONE/(1+EXP(A0))
         E1  = EXP(A0)*X1
         E2  = E1 * E1
         E3  = E2 * E1
         E4  = E3 * E1
         E5  = E4 * E1
         XF(1) = X1*(-E1)
         XF(2) = X1*(-HALF* E1 + E2)
         XF(3) = X1*(-E1/SIX + E2 - E3)
         XF(4) = X1*(-E1/C24E0 + E2*SEVEN/C12E0 - E3*THREE/TWO + E4)
         XF(5) = X1*(-E1/C120E0 + E2/FOUR - E3*FIVE/FOUR +
     *         E4*TWO - E5)
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNS',1)
         ENDIF
      ELSEIF(CF.EQ.'GAUS') THEN
*
*     DIESE FUNKTION BESCHREIBT DIE ENTWICKLUNG VON EXP(-X*X)
*
         XF(0) = EXP(-A0*A0)
         XF(1) = -TWO*A0*X1
         XF(2) = (-ONE+TWO*A0*A0)*X1
         XF(3) = (C12E0*A0-EIGHT*A0*A0*A0)/SIX*X1
         XF(4) = (C16E0*A0*A0*A0*A0-C48E0*A0*A0+C12E0)/C24E0*X1
         XF(5) = (-C32E0*A0*A0*A0*A0*A0+C160E0*A0*A0*A0-C120E0*A0)/
     *           C120E0*X1
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNT',1)
         ENDIF
      ELSEIF(CF.EQ.'ERF   ') THEN
*
*    ERF(X) STELLT DAS INTEGRAL VON 0 BIS X VON [ 2/SQRT(PI) * EXP(-X*X) ]
*    DAR
*
         RPI4 = SQRT(ATAN(ONE))
         E1   = EXP(-A0*A0)/RPI4
         IF(A0.LT.ONE) THEN
           XN   = A0
           FAK  = ONE
           L    = 1
           X1   = L*XN/FAK
           DO 342 I = 1,20
             L    = L *(-1)
             XN   = XN*A0*A0
             FAK  = FAK*I
             D    = 2*I+1
             X1  = X1 + L*XN/FAK/D
  342      CONTINUE
           XF(0) = X1 / RPI4
         ELSE
           A1    = .254829592D0
           A2    = -.284496736D0
           A3    = 1.421413741D0
           A4    = -1.453152027D0
           A5    = 1.061405429D0
           P     = .3275911
           T     = ONE/(1+P*A0)
           E2    = 1-T*(A1+T*(A2+T*(A3+T*(A4+T*A5))))*E1*RPI4
           XF(0) = E2
         END IF
         XF(1) = E1
         XF(2) = -A0*E1
         XF(3) = (-TWO+FOUR*A0*A0)/SIX*E1
         XF(4) = (C12E0*A0-EIGHT*A0*A0*A0)/C24E0*E1
         XF(5) = (C16E0*A0*A0*A0*A0-C48E0*A0*A0+C12E0)/C120E0*E1
         IF(NO.GT.5) THEN
            PRINT*,'ERROR IN DAFUN, ',CF, ' ONLY UP TO NO = 5'
C           CALL DADEB(31,'ERR DAFUNU',1)
         ENDIF
      ELSEIF(CF.EQ.'LOG2  ') THEN
*        LOG(A0+P) = LOG(A0) + (P/A0) - 1/2*(P/A0)**2 + 1/3*(P/A0)**3 - ...)
         IF(A0.LE.0) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNV',1)
            LFUN = 0
            RETURN
         ENDIF
         EA  = LOG(A0)
         XF(0) = EA/LOG(TWO)
         XF(1) = ONE/A0/LOG(TWO)
         DO 343 I=2,NO
  343    XF(I) = -XF(I-1)/A0/DBLE(I)*(I-1)
      ELSEIF((CF.EQ.'LOG10 ').OR.(CF.EQ.'ALOG10')) THEN
*        LOG(A0+P) = LOG(A0) + (P/A0) - 1/2*(P/A0)**2 + 1/3*(P/A0)**3 - ...)
         IF(A0.LE.0) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNW',1)
            LFUN = 0
            RETURN
         ENDIF
         EA  = LOG(A0)
         XF(0) = EA/LOG(TEN)
         XF(1) = ONE/A0/LOG(TEN)
         DO 344 I=2,NO
  344    XF(I) = -XF(I-1)/A0/DBLE(I)*(I-1)
      ELSEIF((CF.EQ.'INT   ').OR.(CF.EQ.'IFIX  ').OR.(CF.EQ.'IDINT ')
     *   .OR.(CF.EQ.'AINT  ')) THEN
         AA0 = AINT(A0)
         IF(ABS(NINT(A0)-A0).LT.C1M12) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNX',1)
            LFUN = 0
            RETURN
         ENDIF
         XF(0) = AA0
         DO 345 I=1,NO
  345    XF(I) = ZERO
      ELSEIF((CF.EQ.'NINT  ').OR.(CF.EQ.'IDNINT')
     *   .OR.(CF.EQ.'ANINT ')) THEN
         AA0 = ANINT(A0)
         IF(ABS(ABS(NINT(A0)-A0)-HALF).LT.C1M12) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNY',1)
            LFUN = 0
            RETURN
         ENDIF
         XF(0) = AA0
         DO 346 I=1,NO
  346    XF(I) = ZERO
      ELSEIF((CF.EQ.'ABS   ').OR.(CF.EQ.'IABS  ')) THEN
         AA0 = ABS(A0)
         IF(ABS(A0).LT.C1M12) THEN
            WRITE(*,1000) CF,INA,A0
C           CALL DADEB(31,'ERR DAFUNZ',1)
            LFUN = 0
            RETURN
         ENDIF
         XF(0) = AA0
         IF(A0.GT.ZERO) THEN
            XF(1) = ONE
         ELSE
            XF(1) = -ONE
         ENDIF
         DO 347 I=2,NO
  347    XF(I) = ZERO
      ELSE
         PRINT*,'ERROR, UNSUPPORTED FUNCTION ',CF
      ENDIF
*
      CALL DACOP(INA,INON)
      CALL DACON(INC,XF(0))
      CALL DAPOK(INON,JJ,ZERO)
      CALL DACON(IPOW,ONE)
*
      DO 800 I=1,MIN(NO,NOCUT)
*
      CALL DAMUL(INON,IPOW,ISCR)
      CALL DACOP(ISCR,IPOW)
      CALL DACMA(INC,IPOW,XF(I),INC)
*
 800  CONTINUE
*
 1000 FORMAT('ERROR IN DAFUN, ',A4,' DOES NOT EXIST FOR VECTOR ',A10,
     *       'CONST TERM  = ',E12.5)
*
      I_SPEC_DUMMY(1) = ISCR
      CALL DADAL(I_SPEC_DUMMY,1)
      ISCR = I_SPEC_DUMMY(1)
      I_SPEC_DUMMY(1) = INON
      CALL DADAL(I_SPEC_DUMMY,1)
      INON = I_SPEC_DUMMY(1)
      I_SPEC_DUMMY(1) = IPOW
      CALL DADAL(I_SPEC_DUMMY,1)
      IPOW = I_SPEC_DUMMY(1)
*
      RETURN
      END
*FF
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAABS(INA,ANORM)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***************************
*
*     THIS SUBROUTINE COMPUTES THE NORM OF THE DA VECTOR A
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
*
      ANORM = ZERO
      DO 100 I=IPOA,IPOA+ILLA-1
      ANORM = ANORM + ABS(CC(I))
 100  CONTINUE
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACOM(INA,INB,DNORM)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
      DIMENSION I_SPEC_DUMMY(1)
      save
*     *******************************
*
*     THIS SUBROUTINE COMPARES TWO DA VECTORS BY RETURNING THE NORM
*     OF THE DIFFERENCE
*FRS
*FRS
      IDACOM = 0
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
      I_SPEC_DUMMY(1) = IDACOM
      CALL DAALL(I_SPEC_DUMMY,1,'$$DACOM $$',INOC,INVC)
      IDACOM = I_SPEC_DUMMY(1)
      CALL DASUB(INA,INB,IDACOM)
      CALL DAABS(IDACOM,DNORM)
      I_SPEC_DUMMY(1) = IDACOM
      CALL DADAL(I_SPEC_DUMMY,1)
      IDACOM = I_SPEC_DUMMY(1)
*
      RETURN
      END
*FF
  
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPOS(INA,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *************************
*
*     THIS SUBROUTINE MAKES THE SIGNS OF ALL THE COEFFICIENTS OF A POSITIVE
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INB,INOB,INVB,IPOB,ILMB,ILLB)
*
      IF(INVA.EQ.0) THEN
         DO 10 I=0,ILLA-1
  10     CC(IPOB+I) = ABS(CC(IPOA+I))
         IDALL(INB) = IDALL(INA)
         IF(IDALL(INB).GT.IDALM(INB)) THEN
            PRINT*,'ERROR IN DAPOS'
C           CALL DADEB(31,'ERR DAPOS1',1)
         ENDIF
      ENDIF
*
      CALL DACHK(INA,INOA,INVA,0,-1,-1,INB,INOB,INVB)
*
      IB = IPOB - 1
*
      DO 100 IA = IPOA,IPOA+ILLA-1
*
      IF(IEO(IA1(I1(IA))+IA2(I2(IA))).GT.NOCUT) GOTO 100
      IB     = IB + 1
      CC(IB) = ABS(CC(IA))
      I1(IB) = I1(IA)
      I2(IB) = I2(IA)
*
 100  CONTINUE
*
      IDALL(INB) = IB - IPOB + 1
      IF(IDALL(INB).GT.IDALM(INB)) THEN
         PRINT*,'ERROR IN DAPOS '
C        CALL DADEB(31,'ERR DAPOS2',1)
      ENDIF
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACCT(MA,IA,MB,IB,MC,IC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***********************************
*
*     THIS SUBROUTINE PERFORMS A CONCATENATION MA = MB O MC
*     WHERE MA, MB AND MC ARE MATRICES CONSISTING OF IA, IB AND IC
*     DA VECTORS, RESPECTIVELY.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      INTEGER MON(LNV),MB(*),MC(*),MA(*)
C     INTEGER JJ(LNV),ICC(LNO)
      save
  
  
      IF(MA(1).EQ.MC(1).OR.MB(1).EQ.MC(1)) THEN
        CALL DAINF(MC(1),INOC,INVC,IPOC,ILMC,ILLC)
        DO 101 IJ=1,IC
 101    MON(IJ)=0
        CALL DAALL(MON,IC,'$$DAJUNK$$',INOC,INVC)
        CALL DACCTT(MA,IA,MB,IB,MON,IC)
        DO 9 I=1,IC
 9      CALL DACOP(MON(I),MC(I))
        CALL DADAL(MON,IC)
      ELSE
        CALL DACCTT(MA,IA,MB,IB,MC,IC)
      ENDIF
  
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACCTT(MB,IB,MC,IC,MA,IA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***********************************
*
*     THIS SUBROUTINE PERFORMS A CONCATENATION MA = MB O MC
*     WHERE MA, MB AND MC ARE MATRICES CONSISTING OF IA, IB AND IC
*     DA VECTORS, RESPECTIVELY.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
C      INTEGER JJ(LNV),MON(LNO+1),ICC(LNV),MB(*),MC(*),MA(*)
CETIENNE
      INTEGER MON(LNO+1),ICC(LNO),MB(*),MC(*),MA(*)
C     INTEGER JJ(LNV)
CETIENNE
      save
*
*     CONSISTENCY CHECKS
*     ******************
*
      IIA = MA(1)
      IIB = MB(1)
      IIC = MC(1)
      CALL DAINF(IIA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(IIB,INOB,INVB,IPOB,ILMB,ILLB)
      CALL DAINF(IIC,INOC,INVC,IPOC,ILMC,ILLC)
*
      CALL DAMCH(MA,IA)
      CALL DAMCH(MB,IB)
*
      IF(IA.NE.IB) THEN
         PRINT*,'ERROR IN DACCT, IA .NE. IB'
C        CALL DADEB(31,'ERR DACCT1',1)
      ELSEIF(IC.NE.INVB) THEN
         PRINT*,'ERROR IN DACCT, IC.NE.INVB'
C        CALL DADEB(31,'ERR DACCT2',1)
      ENDIF
*
*     ALLOCATING LOCAL VECTORS AND CALLING MTREE
*     ******************************************
*
      DO 50 I=1,IB
  50  ICC(I) = 0
*
      DO 60 I=1,NOMAX+1
  60  MON(I) = 0
*
      CALL DAALL(ICC,IB,'$$DACCT $$',NOMAX,NVMAX)
      CALL DAALL(MON,NOMAX+1,'$$DAMON $$',INOC,INVC)
*
      CALL MTREE(MB,IB,ICC,IB)
*
*     PERFORMING CONCATENATION
*     ************************
*
      DO 80 I=1,IA
      CALL DACON(MA(I),CC(IDAPO(ICC(I))))
  80  CONTINUE
*
      CALL DACON(MON(1),1.D0)
*
      DO 100 I=1,IDALL(ICC(1))-1
*
      JL = I1(IDAPO(ICC(1))+I)
      JV = I2(IDAPO(ICC(1))+I)
*
      CALL DAMUL(MON(JL),MC(JV),MON(JL+1))
*
      DO 100 IV=1,IA
*
      CCF = CC(IDAPO(ICC(IV))+I)
      IF(DABS(CCF).GT.EPS) CALL DACMA(MA(IV),MON(JL+1),CCF,MA(IV))
*
 100  CONTINUE
*
      CALL DADAL(MON,NOMAX+1)
      CALL DADAL(ICC,IB)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DATRK(MB,IB,MC,IC,N, MA,IA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **************************************
*
*     THIS SUBROUTINE TRACKS THE COORDINATES IN CA VECTOR MC |N| TIMES
*     THROUGH THE MATRIX MB. THE RESULTING COORDINATES ARE STORED IN CA
*     VECTOR MA.
*     IF N IS NEGATIVE, SYMPLECTIC TRACKING WILL BE PERFORMED *** THIS OPTION
*     DOES NOT EXIST YET.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION MB(*),MC(*),MBB(LNV),MA(*),XI(LNV),XF(LNV)
      DIMENSION II_SPEC_DUMMY(1)
      DIMENSION I_SPEC_DUMMY(1)
      save
*
      IF(IDALM(MC(1))*ABS(N).GT.IDALM(MA(1))) THEN
         PRINT*,'ERROR IN ROUTINE DATRK, IA.LT.IC*|N|'
C        CALL DADEB(31,'ERR DATRK1',1)
      ENDIF
*
      CALL DAALL(MBB,IB,'$$DATRK $$',NOMAX,NVMAX)
*
      CALL DATREE(MB,IB,MBB,IB)
*
      DO 100 I=1,IDALL(MC(1))
*
      II_SPEC_DUMMY(1)=(I-1)*N+1
      DO 50 J=1,IC
      I_SPEC_DUMMY(1)= I
      CALL DAPEK(MC(J), I_SPEC_DUMMY,       XI(J))
      CALL DAPOK(MA(J),II_SPEC_DUMMY,XI(J))
  50  CONTINUE
  
      DO 100 K=2,ABS(N)
*
      CALL DAPUSH(MBB,IB,XI,XF)
*
      IF(N.LT.0) THEN
      ENDIF
*
      II_SPEC_DUMMY(1)=(I-1)*N+K
      DO 60 J=1,IC
      XI(J) = XF(J)
      CALL DAPOK(MA(J),II_SPEC_DUMMY,XI(J))
  60  CONTINUE
*
 100  CONTINUE
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DATREE(MB,IB,MC,IC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE IS USED FOR CONCATENATION AND TRACKING OF VECTORS
*     THROUGH A DA MAP. IT COMPUTES THE TREE THAT HAS TO BE TRANSVERSED
*     MB IS THE DA MATRIX WITH IA TERMS. THE OUTPUT MC IS A CA MATRIX WHICH
*     CONTAINS COEFFICIENTS AND CONTROL INTEGERS USED FOR THE TRAVERSAL.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      INTEGER JJ(LNV),JV(0:LNO),MB(*),MC(*)
      DIMENSION I_SPEC_DUMMY(1)
      save
*
*     CONSISTENCY CHECKS
*     ******************
*
      IIB = MB(1)
      IIC = MC(1)
      CALL DAINF(IIB,INOB,INVB,IPOB,ILMB,ILLB)
      CALL DAINF(IIC,INOC,INVC,IPOC,ILMC,ILLC)
*
      CALL DAMCH(MB,IB)
      CALL DAMCH(MC,IC)
*
      IF(IB.NE.IC) THEN
         PRINT*,'ERROR IN DATREE, IB .NE. IC'
C        CALL DADEB(31,'ERR DATRE1',1)
      ENDIF
*
*     ALLOCATING LOCAL VECTORS
*     ************************
*
      ICHK = 0
      I_SPEC_DUMMY(1) = ICHK
      CALL DAALL(I_SPEC_DUMMY,1,'$$DATREE$$',NOMAX,NVMAX)
      ICHK = I_SPEC_DUMMY(1)
*
*     FIND ALL THE ENTRIES TO BE LOOKED FOR
*     *************************************
*
      CALL DACLR(1)
*
      CC(1) = ONE
*
      DO 100 I=1,IB
      DO  90 IBI = IDAPO(MB(I)),IDAPO(MB(I))+IDALL(MB(I))-1
      ICC = IA1(I1(IBI)) + IA2(I2(IBI))
      IF(IEO(ICC).GT.INOB) GOTO 90
      CC(ICC) = ONE
   90 CONTINUE
  100 CONTINUE
*
      DO 150 II=1,INOB
*
*     SEARCHING FOR FATHER FOR EACH TERM
*
      DO 140 I=1,NMMAX
      IF(CC(I).LT.HALF) GOTO 140
*
      JNON = 0
      CALL DANCD(I1(I),I2(I),JJ)
      DO 130 J=1,INVB
      IF(JJ(J).EQ.0) GOTO 130
      JNON = J
      JJ(J) = JJ(J) - 1
      CALL DADCD(JJ,IC1,IC2)
      APEK = CC(IA1(IC1)+IA2(IC2))
      JJ(J) = JJ(J) + 1
      IF(APEK.GE.HALF) GOTO 140
  130 CONTINUE
*
      IF(JNON.EQ.0) GOTO 140
*
*     TERM IS AN ORPHAN, SO CREATE FOSTER FATHER
*
      JJ(JNON) = JJ(JNON) - 1
      CALL DADCD(JJ,IC1,IC2)
      CC(IA1(IC1)+IA2(IC2)) = ONE
*
  140 CONTINUE
  150 CONTINUE
*
      CALL DAPAC(ICHK,0)
*
*     SETTING UP TREE STRUCTURE
*     *************************
*
      NTERMF = IDALL(ICHK)
*
*     ZEROTH ORDER TERMS
*     ******************
*
      DO 160 I=1,LNV
 160  JJ(I) = 0
*
      DO 170 I=1,IB
      CALL DAPEK(MB(I),JJ,BBIJJ)
      I1(IDAPO(MC(I))) = 0
      I2(IDAPO(MC(I))) = 0
      CC(IDAPO(MC(I))) = BBIJJ
 170  CONTINUE
*
      CALL DAPEK(ICHK,JJ,CHKJJ)
      IF(CHKJJ.GT.HALF) THEN
         CALL DAPOK(ICHK,JJ,ONEM)
      ELSE
         PRINT*,'ERROR IN DATREE, ZEROTH ORDER TERM OF ICHK IS ZERO'
C        CALL DADEB(31,'ERR DATRE2',1)
      ENDIF
*
      NTERM = 1
*
*     HIGHER ORDER TERMS
*     ******************
*
      DO 180 JL=1,INOB
 180  JV(JL) = 0
*
      JL = 0
      CHKJJ = ONE
*
 200  CONTINUE
      IF(JL.EQ.0.AND.CHKJJ.LE.HALF) GOTO 250
      IF(JL.LT.INOB.AND.CHKJJ.GT.HALF) THEN
         JL = JL + 1
         JJ(1) = JJ(1) + 1
         JV(JL) = 1
      ELSEIF(JV(JL).EQ.INVB) THEN
         JJ(JV(JL)) = JJ(JV(JL)) - 1
         JV(JL) = 0
         JL = JL - 1
         CHKJJ = ZERO
         GOTO 200
      ELSE
         JJ(JV(JL)) = JJ(JV(JL)) - 1
         JV(JL) = JV(JL) + 1
         JJ(JV(JL)) = JJ(JV(JL)) + 1
      ENDIF
*
      CALL DAPEK(ICHK,JJ,CHKJJ)
*
      IF(CHKJJ.LE.HALF) GOTO 200
*
      NTERM = NTERM + 1
      IF(NTERM.GT.IDALM(MC(1))) THEN
         PRINT*,'ERROR IN DATREE, NTERM TOO LARGE'
C        CALL DADEB(31,'ERR DATRE3',1)
      ENDIF
*
      CALL DAPOK(ICHK,JJ,ONEM)
*
*     PRINT*,'JL,JV = ',JL,JV(JL)
      DO 210 I=1,IB
      CALL DAPEK(MB(I),JJ,BBIJJ)
      I1(IDAPO(MC(I))+NTERM-1) = JL
      I2(IDAPO(MC(I))+NTERM-1) = JV(JL)
      CC(IDAPO(MC(I))+NTERM-1) = BBIJJ
 210  CONTINUE
*
      GOTO 200
*
 250  CONTINUE
*
      DO 260 I=1,IB
 260  IDALL(MC(I)) = NTERM
*
*     PERFORMING CROSS CHECKS
*     ***********************
*
      IF(NTERM.NE.NTERMF.OR.NTERM.NE.IDALL(ICHK)) THEN
         PRINT*,'ERROR IN DATREE, NTERM, NTERMF, IDALL(ICHK) =  '
     *          ,NTERM,NTERMF,IDALL(ICHK)
C        CALL DADEB(31,'ERR DATRE4',1)
      ENDIF
*
      DO 270 I=IDAPO(ICHK),IDAPO(ICHK)+NTERM-1
      IF(ABS(CC(I)+ONE).GT.EPSMAC) THEN
         PRINT*,'ERROR IN DATREE, NOT ALL TERMS IN ICHK ARE -1'
C        CALL DADEB(31,'ERR DATRE5',1)
      ENDIF
 270  CONTINUE
*
      I_SPEC_DUMMY(1) = ICHK
      CALL DADAL(I_SPEC_DUMMY,1)
      ICHK = I_SPEC_DUMMY(1)
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPUSH(MC,IC,XI,XF)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE APPLIES THE MATRIX WHOSE TREE IS STORED IN CA VECTOR MC
*     TO THE COORDINATES IN XI AND STORES THE RESULT IN XF
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION MC(IC),XF(IC),XI(IC),XM(LNO+1)
      save
*
      DO 10 I=1,IC
  10  XF(I) = CC(IDAPO(MC(I)))
*
      XM(1) = ONE
*
      DO 100 I=1,IDALL(MC(1))-1
*
      JL = I1(IDAPO(MC(1))+I)
      JV = I2(IDAPO(MC(1))+I)
      XX = XM(JL)*XI(JV)
      XM(JL+1) = XX
*
      DO 100 IV=1,IC
      XF(IV) = XF(IV) + CC(IDAPO(MC(IV))+I) * XX
 100  CONTINUE
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAINV(MA,IA,MB,IB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE INVERTS THE MATRIX MA WITH IA DA VECTORS AND
*     STORES THE RESULT IN MI
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      INTEGER JJ(LNV),ML(LNV),MA(*),MB(*)
      save
*
      DIMENSION X(LNV)
*
  
      DO 1 I=1,LNV
 1    JJ(I)=0
  
      IF(MA(1).EQ.MB(1)) THEN
        CALL DAINF(MB(1),INOB,INVB,IPOB,ILMB,ILLB)
        DO 9 I=1,IA
 9      CALL DAPOK(MA(I),JJ,ZERO)
        DO 101 IJ=1,IB
 101    ML(IJ)=0
        CALL DAALL(ML,IB,'$$DAJUNK$$',INOB,INVB)
        CALL DAINVT(MA,IA,ML,IB)
        DO 90 I=1,IB
 90     CALL DACOP(ML(I),MB(I))
        CALL DADAL(ML,IB)
      ELSE
        DO 99 I=1,IA
        CALL DAPEK(MA(I),JJ,X(I))
 99     CALL DAPOK(MA(I),JJ,ZERO)
        CALL DAINVT(MA,IA,MB,IB)
        DO 999 I=1,IA
 999    CALL DAPOK(MA(I),JJ,X(I))
      ENDIF
  
      RETURN
      END
*-----------------------------------------------------------------------------1
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAINVT(MA,IA,MB,IB)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE INVERTS THE MATRIX MA WITH IA DA VECTORS AND
*     STORES THE RESULT IN MI
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      INTEGER JJ(LNV),MS(LNV),ML(LNV),MA(*),MB(*)
*
      DIMENSION AA(LNV,LNV),AI(LNV,LNV)
      save
*
      CALL DAINF(MA(1),INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(MB(1),INOB,INVB,IPOB,ILMB,ILLB)
*
*     CONSISTENCY CHECKS
*     ******************
*
      CALL DAMCH(MA,IA)
      CALL DAMCH(MB,IB)
CETIENNE
      DO 959 IE=1,IB
 959  CALL DACON(MB(IE),0.D0)
CETIENNE
*
      IF(IA.NE.IB) THEN
         PRINT*,'ERROR IN DAINV, IA .NE. IB'
C        CALL DADEB(31,'ERR DAINV1',1)
      ELSEIF(IA.NE.INVA.OR.IB.NE.INVB) THEN
         PRINT*,'ERROR IN DAINV, IA.NE.INVA.OR.IB.NE.INVB'
C        CALL DADEB(31,'ERR DAINV2',1)
      ENDIF
*
*     ALLOCATING LOCAL VECTORS
*     ************************
*
      DO 10 I=1,IA
      MS(I) = 0
      ML(I) = 0
  10  CONTINUE
*
      CALL DAALL(MS,IA,'$$INV   $$',INOA,INVA)
      CALL DAALL(ML,IA,'$$INVL  $$',INOA,INVA)
*
*     EXTRACTING LINEAR MATRIX, GENERATING NONLINEAR PART OF A
*     ********************************************************
*
      DO 115 I=1,IB
      DO 110 J=1,IB
      DO 105 K=1,IB
 105  JJ(K) = 0
      JJ(J) = 1
      CALL DAPEK(MA(I),JJ,AMJJ)
      IF(DABS(AMJJ).GT.EPS) CALL DAPOK(MA(I),JJ,0.D0)
 110  AA(I,J) = AMJJ
      CALL DACMU(MA(I),-ONE,MA(I))
 115  CONTINUE
*
*     INVERTING LINEAR MATRIX, CHECKING RESULT AND STORING IN ML
*     **********************************************************
*
      CALL MATINV(AA,AI,IA,LNV,IER)
*
      IF(IER.EQ.132) THEN
         PRINT*,'ERROR IN ROUTINE DAINV'
C        CALL DADEB(31,'ERR DAINV3',1)
      ENDIF
*
      IER = 0
      DO 140 I=1,IB
      DO 140 J=1,IB
      PROD = 0.D0
      DO 120 K=1,IB
      JJ(K) = 0
 120  PROD = PROD + AA(I,K)*AI(K,J)
      IF(I.EQ.J) PROD = PROD - ONE
      IF(DABS(PROD).GT.100*EPSMAC) THEN
         PRINT*,'ERROR IN DAINV, INVERSION DID NOT WORK,I,J,PROD = ',
     *           I,J,PROD
         IER = 1
      ENDIF
CETIENNE
      IF(DABS(PROD).GT.100*EPSMAC) RETURN
CETIENNE
      JJ(J) = 1
      CALL DAPOK(MB(I),JJ,AI(I,J))
      CALL DAPOK(ML(I),JJ,AI(I,J))
 140  CONTINUE
*
C     IF(IER.EQ.1) CALL DADEB(31,'ERR DAINV5',1)
*
*     ITERATIVELY COMPUTING DIFFERENT PARTS OF THE INVERSE
*     ****************************************************
*
*     MB (OF ORDER I) = A1^-1 O [ E - ANL (NONLINEAR) O MB (OF ORDER I) ]
*
      NOCUT0 = NOCUT
*
      DO 250 I=2,NOCUT
*
      NOCUT = I
*
      CALL DACCT(MA,IA,MB,IB,MS,IA)
      DO 240 J=1,IB
      DO 230 K=1,IB
 230  JJ(K) = 0
      JJ(J) = 1
      CALL DAPEK(MS(J),JJ,AMSJJ)
      CALL DAPOK(MS(J),JJ,AMSJJ+ONE)
 240  CONTINUE
*
      CALL DACCT(ML,IA,MS,IA,MB,IB)
*
 250  CONTINUE
*
      NOCUT = NOCUT0
*
*     FLIPPING BACK SIGN OF A, FILLING UP FIRST ORDER PART AGAIN
*     **********************************************************
*
      DO 320 I=1,IB
      CALL DACMU(MA(I),-ONE,MA(I))
      DO 320 J=1,IB
      DO 310 K=1,IB
 310  JJ(K) = 0
      JJ(J) = 1
      CALL DAPOK(MA(I),JJ,AA(I,J))
 320  CONTINUE
*
      CALL DADAL(ML,IA)
      CALL DADAL(MS,IA)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAMATI(A,AI,N,NMX,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *********************************
*
*     THIS SUBROUTINE INVERTS THE MATRIX A AND STORES THE RESULT IN AI
*     INPUT  A   - SAVED
*            N   - ORDER OF MATRIX < 100
*     OUTPUT AI  - A INVERSE
*            IER - 0 NO ERROR
*                  132 ZERO DETERMINANT
*
      PARAMETER (NMAX=100)
      DIMENSION A(NMX,NMX),AI(NMX,NMX),AW(NMAX,NMAX),INDX(NMAX)
      save
  
      DO 12 I=1,N
         DO 11 J=1,N
            AW(I,J) = A(I,J)
11       AI(I,J) = 0.0
12    AI(I,I) = 1.0
  
      CALL DALUDC(AW,N,NMAX,INDX,D,IER)
      IF (IER .EQ. 132) RETURN
      DO 13 J=1,N
13    CALL DALUBK(AW,N,NMAX,INDX,AI(1,J),NMX)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DALUDC(A,N,NP,INDX,D,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ************************************
*
*     THIS SUBROUTINE DECOMPOSES A MATRIX INTO LU FORMAT
*     INPUT A: NXN MATRIX - WILL BE OVERWRITTEN BY THE LU DECOMP.
*           NP: PHYSICAL DIMENSION OF A
*           INDX: ROW PERMUTATION VECTOR
*           D: EVEN OR ODD ROW INTERCHANGES
*
*     REFERENCE: NUMERICAL RECIPIES BY PRESS ET AL (CAMBRIDGE) PG. 35
*
      PARAMETER (NMAX = 100, TINY = 1.0E-20)
      DIMENSION A(NP,NP), INDX(NP), VV(NMAX)
      save
      IER=0.
      D=1.
      DO 12 I=1,N
         AAMAX=0.
         DO 11 J=1,N
            IF(ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11       CONTINUE
         IF(AAMAX.EQ.0.) THEN
            IER=132
            RETURN
         ENDIF
         VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
         IF(J.GT.1) THEN
            DO 14 I=1,J-1
               SUM=A(I,J)
               IF(I.GT.1) THEN
                  DO 13 K=1,I-1
                     SUM=SUM-A(I,K)*A(K,J)
13                CONTINUE
                  A(I,J)=SUM
               ENDIF
14          CONTINUE
         ENDIF
         AAMAX=0.
         DO 16 I=J,N
            SUM=A(I,J)
            IF (J.GT.1) THEN
               DO 15 K=1,J-1
                  SUM=SUM-A(I,K)*A(K,J)
15             CONTINUE
               A(I,J)=SUM
            ENDIF
            DUM=VV(I)*ABS(SUM)
            IF(DUM.GE.AAMAX) THEN
               IMAX=I
               AAMAX=DUM
            ENDIF
16       CONTINUE
         IF (J.NE.IMAX) THEN
            DO 17 K=1,N
               DUM=A(IMAX,K)
               A(IMAX,K)=A(J,K)
               A(J,K)=DUM
17          CONTINUE
            D=-D
            VV(IMAX)=VV(J)
         ENDIF
         INDX(J)=IMAX
         IF(J.NE.N) THEN
            IF(A(J,J).EQ.0.) A(J,J)=TINY
            DUM=1./A(J,J)
            DO 18 I=J+1,N
               A(I,J)=A(I,J)*DUM
18          CONTINUE
         ENDIF
19    CONTINUE
      IF(A(N,N).EQ.0.) A(N,N)=TINY
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DALUBK(A,N,NP,INDX,B,NMX)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ************************************
*
*     THIS SUBROUTINE SOLVES SET OF LINEAR EQUATIONS AX=B,
*     INPUT A: NXN MATRIX IN LU FORM GIVEN BY DALUDC
*           NP: PHYSICAL DIMENSION OF A
*           INDX: ROW PERMUTATION VECTOR
*           D: EVEN OR ODD ROW INTERCHANGES
*           B: RHS OF LINEAR EQUATION - WILL BE OVERWRITTEN BY X
*
*     REFERENCE: NUMERICAL RECIPIES BY PRESS ET AL (CAMBRIDGE) PG. 36
*
      DIMENSION A(NP,NP), INDX(NP), B(NMX)
      save
      II = 0
      DO 12 I=1,N
         LL = INDX(I)
         SUM = B(LL)
         B(LL) = B(I)
         IF(II.NE.0) THEN
            DO 11 J=II,I-1
               SUM = SUM-A(I,J)*B(J)
11          CONTINUE
         ELSE IF (SUM.NE.0.) THEN
            II = I
         ENDIF
         B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
         SUM=B(I)
         IF(I.LT.N) THEN
            DO 13 J=I+1,N
               SUM = SUM-A(I,J)*B(J)
13          CONTINUE
         ENDIF
         B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPIN(MA,IA,MB,IB,JIND)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **********************************
*
*     THIS SUBROUTINE PERFORMS A PARTIAL INVERSION OF THE ROWS MARKED WITH
*     NONZERO ENTRIES IN JJ OF THE MATRIX A. THE RESULT IS STORED IN B.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      INTEGER JJ(LNV),JIND(*),MA(*),MB(*),MN(LNV),MI(LNV),ME(LNV)
      save
*
      CALL DAINF(MA(1),INOA,INVA,IPOA,ILMA,ILLA)
*
      DO 5 I=1,IA
      MN(I) = 0
      MI(I) = 0
      ME(I) = 0
  5   CONTINUE
*
      CALL DAALL(MN,IA,'$$PIN1  $$',INOA,INVA)
      CALL DAALL(MI,IA,'$$PIN2  $$',INOA,INVA)
      CALL DAALL(ME,IA,'$$PIN3  $$',INOA,INVA)
*
      DO 10 I=1,IA
      DO  8 K=1,NVMAX
  8   JJ(K) = 0
      JJ(I) = 1
  10  CALL DAPOK(ME(I),JJ,ONE)
*
      DO 20 I=1,IA
      CALL DACOP(MA(I),MN(I))
      IF(JIND(I).EQ.0) CALL DACOP(ME(I),MN(I))
  20  CONTINUE
*
      CALL DAINV(MN,IA,MI,IA)
*
      DO 30 I=1,IA
      IF(JIND(I).EQ.0) CALL DACOP(MA(I),ME(I))
  30  CONTINUE
*
      CALL DACCT(ME,IA,MI,IA,MB,IB)
*
      CALL DADAL(ME,IA)
      CALL DADAL(MI,IA)
      CALL DADAL(MN,IA)
*
      RETURN
      END
*FF
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DADER(IDIF,INA,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE COMPUTES THE DERIVATIVE WITH RESPECT TO VARIABLE I
*     OF THE VECTOR A AND STORES THE RESULT IN C.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      DIMENSION I_SPEC_DUMMY(1)
      save
*-----------------------------------------------------------------------------9
*
  
      IF(INA.EQ.INC) THEN
        CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
        INCC=0
        I_SPEC_DUMMY(1) = INCC
        CALL DAALL(I_SPEC_DUMMY,1,'$$DAJUNK$$',INOC,INVC)
        INCC = I_SPEC_DUMMY(1)
        CALL DADERT(IDIF,INA,INCC)
        CALL DACOP(INCC,INC)
        I_SPEC_DUMMY(1) = INCC
        CALL DADAL(I_SPEC_DUMMY,1)
        INCC = I_SPEC_DUMMY(1)
      ELSE
        CALL DADERT(IDIF,INA,INC)
      ENDIF
  
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DADERT(IDIF,INA,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE COMPUTES THE DERIVATIVE WITH RESPECT TO VARIABLE I
*     OF THE VECTOR A AND STORES THE RESULT IN C.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      IF(INVA.EQ.0.OR.INVC.EQ.0) THEN
         PRINT*,'ERROR, DADER CALLED WITH CA VECTOR'
C        CALL DADEB(31,'ERR DADER ',1)
      ENDIF
      IF(NOMAX.EQ.1) THEN
         PRINT*,'ERROR, DADER CALLED WITH NOMAX = 1'
C        CALL DADEB(31,'ERR DADER2',1)
      ENDIF
*
      CALL DACHK(INA,INOA,INVA,0,-1,-1,INC,INOC,INVC)
*
      IBASE = NOMAX + 1
*
      IF(IDIF.GT.(NVMAX+1)/2) THEN
         IDER1  = 0
         IDER1S = 0
         IDER2  = IDIF-(NVMAX+1)/2
         IDER2S = 1
         DO 10 JJ=1,IDER2-1
  10     IDER2S = IDER2S*IBASE
         XDIVI  = IDER2S*IBASE
      ELSE
         IDER1  = IDIF
         IDER1S = 1
         DO 20 JJ=1,IDER1-1
  20     IDER1S = IDER1S*IBASE
         IDER2  = 0
         IDER2S = 0
         XDIVI  = IDER1S*IBASE
      ENDIF
*
      IBASE = NOMAX+1
*
      IC = IPOC-1
*
      DO 100 I=IPOA,IPOA+ILLA-1
*
      IF(IDER1.EQ.0) THEN
         IEE = I2(I)
      ELSE
         IEE = I1(I)
      ENDIF
*
      X = IEE/XDIVI
      IFAC = INT(IBASE*(X-INT(X+EPSMAC)+EPSMAC))
*
      IF(IFAC.EQ.0) GOTO 100
*
      IC = IC + 1
      CC(IC) = CC(I)*IFAC
      I1(IC) = I1(I) - IDER1S
      I2(IC) = I2(I) - IDER2S
*
 100  CONTINUE
*
      IDALL(INC) = IC - IPOC + 1
      IF(IDALL(INC).GT.IDALM(INC)) THEN
         PRINT*,'ERROR IN DADER '
C        CALL DADEB(31,'ERR DADER ',1)
      ENDIF
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPOI(INA,INB,INC,N)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *******************************
*
*     THIS SUBROUTINE COMPUTES THE POISSON BRACKET OF THE VECTORS A AND
*     B AND STORES THE RESULT IN C. N IS THE DEGREE OF FREEDOM OF THE SYSTEM.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      INTEGER IS(3)
      save
*
      IS(1) = 0
      IS(2) = 0
      IS(3) = 0
      CALL DAALL(IS,3,'$$DAPOI $$',NOMAX,NVMAX)
*
      CALL DACON(INC,ZERO)
*
      DO 100 I=1,N
*
      CALL DADER(2*I-1,INA,IS(1))
      CALL DADER(2*I,  INB,IS(2))
      CALL DAMUL(IS(1),IS(2),IS(3))
      CALL DAADD(INC,IS(3),IS(1))
      CALL DACOP(IS(1),INC)
*
      CALL DADER(2*I,  INA,IS(1))
      CALL DADER(2*I-1,INB,IS(2))
      CALL DAMUL(IS(1),IS(2),IS(3))
      CALL DASUB(INC,IS(3),IS(1))
      CALL DACOP(IS(1),INC)
*
 100  CONTINUE
*
      CALL DADAL(IS,3)
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACFU(INA,FUN,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE APPLIES THE EXTERNAL REAL*8 FUNCTION
*     OF THE EXPONENTS FUN TO EACH COEFFICIENT OF A AND STORES THE
*     RESULT IN C
*
*-----------------------------------------------------------------------------1
      EXTERNAL FUN
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
C     DIMENSION J(LNV)
      DIMENSION I_SPEC_DUMMY(1)
      save
*
  
      IF(INA.EQ.INC) THEN
        CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
        INCC=0
        I_SPEC_DUMMY(1) = INCC
        CALL DAALL(I_SPEC_DUMMY,1,'$$DAJUNK$$',INOC,INVC)
        INCC = I_SPEC_DUMMY(1)
        CALL DACFUT(INA,FUN,INCC)
        CALL DACOP(INCC,INC)
        I_SPEC_DUMMY(1) = INCC
        CALL DADAL(I_SPEC_DUMMY,1)
        INCC = I_SPEC_DUMMY(1)
      ELSE
        CALL DACFUT(INA,FUN,INC)
      ENDIF
  
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACFUT(INA,FUN,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE APPLIES THE EXTERNAL REAL*8 FUNCTION
*     OF THE EXPONENTS FUN TO EACH COEFFICIENT OF A AND STORES THE
*     RESULT IN C
*
*-----------------------------------------------------------------------------1
      EXTERNAL FUN
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION J(LNV)
      save
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      IF(INVA.EQ.0.OR.INVC.EQ.0) THEN
         PRINT*,'ERROR, DACFU CALLED WITH CA VECTOR'
C        CALL DADEB(31,'ERR DADER ',1)
      ENDIF
      IF(NOMAX.EQ.1) THEN
         PRINT*,'ERROR, DACFU CALLED WITH NOMAX = 1'
C        CALL DADEB(31,'ERR DACFU2',1)
      ENDIF
*
      CALL DACHK(INA,INOA,INVA,0,-1,-1,INC,INOC,INVC)
*
      IC = IPOC - 1
*
      DO 100 IA=IPOA,IPOA+ILLA-1
*
      CALL DANCD(I1(IA),I2(IA),J)
      CFAC = FUN(J)
      IF(DABS(CFAC).LT.EPS) GOTO 100
*
      IC = IC + 1
      CC(IC) = CC(IA)*CFAC
      I1(IC) = I1(IA)
      I2(IC) = I2(IA)
*
 100  CONTINUE
*
      IDALL(INC) = IC - IPOC + 1
      IF(IDALL(INC).GT.IDALM(INC)) THEN
         PRINT*,'ERROR IN DACFU '
C        CALL DADEB(31,'ERR DACFU ',1)
      ENDIF
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAMUF(FUN)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *********************
*
*     THIS SUBROUTINE SETS THE FILTER ARRAY IFI WHICH CONTAINS INFORMATION
*     ON WHICH MONOMIALS ARE TO BE IGNORED IN THE BASIC DA OPERATIONS.
*     ALL ENTRIES WHICH ARE ZERO IN IFI DENOTE A MONOMIAL TO BE IGNORED.
*
*-----------------------------------------------------------------------------1
      EXTERNAL FUN
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION J(LNV)
      save
*
      LFI = 1
*
      DO 100 IA=1,NMMAX
*
      CALL DANCD(I1(IA),I2(IA),J)
      CFAC = FUN(J)
*
      IF(ABS(CFAC).LT.EPS) THEN
         IFI(IA) = 0
      ELSE
         IFI(IA) = 1
      ENDIF
*
 100  CONTINUE
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAIMP(H,LH,INA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **************************
*
*     THIS SUBROUTINE "IMPORTS" THE ARRAY H WITH LENGTH LH AND PUTS ITS
*     ENTRIES INTO THE DA VECTOR A
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION H(LH)
      save
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
*
      IF(NMMAX.GT.LH) THEN
         PRINT*,'ERROR IN ROUTINE DAIMP, ILMA,LH = ',ILMA,LH
C        CALL DADEB(31,'ERR DAIMP1',1)
      ENDIF
*
      DO 10 I=1,NMMAX
  10  CC(I) = H(I)
*
      CALL DAPAC(INA,1)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAEXP(INA,H,LH)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **************************
*
*     THIS SUBROUTINE "EXPORTS" THE DA VACTOR A AND STORES ITS COEFFICIENTS
*     IN THE ARRAY H WITH LENGTH LH
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION H(LH)
      save
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
*
      IF(NMMAX.GT.LH) THEN
         PRINT*,'ERROR IN ROUTINE DAEXP, ILMA,LH = ',ILMA,LH
C        CALL DADEB(31,'ERR DAEXP2',1)
      ENDIF
*
      CALL DAUNP(INA)
*
      DO 10 I=1,NMMAX
  10  H(I) = CC(I)
*
      RETURN
      END
*FF
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPRIA(INA,IUNIT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ****************************
*
*     SPECIAL VERSION FOR CHROMATICITY CALCULATION
*     --------------------------------------------
*
*     THIS SUBROUTINE PRINTS THE DA VECTOR INA TO UNIT IUNIT.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10,C10*10,K10*10                                  1
      COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*
      DIMENSION J(LNV)
      save
*
      IF(INA.LT.1.OR.INA.GT.NDA) THEN
         PRINT*,'ERROR IN DAPRI, INA = ',INA
         STOP
      ENDIF
*
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)
*
C     WRITE(IUNIT,'(/1X,A,A,I5,A,I5,A,I5/1X,A/)')
C    *           DANAME(INA),', NO =',INOA,', NV =',INVA,', INA =',INA,
C    *              '***********'//'**********************************'
*
C     IF(ILLA.NE.0) WRITE(IUNIT,'(A)')
C    *    '    I  COEFFICIENT          ORDER   EXPONENTS'
C     IF(ILLA.EQ.0) WRITE(IUNIT,'(A)') '   ALL COMPONENTS ZERO '
*
      C10='      NO ='
      K10='      NV ='
  
C     WRITE(IUNIT,'(A10,I6,A10,I6)') C10,INOA,K10,INVA
  
      IOUT = 0
*
      DO 100 IOA = 0,INOA
      DO 100 II=IPOA,IPOA+ILLA-1
      IF(IEO(IA1(I1(II))+IA2(I2(II))).NE.IOA) GOTO 100
*
CETIENNE
      IF(DABS(CC(II)).GT.EPS) THEN
CETIENNE
  
      IOUT = IOUT+1
      CALL DANCD(I1(II),I2(II),J)
*
  
C      WRITE(IUNIT,*) IOUT,IOA,CC(II),(J(I),I=1,INVA)
  
      ISUM = 0
      DO 90 I = 6,13
      ISUM = ISUM + J(I)
  90  CONTINUE
  
C     IF (ISUM.GT.1) GOTO 591
      IF (J(1).NE.0) GOTO 591
      IF (J(2).NE.0) GOTO 591
      IF (J(3).NE.0) GOTO 591
      IF (J(4).NE.0) GOTO 591
      IF (J(5).EQ.0) GOTO 591
      WRITE(IUNIT,501) IOA,CC(II),(J(I),I=1,INVA)
 501  FORMAT(' ', I3,1X,G24.17,1X,100(1X,I2))
 502  FORMAT(' ', I5,1X,G24.17,1X,100(1X,I2))
  
 591  CONTINUE
      ENDIF
CETIENNE
*
 100  CONTINUE
*
      DO 111 I=1,LNV
 111  J(I)=0
  
      IF(IOUT.EQ.0) IOUT=1
  
C     WRITE(IUNIT,502) -IOUT,0.D0,(J(I),I=1,INVA)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPRI(INA,IUNIT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***************************
*
*     THIS SUBROUTINE PRINTS THE DA VECTOR INA TO UNIT IUNIT.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*
      DIMENSION J(LNV)
      save
*
      IF(INA.LT.1.OR.INA.GT.NDA) THEN
         PRINT*,'ERROR IN DAPRI, INA = ',INA
C        X = SQRT(-ONE)
C        PRINT*,X
         STOP
      ENDIF
*
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)
*
      WRITE(IUNIT,'(/1X,A,A,I5,A,I5,A,I5/1X,A/)')
     *           DANAME(INA),', NO =',INOA,', NV =',INVA,', INA =',INA,
     *              '***********'//'**********************************'
*
      IF(INVA.EQ.0) THEN
         WRITE(IUNIT,'(A)')
     *    '    I  VALUE  '
         DO 80 I = IPOA,IPOA+ILLA-1
 80      WRITE(IUNIT,'(I6,2X,G20.14)') I-IPOA, CC(I)
      ELSEIF(NOMAX.EQ.1) THEN
         WRITE(IUNIT,'(A)')
     *    '       COEFFICIENT          VARIABLE'
         DO 90 I=1,ILLA
 90      WRITE(IUNIT,'(6X,2X,G20.14,I5)') CC(IPOA+I-1),I-1
      ELSE
         IF(ILLA.NE.0) WRITE(IUNIT,'(A)')
     *    '    I  COEFFICIENT          ORDER   EXPONENTS'
         IF(ILLA.EQ.0) WRITE(IUNIT,'(A)') '   ALL COMPONENTS ZERO '
         IOUT = 0
         DO 100 IOA = 0,INOA
         DO 100 II=IPOA,IPOA+ILLA-1
         IF(IEO(IA1(I1(II))+IA2(I2(II))).NE.IOA) GOTO 100
         CALL DANCD(I1(II),I2(II),J)
CETIENNE
         IF(ABS(CC(II)).GT.EPS) THEN
CETIENNE
         IOUT = IOUT+1
         WRITE(IUNIT,'(I6,2X,G20.14,I5,4X,18(2I2,1X))')
     *      IOUT,CC(II),IOA,(J(III),III=1,NVMAX)
CETIENNE
         WRITE(IUNIT,*) CC(II)
         ENDIF
CETIENNE
*
 100     CONTINUE
*
      ENDIF
  
  
      WRITE(IUNIT,'(A)') '                                      '
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPRIMAX(INA,IUNIT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***************************
*
*     THIS SUBROUTINE PRINTS THE DA VECTOR INA TO UNIT IUNIT.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*
      DIMENSION J(LNV)
      save
*
      IF(INA.LT.1.OR.INA.GT.NDA) THEN
         PRINT*,'ERROR IN DAPRI, INA = ',INA
         STOP
      ENDIF
*
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)
*
C     WRITE(IUNIT,'(/1X,A,A,I5,A,I5,A,I5/1X,A/)')
C    *           DANAME(INA),', NO =',INOA,', NV =',INVA,', INA =',INA,
C    *              '***********'//'**********************************'
*
      IF(INVA.EQ.0) THEN
C        WRITE(IUNIT,'(A)')
C    *    '    I  VALUE  '
         DO 80 I = IPOA,IPOA+ILLA-1
 80      WRITE(IUNIT,'(I6,2X,G20.14)') I-IPOA, CC(I)
      ELSEIF(NOMAX.EQ.1) THEN
C        WRITE(IUNIT,'(A)')
C    *    '       COEFFICIENT          VARIABLE'
         DO 90 I=1,ILLA
 90      WRITE(IUNIT,'(6X,2X,G20.14,I5)') CC(IPOA+I-1),I-1
      ELSE
C        IF(ILLA.NE.0) WRITE(IUNIT,'(A)')
C    *    '    I  COEFFICIENT          ORDER   EXPONENTS'
C        IF(ILLA.EQ.0) WRITE(IUNIT,'(A)') '   ALL COMPONENTS ZERO '
         IOUT = 0
         DO 100 IOA = 0,INOA
         DO 100 II=IPOA,IPOA+ILLA-1
         IF(IEO(IA1(I1(II))+IA2(I2(II))).NE.IOA) GOTO 100
         CALL DANCD(I1(II),I2(II),J)
CETIENNE
         IF(ABS(CC(II)).GT.EPS) THEN
CETIENNE
         IOUT = IOUT+1
         WRITE(IUNIT,'(I6,2X,G20.14,I5,4X,18(2I2,1X))')
     *      IOUT,CC(II),IOA,(J(III),III=1,NVMAX)
CETIENNE
         WRITE(IUNIT,*) CC(II)
         ENDIF
CETIENNE
*
 100     CONTINUE
*
      ENDIF
  
  
C     WRITE(IUNIT,'(A)') '                                      '
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAREA(INA,IUNIT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***************************
*
*     THIS SUBROUTINE READS THE DA VECTOR INA FROM UNIT IUNIT.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*
      CHARACTER C10*10
      DIMENSION J(LNV)
      save
*
      IF(INA.LT.1.OR.INA.GT.NDA) THEN
         PRINT*,'ERROR IN DAREA, INA = ',INA
C        X = SQRT(-ONE)
C        PRINT*,X
         STOP
      ENDIF
*
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)
*
      DO 5 I=1,LNV
   5  J(I) = 0
*
      CALL DACLR(1)
*
      IC = 0
*
      IWARNO = 0
      IWARNV = 0
      IWARIN = 0
*
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(18X,I4)') NNO
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(A10)') C10
*
      IF(NNO.EQ.1) THEN
         DO 90 I=1,ILLA
         READ(IUNIT,'(6X,2X,G20.14,I5)',ERR=95) CC(IPOA+I-1),II
         IF(II.NE.I-1) GOTO 95
  90     CONTINUE
         READ(IUNIT,'(A10)') C10
         IF(C10.NE.'          ') GOTO 95
         RETURN
  95     PRINT*,'ERROR IN DAREA, VECTOR COULD NOT BE READ'
         STOP
      ENDIF
*
      IIN = 0
*
  10  CONTINUE
      IIN = IIN + 1
      READ(IUNIT,'(I6,2X,G20.14,I5,4X,18(2I2,1X))')
     *             II,C,IO,(J(I),I=1,INVA)
*
      IF(II.EQ.0) GOTO 20
CETIENNE
      READ(IUNIT,*) C
CETIENNE
      IF(II.NE.IIN) THEN
         IF(IWARIN.EQ.0) PRINT*,'WARNING IN DAREA, FILE ',
     *              'NUMBERING OUT OF ORDER '
         IWARIN = 1
      ENDIF
      IO1 = 0
      DO 15 I=1,INVA
  15  IO1 = IO1 + J(I)
*
      IF(IO1.NE.IO) THEN
         IF(IWARNV.EQ.0) PRINT*,'WARNING IN DAREA, FILE ',
     *              'CONTAINS MORE VARIABLES THAN VECTOR'
         IWARNV = 1
         GOTO 10
      ENDIF
      IF(IO.GT.INOA) THEN
C        IF(IWARNO.EQ.0) PRINT*,'WARNING IN DAREA, FILE ',
C    *              'CONTAINS HIGHER ORDERS THAN VECTOR '
         IWARNO = 1
         GOTO 10
      ENDIF
*
      IC = IC + 1
      CALL DADCD(J,II1,II2)
      IC = IA1(II1) + IA2(II2)
      CC(IC) = C
      GOTO 10
*
  20  CONTINUE
*
      CALL DAPAC(INA,1)
*
      RETURN
      END
*FF
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAREAB(INA,IUNIT)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***************************
*
*     THIS SUBROUTINE READS THE DA VECTOR INA FROM UNIT IUNIT.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*
      CHARACTER C10*10
C    &                ,K10*10
      DIMENSION J(LNV)
      save
  
*
      IF(INA.LT.1.OR.INA.GT.NDA) THEN
         WRITE(6,*)'ERROR IN DAPRI, INA = ',INA
         X=0.
C        X = SQRT(-ONE)
         WRITE(6,*)X
         STOP
      ENDIF
*
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)
*
      DO 5 I=1,LNV
   5  J(I) = 0
*
      CALL DACLR(1)
*
      IC = 0
*
      IWARNO = 0
      IWARNV = 0
      IWARIN = 0
*
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(18X,I4)') NNO
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(A10)') C10
*
      IF(NNO.EQ.1) THEN
         DO 90 I=1,ILLA
         READ(IUNIT,'(6X,2X,G20.14,I5)',ERR=95) CC(IPOA+I-1),II
         IF(II.NE.I-1) GOTO 95
  90     CONTINUE
         READ(IUNIT,'(A10)') C10
         IF(C10.NE.'          ') GOTO 95
         RETURN
  95     WRITE(6,*)'ERROR IN DAREA, VECTOR COULD NOT BE READ'
         STOP
      ENDIF
*
      IIN = 0
*
  10  CONTINUE
      IIN = IIN + 1
      READ(IUNIT,'(I6,2X,G20.14,I5,4X,18(2I2,1X))')
     *             II,C,IO,(J(I),I=1,INVA)
*
      IF(II.EQ.0) GOTO 20
CETIENNE
      READ(IUNIT,*) C
CETIENNE
      IF(II.NE.IIN) THEN
         IF(IWARIN.EQ.0) WRITE(6,*)'WARNING IN DAREA, FILE ',
     *              'NUMBERING OUT OF ORDER '
         IWARIN = 1
      ENDIF
      IO1 = 0
      DO 15 I=1,INVA
  15  IO1 = IO1 + J(I)
*
      IF(IO1.NE.IO) THEN
         IF(IWARNV.EQ.0) WRITE(6,*)'WARNING IN DAREA, FILE ',
     *              'CONTAINS MORE VARIABLES THAN VECTOR'
         IWARNV = 1
         GOTO 10
      ENDIF
      IF(IO.GT.INOA) THEN
         IF(IWARNO.EQ.0) WRITE(6,*)'WARNING IN DAREA, FILE ',
     *              'CONTAINS HIGHER ORDERS THAN VECTOR '
         IWARNO = 1
         GOTO 10
      ENDIF
*
      IC = IC + 1
      CALL DADCD(J,II1,II2)
      IC = IA1(II1) + IA2(II2)
      CC(IC) = C
      GOTO 10
*
  20  CONTINUE
*
      CALL DAPAC(INA,1)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DADEB(IUNIT,C,ISTOP)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *******************************
*
*     THIS SUBROUTINE SERVES AS A DEBUGGING TOOL. IT PRINTS ALL
*     NONZERO INFORMATION IN THE COMMON BLOCKS AND ALL DA  VECTORS.
*
*FRS   !!!!!!!!  DISABLED  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*
      CHARACTER C*10
      save
*
CFRS
      IF(1.EQ.0) THEN
CFRS
      IF(IUNIT.EQ.31) THEN
      ENDIF
*
      WRITE(IUNIT,'(///A/1X,79(''*'')/)')
     * ' DIFFERENTIAL ALGEBRA DEBUGGING UTILITY, IDENTIFIER = '//C
*
      WRITE(IUNIT,'(4(A,I7)/)') ' NOMAX = ',NOMAX,', NVMAX = ',NVMAX,
     *                          ', NMMAX = ',NMMAX,', NOCUT = ',NOCUT
*
      WRITE(IUNIT,'(A/)')
     *   ' NAME        IDANO IDANV IDAPO IDALM IDALL  '
      DO 50 I=1,NDA
      WRITE(IUNIT,'(1X,A10,6I6)') DANAME(I),IDANO(I),
     *        IDANV(I),IDAPO(I),IDALM(I),IDALL(I)
  50  CONTINUE
*
      WRITE(IUNIT,'(/)')
*
      DO 100 I=1,NDA
      CALL DAPRI(I,IUNIT)
 100  CONTINUE
*
      WRITE(IUNIT,'(/1X,79(''*'')/)')
*
      IF(ISTOP.EQ.0) RETURN
*
*     PRODUCING SYSTEM ERROR TRACEBACK BY PERFORMING ILLEGAL OPERATION
*     ****************************************************************
*
      PRINT*,'  '
      PRINT*,'*** ENVOKING SYSTEM ERROR EXIT '
      PRINT*,'  '
C     PRINT*,SQRT(-ONE)
*
      ENDIF
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DARAN(INA,CM)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ************************
*
*     THIS SUBROUTINE FILLS THE DA VECTOR A WITH RANDOM ENTRIES.
*     FOR CM > 0, THE VECTOR IS FILLED WITH REALS,
*     FOR CM < 0, THE VECTOR IS FILLED WITH SINGLE DIGIT INTEGERS
*     ABS(CM) IS THE FILLING FACTOR
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      save
      DATA IRAN /1234321/
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
*
      IF(INVA.EQ.0.OR.NOMAX.EQ.1) THEN
         DO 10 I=IPOA,IPOA+ILMA-1
         IF(CM.GT.ZERO) THEN
            CC(I) = DABRAN(IRAN)
            IF(CC(I).GT.CM) CC(I) = ZERO
         ELSEIF(CM.LT.ZERO) THEN
            CC(I) = INT(1+10*DABRAN(IRAN))
            IF(CC(I).GT.-TEN*CM) CC(I) = ZERO
         ENDIF
  10     CONTINUE
         IDALL(INA) = IDALM(INA)
         RETURN
      ENDIF
*
      IF(INOA.NE.NOMAX.OR.INVA.NE.NVMAX) THEN
         PRINT*,'ERROR IN DARAN, ONLY VECTORS WITH NO = NOMAX AND'
     *          //' NV = NVMAX ALLOWED'
C        CALL DADEB(31,'ERR DARAN1',1)
      ENDIF
*
      CALL DACLR(1)
*
      DO 100 I=1,NMMAX
      IF(CM.GT.ZERO) THEN
         CC(I) = DABRAN(IRAN)
         IF(CC(I).GT.CM) CC(I) = ZERO
      ELSEIF(CM.LT.ZERO) THEN
         CC(I) = INT(1+10*DABRAN(IRAN))
         IF(CC(I).GT.-TEN*CM) CC(I) = ZERO
      ELSE
         PRINT*,'ERROR IN ROUTINE DARAN'
C        CALL DADEB(31,'ERR DARAN2',1)
      ENDIF
 100  CONTINUE
*
      CALL DAPAC(INA,1)
*
      RETURN
      END
*
C ANFANG FUNKTION
      REAL*8 FUNCTION DABRAN(IRAN)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ************************************
*
*     VERY SIMPLE RANDOM NUMBER GENERATOR
*
*
      save 
      DATA ICALL / 0 /, XRAN / .234D0 /
*
*
      XRAN = XRAN + TEN
      IF(XRAN.GT.C1E4) XRAN = XRAN - 9999.12345
      DABRAN = ABS(SIN(XRAN))
      DABRAN = 10*DABRAN
      DABRAN = DABRAN - INT(DABRAN)
      IF(DABRAN.LT. C1M1) DABRAN = DABRAN + C1M1
*
      RETURN
      END
*
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DANUM(NO,NV,NUMDA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
      save
*     *****************************
*
*     THIS SUBROUTINE COMPUTES THE NUMBER OF MONOMIALS OF
*     ORDER NO AND NUMBER OF VARIABLES NV
*
*
      NUMDA = 1
      MM = MAX(NV,NO)
*
      DO 5 I=1,MIN(NV,NO)
  5   NUMDA = (NUMDA*(MM+I))/I
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **********************************************
*
*     THIS SUBROUTINE SEARCHES THE NUMBER OF DA VECTOR C
*     AND RETURS THE INFORMATION IN COMMON DA
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      IF(INC.GE.1.AND.INC.LE.NDA) THEN
         INOC = IDANO(INC)
         INVC = IDANV(INC)
         IPOC = IDAPO(INC)
         ILMC = IDALM(INC)
         ILLC = IDALL(INC)
         RETURN
      ENDIF
*
      PRINT*, 'ERROR IN DAINF, DA VECTOR ',INC,' NOT FOUND '
C     CALL DADEB(31,'ERR DAINF1',1)
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAPAC(INC,LF)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ************************
*
*     THIS SUBROUTINE PACKS THE INFORMATION IN THE SCRATCH VECTOR 1
*     INTO THE VECTOR INC. IF LF = 1, THE FILTERING (CF DAMUF) IS
*     PERFORMED.
*     INVERSE IS DAUNP.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      IC = IPOC - 1
*
      IF(LFI.EQ.0.OR.LF.EQ.0) THEN
         DO 100 I=1,NMMAX
         CCC = CC(I)
         IF(ABS(CCC).LT.EPS) GOTO 100
         IC = IC + 1
         CC(IC) = CCC
         I1(IC) = IE1(I)
         I2(IC) = IE2(I)
 100     CONTINUE
      ELSE
         DO 200 I=1,NMMAX
         CCC = CC(I)
         IF(IFI(I).EQ.0) GOTO 200
         IF(ABS(CCC).LT.EPS) GOTO 200
         IC = IC + 1
         CC(IC) = CCC
         I1(IC) = IE1(I)
         I2(IC) = IE2(I)
 200     CONTINUE
      ENDIF
*
      IDALL(INC) = IC - IPOC + 1
      IF(IDALL(INC).GT.IDALM(INC)) THEN
         PRINT*,'ERROR IN DAPAC '
C        CALL DADEB(31,'ERR DAPAC1',1)
      ENDIF
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAUNP(INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *********************
*
*     THIS SUBROUTINE UNPACKS THE VECTOR C.
*     INVERSE IS DAPAC.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      CALL DACLR(1)
*
      DO 100 I=IPOC+ILLC-1,IPOC,-1
*
      IF(CC(I).EQ.ZERO) THEN
         PRINT*,'ERROR IN DAUNP AT 1 '
C        CALL DADEB(31,'ERR DAUNP1',1)
      ENDIF
      IC = IA1(I1(I)) + IA2(I2(I))
      IF(CC(IC).NE.ZERO) THEN
         PRINT*,'ERROR IN DAUNP AT 2 '
C        CALL DADEB(31,'ERR DAUNP2',1)
      ENDIF
*
      CC(IC) = CC(I)
*
 100  CONTINUE
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DACHK(INA,INOA,INVA, INB,INOB,INVB, INC,INOC,INVC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *************************************************************
*
*     THIS SUBROUTINE CHECKS IF THE VECTORS A, B AND C
*     HAVE COMPATIBLE ATTRIBUTES
*
      PARAMETER(LSW=1)
      save
*
      IF(LSW.EQ.1) RETURN
*
      IERR = 0
*
*     CASE OF A UNARY OPERATION
*     *************************
*
      IF(INOB.EQ.-1.AND.INVB.EQ.-1) THEN
         INVSUM = INVA + INVC
         IF(INVSUM.EQ.0) THEN
            IF(INOA.GT.INOC) IERR = 1
         ELSEIF(INVSUM.EQ.1) THEN
            IERR = 1
         ELSE
            IF(INOA.GT.INOC.OR.INVA.GT.INVC) IERR = 1
         ENDIF
         IF(IERR.EQ.1) THEN
            PRINT*,'ERROR IN DACHK INVA AND INVC',
     *             ' ARE INCOMPATIBLE',INOA,INVA,INOC,INVC
C           CALL DADEB(31,'ERR DACHK1',1)
         ENDIF
*
*     CASE OF A BINARY OPERATION
*     **************************
*
      ELSE
         INVSUM = INVA + INVB + INVC
         IF(INVSUM.EQ.0) THEN
            IF(INOA.GT.INOC.OR.INOB.GT.INOC) IERR = 1
         ELSEIF(INVSUM.EQ.1.OR.INVSUM.EQ.2) THEN
            IERR = 1
         ELSE
            IF(INOA.GT.INOC.OR.INOB.GT.INOC.OR.
     *         INVA.GT.INVC.OR.INVB.GT.INVC) IERR = 1
         ENDIF
         IF(IERR.EQ.1) THEN
            PRINT*,'ERROR IN DACHK INVA, INVB AND INVC',
     *             ' ARE INCOMPATIBLE'
C           CALL DADEB(31,'ERR DACHK2',1)
         ENDIF
      ENDIF
*
      RETURN
      END
*FF
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAMCH(IAA,IA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ************************
*
*     THIS SUBROUTINE CHECKS IF THE IA VECTORS IN THE MATRIX IA HAVE
*     IDENTICAL ATTRIBUTES.
*
      DIMENSION IAA(*)
      save
*
      CALL DAINF(IAA(1),INO1,INV1,IPOA,ILMA,ILLA)
*
      DO 10 I=2,IA
      CALL DAINF(IAA(I),INOI,INVI,IPOA,ILMA,ILLA)
      IF(INO1.NE.INOI.OR.INV1.NE.INVI) THEN
         PRINT*,'ERROR IN DAMCH, VECTORS ',IAA(1),' AND ',IAA(I),
     *          ' ARE INCOMPATIBLE '
         STOP
      ENDIF
  10  CONTINUE
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DADCD(JJ,IC1,IC2)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ****************************
*
*     THIS SUBROUTINE CODES THE EXPONENTS IN JJ INTO THEIR DA CODES I1,I2.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      INTEGER JJ(LNV)
      IBASE = NOMAX + 1
      ISPLIT = (NVMAX+1)/2
      IC1 = 0
      IC2 = 0
*
      DO 10 I=NVMAX,ISPLIT+1,-1
 10   IC2 = IC2*IBASE + JJ(I)
*
      DO 20 I=ISPLIT,1,-1
 20   IC1 = IC1*IBASE + JJ(I)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DANCD(IC1,IC2,JJ)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ****************************
*
*     THIS SUBROUTINE ENCODES THE EXPONENTS IN JJ FROM THEIR DA CODES I1,I2.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      INTEGER JJ(LNV)
      save
      IBASE = NOMAX + 1
      ISPLIT = (NVMAX+1)/2
*
      IC = IC1
      DO 10 I=1,ISPLIT
      X  = IC/DBLE(IBASE)
      IC = INT(X+EPSMAC)
  10  JJ(I) = NINT(IBASE*(X-IC))
*
      IC = IC2
      DO 20 I=ISPLIT+1,NVMAX
      X  = IC/DBLE(IBASE)
      IC = INT(X+EPSMAC)
  20  JJ(I) = NINT(IBASE*(X-IC))
*
      DO 30 I=NVMAX+1,20
  30  JJ(I) = 0
*
      RETURN
      END
*
C ANFANG FUNKTION
      REAL*8 FUNCTION DARE(INA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***********************************
*     NEW VERSION OF DARE, AUGUST 1992
*     SUPPOSED TO TREAT THE 0TH COMPONENT ACCURATELY
*
*     30.10 1997 E.Mcintosh & F.Schmidt
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      DIMENSION J(LNV)
      save
*-----------------------------------------------------------------------------9
*
c      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
*
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)

CFRS 30.10.1997
      IF(NOMAX.EQ.1) then
        DARE = CC(IPOA)
        RETURN
      endif
CFRS 30.10.1997
CFRS March 1997
C      IF(NOMAX.EQ.1) goto 110
CFRS March 1997

      IOA = 0
      DO 100 II=IPOA,IPOA+ILLA-1
      IF(IEO(IA1(I1(II))+IA2(I2(II))).NE.IOA) GOTO 100
      CALL DANCD(I1(II),I2(II),J)
      DO 110 JJ=1,INVA
      IF(J(JJ).NE.0) GOTO 100
 110  CONTINUE
      DARE = CC(IPOA)
      RETURN
 100  CONTINUE
      DARE = ZERO
      RETURN
      END
C ANFANG UNTERPROGRAMM
      SUBROUTINE MTREE(MB,IB,MC,IC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE IS USED FOR CONCATENATION AND TRACKING OF VECTORS
*     THROUGH A DA MAP. IT COMPUTES THE TREE THAT HAS TO BE TRANSVERSED
*     MB IS THE DA MATRIX WITH IA TERMS. THE OUTPUT MC IS A CA MATRIX WHICH
*     CONTAINS COEFFICIENTS AND CONTROL INTEGERS USED FOR THE TRAVERSAL.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      INTEGER JJ(LNV),JV(0:LNO),MB(*),MC(*)
      DIMENSION I_SPEC_DUMMY(1)
      save
*
*     CONSISTENCY CHECKS
*     ******************
*
      IIB = MB(1)
      IIC = MC(1)
      CALL DAINF(IIB,INOB,INVB,IPOB,ILMB,ILLB)
      CALL DAINF(IIC,INOC,INVC,IPOC,ILMC,ILLC)
*
      CALL DAMCH(MB,IB)
      CALL DAMCH(MC,IC)
*
      IF(IB.NE.IC) THEN
         PRINT*,'ERROR IN MTREE, IB .NE. IC'
C        CALL DADEB(31,'ERR MTREE1',1)
      ENDIF
*
*     ALLOCATING LOCAL VECTORS
*     ************************
*
      ICHK = 0
      I_SPEC_DUMMY(1) = ICHK
      CALL DAALL(I_SPEC_DUMMY,1,'$$MTREE $$',NOMAX,NVMAX)
      ICHK = I_SPEC_DUMMY(1)
*
*     FIND ALL THE ENTRIES TO BE LOOKED FOR
*     *************************************
*
      CALL DACLR(1)
*
      CC(1) = ONE
*
      DO 100 I=1,IB
      IF(NOMAX.EQ.1) THEN
      DO 91 IB1 = 2,7
      CC(IB1) = ONE
   91 CONTINUE
      ELSE
      DO  90 IBI = IDAPO(MB(I)),IDAPO(MB(I))+IDALL(MB(I))-1
      ICC = IA1(I1(IBI)) + IA2(I2(IBI))
      IF(IEO(ICC).GT.INOB) GOTO 90
      CC(ICC) = ONE
   90 CONTINUE
      ENDIF
  100 CONTINUE
*
      DO 150 II=1,INOB
*
*     SEARCHING FOR FATHER FOR EACH TERM
*
      DO 140 I=1,NMMAX
      IF(CC(I).LT.HALF) GOTO 140
*
      JNON = 0
      CALL DANCD(I1(I),I2(I),JJ)
      DO 130 J=1,INVB
      IF(JJ(J).EQ.0) GOTO 130
      JNON = J
      JJ(J) = JJ(J) - 1
      CALL DADCD(JJ,IC1,IC2)
      APEK = CC(IA1(IC1)+IA2(IC2))
      JJ(J) = JJ(J) + 1
      IF(APEK.GE.HALF) GOTO 140
  130 CONTINUE
*
      IF(JNON.EQ.0) GOTO 140
*
*     TERM IS AN ORPHAN, SO CREATE FOSTER FATHER
*
      JJ(JNON) = JJ(JNON) - 1
      CALL DADCD(JJ,IC1,IC2)
      CC(IA1(IC1)+IA2(IC2)) = ONE
*
  140 CONTINUE
  150 CONTINUE
*
      CALL DAPAC(ICHK,0)
C     CALL DAPRI(ICHK,32)
*
*     SETTING UP TREE STRUCTURE
*     *************************
*
      NTERMF = IDALL(ICHK)
*
*     ZEROTH ORDER TERMS
*     ******************
*
      DO 160 I=1,LNV
 160  JJ(I) = 0
*
      DO 170 I=1,IB
      CALL DAPEK(MB(I),JJ,BBIJJ)
      I1(IDAPO(MC(I))) = 0
      I2(IDAPO(MC(I))) = 0
      CC(IDAPO(MC(I))) = BBIJJ
 170  CONTINUE
*
      CALL DAPEK(ICHK,JJ,CHKJJ)
      IF(CHKJJ.GT.HALF) THEN
         CALL DAPOK(ICHK,JJ,ONEM)
      ELSE
         PRINT*,'ERROR IN MTREE, ZEROTH ORDER TERM OF ICHK IS ZERO'
C        CALL DADEB(31,'ERR MTREE2',1)
      ENDIF
*
      NTERM = 1
*
*     HIGHER ORDER TERMS
*     ******************
*
      DO 180 JL=1,INOB
 180  JV(JL) = 0
*
      JL = 0
      CHKJJ = ONE
*
 200  CONTINUE
      IF(JL.EQ.0.AND.CHKJJ.LE.HALF) GOTO 250
      IF(JL.LT.INOB.AND.CHKJJ.GT.HALF) THEN
         JL = JL + 1
         JJ(1) = JJ(1) + 1
         JV(JL) = 1
      ELSEIF(JV(JL).EQ.INVB) THEN
         JJ(JV(JL)) = JJ(JV(JL)) - 1
         JV(JL) = 0
         JL = JL - 1
         CHKJJ = ZERO
         GOTO 200
      ELSE
         JJ(JV(JL)) = JJ(JV(JL)) - 1
         JV(JL) = JV(JL) + 1
         JJ(JV(JL)) = JJ(JV(JL)) + 1
      ENDIF
*
      CALL DAPEK(ICHK,JJ,CHKJJ)
*
      IF(CHKJJ.LE.HALF) GOTO 200
*
      NTERM = NTERM + 1
      IF(NTERM.GT.IDALM(MC(1))) THEN
         PRINT*,'ERROR IN MTREE, NTERM TOO LARGE'
C        CALL DADEB(31,'ERR MTREE3',1)
      ENDIF
*
      CALL DAPOK(ICHK,JJ,ONEM)
*
*     PRINT*,'JL,JV = ',JL,JV(JL)
      DO 210 I=1,IB
      CALL DAPEK(MB(I),JJ,BBIJJ)
      I1(IDAPO(MC(I))+NTERM-1) = JL
      I2(IDAPO(MC(I))+NTERM-1) = JV(JL)
      CC(IDAPO(MC(I))+NTERM-1) = BBIJJ
 210  CONTINUE
*
      GOTO 200
*
 250  CONTINUE
*
      DO 260 I=1,IB
 260  IDALL(MC(I)) = NTERM
*
*     PERFORMING CROSS CHECKS
*     ***********************
*
      IF(NTERM.NE.NTERMF.OR.NTERM.NE.IDALL(ICHK)) THEN
         PRINT*,'ERROR IN MTREE, NTERM, NTERMF, IDALL(ICHK) =  '
     *          ,NTERM,NTERMF,IDALL(ICHK)
C        CALL DADEB(31,'ERR MTREE4',1)
      ENDIF
*
      DO 270 I=IDAPO(ICHK),IDAPO(ICHK)+NTERM-1
      IF(ABS(CC(I)+ONE).GT.EPSMAC) THEN
         PRINT*,'ERROR IN MTREE, NOT ALL TERMS IN ICHK ARE -1'
C        CALL DADEB(31,'ERR MTREE5',1)
      ENDIF
 270  CONTINUE
*
      I_SPEC_DUMMY(1) = ICHK
      CALL DADAL(I_SPEC_DUMMY,1)
      ICHK = I_SPEC_DUMMY(1)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE PPUSH(MC,IC,XI,XF)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *****************************
*
*     THIS SUBROUTINE APPLIES THE MATRIX WHOSE TREE IS STORED IN CA VECTOR MC
*     TO THE COORDINATES IN XI AND STORES THE RESULT IN XF
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION MC(IC),XF(IC),XI(IC),XM(LNO+1)
      save
*
      DO 10 I=1,IC
  10  XF(I) = CC(IDAPO(MC(I)))
*
      XM(1) = ONE
*
      DO 100 I=1,IDALL(MC(1))-1
*
      JL = I1(IDAPO(MC(1))+I)
      JV = I2(IDAPO(MC(1))+I)
      XX = XM(JL)*XI(JV)
      XM(JL+1) = XX
*
      DO 100 IV=1,IC
      XF(IV) = XF(IV) + CC(IDAPO(MC(IV))+I) * XX
 100  CONTINUE
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE MATINV(A,AI,N,NMX,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     *********************************
*
*     THIS SUBROUTINE INVERTS THE MATRIX A AND STORES THE RESULT IN AI
*     INPUT  A   - SAVED
*            N   - ORDER OF MATRIX < 100
*     OUTPUT AI  - A INVERSE
*            IER - 0 NO ERROR
*                  132 ZERO DETERMINANT
*
      PARAMETER (NMAX=100)
      DIMENSION A(NMX,NMX),AI(NMX,NMX),AW(NMAX,NMAX),INDX(NMAX)
      save
  
      DO 12 I=1,N
         DO 11 J=1,N
            AW(I,J) = A(I,J)
11       AI(I,J) = 0.0
12    AI(I,I) = 1.0
  
      CALL LUDCMP(AW,N,NMAX,INDX,D,IER)
      IF (IER .EQ. 132) RETURN
      DO 13 J=1,N
13    CALL LUBKSB(AW,N,NMAX,INDX,AI(1,J),NMX)
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE LUDCMP(A,N,NP,INDX,D,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ************************************
*
*     THIS SUBROUTINE DECOMPOSES A MATRIX INTO LU FORMAT
*     INPUT A: NXN MATRIX - WILL BE OVERWRITTEN BY THE LU DECOMP.
*           NP: PHYSICAL DIMENSION OF A
*           INDX: ROW PERMUTATION VECTOR
*           D: EVEN OR ODD ROW INTERCHANGES
*
*     REFERENCE: NUMERICAL RECIPIES BY PRESS ET AL (CAMBRIDGE) PG. 35
*
      PARAMETER (NMAX = 100, TINY = 1.0E-20)
      DIMENSION A(NP,NP), INDX(NP), VV(NMAX)
      save
      IER=0.
      D=1.
      DO 12 I=1,N
         AAMAX=0.
         DO 11 J=1,N
            IF(ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
11       CONTINUE
         IF(AAMAX.EQ.0.) THEN
            IER=132
            RETURN
         ENDIF
         VV(I)=1./AAMAX
12    CONTINUE
      DO 19 J=1,N
         IF(J.GT.1) THEN
            DO 14 I=1,J-1
               SUM=A(I,J)
               IF(I.GT.1) THEN
                  DO 13 K=1,I-1
                     SUM=SUM-A(I,K)*A(K,J)
13                CONTINUE
                  A(I,J)=SUM
               ENDIF
14          CONTINUE
         ENDIF
         AAMAX=0.
         DO 16 I=J,N
            SUM=A(I,J)
            IF (J.GT.1) THEN
               DO 15 K=1,J-1
                  SUM=SUM-A(I,K)*A(K,J)
15             CONTINUE
               A(I,J)=SUM
            ENDIF
            DUM=VV(I)*ABS(SUM)
            IF(DUM.GE.AAMAX) THEN
               IMAX=I
               AAMAX=DUM
            ENDIF
16       CONTINUE
         IF (J.NE.IMAX) THEN
            DO 17 K=1,N
               DUM=A(IMAX,K)
               A(IMAX,K)=A(J,K)
               A(J,K)=DUM
17          CONTINUE
            D=-D
            VV(IMAX)=VV(J)
         ENDIF
         INDX(J)=IMAX
         IF(J.NE.N) THEN
            IF(A(J,J).EQ.0.) A(J,J)=TINY
            DUM=1./A(J,J)
            DO 18 I=J+1,N
               A(I,J)=A(I,J)*DUM
18          CONTINUE
         ENDIF
19    CONTINUE
      IF(A(N,N).EQ.0.) A(N,N)=TINY
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE LUBKSB(A,N,NP,INDX,B,NMX)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ************************************
*
*     THIS SUBROUTINE SOLVES SET OF LINEAR EQUATIONS AX=B,
*     INPUT A: NXN MATRIX IN LU FORM GIVEN BY LUDCMP
*           NP: PHYSICAL DIMENSION OF A
*           INDX: ROW PERMUTATION VECTOR
*           D: EVEN OR ODD ROW INTERCHANGES
*           B: RHS OF LINEAR EQUATION - WILL BE OVERWRITTEN BY X
*
*     REFERENCE: NUMERICAL RECIPIES BY PRESS ET AL (CAMBRIDGE) PG. 36
*
      DIMENSION A(NP,NP), INDX(NP), B(NMX)
      save
      II = 0
      DO 12 I=1,N
         LL = INDX(I)
         SUM = B(LL)
         B(LL) = B(I)
         IF(II.NE.0) THEN
            DO 11 J=II,I-1
               SUM = SUM-A(I,J)*B(J)
11          CONTINUE
         ELSE IF (SUM.NE.0.) THEN
            II = I
         ENDIF
         B(I)=SUM
12    CONTINUE
      DO 14 I=N,1,-1
         SUM=B(I)
         IF(I.LT.N) THEN
            DO 13 J=I+1,N
               SUM = SUM-A(I,J)*B(J)
13          CONTINUE
         ENDIF
         B(I)=SUM/A(I,I)
14    CONTINUE
      RETURN
      END
*
CETIENNE
C ANFANG UNTERPROGRAMM
      SUBROUTINE DATRA(IDIF,INA,INC)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ******************************
*
*     THIS SUBROUTINE COMPUTES THE DERIVATIVE WITH RESPECT TO VARIABLE I
*     OF THE VECTOR A AND STORES THE RESULT IN C.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
      save
*-----------------------------------------------------------------------------9
*
      CALL DAINF(INA,INOA,INVA,IPOA,ILMA,ILLA)
      CALL DAINF(INC,INOC,INVC,IPOC,ILMC,ILLC)
*
      IF(INVA.EQ.0.OR.INVC.EQ.0) THEN
         PRINT*,'ERROR, DADER CALLED WITH CA VECTOR'
C        CALL DADEB(111,'ERR DATRA1',1)
      ENDIF
*
      CALL DACHK(INA,INOA,INVA,0,-1,-1,INC,INOC,INVC)
*
      IBASE = NOMAX + 1
*
      IF(IDIF.GT.(NVMAX+1)/2) THEN
         IDER1  = 0
         IDER1S = 0
         IDER2  = IDIF-(NVMAX+1)/2
         IDER2S = 1
         DO 10 JJ=1,IDER2-1
  10     IDER2S = IDER2S*IBASE
         XDIVI  = IDER2S*IBASE
      ELSE
         IDER1  = IDIF
         IDER1S = 1
         DO 20 JJ=1,IDER1-1
  20     IDER1S = IDER1S*IBASE
         IDER2  = 0
         IDER2S = 0
         XDIVI  = IDER1S*IBASE
      ENDIF
*
      IBASE = NOMAX+1
*
      IC = IPOC-1
*
      DO 100 I=IPOA,IPOA+ILLA-1
*
      IF(IDER1.EQ.0) THEN
         IEE = I2(I)
      ELSE
         IEE = I1(I)
      ENDIF
*
      X = IEE/XDIVI
      IFAC = INT(IBASE*(X-INT(X)+C1M8))
*
      IF(IFAC.EQ.0) GOTO 100
*
      IC = IC + 1
      CC(IC) = CC(I)
      I1(IC) = I1(I) - IDER1S
      I2(IC) = I2(I) - IDER2S
*
 100  CONTINUE
*
      IDALL(INC) = IC - IPOC + 1
      IF(IDALL(INC).GT.IDALM(INC)) THEN
         PRINT*,'ERROR IN DATRA '
C        CALL DADEB(111,'ERR DATRA2',1)
      ENDIF
*
      RETURN
      END
C ANFANG UNTERPROGRAMM
      SUBROUTINE DASYM(MB,MBDP,MBP,MBD,IB,MC,IC,N, MA,IA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **************************************
*
*     THIS SUBROUTINE TRACKS THE COORDINATES IN CA VECTOR MC |N| TIMES
*     THROUGH THE MATRIX MB. THE RESULTING COORDINATES ARE STORED IN CA
*     VECTOR MA.
*     IF N IS NEGATIVE, SYMPLECTIC TRACKING WILL BE PERFORMED *** THIS OPTION
*     DOES NOT EXIST YET.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION MB(*),MC(*),MBB(LNV),MA(*),XI(LNV),XF(LNV)
C     DIMENSION ICC(LNV)
      DIMENSION MBD(*),MBP(*),MBDP(*),XTW(LNV),XCI(LNV),XD(9),DM(3,3)
      DIMENSION DMI(3,3),MBDB(LNV),MBPB(LNV),MBDPB(LNV)
C     DIMENSION XHA(LNV)
      DIMENSION II_SPEC_DUMMY(1)
      DIMENSION I_SPEC_DUMMY(1)
      COMMON /NEWTON/ EPSS,NITERA
      save
*
      IF(IDALM(MC(1))*ABS(N).GT.IDALM(MA(1))) THEN
         PRINT*,'ERROR IN ROUTINE DASYM, IA.LT.IC*|N|'
C        CALL DADEB(31,'ERR DASYM1',1)
      ENDIF
*
      IBB=IB**2/4
      CALL DAALL(MBB,IB,'$$DATRK $$',NOMAX,NVMAX)
      CALL DAALL(MBPB,IB,'$ETIENNE $',NOMAX,NVMAX)
      CALL DAALL(MBDB,IBB,'$FOREST  $',NOMAX,NVMAX)
      CALL DAALL(MBDPB,IB,'$FOREST1 $',NOMAX,NVMAX)
*
      CALL MTREE(MB,IB,MBB,IB)
      CALL MTREE(MBD,IBB,MBDB,IBB)
      CALL MTREE(MBP,IB,MBPB,IB)
      CALL MTREE(MBDP,IB,MBDPB,IB)
*
      IC2=IB/2
      DO 1000 I=1,3
 1000 DM(I,I)=ONE
      DO 100 I=1,IDALL(MC(1))
*
      II_SPEC_DUMMY(1)=(I-1)*N+1
      I_SPEC_DUMMY(1) = I
      DO 50 J=1,IC
      CALL DAPEK(MC(J), I_SPEC_DUMMY,       XI(J))
      CALL DAPOK(MA(J),II_SPEC_DUMMY,XI(J))
  50  CONTINUE
  
      DO 100 K=2,ABS(N)
*
CETIENNE
      XET=ZERO
      DO 101 IET=1,IB
      XET=ABS(XI(IET))+XET
 101  CONTINUE
      IF(XET.GT.C1E3) THEN
      N=K
      RETURN
      ENDIF
CETIENNE
      CALL PPUSH(MBB,IB,XI,XF)
C      WRITE(11,*)1,(XI(IU),IU=1,IC)
C      WRITE(11,*)1,(XF(IU),IU=1,IC)
*
      DO 1 IE=1,IC2
      XTW(2*IE-1)=XI(2*IE-1)
 1    XTW(2*IE)=XF(2*IE)
      DO 11 IE=IB+1,IC
 11     XTW(IE)=XI(IE)
  
      DO 667 IT=1,NITERA
      CALL PPUSH(MBPB,IB,XTW,XCI)
      CALL PPUSH(MBDB,IBB,XTW,XD)
C      WRITE(11,*)2,(XTW(IU),IU=1,IC)
C      WRITE(11,*)2,(XCI(IU),IU=1,IC)
C      WRITE(11,*)2,(XD(IU),IU=1,IBB)
      IG=1
      DO 2 IE=1,IC2
      XCI(2*IE)=XCI(2*IE)-XI(2*IE)
      DO 3 IF=1,IC2
      DM(IE,IF)=XD(IG)
 3    IG=IG+1
 2    CONTINUE
      CALL MATINV(DM,DMI,3,3,IER)
      DO 4 IE=1,IC2
      DO 5 IF=1,IC2
      IEE=2*IE
      IFE=2*IF
      XTW(IEE)=XTW(IEE)+DMI(IE,IF)*XCI(IFE)
 5    CONTINUE
 4    CONTINUE
C      WRITE(11,*)3,(XTW(IU),IU=1,IC)
      XET=ZERO
      DO 102 IET=1,IC2
      XET=ABS(XTW(2*IET)-XF(2*IET))+XET
 102  CONTINUE
       IF(XET.LT.EPSS) THEN
C      WRITE(4,*) IT,XET
        GOTO 666
       ELSE
        DO 45 IE=1,IC2
 45     XF(2*IE)=XTW(2*IE)
      DO 46 IE=IB+1,IC
 46     XF(IE)=XTW(IE)
       ENDIF
 667  CONTINUE
      WRITE(4,*) K,IT,XET
 666  CONTINUE
        CALL PPUSH(MBDPB,IB,XTW,XF)
C      WRITE(6,*) IT,XET
C      WRITE(6,*) (XF(IU),IU=1,IC)
C      WRITE(4,*) (XF(IU),IU=1,IC)
*
      II_SPEC_DUMMY(1)=(I-1)*N+K
      DO 60 J1=1,IC2
      J=2*J1-1
      JJ=2*J1
      XI(J) = XF(J)
      XI(JJ) = XTW(JJ)
      CALL DAPOK(MA(J),II_SPEC_DUMMY,XI(J))
      CALL DAPOK(MA(JJ),II_SPEC_DUMMY,XI(JJ))
  60  CONTINUE
      DO 61 J1=IB+1,IA
      XI(J1) = XF(J1)
      CALL DAPOK(MA(J1),II_SPEC_DUMMY,XI(J1))
  61  CONTINUE
*
 100  CONTINUE
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DATRKE(MB,IB,MC,IC,N, MA,IA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **************************************
*
*     THIS SUBROUTINE TRACKS THE COORDINATES IN CA VECTOR MC |N| TIMES
*     THROUGH THE MATRIX MB. THE RESULTING COORDINATES ARE STORED IN CA
*     VECTOR MA.
*     IF N IS NEGATIVE, SYMPLECTIC TRACKING WILL BE PERFORMED *** THIS OPTION
*     DOES NOT EXIST YET.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION MB(*),MC(*),MBB(LNV),MA(*),XI(LNV),XF(LNV)
C     DIMENSION ICC(LNV)
      DIMENSION II_SPEC_DUMMY(1)
      DIMENSION I_SPEC_DUMMY(1)
      save
*
      IF(IDALM(MC(1))*ABS(N).GT.IDALM(MA(1))) THEN
         PRINT*,'ERROR IN ROUTINE DATRKE, IA.LT.IC*|N|'
C        CALL DADEB(31,'ERR DATRKE',1)
      ENDIF
*
      CALL DAALL(MBB,IB,'$$DATRK $$',NOMAX,NVMAX)
*
      CALL MTREE(MB,IB,MBB,IB)
*
      DO 100 I=1,IDALL(MC(1))
*
      II_SPEC_DUMMY(1)= (I-1)*N+1
      I_SPEC_DUMMY(1) = I
      DO 50 J=1,IC
      CALL DAPEK(MC(J), I_SPEC_DUMMY,       XI(J))
      CALL DAPOK(MA(J),II_SPEC_DUMMY,XI(J))
  50  CONTINUE
  
      DO 100 K=2,ABS(N)
*
CETIENNE
      XET=ZERO
      DO 101 IET=1,IC
      XET=ABS(XI(IET))+XET
 101  CONTINUE
      IF(XET.GT.C1E3) THEN
      N=K
      RETURN
      ENDIF
CETIENNE
      CALL PPUSH(MBB,IB,XI,XF)
*
      IF(N.LT.0) THEN
      ENDIF
*
      II_SPEC_DUMMY(1)= (I-1)*N+K
      DO 60 J=1,IC
      XI(J) = XF(J)
      CALL DAPOK(MA(J),II_SPEC_DUMMY,XI(J))
  60  CONTINUE
*
 100  CONTINUE
*
      RETURN
      END
CETIENNE
C ANFANG UNTERPROGRAMM
      SUBROUTINE DASYMF(LIN,MB,MBDP,MBP,MBD,IB,MC,IC,N, MA,IA)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     **************************************
*
*     THIS SUBROUTINE TRACKS THE COORDINATES IN CA VECTOR MC |N| TIMES
*     THROUGH THE MATRIX MB. THE RESULTING COORDINATES ARE STORED IN CA
*     VECTOR MA.
*     IF N IS NEGATIVE, SYMPLECTIC TRACKING WILL BE PERFORMED *** THIS OPTION
*     DOES NOT EXIST YET.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
*
      DIMENSION MB(*),MC(*),MBB(LNV),MA(*),XI(LNV),XF(LNV)
C     DIMENSION ICC(LNV)
      DIMENSION MBD(*),MBP(*),MBDP(*),XTW(LNV),XCI(LNV),XD(9),DM(3,3)
      DIMENSION DMI(3,3),MBDB(LNV),MBPB(LNV),MBDPB(LNV),LIN(*)
C     DIMENSION XHA(LNV)
      DIMENSION MLIN(LNV)
      DIMENSION II_SPEC_DUMMY(1)
      DIMENSION I_SPEC_DUMMY(1)
      COMMON /NEWTON/ EPSS,NITERA
      save
*
      IF(IDALM(MC(1))*ABS(N).GT.IDALM(MA(1))) THEN
         PRINT*,'ERROR IN ROUTINE DASYMF, IA.LT.IC*|N|'
C        CALL DADEB(31,'ERR DASYMF',1)
      ENDIF
*
      IBB=IB**2/4
      CALL DAALL(MBB,IB,'$$DATRK $$',NOMAX,NVMAX)
      CALL DAALL(MBPB,IB,'$ETIENNE $',NOMAX,NVMAX)
      CALL DAALL(MBDB,IBB,'$FOREST  $',NOMAX,NVMAX)
      CALL DAALL(MBDPB,IB,'$FOREST1 $',NOMAX,NVMAX)
      CALL DAALL(MLIN,IB, '$NERI  1 $',NOMAX,NVMAX)
*
      CALL MTREE(MB,IB,MBB,IB)
      CALL MTREE(MBD,IBB,MBDB,IBB)
      CALL MTREE(MBP,IB,MBPB,IB)
      CALL MTREE(MBDP,IB,MBDPB,IB)
      CALL MTREE(LIN,IB,MLIN,IB)
*
      IC2=IB/2
      DO 1000 I=1,3
 1000 DM(I,I)=ONE
      DO 100 I=1,IDALL(MC(1))
*
      II_SPEC_DUMMY(1)= (I-1)*N+1
      I_SPEC_DUMMY(1) = I
      DO 50 J=1,IC
      CALL DAPEK(MC(J), I_SPEC_DUMMY,       XI(J))
      CALL DAPOK(MA(J),II_SPEC_DUMMY,XI(J))
  50  CONTINUE
  
      DO 100 K=2,ABS(N)
*
CETIENNE
      XET=ZERO
      DO 101 IET=1,IB
      XET=ABS(XI(IET))+XET
 101  CONTINUE
      IF(XET.GT.C1E3) THEN
      N=K
      RETURN
      ENDIF
CETIENNE
      CALL PPUSH(MLIN,IB,XI,XF)
      DO 676 II=1,IB
 676  XI(II)=XF(II)
      CALL PPUSH(MBB,IB,XI,XF)
C      WRITE(11,*)1,(XI(IU),IU=1,IC)
C      WRITE(11,*)1,(XF(IU),IU=1,IC)
*
      DO 1 IE=1,IC2
      XTW(2*IE-1)=XI(2*IE-1)
 1    XTW(2*IE)=XF(2*IE)
      DO 11 IE=IB+1,IC
 11     XTW(IE)=XI(IE)
  
      DO 667 IT=1,NITERA
      CALL PPUSH(MBPB,IB,XTW,XCI)
      CALL PPUSH(MBDB,IBB,XTW,XD)
C      WRITE(11,*)2,(XTW(IU),IU=1,IC)
C      WRITE(11,*)2,(XCI(IU),IU=1,IC)
C      WRITE(11,*)2,(XD(IU),IU=1,IBB)
      IG=1
      DO 2 IE=1,IC2
      XCI(2*IE)=XCI(2*IE)-XI(2*IE)
      DO 3 IF=1,IC2
      DM(IE,IF)=XD(IG)
 3    IG=IG+1
 2    CONTINUE
      CALL MATINV(DM,DMI,3,3,IER)
      DO 4 IE=1,IC2
      DO 5 IF=1,IC2
      IEE=2*IE
      IFE=2*IF
      XTW(IEE)=XTW(IEE)+DMI(IE,IF)*XCI(IFE)
 5    CONTINUE
 4    CONTINUE
C      WRITE(11,*)3,(XTW(IU),IU=1,IC)
      XET=ZERO
      DO 102 IET=1,IC2
      XET=ABS(XTW(2*IET)-XF(2*IET))+XET
 102  CONTINUE
       IF(XET.LT.EPSS) THEN
C      WRITE(4,*) IT,XET
        GOTO 666
       ELSE
        DO 45 IE=1,IC2
 45     XF(2*IE)=XTW(2*IE)
      DO 46 IE=IB+1,IC
 46     XF(IE)=XTW(IE)
       ENDIF
 667  CONTINUE
      WRITE(4,*) K,IT,XET
 666  CONTINUE
        CALL PPUSH(MBDPB,IB,XTW,XF)
C      WRITE(6,*) IT,XET
C      WRITE(6,*) (XF(IU),IU=1,IC)
C      WRITE(4,*) (XF(IU),IU=1,IC)
*
      II_SPEC_DUMMY(1)= (I-1)*N+K
      DO 60 J1=1,IC2
      J=2*J1-1
      JJ=2*J1
      XI(J) = XF(J)
      XI(JJ) = XTW(JJ)
      CALL DAPOK(MA(J),II_SPEC_DUMMY,XI(J))
      CALL DAPOK(MA(JJ),II_SPEC_DUMMY,XI(JJ))
  60  CONTINUE
      DO 61 J1=IB+1,IA
      XI(J1) = XF(J1)
      CALL DAPOK(MA(J1),II_SPEC_DUMMY,XI(J1))
  61  CONTINUE
*
 100  CONTINUE
*
      RETURN
      END
*
C ANFANG UNTERPROGRAMM
      SUBROUTINE DAREAT(INA,IUNIT,NVORI)
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER(PIENI=1D-17)
      PARAMETER(ZERO=0.0D0,HALF=5.0D-1,ONE=1.0D0)
      PARAMETER(HALFM=-5.0D-1,ONEM=-1.0D0)
      PARAMETER(TWO=2.0D0,THREE=3.0D0,FOUR=4.0D0)
      PARAMETER(C1E3=1.0D3,C2E3=2.0D3,C4E3=4.0D3,C1E4=1.0D4)
      PARAMETER(C1E12=1.0D12,C1E13=1.0D13,C1E15=1.0D15,C1E16=1.0D16)
      PARAMETER(C1M1=1.0D-1,C1M3=1.0D-3,C1M6=1.0D-6,C1M7=1.0D-7)
      PARAMETER(C1M8=1.0D-8,C1M9=1.0D-9,C1M10=1.0D-10)
      PARAMETER(C1M12=1.0D-12,C1M13=1.0D-13,C1M14=1.0D-14)
      PARAMETER(C1M15=1.0D-15,C1M17=1.0D-17,C1M18=1.0D-18)
      PARAMETER(C1M21=1.0D-21,C1M24=1.0D-24,C1M38=1.0D-38)
      PARAMETER(FIVE=5.0D0,SIX=6.0D0,SEVEN=7.0D0,EIGHT=8.0D0)
      PARAMETER(NINE=9.0D0,TEN=10.0D0)
      PARAMETER(C24E0=24.0D0,C120E0=120.0D0,C16E0=16.0D0,C40E0=40.0D0)
      PARAMETER(C80E0=80.0D0,C72E0=72.0D0)
      PARAMETER(C12E0=12.0D0,C32E0=32.0D0,C48E0=48.0D0,C160E0=160.0D0)
      PARAMETER(ISZ=200)
*     ***************************
*
*     THIS SUBROUTINE READS THE DA VECTOR INA FROM UNIT IUNIT.
*
*-----------------------------------------------------------------------------1
      PARAMETER(LDA=10000,LST=100000,LEA=500,
     &LIA=500,LNO=100,LNV=20)
      COMMON / DA / CC(LST),I1(LST),I2(LST),
     &      IE1(LEA),IE2(LEA),IEO(LEA),IA1(0:LIA),IA2(0:LIA),IFI(LEA),
     &      IDANO(LDA),IDANV(LDA),IDAPO(LDA),IDALM(LDA),IDALL(LDA),
     &      NDA,NST,NOMAX,NVMAX,NMMAX,NOCUT,EPS,EPSMAC,LFI
*-----------------------------------------------------------------------------9
      CHARACTER DANAME(LDA)*10                                                1
      COMMON / DANAME / DANAME                                                2
*-----------------------------------------------------------------------------3
*
      CHARACTER C10*10
      DIMENSION J(LNV),JR(LNV,20)
      save
*
      IF(INA.LT.1.OR.INA.GT.NDA) THEN
         PRINT*,'ERROR IN DAPRI, INA = ',INA
C         X = SQRT(-ONE)
C        PRINT*,X
         STOP
      ENDIF
*
      INOA = IDANO(INA)
      INVA = IDANV(INA)
      IPOA = IDAPO(INA)
      ILMA = IDALM(INA)
      ILLA = IDALL(INA)
      REWIND 4
      READ(4,*) NN
      DO 22 I=1,NN
      READ(4,*) (JR(JJ,I),JJ=1,NVORI-INVA)
 22   CONTINUE
*
      DO 5 I=1,LNV
   5  J(I) = 0
*
      CALL DACLR(1)
*
      IC = 0
*
      IWARNO = 0
      IWARNV = 0
      IWARIN = 0
CETIENNE
      IET=0
CETIENNE
*
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(A10)') C10
      READ(IUNIT,'(A10)') C10
*
      IIN = 0
*
  10  CONTINUE
      IIN = IIN + 1
      READ(IUNIT,'(I6,2X,G20.14,I5,4X,18(2I2,1X))')
     *             II,C,IO,(J(I),I=1,NVORI)
*
      IF(II.EQ.0) GOTO 20
CETIENNE
      READ(IUNIT,*) C
CETIENNE
      IO1 = 0
      DO 15 I=1,INVA
  15  IO1 = IO1 + J(I)
      IF(IO1.GT.INOA) GOTO 10
      DO 56 I=1,NN
      DO 55 IE=INVA+1,NVORI
      IF(J(IE).NE.JR(IE-INVA,I)) GOTO 56
 55   CONTINUE
      GOTO 57
 56   CONTINUE
  
      GOTO 10
 57   CONTINUE
      DO 45  I=INVA+1,NVORI
 45   J(I)=0
      IC = IC + 1
      CALL DADCD(J,II1,II2)
      IC = IA1(II1) + IA2(II2)
      CC(IC) = C
      GOTO 10
*
  20  CONTINUE
*
      CALL DAPAC(INA,1)
*
      RETURN
      END
C
