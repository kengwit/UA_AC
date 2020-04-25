C        1         2         3         4         5         6         7
C23456789012345678901234567890123456789012345678901234567890123456789012



      SUBROUTINE MULDEO(U,N1,N2,N3,N4,X,RESULT)
C     =========================================

C --------------------------------------------------------------------
C
C     MULDEO MULTIPLIES MATRIX DEO BY VECTOR X
C     AND GIVES BACK VECTOR RESULT.
C
C --------------------------------------------------------------------


      IMPLICIT NONE

C     %-----------%
C     | ARGUMENTS |
C     %-----------%

      INTEGER         N1,N2,N3,N4

      COMPLEX*16      X(12,N1/2,N2,N3,N4)

      COMPLEX*16      RESULT(12,N1/2,N2,N3,N4)

      COMPLEX*16      U(3,3,4,N1,N2,N3,N4)

C     %------------%
C     | PARAMETERS |
C     %------------%

      COMPLEX*16      ONE
      PARAMETER      (ONE=(1.0D+0,0.0D+0))

C     %------------------------%
C     | LOCAL SCALARS & ARRAYS |
C     %------------------------%

      INTEGER      I,J,K,L
      INTEGER      IP,JP,KP,LP
      INTEGER      IM,JM,KM,LM

      COMPLEX*16   AUX1(12),AUX2(12),AUX3(12)

C     %----------------------%
C     | EXTERNAL SUBROUTINES |
C     %----------------------%

      EXTERNAL  GAMMUL,SU3MUL
      EXTERNAL  ZAXPY,ZCOPY

C     %-----------------------%
C     | EXECUTABLE STATEMENTS |
C     %-----------------------%

C     %----------------------%
C     | LOOP OVER EVEN SITES |
C     %----------------------%

      DO 100 L=1,N4

       LP=MOD(L,N4)+1

       DO 100 K=1,N3

        KP=MOD(K,N3)+1

        DO 100 J=1,N2

         JP=MOD(J,N2)+1

         DO 100 I=(MOD(J+K+L+1,2)+1),N1,2

           IP=MOD(I,N1)+1

           CALL GAMMUL(1,0,X(1,(IP+1)/2,J,K,L),AUX1)
           CALL SU3MUL(U(1,1,1,I,J,K,L),'N',AUX1,AUX3)

           CALL GAMMUL(2,0,X(1,(I+1)/2,JP,K,L),AUX1)
           CALL SU3MUL(U(1,1,2,I,J,K,L),'N',AUX1,AUX2)
           CALL ZAXPY(12,ONE,AUX2,1,AUX3,1)

           CALL GAMMUL(3,0,X(1,(I+1)/2,J,KP,L),AUX1)
           CALL SU3MUL(U(1,1,3,I,J,K,L),'N',AUX1,AUX2)
           CALL ZAXPY(12,ONE,AUX2,1,AUX3,1)

           CALL GAMMUL(4,0,X(1,(I+1)/2,J,K,LP),AUX1)
           CALL SU3MUL(U(1,1,4,I,J,K,L),'N',AUX1,AUX2)
           CALL ZAXPY(12,ONE,AUX2,1,AUX3,1)

           CALL ZCOPY(12,AUX3,1,RESULT(1,(I+1)/2,J,K,L),1)

 100  CONTINUE


      DO 200 L=1,N4

       LM=L-1
       IF(LM.EQ.0) LM=N4

       DO 200 K=1,N3

        KM=K-1
        IF(KM.EQ.0) KM=N3

        DO 200 J=1,N2

         JM=J-1
         IF(JM.EQ.0) JM=N2

         DO 200 I=(MOD(J+K+L+1,2)+1),N1,2

           IM=I-1
           IF(IM.EQ.0) IM=N1

           CALL GAMMUL(1,1,X(1,(IM+1)/2,J,K,L),AUX1)
           CALL SU3MUL(U(1,1,1,IM,J,K,L),'C',AUX1,AUX3)

           CALL GAMMUL(2,1,X(1,(I+1)/2,JM,K,L),AUX1)
           CALL SU3MUL(U(1,1,2,I,JM,K,L),'C',AUX1,AUX2)
           CALL ZAXPY(12,ONE,AUX2,1,AUX3,1)

           CALL GAMMUL(3,1,X(1,(I+1)/2,J,KM,L),AUX1)
           CALL SU3MUL(U(1,1,3,I,J,KM,L),'C',AUX1,AUX2)
           CALL ZAXPY(12,ONE,AUX2,1,AUX3,1)

           CALL GAMMUL(4,1,X(1,(I+1)/2,J,K,LM),AUX1)
           CALL SU3MUL(U(1,1,4,I,J,K,LM),'C',AUX1,AUX2)
           CALL ZAXPY(12,ONE,AUX2,1,AUX3,1)

           CALL ZAXPY(12,ONE,AUX3,1,RESULT(1,(I+1)/2,J,K,L),1)

 200  CONTINUE

C     %---------------%
C     | END OF MULDEO |
C     %---------------%

      RETURN
      END
