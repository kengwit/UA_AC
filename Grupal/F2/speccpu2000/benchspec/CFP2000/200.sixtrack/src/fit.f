      SUBROUTINE LFIT(X,Y,L,KEY,A,B,E)  
      implicit real*8 (a-h,o-z)
C       
C     TO FIT A STRAIGHT LINE    Y=A*X+B    TO L POINTS WITH ERROR E     
C     SEE MENZEL , FORMULAS OF PHYSICS P.116    
C     POINTS WITH Y=0 ARE IGNOERD IF KEY=0      
C     L IS NO. OF POINTS        
C       
      DIMENSION X(1),Y(1)       
      save
C       
C     CALCULATE SUMS    
      IF(L-2) 25,1,1    
    1 COUNT=0.0 
      SUMX=0.0  
      SUMY=0.0  
      SUMXY=0.0 
      SUMXX=0.0 
      SUMYY=0.0 
      DO 10 J=1,L       
      IF(Y(J).EQ.0..AND.KEY.EQ.0) GO TO 10      
      SUMX=SUMX+X(J)    
      SUMY=SUMY+Y(J)    
      COUNT=COUNT+1.0   
   10 CONTINUE  
      IF(COUNT.LE.1.) GO TO 25  
      YMED=SUMY/COUNT   
      XMED=SUMX/COUNT   
      DO 20 J=1,L       
      IF(Y(J).EQ.0..AND.KEY.EQ.0) GO TO 20      
      SCARTX=X(J)-XMED  
      SCARTY=Y(J)-YMED  
      SUMXY=SUMXY+SCARTX   *SCARTY      
      SUMXX=SUMXX+SCARTX   *SCARTX      
      SUMYY=SUMYY+SCARTY   *SCARTY      
   20 CONTINUE  
C       
C     FIT PARAMETERS    
      IF(SUMXX.EQ.0.) GO TO 25  
      A=SUMXY/SUMXX     
      B=YMED-A*XMED     
      IF(COUNT.LT.3.) GO TO 101 
      E=(SUMYY-SUMXY*A          )/(COUNT-2.0)   
      GO TO 100 
C       
C     ISUFFICIENT POINTS        
   25 A=0.0     
      B=0.0     
  101 E=0.0     
  100 RETURN    
      END       
      SUBROUTINE LFITW(X,Y,W,L,KEY,A,B,E)       
      implicit real*8 (a-h,o-z)
C       
C     TO PERFORM A WEIGHTED STRAIGHT LINE FIT   
C       
C     FOR FORMULAE USED SEE MENZEL, FORMULAS OF PHYSICS P.116   
C       
C     FIT IS OF Y=AX+B , WITH S**2 ESTIMATOR E. WEIGHTS ARE IN W.       
C     IF KEY=0, POINTS WITH Y=0 ARE IGNORED     
C     L IS NO. OF POINTS        
C       
      DIMENSION X(1),Y(1),W(1)  
      save
C       
C     CALCULTE SUMS     
      IF(L.LE.1) GO TO 1        
      W2=0.     
      W2X=0.    
      W2Y=0.    
      W2XY=0.   
      W2X2=0.   
      W2Y2=0.   
      ICNT=0    
      DO 2 J=1,L        
      IF(Y(J).EQ.0..AND.KEY.EQ.0) GO TO 2       
      WW=W(J)*W(J)      
      W2=WW+W2  
      WWF=WW*X(J)       
      W2X=WWF+W2X       
      W2X2=WWF*X(J)+W2X2        
      W2XY=WWF*Y(J)+W2XY        
      WWFI=WW*Y(J)      
      W2Y=WWFI+W2Y      
      W2Y2=WWFI*Y(J)+W2Y2       
      ICNT=ICNT+1       
    2 CONTINUE  
C       
C     FIT PARAMETERS    
      A=(W2XY-W2X*W2Y/W2)/(W2X2-W2X**2/W2)      
      B=(W2Y-A*W2X)/W2  
      IF(ICNT.LE.2) GO TO 3     
      E=(W2Y2-W2Y**2/W2-(W2XY-W2X*W2Y/W2)**2/(W2X2-W2X**2/W2))/(ICNT-2) 
      GO TO 4   
C       
C     ISUFFICIENT POINTS        
    1 A=0.      
      B=0.      
    3 E=0.      
    4 RETURN    
      END       
