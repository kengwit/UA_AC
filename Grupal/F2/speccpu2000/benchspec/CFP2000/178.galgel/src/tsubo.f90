C %%%%%%%   TSUBO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c  Linear inner products for Navier-Stokes equation                                          
                                                                        
C   **** Calculation of <Vx(i)*Vx(j)> *************************
                                                                        
      Real*8 Function  XXYY (i, j, FF, A, F1,F2,F3,F4,
     *                                    G1,G2,G3,G4 )       
        Use   razmer

        Implicit Real*8 (A-H,O-Z)                                      
                                                                        
         Dimension FF(-1:mm,-1:mm), P(5)                               
         Real*8, Dimension(0:mm) :: A, F1, F2, F3, F4
         Real*8, Dimension(0:mm) ::    G1, G2, G3, G4

C ==============================================================

          Do M=1,5                                                        
             J1 = J + (M-1)                                                    
                                                                        
             P(M) = A(i) * FF(i,J1) + A(i+1) * F1(i) * FF(i+1,J1)  +                   
     +                                A(i+2) * F2(i) * FF(i+2,J1)  + 
     +                                A(i+3) * F3(i) * FF(i+3,J1)  +        
     +                                A(i+4) * F4(i) * FF(i+4,J1)                                    
          End Do                                                          
                                                                        
          XXYY = A(j) * P(1)  +  A(j+1) * G1(j) * P(2)  + 
     +                           A(j+2) * G2(J )* P(3)  +        
     +                           A(j+3) * G3(j) * P(4)  + 
     +                           A(j+4) * G4(j) * P(5)          

       Return                                                            
      End                                                               
                                                                        

C  **** calculation of <Vxy(i)*Vxy(j)> ***********************
                                                                        
      Real*8 Function  XYYX (i, j, FF, B, F1,F2,F3,F4, 
     *                                    G1,G2,G3,G4  )       
        Use   razmer

        Implicit Real*8 (A-H,O-Z)                                         
                                                                        
         Dimension FF(-1:mm,-1:mm), P(5)                               
         Real*8, Dimension(0:mm) :: B, F1, F2, F3, F4
         Real*8, Dimension(0:mm) ::    G1, G2, G3, G4

C ===========================================================
                                                                        
          Do M=1,5                                                       
             J1=j+M-2                                                         
                                                                        
             P(M) = FF(I-1,J1)  +  F1(i) * FF( I ,J1)  + 
     +                             F2(i) * FF(i+1,J1)  +          
     +                             F3(i) * FF(i+2,J1)  + 
     +                             F4(i) * FF(i+3,J1)           
          End Do                                                         
                                                                        
          XYYX = P(1)  +  G1(j) * P(2)  +  G2(j) * P(3)  +
     +                    G3(j) * P(4)  +  G4(j) * P(5)          
                                                                        
       Return                                                            
      End                                                               
                                                                        

C  **** Calculation of <Vxy(i)*Vx(j)> *****************************
                                                                        
      Real*8 Function  XYXY (i, j, FF, B, F1,F2,F3,F4, 
     *                                    G1,G2,G3,G4  )       
       Use   razmer

        Implicit Real*8 (A-H,O-Z)                                         
                                                                       
         Dimension FF(-1:mm,-1:mm), P(5)                               
         Real*8, Dimension(0:mm) :: B, F1, F2, F3, F4
         Real*8, Dimension(0:mm) ::    G1, G2, G3, G4

C ==================================================================                                                   
                                                                        
          Do M=1,5                                                       
             J1=j+M-1                                                         
                                                                        
             P(M) = FF(I-1,J1)  +  F1(i) * FF( I ,J1)  + 
     +                             F2(i) * FF(i+1,J1)  +          
     +                             F3(i) * FF(i+2,J1)  + 
     +                             F4(i) * FF(i+3,J1)           
          End Do                                                         
                                                                        
          XYXY = P(1)  +  G1(j) * P(2)  +  G2(j) * P(3)  +
     +                    G3(j) * P(4)  +  G4(j) * P(5)          
                                                                        
       Return                                                            
      End                                                               
                                                                        

C  **** Calculation of <VX(i)*VXY(j)> ***********************
                                                                        
      Real*8 Function  YXYX (i, j, FF, B, F1,F2,F3,F4, 
     *                                    G1,G2,G3,G4   )       
       Use   razmer

       Implicit Real*8 (A-H,O-Z)                                         
                                                                        
        Dimension FF(-1:mm,-1:mm), P(5)                               
        Real*8, Dimension(0:mm) :: B, F1, F2, F3, F4
        Real*8, Dimension(0:mm) ::    G1, G2, G3, G4
 
C ============================================================
                                                                       
         Do M=1,5                                                       
            J1=j+M-2                                                         
                                                                        
            P(M) = FF(i,J1)  +  F1(i) * FF(i+1,J1)  + 
     +                          F2(i) * FF(i+2,J1)  +          
     +                          F3(i) * FF(i+3,J1)  + 
     +                          F4(i) * FF(i+4,J1)           
         End Do                                                         
                                                                        
         YXYX = P(1)  +  G1(j) * P(2)  +  G2(j) * P(3)  +
     +                   G3(j) * P(4)  +  G4(j) * P(5)          
                                                                         
       Return                                                            
      End                                                               

