C %%%%%%  TNELGO %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
c
c  Bilinear inner products for Navier-Stokes equation                                              
                                                                       
C **** Calculation of <Vxx*Vxx',Vxx> ********************           
                                                                       
      Real*8 Function  XXXYYY (i, j, L, F1FF, A, F1,F2,F3,F4,              
     *                                           G1,G2,G3,G4,              
     *                                           H1,H2,H3,H4)              
        Use razmer

        Implicit Real*8 (A-H,O-Z)                                      
                                                                        
         Dimension F1FF(0:mm,0:mm,0:mm), P(5), W(5)                           
         Real*8, Dimension(0:mm) :: A, F1, F2, F3, F4
         Real*8, Dimension(0:mm) ::    G1, G2, G3, G4
         Real*8, Dimension(0:mm) ::    H1, H2, H3, H4

C =============================================================                   
                                                                        
          Do M=1,5                                                        
             L1 = L + (M-1)                                                    
                                                                        
             Do K=1,5                                                        
                J1 = J + (K-1)                                                    
                                                                        
                  P(K) = A(i) * F1FF(i,J1,L1) + 
     +                          A(i+1) * F1(i) * F1FF(i+1,J1,L1) +        
     +                          A(i+2) * F2(i) * F1FF(i+2,J1,L1) + 
     +                          A(i+3) * F3(i) * F1FF(i+3,J1,L1) +  
     +                          A(i+4) * F4(i) * F1FF(i+4,J1,L1)                                   
             End Do                                                          
                                                                        
             W(M) = A(j) * P(1)  +  A(j+1) * G1(j) * P(2) +
     +                              A(j+2) * G2(j) * P(3) +        
     +                              A(j+3) * G3(j) * P(4) + 
     +                              A(j+4) * G4(j) * P(5)          
          End Do                                                          
                                                                        
          XXXYYY = A(L) * W(1)  +  A(L+1) * H1(L) * W(2) + 
     +                             A(L+2) * H2(L) * W(3) +      
     +                             A(L+3) * H3(L) * W(4) + 
     +                             A(L+4) * H4(L) * W(5)        
                                                                        
       Return                                                            
      End                                                               
                                                                        

C **** Calculation of <Vxx *Vyx,Vxx> **************************
                                                                        
       Real*8 Function  XYXYXY (i, j, L, FWF, A,  F1,F2,F3,F4,              
     *                                            G1,G2,G3,G4,              
     *                                            H1,H2,H3,H4)              
        Use   razmer

        Implicit Real*8 (A-H,O-Z)                                      
                                                                        
         Dimension FWF(0:mm,-1:mm,0:mm), P(5), W(5)                         
         Real*8, Dimension(0:mm) :: A, F1, F2, F3, F4
         Real*8, Dimension(0:mm) ::    G1, G2, G3, G4
         Real*8, Dimension(0:mm) ::    H1, H2, H3, H4

C ===============================================================                                              
                                                                        
          Do M=1,5                                                        
             L1 = L + (M-1)                                                    
                                                                        
             Do K=1,5                                                        
                J1 = J - 1 + (K-1)                                                
                                                                        
                P(K) = A(i)*FWF(i,J1,L1) + 
     +                            A(i+1) * F1(i) * FWF(i+1,J1,L1) +          
     +                            A(i+2) * F2(i) * FWF(i+2,J1,L1) + 
     +                            A(i+3) * F3(i) * FWF(i+3,J1,L1) +   
     +                            A(i+4) * F4(i) * FWF(i+4,J1,L1)                                   
             End Do                                                          
                                                                        
             W(M) = P(1)  +  G1(j) * P(2)  +  G2(j) * P(3) + 
     +                       G3(j) * P(4)  +  G4(J )* P(5)   
          End Do                                                          
                                                                        
          XYXYXY = A(L) * W(1)  +  A(L+1) * H1(L) * W(2) + 
     +                             A(L+2) * H2(L) * W(3) +      
     +                             A(L+3) * H3(L) * W(4) + 
     +                             A(L+4) * H4(L) * W(5)        
                                                                        
       Return                                                            
      End                                                               
                                                                        

C **** Calculation of <Vxy*Vxy,Vxy> ***************************
                                                                        
       Real*8 Function  XXYYYX (i, j, L, WWW, B,  F1,F2,F3,F4,              
     *                                            G1,G2,G3,G4,              
     *                                            H1,H2,H3,H4 )              
        Use   razmer

        Implicit Real*8 (A-H,O-Z)                                      
                                                                        
         Dimension WWW(-1:mm,-1:mm,-1:mm), P(5), Q(5)                        
         Real*8, Dimension(0:mm) :: B, F1, F2, F3, F4
         Real*8, Dimension(0:mm) ::    G1, G2, G3, G4
         Real*8, Dimension(0:mm) ::    H1, H2, H3, H4
                                                        
C ================================================================
                
          Do M=1,5                                                        
             L1=L+M-2                                                          
                                                                        
             Do K=1,5                                                        
                J1=j+K-2                                                          
                                                                        
                P(K) = WWW(I-1,J1,L1)  +  F1(i) * WWW( I ,J1,L1) + 
     +                                    F2(i) * WWW(i+1,J1,L1) +
     +                                    F3(i) * WWW(i+2,J1,L1) +
     +                                    F4(i) * WWW(i+3,J1,L1) 
             End Do                                                          
                                                                        
             Q(M) = P(1)  +  G1(j) * P(2)  +  G2(j) * P(3)  +
     +                       G3(j) * P(4)  +  G4(j) * P(5)             
          End Do                                                          
                                                                        
          XXYYYX = Q(1)  +  H1(L) * Q(2)  +  H2(L) * Q(3)  +
     +                      H3(L) * Q(4)  +  H4(L) * Q(5)         
                                                                        
       Return                                                            
      End                                                               
                                                                        

C **** Calculation of <Vxy'*Vyy,Vxy> *****************************
                                                                        
       Real*8 Function  XYYYXX (i, j, L, W1FW, B, F1,F2,F3,F4,              
     *                                            G1,G2,G3,G4,              
     *                                            H1,H2,H3,H4 )              
        Use   razmer

        Implicit Real*8 (A-H,O-Z)                                      
                                                                        
         Dimension W1FW(-1:mm,0:mm,-1:mm), P(5), Q(5)                        
         Real*8, Dimension(0:mm) :: B, F1, F2, F3, F4
         Real*8, Dimension(0:mm) ::    G1, G2, G3, G4
         Real*8, Dimension(0:mm) ::    H1, H2, H3, H4

C ==================================================================
                                                                        
          Do M=1,5                                                        
             L1=L+M-2                                                          
                                                                        
             Do K=1,5                                                        
                J1=j+K-1                                                          
                                                                        
                P(K) = W1FW(I-1,J1,L1)  +  F1(i) * W1FW( I ,J1,L1) +                    
     +                                     F2(i) * W1FW(i+1,J1,L1) + 
     +                                     F3(i) * W1FW(i+2,J1,L1) +    
     +                                     F4(i) * W1FW(i+3,J1,L1)      
             End Do                                                          
                                                                        
             Q(M) = B(j) * P(1)  +  B(j+1) * G1(j) * P(2)  + 
     +                              B(j+2) * G2(j) * P(3)  +        
     +                              B(j+3) * G3(j) * P(4)  + 
     +                              B(j+4) * G4(j) * P(5)          
           End Do                                                          
                                                                        
           XYYYXX = Q(1)  +  H1(L) * Q(2)  +  H2(L) * Q(3)  + 
     +                       H3(L) * Q(4)  +  H4(L) * Q(5)   
                                                                        
       Return                                                            
      End                                                               
