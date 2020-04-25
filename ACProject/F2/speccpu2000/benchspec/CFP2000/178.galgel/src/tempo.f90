C %%%%%%%%   TEMPO  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c   Inner products for energy equation                                             
                                                                        
C **** Calculation <Ti(X),Tj(X)> *********************                         
                                                                        
      Real*8 Function  PTXY ( i, j, FF, V1, V2)                               

        Use   razmer

        Implicit Real*8 (A-H,O-Z)                                         
                                                                        
          Dimension FF(-1:mm,-1:mm)                                        
          Dimension V1(0:mm),V2(0:mm)

C ==========================================================                                      
                                                                        
            P1 = FF(i, j ) + V1(i)*FF(i+1, j ) + V2(i)*FF(i+2 ,j )                    
            P2 = FF(i,j+1) + V1(i)*FF(i+1,j+1) + V2(i)*FF(i+2,j+1)              
            P3 = FF(i,j+2) + V1(i)*FF(i+1,j+2) + V2(i)*FF(i+2,j+2)              
                                                                        
            PTXY = P1 + V1(j)*P2 + V2(j)*P3                                   
                                                                        
       Return                                                            
      End                                                               
                                                                        
C **** Calculation  <VXX*T'X,T(X)> ************************                      
                                                                        
      Real*8 Function  VTXXYY (i,j,L,F1FF,A,F1,F2,F3,F4,V1,V2)           

        Use   razmer

         Implicit Real*8 (A-H,O-Z)                                        
                                                                        
       Dimension F1FF(0:mm,0:mm,0:mm), P(3), Q(3)                                   
       Real*8, Dimension(0:mm) :: A, F1, F2, F3, F4, V1, V2                                                

C ===========================================================
                                                                        
        Do K=1,3                                                       
          L1 = L + K - 1                                                   
                                                                        
          Do M=1,3                                                       
             J1 = J + M - 1                                                   
                                                                        
             P(M) = A(i) * F1FF(J1,i,L1) + 
     +                         A(i+1) * F1(i) * F1FF(J1,i+1,L1) +        
     +                         A(i+2) * F2(i) * F1FF(J1,i+2,L1) +
     +                         A(i+3) * F3(i) * F1FF(J1,i+3,L1) +  
     +                         A(i+4) * F4(i) * F1FF(J1,i+4,L1)                                   
          End Do                                                         
                                                                        
          Q(K) = P(1) + V1(j)*P(2) + V2(j)*P(3)                             
                                                                        
        End Do                                                         
                                                                        
        VTXXYY = Q(1) + V1(L)*Q(2) + V2(L)*Q(3)                          
                                                                        
       Return                                                            
      End                                                               
                                                                        

C **** Calculation  <VYX*TX,T(X)> *****************************                       
                                                                        
      Real*8 Function  VTXYYX (i,j,L,FWF,A,F1,F2,F3,F4,V1,V2)            

        Use razmer

         Implicit Real*8 (A-H,O-Z)                                        
                                                                        
          Dimension FWF(0:mm,-1:mm,0:mm), P(3), Q(3)                                   
          Real*8, Dimension(0:mm) :: A, F1, F2, F3, F4, V1, V2                                                

C ===============================================================
                                                                        
            Do K=1,3                                                      
               L1 = L + K - 1                                                  
                                                                        
               Do M=1,3                                                      
                  J1 = J + M - 1                                                  
                                                                        
                  P(M) = FWF(J1,I-1,L1) + F1(i) * FWF(J1, I ,L1) + 
     +                                    F2(i) * FWF(J1,i+1,L1) +
     +                                    F3(i) * FWF(J1,i+2,L1) +
     +                                    F4(i) * FWF(J1,i+3,L1)              
                End Do                                                         
                                                                        
                Q(K) = P(1) + V1(j)*P(2) + V2(j)*P(3)                             
                                                                        
            End Do                                                         
                                                                        
        VTXYYX = Q(1) + V1(L)*Q(2) + V2(L)*Q(3)                           
                                                                        
       Return                                                            
      End                                                               
