C %%%%%%%%%%%%%%%%%%% Arhim %%%%%%%%%%%%%%%%%%%%%%                                  
   
C **** Calculation of <VYX,TX> **************************************
                                                                        
      Real*8 Function VTX (i, j, FBW, F1,F2,F3,F4, V1,V2)                   

       Use   razmer

       Implicit Real*8 (A-H,O-Z)                                        
                                                                        
        Dimension FBW(-1:mm,-1:mm), P(3)                                       
        Real*8, Dimension(0:mm) :: A, F1, F2, F3, F4, V1, V2                                                

C ============================================================================
                                                                        
        Do L=1,3                                                      
         j1 = J + L - 1                                                  
                                                                        
         P(L) = FBW(j1,i-1) + F1(i) * FBW(j1, i ) + 
     +                        F2(i) * FBW(j1,i+1) +      
     +                        F3(i) * FBW(j1,i+2) + 
     +                        F4(i) * FBW(j1,i+3)        
                                                                        
        End Do                                                       
                                                                        
        VTX = P(1) + V1(j)*P(2) + V2(j)*P(3)                              
                                                                        
       Return                                                            
      End                                                               
                                                                        
C **** Calculation of <VYY,TY> ********************************************
                                                                        
      Real*8 Function VTY (i, j, FF, B, G1,G2,G3,G4, T1,T2)                  

       Use   razmer

       Implicit Real*8 (A-H,O-Z)                                        
                                                                        
        Dimension FF(-1:mm,-1:mm), P(3)                                       
        Real*8, Dimension(0:mm) :: B, G1, G2, G3, G4, T1, T2                                                
                        
C ==========================================================================
                                                
          Do L=1,3                                                      
           j1 = J + L - 1                                                  

           P(L) = B(i) * FF(i,j1) + B(i+1) * G1(i) * FF(i+1,j1) + 
     +                              B(i+2) * G2(i) * FF(i+2,j1) +       
     +                              B(i+3) * G3(i) * FF(i+3,j1) +
     +                              B(i+4 )* G4(i) * FF(i+4,j1)       
          End Do

        VTY = P(1)+ T1(j)*P(2) + T2(j)*P(3)                                     
                                                                        
       Return                                                            
      End                                                               
                                                                       
C **** Calculation of <VYX,T> *************************************
                                                                       
      Real*8 Function YXT (i, L, FBW, F1, F2, F3, F4)                         

       Use   razmer

       Implicit Real*8 (A-H,O-Z)                                      
                                                                        
        Dimension FBW(-1:mm,-1:mm)                                       
        Real*8, Dimension(0:mm) :: F1, F2, F3, F4                                                

C ================================================================                    
                                                                        
         YXT = FBW(L,i-1) + F1(i) * FBW(L, i ) + 
     +                      F2(i) * FBW(L,i+1) +          
     +                      F3(i) * FBW(L,i+2) + 
     +                      F4(i) * FBW(L,i+3)            

       Return                                                            
      End                                                               
                                                                       
C **** Calculation of <VYY,T> ***************************************
                                                                       
      Real*8 Function YYT (i, L, FF, B, G1, G2, G3, G4)                        

       Use   razmer

       Implicit Real*8 (A-H,O-Z)                                      
                                                                        
        Dimension FF(-1:mm,-1:mm)                                        
        Real*8, Dimension(0:mm) :: B, G1, G2, G3, G4                                                
                                                    
C ======================================================================
                    
         YYT = B(i) * FF(i,L) + B(i+1) * G1(i) * FF(i+1,L) +
     +                          B(i+4) * G4(i) * FF(i+4,L) + 
     +                          B(i+2) * G2(i) * FF(i+2,L) +
     +                          B(i+3) * G3(i) * FF(i+3,L)  
                                                                        
       Return                                                            
      End                                                               
                                                                        
