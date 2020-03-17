C ############### ArhBt.f ##################################

C **** Calculation of <Vxx*T'(X),T(X)> **********************                   
                                                                        
      Real*8 Function VPXXYY (i, j, L, F1FF, A, F1,F2,F3,F4, V1,V2)           

       Use   razmer

       Implicit Real*8 (A-H,O-Z)                                        
                                                                        
        Dimension F1FF(0:mm,0:mm,0:mm)                                   
        Real*8, Dimension(0:mm) :: A, F1, F2, F3, F4, V1, V2                                                

C ======================================================================  
                                                                        
          P1 = A( I )         * F1FF(j, I ,L) + 
     +         A(i+1) * F1(i) * F1FF(j,i+1,L) +          
     +         A(i+2) * F2(i) * F1FF(j,i+2,L) + 
     +         A(i+3) * F3(i) * F1FF(j,i+3,L) +  
     +         A(i+4) * F4(i) * F1FF(j,i+4,L)                                 
                                                                        
          P2 = A( I )         * F1FF(j, I ,L+1) +
     +         A(i+1) * F1(i) * F1FF(j,i+1,L+1) +      
     +         A(i+2) * F2(i) * F1FF(j,i+2,L+1) + 
     +         A(i+3) * F3(i) * F1FF(j,i+3,L+1) + 
     +         A(i+4) * F4(i) * F1FF(j,i+4,L+1)                                
                                                                        
          P3 = A( I )         * F1FF(j, I ,L+2) + 
     +         A(i+1) * F1(i) * F1FF(j,i+1,L+2) +      
     +         A(i+2) * F2(i) * F1FF(j,i+2,L+2) + 
     +         A(i+3) * F3(i) * F1FF(j,i+3,L+2) + 
     +         A(i+4) * F4(i) * F1FF(j,i+4,L+2)                                
                                                                        
         VPXXYY = P1 + V1(L)*P2 + V2(L)*P3                                
                                                                        
       Return                                                            
      End                                                               
                                                                        
C **** Calculation of <Vyx*T(X),T(X)> *******************************
                                                                        
      Real*8 Function VPXYYX (i,j,L,FWF,A,F1,F2,F3,F4,V1,V2)            

       Use   razmer

       Implicit Real*8 (A-H,O-Z)                                        
                                                                        
        Dimension FWF(0:mm,-1:mm,0:mm)                                   
        Real*8, Dimension(0:mm) :: A, F1, F2, F3, F4, V1, V2                                                

C ========================================================================  
                                                                        
          P1 = FWF(j,i-1,L) + F1(i) * FWF(j, I ,L) + 
     +                        F2(i) * FWF(j,i+1,L) +     
     +                        F3(i) * FWF(j,i+2,L) + 
     +                        F4(i) * FWF(j,i+3,L)       
                                                                        
          P2 = FWF(j,i-1,L+1) + F1(i) * FWF(j, I ,L+1) +
     +                          F2(i) * FWF(j,i+1,L+1) + 
     +                          F3(i) * FWF(j,i+2,L+1) + 
     +                          F4(i) * FWF(j,i+3,L+1)   
                                                                        
          P3 = FWF(j,i-1,L+2) + F1(i) * FWF(j, I ,L+2) + 
     +                          F2(i) * FWF(j,i+1,L+2) + 
     +                          F3(i) * FWF(j,i+2,L+2) + 
     +                          F4(i) * FWF(j,i+3,L+2)   
                                                                        
        VPXYYX = P1 + V1(L)*P2 + V2(L)*P3                                
                                                                        
       Return                                                            
      End                                                               
                                                                        
C **** Calculation of <TXi,T(X)> ****************************** 
                                                                        
      Real*8 Function PPXY (i,j,FF,V1,V2)                               

       Use   razmer

       Implicit Real*8 (A-H,O-Z)                                         
                                                                        
        Dimension FF(-1:mm,-1:mm)                                        
        Dimension V1(0:mm),V2(0:mm)                                      

C =====================================================================
                                                                        
          P1 = FF(i, J )                                                         
          P2 = FF(i,j+1)                                                      
          P3 = FF(i,j+2)                                                      
                                                                        
         PPXY = P1 + V1(j)*P2 + V2(j)*P3                                   
                                                                        
      Return                                                            
      End                                                               
