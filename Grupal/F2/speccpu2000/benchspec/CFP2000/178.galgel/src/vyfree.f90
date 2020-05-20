C %%%%%%    VyFree  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%                                            

      Real*8 Function  GG1 (I)                                                   

         If (i .EQ. 0) then
                            GG1 = 2.D0 / 7.D0
         Else
             i2 = i *i
             i3 = i2*i 
             ii = (i+1)*(i+1)

             GG1 = Dble(2 * ii) / Dble( i3 + 5*i2 + 7*i )
         End If                                                      

       Return                                                            
      End                                                               
                                                                        
      Real*8 Function  GG2 (I)
                                                   
         If (i .EQ. 0) then
                            GG2 = -16.D0 / 7.D0
         Else
             i2 = i *i
             i3 = i2*i 
             i4 = i3*i 

             GG2 = -2.D0 * Dble( i4 + 8*i3 + 26*i2 + 40*i + 24 ) /
     /                     Dble( i4 + 8*i3 + 22*i2 + 21*i      )
         End If                                                      

       Return                                                            
      End                                                               
                                                                        
      Real*8 Function  GG3 (I)                                                   

         If (i .EQ. 0) then
                            GG3 = -6.D0 / 7.D0
         Else
             i2 = i *i
             i3 = i2*i 

             GG3 = -2.D0 * Dble(        i2 + 4*i + 3 ) /
     /                     Dble( i3 + 5*i2 + 7*i     )
         End If                                                      

       Return                                                            
      End                                                               
                                                                        
      Real*8 Function  GG4 (I)                                                   

         If (i .EQ. 0) then
                            GG4 = 4.D0 / 7.D0
         Else
             i2 = i *i
             i3 = i2*i 
             i4 = i3*i 

             GG4 = Dble( i4 + 8*i3 + 22*i2 + 27*i + 12 ) /
     /             Dble( i4 + 8*i3 + 22*i2 + 21*i      )
         End If                                                      

       Return                                                            
      End                                                               
