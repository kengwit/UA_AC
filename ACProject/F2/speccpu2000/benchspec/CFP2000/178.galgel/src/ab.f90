C   ********* Function  AB *********                                          
                                                                        
        Real*8 Function AB (i)

          Use   Parameters

          Implicit Real*8 (a-h, o-z)

            If (i .NE. 0) then                                              
                               AB = W /(2.D0*dble(i)) 
                          else 
                               AB = 0.5D0 * W 
            End If                                                         
 
          Return
        End
        
C   ********* Function  BB *********                                          
                                                                        
       Real*8 FUNCTION BB (I)                                                    

         Implicit Real*8 (a-h, o-z)

           If (i .NE. 0)then                                              
                            BB = -1.D0 /(2.D0*dble(i))                                                      
                        else                                                            
                            BB = -0.5D0                                                          
           End If

         Return                                                            
      End                                                               
