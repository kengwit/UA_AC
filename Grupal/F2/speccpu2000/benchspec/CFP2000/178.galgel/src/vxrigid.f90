
C %%%%%%    VxRigid  %%%%%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&

      Real*8 Function  FF1 (i)

        FF1 = 0.D0

       Return
      End

       Real*8 Function  FF2 (i)

        If (i .NE. 0) then
                          FF2 = -Dble(i)/Dble(i+2) -
     -                           Dble( (i+1)*((i+4)**2) ) /
     /                           Dble( i*(i+2)*(i+3) )
                      else
                          FF2 = -8.D0 / 3.D0
        End If
        
       Return
      End

       Real*8 Function  FF3 (i)

        FF3 = 0.D0

       Return
      End

       Real*8 Function  FF4 (i)
       
        If (i .NE. 0) then                                           
                          FF4 = Dble( (i+1)*(i+4) )/ Dble( I*(i+3) )
                      else
                          FF4 = 4.D0 / 3.D0
        End If

       Return
      End
