C %%%%%%   TMINIT %%%%%%

       Subroutine  TmInit (BT, NKX, NKY)
        Implicit real*8 (a-h, o-z)       
         Dimension BT(NKX,NKY)

          BT = 0.D0

C  ..... Heating from the side ......................

          BT(1,1) =  0.5D0
          BT(2,1) = -0.5D0

        Return
       End
