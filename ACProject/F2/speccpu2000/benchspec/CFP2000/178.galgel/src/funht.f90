C %%%%%%%%%%%%%%  FUNHT  %%%%%%%%%%%%%%%%%%%%

        Subroutine  FunHT (NKX, NKY)

         Use   razmer
         Use   FHT

          Implicit  Real*8 (A-H,O-Z)                                      

C ================================================================

C ***** Coefficients of basis functions of the temperature ********

       NM = Max0(NKX, NKY) + 2

          Do i=0,NM
             V1(i) = VV1(i)
             V2(i) = VV2(i)
             T1(i) = TT1(i)
             T2(i) = TT2(i)
          End Do

        Return
       End
