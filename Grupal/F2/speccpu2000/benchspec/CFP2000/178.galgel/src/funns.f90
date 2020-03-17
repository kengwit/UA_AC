C &&&&&&&&&&&&&&&&&  FUNNS %%%%%%%%%%%%%%%%%%%%%%%

       Subroutine FunNS  (NX, NY)

        Use   razmer
        Use   FNS

         Implicit Real*8 (A-H,O-Z)                                      

C ==================================================================

C ***** Calculation of coefficients in basis functions of velocity *********

      NM = Max0(NX,NY) + 3

        Do i=0,NM
          B(i)  =  BB(i)
          A(i)  =  AB(i)
          G1(i) = GG1(i)
          G2(i) = GG2(i)
          G3(i) = GG3(i)
          G4(i) = GG4(i)
          F1(i) = FF1(i)
          F2(i) = FF2(i)
          F3(i) = FF3(i)
          F4(i) = FF4(i)
        End Do

       Return
      End
