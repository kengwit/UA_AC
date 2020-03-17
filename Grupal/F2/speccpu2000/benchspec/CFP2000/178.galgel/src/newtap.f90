C %%%%%%%%%%%%%% NEWTAP  %%%%%%%%%%%%%%%%%%%
c
c  Driver for Newton steady-state solver


      Subroutine  EvNewt (NS, NX, NY, NKX, NKY, X, ier)

       Use razmer
       Use Parameters
!LAB
       use DynamParamM
!LAB

        Implicit   Real*8 (A-H,O-Z)

         Dimension  X(NS)

         External Diffun

         Real*8, Allocatable, Dimension(:) :: Xref

C ==========================================================
         Allocate ( Xref(NS), stat=jerr )
C ==========================================================

C  **** Definition of parameters for NWTN ********


         Xref(1:NS) = 1.D0

!LAB         Eps   = 1.D-08
!LAB         Nsig  = 6
!LAB         ItMax = 20
         Eps = DynamParam%Eps
         NSig = DynamParam%Nsig
         ItMax = DynamParam%ItMax
!LAB
         C a l l   Nwtn (Diffun, X, NS, Gr, Eps, Nsig,
     *                                    Xref,ItMax, ier)

          Deallocate ( Xref, stat=jerr)
       Return
      End
