C %%%%%%% SysNS_L %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
c Linear terms of Navier-Stokes equation

      Subroutine  SysNSL (N, NX, NY)

        Use razmer
        Use Parameters
        Use NsLin
        Use ENS
        Use Ttime
        Use DumBilin

         Implicit  Real*8 (A-H,O-Z)                                         

         Integer, Allocatable, Dimension(:) :: IPIV

C ===================================================================
         Allocate ( IPIV(N), stat=jerr )
C ===================================================================

C **** Coefficients in the Navier-Stokes equation *******************     

         Do L=1,NX
          Do M=1,NY
           Do i=1,NX
            Do j=1,NY

               EnsTim(NY*(L-1) + M,NY*(i-1) + j) = 
     =                                   ( VXX(i,L) * VXY(j,M) + 
     +                                     VYX(i,L) * VYY(j,M) ) * W

               EnsLap(NY*(L-1) + M,NY*(i-1) + j) = 
     =                                   ( VXX(i,L) * VXY2(j,M) + 
     +                                     VYX(i,L) * VYY2(j,M) ) * W +
     +                                  ( VXX2(i,L) *  VXY(j,M) + 
     +                                    VYX2(i,L) * VYY(j,M)  ) / W

            End Do		
           End Do		
          End Do		
         End Do		

C **** Inverse the matrix *********************

         C a l l    Dgetrf (N, N, EnsTim(1:N,1:N), N, IPIV, info)
               If (info .NE. 0) then
                       Write (*,*) 'sysnsL(Dgetrf): info=',info
                       Stop
               End If

         C a l l     Dgetri (N, EnsTim(1:N,1:N), N, IPIV, POP, N, info)
               If (info .NE. 0) then
                       Write (*,*) 'sysnsL(Dgetri): info=',info
                       Stop
               End If

C ****  Multiplication by the inversed matrix ***********

         EnsLap(1:N,1:N) = MATMUL( EnsTim(1:N,1:N), EnsLap(1:N,1:N) )

         Deallocate ( IPIV, stat=jerr)
       Return
      End
