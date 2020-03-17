CDECK  ID>, HTRL.
      subroutine htrl(a,b,m,n,k,rho)
      implicit real*8 (a-h,o-z)
 
C*********************************************************************
C     Subroutine HTRL to make Householder transform                  *
C                                                                    *
C     Authors:     many                Date:  17.09.1989             *
C                                                                    *
C*********************************************************************
 
C     calculate residual orbit vector
      parameter (nmon1=600)
      parameter (ncor1=600)
      dimension a(nmon1,ncor1),b(nmon1),rho(3*ncor1)
      save
 
      do 10 i= 1,k,1
        b(i)= 0.0d0
   10 continue
 
      do 20 kk=1,k
        lv=m-k+kk
        kn=n+k-kk+1
        kl=k-kk+1
 
        beta=-1.0d0/(rho(kn)*a(kl,kl))
        call htbl(a,b,m,n,kl,beta)
   20 continue
 
      end
