CDECK  ID>, HTUL.
      subroutine htul(a,m,n,k,sig,beta)
      implicit real*8 (a-h,o-z)
 
C*********************************************************************
C     Subroutine HTUL to make Householder transform                  *
C                                                                    *
C     Authors:     many                Date:  17.09.1989             *
C                                                                    *
C*********************************************************************
 
C     calculate vector U
      parameter (nmon1=600)
      parameter (ncor1=600)
      dimension a(nmon1,ncor1)
      save
      sig=0.0d0
 
      do 10 i=k,m
        sig=sig+a(i,k)* a(i,k)
   10 continue
 
      sig=sqrt(sig)
C     on choisit le signe correct pour SIG:
      h=a(k,k)
      if(h.lt.0.0d0)sig=-sig
      beta=h + sig
      a(k,k)=beta
      beta=1.0d0/(sig*beta)
      end
