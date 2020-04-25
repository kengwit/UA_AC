CDECK  ID>, HTBL.
      subroutine htbl(a,b,m,n,k,beta)
 
      implicit real*8 (a-h,o-z)

C*********************************************************************
C     Subroutine HTBL to make Householder transform                  *
C                                                                    *
C     Authors:     many                Date:  17.09.1989             *
C                                                                    *
C*********************************************************************
 
C     Householder transform of vector B
      parameter (nmon1=600)
      parameter (ncor1=600)
      dimension a(nmon1,ncor1),b(nmon1)
      save
 
      h=0.0d0
 
      do 10 k1=k,m
   10 h=h+a(k1,k)*b(k1)
 
      h=beta*h
 
      do 20 k1=k,m
   20 b(k1)=b(k1)-a(k1,k)*h
 
      end
