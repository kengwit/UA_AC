CDECK  ID>, HTAL.
      subroutine htal(a,m,n,k,beta)
      implicit real*8 (a-h,o-z)
 
C*********************************************************************
C     Subroutine HTAL to make Householder transform                  *
C                                                                    *
C     Authors:     many                Date:  17.09.1989             *
C                                                                    *
C*********************************************************************
 
C     Householder transform of matrix A
      parameter (nmon1=600)
      parameter (ncor1=600)
      dimension a(nmon1,ncor1)
      save
 
      nc=n-k
 
      do 20 j=1,nc
        h=0.0d0
 
        do 10 k1=k,m
   10   h=h+a(k1,k)*a(k1,k+j)
 
        h=beta*h
        do 20 k1=k,m
   20 a(k1,k+j)=a(k1,k+j)-a(k1,k)*h
 
      end
