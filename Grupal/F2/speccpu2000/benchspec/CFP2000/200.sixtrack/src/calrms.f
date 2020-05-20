C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
 
      subroutine calrms(r,m,rms,ptp)
 
      implicit real*8 (a-h,o-z)

C*********************************************************************
C     Subroutine CALRMS to calculate rms                             *
C                                                                    *
C     Authors:     many                Date:  17.09.1989             *
C                                                                    *
C*********************************************************************
 
C     calculates rms and p.to.p value of R(1) .... R(M)
      dimension r(m)
      save
      xave = 0.0
      xrms = 0.0
 
      do 10 i=1,m
        xave = xave + r(i)
        xrms = xrms + (r(i)*r(i))
   10 continue
 
      ave = xave / float(m)
      rms = xrms / float(m)
 
      imax=maxmin(r(1),m,1)
      imin=maxmin(r(1),m,0)
      ptp=r(imax)-r(imin)
      rms=sqrt(rms)
      return
      end
