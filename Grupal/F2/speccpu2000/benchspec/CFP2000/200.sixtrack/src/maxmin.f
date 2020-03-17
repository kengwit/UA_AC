C ANFANG FUNCTION
C-----------------------------------------------------------------------
 
      function maxmin (a,n,m)
      implicit real*8 (a-h,o-z)
C     if M=0, MAXMIN=lowest index of minimum element in A
C     if M=1, MAXMIN=lowest index of maximun element in A
C     if N<1, MAXMIN=1
      dimension a(n)
      save
 
      maxmin=1
      if (n.lt.1) return
      curent=a(1)
      do 10 i=2,n
        if ((m.eq.0).and.(a(i).ge.curent)) go to 10
        if ((m.eq.1).and.(a(i).le.curent)) go to 10
        curent=a(i)
        maxmin=i
   10 continue
      return
      end
