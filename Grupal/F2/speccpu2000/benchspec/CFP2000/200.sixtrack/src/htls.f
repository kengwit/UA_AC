CDECK  ID>, HTLS.
      subroutine htls(a,b,m,n,x,ipiv,r,iter,rms,ptp)
 
      implicit real*8 (a-h,o-z)

C*********************************************************************
C     Subroutine HTLS to make Householder transform                  *
C                                                                    *
C     Authors:     many                Date:  17.09.1989             *
C                                                                    *
C*********************************************************************
 
      parameter (nmon1=600)
      parameter (ncor1=600)
 
C     DIMENSION OF ARRAY RHO SHOULD BE 3*NCOR1
C     M    - NUMBER OF AVAILABLE MONITORS
C     N    - NUMBERR OF AVAILABLE INDEPENDENT CORRECTORS
C     ITER - NUMBER OF CORRECTORS TO BE USED
C     RMS  - RMS VALUE TO CORRECT FOR
C     PTP  - PEAK TO PEAK VALUE TO CORRECT FOR
 
      dimension a(nmon1,ncor1),b(nmon1),x(ncor1),ipiv(ncor1),r(nmon1)
      dimension rho(3*ncor1),xiter(ncor1),xrms(ncor1),xptp(ncor1)
      dimension rmss(ncor1),ptop(ncor1)
      real*8    rms,ptp
      real*8    g,h,sig,beta
      save
 
C --- calcul du premier pivot
 
C============================
      beta=0.0
 
      do 10 ij1=1,500
   10 rho(ij1)=0.0
 
      k2=n + 1
      piv=0.0d0
 
      do 40 k=1,n
        ipiv(k)=k
        h=0.0d0
        g=0.0d0
        do 20 i=1,m
          h=h+a(i,k)*a(i,k)
          g=g+a(i,k)*b(i)
   20   continue
        rho(k)=h
        rho(k2) = g
        pivt = g*g/h
        if(pivt-piv)40,40,30
   30   piv = pivt
        kpiv=k
   40 k2 = k2 + 1
 
C --- boucle pour chaque iteration
 
      do 150 k=1,iter
        if(kpiv.eq.k)go to 60
 
C --- on echange les K et KPIV si KPIV plus grand que K
        h=rho(k)
        rho(k)=rho(kpiv)
        rho(kpiv)=h
        k2=n+k
        k3=n+kpiv
        g = rho(k2)
        rho(k2) = rho(k3)
        rho(k3) = g
        do 50 i=1,m
          h=a(i,k)
          a(i,k)=a(i,kpiv)
          a(i,kpiv)=h
   50   continue
 
C --- calcul de beta,sigma et uk dans htul
   60   continue
        call htul(a,m,n,k,sig,beta)
 
C --- on garde SIGMA dans RHO(N+K)
        j=n+k
        rho(j)=-sig
        ip=ipiv(kpiv)
        ipiv(kpiv)=ipiv(k)
        ipiv(k)=ip
        if(k.eq.n) go to 70
 
C --- transformation de A dans HTAL
        call htal(a,m,n,k,beta)
 
C --- transformation de B dans HTBL
   70   continue
        call htbl(a,b,m,n,k,beta)
 
C --- recherche du pivot (K+1)
C=============================
 
        rho(k)=sqrt(piv)
        if(k.eq.n) go to 90
        piv=0.0d0
        kpiv = k + 1
        j1 = kpiv
        k2=n + j1
        do 80 j=j1,n
          h=rho(j)-(a(k,j))*(a(k,j))
 
          if(h.lt.0.0000001) then
            write(6,*)
            write(6,*) 'CORRECTION PROCESS ABORTED.'
            write(6,*) 'DIVISION BY ZERO EXPECTED.'
            write(6,*) 'PROBABLY TWO CORRECTORS TOO CLOSE.'
            write(6,10000) ' SUSPECTED CORRECTOR: ',j
            stop 777
          endif
 
          rho(j)=h
          g=rho(k2)-(a(k,j))*(b(k))
          rho(k2) = g
          pivt = g*g/h
          if(pivt.lt.piv)go to 80
          kpiv=j
          piv=pivt
   80   k2 = k2 + 1
 
C --- calcul des X
   90   x(k)=b(k)/rho(n+k)
        if(k.eq.1)go to 120
        do 110 i=2,k
          kk=k-i+1
          x(kk)=b(kk)
          ki=kk+1
          do 100 j=ki,k
  100     x(kk)=x(kk)-a(kk,j)*x(j)
          x(kk)=x(kk)/rho(n+kk)
  110   continue
  120   continue
 
C --- save residual orbit and inverse sign of corrections (convention!)
        do 130 iii= 1,m
  130   r(iii) = b(iii)
        do 140 iii= 1,k
  140   x(iii) =-x(iii)
 
C --- calcul du vecteur residuel dans HTRL
C=========================================
 
C     transform orbit R back to "normal space"
        call htrl(a,r,m,n,k,rho)
        call calrms(r,m,rmss(k),ptop(k))
        xiter(k+1) = k
        xrms(k+1) = rmss(k)
        xptp(k+1) = ptop(k)
 
        if(ptop(k).le.ptp)go to 160
        if(rmss(k).le.rms)go to 160
  150 continue
      return
 
C --- correction is already good enough:
C=======================================
 
  160 ptp=ptop(k)
      rms=rmss(k)
10000 format(a,i4)
      end
