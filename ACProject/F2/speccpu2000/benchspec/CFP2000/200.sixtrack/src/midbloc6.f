CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C ANFANG UNTERPROGRAMM
      subroutine midbloc6(c,a2,a2i,q)
      implicit real*8 (a-h,o-z)
      parameter(pieni=1d-17)
      parameter(zero=0.0d0,half=5.0d-1,one=1.0d0)
      parameter(halfm=-5.0d-1,onem=-1.0d0)
      parameter(two=2.0d0,three=3.0d0,four=4.0d0)
      parameter(c1e3=1.0d3,c2e3=2.0d3,c4e3=4.0d3,c1e4=1.0d4)
      parameter(c1e12=1.0d12,c1e13=1.0d13,c1e15=1.0d15,c1e16=1.0d16)
      parameter(c180e0=180.0d0,c1e6=1.0d6)
      parameter(c1m1=1.0d-1,c1m3=1.0d-3,c1m6=1.0d-6,c1m7=1.0d-7)
      parameter(c1m8=1.0d-8,c1m9=1.0d-9,c1m10=1.0d-10)
      parameter(c1m12=1.0d-12,c1m13=1.0d-13,c1m14=1.0d-14)
      parameter(c1m15=1.0d-15,c1m17=1.0d-17,c1m18=1.0d-18)
      parameter(c1m21=1.0d-21,c1m24=1.0d-24,c1m38=1.0d-38)
      parameter(five=5.0d0,six=6.0d0,seven=7.0d0,eight=8.0d0)
      parameter(nine=9.0d0,ten=10.0d0)
      parameter(c24e0=24.0d0,c120e0=120.0d0,c16e0=16.0d0,c40e0=40.0d0)
      parameter(c80e0=80.0d0,c72e0=72.0d0)
      parameter(c12e0=12.0d0,c32e0=32.0d0,c48e0=48.0d0,c160e0=160.0d0)
      parameter(pmap=938.2723128d0,pmae=.5109990615d0)
      parameter(crade=2.8179409238d-15)
      parameter (nt=12)
      parameter (ndim2=6)
      parameter (ndim=3)
      integer jx(20)
C      CHARACTER*10 C(*),A2(*),A2I(*)
      dimension cr(ndim2,ndim2),xj(ndim2,ndim2),q(*)
      dimension rr(ndim2),ri(ndim2),sa(ndim2,ndim2)
     @,sai(ndim2,ndim2),cm(ndim2,ndim2),w(ndim2,ndim2)
      dimension vr(ndim2,ndim2),vi(ndim2,ndim2),s1(ndim2,ndim2),p(ndim)
      dimension xq(6),qb(2),qc(2),ifl(6),iplane(3)
*DAE{ (NO,NV) C(NT),A2(NT),A2I(NT)
      integer c(nt),a2(nt),a2i(nt)
*      
      integer          iscrda
      real*8 rscrri
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
      save
*      
*      
      call daall(c,nt,'C         ',no,nv)
      call daall(a2,nt,'A2        ',no,nv)
      call daall(a2i,nt,'A2I       ',no,nv)
*DA }  
      icount1=0
      icount2=0
      x2pi=atan(one)*eight
  195 continue
      n1=0
      do 10 i=1,nt
   10 jx(i)=0
      do 30 i=1,ndim2
        do 20 j=1,ndim2
          jx(j)=1
          call dapek(c(i),jx,r)
          jx(j)=0
          cm(i,j)=r
          cr(j,i)=r
          xj(i,j)=zero
          s1(i,j)=zero
   20   continue
   30 continue
      do 40 i=1,ndim
        xj(2*i-1,2*i)=one
        xj(2*i,2*i-1)=-one
   40 continue
      do 60 i=1,ndim2
        do 50 j=1,ndim2
          sai(i,j)=zero
          w(i,j)=cm(i,j)
          sa(i,j)=zero
   50   continue
   60 continue
      call mul66(xj,w)
      call mul66(cr,w)
      write(6,*)'CHECK OF THE SYMPLECTIC CONDITION ON THE LINEAR PART'
      do 70 i=1,ndim2
        do 70 j=1,ndim2
        write(6,'(D15.7)') w(i,j)
   70 continue
      call eig66(cr,rr,ri,vr,vi)
      do 80 i=1,ndim2
        xq(i)=asin(ri(i))/x2pi
        write(6,'(I4, E15.7)') i,xq(i)
   80 continue
      i9=6
      qb(1)=q(1)/x2pi
      qb(2)=q(2)/x2pi
      qc(1)=q(1)
      qc(2)=q(2)
      write(6,*) 'SELECTING EIGENPLANES FOR NOMINAL TUNES '
      write(6,'(D15.8, D15.8)') qb(1),qb(2)
      if(qb(1).gt.0.5d0) qb(1)=qb(1)-0.5d0
      if(qb(2).gt.0.5d0) qb(2)=qb(2)-0.5d0
      if(i9.eq.6) then
        xp0= 1.d0
        xn0=-1.d0
        do 110 j=1,6
          ifl(j)=1
c          if(xq(j)) 90,100,100
          if(xq(j)) 90,190,100
   90     xn=xq(j)
          if(xn.gt.xn0) then
            xn0=xn
            kn=j
          endif
          go to 110
  100     xp=xq(j)
          if(xp.lt.xp0) then
            xp0=xp
            kp=j
          endif
  110   continue
        ifl(kp)=0
c       if(xq(kp).eq.0d0) kn=kp+1
        ifl(kn)=0
        iplane(3)=kp
        do 180 i=1,2
          k=1
          t0=1.d0
          do 170 j=1,6
            if(ifl(j)) 170,170,120
  120       if(xq(j)) 130,190,140
  130       if(qb(i)-0.5d0) 160,150,150
  140       if(qb(i)-0.5d0) 150,150,160
  150       continue
            tst=abs(abs(qb(i))-abs(xq(j)))
            if(tst.lt.t0) then
              t0=tst
              k=j
            endif
            go to 170
  160       continue
  170     continue
          iplane(i)=k
          ifl(k)=0
  180   continue
C     WRITE(6,*) 'EIGENPLANES ',IPLANE
  190   continue
        n=iplane(1)
        nn=iplane(2)
        n1=iplane(3)
        if(icount1.gt.0.or.icount2.gt.0) then
          n=nrep
          nn=nnrep
          n1=n1rep
        endif
C        write(6,*) 'EIGENPLANES :',n,nn,n1
        write(6,9911) n,nn,n1
9911    format('EIGENPLANES :',I8,I8,I8)
      endif
C     IF(I9.EQ.6) READ(5,*) N,NN,N1
      x=vr(1,n)*vi(2,n)-vr(2,n)*vi(1,n)+vr(3,n)*vi(4,n)-vr(4,n)*vi(3,n)
      if(i9.eq.6) x=x+vr(5,n)*vi(6,n)-vr(6,n)*vi(5,n)
      y=vr(1,nn)*vi(2,nn)-vr(2,nn)*vi(1,nn)+vr(3,nn)*vi(4,nn)-vr(4,nn)
     &*vi(3,nn)
      if(i9.eq.6)y=y+vr(5,nn)*vi(6,nn)-vr(6,nn)*vi(5,nn)
      if(i9.eq.6) then
        z=vr(1,n1)*vi(2,n1)-vr(2,n1)*vi(1,n1)+vr(3,n1)*vi(4,n1)-vr(4,n1)
     +  *vi(3,n1)+vr(5,n1)*vi(6,n1)-vr(6,n1)*vi(5,n1)
      endif
      xx=one
      yy=one
      zz=one
      if(x.lt.zero) xx=-one
      if(y.lt.zero) yy=-one
      if(z.lt.zero) zz=-one
      x=sqrt(abs(x))
      y=sqrt(abs(y))
      z=sqrt(abs(z))
      do 200 i=1,ndim2
        sai(1,i)=vr(i,n)*xx/x
        sai(2,i)=vi(i,n)/x
        sai(3,i)=vr(i,nn)*yy/y
        sai(4,i)=vi(i,nn)/y
CFRS   IF(I9.EQ.6) THEN
        sai(5,i)=vr(i,n1)*zz/z
        sai(6,i)=vi(i,n1)/z
CFRS   ELSE
CFRS      SAI(5,5)=ONE
CFRS      SAI(6,6)=ONE
CFRS   ENDIF
  200 continue
C ADJUST SA SUCH THAT SA(1,2)=0 AND SA(3,4)=0. (COURANT-SNYDER-EDWARDS-T
C PHASE ADVANCES)
      do 210 i=1,ndim
        p(i)=atan(-sai(2*i-1,2*i)/sai(2*i,2*i))
        s1(2*i-1,2*i-1)=cos(p(i))
        s1(2*i,2*i)=cos(p(i))
        s1(2*i-1,2*i)=sin(p(i))
  210 s1(2*i,2*i-1)=-sin(p(i))
      call mul66(s1,sai)
C ADJUST SA TO HAVE SA(1,1)>0 AND SA(3,3)>0 ROTATE BY PI IF NECESSARY.
      xd=one
      yd=one
      zd=one
      if(sai(1,1).lt.zero) xd=-one
      if(sai(3,3).lt.zero) yd=-one
      if(sai(5,5).lt.zero) zd=-one
      s1(1,1)=xd
      s1(1,2)=zero
      s1(2,1)=zero
      s1(2,2)=xd
      s1(3,3)=yd
      s1(3,4)=zero
      s1(4,3)=zero
      s1(4,4)=yd
      s1(5,5)=zd
      s1(5,6)=zero
      s1(6,5)=zero
      s1(6,6)=zd
      call mul66(s1,sai)
C SA IS NOW UNIQUELY AND UNAMBIGEOUSLY DETERMINED.
      do 250 i=1,ndim2
        do 240 j=1,ndim2
          do 230 k=1,ndim2
            do 220 l=1,ndim2
              sa(i,l)=-xj(i,j)*sai(k,j)*xj(k,l)+sa(i,l)
  220       continue
  230     continue
  240   continue
  250 continue
      call mul66(sai,cm)
      do 270 i=1,ndim2
        do 260 j=1,ndim2
  260   cr(i,j)=sa(i,j)
  270 continue
      call mul66(cm,cr)
      do 280 i=1,ndim
        q(i)=acos(cr(2*i-1,2*i-1))
C       q(i)=acos(cr(2*i-1,2*i-1))!/x2pi
        if(cr(2*i-1,2*i).lt.zero) q(i)=x2pi-q(i)
  280 continue
      qdiff10=abs(qc(1)-q(1))
      qdiff20=abs(qc(2)-q(2))
      qdiff12=abs(qc(1)-q(2))
      qdiff13=abs(qc(1)-q(3))
      qdiff23=abs(qc(2)-q(3))
      if(qdiff10.lt.qdiff13.and.qdiff20.lt.qdiff13) then
        if(qdiff10.gt.qdiff12) then
          if(icount1.eq.0) then
            write(6,*)
            write(6,*) 'WARNING: REORDER EIGENPLANES'
            write(6,*) 'WRONG IN HORIZONTAL PLANE'
            write(6,*)
            nrep=nn
            nnrep=n
            n1rep=n1
            icount1=icount1+1
            q(1)=qc(1)
            q(2)=qc(2)
            goto 195
          else
            write(6,*) 'ERROR: PROBLEM WITH THE TUNE EIGENPLANES'
            write(6,*) 'PROGRAM STOP'
            write(6,*)
            stop
          endif
        endif
      else
        if(qdiff10.gt.qdiff13) then
          if(icount2.eq.0) then
            write(6,*)
            write(6,*) 'WARNING: REORDER EIGENPLANES'
            write(6,*) 'WRONG IN LONGITUDINAL PLANE'
            write(6,*) 'REPLACE WITH HORIZONTAL'
            write(6,*)
            nrep=n1
            nnrep=nn
            n1rep=n
            icount2=icount2+1
            q(1)=qc(1)
            q(2)=qc(2)
            goto 195
          else
            write(6,*) 'ERROR: PROBLEM WITH THE TUNE EIGENPLANES'
            write(6,*) 'PROGRAM STOP'
            write(6,*)
            stop
          endif
        else if(qdiff20.gt.qdiff23.and.qdiff10.lt.qdiff13) then
          if(icount2.eq.0) then
            write(6,*)
            write(6,*) 'WARNING: REORDER EIGENPLANES'
            write(6,*) 'WRONG IN LONGITUDINAL PLANE'
            write(6,*) 'REPLACE WITH VERTICAL'
            write(6,*)
            nrep=n
            nnrep=n1
            n1rep=nn
            icount2=icount2+1
            q(1)=qc(1)
            q(2)=qc(2)
            goto 195
          else
            write(6,*) 'ERROR: PROBLEM WITH THE TUNE EIGENPLANES'
            write(6,*) 'PROGRAM STOP'
            write(6,*)
            stop
           endif
        else
          write(6,*) 'ERROR: PROBLEM WITH THE TUNE EIGENPLANES'
          write(6,*) 'PROGRAM STOP'
          write(6,*)
          stop
        endif
      endif
      call dacond6(a2,zero)
      call dacond6(a2i,zero)
      do 300 i=1,ndim2
        do 290 j=1,ndim2
          jx(j)=1
          r=sa(i,j)
          if(r.ne.zero)call dapok(a2(i),jx,r)
          jx(j)=1
          r=sai(i,j)
          if(r.ne.zero)call dapok(a2i(i),jx,r)
          jx(j)=0
  290   continue
  300 continue
      return
      end
