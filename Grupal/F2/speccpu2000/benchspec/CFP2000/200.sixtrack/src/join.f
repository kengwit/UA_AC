C ANFANG UNTERPROGRAMM
C---------------------------------------------------------------------
      subroutine join
C---------------------------------------------------------------------
      implicit real*8 (a-h,o-z)
      parameter(npart=64,nmac=1)
      parameter(nele=700,nblo=300,nper=16,
     &nelb=100,nblz=20000,nzfz=300000,mmul=11)
      parameter(nran=280000,ncom=100,mran=500,mpa=6,nrco=5,nema=15)
      parameter(mcor=10)
      parameter(npos=20000,nlya=10000,ninv=1000,nplo=20000)
      parameter(nmon1=600,ncor1=600)
      parameter(pieni=1d-17)
      parameter(zero=0.0d0,half=0.5d0,one=1.0d0)
      parameter(two=2.0d0,three=3.0d0,four=4.0d0)
      parameter(c1e1=1.0d1,c1e2=1.0d2,c1m2=1.0d-2)
      parameter(c1e3=1.0d3,c2e3=2.0d3,c4e3=4.0d3,c1e4=1.0d4)
      parameter(c1e12=1.0d12,c1e13=1.0d13,c1e15=1.0d15,c1e16=1.0d16)
      parameter(c180e0=180.0d0,c1e6=1.0d6)
      parameter(c1m1=1.0d-1,c1m3=1.0d-3,c1m6=1.0d-6,c1m9=1.0d-9)
      parameter(c1m10=1.0d-10,c1m12=1.0d-12,c1m13=1.0d-13)
      parameter(c1m15=1.0d-15)
      parameter(c1m18=1.0d-18,c1m21=1.0d-21,c1m24=1.0d-24)
      parameter(c1m38=1.0d-38)
      parameter(pmap=938.2723128d0,pmae=.5109990615d0)
      parameter(crade=2.8179409238d-15)
      common/cor/dpscor,sigcor,icode,idam,its6d
      character*80 sixtit,commen
      character*8 cdate,ctime,progrm
      dimension bet0(2),alf0(2),ta(6,6)
      dimension qwc(3),clo(3),clop(3)
      dimension x(mpa,2),y(mpa,2),sigm(mpa),dps(mpa)
      dimension di0(2),dip0(2)
      save
C-----------------------------------------------------------------------
      pi=four*atan(one)
      sigcor=one
      dpscor=one
      read(90,end=60,iostat=ierro) sixtit,commen,cdate,ctime, progrm,
     +ifipa,ilapa,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo(1),clop(1),
     +clo(2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0(2),dip0(2),
     +dummy,dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta(1,5),ta(1,6), ta
     +(2,1),ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6), ta(3,1),ta(3,2),ta
     +(3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta(4,2),ta(4,3),ta(4,4),ta
     +(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),ta(5,4),ta(5,5),ta(5,6), ta
     +(6,1),ta(6,2),ta(6,3),ta(6,4),ta(6,5),ta(6,6)
      if(ierro.gt.0) then
        write(6,10010) 90,ierro
        goto 70
      endif
C-----------------------------------------------------------------------
C  OPTICAL PARAMETERS AT THE STARTING POINT
C-----------------------------------------------------------------------
      bet0(1)=ta(1,1)*ta(1,1)+ta(1,2)*ta(1,2)
      alf0(1)=-(ta(1,1)*ta(2,1)+ta(1,2)*ta(2,2))
      bet0(2)=ta(3,3)*ta(3,3)+ta(3,4)*ta(3,4)
      alf0(2)=-(ta(3,3)*ta(4,3)+ta(3,4)*ta(4,4))
      rewind 90
      ihalf=itopa/2
      if(icode.eq.1.or.icode.eq.2.or.icode.eq.4) idam=1
      if(icode.eq.3.or.icode.eq.5.or.icode.eq.6) idam=2
      if(icode.eq.7) idam=3
      do 50 i=1,ihalf
        read(91-i,end=50,iostat=ierro)
        if(ierro.gt.0) then
          write(6,10010) 91-i,ierro
          goto 50
        endif
        read(91-i-ihalf,end=50,iostat=ierro)
        if(ierro.gt.0) then
          write(6,10010) 91-i-ihalf,ierro
          goto 50
        endif
   10   read(91-i,end=20,iostat=ierro) ia,ipa,dummy, x(1,1),y(1,1),x
     +  (1,2),y(1,2),sigm(1),dps(1),e0
        if(ierro.gt.0) then
          write(6,10010) 91-i,ierro
          goto 20
        endif
        x(1,1)=x(1,1)*c1e3
        y(1,1)=y(1,1)*c1e3
        x(1,2)=x(1,2)*c1e3
        y(1,2)=y(1,2)*c1e3
        sigm(1)=sigm(1)*c1e3
        e0=e0*c1e3
        read(91-i-ihalf,end=20,iostat=ierro) idummy,idummy,dummy, x
     +  (2,1),y(2,1),x(2,2),y(2,2),sigm(2),dps(2)
        if(ierro.gt.0) then
          write(6,10010) 91-i-ihalf,ierro
          goto 20
        endif
        x(2,1)=x(2,1)*c1e3
        y(2,1)=y(2,1)*c1e3
        x(2,2)=x(2,2)*c1e3
        y(2,2)=y(2,2)*c1e3
        sigm(2)=sigm(2)*c1e3
        write(90,iostat=ierro) ia,ipa,dummy, x(1,1),y(1,1),x(1,2),y
     +  (1,2),sigm(1),dps(1),e0, ipa+1,dam,x(2,1),y(2,1),x(2,2),y(2,2),
     +  sigm(2),dps(2),e0
        if(ierro.ne.0) then
          write(6,10010) 90,ierro
          goto 20
        endif
        goto 10
   20   rewind 91-i
        rewind 91-i-ihalf
        write(91-i-ihalf,iostat=ierro)
        if(ierro.ne.0) then
          write(6,10010) 91-i-ihalf,ierro
        endif
        close(91-i-ihalf)
        rewind 90
        read(91-i,iostat=ierro) sixtit,commen,cdate,ctime, progrm,ifipa,
     +  ilapa,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo(1),clop(1),clo
     +  (2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0(2),dip0(2),dummy,
     +  dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta(1,5),ta(1,6), ta(2,1),
     +  ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6), ta(3,1),ta(3,2),ta
     +  (3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta(4,2),ta(4,3),ta(4,4),
     +  ta(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),ta(5,4),ta(5,5),ta
     +  (5,6), ta(6,1),ta(6,2),ta(6,3),ta(6,4),ta(6,5),ta(6,6)
        if(ierro.gt.0) then
          write(6,10010) 91-i,ierro
          goto 40
        endif
        rewind 91-i
        progrm='MADTOSIX'
        write(91-i,iostat=ierro) sixtit,commen,cdate,ctime, progrm,2*i
     +  -1,2*i,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo(1),clop(1),clo
     +  (2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0(2),dip0(2),dummy,
     +  dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta(1,5),ta(1,6), ta(2,1),
     +  ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6), ta(3,1),ta(3,2),ta
     +  (3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta(4,2),ta(4,3),ta(4,4),
     +  ta(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),ta(5,4),ta(5,5),ta
     +  (5,6), ta(6,1),ta(6,2),ta(6,3),ta(6,4),ta(6,5),ta(6,6), zero,
     +  zero,zero,zero,sigcor,dpscor, zero,zero,zero,zero, zero,zero,
     +  zero,zero,zero,zero,zero,zero,zero,zero, zero,zero,zero,zero,
     +  zero,zero,zero,zero,zero,zero, zero,zero,zero,zero,zero,zero,
     +  zero,zero,zero,zero, zero,zero,zero,zero,zero,zero,zero,zero,
     +  zero,zero
        if(ierro.ne.0) then
          write(6,10010) 91-i,ierro
          goto 40
        endif
   30   read(90,end=40,iostat=ierro) ia,ipa,dam, x(1,1),y(1,1),x(1,2),y
     +  (1,2),sigm(1),dps(1),e0, ipa1,dam,x(2,1),y(2,1),x(2,2),y(2,2),
     +  sigm(2),dps(2),e0
        if(ierro.gt.0) then
          write(6,10010) 90,ierro
          goto 40
        endif
        write(91-i,iostat=ierro) ia,ipa,dam, x(1,1),y(1,1),x(1,2),y
     +  (1,2),sigm(1),dps(1),e0, ipa1,dam,x(2,1),y(2,1),x(2,2),y(2,2),
     +  sigm(2),dps(2),e0
        if(ierro.ne.0) then
          write(6,10010) 91-i,ierro
          goto 40
        endif
        goto 30
   40   rewind 90
        rewind 91-i
   50 continue
      close(90)
      goto 70
   60 continue
      write(6,10000) 90
   70 continue
C-----------------------------------------------------------------------
      return
10000 format(//10x,'** ERROR IN JOIN** ----- INPUT DATA EMPTY' ,
     +' (FILE : ',i2,') -----'//)
10010 format(//10x,'** ERROR IN JOIN** ----- PROBLEMS WITH DATA ' ,
     +'FILE : ',i2,' ----- ERROR CODE : ',i10//)
      end
