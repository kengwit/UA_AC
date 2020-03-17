C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine postpr(nfile)
C-----------------------------------------------------------------------
C  POST PROCESSING
C
C  NFILE   :  FILE UNIT
C
C-----------------------------------------------------------------------
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
      character*80 toptit,sixtit,commen
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh
      common/posti3/toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/postr2/nnumxv(npart)
      common/cor/dpscor,sigcor,icode,idam,its6d
      character*80 title(20),chxtit(20),chytit(20)
      character*8 cdate,ctime,progrm
      character*11 hvs
      real*8 dle,tle,wgh,fxs,fzs,slope,const,varlea
      common/phase/ phase(3,npos)
      common/invari/ dani(ninv+1)
      dimension tle(nlya),dle(nlya)
      dimension wgh(nlya),biav(nlya),slope(nlya),varlea(nlya)
      dimension xinv(ninv),invx(ninv),zinv(ninv),invz(ninv)
      dimension xxr(npos),xxi(npos),zzr(npos),zzi(npos),
     &          fxs(npos),fzs(npos)
      dimension bet0(3),alf0(3),t(6,6)
      dimension bet04(2),alf04(2)
      dimension pmin(30),pmax(30)
      integer rdummy(6)
      dimension sumda(60)
      dimension x(2,6),cloau(6),di0au(4)
      dimension qwc(3),clo(3),clop(3),di0(2),dip0(2)
      dimension ta(6,6),txyz(6),txyz2(6),xyzv(6),xyzv2(6),rbeta(6)
      save
C----------------------------------------------------------------------
C--TIME START
      pieni2=1d-8
      do 10 i=1,npos
        do 10 j=1,3
          phase(j,i)=zero
   10 continue
      do 20 i=1,2
        bet04(i)=zero
        alf04(i)=zero
        di0(i)=zero
        dip0(i)=zero
        di0au(i)=zero
        di0au(i+2)=zero
   20 continue
      do 30 i=1,3
        bet0(i)=zero
        alf0(i)=zero
        qwc(i)=zero
        clo(i)=zero
        clop(i)=zero
   30 continue
      do 40 i=1,ninv
        invx(i)=0
        invz(i)=0
        xinv(i)=zero
        zinv(i)=zero
        dani(i)=zero
   40 continue
      dani(ninv+1)=zero
      do 50 i=1,npos
        xxr(i)=zero
        xxi(i)=zero
        zzr(i)=zero
        zzi(i)=zero
        fxs(i)=zero
        fzs(i)=zero
   50 continue
      do 60 i=1,6
        txyz(i)=zero
        txyz2(i)=zero
        xyzv(i)=zero
        xyzv2(i)=zero
        rbeta(i)=zero
        cloau(i)=zero
        x(1,i)=zero
        x(2,i)=zero
   60 continue
      do 70 i=1,6
        do 70 j=1,6
          t(i,j)=zero
          ta(i,j)=zero
   70 continue
      do 80 i=1,30
        pmax(i)=zero
        pmin(i)=zero
   80 continue
      do 90 i=1,20
        title(i)=' '
        chxtit(i)=' '
        chytit(i)=' '
   90 continue
      do 100 i=1,nlya
        tle(i)=zero
        dle(i)=zero
        slope(i)=0.0
        varlea(i)=0.0
        wgh(i)=zero
        biav(i)=zero
  100 continue
      do 110 i=1,60
        sumda(i)=zero
  110 continue
      b0=zero
      nlost=0
      ntwin=1
      nfft=1
      do 120 j=1,npos
        if(nfft.gt.npos/2) goto 130
        nfft=nfft*2
  120 continue
  130 continue
C----------------------------------------------------------------------
C--READING HEADER
C----------------------------------------------------------------------
      rewind nfile
      ia=0
      read(nfile,end=510,iostat=ierro) sixtit,commen,cdate,ctime,
     +progrm,ifipa,ilapa,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo(1),
     +clop(1),clo(2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0(2),dip0
     +(2),dummy,dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta(1,5),ta(1,6),
     +ta(2,1),ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6), ta(3,1),ta(3,2),
     +ta(3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta(4,2),ta(4,3),ta(4,4),
     +ta(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),ta(5,4),ta(5,5),ta(5,6),
     +ta(6,1),ta(6,2),ta(6,3),ta(6,4),ta(6,5),ta(6,6), dmmac,dnms,dizu0,
     +dnumlr,sigcor,dpscor
      if(ierro.gt.0) then
        write(6,10320) nfile
        goto 550
      endif
      sumda(1)=numl
      if(icode.eq.1.or.icode.eq.2.or.icode.eq.4) idam=1
      if(icode.eq.3.or.icode.eq.5.or.icode.eq.6) idam=2
      if(icode.eq.7) idam=3
      if(ilapa.ne.ifipa) ntwin=2
      if(imad.eq.1.and.progrm.eq.'MAD') then
        imad=0
        rewind nfile
        call join
        read(nfile,end=520,iostat=ierro) sixtit,commen,cdate,ctime,
     +  progrm,ifipa,ilapa,itopa,icode,numl,qwc(1),qwc(2),qwc(3), clo
     +  (1),clop(1),clo(2),clop(2),clo(3),clop(3), di0(1),dip0(1),di0
     +  (2),dip0(2),dummy,dummy, ta(1,1),ta(1,2),ta(1,3),ta(1,4),ta
     +  (1,5),ta(1,6), ta(2,1),ta(2,2),ta(2,3),ta(2,4),ta(2,5),ta(2,6),
     +  ta(3,1),ta(3,2),ta(3,3),ta(3,4),ta(3,5),ta(3,6), ta(4,1),ta
     +  (4,2),ta(4,3),ta(4,4),ta(4,5),ta(4,6), ta(5,1),ta(5,2),ta(5,3),
     +  ta(5,4),ta(5,5),ta(5,6), ta(6,1),ta(6,2),ta(6,3),ta(6,4),ta
     +  (6,5),ta(6,6), dmmac,dnms,dizu0,dnumlr,sigcor,dpscor
        ta(1,6)=ta(1,6)*c1e3
        ta(2,6)=ta(2,6)*c1e3
        ta(3,6)=ta(3,6)*c1e3
        ta(4,6)=ta(4,6)*c1e3
        ta(5,6)=ta(5,6)*c1e3
        ta(6,1)=ta(6,1)*c1m3
        ta(6,2)=ta(6,2)*c1m3
        ta(6,3)=ta(6,3)*c1m3
        ta(6,4)=ta(6,4)*c1m3
        ta(6,5)=ta(6,5)*c1m3
        if(ierro.gt.0) then
          write(6,10320) nfile
          goto 550
        endif
      endif
C--PREVENT FAULTY POST-PROCESSING
      read(nfile,end=530,iostat=ierro) iaa
      if(ierro.gt.0) then
        write(6,10320) nfile
        goto 550
      endif
      read(nfile,end=600,iostat=ierro) iab
      if(ierro.gt.0) then
        write(6,10320) nfile
        goto 550
      endif
 600  if((numl+1)/iskip/(iab-iaa)/iav.gt.nlya) nstop=iav*nlya
      rewind nfile
      read(nfile)
      sumda(5)=ta(1,1)*ta(1,1)+ta(1,2)*ta(1,2)
      sumda(6)=ta(3,3)*ta(3,3)+ta(3,4)*ta(3,4)
      if(iconv.eq.1) then
        cma1=one
        cma2=one
        clo(1)=zero
        clo(2)=zero
        clo(3)=zero
        clop(1)=zero
        clop(2)=zero
        clop(3)=zero
        di0(1)=zero
        di0(2)=zero
        dip0(1)=zero
        dip0(2)=zero
        do 140 i=1,6
          do 140 j=1,6
            if(i.ne.j) then
              ta(i,j)=zero
            else
              ta(i,j)=one
            endif
  140   continue
      endif
      cloau(1)= clo(1)
      cloau(2)=clop(1)
      cloau(3)= clo(2)
      cloau(4)=clop(2)
      cloau(5)= clo(3)
      cloau(6)=clop(3)
      di0au(1)= di0(1)
      di0au(2)=dip0(1)
      di0au(3)= di0(2)
      di0au(4)=dip0(2)
      sigcor=cma2
      dpscor=cma1
      if(ifipa.eq.ilapa.and.ndafi.gt.itopa) ndafi=itopa
      if(ilapa-ifipa.eq.1.and.ndafi.gt.itopa/2) ndafi=itopa/2
C-----------------------------------------------------------------------
C  OPTICAL PARAMETERS AT THE STARTING POINT
C-----------------------------------------------------------------------
      bet0(1)=ta(1,1)*ta(1,1)+ta(1,2)*ta(1,2)
      bet0x2 =ta(1,3)*ta(1,3)+ta(1,4)*ta(1,4)
      bet0x3 =ta(1,5)*ta(1,5)+ta(1,6)*ta(1,6)
      gam0x1 =ta(2,1)*ta(2,1)+ta(2,2)*ta(2,2)
      gam0x2 =ta(2,3)*ta(2,3)+ta(2,4)*ta(2,4)
      gam0x3 =ta(2,5)*ta(2,5)+ta(2,6)*ta(2,6)
      alf0(1)=-(ta(1,1)*ta(2,1)+ta(1,2)*ta(2,2))
      alf0x2 =-(ta(1,3)*ta(2,3)+ta(1,4)*ta(2,4))
      alf0x3 =-(ta(1,5)*ta(2,5)+ta(1,6)*ta(2,6))
      bet0(2)=ta(3,3)*ta(3,3)+ta(3,4)*ta(3,4)
      bet0z2 =ta(3,1)*ta(3,1)+ta(3,2)*ta(3,2)
      bet0z3 =ta(3,5)*ta(3,5)+ta(3,6)*ta(3,6)
      gam0z1 =ta(4,3)*ta(4,3)+ta(4,4)*ta(4,4)
      gam0z2 =ta(4,1)*ta(4,1)+ta(4,2)*ta(4,2)
      gam0z3 =ta(4,5)*ta(4,5)+ta(4,6)*ta(4,6)
      alf0(2)=-(ta(3,3)*ta(4,3)+ta(3,4)*ta(4,4))
      alf0z2 =-(ta(3,1)*ta(4,1)+ta(3,2)*ta(4,2))
      alf0z3 =-(ta(3,5)*ta(4,5)+ta(3,6)*ta(4,6))
      bet0(3)=ta(5,5)*ta(5,5)+ta(5,6)*ta(5,6)
      bet0s2 =ta(5,1)*ta(5,1)+ta(5,2)*ta(5,2)
      bet0s3 =ta(5,3)*ta(5,3)+ta(5,4)*ta(5,4)
      gam0s1 =ta(6,5)*ta(6,5)+ta(6,6)*ta(6,6)
      gam0s2 =ta(6,1)*ta(6,1)+ta(6,2)*ta(6,2)
      gam0s3 =ta(6,3)*ta(6,3)+ta(6,4)*ta(6,4)
      alf0(3)=-(ta(5,5)*ta(6,5)+ta(5,6)*ta(6,6))
      alf0s2 =-(ta(5,1)*ta(6,1)+ta(5,2)*ta(6,2))
      alf0s3 =-(ta(5,3)*ta(6,3)+ta(5,4)*ta(6,4))
      bet04(1)=bet0(1)
      bet04(2)=bet0(2)
      alf04(1)=alf0(1)
      alf04(2)=alf0(2)
      if(bet0(1).le.pieni.or.bet0(2).le.pieni) then
        write(6,*) 'WARNING: BETA VALUES ARE ZERO'
        bet0(1)=zero
        bet0(2)=zero
      endif
      do 135 i=1,3
        ii=2*i
        rbeta(ii-1)=sqrt(bet0(i))
        rbeta(ii)=rbeta(ii-1)
        if(abs(rbeta(ii-1)).lt.pieni) rbeta(ii-1)=one
        if(abs(rbeta(ii)).lt.pieni) rbeta(ii)=one
  135 continue
C----------------------------------------------------------------------
C--SETTING UP OF THE PARAMETERS
C----------------------------------------------------------------------
C--HPLOT TITLES
      if(icode.eq.1) hvs(1:11)='Hor        '
      if(icode.eq.2) hvs(1:11)='    Ver    '
      if(icode.eq.3) hvs(1:11)='Hor Ver    '
      if(icode.eq.4) hvs(1:11)='        Syn'
      if(icode.eq.5) hvs(1:11)='Hor     Syn'
      if(icode.eq.6) hvs(1:11)='    Ver Syn'
      if(icode.eq.7) hvs(1:11)='Hor Ver Syn'
      toptit(2)(1:13)='Particle no. '
      write(toptit(2)(14:16),'(I3)') ifipa
      toptit(2)(17:30)=', Phase Space '
      write(toptit(2)(31:41),'(A11)') hvs
      toptit(2)(42:50)=', Dp/p = '
      toptit(2)(61:80)=' '
      toptit(3)(1:5)='Ax = '
      toptit(3)(16:22)=', Ay = '
      toptit(3)(33:80)=' '
      toptit(4)(1:5)='Qx = '
      write(toptit(4)(6:15),10010) qwc(1)
      toptit(4)(16:22)=', Qy = '
      write(toptit(4)(23:32),10010) qwc(2)
      toptit(4)(33:39)=', Qs = '
      write(toptit(4)(39:48),10010) qwc(3)
      toptit(4)(49:80)=' '
      title(1)='Normalized Distance of Phase Space D as a Function '
     &//'of Turn Number N'
      title(2)='Normalized Horizontal Phase Space Projection'
      title(3)='Normalized Vertical Phase Space Projection'
      title(4)='Physical Phase Space Projection'
      title(5)='Synchrotron Phase Space Projection'
      title(6)='Synchrotron versus Horizontal Phase Space '
     &//'Projection'
      title(7)='Synchrotron versus Vertical Phase Space '
     &//'Projection'
      title(8)='Energy E as a Function of Turn Number N'
      title(9)='Stroboscoped Normalized Horizontal Phase Space '
     &//'Projection'
      title(10)='Stroboscoped Normalized Vertical Phase Space '
     &//'Projection'
      title(11)='FFT Analysis of the X Coordinate'
      title(12)='FFT Analysis of the Y Coordinate'
      chxtit(1)='N'
      chytit(1)='D (PI rad)'
      chxtit(2)='X (mm)'
      chytit(2)='Normalized Slope of X (mm)'
      chxtit(3)='Y (mm)'
      chytit(3)='Normalized Slope of Y (MM)'
      chxtit(4)='X (mm)'
      chytit(4)='Y (mm)'
      chxtit(5)='Sigma (mm)'
      chytit(5)='Relative Momentum Deviation'
      chxtit(6)='X (mm)'
      chytit(6)='Relative Momentum Deviation'
      chxtit(7)='Y (mm)'
      chytit(7)='Relative Momentum Deviation'
      chxtit(8)='N'
      chytit(8)='E (MeV)'
      chxtit(9)='X (mm)'
      chytit(9)='Normalized Slope of X (mm)'
      chxtit(10)='Y (mm)'
      chytit(10)='Normalized Slope of Y (mm)'
      chxtit(11)='Horizontal Tune Qx'
      chytit(11)='Normalized FFT Signal'
      chxtit(12)='Vertical Tune Qy'
      chytit(12)='Normalized FFT Signal'
      if(idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0) then
        call hplsiz(15.d0,15.d0,' ')
        call hplset('VSIZ',.24d0)
        call hplset('ASIZ',.19d0)
        call hplset('XLAB',1.5d0)
        call hplset('YLAB',1.0d0)
        call hplset('GSIZ',.19d0)
      endif
      if(iav.lt.1) iav=1
      if(nprint.eq.1) then
        write(6,10040) sixtit,commen
        write(6,10050) progrm,ifipa,itopa,hvs,numl,
     +  bet0(1),bet0x2,bet0x3,
     +  bet0(2),bet0z2,bet0z3,bet0(3),bet0s2,bet0s3,
     +  alf0(1),alf0x2,alf0x3
        write(6,10060) alf0(2),alf0z2,alf0z3,alf0(3),alf0s2,alf0s3,
     +  gam0x1,gam0x2,gam0x3,
     +  gam0z1,gam0z2,gam0z3,gam0s1,gam0s2,gam0s3
        write(6,10061) 
     +  clo(1),clo(2),clo(3),clop(1),clop(2),clop(3),
     +  di0(1),di0(2),dip0(1),dip0(2),qwc(1),qwc(2),qwc(3)
        write(6,10070) iav,nstart,nstop,dphix,dphiz,iwg, qx0,qz0
        write(6,10080) ivox,ivoz,ires,dres,ifh,dfft
        write(6,10090) kwtype,itf/20,icr,idis,icow,istw,iffw
        write(6,10100) iskip,iconv,imad,cma1,cma2,nprint,ndafi
      endif
C--INITIALISATION
      tpi=8*atan(one)
      prec=c1m1
      i1=0
      i11=1
      tlo=zero
      i2=0
      ifp=0
      iwarx=0
      iwarz=0
      iwar6=0
      iapx=0
      iapz=0
      iap6=0
      ivo6=1
      qs0=zero
      armin0=1e9
      armin=armin0
      nivh=ninv/2
      finv=tpi/ninv
      dani(1)=zero
      dani(ninv+1)=tpi
      do 150 i=1,ninv-1
  150 dani(i+1)=i*finv
      dle1=zero
      bold=zero
      dle1c=zero
      const=0.0
      dphx=zero
      dphz=zero
      dph6=zero
      tphx=zero
      tphz=zero
      tph6=zero
      sdpx=zero
      sdpz=zero
      sdp6=zero
      evx=zero
      evz=zero
      evx2=zero
      evz2=zero
      evt=zero
      sevx=zero
      sevz=zero
      sevt=zero
      emax=zero
      emix=zero
      emaz=zero
      emiz=zero
      emag=zero
      emig=zero
      emxa=zero
      emza=zero
      emta=zero
      emxs=zero
      emzs=zero
      emts=zero
      nuex=0
      nuez=0
      nuix=0
      nuiz=0
      xing=zero
      zing=zero
      pinx=zero
      pinz=zero
      pixr=zero
      pizr=zero
C--INVERTING THE MATRIX OF THE GENERATING VECTORS
      do 160 i=1,6
        do 160 j=1,6
  160 t(i,j)=ta(j,i)
      if(abs(t(1,1)).le.pieni.and.abs(t(2,2)).le.pieni) then
        t(1,1)=one
        t(2,2)=one
      endif
      if(abs(t(3,3)).le.pieni.and.abs(t(4,4)).le.pieni) then
        t(3,3)=one
        t(4,4)=one
      endif
      if(abs(t(5,5)).le.pieni.and.abs(t(6,6)).le.pieni) then
        t(5,5)=one
        t(6,6)=one
      endif
      tasum=zero
      its6d=0
      do 170 i=1,6
        tasum=tasum+abs(t(i,5))+abs(t(i,6))
  170 continue
      do 180 i=1,4
        tasum=tasum+abs(t(5,i))+abs(t(6,i))
  180 continue
      tasum=tasum-two
      if(abs(tasum).ge.pieni) its6d=1
      call dinv(6,t,6,rdummy,nerror)
      if(nerror.eq.-1) then
        write(6,10290) nfile
        goto 550
      endif
C----------------------------------------------------------------------
C--FIND MINIMUM VALUE OF THE DISTANCE IN PHASESPACE
C----------------------------------------------------------------------
  190 ifipa=0
      if(ntwin.eq.1) read(nfile,end=200,iostat=ierro) ia,ifipa,b,c,d,e,
     +f,g,h,p
      if(ntwin.eq.2) read(nfile,end=200,iostat=ierro) ia,ifipa,b,c,d,e,
     +f,g,h,p, ilapa,b,c1,d1,e1,f1,g1,h1,p1
      if(ierro.gt.0) then
        write(6,10320) nfile
        goto 550
      endif
      if(ifipa.lt.1) goto 190
      if((ia-nstart).lt.zero) goto 190
      if(progrm.eq.'MAD') then
        c=c*c1e3
        d=d*c1e3
        e=e*c1e3
        f=f*c1e3
        h=h*c1e3
        p=p*c1e3
        if(ntwin.eq.2) then
          c1=c1*c1e3
          d1=d1*c1e3
          e1=e1*c1e3
          f1=f1*c1e3
          h1=h1*c1e3
          p1=p1*c1e3
        endif
      endif
      if(ntwin.eq.2) then
        x(1,1)=c
        x(1,2)=d
        x(1,3)=e
        x(1,4)=f
        x(1,5)=g
        x(1,6)=h
        x(2,1)=c1
        x(2,2)=d1
        x(2,3)=e1
        x(2,4)=f1
        x(2,5)=g1
        x(2,6)=h1
        call distance(x,cloau,di0au,t,b)
      endif
      if(nstop.gt.nstart.and.(ia-nstop).gt.zero) goto 200
      if(b.lt.b0.or.abs(b0).le.pieni) b0=b
      goto 190
  200 if(ia.le.0) goto 530
      rewind nfile
C----------------------------------------------------------------------
C--GET FIRST DATA POINT AS A REFERENCE
C----------------------------------------------------------------------
      read(nfile,iostat=ierro)
      if(ierro.gt.0) then
        write(6,10320) nfile
        goto 550
      endif
  210 ifipa=0
      if(ntwin.eq.1) read(nfile,end=530,iostat=ierro) ia,ifipa,b,c,d,e,
     +f,g,h,p
      if(ntwin.eq.2) read(nfile,end=530,iostat=ierro) ia,ifipa,b,c,d,e,
     +f,g,h,p, ilapa,b,c1,d1,e1,f1,g1,h1,p1
      if(ierro.gt.0) then
        write(6,10320) nfile
        goto 550
      endif
      if(ifipa.lt.1) goto 210
      if((ia-nstart).lt.0) goto 210
      if(progrm.eq.'MAD') then
        c=c*c1e3
        d=d*c1e3
        e=e*c1e3
        f=f*c1e3
        g=g*c1e3
        p=p*c1e3
        if(ntwin.eq.2) then
          c1=c1*c1e3
          d1=d1*c1e3
          e1=e1*c1e3
          f1=f1*c1e3
          g1=g1*c1e3
          p1=p1*c1e3
        endif
      endif
      dp1=h
      write(toptit(2)(51:60),10000) dp1-clop(3)
      if(nprint.eq.1.and.ia.eq.0) then
        write(6,*) 'INITIAL COORDINATES'
        write(6,*) '       X = ',c
        write(6,*) '      XP = ',d
        write(6,*) '       Z = ',e
        write(6,*) '      ZP = ',f
        write(6,*) '   SIGMA = ',g
        write(6,*) '    DP/P = ',h
        write(6,*) '  ENERGY = ',p
      endif
      if(nstop.gt.nstart.and.(ia-nstop).gt.0) goto 540
      ia=ia-nstart
C--LYAPUNOV
      if(ntwin.eq.2) then
        x(1,1)=c
        x(1,2)=d
        x(1,3)=e
        x(1,4)=f
        x(1,5)=g
        x(1,6)=h
        x(2,1)=c1
        x(2,2)=d1
        x(2,3)=e1
        x(2,4)=f1
        x(2,5)=g1
        x(2,6)=h1
        call distance(x,cloau,di0au,t,b)
      endif
C--KEEP THE FIRST TURN NUMBER : IA0
      ia0=ia
      xxr(1)=c
      xxi(1)=zero
      zzr(1)=e
      zzi(1)=zero
      c=c-clo(1)
      d=d-clop(1)
      e=e-clo(2)
      f=f-clop(2)
      g=g-clo(3)
      h=h-clop(3)
      c1=c1-clo(1)
      d1=d1-clop(1)
      e1=e1-clo(2)
      f1=f1-clop(2)
      g1=g1-clo(3)
      h1=h1-clop(3)
      if(icode.ge.4) then
        c=c-di0(1)*h
        d=d-dip0(1)*h
        e=e-di0(2)*h
        f=f-dip0(2)*h
        c1=c1-di0(1)*h
        d1=d1-dip0(1)*h
        e1=e1-di0(2)*h
        f1=f1-dip0(2)*h
      endif
C--EMITTANCES
      xp0=bet0(1)*d+alf0(1)*c
      zp0=bet0(2)*f+alf0(2)*e
      emx=(c*c+xp0*xp0)/bet0(1)
      emz=(e*e+zp0*zp0)/bet0(2)
      if(icode.ge.4.and.its6d.ne.0) then
        c=c+di0(1)*h
        d=d+dip0(1)*h
        e=e+di0(2)*h
        f=f+dip0(2)*h
        c1=c1+di0(1)*h
        d1=d1+dip0(1)*h
        e1=e1+di0(2)*h
        f1=f1+dip0(2)*h
      endif
      emt=emx+emz
      emax=emx
      emix=emx
      emxa=emx
      emaz=emz
      emiz=emz
      emza=emz
      emat=emt
      emit=emt
      emta=emt
      emx0=emx
      emz0=emz
C--COURANT SYNDER
      xyzv(1)=c
      xyzv(2)=d
      xyzv(3)=e
      xyzv(4)=f
      xyzv(5)=g
      xyzv(6)=h
C--CONVERT TO CANONICAL VARIABLES
      if(its6d.eq.1) then
        xyzv(2)=xyzv(2)*(one+xyzv(6)+clop(3))
        xyzv(4)=xyzv(4)*(one+xyzv(6)+clop(3))
      endif
      do 220 iq=1,6
        txyz(iq)=zero
        do 220 jq=1,6
          txyz(iq)=txyz(iq)+t(jq,iq)*xyzv(jq)
  220 continue
C--INITIAL COORDINATES
      if(nprint.eq.1.and.ia.eq.0) then
        write(6,*) 'DISTANCE = ',b
      endif
C--EMITTANCES WITH LINEAR COUPLING
      evx=txyz(1)*txyz(1)+txyz(2)*txyz(2)
      evz=txyz(3)*txyz(3)+txyz(4)*txyz(4)
      xyzv2(1)=c1
      xyzv2(2)=d1
      xyzv2(3)=e1
      xyzv2(4)=f1
      xyzv2(5)=g1
      xyzv2(6)=h1
C--CONVERT TO CANONICAL VARIABLES
      if(its6d.eq.1) then
        xyzv2(2)=xyzv2(2)*(one+xyzv2(6)+clop(3))
        xyzv2(4)=xyzv2(4)*(one+xyzv2(6)+clop(3))
      endif
      do 225 iq=1,6
        txyz2(iq)=zero
        do 225 jq=1,6
          txyz2(iq)=txyz2(iq)+t(jq,iq)*xyzv2(jq)
  225 continue
      evx2=txyz2(1)*txyz2(1)+txyz2(2)*txyz2(2)
      evz2=txyz2(3)*txyz2(3)+txyz2(4)*txyz2(4)
      write(toptit(3)(6:15),10010) sqrt(evx*bet0(1))+sqrt(evz*bet0x2)
      write(toptit(3)(23:32),10010) sqrt(evz*bet0(2))+sqrt(evx*bet0z2)
      if(its6d.eq.1) then
        emiii=txyz(5)*txyz(5)*cma2*cma2+txyz(6)*txyz(6)*cma1*cma1
      else
        emiii=zero
      endif
C--COURANT SYNDER CONT.
      do 230 iq=1,6
        txyz(iq)=txyz(iq)*rbeta(iq)
  230 continue
      c0=txyz(1)
      d0=txyz(2)
      e0=txyz(3)
      f0=txyz(4)
      g0=txyz(5)*cma2
      h0=txyz(6)*cma1
C--MIN MAX VALUES
      pmin(2)=b
      pmax(2)=b
      pmin(3)=c0
      pmax(3)=c0
      pmin(4)=d0
      pmax(4)=d0
      pmin(5)=e0
      pmax(5)=e0
      pmin(6)=f0
      pmax(6)=f0
      pmin(9)=g0
      pmax(9)=g0
      pmin(10)=h0
      pmax(10)=h0
      pmin(16)=p
      pmax(16)=p
C--EMITTANCES WITH LINEAR COUPLING CONT.
      emi=evx
      emii=evz
      angi=zero
      angii=zero
      angiii=zero
      if(abs(txyz(1)).gt.pieni.or.abs(txyz(2)).gt.pieni)
     &angi=atan2(txyz(2),txyz(1))
      if(abs(txyz(3)).gt.pieni.or.abs(txyz(4)).gt.pieni)
     &angii=atan2(txyz(4),txyz(3))
      if(abs(txyz(5)).gt.pieni.or.abs(txyz(6)).gt.pieni)
     &angiii=atan2(txyz(6)*cma1,txyz(5)*cma2)
      evt=evx+evz
      evxma=evx
      evzma=evz
      evtma=evt
      evxmi=evx
      evzmi=evz
      evtmi=evt
C--COORDINATE-ANGLE CONVERSION
      call caconv(dpx,d0,c0)
      call caconv(dpz,f0,e0)
      dpxp=tpi+dpx
      dpzp=tpi+dpz
C--INVARIANTS
      call cinvar(dpx,dphix,dpz,dpzp,nuex,emz,zinv,invz)
      call cinvar(dpz,dphiz,dpx,dpxp,nuez,emx,xinv,invx)
C----------------------------------------------------------------------
C--GET DATA POINTS
C----------------------------------------------------------------------
      iskc=0
  240 ifipa=0
      if(ntwin.eq.1) read(nfile,end=270,iostat=ierro) ia,ifipa,b,c,d,e,
     +f,g,h,p
      if(ntwin.eq.2) read(nfile,end=270,iostat=ierro) ia,ifipa,b,c,d,e,
     +f,g,h,p, ilapa,b,c1,d1,e1,f1,g1,h1,p1
      if(ierro.gt.0) then
        write(6,10320) nfile
        goto 550
      endif
      if(ifipa.lt.1) goto 240
      if(progrm.eq.'MAD') then
        c=c*c1e3
        d=d*c1e3
        e=e*c1e3
        f=f*c1e3
        g=g*c1e3
        p=p*c1e3
        if(ntwin.eq.2) then
          c1=c1*c1e3
          d1=d1*c1e3
          e1=e1*c1e3
          f1=f1*c1e3
          g1=g1*c1e3
          p1=p1*c1e3
        endif
      endif
C--LYAPUNOV
      if(ntwin.eq.2) then
        x(1,1)=c
        x(1,2)=d
        x(1,3)=e
        x(1,4)=f
        x(1,5)=g
        x(1,6)=h
        x(2,1)=c1
        x(2,2)=d1
        x(2,3)=e1
        x(2,4)=f1
        x(2,5)=g1
        x(2,6)=h1
        call distance(x,cloau,di0au,t,b)
      endif
      iskc=iskc+1
      if(mod(iskc,iskip).ne.0) goto 240
      if(nstop.gt.nstart.and.(ia-nstop).gt.0) goto 270
      i1=i1+1
      i11=i1+1
      if(i2.ge.nlya.and.i11.gt.nfft.and.iapx.gt.npos.and.iapz.gt.npos)
     +goto 270
      if(i11.le.nfft) then
        xxr(i11)=c
        xxi(i11)=zero
      endif
      if(i11.le.nfft) then
        zzr(i11)=e
        zzi(i11)=zero
      endif
      c=c-clo(1)
      d=d-clop(1)
      e=e-clo(2)
      f=f-clop(2)
      g=g-clo(3)
      h=h-clop(3)
      if(icode.ge.4) then
        c=c-di0(1)*h
        d=d-dip0(1)*h
        e=e-di0(2)*h
        f=f-dip0(2)*h
      endif
C--EMITTANCES
      xp=bet0(1)*d+alf0(1)*c
      zp=bet0(2)*f+alf0(2)*e
      emx=(c*c+xp*xp)/bet0(1)
      emz=(e*e+zp*zp)/bet0(2)
      if(icode.ge.4.and.its6d.ne.0) then
        c=c+di0(1)*h
        d=d+dip0(1)*h
        e=e+di0(2)*h
        f=f+dip0(2)*h
      endif
      emt=emx+emz
      emxa=emxa+emx
      emza=emza+emz
      emta=emta+emt
      emax=max(emx,emax)
      emix=min(emx,emix)
      emaz=max(emz,emaz)
      emiz=min(emz,emiz)
      emat=max(emt,emat)
      emit=min(emt,emit)
C--COURANT SYNDER
      xyzv(1)=c
      xyzv(2)=d
      xyzv(3)=e
      xyzv(4)=f
      xyzv(5)=g
      xyzv(6)=h
C--CONVERT TO CANONICAL VARIABLES
      if(its6d.eq.1) then
        xyzv(2)=xyzv(2)*(one+xyzv(6)+clop(3))
        xyzv(4)=xyzv(4)*(one+xyzv(6)+clop(3))
      endif
      do 250 iq=1,6
        txyz(iq)=zero
        do 250 jq=1,6
          txyz(iq)=txyz(iq)+t(jq,iq)*xyzv(jq)
  250 continue
C--EMITTANCES WITH LINEAR COUPLING
      evx1=txyz(1)*txyz(1)+txyz(2)*txyz(2)
      evz1=txyz(3)*txyz(3)+txyz(4)*txyz(4)
C--COURANT SYNDER CONT.
      do 260 iq=1,6
        txyz(iq)=txyz(iq)*rbeta(iq)
  260 continue
      c=txyz(1)
      d=txyz(2)
      e=txyz(3)
      f=txyz(4)
      g=txyz(5)*cma2
      h=txyz(6)*cma1
C--MIN MAX VALUES
      pmin(2)=min(pmin(2),b)
      pmax(2)=max(pmax(2),b)
      pmin(3)=min(pmin(3),c)
      pmax(3)=max(pmax(3),c)
      pmin(4)=min(pmin(4),d)
      pmax(4)=max(pmax(4),d)
      pmin(5)=min(pmin(5),e)
      pmax(5)=max(pmax(5),e)
      pmin(6)=min(pmin(6),f)
      pmax(6)=max(pmax(6),f)
      pmin(9)=min(pmin(9),g)
      pmax(9)=max(pmax(9),g)
      pmin(10)=min(pmin(10),h)
      pmax(10)=max(pmax(10),h)
      pmin(16)=min(pmin(16),p)
      pmax(16)=max(pmax(16),p)
C--ADDING (LOG OF) THE DISTANCES OF PHASE SPACE
      ia=ia-nstart
C--GET DIFFERENCE IN THE NUMBER OF TURNS PER DATA ENTRY : IDNT
      if(i1.eq.1) idnt=ia-ia0
      bold=bold+b
      b=b-b0
      dle1c=zero
      if(b.gt.zero) dle1c=log(b)
      if(b.lt.zero) dle1c=-log(-b)
      dle1=dle1+dle1c
C--EMITTANCES WITH LINEAR COUPLING CONT.
      evt1=evx1+evz1
      evxma=max(evx1,evxma)
      evzma=max(evz1,evzma)
      evtma=max(evt1,evtma)
      evxmi=min(evx1,evxmi)
      evzmi=min(evz1,evzmi)
      evtmi=min(evt1,evtmi)
      evx=evx+evx1
      evz=evz+evz1
      evt=evt+evt1
C--ADDING OF THE PHASE ADVANCES
      sx=c*d0-c0*d
      cx=c0*c+d*d0
      if(iapx.le.npos)
     &call cphase(1,dphx,sx,cx,qx0,ivox,iwarx,iapx)
      sz=e*f0-e0*f
      cz=e0*e+f*f0
      if(iapz.le.npos)
     &call cphase(2,dphz,sz,cz,qz0,ivoz,iwarz,iapz)
      s6=g*h0-g0*h
      c6=h0*h+g*g0
      if(iap6.le.npos)
     &call cphase(3,dph6,s6,c6,qs0,ivo6,iwar6,iap6)
C--AVERAGING AFTER IAV TURNS
      if(mod(i1,iav).eq.0) then
        if(i2.ge.nlya) goto 240
        i2=i2+1
        dle(i2)=dle1/iav
        if(ia.gt.0) then
          tle1=ia
          tle1=log(tle1)
          if(i2.gt.1) then
            biav(i2-1)=bold/iav
            if(i2.eq.2) biav(1)=biav(1)*half
            bold=zero
            tle(i2)=(tle1+tlo)*half
            if(abs(tle1-tlo).gt.pieni) then
              wgh(i2)=one/(tle1-tlo)
            else
              write(6,10310) nfile
              wgh(i2)=zero
            endif
          else
            tle(i2)=tle1*half
            wgh(i2)=one/(tle1)
          endif
        else
          tle(i2)=zero
          wgh(i2)=zero
        endif
        tlo=tle1
        dle1=zero
      endif
C--COORDINATE-ANGLE CONVERSION
      call caconv(dpx,d,c)
      call caconv(dpz,f,e)
      dpxp=tpi+dpx
      dpzp=tpi+dpz
C--INVARIANTS
      call cinvar(dpx,dphix,dpz,dpzp,nuex,emz,zinv,invz)
      call cinvar(dpz,dphiz,dpx,dpxp,nuez,emx,xinv,invx)
C--RESET OF COORDINATES
      c0=c
      d0=d
      e0=e
      f0=f
      g0=g
      h0=h
      goto 240
  270 if(i2.lt.1) i2=1
C----------------------------------------------------------------------
C--ANALYSING DATA
C----------------------------------------------------------------------
C--FIT OF DISTANCE IN PHASESPACE + MEAN PHASEADVANCE
      do 280 i=2,i2
        if(iwg.eq.1) call lfitw(tle,dle,wgh,i,1,slope(i-1),const,varlea
     +  (i-1))
        if(iwg.eq.0) call lfit(tle,dle,i,1,slope(i-1),const,varlea(i-1))
  280 continue
      if(iapx.eq.0) then
        write(6,*) 'WARNING: IAPX IS ZERO'
        iapx=1
      endif
      if(iapz.eq.0) then
        write(6,*) 'WARNING: IAPZ IS ZERO'
        iapz=1
      endif
      tphx=dphx/iapx
      tphz=dphz/iapz
      if(iap6.gt.0) tph6=dph6/iap6
C--STANDARD DEVIATION OF PHASEADVANCES
      do 290 i=1,iapx
  290 sdpx=sdpx+(phase(1,i)-tphx)*(phase(1,i)-tphx)
      do 300 i=1,iapz
  300 sdpz=sdpz+(phase(2,i)-tphz)*(phase(2,i)-tphz)
      do 310 i=1,iap6
  310 sdp6=sdp6+(phase(3,i)-tph6)*(phase(3,i)-tph6)
      sdpx=sqrt(sdpx)/iapx
      sdpz=sqrt(sdpz)/iapz
      if(iap6.gt.0) sdp6=sqrt(sdp6)/iap6
C--AVERAGED EMITTANCES
      di11=i11
      if(i11.eq.0) then
        write(6,*) '** ERROR ** - I11 IS ZERO'
        goto 550
      endif
      emxa=emxa/di11
      emza=emza/di11
      emta=emta/di11
      evxm=evx/di11
      evzm=evz/di11
      evtm=evt/di11
C--SMEAR CALCULATION AND 4D-SMEAR
      rewind nfile
      read(nfile,iostat=ierro)
      if(ierro.gt.0) then
        write(6,10320) nfile
        goto 550
      endif
      iskc=-1
      do 340 i=1,i11*iskip+nstart
        ifipa=0
        read(nfile,end=350,iostat=ierro) ia,ifipa,b,c,d,e,f,g,h,p
        if(ierro.gt.0) then
          write(6,10320) nfile
          goto 550
        endif
        if(ifipa.lt.1) goto 340
        if(progrm.eq.'MAD') then
          c=c*c1e3
          d=d*c1e3
          e=e*c1e3
          f=f*c1e3
          g=g*c1e3
          p=p*c1e3
        endif
        iskc=iskc+1
        if(mod(iskc,iskip).ne.0) goto 340
        if((ia-nstart).lt.0) goto 340
        c=c-clo(1)
        d=d-clop(1)
        e=e-clo(2)
        f=f-clop(2)
        g=g-clo(3)
        h=h-clop(3)
        if(icode.ge.4) then
          c=c-di0(1)*h
          d=d-dip0(1)*h
          e=e-di0(2)*h
          f=f-dip0(2)*h
        endif
C--MEAN EMITTANCES
        xp=bet0(1)*d+alf0(1)*c
        zp=bet0(2)*f+alf0(2)*e
        emx=(c*c+xp*xp)/bet0(1)
        emz=(e*e+zp*zp)/bet0(2)
        if(icode.ge.4.and.its6d.ne.0) then
          c=c+di0(1)*h
          d=d+dip0(1)*h
          e=e+di0(2)*h
          f=f+dip0(2)*h
        endif
        emt=emx+emz
        emxs=emxs+(emx-emxa)*(emx-emxa)
        emzs=emzs+(emz-emza)*(emz-emza)
        emts=emts+(emt-emta)*(emt-emta)
C--COURANT SYNDER
        xyzv(1)=c
        xyzv(2)=d
        xyzv(3)=e
        xyzv(4)=f
        xyzv(5)=g
        xyzv(6)=h
C--CONVERT TO CANONICAL VARIABLES
        if(its6d.eq.1) then
          xyzv(2)=xyzv(2)*(one+xyzv(6)+clop(3))
          xyzv(4)=xyzv(4)*(one+xyzv(6)+clop(3))
        endif
        do 320 iq=1,6
          txyz(iq)=zero
          do 320 jq=1,6
            txyz(iq)=txyz(iq)+t(jq,iq)*xyzv(jq)
  320   continue
C--MEAN EMITTANCES WITH LINEAR COUPLING
        evx=txyz(1)*txyz(1)+txyz(2)*txyz(2)
        evz=txyz(3)*txyz(3)+txyz(4)*txyz(4)
C--COURANT SYNDER CONT.
        do 330 iq=1,6
          txyz(iq)=txyz(iq)*rbeta(iq)
  330   continue
        c=txyz(1)
        d=txyz(2)
        e=txyz(3)
        f=txyz(4)
        g=txyz(5)
        h=txyz(6)
C--MEAN EMITTANCES WITH LINEAR COUPLING CONT.
        evt=evx+evz
        sevx=sevx+(evx-evxm)*(evx-evxm)
        sevz=sevz+(evz-evzm)*(evz-evzm)
        sevt=sevt+(evt-evtm)*(evt-evtm)
  340 continue
  350 continue
C--SMEAR IN %
      call sinpro(emxa,di11,emxs,emax,emix)
      call sinpro(emza,di11,emzs,emaz,emiz)
      call sinpro(emta,di11,emts,emat,emit)
      call sinpro(evxm,di11,sevx,evxma,evxmi)
      call sinpro(evzm,di11,sevz,evzma,evzmi)
      call sinpro(evtm,di11,sevt,evtma,evtmi)
C----------------------------------------------------------------------
C--PRINTING
C----------------------------------------------------------------------
      if(nstop.lt.ia.and.(ia.lt.numl.or.ia.lt.nint(dnumlr))) nlost=1
      if(nnumxv(ifipa).eq.0.and.nnumxv(ilapa).eq.0) then
        sumda(22)=ia
        sumda(23)=ia    
      else
        sumda(22)=nnumxv(ifipa)
        sumda(23)=nnumxv(ilapa)
      endif
      sumda(2)=nlost
      sumda(9)=dp1-clop(3)
C--GET DIFFERENCE IN THE NUMBER OF TURNS PER DATA ENTRY : TIDNT
C--NOW CONSIDERING ONLY TURNS LARGER THAN NSTART
      tidnt=(ia-nstart+idnt)/i11
      if(i2.ge.2) then
        if(nprint.eq.1) write(6,10110)
        ilyap=0
        slopem=zero
        do 360 i=1,i2-1
          iturn=nint((i+1)*iav*tidnt)
          if(nprint.eq.1) write(6,10120) iturn,biav(i),slope(i),varlea
     +    (i)
        if(biav(i).gt.0.1d0) ilyap=1
        slopem=max(slopem,dble(slope(i)))
  360   continue
        if(nprint.eq.1) write(6,10130)
        sumda(10)=biav(i2-1)
        if(ilyap.eq.0) then
          sumda(11)=slope(i2-1)
        else
          sumda(11)=slopem
        endif
      endif
C--CALCULATION OF AVERAGED PHASEADVANCES
      tph6=abs(tph6)
      if(nprint.eq.1) write(6,10140) tphx,sdpx,tphz,sdpz,tph6,sdp6,qwc
     +(1),tphx-qwc(1) ,qwc(2),tphz-qwc(2),qwc(3),tph6-qwc(3),dres,ires
      sumda(3)=qwc(1)
      sumda(4)=qwc(2)
      if(abs(tphx).gt.pieni) then
        sumda(12)=tphx-qwc(1)
      else
        sumda(12)=zero
      endif
      sumda(13)=sdpx
      if(abs(tphz).gt.pieni) then
        sumda(14)=tphz-qwc(2)
      else
        sumda(14)=zero
      endif
      sumda(15)=sdpz
      sumda(25)=tph6
C--DISTANCE OF Q-VALUES (AVERAGED PHASEADVANCE) TO RESONANCES
      do 370 i=1,21
        do 370 j=1,21
          im1=i-1
          jm1=j-1
          if(im1.eq.0.and.jm1.eq.0) goto 370
          if(im1+jm1.gt.ires) goto 370
          ares=im1*tphx+jm1*tphz
          dares=anint(ares)
          ares=ares-dares
          if(abs(ares).lt.armin) then
            armin=abs(ares)
            im1s=im1
            jm1s=jm1
          endif
          ared=im1*tphx-jm1*tphz
          dared=anint(ared)
          ared=ared-dared
          if(abs(ared).lt.armin) then
            armin=abs(ared)
            im1s=im1
            jm1s=-jm1
          endif
          if(abs(ares).lt.dres.and.nprint.eq.1) write(6,10170) im1,jm1,
     +    dares,ares
          if(abs(ared).lt.dres.and.jm1.ne.0.and.im1.ne.0.and.nprint.eq.
     +    1) write(6,10170) im1,-jm1,dared,ared
  370 continue
      if(armin.lt.armin0) then
        sumda(16)=im1s
        sumda(17)=jm1s
        sumda(18)=sumda(16)+abs(sumda(17))
      endif
      if(iwarx.eq.1.and.nprint.eq.1) write(6,10150)
      if(iwarz.eq.1.and.nprint.eq.1) write(6,10160)
C--Q-VALUES BY AN FFT-ROUTINE
  380 ifp=ifp+1
      ife=2**ifp
      if(ife.le.i11.and.ife.le.nfft) then
        goto 380
      else
        ifp=ifp-1
        ife=ife/2
      endif
      if(ife.eq.0) then
        write(6,*) '** ERROR ** - IFE IS ZERO'
        goto 550
      endif
      dife=ife
      if(ifp.gt.1) then
        if(nprint.eq.1) write(6,10180) ife,dfft*100
        call fft(xxr,xxi,ifp,ife)
        call fft(zzr,zzi,ifp,ife)
        xxmax=zero
        zzmax=zero
        xxmin=one
        zzmin=one
        if(ifh.eq.0) then
          if1=1
          if2=ife
          ife2=ife
          pmin(21)=qx0
          pmax(21)=qx0+one
          pmin(23)=qz0
          pmax(23)=qz0+one
        else if(ifh.eq.1) then
          if1=1
          if2=ife/2
          ife2=ife/2
          pmin(21)=qx0
          pmax(21)=qx0+half
          pmin(23)=qz0
          pmax(23)=qz0+half
        else
          if1=ife/2+1
          if2=ife
          ife2=ife/2
          pmin(21)=qx0+half
          pmax(21)=qx0+one
          pmin(23)=qz0+half
          pmax(23)=qz0+one
        endif
        do 390 i=if1,if2
          xxmax=max(xxmax,sqrt(xxr(i)**2+xxi(i)**2))
          zzmax=max(zzmax,sqrt(zzr(i)**2+zzi(i)**2))
          xxmin=min(xxmin,sqrt(xxr(i)**2+xxi(i)**2))
          zzmin=min(zzmin,sqrt(zzr(i)**2+zzi(i)**2))
  390   continue
        if(abs(xxmax).gt.pieni) xxmin=xxmin/xxmax
        if(abs(zzmax).gt.pieni) zzmin=zzmin/zzmax
        if(xxmax.le.pieni) then
          write(6,*) 'WARNING: XXMAX IS SET TO : ',pieni
          xxmax=pieni
        endif
        if(zzmax.le.pieni) then
          write(6,*) 'WARNING: ZZMAX IS SET TO : ',pieni
          zzmax=pieni
        endif
        do 400 i=if1,if2
          xxaux=sqrt(xxr(i)**2+xxi(i)**2)
          zzaux=sqrt(zzr(i)**2+zzi(i)**2)
          if(abs(xxaux-xxmax).le.pieni) ffx=(i-1)/dife+qx0
          if(abs(zzaux-zzmax).le.pieni) ffz=(i-1)/dife+qz0
          xxaux=xxaux/xxmax
          zzaux=zzaux/zzmax
          if(xxaux.gt.dfft.and.nprint.eq.1) write(6,10190) (i-1)/dife
     +    +qx0,xxaux*100
          if(zzaux.gt.dfft.and.nprint.eq.1) write(6,10200) (i-1)/dife
     +    +qz0,zzaux*100
  400   continue
        if(nprint.eq.1) write(6,10210) ffx,ffz,qwc(1),ffx-qwc(1),qwc(2),
     +  ffz-qwc(2),dres,ires
C--DISTANCE OF Q-VALUES (FFT) TO RESONANCES
        do 410 i=1,21
          do 410 j=1,21
            im1=i-1
            jm1=j-1
            if(im1.eq.0.and.jm1.eq.0) goto 410
            if(im1+jm1.gt.ires) goto 410
            ares=im1*ffx+jm1*ffz
            dares=anint(ares)
            ares=ares-dares
            ared=im1*ffx-jm1*ffz
            dared=anint(ared)
            ared=ared-dared
            if(abs(ares).lt.dres.and.nprint.eq.1) write(6,10170) im1,
     +      jm1,dares,ares
            if(abs(ared).lt.dres.and.jm1.ne.0.and.im1.ne.0.and.nprint.eq
     +      .1) write(6,10170) im1,-jm1,dared,ared
  410   continue
      endif
C--PRINT 4-D INVARIANTS WITH LINEAR COUPLING
      if(nprint.eq.1) write(6,10270) emi,emii,emiii,angi,angii,angiii,
     +evxm,sevx,evxma,evxmi,evzm,sevz,evzma,evzmi,evtm,sevt,evtma,evtmi
C--PRINT EMITTANCES AND SMEAR
      ampx0=sqrt(bet0(1)*emx0)
      ampz0=sqrt(bet0(2)*emz0)
      if(nprint.eq.1) write(6,10220) emx0,ampx0,emz0,ampz0,emxa,emxs,
     +emax,emix,emza, emzs,emaz,emiz,emta,emts,emat,emit
      sumda(46)=emi
      sumda(47)=emii
      sumda(48)=bet0x2
      sumda(49)=bet0z2
      sumda(7)=sqrt(bet0(1)*emi)+sqrt(bet0x2*emii)
      sumda(8)=sqrt(bet0(2)*emii)+sqrt(bet0z2*emi)
      sumda(26)=sqrt(bet0(1)*evx2)+sqrt(bet0x2*evz2)
      sumda(27)=sqrt(bet0(2)*evz2)+sqrt(bet0z2*evx2)
      sumda(19)=sevx
      sumda(20)=sevz
      sumda(21)=sevt
      sumda(59)=dmmac
      sumda(60)=dnms
      sumda(24)=dizu0
      emax=emax/100*emxa+emxa
      emix=emix/100*emxa+emxa
      emaz=emaz/100*emza+emza
      emiz=emiz/100*emza+emza
      sumda(28)=sqrt(bet0(1)*emix)
      sumda(29)=sqrt(bet0(1)*emxa)
      sumda(30)=sqrt(bet0(1)*emax)
      sumda(31)=sqrt(bet0(2)*emiz)
      sumda(32)=sqrt(bet0(2)*emza)
      sumda(33)=sqrt(bet0(2)*emaz)
      evxma=evxma/100*evxm+evxm
      evxmi=evxmi/100*evxm+evxm
      evzma=evzma/100*evzm+evzm
      evzmi=evzmi/100*evzm+evzm
      sumda(34)=sqrt(bet0(1)*evxmi)
      sumda(35)=sqrt(bet0(1)*evxm)
      sumda(36)=sqrt(bet0(1)*evxma)
      sumda(37)=sqrt(bet0(2)*evzmi)
      sumda(38)=sqrt(bet0(2)*evzm)
      sumda(39)=sqrt(bet0(2)*evzma)
      evtma=evtma/100*evtm+evtm
      evtmi=evtmi/100*evtm+evtm
      if(abs(evxm+evzm).gt.pieni) then
        ratemx=evxm/(evxm+evzm)
        ratemz=evzm/(evxm+evzm)
      else
        ratemx=zero
        ratemz=zero
      endif
      sumda(40)=sqrt(bet0(1)*evtmi*ratemx)
      sumda(41)=sqrt(bet0(1)*evtm*ratemx)
      sumda(42)=sqrt(bet0(1)*evtma*ratemx)
      sumda(43)=sqrt(bet0(2)*evtmi*ratemz)
      sumda(44)=sqrt(bet0(2)*evtm*ratemz)
      sumda(45)=sqrt(bet0(2)*evtma*ratemz)
C--WRITE DATA FOR THE SUMMARY OF THE POSTPROCESSING ON FILE # 10
      write(10,*,iostat=ierro) (sumda(i),i=1,60)
      if(ierro.ne.0) then
        write(6,*)
        write(6,*) '*** ERROR ***,PROBLEMS WRITTING TO FILE # : ',10
        write(6,*) 'ERROR CODE : ',ierro
        write(6,*)
      endif
C--CALCULATION THE INVARIANCES OF THE 4D TRANSVERSAL MOTION
      do 420 i=1,ninv
        if(invx(i).gt.0) then
          nuix=nuix+1
          xing=xing+xinv(i)/invx(i)
        endif
        if(invz(i).gt.0) then
          nuiz=nuiz+1
          zing=zing+zinv(i)/invz(i)
        endif
  420 continue
      pinx=nuix
      pinz=nuiz
      if(nuix.ne.0) then
        pixr=dble(nuez)/dble(nuix)
        xing=xing/nuix
      endif
      if(nuiz.ne.0) then
        pizr=dble(nuex)/dble(nuiz)
        zing=zing/nuiz
      endif
      pinx=pinx/ninv*100
      pinz=pinz/ninv*100
      if(nprint.eq.1) write(6,10230)
      if(nuez.lt.ninv.and.nprint.eq.1) write(6,10240) nuez,ninv
      if(nuex.lt.ninv.and.nprint.eq.1) write(6,10250) nuex,ninv
      if(nprint.eq.1) write(6,10260) nuez,nuix,nuex,nuiz, ninv,pinx,
     +pixr,pinz,pizr,xing,zing
C----------------------------------------------------------------------
C--PLOTTING
C----------------------------------------------------------------------
      pmin(1)=zero
      pmax(1)=ia
      pmin(7)=pmin(3)
      pmax(7)=pmax(3)
      pmin(8)=pmin(5)
      pmax(8)=pmax(5)
      pmin(11)=pmin(3)
      pmax(11)=pmax(3)
      pmin(12)=pmin(10)
      pmax(12)=pmax(10)
      pmin(13)=pmin(5)
      pmax(13)=pmax(5)
      pmin(14)=pmin(10)
      pmax(14)=pmax(10)
      pmax(15)=ia
      pmin(17)=pmin(3)
      pmax(17)=pmax(3)
      pmin(18)=pmin(4)
      pmax(18)=pmax(4)
      pmin(19)=pmin(5)
      pmax(19)=pmax(5)
      pmin(20)=pmin(6)
      pmax(20)=pmax(6)
      pmin(22)=zero
      pmin(24)=zero
      pmax(22)=one
      pmax(24)=one
      do 500 i=1,12
        i2=2*i
        i1=i2-1
        if(pmin(i1).gt.pmax(i1)) pmin(i1)=pmax(i1)
        if(pmin(i2).gt.pmax(i2)) pmin(i2)=pmax(i2)
        if((abs(pmin(i1)-pmax(i1)).le.pieni2) .or.(abs(pmin(i2)-pmax(i2)
     +  ).le.pieni2)) then
          goto 500
        endif
        do 430 i3=i1,i2
          pcha=(pmax(i3)-pmin(i3))*prec
          pmin(i3)=pmin(i3)-pcha
          pmax(i3)=pmax(i3)+pcha
  430   continue
        if(iffw.eq.2) then
          pmin(22)=xxmin/(1d0+abs(prec))
          pmin(24)=zzmin/(1d0+abs(prec))
        endif
        if((i.eq.1.and.idis.eq.1).or. (i.gt.1.and.i.le.8.and.icow.eq.1)
     +  .or. ((i.eq.9.or.i.eq.10).and.istw.eq.1).and. (pmin(i1).ne.pmax
     +  (i1).and.pmin(i2).ne.pmax(i2))) then
C--HBOOK FRAME
          call htitle(title(i))
          call hbook2(i,' ',2,pmin(i1),pmax(i1), 2,
     +    pmin(i2),pmax(i2),0.d0)
          call hplot(i,' ',' ',0)
          call hplax(chxtit(i),chytit(i))
          call hplsof(4.d0,14.75d0,toptit(1),.15d0,0.d0,99.d0,-1)
          call hplsof(4.d0,14.50d0,toptit(2),.15d0,0.d0,99.d0,-1)
          call hplsof(4.d0,14.25d0,toptit(3),.15d0,0.d0,99.d0,-1)
          call hplsof(4.d0,14.00d0,toptit(4),.15d0,0.d0,99.d0,-1)
          call iselnt(10)
          rewind nfile
          read(nfile,iostat=ierro)
          if(ierro.gt.0) then
            write(6,10320) nfile
            goto 550
          endif
          iskc=-1
          do 460 j=1,i11*iskip+nstart
            ifipa=0
            if(ntwin.eq.1) read(nfile,end=470,iostat=ierro) ia,ifipa,b,
     +      c,d,e,f,g,h,p
            if(ntwin.eq.2) read(nfile,end=470,iostat=ierro) ia,ifipa,b,
     +      c,d,e,f,g,h,p, ilapa,b,c1,d1,e1,f1,g1,h1,p1
            if(ierro.gt.0) then
              write(6,10320) nfile
              goto 550
            endif
            if(ifipa.lt.1) goto 460
            iskc=iskc+1
            if(mod(iskc,iskip).ne.0) goto 460
            if((ia-nstart).lt.0) goto 460
            if(progrm.eq.'MAD') then
              c=c*c1e3
              d=d*c1e3
              e=e*c1e3
              f=f*c1e3
              g=g*c1e3
              p=p*c1e3
              if(ntwin.eq.2) then
                c1=c1*c1e3
                d1=d1*c1e3
                e1=e1*c1e3
                f1=f1*c1e3
                g1=g1*c1e3
                p1=p1*c1e3
              endif
            endif
C--LYAPUNOV
            if(ntwin.eq.2) then
              x(1,1)=c
              x(1,2)=d
              x(1,3)=e
              x(1,4)=f
              x(1,5)=g
              x(1,6)=h
              x(2,1)=c1
              x(2,2)=d1
              x(2,3)=e1
              x(2,4)=f1
              x(2,5)=g1
              x(2,6)=h1
              call distance(x,cloau,di0au,t,b)
            endif
            if(icode.ge.4.and.its6d.eq.0) then
              c=c-di0(1)*h
              d=d-dip0(1)*h
              e=e-di0(2)*h
              f=f-dip0(2)*h
            endif
            c=c-clo(1)
            d=d-clop(1)
            e=e-clo(2)
            f=f-clop(2)
            g=g-clo(3)
            h=h-clop(3)
            xyzv(1)=c
            xyzv(2)=d
            xyzv(3)=e
            xyzv(4)=f
            xyzv(5)=g
            xyzv(6)=h
C--CONVERT TO CANONICAL VARIABLES
            if(its6d.eq.1) then
              xyzv(2)=xyzv(2)*(one+xyzv(6)+clop(3))
              xyzv(4)=xyzv(4)*(one+xyzv(6)+clop(3))
            endif
            do 440 iq=1,6
              txyz(iq)=zero
              do 440 jq=1,6
                txyz(iq)=txyz(iq)+t(jq,iq)*xyzv(jq)
  440       continue
            do 450 iq=1,6
              txyz(iq)=txyz(iq)*rbeta(iq)
  450       continue
            c=txyz(1)
            d=txyz(2)
            e=txyz(3)
            f=txyz(4)
            g=txyz(5)*cma2
            h=txyz(6)*cma1
            if(istw.eq.1.and.(i.eq.9.or.i.eq.10)) then
              if(i.eq.9) then
                call caconv(dpz,f,e)
              endif
              if(i.eq.10) then
                call caconv(dpx,d,c)
              endif
            endif
  460     continue
  470     continue
        else if((iffw.eq.1.or.iffw.eq.2).and.(i.eq.11.or.i.eq.12)) then
C--HBOOK FRAME
          call htitle(title(i))
          call hbook2(i,' ',2,pmin(i1),pmax(i1), 2,
     +    pmin(i2),pmax(i2),0.0d0)
          if(iffw.eq.2) call hplopt('LOGY',1)
          call hplot(i,' ',' ',0)
          call hplax(chxtit(i),chytit(i))
          call hplsof(4.d0,14.75d0,toptit(1),.15d0,0.d0,99.d0,-1)
          call hplsof(4.d0,14.50d0,toptit(2),.15d0,0.d0,99.d0,-1)
          call hplsof(4.d0,14.25d0,toptit(3),.15d0,0.d0,99.d0,-1)
          call hplsof(4.d0,14.00d0,toptit(4),.15d0,0.d0,99.d0,-1)
          call iselnt(10)
          if(i.eq.11) then
            do 480 k=if1,if2
              k1=k-if1+1
              xxaux=sqrt(xxr(k)**2+xxi(k)**2)
              xxaux=xxaux/xxmax
              fxs(k1)=(k-1)/dife+qx0
              if(iffw.eq.2) then
                if(abs(xxaux).lt.pieni) then
                  write(6,*) '* * * ERROR * * *'
                  write(6,*)
     +            'Apparently horizontal FFT data are corrupted'
                  xxaux=pieni
                endif
                fzs(k1)=log10(xxaux)
              else
                fzs(k1)=xxaux
              endif
              if(nprint.eq.1) then
                write(14,10030,iostat=ierro) fxs(k1),fzs(k1)
                if(ierro.ne.0) then
                  write(6,*)
                  write(6,*)
     +            '*** ERROR ***,PROBLEMS WRITTING TO FILE # : ',14
                  write(6,*) 'ERROR CODE : ',ierro
                  write(6,*)
                endif
              endif
  480       continue
            call ipl(ife2,fxs,fzs)
          else if(i.eq.12) then
            do 490 k=if1,if2
              k1=k-if1+1
              zzaux=sqrt(zzr(k)**2+zzi(k)**2)
              zzaux=zzaux/zzmax
              fxs(k1)=(k-1)/dife+qz0
              if(iffw.eq.2) then
                if(abs(zzaux).lt.pieni) then
                  write(6,*) '* * * ERROR * * *'
                  write(6,*)
     +            'Apparently vertical FFT data are corrupted'
                  zzaux=pieni
                endif
                fzs(k1)=log10(zzaux)
              else
                fzs(k1)=zzaux
              endif
              if(nprint.eq.1) then
                write(15,10030,iostat=ierro) fxs(k1),fzs(k1)
                if(ierro.ne.0) then
                  write(6,*)
                  write(6,*)
     +            '*** ERROR ***,PROBLEMS WRITTING TO FILE # : ',14
                  write(6,*) 'ERROR CODE : ',ierro
                  write(6,*)
                endif
              endif
  490       continue
            call ipl(ife2,fxs,fzs)
          endif
        endif
        if(iffw.eq.2) call hplopt('LINY',1)
  500 continue
      if(idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0)
     &call hdelet(0)
      goto 560
  510 continue
      write(6,10300) nfile,'HEADER CORRUPTED'
      goto 550
  520 continue
      write(6,10300) nfile,'HEADER OF MADFILE CORRUPTED'
      goto 550
  530 continue
      write(6,10300) nfile,'NO DATA'
      goto 550
  540 continue
      write(6,10300) nfile,'WRONG RANGE OF DATA FOR PROCESSING'
  550 write(10,*,iostat=ierro) (sumda(i),i=1,60)
C--REWIND USED FILES
  560 rewind nfile
      rewind 14
      rewind 15
C--TIME COUNT
C----------------------------------------------------------------------
C----------------------------------------------------------------------
      return
10000 format(d10.4)
10010 format(f10.6)
10020 format(a80)
10030 format(2f10.6)
10040 format( //131('-')//10x,'OOOOOOOOOOOOOOOOOOOOOO' /10x,
     +'OO                  OO' /10x,'OO  POSTPROCESSING  OO' /10x,
     +'OO                  OO' /10x,'OOOOOOOOOOOOOOOOOOOOOO'// /10x,
     +'TITLE AND COMMENT :'//a80//a80// )
10050 format(10x,'THE FOLLOWING PARAMETERS ARE USED:'//
     +10x,'PROGRAM NAME',t102,a8/
     +10x,'PARTICLE NUMBER',t102,i3/
     +10x,'TOTAL NUMBER OF PARTICLES',t102,i3/
     +10x,'PHASE SPACE',t102,a11/
     +10x,'MAXIMUM NUMBER OF TURNS',t102,i8/
     +10x,'HORIZONTAL BETA',t102,f16.10/
     +10x,'HORIZONTAL BETA-II',t102,f16.10/
     +10x,'HORIZONTAL BETA-III',t102,f16.10/
     +10x,'VERTICAL BETA',t102,f16.10/
     +10x,'VERTICAL BETA-II',t102,f16.10/
     +10x,'VERTICAL BETA-III',t102,f16.10/
     +10x,'LONGITUDINAL BETA',t98,f20.10/
     +10x,'LONGITUDINAL BETA-II',t102,f16.10/
     +10x,'LONGITUDINAL BETA-III',t102,f16.10/
     +10x,'HORIZONTAL ALFA',t102,f16.10/
     +10x,'HORIZONTAL ALFA-II',t102,f16.10/
     +10x,'HORIZONTAL ALFA-III',t102,f16.10)
10060 format(
     +10x,'VERTICAL ALFA',t102,f16.10/
     +10x,'VERTICAL ALFA-II',t102,f16.10/
     +10x,'VERTICAL ALFA-III',t102,f16.10/
     +10x,'LONGITUDINAL ALFA',t102,f16.10/
     +10x,'LONGITUDINAL ALFA-II',t102,f16.10/
     +10x,'LONGITUDINAL ALFA-III',t102,f16.10/
     +10x,'HORIZONTAL GAMMA',t102,f16.10/
     +10x,'HORIZONTAL GAMMA-II',t102,f16.10/
     +10x,'HORIZONTAL GAMMA-III',t102,f16.10/
     +10x,'VERTICAL GAMMA',t102,f16.10/
     +10x,'VERTICAL GAMMA-II',t102,f16.10/
     +10x,'VERTICAL GAMMA-III',t102,f16.10/
     +10x,'LONGITUDINAL GAMMA',t102,f16.10/
     +10x,'LONGITUDINAL GAMMA-II',t102,f16.10/
     +10x,'LONGITUDINAL GAMMA-III',t102,f16.10)
10061 format(
     +10x,'HORIZONTAL CLOSED ORBIT',t102,d16.10/
     +10x,'VERTICAL CLOSED ORBIT',t102,d16.10/
     +10x,'LONGITUDINAL CLOSED ORBIT',t102,d16.10/
     +10x,'SLOPE OF HORIZONTAL CLOSED ORBIT',t102,d16.10/
     +10x,'SLOPE OF VERTICAL CLOSED ORBIT',t102,d16.10/
     +10x,'SLOPE OF LONGITUDINAL CLOSED ORBIT',t102,d16.10/
     +10x,'HORIZONTAL DISPERSION',t102,f16.10/
     +10x,'VERTICAL DISPERSION',t102,f16.10/
     +10x,'SLOPE OF HORIZONTAL DISPERSION',t102,f16.10/
     +10x,'SLOPE OF VERTICAL DISPERSION',t102,f16.10/
     +10x,'LINEAR HORIZONTAL TUNE',t102,f16.10/
     +10x,'LINEAR VERTICAL TUNE',t102,f16.10/
     +10x,'LINEAR LONGITUDINAL TUNE',t102,f16.10)
10070 format( 10x,'DATA IS AVERAGED IN SAMPLES OF IAV TURNS',t96,
     +'IAV =    ',i7 /10x,'START TURN NUMBER FOR THE ANALYSIS ',t93,
     +'NSTART =  ',i9 /10x,'THE ANALYSIS STOPS AFTER TURN NUMBER ',t94,
     +'NSTOP =  ',i9, /10x,
     +'HORIZONTAL ANGLE-INTERVAL FOR STROBOSCOPING THE VERTICAL',
     +' PHASESPACE PROJECTION',t94,'DPHIX = ',d16.10 /10x,
     +'VERTICAL ANGLE-INTERVAL FOR STROBOSCOPING THE HORIZONTAL',
     +' PHASESPACE PROJECTION',t94,'DPHIZ = ',d16.10 /10x,
     +'SWITCH FOR THE WEIGHTING OF THE LINEAR FIT FOR THE ' ,
     +'DISTANCE IN PHASESPACE ',t96,'IWG = ',i4 /10x,
     +'INTEGER PART FOR THE HORIZONTAL TUNE ',t96,'QX0 = ' ,f16.10 /10x,
     +'INTEGER PART FOR THE VERTICAL TUNE ',t96,'QZ0 = ' ,f16.10 )
10080 format( 10x,'SWITCH FOR THE QX-VALUE CLOSE TO AN HALF-INTEGER' ,
     +' ( INT => 1 ; HALF-INT => 0 )',t95,'IVOX = ',i4 /10x,
     +'SWITCH FOR THE QZ-VALUE CLOSE TO AN HALF-INTEGER' ,
     +' ( INT => 1 ; HALF-INT => 0 )',t95,'IVOZ = ',i4 /10x,
     +'Q-VALUES ARE CHECKED FOR RESONANCES UP TO ORDER',t95, 'IRES = ',
     +i4 /10x,'A RESONANCE IS CONSIDERED TO BE STRONG WHEN THE Q-VALUES'
     +, ' ARE CLOSER TO IT THAN',t95,'DRES = ',d16.10 /10x,
     +'SWITCH FOR FFT-RANGE ( IFH=0 => 0-1 ; IFH=1 => 0-.5 ' ,
     +'; IFH=2 => .5-1 )',t96,'IFH = ',i4 /10x,
     +'Q-PEAKS OF THE FFT ARE CONSIDERED IF THEY ARE LARGER THAN' ,t95,
     +'DFFT = ',d16.10 )
10090 format( 10x,'TERMINAL TYPE',t93,'KWTYPE = ',i4 /10x,
     +'SWITCH FOR PLOT ON TERMINAL (T) AND/OR GKSFILE (F)' ,
     +' ( F => -1 ; T => 0 ; F+T => 1)',t96,'ITF = ',i4 /10x,
     +'SWITCH TO STOP AFTER PLOTTING' ,
     +' ( ICR = 0 => NO STOP ; ICR = 1 => STOP )',t96,'ICR = ',i4 /10x,
     +'SWITCH FOR PRINTING THE DISTANCE IN PHASE SPACE' ,t95,'IDIS = ',
     +i4 /10x,'SWITCH FOR PRINTING THE COORDINATES',t95,'ICOW = ',i4 /10
     +x,'SWITCH FOR PRINTING THE STROBOSCOPED PHASESPACES',t95,
     +'ISTW = ',i4 /10x,'SWITCH FOR PRINTING THE FFT-SIGNALS',t95,
     +'IFFW = ',i4 ,i4 )
10100 format( 10x,'EVERY ISKIP VALUE IS USED FOR THE ANALYSIS' ,t94,
     +'ISKIP = ',i4 /10x,
     +'SWITCH OF COURANT SYNDER TRANSFORMATION (ICONV = 1 => OFF)' ,t93,
     +' ICONV = ',i4 /10x,
     +'SWITCH FOR READING MAD DATA ( IMAD = 1 => MAD-DATA ' ,
     +'WITH LYAPUNOV ANALYSIS )',t95,'IMAD = ',i4 /10x,
     +'SCALING OF MOMENTUM', ' WITH LYAPUNOV ANALYSIS',t95,'CMA1 = ',f16
     +.10 /10x,'SCALING OF PATH-LENGTH', ' WITH LYAPUNOV ANALYSIS',t95,
     +'CMA2 = ',f16.10 /10x,
     +'SWITCH FOR PRINTING OF THE POSTPROCESSING OUTPUT' ,
     +' NPRINT = ( 0 => OFF ; 1 => ON) ',t93,'NPRINT = ',i4 /10x,
     +'NUMBER OF BINARY FILES TO BE PROCESSED', ' ( 90 - [90-NDAFI+1] )'
     +,t94,'NDAFI = ',i4 //)
10110 format(/10x,'ANALYSING THE INCREASE OF THE DISTANCE IN PHASE-' ,
     +'SPACE'/10x,53('-')/ //12x,'TURNS',10x,'DISTANCE',13x,
     +'SLOPE          RESIDUAL' /10x,63('-'))
10120 format(10x,i7,6x,d16.10,2(2x,f15.10))
10130 format(10x,63('-')//)
10140 format(//10x,'AVERAGED PHASE-ADVANCE' /10x,22('-')/ /10x,
     +'X-PHASE :  ',f14.10,'   +/_ ',f14.10 /10x,'Z-PHASE :  ',f14.10,
     +'   +/_ ',f14.10/ /10x,'S-PHASE :  ',f14.10,'   +/_ ',f14.10/ /10
     +x,'START-QX : ',f14.10,'   CHANGE IN X : ',d16.10 /10x,
     +'START-QZ : ',f14.10,'   CHANGE IN Z : ',d16.10 /10x,'START-QS : '
     +,f14.10,'   CHANGE IN S : ',d16.10// /10x,
     +'THE AVERAGED PHASE-ADVANCES ARE CLOSER THEN ',d10.4,' TO ' ,
     +'THE FOLLOWING RESONANCES UP TO ',i3,' ORDER'/10x,98('-')/ /10x,
     +'NX * QX   +   NZ * QZ   -      P      =      DELTA'/10x, 52('-'))
10150 format(/10x,'WARNING ! X-PHASE MIGHT NOT BE PRECISE'/)
10160 format(/10x,'WARNING ! Z-PHASE MIGHT NOT BE PRECISE'//)
10170 format(12x,i2,11x,i3,7x,f8.1,9x,d10.4)
10180 format(//10x,'Q-VALUES FROM FFT-ROUTINE' /10x,25('-')/ /10x,
     +'THE ANALYSIS WAS DONE WITH ',i7,' ENTRIES.'/ /10x,
     +'THE FOLLOWING Q-PEAKS ARE LARGER THEN ',f8.4,' PERCENT.'/ /10x,
     +'PLANE          Q-VALUE            SIZE [%]'/10x,43('-'))
10190 format(12x,'X',7x,f14.10,5x,f14.10)
10200 format(12x,'Z',7x,f14.10,5x,f14.10)
10210 format(//10x,'MAXIMUM PEAK'/ /10x,'HORIZONTAL PLANE :  ',f14.10
     +/10x,'VERTICAL PLANE   :  ',f14.10/ /10x,'START-QX : ',f14.10,
     +'   CHANGE IN X : ',d16.10 /10x,'START-QZ : ',f14.10,
     +'   CHANGE IN Z : ',d16.10// /10x,
     +'THE MAXIMUM Q-PEAKS ARE CLOSER THEN ',d10.4,' TO ' ,
     +'THE FOLLOWING RESONANCES UP TO ',i3,' ORDER'/10x,96('-')/ /10x,
     +'NX * QX   +   NZ * QZ   -      P      =      DELTA'/10x, 52('-'))
10220 format(////10x,'CALCULATION OF THE AVERAGED EMITTANCES' /10x,38(
     +'-')// 24x,'START-EMITTANCE           START-AMPLITUDE'// 10x,
     +'HORIZONTAL   ',f16.10,9x,f16.10/ 10x,'VERTICAL     ',f16.10,9x,
     +f16.10// 14x,'PLANE',10x,'EMITTANCE',16x,'SMEAR',12x,'MAXIMUM',11
     +x, 'MINIMUM'/28x,'[PI*MM*MRAD]',15x,'[%]',15x,'[%]',15x, '[%]'/10
     +x,86('-')/ 10x,'HORIZONTAL',6x,f16.10,3(6x,f12.6)/ 10x,'VERTICAL',
     +8x,f16.10,3(6x,f12.6)/ 10x,'SUM',13x,f16.10,3(6x,f12.6)/10x,86('-'
     +)//)
10230 format(//10x,'INVARIANTS OF THE 4-DIMENSIONAL PHASE-SPACE' /10x,43
     +('-')//)
10240 format(/10x,'WARNING ! CALCULATION OF THE HORIZONTAL INVARIANT' ,
     +' MIGHT NOT BE PRECISE'/10x,'ONLY ',i5,' ENTRIES COMPARED TO ' ,
     +i5,' ANGLE-INTERVALS !'/10x,'INCREASE THE VERTICAL ANGLE-INT' ,
     +'ERVAL <DPHIZ>'/)
10250 format(/10x,'WARNING ! CALCULATION OF THE VERTICAL INVARIANT' ,
     +' MIGHT NOT BE PRECISE'/10x,'ONLY ',i5,' ENTRIES COMPARED TO ' ,
     +i5,' ANGLE-INTERVALS !'/10x,'INCREASE THE HORIZONTAL ANGLE-INT' ,
     +'ERVAL <DPHIX>'/)
10260 format(/10x,'THERE ARE ',i5,' ENTRIES FOR THE CALCULATION OF' ,
     +' THE HORIZONTAL INVARIANT GROUPED IN ',i5,' ANGLE-INTERVALS' /10
     +x,'--------- ',i5,' ENTRIES ----------------------' ,
     +'---- VERTICAL   -------------------- ',i5,' ANGLE-INTERVALS' //10
     +x,'IF THE MOTION IS CLOSE TO FIXPOINTS THE NUMBER OF THOSE' ,
     +' ANGLE-INTERVALS WILL BE ONLY A SMALL FRACTION'/10x,
     +'OF THE TOTAL NUMBER OF ',i5,' INTERVALS.'/ /25x,
     +'PERCENTAGE OF OCCUPIED INTERVALS     NUMBER OF ENTRIES ' ,
     +'PER OCCUPIED INTERVAL' /10x,'HORIZONTAL',16x,f10.6,30x,f12.6 /10
     +x,'VERTICAL  ',16x,f10.6,30x,f12.6/ //10x,
     +'THE CALCULATED INVARIANTS ARE IN UNITS OF [PI*MM*MRAD]'/ /10x,
     +'HORIZONTAL',10x,f16.10 /10x,'VERTICAL  ',10x,f16.10//)
10270 format(////10x,'LINEARLY DECOUPLED INVARIANTS' /10x,35('-')/ 10x,
     +'INITIAL EMITTANCE MODE I   :',f16.10/ 10x,
     +'INITIAL EMITTANCE MODE II  :',f16.10/ 10x,
     +'INITIAL EMITTANCE MODE III :',f16.10/ 10x,
     +'INITIAL ANGLE     MODE I   :',f16.10/ 10x,
     +'INITIAL ANGLE     MODE II  :',f16.10/ 10x,
     +'INITIAL ANGLE     MODE III :',f16.10/ /10x,35('-')// 14x,'PLANE',
     +10x,'EMITTANCE',14x,'4D-SMEAR',11x,'MAXIMUM',11x, 'MINIMUM'/28x,
     +'[PI*MM*MRAD]',15x,'[%]',15x,'[%]',15x, '[%]'/10x,86('-')/ 10x,
     +'HORIZONTAL',6x,f16.10,3(6x,f12.6)/ 10x,'VERTICAL',8x,f16.10,3
     +(6x,f12.6)/ 10x,'SUM',13x,f16.10,3(6x,f12.6)/10x,86('-')//)
10290 format(//10x,'** ERROR ** ----- TRANSFORMATION MATRIX SINGULAR ' ,
     +'(FILE : ',i2,') -----'//)
10300 format(//10x,'** ERROR ** ----- FILE :',i2,' WITH TRACKING ' ,
     +'DATA EMPTY OR CORRUPTED-----'/10x,'PROBLEM : ',a80//)
10310 format(//10x,'** ERROR ** ----- WEIGHTING OF DISTANCE IN PHASE' ,
     +' SPACE (FILE : ',i2,') NOT POSSIBLE-----'//)
10320 format(//10x,'** ERROR ** ----- INPUT DATA CORRUPTED' ,' (FILE : '
     +,i2,') -----'//)
      end
