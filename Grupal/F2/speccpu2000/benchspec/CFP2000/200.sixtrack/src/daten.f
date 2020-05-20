C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine daten
C-----------------------------------------------------------------------
C  READS INPUT DATA
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
      character*16 bez,bezb,bezr,erbez,coel,bezl
      character*80 toptit,sixtit,commen
      common/erro/ierro
      common/erroc/erbez
      common/kons/pi,pi2,pisqrt,rad
      common/str /il,mper,mblo,mbloz,msym(nper),kanf,iu,ic(nblz)
      common/ell /ed(nele),el(nele),ek(nele),sm(nele),kz(nele),kp(nele)
      common/pla /xpl(nele),xrms(nele),zpl(nele),zrms(nele)
      common/str2 /mel(nblo),mtyp(nblo,nelb),mstr(nblo)
      common/mat/a(nele,2,6),bl1(nblo,2,6),bl2(nblo,2,6)
      common/syos2/rvf(mpa)
      common/tra1/rat,idfor,napx,napxo,numl,numlr,nde(2),nwr(4),
     &ird,imc,irew,ntwin,iclo6,iclo6r,iver,ibidu
      common/syn/qs,e0,pma,ej(mpa),ejf(mpa),phas0,phas,hsy(3),
     &crad,hsyc(nele),phasc(nele),dppoff,sigmoff(nblz),tlen,
     &iicav,itionc(nele),ition,idp,ncy,ixcav
      common/cor/dpscor,sigcor,icode,idam,its6d
      common/multi/bk0(nele,mmul),ak0(nele,mmul),
     &bka(nele,mmul),aka(nele,mmul)
      common/mult1/benki,benkc(nele),r00(nele),irm(nele),nmu(nele)
      common/rand0/zfz(nzfz),iorg,mzu(nblz),izu0,mmac,mcut
      common/rand0c/bezr(3,nele)
      common/rand1/exterr(nblz,40),extalign(nblz,3),tiltc(nblz),
     &tilts(nblz),mout2,icext(nblz),icextal(nblz)
      common/beo /aper(2),di0(2),dip0(2),ta(6,6)
      common/clo/dma,dmap,dkq,dqq,de0,ded,dsi,dech,dsm0,itco,itcro,itqv,
     &iout
      common/qmodi/qw0(3),amp0,iq(3),iqmod,kpa(nele),iqmod6
      common/linop/elbe(nblo),ilin,nt,iprint,ntco,eui,euii,nlin
      common/linopc/bez(nele),bezb(nblo),bezl(nele)
      common/cororb/betam(nmon1,2),pam(nmon1,2),betac(ncor1,2),
     &pac(ncor1,2),bclorb(nmon1,2),nhmoni,nhcorr,nvmoni,nvcorr,
     &ncororb(nele)
      common/apert/apx(nele),apz(nele)
      common/clos/sigma0(2),iclo,ncorru,ncorrep
      common/combin/icomb0(20),icomb(ncom,20),ratio(ncom,20),
     &ratioe(nele),iratioe(nele),icoe
      common/sea/ise,mesa,mp,m21,m22,m23,ise1,ise2,ise3,isea(nele)
      common/subres/qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl
      common/secom/rtc(9,18,10,5),rts(9,18,10,5),ire(12),ipr(5),irmod2
      common/secom1/dtr(10),nre,nur,nch,nqc,npp,nrr(5),nu(5)
      common/postr/dphix,dphiz,qx0,qz0,dres,dfft,cma1,cma2,
     &nstart,nstop,iskip,iconv,imad
      common/posti1/ipos,iav,iwg,ivox,ivoz,ires,ifh
      common/posti3/toptit(5)
      common/posti2/kwtype,itf,icr,idis,icow,istw,iffw,nprint,ndafi
      common/ripp/irip,irco,ramp(nele),rfre(nele),rzph(nele),nrel(nele)
      common/ripp2/nrturn
      common/skew/qwsk(2),betx(2),betz(2),alfx(2),alfz(2),iskew,nskew(6)
      common/titc/sixtit,commen
      common/tit/ithick
      common/co6d/clo6(3),clop6(3)
      common/dkic/dki(nele,3)
      common/beam/clobaux(2,nele),sigman(2,npart,nele),sigman6(2,nele),
     &sigman2(2,npart,nele),sigmanq(2,npart,nele),clobeam(2,npart,nele),
     &beamoff(2,npart,nele),partnum,emitnx,emitnz,gammar,nbeam,nvbeam,
     &ibeco,ibtyp
      common/syos/as(6,2,npart,nele),
     &al(6,2,npart,nele),sigm(mpa),dps(mpa),idz(2)
      common/anf/chi0,chid,exz(2,6),dp1,itra
      common/tra/x(mpa,2),y(mpa,2),amp(2),bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),is(2),ichrom
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor),
     &idalloc
      common/norf/nordf,nvarf,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax
      common/tcorrc/coel(10)
      character*16 sing,stru,prin,trac,diff,sync,ende,bloc,comm
      character*16 fluc,chro,tune,iter,limi,orbi,deco
      character*16 beze,bez0,go,rect,comb,sear,subr,reso,bezext
      character*16 free,geom,cavi,disp
      character*16 idat,next,mult,line,init,ic0,imn,icel,irel
C     character*16 nextb
      character*16 iss,iqq,iele,ilm,ilm0,idum,corr,norm
      character*16 kl,kr,orga,post,ripp,beam
C     character*16 ik
      character*60 ihead
      character*80 ch
      character*160 ch1
      dimension icel(ncom,20),iss(2),iqq(5)
      dimension beze(nblo,nelb),ilm(nelb),ilm0(40),bez0(nele),ic0(10)
      dimension extaux(40),bezext(nblz)
      save
      data sing,stru,prin,sync,ende,next,comm
     &/'SING','STRU','PRIN','SYNC','ENDE','NEXT','COMM'/
      data fluc,mult,chro,iter,tune,line,trac,diff
     &/'FLUC','MULT','CHRO','ITER','TUNE','LINE','TRAC','DIFF'/
      data limi,orbi,bloc,init,go,sear,subr,reso,disp,post,ripp,deco
     &/'LIMI','ORBI','BLOC','INIT','GO','SEAR','SUBR',
     &'RESO','DISP','POST','RIPP','DECO'/
      data rect,comb,free,geom,cavi,beam
     &/'RE','COMB','FREE','GEOM','CAV','BEAM'/
      data idum,kl,kr,orga,norm,corr/' ','(',')','ORGA','NORM','CORR'/
C-----------------------------------------------------------------------
      if(mmul.lt.10.or.mmul.gt.20) call error(85)
      irecuin=0
      iss(1)=' '
      iss(2)=' '
      do 10 i=1,5
        iqq(i)=' '
   10 continue
      do 20 i=1,nele
        bez0(i)=' '
   20 continue
      do 30 i=1,nblo
        do 30 j=1,nelb
          beze(i,j)=' '
   30 continue
      do 40 i=1,40
        ilm0(i)=' '
        extaux(i)=zero
   40 continue
      do 50 i=1,10
        coel(i)=' '
   50 continue
      do 60 i=1,ncom
        do 60 j=1,20
          icel(i,j)=' '
   60 continue
      do 70 i=1,10
        ic0(i)=' '
   70 continue
      do 80 i=1,nelb
        ilm(i)=' '
   80 continue
      ihead=' '
      sixtit=' '
      nbidu=0
      iclo6=0
      iclo6r=0
      iclr=0
      icy=0
      ncy=0
      ncy2=0
      nsix=0
      ncor=0
      nvar2=0
      ndum=0
      numl=1
      napx=0
      amp(1)=c1m3
      amp0=zero
      ird=0
      imc=0
      idial=0
      idz(1)=1
      idz(2)=1
      idfor=0
      irew=0
      nde(1)=0
      nde(2)=0
      nwr(1)=1
      nwr(2)=1
      nwr(3)=1
      nwr(4)=10000
      ntwin=1
      harm=one
      alc=c1m3
      phag=zero
      tlen=one
      pma=pmap
      ition=0
      dpscor=one
      sigcor=one
      iconv=0
      imad=0
      iskip=1
      cma1=one
      cma2=one
      qs=zero
      itra=0
      chi0=zero
      chid=zero
      rat=zero
      rv=one
      ipos=0
      iav=1
      nstart=0
      nstop=0
      iwg=1
      dphix=zero
      dphiz=zero
      qx0=zero
      qz0=zero
      ivox=1
      ivoz=1
      ires=1
      dres=one
      ifh=0
      dfft=one
      kwtype=7878
      itf=0
      icr=0
      idis=0
      icow=0
      istw=0
      iffw=0
      nprint=1
      ndafi=1
      itco=50
      dma=c1m12
      dmap=c1m15
      itcro=10
      dech=c1m10
      de0=c1m9
      ded=c1m9
      dsi=c1m9
      dsm0=c1m10
      itqv=10
      dkq=c1m10
      dqq=c1m10
      ichrom=0
      iqmod=0
      im=0
      ilin=0
      nlin=0
      iout=0
      idp=0
      izu0=0
      mmac=1
      mcut=0
      mout=0
      mout1=0
      mout2=0
      mout3=0
      mout4=0
      kanf=1
      iclo=0
      isub=0
      irmod2=0
      iorg=0
      ise=0
      irip=0
      irco=0
      iskew=0
      preda=c1m38
   90 read(3,10010,end=1530,iostat=ierro) idat,ihead
      if(ierro.gt.0) call error(58)
      if(idat(1:1).eq.'/') goto 90
      if(idat.ne.free.and.idat.ne.geom) call error(1)
      imod=1
      if(idat.eq.geom) imod=2
      write(6,10130)
      write(6,10030)
      write(6,10180) ihead
      sixtit(1:60)=ihead
      if(imod.eq.1) write(6,10190)
      if(imod.eq.2) write(6,10200)
      write(6,10130)
      if(imod.eq.2) then
  100   read(2,10000,end=1520,iostat=ierro) idat
        if(ierro.gt.0) call error(57)
        if(idat(1:1).eq.'/') goto 100
        if(idat.eq.sing) goto 120
        call error(15)
      endif
  110 read(3,10000,end=1530,iostat=ierro) idat
      if(ierro.gt.0) call error(58)
      if(idat(1:1).eq.'/') goto 110
      if(idat.eq.sing) goto 120
      if(idat.eq.bloc) goto 190
      if(idat.eq.stru) goto 320
      if(idat.eq.prin) goto 550
      if(idat.eq.disp) goto 170
      if(idat.eq.tune) goto 600
      if(idat.eq.sync) goto 710
      if(idat.eq.iter) goto 940
      if(idat.eq.fluc) goto 790
      if(idat.eq.mult) goto 740
      if(idat.eq.chro) goto 560
      if(idat.eq.trac) goto 510
      if(idat.eq.diff) goto 520
      if(idat.eq.line) goto 660
      if(idat.eq.limi) goto 950
      if(idat.eq.orbi) goto 980
      if(idat.eq.init) goto 500
      if(idat.eq.comb) goto 1030
      if(idat.eq.subr) goto 1110
      if(idat.eq.reso) goto 1120
      if(idat.eq.sear) goto 1200
      if(idat.eq.orga) goto 880
      if(idat.eq.post) goto 1280
      if(idat.eq.ripp) goto 1290
      if(idat.eq.deco) goto 1320
      if(idat.eq.comm) goto 1390
      if(idat.eq.norm) goto 1400
      if(idat.eq.corr) goto 1410
      if(idat.eq.beam) goto 1600
      if(idat.eq.next) goto 110
      if(idat.eq.ende) goto 771
      call error(15)
C-----------------------------------------------------------------------
C  DATENBLOCK SINGLE ELEMENTS
C  ELLEMENTLISTE
C-----------------------------------------------------------------------
  120 i=1
  130 if(imod.eq.1) then
  140   read(3,10020,end=1530,iostat=ierro) ch
        if(ierro.gt.0) call error(58)
        if(ch(1:1).eq.'/') goto 140
        if(ch(:4).eq.next) goto 110
      else if(imod.eq.2) then
  150   read(2,10020,end=1520,iostat=ierro) ch
        if(ierro.gt.0) call error(57)
        if(ch(1:1).eq.'/') goto 150
        if(ch(:4).eq.next) then
  160     read(2,10000,end=1520,iostat=ierro) idat
          if(ierro.gt.0) call error(57)
          if(idat(1:1).eq.'/') goto 160
          if(idat.ne.bloc) call error(15)
          goto 190
        endif
      endif
      call intepr(1,1,ch,ch1)
      read(11,*) idat,kz(i),ed(i),ek(i),el(i)
C--CHANGING SIGN OF CURVATURE OF VERTICAL THICK DIPOLE
      if((kz(i).eq.4.or.kz(i).eq.5).and.abs(el(i)).gt.pieni)
     &ed(i)=-ed(i)
C--THIN LENS
      if(kz(i).eq.11.and.abs(el(i)+one).le.pieni) then
        dki(i,1) = ed(i)
        dki(i,3) = ek(i)
        ed(i) = one
        ek(i) = one
        el(i) = zero
      else if(kz(i).eq.11.and.abs(el(i)+two).le.pieni) then
        dki(i,2) = ed(i)
        dki(i,3) = ek(i)
        ed(i) = one
        ek(i) = one
        el(i) = zero
      endif
C--CAVITIES
      if(abs(kz(i)).eq.12) then
        if(abs(ed(i)).gt.pieni.and.abs(ek(i)).gt.pieni) then
          ncy2=ncy2+1
          itionc(i)=kz(i)/abs(kz(i))
          kp(i)=6
        endif
        phasc(i)=el(i)
        el(i)=zero
      endif     
C--BEAM-BEAM smallest displacement
      if(abs(kz(i)).eq.20) then
        if(abs(ed(i)).lt.c1m12) ed(i)=c1m12
        if(abs(ek(i)).lt.c1m12) ek(i)=c1m12
      endif
      if(abs(el(i)).gt.pieni.and.kz(i).ne.0) ithick=1
      if(i.gt.nele-1) call error(16)
      if(abs(kz(i)).ne.12) kp(i)=0
      bez(i)=idat
      bez0(i)=idat
      if(ncy2.eq.0) then
        i=i+1
        il=i
        bez(i)=cavi
        bez0(i)=cavi
        kp(i)=6
      else
        il=i
        i=i+1
      endif
      goto 130
C-----------------------------------------------------------------------
C  DATENBLOCK DISPLACEMENT OF ELEMENTS
C-----------------------------------------------------------------------
  170 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 170
      if(ch(:4).eq.next) goto 110
      call intepr(1,1,ch,ch1)
      xpl0=zero
      xrms0=zero
      zpl0=zero
      zrms0=zero
      read(11,*) idat,xpl0,xrms0,zpl0,zrms0
      do 180 j=1,il
        if(idat.ne.bez(j)) goto 180
        xpl(j)=xpl0
        xrms(j)=xrms0
        zpl(j)=zpl0
        zrms(j)=zrms0
  180 continue
      goto 170
C-----------------------------------------------------------------------
C  BLOCK DEFINITIONS
C-----------------------------------------------------------------------
  190 if(imod.eq.1) then
  200   read(3,10020,end=1530,iostat=ierro) ch
        if(ierro.gt.0) call error(58)
        if(ch(1:1).eq.'/') goto 200
      endif
      if(imod.eq.2) then
  210   read(2,10020,end=1520,iostat=ierro) ch
        if(ierro.gt.0) call error(57)
        if(ch(1:1).eq.'/') goto 210
      endif
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) mper,(msym(k),k=1,mper)
      if(mper.gt.nper) call error(17)
      i=0
  220 do 230 m=1,40
  230 ilm0(m)=idum
      if(imod.eq.1) then
  240   read(3,10020,end=1530,iostat=ierro) ch
        if(ierro.gt.0) call error(58)
        if(ch(1:1).eq.'/') goto 240
        if(ch(:4).eq.next) goto 110
      else if(imod.eq.2) then
  250   read(2,10020,end=1520,iostat=ierro) ch
        if(ierro.gt.0) call error(57)
        if(ch(1:1).eq.'/') goto 250
        if(ch(:4).eq.next) then
  260     read(2,10000,end=1520,iostat=ierro) idat
          if(ierro.gt.0) call error(57)
          if(idat(1:1).eq.'/') goto 260
          if(idat.ne.stru) call error(15)
          goto 320
        endif
      endif
      call intepr(2,1,ch,ch1)
      read(11,*) idat,(ilm0(m),m=1,40)
      if(idat.eq.idum) goto 270
      i=i+1
      if(i.gt.nblo-1) call error(18)
      bezb(i)=idat
      k0=0
      mblo=i
  270 ka=k0+1
      ke=k0+40
      do 300 l=ka,ke
        if(l.gt.nelb) call error(26)
        ilm(l)=ilm0(l-k0)
        if(ilm(l).eq.idum) goto 310
        mel(i)=l
        beze(i,l)=ilm(l)
        do 280 j=1,il
          if(bez0(j).eq.ilm(l)) goto 290
  280   continue
        erbez=ilm(l)
        call error(19)
  290   mtyp(i,l)=j
        if(kz(j).ne.8) elbe(i)=elbe(i)+el(j)
  300 continue
  310 k0=l-1
      goto 220
C-----------------------------------------------------------------------
C  STRUCTURE INPUT
C-----------------------------------------------------------------------
  320 i=0
  330 do 340 k=1,40
  340 ilm0(k)=idum
      if(imod.eq.1) then
  350   read(3,10020,end=1530,iostat=ierro) ch
        if(ierro.gt.0) call error(58)
        if(ch(1:1).eq.'/') goto 350
      endif
      if(imod.eq.2) then
  360   read(2,10020,end=1520,iostat=ierro) ch
        if(ierro.gt.0) call error(57)
        if(ch(1:1).eq.'/') goto 360
      endif
      if(ch(:4).eq.next) goto 110
      i2=1
      do 420 ii=1,80
        if(ch(ii:ii).eq.kl) then
          if(ii.gt.1) then
            do 370 jj=1,ii-1
  370       if(ch(jj:jj).ne.' ') goto 380
          endif
          iw=1
          goto 390
  380     rewind 11
          write(11,*,iostat=ierro) ch(:ii-1)
          if(ierro.ne.0) call error(59)
          rewind 11
          read(11,*) iw
  390     ia=i
          iw0=iw-1
          i2=ii+1
          goto 430
        endif
        if(ch(ii:ii).eq.kr) then
          if(iw0.le.0) goto 330
          idi=i-ia
          do 410 k=1,iw0
            do 400 j=1,idi
  400       ic(i+j)=ic(i+j-idi)
            i=i+idi
  410     continue
          mbloz=i
          goto 330
        endif
  420 continue
  430 call intepr(3,i2,ch,ch1)
      read(11,*) (ilm0(k),k=1,40)
      do 490 k=1,40
        if(ilm0(k).eq.idum) goto 490
        if(ilm0(k).eq.go) goto 480
        i=i+1
        do 440 j=1,mblo
          if(bezb(j).eq.ilm0(k)) goto 470
  440   continue
        do 450 l=1,il
          if(bez0(l).eq.ilm0(k)) goto 460
  450   continue
        erbez=ilm0(k)
        call error(20)
  460   continue
        ic(i)=l+nblo
        if(bez0(l).eq.cavi) icy=icy+1
        goto 490
  470   ic(i)=j
        goto 490
  480   kanf=i+1
  490 continue
      mbloz=i
      if(mbloz.gt.nblz-2) call error(21)
      goto 330
C-----------------------------------------------------------------------
C  INITIAL COORDINATES
C-----------------------------------------------------------------------
  500 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).ne.'/') then
        iclr=iclr+1
      else
        goto 500
      endif
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      if(iclr.eq.1) then
        read(11,*) itra,chi0,chid,rat,iver
        if(itra.gt.2) call error(40)
      endif
      if(iclr.eq.2) read(11,*) exz(1,1)
      if(iclr.eq.3) read(11,*) exz(1,2)
      if(iclr.eq.4) read(11,*) exz(1,3)
      if(iclr.eq.5) read(11,*) exz(1,4)
      if(iclr.eq.6) read(11,*) exz(1,5)
      if(iclr.eq.7) read(11,*) exz(1,6)
      if(iclr.eq.8) read(11,*) exz(2,1)
      if(iclr.eq.9) read(11,*) exz(2,2)
      if(iclr.eq.10) read(11,*) exz(2,3)
      if(iclr.eq.11) read(11,*) exz(2,4)
      if(iclr.eq.12) read(11,*) exz(2,5)
      if(iclr.eq.13) read(11,*) exz(2,6)
      if(iclr.eq.14) read(11,*) e0
      if(iclr.eq.15) read(11,*) ej(1)
      if(iclr.eq.16) read(11,*) ej(2)
      if(iclr.ne.16) goto 500
      dp1=exz(1,6)
      iclr=0
      if(iver.ne.0.and.iver.ne.1) iver=0
      nbidu=1
      goto 110
C-----------------------------------------------------------------------
C  TRACKING PARAMETERS
C-----------------------------------------------------------------------
  510 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).ne.'/') then
        iclr=iclr+1
      else
        goto 510
      endif
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      if(iclr.eq.1) read(11,*)
     &numl,numlr,napx,amp(1),amp0,ird,imc
      if(iclr.eq.2) read(11,*) idz(1),idz(2),idfor,irew,iclo6
      if(iclr.eq.3) read(11,*) nde(1),nde(2),
     &nwr(1),nwr(2),nwr(3),nwr(4),ntwin,ibidu
      if(iclo6.eq.5.or.iclo6.eq.6) then
        iclo6=iclo6-4
        iclo6r=1
      endif
      if(iclo6.eq.2.and.idfor.eq.0) idfor=1
      if(iclo6.eq.1.or.iclo6.eq.2) nsix=0
      if(iclr.ne.3) goto 510
      iclr=0
      nbidu=1
      goto 110
C-----------------------------------------------------------------------
C  DIFFERENTIAL ALGEBRA
C-----------------------------------------------------------------------
  520 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 520
      ndum=ndum+1
      if(ch(:4).eq.next) then
        ndum=0
        goto 110
      endif
      if(ndum.eq.1) then
        ch1(:83)=ch(:80)//' / '
        idial=1
        numlr=0
        napx=1
        imc=1
        rewind 11
        write(11,*,iostat=ierro) ch1
        if(ierro.ne.0) call error(59)
        rewind 11
        read(11,*) nord,nvar,preda,nsix,ncor
        if(nord.le.0.or.nvar.le.0) call error(91)
      else
        call intepr(3,1,ch,ch1)
        read(11,*) (ilm0(i),i=1,ncor)
      endif
      if(iclo6.eq.1.or.iclo6.eq.2) nsix=0
      if(nvar.ne.6) then
      nsix=0
      iclo6=0
      endif
      if(nvar.eq.5) then
      idp=1
      ition=1
      hsy(1)=zero
      endif
      if(ndum.eq.1) then
      if(nsix.ne.1) nsix=0
      if(nord.gt.nema) call error(52)
      nvar2=nvar
      goto 520
      else
      if(ncor.gt.mcor) call error(65)
      if(ncor.gt.0) then
        do 540 j1=1,ncor
          do 530 j2=1,il
            if(ilm0(j1).eq.bez(j2)) then
              if(el(j2).ne.zero.or.kz(j2).gt.10) call error(67)
              ipar(j1)=j2
              goto 540
            endif
  530     continue
          call error(66)
  540   continue
      else
        ncor=0
        print *,' '
        print *,'NO EXTRA PARAMETERS FOR THE MAP SPECIFIED'
        print *,' '
      endif
      ndum=0
      nvar=nvar2+ncor
      goto 110
      endif
C-----------------------------------------------------------------------
C  PRINTOUT INPUT PARAMETERS
C-----------------------------------------------------------------------
  550 iout=1
      goto 110
C-----------------------------------------------------------------------
C  CHROMATCITY ADJUSTMENT
C-----------------------------------------------------------------------
  560 ichrom=1
      do 580 l=1,2
  570 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 570
      call intepr(1,1,ch,ch1)
  580 read(11,*) iss(l),cro(l)
      cro(1)=-cro(1)
      cro(2)=-cro(2)
      do 590 j=1,il
      if(iss(1).eq.bez(j)) is(1)=j
  590 if(iss(2).eq.bez(j)) is(2)=j
      goto 110
C-----------------------------------------------------------------------
C  TUNE ADJUSTMENT
C-----------------------------------------------------------------------
  600 iqmod=1
      do 630 l=1,3
  610 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 610
      if(ch(:4).eq.next) then
        if(abs(qw0(1)).gt.pieni.and.abs(qw0(2)).gt.pieni) then
          do 620 j=1,il
            if(iqq(1).eq.bez(j)) iq(1)=j
  620     if(iqq(2).eq.bez(j)) iq(2)=j
          goto 110
        else
          write(6,10370)
          iqmod=0
          goto 110
        endif
      endif
      call intepr(1,1,ch,ch1)
      if(l.eq.1) read(11,*) iqq(1),qw0(1),iqmod6
      if(l.eq.2) read(11,*) iqq(2),qw0(2)
  630 continue
  640 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 640
      call intepr(4,1,ch,ch1)
      read(11,*) iqq(4),iqq(5)
      if(abs(qw0(1)).gt.pieni.and.abs(qw0(2)).gt.pieni
     &.and.abs(qw0(3)).gt.pieni) then
      do 650 j=1,il
        if(iqq(1).eq.bez(j)) iq(1)=j
        if(iqq(2).eq.bez(j)) iq(2)=j
        if(iqq(3).eq.bez(j)) iq(3)=j
        if(iqq(4).eq.bez(j)) kpa(j)=1
  650 if(iqq(5).eq.bez(j)) kpa(j)=2
      goto 110
      else
      write(6,10370)
      iqmod=0
      goto 110
      endif
C-----------------------------------------------------------------------
C  LINEAR OPTICS CALCULATION
C-----------------------------------------------------------------------
  660 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 660
      if(ch(:4).eq.next) goto 110
      call intepr(1,1,ch,ch1)
      read(11,*) idat,nt,ntco,eui,euii
      iprint=0
      if(idat.ne.'BLOCK'.and.idat.ne.'ELEMENT') call error(45)
      if(idat.eq.'BLOCK') iprint=1
      ilin=1
  670 do 680 m=1,40
  680 ilm0(m)=idum
  690 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 690
      if(ch(:4).eq.next) goto 110
      call intepr(2,1,ch,ch1)
      read(11,*) (ilm0(m),m=1,40)
      do 700 m=1,40
      if(ilm0(m).eq.idum) goto 700
      nlin=nlin+1
      if(nlin.gt.nele) call error(81)
      bezl(nlin)=ilm0(m)
  700 continue
      goto 670
C-----------------------------------------------------------------------
C  SYNCHROTRON OSCILLATIONS
C-----------------------------------------------------------------------
  710 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).ne.'/') then
      iclr=iclr+1
      else
      goto 710
      endif
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      if(iclr.eq.1) read(11,*) harm,alc,u0,phag,tlen,pma,ition,dppoff
      if(iclr.eq.2) read(11,*) dpscor,sigcor
      if(iclr.ne.2) goto 710
      iclr=0
      if(abs(pma-pmap).le.c1m1) pmat=pmap
      if(abs(pma-pmae).le.c1m1) pmat=pmae
      if(pmat.ne.pmap.and.pmat.ne.pmae) then
        write(*,*)
        write(*,*) 'Warning: Particle is neither proton nor electron'
        write(*,*)
      endif
      if(pma.lt.pieni) call error(27)
      crad=crade*pmae/pma
      if(abs(tlen).le.pieni) call error(25)
      if(ncy2.eq.0) then
        ncy=icy*mper
        idp=1
        if(ncy.ne.0) goto 720
        idp=0
        write(6,10130)
        write(6,10340)
  720   phas=phag*rad
        if(ncy.ne.0) then
          hsy(1)=u0/dble(ncy)
        else
          hsy(1)=u0
        endif
        if(nvar.eq.5) then
          idp=1
          ition=1
          hsy(1)=zero
        endif
        halc=harm*alc
        halc2=harm/tlen
        hsy(3)=two*pi*halc2
        cosy=cos(phas)
        qigam=pma*pma/e0/e0
        qbet=one-qigam
        halc3=-(qigam-alc)*ition*harm*u0/e0*cosy/(two*pi*qbet)
        if(halc3.lt.zero) call error(28)
        qs=sqrt(halc3)
      else
        idp=1
        ncy=0
        do 725 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(abs(kz(ix)).eq.12) ncy=ncy+1
          endif
  725   continue
        do 730 j=1,il
          if(abs(kz(j)).eq.12) then
            hsyc(j)=two*pi*ek(j)/tlen
            if(nvar.eq.5) then
              ition=1
              ed(j)=zero
            endif
          endif
  730   continue        
      endif
      goto 110
C-----------------------------------------------------------------------
C  MULTIPOLE COEFFICIENTS  FOR KZ = 11
C-----------------------------------------------------------------------
  740 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 740
      call intepr(1,1,ch,ch1)
      read(11,*) imn,r0,benki
      i=1
      r0a=one
      im=im+1
      benkc(im)=benki
      r00(im)=r0
      do 750 j=1,il
      if(imn.eq.bez(j)) then
        irm(j)=im
        goto 760
      endif
  750 continue
  760 write(6,10130)
      write(6,10210) imn,r0,benki
  770 bk0d=zero
      bkad=zero
      ak0d=zero
      akad=zero
  780 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 780
      if(ch(:4).eq.next) goto 110
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) bk0d,bkad,ak0d,akad
      if(abs(bk0d).gt.pieni.or.abs(bkad).gt.pieni
     &.or.abs(ak0d).gt.pieni.or.abs(akad).gt.pieni) nmu(j)=i
      write(6,10220) i,bk0d,bkad,ak0d,akad
      bk0(im,i)=benki*bk0d/r0a
      ak0(im,i)=benki*ak0d/r0a
      bka(im,i)=benki*bkad/r0a
      aka(im,i)=benki*akad/r0a
      i=i+1
      r0a=r0a*r0
      if(i.le.mmul+1) goto 770
      write(6,10380)
      goto 770
C-----------------------------------------------------------------------
C  FLUCTUATION RANDOM STARTING NUMBER
C-----------------------------------------------------------------------
  790 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 790
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) izu0, mmac, mout, mcut
      mcut=iabs(mcut)
      if(mmac.gt.nmac) call error(55)
      call recuin(izu0,irecuin)
      call ranecu(zfz,nzfz,mcut)
      rsum=zero
      do 800 i=1,nzfz
  800 rsum=rsum+zfz(i)
      rmean=rsum/nzfz
      rsqsum=zero
      do 810 i=1,nzfz
  810 rsqsum=rsqsum+(zfz(i)-rmean)*(zfz(i)-rmean)
      rdev=sqrt(rsqsum/nzfz)
      write(6,10410) izu0,nzfz,rmean,rdev
      if(mcut.eq.0) write(6,10430)
      if(mcut.gt.0) write(6,10440) mcut
      write(6,10130)
      if(mout.ge.8) mout4=1
      if(mout.eq.7.or.mout.eq.15) then
        mout1=1
        mout2=1
        mout3=1
      else if(mout.eq.6.or.mout.eq.14) then
        mout2=1
        mout3=1
      else if(mout.eq.5.or.mout.eq.13) then
        mout1=1
        mout3=1
      else if(mout.eq.4.or.mout.eq.12) then
        mout3=1
      else if(mout.eq.3.or.mout.eq.11) then
        mout1=1
        mout2=1
      else if(mout.eq.2.or.mout.eq.10) then
        mout2=1
      else if(mout.eq.1.or.mout.eq.9) then
        mout1=1
      endif
      if(mout1.eq.1) then
        write(6,*)
        write(6,*) '          Multipole errors read in ' ,
     +  'from external file'
        write(6,*)
        iexread=0
        ifiend16=0
        iexnum=0
        read(16,10020,end=861)
        rewind 16
        do 860 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(iexread.eq.0) then
              ilm0(1)=' '
C READ IN REGULAR MULTIPOLES FIRST AND THEN THE SKEW COMPONENTS
              if(ifiend16.eq.0) then
                read(16,10020,end=820,iostat=ierro) ch
              else
                goto 820
              endif
              call intepr(3,1,ch,ch1)
              read(11,*) ilm0(1)
              iexnum=iexnum+1
              bezext(iexnum)=ilm0(1)
              read(16,*,end=870,iostat=ierro) extaux(1),extaux(2),
     +        extaux(3)
              read(16,*,end=870,iostat=ierro) extaux(4),extaux(5),
     +        extaux(6)
              read(16,*,end=870,iostat=ierro) extaux(7),extaux(8),
     +        extaux(9)
              read(16,*,end=870,iostat=ierro) extaux(10),extaux(11),
     +        extaux(12)
              read(16,*,end=870,iostat=ierro) extaux(13),extaux(14),
     +        extaux(15)
              read(16,*,end=870,iostat=ierro) extaux(16),extaux(17),
     +        extaux(18)
              read(16,*,end=870,iostat=ierro) extaux(19),extaux(20)
              read(16,*,end=870,iostat=ierro) extaux(21),extaux(22),
     +        extaux(23)
              read(16,*,end=870,iostat=ierro) extaux(24),extaux(25),
     +        extaux(26)                                                
              read(16,*,end=870,iostat=ierro) extaux(27),extaux(28),
     +        extaux(29)                                                
              read(16,*,end=870,iostat=ierro) extaux(30),extaux(31),
     +        extaux(32)                                                
              read(16,*,end=870,iostat=ierro) extaux(33),extaux(34),
     +        extaux(35)                                                
              read(16,*,end=870,iostat=ierro) extaux(36),extaux(37),
     +        extaux(38)
              read(16,*,end=870,iostat=ierro) extaux(39),extaux(40)
              if(ierro.gt.0) call error(80)
              iexread=1
              goto 840
  820         ifiend16=1
              if(iexnum.eq.0) call error(80)
              do 830 j=1,iexnum
                if(bez(ix).eq.bezext(j)) call error(80)
  830         continue
  840         continue
            endif
            if(ilm0(1).eq.bez(ix)) then
              icext(i)=ix
              do 850 k=1,40
                exterr(i,k)=extaux(k)
  850         continue
              iexread=0
              goto 860
            endif
          endif
  860   continue
  861   continue
        write(6,*) '        From file fort.16 :',iexnum,
     +  ' values read in.'
        write(6,*)
      endif
      if(mout3.eq.1) then
        write(6,*)
        write(6,*) '          Alignment errors read in ' ,
     +  'from external file'
        write(6,*)
        iexread=0
        ifiend8=0
        iexnum=0
        read(8,10020,end=1581)
        rewind 8
        do 1580 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(iexread.eq.0) then
              ilm0(1)=' '
C READ IN HORIZONTAL AND VERTICAL MISALIGNMENT AND TILT
              if(ifiend8.eq.0) then
                read(8,10020,end=1550,iostat=ierro) ch
                if(ierro.gt.0) call error(86)
              else
                goto 1550
              endif
              call intepr(1,1,ch,ch1)
              read(11,*) ilm0(1),alignx,alignz,tilt
              iexnum=iexnum+1
              bezext(iexnum)=ilm0(1)
              iexread=1
              goto 1570
 1550         ifiend8=1
              if(iexnum.eq.0) call error(86)
              do 1560 j=1,iexnum
                if(bez(ix).eq.bezext(j)) call error(86)
 1560         continue
 1570         continue
            endif
            if(ilm0(1).eq.bez(ix)) then
              icextal(i)=ix
              extalign(i,1)=alignx
              extalign(i,2)=alignz
              extalign(i,3)=tilt
              iexread=0
              goto 1580
            endif
          endif
 1580   continue
 1581   continue
        write(6,*) '        From file fort.8 :',iexnum,
     +  ' values read in.'
        write(6,*)
      endif
      izu=0
      iexnum=0
      if(mout4.eq.1) then
        read(30,10020,end=1591)
        rewind 30
        do 1590 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            kpz=kp(ix)
            kzz=kz(ix)
            if(kpz.eq.6.or.kzz.eq.0) goto 1590
            izu=izu+3
            read(30,10020,end=1591,iostat=ierro) ch
            if(ierro.gt.0) call error(87)
            call intepr(1,1,ch,ch1)
            read(11,*) ilm0(1),zfz(izu-2)
            iexnum=iexnum+1
            if(kz(ix).eq.11) izu=izu+2*mmul
          endif
 1590   continue
        if(iexnum.gt.0) then
          write(6,*)
          write(6,*) '          Single (random) kick errors read in ' ,
     +    'from external file'
          write(6,*)
          write(6,*) '        From file fort.30 :',iexnum,
     +    ' values read in.'
          write(6,*)
        endif
        iexread=0
        ifiend8=0
        iexnum=0
        rewind 30
        do 1593 i=1,mper*mbloz
          ix=ic(i)
          if(ix.gt.nblo) then
            ix=ix-nblo
            if(iexread.eq.0) then
 1595         ilm0(1)=' '
C READ IN HORIZONTAL AND VERTICAL MISALIGNMENT AND TILT
              if(ifiend8.eq.0) then
                read(30,10020,end=1594,iostat=ierro) ch
                if(ierro.gt.0) call error(87)
              else
                goto 1594
              endif
              call intepr(1,1,ch,ch1)
              read(11,*) ilm0(1),dummy,alignx,alignz,tilt
              if((abs(alignx)+abs(alignz)+abs(tilt)).le.pieni)
     +        goto 1595
              iexnum=iexnum+1
              bezext(iexnum)=ilm0(1)
              iexread=1
              goto 1596
 1594         ifiend8=1
              do 1597 j=1,iexnum
                if(bez(ix).eq.bezext(j)) call error(87)
 1597         continue
 1596         continue
            endif
            if(ilm0(1).eq.bez(ix)) then
              icextal(i)=ix
              extalign(i,1)=alignx
              extalign(i,2)=alignz
              extalign(i,3)=tilt
              iexread=0
              goto 1593
            endif
          endif
 1593   continue
 1591   continue
      endif
      goto 110
  870 call error(80)
 
C-----------------------------------------------------------------------
C  ORGANISATION OF RANDOM NUMBERS
C-----------------------------------------------------------------------
  880 write(6,10130)
      write(6,10350)
      do 890 i=1,3
      do 890 j=1,nele
  890 bezr(i,j)=idum
  900 iorg=iorg+1
  910 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 910
      call intepr(3,1,ch,ch1)
      read(11,*) idat,bezr(2,iorg),bezr(3,iorg)
      if(idat.ne.next) then
      if(idat.ne.mult.and.idat.ne.idum.and.bezr(2,iorg).eq.idum) write
     +(6,10360) idat
      if(idat.ne.mult.and.idat.ne.idum.and.bezr(2,iorg).ne.idum) write
     +(6,10390) idat,bezr(2,iorg)
      if(idat.ne.mult) bezr(1,iorg)=idat
      if(idat.eq.mult.and.bezr(2,iorg).ne.idum.and.bezr(3,iorg).ne.idum)
     &then
        write(6,10400)bezr(2,iorg),bezr(3,iorg)
        im=im+1
        j0=0
        j1=0
        do 920 i=1,il
          if(bez(i).eq.bezr(2,iorg)) j1=i
  920   if(bez(i).eq.bezr(3,iorg)) j0=i
        if(j0.eq.0.or.j1.eq.0.or.kz(j0).ne.11.or.kz(j1).ne.11)
     +  call error(29)
        irm(j0)=im
        benkc(j0)=benkc(j1)
        r00(j0)=r00(j1)
        imo=irm(j1)
        nmu(j0)=nmu(j1)
        do 930 i1=1,nmu(j0)
          bk0(im,i1)=bk0(imo,i1)
          bka(im,i1)=bka(imo,i1)
          ak0(im,i1)=ak0(imo,i1)
  930   aka(im,i1)=aka(imo,i1)
      endif
      goto 900
      endif
      write(6,10130)
      goto 110
C-----------------------------------------------------------------------
C  ITERATION ERRORS FOR CLOSED ORBIT ,TUNE ADJUSTMENT AND CHROMATICITY
C-----------------------------------------------------------------------
  940 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).ne.'/') then
      iclr=iclr+1
      else
      goto 940
      endif
      if(ch(:4).eq.next) then
      iclr=0
      goto 110
      endif
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      if(iclr.eq.1) read(11,*) itco,dma,dmap
      if(iclr.eq.2) read(11,*) itqv,dkq,dqq
      if(iclr.eq.3) read(11,*) itcro,dsm0,dech
      if(iclr.eq.4) read(11,*) de0,ded,dsi
      if(iclr.ne.4) goto 940
      iclr=0
      goto 110
C-----------------------------------------------------------------------
C  APERTURE LIMITATIONS
C-----------------------------------------------------------------------
  950 write(6,10320)
  960 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 960
      apxx=zero
      apzz=zero
      call intepr(8,1,ch,ch1)
      read(11,*) idat,irel,apxx,apzz
      do 970 j=1,il
      if(idat.ne.bez(j)) goto 970
      kp(j)=1
      if(irel.eq.rect) kp(j)=2
      apx(j)=apxx
      apz(j)=apzz
      write(6,10330) bez(j),irel,apxx,apzz
  970 continue
      if(idat.ne.next) goto 960
      goto 110
C-----------------------------------------------------------------------
C  ORBIT CORRECTION
C-----------------------------------------------------------------------
  980 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 980
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) sigma0,ncorru,ncorrep
      iclo=1
  990 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 990
      iele=idum
      call intepr(4,1,ch,ch1)
      read(11,*) idat,iele
      if(idat.eq.next) goto 110
      if(idat.ne.'HMON='.and.idat.ne.'HCOR='.and. idat.ne.'VMON='.and.
     +idat.ne.'VCOR=') call error(44)
      if(idat.eq.'HMON='.or.idat.eq.'VMON=') goto 1010
      do 1000 j=1,il
      if(iele.ne.bez(j)) goto 1000
      if(idat.eq.'HCOR=') then
        if(kp(j).eq.-4.or.kp(j).eq.3.or.kp(j).eq.-3) call error(83)
        if(kz(j).ne.1.and.kz(j).ne.11) call error(82)
        kp(j)=4
      endif
      if(idat.eq.'VCOR=') then
        if(kp(j).eq.4.or.kp(j).eq.3.or.kp(j).eq.-3) call error(83)
        if(kz(j).ne.-1.and.kz(j).ne.11) call error(82)
        kp(j)=-4
      endif
 1000 continue
      goto 990
 1010 do 1020 j=1,il
      if(iele.ne.bez(j)) goto 1020
      if(idat.eq.'HMON=') then
        if(kp(j).eq.4.or.kp(j).eq.-4.or.kp(j).eq.-3) call error(83)
        kp(j)=3
      endif
      if(idat.eq.'VMON=') then
        if(kp(j).eq.4.or.kp(j).eq.-4.or.kp(j).eq.3) call error(83)
        kp(j)=-3
      endif
 1020 continue
      goto 990
C-----------------------------------------------------------------------
C  COMBINATION OF ELEMENTS
C-----------------------------------------------------------------------
 1030 ii=0
      do 1040 jj=1,ncom
      do 1040 ll=1,20
 1040 icel(jj,ll)=idum
      write(6,10130)
      write(6,10300)
 1050 ii=ii+1
      if(ii.gt.ncom) goto 1100
 1060 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1060
      if(ch(:4).eq.next) goto 110
      icoe=ii
      call intepr(5,1,ch,ch1)
      read(11,*) idat,(ratio(ii,l),icel(ii,l),l=1,20)
      do 1080 j=1,il
      if(idat.ne.bez(j)) goto 1070
      kp(j)=5
      icomb0(ii)=j
      ratioe(j)=one
 1070 do 1080 l=1,20
        if(bez(j).eq.icel(ii,l)) then
          icomb(ii,l)=j
          ratioe(j)=ratio(ii,l)
        endif
 1080 continue
      jj=icomb0(ii)
      if(jj.eq.0) goto 1050
      do 1090 m=1,20
        ico=icomb(ii,m)
        if(ico.eq.jj) call error(92)
        if(ico.eq.0) goto 1090
        write(6,10310) bez(jj),bez(ico),ratio(ii,m)
        iratioe(ico)=jj
        if(el(jj).le.pieni) then
          if(el(ico).le.pieni) then
            ed(ico)=ed(jj)
          else
            ek(ico)=ed(jj)*ratio(ii,m)
          endif
        else
          if(el(ico).le.pieni) then
            ed(ico)=ek(jj)
          else
            ek(ico)=ek(jj)*ratio(ii,m)
          endif
        endif
 1090 continue
      goto 1050
 1100 write(6,10290) ncom
      goto 110
C-----------------------------------------------------------------------
C  SUBRESONANCE CALCULATION
C-----------------------------------------------------------------------
 1110 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1110
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) nta,nte,qxt,qzt,tam1,tam2,ipt,totl
      if(nta.lt.2) call error(37)
      if(nte.lt.nta.or.nte.gt.9) call error(37)
      isub=1
      goto 110
C-----------------------------------------------------------------------
C  RESONANCE-COMPENSATION
C-----------------------------------------------------------------------
 1120 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1120
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) nre
      if(nre.ne.0) then
        rewind 11
        read(11,*) nre,npp,nrr(1),nrr(2),nrr(3),
     &  ipr(1),ipr(2),ipr(3)
      endif
      if(nre.ne.0.and.(npp.lt.2.or.npp.gt.nrco)) call error(46)
      if(nre.lt.0.or.nre.gt.3) call error(47)
      if(abs(nrr(1)).gt.npp.or.abs(nrr(2)).gt.npp.
     &or.abs(nrr(3)).gt.npp) call error(48)
 1130 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1130
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) nur
      if(nur.ne.0) then
        rewind 11
        read(11,*) nur,nu(1),nu(2),nu(3)
      endif
      if(nur.lt.0.or.nur.gt.3) call error(49)
      if(nu(1).gt.9.or.nu(2).gt.9.or.nu(3).gt.9.
     &or.nu(1).lt.0.or.nu(2).lt.0.or.nu(3).lt.0) call error(50)
 1140 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1140
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) totl,qxt,qzt,tam1,tam2
 1150 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1150
      call intepr(3,1,ch,ch1)
      read(11,*) (ilm0(i),i=1,6)
 1160 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1160
      call intepr(6,1,ch,ch1)
      read(11,*) nch
      if(nch.ne.0) then
        rewind 11
        read(11,*) nch,ilm0(7),ilm0(8)
      endif
 1170 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1170
      call intepr(7,1,ch,ch1)
      read(11,*) nqc
      if(nqc.ne.0) then
        rewind 11
        read(11,*) nqc,ilm0(9),ilm0(10),qw0
      endif
      do 1190 k=1,10
      do 1180 j=1,il
        if(ilm0(k).ne.bez(j)) goto 1180
        ire(k)=j
        if(nre.eq.1.and.k.lt.3.and.abs(kz(j)).ne.npp) call error(39)
        if(nre.eq.2.and.k.lt.5.and.abs(kz(j)).ne.npp) call error(39)
        if(nre.eq.3.and.k.lt.7.and.abs(kz(j)).ne.npp) call error(39)
        if(nch.eq.1.and.(k.eq.7.or.k.eq.8).and.kz(j).ne.3) call error
     +  (11)
        if(nqc.eq.1.and.(k.eq.9.or.k.eq.10).and.kz(j).ne.2) call error
     +  (8)
        goto 1190
 1180 continue
      if((nre.eq.1.and.k.lt.3).or.(nre.eq.2.and.k.lt.5).or.
     &(nre.eq.3.and.k.lt.7).or.(nch.eq.1.and.(k.eq.7.or.k.eq.8)).or.
     &(nqc.eq.1.and.(k.eq.9.or.k.eq.10))) call error(3)
 1190 continue
      irmod2=1
      goto 110
C-----------------------------------------------------------------------
C  SEARCH FOR OPTIMUM PLACES TO COMPENSATE RESONANCES
C-----------------------------------------------------------------------
 1200 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1200
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) qxt,qzt,tam1,tam2,totl
 1210 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1210
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) mesa,mp,m21,m22,m23,ise1,ise2,ise3
      if(mp.lt.2.or.mp.gt.9) call error(37)
      if(abs(m21).gt.mp.or.abs(m22).gt.mp.
     &or.abs(m23).gt.mp) call error(48)
      ise=1
      k0=0
 1220 do 1230 m=1,40
 1230 ilm0(m)=idum
 1240 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1240
      call intepr(3,1,ch,ch1)
      read(11,*) idat,(ilm0(m),m=2,40)
      if(idat.eq.next) goto 110
      ilm0(1)=idat
      ka=k0+1
      ke=k0+40
      do 1260 k=ka,ke
      if(k.gt.nele) call error(2)
      if(k.gt.mesa) goto 110
      ki=k-k0
      if(ilm0(ki).eq.idum) goto 1270
      do 1250 j=1,il
        if(ilm0(ki).ne.bez(j)) goto 1250
        isea(k)=j
        if(abs(kz(j)).ne.mp) call error(39)
        goto 1260
 1250 continue
      call error(3)
 1260 continue
 1270 k0=k-1
      goto 1220
C-----------------------------------------------------------------------
C  POSTPROCESSING
C-----------------------------------------------------------------------
 1280 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).ne.'/') then
      iclr=iclr+1
      else
      goto 1280
      endif
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      if(iclr.eq.1) toptit(1)=ch
      if(iclr.eq.2) read(11,*) iav,nstart,nstop,iwg,dphix,dphiz,
     &iskip,iconv,imad,cma1,cma2
      if(iclr.eq.3) read(11,*) qx0,qz0,ivox,ivoz,ires,dres,ifh,dfft
      if(iclr.eq.4) read(11,*) kwtype,itf,icr,idis,icow,istw,iffw,
     &nprint,ndafi
      if(iskip.le.0) iskip=1
      if(iclr.ne.4) goto 1280
      if(nprint.ne.1) nprint=0
      iclr=0
      if(nstart.lt.0) nstart=0
      if(nstop.lt.0) nstop=0
      if(nstop.lt.nstart) then
      nstart=0
      nstop=0
      endif
      if(itf.eq.-1) itf=-20
      if(itf.eq.1) itf=20
      if(itf.eq.0) itf=0
      if(iconv.ne.1) iconv=0
      if(abs(cma1).le.pieni) cma1=one
      cma1=cma1*c1e3
      if(abs(cma2).le.pieni) cma2=one
      ipos=1
      goto 110
C-----------------------------------------------------------------------
C  POWER SUPPLY RIPPLE
C-----------------------------------------------------------------------
 1290 irip=1
 1300 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1300
      if(ch(:4).eq.next) goto 110
      call intepr(1,1,ch,ch1)
      irco=irco+1
      if(irco.gt.nele) call error(51)
      read(11,*) idat,ram,rfr,rph,nrturn
      do 1310 j=1,il
      if(idat.eq.bez(j)) then
        nrel(irco)=j
        ramp(j)=ram
        rfre(j)=rfr
        rzph(j)=rph
        goto 1300
      endif
 1310 continue
      goto 1300
C-----------------------------------------------------------------------
C  DECOUPLING ROUTINE
C-----------------------------------------------------------------------
 1320 iskew=1
 1330 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1330
      call intepr(3,1,ch,ch1)
      read(11,*) idat,(ilm0(m),m=2,4)
      if(idat.eq.next) then
      iskew=0
      goto 110
      endif
      ilm0(1)=idat
      do 1350 i=1,2
 1340 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1340
      if(ch(:4).eq.next) then
        iskew=2
        goto 1360
      endif
      call intepr(1,1,ch,ch1)
      read(11,*) ilm0(4+i),qwsk(i)
 1350 continue
 1360 continue
      do 1380 i=1,6
      do 1380 j=1,il
        if(iskew.eq.2.and.i.gt.4) goto 1380
        if(ilm0(i).eq.bez(j)) then
          if(i.le.4) then
            if(kz(j).ne.-2) call error(62)
          else
            if(kz(j).ne.2) call error(8)
          endif
          nskew(i)=j
          do 1370 i2=1,6
            if(nskew(i2).ne.0.and.(nskew(i2).eq.nskew(i)) .and.(i2.ne.i)
     +      ) call error(63)
 1370     continue
        endif
 1380 continue
      goto 110
C-----------------------------------------------------------------------
C  COMMENT LINE
C-----------------------------------------------------------------------
 1390 read(3,10020,end=1530,iostat=ierro) commen
      if(ierro.gt.0) call error(58)
      if(commen(1:1).eq.'/') goto 1390
      if(commen(:4).eq.next) then
      commen=' '
      endif
      goto 110
C-----------------------------------------------------------------------
C  NORMAL FORMS
C-----------------------------------------------------------------------
 1400 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1400
      if(ch(:4).eq.next) then
      goto 110
      else
      if(idial.eq.0.and.numl.ne.0) then
        write(6,10130)
        write(6,*)
        call error(78)
      endif
      inorm=1
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) nordf,nvarf
      if(nord.ne.0.and.nordf.gt.nord+1) then
        nordf=nord+1
        imod1=1
      endif
      if(nvar.ne.0.and.nvarf.gt.nvar) then
        nvarf=nvar
        imod2=1
      endif
      endif
C-----------------------------------------------------------------------
C  TUNESHIFT CORRECTIONS
C-----------------------------------------------------------------------
 1410 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1410
      if(ch(:4).eq.next) goto 110
      icorr=1
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) nctype,ncor
      if(ncor.gt.mcor) call error(65)
      if(ncor.gt.0) then
      read(3,10020,end=1530,iostat=ierro) ch
      ch1(:83)=ch(:80)//' / '
      call intepr(3,1,ch,ch1)
      read(11,*) (coel(i),i=1,ncor)
      do 1430 j1=1,ncor
        do 1420 j2=1,il
          if(coel(j1).eq.bez(j2)) then
            if(el(j2).ne.zero.or.kz(j2).gt.10) call error(67)
            ipar(j1)=j2
            goto 1430
          endif
 1420   continue
        call error(66)
 1430 continue
      else
      call error(70)
      endif
      if(nctype.eq.0) then
      read(3,*) namp,nmom,dummy,dummy,dummy
      if(namp+nmom.eq.0) call error(71)
      if(namp*nmom.ne.0) call error(72)
      if(namp.lt.0.or.namp.gt.2) call error(73)
      if(nmom.lt.0.or.nmom.eq.1.or.nmom.gt.3) call error(74)
      if(namp.eq.1.or.nmom.eq.2) then
        nord=6
      else
        nord=7
      endif
      else
      read(3,*) nmom1,nmom2,weig1,weig2,dpmax
      if(nmom1.lt.2.or.nmom1.gt.3) call error(75)
      if(nmom1.gt.nmom2) call error(76)
      if(nmom2.lt.2.or.nmom2.gt.3) call error(77)
      nord=2*(nmom2+1)
      endif
C-----------------------------------------------------------------------
      idial=1
      numlr=0
C     NUML=1
      napx=1
      imc=1
      preda=1.d-38
      nsix=1
      nvar=5
      nvar2=nvar
      nvar=nvar2+ncor
C-----------------------------------------------------------------------
      inorm=1
      nordf=nord+1
      nvarf=nvar
C-----------------------------------------------------------------------
      goto 1410
C-----------------------------------------------------------------------
C  Beam-Beam Element
C-----------------------------------------------------------------------
 1600 read(3,10020,end=1530,iostat=ierro) ch
      if(ierro.gt.0) call error(58)
      if(ch(1:1).eq.'/') goto 1600
      if(ch(:4).eq.next) goto 110
      ch1(:83)=ch(:80)//' / '
      rewind 11
      write(11,*,iostat=ierro) ch1
      if(ierro.ne.0) call error(59)
      rewind 11
      read(11,*) partnum,emitnx,emitnz,ibeco,ibtyp
      if(emitnx.le.pieni.or.emitnz.le.pieni) call error(88)
      nbeam=1
      if(ibtyp.eq.1) call wzset
      goto 110
C-----------------------------------------------------------------------
  771 if(napx.ge.1) then
        if(e0.lt.pieni.or.e0.le.pma) call error(27)
        gammar=pma/e0
      endif
      if(iout.eq.0) return
      write(6,10050)
      write(6,10060)
      il1=il
      if(ncy2.eq.0) il1=il-1
      do 1610 k=1,il1
      if(abs(kz(k)).eq.12) then
        write(6,10070) k,bez(k),kz(k),ed(k),ek(k),phasc(k),xpl(k),
     +  xrms(k),zpl(k),zrms(k)
        kz(k)=abs(kz(k))
        phasc(k)=phasc(k)*rad
      else
        write(6,10070) k,bez(k),kz(k),ed(k),ek(k),el(k),xpl(k),xrms(k),
     +  zpl(k),zrms(k)
      endif
 1610 continue  
      write(6,10130)
      write(6,10080)
      write(6,10090) mper,(msym(k),k=1,mper)
      write(6,10250) mblo,mbloz
      write(6,10100)
      do 1450 l=1,mblo
      kk=mel(l)
      ll=kk/6
      if(ll.ne.0) then
        do 1440 l1=1,ll
          l2=(l1-1)*6+1
          l3=l2+5
          if(l2.eq.1) then
            write(6,10260) l,bezb(l),kk,(beze(l,k),k=1,6)
          else
            write(6,10270) (beze(l,k),k=l2,l3)
          endif
 1440   continue
        if(mod(kk,6).ne.0) then
          l4=ll*6+1
          write(6,10270) (beze(l,k),k=l4,kk)
        endif
      else
        write(6,10260) l,bezb(l),kk,(beze(l,k),k=1,kk)
      endif
 1450 continue
      write(6,10120)
      mblozz=mbloz/5+1
      do 1480 k=1,mblozz
      k10=(k-1)*5
      if((mbloz-k10).eq.0) goto 1480
      do 1470 l=1,5
        if((k10+l).gt.mbloz) ic0(l)=' '
        if((k10+l).gt.mbloz) goto 1470
        icc=ic(k10+l)
        if(icc.gt.nblo) goto 1460
        ic0(l)=bezb(icc)
        goto 1470
 1460   ic0(l)=bez0(icc-nblo)
 1470 continue
      k11=k10+1
      write(6,10280) k11,(ic0(l),l=1,5)
 1480 continue
      write(6,10130)
 1490 if(idp.eq.0) goto 1500
      if(nbeam.ge.1) then
        if(partnum.gt.zero) then
          write(6,10140) ncy,dp1,dppoff,tlen,pma,partnum,ibeco,
     +    ibtyp,emitnx,emitnz,e0,dpscor*c1e3,sigcor
        else
          write(6,10141) ncy,dp1,dppoff,tlen,pma,abs(partnum),ibeco,
     +    ibtyp,emitnx,emitnz,e0,dpscor*c1e3,sigcor
        endif
      else
        write(6,10142) ncy,dp1,dppoff,tlen,pma,e0,dpscor
     +  *c1e3,sigcor
      endif
      if(ncy2.eq.0) then
        write(6,10143) harm,u0,phag,qs,alc
      else
        write(6,*)
      endif
      write(6,10130)
 1500 continue
      write(6,10150)
      nfb=nde(1)
      nac=nde(2)
      nft=numl-nde(2)
      if(numl.le.nde(2)) nft=0
      if(numl.le.nde(2)) nac=numl
      if(numl.le.nde(1)) nac=0
      if(numl.le.nde(1)) nfb=numl
      write(6,10160) numl,numlr,nwr(4),nfb,nwr(1),nac,nwr(2),nft,nwr(3),
     +kanf,amp(1),rat,itco,dma,dmap,itqv,dkq,dqq
      write(6,10170) itcro,dsm0,dech,de0,ded,dsi
      if(irip.eq.1) then
      write(6,10230)
      do 1510 i=1,irco
        j=nrel(i)
 1510 write(6,10240) bez(j),ramp(j),rfre(j),rzph(j),nrturn
      endif
      write(6,10130)
      write(6,10040)
      write(6,10130)
      goto 1540
 1520 call error(41)
 1530 call error(42)
 1540 continue
      if(2*mmac*imc*napx.gt.npart) call error(54)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      return
10000 format(11(a4,1x))
10010 format(a4,8x,a60)
10020 format(a80)
10030 format(t10,22(1ho)/t10,2(1ho),18x,2(1ho)/t10,
     +'OO  SIXTRACK-INPUT  OO', /t10,2(1ho),18x,2(1ho)/t10,22(1ho))
10040 format(t10,21(1ho)/t10,2(1ho),17x,2(1ho)/t10,
     +'OO  PREPROCESSING  OO', /t10,2(1ho),17x,2(1ho)/t10,21(1ho))
10050 format(//131('-')//t43,'*** RING PARAMETERS ***'/)
10060 format(t30,'SINGLE ELEMENTS:'/'  NO   NAME  TYP      ',
     +' 1/RHO          STRENGTH          LENGTH           X-POS     ',
     +'     X-RMS            Z-PO          Z-RMS     ' /131('-'))
10080 format(1h /t30,'RINGSTRUCTURE:'//)
10090 format(t10,'NO. OF SUPERPERIODS AND SYMMETRY ' ,t50,i3,'   ',15i4,
     +'   ')
10100 format(//131('-')//t30,'BLOCKSTRUCTURE:'/ t30,
     +'(BLOCKTYP--NO. OF SINGLE ELEMENTS--SINGLE ELEMENT TYPES)'//)
10110 format(t10,i3,' ---',i3,' --- ',30i3)
10120 format(//131('-')//t30,'BLOCKSTRUCTURE OF SUPERPERIOD:'//)
10130 format(1h /131('-')/)
10140 format(1h , t30,'SYNCHROTRON OSCILLATIONS AND BEAM-BEAM'//
     +t10,'NUMBER OF CAVITIES    ', t76,i4/
     +t10,'MOMENTUM AMPLITUDE DP/P ',t66,f14.9/
     +t10,'OFFSET MOMENTUM AMPLITUDE DP/P ',t66,f14.9/
     +t10,'MACHINE LENGTH IN (M) ', t63,f17.9/
     +t10,'PARTICLE MASS (MEV) ', t66,f14.9/
     +t10,'PARTICLE NUMBER ',t66,1pe14.7/
     +t10,'BEAMS HAVE SAME CHARGE'/
     +t10,'CLOSED ORBIT DUE TO BEAM-BEAM KICK (0=LEFT,1=SUBTRACTED) : ',
     +t79,i1/
     +t10,'FAST BEAM-BEAM KICK SWITCH (0=OFF,1=ON) : ',t79,i1/
     +t10,'NORMALIZED HORIZONTAL EMMITTANCE (mu-meter rad)',t71,0pf9.4/
     +t10,'NORMALIZED VERTICAL EMMITTANCE (mu-meter rad)',t71,f9.4/
     +t10,'ENERGY IN (MEV)',t66,f14.3/
     +t10,'DPS CORR.-FACTOR           ',t66,f14.9/
     +t10,'SIG CORR.-FACTOR           ',t66,f14.9)
10141 format(1h , t30,'SYNCHROTRON OSCILLATIONS AND BEAM-BEAM'//
     +t10,'NUMBER OF CAVITIES    ', t76,i4/
     +t10,'MOMENTUM AMPLITUDE DP/P ',t66,f14.9/
     +t10,'OFFSET MOMENTUM AMPLITUDE DP/P ',t66,f14.9/
     +t10,'MACHINE LENGTH IN (M) ', t63,f17.9/
     +t10,'PARTICLE MASS (MEV) ', t66,f14.9/
     +t10,'PARTICLE NUMBER ',t66,1pe14.7/
     +t10,'BEAMS HAVE OPPOSITE CHARGE'/
     +t10,'CLOSED ORBIT DUE TO BEAM-BEAM KICK (0=LEFT,1=SUBTRACTED) : ',
     +t79,i1/
     +t10,'FAST BEAM-BEAM KICK SWITCH (0=OFF,1=ON) : ',t79,i1/
     +t10,'NORMALIZED HORIZONTAL EMMITTANCE (mu-meter rad)',t71,0pf9.4/
     +t10,'NORMALIZED VERTICAL EMMITTANCE (mu-meter rad)',t71,f9.4/
     +t10,'ENERGY IN (MEV)',t66,f14.3/
     +t10,'DPS CORR.-FACTOR           ',t66,f14.9/
     +t10,'SIG CORR.-FACTOR           ',t66,f14.9)
10142 format(1h , t30,'SYNCHROTRON OSCILLATIONS'//
     +t10,'NUMBER OF CAVITIES    ', t76,i4/
     +t10,'MOMENTUM AMPLITUDE DP/P ',t66,f14.9/
     +t10,'OFFSET MOMENTUM AMPLITUDE DP/P ',t66,f14.9/
     +t10,'MACHINE LENGTH IN (M) ', t63,f17.9/
     +t10,'PARTICLE MASS (MEV) ', t66,f14.9/
     +t10,'ENERGY IN (MEV)',t66,f14.3/
     +t10,'DPS CORR.-FACTOR           ',t66,f14.9/
     +t10,'SIG CORR.-FACTOR           ',t66,f14.9)
10143 format(
     +t10,'HARMONIC NUMBER',t74,f6.0/
     +t10,'CIRCUMF. VOLTAGE   (MV)',t66,f14.9/
     +t10,'EQUILIBRIUM PHASE     (DEG)',t66,f14.9/
     +t10,'FREQUENCY (IN UNITS OF REVOLUTION-FREQ.) QS-LINEAR',
     +    t66 ,f14.9/
     +t10,'MOMENTUM COMPACTION',t66,f14.9/)
10150 format(1h //t43,'*** TRACKING PARAMETERS ***'/)
10160 format(t10,'NUMBER OF REVOLUTIONS  ',t48,i8/ t10,
     +'NUMBER OF REVERSE-REVOLUTIONS',t48,i8/ t10,
     +'TURNS PER COOR.-PRINTOUT',t48,i8/ t10,'FLAT BOTTOM UP TO TURN ',
     +t48,i8/ t10,'TURNS PER PRINT ON DATASET',t48,i8/ t10,
     +'ACCELERATION UP TO TURN',t48,i8/ t10,'TURNS PER PRINT ON DATASET'
     +,t48,i8/ t10,'FLAT TOP NUMBER OF TURNS',t48,i8/ t10,
     +'TURNS PER PRINT ON DATASET',t48,i8/ t10,
     +'TRACKING START AT ELEMENT NO.',t48,i8/ t10,
     +'INITIAL AMPLITUDE-H IN (MM)',t49,f7.3/ t10,
     +'COUPLING  EPS-Z/EPS-X',t49,f7.3/ t10,
     +'NUMBER OF C.-O. ITERATIONS ',t48,i8/ t10,
     +'PRECISION OF C.-O. DEVIATION',t47,d9.3/ t10,
     +'PRECISION OF C.-O. SLOPE   ',t47,d9.3/ t10,
     +'NUMBER OF Q-ADJ. ITERATIONS',t48,i8/ t10,
     +'CHANGE IN K-STRENGTH BY',t47,d9.3/ t10,
     +'PRECISION OF Q-ADJUSTEMENT',t47,d9.3)
10170 format(t10,'NUMBER OF CHROMAT.-ADJ. ITER.',t48,i8/ t10,
     +'CHANGE IN SEX.-STRENGTH BY',t47,d9.3/ t10,
     +'PRECISION OF CHROMAT.-ADJ.',t47,d9.3/ t10,
     +'DP-INTERVAL F. CROMAT.-ADJ.',t47,d9.3/ t10,
     +'DP-INTERVAL FOR DISPERSION',t47,d9.3/ t10,
     +'PRECISION FOR C.-O. RMS',t47,d9.3/)
10180 format(t5/t10,a60)
10190 format(t10,'PROGRAM MODE : FREE FORMAT INPUT')
10200 format(t10,'PROGRAM MODE : FREE FORMAT INPUT --READ FROM ',
     +'EXTRA GEOMETRY STRENGTH FILE--')
10220 format(t10,i4,2(' ',d15.8),5x,2(' ',d15.8))
10230 format(//131('-')//t10,'DATA BLOCK RIPPLE OF POWER SUPPLIES'//
     +t10,'ELEMENT',6x,'AMPLITUDE',9x,'FREQUENCY' ,9x,'STARTPHASE',9x,
     +'INI. TURNNUMBER'/t10,62('-')/)
10250 format(t10,'NUMBER OF DIFFERENT BLOCKS',t50,i3/ t10,
     +'BLOCKS PER PERIOD',t49,i5//)
10290 format(t10,'MORE THAN ',i5,' COMBINATIONS SPECIFIED'/)
10300 format(//131('-')//t10,'DATA BLOCK COMBINATION OF ELEMENTS',
     +'  THE FOLLOWING ELEMENTS ARE RELATED IN STRENGTHS--->'/ t10,
     +'ELEMENT RELATED TO ELEMENT BY THE RATIO'/)
10320 format(//131('-')//t10,'DATA BLOCK APERTURE LIMITATIONS'/ /t10,
     +'TYP',t20,'FORM',t30,'APERT-H',t40,'APERT-V')
10340 format(t10,'NO CAVITIES SPECIFIED'/)
10350 format(//131('-')//t10,'DATA BLOCK ORGANISATION OF RANDOM NUMBERS'
     +/5x,'|          |      OWN RANDOM NUMBERS      |      SAME RAN' ,
     +'DOM NUMBERS      |   SAME MULTIPOLECOEFFICIENTS  |'/131('-'))
10370 format(t10,'DESIRED TUNE TO ADJUST IS ZERO'/ t10,
     +'DATA BLOCK TUNE ADJUSTMENT  IGNORED')
10380 format(t10,'HIGHER MULTIPOLES THAN 20-POLES ARE NOT ALLOWED' ,
     +' AND THEREFORE IGNORED')
10410 format(//131('-')//t10,'DATA BLOCK FLUCTUATIONS OF MULTIPOLES'//
     +t10,'RANDOM STARTING NUMBER=  ',i20/ t10,
     +'RANDOM NUMBERS GENERATED:',i20/ t10,'MEAN VALUE=',f15.7,
     +'  -   DEVIATION=',f15.7)
10420 format(t10,22(1ho)/t10,2(1ho),18x,2(1ho)/t10,
     +'OO   NORMAL FORMS   OO', /t10,2(1ho),18x,2(1ho)/t10,22(1ho))
10430 format(/5x,'No cut on random distribution'//)
10440 format(/5x,'Random distribution has been cut to: ',i4,' sigma.'//)
10070 format(1x,i3,1x,a16,1x,i3,1x,d16.10,1x,d16.10,1x,d16.10,1x,d13.7,
     +1x,d12.6,1x,d13.7,1x,d12.6)
10210 format(t10,'DATA BLOCK MULTIPOLE COEFFICIENTS'/ t10,
     +'MULTIPOLE                    ',a16/t10,'RADIUS IN MM            '
     +,f15.7/ t10,'BENDING STRENGTH IN MRAD',f15.7// t10,19x,'NORMAL',25
     +x,'      SKEW '// t10,'      MEAN            RMS-VALUE     ',
     +'       MEAN            RMS-VALUE'/)
10240 format(t10,a16,3(2x,d16.10),2x,i10)
10260 format(t4,i3,1x,a16,1x,i2,1x,6(1x,a16))
10270 format(t28,6(1x,a16))
10280 format(t3,i6,1x,5(a16,1x))
10310 format(t10,a16,10x,a16,6x,f20.15)
10330 format(t8,a16,t18,a2,t30,f8.2,t40,f8.2)
10360 format(5x,'| ELEMENT  |           ',a16,'           |           ',
     +'    |               |               |               |')
10390 format(5x,'| ELEMENTS |                              |    ',a16,
     +'   |    ',a16,'   |               |               |')
10400 format(5x,'| ELEMENTS |                              |          '
     +,'     |               |    ',a16,'   |    ',a16,'   |')
      end
