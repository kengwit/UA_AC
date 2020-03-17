C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine comnul
C-----------------------------------------------------------------------
C  SUBROUTINE TO SET THE ALL COMMON VARIABLES TO ZERO
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
      save
C-----------------------------------------------------------------------
      ncorru=0
      ncorrep=0
      nrturn=0
      ithick=0
      ierro=0
      il=0
      iclo6=0
      iclo6r=0
      mper=0
      mblo=0
      mbloz=0
      kanf=0
      iu=0
      itra=0
      napx=0
      numl=0
      numlr=0
      ird=0
      imc=0
      idp=0
      irew=0
      iorg=0
      itco=0
      itcro=0
      itqv=0
      ichrom=0
      iqmod=0
      ilin=0
      ntco=0
      nt=0
      iprint=0
      iclo=0
      icoe=0
      ise=0
      mesa=0
      mp=0
      m21=0
      m22=0
      m23=0
      ise1=0
      ise2=0
      ise3=0
      isub=0
      nta=0
      nte=0
      ipt=0
      irmod2=0
      nre=0
      nur=0
      nch=0
      nqc=0
      npp=0
      ipos=0
      iconv=0
      imad=0
      iskip=1
      iav=0
      iwg=0
      ivox=0
      ivoz=0
      ires=0
      ifh=0
      kwtype=0
      itf=0
      icr=0
      idis=0
      icow=0
      istw=0
      iffw=0
      irip=0
      irco=0
      idial=0
      nord=0
      nvar=0
      nbeam=0
      ibeco=0
      iver=0
      ibidu=0
C-----------------------------------------------------------------------
      inorm=0
      imod1=0
      imod2=0
      nordf=0
      nvarf=0
C-----------------------------------------------------------------------
      icorr=0
      nctype=0
      namp=0
      nmom=0
      nmom1=0
      nmom2=0
      weig1=zero
      weig2=zero
      dpmax=zero
C-----------------------------------------------------------------------
      pi=zero
      pi2=zero
      pisqrt=zero
      rad=zero
      chi0=zero
      chid=zero
      dp1=zero
      idfor=0
      rat=zero
      qs=zero
      e0=zero
      pma=zero
      phas0=zero
      phas=zero
      ition=0
      dpscor=one
      sigcor=one
      benki=zero
      dma=zero
      dmap=zero
      dkq=zero
      dqq=zero
      de0=zero
      ded=zero
      dsi=zero
      dech=zero
      dsm0=zero
      amp0=zero
      qxt=zero
      qzt=zero
      eui=zero
      euii=zero
      tam1=zero
      tam2=zero
      totl=zero
      nstart=0
      nstop=0
      dphix=zero
      dphiz=zero
      qx0=zero
      qz0=zero
      dres=zero
      dfft=zero
      preda=zero
      partnum=zero
      emitnx=zero
      emitnz=zero
      gammar=zero
C-----------------------------------------------------------------------
      do 10 i=1,2
        nde(i)=0
        is(i)=0
        idz(i)=0
        amp(i)=zero
        bet0(i)=zero
        alf0(i)=zero
        clo(i)=zero
        clop(i)=zero
        aper(i)=c1e3
        di0(i)=zero
        dip0(i)=zero
        cro(i)=zero
        sigma0(i)=zero
        qwsk(i)=zero
        betx(i)=zero
        betz(i)=zero
        alfx(i)=zero
        alfz(i)=zero
   10 continue
      do 20 i=1,3
        iq(i)=0
        hsy(i)=zero
        qw0(i)=zero
        clo6(i)=zero
        clop6(i)=zero
   20 continue
      do 30 i=1,4
        nwr(i)=0
   30 continue
      do 40 i=1,5
        ipr(i)=0
        nrr(i)=0
        nu(i)=0
        toptit(i)=' '
   40 continue
      do 50 i=1,6
        nskew(i)=0
   50 continue
      do 60 i=1,10
        dtr(i)=zero
        coel(i)=' '
   60 continue
      do 70 i=1,12
        ire(i)=0
   70 continue
      do 80 i=1,nper
        msym(i)=0
   80 continue
      do 90 i=1,6
        do 90 j=1,6
          ta(i,j)=zero
   90 continue
      do 100 i=1,2
        do 100 j=1,6
          exz(i,j)=zero
  100 continue
      do 110 i1=1,9
        do 110 i2=1,18
          do 110 i3=1,10
            do 110 i4=1,5
              rtc(i1,i2,i3,i4)=zero
              rts(i1,i2,i3,i4)=zero
  110 continue
C--NUMBER OF ELEMENTS---------------------------------------------------
      do 150 i=1,nele
        kz(i)=0
        kp(i)=0
        irm(i)=0
        nmu(i)=0
        kpa(i)=0
        isea(i)=0
        nrel(i)=0
        ncororb(i)=0
        iratioe(i)=0
        itionc(i)=0
        dki(i,1)=zero
        dki(i,2)=zero
        dki(i,3)=zero
        ed(i)=zero
        el(i)=zero
        ek(i)=zero
        sm(i)=zero
        xpl(i)=zero
        xrms(i)=zero
        zpl(i)=zero
        zrms(i)=zero
        benkc(i)=zero
        r00(i)=zero
        apx(i)=c1e3
        apz(i)=c1e3
        ramp(i)=zero
        rfre(i)=zero
        rzph(i)=zero
        ratioe(i)=one
        hsyc(i)=zero
        phasc(i)=zero
        bez(i)=' '
        bezl(i)=' '
        do 120 i3=1,2
          sigman6(i3,i)=zero
          do 120 i4=1,6
            a(i,i3,i4)=zero
              do 120 i1=1,npart
                al(i4,i3,i1,i)=zero
                sigman(i3,i1,i)=zero
                sigman2(i3,i1,i)=zero
                sigmanq(i3,i1,i)=zero
                clobeam(i3,i1,i)=zero
                beamoff(i3,i1,i)=zero
  120   continue
        do 130 i1=1,mmul
          bk0(i,i1)=zero
          ak0(i,i1)=zero
          bka(i,i1)=zero
          aka(i,i1)=zero
  130   continue
        do 140 i1=1,3
          bezr(i1,i)=' '
  140   continue
  150 continue
C--NUMBER OF BLOCKS-----------------------------------------------------
      do 180 i=1,nblo
        mel(i)=0
        mstr(i)=0
        elbe(i)=zero
        bezb(i)=' '
        do 160 i1=1,2
          do 160 i2=1,6
            bl1(i,i1,i2)=zero
            bl2(i,i1,i2)=zero
  160   continue
        do 170 j=1,nelb
          mtyp(i,j)=0
  170   continue
  180 continue
C--# OF STRUCTURE ELEMENTS----------------------------------------------
      do 190 i=1,nblz
        ic(i)=0
        mzu(i)=0
        icext(i)=0
        icextal(i)=0
        extalign(i,1)=zero
        extalign(i,2)=zero
        extalign(i,3)=zero
        sigmoff(i)=zero
        tiltc(i)=one
        tilts(i)=zero
        do 190 j=1,40
          exterr(i,j)=zero
  190 continue
C--RANDOM NUMBERS-------------------------------------------------------
      do 200 i=1,nzfz
        zfz(i)=zero
  200 continue
C--# OF TRAJECTORIES----------------------------------------------------
      do 220 i=1,mpa
        rvf(i)=one
        sigm(i)=zero
        dps(i)=zero
        ej(i)=zero
        ejf(i)=zero
        do 210 i1=1,2
          x(i,i1)=zero
          y(i,i1)=zero
  210   continue
  220 continue
C--COMBINATION OF ELEMENTS----------------------------------------------
      do 240 i1=1,20
        icomb0(i1)=0
        do 230 i=1,ncom
          icomb(i,i1)=0
          ratio(i,i1)=zero
  230   continue
  240 continue
C-----------------------------------------------------------------------
      return
      end
