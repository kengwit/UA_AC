C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine corrorb
C  CORRECTION OF CLOSED ORBIT FIRST (MOST EFFECTIV CORRECTOR STRATEGY
C  USING MICADO), THEN
C  SCALING OF DIPOLE-ERRORS FOR RMS-VALUES OF THE CLOSED ORBIT
C-----------------------------------------------------------------------
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
      character*16 bez,bezb,bezr,erbez,bezl
C     character*16 coel
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
C-----------------------------------------------------------------------
      real*8 ar(nmon1,ncor1)
      real*8 b(nmon1),orbr(nmon1),xinc(ncor1)
      real*8 rmsx,ptpx,rmsz,ptpz,rzero,rzero1
      dimension clo0(2),clop0(2)
      dimension qwc1(3),nx(ncor1)
      character*16 bezlo(nele)
      save
      rzero=0.0d0
      rzero1=0.0d0
      do 10 l=1,2
        clo0(l)=zero
        clop0(l)=zero
        di0(l)=zero
   10 dip0(l)=zero
      call clorb(ded)
      if(ierro.gt.0) call error(4)
      do 20 l=1,2
        clo0(l)=clo(l)
   20 clop0(l)=clop(l)
      call clorb(zero)
      if(ierro.gt.0) call error(5)
      do 30 l=1,2
        di0(l)=(clo0(l)-clo(l))/ded
   30 dip0(l)=(clop0(l)-clop(l))/ded
      do 40 l=1,ncor1
        xinc(l)=0.0
   40 nx(l)=0
      if(iclo.eq.0) return
C-- ORBIT CORRECTION
      ihflag=0
      ivflag=0
      icflag=0
 
      write(6,*)
      write(6,10000)
      if(ncorru.eq.0) then
        call error(84)
      else
        if(ncorrep.le.0) then
          write(6,10010) ncorru,sigma0(1),sigma0(2)
        else
          write(6,10020) ncorru,ncorrep
        endif
      endif
      write(6,*)
 
C-- SAVE OLD 'LINOPT' SETTINGS
      iprinto=iprint
      nto=nt
      ntcoo=ntco
      do 50 i=1,nlin
        bezlo(i)=bezl(i)
   50 continue
      nlino=nlin
C-- PUT MONITORS AND CORRECTORS INTO LINOPT SETTINGS
C-- GET TWISS PARAMETERS AND DISTORTED ORBIT BACK
      iprint=0
      ntco=0
      nlin=0
      do 60 i=1,il
        if(kp(i).eq.3.or.kp(i).eq.4.or. kp(i).eq.-3.or.kp(i).eq.-4) bezl
     +  (i)=bez(i)
        nlin=nlin+1
   60 continue
      call linopt(zero)
      call phasad(zero,qwc1)
 
C-- CHECK SOME CONDITIONS
      write(6,10100) nhmoni,nhcorr,nvmoni,nvcorr
      if(nhmoni.gt.nmon1) then
        write(6,10070) nhmoni,nmon1
        return
      endif
      if(nvmoni.gt.nmon1) then
        write(6,10070) nvmoni,nmon1
        return
      endif
      if(nhcorr.gt.ncor1) then
        write(6,10080) nhcorr,ncor1
        return
      endif
      if(nvcorr.gt.ncor1) then
        write(6,10080) nvcorr,ncor1
        return
      endif
      if(nhmoni.lt.nhcorr.or.nvmoni.lt.nvcorr) write(6,10090)
 
      write(6,*)
      call orbinit
C-- CORRECT BOTH PLANES
      if(ncorrep.eq.0) then
        icflag=1
        ncorrep=itco
      endif
      do 110 ii=1,ncorrep
C-- HORIZONTAL PLANE FIRST
        do 70 i=1,nhmoni
          b(i)=bclorb(i,1)
          do 70 j=1,nhcorr
            ar(i,j)=sqrt(betam(i,1)*betac(j,1))*cos(abs(pam(i,1)- pac
     +      (j,1))-qwc1(1)*pi)*c1e3/(2*sin(qwc1(1)*pi))
   70   continue
        call calrms(b,nhmoni,rmsx,ptpx)
C-- MICADO WITH HOUSEHOLDER TRANSFORMATION
        call htls(ar,b,nhmoni,nhcorr,xinc,nx,orbr,ncorru,rzero,rzero1)
 
C-- VERTICAL PLANE HERE
        do 80 i=1,nvmoni
          b(i)=bclorb(i,2)
          do 80 j=1,nvcorr
            ar(i,j)=sqrt(betam(i,2)*betac(j,2))*cos(abs(pam(i,2)- pac
     +      (j,2))-qwc1(2)*pi)*c1e3/(2*sin(qwc1(2)*pi))
   80   continue
        call calrms(b,nvmoni,rmsz,ptpz)
        write(6,10030) ii-1,rmsx,rmsz
        write(6,10040) ii-1,ptpx,ptpz
        if(icflag.eq.1.and.sigma0(1).gt.rmsx.and.ihflag.eq.0) then
          write(6,10110)
          ihflag=1
        endif
        if(icflag.eq.1.and.sigma0(2).gt.rmsz.and.ivflag.eq.0) then
          write(6,10120)
          ivflag=1
        endif
 
        if(ihflag.eq.0) then
          write(6,*)
          do 90 ij=1,ncorru/10
            write(6,10050) (nx(10*(ij-1)+k), k=1,10)
   90     continue
          if(mod(ncorru,10).gt.0) then
            write(6,10050) (nx(10*(ij-1)+k), k=1,mod(ncorru,10))
          endif
          call putorb(xinc,nx,1)
        endif
C-- MICADO WITH HOUSEHOLDER TRANSFORMATION
        call htls(ar,b,nvmoni,nvcorr,xinc,nx,orbr,ncorru,rzero,rzero1)
 
        if(ivflag.eq.0) then
          write(6,*)
          do 100 ij=1,ncorru/10
            write(6,10060) (nx(10*(ij-1)+k), k=1,10)
  100     continue
          if(mod(ncorru,10).gt.0) then
            write(6,10060) (nx(10*(ij-1)+k), k=1,mod(ncorru,10))
          endif
          call putorb(xinc,nx,2)
        endif
 
        if(ihflag.eq.1.and.ivflag.eq.1) goto 140
        call linopt(zero)
        call phasad(zero,qwc1)
  110 continue
 
C-- GET LAST VALUES AFTER CORRECTION
      do 120 i=1,nhmoni
        b(i)=bclorb(i,1)
  120 continue
      call calrms(b,nhmoni,rmsx,ptpx)
      do 130 i=1,nvmoni
        b(i)=bclorb(i,2)
  130 continue
      call calrms(b,nvmoni,rmsz,ptpz)
      write(6,10030) ncorrep,rmsx,rmsz
      write(6,10040) ncorrep,ptpx,ptpz
      write(6,*)
 
  140 continue
      if((ii-1).eq.itco) write(6,10130) itco
 
C-- SCALE TO DESIRED RMS VALUE IF IT IS GREATER THAN ZERO
      if(sigma0(1).gt.pieni.or.sigma0(2).gt.pieni) then
        do 180 ii=1,itco
          write(6,10140)
          hfac=sigma0(1)/rmsx
          vfac=sigma0(2)/rmsz
          do 150 i=1,il
            kzz=kz(i)
            kpz=kp(i)
            if(kzz.eq.1.and.el(i).lt.pieni) then
              ed(i)=ed(i)*hfac
              ek(i)=ek(i)*hfac
            endif
            if(kzz.eq.-1.and.el(i).lt.pieni) then
              ed(i)=ed(i)*vfac
              ek(i)=ek(i)*vfac
            endif
            if(kzz.eq.11) then
              im=irm(i)
              ak0(im,1)=ak0(im,1)*vfac
              aka(im,1)=aka(im,1)*vfac
              bk0(im,1)=bk0(im,1)*hfac
              bka(im,1)=bka(im,1)*hfac
            endif
  150     continue
          call linopt(zero)
          do 160 i=1,nhmoni
            b(i)=bclorb(i,1)
  160     continue
          call calrms(b,nhmoni,rmsx,ptpx)
          do 170 i=1,nvmoni
            b(i)=bclorb(i,2)
  170     continue
          call calrms(b,nvmoni,rmsz,ptpz)
          write(6,10150) ii,rmsx,rmsz
          write(6,10160) ii,ptpx,ptpz
          write(6,*)
          if(abs(rmsx-sigma0(1)).lt.dsi.and. abs(rmsz-sigma0(2)).lt.dsi)
     +    goto 190
  180   continue
      endif
      if((ii-1).eq.itco) write(6,10130) itco
  190 continue
 
C-- WRITE OUT ADJUSTED CLOSED ORBIT
      do 200 i=1,nhmoni
        write(28,*) i,bclorb(i,1)
  200 continue
      do 210 i=1,nhmoni
        write(29,*) i,bclorb(i,2)
  210 continue
 
C-- CHANGE BACK TO OLD 'LINOPT' SETTINGS
      iprint=iprinto
      nt=nto
      ntco=ntcoo
      nlin=nlino
      do 220 i=1,nlin
        bezl(i)=bezlo(i)
  220 continue
      ncorru=0
C-----------------------------------------------------------------------
      return
10000 format(t5,'---- ORBIT CORRECTION WITH MOST EFFCTIVE CORRECTOR ',
     +'STRATEGY ----')
10010 format(t5,'     ORBIT CORRECTION WITH ',i4,' CORRECTORS UNTIL',/,
     +t5,'       HOR. RMS SMALLER THAN ',f6.3,' MM',/, t5,
     +'       VER. RMS SMALLER THAN ',f6.3,' MM')
10020 format(t5,'     ORBIT CORRECTION WITH ',i4,' CORRECTORS AND ',i4,
     +' ITERATIONS.')
10030 format(t5,'---- CORRECTION ITERATION NO. ',i4,' HOR.-RMS: ',f6.3,
     +' VER.-RMS: ',f6.3)
10040 format(t5,'---- CORRECTION ITERATION NO. ',i4,' HOR.-PTP: ',f6.3,
     +' VER.-PTP: ',f6.3)
10050 format(t5,'     HORIZONTAL CORRECTORS USED:', i4,i4,i4,i4,i4,i4,
     +i4,i4,i4,i4)
10060 format(t5,'     VERTICAL   CORRECTORS USED:', i4,i4,i4,i4,i4,i4,
     +i4,i4,i4,i4)
10070 format(/,t5,'ERROR: NUMBER OF MONITORS TOO BIG.',/
     +'    THERE ARE ',i4,' MONITORS SET, BUT ONLY ',i4, ' ALLOWED.',/
     +'    NO CORRECTION DONE.',/)
10080 format(/,t5,'ERROR: NUMBER OF CORRECTORS TOO BIG.',/
     +'    THERE ARE ',i4,' MONITORS SET, BUT ONLY ',i4, ' ALLOWED.',/
     +'    NO CORRECTION DONE.',/)
10090 format(/,t5,'WARNING: NUMBER OF MONITORS IS SMALLER THAN NUMBER',
     +' OF CORRECTORS.',/ '    NUMERICAL PROBLEMS MIGHT BE ENCOUNTERED.'
     +)
10100 format(/,t5,'NUMBER OF HOR. MONITORS: ',i4,
     +'  NUMBER OF HOR. CORRECTORS: ',i4,/, t5,
     +'NUMBER OF VER. MONITORS: ',i4, '  NUMBER OF VER. CORRECTORS: ',
     +i4)
10110 format(t10,'HORIZONTAL RMS GOAL REACHED')
10120 format(t10,'VERTICAL RMS GOAL REACHED')
10130 format(t10,'MAXIMUM NUMBER OF ITERATIONS ACHIVED: ',i4,/ ,t10,
     +'INCREASE ITCO TO INCREASE THE NUMBER OF ' ,
     +'CLOSED ORBIT ITERATIONS',/)
10140 format(t5,'---- ORBIT SCALING USING ALL POSSIBLE ELEMENTS ')
10150 format(t5,'---- SCALING ITERATION NO. ',i4,' HOR.-RMS: ',f6.3,
     +' VER.-RMS: ',f6.3)
10160 format(t5,'---- SCALING ITERATION NO. ',i4,' HOR.-PTP: ',f6.3,
     +' VER.-PTP: ',f6.3)
      end
