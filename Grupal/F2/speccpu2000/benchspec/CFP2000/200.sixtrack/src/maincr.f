      program maincr
C ANFANG - HAUPTPROGRAMM -
C-----------------------------------------------------------------------
C
C  SIXTRACK
C
C  SIXDIMENSIONAL PARTICLE-TRACKING
C
C-----------------------------------------------------------------------
C  VECTOR VERSION 2 OCTOBER   1996
C
C  F. SCHMIDT, M. VANTTINEN
C-----------------------------------------------------------------------
C  USED DISKS:
C
C  GEOMETRY AND STRENGTH OF THE ACCELERATOR : UNIT  2
C  TRACKING PARAMETER                       : UNIT  3
C  NORMAL PRINTOUT                          : UNIT  6
C  TRACKING DATA                            : UNIT  8
C  DATA FOR SUMMARY OF THE POSTPROCESSING   : UNIT 10
C  AUXILIARY FILE FOR THE INPUT             : UNIT 11
C  ASCII FILE WITH THE HORIZONTAL FFT DATA  : UNIT 14
C  ASCII FILE WITH THE VERTICAL FFT DATA    : UNIT 15
C  METAFILE FOR PLOTTING WITH GKS           : UNIT 20
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
      common/postr2/nnumxv(npart)
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
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/tasm/tasm(6,6)
      common/sixdim/aml6(6,6),edcor(2),mapout
      integer rdummy(6)
      character*10 cmonth
      character*4 cpto
C     character*80 day,runtim
      character*8 cdate,ctime,progrm
      logical pstop
      common/main1/
     &ekv(npart,nele),fokqv(npart),
     &aaiv(mmul,nmac,nblz),bbiv(mmul,nmac,nblz),
     &smiv(nmac,nblz),zsiv(nmac,nblz),xsiv(nmac,nblz),
     &xsv(npart),zsv(npart),
     &qw(2),qwc(3),clo0(2),clop0(2),
     &eps(2),epsa(2),ekk(2),cr(mmul),ci(mmul),
     &xv(2,npart),yv(2,npart),dam(npart),ekkv(npart),
     &sigmv(npart),dpsv(npart),dp0v(npart),
     &sigmv6(npart),dpsv6(npart),
     &ejv(npart),ejfv(npart),
     &xlv(npart),zlv(npart),
     &pstop(npart),rvv(npart),ejf0v(npart),
     &numxv(npart),nms(npart),nlostp(npart)
      common/main2/ dpd(npart),dpsq(npart),fok(npart),rho(npart),
     &fok1(npart),si(npart),co(npart),g(npart),gl(npart),
     &sm1(npart),sm2(npart),sm3(npart),sm12(npart),
     &as3(npart),as4(npart),as6(npart),sm23(npart),
     &rhoc(npart),siq(npart),aek(npart),afok(npart),
     &hp(npart),hm(npart),hc(npart),hs(npart),wf(npart),
     &wfa(npart),wfhi(npart),rhoi(npart),
     &hi(npart),fi(npart),hi1(npart),
     &xvl(2,npart),yvl(2,npart),ejvl(npart),
     &dpsvl(npart),oidpsv(npart),
     &sigmvl(npart),iv(npart),
     &aperv(npart,2),ixv(npart),
     &clov(2,npart),clopv(2,npart),alf0v(npart,2),
     &bet0v(npart,2),ampv(npart)
      common/main3/ clo6v(3,npart),clop6v(3,npart),
     &hv(6,2,npart,nblo),bl1v(6,2,npart,nblo),
     &tas(npart,6,6),qwcs(npart,3),
     &di0xs(npart),di0zs(npart),dip0xs(npart),dip0zs(npart),
     &xau(2,6),cloau(6),di0au(4),tau(6,6),tasau(npart,6,6),
     &wx(3),x1(6),x2(6),
     &fake(2,20)
      common/main4/ e0f,numx
      dimension cmonth(12),cor6d(2,2)
	EXTERNAL QMOD
C-----------------------------------------------------------------------
      data (cmonth(i),i=1,12)/' January ',' February ','  March   ',
     &'  April   ','   May    ','   June   ','   July   ',' August  ',
     &' September',' October  ',' November ',' December '/
      write(6,10010)

      open(unit=2, file="fort.2")
      open(unit=3, file="fort.3")
      open(unit=7, file="fort.7")
      open(unit=8, file="fort.8")
      open(unit=16, file="fort.16")

      call xuflow(0)
      do 10 i=1,nblz
        xsi(i)=zero
        zsi(i)=zero
        smi(i)=zero
        rsmi(i)=zero
        rfres(i)=zero
        rzphs(i)=zero
   10 continue
      do 20 i=1,mmul
        cr(i)=zero
        ci(i)=zero
   20 continue
      do 30 i=1,2
        eps(i)=zero
        epsa(i)=zero
        ekk(i)=zero
        qw(i)=zero
        qwc(i)=zero
   30 continue
      do 60 i=1,npart
        nnumxv(i)=0
        xv(1,i)=zero
        xv(2,i)=zero
        yv(1,i)=zero
        yv(2,i)=zero
        dam(i)=zero
        ekkv(i)=zero
        sigmv(i)=zero
        dpsv(i)=zero
        dp0v(i)=zero
        ejv(i)=zero
        ejfv(i)=zero
        xlv(i)=zero
        zlv(i)=zero
        rvv(i)=one
        ejf0v(i)=zero
        dpd(i)=zero
        dpsq(i)=zero
        fok(i)=zero
        rho(i)=zero
        fok1(i)=zero
        si(i)=zero
        co(i)=zero
        g(i)=zero
        gl(i)=zero
        sm1(i)=zero
        sm2(i)=zero
        sm3(i)=zero
        sm12(i)=zero
        as3(i)=zero
        as4(i)=zero
        as6(i)=zero
        sm23(i)=zero
        rhoc(i)=zero
        siq(i)=zero
        aek(i)=zero
        afok(i)=zero
        hp(i)=zero
        hm(i)=zero
        hc(i)=zero
        hs(i)=zero
        wf(i)=zero
        wfa(i)=zero
        wfhi(i)=zero
        rhoi(i)=zero
        hi(i)=zero
        fi(i)=zero
        hi1(i)=zero
        xvl(1,i)=zero
        xvl(2,i)=zero
        yvl(1,i)=zero
        yvl(2,i)=zero
        ejvl(i)=zero
        dpsvl(i)=zero
        oidpsv(i)=one
        sigmvl(i)=zero
        iv(i)=0
        aperv(i,1)=zero
        aperv(i,2)=zero
        ixv(i)=0
        clov(1,i)=zero
        clov(2,i)=zero
        clo6v(1,i)=zero
        clo6v(2,i)=zero
        clo6v(3,i)=zero
        clopv(1,i)=zero
        clopv(2,i)=zero
        clop6v(1,i)=zero
        clop6v(2,i)=zero
        clop6v(3,i)=zero
        alf0v(i,1)=zero
        alf0v(i,2)=zero
        bet0v(i,1)=zero
        bet0v(i,2)=zero
        ampv(i)=zero
        do 50 i1=1,6
          do 50 i2=1,6
            tas(i,i1,i2)=zero
   50   continue
        qwcs(i,1)=zero
        qwcs(i,2)=zero
        qwcs(i,3)=zero
        di0xs(i)=zero
        di0zs(i)=zero
        dip0xs(i)=zero
        dip0zs(i)=zero
   60 continue
      qwc(3)=zero
      call comnul
      commen=' '
      progrm='SIXTRACK'
      pi=four*atan(one)
      pi2=pi*half
      pisqrt=sqrt(pi)
      rad=pi/180
      call daten
      if(ithick.eq.1) write(6,10030)
      if(ithick.eq.0) write(6,10040)
      if(ibidu.eq.2) then
        write(6,10025)
        goto 550
      endif
C--SETTING UP THE PLOTTING
      if(ipos.eq.1.and.
     &(idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0)) then
        call hlimit(nplo)
        call hplint(kwtype)
        if(itf.eq.3) then
          call igmeta(-20,-111)
        else
          call hplcap(itf)
        endif
        cpto='NPTO'
        if(icr.eq.1) cpto='PTO '
        call hplopt(cpto,1)
        call hplopt('DATE',1)
        call hplset('DATE',1d0)
        call hplset('CSIZ',.15d0)
      endif
      if(ipos.eq.1.and.napx.eq.0) then
        do 70 i=1,ndafi
          call postpr(91-i)
   70   continue
        call sumpos
        goto 520
      endif
      close(2)
      close(3)
      do 80 i=1,npart
        pstop(i)=.false.
        nnumxv(i)=numl
   80 numxv(i)=numl
      do 90 i=1,20
        fake(1,i)=zero
   90 fake(2,i)=zero
      itra=2
      amp00=amp(1)
      if(napx.ne.1) damp=(amp00-amp0)/(napx-1)/2
      napx=2*napx
      iation=abs(ition)
      ib0=0
      dp00=dp1
      if(napx.le.0.or.imc.le.0) goto 490
      do 260 m=1,mmac
C--MULTIPOLE WITH THEIR RANDOM VALUES ADDED
        if(m.ge.2) then
          call recuin(m*izu0,irecuin)
          call ranecu(zfz,nzfz,mcut)
          rsum=zero
          do 100 i=1,nzfz
  100     rsum=rsum+zfz(i)
          rmean=rsum/nzfz
          rsqsum=zero
          do 110 i=1,nzfz
  110     rsqsum=rsqsum+(zfz(i)-rmean)*(zfz(i)-rmean)
          rdev=sqrt(rsqsum/nzfz)
          write(6,10320) m*izu0,nzfz,rmean,rdev
          write(6,10070)
        endif
        if(m.eq.1) call ord
        call clorb(ded)
        do 120 l=1,2
          clo0(l)=clo(l)
  120   clop0(l)=clop(l)
        call clorb(zero)
        do 130 l=1,2
          ll=2*l
          di0(l)=(clo0(l)-clo(l))/ded
  130   dip0(l)=(clop0(l)-clop(l))/ded
        call corrorb
        if(irmod2.eq.1) call rmod(dp1)
        if(iqmod.ne.0) call qmod
        if(ichrom.ne.0) call chroma
        if(iskew.ne.0) call decoup
c--beam-beam element
        if(nbeam.ge.1) then
          do 135 i=1,nele
            if(kz(i).eq.20) then
              nlin=nlin+1
              if(nlin.gt.nele) call error(81)
              bezl(nlin)=bez(i)
            endif
  135     continue
        endif
        if(ilin.ne.0) call linopt(dp1)
        if(isub.eq.1) call subre(dp1)
        if(ise.eq.1) call search(dp1)
        izu=0
        do 150 i=1,iu
          ix=ic(i)
          if(ix.le.nblo) goto 150
          ix=ix-nblo
          kpz=kp(ix)
          kzz=kz(ix)
          if(kpz.eq.6.or.kzz.eq.0) goto 150
          if(iorg.lt.0) mzu(i)=izu
          izu=mzu(i)+1
          smiv(m,i)=sm(ix)+zfz(izu)*ek(ix)
          smi(i)=smiv(m,i)
          izu=izu+1
          xsiv(m,i)=xpl(ix)+zfz(izu)*xrms(ix)
          xsi(i)=xsiv(m,i)
          izu=izu+1
          zsiv(m,i)=zpl(ix)+zfz(izu)*zrms(ix)
          zsi(i)=zsiv(m,i)
          if(mout2.eq.1) then
            if(kzz.eq.11) zfz(izu-2)=zero
            if(abs(ek(ix)).le.pieni) zfz(izu-2)=zero
            if(abs(xrms(ix)).le.pieni) zfz(izu-1)=zero
            if(abs(zrms(ix)).le.pieni) zfz(izu)=zero
            write(31,'(a16,1p,d19.11,2d14.6,d17.9)') bez(ix),
     +      zfz(izu-2),zfz(izu-1),zfz(izu),extalign(i,3)
          endif
          if(kzz.eq.11) then
            r0=ek(ix)
            if(abs(r0).le.pieni) goto 150
            nmz=nmu(ix)
            if(nmz.eq.0) then
              izu=izu+2*mmul
              goto 150
            endif
            im=irm(ix)
            r0a=one
            do 140 k=1,nmz
              izu=izu+1
              aaiv(k,m,i)=ed(ix)*(ak0(im,k)+zfz(izu)*aka(im,k))/r0a
              aai(i,k)=aaiv(k,m,i)
              izu=izu+1
              bbiv(k,m,i)=ed(ix)*(bk0(im,k)+zfz(izu)*bka(im,k))/r0a
              bbi(i,k)=bbiv(k,m,i)
  140       r0a=r0a*r0
            izu=izu+2*mmul-2*nmz
          endif
  150   continue
        dp0=dp00
        if(imc.gt.1) then
          ddp1=two*dp0/(imc-one)
        endif
        do 250 ib=1,imc
          if(imc.gt.1) then
            dp1=dp0-(ib-1)*ddp1
          endif
          dp10=dp1
C-----------------------------------------------------------------------
          if(idp.ne.1.or.iation.ne.1) iclo6=0
          if (iclo6.eq.1.or.iclo6.eq.2) then
            if(ib.eq.1) then
              if(iclo6r.eq.0) then
                clo6(1)=zero
                clo6(2)=zero
                clo6(3)=zero
                clop6(1)=zero
                clop6(2)=zero
                clop6(3)=zero
              else
                read(33,*) (clo6(l),clop6(l), l=1,3)
              endif
              x1old1 = x(1,1)
              x2old1 = x(1,2)
              y1old1 = y(1,1)
              y2old1 = y(1,2)
              sigmold1 = sigm(1)
              dpold1 = dps(1)
              x1old2 = x(2,1)
              x2old2 = x(2,2)
              y1old2 = y(2,1)
              y2old2 = y(2,2)
              sigmold2 = sigm(2)
              dpold2 = dps(2)
              noold = nord
              nvold = nvar
              nv2old = nvar2
              itraold = itra
              itu6d=0
              iq1=iq(1)
              iq2=iq(2)
              if(iqmod6.eq.1) then
                edcor(1)=ed(iq1)
                edcor(2)=ed(iq2)
              endif
              ncorruo=ncorru
              ncorru=1
              nsixo = nsix
  175         continue
              nord = 1
              nvar = 6
              nvar2 = 6
              nsix = 0
              if(nbeam.ge.1) then
                nvbeam=-1
                call linopt(clop6(3))
              endif
              isixda=1
              call sixdaco6(isixda)
              if(iqmod6.eq.1) then
                do 176 l=1,2
                  x(1,l)=clo6(l)
                  y(1,l)=clop6(l)
  176           continue
                sigm(1)=clo6(3)
                dps(1)=clop6(3)
                nord=2
                nvar=8
                mapout=1
                idalloc=3
                isixda=2
                call sixdaco6(isixda)
              endif
              write(6,10130) m
              write(6,10140) clo6(1),clop6(1)
              write(6,10140) clo6(2),clop6(2)
              write(6,10140) clo6(3),clop6(3)
              x(1,1) = x1old1
              x(1,2) = x2old1
              y(1,1) = y1old1
              y(1,2) = y2old1
              sigm(1) = sigmold1
              dps(1) = dpold1
              x(2,1) = x1old2
              x(2,2) = x2old2
              y(2,1) = y1old2
              y(2,2) = y2old2
              sigm(2) = sigmold2
              dps(2) = dpold2
              dp1=dp10+clop6(3)
              call clorb(clop6(3))
              call betalf(clop6(3),qw)
              call phasad(clop6(3),qwc)
              intwx1=int(qwc(1))
              wx(1)=qwc(1)-intwx1
              intwx2=int(qwc(2))
              wx(2)=qwc(2)-intwx2
              wx(3)=zero
              if(iqmod6.ne.1) then
                call dalie6s(iqmod6,1,wx,cor6d)
              else
                call dalie6s(iqmod6,3,wx,cor6d)
              endif
              rewind 18
              qwc(1)=intwx1+wx(1)
              qwc(2)=intwx2+wx(2)
              qwc(3)=wx(3)
              if(iqmod6.eq.1) then
                itu6d=itu6d+1
                if(itu6d.gt.1) then
                  dq6t=sqrt(dq1*dq1+dq2*dq2)
                  write(6,10340) qw0(1),qwc(1),qw0(2),qwc(2),itu6d-1,
     +            dq6t
                  write(6,10350) edcor(1),ed(iq1),bez(iq1),edcor(2),
     +            ed(iq2),bez(iq2)
                endif
                dq1=qwc(1)-qw0(1)
                dq2=qwc(2)-qw0(2)
                if(itu6d.le.itqv.and.(abs(dq1).gt.dqq.or.
     +          abs(dq2).gt.dqq)) then
                  if(el(iq1).le.pieni) then
                    ed(iq1)=ed(iq1)-cor6d(1,1)*dq1-cor6d(1,2)*dq2
                  else
                    ek(iq1)=ek(iq1)-cor6d(1,1)*dq1-cor6d(1,2)*dq2
                  endif
                  if(el(iq2).le.pieni) then
                    ed(iq2)=ed(iq2)-cor6d(2,1)*dq1-cor6d(2,2)*dq2
                  else
                    ek(iq2)=ek(iq2)-cor6d(2,1)*dq1-cor6d(2,2)*dq2
                  endif
                  goto 175
                else
                  do 177 i6t=1,iu
                    ix=ic(i6t)-nblo
                    if(ix.eq.iq1.or.iratioe(ix).eq.iq1) then
                      smi(i6t)=smi(i6t)+(ed(iq1)-edcor(1))*ratioe(ix)
                      smiv(m,i6t)=smi(i6t)
                    else if(ix.eq.iq2.or.iratioe(ix).eq.iq2) then
                      smi(i6t)=smi(i6t)+(ed(iq2)-edcor(2))*ratioe(ix)
                      smiv(m,i6t)=smi(i6t)
                    endif
  177             continue
                endif
              endif
              nsix = nsixo
              nord = noold
              nvar = nvold
              nvar2 = nv2old
              itra = itraold
            endif
            idalloc=1
            ncorru=ncorruo
            do 190 ib1=1,napx
              ib3=ib1+(m+ib-2)*napx
c--beam-beam element
              if(nbeam.ge.1) then
                do 195 ib11=1,nele
                  if(kz(ib11).eq.20) then
                    clobeam(1,ib3,ib11)=clobaux(1,ib11)
                    clobeam(2,ib3,ib11)=clobaux(2,ib11)
                  endif
  195           continue
              endif
              clo6v(1,ib3)=clo6(1)
              clo6v(2,ib3)=clo6(2)
              clo6v(3,ib3)=clo6(3)
              clop6v(1,ib3)=clop6(1)
              clop6v(2,ib3)=clop6(2)
              clop6v(3,ib3)=clop6(3)
              di0xs(ib3)=di0(1)
              di0zs(ib3)=di0(2)
              dip0xs(ib3)=dip0(1)
              dip0zs(ib3)=dip0(2)
              qwcs(ib3,1)=qwc(1)
              qwcs(ib3,2)=qwc(2)
              qwcs(ib3,3)=qwc(3)
              do 180 i2=1,6
                do 180 j2=1,6
                  tas(ib3,i2,j2)=tasm(i2,j2)
  180         continue
  190       continue
          else
            if(idp.eq.1.and.iation.eq.1) then
              call clorb(zero)
              call betalf(zero,qw)
              call phasad(zero,qwc)
            else
              call clorb(dp1)
              call betalf(dp1,qw)
              call phasad(dp1,qwc)
            endif
            do 170 i=1,napx
              iar=(m+ib-2)*napx+i
              clo6v(1,iar)=clo(1)
              clop6v(1,iar)=clop(1)
              clo6v(2,iar)=clo(2)
              clop6v(2,iar)=clop(2)
              di0xs(iar)=di0(1)
              di0zs(iar)=di0(2)
              dip0xs(iar)=dip0(1)
              dip0zs(iar)=dip0(2)
              qwcs(iar,1)=qwc(1)
              qwcs(iar,2)=qwc(2)
              qwcs(iar,3)=zero
              do 160 i2=1,4
                do 160 j2=1,4
  160         tas(iar,i2,j2)=ta(i2,j2)
  170       continue
          endif
          iar=(m+ib-2)*napx+1
          bet0(1)=tas(iar,1,1)*tas(iar,1,1)+tas(iar,1,2)*tas(iar,1,2)
          bet0x2 =tas(iar,1,3)*tas(iar,1,3)+tas(iar,1,4)*tas(iar,1,4)
          bet0x3 =tas(iar,1,5)*tas(iar,1,5)+tas(iar,1,6)*tas(iar,1,6)
          gam0x1 =tas(iar,2,1)*tas(iar,2,1)+tas(iar,2,2)*tas(iar,2,2)
          gam0x2 =tas(iar,2,3)*tas(iar,2,3)+tas(iar,2,4)*tas(iar,2,4)
          gam0x3 =tas(iar,2,5)*tas(iar,2,5)+tas(iar,2,6)*tas(iar,2,6)
          alf0(1)=-(tas(iar,1,1)*tas(iar,2,1)+tas(iar,1,2)*tas(iar,2,2))
          alf0x2 =-(tas(iar,1,3)*tas(iar,2,3)+tas(iar,1,4)*tas(iar,2,4))
          alf0x3 =-(tas(iar,1,5)*tas(iar,2,5)+tas(iar,1,6)*tas(iar,2,6))
          bet0(2)=tas(iar,3,3)*tas(iar,3,3)+tas(iar,3,4)*tas(iar,3,4)
          bet0z2 =tas(iar,3,1)*tas(iar,3,1)+tas(iar,3,2)*tas(iar,3,2)
          bet0z3 =tas(iar,3,5)*tas(iar,3,5)+tas(iar,3,6)*tas(iar,3,6)
          gam0z1 =tas(iar,4,3)*tas(iar,4,3)+tas(iar,4,4)*tas(iar,4,4)
          gam0z2 =tas(iar,4,1)*tas(iar,4,1)+tas(iar,4,2)*tas(iar,4,2)
          gam0z3 =tas(iar,4,5)*tas(iar,4,5)+tas(iar,4,6)*tas(iar,4,6)
          alf0(2)=-(tas(iar,3,3)*tas(iar,4,3)+tas(iar,3,4)*tas(iar,4,4))
          alf0z2 =-(tas(iar,3,1)*tas(iar,4,1)+tas(iar,3,2)*tas(iar,4,2))
          alf0z3 =-(tas(iar,3,5)*tas(iar,4,5)+tas(iar,3,6)*tas(iar,4,6))
          bet0s1 =tas(iar,5,5)*tas(iar,5,5)+tas(iar,5,6)*tas(iar,5,6)
          bet0s2 =tas(iar,5,1)*tas(iar,5,1)+tas(iar,5,2)*tas(iar,5,2)
          bet0s3 =tas(iar,5,3)*tas(iar,5,3)+tas(iar,5,4)*tas(iar,5,4)
          gam0s1 =tas(iar,6,5)*tas(iar,6,5)+tas(iar,6,6)*tas(iar,6,6)
          gam0s2 =tas(iar,6,1)*tas(iar,6,1)+tas(iar,6,2)*tas(iar,6,2)
          gam0s3 =tas(iar,6,3)*tas(iar,6,3)+tas(iar,6,4)*tas(iar,6,4)
          alf0s1 =-(tas(iar,5,5)*tas(iar,6,5)+tas(iar,5,6)*tas(iar,6,6))
          alf0s2 =-(tas(iar,5,1)*tas(iar,6,1)+tas(iar,5,2)*tas(iar,6,2))
          alf0s3 =-(tas(iar,5,3)*tas(iar,6,3)+tas(iar,5,4)*tas(iar,6,4))
          do 220 ib1=1,napx
            iar=ib1+(m+ib-2)*napx
            do 200 ib2=1,6
              do 200 ib3=1,6
  200       tau(ib2,ib3)=tas(iar,ib3,ib2)
            if(abs(tau(1,1)).le.pieni.and.abs(tau(2,2)).le.pieni) then
              tau(1,1)=one
              tau(2,2)=one
            endif
            if(abs(tau(3,3)).le.pieni.and.abs(tau(4,4)).le.pieni) then
              tau(3,3)=one
              tau(4,4)=one
            endif
            if(abs(tau(5,5)).le.pieni.and.abs(tau(6,6)).le.pieni) then
              tau(5,5)=one
              tau(6,6)=one
              call dinv(6,tau,6,rdummy,nerror)
              its6d=0
              if(ntwin.ne.2) then
                taus=abs(tau(5,1))+abs(tau(5,2))+abs(tau(5,3))+abs
     +          (tau(5,4)) +abs(tau(5,5))+abs(tau(5,6))+abs(tau(6,1))
     +          +abs(tau(6,2)) +abs(tau(6,3))+abs(tau(6,4))+abs
     +          (tau(6,5))+abs(tau(6,6)) +abs(tau(1,5))+abs(tau(2,5))
     +          +abs(tau(3,5))+abs(tau(4,5)) +abs(tau(1,6))+abs
     +          (tau(2,6))+abs(tau(3,6))+abs(tau(4,6))-two
                if(abs(taus).ge.pieni) its6d=1
              endif
              do 210 ib2=1,6
                do 210 ib3=1,6
                  tasau(iar,ib2,ib3)=tau(ib2,ib3)
  210         continue
            endif
  220     continue
          if(ierro.ne.0) then
            write(6,10230) dp1
            goto 520
          endif
          write(6,10070)
          phag=phas*180/pi
          if((idp.eq.0).or.(abs(phas).le.pieni.and.ition.eq.0))
     +    write(6,10170)
     +    qwc(1),clo(1),clop(1),
     +    bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,
     +    qwc(2),clo(2),clop(2),     +
     +    bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2
          if(idp.eq.1.and.iation.eq.1.and.abs(phas).gt.pieni) then
            if(iclo6.eq.0) then
              write(6,10150) phag,
     +        qwc(1),clo(1),clop(1),
     +        bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,
     +        qwc(2),clo(2),clop(2),
     +        bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2
            else
              write(6,10160) phag,
     +        qwc(1),clo6(1),clop6(1),
     +        bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,
     +        bet0x3,alf0x3,gam0x3,
     +        qwc(2),clo6(2),clop6(2),
     +        bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2,
     +        bet0z3,alf0z3,gam0z3,
     +        qwc(3),clo6(3),clop6(3),
     +        bet0s1,alf0s1,gam0s1,bet0s2,alf0s2,gam0s2,
     +        bet0s3,alf0s3,gam0s3
            endif
          endif
          if(idp.eq.1.and.ition.eq.0.and.abs(phas).gt.pieni)
     +    write(6,10190) phag,
     +    qwc(1),clo(1),clop(1),
     +    bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,
     +    qwc(2),clo(2),clop(2),
     +    bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2
          if(idp.eq.1.and.abs(phas).le.pieni.and.iation.eq.1) then
            if(iclo6.eq.0) then
              write(6,10210)
     +        qwc(1),clo(1),clop(1),
     +        bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,
     +        qwc(2),clo(2),clop(2),
     +        bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2
            else
              write(6,10220)
     +        qwc(1),clo6(1),clop6(1),
     +        bet0(1),alf0(1),gam0x1,bet0x2,alf0x2,gam0x2,
     +        bet0x3,alf0x3,gam0x3,
     +        qwc(2),clo6(2),clop6(2),
     +        bet0(2),alf0(2),gam0z1,bet0z2,alf0z2,gam0z2,
     +        bet0z3,alf0z3,gam0z3,
     +        qwc(3),clo6(3),clop6(3),
     +        bet0s1,alf0s1,gam0s1,bet0s2,alf0s2,gam0s2,
     +        bet0s3,alf0s3,gam0s3
            endif
          endif
          write(6,10080) dp1
          e0f=sqrt(e0*e0-pma*pma)
          if(iclo6.eq.0) then
            write(6,10110) clo(1),clop(1),clo(2),clop(2),idz(1),idz(2),
     +      iver, idfor,iclo6,ition
          else
            write(6,10120) clo6(1),clop6(1),clo6(2),clop6(2),clo6(3),
     +      clop6(3), idz(1),idz(2),iver,idfor,iclo6,ition
          endif
          do 240 ib1=1,napx
            ib2=ib0+ib1
            clov(1,ib2)=clo(1)
            clov(2,ib2)=clo(2)
            clopv(1,ib2)=clop(1)
            clopv(2,ib2)=clop(2)
            bet0v(ib2,1)=bet0(1)
            bet0v(ib2,2)=bet0(2)
            alf0v(ib2,1)=alf0(1)
            alf0v(ib2,2)=alf0(2)
            ampv(ib2)=amp(1)-damp*(ib1-1)
            dp0v(ib2)=dp10
            dpsv(ib2)=dp10
            oidpsv(ib2)=one/(one+dp1)
            nms(ib2)=m
            do 230 i=1,nele
              ekv(ib2,i)=ek(i)
  230       continue
  240     continue
c--beam-beam element
          do 245 ib1=1,napx
            ib2=ib0+ib1
            if(nbeam.ge.1) then
              nvbeam=ib2
              ncorruo=ncorru
              ncorru=1
              if(iclo6.eq.0) then
                call linopt(zero)
              else
                call linopt(clop6(3))
              endif
              ncorru=ncorruo
            endif
  245     continue
          ib0=ib0+napx
  250   continue
  260 continue
      if(irip.eq.1) then
        do 280 i=1,iu
          ix=ic(i)
          if(ix.le.nblo) goto 280
          ix=ix-nblo
          do 270 j=1,irco
            jj=nrel(j)
            if(ix.eq.jj) then
              rsmi(i)=ramp(jj)
              rfres(i)=rfre(jj)
              rzphs(i)=rzph(jj)
            endif
  270     continue
  280   continue
      endif
      napx=napx*imc*mmac
      napxo=napx
      if(ibidu.eq.1) then
      write(32)
c
c  left out to do tracking
c  numl,amp0,amp(2),damp,chi0,chid,rat,exz(2,6)
c
c
     &ierro,erbez,pi,pi2,pisqrt,rad,il,mper,mblo,mbloz,msym,kanf,iu,ic,
     &ed,el,ek,sm,kz,kp,xpl,xrms,zpl,zrms,mel,mtyp,mstr,a,bl1,bl2,rvf,
     &idfor,napx,napxo,numlr,nde,nwr,ird,imc,irew,ntwin,iclo6,iclo6r,
     &iver,ibidu,qs,e0,pma,ej,ejf,phas0,phas,hsy,crad,
     &hsyc,phasc,dppoff,sigmoff,tlen,
     &iicav,itionc,ition,idp,ncy,ixcav,dpscor,
     &sigcor,icode,idam,its6d,bk0,ak0,bka,aka,benki,benkc,r00,irm,nmu,
     &zfz,iorg,mzu,bezr,izu0,mmac,mcut,exterr,extalign,tiltc,tilts,
     &mout2,icext,icextal,aper,di0,dip0,ta,dma,dmap,dkq,dqq,de0,ded,dsi,
     &dech,dsm0,itco,itcro,itqv,iout,qw0,iq,iqmod,kpa,iqmod6,bez,elbe,
     &bezb,ilin,nt,iprint,ntco,eui,euii,nlin,bezl,betam,pam,betac,pac,
     &bclorb,nhmoni,nhcorr,nvmoni,nvcorr,ncororb,apx,apz,sigma0,iclo,
     &ncorru,ncorrep,icomb0,icomb,ratio,ratioe,iratioe
     &icoe,ise,mesa,mp,m21,m22,m23,
     &ise1,ise2,ise3,isea,qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl,rtc,
     &rts,ire,ipr,irmod2,dtr,nre,nur,nch,nqc,npp,nrr,nu,dphix,dphiz,qx0,
     &qz0,dres,dfft,cma1,cma2,nstart,nstop,iskip,iconv,imad,ipos,iav,
     &iwg,ivox,ivoz,ires,ifh,toptit,kwtype,itf,icr,idis,icow,istw,iffw
      write(32)
     &nprint,ndafi,irip,irco,ramp,rfre,rzph,nrel,nrturn,qwsk,betx,betz,
     &alfx,alfz,iskew,nskew,sixtit,commen,ithick,clo6,clop6,dki,
     &clobaux,sigman,sigman6,sigman2,sigmanq,clobeam,beamoff,partnum,
     &emitnx,emitnz,gammar,nbeam,nvbeam,ibeco,ibtyp,
     &as,al,sigm,dps,idz,dp1,itra,
     &x,y,bet0,alf0,clo,clop,cro,is,ichrom,xsi,zsi,smi,aai,
     &bbi,rsmi,rfres,rzphs,ampt,tasm,preda,idial,nord,nvar,
     &nvar2,nsix,ncor,ipar,idalloc,nordf,nvarf,inorm,imod1,imod2,
     &icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,coel,
     &ekv,fokqv,aaiv,bbiv,smiv,zsiv,xsiv,xsv,zsv,qw,qwc,clo0,
     &clop0,eps,epsa,ekk,cr,ci,xv,yv,dam,ekkv,sigmv,dpsv,dp0v,sigmv6,
     &dpsv6,ejv,ejfv,xlv,zlv,pstop,rvv,ejf0v,nms,nlostp,dpd,dpsq,
     &fok,rho,fok1,si,co,g,gl,sm1,sm2,sm3,sm12,as3,as4,as6,sm23,rhoc,
     &siq,aek,afok,hp,hm,hc,hs,wf,wfa,wfhi,rhoi,hi,fi,hi1,xvl,yvl,ejvl,
     &dpsvl,oidpsv,sigmvl,iv,aperv,ixv,clov,clopv,alf0v,bet0v,ampv,
     &clo6v,clop6v,hv,bl1v,tas,qwcs,di0xs,di0zs,dip0xs,dip0zs,xau,cloau,
     &di0au,tau,tasau,wx,x1,x2,fake,e0f,numx
      endif
  550 continue
      if(ibidu.eq.2) then
      read(32)
c
c  left out to do tracking
c  numl,amp0,amp(2),damp,chi0,chid,rat,exz(2,6)
c
c
     &ierro,erbez,pi,pi2,pisqrt,rad,il,mper,mblo,mbloz,msym,kanf,iu,ic,
     &ed,el,ek,sm,kz,kp,xpl,xrms,zpl,zrms,mel,mtyp,mstr,a,bl1,bl2,rvf,
     &idfor,napx,napxo,numlr,nde,nwr,ird,imc,irew,ntwin,iclo6,iclo6r,
     &iver,ibidu,qs,e0,pma,ej,ejf,phas0,phas,hsy,crad,
     &hsyc,phasc,dppoff,sigmoff,tlen,
     &iicav,itionc,ition,idp,ncy,ixcav,dpscor,
     &sigcor,icode,idam,its6d,bk0,ak0,bka,aka,benki,benkc,r00,irm,nmu,
     &zfz,iorg,mzu,bezr,izu0,mmac,mcut,exterr,extalign,tiltc,tilts,
     &mout2,icext,icextal,aper,di0,dip0,ta,dma,dmap,dkq,dqq,de0,ded,dsi,
     &dech,dsm0,itco,itcro,itqv,iout,qw0,iq,iqmod,kpa,iqmod6,bez,elbe,
     &bezb,ilin,nt,iprint,ntco,eui,euii,nlin,bezl,betam,pam,betac,pac,
     &bclorb,nhmoni,nhcorr,nvmoni,nvcorr,ncororb,apx,apz,sigma0,iclo,
     &ncorru,ncorrep,icomb0,icomb,ratio,ratioe,iratioe
     &icoe,ise,mesa,mp,m21,m22,m23,
     &ise1,ise2,ise3,isea,qxt,qzt,tam1,tam2,isub,nta,nte,ipt,totl,rtc,
     &rts,ire,ipr,irmod2,dtr,nre,nur,nch,nqc,npp,nrr,nu,dphix,dphiz,qx0,
     &qz0,dres,dfft,cma1,cma2,nstart,nstop,iskip,iconv,imad,ipos,iav,
     &iwg,ivox,ivoz,ires,ifh,toptit,kwtype,itf,icr,idis,icow,istw,iffw
      read(32)
     &nprint,ndafi,irip,irco,ramp,rfre,rzph,nrel,nrturn,qwsk,betx,betz,
     &alfx,alfz,iskew,nskew,sixtit,commen,ithick,clo6,clop6,dki,
     &clobaux,sigman,sigman6,sigman2,sigmanq,clobeam,beamoff,partnum,
     &emitnx,emitnz,gammar,nbeam,nvbeam,ibeco,ibtyp,
     &as,al,sigm,dps,idz,dp1,itra,
     &x,y,bet0,alf0,clo,clop,cro,is,ichrom,xsi,zsi,smi,aai,
     &bbi,rsmi,rfres,rzphs,ampt,tasm,preda,idial,nord,nvar,
     &nvar2,nsix,ncor,ipar,idalloc,nordf,nvarf,inorm,imod1,imod2,
     &icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax,coel,
     &ekv,fokqv,aaiv,bbiv,smiv,zsiv,xsiv,xsv,zsv,qw,qwc,clo0,
     &clop0,eps,epsa,ekk,cr,ci,xv,yv,dam,ekkv,sigmv,dpsv,dp0v,sigmv6,
     &dpsv6,ejv,ejfv,xlv,zlv,pstop,rvv,ejf0v,nms,nlostp,dpd,dpsq,
     &fok,rho,fok1,si,co,g,gl,sm1,sm2,sm3,sm12,as3,as4,as6,sm23,rhoc,
     &siq,aek,afok,hp,hm,hc,hs,wf,wfa,wfhi,rhoi,hi,fi,hi1,xvl,yvl,ejvl,
     &dpsvl,oidpsv,sigmvl,iv,aperv,ixv,clov,clopv,alf0v,bet0v,ampv,
     &clo6v,clop6v,hv,bl1v,tas,qwcs,di0xs,di0zs,dip0xs,dip0zs,xau,cloau,
     &di0au,tau,tasau,wx,x1,x2,fake,e0f,numx
C--SETTING UP THE PLOTTING
        damp=(amp(1)-amp0)/(napx/2-1)/2
        do 551 i=1,npart
          pstop(i)=.false.
          nnumxv(i)=numl
  551   numxv(i)=numl
        if(ipos.eq.1.and.
     &  (idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0)) then
          call hlimit(nplo)
          call hplint(kwtype)
          if(itf.eq.3) then
            call igmeta(-20,-111)
          else
            call hplcap(itf)
          endif
          cpto='NPTO'
          if(icr.eq.1) cpto='PTO '
          call hplopt(cpto,1)
          call hplopt('DATE',1)
          call hplset('DATE',1d0)
          call hplset('CSIZ',.15d0)
        endif
      endif
      rat0=rat
      do 340 ia=1,napx,2
        if(idfor.ne.2) then
C---------------------------------------  SUBROUTINE 'ANFB' IN-LINE
          write(6,10050)
          bet0x2=tas(ia,1,3)*tas(ia,1,3)+tas(ia,1,4)*tas(ia,1,4)
          bet0z2=tas(ia,3,1)*tas(ia,3,1)+tas(ia,3,2)*tas(ia,3,2)
          bet0s1=tas(ia,5,5)*tas(ia,5,5)+tas(ia,5,6)*tas(ia,5,6)
          dsign=one
          rat=rat0
          if(tas(ia,3,3).lt.-pieni) rat=-rat
          if(rat.lt.-pieni) dsign=-one
          x11=ampv(ia)/(sqrt(bet0v(ia,1))+sqrt(abs(rat)*bet0x2))
          x13=x11*dsign*sqrt(abs(rat))
          amp(2)=dsign*(1-iver)* (abs(x11)*sqrt(bet0z2)+abs(x13)*sqrt
     +    (bet0v(ia,2)))
          x1(5)=zero
          x1(6)=dpsv(ia)*sqrt(bet0s1)
          chi=chi0*rad
          dchi=chid*rad
          do 320 i2=1,2
            i3=ia+i2-1
            sic=sin(chi)
            coc=cos(chi)
            x1(1)=x11*coc
            x1(2)=x11*sic
            x1(3)=x13*coc
            x1(4)=x13*sic
            do 300 ii=1,6
              x2(ii)=zero
              do 290 jj=1,6
                x2(ii)=x2(ii)+tas(ia,ii,jj)*x1(jj)
  290         continue
  300       continue
            if(iclo6.eq.1.or.iclo6.eq.2) then
              x2(2)=x2(2)/(one+x2(6)+clop6v(3,ia))      
              x2(4)=x2(4)/(one+x2(6)+clop6v(3,ia))      
            endif       
            if(abs(bet0s1).le.pieni) x2(6)=dpsv(ia)
            if(iver.eq.1) then
              x2(3)=zero
              x2(4)=zero
            endif
            do 310 l=1,2
              ll=(l-1)*2
              xv(l,i3)=x2(1+ll)+exz(i2,1+ll)
              yv(l,i3)=x2(2+ll)+exz(i2,2+ll)
  310       continue
            sigmv(i3)=x2(5)+exz(i2,5)
            dpsv(i3)=x2(6)
            dpsic=dpsv(i3)+clop6v(3,ia)
            if(idp.eq.1.and.abs(ition).eq.1.and.iclo6.eq.0) then
              xv(1,i3)=xv(1,i3)+di0xs(ia)*dpsic
              xv(2,i3)=xv(2,i3)+di0zs(ia)*dpsic
              yv(1,i3)=yv(1,i3)+dip0xs(ia)*dpsic
              yv(2,i3)=yv(2,i3)+dip0zs(ia)*dpsic
            endif
            chi=chi+dchi
  320     continue
          write(6,10260) ia,nms(ia)*izu0,dpsv(ia)
          write(6,10060) xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),sigmv(ia),
     +    dpsv(ia),xv(1,ia+1),yv(1,ia+1),xv(2,ia+1),yv(2,ia+1), sigmv
     +    (ia+1),dpsv(ia+1)
C---------------------------------------  END OF 'ANFB'
          if(iclo6.eq.2) then
            xv(1,ia)=xv(1,ia)+clo6v(1,ia)
            yv(1,ia)=yv(1,ia)+clop6v(1,ia)
            xv(2,ia)=xv(2,ia)+clo6v(2,ia)
            yv(2,ia)=yv(2,ia)+clop6v(2,ia)
            sigmv(ia)=sigmv(ia)+clo6v(3,ia)
            dpsv(ia)=dpsv(ia)+clop6v(3,ia)
            xv(1,ia+1)=xv(1,ia+1)+clo6v(1,ia)
            yv(1,ia+1)=yv(1,ia+1)+clop6v(1,ia)
            xv(2,ia+1)=xv(2,ia+1)+clo6v(2,ia)
            yv(2,ia+1)=yv(2,ia+1)+clop6v(2,ia)
            sigmv(ia+1)=sigmv(ia+1)+clo6v(3,ia)
            dpsv(ia+1)=dpsv(ia+1)+clop6v(3,ia)
            oidpsv(ia)=one/(one+dpsv(ia))
            oidpsv(ia+1)=one/(one+dpsv(ia+1))
          endif
          if(iclo6.ne.2) then
            xv(1,ia)=xv(1,ia)+clov(1,ia)*idz(1)*(1-idfor)
            yv(1,ia)=yv(1,ia)+clopv(1,ia)*idz(1)*(1-idfor)
            xv(2,ia)=xv(2,ia)+clov(2,ia)*idz(2)*(1-idfor)
            yv(2,ia)=yv(2,ia)+clopv(2,ia)*idz(2)*(1-idfor)
            xv(1,ia+1)=xv(1,ia+1)+clov(1,ia)*idz(1)*(1-idfor)
            yv(1,ia+1)=yv(1,ia+1)+clopv(1,ia)*idz(1)*(1-idfor)
            xv(2,ia+1)=xv(2,ia+1)+clov(2,ia)*idz(2)*(1-idfor)
            yv(2,ia+1)=yv(2,ia+1)+clopv(2,ia)*idz(2)*(1-idfor)
          endif
          ejfv(ia)=e0f*(one+dpsv(ia))
          ejfv(ia+1)=e0f*(one+dpsv(ia+1))
          ejv(ia)=sqrt(ejfv(ia)*ejfv(ia)+pma*pma)
          ejv(ia+1)=sqrt(ejfv(ia+1)*ejfv(ia+1)+pma*pma)
          epsa(1)=(ampv(ia)*ampv(ia)/bet0v(ia,1))
          epsa(2)=(amp(2)*amp(2)/bet0v(ia,2))
          write(6,10020) ampv(ia),amp(2),epsa
        else
          read(13,*,iostat=ierro) xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),
     +    sigmv(ia),dpsv(ia),xv(1,ia+1),yv(1,ia+1),xv(2,ia+1),yv
     +    (2,ia+1), sigmv(ia+1),dpsv(ia+1),e0,ejv(ia),ejv(ia+1)
          if(ierro.ne.0) call error(56)
          e0f=sqrt(e0*e0-pma*pma)
          ejfv(ia)=sqrt(ejv(ia)*ejv(ia)-pma*pma)
          ejfv(ia+1)=sqrt(ejv(ia+1)*ejv(ia+1)-pma*pma)
          oidpsv(ia)=one/(one+dpsv(ia))
          oidpsv(ia+1)=one/(one+dpsv(ia+1))
        endif
        write(6,10090) xv(1,ia),yv(1,ia),xv(2,ia),yv(2,ia),sigmv(ia),
     +  dpsv(ia),xv(1,ia+1),yv(1,ia+1),xv(2,ia+1),yv(2,ia+1), sigmv
     +  (ia+1),dpsv(ia+1),e0,ejv(ia),ejv(ia+1)
        idam=3
        icode=0
        if(abs(xv(1,ia)).le.pieni.and.abs(yv(1,ia)).le.pieni) then
          idam=idam-1
        else
          icode=icode+1
        endif
        if(abs(xv(2,ia)).le.pieni.and.abs(yv(2,ia)).le.pieni) then
          idam=idam-1
        else
          icode=icode+2
        endif
        if(idp.eq.0.or.abs(ition).eq.0) then
          idam=idam-1
        else
          icode=icode+4
        endif
        if(idam.eq.0) idam=1
        ia2=(ia+1)/2
        if(ntwin.ne.2) then
          if(mod(ia+1,2).eq.0) then
            xau(1,1)= xv(1,ia-1)
            xau(1,2)= yv(1,ia-1)
            xau(1,3)= xv(2,ia-1)
            xau(1,4)= yv(2,ia-1)
            xau(1,5)=sigmv(ia-1)
            xau(1,6)= dpsv(ia-1)
            xau(2,1)= xv(1,ia)
            xau(2,2)= yv(1,ia)
            xau(2,3)= xv(2,ia)
            xau(2,4)= yv(2,ia)
            xau(2,5)=sigmv(ia)
            xau(2,6)= dpsv(ia)
            cloau(1)= clo6v(1,ia)
            cloau(2)=clop6v(1,ia)
            cloau(3)= clo6v(2,ia)
            cloau(4)=clop6v(2,ia)
            cloau(5)= clo6v(3,ia)
            cloau(6)=clop6v(3,ia)
            di0au(1)= di0xs(ia)
            di0au(2)=dip0xs(ia)
            di0au(3)= di0zs(ia)
            di0au(4)=dip0zs(ia)
            do 330 ib2=1,6
              do 330 ib3=1,6
                tau(ib2,ib3)=tasau(ia,ib2,ib3)
  330       continue
            call distance(xau,cloau,di0au,tau,dam1)
            dam(ia)=dam1
            dam(ia-1)=dam1
          endif
          write(91-ia2,iostat=ierro) sixtit,commen,cdate, ctime,progrm,
     +    ia,ia,napx,icode,numl,qwcs(ia,1),qwcs(ia,2), qwcs(ia,3),clo6v
     +    (1,ia),clop6v(1,ia),clo6v(2,ia),clop6v(2,ia), clo6v(3,ia),
     +    clop6v(3,ia), di0xs(ia),dip0xs(ia),di0zs(ia),dip0zs(ia),zero,
     +    one, tas(ia,1,1),tas(ia,1,2),tas(ia,1,3),tas(ia,1,4),tas
     +    (ia,1,5), tas(ia,1,6), tas(ia,2,1),tas(ia,2,2),tas(ia,2,3),tas
     +    (ia,2,4),tas(ia,2,5), tas(ia,2,6), tas(ia,3,1),tas(ia,3,2),tas
     +    (ia,3,3),tas(ia,3,4),tas(ia,3,5), tas(ia,3,6), tas(ia,4,1),tas
     +    (ia,4,2),tas(ia,4,3),tas(ia,4,4),tas(ia,4,5), tas(ia,4,6), tas
     +    (ia,5,1),tas(ia,5,2),tas(ia,5,3),tas(ia,5,4),tas(ia,5,5), tas
     +    (ia,5,6), tas(ia,6,1),tas(ia,6,2),tas(ia,6,3),tas(ia,6,4),tas
     +    (ia,6,5), tas(ia,6,6),
     +    dble(mmac),dble(nms(ia)),dble(izu0),
     +    dble(numlr),sigcor,dpscor,dble(nrturn),zero,zero,zero,
     +    zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,
     +    zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,
     +    zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,
     +    zero,zero,zero,zero,zero,zero,zero,zero,zero,zero
        else
          write(91-ia2,iostat=ierro) sixtit,commen,cdate, ctime,progrm,
     +    ia,ia+1,napx,icode,numl,qwcs(ia,1),qwcs(ia,2), qwcs(ia,3),
     +    clo6v(1,ia),clop6v(1,ia),clo6v(2,ia),clop6v(2,ia), clo6v
     +    (3,ia),clop6v(3,ia), di0xs(ia),dip0xs(ia),di0zs(ia),dip0zs
     +    (ia),zero,one, tas(ia,1,1),tas(ia,1,2),tas(ia,1,3),tas
     +    (ia,1,4),tas(ia,1,5), tas(ia,1,6), tas(ia,2,1),tas(ia,2,2),tas
     +    (ia,2,3),tas(ia,2,4),tas(ia,2,5), tas(ia,2,6), tas(ia,3,1),tas
     +    (ia,3,2),tas(ia,3,3),tas(ia,3,4),tas(ia,3,5), tas(ia,3,6), tas
     +    (ia,4,1),tas(ia,4,2),tas(ia,4,3),tas(ia,4,4),tas(ia,4,5), tas
     +    (ia,4,6), tas(ia,5,1),tas(ia,5,2),tas(ia,5,3),tas(ia,5,4),tas
     +    (ia,5,5), tas(ia,5,6), tas(ia,6,1),tas(ia,6,2),tas(ia,6,3),tas
     +    (ia,6,4),tas(ia,6,5), tas(ia,6,6),
     +    dble(mmac),dble(nms(ia)),dble(izu0),
     +    dble(numlr),sigcor,dpscor,dble(nrturn),zero,zero,zero,
     +    zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,
     +    zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,
     +    zero,zero,zero,zero,zero,zero,zero,zero,zero,zero,
     +    zero,zero,zero,zero,zero,zero,zero,zero,zero,zero
        endif
        if(ierro.ne.0) then
          write(6,*)
          write(6,*) '*** ERROR ***,PROBLEMS WRITTING TO FILE # : ',91
     +    -ia2
          write(6,*) 'ERROR CODE : ',ierro
          write(6,*)
          goto 520
        endif
  340 continue
      if(e0.gt.pieni) then
        do 350 j=1,napx
  350   rvv(j)=ejv(j)/e0*e0f/ejfv(j)
      else
        call error(79)
      endif
      if(ithick.eq.1) call envarsv(dpsv,oidpsv,rvv,ekv)
C-------------------------------------  START OF 'BLOCK'
      do 440 k=1,mblo
        jm=mel(k)
        ikk=mtyp(k,1)
        do 370 lkk=1,2
          do 370 mkk=1,6
            do 360 ia=1,napx
              dpoff=dpsv(ia)*c1e3
              if(abs(dpoff).le.pieni) dpoff=one
              hv(mkk,lkk,ia,1)=al(mkk,lkk,ia,ikk)
  360       if(mkk.eq.5.or.mkk.eq.6) hv(mkk,lkk,ia,1)=hv(mkk,lkk,ia,1)
     +      /dpoff
  370   continue
        if(jm.eq.1) goto 410
        do 400 j=2,jm
          ikk=mtyp(k,j)
          do 390 lkk=1,2
            do 380 ia=1,napx
              dpoff=dpsv(ia)*c1e3
              if(abs(dpoff).le.pieni) dpoff=one
              hv(1,lkk,ia,j)=hv(1,lkk,ia,j-1)*al(1,lkk,ia,ikk)+ hv(3,
     +        lkk,ia,j-1)*al(2,lkk,ia,ikk)
              hv(2,lkk,ia,j)=hv(2,lkk,ia,j-1)*al(1,lkk,ia,ikk)+ hv(4,
     +        lkk,ia,j-1)*al(2,lkk,ia,ikk)
              hv(3,lkk,ia,j)=hv(1,lkk,ia,j-1)*al(3,lkk,ia,ikk)+ hv(3,
     +        lkk,ia,j-1)*al(4,lkk,ia,ikk)
              hv(4,lkk,ia,j)=hv(2,lkk,ia,j-1)*al(3,lkk,ia,ikk)+ hv(4,
     +        lkk,ia,j-1)*al(4,lkk,ia,ikk)
              hv(5,lkk,ia,j)=hv(5,lkk,ia,j-1)*al(1,lkk,ia,ikk)+ hv(6,
     +        lkk,ia,j-1)*al(2,lkk,ia,ikk)+al(5,lkk,ia,ikk)/dpoff
              hv(6,lkk,ia,j)=hv(5,lkk,ia,j-1)*al(3,lkk,ia,ikk)+ hv(6,
     +        lkk,ia,j-1)*al(4,lkk,ia,ikk)+al(6,lkk,ia,ikk)/dpoff
  380       continue
  390     continue
  400   continue
  410   do 430 lkk=1,2
          do 430 mkk=1,6
            do 420 ia=1,napx
  420       bl1v(mkk,lkk,ia,k)=hv(mkk,lkk,ia,jm)
  430   continue
  440 continue
C---------------------------------------  END OF 'BLOCK'
      write(6,10200)
C---------------------------------------  LOOP OVER TURNS TO BE TRACKED
      if(ithick.eq.0) call trauthin(nthinerr)
      if(ithick.eq.1) call trauthck(nthinerr)
      if(nthinerr.eq.3000) goto 520
      if(nthinerr.eq.3001) goto 460
C---------------------------------------  END OF LOOP OVER TURNS
  460 continue
      napxto=0
      id=0
      numx=numx+1
      call writebin(nthinerr)
      if(nthinerr.eq.3000) goto 520
      do 470 ia=1,napxo,2
        ie=ia+1
        ia2=(ie)/2
        napxto=napxto+numxv(ia)+numxv(ie)
        if(pstop(ia).and.pstop(ie)) then
C-- BOTH PARTICLES LOST
          write(6,10000) ia,nms(ia)*izu0,dp0v(ia),numxv(ia),
     +    abs(xvl(1,ia)),aperv(ia,1),abs(xvl(2,ia)),aperv(ia,2)
          write(6,10000) ie,nms(ia)*izu0,dp0v(ia),numxv(ie),
     +    abs(xvl(1,ie)),aperv(ie,1),abs(xvl(2,ie)),aperv(ie,2)
          write(6,10280)
     +    xvl(1,ia),yvl(1,ia),xvl(2,ia),yvl(2,ia),sigmvl(ia),dpsvl(ia),
     +    xvl(1,ie),yvl(1,ie),xvl(2,ie),yvl(2,ie),sigmvl(ie),dpsvl(ie),
     +    e0,ejvl(ia),ejvl(ie)
          write(12,10280,iostat=ierro)
     +    xvl(1,ia),yvl(1,ia),xvl(2,ia),yvl(2,ia),sigmvl(ia),dpsvl(ia),
     +    xvl(1,ie),yvl(1,ie),xvl(2,ie),yvl(2,ie),sigmvl(ie),dpsvl(ie),
     +    e0,ejvl(ia),ejvl(ie)
          if(ierro.ne.0) write(*,*) 'Warning from maincr: fort.12 has ',
     +    'corrupted output probably due to lost particle: ',ia,
     +    ' or: ',ie
        endif
        if(.not.pstop(ia).and.pstop(ie)) then
C-- SECOND PARTICLE LOST
          id=id+1
          write(6,10240) ia,nms(ia)*izu0,dp0v(ia),numxv(ia)
          write(6,10000) ie,nms(ia)*izu0,dp0v(ia),numxv(ie),
     +    abs(xvl(1,ie)),aperv(ie,1),abs(xvl(2,ie)),aperv(ie,2)
          write(6,10280)
     +    xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),
     +    xvl(1,ie),yvl(1,ie),xvl(2,ie),yvl(2,ie),sigmvl(ie),dpsvl(ie),
     +    e0,ejv(id),ejvl(ie)
          write(12,10280,iostat=ierro)
     +    xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),
     +    xvl(1,ie),yvl(1,ie),xvl(2,ie),yvl(2,ie),sigmvl(ie),dpsvl(ie),
     +    e0,ejv(id),ejvl(ie)
          if(ierro.ne.0) write(*,*) 'Warning from maincr: fort.12 has ',
     +    'corrupted output probably due to lost particle: ',ie
        endif
        if(pstop(ia).and..not.pstop(ie)) then
C-- FIRST PARTICLE LOST
          id=id+1
          write(6,10000) ia,nms(ia)*izu0,dp0v(ia),numxv(ia),
     +    abs(xvl(1,ia)),aperv(ia,1),abs(xvl(2,ia)),aperv(ia,2)
          write(6,10240) ie,nms(ia)*izu0,dp0v(ia),numxv(ie)
          write(6,10280)
     +    xvl(1,ia),yvl(1,ia),xvl(2,ia),yvl(2,ia),sigmvl(ia),dpsvl(ia),
     +    xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),
     +    e0,ejvl(ia),ejv(id)
          write(12,10280,iostat=ierro)
     +    xvl(1,ia),yvl(1,ia),xvl(2,ia),yvl(2,ia),sigmvl(ia),dpsvl(ia),
     +    xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),
     +    e0,ejvl(ia),ejv(id)
          if(ierro.ne.0) write(*,*) 'Warning from maincr: fort.12 has ',
     +    'corrupted output probably due to lost particle: ',ia
        endif
        if(.not.pstop(ia).and..not.pstop(ie)) then
C-- BOTH PARTICLES STABLE
          id=id+1
          ig=id+1
          write(6,10270) ia,ie,nms(ia)*izu0,dp0v(ia),numxv(ia)
          write(6,10280)
     +    xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),
     +    xv(1,ig),yv(1,ig),xv(2,ig),yv(2,ig),sigmv(ig),dpsv(ig),
     +    e0,ejv(id),ejv(ig)
          write(12,10280,iostat=ierro)
     +    xv(1,id),yv(1,id),xv(2,id),yv(2,id),sigmv(id),dpsv(id),
     +    xv(1,ig),yv(1,ig),xv(2,ig),yv(2,ig),sigmv(ig),dpsv(ig),
     +    e0,ejv(id),ejv(ig)
          if(ierro.ne.0) write(*,*) 'Warning from maincr: fort.12 has ',
     +    'corrupted output although particles stable'
          id=ig
        endif
  470 continue
      iposc=0
      if(ipos.eq.1) then
        do 480 ia=1,napxo,2
          ia2=(ia+1)/2
          iposc=iposc+1
          call postpr(91-ia2)
  480   continue
        if(iposc.ge.1) call sumpos
      endif
      goto 520
  490 if(ipos.eq.1) then
        ndafi2=ndafi
        do 500 ia=1,ndafi2
          if(ia.gt.ndafi) goto 510
          call postpr(91-ia)
          close(91-ia)
  500   continue
  510   if(ndafi.ge.1) call sumpos
      endif
  520 continue
C--HPLOTTING END
      if(ipos.eq.1.and.
     &(idis.ne.0.or.icow.ne.0.or.istw.ne.0.or.iffw.ne.0)) call hplend
      write(6,10300) napxto
C-----------------------------------------------------------------------
      stop
10000 format(1h /t10,'TRACKING ENDED ABNORMALLY'/t10, 'PARTICLE ',i3,
     +' RANDOM SEED ',i8,/ t10,' MOMENTUM DEVIATION ',g12.5,
     +' LOST IN REVOLUTION ',i8,/ t10,'HORIZ:  AMPLITUDE = ',f15.3,
     +'   APERTURE = ',f15.3/ t10,'VERT:   AMPLITUDE = ',f15.3,
     +'   APERTURE = ',f15.3/)
10010 format(1h /t10,'SIXTRACK VECTOR VERSION 2 (with tilt)',
     +'  --  (last change: 17.02.1998)'//)
10020 format(1h /t10,'UNCOUPLED AMPLITUDES AND EMITTANCES:', /t10,
     +'AMPLITUDE-X = ',f15.3,10x,'AMPLITUDE-Z = ',f15.3, '  MM'/t10,
     +'EMITTANCE-X = ',f15.3,10x,'EMITTANCE-Z = ',f15.3, '  PI*MRAD*MM')
10025 format(1h /t10,'Run started from binary dump file # 32')
10030 format(1h /t10,'STRUCTURE INPUT FILE HAS -THICK- LINEAR ',
     +'ELEMENTS'//)
10040 format(1h /t10,'STRUCTURE INPUT FILE HAS ONLY -THIN- LINEAR ',
     +'ELEMENTS'//)
10050 format(//131('-')//t10,27(1ho)/t10,2(1ho),23x,2(1ho)/t10,
     +'OO  INITIAL COORDINATES  OO'/ t10,2(1ho),23x,2(1ho)/t10,27(1ho)
     +//131('-')//)
10060 format(/5x,'---- TWIN-TRAJECTORIES NO CL.ORBIT ADDED'/ 5x,'/X1  /'
     +,f47.33/5x,'/XP1 /',f47.33/ 5x,'/Z1  /',f47.33/5x,'/ZP1 /',f47.33/
     +5x,'/SIG1/',f47.33/5x,'/DP1 /',f47.33/ 5x,'/X2  /',f47.33/5x,
     +'/XP2 /',f47.33/ 5x,'/Z2  /',f47.33/5x,'/ZP2 /',f47.33/ 5x,
     +'/SIG2/',f47.33/5x,'/DP2 /',f47.33/)
10070 format(1h /131('-'))
10080 format(1h /t10,'REL. MOMENTUM DEVIATION=',f19.16/ t8,
     +'========================================')
10090 format(/5x,'---- INITIAL COORD. OF TWIN-TRAJECTORIES'/ 15(10x,f47.
     +33/))
10110 format(/5x,'---- CLOSED ORBIT AND DECOUPLING (1=COU,0=DECOU)'/ 5x,
     +'/CLX /',f47.33/5x,'/CLXP/',f47.33/ 5x,'/CLZ /',f47.33/5x,'/CLZP/'
     +,f47.33/ 5x,'/DCX / ',i4/5x,'/DCZ / ',i4/ 5x,'/IVER /',i4/ 5x,
     +'/IDFOR/',i4/ 5x,'/ICLO6/',i4/ 5x,'/ITION/',i4/5x/)
10120 format(/5x,'---- CLOSED ORBIT AND DECOUPLING (1=COU,0=DECOU)'/ 5x,
     +'/CLX /',f47.33/5x,'/CLXP/',f47.33/ 5x,'/CLZ /',f47.33/5x,'/CLZP/'
     +,f47.33/ 5x,'/CLS /',f47.33/5x,'/CLSP/',f47.33/ 5x,'/DCX / ',i4/5
     +x,'/DCZ / ',i4/ 5x,'/IVER /',i4/ 5x,'/IDFOR/',i4/ 5x,'/ICLO6/',i4/
     +5x,'/ITION/',i4/5x/)
10130 format(/t10,' SIX-DIMENSIONAL CLOSED ORBIT CALCULATED'/ t10,
     +'FOR SEED NUMBER = ',i1)
10140 format(1x,f47.33/1x,f47.33)
10150 format(/t10,'TRACKING WITH SYNCHROTRON OSCILLATIONS'/ 15x,
     +'ACCELERATION WITH PHASE = ',f8.4/ t15,
     +'       TUNE         CLO            CLOP           ',
     +'   BET0           ALF0           GAMMA      '//
     +t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/
     +t10,'  Z  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9,f15.9/)
10160 format(/t10,'TRACKING WITH SYNCHROTRON OSCILLATIONS'/ 15x,
     +'ACCELERATION WITH PHASE = ',f8.4/ t15,
     +'       TUNE         CLO            CLOP           ',
     +'   BET0           ALF0           GAMMA      '//
     +t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/
     +t10,'  Z  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/
     +t10,'  S  ',f14.10,2(1x,g14.8),1x,f15.7,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/)
10170 format(/t10,'TRACKING FOR CONSTANT MOMENTUM DEVIATION'// 15x,
     +'------ NO ACCELERATION ------'// t15,
     +'       TUNE         CLO            CLOP           ',
     +'   BET0           ALF0           GAMMA      '//
     +t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/
     +t10,'  Z  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/)
10180 format(t5//t5,'BACK-TRACKING'/ t5, '============='//)
10190 format(t10,'TRACKING FOR CONSTANT MOMENTUM DEVIATION'// 15x,
     +'ACCELERATION WITH PHASE = ',f8.4/ t15,
     +'       TUNE         CLO            CLOP           ',
     +'   BET0           ALF0           GAMMA      '//
     +t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/
     +t10,'  Z  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/)
10200 format(//131('-')//t10,16(1ho)/t10,2(1ho),12x,2(1ho)/t10,
     +'OO  TRACKING  OO', /t10,2(1ho),12x,2(1ho)/t10,16(1ho)//131('-')//
     +)
10210 format(/t10,'TRACKING WITH SYNCHROTRON OSCILLATIONS'/ 15x,
     +'------ NO ACCELERATION ------'// t15,
     +'       TUNE         CLO            CLOP           ',
     +'   BET0           ALF0           GAMMA      '//
     +t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/
     +t10,'  Z  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/)
10220 format(/t10,'TRACKING WITH SYNCHROTRON OSCILLATIONS'/ 15x,
     +'------ NO ACCELERATION ------'// t15,
     +'       TUNE         CLO            CLOP           ',
     +'   BET0           ALF0           GAMMA      '//
     +t10,'  X  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/
     +t10,'  Z  ',f14.10,2(1x,g14.8),1x,f15.9,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/
     +t10,'  S  ',f14.10,2(1x,g14.8),1x,f15.7,1x,f15.10,f15.9/
     +t60,f15.9,1x,f15.10,f15.9/t60,f15.9,1x,f15.10,f15.9/)
10230 format(t10,'NO OPTICAL SOLUTION FOR',2x,f19.16,2x,
     +'RELATIVE MOMENTUM DEVIATION')
10240 format(1x/5x,'PARTICLE ',i3,' STABLE - RANDOM SEED ', i8,
     +' MOMENTUM DEVIATION ',g12.5 /5x,'REVOLUTION ',i8/)
10250 format(1x/5x,'PARTICLE ',i3,' RANDOM SEED ',i8,
     +' MOMENTUM DEVIATION ',g12.5 /5x,'REVOLUTION ',i8/)
10260 format(1x/5x,'PARTICLE ',i3,' RANDOM SEED ',i8,
     +' MOMENTUM DEVIATION ',g12.5/)
10270 format(1x/5x,'PARTICLE ',i3,' AND ',i3,' STABLE - RANDOM SEED ',
     +i8,' MOMENTUM DEVIATION ',g12.5 /5x,'REVOLUTION ',i8/)
10280 format(10x,f47.33)
10290 format(/10x,'The Preparating Calculations took',f12.3,' second(s)'
     +,' of Computing Time')
10300 format(/10x,'Sixtrack ',i12,' Turn(s)',
     +' Completed'//131('-'))
10310 format(//10x,'Total Time used: ',g12.3,' second(s)'//131('-'))
10320 format(//131('-')//t10,'DATA BLOCK FLUCTUATIONS OF MULTIPOLES'//
     +t10,'RANDOM STARTING NUMBER=  ',i20/ t10,
     +'RANDOM NUMBERS GENERATED:',i20/ t10,'MEAN VALUE=',f15.7,
     +'  -   DEVIATION=',f15.7)
10330 format(/10x,'ERROR IN OPENING FILES')
10340 format(//131('-')//t10,'6D TUNE-VARIATION' / /t10,
     +'TUNE'           ,26x,'THEORET.       AFTER CORRECTION'/ t10,
     +'HORIZONTAL'     ,15x,g20.14,g20.14/ t10,
     +'VERTICAL'       ,17x,g20.14,g20.14// t10,
     +'ITERATION:'     ,21x,I3/ t10,
     +'ACCURACY:'      ,17x,g16.10/)
10350 format(t10,'QUADRU.STRENGTHS',7x,g16.10,2x,g16.10,'   TYP     ',
     +   a16/t10,                  23x,g16.10,2x,g16.10,'           ',
     +a16)
      end
