C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine linopt(dpp)
C-----------------------------------------------------------------------
C  LINEAR PARAMETERS AT THE POSITION OF EVERY ELEMENT OR BLOCK
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
      integer dj
      character*16 idum
      dimension t(6,4)
      dimension beta(2),alfa(2),phibf(2),phi(2)
      dimension clo0(2),clop0(2),di00(2),dip00(2),qw(2),qwc(3)
      dimension aa(mmul),bb(mmul),dpr(6)
      dimension cr(mmul),ci(mmul)
      dimension aeg(nele,2,6),bl1eg(nblo,2,6),bl2eg(nblo,2,6)
      save
      data dpr/6*0d0/
C-----------------------------------------------------------------------
      nhmoni=0
      nvmoni=0
      nhcorr=0
      nvcorr=0
      ium=6
      pie=two*pi
      if(ncorru.eq.0) then
        write (6,10010)
        write(6,10000)
      endif
      do 10 i=1,ium
        dpr(i)=zero
   10 continue
      do 20 i=1,ium
        do 20 j=1,4
          t(i,j)=zero
   20 continue
      do 30 i=1,2
        beta(i)=zero
        alfa(i)=zero
        phibf(i)=zero
        phi(i)=zero
        clo0(i)=zero
        clop0(i)=zero
        di00(i)=zero
        dip00(i)=zero
        qw(i)=zero
        qwc(i)=zero
   30 continue
      qwc(3)=zero
      do 40 i=1,mmul
        aa(i)=zero
        bb(i)=zero
        cr(i)=zero
        ci(i)=zero
   40 continue
      etl=zero
      nr=0
      dpr(1)=dpp*c1e3
      dpr(6)=c1e3
      dpp1=dpp+ded
      call clorb(dpp1)
      do 50 l=1,2
        clo0(l)=clo(l)
   50 clop0(l)=clop(l)
      call clorb(dpp)
      do 60 l=1,2
        ll=2*l
        di0(l)=(clo0(l)-clo(l))/ded
        dip0(l)=(clop0(l)-clop(l))/ded
        t(6,ll-1)=di0(l)
   60 t(6,ll)=dip0(l)
      if(ncorru.eq.0) then
        write(6,10010)
        write(6,10050) (di0(l),dip0(l),l=1,2)
      endif
      call betalf(dpp,qw)
      call phasad(dpp,qwc)
      if(ierro.ne.0) call error(22+ierro)
      if(ncorru.eq.0) write(6,10040) dpp,qwc(1),qwc(2)
      call envar(dpp)
      if(ithick.eq.1) call envardis(dpp1,aeg,bl1eg,bl2eg)
C--STARTVALUES OF THE TRAJECTORIES
      do 70 l=1,2
        ll=2*l
        t(1,ll-1)=clo(l)
   70 t(1,ll)=clop(l)
      do 80 i=1,4
        do 80 j=1,4
          t(i+1,j)=ta(j,i)
   80 t(i+1,j)=ta(j,i)
      if(ncorru.eq.0) then
        write(6,10010)
        if(iprint.ne.0) write(6,10030)
        write(6,10020)
        write(6,10010)
      endif
      iflag=0
      idum='START'
      call writelin(nr,idum,etl,phi,t,ix)
      if(ntco.ne.0.and.mod(nr,ntco).eq.0) call cpltwis(idum,t,etl,phi)
C--SINGLE TURN BLOCKLOOP
      if(nt.le.0.or.nt.gt.iu) nt=iu
      izu=0
      do 500 k=1,nt
        ix=ic(k)
        if(ix.gt.nblo) goto 220
        if(ithick.eq.1.and.iprint.ne.0) goto 160
        jj=0
        dj=1
        if(ix.gt.0) goto 90
        ix=-ix
        jj=mel(ix)+1
        dj=-1
   90   jm=mel(ix)
C--BLOCKELEMENTLOOP
        do 150 j=1,jm
          jj=jj+dj
          jk=mtyp(ix,jj)
          if(ithick.eq.1.and.kz(jk).ne.0) goto 120
          if(ithick.eq.0.and.kz(jk).ne.0) goto 500
C--PURE DRIFTLENGTH
          etl=etl+el(jk)
          do 100 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=pi2
            endif
            do 100 i=1,ium
  100     t(i,ll-1)=t(i,ll-1)+t(i,ll)*(el(jk))
          do 110 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=pi2-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
  110     phi(l)=phi(l)+dphi/pie
          nr=nr+1
          call writelin(nr,bez(jk),etl,phi,t,ix)
          if(ntco.ne.0.and.mod(nr,ntco).eq.0) call cpltwis(bez(jk),t,
     +    etl, phi)
          goto 150
C--MAGNETELEMENT
  120     continue
          if(kz(jk).ne.8) etl=etl+el(jk)
          do 130 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            puf=t(6,ll-1)
            t(6,ll-1)=(aeg(jk,l,1)*(t(1,ll-1)+puf*ded)+ aeg(jk,l,2)*(t
     +      (1,ll)+t(6,ll)*ded)+aeg(jk,l,5)*dpp1*c1e3- a(jk,l,1)*t
     +      (1,ll-1)-a(jk,l,2)*t(1,ll)- a(jk,l,5)*dpr(1))/ded
            t(6,ll)=(aeg(jk,l,3)*(t(1,ll-1)+puf*ded)+ aeg(jk,l,4)*(t
     +      (1,ll)+t(6,ll)*ded)+aeg(jk,l,6)*dpp1*c1e3- a(jk,l,3)*t
     +      (1,ll-1)-a(jk,l,4)*t(1,ll)- a(jk,l,6)*dpr(1))/ded
            do 130 i=1,ium-1
              puf=t(i,ll-1)
              t(i,ll-1)=puf*a(jk,l,1)+t(i,ll)*a(jk,l,2)+dpr(i)*a(jk,l,5)
  130     t(i,ll)=puf*a(jk,l,3)+t(i,ll)*a(jk,l,4)+dpr(i)*a(jk,l,6)
          do 140 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
  140     phi(l)=phi(l)+dphi/pie
          nr=nr+1
          call writelin(nr,bez(jk),etl,phi,t,ix)
          if(ntco.ne.0.and.mod(nr,ntco).eq.0) call cpltwis(bez(jk),t,
     +    etl, phi)
  150   continue
        goto 500
C--BETACALCULATION FOR SERIES OF BLOCKS
  160   continue
        if(ix.le.0) goto 190
C--REGULAR RUN THROUGH BLOCKS
        etl=etl+elbe(ix)
        do 170 l=1,2
          ll=2*l
          if(abs(t(ll,ll-1)).gt.pieni) then
            phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
          else
            phibf(l)=zero
          endif
          puf=t(6,ll-1)
          t(6,ll-1)=(bl1eg(ix,l,1)*(t(1,ll-1)+puf*ded)+ bl1eg(ix,l,2)*(t
     +    (1,ll)+t(6,ll)*ded)+ bl1eg(ix,l,5)*dpp1*c1e3- bl1(ix,l,1)*t
     +    (1,ll-1)-bl1(ix,l,2)*t(1,ll)- bl1(ix,l,5)*dpr(1))/ded
          t(6,ll)=(bl1eg(ix,l,3)*(t(1,ll-1)+puf*ded)+ bl1eg(ix,l,4)*(t
     +    (1,ll)+t(6,ll)*ded)+ bl1eg(ix,l,6)*dpp1*c1e3- bl1(ix,l,3)*t
     +    (1,ll-1)-bl1(ix,l,4)*t(1,ll)- bl1(ix,l,6)*dpr(1))/ded
          do 170 i=1,ium-1
            puf=t(i,ll-1)
            t(i,ll-1)=bl1(ix,l,1)*puf+bl1(ix,l,2)*t(i,ll)+dpr(i)*bl1
     +      (ix,l,5)
  170   t(i,ll)=bl1(ix,l,3)*puf+bl1(ix,l,4)*t(i,ll)+dpr(i)*bl1(ix,l,6)
        do 180 l=1,2
          ll=2*l
          if(abs(t(ll,ll-1)).gt.pieni) then
            dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
          else
            dphi=-phibf(l)
          endif
          if(-dphi.gt.pieni) dphi=dphi+pi
  180   phi(l)=phi(l)+dphi/pie
        nr=nr+1
        call writelin(nr,bezb(ix),etl,phi,t,ix)
        if(ntco.ne.0.and.mod(nr,ntco).eq.0) call cpltwis(bezb(ix),t,etl,
     +  phi)
        goto 500
C--REVERSE RUN THROUGH BLOCKS
  190   ix=-ix
        etl=etl+elbe(ix)
        do 200 l=1,2
          ll=2*l
          if(abs(t(ll,ll-1)).gt.pieni) then
            phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
          else
            phibf(l)=zero
          endif
          puf=t(6,ll-1)
          t(6,ll-1)=(bl2eg(ix,l,1)*(t(1,ll-1)+puf*ded)+ bl2eg(ix,l,2)*(t
     +    (1,ll)+t(6,ll)*ded)+ bl2eg(ix,l,5)*dpp1*c1e3- bl2(ix,l,1)*t
     +    (1,ll-1)-bl2(ix,l,2)*t(1,ll)- bl2(ix,l,5)*dpr(1))/ded
          t(6,ll)=(bl2eg(ix,l,3)*(t(1,ll-1)+puf*ded)+ bl2eg(ix,l,4)*(t
     +    (1,ll)+t(6,ll)*ded)+ bl2eg(ix,l,6)*dpp1*c1e3- bl2(ix,l,3)*t
     +    (1,ll-1)-bl2(ix,l,4)*t(1,ll)- bl2(ix,l,6)*dpr(1))/ded
          do 200 i=1,ium-1
            puf=t(i,ll-1)
            t(i,ll-1)=bl2(ix,l,1)*puf+bl2(ix,l,2)*t(i,ll)+dpr(i)*bl2
     +      (ix,l,5)
  200   t(i,ll)=bl2(ix,l,3)*puf+bl2(ix,l,4)*t(i,ll)+dpr(i)*bl2(ix,l,6)
        do 210 l=1,2
          ll=2*l
          if(abs(t(ll,ll-1)).gt.pieni) then
            dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
          else
            dphi=-phibf(l)
          endif
          if(-dphi.gt.pieni) dphi=dphi+pi
  210   phi(l)=phi(l)+dphi/pie
        nr=nr+1
        call writelin(nr,bezb(ix),etl,phi,t,ix)
        if(ntco.ne.0.and.mod(nr,ntco).eq.0) call cpltwis(bezb(ix),t,etl,
     +  phi)
        goto 500
C--NL-INSERTION
  220   ix=ix-nblo
        qu=zero
        qv=zero
        dyy1=zero
        dyy2=zero
        kpz=kp(ix)
        if(kpz.eq.6) goto 500
        kzz=kz(ix)
        if(kzz.eq.0) goto 500
        if(kzz.eq.20.and.nbeam.ge.1) then
          nbeam=k
          nr=nr+1
          call writelin(nr,bez(ix),etl,phi,t,ix)
        endif
        dyy1=zero
        dyy2=zero
        if(iorg.lt.0) mzu(k)=izu
        izu=mzu(k)+1
        ekk=(sm(ix)+zfz(izu)*ek(ix))/(one+dpp)
        izu=izu+1
        xs=xpl(ix)+zfz(izu)*xrms(ix)
        izu=izu+1
        zs=zpl(ix)+zfz(izu)*zrms(ix)
        xl=(t(1,1)-xs)*tiltc(k)+(t(1,3)-zs)*tilts(k)
        zl=-(t(1,1)-xs)*tilts(k)+(t(1,3)-zs)*tiltc(k)
        crkve=xl
        cikve=zl
        if(kzz.lt.0) goto 370
        goto(230,240,250,260,270,280,290,300,310,320,330),kzz
        goto 500
C--HORIZONTAL DIPOLE
  230   ekk=ekk*c1e3
        dyy1=ekk*tiltc(k)
        dyy2=ekk*tilts(k)
        qu=zero
        qv=zero
        goto 480
C--NORMAL QUADRUPOLE
  240   continue
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        goto 480
C--NORMAL SEXTUPOLE
  250   ekk=ekk*c1m3
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*crkve+tiltsk*cikve)
        qv=ekk*two*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
C--NORMAL OCTUPOLE
  260   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=three*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
C--NORMAL DECAPOLE
  270   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=four*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
C--NORMAL DODECAPOLE
  280   ekk=ekk*c1m12
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=5*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
C--NORMAL 14-POLE
  290   ekk=ekk*c1m15
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=6*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
C--NORMAL 16-POLE
  300   ekk=ekk*c1m18
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=7*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
C--NORMAL 18-POLE
  310   ekk=ekk*c1m21
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=8*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
C--NORMAL 20-POLE
  320   ekk=ekk*c1m24
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=9*ekk*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 480
  330   r0=ek(ix)
        if(abs(dki(ix,1)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)+(qu*xl-dppi*dpp)*tiltc(k)
     +      +dppi*(one-tiltc(k))
            t(1,4)=t(1,4)+(qu*xl-dppi*dpp)*tilts(k)
     +      +dppi*tilts(k)
            t(6,2)=t(6,2)-(qu*xl+dppi)/(one+dpp)*tiltc(k)
     +      -dppi/(one+dpp)*(one-tiltc(k))
            t(6,4)=t(6,4)-(qu*xl+dppi)/(one+dpp)*tilts(k)
     +      -dppi/(one+dpp)*tilts(k)
            do 340 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tiltc(k)
              t(i,4)=t(i,4)+qu*t(i,3)*tilts(k)
  340       continue
          else
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tiltc(k)
     +      +dppi*(one-tiltc(k))
            t(1,4)=t(1,4)-dppi*dpp*tilts(k)
     +      +dppi*tilts(k)
            t(6,2)=t(6,2)-dppi/(one+dpp)*tiltc(k)
     +      -dppi/(one+dpp)*(one-tiltc(k))
            t(6,4)=t(6,4)-dppi/(one+dpp)*tilts(k)
     +      -dppi/(one+dpp)*tilts(k)
          endif
        endif
        if(abs(dki(ix,2)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=dki(ix,2)/dki(ix,3)*dki(ix,2)/(one+dpp)
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)+(qu*zl-dppi*dpp)*tilts(k)
     +      +dppi*tilts(k)
            t(1,4)=t(1,4)+(-qu*zl+dppi*dpp)*tiltc(k)
     +      -dppi*(one-tiltc(k))
            t(6,2)=t(6,2)+(-qu*zl-dppi)/(one+dpp)*tilts(k)
     +      -dppi/(one+dpp)*tilts(k)
            t(6,4)=t(6,4)+(qu*zl+dppi)/(one+dpp)*tiltc(k)
     +      +dppi/(one+dpp)*(one-tiltc(k))
            do 350 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tilts(k)
              t(i,4)=t(i,4)-qu*t(i,3)*tiltc(k)
  350       continue
          else
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tilts(k)
     +      +dppi*tilts(k)
            t(1,4)=t(1,4)+dppi*dpp*tiltc(k)
     +      -dppi*(one-tiltc(k))
            t(6,2)=t(6,2)-dppi/(one+dpp)*tilts(k)
     +      -dppi/(one+dpp)*tilts(k)
            t(6,4)=t(6,4)+dppi/(one+dpp)*tiltc(k)
     +      +dppi/(one+dpp)*(one-tiltc(k))
          endif
        endif
        if(abs(r0).le.pieni) goto 500
        nmz=nmu(ix)
        if(nmz.eq.0) then
          izu=izu+2*mmul
          goto 500
        endif
        im=irm(ix)
        r0a=one
        benkr=ed(ix)/(one+dpp)
        do 360 l=1,nmz
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
  360   continue
        if(nmz.ge.2) then
          qu=bb(2)
          qv=-aa(2)
          dyy1=bb(1)+bb(2)*crkve+aa(2)*cikve
          dyy2=aa(1)-bb(2)*cikve+aa(2)*crkve
          do 365 l=3,nmz
            l1=l-1
            qu=qu+l1*(bb(l)*crkve+aa(l)*cikve)
            qv=qv+l1*(bb(l)*cikve-aa(l)*crkve)
            crkveuk=crkve*xl-cikve*zl
            cikve=crkve*zl+cikve*xl
            crkve=crkveuk
            dyy1=dyy1+bb(l)*crkve+aa(l)*cikve
            dyy2=dyy2-bb(l)*cikve+aa(l)*crkve
  365     continue
        else
          qu=zero
          qv=zero
          dyy1=bb(1)
          dyy2=aa(1)
        endif
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu1=tiltck*qu-tiltsk*qv
        qv=tiltck*qv+tiltsk*qu
        qu=qu1
        dyy11=tiltc(k)*dyy1-tilts(k)*dyy2
        dyy2=tiltc(k)*dyy2+tilts(k)*dyy1
        dyy1=dyy11
        izu=izu+2*mmul-2*nmz
        goto 480
C--SKEW ELEMENTS
  370   kzz=-kzz
        goto(380,390,400,410,420,430,440,450,460,470),kzz
        goto 500
C--VERTICAL DIPOLE
  380   ekk=ekk*c1e3
        dyy1=-ekk*tilts(k)
        dyy2=ekk*tiltc(k)
        qu=zero
        qv=zero
        goto 480
C--SKEW QUADRUPOLE
  390   continue
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        goto 480
C--SKEW SEXTUPOLE
  400   ekk=ekk*c1m3
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*cikve-tiltsk*crkve)
        qv=-ekk*two*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
C--SKEW OCTUPOLE
  410   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-three*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
C--SKEW DECAPOLE
  420   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-four*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
C--SKEW DODECAPOLE
  430   ekk=ekk*c1m12
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-5*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
C--SKEW 14-POLE
  440   ekk=ekk*c1m15
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-6*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
C--SKEW 16-POLE
  450   ekk=ekk*c1m18
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-7*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
C--SKEW 18-POLE
  460   ekk=ekk*c1m21
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-8*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 480
C--SKEW 20-POLE
  470   ekk=ekk*c1m24
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-9*ekk*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
  480   t(6,2)=t(6,2)-dyy1/(one+dpp)
        t(6,4)=t(6,4)-dyy2/(one+dpp)
        t(1,2)=t(1,2)+dyy1
        t(1,4)=t(1,4)+dyy2
        do 490 i=2,ium
          t(i,2)=t(i,2)+qu*t(i,1)-qv*t(i,3)
  490   t(i,4)=t(i,4)-qu*t(i,3)-qv*t(i,1)
        nr=nr+1
        call writelin(nr,bez(ix),etl,phi,t,ix)
        if(ntco.ne.0.and.mod(nr,ntco).eq.0) call cpltwis(bez(ix),t,etl,
     +  phi)
  500 continue
      call clorb(ded)
      do 510 l=1,2
        clo0(l)=clo(l)
        clop0(l)=clop(l)
  510 continue
      call clorb(zero)
      do 520 l=1,2
        ll=2*l
        di0(l)=(clo0(l)-clo(l))/ded
        dip0(l)=(clop0(l)-clop(l))/ded
  520 continue
      if(ncorru.eq.0)
     +write(6,10060)
C-----------------------------------------------------------------------
      return
10000 format(t5 ,'---- ENTRY LINOPT ----')
10010 format(132('-'))
10020 format(1h ,'  NR     TYP      L-TOTAL    P     PHI          ',
     +'BETA         ALFA         GAMMA        DIS        DISP         ',
     +'CLO        CLOP'/ 1x,
     +'                    (M)           (2*PI)        ',
     +'(M)          (RAD)         (M)         (M)        (RAD)        ',
     +'(MM)       (MRAD)')
10030 format(1h ,'  LINEAR OPTICS CALCULATION WITH PRINTOUT ',
     +'AFTER EACH BLOCK'/
     +'   A T T E N T I O N : BETATRON PHASE CALCULATION MIGHT BE WRONG'
     +,' BY A MULTIPLE OF 0.5 FOR EACH LARGE BLOCK'/)
10040 format(1h /10x,'RELATIVE ENERGY DEVIATION  ',t40,f10.7/ 10x,
     +'TUNES -HORIZONTAL',t40,f10.7/ 10x,'      -VERTICAL  ',t40,f10.7/)
10050 format(t8,'  PLANE          DISP(MM)                 DISP(MRAD)'/
     +t6,'      X  ',2(f20.12,6x)/t10,'  Z  ',2(f20.12,6x)/)
10060 format(//131('-')//)
      end
