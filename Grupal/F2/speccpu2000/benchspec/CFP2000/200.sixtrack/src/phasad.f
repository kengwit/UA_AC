C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine phasad(dpp,qwc)
C-----------------------------------------------------------------------
C  ADDITIONAL ADJUSTMENT OF THE X-PHASEADVANCE BETWEEN 2 POSITIONS
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
      dimension t(5,4)
      dimension beta(2),alfa(2),phi(2),phibf(2)
      dimension qw(2),qwc(3)
      dimension aa(mmul),bb(mmul),dpr(5)
      dimension cr(mmul),ci(mmul)
      save
C-----------------------------------------------------------------------
      ium=5
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
        phi(i)=zero
        phibf(i)=zero
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
      pie=two*pi
      ikpv=0
      dpr(1)=dpp*c1e3
      call clorb(dpp)
      call betalf(dpp,qw)
      if(ierro.ne.0) call error(22+ierro)
      call envar(dpp)
C--STARTVALUES OF THE TRAJECTORIES
      do 50 l=1,2
        ll=2*l
        alfa(l)=alf0(l)
        beta(l)=bet0(l)
        t(1,ll-1)=clo(l)
   50 t(1,ll)=clop(l)
      do 60 i=1,4
        do 60 j=1,4
          t(i+1,j)=ta(j,i)
   60 t(i+1,j)=ta(j,i)
C--SINGLE TURN BLOCKLOOP
      izu=0
      do 450 k=1,iu
        ix=ic(k)
        if(ix.gt.nblo) goto 140
        jj=0
        dj=1
        if(ix.gt.0) goto 70
        ix=-ix
        jj=mel(ix)+1
        dj=-1
   70   jm=mel(ix)
C--BLOCKELEMENTLOOP
        do 130 j=1,jm
          jj=jj+dj
          jk=mtyp(ix,jj)
          if(ithick.eq.1.and.kz(jk).ne.0) goto 100
          if(ithick.eq.0.and.kz(jk).ne.0) goto 450
C--PURE DRIFTLENGTH
          do 80 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=pi2
            endif
            do 80 i=1,ium
   80     t(i,ll-1)=t(i,ll-1)+t(i,ll)*(el(jk))
          do 90 l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=pi2-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
   90     phi(l)=phi(l)+dphi/pie
          goto 130
C--MAGNETELEMENT
  100     continue
          do 110 l=1,2
            ll=2*l
            if(abs(t(ll,ll-1)).gt.pieni) then
              phibf(l)=atan(t(ll+1,ll-1)/t(ll,ll-1))
            else
              phibf(l)=zero
            endif
            do 110 i=1,ium
              puf=t(i,ll-1)
              t(i,ll-1)=puf*a(jk,l,1)+t(i,ll)*a(jk,l,2)+dpr(i)*a(jk,l,5)
  110     t(i,ll)=puf*a(jk,l,3)+t(i,ll)*a(jk,l,4)+dpr(i)*a(jk,l,6)
          do 120 l=1,2
            ll=2*l
            beta(l)=t(ll,ll-1)*t(ll,ll-1)+t(ll+1,ll-1)*t(ll+1,ll-1)
            alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
            if(abs(t(ll,ll-1)).gt.pieni) then
              dphi=atan(t(ll+1,ll-1)/t(ll,ll-1))-phibf(l)
            else
              dphi=-phibf(l)
            endif
            if(-dphi.gt.pieni) dphi=dphi+pi
  120     phi(l)=phi(l)+dphi/pie
  130   continue
        goto 450
C--NL-INSERTION
  140   ix=ix-nblo
        qu=zero
        qv=zero
        dyy1=zero
        dyy2=zero
        kpz=kp(ix)
        if(kpz.eq.6) goto 450
        kzz=kz(ix)
        kpv=kpa(ix)
        if(kpv.ne.1) goto 150
        qxsa=phi(1)
  150   if(kpv.ne.2.or.ikpv.eq.1) goto 160
        qxse=phi(1)
        qwc(3)=qxse-qxsa
        ikpv=1
  160   if(kzz.eq.0) goto 450
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
        if(kzz.lt.0) goto 310
        goto(170,180,190,200,210,220,230,240,250,260,270),kzz
        goto 450
C--HORIZONTAL DIPOLE
  170   ekk=ekk*c1e3
        dyy1=ekk*tiltc(k)
        dyy2=ekk*tilts(k)
        qu=zero
        qv=zero
        goto 420
C--NORMAL QUADRUPOLE
  180   continue
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        goto 420
C--NORMAL SEXTUPOLE
  190   ekk=ekk*c1m3
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*crkve+tiltsk*cikve)
        qv=ekk*two*(tiltck*cikve-tiltsk*crkve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        dyy2=ekk*(-tiltc(k)*cikve+tilts(k)*crkve)
        goto 420
C--NORMAL OCTUPOLE
  200   ekk=ekk*c1m6
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
        goto 420
C--NORMAL DECAPOLE
  210   ekk=ekk*c1m9
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
        goto 420
C--NORMAL DODECAPOLE
  220   ekk=ekk*c1m12
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
        goto 420
C--NORMAL 14-POLE
  230   ekk=ekk*c1m15
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
        goto 420
C--NORMAL 16-POLE
  240   ekk=ekk*c1m18
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
        goto 420
C--NORMAL 18-POLE
  250   ekk=ekk*c1m21
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
        goto 420
C--NORMAL 20-POLE
  260   ekk=ekk*c1m24
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
        goto 420
  270   r0=ek(ix)
        if(abs(dki(ix,1)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)+(qu*xl-dppi*dpp)*tiltc(k)
     +      +dppi*(one-tiltc(k))
            t(1,4)=t(1,4)+(qu*xl-dppi*dpp)*tilts(k)
     +      +dppi*tilts(k)
            do 280 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tiltc(k)
              t(i,4)=t(i,4)+qu*t(i,3)*tilts(k)
  280       continue
          else
            dppi=c1e3*dki(ix,1)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tiltc(k)
     +      +dppi*(one-tiltc(k))
            t(1,4)=t(1,4)-dppi*dpp*tilts(k)
     +      +dppi*tilts(k)
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
            do 290 i=2,ium
              t(i,2)=t(i,2)+qu*t(i,1)*tilts(k)
              t(i,4)=t(i,4)-qu*t(i,3)*tiltc(k)
  290       continue
          else
            dppi=c1e3*dki(ix,2)/(one+dpp)
            t(1,2)=t(1,2)-dppi*dpp*tilts(k)
     +      +dppi*tilts(k)
            t(1,4)=t(1,4)+dppi*dpp*tiltc(k)
     +      -dppi*(one-tiltc(k))
          endif
        endif
        if(abs(r0).le.pieni) goto 450
        nmz=nmu(ix)
        if(nmz.eq.0) then
          izu=izu+2*mmul
          goto 450
        endif
        im=irm(ix)
        r0a=one
        benkr=ed(ix)/(one+dpp)
        do 300 l=1,nmz
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
  300   continue
        if(nmz.ge.2) then
          qu=bb(2)
          qv=-aa(2)
          dyy1=bb(1)+bb(2)*crkve+aa(2)*cikve
          dyy2=aa(1)-bb(2)*cikve+aa(2)*crkve
          do 305 l=3,nmz
            l1=l-1
            qu=qu+l1*(bb(l)*crkve+aa(l)*cikve)
            qv=qv+l1*(bb(l)*cikve-aa(l)*crkve)
            crkveuk=crkve*xl-cikve*zl
            cikve=crkve*zl+cikve*xl
            crkve=crkveuk
            dyy1=dyy1+bb(l)*crkve+aa(l)*cikve
            dyy2=dyy2-bb(l)*cikve+aa(l)*crkve
  305     continue
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
        goto 420
C--SKEW ELEMENTS
  310   kzz=-kzz
        goto(320,330,340,350,360,370,380,390,400,410),kzz
        goto 450
C--VERTICAL DIPOLE
  320   ekk=ekk*c1e3
        dyy1=-ekk*tilts(k)
        dyy2=ekk*tiltc(k)
        qu=zero
        qv=zero
        goto 420
C--SKEW QUADRUPOLE
  330   continue
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        goto 420
C--SKEW SEXTUPOLE
  340   ekk=ekk*c1m3
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*cikve-tiltsk*crkve)
        qv=-ekk*two*(tiltck*crkve+tiltsk*cikve)
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*(tiltc(k)*cikve-tilts(k)*crkve)
        dyy2=ekk*(tiltc(k)*crkve+tilts(k)*cikve)
        goto 420
C--SKEW OCTUPOLE
  350   ekk=ekk*c1m6
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
        goto 420
C--SKEW DECAPOLE
  360   ekk=ekk*c1m9
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
        goto 420
C--SKEW DODECAPOLE
  370   ekk=ekk*c1m12
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
        goto 420
C--SKEW 14-POLE
  380   ekk=ekk*c1m15
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
        goto 420
C--SKEW 16-POLE
  390   ekk=ekk*c1m18
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
        goto 420
C--SKEW 18-POLE
  400   ekk=ekk*c1m21
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
        goto 420
C--SKEW 20-POLE
  410   ekk=ekk*c1m24
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
  420   t(1,2)=t(1,2)+dyy1
        t(1,4)=t(1,4)+dyy2
        do 430 i=2,ium
          t(i,2)=t(i,2)+qu*t(i,1)-qv*t(i,3)
          t(i,4)=t(i,4)-qu*t(i,3)-qv*t(i,1)
  430   continue
        do 440 l=1,2
          ll=2*l
  440   alfa(l)=-(t(ll,ll-1)*t(ll,ll)+t(ll+1,ll-1)*t(ll+1,ll))
  450 continue
      qwc(1)=phi(1)
      qwc(2)=phi(2)
C-----------------------------------------------------------------------
      return
      end
