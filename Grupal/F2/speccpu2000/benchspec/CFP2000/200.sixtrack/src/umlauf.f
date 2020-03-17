C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine umlauf(dpp,ium,ierr)
C-----------------------------------------------------------------------
C     ONE TURN-TRANSFORMATION (INCLUDING QUADRUPOLE CONTRIBUTIONS)
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
      dimension aa(mmul),bb(mmul),dpr(5)
      dimension cr(mmul),ci(mmul)
      save
C-----------------------------------------------------------------------
      do 10 i=1,mmul
        aa(i)=zero
        bb(i)=zero
        cr(i)=zero
        ci(i)=zero
   10 continue
      do 20 i=1,5
        dpr(i)=zero
   20 continue
      ierr=0
      dpr(1)=dpp*c1e3
      izu=0
      do 350 k=1,iu
        ix=ic(k)
        if(ix.gt.nblo) goto 60
        if(ix.le.0) goto 40
        do 30 j=1,ium
          do 30 kx=1,2
            if(ithick.eq.1) then
              puf=x(j,kx)
              x(j,kx)=bl1(ix,kx,1)*puf+bl1(ix,kx,2)*y(j,kx)+dpr(j)*bl1
     +        (ix,kx,5)
              y(j,kx)=bl1(ix,kx,3)*puf+bl1(ix,kx,4)*y(j,kx)+dpr(j)*bl1
     +        (ix,kx,6)
            else
              x(j,kx)=x(j,kx)+bl1(ix,kx,2)*y(j,kx)
            endif
   30   continue
        goto 350
   40   ix=-ix
        do 50 j=1,ium
          do 50 kx=1,2
            if(ithick.eq.1) then
              puf=x(j,kx)
              x(j,kx)=bl2(ix,kx,1)*puf+bl2(ix,kx,2)*y(j,kx)+dpr(j)*bl2
     +        (ix,kx,5)
              y(j,kx)=bl2(ix,kx,3)*puf+bl2(ix,kx,4)*y(j,kx)+dpr(j)*bl2
     +        (ix,kx,6)
            else
              x(j,kx)=x(j,kx)+bl2(ix,kx,2)*y(j,kx)
            endif
   50   continue
        goto 350
   60   ix=ix-nblo
        qu=zero
        qv=zero
        dyy1=zero
        dyy2=zero
        kpz=kp(ix)
        if(kpz.eq.6) goto 350
        kzz=kz(ix)
        if(abs(x(1,1)).lt.c1e4.and.abs(x(1,2)).lt.c1e4) goto 70
        ierr=1
        write(6,10010)
        write(6,10000)
        return
   70   if(kzz.eq.0) goto 350
        if(iorg.lt.0) mzu(k)=izu
        izu=mzu(k)+1
        ekk=(sm(ix)+zfz(izu)*ek(ix))/(one+dpp)
        izu=izu+1
        xs=xpl(ix)+zfz(izu)*xrms(ix)
        izu=izu+1
        zs=zpl(ix)+zfz(izu)*zrms(ix)
        xl=(x(1,1)-xs)*tiltc(k)+(x(1,2)-zs)*tilts(k)
        zl=-(x(1,1)-xs)*tilts(k)+(x(1,2)-zs)*tiltc(k)
        crkve=xl
        cikve=zl
        if(kzz.lt.0) goto 220
        goto(80,90,100,110,120,130,140,150,160,170,180),kzz
        goto 350
C--HORIZONTAL DIPOLE
   80   ekk=ekk*c1e3
        y(1,1)=y(1,1)+ekk*tiltc(k)
        y(1,2)=y(1,2)+ekk*tilts(k)
        goto 350
C--NORMAL QUADRUPOLE
   90   continue
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*tiltck
        qv=-ekk*tiltsk
        goto 330
C--NORMAL SEXTUPOLE
  100   ekk=ekk*c1m3
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*crkve+tiltsk*cikve)
        qv=ekk*two*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--NORMAL OCTUPOLE
  110   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=three*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--NORMAL DECAPOLE
  120   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=four*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--NORMAL DODECAPOLE
  130   ekk=ekk*c1m12
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=5*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--NORMAL 14-POLE
  140   ekk=ekk*c1m15
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
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=6*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--NORMAL 16-POLE
  150   ekk=ekk*c1m18
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
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=7*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--NORMAL 18-POLE
  160   ekk=ekk*c1m21
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
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=8*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--NORMAL 20-POLE
  170   ekk=ekk*c1m24
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
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*crkve+tiltsk*cikve)
        qv=9*ekk*(tiltck*cikve-tiltsk*crkve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*crkve
        dyy2=-ekk*cikve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
  180   r0=ek(ix)
        if(abs(dki(ix,1)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=-dki(ix,1)/dki(ix,3)*dki(ix,1)/(one+dpp)
            y(1,1)=y(1,1)+(qu*xl-dpp*c1e3*dki(ix,1)
     &      /(one+dpp))*tiltc(k)
     +      +c1e3*dki(ix,1)/(one+dpp)*(one-tiltc(k))
            y(1,2)=y(1,2)+(qu*xl-dpp*c1e3*dki(ix,1)
     &      /(one+dpp))*tilts(k)
     +      +c1e3*dki(ix,1)/(one+dpp)*tilts(k)
            do 190 j=2,ium
              y(j,1)=y(j,1)+qu*x(j,1)*tiltc(k)
              y(j,2)=y(j,2)+qu*x(j,2)*tilts(k)
  190       continue
          else
            y(1,1)=y(1,1)-dki(ix,1)*dpp/(one+dpp)*c1e3*tiltc(k)
     +      +c1e3*dki(ix,1)/(one+dpp)*(one-tiltc(k))
            y(1,2)=y(1,2)-dki(ix,1)*dpp/(one+dpp)*c1e3*tilts(k)
     +      +c1e3*dki(ix,1)/(one+dpp)*tilts(k)
          endif
        endif
        if(abs(dki(ix,2)).gt.pieni) then
          if(abs(dki(ix,3)).gt.pieni) then
            qu=dki(ix,2)/dki(ix,3)*dki(ix,2)/(one+dpp)
            y(1,1)=y(1,1)+(qu*zl-dpp*c1e3*dki(ix,2)
     &      /(one+dpp))*tilts(k)
     +      +c1e3*dki(ix,2)/(one+dpp)*tilts(k)
            y(1,2)=y(1,2)+(-qu*zl+dpp*c1e3*dki(ix,2)
     &      /(one+dpp))*tiltc(k)
     +      -c1e3*dki(ix,2)/(one+dpp)*(one-tiltc(k))
            do 200 j=2,ium
              y(j,1)=y(j,1)+qu*x(j,1)*tilts(k)
              y(j,2)=y(j,2)-qu*x(j,2)*tiltc(k)
  200       continue
          else
            y(1,1)=y(1,1)-dki(ix,2)*dpp/(one+dpp)*c1e3*tilts(k)
     +      +dki(ix,2)/(one+dpp)*c1e3*tilts(k)
            y(1,2)=y(1,2)+dki(ix,2)*dpp/(one+dpp)*c1e3*tiltc(k)
     +      -dki(ix,2)/(one+dpp)*c1e3*(one-tiltc(k))
          endif
        endif
        if(abs(r0).le.pieni) goto 350
        nmz=nmu(ix)
        if(nmz.eq.0) then
          izu=izu+2*mmul
          goto 350
        endif
        im=irm(ix)
        r0a=one
        benkr=ed(ix)/(one+dpp)
        do 210 l=1,nmz
          izu=izu+1
          aa(l)=ak0(im,l)+zfz(izu)*aka(im,l)
          aa(l)=benkr*aa(l)/r0a
          izu=izu+1
          bb(l)=bk0(im,l)+zfz(izu)*bka(im,l)
          bb(l)=benkr*bb(l)/r0a
          r0a=r0a*r0
  210   continue
        if(nmz.ge.2) then
          qu=bb(2)
          qv=-aa(2)
          dyy1=bb(1)+bb(2)*crkve+aa(2)*cikve
          dyy2=aa(1)-bb(2)*cikve+aa(2)*crkve
          do 215 l=3,nmz
            l1=l-1
            qu=qu+l1*(bb(l)*crkve+aa(l)*cikve)
            qv=qv+l1*(bb(l)*cikve-aa(l)*crkve)
            crkveuk=crkve*xl-cikve*zl
            cikve=crkve*zl+cikve*xl
            crkve=crkveuk
            dyy1=dyy1+bb(l)*crkve+aa(l)*cikve
            dyy2=dyy2-bb(l)*cikve+aa(l)*crkve
  215     continue
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
        y(1,1)=y(1,1)+dyy1
        y(1,2)=y(1,2)+dyy2
        if(ium.eq.1) goto 350
        goto 330
C--SKEW ELEMENTS
  220   kzz=-kzz
        goto(230,240,250,260,270,280,290,300,310,320),kzz
        goto 350
C--VERTICAL DIPOLE
  230   ekk=ekk*c1e3
        y(1,1)=y(1,1)-ekk*tilts(k)
        y(1,2)=y(1,2)+ekk*tiltc(k)
        goto 350
C--SKEW QUADRUPOLE
  240   continue
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=-ekk*tiltsk
        qv=-ekk*tiltck
        goto 330
C--SKEW SEXTUPOLE
  250   ekk=ekk*c1m3
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=ekk*two*(tiltck*cikve-tiltsk*crkve)
        qv=-ekk*two*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--SKEW OCTUPOLE
  260   ekk=ekk*c1m6
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=three*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-three*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--SKEW DECAPOLE
  270   ekk=ekk*c1m9
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=four*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-four*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--SKEW DODECAPOLE
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
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=5*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-5*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--SKEW 14-POLE
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
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=6*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-6*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--SKEW 16-POLE
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
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=7*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-7*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--SKEW 18-POLE
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
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=8*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-8*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
        goto 330
C--SKEW 20-POLE
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
        if(ium.ne.1) then
        tiltck=tiltc(k)*tiltc(k)-tilts(k)*tilts(k)
        tiltsk=two*tiltc(k)*tilts(k)
        qu=9*ekk*(tiltck*cikve-tiltsk*crkve)
        qv=-9*ekk*(tiltck*crkve+tiltsk*cikve)
        endif
                        crkveuk=crkve*xl-cikve*zl
                        cikve=crkve*zl+cikve*xl
                        crkve=crkveuk
        dyy1=ekk*cikve
        dyy2=ekk*crkve
        y(1,1)=y(1,1)+tiltc(k)*dyy1-tilts(k)*dyy2
        y(1,2)=y(1,2)+tiltc(k)*dyy2+tilts(k)*dyy1
        if(ium.eq.1) goto 350
  330   do 340 j=2,ium
          y(j,1)=y(j,1)+x(j,1)*qu-qv*x(j,2)
          y(j,2)=y(j,2)-x(j,2)*qu-qv*x(j,1)
  340   continue
  350 continue
C-----------------------------------------------------------------------
      return
10000 format(10x,'AMPLITUDES EXCEED THE MAXIMUM VALUES IN UMLAUF'/)
10010 format(1h /131('-'))
      end
