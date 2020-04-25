C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine betalf(dpp,qw)
C-----------------------------------------------------------------------
C  CALCULATION OF : OPT. PARAMETERS AT THE STARTING POSITION:
C                   BETA-, ALFA-FUNCTIONS, Q-VALUES
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
      dimension rcw1(4),ycw1(4),rcw2(4),ycw2(4)
      dimension am(4,4)
      dimension qw(2)
      save
      ierro=0
      call matrix(dpp,am)
C--CALCULATION OF EIGENVALUES
   10 spa=am(1,1)+am(2,2)
      spd=am(3,3)+am(4,4)
      det=(am(1,3)+am(4,2))*(am(2,4)+am(3,1))
     &-(am(1,4)-am(3,2))*(am(2,3)-am(4,1))
      f0=spa-spd
      f1=spa+spd
      f2=f0*f0+four*det
      if(f2 .lt. zero) goto 160
      f2=sqrt(f2)
      if(f0) 30,20,20
   20 egwg1=(f1+f2)*half
      egwg2=(f1-f2)*half
      goto 40
   30 egwg1=(f1-f2)*half
      egwg2=(f1+f2)*half
   40 continue
      f1=egwg1*egwg1-four
      f2=egwg2*egwg2-four
      rca1=f1
      yca1=zero
      rca2=f2
      yca2=zero
      if (rca1.ge.0) then
        rca1=sqrt(rca1)
      else
        yca1=sqrt(-rca1)
        rca1=zero
      endif
      if (rca2.ge.0) then
        rca2=sqrt(rca2)
      else
        yca2=sqrt(-rca2)
        rca2=zero
      endif
      rclam1=(egwg1+rca1)/two
      yclam1=yca1/two
      rclam2=(egwg2+rca2)/two
      yclam2=yca2/two
      if(egwg1*egwg1 .ge. four) goto 160
      if(egwg2*egwg2 .ge. four) goto 160
   50 continue
      detb=am(1,3)*am(2,4)-am(1,4)*am(2,3)
      detc=am(3,1)*am(4,2)-am(3,2)*am(4,1)
      fak1=spd-egwg1
      if(abs(fak1).gt.pieni) then
        rcw1(1)=-(am(1,3)*am(3,2)+am(1,4)*am(4,2))/fak1+am(1,2)
        ycw1(1)=zero
        rcw1(2)=(am(1,3)*am(3,1)+am(1,4)*am(4,1)+detb)/fak1-(am(1,1)
     +  -rclam1)
        ycw1(2)=yclam1
        rcw1(3)=-((am(3,1)+am(2,4))*rcw1(1)+(am(3,2)-am(1,4))*rcw1(2))
     +          /fak1
        ycw1(3)=-((am(3,1)+am(2,4))*ycw1(1)+(am(3,2)-am(1,4))*ycw1(2))
     +          /fak1
        rcw1(4)=-((am(4,1)-am(2,3))*rcw1(1)+(am(4,2)+am(1,3))*rcw1(2))
     +          /fak1
        ycw1(4)=-((am(4,1)-am(2,3))*ycw1(1)+(am(4,2)+am(1,3))*ycw1(2))
     +          /fak1
      else
        rcw1(1)=am(1,2)
        ycw1(1)=zero
        rcw1(2)=-am(1,1)+rclam1
        ycw1(2)=yclam1
        rcw1(3)=zero
        ycw1(3)=zero
        rcw1(4)=zero
        ycw1(4)=zero
      endif
      fak2=spa-egwg2
      if(abs(fak2).gt.pieni) then
        rcw2(3)=-(am(3,1)*am(1,4)+am(3,2)*am(2,4))/fak2+am(3,4)
        ycw2(3)=zero
        rcw2(4)=(am(3,1)*am(1,3)+am(3,2)*am(2,3)+detc)/fak2-(am(3,3)
     +  -rclam2)
        ycw2(4)=yclam2
        rcw2(1)=-((am(1,3)+am(4,2))*rcw2(3)+(am(1,4)-am(3,2))*rcw2(4))
     +         /fak2
        ycw2(1)=-((am(1,3)+am(4,2))*ycw2(3)+(am(1,4)-am(3,2))*ycw2(4))
     +         /fak2
        rcw2(2)=-((am(2,3)-am(4,1))*rcw2(3)+(am(2,4)+am(3,1))*rcw2(4))
     +         /fak2
        ycw2(2)=-((am(2,3)-am(4,1))*ycw2(3)+(am(2,4)+am(3,1))*ycw2(4))
     +         /fak2
      else
        rcw2(3)=am(3,4)
        ycw2(3)=zero
        rcw2(4)=-am(3,3)+rclam2
        ycw2(4)=yclam2
        rcw2(1)=zero
        ycw2(1)=zero
        rcw2(2)=zero
        ycw2(2)=zero
      endif
C--LEAVING COMPLEX NUMBERS
      do 60 i=1,4
        ta(i,1)=rcw1(i)
        ta(i,3)=rcw2(i)
        ta(i,2)=ycw1(i)
        ta(i,4)=ycw2(i)
   60 continue
C--NORMALISATION OF EIGENVALUES
      rn1=ta(1,1)*ta(2,2)-ta(2,1)*ta(1,2)
     &+ta(3,1)*ta(4,2)-ta(4,1)*ta(3,2)
      if(rn1) 70,160,90
   70 yclam1=-yclam1
      do 80 i=1,4
   80 ta(i,2)=-ta(i,2)
   90 sqrn=sqrt(abs(rn1))
      do 100 i=1,4
        ta(i,1)=ta(i,1)/sqrn
  100 ta(i,2)=ta(i,2)/sqrn
      rn2=ta(1,3)*ta(2,4)-ta(2,3)*ta(1,4)
     &+ta(3,3)*ta(4,4)-ta(4,3)*ta(3,4)
      if(rn2) 110,160,130
  110 yclam2=-yclam2
      do 120 i=1,4
  120 ta(i,4)=-ta(i,4)
  130 sqrn=sqrt(abs(rn2))
      do 140 i=1,4
        ta(i,3)=ta(i,3)/sqrn
  140 ta(i,4)=ta(i,4)/sqrn
      qw(1)= atan(yclam1/(one+rclam1))/pi
      qw(2)= atan(yclam2/(one+rclam2))/pi
C-----------------------------------------------------------------------
C  OPTICAL PARAMETERS AT THE STARTING POINT
C-----------------------------------------------------------------------
      betx(1)=ta(1,1)*ta(1,1)+ta(1,2)*ta(1,2)
      alfx(1)=-(ta(1,1)*ta(2,1)+ta(1,2)*ta(2,2))
      betx(2)=ta(1,3)*ta(1,3)+ta(1,4)*ta(1,4)
      alfx(2)=-(ta(1,3)*ta(2,3)+ta(1,4)*ta(2,4))
      betz(1)=ta(3,1)*ta(3,1)+ta(3,2)*ta(3,2)
      alfz(1)=-(ta(3,1)*ta(4,1)+ta(3,2)*ta(4,2))
      betz(2)=ta(3,3)*ta(3,3)+ta(3,4)*ta(3,4)
      alfz(2)=-(ta(3,3)*ta(4,3)+ta(3,4)*ta(4,4))
      bet0(1)=betx(1)
      alf0(1)=alfx(1)
      bet0(2)=betz(2)
      alf0(2)=alfz(2)
      if(ta(1,1).lt.-pieni) then
        do 150 i=1,4
          do 150 j=1,4
            ta(i,j)=-ta(i,j)
  150   continue
      endif
      return
C-----------------------------------------------------------------------
  160 ierro=1
      return
      end
