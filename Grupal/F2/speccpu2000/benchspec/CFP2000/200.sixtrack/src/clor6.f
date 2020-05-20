C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine clor6
C-----------------------------------------------------------------------
C  CALCULATION OF THE SIX-DIMENSIONAL CLOSED ORBIT
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
      common/sixdim/aml6(6,6),edcor(2),mapout
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor),
     &idalloc
      common/norf/nordf,nvarf,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax
      common/tcorrc/coel(10)
      dimension x1(3),y1(3)
C    &                     ,x0(3),y0(3)
      dimension dclo(3),dclop(3)
      dimension dx(3),dy(3)
C    &                     ,am(6,6),a6i(6,6)
      integer ind(6),nerror
      save
C-----------------------------------------------------------------------
      write(6,10010)
      mapout=0
      iqmod6o=iqmod6
      iqmod6=0
      idalloc=1
      do 80 ii=1,itco
        do 20 l=1,2
          x(1,l)=clo6(l)
          y(1,l)=clop6(l)
   20   continue
        sigm(1)=clo6(3)
        dps(1)=clop6(3)
        if(ii.eq.itco) then
          idalloc=2
          if(iqmod6o.eq.0) mapout=1
        endif
        call umlau6
        call dinv(6,aml6,6,ind,nerror)
        if(nerror.ne.0) write(6,*) ' ATTENTION, MATRIX SINGULAR '
        do 30 l=1,2
          x1(l)=x(1,l)
          y1(l)=y(1,l)
   30   continue
        x1(3)=sigm(1)
        y1(3)=dps(1)
        do 40 l=1,3
          dx(l)=clo6(l)-x1(l)
          dy(l)=clop6(l)-y1(l)
   40   continue
        dcx=abs(dx(1))
        dcxp=abs(dy(1))
        dcy=abs(dx(2))
        dcyp=abs(dy(2))
        dcz=abs(dx(3))*c1m2
        dczp=abs(dy(3))
        if(dcx.le.c1m10.and.dcz.le.c1m10.and.dcxp.le.c1m10.and.dczp
     +  .le.c1m10.and.dcy.le.c1m10.and.dcyp.le.c1m10) goto 90
        do 60 k=1,3
          dclo(k)=zero
          dclop(k)=zero
          do 50 j=1,3
            jj=2*j
            kk=2*k
            dclo(k)=aml6(kk-1,jj-1)*dx(j)+dclo(k)
            dclo(k)=aml6(kk-1,jj)*dy(j)+dclo(k)
            dclop(k)=aml6(kk,jj-1)*dx(j)+dclop(k)
            dclop(k)=aml6(kk,jj)*dy(j)+dclop(k)
   50     continue
   60   continue
        write(6,10020) clo6(1),clop6(1),clo6(2),clop6(2),clo6(3),
     +  clop6(3)
        do 70 l=1,3
          clo6(l)=clo6(l)+dclo(l)
          clop6(l)=clop6(l)+dclop(l)
   70   continue
        cor=sqrt(dcx*dcx+dcy*dcy+dcz*dcz)
        if(ii.ne.itco) then
          write(6,10030) clo6(1),clop6(1), clo6(2),clop6(2),clo6(3),
     +    clop6(3), ii,cor
        endif
   80 continue
      write(6,10000) itco
      ii=itco
   90 continue
      if(ii.ne.itco) then
        do 65 k=1,3
          dclo(k)=zero
          dclop(k)=zero
          do 55 j=1,3
            jj=2*j
            kk=2*k
            dclo(k)=aml6(kk-1,jj-1)*dx(j)+dclo(k)
            dclo(k)=aml6(kk-1,jj)*dy(j)+dclo(k)
            dclop(k)=aml6(kk,jj-1)*dx(j)+dclop(k)
            dclop(k)=aml6(kk,jj)*dy(j)+dclop(k)
   55     continue
   65   continue
        write(6,10020) clo6(1),clop6(1),clo6(2),clop6(2),clo6(3),
     +  clop6(3)
        do 75 l=1,3
          clo6(l)=clo6(l)+dclo(l)
          clop6(l)=clop6(l)+dclop(l)
   75   continue
        cor=sqrt(dcx*dcx+dcy*dcy+dcz*dcz)
        write(6,10030) clo6(1),clop6(1), clo6(2),clop6(2),clo6(3),
     +  clop6(3), ii,cor
        do 95 l=1,2
          x(1,l)=clo6(l)
          y(1,l)=clop6(l)
   95   continue
        sigm(1)=clo6(3)
        dps(1)=clop6(3)
        idalloc=2
        if(iqmod6o.eq.0) mapout=1
        call umlau6
        call dinv(6,aml6,6,ind,nerror)
        if(nerror.ne.0) write(6,*) ' ATTENTION, MATRIX SINGULAR '
        do 35 l=1,2
          x1(l)=x(1,l)
          y1(l)=y(1,l)
   35   continue
        x1(3)=sigm(1)
        y1(3)=dps(1)
        do 45 l=1,3
          dx(l)=clo6(l)-x1(l)
          dy(l)=clop6(l)-y1(l)
   45   continue
        dcx=abs(dx(1))
        dcxp=abs(dy(1))
        dcy=abs(dx(2))
        dcyp=abs(dy(2))
        dcz=abs(dx(3))*c1m2
        dczp=abs(dy(3))
      endif
      iqmod6=iqmod6o
      cor=sqrt(dcx*dcx+dcy*dcy+dcz*dcz)
      write(6,10040) clo6(1),clop6(1),dcx,dcxp,clo6(2),clop6(2),dcy,
     +dcyp, clo6(3),clop6(3),dcz,dczp, ii,cor
      dps(1)=dps(1)
C-----------------------------------------------------------------------
      return
10000 format(t5/t10,'CLOSED ORBIT CALCULATION'/ t10,
     +'MAXIMUM NUMBER OF ITERATIONS ACHIEVED--->',2x,i4/ t10,
     +'PROCEDURE MAY NOT HAVE CONVERGED')
10010 format(t5,/'---- ENTRY CLOR6 ----'/)
10020 format(t5,'---- 6d closed orbit before correction----'/t5,
     +' CLOX ',2g16.9,/t5,' CLOZ ',2g16.9,/t5, ' CLOS ',2g16.9/)
10030 format(t5,'---- after correction----'/t5, ' CLOX ',2g16.9,/t5,
     +' CLOZ ',2g16.9,/t5, ' CLOS ',2g16.9,/t5,' ITERAT.=',i3,
     +' ACCURACY=',d13.6//)
10040 format(t5,'---- END CLOR6 ----'/t5,' CLOX ',2g16.9,/t5,'  D-X ', 
     +2g16.9,/t5,' CLOZ ',2g16.9,/t5,'  D-Z ',2g16.9, /t5,' CLOS ',2
     +g16.9,/t5,'  D-S ',2g16.9,/t5,' ITERAT.=',i3,' ACCURACY=',d13.6/)
      end
