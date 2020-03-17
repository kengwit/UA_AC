C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine umlau6
C-----------------------------------------------------------------------
C  CENTRAL LOOP FOR 6-DIMENSIONAL CLOSED ORBIT
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
      common/tra/xxtr(mpa,2),yytr(mpa,2),amp(2),
     &bet0(2),alf0(2),clo(2),clop(2)
      common/chrom/cro(2),issss(2),ichrom
      common/dial/preda,idial,nord,nvar,nvar2,nsix,ncor,ipar(mcor),
     &idalloc
      common/norf/nordf,nvarf,inorm,imod1,imod2
      common/tcorr/icorr,nctype,namp,nmom,nmom1,nmom2,weig1,weig2,dpmax
      common/tcorrc/coel(10)
      common/sixdim/aml6(6,6),edcor(2),mapout
      common/xz/xsi(nblz),zsi(nblz),smi(nblz),
     &aai(nblz,mmul),bbi(nblz,mmul)
      common/rfres/rsmi(nblz),rfres(nblz),rzphs(nblz)
      common/damp/damp,ampt
      common/main4/ e0f,numx
      common/daele/alda,asda,aldaq,asdaq,smida,dpda,dpda1,sigmda,ej1,
     &ejf1,rv
      common/dael6/ald6(nele,2,6,nema),asd6(nele,2,6,nema)
C-----------------------------------------------------------------------
      dimension zfeld1(100),zfeld2(100)
      dimension jj(6),dpdav2(6)
C-----------------------------------------------------------------------
*FOX  B D ;
*FOX  D V DA COM SIGMDA NORD NVAR ; D V DA COM DPDA NORD NVAR ;
*FOX  D V DA COM DPDA1 NORD NVAR ; D V DA COM RV NORD NVAR ;
*FOX  D V DA COM EJ1 NORD NVAR ; D V DA COM EJF1 NORD NVAR ;
*FOX  D V DA COM ALDA NORD NVAR 2 6 ; D V DA COM ASDA NORD NVAR 2 6 ;
*FOX  D V DA COM ALDAQ NORD NVAR 2 6 ; D V DA COM ASDAQ NORD NVAR 2 6 ;
*FOX  D V DA COM SMIDA NORD NVAR MCOR ;
*FOX  D V DA INT X NORD NVAR 2 ; D V DA INT Y NORD NVAR 2 ;
*FOX  D V DA INT YP NORD NVAR 2 ; D V DA INT DKIP NORD NVAR ;
*FOX  D V DA INT CORROLD NORD NVAR 8 ; D V DA INT CORRNEW NORD NVAR 8 ;
*FOX  D V DA INT CORRAU1 NORD NVAR 8 ; D V DA INT CORRAU2 NORD NVAR 8 ;
*FOX  D V DA INT AA NORD NVAR 11 ;  D V DA INT BB NORD NVAR 11 ;
*FOX  D V DA INT PUX NORD NVAR ; D V DA INT PUZ NORD NVAR ;
*FOX  D V DA INT EJF0 NORD NVAR ; D V DA INT EKK NORD NVAR ;
*FOX  D V DA INT XL NORD NVAR ; D V DA INT ZL NORD NVAR ;
*FOX  D V DA INT CRKVE NORD NVAR ; D V DA INT CIKVE NORD NVAR ;
*FOX  D V DA INT CRKVEUK NORD NVAR ; D V DA INT CBZBF NORD NVAR ;
*FOX  D V DA INT YV1J NORD NVAR ; D V DA INT YV2J NORD NVAR ;
*FOX  D V DA INT CRKVEBF NORD NVAR ; D V DA INT CIKVEBF NORD NVAR ;
*FOX  D V DA INT RHO2BF NORD NVAR ; D V DA INT TKBF NORD NVAR ;
*FOX  D V DA INT R2BF NORD NVAR ; D V DA INT RBF NORD NVAR ;
*FOX  D V DA INT RKBF NORD NVAR ; D V DA INT XRBF NORD NVAR ;
*FOX  D V DA INT ZRBF NORD NVAR ; D V DA INT XBBF NORD NVAR ;
*FOX  D V DA INT ZBBF NORD NVAR ; D V DA INT CRXBF NORD NVAR ;
*FOX  D V DA INT CBXBF NORD NVAR ; D V DA INT CRZBF NORD NVAR ;
*FOX  D V DA INT XX NORD NVAR ; D V DA INT YY NORD NVAR ;
*FOX  D V DA INT WX NORD NVAR ; D V DA INT WY NORD NVAR ;
*FOX  D V RE INT AAI NBLZ MMUL ; D V RE INT BBI NBLZ MMUL ;
*FOX  D V RE INT TILTC NBLZ ; D V RE INT TILTS NBLZ ;
*FOX  D V RE INT DPS MPA ; D V RE INT SIGM MPA ; D V RE INT EKKS ;
*FOX  D V RE INT DKI NELE 3 ; D V RE INT BL1 NBLO 2 6 ;
*FOX  D V RE INT EL NELE ; D V RE INT EJ MPA ; D V RE INT EJF MPA ;
*FOX  D V RE INT SMI NBLZ ; D V RE INT ED1 ; D V RE INT ED2 ;
*FOX  D V RE INT DPDAV ; D V RE INT DPDAV2 6 ; D V RE INT BETR0 ;
*FOX  D V RE INT E0 ; D V RE INT E0F ; D V RE INT PMA ;
*FOX  D V RE INT XS ; D V RE INT ZS ; D V RE INT OX ; D V RE INT OXP ;
*FOX  D V RE INT OZ ; D V RE INT OZP ; D V RE INT SIGM1 ;
*FOX  D V RE INT BEAMOFF1 ; D V RE INT BEAMOFF2 ; D V RE INT DPS1 ;
*FOX  D V RE INT SIGMAN6 2 NELE ; D V RE INT CRAD ; D V RE INT GAMMAR ;
*FOX  D V RE INT PARTNUM ; D V RE INT PISQRT ; D V RE INT SCRKVEB ;
*FOX  D V RE INT SCIKVEB ; D V RE INT STARTCO ; D V RE INT RATIOE NELE ;
*FOX  D V RE INT BLAH1 ; D V RE INT BLAH2 ;
*FOX  D V RE INT SIGMDAC ;
*FOX  D V RE INT C5M4 ; D V RE INT C2E3 ; D V RE INT C1E6 ;
*FOX  D V RE INT C1E3 ; D V RE INT C1M3 ; D V RE INT C1M6 ;
*FOX  D V RE INT C1M9 ; D V RE INT C1M12 ; D V RE INT C1M15 ;
*FOX  D V RE INT C1M18 ; D V RE INT C1M21 ; D V RE INT C1M24 ;
*FOX  D V RE INT ONE ; D V RE INT TWO ; D V RE INT THREE ;
*FOX  D V RE INT FOUR ; D V RE INT ZERO ;
*FOX  D V IN INT IDZ 2 ; D V IN INT KX ; D V IN INT IX ; D V IN INT JX ;
*FOX  D V IN INT I ; D V IN INT IPCH ; D V IN INT K ; D V IN INT KKK ;
*FOX  D F RE DARE 1 ;
*FOX  E D ;
*FOX{
      INTEGER SIGMDA  
      INTEGER DPDA    
      INTEGER DPDA1   
      INTEGER RV      
      INTEGER EJ1     
      INTEGER EJF1    
      INTEGER ALDA    (2,6)
      INTEGER ASDA    (2,6)
      INTEGER ALDAQ   (2,6)
      INTEGER ASDAQ   (2,6)
      INTEGER SMIDA   (MCOR)
      INTEGER X       (2)
      INTEGER Y       (2)
      INTEGER YP      (2)
      INTEGER DKIP    
      INTEGER CORROLD (8)
      INTEGER CORRNEW (8)
      INTEGER CORRAU1 (8)
      INTEGER CORRAU2 (8)
      INTEGER AA      (11)
      INTEGER BB      (11)
      INTEGER PUX     
      INTEGER PUZ     
      INTEGER EJF0    
      INTEGER EKK     
      INTEGER XL      
      INTEGER ZL      
      INTEGER CRKVE   
      INTEGER CIKVE   
      INTEGER CRKVEUK 
      INTEGER CBZBF   
      INTEGER YV1J    
      INTEGER YV2J    
      INTEGER CRKVEBF 
      INTEGER CIKVEBF 
      INTEGER RHO2BF  
      INTEGER TKBF    
      INTEGER R2BF    
      INTEGER RBF     
      INTEGER RKBF    
      INTEGER XRBF    
      INTEGER ZRBF    
      INTEGER XBBF    
      INTEGER ZBBF    
      INTEGER CRXBF   
      INTEGER CBXBF   
      INTEGER CRZBF   
      INTEGER XX      
      INTEGER YY      
      INTEGER WX      
      INTEGER WY      
      INTEGER ISCRDA, ISCRRI,IDAO
C     INTEGER LFOX0, LFOX1
      REAL*8 RSCRRI
      COMMON/DASCR/ISCRDA(100),RSCRRI(100),ISCRRI(100),IDAO
      save
      if(idalloc.eq.1.or.idalloc.eq.3) then                           
         CALL DAKEY('FOX V2.1')
         CALL DAALL(SIGMDA  ,1,'SIGMDA    ',NORD,NVAR)
         CALL DAALL(DPDA    ,1,'DPDA      ',NORD,NVAR)
         CALL DAALL(DPDA1   ,1,'DPDA1     ',NORD,NVAR)
         CALL DAALL(RV      ,1,'RV        ',NORD,NVAR)
         CALL DAALL(EJ1     ,1,'EJ1       ',NORD,NVAR)
         CALL DAALL(EJF1    ,1,'EJF1      ',NORD,NVAR)
         CALL DAALL(ALDA    ,1*(2)*(6),'ALDA      ',NORD,NVAR)
         CALL DAALL(ASDA    ,1*(2)*(6),'ASDA      ',NORD,NVAR)
         CALL DAALL(ALDAQ   ,1*(2)*(6),'ALDAQ     ',NORD,NVAR)
         CALL DAALL(ASDAQ   ,1*(2)*(6),'ASDAQ     ',NORD,NVAR)
         CALL DAALL(SMIDA   ,1*(MCOR),'SMIDA     ',NORD,NVAR)
         CALL DAALL(X       ,1*(2),'X         ',NORD,NVAR)
         CALL DAALL(Y       ,1*(2),'Y         ',NORD,NVAR)
         CALL DAALL(YP      ,1*(2),'YP        ',NORD,NVAR)
         CALL DAALL(DKIP    ,1,'DKIP      ',NORD,NVAR)
         CALL DAALL(CORROLD ,1*(8),'CORROLD   ',NORD,NVAR)
         CALL DAALL(CORRNEW ,1*(8),'CORRNEW   ',NORD,NVAR)
         CALL DAALL(CORRAU1 ,1*(8),'CORRAU1   ',NORD,NVAR)
         CALL DAALL(CORRAU2 ,1*(8),'CORRAU2   ',NORD,NVAR)
         CALL DAALL(AA      ,1*(11),'AA        ',NORD,NVAR)
         CALL DAALL(BB      ,1*(11),'BB        ',NORD,NVAR)
         CALL DAALL(PUX     ,1,'PUX       ',NORD,NVAR)
         CALL DAALL(PUZ     ,1,'PUZ       ',NORD,NVAR)
         CALL DAALL(EJF0    ,1,'EJF0      ',NORD,NVAR)
         CALL DAALL(EKK     ,1,'EKK       ',NORD,NVAR)
         CALL DAALL(XL      ,1,'XL        ',NORD,NVAR)
         CALL DAALL(ZL      ,1,'ZL        ',NORD,NVAR)
         CALL DAALL(CRKVE   ,1,'CRKVE     ',NORD,NVAR)
         CALL DAALL(CIKVE   ,1,'CIKVE     ',NORD,NVAR)
         CALL DAALL(CRKVEUK ,1,'CRKVEUK   ',NORD,NVAR)
         CALL DAALL(CBZBF   ,1,'CBZBF     ',NORD,NVAR)
         CALL DAALL(YV1J    ,1,'YV1J      ',NORD,NVAR)
         CALL DAALL(YV2J    ,1,'YV2J      ',NORD,NVAR)
         CALL DAALL(CRKVEBF ,1,'CRKVEBF   ',NORD,NVAR)
         CALL DAALL(CIKVEBF ,1,'CIKVEBF   ',NORD,NVAR)
         CALL DAALL(RHO2BF  ,1,'RHO2BF    ',NORD,NVAR)
         CALL DAALL(TKBF    ,1,'TKBF      ',NORD,NVAR)
         CALL DAALL(R2BF    ,1,'R2BF      ',NORD,NVAR)
         CALL DAALL(RBF     ,1,'RBF       ',NORD,NVAR)
         CALL DAALL(RKBF    ,1,'RKBF      ',NORD,NVAR)
         CALL DAALL(XRBF    ,1,'XRBF      ',NORD,NVAR)
         CALL DAALL(ZRBF    ,1,'ZRBF      ',NORD,NVAR)
         CALL DAALL(XBBF    ,1,'XBBF      ',NORD,NVAR)
         CALL DAALL(ZBBF    ,1,'ZBBF      ',NORD,NVAR)
         CALL DAALL(CRXBF   ,1,'CRXBF     ',NORD,NVAR)
         CALL DAALL(CBXBF   ,1,'CBXBF     ',NORD,NVAR)
         CALL DAALL(CRZBF   ,1,'CRZBF     ',NORD,NVAR)
         CALL DAALL(XX      ,1,'XX        ',NORD,NVAR)
         CALL DAALL(YY      ,1,'YY        ',NORD,NVAR)
         CALL DAALL(WX      ,1,'WX        ',NORD,NVAR)
         CALL DAALL(WY      ,1,'WY        ',NORD,NVAR)
      ENDIF
      IDAA = IDAO
*FOX}
C-----------------------------------------------------------------------
      C5M4=5.0D-4
      nbeamo=nbeam
      if(nbeam.gt.0) nbeam=nbeam+1
      e0f=sqrt(e0*e0-pma*pma)
      betr0=sqrt(one-(pma/e0)**2)
      itra=1
      ox=xxtr(1,1)
      oxp=yytr(1,1)
      oz=xxtr(1,2)
      ozp=yytr(1,2)
      sigm1=sigm(1)
      if(abs(dppoff).gt.pieni) then
        dps1=dppoff
        clop6(3)=dppoff
      else
        dps1=dps(1)
      endif
      if(el(iq(1)).le.pieni) then
        ed1=ed(iq(1))
      else
        ed1=ek(iq(1))
      endif
      if(el(iq(2)).le.pieni) then
        ed2=ed(iq(2))
      else
        ed2=ek(iq(2))
      endif
      call davar(x(1),zero,1)
*FOX  X(1)=X(1)+OX ;                                                    *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*OX         ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),X          ((1)))                     
      call davar(yp(1),zero,2)
*FOX  YP(1)=YP(1)+OXP*(ONE+DPS1) ;                                      *FOX
      RSCRRI(  1+IDAA) = ONE         + DPS1                             
      CALL DACOP(YP         ((1)),ISCRDA(  2+IDAA))                     
      RSCRRI(  3+IDAA) = OXP         * RSCRRI(  1+IDAA)                 
      CALL DACAD(ISCRDA(  2+IDAA),ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  4+IDAA),YP         ((1)))                     
      call davar(x(2),zero,3)
*FOX  X(2)=X(2)+OZ ;                                                    *FOX
      CALL DACOP(X          ((2)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*OZ         ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),X          ((2)))                     
      call davar(yp(2),zero,4)
*FOX  YP(2)=YP(2)+OZP*(ONE+DPS1) ;                                      *FOX
      RSCRRI(  1+IDAA) = ONE         + DPS1                             
      CALL DACOP(YP         ((2)),ISCRDA(  2+IDAA))                     
      RSCRRI(  3+IDAA) = OZP         * RSCRRI(  1+IDAA)                 
      CALL DACAD(ISCRDA(  2+IDAA),ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  4+IDAA),YP         ((2)))                     
      call davar(sigmda,zero,5)
*FOX  SIGMDA=SIGMDA+SIGM1 ;                                             *FOX
      CALL DACAD(SIGMDA     ,ONE*SIGM1      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),SIGMDA     )                          
      call davar(dpda1,zero,6)
*FOX  DPDA1=DPDA1+DPS1*C1E3 ;                                           *FOX
      RSCRRI(  1+IDAA) = DPS1        * C1E3                             
      CALL DACAD(DPDA1      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),DPDA1      )                          
*FOX  DPDA=DPDA1*C1M3 ;                                                 *FOX
      CALL DACMU(DPDA1      ,ONE*C1M3       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),DPDA       )                          
      if(iqmod6.eq.1) then
        call davar(smida(1),zero,7)
*FOX  SMIDA(1)=SMIDA(1)+ED1 ;                                           *FOX
      CALL DACOP(SMIDA      ((1)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*ED1        ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),SMIDA      ((1)))                     
        call davar(smida(2),zero,8)
*FOX  SMIDA(2)=SMIDA(2)+ED2 ;                                           *FOX
      CALL DACOP(SMIDA      ((2)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*ED2        ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),SMIDA      ((2)))                     
      endif
*FOX  CORROLD(1)=X(1) ;                                                 *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORROLD    ((1)))                     
*FOX  CORROLD(2)=YP(1) ;                                                *FOX
      CALL DACOP(YP         ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORROLD    ((2)))                     
*FOX  CORROLD(3)=X(2) ;                                                 *FOX
      CALL DACOP(X          ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORROLD    ((3)))                     
*FOX  CORROLD(4)=YP(2) ;                                                *FOX
      CALL DACOP(YP         ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORROLD    ((4)))                     
*FOX  CORROLD(5)=SIGMDA ;                                               *FOX
      CALL DACOP(SIGMDA     ,CORROLD    ((5)))                          
*FOX  CORROLD(6)=DPDA1 ;                                                *FOX
      CALL DACOP(DPDA1      ,CORROLD    ((6)))                          
            DO 5 KKK=1,6
              DPDAV=DARE(CORROLD(KKK))                                         *FOX
*FOX  CORROLD(KKK)=CORROLD(KKK)-DPDAV ;                                 *FOX
      CALL DACOP(CORROLD    (KKK        ),ISCRDA(  1+IDAA))             
      CALL DACSU(ISCRDA(  1+IDAA),ONE*DPDAV      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CORROLD    (KKK        ))             
    5       CONTINUE
*FOX  Y(1)=YP(1)/(ONE+DPDA) ;                                           *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(YP         ((1)),ISCRDA(  2+IDAA))                     
      CALL DADIV(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),Y          ((1)))                     
*FOX  Y(2)=YP(2)/(ONE+DPDA) ;                                           *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(YP         ((2)),ISCRDA(  2+IDAA))                     
      CALL DADIV(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),Y          ((2)))                     
      iflag=0
*FOX  EJF1=E0F*(ONE+DPDA) ;                                             *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACMU(ISCRDA(  1+IDAA),ONE*E0F        ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),EJF1       )                          
*FOX  EJ1=SQRT(EJF1*EJF1+PMA*PMA) ;                                     *FOX
      CALL DAMUL(EJF1       ,EJF1       ,ISCRDA(  1+IDAA))              
      RSCRRI(  2+IDAA) = PMA         * PMA                              
      CALL DACAD(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DAFUN('SQRT  ',ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))            
      CALL DACOP(ISCRDA(  4+IDAA),EJ1        )                          
*FOX  RV=EJ1/E0*E0F/EJF1 ;                                              *FOX
      CALL DACDI(EJ1        ,ONE*E0         ,ISCRDA(  1+IDAA))          
      CALL DACMU(ISCRDA(  1+IDAA),ONE*E0F        ,ISCRDA(  2+IDAA))     
      CALL DADIV(ISCRDA(  2+IDAA),EJF1       ,ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),RV         )                          
      if(ithick.eq.1) call envada(numl-1)
      icav=0
      do 430 i=1,iu
        if(iflag.eq.1) then
*FOX  EJF1=E0F*(ONE+DPDA) ;                                             *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACMU(ISCRDA(  1+IDAA),ONE*E0F        ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),EJF1       )                          
*FOX  EJ1=SQRT(EJF1*EJF1+PMA*PMA) ;                                     *FOX
      CALL DAMUL(EJF1       ,EJF1       ,ISCRDA(  1+IDAA))              
      RSCRRI(  2+IDAA) = PMA         * PMA                              
      CALL DACAD(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DAFUN('SQRT  ',ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))            
      CALL DACOP(ISCRDA(  4+IDAA),EJ1        )                          
*FOX  RV=EJ1/E0*E0F/EJF1 ;                                              *FOX
      CALL DACDI(EJ1        ,ONE*E0         ,ISCRDA(  1+IDAA))          
      CALL DACMU(ISCRDA(  1+IDAA),ONE*E0F        ,ISCRDA(  2+IDAA))     
      CALL DADIV(ISCRDA(  2+IDAA),EJF1       ,ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),RV         )                          
          if(ithick.eq.1) then
*FOX  YP(1)=Y(1)*(ONE+DPDA) ;                                           *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(Y          ((1)),ISCRDA(  2+IDAA))                     
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),YP         ((1)))                     
*FOX  YP(2)=Y(2)*(ONE+DPDA) ;                                           *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(Y          ((2)),ISCRDA(  2+IDAA))                     
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),YP         ((2)))                     
            if(icav.eq.0) then
*FOX  CORRNEW(1)=X(1) ;                                                 *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRNEW    ((1)))                     
*FOX  CORRNEW(2)=YP(1) ;                                                *FOX
      CALL DACOP(YP         ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRNEW    ((2)))                     
*FOX  CORRNEW(3)=X(2) ;                                                 *FOX
      CALL DACOP(X          ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRNEW    ((3)))                     
*FOX  CORRNEW(4)=YP(2) ;                                                *FOX
      CALL DACOP(YP         ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRNEW    ((4)))                     
*FOX  CORRNEW(5)=SIGMDA ;                                               *FOX
      CALL DACOP(SIGMDA     ,CORRNEW    ((5)))                          
*FOX  CORRNEW(6)=DPDA1 ;                                                *FOX
      CALL DACOP(DPDA1      ,CORRNEW    ((6)))                          
              DO 24 KKK=1,6
                DPDAV=DARE(CORRNEW(KKK))                                       *FOX
*FOX  CORRNEW(KKK)=CORRNEW(KKK)-DPDAV ;                                 *FOX
      CALL DACOP(CORRNEW    (KKK        ),ISCRDA(  1+IDAA))             
      CALL DACSU(ISCRDA(  1+IDAA),ONE*DPDAV      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CORRNEW    (KKK        ))             
   24         CONTINUE  
            else
*FOX  CORRAU2(1)=X(1) ;                                                 *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU2    ((1)))                     
*FOX  CORRAU2(2)=YP(1) ;                                                *FOX
      CALL DACOP(YP         ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU2    ((2)))                     
*FOX  CORRAU2(3)=X(2) ;                                                 *FOX
      CALL DACOP(X          ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU2    ((3)))                     
*FOX  CORRAU2(4)=YP(2) ;                                                *FOX
      CALL DACOP(YP         ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU2    ((4)))                     
*FOX  CORRAU2(5)=SIGMDA ;                                               *FOX
      CALL DACOP(SIGMDA     ,CORRAU2    ((5)))                          
*FOX  CORRAU2(6)=DPDA1 ;                                                *FOX
      CALL DACOP(DPDA1      ,CORRAU2    ((6)))                          
              DO 25 KKK=1,6
*FOX  CORRAU1(KKK)=CORRNEW(KKK) ;                                       *FOX
      CALL DACOP(CORRNEW    (KKK        ),ISCRDA(  1+IDAA))             
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU1    (KKK        ))             
                DPDAV=DARE(CORRAU2(KKK))                                       *FOX
*FOX  CORRAU2(KKK)=CORRAU2(KKK)-DPDAV ;                                 *FOX
      CALL DACOP(CORRAU2    (KKK        ),ISCRDA(  1+IDAA))             
      CALL DACSU(ISCRDA(  1+IDAA),ONE*DPDAV      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CORRAU2    (KKK        ))             
   25         CONTINUE  
        if(nvar.eq.8) then
*FOX  CORRAU2(7)=SMIDA(1) ;                                             *FOX
      CALL DACOP(SMIDA      ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU2    ((7)))                     
*FOX  CORRAU2(8)=SMIDA(2) ;                                             *FOX
      CALL DACOP(SMIDA      ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU2    ((8)))                     
          blah1=dare(smida(1))
          blah2=dare(smida(2))
*FOX  CORRAU1(7)=SMIDA(1)-BLAH1 ;                                       *FOX
      CALL DACOP(SMIDA      ((1)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*BLAH1      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CORRAU1    ((7)))                     
*FOX  CORRAU1(8)=SMIDA(2)-BLAH2 ;                                       *FOX
      CALL DACOP(SMIDA      ((2)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*BLAH2      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CORRAU1    ((8)))                     
        endif
              call dacct(corrau2,nvar,corrau1,nvar,corrnew,nvar)
            endif
            dpdav=dare(x(1))
*FOX  X(1)=CORROLD(1)+DPDAV ;                                           *FOX
      CALL DACOP(CORROLD    ((1)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*DPDAV      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),X          ((1)))                     
            dpdav=dare(yp(1))
*FOX  YP(1)=CORROLD(2)+DPDAV ;                                          *FOX
      CALL DACOP(CORROLD    ((2)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*DPDAV      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),YP         ((1)))                     
            dpdav=dare(x(2))
*FOX  X(2)=CORROLD(3)+DPDAV ;                                           *FOX
      CALL DACOP(CORROLD    ((3)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*DPDAV      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),X          ((2)))                     
            dpdav=dare(yp(2))
*FOX  YP(2)=CORROLD(4)+DPDAV ;                                          *FOX
      CALL DACOP(CORROLD    ((4)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*DPDAV      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),YP         ((2)))                     
            dpdav=dare(sigmda)
*FOX  SIGMDA=CORROLD(5)+DPDAV ;                                         *FOX
      CALL DACOP(CORROLD    ((5)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*DPDAV      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),SIGMDA     )                          
            dpdav=dare(dpda1)
*FOX  DPDA1=CORROLD(6)+DPDAV ;                                          *FOX
      CALL DACOP(CORROLD    ((6)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*DPDAV      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),DPDA1      )                          
*FOX  DPDA=DPDA1*C1M3 ;                                                 *FOX
      CALL DACMU(DPDA1      ,ONE*C1M3       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),DPDA       )                          
*FOX  Y(1)=YP(1)/(ONE+DPDA) ;                                           *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(YP         ((1)),ISCRDA(  2+IDAA))                     
      CALL DADIV(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),Y          ((1)))                     
*FOX  Y(2)=YP(2)/(ONE+DPDA) ;                                           *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(YP         ((2)),ISCRDA(  2+IDAA))                     
      CALL DADIV(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),Y          ((2)))                     
*FOX  EJF1=E0F*(ONE+DPDA) ;                                             *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACMU(ISCRDA(  1+IDAA),ONE*E0F        ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),EJF1       )                          
*FOX  EJ1=SQRT(EJF1*EJF1+PMA*PMA) ;                                     *FOX
      CALL DAMUL(EJF1       ,EJF1       ,ISCRDA(  1+IDAA))              
      RSCRRI(  2+IDAA) = PMA         * PMA                              
      CALL DACAD(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DAFUN('SQRT  ',ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))            
      CALL DACOP(ISCRDA(  4+IDAA),EJ1        )                          
*FOX  RV=EJ1/E0*E0F/EJF1 ;                                              *FOX
      CALL DACDI(EJ1        ,ONE*E0         ,ISCRDA(  1+IDAA))          
      CALL DACMU(ISCRDA(  1+IDAA),ONE*E0F        ,ISCRDA(  2+IDAA))     
      CALL DADIV(ISCRDA(  2+IDAA),EJF1       ,ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),RV         )                          
            icav=icav+1
            call envada(numl*icav)
          endif
          iflag=0
        endif
        ix=ic(i)
        if(ix.gt.nblo) goto 50
        if(ix.le.0) then
          call error(93)
        endif
        jmel=mel(ix)
        do 40 kx=1,2
          if(ithick.eq.1) then
*FOX  PUX=X(KX) ;                                                       *FOX
      CALL DACOP(X          (KX         ),ISCRDA(  1+IDAA))             
      CALL DACOP(ISCRDA(  1+IDAA),PUX        )                          
*FOX  PUZ=Y(KX) ;                                                       *FOX
      CALL DACOP(Y          (KX         ),ISCRDA(  1+IDAA))             
      CALL DACOP(ISCRDA(  1+IDAA),PUZ        )                          
          endif
          if(idp.eq.0.or.ition.eq.0) then
            if(ithick.eq.1) then
*FOX  X(KX)=BL1(IX,KX,1)*PUX+BL1(IX,KX,2)*PUZ+                          *FOX
*FOX  BL1(IX,KX,5)*IDZ(KX)*DPDA*C1E3 ;                                  *FOX
      RSCRRI(  1+IDAA) = BL1        (IX         ,KX         ,(1))       
      RSCRRI(  2+IDAA) = BL1        (IX         ,KX         ,(2))       
      RSCRRI(  3+IDAA) = BL1        (IX         ,KX         ,(5))       
      ISCRRI(  4+IDAA) = IDZ        (KX         )                       
      CALL DACMU(PUX        ,ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))     
      CALL DACMU(PUZ        ,ONE*RSCRRI(  2+IDAA),ISCRDA(  6+IDAA))     
      RSCRRI(  7+IDAA) = RSCRRI(  3+IDAA) * ISCRRI(  4+IDAA)            
      CALL DACMU(DPDA       ,ONE*RSCRRI(  7+IDAA),ISCRDA(  8+IDAA))     
      CALL DACMU(ISCRDA(  8+IDAA),ONE*C1E3       ,ISCRDA(  9+IDAA))     
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA( 10+IDAA))    
      CALL DAADD(ISCRDA( 10+IDAA),ISCRDA(  9+IDAA),ISCRDA( 11+IDAA))    
      CALL DACOP(ISCRDA( 11+IDAA),X          (KX         ))             
*FOX  Y(KX)=BL1(IX,KX,3)*PUX+BL1(IX,KX,4)*PUZ+                          *FOX
*FOX  BL1(IX,KX,6)*IDZ(KX)*DPDA*C1E3 ;                                  *FOX
      RSCRRI(  1+IDAA) = BL1        (IX         ,KX         ,(3))       
      RSCRRI(  2+IDAA) = BL1        (IX         ,KX         ,(4))       
      RSCRRI(  3+IDAA) = BL1        (IX         ,KX         ,(6))       
      ISCRRI(  4+IDAA) = IDZ        (KX         )                       
      CALL DACMU(PUX        ,ONE*RSCRRI(  1+IDAA),ISCRDA(  5+IDAA))     
      CALL DACMU(PUZ        ,ONE*RSCRRI(  2+IDAA),ISCRDA(  6+IDAA))     
      RSCRRI(  7+IDAA) = RSCRRI(  3+IDAA) * ISCRRI(  4+IDAA)            
      CALL DACMU(DPDA       ,ONE*RSCRRI(  7+IDAA),ISCRDA(  8+IDAA))     
      CALL DACMU(ISCRDA(  8+IDAA),ONE*C1E3       ,ISCRDA(  9+IDAA))     
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA( 10+IDAA))    
      CALL DAADD(ISCRDA( 10+IDAA),ISCRDA(  9+IDAA),ISCRDA( 11+IDAA))    
      CALL DACOP(ISCRDA( 11+IDAA),Y          (KX         ))             
            else
*FOX  X(KX)=X(KX)+BL1(IX,KX,2)*Y(KX) ;                                  *FOX
      CALL DACOP(X          (KX         ),ISCRDA(  1+IDAA))             
      RSCRRI(  2+IDAA) = BL1        (IX         ,KX         ,(2))       
      CALL DACOP(Y          (KX         ),ISCRDA(  3+IDAA))             
      CALL DACMU(ISCRDA(  3+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(ISCRDA(  5+IDAA),X          (KX         ))             
            endif
          else
            do 30 jb=1,jmel
              jx=mtyp(ix,jb)
              if(ithick.eq.1) then
                do 20 ip=1,6
                  do 10 ien=1,nord+1
                    zfeld1(ien)=ald6(jx,kx,ip,ien)
                    zfeld2(ien)=asd6(jx,kx,ip,ien)
   10             continue
                  if(nvar2.eq.5) then
                    call darea6(alda(kx,ip),zfeld1,5)
                    call darea6(asda(kx,ip),zfeld2,5)
                  else if(nvar2.eq.6) then
                    call darea6(alda(kx,ip),zfeld1,6)
                    call darea6(asda(kx,ip),zfeld2,6)
                  endif
   20           continue
                ipch=0
                if(iqmod6.eq.1.and.kz(jx).eq.2) then
                  if(jx.eq.iq(1).or.iratioe(jx).eq.iq(1)) then
                    ipch=1
                  else if(jx.eq.iq(2).or.iratioe(jx).eq.iq(2)) then
                    ipch=2
                  endif
                endif
                if(ipch.ne.0) then
                  call envquad(jx,ipch)
*FOX  SIGMDA=SIGMDA+ASDAQ(KX,1)+ASDAQ(KX,2)*PUX+                        *FOX
*FOX  ASDAQ(KX,3)*PUZ+ASDAQ(KX,4)*PUX*PUZ+ASDAQ(KX,5)*PUX*PUX+          *FOX
*FOX  ASDAQ(KX,6)*PUZ*PUZ ;                                             *FOX
      CALL DACOP(ASDAQ      (KX         ,(1)),ISCRDA(  1+IDAA))         
      CALL DACOP(ASDAQ      (KX         ,(2)),ISCRDA(  2+IDAA))         
      CALL DACOP(ASDAQ      (KX         ,(3)),ISCRDA(  3+IDAA))         
      CALL DACOP(ASDAQ      (KX         ,(4)),ISCRDA(  4+IDAA))         
      CALL DACOP(ASDAQ      (KX         ,(5)),ISCRDA(  5+IDAA))         
      CALL DACOP(ASDAQ      (KX         ,(6)),ISCRDA(  6+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),PUX        ,ISCRDA(  7+IDAA))         
      CALL DAMUL(ISCRDA(  3+IDAA),PUZ        ,ISCRDA(  8+IDAA))         
      CALL DAMUL(ISCRDA(  4+IDAA),PUX        ,ISCRDA(  9+IDAA))         
      CALL DAMUL(ISCRDA(  9+IDAA),PUZ        ,ISCRDA( 10+IDAA))         
      CALL DAMUL(ISCRDA(  5+IDAA),PUX        ,ISCRDA( 11+IDAA))         
      CALL DAMUL(ISCRDA( 11+IDAA),PUX        ,ISCRDA( 12+IDAA))         
      CALL DAMUL(ISCRDA(  6+IDAA),PUZ        ,ISCRDA( 13+IDAA))         
      CALL DAMUL(ISCRDA( 13+IDAA),PUZ        ,ISCRDA( 14+IDAA))         
      CALL DAADD(SIGMDA     ,ISCRDA(  1+IDAA),ISCRDA( 15+IDAA))         
      CALL DAADD(ISCRDA( 15+IDAA),ISCRDA(  7+IDAA),ISCRDA( 16+IDAA))    
      CALL DAADD(ISCRDA( 16+IDAA),ISCRDA(  8+IDAA),ISCRDA( 17+IDAA))    
      CALL DAADD(ISCRDA( 17+IDAA),ISCRDA( 10+IDAA),ISCRDA( 18+IDAA))    
      CALL DAADD(ISCRDA( 18+IDAA),ISCRDA( 12+IDAA),ISCRDA( 19+IDAA))    
      CALL DAADD(ISCRDA( 19+IDAA),ISCRDA( 14+IDAA),ISCRDA( 20+IDAA))    
      CALL DACOP(ISCRDA( 20+IDAA),SIGMDA     )                          
*FOX  X(KX)=ALDAQ(KX,1)*PUX+ALDAQ(KX,2)*PUZ+ALDAQ(KX,5)*IDZ(KX) ;       *FOX
      CALL DACOP(ALDAQ      (KX         ,(1)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDAQ      (KX         ,(2)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDAQ      (KX         ,(5)),ISCRDA(  3+IDAA))         
      ISCRRI(  4+IDAA) = IDZ        (KX         )                       
      CALL DAMUL(ISCRDA(  1+IDAA),PUX        ,ISCRDA(  5+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),PUZ        ,ISCRDA(  6+IDAA))         
      CALL DACMU(ISCRDA(  3+IDAA),ONE*ISCRRI(  4+IDAA),ISCRDA(  7+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DAADD(ISCRDA(  8+IDAA),ISCRDA(  7+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),X          (KX         ))             
*FOX  Y(KX)=ALDAQ(KX,3)*PUX+ALDAQ(KX,4)*PUZ+ALDAQ(KX,6)*IDZ(KX) ;       *FOX
      CALL DACOP(ALDAQ      (KX         ,(3)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDAQ      (KX         ,(4)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDAQ      (KX         ,(6)),ISCRDA(  3+IDAA))         
      ISCRRI(  4+IDAA) = IDZ        (KX         )                       
      CALL DAMUL(ISCRDA(  1+IDAA),PUX        ,ISCRDA(  5+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),PUZ        ,ISCRDA(  6+IDAA))         
      CALL DACMU(ISCRDA(  3+IDAA),ONE*ISCRRI(  4+IDAA),ISCRDA(  7+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DAADD(ISCRDA(  8+IDAA),ISCRDA(  7+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          (KX         ))             
                else
*FOX  SIGMDA=SIGMDA+ASDA(KX,1)+ASDA(KX,2)*PUX+                          *FOX
*FOX  ASDA(KX,3)*PUZ+ASDA(KX,4)*PUX*PUZ+ASDA(KX,5)*PUX*PUX+             *FOX
*FOX  ASDA(KX,6)*PUZ*PUZ ;                                              *FOX
      CALL DACOP(ASDA       (KX         ,(1)),ISCRDA(  1+IDAA))         
      CALL DACOP(ASDA       (KX         ,(2)),ISCRDA(  2+IDAA))         
      CALL DACOP(ASDA       (KX         ,(3)),ISCRDA(  3+IDAA))         
      CALL DACOP(ASDA       (KX         ,(4)),ISCRDA(  4+IDAA))         
      CALL DACOP(ASDA       (KX         ,(5)),ISCRDA(  5+IDAA))         
      CALL DACOP(ASDA       (KX         ,(6)),ISCRDA(  6+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),PUX        ,ISCRDA(  7+IDAA))         
      CALL DAMUL(ISCRDA(  3+IDAA),PUZ        ,ISCRDA(  8+IDAA))         
      CALL DAMUL(ISCRDA(  4+IDAA),PUX        ,ISCRDA(  9+IDAA))         
      CALL DAMUL(ISCRDA(  9+IDAA),PUZ        ,ISCRDA( 10+IDAA))         
      CALL DAMUL(ISCRDA(  5+IDAA),PUX        ,ISCRDA( 11+IDAA))         
      CALL DAMUL(ISCRDA( 11+IDAA),PUX        ,ISCRDA( 12+IDAA))         
      CALL DAMUL(ISCRDA(  6+IDAA),PUZ        ,ISCRDA( 13+IDAA))         
      CALL DAMUL(ISCRDA( 13+IDAA),PUZ        ,ISCRDA( 14+IDAA))         
      CALL DAADD(SIGMDA     ,ISCRDA(  1+IDAA),ISCRDA( 15+IDAA))         
      CALL DAADD(ISCRDA( 15+IDAA),ISCRDA(  7+IDAA),ISCRDA( 16+IDAA))    
      CALL DAADD(ISCRDA( 16+IDAA),ISCRDA(  8+IDAA),ISCRDA( 17+IDAA))    
      CALL DAADD(ISCRDA( 17+IDAA),ISCRDA( 10+IDAA),ISCRDA( 18+IDAA))    
      CALL DAADD(ISCRDA( 18+IDAA),ISCRDA( 12+IDAA),ISCRDA( 19+IDAA))    
      CALL DAADD(ISCRDA( 19+IDAA),ISCRDA( 14+IDAA),ISCRDA( 20+IDAA))    
      CALL DACOP(ISCRDA( 20+IDAA),SIGMDA     )                          
*FOX  X(KX)=ALDA(KX,1)*PUX+ALDA(KX,2)*PUZ+ALDA(KX,5)*IDZ(KX) ;          *FOX
      CALL DACOP(ALDA       (KX         ,(1)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDA       (KX         ,(2)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (KX         ,(5)),ISCRDA(  3+IDAA))         
      ISCRRI(  4+IDAA) = IDZ        (KX         )                       
      CALL DAMUL(ISCRDA(  1+IDAA),PUX        ,ISCRDA(  5+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),PUZ        ,ISCRDA(  6+IDAA))         
      CALL DACMU(ISCRDA(  3+IDAA),ONE*ISCRRI(  4+IDAA),ISCRDA(  7+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DAADD(ISCRDA(  8+IDAA),ISCRDA(  7+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),X          (KX         ))             
*FOX  Y(KX)=ALDA(KX,3)*PUX+ALDA(KX,4)*PUZ+ALDA(KX,6)*IDZ(KX) ;          *FOX
      CALL DACOP(ALDA       (KX         ,(3)),ISCRDA(  1+IDAA))         
      CALL DACOP(ALDA       (KX         ,(4)),ISCRDA(  2+IDAA))         
      CALL DACOP(ALDA       (KX         ,(6)),ISCRDA(  3+IDAA))         
      ISCRRI(  4+IDAA) = IDZ        (KX         )                       
      CALL DAMUL(ISCRDA(  1+IDAA),PUX        ,ISCRDA(  5+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),PUZ        ,ISCRDA(  6+IDAA))         
      CALL DACMU(ISCRDA(  3+IDAA),ONE*ISCRRI(  4+IDAA),ISCRDA(  7+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DAADD(ISCRDA(  8+IDAA),ISCRDA(  7+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          (KX         ))             
                endif
*FOX  PUX=X(KX) ;                                                       *FOX
      CALL DACOP(X          (KX         ),ISCRDA(  1+IDAA))             
      CALL DACOP(ISCRDA(  1+IDAA),PUX        )                          
*FOX  PUZ=Y(KX) ;                                                       *FOX
      CALL DACOP(Y          (KX         ),ISCRDA(  1+IDAA))             
      CALL DACOP(ISCRDA(  1+IDAA),PUZ        )                          
              else
*FOX  X(KX)=X(KX)+EL(JX)*Y(KX) ;                                        *FOX
      CALL DACOP(X          (KX         ),ISCRDA(  1+IDAA))             
      RSCRRI(  2+IDAA) = EL         (JX         )                       
      CALL DACOP(Y          (KX         ),ISCRDA(  3+IDAA))             
      CALL DACMU(ISCRDA(  3+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(ISCRDA(  5+IDAA),X          (KX         ))             
                if (kx.eq.1) then
*FOX  SIGMDA=SIGMDA+                                                    *FOX
*FOX  EL(JX)*(C1E3-RV*(C1E3+(Y(1)*Y(1)+Y(2)*Y(2))*C5M4)) ;              *FOX
      CALL DACOP(Y          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(Y          ((1)),ISCRDA(  2+IDAA))                     
      CALL DACOP(Y          ((2)),ISCRDA(  3+IDAA))                     
      CALL DACOP(Y          ((2)),ISCRDA(  4+IDAA))                     
      CALL DAMUL(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DAMUL(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  6+IDAA))    
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA(  7+IDAA))    
      CALL DACMU(ISCRDA(  7+IDAA),ONE*C5M4       ,ISCRDA(  8+IDAA))     
      CALL DACAD(ISCRDA(  8+IDAA),ONE*C1E3       ,ISCRDA(  9+IDAA))     
      CALL DAMUL(RV         ,ISCRDA(  9+IDAA),ISCRDA( 10+IDAA))         
      CALL DASUC(ISCRDA( 10+IDAA),ONE*C1E3       ,ISCRDA( 11+IDAA))     
      RSCRRI( 12+IDAA) = EL         (JX         )                       
      CALL DACMU(ISCRDA( 11+IDAA),ONE*RSCRRI( 12+IDAA),ISCRDA( 13+IDAA))
     *                                                                  
      CALL DAADD(SIGMDA     ,ISCRDA( 13+IDAA),ISCRDA( 14+IDAA))         
      CALL DACOP(ISCRDA( 14+IDAA),SIGMDA     )                          
                endif
              endif
   30       continue
       
          endif
   40   continue
        goto 430
   50   ix=ix-nblo
        do 60 j=1,itra
          if(abs(dare(x(1))).gt.aint(aper(1)) .or.dare(x(1)).ne.dare
     +    (x(1))) goto 470
          if(abs(dare(x(2))).gt.aint(aper(2)) .or.dare(x(2)).ne.dare
     +    (x(2))) goto 470
   60   continue
        kpz=abs(kp(ix))
        if(kpz.eq.0) goto 80
        goto(80,80,80,80,80,70),kpz
   70   continue
        if(ition.ne.0) then
*FOX  EJF0=EJF1 ;                                                       *FOX
      CALL DACOP(EJF1       ,EJF0       )                               
          ixcav=ix
          if(abs(dppoff).gt.pieni) then
            sigmdac=dare(sigmda)
*FOX  SIGMDA=SIGMDA-SIGMDAC ;                                           *FOX
      CALL DACSU(SIGMDA     ,ONE*SIGMDAC    ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),SIGMDA     )                          
            sigmoff(i)=sigmdac
          endif
          call synoda
*FOX  DPDA=DPDA1*C1M3 ;                                                 *FOX
      CALL DACMU(DPDA1      ,ONE*C1M3       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),DPDA       )                          
*FOX  Y(1)=EJF0/EJF1*Y(1) ;                                             *FOX
      CALL DACOP(Y          ((1)),ISCRDA(  1+IDAA))                     
      CALL DADIV(EJF0       ,EJF1       ,ISCRDA(  2+IDAA))              
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),Y          ((1)))                     
*FOX  Y(2)=EJF0/EJF1*Y(2) ;                                             *FOX
      CALL DACOP(Y          ((2)),ISCRDA(  1+IDAA))                     
      CALL DADIV(EJF0       ,EJF1       ,ISCRDA(  2+IDAA))              
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),Y          ((2)))                     
          iflag=1
        endif
        goto 430
   80   kzz=kz(ix)
        if(kzz.eq.20) then
          if(nbeam.ge.1) then
            clobaux(1,ix)=dare(x(1))
            clobaux(2,ix)=dare(x(2))
            if(sigman6(1,ix).eq.sigman6(2,ix)) then
      BEAMOFF1=ZERO
      BEAMOFF2=ZERO
              if(ibeco.eq.1) then
            rho2b=ed(ix)*ed(ix)+ek(ix)*ek(ix)
            if(rho2b.gt.pieni)
     +          then
            if(abs(sigman6(1,ix)).lt.pieni) call error(88)      
            tkb=rho2b/(two*sigman6(1,ix)*sigman6(1,ix))
            beamoff1=two*crad*partnum*gammar*c1e6*ed(ix)/rho2b*
     &      (one-exp(-tkb))
            beamoff2=two*crad*partnum*gammar*c1e6*ek(ix)/rho2b*
     &      (one-exp(-tkb))
                endif
              endif
            startco=dare(x(1))-ed(ix)
*FOX  CRKVEBF=X(1)-STARTCO ;                                            *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*STARTCO    ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CRKVEBF    )                          
            startco=dare(x(2))-ek(ix)
*FOX  CIKVEBF=X(2)-STARTCO ;                                            *FOX
      CALL DACOP(X          ((2)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*STARTCO    ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CIKVEBF    )                          
*FOX  RHO2BF=CRKVEBF*CRKVEBF+CIKVEBF*CIKVEBF ;                          *FOX
      CALL DAMUL(CRKVEBF    ,CRKVEBF    ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVEBF    ,CIKVEBF    ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),RHO2BF     )                          
              if(abs(dare(rho2bf)).gt.pieni) then
      if(abs(sigman6(1,ix)).lt.pieni) call error(88)    
*FOX  TKBF=RHO2BF/(TWO*SIGMAN6(1,IX)*SIGMAN6(1,IX)) ;                   *FOX
      RSCRRI(  1+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  2+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  3+IDAA) = TWO         * RSCRRI(  1+IDAA)                 
      RSCRRI(  4+IDAA) = RSCRRI(  3+IDAA) * RSCRRI(  2+IDAA)            
      CALL DACDI(RHO2BF     ,ONE*RSCRRI(  4+IDAA),ISCRDA(  5+IDAA))     
      CALL DACOP(ISCRDA(  5+IDAA),TKBF       )                          
*FOX   Y(1)=Y(1)+(TWO*CRAD*PARTNUM*GAMMAR*C1E6*CRKVEBF/RHO2BF*          *FOX
*FOX  (ONE-EXP(-TKBF))-BEAMOFF1)/(ONE+DPDA) ;                           *FOX
      CALL DACMU(TKBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAFUN('EXP   ',ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))            
      CALL DASUC(ISCRDA(  2+IDAA),ONE*ONE        ,ISCRDA(  3+IDAA))     
      RSCRRI(  4+IDAA) = TWO         * CRAD                             
      RSCRRI(  5+IDAA) = RSCRRI(  4+IDAA) * PARTNUM                     
      RSCRRI(  6+IDAA) = RSCRRI(  5+IDAA) * GAMMAR                      
      RSCRRI(  7+IDAA) = RSCRRI(  6+IDAA) * C1E6                        
      CALL DACMU(CRKVEBF    ,ONE*RSCRRI(  7+IDAA),ISCRDA(  8+IDAA))     
      CALL DADIV(ISCRDA(  8+IDAA),RHO2BF     ,ISCRDA(  9+IDAA))         
      CALL DAMUL(ISCRDA(  9+IDAA),ISCRDA(  3+IDAA),ISCRDA( 10+IDAA))    
      CALL DACSU(ISCRDA( 10+IDAA),ONE*BEAMOFF1   ,ISCRDA( 11+IDAA))     
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA( 12+IDAA))          
      CALL DACOP(Y          ((1)),ISCRDA( 13+IDAA))                     
      CALL DADIV(ISCRDA( 11+IDAA),ISCRDA( 12+IDAA),ISCRDA( 14+IDAA))    
      CALL DAADD(ISCRDA( 13+IDAA),ISCRDA( 14+IDAA),ISCRDA( 15+IDAA))    
      CALL DACOP(ISCRDA( 15+IDAA),Y          ((1)))                     
*FOX   Y(2)=Y(2)+(TWO*CRAD*PARTNUM*GAMMAR*C1E6*CIKVEBF/RHO2BF*          *FOX
*FOX  (ONE-EXP(-TKBF))-BEAMOFF2)/(ONE+DPDA) ;                           *FOX
      CALL DACMU(TKBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAFUN('EXP   ',ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))            
      CALL DASUC(ISCRDA(  2+IDAA),ONE*ONE        ,ISCRDA(  3+IDAA))     
      RSCRRI(  4+IDAA) = TWO         * CRAD                             
      RSCRRI(  5+IDAA) = RSCRRI(  4+IDAA) * PARTNUM                     
      RSCRRI(  6+IDAA) = RSCRRI(  5+IDAA) * GAMMAR                      
      RSCRRI(  7+IDAA) = RSCRRI(  6+IDAA) * C1E6                        
      CALL DACMU(CIKVEBF    ,ONE*RSCRRI(  7+IDAA),ISCRDA(  8+IDAA))     
      CALL DADIV(ISCRDA(  8+IDAA),RHO2BF     ,ISCRDA(  9+IDAA))         
      CALL DAMUL(ISCRDA(  9+IDAA),ISCRDA(  3+IDAA),ISCRDA( 10+IDAA))    
      CALL DACSU(ISCRDA( 10+IDAA),ONE*BEAMOFF2   ,ISCRDA( 11+IDAA))     
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA( 12+IDAA))          
      CALL DACOP(Y          ((2)),ISCRDA( 13+IDAA))                     
      CALL DADIV(ISCRDA( 11+IDAA),ISCRDA( 12+IDAA),ISCRDA( 14+IDAA))    
      CALL DAADD(ISCRDA( 13+IDAA),ISCRDA( 14+IDAA),ISCRDA( 15+IDAA))    
      CALL DACOP(ISCRDA( 15+IDAA),Y          ((2)))                     
              endif
            else if(sigman6(1,ix).gt.sigman6(2,ix)) then
      BEAMOFF1=ZERO
      BEAMOFF2=ZERO
              if(ibeco.eq.1) then
            if(abs(sigman6(1,ix)).lt.pieni.or.
     &         abs(sigman6(2,ix)).lt.pieni) call error(88)      
            r2b=two*(sigman6(1,ix)*sigman6(1,ix)-
     &      sigman6(2,ix)*sigman6(2,ix))
            rb=sqrt(r2b)
            rkb=two*crad*partnum*gammar*c1e6*pisqrt/rb
            xrb=abs(ed(ix))/rb
            zrb=abs(ek(ix))/rb
            call errf(xrb,zrb,crxb,crzb)
c            if(ibtyp.eq.0) call errf(xrb,zrb,crxb,crzb)
c            if(ibtyp.eq.1) call wzsub(xrb,zrb,crxb,crzb)
            if(abs(sigman6(1,ix)).lt.pieni.or.
     &         abs(sigman6(2,ix)).lt.pieni) call error(88)      
            tkb=(ed(ix)*ed(ix)/(sigman6(1,ix)*sigman6(1,ix))+
     &      ek(ix)*ek(ix)/(sigman6(2,ix)*sigman6(2,ix)))/two
            xbb=sigman6(2,ix)/sigman6(1,ix)*xrb
            zbb=sigman6(1,ix)/sigman6(2,ix)*zrb
            call errf(xbb,zbb,cbxb,cbzb)
c            if(ibtyp.eq.0) call errf(xbb,zbb,cbxb,cbzb)
c            if(ibtyp.eq.1) call wzsub(xbb,zbb,cbxb,cbzb)
              beamoff1=rkb*(crzb-exp(-tkb)*cbzb)*
     &        sign(one,ed(ix))
              beamoff2=rkb*(crxb-exp(-tkb)*cbxb)*
     &        sign(one,ek(ix))
              endif
      if(abs(sigman6(1,ix)).lt.pieni.or.
     &   abs(sigman6(2,ix)).lt.pieni) call error(88)    
*FOX  R2BF=TWO*(SIGMAN6(1,IX)*SIGMAN6(1,IX)-                            *FOX
*FOX  SIGMAN6(2,IX)*SIGMAN6(2,IX)) ;
      RSCRRI(  1+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  2+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  3+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  4+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  5+IDAA) = RSCRRI(  1+IDAA) * RSCRRI(  2+IDAA)            
      RSCRRI(  6+IDAA) = RSCRRI(  3+IDAA) * RSCRRI(  4+IDAA)            
      RSCRRI(  7+IDAA) = RSCRRI(  5+IDAA) - RSCRRI(  6+IDAA)            
      RSCRRI(  8+IDAA) = TWO         * RSCRRI(  7+IDAA)                 
      RSCRRI(100) = RSCRRI(  8+IDAA)                                    
      CALL DACON(R2BF       ,RSCRRI(100))                               
*FOX  RBF=SQRT(R2BF) ;                                                  *FOX
      CALL DAFUN('SQRT  ',R2BF       ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),RBF        )                          
*FOX  RKBF=TWO*CRAD*PARTNUM*GAMMAR*C1E6*PISQRT/RBF ;                    *FOX
      RSCRRI(  1+IDAA) = TWO         * CRAD                             
      RSCRRI(  2+IDAA) = RSCRRI(  1+IDAA) * PARTNUM                     
      RSCRRI(  3+IDAA) = RSCRRI(  2+IDAA) * GAMMAR                      
      RSCRRI(  4+IDAA) = RSCRRI(  3+IDAA) * C1E6                        
      RSCRRI(  5+IDAA) = RSCRRI(  4+IDAA) * PISQRT                      
      CALL DADIC(RBF        ,ONE*RSCRRI(  5+IDAA),ISCRDA(  6+IDAA))     
      CALL DACOP(ISCRDA(  6+IDAA),RKBF       )                          
            startco=dare(x(1))-ed(ix)
*FOX  CRKVEBF=X(1)-STARTCO ;                                            *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*STARTCO    ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CRKVEBF    )                          
            startco=dare(x(2))-ek(ix)
*FOX  CIKVEBF=X(2)-STARTCO ;                                            *FOX
      CALL DACOP(X          ((2)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*STARTCO    ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CIKVEBF    )                          
c*FOX  XRBF=ABS(CRKVEBF)/RBF ;                                          *FOX
c*FOX  ZRBF=ABS(CIKVEBF)/RBF ;                                          *FOX
*FOX  XRBF=CRKVEBF/RBF ;                                                *FOX
      CALL DADIV(CRKVEBF    ,RBF        ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),XRBF       )                          
      if(dare(XRBF).lt.zero) then                                       *FOX
*FOX  XRBF=-XRBF ;                                                      *FOX
      CALL DACMU(XRBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),XRBF       )                          
      endif
*FOX  ZRBF=CIKVEBF/RBF ;                                                *FOX
      CALL DADIV(CIKVEBF    ,RBF        ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ZRBF       )                          
      if(dare(ZRBF).lt.zero) then                                       *FOX
*FOX  ZRBF=-ZRBF ;                                                      *FOX
      CALL DACMU(ZRBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),ZRBF       )                          
      endif
            call errff(xrbf,zrbf,crxbf,crzbf,i)
c            if(ibtyp.eq.0) call errff(xrbf,zrbf,crxbf,crzbf,i)
c            if(ibtyp.eq.1) call wzsubf(xrbf,zrbf,crxbf,crzbf,i,nbeam)
      if(abs(sigman6(1,ix)).lt.pieni.or.
     &   abs(sigman6(2,ix)).lt.pieni) call error(88)    
*FOX  TKBF=(CRKVEBF*CRKVEBF/(SIGMAN6(1,IX)*SIGMAN6(1,IX))+              *FOX
*FOX  CIKVEBF*CIKVEBF/(SIGMAN6(2,IX)*SIGMAN6(2,IX)))/TWO ;              *FOX
      RSCRRI(  1+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  2+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  3+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  4+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  5+IDAA) = RSCRRI(  1+IDAA) * RSCRRI(  2+IDAA)            
      RSCRRI(  6+IDAA) = RSCRRI(  3+IDAA) * RSCRRI(  4+IDAA)            
      CALL DAMUL(CRKVEBF    ,CRKVEBF    ,ISCRDA(  7+IDAA))              
      CALL DACDI(ISCRDA(  7+IDAA),ONE*RSCRRI(  5+IDAA),ISCRDA(  8+IDAA))
     *                                                                  
      CALL DAMUL(CIKVEBF    ,CIKVEBF    ,ISCRDA(  9+IDAA))              
      CALL DACDI(ISCRDA(  9+IDAA),ONE*RSCRRI(  6+IDAA),ISCRDA( 10+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  8+IDAA),ISCRDA( 10+IDAA),ISCRDA( 11+IDAA))    
      CALL DACDI(ISCRDA( 11+IDAA),ONE*TWO        ,ISCRDA( 12+IDAA))     
      CALL DACOP(ISCRDA( 12+IDAA),TKBF       )                          
*FOX  XBBF=SIGMAN6(2,IX)/SIGMAN6(1,IX)*XRBF ;                           *FOX
      RSCRRI(  1+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  2+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  3+IDAA) = RSCRRI(  1+IDAA) / RSCRRI(  2+IDAA)            
      CALL DACMU(XRBF       ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),XBBF       )                          
*FOX  ZBBF=SIGMAN6(1,IX)/SIGMAN6(2,IX)*ZRBF ;                           *FOX
      RSCRRI(  1+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  2+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  3+IDAA) = RSCRRI(  1+IDAA) / RSCRRI(  2+IDAA)            
      CALL DACMU(ZRBF       ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),ZBBF       )                          
            if(nbeamo.eq.i) nbeam=nbeamo
            call errff(xbbf,zbbf,cbxbf,cbzbf,i)
c            if(ibtyp.eq.0) call errff(xbbf,zbbf,cbxbf,cbzbf,i)
c            if(ibtyp.eq.1) call wzsubf(xbbf,zbbf,cbxbf,cbzbf,i,nbeam)
      SCRKVEB=SIGN(ONE,DARE(CRKVEBF))                                   *FOX
      SCIKVEB=SIGN(ONE,DARE(CIKVEBF))                                   *FOX
*FOX  Y(1)=Y(1)+(RKBF*(CRZBF-EXP(-TKBF)*                                *FOX
*FOX  CBZBF)*SCRKVEB-BEAMOFF1)/(ONE+DPDA) ;                             *FOX
      CALL DACMU(TKBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAFUN('EXP   ',ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))            
      CALL DAMUL(ISCRDA(  2+IDAA),CBZBF      ,ISCRDA(  3+IDAA))         
      CALL DASUB(CRZBF      ,ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))         
      CALL DAMUL(RKBF       ,ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))         
      CALL DACMU(ISCRDA(  5+IDAA),ONE*SCRKVEB    ,ISCRDA(  6+IDAA))     
      CALL DACSU(ISCRDA(  6+IDAA),ONE*BEAMOFF1   ,ISCRDA(  7+IDAA))     
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  8+IDAA))          
      CALL DACOP(Y          ((1)),ISCRDA(  9+IDAA))                     
      CALL DADIV(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA( 10+IDAA))    
      CALL DAADD(ISCRDA(  9+IDAA),ISCRDA( 10+IDAA),ISCRDA( 11+IDAA))    
      CALL DACOP(ISCRDA( 11+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+(RKBF*(CRXBF-EXP(-TKBF)*                                *FOX
*FOX  CBXBF)*SCIKVEB-BEAMOFF2)/(ONE+DPDA) ;                             *FOX
      CALL DACMU(TKBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAFUN('EXP   ',ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))            
      CALL DAMUL(ISCRDA(  2+IDAA),CBXBF      ,ISCRDA(  3+IDAA))         
      CALL DASUB(CRXBF      ,ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))         
      CALL DAMUL(RKBF       ,ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))         
      CALL DACMU(ISCRDA(  5+IDAA),ONE*SCIKVEB    ,ISCRDA(  6+IDAA))     
      CALL DACSU(ISCRDA(  6+IDAA),ONE*BEAMOFF2   ,ISCRDA(  7+IDAA))     
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  8+IDAA))          
      CALL DACOP(Y          ((2)),ISCRDA(  9+IDAA))                     
      CALL DADIV(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA( 10+IDAA))    
      CALL DAADD(ISCRDA(  9+IDAA),ISCRDA( 10+IDAA),ISCRDA( 11+IDAA))    
      CALL DACOP(ISCRDA( 11+IDAA),Y          ((2)))                     
            else if(sigman6(1,ix).lt.sigman6(2,ix)) then
      BEAMOFF1=ZERO
      BEAMOFF2=ZERO
              if(ibeco.eq.1) then
            if(abs(sigman6(1,ix)).lt.pieni.or.
     &         abs(sigman6(2,ix)).lt.pieni) call error(88)      
            r2b=two*(sigman6(2,ix)*sigman6(2,ix)-
     &      sigman6(1,ix)*sigman6(1,ix))
            rb=sqrt(r2b)
            rkb=two*crad*partnum*gammar*c1e6*pisqrt/rb
            xrb=abs(ed(ix))/rb
            zrb=abs(ek(ix))/rb
            call errf(zrb,xrb,crzb,crxb)
c            if(ibtyp.eq.0) call errf(zrb,xrb,crzb,crxb)
c            if(ibtyp.eq.1) call wzsub(zrb,xrb,crzb,crxb)
            if(abs(sigman6(1,ix)).lt.pieni.or.
     &         abs(sigman6(2,ix)).lt.pieni) call error(88)      
            tkb=(ed(ix)*ed(ix)/(sigman6(1,ix)*sigman6(1,ix))+
     &      ek(ix)*ek(ix)/(sigman6(2,ix)*sigman6(2,ix)))/two
            xbb=sigman6(2,ix)/sigman6(1,ix)*xrb
            zbb=sigman6(1,ix)/sigman6(2,ix)*zrb
            call errf(zbb,xbb,cbzb,cbxb)
c            if(ibtyp.eq.0) call errf(zbb,xbb,cbzb,cbxb)
c            if(ibtyp.eq.1) call wzsub(zbb,xbb,cbzb,cbxb)
              beamoff1=rkb*(crzb-exp(-tkb)*cbzb)*
     &        sign(one,ed(ix))
              beamoff2=rkb*(crxb-exp(-tkb)*cbxb)*
     &        sign(one,ek(ix))
              endif
      if(abs(sigman6(1,ix)).lt.pieni.or.
     &   abs(sigman6(2,ix)).lt.pieni) call error(88)    
*FOX  R2BF=TWO*(SIGMAN6(2,IX)*SIGMAN6(2,IX)-                            *FOX
*FOX  SIGMAN6(1,IX)*SIGMAN6(1,IX)) ;
      RSCRRI(  1+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  2+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  3+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  4+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  5+IDAA) = RSCRRI(  1+IDAA) * RSCRRI(  2+IDAA)            
      RSCRRI(  6+IDAA) = RSCRRI(  3+IDAA) * RSCRRI(  4+IDAA)            
      RSCRRI(  7+IDAA) = RSCRRI(  5+IDAA) - RSCRRI(  6+IDAA)            
      RSCRRI(  8+IDAA) = TWO         * RSCRRI(  7+IDAA)                 
      RSCRRI(100) = RSCRRI(  8+IDAA)                                    
      CALL DACON(R2BF       ,RSCRRI(100))                               
*FOX  RBF=SQRT(R2BF) ;                                                  *FOX
      CALL DAFUN('SQRT  ',R2BF       ,ISCRDA(  1+IDAA))                 
      CALL DACOP(ISCRDA(  1+IDAA),RBF        )                          
*FOX  RKBF=TWO*CRAD*PARTNUM*GAMMAR*C1E6*PISQRT/RBF ;                    *FOX
      RSCRRI(  1+IDAA) = TWO         * CRAD                             
      RSCRRI(  2+IDAA) = RSCRRI(  1+IDAA) * PARTNUM                     
      RSCRRI(  3+IDAA) = RSCRRI(  2+IDAA) * GAMMAR                      
      RSCRRI(  4+IDAA) = RSCRRI(  3+IDAA) * C1E6                        
      RSCRRI(  5+IDAA) = RSCRRI(  4+IDAA) * PISQRT                      
      CALL DADIC(RBF        ,ONE*RSCRRI(  5+IDAA),ISCRDA(  6+IDAA))     
      CALL DACOP(ISCRDA(  6+IDAA),RKBF       )                          
            startco=dare(x(1))-ed(ix)
*FOX  CRKVEBF=X(1)-STARTCO ;                                            *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*STARTCO    ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CRKVEBF    )                          
            startco=dare(x(2))-ek(ix)
*FOX  CIKVEBF=X(2)-STARTCO ;                                            *FOX
      CALL DACOP(X          ((2)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*STARTCO    ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CIKVEBF    )                          
c*FOX  XRBF=ABS(CRKVEBF)/RBF ;                                          *FOX
c*FOX  ZRBF=ABS(CIKVEBF)/RBF ;                                          *FOX
*FOX  XRBF=CRKVEBF/RBF ;                                                *FOX
      CALL DADIV(CRKVEBF    ,RBF        ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),XRBF       )                          
      if(dare(XRBF).lt.zero) then                                       *FOX
*FOX  XRBF=-XRBF ;                                                      *FOX
      CALL DACMU(XRBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),XRBF       )                          
      endif
*FOX  ZRBF=CIKVEBF/RBF ;                                                *FOX
      CALL DADIV(CIKVEBF    ,RBF        ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),ZRBF       )                          
      if(dare(ZRBF).lt.zero) then                                       *FOX
*FOX  ZRBF=-ZRBF ;                                                      *FOX
      CALL DACMU(ZRBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),ZRBF       )                          
      endif
            call errff(zrbf,xrbf,crzbf,crxbf,i)
c            if(ibtyp.eq.0) call errff(zrbf,xrbf,crzbf,crxbf,i)
c            if(ibtyp.eq.1) call wzsubf(zrbf,xrbf,crzbf,crxbf,i,nbeam)
      if(abs(sigman6(1,ix)).lt.pieni.or.
     &   abs(sigman6(2,ix)).lt.pieni) call error(88)    
*FOX  TKBF=(CRKVEBF*CRKVEBF/(SIGMAN6(1,IX)*SIGMAN6(1,IX))+              *FOX
*FOX  CIKVEBF*CIKVEBF/(SIGMAN6(2,IX)*SIGMAN6(2,IX)))/TWO ;              *FOX
      RSCRRI(  1+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  2+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  3+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  4+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  5+IDAA) = RSCRRI(  1+IDAA) * RSCRRI(  2+IDAA)            
      RSCRRI(  6+IDAA) = RSCRRI(  3+IDAA) * RSCRRI(  4+IDAA)            
      CALL DAMUL(CRKVEBF    ,CRKVEBF    ,ISCRDA(  7+IDAA))              
      CALL DACDI(ISCRDA(  7+IDAA),ONE*RSCRRI(  5+IDAA),ISCRDA(  8+IDAA))
     *                                                                  
      CALL DAMUL(CIKVEBF    ,CIKVEBF    ,ISCRDA(  9+IDAA))              
      CALL DACDI(ISCRDA(  9+IDAA),ONE*RSCRRI(  6+IDAA),ISCRDA( 10+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  8+IDAA),ISCRDA( 10+IDAA),ISCRDA( 11+IDAA))    
      CALL DACDI(ISCRDA( 11+IDAA),ONE*TWO        ,ISCRDA( 12+IDAA))     
      CALL DACOP(ISCRDA( 12+IDAA),TKBF       )                          
*FOX  XBBF=SIGMAN6(2,IX)/SIGMAN6(1,IX)*XRBF ;                           *FOX
      RSCRRI(  1+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  2+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  3+IDAA) = RSCRRI(  1+IDAA) / RSCRRI(  2+IDAA)            
      CALL DACMU(XRBF       ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),XBBF       )                          
*FOX  ZBBF=SIGMAN6(1,IX)/SIGMAN6(2,IX)*ZRBF ;                           *FOX
      RSCRRI(  1+IDAA) = SIGMAN6    ((1),IX         )                   
      RSCRRI(  2+IDAA) = SIGMAN6    ((2),IX         )                   
      RSCRRI(  3+IDAA) = RSCRRI(  1+IDAA) / RSCRRI(  2+IDAA)            
      CALL DACMU(ZRBF       ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACOP(ISCRDA(  4+IDAA),ZBBF       )                          
            if(nbeamo.eq.i) nbeam=nbeamo
            call errff(zbbf,xbbf,cbzbf,cbxbf,i)
c            if(ibtyp.eq.0) call errff(zbbf,xbbf,cbzbf,cbxbf,i)
c            if(ibtyp.eq.1) call wzsubf(zbbf,xbbf,cbzbf,cbxbf,i,nbeam)
      SCRKVEB=SIGN(ONE,DARE(CRKVEBF))                                   *FOX
      SCIKVEB=SIGN(ONE,DARE(CIKVEBF))                                   *FOX
*FOX  Y(1)=Y(1)+(RKBF*(CRZBF-EXP(-TKBF)*                                *FOX
*FOX  CBZBF)*SCRKVEB-BEAMOFF1)/(ONE+DPDA) ;                             *FOX
      CALL DACMU(TKBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAFUN('EXP   ',ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))            
      CALL DAMUL(ISCRDA(  2+IDAA),CBZBF      ,ISCRDA(  3+IDAA))         
      CALL DASUB(CRZBF      ,ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))         
      CALL DAMUL(RKBF       ,ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))         
      CALL DACMU(ISCRDA(  5+IDAA),ONE*SCRKVEB    ,ISCRDA(  6+IDAA))     
      CALL DACSU(ISCRDA(  6+IDAA),ONE*BEAMOFF1   ,ISCRDA(  7+IDAA))     
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  8+IDAA))          
      CALL DACOP(Y          ((1)),ISCRDA(  9+IDAA))                     
      CALL DADIV(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA( 10+IDAA))    
      CALL DAADD(ISCRDA(  9+IDAA),ISCRDA( 10+IDAA),ISCRDA( 11+IDAA))    
      CALL DACOP(ISCRDA( 11+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+(RKBF*(CRXBF-EXP(-TKBF)*                                *FOX
*FOX  CBXBF)*SCIKVEB-BEAMOFF2)/(ONE+DPDA) ;                             *FOX
      CALL DACMU(TKBF       ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAFUN('EXP   ',ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))            
      CALL DAMUL(ISCRDA(  2+IDAA),CBXBF      ,ISCRDA(  3+IDAA))         
      CALL DASUB(CRXBF      ,ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))         
      CALL DAMUL(RKBF       ,ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))         
      CALL DACMU(ISCRDA(  5+IDAA),ONE*SCIKVEB    ,ISCRDA(  6+IDAA))     
      CALL DACSU(ISCRDA(  6+IDAA),ONE*BEAMOFF2   ,ISCRDA(  7+IDAA))     
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  8+IDAA))          
      CALL DACOP(Y          ((2)),ISCRDA(  9+IDAA))                     
      CALL DADIV(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA( 10+IDAA))    
      CALL DAADD(ISCRDA(  9+IDAA),ISCRDA( 10+IDAA),ISCRDA( 11+IDAA))    
      CALL DACOP(ISCRDA( 11+IDAA),Y          ((2)))                     
            endif
            goto 430
          endif
          goto 430
        endif
        if(kzz.eq.0) goto 430
        ipch=0
        if(iqmod6.eq.1) then
          if(ix.eq.iq(1).or.iratioe(ix).eq.iq(1)) then
            ipch=1
          else if(ix.eq.iq(2).or.iratioe(ix).eq.iq(2)) then
            ipch=2
          endif
        endif
        if(ipch.ne.0) then
          ekks=smi(i)-edcor(ipch)
*FOX  EKK=(SMIDA(IPCH)*RATIOE(IX)+EKKS)/(ONE+DPDA) ;                    *FOX
      CALL DACOP(SMIDA      (IPCH       ),ISCRDA(  1+IDAA))             
      RSCRRI(  2+IDAA) = RATIOE     (IX         )                       
      CALL DACMU(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACAD(ISCRDA(  3+IDAA),ONE*EKKS       ,ISCRDA(  4+IDAA))     
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  5+IDAA))          
      CALL DADIV(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(ISCRDA(  6+IDAA),EKK        )                          
        else
*FOX  EKK=SMI(I)/(ONE+DPDA) ;                                           *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      RSCRRI(  2+IDAA) = SMI        (I          )                       
      CALL DADIC(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  3+IDAA),EKK        )                          
        endif
        xs=xsi(i)
        zs=zsi(i)
*FOX  XL=(X(1)-XS)*TILTC(I)+(X(2)-ZS)*TILTS(I) ;                        *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(X          ((2)),ISCRDA(  2+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*XS         ,ISCRDA(  3+IDAA))     
      CALL DACSU(ISCRDA(  2+IDAA),ONE*ZS         ,ISCRDA(  4+IDAA))     
      RSCRRI(  5+IDAA) = TILTC      (I          )                       
      RSCRRI(  6+IDAA) = TILTS      (I          )                       
      CALL DACMU(ISCRDA(  3+IDAA),ONE*RSCRRI(  5+IDAA),ISCRDA(  7+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA(  4+IDAA),ONE*RSCRRI(  6+IDAA),ISCRDA(  8+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),XL         )                          
*FOX  ZL=-(X(1)-XS)*TILTS(I)+(X(2)-ZS)*TILTC(I) ;                       *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(X          ((2)),ISCRDA(  2+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*XS         ,ISCRDA(  3+IDAA))     
      CALL DACSU(ISCRDA(  2+IDAA),ONE*ZS         ,ISCRDA(  4+IDAA))     
      RSCRRI(  5+IDAA) = TILTS      (I          )                       
      RSCRRI(  6+IDAA) = TILTC      (I          )                       
      CALL DACMU(ISCRDA(  3+IDAA),ONE*(-ONE       ),ISCRDA(  7+IDAA))   
      CALL DACMU(ISCRDA(  7+IDAA),ONE*RSCRRI(  5+IDAA),ISCRDA(  8+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA(  4+IDAA),ONE*RSCRRI(  6+IDAA),ISCRDA(  9+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  8+IDAA),ISCRDA(  9+IDAA),ISCRDA( 10+IDAA))    
      CALL DACOP(ISCRDA( 10+IDAA),ZL         )                          
*FOX  CRKVE=XL ;                                                        *FOX
      CALL DACOP(XL         ,CRKVE      )                               
*FOX  CIKVE=ZL ;                                                        *FOX
      CALL DACOP(ZL         ,CIKVE      )                               
        if(kzz.lt.0) goto 320
        goto(90,100,110,120,130,140,150,160,170,180,190),kzz
        goto 430
C--HORIZONTAL DIPOLE
   90   continue
*FOX  EKK=EKK*C1E3 ;                                                    *FOX
      CALL DACMU(EKK        ,ONE*C1E3       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  Y(1)=Y(1)+EKK*TILTC(I) ;                                          *FOX
      CALL DACOP(Y          ((1)),ISCRDA(  1+IDAA))                     
      RSCRRI(  2+IDAA) = TILTC      (I          )                       
      CALL DACMU(EKK        ,ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))     
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACOP(ISCRDA(  4+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*TILTS(I) ;                                          *FOX
      CALL DACOP(Y          ((2)),ISCRDA(  1+IDAA))                     
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(EKK        ,ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))     
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACOP(ISCRDA(  4+IDAA),Y          ((2)))                     
        goto 430
C--NORMAL QUADRUPOLE
  100   continue
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(-TILTC(I)*CIKVE+TILTS(I)*CRKVE) ;                  *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      RSCRRI(  3+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        goto 430
C---NORMAL SEXTUPOLE
  110   continue
*FOX  EKK=EKK*C1M3 ;                                                    *FOX
      CALL DACMU(EKK        ,ONE*C1M3       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(-TILTC(I)*CIKVE+TILTS(I)*CRKVE) ;                  *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      RSCRRI(  3+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        goto 430
C--NORMAL OCTUPOLE
  120   continue
*FOX  EKK=EKK*C1M6 ;                                                    *FOX
      CALL DACMU(EKK        ,ONE*C1M6       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(-TILTC(I)*CIKVE+TILTS(I)*CRKVE) ;                  *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      RSCRRI(  3+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        goto 430
C--NORMAL DECAPOLE
  130   continue
*FOX  EKK=EKK*C1M9 ;                                                    *FOX
      CALL DACMU(EKK        ,ONE*C1M9       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(-TILTC(I)*CIKVE+TILTS(I)*CRKVE) ;                  *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      RSCRRI(  3+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        goto 430
C---NORMAL DODECAPOL
  140   continue
*FOX  EKK=EKK*C1M12 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M12      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(-TILTC(I)*CIKVE+TILTS(I)*CRKVE) ;                  *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      RSCRRI(  3+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        goto 430
C---NORMAL 14-POL
  150   continue
*FOX  EKK=EKK*C1M15 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M15      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(-TILTC(I)*CIKVE+TILTS(I)*CRKVE) ;                  *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      RSCRRI(  3+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        goto 430
C---NORMAL 16-POL
  160   continue
*FOX  EKK=EKK*C1M18 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M18      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(-TILTC(I)*CIKVE+TILTS(I)*CRKVE) ;                  *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      RSCRRI(  3+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        goto 430
C---NORMAL 18-POL
  170   continue
*FOX  EKK=EKK*C1M21 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M21      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(-TILTC(I)*CIKVE+TILTS(I)*CRKVE) ;                  *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      RSCRRI(  3+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        goto 430
C---NORMAL 20-POL
  180   continue
*FOX  EKK=EKK*C1M24 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M24      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(-TILTC(I)*CIKVE+TILTS(I)*CRKVE) ;                  *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      RSCRRI(  3+IDAA) = (-ONE       ) * RSCRRI(  1+IDAA)               
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  5+IDAA))     
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))         
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        goto 430
  190   r0=ek(ix)
        nmz=nmu(ix)
          if(abs(dki(ix,1)).gt.pieni) then
            if(abs(dki(ix,3)).gt.pieni) then
*FOX  DKIP=DKI(IX,1)/(ONE+DPDA) ;                                       *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      RSCRRI(  2+IDAA) = DKI        (IX         ,(1))                   
      CALL DADIC(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  3+IDAA),DKIP       )                          
*FOX  Y(1)=Y(1)-(DKI(IX,1)/DKI(IX,3)*XL+DPDA*C1E3)*                     *FOX
*FOX  TILTC(I)*DKIP                                                     *FOX
*FOX  +C1E3*DKI(IX,1)/(ONE+DPDA)*(ONE-TILTC(I)) ;                       *FOX
      RSCRRI(  1+IDAA) = DKI        (IX         ,(1))                   
      RSCRRI(  2+IDAA) = DKI        (IX         ,(3))                   
      RSCRRI(  3+IDAA) = TILTC      (I          )                       
      RSCRRI(  4+IDAA) = RSCRRI(  1+IDAA) / RSCRRI(  2+IDAA)            
      CALL DACMU(XL         ,ONE*RSCRRI(  4+IDAA),ISCRDA(  5+IDAA))     
      CALL DACMU(DPDA       ,ONE*C1E3       ,ISCRDA(  6+IDAA))          
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA(  7+IDAA))    
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  8+IDAA))          
      RSCRRI(  9+IDAA) = ONE         - RSCRRI(  3+IDAA)                 
      CALL DACOP(Y          ((1)),ISCRDA( 10+IDAA))                     
      RSCRRI( 11+IDAA) = TILTC      (I          )                       
      RSCRRI( 12+IDAA) = DKI        (IX         ,(1))                   
      CALL DACMU(ISCRDA(  7+IDAA),ONE*RSCRRI( 11+IDAA),ISCRDA( 13+IDAA))
     *                                                                  
      CALL DAMUL(ISCRDA( 13+IDAA),DKIP       ,ISCRDA( 14+IDAA))         
      RSCRRI( 15+IDAA) = C1E3        * RSCRRI( 12+IDAA)                 
      CALL DADIC(ISCRDA(  8+IDAA),ONE*RSCRRI( 15+IDAA),ISCRDA( 16+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA( 16+IDAA),ONE*RSCRRI(  9+IDAA),ISCRDA( 17+IDAA))
     *                                                                  
      CALL DASUB(ISCRDA( 10+IDAA),ISCRDA( 14+IDAA),ISCRDA( 18+IDAA))    
      CALL DAADD(ISCRDA( 18+IDAA),ISCRDA( 17+IDAA),ISCRDA( 19+IDAA))    
      CALL DACOP(ISCRDA( 19+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)-(DKI(IX,1)/DKI(IX,3)*XL+DPDA*C1E3)*                     *FOX
*FOX  TILTS(I)*DKIP                                                     *FOX
*FOX  +C1E3*DKI(IX,1)/(ONE+DPDA)*TILTS(I) ;                             *FOX
      RSCRRI(  1+IDAA) = DKI        (IX         ,(1))                   
      RSCRRI(  2+IDAA) = DKI        (IX         ,(3))                   
      RSCRRI(  3+IDAA) = RSCRRI(  1+IDAA) / RSCRRI(  2+IDAA)            
      CALL DACMU(XL         ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(DPDA       ,ONE*C1E3       ,ISCRDA(  5+IDAA))          
      CALL DAADD(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  7+IDAA))          
      CALL DACOP(Y          ((2)),ISCRDA(  8+IDAA))                     
      RSCRRI(  9+IDAA) = TILTS      (I          )                       
      RSCRRI( 10+IDAA) = DKI        (IX         ,(1))                   
      RSCRRI( 11+IDAA) = TILTS      (I          )                       
      CALL DACMU(ISCRDA(  6+IDAA),ONE*RSCRRI(  9+IDAA),ISCRDA( 12+IDAA))
     *                                                                  
      CALL DAMUL(ISCRDA( 12+IDAA),DKIP       ,ISCRDA( 13+IDAA))         
      RSCRRI( 14+IDAA) = C1E3        * RSCRRI( 10+IDAA)                 
      CALL DADIC(ISCRDA(  7+IDAA),ONE*RSCRRI( 14+IDAA),ISCRDA( 15+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA( 15+IDAA),ONE*RSCRRI( 11+IDAA),ISCRDA( 16+IDAA))
     *                                                                  
      CALL DASUB(ISCRDA(  8+IDAA),ISCRDA( 13+IDAA),ISCRDA( 17+IDAA))    
      CALL DAADD(ISCRDA( 17+IDAA),ISCRDA( 16+IDAA),ISCRDA( 18+IDAA))    
      CALL DACOP(ISCRDA( 18+IDAA),Y          ((2)))                     
            else
*FOX  Y(1)=Y(1)-DKI(IX,1)*DPDA*C1E3/(ONE+DPDA)*TILTC(I)                 *FOX
*FOX  +C1E3*DKI(IX,1)/(ONE+DPDA)*(ONE-TILTC(I)) ;                       *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  2+IDAA))          
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  3+IDAA))          
      RSCRRI(  4+IDAA) = ONE         - RSCRRI(  1+IDAA)                 
      CALL DACOP(Y          ((1)),ISCRDA(  5+IDAA))                     
      RSCRRI(  6+IDAA) = DKI        (IX         ,(1))                   
      RSCRRI(  7+IDAA) = TILTC      (I          )                       
      RSCRRI(  8+IDAA) = DKI        (IX         ,(1))                   
      CALL DACMU(DPDA       ,ONE*RSCRRI(  6+IDAA),ISCRDA(  9+IDAA))     
      CALL DACMU(ISCRDA(  9+IDAA),ONE*C1E3       ,ISCRDA( 10+IDAA))     
      CALL DADIV(ISCRDA( 10+IDAA),ISCRDA(  2+IDAA),ISCRDA( 11+IDAA))    
      CALL DACMU(ISCRDA( 11+IDAA),ONE*RSCRRI(  7+IDAA),ISCRDA( 12+IDAA))
     *                                                                  
      RSCRRI( 13+IDAA) = C1E3        * RSCRRI(  8+IDAA)                 
      CALL DADIC(ISCRDA(  3+IDAA),ONE*RSCRRI( 13+IDAA),ISCRDA( 14+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA( 14+IDAA),ONE*RSCRRI(  4+IDAA),ISCRDA( 15+IDAA))
     *                                                                  
      CALL DASUB(ISCRDA(  5+IDAA),ISCRDA( 12+IDAA),ISCRDA( 16+IDAA))    
      CALL DAADD(ISCRDA( 16+IDAA),ISCRDA( 15+IDAA),ISCRDA( 17+IDAA))    
      CALL DACOP(ISCRDA( 17+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)-DKI(IX,1)*DPDA*C1E3/(ONE+DPDA)*TILTS(I)                 *FOX
*FOX  +C1E3*DKI(IX,1)/(ONE+DPDA)*TILTS(I) ;                             *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  2+IDAA))          
      CALL DACOP(Y          ((2)),ISCRDA(  3+IDAA))                     
      RSCRRI(  4+IDAA) = DKI        (IX         ,(1))                   
      RSCRRI(  5+IDAA) = TILTS      (I          )                       
      RSCRRI(  6+IDAA) = DKI        (IX         ,(1))                   
      RSCRRI(  7+IDAA) = TILTS      (I          )                       
      CALL DACMU(DPDA       ,ONE*RSCRRI(  4+IDAA),ISCRDA(  8+IDAA))     
      CALL DACMU(ISCRDA(  8+IDAA),ONE*C1E3       ,ISCRDA(  9+IDAA))     
      CALL DADIV(ISCRDA(  9+IDAA),ISCRDA(  1+IDAA),ISCRDA( 10+IDAA))    
      CALL DACMU(ISCRDA( 10+IDAA),ONE*RSCRRI(  5+IDAA),ISCRDA( 11+IDAA))
     *                                                                  
      RSCRRI( 12+IDAA) = C1E3        * RSCRRI(  6+IDAA)                 
      CALL DADIC(ISCRDA(  2+IDAA),ONE*RSCRRI( 12+IDAA),ISCRDA( 13+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA( 13+IDAA),ONE*RSCRRI(  7+IDAA),ISCRDA( 14+IDAA))
     *                                                                  
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA( 11+IDAA),ISCRDA( 15+IDAA))    
      CALL DAADD(ISCRDA( 15+IDAA),ISCRDA( 14+IDAA),ISCRDA( 16+IDAA))    
      CALL DACOP(ISCRDA( 16+IDAA),Y          ((2)))                     
            endif
            if(idp.eq.1.and.iabs(ition).eq.1) then
*FOX  SIGMDA=SIGMDA+RV*DKI(IX,1)*XL ;                                   *FOX
      RSCRRI(  1+IDAA) = DKI        (IX         ,(1))                   
      CALL DACMU(RV         ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DAMUL(ISCRDA(  2+IDAA),XL         ,ISCRDA(  3+IDAA))         
      CALL DAADD(SIGMDA     ,ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))         
      CALL DACOP(ISCRDA(  4+IDAA),SIGMDA     )                          
            endif
          endif
          if(abs(dki(ix,2)).gt.pieni) then
            if(abs(dki(ix,3)).gt.pieni) then
*FOX  DKIP=DKI(IX,2)/(ONE+DPDA) ;                                       *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      RSCRRI(  2+IDAA) = DKI        (IX         ,(2))                   
      CALL DADIC(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  3+IDAA),DKIP       )                          
*FOX  Y(1)=Y(1)+(DKI(IX,2)/DKI(IX,3)*ZL-DPDA*C1E3)*                     *FOX
*FOX  TILTS(I)*DKIP                                                     *FOX
*FOX  +C1E3*DKI(IX,2)/(ONE+DPDA)*TILTS(I) ;                             *FOX
      RSCRRI(  1+IDAA) = DKI        (IX         ,(2))                   
      RSCRRI(  2+IDAA) = DKI        (IX         ,(3))                   
      RSCRRI(  3+IDAA) = RSCRRI(  1+IDAA) / RSCRRI(  2+IDAA)            
      CALL DACMU(ZL         ,ONE*RSCRRI(  3+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(DPDA       ,ONE*C1E3       ,ISCRDA(  5+IDAA))          
      CALL DASUB(ISCRDA(  4+IDAA),ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))    
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  7+IDAA))          
      CALL DACOP(Y          ((1)),ISCRDA(  8+IDAA))                     
      RSCRRI(  9+IDAA) = TILTS      (I          )                       
      RSCRRI( 10+IDAA) = DKI        (IX         ,(2))                   
      RSCRRI( 11+IDAA) = TILTS      (I          )                       
      CALL DACMU(ISCRDA(  6+IDAA),ONE*RSCRRI(  9+IDAA),ISCRDA( 12+IDAA))
     *                                                                  
      CALL DAMUL(ISCRDA( 12+IDAA),DKIP       ,ISCRDA( 13+IDAA))         
      RSCRRI( 14+IDAA) = C1E3        * RSCRRI( 10+IDAA)                 
      CALL DADIC(ISCRDA(  7+IDAA),ONE*RSCRRI( 14+IDAA),ISCRDA( 15+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA( 15+IDAA),ONE*RSCRRI( 11+IDAA),ISCRDA( 16+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  8+IDAA),ISCRDA( 13+IDAA),ISCRDA( 17+IDAA))    
      CALL DAADD(ISCRDA( 17+IDAA),ISCRDA( 16+IDAA),ISCRDA( 18+IDAA))    
      CALL DACOP(ISCRDA( 18+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)-(DKI(IX,2)/DKI(IX,3)*ZL-DPDA*C1E3)*                     *FOX
*FOX  TILTC(I)*DKIP                                                     *FOX
*FOX  -C1E3*DKI(IX,2)/(ONE+DPDA)*(ONE-TILTC(I)) ;                       *FOX
      RSCRRI(  1+IDAA) = DKI        (IX         ,(2))                   
      RSCRRI(  2+IDAA) = DKI        (IX         ,(3))                   
      RSCRRI(  3+IDAA) = TILTC      (I          )                       
      RSCRRI(  4+IDAA) = RSCRRI(  1+IDAA) / RSCRRI(  2+IDAA)            
      CALL DACMU(ZL         ,ONE*RSCRRI(  4+IDAA),ISCRDA(  5+IDAA))     
      CALL DACMU(DPDA       ,ONE*C1E3       ,ISCRDA(  6+IDAA))          
      CALL DASUB(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA(  7+IDAA))    
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  8+IDAA))          
      RSCRRI(  9+IDAA) = ONE         - RSCRRI(  3+IDAA)                 
      CALL DACOP(Y          ((2)),ISCRDA( 10+IDAA))                     
      RSCRRI( 11+IDAA) = TILTC      (I          )                       
      RSCRRI( 12+IDAA) = DKI        (IX         ,(2))                   
      CALL DACMU(ISCRDA(  7+IDAA),ONE*RSCRRI( 11+IDAA),ISCRDA( 13+IDAA))
     *                                                                  
      CALL DAMUL(ISCRDA( 13+IDAA),DKIP       ,ISCRDA( 14+IDAA))         
      RSCRRI( 15+IDAA) = C1E3        * RSCRRI( 12+IDAA)                 
      CALL DADIC(ISCRDA(  8+IDAA),ONE*RSCRRI( 15+IDAA),ISCRDA( 16+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA( 16+IDAA),ONE*RSCRRI(  9+IDAA),ISCRDA( 17+IDAA))
     *                                                                  
      CALL DASUB(ISCRDA( 10+IDAA),ISCRDA( 14+IDAA),ISCRDA( 18+IDAA))    
      CALL DASUB(ISCRDA( 18+IDAA),ISCRDA( 17+IDAA),ISCRDA( 19+IDAA))    
      CALL DACOP(ISCRDA( 19+IDAA),Y          ((2)))                     
            else
*FOX  Y(1)=Y(1)-DKI(IX,2)*DPDA*C1E3/(ONE+DPDA)*TILTS(I)                 *FOX
*FOX  +C1E3*DKI(IX,2)/(ONE+DPDA)*TILTS(I) ;                             *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  2+IDAA))          
      CALL DACOP(Y          ((1)),ISCRDA(  3+IDAA))                     
      RSCRRI(  4+IDAA) = DKI        (IX         ,(2))                   
      RSCRRI(  5+IDAA) = TILTS      (I          )                       
      RSCRRI(  6+IDAA) = DKI        (IX         ,(2))                   
      RSCRRI(  7+IDAA) = TILTS      (I          )                       
      CALL DACMU(DPDA       ,ONE*RSCRRI(  4+IDAA),ISCRDA(  8+IDAA))     
      CALL DACMU(ISCRDA(  8+IDAA),ONE*C1E3       ,ISCRDA(  9+IDAA))     
      CALL DADIV(ISCRDA(  9+IDAA),ISCRDA(  1+IDAA),ISCRDA( 10+IDAA))    
      CALL DACMU(ISCRDA( 10+IDAA),ONE*RSCRRI(  5+IDAA),ISCRDA( 11+IDAA))
     *                                                                  
      RSCRRI( 12+IDAA) = C1E3        * RSCRRI(  6+IDAA)                 
      CALL DADIC(ISCRDA(  2+IDAA),ONE*RSCRRI( 12+IDAA),ISCRDA( 13+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA( 13+IDAA),ONE*RSCRRI(  7+IDAA),ISCRDA( 14+IDAA))
     *                                                                  
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA( 11+IDAA),ISCRDA( 15+IDAA))    
      CALL DAADD(ISCRDA( 15+IDAA),ISCRDA( 14+IDAA),ISCRDA( 16+IDAA))    
      CALL DACOP(ISCRDA( 16+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+DKI(IX,2)*DPDA*C1E3/(ONE+DPDA)*TILTC(I)                 *FOX
*FOX  -C1E3*DKI(IX,2)/(ONE+DPDA)*(ONE-TILTC(I)) ;                       *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  2+IDAA))          
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  3+IDAA))          
      RSCRRI(  4+IDAA) = ONE         - RSCRRI(  1+IDAA)                 
      CALL DACOP(Y          ((2)),ISCRDA(  5+IDAA))                     
      RSCRRI(  6+IDAA) = DKI        (IX         ,(2))                   
      RSCRRI(  7+IDAA) = TILTC      (I          )                       
      RSCRRI(  8+IDAA) = DKI        (IX         ,(2))                   
      CALL DACMU(DPDA       ,ONE*RSCRRI(  6+IDAA),ISCRDA(  9+IDAA))     
      CALL DACMU(ISCRDA(  9+IDAA),ONE*C1E3       ,ISCRDA( 10+IDAA))     
      CALL DADIV(ISCRDA( 10+IDAA),ISCRDA(  2+IDAA),ISCRDA( 11+IDAA))    
      CALL DACMU(ISCRDA( 11+IDAA),ONE*RSCRRI(  7+IDAA),ISCRDA( 12+IDAA))
     *                                                                  
      RSCRRI( 13+IDAA) = C1E3        * RSCRRI(  8+IDAA)                 
      CALL DADIC(ISCRDA(  3+IDAA),ONE*RSCRRI( 13+IDAA),ISCRDA( 14+IDAA))
     *                                                                  
      CALL DACMU(ISCRDA( 14+IDAA),ONE*RSCRRI(  4+IDAA),ISCRDA( 15+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA( 12+IDAA),ISCRDA( 16+IDAA))    
      CALL DASUB(ISCRDA( 16+IDAA),ISCRDA( 15+IDAA),ISCRDA( 17+IDAA))    
      CALL DACOP(ISCRDA( 17+IDAA),Y          ((2)))                     
            endif
            if(idp.eq.1.and.iabs(ition).eq.1) then
*FOX  SIGMDA=SIGMDA-RV*DKI(IX,2)*ZL ;                                   *FOX
      RSCRRI(  1+IDAA) = DKI        (IX         ,(2))                   
      CALL DACMU(RV         ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DAMUL(ISCRDA(  2+IDAA),ZL         ,ISCRDA(  3+IDAA))         
      CALL DASUB(SIGMDA     ,ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))         
      CALL DACOP(ISCRDA(  4+IDAA),SIGMDA     )                          
            endif
          endif
        if(abs(r0).le.pieni.or.nmz.eq.0) goto 430
        if(nmz.ge.2) then
*FOX  YV1J=BBI(I,1)+BBI(I,2)*XL+AAI(I,2)*ZL ;                           *FOX
      RSCRRI(  1+IDAA) = BBI        (I          ,(1))                   
      RSCRRI(  2+IDAA) = BBI        (I          ,(2))                   
      RSCRRI(  3+IDAA) = AAI        (I          ,(2))                   
      CALL DACMU(XL         ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(ZL         ,ONE*RSCRRI(  3+IDAA),ISCRDA(  5+IDAA))     
      CALL DACAD(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  6+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACOP(ISCRDA(  7+IDAA),YV1J       )                          
*FOX  YV2J=AAI(I,1)-BBI(I,2)*ZL+AAI(I,2)*XL ;                           *FOX
      RSCRRI(  1+IDAA) = AAI        (I          ,(1))                   
      RSCRRI(  2+IDAA) = BBI        (I          ,(2))                   
      RSCRRI(  3+IDAA) = AAI        (I          ,(2))                   
      CALL DACMU(ZL         ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DACMU(XL         ,ONE*RSCRRI(  3+IDAA),ISCRDA(  5+IDAA))     
      CALL DASUC(ISCRDA(  4+IDAA),ONE*RSCRRI(  1+IDAA),ISCRDA(  6+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))    
      CALL DACOP(ISCRDA(  7+IDAA),YV2J       )                          
*FOX  CRKVE=XL ;                                                        *FOX
      CALL DACOP(XL         ,CRKVE      )                               
*FOX  CIKVE=ZL ;                                                        *FOX
      CALL DACOP(ZL         ,CIKVE      )                               
          do 200 k=3,nmz
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  YV1J=YV1J+BBI(I,K)*CRKVE+AAI(I,K)*CIKVE ;                         *FOX
      RSCRRI(  1+IDAA) = BBI        (I          ,K          )           
      RSCRRI(  2+IDAA) = AAI        (I          ,K          )           
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(YV1J       ,ISCRDA(  3+IDAA),ISCRDA(  5+IDAA))         
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  4+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(ISCRDA(  6+IDAA),YV1J       )                          
*FOX  YV2J=YV2J-BBI(I,K)*CIKVE+AAI(I,K)*CRKVE ;                         *FOX
      RSCRRI(  1+IDAA) = BBI        (I          ,K          )           
      RSCRRI(  2+IDAA) = AAI        (I          ,K          )           
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(YV2J       ,ISCRDA(  3+IDAA),ISCRDA(  5+IDAA))         
      CALL DAADD(ISCRDA(  5+IDAA),ISCRDA(  4+IDAA),ISCRDA(  6+IDAA))    
      CALL DACOP(ISCRDA(  6+IDAA),YV2J       )                          
  200     continue
*FOX  Y(1)=Y(1)+(TILTC(I)*YV1J-TILTS(I)*YV2J)/(ONE+DPDA) ;              *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(YV1J       ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(YV2J       ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  6+IDAA))          
      CALL DACOP(Y          ((1)),ISCRDA(  7+IDAA))                     
      CALL DADIV(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+(TILTC(I)*YV2J+TILTS(I)*YV1J)/(ONE+DPDA) ;              *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(YV2J       ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(YV1J       ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  6+IDAA))          
      CALL DACOP(Y          ((2)),ISCRDA(  7+IDAA))                     
      CALL DADIV(ISCRDA(  5+IDAA),ISCRDA(  6+IDAA),ISCRDA(  8+IDAA))    
      CALL DAADD(ISCRDA(  7+IDAA),ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))    
      CALL DACOP(ISCRDA(  9+IDAA),Y          ((2)))                     
        else  
*FOX  Y(1)=Y(1)+(TILTC(I)*BBI(I,1)-TILTS(I)*AAI(I,1))/(ONE+DPDA) ;      *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = BBI        (I          ,(1))                   
      RSCRRI(  3+IDAA) = TILTS      (I          )                       
      RSCRRI(  4+IDAA) = AAI        (I          ,(1))                   
      RSCRRI(  5+IDAA) = RSCRRI(  1+IDAA) * RSCRRI(  2+IDAA)            
      RSCRRI(  6+IDAA) = RSCRRI(  3+IDAA) * RSCRRI(  4+IDAA)            
      RSCRRI(  7+IDAA) = RSCRRI(  5+IDAA) - RSCRRI(  6+IDAA)            
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  8+IDAA))          
      CALL DACOP(Y          ((1)),ISCRDA(  9+IDAA))                     
      CALL DADIC(ISCRDA(  8+IDAA),ONE*RSCRRI(  7+IDAA),ISCRDA( 10+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  9+IDAA),ISCRDA( 10+IDAA),ISCRDA( 11+IDAA))    
      CALL DACOP(ISCRDA( 11+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+(TILTC(I)*AAI(I,1)+TILTS(I)*BBI(I,1))/(ONE+DPDA) ;      *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = AAI        (I          ,(1))                   
      RSCRRI(  3+IDAA) = TILTS      (I          )                       
      RSCRRI(  4+IDAA) = BBI        (I          ,(1))                   
      RSCRRI(  5+IDAA) = RSCRRI(  1+IDAA) * RSCRRI(  2+IDAA)            
      RSCRRI(  6+IDAA) = RSCRRI(  3+IDAA) * RSCRRI(  4+IDAA)            
      RSCRRI(  7+IDAA) = RSCRRI(  5+IDAA) + RSCRRI(  6+IDAA)            
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  8+IDAA))          
      CALL DACOP(Y          ((2)),ISCRDA(  9+IDAA))                     
      CALL DADIC(ISCRDA(  8+IDAA),ONE*RSCRRI(  7+IDAA),ISCRDA( 10+IDAA))
     *                                                                  
      CALL DAADD(ISCRDA(  9+IDAA),ISCRDA( 10+IDAA),ISCRDA( 11+IDAA))    
      CALL DACOP(ISCRDA( 11+IDAA),Y          ((2)))                     
        endif
        goto 430
C--SKEW ELEMENTS
  320   kzz=-kzz
        goto(330,340,350,360,370,380,390,400,410,420),kzz
        goto 430
C---VERTICAL DIPOLE
  330   continue
*FOX  EKK=EKK*C1E3 ;                                                    *FOX
      CALL DACMU(EKK        ,ONE*C1E3       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  Y(1)=Y(1)-EKK*TILTS(I) ;                                          *FOX
      CALL DACOP(Y          ((1)),ISCRDA(  1+IDAA))                     
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(EKK        ,ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))     
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACOP(ISCRDA(  4+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*TILTC(I) ;                                          *FOX
      CALL DACOP(Y          ((2)),ISCRDA(  1+IDAA))                     
      RSCRRI(  2+IDAA) = TILTC      (I          )                       
      CALL DACMU(EKK        ,ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))     
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))    
      CALL DACOP(ISCRDA(  4+IDAA),Y          ((2)))                     
        goto 430
C---SKEW QUADRUPOLE
  340   continue
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CIKVE-TILTS(I)*CRKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((2)))                     
        goto 430
C---SKEW SEXTUPOLE
  350   continue
*FOX  EKK=EKK*C1M3 ;                                                    *FOX
      CALL DACMU(EKK        ,ONE*C1M3       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CIKVE-TILTS(I)*CRKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((2)))                     
        goto 430
C---SKEW OCTUPOLE
  360   continue
*FOX  EKK=EKK*C1M6 ;                                                    *FOX
      CALL DACMU(EKK        ,ONE*C1M6       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CIKVE-TILTS(I)*CRKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((2)))                     
        goto 430
C---SKEW DECAPOLE
  370   continue
*FOX  EKK=EKK*C1M9 ;                                                    *FOX
      CALL DACMU(EKK        ,ONE*C1M9       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CIKVE-TILTS(I)*CRKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((2)))                     
        goto 430
C---SKEW DODECAPOL
  380   continue
*FOX  EKK=EKK*C1M12 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M12      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CIKVE-TILTS(I)*CRKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((2)))                     
        goto 430
C---SKEW 14-POL
  390   continue
*FOX  EKK=EKK*C1M15 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M15      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CIKVE-TILTS(I)*CRKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((2)))                     
        goto 430
C---SKEW 16-POL
  400   continue
*FOX  EKK=EKK*C1M18 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M18      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CIKVE-TILTS(I)*CRKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((2)))                     
        goto 430
C---SKEW 18-POL
  410   continue
*FOX  EKK=EKK*C1M21 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M21      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CIKVE-TILTS(I)*CRKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((2)))                     
        goto 430
C---SKEW 20-POL
  420   continue
*FOX  EKK=EKK*C1M24 ;                                                   *FOX
      CALL DACMU(EKK        ,ONE*C1M24      ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),EKK        )                          
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  CRKVEUK=CRKVE*XL-CIKVE*ZL ;                                       *FOX
      CALL DAMUL(CRKVE      ,XL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,ZL         ,ISCRDA(  2+IDAA))              
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CRKVEUK    )                          
*FOX  CIKVE=CRKVE*ZL+CIKVE*XL ;                                         *FOX
      CALL DAMUL(CRKVE      ,ZL         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(CIKVE      ,XL         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),CIKVE      )                          
*FOX  CRKVE=CRKVEUK ;                                                   *FOX
      CALL DACOP(CRKVEUK    ,CRKVE      )                               
*FOX  Y(1)=Y(1)+EKK*(TILTC(I)*CIKVE-TILTS(I)*CRKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((1)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((1)))                     
*FOX  Y(2)=Y(2)+EKK*(TILTC(I)*CRKVE+TILTS(I)*CIKVE) ;                   *FOX
      RSCRRI(  1+IDAA) = TILTC      (I          )                       
      RSCRRI(  2+IDAA) = TILTS      (I          )                       
      CALL DACMU(CRKVE      ,ONE*RSCRRI(  1+IDAA),ISCRDA(  3+IDAA))     
      CALL DACMU(CIKVE      ,ONE*RSCRRI(  2+IDAA),ISCRDA(  4+IDAA))     
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(Y          ((2)),ISCRDA(  6+IDAA))                     
      CALL DAMUL(EKK        ,ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))         
      CALL DAADD(ISCRDA(  6+IDAA),ISCRDA(  7+IDAA),ISCRDA(  8+IDAA))    
      CALL DACOP(ISCRDA(  8+IDAA),Y          ((2)))                     
  430 continue
*FOX  YP(1)=Y(1)*(ONE+DPDA) ;                                           *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(Y          ((1)),ISCRDA(  2+IDAA))                     
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),YP         ((1)))                     
*FOX  YP(2)=Y(2)*(ONE+DPDA) ;                                           *FOX
      CALL DACAD(DPDA       ,ONE*ONE        ,ISCRDA(  1+IDAA))          
      CALL DACOP(Y          ((2)),ISCRDA(  2+IDAA))                     
      CALL DAMUL(ISCRDA(  2+IDAA),ISCRDA(  1+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),YP         ((2)))                     
      if(icav.eq.0.or.ithick.ne.1) then
        xxtr(1,1) = dare(x(1))
        yytr(1,1) = dare(y(1))
        xxtr(1,2) = dare(x(2))
        yytr(1,2) = dare(y(2))
        sigm(1) = dare(sigmda)
        dps(1) = dare(dpda)
      else
*FOX  CORRAU1(1)=X(1) ;                                                 *FOX
      CALL DACOP(X          ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU1    ((1)))                     
*FOX  CORRAU1(2)=YP(1) ;                                                *FOX
      CALL DACOP(YP         ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU1    ((2)))                     
*FOX  CORRAU1(3)=X(2) ;                                                 *FOX
      CALL DACOP(X          ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU1    ((3)))                     
*FOX  CORRAU1(4)=YP(2) ;                                                *FOX
      CALL DACOP(YP         ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU1    ((4)))                     
*FOX  CORRAU1(5)=SIGMDA ;                                               *FOX
      CALL DACOP(SIGMDA     ,CORRAU1    ((5)))                          
*FOX  CORRAU1(6)=DPDA1 ;                                                *FOX
      CALL DACOP(DPDA1      ,CORRAU1    ((6)))                          
        DO 435 KKK=1,6
          DPDAV2(KKK)=DARE(CORRAU1(KKK))                                       *FOX
*FOX  CORRAU1(KKK)=CORRAU1(KKK)-DPDAV2(KKK) ;                           *FOX
      CALL DACOP(CORRAU1    (KKK        ),ISCRDA(  1+IDAA))             
      RSCRRI(  2+IDAA) = DPDAV2     (KKK        )                       
      CALL DACSU(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  3+IDAA),CORRAU1    (KKK        ))             
  435   CONTINUE
        if(nvar.eq.8) then
*FOX  CORRAU1(7)=SMIDA(1) ;                                             *FOX
      CALL DACOP(SMIDA      ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU1    ((7)))                     
*FOX  CORRAU1(8)=SMIDA(2) ;                                             *FOX
      CALL DACOP(SMIDA      ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),CORRAU1    ((8)))                     
          blah1=dare(smida(1))
          blah2=dare(smida(2))
*FOX  CORRNEW(7)=SMIDA(1)-BLAH1 ;                                       *FOX
      CALL DACOP(SMIDA      ((1)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*BLAH1      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CORRNEW    ((7)))                     
*FOX  CORRNEW(8)=SMIDA(2)-BLAH2 ;                                       *FOX
      CALL DACOP(SMIDA      ((2)),ISCRDA(  1+IDAA))                     
      CALL DACSU(ISCRDA(  1+IDAA),ONE*BLAH2      ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),CORRNEW    ((8)))                     
        endif
        call dacct(corrau1,nvar,corrnew,nvar,corrau2,nvar)
        DO 436 KKK=1,6
*FOX  CORRAU2(KKK)=CORRAU2(KKK)+DPDAV2(KKK) ;                           *FOX
      CALL DACOP(CORRAU2    (KKK        ),ISCRDA(  1+IDAA))             
      RSCRRI(  2+IDAA) = DPDAV2     (KKK        )                       
      CALL DACAD(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DACOP(ISCRDA(  3+IDAA),CORRAU2    (KKK        ))             
  436   CONTINUE
*FOX  CORRAU1(2)=CORRAU2(2)/(ONE+CORRAU2(6)) ;                          *FOX
      CALL DACOP(CORRAU2    ((6)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*ONE        ,ISCRDA(  2+IDAA))     
      CALL DACOP(CORRAU2    ((2)),ISCRDA(  3+IDAA))                     
      CALL DADIV(ISCRDA(  3+IDAA),ISCRDA(  2+IDAA),ISCRDA(  4+IDAA))    
      CALL DACOP(ISCRDA(  4+IDAA),CORRAU1    ((2)))                     
*FOX  CORRAU1(4)=CORRAU2(4)/(ONE+CORRAU2(6)) ;                          *FOX
      CALL DACOP(CORRAU2    ((6)),ISCRDA(  1+IDAA))                     
      CALL DACAD(ISCRDA(  1+IDAA),ONE*ONE        ,ISCRDA(  2+IDAA))     
      CALL DACOP(CORRAU2    ((4)),ISCRDA(  3+IDAA))                     
      CALL DADIV(ISCRDA(  3+IDAA),ISCRDA(  2+IDAA),ISCRDA(  4+IDAA))    
      CALL DACOP(ISCRDA(  4+IDAA),CORRAU1    ((4)))                     
        xxtr(1,1) = dare(corrau2(1))
        yytr(1,1) = dare(corrau1(2))
        xxtr(1,2) = dare(corrau2(3))
        yytr(1,2) = dare(corrau1(4))
        sigm(1) = dare(corrau2(5))
        dps(1) = dare(CORRAU2(6))*C1M3                                  *FOX
*FOX  X(1)=CORRAU2(1) ;                                                 *FOX
      CALL DACOP(CORRAU2    ((1)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),X          ((1)))                     
*FOX  YP(1)=CORRAU2(2) ;                                                *FOX
      CALL DACOP(CORRAU2    ((2)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),YP         ((1)))                     
*FOX  X(2)=CORRAU2(3) ;                                                 *FOX
      CALL DACOP(CORRAU2    ((3)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),X          ((2)))                     
*FOX  YP(2)=CORRAU2(4) ;                                                *FOX
      CALL DACOP(CORRAU2    ((4)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),YP         ((2)))                     
*FOX  SIGMDA=CORRAU2(5) ;                                               *FOX
      CALL DACOP(CORRAU2    ((5)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),SIGMDA     )                          
*FOX  DPDA1=CORRAU2(6) ;                                                *FOX
      CALL DACOP(CORRAU2    ((6)),ISCRDA(  1+IDAA))                     
      CALL DACOP(ISCRDA(  1+IDAA),DPDA1      )                          
*FOX  DPDA=DPDA1*C1M3 ;                                                 *FOX
      CALL DACMU(DPDA1      ,ONE*C1M3       ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),DPDA       )                          
      endif
      if(iqmod6.ne.1) then
        do 440 i = 1,6
          jj(i)=1
          call dapek(x(1),jj,xlya6)
          if(i.eq.6) xlya6=xlya6*c1e3
          aml6(1,i) = xlya6
          call dapek(yp(1),jj,xlya6)
          if(i.eq.6) xlya6=xlya6*c1e3
          aml6(2,i) = xlya6
          call dapek(x(2),jj,xlya6)
          if(i.eq.6) xlya6=xlya6*c1e3
          aml6(3,i) = xlya6
          call dapek(yp(2),jj,xlya6)
          if(i.eq.6) xlya6=xlya6*c1e3
          aml6(4,i) = xlya6
          call dapek(sigmda,jj,xlya6)
          if(i.eq.6) xlya6=xlya6*c1e3
          aml6(5,i) = xlya6
          call dapek(dpda,jj,xlya6)
          if(i.eq.6) xlya6=xlya6*c1e3
          aml6(6,i) = xlya6
          jj(i)=0
  440   continue
      endif
      do 450 i = 1,6
        aml6(i,i) = aml6(i,i) - one
  450 continue
C--now do the output
      if(mapout.eq.1) then
        call dapri(x(1),18)
        call dapri(yp(1),18)
        call dapri(x(2),18)
        call dapri(yp(2),18)
        call dapri(sigmda,18)
        call dapri(dpda1,18)
        rewind 18
      endif
      nbeam=nbeamo
      if(idalloc.eq.2.or.idalloc.eq.3) then
        CALL DADAL(WY      ,1)                                                  
        CALL DADAL(WX      ,1)                                                  
        CALL DADAL(YY      ,1)                                                  
        CALL DADAL(XX      ,1)                                                  
        CALL DADAL(CRZBF   ,1)                                                  
        CALL DADAL(CBXBF   ,1)                                                  
        CALL DADAL(CRXBF   ,1)                                                  
        CALL DADAL(ZBBF    ,1)                                                  
        CALL DADAL(XBBF    ,1)                                                  
        CALL DADAL(ZRBF    ,1)                                                  
        CALL DADAL(XRBF    ,1)                                                  
        CALL DADAL(RKBF    ,1)                                                  
        CALL DADAL(RBF     ,1)                                                  
        CALL DADAL(R2BF    ,1)                                                  
        CALL DADAL(TKBF    ,1)                                                  
        CALL DADAL(RHO2BF  ,1)                                                  
        CALL DADAL(CIKVEBF ,1)                                                  
        CALL DADAL(CRKVEBF ,1)                                                  
        CALL DADAL(YV2J    ,1)                                                  
        CALL DADAL(YV1J    ,1)                                                  
        CALL DADAL(CBZBF   ,1)                                                  
        CALL DADAL(CRKVEUK ,1)                                                  
        CALL DADAL(CIKVE   ,1)                                                  
        CALL DADAL(CRKVE   ,1)                                                  
        CALL DADAL(ZL      ,1)                                                  
        CALL DADAL(XL      ,1)                                                  
        CALL DADAL(EKK     ,1)                                                  
        CALL DADAL(EJF0    ,1)                                                  
        CALL DADAL(PUZ     ,1)                                                  
        CALL DADAL(PUX     ,1)                                                  
        CALL DADAL(BB      ,1*(11))                                             
        CALL DADAL(AA      ,1*(11))                                             
        CALL DADAL(CORRAU2 ,1*(8))                                              
        CALL DADAL(CORRAU1 ,1*(8))                                              
        CALL DADAL(CORRNEW ,1*(8))                                              
        CALL DADAL(CORROLD ,1*(8))                                              
        CALL DADAL(DKIP    ,1)                                                  
        CALL DADAL(YP      ,1*(2))                                              
        CALL DADAL(Y       ,1*(2))                                              
        CALL DADAL(X       ,1*(2))                                              
        CALL DADAL(SMIDA   ,1*(MCOR))                                           
        CALL DADAL(ASDAQ   ,1*(2)*(6))                                          
        CALL DADAL(ALDAQ   ,1*(2)*(6))                                          
        CALL DADAL(ASDA    ,1*(2)*(6))                                          
        CALL DADAL(ALDA    ,1*(2)*(6))                                          
        CALL DADAL(EJF1    ,1)                                                  
        CALL DADAL(EJ1     ,1)                                                  
        CALL DADAL(RV      ,1)                                                  
        CALL DADAL(DPDA1   ,1)                                                  
        CALL DADAL(DPDA    ,1)                                                  
        CALL DADAL(SIGMDA  ,1)                                                  
C     DADAL AUTOMATIC INCLUSION
      endif
      return
  470 write(6,10000) j,i,xxtr(j,1),aper(1),xxtr(j,2),aper(2),ix, kz(ix),
     +bez(ix)
      nbeam=nbeamo
      if(idalloc.eq.2.or.idalloc.eq.3) then
        CALL DADAL(WY      ,1)                                                  
        CALL DADAL(WX      ,1)                                                  
        CALL DADAL(YY      ,1)                                                  
        CALL DADAL(XX      ,1)                                                  
        CALL DADAL(CRZBF   ,1)                                                  
        CALL DADAL(CBXBF   ,1)                                                  
        CALL DADAL(CRXBF   ,1)                                                  
        CALL DADAL(ZBBF    ,1)                                                  
        CALL DADAL(XBBF    ,1)                                                  
        CALL DADAL(ZRBF    ,1)                                                  
        CALL DADAL(XRBF    ,1)                                                  
        CALL DADAL(RKBF    ,1)                                                  
        CALL DADAL(RBF     ,1)                                                  
        CALL DADAL(R2BF    ,1)                                                  
        CALL DADAL(TKBF    ,1)                                                  
        CALL DADAL(RHO2BF  ,1)                                                  
        CALL DADAL(CIKVEBF ,1)                                                  
        CALL DADAL(CRKVEBF ,1)                                                  
        CALL DADAL(YV2J    ,1)                                                  
        CALL DADAL(YV1J    ,1)                                                  
        CALL DADAL(CBZBF   ,1)                                                  
        CALL DADAL(CRKVEUK ,1)                                                  
        CALL DADAL(CIKVE   ,1)                                                  
        CALL DADAL(CRKVE   ,1)                                                  
        CALL DADAL(ZL      ,1)                                                  
        CALL DADAL(XL      ,1)                                                  
        CALL DADAL(EKK     ,1)                                                  
        CALL DADAL(EJF0    ,1)                                                  
        CALL DADAL(PUZ     ,1)                                                  
        CALL DADAL(PUX     ,1)                                                  
        CALL DADAL(BB      ,1*(11))                                             
        CALL DADAL(AA      ,1*(11))                                             
        CALL DADAL(CORRAU2 ,1*(8))                                              
        CALL DADAL(CORRAU1 ,1*(8))                                              
        CALL DADAL(CORRNEW ,1*(8))                                              
        CALL DADAL(CORROLD ,1*(8))                                              
        CALL DADAL(DKIP    ,1)                                                  
        CALL DADAL(YP      ,1*(2))                                              
        CALL DADAL(Y       ,1*(2))                                              
        CALL DADAL(X       ,1*(2))                                              
        CALL DADAL(SMIDA   ,1*(MCOR))                                           
        CALL DADAL(ASDAQ   ,1*(2)*(6))                                          
        CALL DADAL(ALDAQ   ,1*(2)*(6))                                          
        CALL DADAL(ASDA    ,1*(2)*(6))                                          
        CALL DADAL(ALDA    ,1*(2)*(6))                                          
        CALL DADAL(EJF1    ,1)                                                  
        CALL DADAL(EJ1     ,1)                                                  
        CALL DADAL(RV      ,1)                                                  
        CALL DADAL(DPDA1   ,1)                                                  
        CALL DADAL(DPDA    ,1)                                                  
        CALL DADAL(SIGMDA  ,1)                                                  
C     DADAL AUTOMATIC INCLUSION
      endif
  480 continue
C-----------------------------------------------------------------------
      return
10000 format(1h /t10,'CO-TRACKING ENDED ABNORMALLY'/t10, 'PARTICLE NO. '
     +,i3,' AT ELEMENT ',i4/ t10,'HORIZ:  AMPLITUDE = ',f15.3,
     +'   APERTURE = ',f15.3/ t10,'VERT:   AMPLITUDE = ',f15.3,
     +'   APERTURE = ',f15.3/ t10,'ELEMENT - LIST NUMBER ',i4,
     +' TYP NUMBER ',i4,' NAME ',a16/)
10010 format(//t10,30(1h*)/t10,'**** ONE TURN COMPLETED ****'/ t10,30(
     +1h*)/)
10020 format(/10x,'The Preparating Calculations took',f12.3,' second(s)'
     +,' of Computing Time')
10030 format(/10x,'DA-Calculation of Order : ',i7,' took ', f12.3,
     +' second(s) of CPU Time'//131('-')//)
      end
