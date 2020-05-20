C ANFANG UNTERPROGRAMM
C-----------------------------------------------------------------------
      subroutine synoda
C-----------------------------------------------------------------------
C  SYNCHROTRON OSCILLATIONS
C        SPECIALLY PREPARED FOR NEW D.A.
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
      common/daele/alda,asda,aldaq,asdaq,smida,dpda,dpda1,sigmda,ej1,
     &ejf1,rv
      common/main4/ e0f,numx
C-----------------------------------------------------------------------
*FOX  B D ;
*FOX  D V DA EXT SIGMDA NORD NVAR ; D V DA EXT DPDA NORD NVAR ;
*FOX  D V DA EXT DPDA1 NORD NVAR ; D V DA EXT RV NORD NVAR ;
*FOX  D V DA EXT EJ1 NORD NVAR ; D V DA EXT EJF1 NORD NVAR ;
*FOX  D V DA EXT ALDA NORD NVAR 2 6 ; D V DA EXT ASDA NORD NVAR 2 6 ;
*FOX  D V DA EXT ALDAQ NORD NVAR 2 6 ; D V DA EXT ASDAQ NORD NVAR 2 6 ;
*FOX  D V DA EXT SMIDA NORD NVAR MCOR ;
*FOX  D V RE INT E0 ; D V RE INT PMA ; D V RE EXT E0F ;
*FOX  D V RE INT HSY 3 ; D V RE INT PHAS ;
*FOX  D V RE EXT ED NELE ; D V RE EXT HSYC NELE ;
*FOX  D V RE EXT PHASC NELE ;
*FOX  D V RE INT C1E3 ; D V RE INT ONE ;
*FOX  D V IN EXT ITIONC NELE ; D V IN INT ITION ; D V IN INT IX ;
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
      INTEGER LFOX0, ISCRDA, ISCRRI,IDAO
C     INTEGER LFOX1
      REAL*8 RSCRRI
      COMMON/DASCR/ISCRDA(100),RSCRRI(100),ISCRRI(100),IDAO
      save 
      DATA LFOX0   / 0 / 
      IF(LFOX0.EQ.0) THEN
         LFOX0 = 1
         CALL DAKEY('FOX V2.1')
      ENDIF
      IDAA = IDAO
*FOX}
C-----------------------------------------------------------------------
      ix=ixcav
      if(kz(ix).eq.12) then
*FOX  EJ1=EJ1+ED(IX)*SIN(HSYC(IX)*SIGMDA/C1E3*                          *FOX
*FOX  ITIONC(IX)+PHASC(IX)) ;
      RSCRRI(  1+IDAA) = HSYC       (IX         )                       
      ISCRRI(  2+IDAA) = ITIONC     (IX         )                       
      RSCRRI(  3+IDAA) = PHASC      (IX         )                       
      CALL DACMU(SIGMDA     ,ONE*RSCRRI(  1+IDAA),ISCRDA(  4+IDAA))     
      CALL DACDI(ISCRDA(  4+IDAA),ONE*C1E3       ,ISCRDA(  5+IDAA))     
      CALL DACMU(ISCRDA(  5+IDAA),ONE*ISCRRI(  2+IDAA),ISCRDA(  6+IDAA))
     *                                                                  
      CALL DACAD(ISCRDA(  6+IDAA),ONE*RSCRRI(  3+IDAA),ISCRDA(  7+IDAA))
     *                                                                  
      RSCRRI(  8+IDAA) = ED         (IX         )                       
      CALL DAFUN('SIN   ',ISCRDA(  7+IDAA),ISCRDA(  9+IDAA))            
      CALL DACMU(ISCRDA(  9+IDAA),ONE*RSCRRI(  8+IDAA),ISCRDA( 10+IDAA))
     *                                                                  
      CALL DAADD(EJ1        ,ISCRDA( 10+IDAA),ISCRDA( 11+IDAA))         
      CALL DACOP(ISCRDA( 11+IDAA),EJ1        )                          
      else
*FOX  EJ1=EJ1+HSY(1)*SIN(HSY(3)*SIGMDA/C1E3*ITION+PHAS) ;               *FOX
      RSCRRI(  1+IDAA) = HSY        ((3))                               
      CALL DACMU(SIGMDA     ,ONE*RSCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACDI(ISCRDA(  2+IDAA),ONE*C1E3       ,ISCRDA(  3+IDAA))     
      CALL DACMU(ISCRDA(  3+IDAA),ONE*ITION      ,ISCRDA(  4+IDAA))     
      CALL DACAD(ISCRDA(  4+IDAA),ONE*PHAS       ,ISCRDA(  5+IDAA))     
      RSCRRI(  6+IDAA) = HSY        ((1))                               
      CALL DAFUN('SIN   ',ISCRDA(  5+IDAA),ISCRDA(  7+IDAA))            
      CALL DACMU(ISCRDA(  7+IDAA),ONE*RSCRRI(  6+IDAA),ISCRDA(  8+IDAA))
     *                                                                  
      CALL DAADD(EJ1        ,ISCRDA(  8+IDAA),ISCRDA(  9+IDAA))         
      CALL DACOP(ISCRDA(  9+IDAA),EJ1        )                          
      endif
*FOX  EJF1=SQRT(EJ1*EJ1-PMA*PMA) ;                                      *FOX
      CALL DAMUL(EJ1        ,EJ1        ,ISCRDA(  1+IDAA))              
      RSCRRI(  2+IDAA) = PMA         * PMA                              
      CALL DACSU(ISCRDA(  1+IDAA),ONE*RSCRRI(  2+IDAA),ISCRDA(  3+IDAA))
     *                                                                  
      CALL DAFUN('SQRT  ',ISCRDA(  3+IDAA),ISCRDA(  4+IDAA))            
      CALL DACOP(ISCRDA(  4+IDAA),EJF1       )                          
*FOX  DPDA1=(EJF1-E0F)/E0F*C1E3 ;                                       *FOX
      CALL DACSU(EJF1       ,ONE*E0F        ,ISCRDA(  1+IDAA))          
      CALL DACDI(ISCRDA(  1+IDAA),ONE*E0F        ,ISCRDA(  2+IDAA))     
      CALL DACMU(ISCRDA(  2+IDAA),ONE*C1E3       ,ISCRDA(  3+IDAA))     
      CALL DACOP(ISCRDA(  3+IDAA),DPDA1      )                          
      return
      end
