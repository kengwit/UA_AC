      SUBROUTINE ERRFF(XX,YY,WX,WY,IX)
*----------------------------------------------------------------------*
* PURPOSE:                                                             *
*   MODIFICATION OF WWERF, REAL*8 COMPLEX ERROR FUNCTION,    *
*   WRITTEN AT CERN BY K. KOELBIG.                                     *
*   TAKEN FROM MAD8                                                    *
*   VERSION FOR MAP PRODUCTION USING BERZ'S DA PACKAGE                 *
* INPUT:                                                               *
*   XX, YY    (REAL)    ARGUMENT TO CERF.                              *
* OUTPUT:                                                              *
*   WX, WY    (REAL)    FUNCTION RESULT.                               *
*----------------------------------------------------------------------*
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
      PARAMETER(CC=1.12837916709551D0)
      PARAMETER(XLIM=5.33D0)
      PARAMETER(YLIM=4.29D0)
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
C-----------------------------------------------------------------------
*FOX  B D ;
*FOX  D V DA EXT XX NORD NVAR ; D V DA EXT YY NORD NVAR ;
*FOX  D V DA EXT WX NORD NVAR ; D V DA EXT WY NORD NVAR ;
*FOX  D V DA INT X NORD NVAR ; D V DA INT Y NORD NVAR ;
*FOX  D V DA INT Q NORD NVAR ; D V DA INT H NORD NVAR ;
*FOX  D V DA INT XH NORD NVAR ; D V DA INT YH NORD NVAR ;
*FOX  D V DA INT RX NORD NVAR 33 ; D V DA INT RY NORD NVAR 33 ;
*FOX  D V DA INT TX NORD NVAR ; D V DA INT TN NORD NVAR ;
*FOX  D V DA INT TY NORD NVAR ;D V DA INT SAUX NORD NVAR ;
*FOX  D V DA INT SX NORD NVAR ; D V DA INT SY NORD NVAR ;
*FOX  D V DA INT XL NORD NVAR ;
*FOX  D V RE INT XLIM ; D V RE INT YLIM ; D V RE INT ONE ;
*FOX  D V RE INT TWO ; D V RE INT CC ;
*FOX  D V IN INT NC ; D V IN INT N ; D V IN INT N1 ; D V IN INT NUU ;
*FOX  D V IN INT NUU1 ;
*FOX  D F RE DARE 1 ;
*FOX  E D ;
*FOX{
      INTEGER XX      
      INTEGER YY      
      INTEGER WX      
      INTEGER WY      
      INTEGER X       
      INTEGER Y       
      INTEGER Q       
      INTEGER H       
      INTEGER XH      
      INTEGER YH      
      INTEGER RX      (33)
      INTEGER RY      (33)
      INTEGER TX      
      INTEGER TN      
      INTEGER TY      
      INTEGER SAUX    
      INTEGER SX      
      INTEGER SY      
      INTEGER XL      
      INTEGER ISCRDA, ISCRRI,IDAO
C     INTEGER LFOX0, LFOX1
      REAL*8 RSCRRI
      COMMON/DASCR/ISCRDA(100),RSCRRI(100),ISCRRI(100),IDAO
      save
      if(idalloc.eq.1.or.idalloc.eq.3) then                           
         CALL DAKEY('FOX V2.1')
         CALL DAALL(X       ,1,'X         ',NORD,NVAR)
         CALL DAALL(Y       ,1,'Y         ',NORD,NVAR)
         CALL DAALL(Q       ,1,'Q         ',NORD,NVAR)
         CALL DAALL(H       ,1,'H         ',NORD,NVAR)
         CALL DAALL(XH      ,1,'XH        ',NORD,NVAR)
         CALL DAALL(YH      ,1,'YH        ',NORD,NVAR)
         CALL DAALL(RX      ,1*(33),'RX        ',NORD,NVAR)
         CALL DAALL(RY      ,1*(33),'RY        ',NORD,NVAR)
         CALL DAALL(TX      ,1,'TX        ',NORD,NVAR)
         CALL DAALL(TN      ,1,'TN        ',NORD,NVAR)
         CALL DAALL(TY      ,1,'TY        ',NORD,NVAR)
         CALL DAALL(SAUX    ,1,'SAUX      ',NORD,NVAR)
         CALL DAALL(SX      ,1,'SX        ',NORD,NVAR)
         CALL DAALL(SY      ,1,'SY        ',NORD,NVAR)
         CALL DAALL(XL      ,1,'XL        ',NORD,NVAR)
      ENDIF
      IDAA = IDAO
*FOX}
C-----------------------------------------------------------------------
      if(idalloc.eq.1) idalloc=0
      if(idalloc.eq.3) idalloc=2
c*FOX  X=ABS(XX) ;                                                      *FOX
*FOX  X=XX ;                                                            *FOX
      CALL DACOP(XX         ,X          )                               
      if(dare(x).lt.zero) then
*FOX  X=-X ;                                                            *FOX
      CALL DACMU(X          ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),X          )                          
      endif
c*FOX  Y=ABS(YY) ;                                                      *FOX
*FOX  Y=YY ;                                                            *FOX
      CALL DACOP(YY         ,Y          )                               
      if(dare(y).lt.zero) then
*FOX  Y=-Y ;                                                            *FOX
      CALL DACMU(Y          ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),Y          )                          
      endif
      if(dare(y).lt.ylim.and.dare(x).lt.xlim) then
*FOX  Q=(ONE-Y/YLIM)*SQRT(ONE-(X/XLIM)**2) ;                            *FOX
      CALL DACDI(X          ,ONE*XLIM       ,ISCRDA(  1+IDAA))          
      CALL DAEXC(ISCRDA(  1+IDAA),ONE*(2          ),ISCRDA(  2+IDAA))   
      CALL DACDI(Y          ,ONE*YLIM       ,ISCRDA(  3+IDAA))          
      CALL DASUC(ISCRDA(  3+IDAA),ONE*ONE        ,ISCRDA(  4+IDAA))     
      CALL DASUC(ISCRDA(  2+IDAA),ONE*ONE        ,ISCRDA(  5+IDAA))     
      CALL DAFUN('SQRT  ',ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))            
      CALL DAMUL(ISCRDA(  4+IDAA),ISCRDA(  6+IDAA),ISCRDA(  7+IDAA))    
      CALL DACOP(ISCRDA(  7+IDAA),Q          )                          
*FOX  H=ONE/(3.2D0*Q) ;                                                 *FOX
      CALL DACMU(Q          ,ONE*(3.2D0      ),ISCRDA(  1+IDAA))        
      CALL DADIC(ISCRDA(  1+IDAA),ONE*ONE        ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),H          )                          
      nc=7+int(23.0*dare(q))
*FOX  XL=H**(1-NC) ;                                                    *FOX
      ISCRRI(  1+IDAA) = (1          ) - NC                             
      CALL DAEXC(H          ,ONE*ISCRRI(  1+IDAA),ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),XL         )                          
*FOX  XH=Y+0.5D0/H ;                                                    *FOX
      CALL DADIC(H          ,ONE*(0.5D0      ),ISCRDA(  1+IDAA))        
      CALL DAADD(Y          ,ISCRDA(  1+IDAA),ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),XH         )                          
*FOX  YH=X ;                                                            *FOX
      CALL DACOP(X          ,YH         )                               
      nuu=10+int(21.0*dare(q))
      nuu1=nuu+1
*FOX  RX(NUU1)=0. ;                                                     *FOX
      RSCRRI(100) = (0.         )                                       
      CALL DACON(RX         (NUU1       ),RSCRRI(100))                  
*FOX  RY(NUU1)=0. ;                                                     *FOX
      RSCRRI(100) = (0.         )                                       
      CALL DACON(RY         (NUU1       ),RSCRRI(100))                  
        do 10 n=nuu,1,-1
      n1=n+1
*FOX  TX=XH+N*RX(N1) ;                                                  *FOX
      CALL DACOP(RX         (N1         ),ISCRDA(  1+IDAA))             
      CALL DACMU(ISCRDA(  1+IDAA),ONE*N          ,ISCRDA(  2+IDAA))     
      CALL DAADD(XH         ,ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),TX         )                          
*FOX  TY=YH-N*RY(N1) ;                                                  *FOX
      CALL DACOP(RY         (N1         ),ISCRDA(  1+IDAA))             
      CALL DACMU(ISCRDA(  1+IDAA),ONE*N          ,ISCRDA(  2+IDAA))     
      CALL DASUB(YH         ,ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),TY         )                          
*FOX  TN=TX*TX+TY*TY ;                                                  *FOX
      CALL DAMUL(TX         ,TX         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(TY         ,TY         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),TN         )                          
*FOX  RX(N)=0.5D0*TX/TN ;                                               *FOX
      CALL DACMU(TX         ,ONE*(0.5D0      ),ISCRDA(  1+IDAA))        
      CALL DADIV(ISCRDA(  1+IDAA),TN         ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),RX         (N          ))             
*FOX  RY(N)=0.5D0*TY/TN ;                                               *FOX
      CALL DACMU(TY         ,ONE*(0.5D0      ),ISCRDA(  1+IDAA))        
      CALL DADIV(ISCRDA(  1+IDAA),TN         ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),RY         (N          ))             
   10   continue
*FOX  SX=0. ;                                                           *FOX
      RSCRRI(100) = (0.         )                                       
      CALL DACON(SX         ,RSCRRI(100))                               
*FOX  SY=0. ;                                                           *FOX
      RSCRRI(100) = (0.         )                                       
      CALL DACON(SY         ,RSCRRI(100))                               
        DO 20 N=NC,1,-1
*FOX  SAUX=SX+XL ;                                                      *FOX
      CALL DAADD(SX         ,XL         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),SAUX       )                          
*FOX  SX=RX(N)*SAUX-RY(N)*SY ;                                          *FOX
      CALL DACOP(RX         (N          ),ISCRDA(  1+IDAA))             
      CALL DACOP(RY         (N          ),ISCRDA(  2+IDAA))             
      CALL DAMUL(ISCRDA(  1+IDAA),SAUX       ,ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),SY         ,ISCRDA(  4+IDAA))         
      CALL DASUB(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(ISCRDA(  5+IDAA),SX         )                          
*FOX  SY=RX(N)*SY+RY(N)*SAUX ;                                          *FOX
      CALL DACOP(RX         (N          ),ISCRDA(  1+IDAA))             
      CALL DACOP(RY         (N          ),ISCRDA(  2+IDAA))             
      CALL DAMUL(ISCRDA(  1+IDAA),SY         ,ISCRDA(  3+IDAA))         
      CALL DAMUL(ISCRDA(  2+IDAA),SAUX       ,ISCRDA(  4+IDAA))         
      CALL DAADD(ISCRDA(  3+IDAA),ISCRDA(  4+IDAA),ISCRDA(  5+IDAA))    
      CALL DACOP(ISCRDA(  5+IDAA),SY         )                          
*FOX  XL=H*XL ;                                                         *FOX
      CALL DAMUL(H          ,XL         ,ISCRDA(  1+IDAA))              
      CALL DACOP(ISCRDA(  1+IDAA),XL         )                          
   20   CONTINUE
*FOX  WX=CC*SX ;                                                        *FOX
      CALL DACMU(SX         ,ONE*CC         ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),WX         )                          
*FOX  WY=CC*SY ;                                                        *FOX
      CALL DACMU(SY         ,ONE*CC         ,ISCRDA(  1+IDAA))          
      CALL DACOP(ISCRDA(  1+IDAA),WY         )                          
      else
*FOX  XH=Y ;                                                            *FOX
      CALL DACOP(Y          ,XH         )                               
*FOX  YH=X ;                                                            *FOX
      CALL DACOP(X          ,YH         )                               
*FOX  RX(1)=0. ;                                                        *FOX
      RSCRRI(100) = (0.         )                                       
      CALL DACON(RX         ((1)),RSCRRI(100))                          
*FOX  RY(1)=0. ;                                                        *FOX
      RSCRRI(100) = (0.         )                                       
      CALL DACON(RY         ((1)),RSCRRI(100))                          
        do 30 n=9,1,-1
*FOX  TX=XH+N*RX(1) ;                                                   *FOX
      CALL DACOP(RX         ((1)),ISCRDA(  1+IDAA))                     
      CALL DACMU(ISCRDA(  1+IDAA),ONE*N          ,ISCRDA(  2+IDAA))     
      CALL DAADD(XH         ,ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),TX         )                          
*FOX  TY=YH-N*RY(1) ;                                                   *FOX
      CALL DACOP(RY         ((1)),ISCRDA(  1+IDAA))                     
      CALL DACMU(ISCRDA(  1+IDAA),ONE*N          ,ISCRDA(  2+IDAA))     
      CALL DASUB(YH         ,ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))         
      CALL DACOP(ISCRDA(  3+IDAA),TY         )                          
*FOX  TN=TX*TX+TY*TY ;                                                  *FOX
      CALL DAMUL(TX         ,TX         ,ISCRDA(  1+IDAA))              
      CALL DAMUL(TY         ,TY         ,ISCRDA(  2+IDAA))              
      CALL DAADD(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))    
      CALL DACOP(ISCRDA(  3+IDAA),TN         )                          
*FOX  RX(1)=0.5D0*TX/TN ;                                               *FOX
      CALL DACMU(TX         ,ONE*(0.5D0      ),ISCRDA(  1+IDAA))        
      CALL DADIV(ISCRDA(  1+IDAA),TN         ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),RX         ((1)))                     
*FOX  RY(1)=0.5D0*TY/TN ;                                               *FOX
      CALL DACMU(TY         ,ONE*(0.5D0      ),ISCRDA(  1+IDAA))        
      CALL DADIV(ISCRDA(  1+IDAA),TN         ,ISCRDA(  2+IDAA))         
      CALL DACOP(ISCRDA(  2+IDAA),RY         ((1)))                     
   30   continue
*FOX  WX=CC*RX(1) ;                                                     *FOX
      CALL DACOP(RX         ((1)),ISCRDA(  1+IDAA))                     
      CALL DACMU(ISCRDA(  1+IDAA),ONE*CC         ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),WX         )                          
*FOX  WY=CC*RY(1) ;                                                     *FOX
      CALL DACOP(RY         ((1)),ISCRDA(  1+IDAA))                     
      CALL DACMU(ISCRDA(  1+IDAA),ONE*CC         ,ISCRDA(  2+IDAA))     
      CALL DACOP(ISCRDA(  2+IDAA),WY         )                          
      endif
      if(dare(y).eq.0.) then
*FOX  WX=EXP(-X*X) ;                                                    *FOX
      CALL DACMU(X          ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DAMUL(ISCRDA(  1+IDAA),X          ,ISCRDA(  2+IDAA))         
      CALL DAFUN('EXP   ',ISCRDA(  2+IDAA),ISCRDA(  3+IDAA))            
      CALL DACOP(ISCRDA(  3+IDAA),WX         )                          
      endif
      if(dare(yy).lt.0.) then
*FOX  WX=TWO*EXP(Y*Y-X*X)*COS(TWO*X*Y)-WX ;                             *FOX
      CALL DAMUL(Y          ,Y          ,ISCRDA(  1+IDAA))              
      CALL DAMUL(X          ,X          ,ISCRDA(  2+IDAA))              
      CALL DACMU(X          ,ONE*TWO        ,ISCRDA(  3+IDAA))          
      CALL DAMUL(ISCRDA(  3+IDAA),Y          ,ISCRDA(  4+IDAA))         
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DAFUN('EXP   ',ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))            
      CALL DAFUN('COS   ',ISCRDA(  4+IDAA),ISCRDA(  7+IDAA))            
      CALL DACMU(ISCRDA(  6+IDAA),ONE*TWO        ,ISCRDA(  8+IDAA))     
      CALL DAMUL(ISCRDA(  8+IDAA),ISCRDA(  7+IDAA),ISCRDA(  9+IDAA))    
      CALL DASUB(ISCRDA(  9+IDAA),WX         ,ISCRDA( 10+IDAA))         
      CALL DACOP(ISCRDA( 10+IDAA),WX         )                          
*FOX  WY=-TWO*EXP(Y*Y-X*X)*SIN(TWO*X*Y)-WY ;                            *FOX
      CALL DAMUL(Y          ,Y          ,ISCRDA(  1+IDAA))              
      CALL DAMUL(X          ,X          ,ISCRDA(  2+IDAA))              
      CALL DACMU(X          ,ONE*TWO        ,ISCRDA(  3+IDAA))          
      CALL DAMUL(ISCRDA(  3+IDAA),Y          ,ISCRDA(  4+IDAA))         
      CALL DASUB(ISCRDA(  1+IDAA),ISCRDA(  2+IDAA),ISCRDA(  5+IDAA))    
      CALL DAFUN('EXP   ',ISCRDA(  5+IDAA),ISCRDA(  6+IDAA))            
      CALL DAFUN('SIN   ',ISCRDA(  4+IDAA),ISCRDA(  7+IDAA))            
      RSCRRI(  8+IDAA) = (-ONE       ) * TWO                            
      CALL DACMU(ISCRDA(  6+IDAA),ONE*RSCRRI(  8+IDAA),ISCRDA(  9+IDAA))
     *                                                                  
      CALL DAMUL(ISCRDA(  9+IDAA),ISCRDA(  7+IDAA),ISCRDA( 10+IDAA))    
      CALL DASUB(ISCRDA( 10+IDAA),WY         ,ISCRDA( 11+IDAA))         
      CALL DACOP(ISCRDA( 11+IDAA),WY         )                          
        if(dare(xx).gt.0.) then
*FOX  WY=-WY ;                                                          *FOX
      CALL DACMU(WY         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),WY         )                          
        endif
      else
        if(dare(xx).lt.0.) then
*FOX  WY=-WY ;                                                          *FOX
      CALL DACMU(WY         ,ONE*(-ONE       ),ISCRDA(  1+IDAA))        
      CALL DACOP(ISCRDA(  1+IDAA),WY         )                          
        endif
      endif
      if(IDALLOC.eq.2.and.ix.eq.nbeam) then
        CALL DADAL(XL      ,1)                                                  
        CALL DADAL(SY      ,1)                                                  
        CALL DADAL(SX      ,1)                                                  
        CALL DADAL(SAUX    ,1)                                                  
        CALL DADAL(TY      ,1)                                                  
        CALL DADAL(TN      ,1)                                                  
        CALL DADAL(TX      ,1)                                                  
        CALL DADAL(RY      ,1*(33))                                             
        CALL DADAL(RX      ,1*(33))                                             
        CALL DADAL(YH      ,1)                                                  
        CALL DADAL(XH      ,1)                                                  
        CALL DADAL(H       ,1)                                                  
        CALL DADAL(Q       ,1)                                                  
        CALL DADAL(Y       ,1)                                                  
        CALL DADAL(X       ,1)                                                  
C     DADAL AUTOMATIC INCLUSION
      endif
      end
