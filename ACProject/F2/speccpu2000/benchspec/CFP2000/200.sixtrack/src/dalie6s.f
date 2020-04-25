C ANFANG - UNTERPROGRAMM -
      subroutine dalie6s(iqmod6,nz,wx,cor6d)
      implicit real*8 (a-h,o-z)
      parameter(pieni=1d-17)
      parameter(zero=0.0d0,half=5.0d-1,one=1.0d0)
      parameter(halfm=-5.0d-1,onem=-1.0d0)
      parameter(two=2.0d0,three=3.0d0,four=4.0d0)
      parameter(c1e3=1.0d3,c2e3=2.0d3,c4e3=4.0d3,c1e4=1.0d4)
      parameter(c1e12=1.0d12,c1e13=1.0d13,c1e15=1.0d15,c1e16=1.0d16)
      parameter(c180e0=180.0d0,c1e6=1.0d6)
      parameter(c1m1=1.0d-1,c1m3=1.0d-3,c1m6=1.0d-6,c1m7=1.0d-7)
      parameter(c1m8=1.0d-8,c1m9=1.0d-9,c1m10=1.0d-10)
      parameter(c1m12=1.0d-12,c1m13=1.0d-13,c1m14=1.0d-14)
      parameter(c1m15=1.0d-15,c1m17=1.0d-17,c1m18=1.0d-18)
      parameter(c1m21=1.0d-21,c1m24=1.0d-24,c1m38=1.0d-38)
      parameter(five=5.0d0,six=6.0d0,seven=7.0d0,eight=8.0d0)
      parameter(nine=9.0d0,ten=10.0d0)
      parameter(c24e0=24.0d0,c120e0=120.0d0,c16e0=16.0d0,c40e0=40.0d0)
      parameter(c80e0=80.0d0,c72e0=72.0d0)
      parameter(c12e0=12.0d0,c32e0=32.0d0,c48e0=48.0d0,c160e0=160.0d0)
      parameter(pmap=938.2723128d0,pmae=.5109990615d0)
      parameter(crade=2.8179409238d-15)
      parameter (ndim=3)
      parameter (ndim2=6)
      parameter (nt=12)
      parameter (ntt=20)
      dimension wx(ndim),cor6d(2,2)
      common/tasm/tasm(6,6)
      common /ii/no,nv
      real*8 rscrri
      common/dascr/iscrda(100),rscrri(100),iscrri(100),idao
      common /tune/ angle(ndim),abm(-ntt:ntt,-ntt:ntt,-ntt:ntt)
     @,aam(-ntt:ntt,-ntt:ntt,-ntt:ntt),tue(-ntt:ntt,-ntt:ntt,-ntt:ntt)
      save
      x2pi=atan(one)*eight
      noo=no
      nvo=nv
      no=nz
      if(iqmod6.ne.1) then
        nv=6
      else
        nv=8
      endif
      write(6,*) '   ---- ENTRY DALIE ----'
      write(6,*) 'TUNES : WX,WY,WT'
      write(6,9913) (wx(i),i=1,ndim)
9913  format(3F24.16)
      write(6,*) 'ORDER, # VARIABLES'
      write(6,9914) no,nv
9914  format(2I8)
      do 10 i=1,ndim
        angle(i)=wx(i)*x2pi
   10 continue
      call daini(no,nv,0)
      call daall(iscrda,100,'$$IS      ',no,nv)
      eps=1.d-38
      call daeps(eps)
      call mainia6(iqmod6,eps,cor6d)
      do 20 i=1,ndim
        wx(i)=angle(i)/x2pi
   20 continue
      if(abs(wx(3)).gt.half) wx(3)=abs(abs(wx(3))-one)
      no=noo
      nv=nvo
      return
      end
