C ANFANG UNTERPROGRAMM
      subroutine ambm6(no,iham)
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
*****************************************************************
* COMPUTES THE FACTORS USED IN RESB OR RESBS FOR THE COMPUTATION*
* OF THE CANONICAL TRANSFORMATION.                              *
* THE USER WILL MODIFY THIS SUBROUTINE TO LEAVE SOME RESONANCES,*
* AT PRESENT THE TARGET MAP IS AN AMPLITUDE DEPENDENT ROTATION  *
*****************************************************************
      parameter (ntt=20)
      parameter (ndim=3)
      common /tune/ angle(ndim),abm(-ntt:ntt,-ntt:ntt,-ntt:ntt)
     @,aam(-ntt:ntt,-ntt:ntt,-ntt:ntt),tue(-ntt:ntt,-ntt:ntt,-ntt:ntt)
      common /stable/sta(ndim),dsta(ndim)
      dimension mx(ndim,20)
      save
      nres=0
      do 40 i=-no,no
        do 30 j=-no,no
          do 20 k=-no,no
            abm(i,j,k)=zero
            aam(i,j,k)=zero
            tue(i,j,k)=one
            if(nres.eq.-1) goto 20
            if(abs(i)+abs(j)+abs(k).gt.no) goto 20
            if(abs(i)+abs(j)+abs(k).ne.0)then
              ires=0
              do 10 ic=1,nres
                if(mx(1,ic).eq.i.and.mx(2,ic).eq.j.and.mx(3,ic).eq.k)
     +          ires=1
                if(mx(1,ic).eq.-i.and.mx(2,ic).eq.-j.and.mx(3,ic).eq.-k)
     +          ires=1
                if(ires.eq.1) goto 20
   10         continue
              an1=i*angle(1)
              an2=j*angle(2)
              an3=k*angle(3)
              ad1=an1*dsta(1)
              ad2=an2*dsta(2)
              ad3=an3*dsta(3)
              ad=ad1+ad2+ad3
              as1=an1*sta(1)
              as2=an2*sta(2)
              as3=an3*sta(3)
              as=as1+as2+as3
* IHAM = 1 FOR CALCULATION ON THE PSEUDO-HAMILTONIAN
              if(iham.ne.1) then
                co=cos(as)
                exh=exp(ad/two)
                ex=exh**2
                ans=four*ex*(sinh(ad/two)**2+sin(as/two)**2)
                aam(i,j,k)=two*(-exh*sinh(ad/two)+ex*sin(as/two)**2)
     +          /ans
                abm(i,j,k)=sin(as)*ex/ans
                tue(i,j,k)=zero
              else
                ans=ad**2+as**2
                abm(i,j,k)=as/ans
                aam(i,j,k)=-ad/ans
                tue(i,j,k)=zero
              endif
            endif
   20     continue
   30   continue
   40 continue
      return
      end
