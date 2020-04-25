      subroutine wzsubv(napx,vx,vy,vu,vv)
      implicit real*8 (a-h,o-z)
      parameter(npart=64,nmac=1)
      parameter(nele=700,nblo=300,nper=16,
     &nelb=100,nblz=20000,nzfz=300000,mmul=11)
      parameter(nran=280000,ncom=100,mran=500,mpa=6,nrco=5,nema=15)
      parameter(mcor=10)
      parameter(npos=20000,nlya=10000,ninv=1000,nplo=20000)
      parameter(nmon1=600,ncor1=600)
c
c  *** Note.  The values of nx and ny in the following parameter state-
c  ment must be the same as those in subroutine WZSET.
      parameter ( xcut=7.77d0, ycut=7.46d0 )
      parameter ( h=1.d0/63.d0 )
      parameter ( nx=490, ny=470 )
      parameter ( idin = (nx+2)*(ny+2) )
      parameter ( half=0.5d0, one=1.d0 )
      common /wzcom1/ hrecip, kstep
      common /wzcom2/ wtreal(idin), wtimag(idin)
      integer vmu,vnu
c
      parameter ( a1=0.5124242248d0, a2=0.0517653588d0 )
      parameter ( b1=0.2752551286d0, b2=2.7247448714d0 )
 
c     temporary arrays to facilitate vectorisation
      dimension vx(npart),vy(npart),vu(npart),vv(npart)
      dimension vsreal(npart),vsimag(npart),vp(npart),vq(npart)
      dimension vqsq(npart),vt(npart),vr(npart)
      dimension vxh(npart),vyh(npart),vmu(npart),vnu(npart)
      dimension vw4r(npart),vw4i(npart)
      dimension vw3r(npart),vw3i(npart),vd34r(npart),vd34i(npart)
      dimension vw2r(npart),vw2i(npart),vd23r(npart),vd23i(npart)
      dimension vtr(npart),vti(npart),vtdd24r(npart),vtdd24i(npart)
      dimension vw1r(npart),vw1i(npart)
      dimension vd12r(npart),vd12i(npart),vtdd13r(npart),vtdd13i(npart)
      dimension vtdddr(npart),vtdddi(npart),vxhrel(npart),vyhrel(npart)
      dimension vusum(npart),vvsum(npart),vusum3(npart),vvsum3(npart)
C     dimension vusum4(npart),vvsum4(npart)
      save
c
c  *********************************************************************
c
c  This subroutine sets u=real(w(z)) and v=imag(w(z)), where z=x+i*y and
c  where w(z) is the complex error function defined by formula 7.1.3 in
c  "Handbook of Mathematical functions [eds. M.Abramowitz & I.A.Stegun,
c  Washington, 1966].  The absolute error of the computed value is less
c  than 1E-8.
c
c  *** Note.  Subroutine WZSET must have been called before this sub-
c  routine can be used.
c
c  For (x,y) inside the rectangle with opposite corners (xcut,0) and
c  (0,ycut), where xcut and ycut have been set by WZSET, an interpo-
c  lation formula is used.  For (x,y) outside this rectangle, a two-
c  term rational approximation is used.
c
c  (G.A.Erskine, 29.09.1997)
c
c  Vectorised for up to 64 argument values by E.McIntosh, 30.10.1997.
c
c
c  Third-order divided-difference interpolation over the corners of a
c  square [e.g. formula (2.5.1) in "Introduction to Numerical Analysis"
c  (F.B.Hildebrand New York, 1957), but with complex nodes and
c  function values].
c
c  In the interpolation formula the corners of the grid square contain-
c  ing (x,y) are numbered (0,0)=3, (h,0)=4, (h,h)=1, (0,h)=2.
c  Identifiers d, dd and ddd denote divided-differences of orders 1, 2
c  and 3 respectively, and a preceding 't' indicates twice the value.
c
c
c  Two-term rational approximation to w(z) [Footnote to Table 7.9
c  in "Handbook of Mathematical Functions (eds. M.Abramowitz &
c  I.A.Stegun, Washington, 1966), but with additional digits in
c  the constants]:
c              u+i*v = i*z*( a1/(z**2-b1) + a2/(z**2-b2) ).
c  Maximum absolute error:
c        <1.E-6  for  x>=4.9  or  y>=4.4
c        <1.E-7  for  x>=6.1  or  y>=5.7
c        <1.E-8  for  x>=7.8  or  y>=7.5
c
c  *********************************************************************
c
c  Start.
      ss=0d0
      do 1 j=1,napx
c     if ( vx.ge.xcut .or. vy.ge.ycut )
        ss=ss+sign(1.0d0,
     &        sign(1.0d0,vx(j)-xcut)+sign(1.0d0,vy(j)-ycut))
    1 continue
c
      if (idnint(ss).eq.napx) then
c     everything outside the rectangle so approximate
 
        do 2 j=1,napx
 
      vp(j)=vx(j)**2-vy(j)**2
      vq(j)=2.d0*vx(j)*vy(j)
      vqsq(j)=vq(j)**2
c  First term.
      vt(j)=vp(j)-b1
      vr(j)=a1/(vt(j)**2+vqsq(j))
      vsreal(j)=vr(j)*vt(j)
      vsimag(j)=-vr(j)*vq(j)
c  Second term
      vt(j)=vp(j)-b2
      vr(j)=a2/(vt(j)**2+vqsq(j))
      vsreal(j)=vsreal(j)+vr(j)*vt(j)
      vsimag(j)=vsimag(j)-vr(j)*vq(j)
c  Multiply by i*z.
      vu(j)=-(vy(j)*vsreal(j)+vx(j)*vsimag(j))
      vv(j)=vx(j)*vsreal(j)-vy(j)*vsimag(j)
    2   continue
 
      elseif (idnint(ss).ne.-napx) then
c     we have a mixture
 
        do 3 j=1,napx
 
          if ( vx(j).ge.xcut .or. vy(j).ge.ycut ) then
 
      vp(j)=vx(j)**2-vy(j)**2
      vq(j)=2.d0*vx(j)*vy(j)
      vqsq(j)=vq(j)**2
c  First term.
      vt(j)=vp(j)-b1
      vr(j)=a1/(vt(j)**2+vqsq(j))
      vsreal(j)=vr(j)*vt(j)
      vsimag(j)=-vr(j)*vq(j)
c  Second term
      vt(j)=vp(j)-b2
      vr(j)=a2/(vt(j)**2+vqsq(j))
      vsreal(j)=vsreal(j)+vr(j)*vt(j)
      vsimag(j)=vsimag(j)-vr(j)*vq(j)
c  Multiply by i*z.
      vu(j)=-(vy(j)*vsreal(j)+vx(j)*vsimag(j))
      vv(j)=vx(j)*vsreal(j)-vy(j)*vsimag(j)
 
          else
 
      vxh(j) = hrecip*vx(j)
      vyh(j) = hrecip*vy(j)
      vmu(j) = int(vxh(j))
      vnu(j) = int(vyh(j))
c  Compute divided differences.
      k = 2 + vmu(j) + vnu(j)*kstep
      vw4r(j) = wtreal(k)
      vw4i(j) = wtimag(k)
      k = k - 1
      vw3r(j) = wtreal(k)
      vw3i(j) = wtimag(k)
      vd34r(j) = vw4r(j) - vw3r(j)
      vd34i(j) = vw4i(j) - vw3i(j)
      k = k + kstep
      vw2r(j) = wtreal(k)
      vw2i(j) = wtimag(k)
      vd23r(j) = vw2i(j) - vw3i(j)
      vd23i(j) = vw3r(j) - vw2r(j)
      vtr(j) = vd23r(j) - vd34r(j)
      vti(j) = vd23i(j) - vd34i(j)
      vtdd24r(j) = vti(j) - vtr(j)
      vtdd24i(j) = - ( vtr(j) + vti(j) )
      k = k + 1
      vw1r(j) = wtreal(k)
      vw1i(j) = wtimag(k)
      vd12r(j) = vw1r(j) - vw2r(j)
      vd12i(j) = vw1i(j) - vw2i(j)
      vtr(j) = vd12r(j) - vd23r(j)
      vti(j) = vd12i(j) - vd23i(j)
      vtdd13r(j) = vtr(j) + vti(j)
      vtdd13i(j) = vti(j) - vtr(j)
      vtdddr(j) = vtdd13i(j) - vtdd24i(j)
      vtdddi(j) = vtdd24r(j) - vtdd13r(j)
c  Evaluate polynomial.
      vxhrel(j) = vxh(j) - dble(vmu(j))
      vyhrel(j) = vyh(j) - dble(vnu(j))
      vusum3(j)=half*(vtdd13r(j)+
     +          (vxhrel(j)*vtdddr(j)-vyhrel(j)*vtdddi(j)))
      vvsum3(j)=half*(vtdd13i(j)+
     +          (vxhrel(j)*vtdddi(j)+vyhrel(j)*vtdddr(j)))
      vyhrel(j) = vyhrel(j) - one
      vusum(j)=vd12r(j)+(vxhrel(j)*vusum3(j)-vyhrel(j)*vvsum3(j))
      vvsum(j)=vd12i(j)+(vxhrel(j)*vvsum3(j)+vyhrel(j)*vusum3(j))
      vxhrel(j) = vxhrel(j) - one
      vu(j)=vw1r(j)+(vxhrel(j)*vusum(j)-vyhrel(j)*vvsum(j))
      vv(j)=vw1i(j)+(vxhrel(j)*vvsum(j)+vyhrel(j)*vusum(j))
 
          endif
 
    3   continue
 
      else
c     everything inside the square, so interpolate
 
        do 4 j=1,napx
 
      vxh(j) = hrecip*vx(j)
      vyh(j) = hrecip*vy(j)
      vmu(j) = int(vxh(j))
      vnu(j) = int(vyh(j))
c  Compute divided differences.
      k = 2 + vmu(j) + vnu(j)*kstep
      vw4r(j) = wtreal(k)
      vw4i(j) = wtimag(k)
      k = k - 1
      vw3r(j) = wtreal(k)
      vw3i(j) = wtimag(k)
      vd34r(j) = vw4r(j) - vw3r(j)
      vd34i(j) = vw4i(j) - vw3i(j)
      k = k + kstep
      vw2r(j) = wtreal(k)
      vw2i(j) = wtimag(k)
      vd23r(j) = vw2i(j) - vw3i(j)
      vd23i(j) = vw3r(j) - vw2r(j)
      vtr(j) = vd23r(j) - vd34r(j)
      vti(j) = vd23i(j) - vd34i(j)
      vtdd24r(j) = vti(j) - vtr(j)
      vtdd24i(j) = - ( vtr(j) + vti(j) )
      k = k + 1
      vw1r(j) = wtreal(k)
      vw1i(j) = wtimag(k)
      vd12r(j) = vw1r(j) - vw2r(j)
      vd12i(j) = vw1i(j) - vw2i(j)
      vtr(j) = vd12r(j) - vd23r(j)
      vti(j) = vd12i(j) - vd23i(j)
      vtdd13r(j) = vtr(j) + vti(j)
      vtdd13i(j) = vti(j) - vtr(j)
      vtdddr(j) = vtdd13i(j) - vtdd24i(j)
      vtdddi(j) = vtdd24r(j) - vtdd13r(j)
c  Evaluate polynomial.
      vxhrel(j) = vxh(j) - dble(vmu(j))
      vyhrel(j) = vyh(j) - dble(vnu(j))
      vusum3(j)=half*(vtdd13r(j)+
     +          (vxhrel(j)*vtdddr(j)-vyhrel(j)*vtdddi(j)))
      vvsum3(j)=half*(vtdd13i(j)+
     +          (vxhrel(j)*vtdddi(j)+vyhrel(j)*vtdddr(j)))
      vyhrel(j) = vyhrel(j) - one
      vusum(j)=vd12r(j)+(vxhrel(j)*vusum3(j)-vyhrel(j)*vvsum3(j))
      vvsum(j)=vd12i(j)+(vxhrel(j)*vvsum3(j)+vyhrel(j)*vusum3(j))
      vxhrel(j) = vxhrel(j) - one
      vu(j)=vw1r(j)+(vxhrel(j)*vusum(j)-vyhrel(j)*vvsum(j))
      vv(j)=vw1i(j)+(vxhrel(j)*vvsum(j)+vyhrel(j)*vusum(j))
    4   continue
      endif
      return
      end
