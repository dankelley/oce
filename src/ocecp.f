      subroutine cp_driver(S, T, p, n, cp)
      double precision S(n), T(n), p(n), cp(n)
      do i = 1, n
         call ocecp(S(i), T(i), p(i), cp(i))
      end do
      return
      end


c n fofonoff & r millard
      subroutine ocecp(s,t,p0,CP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c ****************************
c units:
c       pressure        p0       decibars
c       temperature     t        deg celsius (ipts-68)
c       salinity        s        (ipss-78)
c       specific heat   cpsw     j/(kg deg c)
c********************************************************
c ref: millero et al,1973,jgr,78,4499-4507
c       millero et al, unesco report no. 38 1981 pp. 99-188.
c pressure variation from least squares polynomial
c developed by fofonoff 1980.
c check value: cpsw = 3849.500 j/(kg deg. c) for s = 40 (ipss-78),
c t = 40 deg c, p0= 10000 decibars
c   scale pressure to bars
      p=p0/10.
c**************************
c sqrt salinity for fractional terms
      sr = sqrt(abs(s))
c specific heat cp0 for p=0 (millero et al ,unesco 1981)
      a = (-1.38385e-3*t+0.1072763)*t-7.643575
      b = (5.148e-5*t-4.07718e-3)*t+0.1770383
      c = (((2.093236e-5*t-2.654387e-3)*t+0.1412855)*t
     x    -3.720283)*t+4217.4
      cp0 = (b*sr + a)*s + c
c cp1 pressure and temperature terms for s = 0
      a = (((1.7168e-8*t+2.0357e-6)*t-3.13885e-4)*t+1.45747e-2)*t
     x   -0.49592
      b = (((2.2956e-11*t-4.0027e-9)*t+2.87533e-7)*t-1.08645e-5)*t
     x   +2.4931e-4
      c = ((6.136e-13*t-6.5637e-11)*t+2.6380e-9)*t-5.422e-8
      cp1 = ((c*p+b)*p+a)*p
c cp2 pressure and temperature terms for s > 0
      a = (((-2.9179e-10*t+2.5941e-8)*t+9.802e-7)*t-1.28315e-4)*t
     x   +4.9247e-3
      b = (3.122e-8*t-1.517e-6)*t-1.2331e-4
      a = (a+b*sr)*s
      b = ((1.8448e-11*t-2.3905e-9)*t+1.17054e-7)*t-2.9558e-6
      b = (b+9.971e-8*sr)*s
      c = (3.513e-13*t-1.7682e-11)*t+5.540e-10
      c = (c-1.4300e-12*t*sr)*s
      cp2 = ((c*p+b)*p+a)*p
c specific heat return
      CP = cp0 + cp1 + cp2
      return
      end
