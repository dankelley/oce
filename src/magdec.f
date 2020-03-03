      subroutine md_driver(colat, elong, date, n,
     1 declination, inclination, intensity, iversion)

      implicit double precision (a-h,o-z)
      double precision colat(n), elong(n), date(n)
      double precision declination(n), inclination(n), intensity(n)
      double precision x, y, z
      isv = 0
      itype = 1
      alt = 0.0
      if (iversion .EQ. 12) then
          do i = 1, n
              call igrf12syn(isv, date(i), itype, alt, colat(i),
     1                       elong(i), x, y, z, f)
              declination(i) = 57.2957795130823*atan2(y,x)
              inclination(i) = 57.2957795130823*atan2(z,sqrt(x**2+y**2))
              intensity(i) = f
          end do
      else if (iversion .EQ. 13) then
          do i = 1, n
              call igrf13syn(isv, date(i), itype, alt, colat(i),
     1                       elong(i), x, y, z, f)
              declination(i) = 57.2957795130823*atan2(y,x)
              inclination(i) = 57.2957795130823*atan2(z,sqrt(x**2+y**2))
              intensity(i) = f
          end do
      end if
      return
      end

