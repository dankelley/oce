c ftp://climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles/sunae.f
c ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c RCS version control information:
c $Header: sunae.f,v 1.3 96/05/30 09:30:15 wiscombe Exp $
c ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      SUBROUTINE SUNAE( YEAR, DAY, HOUR, LAT, LONG,
     &                  AZ, EL, SOLDIA, SOLDST )

c     Calculates the local solar azimuth and elevation angles, and
c     the distance to and angle subtended by the Sun, at a specific 
c     location and time using approximate formulas in The Astronomical 
c     Almanac.  Accuracy of angles is 0.01 deg or better (the angular 
c     width of the Sun is about 0.5 deg, so 0.01 deg is more than
c     sufficient for most applications).

c     Unlike many GCM (and other) sun angle routines, this
c     one gives slightly different sun angles depending on
c     the year.  The difference is usually down in the 4th
c     significant digit but can slowly creep up to the 3rd
c     significant digit after several decades to a century.

c     A refraction correction appropriate for the "US Standard
c     Atmosphere" is added, so that the returned sun position is
c     the APPARENT one.  The correction is below 0.1 deg for solar
c     elevations above 9 deg.  To remove it, comment out the code
c     block where variable REFRAC is set, and replace it with
c     REFRAC = 0.0.

c   References:

c     Michalsky, J., 1988: The Astronomical Almanac's algorithm for
c        approximate solar position (1950-2050), Solar Energy 40,
c        227-235 (but the version of this program in the Appendix
c        contains errors and should not be used)

c     The Astronomical Almanac, U.S. Gov't Printing Office, Washington,
c        D.C. (published every year): the formulas used from the 1995
c        version are as follows:
c        p. A12: approximation to sunrise/set times
c        p. B61: solar elevation ("altitude") and azimuth
c        p. B62: refraction correction
c        p. C24: mean longitude, mean anomaly, ecliptic longitude,
c                obliquity of ecliptic, right ascension, declination,
c                Earth-Sun distance, angular diameter of Sun
c        p. L2:  Greenwich mean sidereal time (ignoring T^2, T^3 terms)


c   Authors:  Dr. Joe Michalsky (joe@asrc.albany.edu)
c             Dr. Lee Harrison (lee@asrc.albany.edu)
c             Atmospheric Sciences Research Center
c             State University of New York
c             Albany, New York

c   Modified by:  Dr. Warren Wiscombe (wiscombe@climate.gsfc.nasa.gov)
c                 NASA Goddard Space Flight Center
c                 Code 913
c                 Greenbelt, MD 20771


c   WARNING:  Do not use this routine outside the year range
c             1950-2050.  The approximations become increasingly
c             worse, and the calculation of Julian date becomes
c             more involved.

c   Input:

c      YEAR     year (INTEGER; range 1950 to 2050)

c      DAY      day of year at LAT-LONG location (INTEGER; range 1-366)

c      HOUR     hour of DAY [GMT or UT] (REAL; range -13.0 to 36.0)
c               = (local hour) + (time zone number)
c                 + (Daylight Savings Time correction; -1 or 0)
C               where (local hour) range is 0 to 24,
c               (time zone number) range is -12 to +12, and
c               (Daylight Time correction) is -1 if on Daylight Time
c               (summer half of year), 0 otherwise;  
c               Example: 8:30 am Eastern Daylight Time would be
c
c                           HOUR = 8.5 + 5 - 1 = 12.5

c      LAT      latitude [degrees]
c               (REAL; range -90.0 to 90.0; north is positive)

c      LONG     longitude [degrees]
c               (REAL; range -180.0 to 180.0; east is positive)


c   Output:

c      AZ       solar azimuth angle (measured east from north,
c               0 to 360 degs)

c      EL       solar elevation angle [-90 to 90 degs]; 
c               solar zenith angle = 90 - EL

c      SOLDIA   solar diameter [degs]

c      SOLDST   distance to sun [Astronomical Units, AU]
c               (1 AU = mean Earth-sun distance = 1.49597871E+11 m
c                in IAU 1976 System of Astronomical Constants)


c   Local Variables:

c     DEC       Declination (radians)

c     ECLONG    Ecliptic longitude (radians)

c     GMST      Greenwich mean sidereal time (hours)

c     HA        Hour angle (radians, -pi to pi)

c     JD        Modified Julian date (number of days, including 
c               fractions thereof, from Julian year J2000.0);
c               actual Julian date is JD + 2451545.0

c     LMST      Local mean sidereal time (radians)

c     MNANOM    Mean anomaly (radians, normalized to 0 to 2*pi)

c     MNLONG    Mean longitude of Sun, corrected for aberration 
c               (deg; normalized to 0-360)

c     OBLQEC    Obliquity of the ecliptic (radians)

c     RA        Right ascension  (radians)

c     REFRAC    Refraction correction for US Standard Atmosphere (degs)

c --------------------------------------------------------------------
c   Uses double precision for safety and because Julian dates can
c   have a large number of digits in their full form (but in practice
c   this version seems to work fine in single precision if you only
c   need about 3 significant digits and aren't doing precise climate
c   change or solar tracking work).
c --------------------------------------------------------------------

c   Why does this routine require time input as Greenwich Mean Time 
c   (GMT; also called Universal Time, UT) rather than "local time"?
c   Because "local time" (e.g. Eastern Standard Time) can be off by
c   up to half an hour from the actual local time (called "local mean
c   solar time").  For society's convenience, "local time" is held 
c   constant across each of 24 time zones (each technically 15 longitude
c   degrees wide although some are distorted, again for societal 
c   convenience).  Local mean solar time varies continuously around a 
c   longitude belt;  it is not a step function with 24 steps.  
c   Thus it is far simpler to calculate local mean solar time from GMT,
c   by adding 4 min for each degree of longitude the location is
c   east of the Greenwich meridian or subtracting 4 min for each degree
c   west of it.  

c --------------------------------------------------------------------

c   TIME
c   
c   The measurement of time has become a complicated topic.  A few
c   basic facts are:
c   
c   (1) The Gregorian calendar was introduced in 1582 to replace 
c   Julian calendar; in it, every year divisible by four is a leap 
c   year just as in the Julian calendar except for centurial years
c   which must be exactly divisible by 400 to be leap years.  Thus 
c   2000 is a leap year, but not 1900 or 2100.

c   (2) The Julian day begins at Greenwich noon whereas the calendar 
c   day begins at the preceding midnight;  and Julian years begin on
c   "Jan 0" which is really Greenwich noon on Dec 31.  True Julian 
c   dates are a continous count of day numbers beginning with JD 0 on 
c   1 Jan 4713 B.C.  The term "Julian date" is widely misused and few
c   people understand it; it is best avoided unless you want to study
c   the Astronomical Almanac and learn to use it correctly.

c   (3) Universal Time (UT), the basis of civil timekeeping, is
c   defined by a formula relating UT to GMST (Greenwich mean sidereal
c   time).  UTC (Coordinated Universal Time) is the time scale 
c   distributed by most broadcast time services.  UT, UTC, and other
c   related time measures are within a few sec of each other and are
c   frequently used interchangeably.

c   (4) Beginning in 1984, the "standard epoch" of the astronomical
c   coordinate system is Jan 1, 2000, 12 hr TDB (Julian date 
c   2,451,545.0, denoted J2000.0).  The fact that this routine uses
c   1949 as a point of reference is merely for numerical convenience.
c --------------------------------------------------------------------

c     .. Scalar Arguments ..

      INTEGER   YEAR, DAY
cDEK      REAL      AZ, EL, HOUR, LAT, LONG, SOLDIA, SOLDST
      DOUBLE PRECISION      AZ, EL, HOUR, LAT, LONG, SOLDIA, SOLDST
c     ..
c     .. Local Scalars ..

      LOGICAL   PASS1
      INTEGER   DELTA, LEAP
      DOUBLE PRECISION  DEC, DEN, ECLONG, GMST, HA, JD, LMST,
     &                  MNANOM, MNLONG, NUM, OBLQEC, PI, RA,
     &                  RPD, REFRAC, TIME, TWOPI
c     ..
c     .. Intrinsic Functions ..

      INTRINSIC AINT, ASIN, ATAN, COS, MOD, SIN, TAN
c     ..
c     .. Data statements ..

      SAVE     PASS1, PI, TWOPI, RPD
      DATA     PASS1 /.True./
c     ..

      IF( YEAR.LT.1950 .OR. YEAR.GT.2050 ) 
     &    STOP 'SUNAE--bad input variable YEAR'
      IF( DAY.LT.1 .OR. DAY.GT.366 ) 
     &    STOP 'SUNAE--bad input variable DAY'
      IF( HOUR.LT.-13.0 .OR. HOUR.GT.36.0 ) 
     &    STOP 'SUNAE--bad input variable HOUR'
      IF( LAT.LT.-90.0 .OR. LAT.GT.90.0 ) 
     &    STOP 'SUNAE--bad input variable LAT'
      IF( LONG.LT.-180.0 .OR. LONG.GT.180.0 ) 
     &    STOP 'SUNAE--bad input variable LONG'

      IF(PASS1) THEN
         PI     = 2.*ASIN( 1.0 )
         TWOPI  = 2.*PI
         RPD    = PI / 180.
         PASS1 = .False.
      ENDIF

c                    ** current Julian date (actually add 2,400,000 
c                    ** for true JD);  LEAP = leap days since 1949;
c                    ** 32916.5 is midnite 0 jan 1949 minus 2.4e6

      DELTA  = YEAR - 1949
      LEAP   = DELTA / 4
      JD     = 32916.5 + (DELTA*365 + LEAP + DAY) + HOUR / 24.

c                    ** last yr of century not leap yr unless divisible
c                    ** by 400 (not executed for the allowed YEAR range,
c                    ** but left in so our successors can adapt this for 
c                    ** the following 100 years)

      IF( MOD( YEAR, 100 ).EQ.0 .AND.
     &    MOD( YEAR, 400 ).NE.0 ) JD = JD - 1.

c                     ** ecliptic coordinates
c                     ** 51545.0 + 2.4e6 = noon 1 jan 2000

      TIME  = JD - 51545.0

c                    ** force mean longitude between 0 and 360 degs

      MNLONG = 280.460 + 0.9856474*TIME
      MNLONG = MOD( MNLONG, 360. )
      IF( MNLONG.LT.0. ) MNLONG = MNLONG + 360.

c                    ** mean anomaly in radians between 0 and 2*pi

      MNANOM = 357.528 + 0.9856003*TIME
      MNANOM = MOD( MNANOM, 360. )
      IF( MNANOM.LT.0.) MNANOM = MNANOM + 360.

      MNANOM = MNANOM*RPD

c                    ** ecliptic longitude and obliquity 
c                    ** of ecliptic in radians

      ECLONG = MNLONG + 1.915*SIN( MNANOM ) + 0.020*SIN( 2.*MNANOM )
      ECLONG = MOD( ECLONG, 360. )
      IF( ECLONG.LT.0. ) ECLONG = ECLONG + 360.

      OBLQEC = 23.439 - 0.0000004*TIME
      ECLONG = ECLONG*RPD
      OBLQEC = OBLQEC*RPD

c                    ** right ascension

      NUM  = COS( OBLQEC )*SIN( ECLONG )
      DEN  = COS( ECLONG )
      RA   = ATAN( NUM / DEN )

c                    ** Force right ascension between 0 and 2*pi

      IF( DEN.LT.0.0 ) THEN
         RA  = RA + PI
      ELSE IF( NUM.LT.0.0 ) THEN
         RA  = RA + TWOPI
      END IF

c                    ** declination

      DEC  = ASIN( SIN( OBLQEC )*SIN( ECLONG ) )

c                    ** Greenwich mean sidereal time in hours

      GMST = 6.697375 + 0.0657098242*TIME + HOUR

c                    ** Hour not changed to sidereal time since 
c                    ** 'time' includes the fractional day

      GMST  = MOD( GMST, 24. )
      IF( GMST.LT.0. ) GMST   = GMST + 24.

c                    ** local mean sidereal time in radians

      LMST  = GMST + LONG / 15.
      LMST  = MOD( LMST, 24. )
      IF( LMST.LT.0. ) LMST   = LMST + 24.

      LMST   = LMST*15.*RPD

c                    ** hour angle in radians between -pi and pi

      HA  = LMST - RA

      IF( HA.LT.- PI ) HA  = HA + TWOPI
      IF( HA.GT.PI )   HA  = HA - TWOPI

c                    ** solar azimuth and elevation

      EL  = ASIN( SIN( DEC )*SIN( LAT*RPD ) +
     &            COS( DEC )*COS( LAT*RPD )*COS( HA ) )

      AZ  = ASIN( - COS( DEC )*SIN( HA ) / COS( EL ) )

c                    ** Put azimuth between 0 and 2*pi radians

      IF( SIN( DEC ) - SIN( EL )*SIN( LAT*RPD ).GE.0. ) THEN

         IF( SIN(AZ).LT.0.) AZ  = AZ + TWOPI

      ELSE

         AZ  = PI - AZ

      END IF

c                     ** Convert elevation and azimuth to degrees
      EL  = EL / RPD
      AZ  = AZ / RPD

c  ======== Refraction correction for U.S. Standard Atmos. ==========
c      (assumes elevation in degs) (3.51823=1013.25 mb/288 K)

      IF( EL.GE.19.225 ) THEN

         REFRAC = 0.00452*3.51823 / TAN( EL*RPD )

      ELSE IF( EL.GT.-0.766 .AND. EL.LT.19.225 ) THEN

         REFRAC = 3.51823 * ( 0.1594 + EL*(0.0196 + 0.00002*EL) ) /
     &            ( 1. + EL*(0.505 + 0.0845*EL) )

      ELSE IF( EL.LE.-0.766 ) THEN

         REFRAC = 0.0

      END IF

      EL  = EL + REFRAC
c ===================================================================

c                   ** distance to sun in A.U. & diameter in degs

      SOLDST = 1.00014 - 0.01671*COS(MNANOM) - 0.00014*COS( 2.*MNANOM )
      SOLDIA = 0.5332 / SOLDST

      IF( EL.LT.-90.0 .OR. EL.GT.90.0 )
     &    STOP 'SUNAE--output argument EL out of range'
      IF( AZ.LT.0.0 .OR. AZ.GT.360.0 )
     &    STOP 'SUNAE--output argument AZ out of range'

      RETURN

      END
