library(oce)
context("sw")
## Table of contents.
##  1. rho and sigma
##  2. potential temperature and temperature scales
##  3. Absolute Salinity and Conservative Temperature
##  4. sound speed
##  5. freezing temperature
##  6. specific heat
##  7. adiabatic lapse rate
##  8. alpha and beta
##  9. swSTrho
## 10. sound absorption (unesco only)
## 11. viscosity (unesco only)
## 12. thermal conductivity (unesco only)
## 13. electrical conductivity
## 14. depth and pressure [FIXME: INCOMPLETE]
## 15. spiciness

## spec vol anom and dens anom
## pressure to depth

## The UNESCO properties are generally tested against UNESCO documents [1].
## The GSW properties are more complicated because its check values are generally
## stated in terms of Absolute Salinity (SA), not Practical Salinity (SP).
## However, we already know that the GSW functions are very well tested
## through tests of the 'gsw' package, so all we really need is a single
## test case for each function, to ensure that eos="gsw" is interpreted
## correctly, etc.
##
## References:
## [1] N. P. Fofonoff and R. C. Millard Jr., 1983.
##     Algorithms for computation of fundamental properties of seawater.
##     UNESCO technical papers in marine science, vol 44.
##     http://www.jodc.go.jp/info/ioc_doc/UNESCO_tech/059832eb.pdf
## [2] Trevor J. McDougall, 1987. Neutral Surfaces, Journal of Physical
##     Oceanography, volume 17, pages 1950-1964.

test_that("rho and sigma", {
          ## 1. rho and sigma
          ## 1.1 UNESCO rho [1 p19]. Note that we must
          ## convert to the T68 temperature scale, which was in use at the time
          ## that [1] was written.
          S <- c( 0,   0,   0,   0,  35,  35,  35,  35)
          T <- T90fromT68(c( 5,   5,  25,  25,   5,   5,  25,  25))
          p <- c( 0, 1e4,   0, 1e4,   0, 1e4,   0, 1e4)
          rho <- c(999.96675, 1044.12802, 997.04796, 1037.90204, 1027.67547, 1069.48914, 1023.34306, 1062.53817)
          expect_equal(swRho(S, T, p, eos="unesco"), rho)
          ## check sigma from this
          expect_equal(swRho(S, T, p, eos="unesco")-1000, swSigma(S,T,p,eos="unesco"))
          ## 1.2 GSW
          ## Since gsw_ functions are tested in the gsw package, we just need a consistency check.
          longitude <- 188
          latitude <- 4
          SP <- 35
          t <- 10
          p <- 1000
          SA <- gsw::gsw_SA_from_SP(SP, p, longitude, latitude)
          CT <- gsw::gsw_CT_from_t(SA, t, p)
          ## Test density.
          rhoGSW <- gsw::gsw_rho(SA, CT, p)
          rho <- swRho(SP, t, p, longitude, latitude, "gsw")
          expect_equal(rhoGSW, rho)
          ## Now use density to test sigma (not provided by gsw).
          sigma <- swSigma(SP, t, p, longitude, latitude, "gsw")
          expect_equal(rhoGSW-1000, sigma)
          ## The following was hard-coded using values from GSW3.03, and it failed with GSW3.05.
          ##expect_equal(30.823971,swSigma(35,13,1000,eos="gsw"),scale=1,tolerance=0.000001)
          expect_equal(30.818302,swSigma(35,T90fromT68(13),1000,eos="unesco"),scale=1,tolerance=0.000001)
          ## The sigmaT tests are not from definititive test values, and so are just
          ## checks against future changes.
          ##expect_equal(swSigmaT(35, 13, 1000, eos="gsw"), 26.396816, scale=1, tolerance=0.000001)
          expect_equal(swSigmaT(35, T90fromT68(13), 1000, eos="unesco"), 26.393538, scale=1, tolerance=0.000001)
})

test_that("potential_temperature (UNESCO)", {
          ## 2.1 UNESCO potential temperature
          ##
          ## The following is an official test value from [1 p44], first with all args,
          ## second with a ctd object as an arg. Note the need to convert to the 1968
          ## temperature scale, which was used in the UNESCO formulation.
          expect_equal(swTheta(40, T90fromT68(40), 10000, eos="unesco"),
                       T90fromT68(36.89073), scale=1, tolerance=0.00002)
          expect_equal(swTheta(as.ctd(40, T90fromT68(40), 10000), eos="unesco"),
                       T90fromT68(36.89073), scale=1, tolerance=0.00002)
          ## Test self-consistency at the surface (also a test of vector reframing)
          T <- 10 + rnorm(50)
          expect_equal(0, sum(abs(T - swTheta(rep(35, 50), T, 0, eos="unesco"))))
})

test_that("SA and CT, sound speed (GSW)", {
          ## 2.2 GSW potential temperature
          ##
          ## Since gsw_ functions are tested in the gsw package, we just need a consistency check.
          SP <- 35
          t <- 13                                 # notation in gsw_...() functions
          p <- 1000
          lon <- 300
          lat <- 30
          ctd <- as.ctd(SP, t, p, longitude=lon, latitude=lat)
          SA <- gsw::gsw_SA_from_SP(SP, p, longitude=lon, latitude=lat)
          thetaGSW <- gsw::gsw_pt_from_t(SA, t, p, p_ref=0)
          theta <- swTheta(ctd, eos="gsw")
          expect_equal(thetaGSW, theta)
          CT <- gsw::gsw_CT_from_t(SA=SA, t=t, p=p)
          expect_equal(SA, swAbsoluteSalinity(salinity=SP, pressure=p, longitude=lon, latitude=lat))
          expect_equal(SA, swAbsoluteSalinity(ctd))
          expect_equal(CT, swConservativeTemperature(salinity=SP, temperature=t, pressure=p, longitude=lon, latitude=lat))
          expect_equal(CT, swConservativeTemperature(ctd))
          expect_equal(1731.995, swSoundSpeed(40,T90fromT68(40),1e4,eos="unesco"),scale=1,tolerance=0.001)
          expect_equal(1731.995, swSoundSpeed(as.ctd(40,T90fromT68(40),1e4),eos="unesco"),scale=1,tolerance=0.001)
          SA <- gsw::gsw_SA_from_SP(SP=40, p=1e4, longitude=300, latitude=30)
          CT <- gsw::gsw_CT_from_t(SA, 40, 1e4)
          speedGSW <- gsw::gsw_sound_speed(SA, CT, 1e4)
          speed <- swSoundSpeed(salinity=40, temperature=40, pressure=1e4, longitude=300, latitude=30, eos="gsw")
          expect_equal(speedGSW, speed)
})

test_that("temperature scales", {
          expect_equal(10, T68fromT90(T90fromT68(10)))
})

test_that("freezing temperature", {
          ## 5.1 UNESCO freezing temperature [1 p29]
          Tf <- swTFreeze(40, 500, eos="unesco")
          expect_equal(Tf, T90fromT68(-2.588567), scale=1, tolerance=1e-6)
          ## 5.2 GSW freezing temperature. This is actually just a test that
          ## the GSW functions are called correctly -- it is *not* using
          ## check values. However, we don't have to worry about check
          ## values, because in the gsw package test suite, there are multiple
          ## tests that check values against those published on the GSW/TEOS10
          ## website.
          SA <- gsw::gsw_SA_from_SP(SP=40, p=500, longitude=300, latitude=30)
          TfGSW <- gsw::gsw_t_freezing(SA=SA, p=500, saturation_fraction=1)
          Tf <- swTFreeze(40, 500, longitude=300, latitude=30, eos="gsw")
          expect_equal(TfGSW, Tf)
})

test_that("specific heat", {
          ## [1 p31]
          p <- 1e4
          t <- 40
          SP <- 40
          lon <- 300
          lat <- 30
          C <- swSpecificHeat(salinity=SP, temperature=T90fromT68(t), pressure=p, eos="unesco")
          expect_equal(C, 3849.499, scale=1, tolerance=1e-3)
          SA <- gsw::gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
          CGSW <- gsw::gsw_cp_t_exact(SA=SA, t=t, p=p)
          C <- swSpecificHeat(salinity=SP, temperature=t, pressure=p, longitude=lon, latitude=lat, eos="gsw")
          expect_equal(CGSW, C)
})

test_that("lapse rate", {
          ## [1 p38]
          SP <- 40
          t <- 40
          p <- 1e4
          l <- swLapseRate(salinity=SP, temperature=T90fromT68(t), pressure=p, eos="unesco")
          expect_equal(l, 3.255976e-4, tolerance=1e-7)
          lon <- 300
          lat <- 30
          SA <- gsw::gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
          CT <- gsw::gsw_CT_from_t(SA=SA, t=t, p=p)
          lGSW <- 1e4*gsw::gsw_adiabatic_lapse_rate_from_CT(SA=SA, CT=CT, p=p) # convert to deg/m
          l <- swLapseRate(salinity=SP, temperature=t, pressure=p, longitude=lon, latitude=lat, eos="gsw")
          expect_equal(lGSW, l)
})

test_that("alpha and beta", {
          ## 8 alpha and beta
          ## 8.1 UNESCO alpha and beta
          ## The formulae used are not actually from UNESCO, but are rather given in [2],
          ## and the test values come from [2] also.
          ##
          ## Since [2] gives formula is in terms of theta=10C, we must compute the
          ## corresponding in-situ temperature first. Use S=40 and 4000dbar to match
          ## his check value.
          T <- uniroot(function(x) 10-swTheta(40, x, 4000, eos="unesco"), c(9, 12))$root
          ## The beta=7.2088e-4 value is from the last sentence of McDougall's Appendix.
          expect_equal(7.2088e-4, swBeta(40, T, 4000, eos="unesco"), scale=1, tolerance=1e-8)
          ## The alpha/beta=0.34763 is from the left-hand column of McDougall's p1964.
          expect_equal(0.34763, swAlphaOverBeta(40, T, 4000, eos="unesco"), scale=1, tolerance=1e-5)
          expect_equal(0.34763*7.20883e-4, swAlpha(40, T, 4000, eos="unesco"), scale=1, tolerance=1e-5)
          ## 8.1 GSW alpha and beta
          ## Check against gsw_ values, which we know to be correct from the gsw test suite.
          SP <- 40
          t <- 10
          p <- 4000
          lon <- 300
          lat <- 30
          a <- swAlpha(SP, t, p, longitude=lon, latitude=lat, eos="gsw")
          b <- swBeta(SP, t, p, longitude=lon, latitude=lat, eos="gsw")
          SA <- gsw::gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat)
          CT <- gsw::gsw_CT_from_t(SA=SA, t=t, p=p)
          aGSW <- gsw::gsw_alpha(SA=SA, CT=CT, p=p)
          expect_equal(a, aGSW)
          bGSW <- gsw::gsw_beta(SA=SA, CT=CT, p=p)
          expect_equal(b, bGSW)
          ## 8.2 swAlphaOverBeta
          ## Ensure that alpha, beta, and alpha/beta are consistent, in both EOS
          S <- 34
          T <- 10
          p <- 100
          expect_equal(swAlphaOverBeta(S, T, p, longitude=300, latitude=30, eos="gsw"),
                       swAlpha(S, T, p, longitude=300, latitude=30, eos="gsw") /
                       swBeta(S, T, p, longitude=300, latitude=30, eos="gsw"))
          expect_equal(swAlphaOverBeta(S, T, p, eos="unesco"),
                       swAlpha(S, T, p, eos="unesco") /
                       swBeta(S, T, p, eos="unesco"))
})

test_that("swSTrho", {
          ## 9. swSTrho
          ## This is used to draw isopycnals on TS diagrams.
          T <- 10
          rho <- 1022
          p <- 0
          ## 9.1 UNESCO swSTrho
          Su <- swSTrho(T90fromT68(T), rho, p, eos="unesco")
          expect_equal(Su, 28.65114808083)
          expect_equal(rho, swRho(Su, T90fromT68(T), 0, eos="unesco"))
          ## 9.2 GSW swSTrho
          CT <- gsw::gsw_CT_from_t(Su, T, p)
          Sg <- swSTrho(CT, rho, p, eos="gsw")
          expect_equal(gsw::gsw_rho(Sg,CT,p), rho)
})

test_that("misc sw calculations", {
          ## The following was hard-coded using values from GSW3.03, and it failed with GSW3.05.
          ## expect_equal(Sg, 28.7842812841013, scale=1, tolerance=1e-8)
          T <- swTSrho(35, 23, 0, eos="unesco") # 26.11301
          expect_equal(T68fromT90(T), 26.1130113601685, scale=1, tolerance=1e-8)
          expect_equal(swRho(35, T, 0, eos="unesco"), 1023, scale=1, tolerance=1e-5)

})

test_that("sound absorption", {
          ## Compared with Table IV of Fisher & Simmons 1977.
          alpha <- swSoundAbsorption(100e3, 35, 4, 4990) # at 500 atm (4990 dbar of water)
          expect_equal(alpha, 0.0175, tolerance=0.01) # 1% test
          alpha <- swSoundAbsorption(10e3, 35, 4, 0) # expect 0.00083 at 1 atm (0dbar of water)
          expect_equal(alpha, 0.000829, tolerance=0.01) # 1% test
})


test_that("viscosity", {
          ## Merely a test against future changes; the original reference did not provide a test value.
          expect_equal(swViscosity(30, 10), 0.001383779, scale=1, tolerance=0.000000001)
})

test_that("thermal conductivity", {
          ## Caldwell 1975 table 1 gives 4 digits, i.e. to 1e-6
          joulePerCalorie <- 4.18400
          cmPerM <- 100
          test <- swThermalConductivity(31.5,10,1000) / joulePerCalorie / cmPerM
          expect_equal(test, 1478e-6, scale=1, tolerance=0.6e-6)
})

test_that("electrical conductivity: definitional check values", {
          expect_equal(swCSTp(35, T90fromT68(15), 0, eos="unesco"),  1)
          expect_equal(swCSTp(35, T90fromT68(15), 0, eos="gsw"), 1)
          expect_equal(swSCTp( 1, T90fromT68(15), 0, eos="unesco"), 35)
          expect_equal(swSCTp( 1, T90fromT68(15), 0, eos="gsw"), 35)
          expect_equal(swSCTp(0.5, 10, 100, eos="unesco"),
                       swSCTp(0.5, 10, 100, eos="gsw"))
          ## These test values are not against a known standard; rather, they simply
          ## assure that there has been no change since a test done on 2019 Mar 23
          ## whilst working on issue https://github.com/dankelley/oce/issues/1514
          expect_equal(swSCTp(0.02, 10, 100, eos="gsw"), 0.601398102117915)
          expect_equal(swSCTp(0.02, 10, 100, eos="unesco"), 0.601172086373874)
})

test_that("electrical conductivity: semi-definitional check values (AUTHOR IS CONFUSED ON THESE)", {
          ## the C=1 value can be tested directly in gsw, but others are tested against gsw.
          SP <- swSCTp(1.2, T90fromT68(20), 2000, eos="gsw")
          expect_equal(1.2, gsw::gsw_C_from_SP(SP, T90fromT68(20), 2000) / gsw::gsw_C_from_SP(35, T90fromT68(15), 0))
          SP <- swSCTp(0.65, T90fromT68(5), 1500, eos="gsw")
          expect_equal(0.65, gsw::gsw_C_from_SP(SP, T90fromT68(5), 1500) / gsw::gsw_C_from_SP(35, T90fromT68(15), 0))
})

test_that("electrical conductivity: real-data checks", {
          data(ctd)
          ## This does not have conductivity, so add it
          salinity <- ctd[["salinity"]]
          temperature <- ctd[["temperature"]]
          pressure <- ctd[["pressure"]]
          conductivity <- swCSTp(salinity, temperature, pressure, eos="unesco")
          ctd <- oceSetData(ctd, name="conductivity", value=conductivity, unit=list(unit=expression(), scale=""))
          S <- swSCTp(ctd)
          expect_equal(S, salinity, tolerance=1e-3)
          ## Test that swCSTp() takes both salinity and CTD [issue 630]
          cond1 <- swCSTp(salinity, temperature, pressure, eos="unesco")
          cond2 <- swCSTp(ctd)
          expect_equal(cond1, cond2)
})

test_that("depth and pressure", {
          ## 14. depth and pressure
          ## The UNESCO test is basically for consistency with old versions, I think,
          ## but the GSW test is against gsw_z_from_p(), which is well-tested in
          ## the building of the gsw package.
          depth <- swDepth(10000, 30, eos="unesco")
          expect_equal(depth, 9712.653, scale=1, tolerance=0.001)
          depth <- swDepth(10000, 30, eos="gsw")
          expect_equal(depth, 9713.735, scale=1, tolerance=0.001)
          pressure <- swPressure(9712.653, 30, eos="unesco")
          expect_equal(pressure, 10000., scale=1, tolerance=0.001)
          pressure <- swPressure(9712.653, 30, eos="gsw")
          expect_equal(pressure, gsw::gsw_p_from_z(-9712.653, 30), scale=1, tolerance=0.001)

})

test_that("spiciness", {
          expect_error(swSpice(35, T90fromT68(10), 100, eos="gsw"), "must supply longitude")
          ## Q: is this test value from Flament's paper, or is it just a consistency check?
          sp <- swSpice(35, T90fromT68(10), 100, eos="unesco")
          expect_equal(sp, 1.131195, tolerance=0.0000015)
          ## compare against direct gsw:: computation
          data(ctd)
          S <- ctd[["salinity"]]
          T <- ctd[["temperature"]]
          p <- ctd[["pressure"]]
          lon <- rep(ctd[["longitude"]], length(S))
          lat <- rep(ctd[["latitude"]], length(S))
          piOce <- swSpice(S, T, p, longitude=lon, latitude=lat, eos="gsw")
          piGsw <- gsw::gsw_spiciness0(ctd[["SA"]], ctd[["CT"]])
          expect_equal(piOce, piGsw)

})

test_that("CTD object accessors for derived properties", {
          data(ctd)
          sigma0 <- swSigma0(ctd[["salinity"]], ctd[["temperature"]], ctd[["pressure"]],
                             longitude=ctd[["longitude"]], latitude=ctd[["latitude"]])
          sigmaTheta <- swSigmaTheta(ctd[["salinity"]], ctd[["temperature"]], ctd[["pressure"]],
                                     longitude=ctd[["longitude"]], latitude=ctd[["latitude"]])
          spice <- swSpice(ctd[["salinity"]], ctd[["temperature"]], ctd[["pressure"]],
                           longitude=ctd[["longitude"]], latitude=ctd[["latitude"]])
          sigmaTheta <- swSigmaTheta(ctd)
          expect_equal(sigma0, ctd[["sigma0"]])
          expect_equal(sigmaTheta, ctd[["sigmaTheta"]])
          expect_equal(spice, ctd[["spice"]])
          expect_equal(sigma0, swSigma0(ctd))
          expect_equal(sigmaTheta, swSigmaTheta(ctd))
          expect_equal(spice, swSpice(ctd))
})

test_that("non-CTD object accessors for derived properties", {
          data(ctd)
          sigma0 <- swSigma0(ctd)
          sigmaTheta <- swSigmaTheta(ctd)
          spice <- swSpice(ctd)
          general <- new("oce")
          general <- oceSetData(general, "temperature", ctd[["temperature"]])
          general <- oceSetData(general, "salinity", ctd[["salinity"]])
          general <- oceSetData(general, "pressure", ctd[["pressure"]])
          general <- oceSetMetadata(general, "longitude", ctd[["longitude"]])
          general <- oceSetMetadata(general, "latitude", ctd[["latitude"]])
          expect_equal(sigma0, general[["sigma0"]])
          expect_equal(sigmaTheta, general[["sigmaTheta"]])
          expect_equal(spice, general[["spice"]])
          expect_equal(sigma0, swSigma0(general))
          expect_equal(sigmaTheta, swSigmaTheta(general))
          expect_equal(spice, swSpice(general))
})

