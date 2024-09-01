# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4
library(oce)

test_that("as.unit", {
    # not found
    expect_equal(as.unit("nosuchunit", default = NULL), NULL)
    # empty
    expect_equal(as.unit(), list(unit = expression(), scale = ""))
    # unitless
    expect_equal(as.unit("1"), list(unit = expression(), scale = ""))
    # extinction coefficient (more space checking here than for others; testing
    # all cases for this is just tedious.
    expect_equal(as.unit("1/m"), list(unit = expression(1 / m), scale = ""))
    expect_equal(as.unit(" 1/m"), list(unit = expression(1 / m), scale = ""))
    expect_equal(as.unit("1/m "), list(unit = expression(1 / m), scale = ""))
    expect_equal(as.unit(" 1/m "), list(unit = expression(1 / m), scale = ""))
    # check spacing in middle (tedious to do this for all, so for now just 1 case)
    expect_equal(as.unit("1 /m"), list(unit = expression(1 / m), scale = ""))
    expect_equal(as.unit("1 / m"), list(unit = expression(1 / m), scale = ""))
    expect_equal(as.unit("1 /m"), list(unit = expression(1 / m), scale = ""))
    # pressure (start by testing spacings)
    expect_equal(as.unit("1 /m"), list(unit = expression(1 / m), scale = ""))
    expect_equal(as.unit("1 / m"), list(unit = expression(1 / m), scale = ""))
    expect_equal(as.unit("1 /m"), list(unit = expression(1 / m), scale = ""))
    # pressure
    expect_equal(as.unit("db"), list(unit = expression(dbar), scale = ""))
    expect_equal(as.unit("dbar"), list(unit = expression(dbar), scale = ""))
    expect_equal(as.unit("dbars"), list(unit = expression(dbar), scale = ""))
    expect_equal(as.unit("decibar"), list(unit = expression(dbar), scale = ""))
    expect_equal(as.unit("decibars"), list(unit = expression(dbar), scale = ""))
    # extinction coefficient
    expect_equal(as.unit("1/m"), list(unit = expression(1 / m), scale = ""))
    expect_equal(as.unit("m-1"), list(unit = expression(1 / m), scale = ""))
    # location
    expect_equal(as.unit("degree east"), list(unit = expression(degree * E), scale = ""))
    expect_equal(as.unit("degrees east"), list(unit = expression(degree * E), scale = ""))
    expect_equal(as.unit("degree west"), list(unit = expression(degree * W), scale = ""))
    expect_equal(as.unit("degrees west"), list(unit = expression(degree * W), scale = ""))
    expect_equal(as.unit("degree north"), list(unit = expression(degree * N), scale = ""))
    expect_equal(as.unit("degrees north"), list(unit = expression(degree * N), scale = ""))
    expect_equal(as.unit("degree south"), list(unit = expression(degree * S), scale = ""))
    expect_equal(as.unit("degrees south"), list(unit = expression(degree * S), scale = ""))
    # temperature
    expect_equal(as.unit("degree"), list(unit = expression(degree), scale = ""))
    expect_equal(as.unit("degrees"), list(unit = expression(degree), scale = ""))
    expect_equal(as.unit("degree C"), list(unit = expression(degree * C), scale = ""))
    expect_equal(as.unit("degrees C"), list(unit = expression(degree * C), scale = ""))
    expect_equal(as.unit("degree celsius"), list(unit = expression(degree * C), scale = ""))
    expect_equal(as.unit("degrees celsius"), list(unit = expression(degree * C), scale = ""))
    expect_equal(as.unit("ipts-68"), list(unit = expression(degree * C), scale = "IPTS-68"))
    expect_equal(as.unit("ipts 68"), list(unit = expression(degree * C), scale = "IPTS-68"))
    expect_equal(as.unit("its-90"), list(unit = expression(degree * C), scale = "ITS-90"))
    expect_equal(as.unit("its 90"), list(unit = expression(degree * C), scale = "ITS-90"))
    # density
    expect_equal(as.unit("kg/m^3"), list(unit = expression(kg / m^3), scale = ""))
    expect_equal(as.unit("kg m-3"), list(unit = expression(kg / m^3), scale = ""))
    # distance
    expect_equal(as.unit("m"), list(unit = expression(m), scale = ""))
    # velocity
    expect_equal(as.unit("m/s"), list(unit = expression(m / s), scale = ""))
    expect_equal(as.unit("m s-1"), list(unit = expression(m / s), scale = ""))
    # acceleration
    expect_equal(as.unit("m/s^2"), list(unit = expression(m / s^2), scale = ""))
    expect_equal(as.unit("m s-2"), list(unit = expression(m / s^2), scale = ""))
    # gas concentration (note litre is sometimes 'l', other times 'L')
    expect_equal(as.unit("ml/l"), list(unit = expression(ml / l), scale = ""))
    expect_equal(as.unit("ml/L"), list(unit = expression(ml / l), scale = ""))
    expect_equal(as.unit("ml l-1"), list(unit = expression(ml / l), scale = ""))
    expect_equal(as.unit("ml L-1"), list(unit = expression(ml / l), scale = ""))
    # salinity
    expect_equal(as.unit("pss 78"), list(unit = expression(), scale = "PSS-78"))
    # concentration
    expect_equal(as.unit("ug/l"), list(unit = expression(mu * g / l), scale = ""))
    expect_equal(as.unit("ug/kg"), list(unit = expression(mu * g / kg), scale = ""))
    expect_equal(as.unit("umol/kg"), list(unit = expression(mu * mol / kg), scale = ""))
    expect_equal(as.unit("umol kg-1"), list(unit = expression(mu * mol / kg), scale = ""))
    expect_equal(as.unit("micromole/kg"), list(unit = expression(mu * mol / kg), scale = ""))
    expect_equal(as.unit("micromole kg-1"), list(unit = expression(mu * mol / kg), scale = ""))
    expect_equal(as.unit("umol/l"), list(unit = expression(mu * mol / l), scale = ""))
    expect_equal(as.unit("umol l-1"), list(unit = expression(mu * mol / l), scale = ""))
    expect_equal(as.unit("micromole/l"), list(unit = expression(mu * mol / l), scale = ""))
    expect_equal(as.unit("micromole l-1"), list(unit = expression(mu * mol / l), scale = ""))
    expect_equal(as.unit("mg/m^3"), list(unit = expression(mg / m^3), scale = ""))
    expect_equal(as.unit("mg m-3"), list(unit = expression(mg / m^3), scale = ""))
    # radition
    expect_equal(as.unit("uEinsteins/s/m^2"), list(unit = expression(mu * Einstein / s / m^2), scale = ""))
    expect_equal(as.unit("uEinsteins s-1 m-2"), list(unit = expression(mu * Einstein / s / m^2), scale = ""))
    # inverse velocity
    expect_equal(as.unit("s/m"), list(unit = expression(s / m), scale = ""))
    # conductivity
    expect_equal(as.unit("S/m"), list(unit = expression(S / m), scale = ""))
    expect_equal(as.unit("S m-1"), list(unit = expression(S / m), scale = ""))
    expect_equal(as.unit("mS/cm"), list(unit = expression(mS / cm), scale = ""))
    expect_equal(as.unit("mS cm-1"), list(unit = expression(mS / cm), scale = ""))
    expect_equal(as.unit("uS/cm"), list(unit = expression(mu * S / cm), scale = ""))
    expect_equal(as.unit("uS cm-1"), list(unit = expression(mu * S / cm), scale = ""))
    # time
    expect_equal(as.unit("seconds since"), list(unit = expression(s), scale = ""))
    expect_equal(as.unit("volt"), list(unit = expression(V), scale = ""))
    # voltage
    expect_equal(as.unit("volts"), list(unit = expression(V), scale = ""))
})

if (dir.exists("local_data")) {
    test_that("units in a CTD file of type WOCE (style 1)", {
        woce <- read.ctd.woce("local_data/ctd/18HU2010014_00003_00001_ct1.csv")
        # test units (issue 1194)
        expect_equal(woce[["pressureUnit"]], list(unit = expression(dbar), scale = ""))
        expect_equal(woce[["temperatureUnit"]], list(unit = expression(degree * C), scale = "IPTS-68"))
        expect_equal(woce[["salinityUnit"]], list(unit = expression(), scale = "PSS-78"))
        expect_equal(woce[["oxygenUnit"]], list(unit = expression(mu * mol / kg), scale = ""))
    })
}
