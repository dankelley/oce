## vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
library(oce)
data(argo)

context("Argo")

test_that("subset.argo(argo, \"adjusted\") correctly alters metadata and data", {
          a <- subset(argo, "adjusted")
          expect_equal(a@metadata$flags$pressureQc, argo@metadata$flags$pressureAdjustedQc)
          expect_equal(a@metadata$flags$temperatureQc, argo@metadata$flags$temperatureAdjustedQc)
          expect_equal(a@metadata$flags$salinityQc, argo@metadata$flags$salinityAdjustedQc)
          expect_equal(a@metadata$flags$pressure, argo@metadata$flags$pressureAdjusted)
          expect_equal(a@metadata$flags$salinity, argo@metadata$flags$salinityAdjusted)
          expect_equal(a@metadata$flags$temperature, argo@metadata$flags$temperatureAdjusted)
})

test_that("argo [[ handles SA and CT", {
          SA <- argo[["SA"]]
          CT <- argo[["CT"]]
          SP <- argo[["salinityAdjusted"]]
          t <- argo[["temperatureAdjusted"]]
          p <- argo[["pressureAdjusted"]]
          lon <- rep(argo[["longitude"]], each=dim(SP)[1])
          lat <- rep(argo[["latitude"]], each=dim(SP)[1])
          expect_equal(SA, gsw_SA_from_SP(SP=SP, p=p, longitude=lon, latitude=lat))
          expect_equal(CT, gsw_CT_from_t(SA=SA, t=t, p=p))
})

test_that("argo name conversion", {
          table <- "BBP BBP
          BETA_BACKSCATTERING betaBackscattering
          BPHASE_DOXY bphaseOxygen
          CDOM CDOM
          CHLA chlorophyllA
          CNDC conductivity
          CP beamAttenuation
          CYCLE_NUMBER cycleNumber
          DOWN_IRRADIANCE downwellingIrradiance
          DOWNWELLING_PAR downwellingPAR
          DOXY oxygen
          FIT_ERROR_NITRATE fitErrorNitrate
          FLUORESCENCE_CDOM fluorescenceCDOM
          FLUORESCENCE_CHLA fluorescenceChlorophyllA
          MOLAR_DOXY oxygenUncompensated
          NITRATE nitrate
          PH_IN_SITU_FREE pHFree
          PH_IN_SITU_TOTAL pH
          PRES pressure
          RAW_DOWNWELLING_IRRADIANCE rawDownwellingIrradiance
          RAW_DOWNWELLING_PAR rawDownwellingPAR
          RAW_UPWELLING_RADIANCE rawUpwellingRadiance
          TEMP temperature
          TEMP_DOXY temperatureOxygen
          TEMP_NITRATE temperatureNitrate
          TEMP_PH temperaturePH
          TEMP_CPU_CHLA temperatureCPUChlA
          TEMP_SPECTROPHOTOMETER_NITRATE temperatureSpectrophotometerNitrate
          TEMP_VOLTAGE_DOXY temperatureVoltageOxygen
          TILT tilt
          TURBIDITY turbidity
          TRANSMITTANCE_PARTICLE_BEAM_ATTENUATION transmittanceParticleBeamAttenuation
          UP_RADIANCE upwellingRadiance
          UV_INTENSITY_DARK_NITRATE UVIntensityDarkNitrate
          UV_INTENSITY_NITRATE UVIntensityNitrate
          VRS_PH VRSpH
          "

          ## REMAINING TO DO--
          ## see https://github.com/dankelley/oce/issues/1122
          ##
          ## BPHASE_DOXY2
          ## C1PHASE_DOXY
          ## C2PHASE_DOXY
          ## FREQUENCY_DOXY
          ## HUMIDITY_NITRATE
          ## IB_PH
          ## PHASE_DELAY_DOXY
          ## RPHASE_DOXY
          ## TPHASE_DOXY

          a <- read.table(text=table, header=FALSE, stringsAsFactors=FALSE)
          expect_equal(argoNames2oceNames(a$V1), a$V2)
          expect_equal(argoNames2oceNames(paste(a$V1, "123", sep="")), paste(a$V2, "123", sep=""))
})

