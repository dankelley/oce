# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Infer variable name, units and scale from a Seabird (.cnv) header line
#'
#' This function is used by [read.ctd.sbe()] to infer data names
#' and units from the coding used by Teledyne/Seabird (SBE) `.cnv`
#' files.  Lacking access to documentation on the SBE format,
#' the present function is based on inspection of a suite of CNV files
#' available to the `oce` developers.
#'
#' A few sample header lines that have been encountered are:
#'```
#' # name 4 = t068: temperature, IPTS-68 [deg C]
#' # name 3 = t090C: Temperature [ITS-90, deg C]
#' # name 4 = t190C: Temperature, 2 [ITS-90, deg C]
#'```
#' Examination of several CNV files suggests that it is best to
#' try to infer the name from the characters between the "`=`"
#' and "`:`" characters, because the material after the colon
#' seems to vary more between sample files.
#'
#' The table given below indicates the translation patterns used. These are
#' taken from reference 1. The `.cnv` convention for multiple sensors is to
#' include optional extra digits in the name, and these are indicated
#' with `~` in the table; their decoding is done with [grep()].
#'
#' It is important to note that this table is by no means complete, since there
#' are a great many SBE names listed in their document (reference 1), plus names
#' not listed there but present in data files
#' supplied by prominent archiving agencies. If an SBE name is not recognized,
#' then the oce name is set to that SBE name. This can cause problems in
#' some other processing steps (e.g. if [swRho()] or a similar
#' function is called with an `oce` object as first argument), and so
#' users are well-advised to rename the items as appropriate. The first
#' step in doing this is to pass the object to `summary()`, to discover
#' the SBE names in question. Then consult the SBE documentation to find
#' an appropriate name for the data, and either manipulate the names in the object
#' data slot directly or use
#' [renameData()] to rename the elements. Finally, please publish
#' an 'issue' on the oce Github site `https://github.com/dankelley/oce/issues`
#' so that the developers can add the data type in question. (To save
#' development time, there is no plan to add all possible data types without
#' a reasonable and specific expression user interest. Oxygen alone has over forty
#' variants.)
#'
#' \tabular{llll}{
#'   **Key**       \tab **Result**                     \tab **Unit;scale**      \tab **Notes** \cr
#'   `alt`         \tab `altimeter`                    \tab m                   \tab    \cr
#'   `altM`        \tab `altimeter`                    \tab m                   \tab    \cr
#'   `accM`        \tab `acceleration`                 \tab m/s^2               \tab    \cr
#'   `bat~`        \tab `beamAttenuation`              \tab 1/m                 \tab    \cr
#'   `C2-C1S/m`    \tab `conductivityDifference`       \tab S/m                 \tab    \cr
#'   `C2-C1mS/cm`  \tab `conductivityDifference`       \tab mS/cm               \tab    \cr
#'   `C2-C1uS/cm`  \tab `conductivityDifference`       \tab uS/cm               \tab    \cr
#'   `c~mS/cm`     \tab `conductivity`                 \tab mS/cm               \tab    \cr
#'   `cond~mS/cm`  \tab `conductivity`                 \tab mS/cm               \tab    \cr
#'   `c~S/m`       \tab `conductivity`                 \tab S/m                 \tab    \cr
#'   `cond~S/m`    \tab `conductivity`                 \tab S/m                 \tab    \cr
#'   `c~uS/cm`     \tab `conductivity`                 \tab uS/cm               \tab    \cr
#'   `cond~uS/cm`  \tab `conductivity`                 \tab uS/cm               \tab    \cr
#'   `CStarAt~`    \tab `beamAttenuation`              \tab 1/m                 \tab    \cr
#'   `CStarTr~`    \tab `beamTransmission`             \tab percent             \tab    \cr
#'   `density~~`   \tab `density`                      \tab kg/m^3              \tab    \cr
#'   `depS`        \tab `depth`                        \tab m                   \tab    \cr
#'   `depSM`       \tab `depth`                        \tab m                   \tab    \cr
#'   `depF`        \tab `depth`                        \tab m                   \tab    \cr
#'   `depFM`       \tab `depth`                        \tab m                   \tab    \cr
#'   `dz/dtM`      \tab `descentRate`                  \tab m/s                 \tab    \cr
#'   `f~`          \tab `frequency`                    \tab Hz                  \tab    \cr
#'   `f~~`         \tab `frequency`                    \tab Hz                  \tab    \cr
#'   `flC~`        \tab `fluorescence`                 \tab ug/l; Chelsea Aqua 3\tab    \cr
#'   `flCM`        \tab `fluorescence`                 \tab ug/l; Chelsea Mini Chl Con\tab\cr
#'   `flCUVA~`     \tab `fluorescence`                 \tab ug/l; Chelsea UV Aquatracka\tab\cr
#'   `flEC-AFL~`   \tab `fluorescence`                 \tab mg/m^3; WET Labs ECO-AFL/FLtab\cr
#'   `flS`         \tab `fluorescence`                 \tab -; Seatech          \tab    \cr
#'   `flScufa~`    \tab `fluorescence`                 \tab -; Turner SCUFA (RFU)\tab\cr
#'   `flSP`        \tab `fluorescence`                 \tab -; Seapoint         \tab    \cr
#'   `flSPR`       \tab `fluorescence`                 \tab -; Seapoint, Rhodamine\tab  \cr
#'   `flSPuv`      \tab `fluorescence`                 \tab -; Seapoint, UV      \tab   \cr
#'   `flT`         \tab `fluorescence`                 \tab -; Turner 10-005 flT\tab   \cr
#'   `gpa`         \tab `geopotentialAnomaly`          \tab -; J/kg              \tab   \cr
#'   `latitude`    \tab `latitude`                     \tab degN                 \tab   \cr
#'   `longitude`   \tab `longitude`                    \tab degE                 \tab   \cr
#'   `n2satML/L`   \tab `nitrogenSaturation`           \tab ml/l                 \tab   \cr
#'   `n2satMg/L`   \tab `nitrogenSaturation`           \tab mg/l                 \tab   \cr
#'   `n2satumol/kg`\tab `nitrogenSaturation`           \tab umol/kg              \tab   \cr
#'   `nbin`        \tab `nbin`                         \tab                      \tab   \cr
#'   `obsscufa~`   \tab `backscatter`                  \tab NTU; Turner SCUFA    \tab   \cr
#'   `opoxMg/L`    \tab `oxygen`                       \tab mg/l; Optode, Aanderaa\tab   \cr
#'   `opoxML/L`    \tab `oxygen`                       \tab ml/l; Optode, Aanderaa\tab   \cr
#'   `opoxMm/L`    \tab `oxygen`                       \tab umol/l; Optode, Aanderaa\tab \cr
#'   `opoxPS`      \tab `oxygen`                       \tab percent; Optode, Aanderaa\tab   \cr
#'   `oxsatML/L`   \tab `oxygen`                       \tab ml/l; Weiss          \tab   \cr
#'   `oxsatMg/L`   \tab `oxygen`                       \tab mg/l; Weiss          \tab   \cr
#'   `oxsatMm/Kg`  \tab `oxygen`                       \tab umol/kg; Weiss       \tab   \cr
#'   `oxsolML/L`   \tab `oxygen`                       \tab ml/l; Garcia-Gordon  \tab   \cr
#'   `oxsolMg/L`   \tab `oxygen`                       \tab mg/l; Garcia-Gordon  \tab   \cr
#'   `oxsolMm/Kg`  \tab `oxygen`                       \tab umol/kg; Garcia-Gordon\tab  \cr
#'   `par~`        \tab `PAR`                          \tab -; Biospherical/Licor\tab   \cr
#'   `par/log`     \tab `PAR`                          \tab log; Satlantic       \tab   \cr
#'   `ph`          \tab `pH`                           \tab -                    \tab   \cr
#'   `potemp~68C`  \tab `thetaM`                       \tab degC; IPTS-68        \tab   \cr
#'   `potemp~90C`  \tab `thetaM`                       \tab degC; ITS-90         \tab   \cr
#'   `pr`          \tab `pressure`                     \tab dbar                 \tab 1 \cr
#'   `prM`         \tab `pressure`                     \tab dbar                 \tab   \cr
#'   `pr50M`       \tab `pressure`                     \tab dbar; SBE50          \tab   \cr
#'   `prSM`        \tab `pressure`                     \tab dbar                 \tab   \cr
#'   `prDM`        \tab `pressure`                     \tab dbar; digiquartz     \tab   \cr
#'   `prdE`        \tab `pressure`                     \tab psi; strain gauge    \tab 2 \cr
#'   `prDE`        \tab `pressure`                     \tab psi; digiquartz      \tab 2 \cr
#'   `prdM`        \tab `pressure`                     \tab dbar; strain gauge   \tab   \cr
#'   `prSM`        \tab `pressure`                     \tab dbar; strain gauge   \tab   \cr
#'   `ptempC`      \tab `pressureTemperature`          \tab degC; ITS-90         \tab 3 \cr
#'   `pumps`       \tab `pumpStatus`                   \tab                      \tab   \cr
#'   `rhodflTC~`   \tab `Rhodamine`                    \tab ppb; Turner Cyclops  \tab   \cr
#'   `sal~~`       \tab `salinity`                     \tab -, PSS-78            \tab 4 \cr
#'   `sbox~dV/dT`  \tab `oxygen`                       \tab dov/dt; SBE43        \tab   \cr
#'   `sbeox~ML/L`  \tab `oxygen`                       \tab ml/l; SBE43          \tab   \cr
#    `sbox~ML/L`   \tab `oxygen`                       \tab ml/l; SBE43          \tab   \cr
#'   `sbeox~Mm/Kg` \tab `oxygen`                       \tab umol/kg; SBE43       \tab   \cr
#'   `sbox~Mm/Kg`  \tab `oxygen`                       \tab umol/kg; SBE43       \tab   \cr
#'   `sbeox~Mm/L`  \tab `oxygen`                       \tab umol/l; SBE43        \tab   \cr
#'   `sbox~Mm/L`   \tab `oxygen`                       \tab umol/l; SBE43        \tab   \cr
#'   `sbeox~PS`    \tab `oxygen`                       \tab percent; SBE43       \tab   \cr
#'   `sbox~PS`     \tab `oxygen`                       \tab percent; SBE43       \tab   \cr
#'   `sbeox~V`     \tab `oxygenRaw`                    \tab V; SBE43             \tab   \cr
#'   `sbox~V`      \tab `oxygenRaw`                    \tab V; SBE43             \tab   \cr
#'   `scan`        \tab `scan`                         \tab -                    \tab   \cr
#'   `seaTurbMtr~` \tab `turbidity`                    \tab FTU; Seapoint        \tab   \cr
#'   `secS-priS`   \tab `salinityDifference`           \tab -, PSS-78            \tab   \cr
#'   `sigma-t`     \tab `sigmaT`                       \tab kg/m^3               \tab   \cr
#'   `sigma-theta` \tab `sigmaTheta`                   \tab kg/m^3               \tab 5 \cr
##   `sigma-é`     \tab `sigmaTheta`                   \tab kg/m^3               \tab 5 \cr
#'   \code{sigma-}\enc{é}{e} \tab `sigmaTheta`         \tab kg/m^3               \tab 5 \cr
#'   `spar`        \tab `spar`                         \tab -                    \tab   \cr
#'   `specc`       \tab `conductivity`                 \tab uS/cm                \tab   \cr
#'   `sva`         \tab `specificVolumeAnomaly`        \tab 1e-8 m^3/kg;         \tab   \cr
#'   `svCM~`       \tab `soundSpeed`                   \tab m/s; Chen-Millero    \tab   \cr
#'   `T2~68C`      \tab `temperatureDifference`        \tab degC; IPTS-68        \tab   \cr
#'   `T2~90C`      \tab `temperatureDifference`        \tab degC; ITS-90         \tab   \cr
#'   `t~68`        \tab `temperature`                  \tab degC; IPTS-68        \tab   \cr
#'   `t~90`        \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `t~68`        \tab `temperature`                  \tab degC; IPTS-68        \tab   \cr
#'   `t~68C`       \tab `temperature`                  \tab degC; IPTS-68        \tab   \cr
#'   `t~90C`       \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `t090Cm`      \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `t4990C`      \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `tnc90C`      \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `tsa`         \tab `thermostericAnomaly`          \tab 1e-8 m^3/kg          \tab   \cr
#'   `tv290C`      \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `t4968C`      \tab `temperature`                  \tab degC; IPTS-68        \tab   \cr
#'   `tnc68C`      \tab `temperature`                  \tab degC; IPTS-68        \tab   \cr
#'   `tv268C`      \tab `temperature`                  \tab degC; IPTS-68        \tab   \cr
#'   `t190C`       \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `tnc290C`     \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `tnc268C`     \tab `temperature`                  \tab degC; IPTS-68        \tab   \cr
#'   `t3890C~`     \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `t38~90C`     \tab `temperature`                  \tab degC; ITS-90         \tab   \cr
#'   `t3868C~`     \tab `temperature`                  \tab degC; IPTS-68        \tab   \cr
#'   `t38~38C`     \tab `temperature`                  \tab degC; IPTS-68        \tab   \cr
#'   `timeH`       \tab `timeH`                        \tab hour; elapsed        \tab   \cr
#'   `timeJ`       \tab `timeJ`                        \tab julian day           \tab   \cr
#'   `timeJV2`     \tab `timeJV2`                      \tab julian day           \tab   \cr
#'   `timeK`       \tab `timeK`                        \tab s; since Jan 1, 2000 \tab   \cr
#'   `timeM`       \tab `timeM`                        \tab minute; elapsed      \tab   \cr
#'   `timeN`       \tab `timeN`                        \tab s; NMEA since Jan 1, 1970\tab\cr
#'   `timeQ`       \tab `timeQ`                        \tab s; NMEA since Jan 1, 2000\tab\cr
#'   `timeS`       \tab `timeS`                        \tab s; elapsed           \tab   \cr
#'   `turbflTC~`   \tab `turbidity`                    \tab NTU; Turner Cyclops  \tab   \cr
#'   `turbflTCdiff`\tab `turbidityDifference`          \tab NTU; Turner Cyclops  \tab   \cr
#'   `turbWETbb~`  \tab `turbidity`                    \tab 1/(m\*sr); WET Labs ECO\tab   \cr
#'   `turbWETbbdiff`\tab `turbidityDifference`         \tab 1/(m\*sr); WET Labs ECO\tab   \cr
#'   `turbWETntu~` \tab `turbidity`                    \tab NTU; WET Labs ECO    \tab   \cr
#'   `turbWETntudiff`\tab `turbidityDifference`        \tab NTU; WET Labs ECO    \tab   \cr
#'   `upoly~`      \tab `upoly`                        \tab -                    \tab   \cr
#'   `user~`       \tab `user`                         \tab -                    \tab   \cr
#'   `v~~`         \tab `voltage`                      \tab V                    \tab   \cr
#'   `wetBAttn`    \tab `beamAttenuation`              \tab 1/m; WET Labs AC3    \tab   \cr
#'   `wetBTrans`   \tab `beamTransmission`             \tab percent; WET Labs AC3\tab   \cr
#'   `wetCDOM~`    \tab `fluorescence`                 \tab mg/m^3; WET Labs CDOM\tab   \cr
#'   `wetCDOMdiff` \tab `fluorescenceDifference`       \tab mg/m^3; WET Labs CDOM\tab   \cr
#'   `wetChAbs`    \tab `fluorescence`                 \tab 1/m; WET Labs AC3 absorption\tab   \cr
#'   `wetStar~`    \tab `fluorescence`                 \tab mg/m^3; WET Labs WETstar\tab   \cr
#'   `wetStardiff` \tab `fluorescenceDifference`       \tab mg/m^3; WET Labs WETstar\tab   \cr
#'   `xmiss`       \tab `beamTransmission`             \tab percent; Chelsea/Seatech\tab \cr
#'   `xmiss~`      \tab `beamTransmission`             \tab percent; Chelsea/Seatech\tab \cr
#' }
#' Notes:
#' 1. 'pr' is in a Dalhousie-generated data file but seems not to be in reference 1.
#' 2. This is an odd unit, and so if `sw*` functions are called on an object
#' containing this, a conversion will be made before performing the computation. Be
#' on the lookout for errors, since this is a rare situation.
#' 3. Assume ITS-90 temperature scale, since sample `.cnv` file headers do not specify it.
#' 4. Some files have PSU for this. Should we handle that? And are there other S scales to consider?
#' 5. The 'theta' symbol (here shown accented e) may appear in different ways with different
#' encoding configurations, set up within R or in the operating system.
#'
#' @param h The header line.
#'
#' @param columns Optional list containing name correspondences, as described for
#' [read.ctd.sbe()].
#'
#' @template debugTemplate
#'
## NOTE: @return is handled in readCtdTemplate
#'
#' @author Dan Kelley
#'
#' @references
#' 1. A SBE data processing manual was once at
#' `http://www.seabird.com/document/sbe-data-processing-manual`,
#' but as of summer 2018, this no longer seems to be provided by SeaBird.
#' A web search will turn up copies of the manual that have been put
#' online by various research groups and data-archiving agencies.
#' As of 2018-07-05, the latest version was named
#' `SBEDataProcessing_7.26.4.pdf` and had release date 12/08/2017,
#' and this was the reference version used in coding `oce`.
#'
#' @family things related to ctd data
#' @family functions that interpret variable names and units from headers
cnvName2oceName <- function(h, columns=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "cnvName2oceName() {\n", unindent=1)
    if (length(h) != 1)
        stop("oceNameFromSBE() expects just 1 line of header")
    ## An example, for which the grep is designed, is below.
    ## '# name 4 = t190C: Temperature, 2 [ITS-90, deg C]'
    if (1 != length(grep("^# name [0-9][0-9]* = .*:.*$", h, ignore.case=TRUE)))
        stop("header line does not contain a variable name")
    ## message("h: '", h, "'")

    # 2022-07-15: drop useBytes=TRUE, which is a problem for upcoming R.
    name <- gsub("^# name [0-9][0-9]* = (.*):.*$", "\\1", h, ignore.case=TRUE)
    nameAfterColon <- gsub("^# name [0-9][0-9]* = .*:(.*)$", "\\1", h, ignore.case=TRUE)
    nameOriginal <- name

    ## If 'name' is mentioned in columns, then use columns and ignore the lookup table.
    if (!is.null(columns)) {
        oceDebug(debug, "columns given. Look for name='", name, "' in it\n", sep="")
        cnames <- names(columns)
        for (i in seq_along(cnames)) {
            if (name == columns[[i]]$name) {
                oceDebug(debug, "recognized this name in names(columns)[", i, "]\n")
                return(list(name=cnames[i], nameOriginal=name, unit=columns[[i]]$unit))
            }
        }
    }

    ## Since 'name' is not mentioned in 'columns', try looking it up. Some of these
    ## tests are a bit subtle, and could be wrong.
    if (1 == length(grep("^alt[M]?$", name))) {
        name <- "altimeter"
        unit <- list(unit=expression(m), scale="")
    } else if (1 == length(grep("^acc[M]?$", name))) {
        name <- "acceleration"
        unit <- list(unit=expression(m/s^2), scale="")
    } else if (1 == length(grep("^bat[0-9]?$", name))) {
        name <- "beamAttenuation"
        unit <- list(unit=expression(1/m), scale="Chelsea/Seatech")
    } else if (1 == length(grep("^C2-C1S/m$", name))) {
        name <- "conductivityDifference"
        unit <- list(unit=expression(S/m), scale="")
    } else if (1 == length(grep("^C2-C1mS/cm$", name))) {
        name <- "conductivityDifference"
        unit <- list(unit=expression(mS/cm), scale="")
    } else if (1 == length(grep("^C2-C1uS/cm$", name))) {
        name <- "conductivityDifference"
        unit <- list(unit=expression(mu*S/cm), scale="")
    } else if (1 == length(grep("^c(ond)?((_)|([0-2]))mS/cm$", name))) {
        name <- "conductivity"
        unit <- list(unit=expression(mS/cm), scale="")
    } else if (1 == length(grep("^c(ond)?((_)|([0-2]))S/m$", name))) {
        name <- "conductivity"
        unit <- list(unit=expression(S/m), scale="")
    } else if (1 == length(grep("^c(ond)?((_)|([0-2]))uS/cm$", name))) {
        name <- "conductivity"
        unit <- list(unit=expression(mu*S/cm), scale="")
    } else if (1 == length(grep("^CStarTr[0-9]$", name))) {
        name <- "beamTransmission"
        unit <- list(unit=expression(percent), scale="WET Labs C-Star")
    } else if (1 == length(grep("^CStarAt[0-9]$", name))) {
        name <- "beamAttenuation"
        unit <- list(unit=expression(1/m), scale="")
    } else if (1 == length(grep("^density[0-9]{2}$", name))) {
        name <- "density"
        unit <- list(unit=expression(kg/m^3), scale="")
    } else if (1 == length(grep("^dep[FS][M]?$", name))) {
        name <- "depth"
        unit <- list(unit=expression(m), scale="")
    } else if (1 == length(grep("^dz/dt[M]?$", name))) {
        name <- "descentRate"
        unit <- list(unit=expression(m/s), scale="")
    } else if (1 == length(grep("^f[0-9][0-9]?$", name))) {
        name <- "frequency"
        unit <- list(unit=expression(Hz), scale="")
    } else if (1 == length(grep("^flag$", name))) {
        name <- "flag"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^flC[1]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mu*g/l), scale="Chelsea")
    } else if (1 == length(grep("^flCM[1]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mu*g/l), scale="Chelsea Mini Chl Con")
    } else if (1 == length(grep("^flCUVA[12]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mu*g/l), scale="Chelsea UV Aquatracka")
    } else if (1 == length(grep("^flECO-AFL[0-9]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs")
    } else if (1 == length(grep("^flflTC[0-1]{1}$", name))) {
        name <- "fluorescein"
        unit <- list(unit=expression(ppb), scale="Turner Cyclops")
    } else if (1 == length(grep("^flflTCdiff$", name))) {
        name <- "fluoresceinDifference"
        unit <- list(unit=expression(ppb), scale="Turner Cyclops")
    } else if (1 == length(grep("^flScufa[0-9]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Turner SCUFA")
    } else if (1 == length(grep("^flSP[0-9]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seapoint")
    } else if (1 == length(grep("^flSPR$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seapoint, Rhodamine")
    } else if (1 == length(grep("^flSPuv[0-9]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seapoint, UV")
    } else if (1 == length(grep("^flS$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seatech")
    } else if (1 == length(grep("^flT$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Turner")
    } else if (1 == length(grep("^gpa$", name))) {
        name <- "geopotentialAnomaly"
        unit <- list(unit=expression(J/kg), scale="")
    } else if (1 == length(grep("^latitude$", name))) {
        name <- "latitude"
        unit <- list(unit=expression(degree*N), scale="")
    } else if (1 == length(grep("^longitude$", name))) {
        name <- "longitude"
        unit <- list(unit=expression(degree*E), scale="")
    } else if (1 == length(grep("^n2satML/L$", name))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(ml/l), scale="")
    } else if (1 == length(grep("^n2satMg/L$", name))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(mg/l), scale="")
    } else if (1 == length(grep("^n2satumol/L$", name))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(mu*mol/l), scale="")
    } else if (1 == length(grep("^n2satumol/kg$", name))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(mu*mol/kg), scale="")
    } else if (1 == length(grep("^nbin$", name))) {
        name <- "nbin"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^nbf$", name))) {
        name <- "bottlesFired"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^obsscufa[1-9]{0,1}$", name))) {
        name <- "backscatter"
        unit <- list(unit=expression(NTU), scale="Turner SCUFA")
    } else if (1 == length(grep("^opoxMg/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Optode, Aanderaa")
    } else if (1 == length(grep("^opoxML/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Optode, Aanderaa")
    } else if (1 == length(grep("^opoxMm/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/l), scale="Optode, Aanderaa")
    } else if (1 == length(grep("^opoxPS$", name))) {
        name <- "oxygenSaturation"
        unit <- list(unit=expression(percent), scale="Optode, Aanderaa")
    } else if (1 == length(grep("oxC", name))) {
        name <- "oxygenCurrent"
        unit <- list(unit=expression(mu*amp), scale="Beckman/YSI")
    } else if (1 == length(grep("oxTC", name))) {
        name <- "oxygenTemperature"
        unit <- list(unit=expression(degree*C), scale="Beckman/YSI")
    } else if (1 == length(grep("oxMg/L", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Beckman/YSI")
    } else if (1 == length(grep("oxPS", name))) {
        name <- "oxygenSaturation"
        unit <- list(unit=expression(percent), scale="Beckman/YSI")
    } else if (1 == length(grep("^oxsatML/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Weiss")
    } else if (1 == length(grep("^oxsatMg/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Weiss")
    } else if (1 == length(grep("^oxsatMm/Kg$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/kg), scale="Weiss")
    } else if (1 == length(grep("^oxsolML/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Garcia-Gordon")
    } else if (1 == length(grep("^oxsolMg/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Garcia-Gordon")
    } else if (1 == length(grep("^oxsolMm/Kg$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(umol/kg), scale="Garcia-Gordon")
    } else if (1 == length(grep("^cpar$", name))) {
        name <- "CPAR/Corrected Irradience"
        unit <- list(unit=expression(percent), scale="")
    } else if (1 == length(grep("^par[0-9]?$", name))) {
        name <- "par"
        unit <- list(unit=expression(), scale="Biospherical/Licor")
    } else if (1 == length(grep("^par/log$", name))) {
        name <- "par"
        unit <- list(unit=expression(log), scale="Satlantic")
    } else if (1 == length(grep("^ph$", name))) {
        name <- "pH"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^pr$", name))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="")
    } else if (1 == length(grep("^prdE$", name))) {
        ## Caution: English unit
        name <- "pressurePSI"
        unit <- list(unit=expression(psi), scale="")
    } else if (1 == length(grep("^prDE$", name))) {
        ## Caution: English unit
        name <- "pressurePSI"
        unit <- list(unit=expression(psi), scale="")
    } else if (1 == length(grep("^prM$", name))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="")
    } else if (1 == length(grep("^pr50M[0-9]?$", name))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="SBE50")
    } else if (1 == length(grep("^prDM$", name))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="Digiquartz")
    } else if (1 == length(grep("^pr[dS]M$", name))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="Strain Gauge")
    } else if (1 == length(grep("^ptempC$", name))) {
        name <- "pressureTemperature"
        unit <- list(unit=expression(degree*C), scale="ITS-90") # FIXME: guess on scale
    } else if (1 == length(grep("^potemp[0-9]*68C$", name))) {
        name <- "theta"
        unit <- list(unit=expression(degree*C), scale="ITS-68")
    } else if (1 == length(grep("^potemp[0-9]*90C$", name))) {
        name <- "theta"
        unit <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (1 == length(grep("^pumps$", name))) {
        name <- "pumpStatus"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^rhodflTC[0-1]{1}$", name))) {
        name <- "Rhodamine"
        unit <- list(unit=expression(ppb), scale="Turner Cyclops")
    } else if (1 == length(grep("^rhodflTCdiff$", name))) {
        name <- "RhodamineDifference"
        unit <- list(unit=expression(ppb), scale="Turner Cyclops")
    } else if (1 == length(grep("^sal[0-9]{2}$", name))) {
        name <- "salinity"
        unit <- list(unit=expression(), scale="PSS-78") # FIXME: guess on scale
    } else if (1 == length(grep("^sbe?ox[0-9]ML/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]Mg/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]Mm/Kg$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/kg), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]Mm/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/l), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]PS$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(percent), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]V$", name))) {
        name <- "oxygenRaw"
        unit <- list(unit=expression(V), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]dV/dT$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(dov/dt), scale="SBE43")
    } else if (1 == length(grep("^scan$", name))) {
        name <- "scan"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^secS-priS$", name))) {
        name <- "salinityDifference"
        unit <- list(unit=expression(), scale="PSS-78")
    } else if (1 == length(grep("^seaTurbMtr[1]?$", name))) {
        name <- "turbidity"
        unit <- list(unit=expression(FTU), scale="Seapoint")
    } else if (1 == length(grep("sigma-t[0-9]{2}", name))) {
        name <- "sigmaT"
        unit <- list(unit=expression(kg/m^3), scale="")
    ##} else if (1 == length(grep("sigma-.*[0-9]*", name, ignore.case=TRUE))) {
    } else if (1 == length(grep("^sigma", name))) {
        ## there are several cases, and we match the sigma-theta case
        ## by exclusion, because of limited understanding of how
        ## to match non-ascii characters on Windows machines.
        if (1 == length(grep("^sigma-t[0-9]{2}$", name))) {
            name <- "sigmaT"
        } else if (1 == length(grep("^sigma-1[0-9]{2}$", name))) {
            name <- "sigma1"
        } else if (1 == length(grep("^sigma-2[0-9]{2}$", name))) {
            name <- "sigma2"
        } else if (1 == length(grep("^sigma-3[0-9]{2}$", name))) {
            name <- "sigma3"
        } else if (1 == length(grep("^sigma-4[0-9]{2}$", name))) {
            name <- "sigma4"
        # 2022-06-28 } else if (1 == length(grep("^sigma-\x09[0-9]{2}$", name))) {
        #} else if (1 == length(grep("^sigma-\u00e9[0-9]{2}$", name))) {
        } else if (grepl("^sigma-.*[0-9]{2}$", name)
            && grepl("sigma-theta", nameAfterColon)) {
            name <- "sigmaTheta"
            # 2016-12-22 DK
            #
            # The above regexp matches for what we see in the supplied file
            #    system.file("extdata", "d201211_0011.cnv", package="oce")
            # at line 54, an acute-accented "e" (which maybe looked like a theta
            # to someone at SBE, when the format was invented. Clark found the
            # SBE docs and did some tests, which made it clear that this
            # accented "e" is always used, i.e. it is not just in some sample
            # files.
            #
            # 2022-07-15 DK
            #
            # Changes to R-devel with respect to encoding required changing code
            # for this, and throughout.  See
            # https://github.com/dankelley/oce/issues/1977 for details.
        } else {
            name <- "sigma" ## give up; this is a default
        }
        ## In all these cases, the unit is the same
        unit <- list(unit=expression(kg/m^3), scale="")
    } else if (1 == length(grep("^spar$", name))) {
        name <- "spar"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^specc$", name))) {
        name <- "conductivity"
        unit <- list(unit=expression(uS/cm), scale="")
    } else if (1 == length(grep("^sva$", name))) {
        name <- "specificVolumeAnomaly"
        unit <- list(unit=expression(10^-8*m^3/kg), scale="")
    } else if (1 == length(grep("^svCM[0-9]?$", name))) {
        name <- "soundSpeed"
        unit <- list(unit=expression(m/s), scale="Chen-Millero")
    } else if (1 == length(grep("^T2-T[01]68C$", name))) {
        name <- "temperatureDifference"
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (1 == length(grep("^T2-T[01]90C$", name))) {
        name <- "temperatureDifference"
        unit <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (1 == length(grep("^t[0-9]68((C)|(Cm))?$", name))) {
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (1 == length(grep("^t[0-9]90((C)|(Cm))?$", name))) {
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (name %in% c("t4968C", "tnc68C", "tv268C", "tnc268C", "t3868C", "t3836C1", "t38_68C")) {
        ## [1] p169-170
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (name %in% c("t4990C", "tnc90C", "tv290C", "tnc290C", "t3890C", "t3890C1", "t38_90C")) {
        ## [1] p169-170
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (1 == length(grep("^timeH$", name))) {
        name <- "timeH"
        unit <- list(unit=expression(hour), scale="")
    } else if (1 == length(grep("^timeJ$", name))) {
        name <- "timeJ"
        unit <- list(unit=expression(day), scale="")
    } else if (1 == length(grep("^timeJV2$", name))) {
        name <- "timeJV2"
        unit <- list(unit=expression(day), scale="")
    } else if (1 == length(grep("^timeK$", name))) {
        name <- "timeK"
        unit <- list(unit=expression(s), scale="since Jan 1, 2000")
    } else if (1 == length(grep("^timeM$", name))) {
        name <- "timeM"
        unit <- list(unit=expression(minute), scale="")
    } else if (1 == length(grep("^timeN$", name))) {
        name <- "timeN"
        unit <- list(unit=expression(s), scale="NMEA since Jan 1, 1970")
    } else if (1 == length(grep("^timeQ$", name))) {
        name <- "timeQ"
        unit <- list(unit=expression(s), scale="NMEA since Jan 1, 2000")
    } else if (1 == length(grep("^timeS$", name))) {
        name <- "timeS"
        unit <- list(unit=expression(s), scale="")
    } else if (1 == length(grep("^tsa$", name))) {
        name <- "thermostericAnomaly"
        unit <- list(unit=expression(10^-8*m^3/kg), scale="")
    } else if (1 == length(grep("^turbflTC[0-1]$", name))) {
        name <- "turbidity"
        unit <- list(unit=expression(NTU), scale="Turner Cyclops")
    } else if (1 == length(grep("^turbflTCdiff$", name))) {
        name <- "turbidityDifference"
        unit <- list(unit=expression(NTU), scale="Turner Cyclops")
    } else if (1 == length(grep("^turbWETbb[0-4]$", name))) {
        name <- "turbidity"
        unit <- list(unit=expression(1/m*sr), scale="WET Labs ECO")
    } else if (1 == length(grep("^turbWETbbdiff$", name))) {
        name <- "turbidityDifference"
        unit <- list(unit=expression(1/m*sr), scale="WET Labs ECO")
    } else if (1 == length(grep("^turbWETntu[0-5]$", name))) {
        name <- "turbidity"
        unit <- list(unit=expression(NTU), scale="WET Labs ECO")
    } else if (1 == length(grep("^turbWETntudiff$", name))) {
        name <- "turbidityDifference"
        unit <- list(unit=expression(NTU), scale="WET Labs ECO")
    } else if (1 == length(grep("^upoly[0-2]$", name))) {
        name <- "upoly"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^user[1-5]$", name))) {
        name <- "user"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^v[0-9][0-9]?$", name))) {
        unit <- list(unit=expression(V), scale="")
    } else if (1 == length(grep("^wetBAttn$", name))) {
        name <- "beamAttenuation"
        unit <- list(unit=expression(1/m), scale="WET Labs AC3")
    } else if (1 == length(grep("^wetBTrans$", name))) {
        name <- "beamTransmission"
        unit <- list(unit=expression(percent), scale="WET Labs AC3")
    } else if (1 == length(grep("^wetCDOM[0-5]{0,1}$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs CDOM")
    } else if (1 == length(grep("^wetCDOMdiff$", name))) {
        name <- "fluorescenceDifference"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs CDOM")
    } else if (1 == length(grep("^wetChAbs$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(1/m), scale="WET Labs AC3 absorption")
    } else if (1 == length(grep("^wetStar[0-9]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs WETstar")
    } else if (1 == length(grep("^wetStardiff$", name))) {
        name <- "fluorescenceDifference"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs WETstar")
    } else if (1 == length(grep("^xmiss[0-9]?$", name))) {
        name <- "beamTransmission"
        unit <- list(unit=expression(percent), scale="Chelsea/Seatech")
    } else {
        warning("unrecognized SBE name '", name, "'; consider using 'columns' to define this name")
        unit <- list(unit=expression(), scale="")
    }
    oceDebug(debug, " SBE name '", nameOriginal, "' converted to oce name '", name, "'; the scale is '", unit$scale, "'\n", sep="")
    oceDebug(debug, "} # cnvName2oceName()\n", unindent=1)
    list(name=name, nameOriginal=nameOriginal, unit=unit)
}


#' Read a Seabird CTD File
#'
#' @template readCtdTemplate
#'
#' @template encodingTemplate
#'
#' @param btl a logical value, with `TRUE` indicating that this is a `.BTL` file and `FALSE`
#' (the default) indicating a `.CNV` file.  Note that if `btl` is `TRUE`, the data column
#' names are taken directly from the file (without e.g. translating to `"Sal00"`
#' to `"salinity"`.  Also, the "avg" and "sdev" columns are blended together, with
#' all the latter named as in the file, but with `"_sdev"` appended.
#'
## @param humanDateFormat optional character string specifying the format for dates
## in the human-entered header line that starts with "`** Date:`". See the
## \dQuote{A note on hand-entered headers} section for the reason for this parameter.
## If supplied, then `humanDateFormat` is supplied as the `format` argument to
## [as.POSIXct()], which is supplied with the information on this date line.
#'
#' @author Dan Kelley and Clark Richards
#'
#' @details
#' This function reads files stored in Seabird `.cnv` format.
#' Note that these files can contain multiple sensors for a given field. For example,
#' the file might contain a column named `t090C` for one
#' temperature sensor and `t190C` for a second. The first will be denoted
#' `temperature` in the `data` slot of the return value, and the second
#' will be denoted `temperature1`. This means that the first sensor
#' will be used in any future processing that accesses `temperature`. This
#' is for convenience of processing, and it does not pose a limitation, because the
#' data from the second sensor are also available as e.g. `x[["temperature1"]]`,
#' where `x` is the name of the returned value.  For the details of the
#' mapping from `.cnv` names to `ctd` names, see [cnvName2oceName()].
#'
#' The names of the elements in the `data` slot of the returned value depend on
#' the file type, as signalled by the `btl` argument.  For the default case of `.cnv` files,
#' the original data names as stored in `file` are stored within the `metadata`
#' slot as `dataNamesOriginal`, and are displayed with `summary` alongside the
#' numerical summary; see the Appendix VI of reference 2 for the meanings of these
#' names (in the "Short Name" column of the table spanning pages 161 through 172).
#' However, for the case of `.btl` files, the column names are as described
#' in the documentation entry for the `btl` argument.
#'
#' @section A note on hand-entered headers:
#'
#' CNV files may have a section that contains human-entered information. This is
#' detected by `read.ctd.sbe()` as lines that begin with two asterisks. Decoding
#' this information can be tricky, because humans have many ways of writing
#' things.
#'
#' For example, consider the `date` item in the `metadata` slot of the returned
#' value.  `read.ctd.sbe()` infers this value in one of two ways.  First, if
#' there is a header line staring with
#'```
#'* NMEA UTC (Time) =
#'```
#' then that value is decoded and used for `date`.  This header line, preceded
#' by a single asterisk, is not human-entered, and so there is reason to hope
#' for a uniform format that can be handled by `read.ctd.sbe()`.  However, if
#' there is no NMEA header line, then `read.ctd.sbe()` will look for a line
#' starting with
#'```
#'** Date:
#'```
#' which was human-entered. This is the second choice, because humans write
#' dates in a bewildering variety of ways, and [as.POSIXct()], which
#' `read.ctd.sbe` uses to parse the date, cannot handle them all. If there is a
#' problem, `read.ctd.sbe()` issues a warning and stores NA in `date`.
#'
#' A similar error-detection procedure is used for human-entered location data,
#' which appear in lines starting with either
#'```
#'** Longitude:
#'```
#' or
#'```
#'** Latitude:
#'```
#' which often take forms that `read.ctd.sbe()` cannot parse.
#'
#' It is important to note that, even if no warnings are issued, there is a
#' reasonably high chance that human-entered data will be scanned incorrectly.
#' (Did the operator remember to indicate the hemisphere? Does 123.456 indicate
#' decimal degrees, or 123 degrees plus 45.6 minutes? Is hemisphere indicated by
#' sign or by letter, and, if the latter, where does it appear?)
#'
#' In deep-sea work, a ship might steam for 6 hours between CTD stations, so the
#' ship-time cost of each CTD file can be several thousand dollars.  Surely it
#' is not unreasonable for an analyst to take a minute to glance at the CNV
#' file, to ascertain whether `read.ctd.sbe()` inferred correct values.
#'
#' [oceSetMetadata()] is helpful for correcting problems with individual files,
#' but if many files are systematically problematic, say for a whole cruise or
#' perhaps even for a whole institution, then it might sense to set up a wrapper
#' function to correct deficiencies in the CNV files. As an example, the
#' following handles dates specified in a particular nonstandard way.
#'```
#' read.ctd.sbe.wrapper <- function(cnv)
#' {
#'     lines <- readLines(cnv)
#'     # Change month-day-year to year-month-day, so as.POSIXct() can parse it.
#'     lines <- gsub("^\\*\\* Date: (.*)-(.*)-(.*)", "** Date: \\3-\\1-\\2", lines)
#'     read.ctd.sbe(textConnection(lines))
#' }
#'```
#'
#' @section A note on sampling times:
#'
#' Until November of 2018, there was a possibility for great confusion in the
#' storage of the time entries within the `data` slot, because `read.ctd.sbe`
#' renamed each of the ten variants of time (see reference 2 for a list) as
#' `"time"` in the `data` slot of the returned value.  For CTD profiles, this
#' was perhaps not a great problem, but it could lead to significant confusion
#' for moored data. Therefore, a change to `read.ctd.sbe` was made, so that it
#' would Seabird times, using the `start_time` entry in the CNV file header
#' (which is stored as `startTime` in the object `metadata` slot), along with
#' specific time columns as follows (and as documented, with uneven clarity, in
#' the SBE Seasoft data processing manual, revision 7.26.8, Appendix VI):
#'
#' \tabular{rl}{
#' **Item** \tab **Meaning**\cr
#' `timeS`   \tab seconds elapsed since `start_time`\cr
#' `timeM`   \tab minutes elapsed since `start_time`\cr
#' `timeH`   \tab hours elapsed since `start_time`\cr
#' `timeJ`   \tab Julian days since the start of the year of the first observation\cr
#' `timeN`   \tab NMEA-based time, in seconds past Jan 1, 1970\cr
#' `timeQ`   \tab NMEA-based time, in seconds past Jan 1, 2000\cr
#' `timeK`   \tab NMEA-based time, in seconds past Jan 1, 2000\cr
#' `timeJV2` \tab as `timeJ`\cr
#' `timeSCP` \tab as `timeJ`\cr
#' `timeY`   \tab computer time, in seconds past Jan 1, 1970\cr
#'}
#' NOTE: not all of these times have been tested properly, and so users
#' are asked to report incorrect times, so that `read.ctd.sbe` can
#' be improved.
#'
#' @section A note on scales:
#'
#' The user might encounter data files with a variety of scales for temperature and
#' salinity. Oce keeps track of these scales in the units it sets up for the stored
#' variables. For example, if `A` is a CTD object, then
#' `A[["temperatureUnit"]]$scale` is a character string that will indicate the scale.
#' Modern-day data will have `"ITS-90"` for that scale, and old data may have
#' `"IPTS-68"`. The point of saving the scale in this way is so that the various
#' formulas that deal with water properties can account for the scale, e.g. converting
#' from numerical values saved on the `"IPTS-68"` scale to the newer scale, using
#' [T90fromT68()] before doing calculations that are expressed in
#' terms of the `"ITS-90"` scale. This is taken care of by retrieving temperatures
#' with the accessor function, e.g. writing `A[["temperature"]]` will either
#' retrieve the stored values (if the scale is ITS-90) or converted values (if
#' the scale is IPTS-68). Even though this procedure should work, users who
#' really care about the details of their data are well-advised to do a couple
#' of tests after examining the first data line of their data file in an editor.
#' Note that reading a file that contains IPTS-68 temperatures produces a warning.
#'
#' @examples
#' f <- system.file("extdata", "ctd.cnv", package="oce")
#' d <- read.ctd(f)
#'
#' @references
#' 1. The Sea-Bird SBE 19plus profiler is described at
#' `http://www.seabird.com/products/spec_sheets/19plusdata.htm`.  Some more
#' information is given in the Sea-Bird data-processing manual
#' (next item).
#'
#' 2. A SBE data processing manual was once at
#' `http://www.seabird.com/document/sbe-data-processing-manual`,
#' but as of summer 2018, this no longer seems to be provided by SeaBird.
#' A web search will turn up copies of the manual that have been put
#' online by various research groups and data-archiving agencies.
#' As of 2018-07-05, the latest version was named
#' `SBEDataProcessing_7.26.4.pdf` and had release date 12/08/2017,
#' and this was the reference version used in coding `oce`.
#'
#' @family functions that read ctd data
read.ctd.sbe <- function(file, columns=NULL, station=NULL, missingValue,
    deploymentType="unknown", btl=FALSE, monitor=FALSE,
    #humanDateFormat=NULL,
    encoding="latin1",
    debug=getOption("oceDebug"), processingLog, ...)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    if (is.character(file) && grepl("\\*", file, ignore.case=TRUE)) {
        oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") { # will read a series of files\n", unindent=1)
        files <- list.files(pattern=file)
        nfiles <- length(files)
        if (monitor)
            pb <- txtProgressBar(1, nfiles, style=3)
        res <- vector("list", nfiles)
        for (i in 1:nfiles) {
            res[[i]] <- read.ctd.sbe(files[i], debug=debug-1)
            if (monitor)
                setTxtProgressBar(pb, i)
        }
        oceDebug(debug, "} # read.ctd.sbe() {\n")
        return(res)
    }
    oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") {\n", unindent=1)

    ## Read Seabird data file.  Note on headers: '*' is machine-generated,
    ## '**' is a user header, and '#' is a post-processing header.
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        #<> message("1 FIXME: opening '", file, "' with encoding='", encoding, "'")
        file <- file(file, "r", encoding=encoding)
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        #<> message("2 FIXME: opening '", file, "' with encoding='", encoding, "'")
        open(file, "r", encoding=encoding)
        on.exit(close(file))
    }
    res <- new("ctd")
    ## Header
    scientist <- ship <- institute <- address <- cruise <- hexfilename <- ""
    sampleInterval <- NA
    sampleIntervalUnits <- ""
    systemUploadTime <- NULL
    latitude <- longitude <- NA
    waterDepth <- NA
    startTime <- recoveryTime <- NA
    date <- NA
    header <- c()
    ##conductivity.standard <- 4.2914
    foundHeaderLatitude <- foundHeaderLongitude <- FALSE
    serialNumber <- serialNumberConductivity <- serialNumberTemperature <- ""
    ## units$conductivity <- list(unit=expression(), scale="") # guess; other types are "mS/cm" and "S/m"
    ## units$temperature <- list(unit=expression(degree*C), scale="ITS-90") # guess; other option is IPTS-68
    pressureType <- "sea"              # guess; other option is "absolute"

    ## Silence warnings because binary files have 'NUL' characters that spew many warnings
    warn <- options("warn")$warn
    options(warn=-1)
    # 2022-07-15: drop encoding=, which is a problem for upcoming R.
    lines <- readLines(file)
    options(warn=warn)

    ## Get names and units of columns in the SBE data file
    nameLines  <- grep("^# name [0-9][0-9]* = .*:.*$", lines, ignore.case=TRUE)
    colUnits <- vector("list", length(nameLines))
    colNamesInferred <- NULL
    dataNamesOriginal <- list()
    namesUsed <- NULL
    #<> message("encoding=",encoding,"; see DAN")
    #<> DAN<<-list(lines=lines,nameLines=nameLines)
    #<> message("try\na<-with(DAN, lines[nameLines[23]])\nEncoding(a)")
    for (iline in seq_along(nameLines)) {
        nu <- cnvName2oceName(lines[nameLines[iline]], columns, debug=debug-1)
        #<> if (iline==23)
        #<> message("iline=", iline, ", nu$name='", nu$name, "', nu$nameOriginal='", nu$nameOriginal, "'")
        if (nu$name %in% namesUsed) {
            trial <- 2
            while (paste(nu$name, trial, sep="") %in% namesUsed) {
                trial <- trial + 1
                if (trial > 10L) {
                    warning("stopped renaming ", nu$name, "after got to 10 variants\n")
                    break
                }
            }
            nu$name <- paste(nu$name, trial, sep="")
        }
        namesUsed <- c(namesUsed, nu$name)
        dataNamesOriginal[[nu$name]] <- nu$nameOriginal
        colUnits[[iline]] <- nu$unit
        colNamesInferred <- c(colNamesInferred, nu$name)
    }
    colNamesInferred <- unduplicateNames(colNamesInferred)
    names(colUnits) <- colNamesInferred
    ##print(colUnits)
    ## names(dataNamesOriginal) <- colNamesInferred
    res@metadata$dataNamesOriginal <- dataNamesOriginal
    ##found.scan <- "scan" %in% colNamesInferred
    ##found.temperature <- "temperature" %in% colNamesInferred
    foundPressure <- "pressure" %in% colNamesInferred
    foundSalinity <- "salinity" %in% colNamesInferred
    ##found.time <- "time" %in% colNamesInferred
    foundDepth <- "depth" %in% colNamesInferred
    foundConductivity <- "conductivity" %in% colNamesInferred
    foundConductivityRatio <- "conductivity.ratio" %in% colNamesInferred
    ## FIXME: should we insist on having salinity, temperature, and pressure?
    fileType <- "unknown"

    for (iline in seq_along(lines)) {
        line <- lines[iline]
        ##message(line)
        #line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        oceDebug(debug > 1L, paste("Examining header line ", iline, " '", line, "'\n", sep=""))
        header <- c(header, line)
        ##if (length(grep("\*END\*", line))) #BUG# why is this regexp no good (new with R-2.1.0)
        aline <- iconv(line, from="UTF-8", to="ASCII", sub="?")
        if (length(grep("^\\s*\\*END\\*\\s*$", aline, perl=TRUE))) {
            ##message("got *END* at line ", iline)
            ## Sometimes SBE files have a header line after the *END* line.
            iline <- iline + 1
            if (length(grep("[a-cf-zA-CF-Z]", lines[iline])))
                iline <- iline + 1
            break
        }
        ##if (iline>129) browser()
        lline <- tolower(aline)
        # Use NMEA (if present) in preference to a hand-entered date.
        # See https://github.com/dankelley/oce/issues/1949#issuecomment-1133613831
        # * NMEA UTC (Time) = Aug 09 2012 06:34:34
        if (grepl("^\\* NMEA.*Time.*=", aline)) {  # NMD
            rhs <- trimws(gsub("^\\* NMEA.*Time.*=(.*)", "\\1", aline))
            dateTry <- try(as.POSIXct(rhs,
                    tryFormats=c("%Y-%m-%d %H:%M:%S", "%b %d %Y %H:%M:%S"),
                    tz="UTC"), silent=TRUE)
            if (inherits(dateTry, "try-error")) {
                warning("cannot parse date in `", aline, "`, but will try a '** Date:' line, if there is one", sep="")
            } else {
                date <- dateTry
                oceDebug(debug, "inferred date=", format(date), " from automatically-generated NMEA-time line\n")
            }
        }
        # Look at hand-entered date only if an NMEA time was not found, or was unparseable
        if (is.na(date) && grepl("^\\*\\*.*Date:", aline)) {
            #> message(aline)
            dateString <- gsub(".*date:(.*)","\\1", lline)
            #> message(dateString)
            # Remove a timezone, if there is one, because timezones are often
            # contradictory, e.g. AST could be a time in Atlantic Canada, or
            # Australia, or in some other place.
            #>>>dateString <- gsub(" [a-zA-Z]$", "", dateString)
            #> message(dateString)
            dateString <- trimws(dateString)
            #> message(dateString)
            dateTry <- try(as.POSIXct(dateString, tz="UTC"), silent=TRUE)
            if (inherits(dateTry, "try-error")) {
                warning("cannot parse date in `", aline, "`; see 'A note on hand-entered headers' in ?read.ctd.sbe", sep="")
            } else {
                if (dateTry < as.POSIXct("1900-01-01")) {
                    warning("impossible date in `", aline, "` is being ignored\n", sep="")
                } else {
                    oceDebug(debug, "inferred date=", format(date), " from `", aline, "`\n", sep="")
                    date <- dateTry
                }
            }
            #>>> if (!is.null(humanDateFormat)) {
            #>>>     dateTry <- try(as.POSIXct(dateString, format=humanDateFormat, tz="UTC"), silent=TRUE)
            #>>>     if (inherits(dateTry, "try-error")) {
            #>>>         warning("cannot decode human-entered date in header line `", aline, "` using humanDateFormat=`", humanDateFormat, "`\n", sep="")
            #>>>         date <- NA
            #>>>     } else {
            #>>>         date <- dateTry
            #>>>     }
            #>>>     #> message("date=", date, " with humanDateFormat")
            #>>> } else {
            #>>>     dateTry <- try(as.POSIXct(dateString, tz="UTC"), silent=TRUE)
            #>>>     if (inherits(dateTry, "try-error")) {
            #>>>         warning("cannot decode human-entered date in header line `", aline, "`. Try supplying humanDateFormat\n", sep="")
            #>>>         date <- NA
            #>>>     } else {
            #>>>         date <- dateTry
            #>>>     }
            #>>>     #> message("date=", date, " without humanDateFormat")
            #>>> }
        }
        if (0 < regexpr(".*seacat profiler.*", lline))
            serialNumber <- gsub("[ ].*$", "", gsub(".*sn[ ]*", "", lline))
        if (length(grep("^\\* Temperature SN", lline, ignore.case=TRUE)))
            serialNumberTemperature <- gsub("^.*=\\s", "", lline)
        if (length(grep("^\\* Conductivity SN", lline, ignore.case=TRUE)))
            serialNumberConductivity <- gsub("^.*=\\s", "", lline)
        ##20181014 (issue 1460): if (0 < (r<-regexpr("date:", lline))) {
        ##20181014 (issue 1460):     oceDebug(debug, "found 'date:' header line\n")
        ##20181014 (issue 1460):     d <- sub("(.*)date:([ ])*", "", lline)
        ##20181014 (issue 1460):     date <- decodeTime(d, "%Y%m%d") # e.g. 20130701 Canada Day
        ##20181014 (issue 1460): }
        if (length(grep("^#[ \t]*file_type[ \t]*=[ \t]*", lline))) {
            ## file_type = ascii
            ## file_type = binary
            fileType <- gsub("[ \t\n]+$", "", gsub(".*=[ \t]*", "", lline))
            ##> message("fileType='", fileType, "'")
            ##> message("lline='", lline, "'")
        }
        ##* NMEA UTC (Time) = Jul 28 2011  04:17:53
        ##* system upload time = jan 26 2010 13:02:57
        #> See https://github.com/dankelley/oce/issues/1949#issuecomment-1133053873
        #> if (length(grep("^\\* .*time.*=.*$", lline))) {
        #>     if (is.na(date) && 0 == length(grep("real-time sample interval", lline))) {
        #>         warning("inferring date from `", aline, "`. This behaviour is slated for removal!!!\n")
        #>         #oceDebug(debug, "found 'real-time sample interval' header line\n")
        #>         d <- sub(".*=", "", lline)
        #>         d <- sub("^ *", "", d)
        #>         d <- sub(" *$", "", d)
        #>         date <- decodeTime(d)
        #>         message("L987: date=", date[1], " DANDANDAN\n")
        #>     }
        #> }
        if (length(grep("^\\* Sea-Bird SBE (.*) Data File:$", lline, ignore.case=TRUE))) {
            model <- gsub("^\\* sea-bird sbe (.*) data file:$", "\\1", lline)
            res@metadata$model <- model
        }
        if (0 < (r<-regexpr("filename", lline)))
            hexfilename <- sub("(.*)FileName =([ ])*", "", ignore.case=TRUE, lline)
        if (0 < (r<-regexpr("system upload time", lline))) {
            d <- sub("([^=]*)[ ]*=[ ]*", "", ignore.case=TRUE, lline)
            systemUploadTime <- decodeTime(d)
            oceDebug(debug, " systemUploadTime ", format(systemUploadTime), " inferred from substring '", d, "'\n", sep="")
        }
        ## Styles:
        ## * NMEA Latitude = 47 54.760 N
        ## ** Latitude:      47 53.27 N
        if (!foundHeaderLatitude && (0 < (r<-regexpr("latitude*[0-8]*", lline, ignore.case=TRUE)))) {
            latitude <- parseLatLon(lline, debug=debug-1)
            foundHeaderLatitude <- TRUE
        }
        if (!foundHeaderLongitude && (0 < (r<-regexpr("longitude*[0-8]*", lline, ignore.case=TRUE)))) {
            longitude <- parseLatLon(lline, debug=debug-1)
            foundHeaderLongitude <- TRUE
        }
        if (0 < (r<-regexpr("start_time =", lline))) {
            d <- sub("#[ ]*start_time[ ]*=[ ]*", "", lline)
            startTime <- decodeTime(d)
            oceDebug(debug, " startTime ", format(startTime), "' inferred from substring '", d, "'\n", sep="")
        }
        if (0 < (r<-regexpr("ship:", lline))) {
            ship <- sub("(.*)ship:([ \t])*", "", ignore.case=TRUE, line) # note: using full string
            ship <- sub("[ \t]*$", "", ship)
        }
        if (0 < (r<-regexpr("scientist:", lline)))
            scientist <- sub("(.*)scientist:([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("chef", lline)))
            scientist <- sub("(.*):([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("institute:", lline)))
            institute <- sub("(.*)institute:([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("address:", lline)))
            address <- sub("(.*)address:([ ])*", "", ignore.case=TRUE, line) # full string
        if (0 < (r<-regexpr("cruise:", lline))) {
            cruise <- sub("(.*)cruise:([ ])*", "", ignore.case=TRUE, line) # full string
            cruise <- sub("[ ]*$", "", ignore.case=TRUE, cruise) # full string
        }
        if (is.null(station)) {
            if (0 < (r<-regexpr("station:", lline)))
                station <- sub("[ ]*$", "", sub("(.*)station:([ ])*", "", ignore.case=TRUE, line))
            if (0 < (r<-regexpr("station_name:", lline)))
                station <- sub("[ ]*$", "", sub("(.*)station_name:([ ])*", "", ignore.case=TRUE, line))
        }
        if (0 < (r<-regexpr("recovery:", lline)))
            recoveryTime <- sub("(.*)recovery:([ ])*", "", lline)

        if (length(grep("^#[ \t]+bad_flag[ \t]*=", lline))) {
            ## bad_flag = -9.990e-29
            bad_flag <- sub("#[ \t]*bad_flag[ \t]*=[ \t]*", "", lline)
            if (missing(missingValue))
                missingValue <- as.numeric(bad_flag)
        }
        # Water depth
        # See https://github.com/dankelley/oce/issues/1950
        if (grepl("^\\*\\* Depth.*:.*$", aline)
            || grepl("^\\*\\* Water Depth.*:.*$", aline)
            || grepl("^\\*\\* profondeur.*:.*$", aline, ignore.case=TRUE)) {
            look <- gsub(".*:", "", lline)
            #> ## "** Depth (m): 3447 "
            #> look <- sub("[a-z:()]*", "", lline, ignore.case=TRUE)
            #> look <- gsub("^[*a-zA-Z\\(\\) :]*", "", lline, ignore.case=TRUE)
            #> look <- gsub("[ ]*", "", look, ignore.case=TRUE)
            #> oceDebug(debug, "    pruned '", aline, "' to '", look, "'\n", sep="")
            # Remove any non-numeric (e.g. sometimes a unit is here)
            look <- trimws(gsub("[-a-zA-Z]", "", look))
            waterDepth<- as.numeric(look)
            oceDebug(debug, "inferred waterDepth=", waterDepth, "[m] from '", aline, "'\n", sep="")
        }
        # if (0 < (r<-regexpr("water depth:", lline))
        #     || 0 < (r<-regexpr(pattern="profondeur", text=lline))) {
        #     ## Examples from files in use by author:
        #     ##** Profondeur: 76
        #     ##** Water Depth:   40 m
        #     look <- sub("[ ]*$", "", sub(".*:[ ]*", "", lline))
        #     linesplit <- strsplit(look, " ")[[1]]
        #     nitems <- length(linesplit)
        #     if (nitems == 1) {
        #         waterDepth <- as.numeric(linesplit[1])
        #     } else if (nitems == 2) {
        #         unit <- linesplit[2]
        #         if (unit == "m") {
        #             waterDepth <- as.numeric(linesplit[1])
        #         } else if (unit == "km") {
        #             waterDepth <- 1000 * as.numeric(linesplit[1])
        #         } else {
        #             warning("ignoring unit on water depth '", look, "'")
        #         }
        #     } else {
        #         warning("cannot interpret water depth from '", lline, "'")
        #     }
        # }
        ## [1] "# interval = seconds: 1
        ## [1] "# interval = decibars: 1
        if (length(grep("^# interval = .*$", lline))) {
            ##print(lline)
            value <- gsub("^.*:[ ]*([0-9.]*)[ ]*$", "\\1", lline)
            ##cat("value='", value, "'\n", sep="")
            ##cat("value='", as.numeric(value), "'\n", sep="")
            units <- gsub("^.*=[ ]*(.*):(.*)$", "\\1", lline)
            ##cat("units='", units, "'\n", sep="")
            sampleInterval <- as.numeric(value)
            if (units == "seconds")
                units <- "s"
            else if (units == "decibars")
                units <- "dbar"
            ##cat("units='", units, "'\n", sep="")
            sampleIntervalUnits <- units
        } else if (0 < (r<-regexpr("^. sample rate =", lline))) {
            ## * sample rate = 1 scan every 5.0 seconds
            rtmp <- lline
            rtmp <- sub("(.*) sample rate = ", "", rtmp)
            rtmp <- sub("scan every ", "", rtmp)
            rtmp <- strsplit(rtmp, " ")
            ##      if (length(rtmp[[1]]) != 3)
            ##        warning("cannot parse sample-rate string in `",line,"'")
            sampleInterval <- as.double(rtmp[[1]][2]) / as.double(rtmp[[1]][1])
            sampleIntervalUnits <- "s"
            if (rtmp[[1]][3] == "minutes") {
                sampleInterval <- sampleInterval / 60
            } else {
                if (rtmp[[1]][3] == "hours") {
                    sampleInterval <- sampleInterval / 3600
                } else {
                    warning("cannot understand `", rtmp[[1]][2], "' as a unit of time for sampleInterval")
                }
            }
        }
    }
    oceDebug(debug, "Finished reading header\n")
    if (debug > 0) {
        if (is.nan(sampleInterval))
            warning("'* sample rate =' not found in header")
        if (is.nan(latitude))
            warning("'** Latitude:' not found in header")
        if (is.na(longitude))
            warning("'** Longitude:' not found in header")
        if (is.na(date))
            warning("'** Date:' not found in header")
        if (is.na(recoveryTime))
            warning("'** Recovery:' not found in header")
    }
    ## Require p,S,T data at least
    if (!btl && !("temperature" %in% colNamesInferred))
        warning("cannot find temperature; try using the 'columns' argument")

    res@metadata$header <- header
    res@metadata$type <- "SBE"
    res@metadata$hexfilename <- hexfilename # from instrument
    res@metadata$serialNumber <- serialNumber
    res@metadata$serialNumberTemperature <- serialNumberTemperature
    res@metadata$serialNumberConductivity <- serialNumberConductivity
    res@metadata$pressureType <- pressureType
    res@metadata$units <- colUnits
    names(res@metadata$units) <- colNamesInferred
    res@metadata$systemUploadTime <- systemUploadTime
    res@metadata$ship <- ship
    res@metadata$scientist <- scientist
    res@metadata$institute <- institute
    res@metadata$address <- address
    res@metadata$cruise <- cruise
    res@metadata$station <- station
    res@metadata$deploymentType <- deploymentType
    res@metadata$date <- date
    if (!is.na(startTime) && startTime < as.POSIXct("1950-01-01"))
        warning("startTime (", startTime, ") is < 1950, suggesting a turn-of-the-century problem in this cnv file")
    res@metadata$startTime <- startTime
    if (!is.na(recoveryTime) && recoveryTime < as.POSIXct("1950-01-01"))
        warning("recoveryTime < 1950, suggesting y2k problem in this cnv file")
    res@metadata$recoveryTime <- recoveryTime
    #res@metadata$time <- date          # standardized name
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$waterDepth <- waterDepth # if NA, will update later
    res@metadata$sampleInterval <- sampleInterval
    res@metadata$sampleIntervalUnits <- sampleIntervalUnits
    #res@metadata$names <- colNamesInferred
    #res@metadata$labels <- colNamesInferred
    res@metadata$filename <- filename
    if (fileType == "binary") {
        warning("can only handle non-binary .cnv files")
        res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        return(res)
    }
    # Read the data as a table.
    if (btl) {
        pushBack(lines, file) # push back header so we can read from a file, not a text vector (for speed)
        # The .BTL format was reverse-engineered by inspecting some files, since I could
        # not discover any documentation on the format.  Therefore, there is a good chance
        # of errors in this code.  See https://github.com/dankelley/oce/issues/1681
        oceDebug(debug, "About to read .btl data\n")
        dataHeaderStartLine <- grep("^[^#^*]", lines)[1]
        if (!length(dataHeaderStartLine))
            stop("cannot find the start of .btl data")
        colNames <- tail(strsplit(lines[dataHeaderStartLine], "[ ]+")[[1]], -1)
        colNames <- c(colNames, "type") # tack on col for "(avg)" or "(sdev)"
        oceDebug(debug, "colNames=c(\"", paste(colNames, collapse="\", \""), "\")\n", sep="")
        lastLine <- length(lines)
        iodd <- seq(dataHeaderStartLine + 2, lastLine, 2)
        ieven <- seq(dataHeaderStartLine + 3, lastLine, 2)
        ## Check that we have the rows interpreted correctly by examining the final column.
        if (any(!grepl("^.*\\(avg\\)$", lines[iodd])))
            stop("odd-numbered data lines in .btl files must end with `(avg)`, but lines ", paste(grep("(avg)$", lines[iodd], invert=TRUE), collapse=","), " do not")
        if (any(!grepl("^.*\\(sdev\\)$", lines[ieven])))
            stop("even-numbered data lines in .btl files must end with `(sdev)`, but lines ", paste(grep("(sdev)$", lines[ieven], invert=TRUE), collapse=","), " do not")
        # It's a multistep process, working with this odd paired-line format.  We
        # divide it into steps, in hopes of making it easier to modify the code later,
        # in case this fails on some files, or there is a need to change the output
        # scheme.
        dataInterwoven <- utils::read.fwf(file, widths=rep(11, length(colNames)), skip=1+dataHeaderStartLine,
                                          col.names=colNames)
        ndataInterwoven <- dim(dataInterwoven)[1]
        # Break up into two dataframes
        iavgs <- seq(1, ndataInterwoven, by=2)
        isdevs <- seq(2, ndataInterwoven, by=2)
        avg <- dataInterwoven[iavgs, ]
        sdev <- dataInterwoven[isdevs,]
        # Get time-of-day from sdev, then trim out NA columns, and finally rename columns
        hms <- gsub(" ", "", sdev$Date)
        sdevValid <- !is.na(sdev[1,]) & !grepl("Date", names(sdev))
        sdev <- sdev[, sdevValid]
        names(sdev) <- paste0(names(sdev), "_sdev")
        ## Recombine, then trim the "type" columns, which we kept only for testing, so far
        data <- cbind(avg, sdev)
        trimCols <- grep("^(type)|(typeSdev)$", names(data))
        if (2 != length(trimCols))
            stop("expecting 2 'type' columns to trim, but found ", length(trimCols))
        data <- data[, -trimCols]
        data$time <- as.POSIXct(paste(data$Date, hms), format="%b %d %Y %H:%M:%S", tz="UTC")
        data <- data[, -which(names(data) == "Date")]
        haveData <- TRUE
        names <- colNames # used later (perhaps incorrectly, since we don't have flags etc for .btl files)
    } else {
        pushBack(lines, file) # push back header so we can read from a file, not a text vector (for speed)
        oceDebug(debug, "About to read .cnv data with these names: c(\"", paste(colNamesInferred, collapse='","'), "\")\n", sep="")
        #message("skipping ", iline-1, " lines at top of file")
        data <- as.list(read.table(file, skip=iline-1L, header=FALSE, encoding=encoding))

        if (length(data) != length(colNamesInferred))
            stop("Number of columns in .cnv data file (", length(data), ") must match number of variables named in the header (", length(colNamesInferred), ")")
        names(data) <- colNamesInferred
        ndata <- length(data[[1]])
        if (0 < ndata) {
            haveData <- TRUE
            names <- names(data)
            #labels <- names
            # if (!found.scan) {
            #     data$scan <- 1:ndata
            #     names <- names(data)
            #     colNamesInferred <- c(colNamesInferred, "scan")
            #     colNamesOriginal <- c(colNamesOriginal, "scan")
            # }
        } else {
            haveData <- FALSE
            warning("no data in CTD file \"", filename, "\"")
            data <- list(scan=NULL, salinity=NULL, temperature=NULL, pressure=NULL)
        }
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    #hitem <- processingLogItem(processingLog)
    # replace any missingValue with NA
    if (!missing(missingValue) && !is.null(missingValue)) {
        for (item in names(data)) {
            data[[item]] <- ifelse(data[[item]]==missingValue, NA, data[[item]])
        }
    }
    res@data <- data
    # Add standard things, if missing
    if (haveData) {
        if (!foundSalinity) {
            if (foundConductivityRatio) {
                C <- data$conductivityratio
                S <- swSCTp(C, data$temperature, data$pressure)
                res <- oceSetData(res, name="salinity", value=S,
                                  unit=list(unit=expression(), scale="PSS-78"))
                warning("created 'salinity' from 'temperature', 'conductivity' and 'pressure'", immediate.=TRUE)
            } else if (foundConductivity) {
                C <- data$conductivity
                if (!is.null(res@metadata$units$conductivity)) {
                    unit <- as.character(res@metadata$units$conductivity$unit)
                    # Conductivity Ratio is conductivity divided by 42.914 mS/cm (Culkin and Smith 1980
                    # see ?read.rsk for full citation)
                    if (length(unit)) {
                        oceDebug(debug, "'columns' indicates that the conductivity unit is '", unit, "'\n", sep="")
                        if ("uS/cm" == unit) {
                            C <- C / 429.14
                        } else if ("mS/cm" == unit) {
                            C <- C / 42.914 # e.g. RSK
                        } else if ("S/m" == unit) {
                            C <- C / 4.2914
                        } else {
                            warning("unrecognized conductivity unit '", unit, "'; assuming unitless for salinity calculation -- results should be used with caution", immediate.=TRUE)
                        }
                    } else {
                        warning("missing conductivity unit, so assuming unitless for salinity calculation -- results should be used with caution", immediate.=TRUE)
                    }
                } else {
                    warning("missing conductivity unit; guessing a unit based on maximum value", immediate.=TRUE)
                    cmax <- max(C, na.rm=TRUE)
                    if (cmax > 10) {
                        warning("max(conductivity) > 10, so using using conductivity/42.914 as a conductivity ratio for computation of salinity", immediate.=TRUE)
                        C <- C / 42.914
                    } else if (cmax > 1) {
                        warning("max(conductivity) between 1 and 10, so using using conductivity/4.2914 as a conductivity ratio for computation of salinity", immediate.=TRUE)
                        C <- C / 4.2914
                    }
                }
                S <- swSCTp(C, data$temperature, data$pressure)
                res <- oceSetData(res, name="salinity", value=S,
                                  unit=list(unit=expression(), scale="PSS-78"))
                warning("created 'salinity' from 'temperature', 'conductivity' and 'pressure'", immediate.=TRUE)
            } else {
                warning("cannot find salinity or conductivity in .cnv file; try using columns argument if the file actually contains these items", immediate.=TRUE)
            }
        }
        if ("pressurePSI" %in% names && !("pressure" %in% names)) {
            ## DK 20170114: I cannot find what I consider to be a definitive source, so
            ## I am taking the wikipedia value.
            ## 0.6894757293168  https://en.wikipedia.org/wiki/Pounds_per_square_inch
            ## 0.689475728      http://www.convertunits.com/from/psi/to/decibar
            res <- oceSetData(res, name="pressure", value=res@data$pressurePSI*0.6894757293168,
                              unit=list(unit=expression("dbar"), scale=""))
            warning("created 'pressure' from 'pressurePSI'")
        } else if (foundDepth && !foundPressure) {
            ## BUG: this is a poor, nonrobust approximation of pressure
            g <- if (foundHeaderLatitude) gravity(latitude) else 9.8
            rho0 <- 1000 + swSigmaTheta(median(res[["salinity"]]), median(res[["temperature"]]), 0)
            ## res <- ctdAddColumn(res, res@data$depth * g * rho0 / 1e4, name="pressure", label="Pressure",
            ##                     unit=list(unit=expression("dbar"), scale=""), debug=debug-1)
            res <- oceSetData(res, name="pressure", value=res@data$depth * g * rho0 / 1e4,
                              unit=list(unit=expression("dbar"), scale=""))
            ## colNamesOriginal <- c(colNamesOriginal, "NA")
            warning("created 'pressure' from 'depth'")
        }
    }
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))

    if (("temperature" %in% names(res@metadata$units)) && res@metadata$units$temperature$scale == "IPTS-68") {
        warning("this CNV file has temperature in the IPTS-68 scale and this is stored in the object, but note that [[\"temperature\"]] and the sw* functions convert the numbers to ITS-90 values")
    }
    # Note: previously, at this spot, there was code to switch from the IPTS-68 scale
    # to the ITS-90 scale. The old-scale data were saved in a column named
    # "temperature68". However, that scheme could be confusing both in oce code and
    # in user code, and it became unnecessary when the scale started being
    # stored in the unit. See the "note on scales" in the documentation for
    # the scheme used to prevent problems.
    oceDebug(debug, "} # read.ctd.sbe()\n")
    res
}
