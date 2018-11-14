#' Infer variable name, units and scale from a Seabird (.cnv) header line
#'
#' This function is used by \code{\link{read.ctd.sbe}} to infer data names
#' and units from the coding used by Teledyne/Seabird (SBE) \code{.cnv}
#' files.  Lacking access to documentation on the SBE format,
#' the present function is based on inspection of a suite of CNV files
#' available to the \code{oce} developers.
#'
#' A few sample header lines that have been encountered are:
#'\preformatted{
#' # name 4 = t068: temperature, IPTS-68 [deg C]
#' # name 3 = t090C: Temperature [ITS-90, deg C]
#' # name 4 = t190C: Temperature, 2 [ITS-90, deg C]
#'}
#' Examination of several CNV files suggests that it is best to
#' try to infer the name from the characters between the "\code{=}"
#' and "\code{:}" characters, because the material after the colon
#' seems to vary more between sample files.
#'
#' The table given below indicates the translation patterns used. These are
#' taken from [1]. The \code{.cnv} convention for multiple sensors is to
#' include optional extra digits in the name, and these are indicated
#' with \code{~} in the table; their decoding is done with \code{\link{grep}}.
#'
#' It is important to note that this table is by no means complete, since there
#' are a great many SBE names listed in their document [1], plus names
#' not listed there but present in data files
#' supplied by prominent archiving agencies. If an SBE name is not recognized,
#' then the oce name is set to that SBE name. This can cause problems in
#' some other processing steps (e.g. if \code{\link{swRho}} or a similar
#' function is called with an \code{oce} object as first argument), and so
#' users are well-advised to rename the items as appropriate. The first
#' step in doing this is to pass the object to \code{summary()}, to discover
#' the SBE names in question. Then consult the SBE documentation to find
#' an appropriate name for the data, and either manipulate the names in the object
#' data slot directly or use
#' \code{\link{renameData}} to rename the elements. Finally, please publish
#' an 'issue' on the oce Github site \url{https://github.com/dankelley/oce/issues}
#' so that the developers can add the data type in question. (To save
#' development time, there is no plan to add all possible data types without
#' a reasonable and specific expression user interest. Oxygen alone has over forty
#' variants.)
#'
#' \tabular{llll}{
#'   \strong{Key}       \tab \strong{Result}                     \tab \strong{Unit;scale} \tab \strong{Notes} \cr
#'   \code{alt}         \tab \code{altimeter}                    \tab m                   \tab    \cr
#'   \code{altM}        \tab \code{altimeter}                    \tab m                   \tab    \cr
#'   \code{accM}        \tab \code{acceleration}                 \tab m/s^2               \tab    \cr
#'   \code{bat~}        \tab \code{beamAttenuation}              \tab 1/m                 \tab    \cr
#'   \code{C2-C1S/m}    \tab \code{conductivityDifference}       \tab S/m                 \tab    \cr
#'   \code{C2-C1mS/cm}  \tab \code{conductivityDifference}       \tab mS/cm               \tab    \cr
#'   \code{C2-C1uS/cm}  \tab \code{conductivityDifference}       \tab uS/cm               \tab    \cr
#'   \code{c~mS/cm}     \tab \code{conductivity}                 \tab mS/cm               \tab    \cr
#'   \code{cond~mS/cm}  \tab \code{conductivity}                 \tab mS/cm               \tab    \cr
#'   \code{c~S/m}       \tab \code{conductivity}                 \tab S/m                 \tab    \cr
#'   \code{cond~S/m}    \tab \code{conductivity}                 \tab S/m                 \tab    \cr
#'   \code{c~uS/cm}     \tab \code{conductivity}                 \tab uS/cm               \tab    \cr
#'   \code{cond~uS/cm}  \tab \code{conductivity}                 \tab uS/cm               \tab    \cr
#'   \code{CStarAt~}    \tab \code{beamAttenuation}              \tab 1/m                 \tab    \cr
#'   \code{CStarTr~}    \tab \code{beamTransmission}             \tab percent             \tab    \cr
#'   \code{density~~}   \tab \code{density}                      \tab kg/m^3              \tab    \cr
#'   \code{depS}        \tab \code{depth}                        \tab m                   \tab    \cr
#'   \code{depSM}       \tab \code{depth}                        \tab m                   \tab    \cr
#'   \code{depF}        \tab \code{depth}                        \tab m                   \tab    \cr
#'   \code{depFM}       \tab \code{depth}                        \tab m                   \tab    \cr
#'   \code{dz/dtM}      \tab \code{descentRate}                  \tab m/s                 \tab    \cr
#'   \code{f~}          \tab \code{frequency}                    \tab Hz                  \tab    \cr
#'   \code{f~~}         \tab \code{frequency}                    \tab Hz                  \tab    \cr
#'   \code{flC~}        \tab \code{fluorescence}                 \tab ug/l; Chelsea Aqua 3\tab    \cr
#'   \code{flCM}        \tab \code{fluorescence}                 \tab ug/l; Chelsea Mini Chl Con\tab\cr
#'   \code{flCUVA~}     \tab \code{fluorescence}                 \tab ug/l; Chelsea UV Aquatracka\tab\cr
#'   \code{flEC-AFL~}   \tab \code{fluorescence}                 \tab mg/m^3; WET Labs ECO-AFL/FLtab\cr
#'   \code{flS}         \tab \code{fluorescence}                 \tab -; Seatech          \tab    \cr
#'   \code{flScufa~}    \tab \code{fluorescence}                 \tab -; Turner SCUFA [RFU]\tab\cr
#'   \code{flSP}        \tab \code{fluorescence}                 \tab -; Seapoint         \tab    \cr
#'   \code{flSPR}       \tab \code{fluorescence}                 \tab -; Seapoint, Rhodamine\tab  \cr
#'   \code{flSPuv}      \tab \code{fluorescence}                 \tab -; Seapoint, UV      \tab   \cr
#'   \code{flT}         \tab \code{fluorescence}                 \tab -; Turner 10-005 flT\tab   \cr
#'   \code{gpa}         \tab \code{geopotentialAnomaly}          \tab -; J/kg              \tab   \cr
#'   \code{latitude}    \tab \code{latitude}                     \tab degN                 \tab   \cr
#'   \code{longitude}   \tab \code{longitude}                    \tab degE                 \tab   \cr
#'   \code{n2satML/L}   \tab \code{nitrogenSaturation}           \tab ml/l                 \tab   \cr
#'   \code{n2satMg/L}   \tab \code{nitrogenSaturation}           \tab mg/l                 \tab   \cr
#'   \code{n2satumol/kg}\tab \code{nitrogenSaturation}           \tab umol/kg              \tab   \cr
#'   \code{nbin}        \tab \code{nbin}                         \tab                      \tab   \cr
#'   \code{obsscufa~}   \tab \code{backscatter}                  \tab NTU; Turner SCUFA    \tab   \cr
#'   \code{opoxMg/L}    \tab \code{oxygen}                       \tab mg/l; Optode, Anderaa\tab   \cr
#'   \code{opoxML/L}    \tab \code{oxygen}                       \tab ml/l; Optode, Anderaa\tab   \cr
#'   \code{opoxMm/L}    \tab \code{oxygen}                       \tab umol/l; Optode, Anderaa\tab \cr
#'   \code{opoxPS}      \tab \code{oxygen}                       \tab percent; Optode, Anderaa\tab   \cr
#'   \code{oxsatML/L}   \tab \code{oxygen}                       \tab ml/l; Weiss          \tab   \cr
#'   \code{oxsatMg/L}   \tab \code{oxygen}                       \tab mg/l; Weiss          \tab   \cr
#'   \code{oxsatMm/Kg}  \tab \code{oxygen}                       \tab umol/kg; Weiss       \tab   \cr
#'   \code{oxsolML/L}   \tab \code{oxygen}                       \tab ml/l; Garcia-Gordon  \tab   \cr
#'   \code{oxsolMg/L}   \tab \code{oxygen}                       \tab mg/l; Garcia-Gordon  \tab   \cr
#'   \code{oxsolMm/Kg}  \tab \code{oxygen}                       \tab umol/kg; Garcia-Gordon\tab  \cr
#'   \code{par~}        \tab \code{PAR}                          \tab -; Biospherical/Licor\tab   \cr
#'   \code{par/log}     \tab \code{PAR}                          \tab log; Satlantic       \tab   \cr
#'   \code{ph}          \tab \code{pH}                           \tab -                    \tab   \cr
#'   \code{potemp~68C}  \tab \code{thetaM}                       \tab degC; IPTS-68        \tab   \cr
#'   \code{potemp~90C}  \tab \code{thetaM}                       \tab degC; ITS-90         \tab   \cr
#'   \code{pr}          \tab \code{pressure}                     \tab dbar                 \tab 1 \cr
#'   \code{prM}         \tab \code{pressure}                     \tab dbar                 \tab   \cr
#'   \code{pr50M}       \tab \code{pressure}                     \tab dbar; SBE50          \tab   \cr
#'   \code{prSM}        \tab \code{pressure}                     \tab dbar                 \tab   \cr
#'   \code{prDM}        \tab \code{pressure}                     \tab dbar; digiquartz     \tab   \cr
#'   \code{prdE}        \tab \code{pressure}                     \tab psi; strain gauge    \tab 2 \cr
#'   \code{prDE}        \tab \code{pressure}                     \tab psi; digiquartz      \tab 2 \cr
#'   \code{prdM}        \tab \code{pressure}                     \tab dbar; strain gauge   \tab   \cr
#'   \code{prSM}        \tab \code{pressure}                     \tab dbar; strain gauge   \tab   \cr
#'   \code{ptempC}      \tab \code{pressureTemperature}          \tab degC; ITS-90         \tab 3 \cr
#'   \code{pumps}       \tab \code{pumpStatus}                   \tab                      \tab   \cr
#'   \code{rhodflTC~}   \tab \code{Rhodamine}                    \tab ppb; Turner Cyclops  \tab   \cr
#'   \code{sal~~}       \tab \code{salinity}                     \tab -, PSS-78            \tab 4 \cr
#'   \code{sbeox~ML/L}  \tab \code{oxygen}                       \tab ml/l; SBE43          \tab   \cr
#'   \code{sbox~ML/L}   \tab \code{oxygen}                       \tab ml/l; SBE43 (?)      \tab   \cr
#'   \code{sbeox~Mm/Kg} \tab \code{oxygen}                       \tab umol/kg; SBE43       \tab   \cr
#'   \code{sbox~Mm/Kg}  \tab \code{oxygen}                       \tab umol/kg; SBE43 (?)   \tab   \cr
#'   \code{sbeox~Mm/L}  \tab \code{oxygen}                       \tab umol/l; SBE43        \tab   \cr
#'   \code{sbox~Mm/L}   \tab \code{oxygen}                       \tab umol/l; SBE43 (?)    \tab   \cr
#'   \code{sbeox~PS}    \tab \code{oxygen}                       \tab percent; SBE43       \tab   \cr
#'   \code{sbox~PS}     \tab \code{oxygen}                       \tab percent; SBE43 (?)   \tab   \cr
#'   \code{sbeox~V}     \tab \code{oxygenRaw}                    \tab V; SBE43             \tab   \cr
#'   \code{sbox~V}      \tab \code{oxygenRaw}                    \tab V; SBE43 (?)         \tab   \cr
#'   \code{scan}        \tab \code{scan}                         \tab -                    \tab   \cr
#'   \code{seaTurbMtr~} \tab \code{turbidity}                    \tab FTU; Seapoint        \tab   \cr
#'   \code{secS-priS}   \tab \code{salinityDifference}           \tab -, PSS-78            \tab   \cr
#'   \code{sigma-t}     \tab \code{sigmaT}                       \tab kg/m^3               \tab   \cr
#'   \code{sigma-theta} \tab \code{sigmaTheta}                   \tab kg/m^3               \tab 5 \cr
#'   \code{sigma-é}     \tab \code{sigmaTheta}                   \tab kg/m^3               \tab 5 \cr
#'   \code{spar}        \tab \code{spar}                         \tab -                    \tab   \cr
#'   \code{specc}       \tab \code{conductivity}                 \tab uS/cm                \tab   \cr
#'   \code{sva}         \tab \code{specificVolumeAnomaly}        \tab 1e-8 m^3/kg;         \tab   \cr
#'   \code{svCM~}       \tab \code{soundSpeed}                   \tab m/s; Chen-Millero    \tab   \cr
#'   \code{T2~68C}      \tab \code{temperatureDifference}        \tab degC; IPTS-68        \tab   \cr
#'   \code{T2~90C}      \tab \code{temperatureDifference}        \tab degC; ITS-90         \tab   \cr
#'   \code{t~68}        \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t~90}        \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t~68}        \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t~68C}       \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t~90C}       \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t090Cm}      \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t4990C}      \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{tnc90C}      \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{tsa}         \tab \code{thermostericAnomaly}          \tab 1e-8 m^3/kg          \tab   \cr
#'   \code{tv290C}      \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t4968C}      \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{tnc68C}      \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{tv268C}      \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t190C}       \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{tnc290C}     \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{tnc268C}     \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t3890C~}     \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t38~90C}     \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t3868C~}     \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t38~38C}     \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{timeH}       \tab \code{timeH}                        \tab hour; elapsed        \tab   \cr
#'   \code{timeJ}       \tab \code{timeJ}                        \tab julian day           \tab   \cr
#'   \code{timeJV2}     \tab \code{timeJV2}                      \tab julian day           \tab   \cr
#'   \code{timeK}       \tab \code{timeK}                        \tab s; since Jan 1, 2000 \tab   \cr
#'   \code{timeM}       \tab \code{timeM}                        \tab minute; elapsed      \tab   \cr
#'   \code{timeN}       \tab \code{timeN}                        \tab s; NMEA since Jan 1, 1970\tab\cr
#'   \code{timeQ}       \tab \code{timeQ}                        \tab s; NMEA since Jan 1, 2000\tab\cr
#'   \code{timeS}       \tab \code{timeS}                        \tab s; elapsed           \tab   \cr
#'   \code{turbflTC~}   \tab \code{turbidity}                    \tab NTU; Turner Cyclops  \tab   \cr
#'   \code{turbflTCdiff}\tab \code{turbidityDifference}          \tab NTU; Turner Cyclops  \tab   \cr
#'   \code{turbWETbb~}  \tab \code{turbidity}                    \tab 1/(m*sr); WET Labs ECO\tab   \cr
#'   \code{turbWETbbdiff}\tab \code{turbidityDifference}         \tab 1/(m*sr); WET Labs ECO\tab   \cr
#'   \code{turbWETntu~} \tab \code{turbidity}                    \tab NTU; WET Labs ECO    \tab   \cr
#'   \code{turbWETntudiff}\tab \code{turbidityDifference}        \tab NTU; WET Labs ECO    \tab   \cr
#'   \code{upoly~}      \tab \code{upoly}                        \tab -                    \tab   \cr
#'   \code{user~}       \tab \code{user}                         \tab -                    \tab   \cr
#'   \code{v~~}         \tab \code{voltage}                      \tab V                    \tab   \cr
#'   \code{wetBAttn}    \tab \code{beamAttenuation}              \tab 1/m; WET Labs AC3    \tab   \cr
#'   \code{wetBTrans}   \tab \code{beamTransmission}             \tab percent; WET Labs AC3\tab   \cr
#'   \code{wetCDOM~}    \tab \code{fluorescence}                 \tab mg/m^3; WET Labs CDOM\tab   \cr
#'   \code{wetCDOMdiff} \tab \code{fluorescenceDifference}       \tab mg/m^3; WET Labs CDOM\tab   \cr
#'   \code{wetChAbs}    \tab \code{fluorescence}                 \tab 1/m; WET Labs AC3 absorption\tab   \cr
#'   \code{wetStar~}    \tab \code{fluorescence}                 \tab mg/m^3; WET Labs WETstar\tab   \cr
#'   \code{wetStardiff} \tab \code{fluorescenceDifference}       \tab mg/m^3; WET Labs WETstar\tab   \cr
#'   \code{xmiss}       \tab \code{beamTransmission}             \tab percent; Chelsea/Seatech\tab \cr
#'   \code{xmiss~}      \tab \code{beamTransmission}             \tab percent; Chelsea/Seatech\tab \cr
#' }
#' Notes:
#' \itemize{
#' \item{1: 'pr' is in a Dalhousie-generated data file but seems not to be in [1].}
#' \item{2: this is an odd unit, and so if \code{sw*} functions are called on an object
#' containing this, a conversion will be made before performing the computation. Be
#' on the lookout for errors, since this is a rare situation.}
#' \item{3: assume ITS-90 temperature scale, since sample \code{.cnv} file headers do not specify it.}
#' \item{4: some files have PSU for this. Should we handle that? And are there other S scales to consider?}
#' \item{5: 'theta' may appear in different ways with different encoding configurations, set up
#' within R or in the operating system.}
#' }
#'
#' @param h The header line.
#' @param columns Optional list containing name correspondances, as described for
#' \code{\link{read.ctd.sbe}}.
#' @template debugTemplate
#' @return a list containing \code{name} (the oce name), \code{nameOriginal} (the SBE name) and \code{unit}.
#' @author Dan Kelley
#' @references
#' 1. A SBE data processing manual was once at
#' \code{http://www.seabird.com/document/sbe-data-processing-manual},
#' but as of summer 2018, this no longer seems to be provided by SeaBird.
#' A web search will turn up copies of the manual that have been put
#' online by various research groups and data-archiving agencies.
#' As of 2018-07-05, the latest version was named
#' \code{SBEDataProcessing_7.26.4.pdf} and had release date 12/08/2017,
#' and this was the reference version used in coding \code{oce}.
#'
#' @family things related to \code{ctd} data
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
    name <- gsub("^# name [0-9][0-9]* = (.*):.*$", "\\1", h, ignore.case=TRUE, useBytes=TRUE)
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
    if (1 == length(grep("^alt[M]?$", name, useBytes=TRUE))) {
        name <- "altimeter"
        unit <- list(unit=expression(m), scale="")
    } else if (1 == length(grep("^acc[M]?$", name, useBytes=TRUE))) {
        name <- "acceleration"
        unit <- list(unit=expression(m/s^2), scale="")
    } else if (1 == length(grep("^bat[0-9]?$", name, useBytes=TRUE))) {
        name <- "beamAttenuation"
        unit <- list(unit=expression(1/m), scale="Chelsea/Seatech")
    } else if (1 == length(grep("^C2-C1S/m$", name, useBytes=TRUE))) {
        name <- "conductivityDifference"
        unit <- list(unit=expression(S/m), scale="")
    } else if (1 == length(grep("^C2-C1mS/cm$", name, useBytes=TRUE))) {
        name <- "conductivityDifference"
        unit <- list(unit=expression(mS/cm), scale="")
    } else if (1 == length(grep("^C2-C1uS/cm$", name, useBytes=TRUE))) {
        name <- "conductivityDifference"
        unit <- list(unit=expression(mu*S/cm), scale="")
    } else if (1 == length(grep("^c(ond)?((_)|([0-2]))mS/cm$", name, useBytes=TRUE))) {
        name <- "conductivity"
        unit <- list(unit=expression(mS/cm), scale="")
    } else if (1 == length(grep("^c(ond)?((_)|([0-2]))S/m$", name, useBytes=TRUE))) {
        name <- "conductivity"
        unit <- list(unit=expression(S/m), scale="")
    } else if (1 == length(grep("^c(ond)?((_)|([0-2]))uS/cm$", name, useBytes=TRUE))) {
        name <- "conductivity"
        unit <- list(unit=expression(mu*S/cm), scale="")
    } else if (1 == length(grep("^CStarTr[0-9]$", name, useBytes=TRUE))) {
        name <- "beamTransmission"
        unit <- list(unit=expression(percent), scale="WET Labs C-Star")
    } else if (1 == length(grep("^CStarAt[0-9]$", name, useBytes=TRUE))) {
        name <- "beamAttenuation"
        unit <- list(unit=expression(1/m), scale="")
    } else if (1 == length(grep("^density[0-9]{2}$", name, useBytes=TRUE))) {
        name <- "density"
        unit <- list(unit=expression(kg/m^3), scale="")
    } else if (1 == length(grep("^dep[FS][M]?$", name, useBytes=TRUE))) {
        name <- "depth"
        unit <- list(unit=expression(m), scale="")
    } else if (1 == length(grep("^dz/dt[M]?$", name, useBytes=TRUE))) {
        name <- "descentRate"
        unit <- list(unit=expression(m/s), scale="")
    } else if (1 == length(grep("^f[0-9][0-9]?$", name, useBytes=TRUE))) {
        name <- "frequency"
        unit <- list(unit=expression(Hz), scale="")
    } else if (1 == length(grep("^flag$", name, useBytes=TRUE))) {
        name <- "flag"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^flC[1]?$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mu*g/l), scale="Chelsea")
    } else if (1 == length(grep("^flCM[1]?$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mu*g/l), scale="Chelsea Mini Chl Con")
    } else if (1 == length(grep("^flCUVA[12]?$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mu*g/l), scale="Chelsea UV Aquatracka")
    } else if (1 == length(grep("^flECO-AFL[0-9]?$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs")
    } else if (1 == length(grep("^flflTC[0-1]{1}$", name, useBytes=TRUE))) {
        name <- "fluorescein"
        unit <- list(unit=expression(ppb), scale="Turner Cyclops")
    } else if (1 == length(grep("^flflTCdiff$", name, useBytes=TRUE))) {
        name <- "fluoresceinDifference"
        unit <- list(unit=expression(ppb), scale="Turner Cyclops")
    } else if (1 == length(grep("^flScufa[0-9]?$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Turner SCUFA")
    } else if (1 == length(grep("^flSP[0-9]?$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seapoint")
    } else if (1 == length(grep("^flSPR$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seapoint, Rhodamine")
    } else if (1 == length(grep("^flSPuv[0-9]?$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seapoint, UV")
    } else if (1 == length(grep("^flS$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seatech")
    } else if (1 == length(grep("^flT$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Turner")
    } else if (1 == length(grep("^gpa$", name, useBytes=TRUE))) {
        name <- "geopotentialAnomaly"
        unit <- list(unit=expression(J/kg), scale="")
    } else if (1 == length(grep("^latitude$", name, useBytes=TRUE))) {
        name <- "latitude"
        unit <- list(unit=expression(degree*N), scale="")
    } else if (1 == length(grep("^longitude$", name, useBytes=TRUE))) {
        name <- "longitude"
        unit <- list(unit=expression(degree*E), scale="")
    } else if (1 == length(grep("^n2satML/L$", name, useBytes=TRUE))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(ml/l), scale="")
    } else if (1 == length(grep("^n2satMg/L$", name, useBytes=TRUE))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(mg/l), scale="")
    } else if (1 == length(grep("^n2satumol/L$", name, useBytes=TRUE))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(mu*mol/l), scale="")
    } else if (1 == length(grep("^n2satumol/kg$", name, useBytes=TRUE))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(mu*mol/kg), scale="")
    } else if (1 == length(grep("^nbin$", name, useBytes=TRUE))) {
        name <- "nbin"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^nbf$", name, useBytes=TRUE))) {
        name <- "bottlesFired"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^obsscufa[1-9]{0,1}$", name, useBytes=TRUE))) {
        name <- "backscatter"
        unit <- list(unit=expression(NTU), scale="Turner SCUFA")
    } else if (1 == length(grep("^opoxMg/L$", name, useBytes=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Optode, Anderaa")
    } else if (1 == length(grep("^opoxML/L$", name, useBytes=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Optode, Anderaa")
    } else if (1 == length(grep("^opoxMm/L$", name, useBytes=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/l), scale="Optode, Anderaa")
    } else if (1 == length(grep("^opoxPS$", name, useBytes=TRUE))) {
        name <- "oxygenSaturation"
        unit <- list(unit=expression(percent), scale="Optode, Anderaa")
    } else if (1 == length(grep("^oxsatML/L$", name, useBytes=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Weiss")
    } else if (1 == length(grep("^oxsatMg/L$", name, useBytes=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Weiss")
    } else if (1 == length(grep("^oxsatMm/Kg$", name, useBytes=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/kg), scale="Weiss")
    } else if (1 == length(grep("^oxsolML/L$", name, useBytes=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Garcia-Gordon")
    } else if (1 == length(grep("^oxsolMg/L$", name, useBytes=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Garcia-Gordon")
    } else if (1 == length(grep("^oxsolMm/Kg$", name, useBytes=TRUE))) {
        name <- "oxygen"
        unit <- list(unit=expression(umol/kg), scale="Garcia-Gordon")
    } else if (1 == length(grep("^cpar$", name, useBytes=TRUE))) {
        name <- "CPAR/Corrected Irradience"
        unit <- list(unit=expression(percent), scale="")
    } else if (1 == length(grep("^par[0-9]?$", name, useBytes=TRUE))) {
        name <- "par"
        unit <- list(unit=expression(), scale="Biospherical/Licor")
    } else if (1 == length(grep("^par/log$", name, useBytes=TRUE))) {
        name <- "par"
        unit <- list(unit=expression(log), scale="Satlantic")
    } else if (1 == length(grep("^ph$", name, useBytes=TRUE))) {
        name <- "pH"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^pr$", name, useBytes=TRUE))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="")
    } else if (1 == length(grep("^prdE$", name, useBytes=TRUE))) {
        ## Caution: English unit
        name <- "pressurePSI"
        unit <- list(unit=expression(psi), scale="")
    } else if (1 == length(grep("^prDE$", name, useBytes=TRUE))) {
        ## Caution: English unit
        name <- "pressurePSI"
        unit <- list(unit=expression(psi), scale="")
    } else if (1 == length(grep("^prM$", name, useBytes=TRUE))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="")
    } else if (1 == length(grep("^pr50M[0-9]?$", name, useBytes=TRUE))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="SBE50")
    } else if (1 == length(grep("^prDM$", name, useBytes=TRUE))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="Digiquartz")
    } else if (1 == length(grep("^pr[dS]M$", name, useBytes=TRUE))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="Strain Gauge")
    } else if (1 == length(grep("^ptempC$", name, useBytes=TRUE))) {
        name <- "pressureTemperature"
        unit <- list(unit=expression(degree*C), scale="ITS-90") # FIXME: guess on scale
    } else if (1 == length(grep("^potemp[0-9]*68C$", name, useBytes=TRUE))) {
        name <- "theta"
        unit <- list(unit=expression(degree*C), scale="ITS-68")
    } else if (1 == length(grep("^potemp[0-9]*90C$", name, useBytes=TRUE))) {
        name <- "theta"
        unit <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (1 == length(grep("^pumps$", name, useBytes=TRUE))) {
        name <- "pumpStatus"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^rhodflTC[0-1]{1}$", name, useBytes=TRUE))) {
        name <- "Rhodamine"
        unit <- list(unit=expression(ppb), scale="Turner Cyclops")
    } else if (1 == length(grep("^rhodflTCdiff$", name, useBytes=TRUE))) {
        name <- "RhodamineDifference"
        unit <- list(unit=expression(ppb), scale="Turner Cyclops")
    } else if (1 == length(grep("^sal[0-9]{2}$", name, useBytes=TRUE))) {
        name <- "salinity"
        unit <- list(unit=expression(), scale="PSS-78") # FIXME: guess on scale
    } else if (1 == length(grep("^sbe?ox[0-9]ML/L$", name, useBytes=TRUE))) {
        if (length(grep("^sbo", name, useBytes=TRUE)))
            warning("assuming '", name, "' is equivalent to '", gsub("^sb", "sbe", name), "'", sep="")
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]Mg/L$", name, useBytes=TRUE))) {
        if (length(grep("^sbo", name, useBytes=TRUE)))
            warning("assuming '", name, "' is equivalent to '", gsub("^sb", "sbe", name), "'", sep="")
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]Mm/Kg$", name, useBytes=TRUE))) {
        if (length(grep("^sbo", name, useBytes=TRUE)))
            warning("assuming '", name, "' is equivalent to '", gsub("^sb", "sbe", name), "'", sep="")
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/kg), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]Mm/L$", name, useBytes=TRUE))) {
        if (length(grep("^sbo", name, useBytes=TRUE)))
            warning("assuming '", name, "' is equivalent to '", gsub("^sb", "sbe", name), "'", sep="")
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/l), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]PS$", name, useBytes=TRUE))) {
        if (length(grep("^sbo", name, useBytes=TRUE)))
            warning("assuming '", name, "' is equivalent to '", gsub("^sb", "sbe", name), "'", sep="")
        name <- "oxygen"
        unit <- list(unit=expression(percent), scale="SBE43")
    } else if (1 == length(grep("^sbe?ox[0-9]V$", name, useBytes=TRUE))) {
        if (length(grep("^sbo", name, useBytes=TRUE)))
            warning("assuming '", name, "' is equivalent to '", gsub("^sb", "sbe", name), "'", sep="")
        name <- "oxygenRaw"
        unit <- list(unit=expression(V), scale="SBE43")
    } else if (1 == length(grep("^scan$", name, useBytes=TRUE))) {
        name <- "scan"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^secS-priS$", name, useBytes=TRUE))) {
        name <- "salinityDifference"
        unit <- list(unit=expression(), scale="PSS-78")
    } else if (1 == length(grep("^seaTurbMtr[1]?$", name, useBytes=TRUE))) {
        name <- "turbidity"
        unit <- list(unit=expression(FTU), scale="Seapoint")
    } else if (1 == length(grep("sigma-t[0-9]{2}", name, useBytes=TRUE))) {
        name <- "sigmaT"
        unit <- list(unit=expression(kg/m^3), scale="")
    ##} else if (1 == length(grep("sigma-.*[0-9]*", name, ignore.case=TRUE))) {
    } else if (1 == length(grep("^sigma", name, useBytes=TRUE))) {
        ## there are several cases, and we match the sigma-theta case
        ## by exclusion, because of limited understanding of how
        ## to match non-ascii characters on Windows machines.
        if (1 == length(grep("^sigma-t[0-9]{2}$", name, useBytes=TRUE))) {
            name <- "sigmaT"
        } else if (1 == length(grep("^sigma-1[0-9]{2}$", name, useBytes=TRUE))) {
            name <- "sigma1"
        } else if (1 == length(grep("^sigma-2[0-9]{2}$", name, useBytes=TRUE))) {
            name <- "sigma2"
        } else if (1 == length(grep("^sigma-3[0-9]{2}$", name, useBytes=TRUE))) {
            name <- "sigma3"
        } else if (1 == length(grep("^sigma-4[0-9]{2}$", name, useBytes=TRUE))) {
            name <- "sigma4"
        } else if (1 == length(grep("^sigma-\xe9[0-9]{2}$", name, useBytes=TRUE))) {
            name <- "sigmaTheta"
            ## 2016-12-22 DK
            ## The above regexp matches for what we see in the supplied file
            ##    system.file("extdata", "d201211_0011.cnv", package="oce")
            ## at line 54, an acute-accented "e" (which maybe looked like
            ## a theta to someone at SBE, when the format was invented. Clark
            ## found the SBE docs and did some tests, which made it clear that
            ## this accented "e" is always used, i.e. it is not just in some
            ## sample files. Therefore, the above should not need changes.
            ## However, it is worth explaining more, since the above regexp
            ## replaces one that we had before today. That older one failed
            ## on mswindows. On the assumption that this was an encoding issue,
            ## the readLines() that reads in the data (line 686 of the present file)
            ## was provided with an encoding of UTF-8, which is almost certainly
            ## what osx and linux are using by default, but it's a *bad idea*
            ## to rely on defaults, so we now set the encoding when we read
            ## the data. Note that this switch also required to use the
            ## useBytes=TRUE setting in all the grep() calls in the present
            ## block.
        } else {
            name <- "sigma" ## give up; this is a default
        }
        ## In all these cases, the unit is the same
        unit <- list(unit=expression(kg/m^3), scale="")
    } else if (1 == length(grep("^spar$", name, useBytes=TRUE))) {
        name <- "spar"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^specc$", name, useBytes=TRUE))) {
        name <- "conductivity"
        unit <- list(unit=expression(uS/cm), scale="")
    } else if (1 == length(grep("^sva$", name, useBytes=TRUE))) {
        name <- "specificVolumeAnomaly"
        unit <- list(unit=expression(10^-8*m^3/kg), scale="")
    } else if (1 == length(grep("^svCM[0-9]?$", name, useBytes=TRUE))) {
        name <- "soundSpeed"
        unit <- list(unit=expression(m/s), scale="Chen-Millero")
    } else if (1 == length(grep("^T2-T[01]68C$", name, useBytes=TRUE))) {
        name <- "temperatureDifference"
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (1 == length(grep("^T2-T[01]90C$", name, useBytes=TRUE))) {
        name <- "temperatureDifference"
        unit <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (1 == length(grep("^t[0-9]68((C)|(Cm))?$", name, useBytes=TRUE))) {
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (1 == length(grep("^t[0-9]90((C)|(Cm))?$", name, useBytes=TRUE))) {
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
    } else if (1 == length(grep("^timeH$", name, useBytes=TRUE))) {
        name <- "timeH"
        unit <- list(unit=expression(hour), scale="")
    } else if (1 == length(grep("^timeJ$", name, useBytes=TRUE))) {
        name <- "timeJ"
        unit <- list(unit=expression(day), scale="")
    } else if (1 == length(grep("^timeJV2$", name, useBytes=TRUE))) {
        name <- "timeJV2"
        unit <- list(unit=expression(day), scale="")
    } else if (1 == length(grep("^timeK$", name, useBytes=TRUE))) {
        name <- "timeK"
        unit <- list(unit=expression(s), scale="since Jan 1, 2000")
    } else if (1 == length(grep("^timeM$", name, useBytes=TRUE))) {
        name <- "timeM"
        unit <- list(unit=expression(minute), scale="")
    } else if (1 == length(grep("^timeN$", name, useBytes=TRUE))) {
        name <- "timeN"
        unit <- list(unit=expression(s), scale="NMEA since Jan 1, 1970")
    } else if (1 == length(grep("^timeQ$", name, useBytes=TRUE))) {
        name <- "timeQ"
        unit <- list(unit=expression(s), scale="NMEA since Jan 1, 2000")
    } else if (1 == length(grep("^timeS$", name, useBytes=TRUE))) {
        name <- "timeS"
        unit <- list(unit=expression(s), scale="")
    } else if (1 == length(grep("^tsa$", name, useBytes=TRUE))) {
        name <- "thermostericAnomaly"
        unit <- list(unit=expression(10^-8*m^3/kg), scale="")
    } else if (1 == length(grep("^turbflTC[0-1]$", name, useBytes=TRUE))) {
        name <- "turbidity"
        unit <- list(unit=expression(NTU), scale="Turner Cyclops")
    } else if (1 == length(grep("^turbflTCdiff$", name, useBytes=TRUE))) {
        name <- "turbidityDifference"
        unit <- list(unit=expression(NTU), scale="Turner Cyclops")
    } else if (1 == length(grep("^turbWETbb[0-4]$", name, useBytes=TRUE))) {
        name <- "turbidity"
        unit <- list(unit=expression(1/m*sr), scale="WET Labs ECO")
    } else if (1 == length(grep("^turbWETbbdiff$", name, useBytes=TRUE))) {
        name <- "turbidityDifference"
        unit <- list(unit=expression(1/m*sr), scale="WET Labs ECO")
    } else if (1 == length(grep("^turbWETntu[0-5]$", name, useBytes=TRUE))) {
        name <- "turbidity"
        unit <- list(unit=expression(NTU), scale="WET Labs ECO")
    } else if (1 == length(grep("^turbWETntudiff$", name, useBytes=TRUE))) {
        name <- "turbidityDifference"
        unit <- list(unit=expression(NTU), scale="WET Labs ECO")
    } else if (1 == length(grep("^upoly[0-2]$", name, useBytes=TRUE))) {
        name <- "upoly"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^user[1-5]$", name, useBytes=TRUE))) {
        name <- "user"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^v[0-9][0-9]?$", name, useBytes=TRUE))) {
        unit <- list(unit=expression(V), scale="")
    } else if (1 == length(grep("^wetBAttn$", name, useBytes=TRUE))) {
        name <- "beamAttenuation"
        unit <- list(unit=expression(1/m), scale="WET Labs AC3")
    } else if (1 == length(grep("^wetBTrans$", name, useBytes=TRUE))) {
        name <- "beamTransmission"
        unit <- list(unit=expression(percent), scale="WET Labs AC3")
    } else if (1 == length(grep("^wetCDOM[0-5]{0,1}$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs CDOM")
    } else if (1 == length(grep("^wetCDOMdiff$", name, useBytes=TRUE))) {
        name <- "fluorescenceDifference"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs CDOM")
    } else if (1 == length(grep("^wetChAbs$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(1/m), scale="WET Labs AC3 absorption")
    } else if (1 == length(grep("^wetStar[0-9]?$", name, useBytes=TRUE))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs WETstar")
    } else if (1 == length(grep("^wetStardiff$", name, useBytes=TRUE))) {
        name <- "fluorescenceDifference"
        unit <- list(unit=expression(mg/m^3), scale="WET Labs WETstar")
    } else if (1 == length(grep("^xmiss[0-9]?$", name, useBytes=TRUE))) {
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
#' @author Dan Kelley and Clark Richards
#'
#' @details
#' This function reads files stored in Seabird \code{.cnv} format.
#' Note that these files can contain multiple sensors for a given field. For example,
#' the file might contain a column named \code{t090C} for one
#' temperature sensor and \code{t190C} for a second. The first will be denoted
#' \code{temperature} in the \code{data} slot of the return value, and the second
#' will be denoted \code{temperature1}. This means that the first sensor
#' will be used in any future processing that accesses \code{temperature}. This
#' is for convenience of processing, and it does not pose a limitation, because the
#' data from the second sensor are also available as e.g. \code{x[["temperature1"]]},
#' where \code{x} is the name of the returned value.  For the details of the
#' mapping from \code{.cnv} names to \code{ctd} names, see \code{\link{cnvName2oceName}}.
#'
#' The original data names as stored in \code{file} are stored within the \code{metadata}
#' slot as \code{dataNamesOriginal}, and are displayed with \code{summary} alongside the
#' numerical summary. See the Appendix VI of [2] for the meanings of these
#' names (in the "Short Name" column of the table spanning pages 161 through 172).
#'
#' @section A note on sampling times:
#' Until November of 2018,
#' there was a possibility for great confusion in the storage
#' of the time entries within the \code{data} slot, because \code{read.ctd.sbe}
#' renamed each of the ten variants of time (see [2] for a list)
#' as \code{"time"} in the \code{data} slot of the returned value.
#' For CTD profiles, this was perhaps not a great problem, but it could
#' lead to great confusion for moored data. Therefore, a change to \code{read.ctd.sbe} was
#' made, so that it would Seabird times, using the \code{start_time} entry in
#' the CNV file header (which is stored as \code{startTime} in the object
#' \code{metadata} slot), along with specific time columns as follows
#' (and as documented, with uneven clarity, in the
#' SBE Seasoft data processing manual, revision 7.26.8, Appendix VI):
#' \code{timeS} (seconds elapsed since \code{start_time}),
#' \code{timeM} (minutes elapsed since \code{start_time}),
#' \code{timeH} (hours elapsed since \code{start_time}),
#' \code{timeJ} (Julian days since the start of the year of the first observation),
#' \code{timeN} (NMEA-based time, in seconds past Jan 1, 1970),
#' \code{timeQ} (NMEA-based time, in seconds past Jan 1, 2000),
#' \code{timeK} (NMEA-based time, in seconds past Jan 1, 2000),
#' \code{timeJV2} (as \code{timeJ}),
#' \code{timeSCP} (as \code{timeJ}),
#' and
#' \code{timeY} (computer time, in seconds past Jan 1, 1970).
#' NOTE: not all of these times have been tested properly, and so users
#' are asked to report incorrect times, so that \code{read.ctd.sbe} can
#' be improved.
#'
#' @section A note on scales:
#' The user might encounter data files with a variety of scales for temperature and
#' salinity. Oce keeps track of these scales in the units it sets up for the stored
#' variables. For example, if \code{A} is a CTD object, then
#' \code{A[["temperatureUnit"]]$scale} is a character string that will indicate the scale.
#' Modern-day data will have \code{"ITS-90"} for that scale, and old data may have
#' \code{"IPTS-68"}. The point of saving the scale in this way is so that the various
#' formulas that deal with water properties can account for the scale, e.g. converting
#' from numerical values saved on the \code{"IPTS-68"} scale to the newer scale, using
#' \code{\link{T90fromT68}} before doing calculations that are expressed in
#' terms of the \code{"ITS-90"} scale. This is taken care of by retrieving temperatures
#' with the accessor function, e.g. writing \code{A[["temperature"]]} will either
#' retrieve the stored values (if the scale is ITS-90) or converted values (if
#' the scale is IPTS-68). Even though this procedure should work, users who
#' really care about the details of their data are well-advised to do a couple
#' of tests after examining the first data line of their data file in an editor.
#' Note that reading a file that contains IPTS-68 temperatures produces a warning.
#'
#' @examples
#' f <- system.file("extdata", "ctd.cnv", package="oce")
#' ## Read the file in the normal way
#' d <- read.ctd(f)
#' ## Read an imaginary file, in which salinity is named 'salt'
#' d <- read.ctd(f, columns=list(
#'   salinity=list(name="salt", unit=list(unit=expression(), scale="PSS-78"))))
#'
#' @references
#' 1. The Sea-Bird SBE 19plus profiler is described at
#' \code{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.  Some more
#' information is given in the Sea-Bird data-processing manual
#' (next item).
#'
#' 2. A SBE data processing manual was once at
#' \code{http://www.seabird.com/document/sbe-data-processing-manual},
#' but as of summer 2018, this no longer seems to be provided by SeaBird.
#' A web search will turn up copies of the manual that have been put
#' online by various research groups and data-archiving agencies.
#' As of 2018-07-05, the latest version was named
#' \code{SBEDataProcessing_7.26.4.pdf} and had release date 12/08/2017,
#' and this was the reference version used in coding \code{oce}.
read.ctd.sbe <- function(file, columns=NULL, station=NULL, missingValue,
                         monitor=FALSE, debug=getOption("oceDebug"), processingLog, ...)
{
    if (length(grep("\\*", file, ignore.case=TRUE))) {
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
    oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") { # will read an individual file\n", unindent=1)

    ## Read Seabird data file.  Note on headers: '*' is machine-generated,
    ## '**' is a user header, and '#' is a post-processing header.
    filename <- ""
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
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
    date <- NA # is this used? (or useful?)
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
    lines <- readLines(file, encoding="UTF-8")
    options(warn=warn)

    ## Get names and units of columns in the SBE data file
    nameLines  <- grep("^# name [0-9][0-9]* = .*:.*$", lines, ignore.case=TRUE)
    colUnits <- vector("list", length(nameLines))
    colNamesInferred <- NULL
    dataNamesOriginal <- list()
    namesUsed <- NULL
    for (iline in seq_along(nameLines)) {
        nu <- cnvName2oceName(lines[nameLines[iline]], columns, debug=debug-1)
        ##newname <- unduplicateName(nu$name, colNamesInferred)
        ##colNamesInferred <- c(colNamesInferred, newname)
        ## dataNamesOriginal[[newname]] <- nu$nameOriginal
        if (nu$name %in% namesUsed) {
            trial <- 2
            while (paste(nu$name, trial, sep="") %in% namesUsed) {
                trial <- trial + 1
                ##message("trial=", trial)
                if (trial > 10)
                    break
            }
            ## message("** REUSING NAME '", nu$name)
            nu$name <- paste(nu$name, trial, sep="")
            ##message("  -> '", nu$name, "'")
        }
        namesUsed <- c(namesUsed, nu$name)
        dataNamesOriginal[[nu$name]] <- nu$nameOriginal
        colUnits[[iline]] <- nu$unit
        colNamesInferred <- c(colNamesInferred, nu$name)
        ##message("SBE name=", nu$name, "; nameOriginal=", nu$nameOriginal, "; unit='", as.character(nu$unit$unit),"'")
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
        ##message("** scan at iline ", iline)
        line <- lines[iline]
        ##message(line)
        #line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        oceDebug(debug, paste("Examining header line '", line, "'\n", sep=""))
        header <- c(header, line)
        ##if (length(grep("\*END\*", line))) #BUG# why is this regexp no good (new with R-2.1.0)
        aline <- iconv(line, from="UTF-8", to="ASCII", sub="?")
        if (length(grep("^\\s*\\*END\\*\\s*$", aline, perl=TRUE, useBytes=TRUE))) {
            ##message("got *END* at line ", iline)
            ## Sometimes SBE files have a header line after the *END* line.
            iline <- iline + 1
            if (length(grep("[a-cf-zA-CF-Z]", lines[iline])))
                iline <- iline + 1
            break
        }
        ##if (iline>129) browser()
        lline <- tolower(aline)
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
        if (length(grep("^\\* .*time.*=.*$", lline))) {
            if (0 == length(grep("real-time sample interval", lline))) {
                oceDebug(debug, "found 'real-time sample interval' header line\n")
                d <- sub(".*=", "", lline)
                d <- sub("^ *", "", d)
                d <- sub(" *$", "", d)
                date <- decodeTime(d)
            }
        }
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
        if (0 < (r<-regexpr("depth", lline))) {
            ## "** Depth (m): 3447 "
            look <- sub("[a-z:()]*", "", lline, ignore.case=TRUE)
            look <- gsub("^[*a-zA-Z\\(\\) :]*", "", lline, ignore.case=TRUE)
            look <- gsub("[ ]*", "", look, ignore.case=TRUE)
            oceDebug(debug, " trying to get water depth from '", lline, "', reduced to '", look, "'\n", sep="")
            if (!length(grep('[a-zA-Z]', look))) {
                waterDepth<- as.numeric(look)
                oceDebug(debug, "got waterDepth: ", waterDepth, "\n")
            }
        }
        if (0 < (r<-regexpr("water depth:", lline))
            || 0 < (r<-regexpr(pattern="profondeur", text=lline))) {
            ## Examples from files in use by author:
            ##** Profondeur: 76
            ##** Water Depth:   40 m
            look <- sub("[ ]*$", "", sub(".*:[ ]*", "", lline))
            linesplit <- strsplit(look, " ")[[1]]
            nitems <- length(linesplit)
            if (nitems == 1) {
                waterDepth <- as.numeric(linesplit[1])
            } else if (nitems == 2) {
                unit <- linesplit[2]
                if (unit == "m") {
                    waterDepth <- as.numeric(linesplit[1])
                } else if (unit == "km") {
                    waterDepth <- 1000 * as.numeric(linesplit[1])
                } else {
                    warning("ignoring unit on water depth '", look, "'")
                }
            } else {
                warning("cannot interpret water depth from '", lline, "'")
            }
        }
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
            warning("'** Recovery' not found in header")
    }
    ## Require p,S,T data at least
    if (!("temperature" %in% colNamesInferred))
        warning("cannot find temperature in this file; try using columns argument if this is an error")

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
    res@metadata$deploymentType <- "unknown"
    res@metadata$date <- date
    res@metadata$startTime <- startTime
    res@metadata$recoveryTime <- recoveryTime
##    res@metadata$time <- date          # standardized name
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$waterDepth <- waterDepth # if NA, will update later
    res@metadata$sampleInterval <- sampleInterval
    res@metadata$sampleIntervalUnits <- sampleIntervalUnits
    ##res@metadata$names <- colNamesInferred
    ##res@metadata$labels <- colNamesInferred
    res@metadata$filename <- filename
    if (fileType == "binary") {
        warning("can only handle non-binary .cnv files")
        res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
        return(res)
    }
    ## Read the data as a table.
    pushBack(lines, file)
    oceDebug(debug, "About to read these names: c(\"", paste(colNamesInferred, collapse='","'), "\")\n", sep="")
    ##message("skipping ", iline-1, " lines at top of file")
    data <- as.list(read.table(file, skip=iline-1, header=FALSE))
    if (length(data) != length(colNamesInferred))
        stop("Number of columns in .cnv data file must match number of variables named in the header")
    names(data) <- colNamesInferred
    ndata <- length(data[[1]])
    if (0 < ndata) {
        haveData <- TRUE
        names <- names(data)
        ##labels <- names
        ## if (!found.scan) {
        ##     data$scan <- 1:ndata
        ##     names <- names(data)
        ##     colNamesInferred <- c(colNamesInferred, "scan")
        ##     colNamesOriginal <- c(colNamesOriginal, "scan")
        ## }
    } else {
        haveData <- FALSE
        warning("no data in CTD file \"", filename, "\"")
        data <- list(scan=NULL, salinity=NULL, temperature=NULL, pressure=NULL)
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ##hitem <- processingLogItem(processingLog)
    ## replace any missingValue with NA
    if (!missing(missingValue) && !is.null(missingValue)) {
        for (item in names(data)) {
            data[[item]] <- ifelse(data[[item]]==missingValue, NA, data[[item]])
        }
    }
    res@data <- data
    ## Add standard things, if missing
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
                    ## Conductivity Ratio is conductivity divided by 42.914 mS/cm (Culkin and Smith 1980
                    ## see ?read.rsk for full citation)
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
            res <- oceSetData(res, name="pressure", value=res@data$pressurePSI/0.6894757293168,
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
    ##20181014(issue 1460) ## Store time in metadata, if it's not in the data. This was done
    ##20181014(issue 1460) ## 2018-07-05 in response to issue 1434, on the assumption that
    ##20181014(issue 1460) ## some other code might be relying on `d[["time"]]` retrieving
    ##20181014(issue 1460) ## *something**.
    ##20181014(issue 1460) if (!("time" %in% names(res@data)))
    ##20181014(issue 1460)     res@metadata$time <- date
    ##res@metadata$dataNamesOriginal <- colNamesOriginal
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))

    if (("temperature" %in% names(res@metadata$units)) && res@metadata$units$temperature$scale == "IPTS-68") {
        warning("this CNV file has temperature in the IPTS-68 scale, and this is stored in the object; note that [[\"temperature\"]] and the sw* functions will convert to the modern ITS-90 value")
    }

    ## Note: previously, at this spot, there was code to switch from the IPTS-68 scale
    ## to the ITS-90 scale. The old-scale data were saved in a column named
    ## "temperature68". However, that scheme could be confusing both in oce code and
    ## in user code, and it became unnecessary when the scale started being
    ## stored in the unit. See the "note on scales" in the documentation for
    ## the scheme used to prevent problems.


    oceDebug(debug, "} # read.ctd.sbe()\n")
    res
}
