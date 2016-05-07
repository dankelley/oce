#' Infer variable name, units and scale from a Seabird (.cnv) header line
#'
#' This function is used by \code{\link{read.ctd.sbe}} to infer data names
#' and units from the coding used by Teledyne/Seabird (SBE) \code{.cnv}
#' files.  Lacking access to documentation on the SBE format,
#' the present function is based on inspection of a suite of CNV files
#' available to the \code{oce} developers.
#'
#' A few sample header lines to be decoded are as follows.
#'\preformatted{
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
#' supplied by prominent archiving agencies. If an SBE name is not recogized,
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
#' so that the developers can add the data type in question. (There is no plan to add
#' all data types without user interest, because there are simply so many types;
#' oxygen alone has over 40 variants!)
#'
#' \tabular{llll}{
#'   \strong{Key}       \tab \strong{Result}                     \tab \strong{Unit;scale} \tab \strong{Notes} \cr
#'   \code{altM}        \tab \code{altimeter}                    \tab m                   \tab    \cr
#'   \code{c~mS/cm}     \tab \code{conductivity}                 \tab mS/cm               \tab    \cr
#'   \code{c~S/m}       \tab \code{conductivity}                 \tab s/m                 \tab    \cr
#'   \code{CStarAt~}    \tab \code{beamAttenuation}              \tab 1/m                 \tab    \cr
#'   \code{CStarTr~}    \tab \code{beamTransmission}             \tab percent             \tab    \cr
#'   \code{density~~}   \tab \code{density}                      \tab kg/m^3              \tab    \cr
#'   \code{depS}        \tab \code{depth}                        \tab m                   \tab    \cr
#'   \code{depSM}       \tab \code{depth}                        \tab m                   \tab    \cr
#'   \code{dz/dtM}      \tab \code{descentRate}                  \tab m/s                 \tab    \cr
#'   \code{f~}          \tab \code{frequency}                    \tab Hz                  \tab    \cr
#'   \code{f~~}         \tab \code{frequency}                    \tab Hz                  \tab    \cr
#'   \code{flC}         \tab \code{fluorescence}                 \tab ug/l; Chelsea Aqua 3\tab    \cr
#'   \code{flEC-AFLM}   \tab \code{fluorescence}                 \tab mg/m^3; WET labs ECO-AFL/FLtab\cr
#'   \code{flS}         \tab \code{fluorescence}                 \tab -; Seatech          \tab    \cr
#'   \code{flSP}        \tab \code{fluorescence}                 \tab -; Seapoint         \tab    \cr
#'   \code{flSPR}       \tab \code{fluorescence}                 \tab -; Seapoint, Rhodamine\tab  \cr
#'   \code{flSPuv}      \tab \code{fluorescence}                 \tab -; Seapoint, UV      \tab   \cr
#'   \code{flT}         \tab \code{fluorescence}                 \tab -; Turner            \tab   \cr
#'   \code{latitude}    \tab \code{latitude}                     \tab degN                 \tab   \cr
#'   \code{longitude}   \tab \code{longitude}                    \tab degE                 \tab   \cr
#'   \code{n2satML/L}   \tab \code{nitrogenSaturation}           \tab ml/l                 \tab   \cr
#'   \code{n2satMg/L}   \tab \code{nitrogenSaturation}           \tab mg/l                 \tab   \cr
#'   \code{n2satumol/kg}\tab \code{nitrogenSaturation}           \tab umol/kg              \tab   \cr
#'   \code{nbin}        \tab \code{nbin}                         \tab                      \tab   \cr
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
#'   \code{prdM}        \tab \code{pressure}                     \tab dbar; strain gauge   \tab   \cr
#'   \code{prSM}        \tab \code{pressure}                     \tab dbar; strain gauge   \tab   \cr
#'   \code{ptempC}      \tab \code{pressureTemperature}          \tab degC; ITS-90         \tab 3 \cr
#'   \code{pumps}       \tab \code{pumpStatus}                   \tab                      \tab   \cr
#'   \code{sal~~}       \tab \code{salinity}                     \tab -, PSS-78            \tab 4 \cr
#'   \code{seaTurbMtr~} \tab \code{turbidity}                    \tab FTU; SeaPoint        \tab   \cr
#'   \code{sbeox~ML/L}  \tab \code{oxygen}                       \tab ml/l                 \tab   \cr
#'   \code{sbeox~Mm/Kg} \tab \code{oxygen}                       \tab ml/l                 \tab   \cr
#'   \code{sbeox~Ps}    \tab \code{oxygen}                       \tab percent              \tab   \cr
#'   \code{sbeox~V}     \tab \code{oxygenRaw}                    \tab V                    \tab   \cr
#'   \code{scan}        \tab \code{scan}                         \tab -                    \tab   \cr
#'   \code{sigma-t}     \tab \code{sigmaT}                       \tab kg/m^3               \tab   \cr
#'   \code{sigma-theta} \tab \code{sigmaTheta}                   \tab kg/m^3               \tab 5 \cr
#'   \code{spar}        \tab \code{spar}                         \tab -                    \tab   \cr
#'   \code{svCM}        \tab \code{soundSpeed}                   \tab m/s; Chen-Millero    \tab   \cr
#'   \code{t~68}        \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr 
#'   \code{t~90}        \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr 
#'   \code{t~68C}       \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr 
#'   \code{t~90C}       \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t068C}       \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t168C}       \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t090Cm}      \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t4990C}      \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{tnc90C}      \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{tv290C}      \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t4968C}      \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{tnc68C}      \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{tv268C}      \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t190C}       \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{tnc290C}     \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{tnc268C}     \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t3890C}      \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t38~90C}     \tab \code{temperature}                  \tab degC; ITS-90         \tab   \cr
#'   \code{t3868C}      \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{t38~38C}     \tab \code{temperature}                  \tab degC; IPTS-68        \tab   \cr
#'   \code{timeS}       \tab \code{time}                         \tab second               \tab   \cr
#'   \code{timeM}       \tab \code{time}                         \tab minute               \tab   \cr
#'   \code{timeH}       \tab \code{time}                         \tab hour                 \tab   \cr
#'   \code{timeJ}       \tab \code{time}                         \tab day                  \tab   \cr
#'   \code{upoly~}      \tab \code{upoly}                        \tab -                    \tab   \cr
#'   \code{v~}          \tab \code{voltage}                      \tab V                    \tab   \cr
#'   \code{wetCDOM}     \tab \code{fluorescence}                 \tab mg/m^3               \tab   \cr
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
#' 1. A SBE data processing manual is at \url{http://www.seabird.com/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.seabird.com/sites/default/files/documents/SBEDataProcessing_7.25.0.pdf&nid=1320}.
#'
#' @family things related to \code{ctd} data
#' @family functions that interpret variable names and units from headers
cnvName2oceName <- function(h, columns=NULL, debug=getOption("oceDebug"))
{
    if (length(h) != 1)
        stop("oceNameFromSBE() expects just 1 line of header")
    ## An example, for which the grep is designed, is below.
    ## '# name 4 = t190C: Temperature, 2 [ITS-90, deg C]'
    if (1 != length(grep("^# name [0-9][0-9]* = .*:.*$", h, ignore.case=TRUE)))
        stop("header line does not contain a variable name")
    ## message("h: '", h, "'")
    name <- gsub("^# name [0-9][0-9]* = (.*):.*$", "\\1", h, ignore.case=TRUE)
    nameOriginal <- name

    ## If 'name' is mentioned in columns, then use columns and ignore the lookup table.
    if (!is.null(columns)) {
        ##message("name:", name)
        ## d<-read.ctd("~/src/oce/create_data/ctd/ctd.cnv",columns=list(salinity=list(name="sal00",unit=list(expression(), scale="PSS-78monkey"))))
        cnames <- names(columns)
        for (i in seq_along(cnames)) {
            if (name == columns[[i]]$name) {
                ##message("HIT; name=", cnames[i])
                ##message("HIT; unit$scale=", columns[[i]]$unit$scale)
                return(list(name=cnames[i], nameOriginal=name, unit=columns[[i]]$unit))
            }
        }
    }

    ## Since 'name' is not mentioned in 'columns', try looking it up. Some of these
    ## tests are a bit subtle, and could be wrong.
    if (1 == length(grep("^altM$", name))) {
        name <- "altimeter"
        unit <- list(unit=expression(m), scale="")
    } else if (1 == length(grep("^c[0-9]mS/cm$", name))) {
        name <- "conductivity"
        unit <- list(unit=expression(mS/cm), scale="")
    } else if (1 == length(grep("^c[0-9]S/m$", name))) {
        name <- "conductivity"
        unit <- list(unit=expression(S/m), scale="")
    } else if (1 == length(grep("^CStarTr[0-9]$", name))) {
        name <- "beamTransmission"
        unit <- list(unit=expression(percent), scale="WET Labs C-Star")
    } else if (1 == length(grep("^CStarAt[0-9]$", name))) {
        name <- "beamAttenuation"
        unit <- list(unit=expression(1/m), scale="")
    } else if (1 == length(grep("^density[0-9]{2}$", name))) {
        name <- "density"
        unit <- list(unit=expression(kg/m^3), scale="")
    } else if (1 == length(grep("^depS[M]?$", name))) {
        name <- "depth"
        unit <- list(unit=expression(m), scale="")
    } else if (1 == length(grep("^dz/dt[M]?$", name))) {
        name <- "descentRate"
        unit <- list(unit=expression(m/s), scale="")
    } else if (1 == length(grep("^f[0-9]{1,2}$", name))) {
        name <- "frequency"
        unit <- list(unit=expression(Hz), scale="")
    } else if (1 == length(grep("flag", name))) {
        name <- "flag"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^flC[1]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mu*g/l), scale="Chelsea")
    } else if (1 == length(grep("^flECO-AFL[0-9]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="WET labs")
    } else if (1 == length(grep("^flSP[0-9]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seapoint")
    } else if (1 == length(grep("^flsPR$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seapoint, Rhodamine")
    } else if (1 == length(grep("^flsPuv[0-9]?$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seapoint, UV")
    } else if (1 == length(grep("^flS$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Seatech")
    } else if (1 == length(grep("^flT$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(), scale="Turner")
    } else if (1 == length(grep("latitude", name))) {
        name <- "latitude"
        unit <- list(unit=expression(degree*N), scale="")
    } else if (1 == length(grep("longitude", name))) {
        name <- "longitude"
        unit <- list(unit=expression(degree*E), scale="")
    } else if (1 == length(grep("n2satML/L", name))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(ml/l), scale="")
    } else if (1 == length(grep("n2satMg/L", name))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(mg/l), scale="")
    } else if (1 == length(grep("n2satumol/L", name))) {
        name <- "nitrogenSaturation"
        unit <- list(unit=expression(mu*mol/l), scale="")
    } else if (name == "nbin") {
        name <- "nbin"
        unit <- list(unit=expression(), scale="")
    } else if (name == "nbf") {
        name <- "bottlesFired"
        unit <- list(unit=expression(), scale="")
    } else if (name == "oxsatML/L") {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Weiss")
    } else if (name == "oxsatMg/L") {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Weiss")
    } else if (name == "oxsatMm/Kg") {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/kg), scale="Weiss")
    } else if (name == "oxsolML/L") {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="Garcia-Gordon")
    } else if (name == "oxsolMg/L") {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="Garcia-Gordon")
    } else if (name == "oxsolMm/Kg") {
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
        name <- "pressure"; unit <- list(unit=expression(dbar), scale="")
    } else if (1 == length(grep("^prdE$", name))) { # Caution: English unit
        name <- "pressure"; unit <- list(unit=expression(psi), scale="")
        warning("this .cnv file contains a pressure in PSI; be careful in using this")
    } else if (1 == length(grep("^prM$", name))) {
        name <- "pressure"; unit <- list(unit=expression(dbar), scale="")
    } else if (1 == length(grep("^pr50M[0-9]?$", name))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="SBE50")
    } else if (1 == length(grep("^prDM$", name))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="Digiquartz")
    } else if (1 == length(grep("^pr[dS]M$", name))) {
        name <- "pressure"
        unit <- list(unit=expression(dbar), scale="Strain Gauge")

    } else if (1 == length(grep("ptempC", name))) {
        name <- "pressureTemperature" # temperature at the pressure sensor
        unit <- list(unit=expression(degree*C), scale="ITS-90") # FIXME: guess on scale
    } else if (1 == length(grep("^potemp[0-9]*68C$", name))) {
        name <- "theta68"
        unit <- list(unit=expression(degree*C), scale="ITS-68") # FIXME: guess on scale
    } else if (1 == length(grep("^potemp[0-9]*90C$", name))) {
        name <- "theta"
        unit <- list(unit=expression(degree*C), scale="ITS-90") # FIXME: guess on scale
    } else if (1 == length(grep("^pumps$", name))) {
        name <- "pumpStatus"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("^sal[0-9]{2}$", name))) {
        name <- "salinity"
        unit <- list(unit=expression(), scale="PSS-78") # FIXME: guess on scale
    } else if (1 == length(grep("^sbeox[0-9]ML/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(ml/l), scale="SBE43")
    } else if (1 == length(grep("^sbeox[0-9]Mg/L$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mg/l), scale="SBE43")
    } else if (1 == length(grep("^sbeox[0-9]Mm/Kg$", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(mu*mol/kg), scale="SBE43")
    } else if (1 == length(grep("sbeox[0-9]PS", name))) {
        name <- "oxygen"
        unit <- list(unit=expression(percent), scale="SBE43")
    } else if (1 == length(grep("sbeox[0-9]V", name))) {
        name <- "oxygenRaw"
        unit <- list(unit=expression(V), scale="SBE43")
    } else if (1 == length(grep("scan", name))) {
        name <- "scan"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("seaTurbMtr", name))) {
        name <- "turbidity"
        unit <- list(unit=expression(FTU), scale="SeaPoint")
    } else if (1 == length(grep("sigma-t[0-9]{2}", name))) {
        name <- "sigmaT"
        unit <- list(unit=expression(kg/m^3), scale="")
    ##} else if (1 == length(grep("sigma-.*[0-9]*", name, ignore.case=TRUE))) {
    } else if (1 == length(grep("^sigma-\xfc\xbe\x8e\x96\x94\xbc[0-9]{2}$", name, useBytes=TRUE))) {
        name <- "sigmaTheta"
        unit <- list(unit=expression(kg/m^3), scale="")
    } else if (1 == length(grep("spar", name))) {
        name <- "spar"
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("svCM", name))) {
        name <- "soundSpeed"
        unit <- list(unit=expression(m/s), scale="Chen-Millero")
    } else if (1 == length(grep("^t[0-9]68((C)|(Cm))?$", name))) {
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (1 == length(grep("^t[0-9]90((C)|(Cm))?$", name))) {
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="ITS-90")
    } else if (name %in% c("t4968C", "tnc68C", "tv268C",
                           "tnc268C", "t3868C", "t38_68C")) { # [1] p169-170
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="IPTS-68")
    } else if (name %in% c("t4990C", "tnc90C", "tv290C",
                           "tnc290C", "t3890C", "t38_90C")) { # [1] p169-170
        name <- "temperature"
        unit <- list(unit=expression(degree*C), scale="ITS-90")

    } else if (1 == length(grep("timeS", name))) {
        name <- "time"; unit <- list(unit=expression(s), scale="")
    } else if (1 == length(grep("timeM", name))) {
        name <- "time"; unit <- list(unit=expression(minute), scale="")
    } else if (1 == length(grep("timeH", name))) {
        name <- "time"; unit <- list(unit=expression(hour), scale="")
    } else if (1 == length(grep("timeJ", name))) {
        name <- "time"; unit <- list(unit=expression(day), scale="")

    } else if (1 == length(grep("upoly[0-9]+", name))) {
        name <- paste("upoly", gsub("upoly", "", name), sep="")
        unit <- list(unit=expression(), scale="")
    } else if (1 == length(grep("v[0-9]+", name))) {
        name <- paste("v", gsub("v", "", name), sep="")
        unit <- list(unit=expression(V), scale="")
    } else if (1 == length(grep("^wetBTrans$", name))) {
        name <- "beamTransmission"
        unit <- list(unit=expression(percent), scale="WET labs AC3")
    } else if (1 == length(grep("^wetCDOM$", name))) {
        name <- "fluorescence"
        unit <- list(unit=expression(mg/m^3), scale="")
    } else if (1 == length(grep("^xmiss[0-9]?$", name))) {
        name <- "beamTransmission"
        unit <- list(unit=expression(percent), scale="Chelsea/Seatech")
    } else {
        warning("unrecognized SBE name '", name, "'; consider using 'columns' to define this name")
        unit <- list(unit=expression(), scale="")
    }
    if (debug > 0)
        message("cnvName2oceName(): '", nameOriginal, "' -> '", name, "' (", unit$scale, ")")
    list(name=name, nameOriginal=nameOriginal, unit=unit)
}


#' Read an Seabird CTD File
#' @template readCtdTemplate
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
#' @examples
#' f <- system.file("extdata", "ctd.cnv", package="oce")
#' ## Read the file in the normal way
#' d <- read.ctd(f)
#' ## Read an imaginary file, in which salinity is named 'salt'
#' d <- read.ctd(f, columns=list(
#'   salinity=list(name="salt", unit=list(expression(), scale="PSS-78"))))
#'
#' @references
#' 1. The Sea-Bird SBE 19plus profiler is described at
#' \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.  Some more
#' information is given in the Sea-Bird data-processing manaual
#' \url{http://www.seabird.com/old-manuals/Software_Manuals/SBE_Data_Processing/SBEDataProcessing_7.20g.pdf}.
#'
#' 2. A SBE data processing manual is at \url{http://www.seabird.com/sites/all/modules/pubdlcnt/pubdlcnt.php?file=http://www.seabird.com/sites/default/files/documents/SBEDataProcessing_7.25.0.pdf&nid=1320}.
read.ctd.sbe <- function(file, columns=NULL, station=NULL, missing.value,
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
    oceDebug(debug, "read.ctd.sbe(file=\"", file, "\") {\n", unindent=1)

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
    res <- new("ctd", pressureType="sea")
    ## Header
    scientist <- ship <- institute <- address <- cruise <- hexfilename <- ""
    sampleInterval <- NA
    systemUploadTime <- NULL
    latitude <- longitude <- NA
    startTime <- NULL
    waterDepth <- NA
    date <- recovery <- NA
    header <- c()
    ##conductivity.standard <- 4.2914
    found.header.latitude <- found.header.longitude <- FALSE
    serialNumber <- serialNumberConductivity <- serialNumberTemperature <- ""
    ## units$conductivity <- list(unit=expression(), scale="") # guess; other types are "mS/cm" and "S/m"
    ## units$temperature <- list(unit=expression(degree*C), scale="ITS-90") # guess; other option is IPTS-68
    pressureType = "sea"               # guess; other option is "absolute"

    lines <- readLines(file, encoding="UTF-8")

    ## Get names and units of columns in the SBE data file
    nameLines  <- grep("^# name [0-9][0-9]* = .*:.*$", lines, ignore.case=TRUE)
    colUnits <- list()
    colNamesInferred <- NULL
    dataNamesOriginal <- NULL
    for (iline in seq_along(nameLines)) {
        nu <- cnvName2oceName(lines[nameLines[iline]], columns, debug=debug-1)
        colNamesInferred <- c(colNamesInferred, nu$name)
        dataNamesOriginal <- c(dataNamesOriginal, nu$nameOriginal)
        colUnits[[iline]] <- nu$unit
        ## message("nu$name: ", nu$name, "; scale: ", colUnits[[nu$name]]$unit$scale)
    }
    colNamesInferred <- unduplicateNames(colNamesInferred)
    res@metadata$dataNamesOriginal <- dataNamesOriginal
    ##found.scan <- "scan" %in% colNamesInferred
    ##found.temperature <- "temperature" %in% colNamesInferred
    found.pressure <- "pressure" %in% colNamesInferred
    found.salinity <- "salinity" %in% colNamesInferred
    ##found.time <- "time" %in% colNamesInferred
    found.depth <- "depth" %in% colNamesInferred
    found.conductivity <- "conductivity" %in% colNamesInferred
    found.conductivity.ratio <- "conductivity.ratio" %in% colNamesInferred
    ## FIXME: should we insist on having salinity, temperature, and pressure?

    for (iline in seq_along(lines)) {
        line <- lines[iline]
        #line <- scan(file, what='char', sep="\n", n=1, quiet=TRUE)
        oceDebug(debug>1, paste("Examining header line '",line,"'\n", sep=""))
        header <- c(header, line)
        ##if (length(grep("\*END\*", line))) #BUG# why is this regexp no good (new with R-2.1.0)
        aline <- iconv(line, from="UTF-8", to="ASCII", sub="?")
        if (length(grep("END", aline, perl=TRUE, useBytes=TRUE))) {
            ## Sometimes SBE files have a header line after the *END* line.
            iline <- iline + 1
            if (length(grep("[a-cf-zA-CF-Z]", lines[iline])))
                iline <- iline + 1
            break
        }
        lline <- tolower(aline)
        if (0 < regexpr(".*seacat profiler.*", lline))
            serialNumber <- gsub("[ ].*$","",gsub(".*sn[ ]*","",lline))
        if (length(grep("^\\* Temperature SN", lline, ignore.case=TRUE)))
            serialNumberTemperature <- gsub("^.*=\\s", "", lline)
        if (length(grep("^\\* Conductivity SN", lline, ignore.case=TRUE)))
            serialNumberConductivity <- gsub("^.*=\\s", "", lline)
        if (0 < (r<-regexpr("date:", lline))) {
            d <- sub("(.*)date:([ ])*", "", lline)
            date <- decodeTime(d, "%Y%m%d") # e.g. 20130701 Canada Day
        }
        ##* NMEA UTC (Time) = Jul 28 2011  04:17:53
        ##* system upload time = jan 26 2010 13:02:57
        if (length(grep("^\\* .*time.*=.*$", lline))) {
            if (0 == length(grep("real-time sample interval", lline))) {
                d <- sub(".*=", "", lline)
                d <- sub("^ *", "", d)
                d <- sub(" *$", "", d)
                date <- decodeTime(d)
            }
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
        if (!found.header.latitude && (0 < (r<-regexpr("latitude*[0-8]*", lline, ignore.case=TRUE)))) {
            latitude <- parseLatLon(lline, debug=debug-1)
            found.header.latitude <- TRUE
        }
        if (!found.header.longitude && (0 < (r<-regexpr("longitude*[0-8]*", lline, ignore.case=TRUE)))) {
            longitude <- parseLatLon(lline, debug=debug-1)
            found.header.longitude <- TRUE
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
                station <- sub("[ ]*$", "", sub("(.*)station:([ ])*", "", ignore.case=TRUE, line)) # full string
        }
        if (0 < (r<-regexpr("recovery:", lline)))
            recovery <- sub("(.*)recovery:([ ])*", "", lline)
        if (0 < (r<-regexpr("depth", lline))) { # "** Depth (m): 3447 "
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
            linesplit <- strsplit(look," ")[[1]]
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
        if (0 < (r<-regexpr("^. sample rate =", lline))) {
            ## * sample rate = 1 scan every 5.0 seconds
            rtmp <- lline;
            rtmp <- sub("(.*) sample rate = ", "", rtmp)
            rtmp <- sub("scan every ", "", rtmp)
            rtmp <- strsplit(rtmp, " ")
            ##      if (length(rtmp[[1]]) != 3)
            ##        warning("cannot parse sample-rate string in `",line,"'")
            sampleInterval <- as.double(rtmp[[1]][2]) / as.double(rtmp[[1]][1])
            if (rtmp[[1]][3] == "seconds") {
                ;
            } else {
                if (rtmp[[1]][3] == "minutes") {
                    sampleInterval <- sampleInterval / 60;
                } else {
                    if (rtmp[[1]][3] == "hours") {
                        sampleInterval <- sampleInterval / 3600;
                    } else {
                        warning("cannot understand `",rtmp[[1]][2],"' as a unit of time for sampleInterval")
                    }
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
        if (is.nan(longitude))
            warning("'** Longitude:' not found in header")
        if (is.null(date))
            warning("'** Date:' not found in header")
        if (is.null(recovery))
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
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$recovery <- recovery
    res@metadata$waterDepth <- waterDepth # if NA, will update later
    res@metadata$sampleInterval <- sampleInterval
    res@metadata$names <- colNamesInferred
    res@metadata$labels <- colNamesInferred
    res@metadata$filename <- filename
    ## Read the data as a table.
    pushBack(lines, file)
    ##if (is.null(columns)) {
    oceDebug(debug, "About to read these names: c(\"", paste(colNamesInferred, collapse='","'),"\")\n", sep="")
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
        warning("no data in CTD file \"", filename, "\"\n")
        data <- list(scan=NULL, salinity=NULL, temperature=NULL, pressure=NULL)
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ##hitem <- processingLogItem(processingLog)
    res@data <- data
    ## Add standard things, if missing
    if (haveData) {
        if (!found.salinity) {
            if (found.conductivity.ratio) {
                C <- data$conductivityratio
                S <- swSCTp(C, data$temperature, data$pressure)
                warning("created a salinity data item from the temperature, conductivity-ratio and pressure items")
            } else if (found.conductivity) {
                C <- data$conductivity
                if (!is.null(res@metadata$units$conductivity)) {
                    unit <- as.character(res@metadata$units$conductivity$unit)
                    ## Conductivity Ratio is conductivity divided by 42.914 mS/cm (Culkin and Smith 1980;
                    ## see ?read.rsk for full citation)
                    if ("mS/cm" == unit) {
                        C <- C / 42.914 # e.g. RSK 
                    } else if ("S/m" == unit) {
                        C <- C / 4.2914
                    } else {
                        warning("unrecognized conductivity unit '", unit, "'; assuming mS/cm for salinity calculation -- results should be used with caution")
                    }
                } else {
                    warning("missing conductivity unit; guessing a unit based on maximum value")
                    cmax <- max(C, na.rm=TRUE)
                    if (cmax > 10) {
                        warning("max(conductivity) > 10, so using using conductivity/42.914 as a conductivity ratio for computation of salinity")
                        C <- C / 42.914
                    } else if (cmax > 1) {
                        warning("max(conductivity) between 1 and 10, so using using conductivity/4.2914 as a conductivity ratio for computation of salinity")
                        C <- C / 4.2914
                    }
                }
                S <- swSCTp(C, data$temperature, data$pressure)
                warning("created a salinity data item from the temperature, conductivity and pressure items")
            } else {
                warning("cannot find salinity or conductivity in .cnv file; try using columns argument if the file actually contains these items")
            }
            ## FIXME: move this to the very end, where we add 'scan' if that's not found.
            res <- ctdAddColumn(res, S, name="salinity", label="Salinity",
                                unit=c(unit=expression(), scale="PSS-78"), debug=debug-1)
            ## colNamesOriginal <- c(colNamesOriginal, "NA")
        }
        if (found.depth && !found.pressure) { # BUG: this is a poor, nonrobust approximation of pressure
            g <- if (found.header.latitude) gravity(latitude) else 9.8
            rho0 <- 1000 + swSigmaTheta(median(res@data$salinity), median(res@data$temperature), 0)
            res <- ctdAddColumn(res, res@data$depth * g * rho0 / 1e4, name="pressure", label="Pressure",
                                unit=list(unit=expression("dbar"), scale=""), debug=debug-1)
            ## colNamesOriginal <- c(colNamesOriginal, "NA")
            warning("created a pressure data item from the depth item")
        }
    }
    ##res@metadata$dataNamesOriginal <- colNamesOriginal
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))

    ## update to temperature IPTS-90, if have an older version
    ## if (!("temperature" %in% names(res@data)) && ("temperature68" %in% names(res@data))) {
    ##     res <- ctdAddColumn(res, T90fromT68(res@data$temperature68),
    ##                         name="temperature", label="Temperature",
    ##                         unit=c(unit=expression(degree*C), scale="ITS-90"), debug=debug-1)
    ##     warning("converted temperature from IPTS-68 to ITS-90")
    ##     res@processingLog <- processingLogAppend(res@processingLog, "converted temperature from IPTS-68 to ITS-90")
    ## }
    ## if (!("scan" %in% names(res@data))) {
    ##     res <- ctdAddColumn(res, 1:length(res@data[[1]]), name="scan", label="scan",
    ##                         unit=c(unit=expression(), scale=""), debug=debug-1)
    ## }
    ## ## FIXME(20160429): do we need/want next 3 lines?
    ## if (!("salinity" %in% names(res@metadata$units))) res@metadata$units$salinity <- list(unit=expression(), scale="PSS-78")
    ## if (!("pressure" %in% names(res@metadata$units))) res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    ## if (!("depth" %in% names(res@metadata$units))) res@metadata$units$depth <- list(unit=expression(m), scale="")
    oceDebug(debug, "} # read.ctd.sbe()\n")
    res
}

