# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Find the Type of an Oceanographic Data File
#'
#' `oceMagic` tries to infer the file type, based on the data
#' within the file, the file name, or a combination of the two.
#'
#' `oceMagic` was previously called `oce.magic`, but that
#' alias was removed in version 0.9.24; see [oce-defunct].
#'
#' @param file a connection or a character string giving the name of the file
#' to be checked.
#'
#' @template encodingTemplate
#'
#' @param debug an integer, set non-zero to turn on debugging.  Higher values
#' indicate more debugging.
#'
#' @return A character string indicating the file type, or `"unknown"`, if
#' the type cannot be determined. If the result contains `"/"` characters,
#' these separate a list describing the file type, with the first element being
#' the general type, the second element being the manufacturer, and the third
#' element being the manufacturer's name for the instrument. For example,
#' `"adp/nortek/aquadopp"` indicates a acoustic-doppler profiler made by
#' NorTek, of the model type called Aquadopp.
#'
#' @author Dan Kelley
#'
#' @seealso This is used mainly by [read.oce()].
oceMagic <- function(file, encoding = "latin1", debug = getOption("oceDebug")) {
    filename <- file
    oceDebug(debug, paste("oceMagic(file=\"", filename, "\") START\n", sep = ""), sep = "", unindent = 1)
    isdir <- file.info(file)$isdir
    if (is.finite(isdir) && isdir) {
        tst <- file.info(paste(file, "/", file, "_MTL.txt", sep = ""))$isdir
        if (!is.na(tst) && !tst) {
            oceDebug(debug, "END oceMagic() returning landsat\n", unindent = 1)
            return("landsat")
        } else {
            stop("please supply a file name, not a directory name")
        }
    }
    if (is.character(file)) {
        oceDebug(debug, "'file' is a character value\n")
        if (grepl(".asc$", filename)) {
            someLines <- readLines(file, n = 1L)
            if (42 == length(strsplit(someLines[1], " ")[[1]])) {
                oceDebug(debug, "END oceMagic() returning lisst\n", unindent = 1)
                return("lisst")
            }
        }
        if (grepl(".adr$", filename)) {
            oceDebug(debug, "file names ends in .adr, so this is an adv/sontek/adr file.\n")
            oceDebug(debug, "END oceMagic() returning adv/sontek/adr\n", unindent = 1)
            return("adv/sontek/adr")
        }
        if (grepl(".rsk$", filename)) {
            oceDebug(debug, "file names ends with \".rsk\", so this is an RBR/rsk file.\n")
            oceDebug(debug, "END oceMagic() returning RBR/rsk\n", unindent = 1)
            return("RBR/rsk")
        }
        if (grepl(".s4a.", filename)) {
            oceDebug(debug, "file names contains \".s4a.\", so this is an interocean S4 file.\n")
            oceDebug(debug, "END oceMagic() returning interocean/s4\n", unindent = 1)
            return("interocean/s4")
        }
        if (grepl(".ODF$", filename, ignore.case = TRUE)) {
            # in BIO files, the data type seems to be on line 14.  Read more, for safety.
            lines <- readLines(file, encoding = "latin1")
            dt <- try(
                {
                    grep("DATA_TYPE[ \t]*=", lines, perl = TRUE)
                },
                silent = TRUE
            )
            if (inherits(dt, "try-error") || length(t) < 1) {
                stop("cannot infer type of ODF file")
            }
            subtype <- gsub("[',]", "", tolower(strsplit(lines[dt[1]], "=")[[1]][2]))
            subtype <- gsub("^\\s*", "", subtype)
            subtype <- gsub("\\s*$", "", subtype)
            res <- paste(subtype, "odf", sep = "/")
            oceDebug(debug, paste0("END oceMagic() returning ", res, "\n"), sep = "", unindent = 1)
            return(res)
        }
        if (grepl(".WCT$", filename, ignore.case = TRUE)) {
            # old-style WOCE
            oceDebug(debug, "END oceMagic() returning ctd/woce/other\n", unindent = 1)
            return("ctd/woce/other") # e.g. http://cchdo.ucsd.edu/data/onetime/atlantic/a01/a01e/a01ect.zip
        }
        if (grepl(".nc$", filename, ignore.case = TRUE)) {
            # For a NetCDF file, do a check to see if it holds Argo data.
            if (requireNamespace("ncdf4", quietly = TRUE)) {
                if (substr(filename, 1, 5) == "http:") {
                    stop(
                        "can't open netcdf files over the web; try as follows\n    download.file(\"",
                        filename, "\", \"", gsub(".*/", "", filename), "\")"
                    )
                }
                f <- ncdf4::nc_open(filename)
                if ("DATA_TYPE" %in% names(f$var) &&
                    grepl("argo", ncdf4::ncvar_get(f, "DATA_TYPE"), ignore.case = TRUE)) {
                    oceDebug(debug, "END oceMagic() returning argo (upper-case style)\n", unindent = 1)
                    ncdf4::nc_close(f)
                    return("argo")
                } else if ("data_type" %in% names(f$var) &&
                    grepl("argo", ncdf4::ncvar_get(f, "data_type"), ignore.case = TRUE)) {
                    oceDebug(debug, "END oceMagic() returning argo (lower-case style)\n", unindent = 1)
                    ncdf4::nc_close(f)
                    return("argo")
                }
            }
            oceDebug(debug, "END oceMagic() returning netcdf\n", unindent = 1)
            return("netcdf")
        }
        if (grepl(".xml$", filename, ignore.case = TRUE)) {
            firstLine <- readLines(filename, 1L)
            if (grepl(".weather.gc.ca", firstLine)) {
                oceDebug(debug, "END oceMagic() returning met/xml2\n", unindent = 1)
                return("met/xml2")
            }
        }
        if (grepl(".osm.xml$", filename, ignore.case = TRUE)) {
            oceDebug(debug, "END oceMagic() returning openstreetmap (xml style)\n", unindent = 1)
            return("openstreetmap")
        }
        if (grepl(".osm$", filename, ignore.case = TRUE)) {
            oceDebug(debug, "END oceMagic() returning openstreetmap (non xml style)\n", unindent = 1)
            return("openstreetmap")
        }
        if (grepl(".gpx$", filename, ignore.case = TRUE)) {
            oceDebug(debug, "END oceMagic() returning gpx (e.g. Garmin GPS data)\n", unindent = 1)
            return("gpx")
        }
        if (grepl(".csv$", filename, ignore.case = TRUE)) {
            con <- file(filename, "r", encoding = encoding)
            someLines <- readLines(con, 30L) # , encoding="UTF-8-BOM")
            close(con)
            # print(someLines[1])
            if (grepl("^SSDA Sea & Sun Technology", someLines[1])) {
                return("ctd/ssda")
            } else if (1L == length(grep("^.*\"WMO Identifier\",", someLines))) {
                oceDebug(debug, "END oceMagic() returning met/csv1\n", unindent = 1)
                return("met/csv1") # FIXME: may be other things too ...
            } else if (grepl("^.*Longitude.*Latitude.*Station Name.*Climate ID.*Dew Point", someLines[1])) {
                oceDebug(debug, "END oceMagic() returning met/csv2 or met/csv3\n", unindent = 1)
                if (grepl("Time \\(LST\\)", someLines[1])) {
                    oceDebug(debug, "END oceMagic() returning met/csv2\n", unindent = 1)
                    return("met/csv3")
                } else {
                    oceDebug(debug, "END oceMagic() returning met/csv3\n", unindent = 1)
                    return("met/csv2")
                }
            } else if (length(grep("^Station_Name,", someLines))) {
                oceDebug(debug, "END oceMagic() returning sealevel\n", unindent = 1)
                return("sealevel")
            } else if (1L == length(grep("^CTD,", someLines))) {
                oceDebug(debug, "END oceMagic() returning ctd/woce/exchange\n", unindent = 1)
                return("ctd/woce/exchange")
            } else if (1L == length(grep("^BOTTLE,", someLines))) {
                oceDebug(debug, "END oceMagic() returning section\n", unindent = 1)
                return("section")
            } else {
                return("unknown")
            }
        }
        if (grepl(".edf$", filename, ignore.case = TRUE)) {
            oceDebug(debug, "END oceMagic() returning xbt/edf\n", unindent = 1)
            return("xbt/edf")
        }
        file <- file(file, "r", encoding = encoding)
    }
    if (!inherits(file, "connection")) {
        stop("argument 'file' must be a character string or connection")
    }
    oceDebug(debug, "'file' is a connection\n")
    if (!isOpen(file)) {
        open(file, "r", encoding = encoding)
    }
    # Grab text at start of file.
    lines <- readLines(file, n = 2L, skipNul = TRUE)
    line <- lines[1]
    line2 <- lines[2]
    oceDebug(debug, "first line of file: ", line, "\n", sep = "")
    oceDebug(debug, "second line of file: ", line2, "\n", sep = "")
    close(file)
    file <- file(filename, "rb")
    bytes <- readBin(file, what = "raw", n = 4)
    oceDebug(debug, paste("first two bytes in file: 0x", bytes[1], " and 0x", bytes[2], "\n", sep = ""))
    on.exit(close(file))
    if (bytes[1] == 0x00 && bytes[2] == 0x00 && bytes[3] == 0x27 && bytes[4] == 0x0a) {
        oceDebug(debug, "this is a shapefile; see e.g. http://en.wikipedia.org/wiki/Shapefile\n")
        oceDebug(debug, "END oceMagic() returning shapefile\n", unindent = 1)
        return("shapefile")
    }
    if (bytes[3] == 0xff && bytes[4] == 0xff) {
        oceDebug(debug, "END oceMagic() returning echosounder\n", unindent = 1)
        return("echosounder")
    }
    if (bytes[1] == 0x10 && bytes[2] == 0x02) {
        # 'ADPManual v710.pdf' p83
        if (96 == readBin(bytes[3:4], "integer", n = 1, size = 2, endian = "little")) {
            oceDebug(debug, "this is adp/sontek (4 byte match)\n")
        } else {
            oceDebug(debug, "this is adp/sontek (2 byte match, but bytes 3 and 4 should become integer 96)\n")
        }
        oceDebug(debug, "END oceMagic() returning adp/sontek\n", unindent = 1)
        return("adp/sontek")
    }
    if (bytes[1] == 0x7f && bytes[2] == 0x7f) {
        oceDebug(debug, "END oceMagic() returning adp/rdi\n", unindent = 1)
        return("adp/rdi")
    }
    if (bytes[1] == 0xa5 && bytes[2] == 0x05) {
        # NorTek files require deeper inspection.  Here, SIG stands for "System Integrator Guide",
        # Dated Jue 2008 (Nortek Doc No PS100-0101-0608)
        seek(file, 0)
        oceDebug(debug, "This is probably a nortek file of some sort.  Reading further to see for sure ...\n")
        hardware.configuration <- readBin(file, what = "raw", n = 48) # FIXME: this hard-wiring is repeated elsewhere
        if (hardware.configuration[1] != 0xa5 || hardware.configuration[2] != 0x05) {
            return("unknown")
        }
        oceDebug(debug, "hardware.configuration[1:2]", hardware.configuration[1:2], "(expect 0xa5 0x05)\n")
        head.configuration <- readBin(file, what = "raw", n = 224)
        oceDebug(debug, "head.configuration[1:2]", head.configuration[1:2], "(expect 0xa5 0x04)\n")
        if (head.configuration[1] != 0xa5 || head.configuration[2] != 0x04) {
            return("unknown")
        }
        user.configuration <- readBin(file, what = "raw", n = 512)
        oceDebug(debug, "user.configuration[1:2]", user.configuration[1:2], "(expect 0xa5 0x00)\n")
        if (user.configuration[1] != 0xa5 || user.configuration[2] != 0x00) {
            return("unknown")
        }
        nextTwoBytes <- readBin(file, what = "raw", n = 2)
        oceDebug(
            debug, "nextTwoBytes:", paste("0x", nextTwoBytes[1], sep = ""),
            paste("0x", nextTwoBytes[2], sep = ""), "(e.g. 0x5 0x12 is adv/nortek/vector)\n"
        )
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x12) {
            oceDebug(debug, "END oceMagic() returning adv/nortek/vector\n", unindent = 1)
            return("adv/nortek/vector")
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x01) {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudopp (see system-integrator-manual_jan2011.pdf Table 5.2)\n")
            oceDebug(debug, "END oceMagic() returning adp/nortek/aquadopp\n", unindent = 1)
            return("adp/nortek/aquadopp")
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x81) {
            oceDebug(debug, "these two bytes imply this is adp/nortek/aqudopp (see N3015-023-Integrators-Guide-Classic_1220.pdf page 30)\n")
            oceDebug(debug, "END oceMagic() returning adp/nortek/aquadoppPlusMagnetometer\n", unindent = 1)
            return("adp/nortek/aquadoppPlusMagnetometer")
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x21) {
            oceDebug(debug, "END oceMagic() returning adp/nortek/aquadoppProfiler\n", unindent = 1)
            return("adp/nortek/aquadoppProfiler") # p37 SIG
        }
        if (nextTwoBytes[1] == 0xa5 && nextTwoBytes[2] == 0x2a) {
            oceDebug(debug, "END oceMagic() returning adp/nortek/aquadoppHR\n", unindent = 1)
            return("adp/nortek/aquadoppHR") # p38 SIG
        }
        stop("some sort of nortek ... two bytes are 0x", nextTwoBytes[1], " and 0x", nextTwoBytes[2], " but cannot figure out what the type is")
    }
    if (bytes[1] == 0xa5 && bytes[4] == 0x10) {
        oceDebug(debug, "END oceMagic() returning adp/nortek/ad2cp\n", unindent = 1)
        return("adp/nortek/ad2cp")
    }
    if (bytes[1] == 0x9b && bytes[2] == 0x00) {
        warning(paste(
            "Possibly this is an RDI CTD file. Oce cannot read such files yet, because\n",
            " the author has not located file-format documents.  If you get such documents\n",
            " from RDI, please send them to dan.kelley@dal.ca so the format can be added."
        ))
        return("possibly RDI CTD")
    }
    if (1 == length(grep("^CTD", line))) {
        oceDebug(debug, "END oceMagic() returning ctd/woce/exchange\n", unindent = 1)
        return("ctd/woce/exchange")
    }
    if (1 == length(grep("^EXPOCODE", line))) {
        oceDebug(debug, "END oceMagic() returning ctd/woce/other\n", unindent = 1)
        return("ctd/woce/other")
    }
    if (1 == length(grep("^\\s*ODF_HEADER", line))) {
        oceDebug(debug, "END oceMagic() returning odf\n", unindent = 1)
        return("odf")
    }
    if (grepl("^\\* Sea-Bird SBE", line) || grepl("^\\* Viking Buoy CTD file", line)) {
        oceDebug(debug, "END oceMagic() returning ctd/sbe\n", unindent = 1)
        return("ctd/sbe")
    }

    if (1 == length(grep("^%ITP", line))) {
        oceDebug(debug, "END oceMagic() returning ctd/itp\n", unindent = 1)
        return("ctd/itp")
    }
    if (1 == length(grep("^# -b", line))) {
        oceDebug(debug, "END oceMagic() returning coastline\n", unindent = 1)
        return("coastline")
    }
    if (1 == length(grep("^# Station_Name,", line))) {
        oceDebug(debug, "END oceMagic() returning sealevel\n", unindent = 1)
        return("sealevel")
    }
    if (1 == length(grep("^Station_Name,", line))) {
        oceDebug(debug, "END oceMagic() returning sealevel\n", unindent = 1)
        return("sealevel")
    }
    if (1 == length(grep("^[0-9][0-9][0-9][A-Z] ", line))) {
        oceDebug(debug, "END oceMagic() returning sealevel\n", unindent = 1)
        return("sealevel")
    }
    if (1 == length(grep("^NCOLS[ ]*[0-9]*[ ]*$", line))) {
        oceDebug(debug, "END oceMagic() returning topo\n", unindent = )
        return("topo")
    }
    if (1 == length(grep("^RBR TDR", line))) {
        # FIXME: obsolete; to be removed Fall 2015
        oceDebug(debug, "END oceMagic() returning RBR/dat\n", unindent = 1)
        return("RBR/dat")
    }
    if (1 == length(grep("^Model=", line))) {
        oceDebug(debug, "END oceMagic() returning RBR/txt\n", unindent = 1)
        return("RBR/txt")
    }
    if (1 == length(grep("^BOTTLE", line))) {
        oceDebug(debug, "END oceMagic() returning section\n", unindent = 1)
        return("section")
    }
    oceDebug(debug, "this is unknown\n")
    return("unknown")
}
