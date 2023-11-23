#' @section How the binary file is decoded:
#'
#' This file type, like other acoustic-Doppler types, is read
#' with a hybrid R/C++ system, for efficiency.
#' The processing steps are sketched below, for
#' users who want to inspect the code or build upon it.
#'
#' 1. In R, [readBin()] is used to insert the file contents into [vector]
#' of type `raw`.
#'
#' 2. In C++, this raw vector is scanned byte by byte,
#' to find the starting indices of data "chunks", or subsections of
#' the data that correspond to individual sampling times.
#' Checksum computations are also done at this stage, to detect
#' possible data corruption.  Warnings are issued for any bad chunks,
#' and they are skipped in further processing. The list of
#' starting points is then passed back to R as a [vector] of
#' type `integer`.
#'
#' 3. In R, [readBin()] is used to read components of the individual chunks
#' For speed, this is done in a vectorized fashion, e.g. all the velocities
#' are read in a single call, using a suitable subset
#' that spans the length of the buffer. This is done for all the data
#' fields that are handled.  Importantly, the individual [readBin()]
#' calls are tailored to the data, using individualized values of
#' the `size`, `endian` and `signed` parameters.
#' Scaling factors are then applied to convert to physical units.
#'
#' 4. Finally, the acquired items are inserted into the `data` or
#' `metadata` slot of the return value, according to oce convention.
