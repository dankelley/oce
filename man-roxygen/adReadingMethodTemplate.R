#' @section How the binary file is decoded:
#'
#' This file type, like other acoustic-Doppler types, is read
#' with a hybrid R/C++ system, for efficiency.
#' The processing steps are sketched below, for
#' users who want to inspect the code or build upon it.
#'
#' 1. In R, [readBin()] is used to insert the file contents into
#' a [vector] of type `raw`.
#'
#' 2. In C++, this raw vector is scanned byte by byte,
#' to find the starting indices of data "chunks", or subsections of
#' the data that correspond to individual sampling times.
#' Checksum computations are also done at this stage, to detect
#' possible data corruption.  Warnings are issued for any bad chunks,
#' and they are skipped in further processing. The valid
#' starting points are then passed back to R as a [vector] of
#' type `integer`.
#'
#' 3. In R, [readBin()] is used to read the components of each chunk.
#' For speed, this is done in a vectorized fashion. For example,
#' all the velocities in the whole file are read in a single call
#' to [readBin()]. This process is done for each of the data
#' fields that are to be handled.  Importantly, these [readBin()]
#' calls are tailored to the data, using values of
#' the `size`, `endian` and `signed` parameters that are tailored
#' to the structure of the given component.
#' Scaling factors are then applied as required, to convert the
#' components to physical units.
#'
#' 4. Finally, in R, the acquired items are inserted into the `data` or
#' `metadata` slot of the return value, according to oce convention.
