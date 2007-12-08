\name{section}
\docType{section}
\alias{section}
\title{Sample seawater (CTD) section}
\description{A CTD section running the length of Halifax Harbour.}
\usage{data(section)}
\format{
The individual stations are available as \code{section$stations[[1]]} through
to \code{section$stations[[7]]}, each of which is an object of type
\code{ctd}.
}
\source{
This section is based on measurements made during October of 2003 in
Halifax Harbour by students in the \emph{Introduction to Physical Oceanography}
class at Dalhousie University, taught by Dan Kelley; supervision at sea was
provided by teaching assistant Natacha Bernier, at the time a PhD student at
Dalhousie. Note the alteration of two station locations, to correct errors in
transcribing field notes (e.g. a hand-written ``8'' was evidently typed as ``3'').
\preformatted{
library(oce)
p <- seq(0, 30, 0.5)
stn08 <- ctd.decimate(ctd.trim(read.ctd("BED0308.CNV", station="308")),p=p)
stn09 <- ctd.decimate(ctd.trim(read.ctd("BED0309.CNV", station="309")),p=p)
stn10 <- ctd.decimate(ctd.trim(read.ctd("BED0310.CNV", station="310")),p=p)
stn01 <- ctd.decimate(ctd.trim(read.ctd("BED0301.CNV", station="301")),p=p)
stn11 <- ctd.decimate(ctd.trim(read.ctd("BED0311.CNV", station="311")),p=p)
stn12 <- ctd.decimate(ctd.trim(read.ctd("BED0312.CNV", station="312")),p=p)
stn12$latitude <- 44 + 39.894 / 60
stn12 <- processing.log.append(stn12, "DEK: change lat minutes from 39.394 to 39.894 (typo)")
stn13 <- ctd.decimate(ctd.trim(read.ctd("BED0313.CNV", station="313")),p=p)
stn15 <- ctd.decimate(ctd.trim(read.ctd("BED0315.CNV", station="315")),p=p)
stn15$latitude <- stn15$latitude + 1
stn15 <- processing.log.append(stn15, "DEK add 1 to lat (typo")
section <- make.section(stn08, stn09, stn10, stn01, stn11, stn12, stn13, stn15)
section$section.id <- "Halifax-Harbour-2003-oct"
}
The stations cover a 14 km region running from station 308, near the Sackville
River, seaward to station 315, at the entrance to the general Harbour, offshore of
the lovely Point Pleasant Park.
} 
\references{
A summary of the stations can be obtained from \code{\link{summary.section}},
and a plot can be created with \code{\link{plot.section}}.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
