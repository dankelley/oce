\name{section}
\docType{section}
\alias{section}
\title{Sample seawater (CTD) section}
\description{A CTD section running the length of Halifax Harbour.}
\usage{data(section)}
\format{
The individual stations are available as \code{section$station[[1]]} through
to \code{section$station[[7]]}, each of which is an object of type
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
p <- seq(0, 30, 2.5)
stn08 <- ctd.trim(read.ctd("BED0308.CNV", station="308"))
stn09 <- ctd.trim(read.ctd("BED0309.CNV", station="309"))
stn10 <- ctd.trim(read.ctd("BED0310.CNV", station="310"))
stn01 <- ctd.trim(read.ctd("BED0301.CNV", station="301"))
stn11 <- ctd.trim(read.ctd("BED0311.CNV", station="311"))
stn12 <- ctd.trim(read.ctd("BED0312.CNV", station="312"))
stn12$latitude <- 44 + 39.894 / 60
stn12 <- processing.log.append(stn12, "DEK/typo - lat minutes from 39.394 to 39.894")
stn13 <- ctd.trim(read.ctd("BED0313.CNV", station="313"))
stn15 <- ctd.trim(read.ctd("BED0315.CNV", station="315"))
stn15$latitude <- stn15$latitude + 1
stn15 <- processing.log.append(stn15, "DEK/typo - add 1 to lat")
section <- make.section(stn08, stn09, stn10, stn01, stn11, stn12, stn13, stn15)
section$section.id <- "Halifax-Harbour-2003-oct"
section <- section.grid(section, p=p, method="boxcar")
data(coastline.halifax)
plot(section, coastline=coastline.halifax, grid=TRUE)
}
The stations cover a 14 km region running from station 308, near the Sackville
River, seaward to station 315, at the entrance to the general Harbour, offshore of
the lovely Point Pleasant Park.
} 
\references{
A summary of the stations can be obtained from \code{\link{summary.section}},
and a plot can be created with \code{\link{plot.section}}.
}
\author{Dan Kelley}
\keyword{misc}
