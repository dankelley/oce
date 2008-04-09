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
class at Dalhousie University.  The course was taught
Dan Kelley (the author of this package) and supervision at sea was
provided by teaching assistant Natacha Bernier, at the time a PhD student at
Dalhousie. Note the alteration of two station locations, to correct errors in
transcribing field notes (e.g. a hand-written \code{8} was evidently typed as
\code{3}).
\preformatted{
library(oce)
stn08 <- ctd.trim(read.ctd("BED0308.CNV", station="8"))
stn09 <- ctd.trim(read.ctd("BED0309.CNV", station="9"))
stn10 <- ctd.trim(read.ctd("BED0310.CNV", station="10"))
stn01 <- ctd.trim(read.ctd("BED0301.CNV", station="1"))
stn11 <- ctd.trim(read.ctd("BED0311.CNV", station="11"))
stn12 <- ctd.trim(read.ctd("BED0312.CNV", station="12"))
stn12 <- oce.edit(stn12, "latitude", 44 + 39.894 / 60,
                  "typo in lat minutes", "Dan Kelley")
stn13 <- ctd.trim(read.ctd("BED0313.CNV", station="13"))
stn15 <- ctd.trim(read.ctd("BED0315.CNV", station="15"))
stn15 <- oce.edit(stn15, "latitude", stn15$metadata$latitude + 1,
                  "typo in lat degrees", "Dan Kelley")
section <- make.section(stn08, stn09, stn10, stn01, stn11, stn12, stn13, stn15)
section <- oce.edit(section, "section.id",  "Halifax-Harbour-2003-oct",
                   "", "Dan Kelley")
p <- seq(0, 30, 2.5)
section <- section.grid(section, p=p, method="boxcar")
data(coastline.hal)
plot(section, coastline=coastline.hal, grid=TRUE)
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
