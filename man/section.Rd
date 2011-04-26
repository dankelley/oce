\name{section}
\alias{section}
\title{Seawater CTD section}
\description{A CTD section running the length of Halifax Harbour}
\usage{data(section)}

\format{The individual stations are available as \code{ctd} objects, in
  \code{section$data$station[[i]]}, with \code{i} in the range from 1 to
  7.}

\source{This section is based on measurements made during October of
  2003 in Halifax Harbour by students in the \emph{Introduction to
  Physical Oceanography} class at Dalhousie University.  The course was
  taught by the author of this package, and supervision at sea was
  provided by N. Bernier, who was then a PhD student at Dalhousie. Note
  the alteration of two station locations, to correct errors in
  transcribing field notes (e.g. a hand-written \code{8} was evidently
  typed as \code{3}).

\preformatted{
library(oce)
stn08 <- ctdTrim(read.ctd("BED0308.CNV", station="8"))
stn09 <- ctdTrim(read.ctd("BED0309.CNV", station="9"))
stn10 <- ctdTrim(read.ctd("BED0310.CNV", station="10"))
stn01 <- ctdTrim(read.ctd("BED0301.CNV", station="1"))
stn11 <- ctdTrim(read.ctd("BED0311.CNV", station="11"))
stn12 <- ctdTrim(read.ctd("BED0312.CNV", station="12"))
stn12 <- oceEdit(stn12, "latitude", 44 + 39.894 / 60,
                 reason="typo in lat minutes", person="Dan Kelley")
stn13 <- ctdTrim(read.ctd("BED0313.CNV", station="13"))
stn15 <- ctdTrim(read.ctd("BED0315.CNV", station="15"))
stn15 <- oceEdit(stn15, "latitude", stn15$metadata$latitude + 1,
                 reason="typo in lat degrees", person="Dan Kelley")
section <- makeSection(stn08,stn09,stn10,stn01,stn11,stn12,stn13,stn15)
section <- oceEdit(section, "sectionId",  "Halifax-Harbour-2003-oct",
                   person="Dan Kelley")
section <- sectionGrid(section, p=seq(0,60,2))
save(section, file="section.rda")
}
The stations cover a 14 km region running from station 308, near the
Sackville River, seaward to station 315, at the entrance to the general
Harbour, offshore of the aptly-named Point Pleasant Park.
} 

\examples{
data(section)
data(coastlineHalifax)
plot(section, coastline=coastlineHalifax, grid=TRUE)
}

\references{A summary of the stations can be obtained from \code{\link{summary.section}},
and a plot can be created with \code{\link{plot.section}}.}

\author{Dan Kelley}

\keyword{misc}
