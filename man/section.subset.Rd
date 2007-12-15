\name{section.subset}
\alias{section.subset}
\title{Subset a section}
\description{Subset a section, by removing stations or rearranging their order.}
\usage{subset <- section.subset(section, indices=1:length(section$stations))}
\arguments{
  \item{section}{a section to be subsetted.}
  \item{indices}{the index numbers of stations to keep, e.g. the first station in the section has index 1.}
}
\details{
Stations with the provided \code{indices} are kept, and the rest are deleted. The \code{indices}
may be in any order, and may contain gaps. Common uses of \code{section.subset} include dropping
stations that have erroneous data, dropping stations that are separated from a cruise track of
interest, and reordering a section so that \code{plot.section} will measure distance from a
particular station of interest (see Examples).
}
\value{
A \code{section} object, containing only the indicated stations, in the indicated order.
}
\seealso{
Sections are normally created with \code{\link{read.section}} or \code{\link{make.section}}.
}
\examples{
library(oce)
data(a03)
# Gulf Stream (note that section runs east to west)
Gulf.Stream <- section.subset(a03, 124:102)
data(coastline.world)
plot(section.grid(Gulf.Stream), coastline=coastline.world, map.xlim=c(-80,-60))
}
\author{Dan Kelley}
\keyword{misc}
