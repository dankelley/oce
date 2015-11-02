% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{coastlineWorld}

\docType{data}

\alias{coastlineWorld}

\title{World coastline}

\description{World coastline, in coarse resolution and with country boundaries}

\usage{data(coastlineWorld)}

\details{This is a coarse resolution coastline at scale 1:110M, with 10,696
  points, suitable for world-scale plots plotted at a small size, e.g. inset
  diagrams. Finer resolution coastline files are provided in the
  \CRANpkg{ocedata} package.  The data set was created as follows.

  \enumerate{
    \item Visit \url{http://www.naturalearthdata.com} and click the
    \code{downloads} tab. Three data sets are provided. Choose the coarse one,
    labelled ``Small scale data, 1:110m.'' For \code{coastlineWorld}, the link
    named ``Cultural'' was clicked, and this yields a new webpage.
    On this page, find the section named ``Admin 0 - Countries'' and select the
    link named ``Download countries.'' Now you will have a zip file on your
    machine.
    \item Locate and expand the newly-downloaded zip file.
    \item Run the following R code to read and save the coastline
    \preformatted{
      library(oce)
      coastlineWorld <- read.oce("ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
      save(coastlineWorld, file="coastlineWorld.rda")
      library(tools)
      resaveRdaFiles("coastlineWorld.rda", compress="auto")
    }
  }
}

\source{Downloaded from \url{http://www.naturalearthdata.com}, in
  \code{ne_110m_admin_0_countries.shp}. This procedure worked in July 2015 and
  also in October 2015, so it is likely to be reasonably stable, but be aware
  that webpages do tend to change.}

\section{Installing your own datasets}{Follow the procedure along the lines
  described in \dQuote{Details}, where of course your source file will differ.
  Also, you should change the name of the coastline object from
  \code{coastlineWorld}, to avoid conflicts with the built-in dataset. Save the
  \code{.rda} file to some directory of your choosing, e.g. perhaps
  \code{/data/coastlines} or \code{~/data/coastlines} on a unix-type machine.
  Then, whenever you need the file, use \code{\link{load}} to load it.  Most
  users find it convenient to do the loading in an \code{\link{Rprofile}}
  startup file.}

\seealso{The \CRANpkg{ocedata} package provides two more coastlines with better
  resolution: \code{coastlineWorldMedium} and \code{coastlineWorldFine}.
    
  The documentation for \code{\link{coastline-class}} explains the structure of
  \code{coastline} objects and discusses functions that deal with them.

  The \code{maps} package provides a database named \code{world} that has 27221
  points.}

\author{Dan Kelley}

\keyword{datasets}

