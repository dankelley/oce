\name{coastline-class}

\docType{class}

\alias{coastline-class}
\alias{[[,coastline-method}
\alias{[[<-,coastline-method}
\alias{show,coastline-method}

\title{Class to store coastline data}

\description{Class to store coastline data, with standard slots \code{metadata}
    (containing \code{fillable} and \code{filename}),  \code{data} (containing
    \code{longitude} and \code{latitude}) and \code{processingLog}.}

\section{Methods}{

    \subsection{Extracting values}{Positional data may be accessed as e.g.
        \code{coastline[["longitude"]]} or \code{coastline[["latitude"]]}, with
        abbreviations (e.g. \code{coastline[["lo"]]}) being permitted so long
        as they are distinct. However, metadata must be named in full.}

    \subsection{Assigning values}{Latitude may be changed with e.g.
        \code{coastline[["longitude"]] <- value}, and of course the same can be
        done for \code{latitude}.}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(coastline)}) displays information about the object.}
}

\section{Data sources}{The NOAA website
    \url{http://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html} is a popular
    starting point for coastline data. One assumes the data have some sort of
    authority, being provided by a national organization.  Another source for
    coastline data is \url{http://www.naturalearthdata.com}, which provides
    world datasets in a variety of resolutions. Another advantage of the
    Natural Earth is that it provides data not just for coastlines, but also
    for rivers, lakes, glaciated areas, and place names. The \code{oce} dataset
    named \code{\link{coastlineWorld}} came from Natural Earth.}

\section{Archiving your own coastline files}{The present package provides a
    coarse-resolution world coastline in \code{\link{coastlineWorld}}, and the
    \CRANpkg{ocedata} package provides two alternatives at world scale. It is
    common for users to want to use other coastlines, but CRAN policies do not
    allow packages with tens of megabytes of such data. Users should download
    such data and store them in \code{.rda} files with e.g., for a
    shapefile-formate file is named \code{"mycoastline.shp"} to be stored
    in a directory named \code{/data/coastlines}, one might do as
    follows.

    \preformatted{
      coastlineWorld <- oce::read.oce("mycoastline.shp")
      save(coastlineWorld, file="/data/coastlines/mycoastline.rda")
      tools::resaveRdaFiles("/data/coastlines/mycoastline.rda", compress="auto")
    }
    after which accessing the coastline is as simple as writing
    \preformatted{
        load("/data/coastlines/mycoastline.rda")
    }
}

\seealso{Use \code{\link{as.coastline}} to convert data to this form,
    \code{\link{read.coastline}} to read data in various formats, and
    \code{\link{plot.coastline}} to plot coastlines.}

\author{Dan Kelley}

\keyword{classes}

\keyword{oce}

