\name{landsat-class}

\docType{class}

\alias{landsat-class}
\alias{[[,landsat-method}
\alias{[[<-,landsat-method}
\alias{show,landsat-method}

\title{Class to store Landsat data}

\description{Class to store Landsat data, with standard slots \code{metadata},
    \code{data} and \code{processingLog}.  Landsat data are available at
    several websites (see e.g. [1]). Although the various functions may work
    for other satellites, the discussion here focusses on Landsat 8 and Landsat
    7.}

\section{Landsat 8}{The Landsat 8 satellite has 11 frequency bands, listed
    below (see [2]).
\preformatted{
.------------------------------------------------------------------------------.
| Band | Band                      | Band         | Wavelength    | Resolution |
| No.  | Contents                  | Name         | (micrometers) |   (meters) |
|------+---------------------------+--------------+---------------+------------|
|    1 | Coastal aerosol           | aerosol      |  0.43 -  0.45 |         30 |
|    2 | Blue                      | blue         |  0.45 -  0.51 |         30 |
|    3 | Green                     | green        |  0.53 -  0.59 |         30 |
|    4 | Red                       | red          |  0.64 -  0.67 |         30 |
|    5 | Near Infrared (NIR)       | nir          |  0.85 -  0.88 |         30 |
|    6 | SWIR 1                    | swir1        |  1.57 -  1.65 |         30 |
|    7 | SWIR 2                    | swir2        |  2.11 -  2.29 |         30 |
|    8 | Panchromatic              | panchromatic |  0.50 -  0.68 |         15 |
|    9 | Cirrus                    | cirrus       |  1.36 -  1.38 |         30 |
|   10 | Thermal Infrared (TIRS) 1 | tirs1        | 10.60 - 11.19 |        100 |
|   11 | Thermal Infrared (TIRS) 2 | tirs2        | 11.50 - 12.51 |        100 |
.------------------------------------------------------------------------------.
}

    Band 8 is panchromatic, and has the highest resolution.  For convenience of
    programming, \code{\link{read.landsat}} subsamples the \code{tirs1} and
    \code{tirs2} bands to the 30m resultion of the other bands.  See Reference
    [3] for information about the evolution of Landsat 8 calibration
    coefficients, which as of summer 2014 are still subject to change.  }

\section{Landsat 7}{

    Band information is as follows (from [8]).  The names are not official, but
    are just set up to roughly Landsat-8 names, according to wavelength.  An
    exception is the Landsat-7 bands named \code{tirs1} and \code{tirs2}, which
    are at two different gain settings, with identical wavelength span for
    each, which roughly matches the range of the Landsat-8 bands \code{tirs1}
    and \code{tirs2} combined.  This may seem confusing, but it lets code like
    \code{plot(im, band="tirs1")} to work with both Landsat-8 and Landsat-7.

\preformatted{
.------------------------------------------------------------------------------.
| Band | Band                      | Band         | Wavelength    | Resolution |
| No.  | Contents                  | Name         | (micrometers) |   (meters) |
|------+---------------------------+--------------+---------------+------------|
|    1 | Blue                      | blue         |  0.45 -  0.52 |         30 |
|    2 | Green                     | green        |  0.52 -  0.60 |         30 |
|    3 | Red                       | red          |  0.63 -  0.69 |         30 |
|    4 | Near IR                   | nir          |  0.77 -  0.90 |         30 |
|    5 | SWIR                      | swir1        |  1.55 -  1.75 |         30 |
|    6 | Thermal IR                | tirs1        | 10.4  - 12.50 |         30 |
|    7 | Thermal IR                | tirs2        | 10.4  - 12.50 |         30 |
|    8 | SWIR                      | swir2        |  2.09 -  2.35 |         30 |
|    9 | Panchromatic              | panchromatic |  0.52 -  0.90 |         15 |
.------------------------------------------------------------------------------.
}
}

\section{Methods}{

    Consider a landsat object named \code{landsat}.

    \emph{Accessing data values.} The data may be accessed with e.g.
    \code{landsat[["panchromatic"]]}, for the panchromatic band.  If a new
    ``band'' is added with \code{\link{landsatAdd}}, it may be referred by
    name.  In all cases, a second argument can be provided, to govern
    decimation.  If this is missing, all the relevant data are returned.  If
    this is present and equal to \code{TRUE}, then the data will be
    automatically decimated (subsampled) to give approximately 800 elements in
    the longest side of the matrix.  If this is present and numerical, then its
    value governs decimation.  For example,
    \code{landsat[["panchromatic",TRUE]]} will auto-decimate, typically
    reducing the grid width and height from 16000 to about 800.  Similarly,
    \code{landsat[["panchromatic",10]]} will reduce width and height to about
    1600.  On machines with limited RAM (e.g. under about 6GB), decimation is a
    good idea in almost all processing steps.  It also makes sense for
    plotting, and in fact is done through the \code{decimate} argument of
    \code{\link{plot.landsat}}.
  
    \emph{Accessing derived data.}  One may retrieve several derived quantities
    that are calculated from data stored in the object:
    \code{landsat[["longitude"]]} and \code{landsat[["latitude"]]} give pixel
    locations.  Accessing \code{landsat[["temperature"]]} creates an estimate
    of ground temperature as follows (see [4]).  First, the ``count value'' in
    band 10, denoted \eqn{b_{10}}{b_10} say, is scaled with coefficients stored
    in the image metadata using
    \eqn{\lambda_L=b_{10}M_L+A_L}{lambda_L=b_10*M_L+A_L} where \eqn{M_L}{M_L}
    and \eqn{A_L}{A_L} are values stored in the metadata (e.g.  the first in
    \code{landsat@metadata$header$radiance_mult_band_10}) Then the result is
    used, again with coefficients in the metadata, to compute Celcius
    temperature \eqn{T=K_2/ln(\epsilon
        K_1/\lambda_L+1)-273.15}{T=K_2/ln(epsilon*K_1/\lambda_L+1)-273.15}.
    The value of the emissivity \eqn{\epsilon}{epsilon} is set to unity by
    \code{\link{read.landsat}}, although it can be changed easily later, by
    assigning a new value to \code{landsat@metadata$emissivity}. The default
    emissivity value set by \code{\link{read.landsat}} is from [11], and is
    within the oceanic range suggested by [5]. Adjustment is as simple as
    altering \code{landsat@metadata$emissivity}. This value can be a single
    number meant to apply for the whole image, or a matrix with dimensions
    matching those of band 10.  The matrix case is probably more useful for
    images of land, where one might wish to account for the different
    emissivities of soil and vegetation, etc.; for example, Table 4 of [9]
    lists 0.9668 for soil and 0.9863 for vegetation, while Table 5 of [10]
    lists 0.971 and 0.987 for the same quantities.
    
    \emph{Accessing metadata.} Anything in the metadata can be accessed by
    name, e.g. \code{landsat[["time"]]}.  Note that some items are simply
    copied over from the source data file and are not altered by e.g.
    decimation.  An exception is the lat-lon box, which is altered by
    \code{\link{landsatTrim}}.

    \emph{Overview of contents.} The \code{summary} method displays information
    about the object.

}

\section{Data storage}{Oce stores the satellite data in 16-bit resolution.  Oce
    breaks these 16 bits up into most-significant and least-significant bytes.
    For example, the aerosol band of a Landsat object named \code{x} are
    contained within \code{x@data$aerosol$msb} and \code{x@data$aerosol$lsb},
    each of which is a matrix of raw values.  The results may be combined as
    e.g.  \code{256L*as.integer(x@data[[i]]$msb) +
        as.integer(x@data[[i]]$lsb)}, and this is what is returned by executing
    \code{x[["aerosol"]]}.

    Landsat data files typically occupy approximately a
    gigabyte of storage.  That means that corresponding Oce objects are about
    the same size, and this can pose significant problems on computers with
    less than 8GB of memory.  It is sensible to specify bands of interest when
    reading data with \code{\link{read.landsat}}, and also to use
    \code{\link{landsatTrim}} to isolate geographical regions that need
    processing.
    
    Experts may need to get direct access to the data, and this is easy because
    all Landsat objects (regardless of satellite) use a similar storage form.
    Band information is stored in byte form, to conserve space.  Two bytes are
    used for each pixel in Landsat-8 objects, with just one for other objects.
    For example, if a Landsat-8 object named \code{L} contains the \code{tirs1}
    band, the most- and least-significant bytes will be stored in matrices
    \code{L@data$tirs1$msb} and \code{L@data$tirs1$lsb}.  A similar Landsat-7
    object would have the same items, but \code{msb} would be just the value
    \code{0x00}.  
    
    Derived bands, which may be added to a landsat object with
    \code{\link{landsatAdd}}, are not stored in byte matrices.  Instead they
    are stored in numerical matrices, which means that they use 4X more storage
    space for Landsat-8 images, and 8X more storage space for other satellites.
    A computer needs at least 8GB of RAM to work with such data.}

\seealso{A file containing Landsat data may be read with
    \code{\link{read.landsat}} or \code{\link{read.oce}}, and one such file is
    provided by the \code{ocedata} package as a dataset named \code{landsat}.

    Plots may be made with \code{\link{plot.landsat}}.  Since plotting can be
    quite slow, decimation is available both in the plotting function and as
    the separate function \code{\link{decimate}}.  Images may be subsetted with
    \code{\link{landsatTrim}}.
}

\references{
1. \url{http://glovis.usgs.gov} 

2. \url{http://landsat.gsfc.nasa.gov/?page_id=5377}

3. \url{http://landsat.usgs.gov/calibration_notices.php}

4. \url{http://dankelley.github.io/r/2014/07/01/landsat.html}

5. \url{http://scienceofdoom.com/2010/12/27/emissivity-of-the-ocean/}

6. \url{http://landsat.usgs.gov/Landsat8_Using_Product.php}

7. \url{http://landsathandbook.gsfc.nasa.gov/pdfs/Landsat7_Handbook.pdf}

8. \url{http://landsat.usgs.gov/band_designations_landsat_satellites.php}

9. Yu, X. X. Guo and Z. Wu., 2014. Land Surface Temperature Retrieval from
Landsat 8 TIRS-Comparison between Radiative Transfer Equation-Based Method,
Split Window Algorithm and Single Channel Method, \emph{Remote Sensing}, 6,
9829-9652.  \url{http://www.mdpi.com/2072-4292/6/10/9829}

10. Rajeshwari, A., and N. D. Mani, 2014.  Estimation of land surface
temperature of Dindigul district using Landsat 8 data.  \emph{International
    Journal of Research in Engineering and Technology}, 3(5), 122-126.
\url{http://www.academia.edu/7655089/ESTIMATION_OF_LAND_SURFACE_TEMPERATURE_OF_DINDIGUL_DISTRICT_USING_LANDSAT_8_DATA}

11. Konda, M. Imasato N., Nishi, K., and T. Toda, 1994.  Measurement of the Sea
Surface Emissivity.  \emph{Journal of Oceanography}, 50, 17:30.
\url{http://www.terrapub.co.jp/journals/JO/pdf/5001/50010017.pdf}
}


\author{Dan Kelley}

\keyword{classes}

\keyword{oce}

