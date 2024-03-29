% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coastline.R
\docType{data}
\name{coastlineWorld}
\alias{coastlineWorld}
\title{Sample coastline Data (Global, at 1:110M scale)}
\source{
Downloaded from \verb{https://www.naturalearthdata.com}, in
\code{ne_110m_admin_0_countries.shp} in July 2015, with an
update on December 16, 2017.
}
\description{
This is a coarse resolution coastline at scale 1:110M, with 10,696 points,
suitable for world-scale plots plotted at a small size, e.g. inset diagrams.
Finer resolution coastline files are provided in the
\CRANpkg{ocedata} package.
}
\section{Installing your own datasets}{
 Follow the procedure along the lines
described in \dQuote{Details}, where of course your source file will differ.
Also, you should change the name of the coastline object from
\code{coastlineWorld}, to avoid conflicts with the built-in dataset. Save
the \code{.rda} file to some directory of your choosing, e.g. perhaps
\verb{/data/coastlines} or \verb{~/data/coastlines} on a unix-type machine.
Then, whenever you need the file, use \code{\link[=load]{load()}} to load it.  Most
users find it convenient to do the loading in an \code{\link[=Rprofile]{Rprofile()}}
startup file.
}

\seealso{
Other datasets provided with oce: 
\code{\link{adp}},
\code{\link{adv}},
\code{\link{amsr}},
\code{\link{argo}},
\code{\link{cm}},
\code{\link{ctd}},
\code{\link{ctdRaw}},
\code{\link{echosounder}},
\code{\link{landsat}},
\code{\link{lisst}},
\code{\link{lobo}},
\code{\link{met}},
\code{\link{ocecolors}},
\code{\link{rsk}},
\code{\link{sealevel}},
\code{\link{sealevelTuktoyaktuk}},
\code{\link{section}},
\code{\link{topoWorld}},
\code{\link{wind}},
\code{\link{xbt}}

Other things related to coastline data: 
\code{\link{[[,coastline-method}},
\code{\link{[[<-,coastline-method}},
\code{\link{as.coastline}()},
\code{\link{coastline-class}},
\code{\link{coastlineBest}()},
\code{\link{coastlineCut}()},
\code{\link{download.coastline}()},
\code{\link{plot,coastline-method}},
\code{\link{read.coastline.openstreetmap}()},
\code{\link{read.coastline.shapefile}()},
\code{\link{subset,coastline-method}},
\code{\link{summary,coastline-method}}
}
\concept{datasets provided with oce}
\concept{things related to coastline data}
