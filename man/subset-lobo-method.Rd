% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lobo.R
\name{subset,lobo-method}
\alias{subset,lobo-method}
\title{Subset a lobo Object}
\usage{
\S4method{subset}{lobo}(x, subset, ...)
}
\arguments{
\item{x}{a \linkS4class{lobo} object.}

\item{subset}{a condition to be applied to the \code{data} portion of
\code{x}.  See \dQuote{Details}.}

\item{\dots}{ignored.}
}
\value{
A \linkS4class{lobo} object.
}
\description{
Subset an lobo object, in a way that is somewhat
analogous to \code{\link[=subset.data.frame]{subset.data.frame()}}.
}
\seealso{
Other things related to lobo data: 
\code{\link{[[,lobo-method}},
\code{\link{[[<-,lobo-method}},
\code{\link{as.lobo}()},
\code{\link{lobo}},
\code{\link{lobo-class}},
\code{\link{plot,lobo-method}},
\code{\link{read.lobo}()},
\code{\link{summary,lobo-method}}

Other functions that subset oce objects: 
\code{\link{subset,adp-method}},
\code{\link{subset,adv-method}},
\code{\link{subset,amsr-method}},
\code{\link{subset,argo-method}},
\code{\link{subset,cm-method}},
\code{\link{subset,coastline-method}},
\code{\link{subset,ctd-method}},
\code{\link{subset,echosounder-method}},
\code{\link{subset,met-method}},
\code{\link{subset,oce-method}},
\code{\link{subset,odf-method}},
\code{\link{subset,rsk-method}},
\code{\link{subset,sealevel-method}},
\code{\link{subset,section-method}},
\code{\link{subset,topo-method}},
\code{\link{subset,xbt-method}}
}
\author{
Dan Kelley
}
\concept{functions that subset oce objects}
\concept{things related to lobo data}
