\name{adv-class}

\docType{class}

\alias{adv-class}
\alias{[[,adv-method}

\title{Class to store acoustic Doppler velocimeter data}

\description{Class to store acoustic Doppler velocimeter data, with standard
    slots \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{adv[["u1"]]}, where the
    string could also be \code{"u2"} or \code{"u3"} for the other velocity components,
    or \code{"heading"}, \code{"pitch"} or \code{"roll"} for the instrument angles, or
    \code{"temperature"} for the temperature.}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}

