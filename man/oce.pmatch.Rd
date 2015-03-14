\name{oce.pmatch}

\alias{oce.pmatch}
\alias{ocePmatch}

\title{Partial matching of strings or numbers}

\description{An extended version of \code{\link{pmatch}} that allows \code{x}
    to be numeric or string-based.  As with \code{\link{pmatch}}, partial
    string matches are handled.
}

\usage{oce.pmatch(x, table, nomatch=NA_integer_, duplicates.ok=FALSE)}

\arguments{

    \item{x}{a code, or vector of codes.  This may be numeric, in which case it
        is simply returned without further analysis of the other arguments, or
        it may be string-based, in which case \code{\link{pmatch}} is used to
        find numeric matches.}

    \item{table}{a list that maps strings to numbers; \code{\link{pmatch}} is
        used on \code{names(table)}.  If the name contains characters that are
        normally not permitted in a variable name, use quotes, e.g.
        \code{list(salinity=1, temperature=2, "salinity+temperature"=3)}. }

    \item{nomatch}{value to be returned for cases of no match (passed to
        \code{\link{pmatch}}.}

    \item{duplicates.ok}{code for the handling of duplicates (passed to
        \code{\link{pmatch}}).}

}

\details{This is a wrapper that is useful mainly for \code{which} arguments to
    plotting functions.}

\value{A number, or vector of numbers, corresponding to the matches.
    Non-matches are indicated with \code{NA} values, or whatever value is
    given by the \code{NA} argument.}

\seealso{Since \code{\link{pmatch}} is used for the actual matching, its
    documentation should be consulted.}

\examples{
library(oce)
oce.pmatch(c("s", "at", "te"), list(salinity=1, temperature=3.1))
}

\author{Dan Kelley}

\keyword{misc}
