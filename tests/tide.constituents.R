create.constituents <- function(file="tide3.dat", nrows=146) # FIXME: work with rest of file
{
	x <- read.table(file=file, nrows=nrows, fill=TRUE, as.is=TRUE, header=FALSE)
	data.frame(name=x$V1, frequency=x$V2, compare=x$V3, standard=x$V3!="", stringsAsFactors=FALSE)
}
tidal.constituents <- create.constituents()
# These values are then hand-edited into the code of tide.constituents.R
stopifnot(tidal.constituents$name[1] == "Z0")
stopifnot(tidal.constituents$name[2] == "SA")
stopifnot(tidal.constituents$frequency[2] == 0.0001140741)
stopifnot(tidal.constituents$compare[2] == "SSA")
