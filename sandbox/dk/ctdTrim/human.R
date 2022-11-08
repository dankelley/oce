library(oce)

# Helper function
user <- function()
{
    res <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME")
        else Sys.getenv("USER")
    if (is.null(res) || 0L == nchar(res))
        res <- "username"
    res
}

# Create analysis_USERNAME.csv, if it doesn't already exist
csv <- paste0("analysis_", user(), ".csv")
if (!file.exists(csv)) {
    cat('file,start,end\n', file=csv)
    message("created a new empty file '", csv, "'")
} else {
    message("appending to an existing file '", csv, "'")
}
analysis <- read.csv(csv)

# Examine all .cnv files in local directory, but skip those already analysed.
files <-  list.files(pattern="*.cnv")
for (ifile in seq_along(files)) {
    file <- files[ifile]
    if (file %in% analysis$file) {
        message("Skipping '", file, "' since it is already in ", csv)
        next
    }
    # Ask user whether to analyse this (useful? waste of time?)
    ok <- try(askYesNo(paste0("Analyse ", file), TRUE, c("yes", "no", "quit")), silent=TRUE)
    if (inherits(ok, "try-error")) {
        warning("bad value entered; assuming 'no'")
        ok <- FALSE
    }
    if (is.na(ok))
        stop("User said 'quit'")
    if (!ok)
        next
    d <- read.oce(file)
    plotScan(d, xaxs="i")
    mtext(file, line=0.25)
    np <- length(d[["pressure"]])
    buf <- np / 20
    # Get START index
    while (TRUE) {
        message("Please click at the downcast START.")
        xy <- locator(1)
        abline(v=xy$x, col="forestgreen", lty=2)
        ok <- try(askYesNo("Is green START line okay", TRUE, c("yes","no","quit")), silent=TRUE)
        if (inherits(ok, "try-error")) {
            warning("bad value entered; assuming 'no'")
            ok <- FALSE
        }
        if (is.na(ok))
            stop("User said 'quit'")
        if (ok) {
            start <- as.integer(round(xy$x))
            break
        }
        xlim <- c(max(1L, xy$x -buf), np)
        plotScan(d, xaxs="i", xlim=xlim)
        mtext(file, line=0.25)
    }
    # Get END index
    while (TRUE) {
        message("Please click at the downcast END.")
        xy <- locator(1)
        abline(v=xy$x, col=2, lty=2)
        ok <- try(askYesNo("Is red END line okay", TRUE, c("yes","no","quit")), silent=TRUE)
        if (inherits(ok, "try-error")) {
            warning("bad value entered; assuming 'no'")
            ok <- FALSE
        }
        if (is.na(ok))
            stop()
        if (ok) {
            end <- as.integer(round(xy$x))
            break
        }
        xlim <- c(max(1L, start - buf), min(xy$x + buf, np))
        plotScan(d, xaxs="i", xlim=xlim)
        mtext(file, line=0.25)
        abline(v=start, col="forestgreen", lty=2)
    }
    # Display and store results. FIXME: the write.csv might be wrong, overwriting file.
    analysis <- rbind(analysis,
        data.frame(file=file, start=start, end=end))
    write.csv(analysis, file=csv, row.names=FALSE)
    print(read.csv(csv))
}

