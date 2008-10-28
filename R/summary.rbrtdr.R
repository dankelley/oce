summary.rbrtdr <- function(object, ...)
{
    if (!inherits(object, "rbrtdr")) stop("method is only for rbrtdr objects")
    cat("RBR TDR record\n")
    cat("  No. of samples:      ", length(object$data$temperature),  "\n")
    time.range <- range(object$data$t)
    cat(sprintf("  Start time: %s\n", as.character(time.range[1])))
    cat(sprintf("  End   time: %s\n", as.character(time.range[2])))
    cat(sprintf("   %15s  %10s %10s %10s %10s %10s\n", "ITEM", "min", "Q1", "median", "Q3", "max"));
    f <- fivenum(object$data$temperature)
    cat(sprintf("    %15s %10.1f %10.1f %10.1f %10.1f %10.1f\n", "Temperature", f[1], f[2], f[3], f[4], f[5]))
    f <- fivenum(object$data$pressure)
    cat(sprintf("    %15s %10.1f %10.1f %10.1f %10.1f %10.1f\n", "Pressure", f[1], f[2], f[3], f[4], f[5]))
    processing.log.summary(object)
    invisible()                         # BUG: should copy summary.lmle()
}
