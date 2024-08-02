library(oce)
f <- "DP1K_000.000"
buf <- readBin(f, "raw", n = 1e4)
i <- 1
if (buf[i] == 0x7f && buf[i + 1] == 0x79) {
    cat("Got 0x7f 0x79 (for waves type) see wavemon manual section 4.5 table 16\n")
    i <- i + 2
    checksumOffset <- readBin(buf[i + 0:1], "integer", n = 1, size = 2)
    cat(" ", vectorShow(checksumOffset))
    i <- i + 2
    spare <- buf[i]
    cat(" ", vectorShow(spare))
    i <- i + 1
    numberOfDataTypes <- readBin(buf[i], "integer", n = 1, size = 1)
    cat(" ", vectorShow(numberOfDataTypes))
    i <- i + 1
    offset <- readBin(buf[i + 0:1], "integer", n = 1, size = 2)
    cat(" ", vectorShow(offset))
    buf[offset + 2]
    i <- offset + 1 # or maybe i + 2
    buf[15] # maybe nbins
    # NEXT FAILS. is manual wrong, or am I reading it incorrectly?
    if (buf[i] == 0x01 && buf[i + 1] == 0x03) {
        cat("  First Leader Type at index ", i, "\n")
    } else {
        cat("  ERROR: want 0x01 0x03 (first leader) but got 0x",
            buf[i], " 0x", buf[i + 1], "\n",
            sep = ""
        )
    }
}

if (FALSE) {
    d <- read.adp.rdi(f, debug = 1)
    summary(d)
}
