b <- readBin("adp_sentinel_v.pd0", what="raw", n=1e6)
writeBin(b, "test.pd0")

