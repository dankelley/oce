library(oce)
library(testthat)

# SECRET test file (not on github, only shared among 2 oce developers)
f <- "~/Dropbox/oce_secret_data/ctd_with_location.rsk"
if (file.exists(f)) {
    dist <- function(s1, s2) {
        lon1 <- s1[["longitude"]]
        lat1 <- s1[["latitude"]]
        lon2 <- s2[["longitude"]]
        lat2 <- s2[["latitude"]]
        sqrt(mean(geodDist(lon1, lat1, lon2, lat2)^2))
    }
    # 1. Show equality of A (auto-inferred tzOffsetLocation) and B (specified)
    expect_error(read.rsk(f, allTables=FALSE) |> ctdFindProfilesRBR() |> as.section(),
        "the metadata slot must contain \"regionCast\"")
    A <- read.rsk(f) |> ctdFindProfilesRBR() |> as.section()
    off <- -28802102 / 3600 / 1000 # 8.000584 h
    B <- read.rsk(f, tzOffsetLocation=off) |> ctdFindProfilesRBR() |> as.section()
    C <- read.rsk(f, tzOffsetLocation=-8) |> ctdFindProfilesRBR() |> as.section()
    cat(sprintf("Offset for inferref tz offset vs specified 8.0005484 h: %f km\n", dist(A, B)))
    cat(sprintf("Offset for inferred tz offset vs specified 8 h: %f km\n", dist(A, C)))
    D <- read.rsk(f, tzOffsetLocation=0) |> ctdFindProfilesRBR() |> as.section()
    cat(sprintf("Offset for inferred tz offset vs specified 0 h: %f km\n", dist(A, D)))
}
