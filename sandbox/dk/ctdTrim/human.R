library(oce)
#f <- "SBE19plus_01906009_2019_04_11.cnv"
File <- NULL
Start <- NULL
End <- NULL
for (file in list.files(pattern="*.cnv")) {
    d0 <- read.oce(file)
    plotScan(d0)
    indices <- as.integer(round(locator(2)$x))
    mtext(paste(file,",",indices[1],",",indices[2]))
    start <- min(indices)
    end <- max(indices)
    abline(v=indices, col=2)
    File <- c(File, file)
    Start <- c(Start, indices[1])
    End <- c(End, indices[2])
    cat(sprintf("%s,%d,%d\n", file, start, end))
}
df <- data.frame(File=File, Start=Start, End=End)
write.csv(df, file="analysis_dk.csv")



