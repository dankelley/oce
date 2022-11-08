library(oce)
user <- function()
{
    res <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME")
        else Sys.getenv("USER")
    if (is.null(res) || 0L == nchar(res))
        res <- "username"
    res
}
csv <- paste0("analysis_", user(), ".csv")
#f <- "SBE19plus_01906009_2019_04_11.cnv"
File <- NULL
Start <- NULL
End <- NULL
files <-  list.files(pattern="*.cnv")
print(files)
if (!file.exists(csv)) {
    cat('file,start,end\n"",NA,NA\n', file=csv)
    message("created empty file '", csv, "'")
} else {
    message("adding to existing file '", csv, "'")
}
a <- read.csv(csv)

for (ifile in seq_along(files)) {
    file <- files[ifile]
    if (file %in% a$file) {
        message(file, " already handled")
        next
    }
    d0 <- read.oce(file)
    # fx and fy are size of 'clear' box, divided by width and height
    fx <- 0.2
    fy <- 0.1
    plotScan(d0, xaxs="i")
    mtext(file, line=0.25)
    usr <- par("usr")
    #<old> # 'clear' box
    #<old> xleft <- usr[1]
    #<old> ybottom <- usr[4] - fy * (usr[4] - usr[3])
    #<old> xright <- usr[1] + fx * (usr[2] - usr[1])
    #<old> ytop <- usr[4]
    #<old> rect(xleft, ybottom, xright, ytop, border=2, lwd=2, col="pink")
    #<old> text(0.5*(xleft+xright), 0.5*(ybottom+ytop), "clear", cex=0.75)
    # START
    while (TRUE) {
        xy <- locator(1)
        abline(v=xy$x, col="forestgreen", lty=2)
        ok <- askYesNo("Is green START line okay",
            TRUE, c("yes","no","quit"))
        if (is.na(ok))
            stop()
        if (ok) {
            start <- xy$x
            break
        }
        plotScan(d0, xaxs="i")
        mtext(file, line=0.25)
    }
    abline(v=xy$x, col="forestgreen", lty=2)
    # END
    while (TRUE) {
        start <- xy$x
        xy <- locator(1)
        abline(v=xy$x, col=2, lty=2)
        ok <- askYesNo("Is red ENDline okay",
            TRUE, c("yes","no","quit"))
        if (is.na(ok))
            stop()
        if (ok) {
            end <- xy$x
            break
        }
        plotScan(d0, xaxs="i")
        mtext(file, line=0.25)
        abline(v=xy$x, col="forestgreen", lty=2)
    }
    cat(sprintf("%s,%d,%d\n", file, start, end))
    File <- c(File, file)
    Start <- c(Start, start)
    End <- c(End, end)
}
df <- data.frame(File=File, Start=Start, End=End)
print(df)
write.csv(df, file=csv, row.names=FALSE, append=TRUE)

