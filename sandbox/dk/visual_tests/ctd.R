library(oce)
# plot
data(ctd)
# do both EOS

w <- c("1", "2", "3", "4", "5", "5.1", "6", "7", "8", "9", "10",
    "11", "12", "13", "14", "15", "16", "17", "20", "21", "30",
    "31", "32", "33")
for (eos in c("unesco", "gsw")) {
    for (which in w) {
        if (which %in% c("14")) {
            message("cannot plot which=", which)
        } else {
            message("which=", which)
            png(sprintf("ctd_%s_%s.png", which, eos))
            plot(ctd, which=which, eos=eos)
            mtext(paste0("which=", which, ", eos=", eos),
                col=4, font=2, line=-1.5, cex=1.2)
            message(" ... plotted")
            dev.off()
        }
    }
}
