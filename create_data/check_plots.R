library(oce)

## below constructed from the files in oce/data; ignore data-only items like
## 'colors' and 'wind'.

names <- c("adp", "adv", "argo", "cm", "coastlineWorld", "ctd", "ctdRaw",
           "echosounder", "landsat", "lisst", "lobo", "met", "rsk", "sealevel",
           "sealevelTuktoyaktuk", "section", "topoWorld")
if (!interactive()) pdf("check_plots.pdf")
for (name in names) {
    data(list=name)
    x <- get(name)
    plot(x)
}
if (!interactive()) dev.off()

