library(oce)
source("R/oce.R")
source("R/odf.R")
source("R/ctd.R")
source("R/ctd.odf.R")
file <- "CTD_1994038_147_1_DN.ODF"
if (!file.exists(file)) {
    dir <- "https://raw.githubusercontent.com/cioos-siooc/cioos-siooc_data_transform/odf_transform/projects/odf_transform/sample_data/test_files"
    download.file(paste0(dir, "/", file), file)
}
d <- read.ctd.odf(file, debug=3)
message("are these ok for flag names? ", paste(names(d@metadata$flags), collapse=","))

