## Create compact color palettes.  This is better than storing these in R code because
## 1. Each palette takes under 1K, compared with 7K in R.
## 2. The data are available for any programmatic use, which is not true if they
##    are locked within R code.
## 3. Adding new colormaps should be easy; just modify the present file, run "make",
##    copy the .rda to ../../data, and then modify ../../R/oce.R (look at oce.colorsViridis
##    for an example).
## 4. R/oce.R is reduced by 256 lines for each color palette moved here, which makes
##    it a lot easier to read and modify.

## Data files should be in the format of viridis.dat, i.e. three columns of
## space-separated R, G, and B data.  There is no need to gzip the files,
## because they do not go into the R package as stored on CRAN.
##
## Procedure:
##    make
##    make install

## cmocean
cmoceanFiles <- c("CDOM-rgb.txt", "Chlorophyll-rgb.txt", "Density-rgb.txt",
                  "Freesurface-rgb.txt", "Oxygen-rgb.txt", "PAR-rgb.txt",
                  "Phase-rgb.txt", "Salinity-rgb.txt", "Temperature-rgb.txt",
                  "Turbidity-rgb.txt", "Velocity-rgb.txt", "Vorticity-rgb.txt")
files <- c("turbo.txt", "viridis.txt", cmoceanFiles)
ocecolors <- list()
for (file in files) {
    oceName <- tolower(gsub(".txt$", "", gsub("-rgb", "", file)))
    rgb <- read.table(file, header=FALSE)
    message("oceName '", oceName, "', file '", file, "'\n")
    message("    first line: ", paste(rgb[1,], collapse=" "))
    message("    first line: ", paste(255*rgb[1,], collapse=" "))
    ocecolors[[oceName]] <- rgb(r=rgb$V1, g=rgb$V2, b=rgb$V3)
}

save(ocecolors, file="ocecolors.rda", version=2)
tools::resaveRdaFiles("ocecolors.rda", version=2)

