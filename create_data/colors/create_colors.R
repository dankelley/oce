## Create compact colour palettes.  This is better than storing these in R code because
## 1. Each palette takes under 1K, compared with 7K in R.
## 2. The data are available for any programmatic use, which is not true if they
##    are locked within R code.
## 3. Adding new colormaps should be easy; just modify the present file, run "make",
##    copy the .rda to ../../data, and then modify ../../R/oce.R (look at oce.colorsViridis
##    for an example).
## 4. R/oce.R is reduced by 256 lines for each color palette moved here, which makes
##    it a lot easier to read and modify.

## Data files should be in the format of viridis.dat, i.e. a header line followed
## by R/G/B values in the 0-1 range. There is no need to gzip the files, because they
## do not go into the R package as stored on CRAN.

## Viridis
rgb <- read.table("viridis.dat")
colors <- list(viridis=rgb(r=rgb$V1, g=rgb$V2, b=rgb$V3))

## cmocean colours

cmoceanFiles <- c("CDOM-rgb.txt", "Chlorophyll-rgb.txt", "Density-rgb.txt",
                  "Freesurface-rgb.txt", "Oxygen-rgb.txt", "PAR-rgb.txt",
                  "Phase-rgb.txt", "Salinity-rgb.txt", "Temperature-rgb.txt",
                  "Turbidity-rgb.txt", "Velocity-rgb.txt", "Vorticity-rgb.txt")
for (cmoceanFile in cmoceanFiles) {
    oceName <- tolower(gsub("-rgb.txt", "", cmoceanFile))
    rgb <- read.table(paste("cmocean", cmoceanFile, sep="/"), header=FALSE)
    colors[[oceName]] <- rgb(r=rgb$V1, g=rgb$V2, b=rgb$V3)
}

## Put other colormaps above, and add to the list below.

save(colors, file="colors.rda")
tools::resaveRdaFiles("colors.rda")

