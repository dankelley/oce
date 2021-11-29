library(oce)
source("R/imagep.R")
data(section)
stations <- section[['station']]
meanT <- unlist(lapply(stations, function(x) mean(x[["temperature"]])))
cm <- colormap(meanT)
length(cm$zlim) # Showing length of 2
#> [1] 2
drawPalette(cm,pos=3)
drawPalette(colormap=cm,pos=3)
#> Error in drawPalette(cm, pos = 3): 'zlim' must be of length 2

