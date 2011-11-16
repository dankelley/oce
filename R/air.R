airRho <- function(temperature, pressure, humidity)
{
    Tkelvin <- temperature + 273.15
    ## http://en.wikipedia.org/wiki/Density_of_air
    M <- 0.0289644                      # kg/mol
    R <- 287.058
    R <- 8.31447
    ##1.225
    M * pressure / R / Tkelvin
}

