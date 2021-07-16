if (FALSE) {
    download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip",
                  destfile = "CL.zip")
    unzip("CL.zip", exdir="CL")
    CL <- rgdal::readOGR("CL/ne_10m_coastline.shp")
}
