---
title: Updating oce data(topoWorld)
author: Dan Kelley
date: 2019 May 11
---

To get an up-to-date idea of the URL for topo data I visited

https://maps.ngdc.noaa.gov/viewers/wcs-client/

and used a dialog box to set lonW=0 lonE=10 latS=0 latN=10, which yielded the following URL at the 'download' link.

https://gis.ngdc.noaa.gov/mapviewer-support/wcs-proxy/wcs.groovy?filename=etopo1_bedrock.nc&request=getcoverage&version=1.0.0&service=wcs&coverage=etopo1_bedrock&CRS=EPSG:4326&format=netcdf&resx=0.016666666666666667&resy=0.016666666666666667&bbox=0,0,9.999999999999831,9.999999999999837

For comparison, oce create_data/topo/create_topoWorld_new.R uses

```
topoFile <- download.topo(west=-179.5, east=180, south=-89.5, north=90,
                          resolution=30, format="netcdf", destdir=".", debug=3)
```
uses the URL

https://gis.ngdc.noaa.gov/cgi-bin/public/wcs/etopo1.xyz?filename=etopo1.nc&request=getcoverage&version=1.0.0&service=wcs&coverage=etopo1&CRS=EPSG:4326&format=netcdf&resx=0.500000&resy=0.500000&bbox=-179.500000,-89.500000,180.000000,90.000000

and the data read from that have lon=-179.25 -178.75 ... 179.75 (length 719)

By contrast, the data(topoWorld) as of yesterday has lon=-179.5 -179.0 ... 180 (length 720)

So what gives?? Why doesn't the new one start at -179.5?  Let's try with full limits (call this test2):

https://gis.ngdc.noaa.gov/mapviewer-support/wcs-proxy/wcs.groovy?filename=etopo1.nc&request=getcoverage&version=1.0.0&service=wcs&coverage=etopo1&CRS=EPSG:4326&format=netcdf&resx=0.500000&resy=0.500000&bbox=-180,-90,180,90

```
f <- nc_open("~/Downloads/test2.nc")
vectorShow(ncvar_get(f, "lon"))
[1] "ncvar_get(f, \"lon\"): -180.0, -179.5, ..., 179.5, 180.0 (length 721)\n"
```

So, now try with (call that test3)

https://gis.ngdc.noaa.gov/mapviewer-support/wcs-proxy/wcs.groovy?filename=etopo1.nc&request=getcoverage&version=1.0.0&service=wcs&coverage=etopo1&CRS=EPSG:4326&format=netcdf&resx=0.500000&resy=0.500000&bbox=-179.5,-89.5,180,90


```
f <- nc_open("~/Downloads/test3.nc")
vectorShow(ncvar_get(f, "lon"))
[1] "ncvar_get(f, \"lon\"): -179.5, -179.0, ..., 179.5, 180.0 (length 720)\n"
```

so that at least is going to give us the same lon and lat. What about z, though? The built-in dataset has
```
topoWorld[["z"]][1:3, 1:3]
     [,1] [,2] [,3]
[1,] 2895 2987 3078
[2,] 2895 2987 3078
[3,] 2895 2987 3078
```
and the new one has
```
     [,1] [,2] [,3]
[1,] 2869 2985 3080
[2,] 2868 2983 3080
[3,] 2866 2981 3079
```

so not the same. Oh my. I'll try some broader tests in 1551a.R
