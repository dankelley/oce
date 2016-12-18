**Contents of directory.** This document explains how the files in this
directory were created.  In many cases, the data are truncated using `dd`,
because the original files are too large to be stored on github.

**Instructions for adding to this directory.** 

1. Place entries in alphabetical order. Change the numbering of the headers, as
   required. Try to use file names that will have some meaning to a reader
doing a file listing. 

2. Put an entry in a `.R` file in the parent directory to read the file and
   test the resultant data in some useful way. The file name should be start
with `test_local`.  Some tests may involve comparison against a second source,
e.g. information provided by manufacturer's software or inferred by inspection
of the data file with an editor or other software.  It also very helpful to
have consistency checks to guard against changes to oce; these can be
constructed simply by reading in the data once, and creating a test that the
same results are obtained by rereading.

**How these file are used.** Simply adding a file here does nothing useful.  

# 1. ADP: RDI, Nortek, and Sontek

## 1.1 RDI
```
dd if=/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000 \
   of=rdi bs=40000 count=1
```

## 1.2 Nortek Aquadopp
```
dd if=/data/archive/sleiwex/2008/moorings/m05/adp/nortek_aqd2843/raw/adp_nortek_aqd2843.prf \
   of=adp_nortek_aquadopp bs=40000 count=1
```

## 1.3 Sontek
```
dd if=/data/archive/sleiwex/2008/moorings/m07/adp/sontek_h53/raw/adp_sontek_h53.adp \
   of=adp_sontek bs=40000 count=1
```



# 2. ADV

## 2.1 Nortek

```
dd if=/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/raw/adv_nortek_1943.vec \
   of=adv_nortek_vector bs=40000 count=1
```

## 2.w Sontek

```
dd if=/data/archive/sleiwex/2008/moorings/m03/adv/sontek_b373h/raw/adv_sontek_b373h.adr \
   of=adv_sontek bs=40000 count=1
```



# 3. Argo

```
cp ~/git/oce/create_data/argo/6900388_prof.nc .
mv ~/Dropbox/BR5904179_001.nc . # from a server; this is a BIOargo with new fields
```


# 4. AMSR satellite

```
cp ~/data/amsr/f34_20160808v7.2.gz .
```


# 5. Bremen

```
cp /data/flemishCap/msm27_ladcp/msm27_003.ladcp .
```


# 6. cm

```
cp /data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab .
```

# 7. CTD

18HU2010014_00003_00001_ct1.csv is from https://cchdo.ucsd.edu/data/9837/18HU20100513_ct1.zip


```
curl https://www.nodc.noaa.gov/woce/woce_v3/wocedata_1/whp/exchange/example_ct1.csv > example_ct1.csv
```


# Echosounder

```
cp /data/archive/sleiwex/2008/fielddata/2008-07-04/Merlu/Biosonics/20080704_112452.dt4 echosounder.dt4
```

# 8. GPS

```
cp ~/src/rgdal/branches/rgdal_iconv/inst/vectors/test_trk2.gpx .
```

# 9. Ice-tethered profiler
The original source was ftp://ftp.whoi.edu/whoinet/itpdata/itp99grddata.zip
```
cp  ~/Downloads/itp99grddata/itp99grd0000.dat  .
```

# 10. Index

```
curl http://www.cgd.ucar.edu/cas/catalog/climind/SOI.signal.ascii > SOI.signal.ascii
```

# 11. Lobo

```
cp ../../../create_data/lobo/lobo.dat  .
```

