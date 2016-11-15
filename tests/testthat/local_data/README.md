Files created as follows.

```

# ADP: RDI, Nortek, and Sontek

dd if=/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000 of=rdi bs=40000 count=1

dd if=/data/archive/sleiwex/2008/moorings/m05/adp/nortek_aqd2843/raw/adp_nortek_aqd2843.prf of=adp_nortek_aquadopp bs=40000 count=1

dd if=/data/archive/sleiwex/2008/moorings/m07/adp/sontek_h53/raw/adp_sontek_h53.adp of=adp_sontek bs=40000 count=1

# ADV: Nortek and Sontek

dd if=/data/archive/sleiwex/2008/moorings/m05/adv/nortek_1943/raw/adv_nortek_1943.vec of=adv_nortek_vector bs=40000 count=1

dd if=/data/archive/sleiwex/2008/moorings/m03/adv/sontek_b373h/raw/adv_sontek_b373h.adr of=adv_sontek bs=40000 count=1

# AMSR satellite

cp ~/data/amsr/f34_20160808v7.2.gz .

# Echosounder

cp /data/archive/sleiwex/2008/fielddata/2008-07-04/Merlu/Biosonics/20080704_112452.dt4 echosounder.dt4

# Lobo

cp ../../../create_data/lobo/lobo.dat  .

# GPS

cp ~/src/rgdal/branches/rgdal_iconv/inst/vectors/test_trk2.gpx .

# Ice-tethered profiler
# ftp://ftp.whoi.edu/whoinet/itpdata/itp99grddata.zip
cp  ~/Downloads/itp99grddata/itp99grd0000.dat  .


# CTD

18HU2010014_00003_00001_ct1.csv is from https://cchdo.ucsd.edu/data/9837/18HU20100513_ct1.zip

```
