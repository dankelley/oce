# Print an overview of the contents of a NetCDF file

This prints an overview of the variables in a NetCDF file, along with
time, longitude and latitude, if the last three are stored as
dimensions.

## Usage

``` r
netcdfTOC(file, level = 1L, debug = getOption("oceDebug"))
```

## Arguments

- file:

  character value naming a NetCDF file.

- level:

  integer indicating the level of the overview. If `level` is 1, which
  is the default, then a list holding the names of variables and
  dimensions is printed (and returned, silently). If `level` is 2, then
  more information is printed.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Value

If `level` is 1, then the printed list of variables and dimensions is
returned. Otherwise, more information is printed, but the return value
is the same as for `level` 1.

## Details

An alternative to this is to examine what is printed by running
[`ncdf4::nc_open()`](https://rdrr.io/pkg/ncdf4/man/nc_open.html) with
the given filename, but this output can be a bit confusing to read,
especially for files that have voluminous global attributes.

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Use an Argo file that comes with the package.
if (requireNamespace("ncdf4") && requireNamespace("jsonlite")) {
    file <- system.file("extdata/D4902337_219.nc", package = "oce")
    netcdfTOC(file)
}
#> $variables
#>  [1] "DATA_TYPE"                    "FORMAT_VERSION"              
#>  [3] "HANDBOOK_VERSION"             "REFERENCE_DATE_TIME"         
#>  [5] "DATE_CREATION"                "DATE_UPDATE"                 
#>  [7] "PLATFORM_NUMBER"              "PROJECT_NAME"                
#>  [9] "PI_NAME"                      "STATION_PARAMETERS"          
#> [11] "CYCLE_NUMBER"                 "DIRECTION"                   
#> [13] "DATA_CENTRE"                  "DC_REFERENCE"                
#> [15] "DATA_STATE_INDICATOR"         "DATA_MODE"                   
#> [17] "PLATFORM_TYPE"                "FLOAT_SERIAL_NO"             
#> [19] "FIRMWARE_VERSION"             "WMO_INST_TYPE"               
#> [21] "JULD"                         "JULD_QC"                     
#> [23] "JULD_LOCATION"                "LATITUDE"                    
#> [25] "LONGITUDE"                    "POSITION_QC"                 
#> [27] "POSITIONING_SYSTEM"           "VERTICAL_SAMPLING_SCHEME"    
#> [29] "CONFIG_MISSION_NUMBER"        "PROFILE_PRES_QC"             
#> [31] "PROFILE_TEMP_QC"              "PROFILE_PSAL_QC"             
#> [33] "PRES"                         "PRES_QC"                     
#> [35] "PRES_ADJUSTED"                "PRES_ADJUSTED_QC"            
#> [37] "PRES_ADJUSTED_ERROR"          "TEMP"                        
#> [39] "TEMP_QC"                      "TEMP_ADJUSTED"               
#> [41] "TEMP_ADJUSTED_QC"             "TEMP_ADJUSTED_ERROR"         
#> [43] "PSAL"                         "PSAL_QC"                     
#> [45] "PSAL_ADJUSTED"                "PSAL_ADJUSTED_QC"            
#> [47] "PSAL_ADJUSTED_ERROR"          "PARAMETER"                   
#> [49] "SCIENTIFIC_CALIB_EQUATION"    "SCIENTIFIC_CALIB_COEFFICIENT"
#> [51] "SCIENTIFIC_CALIB_COMMENT"     "SCIENTIFIC_CALIB_DATE"       
#> [53] "HISTORY_INSTITUTION"          "HISTORY_STEP"                
#> [55] "HISTORY_SOFTWARE"             "HISTORY_SOFTWARE_RELEASE"    
#> [57] "HISTORY_REFERENCE"            "HISTORY_DATE"                
#> [59] "HISTORY_ACTION"               "HISTORY_PARAMETER"           
#> [61] "HISTORY_START_PRES"           "HISTORY_STOP_PRES"           
#> [63] "HISTORY_PREVIOUS_VALUE"       "HISTORY_QCTEST"              
#> 
#> $dimensions
#>  [1] "DATE_TIME" "STRING256" "STRING64"  "STRING32"  "STRING16"  "STRING8"  
#>  [7] "STRING4"   "STRING2"   "N_PROF"    "N_PARAM"   "N_LEVELS"  "N_CALIB"  
#> [13] "N_HISTORY"
#> 
```
