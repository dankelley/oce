# Rename variables according to a specified dictionary

There are many conventions for naming oceanographic variables, and this
function provides a way to map names in data files to names to be used
in an object created from those files.

## Usage

``` r
oceRename(x, dictionary = "ioos", debug = 0)
```

## Arguments

- x:

  either an
  [oce](https://dankelley.github.io/oce/reference/oce-class.md) object,
  the elements of which will be renamed, or NULL. In the latter case,
  the dictionary is returned as a data frame, which can be useful for
  users who want to use [`rbind()`](https://rdrr.io/r/base/cbind.html)
  to append dictionary elements of their own, thus customizing the
  action of `oceRename()`.

- dictionary:

  either a string or a data frame. If a string, then it is either the
  name of a built-in vocabulary (either `"ioos"` or `"sbe"`) or the name
  of a CSV file that defines a dictionary in a four-column format as
  described in ‘Details’. If it is a data frame, then it must hold four
  columns that follow the same pattern as in the CSV style.

- debug:

  an integer specifying whether debugging information is to be printed
  during the processing. This is a general parameter that is used by
  many `oce` functions. Generally, setting `debug=0` turns off the
  printing, while higher values suggest that more information be
  printed. If one function calls another, it usually reduces the value
  of `debug` first, so that a user can often obtain deeper debugging by
  specifying higher `debug` values.

## Details

The dictionary format, whether read from a built-in CSV file, or from a
user-supplied CSV file, or as a data frame, contains four
character-valued columns, as follows.

1.  The original name of a variable in the `data` slot of `x`. This is
    used in matching such names against targets. Matches may be in the
    form of equality, or [regexp](https://rdrr.io/r/base/regex.html)
    match. In the latter case, a `#` character may be used as an
    abbreviation for a digit. Note that `^` is inserted at the start of
    the value, and `$` at the end, before searching for a match with
    [`grep()`](https://rdrr.io/r/base/grep.html).

2.  The desired oce-convention name to be used for a match. Many files
    will yield duplicates, e.g. for multiple temperature sensors, so
    [`unduplicateNames()`](https://dankelley.github.io/oce/reference/unduplicateNames.md)
    is called after all names are processed, to avoid problems.

3.  The unit for the column, typically in a format handled by
    [`expression()`](https://rdrr.io/r/base/expression.html). Note that
    this value is ignored if the object already holds stated units for
    the quantity in question.

4.  The scale for the column (again, only used if the object does not
    already hold a scale).

The built-in dictionaries are stored in locations

    system.file("extdata", "dictionary_codas.csv", package = "oce")
    system.file("extdata", "dictionary_ioos.csv", package = "oce")
    system.file("extdata", "dictionary_sbe.csv", package = "oce")

The data for these come from References 1, 2 and 3, respectively. The
format is simple, consisting of 4 columns, with no header. The column
entries are as follows.

1.  The first column holds a specialized regular expression for the
    variable name as stored in the datafile. This is conventional,
    except that `#` is a stand-in for the regular expression `[0-9]`
    (that is, a single digit). Formulating these expressions requires a
    bit of care, so it can make sense to look at the
    `dictionary_sbe.csv` file to get some hints.

2.  The second column holds the oce name.

3.  The third column is the unit.

4.  The fourth column is the scale.

In many cases, the third and fourth columns are empty, and even if
values are provided, they will be superseded by values within the data
file.

As an example, the entry

    PSALST##,salinity,,PSS-78

indicates that a variable named `"PSALT"` followed by 2 digits is to be
renamed as `"salinity"`, that the unit (if not already defined within
`x`) is to be blank, and that the scale (again, if not already defined
within `x`) is to be `"PSS-78"`.

## History and Plans

This function was written in late September, 2024. It is likely to
evolve through the remaining months of 2024, after real-world testing by
the developers.

## References

1.  CODAS naming convention
    <https://currents.soest.hawaii.edu/docs/adcp_doc/UHDAS_OPERATIONS/UHDAS_atsea/adcp_access/read_netCDF.html>

2.  IOOS naming convention
    <https://cfconventions.org/Data/cf-standard-names/78/build/cf-standard-name-table.html>

3.  The SBE names come from a processing manual that was once at
    `http://www.seabird.com/document/sbe-data-processing-manual`, but as
    of summer 2018, this no longer seems to be provided by SeaBird. A
    web search will turn up copies of the manual that have been put
    online by various research groups and data-archiving agencies. On
    2018-07-05, the latest version was named
    `SBEDataProcessing_7.26.4.pdf` and had release date 12/08/2017; this
    was the reference version used in coding `oce`.

## Author

Dan Kelley

## Examples

``` r
library(oce)
# Example 1: made-up data
d <- new("oce")
d <- oceSetData(d, "S", c(30, 31))
d <- oceSetData(d, "T", c(10, 11))
dictText <- "S,salinity,,
T,temperature,degree*C,ITS-90"
dictionary <- read.csv(text = dictText, header = FALSE)
oceRename(d, dictionary)
#> oce object has data as follows.
#>    salinity[1:2]: 30, 31
#>    temperature[1:2]: 10, 11
#
# Example 2: a CIOOS NetCDF file. Note that this file
# is downloaded and removed at the end; in practice,
# it is likely that the file might be retained locally.
if (requireNamespace("curl")) {
    file <- tempfile(fileext = ".nc") # removed later
    server <- "https://cioosatlantic.ca/erddap/files"
    program <- "bio_atlantic_zone_monitoring_program_ctd"
    subprogram <- "Bedford%20Basin%20Monitoring%20Program"
    year <- 2023
    cast <- 1
    url <- sprintf(
        "%s/%s/%s/%s/CTD_BCD%s667_%03d_1_DN.ODF.nc",
        server, program, subprogram, year, year, cast
    )
    t <- try(curl::curl_download(url, file), silent = TRUE)
    if (!inherits(t, "try-error")) {
        d <- read.netcdf(file)
        summary(d)
        dd <- oceRename(d, "ioos")
        summary(dd)
    } else {
        message("Cannot connect to ", url)
    }
    unlink(file)
}
#> * Time: 2023-01-05 13:14:45
#> * Data Overview
#> 
#>                               Min.       Mean       Max.       Dim. NAs OriginalName      
#>     measurement_time          1672924837 1672924922 1672925007 141  0   "measurement_time"
#>     ScanNumber                5634       6989.8     8352       141  0   "ScanNumber"      
#>     QCNTR_01                  1          1          1          141  0   "QCNTR_01"        
#>     PRESPR01 [dbar]           0.5        35.5       70.5       141  0   "PRESPR01"        
#>     QPRES_01                  1          1          1          141  0   "QPRES_01"        
#>     TEMPS901 [°C, ITS-90]     5.6177     7.5868     7.8938     141  0   "TEMPS901"        
#>     TEMPP901 [°C, ITS-90]     5.6177     7.5868     7.8938     141  0   "TEMPP901"        
#>     TEMPPR01 [°C, ITS-90]     5.6177     7.5868     7.8938     141  0   "TEMPPR01"        
#>     QTEMP_01                  1          1          1          141  0   "QTEMP_01"        
#>     CNDCST01 [S/m]            2.8711     3.2306     3.2974     141  0   "CNDCST01"        
#>     QCNDC_01                  1          1          1          141  0   "QCNDC_01"        
#>     OXYOCPVL01 [V]            0.816      2.2953     3.07       141  0   "OXYOCPVL01"      
#>     QOXYV_01                  1          1          1          141  0   "QOXYV_01"        
#>     CPHLPR01 [mg/m³]          1.1592     1.6785     4.3245     141  0   "CPHLPR01"        
#>     QCPHLPR01                 1          1          1          141  0   "QCPHLPR01"       
#>     PHXXZZ01                  8.187      8.281      8.784      141  0   "PHXXZZ01"        
#>     QPHPH_01                  1          1          1          141  0   "QPHPH_01"        
#>     IRRDUV01 [μEinstein/s/m²] 0.00025    3.1998     69.881     141  5   "IRRDUV01"        
#>     QPSAR_01                  1          1.2837     9          141  0   "QPSAR_01"        
#>     PSALST01 [PSS-78]         29.011     31.178     31.624     141  0   "PSALST01"        
#>     PSLTZZ01 [PSS-78]         29.011     31.178     31.624     141  0   "PSLTZZ01"        
#>     QPSAL_01                  1          1          1          141  0   "QPSAL_01"        
#>     POTMCV01 [°C, ITS-90]     5.6177     7.5835     7.888      141  0   "POTMCV01"        
#>     QPOTM_01                  1          1          1          141  0   "QPOTM_01"        
#>     SIGTEQ01 [kg/m³]          22.866     24.329     24.644     141  0   "SIGTEQ01"        
#>     QSIGP_01                  1          1          1          141  0   "QSIGP_01"        
#>     DOXYZZ01 [ml/l]           0.7969     4.5753     7.178      141  0   "DOXYZZ01"        
#>     QDOXY_01                  1          1          1          141  0   "QDOXY_01"        
#>     RecPerBin                 11         18.27      110        141  0   "RecPerBin"       
#>     QCNTR_02                  1          1          1          141  0   "QCNTR_02"        
#>     QCFF_01                   0          0          0          141  0   "QCFF_01"         
#>     time                      1672924485 1672924485 1672924485 1    0   "time"            
#>     latitude [°N]             44.683     44.683     44.683     1    0   "latitude"        
#>     longitude [°E]            -63.633    -63.633    -63.633    1    0   "longitude"       
#> 
#> * Processing Log
#> 
#>     - 2026-02-17 20:57:34 UTC: `Create oce object`
#>     - 2026-02-17 20:57:34 UTC: `read.netcdf("/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T//RtmpDk8HXN/file32e77d0a1f46.nc")`
#> * Time: 2023-01-05 13:14:45
#> * Data Overview
#> 
#>                               Min.       Mean       Max.       Dim. NAs OriginalName      
#>     timeMeasurement           1672924837 1672924922 1672925007 141  0   "measurement_time"
#>     scan                      5634       6989.8     8352       141  0   "ScanNumber"      
#>     pressure [dbar]           0.5        35.5       70.5       141  0   "PRESPR01"        
#>     temperature [°C, ITS-90]  5.6177     7.5868     7.8938     141  0   "TEMPS901"        
#>     temperature2 [°C, ITS-90] 5.6177     7.5868     7.8938     141  0   "TEMPP901"        
#>     temperature3 [°C, ITS-90] 5.6177     7.5868     7.8938     141  0   "TEMPPR01"        
#>     conductivity [S/m]        2.8711     3.2306     3.2974     141  0   "CNDCST01"        
#>     oxygenVoltage [V]         0.816      2.2953     3.07       141  0   "OXYOCPVL01"      
#>     chlorophyllA [mg/m³]      1.1592     1.6785     4.3245     141  0   "CPHLPR01"        
#>     pH                        8.187      8.281      8.784      141  0   "PHXXZZ01"        
#>     PAR [μEinstein/s/m²]      0.00025    3.1998     69.881     141  5   "IRRDUV01"        
#>     salinity [PSS-78]         29.011     31.178     31.624     141  0   "PSALST01"        
#>     salinity2 [PSS-78]        29.011     31.178     31.624     141  0   "PSLTZZ01"        
#>     theta [°C, ITS-90]        5.6177     7.5835     7.888      141  0   "POTMCV01"        
#>     sigmaTheta [kg/m³]        22.866     24.329     24.644     141  0   "SIGTEQ01"        
#>     oxygen [ml/l]             0.7969     4.5753     7.178      141  0   "DOXYZZ01"        
#>     recordsPerBin             11         18.27      110        141  0   "RecPerBin"       
#>     quality                   0          0          0          141  0   "QCFF_01"         
#>     time                      1672924485 1672924485 1672924485 1    0   "time"            
#>     latitude [°N]             44.683     44.683     44.683     1    0   "latitude"        
#>     longitude [°E]            -63.633    -63.633    -63.633    1    0   "longitude"       
#> 
#> * Data-quality Flags
#> 
#>     scan:          "1" 141
#>     pressure:      "1" 141
#>     temperature:   "1" 141
#>     conductivity:  "1" 141
#>     oxygenVoltage: "1" 141
#>     chlorophyllA:  "1" 141
#>     pH:            "1" 141
#>     PAR:           "1" 136, "9" 5
#>     salinity:      "1" 141
#>     theta:         "1" 141
#>     sigmaTheta:    "1" 141
#>     oxygen:        "1" 141
#> 
#> * Processing Log
#> 
#>     - 2026-02-17 20:57:34 UTC: `Create oce object`
#>     - 2026-02-17 20:57:34 UTC: `read.netcdf("/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T//RtmpDk8HXN/file32e77d0a1f46.nc")`
#>     - 2026-02-17 20:57:34 UTC: `oceRename(..., "/private/var/folders/8b/l4h64m1j22v5pb7vj049ff140000gn/T/RtmpdNg0iG/temp_libpath32578b0e13b/oce/extdata/dictionary_ioos.csv")`
```
