# Coriolis Parameter on the Earth

Compute \\f\\, the Coriolis parameter as a function of latitude (see
reference 1), assuming earth siderial angular rotation rate
\\omega\\=7292115e-11 rad/s. See reference 1 for general notes, and see
reference 2 for comments on temporal variations of \\omega\\.

## Usage

``` r
coriolis(latitude, degrees = TRUE)
```

## Arguments

- latitude:

  Vector of latitudes in \\^\circ\\N or radians north of the equator.

- degrees:

  Flag indicating whether degrees are used for latitude; if set to
  `FALSE`, radians are used.

## Value

Coriolis parameter, in radian/s.

## References

1.  Gill, A.E., 1982. *Atmosphere-ocean Dynamics*, Academic Press, New
    York, 662 pp.

2.  Groten, E., 2004: Fundamental Parameters and Current, 2004. Best
    Estimates of the Parameters of Common Relevance to Astronomy,
    Geodesy, and Geodynamics. Journal of Geodesy, 77:724-797.
    (downloaded from
    `http://www.iag-aig.org/attach/e354a3264d1e420ea0a9920fe762f2a0/51-groten.pdf`
    March 11, 2017).

## Author

Dan Kelley

## Examples

``` r
C <- coriolis(45) # 1e-4
```
