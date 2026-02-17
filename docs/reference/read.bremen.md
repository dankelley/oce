# Read a bremen File

Read a file in Bremen format.

## Usage

``` r
read.bremen(file, encoding = "latin1")
```

## Arguments

- file:

  a connection or a character string giving the name of the file to
  load.

- encoding:

  a character value that indicates the encoding to be used for this data
  file, if it is textual. The default value for most functions is
  `"latin1"`, which seems to be suitable for files containing text
  written in English and French.

## Value

A [bremen](https://dankelley.github.io/oce/reference/bremen-class.md)
object.

## Details

Velocities are assumed to be in cm/s, and are converted to m/s to follow
the oce convention. Shears (which is what the variables named `uz` and
`vz` are assumed to represent) are assumed to be in (cm/s)/m, although
they could be in 1/s or something else; the lack of documentation is a
problem here. Also, note that the assumed shears are not just
first-difference estimates of velocity, given the results of a sample
dataset:

    > head(data.frame(b[["data"]]))
      pressure      u      v       uz       vz
    1        0  0.092 -0.191  0.00000  0.00000
    2       10  0.092 -0.191  0.02183 -0.35412
    3       20  0.092 -0.191  0.03046 -0.09458
    4       30  0.026 -0.246 -0.03948  0.02169
    5       40 -0.003 -0.212 -0.02614  0.03111
    6       50 -0.023 -0.169 -0.03791  0.01706

## Issues

This function may be renamed (or removed) without notice. It was created
to read some data being used in a particular research project, and will
be rendered useless if Bremen changes this data format.

## See also

Other things related to bremen data:
[`[[,bremen-method`](https://dankelley.github.io/oce/reference/sub-sub-bremen-method.md),
`[[<-,bremen-method`,
[`bremen-class`](https://dankelley.github.io/oce/reference/bremen-class.md),
[`plot,bremen-method`](https://dankelley.github.io/oce/reference/plot-bremen-method.md),
[`summary,bremen-method`](https://dankelley.github.io/oce/reference/summary-bremen-method.md)

## Author

Dan Kelley
