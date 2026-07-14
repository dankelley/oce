# Draw a Stick Plot

The arrows are drawn with directions on the graph that match the
directions indicated by the `u` and `v` components. The arrow size is
set relative to the units of the `y` axis, according to the value of
`yscale`, which has the unit of `v` divided by the unit of `y`. The
interpretation of diagrams produced by `plotSticks` can be difficult,
owing to overlap in the arrows. For this reason, it is often a good idea
to smooth `u` and `v` before using this function.

## Usage

``` r
plotSticks(
  x,
  y,
  u,
  v,
  yscale = 1,
  add = FALSE,
  length = 1/20,
  mgp = getOption("oceMgp"),
  mar = c(mgp[1] + 1, mgp[1] + 1, 1, 1 + par("cex")),
  xlab = "",
  ylab = "",
  col = 1,
  ...
)
```

## Arguments

- x:

  x coordinates of stick origins.

- y:

  y coordinates of stick origins. If not supplied, 0 will be used; if
  length is less than that of x, the first number is repeated and the
  rest are ignored.

- u:

  x component of stick length.

- v:

  y component of stick length.

- yscale:

  scale from u and v to y (see “Description”).

- add:

  boolean, set `TRUE` to add to an existing plot.

- length:

  value to be provided to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html); here, we set a
  default that is smaller than normally used, because these plots tend
  to be crowded in oceanographic applications.

- mgp:

  3-element numerical vector to use for
  [`par`](https://rdrr.io/r/graphics/par.html)`("mgp")`. Note that the
  default `mar` is computed from the `mgp` value. The default is tighter
  than the R default, in order to use more space for the data and less
  for the axes.

- mar:

  value to be used with
  [`par`](https://rdrr.io/r/graphics/par.html)`("mar")`.

- xlab, ylab:

  labels for the plot axes. The default is not to label them.

- col:

  color of sticks, in either numerical or character format. This is made
  to have length matching that of `x` by a call to
  [`rep()`](https://rdrr.io/r/base/rep.html), which can be handy in e.g.
  colorizing a velocity field by day.

- ...:

  graphical parameters passed down to
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html). It is common,
  for example, to use smaller arrow heads than
  [`arrows()`](https://rdrr.io/r/graphics/arrows.html) uses; see
  “Examples”.

## Author

Dan Kelley

## Examples

``` r
library(oce)

# Flow from a point source
n <- 16
x <- rep(0, n)
y <- rep(0, n)
theta <- seq(0, 2 * pi, length.out = n)
u <- sin(theta)
v <- cos(theta)
plotSticks(x, y, u, v, xlim = c(-2, 2), ylim = c(-2, 2))

rm(n, x, y, theta, u, v)

# Oceanographic example
data(met)
t <- met[["time"]]
u <- met[["u"]]
v <- met[["v"]]
p <- met[["pressure"]]
oce.plot.ts(t, p)
plotSticks(t, 99, u, v, yscale = 25, add = TRUE)
```
