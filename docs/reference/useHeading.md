# Replace the Heading for One Instrument With That of Another

Replace the heading angles in one oce object with that from another,
possibly with a constant adjustment.

## Usage

``` r
useHeading(b, g, add = 0)
```

## Arguments

- b:

  object holding data from an instrument whose heading is bad, but whose
  other data are good.

- g:

  object holding data from an instrument whose heading is good, and
  should be interpolated to the time base of `b`.

- add:

  an angle, in degrees, to be added to the heading.

## Value

A copy of `b`, but with `b$data$heading` replaced with heading angles
that result from linear interpolation of the headings in `g`, and then
adding the angle `add`.

## Author

Dan Kelley
