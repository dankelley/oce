# Format a Confidence Interval

This formats a confidence interval in either the +/- notation or the
parenthetic notation. For example, if a quantity has mean 1 with
uncertainty 0.05, which means a CI of 0.95 to 1.05, the `"+-"` style
yields `"1+/-0.05"`, and the `"parentheses"` style yields \`""'.

## Usage

``` r
formatCI(
  ci,
  style = c("+/-", "parentheses"),
  model,
  digits = 2,
  debug = getOption("oceDebug", 0)
)
```

## Arguments

- ci:

  optional vector of length 2 or 3.

- style:

  string indicating notation to be used.

- model:

  optional regression model, e.g. returned by
  [`lm()`](https://rdrr.io/r/stats/lm.html) or
  [`nls()`](https://rdrr.io/r/stats/nls.html).

- digits:

  optional number of digits to use. This is ignored if `style` is
  `"parentheses"`.

- debug:

  integer value indicating debugging level. If 0, then `formatCI()`
  works silently. If greater than 0, then some debugging messages are
  printed during processing.

## Value

If `ci` is given, the result is a character string with the estimate and
its uncertainty, in plus/minus or parenthetic notation. If `model` is
given, the result is a 1-column matrix holding character strings, with
row names corresponding to the parameters of the model.

## Details

If a `model` is given, then `ci` is ignored, and a confidence interval
is calculated using [`confint()`](https://rdrr.io/r/stats/confint.html)
with `level` set to 0.6914619. This `level` corresponds to a range of
plus or minus one standard deviation, for the t distribution and a large
number of degrees of freedom (since `qt(0.6914619, 100000)` is 0.5).

If `model` is missing, `ci` must be provided. If it contains 3 elements,
then first and third elements are taken as the range of the confidence
interval (which by convention should use the `level` stated in the
previous paragraph), and the second element is taken as the central
value. Alternatively, if `ci` has 2 elements, they are taken to be
bounds of the confidence interval and their mean is taken to be the
central value.

In the `+/-` notation, e.g. \\a \pm b\\ indicates that the true value
lies between \\a-b\\ and \\a+b\\ with a high degree of certainty. Mills
et al. (1993, section 4.1 on page 83) suggest that \\b\\ should be set
equal to 2 times the standard uncertainty or standard deviation. JCGM
(2008, section 7.2.2 on pages 25 and 26), however, suggest that \\b\\
should be set to the standard uncertainty, while also recommending that
the \\\pm\\ notation (and presumably the parentheses notation also) be
avoided altogether, in favour of writing sentences that explains
uncertainties in clear terms.

The `parentheses` notation is often called the compact notation. In it,
the digits in parentheses indicate the uncertainty in the corresponding
digits to their left, e.g. 12.34(3) means that the last digit (4) has an
uncertainty of 3. However, as with the \\\pm\\ notation, different
authorities offer different advice on defining this uncertainty; Mills
et al. (1993) provide an example in which the parenthetic value is half
the \\\pm\\ value, whereas JCM (2008) suggest using the same values.

The JCM(2008) convention is used by `formatCI()` for the parentheses
notation, as illustrated in Examples 1 and 2. Note, however, that if the
confidence range exceeds the value, then a request for `parentheses`
format reverts to `+/-` format.

## References

1.  JCGM, 2008. *Evaluation of measurement data - Guide to the
    expression of uncertainty in measurement (JCGM 100:2008)*, published
    by the Joint Committee for Guides in Metrology, available (as of
    November 2023) at
    https://www.bipm.org/documents/20126/2071204/JCGM_100_2008_E.pdf.
    See section 7.2.2 on Page 25, for a summary of notation, including
    an illustration of the use of equal values for both the `+-` and the
    parentheses notations.

2.  Mills, I., T. Cvitas, K. Homann, N. Kallay, and K. Kuchitsu, 1993.
    *Quantities, Units and Symbols in Physical Chemistry*, published
    Blackwell Science for the International Union of Pure and Applied
    Chemistry. (See section 4.1, page 83, for a summary of notation,
    which shows that a value to the right of a `+-` sign is to be halved
    if put in in parentheses, which is not done in the present function,
    because of a choice to follow the recommendation of reference 1.

## Author

Dan Kelley

## Examples

``` r
library(oce)

# Example 1: mean=1, uncertainty=0.05, in +/- notation.
formatCI(c(0.95, 1.05)) # "1+/-0.05"
#> [1] "1+/-0.05"

# Example 2: save mean and uncertainty, but in parentheses notation.
formatCI(c(0.95, 1.05), style = "parentheses") # "1.00(5)"
#> [1] "1.00(5)"

# example 3: using t.test to find a CI.
a <- rnorm(100, mean = 10, sd = 1)
CI <- t.test(a)$conf.int
formatCI(CI)
#> [1] "9.9+/-0.2"
formatCI(CI, style = "parentheses")
#> [1] "9.9(1.99)"

# example 4: specifying a model
x <- seq(0, 10, 0.1)
y <- 2 + 3 * x + rnorm(x, sd = 0.1)
m <- lm(y ~ x)
formatCI(model = m)
#>             value       
#> (Intercept) "2+/-0.02"  
#> x           "3+/-0.0035"
formatCI(model = m, style = "parentheses")
#>             value       
#> (Intercept) "1.99(2.03)"
#> x           "3.002(3.5)"
```
