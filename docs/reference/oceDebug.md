# Print a Debugging Message

Print an indented debugging message. Many oce functions decrease the
`debug` level by 1 when they call other functions, so the effect is a
nesting, with more space for deeper function level.

## Usage

``` r
oceDebug(debug = 0, ..., unindent = 0, sep = "", style = "plain")
```

## Arguments

- debug:

  an integer, less than or equal to zero for no message, and greater
  than zero for increasing levels of debugging. Values greater than 4
  are treated like 4.

- ...:

  items to be supplied to [`cat()`](https://rdrr.io/r/base/cat.html),
  which does the printing. Note that no newline will be printed unless
  ... contains a string with a newline character (as in the example).

- unindent:

  integer giving the number of levels to un-indent, e.g. for start and
  end lines from a called function.

- sep:

  character to insert between elements of `...`, by passing it to
  [`cat()`](https://rdrr.io/r/base/cat.html).

- style:

  either a string or a function. If a string, it must be `"plain"` (the
  default) for plain text, `"bold"`, `"italic"`, `"red"`, `"green"` or
  `"blue"` (with obvious meanings). Note that none of these has any
  effect for non-interactive use, because doing so would make it
  difficult to work with R-markdown and similar documents that are to be
  run through latex.

  If `style` is a function, it must prepend and postpend the text with
  control codes, as in the cyan-coloured example; note that
  [crayon](https://CRAN.R-project.org/package=crayon) provides many
  functions that work well for `style`.

## Author

Dan Kelley

## Examples

``` r
oceDebug(debug = 1, "Example", 1, "Plain text")
#>         Example1Plain text
oceDebug(debug = 1, "Example", 2, "Bold", style = "bold")
#>         Example2Bold
oceDebug(debug = 1, "Example", 3, "Italic", style = "italic")
#>         Example3Italic
oceDebug(debug = 1, "Example", 4, "Red", style = "red")
#>         Example4Red
oceDebug(debug = 1, "Example", 5, "Green", style = "green")
#>         Example5Green
oceDebug(debug = 1, "Example", 6, "Blue", style = "blue")
#>         Example6Blue
mycyan <- function(...) paste("\033[36m", paste(..., sep = " "), "\033[0m", sep = "")
oceDebug(debug = 1, "Example", 7, "User-set cyan", style = mycyan)
#>         Example 7 User-set cyan
```
