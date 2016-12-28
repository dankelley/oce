library(oce)
context("cm dataset")
test_that("tab-delimited (may be specific to Dalhousie)", {
          if (1 == length(list.files(path=".", pattern="local_data"))) {
              cm <- read.oce("local_data/cm_interocean_0811786.s4a.tab")
              summary(cm)
              plot(cm)
              expect_equal(cm[['u']][100:110],
                           c(0.634, 0.648, -1.098, -1.338, -1.394, -1.386,
                             -1.396, -1.388, -1.396, -1.390, -1.396))
          }
})
