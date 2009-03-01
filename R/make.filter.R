make.filter <- function(type=c("blackman-harris"), m)
{
    type <- match.arg(type)
    if (type == "blackman-harris") {
        if (missing(m)) stop("must supply 'm'")
        ## See Harris (1978)
        if (m == (2 * floor(m/2))) m <- m + 1 # make odd
        n <- seq(0, m - 1)
        a <- c(0.35875, 0.488829, 0.14128, 0.01168)
        ff <- pi * n / (m - 1)
        coef <- a[1] - a[2]*cos(2*ff) + a[3]*cos(4*ff) - a[4]*cos(6*ff)
        coef / sum(coef)                # make unit sum
    }
}
