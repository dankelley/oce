sw.alpha <- function(S, t, p, is.theta = FALSE)
{
	oce::sw.alpha.over.beta(S, t, p, is.theta) * oce::sw.beta(S, t, p, is.theta)
}
