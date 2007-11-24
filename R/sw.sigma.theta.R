sw.sigma.theta <- function(S, t, p)
{
  	p.top <- rep(0, length(S))
  	oce::sw.rho(S, oce::sw.theta(S, t, p), p.top) - 1000
}
