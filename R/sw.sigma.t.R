sw.sigma.t <- function(S, t, p)
{
  	p.top <- rep(0, length(S))
  	oce::sw.rho(S, t, p.top) - 1000
}
