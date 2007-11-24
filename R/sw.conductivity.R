sw.conductivity <- function (S, t, p)
{
	return(0.57057 * (1 + t * (0.003 - 1.025e-05 * t) + 0.000653 * 
      p - 0.00029 * S))
}
