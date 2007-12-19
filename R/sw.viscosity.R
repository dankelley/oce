sw.viscosity <- function (S, t=NULL)
{
	if (inherits(S, "ctd")) {
		t <- S$data$temperature
		S <- S$data$salinity # note: this destroys the ctd object
	}
    return(0.001798525 + S * (2.634749e-06 - 7.088328e-10 * t^2 + 
        S * (-4.702342e-09 + S * (5.32178e-11))) + t * (-6.293088e-05 + 
        t * (1.716685e-06 + t * (-3.479273e-08 + t * (+3.566255e-10)))))
}
