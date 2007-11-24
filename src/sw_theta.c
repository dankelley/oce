#include <R.h>
#include <Rdefines.h>

void 
theta_Bryden_1973(int *n, double *pS, double *pT, double *pp, double *value)
{
	/* Source: Bryden 1973 */
	int i;
	for (i = 0; i < *n; i++) {
		double S = *pS++;
		double T = *pT++;
		double p = *pp++;
		if (ISNA(S) || ISNA(T) || ISNA(p)) {
			*value++ = NA_REAL;
		} else {
			S -= 35.0;
			p /= 10.0; /* formula is in bars, but argument is in decibars! */
			*value++ = T
				- p * (((3.6504e-4 + T * (8.3198e-5 + T * (-5.4065e-7 + T * 4.0274e-9)))
					+ S * (1.7439e-5 - T * 2.9778e-7))
		       			+ p * ((8.9309e-7 + T * (-3.1628e-8 + T * 2.1987e-10) - S * 4.1057e-9)
			      		+ p * (-1.6056e-10 + T * 5.0484e-12)));
		}
	}
}
double
atg_UNESCO_1983(double S, double T, double p)
{
	/* Adiabatic temperature gradient, UNESCO 1983
	 *
     * Usage: atg_UNESCO_1983(S, T, [P])
	 *
     * Input:
     *   S = Salinity,     [PSS-78]
     *   T = Temperature,  [°C]
     *   p = Pressure,     [dbar]
     *
     * Output:
     *   Adiabatic temperature gradient,  [K/dbar]
     *
     * Check value:
	 * ATG=3.255976e-4 C/dbar for S=40, T=40degC, p=10000dbar
	 */
	S -= 35.0;
    return(3.5803e-5 + (8.5258e-6 + (-6.836e-8 + 6.6228e-10*T)*T)*T
		+ (1.8932e-6 - 4.2393e-8*T)*S  
	 	+ ((1.8741e-8 + (-6.7795e-10 + (8.733e-12 - 5.4481e-14*T)*T)*T)
        + (-1.1351e-10 + 2.7759e-12*T)*S)*p
        + (-4.6206e-13 + (1.8676e-14 - 2.1687e-16*T)*T)*p*p);
}

void
theta_UNESCO_1983(int *n, double *pS, double *pT, double *pp, double *ppref, double *value)
{
	/* Source: UNESCO 1983
	 * check value from Fofonoff et al. (1983)
	 * theta = 36.89073C at S=40, T=40, p=10000, pref=0
	 */
	int i;
	for (i = 0; i < *n; i++) {
		double S = *pS++;
		double T = *pT++;
		double p = *pp++;
		double pref = *ppref++;
		if (ISNA(S) || ISNA(T) || ISNA(p) || ISNA(pref)) {
			*value++ = NA_REAL;
		} else {
			double H, XK, Q;
			H = pref - p;
			XK = H * atg_UNESCO_1983(S,T,p);
			T = T + 0.5 * XK;
			Q = XK;
			p = p + 0.5 * H;
			XK = H * atg_UNESCO_1983(S,T,p);
			T = T + 0.29289322 * (XK - Q);
			Q = 0.58578644 * XK + 0.121320344 * Q;
			XK = H * atg_UNESCO_1983(S,T,p);
			T = T + 1.707106781 * (XK - Q);
			Q = 3.414213562 * XK - 4.121320344 * Q;
			p = p + 0.5 * H;
			XK = H * atg_UNESCO_1983(S,T,p);
			*value++ = T + (XK - 2.0 * Q) / 6.0;
		}
	}
}
