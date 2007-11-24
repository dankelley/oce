#include <R.h>
#include <Rdefines.h>

void
sw_rho(int *n, double *pS, double *pT, double *pp, double *value)
{
	int i;
	for (i = 0; i < *n; i++) {
		double S = *pS++;
		double T = *pT++;
		double p = *pp++;
		double rho_w, Kw, Aw, Bw, p1, S12, ro, xkst;

/*
		printf("%f %f %f\n",S,T,p);
		printf("missing? %d %d %d\n",ISNA(S),ISNA(T),ISNA(p));
*/

		if (ISNA(S) || ISNA(T) || ISNA(p)) {
			*value++ = NA_REAL;
		} else {
			rho_w = 999.842594 +
				T * (6.793952e-2 +
				     T * (-9.095290e-3 +
					  T * (1.001685e-4 +
					       T * (-1.120083e-6 + T * 6.536332e-9))));
			Kw = 19652.21
				+ T * (148.4206 +
				       T * (-2.327105 +
					    T * (1.360477e-2 - T * 5.155288e-5)));
			Aw = 3.239908 +
				T * (1.43713e-3 +
				     T * (1.16092e-4 -
					  T * 5.77905e-7));
			Bw = 8.50935e-5 +
				T * (-6.12293e-6 +
				     T * 5.2787e-8);
			p1 = 0.1 * p;
			S12 = sqrt(S);
			ro = rho_w +
				S * (8.24493e-1 +
				     T * (-4.0899e-3 +
					  T * (7.6438e-5 +
					       T * (-8.2467e-7 + T * 5.3875e-9))) +
				     S12 * (-5.72466e-3 +
					    T * (1.0227e-4 -
						 T * 1.6546e-6) +
					    S12 * 4.8314e-4));
			xkst = Kw +
				S * (54.6746 +
				     T * (-0.603459 +
					  T * (1.09987e-2 -
					       T * 6.1670e-5)) +
				     S12 * (7.944e-2 +
					    T * (1.6483e-2 +
						 T * (-5.3009e-4)))) +
				p1 * (Aw +
				      S * (2.2838e-3 +
					   T * (-1.0981e-5 +
						T * (-1.6078e-6)) +
					   S12 * (1.91075e-4)) +
				      p1 * (Bw +
					    S * (-9.9348e-7 +
						 T * (2.0816e-8 +
						      T * (9.1697e-10)))));
			*value++ = (ro / (1.0 - p1 / xkst));
		}
	}
}
