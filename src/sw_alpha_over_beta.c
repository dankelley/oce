#include <R.h>
#include <Rdefines.h>

void
sw_alpha_over_beta(int *n, double *pS, double *ptheta, double *pp, double *value)
{
	int i;
	for (i = 0; i < *n; i++) {
		double S = *pS++;
		double theta = *ptheta++;
		double p = *pp++;
/*
		printf("%f %f %f\n",S,T,p);
		printf("missing? %d %d %d\n",ISNA(S),ISNA(T),ISNA(p));
*/
		if (ISNA(S) || ISNA(theta) || ISNA(p)) {
			*value++ = NA_REAL;
		} else {
			S -= 35.0;
			*value++ = (0.665157e-1 + theta * (0.170907e-1 + theta * (-0.203814e-3 + theta * (0.298357e-5 + theta * (-0.255019e-7)))))
				+ S * (
				(0.378110e-2 + theta * (-0.846960e-4))
				+ p * (-0.164759e-6 + p * (-0.251520e-11)))
				+ S * S * (-0.678662e-5)
				+ p * (0.380374e-4 + theta * (-0.933746e-6 + theta * (0.791325e-8)))
				+ 0.512857e-12* p * p * theta *theta
				+ -0.302285e-13 * p * p * p;
		}
	}
}
