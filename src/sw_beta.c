#include <R.h>
#include <Rdefines.h>
void
sw_beta(int *n, double *pS, double *ptheta, double *pp, double *value)
{
	int i;
	for (i = 0; i < *n; i++) {
		double S = *pS++;
		double theta = *ptheta++;
		double p = *pp++;
		if (ISNA(S) || ISNA(theta) || ISNA(p)) {
			*value++ = NA_REAL;
		} else {
			S -= 35.0;
			*value++ =
				0.785567e-3 + theta * (-0.301985e-5 + theta * (0.555579e-7 + theta *(-0.415613e-9)))
				+ S * (-0.356603e-6 + 0.788212e-8 * theta + p * (0.408195e-10 + p * (-0.602281e-15)))
				+ S * S * (0.515032e-8)
				+ p * (-0.121555e-7 + theta * (0.192867e-9 + theta * (-0.213127e-11)))
				+ p * p * (0.176621e-12 + theta * (-0.175379e-14))
				+ p * p * p * (0.121551e-17);
		}
	}
}
