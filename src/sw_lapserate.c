#include <R.h>
void
sw_lapserate(int *n, double *pS, double *pT, double *pp, double *value)
{
	/* Fofonoff & Millard (1983 UNESCO) section 7, equation 31*/
	static double a[4] = {
		3.5803e-5, 8.5258e-6, -6.8360e-8, 6.6228e-10
	};
	static double b[2] = {
		1.8932e-6, -4.2393e-8
	};
	static double c[4] = {
		1.8741e-8, -6.7795e-10, 8.7330e-12, -5.4481e-14
	};
	static double d[2] = {
		 -1.1351e-10, 2.7759e-12
	};
	static double e[3] = {
		-4.6206e-13, 1.8676e-14, -2.1687e-16
	};
	int i;
	for (i = 0; i < *n; i++) {
		double S = *pS++;
		double T = *pT++;
		double p = *pp++;
		if (ISNA(S) || ISNA(T) || ISNA(p)) {
			*value++ = NA_REAL;
		} else {
			double lf = a[0] + T * (a[1] + T * (a[2] + T * a[3]))
				+ (b[0] + b[1] * T) * (S - 35.0)
				+ (c[0] + T * (c[1] + T * (c[2] + T * c[3]))
					+ (d[0] + T * d[1]) * (S - 35.0)) * p
				+ (e[0] + T * (e[1] + T * e[2])) * p * p;
			*value++ = lf;
		}
	}
}
