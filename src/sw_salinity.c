#include <R.h>
void
sw_salinity(int *n, double *pC, double *pT, double *pp, double *value)
{
	int i;
	static double c[5] = {
		0.6766097, 2.00564e-2, 1.104259e-4, -6.9698e-7, 1.0031e-9
	};
	static double d[4] = {
		3.426e-2, 4.464e-4, 4.215e-1, -3.107e-3
	};
	static double e[4] = {
		2.070e-5, -6.370e-10, 3.989e-15
	};
	static double a[6] = {
		 0.0080, -0.1692, 25.3851, 14.0941,	-7.0261, 2.7081
	};
	static double b[6] = {
		0.0005,	-0.0056, -0.0066, -0.0375, 0.0636, -0.0144
	};
	static double k = 0.0162;
	double rt, Rp, Rt, Rtx, del_T, del_S, S;
	double C, T, p;
	for (i = 0; i < *n; i++) {
		C = *pC++;
		T = *pT++;
		p = *pp++;
		if (ISNA(C) || ISNA(T) || ISNA(p)) {
			*value++ = NA_REAL;
		} else {
			/* Follows the UNESCO formulae of FM83, i.e.
			 * Fofonoff, P. and R. C. Millard Jr, 1983. Algorithms for computation of
		 	 * fundamental properties of seawater. \emph{Unesco Technical Papers in Marine
		 	 * Science}, \bold{44}, 53 pp
			 *
			 * Test values, p9 of FM83:
			 * stopifnot(all.equal.numeric(S.C.T.p(1,   15,   0), 35.000000, 1e-6))
			 * stopifnot(all.equal.numeric(S.C.T.p(1.2, 20,2000), 37.245628, 1e-6))
			 * stopifnot(all.equal.numeric(S.C.T.p(0.65, 5,1500), 27.995347, 1e-6))
			 */

			/* rt = rt(T) = C(35,T,0)/C(35,15,0), eqn (3) p.7 FM83 */
			rt = c[0] + T*(c[1] + T*(c[2] + T*(c[3] + T*c[4])));
		    /* Rp, eqn (4) p.8 FM83 */
			Rp = 1 + ( p * (e[0] + p * (e[1] + p * e[2]))) /
			     (1 + T *(d[0] + T * d[1]) + (d[2] + T * d[3]) * C);
			Rt = C / (Rp * rt);
			/* Eqn (1) & (2) p6 and 7 FM83 */
			Rtx = sqrt(Rt);
			del_T = T - 15;
			del_S = (del_T / (1 + k * del_T) ) *
			        (b[0] + (b[1] + (b[2]+ (b[3] + (b[4] + b[5]*Rtx)*Rtx)*Rtx)*Rtx)*Rtx);
			S = a[0] + (a[1] + (a[2] + (a[3] + (a[4] + a[5]*Rtx)*Rtx)*Rtx)*Rtx)*Rtx;
			S = S + del_S;
			*value++ = S;
		}
	}
}
