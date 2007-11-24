#include <R.h>
#include <Rdefines.h>

void
sw_svel(int *n, double *pS, double *pT, double *pp, double *value)
{
	int i;
	for (i = 0; i < *n; i++) {
		double S = *pS++;
		double T = *pT++;
		double p = *pp++;
		double rho_w, Kw, Aw, Bw, p1, S12, ro, xkst;
		p = p / 10.0; /* use bar to match UNESCO routines */
		/*
 		 * eqn 34 p.46
		 */
		double c00 = 1402.388;
		double c01 =    5.03711;
		double c02 =   -5.80852e-2;
		double c03 =    3.3420e-4;
		double c04 =   -1.47800e-6;
		double c05 =    3.1464e-9;
		double c10 =  0.153563;
		double c11 =  6.8982e-4;
		double c12 = -8.1788e-6;
		double c13 =  1.3621e-7;
		double c14 = -6.1185e-10;
		double c20 =  3.1260e-5;
		double c21 = -1.7107e-6;
		double c22 =  2.5974e-8;
		double c23 = -2.5335e-10;
		double c24 =  1.0405e-12;
		double c30 = -9.7729e-9;
		double c31 =  3.8504e-10;
		double c32 = -2.3643e-12;
		double Cw = c00 
			+ T * (c01 + T * (c02 + T * (c03 + T * (c04 + T * c05))))
   				+ p * (c10 + T * (c11 + T * (c12 + T * (c13 + T * c14)))
   					+ p * (c20 + T * (c21 + T * (c22 + T * (c23 + T * c24)))
						+ p * (c30 + T * (c31 + T * c32))));
		/*
		 * eqn 35. p.47
		 */
		double a00 =  1.389;
		double a01 = -1.262e-2;
		double a02 =  7.164e-5;
		double a03 =  2.006e-6;
		double a04 = -3.21e-8;
		double a10 =  9.4742e-5;
		double a11 = -1.2580e-5;
		double a12 = -6.4885e-8;
		double a13 =  1.0507e-8;
		double a14 = -2.0122e-10;
		double a20 = -3.9064e-7;
		double a21 =  9.1041e-9;
		double a22 = -1.6002e-10;
		double a23 =  7.988e-12;
		double a30 =  1.100e-10;
		double a31 =  6.649e-12;
		double a32 = -3.389e-13;
		double A = a00
			+ T * (a01 + T * (a02 + T * (a03 + T * a04)))
     			+ p * (a10 + T * (a11 + T * (a12 + T * (a13 + T * a14)))
     				+ p * (a20 + T * (a21 + T * (a22 + T * a23))
     				  	+ p * (a30 + T * (a31 + T * a32))));

 	   /*
		* eqn 36 p.47
		*/
		double b00 = -1.922e-2;
		double b01 = -4.42e-5;
		double b10 =  7.3637e-5;
		double b11 =  1.7945e-7;
		double B = b00 + T * b01 + p * (b10 + T * b11);

		/*
		 * eqn 37 p.47
		 */
		double d00 =  1.727e-3;
		double d10 = -7.9836e-6;
		double D = d00 + d10 * p;

		/*
		* eqn 33 p.46
		*/
		*value++ = Cw + S * (A + B * sqrt(S) + S * D);
	}
}

