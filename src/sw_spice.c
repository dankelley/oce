#include <R.h>
/* See license information and history at end of file */
void
sw_spice(double *ps, double *pt, double *pp, double *spice)
{
	double p = *pp, t = *pt, s = *ps;
	double sp,T,S;
	int i,j;
	/* http://satftp.soest.hawaii.edu/spice/spice.txt */
	static double b[6][5] = {
		{ 0.,          7.7442e-1, -5.85e-3,   -9.84e-4,   -2.06e-4},
		{ 5.1655e-2,   2.034e-3,  -2.742e-4,  -8.5e-6,     1.36e-5},
		{ 6.64783e-3, -2.4681e-4, -1.428e-5,   3.337e-5,   7.894e-6},
		{-5.4023e-5,   7.326e-6,   7.0036e-6, -3.0412e-6, -1.0853e-6},
		{ 3.949e-7,   -3.029e-8,  -3.8209e-7,  1.0012e-7,  4.7133e-8},
		{-6.36e-10,   -1.309e-9,   6.048e-9,  -1.1409e-9, -6.676e-10}};
	*spice = NA_REAL;
	if (ISNA(*ps) || ISNA(*pt) || ISNA(*pp))
		return;
	s=(s-35.);
	sp=0.0;
	T=1.0;
	for (i=0;i<6;i++) {
		S=1.0;
		for(j=0;j<5;j++) {
			sp+=b[i][j]*T*S;
			S*=s;
		}
		T*=t;
	}
	*spice = sp;
}

/* Original code from Pierre Flament's website 
   http://satftp.soest.hawaii.edu/spice/spice.html

   Converted to a form suitable R usage in the "oce" library,
   including changing the call and making the coefficient definition
   for efficiency, by Dan Kelley 2003-jul-27.

   NB. pressure is ignored.

   License information: the following is quoted from an email
   from Pierre Flament to Dan Kelley, 
     Message-id: <3F22F951.5010505@mael.soest.hawaii.edu>
     From pflament@mael.satlab.hawaii.edu  Sat Jul 26 18:59:44 2003

   "I hereby tranfer all rights of use, licensing and copyright to my
    definition of "spiciness" which appeared in Progress in
    Oceanography, to Dan Kelley, with the provision that he uses,
    diseminates and relicenses it under the provision of the GNU
    licensing scheme."

    Pierre Flament.
*/
