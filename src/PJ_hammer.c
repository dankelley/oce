#define PROJ_PARMS__ \
	double w; \
	double m, rm;
#define PJ_LIB__
#define EPS 1.0e-10

# include	"projects.h"
PROJ_HEAD(hammer, "Hammer & Eckert-Greifendorff")
	"\n\tMisc Sph, no inv.\n\tW= M=";
FORWARD(s_forward); /* spheroid */
	double cosphi, d;

	d = sqrt(2./(1. + (cosphi = cos(lp.phi)) * cos(lp.lam *= P->w)));
	xy.x = P->m * d * cosphi * sin(lp.lam);
	xy.y = P->rm * d * sin(lp.phi);
	return (xy);
}
// This patch is said to be based on
//   http://mathworld.wolfram.com/Hammer-AitoffEqual-AreaProjection.html
// and was submitted to proj4 in 2010 but has (as of Feb 3, 2015) not been
// incorporated by proj4 ... why? (Email sent to JW, the author, on Feb 3).
// Some spots where this has been used are given below.
//   https://github.com/jswhit/pyproj/blob/master/src/PJ_hammer.c.diff
//   https://github.com/matplotlib/basemap/blob/master/src/PJ_hammer.c
INVERSE(s_inverse); /* spheroid */
        //lp.lam=0.0;
        //lp.phi=0.0;
        double z;
	z = sqrt(1. - 0.25*P->w*P->w*xy.x*xy.x - 0.25*xy.y*xy.y);
	if (fabs(2.*z*z-1.) < EPS) {
           lp.lam = HUGE_VAL;
           lp.phi = HUGE_VAL;
           pj_errno = -14;
	} else {
	   lp.lam = aatan2(P->w * xy.x * z,2. * z * z - 1)/P->w;
	   lp.phi = aasin(P->ctx,z * xy.y);
        }
	return (lp);
}
FREEUP; if (P) pj_dalloc(P); }
ENTRY0(hammer)
	if (pj_param(P->ctx, P->params, "tW").i) {
		if ((P->w = fabs(pj_param(P->ctx, P->params, "dW").f)) <= 0.) E_ERROR(-27);
	} else
		P->w = .5;
	if (pj_param(P->ctx, P->params, "tM").i) {
		if ((P->m = fabs(pj_param(P->ctx, P->params, "dM").f)) <= 0.) E_ERROR(-27);
	} else
		P->m = 1.;
	P->rm = 1. / P->m;
	P->m /= P->w;
	P->es = 0.; P->fwd = s_forward; P->inv = s_inverse;
ENDENTRY(P)
