#define PROJ_PARMS__ \
	double w; \
	double m, rm;
#define PJ_LIB__
# include	"projects.h"
PROJ_HEAD(hammer, "Hammer & Eckert-Greifendorff") "\n\tMisc Sph, \n\tW= M=";
	//"\n\tMisc Sph, no inv.\n\tW= M=";
FORWARD(s_forward); /* spheroid */
	double cosphi, d;

	d = sqrt(2./(1. + (cosphi = cos(lp.phi)) * cos(lp.lam *= P->w)));
	xy.x = P->m * d * cosphi * sin(lp.lam);
	xy.y = P->rm * d * sin(lp.phi);
	return (xy);
}
// from https://github.com/jswhit/pyproj/blob/master/src/PJ_hammer.c.diff
INVERSE(s_inverse); /* spheroid */
// supposedly this patch is from
//   http://mathworld.wolfram.com/Hammer-AitoffEqual-AreaProjection.html
// but the 0.25 is squared there. Also, the patch was given in 2010 but
// has (as of Feb 3, 2015) not been taken up by proj4, and that doesn't
// seem to be a good sign.
        double z;
	z = sqrt(1. - 0.25*P->w*P->w*xy.x*xy.x - 0.25*xy.y*xy.y);
        lp.lam = aatan2(P->w * xy.x * z,2. * z * z - 1)/P->w;
        lp.phi = aasin(P->ctx,z * xy.y);
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
	P->es = 0.; P->fwd = s_forward;
ENDENTRY(P)
