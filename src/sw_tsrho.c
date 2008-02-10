#include <R.h>
#include <stdio.h>
static double sig_0, p_ref, S, T;
void
sw_tsrho(double *pS, double *prho, double *pp, double *res)
{
	int tsrho_bisection_search (double *x, double x1, double x2, double eps, double eta);
	int bs_res;
	S = *pS;
	sig_0 = *prho;		/* target density */
	p_ref = *pp;		/* target pressure */
	*res = NA_REAL;
	if (ISNA(S) || ISNA(sig_0) || ISNA(p_ref))
		return;
	/* NOTE: do not use wide values for TLOW and THIGH, because the UNESCO
	* equation of state rho() may be odd in such limits, preventing a 
	* bisection from working.  I found this out by using a TLOW
	* value of -50.  The range below should be OK for oceanographic use.
	*/
#define TLOW  -3.0		/* left bracket on T search region */
#define THIGH 40.0		/* right bracket on T search region */
#define RHO_TOL 0.0001		/* stop if get to within this rho value */
#define T_RES   0.0001		/* stop if bracket T to within this resolution */
	bs_res = tsrho_bisection_search(&T, TLOW, THIGH, T_RES, RHO_TOL);/* BUG: ignoring result */
	/* printf(" tsrho(S=%f  rho=%f  p=%f) returning %f\n",S,sig_0,p_ref,T); */
	*res = T;
}

double
tsrho_f(double x)
{
	extern double p_ref, sig_0;
	void sw_rho(int *n, double *pS, double *pT, double *pp, double *res);
	double this_rho;
	int n=1;
	sw_rho(&n, &S, &x, &p_ref, &this_rho);
	/* printf(" f returning %f\n", this_rho-1000.0-sig_0); */
	return (this_rho - 1000.0 - sig_0);
}

/* bisection rootsolver
   SYNTAX
   int bis(double *x,double x1,double x2,double eps,double eta);
   DESCRIPTION: Searches for a root of f(x) over the interval [x1,x2].
   ftol = maximum allowed error in f(x)
   xresolution = maximum size of final interval bracketing  root
   RETURN VALUE
   0 if root found to within tolerance; 1 otherwise
*/
int
tsrho_bisection_search(double *x, double x1, double x2, double xresolution, double ftol)
{
	/* printf("in bisection_search(x=%f,  x1=%f,  x2=%f)\n",*x,x1,x2); */
	double tsrho_f(double x);
	double g1, g2, g;
	g1 = tsrho_f(x1);
	g2 = tsrho_f(x2);
	if (g1 * g2 > 0.0) {
		*x = NA_REAL;
		return 0;
	}
	/* printf("TOP of bs.  g1=%f   g2=%f\n",g1,g2); */
	while (fabs (g = tsrho_f (*x = (x1 + x2) / 2.0)) > ftol || fabs (x1 - x2) > xresolution) {
		/* printf("in bis loop x=%f   g=%f   g1=%f\n",*x,g,g1); */
		if (g1 * g < 0) { /* root is nearer x1 so move x2 to x */
			x2 = *x;
			g2 = g;
			/* printf("bs CASE 1.  x1=%f  x2=%f  g1=%f  g2=%f\n",x1,x2,g1,g2); */
		} else if (g2 * g < 0) { /* root is nearer x2 so move x1 to x */
			x1 = *x;
			g1 = g;
			/* printf("bs CASE 2.  x1=%f  x2=%f  g1=%f  g2=%f\n",x1,x2,g1,g2); */
		} else {	/* not bracketed BUG */
			/* printf("bs CASE 3 (not bracketed)  x1=%f  x2=%f  g1=%f  g2=%f\n",x1,x2,g1,g2); */
			*x = NA_REAL;
			return (1); /* exact solution */
		}
	}
	return (0); 		/* converged by default */
}
