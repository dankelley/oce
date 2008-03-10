#include <R.h>
#include <stdio.h>
static double sig_0, p_ref, S, T;
void
sw_strho(double *pT, double *prho, double *pp, double *res)
{
    int strho_bisection_search (double *x, double x1, double x2, double eps, double eta);
	int bs_res;
	T = *pT;
	sig_0 = *prho;				/* target density */
	p_ref = *pp;				/* target pressure */
	*res = NA_REAL;
	if (ISNA(*pT) || ISNA(*prho) || ISNA(*pp))
		return;
#define SLOW  0.0001   /* left bracket on search region */
#define SHIGH 200.0	   /* right bracket on search region */
#define RHO_TOL 0.00001 /* stop if get to within this rho value */
#define S_RES   0.00001 /* stop if bracket to within this resolution */
	bs_res = strho_bisection_search(&S, SLOW, SHIGH, S_RES, RHO_TOL);/* BUG: ignoring result */
	/* printf(" trsho(S=%f  rho=%f  p=%f) returning %f\n",S,sig_0,p_ref,T); */
	*res = S;
}


double
strho_f(double x)
{
	extern double p_ref, sig_0;
	void sw_rho(int *n, double *pS, double *pT, double *pp, double *res);
	double this_rho;
	int n=1;
	sw_rho(&n, &x, &T, &p_ref, &this_rho);
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
strho_bisection_search(double *x, double x1, double x2, double xresolution, double ftol)
{
	/* printf("in bisection_search(x=%f,  x1=%f,  x2=%f)\n",*x,x1,x2); */
	extern double strho_f(double x);
	double g1, g2, g;
	g1 = strho_f(x1);
	g2 = strho_f(x2);
	if (g1 * g2 > 0.0) {
		*x = NA_REAL;
		return 0;
	}
	/* printf("TOP of bs.  g1=%f   g2=%f\n",g1,g2); */
	while (fabs (g = strho_f (*x = (x1 + x2) / 2.0)) > ftol || fabs (x1 - x2) > xresolution) {
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
			/* printf("bs CASE 3 (not bracketed)  x1=%f  x2=%f  g1=%f  g2=%f\n",x1,x2,g1,g2);*/
			*x = NA_REAL;
			return (1); /* exact solution */
		}
	}
	return (0); 		/* converged by default */
}
