// Barnes in domain x,y,t
// $Id: nc_barnes3d.cc,v 1.20 1998/04/10 18:44:40 kelley Exp $
// Log at end of file

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stream.h>
#include <iostream.h>
#include <netcdf.hh>		// c++ interface

int debug = 0;			// make 1 to get debug output

char *USAGE = " [-d level] [-m missing_value] [-i initial] xname scale:min:max:inc yname scale:min:max:inc tname scale:min:max:inc F gamma iterations in.nc\n"
"  where:\n"
" '-d level' turns on debugging mode to indicated level, e.g.\n"
"       0 -> minimal (no) debugging\n"
"      >0 -> minimal debugging (grid geometry, etc)\n"
"      >1 -> print data as read them\n"
"      >2 -> print all kinds of stuff\n"
" '-m missing_value' sets a missing value (default -999)\n"
" 'xname' names x-column.  Initial smoothing scale is 'scale'; grid runs from min to max, by inc.\n"
"    ... same for 'yname' (y-column) and 'tname' (time)\n"
"\n"
" The grid runs min to max by increments of inc, and F is gridded \n"
" as function of (x,y,t) using 3D Barnes algorithm with indicated gamma\n"
" and number of iterations.\n"
;


#define NAME_LEN 25		// max length of variable names

float missing_value = -999.0;


inline bool is_missing(float f)
{
    return (fabs(f - missing_value) < 0.001) ? true : false;
}
int
main(int argc, char *argv[])
{
    // Decode commandline
    extern char    *optarg;
    int             c;		// char of option-flag
    int             options = 0;
    double initial_guess = 0.0;
    while ((c = getopt (argc, argv, "d:m:")) != -1) {
	switch (c) {
	case 'd':
	    debug = atoi(optarg);
	    options += 2;
	    break;
	case 'm':
	    missing_value = atof(optarg);
	    options += 2;
	    break;
	}
    }
    if (debug > 0)
	fprintf(stderr, "initial guess is %f\n", initial_guess);
    if (argc !=  11 + options) {
	fprintf(stderr, "%s: wrong number of arguments\nUsage:\n%s", argv[0], USAGE);
	exit(1);
    }
    float xscale, xmin, xmax, xinc;
    float yscale, ymin, ymax, yinc;
    float tscale, tmin, tmax, tinc;
    char xname[NAME_LEN], yname[NAME_LEN], zname[NAME_LEN], Fname[NAME_LEN];
    //
    // X
    strcpy(xname, argv[options + 1]);
    if (4 != sscanf(argv[options + 2], "%f:%f:%f:%f", &xscale, &xmin, &xmax, &xinc)) {
	cerr << argv[0] << ": cannot decode X data from `" << argv[options + 2] << "'\n";
	exit(1);
    }
    if (debug > 0)
	cerr << "X data named `" << xname << "'.  radius=" << xscale << " min=" << xmin << " max=" << xmax << " inc= " <<  xinc << endl;
    //
    // Y
    strcpy(yname, argv[options + 3]);
    if (4 != sscanf(argv[options + 4], "%f:%f:%f:%f", &yscale, &ymin, &ymax, &yinc)) {
	cerr << argv[0] << ": cannot decode Y data from `" << argv[options + 4] << "'\n";
	exit(1);
    }
    if (debug > 0)
	cerr << "Y data named `" << yname << "'.  radius=" << yscale << " min=" << ymin << " max=" << ymax << " inc= " <<  yinc << endl;
    //
    // time
    char tname[NAME_LEN];
    strcpy(tname, argv[options + 5]);
    if (4 != sscanf(argv[options + 6], "%f:%f:%f:%f", &tscale, &tmin, &tmax, &tinc)) {
	cerr << argv[0] << ": cannot decode time data from `" << argv[options + 6] << "'\n";
	exit(1);
    }
    if (debug > 0)
	cerr << "time data named `" << tname << "'.  radius=" << tscale << " min=" << tmin << " max=" << tmax << " inc= " <<  tinc << endl;
    //
    // F
    strcpy(Fname, argv[options + 7]);
    if (debug > 0)
	cerr << "F data named `" << Fname << "'" << endl;
    //
    float gamma;
    sscanf(argv[options + 8], "%f", &gamma);
    int iterations;
    sscanf(argv[options + 9], "%d", &iterations);
    if (debug > 0)
	cerr << "Gamma = " << gamma << "; iterations = " << iterations << endl;
    char *inName = argv[options + 10];
    NcFile in(inName, NcFile::ReadOnly);
    if (debug > 0)
	cerr << "Opened file `" << inName << "'\n";
    if (in.num_dims() != 1) {
	cerr << "There should be 1 dimension to the netcdf file `" << 
	    inName << "', but have " << in.num_dims() << endl;
	exit(1);
    }
    //
    // Find length of vectors
    NcVar *vp = in.get_var(0);
    long *e = vp->edges();
    long length = e[0];
    // Get x
    NcVar *var_x = in.get_var(xname);
    if (var_x->edges()[0] != length) {
	cerr << "Inconsistent length for x variable, `" << xname << "'\n";
	exit(1);
    }
    float *xdata = new float[length];
    if (!var_x->get(xdata, e)) {
	cerr << "Can't read x variable, `" << xname << "'\n";
	exit(1);
    }
    if (debug > 0)
	fprintf(stderr, "Read %d as length of x-column, named `%s'\n", length, xname);
    if (debug > 1) {
	fprintf(stderr, "x data are as follows:\n");
	for (unsigned int i = 0; i < length; i++)
	    fprintf(stderr, "\t%g\n", xdata[i]);
    }

    // Get y
    NcVar *var_y = in.get_var(yname);
    if (var_y->edges()[0] != length) {
	cerr << "Inconsistent length for y variable, `" << yname << "'\n";
	exit(1);
    }
    float *ydata = new float[length];
    if (!var_y->get(ydata, e)) {
	cerr << "Can't read y variable, `" << yname << "'\n";
	exit(1);
    }
    if (debug > 0)
	fprintf(stderr, "Read %d as length of y-column, named `%s'\n", length, yname);
    if (debug > 1) {
	fprintf(stderr, "y data are as follows:\n");
	for (unsigned int i = 0; i < length; i++)
	    fprintf(stderr, "\t%g\n", ydata[i]);
    }

    // Get time ('t')
    NcVar *var_t = in.get_var(tname);
    if (var_t->edges()[0] != length) {
	cerr << "Inconsistent length for t variable, `" << tname << "'\n";
	exit(1);
    }
    float *tdata = new float[length];
    if (!var_t->get(tdata, e)) {
	cerr << "Can't read t variable, `" << tname << "'\n";
	exit(1);
    }
    if (debug > 0)
	fprintf(stderr, "Read %d as length of time column, named `%s'\n", length, tname);
    if (debug > 1) {
	fprintf(stderr, "time data are as follows:\n");
	for (unsigned int i = 0; i < length; i++)
	    fprintf(stderr, "\t%g\n", tdata[i]);
    }


    // Get F
    NcVar *var_F = in.get_var(Fname);
    if (var_F->edges()[0] != length) {
	cerr << "Inconsistent length for F variable, `" << Fname << "'\n";
	exit(1);
    }
    float *Fdata = new float[length];
    if (!var_F->get(Fdata, e)) {
	cerr << "Can't read F variable, `" << Fname << "'\n";
	exit(1);
    }
    if (debug > 0)
	fprintf(stderr, "Read %d as length of F(x,y,t) column, named `%s'\n", length, Fname);
    if (debug > 1) {
	fprintf(stderr, "F(x,y,t) data are as follows:\n");
	for (unsigned int i = 0; i < length; i++)
	    fprintf(stderr, "\t%g\n", Fdata[i]);
    }


    //
    // Get storage
    int xnum = int(floor(1.5 + (xmax - xmin) / xinc));
    int ynum = int(floor(1.5 + (ymax - ymin) / yinc));
    int tnum = int(floor(1.5 + (tmax - tmin) / tinc));
    if (debug > 0)
	cerr << "xnum= " << xnum << "    ynum= " << ynum << "    tnum= " << tnum << endl;

    double *xg = new double[xnum];
    double *yg = new double[ynum];
    double *tg = new double[tnum];
    double *F_last = new double[length];
    double *F_last2 = new double[length];
    double G[xnum][ynum][tnum];
    bool   *missing = new bool[length];

    // Set Initial Conditions
    xg[0] = xmin;
    yg[0] = ymin;
    tg[0] = tmin;
    for (int ii = 0; ii < xnum - 1; ii++)
	xg[ii + 1] = xg[ii] + xinc;
    for (int jj = 0; jj < ynum - 1; jj++)
	yg[jj + 1] = yg[jj] + yinc;
    for (int kk = 0; kk < tnum - 1; kk++)
        tg[kk + 1] = tg[kk] + tinc;
    for (int ii = 0; ii < xnum; ii++)
	for (int jj = 0; jj < ynum; jj++)
	    for (int kk=0; kk < tnum; kk++)
		G[ii][jj][kk] = initial_guess;
    for (int rr = 0; rr < length; rr++) {
        F_last[rr] = initial_guess;
	missing[rr] = is_missing(xdata[rr]) || is_missing(ydata[rr]) || is_missing(tdata[rr]) || is_missing(Fdata[rr]);
	if (debug > 2) {
	    fprintf(stderr, "\trr=%d F_last=%g missing=%c\n", rr, F_last[rr], missing[rr]?'Y':'N');
	}
    }
    
    for (int iteration = 0; iteration < iterations; iteration++) {
	if (debug > 0)
	    fprintf(stderr, "Iteration %d\n", iteration);
	// interpolate at grid
	for (int ii=0; ii < xnum; ii++) {
	    if (debug > 0)
		fprintf(stderr, ".");
	    for (int jj=0; jj < ynum; jj++) {
		for (int kk=0; kk < tnum; kk++) {
		    double sum = 0.0;
		    double sum_w = 0.0;
		    bool have_some_nonmissing_data = false;
		    for (int rr=0; rr < length; rr++) {
			if(!missing[rr]) {
			    have_some_nonmissing_data = true;
			    double dx = (xdata[rr] - xg[ii]) / xscale;
			    double dy = (ydata[rr] - yg[jj]) / yscale;
			    double dt = (tdata[rr] - tg[kk]) / tscale;
			    double arg = dx * dx + dy * dy + dt * dt;
			    double w;
			    // Approximation to within 1 part in 4000
			    w = 1.0 / (0.999448 
				       + arg * (1.023820 
						+ arg * (0.3613967
							 + arg * (0.4169646
								  + arg * (-0.1292509
									   + arg * 0.0499565)))));
			    //double w = exp(-dx * dx - dy * dy - dt * dt);
			    sum_w += w;
			    sum   += w * (Fdata[rr] - F_last[rr]);
			    //printf("ii=%d jj=%d kk=%d rr=%d w=%f sum=%f\n",ii,jj,kk,rr,w,sum);
			}
		    }
		    if (!have_some_nonmissing_data) {
			fprintf(stderr, "\nERROR: while making grid (ii=%d jj=%d kk=%d) all data missing.  iteration=%d\n", ii, jj, kk, iteration);
			exit(2);
		    }

		    //printf("G[%d][%d][%d] = %f (before) ... ", ii,jj,kk,G[ii][jj][kk]);
		    G[ii][jj][kk] += sum / sum_w;
		    //printf("%f after\n", G[ii][jj][kk]);
		}
	    }
	}
	// interpolate at data
	for (int r=0; r < length; r++) {
	    double sum = 0.0;
	    double sum_w = 0.0;
	    bool have_some_nonmissing_data = false;
	    for (int rr=0; rr < length; rr++) {
		if(!missing[rr]) {
		    have_some_nonmissing_data = true;
		    double dx = (xdata[rr] - xdata[r]) / xscale;
		    double dy = (ydata[rr] - ydata[r]) / yscale;
		    double dt = (tdata[rr] - tdata[r]) / tscale;
		    double arg = dx * dx + dy * dy + dt * dt;
		    double w;
		    // Approximation is good to within 1 part in 4000
		    w = 1.0 / (0.999448 
			       + arg * (1.023820 
					+ arg * (0.3613967
						 + arg * (0.4169646
							  + arg * (-0.1292509
								   + arg * 0.0499565)))));
		    
		    sum_w += w;
		    sum   += w * (Fdata[rr] - F_last[rr]);
		    if (debug > 2) {
			fprintf(stderr, "r=%d rr=%d: dx=%4.1f dy=%4.1f dt=%4.1f arg=%g w=%g Fdata=%g F_last=%g sum_entry=%g sum=%g sum_w=%g\n",
				r, rr, dx, dy, dt, arg, w, Fdata[rr], F_last[rr], w * (Fdata[rr]-F_last[rr]), sum, sum_w);
		    }
		    if (sum > 1e100) exit(1);

		}
	    }
	    if (!have_some_nonmissing_data) {
		fprintf(stderr, "\nERROR: all data missing.  iteration=%d  r=%d\n", iteration, r);
		exit(2);
	    }

	    F_last2[r] += sum / sum_w;
	}
	for (int r=0; r < length; r++)
            F_last[r] = F_last2[r];
    }

    // Output results
    cout << tscale << ' ' << xscale << ' ' << yscale << 
	" // tscale xscale yscale " << endl;
    cout <<   tmin << ' ' <<   tmax << ' ' <<   tinc
	<< " // tmin tmax tinc" << endl;
    for (int kk=0; kk < tnum; kk++) {
	cout << tg[kk] << ' '
	    << xmin <<  ' ' << xmax <<  ' ' << xinc <<  ' '
	    << ymin <<  ' ' << ymax <<  ' ' << yinc
	    << " // t xmin xmax xinc ymin ymax yinc" << endl;
        for (int jj = ynum - 1; jj > -1; jj--) {
            for (int ii=0; ii < xnum; ii++) {
		cout << G[ii][jj][kk] << ' ';
	    }
            cout << endl;
	}
    }	
    return 0;
}

// $Log: nc_barnes3d.cc,v $
// Revision 1.20  1998/04/10  18:44:40  kelley
// Changed some storage allocation (xg, yg, etc) and, magically, it works
// again on my tritium Kv application.  But now I'm worried about the
// G[][][] allocation, which uses GNU-style (and, I now infer, risky)
// allocation.
//
// Revision 1.19  1998/04/10  16:20:25  kelley
// Add more debugging
//
// Revision 1.14  1997/06/03  21:12:08  kelley
// (1) add new debugging info; (2) to ansi C++ for() scoping.
//
// Revision 1.10  1995/12/15  17:36:59  kelley
// Add indication of tscale, xscale and yscale.
// Add comments (in gri format) of what is contained.
//
// Revision 1.7  1995/12/14  14:46:20  kelley
// 1) move to c++ interface (simpler to code/debug)
// 2) add -d debug option, which is good for default, actually
//

