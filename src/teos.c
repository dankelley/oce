// R CMD SHLIB dlsym2.c
// R --no-save < dlsym.R
#include <dlfcn.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

void gsw3a(char **lib, char **name, int *n, double *a1, double *a2, double *a3, double *rval)
{
    //*lib = "/usr/local/lib/libgswteos-10.so"; // FIXME: testing, since getting an error otherwise
    //Rprintf("%s:%d about to do dlopen on '%s'\n", __FILE__, __LINE__, *lib);
    void *handle = dlopen(*lib, RTLD_LOCAL|RTLD_LAZY);
    //Rprintf("%s:%d about to do check handle for that dlopen\n", __FILE__, __LINE__);
    if (!handle) {
        error("cannot open TEOS library %s; error is: %s", *lib, dlerror());
    }
    //Rprintf("%s:%d about to do try to find dlsym(handle, \"%s\"\n", __FILE__, __LINE__, *name);
    double (*f3)(double, double, double) = dlsym(handle, *name);
    if (!f3) 
        error("cannot find \"%s\" in TEOS library %s", *name, *lib);
    //Rprintf("%s:%d about to do the loop calling f3 (\"%s\")\n", __FILE__, __LINE__, *name);
    for (int i = 0; i < *n; i++) {
        //Rprintf("%s:%d in loop i=%d, a1[i]=%f, a2[i]=%f a3[i]=%f\n",__FILE__,__LINE__,i,a1[i],a2[i],a3[i]);
        rval[i] = (*f3)(a1[i], a2[i], a3[i]);
    }
    //Rprintf("      gsw3a(lib, '%s', n=%d, a1=%f a2=%f a3=%f  RETURNING %f\n", *name, *n, *a1, *a2, *a3, rval[0]);
    dlclose(handle);
}

void gsw4a(char **lib, char **name, int *n, double *a1, double *a2, double *a3, double *a4, double *rval)
{
    //*lib = "/usr/local/lib/libgswteos-10.so"; // FIXME: testing, since getting an error otherwise
    //Rprintf("DEBUG: using library \"%s\"\n", *lib);
    void *handle = dlopen(*lib, RTLD_LOCAL|RTLD_LAZY);
    if (!handle) {
        error("cannot open TEOS library %s; error is: %s", *lib, dlerror());
    }
    double (*f4)(double, double, double, double) = dlsym(handle, *name);
    if (!f4) 
        error("cannot find \"%s\" in TEOS library %s", *name, *lib);
    for (int i = 0; i < *n; i++) {
        rval[i] = (*f4)(a1[i], a2[i], a3[i], a4[i]);
    }
    dlclose(handle);
}

