// R CMD SHLIB dlsym2.c
// R --no-save < dlsym.R
#include <dlfcn.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

void gsw3a(char **lib, char **name, int *n, double *a1, double *a2, double *a3, double *rval)
{
    //*lib = "/usr/local/lib/libgswteos-10.so"; // FIXME: testing, since getting an error otherwise
    //Rprintf("DEBUG: using library \"%s\"\n", *lib);
    void *handle = dlopen(*lib, RTLD_LOCAL|RTLD_LAZY);
    if (!handle) {
        error("cannot open TEOS library %s; error is: %s", *lib, dlerror());
    }
    double (*f3)(double, double, double) = dlsym(handle, *name);
    if (!f3) 
        error("cannot find \"%s\" in TEOS library %s", *name, *lib);
    for (int i = 0; i < *n; i++) {
        rval[i] = (*f3)(a1[i], a2[i], a3[i]);
    }
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

