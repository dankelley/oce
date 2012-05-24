// R CMD SHLIB dlsym2.c
// R --no-save < dlsym.R
#include <string.h>

#ifdef Unix
# include <dlfcn.h>
#else
# ifdef Win32
#  include <windows.h>
# endif
#endif

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
static int first_teos_call = 1;
static void *teos_handle = NULL;

char *libteosp;
// system("R CMD SHLIB sw.c")
// .C("set_libteos", "dan")
void set_libteos(char **lib)
{
  Rprintf("set_libteos() ...\n");
  Rprintf("lib='%s' (length %d)\n", *lib, strlen(*lib));
  libteosp = (char *)malloc(sizeof(char)*strlen(*lib));
  strcpy(libteosp, *lib);
  Rprintf(" assigned '%s'\n",libteosp);
}
char *get_libteos()
{
    return(libteosp);
}

void gsw2a(char **lib, char **name, int *n, double *a1, double *a2, double *rval)
{
#ifdef Unix
    if (first_teos_call) {
        teos_handle = dlopen(*lib, RTLD_LOCAL|RTLD_LAZY);
        first_teos_call = 0;
    }
    Rprintf("FYI libteos '%s'\n", libteosp);
    if (!teos_handle)
        error("cannot open TEOS library %s; error is: %s", *lib, dlerror());
    Rprintf("%s:%d about to do try to find dlsym(handle, \"%s\"\n", __FILE__, __LINE__, *name);
    double (*f2)(double, double) = dlsym(teos_handle, *name);
    if (!f2) 
        error("cannot find \"%s\" in TEOS library %s; error is: %s", *name, *lib, dlerror());
    Rprintf("%s:%d about to do the loop calling f2 (\"%s\")\n", __FILE__, __LINE__, *name);
    for (int i = 0; i < *n; i++) {
        //Rprintf("%s:%d in loop i=%d, a1[i]=%f, a2[i]=%f\n",__FILE__,__LINE__,i,a1[i],a2[i]);
        rval[i] = (*f2)(a1[i], a2[i]);
    }
#else
#ifdef Win32
        error("teos()/gsw2a does not work for the Windows OS (yet)");
#endif
#endif
}

void gsw3a(char **lib, char **name, int *n, double *a1, double *a2, double *a3, double *rval)
{
#ifdef Unix
    if (first_teos_call) {
        teos_handle = dlopen(*lib, RTLD_LOCAL|RTLD_LAZY);
        first_teos_call = 0;
    }
    Rprintf("FYI libteos '%s'\n", libteosp);
    if (!teos_handle)
        error("cannot open TEOS library %s; error is: %s", *lib, dlerror());
    Rprintf("%s:%d about to do try to find dlsym(handle, \"%s\"\n", __FILE__, __LINE__, *name);
    double (*f3)(double, double, double) = dlsym(teos_handle, *name);
    if (!f3) 
        error("cannot find \"%s\" in TEOS library %s; error is: %s", *name, *lib, dlerror());
    Rprintf("%s:%d about to do the loop calling f3 (\"%s\")\n", __FILE__, __LINE__, *name);
    for (int i = 0; i < *n; i++) {
        Rprintf("%s:%d in loop i=%d, a1[i]=%f, a2[i]=%f a3[i]=%f\n",__FILE__,__LINE__,i,a1[i],a2[i],a3[i]);
        rval[i] = (*f3)(a1[i], a2[i], a3[i]);
    }
#else
#ifdef Win32
        error("teos()/gsw3a does not work for the Windows OS (yet)");
#endif
#endif
}

void gsw4a(char **lib, char **name, int *n, double *a1, double *a2, double *a3, double *a4, double *rval)
{
    Rprintf("111\n");
#ifdef __GNUC__
  Rprintf("IS __GNUC__\n");
#endif
#ifdef unix
  Rprintf("IS unix\n");
#endif
#ifdef Unix
  Rprintf("IS Unix\n");
#endif
#ifdef WIN32
  Rprintf("IS WIN32\n");
#endif

#ifdef Unix
    Rprintf("222\n");
    //*lib = "/usr/local/lib/libgswteos-10.so"; // FIXME: testing, since getting an error otherwise
    Rprintf("DEBUG: using library \"%s\"\n", *lib);
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
    Rprintf("333\n");
    dlclose(handle);
#else
#ifdef Win32
        error("teos()/gsw4a does not work for the Windows OS (yet)");
#endif
#endif
}

