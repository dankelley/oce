// vim: set noexpandtab shiftwidth=2 softtabstop=2 tw=70:
//
// Notes on trying to get windows to work:
//   __GNUCC__ is defined for OSX, but none of the following are:
//    _unix_ __unix__ unix Unix
//*lib = "/usr/local/lib/libgswteos-10.so"; // FIXME: testing, since getting an error otherwise
#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#ifdef WIN32
#  include <windows.h>
#else
# include <dlfcn.h>
static void *teos_handle = NULL;
#endif

//#define DEBUG

static int first_teos_call = 1;

char *libteosp;
// system("R CMD SHLIB sw.c")
// .C("set_libteos", "dan")
void set_libteos(char **lib)
{
#ifdef DEBUG
  Rprintf("set_libteos(%s)\n", *lib);
#endif
  libteosp = (char *)malloc(sizeof(char)*strlen(*lib));
  strcpy(libteosp, *lib);
}

char *get_libteos()
{
  return(libteosp);
}

void gsw2a(char **lib, char **name, int *n, double *a1, double *a2, double *rval)
{
#ifdef DEBUG
  Rprintf("gsw2a('%s', '%s', ...)\n", *lib, *name);
#endif
#ifdef WIN32
  error("teos()/gsw2a() does not work for the Windows OS (yet)");
#else
  if (first_teos_call) {
    teos_handle = dlopen(*lib, RTLD_LOCAL|RTLD_LAZY);
    first_teos_call = 0;
  }
  if (!teos_handle)
    error("cannot open TEOS library %s; error is: %s", *lib, dlerror());
#ifdef DEBUG
  Rprintf("%s:%d about to do try to find dlsym(handle, \"%s\"\n", __FILE__, __LINE__, *name);
#endif
  double (*f2)(double, double) = dlsym(teos_handle, *name);
  if (!f2) 
    error("cannot find \"%s\" in TEOS library %s; error is: %s", *name, *lib, dlerror());
#ifdef DEBUG
  Rprintf("%s:%d about to do the loop calling f2 (\"%s\")\n", __FILE__, __LINE__, *name);
#endif
  for (int i = 0; i < *n; i++) {
    //Rprintf("%s:%d in loop i=%d, a1[i]=%f, a2[i]=%f\n",__FILE__,__LINE__,i,a1[i],a2[i]);
    rval[i] = (*f2)(a1[i], a2[i]);
  }
#endif
}

void gsw3a(char **lib, char **name, int *n, double *a1, double *a2, double *a3, double *rval)
{
#ifdef DEBUG
  Rprintf("gsw3a('%s', '%s', ...)\n", *lib, *name);
#endif
#ifdef WIN32
  error("teos()/gsw3a() does not work for the Windows OS (yet)");
#else
  if (first_teos_call) {
    teos_handle = dlopen(*lib, RTLD_LOCAL|RTLD_LAZY);
    first_teos_call = 0;
  }
  if (!teos_handle)
    error("cannot open TEOS library %s; error is: %s", *lib, dlerror());
#ifdef DEBUG
  Rprintf("%s:%d about to do try to find dlsym(handle, \"%s\"\n", __FILE__, __LINE__, *name);
#endif
  double (*f3)(double, double, double) = dlsym(teos_handle, *name);
  if (!f3) 
    error("cannot find \"%s\" in TEOS library %s; error is: %s", *name, *lib, dlerror());
  for (int i = 0; i < *n; i++) {
    //Rprintf("%s:%d in loop i=%d, a1[i]=%f, a2[i]=%f a3[i]=%f\n",__FILE__,__LINE__,i,a1[i],a2[i],a3[i]);
    rval[i] = (*f3)(a1[i], a2[i], a3[i]);
  }
#endif
}

void gsw4a(char **lib, char **name, int *n, double *a1, double *a2, double *a3, double *a4, double *rval)
{
#ifdef DEBUG
  Rprintf("gsw4a('%s', '%s', ...)\n", *lib, *name);
#endif
#ifdef WIN32
  error("teos()/gsw4a() does not work for the Windows OS (yet)");
#else
  if (first_teos_call) {
    teos_handle = dlopen(*lib, RTLD_LOCAL|RTLD_LAZY);
    first_teos_call = 0;
  }
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
#endif
}

