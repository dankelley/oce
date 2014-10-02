#include <R.h>
/* print projection's list of parameters */
#include "projects.h"
#include <stdio.h>
#include <string.h>
#define LINE_LEN 72
	static int
pr_list(PJ *P, int not_used) {
	paralist *t;
	int l, n = 1, flag = 0;

	//(void)putchar('#');
        Rprintf("#");
	for (t = P->params; t; t = t->next)
		if ((!not_used && t->used) || (not_used && !t->used)) {
			l = strlen(t->param) + 1;
			if (n + l > LINE_LEN) {
                                //(void)fputs("\n#", stdout);
                                Rprintf("\n#");
				n = 2;
			}
			//(void)putchar(' ');
                        Rprintf(" ");
			if (*(t->param) != '+') {
				//(void)putchar('+');
                                Rprintf("+");
                        }
			//(void)fputs(t->param, stdout);
                        Rprintf(t->param);
			n += l;
		} else
			flag = 1;
	if (n > 1) {
		//(void)putchar('\n');
                Rprintf("\n");
        }
	return flag;
}
	void /* print link list of projection parameters */
pj_pr_list(PJ *P) {
	char const *s;

	//(void)putchar('#');
        Rprintf("#");
	for (s = P->descr; *s ; ++s) {
		//(void)putchar(*s);
                Rprintf("%c", *s);
		if (*s == '\n') {
			//(void)putchar('#');
                        Rprintf("#");
                }
	}
	//(void)putchar('\n');
        Rprintf("\n");
	if (pr_list(P, 0)) {
		//(void)fputs("#--- following specified but NOT used\n", stdout);
                Rprintf("#--- following specified but NOT used\n");
		(void)pr_list(P, 1);
	}
}

/************************************************************************/
/*                             pj_get_def()                             */
/*                                                                      */
/*      Returns the PROJ.4 command string that would produce this       */
/*      definition expanded as much as possible.  For instance,         */
/*      +init= calls and +datum= definitions would be expanded.         */
/************************************************************************/

char *pj_get_def( PJ *P, int options )

{
    paralist *t;
    int l;
    char *definition;
    int  def_max = 10;

    definition = (char *) pj_malloc(def_max);
    definition[0] = '\0';

    for (t = P->params; t; t = t->next)
    {
        /* skip unused parameters ... mostly appended defaults and stuff */
        if (!t->used)
            continue;

        /* grow the resulting string if needed */
        l = strlen(t->param) + 1;
        if( strlen(definition) + l + 5 > def_max )
        {
            char *def2;

            def_max = def_max * 2 + l + 5;
            def2 = (char *) pj_malloc(def_max);
            strcpy( def2, definition );
            pj_dalloc( definition );
            definition = def2;
        }

        /* append this parameter */
        strcat( definition, " +" );
        strcat( definition, t->param );
    }

    return definition;
}
