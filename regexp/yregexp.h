/* $Id4
 * Definitions etc. for regexp(3) routines.
 *
 * Caveat:  this is V8 regexp(3) [actually, a reimplementation thereof],
 * not the System V one.
 */
#define NSUBEXP  10
typedef struct regexp {
  char *startp[NSUBEXP];
  char *endp[NSUBEXP];
  char regstart;       /* Internal use only. */
  char reganch;        /* Internal use only. */
  char *regmust;       /* Internal use only. */
  int regmlen;         /* Internal use only. */
  char program[1];     /* Unwarranted chumminess with compiler. */
} regexp;

#include "plugin.h"

PLUG_API regexp *regcomp_hs(char *exp);
PLUG_API int regexec_hs(regexp *prog, char *string);
PLUG_API void regsub_hs(regexp *prog, char *source, char *dest);

/* D. Munro added crude error query interface, nsubexp counter */
PLUG_API char *regerr_dm(void);
PLUG_API int regnpar_dm(void);
