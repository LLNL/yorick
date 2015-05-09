/* psoftfpe.c
 * raise pending software SIGFPE using fenv.h
 */
/* Copyright (c) 2015, David H. Munro.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "play.h"

#ifdef HAS_FENV_H

/* fenv.h is C99, signal.h is C89 */
#include <fenv.h>
#include <signal.h>

/* FE_INEXACT and FE_UNDERFLOW are not really exception conditions */
#define P_TRUE_EXCEPTIONS FE_DIVBYZERO | FE_INVALID | FE_OVERFLOW

/* code should use P_SOFTFPE_TEST macro, never call this directly */
void
p_softfpe(void)
{
  if (fetestexcept(P_TRUE_EXCEPTIONS)) {
    feclearexcept(P_TRUE_EXCEPTIONS);
    raise(SIGFPE);
  }
}

static fenv_t p_fenv;
static int p_valid_fenv = 0;
static int p_depth_fenv = 0;  /* zero means at p_fenv */

void
p_fpehandling(int on)
{
  p_valid_fenv = (p_valid_fenv || !fegetenv(&p_fenv));
  if (p_valid_fenv) {
    if (on) {
      if (on != 1) p_depth_fenv = 1;
      if (p_depth_fenv && !--p_depth_fenv)
        fesetenv(&p_fenv);
    } else {
      if (!p_depth_fenv++)
        fesetenv(FE_DFL_ENV);
    }
  }
}

#else

/* never invoked if code uses P_SOFTFPE_TEST macro as intended */
void
p_softfpe(void)
{
}

void
p_fpehandling(int on)
{
}

#endif
