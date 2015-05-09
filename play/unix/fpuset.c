/*
 * $Id: fpuset.c,v 1.6 2007-11-08 16:34:30 dhmunro Exp $
 * set up FPU to trap floating point exceptions
 * - this is very non-portable, not covered by ANSI C, POSIX, or even C9X
 * - if you port to a new platform (eg- Ultrix) please contact the author
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef FROM_FPUTEST
# include "config.h"
# include "playu.h"
#else
extern void u_fpu_setup(int when);
#endif

/* when = -1 for initial call before setjmp
 *         0 after each longjmp out of interrupt handler (after setjmp)
 *         1 inside interrupt handler before signal() re-enables SIGFPE
 */

#ifdef FPU_GCC_X86_64
# define FPU_GCC_X86
#endif

#if defined(FPU_DIGITAL) || defined(FPU_ALPHA_LINUX)

/* FPU_ALPHA_LINUX: see README.fpu */
/* man pages: exception_intro, ieee */
# ifdef FPU_DIGITAL
#  include <machine/fpu.h>
# else
   extern void ieee_set_fp_control(long);
#  define IEEE_TRAP_ENABLE_INV 0x000002
#  define IEEE_TRAP_ENABLE_DZE 0x000004
#  define IEEE_TRAP_ENABLE_OVF 0x000008
#  define IEEE_MAP_DMZ         (1UL<<12)
#  define IEEE_MAP_UMZ         (1UL<<13)
# endif
void
u_fpu_setup(int when)
{
  /* possibly should include IEEE_MAP_DMZ and IEEE_MAP_UMZ
   * to map denorm inputs and underflowed outputs to zero
   * --however, these apparently only have an effect for software
   * completed operations (the hardware always maps underflows to zero)
   */
  if (when < 0) {
    ieee_set_fp_control(IEEE_TRAP_ENABLE_INV | IEEE_TRAP_ENABLE_DZE |
                        IEEE_TRAP_ENABLE_OVF);
  }
}

#elif defined(FPU_AIX)

/* man pages: fp_trap, fp_enable */
#include <fptrap.h>
void
u_fpu_setup(int when)
{
  if (when) {
    fp_trap(FP_TRAP_FASTMODE);
    fp_enable(TRP_INVALID | TRP_DIV_BY_ZERO | TRP_OVERFLOW);
  }
}

#elif defined(FPU_HPUX)

/* man pages: fpsetmask
 * library: -lm */
/* HPUX turns off FP_X_* without this (_INCLUDE_HPUX_SOURCE) */
#ifndef _HPUX_SOURCE
#define _HPUX_SOURCE 1
#endif
#include <math.h>
void
u_fpu_setup(int when)
{
  if (when <= 0) {
    fpsetmask(FP_X_INV | FP_X_DZ | FP_X_OFL);  /* 0x1c */
    fpsetfastmode(1);    /* fast underflows */
  }
}

#elif defined(FPU_IRIX)

/* man pages: handle_sigfpes, note lethal TRAP_FPE environment variable
 * library: -lfpe
 * note: earlier versions used get_fpc_csr/set_fpc_csr?, sys/fpu.h */
#include <sigfpe.h>
void
u_fpu_setup(int when)
{
  if (when < 0) {
    extern void u_sigfpe(int sig);  /* from handler.c (or fputest.c) */
    handle_sigfpes(_ON, _EN_OVERFL|_EN_DIVZERO|_EN_INVALID,
                   (void (*)())0, _USER_HANDLER, (void (*)())&u_sigfpe);
  }
}

#elif defined(FPU_SOLARIS)

/* man pages: fpsetmask
 *    Sun's -fnonstd compiler switch switches between __fnonstd.o
 *      and __fstd.o under Solaris, as far as I can tell.  Use FPU_IGNORE
 *        if you do this.  */
#include <ieeefp.h>
void
u_fpu_setup(int when)
{
  if (when < 0) {
    fpsetmask(FP_X_INV | FP_X_DZ | FP_X_OFL);
    /* this doesn't set the "nonstandard arithmetic" bit, which prevents
     * software emulation of IEEE gradual underflow
     * -- apparently no way to do this in libc (see FPU_GCC_SPARC) */
  }
}

#elif defined(FPU_SUN4)

/* man pages: ieee_handler
 *               nonstandard_arithmetic is undocumented, but rumored
 *               to be important to get rapid underflows
 * library: -lsunmath (under /usr/lang hierarchy)
 *          may also be in -lm (standard libm)?
 *   note: libsunmath.a is provided by Sun only if you purchase their
 *         compilers; if you are trying to compile with gcc on a SPARC
 *         architecture, try FPU_GCC_SPARC
 *    Sun's -fnonstd compiler switch buggers crt1.o under SunOS 4,
 *      as far as I can tell.  Use FPU_IGNORE if you do this
 *      (not possible with gcc?).  */
void
u_fpu_setup(int when)
{
  if (when < 0) {
    extern void u_sigfpe(int sig);  /* from handler.c (or fputest.c) */
    nonstandard_arithmetic();
    ieee_handler("set","common", &u_sigfpe);
  }
}

#elif defined(FPU_UNICOS)

/* delivers SIGFPE by default, this just arranges to trap on
 * libm errors as well */
void
u_fpu_setup(int when)
{
  if (when < 0) {
    int flag = -1;
    libmset(&flag);
  }
}

#elif defined(FPU_GCC_X86)
/* This branch is now preferred over FPU_GNU_FENV or FPU_GNU_I86 for
 * modern Intel Pentium and AMD machines.  The GNU fenv.h extension
 * will unmask interrupts properly, but provides no means for setting
 * the flush-to-zero and denormals-are-zero bits required for high
 * performance with the SSE/SSE2 floating point unit.
 */
# undef X86_PREFIX
# ifdef FPU_GCC_X86_64
#  define X86_PREFIX "r"
# else
#  define X86_PREFIX "e"
# endif

static unsigned int u_fpu_detect(void);
static unsigned int u_mxcsr_mask = 0;
static unsigned int u_fpu_features = 0x8000;
/* 0x001   x87 fpu on chip
 * 0x002   mmx technology
 * 0x004   fxsave/fxrestor instructions present
 * 0x008   sse extensions
 * 0x010   sse2 extensions
 * 0x020   sse3 extensions
 * 0x040   daz supported
 */
static unsigned int
u_fpu_detect(void)
{
  unsigned int features = 0;
  unsigned int x, y;
  __asm __volatile ("pushf\n\t pop %%"X86_PREFIX"ax" : "=a" (x));
  y = x;          /* set x and y to original value of eflags */
  x ^= 0x200000;  /* flip bit 21, ID */
  __asm __volatile ("push %%"X86_PREFIX"ax\n\t popf" : : "a" (x));
  __asm __volatile ("pushf\n\t pop %%"X86_PREFIX"ax" : "=a" (x));
  if (x ^ y) {
    unsigned long ecx, edx;
    /* this cpu has the cpuid instruction, restore original eflags */
    __asm __volatile ("push %%"X86_PREFIX"ax\n\t popf" : : "a" (y));
    /* get mmx, sse related feature bits from cpuid */
    __asm __volatile ("mov %%"X86_PREFIX"bx, %%"X86_PREFIX"si \n\t"
                      "cpuid \n\t"
                      "xchg %%"X86_PREFIX"bx, %%"X86_PREFIX"si"
                      : "=c" (ecx), "=d" (edx) : "a" (1) : "si");
    features = (edx & 1) | ((edx & 0x7800000) >> 22) | ((ecx & 1) >> 5);
    if ((features & 0x004) && (features & 0x018)) {
      /* get mxcsr_mask to find out if DAZ supported */
      unsigned char pstate[528], *state;
      int i;
      for (state=pstate,i=0 ; i<528 ; i++) *state++ = '\0';
      /* get a 16-byte aligned state buffer for fxsave */
      state = pstate + 15;
      state = ((state - (unsigned char *)0)&(~0xfL)) + (unsigned char *)0;
      __asm __volatile ("fxsave %0" : : "m" (*state));
      u_mxcsr_mask = *((unsigned int *)(state + 28));
      if (!u_mxcsr_mask) u_mxcsr_mask = 0xffbf;
      else if (u_mxcsr_mask & 0x40) features |= 0x040;
    }
  }
  return features;
}

/* interrupt mask bits for x87 and sse (xmm) fpu's */
/* typical initial values: fctrl=0x037f  mxcsr=0x1f80 */
#define U_FPU_X87CW 0x1372
#define U_FPU_MXCSR 0x9940
/*   bit meaning         x87      sse      yes=mask=1, no=deliver=0
 * flush to zero          --     0x8000       yes
 * infinity control     0x1000     --    (for 287 coprocessor compatibility)
 * rounding control     0x0c00   0x6000        0
 * precison control     0x0300     --    (0x0300 means 64-bit extended)
 * precision mask       0x0020   0x1000       yes
 * underflow mask       0x0010   0x0800       yes
 * overflow mask        0x0008   0x0400       no
 * zero divide mask     0x0004   0x0200       no
 * denormal op mask     0x0002   0x0100       yes
 * invalid op mask      0x0001   0x0080       no
 * denormals are zero     --     0x0040       yes
 *   sse exception bits 0x003f correspond to mask bits 0x1f80  (>>7)
 */
void
u_fpu_setup(int when)
{
  if (u_fpu_features == 0x8000)
    u_fpu_features = u_fpu_detect();
# if defined(__CYGWIN__)
  __asm__ ("fclex" : : );  /* clear i87 fp exception bits */
  if (u_mxcsr_mask) {
    /* clear sse fp exception bits as a side-effect */
    unsigned int mxcsr = U_FPU_MXCSR & u_mxcsr_mask;
    __asm__ ("ldmxcsr %0" : : "m" (mxcsr));
  }
  if (when<0) {
# elif defined(__NeXT)
  if (when<=0) {
# else
  if (when) {
# endif
    unsigned int fpucw = U_FPU_X87CW;
    __asm__ ("fldcw %0" : : "m" (fpucw));
# if ! defined(__CYGWIN__)
    if (u_mxcsr_mask) {
      unsigned int mxcsr = U_FPU_MXCSR & u_mxcsr_mask;
      __asm__ ("ldmxcsr %0" : : "m" (mxcsr));
      /* get mxscr: __asm__ ("stmxcsr %0" : : "m" (mxcsr)); */
    }
# endif
  }
}

#elif defined(FPU_GNU_FENV)

/* GCC enhanced C9X fenv.h interface by adding feenableexcept */
#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#include <fenv.h>
void
u_fpu_setup(int when)
{
  if (when <= 0)
    feenableexcept(FE_DIVBYZERO | FE_OVERFLOW | FE_INVALID);
}

#elif defined(FPU_GCC_I86)

/* see also: fpu_control.h or i386/fpu_control.h, __setfpucw function */
void
u_fpu_setup(int when)
{
# if defined(__CYGWIN__)
  __asm__ ("fclex" : : );
  if (when<0) {
# elif defined(__NeXT)
  if (when<=0) {
# else
  if (when) {
# endif
    unsigned int fpucw = 0x1372;
# ifndef __CYGWIN__
    /* CygWin currently broken (20/Aug/04) so treat as FPU_IGNORE */
    __asm__ ("fldcw %0" : : "m" (fpucw));
# endif
  }
}

#elif defined(FPU_GCC_POWERPC)

void
u_fpu_setup(int when)
{
  if (when) {
    unsigned int tmp[2] __attribute__ ((__aligned__(8)));
    tmp[0] = 0xFFF80000; /* More-or-less arbitrary; this is a QNaN. */
    tmp[1] = 0xd0;
    __asm__ ("lfd 0,%0; mtfsf 255,0" : : "m" (*tmp) : "fr0");
  }
}

#elif defined(FPU_GCC_SPARC)

void
u_fpu_setup(int when)
{
  if (when < 0) {
    unsigned int fpucw = 0xd400000;  /* the 4 is nonstandard arithmetic bit */
    __asm__ ("ld %0,%%fsr" : : "m" (fpucw));
  }
}

#elif defined(FPU_GCC_M68K)

/* works on NeXT as well as m68k Linux */
void
u_fpu_setup(int when)
{
  if (when <= 0) {
    asm("fmovel     #0x7400,fpcr");   /* set OVFL and ZD bits */
    /* unsigned int fpucw = 0x7400;
     * __asm__ volatile ("fmove%.l %0, %!" : : "dm" (fpucw)); */
    /* includes bit to trap on signalling NaN (may affect libm behavior) */
  }
}

#elif defined(FPU_GCC_ARM)

void
u_fpu_setup(int when)
{
  if (when <= 0) {
    unsigned int fpucw = 0x70200;
    __asm__ ("wfs %0" : : "r" (fpucw));
    /* includes bit to trap on signalling NaN (may affect libm behavior) */
  }
}

#elif defined(FPU_IGNORE)

void
u_fpu_setup(int when)
{
}

#elif defined(FPU_MACOSX)

/* MacOS X 10.3.0, 10.3.1, and 10.3.2 (at least) shipped with
 * broken libm (/usr/lib/libm.dylib), for which the functions
 * sqrt, tanh, sinh cause incorrect SIGFPE for legal arguments
 * when FPEs are enabled.  See README.fpu for more information.
 */

#include <architecture/ppc/fp_regs.h>

# ifdef FPU_MACOSX_10_1
/* for Darwin version 6.0 (MacOS X 10.2) FE0=FE1=1 initially
 * for Darwin version 5.5 (MacOS X <=10.1) FE0=FE1=0 initially
 * Darwin 5.5 resets MSR to FE0=FE1=0 after each SIGFPE
 * A thread cannot set its own MSR, so we have to create a second thread
 * to change our MSR to a value which permits FPE unmasking bits in SCR
 * to have any effect (yuck).
 */
#include <mach/mach.h>
#include <pthread.h>

static void *fpu_fpe_enable(void *arg);
#define FE0_MASK (1<<11)
#define FE1_MASK (1<<8)
/* FE0  FE1   exceptions enabled if either FE0 or FE1 set
 *  0    0    -- floating-point exceptions disabled
 *  0    1    -- floating-point imprecise nonrecoverable
 *  1    0    -- floating-point imprecise recoverable
 *  1    1    -- floating-point precise mode
 */

/* a thread cannot get or set its own MSR bits */
static void *
fpu_fpe_enable(void *arg)
{
  thread_t t = *(thread_t *)arg;
  struct ppc_thread_state state;
  unsigned int state_size = PPC_THREAD_STATE_COUNT;
  if (thread_get_state(t, PPC_THREAD_STATE,
                       (natural_t *)&state, &state_size) == KERN_SUCCESS) {
    state.srr1 |= FE1_MASK;
    state.srr1 &= ~FE0_MASK;
    thread_set_state(t, PPC_THREAD_STATE, (natural_t *)&state, state_size);
  }
  return 0;
}
# endif

void
u_fpu_setup(int when)
{
  static volatile int looping = 0;
  if (when) {
    ppc_fp_scr_t r = get_fp_scr();
    /* turn off exception bits to prevent immediate re-fault */
    r.fx = r.fex = r.vx = r.ox = r.ux = r.zx = r.xx = r.vx_snan = r.vx_isi =
      r.vx_idi = r.vx_zdz = r.vx_imz = r.vx_xvc = r.vx_cvi = r.vx_soft = 0;
    /* rsvd2 is actually vx_sqrt, set by fsqrt instruction
     * fsqrt is optional, not present on G4 and earlier Macs (but on G5)
     */
    r.rsvd2 = 0;
    /* these only have to be set once, but may as well set anyway */
    r.ve = 1;  /* invalid */
    r.oe = 1;  /* overflow */
    r.ue = 0;  /* underflow */
    r.ze = 1;  /* zero divide */
    r.xe = 0;  /* inexact */
    if (!looping) {
      looping |= 1;
      set_fp_scr(r);
      looping &= ~1;
    }
  }
# ifdef FPU_MACOSX_10_1
  if (when <= 0) {
    thread_t self = mach_thread_self();
    pthread_t enabler;
    if (!looping) {
      looping |= 2;
      if (!pthread_create(&enabler, 0, fpu_fpe_enable, &self))
        pthread_join(enabler, 0);
      looping &= ~2;
    }
  }
# endif
  looping = 0;
}

#else

#error <read play/unix/README.fpu for help>

#endif
