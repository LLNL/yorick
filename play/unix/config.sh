#! /bin/sh
# $Id: config.sh,v 1.4 2006-10-06 05:28:42 dhmunro Exp $

debug=no

echo ""
echo "  ============= begin play/unix configuration ============="
echo ""

rm -f cfg* config.h

curdate=`date`
cursystem=`uname -a`
fatality=0
cat >config.h <<EOF
/* config.h used during config.sh script */
#ifndef CONFIG_SCRIPT
# error destroy this config.h and rerun configure script
#endif
EOF
cat >config.0h <<EOF
/* config.h from config.sh script $curdate
 * $cursystem
 * always empty -- see D_... defines in Make.cfg
 */
EOF
# should LD_LIBRARY_PATH, LIBPATH (AIX), LPATH, SHLIB_PATH (HPUX) be saved?

commonargs="-DCONFIG_SCRIPT $CFLAGS -I. -I.. $LDFLAGS -o cfg config.c"

# find CPU time function (getrusage is best if present)
args="-DTEST_UTIME $commonargs"
if $CC -DUSE_GETRUSAGE $args >cfg.01a 2>&1; then
  echo "using getrusage() (CPU timer)"
  echo "D_TIMEU=-DUSE_GETRUSAGE" >>../../Make.cfg
elif $CC -DUSE_TIMES $args >cfg.01b 2>&1; then
  echo "using times() (CPU timer)"
  echo "D_TIMEU=-DUSE_TIMES" >>../../Make.cfg
  if test $debug = no; then rm -f cfg.01a; fi
elif $CC $args >cfg.01c 2>&1; then
  echo "fallback to clock(), getrusage() and times() missing (CPU timer)"
  echo "D_TIMEU=" >>../../Make.cfg
else
  echo "FATAL getrusage(), times(), and clock() all missing (timeu.c)"
  fatality=1
fi

# find wall time function (gettimeofday is best if present)
args="-DTEST_WTIME $commonargs"
if $CC -DUSE_GETTIMEOFDAY $args >cfg.02a 2>&1; then
  echo "using gettimeofday() (wall timer)"
  echo "D_TIMEW=-DUSE_GETTIMEOFDAY" >>../../Make.cfg
elif $CC $args >cfg.02b 2>&1; then
  echo "fallback to time()+difftime(), gettimeofday() missing (wall timer)"
  echo "D_TIMEW=" >>../../Make.cfg
else
  echo "FATAL gettimeofday(), and time() or difftime() missing (timew.c)"
  fatality=1
fi

# find function to get user name
if test -z "$NO_PASSWD"; then
  args="-DTEST_USERNM $commonargs"
  if $CC $args >cfg.03a 2>&1; then
    echo "using POSIX getlogin(), getpwuid(), getuid() functions"
    echo "D_USERNM=" >>../../Make.cfg
  elif $CC -DNO_PASSWD $args >cfg.03b 2>&1; then
    echo "fallback to cuserid(), POSIX getlogin() family missing"
    echo "D_USERNM=-DNO_PASSWD" >>../../Make.cfg
  else
    echo "FATAL cuserid(), POSIX getlogin() family both missing (usernm.c)"
    fatality=1
  fi
else
  if test -z "$NO_CUSERID"; then
    echo "using cuserid(), POSIX getlogin() family missing"
    echo "D_USERNM=-DNO_PASSWD" >>../../Make.cfg
  else
    echo "using getenv(LOGNAME), cuserid(), POSIX getlogin() family missing"
    echo "D_USERNM=-DNO_PASSWD -DNO_CUSERID" >>../../Make.cfg
  fi
fi

# find function to get controlling terminal process group
if test -z "$NO_PROCS"; then
args="-DTEST_TIOCGPGRP $commonargs"
cargs="-DTEST_TIOCGPGRP -DCONFIG_SCRIPT $CFLAGS -I. -I.. $LDFLAGS -c config.c"
if $CC $cargs >cfg.04a 2>&1; then
  :
  echo "D_UINBG1=" >>../../Make.cfg
elif $CC -DUSE_POSIX_GETPGRP $cargs >cfg.04a 2>&1; then
  echo "using strict POSIX getpgrp prototype"
  args="-DUSE_POSIX_GETPGRP -DTEST_TIOCGPGRP $commonargs"
  echo "D_UINBG1=-DUSE_POSIX_GETPGRP" >>../../Make.cfg
fi
if $CC $args >cfg.04a 2>&1; then
  echo "using POSIX tcgetpgrp() function"
  echo "D_UINBG2=" >>../../Make.cfg
elif $CC '-DUSE_TIOCGPGRP_IOCTL=<sys/termios.h>' $args >cfg.04b 2>&1; then
  echo "fallback to TIOCGPGRP in sys/termios.h, POSIX tcgetpgrp() missing"
  echo "D_UINBG2=-DUSE_TIOCGPGRP_IOCTL=<sys/termios.h>" >>../../Make.cfg
elif $CC '-DUSE_TIOCGPGRP_IOCTL=<sgtty.h>' $args >cfg.04c 2>&1; then
  echo "fallback to TIOCGPGRP in sgtty.h, POSIX tcgetpgrp() missing"
  echo "D_UINBG2=-DUSE_TIOCGPGRP_IOCTL='<sgtty.h>'" >>../../Make.cfg
  if test $debug = no; then rm -f cfg.04b; fi
else
  echo "FATAL unable to find TIOCGPGRP ioctl header (uinbg.c)"
  fatality=1
fi
  echo "D_NO_PROCS=" >>../../Make.cfg
else
  echo "using no processes, tcgetpgrp(), popen(), system() missing"
  echo "D_UINBG1=-DNO_PROCS" >>../../Make.cfg
  echo "D_UINBG2=" >>../../Make.cfg
  echo "D_NO_PROCS=-DNO_PROCS" >>../../Make.cfg
fi

# find function to get current working directory
args="-DTEST_GETCWD $commonargs"
if $CC $args >cfg.05a 2>&1; then
  echo "using POSIX getcwd() function"
  echo "D_DIR1=" >>../../Make.cfg
elif $CC -DUSE_GETWD $args >cfg.05b 2>&1; then
  echo "fallback to getwd(), POSIX getcwd() missing"
  echo "D_DIR1=-DUSE_GETWD" >>../../Make.cfg
else
  echo "FATAL getcwd(), getwd() both missing (dir.c)"
  fatality=1
fi

# find headers required to read directories
args="-DTEST_DIRENT $commonargs"
if $CC $args >cfg.06a 2>&1; then
  echo "using POSIX dirent.h header for directory ops"
  echo "D_DIR2=" >>../../Make.cfg
elif $CC '-DDIRENT_HEADER=<sys/dir.h>' $args >cfg.06b 2>&1; then
  echo "using sys/dir.h header for directory ops"
  echo "D_DIR2=-DDIRENT_HEADER=<sys/dir.h>" >>../../Make.cfg
  if test $debug = no; then rm -f cfg.06a; fi
elif $CC '-DDIRENT_HEADER=<sys/ndir.h>' $args >cfg.06c 2>&1; then
  echo "using sys/ndir.h header for directory ops"
  echo "D_DIR2=-DDIRENT_HEADER=<sys/ndir.h>" >>../../Make.cfg
  if test $debug = no; then rm -f cfg.06a cfg.06b; fi
elif $CC '-DDIRENT_HEADER=<ndir.h>' $args >cfg.06d 2>&1; then
  echo "using ndir.h header for directory ops"
  echo "D_DIR2=-DDIRENT_HEADER=<ndir.h>" >>../../Make.cfg
  if test $debug = no; then rm -f cfg.06a cfg.06b cfg.06c; fi
else
  echo "FATAL dirent.h, sys/dir.h, sys/ndir.h, ndir.h all missing (dir.c)"
  fatality=1
fi

# find headers and functions required for poll/select functionality
# NO_PROCS=yes  produces crippled yorick that blocks waiting for stdin
# NO_POLL=yes   forces use of select(), otherwise poll() used if found
if test -z "$NO_PROCS"; then
  if test -x /usr/bin/sw_vers; then
    # Mac OS X 10.4 poll() is broken, but select() works
    swv="`/usr/bin/sw_vers -productVersion`"
    case "$swv" in
      # 10.4*) NO_POLL=yes ;;
      # even though poll works before 10.4, want yorick built on
      # 10.3 system to be able to run on a 10.4 system!
      10.*) NO_POLL=yes ;;
    esac
  fi
  if test -z "$NO_POLL"; then
    args="-DTEST_POLL $commonargs"
    if $CC $args >cfg.07a 2>&1; then
      echo "using poll(), poll.h header"
      echo "D_UEVENT=" >>../../Make.cfg
    elif $CC -DUSE_SYS_POLL_H $args >cfg.07b 2>&1; then
      echo "using poll(), sys/poll.h header"
      echo "D_UEVENT=-DUSE_SYS_POLL_H" >>../../Make.cfg
      if test $debug = no; then rm -f cfg.07a; fi
    else
      NO_POLL=yes
    fi
  fi
  if test -n "$NO_POLL"; then
    maxdefs="-DUSE_SELECT -DNO_SYS_TIME_H -DNEED_SELECT_PROTO"
    if $CC -DUSE_SELECT -DHAVE_SYS_SELECT_H $args >cfg.07c 2>&1; then
      echo "using select(), sys/select.h header"
      echo "D_UEVENT=-DUSE_SELECT -DHAVE_SYS_SELECT_H" >>../../Make.cfg
      if test $debug = no; then rm -f cfg.07a cfg.07b; fi
    elif $CC -DUSE_SELECT -DNEED_SELECT_PROTO $args >cfg.07d 2>&1; then
      echo "using select(), sys/time.h, sys/types.h headers"
      echo "D_UEVENT=-DUSE_SELECT -DNEED_SELECT_PROTO" >>../../Make.cfg
      if test $debug = no; then rm -f cfg.07[a-c]; fi
    elif $CC $maxdefs $args >cfg.07e 2>&1; then
      echo "using select(), time.h, sys/types.h headers"
      echo "D_UEVENT=$maxdefs" >>../../Make.cfg
      if test $debug = no; then rm -f cfg.07[a-d]; fi
    elif $CC -DUSE_SELECT $args >cfg.07f 2>&1; then
      echo "using select(), sys/time.h header"
      echo "D_UEVENT=-DUSE_SELECT" >>../../Make.cfg
      if test $debug = no; then rm -f cfg.07[a-e]; fi
    elif $CC -DUSE_SELECT -DNO_SYS_TIME_H $args >cfg.07g 2>&1; then
      echo "using select(), time.h header"
      echo "D_UEVENT=-DUSE_SELECT -DNO_SYS_TIME_H" >>../../Make.cfg
      if test $debug = no; then rm -f cfg.07[a-f]; fi
    else
      echo "FATAL neither poll() nor select() usable? (uevent.c, upoll.c)"
      fatality=1
    fi
  fi
else
# NO_PROCS is not really related to NO_POLLING, but is intended for
# the catamount parallel kernel, which is missing both process
# fork and select/poll
  echo "missing poll() and select(), using blocking stdin"
  echo "D_UEVENT=-DUSE_SELECT -DNO_POLLING" >>../../Make.cfg
fi

if test -z "$NO_SOCKETS"; then
  args="-DTEST_SOCKETS $commonargs"
  if $CC $args >cfg.10 2>&1; then
    echo "using BSD sockets"
    echo "D_NO_SOCKETS=" >>../../Make.cfg
    if test $debug = no; then rm -f cfg.10; fi
  else
    echo "missing BSD sockets, socket interface disabled"
    echo "D_NO_SOCKETS=-DNO_SOCKETS" >>../../Make.cfg
  fi
else
  echo "skipping sockets, socket interface disabled"
  echo "D_NO_SOCKETS=-DNO_SOCKETS" >>../../Make.cfg
fi

#----------------------------------------------------------------------
# try to figure out how to get SIGFPE delivered
#----------------------------------------------------------------------
args="-DCONFIG_SCRIPT $CFLAGS -I. -I.. $LDFLAGS -o fputest fputest.c"
fpedef=
fpelib=
fpelibm=
if test -n "$FPU_IGNORE"; then
  fpedef=
elif $CC -DFPU_DIGITAL $args >cfg.08 2>&1; then
  echo "using FPU_DIGITAL (SIGFPE delivery)"
  fpedef=-DFPU_DIGITAL
elif $CC -DFPU_AIX $args >cfg.08 2>&1; then
  echo "using FPU_AIX (SIGFPE delivery)"
  fpedef=-DFPU_AIX
elif $CC -DFPU_HPUX $args $MATHLIB >cfg.08 2>&1; then
  echo "using FPU_HPUX (SIGFPE delivery)"
  fpedef=-DFPU_HPUX
  fpelibm=$MATHLIB
elif $CC -DFPU_SOLARIS $args >cfg.08 2>&1; then
  echo "using FPU_SOLARIS (SIGFPE delivery)"
  fpedef=-DFPU_SOLARIS
  # note this works under IRIX 6.3, while FPU_IRIX does not??
elif $CC -DFPU_SUN4 $args $MATHLIB >cfg.08 2>&1; then
  echo "using FPU_SUN4 (-lm) (SIGFPE delivery)"
  fpedef=-DFPU_SUN4
  fpelibm=$MATHLIB
elif $CC -DFPU_SUN4 $args -lsunmath >cfg.08 2>&1; then
  echo "using FPU_SUN4 (-lsunmath) (SIGFPE delivery)"
  fpedef=-DFPU_SUN4
  fpelib=-lsunmath
elif $CC -DFPU_IRIX $args -lfpe >cfg.08 2>&1; then
  # FPU_SOLARIS seems to work better??
  echo "using FPU_IRIX (SIGFPE delivery)"
  fpedef=-DFPU_IRIX
  fpelib=-lfpe
elif $CC -DFPU_IRIX $args >cfg.08 2>&1; then
  echo "using FPU_IRIX (SIGFPE delivery), but no libfpe??"
  fpedef=-DFPU_IRIX
elif $CC -DFPU_GCC_X86_64 $args >cfg.08 2>&1; then
  echo "using FPU_GCC_X86_64 (SIGFPE delivery)"
  fpedef=-DFPU_GCC_X86_64
elif $CC -DFPU_GCC_X86 $args >cfg.08 2>&1; then
  echo "using FPU_GCC_X86 (SIGFPE delivery)"
  fpedef=-DFPU_GCC_X86
elif $CC -DFPU_MACOSX $args >cfg.08 2>&1; then
  echo "using FPU_MACOSX (SIGFPE delivery)"
  fpedef=-DFPU_MACOSX
elif $CC -DFPU_GNU_FENV $args $MATHLIB >cfg.08 2>&1; then
  echo "using FPU_GNU_FENV (SIGFPE delivery)"
  fpedef=-DFPU_GNU_FENV
  fpelibm=$MATHLIB
elif $CC -DFPU_UNICOS $args $MATHLIB >cfg.08 2>&1; then
  echo "using FPU_UNICOS (SIGFPE delivery)"
  fpedef=-DFPU_UNICOS
  fpelibm=$MATHLIB
elif $CC -DTEST_GCC $commonargs >cfg.08 2>&1; then
  if $CC -DFPU_ALPHA_LINUX $args >cfg.08 2>&1; then
    echo "using FPU_ALPHA_LINUX (SIGFPE delivery)"
    fpedef=-DFPU_ALPHA_LINUX
    echo "...libm may be broken -- read play/unix/README.fpu for more"
    echo "...fputest failure may not mean that yorick itself is broken"
    # CC="$CC -mfp-trap-mode=su -mtrap-precision=i"
  elif $CC -DFPU_GCC_I86 $args >cfg.08 2>&1; then
    echo "using FPU_GCC_I86 (SIGFPE delivery)"
    fpedef=-DFPU_GCC_I86
  elif $CC -DFPU_GCC_SPARC $args >cfg.08 2>&1; then
    echo "using FPU_GCC_SPARC (SIGFPE delivery)"
    fpedef=-DFPU_GCC_SPARC
  elif $CC -DFPU_GCC_M68K $args >cfg.08 2>&1; then
    echo "using FPU_GCC_M68K (SIGFPE delivery)"
    fpedef=-DFPU_GCC_M68K
  elif $CC -DFPU_GCC_POWERPC $args >cfg.08 2>&1; then
    echo "using FPU_GCC_POWERPC (SIGFPE delivery)"
    fpedef=-DFPU_GCC_POWERPC
  elif $CC -DFPU_GCC_ARM $args >cfg.08 2>&1; then
    echo "using FPU_GCC_ARM (SIGFPE delivery)"
    fpedef=-DFPU_GCC_ARM
  fi
fi
if test -z "$fpedef"; then
  if $CC -DFPU_IGNORE $args $MATHLIB >cfg.08 2>&1; then
    echo "using FPU_IGNORE (SIGFPE delivery)"
    fpedef=-DFPU_IGNORE
  else
    echo "FATAL unable to build SIGFPE fputest? (fputest.c, fpuset.c)"
    fatality=1
  fi
fi
echo "D_FPUSET=$fpedef" >>../../Make.cfg
if test -z "$fpelib"; then
  echo "FPELIB=" >>../../Make.cfg
else
  echo "FPELIB=$fpelib" >>../../Make.cfg
fi
if test -z "$fpelibm"; then
  echo "FPELIBM=" >>../../Make.cfg
else
  echo "FPELIBM=$fpelibm" >>../../Make.cfg
fi
if test -n "$fpedef"; then
  # on IRIX be sure that TRAP_FPE environment variable is turned off
  unset TRAP_FPE
  if ./fputest; then
    :
  else
    echo ""
    echo "*************************WARNING***************************"
    echo "*** play/unix configuration failed to get SIGFPE delivered"
    echo "*** read the notes in play/unix/README.fpu"
    echo "*************************WARNING***************************"
    echo ""
  fi
fi
rm -f fputest

# check if fenv.h present
softfpe=
args="-DTEST_FENV_H $commonargs"
if $CC $args $MATHLIB >cfg.11 2>&1; then
  echo "found fenv.h header"
  echo "D_HAS_FENV_H=-DHAS_FENV_H" >>../../Make.cfg
  if test "$fpedef" = "-DFPU_IGNORE"; then
    echo "...activating software SIGFPE support"
    softfpe=-DUSE_SOFTFPE
  fi
  if test $debug = no; then rm -f cfg.11; fi
else
  echo "WARNING fenv.h missing, no floating point environment control"
  echo "D_HAS_FENV_H=" >>../../Make.cfg
fi
echo "D_USE_SOFTFPE=$softfpe" >>../../Make.cfg

#----------------------------------------------------------------------
# try to figure out how dynamic linking for plugins works
#----------------------------------------------------------------------

PLUG_DEF=""
os_name=`(uname -s) 2>/dev/null` || os_name=unknown
case "$os_name" in
  AIX)        # IBM RS/6000 (powerpc) architecture
              # no such thing as non-PIC so no PLUG_PIC flag
              # -bexpall fails for symbols beginning with _   <--WARNING
    PLUG_UDL=-DPLUG_LIBDL
    PLUG_EXPORT='-Wl,-bexpall,-brtl'
    PLUG_LIB=-ldl
    PLUG_PIC=
    PLUG_SHARED=-G
    PLUG_SFX=.so
  ;;
  IRIX*)       # SGI MIPS architecture
               # PLUG_EXPORT with -Wl,-hidden_symbol ??
    PLUG_UDL=-DPLUG_LIBDL
    PLUG_EXPORT=
    PLUG_LIB=
    PLUG_PIC=
    PLUG_SHARED=-shared
    PLUG_SFX=.so
  ;;
  SunOS)      # Sun SPARC architecture
    PLUG_UDL=-DPLUG_LIBDL
    PLUG_EXPORT=
    PLUG_LIB=-ldl
    PLUG_PIC=-KPIC
    PLUG_SHARED="-KPIC -G"
    PLUG_SFX=.so
  ;;
  OSF1)       # DEC/Compaq/HP alpha architecture
              # PLUG_EXPORT with -Wl,-non_hidden ??
    PLUG_UDL=-DPLUG_LIBDL
    PLUG_EXPORT=
    PLUG_LIB=
    PLUG_PIC=
    PLUG_SHARED="-shared -Wl,-expect_unresolved,*"
    PLUG_SFX=.so
  ;;
  HP-UX)      # HP PA-RISC architecture
    PLUG_UDL=-DPLUG_HPUX
    PLUG_EXPORT=-Wl,-E
    PLUG_LIB=-ldld
    PLUG_PIC=+Z
    PLUG_SHARED=-n
    PLUG_SFX=.sl
  ;;
  Darwin)     # Apple MacOS X powerpc architecture (ignore i86 versions?)
    # for Mac OS X 10.3 and earlier, use -DPLUG_MACOSX
    # see https://developer.apple.com/library/mac/#qa/qa1180/_index.html
    PLUG_UDL=-DPLUG_LIBDL
    PLUG_EXPORT=
    PLUG_LIB=
    PLUG_PIC=
    PLUG_SHARED="-bundle -bundle_loader ./cfg"
    PLUG_SFX=.so
  ;;
  CYGWIN*)    # CygWin i86 UNIX environment for MS Windows
    PLUG_UDL=-DPLUG_LIBDL
    PLUG_EXPORT=
    PLUG_LIB=
    PLUG_PIC=-DPLUG_IN
    PLUG_SHARED=-shared
    PLUG_SFX=.dll
    PLUG_DEF=cygyor.def
  ;;
  *)          # Linux, Free/Net/OpenBSD i86 architecture
              # these are generic GNU/gcc values, also work for Intel/icc
    PLUG_UDL=-DPLUG_LIBDL
    PLUG_EXPORT=-Wl,-E
    PLUG_LIB=-ldl
    PLUG_PIC=-fPIC
    PLUG_SHARED="-fPIC -shared"
    PLUG_SFX=.so
  ;;
esac

# command to link plugin
LD_PLUGIN="$CC $CFLAGS $LDFLAGS $PLUG_SHARED"

# no compiler switch to make Mac OS X .dylib
# need special env var to be able to call things in loading program
# none of this is necessary with -bundle instead of .dylib
# Makefile needs two lines to link with either CC or libtool
#PLUG_UNIX=""
#PLUG_OSX=":"
#  export MACOSX_DEPLOYMENT_TARGET
#  MACOSX_DEPLOYMENT_TARGET=10.3
#  LD_PLUGIN="libtool -dynamic $PLUG_SHARED"
#  PLUG_UNIX=":"
#  PLUG_OSX='MACOSX_DEPLOYMENT_TARGET=10.3 libtool -dynamic $(PLUG_SHARED)'

# find functions to do dynamic linking for plugins
# first check that TEST_PLUG branch of config.c/udltest.c compiles and loads
#   this checks PLUG_UDL, PLUG_EXPORT, and PLUG_LIB settings
dl_works=no
if test -z "$NO_PLUGINS"; then
args="$CFLAGS $PLUG_EXPORT $PLUG_UDL -I. $LDFLAGS -o cfg config.c"
if $CC -DTEST_PLUG $args $PLUG_LIB >cfg.09 2>&1; then
  # next check that PLUG_PIC flag works, so can build shareable object code
  args="$CFLAGS $PLUG_PIC"
  if $CC -DTEST_SHARED $args -c udltest.c >cfg.09a 2>&1; then
    # next check that PLUG_SHARED flag works, so can build dynamic libraries
    if $LD_PLUGIN -o udltest$PLUG_SFX $PLUG_DEF udltest.o >cfg.09b 2>&1; then
      # finally check that dynamically linked main program runs
      if ./cfg; then
        echo "configured plugins for $os_name"
        dl_works=yes
      else
        echo "*** WARNING *** no plugins - shared lib run failed"
      fi
    else
      echo "*** WARNING *** no plugins - shared lib link failed"
    fi
    rm -f udltest$PLUG_SFX
  else
    echo "*** WARNING *** no plugins - shared object compile failed"
  fi
  rm -f udltest.o

else
  echo "*** WARNING *** no plugins - dynamic linking main failed to build"
fi
fi

ALT_LIBS=
IF_TGT='$(IF_DLL)'
if test $dl_works = no; then
  PLUG_UDL=-DPLUG_UNSUPPORTED
  PLUG_EXPORT=
  PLUG_LIB=
  PLUG_PIC=
  PLUG_SHARED=
  PLUG_DEF=
  echo "DEFAULT_TGT=exe" >>../../Make.cfg
  ALT_LIBS='$(DIST_LIBS)'
  IF_TGT='$(IF_EXE)'
else
  case "$os_name" in
    AIX)
      PLUG_EXPORT='-Wl,-bE:$(Y_LIBEXE)/yorapi.def,-brtl'
    ;;
    OSF1)
      # fix quoting to account for difference between make and sh variable
      PLUG_SHARED="-shared -Wl,-expect_unresolved,'*'"
      rm -f so_locations
    ;;
    Darwin)
      # fix exe name for Makefiles
      PLUG_SHARED='-bundle -bundle_loader $(Y_EXE)'
    ;;
  esac
  echo "DEFAULT_TGT=dll" >>../../Make.cfg
fi
if test "$LD_STATIC" = yes; then
  ALT_LIBS='$(DIST_LIBS)'
  IF_TGT='$(IF_EXE)'
  echo "DISTRIB_TGT=exe" >>../../Make.cfg
else
  echo 'DISTRIB_TGT=$(DEFAULT_TGT)' >>../../Make.cfg
fi
echo "ALT_LIBS=$ALT_LIBS" >>../../Make.cfg
echo "IF_TGT=$IF_TGT" >>../../Make.cfg

echo "PLUG_UDL=$PLUG_UDL" >>../../Make.cfg
echo "PLUG_EXPORT=$PLUG_EXPORT" >>../../Make.cfg
echo "PLUG_LIB=$PLUG_LIB" >>../../Make.cfg
echo "PLUG_PIC=$PLUG_PIC" >>../../Make.cfg
echo "PLUG_SHARED=$PLUG_SHARED" >>../../Make.cfg
echo "PLUG_SFX=$PLUG_SFX" >>../../Make.cfg
echo "PLUG_DEF=$PLUG_DEF" >>../../Make.cfg

#----------------------------------------------------------------------

# clean up, issue warning if compiler gave fishy output
rm -f config.h config.o cfg cfg.c cfg.o
for f in cfg.[0-9]*; do
  if grep ... $f >/dev/null 2>&1; then   # or use test -s $f ?
    echo "WARNING - check compiler message in $f"
  else # remove empty files
    rm -f $f
  fi
done

if test $fatality = 1; then
  echo "*** at least one play/unix component could not be configured"
  echo "*** see configuration notes in play/unix/README.cfg"
else
  mv config.0h config.h
  echo "wrote config.h, ../../Make.cfg"
fi

echo ""
echo "  ============== end play/unix configuration =============="
exit $fatality
