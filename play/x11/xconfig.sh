#! /bin/sh
# $Id: xconfig.sh,v 1.1 2005-09-18 22:05:34 dhmunro Exp $

debug=no

echo ""
echo "  ============= begin play/x11 configuration =============="
echo ""

rm -f cfg*

fatality=0

# figure out directories to compile and load with X11
if test -z "$NO_XLIB"; then
  if test -z "$X11BASE"; then
    X11BASE=/no/suggested/x11dir
  fi
  # directory list is from autoconf, except openwin promoted near top
  xlist=" \
    $X11BASE/include           \
    /usr/X11R6/include         \
    /usr/X11R5/include         \
    /usr/X11R4/include         \
    /usr/include/X11R6         \
    /usr/include/X11R5         \
    /usr/include/X11R4         \
    /usr/openwin/include       \
    /usr/openwin/share/include \
    /usr/local/X11R6/include   \
    /usr/local/X11R5/include   \
    /usr/local/X11R4/include   \
    /usr/local/include/X11R6   \
    /usr/local/include/X11R5   \
    /usr/local/include/X11R4   \
    /usr/X11/include           \
    /usr/include/X11           \
    /usr/local/X11/include     \
    /usr/local/include/X11     \
    /usr/X386/include          \
    /usr/x386/include          \
    /usr/XFree86/include/X11   \
    /usr/include               \
    /usr/local/include         \
    /usr/unsupported/include   \
    /usr/athena/include        \
    /usr/local/x11r5/include   \
    /usr/lpp/Xamples/include"

  cat >cfg.c <<EOF
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/Xatom.h>
#include <X11/cursorfont.h>
extern XVisualInfo xvi;
XVisualInfo xvi;             /* XVisualInfo declared in Xutil.h */
int main(int argc, char *argv[])
{
  Display *dpy = XOpenDisplay("nosuchserver:0.0");
  xvi.screen = XK_BackSpace;  /* XK_BackSpace defined in keysym.h */
  xvi.red_mask = XA_PRIMARY;  /* XA_PRIMARY defined in Xatom.h */
  xvi.depth = XC_crosshair;   /* XC_crosshair defined in cursorfont.h */
  XCloseDisplay(dpy);
  return 0;
}
EOF
  xfound=no
  xinc=
  xlib=
  xl64=
  if test -n "$X11INC"; then
    if test -r $d/X11/Xlib.h; then
      xinc=-I$X11INC
      if $CC $CFLAGS $xinc -c cfg.c >cfg.10b 2>&1; then
        xfound=yes
        xlib=`echo -L$d | sed s/include/lib/`
        xl64=`echo -L$d | sed s/include/lib64/`
      else
        echo "FATAL (play/x11) unable to build with X11 headers in $X11INC"
        fatality=1
      fi
    else
      echo "FATAL (play/x11) X11/Xlib.h missing from $X11INC"
      fatality=1
    fi
  elif $CC $CFLAGS -c cfg.c >cfg.10a 2>&1; then
    xfound=yes
  else
    for d in $xlist; do
      if test -r $d/X11/Xlib.h; then
        xinc=-I$d
        if $CC $CFLAGS $xinc -c cfg.c >cfg.10b 2>&1; then
          xfound=yes
          xlib=`echo -L$d | sed s/include/lib/`
          xl64=`echo -L$d | sed s/include/lib64/`
          if test $debug = no; then rm -f cfg.10a; fi
        fi
        break
      fi
    done
    if test $xfound = no; then
      echo "FATAL unable to find X11 includes (play/x11)"
      fatality=1
    fi
  fi
  if test $xfound = yes; then
    args="$CFLAGS $xinc $LDFLAGS -o cfg cfg.c"
    if test -n "$X11LIB"; then
      xlib=-L$X11LIB
      if $CC $args $xlib -lX11 $LIBS >cfg.10c 2>&1; then
        xfound=both
      fi
    elif $CC $args -lX11 $LIBS >cfg.10c 2>&1; then
      xlib=
      xfound=both
    elif test -n "$xlib" && $CC $args $xlib -lX11 $LIBS >cfg.10c 2>&1; then
      xfound=both
    elif test -n "$xl64" && $CC $args $xl64 -lX11 $LIBS >cfg.10c 2>&1; then
      xlib="$xl64"
      xfound=both
    else
      for d0 in $xlist; do
        for d1 in lib lib64; do
          d=`echo $d0 | sed s/include/$d1/`
          xall=`echo $d/libX11*`
          if test "$xall" != $d/'libX11*'; then
            xlib=-L$d
            if $CC $args $xlib -lX11 $LIBS >cfg.10d 2>&1; then
              xfound=both
              if test $debug = no; then rm -f cfg.10c; fi
              break
            fi
          fi
        done
        if test "$xfound" = both; then break; fi
      done
    fi
  fi
  if test $xfound = yes; then
    echo "FATAL unable to find X11 libraries (play/x11) $xlib"
    fatality=1
  else
    echo "found X Window System, X11 headers and libraries"
    echo "  - using X11 header switch ${xinc:-[none]}"
    echo "  - using X11 loader switch ${xlib:-[none]}"
  fi
  echo "XINC=$xinc" >>../../Make.cfg
  echo "XLIB=$xlib" >>../../Make.cfg
  echo 'X11LIB=$(XLIB) -lX11' >>../../Make.cfg
fi

# clean up, issue warning if compiler gave fishy output
rm -f cfg cfg.c cfg.o
for f in cfg.[0-9]*; do
  if grep ... $f >/dev/null 2>&1; then   # or use test -s $f ?
    echo "WARNING - check compiler message in $f"
  else # remove empty files
    rm -f $f
  fi
done

echo "appended to ../../Make.cfg"
echo ""
echo "  ============== end play/x11 configuration ==============="

exit $fatality
