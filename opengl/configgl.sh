#! /bin/sh

debug=no

echo ""
echo "  ============= begin OpenGL for GLX configuration =============="
echo ""

rm -f cfg*

fatality=0
have_gl=no

# need to read xlib, xinc from Y_HOME/Make.cfg

gllist=" \
  /usr/X11R6/include \
  /usr/local/include"

cat >cfg.c <<EOF
#include <GL/glx.h>
#include <GL/gl.h>
#ifndef USE_MESA_PIXMAPS
# undef glXCreateGLXPixmapMESA
# define glXCreateGLXPixmapMESA(d, v, p, c) glXCreateGLXPixmap(d, v, p)
#endif
static int attr[] = {
  GLX_RGBA, GLX_DOUBLEBUFFER, GLX_RED_SIZE, 1, GLX_GREEN_SIZE, 1,
  GLX_BLUE_SIZE, 1, None };
int main(int argc, char *argv[])
{
  Display *dpy = XOpenDisplay("nosuchserver:0.0");
  XVisualInfo *vi = glXChooseVisual(dpy, DefaultScreen(dpy), attr);
  GLXContext cx = glXCreateContext(dpy, vi, 0, GL_TRUE);
  Window win = XCreateSimpleWindow(dpy, RootWindow(dpy, vi->screen),
                                   0,0, 100,100, 4, 0, 0);
  Drawable d = glXCreateGLXPixmapMESA(dpy, vi, win,
                                      DefaultColormap(dpy, vi->screen));
  glXDestroyGLXPixmap(dpy, d);
  glXMakeCurrent(dpy, win, cx);
  glClearColor(1,1,0,1);
  glClear(GL_COLOR_BUFFER_BIT);
  glFlush();
  XCloseDisplay(dpy);
  return 0;
}
EOF

xfound=no
glinc=$GLINC
gllib=$GLLIB
gmesapix=""
if $CC $CFLAGS $glinc $xinc -c cfg.c >cfg.10a 2>&1; then
  xfound=yes
else
  for d in $gllist; do
    if test -r $d/GL/glx.h; then
      glinc=-I$d
      if $CC $CFLAGS $glinc $xinc -c cfg.c >cfg.10b 2>&1; then
	  xfound=yes
        gllib=`echo -L$d | sed s/include/lib/`
        if test $debug = no; then rm -f cfg.10a; fi
      fi
      break
    fi
  done
fi
if test -z "$xinc"; then
  gincs="$glinc"
else
  gincs="$glinc $xinc"
fi
if test $xfound = yes; then
  args="$CFLAGS $gincs $LDFLAGS -o cfg cfg.c"
  xlibs="$xlib -lXext -lX11"
  xlibm="$xlibs $MATHLIB"
  for gll in -lGL -lMesaGL; do
    glibs="$xlib $gll -lXext -lX11"
    if $CC $args $glibs $MATHLIB >cfg.10c 2>&1; then
      xfound=both
    elif test -n "$gllib" && \
         $CC $args $gllib $gll $xlibm >cfg.10c 2>&1; then
      glibs="$gllib $gll $xlibs"
      xfound=both
    else
      glibs="$xlib -lX11"
      gllist=`echo $gllist | sed s/include/lib/g`
      for d in $gllist; do
	  xall=`echo $d/libX11*`
        if test "$xall" != $d/'libX11*'; then
          gllib=-L$d
          if $CC $args $gllib $gll $xlibm >cfg.10d 2>&1; then
            glibs="$gllib $gll $xlibs"
	      xfound=both
            if test $debug = no; then rm -f cfg.10c; fi
          fi
          break
        fi
      done
    fi
    if test $xfound = both; then
      break
    fi
  done
  if test $xfound = both; then
    have_gl=yes
    echo "found OpenGL, GL headers and GL+GLX libraries"
    echo "  - using GL+X11 header switch ${gincs:-[none]}"
    echo "  - using GL+X11 loader switch ${glibs:-[none]}"
    if $CC -DUSE_MESA_PIXMAPS $args $glibs $MATHLIB >/dev/null 2>&1; then
      gmesapix=-DUSE_MESA_PIXMAPS
      echo "  - using glXCreateGLXPixmapMESA"
    fi
  fi
fi

if test $have_gl = yes; then
  echo "GINCS=$gincs" >>../../Make.cfg
  echo "GLIBS=$glibs" >>../../Make.cfg
  echo "GLCFLAGS=$gmesapix" >>../../Make.cfg
  echo "GLXOBJ=oglx.o" >>../../Make.cfg
elif test -n "$WITH_GLX"; then
  echo "FATAL - GLX not found"
  fatality=1
fi

echo "appended to ../../Make.cfg"
echo ""
echo "  ============== end OpenGL for GLX configuration ==============="

exit $fatality
