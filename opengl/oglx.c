/*
 * $Id: oglx.c,v 1.1 2005-09-18 22:06:22 dhmunro Exp $
 * GL window management for X11, using GLX
 * for yorick-gl package - this source unused and may be out of date
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "config.h"
#include "pstdlib.h"
#include "playwin.h"
#include "playgl.h"

#include <GL/glx.h>

/* during debug, generate as many Expose events as possible */
#ifndef P_DEBUG
# define BACKING_STORE WhenMapped
#else
# define BACKING_STORE NotUseful
#endif

static p_glwin *gl_winlist = 0;

struct p_glwin {
  void *context;    /* must match p_win struct member */
  p_glwin* next;
  p_scr *s;
  p_win *parent;
  int offscreen;
  Drawable d;
  Colormap cmap;
  GLXContext glx;
  XVisualInfo *v;
  int error_base, event_base;
};

static int pgl_attrib[] = { GLX_DOUBLEBUFFER,
                            GLX_RGBA,
                            GLX_RED_SIZE, 1,
                            GLX_GREEN_SIZE, 1,
                            GLX_BLUE_SIZE, 1,
                            GLX_DEPTH_SIZE, 1,
                            None };

/* visual debugging:
 *
 * int status, value; XVisualInfo *v;
 * status = glXGetConfig(dpy, v, GLX_USE_GL, &value);
 *   bad if (status==GLX_NO_EXTENSION || status==GLX_BAD_VISUAL || !value)
 * glXGetConfig(dpy, v, property, &value);
 *   where property is
 * GLX_BUFFER_SIZE, GLX_LEVEL, GLX_RGBA, GLX_DOUBLEBUFFER, GLX_STEREO,
 * GLX_AUX_BUFFERS, GLX_DEPTH_SIZE, GLX_STENCIL_SIZE,
 * GLX_RED_SIZE, GLX_GREEN_SIZE, GLX_BLUE_SIZE, GLX_ALPHA_SIZE,
 * GLX_ACCUM_RED_SIZE, GLX_ACCUM_GREEN_SIZE, GLX_ACCUM_BLUE_SIZE,
 * GLX_ACCUM_ALPHA_SIZE
 *
 * xdpyinfo utility is also useful
 */
extern int p_glvid_force;  /* set to force this visualid, 0 to choose best */
int p_glvid_force = 0;
extern int p_glvid_using;  /* p_glcreate sets to actual visualid */
int p_glvid_using = 0;

p_glwin *
p_glcreate(p_win *parent, int width, int height, int x, int y, void *ctx)
{
  p_scr *s = parent->s;
  Display *dpy = s->xdpy->dpy;
  XVisualInfo *v;
  Colormap cmap = None;
  int error_base, event_base;
  p_glwin *w, *cousin = gl_winlist;

  while (cousin && cousin->s!=s) cousin = cousin->next;

  if (!cousin) {
    if (!glXQueryExtension(dpy, &error_base, &event_base))
      return 0;

    if (!p_glvid_force) {
      v = glXChooseVisual(dpy, s->scr_num, pgl_attrib);
    } else {
      XVisualInfo vreq;
      int n;
      vreq.screen = s->scr_num;
      vreq.visualid = p_glvid_force;
      v = XGetVisualInfo(dpy, (VisualScreenMask | VisualIDMask), &vreq, &n);
    }
    if (!v) return 0;

  } else {
    error_base = cousin->error_base;
    event_base = cousin->event_base;
    cmap = cousin->cmap;
    v = cousin->v;
  }

  p_glvid_using = v->visualid;

  w = p_malloc(sizeof(p_glwin));
  if (!w) return 0;

  w->context = ctx;
  w->s = s;
  w->parent = parent;
  w->error_base = error_base;
  w->event_base = event_base;
  w->v = v;

  /* go ahead and install rgb-model 5x9x5 palette GLX can use */
  if (s->vclass==PseudoColor) x_rgb_palette(parent);
  if (v->visualid==XVisualIDFromVisual(DefaultVisual(dpy,s->scr_num))) {
    cmap = parent->parent? parent->parent->cmap : parent->cmap;
    if (cmap==None)
      cmap = DefaultColormap(dpy, s->scr_num);
    w->cmap = None;
  } else if (cmap==None) {
    w->cmap = cmap = XCreateColormap(dpy, RootWindow(dpy,s->scr_num),
                                     v->visual, AllocNone);
  } else {
    w->cmap = cmap;
  }

  w->offscreen = (parent->parent!=0);
  if (!w->offscreen) {
    XSetWindowAttributes attr;
    attr.colormap = cmap;
    attr.backing_store = BACKING_STORE;

    w->d = XCreateWindow( dpy, parent->d, x, y, width, height,
                         0, v->depth, InputOutput, v->visual,
                         CWColormap | CWBackingStore, &attr );
    if (w->d) {
      /* let all events propagate to parent window
       * -- except expose cannot propagate */
      XSelectInput(dpy, w->d, ExposureMask);
      if (p_hinsert(s->xdpy->id2pwin, P_IHASH(w->d), w)) {
        XDestroyWindow(dpy, w->d);
        w->d = None;
      } else {
        XMapWindow(dpy, w->d);
      }
    }
  } else {
#ifndef USE_MESA_PIXMAPS
    w->d = glXCreateGLXPixmap(dpy, v, parent->d);
#else
    w->d = glXCreateGLXPixmapMESA(dpy, v, parent->d, cmap);
#endif
  }

  w->glx = (w->d!=None)?
    glXCreateContext(dpy, v, cousin?cousin->glx:0, True) : 0;
  if (!w->glx) {
    p_free(w);
    return 0;
  }

  w->next = gl_winlist;
  gl_winlist = w;
  return w;
}

void
p_gldestroy(p_glwin *w)
{
  if (w) {
    p_glwin *list = gl_winlist;
    if (list==w) {
      gl_winlist = w->next;
    } else while (list) {
      if (list->next==w) {
        list->next = w->next;
        break;
      }
      list = list->next;
    }
    if (w->cmap != None) {
      Colormap cmap = w->cmap;
      w->cmap = None;
      for (list=gl_winlist ; list ; list=list->next) {
        if (list->s==w->s && list->cmap==cmap) {
          cmap = None;
          break;
        }
      }
      if (cmap != None) XFreeColormap(w->s->xdpy->dpy, cmap);
    }
    glXDestroyContext(w->s->xdpy->dpy, w->glx);
    if (w->offscreen) glXDestroyGLXPixmap(w->s->xdpy->dpy, w->d);
    else XDestroyWindow(w->s->xdpy->dpy, w->d);
  }
}

void
p_glresize(p_glwin *w, int width, int height, int x, int y)
{
  if (w && !w->parent->parent)
    XMoveResizeWindow(w->s->xdpy->dpy, w->d, x, y, width, height);
}

void
p_glswap(p_glwin *w)
{
  glXSwapBuffers(w->s->xdpy->dpy, w->d);
  p_flush(w->parent);
}

void
p_glcurrent(p_glwin *w)
{
  glXMakeCurrent(w->s->xdpy->dpy, w->d, w->glx);
}
