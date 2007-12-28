/*
 * $Id: xbasic.h,v 1.3 2007-12-28 20:20:18 thiebaut Exp $
 * Declare the basic play engine for GIST.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef XBASIC_H
#define XBASIC_H

#include "gist.h"
#include "engine.h"
#include "play.h"

typedef struct XEngine XEngine;
struct XEngine {
  Engine e;

  /* --------------- Specific to XEngine ------------------- */

  p_scr *s;
  p_win *win;
  int width, height;  /* of (virtual page) graphics window */
  int wtop, htop;     /* of actual top-level window */
  int topMargin;   /* height of top menu bar, if any */
  int leftMargin;  /* width of left menu bar, if any */
  int x, y;        /* position of graphics relative to win */
  int dpi;         /* resolution of X window (dots per inch, 75 or 100) */
  int mapped, clipping;

  /* if w!=win, this is animation mode */
  p_win *w;
  int a_width, a_height;        /* of animation Pixmap */
  int a_x, a_y;                 /* where it goes on graphics window */
  GpTransform swapped;          /* transform for graphics window while
                                 * in animation mode */

  /* if non-zero, these handlers can deal with input events */
  void (*HandleExpose)(Engine *engine, Drauing *drawing, int *xy);
  void (*HandleClick)(Engine *e,int b,int md,int x,int y, unsigned long ms);
  void (*HandleMotion)(Engine *e,int md,int x,int y);
  void (*HandleKey)(Engine *e,int k,int md);
};

PLUG_API p_scr *g_connect(char *displayName);
PLUG_API void g_disconnect(p_scr *s);

PLUG_API int gx75width, gx100width;    /* defaults are 450 and 600 pixels */
#define DefaultTopWidth(dpi) \
  (gx75width<gx100width?((dpi)*gx100width)/100:gx100width)
PLUG_API int gx75height, gx100height;  /* defaults are 450 and 600 pixels */
#define DefaultTopHeight(dpi) \
  (gx75width<gx100width?((dpi)*gx100height)/100:gx100height)
#define PixelsPerNDC(dpi) ((dpi)/ONE_INCH)

/* hack for p_subwindow communication */
PLUG_API unsigned long gx_parent;
PLUG_API int gx_xloc, gx_yloc;

/* Engine which currently has mouse focus. */
PLUG_API Engine *gxCurrentEngine;

/* GxEngine creates an XEngine and adds it to the list of GIST engines.
   The top window will generally be smaller than the graphics
   window created by GxEngine; specific engines are responsible for
   scrolling of the graphics window relative to the top window, although
   the initial location is passed in via (x, y).  The size argument is
   sizeof(XEngine), or sizeof some derived engine class.  */
PLUG_API XEngine *GxEngine(p_scr *s, char *name, GpTransform *toPixels,
                           int x, int y,
                           int topMargin, int leftMargin, long size);

/* GxInput sets optional event handlers, and calls XSelectInput with
   the given eventMask.  HandleExpose, if non-zero, will be called
   instead of redrawing the Drauing associated with the Engine, which
   is the default action.  HandleResize, if non-zero, will be called
   instead of the default resize action (which is to recenter the
   graphics window).  HandleOther, if non-zero, will be called for
   keyboard, button, or other events not recogized by the default
   handler.  */
PLUG_API int GxInput(Engine *engine,
                     void (*HandleExpose)(Engine *, Drauing *, int *),
                     void (*HandleClick)(Engine *,
                                         int, int, int, int, unsigned long),
                     void (*HandleMotion)(Engine *, int, int, int),
                     void (*HandleKey)(Engine *, int, int));

PLUG_API XEngine *GisXEngine(Engine *engine);

PLUG_API void GxExpose(Engine *engine, Drauing *drawing, int *xy);
PLUG_API void GxRecenter(XEngine *xEngine, int width, int height);

/* GxAnimate creates an offscreen pixmap for the specified region of
   the window.  Subsequent drawing takes place on the pixmap, not
   on the graphics window.  GxStrobe copies the pixmap to the screen,
   optionally clearing it for the next frame of the animation.
   The viewport should be large enough to cover everything that will
   change as the animation proceeds, but no larger to get peak speed.
   GxDirect restores the usual direct-to-screen drawing mode.  */
PLUG_API int GxAnimate(Engine *engine, GpBox *viewport);
PLUG_API int GxStrobe(Engine *engine, int clear);
PLUG_API int GxDirect(Engine *engine);

PLUG_API int g_rgb_read(Engine *eng, GpColor *rgb, long *nx, long *ny);

#endif
