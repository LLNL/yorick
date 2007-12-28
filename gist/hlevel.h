/*
 * $Id: hlevel.h,v 1.4 2007-12-28 20:20:18 thiebaut Exp $
 * Declare routines for recommended GIST interactive interface
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef HLEVEL_H
#define HLEVEL_H

#include "gist.h"

/* See README for description of these control functions */

PLUG_API void GhBeforeWait(void);
PLUG_API void GhFMA(void);
PLUG_API void GhRedraw(void);
PLUG_API void GhHCP(void);
PLUG_API void GhFMAMode(int hcp, int animate); /* 0 off, 1 on, 2 nc, 3 toggle*/

/* The pldevice call should create the necessary engines, and set their
   Engine pointers in ghDevices, then call GhSetPlotter to set the
   current device, deactivating the old device.  GhSetPlotter returns
   0 if successful, 1 if neither display nor hcp engine has been defined
   for the requested device.  GhGetPlotter returns the number of the
   current plot device, or -1 if none has been set.  */

typedef struct GhDevice GhDevice;
struct GhDevice {
  Drauing *drawing;
  Engine *display, *hcp;
  int doLegends;
  int fmaCount;
  void *hook;
};

/* Allow up to GH_NDEVS windows per application */
#ifndef GH_NDEVS
# define GH_NDEVS 64
#endif
PLUG_API GhDevice ghDevices[GH_NDEVS];

PLUG_API int GhSetPlotter(int number);
PLUG_API int GhGetPlotter(void);

/* The default hardcopy device is used for hcp commands whenever the
   current device has no hcp engine of its own.  */
PLUG_API Engine *hcpDefault;

PLUG_API void GhDumpColors(int n, int hcp, int pryvate);
PLUG_API void GhSetPalette(int n, GpColorCell *palette, int nColors);
PLUG_API int GhReadPalette(int n, const char *gpFile,
                           GpColorCell **palette, int maxColors);
PLUG_API int GhGetPalette(int n, GpColorCell **palette);
PLUG_API int GhGetColorMode(Engine *engine);
PLUG_API void SetHCPPalette(void);
PLUG_API void GhDeletePalette(int n);

/* A high-level error handler takes down an X-window before calling
   the user-installed error handler.  This prevents a huge blast of
   errors when a window is detroyed bby a window manager (for example),
   but is obviously a litle more fragile than a smart error handler
   could be.  */
PLUG_API int GhSetXHandler(void (*XHandler)(char *msg));

/* For each of the D level drawing primitives, a set of
   default parameter settings is maintained, and can be installed
   with the appropriate GhGet routine.  The GhSet routines set the
   defaults themselves.  GdCells does not use any attributes,
   and GdContours uses the same attributes as GdLines.
   GdFillMesh uses line attributes for edges (if any).  */
PLUG_API void GhGetLines(void);
PLUG_API void GhGetText(void);
PLUG_API void GhGetMesh(void);
PLUG_API void GhGetVectors(void);
PLUG_API void GhGetFill(void);

PLUG_API void GhSetLines(void);
PLUG_API void GhSetText(void);
PLUG_API void GhSetMesh(void);
PLUG_API void GhSetVectors(void);
PLUG_API void GhSetFill(void);

/* The GpFXEngine (fancy X engine) has controls for the zoom factor
   and a function for initiating a point-and-click sequence.  */

PLUG2_API GpReal gxZoomFactor;   /* should be >1.0, default is 1.5 */

/* The GxPointClick function initiates an interactive point-and-click
   session with the window -- it will not return until a button has
   been pressed, then released.  It returns non-zero if the operation
   was aborted by pressing a second button before releasing the first.
     engine --   an X engine whose display is to be used
     style --    1 to draw a rubber box, 2 to draw a rubber line,
                 otherwise, no visible indication of operation
     system --   system number to which the world coordinates should
                 be transformed, or -1 to use the system under the
                 pointer -- the release coordinates are always in the
                 same system as the press coordinates
     CallBack -- function to be called twice, first when the button is
                 pressed, next when it is released -- operation will
                 be aborted if CallBack returns non-zero
                 Arguments passed to CallBack:
                   engine  -- in which press/release occurred
                   system  -- system under pointer, if above system -1
                   release -- 0 on press, 1 on release
                   x, y    -- coordinates of pointer relative to system
                   butmod  -- 1 - 5 on press to tell which button
                              mask to tell which modifiers on release:
                              1 shift, 2 lock, 4 control, 8 - 128 mod1-5
                   xn, yn  -- NDC coordinates of pointer
 */
PLUG2_API int GxPointClick(Engine *engine, int style, int system,
                           int (*CallBack)(Engine *engine, int system,
                                           int release, GpReal x, GpReal y,
                                           int butmod, GpReal xn, GpReal yn));

/* Variables to store engine which currently has mouse focus and
   coordinate system and mouse coordinates after last mouse motion. */
PLUG2_API Engine *gxCurrentEngine;
PLUG2_API int gxCurrentSys;
PLUG2_API GpReal gxCurrentX, gxCurrentY;

/* The GhGetMouse function stores the current coordinate system and
   mouse position at SYS, X and Y repectively (any of them can be
   NULL) and returns the device number which has the mouse focus.
   If no device currently has the focus, -1 is returned. */
PLUG_API int GhGetMouse(int *sys, double *x, double *y);

PLUG2_API int g_rgb_read(Engine *eng, GpColor *rgb, long *nx, long *ny);
PLUG_API void (*g_on_idle)(void);

#endif
