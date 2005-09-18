/*
 * $Id: playx.h,v 1.1 2005-09-18 22:05:32 dhmunro Exp $
 * declare routines and structs to use an X11 server
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/* X11 implementation files include this instead of play.h */
#include "playwin.h"

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/* point list for polylines, fills, dots, segments */
extern XPoint x_pt_list[2050];
extern int x_pt_count;

/* retrieve p_win* given Window id number, Display* (for event handling)
 * - the p_win context can be used to back up the hierarchy further
 * - this could be implemented using XContext mechanism */
extern x_display *x_dpy(Display *dpy);
extern p_win *x_pwin(x_display *xdpy, Drawable d);

/* ordinary and I/O X error handlers */
extern int x_err_handler(Display *dpy, XErrorEvent *event);
extern int x_panic(Display *dpy);
extern void (*x_on_panic)(p_scr *s);

/* arrange to deliver X events for an X window to the event
 * handler for the corresponding p_win
 * this is virtual to allow a simpler mode in case p_gui is never called */
extern void (*x_wire_events)(x_display *xdpy, int disconnect);

/* routines to convert Gist colors, linestyles, and fonts to X11 */
extern XFontStruct *x_font(x_display *xdpy, int font, int pixsize);
extern void x_clip(Display *dpy, GC gc, int x0, int y0, int x1, int y1);
extern GC x_getgc(p_scr *s, p_win *w, int fillstyle);
extern p_col_t x_getpixel(p_win *w, p_col_t color);
extern void x_nuke_shared(p_scr *s);

/* optional X resource values (class Gist) */
extern char *x_xfont;       /* boldfont, font, Font */
extern char *x_foreground;  /* foreground, Foreground */
extern char *x_background;  /* background, Background */
extern char *x_guibg;       /* guibg */
extern char *x_guifg;       /* guifg */
extern char *x_guihi;       /* guihi */
extern char *x_guilo;       /* guilo */

extern Cursor x_cursor(p_scr *s, int cursor);

/* simple destructors that zero input pointer */
extern void x_rotzap(p_scr *s);
extern void x_tmpzap(void *ptmp);   /* x_tmpzap(anytype **ptmp) */
extern void x_gczap(Display *dpy, GC *pgc);
extern void x_imzap(p_scr *s);
extern void x_pxzap(Display *dpy, Pixmap *ppx);
extern void x_cmzap(Display *dpy, Colormap *pcm);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif
