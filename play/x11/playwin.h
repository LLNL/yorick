/*
 * $Id: playwin.h,v 1.1 2005-09-18 22:05:34 dhmunro Exp $
 * platform-dependent exposure of p_win struct, X11 version
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "play.h"
#include "phash.h"
#include <X11/Xlib.h>

#define N_FONT_CACHE 6

/* the Display struct may be shared among several root windows,
 * in the unusual case that a single server drives several screens
 * - fonts, however, are shared among all screens of a server
 *   (I ignore a possible exception of the default font, which could
 *    in principle differ from screen to screen on a server -- this
 *    may be a severe problem if the two screens differ greatly in
 *    resolution, so that what is legible on one is not on the other
 *    -- hopefully this is even rarer than multiple screens) */

typedef struct x_display x_display;
struct x_display {
  int panic;
  p_scr *screens;       /* list of screens on this server (for panic) */
  x_display *next;      /* list of all servers */
  Display *dpy;

  Atom wm_protocols, wm_delete;
  p_hashtab *id2pwin;   /* use phash instead of XContext */

  XFontStruct *font;    /* default font to use on this server */
  int unload_font;      /* non-0 if font must be unloaded */

  struct {
    XFontStruct *f;
    int font, pixsize, next;
  } cached[N_FONT_CACHE];
  int most_recent;

  struct {
    int nsizes, *sizes;
    char **names;
  } available[20];

  Cursor cursors[14];

  /* number of motion events queued when previous motion callback
   * completed -- all but the last will be skipped */
  int motion_q;

  unsigned int meta_state, alt_state;  /* masks for modifier keys */

  /* selection data */
  p_win *sel_owner;
  char *sel_string;

  /* menu count for pointer grabs */
  int n_menus;
};

extern x_display *x_displays;
typedef struct x_cshared x_cshared;

struct p_scr {
  x_display *xdpy;  /* may be accessing multiple screens on server */
  p_scr *next;       /* keep list of all screens on this server */

  int scr_num;          /* screen number on this server */
  Window root;          /* root window on this screen */
  int width, height, depth;  /* of root window */
  int nwins;            /* window count */

  int vclass;           /* visual class for this screen */
  /* pixels==0 (part of p_win) for PseudoColor visual
   * for all others, points to pixels[256] */
  p_col_t *pixels;
  /* red, green, and blue masks for TrueColor and DirectColor */
  p_col_t rmask, gmask, bmask;
  Colormap cmap;        /* None except for GrayScale and DirectColor */

  XColor colors[14];    /* standard colors */
  int free_colors;      /* bit flags for which ones need to be freed */
  Pixmap gray;          /* in case GRAYA-GRAYD are stipples */
  int gui_flags;        /* marker for stippled grays */
  x_cshared *shared;    /* tables for shared PseudoColor colors */

  /* generic graphics context and its current state */
  GC gc;
  p_col_t gc_color;
  int gc_fillstyle;     /* invalid for stippled colors, see colors.c */
  p_win *gc_w_clip;     /* gc contains clipping for this window */
  int gc_width, gc_type;
  int gc_font, gc_pixsize;

  /* temporaries required for rotating fonts (see textout.c) */
  void *tmp;
  XImage *image;
  int own_image_data;
  Pixmap pixmap;
  GC rotgc;
  int rotgc_font, rotgc_pixsize, rotgc_orient;
};

struct p_win {
  void *context;     /* application context for event callbacks */
  p_scr *s;

  Drawable d;
  p_win *parent;     /* non-0 only for offscreen pixmaps */
  int is_menu;       /* non-0 only for menus */

  Colormap cmap;
  p_col_t *pixels, *rgb_pixels;
  int n_palette;  /* number of pixels[] belonging to palette */
  int x, y, width, height, xyclip[4];
};

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/* need to expose this for use by oglx.c */
PLUG_API int x_rgb_palette(p_win *w);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif
