/*
 * $Id: playwin.h,v 1.1 2005-09-18 22:05:38 dhmunro Exp $
 * platform-dependent exposure of p_win struct, MSWindows version
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "play.h"

#ifndef __AFX_H__
#include <windows.h>
#endif

#define W_FONTS_CACHED 6

struct p_scr {
  int width, height, depth;
  int x0, y0;                  /* usable area may not start at (0,0) */
  int does_linetypes;          /* Win9x can't do dashed lines */
  int does_rotext;             /* may not be able to rotate text */

  COLORREF sys_colors[15], sys_index[15];
  PALETTEENTRY *sys_pal;
  int sys_offset;

  HFONT gui_font;
  int font_order[W_FONTS_CACHED];  /* indices into font_cache */
  struct {
    HFONT hfont;
    int font, pixsize, orient;
  } font_cache[W_FONTS_CACHED];
  p_win *font_win;             /* most recent window that called p_font */

  HCURSOR cursors[P_NONE];

  p_win *first_menu;
  p_win *active;               /* last p_win which set foreground palette */
};

struct p_win {
  void *ctx;                 /* first three members grouped for oglw.c */
  HWND w;
  unsigned long keydown;     /* required to interpret WM_CHAR */

  p_scr *s;

  HDC dc;
  HBITMAP bm;
  int menu;

  p_win *parent;
  HCURSOR cursor;
  HPALETTE palette;

  /* DC keeps three separate colors, only set when actually used */
  COLORREF *pixels;
  int n_pixels, rgb_mode;
  unsigned long color;       /* pending color */

  int pbflag;                /* 1 set if null brush installed in dc
                              * 2 set if null pen installed in dc */
  HBRUSH brush;              /* brush to be installed in dc */
  unsigned long brush_color; /* actual color of brush */
  HPEN pen;                  /* pen to be installed in dc */
  unsigned long pen_color;   /* actual color of pen */
  int pen_width, pen_type;   /* pending pen width and type */

  unsigned long font_color;  /* actual color of current font */
  int font, pixsize, orient;

  unsigned long bg;          /* for p_clear */

  HWND ancestor;             /* p_destroy or p_resize communication */
};

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

/* expose MSWindows specific data required to create subwindow */
PLUG_API HINSTANCE w_linker(LPCTSTR *pw_class, WNDPROC *pw_proc);

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif
