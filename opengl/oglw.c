/*
 * $Id: oglw.c,v 1.1 2005-09-18 22:06:23 dhmunro Exp $
 * GL window management for MS Windows
 * for yorick-gl package - this source unused and may be out of date
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "pstdlib.h"
#include "playwin.h"
#include "playgl.h"

static LRESULT CALLBACK w_glsubclass(HWND hwnd,
                                     UINT msg, WPARAM wp, LPARAM lp);

struct p_glwin {
  void *ctx;        /* ctx, w, keydown must match p_win struct */
  HWND w;
  unsigned long keydown;

  p_win *parent;
  HDC dc;
  HGLRC glrc;
  int x, y;
};

static unsigned char bits3[8] = {
  0, 0111>>1, 0222>>1, 0333>>1, 0444>>1, 0555>>1, 0666>>1, 0777>>1 };
static unsigned char bits2[4] = { 0, 0x55, 0xaa, 0xff };
static unsigned char bits1[2] = { 0, 0xff };
static unsigned char *bits[3] = { bits1, bits2, bits3 };
static int std_ndx[13] = {
  0, 3, 24, 27, 64, 67, 88, 173, 181, 236, 247, 164, 91 };
static PALETTEENTRY std_pal[20] = {
  { 0,   0,   0,    0 },{ 0x80,0,   0,    0 },{ 0,   0x80,0,    0 },
  { 0x80,0x80,0,    0 },{ 0,   0,   0x80, 0 },{ 0x80,0,   0x80, 0 },
  { 0,   0x80,0x80, 0 },{ 0xC0,0xC0,0xC0, 0 },
  { 192, 220, 192,  0 },{ 166, 202, 240,  0 },{ 255, 251, 240,  0 },
  { 160, 160, 164,  0 },
  { 0x80,0x80,0x80, 0 },{ 0xFF,0,   0,    0 },{ 0,   0xFF,0,    0 },
  { 0xFF,0xFF,0,    0 },{ 0,   0,   0xFF, 0 },{ 0xFF,0,   0xFF, 0 },
  { 0,   0xFF,0xFF, 0 },{ 0xFF,0xFF,0xFF, 0 }};

static HINSTANCE wc_app_instance = 0;
static LPCTSTR wc_win_class = 0;
static WNDPROC wc_winproc = 0;

/* used for glx debugging, irrelevant for MSWindows gl */
extern int p_glvid_force, p_glvid_using;
int p_glvid_force = 0, p_glvid_using = 0;

p_glwin *
p_glcreate(p_win *parent, int width, int height, int x, int y, void *ctx)
{
  int is_pixmap = (parent->bm!=0);
  HDC dc = parent->dc;
  p_scr *s = parent->s;
  p_glwin *w = p_malloc(sizeof(p_glwin));
  HWND hw = 0;
  HGLRC glrc = 0;
  PIXELFORMATDESCRIPTOR pfd;
  int pf_index;

  if (!w) return 0;

  if (!is_pixmap) {
    DWORD style = WS_CHILD | WS_CLIPCHILDREN | WS_CLIPSIBLINGS;
    DWORD xstyle = 0;
    if (!wc_win_class)
      wc_app_instance = w_linker(&wc_win_class, &wc_winproc);
    hw = CreateWindowEx(xstyle, wc_win_class, 0, style,
                        x, y, width, height, parent->w, 0,
                        wc_app_instance, 0);
    if (!hw) return 0;
    SetWindowLong(hw, GWL_USERDATA, (LONG)w);
    /* idea -- subclass the window to pass events to parent */
    SetWindowLong(hw, GWL_WNDPROC, (LONG)w_glsubclass);
    ShowWindow(hw, SW_SHOWNA);
    dc = GetDC(hw);
  }

  pfd.nSize = sizeof(PIXELFORMATDESCRIPTOR);
  pfd.nVersion = 1;
  pfd.dwFlags = (is_pixmap? PFD_DRAW_TO_BITMAP : PFD_DRAW_TO_WINDOW) |
    PFD_SUPPORT_OPENGL | PFD_DOUBLEBUFFER;
  pfd.iPixelType = PFD_TYPE_RGBA;
  pfd.cColorBits = 24;
  pfd.cDepthBits = 32;
  pfd.iLayerType = PFD_MAIN_PLANE;
  pfd.cAccumBits = pfd.cStencilBits = pfd.cAuxBuffers = 0;
  /* rest are ignored */
  pfd.cRedBits = pfd.cGreenBits = pfd.cBlueBits = pfd.cAlphaBits = 8;
  pfd.cRedShift = pfd.cGreenShift = pfd.cBlueShift = pfd.cAlphaShift = 0;
  pfd.cAccumRedBits = pfd.cAccumGreenBits = pfd.cAccumBlueBits = 0;
  pfd.cAccumAlphaBits = 0;
  pfd.bReserved = 0;
  pfd.dwLayerMask = pfd.dwVisibleMask = pfd.dwDamageMask = 0;

  pf_index = ChoosePixelFormat(dc, &pfd);
  if (pf_index && SetPixelFormat(dc, pf_index, &pfd)) {
    /* retrieve actual pfd obtained */
    pf_index = DescribePixelFormat(dc, pf_index,
                                  sizeof(PIXELFORMATDESCRIPTOR), &pfd);
    glrc = wglCreateContext(dc);
    if (glrc) {
      w->parent = parent;
      w->w = hw;
      w->dc = dc;
      w->glrc = glrc;
      w->x = x;
      w->y = y;
      w->ctx = ctx;

      if (pfd.dwFlags & PFD_NEED_PALETTE) {
        HPALETTE palette;
        /* from MSDN CUBE sample code */
        unsigned int i, n = 1 << pfd.cColorBits;
        LOGPALETTE *pal = p_malloc(sizeof(LOGPALETTE)+n*sizeof(PALETTEENTRY));
        int r = pfd.cRedBits-1, g = pfd.cGreenBits-1, b = pfd.cBlueBits-1;
        unsigned int rmsk, gmsk, bmsk;
        if (r<0 || r>2) r = 0;
        if (g<0 || g>2) r = 0;
        if (b<0 || b>2) r = 0;
        rmsk = (2<<r)-1, gmsk = (2<<g)-1, bmsk = (2<<b)-1;
        if (n>256) n = 256;
        pal->palVersion = 0x300;
        pal->palNumEntries = n;
        for (i=0 ; i<n ; i++) {
          pal->palPalEntry[i].peRed = bits[r][(i>>pfd.cRedShift)&rmsk];
          pal->palPalEntry[i].peGreen = bits[g][(i>>pfd.cGreenShift)&gmsk];
          pal->palPalEntry[i].peBlue = bits[b][(i>>pfd.cBlueShift)&bmsk];
          pal->palPalEntry[i].peFlags = 0;
        }
        if ((pfd.cColorBits == 8) &&
            (pfd.cRedBits   == 3) && (pfd.cRedShift   == 0) &&
            (pfd.cGreenBits == 3) && (pfd.cGreenShift == 3) &&
            (pfd.cBlueBits  == 2) && (pfd.cBlueShift  == 6)) {
          for (i=1 ; i<=12 ; i++)
            pal->palPalEntry[std_ndx[i]] = std_pal[i];
        }
        palette = CreatePalette(pal);
        p_free(pal);
        if (palette) {
          HPALETTE pt = SelectPalette(parent->dc, palette, 0);
          if (pt) DeleteObject(pt);
          parent->rgb_mode = 1;  /* inhibit any further palette changes */
          RealizePalette(parent->dc);
          SelectPalette(dc, palette, 0);
          RealizePalette(dc);
        }
      }
    }
  }

  if (!glrc) {
    if (hw) DestroyWindow(hw);
    p_free(w);
    w = 0;
  }

  return w;
}

void
p_gldestroy(p_glwin *w)
{
  if (w) {
    wglDeleteContext(w->glrc);
    if (w->w) DestroyWindow(w->w);
    p_free(w);
  }
}

void
p_glresize(p_glwin *w, int width, int height, int x, int y)
{
  if (w && w->w && !w->parent->bm)
    SetWindowPos(w->w, 0, x,y, width,height, SWP_NOACTIVATE|SWP_NOZORDER);
}

void
p_glswap(p_glwin *w)
{
  SwapBuffers(w->dc);
  GdiFlush();
}

void
p_glcurrent(p_glwin *w)
{
  wglMakeCurrent(w->dc, w->glrc);
}

static LRESULT CALLBACK
w_glsubclass(HWND hwnd, UINT msg, WPARAM wp, LPARAM lp)
{
  p_glwin *pgl = (p_glwin *)GetWindowLong(hwnd, GWL_USERDATA);
  if (pgl && pgl->w==hwnd) {
    switch (msg) {
    case WM_PAINT:
      break;
    case WM_DESTROY:
      pgl->w = 0;
      p_gldestroy(pgl);
      return 0;
    case WM_RBUTTONDOWN:
    case WM_MBUTTONDOWN:
    case WM_LBUTTONDOWN:
    case WM_RBUTTONUP:
    case WM_MBUTTONUP:
    case WM_LBUTTONUP:
    case WM_MOUSEMOVE:
      {
        int x = LOWORD(lp);
        int y = HIWORD(lp);
        x += pgl->x;
        y += pgl->y;
        lp = MAKELPARAM(x, y);
        hwnd = pgl->parent->w;
      }
      break;
    default:
      hwnd = pgl->parent->w;
      break;
    }
  }
  return CallWindowProc(wc_winproc, hwnd, msg, wp, lp);
}
