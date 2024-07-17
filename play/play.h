/*
 * $Id: play.h,v 1.4 2010-04-08 10:53:49 thiebaut Exp $
 * portability layer programming model declarations
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef _PLAY_H
#define _PLAY_H 1

#include "plugin.h"

BEGIN_EXTERN_C

/* application entry point */
extern int on_launch(int argc, char *argv[]);

/* main event loop control and system services */
PLUG_API void p_quit(void);
PLUG_API void p_abort(void);               /* never returns to caller */
PLUG_API void p_qclear(void);              /* clears event queue */
PLUG_API void p_stdout(char *output_line); /* only after p_stdinit */
PLUG_API void p_stderr(char *output_line); /* only after p_stdinit */
PLUG_API double p_wall_secs(void);         /* must interoperate with on_poll */
PLUG_API double p_cpu_secs(double *sys);
/* p_getenv and p_getuser return pointers to static memory */
PLUG_API char *p_getenv(const char *name);
PLUG_API char *p_getuser(void);

/* dont do anything critical if this is set -- call p_abort */
PLUG_API volatile int p_signalling;

/* turn off SIGFPE handling (for libraries which require that, like OpenGL)
 * p_fpehandling(0) masks SIGFPE, p_fpehandling(1) restores SIGFPE handling
 * multiple succesive calls to p_fpehandling(1) require an equal number of
 *   calls p_fpehandling(1) restore
 * p_fpehandling(2) restores SIGFPE handling no matter how many calls
 *   to p_fpehandling(1) have been made
 * requires fenv.h
 */
PLUG_API void p_fpehandling(int on);
/* partial support for floating point exceptions in FPU_IGNORE environments
 * p_softfpe() raises SIGFPE if fpu exception bits are set
 * requires fenv.h
 */
#ifdef USE_SOFTFPE
#define P_SOFTFPE_TEST p_softfpe()
#else
#define P_SOFTFPE_TEST
#endif
PLUG_API void p_softfpe(void);

/* data structures (required for screen graphics only)
 * - p_scr represents a screen (plus keyboard and mouse)
 * - p_win represents a window on a screen
 * all are opaque to platform independent code */
typedef struct p_scr p_scr;
typedef struct p_win p_win;
typedef unsigned long p_col_t;

/* routines to establish callback functions for various events
 * - application calls these either once or never */
PLUG_API void p_quitter(int (*on_quit)(void));
PLUG_API void p_stdinit(void (*on_stdin)(char *input_line));
PLUG_API void p_handler(void (*on_exception)(int signal, char *errmsg));
PLUG_API void p_gui(void (*on_expose)(void *c, int *xy),
                    void (*on_destroy)(void *c),
                    void (*on_resize)(void *c,int w,int h),
                    void (*on_focus)(void *c,int in),
                    void (*on_key)(void *c,int k,int md),
                    void (*on_click)(void *c,int b,int md,int x,int y,
                                     unsigned long ms),
                    void (*on_motion)(void *c,int md,int x,int y),
                    void (*on_deselect)(void *c),
                    void (*on_panic)(p_scr *screen));
PLUG_API void p_gui_query(void (**on_expose)(void *c, int *xy),
			  void (**on_destroy)(void *c),
			  void (**on_resize)(void *c,int w,int h),
			  void (**on_focus)(void *c,int in),
			  void (**on_key)(void *c,int k,int md),
			  void (**on_click)(void *c,int b,int md,int x,int y,
					    unsigned long ms),
			  void (**on_motion)(void *c,int md,int x,int y),
			  void (**on_deselect)(void *c),
			  void (**on_panic)(p_scr *screen));
PLUG_API void (*p_on_connect)(int dis, int fd);

/* asynchronous subprocesses using callbacks (see also p_popen, p_system) */
typedef struct p_spawn_t p_spawn_t;
PLUG_API p_spawn_t *p_spawn(char *name, char **argv,
                          void (*callback)(void *ctx, int err),
                          void *ctx, int err);
PLUG_API long p_recv(p_spawn_t *proc, char *msg, long len);
/* in p_send msg=0, len<0 sends signal -len */
PLUG_API int p_send(p_spawn_t *proc, char *msg, long len);
PLUG_API void p_spawf(p_spawn_t *proc, int nocallback);

/* socket interface */
typedef struct psckt_t psckt_t;
typedef void psckt_cb_t(psckt_t *sock, void *ctx);
/* two kinds of sockets -- listener and data
 *   - sock argument to accept must be a listener socket
 *   - sock argument to send, recv must be a data socket
 *   - listen returns listener socket, which waits for incoming connections
 *   - accept and connect return data socket
 * listen: sets *pport to actual port if *pport == 0 on input
 *   - non-zero callback to listen in background (callback calls accept)
 * accept: wait for listening socket, return connection socket and addr
 * connect: connect to remote listener
 *   - non-zero callback to wait for data in background (callback calls recv)
 * recv, send: use the socket
 * close: free all resources for socket, close(0) closes all sockets (on_quit)
 */
PLUG_API psckt_t *psckt_listen(int *pport, void *ctx, psckt_cb_t *callback);
PLUG_API psckt_t *psckt_accept(psckt_t *listener, char **ppeer, void *ctx,
                               psckt_cb_t *callback);
PLUG_API psckt_t *psckt_connect(const char *addr, int port, void *ctx,
                                psckt_cb_t *callback);
PLUG_API long psckt_recv(psckt_t *sock, void *msg, long len);
PLUG_API int psckt_send(psckt_t *sock, const void *msg, long len);
PLUG_API void psckt_close(psckt_t *sock);

/* screen graphics connection */
PLUG_API p_scr *p_connect(char *server_name);  /* server_name 0 gets default */
PLUG_API void p_disconnect(p_scr *screen);
PLUG_API p_scr *p_multihead(p_scr *other_screen, int number);

/* screen graphics queries (note when parameter is screen not window) */
PLUG_API int p_txheight(p_scr *s, int font, int pixsize, int *baseline);
PLUG_API int p_txwidth(p_scr *s, const char *text,int n, int font,int pixsize);
PLUG_API int p_sshape(p_scr *s, int *width, int *height);
PLUG_API int p_wincount(p_scr *s);
PLUG_API void p_winloc(p_win *w, int *x, int *y);

/* screen graphics window and pixmap management */
PLUG_API p_win *p_window(p_scr *s, int width, int height, const char *title,
                         p_col_t bg, int hints, void *ctx);
PLUG_API p_win *p_subwindow(p_scr *s, int width, int height,
                            unsigned long parent_id, int x, int y,
                            p_col_t bg, int hints, void *ctx);
PLUG_API p_win *p_menu(p_scr *s, int width, int height, int x, int y,
                       p_col_t bg, void *ctx);
PLUG_API p_win *p_offscreen(p_win *parent, int width, int height);
PLUG_API p_win *p_metafile(p_win *parent, char *filename,
                           int x0, int y0, int width, int height, int hints);
PLUG_API void p_destroy(p_win *w);

/* screen graphics interactions with selection or clipboard */
PLUG_API int p_scopy(p_win *w, char *string, int n);
PLUG_API char *p_spaste(p_win *w);

/* screen graphics control functions */
PLUG_API void p_feep(p_win *w);
PLUG_API void p_flush(p_win *w);
PLUG_API void p_clear(p_win *w);
PLUG_API void p_resize(p_win *w, int width, int height);
PLUG_API void p_raise(p_win *w);
PLUG_API void p_cursor(p_win *w, int cursor);
PLUG_API void p_palette(p_win *w, p_col_t *colors, int n);
PLUG_API void p_clip(p_win *w, int x0, int y0, int x1, int y1);

/* screen graphics property setting functions */
PLUG_API void p_color(p_win *w, p_col_t color);
PLUG_API void p_font(p_win *w, int font, int pixsize, int orient);
PLUG_API void p_pen(p_win *w, int width, int type);

/* set point list for p_dots, p_lines, p_fill, p_segments (pairs in list)
 * if n>=0, creates a new list of points
 * if n<0, appends to existing list of points
 * total number of points (after all appends) will be <=2048
 * any drawing call resets the point list */
PLUG_API void p_i_pnts(p_win *w, const int *x, const int *y, int n);
PLUG_API void p_d_pnts(p_win *w, const double *x, const double *y, int n);
/* query or set coordinate mapping for p_d_pnts */
PLUG_API void p_d_map(p_win *w, double xt[], double yt[], int set);

/* screen graphics drawing functions */
PLUG_API void p_text(p_win *w, int x0, int y0, const char *text, int n);
PLUG_API void p_rect(p_win *w, int x0, int y0, int x1, int y1, int border);
PLUG_API void p_ellipse(p_win *w, int x0, int y0, int x1, int y1, int border);
PLUG_API void p_dots(p_win *w);
PLUG_API void p_segments(p_win *w);
PLUG_API void p_lines(p_win *w);
PLUG_API void p_fill(p_win *w, int convexity);
PLUG_API void p_ndx_cell(p_win *w, unsigned char *ndxs, int ncols, int nrows,
                         int x0, int y0, int x1, int y1);
PLUG_API void p_rgb_cell(p_win *w, unsigned char *rgbs, int ncols, int nrows,
                         int x0, int y0, int x1, int y1);
PLUG_API void p_bitblt(p_win *w, int x, int y, p_win *offscreen,
                       int x0, int y0, int x1, int y1);

PLUG_API void p_rgb_read(p_win *w, unsigned char *rgbs,
                         int x0, int y0, int x1, int y1);

/*------------------------------------------------------------------------*/
/* following have generic implementations */

/* idle and alarm "events" */
PLUG_API void p_idler(int (*on_idle)(void));
PLUG_API void p_on_idle(int reset);
PLUG_API double p_timeout(void);
PLUG_API void p_set_alarm(double secs, void (*on_alarm)(void *context),
                          void *context);
PLUG_API void p_clr_alarm(void (*on_alarm)(void *c), void *context);

/* bitmap rotation, lsbit first and msbit first versions */
PLUG_API unsigned char p_bit_rev[256];
PLUG_API void p_lrot180(unsigned char *from, unsigned char *to,
                        int fcols, int frows);
PLUG_API void p_lrot090(unsigned char *from, unsigned char *to,
                        int fcols, int frows);
PLUG_API void p_lrot270(unsigned char *from, unsigned char *to,
                        int fcols, int frows);
PLUG_API void p_mrot180(unsigned char *from, unsigned char *to,
                        int fcols, int frows);
PLUG_API void p_mrot090(unsigned char *from, unsigned char *to,
                        int fcols, int frows);
PLUG_API void p_mrot270(unsigned char *from, unsigned char *to,
                        int fcols, int frows);

/* 5x9x5 rgb colormap for p_palette(w,p_595,225) */
PLUG_API p_col_t p_595[225];

END_EXTERN_C

/*------------------------------------------------------------------------*/

/* on_exception arguments */
#define PSIG_NONE  0
#define PSIG_SOFT  1
#define PSIG_INT   2
#define PSIG_FPE   3
#define PSIG_SEGV  4
#define PSIG_ILL   5
#define PSIG_BUS   6
#define PSIG_IO    7
#define PSIG_OTHER 8

/* window hints */
#define P_PRIVMAP  0x01
#define P_NOKEY    0x02
#define P_NOMOTION 0x04
#define P_NORESIZE 0x08
#define P_DIALOG   0x10
#define P_MODAL    0x20
#define P_RGBMODEL 0x40

/* cursors */
#define P_SELECT    0
#define P_CROSSHAIR 1
#define P_TEXT      2
#define P_N         3
#define P_S         4
#define P_E         5
#define P_W         6
#define P_NS        7
#define P_EW        8
#define P_NSEW      9
#define P_ROTATE   10
#define P_DEATH    11
#define P_HAND     12
#define P_NONE     13

/* colors */
#define P_IS_NDX(color) ((p_col_t)(color)<256UL)
#define P_IS_RGB(color) ((p_col_t)(color)>=256UL)
#define P_R(color) ((p_col_t)(color)&0xffUL)
#define P_G(color) (((p_col_t)(color)>>8)&0xffUL)
#define P_B(color) (((p_col_t)(color)>>16)&0xffUL)
#define P_RGB(r,g,b) ((p_col_t)(r) | ((p_col_t)(g)<<8) | ((p_col_t)(b)<<16) | 0x01000000)
#define P_BG      255UL
#define P_FG      254UL
#define P_BLACK   253UL
#define P_WHITE   252UL
#define P_RED     251UL
#define P_GREEN   250UL
#define P_BLUE    249UL
#define P_CYAN    248UL
#define P_MAGENTA 247UL
#define P_YELLOW  246UL
#define P_GRAYD   245UL
#define P_GRAYC   244UL
#define P_GRAYB   243UL
#define P_GRAYA   242UL
#define P_XOR     241UL
#define P_EXTRA   240UL

/* fonts */
#define P_COURIER     0
#define P_TIMES       4
#define P_HELVETICA   8
#define P_SYMBOL     12
#define P_NEWCENTURY 16
#define P_GUI_FONT   20
#define P_BOLD        1
#define P_ITALIC      2
#define P_OPAQUE     32

/* line types */
#define P_SOLID      0
#define P_DASH       1
#define P_DOT        2
#define P_DASHDOT    3
#define P_DASHDOTDOT 4
#define P_SQUARE     8

/* mouse buttons and shift keys */
#define P_BTN1      000010
#define P_BTN2      000020
#define P_BTN3      000040
#define P_BTN4      000100
#define P_BTN5      000200
#define P_SHIFT     000400
#define P_CONTROL   001000
#define P_META      002000
#define P_ALT       004000
#define P_COMPOSE   010000
#define P_KEYPAD    020000

/* keys beyond ASCI */
#define P_LEFT    0x0100
#define P_RIGHT   0x0101
#define P_UP      0x0102
#define P_DOWN    0x0103
#define P_PGUP    0x0104
#define P_PGDN    0x0105
#define P_HOME    0x0106
#define P_END     0x0107
#define P_INSERT  0x0108
#define P_F0      0x0200
#define P_F1      0x0201
#define P_F2      0x0202
#define P_F3      0x0203
#define P_F4      0x0204
#define P_F5      0x0205
#define P_F6      0x0206
#define P_F7      0x0207
#define P_F8      0x0208
#define P_F9      0x0209
#define P_F10     0x020a
#define P_F11     0x020b
#define P_F12     0x020c

#endif /* _PLAY_H */
