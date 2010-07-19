/*
 * $Id: playw.h,v 1.2 2010-07-19 07:39:13 thiebaut Exp $
 * MS Windows-private portability layer declarations
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "playwin.h"

#if defined(__cplusplus) || defined(c_plusplus)
extern "C" {
#endif

extern void w_initialize(HINSTANCE i, HWND w,  void (*wquit)(void),
                         int (*wstdinit)(void(**)(char*,long),
                                         void(**)(char*,long)),
                         HWND (*wparent)(int, int, char *, int));
extern int w_on_quit(void);

extern void w_caught(void);
extern int w_sigint(int delay);
extern void w_siginit(void);
extern void w_fpu_setup(void);
extern int w_protect(int (*run)(void));

extern DWORD w_id_worker;  /* required for PostThreadMessage */
extern HANDLE w_worker;
extern int w_work_idle(void);
extern void w_pollinit(void);
extern UINT w_add_msg(void (*on_msg)(MSG *));
extern int w_app_msg(MSG *msg);
extern int (*w_msg_hook)(MSG *msg);
extern int w_add_input(HANDLE wait_obj, void (*on_input)(void *),
                       void *context);

extern int w_no_mdi;
extern int con_stdinit(void(**)(char*,long), void(**)(char*,long));
extern int (*w_stdinit)(void (**wout)(char*,long),
                        void (**werr)(char*,long));
extern void con_stdout(char *output_line, long len);
extern void con_stderr(char *output_line, long len);
extern void w_deliver(char *buf);   /* calls on_stdin */
extern char *w_sendbuf(long len);

extern int w_nwins;  /* count of graphics windows */

extern HINSTANCE w_app_instance;
extern HWND w_main_window;
extern HWND (*w_parent)(int width, int height, char *title, int hints);
extern LRESULT CALLBACK w_winproc(HWND hwnd, UINT uMsg,
                                  WPARAM wParam, LPARAM lParam);

extern char *w_pathname(const char *name);
extern char *w_unixpath(const char *wname);

/* if set non-zero, p_abort will call this */
extern void (*w_abort_hook)(void);

/* ------------------------------------------------------------------------ */

extern p_scr w_screen;
extern HCURSOR w_cursor(int cursor);
extern LPCTSTR w_win_class;
extern LPCTSTR w_menu_class;

extern COLORREF w_color(p_win *w, unsigned long color);
extern HDC w_getdc(p_win *w, int flags);

/* all bitmap functions seem to get colors backwards?? */
#define W_SWAPRGB(c) (((c)&0xff00)|(((c)>>16)&0xff)|(((c)&0xff)<<16))

extern POINT w_pt_list[2050];
extern int w_pt_count;

#if defined(__cplusplus) || defined(c_plusplus)
}
#endif
