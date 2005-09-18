/*
 * $Id: test3d.c,v 1.1 2005-09-18 22:06:23 dhmunro Exp $
 * OpenGL platform library interface test program
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "play.h"
#include "playgl.h"
#include "pstdlib.h"
#include "pstdio.h"

#include <GL/gl.h>

#include <stdio.h>
#include <string.h>

extern int on_quit(void);

extern void on_stdin(char *input_line);

extern int on_idle(void);

extern void on_focus(void *c,int in);
extern void on_key(void *c,int k,int md);
extern void on_click(void *c,int b,int md,int x,int y,
		     unsigned long ms);
extern void on_motion(void *c,int md,int x,int y);
extern void on_expose(void *c, int *xy);
extern void on_deselect(void *c);
extern void on_destroy(void *c);
extern void on_resize(void *c,int w,int h);

extern void on_exception(int signal, char *errmsg);

extern void on_panic(p_scr *screen);

extern void seg_draw(p_win *w, int x0, int y0, int x1, int y1);

static void help_act(char *args);
static void quit_act(char *args);
static void bar_act(char *args);
static void check_err(char *msg);

static void init_lighting(void);
static void set_material(float r, float g, float b);
static void init_cube(GLfloat *rot);
static void draw_cube(GLfloat *rot);
static void roll_cube(float dx, float dy);
static void draw_face(float nx, float ny, float nz,
		      float x, float y, float z);

extern p_scr *scr;
extern p_win *win1, *win2;
extern p_glwin *glw1, *glw2;
p_scr *scr=0;
p_win *win1=0, *win2=0;
p_glwin *glw1=0, *glw2=0;
static int menubar = 0;
static int ignore_destroy = 0;

int
on_quit(void)
{
  if (glw1) p_gldestroy(glw1);
  if (win1) p_destroy(win1);
  if (glw2) p_gldestroy(glw2);
  if (win2) p_destroy(win2);
  if (scr) p_disconnect(scr);
  p_stdout("\ntest3d: quitting now\n");
  return 0;
}

static int prompt_issued = 0;
static int panic_count = 0;

static char *sig_name[] = {
  "PSIG_NONE", "PSIG_SOFT", "PSIG_INT", "PSIG_FPE", "PSIG_SEGV",
  "PSIG_ILL", "PSIG_BUS", "PSIG_IO", "PSIG_OTHER" };

void
on_exception(int signal, char *errmsg)
{
  p_qclear();

  if (signal<0) signal = 0;
  else if (signal>PSIG_OTHER) signal = PSIG_OTHER;
  sprintf(p_wkspc.c, "test3d received signal %s\n", sig_name[signal]);
  p_stdout(p_wkspc.c);
  if (errmsg) {
    sprintf(p_wkspc.c, "  with errmsg = %s\n", errmsg);
    p_stdout(p_wkspc.c);
  }

  prompt_issued = 0;
  if (signal==PSIG_INT) p_quit();
}

void
on_panic(p_scr *screen)
{
  if (screen==scr) panic_count++;
  p_stdout("test3d: on_panic called\n");
  p_quit();
  prompt_issued = 0;
}

struct command {
  char *name;
  void (*action)(char *args);
  char *desc;
} commands[] = {
  { "help", &help_act, "help\n" },
  { "quit", &quit_act, "quit [value]\n" },
  { "bar", &bar_act, "bar\n" },
  { (char*)0, (void (*)(char*))0 }
};

/* ARGSUSED */
static void
help_act(char *args)
{
  int i;
  for (i=0 ; commands[i].desc ; i++)
    p_stdout(commands[i].desc);
}

/* ARGSUSED */
static void
quit_act(char *args)
{
  p_quit();
}

/* ARGSUSED */
static void
bar_act(char *args)
{
  int h;
  if (glw1) p_gldestroy(glw1), glw1 = 0;
  ignore_destroy = 1;
  if (win1) p_destroy(win1), win1 = 0;
  ignore_destroy = 0;
  menubar = !menubar;
  h = 30*menubar;
  win1 = p_window(scr, 300, 300+h, "test3d 1", P_BG,
		 P_NORESIZE | P_RGBMODEL, &win1);
  glw1 = p_glcreate(win1, 300, 300, 0, h, &glw1);
}

void
on_stdin(char *input_line)
{
  struct command *cmnd;
  int n = strspn(input_line, " \t\n\r");
  input_line += n;
  n = strcspn(input_line, " \t\n\r");
  prompt_issued = 0;
  if (n) {
    for (cmnd=commands ; cmnd->name ; cmnd++) {
      if (strncmp(cmnd->name, input_line, n)) continue;
      if (!cmnd->action) break;
      strcpy(p_wkspc.c, "doing: ");
      strncat(p_wkspc.c, input_line, n);
      strcat(p_wkspc.c, "\n");
      p_stdout(p_wkspc.c);
      input_line += n;
      input_line += strspn(input_line, " \t\n\r");
      cmnd->action(input_line);
      return;
    }

    strcpy(p_wkspc.c, "\ntest3d command not recognized: ");
    strncat(p_wkspc.c, input_line, n);
    strcat(p_wkspc.c, "\n");
    p_stderr(p_wkspc.c);
  }
}

/* ARGSUSED */
int
on_launch(int argc, char *argv[])
{
  p_quitter(&on_quit);
  p_idler(&on_idle);
  p_stdinit(&on_stdin);
  p_handler(&on_exception);
  p_gui(&on_expose, &on_destroy, &on_resize, &on_focus,
        &on_key, &on_click, &on_motion, &on_deselect, &on_panic);
  return 0;
}

int
on_idle(void)
{
  if (!scr && (panic_count<3)) {
    scr = p_connect(0);
    win1 = p_window(scr, 300, 300, "test3d 1", P_BG, P_RGBMODEL, &win1);
    glw1 = p_glcreate(win1, 300, 300, 0, 0, &glw1);
  }
  if (!prompt_issued) {
    p_stdout("ogl> ");
    prompt_issued = 1;
  }
  return 0;
}

static GLfloat rot1[3] = {30., 20., -10.};
static int glerr = 0;

void
on_expose(void *c, int *xy)
{
  p_win *w = *(p_win**)c;
  p_glwin *gl = *(p_glwin**)c;
  if (w==win1 && menubar) {
    p_color(w, P_FG);
    p_pen(w, 1, P_SOLID);
    seg_draw(w, 0,29, 300,29);
    p_font(w, P_COURIER | P_BOLD, 14, 0);
    p_text(w, 20,20, "2D plotting area above line", 27);
  } else if (gl==glw1) {
    p_glcurrent(gl);
    check_err("on_expose entry");  /* may get error before p_glcurrent */
    init_cube(rot1);
    draw_cube(rot1);
    p_glswap(gl);
    check_err("on_expose exit");
  }
}

static void
check_err(char *msg)
{
  glerr = glGetError();
  if (glerr) {
    sprintf(p_wkspc.c, "test3d: *WARNING* OpenGL error #%d at %s\n",
	    glerr, msg);
    p_stderr(p_wkspc.c);
    prompt_issued = 0;
  }
}

/* ARGSUSED */
void
on_focus(void *c,int in)
{
}

/* ARGSUSED */
void
on_key(void *c,int k,int md)
{
  p_win *w = *(p_win**)c;
  if (w==win1) {
    float dx = 0.;
    float dy = 0.;
    if (k==P_LEFT) dx -= 5.;
    else if (k==P_RIGHT) dx += 5.;
    else if (k==P_UP) dy -= 5.;
    else if (k==P_DOWN) dy += 5.;
    if (dx!=0. || dy!=0.) {
      check_err("on_key entry");
      roll_cube(dx, dy);
      check_err("on_key exit");
    }
    if (k=='q' || k=='Q') p_quit();
  }
}

static int prev_x, prev_y;
static int tracking = 0;

/* ARGSUSED */
void
on_click(void *c,int b,int md,int x,int y, unsigned long ms)
{
  p_win *w = *(p_win**)c;
  int down = (md&(1<<(b+2)))==0;
  if (w==win1 && (y>=30*menubar || !down)) {
    if (down) {
      prev_x = x;
      prev_y = y;
      p_cursor(w, P_ROTATE);
      tracking = 1;
    } else {
      p_cursor(w, P_SELECT);
      tracking = 0;
    }
  }
}

void
on_motion(void *c,int md,int x,int y)
{
  p_win *w = *(p_win**)c;
  if (w==win1 && (md&(P_BTN1|P_BTN2|P_BTN3|P_BTN4|P_BTN5)) && tracking) {
    float dx = (float)(x - prev_x);
    float dy = (float)(y - prev_y);
    prev_x = x;
    prev_y = y;
    check_err("on_motion entry");
    roll_cube(dx, dy);
    check_err("on_motion exit");
  }
}

/* ARGSUSED */
void
on_deselect(void *c)
{
}

void
on_destroy(void *c)
{
  p_win *w = *(p_win **)c;
  if (w==win2) glw2 = 0, win2 = 0;
  else if (w==win1) glw1 = 0, win1 = 0;
  if (!ignore_destroy) p_quit();
}

/* ARGSUSED */
void
on_resize(void *c,int w,int h)
{
}

void
seg_draw(p_win *w, int x0, int y0, int x1, int y1)
{
  int x[2], y[2];
  x[0] = x0;  x[1] = x1;  y[0] = y0;  y[1] = y1;
  p_i_pnts(w, x, y, 2);
  p_lines(w);
}

static void
init_cube(GLfloat *rot)
{
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glShadeModel(GL_SMOOTH);
  glEnable(GL_DEPTH_TEST);

  init_lighting();

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glFrustum(-1.0, 1.0,  -1.0, 1.0,  1.0, 10.0);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(0.0, 0.0, -5.0);
  /*
  glCullFace(GL_BACK);
  glEnable(GL_CULL_FACE);
  */
}

static void
draw_cube(GLfloat *rot)
{
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glPushMatrix();
  glRotatef(rot[0], 1.0, 0.0, 0.0);
  glRotatef(rot[1], 0.0, 1.0, 0.0);
  glRotatef(rot[2], 0.0, 0.0, 1.0);

  /* X faces */
  set_material(1., 0., 0.);
  draw_face( 1.,0.,0., 0.,0.,0.);
  draw_face(-1.,0.,0., 0.,0.,0.);

  /* Y faces */
  set_material(0., 1., 0.);
  draw_face(0., 1.,0., 0.,0.,0.);
  draw_face(0.,-1.,0., 0.,0.,0.);

  /* Z faces */
  set_material(0., 0., 1.);
  draw_face(0.,0., 1., 0.,0.,0.);
  draw_face(0.,0.,-1., 0.,0.,0.);

  glPopMatrix();
  glFlush();
}

static void
draw_face(float nx, float ny, float nz, float x, float y, float z)
{
  x += nx;
  y += ny;
  z += nz;
  glBegin(GL_POLYGON);
  glVertex3f(x+(nx==0.), y+(ny==0.), z+(nz==0.));
  glNormal3f(nx, ny, nz);
  glVertex3f(x+(nx==0.)*(ny-nz), y+(ny==0.)*(nz-nx), z+(nz==0.)*(nx-ny));
  glNormal3f(nx, ny, nz);
  glVertex3f(x-(nx==0.), y-(ny==0.), z-(nz==0.));
  glNormal3f(nx, ny, nz);
  glVertex3f(x-(nx==0.)*(ny-nz), y-(ny==0.)*(nz-nx), z-(nz==0.)*(nx-ny));
  glNormal3f(nx, ny, nz);
  glEnd();
}

static void
roll_cube(float dx, float dy)
{
  rot1[2] -= 0.5f*dx;
  if (rot1[2] > 180.f) rot1[2] -= 360.f;
  else if (rot1[2] <= -180.f) rot1[2] += 360.f;
  rot1[0] += 0.5f*dy;
  if (rot1[1] > 180.f) rot1[1] -= 360.f;
  else if (rot1[1] <= -180.f) rot1[1] += 360.f;
  p_glcurrent(glw1);
  draw_cube(rot1);
  p_glswap(glw1);
}

static float ambient[] = {0.1f, 0.1f, 0.1f, 1.0};
static float diffuse[] = {1.0, 1.0, 1.0, 1.0};
static float position0[] = {0.0, 0.0, 20.0, 0.0};
static float position1[] = {0.0, 0.0, -20.0, 0.0};
static float lmodel_ambient[] = {0.0, 1.0, 0.0, 1.0};
static float lmodel_twoside[] = {GL_FALSE};

static void
init_lighting(void)
{

  glLightfv(GL_LIGHT0, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT0, GL_POSITION, position0);
  glEnable(GL_LIGHT0);

  glLightfv(GL_LIGHT1, GL_AMBIENT, ambient);
  glLightfv(GL_LIGHT1, GL_DIFFUSE, diffuse);
  glLightfv(GL_LIGHT1, GL_POSITION, position1);
  glEnable(GL_LIGHT1);

  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);
  glLightModelfv(GL_LIGHT_MODEL_TWO_SIDE, lmodel_twoside);
  glEnable(GL_LIGHTING);
}

static void
set_material(float r, float g, float b)
{
  float front_mat_shininess[1];
  float front_mat_specular[4];
  float front_mat_diffuse[4];
  front_mat_shininess[0] = 60.0;
  front_mat_specular[0] = r;
  front_mat_specular[1] = g;
  front_mat_specular[2] = b;
  front_mat_specular[3] = 1.0;
  front_mat_diffuse[0] = r;
  front_mat_diffuse[1] = g;
  front_mat_diffuse[2] = b;
  front_mat_diffuse[3] = 1.0;
  glMaterialfv(GL_FRONT_AND_BACK, GL_SHININESS, front_mat_shininess);
  glMaterialfv(GL_FRONT_AND_BACK, GL_SPECULAR, front_mat_specular);
  glMaterialfv(GL_FRONT_AND_BACK, GL_DIFFUSE, front_mat_diffuse);
}
