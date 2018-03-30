/*
 * $Id: graph.c,v 1.12 2010-04-13 11:34:30 thiebaut Exp $
 * Define interactive graphics interface using Gist graphics package.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "gist.h"
#include "xbasic.h"
#include "hlevel.h"

/* primitive allowance for other non-X windows systems */
#ifndef DISPLAY_ENGINE
# define DISPLAY_ENGINE GpFXEngine
#endif
#ifdef NO_XLIB
# define NO_MOUSE
static int my_rgb_read(Engine *eng, GpColor *rgb, long *nx, long *ny);
# define RGB_READER my_rgb_read
#else
# ifndef NO_MOUSE
#  ifndef DISPLAY_MOUSE
#   define DISPLAY_MOUSE GxPointClick
#  endif
# endif
# define RGB_READER g_rgb_read
extern unsigned long gx_parent;
#endif
#ifdef NO_MOUSE
# ifndef DISPLAY_ZOOM_FACTOR
#  define DISPLAY_ZOOM_FACTOR myZoomFactor
static double myZoomFactor= 1.0;
# endif
#endif
#ifndef DISPLAY_ZOOM_FACTOR
# define DISPLAY_ZOOM_FACTOR gxZoomFactor
#endif

/* various X headers included by xfancy.h define True and False */
#undef True
#undef False

#include "ydata.h"
#include "yio.h"
#include "pstdlib.h"
#include "play.h"
#include <string.h>

/* Quining operation defined in parse.c */
extern void YpQuine(char *name, int nQuined);
extern int PutsAsArray(char *s);              /* from yio.c */
/* in graph0.c */
extern long QuickMeshZone(double xx, double yy, double *x, double *y,
                          int *reg, long ix, long ijx, long i,
                          long *bndy, long nbndy);
extern long *BuildMeshBndy(double *x, double *y, int *reg,
                           long ix, long ijx, long *nbndy);

extern BuiltIn Y_plg, Y_plm, Y_plc, Y_plv, Y_plf, Y_pli, Y_plt, Y_pldj;
extern BuiltIn Y_plmesh, Y_bytscl, Y_plfp;

extern BuiltIn Y_limits, Y_logxy, Y_zoom_factor, Y_unzoom;
/* Note: range function is interpreted shell for limits */

extern BuiltIn Y_window, Y_hcp_file, Y_hcp_finish, Y_plsys, Y_palette;
extern BuiltIn Y_window_geometry, Y_window_exists, Y_window_select,
  Y_window_list;

extern BuiltIn Y_fma, Y_redraw, Y_hcp, Y_hcpon, Y_hcpoff, Y_animate;

extern BuiltIn Y_plq, Y_pledit, Y_pldefault, Y_gridxy;

extern BuiltIn Y__pl_init;  /* called at initialization by graph.i */

extern BuiltIn Y_mouse, Y_contour, Y_mesh_loc, Y_pause, Y_current_window;
extern BuiltIn Y_keybd_focus, Y_rgb_read;
extern BuiltIn Y_current_mouse, Y_set_gpath;

/*--------------------------------------------------------------------------*/

static void FreeReference(void *obj);
static void Safe_free(void *vptr);

static char *SetHCPname(int n, char *name);
static char *GetHCPname(int n);
static char *hcpNames[GH_NDEVS+1];
static void SetHCPDefault(void);
static void CheckDefaultWindow(void);
static void CheckDefaultPalette(void);

static double *Get1Ddouble(Symbol *stack, long *length);
static double *Get2Ddouble(Symbol *stack, long *len1, long *len2);
static int *Get2Dint(Symbol *stack, long *len1, long *len2);
static short *Get2Dshort(Symbol *stack, long *len1, long *len2);
static double *GetDouble(Symbol *stack, long *n);
static void LegendAndHide(char *func, char *arg1, char *arg2, char *arg3,
                          char *arg4, Symbol *keySymbols[]);
static int GetLineType(Symbol *stack);
static int YgetColor(Symbol *stack);
static int GetFont(Symbol *stack);
static int GetTypeface(char *s);
static void GetJustify(Symbol *stack);
static long Safe_strlen(const char *s);
static void AllocTmpLegend(long len);
static void FreeTmpLegend(void);
static long escape_count(char *arg);
static void escape_cat(char *leg, char *arg);
static Symbol *GrabMesh(Symbol *stack, Symbol *triKey, GaQuadMesh *mesh,
                        char **y_name, char **x_name, char **r_name, int tmp);
static int *PadRegionArray(Symbol *stack, int *reg, long iMax, long jMax);
static void *CopyArray(Symbol *stack, void *xOld,
                       StructDef *base, long iMax, long jMax);
static double *CopyLevels(double *levels, long nLevels);
static void GetZCrange(double *zmn, double *zmx, double *z, int *reg,
                       int region, long iMax, long jMax, int zCompressed);
static void GetPCrange(double *zmn, double *zmx, double *z, int *reg,
                       int region, long iMax, long jMax);
static void GrabByteScale(Symbol **keySymbols, double *scale, double *offset,
                          double *zn, double *zx, double *z, int *reg,
                          int region, long iMax, long jMax, int zCompressed);
static GpColor *PushColors(double *z, long len, double zmin, double zmax,
                           double scale, double offset);

static void PrintHideLegend(char *line, int type);
static void PrintColor(char *line, int color, int suffix);
static void PrintTypeWidth(char *line, int suffix);
static void PrintMarks(char *line, int suffix);
static void PrintSuffix(int suffix);
static void PrintRegion(char *line, int suffix);
static double Safe_dbl(double x);
static void *MakePropArray(StructDef *base, long size);

static void CheckPalette(void);

static int maxColors= 200;  /* maximum number of colors for GpReadPalette */
static int hcpDump= 1;      /* whiners can't figure out how to dump colors */
static int hcpPSdefault= 1;
static int hcpOnFMA= 0;
static int defaultDPI= 75;
static int defaultLegends= 1;
static char *defaultStyle= 0;
static char *defaultPalette= 0;

/* Pointers to the default mesh set with plmesh are actually
   Yorick Array pointees.  */
static long iMesh= 0, jMesh= 0;
static double *xMesh= 0, *yMesh= 0;
static int *regMesh= 0;
static short *triangleMesh= 0;

static int curElement= -1;

static void RefMesh(GaQuadMesh *mesh);

extern int YCurrentPlotter(void); /* for style.c */

/*--------------------------------------------------------------------------*/

static long yOrigin;

static double *Get1Ddouble(Symbol *stack, long *length)
{
  Operand op;
  long n= *length;
  if (!stack->ops) YError("unexpected keyword argument (BUG?)");
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&voidOps) return 0;
  if (op.ops->promoteID>T_DOUBLE || (op.type.dims && op.type.dims->next))
    YError("expecting 1D array convertable to type double as argument");
  op.ops->ToDouble(&op);
  if (n>0 && op.type.number!=n)
    YError("1D double array must be same length as a previous argument");
  if (op.type.dims) yOrigin= op.type.dims->origin;
  else yOrigin= 1L;
  *length= op.type.number;
  return (double *)op.value;
}

static double *Get2Ddouble(Symbol *stack, long *len1, long *len2)
{
  Operand op;
  long n1= *len1;
  long n2= *len2;
  Dimension *next= 0;
  if (!stack->ops) YError("unexpected keyword argument (BUG?)");
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&voidOps) return 0;
  if (op.ops->promoteID>T_DOUBLE ||
      !op.type.dims || !(next= op.type.dims->next) || next->next)
    YError("expecting 2D array convertable to type double as argument");
  op.ops->ToDouble(&op);
  if (n1>0 && (op.type.dims->number!=n1 || next->number!=n2))
    YError("2D double array must be same shape as a previous argument");
  *len1= op.type.dims->number;
  *len2= op.type.dims->next->number;
  return (double *)op.value;
}

static int *Get2Dint(Symbol *stack, long *len1, long *len2)
{
  Operand op;
  long n1= *len1;
  long n2= *len2;
  Dimension *next= 0;
  if (!stack->ops) YError("unexpected keyword argument (BUG?)");
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&voidOps) return 0;
  if (op.ops->promoteID>T_LONG ||
      !op.type.dims || !(next= op.type.dims->next) || next->next)
    YError("expecting 2D array convertable to type int as argument");
  op.ops->ToInt(&op);
  if (n1>0 && (op.type.dims->number!=n1 || next->number!=n2))
    YError("2D int array must be same shape as a previous argument");
  *len1= op.type.dims->number;
  *len2= op.type.dims->next->number;
  return (int *)op.value;
}

static short *Get2Dshort(Symbol *stack, long *len1, long *len2)
{
  Operand op;
  long n1= *len1;
  long n2= *len2;
  Dimension *next= 0;
  if (!stack->ops) YError("unexpected keyword argument (BUG?)");
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&voidOps) return 0;
  if (op.ops->promoteID>T_LONG ||
      !op.type.dims || !(next= op.type.dims->next) || next->next)
    YError("expecting 2D array convertable to type short as argument");
  op.ops->ToShort(&op);
  if (n1>0 && (op.type.dims->number!=n1 || next->number!=n2))
    YError("2D short array must be same shape as a previous argument");
  *len1= op.type.dims->number;
  *len2= op.type.dims->next->number;
  return (short *)op.value;
}

static long Safe_strlen(const char *s)
{
  if (s) return strlen(s);
  else return 0;
}

static char *tmpLegend = 0;

static void
AllocTmpLegend(long len)
{
  if (tmpLegend) FreeTmpLegend();
  tmpLegend = p_malloc(len+1);
  tmpLegend[0] = '\0';
}

static void FreeTmpLegend(void)
{
  if (tmpLegend) {
    char *legend= tmpLegend;
    tmpLegend= 0;
    p_free(legend);
  }
}

static long escape_count(char *arg)
{
  long n= 0;
  if (arg) while (*arg) {
    if (*arg=='!' || *arg=='_' || *arg=='^') n++;
    arg++;
  }
  return n;
}

static void escape_cat(char *leg, char *arg)
{
  while (*arg) {
    if (*arg=='!' || *arg=='_' || *arg=='^') *(leg++)= '!';
    *(leg++)= *(arg++);
  }
  *leg= '\0';
}

static void LegendAndHide(char *func, char *arg1, char *arg2, char *arg3,
                          char *arg4, Symbol *keySymbols[])
{
  /* check for hide= keyword */
  if (YNotNil(keySymbols[1])) gistD.hidden= (YGetInteger(keySymbols[1])!=0);
  else gistD.hidden= 0;

  if (tmpLegend) FreeTmpLegend();

  /* check for legend= keyword -- put legend into tmpLegend */
  if (keySymbols[0]) {
    /* legend=[] is same as legend=string() */
    Symbol *stack= keySymbols[0];
    if (YNotNil(stack)) tmpLegend= p_strcpy(YGetString(stack));

  } else if (func) {
    /* construct default legend from up to 4 quined arguments */
    long len0= Safe_strlen(func);
    long len1= Safe_strlen(arg1)+escape_count(arg1);
    long len2= Safe_strlen(arg2)+escape_count(arg2);
    long len3= Safe_strlen(arg3)+escape_count(arg3);
    long len4= Safe_strlen(arg4)+escape_count(arg4);
    AllocTmpLegend(len0+len1+len2+len3+len4+6);
    if (func) strcat(tmpLegend, func);
    if (arg1) {
      escape_cat(tmpLegend+len0, arg1);
      len0+= len1;
      if (arg2) {
        strcat(tmpLegend+len0, ", ");
        escape_cat(tmpLegend+len0+2, arg2);
        len0+= 2+len2;
        if (arg3) {
          strcat(tmpLegend+len0, ", ");
          escape_cat(tmpLegend+len0+2, arg3);
          len0+= 2+len3;
          if (arg4) {
            strcat(tmpLegend+len0, ", ");
            escape_cat(tmpLegend+len0+2, arg4);
            len0+= 2+len4;
          }
        }
      }
    }
  }

  /* Put tmpLegend into gistD.legend -- it will be copied out when the
     element is created.  Only danger is pledit, since GdEdit just
     copies the pointer, not the string -- handle this case specially.  */
  gistD.legend= tmpLegend;
}

static int GetLineType(Symbol *stack)
{
  Operand op;
  if (!stack->ops) YError("unexpected keyword argument (BUG?)");
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&stringOps) {
    char *s= ((char **)op.value)[0];
    if (op.type.dims) YError("illegal line type -- need scalar string");
    if (strcmp(s, "none")==0) return L_NONE;
    else if (strcmp(s, "solid")==0) return L_SOLID;
    else if (strcmp(s, "dash")==0) return L_DASH;
    else if (strcmp(s, "dot")==0) return L_DOT;
    else if (strcmp(s, "dashdot")==0) return L_DASHDOT;
    else if (strcmp(s, "dashdotdot")==0) return L_DASHDOTDOT;
    YError("unrecognized line type keyword");
    return 0;
  } else {
    int type= (int)YGetInteger(stack);
    if (type<0) type= 0;
    else if (type>5) type= 1 + (type-1)%5;
    return type;
  }
}

static int
YgetColor(Symbol *stack)
{
  Operand op;
  if (!stack->ops) YError("unexpected keyword argument (BUG?)");
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&stringOps) {
    char *s= ((char **)op.value)[0];
    if (op.type.dims) YError("illegal color -- need scalar string");
    if (strcmp(s, "bg")==0) return P_BG;
    else if (strcmp(s, "fg")==0) return P_FG;
    else if (strcmp(s, "black")==0) return P_BLACK;
    else if (strcmp(s, "white")==0) return P_WHITE;
    else if (strcmp(s, "red")==0) return P_RED;
    else if (strcmp(s, "green")==0) return P_GREEN;
    else if (strcmp(s, "blue")==0) return P_BLUE;
    else if (strcmp(s, "cyan")==0) return P_CYAN;
    else if (strcmp(s, "magenta")==0) return P_MAGENTA;
    else if (strcmp(s, "yellow")==0) return P_YELLOW;
    else if (strcmp(s, "grayd")==0) return P_GRAYD;
    else if (strcmp(s, "grayc")==0) return P_GRAYC;
    else if (strcmp(s, "grayb")==0) return P_GRAYB;
    else if (strcmp(s, "graya")==0) return P_GRAYA;
    YError("unrecognized color keyword (fg, bg, or 8 primaries only)");
    return 0;
  } else {
    Dimension *dims;
    int *color = YGet_I(stack, 0, &dims);
    if (dims && (dims->next || dims->number!=3))
      YError("color must be integer scalar or triple (rgb)");
    if (dims) return P_RGB(color[0],color[1],color[2]);
    if (color[0] < 256) return (color[0]&0xff); /* indexed color */
    return ((color[0]&0xffffff) | 0x01000000);  /* reform packed RGB color */
  }
}

static int GetFont(Symbol *stack)
{
  Operand op;
  if (!stack->ops) YError("unexpected keyword argument (BUG?)");
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&stringOps) {
    char *s= ((char **)op.value)[0];
    if (op.type.dims) YError("illegal font -- need scalar string");
    if (strncmp(s, "courier", 7)==0)
      return T_COURIER | GetTypeface(&s[7]);
    else if (strncmp(s, "times", 5)==0)
      return T_TIMES | GetTypeface(&s[5]);
    else if (strncmp(s, "helvetica", 9)==0)
      return T_HELVETICA | GetTypeface(&s[9]);
    else if (strncmp(s, "symbol", 6)==0)
      return T_SYMBOL | GetTypeface(&s[6]);
    else if (strncmp(s, "schoolbook", 10)==0)
      return T_NEWCENTURY | GetTypeface(&s[10]);
    YError("unrecognized font keyword");
    return 0;
  } else {
    return (int)YGetInteger(stack);
  }
}

static int GetTypeface(char *s)
{
  int face= 0;
  while (*s) {
    if (*s=='B' && !(face&T_BOLD)) face|= T_BOLD;
    else if (*s=='I' && !(face&T_ITALIC)) face|= T_ITALIC;
    else YError("illegal font keyword suffix -- B is bold, I is italic");
    s++;
  }
  return face;
}

static void GetJustify(Symbol *stack)
{
  Operand op;
  if (!stack->ops) YError("unexpected keyword argument (BUG?)");
  stack->ops->FormOperand(stack, &op);
  if (op.ops==&stringOps) {
    char *s= ((char **)op.value)[0];
    if (op.type.dims) YError("illegal justify -- need scalar string");
    if (*s=='N') { gistA.t.alignH= TH_NORMAL; s++; }
    else if (*s=='L') { gistA.t.alignH= TH_LEFT; s++; }
    else if (*s=='C') { gistA.t.alignH= TH_CENTER; s++; }
    else if (*s=='R') { gistA.t.alignH= TH_RIGHT; s++; }
    else { while (*s) s++; }
    if (*s=='N') gistA.t.alignV= TV_NORMAL;
    else if (*s=='T') gistA.t.alignV= TV_TOP;
    else if (*s=='C') gistA.t.alignV= TV_CAP;
    else if (*s=='H') gistA.t.alignV= TV_HALF;
    else if (*s=='A') gistA.t.alignV= TV_BASE;
    else if (*s=='B') gistA.t.alignV= TV_BOTTOM;
    else YError("unrecognized justify keyword");
  } else {
    int justify= (int)YGetInteger(stack);
    gistA.t.alignH= justify&3;
    gistA.t.alignV= justify>>2;
  }
}

/*--------------------------------------------------------------------------*/

#undef N_KEYWORDS
#define N_KEYWORDS 19
static char *plgKeys[N_KEYWORDS+1]= {
  "legend", "hide", "color", "type", "width",
  "marks", "mcolor", "marker", "msize", "mspace", "mphase",
  "rays", "arrowl", "arroww", "rspace", "rphase",
  "closed", "smooth", "n", 0 };

void Y_plg(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, plgKeys, keySymbols);
  int iPass= 0;
  double *x= 0, *y= 0;
  char *x_name= 0, *y_name= 0;
  long n= 0;
  long nparts= 0, *np= 0;
  int defaultX= 0;

  if (!CalledAsSubroutine())
    YError("plg may not be invoked as a function -- subroutine only");

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }

    if (iPass==0) y= Get1Ddouble(stack, &n);
    else if (iPass==1) y_name= YGetString(stack);
    else if (iPass==2) x= Get1Ddouble(stack, &n);
    else if (iPass==3) x_name= YGetString(stack);
    else YError("plg takes at most two non-keyword arguments");

    iPass++;
    stack++;
  }

  if (!y) YError("plg needs at least one non-keyword argument");

  /* set legend and hide in gistD */
  CheckDefaultWindow();
  LegendAndHide("\001: plg, ", y_name, x_name, (char *)0, (char *)0,
                keySymbols);

  /* set properties, starting from defaults for decorated polylines */
  GhGetLines();

  if (YNotNil(keySymbols[2]))
    gistA.l.color= gistA.m.color= YgetColor(keySymbols[2]);
  if (YNotNil(keySymbols[3]))
    gistA.l.type= GetLineType(keySymbols[3]);
  if (YNotNil(keySymbols[4]))
    gistA.l.width= YGetReal(keySymbols[4]);
  if (YNotNil(keySymbols[5]))
    gistA.dl.marks= (YGetInteger(keySymbols[5])!=0);
  if (YNotNil(keySymbols[6]))
    gistA.m.color= YgetColor(keySymbols[6]);
  if (YNotNil(keySymbols[7]))
    gistA.m.type= (int)YGetInteger(keySymbols[7]);
  if (YNotNil(keySymbols[8]))
    gistA.m.size= YGetReal(keySymbols[8]);
  if (YNotNil(keySymbols[9]))
    gistA.dl.mSpace= YGetReal(keySymbols[9]);
  if (YNotNil(keySymbols[10]))
    gistA.dl.mPhase= YGetReal(keySymbols[10]);
  if (YNotNil(keySymbols[11]))
    gistA.dl.rays= (YGetInteger(keySymbols[11])!=0);
  if (YNotNil(keySymbols[12]))
    gistA.dl.arrowL= YGetReal(keySymbols[12]);
  if (YNotNil(keySymbols[13]))
    gistA.dl.arrowW= YGetReal(keySymbols[13]);
  if (YNotNil(keySymbols[14]))
    gistA.dl.rSpace= YGetReal(keySymbols[14]);
  if (YNotNil(keySymbols[15]))
    gistA.dl.rPhase= YGetReal(keySymbols[15]);
  if (YNotNil(keySymbols[16]))
    gistA.dl.closed= (YGetInteger(keySymbols[16])!=0);
  if (YNotNil(keySymbols[17]))
    gistA.dl.smooth= (YGetInteger(keySymbols[17])!=0);

  if (YNotNil(keySymbols[18])) {
    long i, ntot;
    Dimension *dims;
    np= YGet_L(keySymbols[18], 0, &dims);
    if (!dims || dims->next) YError("n= keyword must be 1D in plg");
    nparts= dims->number;
    for (i=ntot=0 ; i<nparts ; i++) ntot+= np[i];
    if (ntot!=n) YError("n= keyword must sum to numberof(y) in plg");
  }

  if (!x) {
    /* default x runs from origin of y dimension in steps of 1 */
    long i;
    Array *array=
      PushDataBlock(NewArray(&doubleStruct,
                             NewDimension(n, 1L, (Dimension *)0)));
    array->type.dims->references--;
    x= array->value.d;
    defaultX= 1;
    if (!nparts) {
      for (i=0 ; i<n ; i++) x[i]= yOrigin+(double)i;
    } else {
      long j;
      for (j=0 ; j<nparts ; j++)
        for (i=0 ; i<np[j] ; i++) x[i]= yOrigin+(double)i;
    }
  }

  /* add the graph(s) to the current display list */
  if (!nparts) {
    curElement= -1;
    curElement= GdLines(n, x, y);
  } else {
    int ce= 0, cel= 0;
    curElement= -1;
    while (nparts--) {
      cel= GdLines(np[0], x, y);
      if (cel<0) ce= cel;
      x+= np[0];
      y+= np[0];
      np++;
    }
    if (!ce) curElement= cel;
  }
  if (curElement<0) YWarning("Gist GdLines plotter failed");

  Drop(nArgs+defaultX);
}

#undef N_KEYWORDS
#define N_KEYWORDS 8
static char *plmKeys[N_KEYWORDS+1]= {
  "legend", "hide", "color", "type", "width", "region", "boundary",
  "inhibit", 0 };

void Y_plm(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, plmKeys, keySymbols);
  char *y_name= 0, *x_name= 0, *r_name= 0;
  GaQuadMesh mesh;

  if (!CalledAsSubroutine())
    YError("plm may not be invoked as a function -- subroutine only");

  stack= GrabMesh(stack, (Symbol *)0, &mesh, &y_name, &x_name, &r_name, 0);
  while (stack<=sp) {
    if (!stack->ops) stack+= 2;
    else YError("plm takes at most three non-keyword arguments");
  }

  /* set legend and hide in gistD */
  CheckDefaultWindow();
  LegendAndHide("plm, ", y_name, x_name, r_name, (char *)0, keySymbols);

  /* set properties, starting from defaults for meshes */
  GhGetMesh();
  gistD.region= 0;
  gistD.boundary= 0;
  gistD.inhibit= 0;

  if (YNotNil(keySymbols[2]))
    gistA.l.color= YgetColor(keySymbols[2]);
  if (YNotNil(keySymbols[3]))
    gistA.l.type= GetLineType(keySymbols[3]);
  if (YNotNil(keySymbols[4]))
    gistA.l.width= YGetReal(keySymbols[4]);
  if (YNotNil(keySymbols[5]))
    gistD.region= (int)YGetInteger(keySymbols[5]);
  if (YNotNil(keySymbols[6]))
    gistD.boundary= (YGetInteger(keySymbols[6])!=0);
  if (YNotNil(keySymbols[7]))
    gistD.inhibit= (int)YGetInteger(keySymbols[7]);

  curElement= -1;
  curElement= GdMesh(NOCOPY_MESH, &mesh, gistD.region, gistD.boundary,
                     gistD.inhibit);
  if (curElement<0) YWarning("Gist GdMesh plotter failed");

  RefMesh(&mesh);
  Drop(nArgs);
}

#undef N_KEYWORDS
#define N_KEYWORDS 1
static char *meshKeys[N_KEYWORDS+1]= { "triangle", 0 };

void Y_plmesh(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, meshKeys, keySymbols);
  GaQuadMesh mesh;

  if (nArgs==0) {
    Safe_free(&xMesh);
    Safe_free(&yMesh);
    Safe_free(&regMesh);
    Safe_free(&triangleMesh);
  }

  stack= GrabMesh(stack, keySymbols[0], &mesh,
                  (char **)0, (char **)0, (char **)0, 0);

  if (mesh.x!=xMesh) {
    /* a new default mesh has been defined */
    Safe_free(&xMesh);
    Safe_free(&yMesh);
    Safe_free(&regMesh);
    Safe_free(&triangleMesh);
    RefMesh(&mesh);
    iMesh= mesh.iMax;
    jMesh= mesh.jMax;
    xMesh= mesh.x;
    yMesh= mesh.y;
    if (mesh.reg) {
      regMesh= mesh.reg;
    } else {
      /* supply a default region array now */
      long ijMax= iMesh*jMesh;
      long i= ijMax+iMesh+1;
      Array *array=
        PushDataBlock(NewArray(&intStruct,
                               NewDimension(i, 1L, (Dimension *)0)));
      int *r= array->value.i;
      array->type.dims->references--;

      for (i=0 ; i<=iMesh ; i++) r[i]= 0;
      for (i=iMesh+1 ; i<ijMax ; i++) r[i]= 1;
      for (i=0 ; i<=iMesh ; i++) r[ijMax+i]= 0;
      for (i=2*iMesh ; i<ijMax ; i+=iMesh) r[i]= 0;

      array->references++;  /* preserve across Drop */
      regMesh= array->value.i;
      Drop(1);
    }
    triangleMesh= mesh.triangle;

  } else {
    /* perhaps reg or triangle has been updated */
    if (mesh.x==xMesh) {
      mesh.x= mesh.y= 0;
      if (mesh.reg==regMesh) mesh.reg= 0;
      if (mesh.triangle==triangleMesh) mesh.triangle= 0;
    }
    RefMesh(&mesh);
    if (mesh.reg) {
      Safe_free(&regMesh);
      regMesh= mesh.reg;
    }
    if (mesh.triangle) {
      Safe_free(&triangleMesh);
      triangleMesh= mesh.triangle;
    }
  }

  Drop(nArgs);
}

static void Safe_free(void *vptr)
{
  void **ptr= vptr;
  void *obj= *ptr;
  *ptr= 0;  /* zero reference before freeing object */
  FreeReference(obj);
}

static Symbol *GrabMesh(Symbol *stack, Symbol *triKey, GaQuadMesh *mesh,
                        char **y_name, char **x_name, char **r_name, int tmp)
{
  Symbol *yStack= 0, *xStack= 0, *rStack= 0;
  int stackInc= y_name? 1 : 2;
  int iPass= 0;

  mesh->x= mesh->y= 0;
  mesh->reg= 0;
  mesh->iMax= mesh->jMax= 0;
  mesh->triangle= 0;

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }

    if (iPass==0)
      mesh->y= Get2Ddouble(yStack= stack, &mesh->jMax, &mesh->iMax);
    else if (iPass==1) *y_name= YGetString(stack);
    else if (iPass==2)
      mesh->x= Get2Ddouble(xStack= stack, &mesh->jMax, &mesh->iMax);
    else if (iPass==3) *x_name= YGetString(stack);
    else if (iPass==4)
      mesh->reg= Get2Dint(rStack= stack, &mesh->jMax, &mesh->iMax);
    else if (iPass==5) *r_name= YGetString(stack);
    else break;

    iPass+= stackInc;
    stack++;
  }

  if (YNotNil(triKey))
    mesh->triangle= Get2Dshort(triKey, &mesh->jMax, &mesh->iMax);

  if ((mesh->x!=0)^(mesh->y!=0))
    YError("both y and x arrays must be specified for a mesh");

  if (!mesh->x) {
    /* neither y nor x have been specified -- use defaults */
    if (!xMesh)
      YError("no default mesh exists to define y and x -- use plmesh");
    if ((mesh->reg || mesh->triangle) &&
        (iMesh!=mesh->iMax || jMesh!=mesh->jMax))
      YError("ireg and triangle must have same dimensions as default mesh");
    mesh->iMax= iMesh;
    mesh->jMax= jMesh;
    mesh->x= xMesh;
    mesh->y= yMesh;

  } else {
    /* both y and x have been specified -- copy them for Gist */
    if (mesh->iMax<2 || mesh->jMax<2)
      YError("a mesh have dimensions of at least 2-by-2");
    mesh->x=
      CopyArray(xStack, mesh->x, &doubleStruct, mesh->iMax, mesh->jMax);
    mesh->y=
      CopyArray(yStack, mesh->y, &doubleStruct, mesh->iMax, mesh->jMax);
  }

  /* the Gist region array requires guard zones beyond iMax*jMax */
  if (mesh->reg) mesh->reg=
    PadRegionArray(rStack, mesh->reg, mesh->iMax, mesh->jMax);
  else if (mesh->x==xMesh)
    mesh->reg= regMesh;

  if (mesh->triangle) {
    if (!tmp)
      mesh->triangle= CopyArray(triKey, mesh->triangle, &shortStruct,
                                mesh->iMax, mesh->jMax);
  } else if (mesh->x==xMesh && triKey) {
    mesh->triangle= triangleMesh;
  }

  return stack;
}

static void RefMesh(GaQuadMesh *mesh)
{
  if (mesh->x) ((Array *)Pointee(mesh->x))->references++;
  if (mesh->y) ((Array *)Pointee(mesh->y))->references++;
  if (mesh->reg) ((Array *)Pointee(mesh->reg))->references++;
  if (mesh->triangle) ((Array *)Pointee(mesh->triangle))->references++;
}

static int *PadRegionArray(Symbol *stack, int *reg, long iMax, long jMax)
{
  long ijMax= iMax*jMax;
  long i= ijMax+iMax+1;
  Array *array=
    PushDataBlock(NewArray(&intStruct, NewDimension(i, 1L, (Dimension *)0)));
  int *r= array->value.i;
  array->type.dims->references--;

  for (i=0 ; i<=iMax ; i++) r[i]= 0;
  for (i=iMax+1 ; i<ijMax ; i++) r[i]= reg[i];
  for (i=0 ; i<=iMax ; i++) r[ijMax+i]= 0;
  for (i=2*iMax ; i<ijMax ; i+=iMax) r[i]= 0;

  PopTo(stack);
  return r;
}

static void *CopyArray(Symbol *stack, void *xOld,
                       StructDef *base, long iMax, long jMax)
{
  if (stack->ops==&dataBlockSym &&
      (stack->value.db->references || !stack->value.db->ops->isArray)) {
    long len= iMax*jMax;
    Array *array=
      PushDataBlock(NewArray(base, NewDimension(len, 1L, (Dimension *)0)));
    void *x= array->value.c;
    array->type.dims->references--;
    base->Copy(base, x, xOld, len);
    PopTo(stack);
    return x;
  } else {
    /* no need to copy temporaries */
    return xOld;
  }
}

#undef N_KEYWORDS
#define N_KEYWORDS 15
static char *plcKeys[N_KEYWORDS+1]= {
  "legend", "hide", "region", "color", "type", "width",
  "marks", "mcolor", "marker", "msize", "mspace", "mphase",
  "smooth", "triangle", "levs", 0 };

static double *tmpLevels= 0;

void Y_plc(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, plcKeys, keySymbols);
  char *z_name= 0, *y_name= 0, *x_name= 0, *r_name= 0;
  long iMax= 0, jMax= 0, nLevels= 0;
  double *z= 0, *levels= 0;
  GaQuadMesh mesh;

  if (!CalledAsSubroutine())
    YError("plc may not be invoked as a function -- subroutine only");

  if (stack<sp) {
    z= Get2Ddouble(stack++, &jMax, &iMax);
    z_name= YGetString(stack++);
  }
  if (!z) YError("plc needs at least one non-keyword argument");
  stack= GrabMesh(stack, keySymbols[13], &mesh, &y_name, &x_name, &r_name, 0);
  while (stack<=sp) {
    if (!stack->ops) stack+= 2;
    else YError("plc takes at most four non-keyword arguments");
  }
  if (mesh.iMax!=iMax || mesh.jMax!=jMax)
    YError("z array must have same dimensions as mesh in plc");

  /* set legend and hide in gistD */
  CheckDefaultWindow();
  LegendAndHide("\001: plc, ", z_name, y_name, x_name, r_name, keySymbols);

  /* set properties, starting from defaults for decorated polylines */
  GhGetLines();
  gistD.region= 0;

  if (YNotNil(keySymbols[2]))
    gistD.region= (int)YGetInteger(keySymbols[2]);
  if (YNotNil(keySymbols[3]))
    gistA.l.color= gistA.m.color= YgetColor(keySymbols[3]);
  if (YNotNil(keySymbols[4]))
    gistA.l.type= GetLineType(keySymbols[4]);
  if (YNotNil(keySymbols[5]))
    gistA.l.width= YGetReal(keySymbols[5]);
  if (YNotNil(keySymbols[6]))
    gistA.dl.marks= (YGetInteger(keySymbols[6])!=0);
  if (YNotNil(keySymbols[7]))
    gistA.m.color= YgetColor(keySymbols[7]);
  if (YNotNil(keySymbols[8]))
    gistA.m.type= (int)YGetInteger(keySymbols[8]);
  if (YNotNil(keySymbols[9]))
    gistA.m.size= YGetReal(keySymbols[9]);
  if (YNotNil(keySymbols[10]))
    gistA.dl.mSpace= YGetReal(keySymbols[10]);
  if (YNotNil(keySymbols[11]))
    gistA.dl.mPhase= YGetReal(keySymbols[11]);
  if (YNotNil(keySymbols[12]))
    gistA.dl.smooth= (YGetInteger(keySymbols[12])!=0);

  /* set contour levels */
  if (YNotNil(keySymbols[14])) {
    levels= Get1Ddouble(keySymbols[14], &nLevels);
    if (levels)
      levels= CopyLevels(levels, nLevels);
  }

  if (!levels) {
    /* create a default set of contour levels now */
    int i;
    double zmin, zmax, step;

    nLevels= 8;
    levels= CopyLevels((double *)0, nLevels);
    GetPCrange(&zmin, &zmax, z, mesh.reg, gistD.region, iMax, jMax);

    step= (zmax-zmin)/8.0;
    levels[0]= zmin+0.5*step;
    for (i=1 ; i<nLevels ; i++) levels[i]= levels[i-1]+step;
  }

  curElement= -1;
  curElement=
    GdContours(NOCOPY_MESH, &mesh, gistD.region, z, levels, (int)nLevels);
  if (curElement<0) YWarning("Gist GdContour plotter failed");
  tmpLevels= 0;  /* Gist now owns this pointer */

  RefMesh(&mesh);
  Drop(nArgs);
}

static double *CopyLevels(double *levels, long nLevels)
{
  long i;
  double *tmp= tmpLevels;
  tmpLevels= 0;
  if (tmp) p_free(tmp);
  tmpLevels= p_malloc(sizeof(double)*nLevels);
  for (i=0 ; i<nLevels ; i++) tmpLevels[i]= levels? levels[i] : 0.0;
  return tmpLevels;
}

static void GetZCrange(double *zmn, double *zmx, double *z, int *reg,
                       int region, long iMax, long jMax, int zCompressed)
{
  double zmin= 0.0, zmax= 0.0;
  long i, j= iMax-1;
  long len= (zCompressed? j : iMax)*(jMax-1);

  if (zCompressed) {
    long len= (iMax-1)*(jMax-1);
    if (reg) reg+= iMax+1;
    for (i=0 ; i<len ; i++) {   /* first loop finds first z */
      if (reg? (region? (*reg==region) : (*reg!=0)) : 1) {
        zmin= zmax= z[i];
        break;
      }
      if (reg) {
        if (!(--j)) { reg+= 2; j= iMax-1; }
        else reg++;
      }
    }
    if (reg) {
      if (!(--j)) { reg+= 2; j= iMax-1; }
      else reg++;
    }
    for (i++ ; i<len ; i++) {   /* second loop judges extreme values */
      if (reg? (region? (*reg==region) : (*reg!=0)) : 1) {
        if (zmin>z[i]) zmin= z[i];
        else if (zmax<z[i]) zmax= z[i];
      }
      if (reg) {
        if (!(--j)) { reg+= 2; j= iMax-1; }
        else reg++;
      }
    }

  } else {
    z+= iMax+1;                 /* GrabMesh guarantees at least 2-by-2 */
    if (reg) reg+= iMax+1;
    for (i=1 ; i<len ; i++) {   /* first loop finds first z */
      if (--j) {
        if (reg? (region? (*reg==region) : (*reg!=0)) : 1) {
          zmin= zmax= z[i];
          break;
        }
      } else {
        j= iMax;
      }
    }
    for (i++ ; i<len ; i++) {   /* second loop judges extreme values */
      if (--j) {
        if (reg? (region? (*reg==region) : (*reg!=0)) : 1) {
          if (zmin>z[i]) zmin= z[i];
          else if (zmax<z[i]) zmax= z[i];
        }
      } else {
        j= iMax;
      }
    }
  }

  *zmn= zmin;
  *zmx= zmax;
}

static void GetPCrange(double *zmn, double *zmx, double *z, int *reg,
                       int region, long iMax, long jMax)
{
  double zmin= 0.0, zmax= 0.0;
  long i, len= iMax*jMax;

  for (i=0 ; i<len ; i++) {     /* first loop finds first z */
    if (reg? (region?
              (reg[i]==region || reg[i+1]==region ||
               reg[i+iMax]==region || reg[i+iMax+1]==region) :
              (reg[i] || reg[i+1] || reg[i+iMax] || reg[i+iMax+1])) : 1) {
      zmin= zmax= z[i];
      break;
    }
  }

  for ( ; i<len ; i++) {        /* second loop judges extreme values */
    if (reg? (region?
              (reg[i]==region || reg[i+1]==region ||
               reg[i+iMax]==region || reg[i+iMax+1]==region) :
              (reg[i] || reg[i+1] || reg[i+iMax] || reg[i+iMax+1])) : 1) {
      if (zmin>z[i]) zmin= z[i];
      else if (zmax<z[i]) zmax= z[i];
    }
  }

  *zmn= zmin;
  *zmx= zmax;
}

#undef N_KEYWORDS
#define N_KEYWORDS 8
static char *plvKeys[N_KEYWORDS+1]= {
  "legend", "hide", "region",
  "color", "hollow", "width", "aspect", "scale", 0 };

void Y_plv(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, plvKeys, keySymbols);
  char *v_name= 0, *u_name= 0, *y_name= 0, *x_name= 0, *r_name= 0;
  long iMax= 0, jMax= 0;
  double *u= 0, *v= 0, scale;
  GaQuadMesh mesh;
  int iPass= 0;

  if (!CalledAsSubroutine())
    YError("plv may not be invoked as a function -- subroutine only");

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }
    if (iPass==0) v= Get2Ddouble(stack, &jMax, &iMax);
    else if (iPass==1) v_name= YGetString(stack);
    else if (iPass==2) u= Get2Ddouble(stack, &jMax, &iMax);
    else if (iPass==3) u_name= YGetString(stack);
    else break;
    iPass++;
    stack++;
  }
  if (!u || !v) YError("plv needs at least two non-keyword arguments");
  stack= GrabMesh(stack, (Symbol *)0, &mesh, &y_name, &x_name, &r_name, 0);
  while (stack<=sp) {
    if (!stack->ops) stack+= 2;
    else YError("plv takes at most five non-keyword arguments");
  }
  if (mesh.iMax!=iMax || mesh.jMax!=jMax)
    YError("v and u arrays must have same dimensions as mesh in plv");

  /* set legend and hide in gistD */
  CheckDefaultWindow();
  LegendAndHide("plv, ", v_name, u_name, y_name, x_name, keySymbols);

  /* set properties, starting from defaults for vectors */
  GhGetVectors();
  gistD.region= 0;

  if (YNotNil(keySymbols[2]))
    gistD.region= (int)YGetInteger(keySymbols[2]);
  if (YNotNil(keySymbols[3]))
    gistA.l.color= gistA.f.color= YgetColor(keySymbols[3]);
  if (YNotNil(keySymbols[4]))
    gistA.vect.hollow= (YGetInteger(keySymbols[4])!=0);
  if (YNotNil(keySymbols[5]))
    gistA.l.width= YGetReal(keySymbols[5]);
  if (YNotNil(keySymbols[6]))
    gistA.vect.aspect= YGetReal(keySymbols[6]);

  /* set vector scale factor */
  if (YNotNil(keySymbols[7])) {
    scale= YGetReal(keySymbols[7]);

  } else {
    /* set vector scale factor to make maximum vector length a
       "typical" zone dimension */
    double umin, umax, vmin, vmax, xmin, xmax, ymin, ymax;

    GetPCrange(&xmin, &xmax, mesh.x, mesh.reg, gistD.region, iMax, jMax);
    GetPCrange(&ymin, &ymax, mesh.y, mesh.reg, gistD.region, iMax, jMax);
    GetPCrange(&umin, &umax, u, mesh.reg, gistD.region, iMax, jMax);
    GetPCrange(&vmin, &vmax, v, mesh.reg, gistD.region, iMax, jMax);

    umax-= umin;
    vmax-= vmin;
    if (vmax>umax) umax= vmax;
    xmax= (xmax-xmin)+(ymax-ymin);
    xmax/= (iMax+jMax);

    if (umax>0.0) scale= xmax/umax;
    else scale= 1.0;
  }

  curElement= -1;
  curElement= GdVectors(NOCOPY_MESH, &mesh, gistD.region, u, v, scale);
  if (curElement<0) YWarning("Gist GdVectors plotter failed");

  RefMesh(&mesh);
  Drop(nArgs);
}

#undef N_KEYWORDS
#define N_KEYWORDS 9
static char *plfKeys[N_KEYWORDS+1]= {
  "legend", "hide", "region", "top", "cmin", "cmax",
  "edges", "ecolor", "ewidth", 0 };

void Y_plf(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, plfKeys, keySymbols);
  char *z_name= 0, *y_name= 0, *x_name= 0, *r_name= 0;
  long iMax= 0, jMax= 0;
  double *z= 0;
  GpColor *zc= 0;
  GaQuadMesh mesh;
  int convertedZ= 0;
  int rgb = 0;

  if (!CalledAsSubroutine())
    YError("plf may not be invoked as a function -- subroutine only");

  if (stack<sp) {
    Operand op;
    if (stack->ops==&referenceSym) ReplaceRef(stack);
    stack->ops->FormOperand(stack, &op);
    if (op.ops!=&charOps) {
      z= Get2Ddouble(stack, &jMax, &iMax);
    } else {
      Dimension *next= 0;
      if (!op.type.dims || !(next= op.type.dims->next) || next->next) {
        if (!next || !next->next || next->next->next ||
            next->next->number!=3)
          YError("expecting NXxNY or 3xNXxNY array as argument to plf");
        rgb = 1;
      }
      iMax= next->number;
      jMax= op.type.dims->number;
      zc= op.value;
    }
    stack++;
    z_name= YGetString(stack++);
  }
  stack= GrabMesh(stack, (Symbol *)0, &mesh, &y_name, &x_name, &r_name, 0);
  while (stack<=sp) {
    if (!stack->ops) stack+= 2;
    else YError("plf takes at most four non-keyword arguments");
  }
  if ((z || zc) && ((mesh.iMax!=iMax || mesh.jMax!=jMax) &&
                    (mesh.iMax!=iMax+1 || mesh.jMax!=jMax+1)))
    YError("z array must have same or 1 smaller dimensions as mesh in plf");

  /* set legend and hide in gistD */
  CheckDefaultWindow();
  CheckDefaultPalette();
  LegendAndHide("plf, ", z_name, y_name, x_name, r_name, keySymbols);

  gistD.region= 0;
  if (YNotNil(keySymbols[2]))
    gistD.region= (int)YGetInteger(keySymbols[2]);

  if (!zc && z) {
    /* need to generate colors array on stack now */
    double zmin, zmax, scale, offset;

    GrabByteScale(&keySymbols[3], &scale, &offset, &zmin, &zmax,
                  z, mesh.reg, gistD.region, mesh.iMax, mesh.jMax,
                  mesh.iMax!=iMax);
    zc= PushColors(z, iMax*jMax, zmin, zmax, scale, offset);
    convertedZ= 1;
  }

  GhGetFill();
  if (YNotNil(keySymbols[6]))
    gistA.e.type= YGetInteger(keySymbols[6])? L_SOLID : L_NONE;
  if (YNotNil(keySymbols[7]))
    gistA.e.color= YgetColor(keySymbols[7]);
  if (YNotNil(keySymbols[8]))
    gistA.e.width= YGetReal(keySymbols[8]);
  gistA.rgb = rgb;

  if (mesh.iMax==iMax) zc += rgb? 3*(iMax+1) : iMax+1;
  curElement= -1;
  curElement= GdFillMesh(NOCOPY_MESH, &mesh, gistD.region, zc, iMax);
  if (curElement<0) YWarning("Gist GdFillMesh plotter failed");

  RefMesh(&mesh);
  Drop(nArgs+convertedZ);
}

static void GrabByteScale(Symbol **keySymbols, double *scale, double *offset,
                          double *zn, double *zx, double *z, int *reg,
                          int region, long iMax, long jMax, int zCompressed)
{
  int top;
  double zmin= 0.0, zmax= 0.0;
  int minGiven, maxGiven;
  GpColorCell *palette;

  /* get any parameters specified as keywords */
  if (YNotNil(keySymbols[0]))
    top= (int)YGetInteger(keySymbols[0]);
  else
    top= GhGetPalette(-1,&palette)-1;
  if ((minGiven= YNotNil(keySymbols[1])))
    zmin= YGetReal(keySymbols[1]);
  if ((maxGiven= YNotNil(keySymbols[2])))
    zmax= YGetReal(keySymbols[2]);

  /* fill in zmin and zmax from data if not specified */
  if (!minGiven || !maxGiven) {
    double zmn, zmx;
    GetZCrange(&zmn, &zmx, z, reg, region, iMax, jMax, zCompressed);
    if (!minGiven) zmin= zmn;
    if (!maxGiven) zmax= zmx;
  }

  /* adjust zmin and zmax to avert numerical catastrophes */
  if (zmin>zmax) { double tmp= zmin; zmin= zmax; zmax= tmp; }
  else if (zmin==zmax) {
    if (zmin>0.0) { zmin= 0.9999*zmin; zmax= 1.0001*zmax; }
    if (zmin<0.0) { zmin= 1.0001*zmin; zmax= 0.9999*zmax; }
    else { zmin= -0.0001; zmax= 0.0001; }
  }
  *zn= zmin;
  *zx= zmax;

  /* adjust top value if it is silly */
  if (top<0 || top>255) top= 255;

  /* (byte value)= scale*(z cut off at zmin, zmax)+offset
     maps from z to interval [0, top] */
  *scale= (double)top/(zmax-zmin);
  *offset= zmin-(0.4999/(*scale));        /* zmin->0.5, zmax->top+0.5 */
}

static GpColor *PushColors(double *z, long len, double zmin, double zmax,
                           double scale, double offset)
{
  long i;
  double zz;
  Array *array=
    PushDataBlock(NewArray(&charStruct,
                           NewDimension(len, 1L, (Dimension *)0)));
  GpColor *zc= (GpColor *)array->value.c;
  array->type.dims->references--;

  for (i=0 ; i<len ; i++) {
    zz= z[i];
    if (zz<zmin) zz= zmin;
    else if (zz>zmax) zz= zmax;
    zc[i]= (int)((zz-offset)*scale);
  }

  return zc;
}

#undef N_KEYWORDS
#define N_KEYWORDS 5
static char *pliKeys[N_KEYWORDS+1]= {
  "legend", "hide", "top", "cmin", "cmax", 0 };

void Y_pli(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, pliKeys, keySymbols);
  char *z_name= 0;
  double *z= 0, x0, y0, x1, y1;
  long iMax= 0, jMax= 0;
  GpColor *zc= 0;
  int convertedZ= 0;
  int iPass= 0;
  int rgb = 0;

  if (!CalledAsSubroutine())
    YError("pli may not be invoked as a function -- subroutine only");

  x0= y0= x1= y1= 0.0;
  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }
    if (iPass==0) {
      Operand op;
      if (stack->ops==&referenceSym) ReplaceRef(stack);
      stack->ops->FormOperand(stack, &op);
      if (op.ops!=&charOps) {
        z= Get2Ddouble(stack, &jMax, &iMax);
      } else {
        Dimension *next= 0;
        if (!op.type.dims || !(next= op.type.dims->next) || next->next) {
          if (!next || !next->next || next->next->next ||
              next->next->number!=3)
            YError("expecting NXxNY or 3xNXxNY array as argument to pli");
          rgb = 1;
        }
        iMax= next->number;
        jMax= op.type.dims->number;
        zc= op.value;
      }
    } else if (iPass==1) z_name= YGetString(stack);
    else if (iPass==2) x0= YGetReal(stack);
    else if (iPass==3) y0= YGetReal(stack);
    else if (iPass==4) x1= YGetReal(stack);
    else if (iPass==5) y1= YGetReal(stack);
    else YError("pli takes at most five non-keyword arguments");
    iPass++;
    stack++;
  }
  if (!z && !zc) YError("pli needs at least one non-keyword argument");

  /* handle defaulted corner values */
  if (iPass!=2 && iPass!=4 && iPass!=6)
      YError("pli needs either 0, 1, or 2 corner (x,y) points");
  if (iPass==2) {
    /* no corners specified */
    x0= y0= 0.0;
    x1= (double)iMax;
    y1= (double)jMax;
  } else if (iPass==4) {
    /* two corners specified */
    x1= x0;
    y1= y0;
    x0= y0= 0.0;
  }

  /* set legend and hide in gistD */
  CheckDefaultWindow();
  CheckDefaultPalette();
  LegendAndHide("pli, ", z_name, (char *)0,(char *)0,(char *)0, keySymbols);

  if (!zc) {
    /* need to generate colors array on stack now */
    double zmin, zmax, scale, offset;

    GrabByteScale(&keySymbols[2], &scale, &offset, &zmin, &zmax,
                  z, (int *)0, 0, iMax+1, jMax+1, 1);
    zc= PushColors(z, iMax*jMax, zmin, zmax, scale, offset);
    convertedZ= 1;
  }

  gistA.rgb = rgb;

  curElement= -1;
  curElement= GdCells(x0, y0, x1, y1, iMax, jMax, iMax, zc);
  if (curElement<0) YWarning("Gist GdCells plotter failed");

  Drop(nArgs+convertedZ);
}

#undef N_KEYWORDS
#define N_KEYWORDS 8
static char *plfpKeys[N_KEYWORDS+1]= {
  "legend", "hide", "top", "cmin", "cmax", "edges", "ecolor", "ewidth", 0 };

void Y_plfp(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, plfpKeys, keySymbols);
  long n= 0, ny= 0, *pn= 0;
  double *z= 0, *x= 0, *y= 0;
  GpColor *zc= 0;
  int convertedZ= 0;
  int rgb = 0;

  if (!CalledAsSubroutine())
    YError("plfp may not be invoked as a function -- subroutine only");

  while (stack<=sp) {
    Operand op;
    if (!stack->ops) {
      stack+= 2;
      continue;
    }
    if (stack->ops==&referenceSym) ReplaceRef(stack);
    if (!convertedZ) {
      stack->ops->FormOperand(stack, &op);
      if (op.ops!=&charOps) {
        if (op.ops!=&voidOps)
          z= Get1Ddouble(stack, &n);
      } else {
        Dimension *next = op.type.dims? op.type.dims->next : 0;
        if (!op.type.dims || next) {
          if (next->next || next->number!=3)
            YError("expecting 1D or 3xN color array as argument to plfp");
          rgb = 1;
        }
        n= op.type.number;
        if (rgb) n /= 3;
        zc= op.value;
      }
      convertedZ= 1;
    } else if (!y) {
      y= Get1Ddouble(stack, &ny);
      if (!y) YError("expecting non-nil argument in plfp");
    } else if (!x) {
      long nx= 0;
      x= Get1Ddouble(stack, &nx);
      if (!x) YError("expecting non-nil argument in plfp");
      if (nx!=ny) YError("numberof(x)!=numberof(y) in plfp");
    } else if (!pn) {
      long i, np;
      stack->ops->FormOperand(stack, &op);
      if (op.ops==&voidOps) YError("expecting non-nil argument in plfp");
      if (op.ops->promoteID>T_LONG || (op.type.dims && op.type.dims->next))
        YError("expecting 1D array convertable to type long as argument");
      op.ops->ToLong(&op);
      if ((z||zc) && op.type.number!=n)
        YError("numberof(pn)!=numberof(z) in plfp");
      else
        n= op.type.number;
      pn= (long *)op.value;
      for (np=i=0 ; i<n ; i++) np+= pn[i];
      if (np!=ny) YError("numberof(y)!=sum(pn) in plfp");
    } else {
      YError("plfp takes at most four non-keyword arguments");
    }
    stack++;
  }
  if (!pn) YError("plfp needs four non-keyword arguments");

  /* set legend and hide in gistD */
  CheckDefaultWindow();
  CheckDefaultPalette();
  /* would need to add plfp to quine list with YpQuine to get legend
     LegendAndHide("plfp, ", z_name, y_name, x_name, r_name, keySymbols); */
  LegendAndHide((char *)0, (char *)0, (char *)0,
                (char *)0, (char *)0, keySymbols);

  if (!zc && z) {
    /* need to generate colors array on stack now */
    double zmin, zmax, scale, offset;

    GrabByteScale(&keySymbols[2], &scale, &offset, &zmin, &zmax,
                  z, (int *)0, 0, n+1, 2L, 1);
    zc= PushColors(z, n, zmin, zmax, scale, offset);
    convertedZ= 1;
  } else {
    convertedZ= 0;
  }

  GhGetFill();
  if (YNotNil(keySymbols[5]))
    gistA.e.type= YGetInteger(keySymbols[5])? L_SOLID : L_NONE;
  if (YNotNil(keySymbols[6]))
    gistA.e.color= YgetColor(keySymbols[6]);
  if (YNotNil(keySymbols[7]))
    gistA.e.width= YGetReal(keySymbols[7]);
  gistA.rgb = rgb;

  curElement= -1;
  curElement= GdFill(n, zc, x, y, pn);
  if (curElement<0) YWarning("Gist GdFill plotter failed");

  Drop(nArgs+convertedZ);
}

#undef N_KEYWORDS
#define N_KEYWORDS 9
static char *pltKeys[N_KEYWORDS+1]= {
  "legend", "hide",
  "color", "font", "height", "orient", "justify", "opaque", "tosys", 0 };

void Y_plt(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, pltKeys, keySymbols);
  char *text= 0;
  double x= 0.0, y= 0.0;
  int toSys;
  int iPass= 0;

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }
    if (iPass==0) text= YGetString(stack);
    else if (iPass==1) x= YGetReal(stack);
    else if (iPass==2) y= YGetReal(stack);
    iPass++;
    stack++;
  }
  if (iPass!=3)
    YError("plt requires exactly three non-keyword arguments");

  /* set legend and hide in gistD */
  CheckDefaultWindow();
  LegendAndHide((char *)0, (char *)0, (char *)0,
                (char *)0, (char *)0, keySymbols);

  /* set properties, starting from defaults for vectors */
  GhGetText();

  if (YNotNil(keySymbols[2]))
    gistA.t.color= YgetColor(keySymbols[2]);
  if (YNotNil(keySymbols[3]))
    gistA.t.font= GetFont(keySymbols[3]);
  if (YNotNil(keySymbols[4]))
    gistA.t.height= YGetReal(keySymbols[4])*ONE_POINT;
  if (YNotNil(keySymbols[5]))
    gistA.t.orient= YGetInteger(keySymbols[5]);
  if (YNotNil(keySymbols[6]))
    GetJustify(keySymbols[6]);
  if (YNotNil(keySymbols[7]))
    gistA.t.opaque= (YGetInteger(keySymbols[7])!=0);

  if (!gistA.t.orient) {
    gistA.t.orient= TX_RIGHT;
  } else {
    if (gistA.t.orient==1) gistA.t.orient= TX_UP;
    else if (gistA.t.orient==2) gistA.t.orient= TX_LEFT;
    else if (gistA.t.orient==3) gistA.t.orient= TX_DOWN;
    else {
      gistA.t.orient= TX_RIGHT;
      YError("orient= keyword must be 0, 1, 2, or 3");
    }
  }

  toSys= 0;
  if (YNotNil(keySymbols[8]))
    toSys= (YGetInteger(keySymbols[8])!=0);

  if (!text) text= "";
  curElement= -1;
  curElement= GdText(x, y, text, toSys);
  if (curElement<0) YWarning("Gist GdText plotter failed");

  Drop(nArgs);
}

#undef N_KEYWORDS
#define N_KEYWORDS 5
static char *pldjKeys[N_KEYWORDS+1]= {
  "legend", "hide", "color", "type", "width", 0 };

void Y_pldj(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, pldjKeys, keySymbols);
  double *x0= 0, *y0= 0, *x1= 0, *y1= 0;
  char *x0_name= 0, *y0_name= 0, *x1_name= 0, *y1_name= 0;
  long n= 0;
  int iPass= 0;

  if (!CalledAsSubroutine())
    YError("pldj may not be invoked as a function -- subroutine only");

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }
    if (iPass==0) x0= GetDouble(stack, &n);
    else if (iPass==1) x0_name= YGetString(stack);
    else if (iPass==2) y0= GetDouble(stack, &n);
    else if (iPass==3) y0_name= YGetString(stack);
    else if (iPass==4) x1= GetDouble(stack, &n);
    else if (iPass==5) x1_name= YGetString(stack);
    else if (iPass==6) y1= GetDouble(stack, &n);
    else if (iPass==7) y1_name= YGetString(stack);
    iPass++;
    stack++;
  }
  if (iPass!=8)
    YError("pldj requires exactly four non-keyword arguments");

  /* set legend and hide in gistD */
  CheckDefaultWindow();
  LegendAndHide("pldj, ", x0_name, y0_name, x1_name, y1_name, keySymbols);

  /* set properties, starting from defaults for simple polylines */
  GhGetMesh();

  if (YNotNil(keySymbols[2]))
    gistA.l.color= YgetColor(keySymbols[2]);
  if (YNotNil(keySymbols[3]))
    gistA.l.type= GetLineType(keySymbols[3]);
  if (YNotNil(keySymbols[4]))
    gistA.l.width= YGetReal(keySymbols[4]);

  curElement= -1;
  curElement= GdDisjoint(n, x0, y0, x1, y1);
  if (curElement<0) YWarning("Gist GdDisjoint plotter failed");

  Drop(nArgs);
}

static double *GetDouble(Symbol *stack, long *n)
{
  Operand op;
  if (!stack || !stack->ops)
    YError("unexpected keyword or missing argument (BUG?)");
  stack->ops->FormOperand(stack, &op);
  if (op.ops->promoteID>T_DOUBLE)
    YError("expecting argument convertable to type double");
  op.ops->ToDouble(&op);
  *n= op.type.number;
  return (double *)op.value;
}

/*--------------------------------------------------------------------------*/

#undef N_KEYWORDS
#define N_KEYWORDS 3
static char *limKeys[N_KEYWORDS+1]= {
  "square", "nice", "restrict", 0 };

void Y_limits(int nArgs)
{
  /* NB-- If the plot has not been displayed yet, this will not retrieve
          the latest extreme values calculated by GdScan.  Nevertheless,
          it DOES retrieve the precise state of the limits at the time
          of this call, and retoring them will work correctly.  */
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, limKeys, keySymbols);
  Operand op;
  double old_limits[5], *new_limits= 0;
  double xmin= 0.0, xmax= 0.0, ymin= 0.0, ymax= 0.0;
  char *extreme;
  int flags= 0, changed= 0;
  int iPass= 0;

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }
    if (new_limits)
      YError("only one argument allowed when restoring saved limits");

    if (YNotNil(stack)) {
      stack->ops->FormOperand(stack, &op);
      extreme= (op.ops==&stringOps && !op.type.dims)?
        ((char **)op.value)[0] : 0;
      if (!extreme || (extreme[0]!='e' && extreme[0]!='E') ||
          extreme[1]!='\0') {
        if (op.ops->promoteID>T_DOUBLE ||
            (op.type.dims && (iPass!=0 || op.type.dims->number!=5)))
          YError("illegal argument type in limits function");
        op.ops->ToDouble(&op);
      }
      if (iPass==0) {
        if (op.type.dims) new_limits= (double *)op.value;
        else if (extreme) flags|= D_XMIN;
        else xmin= *((double *)op.value);
        changed|= 1;
      } else if (iPass==1) {
        if (extreme) flags|= D_XMAX;
        else xmax= *((double *)op.value);
        changed|= 2;
      } else if (iPass==2) {
        if (extreme) flags|= D_YMIN;
        else ymin= *((double *)op.value);
        changed|= 4;
      } else if (iPass==3) {
        if (extreme) flags|= D_YMAX;
        else ymax= *((double *)op.value);
        changed|= 8;
      } else {
        YError("limits takes at most 4 non-keyword arguments");
      }
    }

    iPass++;
    stack++;
  }

  /* retrieve current limits and flags */
  GdGetLimits();
  old_limits[0]= gistD.limits.xmin;
  old_limits[1]= gistD.limits.xmax;
  old_limits[2]= gistD.limits.ymin;
  old_limits[3]= gistD.limits.ymax;
  old_limits[4]= (double)gistD.flags;

  /* process square=, nice=, restrict= keywords */
  if (YNotNil(keySymbols[0])) {
    if (new_limits) flags= 1;
    else if (YGetInteger(keySymbols[0])) gistD.flags|= D_SQUARE;
    else gistD.flags&= ~D_SQUARE;
    changed|= 16;
  }
  if (YNotNil(keySymbols[1])) {
    if (new_limits) flags= 1;
    else if (YGetInteger(keySymbols[1])) gistD.flags|= D_NICE;
    else gistD.flags&= ~D_NICE;
    changed|= 16;
  }
  if (YNotNil(keySymbols[2])) {
    if (new_limits) flags= 1;
    else if (YGetInteger(keySymbols[2])) gistD.flags|= D_RESTRICT;
    else gistD.flags&= ~D_RESTRICT;
    changed|= 16;
  }

  if (new_limits) {
    /* restore limits saved with previous limits command */
    if (flags) YError("no keywords allowed when restoring saved limits");
    gistD.limits.xmin= new_limits[0];
    gistD.limits.xmax= new_limits[1];
    gistD.limits.ymin= new_limits[2];
    gistD.limits.ymax= new_limits[3];
    gistD.flags= (int)new_limits[4];

  } else if (nArgs) {
    /* process xmin, xmax, ymin, ymax */
    if (changed&1) {
      gistD.limits.xmin= xmin;
      if (flags&D_XMIN) gistD.flags|= D_XMIN;
      else gistD.flags&= ~D_XMIN;
    }
    if (changed&2) {
      gistD.limits.xmax= xmax;
      if (flags&D_XMAX) gistD.flags|= D_XMAX;
      else gistD.flags&= ~D_XMAX;
    }
    if (changed&4) {
      gistD.limits.ymin= ymin;
      if (flags&D_YMIN) gistD.flags|= D_YMIN;
      else gistD.flags&= ~D_YMIN;
    }
    if (changed&8) {
      gistD.limits.ymax= ymax;
      if (flags&D_YMAX) gistD.flags|= D_YMAX;
      else gistD.flags&= ~D_YMAX;
    }

  } else {
    /* just reset to extreme values */
    changed= (D_XMIN | D_XMAX | D_YMIN | D_YMAX);
    gistD.flags|= changed;
  }

  /* set new limits in drawing */
  if (changed) GdSetLimits();

  Drop(nArgs);
  if (!CalledAsSubroutine()) {
    Array *array=
      PushDataBlock(NewArray(&doubleStruct,
                             NewDimension(5L, 1L, (Dimension *)0)));
    double *lims= array->value.d;
    int i;
    array->type.dims->references--;
    for (i=0 ; i<5 ; i++) lims[i]= old_limits[i];
  }
}

void Y_logxy(int nArgs)
{
  int xflag= 0, yflag= 0, changed;
  int iPass= 0;
  Symbol *stack= sp-nArgs+1;

  changed= 0;
  while (stack<=sp) {
    if (!stack->ops) YError("logxy takes no keyword arguments");
    if (YNotNil(stack)) {
      if (iPass==0) {
        xflag= (YGetInteger(stack)!=0);
        changed|= 1;
      } else if (iPass==1) {
        yflag= (YGetInteger(stack)!=0);
        changed|= 2;
      } else {
        YError("logxy takes at most two arguments");
      }
    }
    iPass++;
    stack++;
  }

  if (changed) {
    GdGetLimits();
    if (changed&1) {
      if (xflag) gistD.flags|= D_LOGX;
      else gistD.flags&= ~D_LOGX;
    }
    if (changed&2) {
      if (yflag) gistD.flags|= D_LOGY;
      else gistD.flags&= ~D_LOGY;
    }
    GdSetLimits();
  }

  Drop(nArgs);
}

void Y_zoom_factor(int nArgs)
{
  if (nArgs!=1) YError("zoom_factor takes exactly one argument");
  DISPLAY_ZOOM_FACTOR= YGetReal(sp);
  /* avert various disasters --
     doesn't address DISPLAY_ZOOM_FACTOR==1.0, which would be frustrating... */
  if (DISPLAY_ZOOM_FACTOR<0.0) DISPLAY_ZOOM_FACTOR= -DISPLAY_ZOOM_FACTOR;
  if (DISPLAY_ZOOM_FACTOR<0.05) DISPLAY_ZOOM_FACTOR= 0.05;
  else if (DISPLAY_ZOOM_FACTOR>20.0) DISPLAY_ZOOM_FACTOR= 20.0;
}

void Y_unzoom(int nArgs)
{
  if (nArgs!=0) YError("unzoom takes exactly zero argument");
  GdRevertLimits(1);
}

/*--------------------------------------------------------------------------*/

static char *window_name(int n);
static char *
window_name(int n)
{
  static char buffer[20];
  sprintf(buffer, "Yorick %d", n);
  return buffer;
}

static void SetHCPDefault(void)
{
  int i, j;
  p_file *f;
  char hcpName[16];
  if (!hcpPSdefault) strcpy(hcpName, "Aa00.cgm");
  else strcpy(hcpName, "Aa00.ps");

  for (j='A' ; j<='Z' ; j++) {
    hcpName[0]= j;
    for (i='a' ; i<='z' ; i++) {
      hcpName[1]= i;
      if ((f= p_fopen(hcpName, "rb"))) p_fclose(f);
      else goto got1;
    }
  }
  YError("you appear to have Aa00 through Zz00 hcp files -- clean up");

 got1:
  if (!hcpPSdefault)
    hcpDefault= GpCGMEngine("Yorick default", 0, hcpDump,
                            SetHCPname(-1, hcpName));
  else
    hcpDefault= GpPSEngine("Yorick default", 0, hcpDump,
                           SetHCPname(-1, hcpName));
  if (!hcpDefault) YError("failed to create default hcp file");
}

static void CheckDefaultWindow(void)
{
  int i;
  for (i=0 ; i<GH_NDEVS ; i++) if (ghDevices[i].drawing) {
    if (!ghDevices[i].display && !ghDevices[i].hcp) {
      Drauing *drawing= ghDevices[i].drawing;
      ghDevices[i].drawing= 0;
      GdKillDrawing(drawing);
      curElement= -1;
    }
  }
  if (GhGetPlotter()<0) {
    for (i=0 ; i<GH_NDEVS ; i++) if (ghDevices[i].drawing)
      YError("graphics window killed -- use window command to re-select");
    ghDevices[0].drawing=
      GdNewDrawing(defaultStyle? defaultStyle : "work.gs");
    curElement= -1;
    if (!ghDevices[0].drawing)
      YError("failed to create drawing -- Gist work.gs style sheet missing");
    ghDevices[0].doLegends= defaultLegends;

#ifndef NO_XLIB
    gist_private_map = gist_rgb_hint = 0;
    gx_parent = 0;
    ghDevices[0].display=
      DISPLAY_ENGINE(window_name(0), 0, defaultDPI, (char *)0);
    if (!ghDevices[0].display)
      YError("failed to open X display or create X window");
#else
    ghDevices[0].display= 0;
    ghDevices[0].hcp= hcpDefault;
    hcpDefault= 0;
#endif

    GhSetPlotter(0);
  }
}

static void CheckDefaultPalette(void)
{
  GpColorCell *palette;
  GhGetPalette(-1, &palette);
  if (!palette) GhReadPalette(-1, defaultPalette? defaultPalette : "viridis.gp",
                              &palette, maxColors);
}

static void CheckPalette(void)
{
  int n= GhGetPlotter();
  if (n>=0 && !ghDevices[n].hcp) {
    if (!hcpDefault) SetHCPDefault();
    SetHCPPalette();
  }
}

static char *SetHCPname(int n, char *name)
{
  char *now;
  if (n<0 || n>GH_NDEVS) n= GH_NDEVS;
  now= hcpNames[n];
  hcpNames[n]= YExpandName(name);
  p_free(now);
  return hcpNames[n];
}

static char *GetHCPname(int n)
{
  if (n>=0 && n<GH_NDEVS && ghDevices[n].hcp) return hcpNames[n];
  else return hcpNames[GH_NDEVS];
}

#undef N_KEYWORDS
#define N_KEYWORDS 14
static char *windowKeys[N_KEYWORDS+1]= {
  "display", "dpi", "private", "hcp", "legends", "dump", "style", "wait",
  "width", "height", "rgb", "parent", "xpos", "ypos", 0 };

static Instruction *yg_pc_resume = 0;
extern void yg_got_expose(void);
extern Instruction *ym_suspend(void);
extern void ym_resume(Instruction *);
extern int yg_blocking;
int yg_blocking = 0;

void Y_window(int nArgs)
{
  int n, nGiven;
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, windowKeys, keySymbols);
  GpColorCell *palette;
  int nColors= 0;
  int wait_for_expose = 0;
  int rgb = 0;
  int n0 = GhGetPlotter();

  if (stack<=sp && YNotNil(stack++)) {
    n= (int)YGetInteger(stack-1);
    if (n<0 || n>=GH_NDEVS) YError("bad graphics windows are number");
    nGiven= (!ghDevices[n].display && !ghDevices[n].hcp);
  } else {
    n = n0;
    nGiven= (n<0);
    if (nGiven) n= 0;
  }
  while (stack<=sp) {
    if (!stack->ops) stack+= 2;
    else YError("window function takes at most one non-keyword argument");
  }

  curElement= -1;

  /* get current palette for this graphics window */
  nColors= GhGetPalette(n, &palette);

  /* check for width and height specs, subwindow hack */
#ifndef NO_XLIB
  gx_parent = 0;
  if (YNotNil(keySymbols[8])) {
    extern int gx75width, gx100width;
    int width= (int)YGetInteger(keySymbols[8]);
    if (width>30) gx75width= gx100width= width;
    else { gx75width= 450; gx100width= 600; }
  }
  if (YNotNil(keySymbols[9])) {
    extern int gx75height, gx100height;
    int height= (int)YGetInteger(keySymbols[9]);
    if (height>30) gx75height= gx100height= height;
    else { gx75height= 450; gx100height= 600; }
  }
  if (YNotNil(keySymbols[11])) {
    extern int gx_xloc, gx_yloc;
    gx_parent = (unsigned long)YGetInteger(keySymbols[11]);
    gx_xloc = gx_yloc = 0;
    if (YNotNil(keySymbols[12]))
      gx_xloc = (int)YGetInteger(keySymbols[12]);
    if (YNotNil(keySymbols[13]))
      gx_yloc = (int)YGetInteger(keySymbols[13]);
  }
#endif

  if (nGiven || keySymbols[0] || keySymbols[1] || keySymbols[2]) {
    /* display= and/or dpi= keywords */
    char *display= 0;
    int dpi= defaultDPI;
    int privmap = 0;
    Engine *engine= ghDevices[n].display;  /* current display engine */

    if (YNotNil(keySymbols[0])) display= YGetString(keySymbols[0]);
    if (YNotNil(keySymbols[1])) {
      if (engine) YError("cannot change dpi of an existing graphics window");
      dpi= (int)YGetInteger(keySymbols[1]);
      /*if (dpi!=100 && dpi!=75)
        YError("dpi=100 or dpi=75 are only legal values");*/
      if (dpi<25) dpi = 25;
      else if (dpi>2400) dpi = 2400;
    }
    if (YNotNil(keySymbols[2])) {
      /* private= keyword -- turn on/off private X window colormap */
      if (engine)
        YError("cannot give existing graphics window private colormap");
      if (!(nGiven? (!display || display[0]) : (display && display[0])))
        YError("private= keyword not legal without display engine");
      privmap = YGetInteger(keySymbols[2])!=0;
    }
    if (YNotNil(keySymbols[10])) {
      /* rgb= keyword -- maybe make this a true color window */
      if (engine)
        YError("cannot use rgb= on existing graphics window");
      if (!(nGiven? (!display || display[0]) : (display && display[0])))
        YError("rgb= keyword not legal without display engine");
      rgb = YGetInteger(keySymbols[10])!=0;
    }

    if (engine) GpKillEngine(engine);

    if (nGiven? (!display || display[0]) : (display && display[0])) {
#ifndef NO_XLIB
      gist_private_map = privmap;
      gist_rgb_hint = rgb;
      engine= DISPLAY_ENGINE(window_name(n), 0, dpi, display);
      if (!engine) YError("failed to open X display or create X window");
      else wait_for_expose = 1;
      ghDevices[n].display= engine;
      if (palette) GhSetPalette(n, palette, nColors);
#else
      YError("No interactive graphics in this Yorick -- hcp only");
#endif
    }
  }

  if (keySymbols[3]) {
    /* hcp= keyword -- make a new hcp file */
    Engine *engine= ghDevices[n].hcp;
    char *hcp= 0;
    if (YNotNil(keySymbols[3])) hcp= YGetString(keySymbols[3]);

    if (engine) {
      ghDevices[n].hcp= 0;
      GpKillEngine(engine);
      SetHCPname(n, (char *)0);
    }

    if (hcp && hcp[0]) {
      long len= strlen(hcp);
      if (len>3 && strcmp(&hcp[len-3], ".ps")==0) {
        engine= GpPSEngine(window_name(n), 0, hcpDump, SetHCPname(n, hcp));
        if (!engine) YError("failed to create PostScript file");
      } else {
        engine= GpCGMEngine(window_name(n), 0, hcpDump, SetHCPname(n, hcp));
        if (!engine) YError("failed to create binary CGM file");
      }
      ghDevices[n].hcp= engine;
      if (palette) GhSetPalette(n, palette, nColors);
    }
  }

  if (keySymbols[4] || keySymbols[3] ||
      nGiven || keySymbols[0] || keySymbols[1]) {
    if (YNotNil(keySymbols[4]))
      /* legends= keyword -- turn on/off legend dumping to hcp file */
      ghDevices[n].doLegends= (YGetInteger(keySymbols[4])!=0);
    else
      ghDevices[n].doLegends= defaultLegends;
  }

  if (YNotNil(keySymbols[5])) {
    /* dump= keyword -- turn on/off colormap dumping to hcp file */
    if (!ghDevices[n].hcp)
      YError("dump= keyword not legal without hcp engine -- use hcp_file");
    GhDumpColors(n, 1, (YGetInteger(keySymbols[5])!=0));
  }

  if (!ghDevices[n].display && !ghDevices[n].hcp) {
    /* shut down this graphics window completely */
    Drauing *drawing= ghDevices[n].drawing;
    ghDevices[n].drawing= 0;
    if (drawing) GdKillDrawing(drawing);
    GhDeletePalette(n);
    if (n==n0) {
      /* highest numbered remaining window becomes current window */
      for (n=GH_NDEVS-1 ; n>=0 ; n--)
        if (ghDevices[n].display || ghDevices[n].hcp) break;
      GhSetPlotter(n);
    }

  } else {
    Drauing *drawing= ghDevices[n].drawing;
    if (keySymbols[6]) {
      /* style= keyword -- make new drawing */
      char *style= YNotNil(keySymbols[6]) ? YGetString(keySymbols[6]) : 0;
      if (drawing) {
        ghDevices[n].drawing= 0;
        GdKillDrawing(drawing);
      }
      if (!style || !style[0]) style= defaultStyle;
      ghDevices[n].drawing= drawing= GdNewDrawing(style? style : "work.gs");

    } else if (!drawing) {
      /* supply default drawing */
      ghDevices[n].drawing= drawing=
        GdNewDrawing(defaultStyle? defaultStyle : "work.gs");
    }

    if (!drawing) {
      ghDevices[n].drawing= drawing= GdNewDrawing("work.gs");
      if (drawing)
        YError("failed to create drawing -- bad style sheet name?");
      else
        YError("failed to create drawing -- Gist work.gs style sheet missing");
    }

    /* make this window current */
    GhSetPlotter(n);

    /* wait= keyword -- pause until X window is exposed */
    wait_for_expose = wait_for_expose &&
      YNotNil(keySymbols[7]) && YGetInteger(keySymbols[7]);
  }

  Drop(nArgs);
  PushLongValue((long)n);

#ifndef NO_XLIB
  if (wait_for_expose) {
    Instruction *ipc = yg_pc_resume;
    int oops = 0;
    if (!ipc) {
      oops = gist_expose_wait(ghDevices[n].display, yg_got_expose);
      if (!oops) {
        yg_blocking = 1;
        yg_pc_resume = ym_suspend();
      } else if (oops == 2) {
        /* window was already exposed */
        oops = 0;
      }
    }
    if (ipc || oops) {
      yg_got_expose();
      YError("window,wait=1 while already waiting for a window");
    }
  }
#endif
}

static void yg_alarm(void *);

void
yg_got_expose(void)
{
  Instruction *ipc = yg_pc_resume;
  yg_pc_resume = 0;
  if (yg_blocking==2) p_clr_alarm(yg_alarm, 0);
  yg_blocking = 0;
  if (ipc) ym_resume(ipc);
}

void Y_window_geometry(int argc)
{
  int win;
  double *geom;
  Engine *engine;
  GpXYMap *map;
  long dims[2];

  double one_pixel, dpi, xbias, ybias, width, height;

  if (argc != 1) {
    YError("window_geometry takes exactly one, possibly nil, argument");
  }
  if (YNotNil(sp)) {
    win = (int)YGetInteger(sp);
  } else {
    win = GhGetPlotter();
  }
  if (win < 0 || win >= GH_NDEVS || ! ghDevices[win].display) {
    PushDataBlock(RefNC(&nilDB));
    return;
  }

  /* NDC -> pixel coordinate transform:
   *   XPIX = (int)(XSCALE*XNDC + XOFFSET)
   *   YPIX = (int)(YSCALE*YNDC + YOFFSET)
   * with:
   *   XSCALE = ENGINE->map.x.scale    XOFFSET = ENGINE->map.x.offset - margin
   *   YSCALE = ENGINE->map.y.scale    YOFFSET = ENGINE->map.y.offset - margin
   * assuming:
   *   (XSCALE*XNDC + XOFFSET) >= 0
   *   (YSCALE*YNDC + YOFFSET) >= 0
   * the reverse transform is:
   *   XPIX <= XSCALE*XNDC + XOFFSET < XPIX + 1
   *   YPIX <= YSCALE*YNDC + YOFFSET < YPIX + 1
   * to avoid rounding errors we choose the middle of the interval:
   *   XNDC  =  (XPIX - XOFFSET + 0.5)/XSCALE  =  XBIAS + XPIX*ONE_PIXEL
   *   YNDC  =  (YPIX - YOFFSET + 0.5)/YSCALE  =  YBIAS - YPIX*ONE_PIXEL
   * with:
   *   ONE_PIXEL = 1.0/XSCALE = -1.0/YSCALE
   *       XBIAS = (0.5 - XOFFSET)*ONE_PIXEL
   *       XBIAS = (YOFFSET - 0.5)*ONE_PIXEL
   */

  engine = ghDevices[win].display;
  if (engine) {
    map= &engine->map;
    dpi = ((XEngine *)engine)->dpi;
    one_pixel = 2.0/(map->x.scale - map->y.scale);
#define MARGIN(SIDE) (((XEngine *)engine)->SIDE##Margin)
    xbias = (MARGIN(left) - map->x.offset + 0.5)/map->x.scale;
    ybias = (MARGIN(top)  - map->y.offset + 0.5)/map->y.scale;
#undef MARGIN
    width = ((XEngine *)engine)->wtop;
    height = ((XEngine *)engine)->htop;
  } else {
    dpi = one_pixel = xbias = ybias = width = height = 0.0;
  }

  /* Build result array: [DPI, ONE_PIXEL, XBIAS, YBIAS, WIDTH, HEIGHT] */
  dims[0] = 1L;
  dims[1] = 6L;
  geom = ypush_d(dims);
  geom[0] = dpi;
  geom[1] = one_pixel;
  geom[2] = xbias;
  geom[3] = ybias;
  geom[4] = width;
  geom[5] = height;
}

void Y_window_exists(int argc)
{
  long n;
  if (argc != 1) YError("window_exists takes exactly one argument");
  n = YGetInteger(sp);
  PushIntValue(((n >= 0 && n < GH_NDEVS && ghDevices[n].display) ? 1 : 0));
}

void Y_window_select(int argc)
{
  int n;
  if (argc != 1) YError("window_select takes exactly one argument");
  n = (int)YGetInteger(sp);
  if (n >= 0 && n < GH_NDEVS && ghDevices[n].display) {
    GhSetPlotter(n);
    PushIntValue(1);
  } else {
    PushIntValue(0);
  }
}

void Y_window_list(int argc)
{
  long *p, i, n, dims[2];

  if (argc != 1 || YNotNil(sp)) {
    YError("window_list takes exactly one nil argument");
  }
  for (n=i=0 ; i<GH_NDEVS ; ++i) {
    if (ghDevices[i].display) {
      ++n;
    }
  }
  if (n >= 1) {
    dims[0] = 1;
    dims[1] = n;
    p = ypush_l(dims);
    for (n=i=0 ; i<GH_NDEVS ; ++i) {
      if (ghDevices[i].display) {
	p[n++] = i;
      }
    }
  } else {
    ypush_nil();
  }
}

#undef N_KEYWORDS
#define N_KEYWORDS 2
static char *hcpKeys[N_KEYWORDS+1]= { "dump", "ps", 0 };

void Y_hcp_file(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, hcpKeys, keySymbols);
  Engine *engine= hcpDefault;
  int gotDump= YNotNil(keySymbols[0]);

  if (gotDump) hcpDump= (YGetInteger(keySymbols[0])!=0);

  if (YNotNil(keySymbols[1])) hcpPSdefault= (YGetInteger(keySymbols[1])!=0);

  if (stack<=sp && YNotNil(stack)) {
    char *hcp= YGetString(stack);
    long len= Safe_strlen(hcp);

    if (engine) {
      hcpDefault= 0;
      GpKillEngine(engine);
      SetHCPname(-1, (char *)0);
      engine= 0;
    }

    if (len>3 && strcmp(&hcp[len-3], ".ps")==0) {
      engine= GpPSEngine("Yorick default", 0, hcpDump, SetHCPname(-1, hcp));
      if (!engine) YError("failed to create PostScript file");
    } else if (len>0) {
      engine= GpCGMEngine("Yorick default", 0, hcpDump, SetHCPname(-1, hcp));
      if (!engine) YError("failed to create binary CGM file");
    }

    hcpDefault= engine;
    stack++;
  } else if (gotDump) {
    GhDumpColors(-1, 1, hcpDump);
  }
  while (stack<=sp) {
    if (!stack->ops) stack+= 2;
    else YError("hcp_file function takes at most one non-keyword argument");
  }

  Drop(nArgs);
}

void Y_hcp_finish(int nArgs)
{
  /* just return name of current hcp file */
  int n= GhGetPlotter();
  Array *array;
  Engine *engine;

  if (nArgs==1) {
    if (YNotNil(sp)) n= YGetInteger(sp);
    if (n<-1 || n>=GH_NDEVS)
      YError("hcp_finish argument must be -1 or a graphics window number");
  } else if (nArgs) {
    YError("hcp_finish takes zero or one arguments");
  }

  array= PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
  array->value.q[0]= p_strcpy(GetHCPname(n));

  if (n>=0) engine= ghDevices[n].hcp? ghDevices[n].hcp : hcpDefault;
  else engine= hcpDefault;
  if (engine) {
    if (engine==hcpDefault) {
      hcpDefault= 0;
    } else {
      ghDevices[n].hcp= 0;
    }
    GpKillEngine(engine);
    SetHCPname(n, (char *)0);
  }
}

void Y_plsys(int nArgs)
{
  int n0;
  if (nArgs!=1) YError("plsys takes exactly one argument");

  CheckDefaultWindow();
  n0= GdGetSystem();

  if (YNotNil(sp)) {
    int n= (int)YGetInteger(sp);
    if (GdSetSystem(n)!=E_SYSTEM && n!=0)
      YError("no such coordinate system exists in current graphics window");
  }

  PushLongValue((long)n0);
}

#undef N_KEYWORDS
#define N_KEYWORDS 2
static char *paletteKeys[N_KEYWORDS+1]= { "ntsc", "query", 0 };

void Y_palette(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, paletteKeys, keySymbols);
  Operand op;
  GpColorCell *palette= 0;
  unsigned char *red= 0, *green= 0, *blue= 0, *gray= 0;
  int i, iPass, nColors, nDevice, query= 0;
  Engine *engine;
  int sourceDevice= -2;
  char **name= 0;

  CheckDefaultWindow();
  nDevice= GhGetPlotter();
  engine= ghDevices[nDevice].display;
  if (!engine) engine= ghDevices[nDevice].hcp;

  if (YNotNil(keySymbols[1])) {
    Dimension *dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    query= (YGetInteger(keySymbols[1])!=0);
  }

  iPass= nColors= 0;
  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }
    if (iPass>3)
      YError("palette takes at most red, green, blue, gray arguments");

    if (query) {
      if (stack->ops!=&referenceSym)
        YError("palette query needs simple variable references as arguments");
      if (iPass==0) nColors= GpGetPalette(engine, &palette);
      if (nColors>0) {
        Array *array;
        tmpDims= NewDimension((long)nColors, 1L, (Dimension *)0);
        array= PushDataBlock(NewArray(&charStruct, tmpDims));
        red= (unsigned char *)array->value.c;
        if (iPass==0)
          for (i=0 ; i<nColors ; i++) red[i]=(unsigned char)(P_R(palette[i]));
        else if (iPass==1)
          for (i=0 ; i<nColors ; i++) red[i]=(unsigned char)(P_G(palette[i]));
        else if (iPass==2)
          for (i=0 ; i<nColors ; i++) red[i]=(unsigned char)(P_B(palette[i]));
        else if (iPass==3)
          for (i=0 ; i<nColors ; i++)
            red[i]=(unsigned char)((P_R(palette[i])+P_G(palette[i])+
                                    P_B(palette[i]))/3);
      } else {
        PushDataBlock(RefNC(&nilDB));
      }
      PopTo(&globTab[stack->index]);

    } else {
      stack->ops->FormOperand(stack, &op);
      if (iPass==0) {
        if (op.ops==&stringOps) {
          /* palette, filename */
          name= op.value;

        } else if (!op.type.dims) {
          /* palette, source_window */
          if (op.ops->promoteID>T_LONG)
            YError("palette source window number must be an integer");
          op.ops->ToLong(&op);
          sourceDevice= (int)(*((long *)op.value));
          if (sourceDevice<0 || sourceDevice>=GH_NDEVS ||
              (!(engine= ghDevices[sourceDevice].display) &&
               !(engine= ghDevices[sourceDevice].hcp)))
            YError("specified palette source window does not exist");
          nColors= GpGetPalette(engine, &palette);

        } else {
          /* palette, red, green, blue */
          op.ops->ToChar(&op);
          red= op.value;
          nColors= (int)op.type.number;
        }

        if (nColors>256)
          YError("Gist palettes can never have more than 256 colors");

      } else {
        /* palette, red, green, blue */
        if (!red) YError("garbled arguments to palette command");
        op.ops->ToChar(&op);
        if (op.type.number != nColors)
          YError("red, green, blue, and gray arguments must be same length");
        if (iPass==1) green= op.value;
        if (iPass==2) blue= op.value;
        if (iPass==3) gray= op.value;
      }
    }

    iPass++;
    stack++;
  }

  if (!query) {
    if (sourceDevice!=nDevice) {
      /* be sure to preserve dump=1 setting even if hcp palette
         is deleted */
      int dump;
      if (hcpDefault) dump= GhGetColorMode(hcpDefault);
      else dump= 0;
      GhDeletePalette(nDevice);
      if (hcpDefault) GhDumpColors(-1, 1, dump);
    }
    if (red || palette) {
      if (red) {
        if (iPass<3)
          YError("palette needs at least red, green, and blue components");
        /* palette is unprotected against asynchronous interrupts...
           fix this someday */
        palette = p_malloc(sizeof(GpColorCell)*nColors);
        /* palette, red, green, blue malloc'ed like GhReadPalette */
        for (i=0 ; i<nColors ; i++) {
          palette[i] = P_RGB(red[i], green[i], blue[i]);
          /* if (gray) palette[i].gray = gray[i]; */
        }
      }
      if (!gray) {
        if (YNotNil(keySymbols[0]) && YGetInteger(keySymbols[0])!=0)
          GpPutNTSC(nColors, palette);
        else
          GpPutGray(nColors, palette);
      }
      GhSetPalette(nDevice, palette, nColors);

    } else if (name) {
      nColors= GhReadPalette(nDevice, name[0], &palette, maxColors);
      if (nColors<=0)
        YError("no such palette -- missing Gist palette file?");

    } else {
      YError("palette needs at least one non-keyword argument");
    }
  }
}

/*--------------------------------------------------------------------------*/

void Y_fma(int nArgs)
{
  if (nArgs) YError("fma takes exactly zero argument");
  CheckDefaultWindow();
  if (hcpOnFMA) CheckPalette();
  curElement= -1;
  GhFMA();
}

void Y_redraw(int nArgs)
{
  if (nArgs) YError("redraw takes exactly zero argument");
  CheckDefaultWindow();
  GhRedraw();
}

void Y_hcp(int nArgs)
{
  if (nArgs) YError("hcp takes exactly zero argument");
  CheckDefaultWindow();
  CheckPalette();
  GhHCP();
}

void Y_hcpon(int nArgs)
{
  if (nArgs) YError("hcpon takes exactly zero argument");
  CheckDefaultWindow();
  hcpOnFMA= 1;
  GhFMAMode(1, 2);
}

void Y_hcpoff(int nArgs)
{
  if (nArgs) YError("hcpoff takes exactly zero argument");
  CheckDefaultWindow();
  hcpOnFMA= 0;
  GhFMAMode(0, 2);
}

void Y_animate(int nArgs)
{
  int i= 3;  /* default is to toggle */

  if (nArgs==1 && YNotNil(sp)) i= (int)YGetInteger(sp);
  else if (nArgs>1) YError("animate takes zero or one argument");

  CheckDefaultWindow();

  curElement= -1;
  GhFMAMode(2, i);
}

/*--------------------------------------------------------------------------*/

static long prop3sizes[10]= {0, 8, 2, 5, 5, 4, 3, 7, 1, 3};
static long prop4sizes[10]= {0, 8, 1, 3, 1, 1, 3, 4, 4, 1};
static long prop5sizes[10]= {0, 3, 5, 2, 5, 6, 7, 9, 3, 5};

static int curIX= -1, curIXc= -1;
static char specialMarkers[5]= ".+*ox";

void Y_plq(int nArgs)
{
  int type, n_element= 0, n_contour= 0;

  if (nArgs==1) {
    if (YNotNil(sp)) n_element= (int)YGetInteger(sp);
  } else if (nArgs==2) {
    if (YNotNil(sp-1)) n_element= (int)YGetInteger(sp-1);
    if (YNotNil(sp)) n_contour= (int)YGetInteger(sp);
  } else if (nArgs>2) {
    YError("plq function takes no more than two arguments");
  }
  Drop(nArgs);

  /* Yorick uses 1-origin element numbering, Gist uses 0-origin */
  n_element--;
  n_contour--;

  if (n_element>=0) {
    /* retrieve specified element */
    type= GdSetElement(n_element);
    if (n_contour>=0) {
      if (type!=E_CONTOURS)
        YError("current graphical element is not contours in pledit");
      type= GdSetContour(n_contour);
    }
    curElement= -6666; /* differs from -1 to allow pledit after plq */
    curIX= n_element;  /* need these, too */
    curIXc= n_contour;
    if (type==E_LINES) type= 1;
    else if (type==E_DISJOINT) type= 2;
    else if (type==E_TEXT) type= 3;
    else if (type==E_MESH) type= 4;
    else if (type==E_FILLED) type= 5;
    else if (type==E_VECTORS) type= 6;
    else if (type==E_CONTOURS) type= 7;
    else if (type==E_CELLS) type= 8;
    else if (type==E_POLYS) type= 9;
    else type= 0;

    if (CalledAsSubroutine()) {
      /* return printed summary of keyword values */
      char line[120];
      PrintInit(YputsOut);

      if (type==0) {
        sprintf(line, "<no such object>  element# %d", n_element+1);
        PrintFunc(line);
        if (n_contour>=0) {
          sprintf(line, "  contour# %d", n_contour+1);
          PrintFunc(line);
        }
        ForceNewline();

      } else if (type==1) {
        sprintf(line, "plg  element# %d", n_element+1);
        PrintFunc(line);
        if (n_contour>=0) {
          sprintf(line, "  contour# %d", n_contour+1);
          PrintFunc(line);
          ForceNewline();
          sprintf(line, "  at level value %g", gistD.levels[n_contour]);
          PrintFunc(line);
        }
        ForceNewline();
        PrintHideLegend(line, type);
        PrintColor(line, gistA.l.color, 1);
        PrintTypeWidth(line, 3);
        PrintMarks(line, 3);
        sprintf(line, "rays= %d,", gistA.dl.rays);
        PrintFunc(line);
        ForceNewline();
        sprintf(line,
                "  arrowl= %.2f, arroww= %.2f, rspace= %.5f, rphase= %.5f,",
                Safe_dbl(gistA.dl.arrowL), Safe_dbl(gistA.dl.arrowW),
                Safe_dbl(gistA.dl.rSpace), Safe_dbl(gistA.dl.rPhase));
        PrintFunc(line);
        ForceNewline();
        sprintf(line, "smooth= %d,  closed= %d",
                gistA.dl.smooth, gistA.dl.closed);
        PrintFunc(line);
        ForceNewline();

      } else if (type==2) {
        sprintf(line, "pldj  element# %d", n_element+1);
        PrintFunc(line);
        ForceNewline();
        PrintHideLegend(line, type);
        PrintColor(line, gistA.l.color, 1);
        PrintTypeWidth(line, 2);

      } else if (type==3) {
        sprintf(line, "plt  element# %d", n_element+1);
        PrintFunc(line);
        ForceNewline();
        PrintHideLegend(line, type);
        PrintColor(line, gistA.t.color, 3);
        sprintf(line, "text= %.80s", gistD.text);
        PrintFunc(line);
        ForceNewline();

      } else if (type==4) {
        sprintf(line, "plm  element# %d", n_element+1);
        PrintFunc(line);
        ForceNewline();
        PrintHideLegend(line, type);
        PrintColor(line, gistA.l.color, 1);
        PrintTypeWidth(line, 2);
        PrintRegion(line, 1);
        sprintf(line, "boundary= %d, inhibit= %d", gistD.boundary,
                gistD.inhibit);
        PrintFunc(line);
        ForceNewline();

      } else if (type==5) {
        sprintf(line, "plf  element# %d", n_element+1);
        PrintFunc(line);
        ForceNewline();
        PrintHideLegend(line, type);
        sprintf(line, "edges= %d, e", gistA.e.type!=L_NONE);
        PrintFunc(line);
        PrintColor(line, gistA.e.color, 1);
        sprintf(line, "ewidth= %.2f", Safe_dbl(gistA.e.width));
        PrintFunc(line);
        ForceNewline();
        PrintRegion(line, 2);

      } else if (type==6) {
        sprintf(line, "plv  element# %d", n_element+1);
        PrintFunc(line);
        ForceNewline();
        PrintHideLegend(line, type);
        PrintColor(line, gistA.l.color, 1);
        sprintf(line, "width= %.2f,", Safe_dbl(gistA.l.width));
        PrintFunc(line);
        ForceNewline();
        sprintf(line, "hollow= %d,  aspect= %.4f,", gistA.vect.hollow,
                Safe_dbl(gistA.vect.aspect));
        PrintFunc(line);
        ForceNewline();
        PrintRegion(line, 3);
        sprintf(line, "scale= %g", gistD.scale);
        PrintFunc(line);
        ForceNewline();

      } else if (type==7) {
        int i;
        sprintf(line, "plc  element# %d", n_element+1);
        PrintFunc(line);
        ForceNewline();
        PrintHideLegend(line, type);
        PrintColor(line, gistA.l.color, 1);
        PrintTypeWidth(line, 3);
        PrintMarks(line, 3);
        sprintf(line, "smooth= %d,", gistA.dl.smooth);
        PrintFunc(line);
        ForceNewline();
        PrintRegion(line, 2);
        sprintf(line, "%d contour levels, levs=", gistD.nLevels);
        PrintFunc(line);
        ForceNewline();
        PrintFunc("[");
        if (gistD.nLevels>0) {
          for (i=0 ; ; i++) {
            sprintf(line, "%g", gistD.levels[i]);
            PrintFunc(line);
            if (i==gistD.nLevels-1) break;
            PrintFunc(",");
            PermitNewline(0);
          }
        }
        PrintFunc("]");
        ForceNewline();

      } else if (type==8) {
        sprintf(line, "pli  element# %d", n_element+1);
        PrintFunc(line);
        ForceNewline();
        PrintHideLegend(line, type);
        sprintf(line, "x0= %g,  y0= %g,  x1= %g,  y1= %g",
                gistD.px, gistD.py, gistD.qx, gistD.qy);
        PrintFunc(line);
        ForceNewline();

      } else if (type==9) {
        sprintf(line, "plfp  element# %d", n_element+1);
        PrintFunc(line);
        ForceNewline();
        PrintHideLegend(line, type);
        sprintf(line, "%d polygons", gistD.n);
        PrintFunc(line);
        ForceNewline();
      }

    } else {
      /* return properties array */
      Dimension *dims= NewDimension(6L, 1L, (Dimension *)0);
      Array *array= PushDataBlock(NewArray(&pointerStruct, dims));
      void **p= array->value.p;
      char **legend;
      int *ival;
      double *dval;
      long *lval;
      dims->references--;

      dims= NewDimension(2L, 1L, (Dimension *)0);
      p[0]= (NewArray(&intStruct, dims))->value.c;
      dims->references--;
      p[1]= (NewArray(&stringStruct, (Dimension *)0))->value.c;
      p[2]= MakePropArray(&intStruct, prop3sizes[type]);
      p[3]= MakePropArray(&doubleStruct, prop4sizes[type]);
      p[4]= MakePropArray(&longStruct, prop5sizes[type]);

      ival= (int *)p[0];
      ival[0]= type;
      ival[1]= type? gistD.hidden : 0;

      if (type) {
        legend= (char **)p[1];
        legend[0]= p_strcpy(gistD.legend);
        if ((type==1 || type==7) && legend[0] && legend[0][0]=='\001') {
          if (gistA.m.type>=' ' && gistA.m.type<'\177')
            legend[0][0]= (char)gistA.m.type;
          else if (gistA.m.type>=1 && gistA.m.type<=5)
            legend[0][0]= specialMarkers[gistA.m.type-1];
          else
            legend[0][0]= '?';
        }
      }

      ival= (int *)p[2];
      dval= (double *)p[3];
      lval= (long *)p[4];
      if (type==1) {                 /* plg */
        ival[0]= gistA.l.color;
        ival[1]= gistA.l.type;
        ival[2]= gistA.dl.marks;
        ival[3]= gistA.m.color;
        ival[4]= gistA.m.type;
        ival[5]= gistA.dl.rays;
        ival[6]= gistA.dl.closed;
        ival[7]= gistA.dl.smooth;
        dval[0]= gistA.l.width;
        dval[1]= gistA.m.size;
        dval[2]= gistA.dl.mSpace;
        dval[3]= gistA.dl.mPhase;
        dval[4]= gistA.dl.rSpace;
        dval[5]= gistA.dl.rPhase;
        dval[6]= gistA.dl.arrowL;
        dval[7]= gistA.dl.arrowW;
        lval[0]= gistD.n;
        lval[1]= ((char *)gistD.x)-((char *)0);
        lval[2]= ((char *)gistD.y)-((char *)0);
      } else if (type==2) {           /* pldj */
        ival[0]= gistA.l.color;
        ival[1]= gistA.l.type;
        dval[0]= gistA.l.width;
        lval[0]= gistD.n;
        lval[1]= ((char *)gistD.x)-((char *)0);
        lval[2]= ((char *)gistD.y)-((char *)0);
        lval[3]= ((char *)gistD.xq)-((char *)0);
        lval[4]= ((char *)gistD.yq)-((char *)0);
      } else if (type==3) {           /* plt */
        ival[0]= gistA.t.color;
        ival[1]= gistA.t.font;
        ival[2]= gistA.t.orient;
        ival[3]= (gistA.t.alignH | (gistA.t.alignV<<2));
        ival[4]= gistA.t.opaque;
        dval[0]= gistA.t.height/ONE_POINT;
        dval[1]= gistD.x0;
        dval[2]= gistD.y0;
        lval[0]= Safe_strlen(gistD.text);
        lval[1]= ((char *)gistD.text)-((char *)0);
      } else if (type==4) {           /* plm */
        ival[0]= gistA.l.color;
        ival[1]= gistA.l.type;
        ival[2]= gistD.region;
        ival[3]= gistD.boundary;
        ival[4]= gistD.inhibit;
        dval[0]= gistA.l.width;
        lval[0]= gistD.mesh.iMax;
        lval[1]= gistD.mesh.jMax;
        lval[2]= ((char *)gistD.mesh.x)-((char *)0);
        lval[3]= ((char *)gistD.mesh.y)-((char *)0);
        lval[4]= ((char *)gistD.mesh.reg)-((char *)0);
      } else if (type==5) {           /* plf */
        ival[0]= gistD.region;
        ival[1]= gistA.e.type!=L_NONE;
        ival[2]= gistA.e.color;
	ival[3]= gistA.rgb;
        dval[0]= gistA.e.width;
        lval[0]= gistD.mesh.iMax;
        lval[1]= gistD.mesh.jMax;
        lval[2]= ((char *)gistD.mesh.x)-((char *)0);
        lval[3]= ((char *)gistD.mesh.y)-((char *)0);
        lval[4]= ((char *)gistD.mesh.reg)-((char *)0);
        lval[5]= ((char *)gistD.colors)-((char *)0);
      } else if (type==6) {           /* plv */
        ival[0]= gistD.region;
        ival[1]= gistA.l.color;
        ival[2]= gistA.vect.hollow;
        dval[0]= gistA.l.width;
        dval[1]= gistA.vect.aspect;
        dval[2]= gistD.scale;
        lval[0]= gistD.mesh.iMax;
        lval[1]= gistD.mesh.jMax;
        lval[2]= ((char *)gistD.mesh.x)-((char *)0);
        lval[3]= ((char *)gistD.mesh.y)-((char *)0);
        lval[4]= ((char *)gistD.mesh.reg)-((char *)0);
        lval[5]= ((char *)gistD.u)-((char *)0);
        lval[6]= ((char *)gistD.v)-((char *)0);
      } else if (type==7) {           /* plc */
        ival[0]= gistD.region;
        ival[1]= gistA.l.color;
        ival[2]= gistA.l.type;
        ival[3]= gistA.dl.marks;
        ival[4]= gistA.m.color;
        ival[5]= gistA.m.type;
        ival[6]= gistA.dl.smooth;
        dval[0]= gistA.l.width;
        dval[1]= gistA.m.size;
        dval[2]= gistA.dl.mSpace;
        dval[3]= gistA.dl.mPhase;
        lval[0]= gistD.mesh.iMax;
        lval[1]= gistD.mesh.jMax;
        lval[2]= ((char *)gistD.mesh.x)-((char *)0);
        lval[3]= ((char *)gistD.mesh.y)-((char *)0);
        lval[4]= ((char *)gistD.mesh.reg)-((char *)0);
        lval[5]= ((char *)gistD.z)-((char *)0);
        lval[6]= ((char *)gistD.mesh.triangle)-((char *)0);
        lval[7]= gistD.nLevels;
        lval[8]= ((char *)gistD.levels)-((char *)0);
      } else if (type==8) {           /* pli */
	ival[0]= gistA.rgb;
        dval[0]= gistD.px;
        dval[1]= gistD.py;
        dval[2]= gistD.qx;
        dval[3]= gistD.qy;
        lval[0]= gistD.width;
        lval[1]= gistD.height;
        lval[2]= ((char *)gistD.colors)-((char *)0);
      } else if (type==9) {           /* plfp */
	ival[0]= gistA.e.type;
	ival[1]= gistA.e.color;
	ival[2]= gistA.rgb;
	dval[0]= gistA.e.width;
        lval[0]= gistD.n;
        lval[1]= ((char *)gistD.x)-((char *)0);
        lval[2]= ((char *)gistD.y)-((char *)0);
        lval[3]= ((char *)gistD.colors)-((char *)0);
        lval[4]= ((char *)gistD.pn)-((char *)0);
      }
    }

  } else if (n_contour>=0) {
    YError("contour number cannot be specified without element number");

  } else {
    char line[16];
    int i, offset;
    /* print list of legends... */
    if (CalledAsSubroutine()) {
      /* ...to terminal */
      PrintInit(YputsOut);
    } else {
      /* ...to result string array */
      PrintInit(&PutsAsArray);
      PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
    }

    curElement= -1;
    for (i=0 ; (type= GdSetElement(i))!=E_NONE ; i++) {
      sprintf(line, "%s%2d: ", gistD.hidden?"(H)":"", i+1);
      PrintFunc(line);
      offset= 0;
      if ((type==E_LINES || type==E_CONTOURS) && gistD.legend &&
          gistD.legend[0]=='\001') {
        char marker[2];
        marker[1]= '\0';
        if (gistA.m.type>=' ' && gistA.m.type<'\177')
          marker[0]= (char)gistA.m.type;
        else if (gistA.m.type>=1 && gistA.m.type<=5)
          marker[0]= specialMarkers[gistA.m.type-1];
        else
          marker[0]= '?';
        PrintFunc(marker);
        offset= 1;
      }
      if (gistD.legend) PrintFunc(gistD.legend+offset);
      ForceNewline();
    }
  }
}

static void PrintHideLegend(char *line, int type)
{
  int offset= 0;
  char marker[5];
  marker[0]= '\0';
  sprintf(line, "hide= %d,", gistD.hidden);
  PrintFunc(line);
  ForceNewline();
  if ((type==1 || type==7) && gistD.legend && gistD.legend[0]=='\001') {
    marker[0]= '\\';
    marker[1]= marker[2]= '0';
    marker[3]= '1';
    marker[4]= '\0';
    offset= 1;
  }
  sprintf(line, "legend= \"%s%.104s\",", marker,
          gistD.legend? gistD.legend+offset : "");
  PrintFunc(line);
  ForceNewline();
}

static void PrintColor(char *line, int color, int suffix)
{
  if (color>=0) {
    sprintf(line, "color= %d,", color);
    PrintFunc(line);
  } else if (color==P_FG) PrintFunc("color= \"fg\"");
  else if (color==P_BG) PrintFunc("color= \"bg\"");
  else if (color==P_RED) PrintFunc("color= \"red\"");
  else if (color==P_GREEN) PrintFunc("color= \"green\"");
  else if (color==P_BLUE) PrintFunc("color= \"blue\"");
  else if (color==P_CYAN) PrintFunc("color= \"cyan\"");
  else if (color==P_MAGENTA) PrintFunc("color= \"magenta\"");
  else if (color==P_YELLOW) PrintFunc("color= \"yellow\"");
  else if (color==P_GRAYD) PrintFunc("color= \"grayd\"");
  else if (color==P_GRAYC) PrintFunc("color= \"grayc\"");
  else if (color==P_GRAYB) PrintFunc("color= \"grayb\"");
  else if (color==P_GRAYA) PrintFunc("color= \"graya\"");
  else PrintFunc("color= <unknown>");
  PrintSuffix(suffix);
}

static void PrintTypeWidth(char *line, int suffix)
{
  if (gistA.l.type==L_NONE) PrintFunc("type= \"none\"");
  else if (gistA.l.type==L_SOLID) PrintFunc("type= \"solid\"");
  else if (gistA.l.type==L_DASH) PrintFunc("type= \"dash\"");
  else if (gistA.l.type==L_DOT) PrintFunc("type= \"dot\"");
  else if (gistA.l.type==L_DASHDOT) PrintFunc("type= \"dashdot\"");
  else if (gistA.l.type==L_DASHDOTDOT) PrintFunc("type= \"dashdotdot\"");
  else PrintFunc("type= <unknown>");
  sprintf(line, ",  width= %.2f", Safe_dbl(gistA.l.width));
  PrintFunc(line);
  PrintSuffix(suffix);
}

static void PrintMarks(char *line, int suffix)
{
  sprintf(line, "marks= %d,  mcolor= 0x%02lx,  ",
          gistA.dl.marks, gistA.m.color);
  PrintFunc(line);
  if (gistA.m.type<=' ' || gistA.m.type>=0xff)
    sprintf(line, "marker= '\\%o',", gistA.m.type);
  else
    sprintf(line, "marker= '%c',", gistA.m.type);
  PrintFunc(line);
  ForceNewline();
  sprintf(line,
          "  msize= %.2f, mspace= %.5f, mphase= %.5f",
          Safe_dbl(gistA.m.size),
          Safe_dbl(gistA.dl.mSpace), Safe_dbl(gistA.dl.mPhase));
  PrintFunc(line);
  PrintSuffix(suffix);
}

static void PrintRegion(char *line, int suffix)
{
  sprintf(line, "region= %d", gistD.region);
  PrintFunc(line);
  PrintSuffix(suffix);
}

static void PrintSuffix(int suffix)
{
  if (suffix==1) PrintFunc(",  ");
  else if (suffix==3) PrintFunc(",");
  if (suffix&2) ForceNewline();
}

static double Safe_dbl(double x)
{
  if (x>1000.0) return 1000.0;
  else if (x<-1000.0) return -1000.0;
  else return x;
}

static void *MakePropArray(StructDef *base, long size)
{
  Array *array;
  if (!size) return 0;
  array= NewArray(base, NewDimension(size, 1L, (Dimension *)0));
  array->type.dims->references--;
  return array->value.c;
}

/*--------------------------------------------------------------------------*/

#undef N_KEYWORDS
#define N_KEYWORDS 36
static char *editKeys[N_KEYWORDS+1]= {
  "legend", "hide",
  "color", "type", "width",
  "marks", "mcolor", "marker", "msize", "mspace", "mphase",
  "rays", "arrowl", "arroww", "rspace", "rphase", "closed", "smooth",
  "font", "height", "orient", "justify", "opaque",
  "hollow", "aspect", "region", "boundary", "levs", "scale", "scalem",
  "dx", "dy", "edges", "ecolor", "ewidth", "inhibit", 0 };

void Y_pledit(int nArgs)
{
  int type= 0, n_element= 0, n_contour= 0;
  int changes= 0, resetLevs= 0;
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, editKeys, keySymbols);
  int iPass= 0;
  char *legend= 0;

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }

    if (iPass==0) {
      if (YNotNil(stack)) n_element= (int)YGetInteger(stack);
    } else if (iPass==1) {
      if (YNotNil(stack)) n_contour= (int)YGetInteger(stack);
    } else {
      YError("pledit takes at most two non-keyword arguments");
    }

    iPass++;
    stack++;
  }

  /* Yorick uses 1-origin element numbering, Gist uses 0-origin */
  n_element--;
  n_contour--;
  if (n_element<0) {
    if (curElement>=0) {
      n_element= GdFindIndex(curElement);
      if (n_element<0) {
        curElement= -1;
        YError("lost current graphical element for pledit (BUG?)");
      }
    } else if (curElement==-6666) {
      n_element= curIX;
      n_contour= curIXc;
    } else {
      YError("no current graphical element for pledit");
    }
  }
  if (n_element>=0 || n_contour>=0) {
    /* retrieve specified element */
    if (n_element>=0) type= GdSetElement(n_element);
    if (n_contour>=0) {
      if (type!=E_CONTOURS)
        YError("current graphical element is not contours in pledit");
      type= GdSetContour(n_contour);
    }
    curElement= -6666;  /* differs from -1 to allow pledit after plq */
    curIX= n_element;   /* need these, too */
    curIXc= n_contour;
    if (type==E_LINES) type= 1;
    else if (type==E_DISJOINT) type= 2;
    else if (type==E_TEXT) type= 3;
    else if (type==E_MESH) type= 4;
    else if (type==E_FILLED) type= 5;
    else if (type==E_VECTORS) type= 6;
    else if (type==E_CONTOURS) type= 7;
    else if (type==E_CELLS) type= 8;
    else type= 0;
    if (type==0) YError("no such graphical element for pledit");
  }

  /* legend and hide */
  if (keySymbols[0] && YNotNil(keySymbols[0]))
    legend= YGetString(keySymbols[0]);
  if (YNotNil(keySymbols[1])) gistD.hidden= (YGetInteger(keySymbols[1])!=0);

  /* GdLines properties */
  if (YNotNil(keySymbols[2]))
    gistA.l.color= gistA.m.color= gistA.f.color=
      gistA.t.color= YgetColor(keySymbols[2]);
  if (YNotNil(keySymbols[3]))
    gistA.l.type= GetLineType(keySymbols[3]);
  if (YNotNil(keySymbols[4]))
    gistA.l.width= YGetReal(keySymbols[4]);
  if (YNotNil(keySymbols[5]))
    gistA.dl.marks= (YGetInteger(keySymbols[5])!=0);
  if (YNotNil(keySymbols[6]))
    gistA.m.color= YgetColor(keySymbols[6]);
  if (YNotNil(keySymbols[7]))
    gistA.m.type= (int)YGetInteger(keySymbols[7]);
  if (YNotNil(keySymbols[8]))
    gistA.m.size= YGetReal(keySymbols[8]);
  if (YNotNil(keySymbols[9]))
    gistA.dl.mSpace= YGetReal(keySymbols[9]);
  if (YNotNil(keySymbols[10]))
    gistA.dl.mPhase= YGetReal(keySymbols[10]);
  if (YNotNil(keySymbols[11]))
    gistA.dl.rays= (YGetInteger(keySymbols[11])!=0);
  if (YNotNil(keySymbols[12]))
    gistA.dl.arrowL= YGetReal(keySymbols[12]);
  if (YNotNil(keySymbols[13]))
    gistA.dl.arrowW= YGetReal(keySymbols[13]);
  if (YNotNil(keySymbols[14]))
    gistA.dl.rSpace= YGetReal(keySymbols[14]);
  if (YNotNil(keySymbols[15]))
    gistA.dl.rPhase= YGetReal(keySymbols[15]);
  if (YNotNil(keySymbols[16]))
    gistA.dl.closed= (YGetInteger(keySymbols[16])!=0);
  if (YNotNil(keySymbols[17]))
    gistA.dl.smooth= (YGetInteger(keySymbols[17])!=0);

  /* GdText properties */
  if (YNotNil(keySymbols[18]))
    gistA.t.font= GetFont(keySymbols[18]);
  if (YNotNil(keySymbols[19]))
    gistA.t.height= YGetReal(keySymbols[19])*ONE_POINT;
  if (YNotNil(keySymbols[20]))
    gistA.t.orient= YGetInteger(keySymbols[20]);
  if (YNotNil(keySymbols[21]))
    GetJustify(keySymbols[21]);
  if (YNotNil(keySymbols[22]))
    gistA.t.opaque= (YGetInteger(keySymbols[22])!=0);

  if (!gistA.t.orient) {
    gistA.t.orient= TX_RIGHT;
  } else {
    if (gistA.t.orient==1) gistA.t.orient= TX_UP;
    else if (gistA.t.orient==2) gistA.t.orient= TX_LEFT;
    else if (gistA.t.orient==3) gistA.t.orient= TX_DOWN;
    else {
      gistA.t.orient= TX_RIGHT;
      YError("orient= keyword must be 0, 1, 2, or 3");
    }
  }

  /* GdVectors properties */
  if (YNotNil(keySymbols[23]))
    gistA.vect.hollow= (YGetInteger(keySymbols[23])!=0);
  if (YNotNil(keySymbols[24]))
    gistA.vect.aspect= YGetReal(keySymbols[24]);

  if (YNotNil(keySymbols[25])) {  /* region */
    if (type<4 || type>7)
      YError("region= in pledit allowed only for plm, plf, plv, plc");
    gistD.region= (int)YGetInteger(keySymbols[25]);
  }

  if (YNotNil(keySymbols[26])) {  /* boundary */
    if (type!=4) YError("boundary= in pledit allowed only for plm");
    gistD.boundary= (YGetInteger(keySymbols[26])!=0);
  }

  if (YNotNil(keySymbols[27])) {  /* levs */
    double *levels;
    long nLevels= 0;
    if (type!=7) YError("levs= in pledit allowed only for plc");
    levels= Get1Ddouble(keySymbols[27], &nLevels);
    if (!levels)
      YError("pledit cannot recompute default contour levels");
    levels= CopyLevels(levels, nLevels);
    /* WARNING --
       this is a critical code section, since until GdEdit successfully
       completes, Gist owns a pointer to the freed levels -- no way to
       gracefully avoid this without "knowing" more about guts of Gist's
       data structures than seem reasonable here... */
    p_free(gistD.levels);
    gistD.levels= levels;
    gistD.nLevels= nLevels;
    changes|= CHANGE_Z;
    resetLevs= 1;
  }

  if (YNotNil(keySymbols[28])) {  /* scale */
    if (type!=6) YError("scale= in pledit allowed only for plv");
    gistD.scale= YGetReal(keySymbols[28]);
  }

  if (YNotNil(keySymbols[29])) {  /* scalem */
    if (type!=6) YError("scalem= in pledit allowed only for plv");
    gistD.scale*= YGetReal(keySymbols[29]);
  }

  if (YNotNil(keySymbols[30])) {  /* dx */
    if (type!=3) YError("dx= in pledit allowed only for plt");
    gistD.x0+= YGetReal(keySymbols[30]);
  }

  if (YNotNil(keySymbols[31])) {  /* dy */
    if (type!=3) YError("dy= in pledit allowed only for plt");
    gistD.y0+= YGetReal(keySymbols[31]);
  }

  if (YNotNil(keySymbols[32]))
    gistA.e.type= YGetInteger(keySymbols[32])? L_SOLID : L_NONE;
  if (YNotNil(keySymbols[33]))
    gistA.e.color= YgetColor(keySymbols[33]);
  if (YNotNil(keySymbols[34]))
    gistA.e.width= YGetReal(keySymbols[34]);

  if (YNotNil(keySymbols[35])) {  /* inhibit */
    if (type!=4) YError("inhibit= in pledit allowed only for plm");
    gistD.inhibit= (int)YGetInteger(keySymbols[35]);
  }

  if (legend) {
    /* Some giggery-pokery necessary to get the old legend deleted properly,
       and the new legend allocated properly, so that Gist will delete it
       correctly when the graphical element is deleted.  */
    char *oldleg= gistD.legend;
    gistD.legend= p_malloc(strlen(legend)+1);
    strcpy(gistD.legend, legend);
    legend= oldleg;
  }
  GdEdit(changes);
  if (legend) p_free(legend);
  if (resetLevs) tmpLevels= 0;
  Drop(nArgs);
}

#undef N_KEYWORDS
#define N_KEYWORDS 29
static char *dfltKeys[N_KEYWORDS+1]= {
  "color", "type", "width",
  "marks", "mcolor", "marker", "msize", "mspace", "mphase",
  "rays", "arrowl", "arroww", "rspace", "rphase",
  "font", "height", "orient", "justify", "opaque",
  "hollow", "aspect", "dpi", "style", "legends", "palette", "maxcolors",
  "edges", "ecolor", "ewidth", 0 };

void Y_pldefault(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, dfltKeys, keySymbols);

  if (stack<=sp) YError("pldefault takes no non-keyword arguments");

  /* retrieve all default settings */
  GhGetLines();
  GhGetMesh();
  GhGetVectors();
  GhGetText();

  if (YNotNil(keySymbols[0]))
    gistA.l.color= gistA.m.color= gistA.f.color=
      gistA.t.color= gistA.e.color= YgetColor(keySymbols[0]);
  if (YNotNil(keySymbols[1]))
    gistA.l.type= GetLineType(keySymbols[1]);
  if (YNotNil(keySymbols[2]))
    gistA.l.width= YGetReal(keySymbols[2]);
  if (YNotNil(keySymbols[3]))
    gistA.dl.marks= (YGetInteger(keySymbols[3])!=0);
  if (YNotNil(keySymbols[4]))
    gistA.m.color= YgetColor(keySymbols[4]);
  if (YNotNil(keySymbols[5]))
    gistA.m.type= (int)YGetInteger(keySymbols[5]);
  if (YNotNil(keySymbols[6]))
    gistA.m.size= YGetReal(keySymbols[6]);
  if (YNotNil(keySymbols[7]))
    gistA.dl.mSpace= YGetReal(keySymbols[7]);
  if (YNotNil(keySymbols[8]))
    gistA.dl.mPhase= YGetReal(keySymbols[8]);
  if (YNotNil(keySymbols[9]))
    gistA.dl.rays= (YGetInteger(keySymbols[9])!=0);
  if (YNotNil(keySymbols[10]))
    gistA.dl.arrowL= YGetReal(keySymbols[10]);
  if (YNotNil(keySymbols[11]))
    gistA.dl.arrowW= YGetReal(keySymbols[11]);
  if (YNotNil(keySymbols[12]))
    gistA.dl.rSpace= YGetReal(keySymbols[12]);
  if (YNotNil(keySymbols[13]))
    gistA.dl.rPhase= YGetReal(keySymbols[13]);
  if (YNotNil(keySymbols[14]))
    gistA.t.font= GetFont(keySymbols[14]);
  if (YNotNil(keySymbols[15]))
    gistA.t.height= YGetReal(keySymbols[15])*ONE_POINT;
  if (YNotNil(keySymbols[16]))
    gistA.t.orient= YGetInteger(keySymbols[16]);
  if (YNotNil(keySymbols[17]))
    GetJustify(keySymbols[17]);
  if (YNotNil(keySymbols[18]))
    gistA.t.opaque= (YGetInteger(keySymbols[18])!=0);
  if (YNotNil(keySymbols[19]))
    gistA.vect.hollow= (YGetInteger(keySymbols[19])!=0);
  if (YNotNil(keySymbols[20]))
    gistA.vect.aspect= YGetReal(keySymbols[20]);

  if (!gistA.t.orient) {
    gistA.t.orient= TX_RIGHT;
  } else {
    if (gistA.t.orient==1) gistA.t.orient= TX_UP;
    else if (gistA.t.orient==2) gistA.t.orient= TX_LEFT;
    else if (gistA.t.orient==3) gistA.t.orient= TX_DOWN;
    else {
      gistA.t.orient= TX_RIGHT;
      YError("orient= keyword must be 0, 1, 2, or 3");
    }
  }

  if (YNotNil(keySymbols[21])) {
    int dpi= YGetInteger(keySymbols[21]);
    /*if (dpi!=75 && dpi!=100)
      YError("dpi=75 or dpi=100 are only legal values");*/
    if (dpi<25) dpi = 25;
    else if (dpi>2400) dpi = 2400;
    defaultDPI= dpi;
  }
  if (YNotNil(keySymbols[22])) {
    char *style= defaultStyle;
    defaultStyle= 0;
    p_free(style);
    style= YGetString(keySymbols[22]);
    if (style && style[0]) defaultStyle= p_strcpy(style);
  }
  if (YNotNil(keySymbols[23]))
    /* legends= keyword -- turn on/off legend dumping to hcp file */
    defaultLegends= (YGetInteger(keySymbols[23])!=0);
  if (keySymbols[24]) {
    char *name= defaultPalette;
    defaultPalette= 0;
    p_free(name);
    if (YNotNil(keySymbols[24]))
      defaultPalette= p_strcpy(YGetString(keySymbols[24]));
  }
  if (YNotNil(keySymbols[25]))
    maxColors= YGetInteger(keySymbols[25]);

  if (YNotNil(keySymbols[26]))
    gistA.e.type= YGetInteger(keySymbols[26])? L_SOLID : L_NONE;
  if (YNotNil(keySymbols[27]))
    gistA.e.color= YgetColor(keySymbols[27]);
  if (YNotNil(keySymbols[28]))
    gistA.e.width= YGetReal(keySymbols[28]);

  /* store all default settings */
  GhSetLines();
  GhSetMesh();
  GhSetVectors();
  GhSetText();
  GhSetFill();
}

#undef N_KEYWORDS
#define N_KEYWORDS 6
static char *gridKeys[N_KEYWORDS+1]= {
  "color", "type", "width", "base60", "degrees", "hhmm", 0 };

void Y_gridxy(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, gridKeys, keySymbols);
  int xgrid= 0;
  int ygrid= 0;
  int iPass= 0;
  int ticks = 0;

  while (stack<=sp) {
    if (!stack->ops) { stack+= 2; continue; }

    if (iPass==0) {
      if (YNotNil(stack)) xgrid= (int)YGetInteger(stack);
    } else if (iPass==1) {
      if (YNotNil(stack)) ygrid= (int)YGetInteger(stack);
    } else {
      YError("gridxy takes at most two non-keyword arguments");
    }

    iPass++;
    stack++;
  }

  /* If a single argument is given, use it for both xgrid and ygrid */
  if (iPass==1) ygrid= xgrid;

  CheckDefaultWindow();

  if (YNotNil(keySymbols[0]))
    gistD.ticks.horiz.gridStyle.color=
      gistD.ticks.vert.gridStyle.color= YgetColor(keySymbols[0]);
  if (YNotNil(keySymbols[1]))
    gistD.ticks.horiz.gridStyle.type=
      gistD.ticks.vert.gridStyle.type= GetLineType(keySymbols[1]);
  if (YNotNil(keySymbols[2]))
    gistD.ticks.horiz.gridStyle.width=
      gistD.ticks.vert.gridStyle.width= YGetReal(keySymbols[2]);
  if (YNotNil(keySymbols[3]))
    ticks |= 1 | ((YGetInteger(keySymbols[3]) & 3) << 1);
  if (YNotNil(keySymbols[4]))
    ticks |= 1 | ((YGetInteger(keySymbols[4]) & 3) << 3);
  if (YNotNil(keySymbols[5]))
    ticks |= 1 | ((YGetInteger(keySymbols[5]) & 3) << 5);
  if (ticks&1) {
    if (ticks&0x2a) {
      if (ticks&0x2) GdAltTick(&Base60Ticks,0,0,0);
      else if (ticks&0x8) GdAltTick(&Base60Ticks,&DegreeLabels,0,0);
      else GdAltTick(&Base60Ticks,&HourLabels,0,0);
      gistD.ticks.horiz.flags |= ALT_TICK;
      if (ticks&0x2) gistD.ticks.horiz.flags &= ~ALT_LABEL;
      else gistD.ticks.horiz.flags |= ALT_LABEL;
    } else {
      gistD.ticks.horiz.flags &= ~(ALT_TICK|ALT_LABEL);
    }
    if (ticks&0x54) {
      if (ticks&0x4) GdAltTick(0,0,&Base60Ticks,0);
      else if (ticks&0x10) GdAltTick(0,0,&Base60Ticks,&DegreeLabels);
      else GdAltTick(0,0,&Base60Ticks,&HourLabels);
      gistD.ticks.vert.flags |= ALT_TICK;
      if (ticks&0x4) gistD.ticks.vert.flags &= ~ALT_LABEL;
      else gistD.ticks.vert.flags |= ALT_LABEL;
    } else {
      gistD.ticks.vert.flags &= ~(ALT_TICK|ALT_LABEL);
    }
  }

  if (iPass>0) {
    gistD.ticks.horiz.flags&= ~(GRID_F|GRID_O);
    if (xgrid==1)
      gistD.ticks.horiz.flags|= GRID_F;
    else if (xgrid==2)
      gistD.ticks.horiz.flags|= GRID_O;
    if (xgrid&0x200) {
      gistD.ticks.horiz.flags= (xgrid&0x1ff);
      gistD.ticks.frame= (xgrid&0x400)!=0;
    }

    gistD.ticks.vert.flags&= ~(GRID_F|GRID_O);
    if (ygrid&1)
      gistD.ticks.vert.flags|= GRID_F;
    else if (ygrid&2)
      gistD.ticks.vert.flags|= GRID_O;
    if (ygrid&0x200) {
      gistD.ticks.vert.flags= (ygrid&0x1ff);
      gistD.ticks.frame= (ygrid&0x400)!=0;
    }
  }

  GdSetPort();
}

/*--------------------------------------------------------------------------*/

static void FreeReference(void *obj)
{
  Array *array= obj? Pointee(obj) : 0;
  Unref(array);
}

/* defined in task.c */
extern void (*CleanUpForExit)(void);

static void (*OtherCleanUp)(void)= 0;
static void CleanUpGraphics(void);
static void CleanUpGraphics(void)
{
  int n;
  if (hcpDefault) GpKillEngine(hcpDefault);
  for (n=GH_NDEVS-1 ; n>=0 ; n--) {
    if (ghDevices[n].display) GpKillEngine(ghDevices[n].display);
    if (ghDevices[n].hcp) GpKillEngine(ghDevices[n].hcp);
  }
  if (OtherCleanUp) OtherCleanUp();
}

extern void yg_before_wait(void);
void
yg_before_wait(void)
{
  GhBeforeWait();
}

void
Y_set_gpath(int argc)
{
  char *p = ((argc==1) && YNotNil(sp))? YGetString(sp) : 0;
  if (argc > 1) YError("set_gpath accepts only one argument");
  if (!CalledAsSubroutine()) {
    Array *a = PushDataBlock(NewArray(&stringStruct, (Dimension *)0));
    a->value.q[0] = p_strcpy(g_set_path((char*)0));
  }
  if (p) g_set_path(p);
}

void Y__pl_init(int nArgs)
{
#ifndef NO_XLIB
  g_initializer(&ym_argc, ym_argv);
#else
  extern char *g_argv0;
  g_argv0 = ym_argv? ym_argv[0] : 0;
#endif

  /* Install routine to kill graphics engines when Yorick quits.  */
  OtherCleanUp= CleanUpForExit;
  CleanUpForExit= &CleanUpGraphics;

  /* Additionally, set up so that Gist Drauing structures actually own
     a use of mesh-sized Yorick Arrays.  This allows one or more
     Drauings to share these potentially large objects.  */
  GdFree= &FreeReference;

  /* Install Yorick's best guess at a GISTPATH.  The GISTPATH
     environment variable, if present, will be used; otherwise the
     argument to this function, if non-nil, will be used; otherwise,
     the default compiled into libgist.a will be used.  */
  if (!p_getenv("GISTPATH") && nArgs==1 && YNotNil(sp))
    gistPathDefault= p_strcpy(YGetString(sp));
  GhSetXHandler((void (*)(char *))&YError);

  /* Set up parser to pass string equivalents of arguments to the
     plotting functions for use in the construction of default legends.
     NB-- The parser can only quine functions invoked as subroutines,
          so all quined functions must check CalledAsSubroutine().  */
  YpQuine("plg", 2);
  YpQuine("plm", 3);
  YpQuine("plc", 4);
  YpQuine("plv", 4);
  YpQuine("plf", 4);
  YpQuine("pli", 1);
  /* plt does not use legends */
  YpQuine("pldj", 4);

  /* Default is to put occasional markers on curves.  */
  GhGetLines();
  gistA.dl.marks= 1;
  GhSetLines();

  /* Default text is 14 point Helvetica.  */
  GhGetText();
  gistA.t.font= T_HELVETICA;
  gistA.t.height= 14.0*ONE_POINT;
  GhSetText();
}

void Y_keybd_focus(int nArgs)
{
  /* Set the input hint to false, meaning that Yorick graphics
   * windows do not ever want keyboard focus.  This is not supposed
   * to affect mouse input events, which are wanted.  */
#ifndef NO_XLIB
  if (nArgs==1) gist_input_hint= YGetInteger(sp);
#endif
}

/*--------------------------------------------------------------------------*/

#undef N_KEYWORDS
#define N_KEYWORDS 3
static char *bsKeys[N_KEYWORDS+1]= { "top", "cmin", "cmax", 0 };

void Y_bytscl(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, bsKeys, keySymbols);
  double *z, zmin, zmax, scale, offset, zz;
  Operand op;
  Array *array;
  GpColor *zc;
  long i;

  z= 0;
  while (stack<=sp) {
    if (!stack->ops) { stack+=2; continue; }
    if (z) { z= 0; break; }
    stack->ops->FormOperand(stack, &op);
    if (op.ops->promoteID>T_DOUBLE)
      YError("bytscl argument must be convertable to type double");
    op.ops->ToDouble(&op);
    z= op.value;
    stack++;
  }
  if (!z) YError("bytscl takes exactly one non-keyword argument");

  GrabByteScale(keySymbols, &scale, &offset, &zmin, &zmax,
                z, (int *)0, 0, op.type.number+1, 2L, 1);

  array= PushDataBlock(NewArray(&charStruct, op.type.dims));
  zc= (GpColor *)array->value.c;

  for (i=0 ; i<op.type.number ; i++) {
    zz= z[i];
    if (zz<zmin) zz= zmin;
    else if (zz>zmax) zz= zmax;
    zc[i]= (int)((zz-offset)*scale);
  }
}

/*--------------------------------------------------------------------------*/

#undef N_KEYWORDS
#define N_KEYWORDS 2
static char *cntrKeys[N_KEYWORDS+1]= { "triangle", "region", 0 };

void Y_contour(int nArgs)
{
  Symbol *keySymbols[N_KEYWORDS];
  Symbol *stack= YGetKeywords(sp-nArgs+1, nArgs, cntrKeys, keySymbols);
  long iMax= 0, jMax= 0;
  double *z= 0;
  GaQuadMesh mesh;
  long xci= 0, yci= 0;
  Symbol *xs= 0 , *ys= 0;
  double *levs= 0;
  int two_levels= 0;
  Dimension *dims;
  Array *array;
  double *xcp, *ycp;
  long *np, nparts, ntotal;
  int region= 0;
  int iPass= 0;

  while (stack<=sp && iPass<4) {
    if (!stack->ops) { stack+= 2; continue; }

    if (iPass==0) {
      yci= YGet_Ref(stack);
      ys= stack;
    } else if (iPass==1) {
      xci= YGet_Ref(stack);
      xs= stack;
    } else if (iPass==2) {
      levs= YGet_D(stack, 0, &dims);
      if (dims && (dims->number>2 || dims->next))
        YError("contour levs must be either single value or value pair");
      if (dims && dims->number==2) two_levels= 1;
    } else if (iPass==3) {
      z= Get2Ddouble(stack, &jMax, &iMax);
    }

    iPass++;
    stack++;
  }
  if (iPass<4) YError("contour needs at least six arguments");
  stack= GrabMesh(stack, keySymbols[0], &mesh,
                  (char **)0, (char **)0, (char **)0, 1);
  while (stack<=sp) {
    if (!stack->ops) stack+= 2;
    else YError("contour takes at most seven non-keyword arguments");
  }
  if (mesh.iMax!=iMax || mesh.jMax!=jMax)
    YError("z array must have same dimensions as mesh in contour");
  if (YNotNil(keySymbols[1]))
    region= (int)YGetInteger(keySymbols[1]);

  if (!mesh.triangle) {
    /* provide a temporary triangle array if none supplied */
    short *triangle;
    long ijMax= iMax*jMax;
    dims= tmpDims;
    tmpDims= 0;
    FreeDimension(dims);
    tmpDims= NewDimension(ijMax, 1L, (Dimension *)0);
    triangle= ((Array*)PushDataBlock(NewArray(&shortStruct,
                                              tmpDims)))->value.s;
    mesh.triangle= triangle;
    while (ijMax--) *(triangle++)= 0;
    CheckStack(1);
  }

  /* initialize trace, counting ntotal and nparts */
  ntotal= two_levels? GcInit2(&mesh,region,z,levs,30L,&nparts) :
    GcInit1(&mesh,region,z,levs[0],&nparts);

  if (!ntotal) {
    /* handle case of no points on contour */
    PushDataBlock(RefNC(&nilDB));
    YPut_Result(sp, yci);
    YPut_Result(sp, xci);
    return;
  }

  /* stuff x and y arrays for results onto stack over output symbols */
  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  tmpDims= dims= NewDimension(ntotal, 1L, (Dimension *)0);
  array= NewArray(&doubleStruct, tmpDims);
  ys->value.db= (DataBlock *)array;
  ys->ops= &dataBlockSym;
  ycp= array->value.d;
  array= NewArray(&doubleStruct, tmpDims);
  xs->value.db= (DataBlock *)array;
  xs->ops= &dataBlockSym;
  xcp= array->value.d;

  /* return list goes on top of stack */
  tmpDims= 0;
  FreeDimension(dims);
  tmpDims= NewDimension(nparts, 1L, (Dimension *)0);
  array= PushDataBlock(NewArray(&longStruct, tmpDims));
  np= array->value.l;

  if (GcTrace(np, xcp, ycp)!=ntotal) YError("GcTrace failed in contour");

  /* move results from stack back to output symbols */
  YPut_Result(ys, yci);
  YPut_Result(xs, xci);
}

void Y_mesh_loc(int nArgs)
{
  Symbol *stack= sp-nArgs+1;
  double *x0= 0, *y0= 0;
  Operand xop, yop;
  Array *result;
  long i, n, *zone;
  GaQuadMesh mesh;
  int iPass= 0;
  Dimension *dims;
  long ix0, j, ijx, *bndy, nbndy;

  mesh.x= mesh.y= 0;
  mesh.reg= 0;
  mesh.iMax= mesh.jMax= 0;
  mesh.triangle= 0;

  if (nArgs<2) YError("mesh_loc requires at least two arguments");
  while (stack<=sp) {
    if (!stack->ops) YError("mesh_loc takes no keyword arguments");

    if (iPass==0) {
      Dimension *dims;
      y0= YGet_D(stack, 0, &dims);
      stack->ops->FormOperand(stack, &yop);
    } else if (iPass==1) {
      Dimension *dims;
      x0= YGet_D(stack, 0, &dims);
      stack->ops->FormOperand(stack, &xop);
    } else if (iPass==2) mesh.y= Get2Ddouble(stack, &mesh.jMax, &mesh.iMax);
    else if (iPass==3) mesh.x= Get2Ddouble(stack, &mesh.jMax, &mesh.iMax);
    else if (iPass==4) mesh.reg= Get2Dint(stack, &mesh.jMax, &mesh.iMax);
    else YError("mesh_loc takes at most five arguments");

    iPass++;
    stack++;
  }

  if ((mesh.x!=0)^(mesh.y!=0))
    YError("both y and x arrays must be specified for a mesh");

  if (!mesh.x) {
    /* neither y nor x have been specified -- use defaults */
    if (!xMesh)
      YError("no default mesh exists to define y and x -- use plmesh");
    if (mesh.reg && (iMesh!=mesh.iMax || jMesh!=mesh.jMax))
      YError("ireg must have same dimensions as default mesh");
    mesh.iMax= iMesh;
    mesh.jMax= jMesh;
    mesh.x= xMesh;
    mesh.y= yMesh;

  } else {
    /* both y and x have been specified -- copy them for Gist */
    if (mesh.iMax<2 || mesh.jMax<2)
      YError("a mesh must have dimensions of at least 2-by-2");
  }

  if (!mesh.reg && mesh.x==xMesh) mesh.reg= regMesh;

  if (BinaryConform(&xop, &yop)&4) YError("x0 and y0 not conformable");
  n= TotalNumber(tmpDims);
  if (n>1) {
    result= PushDataBlock(NewArray(&longStruct, tmpDims));
    zone= result->value.l;
  } else {
    PushLongValue(0);
    zone= &sp->value.l;
  }

  /* get fastest varying dimension in (x0,y0) */
  dims= tmpDims;
  ix0= 1;
  while (dims && dims->next) {
    dims= dims->next;
    if (dims->number>1) ix0= dims->number;
  }

  ijx= mesh.iMax*mesh.jMax;
  if (mesh.reg) {
    long i0= 0;
    for (j=mesh.iMax+1 ; j<ijx ; j++) {
      if ((++i0)==mesh.iMax) { i0= 1; j++; }
      if (mesh.reg[j]) break;
    }
    if (j>=ijx) j= -1;
  } else {
    j= mesh.iMax+1;
  }
  bndy= 0;
  nbndy= 0;
  for (i=0 ; i<n ; i++) {
    if (i && !(i%ix0)) j= zone[i-ix0]-1;
    j= QuickMeshZone(x0[i], y0[i], mesh.x, mesh.y, mesh.reg,
                     mesh.iMax, ijx, j, bndy, nbndy);
    if (!bndy && j<0) {
      bndy= BuildMeshBndy(mesh.x, mesh.y, mesh.reg, mesh.iMax, ijx, &nbndy);
      j= QuickMeshZone(x0[i], y0[i], mesh.x, mesh.y, mesh.reg,
                       mesh.iMax, ijx, j, bndy, nbndy);
    }
    zone[i]= j+1;
  }
  if (bndy) Drop(1);
}

/*--------------------------------------------------------------------------*/

#ifndef NO_MOUSE
static int MouseCallBack(Engine *engine, int system,
                         int release, GpReal x, GpReal y,
                         int butmod, GpReal xn, GpReal yn);
static char *defaultPrompts[2]= {
  "<Click mouse at point>", "<Press, drag, and release mouse>" };
static Array *mouse_array = 0;
#endif

void Y_mouse(int nArgs)
{
#ifdef DISPLAY_MOUSE
  Symbol *stack= sp-nArgs+1;
  char *prompt= 0;
  int system= -1, style= 0, iPass= 0;
  int n= GhGetPlotter();

  if (n<0 || !ghDevices[n].display)
    YError("no current graphics window for mouse function");

  while (stack<=sp) {
    if (!stack->ops) YError("mouse function takes no keyword arguments");
    if (iPass==0)
      system= YNotNil(stack)? YGetInteger(stack) : -1;
    else if (iPass==1)
      style= YNotNil(stack)? YGetInteger(stack) : 0;
    else if (iPass==2)
      prompt= YNotNil(stack)? YGetString(stack) : 0;
    else
      YError("mouse function takes at most three arguments");
    iPass++;
    stack++;
  }

  /* GhWaitDisplay();   otherwise can lock up ?? */
  GhBeforeWait();    /* be sure display is current */
  if (!prompt) YPrompt(defaultPrompts[style!=0]);
  else if (prompt[0]) YPrompt(prompt);
  if (!prompt || prompt[0]) YPrompt("\n");

  if (DISPLAY_MOUSE(ghDevices[n].display, style, system, &MouseCallBack)) {
    PushDataBlock(RefNC(&nilDB));
  } else {
    mouse_array=
      PushDataBlock(NewArray(&doubleStruct,
                             NewDimension(11L, 1L, (Dimension *)0)));
    mouse_array->type.dims->references--;
    for (n=0 ; n<11 ; n++) mouse_array->value.d[n]= 0.0;
    if (yg_pc_resume) {
      yg_got_expose();
      YError("mouse while already waiting or suspended");
    } else {
      yg_blocking = 1;
      yg_pc_resume = ym_suspend();
    }
  }
#else
  YError("no mouse function in this version of Yorick");
#endif
}

#ifndef NO_MOUSE
static int MouseCallBack(Engine *engine, int system,
                         int release, GpReal x, GpReal y,
                         int butmod, GpReal xn, GpReal yn)
{
  int n= GhGetPlotter();
  if (n<0 || ghDevices[n].display!=engine) {
    mouse_array = 0;
    yg_got_expose();
    return 1;
  } else if (!mouse_array || sp->ops!=&dataBlockSym ||
             mouse_array != (Array*)sp->value.db ||
             mouse_array->type.base != &doubleStruct ||
             mouse_array->type.number != 11) {
    mouse_array = 0;
    yg_got_expose();
    return 1;
  } else if (release == -1) {
    mouse_array = 0;
    yg_got_expose();
    return 1;
  }
  if (!release) {
    mouse_array->value.d[8]= (double)system;
    mouse_array->value.d[9]= (double)butmod;
    mouse_array->value.d[0]= x;
    mouse_array->value.d[1]= y;
    mouse_array->value.d[4]= xn;
    mouse_array->value.d[5]= yn;
  } else {
    mouse_array->value.d[10]= (double)butmod;
    mouse_array->value.d[2]= x;
    mouse_array->value.d[3]= y;
    mouse_array->value.d[6]= xn;
    mouse_array->value.d[7]= yn;
    mouse_array = 0;
  }
  return 0;
}
#endif

/*--------------------------------------------------------------------------*/

int YCurrentPlotter(void)
{
  return GhGetPlotter();
}

/*--------------------------------------------------------------------------*/

void Y_current_window(int nArgs)
{
  PushIntValue(GhGetPlotter());
}

void
Y_pause(int nArgs)
{
  long timeout;
  if (nArgs!=1) YError("pause requires exactly one argument");
  timeout = YGetInteger(sp);
  if (timeout<0) timeout = 0;
  Drop(nArgs);
  sp->ops = &intScalar;
  sp->value.i = 0;
  if (yg_pc_resume) {
    yg_got_expose();
    YError("pause while already waiting or suspended");
  } else {
    p_set_alarm(0.001*timeout, yg_alarm, (sp-spBottom)+(char*)0);
    yg_blocking = 2;
    yg_pc_resume = ym_suspend();
  }
}

/* ARGSUSED */
static void
yg_alarm(void *context)
{
  if (yg_blocking != 2) return;
  if (sp-spBottom==((char*)context-(char*)0) &&
      sp->ops==&intScalar && sp->value.i==0)
    sp->value.i = 1;
  yg_got_expose();
}

void
Y_rgb_read(int nArgs)
{
  int n = GhGetPlotter();
  long nx, ny;
  Array *result;
  Dimension *dims = tmpDims;
  if (nArgs>1) YError("rgb_read takes no more than one argument");
  if (nArgs==1 && YNotNil(sp)) n = YGetInteger(sp);
  if (n<0 || n>=GH_NDEVS || !ghDevices[n].display ||
      RGB_READER(ghDevices[n].display, (GpColor*)0, &nx, &ny))
    YError("rgb_read(n_window) with no such n_window");
  tmpDims = 0;
  FreeDimension(dims);
  tmpDims = NewDimension(3L, 1L, (Dimension *)0);
  tmpDims = NewDimension(nx, 1L, tmpDims);
  tmpDims = NewDimension(ny, 1L, tmpDims);
  result = PushDataBlock(NewArray(&charStruct, tmpDims));
  RGB_READER(ghDevices[n].display, (GpColor*)result->value.c, &nx, &ny);
}

#ifdef NO_XLIB
/* ARGSUSED */
static int
my_rgb_read(Engine *eng, GpColor *rgb, long *nx, long *ny)
{
  YError("rgb_read impossible - no interactive graphics in this yorick");
  return 1;
}
#endif

/*--------------------------------------------------------------------------*/

void
Y_current_mouse(int argc)
{
#ifdef NO_XLIB
  PushDataBlock(RefNC(&nilDB));
#else
  double x, y;
  int sys, win, target_win;
  Array *array;
  double *result;

  if (argc != 1) {
    YError("current_mouse takes exactly one, possibly nil, argument");
  }
  win = GhGetMouse(&sys, &x, &y);
  if (YNotNil(sp)) {
    target_win = (int)YGetInteger(sp);
  } else {
    target_win = win;
  }
  if (win < 0 || win != target_win) {
    PushDataBlock(RefNC(&nilDB));
  } else {
    array = PushDataBlock(NewArray(&doubleStruct,
				   NewDimension(4L, 1L, (Dimension *)0)));
    --array->type.dims->references;
    result = array->value.d;
    result[0] = x;
    result[1] = y;
    result[2] = sys;
    result[3] = win;
  }
#endif
}

/*--------------------------------------------------------------------------*/
