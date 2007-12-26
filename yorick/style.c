/*
 * $Id: style.c,v 1.2 2007-12-26 04:25:46 dhmunro Exp $
 * Set/get details of graphics style for style.i functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "gist.h"
#include "hlevel.h"
#include "draw.h"

/* various X headers included by xfancy.h define True and False */
#undef True
#undef False

#include "ydata.h"
#include "pstdlib.h"

typedef struct GfakeSystem GfakeSystem;
struct GfakeSystem {
  double viewport[4];    /* [xmin,xmax,ymin,ymax] in NDC coordinates */
  GaTickStyle ticks;     /* tick style for this coordinate system */
  char *legend;          /* e.g.- "System 0" or "System 1", p_malloc */
};

/* If nsys==0, this is a query operation which fills in the input
               data arrays (if the pointers are non-zero)
   otherwise, systems is systems[nsys], and the operation is to
               set the values specified in the data arrays
   the return value is always the number of coordinate systems;
   for queries the routine must be called twice, first with systems==0
   to retrieve the number of systems, then with a large enough systems
   to hold the returned values */
extern int raw_style(long nsys, int *landscape,
                     GfakeSystem *systems, GeLegendBox *legends);

extern BuiltIn Y_viewport;

/* write wrapper routine by hand -- someday should let codger do it */
extern BuiltIn Y_raw_style;
void Y_raw_style(int nArgs)
{
  if (nArgs!=4) YError("raw_style takes exactly 4 arguments");
  PushIntValue(raw_style(yarg_si(3),yarg_i(2,0),*yarg_p(1,0),*yarg_p(0,0)));
}

int raw_style(long nsys, int *landscape,
              GfakeSystem *systems, GeLegendBox *legends)
{
  extern int YCurrentPlotter(void); /* defined in graph.c */
  int nsy= YCurrentPlotter();
  Drauing *drawing= (nsy>=0 && nsy<GH_NDEVS)? ghDevices[nsy].drawing : 0;
  GeSystem *sys= drawing? drawing->systems : 0;

  if (!nsys) {
    /* query operation */
    if (!drawing) return 0;
    if (landscape) *landscape= drawing->landscape;
    nsy= drawing->nSystems;
    if (systems && nsy>0) {
      int i;
      for (i=0 ; i<nsy ; i++,sys=(GeSystem *)sys->el.next) {
        if (systems[i].legend) {
          char *legend= systems[i].legend;
          systems[i].legend= 0;
          p_free(legend);
        }
        systems[i].legend= p_strcpy(sys->el.legend);
        systems[i].viewport[0]= sys->trans.viewport.xmin;
        systems[i].viewport[1]= sys->trans.viewport.xmax;
        systems[i].viewport[2]= sys->trans.viewport.ymin;
        systems[i].viewport[3]= sys->trans.viewport.ymax;
        /* lazy today -- use ANSI struct assignment */
        systems[i].ticks= sys->ticks;
      }
    }
    if (legends) {
      /* lazy today -- use ANSI struct assignment */
      legends[0]= drawing->legends[0];
      legends[1]= drawing->legends[1];
    }
    return nsy;

  } else {
    /* set new style operation */
    int i;
    extern void GdKillSystems(void);  /* defined in draw.c */
    GpBox vp;

    if (!landscape || !systems || !legends)
      YError("missing data in Y_raw_style call");

    /* don't clobber the current display list(s) unless the
       number of coordinate systems has changed */
    nsy= drawing? drawing->nSystems : 0;
    if (nsy != nsys) GdKillSystems();

    for (i=0 ; i<nsys ; i++) {
      gistD.hidden= 0;
      gistD.legend= systems[i].legend;
      vp.xmin= systems[i].viewport[0];
      vp.xmax= systems[i].viewport[1];
      vp.ymin= systems[i].viewport[2];
      vp.ymax= systems[i].viewport[3];
      if (nsy==nsys) {
        GdSetSystem(i+1);
        /* don't bother with the legend */
        gistD.trans.viewport.xmin= vp.xmin;
        gistD.trans.viewport.xmax= vp.xmax;
        gistD.trans.viewport.ymin= vp.ymin;
        gistD.trans.viewport.ymax= vp.ymax;
        /* lazy today -- use ANSI struct assignment */
        gistD.ticks= systems[i].ticks;
        GdSetPort();
      } else if (GdNewSystem(&vp, &systems[i].ticks)<0) {
        gistD.legend= 0;
        YError("GdNewSystem failed in Y_raw_style");
      }
      gistD.legend= 0;
    }
    if (nsys && nsy==nsys) GdSetSystem(1);

    for (i=0 ; i<2 ; i++)
      GdLegendBox(i, legends[i].x, legends[i].y,
                  legends[i].dx, legends[i].dy,
                  &legends[i].textStyle, legends[i].nchars,
                  legends[i].nlines, legends[i].nwrap);

    GdLandscape(*landscape);

    return (int)nsys;
  }
}

void Y_viewport(int nArgs)
{
  Array *array=
    PushDataBlock(NewArray(&doubleStruct,
                           NewDimension(4L, 1L, (Dimension *)0)));
  double *port= array->value.d;
  array->type.dims->references--;
  port[0]= gistD.trans.viewport.xmin;
  port[1]= gistD.trans.viewport.xmax;
  port[2]= gistD.trans.viewport.ymin;
  port[3]= gistD.trans.viewport.ymax;
}
