/*
 * $Id: binstd.c,v 1.1 2005-09-18 22:03:53 dhmunro Exp $
 * Define various standard binary data formats.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "binio.h"
#include "pstdlib.h"

/* ------------------------------------------------------------------------ */
/*                     FLOATING POINT FORMATS                               */
/* ------------------------------------------------------------------------ */

extern FPLayout fp4ieee, fp8ieee, fp12ieee,
  fp8cray, fp4vax, fp8hvax, fp8gvax;

#define REF_COUNTER 0
/*                               S& E&  E#  M&  M# M? E-bias  */

FPLayout fp4ieee=  {REF_COUNTER, 0, 1,  8,  9, 23, 0, 0x7fL};
FPLayout fp8ieee=  {REF_COUNTER, 0, 1, 11, 12, 52, 0, 0x3ffL};
FPLayout fp12ieee= {REF_COUNTER, 0, 1, 15, 32, 64, 1, 0x3ffeL};

FPLayout fp8cray=  {REF_COUNTER, 0, 1, 15, 16, 48, 1, 0x4000L};

FPLayout fp4vax=   {REF_COUNTER, 0, 1,  8,  9, 23, 0, 0x81L};
FPLayout fp8hvax=  {REF_COUNTER, 0, 1,  8,  9, 55, 0, 0x81L};
FPLayout fp8gvax=  {REF_COUNTER, 0, 1, 11, 12, 52, 0, 0x401L};

/* S&  bit address of sign
   E&  bit address of exponent
   E#  number of bits in exponent
   M&  bit address of mantissa
   M#  number of bits in mantissa
   M?  0 iff high order mantissa bit (always 1) is implicit
   E-bias   exponent bias
 */

static long knownSizes[]= {4, 8, 12, 16, 0};
static FPLayout *known4[]= {&fp4ieee, &fp4vax, 0};
static FPLayout *known8[]= {&fp8ieee, &fp8cray, &fp8hvax, &fp8gvax, 0};
static FPLayout *known12[]= {&fp12ieee, 0};
static FPLayout *known16[]= {0};
static FPLayout *knownMisc[]= {0};
static FPLayout **knownLayouts[]= {
  known4, known8, known12, known16, knownMisc};

/* ------------------------------------------------------------------------ */
/*                      BIG-ENDIAN MACHINES                                 */
/* ------------------------------------------------------------------------ */

extern DataLayout bigendLayout[6], sun3Layout[6], pdbLayout[6],
  crayLayout[6], cchybridLayout[6], mac4Layout[7], mac2Layout[7];

/* Each data type is represented by {alignment, size, order, fpLayout},
   and each machine has an array of 6 types in the order:
        char, short, int, long, float, double
   Some machines include an alternate form of double as a 7th type.
 */

/* The primitive data type formats are the same for the following
   machines:
     Sun SPARC, HP 700 series, IBM RS6000, SGI, and MIPS workstations
 */

DataLayout bigendLayout[6]= {
  {1, 1, 1, 0}, {2, 2, 1, 0}, {4, 4, 1, 0}, {4, 4, 1, 0},
  {4, 4, 1, &fp4ieee}, {8, 8, 1, &fp8ieee}
};

/* Sun-2 and Sun-3 (M68000-based) have less restrictive alignments */
DataLayout sun3Layout[6]= {
  {1, 1, 1, 0}, {2, 2, 1, 0}, {2, 4, 1, 0}, {2, 4, 1, 0},
  {2, 4, 1, &fp4ieee}, {2, 8, 1, &fp8ieee}
};

/* Default for old PDBLib corresponds to no known machine (?!) */
DataLayout pdbLayout[6]= {
  {1, 1, 1, 0}, {4, 2, 1, 0}, {4, 4, 1, 0}, {4, 4, 1, 0},
  {4, 4, 1, &fp4ieee}, {4, 8, 1, &fp8ieee}
};

/* Unicos Cray 1, XMP, YMP machines */
DataLayout crayLayout[6]= {
  {1, 1, 1, 0}, {8, 8, 1, 0}, {8, 8, 1, 0}, {8, 8, 1, 0},
  {8, 8, 1, &fp8cray}, {8, 8, 1, &fp8cray}
};

/* Defunct (?) hybrid C compiler on LTSS Crays */
DataLayout cchybridLayout[6]= {
  {8, 1, 1, 0}, {8, 8, 1, 0}, {8, 8, 1, 0}, {8, 8, 1, 0},
  {8, 8, 1, &fp8cray}, {8, 8, 1, &fp8cray}
};

/* MacIntosh has both 12 and 8 byte double formats, and
   compilers disagree about the size of an int */
DataLayout mac4Layout[7]= {
  {1, 1, 1, 0}, {2, 2, 1, 0}, {2, 4, 1, 0}, {2, 4, 1, 0},
  {2, 4, 1, &fp4ieee}, {2, 12, 1, &fp12ieee},
  {2, 8, 1, &fp8ieee}
};

/* NOTE NOTE NOTE -- the fact that the 12 byte double is listed BEFORE
                     the 8 byte double is significant in binpdb.c  */
DataLayout mac2Layout[7]= {
  {1, 1, 1, 0}, {2, 2, 1, 0}, {2, 2, 1, 0}, {2, 4, 1, 0},
  {2, 4, 1, &fp4ieee}, {2, 12, 1, &fp12ieee},
  {2, 8, 1, &fp8ieee}
};

/* ------------------------------------------------------------------------ */
/*                      LITTLE-ENDIAN MACHINES                              */
/* ------------------------------------------------------------------------ */

extern DataLayout littleLayout[6], ibmpcLayout[6], xenixLayout[6],
  vaxLayout[7];

/* DEC workstations are little-endian version of bigendLayout
   IBM PC differs only in having a less restrictive alignment,
          and the XENIX OS on PCs differs in int size from that */

DataLayout littleLayout[6]= {
  {1, 1, -1, 0}, {2, 2, -1, 0}, {4, 4, -1, 0}, {4, 4, -1, 0},
  {4, 4, -1, &fp4ieee}, {8, 8, -1, &fp8ieee}
};

DataLayout ibmpcLayout[6]= {
  {1, 1, -1, 0}, {2, 2, -1, 0}, {2, 2, -1, 0}, {2, 4, -1, 0},
  {2, 4, -1, &fp4ieee}, {2, 8, -1, &fp8ieee}
};

DataLayout xenixLayout[6]= {
  {1, 1, -1, 0}, {2, 2, -1, 0}, {2, 4, -1, 0}, {2, 4, -1, 0},
  {2, 4, -1, &fp4ieee}, {2, 8, -1, &fp8ieee}
};

/* VAX has G-format and H-format doubles, as well as
   odd byte ordering for all of its floating point formats.  */
/* WARNING --
   Did I read the lack-of-alignment in the VAX C manual correctly?
   PDBLib uses alignment of 4 for all but char...  */
DataLayout vaxLayout[7]= {
  {1, 1, -1, 0}, {1, 2, -1, 0}, {1, 4, -1, 0}, {1, 4, -1, 0},
  {1, 4, -2, &fp4vax}, {1, 8, -2, &fp8hvax},
  {1, 8, -2, &fp8gvax}
};

/* ------------------------------------------------------------------------ */
/*                      END OF DATA LAYOUTS                                 */
/* ------------------------------------------------------------------------ */

FPLayout *MakeFPLayout(FPLayout *model, long size)
{
  int i;
  FPLayout **list, *layout;

  if (!model) return 0;

  /* if this floating point layout is for this machine,
     just return reference */
  if (size==sizeof(float) &&
      SameFPLayout(fltLayout, model)) return Ref(fltLayout);
  if (size==sizeof(double) &&
      SameFPLayout(dblLayout, model)) return Ref(dblLayout);

  /* if this floating point layout is known, just return reference */
  for (i=0 ; knownSizes[i] ; i++) if (knownSizes[i]==size) break;
  list= knownLayouts[i];
  while (*list) {
    if (SameFPLayout(*list, model)) return Ref(*list);
    list++;
  }

  /* otherwise, make a fresh copy */
  layout= p_malloc(sizeof(FPLayout));
  layout->references= 0;
  layout->sgnAddr= model->sgnAddr;
  layout->expAddr= model->expAddr;
  layout->expSize= model->expSize;
  layout->manAddr= model->manAddr;
  layout->manSize= model->manSize;
  layout->manNorm= model->manNorm;
  layout->expBias= model->expBias;
  return layout;
}

int SameFPLayout(FPLayout *l, FPLayout *m)
{
  return (l->sgnAddr==m->sgnAddr && l->expAddr==m->expAddr &&
          l->expSize==m->expSize && l->manAddr==m->manAddr &&
          l->manSize==m->manSize && l->manNorm==m->manNorm &&
          l->expBias==m->expBias);
}

void FreeFPLayout(FPLayout *layout)
{
  if (layout && !layout->references--) p_free(layout);
}
