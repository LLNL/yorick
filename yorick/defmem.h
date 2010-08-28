/*
 * $Id: defmem.h,v 1.2 2010-08-28 23:26:10 dhmunro Exp $
 * Declare structures and functions for memory management.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
    Ymalloc, Yfree, Yrealloc declared, plus a generic block allocator
    package.

    The generic block allocator provides a much faster, less fragmenting
    alternative to malloc/free for cases in which many small units
    of memory are to be allocated and freed.  The strategy is to
    allocated a large block of units and maintain a list of free units;
    when the free list becomes empty, a new large block is allocated.
    Each unit must have alignment at least as strict as void*, and arrays
    of units are not supported.  (The surest way to set the proper unit
    size is to use sizeof(union{your unit; void *dummy}).)

    To use the package, you would generally declare a static MemryBlock,
    initialized with 0 pointers and the unitSize and blockSize you desire.
    The unitSize may not change once a block has been allocated, but a
    change in blockSize is benign and will take effect the next time a
    large block is allocated.
 */

#ifndef DEFMEM_H
#define DEFMEM_H

#include "plugin.h"

/* FreeUnit conflicts with Fortran I/O function on some platforms */
#define FreeUnit y_FreeUnit

PLUG2_API void YError(const char *msg);   /* also declared in ydata.h */

typedef struct MemryBlock MemryBlock;
struct MemryBlock {
  void *freeList;   /* allocate new block when reaches 0 */
  void *blockList;  /* list of blocks kept using first unit of each block */
  long unitSize;    /* byte size of units, multiple of sizeof(void**) */
  long blockSize;   /* multiple of unit size, at least 2*unitSize */
};

PLUG_API void *NextUnit(MemryBlock *block);       /* ptr= malloc(unitSize) */
PLUG_API void FreeUnit(MemryBlock *block, void *ptr);         /* free(ptr) */
PLUG_API void FreeAllUnits(MemryBlock *block);

#endif
