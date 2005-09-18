/*
 * $Id: defmem.c,v 1.1 2005-09-18 22:04:00 dhmunro Exp $
 * Implement functions for memory management.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "defmem.h"
#include "pstdlib.h"

extern long y_net_blocks;

long y_net_blocks= 0;

static void *NextBlock(MemryBlock *block)
{
  long unitSize= block->unitSize;
  long n= block->blockSize;
  char *newUnit= p_malloc(n+unitSize);  /* extra unit for block list */
  void *newBlock;

  /* Add to blockList (1st unit of each block reserved for this). */
  *((void **)newUnit)= block->blockList;
  block->blockList= newUnit;

  /* Usable units begin with 2nd in block, this will be returned. */
  newUnit+= unitSize;
  newBlock= newUnit;

  /* Free list begins with 3rd unit in block and continues to end.
     Initialize the block as a chain of free units. */
  n-= unitSize;
  newUnit+= unitSize;
  block->freeList= newUnit;
  while ((n-=unitSize)>0) {
    /* smallest legal n=2*unitSize barely fails to reach this point */
    *((void **)newUnit)= newUnit+unitSize;
    newUnit+= unitSize;
  }
  *((void **)newUnit)= 0;

  return newBlock;
}

void *NextUnit(MemryBlock *block)
{
  void *nextFree= block->freeList;
  y_net_blocks++;
  if (nextFree) {
    /* (void**)<->(void*) typecast in next line is equivalent to
       list->next if list is a struct List {struct List *next;}... */
    block->freeList= *(void **)nextFree;
    return nextFree;
  } else {
    return NextBlock(block);
  }
}

void FreeUnit(MemryBlock *block, void *ptr)
{
  *((void**)ptr)= block->freeList;
  block->freeList= ptr;
  y_net_blocks--;
}

void FreeAllUnits(MemryBlock *block)
{
  void *blockList, *next= block->blockList;
  while ((blockList=next)) {
    next= *(void**)blockList;
    p_free(blockList);
  }
}
