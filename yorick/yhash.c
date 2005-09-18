/*
 * $Id: yhash.c,v 1.1 2005-09-18 22:04:09 dhmunro Exp $
 * Define interface to hash routines
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "hash.h"
#include "defmem.h"
#include "pstdlib.h"
#include <string.h>

long hashIndex;   /* set by HashFind, HashAdd, etc. */

int hashRealloc;  /* set by HashAdd if it returned 0 and if
                     the call forced table->maxItems to grow */

static long Hash(const char *name, long nSlots);
static long HashN(const char *name, long n, long nSlots);
static void ZapItems(HashItem **items, long nSlots);
static void HashReverse(HashItem **items, long nSlots);
static void Rehash(HashTable *table);
static HashItem *NewHashItem(HashItem *next, long index);
static void FreeHashItem(HashItem *item);
static long SlotsRequired(long nItems, long *nSlots);

static void XRehash(HashXTable *table, long n,
                    void *(*Index)(void *keyTable, long index),
                    void *keyTable);

/* ------------------------------------------------------------------------ */

int HashFind(const HashTable *table, const char *name, long n)
{
  long nSlots= table->nSlots;
  char **names= table->names;
  HashItem *item;

  if (nSlots<=0) return 0;

  if (n>0) {
    char *namei;

    hashIndex= HashN(name, n, nSlots);

    for (item=table->items[hashIndex] ; item ; item=item->next) {
      namei= names[item->index];
      if (strncmp(namei, name, n)==0 && !namei[n]) break;
    }

  } else {
    hashIndex= Hash(name, nSlots);

    for (item=table->items[hashIndex] ; item ; item=item->next)
      if (strcmp(names[item->index], name)==0) break;
  }

  if (!item) return 0;
  hashIndex= item->index;
  return 1;
}

int HashAdd(HashTable *table, const char *name, long n)
{
  if (HashFind(table, name, n)) {
    return 1;

  } else {
    long nItems= table->nItems;
    HashItem **item;

    if (table->maxItems <= nItems) {
      Rehash(table);
      hashIndex= n>0? HashN(name, n, table->nSlots) :
                      Hash(name, table->nSlots);
      hashRealloc= 1;
    } else {
      hashRealloc= 0;
    }

    item= &table->items[hashIndex];
    *item= NewHashItem(*item, nItems);
    table->names[nItems]= n>0? p_strncat(0, name, n) : p_strcpy(name);
    table->nItems= nItems+1;

    hashIndex= nItems;
    return 0;
  }
}

/* ------------------------------------------------------------------------ */

int HashXFind(const HashXTable *table, const void *key, long n,
              int (*Compare)(void *keyTable, long index, const void *key),
              void *keyTable)
{
  long nSlots= table->nSlots;
  HashItem *item;

  if (nSlots<=0) return 0;

  hashIndex= HashN(key, n, nSlots);

  for (item=table->items[hashIndex] ; item ; item=item->next)
    if (Compare(keyTable, item->index, key)==0) break;

  if (!item) return 0;
  hashIndex= item->index;
  return 1;
}

int HashXAdd(HashXTable *table, const char *key, long n,
             int (*Compare)(void *keyTable, long index, const void *key),
             void *(*Index)(void *keyTable, long index),
             void *keyTable)
{
  if (HashXFind(table, key, n, Compare, keyTable)) {
    return 1;

  } else {
    long nItems= table->nItems;
    HashItem **item;

    if (table->maxItems <= nItems) {
      XRehash(table, n, Index, keyTable);
      hashIndex= HashN(key, n, table->nSlots);
      hashRealloc= 1;
    } else {
      hashRealloc= 0;
    }

    item= &table->items[hashIndex];
    *item= NewHashItem(*item, nItems);
    table->nItems= nItems+1;

    hashIndex= nItems;
    return 0;
  }
}

/* ------------------------------------------------------------------------ */

/* hashSlots  - HashTable->nSlots will always be one of these numbers,
                which are primes nearest powers of 2
   hashCutoff - when HashTable->maxItems reaches these values, the
                HashTable will be rehashed to the corresponding hashSlots.
                The numbers are chosen to keep the average number of items
                per slot between about 3/4 and 3/2.  This keeps the expected
                fraction of empty slots between about 1/2 and 1/4, or the
                average number of items per *occupied* slot between
                about 3/2 and 2.  The expected worst case (largest number
                of items in a slot) is less than about log(nSlots), which
                means that the worst case HashTable lookup time should
                be comparable to the average lookup time for a binary
                search. */
static long hashSlots[]= {
  3,  7, 17, 31, 67, 127, 257, 521, 1031, 2053, 4099,  8191, 16381, 32771,
  65537, 131071, 262147, 524287, 1048573, 2097143, 4194301,  8388617,
  16777213, 33554467,  67108859, 134217757, 268435459, 536870909, 1073741827};
static long hashCutoff[]= {
  5, 12, 24, 49, 97, 192, 389, 776, 1542, 3076, 6145, 12287, 24573, 49158,
  98307, 196608, 393222, 786432, 1572861, 3145716, 6291453, 12582927,
  25165821, 50331702, 100663290, 201326637, 402653190, 805306365, 1610612742};
#define N_SIZES 29

/* ------------------------------------------------------------------------ */

void HashInit(HashTable *table, int estSize)
{
  int i;
  HashItem **items;
  long n;
  for (i=0 ; i<N_SIZES-1 ; i++) if (hashCutoff[i]>=estSize) break;
  table->maxItems= hashCutoff[i];
  table->nSlots= hashSlots[i];
  table->nItems= 0;
  table->items= items= p_malloc(sizeof(HashItem *)*hashSlots[i]);
  table->names= p_malloc(sizeof(char *)*hashCutoff[i]);
  n= hashSlots[i];
  while (n--) (*items++)= 0;
}

void HashClear(HashTable *table)
{
  long nItems= table->nItems;
  char **names= table->names;
  HashItem **items= table->items;
  long nSlots= table->nSlots;
  long i;
  char *name;

  table->maxItems= 0;
  table->nSlots= 0;
  table->items= 0;

  ZapItems(items, nSlots);

  for (i=0 ; i<nItems ; i++) {
    name= names[i];
    names[i]= 0;
    p_free(name);
  }
  table->nItems= 0;
  table->names= 0;
  p_free(names);
}

void HashXClear(HashXTable *table)
{
  HashItem **items= table->items;
  long nSlots= table->nSlots;

  table->maxItems= 0;
  table->nSlots= 0;
  table->items= 0;

  ZapItems(items, nSlots);
  table->nItems= 0;
}

void HashShrink(HashTable *table)
{
  long nItems= table->nItems;
  if (nItems<=0) {
    HashClear(table);
  } else {
    long maxItems= table->maxItems;
    if (maxItems!=nItems) {
      Rehash(table);
      if (table->maxItems!=nItems) {
        table->maxItems= nItems;
        table->names= p_realloc(table->names, sizeof(char *)*nItems);
        hashRealloc= 1;
      } else {
        hashRealloc= 0;
      }
    }
  }
}

void HashRemove(HashTable *table, long index0)
{
  char **names= table->names;
  char *name0= names[index0];
  long nItems= table->nItems;
  long nSlots= table->nSlots;
  HashItem **items= table->items;
  long i= Hash(name0, nSlots);
  HashItem *item= items[i];
  HashItem *prev= 0;

  /* free the name */
  names[index0]= 0;
  p_free(name0);

  /* unlink HashItem from the list in its slot and free it */
  while (item->index!=index0) { prev= item; item= item->next; }
  if (prev) prev->next= item->next;
  else items[i]= item->next;
  FreeHashItem(item);

  /* adjust nItems */
  table->nItems--;

  /* scan through all hash items to renumber those above index0 */
  if (index0<(nItems-1)/2) {
    /* if index0 is small, faster to go through the whole list */
    for (i=0 ; i<nSlots ; i++) {
      for (item=table->items[i] ; item ; item=item->next)
        if (item->index>index0) item->index--;
    }
    for (i=index0+1 ; i<nItems ; i++) names[i-1]= names[i];

  } else {
    /* if index0 is close to nItems, faster to use hash lookup */
    for (i=index0+1 ; i<nItems ; i++) {
      item= items[Hash(names[i], nSlots)];
      while (item->index!=i) item= item->next;
      item->index--;
      names[i-1]= names[i];
    }
  }
}

void HashSwap(HashTable *table, long index0, long index1)
{
  char **names= table->names;
  char *name0= names[index0];  /* both index0 and index1 had better... */
  char *name1= names[index1];  /* ...be between 0 and nItems-1 inclusive */
  long nSlots= table->nSlots;
  HashItem **items= table->items;

  HashItem *item= items[Hash(name0, nSlots)];
  while (item->index!=index0) item= item->next;
  item->index= index1;
  names[index0]= name1;

  item= items[Hash(name1, nSlots)];
  while (item->index!=index1) item= item->next;
  item->index= index0;
  names[index1]= name0;
}

/* ------------------------------------------------------------------------ */

static long Hash(const char *name, long nSlots)
{
  register unsigned char c;
  long hash= 0;
  while ((c= *name++))
    hash= ((hash<<1)&0x7fffffffL) ^ c;  /* long is at least 4 bytes */
  return hash%nSlots;
}

static long HashN(const char *name, long n, long nSlots)
{
  long hash= 0;
  while (n--)
    hash= ((hash<<1)&0x7fffffffL) ^ ((unsigned char)(*name++));
  return hash%nSlots;
}

/* ------------------------------------------------------------------------ */

static HashItem **itemList= 0;   /* failsafe list being zapped */
static long itemSlots= 0;        /* nSlots for itemList */

static void ZapItems(HashItem **items, long nSlots)
{
  HashItem *item;

  /* failsafe is really for Rehash, but must be done here */
  if (itemList && itemList!=items) ZapItems(itemList, itemSlots);
  if (!items) return;

  itemList= items;
  itemSlots= nSlots;

  while (nSlots--) {
    for (item=*items ; item ; item=*items) {
      *items= item->next;
      FreeHashItem(item);
    }
    items++;
  }

  items= itemList;
  itemList= 0;
  p_free(items);
  itemSlots= 0;
}

static void HashReverse(HashItem **items, long nSlots)
{
  HashItem *item, *prev, *next;
  while (nSlots--) {
    prev= 0;
    for (item=*items ; item ; item=next) {
      next= item->next;
      item->next= prev;
      prev= item;
    }
    *(items++)= prev;
  }
}

static void Rehash(HashTable *table)
{
  long nItems= table->nItems;
  char **names= table->names;
  long nSlots= table->nSlots;
  HashItem **items= table->items;
  long newSlots, maxItems= SlotsRequired(nItems, &newSlots);

  if (newSlots!=nSlots) {
    /* a change in number of slots means that the table must
       be rehashed -- otherwise, only the names list needs to be
       reallocated */
    long i, old, niw;
    HashItem *item;

    table->maxItems= 0;  /* will force rehash if interrupted */
    table->nSlots= 0;
    table->items= 0;
    itemList= items;     /* failsafe storage */
    itemSlots= nSlots;
    items= p_malloc(sizeof(HashItem *)*newSlots);
    for (i=0 ; i<newSlots ; i++) items[i]= 0;
    table->items= items;
    table->nSlots= newSlots;

    HashReverse(itemList, nSlots);  /* reverse order of old items */

    for (i=0 ; i<nItems ; i++) {
      old= Hash(names[i], nSlots);
      niw= Hash(names[i], newSlots);
      item= itemList[old];  /* this must have item->index==i */
      itemList[old]= item->next;
      if (!item || item->index!=i) {
        /* attempt to repair partial table */
        item= NewHashItem(items[niw], i);
      } else {
        /* just move correct item from old table into niw */
        item->next= items[niw];
      }
      items[niw]= item;
    }

    items= itemList;
    itemList= 0;
    ZapItems(items, nSlots);
  }

  table->names= p_realloc(names, sizeof(char *)*maxItems);
  table->maxItems= maxItems;
}

static void XRehash(HashXTable *table, long n,
                    void *(*Index)(void *keyTable, long index),
                    void *keyTable)
{
  long nItems= table->nItems;
  long nSlots= table->nSlots;
  HashItem **items= table->items;
  long newSlots, maxItems= SlotsRequired(nItems, &newSlots);

  if (newSlots!=nSlots) {
    /* a change in number of slots means that the table must
       be rehashed */
    long i, old, niw;
    HashItem *item;
    void *key;

    table->maxItems= 0;  /* will force rehash if interrupted */
    table->nSlots= 0;
    table->items= 0;
    itemList= items;     /* failsafe storage */
    itemSlots= nSlots;
    items= p_malloc(sizeof(HashItem *)*newSlots);
    for (i=0 ; i<newSlots ; i++) items[i]= 0;
    table->items= items;
    table->nSlots= newSlots;
    table->maxItems= maxItems;

    HashReverse(itemList, nSlots);  /* reverse order of old items */

    for (i=0 ; i<nItems ; i++) {
      key= Index(keyTable, i);
      old= HashN(key, n, nSlots);
      niw= HashN(key, n, newSlots);
      item= itemList[old];  /* this must have item->index==i */
      itemList[old]= item->next;
      if (!item || item->index!=i) {
        /* attempt to repair partial table */
        item= NewHashItem(items[niw], i);
      } else {
        /* just move correct item from old table into niw */
        item->next= items[niw];
      }
      items[niw]= item;
    }

    items= itemList;
    itemList= 0;
    ZapItems(items, nSlots);
  } else {
    table->maxItems= maxItems;
  }
}

/* ------------------------------------------------------------------------ */

/* Set up a block allocator for HashItems */
union HashBlock {
  HashItem item;
  void *dummy;  /* forces sizeof(HashBlock) to multiple of sizeof(void*) */
};
static MemryBlock hashBlock= {0, 0, sizeof(union HashBlock),
                                 256*sizeof(union HashBlock)};

static HashItem *NewHashItem(HashItem *next, long index)
{
  HashItem *item= NextUnit(&hashBlock);
  item->next= next;
  item->index= index;
  return item;
}

static void FreeHashItem(HashItem *item)
{
  if (!item) return;
  FreeUnit(&hashBlock, item);
}

/* ------------------------------------------------------------------------ */

/* Slots required returns the number of slots required for a HashTable
   to hold nItems.  The function returns maxItems>=nItems, and nSlots
   is set to the required number of slots.  When the table has grown
   to maxItems, it is time to expand, and SlotsRequired should be
   called again.  */
static long SlotsRequired(long nItems, long *nSlots)
{
  int i;
  long maxItems;

  for (i=0 ; i<N_SIZES ; i++) if (nItems<hashCutoff[i]) break;

  if (i<N_SIZES) {
    maxItems= hashCutoff[i];
    *nSlots= hashSlots[i];

  } else {
    /* should never happen */
    maxItems= 2*nItems;
    *nSlots= hashCutoff[N_SIZES-1];
  }

  return maxItems;
}

/* ------------------------------------------------------------------------ */
