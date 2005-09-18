/*
 * $Id: hash.h,v 1.1 2005-09-18 22:04:07 dhmunro Exp $
 * Declare interface to hash routines
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef HASH_H
#define HASH_H

#include "plugin.h"

/* ------------------------------------------------------------------------ */

typedef struct HashTable HashTable;

PLUG_API long hashIndex;  /* set by HashFind, HashAdd, etc. */

PLUG_API int HashFind(const HashTable *table, const char *name, long n);
/* Note-- n is the exact length of name, or n==0 to use strlen(name)
   Usage:
     if (HashFind(table, name, n)) {
       ... name exists in table, and was the hashIndex +1st item
           added to the table ...
     } else {
       ... name does not exist, and hashIndex is the value of the
           hashing function (a slot index) ...
     }
 */

PLUG_API int HashAdd(HashTable *table, const char *name, long n);
/* Note-- n is the exact length of name, or n==0 to use strlen(name)
   Usage:
     if (HashAdd(table, name, n)) {
       ... name existed previously in table, as the hashIndex +1st item
           added to the table ...
     } else {
       ... name did not exist previously, and is now the hashIndex +1st
           item added to the table ...
       ... use the HASH_MANAGE macro to manage any lists associated with
           this hash table, in case they need to be lengthened ...
     }
   Note well: The name is copied if added to the table, so the caller
              should dispose of it as appropriate.
 */

/* ------------------------------------------------------------------------ */

PLUG_API int hashRealloc;  /* set by HashAdd if it returned 0 and if
                              the call forced table->maxItems to grow */

#define HASH_MANAGE(table, type, list) \
  if (hashRealloc) (list)= p_realloc((list), (table).maxItems*sizeof(type))
/* NOTE: If you use this, you need to include defmem.h or otherwise
         declare Yrealloc (same semantics as ANSI standard realloc).
   Usage:
     HashTable table;   == associates name with index into table.names ==
     Object *list;      == typically, one or more lists in addition to
                           table.names are maintained representing the
                           objects which bear the names ==
     ...
     if (HashAdd(&table, name, n)) {
       == often, it is an error to add a duplicate name ==
     } else {
       HASH_MANAGE(table, Object, list);  == lengthen list if reqd ==
       list[hashIndex]= value_of_name;
     }
 */

/* ------------------------------------------------------------------------ */

PLUG_API void HashInit(HashTable *table, int estSize);
/* Pass HashInit the estimated final nItems for the table to bypass
   the rehashing which otherwise takes place every with every factor
   of two size increase.  A HashTable with all its members 0 is also
   a legal initialization; this is the situation after HashClear.  */

PLUG_API void HashClear(HashTable *table);
/* Clear a hash table, freeing all associated memory.  */

PLUG_API void HashShrink(HashTable *table);
/* Shrink maxItems to nItems, and rehash if the number of slots
   can be reduced.  Follow this with the HASH_MANAGE macro if the
   table has associated lists.  */

PLUG_API void HashRemove(HashTable *table, long index0);
/* Removing an item from a hash table is a very expensive operation,
   unless it is one of the few elements most recently added to the
   table.  The index0 must have been returned as the hashIndex for a
   previous call to HashFind or HashAdd.  */

PLUG_API void HashSwap(HashTable *table, long index0, long index1);
/* Swaps the item->index and names corresponding to the two
   entries, to reorder the names list after an incorrectly ordered
   sequence of calls to HashAdd.  */

/* ------------------------------------------------------------------------ */

typedef struct HashItem HashItem;

struct HashTable {
  long nItems, maxItems;  /* used and actual length of names array */
  char **names;
  long nSlots;            /* length of items, or number of hash slots */
  HashItem **items;
};

struct HashItem {
  HashItem *next;  /* next item in this hash slot */
  long index;      /* index into names array in associated HashTable */
};

/* ------------------------------------------------------------------------ */

typedef struct HashXTable HashXTable;
struct HashXTable {
  long nItems, maxItems;  /* used and actual length of names array */
  long nSlots;            /* length of items, or number of hash slots */
  HashItem **items;
};

PLUG_API int HashXFind(const HashXTable *table, const void *key, long n,
                       int (*Compare)(void *keyTable,
                                      long index, const void *key),
                       void *keyTable);
/* Notes-- 1. n>0 is the exact length of key (uses HashN)
           2. Compare(keyTable, index, key) should return 0 when
              the key matches the index-th entry of the keyTable
   Usage:
     if (HashXFind(table, key, n, Compare, keyTable)) {
       ... key exists in (table,keyTable), and was the hashIndex +1st
           item added to the table ...
     } else {
       ... key does not exist, and hashIndex is the value of the
           hashing function (a slot index) ...
     }
 */

PLUG_API int HashXAdd(HashXTable *table, const char *key, long n,
                      int (*Compare)(void *keyTable,
                                     long index, const void *key),
                      void *(*Index)(void *keyTable, long index),
                      void *keyTable);
/* Notes-- 1. n>0 is the exact length of key (uses HashN)
           2. Compare(keyTable, index, key) should return 0 when
              the key matches the index-th entry of the keyTable
           3. Index(keyTable, index) should return the key for
              the index-th entry of the keyTable (required for rehash)
   Usage:
     HashXTable *table;
     KeyedObject *keyTable;
     ...
     if (HashXAdd(table, name, n, Compare, Index, keyTable)) {
       ... key existed previously in table, as the hashIndex +1st item
           added to the table ...
     } else {
       ... key did not exist previously, and is now the hashIndex +1st
           item added to the table ...
       HASH_MANAGE(*table, KeyedObject, keyTable);
       keyTable[hashIndex]= object_corresponding_to_key;
       ... use the HASH_MANAGE macro to manage other lists associated with
           this hash table, in case they need to be lengthened ...
     }
 */

PLUG_API void HashXClear(HashXTable *table);
/* Clear a hash table, freeing all associated memory.  */

/* ------------------------------------------------------------------------ */

#endif
