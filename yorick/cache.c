/*
 * $Id: cache.c,v 1.5 2009-05-22 04:02:26 dhmunro Exp $
 * Define caching (disk buffering) scheme used for random access
 * binary I/O.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "binio.h"
#include "pstdlib.h"
#include "pstdio.h"
#include <string.h>

/*--------------------------------------------------------------------------*/

/* Cache block addresses always rounded to 4 kbytes (BLOCK_SIZE) */
long yMaxBlockSize= 0x080000; /* 512k is default maximum cache block size */
long yCacheSize=    0x140000;  /* total of all cache blocks < 1.25 Mbytes */
int yCacheNumber= 16;    /* don't try to manage more than 16 cache blocks */

long yCacheTotal= 0;  /* sum of sizes of all existing cache blocks */
int yCacheN= 0;       /* total number of existing cache blocks */

/* YcRead and YcWrite loop on RawRead and RawWrite, respectively.  The
   raw routines grab just the portion of the request within a single cache
   block -- creating the block if necessary.  Multiple calls are
   required if and only if the request spans several existing cache
   blocks.  */
static long RawRead(IOStream *file, void *buffer, long address, long nbytes);
static long RawWrite(IOStream *file, const void *buffer,
                     long address, long nbytes);

static int ReadBlock(CacheBlock *block);   /* returns non-0 if EOF hit */
static void UseCacheBlock(CacheBlock *block);    /* makes block mostRU */
static void PruneCache(long newSize);  /* makes space for newSize, maybe
                                          deleting existing blocks */

static CacheBlock *NewBlock(IOStream *file, long addr, long last);
static void FreeCacheBlock(CacheBlock *block);
static void FlushCacheBlock(CacheBlock *block);

extern void YErrorIO(const char *msg);

/*--------------------------------------------------------------------------*/

long YcRead(IOStream *file, void *buffer, long address, long nbytes)
{
  long nio= nbytes>0? RawRead(file, buffer, address, nbytes) : 0;
  long ntotal= nio;
  while (nbytes>nio) {
    nbytes-= nio;
    address+= nio;
    nio= RawRead(file, (void *)0, address, nbytes);
    if (nio==0) break;
    ntotal+= nio;
  }
  file->seqAddress= address+nio;
  return ntotal;
}

void YcWrite(IOStream *file, const void *buffer, long address, long nbytes)
{
  long nio;
  HistoryInfo *history= file->history;

  if (history) {
    /* Enforce restrictions on writing to files with history records.  */
    if (history->fileNumber!=history->nFamily-1)
      YError("illegal to write except to last file in a record family");
    if (history->nRecords>0 && file==history->parent)
      YError("illegal to write into non-record variable after 1st record");
  }

  nio= nbytes>0? RawWrite(file, buffer, address, nbytes) : 0;
  while (nbytes>nio) {
    nbytes-= nio;
    address+= nio;
    nio= RawWrite(file, (void *)0, address, nbytes);
    if (nio==0) break;
  }
  file->seqAddress= address+nio;
}

/*--------------------------------------------------------------------------*/
/* Implement caching system for binary files.  */

/* global most and least recently used cache blocks */
CacheBlock *yMostRU, *yLeastRU;

struct CacheBlock {
  IOStream *file;    /* origin of contents of this block */
  long address;      /* byte address of 1st byte of block in file */
  long nextAddress;  /* byte immediately following this block in file */
  long validBefore;  /* byte immediately following valid data in block */
  long dirtyAfter;   /* 1st byte which has not yet been written to file */
  CacheBlock *prev, *next;  /* prev is block with lower address, next has
                               higher address -- blocks do not overlap,
                               and file->blockList has largest address */
  CacheBlock *moreRU, *lessRU;  /* links into most and least recently used
                                   list -- note that CacheBlock is
                                   quadruply linked, a personal record... */

  /* The data associated with the CacheBlock follows it in memory, with
     an offset of sizeof(MemCacheBlock), or CACHE_HEADER.  */
};

/* sizeof(MemCacheBlock) must be a multiple of the most restrictive
   alignment of any of the basic data types */
union MemCacheBlock {
  CacheBlock cb;
  char c; short s; int i; long l; float f; double d; char *q; void *p;
};

#define CACHE_HEADER sizeof(union MemCacheBlock)

/*--------------------------------------------------------------------------*/

static char *prevBuffer;
static CacheBlock *prevBlock;
static int prevEOF;

static long RawRead(IOStream *file, void *buf, long addr, long len)
{
  long last= addr+len;
  if (!(file->permissions & 1))
    YErrorIO("attempt to read from binary file opened in w or a mode");

  if (y_vopen_file(file->stream)) {
    file->ioOps->Seek(file, addr);
    return file->ioOps->Read(file, buf, sizeof(char), len);
  }

  /* similar to strtok -- buf==0 identifies a follow-up call
     when the first call did not meet the entire request */
  if (buf) {
    if (len > yMaxBlockSize-file->blockSize) {
      /* Request might require a cache block longer than yMaxBlockSize,
         so read it directly.  This simple calculation assumes that
         yMaxBlockSize is a multiple of file->blockSize+1, which will
         be true if both are powers of two, as intended.
         Note that all reads will be direct if the two are equal.  */
      FlushFile(file, 0);  /* Be sure any pending writes are done.  */
      prevEOF= 1; /* Force immediate return if called again with buf==0.  */
      file->ioOps->Seek(file, addr);
      return file->ioOps->Read(file, buf, sizeof(char), len);

    } else {
      CacheBlock *block= file->blockList;
      /* Note- By making file->blockList point to the LAST existing
         cache block, this search will immediately find the proper block
         if the reads happen to be sequential.  (Conversely, the search
         will be slowest for a series of reads in reverse order.)  */
      prevBlock= 0;
      while (block && block->nextAddress>addr) {
        prevBlock= block;
        block= block->prev;
      }
      prevBuffer= buf;
      prevEOF= 0;
    }

  } else if (prevEOF) {
    return 0;
  }

  /* prevBlock is 1st cache block with addr < prevBlock->nextAddress */

  if (!prevBlock) {
    /* addr is after all existing cache blocks, just get a new one */
    prevBlock= NewBlock(file, addr, last);

  } else if (addr<prevBlock->address) {
    /* at least part of the request precedes prevBlock */
    if (last>prevBlock->address) {
      last= prevBlock->address;
      UseCacheBlock(prevBlock);
    }
    /* If request overlaps prevBlock, it has just been made MRU and
       the call to NewBlock cannot release it -- this assures next
       pass will be able to copy from cache without reading a new
       cache block.  */
    prevBlock= NewBlock(file, addr, last);

  } else {
    /* at least part of the request is in prevBlock */
    UseCacheBlock(prevBlock);
    if (last>prevBlock->nextAddress) last= prevBlock->nextAddress;
  }

  /* prevBlock now contains addr */

  if (last>prevBlock->validBefore) {
    /* This request extends beyond the part of the block containing
       valid data.  Attempt to read the entire block.  */
    if (ReadBlock(prevBlock)) prevEOF= 1;
    if (last>prevBlock->validBefore) last= prevBlock->validBefore;
  }

  /* Move the data from the cache block into the result buffer.  */
  len= last-addr;
  if (len > 0) {
    memcpy(prevBuffer,
           &((char *)prevBlock+CACHE_HEADER)[addr-prevBlock->address],
           len);
  } else {
    len= 0;
  }

  /* Set up for a subsequent call with buf==0.  */
  prevBuffer+= len;
  prevBlock= prevBlock->next;
  return len;
}

static const char *wrtBuffer;

static long RawWrite(IOStream *file, const void *buf, long addr, long len)
{
  long last= addr+len;

  if (y_vopen_file(file->stream)) {
    file->ioOps->Seek(file, addr);
    file->ioOps->Write(file, buf, sizeof(char), len);
    if (last > file->nextAddress) file->nextAddress = last;
    return len;
  }

  /* similar to strtok -- buf==0 identifies a follow-up call
     when the first call did not meet the entire request */
  if (buf) {
    CacheBlock *block= file->blockList;
    long blockSize= file->blockSize;  /* power of 2 minus 1 */
    wrtBuffer= buf;
    if (len>blockSize+1 && len>yMaxBlockSize-blockSize) {
      /* Request might require a cache block longer than yMaxBlockSize.
         Write block fragment at beginning, followed by direct write
         of center, followed by fragment at end.  This procedure assures
         that writes always occur in multiples of the block size.  */
      long first= addr&(~blockSize);
      long final= last&(~blockSize);
      const char *bufc= buf;
      if (first<addr) {
        first= blockSize+1+first-addr;
        YcWrite(file, bufc, addr, first); /* clobbers wrtBuffer! */
        addr+= first;
        bufc+= first;
        block = file->blockList;   /* YcWrite may clobber blockList! */
      }
      /* Must release any cache blocks in this region before this write
         (or update and mark them as not dirty).  */
      while (block && block->address>=final) block= block->prev;
      while (block && block->nextAddress>addr) {
        prevBlock= block->prev;
        FreeCacheBlock(block);  /* take easy way out for now... */
        block = prevBlock;
      }
      prevBlock = 0;
      file->ioOps->Seek(file, addr);
      file->ioOps->Write(file, bufc, sizeof(char), final-addr);
      if (last>final) {
        first= final-addr;
        addr+= first;
        bufc+= first;
        YcWrite(file, bufc, addr, last-final);
      } else if (last>file->nextAddress) {
        file->nextAddress= last;
      }
      return len;

    } else {
      /* Note- By making file->blockList point to the LAST existing
         cache block, this search will immediately find the proper block
         if the writes happen to be sequential.  (Conversely, the search
         will be slowest for a series of writes in reverse order.)  */
      prevBlock= 0;
      while (block && block->nextAddress>addr) {
        prevBlock= block;
        block= block->prev;
      }
    }
  }

  /* prevBlock is 1st cache block with addr < prevBlock->nextAddress */

  if (!prevBlock) {
    /* addr is after all existing cache blocks, just get a new one */
    prevBlock= NewBlock(file, addr, last);

  } else if (addr<prevBlock->address) {
    /* at least part of the request precedes prevBlock */
    if (last>prevBlock->address) {
      last= prevBlock->address;
      UseCacheBlock(prevBlock);
    }
    /* If request overlaps prevBlock, it has just been made MRU and
       the call to NewBlock cannot release it -- this assures next
       pass will be able to copy from cache without reading a new
       cache block.  */
    prevBlock= NewBlock(file, addr, last);

  } else {
    /* at least part of the request is in prevBlock */
    UseCacheBlock(prevBlock);
    if (last>prevBlock->nextAddress) last= prevBlock->nextAddress;
  }

  /* prevBlock now contains addr */

  /* Be sure any data in the block before addr is valid.  */
  if (addr > prevBlock->validBefore) {
    ReadBlock(prevBlock);
    if (addr > prevBlock->validBefore) {
      /* When writing beyond end-of-file, zero portion of cache block
         between end-of-file and write address.  This does not guarantee
         that all bytes not explicitly referenced will be written as
         zeroes, since the end of a previous cache block might never be
         written.  It does guarantee that all bytes actually written by
         the cache package will have been initialized.  Perhaps a
         "coarseness" parameter should be added to guarantee that things
         like XDR format will work correctly?  */
      memset(&((char *)prevBlock+CACHE_HEADER)[prevBlock->validBefore-
                                               prevBlock->address],
             0, addr-prevBlock->validBefore);
      prevBlock->validBefore= addr;
    }
  }

  /* Move the data from the source buffer into the cache block.  */
  len= last-addr;
  memcpy(&((char *)prevBlock+CACHE_HEADER)[addr-prevBlock->address],
         wrtBuffer, len);
  if (last > prevBlock->validBefore) prevBlock->validBefore= last;

  /* Mark the block as dirty, but if its last word is being written,
     go ahead and flush it.  */
  if (addr < prevBlock->dirtyAfter) prevBlock->dirtyAfter= addr;
  if (last==prevBlock->nextAddress) FlushCacheBlock(prevBlock);

  /* Set up for a subsequent call with buf==0.  */
  wrtBuffer+= len;
  prevBlock= prevBlock->next;
  if (last>file->nextAddress) file->nextAddress= last;
  return len;
}

/*--------------------------------------------------------------------------*/

/* Be sure pending writes are done, optionally discarding all cache
   blocks associated with file.  */
void FlushFile(IOStream *file, int discardCache)
{
  if (discardCache) {
    while (file->blockList) FreeCacheBlock(file->blockList);
  } else {
    CacheBlock *block= file->blockList;
    while (block) {
      FlushCacheBlock(block);
      block= block->prev;
    }
  }
  p_fflush((p_file *)file->stream);
  if (file->contentsLog) {
    HistoryInfo *history= file->history;
    if (history) {
      if (file==history->parent) {
        if (history->nRecords<=0) CLupdate(file);
      } else {
        CLupdate(file);
      }
    } else if (file->dataTable.nItems) {
      CLupdate(file);
    }
  }
}

int YCopyFile(IOStream *dst, IOStream *src)
{
  long address;
  CacheBlock *srcBlock;
  long last= src->nextAddress;
  long blockSize= src->blockSize>dst->blockSize? dst->blockSize :
                                                 src->blockSize;
  long chunkSize= 16*(blockSize+1);
  if (chunkSize >= yMaxBlockSize-blockSize) {
    chunkSize= yMaxBlockSize-blockSize-1;
    chunkSize&= blockSize;
  }
  if (chunkSize > last) chunkSize= last;

  /* discard all cache blocks for these files */
  FlushFile(dst, 1);
  FlushFile(src, 1);

  address= 0;
  while (address < last) {
    if (address+chunkSize > last) chunkSize= last-address;
    srcBlock= NewBlock(src, address, address+chunkSize);
    ReadBlock(srcBlock);
    if (srcBlock->validBefore<address+chunkSize) {
      chunkSize= srcBlock->validBefore-address;
      last= address+chunkSize;
    }
    if (chunkSize>0) {
      dst->ioOps->Seek(dst, address);
      dst->ioOps->Write(dst,
                        &((char *)srcBlock+CACHE_HEADER)[address-
                                                         srcBlock->address],
                        sizeof(char), chunkSize);
    }
    FreeCacheBlock(srcBlock);
    address+= chunkSize;
  }

  if (dst->nextAddress<last) dst->nextAddress= last;
  dst->seqAddress= src->seqAddress= last;

  return last<src->nextAddress;
}

/*--------------------------------------------------------------------------*/

static int ReadBlock(CacheBlock *block)
{
  IOStream *file= block->file;
  long address= block->validBefore;
  long length= block->nextAddress - address;
  long nio= address-block->address;
  file->ioOps->Seek(file, address);
  nio= file->ioOps->Read(file, (char *)block+CACHE_HEADER+nio,
                         sizeof(char), length);
  block->validBefore= address+nio;
  return nio<length;    /* returns non-0 on EOF, else 0 */
}

static void FlushCacheBlock(CacheBlock *block)
{
  IOStream *file;
  long address= block->dirtyAfter;
  long length= block->validBefore - address;
  if (length<=0) return;
  file= block->file;
  file->ioOps->Seek(file, address);
  file->ioOps->Write(file, (char *)block+CACHE_HEADER+address-block->address,
                     sizeof(char), length);
  block->dirtyAfter= block->nextAddress;
}

static void UseCacheBlock(CacheBlock *block)
{
  CacheBlock *moreRU= block->moreRU;
  if (moreRU) {
    /* This is not already the most recently used block, so relink list.  */
    block->moreRU= 0;            /* there are no more recently used */
    if (!block->lessRU) yLeastRU= moreRU;      /* no longer the LRU */
    else block->lessRU->moreRU= moreRU;
    moreRU->lessRU= block->lessRU;  /* close up "hole" where it was */
    block->lessRU= yMostRU;                   /* old MRU now second */
    yMostRU->moreRU= block;
    yMostRU= block;
  }
}

static CacheBlock *NewBlock(IOStream *file, long addr, long last)
{
  CacheBlock *block, *next, *prev;
  long mask= ~file->blockSize;  /* blockSize must be a power of 2 minus 1 */

  /* round to multiple of file->blockSize+1 and get new cache block */
  addr&= mask;
  last= (file->blockSize+1) + ((last-1)&mask);
  PruneCache(last-addr);
  block= p_malloc(CACHE_HEADER+last-addr);

  /* fill in file and address range */
  block->file= file;
  block->address= addr;
  block->nextAddress= last;
  block->validBefore= addr;  /* no data yet valid */
  block->dirtyAfter= last;   /* no writes yet pending */

  /* link into most/least recently used list */
  block->prev= block->next= 0;  /* temporary protection */
  block->moreRU= 0;
  block->lessRU= yMostRU;
  if (yMostRU) yMostRU->moreRU= block;
  yMostRU= block;
  if (!yLeastRU) yLeastRU= block;

  /* link into file's blockList */
  prev= file->blockList;
  next= 0;
  while (prev && prev->nextAddress>last) {
    next= prev;
    prev= prev->prev;
  }
  if ((block->prev= prev)) prev->next= block;
  if ((block->next= next)) next->prev= block;
  else file->blockList= block;

  /* increment total space and total number of cache blocks */
  yCacheTotal+= last-addr;
  yCacheN++;

  return block;
}

static void FreeCacheBlock(CacheBlock *block)
{
  IOStream *file;
  if (!block) return;
  file= block->file;

  FlushCacheBlock(block);

  /* unlink from global most/least rectently used list */
  if (block->lessRU) block->lessRU->moreRU= block->moreRU;
  if (block->moreRU) block->moreRU->lessRU= block->lessRU;
  if (block==yMostRU) yMostRU= block->lessRU;
  if (block==yLeastRU) yLeastRU= block->moreRU;
  block->lessRU= block->moreRU= 0;
  yCacheTotal-= block->nextAddress-block->address;
  yCacheN--;

  /* unlink from file blockList */
  if (block->next) block->next->prev= block->prev;
  else if (file) file->blockList= block->prev;
  if (block->prev) block->prev->next= block->next;

  /* probably impossible, but best to be safe */
  if (block == prevBlock) prevBlock = 0;

  p_free(block);
}

static void PruneCache(long newSize)
{
  long total= yCacheTotal+newSize;
  int n= yCacheN+1;
  CacheBlock *lessRU, *block= 0;
  long size;

  /* Starting from the least recently used, work forward until enough
     cumulative space would be freed.  */
  while (n>yCacheNumber || total>yCacheSize) {
    if (!block) block= yLeastRU;
    else block= block->moreRU;
    if (!block) {
      yCacheTotal= 0;
      yCacheN= 0;
      return;
    }
    n--;
    total-= block->nextAddress-block->address;
  }

  if (!block) return;
  if (block==yMostRU) {
    block= block->lessRU;
    if (!block) return;
  }

  size= block->nextAddress-block->address;
  for (;;) {
    lessRU= block->lessRU;
    FreeCacheBlock(block);
    if (!lessRU) return;
    block= lessRU;
    size= block->nextAddress-block->address;
    while (n+1<=yCacheNumber && total+size<=yCacheSize) {
      n++;
      total+= size;
      block= block->lessRU;
      if (!block) return;
      size= block->nextAddress-block->address;
    }
  }
}

/*--------------------------------------------------------------------------*/
