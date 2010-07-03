/*
 * $Id: binio.h,v 1.6 2010-07-03 19:42:31 dhmunro Exp $
 * Declare structures and functions for arrays of data that have
 * meaningful interpretations on disk.
 *
 * Yorick supports the following data types:
 *
 *     char   short   int   long   float   double
 *     string   pointer   complex   struct(instance)
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifndef BINIO_H
#define BINIO_H

#include "hash.h"

/*--------------------------------------------------------------------------*/

/* IOStream is a complicated data structure which completely represents
   a binary I/O stream.  The data in the file(s) can be decribed using
   the Contents Log (CLOG) language.  Major components of the IOStream
   are:
      1. A table of data primitive and compound data types (structTable).
      2. A table of variables (dataTable), which associate a variable
         name with its data type, dimensions, and disk address.
      3. An optional block of history information, detailing the
         disk addresses (and files, if this is a family of files) of
         the history records, if there are any.
      4. An optional list of pointees, associating memory and disk
         addresses of indirectly referenced data (not explicitly
         in the dataTable).
      5. An optional file containing the (plain text) CLOG description
         of this file.
      6. The list of cache blocks associated with this file, and the
         last address read or written (since ftell doesn't know about
         the cache buffers).
 */
typedef struct IOStream IOStream;

/* StructDef is the definition of a data structure, explicating the
   meaning of data either on disk or in memory, and its relationship to
   other data types.  All structure definitions representing disk data
   are "owned" by the IOStream associated with the disk file.  Each
   disk structure has a "model" data structure representing the
   memory version of the same data.  A conversion procedure converts
   from a representation to its model or vice versa -- a feature
   designed to handle disk data written in a non-native format.
   (A structure representing data in memory may also have a "model"
   and a corresponding conversion routine to handle automatic memory-to-
   memory type conversions.)
   A StructDef may represent either a primitive data type, or a compound
   data type consisting of dimensioned members of simpler data types.
   All data structures have an ASCII name, which is used to make
   default correspondences between in-memory and on-disk structures.  */
typedef struct StructDef StructDef;

/* Array is a block of data with a specific data type (specified by
   a StructDef) and dimensions.  All objects of type pointer must be
   either 0, or point to the first address of an Array value.
   An Array represents scalar data if its dimension list is nil.  */
typedef struct Array Array;

/* Dimension is an element of a linked list of dimensions.  A Dimension
   list is always from slowest to fastest varying dimension.  Each
   dimension has an origin (suggested minimum index value) and length.  */
typedef struct Dimension Dimension;
struct Dimension {
  Dimension *next;   /* list goes from slowest to fastest varying */
  long number;       /* length of this dimension */
  long origin;       /* index of first element along this dimension */
  int references;    /* reference counter */
};

/* Member is a data type and dimension list.  The dataTable of an IOStream,
   the member list of a StructDef, and an Array all consist in part of
   one or more Member instances.  */
typedef struct Member Member;
struct Member { /* most general type of x:  base_type x[][]...[][]; */
  StructDef *base;   /* array base data type */
  Dimension *dims;   /* array dimension list (may be nil) */
  long number;       /* of items in array (product of dimensions) */
};

/* Strider is an element of a linked list of strides through data.  Each
   Strider consists of a stride (in bytes), a number of "footprints",
   and an optional Array of type long containing an index list.  */
typedef struct Strider Strider;
struct Strider {
  Strider *next;        /* logically next faster varying block */
  long stride;          /* stride length in bytes */
  long number;          /* number of strides to take + 1 (# footprints!) */
  Array *indexList;     /* if !=0, offset list (units of 'stride' bytes) */
};

/* Operations is a virtual function table for a DataBlock, the
   container class for Array, StructDef, and IOStream (and others).  */
typedef struct Operations Operations;
/* The Operations* members depend on the details of Yorick, and are
   therefore split out to allow this binary file package to be used
   in non-Yorick based codes.  These three values are explicitly
   needed:  */
PLUG_API Operations *yOpsStruct, *yOpsStream, *yOpsStructDef;

/* DataLayout is a complete description of a primitive data type,
   including byte size, alignment, byte order, and floating point layout
   (if the type represents a real number).  */
typedef struct DataLayout DataLayout;

/* FPLayout is a description of the format of a floating point number,
   detailing the bit addresses and number of bits in the exponent and
   mantissa.  The StructDefs of an IOStream corresponding to the float
   and double primitive types must be describable by an FPLayout.  */
typedef struct FPLayout FPLayout;

/* IOOperations is an additional virtual function table for an IOStream.  */
typedef struct IOOperations IOOperations;

/* HistoryInfo keeps history record information for an IOStream.  */
typedef struct HistoryInfo HistoryInfo;

/* PointeeList keeps an association of disk and memory addresses for an
   IOStream.  This allows multiply referenced pointers to be written
   only once.  More subtlely, with a PointeeList, the algorithm to
   write an arbitrarily complex tree of pointers requires a bounded
   amount of stack space.  */
typedef struct PointeeList PointeeList;
typedef struct MDinfo MDinfo;  /* for PointeeList */
struct PointeeList {
  int writing;         /* 0 if table keyed to disk address,
                          1 if keyed to memory address */
  long nValid;         /* number of valid objects (i.e.- where both
                          disk data address and m pointer contain
                          valid data) */
  HashXTable table;    /* generic hash table for address correspondence */
  MDinfo *mdInfo;      /* memory-disk correspondence list */
};

/* CLbuffer describes the state of the binary data description language
   file associated with an IOStream.  Defined in clog.c.  */
typedef struct CLbuffer CLbuffer;

/* CacheBlock is required by IOStream, but is opaque here.  See cache.c.  */
typedef struct CacheBlock CacheBlock;

/*--------------------------------------------------------------------------*/

struct IOStream {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  void *stream;        /* usually the FILE*, but opaque here --
                          0 indicates file has been closed */
  char *fullname;      /* filename after YExpandName */
  int permissions;     /* +1 read permission, +2 write permission
                          +4 append mode, +8 binary mode,
                          +16 not seekable, +32 pipe, +64 clog mode
                       */

  IOOperations *ioOps; /* virtual functions to read and write file */

  long blockSize;         /* MUST be a power of 2 minus 1 (default 0x3fff)
                             all cache blocks will be multiples of
                             blockSize+1 bytes */
  CacheBlock *blockList;  /* list of cache blocks for this file,
                             stored highest to lowest address */
  long seqAddress;        /* address immediately after last address
                             actually read or written using YcRead or
                             YcWrite -- different from ftell(stream)
                             because of caching */

  int structAlign;        /* byte offset of any struct in another struct
                             must be multiple of this (the alignment of
                             some structs may be more strict than this) */
  int dataAlign;          /* 0  -- align variables to addresses as if
                                   the file were a giant struct instance
                             >0 -- byte offset of any variable in file
                                   must be a multiple of this
                             WARNING WARNING WARNING WARNING WARNING
                             This MUST be 0 in order to correctly write
                             a PDB file with history records.  */

  HashTable structTable;  /* table of StructDef names for this file */
  StructDef **structList; /* corresponding to structTable->names */

  HashTable dataTable;    /* table of variable names for this file */
  Member *types;          /* corresponding types and dimensions */
  long *addresses;        /* corresponding addresses */
  long nextAddress;       /* next free address (may be end of pointee) */
  long offset;            /* to be applied to all addresses */

  HistoryInfo *history;   /* non-0 if there is a history record structure
                             -- If so, then no changes to the dataTable
                             (which represents static variables) are
                             permitted.
                             Note that this IOStream may be either
                             history->parent or history->child.  */

  CLbuffer *contentsLog;  /* if non-0, contains "CLOG" textual equivalent
                             of structTable, dataTable, and history */

  PointeeList pointeeList;  /* list of memory<->disk address correspondence
                               to aid in pointee reads or writes */
  void (*CloseHook)(IOStream *file);  /* if non-0, called before close */
};

/* check to see if this is in-memory file created by vopen
 * return value is the Array* or 0, stream is p_file*
 */
PLUG_API void *y_vopen_file(void *stream);

/*--------------------------------------------------------------------------*/

/* A Copier copies data of type base from t to s.  If t==s, the Copier
   must appropriately zero s, releasing pointees if appropriate.  If
   t==s, non-pointered data should be left unchanged (the Copier is a
   no-op).  There are only 4 possible Copiers-- CopyX should be used
   for StructDefs which contain no pointers, CopyS for StructDefs
   whose members include pointers or strings.  CopyQ is for stringStruct
   and CopyP is for pointerStruct.  */
typedef void Copier(StructDef *base, void *dst, const void *src, long n);
PLUG_API Copier CopyX, CopyQ, CopyP, CopyS;

/* A Converter converts n objects to or from type base, which will often
   be a foreign disk file data type.  The conversion is from baseData to
   modelData if toBase is 0, from modelData to baseData if toBase
   is non-0.  */
typedef void Converter(StructDef *base, void *baseData, void *modelData,
                       long n, int toBase);
PLUG_API Converter ConvertI, ConvertF, ConvertQ, ConvertP, ConvertS;

struct StructDef {
  int references;     /* reference counter */
  Operations *ops;    /* virtual function table */
  Operations *dataOps;  /* virtual functions for an instance */

  long size;            /* size of an instance of this data structure */
  int alignment;        /* byte address must be multiple of this */

  HashTable table;      /* rapid name->index and index->name associations */
  Member *members;      /* members[table->maxEntries] is Member array */
  long *offsets;        /* byte offsets corresponding to members */

  Copier *Copy;         /* copy/initialize/delete operation */

  IOStream *file;       /* non-zero if the data resides on disk
                           if file!=0, then model!=0 */
  long index;           /* if file!=0, index in file->structList,
                           else        index in yStructList */

  int addressType;      /* 0 -> object is in memory (file==0), and address
                                is void*
                           1 -> object is on disk (file!=0) and address
                                is long
                           2 -> address cannot be meaningfully incremented
                                to find members or array elements  */

  /* The remaining elements are to implement "transparent translation"
     of this data type into another -- usually to change from disk
     format to memory format.  */
  StructDef *model;     /* memory equivalent of disk StructDef
                           is NEVER 0 when file!=0,
                           also destination or source type for Convert */
  Converter *Convert;
         /* Convert may be 0 if no conversion is required.
            The order and fpLayout members can be used as a context
            by Convert.  In particular, fpLayout need not actually point
            to an FPLayout, as long as the first member of the struct
            it does point to is a reference counter.  */
  int order;           /* 1 MSB first, -1 LSB first,
                          -w for LSW first, MSB first within word size w
                          w for MSW first, LSB first within word size w */
  FPLayout *fpLayout;  /* if non-0, used by Export/Import to translate
                          data format */
};

/* Nine in-memory data structures are predefined, corresponding with
   the primitive data types in the C programming language:  */
PLUG_API StructDef charStruct;
PLUG_API StructDef shortStruct;
PLUG_API StructDef intStruct;
PLUG_API StructDef longStruct;
PLUG_API StructDef floatStruct;
PLUG_API StructDef doubleStruct;
PLUG_API StructDef stringStruct;   /* char *    always 0-terminated */
PLUG_API StructDef pointerStruct;  /* void *    all pointers generic */
PLUG_API StructDef complexStruct;  /* struct complex { double re, im; } */
/* The indirect types string and pointer are for restricted use only,
   in the sense that special memory management routines must be used
   to allocate and free the associated memory.
   An IOStream structTable always begins with the on-disk analogues
   of char, short, int, long, float, double, string, and pointer.  */

/*--------------------------------------------------------------------------*/

/* Every on-disk StructDef in the structList for an IOStream has a
   counterpart "model" StructDef representing the same data translated
   into memory format.  As for IOStreams, the first 8 StructDefs in
   yStructList are always, in order, char, short, int, long, float,
   double, string, pointer.  The 9th in yStructList is complex.  */

PLUG_API HashTable yStructTable;  /* table of StructDef names for memory */
PLUG_API StructDef **yStructList; /* corresponding to yStructTable->names */

/*--------------------------------------------------------------------------*/

/* possibly should do this at configuration time... */
#if defined(__i386__) || defined(_WIN32)
# define PAD_ARRAY
#endif

struct Array {
  int references;   /* reference counter */
  Operations *ops;  /* virtual function table */
  Member type;      /* full data type, i.e.-    base_type x[]...[] */
#ifdef PAD_ARRAY
  /* on a pentium, 4-byte aligned doubles are legal, but access is
   * far slower than to 8-byte aligned doubles
   * since a Member is 12 bytes long, need to add 4 more here
   * -- no other platform at this time needs this hack */
  int pad;
#endif
  union {
    /* Appropriate member is selected by ops; there are only 10 possible ops,
       corresponding to char, short, int, long, float, double, complex,
       string, pointer, and struct instance.
       Actual array length is determined by type. */
    char c[1];
    short s[1];
    int i[1];
    long l[1];
    float f[1];
    double d[2]; /* also complex */
    char *q[1];  /* string */
    void *p[1];
  } value;
};

/*--------------------------------------------------------------------------*/

/* Each StructDef has an alignment, computed as the alignment of its
   most restrictively aligned member.  Some compilers/machines impose
   an additional restriction on all structs, regardless of the most
   restrictive member, in which case yStructAlign is set to that
   additional struct alignment restriction.

   IBM has a crazy special rule for PowerPC architecture, where
   if the first member of a struct is a double, the struct alignment
   is 8 instead of 4 (which is otherwise the most restrictive alignment).
   However, IBM's own C compiler misapplies this rule so that only
   the size of the struct is padded up to a multiple of 8 while its
   alignment remains 4.  This is yStructAlign=-1.
   On PowerPCs, gcc attempts to follow IBMs written rule, and they
   interpret a struct whose first member is an array of double to
   *not* get the extra alignment.  This is yStructAlign=-2.
   See play/any/numfmt.c for more details.

   It is also possible for a C compiler to require different alignment
   for an array than for a scalar.  Yorick cannot handle this.
   Finally, the (hopefully defunct?) hybrid C compiler under the
   (definitely defunct) NLTSS Cray operating system aligned a scalar
   char to an address with remainder 7 modulo 8, which is hopeless.  */
PLUG_API int yStructAlign;

struct DataLayout {
  int alignment;
  long size;
  int order;
  FPLayout *fpLayout;
};

/* Foreign formats for floating point data can be handled, provided they
   fit within the parametrization of the FPLayout struct.
   Foreign integer formats are described by a simple int (the order
   member of a StructDef above).  */
struct FPLayout {
  /* Addresses are in bits with 0 128-bit of the most significant byte
     of the object, 8 the 128-bit of the 2nd most significant byte,
     and so on until 8*size-1 is the 1-bit of the least significant
     byte.  The actual storage order of the bytes is given by the
     order member of the DataLayout.
     sgnAddr            - bit address of overall sign
     expAddr, expSize   - bit address and size of exponent
     manAddr, manSize   - bit address and size of mantissa
     manNorm            - if non-zero, address of leading 1 in mantissa
     expBias            - exponent bias
   */
  int references;      /* reference counter */
  int sgnAddr, expAddr, expSize, manAddr, manSize, manNorm;
  long expBias;
};

/* Floating point layouts for floats and doubles on this machine */
PLUG_API FPLayout *fltLayout, *dblLayout;

/*--------------------------------------------------------------------------*/

struct IOOperations {
  /* An IOStream requires the virtual functions Read, Write, Tell,
     Seek, and SeekEnd, which are similar to the C standard library
     routines fread, fwrite, ftell, fseek(,,SEEK_SET), and fseek(,,SEEK_END).
     All 5 routines must be provided, but they may simply call YError if
     the operation makes no sense.  The Read function should return the
     number (of objects of size size) actually read if EOF occured before
     the read was complete; n on success.
     In all cases, YError should be called if an error is detected,
     AFTER the error is cleared, on the presumption that the next
     operation will be valid.  */
  long (*Read)(IOStream *file, void *buf, long size, long n);
  void (*Write)(IOStream *file, const void *buf, long size, long n);
  long (*Tell)(IOStream *file, long offset);
  void (*Seek)(IOStream *file, long offset);
  void (*SeekEnd)(IOStream *file, long offset);
  void (*Close)(IOStream *file);
};

struct HistoryInfo {
  IOStream *parent;  /* original file always used for static data */
  IOStream *child;   /* separate IOStream used for history record traffic
                        so that child->members[i].base->file has a place
                        to point which may represent a different file in
                        a history family starting from the original parent */

  int nFamily;       /* number of files in this family */
  char **famNames;   /* used for child->fullname,
                        famNames[0]==parent->fullname
                        allocated length is 4*(1 + (nFamily-1)/4) */
  int fileNumber;    /* 0 to nFamily-1, specifies index of current child */
  long fileSize;     /* approximate maximum size for a file in family */
#define DEFAULT_FILE_SIZE 0x800000
  int copyParent;    /* non-0 if parent (static variables) is to be copied
                        when a new file is added to this family */

  long recordSize;   /* length of one record in bytes (not including
                        any strings, pointees, or other indirections) */
  int recordAlign;   /* alignment for records -- either child->dataAlign,
                        or most restrictive alignment in child->structList
                        when 1st record was declared
                        Unlike a StructDef, recordSize is not necessarily
                        a multiple of recordAlign.  */
  long nRecords;     /* number of history records of this type */
  long recNumber;    /* index into ifile and offset
                        for record represented by child (-1 if none) */

  /* allocated length of following arrays is 16*(1 + (nRecs-1)/16) */
  int *ifile;        /* list of indices into famNames */
  long *offset;      /* list of record byte addresses */
  double *time;      /* 0 or list of floating point values of records */
  long *ncyc;        /* 0 or list of integer values of records */
};

struct MDinfo {
  void *m;       /* memory address, always an Array->value.c; the
                    MDinfo owns a reference of the Array */
  long d;        /* disk address of the pointee header */
  long data;     /* disk address of the pointee data */
  StructDef *base;  /* disk StructDef for this data -- this pointer does
                       NOT own a reference to base */
};

/*--------------------------------------------------------------------------*/

/* fullname is NOT copied and FreeIOStream will do p_free(ios->fullname) */
PLUG_API IOStream *NewIOStream(char *fullname, void *stream, int permissions);
PLUG_API void FreeIOStream(void *ios);      /* *** Use Unref(ios) *** */

PLUG_API StructDef *NewStructDef(IOStream *file, long index);
PLUG_API void FreeStructDef(void *base);      /* *** Use Unref(base) *** */

PLUG_API Array *NewArray(StructDef *base, Dimension *dims);
PLUG_API void FreeArray(void *array);        /* *** Use Unref(array) *** */

/* NewTmpArray/ClearTmpArray may be used to get one or two temporary
   arrays.  Call ClearTmpArray first, then NewTmpArray several times,
   then ClearTmpArray.  At the third call to NewTmpArray, the Array
   returned by the first call is Unref'ed; at the fourth call to
   NewTmpArray, the second is Unref'ed; and so on.  */
PLUG_API Array *NewTmpArray(StructDef *base, Dimension *dims);
PLUG_API void ClearTmpArray(void);

/* NewDimension does NOT do Ref(next) */
/* WARNING: NewArray(base, NewDimension(...)) is a memory leak!
 * need to use ynew_dim() below instead in this context
 */
PLUG_API int yForceOrigin; /* non-zero to ignore stated origin in Dimension
                            lists, and always assume 1-origin indices */
PLUG_API Dimension *NewDimension(long number, long origin, Dimension *next);
PLUG_API void FreeDimension(Dimension *dims);

PLUG_API Strider *NewStrider(long stride, long number);
PLUG_API void FreeStrider(Strider *strider);

/*--------------------------------------------------------------------------*/

#ifdef NOT_YORICK
/* This is the minimal Operations -- a virtual delete function.
   Without this, the Unref macro is unusable.  */
struct Operations {
  void (*Free)(void *);  /* crucial member for Unref -- first in struct
                            to allow alternate Operations to be used */
};
#endif

#define Ref(db) ((db)?++(db)->references:0 , (db))
#define RefNC(db) (++(db)->references , (db))
#define Unref(db) {if ((db) && --(db)->references<0) (db)->ops->Free(db);}
#define UnrefNC(db) {if (--(db)->references<0) (db)->ops->Free(db);}

PLUG_API long TotalNumber(const Dimension *dims);
PLUG_API int CountDims(const Dimension *dims);
/* Note-- like NewDimension, CopyDims does NOT do Ref(next) */
PLUG_API Dimension *CopyDims(Dimension *dims, Dimension *next, int copyOrigin);

/* CopyStrider also does NOT do Ref(next) */
PLUG_API Strider *CopyStrider(Strider *strider, Strider *next);

/* tmpDims can be used for "protected" scratch space to build dimension
 * lists.  Before using tmpDims, always call FreeDimension(tmpDims).
 * ynew_dim() sets tmpDims to the result of NewDimension(), so
 * it can be used as argument to functions like NewArray which
 * increment the use counter
 */
PLUG_API Dimension *tmpDims;
PLUG_API Dimension *ynew_dim(long n, Dimension *next);

/* Given a pointer to data (memory representation of a pointerStruct),
   return the associated Array.  */
PLUG_API void *Pointee(void *pointer);

/* StructEqual tests for two structures which are exactly the same.
   EquivStruct tests whether the file StructDef belonging to an IOStream
   is equivalent to the mem StructDef belonging to yStructTable.  */
PLUG_API int StructEqual(StructDef *l, StructDef *r);
PLUG_API int EquivStruct(StructDef *fil, StructDef *mem);

/* Adjust the address to a multiple of the alignment (no-op if <2).  */
PLUG_API long AlignAdjust(long address, int alignment);

/* StructName works for disk or memory based StructDefs,
   CopyStruct returns the StructDef in file->structTable equivalent
               to the given base, creating it if necessary */
PLUG_API char *StructName(StructDef *base);
PLUG_API StructDef *CopyStruct(IOStream *file, StructDef *base);

/*--------------------------------------------------------------------------*/

/* AddStruct adds an empty data structure to a file structTable,
   returning the new StructDef.  Returns 0 on error.  If the name
   is a duplicate, returns 0 to signal error, but the names "string"
   and "pointer" are excepted from this rule, provided that the
   associated StructDef has not been referenced.
   If file==0, the yStructTable is assumed.
   If n is non-0, it must be the exact length of name --
   if n is 0, name is presumed 0-terminated.
   If file->history!=0, the struct is added to the history child.
   This is illegal after the second history record has begun.  */
PLUG_API StructDef *AddStruct(IOStream *file, const char *name, long n);

/* AddMember increments references to memType, dims -- offset
   is computed if input offset<0.  Returns 0 on success, 1 if name is a
   duplicate, 2 if memType or a sub-structure has a type name conflict
   in the file which owns base.
   (If return non-0 memType, dims references NOT incremented).
   In any case, hashIndex set to index of member in base.
   In the case of a duplicate name, memType and dims are NOT
   checked for consistency with the existing member.
   *******
   WARNING-- if offset is specified, base->alignment cannot be updated,
             and the proper alignment must be set by hand  */
PLUG_API int AddMember(StructDef *base, long offset, const char *name,
                     StructDef *memType, Dimension *dims);

/* DefinePrimitive is called in lieu of AddMember to define a primitive
   data type after AddStruct has been called.  If model==0, then must
   have Convert==0, and DefinePrimitive will try to match the given
   size, order, and fpLayout to one of char, short, long, float, or
   double (never int) as best it can.  If model is one of the primitive
   types (char, short, int, long, float, double, string, or pointer),
   and size==0, DefinePrimitive will also set Convert appropriately.
   If order==0 (opaque primitive), a model must be supplied.
   If non-zero, fpLayout is copied.
   Returns 0 on success,
           1 on attempt to redefine char, short, int, long, float, or double
             to a model other than the default one
           2 on attempt to alias another type to string or pointer without
             providing an explicit Converter
           3 if an opaque type model size differs and no Converter supplied,
           4 if Converter supplied without a model
           5 if no model supplied for opaque primitive */
PLUG_API int DefinePrimitive(StructDef *base, long size, int alignment,
                           int addressType, int order, FPLayout *fpLayout,
                           StructDef *model, Converter *Convert);

/* InstallStruct finishes a structure defintion begun with AddStruct
   and AddMember calls.  If base->file==0, base is added to yStructTable
   incrementing base->references.
   If base->file, then an equivalent in-memory model is found for
   base->model: First, yStructTable table is checked for an
   in-memory equivalent StructDef of the same name as base.  Failing
   this, if base is a primitive with non-zero base->order or base->size==1,
   it is matched to the "closest" of the primitive data types char, short,
   long, float, or double (never int).  Failing this, an in-memory model
   is constructed and installed on the spot.
   In all cases, base->dataOps is set to "validate" the StructDef.
   If model is non-zero, it is assumed to be the correct model for base.  */
PLUG_API void InstallStruct(StructDef *base, StructDef *model);

/* If DefInstallHook is non-0, InstallStruct calls it after
   installing base in yStructTable if base->file!=0 */
PLUG_API void (*DefInstallHook)(const char *name, StructDef *base);

/* AddVariable increments references to varType, dims -- address
   is computed if input address<0.  Returns 0 on success, 1 if name is
   a duplicate, and 2 if the name of varType or one of its sub-structures
   already belongs to a non-equivalent data structure (in either non-0
   case varType, dims references NOT incremented).
   For returns 0 or 1, hashIndex set to index of name in file->dataTable,
   or file->history->child->dataTable, if file->history!=0.
   In the case of a duplicate name, varType and dims are NOT
   checked for consistency with the existing variable.
   If file->history!=0, the variable is added to the history child
   (i.e.- it becomes a record variable).  This is illegal after the
   second history record has begun.  */
PLUG_API int AddVariable(IOStream *file, long address, const char *name,
                       StructDef *varType, Dimension *dims);

/* AddHistory adds a history child to an existing IOStream, returning the
   file->history if successful.  Size is the approximate maximum size of
   one file in the history family, or 0 for the default (4 Megabytes).  */
PLUG_API HistoryInfo *AddHistory(IOStream *file, long size);

/* AddRecord adds a record to the HistoryInfo, returning 0 on success.
   flags is +1 if time exists, +2 if ncyc exists
   The record is assumed to be in the current history child at the
   specified address, or at the next available address if address<0.  */
PLUG_API int AddRecord(HistoryInfo *history,
                     int flags, double time, long ncyc, long address);

/* AddNextFile adds a new file to the HistoryInfo, returning 0 on
   success.  If the fullname is 0, the NextFileName function is used.
   If the create flag is non-0, the file is created (possibly copying
   the parent data), otherwise the file is expected to exist.  */
PLUG_API int AddNextFile(HistoryInfo *history, char *filename, int create);

/* Return value must be freed with p_free.
   Algorithm:
   (1) Strip directory (if any) from name.
   (2) If final character is a digit (0-9), it becomes increment character,
       else the character before the final dot (.) is the increment
            character (or the final character if there is no dot).
   (3) If the increment character is not A-Z, a-z, or 0-9, scan backwards
       through the name until a character in this range is found.
   (4) Loop:
         If the increment character is before the beginning of name,
            or if it is not in the range A-Z, a-z, or 0-9, insert the
            character "A" and break out of this loop.
         If it is 9 or z or Z, it becomes 0,
         else increment it and break out of this loop.
         Back up so the increment character becomes the previous character.
 */
PLUG_API char *NextFileName(const char *name);

/* JumpToTime (JumpToCycle) jumps to the nearest time (ncyc) in the
   current record sequence to the specified time (ncyc).  Returns 0
   on success, non-0 on failure.  */
PLUG_API int JumpToTime(HistoryInfo *history, double time);
PLUG_API int JumpToCycle(HistoryInfo *history, long ncyc);
/* JumpRecord returns 0 on success, 1 on recNumber out of range.
   If recNumber is out of range, there is no current record.  */
PLUG_API int JumpRecord(HistoryInfo *history, long recNumber);

/*--------------------------------------------------------------------------*/

/* Parameters for adjusting caching system -- see cache.c */
PLUG_API long yMaxBlockSize, yCacheSize, yCacheTotal, y_block_size_0;
PLUG_API int yCacheNumber, yCacheN;

/* copy src from 0 to src->nextAddress to dst, return 0 on success */
PLUG_API int YCopyFile(IOStream *dst, IOStream *src);

/* FlushFile flushes all cache blocks associated with file, optionally
   discarding them altogether.  */
PLUG_API void FlushFile(IOStream *file, int discardCache);

/* YcRead and YcWrite are the lowest level read and write primitives for
   moving data to or from an IOStream.  The addr and len are in bytes.
   All data goes through the cache buffer system defined in cache.c.  */
PLUG_API long YcRead(IOStream *file, void *buf, long addr, long len);
PLUG_API void YcWrite(IOStream *file, const void *buf, long addr, long len);

/* YRead and YWrite are the high level interface to an IOStream.
   The base->file MUST be non-zero.  In both cases, the void* is an
   in-memory buffer holding (or big enough to hold) number objects of size
   base->model->...->size.  If strider is non-0, number is ignored and
   the disk data referenced will be described by the strider.  (Use the
   Scatter and Gather routines from bcast.h to apply a Strider to arrays
   in memory.)
   These routines perform any data type conversions necessary to move
   foreign formats, strings, or pointers to and from disk.  */
PLUG_API void YWrite(void *src, long dst, StructDef *base, long number,
                   const Strider *strider);
PLUG_API void YRead(void *dst, long src, StructDef *base, long number,
                  const Strider *strider);

/*--------------------------------------------------------------------------*/

/* CLupdate ensures that file->contentsLog exists and is up to date.  */
PLUG_API void CLupdate(IOStream *file);

/* CLopen checks to see if file has a Clog description appended to it,
   returning 0 if it does, in which case the file is updated.  */
PLUG_API int CLopen(IOStream *file, int familyOK);

/* CLclose is suitable for use as a CloseHook -- appends Clog description
   to end of file and zaps file->contentsLog.  */
PLUG_API void CLclose(IOStream *file);

/* ZapClogFile frees file->contentsLog, and destroys the associated file.  */
PLUG_API void ZapClogFile(IOStream *file);

/* FreeClogFile frees file->contentsLog.  */
PLUG_API void FreeClogFile(IOStream *file);

/* DumpClogFile creates a Contents Log named clogName, describing file.
   This is completely independent of the file->contentsLog, if any.  */
PLUG_API int DumpClogFile(IOStream *file, const char *clogName);

/* ReadClogFile reads a Contents Log named clogName and makes file
   look like the description therein.  (CLopen with a separate file.)
   This is completely independent of the file->contentsLog, if any.  */
PLUG_API int ReadClogFile(IOStream *file, const char *clogName);

/* functions to manage file->pointeeList */
PLUG_API void ReadPointees(IOStream *file);
PLUG_API void WritePointees(IOStream *file);
PLUG_API void ClearPointees(IOStream *file, int writing);

/* functions to manage FPLayouts */
PLUG_API FPLayout *MakeFPLayout(FPLayout *model, long size);
PLUG_API void FreeFPLayout(FPLayout *layout);
PLUG_API int SameFPLayout(FPLayout *l, FPLayout *m);

/* ------------------------------------------------------------------------ */

/* YError and YWarning must be supplied by user of binary file package.
   YError should longjump or exit, YWarning may return.  */
PLUG_API void YWarning(const char *msg);
PLUG_API void YError(const char *msg);

/* ------------------------------------------------------------------------ */

#endif
