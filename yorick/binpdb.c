/*
 * $Id: binpdb.c,v 1.5 2010-07-03 19:42:31 dhmunro Exp $
 * Define Yorick interface to PDB files
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#ifdef NOT_YORICK
#include "binio.h"
#else
/* Only problem is that RemoveStruct uses Unref... */
#include "ydata.h"
#endif
#include "yio.h"
#include "pstdlib.h"
#include <string.h>
#include <time.h>

/* Uses: strtok, strtol, strcspn, strspn, strcmp, strncmp, strchr, strlen */
extern char *Ytimestamp(void);  /* 24 character return */

extern DataLayout bigendLayout[6], sun3Layout[6], cchybridLayout[6],
  ibmpcLayout[6], mac2Layout[7], vaxLayout[7], pdbLayout[6];

extern Converter PDBconvert;

extern int yPDBopen;
int yPDBopen= 010;  /* option bits to alter PDB behavior on open:

                       if either of bits 001 or 002 is set, when a
                       PDB file is opened, its Major-Order: extra
                       may be altered as follows:
                       001  Major-Order:102 always
                       002  Major-Order:  opposite from what file says
                       003  Major-Order:101 always

                       004  Strip Basis @... suffices from variable names
                       010  Use Basis @history convention on input
                       */

static int yPDBclose;  /* options for closing a PDB file, selected by
                          which close hook (YPDBcloser) is selected as the
                          CloseHook:
                          001  Write Major-Order 102 PDB file
                          002  Write PDB style history data
                               The following are no-ops unless bit 002 is set:
                          004  Use Basis @history convention on output
                          010  Do NOT pack all history record variables into
                               a single structure instance.
                          */

/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/

/* The PDB file format was invented by Stewart Brown of Lawrence
 * Livermore Laboratory.  This interface to the PDB format is entirely
 * my own (David Munro); Stewart Brown's PDBlib API is a part of his PACT
 * library, which you can obtain from http://pact.llnl.gov.
 */

/*--------------------------------------------------------------------------*/

/* Here are the interface routines for opening an existing PDB file
   (possibly with DMIPDB-style history records), and initializing a newly
   created file.  */
extern int YtestPDB(IOStream *file, int familyOK);
extern int YinitPDB(IOStream *file, int close102);

extern void YPDBpointers(IOStream *file, long size, int align);

extern void (*YPDBcloser[16])(IOStream *file);

/* Data type names must be altered slightly:
   Name in PDB file             Name in IOStream
   ----------------             ----------------
   integer                      int
   int                          integer
   "any *"                      "char*"    (for variables -- size==0)
                                "char *"   (for members -- size>0)
 */

extern void YErrorIO(const char *msg);

/*--------------------------------------------------------------------------*/

/* required by PDBconvert to handle pointee headers */
extern char *PDBvarType(char *typeName);

extern void CopyLayouts(IOStream *file, DataLayout *src);

static int DecodePrimitives(IOStream *file,
                            unsigned char *buffer, char *token);
static void SetConverter(StructDef *base, int isReal);
static int DecodeOrder(int *order, unsigned char *permute, long n);
static void DecodeMachine(IOStream *file, long machine);
static void RDextras(IOStream *file, long address);
static void CheckOrderScratch(long size);
static int RDchart(IOStream *file, long address);
static char *FlopIntegerAndInt(char *typeName);
static int RDsymtab(IOStream *file, long address);
static int GrabSymbol(char *dataName, IOStream *file, int goofCount,
                      long offset, char **context);
static void CheckDMIScratch(long maxLength);
static void DMIhistory(IOStream *file, long addrChrt, long address,
                       int familyOK);
static void RemoveStruct(IOStream *file, char *baseName);
static int FindSymtab(IOStream *file, long *addrSymt, long *addrChrt);
static int FindRecords(IOStream *file, long addrSymt,
                       long *addrTimes, long *addrCycles, int style);
static int BurstRecord(IOStream *file, StructDef *base);
static void GrabDMIData(IOStream *file, long firstAddr, long firstLength,
                        long numbAddr);
static int DMIcheckSymbol(char *dataName);
static int HasDMIrecords(void);
static int IsDMIvariable(char *dataName);
static long IsDMIrecord(char *dataName);
static char *FindAddresses(IOStream *file, long addrSymt, char *baseName,
                           int style);
static int VerifyRecType(IOStream *file, long addrChrt,
                         char *baseName, long baseSize);

static char *EncodePrimitives(IOStream *file, char *buffer);
static long EncodeOrder(unsigned char *buf, int order, long size);
static int WRchart(IOStream *file);
static int WRstructure(char **names, char *name, long size, long nMembers,
                       char **mNames, Member *members, long *offsets);
static void PutDimList(Dimension *dims, int fastest1st);
static void Put1Dim(Dimension *dims, int firstPass);
static int DoStructName(char *name, long size, int putEnd);
static int WRsymtab(IOStream *file);
static int WRsymbol(char *name, char *typeName, long number, long address,
                    Dimension *dims, long nlen);
static void Put102Dims(Dimension *dims);
static int WRextras(IOStream *file);
static int WRheader(IOStream *file, long newChart, long newSymtab);
static int WRhistory(HistoryInfo *history);

static int fastestFirst;    /* flag set if fastest dimension first */
static long defaultOrigin;  /* default minimum array index */
static int gotAlignment;    /* flag set if Alignment: extra exists */

static char *memberPointer= "char *";
static char *variablePointer= "char*";

static long mptrSize;
static int mptrAlign;

static int has_dirs;

/* state information for DMI-style record structure in PDB file */
static long firstLength, nSeq, nSeq2, nTimes, nCycles;
static long firstAddr, numbAddr, timeAddr, ncycAddr;

/* PDB header token to uniquely identify as PDB file */
#define HEADTOK "!<<PDB:II>>!"
#define OLDHEADTOK "!<><PDB><>!"

/* PDB recognizes any of 3 possible types of newline character */
#define ANY_NEWLINE "\012\015\037"
#define SOH_OR_NEWLINE "\001\012\015\037"

/* Error returns:
   1  -> PDB header not found (either old or new)
   2  -> unable to read primitive formats
   3  -> unable to read structure defintions
   4  -> unable to read symbol table
   5  -> unable to read extras tags
 */

/* InitBuffer initializes a the state information and allocates a buffer
              - first read will be at specified address
   ClearBuffer deallocates the buffer
   ReadUntil scans until the next occurance of any of the delimiters
             (but at most buffer size characters), replaces it with
             a '\0', and returns the resulting string
             \0 bytes in the file do NOT terminate the scan
   ReadBlock advances the reader nBytes bytes for the next ReadBlock or
             ReadUntil (nBytes must not be larger than buffer size)
   CheckBuffer checks whether there are the required number of bytes
             remaining in the buffer -- returns non-0 iff buffer too small,
             otherwise, may write the buffer and reset nextByte
   nextByte is a pointer into the next word of the buffer which will be
             read by ReadUntil or ReadBlock, or which should be written to
             after CheckBuffer
   ReadBuffer is an internal routine -- don't call it
   WriteBuffer writes the buffer to disk
   NextWRAddress returns next address to be written in buffer
 */
static void InitBuffer(IOStream *file, long size, long address);
static int ClearBuffer(int value);
static char *ReadUntil(char *delimiters);
static char *ReadNonblank(char *delimiters);
static char *ReadBlock(long nBytes);
static int CheckBuffer(long nRequired);
static char *nextByte;
static int ReadBuffer(void);
static void WriteBuffer(void);
static long NextWRAddress(int afterRead);

/* Stuff to handle new PDB history files and Basis deviations.  */
static HashTable blocksTable;
static long nBlocks, *blocksAddrs;
static void BlocksInit(void);
static int BlocksVerify(void);
static int BlocksLine(char *line);

static int TokenAsLong(char *token, long *result);

/* returns 0 on success, 1 if header indicates not a PDB,
   2 if the file's primitive data types were modified, but the routine
     was unable to read the structure chart and/or symbol table */
int YtestPDB(IOStream *file, int familyOK)
{
  char *token;
  long chartAddress, symtabAddress;
  int i;

  if (!(file->permissions&1)) return 1;

  InitBuffer(file, 280L, 0L);

/*   The total number of bytes in a PDB header is less than:

        43 + sizeof(float) + sizeof(double) +
        2*(number of decimal digits in float or double exponent bias) +
        2*(number of decimal digits in the size of the file in bytes)

     A 10 Gigabyte file with 12-byte floating point values with an
     decimal exponent range of plus or minus 32767 would therefore
     require no more than 97 bytes of PDB header information.
 */

  /* The first dozen bytes of the file should be HEADTOK or OLDHEADTOK */
  token= ReadUntil(ANY_NEWLINE);
  if (!token || !(token= strtok(token, " "))) return ClearBuffer(1);

  if (strcmp(token, HEADTOK) == 0) {
    /* a PDB_SYSTEM_VERSION 2 or later style file */

    /* primitive data type format information begins at next byte
       -- get everything except exponent biases and alignments
       next byte is a byte count which determines the location of the
       ASCII exponent biases -- the intervening bytes contain the
       rest of the primitive data type format information */
    if (!(token= ReadBlock(1L)) ||
        !(token= ReadBlock(token[0]-1L)) ||
        DecodePrimitives(file, (unsigned char *)token,
                         ReadUntil(ANY_NEWLINE))) return ClearBuffer(2);

  } else if (strcmp(token, OLDHEADTOK) == 0) {
    /* a pre-PDB_SYSTEM_VERSION 2 style file */

    /* the second token is the machine type that wrote the file
     * set the file->std for machine type
     * for PD_open the file->std is always the PDBfile standard
     * alignment issues are not properly handled before PDB_SYSTEM_VERSION 3
     * but do the best that we can
     */
    token = strtok(0, " ");
    if (!token) return ClearBuffer(2);

    DecodeMachine(file, strtol(token, (char **)0, 10));

  } else {
    return ClearBuffer(1);  /* bad header */
  }

  /* chart and symtab addresses are stored with "%ld\001%ld\001\n" */
  token= ReadUntil(ANY_NEWLINE);
  if (!token) return ClearBuffer(2);
  if (TokenAsLong(token, &chartAddress)) return ClearBuffer(2);
  if (TokenAsLong(0, &symtabAddress)) return ClearBuffer(2);

  /* An IOStream is born with 8 primitive data types--
     add memberPointer and variablePointer now.  */
  mptrAlign= mptrSize;  /* will be modified when Alignment: read */
  YPDBpointers(file, mptrSize, mptrAlign);

  /* The extras section must be read first, since it contains alignments,
     primitive data types, and dimension list order required in order
     to read both the chart and the symtab.  This is inefficient, since
     the symtab must be scanned to get to the extras, but so be it.
     The variables fastestFirst, gotAlignment, and defaultOrigin are set.
     The Alignment:, Primitive-Types:, Major-Order:, and Offset: extras
     alter the meaning of the chart and symtab.

     While scanning through the symbol table, RDextras notes the presence
     or absence of the DMI-style special record variables FirstRecordNames,
     NumberOfRecords, times, and ncycs.  The first pass through the symbol
     table thereby allows RDsymtab to function more efficiently, since
     the record-related variables can be correctly added to the history
     child, rather than being moved in a later pass.

     RDExtras also builds the blocksTable if the file contains PDB-style
     history data.  If present, and if DMI-style history data is not
     present, RDsymtab uses this blocksTable to identify history data.  */
  BlocksInit();
  yPDBclose= 06;   /* initialize file style here */
  has_dirs = 0;
  RDextras(file, symtabAddress);
  if (yPDBopen&3) {
    if ((yPDBopen&3)==1) fastestFirst= 1;
    else if ((yPDBopen&3)==3) fastestFirst= 0;
    else if (fastestFirst) fastestFirst= 0;
    else fastestFirst= 1;
  }
  yPDBclose|= fastestFirst;

  /* Now that the extras section has been read, the specification of the
     primitive data types is complete, and a determination of whether
     a converter is required may be made.  */
  for (i=1 ; i<6 ; i++) SetConverter(file->structList[i], i>3);

  /* Read the structure chart.  */
  RDchart(file, chartAddress);

  /* Read the symbol table.  This is the second pass (RDextras was first);
     the variables are actually added here.  */
  RDsymtab(file, symtabAddress);

  file->CloseHook= YPDBcloser[yPDBclose];

  /* Check for DMI-style time history records.
     Revises file dataTable and adds history child if found.
     All files in a family are scanned here.
     Whether or not there are history records, DMIhistory tries to
     set file->nextAddress intelligently...  */
  DMIhistory(file, chartAddress, symtabAddress, familyOK);

  has_dirs = 0;
  BlocksInit();
  return ClearBuffer(0);
}

/*--------------------------------------------------------------------------*/

int YinitPDB(IOStream *file, int close102)
{
  long headerAddress, headLen, i;
  StructDef *longS= file->structList[3];

  if ((file->permissions&3)!=3) return 1;

  yPDBclose= close102 & 017;

  /* An IOStream is born with 8 primitive data types--
     add memberPointer and variablePointer now.  */
  YPDBpointers(file, longS->size, longS->alignment);

  InitBuffer(file, 512L, 0L);

  sprintf(nextByte, "%s\012", HEADTOK);
  nextByte+= strlen(nextByte);

  nextByte= EncodePrimitives(file, nextByte);

  headerAddress= NextWRAddress(0);

  /* write a dummy chart and symtab address, leaving 128 bytes for
     this header line to use later */
  sprintf(nextByte, "%ld\001%ld\001\012",
          headerAddress+128, headerAddress+128);
  headLen= strlen(nextByte);
  nextByte+= headLen;
  for (i=headLen ; i<=128 ; i++) *(nextByte++)= '\0';

  WriteBuffer();

  /* force struct-like data alignment to ensure that record data can be
     described as data structures -- this is NOT standard PDBLib practice */
  file->dataAlign= 0;

  file->CloseHook= YPDBcloser[yPDBclose];

  return ClearBuffer(0);
}

void YPDBpointers(IOStream *file, long size, int align)
{
  /* Each IOStream is "born" with 8 primitive data types
     (char, short, int, long, float, double, string, and pointer).
     Add the "char *" and "char*" PDB pointer types now.  */
  StructDef *base;
  if (!HashFind(&file->structTable, memberPointer, 0L)) {
    base= AddStruct(file, memberPointer, 0L);
    DefinePrimitive(base, size, align, 2, 0, (FPLayout *)0,
                    &pointerStruct, &PDBconvert);
    InstallStruct(base, (StructDef *)0);
  }
  if (!HashFind(&file->structTable, variablePointer, 0L)) {
    base= AddStruct(file, variablePointer, 0L);
    DefinePrimitive(base, 0L, 1, 2, 0, (FPLayout *)0,
                    &pointerStruct, &PDBconvert);
    InstallStruct(base, (StructDef *)0);
  }
  /* note: before yorick-1.6, always wrote just member and variable pointer
   * in addition to the 8 automatically defined types, for a total of 10
   * types always assumed present
   * for a short time until mid-yorick-1.6.03, created Directory here
   * as an 11th type, which causes no problems until you attempt to read
   * a file written with yorick-1.5 containing pointers to objects with
   * structs beyond the original total of 10 (either complex or a true
   * struct) - at that ponit the numbering of the structs in the 1.6
   * yorick did not match the numbering in 1.5, preventing 1.6 from
   * correctly interpreting such 1.5 files
   * -the reverse operation, reading 1.6.02 files with 1.5 works correctly,
   *  because the Directory struct is written into the file as
   *  the eleventh type, which it really is
   * hence, for backward compatibility, must always define precisely
   * the original 10 data type automatically, no more and no less,
   * defining "Directory" if and only if it is actually used
   * - interestingly, none of the files is actually incorrect, the
   *   problem is that yorick pointee headers assume an offset of
   *   exactly 10 to the first user-defined struct/primitive
   *   - this is poor design, but hard to fix given that the PrimitiveType
   *     and struct order is defined only by the yorick source code
   */
}

/*--------------------------------------------------------------------------*/

/* YclosePDB is meant to be installed as the IOStream CloseHook
   for a PDB file.  It must write the PDB structure chart, symbol table,
   and extras section to the file.  Additionally, for a time history file,
   the time history variables must be created and written before the
   chart, symtab, and extras.  */

static void ClosePDB(IOStream *file);

static void Y00close(IOStream *), Y01close(IOStream *), Y02close(IOStream *);
static void Y03close(IOStream *);
static void Y12close(IOStream *), Y13close(IOStream *);
static void Y16close(IOStream *), Y17close(IOStream *);

void (*YPDBcloser[16])(IOStream *file)= { &Y00close, &Y01close, &Y02close,
  &Y03close, &Y00close, &Y01close, &Y02close, &Y03close, &Y00close,
  &Y01close, &Y12close, &Y13close, &Y00close, &Y01close, &Y16close,
  &Y17close };

static void Y00close(IOStream *file) { yPDBclose=000; ClosePDB(file); }
static void Y01close(IOStream *file) { yPDBclose=001; ClosePDB(file); }
static void Y02close(IOStream *file) { yPDBclose=002; ClosePDB(file); }
static void Y03close(IOStream *file) { yPDBclose=003; ClosePDB(file); }
static void Y12close(IOStream *file) { yPDBclose=012; ClosePDB(file); }
static void Y13close(IOStream *file) { yPDBclose=013; ClosePDB(file); }
static void Y16close(IOStream *file) { yPDBclose=016; ClosePDB(file); }
static void Y17close(IOStream *file) { yPDBclose=017; ClosePDB(file); }

static void ClosePDB(IOStream *file)
{
  long symtabAddress;
  long chartAddress;
  int notOK;

  if (!(file->permissions&2) ||
      (file->history &&
       file->history->fileNumber!=file->history->nFamily-1)) return;
  has_dirs = 0;

  FreeClogFile(file);  /* don't want to log this stuff... */

  notOK= WRhistory(file->history);
  if (file->history) chartAddress= file->history->child->nextAddress;
  else chartAddress= file->nextAddress;

  InitBuffer(file, 4096L, chartAddress);

  notOK|= WRchart(file);

  symtabAddress= NextWRAddress(0);
  notOK|= WRsymtab(file);

  notOK|= WRextras(file);

  WriteBuffer();
  ClearBuffer(0);

  notOK|= WRheader(file, chartAddress, symtabAddress);

  has_dirs = 0;
  if (notOK) {
    /* don't try to call YclosePDB again */
    if (!file->history) file->CloseHook= 0;
    else file->history->parent->CloseHook= file->history->child->CloseHook= 0;
    YErrorIO("failed (but tried) to properly close PDB file");
  } else {
    ZapClogFile(file);
  }
}

/*--------------------------------------------------------------------------*/

/*                            READ ROUTINES                                 */

/*--------------------------------------------------------------------------*/

static int DecodePrimitives(IOStream *file,
                            unsigned char *buffer, char *token)
{
  int i;
  FPLayout ffpLayout, dfpLayout;

  StructDef **structList= file->structList;
  /* StructDef *charS= structList[0]; */
  StructDef *shortS= structList[1];
  StructDef *intS= structList[2];
  StructDef *longS= structList[3];
  StructDef *floatS= structList[4];
  StructDef *doubleS= structList[5];
  StructDef *stringS= structList[6];
  StructDef *pointerS= structList[7];

  /* get the byte lengths */
  mptrSize= *(buffer++);       /* PDB-style pointer, type "char *" */
  shortS->size= *(buffer++);
  intS->size= *(buffer++);
  longS->size= *(buffer++);
  floatS->size= *(buffer++);
  doubleS->size= *(buffer++);

  /* get the integral types byte order */
  shortS->order= (*(buffer++)==1)? 1 : -1;
  intS->order= (*(buffer++)==1)? 1 : -1;
  longS->order= (*(buffer++)==1)? 1 : -1;

  /* pointer and string must track long */
  pointerS->size= stringS->size= longS->size;
  pointerS->order= stringS->order= longS->order;

  /* get the float byte order */
  if (DecodeOrder(&floatS->order, buffer, floatS->size)) {
    floatS->order= 0;
    YWarning("PDB floating point format weird -- float data will be bad");
  }
  buffer+= floatS->size;

  /* get the double byte order */
  if (DecodeOrder(&doubleS->order, buffer, doubleS->size)) {
    doubleS->order= 0;
    YWarning("PDB floating point format weird -- double data will be bad");
  }
  buffer+= doubleS->size;

 /*               - Floating Point Format Descriptor
  *               -   format[0] = # of bits per number
  *               -   format[1] = # of bits in exponent
  *               -   format[2] = # of bits in mantissa
  *               -   format[3] = start bit of sign
  *               -   format[4] = start bit of exponent
  *               -   format[5] = start bit of mantissa
  *               -   format[6] = high order mantissa bit (CRAY needs this)
  *               -       bias of exponent written separately
  */

  /* get the float format data (except exponent bias) */
  buffer++;
  ffpLayout.expSize= *(buffer++);
  ffpLayout.manSize= *(buffer++);
  ffpLayout.sgnAddr= *(buffer++);
  ffpLayout.expAddr= *(buffer++);
  ffpLayout.manAddr= *(buffer++);
  ffpLayout.manNorm= *(buffer++);

  /* get the double format data (except exponent bias) */
  buffer++;
  dfpLayout.expSize= *(buffer++);
  dfpLayout.manSize= *(buffer++);
  dfpLayout.sgnAddr= *(buffer++);
  dfpLayout.expAddr= *(buffer++);
  dfpLayout.manAddr= *(buffer++);
  dfpLayout.manNorm= *(buffer++);

  /* exponent biases are stored with "%ld\001%ld\001\n" */
  if (!token) return 1;

  token= strtok(token, "\001");
  if (!token) return 2;
  ffpLayout.expBias= strtol(token, (char **)0, 10);
  token= strtok(0, "\001");
  if (!token) return 3;
  dfpLayout.expBias= strtol(token, (char **)0, 10);

  /* either recognize the formats or copy them */
  FreeFPLayout(floatS->fpLayout);
  floatS->fpLayout= MakeFPLayout(&ffpLayout, floatS->size);
  FreeFPLayout(doubleS->fpLayout);
  doubleS->fpLayout= MakeFPLayout(&dfpLayout, doubleS->size);

  /* make a wild guess at alignment for now --
     alignments are actually retrieved from the extras section later */
  for (i=0 ; i<8 ; i++) structList[i]->alignment= structList[i]->size;

  return 0;
}

static void SetConverter(StructDef *base, int isReal)
{
  StructDef *model= base->model;
  if (base->size!=model->size || base->order!=model->order) {
    base->Convert= isReal? &ConvertF : &ConvertI;
  } else if (isReal && !SameFPLayout(base->fpLayout, model->fpLayout)) {
    base->Convert= &ConvertF;
  } else {
    base->Convert= 0;
  }
}

static int DecodeOrder(int *order, unsigned char *permute, long n)
{
  int i, lswFirst=0, wordSize, w, b;

  /* first, scan for the "1" or "n" in the permutation */
  for (i=0 ; i<n ; i++) {
    if (permute[i]==1) {
      lswFirst= 0;
      break;
    } else if (permute[i]==n) {
      lswFirst= 1;
      break;
    }
  }
  if (i<n) {
    wordSize= i+1;
  } else {
    /* this will return an error below */
    lswFirst= 0;
    wordSize= 1;
  }

  *order= lswFirst? -wordSize : wordSize;

  /* n must be a multiple of imputed wordSize */
  if (n%wordSize) return 1;

  /* check to be sure that entire permutation is in fact described by
     lswFirst and wordSize */
  if (lswFirst) {
    for (w=n-1 ; w>=0 ; w-=wordSize)
      for (b=1 ; b<=wordSize ; b++) if ((*permute++)!=(w+b)) return 1;
  } else {
    for (w=0 ; w<n ; w+=wordSize)
      for (b=wordSize ; b>0 ; b--) if ((*permute++)!=(w+b)) return 1;
  }

  return 0;
}

#define IEEE_32_64   1        /* IEEE standard 32 bit float : 64 bit double */
#define INTEL_X86    2                        /* Intel 80x86 class machines */
#define CRAY_64      3                               /* CRAY class machines */
#define VAX_11       4                                /* VAX class machines */
#define IEEE_32_96   6        /* IEEE standard 32 bit float : 96 bit double */

static void DecodeMachine(IOStream *file, long machine)
{
  switch (machine) {
  case IEEE_32_64:
    CopyLayouts(file, sun3Layout);
    break;
  case IEEE_32_96:
    CopyLayouts(file, mac2Layout);
    break;
  case INTEL_X86:
    CopyLayouts(file, ibmpcLayout);
    break;
  case CRAY_64:
    CopyLayouts(file, cchybridLayout);
    /* original code (PDBLib) would have used
       CopyLayouts(file, crayLayout);
       if COMPILED on non-Livermore Computer Center machine */
    break;
  case VAX_11:
    CopyLayouts(file, vaxLayout);
    /* original code (PDBLib) would have used fp8gvax instead of fp8hvax
       if COMPILED with GFLOAT floating point switch set */
    break;
  default:
    CopyLayouts(file, pdbLayout);
    /* probably wrong?? */
    break;
  }
}

void CopyLayouts(IOStream *file, DataLayout *src)
{
  int i;
  StructDef **structList= file->structList;

  for (i=0 ; i<6 ; i++) {
    structList[i]->alignment= src[i].alignment;
    structList[i]->size= src[i].size;
    structList[i]->order= src[i].order;
    FreeFPLayout(structList[i]->fpLayout);
    structList[i]->fpLayout= Ref(src[i].fpLayout);
  }

  /* all of the particular machines in DecodeMachine have: */
  mptrSize= structList[3]->size;

  /* string and pointer must track long */
  structList[6]->size= structList[7]->size= structList[3]->size;
  structList[6]->alignment= structList[7]->alignment=
    structList[3]->alignment;
  structList[6]->order= structList[7]->order= structList[3]->order;
}

/*--------------------------------------------------------------------------*/

static unsigned char *fltOrder= 0;
static long orderSize= 0;

static void RDextras(IOStream *file, long address)
{
  char *line, *token;
  long version= 0;
  int dateBugCheck= 0;

  /* set default values for Major-Order, Offset, and Alignment extras */
  fastestFirst= 0;
  defaultOrigin= 0;
  gotAlignment= 0;

  InitBuffer(file, 4096L, address);

  /* skip over the symbol table to get to the extras--
     two consecutive newlines marks end of symtab
     Check for DMI-style history records during this pass.  */
  firstLength= nSeq= nSeq2= nTimes= nCycles= 0;
  firstAddr= numbAddr= timeAddr= ncycAddr= 0;
  while ((line= ReadUntil(ANY_NEWLINE)) && line[0])
    DMIcheckSymbol(strtok(line, "\001"));

  /* two consecutive newlines marks end of extras */
  while ((line= ReadUntil(ANY_NEWLINE)) && (line[0] || dateBugCheck)) {
    if (!line[0]) {
      /* The date in the Version: extra can easily have an erronious
         newline character.  Check for this common bug before giving up.  */
      dateBugCheck= 0;
      if (!(line= ReadUntil(ANY_NEWLINE)) || !line[0]) break;
    }

    if (strncmp(line, "Offset:", 7L)==0) {
      /* The default offset is meaningless to Yorick -- the correct
         minimum index for every variable in the file has already been
         written.  However, it may be needed to interpret the dimension
         list of a structure member when reading the chart.
         Actually, the best idea might be to recognize variables
         with minimum index equal to defaultOrigin, and translate them
         to 1-origin...  */
      TokenAsLong(&line[7], &defaultOrigin);

    } else if (strncmp(line, "Alignment:", 10L)==0) {
      StructDef **structList= file->structList;
      structList[0]->alignment= line[10];
      mptrAlign= line[11];
      if (HashFind(&file->structTable, memberPointer, 0L))
        structList[hashIndex]->alignment= mptrAlign;
      structList[1]->alignment= line[12];
      structList[2]->alignment= line[13];
      structList[3]->alignment= /* long, string, and pointer alignment */
        structList[6]->alignment= structList[7]->alignment= line[14];
      structList[4]->alignment= line[15];
      structList[5]->alignment= line[16];
      gotAlignment= 1;

    } else if (strncmp(line, "Struct-Align:", 13L)==0 ||
               strncmp(line, "Struct-Alignment:", 17L)==0) {
      long structAlign = 0;
      TokenAsLong(line[13]!='e'? &line[13]:&line[17], &structAlign);
      if (structAlign<=0) structAlign= 1;
      file->structAlign= (int)structAlign;

    } else if (strncmp(line, "Version:", 8L)==0) {
      TokenAsLong(&line[8], &version);
      /* The version number is followed by a vertical bar "|", then the
         date in ASCII.  Ignore them.  */
      dateBugCheck= 1;

    } else if (strncmp(line, "Blocks:", 7L)==0) {
      /* This one reads several more ANY_NEWLINE delimited lines...  */
      /* The blocks table may be meaningless to Yorick, but it may represent
         time history data.  It ends with "\002".  */
      BlocksVerify();

    } else if (strncmp(line, "Casts:", 6L)==0) {
      /* This one reads several more ANY_NEWLINE delimited lines...  */
      /* The casts table is totally meaningless to Yorick, since the
         correct (cast) data type is written at the beginning of every
         pointee int the file.  Just skip it all (until "\002" line).  */
      while ((line= ReadUntil(ANY_NEWLINE))) if (line[0]=='\002') break;

    } else if (strncmp(line, "Major-Order:", 12L)==0) {
      /* If the fastest varying dimension is listed first, the Dimension
         linked lists in the dataTable will need to be reversed, and the
         dimensioned members of a struct will be read differently.  */
      long code;
      if (TokenAsLong(&line[12], &code)==0 && code==102) fastestFirst= 1;

    } else if (strncmp(line, "Has-Directories:", 16L)==0) {
      long hd;
      if (TokenAsLong(&line[16], &hd)==0 && hd!=0) has_dirs = 1;

    } else if (strncmp(line, "Primitive-Types:", 16L)==0) {
      /* This one reads several more ANY_NEWLINE delimited lines...  */
      char *typeName;
      long size, align, order_flag, i, value;
      int order, isFloat;
      FPLayout fpLayout;
      StructDef *base;

      while ((line= ReadUntil(ANY_NEWLINE)) && line[0]!='\002') {
        typeName= FlopIntegerAndInt(strtok(line, "\001"));
        if (TokenAsLong(0, &size) ||
            TokenAsLong(0, &align) ||
            TokenAsLong(0, &order_flag)) break;
        /* note- order_flag is 1 for MSB first, 2 for LSB first */

        if (!(token= strtok(0, "\001"))) break;
        if (strcmp(token, "ORDER")==0) {
          CheckOrderScratch(size);
          for (i=0 ; i<size ; i++) {
            if (TokenAsLong(0, &value)) break;
            fltOrder[i]= (unsigned char)value;
          }
          if (i<size) break;
          if (DecodeOrder(&order, fltOrder, size)) break;
        } else { /* else token should be "DEFORDER" */
          order= order_flag==1? 1 : -1;
        }

        if (!(token= strtok(0, "\001"))) break;
        isFloat= 0;
        if (strcmp(token, "FLOAT")==0) {
          isFloat= 1;
          if (TokenAsLong(0, &value)) break;  /* 1st unused */
          if (TokenAsLong(0, &value)) break;
          fpLayout.expSize= value;
          if (TokenAsLong(0, &value)) break;
          fpLayout.manSize= value;
          if (TokenAsLong(0, &value)) break;
          fpLayout.sgnAddr= value;
          if (TokenAsLong(0, &value)) break;
          fpLayout.expAddr= value;
          if (TokenAsLong(0, &value)) break;
          fpLayout.manAddr= value;
          if (TokenAsLong(0, &value)) break;
          fpLayout.manNorm= value;
          if (TokenAsLong(0, &fpLayout.expBias)) break;
        } else if (strcmp(token, "NO-CONV")==0) {
          order= 0;
        } /* else token should be "FIX" */

        /* install the new primitive type */
        if ((strcmp(typeName, "string")==0 ||
             strcmp(typeName, "pointer")==0) && !isFloat) {
          StructDef *longDef= file->structList[3];
          if (size==longDef->size && align==longDef->alignment &&
              order==longDef->order) continue;
        }
        if (strcmp(typeName, "*")==0 ||
            (HashFind(&file->structTable, typeName, 0L) && hashIndex<6))
          continue;   /* PDBLib duplicates basic types here */
        base= AddStruct(file, typeName, 0L);
        if (!base) continue;
        /* fool DefinePrimitive and InstallStruct by resetting
           base->references and base->dataOps */
        isFloat= DefinePrimitive(base, size, align, 1, order,
                                 isFloat? &fpLayout : 0,
                                 (StructDef *)0, (Converter *)0);
        /* a common problem is a NO-CONV object, which will return 5
           here -- just let this blow up if the guy actually tries to
           read or write one, the warnings become too tedious */
        if (isFloat && (order || isFloat!=5))
          YWarning("illegal primitive type definition in PDB extras section");
        InstallStruct(base, (StructDef *)0);
      }
      if (line && line[0]!='\002') {
        /* attempt to resynchronize after garbled primitive type */
        YWarning("Primitive-Types PDB extra garbled -- resynchronizing");
        while ((line= ReadUntil(ANY_NEWLINE))) if (line[0]=='\002') break;
      }

    } else {
      /* An unknown extra has been found-- presumably defined after
         this code was written.  Skip over it.
         Extras must be of the form:
           Extra-Name: blah-blah-blah \n
         or
           Extra-Name:\n
           blah\n
           blah\n
           \n
         or
           Extra-Name:\n
           blah\n
           blah\n
           \002 blah-blah-blah \n
       */
      line+= strcspn(line, ":");  /* skip the Extra-Name */
      if (!line[0]) {
        /* this is a multi-line extra */
        while ((line= ReadUntil(ANY_NEWLINE)))
          if (!line[0] || line[0]=='\002') break;
      }
    }
  }
}

static void CheckOrderScratch(long size)
{
  if (size>orderSize) {
    fltOrder= p_realloc(fltOrder, size+8);
    orderSize= size+8;
  }
}

/*--------------------------------------------------------------------------*/

/* Read the PDB structure chart.
   Since the Alignment and PrimitiveType extras have not yet been
   read, the sizes, alignments, and member offsets will not be correct.
   A second pass corrects them, after the extras have been read.
   The biggest complication here is the use of a PrimitiveType as the
   data type of a member -- a blank definition is inserted here.  */
static int RDchart(IOStream *file, long address)
{
  char *typeName, *descriptor, *mType, *mName, *token;
  StructDef *base;
  long size, number, origin;
  Dimension *dims, *next;
  int warned= gotAlignment;
  int goofCount= 0, goofed= 0;

  InitBuffer(file, 4096L, address);

  while ((typeName= ReadUntil(SOH_OR_NEWLINE)) && typeName[0]!='\002') {
    if (!warned) {
      YWarning("no PDB Alignments: extra -- beware of structure instances");
      warned= 1;
    }
    goofed= 0;

    /* get the size of an instance of this structure --
       this had better be at least the size implicit from the descriptor
       list, and a multiple of the alignment calculated therefrom */
    descriptor= ReadUntil(SOH_OR_NEWLINE);
    if (!descriptor) {
      if (goofCount<10) YWarning("illegal struct size in PDB chart");
      goofCount++;
      continue;
    }

    base= 0;
    size= strtol(descriptor, (char **)0, 10);

    /* Member descriptors are separated by "\001", and the decsriptor
       list ends with one of the legal NEWLINE characters, hence a
       "\001\n" marks the end of the member descriptor list.  */
    /* NOTE: This code differs in detail from the original PDBLib code,
             but is closer to the recently documented PDB file format.  */
    while ((descriptor= ReadUntil(SOH_OR_NEWLINE)) && descriptor[0]) {
      if (goofed) continue;

      if (strchr(descriptor, '*')) {
        mType= memberPointer;
        strtok(descriptor, " *\t"); /* initialize strtok */
      } else {
        mType= strtok(descriptor, " \t");
        if (!mType || !mType[0]) {
          if (goofCount<10) YWarning("bad member type in PDB chart");
          goofed= 1;
          continue;
        }
        mType= FlopIntegerAndInt(mType);
      }

      mName= strtok(0, " *\t([");
      if (!mName || !mName[0]) {
        if (goofCount<10) YWarning("bad member name in PDB chart");
        goofed= 1;
        continue;
      }

      dims= tmpDims;
      tmpDims= 0;
      FreeDimension(dims);
      number= 1;
      while ((token= strtok(0, " \t[(,)]"))) {
        number= strtol(token, (char **)0, 10);
        token+= strcspn(token, ":");
        if (token[0]) {
          origin= number;
          number= strtol(token+1, (char **)0, 10) - origin + 1;
        } else {
          /* The other possibility here is defaultOrigin for the file.
             I prefer to suppose that if no origin is explicitly
             mentioned, then the preference of the person reading
             the file makes more sense than the preference of the one
             writing the file.  */
          origin= 1L;
        }
        if (number<=0) break;
        if (fastestFirst) {
          /* add new dimension to head of list */
          tmpDims= NewDimension(number, origin, tmpDims);
        } else {
          /* add faster dimension to tail of list */
          next= NewDimension(number, origin, (Dimension *)0);
          if (tmpDims) dims->next= next;
          else tmpDims= next;
          dims= next;
        }
      }
      if (number<=0) {
        if (goofCount<10) YWarning("bad member dimension list in PDB chart");
        goofed= 1;
      }

      /* create new StructDef on first pass (first member) */
      if (goofed) continue;
      if (!base) {
        base= AddStruct(file, typeName, 0L);
        if (!base) {
          if (goofCount<10) YWarning("illegal struct name in PDB chart");
          goofed= 1;
          continue;
        }
      }

      /* add new member to this StructDef */
      if (!HashFind(&file->structTable, mType, 0L) ||
          AddMember(base, -1L, mName, file->structList[hashIndex], tmpDims)) {
        if (goofCount<10) YWarning("bad member descriptor in PDB chart");
        goofed= 1;
      }
    }

    if (!base) {
      /* primitive -- already handled in RDextras or DecodePrimitives */
      if (!goofed) {
        if (typeName[0]=='*' && !typeName[1]) typeName= memberPointer;
        /* the condition here works around a bug in a previous version of
           Yorick, which wrote files with the integer data type called
           int instead of integer -- plain else would be more correct, but
           then a warning is generated when a file written by the old
           version is opened -- sigh */
        else if (strcmp(typeName,"int") ||
                 HashFind(&file->structTable, "integer", 0L))
          typeName= FlopIntegerAndInt(typeName);
        if (HashFind(&file->structTable, typeName, 0L)) {
          base= file->structList[hashIndex];
        } else {
          if (goofCount<10) YWarning("unknown primitive in PDB chart");
          goofed= 1;
        }
      }

    } else {
      /* install the completed structure definition in the structTable */
      InstallStruct(base, (StructDef *)0);
    }

    if (goofed) {
      goofCount++;
    } else if (base->size!=size) {
      if (goofCount<10) YWarning("struct size inconsistency in PDB chart");
      goofCount++;
    }
  }

  return goofCount>0;
}

static char *intPDB= "integer";
static char *integerPDB= "int";

static char *FlopIntegerAndInt(char *typeName)
{
  if (typeName && typeName[0]=='i' && typeName[1]=='n' && typeName[2]=='t') {
    if (typeName[3]==0) return intPDB;
    else if (strcmp(&typeName[3], "eger")==0) return integerPDB;
  }
  return typeName;
}

char *PDBvarType(char *typeName)
{
  if (typeName) {
    if (strchr(typeName, '*')) {
      /* This is a pointer.
         The data type is irrelevant, since the true type is written
         into the file at the location of the pointee (just before the
         pointee data).  */
      typeName= variablePointer;
    } else {
      /* clean any spurious blanks out of type name --
         Should tabs be included as well as blanks?
         (_PD_member_base_type does NOT allow tabs as of PDB version 7) */
      typeName+= strspn(typeName, " ");
      typeName[strcspn(typeName, " ")]= '\0';
      typeName= FlopIntegerAndInt(typeName);
    }
  }

  return typeName;
}

/*--------------------------------------------------------------------------*/

static char *dmiName= 0;
static long dmiMaxLength= -1;
static long dmiBase, dmiFirst, dmiNumber;

static char atHistory[]= "@history";

/* Read PDB symbol table.  */
static int RDsymtab(IOStream *file, long address)
{
  char *line, *dataName, *token;
  int hasHistory, goofCount= 0;
  long addr, minAddr=0, n=0;

  InitBuffer(file, 4096L, address);

  hasHistory= HasDMIrecords();  /* check results of RDextras */
  if (hasHistory) {
    GrabDMIData(file, firstAddr, firstLength, numbAddr);
    if (dmiNumber<0) hasHistory= 0;
    else if (nBlocks) BlocksInit();
    yPDBclose&= 1;
  }

  /* two consecutive newlines marks end of symtab */
  while((line= ReadUntil(ANY_NEWLINE)) && line[0]) {

    /* first token is variable name */
    dataName= strtok(line, "\001");
    if (!dataName) {
      if (goofCount<10) YWarning("bad variable name in PDB symtab");
      goofCount++;
      continue;
    }

    /* skip record and DMI-descriptive variables -- they are intended
       for the history child */
    if (hasHistory) {
      if (IsDMIvariable(dataName) || IsDMIrecord(dataName)>=0) continue;
    } else if (nBlocks) {
      /* PDB style history variables are skipped as well */
      if (HashFind(&blocksTable, dataName, 0L)) {
        n++;
        continue;
      }
    } else if (yPDBopen&010) {
      /* Basis PDB style history variables must be skipped, but can
         be identified certainly only by "@history" at end of name.  */
      long i, len= strlen(dataName)-1;
      for (i=7 ; i>=0 && len>0 ; i--,len--)
        if (dataName[len]!=atHistory[i]) break;
      if (i<0) {
        if (HashAdd(&blocksTable, dataName, 0L)) {
          if (goofCount<10)
            YWarning("duplicate @history variable name in PDB symtab");
          goofCount++;
        }
        if (!strtok(0, "\001")) continue;
        token= strtok(0, "\001");
        if (!token || strtol(token, (char**)0, 10)<=0) continue;
        dataName= strtok(0, "\001");
        if (!dataName || (addr=strtol(dataName, (char**)0, 10))<0) continue;
        if (!minAddr || addr<minAddr) minAddr= addr;
        n++;
        continue;
      }
    }

    goofCount= GrabSymbol(dataName, file, goofCount, 0L, (char **)0);
  }

  if (n && !nBlocks) {
    nBlocks= 1;
    blocksAddrs= (long *)p_malloc(sizeof(long));
    blocksAddrs[0]= minAddr;
  }

  if (nBlocks) {
    /* guess at how file should be closed */
    long i, len= strlen(blocksTable.names[0])-1;
    for (i=7 ; i>=0 && len>0 ; i--,len--)
      if (blocksTable.names[0][len]!=atHistory[i]) break;
    if (i<0) yPDBclose|= 4;
    else yPDBclose&= 013;
  }

  if (nBlocks && n!=blocksTable.nItems) {
    if (goofCount<10) YWarning("PDB symtab and extras Blocks: inconsistent");
    goofCount++;
  }

  return goofCount>0;
}

static int GrabSymbol(char *dataName, IOStream *file, int goofCount,
                      long offset, char **context)
{
  char *token, *typeName;
  long dataAddress, origin, length, nitems;
  Dimension *dims, *next;
  StructDef *base;

  /* second token is type name -- pointer if followed by any *'s */
  typeName= strtok(0, "\001");
  if (!typeName) {
    if (goofCount<10) YWarning("bad variable type in PDB symtab");
    return goofCount+1;
  }
  typeName= PDBvarType(typeName);

  /* third token is total number of items of specified type */
  token= strtok(0, "\001");
  if (!token || (nitems=strtol(token, (char**)0, 10))<=0) {
    if (goofCount<10) YWarning("bad item count in PDB symtab");
    return goofCount+1;
  }

  /* fourth token is data address (for pointers, this is address of the
     first pointee) */
  token= strtok(0, "\001");
  if (!token || (dataAddress=strtol(token, (char**)0, 10))<0) {
    if (goofCount<10) YWarning("bad data address in PDB symtab");
    return goofCount+1;
  }

  /* fifth token and beyond, if present, are dimensions -- ordinarily
     slowest varying to fastest varying, but might need to be adjusted
     to the other order later */
  dims= tmpDims;
  tmpDims= 0;
  FreeDimension(dims);
  while ((token= strtok(0, "\001"))) {
    origin= strtol(token, (char**)0, 10);
    /* shift arrays with the stated default origin to user's preference
       -- this is not quite ideal, since there is no way for the file
       format to force these to the defaultOrigin... */
    /*if (origin==defaultOrigin)*/ origin= 1L;
    token= strtok(0, "\001");
    if (!token || (length=strtol(token, (char**)0, 10))<=0) {
      nitems= 0;
      break;
    }
    if (fastestFirst) {
      /* add new dimension to head of list */
      tmpDims= NewDimension(length, origin, tmpDims);
    } else {
      next= NewDimension(length, origin, (Dimension *)0);
      if (tmpDims) dims->next= next;
      else tmpDims= next;
      dims= next;
    }
    nitems/= length;
  }
  if (nitems!=1) {
    if (!tmpDims && nitems>1) {
      tmpDims= NewDimension(nitems, 1L, (Dimension *)0);
    } else {
      if (goofCount<10) YWarning("bad dimension list in PDB symtab");
      return goofCount+1;
    }
  }

  /* remove fake trailing dimension from PDB style history variables */
  if (offset && tmpDims && tmpDims->number==1) {
    dims= tmpDims;
    tmpDims= dims->next;
    dims->next= 0;
    FreeDimension(dims);
  }

  if (!HashFind(&file->structTable, typeName, 0L)) {
    if (goofCount<10) YWarning("non-existent type name in PDB symtab");
    return goofCount+1;
  }
  base= file->structList[hashIndex];

  /* Optionally strip Basis @... suffices if possible.  */
  if (yPDBopen&4) {
    char *c= dataName;
    long len;
    while (c[0] && c[0]!='@') c++;
    len= c-dataName;
    if (c[0] && len) {
      /* strip suffix if possible */
      HistoryInfo *history= file->history;
      IOStream *f;
      /* discard variable completely if @macro or @funct */
      if (!strcmp(c,"@macro") || !strcmp(c,"@funct"))
        return goofCount;
      if (history) f= history->parent;
      else f= file;
      if (!HashFind(&f->dataTable, dataName, len)) {
        if (!history ||
            !HashFind(&history->child->dataTable, dataName, len))
          c[0]= '\0';
      }
    }
  }

  if (context) {
    if (blocksTable.nItems==1 && !tmpDims) {
      /* This is a single scalar struct instance, used as a history
         record.  Break it open and record its members as separate
         variables.  */
      if (!BurstRecord(file, base)) return goofCount; /* Success, ... */
      /* ...otherwise, this was not a compound struct. */
      *context= StructName(base);
    }
    if (yPDBopen&010) {
      long i, len= strlen(dataName)-1;
      for (i=7 ; i>=0 && len>0 ; i--,len--)
        if (dataName[len]!=atHistory[i]) break;
      if (i<0) dataName[len+1]= '\0';
    }
  }

  /* Make the entry in the dataTable for this variable.  */
  if (AddVariable(file, dataAddress-offset, dataName, base, tmpDims)) {
    if (goofCount<10) YWarning("illegal variable in PDB symtab");
    goofCount++;
  }

  return goofCount;
}

/*--------------------------------------------------------------------------*/

static void CheckDMIScratch(long maxLength)
{
  if (maxLength>dmiMaxLength) {
    p_free(dmiName);
    dmiName= p_malloc(maxLength+1);
  }
}

static int CheckTorN(IOStream *file, char *name, long type);
static void GrabTorN(HistoryInfo *history, IOStream *child,
                     long timeOffset, long ncycOffset);

static void DMIhistory(IOStream *file, long addrChrt, long address,
                       int familyOK)
{
  HistoryInfo *history;
  IOStream *child;

  long addrTimes= 0, addrCycles= 0, addrSymt;
  long nRecords, r, index, *offsets;
  long timeOffset=-1, ncycOffset=-1;
  char *baseName;
  StructDef *base= 0;
  int dmiStyle= HasDMIrecords();

  if (!dmiStyle && !nBlocks) {
    /* set the nextAddress as PDBLib would set it.  */
    file->nextAddress= addrChrt;
    return;
  }

  /* make this file contain history records */
  history= AddHistory(file, 0L);
  child= history->child;

  /* third pass through symbol table reads record addresses --
     this also creates the child dataTable by transferring the
     record data structure to the child */
  baseName= FindAddresses(child, address, (char *)0, dmiStyle);

  /* fill in the times and ncycs as appropriate */
  if (dmiStyle) {
    addrTimes= timeAddr;
    addrCycles= ncycAddr;
    if (baseName && addrTimes)
      YRead(history->time, addrTimes,
            child->structList[5], dmiNumber, (Strider *)0);
    if (baseName && addrCycles)
      YRead(history->ncyc, addrCycles,
            child->structList[3], dmiNumber, (Strider *)0);
  } else {
    addrTimes= addrCycles= 0;
    if (CheckTorN(child, "time", 5L))
      timeOffset= child->addresses[hashIndex];
    if (CheckTorN(child, "ncyc", 3L))
      ncycOffset= child->addresses[hashIndex];
    GrabTorN(history, child, timeOffset, ncycOffset);
  }

  /* optionally look for more files in this family --
     requires two passes thru symtab, first to get dmiNumber, second
     to get record addresses (this allows the records to be added in
     the proper order, since in files written using PDBLib, the
     symtab is in random order) */
  while (familyOK && AddNextFile(history, (char *)0, 0)==0) {
    if (FindSymtab(child, &addrSymt, &addrChrt) ||
        FindRecords(child, addrSymt, addrTimes? &addrTimes:0,
                    addrCycles? &addrCycles:0, dmiStyle)) continue;

    /* rescan symtab to find record addresses */
    nRecords= history->nRecords;
    baseName= FindAddresses(child, addrSymt,
                            baseName? baseName : (char *)child, dmiStyle);

    /* verify that record type has the expected byte size */
    if (baseName) {
      if (base==0) {
        if (HashFind(&child->structTable, baseName, 0L))
          base= child->structList[hashIndex];
        else
          YErrorIO("DMI-style record structure vanished opening PDB file");
      }
      if (VerifyRecType(child, addrChrt, baseName, base->size)) {
        YWarning("skipping PDB family file -- record struct changed");
        continue;
      }

      /* read true times and cycles if provided */
      if (addrTimes)
        YRead(history->time+nRecords, addrTimes,
              child->structList[5], dmiNumber, (Strider *)0);
      if (addrCycles)
        YRead(history->ncyc+nRecords, addrCycles,
              child->structList[3], dmiNumber, (Strider *)0);
    }
    if (!dmiStyle) GrabTorN(history, child, timeOffset, ncycOffset);
  }

  /* clean up records */
  nRecords= history->nRecords;
  offsets= history->offset;
  index= 0;
  for (r=0 ; r<nRecords ; r++) {
    if (offsets[r]) {
      /*  this record is OK */
      if (index<r) {
        offsets[index]= offsets[r];
        history->ifile[index]= history->ifile[r];
        if (addrTimes) history->time[index]= history->time[r];
        if (addrCycles) history->ncyc[index]= history->ncyc[r];
      }
      index++;
    }
  }
  if (index<nRecords)
    YWarning("some DMI-style records in PDB file were lost");
  history->nRecords= nRecords= index;

  /* try to fix nextAddress in parent
     to reflect actual end of non-record data */
  index= file->nextAddress;
  for (r=0 ; r<nRecords ; r++) {
    if (history->ifile[r]) break;
    if (offsets[r]<index) index= offsets[r];
  }
  file->nextAddress= index;

  /* check to see whether record data structure can be removed
     from structTables */
  if (baseName) {
    RemoveStruct(file, baseName);
    RemoveStruct(child, baseName);
  } else if (nRecords && blocksTable.nItems==1 &&
             HashFind(&blocksTable, "__@history", 0L)) {
    RemoveStruct(file, "__");
    RemoveStruct(child, "__");
  }

  /* Position file to first record.  */
  JumpRecord(history, 0);
}

static void GrabTorN(HistoryInfo *history, IOStream *child,
                     long timeOffset, long ncycOffset)
{
  if (timeOffset>=0 || ncycOffset>=0) {
    double *times= (timeOffset>=0)?
      history->time+history->nRecords-nBlocks : 0;
    long *ncycs= (ncycOffset>=0)?
      history->ncyc+history->nRecords-nBlocks : 0;
    long i;
    /* should child->blockSize be reduced to, say, 0x0fff (4096) here?
       -- big blocks might slow it down a lot */
    for (i=0 ; i<nBlocks ; i++) {
      if (times)
        YRead(&times[i], blocksAddrs[i]+timeOffset,
              child->structList[5], 1L, (Strider *)0);
      if (ncycs)
        YRead(&ncycs[i], blocksAddrs[i]+ncycOffset,
              child->structList[3], 1L, (Strider *)0);
    }
  }
}

static void RemoveStruct(IOStream *file, char *baseName)
{
  if (HashFind(&file->structTable, baseName, 0L)) {
    StructDef **structList= file->structList;
    StructDef *base= structList[hashIndex];
    long r;
    /* only safe to remove if no outstanding references */
    if (!base->references) {
      long nItems= file->structTable.nItems-1;
      base= file->structList[hashIndex];
      file->structList[hashIndex]= 0;
      for (r=hashIndex ; r<nItems ; r++) structList[r]= structList[r+1];
      HashRemove(&file->structTable, hashIndex);
      Unref(base);
    }
  }
}

static int FindSymtab(IOStream *file, long *addrSymt, long *addrChrt)
{
  char *token;

  InitBuffer(file, 280L, 0L);

  token= ReadUntil(ANY_NEWLINE);
  if (!token || !(token= strtok(token, " "))) return ClearBuffer(1);

  if (strcmp(token, HEADTOK) == 0) {
    if (!(token= ReadBlock(1L)) ||
        !(token= ReadBlock((long)token[0]-1L))) return ClearBuffer(2);
    token= ReadUntil(ANY_NEWLINE);  /* skip exponent biases */

  } else if (strcmp(token, OLDHEADTOK) == 0) {
    token = strtok(0, " ");
    if (!token) return ClearBuffer(2);

  } else {
    return ClearBuffer(1);
  }

  /* chart and symtab addresses are stored with "%ld\001%ld\001\n" */
  token= ReadUntil(ANY_NEWLINE);
  if (!token) return ClearBuffer(2);
  if (TokenAsLong(token, addrChrt)) return ClearBuffer(2);
  if (TokenAsLong(0, addrSymt)) return ClearBuffer(2);

  return ClearBuffer(0);
}

/* WARNING-- FindRecords depends on fastestFirst being properly set.  */
static int FindRecords(IOStream *file, long addrSymt,
                       long *addrTimes, long *addrCycles, int style)
{
  char *line, *token;
  int mask, gotMask;
  long minAddr=0, addr=0, n=0, offset=0;
  long need=-1;

  InitBuffer(file, 4096L, addrSymt);

  firstLength= nSeq= nSeq2= nTimes= nCycles= 0;
  firstAddr= numbAddr= timeAddr= ncycAddr= 0;

  /* two consecutive newlines marks end of symtab */
  gotMask= 0;
  if (!addrTimes) gotMask|= 4;
  if (!addrCycles) gotMask|= 8;
  while((line= ReadUntil(ANY_NEWLINE)) && line[0]) {
    token= strtok(line, "\001");
    if (!token) continue;
    if (style) {
      /* DMI style history data */
      mask= DMIcheckSymbol(token);
      gotMask|= mask;
      if (gotMask==0xf) break;

    } else if (nBlocks && HashFind(&blocksTable, token, 0L)) {
      /* PDB style history data */
      char *dataName= token;
      if (!strtok(0, "\001")) continue;
      token= strtok(0, "\001");
      if (!token || strtol(token, (char**)0, 10)<=0) continue;
      token= strtok(0, "\001");
      if (!token || (addr=strtol(token, (char**)0, 10))<0) continue;
      if (!HashFind(&file->dataTable, dataName, 0L)) {
        /* @history may have been stripped, need to check (sigh) */
        long i, len= strlen(dataName)-1;
        for (i=7 ; i>=0 && len>0 ; i--,len--)
          if (dataName[len]!=atHistory[i]) break;
        if (i<0 &&
            HashFind(&file->dataTable, dataName, len+1)) dataName= 0;
      } else {
        dataName= 0;
      }
      if (!dataName) {
        if (n) {
          if (addr<minAddr) minAddr= addr;
          if (addr!=file->addresses[hashIndex]+offset) continue;
        } else {
          need= file->dataTable.nItems;
          offset= addr-file->addresses[hashIndex];
          minAddr= addr;
        }
        n++;
      } else if (blocksTable.nItems==1 && !(yPDBclose&010)) {
        /* records are not burst */
        need= 1;
        minAddr= addr;
        n++;
      }
    }
  }

  if (style) {
    /* DMI style history data */
    ClearBuffer(0);
    if (!HasDMIrecords()) return 0;

    /* grab dmiNumber and dmiName */
    GrabDMIData(file, firstAddr, firstLength, numbAddr);

    /* timeAddr and ncycAddr have been updated */
    if (addrTimes) *addrTimes= timeAddr;
    if (addrCycles) *addrCycles= ncycAddr;

    return dmiNumber<1;

  } else if (n==need) {
    /* PDB style history data -- now positioned at extras section,
       look for Blocks: extra to check for consistency */
    int dateBugCheck= 0;
    while ((line= ReadUntil(ANY_NEWLINE)) && (line[0] || dateBugCheck)) {
      if (!line[0]) {
        dateBugCheck= 0;
        if (!(line= ReadUntil(ANY_NEWLINE)) || !line[0]) break;
      }
      if (strncmp(line, "Version:", 8L)==0) {
        dateBugCheck= 1;
      } else if (strncmp(line, "Blocks:", 7L)==0) {
        if (BlocksVerify()) break;
        if (!nBlocks) {
          /* this file must have only one record */
          nBlocks= 1;
          blocksAddrs= (long *)p_realloc(blocksAddrs, sizeof(long));
          blocksAddrs[0]= minAddr;
        }
        return 0;
      }
    }
    return 1;
  } else {
    YWarning("history family record variable(s) missing");
    return 1;
  }
}

static void GrabDMIData(IOStream *file, long firstAddr, long firstLength,
                        long numbAddr)
{
  char c, *seqNum;
  int i, dmiDigits;

  CheckDMIScratch(firstLength);

  YRead(dmiName, firstAddr,
        file->structList[0], firstLength, (Strider *)0);

  /* last 5 characters of dmiName must be record sequence number */
  dmiName[firstLength]= '\0';
  dmiDigits= strlen(dmiName);
  seqNum= dmiName+dmiDigits;
  if (dmiDigits>5) dmiDigits= 5;
  for (i=0 ; i<dmiDigits && seqNum>dmiName ; i++) {
    c= *(--seqNum);
    if (c<'0' || c>'9') {
      seqNum++;
      break;
    }
  }

  YRead(&dmiNumber, numbAddr, file->structList[3], 1, (Strider *)0);

  if (i==5) {
    if (timeAddr && nTimes<dmiNumber) timeAddr= 0;
    if (ncycAddr && nCycles<dmiNumber) ncycAddr= 0;
    dmiBase= seqNum-dmiName;
    dmiFirst= strtol(seqNum, (char **)0, 10);
  } else {
    timeAddr= ncycAddr= 0;
    nSeq= nSeq2= 0;
    dmiNumber= -1;
  }
}

static int DMIcheckSymbol(char *dataName)
{
  char *token, *typeName;
  int mask, nDims;
  long dataAddr, /*origin,*/ length, lDims[2];

  /* first token is variable name */
  if (!dataName) return 0;

  /* skip anything except the four variables of interest */
  mask= IsDMIvariable(dataName);
  if (!mask) return 0;

  /* second token is type name -- pointer if followed by any *'s */
  typeName= strtok(0, " \001");
  if (!typeName) return 0;

  if (mask&1) {
    if (strcmp(typeName, "char")) return 0;
  } else if (mask&2 || mask&8) {
    if (strcmp(typeName, "long")) return 0;
  } else if (mask&4) {
    if (strcmp(typeName, "double")) return 0;
  }

  /* third token is total number of items of specified type */
  token= strtok(0, "\001");
  if (!token || strtol(token, (char**)0, 10)<=0) return 0;

  /* fourth token is data address (for pointers, this is address of the
     first pointee) */
  token= strtok(0, "\001");
  if (!token || (dataAddr=strtol(token, (char**)0, 10))<0) return 0;

  if (mask&1) firstAddr= dataAddr;
  else if (mask&2) numbAddr= dataAddr;
  else if (mask&4) timeAddr= dataAddr;
  else if (mask&8) ncycAddr= dataAddr;

  nDims= 0;
  while ((token= strtok(0, "\001"))) {
    /* origin= strtol(token, (char**)0, 10); */
    token= strtok(0, "\001");
    if (!token || (length=strtol(token, (char**)0, 10))<=0) {
      nDims= 0;
      break;
    }
    if (nDims<2) lDims[nDims]= length;
    nDims++;
  }

  if (mask&1) {
    if (nDims!=2) return 0;
    /* firstLength and nSeq reversed if fastestFirst -- see HasDMIrecords */
    firstLength= lDims[1];
    nSeq= lDims[0];
  } else if (mask&2) {
    if (nDims!=1) return 0;
    nSeq2= lDims[0];
  } else if (mask&4) {
    if (nDims!=1) return 0;
    nTimes= lDims[0];
  } else if (mask&8) {
    if (nDims!=1) return 0;
    nCycles= lDims[0];
  }

  return mask;
}

static int HasDMIrecords(void)
{
  /* assure that firstAddr, firstLength, and numbAddr have been set to
     legal FirstRecordNames and NumberOfRecords variables */
  if (nSeq2) {
    if (nSeq==nSeq2) return 1;
    if (firstLength==nSeq2 && fastestFirst) {
      firstLength= nSeq;
      nSeq= nSeq2;
      return 1;
    }
  }
  return 0;
}

static int IsDMIvariable(char *dataName)
{
  if (strcmp(dataName, "FirstRecordNames")==0) return 1;
  if (strcmp(dataName, "NumberOfRecords")==0) return 2;
  if (strcmp(dataName, "times")==0) return 4;
  if (strcmp(dataName, "ncycs")==0) return 8;
  return 0;
}

static long IsDMIrecord(char *dataName)
{
  char *token;
  long recNumber;

  if (strncmp(dataName, dmiName, dmiBase)) return -1;

  token= dataName+dmiBase;
  if (strlen(token)!=5) return -1;
  while (token[0]>='0' && token[0]<='9') token++;
  if (token[0]) return -1;

  recNumber= strtol(dataName+dmiBase, (char **)0, 10);
  if (recNumber<dmiFirst || recNumber>=dmiFirst+dmiNumber) return -1;
  return recNumber;
}

static char *FindAddresses(IOStream *file, long addrSymt, char *baseName,
                           int style)
{
  long recNumber, *offset= 0;
  char *line, *token, *dataName, *typeName;
  long dataAddr;
  int goofCount= 0;
  int gotName= 0;

  /* NB- When called on the first file of a family, baseName==0; this
         rountine returns it.  Also, file is always the history child.
         For style!=0, baseName is just a non-zero pointer for subsequent
         files in a family; its value is (char *)file.  */

  InitBuffer(file, 4096L, addrSymt);

  /* two consecutive newlines marks end of symtab */
  while ((line= ReadUntil(ANY_NEWLINE)) && line[0]) {

    /* first token is variable name */
    dataName= strtok(line, "\001");
    if (!dataName) continue;

    if (style) {
      recNumber= IsDMIrecord(dataName);
      if (recNumber<0) continue;

      /* second token is type name -- check that it is expected baseName */
      typeName= strtok(0, " \001");
      if (!typeName) continue;

      if (baseName) {
        if (strcmp(typeName, baseName)) continue;

      } else if (HashFind(&file->structTable, typeName, 0L)) {
        /* must build child dataTable now by transfering from StructDef */
        StructDef *base= file->structList[hashIndex];
        if (BurstRecord(file, base)) return 0;

        baseName= StructName(base);

      } else {
        continue;
      }

      /* third token is total number of items -- check for 1 */
      token= strtok(0, "\001");
      if (!token || strtol(token, (char**)0, 10)!=1) continue;

      /* fourth token is record address */
      token= strtok(0, "\001");
      if (!token || (dataAddr=strtol(token, (char**)0, 10))<0) continue;

      /* on first pass, extend record lists as required */
      if (!offset) {
        HistoryInfo *history= file->history;
        long r, nRecords= history->nRecords; /* remember initial nRecords */
        /* times and ncycs lists already found by RDextras */
        int flags= (timeAddr>=0? 1 : 0) | (ncycAddr>=0? 2 : 0);
        for (r=0 ; r<dmiNumber ; r++) {
          /* add records at disk address 0 for now -- fixed in a few lines */
          if (AddRecord(history, flags, 0.0, 0L, 0L))
            YErrorIO("failed to add record scanning DMI-style PDB file");
        }
        offset= history->offset+nRecords;
      }

      /* set correct record address now */
      offset[recNumber-dmiFirst]= dataAddr;

    } else {
      /* PDB style history variables */
      if (!HashFind(&blocksTable, dataName, 0L)) continue;
      if (!baseName) {
        /* first file in family -- construct child symbol table */
        goofCount= GrabSymbol(dataName, file, goofCount, blocksAddrs[0],
                              &baseName);
        if (baseName) gotName= 1;
      }
    }
  }

  ClearBuffer(0);

  if (!style) {
    /* add records for PDB style history data */
    HistoryInfo *history= file->history;
    long r;
    int flags= (CheckTorN(file,"time",5L)? 1 : 0) |
               (CheckTorN(file,"ncyc",3L)? 2 : 0);
    for (r=0 ; r<nBlocks ; r++) {
      /* add all records at zero time and ncyc for now */
      if (AddRecord(history, flags, 0.0, 0L, blocksAddrs[r]))
        YErrorIO("failed to add record scanning DMI-style PDB file");
    }
  }

  return (style||gotName)? baseName : 0;
}

static int CheckTorN(IOStream *file, char *name, long type)
{
  return HashFind(&file->dataTable, name, 0L) &&
    file->structList[type]==file->types[hashIndex].base &&
    !file->types[hashIndex].dims;
}

static int BurstRecord(IOStream *file, StructDef *base)
{
  long r, nItems= base->table.nItems;
  char **names= base->table.names;
  Member *members= base->members;
  long *offsets= base->offsets;

  /* transfer record structure to history child */
  if (nItems<=0) return 1;
  yPDBclose|= 010;
  for (r=0 ; r<nItems ; r++) {
    if (AddVariable(file, offsets[r],
                    names[r], members[r].base, members[r].dims))
      YErrorIO("failed to add variable scanning DMI-style PDB file");
  }

  return 0;
}

static int VerifyRecType(IOStream *file, long addrChrt,
                         char *baseName, long baseSize)
{
  char *token, *typeName;

  InitBuffer(file, 4096L, addrChrt);

  while ((typeName= ReadUntil(SOH_OR_NEWLINE)) && typeName[0]!='\002') {
    if (strcmp(typeName, baseName)) continue;

    token= ReadUntil(SOH_OR_NEWLINE);
    if (token &&
        strtol(token, (char **)0, 10) == baseSize) return ClearBuffer(0);
    break;
  }

  return ClearBuffer(1);
}

/*--------------------------------------------------------------------------*/

/*                              WRITE ROUTINES                              */

/*--------------------------------------------------------------------------*/

static char *EncodePrimitives(IOStream *file, char *buffer)
{
  StructDef *shortDef= file->structList[1];
  StructDef *intDef= file->structList[2];
  StructDef *longDef= file->structList[3];
  StructDef *floatDef= file->structList[4];
  StructDef *doubleDef= file->structList[5];
  StructDef *ptrDef;
  unsigned char *buf= (unsigned char *)buffer;

  /* byte count + 1 of what follows (until exponent biases) */
  *(buf++)= (unsigned char)(24+floatDef->size+doubleDef->size);

  if (HashFind(&file->structTable, memberPointer, 0L))
    ptrDef= file->structList[hashIndex];
  else
    ptrDef= file->structList[3];

  *(buf++)= (unsigned char)(ptrDef->size);
  *(buf++)= (unsigned char)(shortDef->size);
  *(buf++)= (unsigned char)(intDef->size);
  *(buf++)= (unsigned char)(longDef->size);
  *(buf++)= (unsigned char)(floatDef->size);
  *(buf++)= (unsigned char)(doubleDef->size);

  buf+= EncodeOrder(buf, shortDef->order, 0L);
  buf+= EncodeOrder(buf, intDef->order, 0L);
  buf+= EncodeOrder(buf, longDef->order, 0L);

  buf+= EncodeOrder(buf, floatDef->order, floatDef->size);
  buf+= EncodeOrder(buf, doubleDef->order, doubleDef->size);

  *(buf++)= (unsigned char)(floatDef->size<<3);
  *(buf++)= (unsigned char)(floatDef->fpLayout->expSize);
  *(buf++)= (unsigned char)(floatDef->fpLayout->manSize);
  *(buf++)= (unsigned char)(floatDef->fpLayout->sgnAddr);
  *(buf++)= (unsigned char)(floatDef->fpLayout->expAddr);
  *(buf++)= (unsigned char)(floatDef->fpLayout->manAddr);
  *(buf++)= (unsigned char)(floatDef->fpLayout->manNorm);

  *(buf++)= (unsigned char)(doubleDef->size<<3);
  *(buf++)= (unsigned char)(doubleDef->fpLayout->expSize);
  *(buf++)= (unsigned char)(doubleDef->fpLayout->manSize);
  *(buf++)= (unsigned char)(doubleDef->fpLayout->sgnAddr);
  *(buf++)= (unsigned char)(doubleDef->fpLayout->expAddr);
  *(buf++)= (unsigned char)(doubleDef->fpLayout->manAddr);
  *(buf++)= (unsigned char)(doubleDef->fpLayout->manNorm);

  /* poke byte count back into 1st byte */
  buffer[0]= (int)(buf - (unsigned char *)buffer);

  /* write exponent biases in ASCII */
  sprintf((char *)buf, "%ld\001%ld\001\012",
          floatDef->fpLayout->expBias, doubleDef->fpLayout->expBias);

  return (char *)buf + strlen((char *)buf);
}

static long EncodeOrder(unsigned char *buf, int order, long size)
{
  if (size) {  /* floating point order */
    int w, b;
    if (order>0) {
      for (w=0 ; w<size ; w+=order)
        for (b=order ; b>0 ; b--) *(buf++)= w+b;
    } else if (order<0) {
      for (w=size-1 ; w>=0 ; w+=order)
        for (b=1 ; b<=-order ; b++) *(buf++)= w+b;
    } else {
      for (w=0 ; w<size ; w++) *(buf++)= 1;
    }
    return size;

  } else {     /* integer order */
    *buf= order>=0? 1 : 2;
    return 1;
  }
}

/*--------------------------------------------------------------------------*/

static char *dimFormat[]= {",%ld", "(%ld"};
static char *odimFormat[]= {",%ld:%ld", "(%ld:%ld"};

static int WRchart(IOStream *file)
{
  long i, nItems= file->structTable.nItems;
  char **names= file->structTable.names;
  StructDef **structList= file->structList;
  HistoryInfo *history= file->history;
  StructDef *base;

  int notOK= 0;

  /* fill in basic primitives in order used by PDBLib */
  if (HashFind(&file->structTable, memberPointer, 0L))
    notOK|= DoStructName("*", structList[hashIndex]->size, 1);
  else
    notOK|= DoStructName("*", structList[3]->size, 1);
  notOK|= DoStructName("short", structList[1]->size, 1);
  notOK|= DoStructName("int", structList[2]->size, 1);
  notOK|= DoStructName("long", structList[3]->size, 1);
  notOK|= DoStructName("float", structList[4]->size, 1);
  notOK|= DoStructName("double", structList[5]->size, 1);
  notOK|= DoStructName("char", structList[0]->size, 1);

  /* loop on structure definitions */
  for (i=6 ; i<nItems ; i++) {
    base= structList[i];
    notOK|= WRstructure(names, names[i], base->size, base->table.nItems,
                        base->table.names, base->members, base->offsets);
  }

  /* history records also treated as a data structure --
     NOTE that file MUST be the history child in this case.  */
  if (history && (!(yPDBclose&2) || !(yPDBclose&010))) {
    long n= file->dataTable.nItems;
    long recordSize= history->recordSize;
    int recordAlign= 1;
    if (HashFind(&file->structTable, "__", 0L)) {
      base= structList[hashIndex];
      nItems= base->table.nItems;
      for (i=0 ; i<nItems ; i++) {
        if (i>=n || base->offsets[i]!=file->addresses[i] ||
            base->members[i].base!=file->types[i].base ||
            base->members[i].number!=file->types[i].number) break;
      }
      if (i<nItems || i!=n)
        YWarning("duplicate use of struct name __ in PDB file");
    }
    /* The record size is a knotty problem, since it must be a multiple
       of the record alignment in order to follow the PDB format rules for
       struct definitions.  Unfortunately, recordAlign had to be taken as
       the most restrictive alignment of any struct at the time the first
       record was created, rather than the PDB-struct-like definition of
       the most restrictive element actually present in the structure.
       (In fact, if dataAlign!=0, it is impossible to correctly write a
       PDB file with history records.)  Since the recordSize has NOT been
       adjusted to be a multiple of the recordAlign, we can compute the
       correct alignment, then adjust recordSize here.  */
    nItems= file->dataTable.nItems;
    for (i=0 ; i<nItems ; i++)
      if (file->types[i].base->alignment > recordAlign)
        recordAlign= file->types[i].base->alignment;
    recordSize= AlignAdjust(recordSize, recordAlign);
    notOK|= WRstructure(names, "__", recordSize, n,
                        file->dataTable.names, file->types, file->addresses);
  }

  *(nextByte++)= '\002';   /* "\002\n" marks end of whole chart */
  *(nextByte++)= '\012';

  return notOK;
}

static int WRstructure(char **names, char *name, long size, long nMembers,
                       char **mNames, Member *members, long *offsets)
{
  int notOK= 0;
  StructDef *mBase;
  char *typeName;
  long j, expectedOffset= 0;

  if (nMembers<=0) return 0;   /* don't touch primitives here */

  if (DoStructName(name, size, 0)) return 1;

  /* loop is on members of structure */
  for (j=0 ; j<nMembers ; j++) {
    mBase= members[j].base;
    if (mBase->Convert!=&PDBconvert) typeName= names[mBase->index];
    else typeName= memberPointer;
    typeName= FlopIntegerAndInt(typeName);
    if (CheckBuffer(5+strlen(typeName)+strlen(mNames[j]))) {
      YWarning("garbled struct member skipped closing PDB file");
      return notOK= 1;
    }
    sprintf(nextByte, "%s %s", typeName, mNames[j]);
    nextByte+= strlen(nextByte);

    /* handle dimensions of one member */
    PutDimList(members[j].dims, yPDBclose&1);
    *(nextByte++)= '\001';      /* marks end of member */

    expectedOffset= AlignAdjust(expectedOffset, mBase->alignment);
    if (expectedOffset!=offsets[j]) {
      YWarning("scrambled struct members closing PDB file -- data corrupted");
      notOK= 1;
    }
    expectedOffset+= mBase->size*members[j].number;
  }
  *(nextByte++)= '\012';        /* marks end of structure */

  return notOK;
}

static void PutDimList(Dimension *dims, int fastest1st)
{
  int firstPass;
  if (!dims) return;
  if (!fastest1st) {
    firstPass= 1;
    do {
      Put1Dim(dims, firstPass);
      firstPass= 0;
    } while ((dims=dims->next));
    *(nextByte++)= ')';
  } else {
    firstPass= (dims->next==0);
    if (!firstPass) PutDimList(dims->next, 256);
    Put1Dim(dims, firstPass);
    if (!(fastest1st&256)) *(nextByte++)= ')';
  }
}

static void Put1Dim(Dimension *dims, int firstPass)
{
  if (dims->origin!=1L) {
    CheckBuffer(47);
    sprintf(nextByte, odimFormat[firstPass],
            dims->origin, dims->number);
    nextByte+= strlen(nextByte);
  } else {
    CheckBuffer(26);
    sprintf(nextByte, dimFormat[firstPass], dims->number);
    nextByte+= strlen(nextByte);
  }
}

static int DoStructName(char *name, long size, int putEnd)
{
  if (CheckBuffer(25+strlen(name))) {
    YWarning("impossibly long struct name skipped closing PDB file");
    return 1;
  }
  sprintf(nextByte, "%s\001%ld\001", FlopIntegerAndInt(name), size);
  nextByte+= strlen(nextByte);
  if (putEnd) *(nextByte++)= '\012';    /* marks end of structure */
  return 0;
}

/*--------------------------------------------------------------------------*/

static char *recordVar= 0;

static int WRsymtab(IOStream *file)
{
  HistoryInfo *history= file->history;
  IOStream *parent= history? history->parent : file;
  StructDef *base;
  long i, dir_index;

  int notOK= 0;

  if (!history || history->copyParent) {
    /* there are non-record variables in this file */
    long nItems= parent->dataTable.nItems;
    char **names= parent->dataTable.names;
    Member *members= parent->types;
    long *addresses= parent->addresses;
    long offset= parent->offset;
    char *typeName, **tNames= parent->structTable.names;

    if (HashFind(&parent->structTable, "Directory", 0L))
      dir_index = hashIndex;
    else
      dir_index = -1;

    for (i=0 ; i<nItems ; i++) {
      base= members[i].base;
      if (base->Convert!=&PDBconvert) typeName= tNames[base->index];
      else typeName= variablePointer;
      if (!has_dirs && base->index==dir_index) has_dirs = 1;
      notOK|= WRsymbol(names[i], typeName, members[i].number,
                       addresses[i]+offset, members[i].dims, 0L);
    }
  }

  /* NOTE that file MUST be the history child in this case.  */
  if (history) {
    long *offset= history->offset;
    Dimension *dims;

    if (!offset) {
      notOK = 1;

    } else if (!(yPDBclose&2)) {
      /* already computed dmiFirst and dmiNumber in WRhistory */
      char recordName[8];
      for (i=0 ; i<dmiNumber ; i++) {
        sprintf(recordName, "__%05ld", dmiFirst+i);
        notOK|= WRsymbol(recordName, "__", 1L,
                         offset[dmiFirst+i], (Dimension *)0, 0L);
      }

      /* firstAddr, numbAddr, timeAddr, and ncycAddr have already been
         set by WRhistory */
      dims= tmpDims;
      tmpDims= 0;
      FreeDimension(dims);
      tmpDims= NewDimension(firstLength, 1L, tmpDims);
      tmpDims= NewDimension(1L, 1L, tmpDims);
      if (firstAddr)
        notOK|= WRsymbol("FirstRecordNames", "char", firstLength,
                         firstAddr, tmpDims, 0L);
      dims= tmpDims->next;
      tmpDims->next= 0;
      FreeDimension(dims);
      if (numbAddr)
        notOK|= WRsymbol("NumberOfRecords","long", 1L, numbAddr, tmpDims, 0L);
      if (dmiNumber) {
        tmpDims->number= dmiNumber;
        if (timeAddr)
          notOK|= WRsymbol("times","double",dmiNumber, timeAddr, tmpDims, 0L);
        if (ncycAddr)
          notOK|= WRsymbol("ncycs", "long", dmiNumber, ncycAddr, tmpDims, 0L);
      }

    } else {
      /* history data is to be written in PDB style */
      Dimension *dims= tmpDims;
      tmpDims= 0;
      FreeDimension(dims);
      if (yPDBclose&010) {
        /* write each variable individually */
        char *name= 0;
        IOStream *child= history->child;
        long nItems= child->dataTable.nItems;
        char **names= child->dataTable.names;
        Member *members= child->types;
        long *addresses= child->addresses;
        char *typeName, **tNames= child->structTable.names;

        if (HashFind(&child->structTable, "Directory", 0L))
          dir_index = hashIndex;
        else
          dir_index = -1;

        if (recordVar) {
          name= recordVar;
          recordVar= 0;
          p_free(name);
        }

        for (i=0 ; i<nItems ; i++) {
          base= members[i].base;
          if (base->Convert!=&PDBconvert) typeName= tNames[base->index];
          else typeName= variablePointer;
          if (yPDBclose&4) name= recordVar= p_strncat(names[i], "@history", 0);
          else name= names[i];
          tmpDims= NewDimension(1L, 1L, Ref(members[i].dims));
          if (!has_dirs && base->index==dir_index) has_dirs = 1;
          notOK|= WRsymbol(name, typeName, members[i].number,
                           addresses[i]+offset[dmiFirst], tmpDims, 0L);
          dims= tmpDims;
          tmpDims= 0;
          FreeDimension(dims);
          if (yPDBclose&4) {
            recordVar= 0;
            p_free(name);
          }
        }

      } else {
        /* bundle each record into a single struct instance
           -- name is __@history whether or not yPDBclose&4 is set */
        tmpDims= NewDimension(1L, 1L, tmpDims);
        notOK|= WRsymbol("__@history", "__", 1,
                         offset[dmiFirst], (Dimension *)0, 0L);
      }
    }
  }

  CheckBuffer(1L);
  *(nextByte++)= '\012';
  return notOK;
}

static int WRsymbol(char *name, char *typeName, long number, long address,
                    Dimension *dims, long nlen)
{
  int notOK= 0;
  if (!nlen) nlen = strlen(name);
  typeName = FlopIntegerAndInt(typeName);

  if (CheckBuffer(45+nlen+strlen(typeName))) {
    YWarning("impossibly long type+variable name skipped closing PDB file");
    notOK= 1;
  } else {
    nextByte[0] = '\0';
    strncat(nextByte, name, nlen);
    nextByte += nlen;
    sprintf(nextByte, "\001%s\001%ld\001%ld\001", typeName, number, address);
    nextByte+= strlen(nextByte);
    if (!(yPDBclose&1)) {
      for ( ; dims ; dims=dims->next) {
        CheckBuffer(43);
        sprintf(nextByte, "%ld\001%ld\001", dims->origin, dims->number);
        nextByte+= strlen(nextByte);
      }
    } else {
      if (dims) Put102Dims(dims);
    }
    *(nextByte++)= '\012';
  }

  return notOK;
}

static void Put102Dims(Dimension *dims)
{
  if (dims->next) Put102Dims(dims->next);
  CheckBuffer(43);
  sprintf(nextByte, "%ld\001%ld\001", dims->origin, dims->number);
  nextByte+= strlen(nextByte);
}

/*--------------------------------------------------------------------------*/

extern char *y__pdb_date;
extern int y__pdb_version;

char *y__pdb_date= 0;   /* "Sun Dec  7 06:00:00 1941" --- exactly 24 chars */
int y__pdb_version= 11;

static int WRextras(IOStream *file)
{
  StructDef **structList= file->structList;
  long i, nItems, size;
  int alignment, order, pdbOrder;
  char *typeName;
  FPLayout *fpLayout;
  int notOK= 0;
  time_t n_time;

  CheckBuffer(155+ 24 /* strlen(y__pdb_date) */ + 10);

  /* The default index origin is 1L.  */
  sprintf(nextByte, "Offset:%ld\012", 1L);
  nextByte+= strlen(nextByte);

  /* alignments come from the structTable */
  strcpy(nextByte, "Alignment:");
  nextByte+= 10;
  *(nextByte++)= (unsigned char)structList[0]->alignment;
  if (HashFind(&file->structTable, memberPointer, 0L))
    *(nextByte++)= (unsigned char)structList[hashIndex]->alignment;
  else
    *(nextByte++)= (unsigned char)structList[3]->alignment;
  *(nextByte++)= (unsigned char)structList[1]->alignment;
  *(nextByte++)= (unsigned char)structList[2]->alignment;
  *(nextByte++)= (unsigned char)structList[3]->alignment;
  *(nextByte++)= (unsigned char)structList[4]->alignment;
  *(nextByte++)= (unsigned char)structList[5]->alignment;
  *(nextByte++)= '\012';

  /* Set the structAlignment.  */
  sprintf(nextByte, "Struct-Alignment:%d\012",
          file->structAlign>1? file->structAlign : 0);
  nextByte+= strlen(nextByte);

  /* version number and date don't mean much */
  p_free(y__pdb_date);
  n_time = time((void *)0);
  y__pdb_date= p_strcpy(strtok(ctime(&n_time),"\n"));
  sprintf(nextByte, "Version:%d|%s\012", y__pdb_version,
          y__pdb_date? y__pdb_date : "");
  nextByte+= strlen(nextByte);

  /* Major-Order must come before Blocks for PDBLib */
  sprintf(nextByte, "Major-Order:%s\012", (yPDBclose&1)? "102" : "101");
  nextByte+= 16;
  sprintf(nextByte, "Has-Directories:%d\012",
          HashFind(&file->dataTable, "/", 1L)?1:0);
  nextByte+= 18;

  /* blocks information optional as history mechanism */
  strcpy(nextByte, "Blocks:\012");
  nextByte+= 8;
  /* already computed dmiFirst and dmiNumber in WRhistory */
  if (file->history && dmiNumber>1 && yPDBclose&2) {
    HistoryInfo *history= file->history;
    long *addrs= history->offset+dmiFirst;
    if (yPDBclose&010) {
      /* write each variable individually */
      IOStream *child= history->child;
      char **names= child->dataTable.names;
      Member *members= child->types;
      long j, number, *offset= child->addresses;
      nItems= child->dataTable.nItems;
      for (j=0 ; j<nItems ; j++) {
        CheckBuffer(strlen(names[j])+29);
        strcpy(nextByte, names[j]);
        nextByte+= strlen(nextByte);
        if (yPDBclose&4) {
          strcpy(nextByte, "@history");
          nextByte+= 8;
        }
        sprintf(nextByte, "\001%ld", dmiNumber);
        nextByte+= strlen(nextByte);
        number= members[j].number;
        for (i=0 ; i<dmiNumber ; i++) {
          CheckBuffer(55);   /* 10 bytes for Casts extra... */
          sprintf(nextByte, " %ld %ld", addrs[i]+offset[j], number);
          nextByte+= strlen(nextByte);
        }
        strcpy(nextByte, "\012");
        nextByte++;
      }

    } else {
      /* bundle each record into a single struct instance
         -- name is __@history whether or not yPDBclose&4 is set */
      sprintf(nextByte, "__@history\001%ld", dmiNumber);
      nextByte+= strlen(nextByte);
      for (i=0 ; i<dmiNumber ; i++) {
        CheckBuffer(35);   /* 10 bytes for Casts extra... */
        sprintf(nextByte, " %ld 1", addrs[i]);
        nextByte+= strlen(nextByte);
      }
      strcpy(nextByte, "\012");
      nextByte++;
    }
  }
  strcpy(nextByte, "\002\012");
  nextByte+= 2;

  /* cast information not used */
  strcpy(nextByte, "Casts:\012\002\012");
  nextByte+= 9;

  /* any additional primitive types are written here */
  CheckBuffer(18);
  strcpy(nextByte, "Primitive-Types:\012");
  nextByte+= 17;
  nItems= file->structTable.nItems;
  for (i=6 ; i<nItems ; i++) {
    if (structList[i]->table.nItems ||
        structList[i]->Convert==&PDBconvert ||
        (i<8 && structList[i]->references<1)) continue;
    typeName= FlopIntegerAndInt(file->structTable.names[i]);
    size= structList[i]->size;
    alignment= structList[i]->alignment;
    order= structList[i]->order;
    fpLayout= structList[i]->fpLayout;
    if (fpLayout || order==0) pdbOrder= -1;
    else if (order>0) pdbOrder= 1;
    else pdbOrder= 2;

    if (CheckBuffer(68+strlen(typeName))) {
      YWarning("impossibly long primitive name skipped closing PDB file");
      notOK= 1;
      continue;
    }
    sprintf(nextByte, "%s\001%ld\001%d\001%d\001",
            typeName, size, alignment, pdbOrder);
    nextByte+= strlen(nextByte);

    if (order && (fpLayout || order>1 || order<-1)) {
      CheckBuffer(6);
      strcpy(nextByte, "ORDER\001");
      nextByte+= 6;
      if (order>0) {
        int w, b;
        for (w=size-1 ; w>=0 ; w-=order) {
          for (b=1 ; b<=order ; b++) {
            CheckBuffer(21);
            sprintf(nextByte, "%d\001", w+b);
            nextByte+= strlen(nextByte);
          }
        }
      } else if (order<0) {
        int w, b;
        for (w=0 ; w<size ; w-=order) {
          for (b=-order ; b>0 ; b--) {
            CheckBuffer(21);
            sprintf(nextByte, "%d\001", w+b);
            nextByte+= strlen(nextByte);
          }
        }
      }
    } else {
      CheckBuffer(9);
      strcpy(nextByte, "DEFORDER\001");
      nextByte+= 9;
    }

    if (!order) {
      CheckBuffer(11);
      strcpy(nextByte, "NO-CONV\001");
      nextByte+= 8;
    } else if (fpLayout) {
      CheckBuffer(58);
      strcpy(nextByte, "FLOAT\001");
      nextByte+= 6;
      sprintf(nextByte, "%d\001", (int)((size<<3)&0xff));
      nextByte+= strlen(nextByte);
      sprintf(nextByte, "%d\001", (fpLayout->expSize)&0xff);
      nextByte+= strlen(nextByte);
      sprintf(nextByte, "%d\001", (fpLayout->manSize)&0xff);
      nextByte+= strlen(nextByte);
      sprintf(nextByte, "%d\001", (fpLayout->sgnAddr)&0xff);
      nextByte+= strlen(nextByte);
      sprintf(nextByte, "%d\001", (fpLayout->expAddr)&0xff);
      nextByte+= strlen(nextByte);
      sprintf(nextByte, "%d\001", (fpLayout->manAddr)&0xff);
      nextByte+= strlen(nextByte);
      sprintf(nextByte, "%d\001", (fpLayout->manNorm)&0xff);
      nextByte+= strlen(nextByte);
      sprintf(nextByte, "%ld\001", fpLayout->expBias);
      nextByte+= strlen(nextByte);
    } else {
      CheckBuffer(7);
      strcpy(nextByte, "FIX\001");
      nextByte+= 4;
    }

    CheckBuffer(2);
    *(nextByte++)= '\012';              /* mark end of one primitive type */
  }
  CheckBuffer(40);
  *(nextByte++)= '\002';   /* mark end of all primitive type declarations */
  *(nextByte++)= '\012';

  /* Set the structAlign for old Yorick versions.  */
  sprintf(nextByte, "Struct-Align:%d\012",
          file->structAlign>0? file->structAlign : 1);
  nextByte+= strlen(nextByte);

  /* file ends with two newlines (one is an extra?) */
  *(nextByte++)= '\012';
  *(nextByte++)= '\012';

  return notOK;
}

/*--------------------------------------------------------------------------*/

static int WRheader(IOStream *file, long newChart, long newSymtab)
{
  /* Need to read a few words near the beginning of the file to find
     the header.  */
  char *token, *buffer;
  long chartAddress, symtabAddress;
  long headerAddress;

  InitBuffer(file, 280L, 0L);
  token= ReadUntil(ANY_NEWLINE);
  if (!token || !(token= strtok(token, " "))) return ClearBuffer(1);

  if (strcmp(token, HEADTOK) == 0) {
    if (!(token= ReadBlock(1L)) ||
        !(token= ReadBlock((long)token[0]-1L)) ||
        !ReadUntil(ANY_NEWLINE)) return ClearBuffer(2);

  } else if (strcmp(token, OLDHEADTOK) != 0) {
    return ClearBuffer(2);
  }

  /* nextByte now points to buffer location containing current chart
     and symtab addresses.  Compute corresponding disk address.  */
  headerAddress= NextWRAddress(1);
  buffer= nextByte;

  /* read current value of chart address */
  token= ReadUntil(ANY_NEWLINE);
  if (!token || TokenAsLong(token, &chartAddress) || newChart!=chartAddress ||
      TokenAsLong(0, &symtabAddress) || newSymtab!=symtabAddress) {
    sprintf(buffer, "%ld\001%ld\001\012", newChart, newSymtab);
    YcWrite(file, buffer, headerAddress, strlen(buffer));
  }

  return ClearBuffer(0);
}

/*--------------------------------------------------------------------------*/

static int WRhistory(HistoryInfo *history)
{
  IOStream *child;
  StructDef **structList;
  long i, n;
  int *ifile, thisFile, alignment;
  char recordName[8];

  int notOK= 0;

  /* intialize state information required by WRchart and WRsymtab */
  firstLength= dmiFirst= dmiNumber= 0;
  firstAddr= numbAddr= timeAddr= ncycAddr= 0;
  if (!history) return 0;

  /* be sure this is last member of family */
  thisFile= history->fileNumber;  /* already checked that this is
                                     nFamily-1 in YclosePDB */

  n= history->nRecords;
  ifile= history->ifile;
  dmiNumber= 0;
  dmiFirst= -1;
  for (i=0 ; i<n ; i++) {
    if (ifile[i]==thisFile) {
      if (dmiFirst<0) dmiFirst= i;
      dmiNumber++;
    } else if (dmiNumber) {
      YWarning("skipped scrambled records closing PDB file");
      notOK= 1;
      break;
    }
  }
  if (!dmiNumber) dmiFirst= 0;

  if (!(yPDBclose&2)) {
    sprintf(recordName, "__%05ld", dmiFirst);
    firstLength= 8;

    child= history->child;
    structList= child->structList;

    /* write FirstRecordNames variable */
    if (child->dataAlign) alignment= child->dataAlign;
    else alignment= structList[0]->alignment;
    firstAddr= AlignAdjust(child->nextAddress, alignment);
    YWrite(recordName, firstAddr, structList[0], 8L, (Strider *)0);

    /* write NumberOfRecords variable */
    if (!child->dataAlign) alignment= structList[3]->alignment;
    numbAddr= AlignAdjust(child->nextAddress, alignment);
    YWrite(&dmiNumber, numbAddr, structList[3], 1L, (Strider *)0);

    /* write times variable */
    if (dmiNumber && history->time) {
      if (!child->dataAlign) alignment= structList[3]->alignment;
      timeAddr= AlignAdjust(child->nextAddress, alignment);
      YWrite(&history->time[dmiFirst], timeAddr,
             structList[5], dmiNumber, (Strider *)0);
    }

    /* write ncycs variable */
    if (dmiNumber && history->ncyc) {
      if (!child->dataAlign) alignment= structList[3]->alignment;
      ncycAddr= AlignAdjust(child->nextAddress, alignment);
      YWrite(&history->ncyc[dmiFirst], ncycAddr,
             structList[3], dmiNumber, (Strider *)0);
    }
  }

  return notOK;
}

/*--------------------------------------------------------------------------*/

static int TokenAsLong(char *token, long *result)
{
  token= strtok(token, "\001");
  if (!token) return 1;
  *result= strtol(token, (char **)0, 10);
  return 0;
}

/* These routines are designed to be able to read through the "line-oriented"
   sections of a PDB file.  Some of the quirks are required to deal with
   quirks in the PDB format itself...  */

/* state data for reader routines */
static char *firstByte, *thisByte, *lastByte;
static long bufSize, bufAddress, nextAddress;
static int bufAtEOF;
static IOStream *bufFile;

static void InitBuffer(IOStream *file, long size, long address)
{
  ClearBuffer(0);
  bufSize= size;
  nextByte= thisByte= lastByte= firstByte= p_malloc(bufSize+1);
  nextByte[0]= '\0';
  nextAddress= address;
  bufFile= file;
  bufAddress= address-1;  /* not equal to nextAddress */
  bufAtEOF= 0;
}

static int ClearBuffer(int value)
{
  /* failsafe freeing of firstByte scratch space --
     guards against asynchronous interrupts */
  char *scratch= firstByte;
  nextByte= thisByte= lastByte= firstByte= 0;
  p_free(scratch);
  bufFile= 0;
  return value;
}

static char *ReadUntil(char *delimiters)
{
  long len= 0;

  for (;;) {
    /* scan until one of the delimiters is found */
    len+= strcspn(&nextByte[len], delimiters);
    if (nextByte[len]) {
      nextByte[len++]= '\0';     /* overwrite the delimiter */
      break;
    }

    /* skip over any imbedded \0 bytes (a PDB quirk)
       Actually, none are known, but PDBLib would skip them...  */
    while (nextByte+len<lastByte) if (nextByte[++len]) break;

    /* If this is the end of the input buffer, reread starting from
       the address at the beginning of this line (nextAddress).  */
    if (nextByte+len>=lastByte && ReadBuffer()) return 0;
  }

  thisByte= nextByte;
  nextByte+= len;
  nextAddress+= len;
  return thisByte;
}

static char *ReadNonblank(char *delimiters)
{
  char *line= ReadUntil(delimiters);
  while (!line[0]) line= ReadUntil(delimiters);
  return line;
}

static char *ReadBlock(long nBytes)
{
  if (nextByte+nBytes>lastByte && ReadBuffer()) return 0;
  thisByte= nextByte;
  nextByte+= nBytes;
  nextAddress+= nBytes;
  return thisByte;
}

static int ReadBuffer(void)
{
  long nBytes;
  /* quit if EOF or if this request has same address as previous */
  if (bufAtEOF || nextAddress==bufAddress) return 1;
  nBytes= YcRead(bufFile, firstByte, nextAddress, bufSize);
  lastByte= &firstByte[nBytes];
  lastByte[0]= '\0';
  bufAtEOF= nBytes<bufSize;
  bufAddress= nextAddress;
  nextByte= thisByte= firstByte;
  return 0;
}

static int CheckBuffer(long nRequired)
{
  if ((bufSize-(nextByte-firstByte))>=nRequired) return 0;
  else if (nRequired>bufSize) return 1;
  WriteBuffer();
  return 0;
}

static void WriteBuffer(void)
{
  long nBytes= nextByte-firstByte;
  YcWrite(bufFile, firstByte, nextAddress, nBytes);
  nextAddress+= nBytes;
  nextByte= firstByte;
  nextByte[0]= '\0';
}

static long NextWRAddress(int afterRead)
{
  return (nextByte-firstByte) + (afterRead? bufAddress : nextAddress);
}

/*--------------------------------------------------------------------------*/

static void BlocksInit(void)
{
  long *addrs= blocksAddrs;
  HashClear(&blocksTable);
  nBlocks= 0;
  blocksAddrs= 0;
  p_free(addrs);
}

static int BlocksVerify(void)
{
  char *line;
  int illegal=0, skip=0, hadBlocks=0;
  long i, n=0, count, addr=0, minAddr=0, delAddr=0, nGoofs=0;
  long have=0, need= blocksTable.nItems;

  if (need) {
    /* throw out record addresses from previous file */
    long *addrs= blocksAddrs;
    nBlocks= 0;
    blocksAddrs= 0;
    p_free(addrs);
    hadBlocks= 1;
  }

  /* loop on blocks */
  while ((line= ReadUntil(SOH_OR_NEWLINE))) {
    if (line[0]=='\002') break;  /* only legal exit */
    illegal= 1;

    /* add variable name to blocksTable */
    if (!skip) {
      if (!need) {
        if (HashAdd(&blocksTable, line, 0L)) {
          if (nGoofs++ < 10)
            YWarning("duplicate history variable name in PDB Blocks extra");
        }
      } else {
        if (!HashFind(&blocksTable, line, 0L)) {
          YWarning("unknown history history family record variable");
          skip= 1;
        }
      }
    }

    /* read number of blocks */
    if (BlocksLine(line= ReadNonblank(" " ANY_NEWLINE))) break;
    TokenAsLong(line, &n);
    if (nBlocks || n<=0) {
      if (n!=nBlocks || n<=0) {
        YWarning("Yorick-incompatible PDB Blocks extra (1)");
        skip= 1;
      }
    } else {
      /* this is the first variable (after BlocksInit) */
      nBlocks= n;
      blocksAddrs= (long *)p_realloc(blocksAddrs, n*sizeof(long));
    }

    /* read blocks addresses for this variable, ignoring count fields */
    for (i=0 ; i<n ; i++) {
      if (BlocksLine(line= ReadNonblank(" " ANY_NEWLINE))) break;
      TokenAsLong(line, &addr);
      if (BlocksLine(line= ReadNonblank(" " ANY_NEWLINE))) break;
      TokenAsLong(line, &count);
      if (skip) continue;
      if (minAddr) {
        /* not the first variable */
        if (i) {
          /* not the first block -- check that this block has same
             offset of the equivalent block in the first variable */
          if (blocksAddrs[i]!=addr+delAddr) {
            YWarning("Yorick-incompatible PDB Blocks extra (1)");
            skip= 1;
          }
        } else {
          /* the first block -- record offset for this variable,
             and check whether it has smallest address */
          delAddr= blocksAddrs[0]-addr;
          if (addr<minAddr) minAddr= addr;
        }
      } else {
        /* just record the first variable addresses */
        blocksAddrs[i]= addr;
      }
    }
    if (i<n) break;
    if (!skip) {
      if (!minAddr) minAddr= blocksAddrs[0];
      have++;
    }

    illegal= skip;
  }

  if (nBlocks && !illegal) {
    /* adjust blocksAddrs to reflect the variable which was written first,
       rather than the one which appears first in the Blocks: extra */
    if (!need || have==need) {
      minAddr-= blocksAddrs[0];
      for (i=0 ; i<nBlocks ; i++) blocksAddrs[i]+= minAddr;
    } else {
      YWarning("history family record variable blocks missing");
      illegal= 1;
    }
  } else if (!hadBlocks) {
    BlocksInit();
  }

  return illegal;
}

static int BlocksLine(char *line)
{
  if (!line || line[0]=='\002') {
    YWarning("bad Blocks format in PDB extras section");
    return 1;
  } else {
    return 0;
  }
}

/*--------------------------------------------------------------------------*/
