/*
 * $Id: list.c,v 1.1 2005-09-18 22:04:07 dhmunro Exp $
 * Rudimentary list data type for Yorick.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"
#include "defmem.h"

extern BuiltIn Y__lst, Y__cat, Y__cpy, Y__car, Y__cdr, Y__len;

/* Implement lists as a foreign Yorick data type.  */
typedef struct List_Cell List_Cell;
struct List_Cell {
  int references;      /* reference counter */
  Operations *ops;     /* virtual function table */
  List_Cell *next;
  Symbol sym;
};

extern List_Cell *NewList_Cell(Symbol *sym);
extern void FreeList_Cell(void *list);  /* ******* Use Unref(list) ******* */

Operations listOps = {
  &FreeList_Cell, T_OPAQUE, 0, T_STRING, "list",
  {&PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX, &PromXX},
  &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX, &ToAnyX,
  &NegateX, &ComplementX, &NotX, &TrueX,
  &AddX, &SubtractX, &MultiplyX, &DivideX, &ModuloX, &PowerX,
  &EqualX, &NotEqualX, &GreaterX, &GreaterEQX,
  &ShiftLX, &ShiftRX, &OrX, &AndX, &XorX,
  &AssignX, &EvalX, &SetupX, &GetMemberX, &MatMultX, &PrintX
};

/* Set up a block allocator which grabs space for 64 List_Cell objects
   at a time.  Since List_Cell contains an ops pointer, the alignment
   of a List_Cell must be at least as strict as a void*.  */
static MemryBlock listBlock= {0, 0, sizeof(List_Cell),
                                 64*sizeof(List_Cell)};

List_Cell *NewList_Cell(Symbol *sym)
{
  List_Cell *list_cell= NextUnit(&listBlock);
  list_cell->references= 0;
  list_cell->ops= &listOps;
  list_cell->next= 0;
  /* as in ydata.c:PushCopy */
  list_cell->sym.ops= sym->ops;
  if (sym->ops==&dataBlockSym) list_cell->sym.value.db= Ref(sym->value.db);
  else list_cell->sym.value= sym->value;
  list_cell->sym.index= 1000000000;  /* for stack use only, not List_Cell */
  return list_cell;
}

void FreeList_Cell(void *list)  /* ******* Use Unref(list) ******* */
{
  List_Cell *now= list;
  List_Cell *next= now->next;
  now->next= 0;
  if (now->sym.ops==&dataBlockSym) {
    now->sym.ops= &intScalar;
    Unref(now->sym.value.db);
  }
  FreeUnit(&listBlock, now);
  /* try to prevent very deep recursion by anticipating freeing
     of a whole chain rather than just using Unref(next)
     note that recursion still may occur on the branches */
  while (next && (--next->references<0)) {
    now= next;
    next= now->next;
    now->next= 0;
    if (now->sym.ops==&dataBlockSym) {
      now->sym.ops= &intScalar;
      Unref(now->sym.value.db);
    }
    FreeUnit(&listBlock, now);
  }
}

void Y__lst(int nArgs)
{
  Symbol *s= sp-nArgs+1;
  List_Cell *list= 0;
  if (nArgs<1) YError("_lst function needs at least one argument");
  do {
    /* actually, could make keyword sytax produce an alist... */
    if (!s->ops) YError("unexpected keyword argument in _lst");
    if (s->ops==&referenceSym) ReplaceRef(s);
    if (s->ops==&dataBlockSym && s->value.db->ops==&lvalueOps)
      FetchLValue(s->value.db, s);
    if (!list)
      list= PushDataBlock(NewList_Cell(s));
    else
      list= list->next= NewList_Cell(s);
  } while (++s<sp);
}

void Y__cat(int nArgs)
{
  Symbol *s= sp-nArgs+1;
  List_Cell *list= 0;
  int is_list;
  if (nArgs<1) YError("_cat function needs at least one argument");
  do {
    /* actually, could make keyword sytax produce an alist... */
    if (!s->ops) YError("unexpected keyword argument in _cat");
    if (YNotNil(s)) {
      is_list= 0;
      if (s->ops==&dataBlockSym) {
        if (s->value.db->ops==&lvalueOps) FetchLValue(s->value.db, s);
        else if (s->value.db->ops==&listOps) is_list= 1;
      }
      if (!list) {
        list= PushDataBlock(is_list? Ref(s->value.db) :
                            (DataBlock *)NewList_Cell(s));
      } else {
        list= list->next=
          is_list? (List_Cell *)Ref(s->value.db) : NewList_Cell(s);
      }
      if (is_list) while (list->next) list= list->next;
    }
    s++;
  } while (--nArgs);
  if (!list) PushDataBlock(RefNC(&nilDB));
}

void Y__cpy(int nArgs)
{
  Symbol *s= sp-nArgs+1;
  long i= -1;
  if (nArgs==2 && YNotNil(sp)) i= YGetInteger(sp);
  else if (nArgs!=1) YError("_cpy function takes one or two arguments");
  if (!YNotNil(s) || i==0) {
    PushDataBlock(RefNC(&nilDB));
  } else {
    List_Cell *list, *copy;
    if (s->ops!=&dataBlockSym || s->value.db->ops!=&listOps)
      YError("first argument to _cpy must be a list or nil");
    list= (List_Cell *)s->value.db;
    copy= PushDataBlock(NewList_Cell(&list->sym));
    while ((list= list->next) && --i)
      copy= copy->next= NewList_Cell(&list->sym);
  }
}

void Y__car(int nArgs)
{
  Symbol *s= sp-nArgs+1;
  List_Cell *list;
  int is_db;
  long i= 1;
  if (nArgs<1 || nArgs>3)
    YError("_car function takes one, two, or three arguments");
  if (nArgs>=2 && YNotNil(s+1)) i= YGetInteger(s+1);
  if (!s->ops) YError("unexpected keyword argument in _car");
  if (s->ops==&referenceSym) ReplaceRef(s);
  if (s->ops!=&dataBlockSym || s->value.db->ops!=&listOps)
      YError("first argument to _car must be a list");
  list= (List_Cell *)s->value.db;
  if (i<=0) YError("no such list item in _car");
  while (list && --i) list= list->next;
  if (!list) YError("no such list item in _car");
  /* result is always original value */
  is_db= PushCopy(&list->sym);
  if (nArgs==3) {
    /* set new item value for this cell */
    if (is_db) {
      list->sym.ops= &intScalar;
      Unref(list->sym.value.db);
    }
    s+= 2;
    if (s->ops==&referenceSym) ReplaceRef(s);
    if (s->ops==&dataBlockSym && s->value.db->ops==&lvalueOps)
      FetchLValue(s->value.db, s);
    if (s->ops==&dataBlockSym) list->sym.value.db= Ref(s->value.db);
    else list->sym.value= s->value;
    list->sym.ops= s->ops;
  }
}

void Y__cdr(int nArgs)
{
  Symbol *s= sp-nArgs+1;
  List_Cell *list, *next;
  long i= 1;
  int not_nil;
  if (nArgs<1 || nArgs>3)
    YError("_cdr function takes one, two, or three arguments");
  if (nArgs>=2 && YNotNil(s+1)) i= YGetInteger(s+1);
  if (nArgs>2? (i<1) : (i<0))
    YError("second argument too small in _cdr");
  if (!s->ops) YError("unexpected keyword argument in _cdr");
  if (s->ops==&referenceSym) ReplaceRef(s);
  if (s->ops!=&dataBlockSym || s->value.db->ops!=&listOps)
      YError("first argument to _cdr must be a list");
  list= (List_Cell *)s->value.db;
  next= 0;
  if (i>0) {
    while (list && --i) list= list->next;
    if (list) next= list->next;
  } else {
    next= list;
  }
  /* result is always original value or nil */
  PushDataBlock(next? (DataBlock *)Ref(next) : RefNC(&nilDB));
  if (nArgs==3) {
    /* set new list continuation for this cell */
    List_Cell *tmp;
    s+= 2;
    if (s->ops==&referenceSym) ReplaceRef(s);
    not_nil= YNotNil(s);
    if (not_nil &&
        (s->ops!=&dataBlockSym || s->value.db->ops!=&listOps))
      YError("third argument to _cdr must be a list or nil");
    if (!list) YError("no such list to set in _cdr");
    tmp= list->next;  /* zero list->next before Unref */
    list->next= 0;
    Unref(tmp);
    if (not_nil) list->next= (List_Cell *)Ref(s->value.db);
  }
}

void Y__len(int nArgs)
{
  Symbol *s= sp-nArgs+1;
  long len= 0;
  if (nArgs!=1) YError("_len function takes exactly one argument");
  if (YNotNil(s)) {
    List_Cell *list;
    if (s->ops!=&dataBlockSym || s->value.db->ops!=&listOps)
      YError("first argument to _len must be a list or nil");
    for (list=(List_Cell *)s->value.db ; list ; list=list->next) len++;
  }
  PushLongValue(len);
}
