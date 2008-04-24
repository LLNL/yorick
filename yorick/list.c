/*
 * $Id: list.c,v 1.5 2008-04-24 20:38:56 dhmunro Exp $
 * Rudimentary list data type for Yorick.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include "ydata.h"
#include "defmem.h"

extern BuiltIn Y__lst, Y__cat, Y__cpy, Y__car, Y__cdr, Y__len, Y_is_list;

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
  List_Cell *list_cell = NextUnit(&listBlock);
  list_cell->references = 0;
  list_cell->ops = &listOps;
  list_cell->next = 0;
  /* as in ydata.c:PushCopy */
  list_cell->sym.ops = sym->ops;
  if (sym->ops==&dataBlockSym) list_cell->sym.value.db = Ref(sym->value.db);
  else list_cell->sym.value = sym->value;
  list_cell->sym.index = 1000000000;  /* for stack use only, not List_Cell */
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

void
ypush_list(int n)
{
  if (n) {
    int cat = (n<0);
    int is_list = 0;
    List_Cell *list = 0;
    Symbol *s;
    if (cat) n = -n;
    for (s=sp-n+1 ; n ; s++,n--) {
      /* actually, could make keyword sytax produce an alist... */
      if (!s->ops) y_error("unexpected keyword argument in _lst or _cat");
      if (s->ops==&referenceSym) ReplaceRef(s);
      if (s->ops==&dataBlockSym && s->value.db->ops==&lvalueOps)
        FetchLValue(s->value.db, s);
      if (cat) {
        if (s->ops == &dataBlockSym) {
          if (s->value.db==&nilDB) continue;
          is_list = (s->value.db->ops == &listOps);
        } else {
          is_list = 0;
        }
      }
      if (!list) {
        sp[1].ops = &dataBlockSym;
        if (is_list)
          sp[1].value.db = Ref(s->value.db);
        else
          sp[1].value.db = (DataBlock *)NewList_Cell(s);
        list = (List_Cell *)sp[1].value.db;
        sp++;
      } else if (is_list) {
        list = list->next = (List_Cell *)Ref(s->value.db);
      } else {
        list = list->next = NewList_Cell(s);
      }
      if (is_list) while (list->next) list = list->next;
    }
    if (cat && !list) ypush_nil();
  } else {
    ypush_nil();
  }
}

void
Y__lst(int argc)
{
  if (argc<1) y_error("_lst function needs at least one argument");
  ypush_list(argc);
}

void
Y__cat(int argc)
{
  if (argc<1) y_error("_cat function needs at least one argument");
  ypush_list(-argc);
}

void
Y__cpy(int argc)
{
  long n = -1;
  if (argc==2 && !yarg_nil(0)) n = ygets_l(0);
  else if (argc!=1) y_error("_cpy function takes one or two arguments");
  if (!n || yarg_nil(argc-1)) {
    ypush_nil();
  } else {
    if (n < 0) n = 0;
    if (ypush_cdr(argc-1, -n-1))
      y_error("first argument to _cpy must be a list or nil");
  }
}

static List_Cell *yl_nthcdr(List_Cell *list, long n);

static List_Cell *
yl_nthcdr(List_Cell *list, long n)
{
  List_Cell *next = 0;
  if (n > 0) {
    while (list && --n) list = list->next;
    if (list) next = list->next;
  } else {
    next = list;
  }
  return next;
}

int
ypush_car(int iarg, long n)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops == &referenceSym) s = &globTab[s->index];
    if (s->ops==&dataBlockSym && s->value.db->ops==&listOps) {
      if (n > 0) {
        List_Cell *cell = yl_nthcdr((List_Cell *)s->value.db, n-1);
        if (cell) {
          if (cell->sym.ops == &dataBlockSym)
            sp[1].value.db = Ref(cell->sym.value.db);
          else
            sp[1].value = cell->sym.value;
          sp[1].ops = cell->sym.ops;
          sp++;
          return 0;
        }
      }
      return 2;
    }
  }
  return 1;
}

int
yput_car(int iarg, long n, int jarg)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops == &referenceSym) s = &globTab[s->index];
    if (s->ops==&dataBlockSym && s->value.db->ops==&listOps) {
      if (n > 0) {
        List_Cell *cell = yl_nthcdr((List_Cell *)s->value.db, n-1);
        if (cell) {
          s = sp - jarg;
          if (!sp->ops) return 3;
          if (s->ops == &referenceSym) s = &globTab[s->index];
          if (s->ops==&dataBlockSym && s->value.db->ops==&lvalueOps)
            FetchLValue(s->value.db, s);
          if (cell->sym.ops == &dataBlockSym) {
            cell->sym.ops = &intScalar;
            Unref(cell->sym.value.db);
          }
          if (s->ops==&dataBlockSym) cell->sym.value.db = Ref(s->value.db);
          else cell->sym.value = s->value;
          cell->sym.ops = s->ops;
          return 0;
        }
      }
      return 2;
    }
  }
  return 1;
}

void
Y__car(int argc)
{
  int flag;
  long n = 1;
  if (argc<1 || argc>3)
    y_error("_car function takes one, two, or three arguments");
  if (argc >= 2 && !yarg_nil(argc-2)) n = ygets_l(argc-2);
  flag = ypush_car(argc-1, n);
  if (flag == 2) y_error("no such list item in _car");
  else if (flag) y_error("first argument to _car must be a list");
  if (argc == 3) {
    if (yput_car(argc, n, 1)) y_error("(BUG) impossible error in _car");
  }
}

int
ypush_cdr(int iarg, long n)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops == &referenceSym) s = &globTab[s->index];
    if (s->ops==&dataBlockSym && s->value.db->ops==&listOps) {
      if (n >= 0) {
        List_Cell *next = yl_nthcdr((List_Cell *)s->value.db, n);
        sp[1].value.db = next? (DataBlock *)Ref(next) : RefNC(&nilDB);
        sp[1].ops = &dataBlockSym;
        sp++;
      } else {
        List_Cell *list = (List_Cell *)s->value.db;
        List_Cell *copy = NewList_Cell(&list->sym);
        sp[1].value.db = (DataBlock *)copy;
        sp[1].ops = &dataBlockSym;
        sp++;
        n = -n - 1;
        while ((list=list->next) && --n)
          copy = copy->next = NewList_Cell(&list->sym);
      }
      return 0;
    }
  }
  return 1;
}

int
yput_cdr(int iarg, long n, int jarg)
{
  if (iarg>=0 && jarg>=0) {
    Symbol *s = sp - iarg;
    if (s->ops == &referenceSym) s = &globTab[s->index];
    if (s->ops==&dataBlockSym && s->value.db->ops==&listOps) {
      List_Cell *list = yl_nthcdr((List_Cell *)s->value.db, n-1);
      if (!list || n<1)
        return 2;
      s = sp - jarg;
      if (s->ops == &referenceSym) s = &globTab[s->index];
      if (s->ops == &dataBlockSym) {
        int is_nil = (s->value.db == &nilDB);
        if (is_nil || s->value.db->ops==&listOps) {
          List_Cell *tmp = list->next;
          list->next = 0;
          Unref(tmp);
          if (!is_nil)
            list->next = (List_Cell *)Ref(s->value.db);
          return 0;
        }
      }
    }
  }
  return 1;
}

void
Y__cdr(int argc)
{
  long n = 1;
  if (argc<1 || argc>3)
    y_error("_cdr function takes one, two, or three arguments");
  if (argc >= 2) {
    if (!yarg_nil(argc-2)) n = ygets_l(argc-2);
    if (n<0 || (n<1 && argc>2)) y_error("second argument too small in _cdr");
  }
  if (ypush_cdr(argc-1, n)) y_error("first argument to _cdr must be a list");
  if (argc==3) {
    int flag = yput_cdr(argc, n, 1);
    if (flag == 2) y_error("no such list to set in _cdr");
    else if (flag) y_error("third argument to _cdr must be a list or nil");
  }
}

long
yarg_nlist(int iarg)
{
  long len = -1;
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    if (s->ops == &dataBlockSym
        && (s->value.db==&nilDB || s->value.db->ops==&listOps)) {
      List_Cell *list = (s->value.db==&nilDB)?  0 : (List_Cell *)s->value.db;
      len = 0;
      for ( ; list ; list=list->next) len++;
    }
  }
  return len;
}

void
Y__len(int argc)
{
  long len = 0;
  if (argc!=1) y_error("_len function takes exactly one argument");
  len = yarg_nlist(0);
  if (len < 0) y_error("argument to _len must be a list or nil");
  ypush_long(len);
}

int
yarg_list(int iarg)
{
  if (iarg >= 0) {
    Symbol *s = sp - iarg;
    if (s->ops==&referenceSym) s = &globTab[s->index];
    if (s->ops == &dataBlockSym)
      return s->value.db->ops == &listOps;
  }
  return 0;
}

void
Y_is_list(int argc)
{
  if (argc!=1) y_error("is_list function takes exactly one argument");
  ypush_int(yarg_list(0));
}
