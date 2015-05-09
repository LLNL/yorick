/*
 * $Id: fnctn.c,v 1.4 2010-07-03 19:42:31 dhmunro Exp $
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
    EvalFN evaluates a Yorick interpretive function.  This is one half
    of the Yorick Eval instruction.  The other half is array indexing,
    which is handled in the array.c file.

    The most general Yorick function has 3 distinct kinds of dummy
    parameters:
       func sample(p1, p2, p3, .., pA=, pB=, pC=, pD=)

       where p1, p2, and p3 are ordinary positional parameters,
            .. means that more positional parameters are accessible
               by this function-- the function NextArg() returns
               these additional positional parameters one at a time,
            pA, PB, pC, and pD are keyword parameters,
      Both positional and keyword parameters are optional; if no
      corresponding actual parameter is provided, EvalFN must initialize
      them to nil.

    On entry to EvalFN, the Yorick program stack might look as follows:

    .. sample  p1  kB  pB  p2  p3  kA  pA  p4  kD  pD  p5
         *     *   pB  *   *   *   pA  *   *   pD  *   *  

    where 3 keywords and 2 extra positional parameters have been supplied.
    The second line shows where the Symbol->index pointers point; on input
    only the keyword markers kA, kB, and kD are significant.
    On exit from EvalFN, the stack would read:

    .. sample  p1' kB  pB' p2' p3' kA  pA' p4' kD  pD' p4  p5 (locals)  R
         -2    p1  -1  pB  p2  p3  -1  pA  p4  -1  pD  -1  -1    -1     ?

    The R entry contains the return address.
    In addition to what is shown above, the keyword markers
    now have meaningful offset pointers, namely:

       kL -> kA -> kB -> kD
       k4 -> p4

 */

#include "play.h"
#include "ydata.h"

/*--------------------------------------------------------------------------*/

extern UnaryOp EvalFN, EvalBI;

extern VMaction Return;
extern VMaction NextArg, MoreArgs;

/* YRecoverExterns must be called after an asynchronous interrupt
   of either EvalFN or Return in order to recover from the "partial"
   function call or return sequence.  */
extern void YRecoverExterns(void);

/* ClearStack and AbortReturn are a higher level interface than
   YRecoverExterns.  ClearStack positions the stack to the topmost
   returnSym (if any), returning the associated return pc (or 0).
   AbortReturn clears the stack with ClearStack, then returns to the
   caller WITHOUT leaving any result on the stack.  This is useful
   only during debugging (see task.c).  */
extern Instruction *ClearStack(void);
extern Instruction *AbortReturn(void);

extern void YCatchDrop(long isp);        /* in task.c */
extern long ispCatch;                    /* in task.c */

static void Swap(Symbol *sp, long index);
static Symbol *ExtractKey(int index);

/*--------------------------------------------------------------------------*/

/*
   EvalFN uses Swap to swap the external values of the dummy parameters
   with the values of the actual parameters found on the stack.  This is
   complicated by the possibility of a referenceSym in the actual
   parameter list on the stack.  Any referenceSyms have been copied in a
   first pass through the actual parameters to avoid a possible prior
   swap by a dummy parameter of the same name.
   Swap then does the actual swapping.

   Return uses YRecoverExterns to restore the external values of local
   variables.  YRecoverExterns must also be called after an asynchronous
   interrupt of either EvalFN or Return in order to recover from the
   "partial" function call or return sequence.
 */

/* The code must ensure that the external values can be recovered even
   if the program is interrupted asynchronously in mid-swap.  */
static Symbol *spRecover= 0;

/* YRecoverExterns is usually called by Return, which needs the
   beginning of the referenceSym list, and the beginning of the
   function call list.  */
static Symbol *spFunction= 0;
static Symbol *spReference= 0;
static int nReferences;

static void Swap(Symbol *stack, long index)
{
  Symbol *dummy= &globTab[index];      /* dummy parameter */

  /* The order of the following operations assures that the external
     value can be restored, even if the program is asynchronously
     interrupted during the swapping process (see RecoverExterns).  */

  /* There is a simpler, faster implementation, which takes advantage of
     the fact that dummy->value.db->references does not actually change
     here (the Unref undoes the Ref above).  However, for a very brief
     time, the better algorithm either leaves two references to
     dummy->value.db without incrementing its reference counter, or
     leaves the external value of dummy "unprotected" by spRecover.
     As a first cut, I take the bomb-proof but less efficient solution.
     If this routine proves to be a significant bottleneck for code
     timing, I would consider switching back to the faster, riskier
     algorithm...  */

  /* copy stack value to temporary */
  OpTable *opsX= stack->ops;
  SymbolValue valueX= stack->value;
  OpTable *opsD= dummy->ops;
  int isDB= (opsD==&dataBlockSym);

  /* for bomb-proof safety, "dud" the stack entry */
  stack->ops= &intScalar;   /* value now does NOT reference a pointer */

  /* copy external value to stack */
  if (isDB) stack->value.db= Ref(dummy->value.db);  /* more bomb-proofing */
  else stack->value= dummy->value;
  stack->ops= opsD;

  /* update pointer for YRecoverExterns */
  stack->index= index;  /* mark where to put it back */
  spRecover= stack;

  /* now it is OK to "dud" globTab entry and delete the temporary use */
  if (isDB) {
    dummy->ops= &intScalar;
    Unref(stack->value.db);
  }

  /* copy original stack value into globTab */
  dummy->value= valueX;
  dummy->ops= opsX;

  if (opsX==&dataBlockSym) {
    DataBlock *db= dummy->value.db;
    if (db->ops==&lvalueOps) {
      /* fetch LValue now to avoid repeated fetches during execution */
      Array *array= FetchLValue(db, dummy);
      if (!array->type.dims) {
        if (array->ops==&doubleOps) {
          dummy->ops= &doubleScalar;
          dummy->value.d= array->value.d[0];
          Unref(array);
        } else if (array->ops==&longOps) {
          dummy->ops= &longScalar;
          dummy->value.l= array->value.l[0];
          Unref(array);
        } else if (array->ops==&intOps) {
          dummy->ops= &intScalar;
          dummy->value.i= array->value.i[0];
          Unref(array);
        }
      }
    }
  }
}

void YRecoverExterns(void)
{
  if (spRecover) {
    int index;
    Symbol *local, *spnow;
    OpTable *opsX, *ops;
    SymbolValue valueX;
    int isDB;

    /* Same remark about implementation as for Swap function above.  */

    nReferences= 0;

    for (index=spRecover->index ; index!=-2 ; index=spRecover->index) {
      if (index==-1) { spRecover--; continue; }
      ops= spRecover->ops;
      if (ops==&referenceSym) {
        nReferences++;
        spReference= spRecover--;
        continue;
      }
      isDB= (ops==&dataBlockSym);

      local= &globTab[index];

      /* copy local value to temporary */
      opsX= local->ops;
      valueX= local->value;

      /* temporarily "dud" local value for bomb-proofing */
      local->ops= &intScalar;

      /* copy external value back to globTab */
      if (isDB) local->value.db= Ref(spRecover->value.db);
      else local->value= spRecover->value;
      local->ops= ops;

      /* update pointer for YRecoverExterns */
      spnow= spRecover--;

      /* now it is OK to "dud" stack entry and delete the temporary use */
      if (isDB) {
        (spRecover+1)->ops= &intScalar;
        Unref((spRecover+1)->value.db);
      }

      /* copy local value back to stack */
      spnow->value= valueX;
      spnow->ops= opsX;
    }

    spFunction= spRecover;
    spRecover= 0;
  }
}

/*--------------------------------------------------------------------------*/

static Symbol *actualKeys;

static Symbol *ExtractKey(int index)
{
  Symbol *key= actualKeys, *prev= 0;
  while (key!=prev && (key+1)->index!=index) {
    prev= key;
    key-= key->value.offset;
  }
  if (key!=prev) {
    /* key found, unlink from actualKeys list */
    int offset= key->value.offset;
    if (prev) prev->value.offset= offset? prev-(key-offset) : 0;
    else actualKeys= offset? key-offset : 0;
  } else {
    /* no key corresponds to index in actualKeys list */
    key= 0;
  }
  return key;
}

/*--------------------------------------------------------------------------*/

void EvalFN(Operand *op)
{
  Symbol *stack= op->owner;
  int n= op->references;       /* (sic) # of actual parameters supplied */
  Function *func= op->value;
  Instruction *code= &func->code[1];  /* (code[0] is index to function) */
  int nReq= func->nReq;        /* (see CheckStack call below) */
  int nPos= func->nPos;        /* number of dummy positional parameters */
  int nKey= func->nKey;        /* number of dummy keyword parameters */
  int nLoc= func->nLocal;      /* number of local variables */
  long hasPosList= func->hasPosList;
  long posList;

  int actual, dummy, index, nExtra;
  Symbol *spnow, *extraPos, *key;

  P_SOFTFPE_TEST;

  /* Be sure the stack is long enough for a worst-case invocation of this
     function.  nReq= nPos + (hasPosList&1) + nKey + nLoc + (deepest stack
                      required for expression evaluation) + 10
                      + 1 for return address for this function
     The nPos and nKey terms must be present because they may not be
     actual arguments, and because even if they are supplied they may
     be referenceSyms which must be copied for use during return.
     The extra 10 is so that builtin procedures are always guaranteed
     8(+2 for luck) free stack slots without calling CheckStack.  */
  if (CheckStack(nReq)) stack= sp-n;

  /* Handle all actual parameters.
     This must be done in two passes to avoid accidental collisions
     between dummy parameters and indirect references on the stack
     to external variables of the same name.  All of this could be
     avoided if function parameters were always passed by value,
     never by reference.  But I can't bring myself to disallow the
     FORTRAN-like function which uses its parameters to return values.  */

  /* First pass copies any indirect references.
     The parser has guaranteed that index (dummy) will not be repeated,
     since there may not be 2 dummy parameters with the same name.
     However, nothing prevents the one or more of the actual parameters
     (stack) from being referenceSyms to the same name as a dummy
     parameter.  This possibility requires copying all referenceSym
     actual parameters onto the stack (possibly multiple times).
     Also, note that a globTab entry may NEVER be a referenceSym, so
     if return is to affect external values of parameters, any
     referenceSym parameters must remain on the stack.  */
  posList= hasPosList>>1;
  hasPosList&= 1;
  nExtra= -nPos;
  spnow= stack;
  for (actual=0 ; actual<n ; actual++) {
    spnow++;
    if (spnow->ops) {
      if (spnow->ops==&referenceSym) {
        if (posList) {
          if (nExtra<0 && (posList&1)) {
            /* this is an output parameter */
            extraPos= sp+1;                   /* push copy of referenceSym */
            extraPos->ops= &referenceSym;
            extraPos->index= spnow->index;
            extraPos->value.offset= extraPos-spnow;  /* install ref offset */
            sp= extraPos;
          }
          posList>>= 1;
        }
        ReplaceRef(spnow);       /* replace original reference by object */
      } else if (posList) {
        posList>>= 1;
      }
      nExtra++;
    } else {
      /* skip over keyword arguments */
      spnow++;
      actual++;
    }
  }

  /* Mark beginning of function call for YRecoverExterns.  This MUST
     be done before spRecover has been set (by Swap).  */
  stack->index= -2;

  /* Second pass swaps the external values onto the stack and local
     values into the global symbol table.  */
  posList= -1;
  dummy= 0;
  extraPos= actualKeys= 0;
  spnow= stack;
  for (actual=0 ; actual<n ; actual++) {
    spnow++;
    if (spnow->ops!=0) {        /* actual parameter is positional */
      if (dummy<nPos) {
        dummy++;
        index= (code++)->index;
        Swap(spnow, index);
      } else {
        if (!extraPos) {
          if (!hasPosList) {
            YRecoverExterns();
            YError("too many actual parameters in function call");
          }
          dummy++;
          posList= (code++)->index;
          extraPos= spnow;
        }
        spnow->index= -1;  /* extras cannot be swapped back on return */
      }

    } else {                                /* actual parameter is keyword */
      index= spnow->index;
      spnow->index= -1;  /* keywords must not be swapped back on return */
      spnow->value.offset= actualKeys? spnow-actualKeys : 0;
      actualKeys= spnow++;
      actual++;   /* increment actual, spnow to keyword parameter */
      Swap(spnow, index);
    }
  }

  /* initialize non-actual dummy positionals to nil */
  while (dummy<nPos) {
    dummy++;
    PushDataBlock(RefNC(&nilDB));
    Swap(sp, (code++)->index);
  }
  if (hasPosList && posList<0) posList= (code++)->index;

  /* initialize non-actual dummy keywords to nil */
  for (dummy=0 ; dummy<nKey ; dummy++) {
    index= (code++)->index;
    key= ExtractKey(index);  /* unlinks key (index) from list */
    if (!key) {
      /* missing dummy keywords initialized to nil */
      PushDataBlock(RefNC(&nilDB));
      Swap(sp, index);
    }
  }
  if (actualKeys) {
    YRecoverExterns();
    YError("unrecognized keyword parameter(s) in function call");
  }

  /* handle NextArg() parameter -- this is an entry in the global
     symbol table with the illegal name "*va*" */
  /* NOTE-- assumes fewer than 2048 actual parameters, and that the
            stack depth is less than a million Symbols... */
  if (posList>=0) {
    spnow= sp+1;
    spnow->ops= &longScalar;
    spnow->value.l= extraPos? ((extraPos-spBottom)<<11 | nExtra) : 0;
    sp++;
    Swap(sp, posList);
  }

  /* initialize all local variables to nil */
  for (dummy=0 ; dummy<nLoc ; dummy++) {
    PushDataBlock(RefNC(&nilDB));
    Swap(sp, (code++)->index);
  }

  /* push return address marker */
  spnow= sp+1;
  spnow->ops= &returnSym;
  spnow->index= 0;  /* offset to object-context, if any */
  spnow->value.pc= pc;
  sp++;

  /* stack is again intact, YRecoverExterns can be a no-op */
  spRecover= 0;

  /* set stack and branch into this function */
  pc= code;
}

void EvalBI(Operand *op)
{
  Symbol *stack= op->owner;
  long stackIndex= stack-spBottom;  /* see comment after function call */
  int n= op->references;         /* interpret misuse in FormEvalOp */
  BIFunction *bif= op->value;

  /* Invoke built-in function */
  P_SOFTFPE_TEST;
  bif->function(n);
  P_SOFTFPE_TEST;

  /* Adjust remembered stack to allow for the stack being moved -- this
     can happen in Y_require and Y_include, and there is no other way
     to handle the problem.  The efficiency loss from this instruction
     and the stackIndex definition above is regrettable... */
  stack= spBottom+stackIndex;

  /* Move return value to what will be the top of the stack, and
     discard the reference to the function which is returning.
     However, allow builtin to chain to interpreted function. */
  if (sp>stack && sp->ops!=&returnSym) {
    Symbol *spnow= sp--;
    stack->ops= &intScalar;      /* "dud" BIFunction reference */
    stack->value= spnow->value;  /* move final value into place (dudded) */
    Unref(bif);
    stack->ops= spnow->ops;      /* "arm" final value */
    /* discard the input parameters and scratch space */
    if (sp>stack) Drop((int)(sp-stack));
  }
}

/*--------------------------------------------------------------------------*/

void Return(void)
{
  Symbol *spnow, *extrn;
  OpTable *opsX;
  SymbolValue valueX;

  P_SOFTFPE_TEST;

  /* Pop off any pending catch calls.  */
  if ((sp-1-spBottom)<=ispCatch) YCatchDrop(sp-1-spBottom);

  /* check for object context, update object if present */
  if ((sp-1)->index) yo_cupdate(1 + (int)(sp-1)->index);

  /* Set pc to caller.  Must do this BEFORE the return PC stack element
     is stripped away-- otherwise, there is no way to get back to the
     caller if this routine is asynchronously interrupted.  */
  pc= (sp-1)->value.pc;

  /* Restore external values of local variables in a way that is
     protected against asynchronous interruption.  */
  spRecover= sp-2;
  YRecoverExterns();

  /* Move return value to what will be the top of the stack, and
     discard the reference to the function which is returning.  */
  spnow= sp--;
  valueX= spFunction->value;   /* (know that ops is dataBlockSym) */
  spFunction->ops= &intScalar;
  spFunction->value= spnow->value;
  spFunction->ops= spnow->ops;
  Unref(valueX.db);            /* may clobber sp+1 = spnow !! */

  /* Redefine any actual parameters which were referenceSyms.  */
  while (nReferences--) {
    spnow= spReference - spReference->value.offset;
    extrn= &globTab[spReference->index];
    spReference++;

    /* YRecoverExterns has moved the local value of the dummy argument
       onto the stack.  Delete the external value which is about to
       be replaced, then "dud" the stack value before moving it into
       the external location.  */
    opsX= extrn->ops;
    valueX= extrn->value;
    if (opsX==&dataBlockSym) {
      extrn->ops= &intScalar;
      Unref(valueX.db);
    }
    opsX= spnow->ops;
    spnow->ops= &intScalar;
    extrn->value= spnow->value;
    extrn->ops= opsX;
  }

  /* Clean local variables off stack (where YRecoverExterns put them).  */
  if (sp>spFunction) Drop((int)(sp-spFunction));
}

Instruction *ClearStack(void)
{
  DataBlock *db;
  YRecoverExterns();
  while (sp>spBottom) {
    if (sp->ops==&returnSym) return sp->value.pc;
    db= (sp->ops==&dataBlockSym)? sp->value.db : 0;
    sp--;
    Unref(db);
  }
  return 0;
}

Instruction *AbortReturn(void)
{
  Instruction *pcRet= ClearStack();

  /* Pop off any pending catch calls.  */
  if ((sp-spBottom)<=ispCatch) YCatchDrop(sp-spBottom);

  /* Set pc to caller.  Must do this BEFORE the return PC stack element
     is stripped away-- otherwise, there is no way to get back to the
     caller if this routine is asynchronously interrupted.  */
  if ((pc= pcRet)) {
    /* Restore external values of local variables in a way that is
       protected against asynchronous interruption.  */
    spRecover= sp-1;
    YRecoverExterns();
    Drop((int)(sp-spFunction+1));
  }

  return pcRet;
}

/*--------------------------------------------------------------------------*/

/* If a Yorick function definition includes a ".." dummy parameter, then
   the parser will allow calls to the pseudo-function NextArg(), which
   successively returns positional parameters beyond the named dummy
   parameters.  The MoreArgs() function returns true if there are any
   more positional parameters available with NextArg() (which returns
   nil if there are no more).  */

void MoreArgs(void)
{
  /* this function returns the number of remaining positional parameters */
  long va= globTab[(pc++)->index].value.l;
  PushIntValue(va & 0x7ff);
}

void NextArg(void)
{
  long vaIndex= (pc++)->index;
  long va= globTab[vaIndex].value.l;
  int nExtra= (va & 0x7ff);
  if (nExtra--) {
    Symbol *extraPos= spBottom + (va>>11);
    /* push copy of actual parameter to top of stack */
    (sp+1)->ops= extraPos->ops;
    (sp+1)->value= extraPos->value;
    /* since you only get one crack at this parameter with NextArg, may
       as well trash the original variable to avoid having to increment
       a possible DataBlock reference counter */
    extraPos->ops= &intScalar;
    sp++;
    /* update *va* in globTab for next call to NextArg() or MoreArgs() */
    if (nExtra) {
      /* skip over keywordSym pairs to find next positional parameter */
      while ((++extraPos)->ops==0) extraPos++;
      globTab[vaIndex].value.l= ((extraPos-spBottom)<<11 | nExtra);
    } else {
      globTab[vaIndex].value.l= 0;
    }
  } else {
    PushDataBlock(RefNC(&nilDB));
  }
}

/*--------------------------------------------------------------------------*/
