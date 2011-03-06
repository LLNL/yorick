/*
 * $Id: testp.i,v 1.2 2010-07-03 19:42:31 dhmunro Exp $
 * Test of Yorick parser
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

goofs= 0;  /* cumulative tally of errors detected */
write, "Begin Yorick parser test...";
if (do_stats) "A "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

/* First function is pure parser test, exercising many yucky language
   features, but not producing any usable procedure.  Check disassembled
   function to be sure code is correct.  */
func parser_test(pos1, pos2, pos3, .., key1=, key2=, key3=)
{
  pos1= '\0';
  pos2= 1s;
  pos3= 2n;
  loc1= 3L;
  loc2= 4.0e0F;
  loc3= 5.0;
  loc4= 6.0i;
  loc5= "A string with ' and /* inside.";


  /* Blank lines, then a short comment with imbedded " character */

  /* A multiline comment with blank lines and various types of
     quote characters,

     such as " and ',


     all of which should be completely ignored... */

#if 0
Try commenting something out with #if 0/#endif sequence
#  if 0
     These should nest properly...
#  endif
...So this line should still be commented out.
#endif
#  if 0
  Be sure indented style works.
#  endif

// Comment out something with C++-style comment
            // ... and another comment
/*
// Nested comment test
terrible syntax error
*/
//  first commented out line;  /* nested normal comment */
//  second bad line;
//   third bad line;
//   fourth bad line;   /* final nested comment

  key1= pos3;   // C++ comment
  /* initial comment */ key2 /* imbedded comment */ = 0 /* repeated
     constant */ + 6.;  /* followed by a second repeated constant */

  key3= ext1;  /* first example of an external variable // nest test */

  local loc6;  local loc7, loc8, loc9;  // C++ style comment /* nesting */
  extern ext2, ext3;  extern ext4;

  ext2= -1;  /* negative of existing constant */
  loc6= - /* nasty imbedded
             comment */ 7.0;

  ext3= "multiline string also tests escape sequences: \
\\n\n, \\t\t, \\a\a, \\f\f, \\r\r, \\v\v, \\b\b, \', \"\n\
\?, \q, C-a\1, C-b\02, C-c\003, C-d\0041 (should C-d1), C-z\x1A,\
DEL\x7f";  /* Note: \? and \q should just give ? and q */

  loc7= ext4+1;
  loc8= ext5();   /* should push nil argument */
  ext5(pos1);
  ext5(pos1, key1, loc1);
  ext5(pos1, key1, .., loc1);
  ext5;
  ext5, pos1;
  ext5, pos1, key1, loc1, ext1, /* final argument nil */;

  /* Try lines with implicit semi-colon terminators: */
  loc7(3:, 6*loc1+loc2, ::loc2+loc3)=
    5
  loc8= loc7 + 7*
    loc6(3,
         )

  if (loc1) ext5
  else ext5, 1

  if (loc1) ext5
  ext5, 1

  /* Try several popular styles */
  if (loc1) {
    ext3;
  } else if (!loc2) {
    ext3, 1;
  } else if (loc3) {
    ext3, 1;
    ext3, 2;
  } else {
    ext3, 3;
  }

  if (loc1)
  {
    ext4;
  }
  else if (loc2)
  {
    ext4, 1;
  }
  else if (!loc3)
  {
    ext4, 1;
    ext4, 2;
  }
  else
  {
    ext4, 3;
  }

  if (!loc1) ext5;
  else if (loc2) ext5, 1;
  else if (loc3) { ext5, 1; ext5, 2; }
  else ext5, 3;

  while (loc1--) {
    ext6;
    ext6, 2;
  };            /* check that extraneous trailing semi-colon is OK */

 backward:
  do {
    ext6;
    if (ext1) break;
    if (ext2) continue;
    ext6, 2;
  } while (--loc1);

  if (ext6) goto forward;
  if (ext3) goto backward;

  for (loc1=0 ; loc1<8 ; loc1++) {
    ext5;
    ext5, 2;
  }

 forward:
  for (loc1=0, loc2=loc3=0 ;
       loc1<8 ;
       loc1++, loc2+=2, loc3+=3) {
    if (ext4>=9) continue;
    ext5;
    do {
      ext6;
      if (ext1!=3) break;
      for (loc1=0 ; loc1<8 ; loc1++) {
        ext5;
        if (ext3<=2) break;
        if (ext4==7) continue;
        if (!ext1) goto inloop;
        ext5, 2;
      }
      if (ext2) continue;
      ext6, 2;
    } while (--loc1);
    if (ext3>3) break;
    ext5, 2;
  inloop:
  }

  if (loc1 || loc2 && loc3) goto backward;

  return 3*loc1(3:12:3, ptp, avg:9:21)? 3+ext1 : 2-ext2;
}

if (do_stats) "B "+print(yorick_stats());

#if 0
Here is the correct disassemble output for parser_test:
func parser_test(pos1,pos2,pos3,..,key1=,key2=,key3=)
  17 sp->1    PushChar(0x00)
  19 sp0>1    Define(pos1)
  21 sp->0    DropTop
  22 sp+>1    PushShort(1)
  24 sp0>1    Define(pos2)
  26 sp->0    DropTop
  27 sp+>1    PushInt(2)
  29 sp0>1    Define(pos3)
  31 sp->0    DropTop
  32 sp+>1    PushLong(3)
  34 sp0>1    Define(loc1)
  36 sp->0    DropTop
  37 sp+>1    PushFloat(4)
  39 sp0>1    Define(loc2)
  41 sp->0    DropTop
  42 sp+>1    PushDouble(5)
  44 sp0>1    Define(loc3)
  46 sp->0    DropTop
  47 sp+>1    PushImaginary(6i)
  49 sp0>1    Define(loc4)
  51 sp->0    DropTop
  52 sp+>1    PushString("A string with ' and /* i"...)
  54 sp0>1    Define(loc5)
  56 sp->0    DropTop
  57 sp+>1    PushVariable(pos3)
  59 sp0>1    Define(key1)
  61 sp->0    DropTop
  62 sp+>1    PushLong(0)
  64 sp+>2    PushDouble(6)
  66 sp->1    Add
  67 sp0>1    Define(key2)
  69 sp->0    DropTop
  70 sp+>1    PushVariable(ext1)
  72 sp0>1    Define(key3)
  74 sp->0    DropTop
  75 sp+>1    PushLong(-1)
  77 sp0>1    Define(ext2)
  79 sp->0    DropTop
  80 sp+>1    PushDouble(-7)
  82 sp0>1    Define(loc6)
  84 sp->0    DropTop
  85 sp+>1    PushString("multiline string also te"...)
  87 sp0>1    Define(ext3)
  89 sp->0    DropTop
  90 sp+>1    PushVariable(ext4)
  92 sp+>2    PushLong(1)
  94 sp->1    Add
  95 sp0>1    Define(loc7)
  97 sp->0    DropTop
  98 sp+>1    PushVariable(ext5)
 100 sp+>2    PushNil
 101 sp->1    Eval(1)
 103 sp0>1    Define(loc8)
 105 sp->0    DropTop
 106 sp+>1    PushVariable(ext5)
 108 sp+>2    PushReference(pos1)
 110 sp->1    Eval(1)
 112 sp0>1    Print
 113 sp->0    DropTop
 114 sp+>1    PushVariable(ext5)
 116 sp+>2    PushReference(pos1)
 118 sp+>3    PushReference(key1)
 120 sp+>4    PushReference(loc1)
 122 sp->1    Eval(3)
 124 sp0>1    Print
 125 sp->0    DropTop
 126 sp+>1    PushVariable(ext5)
 128 sp+>2    PushReference(pos1)
 130 sp+>3    PushReference(key1)
 132 sp+>4    FormRangeFlag(..)
 134 sp+>5    PushReference(loc1)
 136 sp->1    Eval(4)
 138 sp0>1    Print
 139 sp->0    DropTop
 140 sp+>1    PushVariable(ext5)
 142 sp0>1    Print
 143 sp->0    DropTop
 144 sp+>1    PushVariable(ext5)
 146 sp+>2    PushReference(pos1)
 148 sp->1    Eval(1)
 150 sp->0    DropTop
 151 sp+>1    PushVariable(ext5)
 153 sp+>2    PushReference(pos1)
 155 sp+>3    PushReference(key1)
 157 sp+>4    PushReference(loc1)
 159 sp+>5    PushReference(ext1)
 161 sp+>6    PushNil
 162 sp->1    Eval(5)
 164 sp->0    DropTop
 165 sp+>1    PushVariable(loc7)
 167 sp+>2    PushLong(3)
 169 sp+>3    PushNil
 170 sp->2    FormRange(2)
 172 sp+>3    PushLong(6)
 174 sp+>4    PushVariable(loc1)
 176 sp->3    Multiply
 177 sp+>4    PushVariable(loc2)
 179 sp->3    Add
 180 sp+>4    PushNil
 181 sp+>5    PushNil
 182 sp+>6    PushVariable(loc2)
 184 sp+>7    PushVariable(loc3)
 186 sp->6    Add
 187 sp->4    FormRange(3)
 189 sp->1    Eval(3)
 191 sp+>2    PushLong(5)
 193 sp->1    Assign
 194 sp->0    DropTop
 195 sp+>1    PushVariable(loc7)
 197 sp+>2    PushLong(7)
 199 sp+>3    PushVariable(loc6)
 201 sp+>4    PushLong(3)
 203 sp+>5    PushNil
 204 sp->3    Eval(2)
 206 sp->2    Multiply
 207 sp->1    Add
 208 sp0>1    Define(loc8)
 210 sp->0    DropTop
 211 sp+>1    PushVariable(loc1)
 213 sp->0    BranchFalse to pc= 221
 215 sp+>1    PushVariable(ext5)
 217 sp0>1    Print
 218 sp->0    DropTop
 219 sp0>0    Branch to pc= 228
 221 sp+>1    PushVariable(ext5)
 223 sp+>2    PushLong(1)
 225 sp->1    Eval(1)
 227 sp->0    DropTop
 228 sp+>1    PushVariable(loc1)
 230 sp->0    BranchFalse to pc= 236
 232 sp+>1    PushVariable(ext5)
 234 sp0>1    Print
 235 sp->0    DropTop
 236 sp+>1    PushVariable(ext5)
 238 sp+>2    PushLong(1)
 240 sp->1    Eval(1)
 242 sp->0    DropTop
 243 sp+>1    PushVariable(loc1)
 245 sp->0    BranchFalse to pc= 253
 247 sp+>1    PushVariable(ext3)
 249 sp0>1    Print
 250 sp->0    DropTop
 251 sp0>0    Branch to pc= 293
 253 sp+>1    PushVariable(loc2)
 255 sp->0    BranchTrue to pc= 266
 257 sp+>1    PushVariable(ext3)
 259 sp+>2    PushLong(1)
 261 sp->1    Eval(1)
 263 sp->0    DropTop
 264 sp0>0    Branch to pc= 293
 266 sp+>1    PushVariable(loc3)
 268 sp->0    BranchFalse to pc= 286
 270 sp+>1    PushVariable(ext3)
 272 sp+>2    PushLong(1)
 274 sp->1    Eval(1)
 276 sp->0    DropTop
 277 sp+>1    PushVariable(ext3)
 279 sp+>2    PushLong(2)
 281 sp->1    Eval(1)
 283 sp->0    DropTop
 284 sp0>0    Branch to pc= 293
 286 sp+>1    PushVariable(ext3)
 288 sp+>2    PushLong(3)
 290 sp->1    Eval(1)
 292 sp->0    DropTop
 293 sp+>1    PushVariable(loc1)
 295 sp->0    BranchFalse to pc= 303
 297 sp+>1    PushVariable(ext4)
 299 sp0>1    Print
 300 sp->0    DropTop
 301 sp0>0    Branch to pc= 343
 303 sp+>1    PushVariable(loc2)
 305 sp->0    BranchFalse to pc= 316
 307 sp+>1    PushVariable(ext4)
 309 sp+>2    PushLong(1)
 311 sp->1    Eval(1)
 313 sp->0    DropTop
 314 sp0>0    Branch to pc= 343
 316 sp+>1    PushVariable(loc3)
 318 sp->0    BranchTrue to pc= 336
 320 sp+>1    PushVariable(ext4)
 322 sp+>2    PushLong(1)
 324 sp->1    Eval(1)
 326 sp->0    DropTop
 327 sp+>1    PushVariable(ext4)
 329 sp+>2    PushLong(2)
 331 sp->1    Eval(1)
 333 sp->0    DropTop
 334 sp0>0    Branch to pc= 343
 336 sp+>1    PushVariable(ext4)
 338 sp+>2    PushLong(3)
 340 sp->1    Eval(1)
 342 sp->0    DropTop
 343 sp+>1    PushVariable(loc1)
 345 sp->0    BranchTrue to pc= 353
 347 sp+>1    PushVariable(ext5)
 349 sp0>1    Print
 350 sp->0    DropTop
 351 sp0>0    Branch to pc= 393
 353 sp+>1    PushVariable(loc2)
 355 sp->0    BranchFalse to pc= 366
 357 sp+>1    PushVariable(ext5)
 359 sp+>2    PushLong(1)
 361 sp->1    Eval(1)
 363 sp->0    DropTop
 364 sp0>0    Branch to pc= 393
 366 sp+>1    PushVariable(loc3)
 368 sp->0    BranchFalse to pc= 386
 370 sp+>1    PushVariable(ext5)
 372 sp+>2    PushLong(1)
 374 sp->1    Eval(1)
 376 sp->0    DropTop
 377 sp+>1    PushVariable(ext5)
 379 sp+>2    PushLong(2)
 381 sp->1    Eval(1)
 383 sp->0    DropTop
 384 sp0>0    Branch to pc= 393
 386 sp+>1    PushVariable(ext5)
 388 sp+>2    PushLong(3)
 390 sp->1    Eval(1)
 392 sp->0    DropTop
 393 sp+>1    PushVariable(loc1)
 395 sp+>2    Push1
 396 sp+>3    DupUnder
 397 sp->2    Subtract
 398 sp0>2    Define(loc1)
 400 sp->1    DropTop
 401 sp->0    BranchFalse to pc= 416
 403 sp+>1    PushVariable(ext6)
 405 sp0>1    Print
 406 sp->0    DropTop
 407 sp+>1    PushVariable(ext6)
 409 sp+>2    PushLong(2)
 411 sp->1    Eval(1)
 413 sp->0    DropTop
 414 sp0>0    Branch to pc= 393
 416 sp+>1    PushVariable(ext6)
 418 sp0>1    Print
 419 sp->0    DropTop
 420 sp+>1    PushVariable(ext1)
 422 sp->0    BranchFalse to pc= 426
 424 sp0>0    Branch to pc= 447
 426 sp+>1    PushVariable(ext2)
 428 sp->0    BranchFalse to pc= 432
 430 sp0>0    Branch to pc= 439
 432 sp+>1    PushVariable(ext6)
 434 sp+>2    PushLong(2)
 436 sp->1    Eval(1)
 438 sp->0    DropTop
 439 sp+>1    PushVariable(loc1)
 441 sp+>2    Push1
 442 sp->1    Subtract
 443 sp0>1    Define(loc1)
 445 sp->0    BranchTrue to pc= 416
 447 sp+>1    PushVariable(ext6)
 449 sp->0    BranchFalse to pc= 453
 451 sp0>0    Branch to pc= 493
 453 sp+>1    PushVariable(ext3)
 455 sp->0    BranchFalse to pc= 459
 457 sp0>0    Branch to pc= 416
 459 sp+>1    PushLong(0)
 461 sp0>1    Define(loc1)
 463 sp->0    DropTop
 464 sp+>1    PushVariable(loc1)
 466 sp+>2    PushLong(8)
 468 sp->1    Less
 469 sp->0    BranchFalse to pc= 493
 471 sp+>1    PushVariable(ext5)
 473 sp0>1    Print
 474 sp->0    DropTop
 475 sp+>1    PushVariable(ext5)
 477 sp+>2    PushLong(2)
 479 sp->1    Eval(1)
 481 sp->0    DropTop
 482 sp+>1    PushVariable(loc1)
 484 sp+>2    Push1
 485 sp+>3    DupUnder
 486 sp->2    Add
 487 sp0>2    Define(loc1)
 489 sp->1    DropTop
 490 sp->0    DropTop
 491 sp0>0    Branch to pc= 464
 493 sp+>1    PushLong(0)
 495 sp0>1    Define(loc1)
 497 sp->0    DropTop
 498 sp+>1    PushLong(0)
 500 sp0>1    Define(loc3)
 502 sp0>1    Define(loc2)
 504 sp->0    DropTop
 505 sp+>1    PushVariable(loc1)
 507 sp+>2    PushLong(8)
 509 sp->1    Less
 510 sp->0    BranchFalse to pc= 660
 512 sp+>1    PushVariable(ext4)
 514 sp+>2    PushLong(9)
 516 sp->1    GreaterEQ
 517 sp->0    BranchFalse to pc= 521
 519 sp0>0    Branch to pc= 633
 521 sp+>1    PushVariable(ext5)
 523 sp0>1    Print
 524 sp->0    DropTop
 525 sp+>1    PushVariable(ext6)
 527 sp0>1    Print
 528 sp->0    DropTop
 529 sp+>1    PushVariable(ext1)
 531 sp+>2    PushLong(3)
 533 sp->1    NotEqual
 534 sp->0    BranchFalse to pc= 538
 536 sp0>0    Branch to pc= 617
 538 sp+>1    PushLong(0)
 540 sp0>1    Define(loc1)
 542 sp->0    DropTop
 543 sp+>1    PushVariable(loc1)
 545 sp+>2    PushLong(8)
 547 sp->1    Less
 548 sp->0    BranchFalse to pc= 596
 550 sp+>1    PushVariable(ext5)
 552 sp0>1    Print
 553 sp->0    DropTop
 554 sp+>1    PushVariable(ext3)
 556 sp+>2    PushLong(2)
 558 sp->1    LessEQ
 559 sp->0    BranchFalse to pc= 563
 561 sp0>0    Branch to pc= 596
 563 sp+>1    PushVariable(ext4)
 565 sp+>2    PushLong(7)
 567 sp->1    Equal
 568 sp->0    BranchFalse to pc= 572
 570 sp0>0    Branch to pc= 585
 572 sp+>1    PushVariable(ext1)
 574 sp->0    BranchTrue to pc= 578
 576 sp0>0    Branch to pc= 633
 578 sp+>1    PushVariable(ext5)
 580 sp+>2    PushLong(2)
 582 sp->1    Eval(1)
 584 sp->0    DropTop
 585 sp+>1    PushVariable(loc1)
 587 sp+>2    Push1
 588 sp+>3    DupUnder
 589 sp->2    Add
 590 sp0>2    Define(loc1)
 592 sp->1    DropTop
 593 sp->0    DropTop
 594 sp0>0    Branch to pc= 543
 596 sp+>1    PushVariable(ext2)
 598 sp->0    BranchFalse to pc= 602
 600 sp0>0    Branch to pc= 609
 602 sp+>1    PushVariable(ext6)
 604 sp+>2    PushLong(2)
 606 sp->1    Eval(1)
 608 sp->0    DropTop
 609 sp+>1    PushVariable(loc1)
 611 sp+>2    Push1
 612 sp->1    Subtract
 613 sp0>1    Define(loc1)
 615 sp->0    BranchTrue to pc= 525
 617 sp+>1    PushVariable(ext3)
 619 sp+>2    PushLong(3)
 621 sp->1    Greater
 622 sp->0    BranchFalse to pc= 626
 624 sp0>0    Branch to pc= 660
 626 sp+>1    PushVariable(ext5)
 628 sp+>2    PushLong(2)
 630 sp->1    Eval(1)
 632 sp->0    DropTop
 633 sp+>1    PushVariable(loc1)
 635 sp+>2    Push1
 636 sp+>3    DupUnder
 637 sp->2    Add
 638 sp0>2    Define(loc1)
 640 sp->1    DropTop
 641 sp->0    DropTop
 642 sp+>1    PushVariable(loc2)
 644 sp+>2    PushLong(2)
 646 sp->1    Add
 647 sp0>1    Define(loc2)
 649 sp->0    DropTop
 650 sp+>1    PushVariable(loc3)
 652 sp+>2    PushLong(3)
 654 sp->1    Add
 655 sp0>1    Define(loc3)
 657 sp->0    DropTop
 658 sp0>0    Branch to pc= 505
 660 sp+>1    PushVariable(loc1)
 662 sp->0    BranchTrue to pc= 673
 664 sp+>1    PushVariable(loc2)
 666 sp->0    BranchFalse to pc= 671
 668 sp+>1    PushVariable(loc3)
 670 sp==0    AndOrLogical for &&
 671 sp+>1    Push0
 672 sp==0    AndOrLogical for ||
 673 sp+>1    Push1
 674 sp->0    BranchFalse to pc= 678
 676 sp0>0    Branch to pc= 416
 678 sp+>1    PushLong(3)
 680 sp+>2    PushVariable(loc1)
 682 sp+>3    PushLong(3)
 684 sp+>4    PushLong(12)
 686 sp+>5    PushLong(3)
 688 sp->3    FormRange(3)
 690 sp+>4    FormRangeFunc(ptp:)
 692 sp+>5    PushLong(9)
 694 sp+>6    PushLong(21)
 696 sp->5    FormRange(2)
 698 sp0>5    AddRangeFunc(avg:)
 700 sp->2    Eval(3)
 702 sp->1    Multiply
 703 sp->0    BranchFalse to pc= 712
 705 sp+>1    PushLong(3)
 707 sp+>2    PushVariable(ext1)
 709 sp->1    Add
 710 sp0>1    Branch to pc= 717
 712 sp+>1    PushLong(2)
 714 sp+>2    PushVariable(ext2)
 716 sp->1    Subtract
 717 sp->0    Return
 718 sp==0    Halt-Virtual-Machine
#endif

/* Try reinstated line */
junk= 1;
#if 1
junk= 0;
#  if 0
junk= 2;
#  endif
#endif
if (junk) {
  goofs++;
  "**FAILURE** #if / #endif construction broken";
}

/* ------------------------------------------------------------------------- */

f= open("../i/testp.i", "r", 1);
if (is_void(f)) f= open(Y_SITE+"i/testp.i", "r", 1);
if (f) {
  while (!strmatch((line= rdline(f)), "Here is the correct disassemble"));
  correct= [];
  while (!strmatch((line= rdline(f)), "#endif")) grow, correct, line;
  close, f;
  if (anyof(disassemble(parser_test)!=correct)) {
    goofs++;
    "**FAILURE** of the parser_test disassembly";
    "            -- writing disassmbly of parser_test to pjunk.jnk";
    f= open("pjunk.jnk", "w");
    write, f, format="%s\n", disassemble(parser_test);
    close, f;
  }
  correct= [];
} else {
  "WARNING-- skipping disassembly check, i/testp.i not present";
}

/* check that first appearance of symbol as keyword leaves it undecided */
func parser_test(x)
{
  png, dpi=72, x;
  dpi = 300;
  local dpi;
}
func parser_test(x)
{
  png, dpi=72, x;
  call, dpi;
  extern dpi;
}
parser_test= [];

/* Check for limitation on yacc-parser stack depth.
   If this fails with a SYNTAX error like "yacc stack overflow", see
   top of yorick.y source-- your yacc may have a switch to fix it.  */
{ if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else if (1) x= [1, 2]; else if (1) x= [1, 2]; else if (1) x= [1, 2];
  else x= [1,2]; }

x= [];

/* If following lines give syntax errors, something is wrong with
   the NumberValue routine in Yorick/yorick.c */
if (abs(100000000000000000000.0-1.e20)>1.e11)
  write, "**WARNING** problem with numeric conversions";
if (0xffffffff != ((0xffff<<16)|0xffff))
  write, "**WARNING** problem with strtoul?";

/* test basic flow control statements */
i= 0;
do { i++; } while (i<20);
if (i!=20)
  error, "***PARSER BUG*** try recompiling Yorick/parse.c unoptimized";
for (i=0 ; i<20 ; ++i);
if (i!=20)
  error, "***PARSER BUG*** try recompiling Yorick/parse.c unoptimized";
i= 0;
while (i<20) ++i;
if (i!=20)
  error, "***PARSER BUG*** try recompiling Yorick/parse.c unoptimized";
i= j= 0;
do { i++; if (i>15) break; else if (i>5) continue; j++; } while (i<20);
if (i!=16 || j!=5)
  error, "***PARSER BUG*** try recompiling Yorick/parse.c unoptimized";
for (i=j=0 ; i<20 ; ++i) { if (i>15) break; if (i>10) continue; ++j; }
if (i!=16 || j!=11)
  error, "***PARSER BUG*** try recompiling Yorick/parse.c unoptimized";
i= j= 0;
while (i<20) { ++i; if (i>15) break; if (i>10) continue; ++j; }
if (i!=16 || j!=10)
  error, "***PARSER BUG*** try recompiling Yorick/parse.c unoptimized";

if (do_stats) "C "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test 17x10x10 binary operators...";

/* Test all binary operations.  There are 10 data types and
   17 operators, so the complete test involves 1700 function calls... */
iS= 1n;  lS= 1;  dS= 1.0
cA= ['\1', '\2'];  sA= [1s, 2s];  iA= [1n, 2n];  lA= [1, 2];
fA= [1.0f, 2.0f];  dA= [1., 2.];  zA= [1+0i, 2+0i];

func op_test(SS, AS, SA, AA, op_name) /* SS, .. AA are correct answers */
{
  extern op;               /* the function to be tested */
  goof= array(1, 10, 10);  /* array to hold any mistakes */

  except_complex= only_integer= goof;
  except_complex(10,)= except_complex(,10)= 0;
  only_integer(3,)= only_integer(,3)= 0;
  only_integer(8:,)= 0;  only_integer(,8:)= 0;

  answer= SS;
  op,iS,iS,1,1; op,lS,iS,2,1; op,dS,iS,3,1;
  op,iS,lS,1,2; op,lS,lS,2,2; op,dS,lS,3,2;
  op,iS,dS,1,3; op,lS,dS,2,3; op,dS,dS,3,3;

  answer= AS;
  op,cA,iS,4,1; op,sA,iS,5,1; op,iA,iS,6,1; op,lA,iS,7,1;
  op,fA,iS,8,1; op,dA,iS,9,1; op,zA,iS,10,1;
  op,cA,lS,4,2; op,sA,lS,5,2; op,iA,lS,6,2; op,lA,lS,7,2;
  op,fA,lS,8,2; op,dA,lS,9,2; op,zA,lS,10,2;
  op,cA,dS,4,3; op,sA,dS,5,3; op,iA,dS,6,3; op,lA,dS,7,3;
  op,fA,dS,8,3; op,dA,dS,9,3; op,zA,dS,10,3;

  answer= SA;
  op,iS,cA,1,4; op,iS,sA,1,5; op,iS,iA,1,6; op,iS,lA,1,7;
  op,iS,fA,1,8; op,iS,dA,1,9; op,iS,zA,1,10;
  op,lS,cA,2,4; op,lS,sA,2,5; op,lS,iA,2,6; op,lS,lA,2,7;
  op,lS,fA,2,8; op,lS,dA,2,9; op,lS,zA,2,10;
  op,dS,cA,3,4; op,dS,sA,3,5; op,dS,iA,3,6; op,dS,lA,3,7;
  op,dS,fA,3,8; op,dS,dA,3,9; op,dS,zA,3,10;

  answer= AA;
  op,cA,cA,4,4; op,cA,sA,4,5; op,cA,iA,4,6; op,cA,lA,4,7;
  op,cA,fA,4,8; op,cA,dA,4,9; op,cA,zA,4,10;
  op,sA,cA,5,4; op,sA,sA,5,5; op,sA,iA,5,6; op,sA,lA,5,7;
  op,sA,fA,5,8; op,sA,dA,5,9; op,sA,zA,5,10;
  op,iA,cA,6,4; op,iA,sA,6,5; op,iA,iA,6,6; op,iA,lA,6,7;
  op,iA,fA,6,8; op,iA,dA,6,9; op,iA,zA,6,10;
  op,lA,cA,7,4; op,lA,sA,7,5; op,lA,iA,7,6; op,lA,lA,7,7;
  op,lA,fA,7,8; op,lA,dA,7,9; op,lA,zA,7,10;
  op,fA,cA,8,4; op,fA,sA,8,5; op,fA,iA,8,6; op,fA,lA,8,7;
  op,fA,fA,8,8; op,fA,dA,8,9; op,fA,zA,8,10;
  op,dA,cA,9,4; op,dA,sA,9,5; op,dA,iA,9,6; op,dA,lA,9,7;
  op,dA,fA,9,8; op,dA,dA,9,9; op,dA,zA,9,10;
  op,zA,cA,10,4; op,zA,sA,10,5; op,zA,iA,10,6; op,zA,lA,10,7;
  op,zA,fA,10,8; op,zA,dA,10,9; op,zA,zA,10,10;

  if (anyof(goof)) {
    goofs++;
    "**FAILURE** of the following operations "+op_name+":";
    where2(goof);
  }
}

if (do_stats) "D "+print(yorick_stats());

func op(l, r, il, ir)
{ goof(il, ir)= anyof((l + r)!=answer); }
op_test, 2, [2, 3], [2, 3], [2, 4], "+";

func op(l, r, il, ir)
{ goof(il, ir)= anyof((l - r)!=answer); }
op_test, 0, [0, 1], [0, -1], [0, 0], "-";

func op(l, r, il, ir)
{ goof(il, ir)= anyof((l * r)!=answer); }
op_test, 1, [1, 2], [1, 2], [1, 4], "*";

func op(l, r, il, ir)
{
  if (structof(l+r)!=structof(l+r+0.0f))
    goof(il, ir)= anyof((l / r)!=structof(l+r)(answer));
  else /* otherwise fails on Crays because division is inexact */
    goof(il, ir)= anyof(abs((l / r) - answer) > 1.e-6);
}
op_test, 1, [1, 2], [1, 0.5], [1, 1], "/";

func op(l, r, il, ir)
{
  if (structof(r)!=structof(r+0.0f))
    goof(il, ir)= anyof((l ^ r)!=answer);
  else /* otherwise fails on MacIntosh for unknown reason */
    goof(il, ir)= anyof(abs((l ^ r) - answer) > 1.e-6);
}
op_test, 1, [1, 2], [1, 1], [1, 4], "^";

func op(l, r, il, ir)
{ goof(il, ir)= anyof((l == r)!=answer); }
op_test, 1, [1, 0], [1, 0], [1, 1], "==";

func op(l, r, il, ir)
{ goof(il, ir)= anyof((l != r)!=answer); }
op_test, 0, [0, 1], [0, 1], [0, 0], "!=";

func op(l, r, il, ir)
{ goof(il, ir)= except_complex(il, ir) && anyof((l % r)!=answer); }
op_test, 0, [0, 0], [0, 1], [0, 0], "%";

func op(l, r, il, ir)
{ goof(il, ir)= except_complex(il, ir) && anyof((l > r)!=answer); }
op_test, 0, [0, 1], [0, 0], [0, 0], ">";

func op(l, r, il, ir)
{ goof(il, ir)= except_complex(il, ir) && anyof((l <= r)!=answer); }
op_test, 1, [1, 0], [1, 1], [1, 1], "<=";

func op(l, r, il, ir)
{ goof(il, ir)= except_complex(il, ir) && anyof((l < r)!=answer); }
op_test, 0, [0, 0], [0, 1], [0, 0], "<";

func op(l, r, il, ir)
{ goof(il, ir)= except_complex(il, ir) && anyof((l >= r)!=answer); }
op_test, 1, [1, 1], [1, 0], [1, 1], ">=";

func op(l, r, il, ir)
{ goof(il, ir)= only_integer(il, ir) && anyof((l << r)!=answer); }
op_test, 2, [2, 4], [2, 4], [2, 8], "<<";

func op(l, r, il, ir)
{ goof(il, ir)= only_integer(il, ir) && anyof((l >> r)!=answer); }
op_test, 0, [0, 1], [0, 0], [0, 0], ">>";

func op(l, r, il, ir)
{ goof(il, ir)= only_integer(il, ir) && anyof((l & r)!=answer); }
op_test, 1, [1, 0], [1, 0], [1, 2], "&";

func op(l, r, il, ir)
{ goof(il, ir)= only_integer(il, ir) && anyof((l | r)!=answer); }
op_test, 1, [1, 3], [1, 3], [1, 2], "|";

func op(l, r, il, ir)
{ goof(il, ir)= only_integer(il, ir) && anyof((l ~ r)!=answer); }
op_test, 0, [0, 3], [0, 3], [0, 0], "~";

op= op_test= [];
if (do_stats) "E "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test unary operators...";

/* Test all unary operators. */

func op_test(SS, AA, op_name) /* SS, AA are correct answers */
{
  extern op;               /* the function to be tested */
  goof= array(1, 10);      /* array to hold any mistakes */

  except_complex= only_integer= goof;
  except_complex(10)= 0;
  only_integer(3)= 0;
  only_integer(8:)= 0;

  answer= SS;
  op,iS,1; op,lS,2; op,dS,3;

  answer= AA&0xff;
  op,cA,4;
  answer= AA;
  op,sA,5; op,iA,6; op,lA,7; op,fA,8; op,dA,9; op,zA,10;

  if (anyof(goof)) {
    goofs++;
    "**FAILURE** of the following operations "+op_name+":";
    where2(goof);
  }
}

if (do_stats) "F "+print(yorick_stats());

func op(l, il)
{ goof(il)= anyof((+ l)!=answer); }
op_test, 1, [1, 2], "+";

func op(l, il)
{ goof(il)= anyof((- l)!=answer); }
op_test, -1, [-1, -2], "-";

func op(l, il)
{ goof(il)= anyof((! (l-1))!=answer); }
op_test, 1, [1, 0], "!";

func op(l, il)
{ goof(il)= only_integer(il) && anyof((~ l)!=answer); }
op_test, -2, [-2, -3], "~";

op= op_test= [];
if (do_stats) "G "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test array manipulation functions...";

/* Test array manipulation functions. */

func not_near(x,y)
{
  return anyof(abs(x-y)>1.e-9);
}

x= [0,1](-,) + [0,10,20](-,-,) + [0,100,200,300](-,-,-,) +
   [0,1000,2000,3000,4000](-,-,-,-,) +
   [0,10000,20000,30000,40000,50000](-,-,-,-,-,);
if (x(1,2,3,4,5,6)!=54321 || x(1,2,1,1,3,4)!=32001) {
  goofs++;
  "**FAILURE** of - subscript or broadcasting";
}

y= [];
grow, y, -2;
if (anyof(y!=-2)) { 
  goofs++;
  "**FAILURE** of grow test 1";
}
grow, y, [1,2,3];
if (anyof(y!=[-2,1,2,3])) { 
  goofs++;
  "**FAILURE** of grow test 2";
}
grow, y, [6,5,4];
if (anyof(y!=[-2,1,2,3,6,5,4])) { 
  goofs++;
  "**FAILURE** of grow test 3";
}
y= [[1,2,3],[4,5,6]];
grow, y, -1;
if (anyof(y!=[[1,2,3],[4,5,6],[-1,-1,-1]])) { 
  goofs++;
  "**FAILURE** of grow test 4";
}
grow, y, [6,5,4];
if (anyof(y!=[[1,2,3],[4,5,6],[-1,-1,-1],[6,5,4]])) { 
  goofs++;
  "**FAILURE** of grow test 5";
}

if (indgen(0)!=orgsof([1])(1) ||
    anyof(indgen(5)!=[0,1,2,3,4]+indgen(0))) {
  goofs++;
  "**FAILURE** of indgen function";
}

if (not_near(span(1,4,4), [1,2,3,4]) ||
    not_near(span(0,[2,4],3), [[0,1,2],[0,2,4]]) ||
    not_near(span(0,[2,4],3,0), [[0,0],[1,2],[2,4]]) ||
    not_near(spanl(1,8,4), [1,2,4,8]) ||
    not_near(spanl(1,[4,16],3,0), [[1,1],[2,4],[4,16]])) {
  goofs++;
  "**FAILURE** of span or spanl function";
}

y= [0., 1, 2, 3, 4, 5, 6, 7, 8, 9];
if (digitize(3.5, y)!=5 ||
    anyof(digitize([[-5, 8.5],[11,5],[.5,-.5]],y)!=[[1,10],[11,7],[2,1]]) ||
    anyof(digitize([[-5, 8.5],[11,5],[.5,-.5]],y(::-1))!=
          [[11,2],[1,5],[10,11]])) {
  goofs++;
  "**FAILURE** of digitize function";
}

if (interp(y, y, 3.5)!=3.5 ||
    anyof(interp(y,y,[[-5, 8.5],[11,5],[.5,-.5]])!=[[0,8.5],[9,5],[.5,0]]) ||
    anyof(interp([y,y],y,[[-5, 8.5],[11,5],[.5,-.5]])!=
          [[[0,8.5],[9,5],[.5,0]],[[0,8.5],[9,5],[.5,0]]]) ||
    anyof(interp(transpose([y,y]),y,[[-5, 8.5],[11,5],[.5,-.5]],0)!=
          [[[0,0],[8.5,8.5]],[[9,9],[5,5]],[[.5,.5],[0,0]]])) {
  goofs++;
  "**FAILURE** of interp function";
}

if (not_near(integ(y, y, 3.5), 0.5*3.5^2) ||
    not_near(integ(y,y,[[-5, 8.5],[11,5],[.5,-.5]]),
             0.5*[[0,8.5],[9,5],[.5,0]]^2)) {
  goofs++;
  "**FAILURE** of integ function";
}

if (anyof(histogram([1,5,2,1,1,5,2,1,4,5])!=[4,2,0,1,3]) ||
    anyof(histogram([1,5,2,1,1,5,2,1,4,5],top=7)!=[4,2,0,1,3,0,0]) ||
    anyof(histogram([1,5,2,1,1,5,2,1,4,5],y,top=7)!=
          [14.,8.,0.,8.,15.,0.,0.])) {
  goofs++;
  "**FAILURE** of histogram function";
}

if (anyof(poly([0.,1.,2.], 1,-2,1)!=[1.,0.,1.]) ||
    anyof(poly([0.,1.,2.], 1,[-2,-1,0],1)!=[1.,1.,5.])) {
  goofs++;
  "**FAILURE** of poly function";
}

if (anyof(sort([5,1,7,3])!=[2,4,1,3]) ||
    anyof(sort([5.,1.,7.,3.])!=[2,4,1,3]) ||
    anyof(sort(["go", "a", "stay", "abc"])!=[2,4,1,3]) ||
    median([5.,1.,7.,3.])!=4 || median([5.,1.,7.,3.,-2500.])!=3 ||
    anyof(median([[5.,1.,7.,3.],[5.,1.,99.,3.]])!=[4,4]) ||
    anyof(median([[5.,5.],[-55.,1.],[7.,99.],[3.,3.]],0)!=[4,4])) {
  goofs++;
  "**FAILURE** of sort or median function";
}

if (anyof(dimsof(x)                          != [6, 1,2,3,4,5,6]) ||
    anyof(dimsof(transpose(x))               != [6, 6,2,3,4,5,1]) ||
    anyof(dimsof(transpose(x,[1,2]))         != [6, 2,1,3,4,5,6]) ||
    anyof(dimsof(transpose(x,[1,0]))         != [6, 6,2,3,4,5,1]) ||
    anyof(dimsof(transpose(x,2))             != [6, 6,1,2,3,4,5]) ||
    anyof(dimsof(transpose(x,0))             != [6, 2,3,4,5,6,1]) ||
    anyof(dimsof(transpose(x,3))             != [6, 5,6,1,2,3,4]) ||
    anyof(dimsof(transpose(x,[4,6,3],[2,5])) != [6, 1,5,6,3,2,4])) {
  goofs++;
  "**FAILURE** of transpose test 1";
}
y= transpose(x,[4,6,3],[2,5]);
if (y(1,5,6,3,2,4)!=x(1,2,3,4,5,6) || y(1,3,4,1,2,1)!=x(1,2,1,1,3,4)) {
  goofs++;
  "**FAILURE** of transpose test 2";
}

x= y= [];
if (do_stats) "H "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test struct instancing and indexing...";

/* Test structs. */

struct Stest {
  char a;
  short b;
  double c(4);
  int d(2,3), e(5);
  complex f(2);
}

x= Stest(a='A', b=13, c=[2,-4,6,-8],
         d=[[-1,2],[-3,4],[-5,6]], e=[10,20,30,40,50], f=[1i,2-2i]);
if (x.a!='A' || x.b!=13 || anyof(x.c!=[2.,-4.,6.,-8.]) ||
    anyof(x.d!=[[-1,2],[-3,4],[-5,6]]) || anyof(x.e!=[10,20,30,40,50]) ||
    anyof(x.f!=[1i,2-2i])) {
  goofs++;
  "**FAILURE** of - struct instance declaration";
}

y= array(Stest, 2);
y(..)= x;
y.a(2)= 'B';
y(2).b= -x.b;
y.c(..,2)= x.c(::-1);
y(2).d(,1:2)= transpose(x.d(,1:2));
y.e(::-1,2)= x.e;
y(2).f= conj(x.f);

if (x!=y(1) || y(2).a!='B' || y(2).b!=-13 || anyof(y(2).c!=[-8.,6.,-4.,2.]) ||
    anyof(y(2).d!=[[-1,-3],[2,4],[-5,6]]) ||
    anyof(y(2).e!=[50,40,30,20,10]) || anyof(y(2).f!=[-1i,2+2i])) {
  goofs++;
  "**FAILURE** of - struct instance array indexing";
}

x= y= [];
if (do_stats) "I "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test range functions...";

/* Test range functions. */

x= [[[3,7,5],[-4,2,-6]], [[-1,-4,-2],[0,4,8]],
    [[-1,-5,2],[1,0,0]], [[9,8,7],[-9,9,-6]]];
y= x+0.5;

if (anyof(x(,-:1:2,,1)!=[[[3,7,5],[3,7,5]],[[-4,2,-6],[-4,2,-6]]])) {
  goofs++;
  "**FAILURE** of - pseudo range function (-)";
}

if (anyof(x(,..)!=x) || anyof(x(..,:)!=x) || anyof(x(,*)!=x(,1:8)) ||
    anyof(x(*,)!=x(1:6,1,))) {
  goofs++;
  "**FAILURE** of - rubber range function (.. or *)";
}

if (anyof(x(,pcen,)(,uncp,)!=x) || anyof(y(,pcen,)(,uncp,)!=y)) {
  goofs++;
  "**FAILURE** of - uncp range function";
}

if (anyof(x(,pcen,)(,2:-1,)!=x(,zcen,)) ||
    anyof(x(,pcen,)(,1,)!=x(,1,)) || anyof(x(,pcen,)(,0,)!=x(,0,)) ||
    anyof(y(,pcen,)!=x(,pcen,)+0.5)) {
  goofs++;
  "**FAILURE** of - pcen range function";
}

if (anyof(x(,zcen,)!=[[[-.5,4.5,-.5]],[[-.5,0,3]],
                      [[0,-2.5,1]],[[0,8.5,.5]]]) ||
    anyof(y(,zcen,)!=[[[-.5,4.5,-.5]],[[-.5,0,3]],
                      [[0,-2.5,1]],[[0,8.5,.5]]] + 0.5)) {
  goofs++;
  "**FAILURE** of - zcen range function";
}

if (anyof(x(,dif,)!=[[[-7,-5,-11]],[[1,8,10]],[[2,5,-2]],[[-18,1,-13]]]) ||
    anyof(y(,dif,)!=[[[-7,-5,-11]],[[1,8,10]],[[2,5,-2]],[[-18,1,-13]]])) {
  goofs++;
  "**FAILURE** of - dif range function";
}

if (anyof(x(,psum,)!=[[[3,7,5],[-1,9,-1]], [[-1,-4,-2],[-1,0,6]],
                      [[-1,-5,2],[0,-5,2]], [[9,8,7],[0,17,1]]]) ||
    anyof(y(,psum,)!=[[[3,7,5],[-1,9,-1]], [[-1,-4,-2],[-1,0,6]],
                      [[-1,-5,2],[0,-5,2]], [[9,8,7],[0,17,1]]] +
          [0.5,1.0](-,))) {
  goofs++;
  "**FAILURE** of - psum range function";
}

if (anyof(x(,cum,)!=[[[0,0,0],[3,7,5],[-1,9,-1]],
                     [[0,0,0],[-1,-4,-2],[-1,0,6]],
                     [[0,0,0],[-1,-5,2],[0,-5,2]],
                     [[0,0,0],[9,8,7],[0,17,1]]]) ||
    anyof(y(,cum,)!=[[[0,0,0],[3,7,5],[-1,9,-1]],
                     [[0,0,0],[-1,-4,-2],[-1,0,6]],
                     [[0,0,0],[-1,-5,2],[0,-5,2]],
                     [[0,0,0],[9,8,7],[0,17,1]]] +
          [0.0,0.5,1.0](-,))) {
  goofs++;
  "**FAILURE** of - cum range function";
}

if (anyof(x(zcen,dif,)!=[[[-6,-8]],[[4.5,9]],[[3.5,1.5]],[[-8.5,-6]]]) ||
    anyof(y(zcen,dif,)!=[[[-6,-8]],[[4.5,9]],[[3.5,1.5]],[[-8.5,-6]]])) {
  goofs++;
  "**FAILURE** of - zcen,dif multiple range function";
}

if (anyof(x(min,,max)!=[7,0]) || anyof(y(,,max)(min,)!=[7,1]+0.5)) {
  goofs++;
  "**FAILURE** of - min or max range function";
}

if (anyof(x(,ptp,)!=[[-7,-5,-11],[1,8,10],[2,5,-2],[-18,1,-13]]) ||
    anyof(y(,ptp,)!=[[-7,-5,-11],[1,8,10],[2,5,-2],[-18,1,-13]])) {
  goofs++;
  "**FAILURE** of - ptp range function";
}

if (anyof(x(,mnx,)!=[[2, 2, 2], [1, 1, 1], [1, 1, 2], [2, 1, 2]]) ||
    anyof(y(,mnx,)!=[[2, 2, 2], [1, 1, 1], [1, 1, 2], [2, 1, 2]])) {
  goofs++;
  "**FAILURE** of - mnx range function";
}

if (anyof(x(,mxx,)!=3-x(,mnx,)) ||
    anyof(y(,mxx,)!=3-y(,mnx,))) {
  goofs++;
  "**FAILURE** of - mxx range function";
}

if (anyof(x(,sum,)!=x(,1,)+x(,2,)) ||
    anyof(y(,sum,)!=y(,1,)+y(,2,))) {
  goofs++;
  "**FAILURE** of - sum range function";
}

if (anyof(x(,avg,)!=0.5*(x(,1,)+x(,2,))) ||
    anyof(y(,avg,)!=0.5*(y(,1,)+y(,2,)))) {
  goofs++;
  "**FAILURE** of - avg range function";
}

if (anyof(abs(x(,rms,)-0.5*abs(x(,1,)-x(,2,)))>1.e-10) ||
    anyof(abs(y(,rms,)-0.5*abs(y(,1,)-y(,2,)))>1.e-10)) {
  goofs++;
  "**FAILURE** of - rms range function";
}

x= [[1,2,3],[-5,5,-8]];
y= [[1,1],[-1,-1],[0,1]];

if (anyof(x(+,)*y(,+) != [[-1,-10],[2,-18]]) ||
    anyof(x(,+)*y(+,) != [[-4,7,-5],[4,-7,5],[-5,5,-8]])) {
  goofs++;
  "**FAILURE** of + matrix multiply function";
}

x+= 0i;

if (anyof(x(+,)*y(,+) != [[-1,-10],[2,-18]]) ||
    anyof(x(,+)*y(+,) != [[-4,7,-5],[4,-7,5],[-5,5,-8]])) {
  goofs++;
  "**FAILURE** of + complex matrix multiply function";
}

/* first test matrix multiply conformability rules
   -- this will just blow up if there's a problem */
rop= lop= array(0., 4, 3, 2);
dst= lop(+,,)*rop(+,,) + array(0., 3,2,3,2);
dst= lop(,+,)*rop(,+,) + array(0., 4,2,4,2);
dst= lop(,,+)*rop(,,+) + array(0., 4,3,4,3);
rop= transpose(rop, 2);
dst= lop(+,,)*rop(,+,) + array(0., 3,2,2,3);
dst= lop(,+,)*rop(,,+) + array(0., 4,2,2,4);
dst= lop(,,+)*rop(+,,) + array(0., 4,3,4,3);
rop= transpose(rop, 2);
dst= lop(+,,)*rop(,,+) + array(0., 3,2,3,2);
dst= lop(,+,)*rop(+,,) + array(0., 4,2,2,4);
dst= lop(,,+)*rop(,+,) + array(0., 4,3,3,4);

/* next, try to exercise all the branches of the matrix
   multiply routines -- test all five dimensions,
   plus unit length leading dimension (special branch) */
lop= [[[1,2],[3,4]],[[5,6],[7,8]]];
rop= lop+10;
dst1= lop(,+,)*rop(,+,);
dst2= lop(1,+,)*rop(,+,);
if (anyof(dst1!=
          [[[[50,74],[146,170]],[[54,80], [158,184]]],
          [[[66,98],[194,226]],[[70,104],[206,240]]]]) ||
    anyof(dst2!=
          [[[50,146],[54,158]],[[66,194],[70,206]]])) {
  goofs++;
  "**FAILURE** of + matrix multiply function";
}

lop+= 0i;
rop+= 0i;

dst1= lop(,+,)*rop(,+,);
dst2= lop(1,+,)*rop(,+,);
if (anyof(dst1!=
          [[[[50,74],[146,170]],[[54,80], [158,184]]],
          [[[66,98],[194,226]],[[70,104],[206,240]]]]) ||
    anyof(dst2!=
          [[[50,146],[54,158]],[[66,194],[70,206]]])) {
  goofs++;
  "**FAILURE** of + complex matrix multiply function";
}

x= y= lop= rop= dst1= dst2= [];
if (do_stats) "J "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test math functions...";

/* Test math functions. */

x= pi/4;  y= [3*pi/4, pi/6];

if (not_near(sin(x),sqrt(0.5)) || not_near(sin(y),[sqrt(0.5),0.5]) ||
    not_near(cos(x),sqrt(0.5)) || not_near(cos(y),[-sqrt(0.5),sqrt(.75)]) ||
    not_near(tan(x),1) || not_near(tan(y),[-1,1/sqrt(3)])) {
  goofs++;
  "**FAILURE** of - sin, cos, tan, or sqrt function";
}

if (not_near(asin(sqrt(0.5)),x) ||
    not_near(asin([sqrt(0.5),0.5]),y-[pi/2,0]) ||
    not_near(acos(sqrt(0.5)),x) || not_near(acos([-sqrt(0.5),sqrt(.75)]),y) ||
    not_near(atan(1),x) || not_near(atan([-1,1/sqrt(3)]),y-[pi,0])) {
  goofs++;
  "**FAILURE** of - asin, acos, atan, or sqrt function";
}

if (not_near(atan(5,5),x) || not_near(atan([.1,1],[-.1,sqrt(3)]),y)) {
  goofs++;
  "**FAILURE** of - 2 argument atan function";
}

if (not_near(exp(1i*x),cos(x)+1i*sin(x)) ||
    not_near(exp(1i*y),cos(y)+1i*sin(y)) ||
    not_near(cos(1i*x), 0.5*(exp(-x)+exp(x))) ||
    not_near(cos(1i*y), 0.5*(exp(-y)+exp(y))) ||
    not_near(sin(1i*x), 0.5i*(exp(x)-exp(-x))) ||
    not_near(sin(1i*y), 0.5i*(exp(y)-exp(-y))) ||
    not_near(tan(1i*x), 1i*(exp(x)-exp(-x))/(exp(-x)+exp(x))) ||
    not_near(tan(1i*y), 1i*(exp(y)-exp(-y))/(exp(-y)+exp(y)))) {
  goofs++;
  "**FAILURE** of - (complex) exp, sin, cos, or tan function";
}

if (not_near(exp(1),2.718281828459) ||
    not_near(exp([-.5,2.5]), [1,exp(1)^3]/sqrt(exp(1))) ||
    not_near(exp(x),cosh(x)+sinh(x)) ||
    not_near(exp(y),cosh(y)+sinh(y)) ||
    not_near(cosh(x), 0.5*(exp(-x)+exp(x))) ||
    not_near(cosh(y), 0.5*(exp(-y)+exp(y))) ||
    not_near(sinh(x), 0.5*(exp(x)-exp(-x))) ||
    not_near(sinh(y), 0.5*(exp(y)-exp(-y))) ||
    not_near(tanh(x), (exp(x)-exp(-x))/(exp(-x)+exp(x))) ||
    not_near(tanh(y), (exp(y)-exp(-y))/(exp(-y)+exp(y)))) {
  goofs++;
  "**FAILURE** of -  exp, sinh, cosh, or tanh function";
}

if (not_near(sech(x), 2/(exp(-x)+exp(x))) ||
    not_near(sech(y), 2/(exp(-y)+exp(y))) ||
    not_near(csch(x), 2/(exp(x)-exp(-x))) ||
    not_near(csch(y), 2/(exp(y)-exp(-y))) ||
    anyof(sech([1.e6,-1.e6])) || anyof(csch([1.e6,-1.e6]))) {
  goofs++;
  "**FAILURE** of -  sech or csch function";
}

if (not_near(acosh(cosh(x)), x) || not_near(acosh(cosh(y)), y) ||
    not_near(asinh(sinh(x)), x) || not_near(asinh(sinh(y)), y) ||
    not_near(atanh(tanh(x)), x) || not_near(atanh(tanh(y)), y)) {
  goofs++;
  "**FAILURE** of -  acosh, asinh, or atanh function";
}

if (not_near(exp(1i*x),cosh(1i*x)+sinh(1i*x)) ||
    not_near(exp(1i*y),cosh(1i*y)+sinh(1i*y)) ||
    not_near(cosh(1i*x), 0.5*(exp(-1i*x)+exp(1i*x))) ||
    not_near(cosh(1i*y), 0.5*(exp(-1i*y)+exp(1i*y))) ||
    not_near(sinh(1i*x), 0.5*(exp(1i*x)-exp(-1i*x))) ||
    not_near(sinh(1i*y), 0.5*(exp(1i*y)-exp(-1i*y))) ||
    not_near(tanh(1i*x), (exp(1i*x)-exp(-1i*x))/(exp(-1i*x)+exp(1i*x))) ||
    not_near(tanh(1i*y), (exp(1i*y)-exp(-1i*y))/(exp(-1i*y)+exp(1i*y)))) {
  goofs++;
  "**FAILURE** of -  (complex) exp, sinh, cosh, or tanh function";
}

if (not_near(log(exp(x)), x) || not_near(log(exp(y)), y) ||
    not_near(log10(10^x), x) || not_near(log10(10^y), y) ||
    not_near(log10(x*y),log10(x)+log10(y)) ||
    not_near(log(x*y),log(x)+log(y)) ||
    not_near(exp(x+y),exp(x)*exp(y)) ||
    not_near(log10([1.e5,1.e-7]),[5,-7]) ||
    not_near(log(10),1/log10(exp(1))) ||
    not_near(log(10)*log10(x),log(x)) || not_near(log(10)*log10(y),log(y))) {
  goofs++;
  "**FAILURE** of -  log, log10, or exp function";
}

if (anyof(abs(x)!=x) || anyof(abs(-x)!=x) ||
    anyof(abs(y)!=y) || anyof(abs(-y)!=y)) {
  goofs++;
  "**FAILURE** of -  abs function";
}

if (anyof(ceil(3.7)!=4) || anyof(ceil([-3.7,2.1])!=[-3,3]) ||
    anyof(floor(3.7)!=3) || anyof(floor([-3.7,2.1])!=[-4,2])) {
  goofs++;
  "**FAILURE** of -  ceil or floor function";
}

if (not_near(abs(x,y,x,y),sqrt(2*(x^2+y^2)))) {
  goofs++;
  "**FAILURE** of -  multiargument abs function";
}

if (anyof(sign(x)!=1) || anyof(sign(-x)!=-1) ||
    anyof(sign(y)!=1) || anyof(sign(-y)!=-1) ||
    sign(0)!=1 || sign(0.0)!=1 || sign(0i)!=1 ||
    not_near(sign(exp(1i*y+x)),exp(1i*y))) {
  goofs++;
  "**FAILURE** of -  sign function";
}

if (conj(x+1i)!=x-1i || anyof(conj(y+1i)!=y-1i)) {
  goofs++;
  "**FAILURE** of -  conj function";
}

if (random()<0.0 || random()>1.0 ||
    anyof(dimsof(random(3,4,2))!=[3,3,4,2])) {
  goofs++;
  "**FAILURE** of -  random function";
}

if (min(x)!=x || min(y)!=pi/6 || anyof(min(x,y)!=[pi/4,pi/6]) ||
    max(x)!=x || max(y)!=3*pi/4 || anyof(max(x,y)!=[3*pi/4,pi/4])) {
  goofs++;
  "**FAILURE** of -  min or max function";
}

if (sum(x)!=x || not_near(sum(y), 11*pi/12) ||
    avg(x)!=x || not_near(avg(y), 11*pi/24)) {
  goofs++;
  "**FAILURE** of -  sum or avg function";
}

if (allof([1,0]) || !allof([1,1]) || anyof([0,0]) || !anyof([1,0]) ||
    noneof([1,0]) || !noneof([0,0]) || nallof([1,1]) || !nallof([1,0])) {
  goofs++;
  "**FAILURE** of -  allof, anyof, noneof, or nallof function";
}

if (anyof(where([[0,1,0,0],[0,0,0,1]])!=[2,8]) ||
    anyof(where2([[0,1,0,0],[0,0,0,1]])!=[[2,1],[4,2]])) {
  goofs++;
  "**FAILURE** of -  where or where2 function";
}

x= Stest(a='A', b=13, c=[2,-4,6,-8],
         d=[[-1,2],[-3,4],[-5,6]], e=[10,20,30,40,50], f=[1i,2-2i]);
y= array(x, 2);
y(1).b= 8;  y(2).b=19;
if (anyof(merge(cA,iS,[1,1,0])!=[1,2,1]) ||
    anyof(merge(lS,sA,[0,0,1])!=[1,2,1]) ||
    anyof(merge(iA,dS,[1,0,1])!=[1,1,2]) ||
    anyof(merge(lA,fA,[[1,1],[0,0]])!=[[1,2],[1,2]]) ||
    anyof(merge(zA,dA,[[1,0],[0,1]])!=[[1,1],[2,2]]) ||
    anyof(merge(cA,cA,[[1,0],[0,1]])!=[[1,1],[2,2]]) ||
    anyof(merge(sA,sA,[[1,0],[0,1]])!=[[1,1],[2,2]]) ||
    anyof(merge(y,x,[1,0,1])!=[y(1),x,y(2)]) ||
    anyof(merge(dA,[],[1,1])!=dA) ||
    anyof(merge([],lA,[0,0])!=lA) ||
    anyof(merge2(lA,zA(::-1),[1,0])!=[1,1])) {
  goofs++;
  "**FAILURE** of -  merge or merge2 function";
}
x= y= [];

if (do_stats) "K "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test informational functions...";

/* Test informational functions. */

if (structof(3.5)!=double || structof('\61')!=char ||
    structof([4,5,6])!=long || structof([1n,-1n])!=int ||
    structof([3s,4s])!=short || structof(4.4f)!=float ||
    structof(1i)!=complex || structof(array(Stest,2,2))!=Stest ||
    structof([&[1,2,3],&[],&[3.5,1.2]])!=pointer || structof("yo")!=string) {
  goofs++;
  "**FAILURE** of - structof function or structure != operation";
}

if (anyof(dimsof([[2,4,6],[1,3,5]])!=[2,3,2]) || anyof(dimsof(5)!=[0]) ||
    anyof(dimsof(array(short,5,-4:-1,3:5,0:1))!=[4,5,4,3,2]) ||
    anyof(dimsof([1,2,3](-,),[1,2])!=[2,2,3])) {
  goofs++;
  "**FAILURE** of - dimsof function";
}

dummy= use_origins(1);
if (anyof(orgsof([[2,4,6],[1,3,5]])!=[2,indgen(0),indgen(0)]) ||
    anyof(orgsof(array(short,5,-4:-1,3:5,0:1))!=[4,indgen(0),-4,3,0])) {
  goofs++;
  "**FAILURE** of - orgsof function";
}
dummy= [];

if (numberof([[2,4,6],[1,3,5]])!=6 || numberof(3.5)!=1 || numberof([])!=0 ||
    numberof(array(short,5,-4:-1,3:5,0:1))!=120) {
  goofs++;
  "**FAILURE** of - numberof function";
}

if (sizeof([[2,4,6],[1,3,5]])!=6*sizeof(long) || sizeof(3.5)!=sizeof(double) ||
    sizeof(array(short,5,-4:-1,3:5,0:1))!=120*sizeof(short)) {
  goofs++;
  "**FAILURE** of - sizeof function";
}

if (typeof(3.5)!="double" || typeof('\61')!="char" ||
    typeof([4,5,6])!="long" || typeof([1n,-1n])!="int" ||
    typeof([3s,4s])!="short" || typeof(4.4f)!="float" ||
    typeof(1i)!="complex" || typeof(array(Stest,2,2))!="struct_instance" ||
    typeof(Stest)!="struct_definition" || typeof(3:52:4)!="range" ||
    typeof([])!="void" || typeof()!="void" || typeof("yo")!="string" ||
    typeof(&[3,4])!="pointer") {
  goofs++;
  "**FAILURE** of - typeof function";
}

if (nameof(Stest)!="Stest" || nameof(not_near)!="not_near") {
  goofs++;
  "**FAILURE** of - nameof function";
}

if (!is_array([3,4]) || !is_array(0) || is_array() || is_array(not_near) ||
    is_array(Stest)) {
  goofs++;
  "**FAILURE** of - is_array function";
}

if (is_void(7) || !is_void() || !is_void([]) || is_void(not_near)) {
  goofs++;
  "**FAILURE** of - is_void function";
}

if (is_func(7) || is_func() || !is_func(not_near) || is_func(Stest)) {
  goofs++;
  "**FAILURE** of - is_func function";
}

if (is_struct(7) || is_struct() || is_struct(not_near) ||
    !is_struct(Stest)) {
  goofs++;
  "**FAILURE** of - is_struct function";
}

if (is_range(7) || is_range() || is_range(not_near) ||
    is_range(Stest) || !is_range(3:4)) {
  goofs++;
  "**FAILURE** of - is_range function";
}

func junk(x)
{
  extern junk_test;
  return junk_test= am_subroutine();
}
junk_test= 0;
junk;
if (!junk_test || junk()) {
  goofs++;
  "**FAILURE** of - am_subroutine function";
}

if (do_stats) "L "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test func declarations...";

/* Test func declarations. */

func junk(&w,x,&y,z,..,k=,l=,m=)
{
  rslt= [w,x,y,z,k,l,m];
  while (more_args()) grow, rslt, next_arg();
  w=x=y=z=k=l=m=16;
  return rslt;
}
a= b= c= d= -2;
if (anyof(junk(k=5,a,b,m=c,3,4,8,9,l=d,10,11)!=[-2,-2,3,4,5,-2,-2,8,9,10,11])
    || a!=16 || b!=-2 || c!=-2 || d!=-2) {
  goofs++;
  "**FAILURE** of - complicated func declaration";
}
junk= [];

/* ------------------------------------------------------------------------- */

write, "Test binary I/O functions...";

/* Test binary I/O functions. */

f= createb("junkb.pdb");

if (is_stream(7) || is_stream() || is_stream(not_near) ||
    is_stream(Stest) || !is_stream(f)) {
  goofs++;
  "**FAILURE** of - is_stream function";
}

x= ["whoa", "okay"];
y= [&(1+0), &[1.5,2.5,3.5], &[]];
z= Stest(a='A', b=13, c=[2,-4,6,-8],
         d=[[-1,2],[-3,4],[-5,6]], e=[10,20,30,40,50], f=[1i,2-2i]);

save, f, x, y, z;
close, f;
f= updateb("junkb.pdb");
save, f, iS, lS, dS;
save, f, cA, sA, iA, lA, fA, dA, zA;
f.sA= [-91,57];
close, f;

f= openb("junkb.pdb");
x= y= z= [];
restore, f, x, y, z;
if (typeof(x)!="string" || typeof(y)!="pointer" ||
    anyof(dimsof(x)!=[1,2]) || anyof(dimsof(y)!=[1,3]) ||
    anyof(x!=["whoa", "okay"]) || typeof(*y(1))!="long" ||
    !is_void(*y(3)) || anyof(*y(2)!=[1.5,2.5,3.5]) ||
    structof(z)!=Stest || z.a!='A' || anyof(dimsof(z.d)!=[2,2,3]) ||
    anyof(dimsof(z.f)!=[1,2]) || anyof(z.f!=[1i,2-2i])) {
  goofs++;
  "**FAILURE** of - restore or save function";
}
if (f.iS!=iS || f.lS!=lS || f.dS!=dS ||
    anyof(f.cA!=cA) || anyof(f.sA!=[-91,57]) || anyof(f.iA!=iA) ||
    anyof(f.lA!=lA) || anyof(f.fA!=fA) || anyof(f.dA!=dA) ||
    anyof(f.zA!=zA) || typeof(f.cA)!="char" || typeof(f.sA)!="short" ||
    typeof(f.iA)!="int" || typeof(f.lA)!="long" || typeof(f.fA)!="float" ||
    typeof(f.dA)!="double" || typeof(f.zA)!="complex") {
  goofs++;
  "**FAILURE** of - f.var syntax or save function";
}
close, f;

remove, "junkb.pdb";

/* try reading and writing a netCDF file */
write, "Test binary I/O to netCDF...";

require, "netcdf.i";
f= nc_create("junkb.nc");
nc_vardef,f, "lS", template=lS, record=1;
nc_vardef,f, "dS", template=dS;
nc_vardef,f, "cA", template=cA;
nc_vardef,f, "sA", template=sA;
nc_vardef,f, "lA", template=lA, record=1;
nc_vardef,f, "fA", template=fA;
nc_vardef,f, "dA", template=dA, record=1;
f= nc_enddef(f);
f.dS= dS;  f.cA= cA;  f.sA= sA;  f.fA= fA;
nc_addrec, f;
save,f, lS,lA,dA;
nc_addrec, f;
save,f, lS,lA,dA;
nc_addrec, f;
save,f, lS,lA,dA;
close, f;
remove, "junkb.ncL";

f= openb("junkb.nc");
if (f.lS!=lS || f.dS!=dS ||
    anyof(f.cA!=cA) || anyof(f.sA!=sA) ||
    anyof(f.lA!=lA) || anyof(f.fA!=fA) || anyof(f.dA!=dA) ||
    typeof(f.cA)!="char" || typeof(f.sA)!="short" ||
    typeof(f.lA)!="long" || typeof(f.fA)!="float" ||
    typeof(f.dA)!="double") {
  goofs++;
  "**FAILURE** of - f.var syntax or save to netCDF";
}

jr,f, 3;
if (f.lS!=lS ||  anyof(f.lA!=lA) || anyof(f.dA!=dA)) {
  goofs++;
  "**FAILURE** of - f.var syntax or save to netCDF";
}

close, f;
remove, "junkb.nc";

if (do_stats) "M "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test ASCII I/O functions...";

/* Test ASCII I/O functions. */

f= open("junkt.txt", "w");
write,f, "The first line.";
write,f, dA;
write,f, sA-7, fA+5;
write,f, format="blah %s %d %e\n", "wow", lA+6, dA-20;
close,f;

f= open("junkt.txt", "r+");
if (rdline(f)!=" The first line.") {
  goofs++;
  "**FAILURE** of - rdline or write function";
}
backup, f;
if (rdline(f)!=" The first line.") {
  goofs++;
  "**FAILURE** of - backup function";
}
mark= bookmark(f);
x= 0*dA;
if (read(f,x)!=2 || anyof(x!=dA)) {
  goofs++;
  "**FAILURE** of - read or write function";
}
y= 0*sA;
if (read(f,y,x)!=4 || anyof(y!=sA-7) || anyof(x!=fA+5)) {
  goofs++;
  "**FAILURE** of - read or write function ";
}
y= 0*lA;
mark2= bookmark(f);
if (read(f, format="blah wow %d %e\n", y,x)!=4 ||
    anyof(y!=lA+6) || anyof(x!=dA-20)) {
  backup, f, mark2;
  if (read(f, format="blah wow %d %e", y,x)!=4 ||
      anyof(y!=lA+6) || anyof(x!=dA-20)) {
    goofs++;
    "**FAILURE** of -  formatted read or write function";
  } else {
    /* this OS does not like trailing \n in read formats */
    "**WARNING** Yorick formatted read peculiarity -- see testp.i";
  }
}
backup, f, mark;
if (read(f,x)!=2 || anyof(x!=dA)) {
  goofs++;
  "**FAILURE** of -  bookmark or backup function";
}
write,f, "Last line.";
close, f;
f= open("junkt.txt");
if (rdline(f,7)(7)!=" Last line.") {
  goofs++;
  "**FAILURE** of -  write to append to end of text";
}
close, f;

remove, "junkt.txt";

if (do_stats) "N "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test string manipulation functions...";

/* Test string manipulation functions. */

if (strlen("abc")!=3 ||
    anyof(strlen([[string(),"","a"],["axx","ab","abcd"]])!=
          [[0,0,1],[3,2,4]])) {
  goofs++;
  "**FAILURE** of - strlen function";
}

if (anyof(strtok("abc    1.23    xxx")!=["abc", "   1.23    xxx"]) ||
    anyof(strtok(["abc    1.23    xxx","c","1.5"], "\t c")!=
          [["ab", "    1.23    xxx"],string(),["1.5",string()]])) {
  goofs++;
  "**FAILURE** of - strtok function";
}

if (!strmatch("abc", "b") || strmatch("abc", "B") ||
    !strmatch("abc", "B", 1) ||
    anyof(strmatch(["abc","aBC"], "B")!=[0,1])) {
  goofs++;
  "**FAILURE** of - strmatch function";
}

if (strpart("abc", 1:1)!="a" || strpart("abc", 2:10)!="bc" ||
    strpart("abc", :-1)!="ab" || strpart("abc", :-5)!="" ||
    anyof(strpart(["abc","yowza"],3:)!=["c","wza"])) {
  goofs++;
  "**FAILURE** of - strpart function";
}

if (do_stats) "O "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test list functions...";

l= _lst(1.5, structof(z), _lst([],z), _prt);
#if 0
write, "<Begin output from _prt list (15 lines gibberish)>";
_prt, l;
write, "<End output from _prt list (15 lines gibberish)>";
#endif
if (_len(l)!=4) {
  goofs++;
  "**FAILURE** of - _lst or _len function";
}
if (_car(l)!=1.5 || _car(l,1)!=1.5 || _car(l,2)!=Stest ||
    typeof(_car(l,3))!="list" || _car(l,4)!=_prt ||
    !is_void(_car(_car(l,3)))) {
  goofs++;
  "**FAILURE** of - _lst or _car function";
}
if (_car(_cdr(l))!=Stest || _car(_cdr(l,3))!=_prt ||
    !is_void(_cdr(l,4))) {
  goofs++;
  "**FAILURE** of - _cdr function";
}
m= _cpy(l,2);
if (_len(m)!=2 || _car(m)!=1.5 || _car(m,2)!=Stest || _len(_cpy(l))!=4) {
  goofs++;
  "**FAILURE** of - _cpy function";
}
if (_car(m,2,2.5)!=Stest || _car(l,2)!=Stest || _car(m,2)!=2.5) {
  goofs++;
  "**FAILURE** of - _car set function";
}
n= _cat(m, _cpy(_cdr(l,2)));
if (n!=m || _len(m)!=4 || _len(n)!=4 || _car(n,4)!=_prt) {
  goofs++;
  "**FAILURE** of - _cat function";
}
if (_car(_cdr(m,3,[]))!=_prt || !is_void(_cdr(n,3)) ||
    !is_void(_cdr(n,3,_lst(_len))) || _car(m,4)!=_len ||
    _car(l,4)!=_prt) {
  goofs++;
  "**FAILURE** of - _cdr set function";
}
n= _map(typeof, m);
if (_car(n)!="double" || _car(n,2)!="double" ||
    _car(n,3)!="list" || _car(n,4)!="builtin") {
  goofs++;
  "**FAILURE** of - _map set function";
}
m= _rev(m);
if (_car(m,4)!=1.5 || _car(m,3)!=2.5 ||
    typeof(_car(m,2))!="list" || _car(m)!=_len ||
    !is_void(_car(_car(m,2)))) {
  goofs++;
  "**FAILURE** of - _rev function";
}
l= m= n= [];

if (do_stats) "P "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

write, "Test catch function...";

func junk(type)
{
  if (catch(0x01)) {
    if (type!=0x01) {
      goofs++;
      "**FAILURE** of - catch function - misidentified error as math";
    }
    return 0x01;
  }
  if (catch(0x02)) {
    if (type!=0x02) {
      goofs++;
      "**FAILURE** of - catch function - misidentified error as io";
    }
    return 0x02;
  }
  if (catch(0x04)) {
    if (type!=0x04) {
      goofs++;
      "**FAILURE** of - catch function - misidentified error as C-c";
    }
    return 0x04;
  }
  if (catch(0x08)) {
    if (type!=0x08) {
      goofs++;
      "**FAILURE** of - catch function - misidentified error as YError";
    }
    return 0x08;
  }
  if (catch(0x10)) {
    if (type!=0x10) {
      goofs++;
      "**FAILURE** of - catch function - misidentified error as interpreted";
    } else if (catch_message!="---test error, should be caught---") {
      goofs++;
      "**FAILURE** of - catch function - catch_message set incorrectly";
    }
    return 0x10;
  }
  if (type==0x01) x= 1.0/0.0;
  if (type==0x02) f= open("no-such-file-ever-existed");
  if (type==0x04) return 0x04; /* need user to hit C-c */
  if (type==0x08) x= 1.0*[];
  if (type==0x10) error, "---test error, should be caught---";
  return 0;
}

if (!junk(0x01)) "**WARNING** 1.0/0.0 does not trigger SIGFPE";
if (!junk(0x02)) {
  goofs++;
  "**FAILURE** of - catch function - I/O error not caught";
}
if (!junk(0x08)) {
  goofs++;
  "**FAILURE** of - catch function - compiled error not caught";
}
if (!junk(0x10)) {
  goofs++;
  "**FAILURE** of - catch function - interpreted error not caught";
}

junk= [];

if (do_stats) "Q "+print(yorick_stats());

/* ------------------------------------------------------------------------- */

iS= lS= dS= cA= sA= iA= lA= fA= dA= zA= [];
write, format= "End of Yorick parser test, %d goofs\n", goofs;

#include "teststr.i"
#include "testoxy.i"

if (!skip_testb) {
  require, "testb.i";
  write,"\n Zeroth test is pdtest files:";  pdcheck2;  write,"";
  testb;
}

/* write if tests twice so that include actually takes place */
if (!skip_test1) include, "test1.i";
if (!skip_test1) { write,"\nShock tracker timing test:";  test1, 20; }

if (!skip_test2) include, "test2.i";
if (!skip_test2) { write,"\nEscape factor timing test:";  test2, 15; }

if (!skip_test3) include, "test3.i";
if (!skip_test3) { write,"\nZone generator timing test:";  test3, 100; }
