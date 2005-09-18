/*
 * $Id: teststr.i,v 1.1 2005-09-18 22:06:18 dhmunro Exp $
 * complete test of yorick string manipulation functions
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

goofs = 0;  /* cumulative tally of errors detected */
write, "begin string manipulation test...";
s0 = string(0);
if (do_stats) "sA "+print(yorick_stats());

/* ------------------------------------------------------------------------- */
/* check DOCUMENT comment examples in i0/std.i */

if (anyof(strchar(["a","b","c"]) != ['a','\0','b','\0','c','\0']) ||
    anyof(strchar([['a','\0','b'],['c','\0','\0']])
          != ["a","b","c",string(0)])) {
  goofs++;
  "**FAILURE** (strchar) example";
}

if (strpart("Hello, world!", 4:6) != "lo," ||
    strpart("Hello, world!", [3,6]) != "lo," ||
    strpart("Hello, world!", [3,3]) != "" ||
    strpart("Hello, world!", [3,2]) ||
    strpart("Hello, world!", [3,20]) ||
    anyof(strpart("Hello, world!", [3,6,7,9]) != ["lo,","wo"]) ||
    anyof(strpart(["one","two"], [[1,2],[0,1]]) != ["n","t"]) ||
    anyof(strpart(["one","two"], [1,2,0,1]) != [["n","o"],["w","t"]])) {
  goofs++;
  "**FAILURE** (strpart) example";
}

if (anyof(strword("  Hello, world!") != [2,15]) ||
    anyof(strword("Hello, world!") != [0,13]) ||
    anyof(strword("Hello, world!", , 2) != [0,6,7,13]) ||
    anyof(strword("Hello, world!", , -2) != [0,6]) ||
    anyof(strword("Hello, world!", ".!, \t\n", -2) != [0,5]) ||
    anyof(strword("Hello, world!", [string(0), ".!, \t\n"], 0) != [0,12]) ||
    anyof(strword("Hello, world!", "A-Za-z", 2) != [5,7,12,13]) ||
    anyof(strword("Hello, world!", "^A-Za-z", 2) != [0,5,7,13]) ||
    anyof(strword("Hello, world!", "^A-Za-z", 3) != [0,5,7,12,13,-1]) ||
    anyof(strword("  Hello, world!", [" \t\n",".!, \t\n"]) != [2,7,9,15]) ||
    anyof(strword("  Hello, world!", [" \t\n",".!, \t\n"], 2)
          != [2,7,9,14,15,-1])) {
  goofs++;
  "**FAILURE** (strword) example";
}

if (anyof(strglob("*.pdb",["a","b.pdb","c.pdbx"]) != [0,1,0]) ||
    anyof(strglob("hack[0-9][0-9]", ["hack4a","hack63",""]) != [0,1,0])) {
  goofs++;
  "**FAILURE** (strglob) example";
}

s = ["one two three", "four five six"];
if (anyof(strfind("o",s) != [[0,1], [1,2]]) ||
    anyof(strfind(" t",s) != [[3,5], [13,-1]]) ||
    anyof(strfind(" t",s,n=2) != [[3,5,7,9], [13,-1,13,-1]]) ||
    anyof(strfind("e",s,n=2,back=1) != [[11,12,12,13], [0,-1,8,9]])) {
  goofs++;
  "**FAILURE** (strfind) example";
}

s = "Hello, world!" ;
if (anyof(strgrep("(Hello|Goodbye), *([a-zA-Z]+)!", s) != [0,13]) ||
    anyof(strgrep("(Hello|Goodbye), *([a-z]*|[A-Z]*)!", s, sub=[1,2])
          != [0,5,7,12]) ||
    anyof(strgrep("(Hello|Goodbye), *([a-z]*|[A-Z]*)!", s, sub=[0,2])
          != [0,13,7,12]) ||
    anyof(strgrep("(Hello|Goodbye), *(([A-Z]*)|([a-z]*))!", s, sub=[0,2,3,4])
          != [0,13,7,12,13,-1,7,12])) {
  goofs++;
  "**FAILURE** (strgrep) example";
}

if (streplace(s,[0,5], "Goodbye") != "Goodbye, world!" ||
    streplace(s,[0,5,7,7], ["Goodbye","cruel "]) != "Goodbye, cruel world!" ||
    streplace(s,[0,5,7,7,12,13], ["Goodbye","cruel ","?"]) !=
    "Goodbye, cruel world?" ||
    streplace(s,[0,5,0,-1,12,13], ["Goodbye","cruel ","?"]) !=
    "Goodbye, world?" ||
    anyof(streplace([s,s],[0,5], ["Goodbye", "Good bye"])
          != ["Goodbye, world!", "Good bye, world!"]) ||
    anyof(streplace([s,s],[0,5,7,7], [["Goodbye","cruel "], ["Good bye",""]])
          != ["Goodbye, cruel world!", "Good bye, world!"])) {
  goofs++;
  "**FAILURE** (streplace) example";
}

/* repeat testp.i tests */

if (strlen("abc")!=3 ||
    anyof(strlen([[string(),"","a"],["axx","ab","abcd"]])!=
          [[0,0,1],[3,2,4]])) {
  goofs++;
  "**FAILURE** (strlen)";
}

if (anyof(strtok("abc    1.23    xxx")!=["abc", "   1.23    xxx"]) ||
    anyof(strtok(["abc    1.23    xxx","c","1.5"], "\t c")!=
          [["ab", "    1.23    xxx"],string(),["1.5",string()]])) {
  goofs++;
  "**FAILURE** (strtok)";
}

if (!strmatch("abc", "b") || strmatch("abc", "B") ||
    !strmatch("abc", "B", 1) ||
    anyof(strmatch(["abc","aBC"], "B")!=[0,1])) {
  goofs++;
  "**FAILURE** (strmatch)";
}

if (strpart("abc", 1:1)!="a" || strpart("abc", 2:10)!="bc" ||
    strpart("abc", :-1)!="ab" || strpart("abc", :-5)!="" ||
    anyof(strpart(["abc","yowza"],3:)!=["c","wza"])) {
  goofs++;
  "**FAILURE** (strpart)";
}

s = [];
if (do_stats) "sB "+print(yorick_stats());
/* ------------------------------------------------------------------------- */

s = array(string, 2, 3, 4);
s(,1,) = "hi";
s(,2:3,1) = "";
x = strlen(s);
if (anyof(dimsof(x) != [3,2,3,4]) || anyof(x(,2:3,)) ||
    anyof(x(,1,) != 2)) {
  goofs++;
  "**FAILURE** (strlen) comprehensive";
}

s = strcase(1,s);
if (anyof(dimsof(s) != [3,2,3,4]) || anyof(s(,2:3,2:3)) ||
    anyof(s(,1,) != "HI") || anyof(s(,2:3,1) != "")) {
  goofs++;
  "**FAILURE** (strcase) test 1";
}
s = strcase(0,s);
if (anyof(dimsof(s) != [3,2,3,4]) || anyof(s(,2:3,2:3)) ||
    anyof(s(,1,) != "hi") || anyof(s(,2:3,1) != "")) {
  goofs++;
  "**FAILURE** (strcase) test 2";
}
strcase, 1, s;
if (anyof(s(,2:3,2:3)) || anyof(s(,1,) != "HI") || anyof(s(,2:3,1) != "")) {
  goofs++;
  "**FAILURE** (strcase) test 3";
}

x = ['a','\0','\0','b','c','\0','\0','d','e','f','\0'];
y = ["a",string(0),"bc",string(0),"def"];
if (anyof(strchar(["a","","bc",string(0),"def"]) != x) ||
    anyof(strchar(x) != y) || anyof(strchar(x(1:-1)) != y) ||
    anyof(strchar(grow(x(1:-1),x(1:-1))) !=
      ["a",string(0),"bc",string(0),"defa",string(0),"bc",string(0),"def"]) ||
    anyof(strchar([x(1:-1),x(1:-1)]) != grow(y,y)) ||
    anyof(strchar([x(1:-1),x(1:-1)](,,-:1:3)) != grow(y,y)(,-:1:3)(*))) {
  goofs++;
  "**FAILURE** (strchar) comprehensive";
}

x = y = array(0, 255);
x('A':'Z') = 1;
x(192:214) = 1;  x(216:222) = 1;  /* iso8859-1 upper case */
y(where(x)+32) = 1;
s = char(indgen(255));
x = char(s + 32*x);  /* upper to lower */
y = char(s - 32*y);  /* lower to upper */
s = strchar(s);
if (strcase(0,s)!=strchar(x) || strcase(1,s)!=strchar(y)) {
  goofs++;
  "**FAILURE** (strcase) test 4";
}

x = y = s = t = [];
if (do_stats) "sC "+print(yorick_stats());

s = "Hello, world!";
x = strpart(s, [[[0,5],[7,12]],[[7,12],[0,5]],[[0,13],[5,5]]]);
y = [["Hello","world"],["world","Hello"],["Hello, world!",""]];
if (anyof(dimsof(x)!=dimsof(y)) || anyof(x!=y) ||
    anyof(strpart(s, [[0,5,7,12],[7,12,0,5],[0,13,5,5]]) != y)) {
  goofs++;
  "**FAILURE** (strpart) test 1";
}
x = [[0,1,4,5],[3,5,5,-1]];
t = strpart(y,x);
s = [[["H","o"],["ld",s0]], [["w","d"],["lo",s0]], [["H","o"],[s0,s0]]];
if (anyof(dimsof(t)!=dimsof(s)) || anyof(t!=s) ||
    anyof(strpart(y(,1:2),x) != s(,,1:2)) ||
    anyof(strpart(y(,1:2),x(,-,)) !=
          [[["H","o"],["w","d"]],[["ld",s0],["lo",s0]]])) {
  goofs++;
  "**FAILURE** (strpart) test 2";
}
strpart, y, x(1:2,);
if (anyof(y != [["H","ld"],["w","lo"],["H",s0]])) {
  goofs++;
  "**FAILURE** (strpart) in place";
}

s = [["Now is the time"," for all good men "],
     ["to come to the", " aid of their party."]];
x = strpart(s, strword(s, 4,,-2));
y = strpart(s, strword(s, [4,16](-,),,-2));
t = strpart(s, strword(s, [0,10](-,),["^a-z"," "," ."],-1));
s = strpart(s, strword(s, [4,16](-,-,),,-2));
if (anyof(x != [["is","all"],["ome","of"]]) ||
    anyof(y != [["is","all"],[s0,"rty."]]) ||
    anyof(t != [[["ow","is"],["for","all"]],[["the",s0],["eir","party"]]]) ||
    anyof(s != [[["is","all"],["ome","of"]],[[s0,"n"],[s0,"rty."]]])) {
  goofs++;
  "**FAILURE** (strword) with offsets";
}

s = ["No trim.", "  lead", "trail  ", "  both.  "];
if (anyof(strtrim(s) != ["No trim.", "lead", "trail", "both."]) ||
    anyof(strtrim(s,1) != ["No trim.", "lead", "trail  ", "both.  "]) ||
    anyof(strtrim(s,2) != ["No trim.", "  lead", "trail", "  both."]) ||
    anyof(strtrim(s,3) != ["No trim.", "lead", "trail", "both."]) ||
    anyof(strtrim(s,1,blank="^ ") != [" trim.","  lead","  ","  both.  "]) ||
    anyof(strtrim(s,2,blank=" .") != ["No trim","  lead","trail","  both"]) ||
    anyof(strtrim(s,blank=" .") != ["No trim", "lead", "trail", "both"])) {
  goofs++;
  "**FAILURE** (strtrim) comprehensive";
}

/* strtok sufficiently tested? */

x = y = s = t = [];
if (do_stats) "sD "+print(yorick_stats());

s = ["one.c", "one.h", "one", "two.c", "two.h", "two"];
if (anyof(strglob("one.[ch]",s) != [1,1,0,0,0,0]) ||
    anyof(strglob("*.[ch]",s) != [1,1,0,1,1,0]) ||
    anyof(strglob("*o*.?",s) != [1,1,0,1,1,0]) ||
    anyof(strglob("*\\o*.?",s,esc=1) != [1,1,0,1,1,0]) ||
    anyof(strglob("*o*.\\?",s,esc=1) != [0,0,0,0,0,0]) ||
    anyof(strglob("One*",s,case=0) != [1,1,1,0,0,0]) ||
    anyof(strglob("One*",s,case=1) != [0,0,0,0,0,0]) ||
    anyof(strglob("One*",s) != [0,0,0,0,0,0]) ||
    !strglob("*.hit","/a/c.hit") || strglob(path=1,"*.hit","/a/c.hit") ||
    !strglob("*.hit",".c.hit",path=1) || strglob(path=2,"*.hit",".c.hit") ||
    !strglob(path=3,".*.hit",".c.hit")) {
  goofs++;
  "**FAILURE** (strglob) comprehensive";
}

s = ["UpCaSe cAsE", "dnCASE case"];
if (anyof(strfind("case",s) != [[11,-1],[7,11]]) ||
    anyof(strfind("case",s,back=1) != [[0,-1],[7,11]]) ||
    anyof(strfind("CaSe",s,back=1) != [[2,6],[0,-1]]) ||
    anyof(strfind("case",s,case=0) != [[2,6],[2,6]]) ||
    anyof(strfind("caSe",s,case=0,back=1) != [[7,11],[7,11]]) ||
    anyof(strfind("",s) != [[0,0],[0,0]]) ||
    anyof(strfind("",back=1,s) != [[11,11],[11,11]]) ||
    anyof(strfind("",s,case=0) != [[0,0],[0,0]]) ||
    anyof(strfind("",back=1,s,case=0) != [[11,11],[11,11]]) ||
    anyof(strfind(n=2,"",s) != [[0,0,0,0],[0,0,0,0]]) ||
    anyof(strfind(n=2,"",back=1,s) != [[11,11,11,11],[11,11,11,11]]) ||
    anyof(strfind(s0,s) != [[0,-1],[0,-1]]) ||
    anyof(strfind(s0,back=1,s) != [[0,-1],[0,-1]]) ||
    anyof(strfind(s0,s,case=0) != [[0,-1],[0,-1]]) ||
    anyof(strfind(s0,back=1,s,case=0) != [[0,-1],[0,-1]]) ||
    anyof(strfind(n=2,s0,s) != [[0,-1,0,-1],[0,-1,0,-1]]) ||
    anyof(strfind(n=2,s0,back=1,s) != [[0,-1,0,-1],[0,-1,0,-1]])) {
  goofs++;
  "**FAILURE** (strglob) endcases and case, back keywords";
}
if (anyof(strgrep("case",s) != [[11,-1],[7,11]]) ||
    anyof(strgrep("",s) != [[0,0],[0,0]]) ||
    anyof(strgrep(n=2,"",s) != [[0,0,0,0],[0,0,0,0]]) ||
    anyof(strgrep(s0,s) != [[0,-1],[0,-1]]) ||
    anyof(strgrep(n=2,s0,s) != [[0,-1,0,-1],[0,-1,0,-1]])) {
  goofs++;
  "**FAILURE** (strgrep) endcases";
}

s = ["ab","c","def"]+["0","123","45","6789"](-,);
x = "ab0c0def0ab123c123def123ab45c45def45ab6789c6789def6789";
y = ["ab0c0def0","ab123c123def123","ab45c45def45","ab6789c6789def6789"];
t = ["ab0ab123ab45ab6789","c0c123c45c6789","def0def123def45def6789"];
if (sum(s)!=x || anyof(s(sum,)!=y) || anyof(s(,sum)!=t)) {
  goofs++;
  "**FAILURE** sum index function for strings";
}

x = y = s = t = [];
if (do_stats) "sE "+print(yorick_stats());

s = array(sum(array("a",32)), 2,3,1,1,4);
x = [20,0,10] + [9,1,6,5](-,);
if (anyof(dimsof(strfind("a",s,x(-,,))) != [6,2,2,3,4,1,4]) ||
    anyof(strfind("a",s,x(-,,)) != [0,1]+x(-,-,,)) ||
    anyof(dimsof(strfind("a",s,x(-,-,,))) != [6,2,2,3,3,4,4]) ||
    anyof(strfind("a",s,x(-,-,,)) != [0,1]+x(-,-,-,,)) ||
    anyof(dimsof(strfind("a",s,x(-,-,-,,))) != [6,2,2,3,1,3,4]) ||
    anyof(strfind("a",s,x(-,-,-,,)) != [0,1]+x(-,-,-,-,,)) ||
    anyof(dimsof(strfind("a",s(,,1),x(-,,))) != [4,2,2,3,4]) ||
    anyof(strfind("a",s(,,1),x(-,,)) != [0,1]+x(-,-,,))) {
  goofs++;
  "**FAILURE** (strfind) offset conformability";
}

y = strfind("a",s,x(-,,));
s1 = strpart(s, [0,1]*x(-,-,,));
s2 = strpart(s, [1,0]*x(-,-,,)+[1,32]);
t = s;
streplace, t, y(,,,1:1,,), ["bc","def",s0](-,);
if (anyof(streplace(s,y,"b") != s1+"b"+s2) ||
    anyof(streplace(s,y,["bc","def",s0](-,)) != s1+["bc","def",s0](-,)+s2) ||
    anyof(t != s1(,,1:1,,)+["bc","def",s0](-,)+s2(,,1:1,,))) {
  goofs++;
  "**FAILURE** (streplace) comprehensive";
}
s1 = s2 = [];

/* ------------------------------------------------------------------------- */

x = y = s = t = [];
write, format= "end string manipulation test, %d goofs\n", goofs;
if (do_stats) "sZ "+print(yorick_stats());
s0 = [];
