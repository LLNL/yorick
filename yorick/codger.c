/*
 * $Id: codger.c,v 1.3 2007-03-29 23:56:19 dhmunro Exp $
 *
 * codger
 * automatic CODe GEneratoR for adding compiled functions to yorick
 *
 * compiled extensions to yorick come in two forms:
 * 1. archive libraries of object code, linked into yorick when
 *    yorick itself is loaded
 * 2. dynamic libraries of object code, linked into yorick at runtime
 *    by means of the interpreted plug_in function (which invokes the
 *    system dynamic loader)
 *
 * the compiled code for a yorick extension is the same in either case
 * (although the object modules must be built with different compiler
 * flags in order to become part of a dynamic library on some platforms)
 *
 * Every yorick extension library (whether a static archive or a dynamic
 * library) must contain a ywrap.o module, which must define a single
 * function (with y_pkg_t prototype), called yk_pkgname where "pkgname"
 * is the name of the package.  Yorick calls this function to return
 * the names and addresses of the built-in functions and data defined by
 * the package.  The static archive would be libpkgname.a, and the
 * dynamic library would be pkgname.so or pkgname.sl or pkgname.dll
 * depending on the platform.
 *
 * yk_pkgname returns pointers to the following objects:
 * 1. the list of .i file names which must be included in order to
 *    load the package, typically installed in the Y_HOME/i0/ directory
 *    - yorick includes these at startup in the static load case,
 *      and at plug_in in the dynamic load case
 * 2. the list of pointers to the builtin functions defined by pkgname
 *      (declared by extern statements in the .i files)
 * 3. the list of pointers to any compiled data defined by pkgname
 *      (declared by extern statments followed by reshape statements
 *       or EXTERN comments in the .i files)
 * 4. the list of names by which the interpreter can reference (2) and (3)
 *
 * ywrap.o optionally contains automatically generated wrappers for
 * the builtin list (2), which reference C functions defined in other
 * modules of the package (in the case that the other modules do not
 * directly provide builtin functions that communicate with the yorick
 * interpreter for their arguments and results)
 * - these wrappers are specified by PROTOTYPE comments in the .i files
 *
 * codger generates the ywrap.c source code, given the .i files for
 * the package
 *
 * Additionally, codger generates the yinit.c source code, containing the
 * on_launch callback that passes the function addresses of yk_yor and
 * any statically loaded yk_pkgname functions to yorick at startup,
 * plus the Y_HOME and Y_SITE directory names.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int usage(void);
static int geninit(int argc, char *argv[]);
static int genwrap(int argc, char *argv[]);
static FILE *out_open(char *name);
static FILE *i_open(char *name, char **dirs, int ndirs);
static int i_process(FILE *out, FILE *in, int flink, char *inname);
static char *deescape(char *path);

#define E_(msg) fputs(msg "\n", stderr)
#define O_(msg) fputs(msg "\n", out)

static int
usage(void)
{
  E_("codger usage:");
  E_("  codger w pkgname pkga.i [pkgb.i ...]");
  E_("    generate ywrap.c for pkgname from pkga.i, pkgb.i, ...");
  E_("    options (after pkgname):");
  E_("      -o alt.c   (before any .i) output to alt.c instead of ywrap.c");
  E_("      -Idir      add dir to search path for .i files");
  E_("      -Ldir -lname   record dependencies required to link package");
  E_("      -f_ -f -F_ -F    select fortran linkage convention (default f_)");
  E_("  codger i Y_HOME Y_SITE pkgname1 [pkgname2 ...]");
  E_("    generate yinit.c for pkgname1, pkgname2, ...");
  E_("    options (after Y_SITE):");
  E_("      -o alt.c   output to alt.c instead of yinit.c");
  return 1;
}

int
main(int argc, char *argv[])
{
  if (argc>=4 && !argv[1][1]) {
    if (argv[1][0]=='i')
      return geninit(argc-2, argv+2);
    else if (argv[1][0]=='w')
      return genwrap(argc-2, argv+2);
  }
  return usage();
}

static int
geninit(int argc, char *argv[])
{
  FILE *out;
  char *outname = 0;
  char *home = argv[0];
  char *site = argv[1];
  char **pkgs = malloc(sizeof(char *) * argc);
  int npkgs = 0;
  long i, n;

  for (argc-=2,argv+=2 ; argc ; argc--,argv++) {
    if (argv[0][0] == '-') {
      if (argv[0][1]=='o' && !argv[0][2]) {
        argc--,argv++;
        if (!argc || outname)
          return usage();
        outname = argv[0];
      } else {
        return usage();
      }
    } else {
      n = strlen(argv[0]);
      if (n > 100) {
        E_("codger: pkgname has over 100 chars");
        return 2;
      }
      for (i=0 ; i<n ; i++) {
        if ((argv[0][i]>='a' && argv[0][i]<='z') ||
            (argv[0][i]>='A' && argv[0][i]<='Z') ||
            (argv[0][i]>='0' && argv[0][i]<='9') ||
            argv[0][i]=='_') continue;
        E_("codger: pkgname has illegal character, not [a-z][A-Z][0-9]_");
        return 2;
      }
      if (strcmp(argv[0], "yor")) {  /* skip repeats and yor */
        for (i=0 ; i<npkgs ; i++) if (!strcmp(argv[0], pkgs[i])) break;
        if (i>=npkgs) 
          pkgs[npkgs++] = argv[0];
      }
    }
  }

  out = fopen(outname? outname : "yinit.c", "w");
  if (!out) {
    fprintf(stderr, "codger: unable to create %s\n",
            outname? outname : "yinit.c");
    return 3;
  }

  O_("/* codger-generated yorick initialization file */");
  O_("#include \"play.h\"");
  O_("#include \"ydata.h\"");
  O_("");
  fprintf(out, "static char *yhome = \"%s\";\n", deescape(home));
  fprintf(out, "static char *ysite = \"%s\";\n", deescape(site));
  O_("");
  O_("extern y_pkg_t yk_yor;");
  for (i=0 ; i<npkgs ; i++)
    fprintf(out, "extern y_pkg_t yk_%s;\n", pkgs[i]);
  O_("");
  O_("static y_pkg_t *ypkgs[] = {");
  O_("  &yk_yor,");
  for (i=0 ; i<npkgs ; i++)
    fprintf(out, "  &yk_%s,\n", pkgs[i]);
  O_("  0");
  O_("};");
  O_("");
  O_("int");
  O_("on_launch(int argc, char *argv[])");
  O_("{");
  O_("  return y_launch(argc, argv, yhome, ysite, ypkgs);");
  O_("}");

  fclose(out);
  return 0;
}

static char *
deescape(char *path)
{
  /* try to cope with escaped blanks in Y_HOME, Y_SITE */
  char *p, *q = path;
  for (p=q ; q[0] ; p++,q++) {
    if (q[0]=='\\' && q[1]==' ') p[0]=' ', q++;
    else if (p!=q) p[0] = q[0];
  }
  if (p!=q) p[0] = '\0';
  return path;
}

#if defined(f_linkage)
# define F_LINK 1
#elif defined(F_LINKAGE_)
# define F_LINK 2
#elif defined(F_LINKAGE)
# define F_LINK 3
#else
# define F_LINK 0
#endif

/* ------------------------------------------------------------------------ */
/* red-black tree is simplest balanced binary tree
 * - use it here to store list of extern variable names
 * - idea is that this is minimal code to support
 *   efficient name collision detection
 */

typedef struct rbtree rbtree;
extern void rbinit(rbtree *node, rbtree *up);
extern rbtree *rblookup(rbtree *root, void *data, long ldata);
extern rbtree *rbinsert(rbtree **proot, void *data, long ldata);
/* rbremove not necessary here */

/* --------- application-specific routines and struct */

extern rbtree *rballoc(rbtree *up, void *data, long ldata);
extern int rbcmp(rbtree *node, void *data, long ldata);

rbtree *var_table = 0;
rbtree *var_first = 0;
rbtree *var_last = 0;

struct rbtree {
  /* generic rbtree links */
  rbtree *left, *right, *up;
  int red;

  /* application-specific data */
  rbtree *next;
  char *cname;
  int type;
  char name[4];
};

#define VAR_UNKNOWN 0
#define VAR_CODE    1
#define VAR_DATA    2
#define VAR_FORT    4
#define VAR_RESHAPE 8

rbtree *
rballoc(rbtree *up, void *data, long ldata)
{
  char *name = data;
  rbtree *node = malloc(sizeof(rbtree)+ldata);
  if (node) {
    rbinit(node, up);
    if (!var_first) var_first = node;
    else            var_last->next = node;
    var_last = node;
    node->next = 0;
    node->cname = 0;
    node->type = 0;
    strncpy(node->name, name, ldata);
    node->name[ldata] = '\0';
  }
  return node;
}

int
rbcmp(rbtree *node, void *data, long ldata)
{
  int cmp = strncmp(node->name, data, ldata);
  if (!cmp && node->name[ldata]) cmp = 1;
  return -cmp;
}

/* --------- generic rbtree routines */

void
rbinit(rbtree *node, rbtree *up)
{
  node->left = node->right = 0;
  node->up = up;
  node->red = (up != 0);
}

rbtree *
rblookup(rbtree *root, void *data, long ldata)
{
  int cmp;
  while (root) {
    cmp = rbcmp(root, data, ldata);
    if (!cmp) return root;
    root = (cmp<0)? root->left : root->right;
  }
  return 0;
}

rbtree *
rbinsert(rbtree **proot, void *data, long ldata)
{
  int lchild, lparent;
  rbtree *node, *child, *gramps, *uncle, *parent = *proot;

  if (!parent)
    return *proot = rballoc(0, data, ldata);

  for (;;) {
    lchild = rbcmp(parent, data, ldata);
    if (!lchild) return 0;   /* cannot insert duplicate data */
    lchild = (lchild < 0);
    if (lchild) {
      if (!parent->left) {
        parent->left = node = rballoc(parent, data, ldata);
        break;
      }
      parent = parent->left;
    } else {
      if (!parent->right) {
        parent->right = node = rballoc(parent, data, ldata);
        break;
      }
      parent = parent->right;
    }
  }
  if (!node) return 0;

  child = node;
  while (parent->red) {    /* note this implies parent->up != 0 */
    gramps = parent->up;
    lparent = gramps->left == parent;
    uncle = lparent? gramps->right : gramps->left;

    if (uncle && uncle->red) {
      /* red uncle: recolor parent, uncle, gramps and propagate upward */
      parent->red = uncle->red = 0;
      parent = gramps->up;
      if (!parent) break;       /* quit if gramps is root */
      gramps->red = 1;          /* was black */
      child = gramps;           /* child always has two black children */
      lchild = (parent->left == child);
      continue;
    }

    /* black uncle: balance tree (4 cases) and quit */
    if (lchild == lparent) {
      if (lparent) {
        /*     ( (c[R] < p[R] < x[B]) < g[B] < u[B] )
         * -->   (c[R] < p[B] < (x[B] < g[R] < u[B]) )
         */
        gramps->left = parent->right;  parent->right = gramps;
        if (gramps->left) gramps->left->up = gramps;
      } else {
        /*      ( u[B] < g[B] < (x[B] < p[R] < c[R]) )
         * --> ( (u[B] < g[R] < x[B]) < p[B] < c[R])
         */
        gramps->right = parent->left;  parent->left = gramps;
        if (gramps->right) gramps->right->up = gramps;
      }
      parent->up = gramps->up;  gramps->up = parent;
      if (!parent->up) *proot = parent;
      else if (parent->up->left == gramps) parent->up->left = parent;
      else parent->up->right = parent;
      parent->red = 0;
    } else {
      if (lparent) {
        /*     ( (x[B] < p[R] < (clB < c[R] < crB)) < g[B] < u[B] )
         * --> ( (x[B] < p[R] < clB) < c[B] < (crB < g[R] < u[B]) )
         */
        parent->right = child->left;  child->left = parent;
        gramps->left = child->right;  child->right = gramps;
        if (parent->right) parent->right->up = parent;
        if (gramps->left) gramps->left->up = gramps;
      } else {
        /*     ( u[B] < g[B] < ((clB < c[R] < crB) < p[R] < x[B]) )
         * --> ( (u[B] < g[R] < clB) < c[B] < (crB < p[R] < x[B]) )
         */
        parent->left = child->right;  child->right = parent;
        gramps->right = child->left;  child->left = gramps;
        if (parent->left) parent->left->up = parent;
        if (gramps->right) gramps->right->up = gramps;
      }
      child->up = gramps->up;  gramps->up = parent->up = child;
      if (!child->up) *proot = child;
      else if (child->up->left == gramps) child->up->left = child;
      else child->up->right = child;
      child->red = 0;
    }
    gramps->red = 1;
    break;
  }

  return node;
}

/* end of red-black tree code */
/* ------------------------------------------------------------------------ */

static int
genwrap(int argc, char *argv[])
{
  FILE *out=0, *in=0;
  char *outname = 0;
  char *pkgname = argv[0];
  char **dirs = malloc(sizeof(char *) * argc);
  int ndirs = 0;
  char **libs = malloc(sizeof(char *) * argc);
  int nlibs = 0;
  char **ins = malloc(sizeof(char *) * argc);
  int nins = 0;
  int flink = F_LINK;  /* 1 supress trailing _, 2 upper case */
  int i;
  rbtree *node;

  for (argc--,argv++ ; argc ; argc--,argv++) {
    if (argv[0][0] == '-') {
      if (argv[0][1]=='o' && !argv[0][2]) {
        argc--,argv++;
        if (!argc || outname || out)
          return usage();
        outname = argv[0];
      } else if (argv[0][1]=='I' && argv[0][2]) {
        dirs[ndirs++] = argv[0]+2;
      } else if ((argv[0][1]=='L' || argv[0][1]=='l') && argv[0][2]) {
        libs[nlibs++] = argv[0];
      } else if (argv[0][1]=='f' || argv[0][1]=='F') {
        if (argv[0][2]=='_' && !argv[0][3]) flink = 0;
        else if (!argv[0][2])               flink = 1;
        else                                return usage();
        flink |= (argv[0][1]=='F')? 2 : 0;
      } else {
        return usage();
      }
    } else {
      if (!out) {
        out = out_open(outname? outname : "ywrap.c");
        if (!out) {
          E_("unable to create output file");
          return 2;
        }
      }
      ins[nins++] = argv[0];
      O_("");
      fprintf(out, "/*----------------begin %s */\n", argv[0]);
      in = i_open(argv[0], dirs, ndirs);
      if (!in) {
        fclose(out);
        return 3;
      }
      /* i_process emits any wrapper code, adds to routines, values, names */
      if (i_process(out, in, flink, argv[0])) {
        fclose(out);
        return 4;
      }
    }
  }

  O_("");
  O_("/*----------------list include files */");
  O_("");
  O_("static char *y0_includes[] = {");
  for (i=0 ; i<nins ; i++)
    fprintf(out, "  \"%s\",\n", ins[i]);
  O_("  0,");
  if (nlibs) {
    O_("  /*--------------dependent libraries this package requires */");
    fprintf(out, "  \"%s", libs[0]);
    for (i=1 ; i<nlibs ; i++)
      fprintf(out, " %s", libs[i]);
    O_("\",");
  }
  O_("  0");
  O_("};");

  O_("");
  O_("/*----------------collect pointers and names */");
  O_("");
  O_("static BuiltIn *y0_routines[] = {");
  for (node=var_first ; node ; node=node->next)
    if (!(node->type&VAR_DATA)) fprintf(out, "  &Y_%s,\n", node->name);
  O_("  0");
  O_("};");
  O_("");
  O_("static void *y0_values[] = {");
  for (node=var_first ; node ; node=node->next)
    if (node->type&VAR_DATA)
      fprintf(out, "  %s,\n", node->cname?  node->cname : node->name);
  O_("  0");
  O_("};");
  O_("");
  O_("static char *y0_names[] = {");
  for (node=var_first ; node ; node=node->next)
    if (!(node->type&VAR_DATA)) fprintf(out, "  \"%s\",\n", node->name);
  for (node=var_first ; node ; node=node->next)
    if (node->type&VAR_DATA) fprintf(out, "  \"%s\",\n", node->name);
  O_("  0");
  O_("};");

  O_("");
  O_("/*----------------define package initialization function */");
  O_("");
  fprintf(out, "PLUG_EXPORT char *yk_%s(char ***,\n", pkgname);
  O_("                         BuiltIn ***, void ***, char ***);");
  fprintf(out, "static char *y0_pkgname = \"%s\";\n", pkgname);
  O_("");
  O_("char *");
  fprintf(out, "yk_%s(char ***ifiles,\n", pkgname);
  O_("       BuiltIn ***code, void ***data, char ***varname)");
  O_("{");
  O_("  *ifiles = y0_includes;");
  O_("  *code = y0_routines;");
  O_("  *data = y0_values;");
  O_("  *varname = y0_names;");
  O_("  return y0_pkgname;");
  O_("}");

  fclose(out);
  return 0;
}

static FILE *
out_open(char *name)
{
  FILE *out = fopen(name, "w");
  if (!out) {
    fprintf(stderr, "codger: unable to create %s\n", name);
    return 0;
  }

  O_("/* codger-generated yorick package wrapper file */");
  O_("#include \"play.h\"");
  O_("#include \"ydata.h\"");
  return out;
}

static FILE *
i_open(char *name, char **dirs, int ndirs)
{
  /* first try curent directory, then directories in -I options */
  FILE *in = fopen(name, "r");
  if (!in) {
    char tmp[2048];
    long i, n, len = strlen(name);
    for (i=ndirs-1 ; i>=0 && !in ; i--) {  /* scan -I in reverse order */
      n = strlen(dirs[i]);
      if (n+len > 2046) {
        E_("codger: pkg.i name plus -Idir option >2046 characters");
        return 0;
      }
      strcpy(tmp, dirs[i]);
      if (n && tmp[n-1]!='/') strcpy(tmp+n, "/");
      strcat(tmp+n, name);
      in = fopen(tmp, "r");
    }
    if (!in)
      fprintf(stderr, "codger: unable to find %s, need -I?", name);
    else
      fprintf(stdout, "found %s in %s\n", name, dirs[i+1]);
  } else {
    fprintf(stdout, "found %s in current directory\n", name);
  }
  return in;
}

static char *next_line(FILE *in);
static char *skip_white(char *pos);
static char *next_nonblank(FILE *in, int hash);
static char *skip_single(char *pos);
static char *skip_quote(FILE *in, char *pos);
static char *skip_comment(FILE *in, char *pos);
static char *skip_hash(FILE *in, char *pos);
static char *skip_brace(FILE *in, char *pos);
static char *next_action(FILE *in, char *pos);
static char *next_token(FILE *in, char *pos, int incomment);
static int is_fortran(FILE *in, char **ppos);
static int name_len(char *pos);
static void handle_extern(FILE *out, char *pos, int len);
static void handle_reshape(FILE *out, char *pos, int len);
static void handle_extcomm(FILE *out, char *pos, int len, int fort);
static void handle_procomm(FILE *out, FILE *in, char *pos, int fort);
static void put_extern(FILE *out, rbtree *var, int check);
static void put_cname(rbtree *var, char *pos, int len);
static int get_ctype(FILE *in, char **ppos);
static void i_warning(char *msg);

#define IS_WHITE(c) (c==' ' || c=='\t' || c=='\n' || c=='\r')
#define IS_EWHITE(c) ((!c) || c==' ' || c=='\t' || c=='\n' || c=='\r')
#define IS_ALPH(c) ((c>='A' && c<='Z') || (c>='a' && c<='z') || c=='_')
#define IS_ALPHNUM(c) (IS_ALPH(c) || (c>='0' && c<='9'))

static long line_num;
static char *file_name;
static int fortran_policy;
static int n_errs;

static int
i_process(FILE *out, FILE *in, int flink, char *inname)
{
  char *pos;
  int fort, lname;

  file_name = inname;
  line_num = 0;
  fortran_policy = flink;
  n_errs = 0;

  for (pos=next_action(in,"") ; pos ; pos=next_action(in,pos)) {
    if (pos[0] == 'e') {         /* extern statement */
      pos = next_token(in, pos+6, 0);
      if (!pos) break;
      lname = name_len(pos);
      if (!lname)
        i_warning("bad extern statement");
      else
        /* this becomes current variable for PROTOTYPE, EXTERNAL comments */
        handle_extern(out, pos, lname);

    } else if (pos[0] == 'P') {  /* PROTOTYPE comment */
      fort = is_fortran(in, &pos);
      /* can go ahead and generate wrapper immediately,
       * also mark current variable definitely a builtin
       */
      handle_procomm(out, in, pos, fort);
      pos = skip_comment(in, pos);

    } else if (pos[0] == 'r') {  /* reshape statement */
      pos = next_token(in, pos+7, 0);
      if (!pos) break;
      if (pos[0] == ',') {
        pos = next_token(in, pos+1, 0);
        if (!pos) break;
      }
      lname = name_len(pos);
      if (!lname)
        i_warning("bad reshape statement");
      else
        /* mark variable definitely data if not already marked */
        handle_reshape(out, pos, lname);

    } else if (pos[0] == 'E') {  /* EXTERNAL comment */
      fort = is_fortran(in, &pos);
      lname = name_len(pos);
      if (!lname)
        i_warning("bad EXTERNAL comment");
      else
        /* mark current variable definitely data, with compiled alias */
        handle_extcomm(out, pos, lname, fort);
      pos = skip_comment(in, pos);
    }
  }
  fclose(in);

  put_extern(out, var_last, 1);

  /* make sure output file is uncompilable if errors detected */
  if (n_errs) {
    fprintf(out, "\n#error %d codger errors in %s*/\n\n", n_errs, inname);
    return 1;
  }
  return 0;
}

static void
i_warning(char *msg)
{
  n_errs++;
  fprintf(stderr, "codger: %s, LINE: %ld FILE: %s\n",
          msg, line_num, file_name);
}

static int
is_fortran(FILE *in, char **ppos)
{
  char *pos = *ppos;
  int fort = 0;
  pos += 8;                       /* skips EXTERNAL */
  if (!IS_EWHITE(pos[0])) pos++;  /* skips PROTOTYPE */
  pos = next_token(in, pos, 1);
  if (pos[0]=='F' && !strncmp(pos+1,"ORTRAN",6) && IS_EWHITE(pos[7])) {
    fort = 1;
    pos = next_token(in, pos+7, 1);
  }
  *ppos = pos;
  return fort;
}

static int
name_len(char *pos)
{
  int len = 0;
  if (pos && IS_ALPH(pos[0]))
    for (len++ ; IS_ALPHNUM(pos[len]) ; len++);
  return len;
}

static char *
next_line(FILE *in)
{
  static char line[4096];
  if (feof(in)) return 0;
  line_num++;
  return fgets(line, 4096, in);
}

static char *
skip_white(char *pos)
{
  if (pos)
    while (pos[0] && IS_WHITE(pos[0])) pos++;
  return pos;
}

static char *
next_nonblank(FILE *in, int hash)
{
  char *pos;
  do {
    pos = next_line(in);
    if (!pos) return 0;
    pos = skip_white(pos);
    if (hash && pos[0]=='#') {
      pos = skip_hash(in, pos);
      if (!pos) return 0;
    }
  } while (!pos[0]);
  return pos;
}

static char *
skip_single(char *pos)
{
  /* pos[0] == ' on input, scan to closing ' */
  pos++;
  if (pos[0]=='\\' && (++pos)[0]) ++pos;
  while (pos[0] && pos[0]!='\'') pos++;
  return pos[0]? pos+1 : pos;
}

static char *
skip_quote(FILE *in, char *pos)
{
  /* pos[0] == " on input, scan to closing " */
  pos++;
  while (pos[0]!='"') {
    if (!pos[0]) {
      pos = next_line(in);
      if (!pos) return 0;
    } else if (pos[0]=='\\') {
      pos++;
      if (pos[0]<='7' && pos[0]>='0') {
        pos++;
        if (pos[0]<='7' && pos[0]>='0') {
          pos++;
          if (pos[0]<='7' && pos[0]>='0') pos++;
        }
      } else if (pos[0] == 'x') {
        pos++;
        while ((pos[0]<='9' && pos[0]>='0')
               || (pos[0]<='F' && pos[0]>='A')
               || (pos[0]<='f' && pos[0]>='a')) pos++;
      } else {
        pos++;
      }
    } else {
      pos++;
    }
  }
  return pos+1;
}

static char *
skip_comment(FILE *in, char *pos)
{
  /* scan to closing star slash */
  do {
    while (pos[0])
      if ((pos++)[0]=='*' && pos[0]=='/') return pos+1;
    pos = next_line(in);
  } while (pos);
  return 0;
}

static char *
skip_hash(FILE *in, char *pos)
{
  /* yorick bug:
   * #if  at beginning of line inside slash-star comment
   * is correctly commented out,
   * but   #endif cannot be similarly commented out if inside an #if
   * this code repeats that bug
   */
  /* pos[0] == # on input, scan to next valid line */
  int depth = 0;
  for(;;) {
    pos = skip_white(pos+1);
    if (depth && !strncmp(pos,"endif",5) && IS_EWHITE(pos[5])) {
      depth--;
    } else if (pos[0]=='i' && pos[1]=='f' && (pos[2]==' ' || pos[2]=='\t')) {
      pos = skip_white(pos+3);
      if (depth || (pos[0]=='0' && IS_EWHITE(pos[1])))
        depth++;
    }
    for (;;) {
      pos = next_line(in);
      if (!pos) return 0;
      pos = skip_white(pos);
      if (pos[0]=='#') break;
      if (depth || !pos[0]) continue;
      return pos;  /* non-blank, non-# */
    }
  }
}

static char *
skip_brace(FILE *in, char *pos)
{
  /* pos[0] == { on input, scan to matching } */
  int depth = 1;
  pos++;
  do {
    pos = skip_white(pos);
    if (!pos[0]) {
      pos = next_nonblank(in, 1);
      if (!pos) return 0;
    }
    if (pos[0]=='}') {
      pos++;
      depth--;
    } else if (pos[0]=='{') {
      pos++;
      depth++;
    } else if (pos[0]=='/') {
      if (pos[1]=='/') {
        pos = next_nonblank(in, 1);
      } else if (pos[1]=='*') {
        pos = skip_comment(in, pos+2);
      } else {
        pos++;
      }
    } else if (pos[0]=='"') {
      pos = skip_quote(in, pos);
      if (!pos) return 0;
    } else if (pos[0]=='\'') {
      pos = skip_single(pos);
    } else {
      pos++;
    }
  } while (pos && depth);
  return pos;
}

static char *
next_action(FILE *in, char *pos)
{
  /* scan to next extern, reshape, PROTOTYPE, or EXTERNAL */
  while (pos) {
    while (pos[0]) {
      /* clear out remainder of line */
      if (pos[0]=='"') {
        pos = skip_quote(in, pos);
      } else if (pos[0]=='/') {
        if (pos[1]=='*') pos = skip_comment(in, pos+2);
        else if (pos[1]=='/') for (pos+=2 ; pos[0] ; pos++);
        else pos++;
      } else if (pos[0]=='{') {
        pos = skip_brace(in, pos);
      } else {
        pos++;
      }
      if (!pos) return 0;
    }
    if (!pos) return 0;
    pos = next_nonblank(in, 1);
    if (!pos) return 0;
    /* check if beginning of line is an action for codger */
    if (pos[0]=='e') {
      if (!strncmp(pos+1,"xtern",5) && !IS_ALPHNUM(pos[6]))
        return pos;           /* extern */
    } else if (pos[0]=='r') {
      if (!strncmp(pos+1,"eshape",6) && !IS_ALPHNUM(pos[7]))
        return pos;           /* reshape */
    } else if (pos[0]=='/' && pos[1]=='*') {
      pos = next_token(in, pos+2, 1);
      if (pos[0]=='P') {
        if (!strncmp(pos+1,"ROTOTYPE",8) && IS_EWHITE(pos[9]))
          return pos;       /* PROTOTYPE comment */
      } else if (pos[0]=='E') {
        if (!strncmp(pos+1,"XTERNAL",7) && IS_EWHITE(pos[8]))
          return pos;       /* EXTERNAL comment */
      }
      pos = skip_comment(in, pos);
    }
  }
  return pos;
}

static char *
next_token(FILE *in, char *pos, int incomment)
{
  if (!pos) return 0;
  for (;;) {
    pos = skip_white(pos);
    while (!pos[0]) {
      pos = next_line(in);
      if (!pos) return 0;
      pos = skip_white(pos);
    }
    if (incomment || pos[0]!='/') break;
    if (pos[1]=='*') pos = skip_comment(in, pos+2);
    else if (pos[1]=='/') for (pos+=2 ; pos[0] ; pos++);
    else break;
  }
  return pos;
}

static void
handle_extern(FILE *out, char *pos, int len)
{
  rbtree *prev = var_last;
  rbtree *v = rbinsert(&var_table, pos, len);
  if (!v) {
    /* multiple externs for same var ugly but legal? */
    if (!rblookup(var_table, pos, len))
      i_warning("MEMORY MANAGER FAILED");

  } else {
    put_extern(out, prev, 1);
  }
}

static void
handle_reshape(FILE *out, char *pos, int len)
{
  if (var_last && rblookup(var_table, pos, len)==var_last) {
    if (var_last->type & VAR_CODE) {
      i_warning("cannot reshape builtin function");
    } else if (var_last->type & VAR_DATA) {
      var_last->type |= VAR_RESHAPE;
    } else {
      var_last->type = VAR_DATA | VAR_RESHAPE;
      /* generate extern declaration immediately */
      put_extern(out, var_last, 0);
    }
  }
}

static void
handle_extcomm(FILE *out, char *pos, int len, int fort)
{
  if (var_last) {
    if (var_last->type & VAR_CODE) {
      i_warning("cannot EXTERNAL builtin function");
    } else if (var_last->type & VAR_RESHAPE) {
      i_warning("EXTERNAL must come before reshape");
    } else if (var_last->type & VAR_DATA) {
      i_warning("duplicate EXTERNAL comment illegal");
    } else {
      var_last->type = VAR_DATA | (fort? VAR_FORT : 0);
      put_cname(var_last, pos, len);
      /* generate extern declaration immediately */
      put_extern(out, var_last, 0);
    }
  }
}

static void
put_cname(rbtree *var, char *pos, int len)
{
  int fort = (var->type & VAR_FORT) != 0;
  /* bit 1 of fortran_policy supresses trailing _ */
  int underscore = (fort && !(fortran_policy & 1));
  var->cname = malloc(len+underscore+1);
  if (var->cname) {
    strncpy(var->cname, pos, len);
    if (underscore) var->cname[len++] = '_';
    var->cname[len] = '\0';
    if (fort) {
      int i;
      /* bit 2 of fortran_policy means upper case, else lower case */
      if (fortran_policy & 2) {
        for (i=0 ; i<len ; i++)
          if (var->cname[i]>='a' && var->cname[i]<='z')
            var->cname[i] ^= ('a' ^ 'A');
      } else {
        for (i=0 ; i<len ; i++)
          if (var->cname[i]>='A' && var->cname[i]<='Z')
            var->cname[i] ^= ('a' ^ 'A');
      }
    }
  } else {
    i_warning("MEMORY MANAGER FAILED");
  }
}

static void
put_extern(FILE *out, rbtree *var, int check)
{
  if (var && (!check || !var->type)) {
    if (var->type & VAR_DATA) {
      fprintf(out, "extern char %s[4];\n", var->cname?var->cname:var->name);
    } else {
      fprintf(out, "extern BuiltIn Y_%s;\n", var->name);
      if (!var->type) var->type = VAR_CODE;
    }
  }
}

/* in alphabetical order */
static char *proto_types[11] = {
  "char", "complex", "double", "float", "int", "long",
  "pointer", "short", "string", "void", 0 };
static char *c_types[11] = {
  "char ", "double ", "double ", "float ", "int ", "long ",
  "void *", "short ", "char *", "void ", 0 };
/* bit 1 return not ok, 2 arg not okay, 4 complex */
static int c_modes[11] = { 0, 5, 0, 0, 0, 0, 1, 0, 1, 2, -1 };
static char *push_result[11] = {
  "PushIntValue((int)", 0, "PushDoubleValue(", "PushDoubleValue((double)",
  "PushIntValue(", "PushLongValue(", 0, "PushIntValue((short)", 0, "", 0 };
static char *push_finish[11] = {
  ")", 0, ")", ")", ")", ")", 0, ")", 0, "", 0 };
static char *yarg_char[11] = {
  "c", "z", "d", "f", "i", "l", "p", "s", "q", 0, 0 };

static int
get_ctype(FILE *in, char **ppos)
{
  char *pos = *ppos;
  int lname = name_len(pos);
  int i = -1;
  if (lname > 1) {
    for (i=0 ; ; i++) {
      if (!proto_types[i] || pos[0]<proto_types[i][0]) return -1;
      if (pos[0] > proto_types[i][0]) continue;
      if (!strncmp(proto_types[i]+1, pos+1, lname-1) &&
          !proto_types[i][lname]) break;
    }
    pos += lname;
    *ppos = pos;
  }
  return i;
}

static void
handle_procomm(FILE *out, FILE *in, char *pos, int fort)
{
  char proto[256], delim;
  int lname, ret, arg, nargs, len, i, star;

  if (!var_last) return;
  if (var_last->type & VAR_DATA) {
    i_warning("cannot PROTOTYPE data variable");
    return;
  }
  var_last->type = VAR_CODE | (fort? VAR_FORT : 0);
  /* generate extern declaration for Y_ wrapper immediately */
  put_extern(out, var_last, 0);

  /* remains to generate
   * (1) prototype for compiled function
   * (2) Y_ wrapper function definition
   */
  ret = get_ctype(in, &pos);
  if (ret<0 || (c_modes[ret]&1)) {
    i_warning("bad PROTOTYPE return type");
    return;
  }
  pos = next_token(in, pos, 1);
  lname = name_len(pos);
  if (!lname) {
    i_warning("bad PROTOTYPE function name");
    return;
  }
  put_cname(var_last, pos, lname);
  pos = next_token(in, pos+lname, 1);
  for (nargs=0,delim = '(' ; ; nargs++) {
    /* skip delimiter */
    if (pos[0]!=delim && (nargs!=1 || pos[0]!=(delim=','))) {
      if (nargs && pos[0]==')') break;        /* only legal way out */
      i_warning("bad PROTOTYPE syntax");
      return;
    } else if (nargs >= 256) {
      i_warning("PROTOTYPE has >256 function parameters");
      return;
    }
    pos = next_token(in, pos+1, 1);           /* seek argument type */
    arg = get_ctype(in, &pos);
    if (arg<0 || (nargs && (c_modes[arg]&2))) {
      i_warning("bad PROTOTYPE argument type");
      return;
    }
    pos = next_token(in, pos, 1);
    if ((c_modes[arg]&2) && pos[0]!=')') {
      i_warning("bad PROTOTYPE void argument");
      return;
    }
    lname = name_len(pos);
    if ((lname==5 && !strncmp(pos, "array", 5)) ||
        (!lname && pos[0]=='*')) { /* array qualifier */
      arg |= 64;
      pos = next_token(in, pos+(lname?lname:1), 1);
      lname = name_len(pos);
    }
    proto[nargs] = arg;
    if (lname)                       /* skip optional argument name */
      pos = next_token(in, pos+lname, 1);
  }

  /* first, generate compiled prototype (used in wrapper) */
  fprintf(out, "\nextern %s%s(", c_types[ret], var_last->cname);
  len = 8+strlen(c_types[ret])+strlen(var_last->cname);
  for (i=0 ; i<nargs ; i++) {
    if (len > 60) {
      fprintf(out, "\n  ");
      len = 2;
    }
    arg = proto[i];
    star = ((arg & 64) != 0) || (c_modes[arg&63]&4);
    arg &= 63;
    fprintf(out, "%s%s%s",
            c_types[arg], star? "*":"", (i==nargs-1)?");\n":", ");
    len += strlen(c_types[arg]) + star + 2;
  }

  /* finally, generate the Y_ wrapper definition itself */
  fprintf(out, "void\nY_%s(int n)\n{\n", var_last->name);
  if (nargs==1 && (c_modes[(unsigned char)proto[0]&63]&2) && 
      (c_modes[ret]&2)) nargs=0;
  if (nargs==1 && (c_modes[(unsigned char)proto[0]&63]&2)) {
    fprintf(out, "  if (n>1) YError(\"%s takes void argument\");\n",
            var_last->name);
  } else {
    fprintf(out, "  if (n!=%d) YError(\"%s takes exactly %d arguments\");\n",
            nargs, var_last->name, nargs);
  }
  fprintf(out, "  %s%s(", push_result[ret], var_last->cname);
  len = 2+strlen(push_result[ret])+strlen(var_last->cname);
  for (i=0 ; i<nargs ; i++) {
    if (len > 60) {
      fprintf(out, "\n    ");
      len = 4;
    }
    arg = proto[i];
    star = ((arg & 64) != 0) || (c_modes[arg&63]&4);
    arg &= 63;
    if (yarg_char[arg]) {
      fprintf(out, "yarg_%s%s(%d%s)%s", star? "":"s", yarg_char[arg],
              nargs-1-i, star? ",0":"", (i==nargs-1)?"":", ");
      len += 13-3*star + ((i<9)?1:2);
    }
  }
  fprintf(out, ")%s;\n}\n\n", push_finish[ret]);
}
