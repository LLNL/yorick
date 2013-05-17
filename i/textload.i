/* textload.i
 * $Id: textload.i,v 1.4 2008-11-21 01:46:56 dhmunro Exp $
 * read text files with any end-of-line convention
 *   (handles UNIX LF, Windows/DOS CRLF, or old Mac CR)
 * functions:
 *   text_load   low level worker, returns char array
 *   text_lines  returns one string per line (no trailing newline)
 *   text_cells  returns 2D array with comma or tab column separator
 */

func text_load(filename)
/* DOCUMENT char_array = text_load(filename)
     returns char array representing the text file FILENAME.  If FILENAME
     contains old Mac OS CR end-of-line characters, or Windows/DOS CRLF
     end-of-line sequences, these are converted to the single LF UNIX
     end-of-line.  Adds final newline if not present.
   SEE ALSO: text_lines, text_cells
 */
{
  f = open(filename, "rb");
  n = sizeof(f);
  if (!n) return [];
  c = array(char, n);
  if (_read(f, 0, c) != n) return [];
  close, f;
  mask = ((c == '\f') | !c);
  if (sum(mask)) {  /* remove formfeeds and nulls */
    c = c(where(!mask));
    n = numberof(c);
    if (!n) return [];
  }
  if (c(0) == '\015') c(0) = '\012';
  cr = where(c == '\015');
  ncr = numberof(cr);
  if (ncr) {
    list = where(c(cr+1) != '\012');
    if (numberof(list)) c(cr(list)) = '\012';
    if (ncr > numberof(list)) c = c(where(c != '\015'));
  }
  if (c(0) != '\012') grow, c, ['\012'];
  return c;
}

func text_lines(filename)
/* DOCUMENT string_array = text_lines(filename)
     returns string array representing the text file FILENAME, one
     string per line.  Unlike rdline, text_lines handles old Mac and
     Windows/DOS end-of-lines correctly.
   SEE ALSO: text_cells, text_load
 */
{
  c = (structof(filename)!=char)? text_load(filename) : filename;
  if (!numberof(c)) return [];
  list = where(c == '\012');
  if (numberof(list)) c(list) = '\0';
  return strchar(c);
}

func text_cells(filename, delim, quote=)
/* DOCUMENT text = text_cells(filename, delim)
     returns a 2D array of strings containing the spreadsheet data
     in FILENAME.  FILENAME can be comma separated values (.csv),
     or tab-delimited columns.  DELIM is the field delimiter character,
     which can be omitted to get the following default behavior:
     1. If FILENAME ends in ".csv" (any case), DELIM = ",".
     2. If the file contains any tab characters, DELIM = "\t".
     3. Otherwise, DELIM = ",".
     If DELIM = ",", an attempt is made to conform with.csv format
     conventions with respect to quoted fields.
     The quote= keyword controls whether or not to exclude field
     separators (delim or newline) enclosed in "...".  The default
     is quote=1 (yes) for DELIM=",", otherwise quote=0 (no).
   SEE ALSO: text_lines, text_load, text_csv
 */
{
  if (structof(filename) != string) {
    c = filename;
  } else {
    c = text_load(filename);
    if (is_void(delim) && strmatch(strpart(filename,-3:0), ".csv", 1))
      delim = ',';
  }
  if (!numberof(c)) return [];
  if (is_void(delim))
    delim = anyof(c=='\t')? '\t' : ',';
  else if (structof(delim) == string)
    delim = (*pointer(delim))(1);

  /* make mask that is 1 for delim, -1 for eol */
  mask = (c == delim) - (c == '\012');
  /* allow for CSV quoted delimiter convention */
  if (is_void(quote)) quote = (delim == ',');
  if (quote) mask *= text_unquoted(c);

  fields = where(mask);
  c(fields) = '\0';                /* mark fields for strchar */
  n = numberof(fields);
  mask = mask(fields);
  rows = where(mask < 0);
  nrows = numberof(rows);
  nfields = grow([0], rows)(dif);  /* number of fields in each row */
  ncols = max(nfields);
  mask = array(0, ncols+1, nrows);
  mask(1+nfields+(ncols+1)*indgen(0:nrows-1)) = 1;
  mask = !mask(psum:1:ncols,);

  s = array(string, ncols, nrows);
  s(where(mask)) = strchar(c);
  if (quote) {
    list = where((strpart(s,1:1)=="\"") & (strpart(s,0:0)=="\""));
    if (numberof(list)) s(list) = strpart(s(list),2:-1);
  }
  return s;
}

/* return mask of unquoted characters
 * this simple algorithm not exactly the same as the complex CSV grammar
 *   that allows for the "Paradox" bug,
 * but it is exactly the correct "" quote escape grammar
 * it will still work for the "Paradox" bug in the case that the
 *   unescaped interior quotes are paired and contain no delimiters or eols
 */
func text_unquoted(c)
{
  quotes = (c == '"');
  list = where(quotes);
  if (numberof(list) < 2) return !quotes;
  /* note that if numberof(list) is odd, we know the quoting is incorrect */
  /* mark open quotes as 1, character following close quotes as -1 */
  list = list(2::2);
  quotes(list) = 0;  /* close quote itself is part of the quote */
  if (list(0) == numberof(c)) {
    if (numberof(list) < 2) return !quotes(psum);
    list = list(1:-1);
  }
  --quotes(list+1);  /* if was open quote, don't open, else close */
  return !quotes(psum);
}

func text_csv(f, .., tab=,fmt=,head=)
/* DOCUMENT text_csv, file, col1, col2, ..., colN
            f = text_csv(file, col1, col2, ..., colN)
     write comma or tab delimited columns COL1, ... COLN to FILE, which
     may be a filename, a text file handle, or nil [] to write to the
     terminal.  Called as a function, returns the open text file handle.
     The default delimiter between columns is a comma, unless FILE is
     nil, in which case the default delimiter is tab.  You can force tab
     delimited columns using the tab=1 keyword, and comma delimited columns
     using tab=0.
  
     Each COLi may be nil to leave an empty column, a 1D array to
     produce a single column, or a 2D array to produce several columns.
     For 2D arrays, the first index is the row index, and the second is
     the column index.  Acceptable data types are string or any numeric
     data type.  The columns need not have the same length; the first
     row will be shared.  Numeric types are converted to strings using
     the totxt function.  You can pass a format argument to totxt using
     a fmt= keyword to text_csv.  If fmt=[fmt1,fmt2,...,fmtM], the
     formats will apply to the first M columns (note that one COLi spans
     multiple columns if it is 2D, so multiple fmtM may apply).  The
     fmtI only apply to non-string COLi; the fmtI corresponding to a
     string COLi are ignored.

     Finally, text_csv accepts a head=[head1,head2,...,headM] to write
     a first row of column headings.  Thus,
       text_csv, filename, head=[h1,h2,h3], c1, c2, c3;
     is equivalent to
       text_csv, text_csv(filename, h1, h2, h3), c1, c2, c3;
     assuming that h1, h2, and h3 are scalar strings.  Like fmt=, the
     head= are per column, not per COLi argument.

     Different platforms (e.g.- MSWindows, MacOS X, Linux, etc) behave
     differently, but here are some things to try in order to move your
     yorick arrays into a spreadsheet: If you write tab delimited columns
     to your terminal, you may find that cutting the output from your
     terminal window and pasting it into your spreadsheet window properly
     preserves your columns.  Additionally, if you write a file whose
     name ends in ".csv", your file manager will probably recognize that
     it should be opened in a spreadsheet program.  (You might also want
     to experiment with comma or tab delimited text file names ending in
     ".xls", which often behave like actual spreadsheet files.)  Finally,
     if you are an emacs user, don't miss csv-mode in recent versions.
   SEE ALSO: text_cells, totxt
 */
{
  if (is_void(tab)) tab = is_void(f);
  if (!is_void(head)) {
    d = dimsof(head)(1);
    if (d > 1) error, "head= keyword argument must be scalar or 1D";
    else if (!d) head = [head];
    f = text_csv(f, transpose([head]), tab=tab);
  }
  tab = tab? "\t" : ",";
  n = more_args();
  p = array(pointer, max(n,1));
  nr = array(0, max(n,1));
  for (i=1 ; i<=n ; ++i) {
    p(i) = &(a = next_arg());
    if (is_void(a)) a = "";
    d = dimsof(a);
    if (d(1) > 2) error, "only scalar, 1D, or 2D arrays accepted";
    nr(i) = d(1)? d(2) : 1;
  }
  s = array("", max(max(nr),1));
  for (i=j=1,b=string(0) ; i<=n ; ++i) {
    eq_nocopy, a, *p(i);
    d = dimsof(a);
    nc = (d(1)>1)? d(3) : 1;
    r = nr(i);
    for (k=1 ; k<=nc ; ++k,++j,b=tab) {
      s += b;
      if (structof(a) == string) s(1:r) += a(,k);
      else s(1:r) += totxt(a(,k), ((j<=numberof(fmt))? fmt(j) : []));
    }
  }
  if (!is_void(f) && (structof(f)==string)) f = create(f);
  write, f, format="%s\n", linesize=max(strlen(s))+2, s;
  return f;
}
