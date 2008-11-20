/* textload.i
 * $Id: textload.i,v 1.3 2008-11-20 02:20:20 dhmunro Exp $
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
   SEE ALSO: text_lines, text_load
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
  if (is_void(quote)) quote = (delim == ",");
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
  ws = (c==' ') | (c=='\t') | (c=='\v') | (c=='\f');
  list = where(quotes);
  if (numberof(list) < 2) return !quotes;
  /* note that if numberof(list) is odd, we know the quoting is incorrect */
  /* mark open quotes as 1, character following close quotes as -1 */
  list = list(2::2);
  quotes(list) = 0;  /* close quote itself is part of the quote */
  --quotes(list+1);  /* if was open quote, don't open, else close */
  return quotes(psum);
}
