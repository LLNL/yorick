/*
 * $Id: string.i,v 1.2 2010-04-13 11:38:18 thiebaut Exp $
 * String and related convenience functions.
 */

/* ------------------------------------------------------------------------ */

func gettime(&time)
/* DOCUMENT time = gettime();
         or gettime, time;
     Get current time in the form "HH:MM:SS".

   SEE ALSO: getdate, parsedate, timestamp.
 */
{ return (time = strpart(timestamp(), 12:19)); }

func getdate(&date)
/* DOCUMENT date = getdate();
         or getdate, date;
     Get date of the day in the form "DD/MM/YY".

   SEE ALSO: gettime, parsedate, timestamp.
 */
{
  local day, month, year;
  parsedate, timestamp(), day, month, year;
  year -= (year >= 2000) ? 2000 : 1900;
  return (date= swrite(format="%02d/%02d/%02d", day, month, year));
}

local _parsedate_month_names;
func parsedate(ts, &day, &month, &year, &hour, &minute, &second)
/* DOCUMENT parsedate, timestamp, day,month,year, hour,minute,second;
         or parsedate(timestamp);
     Get numerical version of time as specified by TIMESTAMP.  When called
     as a function, the result is [DAY, MONTH, YEAR, HOUR, MINUTE, SECOND].

   SEE ALSO: gettime, getdate, timestamp.
 */
{
  day_name = month_name = string();
  day = year = hour = minute = second = 0;
  if (sread(ts, format="%s%s%d%d:%d:%d%d", day_name, month_name,
            day, hour, minute, second, year) != 7) {
    error, "invalid time-stamp string";
  }
  month = where(month_name == _parsedate_month_names);
  month = (is_array(month) ? month(1) : 13);
  return [day, month, year, hour, minute, second];
}
_parsedate_month_names = ["Jan", "Feb", "Mar", "Apr",
                          "May", "Jun", "Jul", "Aug",
                          "Sep", "Oct", "Nov", "Dec"];

/* ------------------------------------------------------------------------ */

func strtoupper(s) { return strcase(1, s); }
/* DOCUMENT strtoupper -- convert a string to upper case letters
 *
 * ********** DEPRECATED **************
 *   new code should use strcase directly
 *
 * SEE ALSO: strtolower
 */

func strtolower(s) { return strcase(0, s); }
/* DOCUMENT strtolower -- convert a string to lower case letters
 *
 * ********** DEPRECATED **************
 *   new code should use strcase directly
 *
 * SEE ALSO: strtoupper
 */

/* DHM removed strtrim, now in i0/ystr.i */

func strchr(s, c, last=)
/* DOCUMENT strchr -- get first/last index of a character in a string 
 *
 * SYNOPSIS: i = strchr(s, c)
 *           i = strchr(s, c, last=1)
 *
 * DIAGNOSTIC: returns 0 if character C is not found in string S.
 *
 * HISTORY: October 27, 1995 by Eric THIEBAUT.
 *   DHM modified for yorick-1.6 23/Jan/05
 *
 * ********** DEPRECATED **************
 *   new code should use strfind directly
 *
 * SEE ALSO: strmatch
 */
{
  if (is_void(last)) last = 0;
  return max(strfind(string(&char(c)), s, back=last)(2,..), 0);
}

/* ------------------------------------------------------------------------ */

func scalar(x, def, lt=, le=, gt=, ge=, type=, arg=, fn=)
/* DOCUMENT scalar -- get optional scalar parameter
 *
 * PROTOTYPE
 *   x = scalar(xarg, xdef, lt=, le=, gt=, ge=, type=, arg=, fn=);
 *
 * ARGUMENTS
 *   XARG    argument passed to the function.
 *   XDEF    default value for the scalar argument (optional, if not
 *           specified, then it is guessed that the caller must supply the
 *           argument).
 * KEYWORDS
 *   GE=     to be valid, XARG must be >= GE (optional, only one of GT or GE
 *           can be used).
 *   GT=     to be valid, XARG must be >  GT (optional, only one of GT or GE
 *           can be used).
 *   LE=     to be valid, XARG must be <= LE (optional, only one of LT or LE
 *           can be used).
 *   LT=     to be valid, XARG must be <  LT (optional, only one of LT or LE
 *           can be used).
 *   TYPE=   data type of the scalar (optional).
 *   FN=     function name for error messages (optional string).
 *   ARG=    argument name for error messages (optional string).
 *
 * DESCRIPTION
 *   Check XARG and return a scalar value (i.e., either XARG converted to TYPE
 *   if it is not void or XDEF otherwise).  If XARG is not within any specified
 *   bound or if it is not a scalar or if it is void (e.g., not specified) and
 *   there is no default value XDEF, an error message is written out.
 *
 * EXAMPLE
 *   The following function has 2 scalar arguments X and Y, the 1st one is an
 *   integer (of type long) which must be specified and be strictly greater
 *   than 22 while the 2nd default to .5 and must be in [0., 1.]:
 *     func foo(x,y) {
 *         x= scalar(x,     gt=22,        type=long,   fn="foo", arg="X");
 *         y= scalar(y, .5, ge=0., le=1., type=double, fn="foo", arg="Y");
 *         ...
 *     }
 *
 * WARNING
 *   There is no checking of consistency of options.
 *
 * HISTORY: 29 Sept. 1995 by Eric THIEBAUT.  (Modified slightly by DHM)
 */
{
  /* Efficiency note (DHM):
     This is pretty slow no matter what because of the long argument list.
     A faster implementation might be:
       check_range(default_value(x, def), lower, upper, flags)
     since you could optionally perform the various checks.  Of course,
     the total number of arguments for a complete test isn't any smaller,
     and there would be extra overhead in multiple function calls.
     Furthermore, it would be difficult to pass in the "user friendly"
     function and argument name options.  (The names of the routines in
     the current call chain would be a handy thing to make available by
     means of a Yorick builtin function call, as would the ability to
     have the error function "pop up" some number of levels so it left
     the person in dbug mode at the level of the caller of functions
     like this one...  That still leaves the argument name, though in
     principal Yorick can figure that out at runtime, too.)
   */

  /* get default x if necessary */
  if (is_void(x)) {
    if (is_void(def)) _scalar_err, 5;
    x= def;
  }

  /* check that x is indeed scalar */
  dims= dimsof(x);
  if (is_void(dims) || dims(1)) _scalar_err, 6;

  /* convert data type if required (note type could be function too) */
  if (!is_void(type)) x= type(x);

  /* check that x is in range */
  if (!is_void(lt) && x>=lt) _scalar_err, 1, lt;
  if (!is_void(le) && x>le) _scalar_err, 2, le;
  if (!is_void(gt) && x<=gt) _scalar_err, 3, gt;
  if (!is_void(ge) && x<ge) _scalar_err, 4, ge;

  return x;
}

func _scalar_err(oops, value)
{
  if (is_void(fn)) fn= "";
  else fn= fn+": ";
  if (is_void(arg)) arg= "argument";

  if (oops==5) {
    error, fn+"no default value for "+arg;
  } else if (oops==6) {
    error, fn+arg+" not a scalar value";
  } else {
    error, fn+arg+" must be "+["<","<=",">",">="](oops)+pr1(value);
  }
}

/* ------------------------------------------------------------------------ */
