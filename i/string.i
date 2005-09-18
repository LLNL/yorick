/*
 * $Id: string.i,v 1.1 2005-09-18 22:06:00 dhmunro Exp $
 * String and related convenience functions.
 * (Based on routines contributed by Eric Theibaut.)
 */

/*---------------------------------------------------------------------------
 * string.i: string manipulation and miscellaneous functions for Yorick
 *           by Eric THIEBAUT.
 *----------------------------------------------------------------------------
 * History:
 *      01/23/05 by David Munro, reworked for yorick-1.6
 *      02/11/95 by Eric THIEBAUT: added definitions of `scalar()' and
 *              `is_vector()'.
 */

/* ------------------------------------------------------------------------ */

func gettime(&time)
/* DOCUMENT gettime -- get current time in the form "HH:MM:SS"
 *
 * SYNOPSIS: time= gettime();
 *           gettime, time;
 *
 * HISTORY: October 30, 1995 by Eric THIEBAUT.
 *
 * SEE ALSO: getdate, parsedate, timestamp.
 */
{ return (time= strpart(timestamp(), 12:19)); }

func getdate(&date)
/* DOCUMENT getdate -- get date of the day in the form "DD/MM/YY"
 *
 * SYNOPSIS: date= getdate();
 *           getdate, date;
 *
 * HISTORY: October 30, 1995 by Eric THIEBAUT.
 *
 * SEE ALSO: gettime, parsedate, timestamp.
 */
{
  local day, month, year;
  parsedate, timestamp(), day, month, year;
  year-= (year>=2000)? 2000 : 1900; 
  return (date= swrite(format="%02d/%02d/%02d", day, month, year));
}

func parsedate(timestamp, &day, &month, &year, &hour, &minute, &second)
/* DOCUMENT parsedate -- get numerical version of a timestamp
 *
 * SYNOPSIS: parsedate, timestamp, day,month,year, hour,minute,second;
 *           parsedate(timestamp)
 *
 * HISTORY: October 30, 1995 by Eric THIEBAUT.
 *   DHM modified for yorick-1.6 23/Jan/05
 *
 * SEE ALSO: gettime, getdate, timestamp.
 */
{
  dayName= "";
  monthName= "";
  day= year= hour= minute= second= 0;
  sread, timestamp, format="%s%s%d%d:%d:%d%d", dayName, monthName,
    day, hour, minute, second, year;
  month= where(monthName == ["Jan", "Feb", "Mar", "Apr",
                             "May", "Jun", "Jul", "Aug",
                             "Sep", "Oct", "Nov", "Dec"]);
  month= numberof(month)? month(1) : 13;
  return [day, month, year, hour, minute, second];
}

/* ------------------------------------------------------------------------ */

func strtoupper(s)
/* DOCUMENT strtoupper -- convert a string to upper case letters
 *
 * SYNOPSIS: s2 = strtoupper(s)
 *
 * HISTORY: October 10, 1995 by Eric THIEBAUT.
 *   DHM modified for yorick-1.6 23/Jan/05
 *
 * ********** DEPRECATED **************
 *   new code should use strcase directly
 *
 * SEE ALSO: strtolower
 */
{
  return strcase(1, s);
}

func strtolower(s)
/* DOCUMENT strtolower -- convert a string to lower case letters
 *
 * SYNOPSIS: s2 = strtolower(s)
 *
 * HISTORY: October 10, 1995 by Eric THIEBAUT.
 *   DHM modified for yorick-1.6 23/Jan/05
 *
 * ********** DEPRECATED **************
 *   new code should use strcase directly
 *
 * SEE ALSO: strtoupper
 */
{
  return strcase(0, s);
}

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

func is_scalar(x)
/* DOCUMENT is_scalar(object)
 *    returns 1 if OBJECT is a scalar, else 0.
 * SEE ALSO: is_array, is_func, is_void, is_range, is_struct, is_stream
 */
{ return is_array(x) && !dimsof(x)(1); }

func is_vector(x)
/* DOCUMENT is_vector(object)
 *    returns 1 if OBJECT is a vector (i.e., OBJECT has a single
 *    dimension), else 0.
 * SEE ALSO: is_array, is_func, is_void, is_range, is_struct, is_stream
 */
{ return is_array(x) && dimsof(x)(1)==1; }

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
