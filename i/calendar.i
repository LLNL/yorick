/*
 * $Id: calendar.i,v 1.1 2005-12-30 21:50:55 dhmunro Exp $
 * date.i
 * functions for computing time of day, day of year, etc.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*
 * The Julian calendar comprises a 4 year cycle of 1461 days
 * -- three years of 365 days followed by a year of 366 days.
 * The Gregorian calendar of modern times reduced the number of
 * days in a century from 36525 to 36524 for three centuries out
 * of every four, thus having a cycle of 146097 days every 400
 * years, with a mean year of 365.2425 days.
 *
 * See wikipedia.org/wiki/Mean_tropical_year for a discussion of
 * the accuracy of this convention; the rate of change of earth's
 * orbital parameters and the gradual lengthening of the day
 * invalidate the concept of any calendric periodicity at about
 * this level.  (The mean tropical year is currently 365.2422 days
 * while the time between vernal equinoxes is 365.2424 days, for
 * example.)
 *
 * The wikipedia.org/wiki/Gregorian_calendar article describes the
 * history of the Western calendar.  In summary:
 * The final day of the Julian calendar was 1582 Oct 4, which was
 * followed by 1582 Oct 15, by papal decree.  The Julian calendar,
 * however, was used in many non-Catholic countries long after this
 * date (and even in some Catholic countries until 1587).  In particular,
 * 1752 Sep 2 was followed by 1752 Sep 14 in the British Empire.  Russia
 * did not switch until 1918.  To confuse dates prior to about 1750 even
 * further, the date on which the year number changed varied; 25 Dec,
 * 25 Mar, and Easter were all popular choices.
 *
 * Months in our calendar, like weeks, have become totally arbitrary
 * divisions.  While you will easily find the 146097 and 1461 day
 * cycles in the following algorithms, the 153 day "cycle" associated
 * with the months has no physical basis or meaning at all; it is
 * merely taking advantage of a lucky accident of the pattern of
 * the number of days in the month.  To explain this, notice first
 * that the natural place to begin the year, from a purely computational
 * point of view, is 1 March, because it is most convenient to put
 * February, the only month with a variable number of days, last, so
 * that as the number of days in a year oscillates between 365 and 366,
 * the number of days in the final month can simply oscillate.  With
 * that ordering of the months, their lengths are:
 *   Mar Apr May Jun Jul   Aug Sep Oct Nov Dec   Jan Feb
 *   31  30  31  30  31    31  30  31  30  31    31 (28 or 29)
 * A very convenient way to describe this is as the first 2.4 cycles
 * of a 5 month pattern 31-30-31-30-31 of 153 days.  (There are many
 * other ways to generate this same pattern; this one happens to be
 * relatively easy to explain or excuse.  If the pattern had been only
 * slightly more irregular, no simple formula would have existed.)
 */

/* Julian Days (JD)
 * = astronomical day count based on exactly 365.25 days per Julian year
 *
 * J2000 astronomical epoch is 2000 January 1.5 (noon GMT on Jan 1, 2000)
 * (the instant it becomes 2000 January 1 everywhere on earth)
 * = 2451545.0 JD
 *
 * UNIX time
 * = seconds since 1970 January 1 00:00:00 UTC = 2440587.5 JD
 * = 2^31 at 2038 January 19 03:14:08 UTC = 2465442.634815 JD
 * = 946728000 at J2000
 */

func calendar(&y, &m, &d, n, julian=, noabbrev=)
/* DOCUMENT jdn = calendar(y, m, d)
 *       or calendar, y, m, d, jdn
 *   returns the Julian Day Number, JDN, associated with year Y,
 *   month M (1=Jan, 2=Feb, ..., 12=Dec) and day of month D in the
 *   first form.  In the second form, JDN is the input, and Y, M,
 *   and D are outputs.  
 *
 *   The Julian Day Number is a day count used by astronomers, which
 *   is by definition independent of any calendar; each passing day
 *   increments the count by one.  The zero day of the count falls a
 *   bit before 4700 BC, for esoteric reasons (see Wikipedia).
 *
 *   Julian Day Number modulo 7 (JDN%7) is the day of the week, with
 *   0=Mon, 1=Tue, ..., 6=Sun.
 *
 *   By default, Y, M, and D are in the modern Gregorian calendar.
 *   However, with the julian=1 keyword in either form, calendar
 *   treats Y, M, and D as the date in the Julian calendar, used
 *   in Roman Catholic countries before 1582, in the British
 *   empire before 1752, and in Eastern Orthodox countries well
 *   into the twentieth century (again, see Wikipedia).
 *
 *   In the first form (when computing JDN), if Y<50, calendar
 *   assumes 2000+Y, and if 50<=Y<100, calendar assumes 1900+Y,
 *   unless the noabbrev=1 keyword is present, in which case it
 *   assumes you are doing ill-advised archeological work.  In
 *   the second form, the output Y will always include the century.
 *   The julian=1 keyword implies noabbrev=1.
 *
 * SEE ALSO: unix_time, julian_day, datestamp
 */
{
  if (is_void(n)) {
    yy = long(y);
    mm = long(m);
    dd = long(d);
    janfeb = (mm < 3);
    if (!noabbrev && !julian) yy += (yy<100)*1900 + (yy<50)*100;
    yy += 4800 - janfeb;
    mm -= 3 - janfeb*12;
    if (julian) dd -= 32083;
    else dd += yy/400 - yy/100 - 32045;
    return 1461*yy/4 + (153*mm + 2)/5 + dd;

  } else {
    n = long(n);
    if (julian) {
      n += 32082;
    } else {
      n += 32044;
      c = (4*n + 3)/146097;
      n += (3*c + 3)/4;
    }
    y = (4*n + 3)/1461;
    d = n - 1461*y/4;
    m = (5*d+2)/153;
    d -= (153*m + 2)/5 - 1;
    janfeb = (m > 9);
    y += janfeb - 4800;
    m += 3 - janfeb*12;
  }
}

func unix_time(&y, &m, &d, &h, t, loc=, now=)
/* DOCUMENT t = unix_time(y, m, d, h)
 *       or unix_time, y, m, d, h, t
 *       or t = unix_time(now=1)
 *   converts numeric year Y, month M, day D, and hour H into the
 *   corresponding "UNIX time" T, the number of seconds since
 *   1970 Jan 1 00:00:00 UTC = 2440587.5 JD.
 *   The hour H can be a floating point number with a fractional part.
 *   In the second form, returns Y, M, D, and H, given T.
 *   In the third form, returns T for the current time.
 *   This requires calculating your time zone, as noted below.
 *
 *   By default, the input or returned Y, M, D, H represent Greenwich
 *   time (UT).  (The UNIX time is, by definition, in the Greenwich
 *   time zone.)  However, with the loc=1 keyword in the first two
 *   forms, Y, M, D, H are the local time.
 *
 *   The loc=1 or now=1 keywords require the time zone.  If it has not
 *   been previously set, the tz_set function is invoked to set it.
 *
 * SEE ALSO: julian_day, calendar, datestamp, base60, tz_set
 */
{
  /* zero for 1970 Jan 1 0000 UT */
  if (now) {
    if (is_void(tz_offset)) tz_set;
    ts = timestamp(t);
    if (!is_void(t) && t>=0) return t;
    datestamp, y, m, d, h, ts;
    return unix_time(y, m, d, h) + tz_offset;
  }
  if (loc && is_void(tz_offset)) tz_set;
  if (is_void(t)) {
    hh = is_void(h)? 0.0 : h;
    t = (calendar(y,m,d)-2440588)*86400 + long(3600.0*hh+0.5);
    if (loc) t += tz_offset;
  } else {
    if (loc) t -= tz_offset;
    calendar, y, m, d, (t / 86400) + 2440588;
    h = (t % 86400) / 3600.0;
  }
  return t;
}

func julian_day(&y, &m, &d, &h, jd, loc=, now=)
/* DOCUMENT jd = julian_day(y, m, d, h)
 *       or julian_day, y, m, d, h, jd
 *       or jd = julian_day(now=1)
 *   converts numeric year Y, month M, day D, and hour H into the
 *   corresponding Julian Day, which includes the fractional part
 *   of the day, measured from noon (not midnight) GMT.
 *   The hour H can be a floating point number with a fractional part.
 *   In the second form, returns Y, M, D, and H, given JD.
 *   In the third form, returns JD for the current time.
 *   This requires calculating your time zone, as noted below.
 *
 *   By default, the input or returned Y, M, D, H represent Greenwich
 *   time (UT).  (The UNIX time is, by definition, in the Greenwich
 *   time zone.)  However, with the loc=1 keyword in the first two
 *   forms, Y, M, D, H are the local time.
 *
 *   The loc=1 or now=1 keywords require the time zone.  If it has not
 *   been previously set, the tz_set function is invoked to set it.
 *
 * SEE ALSO: unix_time, calendar, datestamp, base60, tz_set
 */
{
  if (now) {
    t = unix_time(now=1);
    return (t / 86400.0) + 2440587.5;
  }
  if (loc && is_void(tz_offset)) tz_set;
  if (is_void(jd)) {
    jd = calendar(y,m,d) + (is_void(h)? 0.0 : (h-12.0)/24.0);
    if (loc) jd += tz_offset/86400.0;
  } else {
    if (loc) jd -= tz_offset/86400.0;
    h = long(jd + 0.5);
    calendar, y, m, d, h;
    h = (jd + 0.5 - h) * 24.0;
  }
  return jd;
}

func base60(&h, &m, &s, dec)
/* DOCUMENT dec = base60(h, m, s)
 *       or base60, h, m, s, dec
 *   converts hours (or degrees) H, minutes M, and seconds S into
 *   decimal hours (or degrees) DEC, in the first form.  In the
 *   second form, DEC is the input, and H, M, and S are the outputs
 *   (H and M are whole numbers of type double in that case).
 * SEE ALSO: unix_time, julian_day, datestamp, base60
 */
{
  if (is_void(dec)) {
    return h + m/60.0 + s/3600.0;
  } else {
    h = floor(dec);
    s = 60.0*(dec-h);
    m = floor(s);
    s = 60.0*(s-m);
  }
}

func datestamp(&y, &m, &d, &h, ts)
/* DOCUMENT ts = datestamp(y, m, d, h)
 *       or datestamp, y, m, d, h, ts
 *       or ts = datestamp(y, m, d, h, 1)
 *   converts numeric year Y, month M, day D, and hour H into the 25
 *   character string format TS returned by the timestamp() function.
 *   The hour H can be a floating point number witha fractional part.
 *   In the second form, returns Y, M, D, and H, given TS.
 *   In the third form, returns Y, M, D, H, and TS for the current
 *   local time, as returned by the timestamp() function.
 * SEE ALSO: timestamp, unix_time, julian_day, base60, calendar
 */
{
  if (!is_void(ts) && structof(ts)!=string)
    ts = timestamp();
  if (is_void(ts)) {
    hh = long(h);
    s = 60.0*(h-hh);
    mm = long(s);
    s = long(60.*(s-mm));
    dow = _day_names(calendar(y,m,d)%7 + 1);
    mon = _month_names(m);
    y += (y<100)*1900 + (y<50)*100;
    ts = swrite(format="%s %s %2ld %02ld:%02ld:%02ld %04ld",
                dow, mon, d, hh,mm,s, y);
  } else {
    dow = mon = "";
    d = h = mm = s = y = 0;
    sread, ts, format="%s %s %ld %ld:%ld:%ld %ld", dow, mon, d, h, mm, s, y;
    m = where(mon == _month_names)(1);
    h += (60*mm + s)/3600.0;
  }
  return ts;
}

_month_names = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
_day_names = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];

func tz_set(offset)
/* DOCUMENT tz_set
 *       or tz_set, offset
 *   sets the tz_offset global variable, which is the difference
 *   between local time and Greenwich mean time in seconds.  In the
 *   second form, you specify the offset directly.  In the first form,
 *   tz_set attempts to compute it, either from the UNIX time returned
 *   by the timestamp function, or from the system date utility if
 *   that is not available.  If neither of these attempts works,
 *   sets tz_offset to zero.
 * SEE ALSO: unix_time, julian_day
 */
{
  extern tz_offset;
  if (!is_void(offset)) {
    tz_offset = long(offset);
    return;
  }
  local ut, y, m, d, h;
  datestamp, y, m, d, h, timestamp(ut);
  t = unix_time(y,m,d,h, loc=0);
  if (is_void(ut) || ut<0) {
    uh = mm = ss = -1;
    sread, rdline(popen(tz_date_cmd, 0)), format="%ld %ld %ld", uh, mm, ss;
    if (ss < 0) {
      /* just hope we're in Greenwich */
      tz_offset = 0;
      return;
    }
    uh += (mm*60+ss)/3600.0;
    /* round time difference to nearest hour */
    dh = long(floor(uh - h + 24.5));
  } else {
    /* round time difference to nearest hour */
    dh = (ut - t + 88200)/3600;
  }
  tz_offset = (dh%24)*3600;
}

/* command line to retrieve UT from system date utility */
tz_date_cmd = "date -u '+%H %M %S'";
