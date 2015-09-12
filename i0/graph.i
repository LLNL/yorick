/*
 * $Id: graph.i,v 1.15 2011-01-09 01:17:46 dhmunro Exp $
 * Declarations of Yorick graphics functions.
 */
/* Copyright (c) 2005, The Regents of the University of California.
 * All rights reserved.
 * This file is part of yorick (http://yorick.sourceforge.net).
 * Read the accompanying LICENSE file for details.
 */

/*= SECTION(plotout) controlling plot windows and files ====================*/

extern window;
/* DOCUMENT window, n, display="host:server.screen", dpi=100/75, wait=0/1,
                       private=0/1, hcp="hcp_filename", dump=0/1,
                       legends=1/0, style="style_sheet_filename",
                       width=wpixels,height=hpixels,rgb=1,
                       parent=id,xpos=x_in_parent,ypos=y_in_parent

     select window N as the current graphics output window.  N may
     range from 0 to 63, inclusive.  Each graphics window corresponds to
     an X window, and optionally has its own associated hardcopy file.
     If N is omitted, it defaults to the current coordinate system.

     The X window will appear on your default display at 75 dpi, unless
     you specify the display and/or dpi keywords.  A dpi=100 X window
     is larger than a dpi=75 X window; both represent the same thing
     on paper.  Use display="",hcp="filename" to create a graphics window
     which has no associated X window, but instead plots to a hardcopy
     file (you should do this if you want to make plots in a non-interactive
     batch mode).

     By default, if the X window needs to be created, the graphics area
     will be 450x450 pixels if dpi=75, or 600x600 pixels if dpi=100,
     representing a 6x6 inch square on hardcopy paper.  You can override
     this default initial size using the width and height keywords.
     These settings remain in force indefinitely; use width=0,height=0
     to return to the default dpi-dependent behavior.  For a dpi=75,
     landscape=0 window, width=638,height=825 displays the entire sheet
     of hardcopy paper.  Supplying these keywords will not change the
     size of an existing window; only newly created windows.

     By default, an X window will attempt to use shared colors, which
     permits several Yorick graphics windows (including windows from
     multiple instances of Yorick) to use a common palette.  You can
     force an X window to post its own colormap (set its colormap
     attribute) with the private=1 keyword.  You will most likely have
     to fiddle with your window manager to understand how it handles
     colormap focus if you do this.  Use private=0 to return to shared
     colors.

     By default, Yorick will not wait for the X window to become visible;
     code which creates a new window, then plots a series of frames to
     that window should use wait=1 to assure that all frames are actually
     plotted.

     By default, a graphics window does NOT have a hardcopy file
     of its own -- any request for hardcopy are directed to the
     default hardcopy file, so hardcopy output from any window goes
     to a single file.  By specifying the hcp keyword, however, a
     hardcopy file unique to this window will be created.  If the
     "hcp_filename" ends in ".cgm", the hardcopy file is a binary CGM
     file; otherwise, hardcopy files are in Postscript format.  Use
     hcp="" to revert to the default hardcopy file (closing the window
     specific file, if any).  The legends keyword, if present, controls
     whether the curve legends are (legends=1, the default) or are not
     (legends=0) dumped to the hardcopy file.  The dump keyword, if
     present, controls whether all colors are converted to a gray scale,
     (dump=0), or the current palette is dumped at the beginning of each
     page of hardcopy output (dump=1, the default).  (The legends keyword
     applies to all pictures dumped to hardcopy from this graphics
     window.  The dump keyword applies only to the specific hardcopy
     file defined using the hcp keyword -- use the dump keyword in the
     hcp_file command to get the same effect in the default hardcopy
     file.)

     Use rgb=1 to set the rgb color model when you are creating a
     window on an 8-bit display on which you intend to use three
     component rgb colors (see color).  This installs the 5x9x5
     colorcube and avoids having to issue the palette command
     after the first true color object has been drawn.

     If both display="" and hcp="", the graphics window will be
     entirely eliminated.

     The style keyword, if present, specifies the name of a Gist style
     sheet file; the default is "work.gs".  The style sheet determines
     the number and location of coordinate systems, tick and label styles,
     and the like.  Other choices include "axes.gs", "boxed.gs",
     "work2.gs", and "boxed2.gs".

     The parent=id keyword can be used to make the yorick window a
     subwindow of an existing window.  The id is an integer, which is
     the system-dependent window id that must be retrieved from the
     application which owns the parent window.  When parent= is defined,
     xpos= and ypos= specify the offset in that window; both default to 0.

     If invoked as a function, window(...) returns the current
     window number.
   SEE ALSO: plsys, hcp_file, fma, hcp, redraw, palette, animate, plg,
             winkill, gridxy, no_window, cmap, torgb
 */

func winkill(n)
/* DOCUMENT winkill
         or winkill, n
     deletes the current graphics window, or graphics window N (0-63).
   SEE ALSO: window
 */
{
  window, n, display="", hcp="";
}

extern current_window;
/* DOCUMENT n= current_window()
     returns the number of the current graphics window, or -1 if none.
 */

extern window_geometry;
/* DOCUMENT window_geometry()
         or window_geometry(win)
     Get geometry settings of the  visible region of display window WIN (or
     current window  if WIN is nil  or not specified).   These settings are
     subject to change  each time the window get resized.   The result is a
     vector of 6 doubles:
       [DPI, ONE_PIXEL, XBIAS, YBIAS, WIDTH, HEIGHT]
     where:
       DPI = dot-per-inch of WIN
       ONE_PIXEL = pixel size in NDC units
       XBIAS = abscissa offset in NDC units
       YBIAS = ordinate offset in NDC units
       WIDTH = width of visible region in pixels
       HEIGHT = height of visible region in pixels
     Pixel coordinates (XPIX,YPIX) run  from top-left (0,0) to bottom-right
     (WIDTH-1,HEIGHT-1).  The conversion to NDC coordinates is:
       XNDC = XBIAS + XPIX*ONE_PIXEL;
       YNDC = YBIAS - YPIX*ONE_PIXEL;

     If window WIN does not exists, all output values are zero.

     Notes:
       (1) The  top/left  margin(s) used  by  Gist window  to display  some
           message are not considered as part of the "visible" region.
       (2) An  extra 0.5  pixel offset has  been added to  (XBIAS,YBIAS) to
           avoid rounding errors.

   SEE ALSO: window, current_window, viewport, limits.
 */

extern window_select;
extern window_exists;
extern window_list;
/* DOCUMENT window_select(n)
         or window_exists(n)
         or window_list()

     The function window_select makes window number N the current one and
     return 1 (true); unless window number N does not exists, in which case
     the current window is left unchanged and 0 (false) is returned.

     The function window_exists returns 1 or 0 whether or not window number
     N exists.

     The function window_list returns the list of existing windows as a
     vector of longs or nil if no window currently exists.

   SEE ALSO: window, current_window, redraw, fma, limits, window_geometry.
 */

extern current_mouse;
local focused_window;
local has_mouse;
/* DOCUMENT current_mouse();
         or current_mouse(win);
         or focused_window();
         or has_mouse();
         or has_mouse(win);

     The function current_mouse returns the pointer position in the
     graphics window with pointer focus as an array of double's in the
     form [X,Y,SYS,WIN] where X and Y are the pointer coordinates in
     the coordinate system SYS and WIN is the number of the graphics
     window.  If no graphics window currently has the pointer focus or
     if WIN is specified but does not match the graphics window with
     pointer focus, the result is empty.

     The function focused_window returns the number of the graphics
     window with pointer focus, or -1 if none.

     The function has_mouse with a void argument returns true if any
     Yorick graphics window has the pointer focus. If WIN is
     specified, the function has_mouse returns true if graphics window
     WIN has the pointer focus.

     Note that the window which has the pointer focus may be different
     from the so-called current window to which graphics commands are
     directed.  The built-in functions `window' and `current_window'
     (which see) can be used to set/query the current window.

   SEE ALSO: current_window, mouse, window
 */

func focused_window(nil)
{
  m = current_mouse();
  if (is_void(m)) return -1;
  return long(m(4));
}

func has_mouse(win)
{
  return (! is_void(current_mouse(win)));
}

extern viewport;
/* DOCUMENT port= viewport();
     returns [xmin,xmax,ymin,ymax] of the current viewport (or 0,0,0,0
     if currently plotting to system 0) in NDC coordinates.
   SEE ALSO: limits, gridxy
 */

extern raw_style;
/* DOCUMENT raw_style: get_style, set_style, read_style, write_style
     alternatives to the style= keyword of the window command which
     allow the interpreter to set or get all the details of the
     window style.  Read the help for get_style.
 */

extern set_gpath;
/* DOCUMENT old = set_gpath(gist_path)
     set path (colon delimited directories) for Gist graphics package,
     returning old path.  GIST_PATH nil or string(0) just returns old.
 */

extern hcp_file;
/* DOCUMENT hcp_file, filename, dump=0/1, ps=0/1
     sets the default hardcopy file to FILENAME.  If FILENAME ends with
     ".cgm", the file will be a binary CGM, otherwise it will be a
     Postscript file.  By default, the hardcopy file name will be
     "Aa00.ps", or "Ab00.ps" if that exists, or "Ac00.ps" if both
     exist, and so on.  The default hardcopy file gets hardcopy from all
     graphics windows which do not have their own specific hardcopy file
     (see the window command).  If the dump keyword is present and non-zero,
     the current palette will be dumped at the beginning of each frame
     of the default hardcopy file (default behavior).  With dump=0,
     all colors are converted to a gray scale, and the output files are
     smaller because no palette information is included.
     Use ps=0 to make "Aa00.cgm", "Ab00.cgm", etc by default instead of
     Postscript.
     The dump= and ps= settings persist until explicitly changed by a
     second call to hcp_file; the dump=1 setting becomes the default for
     the window command as well.
   SEE ALSO: window, fma, hcp, plg, no_window
 */

extern hcp_finish;
/* DOCUMENT filename= hcp_finish()
         or filename= hcp_finish(n)
     closes the current hardcopy file and returns the filename.
     If N is specified, closes the hcp file associated with window N
     and returns its name; use hcp_finish(-1) to close the default
     hardcopy file.
   SEE ALSO: pdf_finish, window, fma, hcp, hcp_out, plg
 */

func hcp_out(n,keep=)
/* DOCUMENT hcp_out
         or hcp_out, n
     finishes the current hardcopy file and sends it to the printer.
     If N is specified, prints the hcp file associated with window N;
     use hcp_out,-1 to print the default hardcopy file.
     Unless the KEEP keyword is supplied and non-zero, the file will
     be deleted after it is processed by gist and sent to lpr.
   SEE ALSO: window, fma, hcp, hcp_finish, plg
 */
{
  filename= hcp_finish(n);
  if (filename) {
    if (strpart(filename,-2:0)==".ps")
      system, swrite(format=LPR_FORMAT, filename);
    else
      system, swrite(format=GIST_FORMAT, filename);
    if (!keep) remove, filename;
  }
}

func no_window(name, style=)
/* DOCUMENT no_window
            no_window, hcpname
            no_window, ""
     Set up a graphics window with no interactive display, similar to
       window, display="", hcp=hcpname, dump=1, legends=0;
     You can optionally supply a filename HCPNAME; if you do not, the
     default filename will be "no_window".  If HCPNAME is "" or string(0),
     this graphics window is killed, and the special behavior of the
     eps and other commands (see below) is restored to normal.  Use
     no_window if you do not want to create an interactive graphics
     window, for example when yorick is running in batch mode and is
     not connected to any interactive graphics devices, causing the
     code to crash when it tries to create an interactive window.

     As a convenience, no_window accepts a style= keyword, which it will
     pass along to the window command.  If you need to set other window
     properties, call the window function after no_window.
     
     Additionally, the no_window function changes the behavior of the
     single picture commands hcps, eps, pdf, png, jpeg (and other functions
     based on the hcps command) to write the current drawing to the specified
     file, then reissue a non-displaying window command.  The effect is to
     simplify making a sequence of plots in batch mode without creating any
     interactive graphics window.  If you want to write the whole sequence
     into a single .ps file, you use the no_window function to set the
     filename, then hcp or hcp_on to dump frames into the file.

     Alternatively, if you need to write one file per frame (for example
     one png per picture to include in slides using presentation software),
     you can call no_window, then issue the png (or similar) command just
     before advancing to the next frame.  (Unfortunately, you cannot do
     both -- either you are writing all the frames into one ps file, or
     you are writing one frame per file.  Calling the single frame function
     will close the postscript file.)

   SEE ALSO: hcps, eps, pdf, png, jpeg
 */
{
  if (is_void(name)) name = "no_window.ps";
  extern _no_window;
  if (!strlen(name)) {
    _no_window = [];
    winkill;
  } else {
    _no_window = name;
    window, display="", hcp=_no_window, style=style;
  }
}

extern keybd_focus;
/* DOCUMENT keybd_focus, on_off
     By default, graphics windows set a window manager hint which
     allows them to accept keyboard focus.  With ON_OFF zero, that
     hint will not be set when a new graphics window is created.
     This causes the window manager to refuse to offer keyboard
     focus to the graphics window -- very desirable, since it can't
     accept keyboard input anyway.  With fvwm, for example, this
     means keyboard focus can stay in the terminal window even when
     you are mouse zooming the graphics window.  However, many
     window managers confuse colormap focus with keyboard focus, so
     if you set the private=1 colormap in the window function, you
     may not be able to convince the window manager to give the
     graphics window colormap focus since it won't give it keyboard
     focus.  Weird.
 */

func hcps(name)
/* DOCUMENT hcps, name
     writes the picture in the current graphics window to the
     PostScript file NAME+".ps" (i.e.- the suffix .ps is added to NAME).
     Legends are not written, but the palette is always dumped.
     This is mostly for internal use; see png, svg, eps, or jpeg.
   SEE ALSO: png, window, fma, hcp, hcp_finish, plg, no_window
 */
{
  if (strpart(name,-2:0)!=".ps") name+= ".ps";
  extern hcp;
  window, hcp=name, dump=1, legends=0;
  hcp;
  if (!_no_window) window, hcp="";
  else window, display="", hcp=_no_window;
  return name;
}

func epsi(name)
/* DOCUMENT eps, name
     writes the picture in the current graphics window to the Encapsulated
     PostScript file NAME+".epsi" (i.e.- the suffix .epsi is added to NAME).
     The eps function requires the ps2epsi utility which comes with the
     project GNU Ghostscript program.  Any hardcopy file associated with
     the current window is first closed, but the default hardcopy file is
     unaffected.  As a side effect, legends are turned off and color table
     dumping is turned on for the current window.
     The external variable PS2EPSI_FORMAT contains the format for the
     command to start the ps2epsi program.
   SEE ALSO: eps, hcps, window, fma, hcp, hcp_finish, plg, no_window
 */
{
  name= hcps(name);
  system, swrite(format=PS2EPSI_FORMAT, name);
  remove, name;
}
if (is_void(PS2EPSI_FORMAT)) PS2EPSI_FORMAT= "ps2epsi %s";

func eps(name, pdf=)
/* DOCUMENT eps, name
     writes the picture in the current graphics window to the Encapsulated
     PostScript file NAME+".eps" (i.e.- the suffix .eps is added to NAME).
     This function requires ghostscript.  Any hardcopy file associated with
     the current window is first closed, but the default hardcopy file is
     unaffected.  As a side effect, legends are turned off and color table
     dumping is turned on for the current window.
     The external variable EPSGS_CMD contains the command to start
     ghostscript.

     See help,pdf for advice about hairline artifacts between filled
     polygons in plf or plfc output.

   SEE ALSO: pdf, png, svg, jpeg, epsi, hcps, window, fma, hcp, no_window, plg
 */
{
  if (strpart(name, -3:0) == ".eps") name = strpart(name,1:-4);
  /* dump the postscript file */
  psname = hcps(name+".pseps");

  /* begin copying to the eps file */
  f = create(name+".eps");
  g = open(psname);
  write, f, format="%s\n", "%!PS-Adobe-2.0 EPSF-1.2";
  rdline, g;
  line = rdline(g);
  if (strmatch(line,"% EPSF-3.0")) line = rdline(g); /* old ps.ps bug */
  for (i=1 ; i<=4 ; i++) {  /* Title For CreationDate Creator */
    write, f, format="%s\n", line;
    line = rdline(g);
  }

  /* use ghostscript to compute true bounding box */
  bbname = name+".bbeps";
  for (;;) {
    gscmd = EPSGS_CMD+" -sDEVICE=bbox -sOutputFile=- \"%s\" >>\"%s\" 2>&1";
    system, swrite(format=gscmd, psname, bbname);
    bb = rdline(open(bbname), 20);
    bb = bb(where(bb));
    remove, bbname;
    tok = strtok(bb);
    list = where(tok(1,) == "%%HiResBoundingBox:");
    if (!numberof(list)) {
      list = where(tok(1,) == "%%BoundingBox:");
    }
    xmn = ymn = xmx = ymx = 0.;
    if (!numberof(list) || sread(tok(2,list(1)), xmn, ymn, xmx, ymx) != 4) {
      /* Ghostscript 7.07 bbox fails if -dSAFER present,
       * Ghostscript 8.61 bbox fails if -dSAFER absent
       * the 8.61 failure gives an incorrect bounding box, so will never
       *   reach this workaround code
       * the 7.07 bug produces no BoundingBox comments at all, so will
       *   reach here
       * therefore, -dSAFER should be present in EPSGS_CMD initially
       *   and be removed for a second try with code that works in 7.07
       */
      if (strpart(EPSGS_CMD, -7:0) == " -dSAFER")
        EPSGS_CMD = strpart(EPSGS_CMD, 1:-8);
      else
        error, "ghostscript sDEVICE=bbox bug workaround failed";
    }
    break;
  }

  if (!pdf) {
    write, f, format="%s\n", bb;
    write, f, format="%s\n", "save countdictstack mark newpath "+
      "/showpage {} def /setpagedevice {pop} def";
  } else {
    /* concept from epstopdf perl script
     * by Sebastian Rahtz and Heiko Oberdiek,
     * distributed as part of the TeTeX package, see http://www.tug.org
     */
    write, f, format="%%BoundingBox: 0 0 %f %f\n", xmx-xmn, ymx-ymn;
    write, f, format="<< /PageSize [ %f %f ] >> setpagedevice\n",
      xmx-xmn, ymx-ymn;
    write, f, format="gsave %f %f translate\n",
      -xmn, -ymn;
  }
  write, f, format="%s\n", "%%EndProlog";
  while (line) {
    if (strpart(line,1:2)!="%%")
      write, f, format="%s\n", line;
    line = rdline(g);
  }

  close, g;
  remove, psname;

  write, f, format="%s\n", "%%Trailer";
  if (!pdf) {
    write, f, format="%s\n", "cleartomark "+
      "countdictstack exch sub { end } repeat restore";
  } else {
    write, f, format="%s\n", "grestore";
  }
  write, f, format="%s\n", "%%EOF";
  close, f;
  return name+".eps";
}
/* -dSAFER option should be present (see above) and must be last */
if (is_void(EPSGS_CMD)) EPSGS_CMD= "gs -q -dNOPAUSE -dBATCH -dSAFER";

func pdf(name)
/* DOCUMENT pdf, name
     writes the picture in the current graphics window to the Adobe PDF
     file NAME+".pdf" (i.e.- the suffix .pdf is added to NAME).  The
     pdf file is intended to be imported into MS PowerPoint or other
     commercial presentation software, or into in pdftex or pdflatex
     documents; it is cropped.  The result should be equivalent to
     running the epstopdf utility (which comes with TeX, see www.tug.org)
     on the eps file produced by the eps command.
     This function requires ghostscript.  Any hardcopy file associated with
     the current window is first closed, but the default hardcopy file is
     unaffected.  As a side effect, legends are turned off and color table
     dumping is turned on for the current window.
     The external variable EPSGS_CMD contains the command to start
     ghostscript.

     You may have problems with hairline artifacts in plf or plfc output.
     This turns out to be caused by anti-aliasing; the files are correct,
     but modern rendering engines create the artifacts by trying to
     soften pixelated edges.  If you can figure out how to turn off
     antialiasing in your rendering software, you can generally get
     rid of the hairlines.  The downside is, that text an diagonal lines
     will have jagged edges and look worse.  What you want is a very
     high resolution display, and turn off anti-aliasing (since it isn't
     so necessary).

   SEE ALSO: png, svg, jpeg, eps, hcps, window, fma, hcp, no_window, plg,
             pdf_finish
 */
{
  if (strpart(name, -3:0) == ".pdf") name = strpart(name,1:-4);
  /* first run ghostscript to produce an eps translated to (0,0) */
  psname = eps(name+".pdf", pdf=1);
  /* second run ghostscript to produce the cropped pdf */
  gscmd = EPSGS_CMD+" -sDEVICE=pdfwrite -sOutputFile=\"%s\" \"%s\"";
  system, swrite(format=gscmd, name+".pdf", psname);
  remove, psname;
}

func pdf_finish(n)
/* DOCUMENT pdf_finish
         or pdf_finish, n
         or pdf_finish(n)
     closes the current hardcopy file and converts it to pdf format.
     If N is specified, closes the hcp file associated with window N
     and converts it; use pdf_finish,-1 to close the default
     hardcopy file.  Called as a function, pdf_finish returns the name
     of the pdf file created.
     The pdf_finish function only works if the hcp file is a Postscript
     file, with a name ending in ".ps".  The pdf file will have the same
     name, except ending in ".pdf".
   SEE ALSO: pdf, hcp_finish
 */
{
  psname = hcp_finish(n);
  if (!psname || strpart(psname, -2:0)!=".ps")
    error, "pdf_finish only works with postscipt hcp files";
  name = strpart(psname, 1:-3) + ".pdf";
  gscmd = EPSGS_CMD+" -sDEVICE=pdfwrite -sOutputFile=\"%s\" \"%s\"";
  system, swrite(format=gscmd, name, psname);
  remove, psname;
  return name;
}

local png_dpi;
local png_gray;
local png_smooth;
/* DOCUMENT png_dpi, png_gray, png_smooth
     You can set these variables to change the default values
     of the dpi=, gray=, and smooth= keywords for the png command
   SEE ALSO: png
 */

func png(name, dpi=, gray=, smooth=)
/* DOCUMENT png, name
     writes the picture in the current graphics window to the PNG
     file NAME+".png" (i.e.- the suffix .png is added to NAME).  The
     png file is intended to be imported into MS PowerPoint or other
     commercial presentation software.  This function starts ghostscript
     using the EPSGS_CMD variable.  With the gray=1 keyword, you get
     the pnggray ghostscript device, otherwise png16m.
     The default yorick graphics window is 6 inches square, and by
     default png produces 300 dpi (dot per inch) output.  You can change
     this with the dpi= keyword; dpi=72 is screen resolution.
     Finally, the smooth=1 keyword sets the TextAlphaBits and
     GraphicsAlphaBits postscript variables to 2; smooth=2 sets them
     to 4, which produce increasing levels of anti-aliasing.  With
     smooth=1 or smooth=2, you can probably get away with lower dpi.
     The default is smooth=0.  (Arguably, smooth=2 and dpi=72 or 100
     should be the defaults.  But this can cause hairline artifacts in
     plf or plfc output; see help,pdf.)
     The default values of the keywords can be changed by setting
     the corresponding extern variable png_dpi, png_gray, or png_smooth.
   SEE ALSO: pdf, svg, jpeg, eps, hcps, window, plg, no_window
 */
{
  if (strpart(name, -3:0) == ".png") name = strpart(name,1:-4);
  /* first run ghostscript to produce an eps translated to (0,0) */
  psname = eps(name+".png", pdf=1);
  /* second run ghostscript to produce the png */
  gscmd = EPSGS_CMD+" -sDEVICE=%s %s -sOutputFile=\"%s\"%s \"%s\"";
  dev = (gray || png_gray)? "pnggray" : "png16m";
  if (is_void(dpi)) dpi = png_dpi? png_dpi : 300;
  dpi = dpi? "-r"+print(dpi)(1) : "";
  if (is_void(smooth)) smooth = png_smooth;
  if (smooth) {
    smooth = (smooth==1)? 2 : 4;
    smooth = " "+swrite(format=GS_SMOOTH_OPT, smooth, smooth);
  } else {
    smooth = "";
  }
  system, swrite(format=gscmd, dev, dpi, name+".png", smooth, psname);
  remove, psname;
}

GS_SMOOTH_OPT = "-dTextAlphaBits=%ld -dGraphicsAlphaBits=%ld"

func jpeg(name, dpi=, gray=)
/* DOCUMENT jpeg, name
     writes the picture in the current graphics window to the JPEG
     file NAME+".jpg" (i.e.- the suffix .jpg is added to NAME).  The
     jpeg file is intended to be imported into MS PowerPoint or other
     commercial presentation software.  This function starts ghostscript
     using the EPSGS_CMD variable.  With the gray=1 keyword, you get
     the jpeggray ghostscript device, otherwise jpeg.
     The default yorick graphics window is 6 inches square, and by
     default jpeg produces 72 dpi (dot per inch) output.  You can change
     this with the dpi= keyword; dpi=300 is extremely high resolution.
   SEE ALSO: svg, png, pdf, eps, hcps, window, plg, no_window
 */
{
  if (strpart(name, -3:0) == ".jpg") name = strpart(name,1:-4);
  /* first run ghostscript to produce an eps translated to (0,0) */
  psname = eps(name+".jpg", pdf=1);
  /* second run ghostscript to produce the cropped pdf */
  gscmd = EPSGS_CMD+" -sDEVICE=%s %s -sOutputFile=\"%s\" \"%s\"";
  dev = gray? "jpeggray" : "jpeg";
  dpi = dpi? "-r"+print(dpi)(1) : "";
  system, swrite(format=gscmd, dev, dpi, name+".jpg", psname);
  remove, psname;
}

func svg(name)
/* DOCUMENT svg, name
     writes the picture in the current graphics window to the SVG
     file NAME+".svg" (i.e.- the suffix .svg is added to NAME).  The
     svg file can be inserted into html and interpreted by most Web
     browsers.  This function starts ghostscript using the EPSGS_CMD
     variable.

     You may have problems with hairline artifacts in plf or plfc output
     (they also show up occasionally in pdf or postscript output).
     This turns out to be caused by anti-aliasing (for pdf or ps as well);
     the files are correct, but modern rendering engines create the
     artifacts by trying to soften pixelated edges.

     In SVG, the solution is to set the shape-rendering property to
     shape-rendering='crispEdges' or shape-rendering='optimizeSpeed'
     which can be done either globally in the leading <svg> tag, or
     in each group <g> tag ghostscript uses to represent a filled polygon.
     I don't know how to make ghostscript do that for you.  Oddly
     enough, shapeRendering='geometricPrecision" apparently means
     anti-aliasing is turned on; the fuzzy edges causes the hairlines
     you see, since each polygon fuzzes its own edges without paying
     any attention to the adjacent polygon's color.
   SEE ALSO: png, pdf, jpeg, eps, hcps, window, plg, no_window
 */
{
  if (strpart(name, -3:0) == ".svg") name = strpart(name,1:-4);
  /* first run ghostscript to produce an eps translated to (0,0) */
  svgname = eps(name+".svg", pdf=1);
  /* second run ghostscript to produce the svg */
  gscmd = EPSGS_CMD+" -sDEVICE=%s %s -sOutputFile=\"%s\" \"%s\"";
  dev = "svg";
  opt = "";
  system, swrite(format=gscmd, dev, opt, name+".svg", svgname);
  remove, svgname;
}

extern fma;
/* DOCUMENT fma
     frame advance the current graphics window.  The current picture
     remains displayed in the associated X window until the next element
     is actually plotted.
   SEE ALSO: window, hcp, animate, plg, no_window
 */

extern hcp;
extern hcpon;
extern hcpoff;
/* DOCUMENT hcp
            hcpon
            hcpoff
     The hcp command sends the picture displayed in the current graphics
     window to the hardcopy file.  (The name of the default hardcopy file
     can be specified using hcp_file; each individual graphics window may
     have its own hardcopy file as specified by the window command.)
     The hcpon command causes every fma (frame advance) command to do
     and implicit hcp, so that every frame is sent to the hardcopy file.
     The hcpoff command reverts to the default "demand only" mode.
   SEE ALSO: window, fma, plg, pdf, eps, hcps, no_window
 */

extern redraw;
/* DOCUMENT redraw
     redraws the X window associated with the current graphics window.
   SEE ALSO: window, fma, hcp, plg
 */

extern palette;
/* DOCUMENT palette, filename
         or palette, source_window_number
         or palette, red, green, blue, ntsc=1/0
         or palette, red, green, blue, gray
         or palette, red, green, blue, query=1
         or palette, red, green, blue, gray, query=1
     sets (or retrieves with query=1) the palette for the current
     graphics window.

     The cmap function provides a higher level interface, including
     dozens more named palette choices.

     The FILENAME is the name of a Gist palette file; the standard
     palettes are "viridis.gp", "magma.gp", "inferno.gp", "plasma.gp",
     "coolwarm.gp", "earth.gp", "stern.gp", "rainbow.gp", "heat.gp",
     "gray.gp", and "yarg.gp".  Use the maxcolors keyword in the
     pldefault command to put an upper limit on the number of
     colors which will be read from the palette in FILENAME.

     In the second form, the palette for the current window is copied
     from the SOURCE_WINDOW_NUMBER.  If the X colormap for the window is
     private, there will still be two separate X colormaps for the two
     windows, but they will have the same color values.

     In the third form, RED, GREEN, and BLUE are 1-D arrays of the same
     length specifying the palette you wish to install; the values
     should vary between 0 and 255, and your palette should have no
     more than 240 colors.  If ntsc=0, monochrome devices (such as most
     laser printers) will use the average brightness to translate your
     colors into gray; otherwise, the NTSC (television) averaging will
     be used (.30*RED+.59*GREEN+.11*BLUE).  Alternatively, you can specify
     GRAY explicitly.

     Ordinarily, the palette is not dumped to a hardcopy file
     (color hardcopy is still rare and expensive), but you can
     force the palette to dump using the window or hcp_file commands.

     See the dump= keyword for the hcp_file and window commands if you
     are having trouble getting color in your hardcopy files.

   SEE ALSO: cmap, window, fma, hcp, pldefault, plg
 */

extern animate;
/* DOCUMENT animate
         or animate, 0/1
     without any arguments, toggles animation mode; with argument 0,
     turns off animation mode, with argument 1 turns on animation mode.
     In animation mode, the X window associated with a graphics window
     is actually an offscreen pixmap which is bit-blitted onscreen
     when an fma command is issued.  This is confusing unless you are
     actually trying to make a movie, but results in smoother animation
     if you are.  Generally, you should turn animation on, run your movie,
     then turn it off.
   SEE ALSO: window, fma, plg
 */

extern plsys;
/* DOCUMENT plsys, n
         or plsys(n)   or   plsys()
     sets the current coordinate system to number N in the current
     graphics window.  If N equals 0, subsequent elements will be
     plotted in absolute NDC coordinates outside of any coordinate
     system.  The default style sheet "work.gs" defines only a single
     coordinate system, so the only other choice is N equal 1.  You
     can make up your own style sheet (using a text editor) which
     defines mulitple coordinate systems.  You need to do this if
     you want to display four plots side by side on a single page,
     for example.  The standard style sheets "work2.gs" and "boxed2.gs"
     define two overlayed coordinate systems with the first labeled
     to the right of the plot and the second labeled to the left of
     the plot.  When using overlayed coordinate systems, it is your
     responsibility to ensure that the x-axis limits in the two
     systems are identical.
     Return value is coordinate system setting before this call;
     input n may be nil to retrieve this without changing it.  Return
     value can be <0 if the information is unavailable for some reason.
   SEE ALSO: window, limits, plg
 */

/*= SECTION(plotter) plotting functions ====================================*/

extern plg;
/* DOCUMENT plg, y, x
         or plg, y
     plots a graph of Y versus X.  Y and X must be 1-D arrays of equal
     length; if X is omitted, it defaults to [1, 2, ..., numberof(Y)].
     A keyword n=[n1,n2,n3,...nN] can be used to add N curves.  In this
     case, sum(n) must be numberof(y).
     The following keywords are legal (each has a separate help entry):
   KEYWORDS: legend, hide
             type, width, color, closed, smooth
             marks, marker, mspace, mphase
             rays, arrowl, arroww, rspace, rphase
   SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp, plmk
             limits, logxy, range, fma, hcp
 */

extern plm;
/* DOCUMENT plm, y, x, boundary=0/1, inhibit=0/1/2
         or plm, y, x, ireg, boundary=0/1, inhibit=0/1/2
         or plm, boundary=0/1, inhibit=0/1/2
     plots a mesh of Y versus X.  Y and X must be 2-D arrays with equal
     dimensions.  If present, IREG must be a 2-D region number array
     for the mesh, with the same dimensions as X and Y.  The values of
     IREG should be positive region numbers, and zero for zones which do
     not exist.  The first row and column of IREG never correspond to any
     zone, and should always be zero.  The default IREG is 1 everywhere
     else.  If present, the BOUNDARY keyword determines whether the
     entire mesh is to be plotted (boundary=0, the default), or just the
     boundary of the selected region (boundary=1).  If present, the
     INHIBIT keyword causes the (X(,j),Y(,j)) lines to not be plotted
     (inhibit=1), or the (X(i,),Y(i,)) lines to not be plotted (inhibit=2).
     By default (inhibit=0), mesh lines in both logical directions are
     plotted.
     The Y, X, and IREG arguments may all be omitted to default to the
     mesh set by the most recent plmesh call.
     The following keywords are legal (each has a separate help entry):
   KEYWORDS: legend, hide
             type, width, color
             region
   SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp, plmesh
             limits, logxy, range, fma, hcp
 */

extern plmesh;
/* DOCUMENT plmesh, y, x, ireg, triangle=tri_array
         or plmesh
     sets the default mesh for subsequent plm, plc, plv, and plf calls.
     In the second form, deletes the default mesh (until you do this,
     or switch to a new default mesh, the default mesh arrays persist and
     take up space in memory).  The Y, X, and IREG arrays should all be
     the same shape; Y and X will be converted to double, and IREG will
     be converted to int.  If IREG is omitted, it defaults to IREG(1,)=
     IREG(,1)= 0, IREG(2:,2:)=1; that is, region number 1 is the whole
     mesh.  The triangulation array TRI_ARRAY is used by plc; the
     correspondence between TRI_ARRAY indices and zone indices is the
     same as for IREG, and its default value is all zero.
     The IREG or TRI_ARRAY arguments may be supplied without Y and X
     to change the region numbering or triangulation for a given set of
     mesh coordinates.  However, a default Y and X must already have been
     defined if you do this.
     If Y is supplied, X must be supplied, and vice-versa.
   SEE ALSO: plm, plc, plv, plf, plfp
 */

extern plc;
/* DOCUMENT plc, z, y, x, levs=z_values
         or plc, z, y, x, ireg, levs=z_values
         or plc, z, levs=z_values
     plots a contours of Z on the mesh Y versus X.  Y, X, and IREG are
     as for plm.  The Z array must have the same shape as Y and X.
     The function being contoured takes the value Z at each point
     (X,Y) -- that is, the Z array is presumed to be point-centered.
     The Y, X, and IREG arguments may all be omitted to default to the
     mesh set by the most recent plmesh call.
     The LEVS keyword is a list of the values of Z at which you want
     contour curves.  The default is eight contours spanning the
     range of Z.
     See plfc if you want to color the regions between contours.
     The following keywords are legal (each has a separate help entry):
   KEYWORDS: legend, hide
             type, width, color, smooth
             marks, marker, mspace, mphase
             smooth, triangle, region
   SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp, plmesh, plfc
             contour, spann, limits, logxy, range, fma, hcp
 */

extern plv;
/* DOCUMENT plv, vy, vx, y, x, scale=dt
         or plv, vy, vx, y, x, ireg, scale=dt
         or plv, vy, vx, scale=dt
     plots a vector field (VX,VY) on the mesh (X,Y).  Y, X, and IREG are
     as for plm.  The VY and VX arrays must have the same shape as Y and X.
     The Y, X, and IREG arguments may all be omitted to default to the
     mesh set by the most recent plmesh call.
     The SCALE keyword is the conversion factor from the units of
     (VX,VY) to the units of (X,Y) -- a time interval if (VX,VY) is a velocity
     and (X,Y) is a position -- which determines the length of the
     vector "darts" plotted at the (X,Y) points.  If omitted, SCALE is
     chosen so that the longest ray arrows have a length comparable
     to a "typical" zone size.
     You can use the scalem keyword in pledit to make adjustments to the
     SCALE factor computed by default.
     The following keywords are legal (each has a separate help entry):
   KEYWORDS: legend, hide
             type, width, color, smooth
             marks, marker, mspace, mphase
             triangle, region
   SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp, plmesh, pledit,
             limits, logxy, range, fma, hcp
 */

extern plf;
/* DOCUMENT plf, z, y, x
         or plf, z, y, x, ireg
         or plf, z
     plots a filled mesh Y versus X.  Y, X, and IREG are as for plm.
     The Z array must have the same shape as Y and X, or one smaller
     in both dimensions.  If Z is of type char, it is used "as is",
     otherwise it is linearly scaled to fill the current palette, as
     with the bytscl function.
     (See the bytscl function for explanation of top, cmin, cmax.)
     The mesh is drawn with each zone in the color derived from the Z
     function and the current palette; thus Z is interpreted as a
     zone-centered array.

     As for pli and plfp, Z may also be a 3x(NX-1)x(NY-1) array
     of char giving the [r,g,b] components of each color.  See the
     color keyword for cautions about using this if you do not have
     a true color display.

     The Y, X, and IREG arguments may all be omitted to default to the
     mesh set by the most recent plmesh call.
     A solid edge can optionally be drawn around each zone by setting
     the EDGES keyword non-zero.  ECOLOR and EWIDTH determine the edge
     color and width.  The mesh is drawn zone by zone in order from
     IREG(2+imax) to IREG(jmax*imax) (the latter is IREG(imax,jmax)),
     so you can achieve 3D effects by arranging for this order to
     coincide with back-to-front order.  If Z is nil, the mesh zones
     are filled with the background color, which you can use to
     produce 3D wire frames.
     The following keywords are legal (each has a separate help entry):
   KEYWORDS: legend, hide
             region, top, cmin, cmax, edges, ecolor, ewidth
   SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp, plmesh,
             limits, logxy, range, fma, hcp, palette, bytscl, histeq_scale
 */

extern plfp;
/* DOCUMENT plfp, z, y, x, n
     plots a list of filled polygons Y versus X, with colors Z.
     The N array is a 1D list of lengths (number of corners) of the
     polygons; the 1D colors array Z has the same length as N.  The
     X and Y arrays have length sum(N).
     If Z is of type char, it is used "as is", otherwise it is linearly
     scaled to fill the current palette, as with the bytscl function.
     If Z is nil, the background color is used for every polygon.
     (See the bytscl function for explanation of top, cmin, cmax.)

     As for plf and pli, Z may also be a 3-by-numberof(N) array of
     char giving the [r,g,b] components of each color.  See the
     color keyword for cautions about using this if you do not have
     a true color display.

     As a special case, if n(2:)==1, the first polygon is assumed
     to have NDC coordinates, while the remaining individual X and Y
     values are in world coordinates.  The first polygon is drawn
     numberof(n)-1 times, with its (0,0) placed at each of the
     individual (X,Y) values in succession.  This is a hack to enable
     plotting of more elaborate data markers than plg,type=0 -- see
     the plmk function for details.

     The following keywords are legal (each has a separate help entry):
   KEYWORDS: legend, hide, top, cmin, cmax, edges, ecolor, ewidth
   SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfc
             limits, logxy, range, fma, hcp
 */

extern pli;
/* DOCUMENT pli, z
         or pli, z, x1, y1
         or pli, z, x0, y0, x1, y1
     plots the image Z as a cell array -- an array of equal rectangular
     cells colored according to the 2-D array Z.  The first dimension
     of Z is plotted along x, the second dimension is along y.
     If Z is of type char, it is used "as is", otherwise it is linearly
     scaled to fill the current palette, as with the bytscl function.
     (See the bytscl function for explanation of top, cmin, cmax.)

     As for plf and plfp, Z may also be a 3D array with 1st dimension 3
     of char giving the [r,g,b] components of each color.  See the
     color keyword for cautions about using this if you do not have
     a true color display.

     If X1 and Y1 are given, they represent the coordinates of the
     upper right corner of the image.  If X0, and Y0 are given, they
     represent the coordinates of the lower left corner, which is at
     (0,0) by default.  If only the Z array is given, each cell will be
     a 1x1 unit square, with the lower left corner of the image at (0,0).
     The following keywords are legal (each has a separate help entry):
   KEYWORDS: legend, hide, top, cmin, cmax
   SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp,
             limits, logxy, range, fma, hcp, palette, bytscl, histeq_scale
 */

extern pldj;
/* DOCUMENT pldj, x0, y0, x1, y1
     plots disjoint lines from (X0,Y0) to (X1,Y1).  X0, Y0, X1, and Y1
     may have any dimensionality, but all must have the same number of
     elements.
     The following keywords are legal (each has a separate help entry):
   KEYWORDS: legend, hide
             type, width, color
   SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp
             limits, logxy, range, fma, hcp
 */

extern plt;
/* DOCUMENT plt, text, x, y, tosys=0/1
     plots TEXT (a string) at the point (X,Y).  The exact relationship
     between the point (X,Y) and the TEXT is determined by the
     justify keyword.  TEXT may contain newline ("\n") characters
     to output multiple lines of text with a single call.  The
     coordinates (X,Y) are NDC coordinates (outside of any coordinate
     system) unless the tosys keyword is present and non-zero, in
     which case the TEXT will be placed in the current coordinate
     system.  However, the character height is NEVER affected by the
     scale of the coordinate system to which the text belongs.
     Note that the pledit command takes dx and/or dy keywords to
     adjust the position of existing text elements.

     The characters ^, _, and ! are treated specially in TEXT.
     ^ begins a superscript, _ begins a subscript, and ! causes the
     following character to be rendered using the symbol font.  As
     special cases, !^, !_, and !! render the ^, _, and ! characters
     themselves.  However, if ! is the final character of TEXT
     (or immediately before a newline in multiline text), it
     loses its special meaning.  TEXT has just three modes: ordinary,
     superscript, and subscript.  A ^ character enters superscript
     mode from ordinary or subscript mode, and returns to ordinary
     mode from superscript mode.  A _ enters subscript mode, except
     from subscript mode it returns to ordinary mode.  For example,
     Euclid said, "!pr^2", and Einstein said, "G_!s!n_=8!pT_!s!n".
     One final special escape: !] produces the ^ character in the
     symbol font (it is a perpendicular sign, whereas ] is just ]).

     The following keywords are legal (each has a separate help entry):
   KEYWORDS: legend, hide
             color, font, height, opaque, orient, justify
   SEE ALSO: plt1, plg, plm, plc, plv, plf, pli, plt, pldj, plfp, pledit
             limits, range, fma, hcp, pltitle
 */

func plt1(text, x, y, tosys=, color=,font=,height=,opaque=,orient=,justify=)
/* DOCUMENT plt1, text, x, y
     same as plt, but TEXT, X, and Y may be arrays to plot multiple
     strings.  The tosys= keyword works as for plt.
   KEYWORDS: color, font, height, opaque, orient, justify
   SEE ALSO: plt
 */
{
  n= array(0.,dimsof(text,x,y));
  x+= n;
  y+= n;
  text+= array(string,dimsof(n));
  n= numberof(n);
  for (i=1 ; i<=n ; ++i)
    plt,text(i),x(i),y(i),tosys=tosys,color=color,font=font,height=height,
      opaque=opaque,orient=orient,justify=justify;
}

func pltitle(title)
/* DOCUMENT pltitle, title
     Plot TITLE centered above the coordinate system for any of the
     standard Gist styles.  You may want to customize this for other
     plot styles.
     The external variables pltitle_height, pltitle_font, pltitle_xadj,
     and pltitle_yadj determine the font and position of the title,
     if you want to change those.
   SEE ALSO: plt, xytitles
 */
{
  port = viewport();
  x = port(zcen:1:2)(1) + pltitle_xadj;
  y = port(4) + pltitle_yadj;
  plt, title, x, y, font=pltitle_font, justify="CB", height=pltitle_height;
}

func xytitles(xtitle, ytitle, adjust)
/* DOCUMENT xytitles, xtitle, ytitle
         or xytitles, xtitle, ytitle, [deltax,deltay]
     Plot XTITLE horizontally under the viewport and YTITLE vertically
     to the left of the viewport.  If the tick numbers interfere with
     the labels, you can specify the [DELTAX,DELTAY] in NDC units to
     displace the labels.  (Especially for the y title, the adjustment
     may depend on how many digits the numbers on your scale actually
     have.)  Note that DELTAX moves YTITLE and DELTAY moves XTITLE.
     The external variables pltitle_height, pltitle_font, xtitle_xadj,
     xtitle_yadj, ytitle_xadj, and ytitle_yadj determine the font and
     unadjusted position of the titles, if you want to change those.
     WARNING: There is no easy way to ensure that this type of title
              will not interfere with the tick numbering.  Interference
              may make the numbers or the title or both illegible.
   SEE ALSO: plt, pltitle
 */
{
  if (is_void(adjust)) adjust = [0., 0.];
  port = viewport();
  x = port(zcen:1:2)(1) + xtitle_xadj;
  y = port(3) + xtitle_yadj + adjust(2);
  if (xtitle && strlen(xtitle))
    plt, xtitle, x, y, font=pltitle_font, justify="CT", height=pltitle_height;
  x = port(1) + ytitle_xadj + adjust(1);
  y = port(zcen:3:4)(1) + ytitle_yadj;
  if (ytitle && strlen(ytitle))
    plt, ytitle, x, y,
      font=pltitle_font, justify="CB", height=pltitle_height, orient=1;
}

pltitle_height= 18;
pltitle_font= "helvetica";
/* default title center locations relative to midpoint of viewport edge */
pltitle_xadj = 0.;
pltitle_yadj = 0.02;
xtitle_xadj = 0.;
xtitle_yadj = -0.05;
ytitle_xadj = -0.05;
ytitle_yadj = 0.;

/*= SECTION(plotlim) plot limits and axis scaling ==========================*/

e= "e";         /* for use with limits and range functions */

extern limits;
/* DOCUMENT limits
         or limits, xmin, xmax, ymin, ymax,
                    square=0/1, nice=0/1, restrict=0/1
         or old_limits= limits()
         or limits, old_limits

     In the first form, restores all four plot limits to extreme values.

     In the second form, sets the plot limits in the current coordinate
     system to XMIN, XMAX, YMIN, YMAX, which may be nil or omitted to
     leave the corresponding limit unchanged, a number to fix the
     corresponding limit to a specified value, or the string "e" to
     make the corresponding limit take on the extreme value of the
     currently displayed data.

     If present, the square keyword determines whether limits marked
     as extreme values will be adjusted to force the x and y scales
     to be equal (square=1) or not (square=0, the default).
     If present, the nice keyword determines whether limits will be
     adjusted to nice values (nice=1) or not (nice=0, the default).
     There is a subtlety in the meaning of "extreme value" when one
     or both of the limits on the OPPOSITE axis have fixed values --
     does the "extreme value" of the data include points which
     will not be plotted because their other coordinate lies outside
     the fixed limit on the opposite axis (restrict=0, the default),
     or not (restrict=1)?

     If called as a function, limits returns an array of 5 doubles;
     OLD_LIMITS(1:4) are the current xmin, xmax, ymin, and ymax,
     and int(OLD_LIMITS(5)) is a set of flags indicating extreme
     values and the square, nice, restrict, and log flags.

     In the fourth form, OLD_LIMITS is as returned by a previous
     limits call, to restore the limits to a previous state.

     In an X window, the limits may also be adjusted interactively
     with the mouse.  Drag left to zoom in and pan (click left to zoom
     in on a point without moving it), drag middle to pan, and click
     (and drag) right to zoom out (and pan).  If you click just above
     or below the plot, these operations will be restricted to the
     x-axis; if you click just to the left or right, the operations
     are restricted to the y-axis.  A ctrl-left click, drag, and
     release will expand the box you dragged over to fill the plot
     (other popular software zooms with this paradigm).  If the
     rubber band box is not visible with ctrl-left zooming, try
     ctrl-middle or ctrl-right for alternate XOR masks.  Such
     mouse-set limits are equivalent to a limits command specifying
     all four limits EXCEPT that the unzoom command can revert to
     the limits before a series of mouse zooms and pans.

     Holding the shift key and pressing the left mouse button is
     equivalent to pressing the middle mouse button.  Similarly,
     pressing meta-left is equivalent to the right button.  This
     permits access to the middle and right button functions on
     machines (e.g.- most laptops) with two button or one button
     mice.

     The limits you set using the limits or range functions carry over
     to the next plot -- that is, an fma operation does NOT reset the
     limits to extreme values.

   SEE ALSO: plsys, range, logxy, zoom_factor, unzoom, plg, viewport
 */

func range(ymin, ymax) { limits,,, ymin, ymax; }
/* DOCUMENT range, ymin, ymax
     sets the y-axis plot limits in the current coordinate system to
     YMIN, YMAX, which may be nil or omitted to leave the corresponding
     limit unchanged, a number to fix the corresponding limit to a
     specified value, or the string "e" to make the corresponding limit
     take on the extreme value of the currently displayed data.
     Use    limits, xmin, xmin
     to accomplish the same function for the x-axis plot limits.
   SEE ALSO: plsys, limits, logxy, plg
 */

extern logxy;
/* DOCUMENT logxy, xflag, yflag
     sets the linear/log axis scaling flags for the current coordinate
     system.  XFLAG and YFLAG may be nil or omitted to leave the
     corresponding axis scaling unchanged, 0 to select linear scaling,
     or 1 to select log scaling.
   SEE ALSO: plsys, limits, range, plg, gridxy
 */

extern gridxy;
/* DOCUMENT gridxy, flag
         or gridxy, xflag, yflag
     Turns on or off grid lines according to FLAG.  In the first form, both
     the x and y axes are affected.  In the second form, XFLAG and YFLAG
     may differ to have different grid options for the two axes.  In either
     case, a FLAG value of 0 means no grid lines (the default), a value of
     1 means grid lines at all major ticks (the level of ticks which get
     grid lines can be set in the style sheet), and a FLAG value of 2 means
     that the coordinate origin only will get a grid line.  In styles with
     multiple coordinate systems, only the current coordinate system is
     affected.
     The keywords can be used to affect the style of the grid lines.

     You can also turn the ticks off entirely.  (You might want to do this
     to plot your own custom set of tick marks when the automatic tick
     generating machinery will never give the ticks you want.  For example
     a latitude axis in degrees might reasonably be labeled "0, 30, 60,
     90", but the automatic machinery considers 3 an "ugly" number - only
     1, 2, and 5 are "pretty" - and cannot make the required scale.  In
     this case, you can turn off the automatic ticks and labels, and use
     plsys, pldj, and plt to generate your own.)
     To fiddle with the tick flags in this general manner, set the
     0x200 bit of FLAG (or XFLAG or YFLAG), and "or-in" the 0x1ff bits
     however you wish.  The meaning of the various flags is described
     in the file Y_SITE/gist/work.gs.  Additionally, you can use the
     0x400 bit to turn on or off the frame drawn around the viewport.
     Here are some examples:
        gridxy,0x233        work.gs default setting
        gridxy,,0x200       like work.gs, but no y-axis ticks or labels
        gridxy,,0x231       like work.gs, but no y-axis ticks on right
        gridxy,0x62b        boxed.gs default setting

     The three keywords base60=, degrees=, and hhmm= can be used to get
     alternative tick intervals for base 60 systems instead of the
     usual base 10 systems.  The keyword values are 0 to restore the
     default behavior, 1 to set the feature for the x axis, 2 to set it
     for the y axis, and 3 to set it for both axes.  The base60 feature
     allows ticks and labels at multiples of 30 (up to +-3600).  The
     degrees feature causes labels to be printed modulo 360 (so that a
     scale which runs from, say, 90 to 270 will be printed as 90 to 180
     then -180 to -90, mostly for longitude scales).  The hhmm feature
     causes labels to be printed in the form hh:mm (so that, for example,
     150 will be printed as 02:30, mostly for time of day scales).

   KEYWORDS: color, type, width, base60, degrees, hhmm
   SEE ALSO: window, plsys, limits, range, logxy, viewport
 */

extern zoom_factor;
/* DOCUMENT zoom_factor, factor
     sets the zoom factor for mouse-click zoom in and zoom out operations.
     The default FACTOR is 1.5; FACTOR should always be greater than 1.0.
   SEE ALSO: limits, range, unzoom, plg
 */

extern unzoom;
/* DOCUMENT unzoom
     restores limits to their values before zoom and pan operations
     performed interactively using the mouse.
     Use    old_limits=  limits()
            ...
            limits, old_limits
     to save and restore plot limits generally.
   SEE ALSO: limits, range, zoom_factor, plg
 */

/*= SECTION(plotkey) keywords for plotting functions =======================*/

local legend;
/* DOCUMENT legend=   plotting keyword
     sets the legend for a plot.  The default legend is a concatentation
     of the strings used in the original plotting command (plg, plm, etc.),
     except for the plt command, which has no default legend.
     Legends are never plotted to the X window; use the plq command to
     see them interactively.  Legends will appear in hardcopy output
     unless they have been explicitly turned off.
   PLOTTING COMMANDS: plg, plm, plc, plv, plf, pli, plt, pldj
   SEE ALSO: hide
 */

local hide;
/* DOCUMENT hide=   plotting keyword
     sets the visibility of a plotted element.  The default is hide=0,
     which means that the element will be visible.  Use hide=1 to remove
     the element from the plot (but not from the display list).
   PLOTTING COMMANDS: plg, plm, plc, plv, plf, pli, plt, pldj
   SEE ALSO: legend
 */

local type;
/* DOCUMENT type=   plotting keyword
     selects line type.  Valid values are the strings "solid", "dash",
     "dot", "dashdot", "dashdotdot", and "none".  The "none" value
     causes the line to be plotted as a polymarker.  You should also
     check the plmk function if you need polymarkers.
     The type value may also be a number; 0 is "none", 1 is "solid",
     2 is "dash", 3 is "dot", 4 is "dashdot", and 5 is "dashdotdot".
   PLOTTING COMMANDS: plg, plm, plc, pldj
   SEE ALSO: width, color, marks, marker, rays, closed, smooth, plmk
 */

local width;
/* DOCUMENT width=   plotting keyword
     selects line width.  Valid values are positive floating point numbers
     giving the line thickness relative to the default line width of one
     half point, width= 1.0.
   PLOTTING COMMANDS: plg, plm, plc, pldj, plv (only if hollow=1)
   SEE ALSO: type, color, marks, marker, rays, closed, smooth
 */

local color;
/* DOCUMENT color=   plotting keyword
     selects line or text color.  Valid values are the strings "bg", "fg",
     "black", "white", "red", "green", "blue", "cyan", "magenta", "yellow",
     "grayd", "grayc", "grayb", graya", (grayd darkest, graya lightest)
     or a 0-origin index into the current palette.  The default is "fg".
     Negative numbers may be used instead of the strings: -1 is bg
     (background), -2 is fg (foreground), -3 is black, -4 is white,
     -5 is red, -6 is green, -7 is blue, -8 is cyan, -9 is magenta,
     -10 is yellow, -11 is grayd, -12 is grayc, -13 is grayb, and -14 is
     graya.  (The negative numbers are actually taken modulo
     256, so -1 is also 255, -2 is 254, and so on.)

     A color can also be a triple [r, g, b], with values running from
     0 for dark to 255 for full intensity.  Beware, however, of
     specifying an rgb color (either as a color keyword or to the
     plf, pli, or plfp commands) if your display is not a true color
     display (for example, if it is 8 bits deep or less).  In that
     case, it may switch to a 5x9x5 color cube, which causes a
     significant degradation in quality of rendering with smooth
     color palettes.  Furthermore, the hcp command will not work
     properly for rgb colors if the file is a CGM.  Use the rgb=1
     keyword in the window command to avoid having to re-issue a
     palette command after the first rgb object is drawn (this is
     unnecessary on true color screens).

     You can use color=torgb(name) to specify a W3C/X11 color.

   PLOTTING COMMANDS: plg, plm, plc, pldj, plt
   SEE ALSO: type, width, marks, marker, mcolor, rays, closed, smooth, torgb
 */

local marks;
/* DOCUMENT marks=   plotting keyword
     selects unadorned lines (marks=0), or lines with occasional markers
     (marks=1).  Ignored if type is "none" (indicating polymarkers instead
     of occasional markers).  The spacing and phase of the occasional
     markers can be altered using the mspace and mphase keywords; the
     character used to make the mark can be altered using the marker
     keyword.
   PLOTTING COMMANDS: plg, plc
   SEE ALSO: type, width, color, marker, rays, mspace, mphase, msize, mcolor
 */

local marker;
/* DOCUMENT marker=   plotting keyword
     selects the character used for occasional markers along a polyline,
     or for the polymarker if type is "none".  The special values
     '\1', '\2', '\3', '\4', and '\5' stand for point, plus, asterisk,
     circle, and cross, which are prettier than text characters on output
     to some devices.  The default marker is the next available capital
     letter, 'A', 'B', ..., 'Z'.
   PLOTTING COMMANDS: plg, plc
   SEE ALSO: type, width, color, marks, rays, mspace, mphase, msize, mcolor
 */

local mspace, mphase, msize, mcolor;
/* DOCUMENT mspace=   plotting keyword
         or mphase=   plotting keyword
         or msize=    plotting keyword
         or mcolor=   plotting keyword
     selects the spacing, phase, and size of occasional markers placed
     along polylines.  The msize also selects polymarker size if type
     is "none".  The spacing and phase are in NDC units (0.0013 NDC
     equals 1.0 point); the default mspace is 0.16, and the default
     mphase is 0.14, but mphase is automatically incremented for
     successive curves on a single plot.  The msize is in relative
     units, with the default msize of 1.0 representing 10 points.
     The mcolor keyword is the same as the color keyword, but controls
     the marker color instead of the line color.  Setting the color
     automatically sets the mcolor to the same value, so you only
     need to use mcolor if you want the markers for a curve to be a
     different color than the curve itself.
   PLOTTING COMMANDS: plg, plc
   SEE ALSO: type, width, color, marks, marker, rays
 */

local rays;
/* DOCUMENT rays=   plotting keyword
     selects unadorned lines (rays=0), or lines with occasional ray
     arrows (rays=1).  Ignored if type is "none".  The spacing and phase
     of the occasional arrows can be altered using the rspace and rphase
     keywords; the shape of the arrowhead can be modified using the
     arroww and arrowl keywords.
   PLOTTING COMMANDS: plg, plc
   SEE ALSO: type, width, color, marker, marks, rspace, rphase
             arroww, arrowl
 */

local rspace, rphase, arroww, arrowl;
/* DOCUMENT rspace=   plotting keyword
         or rphase=   plotting keyword
         or arroww=   plotting keyword
         or arrowl=   plotting keyword
     selects the spacing, phase, and size of occasional ray arrows
     placed along polylines.  The spacing and phase are in NDC units
     (0.0013 NDC equals 1.0 point); the default rspace is 0.13, and
     the default rphase is 0.11375, but rphase is automatically
     incremented for successive curves on a single plot.
     The arrowhead width, arroww, and arrowhead length, arrowl are
     in relative units, defaulting to 1.0, which translates to an
     arrowhead 10 points long and 4 points in half-width.
   PLOTTING COMMANDS: plg
   SEE ALSO: type, width, color, marks, marker, rays
 */

local closed, smooth;
/* DOCUMENT closed=   plotting keyword
         or smooth=   plotting keyword
     selects closed curves (closed=1) or default open curves (closed=0),
     or Bezier smoothing (smooth>0) or default piecewise linear curves
     (smooth=0).  The value of smooth can be 1, 2, 3, or 4 to get
     successively more smoothing.  Only the Bezier control points are
     plotted to an X window; the actual Bezier curves will show up in
     PostScript hardcopy files.  Closed curves join correctly, which
     becomes more noticeable for wide lines; non-solid closed curves
     may look bad because the dashing pattern may be incommensurate
     with the length of the curve.
   PLOTTING COMMANDS: plg, plc (smooth only)
   SEE ALSO: type, width, color, marks, marker, rays
 */

local font, height, opaque, orient, justify;
/* DOCUMENT font=     plotting keyword
         or height=   plotting keyword
         or opaque=   plotting keyword
         or orient=   plotting keyword
         or justify=  plotting keyword
     selects text properties.  The font can be any of the strings
     "courier", "times", "helvetica" (the default), "symbol", or
     "schoolbook".  Append "B" for boldface and "I" for italic, so
     "courierB" is boldface Courier, "timesI" is Times italic, and
     "helveticaBI" is bold italic (oblique) Helvetica.  Your X server
     should have the Adobe fonts (available free from the MIT X
     distribution tapes) for all these fonts, preferably at both 75
     and 100 dpi.  Occasionally, a PostScript printer will not be
     equipped for some fonts; often New Century Schoolbook is missing.
     The font keyword may also be an integer: 0 is Courier, 4 is Times,
     8 is Helvetica, 12 is Symbol, 16 is New Century Schoolbook, and
     you add 1 to get boldface and/or 2 to get italic (or oblique).

     The height is the font size in points; 14.0 is the default.
     X windows only has 8, 10, 12, 14, 18, and 24 point fonts, so
     don't stray from these sizes if you want what you see on the
     screen to be a reasonably close match to what will be printed.

     By default, opaque=0 and text is transparent.  Set opaque=1 to
     white-out a box before drawing the text.  The default orient
     (orient=0) is left-to-right text; set orient=1 for text rotated 90
     degrees so it reads upward, orient=2 for 180 degree rotation so
     it is upside down, and orient=3 for 270 degree rotation so it
     reads downward.

     The default text justification, justify="NN" is normal is both
     the horizontal and vertical directions.  Other possibilities
     are "L", "C", or "R" for the first character, meaning left,
     center, and right horizontal justification, and "T", "C", "H",
     "A", or "B", meaning top, capline, half, baseline, and bottom
     vertical justification.  The normal justification "NN" is equivalent
     to "LA".  Common values are "LA", "CA", and "RA" for garden variety
     left, center, and right justified text, with the y coordinate at the
     baseline of the last line in the string presented to plt.  The
     characters labeling the right axis of a plot are "RH", so that the
     y value of the text will match the y value of the corresponding
     tick.  Similarly, the characters labeling the bottom axis of a plot
     are "CT".  The justify= may also be a number, horizontal+vertical,
     where horizontal is 0 for "N", 1 for "L", 2 for "C", or 3 for "R",
     and vertical is 0 for "N", 4 for "T", 8 for "C", 12 for "H",
     16 for "A", or 20 for "B".

   PLOTTING COMMANDS: plt
   SEE ALSO: color
 */

local region;
/* DOCUMENT region=   plotting keyword
     selects the part of mesh to consider.  The region should match one
     of the numbers in the IREG array.  Putting region=0 (the default)
     means to plot the entire mesh, that is, everything EXCEPT region
     zero (non-existent zones).  Any other number means to plot only
     the specified region number; region=3 would plot region 3 only.
   PLOTTING COMMANDS: plm, plc, plv, plf
 */

local triangle;
/* DOCUMENT triangle=   plotting keyword
     sets the triangulation array for a contour plot.  The triangulation
     array must be the same shape as the IREG (region number) array, and
     the correspondence between mesh zones and indices is the same as
     for IREG.  The triangulation array is used to resolve the ambiguity
     in saddle zones, in which the function Z being contoured has two
     diagonally opposite corners high, and the other two corners low.
     The triangulation array element for a zone is 0 if the algorithm is
     to choose a triangulation, based on the curvature of the first
     contour to enter the zone.  If zone (i,j) is to be triangulated
     from point (i-1,j-1) to point (i,j), then TRIANGLE(i,j)=1,
     while if it is to be triangulated from (i-1,j) to (i,j-1), then
     TRIANGLE(i,j)=-1.  Contours will never cross this "triangulation
     line".
     You should rarely need to fiddle with the traingulation array;
     it is a hedge for dealing with pathological cases.
   PLOTTING COMMANDS: plc
 */

local hollow, aspect;
/* DOCUMENT hollow=   plotting keyword
         or aspect=   plotting keyword
     set the appearance of the "darts" of a vector field plot.  The
     default darts, hollow=0, are filled; use hollow=1 to get just the
     dart outlines.  The default is aspect=0.125; aspect is the ratio
     of the half-width to the length of the darts.  Use the color
     keyword to control the color of the darts.
   PLOTTING COMMANDS: plv
   SEE ALSO: color
 */

local edges, ecolor, ewidth;
/* DOCUMENT edges=   plotting keyword
         or ecolor=   plotting keyword
         or ewidth=   plotting keyword
     set the appearance of zone edges in a filled mesh plot (plf or plfp).
     By default, edges=0, and the zone edges are not plotted.  If
     edges=1, a solid line is drawn around each zone after it is
     filled; the edge color and width are given by ecolor and ewidth,
     which are "fg" and 1.0 by default.
   PLOTTING COMMANDS: plf
   SEE ALSO: color, width
 */

/*= SECTION(plotq) plot query and edit functions ===========================*/

extern plq;
/* DOCUMENT plq
         or plq, n_element
         or plq, n_element, n_contour
         or legend_list= plq()
         or properties= plq(n_element, n_contour)
     Called as a subroutine, prints the list of legends for the current
     coordinate system (with an "(H)" to mark hidden elements), or prints
     a list of current properties of element N_ELEMENT (such as line type,
     width, font, etc.), or of contour number N_CONTOUR of element number
     N_ELEMENT (which must be contours generated using the plc command).
     Called as a function, returns either the list of legend strings, or a
     list of pointers to the values of the various element properties.
     Elements and contours are both numbered starting with one; hidden
     elements or contours are included in this numbering.

     The PROPERTIES list returned by plq is a list of pointers to the
     relevent properties for the specified graphical element.  Each
     possible property has a particular index in the returned PROPERTIES
     list as follows:
     *PROPERTIES(1)   int([element type (0 for none, 1 for plg, 2 for pldj,
                                         3 for plt, 4 for plm, 5 for plf,
                                         6 for plv, 7 for plc, 8 for pli,
                                         9 for plfp),
                           hide flag])
     *PROPERTIES(2)   string(legend)
     *PROPERTIES(3)   int array, depends on type (names match keywords):
       1 plg:  [color, type, marks, mcolor, marker, rays, closed, smooth]
       2 pldj: [color, type]
       3 plt:  [color, font, orient, justify, opaque]
       4 plm:  [color, type, region, boundary, inhibit]
       5 plf:  [region, edges, ecolor, rgb_flag]
       6 plv:  [region, color, hollow]
       7 plc:  [region, color, type, marks, mcolor, marker, smooth]
       8 pli:  [rgb_flag]
       9 plfp: [edges, ecolor, rgb_flag]
     *PROPERTIES(4)   double array, depends on type (names match keywords):
       1 plg:  [width, msize, mspace, mphase, rspace, rphase, arrowl, arroww]
       2 pldj: [width]
       3 plt:  [height, x, y]
       4 plm:  [width]
       5 plf:  [ewidth]
       6 plv:  [width, aspect, scale]
       7 plc:  [width, msize, mspace, mphase]
       8 pli:  [x0, y0, x1, y1]
       9 plfp: [ewidth]
     *PROPERTIES(5)   long array, depends on type (names match arguments):
       1 plg:  [npoints, &x, &y]
       2 pldj: [npoints, &x0, &y0, &x1, &y1]
       3 plt:  [nchars, &text]
       4 plm:  [imax, jmax, &x, &y, &ireg]
       5 plf:  [imax, jmax, &x, &y, &ireg, &colors]
       6 plv:  [imax, jmax, &x, &y, &ireg, &vx, &vy]
       7 plc:  [imax, jmax, &x, &y, &ireg, &z, &triangle, nlevs, &levs]
       8 pli:  [imax, jmax, &colors]
       9 plfp: [n, &x, &y, &colors, &pn]
     You can use the reshape function to peek at the data at the addresses
     returned in PROPERTIES(5) as longs.  The appropriate data types are:
     char for text, int for ireg, short for triangle, char for colors, and
     double for everything else.  In a plf, colors is (imax-1)-by-(jmax-1).
     Although PROPERTIES(5) returns pointers to the data plotted, attempting
     to poke new values into this data will not produce immediate changes
     to your plot, since the graphics package does not realize that anything
     has changed.  Use pledit to make changes to plotted elements.

     The plq function always operates on the current coordinate system
     in the current graphics window; use window and plsys to change these.
   SEE ALSO: window, plsys, pledit, pldefault, plg
 */

extern pledit;
/* DOCUMENT pledit, key1=value1, key2=value2, ...
         or pledit, n_element, key1=value1, key2=value2, ...
         or pledit, n_element, n_contour, key1=value1, key2=value2, ...
     changes some property of element number N_ELEMENT (and contour
     number N_CONTOUR of that element).  If N_ELEMENT and N_CONTOUR are
     omitted, the default is the most recently added element, or the
     element specified in the most recent plq query command.

     The keywords can be any of the keywords that apply to the current
     element.  These are:
       plg:  color, type, width,
             marks, mcolor, marker, msize, mspace, mphase,
             rays, rspace, rphase, arrowl, arroww,
             closed, smooth
       pldj: color, type, width
       plt:  color, font, height, orient, justify, opaque
       plm:  region, boundary, inhibit, color, type, width
       plf:  region
       plv:  region, color, hollow, width, aspect, scale
       plc:  region, color, type, width,
             marks, mcolor, marker, msize, mspace, mphase
             smooth, levs
     (For contours, if you aren't talking about a particular N_CONTOUR,
      any changes will affect ALL the contours.)

     A plv (vector field) element can also take the scalem
     keyword to multiply all vector lengths by a specified factor.

     A plt (text) element can also take the dx and/or dy
     keywords to adjust the text position by (dx,dy).

   SEE ALSO: window, plsys, plq, pldefault, plg
 */

extern pldefault;
/* DOCUMENT pldefault, key1=value1, key2=value2, ...
     sets default values for the various properties of graphical elements.

     The keywords can be most of the keywords that can be passed to the
     plotting commands:
       plg:  color, type, width,
             marks, mcolor, msize, mspace, mphase,
             rays, rspace, rphase, arrowl, arroww
       pldj: color, type, width
       plt:  color, font, height, orient, justify, opaque
       plm:  color, type, width
       plv:  color, hollow, width, aspect
       plc:  color, type, width,
             marks, mcolor, marker, msize, mspace, mphase
       plf:  edges, ecolor, ewidth

     The initial default values are:
       color="fg", type="solid", width=1.0 (1/2 point),
       marks=1, mcolor="fg", msize=1.0 (10 points),
          mspace=0.16, mphase=0.14,
       rays=0, arrowl=1.0 (10 points), arroww=1.0 (4 points),
          rspace=0.13, rphase=0.11375,
       font="helvetica", height=12.0, justify="NN", opaque=0,
       hollow= 0, aspect=0.125,
       edges=0, ecolor="fg", ewidth=1.0 (1/2 point)

     Additional default keywords are:
       dpi, style, legends  (see window command)
       palette              (to set default filename as in palette command)
       maxcolors            (default 200)

   SEE ALSO: window, plsys, plq, pledit, plg
 */

/*= SECTION(plotmisc) miscellaneous plotting-related functions =============*/

extern bytscl;
/* DOCUMENT bytscl(z)
         or bytscl(z, top=max_byte, cmin=lower_cutoff, cmax=upper_cutoff)
     returns a char array of the same shape as Z, with values linearly
     scaled to the range 0 to one less than the current palette size.
     If MAX_BYTE is specified, the scaled values will run from 0 to
     MAX_BYTE instead.
     If LOWER_CUTOFF and/or UPPER_CUTOFF are specified, Z values outside
     this range are mapped to the cutoff value; otherwise the linear
     scaling maps the extreme values of Z to 0 and MAX_BYTE.
   SEE ALSO: plf, pli, histeq_scale
 */

extern mesh_loc;
/* DOCUMENT mesh_loc(y0, x0)
         or mesh_loc(y0, x0, y, x)
         or mesh_loc(y0, x0, y, x, ireg)
     returns the zone index (=i+imax*(j-1)) of the zone of the mesh
     (X,Y) (with optional region number array IREG) containing the
     point (X0,Y0).  If (X0,Y0) lies outside the mesh, returns 0.
     Thus, eg- ireg(mesh_loc(x0, y0, y, x, ireg)) is the region number of
     the region containing (x0,y0).  If no mesh specified, uses default.
     X0 and Y0 may be arrays as long as they are conformable.

     For mesh_loc wrappers to duplicate the functionality of the
     digitize and interp functions in 2D, see the library file digit2.i.
     Read help,digit2.

   SEE ALSO: plmesh, moush, mouse
 */

extern contour;
/* DOCUMENT nc= contour(yc,xc, level, z, y,x)
         or nc= contour(yc,xc, level, z, y,x,ireg)

     returns the points on the contour curve that would have been
     plotted by plc.  Z, Y, X, and IREG are as for plc, and the
     triangle= and region= keywords are accepted and have the same
     meaning as for plc.  Unlike plc, the triangle array is an output
     as well as an input to contour; if supplied it may be modified
     to reflect any triangulations which were performed by contour.

     LEVEL is a scalar z value to return the points at that contour
     level.  All such points lie on edges of the mesh.  If a contour
     curve closes, the final point is the same as the initial point
     (i.e.- that point is included twice in the returned list).

     LEVEL is a pair of z values [z0,z1] to return the points of
     a set of polygons which outline the regions between the two
     contour levels.  These will include points on the mesh boundary
     which lie between the levels, in addition to the edge points
     for both levels.  The polygons are closed, simply connected,
     and will not contain more than about 4000 points (larger polygons
     are split into pieces with a few points repeated where the pieces
     join).

     YC and XC are the output points on the curve(s), or nil if there
     are no points.  On input, they must be simple variable references,
     not expressions.  The return value NC is a list of the lengths of
     the polygons/polylines returned in (XC,YC), or nil if there are
     none.  numberof(XC)==numberof(YC)==sum(NC).  For the level pair
     case, YC, XC, and NC are ready to be used as inputs to plfp.

   KEYWORDS: triangle, region
   SEE ALSO: plc, plfp
 */

extern mouse;
/* DOCUMENT result= mouse(system, style, prompt)
     displays a PROMPT, then waits for a mouse button to be pressed,
     then released.  Returns array of eleven doubles:
       result= [x_pressed, y_pressed, x_released, y_released,
                xndc_pressed, yndc_pressed, xndc_released, yndc_released,
                system, button, modifiers]

     If SYSTEM>=0, the first four coordinate values will be relative to
     that coordinate system.
     For SYSTEM<0, the first four coordinate values will be relative to
     the coordinate system under the mouse when the button was pressed.
     The second four coordinates are always normalized device coordinates,
     which start at (0,0) in the lower left corner of the 8.5x11 sheet of
     paper the picture will be printed on, with 0.0013 NDC unit being
     1/72.27 inch (1.0 point).  Look in the style sheet for the location
     of the viewport in NDC coordinates (see the style keyword).

     If STYLE is 0, there will be no visual cues that the mouse
     command has been called; this is intended for a simple click.
     If STYLE is 1, a rubber band box will be drawn; if STYLE is 2,
     a rubber band line will be drawn.  These disappear when the
     button is released.

     Clicking a second button before releasing the first cancels the
     mouse function, which will then return nil.
     Ordinary text input also cancels the mouse function, which again
     returns nil.

     The left button reverses forground for background (by XOR) in
     order to draw the rubber band (if any).  The middle and right
     buttons use other masks, in case the rubber band is not visible
     with the left button.

     long(result(9)) is the coordinate system in which the first four
     coordinates are to be interpreted.
     long(result(10)) is the button which was pressed, 1 for left, 2
     for middle, and 3 for right (4 and 5 are also possible).
     long(result(11)) is a mask representing the modifier keys which
     were pressed during the operation: 1 for shift, 2 for shift lock,
     4 for control, 8 for mod1 (alt or meta), 16 for mod2, 32 for mod3,
     64 for mod4, and 128 for mod5.

     Holding the shift key and pressing the left mouse button is
     equivalent to pressing the middle mouse button.  Similarly,
     pressing meta-left is equivalent to the right button.  This
     permits access to the middle and right button functions on
     machines (e.g.- most laptops) with two button or one button
     mice.  The long(result(10)) value returned by mouse() reflects
     this convention, returning 2 or 3 for those cases, even though
     it is button 1 that is actually being pressed.  Therefore, there
     is no way to distinguish shift-left from shift-middle, because the
     long(result(11)) mask indicates tht the shift button is pressed
     in either case.  (And on a machine without a middle button,
     there would be no way to emulate shift-middle anyway.)

   SEE ALSO: moush
 */

func moush(y, x, ireg)
/* DOCUMENT moush()
         or moush(y, x, ireg)
     returns the 1-origin zone index for the point clicked in
     for the default mesh, or for the mesh (X,Y) (region array IREG).
 */
{
  xy= mouse(-1, 0, "<Click mouse in mesh>");
  if (is_void(xy)) return [];
  else return mesh_loc(xy(2), xy(1), y, x, ireg);
}

extern pause;
/* DOCUMENT pause, milliseconds
         or pause(milliseconds)
     pause for the specified number of milliseconds of wall clock
     time, or until input arrives from the keyboard.
     If you call pause as a function, the return value is 1
     if the specified number of milliseconds elapsed, or 0 if
     keyboard input caused the pause to abort.
     This is intended for use in creating animated sequences.
 */

extern rgb_read;
/* DOCUMENT rgb = rgb_read()
         or rgb = rgb_read(n)
     Read contents of current graphics window, or of graphics window N.
     RGB is a 3xNXxNY array of char where NXxNY is the current shape of
     the window in pixels.  RGB(1,,) is the red component, RGB(2,,) is
     the green component, and RGB(3,,) is the blue component, with 0
     black and 255 full intensity.  RGB(,,1) is the top row of the
     window, RGB(,,2) the second row, and so on to RGB(,,0), which is
     the bottom row.  (So RGB(,,::-1) to pli redraws a copy.)
 */

/*--------------------------------------------------------------------------*/

func histeq_scale(z, top=, cmin=, cmax=)
/* DOCUMENT histeq_scale(z, top=top_value, cmin=cmin, cmax=cmax)
     returns a byte-scaled version of the array Z having the property
     that each byte occurs with equal frequency (Z is histogram
     equalized).  The result bytes range from 0 to TOP_VALUE, which
     defaults to one less than the size of the current palette (or
     255 if no pli, plf, or palette command has yet been issued).

     If non-nil CMIN and/or CMAX is supplied, values of Z beyond these
     cutoffs are not included in the frequency counts.

   SEE ALSO: bytscl, plf, pli
 */
{
  if (is_void(top)) top= bytscl([0.,1.])(2);  /* palette size - 1 */
  top= long(top);
  if (top<0 | top>255) error, "top value out of range 0-255";
  y= z(*);
  if (!is_void(cmin)) y= y(where(y>=cmin));
  if (!is_void(cmax)) y= y(where(y<=cmax));
  y= y(sort(y));
  x= span(0.,1., numberof(y));
  xp= span(0.,1., top+2);
  bins= interp(y, x, xp);
  list= where(bins(dif)<=0.0);
  if (numberof(list)) {
    /* some value (or values) of z are repeated many times --
       try to handle this by adding a small slope to the sorted y */
    dy= y(0)-y(1);
    if (!dy) dy= 1.0;
    for (eps=1.e-10 ; eps<1000.1 ; eps*=10.) {
      bins= interp(y+eps*dy*x, x, xp);
      list= where(bins(dif)<=0.0);
      if (!numberof(list)) break;
    }
    if (eps>1000.) error, "impossible error??";
  }
  return char(max(min(digitize(z,bins)-2,top),0));
}

func spann(zmin, zmax, n, fudge=)
/* DOCUMENT spann(zmin, zmax, n)
     return no more than N equally spaced "nice" numbers between
     ZMIN and ZMAX.
   SEE ALSO: span, spanl, plc, plfc
 */
{
  if (is_void(fudge)) fudge= 1.e-12;
  reverse= zmin>zmax;
  if (reverse) { dz=zmin; zmin=zmax; zmax=dz; }
  dz= (zmax-zmin)/max(double(n),0.);
  if (!dz) dz= abs(zmin);
  if (dz) {
    power= floor(log10(dz)+0.00001);
    base= dz/10.^power;
    if (base>5.00001) { base= 1.0; power+= 1.0; }
    else if (base>2.00001) base= 5.0;
    else base= 2.0;
    /* round dz up to the nearest "nice" number */
    dz= base*10.^power;
    zmin= ceil(zmin/dz - fudge);
    zmax= floor(zmax/dz + fudge);
    nz= long(zmax-zmin+1.0);
    if (nz>1) {
      levs= span(zmin*dz, zmax*dz, nz);
    } else {
      if (nz<1) {   /* find any nice number in interval */
        if (base<1.5) { base= 5.0; power-= 1.0; }
        else if (base<2.5) base= 1.0;
        else base= 2.0;
        dz= base*10.^power;
        zmin= ceil(zmin/dz + 0.001);
      }
      levs= [zmin*dz];
    }
  } else {
    levs= [-1.0,1.0];
  }
  if (reverse) levs= levs(0:1:-1);
  return levs;
}

/*--------------------------------------------------------------------------*/

extern _pl_init;
/* xxDOCUMENT _pl_init
     initializes the Gist graphics package -- DON'T EVER CALL THIS.
 */
_pl_init, GISTPATH; /* ...except right here (see paths.i) */

/*= SECTION(plothi) higher level plotting functions ========================*/
/* functions which call plg, plf, or other automatic legend generating
 * functions must be defined after _pl_init, since that function turns
 * on argument "quining" which changes the way things are parsed (yuck) */

func plmk(y,x,marker=,width=,color=,msize=)
/* DOCUMENT plmk, y,x

     Make a scatter plot of the points Y versus X.  If X is nil,
     it defaults to indgen(numberof(Y)).  By default, the marker
     cycles through 7 predefined marker shapes.  You may specify a shape
     using the marker= keyword, line width using the width= keyword (you
     get solid fills for width>=10), color using the color= keyword.
     You can also use the msize= keyword to scale the marker (default
     msize=1.0).  You can change the default width, color, or msize
     using the plmk_default function.

     The predefined marker= values are:

     marker=
       1        square
       2        cross
       3        triangle
       4        circle
       5        diamond
       6        cross (rotated 45 degrees)
       7        triangle (upside down)

     You may also put marker=[xm,ym] where xm and ym are vectors
     of NDC coordinates to design your own custom marker shapes.

   SEE ALSO: plmk_default, plg (type=0 keyword), pleb
 */
{
  if (is_void(marker)) {
    marker= (_plmk_count-1)%7 + 1;
    _plmk_count++;
  }
  if (numberof(marker)==1) {
    marker= *_plmk_markers(marker);
  } else if (dimsof(marker)(1)!=2 || dimsof(marker)(3)!=2 ||
             dimsof(marker)(2)<=2) {
    error, "illegal marker= keyword value";
  }
  xm= marker(,1);
  ym= marker(,2);
  if (is_void(msize)) msize= _plmk_msize;
  if (!is_void(msize)) {
    xm*= msize;
    ym*= msize;
  }
  if (is_void(color)) color= _plmk_color;
  if (structof(color)==string) {
    n= where(color==["bg","fg","black","white",
                     "red","green","blue","cyan","magenta","yellow",
                     "grayd","grayc","grayb","graya"]);
    if (numberof(n)!=1) error, "unrecognized color name: "+color;
    color= char(-n(1));
  }
  ecolor= color;
  if (is_void(width)) width= _plmk_width;
  if (!is_void(width)) {
    if (width>=10) {
      solid= 1;
      if (is_void(color)) color= ecolor= char(-2);
      z= array(char(color), 1+numberof(y));
      width= [];
    }
  }
  n= array(1,1+numberof(y));
  n(1)= numberof(ym);
  if (is_void(x)) x= indgen(numberof(y));
  plfp, z,grow(ym,y),grow(xm,x),n,edges=1,ewidth=width,ecolor=ecolor;
}

func plmk_default(color=, msize=, width=)
/* DOCUMENT plmk_default, color=color, msize=msize, width=width

     sets default color, msize, and width values for plmk.  Use
     width=10 to get solid fills.  With no parameters, plmk_default
     restores the initial default values.

   SEE ALSO: plmk
 */
{
  { extern _plmk_color, _plmk_width, _plmk_msize; }
  i= 0;
  if (!is_void(color)) _plmk_color= color;
  else i++;
  if (!is_void(width)) _plmk_width= width;
  else i++;
  if (!is_void(msize)) _plmk_msize= msize;
  else i++;
  if (i==3) _plmk_msize= _plmk_color= _plmk_width= [];
}

_plmk_count= 1;
_plmk_msize= _plmk_color= _plmk_width= [];
/* predefined markers: square, +, delta, circle, diamond, x, grad */
_plmk_markers= span(-pi,pi,37)(zcen);
_plmk_markers= [&([[-1,1,1,-1],[-1,-1,1,1]]*.007),
                &([[-4,-1,-1,1,1,4,4,1,1,-1,-1,-4],
                   [-1,-1,-4,-4,-1,-1,1,1,4,4,1,1]]*.007/sqrt(7)),
                &([[-sqrt(3),sqrt(3),0],[-1,-1,2]]*.007/sqrt(.75*sqrt(3))),
                &([cos(_plmk_markers),sin(_plmk_markers)]*.007/(pi/4.)),
                &([[-1,0,1,0],[0,-1,0,1]]*.007*sqrt(2)),
                &([[-1,-2.5,-1.5,0,1.5,2.5,1,2.5,1.5,0,-1.5,-2.5],
                   [0,-1.5,-2.5,-1,-2.5,-1.5,0,1.5,2.5,1,2.5,1.5]]*.007*
                  sqrt(2)/sqrt(7)),
                &([[0,sqrt(3),-sqrt(3)],[-2,1,1]]*.007/sqrt(.75*sqrt(3)))];

func plfc(z, y, x, ireg, levs=, colors=, region=, triangle=)
/* DOCUMENT plfc, z, y, x, levs=z_values
         or plfc, z, y, x, ireg, levs=z_values

     fills contours of Z on the mesh Y versus X.  Y, X, and IREG are
     as for plm.  The Z array must have the same shape as Y and X.
     The function being contoured takes the value Z at each point
     (X,Y) -- that is, the Z array is presumed to be point-centered.

     The LEVS keyword is a list of the values of Z at which you want
     contour curves.  These curves divide the mesh into numberof(LEVS)+1
     regions, each of which is filled with a solid color.  If LEVS is
     nil, up to 19 "nice" equally spaced level values spanning the
     range of Z are selected.  The level values actually used are
     always output to the external variable plfc_levs.

     If you specify levs=, you may also specify colors= a list of
     colors of length numberof(LEVS)+1.  The colors should be indices
     into the current palette.  If you do not specify them, equally
     spaced colors are chosen.

     The following keywords are legal (each has a separate help entry):
   KEYWORDS: triangle, region
   SEE ALSO: plg, plm, plc, plv, plf, pli, plt, pldj, plfp, plmesh
             color_bar, spann, contour, limits, logxy, range, fma, hcp
 */
{
  zmin= min(z);
  zmax= max(z);
  if (is_void(levs)) {
    levs= spann(zmin, zmax, 20, fudge=-0.05);
  } else if (numberof(levs)>1) {
    levs = double(levs);
    dz= levs(dif); /* blows up if <2 or not numeric */
    reverse= max(dz);
    if (numberof(dz)!=numberof(levs)-1 ||
        anyof((dz>0.)!=(reverse>0.)) || !reverse)
      error, "levs= values must be monotone 1D";
    reverse= reverse<0.;
    if (reverse) levs= levs(0:1:-1);
    else levs= levs(1:0);
  } else {
    levs= [double(levs(1))];
  }
  { extern plfc_levs, plfc_colors; }
  plfc_levs= levs;
  n= numberof(levs);

  pairs= [grow([min(-1.e30,1.1*zmin)],levs),
          grow(levs,[max( 1.e30,1.1*zmax)])];
  if (reverse) pairs= pairs(0:1:-1,);

  /* make sure some kind of reasonable palette is installed */
  { local nc, yc, xc; }
  palette, query=1, nc, yc, xc;
  nc= numberof(nc);
  if (nc<3) {
    palette, "viridis.gp";
    palette, query=1, nc, yc, xc;
    nc= numberof(nc);
  }

  if (is_void(colors)) {
    colors= char(span(0,nc-1,n+2)(zcen));
  } else {
    if (numberof(colors)!=n+1)
      error, "colors= must specify one more color than levs=";
    if (structof(colors)!=char) {
      cmin= min(colors);
      cmax= max(colors);
      dz= 0.5*(cmax-cmin)/double(n+1);
      colors= bytscl(colors,cmin=cmin-dz,cmax=cmax+dz);
    }
  }
  plfc_colors= colors;

  if (is_void(triangle)) triangle= array(short,dimsof(z));

  for (i=1 ; i<=n+1 ; ++i) {
    pair= pairs(i,);
    if (pair(2)<zmin || pair(1)>zmax) continue;
    nc= contour(yc,xc, pair,z,y,x,ireg,triangle=triangle);
    if (!numberof(nc)) continue;
    plfp,array(colors(i),numberof(nc)),yc,xc,nc,edges=0;
  }
}

func color_bar(levs, colors, vert=, labs=, adjust=, ecolor=)
/* DOCUMENT color_bar
         or color_bar, levs, colors
     Draw a color bar below the current coordinate system.  If LEVS is
     not specified uses plfc_levs (set by previous call to plfc).  If
     COLORS is specified, it should have one more value than LEVS,
     otherwise equally spaced colors are chosen, or plfc_colors if
     plfc_levs was used.  With the vert=1 keyword the color bar appears
     to the left of the current coordinate system (vert=0 is default).
     By default, color_bar will attempt to label some of the color
     interfaces.  With the labs= keyword, you can force the labelling
     algorithm as follows: labs=0 supresses all labels, labs=n forces
     a label at every nth interface, labs=[i,n] forces a label at every
     nth interface starting from interface i (0<=i<=numberof(LEVS)).
     You can use the adjust= keyword to move the bar closer to (adjust<0)
     or further from (adjust>0) the viewport, and the height= keyword to
     set the height of any labels (default 14 points).
   SEE ALSO: plfc
 */
{
  if (is_void(levs)) {
    if (is_void(plfc_levs)) error, "no levels specified";
    levs= plfc_levs;
    n= numberof(levs)+1;
    if (is_void(colors)) colors= plfc_colors;
  } else {
    n= numberof(levs)+1;
    if (is_void(colors)) colors= bytscl(span(1,n,n),cmin=0.5,cmax=n+0.5);
  }
  if (n != numberof(colors))
    error, "numberof(colors) must be one more than numberof(levs)";

  port= viewport();
  if (is_void(adjust)) adjust= 0.;
  dx= dy= 0.;
  if (vert) {
    x= (port(2)+adjust+[0.022,0.042])(-:1:n+1,);
    dx= 0.005;
    y= span(port(3),port(4),n+1)(,-:1:2);
  } else {
    y= (port(3)-adjust-[0.045,0.065])(-:1:n+1,);
    dy= -0.005;
    x= span(port(1),port(2),n+1)(,-:1:2);
  }
  sys= plsys(0);
  plf,[colors],y,x,edges=1,ecolor=ecolor, legend="";
  plsys, sys;

  if (is_void(labs) || labs(0)>0) {
    if (numberof(levs)>1) {
      dz= levs(dif);
      if (numberof(dz)!=numberof(levs)-1 ||
          anyof((dz>0.)!=(dz(1)>0.)) || !dz(1))
        error, "levs must be monotone 1D";
      levs= levs(1:0);
      levs= grow([2*levs(1)-levs(2)],levs,[2*levs(0)-levs(-1)]);
    } else {
      levs= double(levs(1));
      if (!levs) levs= [-1.,levs,1.];
      else levs= [0.,levs,2*levs];
    }
    if (numberof(labs)<2) {
      if (is_void(labs)) labs= (n-1)/4 + 1;
      orig= where(levs<1.e-9*max(levs(dif)));
      if (numberof(orig)==1) labs= [orig(1)%labs,labs];
      else labs= [(n%labs)/2,labs];
    }
    list= where(indgen(0:n)%labs(2)==labs(1));
    x= x(list,);
    y= y(list,);
    labs= swrite(format="%g",levs(list));
    plsys, 0;
    pldj, x(,2),y(,2),x(,2)+dx,y(,2)+dy, legend="";
    plsys, sys;
    plt1, labs,x(,2)+dx,y(,2)+dy, justify=(vert?"LH":"CT"), height=height,
      font="helvetica";
  }
}

/* pleb from Regis Lachaume 2003 */
func pleb(y, x, dx=, dy=, mfill=, color=, width=, marker=, msize=)
/* DOCUMENT pleb, y, x, dx=dx, dy=dy
     plots Y vs. X with error bars.

     Uncertainty on X and/or Y are specified with the dx= and dy= keywords.
     X and Y must have same dimensions, dx= and dy= must be conformable
     with X (or Y).  Either dx or dy may be nil for no error bar in that
     direction.  Scalar dx or dy gives equal error bars at all points,
     dimsof(dx)==dimsof(X), etc., gives different error bar at each point.
     dx= and dy= may also have a trailing dimension of length 2 in order
     to get asymmetric error bars; dx(..,1) is the lower error bar length,
     and dx(..,2) is the upper error bar length in that case, etc.

     If marker=, msize=, or width= is specified, markers are positioned
     at X, Y using plmk.  Use the mfill=1 keyword to get filled markers
     (width>=10. in plmk; width= refers to error bar width in pleb).

   EXAMPLE:
      x = [0, 1, 2, 3];
      y = [0, 2, 4, 7];
      pleb, y, x, dx=0.2, dy=[0.3, 0.4, 0.5, 0.3], mfill=1;
         Uncertainties on dx are the same for all X, and those
         on Y are different for each value of Y.  Filled markers
         will be displayed at (X, Y).

   KEYWORDS: color, width, marker, msize
      dx     uncertainty on X
      dy     uncertainty on Y

   SEE ALSO: plmk, pldj
 */
{
  if (is_void(dx)) dx = 0.;
  if (is_void(dy)) dy = 0.;

  xmin = x-dx;
  xmax = x+dx;
  if (numberof(x) != numberof(xmin)) {
    xmin = xmin(..,1);
    xmax = xmax(..,2);
  }

  ymin = y-dy;
  ymax = y+dy;
  if (numberof(y) != numberof(ymin)) {
    ymin = ymin(..,1);
    ymax = ymax(..,2);
  }

  pldj, x, ymin, x, ymax, color=color, width=width, legend="";
  pldj, xmin, y, xmax, y, color=color, width=width, legend="";
  if (!is_void(marker) || !is_void(msize) || !is_void(mfill))
    plmk, y, x, color=color, msize=msize, marker=marker,
      width=(mfill? 20.: width);
}

/*--------------------------------------------------------------------------*/
