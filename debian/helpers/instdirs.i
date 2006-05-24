/*
  $Id: instdirs.i,v 1.1 2006-05-24 06:13:26 paumard Exp $
  To be used in Debian packages for Yorick add-ons

  yorick -batch debian/instdirs.i $DESTDIR

  Will output the directories for architecture dependent and independent
  files.
  
*/
DESTDIR=get_argv()(2);
if (numberof(get_argv())>=3) INFILE=get_argv()(3);

DEPDIR=DESTDIR+Y_HOME;
if (Y_SITE==Y_HOME) INDEPDIR=DESTDIR+streplace(Y_HOME,strfind("usr/lib/",Y_HOME),"usr/share/"); else INDEPDIR=DESTDIR+Y_SITE;

write,format="%s\n",DEPDIR;
write,format="%s\n",INDEPDIR;
