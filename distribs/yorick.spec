Summary:    interpreted language and scientific graphics
Packager:   David H. Munro <munro1@llnl.gov>
Name:       yorick
Version:    1.6
Release:    02
Source:     yorick-%{version}.%{release}.tgz
URL:        ftp://ftp-icf.llnl.gov/pub/Yorick/doc/index.html

Copyright:  BSD
Group:      Development/Languages
Icon:       yicon48.xpm

Buildroot:  /tmp/yorick_rpmbuild
Prereq:     /sbin/install-info


%description
Yorick is an interpreted programming language for:
 * scientific simulations or calculations
 * postprocessing or steering large simulation codes
 * interactive scientific graphics
 * reading, writing, and translating large files of numbers

The language features a compact syntax for many common array
operations, so it processes large arrays of numbers very quickly and
efficiently.  Superficially, yorick code resembles C code, but yorick
variables are never explicitly declared and have a dynamic scoping
similar to many Lisp dialects.  The yorick language is designed to be
typed interactively at a keyboard, as well as stored in files for
later use.

This package includes an emacs-based development environment, which
you can launch by typing M-x yorick in emacs.


%prep
%setup
make prefix=/usr ysite

%build
make
make docs
cd doc; make yorick.info; cd ..

%install
if [ -d /tmp/yorick_rpmbuild ]; then
# do not use RPM_BUILD_ROOT here
  rm -rf /tmp/yorick_rpmbuild
fi
make INSTALL_ROOT=$RPM_BUILD_ROOT Y_BINDIR=$RPM_BUILD_ROOT/usr/bin install
mkdir $RPM_BUILD_ROOT/usr/share/info
cp -f doc/yorick.info* $RPM_BUILD_ROOT/usr/share/info
mkdir -p $RPM_BUILD_ROOT/usr/share/man/man1
cp -f doc/yorick.1 doc/gist.1 $RPM_BUILD_ROOT/usr/share/man/man1
gzip -9qnf $RPM_BUILD_ROOT/usr/share/info/yorick.info*
gzip -9qnf $RPM_BUILD_ROOT/usr/share/man/man1/yorick.1
gzip -9qnf $RPM_BUILD_ROOT/usr/share/man/man1/gist.1
#want contrib directory to belong to this package (although contents will
#belong to other packages such as yplot) so that clean delete of this package
#once other packages such as yplot are deleted.
mkdir $RPM_BUILD_ROOT/usr/lib/yorick/%{version}/lib/contrib
mkdir $RPM_BUILD_ROOT/usr/share/yorick/%{version}/contrib
mkdir -p $RPM_BUILD_ROOT/usr/share/emacs/site-lisp/site-start.d
cp -f emacs/yorick.el $RPM_BUILD_ROOT/usr/share/emacs/site-lisp
cp -f emacs/yorick-auto.el $RPM_BUILD_ROOT/usr/share/emacs/site-lisp/site-start.d

%clean
# do not use RPM_BUILD_ROOT here
rm -rf /tmp/yorick_rpmbuild

%post 
/sbin/install-info /usr/share/info/yorick.info.gz /usr/share/info/dir

%postun
/sbin/install-info --delete yorick /usr/share/info/dir

%files
%attr(-, root, root) %doc README NEWS LICENSE
%docdir /usr/share/yorick/%{version}/doc
%attr(-, root, root) %doc /usr/share/man/man1/yorick.1.gz
%attr(-, root, root) %doc /usr/share/man/man1/gist.1.gz
%attr(-, root, root) %doc /usr/share/info/yorick*
%attr(-, root, root) /usr/bin/yorick
%attr(-, root, root) /usr/bin/gist
%attr(-, root, root) /usr/lib/yorick/%{version}/
%attr(-, root, root) /usr/share/yorick/%{version}/
%attr(-, root, root) /usr/share/emacs/site-lisp/yorick*
%attr(-, root, root) /usr/share/emacs/site-lisp/site-start.d/yorick*

%changelog
* Sat Mar 12 2005  <munro1@llnl.gov> 1.6-02
- 1.6.02 changed plugin support, package Makefile system, see NEWS

* Fri Jul 30 2004  <munro1@llnl.gov> 1.6-01
- 1.6.01 added plugin support, fixed bugs, see NEWS

* Wed Oct  1 2003  <munro1@llnl.gov> 1.5-14
- 1.5.14 numerous bug fixes, see NEWS

* Thu Oct 31 2002  <munro1@llnl.gov>
- 1.5.12 fixes double alignment for big speedup on i86

* Mon Jul  1 2002  <munro@star.llnl.gov>
- 1.5.10 has numerous bug fixes since 08, see NEWS

* Sat Feb  2 2002  <munro@star.llnl.gov>
- 1.5.08 is 07 infant mortality replacement, see NEWS

* Tue Jan 22 2002  <munro@star.llnl.gov>
- numerous 1.5.07 bug fixes, see NEWS file

* Wed Nov 14 2001  <munro@star.llnl.gov>
- 1.5-06 bug fixes, see NEWS file

* Fri Oct 19 2001  David H. Munro <munro1@llnl.gov>
- numerous bug fixes relative to 1.5-01, 1.5-02 releases, see NEWS file

