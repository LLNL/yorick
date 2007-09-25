/*
  /etc/yorick/20usrlocal.i

  The purpose of this file is to add a local yorick tree under /usr/local for
  add-on packages installed using the pkg_mngr and locally maintained
  packages. Interpreted and compiled library paths (.i and .so files) are
  taken into account. If you additionally require to adjust the Gist palette
  and style file path (.gp and .gs files), please set the GISTPATH environment
  variable prior to starting yorick.

 */
require, "pathfun.i";
add_y_home,"/usr/local/lib/yorick/";
