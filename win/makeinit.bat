echo off

rem Usage: makeinit codir
rem   codir   directory containing codger.exe (e.g.- ..\Debug)
rem Purpose: create yinit.c and ywrap.c for yorick build process

rem edit the parameters for this codger command line to build versions
rem of yorick with alternative sets of packages
rem
%1\codger.exe w yor -I../i0 std.i matrix.i fft.i graph.i
%1\codger.exe i "" "" yor
