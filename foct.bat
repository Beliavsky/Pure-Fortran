@echo off
:: transpile Octave code to Fortran, link it with Fortran object files, and run it
:: usage: foct.bat foo
:: where foo.m is an Octave source file
python xoct2f.py %1.m
if exist a.exe del a.exe
:: gfortran -Wfatal-errors lapack_d.f90 linear_algebra.f90 octave_funcs.f90 %1.f90
gfortran -Wfatal-errors lapack_d.o linear_algebra.o octave_funcs.o %1.f90
if exist a.exe a.exe
