#ForEx
**For**tran User Defined **Ex**ceptions Handler

[![Build Status](https://travis-ci.org/victorsndvg/ForEx.svg)](https://travis-ci.org/victorsndvg/ForEx.svg)
[![codecov.io](https://codecov.io/github/victorsndvg/ForEx/coverage.svg?branch=master)](https://codecov.io/github/victorsndvg/ForEx?branch=master)

##License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)

##What is ForEx?

**ForEx** is fortran 2003 project taking advance of the [C preprocessor](https://gcc.gnu.org/onlinedocs/cpp/) capabilities in order to emulate exception handling.


##How to get ForEx

```git clone https://github.com/victorsndvg/Forex.git ```

##Compilation

**ForEx** compile with GNU Fortran compiler 5.1 (and newer versions) and Intel Fortran compiler 15.0.1 (and newer versions).

**ForEx** uses [CMake](https://cmake.org/) as a portable compilation system. 

The easiest way to compile **ForEx** under Linux is:

```
$ cd ForEx
$ mkdir build
$ cd build
$ cmake ../
$ make
```

*To compile ForEx under Windows use de equivalent commands*

Remember, **ForEx** take advantage of the [C preprocessor](https://gcc.gnu.org/onlinedocs/cpp/). To include it in your project, you have to add the preprocessor flags while compiling.
Preprocesor flags depending on the compiler vendor:
- GNU Fortran: -cpp
- Intel Fortran: -fpp
- IBM XLF: -qsuffix=f=f90:cpp=f90

