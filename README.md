#ForEx

**For**tran User Defined **Ex**ceptions Handler

[![Build Status](https://travis-ci.org/victorsndvg/ForEx.svg?branch=master)](https://travis-ci.org/victorsndvg/ForEx.svg)
[![codecov.io](https://codecov.io/github/victorsndvg/ForEx/coverage.svg?branch=master)](https://codecov.io/github/victorsndvg/ForEx?branch=master)

##License

[![License](https://img.shields.io/badge/license-GNU%20LESSER%20GENERAL%20PUBLIC%20LICENSE%20v3%2C%20LGPLv3-red.svg)](http://www.gnu.org/licenses/lgpl-3.0.txt)

##What is ForEx?

**ForEx** is fortran 2003 project taking advance of the [C preprocessor](https://gcc.gnu.org/onlinedocs/cpp/) capabilities in order to emulate exception handling.

##Features

- **Exception hierarchy:** **ForEx** can handle any error object extended from the **Exception** base class. 
- **Local flow control:** throwing an exception changes the local flow. **THROW** performs local jumps to the end of the **TRY** frame or **FINALLY**.
- **Global handling:** the exception stack is a global object under the singleton pattern.
- **Single THROW call per scope:**
   - Single throw call per local **TRY** scope.
   - Single throw call per local **CATCH** scope.
   - Single throw call per local **FINALLY** scope.
- **Re-throwing:** a exception can be raised again in the **CATCH** scope.
- **Re-throwing Backtrace:** an exception saves the stack of contexts where it has been throwed.
- **Handle any previously throwed exception:** **CATCH** iterate over all the exceptions looking for the first that matches the same *class*. It only handle a single exception per **TRY** frame.
- **Customizable catching action:** Exception *class* contains the **Catch** procedure to customize the action performed when cathing it.
- **Automatic Backtrace of non handled exceptions:** going out of the main **TRY**/**ENDTRY** scope with non handled exceptions in the stack causes *exception backtrace flush*.

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

###Using ForEx in your program

```fortran
program test
USE ForEx

implicit none
#include "ExceptionHandler.i90"

TRY
    ! Variable allocation
    if(Error) then
        THROW(Exception(Code=-1, Message='An error message')
    endif
CATCH(Exception, Ex)
    call Ex%Print()
FINALLY
    ! Variable deallocation
ENDTRY

end program test
```


