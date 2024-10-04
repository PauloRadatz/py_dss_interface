
For Visual C++, gcc and other compilers most parts of the most basic
Delphi units System.pas and Sysutils.pas are translated from the free
pascal sources:

http://www.freepascal.org/

Remarks to the copyright are at the top of each file.


Though free pascal is made for use with many different operating
systems, the translation here is for Windows and Linux only. 

For use under Linux or with other compilers than VisualC++ either 
the line

#include "stdafx.h" 

has to be removed from the sources or a dummy header with this name
has to be made. Some definitions, which are created by Visual Studio
automatically are presupposed:

_UNICODE for unicode/widestring applications
_CONSOLE for console applications
_USRDLL  for dll's


Under Linux the command line arguments aren't accessible before
entering the main function of a program. This function therefore 
should start with something like:

  System::Argv = argv;
  System::Argc = argc;


The C++ counterpart of Sysutils.pas is written in a single source
file. System.pas however is split into some smaller files. But all
headers of these smaller files are put together into "System.h", which
is:


#ifndef SystemH
#define SystemH

#if defined( WIN32 ) || defined( WIN64 )
#define windows 1
#endif

#include "d2c_system.h"
#include "d2c_systypes.h"
#include "d2c_sysconst.h"
#include "d2c_syscurr.h"
#include "d2c_sysdate.h"
#include "d2c_systobj.h"
#include "d2c_openarray.h"
#include "d2c_sysexcept.h"
#include "d2c_sysmath.h"
#include "d2c_sysstring.h"
#include "d2c_sysfile.h"

using namespace System;

#endif // SystemH


There are three pas-file, which contains the according interface
declarations. These pas-files have to be put into the search paths for
files not to convert (VCL) in the option dialog of Delphi2Cpp. 


Restrictions
------------

The code was made originally by automatic translation with a special
version of Delphi2Cpp: FreePascal2Cpp. Some parts of the generated C++
code were improved manually. Large parts were tested and are working
well, but there isn't any guarantee that the code is completely
bug-free. There is no translation for the Variant class and only a
minimal implementation of a Currency class. The formatting of real
types in free pascal depend on a special binary layout of that types.
The formatting couldn't be reproduced exactly. Free pascal uses
Mersenne twister for the generation of random numbers. This part was
not translated back to C++ because there are a lot of C++
implementations of Mersenne twister in C++

http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/VERSIONS/C-LANG/c-lang.html
 
Currently a primitive random generator based on C++ standard
functions is used. Such standard function were preferred to
direct translations of Delphi functions at other places too. So there
might be slight differences to the original Delphi behavior. For
example the error position returned in the Code-parameter of the Val
procedure, is a dummy only in C++.


Despite of these restrictions for many users this provided C++ code will make 
the migration of their Delphi code much easier.

