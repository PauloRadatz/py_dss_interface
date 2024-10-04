#ifndef d2c_systypesH
#define d2c_systypesH

/*
    Copyright of the basic file version
    -----------------------------------

    Explanation: in the following license "library" means
    the following files:

    System.pas / d2c_system.pas
    System.h
    d2c_sysconst.h
    d2c_syscurr.h d2c_syscurr.cpp
    d2c_sysdate.h d2c_sysdate.cpp
    d2c_sysexcept.h d2c_sysexcept.cpp
    d2c_sysfile.h, d2c_sysfile.cpp
    d2c_sysmath.h d2c_sysmath.cpp
    d2c_sysstring.h d2c_sysstring.cpp
    d2c_systypes.h
    d2c_varrec.h
    d2c_smallstring.h
    Windows.pas
    Sysutils.pas
    Sysutils.h Sysutils.cpp

    AS THEY ARE CONTAINED IN THE FREE TRIAL VERSION OF Delphi2Cpp.

    This library is derived from the FreePascal library:

    http://www.freepascal.org/

    FreePascal is published under the terms of GNU Lesser General
    Public License and the same terms apply to this library.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with d2c_sysfile.h/cpp; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA



    Copyright of the extended file version
    --------------------------------------


    The terms of the license above don't apply to extended versions
    of these files, which are distributed with commercial versions of
    Delphi2Cpp. Individual licenses are applied to them.
    The library doesn't depend on the commercial extensions and the
    the commercial extensions only originates from the author
    Dr. Detlef Meyer-Eltz or might use code which has no copyright restrictions

    Copyright (C) <2011>  <Dr. Detlef Meyer-Eltz>

    The extended version of this file is authorized for unlimited use in any
    Delphi2Cpp project.

	  http://www.texttransformer.com/Delphi2Cpp_en.html

*/


#if defined( WIN32 ) || defined( WIN64 )
#define NOMINMAX
#include "windows.h"
#define windows 1

#elif defined(linux)

#include <inttypes.h>
#include <time.h>
#include <fcntl.h>
#include <sys/stat.h>
//#define _WIDESTRING 1

#endif

#include <stdint.h>

#include <string>
#include <vector>
#include <limits>
#include "d2c_smallstring.h"
#include "DelphiSets.h"


#ifdef _UNICODE  // Visual C**
#define _WIDESTRING 1
#endif

#ifdef _MSC_VER
typedef ptrdiff_t ssize_t;
#endif

// definitions in one word are needed with C++Builder for properties
typedef short int shortint;
typedef unsigned char unsignedchar;
typedef unsigned int unsignedint;
typedef unsigned short unsignedshort;

#ifdef _WIDESTRING
  #define __T(x)  L ## x
  typedef std::wstring String;
  typedef wchar_t Char;
  typedef wchar_t UChar;
#else
  #define __T(x)  x
  typedef std::string String;
  typedef char Char;
  typedef unsigned char UChar;
#endif

#define _T(x)   __T(x)


namespace System
{

typedef std::string AnsiString;
typedef std::wstring WideString;
typedef SmallString<255> ShortString;
typedef std::string *PAnsiString;
typedef std::wstring *PWideString;
typedef String *PString;

typedef long double ValReal;
typedef unsigned int Cardinal;
typedef short int Integer;


typedef uint64_t QWord;


typedef void ( * TProcedure )( );



#ifdef windows
typedef HANDLE THandle;
typedef THandle TThreadID;
#else

typedef unsigned char  BYTE;
typedef unsigned short WORD;
typedef unsigned long DWORD;
typedef int BOOL;

typedef unsigned char cuint8;
typedef cuint8 cuchar;
typedef WORD cuint16;
typedef cuint16 cushort;
typedef int32_t cint32;
typedef cint32 cint;

#define __cdecl
#define __stdcall
#define __fastcall
typedef long HRESULT;

#endif


// Updated below to use standard int types 
// (original version was bonkers on 64-bit systems)
typedef double real;
typedef ssize_t SizeInt;
typedef size_t SizeUInt;
typedef intptr_t PtrInt;
typedef uintptr_t PtrUInt;
typedef int ValSInt;
typedef unsigned int ValUInt;


/* zero - terminated Strings */
typedef Char* PChar;
typedef Char** PPChar;

  /* AnsiChar is equivalent of char, So we need
    to use type renamings */
typedef Char TAnsiChar;
typedef Char AnsiChar;
typedef Char* PAnsiChar;
typedef PPChar PPAnsiChar;
typedef unsigned int UCS4Char;
typedef UCS4Char* PUCS4Char;
typedef UCS4Char TUCS4CharArray [ 251658240/*# range 0..0xeffffff*/ ];
typedef UCS4Char* PUCS4CharArray;
typedef std::vector< UCS4Char > UCS4String;
typedef String UTF8String;
typedef UTF8String* PUTF8String;

  //HRESULT             = type Longint;
  //TDateTime           = type Double;

typedef int TError;
typedef float* PSingle;
typedef double* PDouble;
typedef long double* PExtended;
typedef shortint* PSmallInt;
typedef shortint* PShortInt;
typedef int* PInteger;
typedef unsignedchar* PByte;
typedef unsignedint* PLongWord;
typedef int* PLongint;
typedef unsignedint* PCardinal;
typedef QWord* PQWord;
typedef int64_t* PInt64;
typedef PtrInt* PPtrInt;
typedef PtrUInt* PPtrUInt;
typedef SizeInt* psizeint;
typedef void** PPointer;
typedef PPointer* PPPointer;
typedef bool* PBoolean;
typedef unsignedshort* PWordBool;
typedef BOOL* PLongBool;
typedef SmallString<255>* PShortString;
typedef std::string* PAnsiString;
typedef wchar_t* PWideChar;
typedef wchar_t** PPWideChar;
typedef wchar_t UCS2Char;
typedef wchar_t* PUCS2Char;


const int MaxLongint = 0x7FFFFFFF;
#ifndef __BORLANDC__
#ifdef linux
const int MAXINT = std::numeric_limits<int>::max();
const int MaxInt = MAXINT;
#else
const int MaxInt = INT_MAX;
#endif
#endif




typedef int IntegerArray [ 251658240/*# range 0..0xeffffff*/ ];
typedef int* PIntegerArray;
typedef void* PointerArray [ 0x7fffff/*# range 0..512*1024*1024-2*/ ];
typedef void** PPointerArray;
typedef std::vector< SizeInt > TBoundArray;

#pragma pack(push, 1)
typedef Char* TPCharArray [ ( MaxLongint / sizeof( Char* ) ) - 1 - 0 + 1 ];
#pragma pack(pop)


typedef Char** PPCharArray;

typedef uintptr_t NativeUInt;

#ifdef linux

typedef uintptr_t THandle;
typedef uintptr_t TLibHandle;
typedef TLibHandle HMODULE;

#endif

}  // namespace System


using namespace System;

#endif //  d2c_systypesH
