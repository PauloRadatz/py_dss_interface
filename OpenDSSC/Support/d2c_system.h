#ifndef d2c_systemH
#define d2c_systemH

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

#include "d2c_systypes.h"
#include <stdlib.h>
#include <sstream>


#define MAXIDX(x) (sizeof(x)/sizeof(x[0]))-1

class Currency;


namespace System
{
extern int Argc;
extern PPChar Argv;
extern Char* CmdLine;
extern int CmdShow;
extern bool IsLibrary;
extern bool IsConsole;  

template <class T>
void GetMem(T*& P, int Size)
{
  P = ( T* ) malloc(Size);
}

template <class T>
void FreeMem(T*& P, int Size = -1)
{
  free(P);
}

template <class T>
void ReallocMem(T*& P, int Size)
{
  if(P != NULL)
  {
	  if(Size > 0)
  	  P =  ( T* ) realloc ( P, Size );
	  else
	  if(Size == 0)
	  {
	    free(P);
	    P = NULL;
	  }
  }
  else
  {
	  if(Size > 0)
	    P = ( T* )  malloc(Size);
  }
}

template <class T>
T Pred(const T& xT)
{
  int t = (int) xT;  // typecast to "int" allows incrementation of enumerated values
  return (T) --t;
}

template <class T>
T Succ(const T& xT)
{
  int t = (int) xT;  // typecast to "int" allows incrementation of enumerated values
  return (T) ++t;
}

template <class T>
T Abs(const T xT)
{
  return xT < 0 ? -xT : xT;
}


void FillChar( void* X, int Count, unsignedchar Value );
void FillChar( char* X, int Count, unsignedchar Value );
void FillChar( wchar_t* X, int Count, unsignedchar Value );
void FillChar( std::string& X, int Count, unsignedchar Value );
void FillChar( std::wstring& X, int Count, unsignedchar Value );

template <typename T>
void Val(const std::string& S, T* V, int& Code)
{
  std::istringstream iss(S);
  iss >> *V;
  Code = 0;
}

template <typename T>
void Val(const std::wstring& S, T* V, int& Code)
{
  std::wistringstream iss(S);
  iss >> *V;
  Code = 0;
}


template <typename T>
void Str(T xT, std::string& xs)
{
  std::ostringstream oss;
  oss << xT;
  xs = oss.str();  
}

template <typename T>
void Str(T xT, std::wstring& xs)
{
  std::wostringstream oss;
  oss << xT;
  xs = oss.str();  
}

void Str(double xd, std::string& xs);
void Str(long double xd, std::string& xs);
void Str(int xd, int xiMinWidth, std::string& xs);
void Str(double xd, int xiMinWidth, std::string& xs);
void Str(long double xd, int xiMinWidth, std::string& xs);
void Str(long double xd, int xiMinWidth, std::string& xs);
void Str(double xd, int xiMinWidth, int xiDecPlaces, std::string& xs);
void Str(long double xd, int xiMinWidth, int xiDecPlaces, std::string& xs);
void Str(const Currency& xcr, int xiMinWidth, int xiDecPlaces, std::string& xs);

void Str(double xd, std::wstring& xs);
void Str(long double xd, std::wstring& xs);
void Str(int xd, int xiMinWidth, std::wstring& xs);
void Str(double xd, int xiMinWidth, std::wstring& xs);
void Str(long double xd, int xiMinWidth, std::wstring& xs);
void Str(double xd, int xiMinWidth, int xiDecPlaces, std::wstring& xs);
void Str(long double xd, int xiMinWidth, int xiDecPlaces, std::wstring& xs);
void Str(const Currency& xcr, int xiMinWidth, int xiDecPlaces, std::wstring& xs);

void Move(void* Source, void* Dest, int Count);
void Move(const std::string& Source, std::string& Dest, unsigned int Count);
void Move(const std::wstring& Source, std::wstring& Dest, unsigned int Count);
void Move(const char* Source, std::string& Dest, unsigned int Count);
void Move(const wchar_t* Source, std::wstring& Dest, unsigned int Count);
void Move(const std::string& Source, char* Dest, unsigned int Count);
void Move(const std::wstring& Source, wchar_t* Dest, unsigned int Count);



String ParamStr( int Index );
int ParamCount( );


WORD Swap( WORD X );
int Swap( int X );
unsignedint Swap( unsignedint X );
int64_t Swap( int64_t X );


void Assert( bool expr );
void Assert( bool expr, const String& Msg );


/*
// from here on not contained in Delphi2Cpp trial
ObjectIs
Hi
Lo
Odd
Sqt
Dec
Inc
Assert
Assigned
Ptr
Add
High
Low

void* AllocMem( unsignedint Size );
void GetMem( void*& P, int Size );
void FreeMem( void*& P, int Size = - 1 );
void ReallocMem( void*& P, int Size );

// maximum Value of the biggest signed and unsigned Integer type available
MaxSIntValue
MaxUIntValue

*/

}  // namespace System


#endif //  lpl_systemH
