#ifndef d2c_sysstring
#define d2c_sysstring

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
#include <iterator>




namespace System
{

char UpCase( char C );
wchar_t UpCase( wchar_t C );
int Pos(const std::string& Substr, const std::string& S);
int Pos(const char* Substr, const std::string& S); // else SmallString/strin ambigouity
int Pos(char Substr, const std::string& S);
int Pos( const std::wstring& Substr, const std::wstring& Source );
int Pos( wchar_t C, const std::wstring& S );

std::string WideCharToString( wchar_t* S );
wchar_t* StringToWideChar( const std::string& Src, wchar_t* Dest, int DestSize );
std::string WideCharLenToString( wchar_t* S, int Len );
void WideCharLenToStrVar( wchar_t* Src, int Len, std::string& Dest );
void WideCharToStrVar( wchar_t* S, std::string& Dest );
int UnicodeToUtf8( Char* Dest, const wchar_t* Source, unsigned int MaxBytes );
unsigned int UnicodeToUtf8( Char* Dest, unsigned int MaxDestByte, const wchar_t* Source, unsigned int SourceChars );
int Utf8ToUnicode( wchar_t* Dest, const Char* Source, unsigned int MaxChars );
//unsigned int Utf8ToUnicode( wchar_t* Dest, int MaxDestChars, const Char* Source, unsigned int SourceBytes );
UTF8String UTF8Encode( const std::wstring& S );
std::wstring UTF8Decode( const UTF8String S );
UTF8String AnsiToUtf8( const String& S );
String Utf8ToAnsi( const UTF8String S );
UCS4String WideStringToUCS4String( const std::wstring& S );
std::wstring UCS4StringToWideString( const UCS4String& S );
	
/*
// from here on not contained in Delphi2Cpp trial

Copy
Chr
LowerCase
Insert
Delete
StringOfChar
SetString
Concat
*/

} // namespace d2c_string

std::string wstr2str(const std::wstring& xs);
std::wstring str2wstr(const std::string& xs);
char wchar2char(wchar_t xc);
wchar_t char2wchar(char xc);


#endif