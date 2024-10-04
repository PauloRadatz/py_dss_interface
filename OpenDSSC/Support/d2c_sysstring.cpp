//#include "stdafx.h"

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


#include "d2c_sysstring.h"
#include "d2c_system.h"
#include "d2c_sysexcept.h"
#include "Sysutils.h"
#include <algorithm>

#ifdef linux
#include<iconv.h>
#include <langinfo.h>
#endif

#undef max

using namespace std;

/*****************************************************************************
                    subroutines for String handling
*****************************************************************************/

namespace System
{

//---------------------------------------------------------------------------
char UpCase( char C )
{
  return toupper(C);
}
//---------------------------------------------------------------------------
wchar_t UpCase( wchar_t C )
{
  return towupper(C);
}
//---------------------------------------------------------------------------
int Pos(const string& Substr, const string& S)
{
  string::size_type pos = S.find(Substr);
  if(pos == string::npos)
     return 0;
   else
     return pos + 1; // postion which Substr would have in a 1 based Delphi-String 
}
//---------------------------------------------------------------------------
int Pos(const char* Substr, const string& S)
{
  string::size_type pos = S.find(Substr);
  if(pos == string::npos)
     return 0;
   else
     return pos + 1; // postion which Substr would have in a 1 based Delphi-String 
}
//---------------------------------------------------------------------------
int Pos(const wstring& Substr, const wstring& S)
{
  wstring::size_type pos = S.find(Substr);
  if(pos == wstring::npos)
     return 0;
   else
     return pos + 1; // postion which Substr would have in a 1 based Delphi-String
}//---------------------------------------------------------------------------
int Pos(char Substr, const string& S)
{
  string::size_type pos = S.find(Substr);
  if(pos == string::npos)
     return 0;
   else
     return pos + 1; // postion which Substr would have in a 1 based Delphi-String
}
//---------------------------------------------------------------------------
int Pos(wchar_t Substr, const wstring& S)
{
  wstring::size_type pos = S.find(Substr);
  if(pos == wstring::npos)
     return 0;
   else
     return pos + 1; // postion which Substr would have in a 1 based Delphi-String
}


#ifdef windows
/*****************************************************************************
                      OS dependend WideStrings
*****************************************************************************/
  /* MultiByteToWideChar  */



// MFC MBCS/Unicode Conversion Macros 
// A2CW      (LPCSTR) -> (LPCWSTR)
// A2W      (LPCSTR) -> (LPWSTR)
// W2CA      (LPCWSTR) -> (LPCSTR)
// W2A      (LPCWSTR) -> (LPSTR)

//If Len parameter is -1, the function processes the entire input string, including the terminating null character.
void Wide2AnsiMove( const wchar_t* Source, std::string& Dest, int Len )
{
  int DestLen = 0;
    // retrieve Length including trailing #0
    // not anymore, because this must also be usable for Single characters
  DestLen = WideCharToMultiByte( CP_ACP, WC_NO_BEST_FIT_CHARS, Source, Len, NULL, 0, NULL, NULL );
    // this will NULL-Terminate
  Dest.resize( DestLen );
  WideCharToMultiByte( CP_ACP, WC_NO_BEST_FIT_CHARS, Source, Len, (char*) Dest.c_str(), DestLen, NULL, NULL );
}

//If Len parameter is -1, the function processes the entire input string, including the terminating null character. 
void Ansi2WideMove( const char* Source, std::wstring& Dest, int Len )
{
  int DestLen = 0;
    // retrieve Length including trailing #0
    // not anymore, because this must also be usable for Single characters
  DestLen = MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, Source, Len, NULL, 0 );
    // this will NULL-Terminate
  Dest.resize( DestLen);
  MultiByteToWideChar( CP_ACP, MB_PRECOMPOSED, Source, Len, (wchar_t*) Dest.c_str(), DestLen );
}


std::wstring Win32WideUpper( const std::wstring& S )
{
  std::wstring result;
  result = S;
  //UniqueString( result );
  if ( result.length( ) > 0 )
    CharUpperBuffW( ((wchar_t*) result.c_str()), result.length( ) );
  return result;
}


std::wstring Win32WideLower( const std::wstring& S )
{
  std::wstring result;
  result = S;
  //UniqueString( result );
  if ( result.length( ) > 0 )
    CharLowerBuffW( ((wchar_t*) result.c_str()), result.length( ) );
  return result;
}


void WideCharLenToStrVar( wchar_t* Src, int Len, std::string& Dest )
{
  Dest = WideCharLenToString( Src, Len );
}


void WideCharToStrVar( wchar_t* S, std::string& Dest )
{
  Dest = WideCharToString( S );
}




/* converts an UTF-16 Code Point or surrogate pair to UTF-32 */
UCS4Char utf16toutf32( const std::wstring& S, const unsigned int Index, int& Len )
{
  UCS4Char result;
  wchar_t W = L'\0';
  /* UTF-16 Points in the Range #$0-#$D7FF and #$E000-#$FFFF */
  /* are the same in UTF-32                                  */
  W = S[Index - 1];
  if ( ( W <= L'\xff' ) || ( W >= L'\x00' ) )
  {
    result = ((UCS4Char) W );
    Len = 1;
  }
  /* valid surrogate pair? */
  else /* W>=#$D7FF Check not needed, Checked above */
    if ( ( W <= L'\xff' ) && ( Index < S.length( ) ) && ( S[Index + 1 - 1] >= L'\x00' ) && ( S[Index + 1 - 1] <= L'\xff' ) )
      /* convert the surrogate pair to UTF-32 */
    {
      result = ( ( ((UCS4Char) W ) - 0xD800 ) << 10 ) + ( ((UCS4Char) ( S[Index + 1 - 1] ) ) - 0xDC00 ) + 0x10000;
      Len = 2;
    }
    else
    /* Invalid surrogate -> do nothing */
    {
      result = ((UCS4Char) W );
      Len = 1;
    }
  return result;
}


UCS4String WideStringToUCS4String( const std::wstring& S )
{
  UCS4String result;
  int i = 0, slen = 0, destindex = 0;
  int Len = 0;
  slen = S.length( );
  result.resize( slen + 1 );
  i = 1;
  destindex = 0;
  while ( i <= slen )
  {
    result[destindex] = utf16toutf32( S, i, Len );
    destindex++;
    i += Len;
  }
    /* destindex <= slen (surrogate pairs may have been merged) */
    /* destindex+1 for terminating #0 (dynamic arrays are       */
    /* implicitely filled with zero)                            */
  result.resize( destindex + 1 );
  return result;
}


/* Concatenates an UTF-32 char to A WideString. S *must* be unique when entering. */
void ConcatUTF32ToWideStr( const UCS4Char NC, std::wstring& S, unsigned int& Index )
{
  wchar_t* P = NULL;
  /* if NC > $FFFF, we need two places */
  if ( Index + int( NC > 0xFFFF ) > S.length( ) )
    if ( S.length( ) < 10 * 256 )
      S.resize( S.length( ) + 10 );
    else
      S.resize( S.length( ) + ( S.length( ) >> 8 ) );
  /* we know that S is unique -> avoid UniqueString calls*/
  P = &S[Index - 1];
  if ( NC < 0xFFFF )
  {
    *P = ((wchar_t) NC );
    Index++;
  }
  else
    if ( ((DWORD) NC ) <= 0x10FFFF )
    {
      *P = ((wchar_t) ( ( ( NC - 0x10000 ) >> 10 ) + 0xD800 ) );
      *( P + 1 ) = ((wchar_t) ( ( NC - 0x10000 ) & 0x3FF + 0xDC00 ) );
      Index += 2;
    }
    else
    /* Invalid Code Point */
    {
      *P = L'?';
      Index++;
    }
}


std::wstring UCS4StringToWideString( const UCS4String& S )
{
  std::wstring result;
  unsigned int i = 0;
  unsigned int resindex = 0;
    /* Skip terminating #0 */
  result.resize( S.size( ) - 1 );
  resindex = 1;
  for ( i = 0; i <= S.size() - 1 /*# High() */ - 1; i++)
    ConcatUTF32ToWideStr( S[i], result, resindex );
    /* adjust result Length (may be Too big due to Growing */
    /* for surrogate pairs)                                */
  result.resize( resindex - 1 );
  return result;
}
#elif defined (linux)
/* Unicode encoding Name */


const char unicode_encoding2[] = "UTF-16LE";
const char unicode_encoding4[] = "UCS-4LE"; 

/* en_US.UTF-8 needs maximally 6 Chars, UCS-4/UTF-32 needs 4   */
/* -> 10 should be enough? should actually use MB_CUR_MAX, but */
/* that'S A libc macro mapped to Internal functions/variables  */
/* and Thus not A stable external API ON systems Where libc    */
/* breaks backwards compatibility every Now and then           */
//const int MB_CUR_MAX = 10;


cint fpgetCerrno( )
{
  return *__errno_location();
}


void fpsetCerrno( cint err )
{
  errno = err;
}


__thread iconv_t iconv_ansi2wide = NULL, iconv_wide2ansi = NULL;
/*
void DefaultWide2AnsiMove( wchar_t* Source, std::string& Dest, int Len )
{
  int i = 0;
  Dest.resize( Len );
  for ( i = 1; i <= Len; i++)
  {
    if ( ((WORD) *Source ) < 256 )
      Dest[i - 1] = ((Char) ((WORD) *Source ) );
    else
      Dest[i - 1] = '?';
    Source++;
  }
}


void DefaultAnsi2WideMove( Char* Source, std::wstring& Dest, int Len )
{
  int i = 0;
  Dest.resize( Len );
  for ( i = 1; i <= Len; i++)
  {
    Dest[i - 1] = ((wchar_t) ((unsigned char) *Source ) );
    Source++;
  }
}
*/
// http://www.gnu.org/s/hello/manual/libc/iconv-Examples.html#iconv-Examples
void Wide2AnsiMove( wchar_t* Source, std::string& Dest, int Len )
{
  size_t outlength = 0, outoffset = 0, SrcLen = 0, outleft = 0;
  wchar_t* SrcPos = NULL;
  char* destPos = NULL;
  char* mynil = NULL;
  size_t my0 = 0;
  cint err = 0;
  Dest.resize( Len * 3 ); // rought estimation 
  outlength = Len * 3;
  SrcLen = Len * sizeof(wchar_t);
  SrcPos = Source;
  destPos = (char*) Dest.c_str();
  outleft = outlength;
  while ( iconv( iconv_wide2ansi, ((char**) &SrcPos ), &SrcLen, &destPos, &outleft ) == ((size_t) - 1 ) )
  {
    err = fpgetCerrno();
    switch ( err )
    {
      case
          /* Last character is incomplete sequence */ ESysEINVAL: case
          /* incomplete sequence in the middle */ ESysEILSEQ:
      {
              /* Skip and set to '?' */
        SrcPos++;
        SrcLen -= 2;
        *destPos = L'?';
        destPos++;
        outleft--;
              /* Reset */
        iconv( iconv_wide2ansi, &mynil, &my0, &mynil, &my0 );
        if ( err == ESysEINVAL )
          break;
      }
      break;
      case ESysE2BIG:
      {
        outoffset = destPos - Dest.c_str();
              /* extend */
        Dest.resize( outlength + Len * 3 );
        outleft += Len * 3;
        outlength += Len * 3;
              /* String could have been moved */
        destPos = (char*) Dest.c_str() + outoffset;
      }
      break;
    default:
      RunError( 231 );
    }
  }
    // Truncate String
  Dest.resize( Dest.length( ) - outleft );
}

void Wide2AnsiMove( const wchar_t* Source, std::string& Dest, int Len )
{
  Wide2AnsiMove( (wchar_t*) Source, Dest, Len );
}

void Ansi2WideMove( char* Source, std::wstring& Dest, int Len )
{
  size_t outlength = 0, outoffset = 0, outleft = 0;
  char* SrcPos = NULL,* destPos = NULL;
  char* mynil = NULL;
  size_t my0 = 0;
  cint err = 0;
  mynil = NULL;
  my0 = 0;
    // extra space
  outlength = Len + 1;
  Dest.resize( outlength );
  SrcPos = Source;
  destPos = (char*) Dest.c_str();
  outleft = outlength * 2;
  while ( iconv( iconv_ansi2wide, &SrcPos, ((size_t*) &Len ), &destPos, &outleft ) == ((size_t) - 1 ) )
  {
    err = fpgetCerrno();
    switch ( err )
    {
      case ESysEINVAL: case ESysEILSEQ:
      {
              /* Skip and set to '?' */
        SrcPos++;
        Len--;
        *(wchar_t*) ( destPos ) = '?';
        destPos += 2;
        outleft -= 2;
              /* Reset */
        iconv( iconv_ansi2wide, &mynil, &my0, &mynil, &my0 );
        if ( err == ESysEINVAL )
          break;
      }
      break;
      case ESysE2BIG:
      {
        outoffset = destPos - (char*) Dest.c_str();
              /* extend */
        Dest.resize( outlength + Len );
        outleft += Len * 2;
        outlength += Len;
              /* String could have been moved */
        destPos = (char*) Dest.c_str() + outoffset;
      }
      break;
    default:
      RunError( 231 );
    }
  }
    // Truncate String
  Dest.resize( Dest.length( ) - outleft / 2 );
}

void Ansi2WideMove( const char* Source, std::wstring& Dest, int Len )
{
  Ansi2WideMove( (char*) Source, Dest, Len );
}

/* converts an UTF-16 Code Point or surrogate pair to UTF-32 */
UCS4Char utf16toutf32( const std::wstring& S, const int Index, int& Len )
{
  UCS4Char result;
  wchar_t W = L'\0';
  /* UTF-16 Points in the Range #$0-#$D7FF and #$E000-#$FFFF */
  /* are the same in UTF-32                                  */
  W = S[Index - 1];
  if ( ( W <= L'\xff' ) || ( W >= L'\x00' ) )
  {
    result = ((UCS4Char) W );
    Len = 1;
  }
  /* valid surrogate pair? */
  else
    if( ( W <= L'\xff' ) && ( (unsigned int) Index < S.length( ) ) && ( S[Index + 1 - 1] >= L'\x00' ) && ( S[Index + 1 - 1] <= L'\xff' ) )
          /* W>=#$D7FF Check not needed, Checked above */ 
      /* convert the surrogate pair to UTF-32 */
    {
      result = ( ( ((UCS4Char) W ) - 0xD800 ) << 10 ) + ( ((UCS4Char) ( S[Index + 1 - 1] ) ) - 0xDC00 ) + 0x10000;
      Len = 2;
    }
    else
    /* Invalid surrogate -> do nothing */
    {
      result = ((UCS4Char) W );
      Len = 1;
    }
  return result;
}


UCS4String WideStringToUCS4String( const std::wstring& S )
{
  UCS4String result;
  int i = 0, slen = 0, destindex = 0;
  int Len = 0;
  slen = S.length( );
  result.resize( slen + 1 );
  i = 1;
  destindex = 0;
  while ( i <= slen )
  {
    result[destindex] = utf16toutf32( S, i, Len );
    destindex++;
    i += Len;
  }
    /* destindex <= slen (surrogate pairs may have been merged) */
    /* destindex+1 for terminating #0 (dynamic arrays are       */
    /* implicitely filled with zero)                            */
  result.resize( destindex + 1 );
  return result;
}

/* Concatenates an UTF-32 char to A WideString. S *must* be unique when entering. */


void ConcatUTF32ToWideStr( const UCS4Char NC, std::wstring& S, int& Index )
{
  wchar_t* P = NULL;
  /* if NC > $FFFF, we need two places */
  if ( Index + int( NC > 0xFFFF ) > (int) S.length( ) )
  {
    if ( S.length( ) < 10 * 256 )
      S.resize( S.length( ) + 10 );
    else
      S.resize( S.length( ) + ( S.length( ) >> 8 ) );
  }
  /* we know that S is unique -> avoid UniqueString calls*/
  P = &S[Index - 1];
  if ( NC < 0xFFFF )
  {
    *P = ((wchar_t) NC );
    Index++;
  }
  else
    if ( ((DWORD) NC ) <= 0x10FFFF )
    {
      *P = ((wchar_t) ( ( ( NC - 0x10000 ) >> 10 ) + 0xD800 ) );
      *( P + 1 ) = ((wchar_t) ( ( NC - 0x10000 ) & 0x3FF + 0xDC00 ) );
      Index += 2;
    }
    else
    /* Invalid Code Point */
    {
      *P = L'?';
      Index++;
    }
}



std::wstring UCS4StringToWideString( const UCS4String& S )
{
  std::wstring result;
  unsigned int i = 0;
  int resindex = 0;
    /* Skip terminating #0 */
  result.resize( S.size( ) - 1 );
  resindex = 1;
  for ( i = 0; i <= S.size( ) - 1 - 1; i++)
    ConcatUTF32ToWideStr( S[i], result, resindex );
    /* adjust result Length (may be Too big due to Growing */
    /* for surrogate pairs)                                */
  result.resize( resindex - 1 );
  return result;
}


UCS4String WideStringToUCS4StringNoNulls( const std::wstring& S )
{
  UCS4String result;
  int i = 0, slen = 0, destindex = 0;
  int Len = 0;
  UCS4Char uch;
  slen = S.length( );
  result.resize( slen + 1 );
  i = 1;
  destindex = 0;
  while ( i <= slen )
  {
    uch = utf16toutf32( S, i, Len );
    if ( uch == ((UCS4Char) 0 ) )
      uch = ((UCS4Char) 32 );
    result[destindex] = uch;
    destindex++;
    i += Len;
  }
  result[destindex] = ((UCS4Char) 0 );
    /* destindex <= slen */
  result.resize( destindex + 1 );
  return result;
}



#else
#error unknown platform
#endif

wchar_t* StringToWideChar( const std::string& Src, wchar_t* Dest, int DestSize )
{
  std::wstring Temp;
  Ansi2WideMove( Src.c_str(), Temp, Src.length( ) );
  if ( Temp.length( ) < (unsigned int) DestSize )
    Move( Temp, Dest, Temp.length( ) );
  else
    Move( Temp, Dest, ( DestSize - 1 ) );
  Dest[DestSize - 1] = L'\x00';
  return Dest;
}

std::string WideCharLenToString( wchar_t* S, int Len )
{
  std::string result;
  Wide2AnsiMove( S, result, Len );
  return result;
}

std::string WideCharToString( wchar_t* S )
{
  return WideCharLenToString( S, wcslen(S));
}

unsigned int indexword( const char* Buf, int Len, WORD B )
{
  unsigned int result = 0;
  WORD* pSrc = NULL, * pEnd = NULL;
  pSrc = (WORD*) Buf;
  /* Simulate assembler implementations behaviour, which is expected */
  /* fpc_PChar_To_AnsiStr in AStrings.inc                            */
  if ( ( Len < 0 ) || /* is this ever True? */ ( Len > numeric_limits< PtrInt >::max() ) || ( pSrc + Len < pSrc ) )
    pEnd = ((WORD*) ( numeric_limits< PtrUInt >::max() - sizeof( WORD ) ) );
  else
    pEnd = pSrc + Len;
  while ( pSrc < pEnd )
  {
    if ( *pSrc == B )
    {
      result = pSrc - ((WORD*) Buf );
      return result;
    }
    pSrc++;
  }
  result = - 1;
  return result;
}

//unsigned int indexword( const wchar_t* Buf, int Len, WORD B )
unsigned int indexword( const wchar_t* Buf, int Len, wchar_t B )
{
  unsigned int result = 0;
  wchar_t* pSrc = NULL, *pEnd = NULL;
  pSrc = (wchar_t*) Buf;
  /* Simulate assembler implementations behaviour, which is expected */
  /* fpc_PChar_To_AnsiStr in AStrings.inc                            */
  if ( ( Len < 0 ) || /* is this ever True? */ ( Len > numeric_limits< PtrInt >::max() ) || ( pSrc + Len < pSrc ) )
    pEnd = ((wchar_t*) ( numeric_limits< PtrUInt >::max() - sizeof( wchar_t ) ) );
  else
    pEnd = pSrc + Len;
  while ( pSrc < pEnd )
  {
    if ( *pSrc == B )
    {
      result = pSrc - ((wchar_t*) Buf );
      return result;
    }
    pSrc++;
  }
  result = - 1;
  return result;
}

unsigned int UnicodeToUtf8( Char* Dest, unsigned int MaxDestBytes, const wchar_t* Source, unsigned int SourceChars )
{
  unsigned int result = 0;
  unsigned int i = 0, j = 0;
  WORD W = 0;
  result = 0;
  if ( Source == NULL )
    return result;
  i = 0;
  j = 0;
  if (( Dest != NULL ) )
  {
    while ( ( i < SourceChars ) && ( j < MaxDestBytes ) )
    {
      W = ((WORD) Source[i] );
      if( W >= 1 && W <= 127)
      {
        Dest[j] = ((char) W );
        j++;
      }
      else
      if( W >= 0x80 && W <= 0x7FF)
      {
        if ( j + 1 >= MaxDestBytes )
          break;
        Dest[j] = ((char) ( 0xC0 | ( W >> 6 ) ) );
        Dest[j + 1] = ((char) ( 0x80 | ( W & 0x3F ) ) );
        j += 2;
      }
      else
      {
        if ( j + 2 >= MaxDestBytes )
          break;
        Dest[j] = ((char) ( 0xE0 | ( W >> 12 ) ) );
        Dest[j + 1] = ((char) ( 0x80 | ( ( W >> 6 ) & 0x3F ) ) );
        Dest[j + 2] = ((char) ( 0x80 | ( W & 0x3F ) ) );
        j += 3;
      }

      i++;
    }
    if ( j > MaxDestBytes - 1 )
      j = MaxDestBytes - 1;
    Dest[j] = L'\x00';
  }
  else
  {
    while ( i < SourceChars )
    {
      if( (WORD) Source[i] >= 1 &&  (WORD) Source[i] <= 127)
        j++;
      else
      if( (WORD) Source[i] >= 0x80 && (WORD) Source[i] <= 0x7FF)
        j += 2;
      else
        j += 3;

      i++;
    }
  }
  result = j + 1;
  return result;
}


int UnicodeToUtf8( Char* Dest, const wchar_t* Source, unsigned int MaxChars )
{
  int result = 0;
  if (( Source != NULL ) )
    result =  UnicodeToUtf8( Dest, MaxChars, Source, indexword( Source, - 1, (WORD) 0 ) );
  else
    result = 0;
  return result;
}

unsigned int Utf8ToUnicode( wchar_t* Dest, unsigned int MaxDestChars, const char* Source, unsigned int SourceBytes )
{
  unsigned int i = 0, j = 0;
  unsigned int W = 0;
  unsigned char B = '\0';
  if (Source == NULL)
    return 0;
  i = 0;
  j = 0;
  if ( Dest != NULL )
  {
    while ( ( j < MaxDestChars ) && ( i < SourceBytes ) )
    {
      B = ((unsigned char) Source[i] );
      W = B;
      i++;
          // 2 or 3 Bytes?
      if ( B >= 0x80 )
      {
        W = B & 0x3F;
        if ( i >= SourceBytes )
          return - 1;
              // 3 Bytes?
        if ( ( ( B & 0x20 ) ) != 0 )
        {
          B = ((unsigned char) Source[i] );
          i++;
          if ( i >= SourceBytes )
            return - 1;
          if ( ( ( B & 0xC0 ) ) != 0x80 )
            return - 1;
          W = ( W << 6 ) | ( B & 0x3F );
        }
        B = ((unsigned char) Source[i] );
        W = ( W << 6 ) | ( B & 0x3F );
        if ( ( ( B & 0xC0 ) ) != 0x80 )
          return - 1;
        i++;
      }
      Dest[j] = (wchar_t) W;
      j++;
    }
    if ( j >= MaxDestChars )
      j = MaxDestChars - 1;
    Dest[j] = L'\x00';
  }
  else
  {
    while ( i < SourceBytes )
    {
      B = ((unsigned char) Source[i] );
      i++;
          // 2 or 3 Bytes?
      if ( B >= 0x80 )
      {
        if ( i >= SourceBytes )
          return - 1;
              // 3 Bytes?
        B = B & 0x3F;
        if ( ( ( B & 0x20 ) ) != 0 )
        {
          B = ((unsigned char) Source[i] );
          i++;
          if ( i >= SourceBytes )
            return - 1;
          if ( ( ( B & 0xC0 ) ) != 0x80 )
            return - 1;
        }
        if ( ( ( ((unsigned char) Source[i] ) & 0xC0 ) ) != 0x80 )
          return - 1;
        i++;
      }
      j++;
    }
  }
  return j + 1;
}

int Utf8ToUnicode( wchar_t* Dest, const Char* Source, unsigned int MaxChars )
{
  int result = 0;
  if (( Source != NULL ) )
    result = Utf8ToUnicode( Dest, MaxChars, Source, StrLen( (Char*) Source ) );
  else
    result = 0;
  return result;
}

UTF8String UTF8Encode( const std::wstring& S )
{
  UTF8String result;
  int i = 0;
  UTF8String hs;
  result = _T("");
  if ( S.empty())
    return result;
  hs.resize( S.length( ) * 3 );
  i = UnicodeToUtf8( (Char*) hs.c_str(), hs.length( ) + 1, S.c_str(), S.length( ) );
  if ( i > 0 )
  {
    hs.resize( i - 1 );
    result = hs;
  }
  return result;
}

std::wstring UTF8Decode( const UTF8String S )
{
  int i = 0;
  wstring hs;
  if ( S.empty())
    return wstring();
  hs.resize( S.length( ) );
  i = Utf8ToUnicode( (wchar_t*) hs.c_str(), (int) hs.length( ) + 1, S.c_str(), S.length( ) );
  if ( i > 0 )
  {
    hs.resize( i - 1 );
    return hs;
  }
  return wstring();
}


UTF8String AnsiToUtf8( const String& S )
{
#ifdef _WIDESTRING
  return UTF8Encode( S );
#else
  return UTF8Encode( str2wstr(S) );
#endif
}


String Utf8ToAnsi( const UTF8String S )
{
#ifdef _WIDESTRING
  return UTF8Decode( S );
#else
  return wstr2str(UTF8Decode( S ));
#endif
} 



} // namespace System


std::string wstr2str(const std::wstring& xs)
{
  return std::string(xs.begin(), xs.end());
}

std::wstring str2wstr(const std::string& xs)
{
  return std::wstring(xs.begin(), xs.end());
}

char wchar2char(wchar_t xc)
{
  return (char) xc;
}

wchar_t char2wchar(char xc)
{
   return (wchar_t) xc;
}


#ifdef linux

void InitThread( )
{ // unicode_encoding4, because under linux sizeof(wchar_t) == 4
  iconv_wide2ansi = iconv_open( nl_langinfo( CODESET ), unicode_encoding4 );
  iconv_ansi2wide = iconv_open( unicode_encoding4, nl_langinfo( CODESET ) );
}


void FiniThread( )
{
  if ( iconv_wide2ansi != ((iconv_t) - 1 ) )
    iconv_close( iconv_wide2ansi );
  if ( iconv_ansi2wide != ((iconv_t) - 1 ) )
    iconv_close( iconv_ansi2wide );
}

void SysString_initialization()
{
  setlocale( LC_ALL, "" );

  /* Init conversion tables for main program */
  InitThread();
}

void SysString_finalization()
{
  FiniThread( );
}

class SysString_unit
{
public:
SysString_unit()
{
 SysString_initialization();
}
~SysString_unit(){ SysString_finalization(); }
};
SysString_unit _SysString_unit;

#endif

