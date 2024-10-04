#ifndef d2c_sysfileH
#define d2c_sysfileH

#include "d2c_systypes.h"
#include "dirsep.h"


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

class Currency;

#ifdef linux
#include <dirent.h>
#endif


namespace System
{

struct TFileRec {
  THandle Handle;
  int Mode;
  unsigned int RecSize;
  unsigned char _private [ 28 ];
  unsigned char UserData [ 32 ];
  Char Name [ 260 ];
};

template <typename T>
struct TTypedFile
{
  THandle Handle;
  int Mode;
  unsigned int RecSize;
  unsigned char _private [ 28 ];
  unsigned char UserData [ 32 ];
  Char Name [ 260 ];
};

typedef char TextBufA [ 260 ];

struct TTextRec {
  THandle Handle;
  int Mode;
  unsigned int BufSize;
  unsigned int BufPos;
  unsigned int BufEnd;
  char* BufPtr;
  void* OpenFunc;
  void* InOutFunc;
  void* FlushFunc;
  void* CloseFunc;
  unsigned char UserData [ 32 ];
  Char Name [ 260 ];
  char LineEnd[3];
  TextBufA Buffer;
};

typedef TTypedFile<unsigned char> file;
typedef TTextRec Text;
typedef TTextRec *ptext;

enum TTextLineBreakStyle {tlbsLF, tlbsCRLF, tlbsCR };

extern bool FileNameCaseSensitive;
extern bool CtrlZMarksEOF;
extern TTextLineBreakStyle DefaultTextLineBreakStyle;

#ifndef windows
const Char DirectorySeparator = _T(DIRSEP_CHAR);
const Char DriveSeparator = _T('/');
const Char PathSeparator = _T(';');
//const int maxExitCode = 255;
const int MAXPATHLEN = 4096; // LINUX has always got to be bigger

const int UnusedHandle = - 1;
//const int StdInputHandle = 0;
//const int StdOutputHandle = 1;
//const int StdErrorHandle = 2;
#else
const Char DirectorySeparator = _T(DIRSEP_CHAR);
const Char DriveSeparator = _T(':');
const Char PathSeparator = _T(';');
const int MaxPathLen = 260;

extern THandle UnusedHandle;
extern THandle StdInputHandle;
extern THandle StdOutputHandle;
extern THandle StdErrorHandle;

#endif


const bool LFNSupport = true;
const char ExtensionSeparator = '.';
extern TSet < UChar, 0, 255 > AllowDirectorySeparators;
extern TSet < UChar, 0, 255 > AllowDriveSeparators;

extern bool FileNameCaseSensitive;
extern bool CtrlZMarksEOF;

/* File attributes */
const int fmCreate = 0xffff;
const int faReadOnly = 0x00000001;
const int faHidden = 0x00000002;
const int faSysFile = 0x00000004;
const int faVolumeId = 0x00000008;
const int faDirectory = 0x00000010;
const int faArchive = 0x00000020;
const int faSymLink = 0x00000040;
const int faAnyFile = 0x0000003f;
const int fmOpenRead = 0x0000;
const int fmOpenWrite = 0x0001;
const int fmOpenReadWrite = 0x0002;
const int fmShareCompat = 0x0000;
const int fmShareExclusive = 0x0010;
const int fmShareDenyWrite = 0x0020;
const int fmShareDenyRead = 0x0030;
const int fmShareDenyNone = 0x0040;
const int fsFromBeginning = 0;
const int fsFromCurrent = 1;
const int fsFromEnd = 2;
const THandle feInvalidHandle = ((THandle) - 1 );  //return value on FileOpen error


/* file input modes */
const int fmClosed = 0xD7B0;
const int fmInput = 0xD7B1;
const int fmOutput = 0xD7B2;
const int fmInOut = 0xD7B3;
//const int fmAppend = 0xD7B4;
extern TTextRec ErrOutput, Output, Input, Stdout, Stderr;
extern unsigned char FileMode;
WORD IOResult( );

const int fmAppend = 0xD7B4;   // unknown in C++Builder 6?


typedef void ( * FileFunc )( TTextRec&  );

/* Standard In- and Output */
extern WORD InOutRes;


/****************************************************************************
                          Untyped File Management
****************************************************************************/

void AssignFile( TFileRec& f, const String& Name );
void AssignFile( TFileRec& f, Char c );
void Assign( TFileRec& f, const String& Name );

void Rewrite( TFileRec& f, int l = 128 );
void Reset( TFileRec& f, int l = 128 );
void CloseFile( TFileRec& f );
void Close( TFileRec& f );

void BlockWrite( TTypedFile<unsigned char>& f, void* buf, int64_t Count, int& Result );
void BlockWrite( TTypedFile<unsigned char>& f, void* buf, int Count, int& Result );
void BlockWrite( TTypedFile<unsigned char>& f, void* buf, WORD Count, WORD& Result );
void BlockWrite( TTypedFile<unsigned char>& f, void* buf, unsigned int Count, unsigned int& Result );
void BlockWrite( TTypedFile<unsigned char>& f, void* buf, WORD Count, int& Result );
void BlockWrite( TTypedFile<unsigned char>& f, void* buf, int Count );
void BlockRead( TTypedFile<unsigned char>& f, void* buf, int Count, int& Result );
void BlockRead( TTypedFile<unsigned char>& f, void* buf, int64_t Count );
int64_t FileSize( TFileRec& f ); // FileSize can't be used on a text TTypedFile<unsigned char>.
int64_t FilePos( TFileRec& f, int l = 128 );
void Seek( TTypedFile<unsigned char>& f, int64_t Pos );
void Rename( TTypedFile<unsigned char>& f, const char* P );
void Rename( TTypedFile<unsigned char>& f, const wchar_t* P );
void Rename( TTypedFile<unsigned char>& f, const AnsiString& s );
void Rename( TTypedFile<unsigned char>& f, const WideString& s );
void Rename( TTypedFile<unsigned char>& f, char c );
void Rename( TTypedFile<unsigned char>& f, wchar_t c );
bool Eof( TFileRec& f, int l = 128 );
void Truncate( TFileRec& f, int RecSize );


/****************************************************************************
                           Typed File Management
****************************************************************************/

void fpc_typed_write( int typesize, TFileRec& f, void* buf );
void fpc_typed_read( int typesize, TFileRec& f, void* buf );

//---------------------------------------------------------------------------
template <typename T>
void AssignFile( TTypedFile<T>& f, const String& Name )
{
  AssignFile(*((TFileRec*) &f), Name);
}
//---------------------------------------------------------------------------
template <typename T>
void AssignFile( TTypedFile<T>& f, const Char* P )
{
  AssignFile(*((TFileRec*) &f), P);
}
//---------------------------------------------------------------------------
template <typename T>
void AssignFile( TTypedFile<T>& f, Char c )
{
  AssignFile(*((TFileRec*) &f), c);
}
//---------------------------------------------------------------------------
template <typename T>
int64_t FileSize( TTypedFile<T>& f ) // FileSize can't be used on a text file.
{
  return FileSize(*((TFileRec*) &f));
}
//---------------------------------------------------------------------------
template <typename T>
bool Eof( TTypedFile<T>& f )
{
  return Eof(*((TFileRec*) &f), sizeof(T) );
}
//---------------------------------------------------------------------------
template <typename T>
void CloseFile( TTypedFile<T>& f )
{
  CloseFile(*((TFileRec*) &f));
}
//---------------------------------------------------------------------------
template <typename T>
void Close( TTypedFile<T>& f )
{
  CloseFile(*((TFileRec*) &f));
}
//---------------------------------------------------------------------------
template <typename T>
void Reset( TTypedFile<T>& f, int l = -1 )
{
  if( InOutRes != 0)
   return;
  if(l == -1)
    Reset( *((TFileRec*) &f), sizeof(T) );
  else
    Reset( *((TFileRec*) &f), l );

}
//---------------------------------------------------------------------------
template <typename T>
void Rewrite( TTypedFile<T>& f, int l = -1 )
{
  if( InOutRes != 0)
   return;
  if(l == -1)
    Rewrite(*((TFileRec*) &f), sizeof(T) );
  else
    Rewrite(*((TFileRec*) &f), l );
}
//---------------------------------------------------------------------------
template <typename T>
void Write( TTypedFile<T>& f, void* buf )
{
  fpc_typed_write( f.RecSize, *((TFileRec*)&f), buf );
}
//---------------------------------------------------------------------------
template <typename T>
void Read( TTypedFile<T>& f, void* buf )
{
  fpc_typed_read( f.RecSize, *((TFileRec*)&f), buf );
}

/////////
//---------------------------------------------------------------------------
template <typename T>
void Truncate( TTypedFile<T>& f )
{
  if( InOutRes != 0)
   return;
  Truncate(*((TFileRec*) &f), ((TFileRec*) &f )->RecSize );
}

/****************************************************************************
                            Text File Management
****************************************************************************/


void Assign( TTextRec& f, const String& Name );
void Assign( TTextRec& t, Char c );
void AssignFile( TTextRec& f, const String& Name );
void AssignFile( TTextRec& t, Char c );
void CloseFile( TTextRec& t );
void Close( TTextRec& t );
void Rewrite( TTextRec& t );
void Reset( TTextRec& t );
void Append( TTextRec& t );
void Flush( TTextRec& t );
void Erase( TTextRec& t );
void Rename( TTextRec& t, const char* P );
void Rename( TTextRec& t, const wchar_t* P );
void Rename( TTextRec& t, const AnsiString& s );
void Rename( TTextRec& t, const WideString& s );
void Rename( TTextRec& t, char c );
void Rename( TTextRec& t, wchar_t c );
bool Eof( TTextRec& t );
bool Eof( );
bool EoLn( TTextRec& t );
bool EoLn( );
bool SeekEof( TTextRec& t );
bool SeekEof( );
bool SeekEoLn( TTextRec& t );
bool SeekEoLn( );
void SetTextBuf( TTextRec& t, void* buf, int size );
void SetTextLineEnding(TTextRec& t, String& Ending);


void Write( TTextRec& t, const SmallString<255> s, int Len = 0 );
void Write( TTextRec& t, const char* P, int Len = 0 );
void Write( TTextRec& t, const AnsiString& s, int Len = 0 );
void Write( TTextRec& t, const wchar_t* P, int Len = 0 );
void Write( TTextRec& t, const WideString& s, int Len = 0 );
#ifndef _D2C_SYSFILE_H_LONG_IS_INT64
void Write( TTextRec& t, long l, int Len = 0 );
#endif
void Write( TTextRec& t, int l, int Len = 0 );
void Write( TTextRec& t, unsigned int l, int Len = 0 );
void Write( TTextRec& t, unsigned short l, int Len = 0 );
void Write( TTextRec& t, uint64_t q, int Len = 0 );
void Write( TTextRec& t, int64_t i, int Len = 0 );
void Write( TTextRec& t, long double r, int rt = -1, int fixkomma = -1 );
void Write( TTextRec& t, double r, int rt = -1, int fixkomma = -1 );
void Write( TTextRec& t, Currency c, int fixkomma = -1, int Len = 0 );
void Write( TTextRec& t, bool b, int Len = 0 );
void Write( TTextRec& t, char c, int Len = 0 );
void Write( TTextRec& t, unsigned char c, int Len = 0 );
void Write( TTextRec& t, wchar_t c, int Len = 0 );
void WriteLn( TTextRec& t );
void WriteLn( TTextRec& t, const SmallString<255> s, int Len = 0 );
void WriteLn( TTextRec& t, const char* P, int Len = 0 );
void WriteLn( TTextRec& t, const AnsiString& s, int Len = 0 );
void WriteLn( TTextRec& t, const wchar_t* P, int Len = 0 );
void WriteLn( TTextRec& t, const WideString& s, int Len = 0 );
#ifndef _D2C_SYSFILE_H_LONG_IS_INT64
void WriteLn( TTextRec& t, long l, int Len = 0 );
#endif
void WriteLn( TTextRec& t, int l, int Len = 0 );
void WriteLn( TTextRec& t, unsigned int l, int Len = 0 );
void WriteLn( TTextRec& t, unsigned short l, int Len = 0 );
void WriteLn( TTextRec& t, uint64_t q, int Len = 0 );
void WriteLn( TTextRec& t, int64_t i, int Len = 0 );
void WriteLn( TTextRec& t, long double r, int rt = -1, int fixkomma = -1 );
void WriteLn( TTextRec& t, double r, int rt = -1, int fixkomma = -1 );
void WriteLn( TTextRec& t, Currency c, int fixkomma, int Len = -1 );
void WriteLn( TTextRec& t, bool b, int Len = 0 );
void WriteLn( TTextRec& t, char c, int Len = 0 );
void WriteLn( TTextRec& t, unsigned char c, int Len = 0 );
void WriteLn( TTextRec& t, wchar_t c, int Len = 0 );
void Write( const SmallString<255> s, int Len = 0 );
void Write( const char* P, int Len = 0 );
void Write( const AnsiString& s, int Len = 0 );
#ifndef _D2C_SYSFILE_H_LONG_IS_INT64
void Write( long l, int Len = 0 );
#endif
void Write( int l, int Len = 0 );
void Write( unsigned int l, int Len = 0 );
void Write( uint64_t q, int Len = 0 );
void Write( int64_t i, int Len = 0 );
void Write( long double r, int rt = -1, int fixkomma = -1 );
void Write( double r, int rt = -1, int fixkomma = -1 );
void Write( Currency c, int fixkomma = -1, int Len = 0 );
void Write( bool b, int Len = 0 );
void Write( char c, int Len = 0 );
void WriteLn( );
void WriteLn( const SmallString<255> s, int Len = 0 );
void WriteLn( const char* P, int Len = 0 );
void WriteLn( const AnsiString& s, int Len = 0 );
#ifndef _D2C_SYSFILE_H_LONG_IS_INT64
void WriteLn( long l, int Len = 0 );
#endif
void WriteLn( int l, int Len = 0 );
void WriteLn( unsigned int l, int Len = 0 );
void WriteLn( uint64_t q, int Len = 0 );
void WriteLn( int64_t i, int Len = 0 );
void WriteLn( long double r, int rt = -1, int fixkomma = -1 );
void WriteLn( double r, int rt = -1, int fixkomma = -1 );
void WriteLn( Currency c, int fixkomma = -1, int Len = 0 );
void WriteLn( bool b, int Len = 0 );
void WriteLn( char c, int Len = 0 );
void Write( wchar_t c, int Len  = 0);
void WriteLn( wchar_t c, int Len = 0 );
void Write( const wchar_t* P, int Len = 0 );
void Write( const WideString& s, int Len = 0 );
void WriteLn( const wchar_t* P, int Len = 0 );
void WriteLn( const WideString& s, int Len = 0 );
// ----------
void Read( TTextRec& t, SmallString<255>& s );
void Read( TTextRec& t, char* s, int maxlen = 0x7FFFFFFF );
void Read( TTextRec& t, AnsiString& s );
void Read( TTextRec& t, char& c );
void Read( TTextRec& t, WideString& s );
void Read( TTextRec& t, wchar_t& c );
void Read( TTextRec& t, unsigned int& u );
void Read( TTextRec& t, unsigned short& u );
void Read( TTextRec& t, long double& v );
void Read( TTextRec& f, Currency& v );
void Read( TTextRec& t, double& v );
void Read( TTextRec& t, int64_t& i );
void Read( TTextRec& t, uint64_t& q );
void ReadLn( TTextRec& t);
void ReadLn( TTextRec& t, SmallString<255>& s );
void ReadLn( TTextRec& t, char* s, int maxlen = 0x7FFFFFFF );
void ReadLn( TTextRec& t, AnsiString& s );
void ReadLn( TTextRec& t, char& c );
void ReadLn( TTextRec& t, WideString& s );
void ReadLn( TTextRec& t, wchar_t& c );
void ReadLn( TTextRec& t, int& l );
void ReadLn( TTextRec& t, unsigned int& u );
void ReadLn( TTextRec& t, unsigned short& u );
void ReadLn( TTextRec& t, long double& v );
void ReadLn( TTextRec& f, Currency& v );
void ReadLn( TTextRec& t, double& v );
void ReadLn( TTextRec& t, int64_t& i );
void ReadLn( TTextRec& t, uint64_t& q );
void Read( SmallString<255>& s );
void Read( char* s, int maxlen = 0x7FFFFFFF );
void Read( AnsiString& s );
void Read( char& c );
void Read( WideString& s );
void Read( wchar_t& c );
void Read( int& l );
void Read( unsigned int& u );
void Read( long double& v );
void Read( double& v );
void Read( Currency& v );
void Read( int64_t& i );
void Read( uint64_t& q );
void ReadLn( );
void ReadLn( SmallString<255>& s );
void ReadLn( char* s, int maxlen = 0x7FFFFFFF );
void ReadLn( AnsiString& s );
void ReadLn( char& c );
void ReadLn( WideString& s );
void ReadLn( wchar_t& c );
void ReadLn( int& l );
void ReadLn( unsigned int& u );
void ReadLn( unsigned short& u );
void ReadLn( long double& v );
void ReadLn( double& v );
void ReadLn( Currency& v );
void ReadLn( int64_t& i );
void ReadLn( uint64_t& q );


template <unsigned char sz>
void Write( TTextRec& t, SmallString<sz> s, int Len = 0 )
{
  Write( t, &s[1], s[0] - 1, Len, true );
}
//---------------------------------------------------------------------------
template <unsigned char sz>
void WriteLn( TTextRec& t, SmallString<sz> s, int Len = 0 )
{
  WriteLn( t, &s[1], s[0] - 1, Len, true );
}
//---------------------------------------------------------------------------
template <unsigned char sz>
void Write( SmallString<sz> s, int Len = 0 )
{
  Write( &s[1], s[0] - 1, Len, true );
}
//---------------------------------------------------------------------------
template <unsigned char sz>
void WriteLn( SmallString<sz> s, int Len = 0 )
{
  WriteLn( &s[1], s[0] - 1, Len, true );
}
//---------------------------------------------------------------------------
template <unsigned char sz>
void Read( TTextRec& t, SmallString<sz>& s )
{
  char buf[sz + 1];
  Read( t, buf, sz);
  buf[sz] = '\0';
  s = buf;
}
//---------------------------------------------------------------------------
template <unsigned char sz>
void ReadLn( TTextRec& t, SmallString<sz>& s )
{
  char buf[sz + 1];
  ReadLn( t, buf, sz);
  buf[sz] = '\0';
  s = buf;
}
//---------------------------------------------------------------------------
template <unsigned char sz>
void Read( SmallString<sz>& s )
{
  char buf[sz + 1];
  Read( buf, sz);
  buf[sz] = '\0';
  s = buf;
}
//---------------------------------------------------------------------------
template <unsigned char sz>
void ReadLn( SmallString<sz>& s )
{
  char buf[sz + 1];
  ReadLn( buf, sz);
  buf[sz] = '\0';
  s = buf;
}  

/*****************************************************************************
                            Directory Management
*****************************************************************************/
void ChDir( const std::string& S );
void ChDir( const std::wstring& S );
void RmDir( const std::string& S );
void RmDir( const std::wstring& S );
void MkDir( const std::string& S );
void MkDir( const std::wstring& S );
void GetDir( unsignedchar drivenr, SmallString<255>& Dir );
void GetDir( unsignedchar drivenr, String& Dir );  

#ifdef linux
  /* Directory services */

struct Dir {
  int dd_fd;
  int dd_loc;
  int dd_size;
  dirent* dd_buf;
                /*the following are used in libc, but not in the LINUX kernel Sources ??*/
  unsigned int dd_nextoff;
  int dd_max; /*Size of Buf. irrelevant, as Buf is of type dirent*/
  void* dd_lock;
}; 


//typedef Dir TDir; 
//typedef DIR* DIR*; 

Dir* Fpopendir( const char* DirName );
Dir* Fpopendir( const wchar_t* DirName );
cint Fpclosedir( Dir* dirP );
dirent* Fpreaddir( Dir* dirP );

#endif


} // namespace System

//using namespace System;

#endif //  System
