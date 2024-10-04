
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

#ifndef sysutilsH
#define sysutilsH


#include "d2c_systypes.h"
#include "d2c_syscurr.h"
#include "d2c_sysfile.h"
#include "d2c_varrec.h"
#include "dirsep.h"


namespace Sysutils
{

typedef String TMonthNameArray [ 12/*# range 1..12*/ ];
typedef String TWeekNameArray [ 7/*# range 1..7*/ ];

/* ---------------------------------------------------------------------
    number formatting constants
  ---------------------------------------------------------------------*/

extern Char& DecimalSeparator;  // character that comes between Integer and fractional part of A number 
extern Char& ThousandSeparator;  // character that is Put every 3 numbers in A Currency 
extern Char& ListSeparator;
extern unsigned char& CurrencyDecimals;  // number of Decimals to use when formatting A Currency. 
  /* Format to use when formatting Currency :
    0 = $1
    1 = 1$
    2 = $ 1
    3 = 1 $
    4 = Currency String replaces decimal indicator. e.g. 1$50
   */

extern unsigned char& CurrencyFormat;
  /* same as above, only for Negative currencies:
    0 = ($1)
    1 = -$1
    2 = $-1
    3 = $1-
    4 = (1$)
    5 = -1$
    6 = 1-$
    7 = 1$-
    8 = -1 $
    9 = -$ 1
    10 = $ 1-
   */
extern unsigned char& NegCurrFormat;
extern String& CurrencyString;

/* ---------------------------------------------------------------------
    Date formatting settings
  ---------------------------------------------------------------------*/

extern Char& DateSeparator;  // character to be Put between Date, Month and Year 
extern String& ShortDateFormat;  // Format used for SHORT Date notation 
extern String& LongDateFormat;  // Format used for LONG Date notation 
extern TMonthNameArray& ShortMonthNames;  // SHORT Names of months. 
extern TMonthNameArray& LongMonthNames;  // LONG Names of months. 
extern TWeekNameArray& ShortDayNames;  // SHORT Names of Days
extern TWeekNameArray& LongDayNames;  // full Names of Days 
extern String& ShortTimeFormat;  // Format used for SHORT time notation
extern String& LongTimeFormat;  // Format used for LONG time notation
extern Char& TimeSeparator;  // character to be Put between hours and minutes 
extern String& TimeAMString;  // String to indicate AM time when Using 12 Hour clock. 
extern String& TimePMString;  // String to indicate PM time when Using 12 Hour clock.
extern WORD& TwoDigitYearCenturyWindow; // Threshold to be subtracted from Year before Age-Detection.

    /* Delphi Compat fields*/
struct TSysLocale {
  int DefaultLCID, PriLangID, SubLangID;
  /*# case Byte */
  union {
      /* Win32 Names */
    /*# 1 */
    struct {
    bool FarEast;
    bool MiddleEast;
    };
      /* real meaning */
    /*# 2 */
    struct {
    bool MBCS;
    bool RightToLeft;
    };
  }; //union
};


extern TSysLocale SysLocale;

String GetEnvironmentVariable( const String& EnvVar );
int GetEnvironmentVariableCount( );
String GetEnvironmentString( int Index );
#ifdef linux

  const PtrInt NilHandle = ((TLibHandle) 0 ); 
  TLibHandle SafeLoadLibrary( std::string Name );
  TLibHandle SafeLoadLibrary( std::wstring Name );
  TLibHandle LoadLibrary( std::string Name );
  TLibHandle LoadLibrary( std::wstring Name );
  void* GetProcedureAddress( TLibHandle lib, std::string ProcName );
  void* GetProcedureAddress( TLibHandle lib, std::wstring ProcName );
  bool UnloadLibrary( TLibHandle lib );

  // Kylix/Delphi compability

  bool FreeLibrary( TLibHandle lib );
  void* GetProcAddress( TLibHandle lib, std::string ProcName );
  void* GetProcAddress( TLibHandle lib, std::wstring ProcName );

  // These are for easier crossplatform construction of DLL Names in dynloading libs.
  const Char SharedSuffix[] = _T("so");
  
  void Sleep( unsignedint milliseconds );
#endif
int ExecuteProcess( const String& Path, const String& comline );
int ExecuteProcess( const String& Path, const String* comline, int comline_maxidx );
String GetTempDir( bool Global );
String GetTempDir( );
String GetTempFileName( const String& Dir, const String& prefix );
String GetTempFileName( );
DWORD GetTempFileName( Char* Dir, Char* prefix, DWORD uUnique, Char* TempFileName );
String GetAppConfigDir( bool Global );
String GetAppConfigFile( bool Global );
String GetAppConfigFile( bool Global, bool subdir );
String GetUserDir( );
String VendorName( );
String applicationName( );
extern String ConfigExtension;
extern String SysConfigDir;


typedef String ( * TGetVendorNameEvent )( );
typedef String ( * TGetAppNameEvent )( );
typedef String ( * TGetTempDirEvent )( bool  );
typedef String ( * TGetTempFileEvent )( const String& , const String&  );
extern TGetVendorNameEvent OnGetVendorName;
extern TGetAppNameEvent OnGetApplicationName;
extern TGetTempDirEvent OnGetTempDir;
extern TGetTempFileEvent OnGetTempFile;


   /* for FloatToText */
enum TFloatFormat {ffGeneral,
                   ffExponent,
                   ffFixed,
                   ffNumber,
                   ffCurrency };
enum TFloatValue {fvExtended,
                  fvCurrency,
                  fvSingle,
                  fvReal,
                  fvDouble,
                  fvComp };

enum Sysutils__0 {rfReplaceAll, rfIgnoreCase };
typedef TSet < Sysutils__0, rfReplaceAll, rfIgnoreCase > TReplaceFlags;

struct TFloatRec {
  int Exponent;
  bool Negative;
  Char Digits [ 19/*# range 0..18*/ ];
};
  /* for FloatToDateTime and VariantToDate */


extern Currency MinCurrency;
extern Currency MaxCurrency;
extern TSet < UChar, 0, 255 > LeadBytes;
extern String EmptyStr;
extern PString NullStr;
extern std::wstring EmptyWideStr;
extern std::vector< String > TrueBoolStrs, FalseBoolStrs;


std::string StrPas( const char* str );
std::wstring StrPas( const wchar_t* str );
char* StrPCopy( char* Dest, const std::string& Source );
wchar_t* StrPCopy( wchar_t* Dest, const std::wstring& Source );


//void Delete( SmallString<255>& S, int Index, int Count );
//void Insert( const SmallString<255> Source, SmallString<255>& S, int Index );
//void Insert( char Source, SmallString<255>& S, int Index );
//int Pos( const SmallString<255> Substr, const SmallString<255> S );
//int Pos( char C, const SmallString<255> S );
//int Pos( const SmallString<255> Substr, const std::string& Source );
//void SetString( SmallString<255>& S, char* Buffer, int Len );
//void SetLength( SmallString<255>& S, int Len );
//int Length(const  SmallString<255>& S);
//int Pos( const SmallString<255> Substr, wchar_t C );



//PString NewStr( const String& S );
//void DisposeStr( PString S );
void AssignStr( PString& P, const String& S );
void AppendStr( String& Dest, const String& S );
std::string UpperCase( const std::string& S );
std::wstring UpperCase( const std::wstring& S );
std::string LowerCase( const std::string& S ); 
std::wstring LowerCase( const std::wstring& S );
int CompareStr( const String& S1, const String& S2 );
int CompareMemRange( void* p1, void* p2, unsignedint Length );
bool CompareMem( void* p1, void* p2, unsignedint Length );
int CompareText( const String& S1, const String& S2 );
bool SameText( const String& S1, const String& S2 );
String AnsiUpperCase( const String& S );
String AnsiLowerCase( const String& S );
int AnsiCompareStr( const String& S1, const String& S2 );
int AnsiCompareText( const String& S1, const String& S2 );
bool AnsiSameText( const String& S1, const String& S2 );
bool AnsiSameStr( const String& S1, const String& S2 );
int AnsiStrComp( const Char* S1, const Char* S2 );
int AnsiStrIComp( const Char* S1, const Char* S2 );
int AnsiStrLComp( const Char* S1, const Char* S2, unsignedint MaxLen );
int AnsiStrLIComp( const Char* S1, const Char* S2, unsignedint MaxLen );
Char* AnsiStrLower( Char* str );
Char* AnsiStrUpper( Char* str );
Char* AnsiLastChar( const String& S );
Char* AnsiStrLastChar( Char* str );
String Trim( const String& S );
String TrimLeft( const String& S );
String TrimRight( const String& S );
String QuotedStr( const String& S );
String AnsiQuotedStr( const String& S, Char Quote );
String AnsiDequotedStr( const String& S, Char AQuote );
String AnsiExtractQuotedStr( Char*& Src, Char Quote );
String AdjustLineBreaks( const String& S );
String AdjustLineBreaks( const String& S, System::TTextLineBreakStyle Style );
bool IsValidIdent( const String& Ident );
#define IntToStr(x) std::to_string(x)
String IntToHex( int Value, int Digits );
String IntToHex( int64_t Value, int Digits );
String IntToHex( QWord Value, int Digits );
int StrToInt( const String& S );
bool TryStrToInt( const String& S, int& i );
int64_t StrToInt64( const String& S );
bool TryStrToInt64( const String& S, int64_t& i );
QWord StrToQWord( const String& S );
bool TryStrToQWord( const String& S, QWord& Q );
int StrToIntDef( const String& S, int deflt );
int64_t StrToInt64Def( const String& S, int64_t deflt );
QWord StrToQWordDef( const String& S, QWord deflt );
//String LoadStr( int Ident );
// function FmtLoadStr(Ident: Integer; const Args: array of const): String;


String Format( const String& fmt, const  VECTOROFCONST& Args );
unsignedint FormatBuf( wchar_t* Buffer, unsignedint BufLen, const wchar_t* fmt, unsignedint FmtLen, const  VECTOROFCONST& Args );
Char* StrFmt( Char* Buffer, const Char* fmt, const  VECTOROFCONST& Args );
Char* StrLFmt( Char* Buffer, unsignedint MaxLen, const Char* fmt, const  VECTOROFCONST& Args );
void FmtStr( String& RES, const String& fmt, const  VECTOROFCONST& Args );

String FloatToStrF( long double Value, TFloatFormat Format, int Precision, int Digits );
String FloatToStrF( double Value, TFloatFormat Format, int Precision, int Digits );
String FloatToStrF( float Value, TFloatFormat Format, int Precision, int Digits );
String FloatToStrF( int64_t Value, TFloatFormat Format, int Precision, int Digits );
String FloatToStrF( Currency Value, TFloatFormat Format, int Precision, int Digits );
String FloatToStrF( int64_t Value, TFloatFormat Format, int Precision, int Digits );
String CurrToStrF( Currency Value, TFloatFormat Format, int Digits );
String FloatToStr( long double Value );
String FloatToStr( double Value );
String FloatToStr( float Value );
String FloatToStr( int Value );
String FloatToStr( Currency Value );
String FloatToStr( int64_t Value );
long double StrToFloat( const String& S );
long double StrToFloatDef( const String& S, const long double deflt );
bool TryStrToFloat( const String& S, float& Value );
bool TryStrToFloat( const String& S, double& Value );
bool TextToFloat( const Char* Buffer, long double& Value );
bool TextToFloat( const Char* Buffer, void* Value, TFloatValue ValueType );
int FloatToText( Char* Buffer, long double Value, TFloatFormat Format, int Precision, int Digits );
bool StrToBool( const String& S );
String BoolToStr( bool B, bool UseBoolStrs = false );
bool StrToBoolDef( const String& S, bool deflt );
bool TryStrToBool( const String& S, bool& Value );
int LastDelimiter( const String& Delimiters, const String& S );
String StringReplace( const String& S, const String& OldPattern, const String& NewPattern, TReplaceFlags Flags );
int FloatToTextFmt( Char* Buffer, long double Value, Char* Format );
void FloatToDecimal( TFloatRec& result, void* Value, TFloatValue ValueType, int Precision, int Decimals );
void FloatToDecimal( TFloatRec& result, long double Value, int Precision, int Decimals );
String FormatFloat( const String& Format, long double Value );
bool IsDelimiter( const String& Delimiters, const String& S, int Index );
String FormatCurr( const String& Format, Currency Value );
int SScanf( const String& S, const String& fmt, const void** pointers, int pointers_maxidx );

/*// MBCS functions. no MBCS yet, So mostly These are calls to the regular counterparts.*/


enum TMbcsByteType {mbSingleByte,
                    mbLeadByte,
                    mbTrailByte };
TMbcsByteType ByteType( const String& S, int Index );
TMbcsByteType StrByteType( Char* str, unsignedint Index );
int ByteToCharLen( const String& S, int MaxLen );
int CharToByteLen( const String& S, int MaxLen );
int ByteToCharIndex( const String& S, int Index );
int StrCharLength( const char* str );
const char* StrNextChar( const char* str );
const TSet < UChar, 0, 255> SwitchChars = ( TSet < UChar, 0, 255 >() << _T('/') << _T('-') );


typedef TSet < UChar, 0, 255 > TSysCharSet;
typedef TSysCharSet *PSysCharSet;
bool FindCmdLineSwitch( const String& Switch, const TSysCharSet Chars, bool IgnoreCase );
bool FindCmdLineSwitch( const String& Switch, bool IgnoreCase );
bool FindCmdLineSwitch( const String& Switch );
String WrapText( const String& Line, const String& BreakStr, const TSysCharSet BreakChars, int MaxCol );
String WrapText( const String& Line, int MaxCol );
int LastDelimiter( const String& Delimiters, const String& S );


typedef String TFileName;
typedef TSet < int/*# range sizeof(int)*8-1-0+1*/, 0, 255 > TIntegerSet;

#pragma pack(push, 1)
struct LongRec {
  /*# case Integer */
  union {
    /*# 0 */
    struct {
    WORD Lo, Hi;
    };
    /*# 1 */
    struct {
    unsigned char Bytes [ 4/*# range 0..3*/ ];
    };
  }; //union
};
#pragma pack(pop)


#pragma pack(push, 1)
struct WordRec {
  unsigned char Lo, Hi;
};
#pragma pack(pop)


#pragma pack(push, 1)
struct Int64Rec {
  /*# case Integer */
  union {
    /*# 0 */
    struct {
    unsigned int Lo, Hi;
    };
    /*# 1 */
    struct {
    WORD Words [ 4/*# range 0..3*/ ];
    };
    /*# 2 */
    struct {
    unsigned char Bytes [ 8/*# range 0..7*/ ];
    };
  }; //union
};
#pragma pack(pop)


typedef unsigned char *PByteArray;
typedef unsigned char TByteArray [ 32768/*# range 0..32767*/ ];
typedef WORD *PWordArray;
typedef WORD TWordArray [ 16384/*# range 0..16383*/ ];



extern String HexDisplayPrefix;

const char PathDelim = DIRSEP_CHAR;/*System.*/
#ifdef windows
const char DriveDelim = ':';/*System.*/
const char PathSep = ';';/*System.*/
// in WinDef.h const int MAX_PATH = MAXPATHLEN;
#else
const char DriveDelim = '/';/*System.*/
const char PathSep = ':';/*System.*/
const int MAX_PATH = MAXPATHLEN;
extern char** envp;
#endif


  /* read PChar handling functions declaration */
  /*
    *********************************************************************
    Copyright (C) 1997, 1998 Gertjan Schouten

    this program is Free Software; you can redistribute it and/or modify
    it under the terms of the GNU general public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at Your option) Any later version.

    this program is distributed in the hope that it will be useful,
    but without Any WARRANTY; without even the implied WARRANTY of
    MERCHANTABILITY or FITNESS for A particular purpose.  see the
    GNU general public License for more details.

    you should have received A copy of the GNU general public License
    along with this program; if not, write to the Free Software
    Foundation, inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************

    System Utilities for Free p4ascal
*/

/* shared with Strings unit */
unsigned int StrLen( char* P );
unsigned int StrLen( wchar_t* P );
wchar_t* StrCopy( wchar_t* Dest, const wchar_t* Source );
char* StrCopy( char* Dest, const char* Source );
wchar_t* StrLCopy( wchar_t* Dest, const wchar_t* Source, int MaxLen );
char* StrLCopy( char* Dest, const char* Source, int MaxLen );
wchar_t* StrECopy( wchar_t* Dest, const wchar_t* Source );
char* StrECopy( char* Dest, const char* Source );
wchar_t* StrEnd( wchar_t* P );
char* StrEnd( char* P );
wchar_t* StrCat( wchar_t* Dest, const wchar_t* Source );
char* StrCat( char* Dest, const char* Source );

int StrComp( const char* Str1, const char* Str2 );
int StrComp( const wchar_t* Str1, const wchar_t* Str2 );
int StrLComp( const char* Str1, const char* Str2, int l );
int StrLComp( const wchar_t* Str1, const wchar_t* Str2, int l );
int StrIComp( const char* Str1, const char* Str2 );
int StrIComp( const wchar_t* Str1, const wchar_t* Str2 );
int StrLIComp( const char* Str1, const char* Str2, int l );
int StrLIComp( const wchar_t* Str1, const wchar_t* Str2, int l );


char* StrMove( char* Dest, const char* Source, int l );
wchar_t* StrMove( wchar_t* Dest, const wchar_t* Source, int l );
char* StrLCat( char* Dest, const char* Source, int l );
wchar_t* StrLCat( wchar_t* Dest, const wchar_t* Source, int l );
char* StrScan( char* P, char C );
wchar_t* StrScan( wchar_t* P, wchar_t C );
char* StrRScan( char* P, char C );
wchar_t* StrRScan( wchar_t* P, wchar_t C );

Char* StrLower( Char* P );
Char* StrUpper( Char* P );
Char* StrPos( Char* Str1, Char* Str2 );
Char* StrNew( Char* P );

/* different from Strings unit - AnsiStrings or different behaviour /
wchar_t* StrPCopy( wchar_t* Dest, const String& Source );
wchar_t* StrPLCopy( wchar_t* Dest, const String& Source, SizeUInt MaxLen );
wchar_t* StrAlloc( unsignedint Size );
SizeUInt StrBufSize( wchar_t* str );
void StrDispose( wchar_t* str );
*/
  /* MCBS functions */
  /*
    *********************************************************************
    Copyright (C) 2002 by Florian Klaempfl

    this program is Free Software; you can redistribute it and/or modify
    it under the terms of the GNU general public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at Your option) Any later version.

    this program is distributed in the hope that it will be useful,
    but without Any WARRANTY; without even the implied WARRANTY of
    MERCHANTABILITY or FITNESS for A particular purpose.  see the
    GNU general public License for more details.

    you should have received A copy of the GNU general public License
    along with this program; if not, write to the Free Software
    Foundation, inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************
*/
int AnsiCompareFileName( const String& S1, const String& S2 );
String AnsiLowerCaseFileName( const String& S );
String AnsiUpperCaseFileName( const String& S );
int AnsiPos( const String& Substr, const String& S );
Char* AnsiStrPos( Char* str, Char* Substr );
Char* AnsiStrRScan( Char* str, Char Chr );
Char* AnsiStrScan( Char* str, Char Chr );

  /* wide String functions */
  /*
    *********************************************************************
    Copyright (C) 2002 by Florian Klaempfl

    this program is Free Software; you can redistribute it and/or modify
    it under the terms of the GNU general public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at Your option) Any later version.

    this program is distributed in the hope that it will be useful,
    but without Any WARRANTY; without even the implied WARRANTY of
    MERCHANTABILITY or FITNESS for A particular purpose.  see the
    GNU general public License for more details.

    you should have received A copy of the GNU general public License
    along with this program; if not, write to the Free Software
    Foundation, inc., 675 Mass Ave, Cambridge, MA 02139, USA.
    *********************************************************************
*/

std::wstring WideUpperCase( const std::wstring& S );
std::wstring WideLowerCase( const std::wstring& S );
PtrInt WideCompareStr( const std::wstring& S1, const std::wstring& S2 );
bool WideSameStr( const std::wstring& S1, const std::wstring& S2 );
PtrInt WideCompareText( const std::wstring& S1, const std::wstring& S2 );
bool WideSameText( const std::wstring& S1, const std::wstring& S2 );
std::wstring WideFormat( const std::wstring& fmt, const  VECTOROFCONST& Args );
unsignedint WideFormatBuf( void* Buffer, unsignedint BufLen, void* fmt, unsignedint FmtLen, const  VECTOROFCONST& Args );
void WideFmtStr( std::wstring& RES, const std::wstring& fmt, const  VECTOROFCONST& Args );


String ChangeFileExt( const String& Filename, const String& Extension );
String ExtractFilePath( const String& Filename );
String ExtractFileDrive( const String& Filename );
std::string ExtractFileName( const std::string& Filename );
std::wstring ExtractFileName( const std::wstring& Filename );
String ExtractFileExt( const String& Filename );
String ExtractFileDir( const String& Filename );
String ExtractRelativePath( const String& BaseName, const String& DestName );
#ifdef windows
String ExtractShortPathName( const String& Filename );
#endif
String ExpandFileName( const String& Filename );
String ExpandUNCFileName( const String& Filename );
String ExtractRelativePath( const String& BaseName, const String& DestName );
String IncludeTrailingPathDelimiter( const String& Path );
String IncludeTrailingBackslash( const String& Path );
String ExcludeTrailingBackslash( const String& Path );
String ExcludeTrailingPathDelimiter( const String& Path );
bool IsPathDelimiter( const String& Path, int Index );
int GetDirs( String& DirName, Char** dirs, int dirs_maxidx );
bool SameFileName( const String& S1, const String& S2 );

#ifdef windows
#ifdef _WIDESTRING
  typedef WIN32_FIND_DATAW TWin32FindData;  
#else
 typedef WIN32_FIND_DATAA TWin32FindData;  
#endif
#endif

struct TSearchRec {
  int Time;
  int64_t Size;
  int Attr;
  TFileName Name;
  int ExcludeAttr; 
  #ifdef unix
  void* FindHandle;
  mode_t Mode;
  AnsiString PathOnly; 
  #else
  THandle FindHandle;   
  #endif
  #ifdef windows
  TWin32FindData FindData;  
  #endif
};


  /* File errors */
extern THandle feInvalidHandle;
THandle FileOpen( const std::string& Filename, int Mode );
THandle FileOpen( const std::wstring& Filename, int Mode );
THandle FileCreate( const std::string& Filename );
THandle FileCreate( const std::string& Filename, int Mode );
THandle FileCreate( const std::wstring& Filename );
THandle FileCreate( const std::wstring& Filename, int Mode );
int FileRead( THandle Handle, void* Buffer, int Count );
int FileWrite( THandle Handle, void* Buffer, int Count );
int FileSeek( THandle Handle, int FOffset, int Origin );
int64_t FileSeek( THandle Handle, int64_t FOffset, int Origin );
void FileClose( THandle Handle );
int FileAge( const std::string& Filename );
int FileAge( const std::wstring& Filename );
bool FileExists( const std::string& Filename );
bool FileExists( const std::wstring& Filename );
bool DirectoryExists( const std::string& Directory );
bool DirectoryExists( const std::wstring& Directory );
int FindFirst( const std::string& Path, int Attr, TSearchRec& Rslt );
int FindFirst( const std::wstring& Path, int Attr, TSearchRec& Rslt );
int FindNext( TSearchRec& Rslt );
void FindClose( TSearchRec& F );
int FileGetDate( THandle Handle );
int FileSetDate( THandle Handle, int Age );
int FileSetDate( const std::string& Filename, int Age );
int FileSetDate( const std::wstring& Filename, int Age );
int FileGetAttr( const std::string& Filename );
int FileGetAttr( const std::wstring& Filename );
int FileSetAttr( const std::string& Filename, int Attr );
int FileSetAttr( const std::wstring& Filename, int Attr );
bool DeleteFile( const std::string& Filename );
bool DeleteFile( const std::wstring& Filename );
bool RenameFile( const std::string& OldName, const String& newName );
bool RenameFile( const std::wstring& OldName, const String& newName );
std::string FileSearch( const std::string& Name, const std::string& DirList, bool ImplicitCurrentDir = true );
std::wstring FileSearch( const std::wstring& Name, const std::wstring& DirList, bool ImplicitCurrentDir = true );
std::string ExeSearch( const std::string& Name, const std::string& DirList );
std::wstring ExeSearch( const std::wstring& Name, const std::wstring& DirList );
bool FileIsReadOnly( const std::string& Filename );
bool FileIsReadOnly( const std::wstring& Filename );

int64_t DiskFree( unsignedchar Drive );
int64_t DiskSize( unsignedchar Drive );
String GetCurrentDir( );
bool SetCurrentDir( const String& newdir );
bool CreateDir( const String& newdir );
bool RemoveDir( const String& Dir );
bool ForceDirectories( const String& Dir );


void FreeAndNil( void*& Obj );


//HMODULE SafeLoadLibrary( const std::string& Filename, DWORD ErrorMode = 0 );
  
/* some packages and unit related constants for compatibility */
const int pfExeModule = 0x00000000;
const int pfNeverBuild = 0x00000001;
const int pfDesignOnly = 0x00000002;
const int pfRunOnly = 0x00000004;
const int pfIgnoreDupUnits = 0x00000008;
const int pfPackageModule = 0x40000000;
const int pfModuleTypeMask = 0xC0000000;
const int pfV3Produced = 0x00000000;
const int pfProducerUndefined = 0x04000000;
const int pfBCB4Produced = 0x08000000;
const int pfDelphi4Produced = 0x0C000000;
const int pfLibraryModule = 0x80000000;
const int pfProducerMask = 0x0C000000;
const int ufMainUnit = 0x01;
const int ufPackageUnit = 0x02;
const int ufWeakUnit = 0x04;
const int ufOrgWeakUnit = 0x08;
const int ufImplicitUnit = 0x10;
const int ufWeakPackageUnit = ufPackageUnit | ufWeakUnit;

extern int Win32Platform;
extern DWORD Win32MajorVersion, Win32MinorVersion, Win32BuildNumber;
extern SmallString<255> Win32CSDVersion;   // csd record is 128 Bytes only?

/* compatibility with Delphi */

bool Win32Check( bool RES );
bool CheckWin32Version( int major, int minor );
bool CheckWin32Version( int major );
void RaiseLastWin32Error( );
// not in Sysutils unsignedint GetFileVersion( const String& AFileName );
void GetFormatSettings( );

void Beep();
String AdjustPath(const String &S);

}  // namespace Sysutils


using namespace Sysutils;

#endif //  sysutilsH
