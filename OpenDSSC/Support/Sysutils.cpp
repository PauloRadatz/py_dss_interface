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



#include "Sysutils.h"

#include "d2c_sysconst.h"
#include "d2c_system.h"
#include "d2c_sysmath.h"
#include "d2c_sysexcept.h"
#include "d2c_sysstring.h"
#include <sstream>
#include <iomanip>
#include <algorithm>
#include "dirsep.h"

using namespace System;


#ifdef linux
#include <unistd.h>
#include <sys/stat.h>
#include <sys/statfs.h>

#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <dirent.h>
#include <utime.h>
#include <langinfo.h>
#include <locale.h>
#include <dlfcn.h>
#include <iostream>
#include <cerrno>
#include <linux/unistd.h> // exit_group
/* Because exit_group() appears to not be declared in linux/unistd.h */
#include <sys/syscall.h>   /* For SYS_exit_group definition */
#endif

#include <complex>
using namespace std;
using namespace System;


namespace Sysutils
{

#define Sysutils__10 ( TSet < UChar, 0, 255 >() \
                     << _T('A') << _T('B') << _T('C') << _T('D') << _T('E') << _T('F') << _T('G') << _T('H') << _T('I') << _T('J') \
                     << _T('K') << _T('L') << _T('M') << _T('N') << _T('O') << _T('P') << _T('Q') << _T('R') << _T('S') << _T('T') \
                     << _T('U') << _T('V') << _T('W') << _T('X') << _T('Y') << _T('Z') << _T('a') << _T('b') << _T('c') << _T('d') \
                     << _T('e') << _T('f') << _T('g') << _T('h') << _T('i') << _T('j') << _T('k') << _T('l') << _T('m') << _T('n') \
                     << _T('o') << _T('p') << _T('q') << _T('r') << _T('s') << _T('t') << _T('u') << _T('v') << _T('w') << _T('x') \
                     << _T('y') << _T('z') << _T('_') )
#define Sysutils__11 ( TSet < UChar, 0, 255 >() \
                     << _T('A') << _T('B') << _T('C') << _T('D') << _T('E') << _T('F') << _T('G') << _T('H') << _T('I') << _T('J') \
                     << _T('K') << _T('L') << _T('M') << _T('N') << _T('O') << _T('P') << _T('Q') << _T('R') << _T('S') << _T('T') \
                     << _T('U') << _T('V') << _T('W') << _T('X') << _T('Y') << _T('Z') << _T('a') << _T('b') << _T('c') << _T('d') \
                     << _T('e') << _T('f') << _T('g') << _T('h') << _T('i') << _T('j') << _T('k') << _T('l') << _T('m') << _T('n') \
                     << _T('o') << _T('p') << _T('q') << _T('r') << _T('s') << _T('t') << _T('u') << _T('v') << _T('w') << _T('x') \
                     << _T('y') << _T('z') << _T('0') << _T('1') << _T('2') << _T('3') << _T('4') << _T('5') << _T('6') << _T('7') \
                     << _T('8') << _T('9') << _T('_') )
#define Sysutils__12 ( TSet < UChar, 0, 255 >() \
                     << _T('0') << _T('1') << _T('2') << _T('3') << _T('4') << _T('5') << _T('6') << _T('7') << _T('8') << _T('9') )
#define Sysutils__13 ( TSet < int, 0, 255 >() \
                     << int ( 1 ) << int ( 3 ) )
#define Sysutils__14 ( TSet < UChar, 0, 255 >() \
                     << _T('+') << _T('-') )
#define Sysutils__15 ( TSysCharSet () \
                     << _T(' ') << _T('-') << _T('\t') )
#define Sysutils__16 ( TSet < UChar, 0, 255 >() \
                     << _T('+') << _T('-') )
#define Sysutils__17 ( TSet < UChar, 0, 255 >() \
                     << _T('0') << _T('1') << _T('2') << _T('3') << _T('4') << _T('5') << _T('6') << _T('7') << _T('8') << _T('9') )
#define Sysutils__18 ( TSet < UChar, 0, 255 >() \
                     << _T('0') << _T('1') << _T('2') << _T('3') << _T('4') << _T('5') << _T('6') << _T('7') << _T('8') << _T('9') \
                     << _T('+') << _T('-') << _T('.') << _T('e') << _T('E') )
#define Sysutils__2 ( TSet < UChar, 0, 255 >() \
                     << _T('.') )
#define Sysutils__3 ( TSet < UChar, 0, 255 >() \
                     << Char ( '.' ) )
#define Sysutils__4 ( TSet < UChar, 0, 255 >() \
                     << _T('A') << _T('B') << _T('C') << _T('D') << _T('E') << _T('F') << _T('G') << _T('H') << _T('I') << _T('J') \
                     << _T('K') << _T('L') << _T('M') << _T('N') << _T('O') << _T('P') << _T('Q') << _T('R') << _T('S') << _T('T') \
                     << _T('U') << _T('V') << _T('W') << _T('X') << _T('Y') << _T('Z') << _T('a') << _T('b') << _T('c') << _T('d') \
                     << _T('e') << _T('f') << _T('g') << _T('h') << _T('i') << _T('j') << _T('k') << _T('l') << _T('m') << _T('n') \
                     << _T('o') << _T('p') << _T('q') << _T('r') << _T('s') << _T('t') << _T('u') << _T('v') << _T('w') << _T('x') \
                     << _T('y') << _T('z') )
#define Sysutils__5 ( TSet < UChar, 0, 255 >() \
                     << _T('a') << _T('b') << _T('c') << _T('d') << _T('e') << _T('f') << _T('g') << _T('h') << _T('i') << _T('j') \
                     << _T('k') << _T('l') << _T('m') << _T('n') << _T('o') << _T('p') << _T('q') << _T('r') << _T('s') << _T('t') \
                     << _T('u') << _T('v') << _T('w') << _T('x') << _T('y') << _T('z') )
#define Sysutils__6 ( TSet < UChar, 0, 255 >() \
                     << _T('a') << _T('b') << _T('c') << _T('d') << _T('e') << _T('f') << _T('g') << _T('h') << _T('i') << _T('j') \
                     << _T('k') << _T('l') << _T('m') << _T('n') << _T('o') << _T('p') << _T('q') << _T('r') << _T('s') << _T('t') \
                     << _T('u') << _T('v') << _T('w') << _T('x') << _T('y') << _T('z') )
#define Sysutils__7 ( TSet < UChar, 0, 255 >() \
                     << _T('A') << _T('B') << _T('C') << _T('D') << _T('E') << _T('F') << _T('G') << _T('H') << _T('I') << _T('J') \
                     << _T('K') << _T('L') << _T('M') << _T('N') << _T('O') << _T('P') << _T('Q') << _T('R') << _T('S') << _T('T') \
                     << _T('U') << _T('V') << _T('W') << _T('X') << _T('Y') << _T('Z') )
#define Sysutils__8 ( TSet < int, 0, 255 >() \
                     << int ( 97 ) << int ( 98 ) << int ( 99 ) << int ( 100 ) << int ( 101 ) << int ( 102 ) << int ( 103 ) << int ( 104 ) << int ( 105 ) << int ( 106 ) \
                     << int ( 107 ) << int ( 108 ) << int ( 109 ) << int ( 110 ) << int ( 111 ) << int ( 112 ) << int ( 113 ) << int ( 114 ) << int ( 115 ) << int ( 116 ) \
                     << int ( 117 ) << int ( 118 ) << int ( 119 ) << int ( 120 ) << int ( 121 ) << int ( 122 ) )
#define Sysutils__9 ( TSet < int, 0, 255 >() \
                     << int ( 97 ) << int ( 98 ) << int ( 99 ) << int ( 100 ) << int ( 101 ) << int ( 102 ) << int ( 103 ) << int ( 104 ) << int ( 105 ) << int ( 106 ) \
                     << int ( 107 ) << int ( 108 ) << int ( 109 ) << int ( 110 ) << int ( 111 ) << int ( 112 ) << int ( 113 ) << int ( 114 ) << int ( 115 ) << int ( 116 ) \
                     << int ( 117 ) << int ( 118 ) << int ( 119 ) << int ( 120 ) << int ( 121 ) << int ( 122 ) )
struct TPRecord;



struct TFormatSettings {
  unsigned char CurrencyFormat;
  unsigned char NegCurrFormat;
  Char ThousandSeparator;
  Char DecimalSeparator;
  unsigned char CurrencyDecimals;
  Char DateSeparator;
  Char TimeSeparator;
  Char ListSeparator;
  String CurrencyString;
  String ShortDateFormat;
  String LongDateFormat;
  String TimeAMString;
  String TimePMString;
  String ShortTimeFormat;
  String LongTimeFormat;
  TMonthNameArray ShortMonthNames;
  TMonthNameArray LongMonthNames;
  TWeekNameArray ShortDayNames;
  TWeekNameArray LongDayNames;
  WORD TwoDigitYearCenturyWindow;
};

TFormatSettings DefaultFormatSettings;


void DefaultFormatSettingsInit( )
{
  DefaultFormatSettings.CurrencyFormat = 1;
  DefaultFormatSettings.NegCurrFormat = 5;
  DefaultFormatSettings.ThousandSeparator = _T(',');
  DefaultFormatSettings.DecimalSeparator = _T('.');
  DefaultFormatSettings.CurrencyDecimals = 2;
  DefaultFormatSettings.DateSeparator = _T('-');
  DefaultFormatSettings.TimeSeparator = _T(':');
  DefaultFormatSettings.ListSeparator = _T(',');
  DefaultFormatSettings.CurrencyString = _T('$');
  DefaultFormatSettings.ShortDateFormat = _T("d/m/y");
  DefaultFormatSettings.LongDateFormat = _T("dd\" \"mmmm\" \"yyyy");
  DefaultFormatSettings.TimeAMString = _T("AM");
  DefaultFormatSettings.TimePMString = _T("PM");
  DefaultFormatSettings.ShortTimeFormat = _T("hh:mm"); // _T("hh:nn");
  DefaultFormatSettings.LongTimeFormat = _T("hh:mm:ss"); // _T("hh:nn:ss");
  DefaultFormatSettings.ShortMonthNames[0] = _T("Jan");
  DefaultFormatSettings.ShortMonthNames[1] = _T("Feb");
  DefaultFormatSettings.ShortMonthNames[2] = _T("Mar");
  DefaultFormatSettings.ShortMonthNames[3] = _T("Apr");
  DefaultFormatSettings.ShortMonthNames[4] = _T("May");
  DefaultFormatSettings.ShortMonthNames[5] = _T("Jun");
  DefaultFormatSettings.ShortMonthNames[6] = _T("Ju_T(");
  DefaultFormatSettings.ShortMonthNames[7] = _T("Aug");
  DefaultFormatSettings.ShortMonthNames[8] = _T("Sep");
  DefaultFormatSettings.ShortMonthNames[9] = _T("Oct");
  DefaultFormatSettings.ShortMonthNames[10] = _T("Nov");
  DefaultFormatSettings.ShortMonthNames[11] = _T("Dec");
  DefaultFormatSettings.LongMonthNames[0] = _T("January");
  DefaultFormatSettings.LongMonthNames[1] = _T("February");
  DefaultFormatSettings.LongMonthNames[2] = _T("March");
  DefaultFormatSettings.LongMonthNames[3] = _T("Apri_T(");
  DefaultFormatSettings.LongMonthNames[4] = _T("May");
  DefaultFormatSettings.LongMonthNames[5] = _T("June");
  DefaultFormatSettings.LongMonthNames[6] = _T("July");
  DefaultFormatSettings.LongMonthNames[7] = _T("August");
  DefaultFormatSettings.LongMonthNames[8] = _T("September");
  DefaultFormatSettings.LongMonthNames[9] = _T("October");
  DefaultFormatSettings.LongMonthNames[10] = _T("November");
  DefaultFormatSettings.LongMonthNames[11] = _T("December");
  DefaultFormatSettings.ShortDayNames[0] = _T("Sun");
  DefaultFormatSettings.ShortDayNames[1] = _T("Mon");
  DefaultFormatSettings.ShortDayNames[2] = _T("Tue");
  DefaultFormatSettings.ShortDayNames[3] = _T("Wed");
  DefaultFormatSettings.ShortDayNames[4] = _T("Thu");
  DefaultFormatSettings.ShortDayNames[5] = _T("Fri");
  DefaultFormatSettings.ShortDayNames[6] = _T("Sat");
  DefaultFormatSettings.LongDayNames[0] = _T("Sunday");
  DefaultFormatSettings.LongDayNames[1] = _T("Monday");
  DefaultFormatSettings.LongDayNames[2] = _T("Tuesday");
  DefaultFormatSettings.LongDayNames[3] = _T("Wednesday");
  DefaultFormatSettings.LongDayNames[4] = _T("Thursday");
  DefaultFormatSettings.LongDayNames[5] = _T("Friday");
  DefaultFormatSettings.LongDayNames[6] = _T("Saturday");
  DefaultFormatSettings.TwoDigitYearCenturyWindow = 50;
}

Char& DecimalSeparator = DefaultFormatSettings.DecimalSeparator; 
Char& ThousandSeparator = DefaultFormatSettings.ThousandSeparator; 
Char& ListSeparator = DefaultFormatSettings.ListSeparator; 
unsigned char& CurrencyDecimals = DefaultFormatSettings.CurrencyDecimals;
unsigned char& CurrencyFormat = DefaultFormatSettings.CurrencyFormat;
unsigned char& NegCurrFormat = DefaultFormatSettings.NegCurrFormat;
String& CurrencyString = DefaultFormatSettings.CurrencyString;

Char& DateSeparator = DefaultFormatSettings.DateSeparator;
String& ShortDateFormat = DefaultFormatSettings.ShortDateFormat;
String& LongDateFormat =DefaultFormatSettings.LongDateFormat;
TMonthNameArray& ShortMonthNames = DefaultFormatSettings.ShortMonthNames;
TMonthNameArray& LongMonthNames = DefaultFormatSettings.LongMonthNames;
TWeekNameArray& ShortDayNames = DefaultFormatSettings.ShortDayNames;
TWeekNameArray& LongDayNames = DefaultFormatSettings.LongDayNames;
String& ShortTimeFormat = DefaultFormatSettings.ShortTimeFormat;
String& LongTimeFormat = DefaultFormatSettings.LongTimeFormat;
Char& TimeSeparator = DefaultFormatSettings.TimeSeparator;
String& TimeAMString = DefaultFormatSettings.TimeAMString;
String& TimePMString = DefaultFormatSettings.TimePMString;
WORD& TwoDigitYearCenturyWindow = DefaultFormatSettings.TwoDigitYearCenturyWindow; 

   /*  Date time formatting characters:
      C      : ShortDateFormat + ' ' + LongTimeFormat
      D      : Day of Month
      dd     : Day of Month (leading zero)
      ddd    : Day of week (abbreviation)
      dddd   : Day of week (full)
      ddddd  : ShortDateFormat
      dddddd : LongDateFormat
      m      : Month
      mm     : Month (leading zero)
      mmm    : Month (abbreviation)
      mmmm   : Month (full)
      Y      : Year (two Digits)
      yy     : Year (two Digits)
      yyyy   : Year (four Digits, with Century)
      h      : Hour
      hh     : Hour (leading zero)
      n      : minute
      nn     : minute (leading zero)
      S      : second
      SS     : second (leading zero)
      T      : ShortTimeFormat
      TT     : LongTimeFormat
      AM/PM  : use 12 Hour clock and display AM and PM accordingly
                A/P    : use 12 Hour clock and display A and P accordingly
      /      : Insert Date seperator
      :      : Insert time seperator
      "xx"   : literal Text
      'xx'   : literal Text
   */


TSysLocale SysLocale;
String ConfigExtension = _T(".cfg");
String SysConfigDir = _T("");
TGetVendorNameEvent OnGetVendorName;
TGetAppNameEvent OnGetApplicationName;
TGetTempDirEvent OnGetTempDir;
TGetTempFileEvent OnGetTempFile;
TSet < UChar, 0, 255 > LeadBytes = TSet < UChar, 0, 255 >();
String EmptyStr = _T("");
PString NullStr = &EmptyStr;
wstring EmptyWideStr = L"";
//  NullWideStr : PWideString = @EmptyWideStr;

std::vector< String > TrueBoolStrs, FalseBoolStrs;

String HexDisplayPrefix = _T("$");
// commenting is vp fix. These idents are in A different unit there.
/*System.*/
THandle feInvalidHandle = ((THandle) - 1 );  //return Value ON FileOpen Error

int Win32Platform = 0;
DWORD Win32MajorVersion = 0, Win32MinorVersion = 0, Win32BuildNumber = 0;
SmallString<255> Win32CSDVersion;

//---------------------------------------------------------------------------
bool Win32Check( bool RES )
{
  if ( ! RES ) 
    RaiseLastOSError();
  return RES;
}
//---------------------------------------------------------------------------
void RaiseLastWin32Error( )
{
  RaiseLastOSError();
}
//---------------------------------------------------------------------------
bool CheckWin32Version( int major )
{
  return CheckWin32Version( major, 0 );
}
//---------------------------------------------------------------------------
bool CheckWin32Version( int major, int minor )
{
  return ( Win32MajorVersion > ((DWORD) major ) ) || ( ( Win32MajorVersion == ((DWORD) major ) ) && ( Win32MinorVersion >= ((DWORD) minor ) ) );
}
//---------------------------------------------------------------------------
string StrPas( const char* str )
{
  return str;
}
//---------------------------------------------------------------------------
wstring StrPas( const wchar_t* str )
{
  return str;
}


String ChangeFileExt( const String& Filename, const String& Extension )
{
  int i = 0;
  TSet < UChar, 0, 255 > EndSep;
  i = Filename.length( );
  EndSep = AllowDirectorySeparators + AllowDriveSeparators + Sysutils__2;
  while ( ( i > 0 ) && ! ( EndSep.Contains(Filename[i - 1] ) ) ) 
    i--;
  if ( ( i == 0 ) || ( Filename[i - 1] != ExtensionSeparator ) ) 
    i = Filename.length( ) + 1;
  return Filename.substr( 1 - 1, i - 1 ) + Extension;
}


String ExtractFilePath( const String& Filename )
{
  int i = 0;
  TSet < UChar, 0, 255 > EndSep;
  i = Filename.length( );
  EndSep = AllowDirectorySeparators + AllowDriveSeparators;
  while ( ( i > 0 ) && ! ( EndSep.Contains(Filename[i - 1] ) ) ) 
    i--;
  if ( i > 0 ) 
    return Filename.substr( 1 - 1, i );
  else
    return _T("");
}


String ExtractFileDir( const String& Filename )
{
  int i = 0;
  TSet < UChar, 0, 255 > EndSep;
  i = Filename.length( );
  EndSep = AllowDirectorySeparators + AllowDriveSeparators;
  while ( ( i > 0 ) && ! ( EndSep.Contains(Filename[i - 1] ) ) ) 
    i--;
  if ( ( i > 1 ) && ( AllowDirectorySeparators.Contains(Filename[i - 1] ) ) && ! ( EndSep.Contains(Filename[i - 1 - 1] ) ) ) 
    i--;
  if ( i > 0 ) 
    return Filename.substr( 1 - 1, i );
  else
    return _T("");
}


String ExtractFileDrive( const String& Filename )
{
  int i = 0, l = 0;
  l = Filename.length( );
  if ( l < 2 ) 
    return String();
  if ( AllowDriveSeparators.Contains(Filename[2 - 1] ) ) 
    return Filename.substr( 1 - 1, 2 );
  else
    if ( ( AllowDirectorySeparators.Contains(Filename[1 - 1] ) ) && ( AllowDirectorySeparators.Contains(Filename[2 - 1] ) ) ) 
    {
      i = 2;
      while ( ( i < l ) && ! ( AllowDirectorySeparators.Contains(Filename[i + 1 - 1] ) ) ) 
        i++;
      if ( i > 0 ) 
         return Filename.substr( 1 - 1, i );
       else
         return _T("");
    }
  return String();
}


string ExtractFileName( const string& Filename )
{
  int i = 0;
  TSet < UChar, 0, 255 > EndSep;
  i = Filename.length( );
  EndSep = AllowDirectorySeparators + AllowDriveSeparators;
  while ( ( i > 0 ) && ! ( EndSep.Contains(Filename[i - 1] ) ) ) 
    i--;
  return Filename.substr( i + 1 - 1, MAXINT );
}

wstring ExtractFileName( const wstring& Filename )
{
  int i = 0;
  TSet < UChar, 0, 255 > EndSep;
  i = Filename.length( );
  EndSep = AllowDirectorySeparators + AllowDriveSeparators;
  while ( ( i > 0 ) && ! ( EndSep.Contains(Filename[i - 1] ) ) ) 
    i--;
  return Filename.substr( i + 1 - 1, MAXINT );
}


String ExtractFileExt( const String& Filename )
{
  int i = 0;
  TSet < UChar, 0, 255 > EndSep;
  i = Filename.length( );
  EndSep = AllowDirectorySeparators + AllowDriveSeparators + Sysutils__3;
  while ( ( i > 0 ) && ! ( EndSep.Contains(Filename[i - 1] ) ) ) 
    i--;
  if ( ( i > 0 ) && ( Filename[i - 1] == ExtensionSeparator ) ) 
    return Filename.substr( i - 1, MAXINT );
  else
    return String();
}

#ifdef windows
String ExtractShortPathName( const String& Filename )
{
  String result;
  result.resize( MAX_PATH );
  result.resize( GetShortPathName( Filename.c_str(), (Char*) result.c_str(), result.length( ) ) );
  return result;
}

typedef String PathStr;

#elif defined(linux)
typedef SmallString<255> ComStr;
//typedef SmallString<255> PathStr;
//typedef SmallString<255> dirstr;
//typedef SmallString<255> NameStr;
//typedef SmallString<255> extstr;

typedef String PathStr;
typedef String dirstr;
typedef String NameStr;
typedef String extstr;


#else
#error unknown platform

#endif




/*
    this File is part of the Free pascal Run time Library.
    Copyright (C) 1997-2000 by the Free pascal development team

    see the File copying.FPC, included in this distribution,
    for details about the Copyright.

    this program is distributed in the hope that it will be useful,
    but without Any WARRANTY; without even the implied WARRANTY of
    MERCHANTABILITY or FITNESS for A particular purpose.

 ***********************************************************************/

/*****************************************************************************
                A platform Independent FExpand implementation
*****************************************************************************/


void GetDirIO( unsignedchar drivenr, String& Dir )

/* GetDirIO is supposed to return the root of the given Drive   */
/* in case of an Error for compatibility of FExpand with tp/BP. */
{
  WORD OldInOutRes = 0;
  OldInOutRes = InOutRes;
  InOutRes = 0;
  GetDir( drivenr, Dir );
  InOutRes = OldInOutRes;
}


PathStr FExpand( const PathStr Path )

/* LFNSupport Boolean Constant, variable or function must be declared for all
   the platforms, at least locally in the DOS unit implementation part.
   in addition, FPC_FEXPAND_UNC, FPC_FEXPAND_DRIVES, FPC_FEXPAND_GETENV_PCHAR,
   FPC_FEXPAND_TILDE, FPC_FEXPAND_VOLUMES, FPC_FEXPAND_NO_DEFAULT_PATHS,
   FPC_FEXPAND_DRIVESEP_IS_ROOT, FPC_FEXPAND_NO_CURDIR,
   FPC_FEXPAND_NO_DOTS_UPDIR, FPC_FEXPAND_DIRSEP_IS_UPDIR,
   FPC_FEXPAND_DIRSEP_IS_CURDIR and FPC_FEXPAND_MULTIPLE_UPDIR conditionals
   might be defined to specify FExpand behaviour - see end of this File for
   individual descriptions. finally, FPC_FEXPAND_SYSUTILS allows to reuse
   the same implementation for Sysutils.ExpandFileName.
*/
{
  PathStr result;
#ifdef windows
  unsigned int PathStart = 0;
#elif defined(linux)
    unsigned int PathStart = 1;
#else
#error unknown platform
#endif
  bool RootNotNeeded = false;
  PathStr pa, dirs;
  String S;
  unsigned int i = 0, j = 0;

/* First convert the Path to UpperCase if appropriate for current platform. */
  if ( FileNameCaseSensitive ) 
    pa = Path;
  else
    pa = UpperCase( Path );

/* allow both '/' and '\' as Directory separators */
/* by converting all to the native one.           */
/*$warnings OFF*/
  for ( i = 1; i <= pa.length( ); i++)
    if ( AllowDirectorySeparators.Contains(pa[i - 1] ) ) 
      pa[i - 1] = DirectorySeparator;
/*$warnings ON*/

/* PathStart is amount of characters to strip to get beginning */
/* of Path without volume/Drive specification.                 */
#ifdef windows
  PathStart = 3;  
#elif defined(linux)
      /*replace ~/ with $home/*/
  if ( ( pa.length( ) >= 1 ) && ( pa[1 - 1] == _T('~')) && ( ( pa[2 - 1] == DirectorySeparator ) || ( pa.length( ) == 1 ) ) )
  {
    S = GetEnvironmentVariable( _T("HOME"));
    if ( ( S.empty()) || ( ( S.length( ) == 1 ) && ( S[1 - 1] == DirectorySeparator ) ) )
      pa.erase( 1 - 1, 1 );
    else
      if ( S[S.length( ) - 1] == DirectorySeparator )
        pa = S + pa.substr( 3 - 1, pa.length( ) - 2 );
      else
        pa = S + pa.substr( 2 - 1, Pred( pa.length( ) ) );
  } 
#else
#error unknown platform
#endif

/* Expand TILDE to home Directory if appropriate. */      
/* do we have A Drive/volume specification? */
#ifdef linux
/* do we have A Drive/volume specification? */
  if ( ( pa.length( ) > 1 ) && ( Sysutils__4.Contains(pa[1 - 1] ) ) && ( pa[2 - 1] == DriveSeparator ) )
  {

/* we need to know current Directory ON given */
/* volume/Drive _if_ such A thing is defined. */         

/* if drives are not supported, but A Drive */
/* was supplied anyway, IGNORE (remove) it. */
    pa.erase( 1 - 1, 2 );
  }
    /*Check whether we don'T have an ABSOLUTE Path Already*/
  if ( (( pa.length( ) >= PathStart ) && ( pa[PathStart - 1]) != DirectorySeparator ) || ( pa.length( ) < PathStart ) )
#else
  if ( ( pa.length( ) > 1 ) && ( Sysutils__4.Contains(pa[1 - 1] ) ) && ( pa[2 - 1] == DriveSeparator ) ) 
  {

/* we need to know current Directory ON given */
/* volume/Drive _if_ such A thing is defined. */   
            /* always UpperCase driveletter */
    if ( Sysutils__5.Contains(pa[1 - 1] ) ) 
      pa[1 - 1] = Char( int( pa[1 - 1] ) & ~ ( 0x20 ) );
    GetDirIO( int( pa[1 - 1] ) - int( 'A' ) + 1, S ); 

/* do we have more than Just Drive/volume specification? */
    if ( pa.length( ) == Pred( PathStart ) ) 

/* if not, Just use the current Directory for that Drive/volume. */
      pa = S;
    else

/* if yes, find out whether the following Path is RELATIVE or ABSOLUTE. */
      if ( pa[PathStart - 1] != DirectorySeparator ) 
        if ( pa[1 - 1] == S[1 - 1] ) 
        {
                            /* remove ending slash if it Already exists */
          if ( S[S.length( ) - 1] == DirectorySeparator ) 
            S.resize( S.length( ) - 1 );
          pa = S + Char(DirectorySeparator) + pa.substr( PathStart - 1, pa.length( ) - PathStart + 1 );
        }
        else
          pa = String(1, pa[1 - 1]) + Char(DriveSeparator) + Char(DirectorySeparator) + pa.substr( PathStart - 1, pa.length( ) - PathStart + 1 );
  }
  else
#endif
  {

/* get current Directory ON selected Drive/volume. */
    GetDirIO( 0, S );    
#ifdef windows
/* do we have an ABSOLUTE Path without Drive or volume? */
    if ( ( pa.length( ) > 0 ) && ( pa[1 - 1] == DirectorySeparator ) ) 
    { 
                    /*do not touch network Drive Names*/
      if ( ( pa.length( ) > 1 ) && ( pa[2 - 1] == DirectorySeparator ) && LFNSupport ) 
      {
        PathStart = 3;
                            /*find the start of the String of directories*/
        while ( ( PathStart <= pa.length( ) ) && ( pa[PathStart - 1] != DirectorySeparator ) ) 
          PathStart++;
        if ( PathStart > pa.length( ) ) 
                            /*we have Just A Machine Name...*/
          if ( pa.length( ) == 2 ) 
                                /*...or not even that one*/
            PathStart = 2;
          else
            pa = pa + String(1, DirectorySeparator);
        else
          if ( PathStart < pa.length( ) ) 
                                /*we have A Resource Name as well*/
          {
            RootNotNeeded = true;
                                        /*let'S continue in searching*/
            do
            {
              PathStart++;
            }
            while ( ! ( ( PathStart > pa.length( ) ) || ( pa[PathStart - 1] == DirectorySeparator ) ) );
          }
      }
      else
        pa = String(1, S[1 - 1] ) + String(1, DriveSeparator) + pa;
    }
    else 
#endif
      /* we Already have A slash if root is the curent Directory. */
      if ( S.length( ) == PathStart ) 
        pa = S + pa;
      else

                    /* we need an ending slash if FExpand was called  */
                    /* with an empty String for compatibility, except */
                    /* for platforms Where this is Invalid.           */
        if ( pa.length( ) == 0 ) 
          pa = S + String(1, DirectorySeparator);
        else
          pa = S + String(1, DirectorySeparator) + pa;
  }

    /*get String of directories to only process RELATIVE references ON this one*/
  dirs = pa.substr( Succ( PathStart ) - 1, pa.length( ) - PathStart );  
    /*First remove all references to '\.\'*/
  i = Pos( String(1, DirectorySeparator ) + _T(".")+ String(1, DirectorySeparator ), dirs);
  while ( i != 0 ) 
  {
    dirs.erase( i - 1, 2 );
    i = Pos( String(1,  DirectorySeparator ) + _T(".")+ String(1, DirectorySeparator ), dirs);
  }     

    /*Now remove also all references to '\..\' + of course previous dirs..*/
  i = Pos( String(1, DirectorySeparator ) + _T("..")+ String(1, DirectorySeparator), dirs );
  while ( i != 0 ) 
  {
    j = Pred( i );
    while ( ( j > 0 ) && ( dirs[j - 1] != DirectorySeparator ) ) 
      j--;
    dirs.erase( Succ( j ) - 1, i - j + 3 );
    i = Pos( String(1, DirectorySeparator ) + _T("..")+ String(1, DirectorySeparator), dirs );
  }    
    /*then remove also A reference to '\..' at the end of Line
    + the previous Directory, of course,...*/
  i = Pos( String(1, DirectorySeparator ) + _T(".."), dirs);
  if ( ( i != 0 ) && ( i == dirs.length( ) - 2 ) ) 
  {
    j = Pred( i );
    while ( ( j > 0 ) && ( dirs[j - 1] != DirectorySeparator ) ) 
      j--;
    if ( j == 0 ) 
      dirs = _T("");
    else
      dirs.erase( Succ( j ) - 1, i - j + 2 );
  }     
    /*...and also A possible reference to '\.'*/
  if ( dirs.length( ) == 1 ) 
  {
    if ( dirs[1 - 1] == '.' ) 
            /*A special case*/
      dirs = _T("");
  }
  else
    if ( ( dirs.length( ) != 0 ) && ( dirs[dirs.length( ) - 1] == '.' ) && ( dirs[Pred( dirs.length( ) ) - 1] == DirectorySeparator ) ) 
      dirs.erase( dirs.length( ) - 1 - 1, 2 );

    /*finally remove '.\' at the beginning of the String of directories...*/
  while ( ( dirs.length( ) >= 2 ) && ( dirs[1 - 1] == '.' ) && ( dirs[2 - 1] == DirectorySeparator ) ) 
    dirs.erase( 1 - 1, 2 );     
    /*...and possible (Invalid) references to '..\' as well*/
  while ( ( dirs.length( ) >= 3 ) && ( dirs[1 - 1] == '.' ) && ( dirs[2 - 1] == '.' ) && ( dirs[3 - 1] == DirectorySeparator ) ) 
    dirs.erase( 1 - 1, 3 ); 

    /*two special cases - '.' and '..' alone*/
  if ( ( dirs.length( ) == 1 ) && ( dirs[1 - 1] == '.' ) ) 
    dirs = _T("");
  if ( ( dirs.length( ) == 2 ) && ( dirs[1 - 1] == '.' ) && ( dirs[2 - 1] == '.' ) ) 
    dirs = _T(""); 

    /*JOIN the parts back to Create the complete Path*/
  if ( dirs.length( ) == 0 ) 
  {
    pa = pa.substr( 1 - 1, PathStart );
    if ( pa[PathStart - 1] != DirectorySeparator ) 
      pa = pa + String(1, DirectorySeparator);
  }
  else
    pa = pa.substr( 1 - 1, PathStart ) + dirs; 
    /*remove ending \ if not supplied originally, the original String
    wasn'T empty (to stay Compatible) and if not really needed*/
  if
/*A special case with UNC paths*/
    /*reference to current Directory at the end should be removed*/ ( ( pa[pa.length( ) - 1] == DirectorySeparator ) && ( ( pa.length( ) > PathStart ) || ( RootNotNeeded && ( pa.length( ) == PathStart ) ) ) && ( Path.length( ) != 0 ) && ( Path[Path.length( ) - 1] != DirectorySeparator ) ) 
    pa.erase( pa.length( ) - 1, 1 );
  result = pa;
  return result;
}

/* Description of individual conditional defines supported for FExpand
   (Disregard the used Directory separators in Examples, Constant
   System.DirectorySeparator is used in the real implemenation, of course):

   FPC_FEXPAND_UNC - UNC ("universal Naming convention") paths are
   supported (usually used for networking, used in DOS (with
   networking support installed), OS/2, Win32 and at least some
   Netware versions as Far as i remember. an example of such A Path
   is '\\servername\sharename\some\Path'.

   FPC_FEXPAND_DRIVES - Drive letters are supported (DOS-like
   environments - DOS, OS/2, Win32). example is 'C:\test'.

   FPC_FEXPAND_GETENV_PCHAR - an implementation of GetEnv returning
   PChar instead of A ShortString is available (Unix) to support
   LONG values of environment variables.

   FPC_FEXPAND_TILDE - expansion of '~/' to GetEnv('home') - Unix.
   example: '~/some/Path'.

   FPC_FEXPAND_VOLUMES - volumes are supported (Similar to drives,
   but the Name can be longer; used under Netware, amiga and
   probably MacOS as Far as i understand it correctly). example:
   'VolumeName:some:Path' or 'servername/volume:some\Path'
   (Netware).

   FPC_FEXPAND_NO_DEFAULT_PATHS - DOS keeps information about the
   current Directory for every Drive. if some platform Supports
   drives or volumes, but keeps no track of current directories for
   them (i.e. there'S no support for "GetDir(DriveNumber, Dir)" or
   "GetDir(volume, Dir)", but only for "GetDir (0, Dir)" (i.e. the
   overall current Directory), you should define this. otherwise
   constructs like 'C:some\Path' refer A Path RELATIVE to the
   current Directory ON the C: Drive.

   FPC_FEXPAND_DRIVESEP_IS_ROOT - this means that DriveSeparator
   should be used as beginning of the "real" Path for A particular
   Drive or volume instead of the DirectorySeparator. this would be
   used in case that there is only one character (DriveSeparator)
   delimitting the Drive Letter or volume Name from the remaining
   Path _and_ the DriveSeparator marks the root of an ABSOLUTE Path
   in that case. example - 'volume:this/is/ABSOLUTE/Path'.

   FPC_FEXPAND_NO_CURDIR - there is no support to refer to current
   Directory explicitely (like '.' used under both Unix and DOS-like
   environments).

   FPC_FEXPAND_NO_DOTS_UPDIR - '..' cannot be used to refer to the
   upper Directory.

   FPC_FEXPAND_DIRSEP_IS_UPDIR - DirectorySeparator at the beginning of
   A Path (or doubled DirectorySeparator inside the Path) refer to the
   Parent Directory, one more DirectorySeparator to Parent Directory of
   Parent Directory and So ON (amiga). Please, Note that you can decide
   to support both '..' and DirectorySeparator as references to the Parent
   Directory at the same time for compatibility reasons - However this
   support makes it impossible to use an otherwise possibly valid Name
   of '..'.

   FPC_FEXPAND_DIRSEP_IS_CURDIR - DirectorySeparator at the beginning of
   A Path refers to the current Directory (i.e. Path beginning with
   DirectorySeparator is always A RELATIVE Path). two DirectorySeparator
   characters refer to the Parent Directory, three refer to Parent
   Directory of the Parent Directory and So ON (MacOS).

   FPC_FEXPAND_MULTIPLE_UPDIR - Grouping of more characters specifying
   upper Directory references Higher Directory levels. example: '...'
   (Netware).

   FPC_FEXPAND_SYSUTILS allows to reuse the same implementation for
   Sysutils.ExpandFileName by avoiding things specific for unit DOS.
*/

void DoDirSeparators( String& Filename )
{
  unsigned int i = 0;
  for ( i = 1; i <= Filename.length( ); i++)
    if ( AllowDirectorySeparators.Contains(Filename[i - 1] ) ) 
      Filename[i - 1] = DirectorySeparator;
}


String SetDirSeparators( const String& Filename )
{
  String result;
  result = Filename;
  DoDirSeparators( result );
  return result;
}

String ExpandFileName( const String& Filename )
{
  String result;
  String S;
  S = Filename;
  DoDirSeparators( S );
  result = FExpand( S );
  return result;
}



const int MaxDirs = 129;


String ExtractRelativePath( const String& BaseName, const String& DestName )
{
  String result;
  String Source, Dest;
  int sc = 0, DC = 0, i = 0, j = 0;
  Char* sd[ 129/*# range 1..MaxDirs*/ ], *dd[ 129/*# range 1..MaxDirs*/ ];
  const Char OneLevelBack[] = _T(".." DIRSEP_STR); // DirectorySeparator
  if ( UpperCase( ExtractFileDrive( BaseName ) ) != UpperCase( ExtractFileDrive( DestName ) ) ) 
  {
    result = DestName;
    return result;
  }
  Source = ExcludeTrailingPathDelimiter( ExtractFilePath( BaseName ) );
  Dest = ExcludeTrailingPathDelimiter( ExtractFilePath( DestName ) );
  sc = GetDirs( Source, sd, MAXIDX( sd ) );
  DC = GetDirs( Dest, dd, MAXIDX( dd ) );
  i = 1;
  while ( ( i <= DC ) && ( i <= sc ) ) 
  {
    if ( StrIComp( dd[i - 1], sd[i - 1] ) == 0 ) 
      i++;
    else
      break;
  }
  result = _T("");
  for ( j = i; j <= sc; j++)
    result = result + OneLevelBack;
  for ( j = i; j <= DC; j++)
    result = result + dd[j - 1] + String(1, DirectorySeparator);
  result = result + ExtractFileName( DestName );
  return result;
}



/*
  DirName is split in A #0 separated List of Directory Names,
  dirs is an array of pchars, pointing to These Directory Names.
  the function returns the number of directories found, or -1
  if none were found.
*/


int GetDirs( String& DirName, Char** dirs, int dirs_maxidx )
{
  int result = 0;
  unsigned int i = 0;
  i = 1;
  result = - 1;
  while ( i <= DirName.length( ) ) 
  {
    if
       /* avoid Error in case Last char=PathDelim */ ( ( AllowDirectorySeparators.Contains(DirName[i - 1] ) ) && ( DirName.length( ) > i ) ) 
    {
      DirName[i - 1] = '\x00';
      result++;
      dirs[result] = &DirName[i + 1 - 1];
    }
    i++;
  }
  if ( result > - 1 ) 
    result++;
  return result;
}


String IncludeTrailingPathDelimiter( const String& Path )
{
  String result;
  int l = 0;
  result = Path;
  l = result.length( );
  if ( ( l == 0 ) || ! ( AllowDirectorySeparators.Contains(result[l - 1] ) ) ) 
    result = result + String(1, DirectorySeparator);
  return result;
}


String IncludeTrailingBackslash( const String& Path )
{
  String result;
  result = IncludeTrailingPathDelimiter( Path );
  return result;
}


String ExcludeTrailingBackslash( const String& Path )
{
  String result;
  result = ExcludeTrailingPathDelimiter( Path );
  return result;
}


String ExcludeTrailingPathDelimiter( const String& Path )
{
  String result;
  int l = 0;
  l = Path.length( );
  if ( ( l > 0 ) && ( AllowDirectorySeparators.Contains(Path[l - 1] ) ) ) 
    l--;
  if ( l > 0 ) 
    return Path.substr( 1 - 1, l );
  else
    return _T("");
}


bool IsPathDelimiter( const String& Path, int Index )
{
  bool result = false;
  result = ( Index > 0 ) && ( (unsigned int) Index <= Path.length( ) ) && ( AllowDirectorySeparators.Contains(Path[Index - 1] ) );
  return result;
}

String FileSearch( const String& Name, const String& DirList, bool ImplicitCurrentDir )
{
  String result;
  int i = 0;
  String Temp;
  result = Name;
  Temp = SetDirSeparators( DirList );
      // start with checking the File in the current Directory
  if ( ImplicitCurrentDir && ( !result.empty()) && FileExists( result ) ) 
    return result;
  while ( true ) 
  {
    if ( Temp.empty()) 
      break; // no more directories to Search - Fail
    i = Pos( PathSeparator, Temp );
    if ( i != 0 ) 
    {
      result = Temp.substr( 1 - 1, i - 1 );
      Temp.erase( 1 - 1, i );
    }
    else
    {
      result = Temp;
      Temp = _T("");
    }
    if ( !result.empty()) 
      result = IncludeTrailingPathDelimiter( result ) + Name;
    if ( ( !result.empty()) && FileExists( result ) ) 
      return result;
  }
  result = _T("");
  return result;
}


String ExeSearch( const String& Name, const String& DirList )
{
  String result;
  result = FileSearch( Name, DirList, true );
  return result;
}

#ifndef linux
bool FileIsReadOnly( const String& Filename )
{
  bool result = false;
  result = ( ( FileGetAttr( Filename ) & faReadOnly ) ) != 0;
  return result;
}


int FileSetDate( const String& Filename, int Age )
{
  int result = 0;
  THandle FD = 0;
    /* at least Windows requires fmOpenWrite here */
  FD = FileOpen( Filename, fmOpenWrite );
  if ( FD != feInvalidHandle ) 
  {
    try
    {
      result = FileSetDate( FD, Age );
    }
    catch(...)
    {
      FileClose( FD );
      throw;
    }
    /*# finally begin */
    FileClose( FD );
    /*# finally end */ 
  }
  else
    result = (int) FD;
  return result;
} 
#endif


/*   NewStr creates A New PString and assigns S to it
    if Length(S) = 0 NewStr returns nil   */
PString NewStr( const String& S )
{
  PString result = NULL;
  if ( S.empty()) 
    result = NULL;
  else
  {
    result = new String;
    if ( result != NULL ) 
      *result = S;
  }
  return result;
}


/*   AssignStr assigns S to P^   */
void AssignStr( PString& P, const String& S )
{
  *P = S;
}

/*   AppendStr appends S to Dest   */
void AppendStr( String& Dest, const String& S )
{
  Dest = Dest + S;
}
//---------------------------------------------------------------------------
string UpperCase( const string& xs )
{
  string s;
#if defined(linux) or not defined(_MSC_VER)
  string::const_iterator t, tEnd;
  for( t = xs.begin(); t != xs.end(); ++t)
    s.append(1, toupper(*t));
#else
  transform(xs.begin(), xs.end(), back_inserter(s), toupper);
#endif
  return s;
}
//---------------------------------------------------------------------------
wstring UpperCase( const wstring& xs )
{
  wstring s;
  transform(xs.begin(), xs.end(), back_inserter(s), towupper);
  return s;
}
//---------------------------------------------------------------------------
/*   LowerCase returns A copy of S Where all UpperCase characters ( from A to Z )
    have been converted to LowerCase  */
string LowerCase( const string& xs )
{
  string s;
#if defined(linux) or not defined(_MSC_VER)
  string::const_iterator t, tEnd;
  for( t = xs.begin(); t != xs.end(); ++t)
    s.append(1, tolower(*t));
#else
    transform(xs.begin(), xs.end(), back_inserter(s), tolower);
#endif
  return s;
}
//---------------------------------------------------------------------------
wstring LowerCase( const wstring& xs )
{
  wstring s;
  transform(xs.begin(), xs.end(), back_inserter(s), towlower);
  return s;
}
//---------------------------------------------------------------------------


/*   CompareStr compares S1 and S2, the result is the based ON
    substraction of the ASCII values of the characters in S1 and S2
    case     result
    S1 < S2  < 0
    S1 > S2  > 0
    S1 = S2  = 0     */


int CompareStr( const String& S1, const String& S2 )
{
  int result = 0;
  int Count = 0, count1 = 0, count2 = 0;
  result = 0;
  count1 = S1.length( );
  count2 = S2.length( );
  if ( count1 > count2 ) 
    Count = count2;
  else
    Count = count1;
  result = CompareMemRange( ((void*) S1.c_str()), ((void*) S2.c_str()), Count );
  if ( result == 0 ) 
    result = count1 - count2;
  return result;
}

/*   CompareMemRange returns the result of comparison of Length Bytes at p1 and p2
    case       result
    p1 < p2    < 0
    p1 > p2    > 0
    p1 = p2    = 0    */


int CompareMemRange( void* p1, void* p2, unsignedint Length )
{
  int result = 0;
  unsigned int i = 0;
  i = 0;
  result = 0;
  while ( ( result == 0 ) && ( i < Length ) ) 
  {
    result = ((unsigned char) *( unsigned char*) p1 ) - ((unsigned char) *( unsigned char*) p2 );
    p1 = (Char*) ( p1 ) + 1;            // vp Compat.
    p2 = (Char*) ( p2 ) + 1;
    i = i + 1;
  }
  return result;
}


bool CompareMem( void* xp1, void* xp2, unsignedint Length )
{
  PByte p1 = (PByte) xp1, p2 = (PByte) xp2;
  bool result = false;
  unsigned int i = 0;
  result = true;
  i = 0;
  if ( p1 != p2 ) 
    while ( result && ( i < Length ) ) 
    {
      result = *p1 == *p2 ;
      i++;
      p1++;
      p2++;
    }
  return result;
}


/*   CompareText compares S1 and S2, the result is the based ON
    substraction of the ASCII values of characters in S1 and S2
    comparison is case-insensitive
    case     result
    S1 < S2  < 0
    S1 > S2  > 0
    S1 = S2  = 0     */


int CompareText( const String& S1, const String& S2 )
{
  int result = 0;
  int i = 0, Count = 0, count1 = 0, count2 = 0;
  unsigned char Chr1 = '\0', Chr2 = '\0';
  const Char* p1 = NULL,* p2 = NULL;
  count1 = S1.length( );
  count2 = S2.length( );
  if ( count1 > count2 ) 
    Count = count2;
  else
    Count = count1;
  p1 = &S1[1 - 1];
  p2 = &S2[1 - 1];
  i = 0;
  while ( i < Count ) 
  {
    Chr1 = ((unsigned char) *p1 );
    Chr2 = ((unsigned char) *p2 );
    if ( Chr1 != Chr2 ) 
    {
      if ( Sysutils__8.Contains(Chr1 ) ) 
        Chr1 -= 32;
      if ( Sysutils__9.Contains(Chr2 ) ) 
        Chr2 -= 32;
      if ( Chr1 != Chr2 ) 
        break;
    }
    p1++;
    p2++;
    i++;
  }
  if ( i < Count ) 
    result = Chr1 - Chr2;
  else
    result = count1 - count2;
  return result;
}


bool SameText( const String& S1, const String& S2 )
{
  return CompareText( S1, S2 ) == 0;
}

#ifdef linux
/*==============================================================================*/
/*   ANSI String functions                                                      */
/*   These functions rely ON the character set loaded by the OS                 */
/*==============================================================================*/



typedef Char TCaseTranslationTable [ 256/*# range 0..255*/ ];
  /* tables with upper and LowerCase Forms of character sets.
    must be initialized with the correct Code-Pages */


TCaseTranslationTable UpperCaseTable;
TCaseTranslationTable LowerCaseTable;


String GenericAnsiUpperCase( const String& S )
{
  String result;
  int Len = 0, i = 0;
  Len = S.length( );
  result.resize( Len );
  for ( i = 1; i <= Len; i++)
    result[i - 1] = UpperCaseTable[int( S[i - 1] )];
  return result;
}


String GenericAnsiLowerCase( const String& S )
{
  String result;
  int Len = 0, i = 0;
  Len = S.length( );
  result.resize( Len );
  for ( i = 1; i <= Len; i++)
    result[i - 1] = LowerCaseTable[int( S[i - 1] )];
  return result;
}


PtrInt GenericAnsiCompareStr( const String& S1, const String& S2 )
{
  PtrInt result = 0;
  int i = 0, L1 = 0, L2 = 0;
  result = 0;
  L1 = S1.length( );
  L2 = S2.length( );
  i = 1;
  while ( ( result == 0 ) && ( ( i <= L1 ) && ( i <= L2 ) ) )
  {
    result = int( S1[i - 1] ) - int( S2[i - 1] ); //!! must be replaced by ANSI characters !!
    i++;
  }
  if ( result == 0 )
    result = L1 - L2;
  return result;
}


PtrInt GenericAnsiCompareText( const String& S1, const String& S2 )
{
  PtrInt result = 0;
  int i = 0, L1 = 0, L2 = 0;
  result = 0;
  L1 = S1.length( );
  L2 = S2.length( );
  i = 1;
  while ( ( result == 0 ) && ( ( i <= L1 ) && ( i <= L2 ) ) )
  {
    result = int( LowerCaseTable[int( S1[i - 1] )] ) - int( LowerCaseTable[int( S2[i - 1] )] ); //!! must be replaced by ANSI characters !!
    i++;
  }
  if ( result == 0 )
    result = L1 - L2;
  return result;
}


PtrInt GenericAnsiStrComp( const Char* S1, const Char* S2 )
{
  PtrInt result = 0;
  result = 0;
  if ( S1 == NULL )
  {
    if ( S2 == NULL )
      return result;
    result = - 1;
    return result;
  }
  if ( S2 == NULL )
  {
    result = 1;
    return result;
  }
  while ( ( result == 0 ) && ( *S1 != _T('\x00')) && ( *S2 != _T('\x00')) )
  {
    result = int( *S1 ) - int( *S2 ); //!! must be replaced by ANSI characters !!
    S1++;
    S2++;
  }
  if ( ( result == 0 ) && ( *S1 != *S2 ) ){ // loop ended because Exactly one has #0
    if ( *S1 == _T('\x00')) // shorter String is smaller
      result = - 1;
    else
      result = 1;
  }

  return result;
}


PtrInt GenericAnsiStrIComp( const Char* S1, const Char* S2 )
{
  PtrInt result = 0;
  result = 0;
  if ( S1 == NULL )
  {
    if ( S2 == NULL )
      return result;
    result = - 1;
    return result;
  }
  if ( S2 == NULL )
  {
    result = 1;
    return result;
  }
  while ( ( result == 0 ) && ( *S1 != _T('\x00')) && ( *S2 != _T('\x00')) )
  {
    result = int( LowerCaseTable[int( S1[0] )] ) - int( LowerCaseTable[int( S2[0] )] ); //!! must be replaced by ANSI characters !!
    S1++;
    S2++;
  }
  if ( ( result == 0 ) && ( S1[0] != S2[0] ) ){ //Length(S1)<>Length(S2)
    if ( S1[0] == _T('\x00'))
      result = - 1; //S1 shorter than S2
    else
      result = 1; //S1 longer than S2

  }
  return result;
}


PtrInt GenericAnsiStrLComp( const Char* S1, const Char* S2, PtrUInt MaxLen )
{
  PtrInt result = 0;
  unsigned int i = 0;
  result = 0;
  if ( MaxLen == 0 )
    return result;
  if ( S1 == NULL )
  {
    if ( S2 == NULL )
      return result;
    result = - 1;
    return result;
  }
  if ( S2 == NULL )
  {
    result = 1;
    return result;
  }
  i = 0;
  do
  {
    result = int( S1[0] ) - int( S2[0] ); //!! must be replaced by ANSI characters !!
    S1++;
    S2++;
    i++;
  }
  while ( ! ( ( result != 0 ) || ( i == MaxLen ) ) );
  return result;
}


PtrInt GenericAnsiStrLIComp( const Char* S1, const Char* S2, PtrUInt MaxLen )
{
  PtrInt result = 0;
  unsigned int i = 0;
  result = 0;
  if ( MaxLen == 0 )
    return result;
  if ( S1 == NULL )
  {
    if ( S2 == NULL )
      return result;
    result = - 1;
    return result;
  }
  if ( S2 == NULL )
  {
    result = 1;
    return result;
  }
  i = 0;
  do
  {
    result = int( LowerCaseTable[int( S1[0] )] ) - int( LowerCaseTable[int( S2[0] )] ); //!! must be replaced by ANSI characters !!
    S1++;
    S2++;
    i++;
  }
  while ( ! ( ( result != 0 ) || ( i == MaxLen ) ) );
  return result;
}


Char* GenericAnsiStrLower( Char* str )
{
  Char* result = NULL;
  result = str;
  if ( str != NULL )
  {
    while ( *str != _T('\x00'))
    {
      *str = LowerCaseTable[((unsigned char) *str )];
      str = str + 1;
    }
  }
  return result;
}


Char* GenericAnsiStrUpper( Char* str )
{
  Char* result = NULL;
  result = str;
  if ( str != NULL )
  {
    while ( *str != _T('\x00'))
    {
      *str = UpperCaseTable[((unsigned char) *str )];
      str = str + 1;
    }
  }
  return result;
}


bool AnsiSameText( const String& S1, const String& S2 )
{
  bool result = false;
  result = AnsiCompareText( S1, S2 ) == 0;
  return result;
}


bool AnsiSameStr( const String& S1, const String& S2 )
{
  bool result = false;
  result = AnsiCompareStr( S1, S2 ) == 0;
  return result;
}


Char* AnsiLastChar( const String& S )
{
  Char* result = NULL;
  //!! no MultiByte yet, So we return the Last one.
  result = StrEnd( (Char*) ( ((void*) S.c_str()) ) );  // StrEnd checks for nil
  result--;
  return result;
}


Char* AnsiStrLastChar( Char* str )
{
  Char* result = NULL;
  //!! no MultiByte yet, So we return the Last one.
  result = StrEnd( str );
  result--;
  return result;
}

void EnsureAnsiLen( string& S, const int Len )
{
  if ( Len > S.length( ) )
  {
    if ( S.length( ) < 10 * 256 )
      S.resize( S.length( ) + 10 );
    else
      S.resize( S.length( ) + ( S.length( ) >> 8 ) );
  }
}

void EnsureAnsiLen( wstring& S, const int Len )
{
  if ( Len > S.length( ) )
  {
    if ( S.length( ) < 10 * 256 )
      S.resize( S.length( ) + 10 );
    else
      S.resize( S.length( ) + ( S.length( ) >> 8 ) );
  }
}

void ConcatCharToAnsiStr( const char C, string& S, int& Index )
{
  EnsureAnsiLen( S, Index );
  S[Index - 1] = C;
  Index++;
}

void ConcatCharToAnsiStr( const wchar_t C, wstring& S, int& Index )
{
  EnsureAnsiLen( S, Index );
  S[Index - 1] = C;
  Index++;
}
/* Concatenates an UTF-32 char to A WideString. S *must* be unique when entering. */


void ConcatUTF32ToAnsiStr( const wint_t NC, string& S, int& Index, mbstate_t& mbstate )
{
  char* P = NULL;
  size_t mblen = 0;
  /* we know that S is unique -> avoid UniqueString calls*/
  P = &S[Index - 1];
  if ( NC <= 127 )
    ConcatCharToAnsiStr( ((Char) NC ), S, Index );
  else
  {
    EnsureAnsiLen( S, Index + MB_CUR_MAX );
    mblen = wcrtomb( P, ((wchar_t) NC ), &mbstate );
    if ( mblen != ((size_t) - 1 ) )
      Index += mblen;
    else
    {
          /* Invalid wide char */
      *P = L'?';
      Index++;
    }
  }
}


String LowerAnsiString( const String& S )
{
  string result;
  int i = 0, slen = 0, resindex = 0;
  size_t mblen = 0;
  mbstate_t ombstate, nmbstate;
  wchar_t WC = 0;
  FillChar( &ombstate, sizeof( ombstate ), 0 );
  FillChar( &nmbstate, sizeof( nmbstate ), 0 );
  slen = S.length( );
  result.resize( slen + 10 );
  i = 1;
  resindex = 1;
  while ( i <= slen )
  {
    if ( S[i - 1] <= '\x7f' )
    {
      WC = ((wchar_t) S[i - 1] );
      mblen = 1;
    }
    else
      mblen = mbrtowc( &WC, (char*) ( &S[i - 1] ), slen - i + 1, &ombstate );
    switch ( mblen )
    {
      case ((size_t) - 2 ):
      {
              /* partial Invalid character, copy literally */
        while ( i <= slen )
        {
          ConcatCharToAnsiStr( S[i - 1], result, resindex );
          i++;
        }
      }
      break;
      case ((size_t) - 1 ): case 0:
      {
              /* Invalid or NULL character */
        ConcatCharToAnsiStr( S[i - 1], result, resindex );
        i++;
      }
      break;
    default:
    {
              /* A valid sequence */
              /* even if mblen = 1, the LowerCase version may have A */
              /* different Length                                     */
              /* we can'T do anything special if wchar_t is 16 bit... */
      ConcatUTF32ToAnsiStr( towlower( ((wint_t) WC ) ), result, resindex, nmbstate );
      i += mblen;
    }
    }
  }
  result.resize( resindex - 1 );
#ifdef _WIDESTRING
  return str2wstr(result);
#else
  return result;
#endif
}


String UpperAnsiString( const String& S )
{
  string result;
  int i = 0, slen = 0, resindex = 0;
  size_t mblen = 0;
  mbstate_t ombstate, nmbstate;
  wchar_t WC = 0;
  FillChar( &ombstate, sizeof( ombstate ), 0 );
  FillChar( &nmbstate, sizeof( nmbstate ), 0 );
  slen = S.length( );
  result.resize( slen + 10 );
  i = 1;
  resindex = 1;
  while ( i <= slen )
  {
    if ( S[i - 1] <= '\x7f' )
    {
      WC = ((wchar_t) S[i - 1] );
      mblen = 1;
    }
    else
      mblen = mbrtowc( &WC, (char*) ( &S[i - 1] ), slen - i + 1, &ombstate );
    switch ( mblen )
    {
      case ((size_t) - 2 ):
      {
              /* partial Invalid character, copy literally */
        while ( i <= slen )
        {
          ConcatCharToAnsiStr( S[i - 1], result, resindex );
          i++;
        }
      }
      break;
      case ((size_t) - 1 ): case 0:
      {
              /* Invalid or NULL character */
        ConcatCharToAnsiStr( S[i - 1], result, resindex );
        i++;
      }
      break;
    default:
    {
              /* A valid sequence */
              /* even if mblen = 1, the UpperCase version may have A */
              /* different Length                                     */
              /* we can'T do anything special if wchar_t is 16 bit... */
      ConcatUTF32ToAnsiStr( towupper( ((wint_t) WC ) ), result, resindex, nmbstate );
      i += mblen;
    }
    }
  }
  result.resize( resindex - 1 );
#ifdef _WIDESTRING  
  return str2wstr(result);
#else
  return result;
#endif
}



String AnsiUpperCase( const String& S )
{
  return UpperAnsiString( S );
}


String AnsiLowerCase( const String& S )
{
  return LowerAnsiString( S );
}

wstring LowerWideString( const std::wstring& S )
{
  std::wstring result;
  int i = 0;
  result.resize( S.length( ) );
  wchar_t* p = (wchar_t*) result.c_str();
  for ( i = 0; i <= S.length( ) - 1; i++)
    p[i] = ((wchar_t) ( towlower( ((wint_t) ( S[i + 1 - 1] ) ) ) ) );
  return result;
}


wstring UpperWideString( const std::wstring& S )
{
  std::wstring result;
  int i = 0;
  result.resize( S.length( ) );
  wchar_t* p = (wchar_t*) result.c_str();
  for ( i = 0; i <= S.length( ) - 1; i++)
    p[i] = ((wchar_t) ( towupper( ((wint_t) ( S[i + 1 - 1] ) ) ) ) );
  return result;
}







int AnsiCompareStr( const String& S1, const String& S2 )
{
  return GenericAnsiCompareStr( S1, S2 );
}


int AnsiCompareText( const String& S1, const String& S2 )
{
  return GenericAnsiCompareText( S1, S2 );
}


int AnsiStrComp( const Char* S1, const Char* S2 )
{
  return GenericAnsiStrComp( S1, S2 );
}


int AnsiStrIComp( const Char* S1, const Char* S2 )
{
  return GenericAnsiStrIComp( S1, S2 );
}


int AnsiStrLComp( const Char* S1, const Char* S2, unsignedint MaxLen )
{
  return GenericAnsiStrLComp( S1, S2, MaxLen );
}


int AnsiStrLIComp( const Char* S1, const Char* S2, unsignedint MaxLen )
{
  return GenericAnsiStrLIComp( S1, S2, MaxLen );
}


Char* AnsiStrLower( Char* str )
{
  return GenericAnsiStrLower( str );
}


Char* AnsiStrUpper( Char* str )
{
  return GenericAnsiStrUpper( str );
}


/*==============================================================================*/
/*  end of ANSI functions                                                       */
/*==============================================================================*/


#else


bool AnsiSameText( const String& S1, const String& S2 )
{
  bool result = false;
  result = AnsiCompareText( S1, S2 ) == 0;
  return result;
}


bool AnsiSameStr( const String& S1, const String& S2 )
{
  bool result = false;
  result = AnsiCompareStr( S1, S2 ) == 0;
  return result;
}


Char* AnsiLastChar( const String& S )
{
  Char* result = NULL;
  //!! no MultiByte yet, So we return the Last one.
  result = StrEnd( (Char*) ( ((void*) S.c_str()) ) );  // StrEnd checks for nil
  result--;
  return result;
}


Char* AnsiStrLastChar( Char* str )
{
  Char* result = NULL;
  //!! no MultiByte yet, So we return the Last one.
  result = StrEnd( str );
  result--;
  return result;
}

//////////  sysstr.inc




#define test__0 ( TSet< Char, 0, 255 >() \
                     << Char ( _T('a')) << Char ( _T('b')) << Char ( _T('c')) << Char ( _T('d')) << Char ( _T('e')) << Char ( _T('f')) << Char ( _T('g')) << Char ( _T('h')) << Char ( _T('i')) << Char ( _T('j')) \
                     << Char ( _T('k')) << Char ( _T('l')) << Char ( _T('m')) << Char ( _T('n')) << Char ( _T('o')) << Char ( _T('p')) << Char ( _T('q')) << Char ( _T('r')) << Char ( _T('s')) << Char ( _T('t')) \
                     << Char ( _T('u')) << Char ( _T('v')) << Char ( _T('w')) << Char ( _T('x')) << Char ( _T('y')) << Char ( _T('z')) )
#define test__1 ( TSet< Char, 0, 255 >() \
                     << Char ( _T('A')) << Char ( _T('B')) << Char ( _T('C')) << Char ( _T('D')) << Char ( _T('E')) << Char ( _T('F')) << Char ( _T('G')) << Char ( _T('H')) << Char ( _T('I')) << Char ( _T('J')) \
                     << Char ( _T('K')) << Char ( _T('L')) << Char ( _T('M')) << Char ( _T('N')) << Char ( _T('O')) << Char ( _T('P')) << Char ( _T('Q')) << Char ( _T('R')) << Char ( _T('S')) << Char ( _T('T')) \
                     << Char ( _T('U')) << Char ( _T('V')) << Char ( _T('W')) << Char ( _T('X')) << Char ( _T('Y')) << Char ( _T('Z')) )
#define test__2 ( TSet< int, 0, 255 >() \
                     << int ( 97 ) << int ( 98 ) << int ( 99 ) << int ( 100 ) << int ( 101 ) << int ( 102 ) << int ( 103 ) << int ( 104 ) << int ( 105 ) << int ( 106 ) \
                     << int ( 107 ) << int ( 108 ) << int ( 109 ) << int ( 110 ) << int ( 111 ) << int ( 112 ) << int ( 113 ) << int ( 114 ) << int ( 115 ) << int ( 116 ) \
                     << int ( 117 ) << int ( 118 ) << int ( 119 ) << int ( 120 ) << int ( 121 ) << int ( 122 ) )
#define test__3 ( TSet< int, 0, 255 >() \
                     << int ( 97 ) << int ( 98 ) << int ( 99 ) << int ( 100 ) << int ( 101 ) << int ( 102 ) << int ( 103 ) << int ( 104 ) << int ( 105 ) << int ( 106 ) \
                     << int ( 107 ) << int ( 108 ) << int ( 109 ) << int ( 110 ) << int ( 111 ) << int ( 112 ) << int ( 113 ) << int ( 114 ) << int ( 115 ) << int ( 116 ) \
                     << int ( 117 ) << int ( 118 ) << int ( 119 ) << int ( 120 ) << int ( 121 ) << int ( 122 ) )




/*==============================================================================*/
/*   ANSI String functions                                                      */
/*   These functions rely ON the character set loaded by the OS                 */
/*==============================================================================*/



typedef Char TCaseTranslationTable [ 256/*# range 0..255*/ ];
  /* tables with upper and LowerCase Forms of character sets.
    must be initialized with the correct Code-Pages */


TCaseTranslationTable UpperCaseTable;
TCaseTranslationTable LowerCaseTable;


String GenericAnsiUpperCase( const String& S )
{
  String result;
  int Len = 0, i = 0;
  Len = S.length( );
  result.resize( Len );
  for ( i = 1; i <= Len; i++)
    result[i - 1] = UpperCaseTable[int( S[i - 1] )];
  return result;
}


String GenericAnsiLowerCase( const String& S )
{
  String result;
  int Len = 0, i = 0;
  Len = S.length( );
  result.resize( Len );
  for ( i = 1; i <= Len; i++)
    result[i - 1] = LowerCaseTable[int( S[i - 1] )];
  return result;
}


PtrInt GenericAnsiCompareStr( const String& S1, const String& S2 )
{
  PtrInt result;
  SizeInt i, L1, L2;
  result = 0;
  L1 = S1.length( );
  L2 = S2.length( );
  i = 1;
  while ( ( result == 0 ) && ( ( i <= L1 ) && ( i <= L2 ) ) )
  {
    result = int( S1[i - 1] ) - int( S2[i - 1] ); //!! must be replaced by ANSI characters !!
    i++;
  }
  if ( result == 0 )
    result = L1 - L2;
  return result;
}


PtrInt GenericAnsiCompareText( const String& S1, const String& S2 )
{
  PtrInt result;
  SizeInt i, L1, L2;
  result = 0;
  L1 = S1.length( );
  L2 = S2.length( );
  i = 1;
  while ( ( result == 0 ) && ( ( i <= L1 ) && ( i <= L2 ) ) )
  {
    result = int( LowerCaseTable[int( S1[i - 1] )] ) - int( LowerCaseTable[int( S2[i - 1] )] ); //!! must be replaced by ANSI characters !!
    i++;
  }
  if ( result == 0 )
    result = L1 - L2;
  return result;
}


PtrInt GenericAnsiStrComp( const Char* S1, const Char* S2 )
{
  PtrInt result;
  result = 0;
  if ( S1 == NULL )
  {
    if ( S2 == NULL )
      return result;
    result = - 1;
    return result;
  }
  if ( S2 == NULL )
  {
    result = 1;
    return result;
  }
  while ( ( result == 0 ) && ( *S1 != _T('\x00')) && ( *S2 != _T('\x00')) )
  {
    result = int( *S1 ) - int( *S2 ); //!! must be replaced by ANSI characters !!
    S1++;
    S2++;
  }
  if ( ( result == 0 ) && ( *S1 != *S2 ) ) // loop ended because Exactly one has #0
    if ( *S1 == _T('\x00')) // shorter String is smaller
      result = - 1;
    else
      result = 1;
  return result;
}


PtrInt GenericAnsiStrIComp( const Char* S1, const Char* S2 )
{
  PtrInt result;
  result = 0;
  if ( S1 == NULL )
  {
    if ( S2 == NULL )
      return result;
    result = - 1;
    return result;
  }
  if ( S2 == NULL )
  {
    result = 1;
    return result;
  }
  while ( ( result == 0 ) && ( *S1 != _T('\x00')) && ( *S2 != _T('\x00')) )
  {
    result = int( LowerCaseTable[int( S1[0] )] ) - int( LowerCaseTable[int( S2[0] )] ); //!! must be replaced by ANSI characters !!
    S1++;
    S2++;
  }
  if ( ( result == 0 ) && ( S1[0] != S2[0] ) ) //Length(S1)<>Length(S2)
    if ( S1[0] == _T('\x00'))
      result = - 1; //S1 shorter than S2
    else
      result = 1; //S1 longer than S2
  return result;
}


PtrInt GenericAnsiStrLComp( const Char* S1, const Char* S2, PtrUInt MaxLen )
{
  PtrInt result;
  unsigned int i = 0;
  result = 0;
  if ( MaxLen == 0 )
    return result;
  if ( S1 == NULL )
  {
    if ( S2 == NULL )
      return result;
    result = - 1;
    return result;
  }
  if ( S2 == NULL )
  {
    result = 1;
    return result;
  }
  i = 0;
  do
  {
    result = int( S1[0] ) - int( S2[0] ); //!! must be replaced by ANSI characters !!
    S1++;
    S2++;
    i++;
  }
  while ( ! ( ( result != 0 ) || ( i == MaxLen ) ) );
  return result;
}


PtrInt GenericAnsiStrLIComp( const Char* S1, const Char* S2, PtrUInt MaxLen )
{
  PtrInt result;
  unsigned int i = 0;
  result = 0;
  if ( MaxLen == 0 )
    return result;
  if ( S1 == NULL )
  {
    if ( S2 == NULL )
      return result;
    result = - 1;
    return result;
  }
  if ( S2 == NULL )
  {
    result = 1;
    return result;
  }
  i = 0;
  do
  {
    result = int( LowerCaseTable[int( S1[0] )] ) - int( LowerCaseTable[int( S2[0] )] ); //!! must be replaced by ANSI characters !!
    S1++;
    S2++;
    i++;
  }
  while ( ! ( ( result != 0 ) || ( i == MaxLen ) ) );
  return result;
}


Char* GenericAnsiStrLower( Char* str )
{
  Char* result = NULL;
  result = str;
  if ( str != NULL )
  {
    while ( *str != _T('\x00'))
    {
      *str = LowerCaseTable[((unsigned char) *str )];
      str = str + 1;
    }
  }
  return result;
}


Char* GenericAnsiStrUpper( Char* str )
{
  Char* result = NULL;
  result = str;
  if ( str != NULL )
  {
    while ( *str != _T('\x00'))
    {
      *str = UpperCaseTable[((unsigned char) *str )];
      str = str + 1;
    }
  }
  return result;
}

/////////// sysstr.inc


String AnsiUpperCase( const String& S )
{
#ifdef windows
  String result;
  if ( S.length( ) > 0 )
  {
    result = S;
    //UniqueString( result );
    CharUpperBuff( (Char*) result.c_str(), result.length( ) );
  }
  else
    result = _T("");
  return result;
#elif defined(linux)
  return GenericAnsiUpperCase( S );
#else
#error unknown platform
#endif
}


String AnsiLowerCase( const String& S )
{
#ifdef windows
  String result;
  if ( S.length( ) > 0 )
  {
    result = S;
    //UniqueString( result );
    CharLowerBuff( (Char*) result.c_str(), result.length( ) );
  }
  else
    result = _T("");
  return result;

#elif defined(linux)
  return GenericAnsiLowerCase( S );
#else
#error unknown platform
#endif
}


int AnsiCompareStr( const String& S1, const String& S2 )
{
#ifdef windows
  return CompareString( LOCALE_USER_DEFAULT, 0, S1.c_str(), S1.length( ), S2.c_str(), S2.length( ) ) - 2;
#elif defined(linux)
  return GenericAnsiCompareStr( S1, S2 );
#else
#error unknown platform
#endif
}


int AnsiCompareText( const String& S1, const String& S2 )
{
#ifdef windows
  return CompareString( LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1.c_str(), S1.length( ), S2.c_str(), S2.length( ) ) - 2;
#elif defined(linux)
  return GenericAnsiCompareText( S1, S2 );
#else
#error unknown platform
#endif
}


int AnsiStrComp( const Char* S1, const Char* S2 )
{
#ifdef windows
  return CompareString( LOCALE_USER_DEFAULT, 0, S1, - 1, S2, - 1 ) - 2;
#elif defined(linux)
  return GenericAnsiStrComp( S1, S2 );
#else
#error unknown platform
#endif
}


int AnsiStrIComp( const Char* S1, const Char* S2 )
{
#ifdef windows
  return CompareString( LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, - 1, S2, - 1 ) - 2;
#elif defined(linux)
  return GenericAnsiStrIComp( S1, S2 );
#else
#error unknown platform
#endif
}


int AnsiStrLComp( const Char* S1, const Char* S2, unsignedint MaxLen )
{
#ifdef windows
  return CompareString( LOCALE_USER_DEFAULT, 0, S1, MaxLen, S2, MaxLen ) - 2;
#elif defined(linux)
  return GenericAnsiStrLComp( S1, S2, MaxLen );
#else
#error unknown platform
#endif
}


int AnsiStrLIComp( const Char* S1, const Char* S2, unsignedint MaxLen )
{
#ifdef windows
  return CompareString( LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, MaxLen, S2, MaxLen ) - 2;
#elif defined(linux)
  return GenericAnsiStrLIComp( S1, S2, MaxLen );
#else
#error unknown platform
#endif
}


Char* AnsiStrLower( Char* str )
{
#ifdef windows
  Char* result = NULL;
  CharLower( str );
  return str;
#elif defined(linux)
  return GenericAnsiStrLower( str );
#else
#error unknown platform
#endif
}


Char* AnsiStrUpper( Char* str )
{
#ifdef windows
  Char* result = NULL;
  CharUpper( str );
  return str;
#elif defined(linux)
  return GenericAnsiStrUpper( str );
#else
#error unknown platform
#endif
}


/*==============================================================================*/
/*  end of ANSI functions                                                       */
/*==============================================================================*/

#endif

/*   Trim returns A copy of S with blanks characters ON the Left and Right stripped OFF   */


const TSet < UChar, 0, 255> WhiteSpace = ( TSet < UChar, 0, 255 >() \
                     << Char ( 0 ) << Char ( 1 ) << Char ( 2 ) << Char ( 3 ) << Char ( 4 ) << Char ( 5 ) << Char ( 6 ) << Char ( 7 ) << Char ( 8 ) << Char ( _T('\t')) \
                     << Char ( _T('\n')) << Char ( 11 ) << Char ( 12 ) << Char ( _T('\r')) << Char ( 14 ) << Char ( 15 ) << Char ( 16 ) << Char ( 17 ) << Char ( 18 ) << Char ( 19 ) \
                     << Char ( 20 ) << Char ( 21 ) << Char ( 22 ) << Char ( 23 ) << Char ( 24 ) << Char ( 25 ) << Char ( 26 ) << Char ( 27 ) << Char ( 28 ) << Char ( 29 ) \
                     << Char ( 30 ) << Char ( 31 ) << Char ( _T(' ')) );


String Trim( const String& S )
{
  String result;
  int Ofs = 0, Len = 0;
  Len = S.length( );
  while ( ( Len > 0 ) && ( WhiteSpace.Contains(S[Len - 1] ) ) ) 
    Len--;
  Ofs = 1;
  while ( ( Ofs <= Len ) && ( WhiteSpace.Contains(S[Ofs - 1] ) ) ) 
    Ofs++;
  return S.substr( Ofs - 1, 1 + Len - Ofs );
}

/*   TrimLeft returns A copy of S with all blank characters ON the Left stripped OFF  */


String TrimLeft( const String& S )
{
  String result;
  int i = 0, l = 0;
  l = S.length( );
  i = 1;
  while ( ( i <= l ) && ( WhiteSpace.Contains(S[i - 1] ) ) ) 
    i++;
  return S.substr( i - 1, l );
}

/*   TrimRight returns A copy of S with all blank characters ON the Right stripped OFF  */


String TrimRight( const String& S )
{
  String result;
  int l = 0;
  l = S.length( );
  while ( ( l > 0 ) && ( WhiteSpace.Contains(S[l - 1] ) ) ) 
    l--;
  return S.substr( 1 - 1, l );
}

/*   QuotedStr returns S quoted Left and Right and every Single Quote in S
    replaced by two quotes   */
String QuotedStr( const String& S )
{
  return AnsiQuotedStr( S, _T('\'') );
}

/*   AnsiQuotedStr returns S quoted Left and Right by Quote,
    and every Single occurance of Quote replaced by two   */
String AnsiQuotedStr( const String& S, Char Quote )
{
  String result;
  int i = 0, j = 0, Count = 0;
  result = _T("");
  result = result + Quote;
  Count = S.length( );
  i = 0;
  j = 0;
  while ( i < Count ) 
  {
    i = i + 1;
    if ( S[i - 1] == Quote ) 
    {
      result = result + S.substr( 1 + j - 1, i - j ) + Quote;
      j = i;
    }
  }
  if ( i != j ) 
    result = result + S.substr( 1 + j - 1, i - j );
  return result + Quote;
}

/*   AnsiExtractQuotedStr returns A copy of Src with Quote characters
    deleted to the Left and Right and Double occurances
    of Quote replaced by A Single Quote   */


String AnsiExtractQuotedStr( Char*& Src, Char Quote )
{
  String result;
  Char* P = NULL,* Q = NULL,* r = NULL;
  P = Src;
  Q = StrEnd( P );
  result = _T("");
  if ( P == Q ) 
    return result;
  if ( *P != Quote ) 
    return result;
  P++;
  result.resize( ( Q - P ) + 1 );
  r = &result[1 - 1];
  while ( P != Q ) 
  {
    *r = *P;
    r++;
    if ( *P == Quote ) 
    {
      P = P + 1;
      if ( *P != Quote ) 
      {
        r--;
        break;
      }
    }
    P = P + 1;
  }
  Src = P;
  result.resize( ( r - (Char*) ( &result[1 - 1] ) ) );
  return result;
}


/*   AdjustLineBreaks returns S with all CR characters not followed by LF
    replaced with CR/LF  */
//  under LINUX all CR characters or CR/LF combinations should be replaced with LF



String AdjustLineBreaks( const String& S )
{
  String result;
  result = AdjustLineBreaks( S, DefaultTextLineBreakStyle );
  return result;
}


String AdjustLineBreaks( const String& S, TTextLineBreakStyle Style )
{
  String result;
  Char* Source = NULL,* Dest = NULL;
  int DestLen = 0;
  int i = 0, j = 0, l = 0;
  Source = (Char*) ((void*) S.c_str());
  l = S.length( );
  DestLen = l;
  i = 1;
  while ( i <= l ) 
  {
    switch ( S[i - 1] )
    {
      case _T('\x0a'):
        if ( Style == tlbsCRLF )
          DestLen++;
      break;
      case _T('\x0d'):
        if ( Style == tlbsCRLF ) 
          if ( ( i < l ) && ( S[i + 1 - 1] == '\x0a' ) ) 
            i++;
          else
            DestLen++;
        else
          if ( ( i < l ) && ( S[i + 1 - 1] == _T('\x0a')) )
            DestLen--;
      break;
    }
    i++;
  }
  if ( DestLen == l ) 
    result = S;
  else
  {
    result.resize( DestLen );
    FillChar( &result[1 - 1], DestLen, 0 );
    Dest = (Char*) ((void*) result.c_str());
    j = 0;
    i = 0;
    while ( i < l ) 
      switch ( Source[i] )
      {
        case _T('\x0a'):
        {
          if ( Style == tlbsCRLF ) 
          {
            Dest[j] = _T('\x0d');
            j++;
          }
          Dest[j] = _T('\x0a');
          j++;
          i++;
        }
        break;
        case _T('\x0d'):
        {
          if ( Style == tlbsCRLF ) 
          {
            Dest[j] = _T('\x0d');
            j++;
          }
          Dest[j] = _T('\x0a');
          j++;
          i++;
          if ( Source[i] == _T('\x0a')) 
            i++;
        }
        break;
      default:
        Dest[j] = Source[i];
        j++;
        i++;
      }
  }
  return result;
}


/*   IsValidIdent returns True if the First character of Ident is in:
    'A' to 'Z', 'a' to 'z' or '_' and the following characters are
    ON of: 'A' to 'Z', 'a' to 'z', '0'..'9' or '_'    */


bool IsValidIdent( const String& Ident )
{
  bool result = false;
  int i = 0, Len = 0;
  result = false;
  Len = Ident.length( );
  if ( Len != 0 ) 
  {
    result = Sysutils__10.Contains(Ident[1 - 1] );
    i = 1;
    while ( ( result ) && ( i < Len ) ) 
    {
      i = i + 1;
      result = result && ( Sysutils__11.Contains(Ident[i - 1] ) );
    }
  }
  return result;
}

/*   IntToHex returns A String representing the hexadecimal Value of Value   */


Char HexDigits [ 17 ] = _T("0123456789ABCDEF");


String IntToHex( int Value, int Digits )
{
  String result;
  int i = 0;
  result.resize( Digits );
  for ( i = 0; i <= Digits - 1; i++)
  {
    result[Digits - i - 1] = HexDigits[Value & 15];
    Value = Value >> 4;
  }
  while ( Value != 0 ) 
  {
    result = HexDigits[Value & 15] + result;
    Value = Value >> 4;
  }
  return result;
}


String IntToHex( int64_t Value, int Digits )
{
  String result;
  int i = 0;
  result.resize( Digits );
  for ( i = 0; i <= Digits - 1; i++)
  {
    result[Digits - i - 1] = HexDigits[Value & 15];
    Value = Value >> 4;
  }
  while ( Value != 0 ) 
  {
    result = HexDigits[Value & 15] + result;
    Value = Value >> 4;
  }
  return result;
}


String IntToHex( QWord Value, int Digits )
{
  String result;
  result = IntToHex( ((int64_t) Value ), Digits );
  return result;
}


bool TryStrToInt( const String& S, int& i )
{
  bool result = false;
  int Error = 0;
  Val( S, &i, Error );
  result = Error == 0;
  return result;
}

/*   StrToInt converts the String S to an Integer Value,
    if S does not represent A valid Integer Value EConvertError is raised  */


int StrToInt( const String& S )
{
  int result = 0;
  int Error = 0;
  Val( S, &result, Error );
  if ( Error != 0 ) 
    throw EConvertError( ErrorString(_T(SysConst_SInvalidInteger), S )); 
  return result;
}


int64_t StrToInt64( const String& S )
{
  int64_t result = 0;
  int Error = 0;
  Val( S, &result, Error );
  if ( Error != 0 ) 
    throw EConvertError( ErrorString(_T(SysConst_SInvalidInteger),  S )); 
  return result;
}


bool TryStrToInt64( const String& S, int64_t& i )
{
  bool result = false;
  int Error = 0;
  Val( S, &i, Error );
  result = Error == 0;
  return result;
}


QWord StrToQWord( const String& S )
{
  QWord result;
  int Error = 0;
  Val( S, &result, Error );
  if ( Error != 0 ) 
    throw EConvertError( ErrorString(_T(SysConst_SInvalidInteger), S )); 
  return result;
}


bool TryStrToQWord( const String& S, QWord& Q )
{
  bool result = false;
  int Error = 0;
  Val( S, &Q, Error );
  result = Error == 0;
  return result;
}

/*   StrToIntDef converts the String S to an Integer Value,
    Default is returned in case S does not represent A valid Integer Value  */


int StrToIntDef( const String& S, int deflt )
{
  int result = 0;
  int Error = 0;
  Val( S, &result, Error );
  if ( Error != 0 ) 
    result = deflt;
  return result;
}

/*   StrToInt64Def converts the String S to an Int64 Value,
    Default is returned in case S does not represent A valid Int64 Value  */


int64_t StrToInt64Def( const String& S, int64_t deflt )
{
  int64_t result = 0;
  int Error = 0;
  Val( S, &result, Error );
  if ( Error != 0 ) 
    result = deflt;
  return result;
}

/*   StrToQWordDef converts the String S to an QWord Value,
    Default is returned in case S does not represent A valid QWord Value  */


QWord StrToQWordDef( const String& S, QWord deflt )
{
  QWord result;
  int Error = 0;
  Val( S, &result, Error );
  if ( Error != 0 ) 
    result = deflt;
  return result;
}

/*   LoadStr returns the String Resource Ident.   
String LoadStr( int Ident )
{
  String result;
  result = _T("");
  return result;
} */

/*   FmtLoadStr returns the String Resource Ident and Formats it accordingly   */

/* 
String FmtLoadStr( int Ident, const  ARRAYOFCONST& Args )
{
  String result;
  result = _T("");
  return result;
}
*/

const int feInvalidFormat = 1;
const int feMissingArgument = 2;
const int feInvalidArgIndex = 3;


void DoFormatError( int ErrCode )
{
  String S;
  //!! must be changed to contain Format String...
  S = _T("");
  switch ( ErrCode )
  {
    case feInvalidFormat:
      throw EConvertError(ErrorString( _T(SysConst_SInvalidFormat), S )); 
    break;
    case feMissingArgument:
      throw EConvertError( ErrorString(_T(SysConst_SArgumentMissing), S )); 
    break;
    case feInvalidArgIndex:
      throw EConvertError( ErrorString(_T(SysConst_SInvalidArgIndex), S )); 
    break;
  }
}


//---------------------------------------------------------------------------
String hexstr(intptr_t val, size_t cnt)
{
  static Char HexTbl [ 17 ] = _T("0123456789ABCDEF");
  String result;
  result.resize(cnt);
  int i = 0;
  for ( i = cnt; i >= 1; i--)
  {
    result[i - 1] = HexTbl[val & 0xF];
    val = val >> 4;
  }
  return result;
} 
//---------------------------------------------------------------------------
String space( unsignedchar B )
{
  return String(B, _T(' '));
}
//---------------------------------------------------------------------------

const int MaxDigits = 15;


String FloatToStrFIntl( void* Value, TFloatFormat Format, int Precision, int Digits, TFloatValue ValueType, const TFormatSettings& FormatSettings )
{
  String result;
  int P = 0;
  bool Negative = false, TooSmall = false, TooLarge = false;
  Char DS = _T('\0');
  DS = FormatSettings.DecimalSeparator;
  switch ( Format )
  {
    case ffGeneral:
    {
      switch ( ValueType )
      {
        case fvCurrency:
        {
          if ( ( Precision == - 1 ) || ( Precision > 19 ) ) 
            Precision = 19;
          TooSmall = false;
        }
        break;
      default:
      {
        if ( ( Precision == - 1 ) || ( Precision > MaxDigits ) ) 
          Precision = MaxDigits;
        TooSmall = ( Abs( *((long double*) Value) ) < 0.00001 ) && (*((long double*) &Value) != 0.0 );
      }
      }
      if ( ! TooSmall ) 
      {
        switch ( ValueType )
        {
          case fvDouble:
            Str( *((long double*) Value), 0, Precision, result );
          break;
          case fvSingle:
            Str( (float) *((long double*) Value), 0, Precision, result );
          break;
          case fvCurrency:
              Str( *(Currency*) Value, 0, Precision, result );
          break; 
        default:
          Str( *((long double*) Value), 0, Precision, result );
        }
        P = Pos( _T("."), result );
        if ( P != 0 ) 
          result[P - 1] = DS;
        TooLarge = ( P > Precision + 1 ) || ( Pos( _T("E"), result ) != 0 );
      }
      if ( TooSmall || TooLarge ) 
      {
        result = FloatToStrFIntl( Value, ffExponent, Precision, Digits, ValueType, FormatSettings );
            // strip unneeded zeroes.
        P = Pos( _T("E"), result ) - 1;
        if ( P != - 1 ) 
        {
                /* Delete superfluous +? */
          if ( result[P + 2 - 1] == _T('+'))
            result.erase( P + 2 - 1, 1 );
          while ( ( P > 1 ) && ( result[P - 1] == _T('0')) )
          {
            result.erase( P - 1, 1 );
            P--;
          }
          if ( ( P > 0 ) && ( result[P - 1] == DS ) ) 
          {
            result.erase( P - 1, 1 );
            P--;
          }
        }
      }
      else
        if ( P != 0 )  // we have A DecimalSeparator
        {
            /* it seems that in this unit "Precision" must mean "number of */
            /* significant Digits" rather than "number of Digits After the */
            /* decimal Point" (as it does in the System unit) -> adjust    */
            /* (Precision+1 to Count the decimal Point character)          */
          if ( result[1 - 1] == _T('-'))
            Precision++;
          if ( ( result.length( ) > (unsigned int) Precision + 1 ) && ( Precision + 1 > P ) ) 
          {
            P = Precision + 1;
            result.resize( P );
          }
          P = result.length( );
          while ( ( P > 0 ) && ( result[P - 1] == _T('0')) )
            P--;
          if ( ( P > 0 ) && ( result[P - 1] == DS ) ) 
            P--;
          result.resize( P );
        }
    }
    break;
    case ffExponent:
    {
      if ( ( Precision == - 1 ) || ( Precision > MaxDigits ) ) 
        Precision = MaxDigits;
      switch ( ValueType )
      {
        case fvDouble:
          Str( *((long double*) Value), Precision + 7, result );
        break;
        case fvSingle:
          Str( ((float)*((long double*) Value) ), Precision + 6, result );
        break;
        case fvCurrency:
          Str( (double) *(Currency*) Value, Precision + 8, result );
        break; 
      default:
        Str( *((long double*) Value), Precision + 8, result );
      }
        /* Delete leading spaces */
      while ( result[1 - 1] == _T(' '))
        result.erase( 1 - 1, 1 );
      if ( result[1 - 1] == _T('-'))
        result[3 - 1] = DS;
      else
        result[2 - 1] = DS;
      P = Pos( _T("E"), result );
      if ( P != 0 ) 
      {
        P += 2;
        if ( Digits > 4 ) 
          Digits = 4;
        Digits = result.length( ) - P - Digits + 1;
        if ( Digits < 0 ) 
          result.insert( P - 1, String( _T("0000")).substr( 1, - Digits ) );
        else
          while ( ( Digits > 0 ) && ( result[P - 1] == _T('0')) )
          {
            result.erase( P - 1, 1 );
            if ( (unsigned int) P > result.length( ) ) 
            {
              result.erase( P - 2 - 1, 2 );
              break;
            }
            Digits--;
          }
      }
    }
    break;
    case ffFixed:
    {
      if ( Digits == - 1 ) 
        Digits = 2;
      else
        if ( Digits > 18 ) 
          Digits = 18;
      switch ( ValueType )
      {
        case fvDouble:
          Str( *((long double*) Value), 0, Digits, result );
        break;
        case fvSingle:
          Str( (float) *((long double*) Value), 0, Digits, result );
        break;
        case fvCurrency:
          Str( *(Currency*) Value, 0, Digits, result );
        break; 
      default:
        Str( *((long double*) Value), 0, Digits, result );
      }
      if ( result[1 - 1] == _T(' '))
        result.erase( 1 - 1, 1 );
      P = Pos( _T("."), result );
      if ( P != 0 ) 
        result[P - 1] = DS;
    }
    break;
    case ffNumber:
    {
      if ( Digits == - 1 ) 
        Digits = 2;
      else
        if ( Digits > MaxDigits ) 
          Digits = MaxDigits;
      switch ( ValueType )
      {
        case fvDouble:
          Str( *((long double*) Value), 0, Digits, result );
        break;
        case fvSingle:
          Str( (float) *((long double*) Value), 0, Digits, result );
        break;
        case fvCurrency:
          Str( *(Currency*) Value, 0, Digits, result );
        break; 
      default:
        Str( *((long double*) Value), 0, Digits, result );
      }
      if ( result[1 - 1] == _T(' '))
        result.erase( 1 - 1, 1 );
      P = Pos( _T("."), result );
      if ( P != 0 ) 
        result[P - 1] = DS;
      else
        P = result.length( ) + 1;
      P -= 3;
      while ( P > 1 ) 
      {
        if ( FormatSettings.ThousandSeparator && result[P - 1 - 1] != _T('-'))
          result.insert( P - 1, String(1, FormatSettings.ThousandSeparator) );
        P -= 3;
      }
    }
    break;
    case ffCurrency:
    {
      if ( Digits == - 1 ) 
        Digits = FormatSettings.CurrencyDecimals;
      else
        if ( Digits > 18 ) 
          Digits = 18;
      switch ( ValueType )
      {
        case fvDouble:
          Str( *((long double*) Value), 0, Digits, result );
        break;
        case fvSingle:
          Str( (float) *((long double*) Value), 0, Digits, result );
        break;
        case fvCurrency:
          Str( *(Currency*) Value, 0, Digits, result );
        break; 
      default:
        Str( *((long double*) Value), 0, Digits, result );
      }
      Negative = result[1 - 1] == _T('-');
      if ( Negative ) 
        result.erase( 1 - 1, 1 );
      P = Pos( _T("."), result );
      if ( P != 0 ) 
        result[P - 1] = DS;
      P -= 3;
      while ( P > 1 ) 
      {
	if(FormatSettings.ThousandSeparator)
          result.insert( P - 1, String(1, FormatSettings.ThousandSeparator ));
        P -= 3;
      }
      if ( ! Negative ) 
      {
        switch ( FormatSettings.CurrencyFormat )
        {
          case 0:
            result = FormatSettings.CurrencyString + result;
          break;
          case 1:
            result +=  FormatSettings.CurrencyString;
          break;
          case 2:
            result = FormatSettings.CurrencyString + _T(" ") + result;
          break;
          case 3:
            result += _T(" ")+ FormatSettings.CurrencyString;
          break;
        }
      }
      else
      {
        switch ( NegCurrFormat )
        {
          case 0:
            result = _T("(")+ FormatSettings.CurrencyString + result + _T(")");
          break;
          case 1:
            result = _T("-")+ FormatSettings.CurrencyString + result.c_str();
          break;
          case 2:
            result = FormatSettings.CurrencyString + _T("-")+ result;
          break;
          case 3:
            result = FormatSettings.CurrencyString + result + _T("-");
          break;
          case 4:
            result = _T("(")+ result + FormatSettings.CurrencyString + _T(")");
          break;
          case 5:
            result = _T("-")+ result + FormatSettings.CurrencyString.c_str();
          break;
          case 6:
            result = result + _T("-")+ FormatSettings.CurrencyString;
          break;
          case 7:
            result = result + FormatSettings.CurrencyString + _T("-");
          break;
          case 8:
            result = _T("-")+ result + _T(" ")+ FormatSettings.CurrencyString.c_str();
          break;
          case 9:
            result = _T("-")+ FormatSettings.CurrencyString + _T(" ")+ result.c_str();
          break;
          case 10:
            result = FormatSettings.CurrencyString + _T(" ")+ result + _T("-");
          break;
        }
      }
    }
    break;
  }
  
  return result;
}

String FloatToStrF( Currency Value, TFloatFormat Format, int Precision, int Digits, const TFormatSettings& FormatSettings )
{
  return FloatToStrFIntl( &Value, Format, Precision, Digits, fvCurrency, FormatSettings );
}


String FloatToStrF( Currency Value, TFloatFormat Format, int Precision, int Digits )
{
  return FloatToStrF( Value, Format, Precision, Digits, DefaultFormatSettings );
}


String FloatToStrF( long double Value, TFloatFormat Format, int Precision, int Digits, const TFormatSettings& FormatSettings )
{
  long double e = 0.0;
  e = Value;
  return FloatToStrFIntl( &e, Format, Precision, Digits, fvDouble, FormatSettings );
}


String FloatToStrF( long double Value, TFloatFormat Format, int Precision, int Digits )
{
  return FloatToStrF( Value, Format, Precision, Digits, DefaultFormatSettings );
}

String FloatToStrF( double Value, TFloatFormat Format, int Precision, int Digits, const TFormatSettings& FormatSettings )
{
  long double e = 0.0;
  e = Value;
  return FloatToStrFIntl( &e, Format, Precision, Digits, fvDouble, FormatSettings );
}


String FloatToStrF( double Value, TFloatFormat Format, int Precision, int Digits )
{
  return FloatToStrF( Value, Format, Precision, Digits, DefaultFormatSettings );
}



String FloatToStrF( float Value, TFloatFormat Format, int Precision, int Digits, const TFormatSettings& FormatSettings )
{
  long double e = 0.0;
  e = Value;
  return FloatToStrFIntl( &e, Format, Precision, Digits, fvSingle, FormatSettings );
}


String FloatToStrF( float Value, TFloatFormat Format, int Precision, int Digits )
{
  return FloatToStrF( Value, Format, Precision, Digits, DefaultFormatSettings );
}


String FloatToStrF( int64_t Value, TFloatFormat Format, int Precision, int Digits, const TFormatSettings& FormatSettings )
{
  long double e = 0.0;
  e = (long double) Value;
  return FloatToStrFIntl( &e, Format, Precision, Digits, fvComp, FormatSettings );
}


String FloatToStrF( int64_t Value, TFloatFormat Format, int Precision, int Digits )
{
  return FloatToStrF( Value, Format, Precision, Digits, DefaultFormatSettings );
}


String CurrToStrF( Currency Value, TFloatFormat Format, int Digits, const TFormatSettings& FormatSettings )
{
  return FloatToStrF( Value, Format, 19, Digits, FormatSettings );
}


String CurrToStrF( Currency Value, TFloatFormat Format, int Digits )
{
  return CurrToStrF( Value, Format, Digits, DefaultFormatSettings );
}


TDateTime FloatToDateTime( const long double Value )
{
  TDateTime result = 0.0;
  if ( ( Value < (double) MinDateTime ) || ( Value > (double) MaxDateTime ) ) 
  {
    //throw EConvertError( _T(SysConst_SInvalidDateTime), ARRAYOFCONST( Value) )); 
    String sError;
    Str(Value, sError);
    sError += _T(" is not a valid date/time value.");
    throw EConvertError( sError); 
  }
  result = (double) Value;
  return result;
}




String FloatToStr( Currency Value, const TFormatSettings& FormatSettings )
{
  return FloatToStrFIntl( &Value, ffGeneral, 15, 0, fvCurrency, FormatSettings );
}


String FloatToStr( Currency Value )
{
  return FloatToStr( Value, DefaultFormatSettings );
}


String FloatToStr( double Value, const TFormatSettings& FormatSettings )
{
  String result;
  long double e = 0.0;
  e = Value;
  result = FloatToStrFIntl( &e, ffGeneral, 15, 0, fvDouble, FormatSettings );
  return result;
}


String FloatToStr( double Value )
{
  return FloatToStr( Value, DefaultFormatSettings );
}

String FloatToStr( long double Value )
{
  return FloatToStr( (double) Value, DefaultFormatSettings );
}

String FloatToStr( int Value )
{
  return FloatToStr( (double) Value, DefaultFormatSettings );
}



String FloatToStr( float Value, const TFormatSettings& FormatSettings )
{
  String result;
  long double e = 0.0;
  e = Value;
  result = FloatToStrFIntl( &e, ffGeneral, 15, 0, fvSingle, FormatSettings );
  return result;
}


String FloatToStr( float Value )
{
  return FloatToStr( Value, DefaultFormatSettings );
}


String FloatToStr( int64_t Value, const TFormatSettings& FormatSettings )
{
  String result;
  long double e = 0.0;
  e = (long double) Value;
  result = FloatToStrFIntl( &e, ffGeneral, 15, 0, fvComp, FormatSettings );
  return result;
}


String FloatToStr( int64_t Value )
{
  return FloatToStr( Value, DefaultFormatSettings );
}





int FloatToText( Char* Buffer, long double Value, TFloatFormat Format, int Precision, int Digits, const TFormatSettings& FormatSettings )
{
  String tmp = FloatToStrF( (double) Value, Format, Precision, Digits, FormatSettings );;
  return tmp.length();
}


int FloatToText( Char* Buffer, long double Value, TFloatFormat Format, int Precision, int Digits )
{
  int result = 0;
  result = FloatToText( Buffer, Value, Format, Precision, Digits, DefaultFormatSettings );
  return result;
}


void ReadInteger( int64_t& Value, const String& fmt, const  VECTOROFCONST& Args, int& ChPos, int& OldPos, unsigned int& ArgPos, int& Len) 
{
  int Code = 0;
  if ( Value != - 1 ) 
    return; // was Already read.
  OldPos = ChPos;
  while ( ( ChPos <= Len ) && ( Pos( fmt[ChPos - 1], String( _T("1234567890")) ) != 0 ) ) 
    ChPos++;
  if ( ChPos > Len ) 
    DoFormatError( feInvalidFormat );
  if ( fmt[ChPos - 1] == '*' ) 
  {
    if ( ( ChPos > OldPos ) || ( ArgPos > Args.size() - 1 /*# High(Args) */ ) ) 
      DoFormatError( feInvalidFormat );
    switch ( Args[ArgPos].VType )
    {
      case vtInteger:
        Value = Args[ArgPos].VInteger;
      break;
      case vtInt64:
        Value = *Args[ArgPos].VInt64;
      break;
      case vtUInt64:
        Value = *Args[ArgPos].VUInt64;
      break;
    default:
      DoFormatError( feInvalidFormat );
    }
    ArgPos++;
    ChPos++;
  }
  else
  {
    if ( OldPos < ChPos ) 
    {
      Val( fmt.substr( OldPos - 1, ChPos - OldPos ), &Value, Code );
      // this should never happen !!
      if ( Code > 0 ) 
        DoFormatError( feInvalidFormat );
    }
    else
      Value = - 1;
  }
}


void ReadIndex( int64_t& Value, const String& fmt, const  VECTOROFCONST& Args, int& ChPos, int& OldPos, unsigned int& ArgPos, int& Len, int64_t& Index) 
{
  if ( fmt[ChPos - 1] != ':' ) 
    ReadInteger(Value, fmt, Args, ChPos, OldPos, ArgPos, Len);
  else
    Value = 0; // Delphi undocumented behaviour, assume 0, #11099
  if ( fmt[ChPos - 1] == ':' ) 
  {
    if ( Value == - 1 ) 
      DoFormatError( feMissingArgument );
    Index = Value;
    Value = - 1;
    ChPos++;
  }
}


void ReadLeft( const String& fmt, int& ChPos, bool& Left) 
{
  if ( fmt[ChPos - 1] == '-' ) 
  {
    Left = true;
    ChPos++;
  }
  else
    Left = false;
}


void ReadWidth( int64_t& Value, const String& fmt, const  VECTOROFCONST& Args, int& ChPos, int& OldPos, unsigned int& ArgPos, int& Len,  int& Width)
{
  ReadInteger(Value, fmt, Args, ChPos, OldPos, ArgPos, Len);
  if ( Value != - 1 ) 
  {
    Width = (int) Value;
    Value = - 1;
  }
}


void ReadPrec( int64_t& Value, const String& fmt, const  VECTOROFCONST& Args, int& ChPos, int& OldPos, unsigned int& ArgPos, int& Len,  int& prec) 
{
  if ( fmt[ChPos - 1] == '.' ) 
  {
    ChPos++;
    ReadInteger(Value, fmt, Args, ChPos, OldPos, ArgPos, Len);
    if ( Value == - 1 ) 
      Value = 0;
    prec = (int) Value;
  }
}


Char ReadFormat( const String& fmt, const  VECTOROFCONST& Args, int& ChPos, int& OldPos, unsigned int& ArgPos, unsigned int DoArg, int& Len, int64_t& Index,  int& Width,  int& prec, bool& Left )
{
  Char result = _T('\0');
  int64_t Value = 0;
  Index = - 1;
  Width = - 1;
  prec = - 1;
  Value = - 1;
  ChPos++;
  if ( fmt[ChPos - 1] == '%' ) 
  {
    result = _T('%');
    return result;                           // vp fix
  }
  ReadIndex( Value, fmt, Args, ChPos, OldPos, ArgPos, Len, Index);
  ReadLeft( fmt, ChPos, Left);
  ReadWidth( Value, fmt, Args, ChPos, OldPos, ArgPos, Len, Width);
  ReadPrec( Value, fmt, Args, ChPos, OldPos, ArgPos, Len, prec);
  return toupper( fmt[ChPos - 1] );
}


bool Checkarg( int at, bool err, const  VECTOROFCONST& Args, unsigned int& ArgPos, unsigned int DoArg, int64_t& Index)
/*
  Check if argument Index is of correct type (at)
  if Index=-1, ArgPos is used, and ArgPos is augmented with 1
  DoArg is set to the argument that must be used.
*/
{
  bool result = false;
  if ( Index == - 1 ) 
    DoArg = ArgPos;
  else
    DoArg = (unsigned int) Index;
  ArgPos = DoArg + 1;
  if ( ( DoArg > Args.size() - 1 /*# High(Args) */ ) || ( Args[DoArg].VType != at ) ) 
  {
    if ( err ) 
      DoFormatError( feInvalidArgIndex );
    ArgPos--;
    return result;
  }
  return true;
}


String Format( const String& fmt, const  VECTOROFCONST& Args, const TFormatSettings& FormatSettings )
{
  String result;
  int ChPos = 0, OldPos = 0, DoArg = 0, Len = 0;
  unsigned ArgPos = 0;
  String hs, ToAdd;
  int64_t Index = 0;
  int Width = 0, prec = 0;
  bool Left = false;
  Char FCHAR = _T('\0');
  QWord vq;

  /*
    ReadFormat reads the Format String. it returns the type character in
    UpperCase, and sets Index, Width, prec to their correct values,
    or -1 if not set. it sets Left to True if Left alignment was Requested.
    in case of an Error, DoFormatError is called.
  */
  const Char zero[] = _T("000000000000000000000000000000000000000000000000000000000000000");
  result = _T("");
  Len = fmt.length( );
  ChPos = 1;
  OldPos = 1;
  ArgPos = 0;
  while ( ChPos <= Len ) 
  {
    while ( ( ChPos <= Len ) && ( fmt[ChPos - 1] != '%' ) ) 
      ChPos++;
    if ( ChPos > OldPos ) 
      result = result + fmt.substr( OldPos - 1, ChPos - OldPos );
    if ( ChPos < Len ) 
    {
      FCHAR = ReadFormat(fmt, Args, ChPos, OldPos, ArgPos, DoArg, Len, Index, Width, prec, Left);
      switch ( FCHAR )
      {
        case _T('D'):
        {
          if ( Checkarg( vtInteger, false, Args, ArgPos, DoArg, Index ) ) 
            Str( Args[DoArg].VInteger, ToAdd );
          else
            if ( Checkarg( vtInt64, false, Args, ArgPos, DoArg, Index ) ) 
              Str( *Args[DoArg].VInt64, ToAdd );
            else
              if ( Checkarg( vtUInt64, true, Args, ArgPos, DoArg, Index ) ) 
                Str( ((int64_t) *Args[DoArg].VUInt64 ), ToAdd );
          Index = (int64_t) prec - ToAdd.length();
          if(Index > 0) // dme
          {
            if ( ToAdd[1 - 1] != '-' ) 
              ToAdd = String((int) Index,  _T('0') ) + ToAdd;
            else
            {
              // + 1 to accomodate for - sign in Length !!
              String tmp = String( (int) Index + 1 , _T('0'));
              ToAdd.insert( 1, tmp );
            }
          }
        }
        break;
        case _T('U'):
        {
          if ( Checkarg( vtInteger, false, Args, ArgPos, DoArg, Index ) ) 
            Str( ((unsigned int) Args[DoArg].VInteger ), ToAdd );
          else
            if ( Checkarg( vtInt64, false, Args, ArgPos, DoArg, Index ) ) 
              Str( QWord( *Args[DoArg].VInt64 ), ToAdd );
            else
              if ( Checkarg( vtUInt64, true, Args, ArgPos, DoArg, Index ) ) 
                Str( *Args[DoArg].VUInt64, ToAdd );
          Width = Abs( Width );
          Index = (int64_t) prec - ToAdd.length();
          if(Index > 0) // dme
            ToAdd = String( (int) Index, _T('0') ) + ToAdd;
        }
        break;
        case _T('E'):
        {
          if ( Checkarg( vtCurrency, false, Args, ArgPos, DoArg, Index ) ) 
            ToAdd = FloatToStrF( *Args[DoArg].VCurrency, ffExponent, prec, 3, FormatSettings );
          else
            if ( Checkarg( vtExtended, true, Args, ArgPos, DoArg, Index ) ) 
              ToAdd = FloatToStrF( *Args[DoArg].VExtended, ffExponent, prec, 3, FormatSettings );
        }
        break;
        case _T('F'):
        {
          if ( Checkarg( vtCurrency, false, Args, ArgPos, DoArg, Index ) ) 
            ToAdd = FloatToStrF( *Args[DoArg].VCurrency, ffFixed, 9999, prec, FormatSettings );
          else
            if ( Checkarg( vtExtended, true, Args, ArgPos, DoArg, Index ) ) 
              ToAdd = FloatToStrF( *Args[DoArg].VExtended, ffFixed, 9999, prec, FormatSettings );
        }
        break;
        case _T('G'):
        {
          if ( Checkarg( vtCurrency, false, Args, ArgPos, DoArg, Index ) ) 
            ToAdd = FloatToStrF( *Args[DoArg].VCurrency, ffGeneral, prec, 3, FormatSettings );
          else
            if ( Checkarg( vtExtended, true, Args, ArgPos, DoArg, Index ) ) 
              ToAdd = FloatToStrF( *Args[DoArg].VExtended, ffGeneral, prec, 3, FormatSettings );
        }
        break;
        case _T('N'):
        {
          if ( Checkarg( vtCurrency, false, Args, ArgPos, DoArg, Index ) ) 
            ToAdd = FloatToStrF( *Args[DoArg].VCurrency, ffNumber, 9999, prec, FormatSettings );
          else
            if ( Checkarg( vtExtended, true, Args, ArgPos, DoArg, Index ) ) 
              ToAdd = FloatToStrF( *Args[DoArg].VExtended, ffNumber, 9999, prec, FormatSettings );
        }
        break;
        case _T('M'):
        {
          if ( Checkarg( vtExtended, false, Args, ArgPos, DoArg, Index ) ) 
            ToAdd = FloatToStrF( *Args[DoArg].VExtended, ffCurrency, 9999, prec, FormatSettings );
          else
            if ( Checkarg( vtCurrency, true, Args, ArgPos, DoArg, Index ) ) 
              ToAdd = FloatToStrF( *Args[DoArg].VCurrency, ffCurrency, 9999, prec, FormatSettings );
        }
        break;
        case _T('S'):
        {
          if ( Checkarg( vtString, false, Args, ArgPos, DoArg, Index ) ) 
#ifdef _WIDESTRING
            hs = str2wstr((*Args[DoArg].VString).c_str());
#else
            hs = *Args[DoArg].VString;
#endif
          else
            if ( Checkarg( vtChar, false, Args, ArgPos, DoArg, Index ) ) 
#ifdef _WIDESTRING
              hs = char2wchar(Args[DoArg].VChar);
#else
              hs = Args[DoArg].VChar;
#endif
            else
              if ( Checkarg( vtPChar, false, Args, ArgPos, DoArg, Index ) ) 
#ifdef _WIDESTRING
                hs = str2wstr(Args[DoArg].VPChar);
#else
                hs = Args[DoArg].VPChar;
#endif
              else
                if ( Checkarg( vtPWideChar, false, Args, ArgPos, DoArg, Index ) ) 
#ifdef _WIDESTRING
                  hs = Args[DoArg].VPWideChar;
#else
                  hs = wstr2str(Args[DoArg].VPWideChar);
#endif
                else
                  if ( Checkarg( vtWideChar, false, Args, ArgPos, DoArg, Index ) ) 
#ifdef _WIDESTRING
                    hs = String(1, Args[DoArg].VWideChar );
#else
                    hs = String(1, wchar2char(Args[DoArg].VWideChar ));
#endif
                  else
                    if ( Checkarg( vtWideString, false, Args, ArgPos, DoArg, Index ) ) 
#ifdef _WIDESTRING
                      hs = (wchar_t*) Args[DoArg].VWideString;
#else
                      hs = wstr2str( (wchar_t*) Args[DoArg].VWideString );
#endif
                    else
                      if ( Checkarg( vtAnsiString, true, Args, ArgPos, DoArg, Index ) ) 
#ifdef _WIDESTRING
                        hs = str2wstr((char*) Args[DoArg].VAnsiString );
#else
                        hs = (char*) Args[DoArg].VAnsiString;
#endif
          Index = hs.length();
          if ( ( prec != - 1 ) && ( Index > prec ) ) 
            Index = prec;
          if(Index > 0)
            ToAdd = hs.substr( 0, (unsigned int) Index );
          else
            ToAdd = _T("");
        }
        break;
        case _T('P'):
        {
          Checkarg( vtPointer, true, Args, ArgPos, DoArg, Index );
          ToAdd = hexstr( intptr_t(Args[DoArg].VPointer), sizeof(intptr_t) * 2 );
          // Insert ':'. is this needed in 32 bit ? no it isn'T.
          // Insert(':',ToAdd,5);
        }
        break;
        case _T('X'):
        {
          if ( Checkarg( vtInteger, false, Args, ArgPos, DoArg, Index ) ) 
          {
            vq = ((unsigned int) Args[DoArg].VInteger );
            Index = 16;
          }
          else
            if ( Checkarg( vtUInt64, false, Args, ArgPos, DoArg, Index ) ) 
            {
              vq = QWord( *Args[DoArg].VUInt64 );
              Index = 31;
            }
            else
            {
              Checkarg( vtInt64, true, Args, ArgPos, DoArg, Index );
              vq = QWord( *(Args[DoArg].VInt64) );
              Index = 31;
            }
          if ( prec > Index ) 
            ToAdd = hexstr( (int) vq, (unsigned char) Index );
          else
          {
                // determine minimum needed number of hex Digits.
            Index = 1;
            while ( ( ( QWord( 1 ) << ( Index * 4 ) ) <= vq ) && ( Index < 16 ) ) 
              Index++;
            if ( Index > prec ) 
              prec = (int) Index;
            ToAdd = hexstr( (int) vq, (unsigned char) prec );
          }
        }
        break;
        case _T('%'):
          ToAdd = '%';
        break;
      }
      if ( Width != - 1 ) 
        if ( ToAdd.length() < (unsigned int) Width ) 
        {
          if ( ! Left ) 
            ToAdd = space( Width - ToAdd.length()) + ToAdd;
          else
            ToAdd = ToAdd + space( Width - ToAdd.length());
        }
      result = result + ToAdd;
    }
    ChPos++;
    OldPos = ChPos;
  }
  return result;
}
/*$macro OFF*/


String Format( const String& fmt, const  VECTOROFCONST& Args )
{
  return Format( fmt, Args, DefaultFormatSettings );
}


/*
unsignedint FormatBuf( void* Buffer, unsignedint BufLen, void* fmt, unsignedint FmtLen,  const  ARRAYOFCONST& Args, const TFormatSettings& FormatSettings )
{
  unsignedint result = 0;
  String S, F;
  F.resize( FmtLen );
  if ( FmtLen > 0 ) 
    Move( fmt, &F[1 - 1], FmtLen  );
  S = Format( F, Args, FormatSettings );
  if ( ((unsigned int) S.length( ) ) < BufLen ) 
    result = S.length( );
  else
    result = BufLen;
  Move( &S[1 - 1], Buffer, result );
  return result;
}


unsignedint FormatBuf( void* Buffer, unsignedint BufLen, void* fmt, unsignedint FmtLen, const  ARRAYOFCONST& Args )
{
  unsignedint result = 0;
  result = FormatBuf( Buffer, BufLen, fmt, FmtLen, Args, DefaultFormatSettings );
  return result;
}
*/
unsignedint FormatBuf( Char* Buffer, unsignedint BufLen, const Char* fmt, unsignedint FmtLen, const  VECTOROFCONST& Args, const TFormatSettings& FormatSettings )
{
  unsignedint result = 0;
  String S, F;
  F.resize( FmtLen );
  if ( FmtLen > 0 ) 
    Move( fmt, F, FmtLen );
  S = Format( F, Args, FormatSettings );
  if ( ((unsigned int) S.length( ) ) < BufLen ) 
    result = S.length( );
  else
    result = BufLen;
  Move( S, Buffer, result );
  return result;
}


unsignedint FormatBuf( Char* Buffer, unsignedint BufLen, const Char* fmt, unsignedint FmtLen, const  VECTOROFCONST& Args )
{
  return FormatBuf( Buffer, BufLen, fmt, FmtLen, Args, DefaultFormatSettings );
}


void FmtStr( String& RES, const String& fmt, const  VECTOROFCONST& Args, const TFormatSettings& FormatSettings )
{
  RES = Format( fmt, Args, FormatSettings );
}


void FmtStr( String& RES, const String& fmt, const  VECTOROFCONST& Args )
{
  FmtStr( RES, fmt, Args, DefaultFormatSettings );
}


Char* StrFmt( Char* Buffer, const Char* fmt, const  VECTOROFCONST& Args, const TFormatSettings& FormatSettings )
{
  Char* result = NULL;
#ifdef _WIDESTRING
  Buffer[FormatBuf( Buffer, MAXINT, fmt, wcslen( fmt ), Args, FormatSettings )] = _T('\x00');
#else
  Buffer[FormatBuf( Buffer, MAXINT, fmt, strlen( fmt ), Args, FormatSettings )] = _T('\x00');
#endif
  return Buffer;
}


Char* StrLFmt( Char* Buffer, unsignedint MaxLen, const Char* fmt, const  VECTOROFCONST& Args, const TFormatSettings& FormatSettings )
{
  Char* result = NULL;
#ifdef _WIDESTRING
  Buffer[FormatBuf( Buffer, MaxLen, fmt, wcslen( fmt ), Args, FormatSettings )] = _T('\x00');
#else
  Buffer[FormatBuf( Buffer, MaxLen, fmt, strlen( fmt ), Args, FormatSettings )] = _T('\x00');
#endif
  return Buffer;
}

Char* StrFmt( Char* Buffer, const Char* fmt, const  VECTOROFCONST& Args )
{
  return StrFmt( Buffer, fmt, Args, DefaultFormatSettings );
}

Char* StrLFmt( Char* Buffer, unsignedint MaxLen, const Char* fmt, const  VECTOROFCONST& Args )
{
  return StrLFmt( Buffer, MaxLen, fmt, Args, DefaultFormatSettings );
}

bool TextToFloat( const Char* Buffer, long double& Value, const TFormatSettings& FormatSettings )
{
  bool result = false;
  int e = 0, P = 0;
  String S;
  S = Buffer;
  P = Pos( FormatSettings.DecimalSeparator, S );
  if ( P != 0 ) 
    S[P - 1] = '.';
  Val( Trim( S ), &Value, e );
  return ( e == 0 );
}


bool TextToFloat( const Char* Buffer, void* Value, TFloatValue ValueType, const TFormatSettings& FormatSettings )
{
  bool result = false;
  int e = 0, P = 0;
  String S;
  long double TempValue = 0.0;
  S = Buffer;
  P = Pos( FormatSettings.ThousandSeparator, S );
  while ( P != 0 ) 
  {
    S.erase( P - 1, 1 );
    P = Pos( FormatSettings.ThousandSeparator, S );
  }
  P = Pos( FormatSettings.DecimalSeparator, S );
  if ( P != 0 ) 
    S[P - 1] = _T('.');
  switch ( ValueType )
  {
    case fvCurrency:
    {
        // needed for platforms Where Currency = Int64
      Val( S, &TempValue, e );
      Currency Value = (double) TempValue;
    }
    break;
    case fvExtended:
      Val( S, ((long double*) Value), e );
    break;
    case fvDouble:
      Val( S, ((double*) Value), e );
    break;
    case fvSingle:
      Val( S, ((float*) Value), e );
    break;
    case fvComp:
      Val( S, ((complex<double>*) Value), e );
    break;
    case fvReal:
      Val( S, ((double*) Value), e );
    break;
  }
  return ( e == 0 );
}

bool TextToFloat( const Char* Buffer, long double& Value )
{
  return TextToFloat( Buffer, Value, DefaultFormatSettings );
}



bool TryStrToFloat( const String& S, float& Value, const TFormatSettings& FormatSettings )
{
  return TextToFloat( (Char*) ( ((void*) S.c_str()) ), &Value, fvSingle, FormatSettings );
}


bool TryStrToFloat( const String& S, double& Value, const TFormatSettings& FormatSettings )
{
  return TextToFloat( (Char*) ( ((void*) S.c_str()) ), &Value, fvDouble, FormatSettings );
}

bool TextToFloat( const Char* Buffer, void* Value, TFloatValue ValueType )
{
  return TextToFloat( Buffer, Value, ValueType, DefaultFormatSettings );
}


bool TryStrToFloat( const String& S, float& Value )
{
  return TryStrToFloat( S, Value, DefaultFormatSettings );
}

bool TryStrToFloat( const String& S, double& Value )
{
  return TryStrToFloat( S, Value, DefaultFormatSettings );
}


long double StrToFloatDef( const String& S, const long double deflt, const TFormatSettings& FormatSettings )
{
  long double result = 0.0;
  if ( ! TextToFloat( S.c_str(), (void*) &result, fvExtended, FormatSettings ) ) 
    result = deflt;
  return result;
}

long double StrToFloat( const String& S, const TFormatSettings& FormatSettings )
{
  long double result = 0.0; // TextToFloat handles nil properly
  if ( ! TextToFloat( (Char*) ( ((void*) S.c_str()) ), result, FormatSettings ) ) 
    throw EConvertError( ErrorString(_T(SysConst_SInvalidFloat), S ));
  return result;
}

long double StrToFloat( const String& S )
{
  return StrToFloat( S, DefaultFormatSettings );
}


long double StrToFloatDef( const String& S, const long double deflt )
{
  long double result = 0.0;
  return StrToFloatDef( S, deflt, DefaultFormatSettings );
}




String AnsiDequotedStr( const String& S, Char AQuote )
{
  String result;
  Char* P = NULL;
  P = (Char*) ( ((void*) S.c_str()) ); // work around const. Ansiextract is safe for nil
  result = AnsiExtractQuotedStr( P, AQuote );
  if ( result.empty()) 
    result = S;
  return result;
}




bool StrToBool( const String& S )
{
  bool result = false;
  if ( ! ( TryStrToBool( S, result ) ) ) 
    throw EConvertError( ErrorString(_T(SysConst_SInvalidBoolean), S )); 
  return result;
}


void CheckStrs( bool& B, bool& UseBoolStrs, String& result )
{
  if ( TrueBoolStrs.size( ) == 0 ) 
  {
    TrueBoolStrs.resize( 1 );
    TrueBoolStrs[0] = _T("True");
  }
  if ( FalseBoolStrs.size( ) == 0 ) 
  {
    FalseBoolStrs.resize( 1 );
    FalseBoolStrs[0] = _T("False");
  }
}


String BoolToStr( bool B, bool UseBoolStrs )
{
  String result;
  if ( UseBoolStrs ) 
  {
    CheckStrs(B, UseBoolStrs, result);
    if ( B ) 
      result = TrueBoolStrs[0];
    else
      result = FalseBoolStrs[0];
  }
  else
    if ( B ) 
      result = _T("-1");
    else
      result = _T("0");
  return result;
}


bool StrToBoolDef( const String& S, bool deflt )
{
  bool result = false;
  if ( ! ( TryStrToBool( S, result ) ) ) 
    result = deflt;
  return result;
}


bool TryStrToBool( const String& S, bool& Value )
{
  bool result = false;
  String Temp;
  double D = 0.0;
  int Code = 0;
  Temp = UpperCase( S );
  Val( Temp, &D, Code );
  result = true;
  if ( Code == 0 ) 
    Value = ( D != 0.0 );
  else
    if ( Temp == _T("TRUE")) 
      Value = true;
    else
      if ( Temp == _T("FALSE")) 
        Value = false;
      else
        result = false;
  return result;
}


bool GetSectionEnd( Char*& P )
{
  bool result = false;
  Char C = _T('\0');
  bool SQ = false, DQ = false;
  result = false;
  SQ = false;
  DQ = false;
  C = P[0];
  while ( ( C != _T('\x00')) && ( ( C != _T(';')) || SQ || DQ ) ) 
  {
    result = true;
    switch ( C )
    {
      case _T('\x22'):
        if ( ! SQ ) 
          DQ = ! DQ;
      break;
      case _T('\x27'):
        if ( ! DQ ) 
          SQ = ! SQ;
      break;
    }
    P++;
    C = P[0];
  }
  return result;
}


void GetSectionRange( int section, long double& Value, Char*& Format, Char*& FmtStart, Char*& FmtStop )
{
  Char* Sec[ 3/*# range 1..3*/ ];
  bool SecOk[ 3/*# range 1..3*/ ];
  Sec[1 - 1] = Format;
  SecOk[1 - 1] = GetSectionEnd( Sec[1 - 1] );
  if ( section > 1 ) 
  {
    Sec[2 - 1] = Sec[1 - 1];
    if ( Sec[2 - 1][0] != _T('\x00')) 
      Sec[2 - 1]++;
    SecOk[2 - 1] = GetSectionEnd( Sec[2 - 1] );
    if ( section > 2 ) 
    {
      Sec[3 - 1] = Sec[2 - 1];
      if ( Sec[3 - 1][0] != _T('\x00')) 
        Sec[3 - 1]++;
      SecOk[3 - 1] = GetSectionEnd( Sec[3 - 1] );
    }
  }
  if ( ! SecOk[1 - 1] ) 
    FmtStart = NULL;
  else
  {
    if ( ! SecOk[section - 1] ) 
      section = 1;
    else
      if ( section == 2 ) 
        Value = - Value;   /* remove sign */

  /* find Format section ranging from FmtStart to FmtStop. */
    if ( section == 1 ) 
      FmtStart = Format;
    else
    {
      FmtStart = Sec[section - 1 - 1];
      FmtStart++;
    }
    FmtStop = Sec[section - 1];
  }
}

void GetFormatOptions(  Char*& FmtStart, Char*& FmtStop, int& ExpFmt, int& ExpSize, int* Placehold, bool& thousand )
{
  Char* fmt = NULL;
  bool SQ = false, DQ = false;
  int area = 0;
  SQ = false;
  DQ = false;
  fmt = FmtStart;
  ExpFmt = 0;
  area = 1;
  thousand = false;
  Placehold[1 - 1] = 0;
  Placehold[2 - 1] = 0;
  Placehold[3 - 1] = 0;
  Placehold[4 - 1] = 0;
  while ( fmt < FmtStop ) 
  {
    switch ( fmt[0] )
    {
      case _T('\x22'):
      {
        if ( ! SQ ) 
          DQ = ! DQ;
        fmt++;
      }
      break;
      case _T('\x27'):
      {
        if ( ! DQ ) 
          SQ = ! SQ;
        fmt++;
      }
      break;
    default:
       /* if not in quotes, then interpret*/
      if ( ! ( SQ || DQ ) ) 
      {
        switch ( fmt[0] )
        {
          case _T('0'):
          {
            switch ( area )
            {
              case 1:
                area = 2;
              break;
              case 4:
              {
                area = 3;
                Placehold[3 - 1] += Placehold[4 - 1];
                Placehold[4 - 1] = 0;
              }
              break;
            }
            Placehold[area - 1]++;
            fmt++;
          }
          break;
          case _T('#'):
          {
            if ( area == 3 ) 
              area = 4;
            Placehold[area - 1]++;
            fmt++;
          }
          break;
          case _T('.'):
          {
            if ( area < 3 ) 
              area = 3;
            fmt++;
          }
          break;
          case _T(','):
          {
            thousand = true;
            fmt++;
          }
          break;
          case _T('e'): case _T('E'):
            if ( ExpFmt == 0 ) 
            {
              if ( fmt[0] == _T('E')) 
                ExpFmt = 1;
              else
                ExpFmt = 3;
              fmt++;
              if ( fmt < FmtStop ) 
              {
                switch ( fmt[0] )
                {
                  case _T('+'):
                  {
                  }
                  break;
                  case _T('-'):
                    ExpFmt++;
                  break;
                default:
                  ExpFmt = 0;
                }
                if ( ExpFmt != 0 ) 
                {
                  fmt++;
                  ExpSize = 0;
                  while ( ( fmt < FmtStop ) && ( ExpSize < 4 ) && ( Sysutils__12.Contains(fmt[0] ) ) ) 
                  {
                    ExpSize++;
                    fmt++;
                  }
                }
              }
            }
            else
              fmt++;
          break;
        default: /* case */
          fmt++;
        } /* case */
      }  /* begin */
      else
        fmt++;
    } /* case */
  } /* While .. begin */
}




void FloatToStr( long double& Value, String& Digits, String& Exponent, int& ExpFmt, int& ExpSize, int* Placehold, int& UnexpectedDigits, int& DigitExponent )
{
  int i = 0, j = 0, Exp = 0, Width = 0, Decimals = 0, DecimalPoint = 0, Len = 0;
  if ( ExpFmt == 0 ) 
  {
      /* Fixpoint */
    Decimals = Placehold[3 - 1] + Placehold[4 - 1];
    Width = Placehold[1 - 1] + Placehold[2 - 1] + Decimals;
    /*
    if ( Decimals == 0 ) 
      Str( Value, Width, 0, Digits );
    else
      Str( Value, Width + 1, Decimals, Digits );
*/
    Char buf[160];
#ifdef _WIDESTRING
    if ( Decimals == 0 ) 
      swprintf(buf, 160, L"%*.*f", Width, 0, Value);
     else
      swprintf(buf, 160, L"%*.*f", Width + 1, 0, Value);
#else
	if ( Decimals == 0 )
	  sprintf(buf, "%*.*Lf", Width, 0, Value);
	 else
	  sprintf(buf, "%*.*Lf", Width + 1, 0, Value);
#endif
    Digits = buf;

    Len = Digits.size(); // [0];
      /* find the decimal Point */
    if ( Decimals == 0 ) 
      DecimalPoint = Len + 1;
    else
      DecimalPoint = Len - Decimals;
      /* if Value is very small, and no decimal places
        are desired, remove the leading 0.            */
    if ( ( Abs( Value ) < 1 ) && ( Placehold[2 - 1] == 0 ) ) 
    {
      if ( Placehold[1 - 1] == 0 ) 
        Digits.erase( max(0, DecimalPoint - 1 - 1), 1 );
      else
        Digits[DecimalPoint - 1 - 1] = _T(' ');
    }

      /* convert Optional zeroes to spaces. */
    i = Len;
    j = DecimalPoint + Placehold[3 - 1];
    while ( ( i > j ) && ( Digits[i - 1] == _T('0') ) )
    {
      Digits[i - 1] = _T(' ');
      i--;
    }
      /* if Integer Value and no obligatory decimal
        places, remove decimal Point. */
    if ( ( DecimalPoint < Len ) && ( Digits[DecimalPoint + 1 - 1] == _T(' ') ) )
      Digits[DecimalPoint - 1] = _T(' ');
      /* convert spaces Left from obligatory decimal Point to zeroes. */
    i = DecimalPoint - Placehold[2 - 1];
    while ( ( i < DecimalPoint ) && ( Digits[i - 1] == _T(' ') ) )
    {
      Digits[i - 1] = _T('0');
      i++;
    }
    Exp = 0;
  }
  else
  {
      /* Scientific: Exactly <Width> Digits with <Precision> Decimals
        and adjusted Exponent. */
    if ( Placehold[1 - 1] + Placehold[2 - 1] == 0 ) 
      Placehold[1 - 1] = 1;
    Decimals = Placehold[3 - 1] + Placehold[4 - 1];
    Width = Placehold[1 - 1] + Placehold[2 - 1] + Decimals;
      /* depending ON the maximally supported Precision, the Exponent field */
      /* is longer/shorter                                                  */
    Str( Value, Width + 6, Digits );  // dme: 1234.567 : 14 -> 1.234567E+0003

      /* find and cut out Exponent. always the
        Last 6 characters in the String.
        -> 0000e+0000                         
        *** no, not always the Last 6 characters, this depends ON
            the maximally supported Precision (JM)*/
    i = Pos( _T("E"), Digits );
    Val( Digits.substr( i + 1 - 1, 255 ), &Exp, j );
    Exp = Exp + 1 - ( Placehold[1 - 1] + Placehold[2 - 1] );
    //Digits.erase( i - 1, 255 );
    if(i > 0)
      Digits.erase( i - 1 ); // if len parameter is missing all remaining characters are removed
      /* str() always returns at least one digit After the decimal Point.
        if we don'T Want it, we have to remove it. */
    if ( ( Decimals == 0 ) && ( Placehold[1 - 1] + Placehold[2 - 1] <= 1 ) ) 
    {
      if ( Digits[4 - 1] >= _T('5') )
      {
        Digits[2 - 1]++;
        if ( Digits[2 - 1] > _T('9') )
        {
          Digits[2 - 1] = _T('1');
          Exp++;
        }
      }
      Digits.erase( 3 - 1, 2 );
      DecimalPoint = Digits.length( ) + 1;
    }
    else
    {
        /* Move decimal Point at the desired position */
      /* dme 
      Digits.erase( 3 - 1, 1 );
      DecimalPoint = 2 + Placehold[1 - 1] + Placehold[2 - 1];
      if ( Decimals != 0 ) 
        Digits.insert( DecimalPoint - 1, _T(".") );
        */
    }

      /* convert Optional zeroes to spaces. */
    i = Digits.length( );
    j = DecimalPoint + Placehold[3 - 1];
    while ( ( i > j ) && ( Digits[i - 1] == _T('0') ) )
    {
      Digits[i - 1] = _T(' ');
      i--;
    }

      /* if Integer number and no obligatory decimal paces, remove decimal Point */
    if ( ( DecimalPoint < (int) Digits.length( ) ) && ( Digits[DecimalPoint + 1 - 1] == _T(' ') ) )
      Digits[DecimalPoint - 1] = _T(' ');
    if ( Digits[1 - 1] == _T(' ') )
    {
      Digits.erase( 1 - 1, 1 );
      DecimalPoint--;
    }
      /* Calculate Exponent String */
    Str( Abs( Exp ), Exponent );
    while ( (int) Exponent.length( ) < ExpSize )
      Exponent.insert( 1 - 1, _T("0") );
    if ( Exp >= 0 ) 
    {
      if ( Sysutils__13.Contains(ExpFmt ) ) 
        Exponent.insert( 1 - 1, _T("+") );
    }
    else
      Exponent.insert( 1 - 1, _T("-") );
    if ( ExpFmt < 3 ) 
      Exponent.insert( 1 - 1, _T("E") );
    else
      Exponent.insert( 1 - 1, _T("e") );
  }
  DigitExponent = DecimalPoint - 2;
  if ( !Digits.empty() && Digits[1 - 1] == _T('-') )
    DigitExponent--;
  UnexpectedDigits = DecimalPoint - 1 - ( Placehold[1 - 1] + Placehold[2 - 1] );
}


int PutResult( Char*& Buffer, const TFormatSettings& FormatSettings, String& Digits, String& Exponent, Char*& FmtStart, Char*& FmtStop, int& ExpFmt, int& ExpSize, bool& thousand, int& UnexpectedDigits, int& DigitExponent )
{
  int result = 0;
  bool SQ = false, DQ = false;
  Char* fmt = NULL,* Buf = NULL;
  int dig = 0, n = 0;
  SQ = false;
  DQ = false;
  fmt = FmtStart;
  Buf = Buffer;
  dig = 1;
  while ( fmt < FmtStop ) 
  {
      //write(fmt[0]);
    switch ( fmt[0] )
    {
      case _T('\x22'):
      {
        if ( ! SQ ) 
          DQ = ! DQ;
        fmt++;
      }
      break;
      case _T('\x27'):
      {
        if ( ! DQ ) 
          SQ = ! SQ;
        fmt++;
      }
      break;
    default:
      if ( ! ( SQ || DQ ) ) 
      {
        switch ( fmt[0] )
        {
          case _T('0'): case _T('#'): case _T('.'):
          {
            if ( ( dig == 1 ) && ( UnexpectedDigits > 0 ) ) 
            {
                /* everything unexpected is written before the First digit */
              for ( n = 1; n <= UnexpectedDigits; n++)
              {
                Buf[0] = Digits[n - 1];
                Buf++;
                if ( thousand && ( Digits[n - 1] != _T('-') ) )
                {
                  if ( ( DigitExponent % 3 == 0 ) && ( DigitExponent > 0 ) ) 
                  {
                    Buf[0] = FormatSettings.ThousandSeparator;
                    Buf++;
                  }
                  DigitExponent--;
                }
              }
              dig += UnexpectedDigits;
            }
            if ( dig <= (int) Digits.size() && Digits[dig - 1] != _T(' ') )
            {
              if ( Digits[dig - 1] == _T('.') )
                Buf[0] = FormatSettings.DecimalSeparator;
              else
                Buf[0] = Digits[dig - 1];
              Buf++;
              if ( thousand && ( DigitExponent % 3 == 0 ) && ( DigitExponent > 0 ) ) 
              {
                Buf[0] = FormatSettings.ThousandSeparator;
                Buf++;
              }
            }
            dig++;
            DigitExponent--;
            fmt++;
          }
          break;
          case _T('e'): case _T('E'):
          {
            if ( ExpFmt != 0 ) 
            {
              fmt++;
              if ( fmt < FmtStop ) 
              {
                if ( Sysutils__14.Contains(fmt[0] ) ) 
                {
                  fmt += ExpSize;
                  for ( n = 1; n <= (int) Exponent.length( ); n++)
                    Buf[n - 1] = Exponent[n - 1];
                  Buf += Exponent.length( );
                  ExpFmt = 0;
                }
                fmt++;
              }
            }
            else
            {
                /* no Legal Exponential Format.
                  simply write the 'E' to the result. */
              Buf[0] = fmt[0];
              Buf++;
              fmt++;
            }
          }
          break;
        default: /* case */
            /* usual character */
          if ( fmt[0] != _T(',')) 
          {
            Buf[0] = fmt[0];
            Buf++;
          }
          fmt++;
        } /* case */
      }
      else /* if */
      {
          /* character inside Single or Double quotes */
        Buf[0] = fmt[0];
        Buf++;
        fmt++;
      }
    } /* case */
  } /* While .. begin */
  //result = ((PtrUInt) Buf ) - ((PtrUInt) Buffer );
  return Buf- Buffer;
}


int FloatToTextFmt( Char* Buffer, long double Value, Char* Format, const TFormatSettings& FormatSettings )
{
  int result = 0;
  String Digits;                         /* String of Digits                 */
  Digits.resize(40);
  String Exponent;                        /* Exponent strIn                   */
  Exponent.resize(8);
  Char* FmtStart = NULL,* FmtStop                   /* start and end of relevant part   */
                                              /* of Format String                 */ = NULL;
  int ExpFmt = 0, ExpSize                   /* type and Length of               */
                                              /* Exponential Format chosen        */ = 0;
  int Placehold[ 4/*# range 1..4*/ ];          /* number of placeholders in all    */
                                              /* four sections                    */
  bool thousand                          /* thousand separators?             */ = false;
  int UnexpectedDigits                  /* number of unexpected Digits that */
                                              /* have to be inserted before the   */
                                              /* First placeholder.               */ = 0;
  int DigitExponent                     /* Exponent of First digit in       */
                                              /* Digits array.                    */

  /* find end of Format section starting at P. False, if empty */ = 0;

  /* find start and end of Format section to apply. if section doesn'T exist,
    use section 1. if section 2 is used, the sign of Value is ignored.       */
  if ( Value > 0 ) 
    GetSectionRange( 1, Value, Format, FmtStart, FmtStop );
  else
    if ( Value < 0 ) 
      GetSectionRange( 2, Value, Format, FmtStart, FmtStop );
    else
      GetSectionRange( 3, Value, Format, FmtStart, FmtStop );
  if ( FmtStart == NULL ) 
  {
    result = FloatToText( Buffer, Value, ffGeneral, 15, 4, FormatSettings );
  }
  else
  {
    GetFormatOptions( FmtStart, FmtStop, ExpFmt, ExpSize, Placehold, thousand);
    if ( ( ExpFmt == 0 ) && ( Abs( Value ) >= 1E18) ) 
      result = FloatToText( Buffer, Value, ffGeneral, 15, 4, FormatSettings );
    else
    {
      FloatToStr( Value, Digits, Exponent, ExpFmt, ExpSize, Placehold, UnexpectedDigits, DigitExponent );
      result = PutResult(Buffer, FormatSettings, Digits, Exponent, FmtStart, FmtStop, ExpFmt, ExpSize, thousand, UnexpectedDigits, DigitExponent);
    }
  }
  return result;
}

int FloatToTextFmt( Char* Buffer, long double Value, Char* Format )
{
  return FloatToTextFmt( Buffer, Value, Format, DefaultFormatSettings );
}

void FloatToDecimal( TFloatRec& result, void* Value, TFloatValue ValueType, int Precision, int Decimals )
{
  String Buffer;
  int Error = 0, n = 0, l = 0, start = 0, C = 0;
  bool GotNonZeroBeforeDot = false, BeforeDot = false;
  switch ( ValueType )
  {
    case fvExtended:
      Str( *(long double*) Value, 25, Buffer );
    break;
    case fvDouble: case fvReal:
      Str(  *(double*) Value, 23, Buffer );
    break;
    case fvSingle:
      Str(  *(float*) Value, 16, Buffer );
    break;
    case fvCurrency:
      Str( *(Currency*) Value, 25, 2, Buffer );
    break; 
    case fvComp:
      Str( *(Currency*) Value, 23, 2, Buffer );
    break; 
  }
  n = 1;
  l = ((unsigned char) Buffer[0] );
  while ( Buffer[n] == ' ' ) 
    n++;
  result.Negative = ( Buffer[n] == '-' );
  if ( result.Negative ) 
    n++;
  start = n;  //start of Digits
  result.Exponent = 0;
  BeforeDot = true;
  GotNonZeroBeforeDot = false;
  while ( ( l >= n ) && ( Buffer[n] != 'E' ) ) 
  {
    if ( Buffer[n] == '.' ) 
      BeforeDot = false;
    else
    {
      if ( BeforeDot ) 
      {  // Currently this is always 1 char
        result.Exponent++;
        result.Digits[n - start] = Buffer[n];
        if ( Buffer[n] != '0' ) 
          GotNonZeroBeforeDot = true;
      }
      else
        result.Digits[n - start - 1] = Buffer[n];
    }
    n++;
  }
  n++; // Pass through 'e'
  if ( n <= l ) 
  {
    Val( String( Buffer ).substr( n, l - n + 1 ), &C, Error ); // get Exponent After 'e'
    result.Exponent += C;
  }
  // Calculate number of Digits we have from str
  if ( BeforeDot ) 
    n = n - start - 1;
  else
    n = n - start - 2;
  l = sizeof( result.Digits );  // todo
  if ( n < l ) 
    FillChar( &result.Digits[n], l - n, '0' );  //zero remaining space
  if ( Decimals + result.Exponent < Precision )  //After this it is the same as in FloatToDecimal
    n = Decimals + result.Exponent;
  else
    n = Precision;
  if ( n >= l ) 
    n = l - 1;
  if ( n == 0 ) 
  {
    if ( result.Digits[0] >= _T('5')) 
    {
      result.Digits[0] = _T('1');
      result.Digits[1] = _T('\x00');
      result.Exponent++;
    }
    else
      result.Digits[0] = _T('\x00');
  }  //n=0
  else
    if ( n > 0 ) 
    {
      if ( result.Digits[n] >= _T('5')) 
      {
        do
        {
          result.Digits[n] = _T('\x00');
          n--;
          result.Digits[n]++;
        }
        while ( ! ( ( n == 0 ) || ( result.Digits[n] < _T(':')) ) );
        if ( result.Digits[0] == _T(':')) 
        {
          result.Digits[0] = _T('1');
          result.Exponent++;
        }
      }
      else
      {
        result.Digits[n] = _T('0');
        while ( ( n > - 1 ) && ( result.Digits[n] == _T('0')) ) 
        {
          result.Digits[n] = _T('\x00');
          n--;
        }
      }
    } //n>0
    else
      result.Digits[0] = _T('\x00');
  if ( ( result.Digits[0] == _T('\x00')) && ! GotNonZeroBeforeDot ) 
  {
    result.Exponent = 0;
    result.Negative = false;
  }
}


void FloatToDecimal( TFloatRec& result, long double Value, int Precision, int Decimals )
{
  FloatToDecimal( result, &Value, fvExtended, Precision, Decimals );
}


String FormatFloat( const String& Format, long double Value, const TFormatSettings& FormatSettings )
{
  String result;
  Char Buf[ 1025/*# range 0..1024*/ ]; // not changed to PChar(Pointer(). possibly not safe
  Buf[FloatToTextFmt( Buf, Value, (Char*) Format.c_str(), FormatSettings )] = _T('\x00');
  result = Buf;
  return result;
}


String FormatFloat( const String& Format, long double Value )
{
  return FormatFloat( Format, Value, DefaultFormatSettings );
}

String FormatCurr( const String& Format, Currency Value, const TFormatSettings& FormatSettings )
{
  return FormatFloat( Format, (double) Value, FormatSettings );
} 


String FormatCurr( const String& Format, Currency Value )
{
  return FormatCurr( Format, Value, DefaultFormatSettings );
}

int LastDelimiter( const String& Delimiters, const String& S )
{
  int result = 0;
  TSysCharSet cHS;
  unsigned int i = 0;
  cHS = TSysCharSet () ;
  for ( i = 1; i <= Delimiters.length( ); i++)
    cHS << Delimiters[i - 1];
  result = S.length( );
  while ( ( result > 0 ) && ! ( cHS.Contains(S[result - 1] ) ) ) 
    result--;
  return result;
}


String StringReplace( const String& S, const String& OldPattern, const String& NewPattern, TReplaceFlags Flags )
{
  String result;
  String SrcH, OldP, RemS; // SrcH and OldP can contain UpperCase versions of S,OldPattern

  int P = 0;
  SrcH = S;
  OldP = OldPattern;
  if ( Flags.Contains(rfIgnoreCase ) ) 
  {
    SrcH = AnsiUpperCase( SrcH );
    OldP = AnsiUpperCase( OldP );
  }
  RemS = S;
  result = _T("");
  while ( SrcH.length( ) != 0 ) 
  {
    P = AnsiPos( OldP, SrcH );
    if ( P == 0 ) 
    {
      result = result + RemS;
      SrcH = _T("");
    }
    else
    {
      result = result + RemS.substr( 1 - 1, P - 1 ) + NewPattern;
      P = P + OldP.length( );
      RemS = RemS.substr( P - 1, RemS.length( ) - P + 1 );
      if ( ! ( Flags.Contains(rfReplaceAll ) ) ) 
      {
        result = result + RemS;
        SrcH = _T("");
      }
      else
        SrcH = SrcH.substr( P - 1, SrcH.length( ) - P + 1 );
    }
  }
  return result;
}


bool IsDelimiter( const String& Delimiters, const String& S, int Index )
{
  bool result = false;
  result = false;
  if ( ( Index > 0 ) && ( (unsigned int) Index <= S.length( ) ) ) 
    result = Pos( S[Index - 1], Delimiters ) != 0; // Note we don'T do MBCS yet
  return result;
}


int ByteToCharLen( const String& S, int MaxLen )
{
  int result = 0;
  result = S.length( );
  if ( result > MaxLen ) 
    result = MaxLen;
  return result;
}


int ByteToCharIndex( const String& S, int Index )
{
  int result = 0;
  result = Index;
  return result;
}


int CharToByteLen( const String& S, int MaxLen )
{
  int result = 0;
  result = S.length( );
  if ( result > MaxLen ) 
    result = MaxLen;
  return result;
}


int CharToByteIndex( const String& S, int Index )
{
  int result = 0;
  result = Index;
  return result;
}


TMbcsByteType ByteType( const String& S, int Index )
{
  TMbcsByteType result;
  result = mbSingleByte;
  return result;
}


TMbcsByteType StrByteType( Char* str, unsignedint Index )
{
  TMbcsByteType result;
  result = mbSingleByte;
  return result;
}

PtrInt CharLengthPChar( const char* str )
{
  PtrInt result;
  PtrInt nextlen;
  const char* S = NULL;
  mbstate_t mbstate;
  result = 0;
  S = str;
  do
  {
    nextlen = PtrInt( mbrlen( str, MB_CUR_MAX, &mbstate ) );
    if ( nextlen < 0 )
      nextlen = 1;
    result += nextlen;
    S += nextlen;
  }
  while ( ! ( nextlen == 0 ) );
  return result;
}


int StrCharLength( const char* str )
{
  int result = 0;
  //result = widestringmanager.CharLengthPCharProc( str );
  result = CharLengthPChar( str );
  return result;
}

const char* StrNextChar( const char* str )
{
  const char* result = NULL;
  result = str + StrCharLength( str );
  return result;
}


bool FindCmdLineSwitch( const String& Switch, const TSysCharSet Chars, bool IgnoreCase )
{
  bool result = false;
  int i = 0, l = 0;
  String S, T;
  result = false;
  S = Switch;
  if ( IgnoreCase ) 
    S = UpperCase( S );
  i = ParamCount();
  while ( ( ! result ) && ( i > 0 ) ) 
  {
    l = ParamStr( i ).length( );
    if ( ( l > 0 ) && ( Chars.Contains(ParamStr( i )[1] ) ) ) 
    {
      T = ParamStr( i ).substr( 2 - 1, l - 1 );
      if ( IgnoreCase ) 
        T = UpperCase( T );
      result = S == T;
    }
    i--;
  }
  return result;
}


bool FindCmdLineSwitch( const String& Switch, bool IgnoreCase )
{
  bool result = false;
  result = FindCmdLineSwitch( Switch, SwitchChars, IgnoreCase );
  return result;
}


bool FindCmdLineSwitch( const String& Switch )
{
  bool result = false;
  result = FindCmdLineSwitch( Switch, SwitchChars, false );
  return result;
}


String WrapText( const String& Line, const String& BreakStr, const TSysCharSet BreakChars, int MaxCol )
{
  String result;
  const TSet < UChar, 0, 255> quotes = ( TSet < UChar, 0, 255 >() << _T('\'') << _T('"') );
  String l;
  Char C = _T('\0'), LQ = _T('\0'), BC = _T('\0');
  int P = 0, BLen = 0, Len = 0;
  bool HB = false, IBC = false;
  result = _T("");
  l = Line;
  BLen = BreakStr.length( );
  if ( BLen > 0 ) 
    BC = BreakStr[1 - 1];
  else
    BC = _T('\x00');
  Len = l.length( );
  while ( Len > 0 ) 
  {
    P = 1;
    LQ = _T('\x00');
    HB = false;
    IBC = false;
    while ( ( ( P <= Len ) && ( ( P <= MaxCol ) || ! IBC ) ) && ( ( LQ != _T('\x00')) || ! HB ) ) 
    {
      C = l[P - 1];
      if ( C == LQ ) 
        LQ = _T('\x00');
      else
        if ( quotes.Contains(C ) ) 
          LQ = C;
      if ( LQ != _T('\x00')) 
        P++;
      else
      {
        HB = ( ( C == BC ) && ( BreakStr == l.substr( P - 1, BLen ) ) );
        if ( HB ) 
          P += BLen;
        else
        {
          if ( P > MaxCol ) 
            IBC = BreakChars.Contains(C );
          P++;
        }
      }
//      WriteLn('"',C,'" : IBC : ',IBC,' HB  : ',HB,' LQ  : ',LQ,' P>MaxCol : ',P>MaxCol);
    }
    result = result + l.substr( 1 - 1, P - 1 );
    if ( ! HB ) 
      result = result + BreakStr;
    l.erase( 1 - 1, P - 1 );
    Len = l.length( );
  }
  return result;
}

#ifdef windows
const Char sLineBreak[] = _T("\x0d\x0a");
#elif defined (linux)
const Char sLineBreak[] = _T("\x0a");
#else
#error unknown platform
#endif

String WrapText( const String& Line, int MaxCol )
{
  return WrapText( Line, sLineBreak, Sysutils__15, MaxCol );
}

#ifdef linux

/*
   case translation tables
   can be used in internationalization support.

   Although These tables can be obtained through System calls
CD    it is better to not use those, since most implementation are not 100%
   Warning:
   before modifying A translation Table make sure that the current CodePage
   of the OS corresponds to the one you make changes to
*/
   /* upper case translation Table for character set 850 */


Char CP850UCT [ 128/*# range 128..255*/ ];
void CP850UCTInit( )
{
  CP850UCT[0] = _T('\x80');
  CP850UCT[1] = _T('\x9a');
  CP850UCT[2] = _T('\x90');
  CP850UCT[3] = _T('\xb6');
  CP850UCT[4] = _T('\x8e');
  CP850UCT[5] = _T('\xb6');
  CP850UCT[6] = _T('\x8f');
  CP850UCT[7] = _T('\x80');
  CP850UCT[8] = _T('\xd2');
  CP850UCT[9] = _T('\xd3');
  CP850UCT[10] = _T('\xd4');
  CP850UCT[11] = _T('\xd8');
  CP850UCT[12] = _T('\xd7');
  CP850UCT[13] = _T('\xde');
  CP850UCT[14] = _T('\x8e');
  CP850UCT[15] = _T('\x8f');
  CP850UCT[16] = _T('\x90');
  CP850UCT[17] = _T('\x92');
  CP850UCT[18] = _T('\x92');
  CP850UCT[19] = _T('\xe2');
  CP850UCT[20] = _T('\x99');
  CP850UCT[21] = _T('\xe3');
  CP850UCT[22] = _T('\xea');
  CP850UCT[23] = _T('\xeb');
  CP850UCT[24] = _T('Y');
  CP850UCT[25] = _T('\x99');
  CP850UCT[26] = _T('\x9a');
  CP850UCT[27] = _T('\x9d');
  CP850UCT[28] = _T('\x9c');
  CP850UCT[29] = _T('\x9d');
  CP850UCT[30] = _T('\x9e');
  CP850UCT[31] = _T('\x9f');
  CP850UCT[32] = _T('\xb5');
  CP850UCT[33] = _T('\xd6');
  CP850UCT[34] = _T('\xe0');
  CP850UCT[35] = _T('\xe9');
  CP850UCT[36] = _T('\xa5');
  CP850UCT[37] = _T('\xa5');
  CP850UCT[38] = _T('\xa6');
  CP850UCT[39] = _T('\xa7');
  CP850UCT[40] = _T('\xa8');
  CP850UCT[41] = _T('\xa9');
  CP850UCT[42] = _T('\xaa');
  CP850UCT[43] = _T('\xab');
  CP850UCT[44] = _T('\xac');
  CP850UCT[45] = _T('\xad');
  CP850UCT[46] = _T('\xae');
  CP850UCT[47] = _T('\xaf');
  CP850UCT[48] = _T('\xb0');
  CP850UCT[49] = _T('\xb1');
  CP850UCT[50] = _T('\xb2');
  CP850UCT[51] = _T('\xb3');
  CP850UCT[52] = _T('\xb4');
  CP850UCT[53] = _T('\xb5');
  CP850UCT[54] = _T('\xb6');
  CP850UCT[55] = _T('\xb7');
  CP850UCT[56] = _T('\xb8');
  CP850UCT[57] = _T('\xb9');
  CP850UCT[58] = _T('\xba');
  CP850UCT[59] = _T('\xbb');
  CP850UCT[60] = _T('\xbc');
  CP850UCT[61] = _T('\xbd');
  CP850UCT[62] = _T('\xbe');
  CP850UCT[63] = _T('\xbf');
  CP850UCT[64] = _T('\xc0');
  CP850UCT[65] = _T('\xc1');
  CP850UCT[66] = _T('\xc2');
  CP850UCT[67] = _T('\xc3');
  CP850UCT[68] = _T('\xc4');
  CP850UCT[69] = _T('\xc5');
  CP850UCT[70] = _T('\xc7');
  CP850UCT[71] = _T('\xc7');
  CP850UCT[72] = _T('\xc8');
  CP850UCT[73] = _T('\xc9');
  CP850UCT[74] = _T('\xca');
  CP850UCT[75] = _T('\xcb');
  CP850UCT[76] = _T('\xcc');
  CP850UCT[77] = _T('\xcd');
  CP850UCT[78] = _T('\xce');
  CP850UCT[79] = _T('\xcf');
  CP850UCT[80] = _T('\xd0');
  CP850UCT[81] = _T('\xd1');
  CP850UCT[82] = _T('\xd2');
  CP850UCT[83] = _T('\xd3');
  CP850UCT[84] = _T('\xd4');
  CP850UCT[85] = _T('\xd5');
  CP850UCT[86] = _T('\xd6');
  CP850UCT[87] = _T('\xd7');
  CP850UCT[88] = _T('\xd8');
  CP850UCT[89] = _T('\xd9');
  CP850UCT[90] = _T('\xda');
  CP850UCT[91] = _T('\xdb');
  CP850UCT[92] = _T('\xdc');
  CP850UCT[93] = _T('\xdd');
  CP850UCT[94] = _T('\xde');
  CP850UCT[95] = _T('\xdf');
  CP850UCT[96] = _T('\xe0');
  CP850UCT[97] = _T('\xe1');
  CP850UCT[98] = _T('\xe2');
  CP850UCT[99] = _T('\xe3');
  CP850UCT[100] = _T('\xe5');
  CP850UCT[101] = _T('\xe5');
  CP850UCT[102] = _T('\xe6');
  CP850UCT[103] = _T('\xed');
  CP850UCT[104] = _T('\xe8');
  CP850UCT[105] = _T('\xe9');
  CP850UCT[106] = _T('\xea');
  CP850UCT[107] = _T('\xeb');
  CP850UCT[108] = _T('\xed');
  CP850UCT[109] = _T('\xed');
  CP850UCT[110] = _T('\xee');
  CP850UCT[111] = _T('\xef');
  CP850UCT[112] = _T('\xf0');
  CP850UCT[113] = _T('\xf1');
  CP850UCT[114] = _T('\xf2');
  CP850UCT[115] = _T('\xf3');
  CP850UCT[116] = _T('\xf4');
  CP850UCT[117] = _T('\xf5');
  CP850UCT[118] = _T('\xf6');
  CP850UCT[119] = _T('\xf7');
  CP850UCT[120] = _T('\xf8');
  CP850UCT[121] = _T('\xf9');
  CP850UCT[122] = _T('\xfa');
  CP850UCT[123] = _T('\xfb');
  CP850UCT[124] = _T('\xfc');
  CP850UCT[125] = _T('\xfd');
  CP850UCT[126] = _T('\xfe');
  CP850UCT[127] = _T('\xff');
}

   /* lower case translation Table for character set 850 */
Char CP850LCT [ 128/*# range 128..255*/ ];
void CP850LCTInit( )
{
  CP850LCT[0] = _T('\x87');
  CP850LCT[1] = _T('\x81');
  CP850LCT[2] = _T('\x82');
  CP850LCT[3] = _T('\x83');
  CP850LCT[4] = _T('\x84');
  CP850LCT[5] = _T('\x85');
  CP850LCT[6] = _T('\x86');
  CP850LCT[7] = _T('\x87');
  CP850LCT[8] = _T('\x88');
  CP850LCT[9] = _T('\x89');
  CP850LCT[10] = _T('\x8a');
  CP850LCT[11] = _T('\x8b');
  CP850LCT[12] = _T('\x8c');
  CP850LCT[13] = _T('\x8d');
  CP850LCT[14] = _T('\x84');
  CP850LCT[15] = _T('\x86');
  CP850LCT[16] = _T('\x82');
  CP850LCT[17] = _T('\x91');
  CP850LCT[18] = _T('\x91');
  CP850LCT[19] = _T('\x93');
  CP850LCT[20] = _T('\x94');
  CP850LCT[21] = _T('\x95');
  CP850LCT[22] = _T('\x96');
  CP850LCT[23] = _T('\x97');
  CP850LCT[24] = _T('\x98');
  CP850LCT[25] = _T('\x94');
  CP850LCT[26] = _T('\x81');
  CP850LCT[27] = _T('\x9b');
  CP850LCT[28] = _T('\x9c');
  CP850LCT[29] = _T('\x9b');
  CP850LCT[30] = _T('\x9e');
  CP850LCT[31] = _T('\x9f');
  CP850LCT[32] = _T('\xa0');
  CP850LCT[33] = _T('\xa1');
  CP850LCT[34] = _T('\xa2');
  CP850LCT[35] = _T('\xa3');
  CP850LCT[36] = _T('\xa4');
  CP850LCT[37] = _T('\xa4');
  CP850LCT[38] = _T('\xa6');
  CP850LCT[39] = _T('\xa7');
  CP850LCT[40] = _T('\xa8');
  CP850LCT[41] = _T('\xa9');
  CP850LCT[42] = _T('\xaa');
  CP850LCT[43] = _T('\xab');
  CP850LCT[44] = _T('\xac');
  CP850LCT[45] = _T('\xad');
  CP850LCT[46] = _T('\xae');
  CP850LCT[47] = _T('\xaf');
  CP850LCT[48] = _T('\xb0');
  CP850LCT[49] = _T('\xb1');
  CP850LCT[50] = _T('\xb2');
  CP850LCT[51] = _T('\xb3');
  CP850LCT[52] = _T('\xb4');
  CP850LCT[53] = _T('\xa0');
  CP850LCT[54] = _T('\x83');
  CP850LCT[55] = _T('\x85');
  CP850LCT[56] = _T('\xb8');
  CP850LCT[57] = _T('\xb9');
  CP850LCT[58] = _T('\xba');
  CP850LCT[59] = _T('\xbb');
  CP850LCT[60] = _T('\xbc');
  CP850LCT[61] = _T('\xbd');
  CP850LCT[62] = _T('\xbe');
  CP850LCT[63] = _T('\xbf');
  CP850LCT[64] = _T('\xc0');
  CP850LCT[65] = _T('\xc1');
  CP850LCT[66] = _T('\xc2');
  CP850LCT[67] = _T('\xc3');
  CP850LCT[68] = _T('\xc4');
  CP850LCT[69] = _T('\xc5');
  CP850LCT[70] = _T('\xc6');
  CP850LCT[71] = _T('\xc6');
  CP850LCT[72] = _T('\xc8');
  CP850LCT[73] = _T('\xc9');
  CP850LCT[74] = _T('\xca');
  CP850LCT[75] = _T('\xcb');
  CP850LCT[76] = _T('\xcc');
  CP850LCT[77] = _T('\xcd');
  CP850LCT[78] = _T('\xce');
  CP850LCT[79] = _T('\xcf');
  CP850LCT[80] = _T('\xd0');
  CP850LCT[81] = _T('\xd1');
  CP850LCT[82] = _T('\x88');
  CP850LCT[83] = _T('\x89');
  CP850LCT[84] = _T('\x8a');
  CP850LCT[85] = _T('\xd5');
  CP850LCT[86] = _T('\xa1');
  CP850LCT[87] = _T('\x8c');
  CP850LCT[88] = _T('\x8b');
  CP850LCT[89] = _T('\xd9');
  CP850LCT[90] = _T('\xda');
  CP850LCT[91] = _T('\xdb');
  CP850LCT[92] = _T('\xdc');
  CP850LCT[93] = _T('\xdd');
  CP850LCT[94] = _T('\x8d');
  CP850LCT[95] = _T('\xdf');
  CP850LCT[96] = _T('\xa2');
  CP850LCT[97] = _T('\xe1');
  CP850LCT[98] = _T('\x93');
  CP850LCT[99] = _T('\x95');
  CP850LCT[100] = _T('\xe4');
  CP850LCT[101] = _T('\xe4');
  CP850LCT[102] = _T('\xe6');
  CP850LCT[103] = _T('\xed');
  CP850LCT[104] = _T('\xe8');
  CP850LCT[105] = _T('\xa3');
  CP850LCT[106] = _T('\x96');
  CP850LCT[107] = _T('\x97');
  CP850LCT[108] = _T('\xec');
  CP850LCT[109] = _T('\xec');
  CP850LCT[110] = _T('\xee');
  CP850LCT[111] = _T('\xef');
  CP850LCT[112] = _T('\xf0');
  CP850LCT[113] = _T('\xf1');
  CP850LCT[114] = _T('\xf2');
  CP850LCT[115] = _T('\xf3');
  CP850LCT[116] = _T('\xf4');
  CP850LCT[117] = _T('\xf5');
  CP850LCT[118] = _T('\xf6');
  CP850LCT[119] = _T('\xf7');
  CP850LCT[120] = _T('\xf8');
  CP850LCT[121] = _T('\xf9');
  CP850LCT[122] = _T('\xfa');
  CP850LCT[123] = _T('\xfb');
  CP850LCT[124] = _T('\xfc');
  CP850LCT[125] = _T('\xfd');
  CP850LCT[126] = _T('\xfe');
  CP850LCT[127] = _T('\xff');
}

   /* upper case translation Table for character set ISO 8859/1  Latin 1  */
Char CPISO88591UCT [ 64/*# range 192..255*/ ];
void CPISO88591UCTInit( )
{
  CPISO88591UCT[0] = _T('\xc0');
  CPISO88591UCT[1] = _T('\xc1');
  CPISO88591UCT[2] = _T('\xc2');
  CPISO88591UCT[3] = _T('\xc3');
  CPISO88591UCT[4] = _T('\xc4');
  CPISO88591UCT[5] = _T('\xc5');
  CPISO88591UCT[6] = _T('\xc6');
  CPISO88591UCT[7] = _T('\xc7');
  CPISO88591UCT[8] = _T('\xc8');
  CPISO88591UCT[9] = _T('\xc9');
  CPISO88591UCT[10] = _T('\xca');
  CPISO88591UCT[11] = _T('\xcb');
  CPISO88591UCT[12] = _T('\xcc');
  CPISO88591UCT[13] = _T('\xcd');
  CPISO88591UCT[14] = _T('\xce');
  CPISO88591UCT[15] = _T('\xcf');
  CPISO88591UCT[16] = _T('\xd0');
  CPISO88591UCT[17] = _T('\xd1');
  CPISO88591UCT[18] = _T('\xd2');
  CPISO88591UCT[19] = _T('\xd3');
  CPISO88591UCT[20] = _T('\xd4');
  CPISO88591UCT[21] = _T('\xd5');
  CPISO88591UCT[22] = _T('\xd6');
  CPISO88591UCT[23] = _T('\xd7');
  CPISO88591UCT[24] = _T('\xd8');
  CPISO88591UCT[25] = _T('\xd9');
  CPISO88591UCT[26] = _T('\xda');
  CPISO88591UCT[27] = _T('\xdb');
  CPISO88591UCT[28] = _T('\xdc');
  CPISO88591UCT[29] = _T('\xdd');
  CPISO88591UCT[30] = _T('\xde');
  CPISO88591UCT[31] = _T('\xdf');
  CPISO88591UCT[32] = _T('\xc0');
  CPISO88591UCT[33] = _T('\xc1');
  CPISO88591UCT[34] = _T('\xc2');
  CPISO88591UCT[35] = _T('\xc3');
  CPISO88591UCT[36] = _T('\xc4');
  CPISO88591UCT[37] = _T('\xc5');
  CPISO88591UCT[38] = _T('\xc6');
  CPISO88591UCT[39] = _T('\xc7');
  CPISO88591UCT[40] = _T('\xc8');
  CPISO88591UCT[41] = _T('\xc9');
  CPISO88591UCT[42] = _T('\xca');
  CPISO88591UCT[43] = _T('\xcb');
  CPISO88591UCT[44] = _T('\xcc');
  CPISO88591UCT[45] = _T('\xcd');
  CPISO88591UCT[46] = _T('\xce');
  CPISO88591UCT[47] = _T('\xcf');
  CPISO88591UCT[48] = _T('\xd0');
  CPISO88591UCT[49] = _T('\xd1');
  CPISO88591UCT[50] = _T('\xd2');
  CPISO88591UCT[51] = _T('\xd3');
  CPISO88591UCT[52] = _T('\xd4');
  CPISO88591UCT[53] = _T('\xd5');
  CPISO88591UCT[54] = _T('\xd6');
  CPISO88591UCT[55] = _T('\xf7');
  CPISO88591UCT[56] = _T('\xd8');
  CPISO88591UCT[57] = _T('\xd9');
  CPISO88591UCT[58] = _T('\xda');
  CPISO88591UCT[59] = _T('\xdb');
  CPISO88591UCT[60] = _T('\xdc');
  CPISO88591UCT[61] = _T('\xdd');
  CPISO88591UCT[62] = _T('\xde');
  CPISO88591UCT[63] = _T('\x59');
}

   /* lower case translation Table for character set ISO 8859/1  Latin 1  */
Char CPISO88591LCT [ 64/*# range 192..255*/ ];
void CPISO88591LCTInit( )
{
  CPISO88591LCT[0] = _T('\xe0');
  CPISO88591LCT[1] = _T('\xe1');
  CPISO88591LCT[2] = _T('\xe2');
  CPISO88591LCT[3] = _T('\xe3');
  CPISO88591LCT[4] = _T('\xe4');
  CPISO88591LCT[5] = _T('\xe5');
  CPISO88591LCT[6] = _T('\xe6');
  CPISO88591LCT[7] = _T('\xe7');
  CPISO88591LCT[8] = _T('\xe8');
  CPISO88591LCT[9] = _T('\xe9');
  CPISO88591LCT[10] = _T('\xea');
  CPISO88591LCT[11] = _T('\xeb');
  CPISO88591LCT[12] = _T('\xec');
  CPISO88591LCT[13] = _T('\xed');
  CPISO88591LCT[14] = _T('\xee');
  CPISO88591LCT[15] = _T('\xef');
  CPISO88591LCT[16] = _T('\xf0');
  CPISO88591LCT[17] = _T('\xf1');
  CPISO88591LCT[18] = _T('\xf2');
  CPISO88591LCT[19] = _T('\xf3');
  CPISO88591LCT[20] = _T('\xf4');
  CPISO88591LCT[21] = _T('\xf5');
  CPISO88591LCT[22] = _T('\xf6');
  CPISO88591LCT[23] = _T('\xd7');
  CPISO88591LCT[24] = _T('\xf8');
  CPISO88591LCT[25] = _T('\xf9');
  CPISO88591LCT[26] = _T('\xfa');
  CPISO88591LCT[27] = _T('\xfb');
  CPISO88591LCT[28] = _T('\xfc');
  CPISO88591LCT[29] = _T('\xfd');
  CPISO88591LCT[30] = _T('\xfe');
  CPISO88591LCT[31] = _T('\xdf');
  CPISO88591LCT[32] = _T('\xe0');
  CPISO88591LCT[33] = _T('\xe1');
  CPISO88591LCT[34] = _T('\xe2');
  CPISO88591LCT[35] = _T('\xe3');
  CPISO88591LCT[36] = _T('\xe4');
  CPISO88591LCT[37] = _T('\xe5');
  CPISO88591LCT[38] = _T('\xe6');
  CPISO88591LCT[39] = _T('\xe7');
  CPISO88591LCT[40] = _T('\xe8');
  CPISO88591LCT[41] = _T('\xe9');
  CPISO88591LCT[42] = _T('\xea');
  CPISO88591LCT[43] = _T('\xeb');
  CPISO88591LCT[44] = _T('\xec');
  CPISO88591LCT[45] = _T('\xed');
  CPISO88591LCT[46] = _T('\xee');
  CPISO88591LCT[47] = _T('\xef');
  CPISO88591LCT[48] = _T('\xf0');
  CPISO88591LCT[49] = _T('\xf1');
  CPISO88591LCT[50] = _T('\xf2');
  CPISO88591LCT[51] = _T('\xf3');
  CPISO88591LCT[52] = _T('\xf4');
  CPISO88591LCT[53] = _T('\xf5');
  CPISO88591LCT[54] = _T('\xf6');
  CPISO88591LCT[55] = _T('\xf7');
  CPISO88591LCT[56] = _T('\xf8');
  CPISO88591LCT[57] = _T('\xf9');
  CPISO88591LCT[58] = _T('\xfa');
  CPISO88591LCT[59] = _T('\xfb');
  CPISO88591LCT[60] = _T('\xfc');
  CPISO88591LCT[61] = _T('\xfd');
  CPISO88591LCT[62] = _T('\xfe');
  CPISO88591LCT[63] = _T('\xff');
}

#endif


int GetInt( bool usigned, const String& S, const String& fmt, const void** pointers, int& pointers_maxidx, int& xResult, int& i, int& j, unsigned int& n, unsigned int& m, String& S1 )
{
  int result = 0;
  S1 = _T("");
  while ( ( S.length( ) > n ) && ( S[n - 1] == _T(' ')) )
    n++;
      /* read sign */
  if ( ( S.length( ) >= n ) && ( Sysutils__16.Contains(S[n - 1] ) ) ) 
  {
          /* don'T accept - when Reading unsigned */
    if ( usigned && ( S[n - 1] == _T('-')) ) 
    {
      result = S1.length( );
      return result;
    }
    else
    {
      S1 = S1 + S[n - 1];
      n++;
    }
  }
      /* read numbers */
  while ( ( S.length( ) >= n ) && ( Sysutils__17.Contains(S[n - 1] ) ) ) 
  {
    S1 = S1 + S[n - 1];
    n++;
  }
  result = S1.length( );
  return result;
}


int GetFloat( const String& S, const String& fmt, const void** pointers, int& pointers_maxidx, int& xResult, int& i, int& j, unsigned int& n, unsigned int& m, String& S1 )
{
  int result = 0;
  S1 = _T("");
  while ( ( S.length( ) > n ) && ( S[n - 1] == _T(' ')) )
    n++;
  while ( ( S.length( ) >= n ) && ( Sysutils__18.Contains(S[n - 1] ) ) ) 
  {
    S1 = S1 + S[n - 1];
    n++;
  }
  result = S1.length( );
  return result;
}


int GetString( const String& S, const String& fmt, const void** pointers, int& pointers_maxidx, int& xResult, int& i, int& j, unsigned int& n, unsigned int& m, String& S1 )
{
  int result = 0;
  S1 = _T("");
  while ( ( S.length( ) > n ) && ( S[n - 1] == _T(' ')) )
    n++;
  while ( ( S.length( ) >= n ) && ( S[n - 1] != _T(' ')) )
  {
    S1 = S1 + S[n - 1];
    n++;
  }
  result = S1.length( );
  return result;
}


bool ScanStr( Char C, const String& S, const String& fmt, const void** pointers, int& pointers_maxidx, int& xResult, int& i, int& j, unsigned int& n, unsigned int& m, String& S1 )
{
  bool result = false;
  while ( ( S.length( ) > n ) && ( S[n - 1] != C ) ) 
    n++;
  n++;
  if ( n <= S.length( ) ) 
    result = true;
  else
    result = false;
  return result;
}


int GetFmt( const String& S, const String& fmt, const void** pointers, int& pointers_maxidx, int& xResult, int& i, int& j, unsigned int& n, unsigned int& m, String& S1 )
{
  int result = 0;
  result = - 1;
  while ( true ) 
  {
    while ( ( fmt.length( ) > m ) && ( fmt[m - 1] == _T(' ')) )
      m++;
    if ( m >= fmt.length( ) ) 
      break;
    if ( fmt[m - 1] == _T('%'))
    {
      m++;
      switch ( fmt[m - 1] )
      {
        case _T('d'):
          result = vtInteger;
        break;
        case _T('f'):
          result = vtExtended;
        break;
        case _T('s'):
          result = vtString;
        break;
        case _T('c'):
          result = vtChar;
        break;
      default:
        throw EFormatError( ErrorString(_T(SysConst_SInvalidFormat), fmt ) );
      }
      m++;
      break;
    }
    if ( ! ( ScanStr( fmt[m - 1], S, fmt, pointers, pointers_maxidx, xResult, i, j, n, m, S1 ) ) ) 
      break;
    m++;
  }
  return result;
}


int SScanf( const String& S, const String& fmt, const void** pointers, int pointers_maxidx )
{
  int result = 0;
  int i = 0, j = 0;
  unsigned int n = 0, m = 0;
  String S1;
  n = 1;
  m = 1;
  result = 0;
  for ( i = 0; i <= pointers_maxidx /*# High(pointers) */; i++)
  {
    j = GetFmt(S, fmt, pointers, pointers_maxidx, result, i, j, n, m, S1);
    switch ( j )
    {
      case vtInteger:
      {
        if ( GetInt( false, S, fmt, pointers, pointers_maxidx, result, i, j, n, m, S1) > 0 ) 
        {
          *((PLongint) pointers[i] ) = StrToInt( S1 );
          result++;
        }
        else
          break;
      }
      break;
      case vtChar:
      {
        if ( S.length( ) > n ) 
        {
          *(Char*) ( pointers[i] ) = S[n - 1];
          n++;
          result++;
        }
        else
          break;
      }
      break;
      case vtExtended:
      {
        if ( GetFloat(S, fmt, pointers, pointers_maxidx, result, i, j, n, m, S1) > 0 ) 
        {
          *((PExtended) pointers[i] ) = StrToFloat( S1 );
          result++;
        }
        else
          break;
      }
      break;
      case vtString:
      {
        if ( GetString(S, fmt, pointers, pointers_maxidx, result, i, j, n, m, S1) > 0 ) 
        {
          //*((PAnsiString) pointers[i] ) = S1;  // todo ?
          *((PString) pointers[i] ) = S1;  // todo ?
          result++;
        }
        else
          break;
      }
      break;
    default:
      break;
    }
  }
  return result;
}

  /* read PChar handling functions implementation */
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

    System Utilities for Free pascal
*/

/*  PChar functions  */



typedef Char CharArray [ 1/*# range 0..0*/ ];

/* processor dependent part, shared withs Strings unit */  
/*
    this File is part of the Free pascal Run time Library.
    Copyright (C) 1999-2000 by the Free pascal development team

    processor dependent part of Strings.pp, that can be shared with
    Sysutils unit.

    see the File copying.FPC, included in this distribution,
    for details about the Copyright.

    this program is distributed in the hope that it will be useful,
    but without Any WARRANTY; without even the implied WARRANTY of
    MERCHANTABILITY or FITNESS for A particular purpose.

 ***********************************************************************/

/*$asmmode ATT*/                                                    

/* read generic String functions that are not implemented for the processor */
/*
    this File is part of the Free pascal Run time Library.
    Copyright (C) 1999-2000 by Carl-Eric Codere,
    member of the Free pascal development team.

    see the File copying.FPC, included in this distribution,
    for details about the Copyright.

    this program is distributed in the hope that it will be useful,
    but without Any WARRANTY; without even the implied WARRANTY of
    MERCHANTABILITY or FITNESS for A particular purpose.

 ***********************************************************************/


/* processor Independent part, shared with Strings unit */
/*
    this File is part of the Free pascal Run time Library.
    Copyright (C) 1999-2000 by the Free pascal development team

    processor Independent part for Strings and Sysutils units

    see the File copying.FPC, included in this distribution,
    for details about the Copyright.

    this program is distributed in the hope that it will be useful,
    but without Any WARRANTY; without even the implied WARRANTY of
    MERCHANTABILITY or FITNESS for A particular purpose.

 ***********************************************************************/


unsigned int StrLen( char* P )
{
  return strlen(P);
}

unsigned int StrLen( wchar_t* P )
{
  return wcslen(P);
}

char* StrCopy( char* Dest, const char* Source )
{
   return strcpy(Dest, Source);
}

wchar_t* StrCopy( wchar_t* Dest, const wchar_t* Source )
{
   return wcscpy(Dest, Source);
}

wchar_t* StrLCopy( wchar_t* Dest, const wchar_t* Source, int MaxLen )
{
   return wcsncpy(Dest, Source, MaxLen);
}

char* StrLCopy( char* Dest, const char* Source, int MaxLen )
{
   return strncpy(Dest, Source, MaxLen);
}

wchar_t* StrECopy( wchar_t* Dest, const wchar_t* Source )
{
   return StrEnd(wcscpy(Dest, Source));
}

char* StrECopy( char* Dest, const char* Source )
{
   return StrEnd(strcpy(Dest, Source));
}

char* StrEnd( char* P )
{
   return strchr(P, 0);
}

wchar_t* StrEnd( wchar_t* P )
{
   return wcschr(P, 0);
}

wchar_t* StrCat( wchar_t* Dest, const wchar_t* Source )
{
   return wcscat(Dest, Source);
}

char* StrCat( char* Dest, const char* Source )
{
   return strcat(Dest, Source);
}

char* StrLCat( char* Dest, const char* Source, int l )
{
  return strncat(Dest, Source, l);
}

wchar_t* StrLCat( wchar_t* Dest, const wchar_t* Source, int l )
{
  return wcsncat(Dest, Source, l);
}

int StrComp( const char* Str1, const char* Str2 )
{
  return strcmp(Str1, Str2);
}       

int StrComp( const wchar_t* Str1, const wchar_t* Str2 )
{
  return wcscmp(Str1, Str2);
} 

int StrLComp( const char* Str1, const char* Str2, int l )
{
  return strncmp(Str1, Str2, l);
}       

int StrLComp( const wchar_t* Str1, const wchar_t* Str2, int l )
{
  return wcsncmp(Str1, Str2, l);
}       

int StrIComp( const char* Str1, const char* Str2 )
{
#ifdef windows
  return _strcmpi(Str1, Str2);
#elif defined(linux)
   return strcasecmp(Str1, Str2);
#else
#error unknown platform
#endif
}       

int StrIComp( const wchar_t* Str1, const wchar_t* Str2 )
{
#ifdef windows
  return _wcsicmp(Str1, Str2);
#elif defined(linux)
  return wcscasecmp(Str1, Str2);
#else
#error unknown platform
#endif  
} 

int StrLIComp( const char* Str1, const char* Str2, int l )
{
#ifdef windows
  return _strnicmp(Str1, Str2, l);
#elif defined(linux)
  return strncasecmp(Str1, Str2, l);
#else
#error unknown platform
#endif    
}       

int StrLIComp( const wchar_t* Str1, const wchar_t* Str2, int l )
{
#ifdef windows
  return _wcsnicmp(Str1, Str2, l);
#elif defined(linux)
  return wcsncasecmp(Str1, Str2, l);
#else
#error unknown platform
#endif    
}        

char* StrMove( char* Dest, const char* Source )
{
  return strcpy(Dest, Source);
}

wchar_t* StrMove( wchar_t* Dest, const wchar_t* Source )
{
  return wcscpy(Dest, Source);
}

char* StrMove( char* Dest, const char* Source, int MaxLen )
{
  return strncpy(Dest, Source, MaxLen);
}

wchar_t* StrMove( wchar_t* Dest, const wchar_t* Source, int MaxLen )
{
  return wcsncpy(Dest, Source, MaxLen);
}

char* StrScan( char* P, char C )
{
  return strchr(P,C);
}

wchar_t* StrScan( wchar_t* P, wchar_t C )
{
  return wcschr(P,C);
}

char* StrRScan( char* P, char C )
{
  return strrchr(P,C);
}

wchar_t* StrRScan( wchar_t* P, wchar_t C )
{
  return wcsrchr(P,C);
}

Char* StrLower( Char* xP )
{
  Char* P = xP;
#ifdef _WIDESTRING
  while(*P != L'\0')
    *P++ = towlower(*P);
#else
  while(*P != '\0'){
    *P = tolower(*P);
    ++(*P);
  }
#endif  
  return xP;
}

Char* StrUpper( Char* xP )
{
  Char* P = xP;
 #ifdef _WIDESTRING
  while(*P != L'\0')
    *P++ = towupper(*P);
#else
  while(*P != '\0'){
    *P = toupper(*P);
    ++(*P);
  }
#endif  
  return xP;
}



Char* StrPos( Char* Str1, Char* Str2 )
{
  Char* result = NULL;
  Char* P = NULL;
  int lstr2 = 0;
  result = NULL;
  if ( ( Str1 == NULL ) || ( Str2 == NULL ) ) 
    return result;
  P = StrScan( Str1, *Str2 );
  if ( P == NULL ) 
    return result;
  lstr2 = StrLen( Str2 );
  while ( P != NULL ) 
  {
    if ( StrLComp( P, Str2, lstr2 ) == 0 ) 
    {
      result = P;
      return result;
    }
    P++;
    P = StrScan( P, *Str2 );
  }
  return result;
}


/*  StrPCopy copies the pascal String Source to Dest and returns Dest  */
//---------------------------------------------------------------------------
char* StrPCopy( char* Dest, const string& Source )
{
  strcpy(Dest, Source.c_str()); 
  return Dest;
}
//---------------------------------------------------------------------------
wchar_t* StrPCopy( wchar_t* Dest, const wstring& Source )
{
  wcscpy(Dest, Source.c_str()); 
  return Dest;
}

/*  StrPLCopy copies MaxLen or less characters from the pascal String
   Source to Dest and returns Dest  */


Char* StrPLCopy( Char* Dest, const String& Source, SizeUInt MaxLen )
{
  Char* result = NULL;
  SizeUInt Count = 0;
  result = Dest;
  if ( ( result != NULL ) && ( MaxLen != 0 ) ) 
  {
    Count = Source.length( );
    if ( Count > MaxLen ) 
      Count = MaxLen;
    StrMove( Dest, Source.c_str(), Count );
    ((Char*) result )[Count] = _T('\x00');  /* Terminate ! */
  }
  return result;
}




void InitInternationalGeneric( )
{
  FillChar( &SysLocale, sizeof( SysLocale ), 0 );   
}

int AnsiCompareFileName( const String& S1, const String& S2 )
{
  int result = 0;
  if ( FileNameCaseSensitive ) 
    result = AnsiCompareStr( S1, S2 ); // compare case sensitive
  else
    result = AnsiCompareText( S1, S2 ); // compare case insensitive. no MBCS yet.
  return result;
}


bool SameFileName( const String& S1, const String& S2 )
{
  return AnsiCompareFileName( S1, S2 ) == 0;
}


String AnsiLowerCaseFileName( const String& S )
{
  return AnsiLowerCase( S ); // no Locale support or MBCS yet.
}


String AnsiUpperCaseFileName( const String& S )
{
  return AnsiUpperCase( S ); // no Locale support or MBCS yet.
}


int AnsiPos( const String& Substr, const String& S )
{
  return Pos( Substr, S ); // no MBCS yet.
}


Char* AnsiStrPos( Char* str, Char* Substr )
{
  return StrPos( str, Substr );
}


Char* AnsiStrRScan( Char* str, Char Chr )
{
  return StrRScan( str, Chr );
}


Char* AnsiStrScan( Char* str, Char Chr )
{
  return StrScan( str, Chr );
}



/* ---------------------------------------------------------------------
    environment variable Auxiliary routines
  ---------------------------------------------------------------------*/


int FPC_EnvCount = - 1;


int FPCCountEnvVar( char** ep )
{
  int result = 0;
  if ( FPC_EnvCount == - 1 ) 
  {
    FPC_EnvCount = 0;
    if ( ep != NULL ) 
      while ( *ep != NULL ) 
      {
        FPC_EnvCount++;
        ep++;
      }
  }
  return FPC_EnvCount;
}


String FPCGetEnvVarFromP( PPChar ep, const String& EnvVar )
{
  String result;
  PPChar hp = NULL;
  String lenvvar, hs;
  int eqpos = 0;
  lenvvar = UpperCase( EnvVar );
  hp = ep;
  result = _T("");
  if ( hp != NULL ) 
    while (( *hp != NULL ) ) 
    {
      hs = *hp;
      eqpos = Pos( _T("="), hs );    
#ifdef _WIDESTRING
      if ( wcscmp(UpperCase( hs.substr( 1 - 1, eqpos - 1 ) ).c_str(), lenvvar.c_str()) == 0 ) 
#else
      if ( strcmp(UpperCase( hs.substr( 1 - 1, eqpos - 1 ) ).c_str(), lenvvar.c_str() ) == 0 ) 
#endif
      {
        result = hs.substr( eqpos + 1 - 1, hs.length( ) - eqpos );
        return result;
      }
      hp++;
    }
  return result;
}


string FPCGetEnvStrFromP( char** ep, int Index )
{
  string result;
  while (( *ep != NULL ) && ( Index > 1 ) ) 
  {
    Index--;
    ep++;
  }
  if (( *ep != NULL ) ) 
    result = *ep;
  return result;
}


/* ---------------------------------------------------------------------
    Application Name
  ---------------------------------------------------------------------*/


String VendorName( )
{
  String result;
  if (( OnGetVendorName != NULL ) ) 
    result = OnGetVendorName( );
  else
    result = _T("");
  return result;
}


String applicationName( )
{
  String result;
  if (( OnGetApplicationName != NULL ) ) 
    result = OnGetApplicationName( );
  else
    result = ChangeFileExt( ExtractFileName( ParamStr( 0 ) ), _T(""));
  return result;
}


/* ---------------------------------------------------------------------
  get TEMPORARY Directory Name
  ---------------------------------------------------------------------*/

#ifdef windows
String GetTempDir( bool Global )
{
  String result;
  if (( OnGetTempDir != NULL ) ) 
    result = OnGetTempDir( Global );
  else
  {
    result = GetEnvironmentVariable( _T("TEMP"));
    if ( result.empty()) 
      result = GetEnvironmentVariable( _T("TMP"));
  }
  if ( !result.empty()) 
    result = IncludeTrailingPathDelimiter( result );
  return result;
}
#elif defined(linux)
String GetTempDir( bool Global )
{
  String result;
  if (( OnGetTempDir != NULL ) )
    result = OnGetTempDir( Global );
  else
  {
    result = GetEnvironmentVariable( _T("TEMP") );
    if ( result.empty())
      result = GetEnvironmentVariable( _T("TMP") );
    if ( result.empty())
      result = _T("/tmp/"); // fallback.
  }
  if ( !result.empty())
    result = IncludeTrailingPathDelimiter( result );
  return result;
}
#else
#error unknown platform
#endif

String GetTempDir( )
{
  return GetTempDir( true );
}

/* ---------------------------------------------------------------------
  get TEMPORARY File Name
  ---------------------------------------------------------------------*/


String GetTempFileName( const String& Dir, const String& prefix )
{
  String result;
  int i = 0;
  String start;
  if (( OnGetTempFile != NULL ) ) 
    result = OnGetTempFile( Dir, prefix );
  else
  {
    if ( Dir.empty()) 
      start = GetTempDir();
    else
      start = IncludeTrailingPathDelimiter( Dir );
    if ( prefix.empty()) 
      start = start + _T("TMP");
    else
      start = start + prefix;
    i = 0;
    do
    {
//      result = Format( "%s%.5d.tmp", ARRAYOFCONST(( start, i )) );
#ifdef _WIDESTRING
      wstringstream buf;
#else
      stringstream buf;
#endif
      buf << start << std::setprecision(5) << i << _T(".tmp"); 
      result = buf.str();
      i++;
    }
    while ( ! ( ! FileExists( result ) ) );
  }
  return result;
}


String GetTempFileName( )
{
  return GetTempFileName( _T(""), _T(""));
}



DWORD GetTempFileName( Char* Dir, Char* prefix, DWORD uUnique, Char* TempFileName )
{
  DWORD result = 0;
  String P( prefix ), Buf;
  int l = 0;
  if ( uUnique != 0 ) 
  {
    //P = P + Format( "%.4x", ARRAYOFCONST(( uUnique )) );
#ifdef _WIDESTRING
    wstringstream buf;
#else
    stringstream buf;
#endif
    buf << setprecision(4) << uUnique; 
    P += buf.str();
  }
  Buf = GetTempFileName( Dir, P.c_str());
  l = Buf.length( );
  if ( l > 0 ) 
    memcpy( TempFileName, &Buf[1 - 1], (l + 1) * sizeof(Char) );
  if ( uUnique != 0 ) 
    result = uUnique;
  else
    result = 1;
  return result;
}


void FreeAndNil( void*& Obj )
{
  TObject* Temp = NULL;
  Temp = ((TObject*) Obj );
  Obj = NULL;
  delete Temp;
}



/* ---------------------------------------------------------------------
    Diskh functions, OS Independent.
  ---------------------------------------------------------------------*/


bool DoForceDirectories( const String& Dir, bool& xResult, EInOutError*& e, String& ADrv )
{
  bool result = false;
  String ADir;
  String APath;
  result = true;
  ADir = ExcludeTrailingPathDelimiter( Dir );
  if ( ADir.empty()) 
    return result;
  if ( ! DirectoryExists( ADir ) ) 
  {
    APath = ExtractFilePath( ADir );
      //this can happen ON Windows if User Specifies Dir like \User\Name/test/
      //and would, if not Checked for, cause an INFINITE recusrsion and A stack overflow
    if ( APath == ADir ) 
      result = false;
    else
      result = DoForceDirectories( APath, xResult, e, ADrv );
    if ( result ) 
      result = CreateDir( ADir );
  }
  return result;
}


bool IsUncDrive( const String& DRV, const String& Dir, bool& xResult, EInOutError*& e, String& ADrv )
{
  bool result = false;
  result = ( DRV.length( ) > 2 ) && ( DRV[1 - 1] == PathDelim ) && ( DRV[2 - 1] == PathDelim );
  return result;
}


bool ForceDirectories( const String& Dir )
{
  bool result = false;
  EInOutError* e = NULL;
  String ADrv;
  result = false;
  ADrv = ExtractFileDrive( Dir );
  if ( ( !ADrv.empty()) && ( ! DirectoryExists( ADrv ) ) && ( ! IsUncDrive( ADrv, Dir, result, e, ADrv ) ) ) 
    return result;
  if ( Dir.empty()) 
  {
    e = new EInOutError( _T(SysConst_SCannotCreateEmptyDir) );
    e->ErrorCode = 3;
    throw e;
  }
  result = DoForceDirectories( SetDirSeparators( Dir ), result, e, ADrv );
  return result;
}


/*
HMODULE SafeLoadLibrary( const std::string& Filename, DWORD ErrorMode )
{
  HMODULE result = 0;
  try
  {
    result = 0;
  }
  catch(...)
  {
    throw;
  }
  return result;
} */

#ifdef windows

// struct.inc
struct REMOTE_NAME_INFO {
  LPTSTR lpUniversalName;
  LPTSTR lpConnectionName;
  LPTSTR lpRemainingPath;
};


typedef REMOTE_NAME_INFO _REMOTE_NAME_INFO;
typedef REMOTE_NAME_INFO TREMOTENAMEINFO;
typedef REMOTE_NAME_INFO *PREMOTENAMEINFO;

String ExpandUNCFileName( const String& Filename )
/* returns empty String ON errors */
{
  String result;
  String S;
  DWORD Size = 0;
  DWORD rc = 0;
  wchar_t* Buf = NULL;
  S = ExpandFileName( Filename );
  S = S + _T("\x00");
  Size = MAX_PATH;
  GetMem( Buf, Size * sizeof(Char) );
  try
  {/*
    rc = WNetGetUniversalName( S.c_str(), UNIVERSAL_NAME_INFO_LEVEL, Buf, &Size );
    if ( rc == ERROR_MORE_DATA ) 
    {
      ReallocMem( Buf, Size * sizeof(Char) );
      rc = WNetGetUniversalName( S.c_str(), UNIVERSAL_NAME_INFO_LEVEL, Buf, &Size );
    }
    if ( rc == NO_ERROR ) 
      result = ((PREMOTENAMEINFO) Buf )->lpUniversalName;
    else
      if ( rc == ERROR_NOT_CONNECTED ) 
        result = Filename;
      else*/
        result = _T("");
  }
  catch(...)
  {
    FreeMem( Buf );
    throw;
  }
  /*# finally begin */
  FreeMem( Buf );
  /*# finally end */ 
  return result;
}


/*****************************************************************************
                              File functions
*****************************************************************************/



THandle FileOpen( const String& Filename, int Mode )
{
  THandle result = 0;
  unsigned int AccessMode [ 3/*# range 0..2*/ ];
  AccessMode[0] = GENERIC_READ;
  AccessMode[1] = GENERIC_WRITE;
  AccessMode[2] = GENERIC_READ | GENERIC_WRITE;
  int ShareMode [ 5/*# range 0..4*/ ];
  ShareMode[0] = 0;
  ShareMode[1] = 0;
  ShareMode[2] = FILE_SHARE_READ;
  ShareMode[3] = FILE_SHARE_WRITE;
  ShareMode[4] = FILE_SHARE_READ | FILE_SHARE_WRITE;
  String fn;
  fn = Filename + _T("\x00");
  result = CreateFile( &fn[1 - 1], ((DWORD) ( AccessMode[Mode & 3] ) ), ((DWORD) ( ShareMode[( Mode & 0xF0 ) >> 4] ) ), NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
  //if Fail API return feInvalidHandle (INVALIDE_HANDLE=feInvalidHandle=-1)
  return result;
}


THandle FileCreate( const String& Filename )
{
  THandle result = 0;
  String fn;
  fn = Filename + _T("\x00");
  result = CreateFile( &fn[1 - 1], GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
  return result;
}


THandle FileCreate( const String& Filename, int Mode )
{
  THandle result = 0;
  result = FileCreate( Filename );
  return result;
}


int FileRead( THandle Handle, void* Buffer, int Count )
{
  int result = 0;
  DWORD RES = 0;
  if ( ReadFile( Handle, Buffer, Count, &RES, NULL ) ) 
    result = RES;
  else
    result = - 1;
  return result;
}


int FileWrite( THandle Handle, void* Buffer, int Count )
{
  int result = 0;
  DWORD RES = 0;
  if ( WriteFile( Handle, Buffer, Count, &RES, NULL ) ) 
    result = RES;
  else
    result = - 1;
  return result;
}


int FileSeek( THandle Handle, int FOffset, int Origin )
{
  int result = 0;
  result = ((int) SetFilePointer( Handle, FOffset, NULL, Origin ) );
  return result;
}


int64_t FileSeek( THandle Handle, int64_t FOffset, int Origin )
{
  int64_t result = 0;
  if (( SetFilePointerEx != NULL ) ) 
  {
    LARGE_INTEGER li;
    li.QuadPart = FOffset;
    LARGE_INTEGER li2;
    if ( ! ( SetFilePointerEx( Handle, li, &li2, Origin ) ) ) 
      result = - 1;
    FOffset = li.QuadPart;
    result = li2.QuadPart;
  }
  else
    result = ((int) SetFilePointer( Handle, (long) FOffset, NULL, Origin ) );
  return result;
}


void FileClose( THandle Handle )
{
  if ( Handle <= (THandle) 4 ) 
    return;
  CloseHandle( Handle );
}


// types.pp / Windef.h
typedef _FILETIME TFileTime;


BOOL DosToWinTime( int DTime, TFileTime& Wtime )
{
  BOOL result = false;
  TFileTime lft;
  result = DosDateTimeToFileTime( ((LongRec*) &DTime )->Hi, ((LongRec*) &DTime )->Lo, &lft ) && LocalFileTimeToFileTime( &lft, &Wtime );
  return result;
}


BOOL WinToDosTime( TFileTime& Wtime, int& DTime )
{
  BOOL result = false;
  TFileTime lft;
  result = FileTimeToLocalFileTime( &Wtime, &lft ) && FileTimeToDosDateTime( &lft, &((LongRec*) &DTime )->Hi, &((LongRec*) &DTime )->Lo );
  return result;
}


int FileAge( const String& Filename )
{
  int result = 0;
  THandle Handle = 0;
  TWin32FindData FindData;
  Handle = FindFirstFile( Filename.c_str(), &FindData );
  if ( Handle != INVALID_HANDLE_VALUE ) 
  {
    ::FindClose( Handle );
    if ( ( ( FindData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ) ) == 0 ) 
      if ( WinToDosTime( FindData.ftLastWriteTime, result ) ) 
        return result;
  }
  result = - 1;
  return result;
}


bool FileExists( const String& Filename )
{
  bool result = false;
  DWORD Attr = 0;
  Attr = GetFileAttributes( Filename.c_str());
  if ( Attr != 0xFFFFFFFF ) 
    result = ( ( Attr & FILE_ATTRIBUTE_DIRECTORY ) ) == 0;
  else
    result = false;
  return result;
}


bool DirectoryExists( const String& Directory )
{
  bool result = false;
  DWORD Attr = 0;
  Attr = GetFileAttributes( Directory.c_str());
  if ( Attr != 0xFFFFFFFF ) 
    result = ( ( Attr & FILE_ATTRIBUTE_DIRECTORY ) ) > 0;
  else
    result = false;
  return result;
}


int findmatch( TSearchRec& F )
{
  int result = 0;
  /* find File with correct attribute */
  while ( ( ( F.FindData.dwFileAttributes & ((unsigned int) F.ExcludeAttr ) ) ) != 0 ) 
  {
    if ( ! FindNextFile( F.FindHandle, &F.FindData ) ) 
    {
      result = GetLastError();
      return result;
    }
  }
  /* convert some Attributes back */
  WinToDosTime( F.FindData.ftLastWriteTime, F.Time );
  F.Size = F.FindData.nFileSizeLow;
  F.Attr = F.FindData.dwFileAttributes;
  F.Name = F.FindData.cFileName;
  return 0;
}


int FindFirst( const String& Path, int Attr, TSearchRec& Rslt )
{
  int result = 0;
  Rslt.Name = Path;
  Rslt.Attr = Attr;
  Rslt.ExcludeAttr = ( ~ Attr ) & ( 0x1e );
                 /* $1e = faHidden or faSysFile or faVolumeID or faDirectory */
  /* FindFirstFile is A Win32 call */
  Rslt.FindHandle = FindFirstFile( Path.c_str(), &Rslt.FindData );
  if ( Rslt.FindHandle == INVALID_HANDLE_VALUE ) 
  {
    result = GetLastError();
    return result;
  }
  /* find File with correct attribute */
  result = findmatch( Rslt );
  return result;
}


int FindNext( TSearchRec& Rslt )
{
  int result = 0;
  if ( FindNextFile( Rslt.FindHandle, &Rslt.FindData ) ) 
    result = findmatch( Rslt );
  else
    result = GetLastError();
  return result;
}


void FindClose( TSearchRec& F )
{
  if ( F.FindHandle != INVALID_HANDLE_VALUE ) 
    ::FindClose( F.FindHandle );
}


int FileGetDate( THandle Handle )
{
  int result = 0;
  TFileTime ft;
  if ( GetFileTime( Handle, NULL, NULL, &ft ) & WinToDosTime( ft, result ) ) 
    return result;
  result = - 1;
  return result;
}


int FileSetDate( THandle Handle, int Age )
{
  int result = 0;
  TFileTime ft;
  result = 0;
  if ( DosToWinTime( Age, ft ) & SetFileTime( Handle, NULL, NULL, &ft ) ) 
    return result;
  result = GetLastError();
  return result;
}


int FileGetAttr( const String& Filename )
{
  int result = 0;
  result = GetFileAttributes( Filename.c_str());
  return result;
}


int FileSetAttr( const String& Filename, int Attr )
{
  int result = 0;
  if ( SetFileAttributes( Filename.c_str(), Attr ) ) 
    result = 0;
  else
    result = GetLastError();
  return result;
}


bool DeleteFile( const String& Filename )
{
  return ::DeleteFile( Filename.c_str()) == TRUE;
}


bool RenameFile( const String& OldName, const String& newName )
{
  return MoveFile( OldName.c_str(), newName.c_str()) == TRUE;
}


/*****************************************************************************
                              disk functions
*****************************************************************************/


/*
#ifdef _WIDESTRING
typedef BOOL ( __stdcall * TGetDiskFreeSpaceEx )( wchar_t* , void* , void* , void*  );
#else
typedef BOOL ( __stdcall * TGetDiskFreeSpaceEx )( char* , void* , void* , void*  );
#endif


TGetDiskFreeSpaceEx GetDiskFreeSpaceEx;
*/

int64_t DiskFree( unsignedchar Drive )
{
  int64_t result = 0;
  Char disk[ 4/*# range 1..4*/ ];
  ULARGE_INTEGER qwtotal, qwfree, qwcaller;
  if ( Drive == 0 ) 
  {
    disk[1 - 1] = _T('\\');
    disk[2 - 1] = _T('\x00');
  }
  else
  {
    disk[1 - 1] = Char( Drive + 64 );
    disk[2 - 1] = _T(':');
    disk[3 - 1] = _T('\\');
    disk[4 - 1] = _T('\x00');
  }
  if ( GetDiskFreeSpaceEx( &disk[1 - 1], &qwcaller, &qwtotal, &qwfree ) ) 
    result = qwfree.QuadPart;
  else
    result = - 1;
/*  }
  else
  {
    DWORD secs = 0, Bytes = 0, Free = 0, total = 0;
    if ( GetDiskFreeSpace( &disk[1 - 1], &secs, &Bytes, &Free, &total ) ) 
      result = ((int64_t) Free ) * secs * Bytes;
    else
      result = - 1;
  }*/
  return result;
}


int64_t DiskSize( unsignedchar Drive )
{
  int64_t result = 0;
  Char disk[ 4/*# range 1..4*/ ];
  ULARGE_INTEGER qwtotal, qwfree, qwcaller;
  if ( Drive == 0 ) 
  {
    disk[1 - 1] = _T('\\');
    disk[2 - 1] = _T('\x00');
  }
  else
  {
    disk[1 - 1] = Char( Drive + 64 );
    disk[2 - 1] = _T(':');
    disk[3 - 1] = _T('\\');
    disk[4 - 1] = _T('\x00');
  }

  if ( GetDiskFreeSpaceEx( &disk[1 - 1], &qwcaller, &qwtotal, &qwfree ) ) 
      result = qwtotal.QuadPart;
    else
      result = - 1;
  return result;
}


String GetCurrentDir( )
{
  String result;
  GetDir( 0, result );
  return result;
}


bool SetCurrentDir( const String& newdir )
{
  bool result = false;
  result = SetCurrentDirectory( newdir.c_str()) == TRUE;
  return result;
}


bool CreateDir( const String& newdir )
{
  bool result = false;
  result = CreateDirectory( newdir.c_str(), NULL ) == TRUE;
  return result;
}


bool RemoveDir( const String& Dir )
{
  return RemoveDirectory( Dir.c_str()) == TRUE;
}

void SetString(std::string Str, char* myPtr, int size)
{
    
}

/*****************************************************************************
                              Locale functions
*****************************************************************************/


String GetLocaleStr( int lid, int LT, const String& def )
{
  String result;
  int l = 0;
  Char Buf[ 256/*# range 0..255*/ ];
#ifdef _WIDESTRING
  l = GetLocaleInfoW( lid, LT, Buf, sizeof( Buf ) );
#else
  l = GetLocaleInfoA( lid, LT, Buf, sizeof( Buf ) );
#endif
  if ( l > 0 ) 
    SetString( result, &Buf[0], l - 1 );
  else
    result = def;
  return result;
}


Char GetLocaleChar( int lid, int LT, Char def )
{
  Char Buf[ 2/*# range 0..1*/ ];
  if ( GetLocaleInfo( lid, LT, Buf, 2 ) > 0 ) 
    return Buf[0];
  else
    return def;
}


int GetLocaleInt( int lid, int tp, int def )
{
  int result = 0;
  String S;
  int C = 0;
  S = GetLocaleStr( lid, tp, _T("0"));
  Val( S, &result, C );
  if ( C != 0 ) 
    result = def;
  return result;
}


void GetFormatSettings( )
{
  String HF;
  LCID lid = 0;
  int i = 0, Day = 0;
  lid = GetThreadLocale();
  /* Date stuff */
  for ( i = 1; i <= 12; i++)
  {
    ShortMonthNames[i - 1] = GetLocaleStr( lid, LOCALE_SABBREVMONTHNAME1 + i - 1, ShortMonthNames[i - 1] );
    LongMonthNames[i - 1] = GetLocaleStr( lid, LOCALE_SMONTHNAME1 + i - 1, LongMonthNames[i - 1] );
  }
  for ( i = 1; i <= 7; i++)
  {
    Day = ( i + 5 ) % 7;
    ShortDayNames[i - 1] = GetLocaleStr( lid, LOCALE_SABBREVDAYNAME1 + Day, ShortDayNames[i - 1] );
    LongDayNames[i - 1] = GetLocaleStr( lid, LOCALE_SDAYNAME1 + Day, LongDayNames[i - 1] );
  }
  DateSeparator = GetLocaleChar( lid, LOCALE_SDATE, _T('/'));
  ShortDateFormat = GetLocaleStr( lid, LOCALE_SSHORTDATE, _T("m/d/yy"));
  LongDateFormat = GetLocaleStr( lid, LOCALE_SLONGDATE, _T("mmmm d, yyyy"));
  /* time stuff */
  TimeSeparator = GetLocaleChar( lid, LOCALE_STIME, _T(':'));
  TimeAMString = GetLocaleStr( lid, LOCALE_S1159, _T("AM"));
  TimePMString = GetLocaleStr( lid, LOCALE_S2359, _T("PM"));
  if ( StrToIntDef( GetLocaleStr( lid, LOCALE_ITLZERO, _T("0")), 0 ) == 0 ) 
    HF = _T("h");
  else
    HF = _T("hh");
  // no support for 12 Hour stuff at the moment...
  ShortTimeFormat = String( HF ) + _T(":mm"); //_T(":nn");
  LongTimeFormat = String( HF ) +_T(":mm:ss"); // _T(":nn:ss");
  /* Currency stuff */
  CurrencyString = GetLocaleStr( lid, LOCALE_SCURRENCY, _T(""));
  CurrencyFormat = StrToIntDef( GetLocaleStr( lid, LOCALE_ICURRENCY, _T("0")), 0 );
  NegCurrFormat = StrToIntDef( GetLocaleStr( lid, LOCALE_INEGCURR, _T("0")), 0 );
  /* number stuff */
  ThousandSeparator = GetLocaleChar( lid, LOCALE_STHOUSAND, _T(','));
  DecimalSeparator = GetLocaleChar( lid, LOCALE_SDECIMAL, _T('.'));
  CurrencyDecimals = StrToIntDef( GetLocaleStr( lid, LOCALE_ICURRDIGITS, _T("0")), 0 );
}


void InitInternational( )
{
  /* A call to GetSystemMetrics changes the Value of the 8087 Control WORD ON
    Pentium4 with WINXP SP2 */
  WORD old8087CW = 0;
  InitInternationalGeneric();
//  old8087CW = Get8087CW;
  SysLocale.MBCS = GetSystemMetrics( SM_DBCSENABLED ) != 0;
  SysLocale.RightToLeft = GetSystemMetrics( SM_MIDEASTENABLED ) != 0;
  SysLocale.DefaultLCID = 0x0409;
  SysLocale.PriLangID = LANG_ENGLISH;
  SysLocale.SubLangID = SUBLANG_ENGLISH_US;
  // probably needs Update with GetThreadLocale. post 2.0.2
// todo  Set8087CW( old8087CW );
  GetFormatSettings();
}




//#ifdef windows


String GetEnvironmentVariable( const String& EnvVar )
{
  String result;
  String S;
  int i = 0;
  Char* hp = NULL,* P = NULL;
  result = _T("");
  P = GetEnvironmentStrings();
  hp = P;
  while ( *hp != _T('\x00')) 
  {
    S = hp;
    i = Pos( _T("="), S );
    if ( i > 0 && UpperCase( S.substr( 1 - 1, i - 1 ) ) == UpperCase( EnvVar ) ) 
    {
      result = S.substr( i + 1 - 1, S.length( ) - i );
      break;
    }
        /* Next String entry*/
#ifdef _WIDESTRING
    hp = hp + wcslen( hp ) + 1;
#else
    hp = hp + strlen( hp ) + 1;
#endif
  }
  FreeEnvironmentStrings( P );
  return result;
}


int GetEnvironmentVariableCount( )
{
  int result = 0;
  Char* hp = NULL,* P = NULL;
  result = 0;
  P = GetEnvironmentStrings();
  hp = P;
  if ( hp != NULL ) 
    while ( *hp != _T('\x00')) 
    {
      result++;
#ifdef _WIDESTRING
      hp = hp + wcslen( hp ) + 1;
#else
      hp = hp + strlen( hp ) + 1;
#endif
    }
  FreeEnvironmentStrings( P );
  return result;
}


String GetEnvironmentString( int Index )
{
  String result;
  Char* hp = NULL,* P = NULL;
  result = _T("");
  P = GetEnvironmentStrings();
  hp = P;
  if ( hp != NULL ) 
  {
    while ( ( *hp != _T('\x00')) && ( Index > 1 ) ) 
    {
      Index--;
#ifdef _WIDESTRING
      hp = hp + wcslen( hp ) + 1;
#else
      hp = hp + strlen( hp ) + 1;
#endif
    }
    if ( *hp != _T('\x00')) 
      result = hp;
  }
  FreeEnvironmentStrings( P );
  return result;
}



int ExecuteProcess( const String& Path, const String& comline )
{
  int result = 0;
  STARTUPINFO SI;
  PROCESS_INFORMATION pi;
  THandle Proc = 0;
  DWORD l = 0;
  String commandLine;
  EOSError* e = NULL;
  FillChar( &SI, sizeof( SI ), 0 );
  SI.cb = sizeof( SI );
  SI.wShowWindow = 1;
  /* always surround the Name of the Application by quotes
    So that LONG filenames will always be accepted. but don'T
    do it if there are Already Double quotes, since Win32 does not
    like Double quotes which are duplicated!
  */
  if ( Pos( _T("\""), Path ) == 0 ) 
    commandLine = String( _T("\"") ) + Path + _T("\"");
  else
    commandLine = Path;
  if ( !comline.empty()) 
    commandLine = commandLine + _T(" ")+ comline + _T("\x00");
  else
    commandLine = commandLine + _T("\x00");
  if ( ! CreateProcess( NULL, (Char*) commandLine.c_str(), NULL, NULL, false, 0x20, NULL, NULL, &SI, &pi ) ) 
  {
    //e = new EOSError( _T(SysConst_SExecuteProcessFailed), ARRAYOFCONST( commandLine, (int) GetLastError() ) );
    String sError = _T("Failed to execute ");
    sError += commandLine;
    sError += _T(", error code: ");
    sError += IntToStr(GetLastError());
    e = new EOSError( sError );
    e->ErrorCode = GetLastError();
    throw e;
  }
  Proc = pi.hProcess;
  if ( WaitForSingleObject( Proc, ((DWORD) 0xFFFFFFFF ) ) != 0xFFFFFFFF ) 
  {
    GetExitCodeProcess( Proc, &l );
    CloseHandle( Proc );
    CloseHandle( pi.hThread );
    result = l;
  }
  else
  {
    //e = new EOSError( _T(SysConst_SExecuteProcessFailed), ARRAYOFCONST( commandLine, (int) GetLastError() ) );
    String sError = _T("Failed to execute ");
    sError += commandLine;
    sError += _T(", error code: ");
    sError += IntToStr(GetLastError());
    e = new EOSError( sError );
    e->ErrorCode = GetLastError();
    CloseHandle( Proc );
    CloseHandle( pi.hThread );
    throw e;
  }
  return result;
}


int ExecuteProcess( const String& Path, const String* comline, int comline_maxidx )
{
  int result = 0;
  String commandLine;
  int i = 0;
  commandLine = _T("");
  for ( i = 0; i <= comline_maxidx /*# High(comline) */; i++)
    if ( Pos( _T(" "), comline[i] ) != 0 ) 
      commandLine = commandLine + _T(" ")+ _T("\"") + comline[i] + _T("\"");
    else
      commandLine = commandLine + _T(" ")+ comline[i];
  result = ExecuteProcess( Path, commandLine );
  return result;
}


#elif defined(linux)
String GetEnvironmentVariable( const String& EnvVar )
{
  char* p;
#ifdef _WIDESTRING
  string s = wstr2str(EnvVar);
  p = getenv( s.c_str());
  if(p != NULL)
     return str2wstr( p );
#else
  p = getenv( EnvVar.c_str());
  if(p != NULL)
    return StrPas( p );
#endif
  else
   return String();   
}

char** envp = NULL;

int GetEnvironmentVariableCount( )
{
  return FPCCountEnvVar( envp );
}


String GetEnvironmentString( int Index )
{
#ifdef _WIDESTRING
  return str2wstr(FPCGetEnvStrFromP( envp, Index ));
#else
  return FPCGetEnvStrFromP( envp, Index );
#endif
}

char** ArrayStringToPPchar( const string* S, int S_maxidx, int reserveentries ) // const ?
// extra allocate reserveentries PChar'S at the beginning (Default Param=0 After 1.0.X ?)
// Note: for Internal use by skilled programmers only
// if "S" goes out of scope in the Parent procedure, the Pointer is dangling.

{
  char** P = NULL;
  int i = 0;
  if ( S_maxidx /*# High(S) */ < 0 /*# Low(S) */ )
    return NULL;
  GetMem( P, sizeof( char* ) * ( S_maxidx /*# High(S) */ - 0 /*# Low(S) */ + reserveentries + 2 ) );  // one more for nil, one more
                                              // for cmd
  if ( P == NULL )
  {
    return NULL;
  }
  for ( i = 0 /*# Low(S) */; i <= S_maxidx /*# High(S) */; i++)
    P[i + reserveentries] = (char*) S[i].c_str();
  P[S_maxidx /*# High(S) */ + 1 + reserveentries] = NULL;
  return P;
}

/*
#define System__38 ( TSet < Char, 0, 255 >() \
                     << Char ( _T(' ') ) << Char ( _T('\t') ) << Char ( _T('\n') ) )
#define System__39 ( TSet < Char, 0, 255 >() \
                     << Char ( 0 ) << Char ( _T('"') ) )
#define System__40 ( TSet < Char, 0, 255 >() \
                     << Char ( _T(' ') ) << Char ( 0 ) << Char ( _T('\t') ) << Char ( _T('\n') ) )
#define System__41 ( TSet < Char, 0, 255 >() \
                     << Char ( _T(' ') ) << Char ( _T('\t') ) << Char ( _T('\n') ) )
#define System__42 ( TSet < Char, 0, 255 >() \
                     << Char ( 0 ) << Char ( _T('"') ) )
#define System__43 ( TSet < Char, 0, 255 >() \
                     << Char ( _T(' ') ) << Char ( 0 ) << Char ( _T('\t') ) << Char ( _T('\n') ) )

*/
#define System__38 ( TSet < unsigned char, 0, 255 >() \
                     << (unsigned char) ( ' ' ) << (unsigned char) ( '\t' ) << (unsigned char) ( '\n' ) )
#define System__39 ( TSet < unsigned char, 0, 255 >() \
                     << (unsigned char) ( 0 ) << (unsigned char) ( '"' ) )
#define System__40 ( TSet < unsigned char, 0, 255 >() \
                     << (unsigned char) ( ' ' ) << (unsigned char) ( 0 ) << (unsigned char) ( '\t' ) << (unsigned char) ( '\n' ) )
#define System__41 ( TSet < unsigned char, 0, 255 >() \
                     << (unsigned char) ( ' ' ) << (unsigned char) ( '\t' ) << (unsigned char) ( '\n' ) )
#define System__42 ( TSet < unsigned char, 0, 255 >() \
                     << (unsigned char) ( 0 ) << (unsigned char) ( '"' ) )
#define System__43 ( TSet < unsigned char, 0, 255 >() \
                     << (unsigned char) ( ' ' ) << (unsigned char) ( 0 ) << (unsigned char) ( '\t' ) << (unsigned char) ( '\n' ) )
char** StringToPPChar( char* S, int reserveentries )
{
  char** result = NULL;
  int i = 0, nr = 0;
  char* Buf = NULL;
  char** P = NULL;
  Buf = S;
  nr = 1;
  while ( *Buf != _T('\x00') )                   // Count nr of Args
  {
    while ( System__38.Contains(*Buf ) )    // Kill separators.
      Buf++;
    nr++;
    if ( *Buf == _T('\"') )                   // quotes argument?
    {
      Buf++;
      while ( ! ( System__39.Contains(*Buf ) ) ) // then end of argument is end of String or Next Quote
        Buf++;
      if ( *Buf == _T('\"') )                // Skip Closing Quote.
        Buf++;
    }
    else
    {                            // else Std
      while ( ! ( System__40.Contains(*Buf ) ) )
        Buf++;
    }
  }
  GetMem( P, ( reserveentries + nr ) * sizeof( Char* ) );
  result = P;
  if ( P == NULL )
  {
    return result;
  }
  for ( i = 1; i <= reserveentries; i++)
    P++; // Skip empty slots
  Buf = S;
  while ( *Buf != _T('\x00') )
  {
    while ( System__41.Contains(*Buf ) )    // Kill separators.
    {
      *Buf = _T('\x00');
      Buf++;
    }
    if ( *Buf == _T('\"') )                   // quotes argument?
    {
      Buf++;
      *P = Buf;
      P++;
      *P = NULL;
      while ( ! ( System__42.Contains(*Buf ) ) ) // then end of argument is end of String or Next Quote
        Buf++;
      if ( *Buf == _T('\"') )                // Skip Closing Quote.
      {
        *Buf = _T('\x00');
        Buf++;
      }
    }
    else
    {
      *P = Buf;
      P++;
      *P = NULL;
      while ( ! ( System__43.Contains(*Buf ) ) )
        Buf++;
    }
  }
  return result;
} 

char** StringToPPChar( std::string& S, int reserveentries )
/*
  Create A PPChar to structure of pchars which are the Arguments specified
  in the String S. especially usefull for creating an Argv for Exec-calls
*/
{
  return StringToPPChar( (char*) S.c_str(), reserveentries );
}

cint FpExecV( const char* Path, char** Argv )
{
  cint r = execve(Path, Argv, envp);
  return r==-1 ? -errno : r;
}

void Fpexit( int Status )
{
  syscall(SYS_exit_group, Status);
}

pid_t fpfork( )
/*
  this function issues the 'fork' System call. the program is duplicated in memory
  and execution continues in Parent and Child process.
  in the Parent process, fork returns the pid of the Child. in the Child process,
  zero is returned.
  A Negative Value indicates that an Error has occurred, the Error is returned in
  LinuxError.
*/
{
  pid_t r = fork();
  return r==-1 ? -errno : r;
} 

/*******************************************************************************
                             utility calls
*******************************************************************************/

enum TFSearchOption {NoCurrentDirectory,
                     CurrentDirectoryFirst,
                     CurrentDirectoryLast };
		     
typedef struct stat Stat;

bool FpS_ISDIR(  mode_t m )
{
  return ( ( ( m & S_IFMT ) ) == S_IFDIR );
}


string FSearch( const string& Path, string DirList, TFSearchOption CurrentDirStrategy )
/*
  searches for A File 'path' in the List of direcories in 'dirlist'.
  returns an empty String if not found. wildcards are not allowed.
  if DirList is empty, it is set to '.'

this function tries to make FSearch use AnsiStrings, and decrease
stringhandling Overhead at the same Time.

*/
{
  string result;
  string mydir, newdir;
  cint p1 = 0;
  Stat info;
  cint i = 0, j = 0;
  const char* P = NULL;
  if ( CurrentDirStrategy == CurrentDirectoryFirst )
    DirList = string( ".:" ) + DirList;             /*make sure current Dir is First to be searched.*/
  if ( CurrentDirStrategy == CurrentDirectoryLast )
    DirList = DirList + ":.";             /*make sure current Dir is Last to be searched.*/

/*replace ':' and ';' with #0*/
  for ( p1 = 1; p1 <= DirList.length( ); p1++)
    if ( ( DirList[p1 - 1] == ':' ) || ( DirList[p1 - 1] == ';' ) )
      DirList[p1 - 1] = '\x00';

/*Check for wildcards*/
  if ( ( Pos( "?", Path ) != 0 ) || ( Pos( "*", Path ) != 0 ) )
    result = ""; /*no wildcards allowed in These things.*/
  else
  {
    P = DirList.c_str();
    i = DirList.length( );
    j = 1;
    do
    {
      mydir = P;
      if ( ( mydir.length( ) > 0 ) && ( mydir[mydir.length( ) - 1] != '/' ) )
        mydir = mydir + "/";
      newdir = mydir + Path;
      if ( ( stat( newdir.c_str(), &info ) >= 0 ) && ( ! FpS_ISDIR( info.st_mode ) ) )
      {
        if ( Pos( "./", newdir ) == 1 )
          newdir.erase( 1 - 1, 2 );
        /*DOS strips OFF an Initial .\*/
      }
      else
        newdir = "";
      while ( ( j <= i ) && ( *P != '\x00' ) )
      {
        j++;
        P++;
      }
      if ( *P == '\x00')
        P++;
    }
    while ( ! ( ( j >= i ) || ( newdir.length( ) > 0 ) ) );
    result = newdir;
  }
  return result;
}


string FSearch( const string& Path, string DirList )
{
  return FSearch( Path, DirList, CurrentDirectoryFirst );
}

/*******************************************************************************
                          process related calls
*******************************************************************************/

/* most calls of waitpid do not Handle the result correctly, this funktion treats errors more correctly */


cint fpexecve( const char* Path, char** Argv, char** envp )
{
  cint r = execve(Path, Argv, envp);
  return r==-1 ? -errno : r;
}


cint intFpExecVEMaybeP( const string& pathname, char** Args, char** MyEnv, bool SearchPath )
// does an Execve, but still has to Handle P
// Execv Variants call this directly, Execl Variants indirectly via
//     intfpexecl

{
  cint result = 0;
  string NewCmd;
  string thepath;
  if ( SearchPath && ( Pos( "/", pathname ) == 0 ) )
  {
      // the above could be better. (Check if not escaped/quoted '/'S) ?
      // (Jilles says this is ok)
      // Stevens says only Search if NewCmd contains no '/'
      // FSearch is not AnsiString clean yet.

    thepath = getenv( "PATH");
    if ( thepath.empty())
      thepath = ".";     // FreeBSD uses _PATH_DEFPATH = /usr/bin:/bin
                          // but A quick Check showed that _PATH_DEFPATH
                          // varied from OS to OS
    NewCmd = FSearch( pathname, thepath, NoCurrentDirectory );
      // FreeBSD libc keeps ON Trying till A File is successfully Run.
      // Stevens says "try each Path prefix"

      // execp puts NewCmd here.
    *Args = (char*) NewCmd.c_str();
  }
  else
    NewCmd = pathname;
 // Repeat
//      if SearchPath then Args^:=PChar(commandtorun)
  result = fpexecve( NewCmd.c_str(), Args, MyEnv );

/*
// Code that if Exec fails due to permissions, tries to Run it with sh
// should we deallocate P ON Fail? -> no Fpexit is Run no matter What
//
*/
// if intFpExecVEMaybeP=-1 then zoekvolgende File.
// until (Goexit) or SearchExit;


/*
 if IntFpExec=-1 then
    begin
      Error:=fpgeterrno
      case Error of
        ESysE2BIG  : exit(-1);
        ESysELOOP,
          : exit(-1);

*/
  return result;
}




pid_t fpwaitpid( pid_t pid, cint* stat_loc, cint Options )
/*
  waits until A Child with pid pid exits, or returns if it is exited Already.
  Any resources used by the Child are freed.
  the exit Status is reported in the adress referred to by Status. it should
  be A Longint.
*/
{
  pid_t r = waitpid(pid, stat_loc, Options );
  return r==-1 ? -errno : r;
}

cint intfpexecl( const string& pathname, const string* S, int S_maxidx, char** MyEnv, bool SearchPath )
/* handles the array of AnsiString -> PPChar conversion.
  Base for the the "l" Variants.
*/
{
  cint result = 0;
  char** P = NULL;
  if ( pathname.empty())
  {
    seterrno( ESysENOENT );
    return - 1;                 // errno?
  }
  P = ArrayStringToPPchar( S, S_maxidx, 1 );
  if ( P == NULL )
  {
    GetMem( P, 2 * sizeof( Char* ) );
    if ( P == NULL )
    {
      seterrno( ESysENOENT );
      return - 1;
    }
    P[1] = NULL;
  }
  *P = (char*) pathname.c_str();
  result = intFpExecVEMaybeP( pathname, P, MyEnv, SearchPath );
  // if we come here, no attempts were executed successfully.
  FreeMem( P, -1 );
  return result;
}


cint FpExecLE( const string& pathname, const string* S, int S_maxidx, char** MyEnv )
{
  return intfpexecl( pathname, S, S_maxidx, MyEnv, false );
}


cint FpExecL( const string& pathname, const string* S, int S_maxidx )
{
  return intfpexecl( pathname, S, S_maxidx, envp, false );
}


cint FpExecLP( const string& pathname, const string* S, int S_maxidx )
{
  return intfpexecl( pathname, S, S_maxidx, envp, true );
}


cint FpExecLPE( const string& pathname, const string* S, int S_maxidx, char** env )
{
  return intfpexecl( pathname, S, S_maxidx, env, true );
}


cint FpExecV( const string& pathname, char** Args )
{
  return intFpExecVEMaybeP( pathname, Args, envp, false );
}


cint FpExecVP( const string& pathname, char** Args )
{
  return intFpExecVEMaybeP( pathname, Args, envp, true );
}


cint FpExecVPE( const string& pathname, char** Args, char** env )
{
  return intFpExecVEMaybeP( pathname, Args, env, true );
}



/* most calls of waitpid do not Handle the result correctly, this funktion treats errors more correctly */


cint WaitProcess( cint pid ) /* like waitpid(pid,@result,0) handling of signal interrupts (errno=EINTR), returning the ExitCode of process (>=0) or -Status if terminated*/
{
  cint result = 0;
  cint r = 0, S = 0;
  S = 0x7F00;
  do
  {
    r = fpwaitpid( pid, &S, 0 );
    if ( ( r == - 1 ) && ( geterrno() == ESysEINTR ) )
      r = 0;
  }
  while ( ! ( r != 0 ) );
  if ( ( r == - 1 ) || ( r == 0 ) ) // 0 is not A valid return and should never occur (it means Status Invalid when Using WNOHANG)
    result = - 1; // return -1 to indicate an Error.  fpwaitpid updated it.
  else
  {
    if ( WIFEXITED( S ) )
      result = WEXITSTATUS( S );
    else
      if ( S > 0 )  // until Now there is not use of the highest bit , but Check this for the future
        result = - S; // normal case
      else
        result = S; // S<0 should not occur, but wie return also A negativ Value
  }
  return result;
}


int ExecuteProcess( const std::string& Path, const std::string& comline )
{
  int result = 0;
  int pid = 0;
  EOSError* e = NULL;
  std::string commandLine;
  char** cmdline2 = NULL;
  /* always surround the Name of the Application by quotes
    So that LONG filenames will always be accepted. but don'T
    do it if there are Already Double quotes!
  */

   // only place we still parse
  cmdline2 = NULL;
  if ( !comline.empty())
  {
    commandLine = comline;
       /* make an unique copy because StringToPPChar modifies the
         String */
    //UniqueString( commandLine );
    cmdline2 = StringToPPChar( ( char*) commandLine.c_str(), 1 );
    *cmdline2 = (char*) Path.c_str();
  }
  else
  {
    GetMem( cmdline2, 2 * sizeof( char* ) );
    *cmdline2 = (char*) Path.c_str();
    cmdline2[1] = NULL;
  }
  pid = fpfork();
  if ( pid == 0 )
  {
   /*the Child does the Actual Exec, and then exits*/
    FpExecV( (char*) Path.c_str(), cmdline2 );
     /* if the Execve fails, we return an exitvalue of 127, to let it be known*/
    Fpexit( 127 );
  }
  else
    if ( pid == - 1 )         /*fork Failed*/
    {
      //e = new EOSError( str2wstr(SysConst_SExecuteProcessFailed), ARRAYOFCONST(( Path, -1 )) );
      String sError = _T("Failed to execute ");
#ifdef _WIDESTRING    
      sError += str2wstr(Path);
#else
      sError += Path;
#endif      
      e = new EOSError( sError );      
      e->ErrorCode = - 1;
      throw e;
    }

  /* we're in the parent, let'S wait. */
  result = WaitProcess( pid ); // waitpid and result-convert
  if ( !comline.empty())
    FreeMem( cmdline2, -1 );
  if ( ( result < 0 ) || ( result == 127 ) )
  {
    //e = new EOSError( str2wstr(SysConst_SExecuteProcessFailed), ARRAYOFCONST(( Path, 0 )) );
    String sError = _T("Failed to execute ");
#ifdef _WIDESTRING    
    sError += str2wstr(Path);
#else
    sError += Path;
#endif
    e->ErrorCode = result;
    throw e;
  }
  return result;
}


int ExecuteProcess( const string& Path, const string* comline, int comline_maxidx )
{
  int result = 0;
  int pid = 0;
  EOSError* e = NULL;
  pid = fpfork();
  if ( pid == 0 )
  {
     /*the Child does the Actual Exec, and then exits*/
    FpExecL( Path, comline, comline_maxidx );
     /* if the Execve fails, we return an exitvalue of 127, to let it be known*/
    Fpexit( 127 );
  }
  else
    if ( pid == - 1 )         /*fork Failed*/
    {
      //e = new EOSError( str2wstr(SysConst_SExecuteProcessFailed), ARRAYOFCONST(( Path, -1 )) );
      String sError = _T("Failed to execute ");
#ifdef _WIDESTRING
      sError += str2wstr(Path);
#else
      sError += Path;
#endif
      e = new EOSError( sError );
      e->ErrorCode = - 1;
      throw e;
    }

  /* we're in the parent, let'S wait. */
  result = WaitProcess( pid ); // waitpid and result-convert
  if ( ( result < 0 ) || ( result == 127 ) )
  {
    //e = new EOSError( str2wstr(SysConst_SExecuteProcessFailed), ARRAYOFCONST(( Path, 0 )) );
    String sError = _T("Failed to execute ");
#ifdef _WIDESTRING
      sError += str2wstr(Path);
#else
      sError += Path;
#endif    
    e = new EOSError( sError );
    e->ErrorCode = result;
    throw e;
  }
  return result;
}

//typedef timespec* ptimespec;
typedef timespec TTimeSpec;

void Sleep( unsignedint milliseconds )
{
  TTimeSpec timeout, timeoutresult;
  timeout.tv_sec = milliseconds / 1000;
  timeout.tv_nsec = 1000 * 1000 * ( milliseconds % 1000 );
//  FpNanoSleep( &timeout, &timeoutresult );
  nanosleep( &timeout, &timeoutresult );
}


TLibHandle LoadLibrary( string Name )
{
  return ((TLibHandle) ((uintptr_t) dlopen( Name.c_str(), RTLD_LAZY )));
}

TLibHandle LoadLibrary( wstring Name )
{
  string s = wstr2str(Name);
  return LoadLibrary(s);
}



void* GetProcedureAddress( TLibHandle lib, string ProcName )
{
  return dlsym( (void*) lib, ProcName.c_str());
}

void* GetProcedureAddress( TLibHandle lib, wstring ProcName )
{
  string s = wstr2str(ProcName);
  return GetProcedureAddress(lib, s);
}



bool UnloadLibrary( TLibHandle lib )
{
  return dlclose( (void*) lib ) == 0;
}


bool FreeLibrary( TLibHandle lib )
{
  return UnloadLibrary( lib );
}


void* GetProcAddress( TLibHandle lib, string ProcName )
{
  return GetProcedureAddress( lib, ProcName );
}

void* GetProcAddress( TLibHandle lib, wstring ProcName )
{
  string s = wstr2str(ProcName);
  return GetProcedureAddress(lib, s);
}



TLibHandle SafeLoadLibrary( string Name )
{
  TLibHandle result = 0;
  WORD W = 0;
//  W = Get8087CW();
  result = LoadLibrary( Name );
//  Set8087CW( W );
  return result;
}

TLibHandle SafeLoadLibrary( wstring Name )
{
   string s = wstr2str(Name);
   return SafeLoadLibrary(s);
}

#else
#error unknown platform
#endif


#ifdef windows
/*****************************************************************************
                    Target dependent WideString stuff
*****************************************************************************/


PtrInt Win32CompareWideString( const std::wstring& S1, const std::wstring& S2 )
{
  PtrInt result = 0;
  SetLastError( 0 );
  result = CompareStringW( LOCALE_USER_DEFAULT, 0, S1.c_str(), S1.length( ), S2.c_str(), S2.length( ) ) - 2;
  if ( GetLastError() != 0 ) 
    RaiseLastOSError();
  return result;
}


PtrInt Win32CompareTextWideString( const std::wstring& S1, const std::wstring& S2 )
{
  PtrInt result = 0;
  SetLastError( 0 );
  result = CompareStringW( LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1.c_str(), S1.length( ), S2.c_str(), S2.length( ) ) - 2;
  if ( GetLastError() != 0 ) 
    RaiseLastOSError();
  return result;
}


String Win32AnsiUpperCase( const String& S )
{
  String result;
  if ( S.length( ) > 0 ) 
  {
    result = S;
//    UniqueString( result );
    CharUpperBuff( (Char*) result.c_str(), result.length( ) );
  }
  else
    result = _T("");
  return result;
}


String Win32AnsiLowerCase( const String& S )
{
  String result;
  if ( S.length( ) > 0 ) 
  {
    result = S;
//    UniqueString( result );
    CharLowerBuff( (Char*) result.c_str(), result.length( ) );
  }
  else
    result = _T("");
  return result;
}


PtrInt Win32AnsiCompareStr( const String& S1, const String& S2 )
{
  return CompareString( LOCALE_USER_DEFAULT, 0, S1.c_str(), S1.length( ), S2.c_str(), S2.length( ) ) - 2;
}


PtrInt Win32AnsiCompareText( const String& S1, const String& S2 )
{
  return CompareString( LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1.c_str(), S1.length( ), S2.c_str(), S2.length( ) ) - 2;
}


PtrInt Win32AnsiStrComp( Char* S1, Char* S2 )
{
  return CompareString( LOCALE_USER_DEFAULT, 0, S1, - 1, S2, - 1 ) - 2;
}


PtrInt Win32AnsiStrIComp( Char* S1, Char* S2 )
{
  return CompareString( LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, - 1, S2, - 1 ) - 2;
}


PtrInt Win32AnsiStrLComp( Char* S1, Char* S2, PtrUInt MaxLen )
{
  return CompareString( LOCALE_USER_DEFAULT, 0, S1, MaxLen, S2, MaxLen ) - 2;
}


PtrInt Win32AnsiStrLIComp( Char* S1, Char* S2, PtrUInt MaxLen )
{
  return CompareString( LOCALE_USER_DEFAULT, NORM_IGNORECASE, S1, MaxLen, S2, MaxLen ) - 2;
}


Char* Win32AnsiStrLower( Char* str )
{
  CharLower( str );
  return str;
}


Char* Win32AnsiStrUpper( Char* str )
{
  CharUpper( str );
  return str;
}

THandle kernel32dll = 0;


void LoadVersionInfo( )  // and getfreespaceex
{
  OSVERSIONINFO VersionInfo;
  kernel32dll = 0;
  //GetDiskFreeSpaceEx = NULL;
  VersionInfo.dwOSVersionInfoSize = sizeof( VersionInfo );
  GetVersionEx( &VersionInfo );
  Win32Platform = VersionInfo.dwPlatformId;
  Win32MajorVersion = VersionInfo.dwMajorVersion;
  Win32MinorVersion = VersionInfo.dwMinorVersion;
  Win32BuildNumber = VersionInfo.dwBuildNumber;
  Move( (void*) VersionInfo.szCSDVersion, &Win32CSDVersion[1], 128 );
  Win32CSDVersion.SetLength((unsigned char)( StrLen( (Char*) ( &VersionInfo.szCSDVersion[0] ) ) ) );
}

void Beep( )
{
  MessageBeep( 0 );
}

String AdjustPath(const String& S)
{
    return S;
}

#elif defined(linux)


void Beep( )
{
  cout << "/a";
}



String ExpandUNCFileName( const String& Filename )
{
  String result;
  result = ExpandFileName( Filename );
  //!! here should follow Code to replace the Drive: part with UNC...
  return result;
}

/*****************************************************************************
                              File functions
*****************************************************************************/


cint fpopen( const char* Path, cint Flags, mode_t Mode )
{
  cint r = open(Path, Flags | O_LARGEFILE, Mode);
  return r==-1 ? -errno : r;
}

cint fpopen( string Path, cint Flags, mode_t Mode )
{
  return fpopen( Path.c_str(), Flags, Mode );
}

cint fpopen( wstring Path, cint Flags, mode_t Mode )
{
  string s = wstr2str(Path);
  return fpopen( s, Flags, Mode );
}

cint fpopen( string Path, cint Flags )
{
  return fpopen( Path, Flags, 438 );
}


cint fpopen( wstring Path, cint Flags )
{
  string s = wstr2str(Path);
  return fpopen( s, Flags, 438 );
}


cint fpclose( cint FD )
{
  cint r = close(FD);
  return r==-1 ? -errno : r;
}


off_t Fplseek( cint FD, off_t Offset, cint Whence )
{
  return lseek(FD, Offset, Whence);
}


ssize_t fpRead( cint FD, char* Buf, size_t nBytes )
{
  ssize_t r = read(FD, Buf, nBytes);
  return r==-1 ? -errno : r;
}


ssize_t fpWrite( cint FD, const char* Buf, size_t nBytes )
{
  ssize_t r = write(FD, Buf, nBytes);
  return r==-1 ? -errno : r;
}


// d2c_in sysfiles too
cint Fpunlink( const char* Path )
{
  cint r = unlink(Path);
  return r==-1 ? -errno : r;
}


cint Fprename( const char* old, const char* NewPath )
{
  cint r = rename(old, NewPath);
  return r==-1 ? -errno : r;
}

typedef struct stat Stat;


cint fpstat( const char* Path, Stat& Buf )
{
  cint r = stat(Path, &Buf);
  return r==-1 ? -errno : r;
}

/******************************************************************************
               --- Directory:Directory related calls ---
******************************************************************************/


void FSplit( const PathStr Path, dirstr& Dir, NameStr& Name, extstr& Ext )
{
  int DotPos = 0, slashPos = 0, i = 0;
  slashPos = 0;
  DotPos = 256;
  i = Path.length( );
  while ( ( i > 0 ) && ( slashPos == 0 ) )
  {
    if ( ( DotPos == 256 ) && ( Path[i - 1] == _T('.') ) )
    {
      DotPos = i;
    }
    if ( Path[i - 1] == _T('/') )
      slashPos = i;
    i--;
  }
  Ext = Path.substr( DotPos - 1, 255 );
  Dir = Path.substr( 1 - 1, slashPos );
  Name = Path.substr( slashPos + 1 - 1, DotPos - slashPos - 1 );
}


uintptr_t FileOpen( const string& Filename, int Mode )
{
  int result = 0;
  int LinuxFlags = 0;
  LinuxFlags = 0;
  switch ( Mode & 3 )
  {
    case 0:
      LinuxFlags = LinuxFlags | O_RDONLY;
    break;
    case 1:
      LinuxFlags = LinuxFlags | O_WRONLY;
    break;
    case 2:
      LinuxFlags = LinuxFlags | O_RDWR;
    break;
  }
  result = fpopen( Filename.c_str(), LinuxFlags );
  //!! we need to set Locking based ON Mode !!
  return result;
}

uintptr_t FileOpen( const wstring& Filename, int Mode )
{
  string s = wstr2str(Filename);
  return FileOpen(s, Mode);
}

THandle FileCreate( const string& Filename )
{
  return fpopen( Filename.c_str(), O_RDWR | O_CREAT | O_TRUNC );
}


THandle FileCreate( const string& Filename, int Mode )
{
  return fpopen( Filename.c_str(), O_RDWR | O_CREAT | O_TRUNC, Mode );
}

THandle FileCreate( const wstring& Filename )
{
  string s = wstr2str(Filename);
  return FileCreate(s);
}


THandle FileCreate( const wstring& Filename, int Mode )
{
  string s = wstr2str(Filename);
  return FileCreate(s, Mode);  
}


int FileRead( int Handle, void* Buffer, int Count )
{
  return fpRead( Handle, (char*) Buffer, Count );
}


int FileWrite( int Handle, const void* Buffer, int Count )
{
  return fpWrite( Handle, (const char*) Buffer, Count );
}

int64_t FileSeek( int Handle, int64_t FOffset, int Origin )
{
  return Fplseek( Handle, FOffset, Origin );
}


void FileClose( int Handle )
{
  fpclose( Handle );
}

cint Fpftruncate( cint FD, off_t flength )
/* see Notes lseek. this one is completely Similar for the Parameter (but
doesn'T have the ReturnValue 64-bit problem)*/
{
  cint r = ftruncate(FD, flength);
  return r==-1 ? -errno : r;
}


int UnixToWinAge( time_t UnixAge )
{
  int result = 0;
  WORD Y = 0, m = 0, D = 0, hh = 0, mm = 0, SS = 0;
  EpochToLocal( UnixAge, Y, m, D, hh, mm, SS );
  result = DateTimeToFileDate( EncodeDate( Y, m, D ) + EncodeTime( hh, mm, SS, 0 ) );
  return result;
}


int FileAge( const string& Filename )
{
  int result = 0;
  Stat info;
  if ( fpstat( (char*) Filename.c_str(), info ) < 0 )
    return - 1;
  else
    result = UnixToWinAge( info.st_mtime );
  return result;
}

int FileAge( const wstring& Filename )
{
  string s = wstr2str(Filename);
  return FileAge(s);
}

cint Fpaccess( const char* pathname, cint AMode )
/*
  test users access Rights ON the specified File.
  Mode is A Mask xosisting of one or more of R_OK, W_OK, X_OK, F_OK.
  r,W,X stAnd for read,write and execute access, simultaneously.
  F_OK checks whether the test would be allowed ON the File.
  i.e. it checks the Search permissions in all Directory components
  of the Path.
  the test is done with the real User-id, instead of the effective.
  if access is denied, or an Error occurred, False is returned.
  if access is granted, True is returned.
  errors other than no access,are reported in unixerror.
*/
{
  cint r = access(pathname, AMode);
  return r==-1 ? -errno : r;
}

String AdjustPath(const String &S)
{
  string res = S;
  for (auto &ch: res)
  {
    if (ch == '\\')
    {
        ch = '/';
    }
  }
  return res;
}

bool FileExists( const string& Filename )
{
  // don'T use Stat. it fails ON files >2 GB.
  // access obeys the same access rules, So the result should be the same.
  return Fpaccess( (char*) AdjustPath(Filename).c_str(), F_OK ) == 0;
}

bool FileExists( const wstring& Filename )
{
  string s = wstr2str(Filename);
  // don'T use Stat. it fails ON files >2 GB.
  // access obeys the same access rules, So the result should be the same.
  return FileExists( s );
}

/*
bool FpS_ISDIR( mode_t m )
{
  return ( ( ( m & S_IFMT ) ) == S_IFDIR );
}*/

bool DirectoryExists( const string& Directory )
{
  Stat info;
  return ( fpstat( (char*) AdjustPath(Directory).c_str(), info ) >= 0 ) && FpS_ISDIR( info.st_mode );
}

bool DirectoryExists( const wstring& Directory )
{
  Stat info;
  string s = wstr2str(Directory);
  return DirectoryExists(s);
}


bool FpS_ISCHR( mode_t m )
{
  return ( ( ( m & S_IFMT ) ) == S_IFCHR );
}


bool FpS_ISBLK( mode_t m )
{
  return ( ( ( m & S_IFMT ) ) == S_IFBLK );
}


bool FpS_ISREG( mode_t m )
{
  return ( ( ( m & S_IFMT ) ) == S_IFREG );
}


bool FpS_ISFIFO( mode_t m )
{
  return ( ( ( m & S_IFMT ) ) == S_IFIFO );
}


bool FPS_ISLNK( mode_t m )
{
  return ( ( ( m & S_IFMT ) ) == S_IFLNK );
}


bool FPS_ISSOCK( mode_t m )
{
  return ( ( ( m & S_IFMT ) ) == S_IFSOCK );
}

/*
bool WIFEXITED( cint Status )
{
  bool result = false;
  result = ( ( Status & 0x7F ) ) == 0;
  return result;
}


cint WEXITSTATUS( cint Status )
{
  cint result = 0;
  result = ( Status & 0xFF00 ) >> 8;
  return result;
}


cint WSTOPSIG( cint Status )
{
  cint result = 0;
  result = ( Status & 0xFF00 ) >> 8;
  return result;
}


const int wstopped = 127;


bool WIFSIGNALED( cint Status )
{
  bool result = false;
  result = ( ( ( Status & 0xFF ) ) != wstopped ) && ( ( ( Status & 127 ) ) != 0 );
  return result;
}


cint WTERMSIG( cint Status )
{
  cint result = 0;
  result = ((cint) ( Status & 127 ) );
  return result;
}    
*/
int LinuxToWinAttr( char* fn, const Stat& info )
{
  int result = 0;
  string FNL;
  Stat LinkInfo;
  result = faArchive;
  if ( FpS_ISDIR( info.st_mode ) )
    result = result | faDirectory;
  if ( ( fn[0] == '.' ) && ( ! ( fn[1] == '\'' || fn[1] == '"') ) )
    result = result | faHidden;
  if ( ( ( info.st_mode & S_IWUSR ) ) == 0 )
    result = result | faReadOnly;
  if ( FPS_ISSOCK( info.st_mode ) || FpS_ISBLK( info.st_mode ) || FpS_ISCHR( info.st_mode ) || FpS_ISFIFO( info.st_mode ) )
    result = result | faSysFile;
  if ( FPS_ISLNK( info.st_mode ) )
  {
    result = result | faSymLink;
    // Windows reports if the link Points to A Directory.
    FNL = StrPas( fn );
    if ( ( fpstat( (char*) FNL.c_str(), LinkInfo ) >= 0 ) && FpS_ISDIR( LinkInfo.st_mode ) )
      result = result | faDirectory;
  }
  return result;
}


bool DoFNMatch( int i, int j, const string& pattern, const string& Name, bool& xResult, int& LenPat, int& LenName )
{
  bool result = false;
  bool found = false;
  found = true;
  while ( found && ( i <= LenPat ) )
  {
    switch ( pattern[i - 1] )
    {
      case '?':
        found = ( j <= LenName );
      break;
      case '*':
      {
            /*find the Next character in pattern, different of ? and **/
        while ( found )
        {
          i++;
          if ( i > LenPat )
            break;
          switch ( pattern[i - 1] )
          {
            case '*':
            break;
            case '?':
            {
              if ( j > LenName )
              {
                result = false;
                return result;
              }
              j++;
            }
            break;
          default:
            found = false;
          }
        }
        Assert( ( i > LenPat ) || ( ( pattern[i - 1] != '*' ) && ( pattern[i - 1] != '?' ) ) );
            /*Now, find in Name the character which i Points to, if the * or ?
             wasn'T the Last character in the pattern, else, use UP all the
             Chars in Name*/
        found = false;
        if ( i <= LenPat )
        {
          do
          {
                  /*find A Letter (not only First !) which maches pattern[i]*/
            while ( ( j <= LenName ) && ( Name[j - 1] != pattern[i - 1] ) )
              j++;
            if ( j < LenName )
            {
              if ( DoFNMatch( i + 1, j + 1, pattern, Name, xResult, LenPat, LenName ) )
              {
                i = LenPat;
                j = LenName;/*we can Stop*/
                found = true;
                break;
              }
              else
                j++;/*we didn'T find one, need to look further*/
            }
            else
              if ( j == LenName )
              {
                found = true;
                break;
              }
                  /* this 'until' condition must be j>LenName, not j>=LenName.
                    that's because when we 'need to look further' and
                    j = LenName then loop must not Terminate. */
          }
          while ( ! ( j > LenName ) );
        }
        else
        {
          j = LenName;/*we can Stop*/
          found = true;
        }
      }
      break;
    default: /*not A WildCard character in pattern*/
      found = ( j <= LenName ) && ( pattern[i - 1] == Name[j - 1] );
    }
    i++;
    j++;
  }
  result = found && ( j > LenName );
  return result;
}


bool FNMatch( const string& pattern, const string& Name )
{
  bool result = false;
  int LenPat = 0, LenName = 0; /*start FNMatch*/
  LenPat = pattern.length( );
  LenName = Name.length( );
  return DoFNMatch( 1, 1, pattern, Name, result, LenPat, LenName );
}



struct TUnixFindData {
  int NamePos;     /*to track which Search this is*/
  void* DirPtr;     /*Directory Pointer for Reading Directory*/
  string SearchSpec;
  unsigned char SearchType;        /*0=normal, 1=Open will Close, 2=only 1 File*/
  unsigned char SearchAttr;        /*attribute we are searching for*/
};


typedef TUnixFindData* PUnixFindData;


int CurrSearchNum = 0;


void FindClose( TSearchRec& F )
{
  PUnixFindData UnixFindData = NULL;
  UnixFindData = ((PUnixFindData) F.FindHandle );
  if ( UnixFindData == NULL )
    return;
  if ( UnixFindData->SearchType == 0 )
  {
    if ( UnixFindData->DirPtr != NULL )
      Fpclosedir( ((Dir*) UnixFindData->DirPtr ) );
  }
  delete UnixFindData;
  F.FindHandle = NULL;
}

typedef Stat* Pstat;

cint fpLstat( const char* Path, Pstat info )
/*
  get all information ON A link (the link itself), and return it in info.
*/
{
  cint r = lstat(Path, info);
  return r==-1 ? -errno : r;
}

bool FindGetFileInfo( const string& S, TSearchRec& F )
{
  bool result = false;
  Stat st;
  int WinAttr = 0;
  result = false;
  if (( F.FindHandle != NULL ) && ( ( ( ( ( ((PUnixFindData) F.FindHandle )->SearchAttr ) ) & faSymLink ) ) > 0 ) )
    result = ( fpLstat( (char*) S.c_str(), &st ) == 0 );
  else
	result = ( fpstat( (char*) S.c_str(), st ) == 0 );
  if ( ! &FindGetFileInfo )
	return result;
  WinAttr = LinuxToWinAttr( (char*) S.c_str(), st );
  if ( ( F.FindHandle == NULL ) || ( ( ( WinAttr & ~ ( ((PUnixFindData) F.FindHandle )->SearchAttr ) ) ) == 0 ) )
  {
#ifdef _WIDESTRING
    F.Name = ExtractFileName( str2wstr(S) );
#else
    F.Name = ExtractFileName( S );
#endif
    F.Attr = WinAttr;
    F.Size = st.st_size;
    F.Mode = st.st_mode;
    F.Time = UnixToWinAge( st.st_mtime );
    result = true;
  }
  return result;
}


int FindNext( TSearchRec& Rslt )
/*
  re-opens Dir if not Already in array and calls FindGetFileInfo
*/
{
  int result = 0;
  string DirName;
  string FName, SName;
  bool found = false, Finished = false;
  dirent* P = NULL;
  PUnixFindData UnixFindData = NULL;
  result = - 1;
  UnixFindData = ((PUnixFindData) Rslt.FindHandle );
  if ( UnixFindData == NULL )
    return result;
  if ( ( UnixFindData->SearchType == 0 ) && ( UnixFindData->DirPtr == NULL ) )
  {
    if ( UnixFindData->NamePos == 0 )
      DirName = "./";
    else
      DirName = UnixFindData->SearchSpec.substr( 1 - 1, UnixFindData->NamePos );
    UnixFindData->DirPtr = Fpopendir( DirName.c_str() );
  }
  SName = UnixFindData->SearchSpec.substr( UnixFindData->NamePos + 1 - 1, UnixFindData->SearchSpec.length( ) );
  found = false;
  Finished = ( UnixFindData->DirPtr == NULL );
  while ( ! Finished )
  {
    P = Fpreaddir( ((Dir*) UnixFindData->DirPtr ) );
    if ( P == NULL )
      FName = "";
    else
      FName = P->d_name;
    if ( FName.empty())
      Finished = true;
    else
    {
      if ( FNMatch( SName, FName ) )
      {
        found = FindGetFileInfo( UnixFindData->SearchSpec.substr( 1 - 1, UnixFindData->NamePos ) + FName, Rslt );
        if ( found )
        {
          result = 0;
          return result;
        }
      }
    }
  }
  return result;
}


int FindFirst( const String& Path, int Attr, TSearchRec& Rslt )
/*
  opens Dir and calls FindNext if needed.
*/
{
  int result = 0;
  PUnixFindData UnixFindData = NULL;
  result = - 1;
  //FillChar( &Rslt, sizeof( Rslt ), 0 );
  Rslt.Time = 0;
  Rslt.Size = 0;
  Rslt.Attr = 0;
  Rslt.Name.clear();
  Rslt.ExcludeAttr = 0; 
  #ifdef unix
  Rslt.FindHandle = 0;
  Rslt.Mode = 0;
  Rslt.PathOnly.clear(); 
  #else
  Rslt.FindHandle = 0;   
  #endif

  if ( Path.empty())
    return result;
  /*wildcards?*/
  if ( ( Pos( _T("?"), Path ) == 0 ) && ( Pos( _T("*"), Path ) == 0 ) )
  {
#ifdef _WIDESTRING
    if ( FindGetFileInfo( wstr2str(Path), Rslt ) )
#else
    if ( FindGetFileInfo( Path, Rslt ) )
#endif
      result = 0;
  }
  else
  {
     /* allocate UnixFindData */
    UnixFindData = new TUnixFindData;
    //FillChar( UnixFindData, sizeof( *UnixFindData ), 0 );
    UnixFindData->NamePos = 0;
    UnixFindData->DirPtr = NULL;
    UnixFindData->SearchType = '\0';   
    UnixFindData->SearchAttr = '\0'; 
  
    Rslt.FindHandle = UnixFindData;
     /*Create info*/
#ifdef _WIDESTRING     
    UnixFindData->SearchSpec = wstr2str(Path);
#else
    UnixFindData->SearchSpec = Path;
#endif
     /*we always also Search for ReadOnly and archive, regardless of Attr:*/
    UnixFindData->SearchAttr = Attr | faArchive | faReadOnly;
    UnixFindData->NamePos = UnixFindData->SearchSpec.length( );
    while ( ( UnixFindData->NamePos > 0 ) && ( UnixFindData->SearchSpec[UnixFindData->NamePos - 1] != '/' ) )
      UnixFindData->NamePos--;
    result = FindNext( Rslt );
  }
  return result;
}

cint fpfstat( cint FD, Stat& sb )
{
  cint r = fstat(FD, &sb);
  return r==-1 ? -errno : r;
}


int FileGetDate( int Handle )
{
  int result = 0;
  Stat info;
  if ( ( fpfstat( Handle, info ) ) < 0 )
    result = - 1;
  else
    result = info.st_mtime;
  return result;
}


int FileSetDate( int Handle, int Age )
{
  int result = 0;
  // impossible under LINUX from FileHandle !!
  result = - 1;
  return result;
}


int FileGetAttr( const string& Filename )
{
  int result = 0;
  Stat info;
  if ( fpstat( (char*) Filename.c_str(), info ) < 0 )
    result = - 1;
  else
  {
    string Sysutils__28( ExtractFileName( Filename ) );
    result = LinuxToWinAttr( (char*) Sysutils__28.c_str(), info );
  }
  return result;
}

int FileGetAttr( const wstring& Filename )
{
   string s = wstr2str(Filename);
   return FileGetAttr(s);
}



int FileSetAttr( const string& Filename, int Attr )
{
  int result = 0;
  result = - 1;
  return result;
}

int FileSetAttr( const wstring& Filename, int Attr )
{
  string s = wstr2str(Filename);
  return FileSetAttr(s, Attr);
}


bool DeleteFile( const string& Filename )
{
  return Fpunlink( Filename.c_str()) >= 0;
}

bool DeleteFile( const wstring& Filename )
{
  string s = wstr2str(Filename); 
  return DeleteFile(s);
}

bool RenameFile( const string& OldName, const string& newName )
{
  return Fprename( OldName.c_str(), newName.c_str()) >= 0;
}

bool RenameFile( const wstring& OldName, const wstring& newName )
{
  string sOld = wstr2str(OldName); 
  string sNew = wstr2str(newName); 
  return RenameFile( sOld, sNew);
}


bool FileIsReadOnly( const string& Filename )
{
  return Fpaccess( Filename.c_str(), W_OK ) != 0;
}

bool FileIsReadOnly( const wstring& Filename )
{
  string s = wstr2str(Filename); 
  return FileIsReadOnly(s);
}

cint FpUtime( const char* Path, utimbuf* Times )
{
  cint r = utime(Path, Times);
  return r==-1 ? -errno : r;
}

int FileSetDate( const string& Filename, int Age )
{
  int result = 0;
  utimbuf T;
  result = 0;
  T.actime = Age;
  T.modtime = Age;
  if ( FpUtime( Filename.c_str(), &T ) == - 1 )
    result = errno; //fpgeterrno();
  return result;
}

int FileSetDate( const wstring& Filename, int Age )
{
  string s = wstr2str(Filename);
  return FileSetDate(s, Age);
}
/*****************************************************************************
                              disk functions
*****************************************************************************/

/*
  the DiskFree and DiskSize functions need A File ON the specified Drive, since this
  is required for the statfs System call.
  These filenames are set in DriveStr[0..26], and have been Preset to :
   0 - '.'      (Default Drive - Hence current Dir is ok.)
   1 - '/fd0/.'  (floppy Drive 1 - should be adapted to Local System )
   2 - '/fd1/.'  (floppy Drive 2 - should be adapted to Local System )
   3 - '/'       (C: equivalent of DOS is the root partition)
   4..26          (can be set by you're own Applications)
  ! use AddDisk() to Add New drives !
  they both return -1 when A failure occurs.
*/


const char* FixDriveStr [ 4/*# range 0..3*/ ];
void FixDriveStrInit( )
{
  FixDriveStr[0] = ".";
  FixDriveStr[1] = "/fd0/.";
  FixDriveStr[2] = "/fd1/.";
  FixDriveStr[3] = "/.";
}


unsigned char drives = '\0';
char* DriveStr[ 23/*# range 4..26*/ ];


unsignedchar AddDisk( const string& Path )
{
  if ( ! ( DriveStr[drives - 4] == NULL ) )
    FreeMem( DriveStr[drives - 4], StrLen( DriveStr[drives - 4] ) + 1 );
  GetMem( DriveStr[drives - 4], Path.length( ) + 1 );
  strcpy(DriveStr[drives - 4], Path.c_str());
  drives++;
  if ( drives > 26 )
    drives = 4;
  return drives;
}

                    
int64_t DiskFree( unsignedchar Drive )
{
  int64_t result = 0;
  struct statfs fs;
  if ( ( Drive >= 0 && Drive <= 3  && FixDriveStr[Drive] != NULL && statfs( FixDriveStr[Drive], &fs ) != - 1 )  || 
     ( ( Drive <=  26 ) && DriveStr[Drive - 4] != NULL  && statfs( DriveStr[Drive - 4], &fs ) != - 1 ) ) 
    result = ((int64_t) fs.f_bavail ) * ((int64_t) fs.f_bsize );
  else
    result = - 1;
  return result;
}


int64_t DiskSize( unsignedchar Drive )
{
  int64_t result = 0;
  struct statfs fs;
  if ( ( Drive >= 0 && Drive <= 3 && FixDriveStr[Drive] != NULL && statfs( FixDriveStr[Drive], &fs ) != - 1 )  || 
     ( ( Drive <= 26 ) && DriveStr[Drive - 4] != NULL  && statfs( DriveStr[Drive - 4], &fs ) != - 1 ) ) 
    result = ((int64_t) fs.f_blocks ) * ((int64_t) fs.f_bsize );
  else
    result = - 1;
  return result;
}


String GetCurrentDir( )
{
  String result;
  GetDir( 0, result );
  return result;
}


bool SetCurrentDir( const String& newdir )
{
  bool result = false;
  ChDir( newdir );
  result = ( IOResult() == 0 );
  return result;
}


bool CreateDir( const String& newdir )
{
  bool result = false;
  MkDir( newdir );
  result = ( IOResult() == 0 );
  return result;
}


bool RemoveDir( const String& Dir )
{
  bool result = false;
  RmDir( Dir );
  result = ( IOResult() == 0 );
  return result;
}

// clocale

#define clocale__0 ( TSet < unsigned char, 0, 255 >() \
                     << (unsigned char) ( _T('E') ) << (unsigned char) ( _T('O') ) )
#define clocale__1 ( TSet < unsigned char, 0, 255 >() \
                     << (unsigned char) ( _T('E') ) << (unsigned char) ( _T('O') ) )
#define clocale__2 ( TSet < int, 0, 255 >() \
                     << int ( 0 ) << int ( 1 ) << int ( 2 ) << int ( 3 ) << int ( 4 ) )
#define clocale__3 ( TSet < int, 0, 255 >() \
                     << int ( 0 ) << int ( 1 ) )
#define clocale__4 ( TSet < int, 0, 255 >() \
                     << int ( 0 ) << int ( 1 ) )



const int ABDAY_1 = ( __LC_TIME << 16 );
const int DAY_1 = ( ABDAY_1 ) + 7;
const int ABMON_1 = ( ABDAY_1 ) + 14;
const int MON_1 = ( ABDAY_1 ) + 26;
const int AM_STR = ( ABDAY_1 ) + 38;
const int PM_STR = ( ABDAY_1 ) + 39;
const int D_T_FMT = ( ABDAY_1 ) + 40;
const int D_FMT = ( ABDAY_1 ) + 41;
const int T_FMT = ( ABDAY_1 ) + 42;
const int T_FMT_AMPM = ( ABDAY_1 ) + 43;
const int __DECIMAL_POINT = ( __LC_NUMERIC << 16 );
const int RADIXCHAR = __DECIMAL_POINT;
const int __THOUSANDS_SEP = ( __DECIMAL_POINT ) + 1;
const int __INT_CURR_SYMBOL = ( __LC_MONETARY << 16 );
const int __CURRENCY_SYMBOL = ( __INT_CURR_SYMBOL ) + 1;
const int __MON_DECIMAL_POINT = ( __INT_CURR_SYMBOL ) + 2;
const int __MON_THOUSANDS_SEP = ( __INT_CURR_SYMBOL ) + 3;
const int __MON_GROUPING = ( __INT_CURR_SYMBOL ) + 4;
const int __POSITIVE_SIGN = ( __INT_CURR_SYMBOL ) + 5;
const int __NEGATIVE_SIGN = ( __INT_CURR_SYMBOL ) + 6;
const int __INT_FRAC_DIGITS = ( __INT_CURR_SYMBOL ) + 7;
const int __FRAC_DIGITS = ( __INT_CURR_SYMBOL ) + 8;
const int __P_CS_PRECEDES = ( __INT_CURR_SYMBOL ) + 9;
const int __P_SEP_BY_SPACE = ( __INT_CURR_SYMBOL ) + 10;
const int __N_CS_PRECEDES = ( __INT_CURR_SYMBOL ) + 11;
const int __N_SEP_BY_SPACE = ( __INT_CURR_SYMBOL ) + 12;
const int __P_SIGN_POSN = ( __INT_CURR_SYMBOL ) + 13;
const int __N_SIGN_POSN = ( __INT_CURR_SYMBOL ) + 14;
const int _NL_MONETARY_CRNCYSTR = ( __INT_CURR_SYMBOL ) + 15;


String GetLocaleStr( cint item )
{
#ifdef _WIDESTRING
  return str2wstr(nl_langinfo( item ));
#else
  return nl_langinfo( item );
#endif
}


Char GetLocaleChar( cint item )
{
  return nl_langinfo( item )[0]; // b?
} 


Char FindSeparator( const String& S, Char def )
{
  Char result = _T('\0');
  int i = 0, l = 0;
  result = def;
  i = Pos( _T("%"), S );
  if ( i == 0 )
    return result;
  l = S.length( );
  i++;
  if ( ( i <= l ) && ( clocale__0.Contains(S[i - 1] ) ) ) //possible modifier
    i++;
  i++;
  if ( i <= l )
    result = S[i - 1];
  return result;
}


String TransformFormatStr( const String& S )
{
  String result;
  int i = 0, l = 0;
  bool Clock12 = false;
  Clock12 = false; // should ampm get appended?
  result = _T("");
  i = 1;
  l = S.length( );
  while ( i <= l )
  {
    if ( S[i - 1] == _T('%') )
    {
      i++;
      if ( ( i <= l ) && ( clocale__1.Contains(S[i - 1] ) ) ) //IGNORE modifier
        i++;
      if ( i > l )
        return result;
      switch ( S[i - 1] )
      {
        case _T('a'):
          result += _T("ddd");
        break;
        case _T('A'):
          result += _T("dddd");
        break;
        case _T('b'):
          result += _T("mmm");
        break;
        case _T('B'):
          result += _T("mmmm");
        break;
        case _T('c'):
          result += _T("c");
        break;
          //'C'):

        case _T('d'):
          result += _T("dd");
        break;
        case _T('D'):
          result += _T("mm\"/\"dd\"/\"yy");
        break;
        case _T('e'):
          result += _T("d");
        break;
        case _T('F'):
          result += _T("yyyy-mm-dd");
        break;
        case _T('g'):
          result += _T("yy");
        break;
        case _T('G'):
          result += _T("yyyy");
        break;
        case _T('h'):
          result += _T("mmm");
        break;
        case _T('H'):
          result += _T("hh");
        break;
        case _T('I'):
        {
          result += _T("hh");
          Clock12 = true;
        }
        break;
          //'j'):

        case _T('k'):
          result += _T("h");
        break;
        case _T('l'):
        {
          result += _T("h");
          Clock12 = true;
        }
        break;
        case _T('m'):
          result += _T("mm");
        break;
        case _T('M'):
          result += _T("nn");
        break;
        case _T('n'):
          result += sLineBreak;
        break;
        case _T('p'): case _T('P'):
        {
          result += _T("ampm");
          Clock12 = false;
        }
        break;
        case _T('r'):
        {
          result += _T("hh:nn:ss");
          Clock12 = true;
        }
        break;
        case _T('R'):
          result += _T("hh:nn");
        break;
          //'S'):

        case _T('S'):
          result += _T("ss");
        break;
        case _T('t'):
          result += _T("\x09");
        break;
        case _T('T'):
          result += _T("hh:nn:ss");
        break;
          //'U'):
          //'U'):
          //'V'):
          //'W'):
          //'W'):

        case _T('x'):
          result += _T("ddddd");
        break;
        case _T('X'):
          result += _T("t");
        break;
        case _T('y'):
          result += _T("yy");
        break;
        case _T('Y'):
          result += _T("yyyy");
        break;
          //'Z'):
          //'Z'):

        case _T('%'):
          result += _T("%");
        break;
      }
    }
    else
      result += S[i - 1];
    i++;
  }
  i = result.length( );
  if ( Clock12 && ( i > 0 ) )
  {
    if ( result[i] != _T(' ') )
      result += _T(" ");
    result += _T("ampm");
  }
  return result;
}


void GetFormatSettings( )
{
  //                      sign  prec  sep
  unsigned char NegFormatsTable [ 5/*# range 0..4*/ ][ 2/*# range 0..1*/ ][ 2/*# range 0..1*/ ];
  NegFormatsTable[0][0][0] = 4;
  NegFormatsTable[0][0][1] = 15;
  NegFormatsTable[0][1][0] = 0;
  NegFormatsTable[0][1][1] = 14;
  NegFormatsTable[1][0][0] = 5;
  NegFormatsTable[1][0][1] = 8;
  NegFormatsTable[1][1][0] = 1;
  NegFormatsTable[1][1][1] = 9;
  NegFormatsTable[2][0][0] = 7;
  NegFormatsTable[2][0][1] = 10;
  NegFormatsTable[2][1][0] = 3;
  NegFormatsTable[2][1][1] = 11;
  NegFormatsTable[3][0][0] = 6;
  NegFormatsTable[3][0][1] = 13;
  NegFormatsTable[3][1][0] = 1;
  NegFormatsTable[3][1][1] = 9;
  NegFormatsTable[4][0][0] = 7;
  NegFormatsTable[4][0][1] = 10;
  NegFormatsTable[4][1][0] = 2;
  NegFormatsTable[4][1][1] = 12;  //the sign String immediately follows the currency_symbol

  int i = 0;
  unsigned char prec = '\0', sep = '\0', signp = '\0';
  setlocale( __LC_ALL, "" );
  for ( i = 1; i <= 12; i++)
  {
    ShortMonthNames[i - 1] = GetLocaleStr( ABMON_1 + i - 1 );
    LongMonthNames[i - 1] = GetLocaleStr( MON_1 + i - 1 );
  }
  for ( i = 1; i <= 7; i++)
  {
    ShortDayNames[i - 1] = GetLocaleStr( ABDAY_1 + i - 1 );
    LongDayNames[i - 1] = GetLocaleStr( DAY_1 + i - 1 );
  }
  //Date stuff
  ShortDateFormat = GetLocaleStr( D_FMT );
  DateSeparator = FindSeparator( ShortDateFormat, DateSeparator );
  ShortDateFormat = TransformFormatStr( ShortDateFormat );
  LongDateFormat = GetLocaleStr( D_T_FMT );
  LongDateFormat = TransformFormatStr( LongDateFormat );
  //Time stuff
  TimeAMString = GetLocaleStr( AM_STR );
  TimePMString = GetLocaleStr( PM_STR );
  ShortTimeFormat = GetLocaleStr( T_FMT );
  TimeSeparator = FindSeparator( ShortTimeFormat, TimeSeparator );
  ShortTimeFormat = TransformFormatStr( ShortTimeFormat );
  LongTimeFormat = GetLocaleStr( T_FMT_AMPM );
  LongTimeFormat = TransformFormatStr( LongTimeFormat );   
   //Currency stuff
  CurrencyString = GetLocaleStr( _NL_MONETARY_CRNCYSTR );
  CurrencyString = CurrencyString.substr( 2 - 1, CurrencyString.length( ) );
  CurrencyDecimals = StrToIntDef( GetLocaleStr( __FRAC_DIGITS ), CurrencyDecimals );
  prec = ((unsigned char) GetLocaleChar( __P_CS_PRECEDES ) );
  sep = ((unsigned char) GetLocaleChar( __P_SEP_BY_SPACE ) );
  if ( ( prec <= 1 ) && ( sep <= 1 ) )
    CurrencyFormat = ((unsigned char) ! ((bool) prec ) ) + ( sep << 1 );
  prec = ((unsigned char) GetLocaleChar( __N_CS_PRECEDES ) );
  sep = ((unsigned char) GetLocaleChar( __N_SEP_BY_SPACE ) );
  signp = ((unsigned char) GetLocaleChar( __N_SIGN_POSN ) );
  if ( ( clocale__2.Contains(signp ) ) && ( clocale__3.Contains(prec ) ) && ( clocale__4.Contains(sep ) ) )
    NegCurrFormat = NegFormatsTable[signp][ prec][ sep];
  //number stuff
  ThousandSeparator = GetLocaleChar( __THOUSANDS_SEP );
  DecimalSeparator = GetLocaleChar( RADIXCHAR );
}

// clocale end

void InitAnsi( )
{
  int i = 0;
  /*  Fill Table Entries 0 to 127  */
  for ( i = 0; i <= 96; i++)
    UpperCaseTable[i] = Char( i );
  for ( i = 97; i <= 122; i++)
    UpperCaseTable[i] = Char( i - 32 );
  for ( i = 123; i <= 191; i++)
    UpperCaseTable[i] = Char( i );
  Move( &CPISO88591UCT, &UpperCaseTable[192], sizeof( CPISO88591UCT ) );
  for ( i = 0; i <= 64; i++)
    LowerCaseTable[i] = Char( i );
  for ( i = 65; i <= 90; i++)
    LowerCaseTable[i] = Char( i + 32 );
  for ( i = 91; i <= 191; i++)
    LowerCaseTable[i] = Char( i );
  Move( &CPISO88591LCT, &LowerCaseTable[192], sizeof( CPISO88591UCT ) );
}

void InitInternational( )
{
  InitInternationalGeneric();
  InitAnsi();
  GetFormatSettings();
}
#else
#error unknown platform
#endif



void Sysutils_initialization()
{

  InitInternational();    /* Initialize internationalization settings */

#ifdef windows
  LoadVersionInfo();
#elif defined(linux)
  FixDriveStrInit();
#endif
}

void Sysutils_finalization()
{
  ;
}

class Sysutils_unit
{
public:
Sysutils_unit()
{
  DefaultFormatSettingsInit();
  Sysutils_initialization();
}
~Sysutils_unit(){ Sysutils_finalization(); }
};
Sysutils_unit _Sysutils_unit;

}  // namespace Sysutils


