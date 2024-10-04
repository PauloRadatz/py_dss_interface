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


#include "d2c_sysdate.h"
#include "d2c_system.h"
#include "d2c_sysmath.h"
#include "Sysutils.h"
#include "d2c_sysexcept.h"
#include "d2c_sysconst.h"
#include "d2c_sysstring.h"
#include <stdio.h>

#ifdef linux
#include <sys/time.h>
#include <time.h>
#endif

#undef max

using namespace System;
using namespace std;


typedef WORD *PDayTable;
typedef WORD TDayTable [ 12/*# range 1..12*/ ];


//const int64_t UnixDateDelta = Trunc( UnixEpoch ); //25569


   /* True=Leapyear */

extern TDayTable MonthDays [ 2 /*# Boolean */ ];


TDayTable MonthDays [ 2 /*# Boolean */ ];
void MonthDaysInit( )
{
  MonthDays[0][0] = 31;
  MonthDays[0][1] = 28;
  MonthDays[0][2] = 31;
  MonthDays[0][3] = 30;
  MonthDays[0][4] = 31;
  MonthDays[0][5] = 30;
  MonthDays[0][6] = 31;
  MonthDays[0][7] = 31;
  MonthDays[0][8] = 30;
  MonthDays[0][9] = 31;
  MonthDays[0][10] = 30;
  MonthDays[0][11] = 31;
  MonthDays[1][0] = 31;
  MonthDays[1][1] = 29;
  MonthDays[1][2] = 31;
  MonthDays[1][3] = 30;
  MonthDays[1][4] = 31;
  MonthDays[1][5] = 30;
  MonthDays[1][6] = 31;
  MonthDays[1][7] = 31;
  MonthDays[1][8] = 30;
  MonthDays[1][9] = 31;
  MonthDays[1][10] = 30;
  MonthDays[1][11] = 31;
}

TDateTime MinDateTime = - 693593.0;     /* 01/01/0001 12:00:00.000 AM */
TDateTime MaxDateTime = 2958465.99999; /* 12/31/9999 11:59:59.999 PM */


string DateErrorString( const char* xpFormat, const WORD x1, const WORD x2, const WORD x3, const WORD x4 )
{
  char buf[1000];
  if(x4 != WORD(-1))
    sprintf(buf, xpFormat, x1, x2, x3, x4);
  else
  if(x3 != WORD(-1))
    sprintf(buf, xpFormat, x1, x2, x3);
  else
    sprintf(buf, xpFormat, x1, x2);
  return buf;
}

wstring DateErrorString( const wchar_t* xpFormat, const WORD x1, const WORD x2, const WORD x3, const WORD x4 )
{
  wchar_t buf[1000];
  if(x4 != WORD(-1))
    swprintf(buf, 999, xpFormat, x1, x2, x3, x4);
  else
  if(x3 != WORD(-1))
    swprintf(buf, 999, xpFormat, x1, x2, x3);
  else
    swprintf(buf, 999, xpFormat, x1, x2);
  return buf;
}


/*==============================================================================*/
/*   Internal functions                                                         */
/*==============================================================================*/

#define Sysutils__19 ( TSet < int, 0, 255 >() \
                     << int ( 1 ) << int ( 2 ) << int ( 3 ) << int ( 4 ) << int ( 5 ) << int ( 6 ) << int ( 7 ) << int ( 8 ) << int ( 9 ) << int ( 10 ) \
                     << int ( 11 ) << int ( 12 ) )
#define Sysutils__20 ( TSet < UChar, 0, 255 >() \
                     << _T('0') << _T('1') << _T('2') << _T('3') << _T('4') << _T('5') << _T('6') << _T('7') << _T('8') << _T('9') )
#define Sysutils__21 ( TSet < UChar, 0, 255 >() \
                     << _T('0') << _T('1') << _T('2') << _T('3') << _T('4') << _T('5') << _T('6') << _T('7') << _T('8') << _T('9') )
#define Sysutils__22 ( TSet < UChar, 0, 255 >() \
                     << _T('0') << _T('1') << _T('2') << _T('3') << _T('4') << _T('5') << _T('6') << _T('7') << _T('8') << _T('9') )
#define Sysutils__23 ( TSet < UChar, 0, 255 >() \
                     << _T('0') << _T('1') << _T('2') << _T('3') << _T('4') << _T('5') << _T('6') << _T('7') << _T('8') << _T('9') )
#define Sysutils__24 ( TSet < UChar, 0, 255 >() \
                     << _T('0') << _T('1') << _T('2') << _T('3') << _T('4') << _T('5') << _T('6') << _T('7') << _T('8') << _T('9') )
#define Sysutils__25 ( TSet < UChar, 0, 255 >() \
                     << _T('a') << _T('A') )
#define Sysutils__26 ( TSet < UChar, 0, 255 >() \
                     << _T('p') << _T('P') )
#define Sysutils__27 ( TSet < UChar, 0, 255 >() \
                     << _T('"') << _T('\'') )


const int LFAI = numeric_limits< WORD >::max(); // less typing, readable Code

String DoField( WORD arg, WORD def, String Unknown, const WORD& AYear, const WORD& AMonth, const WORD& ADay, const WORD& AHour, const WORD& AMinute, const WORD& ASecond, const WORD& AMilliSecond, const TDateTime& ABaseDate )
{
  String result;
  if ( arg != LFAI )
//    result = Format( _T("%.*d"), ARRAYOFCONST(( Unknown.length(), arg )) );
    result = IntToStr(arg);
  else
    if ( (double) ABaseDate == 0 )
      result = Unknown;
    else
     // result = Format( _T("%.*d"), ARRAYOFCONST(( Unknown.length(), arg )) );
      result = IntToStr(arg);
  return result;
}

void InvalidDateTimeError( const WORD AYear, const WORD AMonth, const WORD ADay, const WORD AHour, const WORD AMinute, const WORD ASecond, const WORD AMilliSecond, const TDateTime ABaseDate )
{
  WORD Y = 0, m = 0, D = 0, h = 0, n = 0, S = 0, MS = 0;
  String Msg;
  DecodeDateTime( ABaseDate, Y, m, D, h, n, S, MS );
  Msg = DoField( AYear, Y, _T("????"), AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, ABaseDate );
  Msg = Msg + DateSeparator + DoField( AMonth, m, _T("??"), AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, ABaseDate );
  Msg = Msg + DateSeparator + DoField( ADay, D, _T("??"), AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, ABaseDate );
  Msg = Msg + _T(" ") + DoField( AHour, h, _T("??"), AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, ABaseDate );
  Msg = Msg + TimeSeparator + DoField( AMinute, n, _T("??"), AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, ABaseDate );
  Msg = Msg + TimeSeparator + DoField( ASecond, S, _T("??"), AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, ABaseDate );
  Msg = Msg + DecimalSeparator + DoField( AMilliSecond, MS, _T("???"), AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, ABaseDate );
  throw EConvertError( ErrorString(_T(SysConst_SErrInvalidTimeStamp),  Msg ) );
}


void InvalidDateTimeError( const WORD AYear, const WORD AMonth, const WORD ADay, const WORD AHour, const WORD AMinute, const WORD ASecond, const WORD AMilliSecond ) // const ABaseDate: TDateTime = 0

{
  InvalidDateTimeError( AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, 0 );
}


void InvalidDateWeekError( const WORD AYear, const WORD AWeekOfYear, const WORD ADayOfWeek )
{
//  throw EConvertError( _T(SysConst_SErrInvalidDateWeek), ARRAYOFCONST(( AYear, AWeekOfYear, ADayOfWeek )) );
  throw EConvertError( DateErrorString(_T(SysConst_SErrInvalidDateWeek), AYear, AWeekOfYear, ADayOfWeek, -1 ) );
}


void InvalidDateDayError( const WORD AYear, const WORD ADayOfYear )
{
  throw EConvertError( DateErrorString(_T(SysConst_SErrInvalidDayOfYear), AYear, ADayOfYear, -1, -1 ) );
}


void InvalidDateMonthWeekError( const WORD AYear, const WORD AMonth, const WORD AWeekOfMonth, const WORD ADayOfWeek )
{
  throw EConvertError( DateErrorString(_T(SysConst_SErrInvalidDateMonthWeek), AYear, AMonth, AWeekOfMonth, ADayOfWeek ) );
}


void InvalidDayOfWeekInMonthError( const WORD AYear, const WORD AMonth, const WORD ANthDayOfWeek, const WORD ADayOfWeek )
{
  throw EConvertError( DateErrorString(_T(SysConst_SErrInvalidDayOfWeekInMonth), AYear, AMonth, ANthDayOfWeek, ADayOfWeek ) );
}

bool TryEncodeDate( WORD Year, WORD Month, WORD Day, TDateTime& Date )
{
  bool result = false;
  unsigned int C = 0, YA = 0;
  result = ( Year > 0 ) && ( Year < 10000 ) && ( Sysutils__19.Contains(Month ) ) && ( Day > 0 ) && ( Day <= MonthDays[IsLeapYear( Year )][ Month - 1] ); // dme Month - 1 
  if ( result ) 
  {
    if ( Month > 2 ) 
      Month -= 3;
    else
    {
      Month += 9;
      Year--;
    }
    C = Year / 100;
    YA = Year - 100 * C;
    Date = (long double) ( ( 146097 * C ) >> 2 ) + ( ( 1461 * YA ) >> 2 ) + ( 153 * ((unsigned int) Month ) + 2 ) / 5 + ((unsigned int) Day );
     // Note that this Line can'T be part of the Line above, since TDateTime is
     // signed and C and YA are not
    Date = (double) Date - 693900;
  }
  return result;
}


bool TryEncodeTime( WORD Hour, WORD Min, WORD Sec, WORD MSec, TDateTime& time )
{
  bool result = false;
  result = ( Hour < 24 ) && ( Min < 60 ) && ( Sec < 60 ) && ( MSec < 1000 );
  if ( result ) 
//    time = ((TDateTime) ( ((unsigned int) Hour ) * 3600000 + ((unsigned int) Min ) * 60000 + ((unsigned int) Sec ) * 1000 + MSec ) ) / MSecsPerDay;
    time = ((double) ( ((unsigned int) Hour ) * 3600000 + ((unsigned int) Min ) * 60000 + ((unsigned int) Sec ) * 1000 + MSec ) ) / MSecsPerDay;
  return result;
}


int DoEncodeDate( WORD Year, WORD Month, WORD Day )
{
  int result = 0;
  TDateTime D = 0.0;
  if ( TryEncodeDate( Year, Month, Day, D ) ) 
    result = (int) Trunc( D );
  else
    result = 0;
  return result;
}


TDateTime DoEncodeTime( WORD Hour, WORD minute, WORD second, WORD millisecond )
{
  TDateTime result = 0.0;
  if ( ! TryEncodeTime( Hour, minute, second, millisecond, result ) ) 
    result = 0;
  return result;
}

//   ComposeDateTime converts A Date and A time into one TDateTime
TDateTime ComposeDateTime( TDateTime Date, TDateTime time )
{
  TDateTime result = 0.0;
  if ( (double) Date < 0 ) 
    result = Trunc( Date ) - Abs( Frac( time ) );
  else
    result = Trunc( Date ) + Abs( Frac( time ) );
  return result;
}

bool TryEncodeDateTime( const WORD AYear, const WORD AMonth, const WORD ADay, const WORD AHour, const WORD AMinute, const WORD ASecond, const WORD AMilliSecond, TDateTime& AValue )
{
  bool result = false;
  TDateTime tmp = 0.0;
  result = TryEncodeDate( AYear, AMonth, ADay, AValue );
  result = result && TryEncodeTime( AHour, AMinute, ASecond, AMilliSecond, tmp );
  if ( result )
    AValue = ComposeDateTime( AValue, tmp );
  return result;
}

TDateTime EncodeDateTime( const WORD AYear, const WORD AMonth, const WORD ADay, const WORD AHour, const WORD AMinute, const WORD ASecond, const WORD AMilliSecond )
{
  TDateTime result = 0.0;
  if ( ! TryEncodeDateTime( AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond, result ) )
    InvalidDateTimeError( AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond );
  return result;
}

void DecodeDateTime( const TDateTime AValue, WORD& AYear, WORD& AMonth, WORD& ADay, WORD& AHour, WORD& AMinute, WORD& ASecond, WORD& AMilliSecond )
{
  DecodeDate( AValue, AYear, AMonth, ADay );
  DecodeTime( AValue, AHour, AMinute, ASecond, AMilliSecond );
}

/*==============================================================================*/
/*   public functions                                                           */
/*==============================================================================*/


/*****************************************************************************
                              time functions
*****************************************************************************/

#ifdef windows
void GetLocalTime( TSystemTime& SYSTEMTIME )
{
//  LPSYSTEMTIME l;
  /*# Windows*/ ::TSystemTime Syst;
  /*# Windows*/ ::GetLocalTime( &Syst );
  SYSTEMTIME.wYear = Syst.wYear;
  SYSTEMTIME.wMonth = Syst.wMonth;
  SYSTEMTIME.wDay = Syst.wDay;
  SYSTEMTIME.wHour = Syst.wHour;
  SYSTEMTIME.wMinute = Syst.wMinute;
  SYSTEMTIME.wSecond = Syst.wSecond;
  SYSTEMTIME.wMilliseconds = Syst.wMilliseconds;
}
#elif defined(linux)

time_t Fptime( )
{
  return time(NULL);
}

typedef timeval* PTimeVal;

cint fpGetTimeOfDay( PTimeVal tp, struct timezone* tzp )
{
  cint r = gettimeofday(tp, tzp);
  return r==-1 ? -errno : r;
}
/*****************************************************************************
                              Locale functions
*****************************************************************************/


cint GetEpochTime( )
/*
  get the number of seconds since 00:00, January 1 1970, GMT
  the Time not corrected Any way
*/
{
  cint result = 0;
  result = Fptime();
  return result;
}

int tzseconds = 0;
const int C1970 = 2440588;
const int D0 = 1461;
const int D1 = 146097;
const int D2 = 1721119;


void JulianToGregorian( int JulianDN, WORD& Year, WORD& Month, WORD& Day )
{
  int YYear = 0, XYear = 0, Temp = 0, TempMonth = 0;
  Temp = ( ( JulianDN - D2 ) << 2 ) - 1;
  JulianDN = Temp / D1;
  XYear = ( Temp % D1 ) | 3;
  YYear = ( XYear / D0 );
  Temp = ( ( ( ( XYear % D0 ) + 4 ) >> 2 ) * 5 ) - 3;
  Day = ( ( Temp % 153 ) + 5 ) / 5;
  TempMonth = Temp / 153;
  if ( TempMonth >= 10 )
  {
    YYear++;
    TempMonth -= 12;
  }
  TempMonth += 3;
  Month = TempMonth;
  Year = YYear + ( JulianDN * 100 );
}


void EpochToLocal( int Epoch, WORD& Year, WORD& Month, WORD& Day, WORD& Hour, WORD& minute, WORD& second )
/*
  transforms Epoch Time into Local Time (Hour, minute,seconds)
*/
{
  int DateNum = 0;
  Epoch += tzseconds;
  DateNum = ( Epoch / 86400 ) + C1970;
  JulianToGregorian( DateNum, Year, Month, Day );
  Epoch = Abs( Epoch % 86400 );
  Hour = Epoch / 3600;
  Epoch = Epoch % 3600;
  minute = Epoch / 60;
  second = Epoch % 60;
}


void GetTime( WORD& Hour, WORD& Min, WORD& Sec, WORD& MSec, WORD& usec )
/*
  gets the current Time, adjusted to Local Time
*/
{
  WORD Year = 0, Day = 0, Month = 0;
  timeval tz;
  fpGetTimeOfDay( &tz, NULL );
  EpochToLocal( tz.tv_sec, Year, Month, Day, Hour, Min, Sec );
  MSec = tz.tv_usec / 1000;
  usec = tz.tv_usec % 1000;
}


void GetTime( WORD& Hour, WORD& Min, WORD& Sec, WORD& sec100 )
/*
  gets the current Time, adjusted to Local Time
*/
{
  WORD usec = 0;
  GetTime( Hour, Min, Sec, sec100, usec );
  sec100 = sec100 / 10;
}


void GetTime( WORD& Hour, WORD& Min, WORD& Sec )
/*
  gets the current Time, adjusted to Local Time
*/
{
  WORD MSec = 0, usec = 0;
  GetTime( Hour, Min, Sec, MSec, usec );
}


void GetDate( WORD& Year, WORD& Month, WORD& Day )
/*
  gets the current Date, adjusted to Local Time
*/
{
  WORD Hour = 0, minute = 0, second = 0;
  EpochToLocal( Fptime(), Year, Month, Day, Hour, minute, second );
}


void GetDateTime( WORD& Year, WORD& Month, WORD& Day, WORD& Hour, WORD& minute, WORD& second )
/*
  gets the current Date, adjusted to Local Time
*/
{
  EpochToLocal( Fptime(), Year, Month, Day, Hour, minute, second );
}


void GetLocalTime( TSystemTime& SYSTEMTIME )
{
  WORD usecs = 0;
  GetTime( SYSTEMTIME.wHour, SYSTEMTIME.wMinute, SYSTEMTIME.wSecond, SYSTEMTIME.wMilliseconds, usecs );
  GetDate( SYSTEMTIME.wYear, SYSTEMTIME.wMonth, SYSTEMTIME.wDay );
//  SYSTEMTIME.millisecond := 0;
}
#else
#error unknown platform
#endif

//   DateTimeToSystemTime converts DateTime Value to SYSTEMTIME 
void DateTimeToSystemTime( TDateTime DateTime, TSystemTime& SYSTEMTIME )
{
  DecodeDate( DateTime, SYSTEMTIME.wYear, SYSTEMTIME.wMonth, SYSTEMTIME.wDay );
  DecodeTime( DateTime, SYSTEMTIME.wHour, SYSTEMTIME.wMinute, SYSTEMTIME.wSecond, SYSTEMTIME.wMilliseconds );
}

//   SystemTimeToDateTime converts SYSTEMTIME to A TDateTime Value   
TDateTime SystemTimeToDateTime( const TSystemTime& SYSTEMTIME )
{
  TDateTime result = 0.0;
  result = ComposeDateTime( DoEncodeDate( SYSTEMTIME.wYear, SYSTEMTIME.wMonth, SYSTEMTIME.wDay ), DoEncodeTime( SYSTEMTIME.wHour, SYSTEMTIME.wMinute, SYSTEMTIME.wSecond, SYSTEMTIME.wMilliseconds ) );
  return result;
}

//   DayOfWeek returns the Day of the week (Sunday is Day 1)  
int DayOfWeek( TDateTime DateTime )
{
  //return 1 + ( Abs( Trunc( DateTime ) - 1 ) % 7 );
  return DateTime.DayOfWeek();
}

//   Date returns the current Date 
TDateTime Date( )
{
  return TDateTime::CurrentDate();
}

//   time returns the current time   
TDateTime Time( )
{
  return TDateTime::CurrentTime();
}

//   Now returns the current Date and time   
TDateTime Now( )
{
  return TDateTime::CurrentDateTime();
}



/*   DateTimeToTimeStamp converts DateTime to A TTimeStamp   */
TTimeStamp DateTimeToTimeStamp( TDateTime DateTime )
{
  TTimeStamp result;
  result.time = (int) Round( Abs( Frac( DateTime ) ) * MSecsPerDay );
  result.Date = DateDelta + (int) Trunc( DateTime );
  return result;
}

/*   TimeStampToDateTime converts TimeStamp to A TDateTime Value   */


TDateTime TimeStampToDateTime( const TTimeStamp& TimeStamp )
{
  return ComposeDateTime( TimeStamp.Date - DateDelta, double( TimeStamp.time ) / MSecsPerDay );
}

/*   MSecsToTimeStamp   */


TTimeStamp MSecsToTimeStamp( int64_t MSecs )
{
  TTimeStamp result;
  result.Date = (int) Trunc( double( MSecs ) / MSecsPerDay );
//  MSecs = MSecs - ((Comp) result.Date ) * MSecsPerDay;
  MSecs = MSecs - ((int64_t) result.Date ) * MSecsPerDay;
  result.time = (int) Round( (long double) MSecs );
  return result;
}

/*   TimeStampToMSecs   */


int64_t TimeStampToMSecs( const TTimeStamp& TimeStamp )
{
  int64_t result = 0;
//  result = TimeStamp.time + ((Comp) TimeStamp.Date ) * MSecsPerDay;
  result = TimeStamp.time + ((int64_t) TimeStamp.Date ) * MSecsPerDay;
  return result;
}



/*   EncodeDate Packs three variables Year, Month and Day into A
    TDateTime Value the result is the number of Days since 12/30/1899   */
TDateTime EncodeDate( WORD Year, WORD Month, WORD Day )
{
  TDateTime result = 0.0;
  if ( ! TryEncodeDate( Year, Month, Day, result ) ) 
    throw EConvertError( DateErrorString(_T("%d-%d-%d is not a valid date specification"), Year, Month, Day, -1 )); 
  return result;
}

/*   EncodeTime Packs four variables Hour, minute, second and millisecond into
    A TDateTime Value     */


TDateTime EncodeTime( WORD Hour, WORD minute, WORD second, WORD millisecond )
{
  TDateTime result = 0.0;
  if ( ! TryEncodeTime( Hour, minute, second, millisecond, result ) ) 
    throw EConvertError( DateErrorString(_T("%d:%d:%d.%d is not a valid time specification"), Hour, minute, second, millisecond )); 
  return result;
}


//   DecodeDate unpacks the Value Date into three values: Year, Month and Day   
void DecodeDate( long double Date, WORD& Year, WORD& Month, WORD& Day )
{
//  unsigned int ly = 0, ld = 0, lm = 0, j = 0;
  int64_t ly = 0, ld = 0, lm = 0, j = 0;
  if ( (double) Date <= - DateDelta )   // if Date is before 1-1-1 then return 0-0-0
  {
    Year = 0;
    Month = 0;
    Day = 0;
  }
  else
  {
    j = Pred( ( Trunc( /*# System*/ int( Date ) ) + 693900 ) << 2 );
    ly = j / 146097;
    j = j - 146097 * ((unsigned int) ly );
    ld = j >> 2;
    j = ( ( ld << 2 ) + 3 ) / 1461;
    ld = ( ( ((unsigned int) ld ) << 2 ) + 7 - 1461 * j ) >> 2;
    lm = ( 5 * ld - 3 ) / 153;
    ld = ( 5 * ld + 2 - 153 * lm ) / 5;
    ly = 100 * ((unsigned int) ly ) + j;
    if ( lm < 10 ) 
      lm += 3;
    else
    {
      lm -= 9;
      ly++;
    }
    Year = (WORD) ly;
    Month = (WORD) lm;
    Day = (WORD) ld;
  }
}

//   DecodeDate unpacks the Value Date into three values: Year, Month and Day   
void DecodeDate( TDateTime Date, WORD& Year, WORD& Month, WORD& Day )
{
  Date.DecodeDate(&Year,  &Month, &Day);
}

bool DecodeDateFully( const TDateTime DateTime, WORD& Year, WORD& Month, WORD& Day, WORD& DOW )
{
  bool result = false;
  //DecodeDate( DateTime, Year, Month, Day );
  DateTime.DecodeDate(&Year,  &Month, &Day);
  DOW = DateTime.DayOfWeek( );
  return IsLeapYear( Year );
}


// DecodeTime unpacks time into four values: Hour, minute, second and millisecond   
void DecodeTime( long double time, WORD& Hour, WORD& minute, WORD& second, WORD& millisecond )
{
  int64_t l = 0;
  l = Round( Abs( Frac( time ) ) * MSecsPerDay );
  Hour = (WORD)  (l / 3600000);
  l = l % 3600000;
  minute = (WORD) (l / 60000);
  l = l % 60000;
  second = (WORD) (l / 1000);
  l = l % 1000;
  millisecond = (WORD) l;
}

// DecodeTime unpacks time into four values: Hour, minute, second and millisecond   
void DecodeTime( TDateTime Time, WORD& Hour, WORD& minute, WORD& second, WORD& millisecond )
{
  Time.DecodeTime( &Hour, &minute, &second, &millisecond );
}


//   IncAMonth is the same as IncMonth, but operates ON decoded Date 
void IncAMonth( WORD& Year, WORD& Month, WORD& Day, int NumberOfMonths )
{
  int TempMonth = 0, S = 0;
  if ( NumberOfMonths >= 0 ) 
    S = 1;
  else
    S = - 1;
  Year += ( NumberOfMonths / 12 );
  TempMonth = Month + ( NumberOfMonths % 12 ) - 1;
  if ( ( TempMonth > 11 ) || ( TempMonth < 0 ) ) 
  {
    TempMonth -= S * 12;
    Year += S;
  }
  Month = TempMonth + 1;          /*   months from 1 to 12   */
  if ( Day > MonthDays[IsLeapYear( Year )][Month - 1] ) // dme Month - 1
    Day = MonthDays[IsLeapYear( Year )][Month - 1]; // dme Month - 1
}


//   IncMonth increments DateTime with NumberOfMonths months,
//    NumberOfMonths can be less than zero   
TDateTime IncMonth( const TDateTime DateTime, int NumberOfMonths )
{
  TDateTime result = 0.0;
  WORD Year = 0, Month = 0, Day = 0;
  DecodeDate( DateTime, Year, Month, Day );
  IncAMonth( Year, Month, Day, NumberOfMonths );
  result = ComposeDateTime( DoEncodeDate( Year, Month, Day ), DateTime );
  return result;
}


/*  IsLeapYear returns True if Year is A leap Year   */
bool IsLeapYear( WORD Year )
{
  return ( Year % 4 == 0 ) && ( ( Year % 100 != 0 ) || ( Year % 400 == 0 ) );
}

/*  DateToStr returns A String representation of Date Using ShortDateFormat   */
String DateToStr( TDateTime Date )
{
  return Date.DateString();
}

/*  TimeToStr returns A String representation of time Using LongTimeFormat   */
String TimeToStr( TDateTime Time )
{
  return Time.TimeString();
}

/*   DateTimeToStr returns A String representation of DateTime Using LongDateTimeFormat   */


String DateTimeToStr( TDateTime DateTime )
{
  return DateTime.DateTimeString();
}

/*   StrToDate converts the String S to A TDateTime Value
    if S does not represent A valid Date Value
    an EConvertError will be raised   */
TDateTime StrToDate( const String& S )
{
  String DF;
  WORD D = 0, m = 0, Y = 0, ly = 0;
  unsigned int n = 0, i = 0;
  int C = 0;
  unsigned char dp = '\0', mp = '\0', yp = '\0', which = '\0';
//  SmallString<4> S1;
  String S1;
  int values[ 3/*# range 1..3*/ ];
  TSystemTime localtime;
  bool YearMoreThenTwoDigits = false;
  YearMoreThenTwoDigits = false;
  DF = UpperCase( ShortDateFormat );
  /* determine order of D,m,Y */
  yp = 0;
  mp = 0;
  dp = 0;
  which = 0;
  i = 0;
  while ( ( i < DF.length( ) ) && ( which < 3 ) ) 
  {
    i++;
    switch ( DF[i - 1] )
    {
      case _T('Y'):
        if ( yp == 0 ) 
        {
          which++;
          yp = which;
        }
      break;
      case _T('M'):
        if ( mp == 0 ) 
        {
          which++;
          mp = which;
        }
      break;
      case _T('D'):
        if ( dp == 0 ) 
        {
          which++;
          dp = which;
        }
      break;
    }
  }
  if ( which != 3 ) 
    throw EConvertError( _T("Illegal format string") );
/* get Actual values */
  for ( i = 1; i <= 3; i++)
    values[i - 1] = 0;
  S1 = _T("");
  n = 0;
  for ( i = 1; i <= S.length( ); i++)
  {
    if ( Sysutils__20.Contains(S[i - 1] ) ) 
      S1 = String( S1 ) + S[i - 1];

     /* space can be part of the ShortDateFormat, and is defaultly in slovak
       Windows, therefor it Shouldn'T be taken as separator (unless So specified)
       and ignored */
    if ( ( DateSeparator != _T(' ') ) && ( S[i - 1] == _T(' ') ) )
      continue;
    if ( ( S[i - 1] == DateSeparator ) || ( ( i == S.length( ) ) && ( Sysutils__21.Contains(S[i - 1] ) ) ) ) 
    {
      n++;
      if ( n > 3 ) 
        throw EConvertError( _T("Invalid date format") );
         // Check if the Year has more then two Digits (if n=yp, then we are evaluating the Year.)
      if ( ( n == yp ) && ( S1[0] > 2 ) ) 
        YearMoreThenTwoDigits = true;
      Val( S1, &values[n - 1], C );
      if ( C != 0 ) 
        throw EConvertError( _T("Invalid date format") );
      S1 = _T("");
    }
    else
      if ( ! ( Sysutils__22.Contains(S[i - 1] ) ) ) 
        throw EConvertError( _T("Invalid date format") );
  }
  // Fill in values.
  GetLocalTime( localtime );
  ly = localtime.wYear;
  if ( n == 3 ) 
  {
    Y = values[yp - 1];
    m = values[mp - 1];
    D = values[dp - 1];
  }
  else
  {
    Y = ly;
    if ( n < 2 ) 
    {
      D = values[1 - 1];
      m = localtime.wMonth;
    }
    else
      if ( dp < mp ) 
      {
        D = values[1 - 1];
        m = values[2 - 1];
      }
      else
      {
        D = values[2 - 1];
        m = values[1 - 1];
      }
  }
  if ( ( Y >= 0 ) && ( Y < 100 ) && ! YearMoreThenTwoDigits ) 
  {
    ly = ly - Sysutils::TwoDigitYearCenturyWindow;
    Y += ly / 100 * 100;
    if ( ( TwoDigitYearCenturyWindow > 0 ) && ( Y < ly ) ) 
      Y += 100;
  }
  return EncodeDate( Y, m, D );
}


/*   StrToTime converts the String S to A TDateTime Value
    if S does not represent A valid time Value an
    EConvertError will be raised   */


int GetElement( const String& S, TDateTime& xResult, int& Len, int& current, int& PM )
{
  int result = 0;
  int j = 0, C = 0;
  result = - 1;
  current++;
  while ( ( result == - 1 ) && ( current <= Len ) ) 
  {
    if ( Sysutils__23.Contains(S[current - 1] ) ) 
    {
      j = current;
      while ( ( current < Len ) && ( Sysutils__24.Contains(S[current + 1 - 1] ) ) ) 
        current++;
      Val( S.substr( j - 1, 1 + current - j ), &result, C );
    }
    else
      if ( ( ( !TimeAMString.empty()) && ( S[current - 1] == TimeAMString[1 - 1] ) ) || ( Sysutils__25.Contains(S[current - 1] ) ) ) 
      {
        PM = 1;
        current = 1 + Len;
      }
      else
        if ( ( ( !TimePMString.empty()) && ( S[current - 1] == TimePMString[1 - 1] ) ) || ( Sysutils__26.Contains(S[current - 1] ) ) ) 
        {
          current = 1 + Len;
          PM = 2;
        }
        else
          if ( ( S[current - 1] == TimeSeparator ) || ( S[current - 1] == _T(' ') ) )
            current++;
          else
            throw EConvertError( _T("Invalid Time format") );
  }
  return result;
}


TDateTime StrToTime( const String& S )
{
  TDateTime result = 0.0;
  int Len = 0, current = 0;
  int PM = 0;
  int i = 0;
  int TimeValues[ 5/*# range 0..4*/ ];
  current = 0;
  Len = S.length( );
  PM = 0;
  for ( i = 0; i <= 4; i++)
    TimeValues[i] = 0;
  i = 0;
  TimeValues[i] = GetElement(S, result, Len, current, PM);
  while ( ( i < 5 ) && ( TimeValues[i] != - 1 ) ) 
  {
    i = i + 1;
    TimeValues[i] = GetElement(S, result, Len, current, PM);
  }
  if ( ( i < 5 ) && ( TimeValues[i] == - 1 ) ) 
    TimeValues[i] = 0;
  if ( PM == 2 ) 
  {
    if ( TimeValues[0] != 12 ) 
      TimeValues[0] += 12;
  }
  else
  {
    if ( ( PM == 1 ) && ( ( TimeValues[0] == 12 ) ) ) 
      TimeValues[0] = 0;
  }
  result = EncodeTime( TimeValues[0], TimeValues[1], TimeValues[2], TimeValues[3] );
  return result;
}

/*   StrToDateTime converts the String S to A TDateTime Value
    if S does not represent A valid Date and/or time Value
    an EConvertError will be raised   */
TDateTime StrToDateTime( const String& S )
{
  TDateTime result = 0.0;
  int i = 0, j = 0, k = 0, l = 0;
  String sd, st;
  l = S.length( );
  i = 1;
  while ( ( i <= l ) && ( S[i - 1] == _T(' ') ) )
    i++;
  j = i;
  while ( ( j <= l ) && ( S[j - 1] != _T(' ') ) )
    j++;
  k = j;
  while ( ( k <= l ) && ( S[k - 1] == _T(' ') ) )
    k++;
  sd = S.substr( i - 1, j - i );
  st = S.substr( k - 1, l );
  if ( ( st.empty()) && ( sd.find( TimeSeparator) != String::npos ) ) 
  {
    st = sd;
    sd = _T("");
  }
  if ( ( !sd.empty()) && ( !st.empty()) ) 
    result = ComposeDateTime( StrToDate( sd ), StrToTime( st ) );
  else
    if ( st.empty()) 
      result = StrToDate( sd );
    else
      result = StrToTime( st );
  return result;
}



String FormatDateTime( const String& FormatStr, TDateTime DateTime )
{
  return DateTime.FormatString(FormatStr);
}

/*   DateTimeToString Formats DateTime to the given Format in FormatStr   */
void DateTimeToString( String& result, const String& FormatStr, const TDateTime DateTime )
{
  result = FormatDateTime( FormatStr, DateTime );
}


int DateTimeToFileDate( TDateTime DateTime )
{
  return DateTime.FileDate();
}


WORD CurrentYear( )
{
  WORD result = 0;
  WORD yy = 0, mm = 0, dd = 0;
  DecodeDate( Now(), yy, mm, dd );
  result = yy;
  return result;
}


TDateTime FileDateToDateTime( int FileDate )
{
  return TDateTime::FileDateToDateTime( FileDate );
} 

// ieuw. These should  be written to work without Exceptions?



bool TryStrToDate( const String& S, TDateTime& Value )
{
  bool result = true;
  try
  {
    Value = StrToDate( S );
  }
  catch( EConvertError & )
  {
    result = false;
  }
  return result;
}


// function TryStrToDate(const S: String; out Value: TDateTime; const FormatSettings: TFormatSettings): Boolean;



bool TryStrToTime( const String& S, TDateTime& Value )
{
  bool result = true;
  try
  {
    Value = StrToTime( S );
  }
  catch( EConvertError & )
  {
    result = false;
  }
  return result;
}


// function TryStrToTime(const S: String; out Value: TDateTime; const FormatSettings: TFormatSettings): Boolean;



bool TryStrToDateTime( const String& S, TDateTime& Value )
{
  bool result = false;
  result = true;
  try
  {
    Value = StrToDateTime( S );
  }
  catch( EConvertError & )
  {
    result = false;
  }
  return result;
}


// function TryStrToDateTime(const S: String; out Value: TDateTime; const FormatSettings: TFormatSettings): Boolean;



TDateTime StrToDateDef( const String& S, const TDateTime DefValue )
{
  TDateTime result = 0.0;
  if ( ! TryStrToDate( S, result ) ) 
    result = DefValue;
  return result;
}


TDateTime StrToTimeDef( const String& S, const TDateTime DefValue )
{
  TDateTime result = 0.0;
  if ( ! TryStrToTime( S, result ) ) 
    result = DefValue;
  return result;
}


TDateTime StrToDateTimeDef( const String& S, const TDateTime DefValue )
{
  TDateTime result = 0.0;
  if ( ! TryStrToDateTime( S, result ) ) 
    result = DefValue;
  return result;
}


void ReplaceTime( TDateTime& dati, TDateTime NewTime )
{
  dati = ComposeDateTime( dati, NewTime );
}


void ReplaceDate( TDateTime& DateTime, const TDateTime NewDate )
{
  TDateTime tmp = 0.0;
  tmp = NewDate;
  ReplaceTime( tmp, DateTime );
  DateTime = tmp;
}



/*****************************************************************************
                              time functions
*****************************************************************************/

/*static*/ TDateTime TDateTime::CurrentDate()
{
  TSystemTime SYSTEMTIME;
  GetLocalTime( SYSTEMTIME );
  return DoEncodeDate( SYSTEMTIME.wYear, SYSTEMTIME.wMonth, SYSTEMTIME.wDay );
}

/*static*/ TDateTime TDateTime::CurrentTime()
{
  TSystemTime SYSTEMTIME;
  GetLocalTime( SYSTEMTIME );
  return DoEncodeTime( SYSTEMTIME.wHour, SYSTEMTIME.wMinute, SYSTEMTIME.wSecond, SYSTEMTIME.wMilliseconds );
}

/*static*/ TDateTime TDateTime::CurrentDateTime()
{
  TSystemTime SYSTEMTIME;
  GetLocalTime( SYSTEMTIME );
  return SystemTimeToDateTime( SYSTEMTIME );
}

/*static*/ TDateTime TDateTime::FileDateToDateTime(int FileDate)
{
  //return ::FileDateToDateTime(fileDate);
  WORD Date = 0, time = 0;
  Date = FileDate >> 16;
  time = FileDate & 0xFFFF;
  return ComposeDateTime( EncodeDate( ( Date >> 9 ) + 1980, ( Date >> 5 ) & 15, Date & 31 ), EncodeTime( time >> 11, ( time >> 5 ) & 63, ( time & 31 ) << 1, 0 ) );
}


TDateTime::TDateTime(const String& src, TDateTimeFlag flag /*= DateTime*/)
{
  switch(flag)
  {
  case Date:
   Val = StrToDate(src);
   break;
  case Time:
    Val = StrToTime(src);
    break;
  case DateTime:
    Val = StrToDateTime(src);
    break;
  }
}

TDateTime::TDateTime(unsigned short Year, unsigned short Month, unsigned short Day)
{ // DoEncodeDate
  TDateTime d;
  if ( TryEncodeDate( Year, Month, Day, d ) ) 
    Val = (int) Trunc( d );
  else
    Val = 0.0;
}


TDateTime::TDateTime(unsigned short Hour, unsigned short Minute, unsigned short Second, unsigned short Millisecond) 
{ // DoEncodeTime
  TDateTime d;
  if ( TryEncodeTime( Hour, Minute, Second, Millisecond, d ) ) 
    Val = d;
  else
    Val = 0.0;
}


String TDateTime::FormatString(const String& FormatStr) const
{  
  //return ::FormatDateTime( format, Val );
  String result;
  int ResultLen = 0;
  const int BufLen = 255;
  Char ResultBuffer[ 256/*# range 0..255*/ ];
  Char* ResultCurrent = NULL;
  WORD Year = 0, Month = 0, Day = 0, DayOfWeek = 0, Hour = 0, minute = 0, second = 0, millisecond = 0;
  DecodeDateFully( Val, Year, Month, Day, DayOfWeek );
  DecodeTime( &Hour, &minute, &second, &millisecond );
  ResultLen = 0;
  ResultCurrent = &ResultBuffer[0];
  StoreFormat( FormatStr, result, ResultLen, ResultBuffer, BufLen, ResultCurrent, Year, Month, Day, DayOfWeek, Hour, minute, second, millisecond );
  ResultBuffer[ResultLen] = _T('\x00');
  return ResultBuffer;
}

String TDateTime::DateString() const
{  
  return FormatString( _T("ddddd"));
}

String TDateTime::TimeString() const
{  
  return FormatString( _T("tt") );
}

String TDateTime::DateTimeString() const
{  
  return FormatString( _T("c") );
}


int TDateTime::DayOfWeek() const
{
  return 1 + ( Abs( Trunc( Val ) - 1 ) % 7 );
}


int TDateTime::FileDate() const
{  // DateTimeToFileDate
  int result = 0;
  WORD yy = 0, mm = 0, dd = 0, h = 0, m = 0, S = 0, MSec = 0;
  ::DecodeDate( Val, yy, mm, dd );
  ::DecodeTime( Val, h, m, S, MSec );
  if ( ( yy < 1980 ) || ( yy > 2099 ) ) 
    result = 0;
  else
  {
    result = ( S >> 1 ) | ( m << 5 ) | ( h << 11 );
    result = result | dd << 16 | ( mm << 21 ) | ( ( yy - 1980 ) << 25 );
  }
  return result;
}


void TDateTime::DecodeDate(unsigned short* xYear, unsigned short* xMonth, unsigned short* xDay) const
{
  unsigned short Year, Month, Day;
  ::DecodeDate(Val, Year, Month, Day);
  *xYear = Year;
  *xMonth = Month;
  *xDay = Day;
}

void TDateTime::DecodeTime(unsigned short* xHour, unsigned short* xMinute, unsigned short* xSecond, unsigned short* xMillisecond) const
{
  unsigned short Hour, Minute, Second, Millisecond;
  ::DecodeTime(Val, Hour, Minute, Second, Millisecond);
  *xHour = Hour;
  *xMinute = Minute; 
  *xSecond = Second;
  *xMillisecond = Millisecond;
}


////////// internal

/*   FormatDateTime Formats DateTime to the given Format String FormatStr   */
void TDateTime::StoreStr( const Char* str, int Len, int& ResultLen, Char*& ResultBuffer, int BufLen, Char*& ResultCurrent ) const
{
  if ( ResultLen + Len <  BufLen ) //sizeof( ResultBuffer ) ) 
  {
    StrMove( ResultCurrent, str, Len );
    ResultCurrent = ResultCurrent + Len;
    ResultLen = ResultLen + Len;
  }
}


void TDateTime::StoreString( const String& str, int& ResultLen, Char*& ResultBuffer, int BufLen, Char*& ResultCurrent ) const
{
  int Len = 0;
  Len = str.length( );
  if ( ResultLen + Len < BufLen ) //sizeof( ResultBuffer ) ) 
  { // StrMove not safe
    StrMove( ResultCurrent, str.c_str(), Len );
    ResultCurrent = ResultCurrent + Len;
    ResultLen = ResultLen + Len;
  }
}


void TDateTime::StoreInt( int Value, int Digits, int& ResultLen, Char*& ResultBuffer, int BufLen, Char*& ResultCurrent ) const
{
  String S;
  int Len = 0;
  S = IntToStr( Value );
  Len = S.length( );
  if ( Len < Digits ) 
  {
    S = String( _T("0000") ).substr( 1, Digits - Len ) + S;
    Len = Digits;
  }
  StoreStr( (Char*) S.c_str(), Len, ResultLen, ResultBuffer, BufLen, ResultCurrent );
}


void TDateTime::StoreFormat( const String& FormatStr, String& result, int& ResultLen, Char* ResultBuffer, int BufLen, Char*& ResultCurrent, WORD& Year, WORD& Month, WORD& Day, WORD& DayOfWeek, WORD& Hour, WORD& minute, WORD& second, WORD& millisecond ) const
{
  Char Token = _T('\0'), lastformattoken = _T('\0');
  Char* FormatCurrent = NULL;
  Char* FormatEnd = NULL;
  int Count = 0;
  bool Clock12 = false;
  Char* P = NULL;
  int tmp = 0;
  FormatCurrent = (Char*) ( ((void*) FormatStr.c_str()) );
  FormatEnd = FormatCurrent + FormatStr.length( );
  Clock12 = false;
  P = FormatCurrent;
  while ( P < FormatEnd ) 
  {
    Token = UpCase( *P );
    if ( Sysutils__27.Contains(Token ) ) 
    {
      P = P + 1;
      while ( ( P < FormatEnd ) && ( *P != Token ) ) 
        P = P + 1;
    }
    else
      if ( Token == _T('A') ) 
      {
        if ( ( StrLIComp( P, _T("A/P"), 3 ) == 0 ) || ( StrLIComp( P, _T("AMPM"), 4 ) == 0 ) || ( StrLIComp( P, _T("AM/PM"), 5 ) == 0 ) ) 
        {
          Clock12 = true;
          break;
        }
      }
    P = P + 1;
  }
  Token = _T('\xff');
  lastformattoken = _T(' ');
  while ( FormatCurrent < FormatEnd ) 
  {
    Token = UpCase( *FormatCurrent );
    Count = 1;
    P = FormatCurrent + 1;
    switch ( Token )
    {
      case _T('\''): case _T('\"'):
      {
        while ( ( P < FormatEnd ) && ( *P != Token ) ) 
          P = P + 1;
        P = P + 1;
        Count = P - FormatCurrent;
        StoreStr( FormatCurrent + 1, Count - 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
      }
      break;
      case _T('A'):
      {
        if ( StrLIComp( FormatCurrent, _T("AMPM"), 4 ) == 0 ) 
        {
          Count = 4;
          if ( Hour < 12 ) 
            StoreString( TimeAMString, ResultLen, ResultBuffer, BufLen, ResultCurrent );
          else
            StoreString( TimePMString, ResultLen, ResultBuffer, BufLen, ResultCurrent );
        }
        else
          if ( StrLIComp( FormatCurrent, _T("AM/PM"), 5 ) == 0 ) 
          {
            Count = 5;
            if ( Hour < 12 ) 
              StoreStr( _T("am"), 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
            else
              StoreStr( _T("pm"), 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
          }
          else
            if ( StrLIComp( FormatCurrent, _T("A/P"), 3 ) == 0 ) 
            {
              Count = 3;
              if ( Hour < 12 ) 
                StoreStr( _T("a"), 1, ResultLen, ResultBuffer, BufLen, ResultCurrent );
              else
                StoreStr( _T("p"), 1, ResultLen, ResultBuffer, BufLen, ResultCurrent );
            }
            else
              throw EConvertError( _T("Illegal character in format string") );
      }
      break;
      case _T('/'):
        StoreStr( &DateSeparator, 1, ResultLen, ResultBuffer, BufLen, ResultCurrent );
      break;
      case _T(':'):
        StoreStr( &TimeSeparator, 1, ResultLen, ResultBuffer, BufLen, ResultCurrent );
      break;
      case _T(' '): case _T('C'): case _T('D'): case _T('H'): case _T('M'): case _T('N'): case _T('S'): case _T('T'): case _T('Y'): case _T('Z'):
      {
//        while ( ( P < FormatEnd ) && ( wcscmp(&UpCase( *P )[1], Token ) == 0 ) ) 
        while ( ( P < FormatEnd ) && ( UpCase( *P ) == Token ) ) 
          P = P + 1;
        Count = P - FormatCurrent;
        switch ( Token )
        {
          case _T(' '):
            StoreStr( FormatCurrent, Count, ResultLen, ResultBuffer, BufLen, ResultCurrent );
          break;
          case _T('Y'):
          {
            if ( Count > 2 ) 
              StoreInt( Year, 4, ResultLen, ResultBuffer, BufLen, ResultCurrent );
            else
              StoreInt( Year % 100, 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
          }
          break;
          case _T('M'):
          {
            if ( lastformattoken == _T('H') ) 
            {
              if ( Count == 1 ) 
                StoreInt( minute, 0, ResultLen, ResultBuffer, BufLen, ResultCurrent );
              else
                StoreInt( minute, 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
            }
            else
            {
              switch ( Count )
              {
                case 1:
                  StoreInt( Month, 0, ResultLen, ResultBuffer, BufLen, ResultCurrent );
                break;
                case 2:
                  StoreInt( Month, 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
                break;
                case 3:
                  StoreString( ShortMonthNames[Month - 1], ResultLen, ResultBuffer, BufLen, ResultCurrent );
                break;
                case 4:
                  StoreString( LongMonthNames[Month - 1], ResultLen, ResultBuffer, BufLen, ResultCurrent );
                break;
              }
            }
          }
          break;
          case _T('D'):
          {
            switch ( Count )
            {
              case 1:
                StoreInt( Day, 0, ResultLen, ResultBuffer, BufLen, ResultCurrent );
              break;
              case 2:
                StoreInt( Day, 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
              break;
              case 3:
                StoreString( ShortDayNames[DayOfWeek - 1], ResultLen, ResultBuffer, BufLen, ResultCurrent );
              break;
              case 4:
                StoreString( LongDayNames[DayOfWeek - 1], ResultLen, ResultBuffer, BufLen, ResultCurrent );
              break;
              case 5:
                StoreFormat( ShortDateFormat, result, ResultLen, ResultBuffer, BufLen, ResultCurrent, Year, Month, Day, DayOfWeek, Hour, minute, second, millisecond );
              break;
              case 6:
                StoreFormat( LongDateFormat, result, ResultLen, ResultBuffer, BufLen, ResultCurrent, Year, Month, Day, DayOfWeek, Hour, minute, second, millisecond );
              break;
            }
          }
          break;
          case _T('H'):
          {
            if ( Clock12 ) 
            {
              tmp = Hour % 12;
              if ( tmp == 0 ) 
                tmp = 12;
              if ( Count == 1 ) 
                StoreInt( tmp, 0, ResultLen, ResultBuffer, BufLen, ResultCurrent );
              else
                StoreInt( tmp, 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
            }
            else
            {
              if ( Count == 1 ) 
                StoreInt( Hour, 0, ResultLen, ResultBuffer, BufLen, ResultCurrent );
              else
                StoreInt( Hour, 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
            }
          }
          break;
          case _T('N'):
          {
            if ( Count == 1 ) 
              StoreInt( minute, 0, ResultLen, ResultBuffer, BufLen, ResultCurrent );
            else
              StoreInt( minute, 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
          }
          break;
          case _T('S'):
          {
            if ( Count == 1 ) 
              StoreInt( second, 0, ResultLen, ResultBuffer, BufLen, ResultCurrent );
            else
              StoreInt( second, 2, ResultLen, ResultBuffer, BufLen, ResultCurrent );
          }
          break;
          case _T('Z'):
          {
            if ( Count == 1 ) 
              StoreInt( millisecond, 0, ResultLen, ResultBuffer, BufLen, ResultCurrent );
            else
              StoreInt( millisecond, 3, ResultLen, ResultBuffer, BufLen, ResultCurrent );
          }
          break;
          case _T('T'):
          {
            if ( Count == 1 ) 
              StoreFormat( ShortTimeFormat, result, ResultLen, ResultBuffer, BufLen, ResultCurrent, Year, Month, Day, DayOfWeek, Hour, minute, second, millisecond );
            else
              StoreFormat( LongTimeFormat, result, ResultLen, ResultBuffer, BufLen, ResultCurrent, Year, Month, Day, DayOfWeek, Hour, minute, second, millisecond );
          }
          break;
          case _T('C'):
          {
            StoreFormat( ShortDateFormat, result, ResultLen, ResultBuffer, BufLen, ResultCurrent, Year, Month, Day, DayOfWeek, Hour, minute, second, millisecond );
            if ( ( Hour != 0 ) || ( minute != 0 ) || ( second != 0 ) ) 
            {
              StoreString( _T(" "), ResultLen, ResultBuffer, BufLen, ResultCurrent );
              StoreFormat( LongTimeFormat, result, ResultLen, ResultBuffer, BufLen, ResultCurrent, Year, Month, Day, DayOfWeek, Hour, minute, second, millisecond );
            }
          }
          break;
        }
        lastformattoken = Token;
      }
      break;
    default:
      StoreStr( &Token, 1, ResultLen, ResultBuffer, BufLen, ResultCurrent );
    }
    FormatCurrent = FormatCurrent + Count;
  }
}



class Sysdate_unit
{
public:
Sysdate_unit()
{
  MonthDaysInit();
}
~Sysdate_unit(){  }
};
Sysdate_unit _Sysdate_unit;
