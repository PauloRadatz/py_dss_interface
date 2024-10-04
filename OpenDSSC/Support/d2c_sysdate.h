#ifndef d2c_sysdateH
#define d2c_sysdateH

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




	class TDateTime
	{
	public:
	  enum TDateTimeFlag {Date, Time, DateTime};

	  static TDateTime CurrentDate();
	  static TDateTime CurrentTime();
	  static TDateTime CurrentDateTime();
	  static TDateTime FileDateToDateTime(int fileDate);

	  TDateTime()                                  {Val = 0;}
	  TDateTime(const TDateTime& src)              {Val = src.Val;}
	  TDateTime(const long double src)             {Val = src;}
	  TDateTime(const double src)                  {Val = src;}
	  TDateTime(const int src)                     {Val = src;}
	  TDateTime(const String& src, TDateTimeFlag flag = DateTime);
	  TDateTime(unsigned short Year, unsigned short Month, unsigned short Day);
	  TDateTime(unsigned short Hour, unsigned short Minute, unsigned short Second, unsigned short Millisecond);

	  TDateTime& operator =(const TDateTime& rhs)
	  {Val = rhs.Val;return *this;}
	  TDateTime& operator =(const long double rhs)
	  {Val = rhs; return *this;}
	  TDateTime& operator =(const double rhs)
	  {Val = rhs; return *this;}
	  TDateTime& operator =(const int rhs)
	  {Val = rhs; return *this;}

	  TDateTime& operator +=(const TDateTime& rhs)
	  {Val+=rhs.Val;return *this;}
	  TDateTime& operator +=(const long double rhs)
	  {Val += rhs; return *this;}
	  TDateTime& operator +=(const double rhs)
	  {Val += rhs; return *this;}
	  TDateTime& operator +=(const int rhs)
	  {Val += rhs; return *this;}

	  TDateTime& operator -=(const TDateTime& rhs)
	  {Val-=rhs.Val;return *this;}
	  TDateTime& operator -=(const long double rhs)
	  {Val -= rhs; return *this;}
	  TDateTime& operator -=(const double rhs)
	  {Val -= rhs; return *this;}
	  TDateTime& operator -=(const int rhs)
	  {Val -= rhs; return *this;}

	  TDateTime& operator ++() {Val++; return *this;}
	  TDateTime operator ++(int) {TDateTime tmp(*this); Val++; return tmp;}
	  TDateTime& operator --() {Val--; return *this;}
	  TDateTime operator --(int) {TDateTime tmp(*this); Val--; return tmp;}

	  TDateTime operator +(const TDateTime& rhs) const
	  {return Val+rhs.Val;}
	  TDateTime operator +(const long double rhs) const {return Val+rhs;}
	  TDateTime operator +(const double rhs) const {return Val+rhs;}
	  TDateTime operator +(const int rhs) const {return Val+rhs;}

	  TDateTime operator -(const TDateTime& rhs) const
	  {return Val-rhs.Val;}
	  TDateTime operator -(const long double rhs) const {return Val-rhs;}
	  TDateTime operator -(const double rhs) const {return Val-rhs;}
	  TDateTime operator -(const int rhs) const {return Val-rhs;}

	  // comparisons
	  bool operator ==(const TDateTime& rhs) const
	  {return Val == rhs.Val;}
	  bool operator >(const TDateTime& rhs) const
	  {return Val > rhs.Val;}
	  bool operator <(const TDateTime& rhs) const
	  {return Val < rhs.Val;}
	  bool operator >=(const TDateTime& rhs) const
	  {return Val >= rhs.Val;}
	  bool operator <=(const TDateTime& rhs) const
	  {return Val <= rhs.Val;}

	  operator String() const;//<Date||Time||DateTime>String(smart)
	  String FormatString(const String& format) const;
	  String DateString() const;
	  String TimeString() const;
	  String DateTimeString() const;
	  operator long double() const {return Val;}
	  operator double() const {return Val;}
	  operator int() const    {return (int)Val;}

	  int DayOfWeek() const;
	  int FileDate() const;
	  void DecodeDate(unsigned short* Year, unsigned short* Month, unsigned short* Day) const;
	  void DecodeTime(unsigned short* Hour, unsigned short* Minute, unsigned short* Second, unsigned short* Millisecond) const;

	private:
		long double Val;

    void StoreStr( const Char* str, int Len, int& ResultLen, Char*& ResultBuffer, int BufLen, Char*& ResultCurrent ) const;
    void StoreString( const String& str, int& ResultLen, Char*& ResultBuffer, int BufLen, Char*& ResultCurrent ) const;
    void StoreInt( int Value, int Digits, int& ResultLen, Char*& ResultBuffer, int BufLen, Char*& ResultCurrent ) const;
    void StoreFormat( const String& FormatStr, String& result, int& ResultLen, Char* ResultBuffer, int BufLen, Char*& ResultCurrent, WORD& Year, WORD& Month, WORD& Day, WORD& DayOfWeek, WORD& Hour, WORD& minute, WORD& second, WORD& millisecond ) const;

	};


typedef TDateTime *PDate;
typedef TDateTime *PDateTime;


const int DateDelta = 693594;        // Days between 1/1/0001 and 12/31/1899
const int HoursPerDay = 24;
const int MinsPerHour = 60;
const int SecsPerMin = 60;
const int MSecsPerSec = 1000;
const int MinsPerDay = HoursPerDay * MinsPerHour;
const int SecsPerDay = MinsPerDay * SecsPerMin;
const int MSecsPerDay = SecsPerDay * MSecsPerSec;

#ifdef windows

typedef SYSTEMTIME TSystemTime;
void DateTimeToSystemTime( TDateTime DateTime, TSystemTime& SYSTEMTIME );
TDateTime SystemTimeToDateTime( const TSystemTime& SYSTEMTIME );
void GetLocalTime( TSystemTime& SYSTEMTIME );

#elif defined(linux)

typedef WORD* PDayTable;
typedef WORD TDayTable [ 12/*# range 1..12*/ ];

/*TDateTime holds the Date as the number of Days since 30 Dec 1899, known as
Microsoft Excel Epoch*/
const double JulianEpoch = ((TDateTime) - 2415018.5 );
const double UnixEpoch = JulianEpoch + (double) ((TDateTime) 2440587.5 );

void EpochToLocal( int Epoch, WORD& Year, WORD& Month, WORD& Day, WORD& Hour, WORD& minute, WORD& second );


//const int64_t UnixDateDelta = Trunc( UnixEpoch ); //25569


   /* True=Leapyear */

extern TDayTable MonthDays [ 2 /*# Boolean */ ];
                             /* Threshold to be subtracted from Year before
                               Age-Detection.*/

   /*  Date Time formatting characters:
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
      :      : Insert Time seperator
      "xx"   : literal Text
      'xx'   : literal Text
   */


/* Windows isn'T defined in 2.0.2 (FK) */ 
   /* Win32 reuses the struct from the Windows unit */
struct TSystemTime {
  WORD wYear, wMonth, wDay;
  WORD wHour, wMinute, wSecond, wMilliseconds;
};

#else
#error unknown System
#endif

struct TTimeStamp {
  int time;   /* number of milliseconds since midnight */
  int Date;   /* one plus number of Days since 1/1/0001 */
};

TTimeStamp DateTimeToTimeStamp( TDateTime DateTime );
TDateTime TimeStampToDateTime( const TTimeStamp& TimeStamp );
TTimeStamp MSecsToTimeStamp( int64_t MSecs );
int64_t TimeStampToMSecs( const TTimeStamp& TimeStamp );
bool TryEncodeDate( WORD Year, WORD Month, WORD Day, TDateTime& Date );
bool TryEncodeTime( WORD Hour, WORD Min, WORD Sec, WORD MSec, TDateTime& time );
TDateTime EncodeDate( WORD Year, WORD Month, WORD Day );
TDateTime EncodeTime( WORD Hour, WORD minute, WORD second, WORD millisecond );
TDateTime ComposeDateTime( TDateTime Date, TDateTime time );
void DecodeDate( TDateTime Date, WORD& Year, WORD& Month, WORD& Day );
bool DecodeDateFully( const TDateTime DateTime, WORD& Year, WORD& Month, WORD& Day, WORD& DOW );
void DecodeTime( TDateTime time, WORD& Hour, WORD& minute, WORD& second, WORD& millisecond );
TDateTime EncodeDateTime( const WORD AYear, const WORD AMonth, const WORD ADay, const WORD AHour, const WORD AMinute, const WORD ASecond, const WORD AMilliSecond );
void DecodeDateTime( const TDateTime AValue, WORD& AYear, WORD& AMonth, WORD& ADay, WORD& AHour, WORD& AMinute, WORD& ASecond, WORD& AMilliSecond );
//void DateTimeToSystemTime( TDateTime DateTime, TSystemTime& SYSTEMTIME );
//TDateTime SystemTimeToDateTime( const TSystemTime& SYSTEMTIME );
int DayOfWeek( TDateTime DateTime );
TDateTime Date( );
TDateTime Time( );
TDateTime Now( );
TDateTime IncMonth( const TDateTime DateTime, int NumberOfMonths = 1 );
void IncAMonth( WORD& Year, WORD& Month, WORD& Day, int NumberOfMonths = 1 );
bool IsLeapYear( WORD Year );
String DateToStr( TDateTime Date );
String TimeToStr( TDateTime time );
String DateTimeToStr( TDateTime DateTime );
TDateTime StrToDate( const String& S );
TDateTime StrToTime( const String& S );
TDateTime StrToDateTime( const String& S );
String FormatDateTime( const String& FormatStr, TDateTime DateTime );
void DateTimeToString( String& result, const String& FormatStr, const TDateTime DateTime );
int DateTimeToFileDate( TDateTime DateTime );
TDateTime FileDateToDateTime( int FileDate );
bool TryStrToDate( const String& S, TDateTime& Value );
bool TryStrToTime( const String& S, TDateTime& Value );
bool TryStrToDateTime( const String& S, TDateTime& Value );
TDateTime StrToDateDef( const String& S, const TDateTime DefValue );
TDateTime StrToTimeDef( const String& S, const TDateTime DefValue );
TDateTime StrToDateTimeDef( const String& S, const TDateTime DefValue );
WORD CurrentYear( );
/* FPC extra */
//void GetLocalTime( TSystemTime& SYSTEMTIME );
void ReplaceTime( TDateTime& dati, TDateTime NewTime );
void ReplaceDate( TDateTime& DateTime, const TDateTime NewDate );

extern TDateTime MinDateTime;
extern TDateTime MaxDateTime;


#endif  // lpl_sysdateH