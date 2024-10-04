
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


#ifndef d2c_sysconstH
#define d2c_sysconstH


namespace System
{
// made with SysConst.ttp
#define SysConst_SAbortError	"Operation aborted"
#define SysConst_SAbstractError	"Abstract method called"
#define SysConst_SAccessDenied	"Access denied"
#define SysConst_SAccessViolation	"Access violation"
#define SysConst_SArgumentMissing	"Missing argument in format \"%s\""
#define SysConst_SAssertError	"%s (%s, line %d)"
#define SysConst_SAssertionFailed	"Assertion failed"
#define SysConst_SBusError	"Bus error or misaligned data access"
#define SysConst_SCannotCreateEmptyDir	"Cannot create empty directory"
#define SysConst_SControlC	"Control-C hit"
#define SysConst_SDiskFull	"Disk Full"
#define SysConst_SDispatchError	"No variant method call dispatch"
#define SysConst_SDivByZero	"Division by zero"
#define SysConst_SEndOfFile	"Read past end of file"
#define SysConst_SErrInvalidDateMonthWeek	"Year %d, month %d, Week %d and day %d is not a valid date."
#define SysConst_SErrInvalidDateWeek	"%d %d %d is not a valid dateweek"
#define SysConst_SErrInvalidDayOfWeek	"%d is not a valid day of the week"
#define SysConst_SErrInvalidDayOfWeekInMonth	"Year %d Month %d NDow %d DOW %d is not a valid date"
#define SysConst_SErrInvalidDayOfYear	"Year %d does not have a day number %d"
#define SysConst_SErrInvalidTimeStamp	"Invalid date/timestamp : \"%s\""
#define SysConst_SExceptionErrorMessage	"exception at %p"
#define SysConst_SExceptionStack	"Exception stack error"
#define SysConst_SExecuteProcessFailed	"Failed to execute \"%s\", error code: %d"
#define SysConst_SExternalException	"External exception %x"
#define SysConst_SFileNotAssigned	"File not assigned"
#define SysConst_SFileNotFound	"File not found"
#define SysConst_SFileNotOpen	"File not open"
#define SysConst_SFileNotOpenForInput	"File not open for input"
#define SysConst_SFileNotOpenForOutput	"File not open for output"
#define SysConst_SInValidFileName	"Invalid filename"
#define SysConst_SIntOverflow	"Arithmetic overflow"
#define SysConst_SIntfCastError	"Interface not supported"
#define SysConst_SInvalidArgIndex	"Invalid argument index in format \"%s\""
#define SysConst_SInvalidBoolean	"\"%s\" is not a valid boolean."
#define SysConst_SInvalidCast	"Invalid type cast"
#define SysConst_SinvalidCurrency	"Invalid Currency: \"%s\""
#define SysConst_SInvalidDateTime	"%f is not a valid date/time value."
#define SysConst_SInvalidDrive	"Invalid drive specified"
#define SysConst_SInvalidFileHandle	"Invalid file handle"
#define SysConst_SInvalidFloat	"\"%s\" is an invalid float"
#define SysConst_SInvalidFormat	"Invalid format specifier : \"%s\""
#define SysConst_SInvalidGUID	"\"%s\" is not a valid TGUID value"
#define SysConst_SInvalidInput	"Invalid input"
#define SysConst_SInvalidInteger	"\"%s\" is an invalid integer"
#define SysConst_SInvalidOp	"Invalid floating point operation"
#define SysConst_SInvalidPointer	"Invalid pointer operation"
#define SysConst_SInvalidVarCast	"Invalid variant type cast"
#define SysConst_SInvalidvarNullOp	"Invalid NULL variant operation"
#define SysConst_SInvalidVarOp	"Invalid variant operation"
#define SysConst_SInvalidBinaryVarOp	"Invalid variant operation %s %s %s"
#define SysConst_SInvalidUnaryVarOp	"Invalid variant operation %s %s"
#define SysConst_SInvalidVarOpWithHResultWithPrefix	"Invalid variant operation (%s%.8x)"
#define SysConst_SNoError	"No error."
#define SysConst_SNoThreadSupport	"Threads not supported. Recompile program with thread driver."
#define SysConst_SMissingWStringManager	"Widestring manager not available. Recompile program with appropiate manager."
#define SysConst_SOSError	"System error, (OS Code %d):"
#define SysConst_SOutOfMemory	"Out of memory"
#define SysConst_SOverflow	"Floating point overflow"
#define SysConst_SPrivilege	"Privileged instruction"
#define SysConst_SRangeError	"Range check error"
#define SysConst_SSafecallException	"Exception in safecall method"
#define SysConst_SiconvError	"iconv error"
#define SysConst_STooManyOpenFiles	"Too many open files"
#define SysConst_SUnKnownRunTimeError	"Unknown Run-Time error : %3.3d"
#define SysConst_SUnderflow	"Floating point underflow"
#define SysConst_SUnkOSError	"An operating system call failed."
#define SysConst_SUnknown	"Unknown run-time error code: "
#define SysConst_SUnknownErrorCode	"Unknown error code: %d"
#define SysConst_SVarArrayBounds	"Variant array bounds error"
#define SysConst_SVarArrayCreate	"Variant array cannot be created"
#define SysConst_SVarArrayLocked	"Variant array locked"
#define SysConst_SVarBadType	"Invalid variant type"
#define SysConst_SVarInvalid	"Invalid argument"
#define SysConst_SVarInvalid1	"Invalid argument: %s"
#define SysConst_SVarNotArray	"Variant doesn't contain an array"
#define SysConst_SVarNotImplemented	"Operation not supported"
#define SysConst_SVarOutOfMemory	"Variant operation ran out memory"
#define SysConst_SVarOverflow	"Variant overflow"
#define SysConst_SVarParamNotFound	"Variant Parameter not found"
#define SysConst_SVarTypeAlreadyUsedWithPrefix	"Custom variant type (%s%.4x) already used by %s"
#define SysConst_SVarTypeConvertOverflow	"Overflow while converting variant of type (%s) into type (%s)"
#define SysConst_SVarTypeCouldNotConvert	"Could not convert variant of type (%s) into type (%s)"
#define SysConst_SVarTypeNotUsableWithPrefix	"Custom variant type (%s%.4x) is not usable"
#define SysConst_SVarTypeOutOfRangeWithPrefix	"Custom variant type (%s%.4x) is out of range"
#define SysConst_SVarTypeRangeCheck1	"Range check error for variant of type (%s)"
#define SysConst_SVarTypeRangeCheck2	"Range check error while converting variant of type (%s) into type (%s)"
#define SysConst_SVarTypeTooManyCustom	"Too many custom variant types have been registered"
#define SysConst_SVarUnexpected	"Unexpected variant error"
#define SysConst_SFallbackError	"An error, whose error code is larger than can be returned to the OS, has occured"
#define SysConst_SNoToolserver	"Toolserver is not installed, cannot execute Tool"
#define SysConst_SNoArrayMatch	"Can't match any allowed value at pattern position %d, string position %d."
#define SysConst_SNoCharMatch	"Mismatch char \"%s\" <> \"%s\" at pattern position %d, string position %d."
#define SysConst_Shhmmerror	"mm in a sequence hh:mm is interpreted as minutes. No longer versions allowed! (Position : %d)."
#define SysConst_SFullpattern	"Couldn't match entire pattern string. Input too short at pattern position %d."
#define SysConst_SPatternCharMismatch	"Pattern mismatch char \"%s\" at position %d."
#define SysConst_SShortMonthNameJan	"Jan"
#define SysConst_SShortMonthNameFeb	"Feb"
#define SysConst_SShortMonthNameMar	"Mar"
#define SysConst_SShortMonthNameApr	"Apr"
#define SysConst_SShortMonthNameMay	"May"
#define SysConst_SShortMonthNameJun	"Jun"
#define SysConst_SShortMonthNameJul	"Jul"
#define SysConst_SShortMonthNameAug	"Aug"
#define SysConst_SShortMonthNameSep	"Sep"
#define SysConst_SShortMonthNameOct	"Oct"
#define SysConst_SShortMonthNameNov	"Nov"
#define SysConst_SShortMonthNameDec	"Dec"
#define SysConst_SLongMonthNameJan	"January"
#define SysConst_SLongMonthNameFeb	"February"
#define SysConst_SLongMonthNameMar	"March"
#define SysConst_SLongMonthNameApr	"April"
#define SysConst_SLongMonthNameMay	"May"
#define SysConst_SLongMonthNameJun	"June"
#define SysConst_SLongMonthNameJul	"July"
#define SysConst_SLongMonthNameAug	"August"
#define SysConst_SLongMonthNameSep	"September"
#define SysConst_SLongMonthNameOct	"October"
#define SysConst_SLongMonthNameNov	"November"
#define SysConst_SLongMonthNameDec	"December"
#define SysConst_SShortDayNameMon	"Mon"
#define SysConst_SShortDayNameTue	"Tue"
#define SysConst_SShortDayNameWed	"Wed"
#define SysConst_SShortDayNameThu	"Thu"
#define SysConst_SShortDayNameFri	"Fri"
#define SysConst_SShortDayNameSat	"Sat"
#define SysConst_SShortDayNameSun	"Sun"
#define SysConst_SLongDayNameMon	"Monday"
#define SysConst_SLongDayNameTue	"Tuesday"
#define SysConst_SLongDayNameWed	"Wednesday"
#define SysConst_SLongDayNameThu	"Thursday"
#define SysConst_SLongDayNameFri	"Friday"
#define SysConst_SLongDayNameSat	"Saturday"
#define SysConst_SLongDayNameSun	"Sunday"


}  // namespace System


#endif //  d2c_sysconstH