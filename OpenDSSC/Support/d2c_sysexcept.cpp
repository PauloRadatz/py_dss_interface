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

#include "d2c_sysexcept.h"
#include <stdexcept>
#include "d2c_system.h"
#include "Sysutils.h"
#include "d2c_varrec.h"
#include "d2c_sysconst.h"
#include "d2c_sysstring.h"
#include <stdio.h>

#ifdef linux
#include <error.h>
#endif

using namespace std;
using namespace System;



namespace System
{

EOutOfMemory* OutOfMemory = NULL;
EInvalidPointer* InvalidPointer = NULL;
TOnShowException OnShowException = NULL;
int ExitCode = 0;

#ifdef linux
int geterrno( )
{
  return errno;
}

void seterrno( int err )
{
  errno = err;
} 


/******************************************************************************
                            Error conversion
******************************************************************************/

/*
  the lowlevel File functions should take care of setting the InOutRes to the
  correct Value if an Error has occured, else leave it untouched
*/


WORD PosixToRunError( int PosixErrno )
/*
  convert errno Error to the correct InOutRes Value
*/
{
  WORD result = 0;
/*
  if PosixErrno=0 then {else it will go through all the cases}
    exit(0);

  Statement commented out. it will not go through all the cases. (dm)
*/
  switch ( PosixErrno )
  {
    case ESysENFILE: case ESysEMFILE:
      result = 4;
    break;
    case ESysENOENT:
      result = 2;
    break;
    case ESysEBADF:
      result = 6;
    break;
    case ESysENOMEM: case ESysEFAULT:
      result = 217;
    break;
    case ESysEINVAL:
      result = 218;
    break;
    case ESysEPIPE: case ESysEINTR: case ESysEIO: case ESysEAGAIN: case ESysENOSPC:
      result = 101;
    break;
    case ESysENAMETOOLONG:
      result = 3;
    break;
    case ESysEROFS: case ESysEEXIST: case ESysENOTEMPTY: case ESysEACCES:
      result = 5;
    break;
    case ESysEISDIR:
      result = 5;
    break;
  default:
    result = PosixErrno;
  }
  InOutRes = result;
  return result;
}
#endif



/*****************************************************************************
                           Target independent RunError
*****************************************************************************/



WORD ErrorCode = 0;

enum TRuntimeError {reNone,
                    reOutOfMemory,
                    reInvalidPtr,
                    reDivByZero,
                    reRangeError,
                    reIntOverflow,
                    reInvalidOp,
                    reZeroDivide,
                    reOverflow,
                    reUnderflow,
                    reInvalidCast,
                    reAccessViolation,
                    rePrivInstruction,
                    reControlBreak,
                    reStackOverflow,
                    reVarTypeCast,
                    reVarInvalidOp,
                    reVarDispatch,
                    reVarArrayCreate,
                    reVarNotArray,
                    reVarArrayBounds,
                    reAssertionFailed,
                    reExternalException,
                    reIntfCastError,
                    reSafeCallError,
                    reQuit,
                    reCodesetConversion };

void Halt( WORD errnum )
{
  ExitCode = errnum;
  exit(errnum);
}

void Halt( )
{
  Halt( 0 );
}

void RunError( WORD W )
{
  ErrorCode = W;
// todo  ErrorAddr = get_caller_addr( Get_frame() );
// todo  ErrorBase = get_caller_frame( Get_frame() );
  if ( ErrorCode <= maxExitCode )
    Halt( (unsigned char) ErrorCode );
  else
    Halt( 255 );
}

void RunError( )
{
  RunError( 0 );
}

int ExceptionErrorMessage( TObject* ExceptObject, void* ExceptAddr, Char* Buffer, int Size )
{
  int result = 0;
  String S;
  int Len = 0;
  //S = Format( _T(SysConst_SExceptionErrorMessage), ARRAYOFCONST(( ExceptAddr, ExceptObject->ClassName() )) );
  S = _T("exception at ");
  Char buf[20];
#ifdef _WIDESTRING
  swprintf( buf, 19, _T("%p"), ExceptAddr);
#else
  snprintf( buf, 19, "%p", ExceptAddr);
#endif
  buf[19]=0;
  S += buf;
  if(dynamic_cast< Exception* >( ExceptObject ) != NULL)
  {
    //S = Format( _T("%s:\x0a%s"), ARRAYOFCONST(( S, ((Exception*)ExceptObject)->ReadPropertyMessage() )) );
    S += _T(":\x0a");
    S += ((Exception*)ExceptObject)->ReadPropertyMessage();
  }
  Len = S.length( );
  if ( S[Len - 1] != '.' )
  {
    S = S + _T(".");
    Len++;
  }
  if ( Len > Size )
    Len = Size;
  if ( Len > 0 )
    memmove(Buffer, S.c_str(), Len * sizeof(Char));
  result = Len;
  return result;
}

void ShowException( TObject* ExceptObject, void* ExceptAddr )
// use ShortString. ON Exception, the heap may be corrupt.
{
  //SmallString<255> Buf;
  Char Buf[256];
  Buf[0] = ExceptionErrorMessage( ExceptObject, ExceptAddr, Buf, 255 );
  if ( IsConsole )
    WriteLn( Buf );
  else
    if (( OnShowException != NULL ) )
      OnShowException( Buf );
}


void Abort( )
{
  throw EAbort( _T(SysConst_SAbortError) ) /* at ((void*) get_caller_addr( Get_frame() ) ) */;
}


void OutOfMemoryError( )
{
  throw OutOfMemory;
}

/*****************************************************************************
                           Target dependent RaiseLastOSError
*****************************************************************************/


#ifdef windows

// Please keep locations corresponding to location in array above
unsigned char RuntimeErrorExitCodes [ 27 /*# TRuntimeError */ ];
void RuntimeErrorExitCodesInit( )
{
  RuntimeErrorExitCodes[0] = 0;
  RuntimeErrorExitCodes[1] = 203;
  RuntimeErrorExitCodes[2] = 204;
  RuntimeErrorExitCodes[3] = 200;
  RuntimeErrorExitCodes[4] = 201;
  RuntimeErrorExitCodes[5] = 215;
  RuntimeErrorExitCodes[6] = 207;
  RuntimeErrorExitCodes[7] = 200;
  RuntimeErrorExitCodes[8] = 205;
  RuntimeErrorExitCodes[9] = 206;
  RuntimeErrorExitCodes[10] = 219;
  RuntimeErrorExitCodes[11] = 216;
  RuntimeErrorExitCodes[12] = 218;
  RuntimeErrorExitCodes[13] = 217;
  RuntimeErrorExitCodes[14] = 202;
  RuntimeErrorExitCodes[15] = 220;
  RuntimeErrorExitCodes[16] = 221;
  RuntimeErrorExitCodes[17] = 222;
  RuntimeErrorExitCodes[18] = 223;
  RuntimeErrorExitCodes[19] = 224;
  RuntimeErrorExitCodes[20] = 225;
  RuntimeErrorExitCodes[21] = 227;
  RuntimeErrorExitCodes[22] = 212;
  RuntimeErrorExitCodes[23] = 228;
  RuntimeErrorExitCodes[24] = 229;
  RuntimeErrorExitCodes[25] = 233;
  RuntimeErrorExitCodes[26] = 234;
}


String SysErrorMessage( int ErrorCode )
{
  String result;
  const int MaxMsgSize = FORMAT_MESSAGE_MAX_WIDTH_MASK;
  Char* MsgBuffer = NULL;
  GetMem( MsgBuffer, MaxMsgSize * sizeof(Char) );
  FillChar( MsgBuffer, MaxMsgSize * sizeof(Char), '\x00' );
  // nSize: number of bytes (ANSI version) or characters (Unicode version)
  FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM, NULL, ErrorCode, MAKELANGID( LANG_NEUTRAL, SUBLANG_DEFAULT ), MsgBuffer, MaxMsgSize, NULL );                 /* this function allocs the memory */                           /* maximum Message Size */
  result = MsgBuffer;
  System::FreeMem( MsgBuffer, MaxMsgSize * sizeof(Char) );
  return result;
}

int GetLastOSError( )
{
  return GetLastError();
}
#elif defined(linux)

const Char* sys_errlist [ 125/*# range 0..sys_errn-1*/ ];
void sys_errlistInit( )
{
  sys_errlist[0] = _T("Success");
  sys_errlist[1] = _T("Operation not permitted");
  sys_errlist[2] = _T("No such file or directory");
  sys_errlist[3] = _T("No such process");
  sys_errlist[4] = _T("Interrupted system cal_T(");
  sys_errlist[5] = _T("I/O error");
  sys_errlist[6] = _T("No such device or address");
  sys_errlist[7] = _T("Arg list too long");
  sys_errlist[8] = _T("Exec format error");
  sys_errlist[9] = _T("Bad file number");
  sys_errlist[10] = _T("No child processes");
  sys_errlist[11] = _T("Try again");
  sys_errlist[12] = _T("Out of memory");
  sys_errlist[13] = _T("Permission denied");
  sys_errlist[14] = _T("Bad address");
  sys_errlist[15] = _T("Block device required");
  sys_errlist[16] = _T("Device or resource busy");
  sys_errlist[17] = _T("File exists");
  sys_errlist[18] = _T("Cross-device link");
  sys_errlist[19] = _T("No such device");
  sys_errlist[20] = _T("Not a directory");
  sys_errlist[21] = _T("Is a directory");
  sys_errlist[22] = _T("Invalid argument");
  sys_errlist[23] = _T("File table overflow");
  sys_errlist[24] = _T("Too many open files");
  sys_errlist[25] = _T("Not a typewriter");
  sys_errlist[26] = _T("Text (code segment) file busy");
  sys_errlist[27] = _T("File too large");
  sys_errlist[28] = _T("No space left on device");
  sys_errlist[29] = _T("Illegal seek");
  sys_errlist[30] = _T("Read-only file system");
  sys_errlist[31] = _T("Too many links");
  sys_errlist[32] = _T("Broken pipe");
  sys_errlist[33] = _T("Math argument out of domain of func");
  sys_errlist[34] = _T("Math result not representable");
  sys_errlist[35] = _T("Resource deadlock would occur");
  sys_errlist[36] = _T("File name too long");
  sys_errlist[37] = _T("No record locks available");
  sys_errlist[38] = _T("Function not implemented");
  sys_errlist[39] = _T("Directory not empty");
  sys_errlist[40] = _T("Too many symbolic links encountered");
  sys_errlist[41] = _T("Operation would block");
  sys_errlist[42] = _T("No message of desired type");
  sys_errlist[43] = _T("Identifier removed");
  sys_errlist[44] = _T("Channel number out of range");
  sys_errlist[45] = _T("Level 2 not synchronized");
  sys_errlist[46] = _T("Level 3 halted");
  sys_errlist[47] = _T("Level 3 reset");
  sys_errlist[48] = _T("Link number out of range");
  sys_errlist[49] = _T("Protocol driver not attached");
  sys_errlist[50] = _T("No CSI structure available");
  sys_errlist[51] = _T("Level 2 halted");
  sys_errlist[52] = _T("Invalid exchange");
  sys_errlist[53] = _T("Invalid request descriptor");
  sys_errlist[54] = _T("Exchange ful_T(");
  sys_errlist[55] = _T("No anode");
  sys_errlist[56] = _T("Invalid request code");
  sys_errlist[57] = _T("Invalid slot");
  sys_errlist[58] = _T("File locking deadlock error");
  sys_errlist[59] = _T("Bad font file format");
  sys_errlist[60] = _T("Device not a stream");
  sys_errlist[61] = _T("No data available");
  sys_errlist[62] = _T("Timer expired");
  sys_errlist[63] = _T("Out of streams resources");
  sys_errlist[64] = _T("Machine is not on the network");
  sys_errlist[65] = _T("Package not installed");
  sys_errlist[66] = _T("Object is remote");
  sys_errlist[67] = _T("Link has been severed");
  sys_errlist[68] = _T("Advertise error");
  sys_errlist[69] = _T("Srmount error");
  sys_errlist[70] = _T("Communication error on send");
  sys_errlist[71] = _T("Protocol error");
  sys_errlist[72] = _T("Multihop attempted");
  sys_errlist[73] = _T("RFS specific error");
  sys_errlist[74] = _T("Not a data message");
  sys_errlist[75] = _T("Value too large for defined data type");
  sys_errlist[76] = _T("Name not unique on network");
  sys_errlist[77] = _T("File descriptor in bad state");
  sys_errlist[78] = _T("Remote address changed");
  sys_errlist[79] = _T("Can not access a needed shared library");
  sys_errlist[80] = _T("Accessing a corrupted shared library");
  sys_errlist[81] = _T(".lib section in a.out corrupted");
  sys_errlist[82] = _T("Attempting to link in too many shared libraries");
  sys_errlist[83] = _T("Cannot exec a shared library directly");
  sys_errlist[84] = _T("Illegal byte sequence");
  sys_errlist[85] = _T("Interrupted system call should be restarted");
  sys_errlist[86] = _T("Streams pipe error");
  sys_errlist[87] = _T("Too many users");
  sys_errlist[88] = _T("Socket operation on non-socket");
  sys_errlist[89] = _T("Destination address required");
  sys_errlist[90] = _T("Message too long");
  sys_errlist[91] = _T("Protocol wrong type for socket");
  sys_errlist[92] = _T("Protocol not available");
  sys_errlist[93] = _T("Protocol not supported");
  sys_errlist[94] = _T("Socket type not supported");
  sys_errlist[95] = _T("Operation not supported on transport endpoint");
  sys_errlist[96] = _T("Protocol family not supported");
  sys_errlist[97] = _T("Address family not supported by protoco_T(");
  sys_errlist[98] = _T("Address already in use");
  sys_errlist[99] = _T("Cannot assign requested address");
  sys_errlist[100] = _T("Network is down");
  sys_errlist[101] = _T("Network is unreachable");
  sys_errlist[102] = _T("Network dropped connection because of reset");
  sys_errlist[103] = _T("Software caused connection abort");
  sys_errlist[104] = _T("Connection reset by peer");
  sys_errlist[105] = _T("No buffer space available");
  sys_errlist[106] = _T("Transport endpoint is already connected");
  sys_errlist[107] = _T("Transport endpoint is not connected");
  sys_errlist[108] = _T("Cannot send after transport endpoint shutdown");
  sys_errlist[109] = _T("Too many references: cannot splice");
  sys_errlist[110] = _T("Connection timed out");
  sys_errlist[111] = _T("Connection refused");
  sys_errlist[112] = _T("Host is down");
  sys_errlist[113] = _T("No route to host");
  sys_errlist[114] = _T("Operation already in progress");
  sys_errlist[115] = _T("Operation now in progress");
  sys_errlist[116] = _T("Stale NFS file handle");
  sys_errlist[117] = _T("Structure needs cleaning");
  sys_errlist[118] = _T("Not a XENIX named type file");
  sys_errlist[119] = _T("No XENIX semaphores available");
  sys_errlist[120] = _T("Is a named type file");
  sys_errlist[121] = _T("Remote I/O error");
  sys_errlist[122] = _T("Quota exceeded");
  sys_errlist[123] = _T("No medium found");
  sys_errlist[124] = _T("Wrong medium type");
}



String StrError( cint err )
{
  String result;
  String S;
  if ( ( err < 0 ) || ( err >= sys_errn ) )
  {
    Str( err, S );
    result = String( _T("Unknown Error (") ) + S + _T(")");
  }
  else
    result = StrPas( sys_errlist[err] );
  return result;
}


void PError( const String& S, cint errno )
{
  Write( Stderr, S ); Write( Stderr, _T(": ") ); WriteLn( Stderr, StrError( errno ) );
}


int GetLastOSError( )
{
  return geterrno();
}

String SysErrorMessage( int ErrorCode )
{
  return StrError( ErrorCode );
}

int GetLastError()
{
  return GetLastOSError( );
}

#else
#error unknown platform
#endif





void RaiseLastOSError( )
{
  int ECode = 0;
  EOSError* e = NULL;
  ECode = GetLastOSError();
  if ( ECode != 0 ) 
//    e = new EOSError( _T(SysConst_SOSError), ARRAYOFCONST(( ECode, SysErrorMessage(ECode) )) );
    e = new EOSError( ErrorString(_T(SysConst_SOSError), IntToStr(ECode) ));
  else 
    e = new EOSError( _T(SysConst_SUnkOSError) );
  e->ErrorCode = ECode;
  throw e;
}


/******************************************************************************
                         internal Error handling
******************************************************************************/

void HandleError( int err )
/*
  procedure to Handle Internal errors, i.e. not User-invoked errors
  Internal function should always call HandleError instead of RunError.
*/
{
   //HandleErrorFrame( err, Get_frame() );
  throw runtime_error("internal error");
} 


int Exception::ReadPropertyHelpContext( ) const {
  return FHelpContext;
}
void Exception::WritePropertyHelpContext( int Value ) {
  FHelpContext = Value;
}
String Exception::ReadPropertyMessage( ) const {
  return FMessage;
}
void Exception::WritePropertyMessage( String Value ) {
  FMessage = Value;
}


Exception::Exception( const String& Msg )
{
  // inherited::Create();
  FMessage = Msg;
}

Exception::Exception( const String& Msg, const VECTOROFCONST& Args )
{
  // inherited::Create();
  FMessage = Format( Msg, Args );
}

Exception::Exception( PString ResString )
{
  // inherited::Create();
  FMessage = *ResString;
}

Exception::Exception( PString ResString, const VECTOROFCONST& Args )
{
  // inherited::Create();
  FMessage = Format( *ResString, Args );
}


Exception::Exception( const String& Msg, int AHelpContext )
{
  // inherited::Create();
  FMessage = Msg;
  FHelpContext = AHelpContext;
}


Exception::Exception( const String& Msg, const VECTOROFCONST& Args, int AHelpContext )
{
  // inherited::Create();
  FMessage = Format( Msg, Args );
  FHelpContext = AHelpContext;
}

Exception::Exception( PString ResString, int AHelpContext )
{
  // inherited::Create();
  FMessage = *ResString;
  FHelpContext = AHelpContext;
}

Exception::Exception( PString ResString, const VECTOROFCONST& Args, int AHelpContext )
{
  // inherited::Create();
  FMessage = Format( *ResString, Args );
  FHelpContext = AHelpContext;
}


void EHeapMemoryError::FreeInstance( )
{
//  if ( AllowFree ) 
//    inherited::FreeInstance();
}

const int VAR_OK = 0x00000000; // HRESULT
const int VAR_PARAMNOTFOUND = 0x80020004; // HRESULT
const int VAR_TYPEMISMATCH = 0x80020005; // HRESULT
const int VAR_BADVARTYPE = 0x80020008; // HRESULT
const int VAR_EXCEPTION = 0x80020009; // HRESULT
const int VAR_OVERFLOW = 0x8002000A; // HRESULT
const int VAR_BADINDEX = 0x8002000B; // HRESULT
const int VAR_ARRAYISLOCKED = 0x8002000D; // HRESULT
const int VAR_NOTIMPL = 0x80004001; // HRESULT
const int VAR_OUTOFMEMORY = 0x8007000e; // HRESULT
const int VAR_INVALIDARG = 0x80070057; // HRESULT
const int VAR_UNEXPECTED = 0x8000FFFF; // HRESULT

EVariantError::EVariantError( int Code )
{
  switch ( Code )
  {
    case VAR_OK:
      FMessage = _T(SysConst_SNoError);
    break;
    case VAR_PARAMNOTFOUND:
      FMessage = _T(SysConst_SVarParamNotFound);
    break;
    case VAR_TYPEMISMATCH:
      FMessage = _T(SysConst_SInvalidVarCast);
    break;
    case VAR_BADVARTYPE:
      FMessage = _T(SysConst_SVarBadType);
    break;
    case VAR_OVERFLOW:
      FMessage = _T(SysConst_SVarOverflow);
    break;
    case VAR_BADINDEX:
      FMessage = _T(SysConst_SVarArrayBounds);
    break;
    case VAR_ARRAYISLOCKED:
      FMessage = _T(SysConst_SVarArrayLocked);
    break;
    case VAR_NOTIMPL:
      FMessage = _T(SysConst_SVarNotImplemented);
    break;
    case VAR_OUTOFMEMORY:
      FMessage = _T(SysConst_SVarOutOfMemory);
    break;
    case VAR_INVALIDARG:
      FMessage = _T(SysConst_SVarInvalid);
    break;
    case VAR_UNEXPECTED: case VAR_EXCEPTION:
      FMessage = _T(SysConst_SVarUnexpected);
    break;
    default:
//      FMessage = Format( _T(SysConst_SUnknownErrorCode), OpenArray<TVarRec>(Code) );
        FMessage = _T("Unknown error code: ");
        FMessage +=  IntToStr(Code);
    break;
  }
  ErrCode = Code;
}

void ErrorHandling_initialization()
{
  //InitExceptions();       /* Initialize Exceptions. OS Independent */
  OutOfMemory = new EOutOfMemory( _T(SysConst_SOutOfMemory) );
  OutOfMemory->AllowFree = false;
  InvalidPointer = new EInvalidPointer( _T(SysConst_SInvalidPointer) );
  InvalidPointer->AllowFree = false;
  #ifdef windows
  RuntimeErrorExitCodesInit( );
  #endif
}

void ErrorHandling_finalization()
{
  OutOfMemory->AllowFree = true;
  delete OutOfMemory;
  InvalidPointer->AllowFree = true;
  delete InvalidPointer;
}

class ErrorHandling_unit
{
public:
ErrorHandling_unit()
{
  ErrorHandling_initialization();
#ifdef linux
  sys_errlistInit();
#endif
}
~ErrorHandling_unit(){ ErrorHandling_finalization(); }
};
ErrorHandling_unit _ErrorHandling_unit;

}

string ErrorString(const char* xpFormat, const string& xsValue)
{
   char buf[1024];
   snprintf( buf, 1024, xpFormat, xsValue.c_str());
   buf[1023] = 0;
   return buf;
}

wstring ErrorString(const wchar_t* xpFormat, const wstring& xsValue)
{
   wchar_t buf[1000];
   swprintf( buf, 999, xpFormat, xsValue.c_str());
   return buf;
}
