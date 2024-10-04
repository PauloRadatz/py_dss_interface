#ifndef d2c_sysexceptH
#define d2c_sysexceptH

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
#include "d2c_systobj.h"
#include "d2c_varrec.h"

#ifdef linux
#include "errno.h"
#endif



namespace System {

#ifdef linux

const int ESysEPERM = 1;    /* operation not permitted */
const int ESysENOENT = 2;    /* no such File or Directory */
const int ESysESRCH = 3;    /* no such process */
const int ESysEINTR = 4;    /* Interrupted System call */
const int ESysEIO = 5;    /* i/O Error */
const int ESysENXIO = 6;    /* no such device or Address */
const int ESysE2BIG = 7;    /* arg List Too LONG */
const int ESysENOEXEC = 8;    /* Exec Format Error */
const int ESysEBADF = 9;    /* bad File number */
const int ESysECHILD = 10;   /* no Child processes */
const int ESysEAGAIN = 11;   /* try again */
const int ESysENOMEM = 12;   /* out of memory */
const int ESysEACCES = 13;   /* permission denied */
const int ESysEFAULT = 14;   /* bad Address */
const int ESysENOTBLK = 15;   /* block device required, not Posix! */
const int ESysEBUSY = 16;   /* device or Resource busy */
const int ESysEEXIST = 17;   /* File exists */
const int ESysEXDEV = 18;   /* Cross-device link */
const int ESysENODEV = 19;   /* no such device */
const int ESysENOTDIR = 20;   /* not A Directory */
const int ESysEISDIR = 21;   /* is A Directory */
const int ESysEINVAL = 22;   /* Invalid argument */
const int ESysENFILE = 23;   /* File Table overflow */
const int ESysEMFILE = 24;   /* Too many Open files */
const int ESysENOTTY = 25;   /* not A typewriter */
const int ESysETXTBSY = 26;   /* Text File busy. the New process was
                            A PURE procedure (shared Text) File which was
                            Open for writing by another process, or File
                            which was Open for writing by another process,
                            or While the PURE procedure File was being
                            executed an Open(2) call Requested write access
                            Requested write access.*/
const int ESysEFBIG = 27;   /* File Too large */
const int ESysENOSPC = 28;   /* no space Left ON device */
const int ESysESPIPE = 29;   /* Illegal seek */
const int ESysEROFS = 30;   /* read-only File System */
const int ESysEMLINK = 31;   /* Too many links */
const int ESysEPIPE = 32;   /* broken Pipe */
const int ESysEDOM = 33;   /* math argument out of domain of Func */
const int ESysERANGE = 34;   /* math result not representable */
const int ESysEDEADLK = 35;   /* Resource deadlock would occur */
const int ESysENAMETOOLONG = 36;   /* File Name Too LONG */
const int ESysENOLCK = 37;   /* no record locks available */
const int ESysENOSYS = 38;   /* function not implemented */
const int ESysENOTEMPTY = 39;      /* Directory not empty */
const int ESysELOOP = 40;   /* Too many symbolic links encountered */
const int ESysEWOULDBLOCK = ESysEAGAIN;   /* operation would block */
const int ESysENOMSG = 42;   /* no Message of desired type */
const int ESysEIDRM = 43;   /* Identifier removed */
const int ESysECHRNG = 44;   /* channel number out of Range */
const int ESysEL2NSYNC = 45;       /* Level 2 not synchronized */
const int ESysEL3HLT = 46;   /* Level 3 halted */
const int ESysEL3RST = 47;   /* Level 3 Reset */
const int ESysELNRNG = 48;   /* link number out of Range */
const int ESysEUNATCH = 49;   /* protocol driver not attached */
const int ESysENOCSI = 50;   /* no CSI structure available */
const int ESysEL2HLT = 51;   /* Level 2 halted */
const int ESysEBADE = 52;   /* Invalid Exchange */
const int ESysEBADR = 53;   /* Invalid request Descriptor */
const int ESysEXFULL = 54;   /* Exchange full */
const int ESysENOANO = 55;   /* no ANode */
const int ESysEBADRQC = 56;   /* Invalid request Code */
const int ESysEBADSLT = 57;   /* Invalid slot */
const int ESysEDEADLOCK = 58;      /* File Locking deadlock Error */
const int ESysEBFONT = 59;   /* bad Font File Format */
const int ESysENOSTR = 60;   /* device not A Stream */
const int ESysENODATA = 61;   /* no Data available */
const int ESysETIME = 62;   /* timer expired */
const int ESysENOSR = 63;   /* out of streams resources */
const int ESysENONET = 64;   /* Machine is not ON the network */
const int ESysENOPKG = 65;   /* package not installed */
const int ESysEREMOTE = 66;   /* object is remote */
const int ESysENOLINK = 67;   /* link has been severed */
const int ESysEADV = 68;   /* advertise Error */
const int ESysESRMNT = 69;   /* Srmount Error */
const int ESysECOMM = 70;   /* communication Error ON send */
const int ESysEPROTO = 71;   /* protocol Error */
const int ESysEMULTIHOP = 72;      /* Multihop attempted */
const int ESysEDOTDOT = 73;   /* RFS specific Error */
const int ESysEBADMSG = 74;   /* not A Data Message */
const int ESysEOVERFLOW = 75;      /* Value Too large for defined Data type */
const int ESysENOTUNIQ = 76;       /* Name not unique ON network */
const int ESysEBADFD = 77;   /* File Descriptor in bad State */
const int ESysEREMCHG = 78;   /* remote Address changed */
const int ESysELIBACC = 79;   /* can not access A needed shared Library */
const int ESysELIBBAD = 80;   /* accessing A corrupted shared Library */
const int ESysELIBSCN = 81;   /* .lib section in A.out corrupted */
const int ESysELIBMAX = 82;   /* Attempting to link in Too many shared Libraries */
const int ESysELIBEXEC = 83;       /* cannot Exec A shared Library directly */
const int ESysEILSEQ = 84;   /* Illegal Byte sequence */
const int ESysERESTART = 85;       /* Interrupted System call should be restarted */
const int ESysESTRPIPE = 86;       /* streams Pipe Error */
const int ESysEUSERS = 87;   /* Too many users */
const int EsysENOTSOCK = 88;       /* socket operation ON non-socket */
const int ESysEDESTADDRREQ = 89;   /* Destination Address required */
const int ESysEMSGSIZE = 90;       /* Message Too LONG */
const int ESysEPROTOTYPE = 91;     /* protocol wrong type for socket */
const int ESysENOPROTOOPT = 92;    /* protocol not available */
const int ESysEPROTONOSUPPORT = 93;        /* protocol not supported */
const int ESysESOCKTNOSUPPORT = 94;        /* socket type not supported */
const int ESysEOPNOTSUPP = 95;     /* operation not supported ON transport endpoint */
const int ESysEPFNOSUPPORT = 96;   /* protocol family not supported */
const int ESysEAFNOSUPPORT = 97;   /* Address family not supported by protocol */
const int ESysEADDRINUSE = 98;     /* Address Already in use */
const int ESysEADDRNOTAVAIL = 99;  /* cannot Assign Requested Address */
const int ESysENETDOWN = 100;      /* network is down */
const int ESysENETUNREACH = 101;   /* network is unreachable */
const int ESysENETRESET = 102;     /* network dropped connection because of Reset */
const int ESysECONNABORTED = 103;  /* Software caused connection Abort */
const int ESysECONNRESET = 104;    /* connection Reset by peer */
const int ESysENOBUFS = 105;  /* no Buffer space available */
const int ESysEISCONN = 106;  /* transport endpoint is Already connected */
const int ESysENOTCONN = 107;      /* transport endpoint is not connected */
const int ESysESHUTDOWN = 108;     /* cannot send After transport endpoint shutdown */
const int ESysETOOMANYREFS = 109;  /* Too many references: cannot SPLICE */
const int ESysETIMEDOUT = 110;     /* connection timed out */
const int ESysECONNREFUSED = 111;  /* connection refused */
const int ESysEHOSTDOWN = 112;     /* host is down */
const int ESysEHOSTUNREACH = 113;  /* no route to host */
const int ESysEALREADY = 114;      /* operation Already in progress */
const int ESysEINPROGRESS = 115;   /* operation Now in progress */
const int ESysESTALE = 116;  /* Stale NFS File Handle */
const int ESysEUCLEAN = 117;  /* structure needs cleaning */
const int ESysENOTNAM = 118;  /* not A XENIX Named type File */
const int ESysENAVAIL = 119;  /* no XENIX semaphores available */
const int ESysEISNAM = 120;  /* is A Named type File */
const int ESysEREMOTEIO = 121;     /* remote i/O Error */
const int ESysEDQUOT = 122;  /* quota exceeded */                           // Error numbers

int geterrno( );
void seterrno( int err );
WORD PosixToRunError( int PosixErrno );

const int sys_errn = 125;
extern const Char* sys_errlist [ 125/*# range 0..sys_errn-1*/ ];                   /* EMEDIUMTYPE */ // BSD or LINUX ones

String StrError( cint err );
void PError( const String& S, cint errno );

int GetLastError();


#endif

extern int ExitCode;
const int maxExitCode = 65535;
typedef void ( * TOnShowException )( Char* Msg );
extern TOnShowException OnShowException;


void RunError( WORD W ); 
void RaiseLastOSError( ); 
void HandleError( int err ); 

void Halt( WORD errnum );
void Halt( );

int ExceptionErrorMessage( System::TObject* ExceptObject, void* ExceptAddr, Char* Buffer, int Size );
void ShowException( TObject* ExceptObject, void* ExceptAddr );
void Abort( );
void OutOfMemoryError( );

String SysErrorMessage( int ErrorCode );

   /* Exceptions */

class Exception: public System::TObject {
  typedef System::TObject inherited;
  friend class EAbort;
  friend class EAbstractError;
  friend class EAccessViolation;
  friend class EAssertionFailed;
  friend class EBusError;
  friend class EControlC;
  friend class EConvertError;
  friend class EDivByZero;
  friend class EExternal;
  friend class EExternalException;
  friend class EFormatError;
  friend class EInOutError;
  friend class EIntError;
  friend class EIntOverflow;
  friend class EIntfCastError;
  friend class EInvalidCast;
  friend class EInvalidContainer;
  friend class EInvalidInsert;
  friend class EInvalidOp;
  friend class EInvalidPointer;
  friend class EMathError;
  friend class ENoThreadSupport;
  friend class ENoWideStringSupport;
  friend class EOSError;
  friend class EOutOfMemory;
  friend class EOverflow;
  friend class EPackageError;
  friend class EPrivilege;
  friend class EPropReadOnly;
  friend class EPropWriteOnly;
  friend class ERangeError;
  friend class ESafecallException;
  friend class EStackOverflow;
  friend class EUnderflow;
  friend class EVariantError;
  friend class EWin32Error;
  friend class EZeroDivide;
  friend class TMultiReadExclusiveWriteSynchronizer;
  private:
  String FMessage;
  int FHelpContext = 0;
  public:
  Exception( const String& Msg );
  Exception( const String& Msg, const VECTOROFCONST& Args );
  Exception( PString ResString );
  Exception( PString ResString, const VECTOROFCONST& Args );
  Exception( const String& Msg, int AHelpContext );
  Exception( const String& Msg, const VECTOROFCONST& Args, int AHelpContext );
  Exception( PString ResString, int AHelpContext );
  Exception( PString ResString, const VECTOROFCONST& Args, int AHelpContext );
  
      /* !!!! */
  /*# __property int HelpContext */
  virtual int ReadPropertyHelpContext( ) const;
  virtual void WritePropertyHelpContext( int Value );
  /*# __property String Message */
  virtual String ReadPropertyMessage( ) const;
  virtual void WritePropertyMessage( String Value );
  //public: 
  inline Exception () {}
};


typedef System::TMetaClass* /*# Exception */ ExceptClass;


class EExternal: public Exception {
  typedef Exception inherited;
  public:
  public: inline EExternal( const String Msg ) : inherited( Msg ) {}
  public: inline EExternal( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EExternal( PString ResString ) : inherited( ResString ) {}
  public: inline EExternal( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EExternal( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EExternal( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EExternal( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EExternal( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};

   /* Integer math Exceptions */


class EIntError: public EExternal {
  typedef EExternal inherited;
  public: inline EIntError( const String Msg ) : inherited( Msg ) {}
  public: inline EIntError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EIntError( PString ResString ) : inherited( ResString ) {}
  public: inline EIntError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EIntError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EIntError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EIntError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EIntError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class EDivByZero: public EIntError {
  typedef EIntError inherited;
  public: inline EDivByZero( const String Msg ) : inherited( Msg ) {}
  public: inline EDivByZero( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EDivByZero( PString ResString ) : inherited( ResString ) {}
  public: inline EDivByZero( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EDivByZero( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EDivByZero( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EDivByZero( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EDivByZero( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class ERangeError: public EIntError {
  typedef EIntError inherited;
  public: inline ERangeError( const String Msg ) : inherited( Msg ) {}
  public: inline ERangeError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline ERangeError( PString ResString ) : inherited( ResString ) {}
  public: inline ERangeError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline ERangeError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline ERangeError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline ERangeError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline ERangeError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class EIntOverflow: public EIntError {
  typedef EIntError inherited;
  public: inline EIntOverflow( const String Msg ) : inherited( Msg ) {}
  public: inline EIntOverflow( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EIntOverflow( PString ResString ) : inherited( ResString ) {}
  public: inline EIntOverflow( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EIntOverflow( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EIntOverflow( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EIntOverflow( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EIntOverflow( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };

   /* general math errors */


class EMathError: public EExternal {
  typedef EExternal inherited;
  public: inline EMathError( const String Msg ) : inherited( Msg ) {}
  public: inline EMathError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EMathError( PString ResString ) : inherited( ResString ) {}
  public: inline EMathError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EMathError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EMathError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EMathError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EMathError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EInvalidOp: public EMathError {
  typedef EMathError inherited;
  public: inline EInvalidOp( const String Msg ) : inherited( Msg ) {}
  public: inline EInvalidOp( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EInvalidOp( PString ResString ) : inherited( ResString ) {}
  public: inline EInvalidOp( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EInvalidOp( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EInvalidOp( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EInvalidOp( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EInvalidOp( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EZeroDivide: public EMathError {
  typedef EMathError inherited;
  public: inline EZeroDivide( const String Msg ) : inherited( Msg ) {}
  public: inline EZeroDivide( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EZeroDivide( PString ResString ) : inherited( ResString ) {}
  public: inline EZeroDivide( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EZeroDivide( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EZeroDivide( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EZeroDivide( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EZeroDivide( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EOverflow: public EMathError {
  typedef EMathError inherited;
  public: inline EOverflow( const String Msg ) : inherited( Msg ) {}
  public: inline EOverflow( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EOverflow( PString ResString ) : inherited( ResString ) {}
  public: inline EOverflow( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EOverflow( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EOverflow( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EOverflow( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EOverflow( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EUnderflow: public EMathError {
  typedef EMathError inherited;
  public: inline EUnderflow( const String Msg ) : inherited( Msg ) {}
  public: inline EUnderflow( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EUnderflow( PString ResString ) : inherited( ResString ) {}
  public: inline EUnderflow( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EUnderflow( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EUnderflow( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EUnderflow( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EUnderflow( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };

   /* Run-time and i/O errors */


class EInOutError: public Exception {
  typedef Exception inherited;
  public:
  int ErrorCode = 0;
  inline EInOutError( const String Msg ) : inherited( Msg ) {}
  inline EInOutError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  inline EInOutError( PString ResString ) : inherited( ResString ) {}
  inline EInOutError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  inline EInOutError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  inline EInOutError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  inline EInOutError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  inline EInOutError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}

  };


class EHeapMemoryError: public Exception {
  typedef Exception inherited;
  public:
  bool AllowFree = false; // access in InitExceptions( )
  protected:
  virtual void FreeInstance( );
  public: inline EHeapMemoryError( const String Msg ) : inherited( Msg ) {}
  public: inline EHeapMemoryError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EHeapMemoryError( PString ResString ) : inherited( ResString ) {}
  public: inline EHeapMemoryError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EHeapMemoryError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EHeapMemoryError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EHeapMemoryError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EHeapMemoryError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


typedef EHeapMemoryError EHeapException;

class EExternalException: public EExternal {
  typedef EExternal inherited;
  public: inline EExternalException( const String Msg ) : inherited( Msg ) {}
  public: inline EExternalException( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EExternalException( PString ResString ) : inherited( ResString ) {}
  public: inline EExternalException( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EExternalException( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EExternalException( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EExternalException( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EExternalException( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EInvalidPointer: public EHeapMemoryError {
  typedef EHeapMemoryError inherited;
  public: inline EInvalidPointer( const String Msg ) : inherited( Msg ) {}
  public: inline EInvalidPointer( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EInvalidPointer( PString ResString ) : inherited( ResString ) {}
  public: inline EInvalidPointer( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EInvalidPointer( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EInvalidPointer( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EInvalidPointer( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EInvalidPointer( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EOutOfMemory: public EHeapMemoryError {
  typedef EHeapMemoryError inherited;
  public: inline EOutOfMemory( const String Msg ) : inherited( Msg ) {}
  public: inline EOutOfMemory( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EOutOfMemory( PString ResString ) : inherited( ResString ) {}
  public: inline EOutOfMemory( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EOutOfMemory( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EOutOfMemory( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EOutOfMemory( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EOutOfMemory( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EInvalidCast: public Exception {
  typedef Exception inherited;
  public: inline EInvalidCast( const String Msg ) : inherited( Msg ) {}
  public: inline EInvalidCast( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EInvalidCast( PString ResString ) : inherited( ResString ) {}
  public: inline EInvalidCast( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EInvalidCast( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EInvalidCast( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EInvalidCast( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EInvalidCast( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EVariantError: public Exception {
  typedef Exception inherited;
  public:
      int ErrCode = 0;
  EVariantError( int Code );
  inline EVariantError( const String Msg ) : inherited( Msg ) {}
  inline EVariantError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  inline EVariantError( PString ResString ) : inherited( ResString ) {}
  inline EVariantError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  inline EVariantError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  inline EVariantError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  inline EVariantError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  inline EVariantError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}

  };


class EAccessViolation: public EExternal {
  typedef EExternal inherited;
  public: inline EAccessViolation( const String Msg ) : inherited( Msg ) {}
  public: inline EAccessViolation( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EAccessViolation( PString ResString ) : inherited( ResString ) {}
  public: inline EAccessViolation( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EAccessViolation( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EAccessViolation( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EAccessViolation( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EAccessViolation( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EBusError: public EAccessViolation {
  typedef EAccessViolation inherited;
  public: inline EBusError( const String Msg ) : inherited( Msg ) {}
  public: inline EBusError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EBusError( PString ResString ) : inherited( ResString ) {}
  public: inline EBusError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EBusError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EBusError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EBusError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EBusError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EPrivilege: public EExternal {
  typedef EExternal inherited;
  public: inline EPrivilege( const String Msg ) : inherited( Msg ) {}
  public: inline EPrivilege( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EPrivilege( PString ResString ) : inherited( ResString ) {}
  public: inline EPrivilege( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EPrivilege( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EPrivilege( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EPrivilege( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EPrivilege( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EStackOverflow: public EExternal {
  typedef EExternal inherited;
  public: inline EStackOverflow( const String Msg ) : inherited( Msg ) {}
  public: inline EStackOverflow( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EStackOverflow( PString ResString ) : inherited( ResString ) {}
  public: inline EStackOverflow( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EStackOverflow( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EStackOverflow( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EStackOverflow( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EStackOverflow( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EControlC: public EExternal {
  typedef EExternal inherited;
  public: inline EControlC( const String Msg ) : inherited( Msg ) {}
  public: inline EControlC( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EControlC( PString ResString ) : inherited( ResString ) {}
  public: inline EControlC( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EControlC( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EControlC( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EControlC( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EControlC( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };

   /* String conversion errors */


class EConvertError: public Exception {
  typedef Exception inherited;
  public: inline EConvertError( const String Msg ) : inherited( Msg ) {}
  public: inline EConvertError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EConvertError( PString ResString ) : inherited( ResString ) {}
  public: inline EConvertError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EConvertError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EConvertError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EConvertError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EConvertError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EFormatError: public Exception {
  typedef Exception inherited;
  public: inline EFormatError( const String Msg ) : inherited( Msg ) {}
  public: inline EFormatError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EFormatError( PString ResString ) : inherited( ResString ) {}
  public: inline EFormatError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EFormatError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EFormatError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EFormatError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EFormatError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };

   /* other errors */


class EAbort: public Exception {
  typedef Exception inherited;
  public: inline EAbort( const String Msg ) : inherited( Msg ) {}
  public: inline EAbort( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EAbort( PString ResString ) : inherited( ResString ) {}
  public: inline EAbort( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EAbort( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EAbort( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EAbort( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EAbort( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EAbstractError: public Exception {
  typedef Exception inherited;
  public: inline EAbstractError( const String Msg ) : inherited( Msg ) {}
  public: inline EAbstractError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EAbstractError( PString ResString ) : inherited( ResString ) {}
  public: inline EAbstractError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EAbstractError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EAbstractError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EAbstractError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EAbstractError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
  };


class EAssertionFailed: public Exception {
  typedef Exception inherited;
  public: inline EAssertionFailed( const String Msg ) : inherited( Msg ) {}
  public: inline EAssertionFailed( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EAssertionFailed( PString ResString ) : inherited( ResString ) {}
  public: inline EAssertionFailed( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EAssertionFailed( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EAssertionFailed( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EAssertionFailed( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EAssertionFailed( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class EPropReadOnly: public Exception {
  typedef Exception inherited;
  public: inline EPropReadOnly( const String Msg ) : inherited( Msg ) {}
  public: inline EPropReadOnly( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EPropReadOnly( PString ResString ) : inherited( ResString ) {}
  public: inline EPropReadOnly( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EPropReadOnly( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EPropReadOnly( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EPropReadOnly( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EPropReadOnly( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class EPropWriteOnly: public Exception {
  typedef Exception inherited;
  public: inline EPropWriteOnly( const String Msg ) : inherited( Msg ) {}
  public: inline EPropWriteOnly( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EPropWriteOnly( PString ResString ) : inherited( ResString ) {}
  public: inline EPropWriteOnly( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EPropWriteOnly( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EPropWriteOnly( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EPropWriteOnly( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EPropWriteOnly( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class EIntfCastError: public Exception {
  typedef Exception inherited;
  public: inline EIntfCastError( const String Msg ) : inherited( Msg ) {}
  public: inline EIntfCastError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EIntfCastError( PString ResString ) : inherited( ResString ) {}
  public: inline EIntfCastError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EIntfCastError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EIntfCastError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EIntfCastError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EIntfCastError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class EInvalidContainer: public Exception {
  typedef Exception inherited;
  public: inline EInvalidContainer( const String Msg ) : inherited( Msg ) {}
  public: inline EInvalidContainer( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EInvalidContainer( PString ResString ) : inherited( ResString ) {}
  public: inline EInvalidContainer( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EInvalidContainer( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EInvalidContainer( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EInvalidContainer( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EInvalidContainer( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class EInvalidInsert: public Exception {
  typedef Exception inherited;
  public: inline EInvalidInsert( const String Msg ) : inherited( Msg ) {}
  public: inline EInvalidInsert( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EInvalidInsert( PString ResString ) : inherited( ResString ) {}
  public: inline EInvalidInsert( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EInvalidInsert( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EInvalidInsert( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EInvalidInsert( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EInvalidInsert( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class EPackageError: public Exception {
  typedef Exception inherited;
  public: inline EPackageError( const String Msg ) : inherited( Msg ) {}
  public: inline EPackageError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EPackageError( PString ResString ) : inherited( ResString ) {}
  public: inline EPackageError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EPackageError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EPackageError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EPackageError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EPackageError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class EOSError: public Exception {
  typedef Exception inherited;
  public:
  int ErrorCode = 0;
  public: inline EOSError( const String Msg ) : inherited( Msg ) {}
  public: inline EOSError( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EOSError( PString ResString ) : inherited( ResString ) {}
  public: inline EOSError( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EOSError( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EOSError( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EOSError( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EOSError( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}

};


class ESafecallException: public Exception {
  typedef Exception inherited;
  public: inline ESafecallException( const String Msg ) : inherited( Msg ) {}
  public: inline ESafecallException( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline ESafecallException( PString ResString ) : inherited( ResString ) {}
  public: inline ESafecallException( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline ESafecallException( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline ESafecallException( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline ESafecallException( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline ESafecallException( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class ENoThreadSupport: public Exception {
  typedef Exception inherited;
  public: inline ENoThreadSupport( const String Msg ) : inherited( Msg ) {}
  public: inline ENoThreadSupport( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline ENoThreadSupport( PString ResString ) : inherited( ResString ) {}
  public: inline ENoThreadSupport( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline ENoThreadSupport( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline ENoThreadSupport( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline ENoThreadSupport( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline ENoThreadSupport( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


class ENoWideStringSupport: public Exception {
  typedef Exception inherited;
  public: inline ENoWideStringSupport( const String Msg ) : inherited( Msg ) {}
  public: inline ENoWideStringSupport( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline ENoWideStringSupport( PString ResString ) : inherited( ResString ) {}
  public: inline ENoWideStringSupport( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline ENoWideStringSupport( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline ENoWideStringSupport( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline ENoWideStringSupport( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline ENoWideStringSupport( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};

class EWin32Error: public Exception {
  typedef Exception inherited;
  public:
  DWORD ErrorCode = 0;
  public: inline EWin32Error( const String Msg ) : inherited( Msg ) {}
  public: inline EWin32Error( const String Msg, const VECTOROFCONST& Args ) : inherited( Msg, Args ) {}
  public: inline EWin32Error( PString ResString ) : inherited( ResString ) {}
  public: inline EWin32Error( PString ResString, const VECTOROFCONST& Args ) : inherited( ResString, Args ) {}
  public: inline EWin32Error( const String Msg, int AHelpContext ) : inherited( Msg, AHelpContext ) {}
  public: inline EWin32Error( const String Msg, const VECTOROFCONST& Args, int AHelpContext ) : inherited( Msg, Args, AHelpContext ) {}
  public: inline EWin32Error( PString ResString, int AHelpContext ) : inherited( ResString, AHelpContext ) {}
  public: inline EWin32Error( PString ResString, const VECTOROFCONST& Args, int AHelpContext ) : inherited( ResString, Args, AHelpContext ) {}
};


}  // namespace System

std::string ErrorString(const char* xpFormat, const std::string& xsValue);
std::wstring ErrorString(const wchar_t* xpFormat, const std::wstring& xsValue);


#endif