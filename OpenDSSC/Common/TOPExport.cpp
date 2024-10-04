
#pragma hdrstop

#include "TOPExport.h"



#include <comobj.hpp>
#include "AnsiStrings.h"
#include <Sysutils.hpp>
#include "../Forms/CmdForms.h"
#include <activex.hpp>
#include "../Common/DSSGlobals.h"



#include <System.hpp>


TOutFile32* TOPTransferFile = NULL;
Variant TOP_Object;




TOutFile32::TOutFile32()
{
}



bool TOP_Inited = false;


void StartTop( )
{
  TOP_Object = CreateOleObject( "TOP2000.MAIN" );
  TOP_Inited = true;
}


void TOutFile32::SendToTop( )
{
  try
  {
    if ( ! TOP_Inited )
      StartTop();
    try
    {
      TOP_Object.OpenFile( TOPTransferFile->Fname );
    } /*Top has become disconnected*/
     // Oops.  Connection to TOP is not valid;
    catch(...)
    {
      try
      {
        StartTop();
        TOP_Object.OpenFile( TOPTransferFile->Fname );
      }
      catch(...)
      {
        DSSInfoMessageDlg( "Export to TOP failed.  Connection lost?" );
      }
    }
  }
  catch( EXCEPTion & E )
  {
    DSSInfoMessageDlg( "Error Connecting to TOP: " + E.Message );
  }
}

//-------------------------------------------------------------------------------------

std::string TOutFile32::get_Fname()
{
    return Fname;
}

//-------------------------------------------------------------------------------------

void TOutFile32::set_Fname(std::string S)
{
    Fname = S;
}

//-------------------------------------------------------------------------------------


void TOutFile32::Open( )
{
  AssignFile( Fout, Fname );
  Rewrite( Fout, 1 );  /*Open untyped file with a recordsize of 1 byte*/
  IOResultToException();
}


void TOutFile32::Close( )
{
  CloseFile( Fout );  /*Close the output file*/
}


void TOutFile32::WriteHeader( const double t_start, const double t_stop, const double h, const int NV, const int NI, const int NameSize, const String Title )
{
  int NumWrite = 0;
  /*# with Header do */
  {
    Header.Size = sizeof( ToutfileHdr );
    strcpy( Header.Signature, "SuperTran V1.00\x00" );
    Header.VersionMajor = 1;
    Header.VersionMinor = 1;
    Header.FBase = DefaultBaseFreq;
    Header.VBase = 1.0;
    Header.tStart = 0;
    Header.tFinish = 0;
    Header.StartTime = t_start;
    Header.StopT = t_stop;
    Header.DeltaT = h;
    Header.Nsteps = trunc( t_stop / h ) + 1;
    Header.NVoltages = NV;
    Header.NCurrents = NI;
    Header.VoltNameSize = NameSize;
    Header.CurrNameSize = NameSize;
    Header.IdxVoltNames = Header.Size;
    Header.IdxCurrentNames = Header.IdxVoltNames + Header.NVoltages * Header.VoltNameSize;
    Header.IdxData = Header.IdxCurrentNames + Header.NCurrents * Header.CurrNameSize;
    Header.IdxBaseData = 0;
//    {$IFDEF MSWINDOWS}
    SysUtils.StrCopy( Header.Title1, Title.c_str());
//    {$ELSE}
//         sysutils.StrCopy(Title1,pWidechar(Title));
//    {$ENDIF}
    Header.Title2[0] = '\x00';
    Header.Title3[0] = '\x00';
    Header.Title4[0] = '\x00';
    Header.Title5[0] = '\x00';
  }

     /* Zap the header to disk */
  BlockWrite( Fout, Header, sizeof( Header ), NumWrite );
}

void TOutFile32::WriteNames( TStringList& Vnames, TStringList& Cnames )
{
  int NumWrite = 0;
  int i = 0;
  wchar_t Buf[ 121/*# range 0..120*/ ];  //120 char buffer to hold names  + null terminator
  if ( Header.NVoltages > 0 )
    for ( int stop = Vnames.count - 1, i = 0; i <= stop; i++)
    {
      SysUtils.StrCopy( Buf, ((AnsiString) Vnames.Strings[i] ).c_str());    // Assign string to a buffer
      BlockWrite( Fout, Buf, Header.VoltNameSize, NumWrite );    // Strings is default property of TStrings
    }
  if ( Header.NCurrents > 0 )
    for ( int stop = Cnames.count - 1, i = 0; i <= stop; i++)
    {
      SysUtils.StrCopy( Buf, ((AnsiString) Cnames.Strings[i] ).c_str());    // Assign string to a buffer
      BlockWrite( Fout, Buf, Header.CurrNameSize, NumWrite );
    }
}


void TOutFile32::WriteData( const double t, const pDoubleArray V, const pDoubleArray Curr )
{
  int NumWrite = 0;
  BlockWrite( Fout, t, sizeof( double ), NumWrite );
  if ( Header.NVoltages > 0 )
    BlockWrite( Fout, V[1], sizeof( double ) * Header.NVoltages, NumWrite );
  if ( Header.NCurrents > 0 )
    BlockWrite( Fout, Curr[1], sizeof( double ) * Header.NCurrents, NumWrite );
}


void TOutFile32::OpenR( )  /*Open for Read Only*/
{
  AssignFile( Fout, Fname );
  Reset( Fout, 1 );
  IOResultToException();
}


void TOutFile32::ReadHeader( ) /*Opposite of WriteHeader*/
{
  int NumRead = 0;
  BlockRead( Fout, Header, sizeof( Header ), NumRead );
}

void TOutFile32::GetVoltage( pDoubleArray t, pDoubleArray V, int idx, int MaxPts ) /*Read a voltage from disk*/

/*Gets a specified voltage from an STO file for plotting.  Idx specifies the index into the voltage array*/
{
  pDoubleArray Vtemp, Ctemp;
  int i = 0;
  int NumRead = 0;
    /*Assumes V is Allocated to hold result*/
  i = 0;
  Seek( Fout, Header.IdxData );
  Getmem( Vtemp, sizeof( double ) * Header.NVoltages );
  Getmem( Ctemp, sizeof( double ) * Header.NCurrents );
  while ( ( ! Eof( Fout ) ) & ( i < MaxPts ) )
  {
    i++;
    BlockRead( Fout, t[i], sizeof( double ), NumRead );
    BlockRead( Fout, Vtemp[1], sizeof( double ) * Header.NVoltages, NumRead );
    BlockRead( Fout, Ctemp[1], sizeof( double ) * Header.NCurrents, NumRead );
    V[i] = Vtemp[idx];
  }
  Freemem( Vtemp, sizeof( double ) * Header.NVoltages );
  Freemem( Ctemp, sizeof( double ) * Header.NCurrents );
}

void TOPExport_initialization()
{
  TOP_Inited = false;
  TOPTransferFile = new TOutFile32;
  TOPTransferFile->Fname = "DSSTransfer.STO";
  CoInitialize( NULL );
}

class TOPExport_unit
{
public:
TOPExport_unit()
{
  TOPExport_initialization();
}
};
TOPExport_unit _TOPExport_unit;








