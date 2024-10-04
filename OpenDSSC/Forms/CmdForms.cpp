

#pragma hdrstop

#include "CmdForms.h"

#ifndef linux
  #define NOMINMAX
  #include <windows.h>
#else
  #include <unistd.h>
#endif

bool ControlPanelCreated = false;
bool RebuildHelpForm = false;

const int colwidth = 25;
const int numcols = 4;  // for listing commands to the console

void ShowHeapUsage( )
{
}


void InitProgressForm( )
{
}


void ShowPctProgress( int Count )
{
}


void ProgressCaption( const String S )
{
  CoutLn( "Progress: " ); CoutLn( S );
}


void ProgressFormCaption( const String S )
{
  CoutLn( "Progress: " ); CoutLn( S );
}


void ProgressHide( )
{
}


void ShowAboutBox( )
{
  CoutLn( "Console OpenDSS (Electric Power Distribution System Simulator)" );
  CoutLn( "Version: " + VersionString );
  CoutLn( "Copyright (c) 2008-2021, Electric Power Research Institute, Inc." );
  CoutLn( "Copyright (c) 2016-2021, Battelle Memorial Institute" );
  CoutLn( "All rights reserved." );
}

void ShowTreeView( const String Fname )
{
}


String GetDSSExeFile( )
{
  String result;
  Char TheFileName[ MAX_PATH ];
  FillChar( TheFileName, sizeof( TheFileName ), '\x00' );  // Fill it with nulls
  #ifndef linux
    GetModuleFileName( NULL, TheFileName, sizeof( TheFileName ) );
  #else
    ssize_t len = readlink("/proc/self/exe", TheFileName, sizeof(TheFileName)-1);
    if(len != -1){
      TheFileName[len] = '\0';
      return std::string(TheFileName);
    }
  #endif

  result = TheFileName;
  if ( IsLibrary )
    IsDLL = true;
  return result;
}


int DSSMessageDlg( const String Msg, bool err )
{
  int result = 0;
  result = 0;
  if ( err )
    CoutLn( "** Error: " );
  CoutLn( Msg );
  return result;
}


void DSSInfoMessageDlg( const String Msg )
{
  CoutLn( Msg );
}


void CreateControlPanel( )
{
}


void ExitControlPanel( )
{
}


void ShowControlPanel( )
{
}


int CompareClassNames( void* Item1, void* Item2 )
{
  int result = 0;
  result = CompareText( ((TDSSClass*) Item1 )->get_myClass_name(), ((TDSSClass*) Item2 )->get_myClass_name() );
  return result;
}


void AddHelpForClasses( WORD BaseClass, bool bProperties )
{
  TList HelpList;
  TDSSClass* pDSSClass;
  int i = 0, j = 0, idx = 0;
  HelpList = TList( );

  idx = ActiveDSSClass[ActiveActor]->Get_First();
  pDSSClass = (TDSSClass*) ActiveDSSObject[ActiveActor];

  pDSSClass = (TDSSClass*) DSSClassList[ActiveActor].Get_First();
  while ( pDSSClass != NULL )
  {
    if ( ( ( pDSSClass->DSSClassType & BaseClassMask ) ) == BaseClass )
      HelpList.push_back( pDSSClass );
    
    idx = ActiveDSSClass[ActiveActor]->Get_Next();
    pDSSClass = (TDSSClass*) ActiveDSSObject[ActiveActor];
    pDSSClass = (TDSSClass*) DSSClassList[ActiveActor].Get_Next();
  }
  //HelpList.Sort( &CompareClassNames );
  for ( int stop = HelpList.size(), i = 1; i <= stop; i++)
  {
    pDSSClass = (TDSSClass*) HelpList[i - 1];
    CoutLn( pDSSClass->get_myClass_name() );
    if ( bProperties == true )
      for ( int stop = pDSSClass->NumProperties, j = 1; j <= stop; j++)
      {
        CoutLn( "  " ); CoutLn( pDSSClass->PropertyName[j] ); CoutLn( ": " ); CoutLn( pDSSClass->PropertyHelp[j] );
      }
  }
  HelpList.clear();
}


void ShowGeneralHelp( )
{
  CoutLn( "This is a console-mode version of OpenDSS, available for Windows, Linux and Mac OS X" );
  CoutLn( "Enter a command at the >> prompt, followed by any required command parameters" );
  CoutLn( "Enter either a carriage return, \"exit\" or \"q(uit)\" to exit the program" );
  CoutLn( "For specific help, enter:" );
  CoutLn( "  \"help command [cmd]\" lists all executive commands, or" );
  CoutLn( "                       if [cmd] provided, details on that command" );
  CoutLn( "  \"help option [opt]\"  lists all simulator options, or" );
  CoutLn( "                       if [opt] provided, details on that option" );
  CoutLn( "  \"help show [opt]\"    lists the options to \"show\" various outputs, or" );
  CoutLn( "                       if [opt] provided, details on that output" );
  CoutLn( "  \"help export [fmt]\"  lists the options to \"export\" in various formats, or" );
  CoutLn( "                       if [fmt] provided, details on that format" );
  CoutLn( "  \"help class [cls]\"   lists the names of all available circuit model classes, or" );
  CoutLn( "                       if [cls] provided, details on that class" );
  CoutLn( "You may truncate any help topic name, which returns all matching entries" );
  CoutLn( "// begins a comment, which is ignored by the parser (including help)" );
}

void ShowAnyHelp( const int num, pStringArray cmd, pStringArray hlp, const String opt )
{
  int i = 0;
  TStringList lst;
  if ( opt.size( ) < 1 )
  {
    lst = TStringList();
    for ( int stop = num, i = 1; i <= stop; i++)
      lst.push_back( (cmd)[i] + PadRight((colwidth - (cmd)[i].size()), " ") );
//    lst.Sort;
    for ( int stop = num, i = 1; i <= stop; i++)
      if ( ( i % numcols ) == 0 )
        CoutLn( lst[i - 1] );
      else
        CoutLn( lst[i - 1] + " " );
    lst.clear();
  }
  else
  {
    for ( int stop = num, i = 1; i <= stop; i++)
    {
      if ( (LowerCase((cmd)[i]).find( opt ) ) != String::npos )
      {
        CoutLn( UpperCase( (cmd)[i]));
        CoutLn( "======================" );
        CoutLn( (hlp)[i] );
      }
    }
  }
}


void ShowClassHelp( const String opt )
{
  TDSSClass* pDSSClass;
  int i = 0, idx = 0;
  if ( opt.size( ) > 0 )
  {
    idx = ActiveDSSClass[ActiveActor]->Get_First();
    pDSSClass = (TDSSClass*) ActiveDSSObject[ActiveActor];
    pDSSClass = (TDSSClass*) DSSClassList[ActiveActor].Get_First();
    while ( pDSSClass != NULL )
    {
      if (LowerCase(pDSSClass->get_myClass_name()).find( opt ) != String::npos )
      {
        CoutLn( UpperCase( pDSSClass->get_myClass_name() ) );
        CoutLn( "======================" );
        for ( int stop = pDSSClass->NumProperties, i = 1; i <= stop; i++)
        {
          CoutLn( "  " ); CoutLn( pDSSClass->PropertyName[i] ); CoutLn( ": " ); CoutLn( pDSSClass->PropertyHelp[i] );
        }
      }
      idx = ActiveDSSClass[ActiveActor]->Get_Next();
      pDSSClass = (TDSSClass*) ActiveDSSObject[ActiveActor];
      pDSSClass = (TDSSClass*) DSSClassList[ActiveActor].Get_Next();
    }
  }
  else
  {
    CoutLn( "== Power Delivery Elements ==" );
    AddHelpForClasses( PD_ELEMENT, false );
    CoutLn( "== Power Conversion Elements ==" );
    AddHelpForClasses( PC_ELEMENT, false );
    CoutLn( "== Control Elements ==" );
    AddHelpForClasses( CTRL_ELEMENT, false );
    CoutLn( "== Metering Elements ==" );
    AddHelpForClasses( METER_ELEMENT, false );
    CoutLn( "== Supporting Elements ==" );
    AddHelpForClasses( 0, false );
    CoutLn( "== Other Elements ==" );
    AddHelpForClasses( NON_PCPD_ELEM, false );
  }
}


void ShowHelpForm( )
{
  String    Param = "", 
            OptName = "";
  String    dummy = Parser[ActiveActor]->GetNextParam();

  Param = LowerCase( Parser[ActiveActor]->MakeString_() );
  dummy = Parser[ActiveActor]->GetNextParam();
  OptName = LowerCase( Parser[ActiveActor]->MakeString_() );
  if ( Param.find("com") != String::npos)
    ShowAnyHelp( NumExecCommands, *(pStringArray*) ExecCommand, *(pStringArray*) CommandHelp, OptName );
  else
    if (Param.find( "op" ) != String::npos)
      ShowAnyHelp( NumExecOptions, *(pStringArray*) ExecOption, *(pStringArray*) OptionHelp, OptName );
    else
      if (Param.find( "sh" ) != String::npos)
        ShowAnyHelp( NumShowOptions, *(pStringArray*) ShowOption, *(pStringArray*) ShowHelp, OptName );
      else
        if (Param.find( 'e' ) != String::npos)
          ShowAnyHelp( NumExportOptions, *(pStringArray*) ExportOption, *(pStringArray*) ExportHelp, OptName );
        else
          if (Param.find( "cl" ) != String::npos)
            ShowClassHelp( OptName );
          else
            ShowGeneralHelp();
}


void ShowMessageForm( TStringList S )
{
  CoutLn( S[0]);
}


void ShowPropEditForm( )
{
}


void CloseDownForms( )
{
}


bool MakeChannelSelection( int NumFieldsToSkip, const String FileName )
{
  bool result = false;
  result = false;
  return result;
}

void CmdForms_initialization()
{
  RebuildHelpForm = true;
}

class CmdForms_unit
{
public:
CmdForms_unit()
{
  CmdForms_initialization();
}
};
CmdForms_unit _CmdForms_unit;


