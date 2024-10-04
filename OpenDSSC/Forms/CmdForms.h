#ifndef CmdFormsH
#define CmdFormsH

/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
    08/17/2016  Created from OpenDSS
 ----------------------------------------------------------
  Copyright (c) 2016-2020 Battelle Memorial Institute
 ----------------------------------------------------------
*/


#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"

#include "ExecCommands.h"
#include "ExecOptions.h"
#include "ShowOptions.h"
#include "ExportOptions.h"
#include "DSSGlobals.h"
#include "DSSClass.h"
#include "DSSClassDefs.h"
#include "ParserDel.h"
#include "Arraydef.h"
#include "myCmdUtils.h"

//#include <classes.hpp>

extern bool ControlPanelCreated;  // signify whether this is the DLL or EXE

extern bool RebuildHelpForm;
void CreateControlPanel( );
void ExitControlPanel( );
void InitProgressForm( );
void ProgressCaption( const String S );
void ProgressFormCaption( const String S );
void ProgressHide( );
void ShowControlPanel( );
void ShowHelpForm( );
void ShowAboutBox( );
void ShowPropEditForm( );
void ShowPctProgress( int Count );
void ShowMessageForm( TStringList S );
int DSSMessageDlg( const String Msg, bool err );
void DSSInfoMessageDlg( const String Msg );
String GetDSSExeFile( );
void CloseDownForms( );
void ShowTreeView( const String Fname );
bool MakeChannelSelection( int NumFieldsToSkip, const String FileName );
void ShowHeapUsage( ); // copied from Lazarus form; not used in command line yet

#endif //  CmdFormsH

