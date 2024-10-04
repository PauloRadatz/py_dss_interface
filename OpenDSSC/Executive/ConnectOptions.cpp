

#pragma hdrstop

#include "ConnectOptions.h"
//#include "TCP_IP.h"
#include "DSSGlobals.h"
#include "ParserDel.h"
#include "Utilities.h"

using namespace std;
using namespace Command;
using namespace DSSGlobals;
using namespace ParserDel;
using namespace System;
using namespace Utilities;

namespace ConnectOptions
{


String* ConnectOption;
String* ConnectHelp;
TCommandList* ConnectCommands = nullptr;

void DefineOptions()
{
	ConnectOption			= new String[2];
	ConnectHelp				= new String[2];
	ConnectOption[1 - 1]	= "address";
	ConnectOption[2 - 1]	= "port";
	ConnectHelp[1 - 1]		= "Address is a string containing the IP address of a particular system with which OpenDSS should form a connection";
	ConnectHelp[2 - 1]		= String("Port is the ID of the desired server connection:") + CRLF
	           + "47625 = OpenDSS Viewer";
}

int DoConnectCmd()
{
	int result = 0;
	result = 0;

//    If NoFormsAllowed Then Begin Result :=1; Exit; End;
        /*
	if (!ASSIGNED(DSSConnectObj))
		DSSConnectObj = TDSSConnect.Create;
	DSSConnectObj.SetDefaults;
	with DSSConnectObj do
	{
		auto& with0 = DSSConnectObj;
		Connect;
	}
	*/
	return result;
}
//Var
//   ParamName, Param:String;
//   ParamPointer, i:Integer;

int DoDisConnectCmd()
{
	int result = 0;
	result = 0;

//    If NoFormsAllowed Then Begin Result :=1; Exit; End;
	/*
	if (ASSIGNED(DSSConnectObj))
	{
		# with DSSConnectObj do
		{
			auto& with0 = DSSConnectObj;
			Disconnect;
		}
	}
	*/
	return result;
}
//Var
//  ParamName, Param:String;
//  ParamPointer, i:Integer;

void DisposeStrings()
{
	delete[] ConnectOption;
	delete[] ConnectHelp;
}


void ConnectOptions_initialization()
{
	DefineOptions();
	ConnectCommands = new TCommandList(ConnectHelp, 2);
	ConnectCommands->set_AbbrevAllowed(true);
}

void ConnectOptions_finalization()
{
	DoDisConnectCmd();
	DisposeStrings();
	delete ConnectCommands;
}

		class 		ConnectOptions_unit
		{
		public:
		ConnectOptions_unit()
		{
			//AssertSystemInitialization();
			ConnectOptions_initialization();
		}
		~		ConnectOptions_unit(){ConnectOptions_finalization(); }
		};
		ConnectOptions_unit _ConnectOptions_unit;

}  // namespace ConnectOptions




