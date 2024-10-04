// OpenDSSX.cpp : Defines the entry point for the application.
//

#pragma hdrstop

#include <iostream>
#include "OpenDSSCDLL.h"
#include <string>
#include <algorithm>
#include <stdio.h>
#include <stdlib.h>
#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"

#include "Arraydef.h"
#include "Command.h"
#include "HashList.h"
#include "PointerList.h"
#include "RPN.h"
#include "ParserDel.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "CktElement.h"
#include "CktElementClass.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Bus.h"
#include "klusolve.h"
#include "PCClass.h"
#include "PCElement.h"
#include "PDClass.h"
#include "PDElement.h"
#include "CktTree.h"
#include "Pstcalc.h"
#include "StackDef.h"
#include "Terminal.h"
#include "mathutil.h"

#include "XYcurve.h"
#include "XfmrCode.h"
#include "WireData.h"
#include "TSLineConstants.h"
#include "CNData.h"
#include "TSData.h"
#include "TempShape.h"
#include "TCC_Curve.h"
#include "Spectrum.h"
#include "PriceShape.h"
#include "OHLineConstants.h"
#include "NamedObject.h"
#include "LoadShape.h"
#include "LineSpacing.h"
#include "LineGeometry.h"
#include "LineConstants.h"
#include "LineCode.h"
#include "GrowthShape.h"
#include "ConductorData.h"
#include "CNLineConstants.h"
#include "CableData.h"
#include "CableConstants.h"

#include "LineUnits.h"
#include "Conductor.h"
#include "Line.h"
#include "Transformer.h"
#include "Capacitor.h"
#include "Reactor.h"
#include "GICTransformer.h"
#include "fuse.h"
#include "Fault.h"
#include "AutoTrans.h"

#include "WindGenVars.h"
#include "WindGen.h"
#include "WindGenUserModel.h"
#include "VSource.h"
#include "VSConverter.h"
#include "vccs.h"
#include "UPFC.h"
#include "StorageVars.h"
#include "Storage.h"
#include "StoreUserModel.h"
#include "PVsystem.h"
#include "PVSystemUserModel.h"
#include "Load.h"
#include "Isource.h"
#include "IndMach012.h"
#include "GICsource.h"
#include "GICLine.h"
#include "GenUserModel.h"
#include "Generic5OrderMach.h"
#include "GeneratorVars.h"
#include "generator.h"
#include "Equivalent.h"

#include "UPFCControl.h"
#include "SwtControl.h"
#include "StorageController.h"
#include "Relay.h"
#include "RegControl.h"
#include "Recloser.h"
#include "InvControl.h"
#include "GenDispatcher.h"
#include "ExpControl.h"
#include "ESPVLControl.h"
#include "CapControlVars.h"
#include "CapControl.h"
#include "CapUserControl.h"

#include "VLNodeVars.h"
#include "Sensor.h"
#include "ReduceAlgs.h"
#include "Monitor.h"
#include "MeterClass.h"
#include "MeterElement.h"
#include "MemoryMap_lib.h"
#include "LD_fm_infos.h"
#include "fMonitor.h"
#include "EnergyMeter.h"

#include "AutoAdd.h"
#include "Solution.h"
#include "SolutionAlgs.h"
#include "Circuit.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "ControlQueue.h"
#include "DSSCallBackRoutines.h"

#include "Dynamics.h"
#include "InvDynamics.h"
#include "EventQueue.h"
#include "ExecCommands.h"
#include "ExecHelper.h"
#include "ExecOptions.h"
#include "Executive.h"
#include "ExportCIMXML.h"
#include "ExportOptions.h"
#include "ExportResults.h"
#include "Feeder.h"
#include "IniRegSave.h"
#include "MyDSSClassDefs.h"
#include "Notes.h"
#include "ShowOptions.h"
#include "ShowResults.h"
#include "TOPExport.h"
#include "Utilities.h"
#include "YMatrix.h"
#include "ConnectOptions.h"
#include "Diakoptics.h"
#include "Sparse_Math.h"
#include "MeTIS_Exec.h"
#include "GISCommands.h"
#include "djson.h"
#include "CmdForms.h"
#include "myCmdUtils.h"
#include "myCmdUtils.cpp"

#ifdef OPENDSSX_CPP_EXTRA_HEADER
// Include another file from a parent project.
// For details, see the note about PARENT PROJECTS at the end of this file.
#include OPENDSSX_CPP_EXTRA_HEADER
#endif


//#pragma resource "*.RES"
#define MAXIDX(x) (sizeof(x)/sizeof(x[0]))-1


//class TMyApplication;

using namespace std;
using namespace DSSGlobals;



class TMyApplication {

public:
	void DoRun();
	TMyApplication();
	~TMyApplication();
	virtual string WriteLicensing();
	int GetOptionIdx(string myCMD);
	string Execute(string myCMD);
};

TMyApplication Application;

const string ExeName = "OpenDSSCMD";
int VarIdx	= 0; // Left this one here instead of DSSGlobals to avoid confusion in other files.

//--------------------------------------------------------------------------------
// Initializes OpenDSS, creates default items and implements the executive class
//--------------------------------------------------------------------------------
void TMyApplication::DoRun()
{
	NoFormsAllowed = true;
	ActiveActor = 1;
	IsDLL = true;
	DSSExecutive[ActiveActor] = new TExecutive();  // Make a DSS object
	DSSExecutive[ActiveActor]->CreateDefaultDSSItems();
	DataDirectory[ActiveActor] = StartupDirectory;
	OutputDirectory[ActiveActor] = StartupDirectory;
	SetCurrentDir(DataDirectory[ActiveActor]);
	NoFormsAllowed = false;  // messages will go to the console
}

//--------------------------------------------------------------------------------
// Executes a command in the form of a string, returns the result
//--------------------------------------------------------------------------------
string TMyApplication::Execute(string myCMD)
{
	int     CmdIdx = 0;
	string  result = "";

	CmdIdx = GetOptionIdx(myCMD);
	if (CmdIdx == 0)
	{
		DSSExecutive[ActiveActor]->Set_Command(myCMD);
		if (DSSExecutive[ActiveActor]->Get_LastError() != "")
			result = DSSExecutive[ActiveActor]->Get_LastError();
		else
			result = GlobalResult;
	}
	else
	{
		switch (CmdIdx)
		{
		case 1:
			result = "To be defined";
			break;
		case 2:
			result = "OpenDSS console " + VersionString;
			break;
		case 3: case 4:
			result = "Not implemented in Embarcadero version";
			break;
		case 5:
			result = WriteLicensing();
			break;
		case 6: case 7:
			result = "Leaving the program";
			break;
		default:
			result = "option not recognized";
		}
	}
	return result;
}


//--------------------------------------------------------------------------------
// Creates the application within the caller memory space
//--------------------------------------------------------------------------------
TMyApplication::TMyApplication()  // This only happens when the app is created
{
	//DoRun();
}

//--------------------------------------------------------------------------------
// Closes the App, nothing to do really
//--------------------------------------------------------------------------------
TMyApplication::~TMyApplication() // This only hapens when the app gets destroyed
{
	// todo check:  inherited::Destroy;
}

//--------------------------------------------------------------------------------
// Identifies special commands outside the DSS command menu, to be evaluated
//--------------------------------------------------------------------------------
int TMyApplication::GetOptionIdx(string myCMD) // Executes command or detects special characters
{
	int result = 0;
	myCMD = LowerCase(myCMD);
	result = 0;
	if ((myCMD == "-help") || (myCMD == "-h"))
		result = 1;
	if (myCMD == "-v")
		result = 2;
	if (myCMD == "-f")
		result = 3;
	if (myCMD == "-l")
		result = 4;
	if (myCMD == "-lic")
		result = 5;
	if (myCMD == "exit")
		result = 6;
	if (myCMD == "q")
		result = 7;
	return result;
}

//--------------------------------------------------------------------------------
// REturns the string for the license agreement
//--------------------------------------------------------------------------------
string TMyApplication::WriteLicensing() // returns the license agreement
{
	string result = "0";
	result = ("Copyright (c) 2008-2023, Electric Power Research Institute, Inc."
		+ CRLF + "All rights reserved."
		+ CRLF + ""
		+ CRLF + "Redistribution and use in source and binary forms, with or without"
		+ CRLF + "modification, are permitted provided that the following conditions are met:"
		+ CRLF + "    * Redistributions of source code must retain the above copyright"
		+ CRLF + "      notice, this list of conditions and the following disclaimer."
		+ CRLF + "    * Redistributions in binary form must reproduce the above copyright"
		+ CRLF + "      notice, this list of conditions and the following disclaimer in the"
		+ CRLF + "      documentation and/or other materials provided with the distribution."
		+ CRLF + "    * Neither the name of the Electric Power Research Institute, Inc., nor"
		+ CRLF + "      the names of its contributors may be used to endorse or promote"
		+ CRLF + "      products derived from this software without specific prior written"
		+ CRLF + "      permission."
		+ CRLF + ""
		+ CRLF + "THIS SOFTWARE IS PROVIDED BY Electric Power Research Institute, Inc., \"AS IS\""
		+ CRLF + "AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE"
		+ CRLF + "IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR"
		+ CRLF + "PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Electric Power Research Institute, Inc.,"
		+ CRLF + "OR ANY OTHER ENTITY CONTRIBUTING TO OR INVOLVED IN THE PROVISION OF THE SOFTWARE,"
		+ CRLF + "BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR"
		+ CRLF + "CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF"
		+ CRLF + "SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS"
		+ CRLF + "INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN"
		+ CRLF + "CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)"
		+ CRLF + "ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE"
		+ CRLF + "POSSIBILITY OF SUCH DAMAGE."
		+ CRLF);
	return result;
}

pAction ActiveAction = nullptr;
TCOMControlProxyObj* COMControlProxyObj = new TCOMControlProxyObj(nullptr,"COM_Proxy");

//*********************************************Objects******************************************************
void TCOMControlProxyObj::ClearActionList()
{
	while (PopAction())
	{
	}; // spin until it is done
}

bool TCOMControlProxyObj::PopAction()
{
	bool Result = false;

	if (ASSIGNED(ActiveAction))
	{
		delete(ActiveAction);
		ActiveAction = nullptr;
	}
	Result = true;
	if (!ActionList.empty())
	{
		ActiveAction = (TAction*) ActionList[0];
		ActionList.erase(ActionList.begin());
	}
	else
		Result = false;

	return Result;
}

TCOMControlProxyObj::TCOMControlProxyObj(DSSClass::TDSSClass* ParClass, const String COMProxyName)
{
	Set_Name(COMProxyName);
	ActionList.resize(0);
}

void TCOMControlProxyObj::DopendingAction(const int Code, int ProxyHdl, int ActorID)
{
	pAction Action = new TAction;

	Action->ActionCode = Code;
	Action->DeviceHandle = ProxyHdl;

	ActionList.push_back(Action);
}

void TCOMControlProxyObj::Reset(int ActorID)
{
	ClearActionList();
}

//******************************************************************DLL commands and interfaces**********************************************************


//****************************************************************************************************************
//--------------------------------------------------------------------------------
// Implements the DSS integer interface
//--------------------------------------------------------------------------------
int __stdcall DSSI(int mode, int arg)
{
	int result = 0;
	switch (mode)
	{
	case    0:                                  // DSS.NumCircuits
		result = (ActiveCircuit[ActiveActor] != nullptr) ? ActiveCircuit[ActiveActor]->NumCircuits : 0;
		break;
	case    1:                                  // DSS.ClearAll
		DoClearAllCmd();
		break;
	case    2:                                  // DSS.ShowPanel

		break;
	case    3:                                  // DSS.Start
		Application.DoRun();
		result = 1;
		break;
	case    4:                                  // DSS.NumClasses
		result = NumIntrinsicClasses;
		break;
	case    5:                                  // DSS.NumUserClasses
		result = NumUserClasses;
		break;
	case    6:                                  // DSS.Reset
		//TBD
		break;
	case    7:                                  // DSS.Allowforms read
		if (NoFormsAllowed)
			result = 1;
		else
			result = 0;
		break;
	case    8:                                  // DSS.Allowforms write
		if (arg == 0)
			NoFormsAllowed = 1;
		else
			NoFormsAllowed = 0;
		break;
	case 9:
		result = 1;
		break;
	default:
		result = -1;
	}
	return result;
}
//--------------------------------------------------------------------------------
// Implements the DSS String interface 
//--------------------------------------------------------------------------------
char* __stdcall DSSS(int mode, char* arg)
{
	string  result = "";
	switch (mode)
	{
	case 0:										// DSS.NewCircuit
			MakeNewCircuit(arg);
		result = "New Circuit";
		break;
	case 1:										// DSS.Version
		result = VersionString + "; license status: Open;"
#if defined(_DEBUG) || !defined(NDEBUG)
		" (DEBUG VERSION)"
#endif
		" built on " __DATE__ " " __TIME__;
		break;
	case 2:										// DSS.DataPath read
		result = DataDirectory[ActiveActor];
		break;
	case 3:										// DSS.Datapath write
		SetDataPath(arg);
		break;
	case 4:										// DSS.DefaultEditor
		result = DefaultEditor;
		break;
	default:
		result = "Error, parameter not recognized";
	}

	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return presult;
}
//--------------------------------------------------------------------------------
// Implements the DSS pointer (former variant) interface 
//--------------------------------------------------------------------------------
void __stdcall DSSV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	int		i = 0;

	switch (mode)
	{
	case 0:											// DSS.Classes
		*myType = 4;								// String
		myStrArray.resize(0);
		for (i = 1; i < NumIntrinsicClasses; i++)
		{
			WriteStr2Array(((TDSSClass*)DSSClassList[ActiveActor].Get(i))->Class_Name);
			WriteStr2Array(Char0());
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 1:											// DSS.UserClasses
		*myType = 4;								// String
		myStrArray.resize(0);
		if (NumUserClasses > 0)
		{
			for (i = NumIntrinsicClasses + 1; i <= DSSClassList[ActiveActor].get_myNumList(); i++)
			{
				WriteStr2Array(((TDSSClass*)DSSClassList[ActiveActor].Get(i))->Class_Name);
				WriteStr2Array(Char0());
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	default:
		*myType = 4;								// String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		myPtr = (uintptr_t*)&(myStrArray[0]);
	}


}

vector<string> Str2StrArray(char* myStr, char Delimiter)
{
    vector<string>	Result;
    char EvalChar	= 0x01;
    int charidx		= 0;
    string StElm	= "";

	Result.clear();
    
	while (!(EvalChar == 0))
    {
        EvalChar = myStr[charidx];
        if (!(EvalChar == 0x0A))
            StElm = StElm + EvalChar;
        else
        {
            Result.push_back(StElm);
            StElm = "";   // reset accumulator
        }
        charidx++;
    }
    Result.push_back(StElm);
    return Result;
}

//****************************************************************************************************************
//--------------------------------------------------------------------------------
// Implements the text interface for the DLL
//--------------------------------------------------------------------------------
char* __stdcall DSSPut_Command(char* myCmd)
{
    vector<string>	myCmds;
    string			result = "",
					DSSReply = "";

    myCmds = Str2StrArray(myCmd, 0x0A);
    for (int i = 0; i < myCmds.size(); i++)
    {
        DSSReply = Application.Execute(myCmds[i].c_str());
        if (!(DSSReply.empty()))
            result = result + DSSReply + char(0x0A);
    }
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}


//****************************************************************************************************************
//--------------------------------------------------------------------------------
// Implements the Lines interface for the DLL
//--------------------------------------------------------------------------------
// ******************************Line Functions************************* 
bool IsLine(TDSSCktElement* CktElem)
{
	bool	result = false;

	result = ((CktElem->DSSObjType & CLASSMASK) == LINE_ELEMENT);
	if (!result)
		DoSimpleMsg("Line Type Expected, but another found.Dss Class = " + CktElem->Get_myPName() + CRLF +
			"Element name = " + CktElem->get_Name(), 5007);
	return result;
}


//****************************************************************************************************************
//--------------------------------------------------------------------------------
// Implements the Lines interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall LinesI(int mode, int arg)
{
	TLineObj* pLine = {};
	int			result = 0;

	switch (mode)
	{
	case 0:												// Lines.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLine = (TLineObj*)ActiveCircuit[ActiveActor]->Lines.Get_First();
			if (ASSIGNED(pLine))
			{
				do
				{
					if (pLine->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLine);
						result = 1;
					}
					else
					{
						pLine = (TLineObj*)ActiveCircuit[ActiveActor]->Lines.Get_Next();
					}
				} while (!((result == 1) || (!ASSIGNED(pLine))));
			}
		}
		break;
	case 1:												// Lines.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLine = (TLineObj*)ActiveCircuit[ActiveActor]->Lines.Get_Next();
			if (ASSIGNED(pLine))
			{
				do
				{
					if (pLine->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLine);
						result = ActiveCircuit[ActiveActor]->Lines.ActiveItem;
					}
					else
					{
						pLine = (TLineObj*)ActiveCircuit[ActiveActor]->Lines.Get_Next();
					}
				} while (!((result > 0) || (!ASSIGNED(pLine))));
			}
		}
		break;
	case 2:												// Lines.Phases read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
				result = ActiveCircuit[ActiveActor]->FActiveCktElement->Get_NPhases();
		}
		break;
	case 3:												// Lines.Phases write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->Set_NPhases(arg);
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 4:												// Lines.NumCust
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				result = ((TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement)->BranchNumCustomers;
			}
		}
		break;
	case 5:												// Lines.Parent
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				pLine = ((TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement);
				if (ASSIGNED(pLine->ParentPDElement))
				{
					if (pLine->ParentPDElement->FEnabled && IsLine(pLine->ParentPDElement))
						result = ActiveCircuit[ActiveActor]->Lines.ActiveItem;
				}
			}
		}
		break;
	case 6:												// Lines.Count
		if (ActiveCircuit[ActiveActor] != nullptr)
			result = ActiveCircuit[ActiveActor]->Lines.NumInList;
		break;
	case 7:												// Lines.Units read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
				result = ((TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement)->LengthUnits;
		}
		break;
	case 8:												// Lines.Units write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				if (arg < 9)
				{
					Parser[ActiveActor]->SetCmdString(Format("units = %s", LineUnitsStr(arg).c_str()));
					with0->Edit(ActiveActor);
					with0->Set_YprimInvalid(ActiveActor, true);
				}
			}
		}
		break;
	default:
		result = -1;
		break;

	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall LinesF(int mode, double arg)
{
	int				RatingIdx = 0;
	TXYcurveObj* RSignal = nullptr;
	double			result = 0.0;

	switch (mode)
	{
	case 0:												// Lines.Length read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				result = ((TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement)->Len;
			}
		}
		break;
	case 1:												// Lines.Length write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->Len = arg;
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 2:												// Lines.R1 read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->R1 / with0->FUnitsConvert;
			}
		}
		break;
	case 3:												// Lines.R1 write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->R1 = arg * with0->FUnitsConvert;
				with0->SymComponentsChanged = true;
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 4:												// Lines.X1 read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->X1 / with0->FUnitsConvert;
			}
		}
		break;
	case 5:												// Lines.X1 write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->X1 = arg * with0->FUnitsConvert;
				with0->SymComponentsChanged = true;
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 6:												// Lines.R0 read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->R0 / with0->FUnitsConvert;
			}
		}
		break;
	case 7:												// Lines.R0 write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->R0 = arg * with0->FUnitsConvert;
				with0->SymComponentsChanged = true;
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 8:												// Lines.X0 read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->X0 / with0->FUnitsConvert;
			}
		}
		break;
	case 9:												// Lines.X0 write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->X0 = arg * with0->FUnitsConvert;
				with0->SymComponentsChanged = true;
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 10:											// Lines.C1 read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = (with0->C1 / with0->FUnitsConvert) * 1.0e9;
			}
		}
		break;
	case 11:											// Lines.C1 write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->C1 = arg * with0->FUnitsConvert * 1.0e9;
				with0->SymComponentsChanged = true;
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 12:											// Lines.C0 read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = (with0->C0 / with0->FUnitsConvert) * 1.0e9;
			}
		}
		break;
	case 13:											// Lines.C0 write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->C0 = arg * with0->FUnitsConvert * 1.0e9;
				with0->SymComponentsChanged = true;
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 14:											// Lines.NormAmps read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->NormAmps;
			}
		}
		break;
	case 15:											// Lines.NormAmps write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->NormAmps = arg;
			}
		}
		break;
	case 16:											// Lines.EmergAmps read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->EmergAmps;
			}
		}
		break;
	case 17:											// Lines.EmergAmps write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->EmergAmps = arg;
			}
		}
		break;
	case 18:											// Lines.Rg read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->Rg;
			}
		}
		break;
	case 19:											// Lines.Rg write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				Parser[ActiveActor]->SetCmdString(Format("rg = %.7g", arg));
				with0->Edit(ActiveActor);
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 20:											// Lines.Xg read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->Xg;
			}
		}
		break;
	case 21:											// Lines.Xg write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				Parser[ActiveActor]->SetCmdString(Format("xg = %.7g", arg));
				with0->Edit(ActiveActor);
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 22:											// Lines.Rho read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->rho;
			}
		}
		break;
	case 23:											// Lines.Xg write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				Parser[ActiveActor]->SetCmdString(Format("rho = %.7g", arg));
				with0->Edit(ActiveActor);
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 24:											// Lines.SeasonRating
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				auto& with1 = ActiveCircuit[ActiveActor]->Solution->DynaVars;
				result = with0->NormAmps;
				if ((!SeasonSignal.empty()) && SeasonalRating)
				{
					RSignal = (TXYcurveObj*)XYCurveClass[ActiveActor]->Find(SeasonSignal);
					if (ASSIGNED(RSignal))
						RatingIdx = trunc(RSignal->GetXValue(with1.intHour));
					// just in case
					if (RatingIdx <= with0->NumAmpRatings)
						result = with0->AmpRatings[RatingIdx];
				}
			}
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall LinesS(int mode, char* arg)
{
	TDSSCktElement* pLine = nullptr;
	int				activesave = 0;
	string			S = "",
					result = "";
	bool			Found = false;

	switch (mode)
	{
	case 0:												// Lines.Name read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				pLine = ActiveCircuit[ActiveActor]->FActiveCktElement;
				if (ASSIGNED(pLine))
					result = pLine->get_Name();
			}
		}
		break;
	case 1:												// Lines.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Lines;
			S = arg;
			activesave = with0.ActiveItem;
			pLine = (TDSSCktElement*)with0.Get_First();
			while (ASSIGNED(pLine))
			{
				if (CompareText(pLine->get_Name(), S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLine);
					Found = true;
					break;
				}
				pLine = (TDSSCktElement*)with0.Get_Next();
			}
			if (!Found)
			{
				DoSimpleMsg("Line """ + S + """ Not Found in Active Circuit.", 5008);
				pLine = (TDSSCktElement*)with0.Get(activesave);
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLine);
			}
		}
		break;
	case 2:												// Lines.Bus1 read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				pLine = ActiveCircuit[ActiveActor]->FActiveCktElement;
				if (ASSIGNED(pLine))
					result = pLine->GetBus(1);
			}
		}
		break;
	case 3:												// Lines.Bus1 write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->SetBus(1, arg);
			}
		}
		break;
	case 4:												// Lines.Bus2 read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				pLine = ActiveCircuit[ActiveActor]->FActiveCktElement;
				if (ASSIGNED(pLine))
					result = pLine->GetBus(2);
			}
		}
		break;
	case 5:												// Lines.Bus2 write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->SetBus(2, arg);
			}
		}
		break;
	case 6:												// Lines.LineCode read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->CondCode;
			}
		}
		break;
	case 7:												// Lines.LineCode write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				with0->FetchLineCode(arg);
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 8:												// Lines.Geometry read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->GeometryCode;
			}
		}
		break;
	case 9:												// Lines.Geometry write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				S = arg;
				Parser[ActiveActor]->SetCmdString("geometry=" + S);
				with0->Edit(ActiveActor);
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	case 10:											// Lines.Spacing read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = with0->SpacingCode;
			}
		}
		break;
	case 11:											// Lines.Spacing write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				S = arg;
				Parser[ActiveActor]->SetCmdString("spacing=" + S);
				with0->Edit(ActiveActor);
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		break;
	default:
		result = "Error, parameter not recognized";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall LinesV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TLineObj* LineElem = nullptr;
	complex		Ztemp = cmplx(0, 0);
	double		Factor = 0.0;
	int			i = 0,
		j = 0,
		k = 0,
		iV = 0,
		NValues = 0;
	complex* cValues = nullptr;
	double* PDouble = nullptr;

	switch (mode)
	{
	case 0:												// Lines.AllNames
		*myType = 4;				// String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->Lines.NumInList > 0)
			{
				LineElem = (TLineObj*)with0->Lines.Get_First();
				while (ASSIGNED(LineElem))
				{
					WriteStr2Array(LineElem->get_Name());
					WriteStr2Array(Char0());
					LineElem = (TLineObj*)with0->Lines.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 1:												// Lines.RMatrix read
		*myType = 2;				// Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				myDblArray.resize(sqr(with0->Get_NPhases()));
				k = 0;
				for (i = 1; i <= with0->Get_NPhases(); i++)
				{
					for (j = 1; j <= with0->Get_NPhases(); j++)
					{
						if (with0->GeometrySpecified || with0->SpacingSpecified)
							myDblArray[k] = with0->Z->GetElement(i, j).re / with0->Len;
						else
							myDblArray[k] = with0->Z->GetElement(i, j).re / with0->FUnitsConvert;
						k++;
					}
				}
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 2:												// Lines.RMatrix write
		*myType = 2;				// Double
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				PDouble = *(double**)myPtr;
				for (i = 1; i <= with0->Get_NPhases(); i++)
				{
					for (j = 1; j <= with0->Get_NPhases(); j++)
					{
						Ztemp = with0->Z->GetElement(i, j);
						with0->Z->SetElement(i, j, cmplx(*PDouble, Ztemp.im));
						k++;
						PDouble++;
					}
				}
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		*mySize = k;
		break;
	case 3:												// Lines.XMatrix read
		*myType = 2;				// Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				myDblArray.resize(sqr(with0->Get_NPhases()));
				k = 0;
				for (i = 1; i <= with0->Get_NPhases(); i++)
				{
					for (j = 1; j <= with0->Get_NPhases(); j++)
					{
						if (with0->GeometrySpecified || with0->SpacingSpecified)
							myDblArray[k] = with0->Z->GetElement(i, j).im / with0->Len;
						else
							myDblArray[k] = with0->Z->GetElement(i, j).im / with0->FUnitsConvert;
						k++;
					}
				}
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 4:												// Lines.XMatrix write
		*myType = 2;				// Double
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				PDouble = *(double**)myPtr;
				for (i = 1; i <= with0->Get_NPhases(); i++)
				{
					for (j = 1; j <= with0->Get_NPhases(); j++)
					{
						Ztemp = with0->Z->GetElement(i, j);
						with0->Z->SetElement(i, j, cmplx(Ztemp.re, *PDouble));
						k++;
						PDouble++;
					}
				}
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		*mySize = k;
		break;
	case 5:												// Lines.CMatrix read
		*myType = 2;				// Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				Factor = TwoPi * with0->BaseFrequency * 1.0e-9 * with0->FUnitsConvert;
				myDblArray.resize(sqr(with0->Get_NPhases()));
				k = 0;
				for (i = 1; i <= with0->Get_NPhases(); i++)
				{
					for (j = 1; j <= with0->Get_NPhases(); j++)
					{
						if (with0->GeometrySpecified || with0->SpacingSpecified)
							myDblArray[k] = with0->YC->GetElement(i, j).im / Factor / with0->Len;
						else
							myDblArray[k] = with0->YC->GetElement(i, j).im / Factor;
						k++;
					}
				}
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 6:												// Lines.CMatrix write
		*myType = 2;				// Double
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				Factor = TwoPi * with0->BaseFrequency * 1.0e-9;
				PDouble = *(double**)myPtr;
				for (i = 1; i <= with0->Get_NPhases(); i++)
				{
					for (j = 1; j <= with0->Get_NPhases(); j++)
					{
						with0->YC->SetElement(i, j, cmplx(0.0, (*PDouble) * Factor));
						k++;
						PDouble++;
					}
				}
				with0->Set_YprimInvalid(ActiveActor, true);
			}
		}
		*mySize = k;
		break;
	case 7:												// Lines.YPrim read
		*myType = 3;				// Complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsLine(ActiveCircuit[ActiveActor]->FActiveCktElement))
			{
				auto with0 = (TLineObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				NValues = sqr(with0->Yorder);
				cValues = with0->GetYPrimValues(ALL_YPRIM);
				if (ASSIGNED(cValues))
				{
					myCmplxArray.resize(NValues);
					for (i = 0; i < NValues; i++)
						myCmplxArray[i] = cValues[i];
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		break;
	case 8:												// Lines.YPrim write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			// {Do Nothing for now}
		}
		break;
	default:
		*myType = 4;				// String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}
//****************************************************************************************************************
//--------------------------------------------------------------------------------
// Implements the Loads interface for the DLL
//--------------------------------------------------------------------------------
// ******************************Load Functions************************* 
bool IsLoad(TDSSCktElement* CktElem)
{
	bool	result = false;

	result = ((CktElem->DSSObjType & CLASSMASK) == LOAD_ELEMENT);
	if (!result)
		DoSimpleMsg("Load Type Expected, but another found.Dss Class = " + CktElem->Get_myPName() + CRLF +
			"Element name = " + CktElem->get_Name(), 5007);
	return result;
}
TLoadObj* ActiveLoad()
{
	TLoadObj*  result = nullptr;
	if (ActiveCircuit[ActiveActor] != nullptr)
	{
		if (IsLoad(ActiveCircuit[ActiveActor]->FActiveCktElement))
		{
			result = (TLoadObj*)ActiveCircuit[ActiveActor]->FActiveCktElement;
		}
	}
	return result;
}
void Set_Parameter(string parm, string val)
{
	string mycmd = "";
	TLoadObj* pLoad = {};
	if (ActiveCircuit[ActiveActor])
	{
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (pLoad != nullptr)
		{
			SolutionAbort = false;
			auto with0 = ActiveCircuit[ActiveActor];
			mycmd = Format("load.%s.%s=%s", pLoad->get_Name().c_str(), parm.c_str(), val.c_str());
			DSSExecutive[ActiveActor]->Set_Command(mycmd);
		}
	}
}
// ******************************int type properties************************* 
int __stdcall DSSLoads(int mode, int arg)
{
	TLoadObj* pLoad = {};
	int			result = 0;

	switch (mode)
	{
	case 0:												// Load.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_First();
			if (ASSIGNED(pLoad))
			{
				do
				{
					if (pLoad->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLoad);
						result = 1;
					}
					else
					{
						pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Next();
					}
				} while (!((result == 1) || (!ASSIGNED(pLoad))));
			}
		}
		break;
	case 1:												// Load.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Next();
			if (ASSIGNED(pLoad))
			{
				do
				{
					if (pLoad->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLoad);
						result = ActiveCircuit[ActiveActor]->Loads.ActiveItem;
					}
					else
					{
						pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Next();
					}
				} while (!((result > 0) || (!ASSIGNED(pLoad))));
			}
		}
		break;
	case 2:												// Load.idx - Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->Loads.ActiveItem;
		}
		break;
	case 3:												// Load.idx - Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get(arg);
			if (ASSIGNED(pLoad))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLoad);
			}
		}
		break;
	case 4:											// Load.Count
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->Loads.NumInList;
		}
		break;
	case 5: 											// Load.Class - Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->LoadClass;
		}
		break;
	case 6: 											// Load.Class - Write
		Set_Parameter("Class", IntToStr(arg));
		break;
	case 7: 											// Load.Model - Read

		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->FLoadModel;
		}
		break;
	case 8: 											// Load.Model - Write
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			pLoad->FLoadModel = arg;
		}
		break;
	case 9: 											// Load.NumCust - Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->NumCustomers;
		}
		break;
	case 10: 											// Load.NumCust - Write
		Set_Parameter("NumCust", IntToStr(arg));
		break;
	case 11: 											// Load.Status - Read
		result = 0;
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (pLoad->ExemptFromLDCurve)
		{
			result = 2;
		}
		else if (pLoad->FIXED)
		{
			result = 1;
		}
		break;
	case 12: 											// Load.Status - Wirte
		switch (arg)
		{
		case 0:
			Set_Parameter("status", "v");
				break;
		case 1:
			Set_Parameter("status", "f");
				break;
		case 2:
			Set_Parameter("status", "e");
				break;
		}

		break;
	case 13: 											// Load.isDelta - Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			if (pLoad->Connection > 0) {
				result = 1;
			}
		}
		break;
	case 14: 											// Load.isDelta- Write
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			pLoad->Connection = Integer(arg);
		}
		break;

	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall DSSLoadsF(int mode, double arg)
{
	TLoadObj* pLoad = {};
	double		result = 0.0;

	switch (mode)
	{
	case 0:												// Load.kw Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->kWBase;
		}
		break;
	case 1:												// Load.kw Write
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			pLoad->kWBase = arg;
			pLoad->LoadSpecType = 0;
			pLoad->RecalcElementData(ActiveActor);// sets kvar based on kW and pF
		}
		break;
	case 2: 												// Load.kv Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->kVLoadBase;
		}
		break;
	case 3:												// Load.kv Write 
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			pLoad->kVLoadBase = arg;
			pLoad->UpdateVoltageBases(); // side effects
		}
		break;
	case 4: 												// Load.kvar Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->kvarBase;
		}
		break;
	case 5: 												// Load.kvar Write
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			pLoad->kvarBase = arg;
			pLoad->LoadSpecType = 1;
			pLoad->RecalcElementData(ActiveActor);// set power factor based on kW, kvar
		}
		break;
	case 6:  												// Load.pf Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->PFNominal;
		}
		break;
	case 7: 												// Load.pf Write
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			pLoad->PFNominal = arg;
			pLoad->LoadSpecType = 0;
			pLoad->RecalcElementData(ActiveActor);// set power factor based on kW, kvar
		}
		break;
	case 8:  												// Load.pctmean Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->FpuMean * 100.0;
		}
		break;
	case 9:  												// Load.pctmean Write
		if (ASSIGNED(pLoad))
		{
			Set_Parameter("%mean", FloatToStr(arg));
		}
		break;
	case 10:   												// Load.pctdev Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->FpuStdDev * 100.0;
		}
		break;
	case 11:    												// Load.pctdev write
		Set_Parameter("%stddev", FloatToStr(arg));
		break;
	case 12:   												// Load.AllocationFactor Read
	{
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
			result = pLoad->FAllocationFactor;
	}
	break;
	case 13:   												// Read.AllocationFactor Write
		Set_Parameter("AllocationFactor", FloatToStr(arg));
		break;
	case 14:    												// Load.CFactor Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->FCFactor;
		}
		break;
	case 15:    												// Load.CFactor Write
		Set_Parameter("CFactor", FloatToStr(arg));
		break;
	case 16:    												// Load.CVRWatts Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->FCVRwattFactor;
		}
		break;
	case 17:    												// Load.CVRWatts Write
		Set_Parameter("CVRWatts", FloatToStr(arg));
		break;
	case 18:    												// Load.CVRvars Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->FCVRvarFactor;
		}
		break;
	case 19:     												// Load.CVRvars Write
		Set_Parameter("CVRvars", FloatToStr(arg));
		break;
	case 20:    												// Load.kva Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->kVABase;
		}
		break;
	case 21:    												// Load.kva Write
		Set_Parameter("kva", FloatToStr(arg));
		break;
	case 22:     												// Load.kwh Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->FkWh;
		}
		break;
	case 23:      												// Load.kwh Write
		Set_Parameter("kwh", FloatToStr(arg));
		break;
	case 24:      												// Load.kwhDay Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->FkWhDays;
		}
		break;
	case 25:      												// Load.kwhDay Write
		Set_Parameter("kwhDay", FloatToStr(arg));
		break;
	case 26:     												// Load.Rneut Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->Rneut;
		}
		break;
	case 27:      												// Load.Rneut Write
		Set_Parameter("Rneut", FloatToStr(arg));
		break;
	case 28:     												// Load.Vmaxpu Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->Vmaxpu;
		}
		break;
	case 29:       												// Load.Vmaxpu Write
		Set_Parameter("Vmaxpu", FloatToStr(arg));
		break;
	case 30:      												// Load.VMinemerg  Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->VminEmerg;
		}
		break;
	case 31:        												// Load.VMinemerg Write
		Set_Parameter("VMinemerg", FloatToStr(arg));
		break;
	case 32:      												    // Load.VMinNorm   Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->VminNormal;
		}
		break;
	case 33:                                              		    // Load.VMinNorm Write
		Set_Parameter("VMinNorm", FloatToStr(arg));
		break;
	case 34:      												    // Load.VMinpu    Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->Vminpu;
		}
		break;
	case 35:                                              		    // Load.VMinpu Write
		Set_Parameter("VMinpu", FloatToStr(arg));
		break;
	case 36:       												    // Load.xfKVA     Read 
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->FConnectedkVA;
		}
		break;
	case 37:                                              		    // Load.XfKVA Write NOT SURE- CHECK
		Set_Parameter("XfKVA", FloatToStr(arg));
		break;
	case 38:       												    // Load.xneut Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->Xneut;
		}
		break;
	case 39:                                              		    // Load.Xneut Write 
		Set_Parameter("Xneut", FloatToStr(arg));
		break;
	case 40:       												    // Load.PctSeriesRL  Read 
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->puSeriesRL * 100;
		}
		break;
	case 41:														// Load.PctSeriesRL Write 
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			pLoad->puSeriesRL = arg / 100.0;
		}
		break;
	case 42:       												    // Load.RelWeight   Read
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			result = pLoad->RelWeighting;
		}
		break;
	case 43:														// Load.RelWeight Write 
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			pLoad->RelWeighting = arg;
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall DSSLoadsS(int mode, char* arg)
{
	TDSSCktElement*	pload = {};
	TLoadObj*		Load = {};
	int				ActiveSave = 0;
	String			S = arg ? arg : "";
	bool			Found = false;
	string			result = "";

	switch (mode)
	{
	case 0:												// Load.Name Read
		result ="";
		Load = ActiveLoad();
		if (Load != nullptr)
		{
			result =Load->get_Name();
		}
		break;
	case 1:												// Load.Name Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Loads;
			Found		= false;
			ActiveSave	= with0.get_myActiveItem();
			pload = (TDSSCktElement*) with0.Get_First();
			while (pload != NULL && !Found)
			{
				if (CompareText(pload->get_Name(), S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pload);
					Found = true;
					break;
				}
				pload = (TDSSCktElement*) with0.Get_Next();
			}
			if (!Found)
			{
				DoSimpleMsg("Load \"" + S + "\" Not Found in Active Circuit.", 5003);
				pload = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Loads.Get(ActiveSave);
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pload);
			}
		}
		result ="";
		break;
	case 2:												// Load.CVRCurve  Read 
		Load = (TLoadObj*) ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (Load != nullptr)
			result =Load->CVRshape;
		break;
	case 3:												// Load.CVRCurve Write 
		Set_Parameter("CVRcurve", S);
		result ="";
		break;
	case 4:												// Load.Daily Read
		Load = ActiveLoad();
		if (Load != nullptr)
			result =Load->DailyShape;
		break;
	case 5:												// Load.Daily Write
		Set_Parameter("Daily", S);
		result ="";
		break;
	case 6:												// Load.Duty Read
		Load = ActiveLoad();
		if (Load != nullptr)
			result =Load->DutyShape;
		break;
	case 7:												// Load.Duty Write
		Set_Parameter("Duty",S);
		result ="";
		break;
	case 8:												// Load.spectrum Read
		Load = ActiveLoad();
		if (Load != nullptr)
			result =Load->Spectrum;
		break;
	case 9:												// Load.spectrum Write
		Set_Parameter("Spectrum", S);
		break;
	case 10:												// Load.yearly Read
		Load = ActiveLoad();
		if (Load != nullptr)
			result =Load->YearlyShape;
		break;
	case 11:												// Load.yearly Write
		Set_Parameter("Yearly", S);
		break;
	case 12:												// Load.Growth Read
		Load = ActiveLoad();
		if (Load != nullptr)
			result =Load->GrowthShape;
		break;
	case 13:												// Load.Growth Write		
		Set_Parameter("Growth", S);
		result ="";
		break;
	case 14:												// Load.sensor Read
		result ="";
		pload = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (pload != nullptr && pload->HasSensorObj)
			result =Load->SensorObj->ElementName;
		break;
	default:
		result ="Error";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall DSSLoadsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{

	TLoadObj*	pLoad = {};
	int			k = 0,
				i = 0,
				LoopLimit = 0;
	double*		pDouble = {};
	switch (mode)
	{
	case 0:												// Loads.AllNames
		*myType = 4;				// String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->Loads.NumInList > 0)
			{
				pLoad = (TLoadObj*)with0->Loads.Get_First();
				while (ASSIGNED(pLoad))
				{
					WriteStr2Array(pLoad->get_Name());
					WriteStr2Array(Char0());
					pLoad = (TLoadObj*)with0->Loads.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 1:												//  Loads.ZIPV - Read
		*myType = 2;				// Double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		if (ASSIGNED(pLoad))
		{
			if (pLoad->FnZIPV >= 1)
			{
				myDblArray.resize(pLoad->FnZIPV);
				for (k = 0; k <= (pLoad->FnZIPV - 1); k++)
					myDblArray[k] = pLoad->ZIPV[k];
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);

		break;
	case 2:												//  Loads.ZIPV - Write
		pLoad = (TLoadObj*)ActiveCircuit[ActiveActor]->Loads.Get_Active();
		*myType = 2;				// Double
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLoad->SetZIPVSize(7);
			if (*mySize > 7)
				LoopLimit = 6;
			else
				LoopLimit = *mySize - 1;
			pDouble = *(double**)myPtr;
			for (i = 0; i <= LoopLimit; i++)
			{
				pLoad->ZIPV[k] = *pDouble;
				k++;
				pDouble++;
			}
		}
		*mySize = k;
		break;

	default:
		*myType = 4;				// String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;

	}

}

//****************************************************************************************************************
//--------------------------------------------------------------------------------
// Implements the Capacitors interface for the DLL
//--------------------------------------------------------------------------------
TCapacitorObj* __stdcall ActiveCapacitor()
{
	TCapacitorObj* result = nullptr;
	if (ActiveCircuit[ActiveActor] != nullptr)
	{
		result = (TCapacitorObj*)ActiveCircuit[ActiveActor]->ShuntCapacitors.Get_Active();
	}
	return result;
}

void Set_Parameter_Capacitor(string parm, string val)
{
	string mycmd = "";
	if (!ActiveCircuit[ActiveActor])
		return;
	SolutionAbort = false;
	auto with0 = ActiveCircuit[ActiveActor];
	mycmd = Format("capacitor.%s.%s=%s", ActiveCapacitor()->get_Name().c_str(), parm.c_str(), val.c_str());
	DSSExecutive[ActiveActor]->Set_Command(mycmd);
}

// ****************************** int type properties************************* 
int __stdcall CapacitorsI(int mode, int arg)
{
	TCapacitorObj* elem;
	TPointerList* lst = nullptr;
	int i;
	int result = 0;
	switch (mode)
	{
	case    0:					// Capacitors.NumSteps read
		elem = ActiveCapacitor();
		if (elem != nullptr)
			result = elem->Get_FNumSteps();
		break;
	case    1:					// Capacitors.NumSteps write
		Set_Parameter_Capacitor("numsteps", std::to_string(arg));
		break;
	case    2:					// Capacitors.IsDelta read
		elem = ActiveCapacitor();
		if (elem != nullptr)
		{
			if (elem->Connection > 0)
				result = 1;
		}
		break;
	case	3:					// Capacitors.IsDelta write
		elem = ActiveCapacitor();
		if (elem != nullptr)
			elem->Connection = static_cast<Integer>(arg);
		break;
	case	4:					// Capacitors.First
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &ActiveCircuit[ActiveActor]->ShuntCapacitors;
			elem = (TCapacitorObj*)lst->Get_First();
			while (elem != nullptr)
			{
				if (elem->Get_Enabled())
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
					result = 1;
					break;
				}
				else
					elem = (TCapacitorObj*)lst->Get_Next();
			}
		}
		break;
	case	5:					// Capacitors.Next
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &ActiveCircuit[ActiveActor]->ShuntCapacitors;
			elem = (TCapacitorObj*)lst->Get_Next();
			while (elem != nullptr)
			{
				if (elem->Get_Enabled())
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
					result = lst->get_myActiveItem();
					break;
				}
				else
					elem = (TCapacitorObj*)lst->Get_Next();
			}
		}
		break;
	case	6:					// Capacitors.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
			result = ActiveCircuit[ActiveActor]->ShuntCapacitors.get_myNumList();
		break;
	case	7:					// Capacitors.AddStep
		elem = ActiveCapacitor();
		if (elem != nullptr)
		{
			if (elem->AddStep(ActiveActor))
				result = 1;
		}
		break;
	case	8:					// Capacitors.SubtractStep
		elem = ActiveCapacitor();
		if (elem != nullptr)
		{
			if (elem->SubtractStep(ActiveActor))
				result = 1;
		}
		break;
	case	9:					// Capacitors.AvailableSteps
		elem = ActiveCapacitor();
		if (elem != nullptr)
			result = elem->AvailableSteps();
		break;
	case	10:					// Capacitors.Open
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = ActiveCapacitor();
			if (elem != nullptr)
			{
				for (i = 1; i <= elem->Get_FNumSteps(); i++)
					elem->set_States(i, ActiveActor, 0);
			}
		}
		break;
	case	11:					// Capacitors.Close
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = ActiveCapacitor();
			if (elem != nullptr)
			{
				elem->ActiveTerminal = &(elem->Terminals[0]);		// make sure terminal 1 is closed
				elem->Set_ConductorClosed(0, ActiveActor, true);	// closes all phases
				for (i = 1; i <= elem->Get_FNumSteps(); i++)
					elem->set_States(i, ActiveActor, 1);
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall CapacitorsF(int mode, double arg)
{
	TCapacitorObj* elem;
	double result = 0.0;		// Default return value
	switch (mode)
	{
	case    0:					// Capacitors.kV read
		result = 0.0;
		elem = ActiveCapacitor();
		if (elem != nullptr)
			result = elem->Get_kvrating();
		break;
	case	1:					// Capacitors.kV write
		Set_Parameter_Capacitor("kv", std::to_string(arg));
		break;
	case	2:					// Capacitors.kvar read
		result = 0.0;
		elem = ActiveCapacitor();
		if (elem != nullptr)
			result = elem->Get_Ftotalkvar();
		break;
	case	3:					// Capacitors.kvar write
		Set_Parameter_Capacitor("kvar", std::to_string(arg));
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//*******************************String type properties***************************
const char* __stdcall CapacitorsS(int mode, const char* arg)
{
	TCapacitorObj*	elem = nullptr;
	int				ActiveSave = 0;
	bool			Found = false;
	TPointerList* lst = nullptr;
	int				k = 0;
	string			S = "",
					result = "0";

	switch (mode)
	{
	case	0:					// Capacitors.Name read
		result = "";
		elem = ActiveCapacitor();
		if (elem != nullptr)
			result = elem->get_Name().c_str();
		break;
	case 1:						// Capacitors.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &ActiveCircuit[ActiveActor]->ShuntCapacitors;
			S = arg;
			Found = false;
			ActiveSave = lst->get_myActiveItem();
			elem = (TCapacitorObj*) lst->Get_First();
			while (elem != nullptr)
			{
                if (CompareText(elem->get_Name(), S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
					Found = true;
					break;
				}
				elem = (TCapacitorObj*)lst->Get_Next();
			}
			if (!Found)
			{
				DoSimpleMsg("Capacitor \"" + S + "\" Not Found in Active Circuit.", 5003);
				elem = (TCapacitorObj*)lst->Get(ActiveSave);
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
			}
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall CapacitorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TCapacitorObj*	elem = nullptr;
	int				ActiveSave = 0;
	string			S = "";
	bool			Found = false;
	TPointerList* lst = nullptr;
	int				k = 0,
					i = 0,
					LoopLimit = 0;
	int*			Pint = nullptr;

	switch (mode)
	{
	case	0:					// Capacitors.AllNames
		myStrArray.resize(0);
		*mySize = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->ShuntCapacitors.get_myNumList() > 0)
			{
				myStrArray.resize(0);
				lst = &with0->ShuntCapacitors;
				k = 0;
				elem = (TCapacitorObj*)lst->Get_First();
				while (elem != nullptr)
				{
					S = elem->get_Name();
					WriteStr2Array(S);
					WriteStr2Array(Char0());
					elem = (TCapacitorObj*) lst->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myType = 4;					//String
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case	1:					//Capacitors.States read
		myIntArray.resize(1);
		myIntArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = ActiveCapacitor();
			if (elem != nullptr)
			{
				myIntArray.resize(elem->Get_FNumSteps());
				k = 0;
				for (i = 1; i <= elem->Get_FNumSteps(); i++)
				{
					myIntArray[k] = elem->get_States(i, ActiveActor);
					k++;
				}
			}
		}
		*myType = 1; // Integer
		*mySize = 4 * (elem->Get_FNumSteps());
		*myPtr = (uintptr_t)(void*)&(myIntArray[0]);
		break;
	case	2:					// Capacitors.States write
		elem = ActiveCapacitor();
		if (elem != nullptr)
		{
			// allocate space based on present value of NumSteps
			// setting NumSteps allocates the memory
			// only put as many elements as proviced up to nZIPV
			// myIntArray   :=  myPointer;
			k = 0;
			Pint = *(int**)myPtr;
			for (i = 1; i <= elem->Get_FNumSteps(); i++)
			{
				elem->set_States(i, ActiveActor, *Pint);
				Pint++;
			}
			elem->FindLastStepInService();
		}
		*myType = 1;					//Integer
		break;
	default:
		myStrArray.resize(0);
		WriteStr2Array("Parameter not identified");
		WriteStr2Array(Char0());
		*myType = 4;					//String
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = 0;
		break;

	}
}

//****************************************************************************************************************
//--------------------------------------------------------------------------------
// Implements the ActiveClass interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall ActiveClassI(int mode, int arg)
{
	int result = 0;
	switch (mode)
	{
	case 0:												// ActiveClass.First
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr && ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveDSSClass[ActiveActor]->Get_First(); // sets active objects
		}
		break;
	case 1:												// ActiveClass.Next 
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr && ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveDSSClass[ActiveActor]->Get_Next(); // sets active objects
		}
		break;
	case 2:												// ActiveClass.NumElements 
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveDSSClass[ActiveActor]->Get_ElementCount();
		}
		break;
	case 3:												// ActiveClass.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveDSSClass[ActiveActor]->Get_ElementCount();
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall ActiveClassS(int mode, char* arg)
{
	TDSSObject* pelem	= nullptr;
	string		result	= "";

	switch (mode)
	{
	case 0:												// ActiveClass.Name Read
		if (ASSIGNED(ActiveDSSObject[ActiveActor]))
		{
			result = ((TDSSCktElement*)ActiveDSSObject[ActiveActor])->get_Name();
		}
		break;
	case 1:												// ActiveClass.Name Write
		if (ASSIGNED(ActiveDSSClass[ActiveActor]))
		{
			pelem = (TDSSObject*) ActiveDSSClass[ActiveActor]->Find(string(arg));
			if (pelem != nullptr)
			{
				if (dynamic_cast<TDSSCktElement*>(pelem) != nullptr) //ASK
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)pelem); //sets ActiveDSSobject
				}
				else
					ActiveDSSObject[ActiveActor] = pelem;
			}
		}
		break;
	case 2:											// ActiveClass.ActiveClassName 
		if (ASSIGNED(ActiveDSSClass[ActiveActor]))
		{
			result = ActiveDSSClass[ActiveActor]->Class_Name;
		}
		else
			result = "";
		break;
	case 3:											// ActiveClass.ActiveClassParent 
		if (ASSIGNED(ActiveDSSClass[ActiveActor]))
		{
			result = "Generic Object";
			if (dynamic_cast<TPCClass*>(ActiveDSSClass[ActiveActor]) != nullptr)
				result = "TPCClass";
			if (dynamic_cast<TPDClass*>(ActiveDSSClass[ActiveActor]) != nullptr)
				result = "TPDClass";
			if (dynamic_cast<TMeterClass*>(ActiveDSSClass[ActiveActor]) != nullptr)
				result = "TMeterClass";
			if (dynamic_cast<TControlClass*>(ActiveDSSClass[ActiveActor]) != nullptr)
				result = "TControlClass";
		}
		else
			result = "Parent Class unknonwn";
		break;
	default:
		result = "Error, parameter not recognized";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall ActiveClassV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	int idx = 0,
		i = 0;
	string S = "";
	switch (mode)
	{
	case 0:
		myStrArray.resize(0);
		*mySize = 0;

		if (ActiveCircuit[ActiveActor] != nullptr && ASSIGNED(ActiveDSSClass[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			myStrArray.resize(0);
			idx = ActiveDSSClass[ActiveActor]->Get_First();
			while (idx > 0)
			{
				S = ((TDSSObject*)ActiveDSSObject[ActiveActor])->LName;
				WriteStr2Array(S);
				WriteStr2Array(Char0());
				idx = ActiveDSSClass[ActiveActor]->Get_Next();
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myType = 4; // String
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;

	default:
		myStrArray.resize(0);
		WriteStr2Array("Parameter not identified");
		WriteStr2Array(Char0());
		*myType = 4;					//String
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*myType = -1;
		break;
	}

}

//****************************************************************************************************************
//--------------------------------------------------------------------------------
// Implements the Bus interface for the DLL
//--------------------------------------------------------------------------------
// ******************************Bus Functions************************* 
bool CheckBusReference(TDSSCktElement* CktElement, int BusReference)
{
	int		i = 0;
	bool	result = false;

	auto with0 = CktElement;
	for (int i = 0; i < with0->Fnterms; i++)
	{
		if (with0->Terminals[i].BusRef == BusReference)
		{
			result = true;
			break;
		}
	}
	return result;
}
// ******************************int type properties************************* 
int __stdcall BUSI(int mode, int arg)
{
	int result = 0;

	switch (mode)
	{
	case 0:												//  Bus.NumNodes 
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];

			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->Buses[with0->ActiveBusIndex]->get_FNumNodesThisBus();
			}
		}
		break;
	case 1:												//  Bus.ZscRefresh 
		result = 0; // Init in case of failure
		if (ExecHelper::DoZscRefresh(ActiveActor) == 0)
		{
			result = 1;
		}
		break;
	case 2:												//  Bus.Coorddefined 
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				if (with0->Buses[with0->ActiveBusIndex]->CoordDefined)
				{
					result = 1;
				}
			}
		}
		break;
	case 3:												//  Bus.GetUniqueNodeNumber 
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = Utilities::GetUniqueNodeNumber(with0->BusList.Get(with0->ActiveBusIndex + 1), arg);
			}
		}
		break;
	case 4:												//  Bus.N_Customers 
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = (with0->Buses[with0->ActiveBusIndex]->BusTotalNumCustomers);
			}
		}
		break;
	case 5:												//  Bus.SectionID 
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = (with0->Buses[with0->ActiveBusIndex]->BusSectionID);
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall BUSF(int mode, double arg)
{
	double result = 0.0;

	switch (mode)
	{
	case 0:												//  Bus.kVBase 
		result = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->Buses[with0->ActiveBusIndex]->kVBase;
			}
		}
		break;
	case 1:												//  Bus.X Read  
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				if (with0->Buses[with0->ActiveBusIndex]->CoordDefined)
				{
					result = with0->Buses[with0->ActiveBusIndex]->x;
				}
			}
		}
		break;
	case 2:												//  Bus.X Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				if (with0->Buses[with0->ActiveBusIndex]->CoordDefined)
				{
					with0->Buses[with0->ActiveBusIndex]->x = arg;
				}
			}
			result = 0;
		}
		break;
	case 3:												//  Bus.Y Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				if (with0->Buses[with0->ActiveBusIndex]->CoordDefined)
				{
					result = with0->Buses[with0->ActiveBusIndex]->y;
				}
			}
		}
		break;
	case 4:												//  Bus.Y Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				with0->Buses[with0->ActiveBusIndex]->CoordDefined = true;
				with0->Buses[with0->ActiveBusIndex]->y = arg;
			}
			result = 0.0;
		}
		break;
	case 5:												//  Bus.Distance
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->Buses[with0->ActiveBusIndex]->DistFromMeter;
			}
		}
		break;
	case 6:												//  Bus.Lambda
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->Buses[with0->ActiveBusIndex]->BusFltRate;
			}
		}
		break;
	case 7:												//  Bus.N_Interrupts
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->Buses[with0->ActiveBusIndex]->Bus_Num_Interrupt;
			}
		}
		break;
	case 8:												//  Bus.Int_Duration
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->Buses[with0->ActiveBusIndex]->Bus_Int_Duration;
			}
		}
		break;
	case 9:												//  Bus.Cust_interrupts
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->Buses[with0->ActiveBusIndex]->BusCustInterrupts;
			}
		}
		break;
	case 10:												//  Bus.Cust_duration
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->Buses[with0->ActiveBusIndex]->BusCustDurations;
			}
		}
		break;
	case 11:												//  Bus.Totalmiles
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->Buses[with0->ActiveBusIndex]->BusTotalMiles;
			}
		}
		break;
	case 12:												//  Bus.Latitude read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				if (with0->Buses[with0->ActiveBusIndex]->CoordDefined)
				{
					result = with0->Buses[with0->ActiveBusIndex]->lat;
				}
			}
		}
		break;
	case 13:												//  Bus.Latitude Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				with0->Buses[with0->ActiveBusIndex]->CoordDefined = true;
				with0->Buses[with0->ActiveBusIndex]->lat = arg;
			}
			result = 0.0;
		}
		break;
	case 14:												//  Bus.Longitude read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				if (with0->Buses[with0->ActiveBusIndex]->CoordDefined)
				{
					result = with0->Buses[with0->ActiveBusIndex]->longitude;
				}
			}
		}
		break;
	case 15:												//  Bus.Longitude Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				with0->Buses[with0->ActiveBusIndex]->CoordDefined = true;
				with0->Buses[with0->ActiveBusIndex]->longitude = arg;
			}
			result = 0.0;
		}
		break;

	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall BUSS(int mode, char* arg)
{
	string result = "0";

	switch (mode)
	{
	case 0:												//  Bus.Name
		result = "";
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				result = with0->BusList.Get(with0->ActiveBusIndex + 1);
			}
		}
		break;
	default:
		result = "Error, Parameter non recognized";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall BUSV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	int BusReference = 0,
				k = 0,
				LoadCount = 0,
				LineCount = 0,
				Nvalues = 0,
				Norder = 0,
				i = 0,
				iV = 0,
				NodeIdxi = 0,
				jj = 0,
				NodeIdxj = 0,
				Nelements = 0,
				j = 0,
				NodeIdx = 0,
				Voc = 0,
				Z = 0,
				Y1 = 0;
	complex		Isc = cmplx(0, 0),
				Volts = cmplx(0, 0);
	TDSSBus*	pBus = nullptr;
	complex		V012[4] = { cmplx(0,0),cmplx(0,0) , cmplx(0,0), cmplx(0,0) },
				Vph[4]	= { cmplx(0,0),cmplx(0,0) , cmplx(0,0), cmplx(0,0) };
	double		BaseFactor = 0.0;
	polar		voltsp = {};
	TDSSCktElement* pElem = nullptr;
	TcMatrix*	Zsc012Temp = nullptr;
	pDoubleArray pValues = nullptr;
	DynStringArray myPXEList;
	String		S = "";


	switch (mode)
	{
	case 0:												//  Bus.Voltages
		*myType = 3; //Complex
		if (ActiveCircuit[ActiveActor] == nullptr || ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			myCmplxArray.resize(1);
			myCmplxArray[0] = CZero;
		}
		else
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				myCmplxArray.resize(0);
				pBus = with0->Buses[with0->ActiveBusIndex];

				Nvalues = pBus->FNumNodesThisBus;
				jj = 1;
				auto with0 = pBus;
				for (i = 0; i < Nvalues; i++)
				{
					do
					{
						NodeIdx = with0->FindIdx(jj); // Get the index of the Node that matches jj
						jj++;
					}
					while (NodeIdx <= 0);
					myCmplxArray.push_back(ActiveCircuit[ActiveActor]->Solution->NodeV[with0->GetRef(NodeIdx)]);
				}
			}
		}
		*mySize = myCmplxArray.size() * sizeof(complex);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 1:												//  Bus.SeqVoltages
		*myType = 2;
		if (ActiveCircuit[ActiveActor] == nullptr || ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			myDblArray.resize(1);
			myDblArray[0] = 0.0;
		}
		else
		{
			auto with0 = ActiveCircuit[ActiveActor];
			myDblArray.resize(3);
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				Nvalues = with0->Buses[with0->ActiveBusIndex]->FNumNodesThisBus;
				if (Nvalues > 3)
					Nvalues = 3;

				if (Nvalues != 3)
				{
					for (int i = 1; i <= 3; i++)
					{
						myDblArray[i - 1] = -1;// Signify seq voltages n/A for less then 3 phases
					}
				}
				else
				{
					iV = 0;
					for (i = 1; i <= 3; i++)
					{
						Vph[i] = with0->Solution->NodeV[with0->Buses[with0->ActiveBusIndex]->Find(i)];
					}
					Phase2SymComp(&Vph[1], &V012[1]); // Compute Symmetrical components
					for (i = 1; i <= 3; i++) {
						myDblArray[iV] = cabs(V012[i]);
						iV++;
					}
				}

			}

		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(double);
		break;
	case 2:												//  Bus.Nodes
		*myType = 1;
		if (ActiveCircuit[ActiveActor] == nullptr)
		{
			myIntArray.resize(1);
			myIntArray[0] = 0;
		}
		else
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				pBus = with0->Buses[with0->ActiveBusIndex];
				auto with0 = pBus;
				myIntArray.resize(pBus->FNumNodesThisBus);
				Nvalues = pBus->FNumNodesThisBus;
				iV = 0;
				jj = 1;
				for (i = 1; i <= Nvalues ; i++)
				{
					do
					{
						NodeIdx = with0->FindIdx(jj);
						jj++;
					}
					while (NodeIdx <= 0);
						myIntArray[iV] = with0->GetNum(NodeIdx);
				}
				iV++;
			}
		}
		*myPtr = (uintptr_t)(void*)(myIntArray.data());
		*mySize = myIntArray.size() * sizeof(int);
		break;
	case 3:												//  Bus.Voc
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				if (!(with0->Buses[with0->ActiveBusIndex]->VBus.empty()))
				{
					Nvalues = with0->Buses[with0->ActiveBusIndex]->FNumNodesThisBus;
					myCmplxArray.resize(Nvalues);
					for (i = 1; i <= Nvalues; i++)
					{
						myCmplxArray[i - 1] = with0->Buses[with0->ActiveBusIndex]->VBus[i - 1];
					}
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 4:												//  Bus.Isc
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				Nvalues = with0->Buses[with0->ActiveBusIndex]->FNumNodesThisBus;
				myCmplxArray.resize(Nvalues, CZero);
				if (!(with0->Buses[with0->ActiveBusIndex]->BusCurrent.empty()))
				{
					for (i = 1; i <= Nvalues; i++)
					{
						myCmplxArray[i - 1] = with0->Buses[with0->ActiveBusIndex]->BusCurrent[i - 1];
					}
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 5:												//  Bus.PuVoltages
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				pBus = with0->Buses[with0->ActiveBusIndex];
				Nvalues = pBus->FNumNodesThisBus;
				myCmplxArray.resize(Nvalues);
				iV = 0;
				jj = 1;
				if (pBus->kVBase > 0.0)
				{
					BaseFactor = 1000.0 * pBus->kVBase;
				}
				else
					BaseFactor = 1.0;
				for (i = 1; i <= Nvalues; i++)
				{
					// this code so nodes come out in order from smallest to larges
					do
					{
						NodeIdx = pBus->FindIdx(jj);
						jj++;
					}
					while (NodeIdx <= 0);
					myCmplxArray[iV] = cdivreal(ActiveCircuit[ActiveActor]->Solution->NodeV[pBus->GetRef(NodeIdx)], BaseFactor);
					iV++;
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 6:												//  Bus.ZscMatrix
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			try
			{
				auto with0 = ActiveCircuit[ActiveActor];
				if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
				{
					if (with0->Buses[with0->ActiveBusIndex]->Zsc.Norder != 0)
					{
						Nelements = with0->Buses[with0->ActiveBusIndex]->Zsc.Norder;
						myCmplxArray.resize(Nelements * Nelements);
						iV = 0;
						auto with0 = ActiveCircuit[ActiveActor]->Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex];
						for (i = 1; i <= Nelements; i++)
						{
							for (j = 1; j <= Nelements; j++)
							{
								myCmplxArray[iV] = with0->Zsc.GetElement(i, j);
								iV++;
							}
						}
					}
				}

			}
			catch (const std::exception &E)
			{
				DoSimpleMsg("ZscMatrix Error: " + (std::string)E.what() + CRLF, 5016);
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 7:												//  Bus.Zsc1
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				myCmplxArray[0] = with0->Buses[with0->ActiveBusIndex]->Get_Zsc1();
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 8:												//  Bus.Zsc0
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				myCmplxArray[0] = with0->Buses[with0->ActiveBusIndex]->Get_Zsc0();
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 9:												//  Bus.YscMatrix
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			try
			{
				auto with0 = ActiveCircuit[ActiveActor];
				if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
				{
					if (with0->Buses[with0->ActiveBusIndex]->Ysc.Norder != 0)
					{
						Nelements = with0->Buses[with0->ActiveBusIndex]->Ysc.Norder;
						myCmplxArray.resize(Nelements * Nelements);
						iV = 0;
						auto with0 = ActiveCircuit[ActiveActor]->Buses[ActiveCircuit[ActiveActor]->ActiveBusIndex];
						for (i = 1; i <= Nelements; i++)
						{
							for (j = 1; j <= Nelements; j++)
							{
								myCmplxArray[iV] = with0->Ysc.GetElement(i, j);
								iV++;
							}
						}
					}
				}

			}
			catch (const std::exception& E)
			{
				DoSimpleMsg("ZscMatrix Error: " + (std::string)E.what() + CRLF, 5017);
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 10:												//  Bus.CplxSeqVoltages
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			myCmplxArray.resize(3);
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				Nvalues = with0->Buses[with0->ActiveBusIndex]->FNumNodesThisBus;
				if (Nvalues > 3)
					Nvalues = 3;

				if (Nvalues != 3)
				{
					for (int i = 0; i <= 2; i++)
					{
						myCmplxArray[i] = cmplx(-1.0, -1.0);// Signify seq voltages n/A for less then 3 phases
					}
				}
				else
				{
					iV = 0;
					for (i = 1; i <= 3; i++)
					{
						Vph[i] = with0->Solution->NodeV[with0->Buses[with0->ActiveBusIndex]->Find(i)];
					}
					Phase2SymComp(&Vph[1], &V012[1]); // Compute Symmetrical components
					for (i = 1; i <= 3; i++) {
						myCmplxArray[iV] = V012[i];
						iV++;
					}
				}

			}

		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 11:												//  Bus.VLL
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				pBus = with0->Buses[with0->ActiveBusIndex];
				Nvalues = pBus->FNumNodesThisBus;
				myCmplxArray.resize(Nvalues);
				iV = 0;
				jj = 1;
				if (Nvalues > 3)
					Nvalues = 3;
				if (Nvalues > 1)
				{
					if (Nvalues == 2)
						Nvalues = 1;// only one L-L voltage if 2 phase
					myCmplxArray.resize((2 * Nvalues) - 1);
					iV = 0;
					auto with1 = pBus;
					if (with1->kVBase > 0.0)
					{
						BaseFactor = 1000.0 * with1->kVBase;
					}
					else
						BaseFactor = 1.0;
					for (i = 1; i <= Nvalues; i++)  // for 2- or 3-phases
					{
						// this code assumes the nodes are ordered 1, 2, 3
						//------------------------------------------------------------------------------------------------
						// This section was added to prevent measuring using disconnected nodes, for example, if the
						// bus has 2 nodes but those are 1 and 3, that will bring a problem.
						jj = i;
						do
						{
							NodeIdxi = with1->FindIdx(jj);// Get the index of the Node that matches i
							jj++;
						}
						while (NodeIdxi <= 0);
						do
						{
							NodeIdxj = with1->FindIdx(jj); // Get the index of the Node that matches i
							if (jj > 3)
								jj = 1;
							else
								jj++;
						}
						while (NodeIdxj <= 0);
						Volts = csub(with0->Solution->NodeV[with1->GetRef(NodeIdxi)], with0->Solution->NodeV[with1->GetRef(NodeIdxj)]);
						myCmplxArray[iV] = Volts;
						iV++;
					}
				}
				else  // for 1-phase buses, do not attempt to compute.
				{
					myCmplxArray.resize(1); // just return -1"s in array
					myCmplxArray[0] = cmplx(-99999.0, 0);
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 12:												//  Bus.PuVLL
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				pBus = with0->Buses[with0->ActiveBusIndex];
				Nvalues = pBus->FNumNodesThisBus;
				if (Nvalues > 3)
					Nvalues = 3;
				if (Nvalues > 1)
				{
					if (Nvalues == 2) Nvalues = 1;// only one L-L voltage if 2 phase
					myCmplxArray.resize((2 * Nvalues) - 1);
					iV = 0;
					auto with0 = pBus;
					if (with0->kVBase > 0.0)
					{
						BaseFactor = 1000.0 * with0->kVBase * sqrt(3);
					}
					else
						BaseFactor = 1.0;
					for (i = 1; i <= Nvalues; i++)  // for 2- or 3-phases
					{
						// this code assumes the nodes are ordered 1, 2, 3
						//------------------------------------------------------------------------------------------------
						// This section was added to prevent measuring using disconnected nodes, for example, if the
						// bus has 2 nodes but those are 1 and 3, that will bring a problem.
						jj = i;
						do
						{
							NodeIdxi = with0->FindIdx(jj);// Get the index of the Node that matches i
							jj++;
						}
						while (NodeIdxi <= 0);
						do
						{
							NodeIdxj = with0->FindIdx(jj); // Get the index of the Node that matches i
							if (jj > 3)
								jj = 1;
							else
								jj++;
						}
						while (NodeIdxj <= 0);
						Volts = csub(ActiveCircuit[ActiveActor]->Solution->NodeV[pBus->GetRef(NodeIdxi)], ActiveCircuit[ActiveActor]->Solution->NodeV[pBus->GetRef(NodeIdxj)]);
						myCmplxArray[iV] = cdivreal(Volts, BaseFactor);
						iV++;
					}

				}
				else  // for 1-phase buses, do not attempt to compute.
				{
					myCmplxArray.resize(1); // just return -1"s in array
					myCmplxArray[0] = cmplx(-99999.0, 0);
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 13:												//  Bus.VMagAngle
		*myType = 3;
		myPolarArray.resize(1);
		myPolarArray[0] = ctopolar(CZero);
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				pBus = with0->Buses[with0->ActiveBusIndex];
				Nvalues = pBus->FNumNodesThisBus;
				myPolarArray.resize(Nvalues);
				iV = 0;
				jj = 1;
				for (i = 1; i <= Nvalues; i++)
				{
					// this code so nodes come out in order from smallest to larges
					do
					{
						NodeIdx = pBus->FindIdx(jj);
						jj++;
					}
					while (NodeIdx <= 0);
					voltsp = ctopolardeg(ActiveCircuit[ActiveActor]->Solution->NodeV[pBus->GetRef(NodeIdx)]);
					myPolarArray[iV] = voltsp;
					iV++;
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myPolarArray.data());
		*mySize = sizeof(polar) * myPolarArray.size();
		break;
	case 14:												//  Bus.PuVMagAngle
		*myType = 3;
		myPolarArray.resize(1);
		myPolarArray[0] = ctopolar(CZero);
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				pBus = with0->Buses[with0->ActiveBusIndex];
				Nvalues = pBus->FNumNodesThisBus;
				myPolarArray.resize(Nvalues);
				iV = 0;
				jj = 1;
				if (pBus->kVBase > 0.0) BaseFactor = 1000.0 * pBus->kVBase;
				else BaseFactor = 1.0;
				for (i = 1; i <= Nvalues; i++)
				{
					do
					{
						NodeIdx = pBus->FindIdx(jj);
						jj++;
					}
					while (NodeIdx <= 0);
					myPolarArray[iV] = ctopolardeg(ActiveCircuit[ActiveActor]->Solution->NodeV[pBus->GetRef(NodeIdx)]);
					myPolarArray[iV].mag = myPolarArray[iV].mag / BaseFactor;
					iV++;
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myPolarArray.data());
		*mySize = sizeof(polar) * myPolarArray.size();
		break;
	case 15:												//  Bus.LineList
		*myType = 4; // String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			BusReference = with0->ActiveBusIndex;
			LineCount = 0;
			pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Lines.Get_First();
			while (ASSIGNED(pElem))
			{
				if (CheckBusReference(pElem, BusReference))
				{
					LineCount++;
				}
				pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Lines.Get_Next();
			}
			if (LineCount > 0)
			{
				myStrArray.resize(0);
				pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Lines.Get_First();
				while (ASSIGNED(pElem))
				{
					if (CheckBusReference(pElem, BusReference))
					{
						S = "LINE." + pElem->LName;
						WriteStr2Array(S);
						WriteStr2Array(Char0());
					}
					pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Lines.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 16:												//  Bus.LoadList
		*myType = 4; // String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			BusReference = with0->ActiveBusIndex;
			LoadCount = 0;
			pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Loads.Get_First();
			while (ASSIGNED(pElem))
			{
				if (CheckBusReference(pElem, BusReference))
				{
					LoadCount++;
				}
				pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Loads.Get_Next();
			}
			if (LoadCount > 0)
			{
				myStrArray.resize(0);
				pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Loads.Get_First();
				while (ASSIGNED(pElem))
				{
					if (CheckBusReference(pElem, BusReference))
					{
						S = "LOAD." + pElem->LName;
						WriteStr2Array(S);
						WriteStr2Array(Char0());
					}
					pElem = (TDSSCktElement*)ActiveCircuit[ActiveActor]->Loads.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 17:												//  Bus.ZSC012Matrix
		*myType = 2; //Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pBus = with0->Buses[with0->ActiveBusIndex];
			if (pBus->FNumNodesThisBus == 3)
			{
				Nvalues = Sqr(pBus->FNumNodesThisBus) * 2; // Should be 9 complex numbers
				// Compute ZSC012 for 3-phase buses else leave it zeros
				// ZSC012 = Ap2s Zsc As2p
				Zsc012Temp = pBus->Zsc.MtrxMult(As2p); // temp for intermediate result
				if (pBus->Zsc012.Norder != 0)
				{
					pBus->Zsc012 = TcMatrix(0);
				}
				TcMatrix* tmp = Ap2s->MtrxMult(Zsc012Temp);
				if (tmp != nullptr)
				{
					pBus->Zsc012 = *tmp;
					free(tmp);
					//Return all the elements of ZSC012
					pValues = pDoubleArray(pBus->Zsc012.GetValuesArrayPtr(Norder));
					myDblArray.resize(Nvalues);
					for (i = 0; i < Nvalues; i++)
					{
						myDblArray[i] = pValues[i];
					}
				}
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 18:												//  Bus.AllPCEatBus
		*myType = 4;  //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				myPXEList = with0->getPCEatBus(with0->BusList.Get(with0->ActiveBusIndex + 1));
				myStrArray.resize(0);
				for (i = 0; i < myPXEList.size(); i++)
				{
					if (!myPXEList.empty())
					{
						WriteStr2Array(myPXEList[i]);
						WriteStr2Array(Char0());
					}
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 19:												//  Bus.AllPDEatBus
		*myType = 4;  //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->ActiveBusIndex >= 0) && (with0->ActiveBusIndex < with0->NumBuses))
			{
				myPXEList = with0->getPDEatBus(with0->BusList.Get(with0->ActiveBusIndex + 1));
				myStrArray.resize(0);
				for (i = 0; i < myPXEList.size(); i++)
				{
					if (!myPXEList.empty())
					{
						WriteStr2Array(myPXEList[i]);
						WriteStr2Array(Char0());
					}
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	default:
		myStrArray.resize(0);
		WriteStr2Array("Parameter not identified");
		WriteStr2Array(Char0());
		*myType = 4;					//String
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}

//****************************************************************************************************************
//--------------------------------------------------------------------------------
// Implements the CapControl interface for the DLL
//--------------------------------------------------------------------------------
// ******************************CapControls Functions************************* 
TCapControlObj* ActiveCapControl()
{
	TCapControlObj* result = nullptr;
	if (ActiveCircuit[ActiveActor] != nullptr)
	{
		result = (TCapControlObj*) ActiveCircuit[ActiveActor]->CapControls.Get_Active();
	}
	return result;
}
void Set_ParameterACC(string parm, string val)
{
	string mycmd = "";
	if (not(ASSIGNED(ActiveCircuit[ActiveActor])))
	{
		 // Or break?
	}
	else
	{
		SolutionAbort = false;// Reset for commands entered from outside
		mycmd = Format("capcontrol.%s.%s=%s", ActiveCapControl()->LName.c_str(), parm.c_str(), val.c_str());
		DSSExecutive[ActiveActor]->Set_Command(mycmd);
	}
}
// ****************************** int  type properties************************* 
int __stdcall CapControlsI(int mode, int arg)
{
	TCapControlObj* elem	= nullptr;
	TPointerList *lst = nullptr;

	int				result	= 0;   // Default return value
	switch (mode)
	{
	case 0:												//  CapControls.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &ActiveCircuit[ActiveActor]->CapControls;
			elem = (TCapControlObj*)lst->Get_First();
			if (elem != nullptr)
			{
				do
				{
					if (elem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = 1;
					}
					else elem = (TCapControlObj*) lst->Get_Next();
				} while (!(result == 1 || elem == nullptr));
			}
		}
		break;
	case 1:												//  CapControls.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &ActiveCircuit[ActiveActor]->CapControls;
			elem = (TCapControlObj*)lst->Get_Next();
			if (elem != nullptr)
			{
				do
				{
					if (elem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = lst->get_myActiveItem();
					}
					else elem = (TCapControlObj*)lst->Get_Next();
				} while (!(result > 0 || elem == nullptr));
			}
		}
		break;
	case 2:												//  CapControls.Mode read
		result = 1;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			switch (elem->get_ControlType())
			{
			case CURRENTCONTROL:
				result = 0;
				break;
			case VOLTAGECONTROL:
				result = 1;
				break;
			case KVARCONTROL:
				result = 2;
				break;
			case TIMECONTROL:
				result = 3;
				break;
			case PFCONTROL:
			case USERCONTROL: // both assign 4 to result
				result = 4;
				break;
			default:
				result = 1;
				break;
			}
		}
		break;
	case 3:												//  CapControls.Mode write
		result = 1;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			switch (arg)
			{
			case 0:
				elem->set_ControlType(CURRENTCONTROL);
				break;
			case 1:
				elem->set_ControlType(VOLTAGECONTROL);
				break;
			case 2:
				elem->set_ControlType(KVARCONTROL);
				break;
			case 3:
				elem->set_ControlType(TIMECONTROL);
				break;
			case 4:
				elem->set_ControlType(PFCONTROL);
				break;
			case 5:
				elem->set_ControlType(USERCONTROL);
				break;
			default:
				result = -1;
				break;
			}
		}
		break;
	case 4:												//  CapControls.MonitoredTerm read
		result = 0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->ElementTerminal;
		}
		break;
	case 5:												//  CapControls.MonitoredTerm write
		Set_ParameterACC("Terminal", IntToStr(arg));
		break;
	case 6:												//  CapControls.UseVoltOverride read
		result = 0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			if (elem->GetVoverride_Value())
				result = 1;
		}
		break;
	case 7:												//  CapControls.UseVoltOverride write
		if (arg == 1)
			Set_ParameterACC("VoltOverride", "Yes");
		else
			Set_ParameterACC("VoltOverride", "No");
		break;
	case 8:												//  CapControls.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor])) 
			result = ActiveCircuit[ActiveActor]->CapControls.get_myNumList();
		break;

	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall CapControlsF(int mode, double arg)
{
	TCapControlObj* elem = nullptr;
	double			result = 0.0;

	switch (mode)
	{
	case 0:												//  CapControls.CTRatio read
		result = 0.0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->GetCTRatio_Value();
		}
		break;
	case 1:												//  CapControls.CTRatio write
		Set_ParameterACC("CTratio", FloatToStr(arg));
		break;
	case 2:												//  CapControls.PTRatio read
		result = 0.0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->GetPTRatio_Value();
		}
		break;
	case 3:												//  CapControls.PTRatio write
		Set_ParameterACC("PTRatio", FloatToStr(arg));
		break;
	case 4:												//  CapControls.ONSetting read
		result = 0.0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->GetON_Value();
		}
		break;
	case 5:												//  CapControls.ONSetting write
		Set_ParameterACC("OnSetting", FloatToStr(arg));
		break;
	case 6:												//  CapControls.OFFSetting read
		result = 0.0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->GetOFF_Value();
		}
		break;
	case 7:												//  CapControls.OFFSetting write
		Set_ParameterACC("OffSetting", FloatToStr(arg));
		break;
	case 8:												//  CapControls.VMax read
		result = 0.0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->GetVmax_Value();
		}
		break;
	case 9:												//  CapControls.VMax write
		Set_ParameterACC("Vmax", FloatToStr(arg));
		break;
	case 10:												//  CapControls.VMin read
		result = 0.0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->GetVmin_Value();
		}
		break;
	case 11:												//  CapControls.VMin write
		Set_ParameterACC("Vmin", FloatToStr(arg));
		break;
	case 12:												//  CapControls.Delay read
		result = 0.0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->GetONDelay_Value();
		}
		break;
	case 13:												//  CapControls.Delay write
		Set_ParameterACC("Delay", FloatToStr(arg));
		break;
	case 14:												//  CapControls.DelayOff read
		result = 0.0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->GetOFFDelay_Value();
		}
		break;
	case 15:												//  CapControls.DelayOff write
		Set_ParameterACC("DelayOff", FloatToStr(arg));
		break;
	case 16:												//  CapControls.DeadTime read
		result = 0.0;
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->GetDeadTime_Value();

		}
		break;
	case 17:												//  CapControls.DeadTime write
		Set_ParameterACC("DeadTime", FloatToStr(arg));
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall CapControlsS(int mode, char* arg)
{
	TCapControlObj* elem = nullptr;
	int				ActiveSave = 0;
	string			S		= "";
	bool			Found	= false;
	TPointerList* lst = nullptr;
	string			result	= "0";

	switch (mode)
	{
	case 0:												//  CapControls.Name read
		result = "";
		elem = ActiveCapControl();
		if (elem != nullptr) {
			result = elem->LName;
		}
		break;
	case 1:												//  CapControls.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &ActiveCircuit[ActiveActor]->CapControls;
			S = arg;
			Found = false;
			ActiveSave = lst->ActiveItem;
			elem = (TCapControlObj*)lst->Get_First();
			while (elem != nullptr)
			{
				if (CompareText(elem->LName, S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
					Found = true;
					break;
				}
				elem = (TCapControlObj*)lst->Get_Next();
			}
			if (!(Found))
			{
				DoSimpleMsg("CapControl \"" + S + "\" Not Found in Active Circuit.", 5003);
				elem = (TCapControlObj*)lst->Get(ActiveSave);    // Restore active Load
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
			}
		}
		break;
	case 2:												//  CapControls.Capacitor read
		result = "";
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->ControlledCapacitor->get_Name();
		}
		break;
	case 3:												//  CapControls.Capacitor write
		Set_ParameterACC("Capacitor", (string)arg);
		break;
	case 4:												//  CapControls.MonitoredObj read
		result = "";
		elem = ActiveCapControl();
		if (elem != nullptr)
		{
			result = elem->ElementName;
		}
		break;
	case 5:												//  CapControls.MonitoredObj write
		Set_ParameterACC("Element", (string)arg);
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall CapControlsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TCapControlObj* elem = nullptr;
	TPointerList* lst = nullptr;
	int i = 0,
		k = 0;
	string S;

	switch (mode)
	{
	case 0: 												//  CapControls.AllNames
		*myType = 4; //string
		myStrArray.resize(0);

		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->CapControls.get_myNumList() > 0)
			{
				lst = &with0->CapControls;
				myStrArray.resize(0);
				k = 0;
				elem = (TCapControlObj*)lst->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->LName);
					WriteStr2Array(Char0());
					elem = (TCapControlObj*)lst->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	default:
		myStrArray.resize(0);
		WriteStr2Array("Parameter not identified");
		WriteStr2Array(Char0());
		*myType = 4;					//String
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Circuit interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall CircuitI(int mode, int arg)
{
	TDSSCktElement* p	= nullptr;
	TDSSMonitor*	Mon = nullptr;
	TEnergyMeter*	Mtr = nullptr;
	int				result = 0;

	switch (mode)
	{
	case 0:                                             // Circuit.NumCktElements
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->NumDevices;
		}
		break;
	case 1:                                             // Circuit.NumBuses
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->NumBuses;
		}
		break;
	case 2:                                             // Circuit.NumNodes
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->NumNodes;
		}
		break;
	case 3:                                             // Circuit.FirstPCElement
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			p = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PCElements.Get_First();
			if (p != nullptr)
			{
				do
				{
					if (p->FEnabled)
					{
						result = 1;
					}
					else
						ActiveCircuit[ActiveActor]->PCElements.Get_Next();
				} while (!((result == 1) || (p == nullptr)));
			}
			else
				result = 0;
		}
		break;
	case 4:                                             // Circuit.NextPCElement
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			p = (TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next();
			if (p != nullptr)
			{
				do
				{
					if (p->FEnabled)
					{
						result = ActiveCircuit[ActiveActor]->PCElements.ActiveItem;
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(p);
					}
					else
						p = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PCElements.Get_Next();
				} while (!((result > 0) || (p == nullptr)));
			}
		}
		else
			result = 0;
		break;
	case 5:                                             // Circuit.FirstPDElement
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			p = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PDElements.Get_First();
			if (p != nullptr)
			{
				do
				{
					if (p->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(p);
						result = 1;
					}
					else
						p = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PDElements.Get_Next();
				} while (!((result == 1) || (p == nullptr)));
			}

		}
		else
			result = 0;
		break;
	case 6:                                             // Circuit.NextPDElement
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			p = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PDElements.Get_Next();
			if (p != nullptr)
			{
				do
				{
					if (p->FEnabled)
					{
						result = ActiveCircuit[ActiveActor]->PDElements.ActiveItem;
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(p);
					}
					else
						p = (TDSSCktElement*)ActiveCircuit[ActiveActor]->PDElements.Get_Next();
				} while (!((result > 0) || (p == nullptr)));
			}
		}
		else
			result = 0;
		break;
	case 7:                                             // Circuit.Sample
		MonitorClass[ActiveActor]->SampleAll(ActiveActor);
		EnergyMeterClass[ActiveActor]->SampleAll(ActiveActor);
		break;
	case 8:                                             // Circuit.SaveSample
		Mon = (TDSSMonitor*)DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("monitor"));
		Mon->SaveAll(ActiveActor);
		Mtr = (TEnergyMeter*)DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("energymeter"));
		Mtr->SaveAll(ActiveActor);
		break;
	case 9:                                             // Circuit.SetActiveBusi
		result = -1; // Signifies Error
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (arg >= 0 && arg < with0->NumBuses)
			{
				with0->ActiveBusIndex = arg;
				result = 0;
			}
		}
		break;
	case 10:                                             // Circuit.FirstElement
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr && ASSIGNED(ActiveDSSClass[ActiveActor]))
		{
			result = ActiveDSSClass[ActiveActor]->Get_First();
		}
		else
			result = 0;
		break;
	case 11:                                             // Circuit.NextElement
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr && ASSIGNED(ActiveDSSClass[ActiveActor]))
		{
			result = ActiveDSSClass[ActiveActor]->Get_Next();
		}
		else
			result = 0;
		break;
	case 12:                                             // Circuit.UpdateStorage
		StorageClass[ActiveActor]->UpdateAll(ActiveActor);
		break;
	case 13:                                             // Circuit.ParentPDElement
		result = 0;
		if (dynamic_cast<TPDElement*>(ActiveCircuit[ActiveActor]->FActiveCktElement) != nullptr)
		{
			p = ((TPDElement*)ActiveCircuit[ActiveActor]->FActiveCktElement)->ParentPDElement;
			if (p != nullptr)
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(p);
				result = p->ClassIndex; //should be > 0
			}
		}
		break;
	case 14:                                             // Circuit.EndofTimeStepUpdate
		EndOfTimeStepCleanup(ActiveActor);
		result = 0;
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall CircuitF(int mode, double arg1, double arg2)
{
	double result = 0.0; //Default return value
	switch (mode)                                             // Circuit.Capacity
	{
	case 0:
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			with0->CapacityStart = arg1;
			with0->CapacityIncrement = arg2;
			if (with0->ComputeCapacity(ActiveActor))
			{
				result = with0->RegisterTotals[3 - 1] + with0->RegisterTotals[19 - 1];
			}
			else
				result = 0.0;
		}
		result = 0.0;
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall CircuitS(int mode, char* arg)
{
	int DevClassIndex = 0;
	string result = ""; // Default return value

	switch (mode)
	{
	case 0:                                             // Circuit.Name
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->LName;
		}
		else
			result = "";
		break;
	case 1:                                             // Circuit.Disable
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			with0->SetElementActive((string)arg);
			if (with0->FActiveCktElement != nullptr)
			{
				with0->FActiveCktElement->FEnabled = false;
			}
		}
		break;
	case 2:                                             // Circuit.Enable
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			with0->SetElementActive((string)arg);
			if (with0->FActiveCktElement != nullptr)
			{
				with0->FActiveCktElement->FEnabled = true;
			}
		}
		break;
	case 3:                                             // Circuit.SetActiveElement
		result = "-1";
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = IntToStr(ActiveCircuit[ActiveActor]->SetElementActive(arg) - 1);// make zero based to be compatible with collections and variant arrays
		}
		else
				DoSimpleMsg("Create a circuit before trying to set an element active!", 5015);
		break;
	case 4:                                             // Circuit.SetActiveBus
		SetActiveBus(StripExtension((string)arg));
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = IntToStr(ActiveCircuit[ActiveActor]->ActiveBusIndex);
		}
		else
			result = "-1";
		break;
	case 5:                                             // Circuit.SetActiveClass
		result = "0";
		DevClassIndex = ClassNames[ActiveActor].Find(arg);
		if (DevClassIndex == 0)
		{
			DoSimpleMsg("Error: Class " + (string)arg + " not found.", 5016);
			break;
		}
		LastClassReferenced[ActiveActor] = DevClassIndex;
		ActiveDSSClass[ActiveActor] = (TDSSClass*) DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
		result = IntToStr(LastClassReferenced[ActiveActor]);
		break;
	default:
		result = "Error, parameter not recognized";
		break;

	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall CircuitV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	complex LossValue{};
	TLineObj* pLine = nullptr;
	complex Loss{};
	TTransfObj* pTransf;
	TDSSCktElement* pCktElem = {};
	complex Curr= cmplx(0,0),
			cPower = cmplx(0,0),
			cLoss = cmplx(0, 0),
			Volts = cmplx(0, 0);
	int		Phase = 0,
			i = 0, 
			j = 0, 
			k = 0, 
			NodeIdx = 0;
	double	VoltsD = 0.0,
			BaseFactor = 0.0;
	string BusName = "";

	unsignedint nBus{}, 
				nNZ{}, 
				NValues{}, 
				iV{}, 
				p{};
	klusparseset_t hY{};
	std::vector<unsigned int> ColPtr;
	std::vector<unsigned int> RowIdx;
	std::vector<complex> cVals;
	vector <double> Temp;
	vector <string> Temp2;
	int* Pint = 0;

	auto with0 = ActiveCircuit[ActiveActor];
	switch (mode)
	{
	case 0:                                             // Circuit.Losses
		*myType = 3; //complex
		myCmplxArray.resize(1);
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			myCmplxArray[0] = ActiveCircuit[ActiveActor]->Get_Losses(ActiveActor);
		}
		else
			myCmplxArray[0] = cmplx(0, 0);
		*mySize = myCmplxArray.size() * sizeof(complex);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 1:                                             // Circuit.LineLosses
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			pLine = (TLineObj*) with0->Lines.Get_First();
			Loss = cmplx(0.0, 0.0);
			while (pLine != nullptr)
			{
				caccum(Loss, pLine->Get_Losses(ActiveActor));
				pLine = (TLineObj*) with0->Lines.Get_Next();
			}
			myCmplxArray[0] = cmulreal(Loss, 0.001);
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 2:                                             // Circuit.SubstationLosses
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			

			pTransf = (TTransfObj*) with0->Transformers.Get_First();
			Loss = cmplx(0.0, 0.0);
			while (pTransf != nullptr)
			{
				if (pTransf->IsSubstation)
				{
					caccum(Loss, pTransf->Get_Losses(ActiveActor));
					pTransf = (TTransfObj*) with0->Transformers.Get_Next();
				}
				pTransf = (TTransfObj*) with0->Transformers.Get_Next();
			}
			myCmplxArray[0] = cmulreal(Loss, 0.001);

		}

		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(complex);
		break;
	case 3:                                             // Circuit.TotalPower
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			
			pCktElem = (TDSSCktElement*)with0->Sources.Get_First();
			cPower = cmplx(0.0, 0.0);
			while (pCktElem != nullptr)
			{
				caccum(cPower, pCktElem->Get_Power(1, ActiveActor));
				pCktElem = (TDSSCktElement*)with0->Sources.Get_Next();
			}
			myCmplxArray[0] = cmulreal(cPower, 0.001);
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		break;
	case 4:                                             // Circuit.AllBusVolts
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			myCmplxArray.resize(with0->NumNodes);
			k = 0;
			for (i = 1; i <= with0->NumBuses; i++)
			{
				for (j = 1 ; j <= with0->Buses[i - 1]->FNumNodesThisBus; j++)
				{
					myCmplxArray[k] = ActiveCircuit[ActiveActor]->Solution->NodeV[(with0->Buses[i - 1]->GetRef(j))];
					k++;
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		break;
	case 5:                                             // Circuit.AllBusVMag
		*myType = 2; //Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			myDblArray.resize(with0->NumNodes);
			k = 0;
			for (i = 1; i <= with0->NumBuses; i++)
			{
				for (j = 1; j <= with0->Buses[i - 1]->FNumNodesThisBus; j++)
				{

					myDblArray[k] = cabs(with0->Solution->NodeV[with0->Buses[i - 1]->GetRef(j)]);
					k++;
				}
			}
		}

		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 6:                                             // Circuit.AllElementNames
		*myType = 4; //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			
			for (i = 1; i <= with0->NumDevices; i++)
			{
				auto with1 = (TDSSCktElement*)with0->CktElements.Get(i);
				WriteStr2Array(with1->ParentClass->Class_Name + "." + with1->LName);
				WriteStr2Array(Char0());
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 7:                                             // Circuit.AllBusNames
		*myType = 4; //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			
			for (i = 1; i <= with0->NumBuses; i++)
			{
				WriteStr2Array(with0->BusList.Get(i));
				WriteStr2Array(Char0());
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 8:                                             // Circuit.AllElementLosses
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			
			myCmplxArray.resize(with0->NumDevices);
			k = 0;
			pCktElem = (TDSSCktElement*)with0->CktElements.Get_First();
			while (pCktElem != nullptr)
			{
				myCmplxArray[k] = cmulreal(pCktElem->Get_Losses(ActiveActor), 0.001);
				k++;
				pCktElem = (TDSSCktElement*)with0->CktElements.Get_Next();
			}
		}

		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		break;
	case 9:                                             // Circuit.AllBusVMagPu
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			myDblArray.resize(with0->NumNodes);
			k = 0;
			for (i = 1; i <= with0->NumBuses; i++)
			{
				if (with0->Buses[i - 1]->kVBase > 0.0)
					BaseFactor = 1000.0 * with0->Buses[i - 1]->kVBase;
				else
					BaseFactor = 1.0;
				for (j = 1; j <= with0->Buses[i - 1]->FNumNodesThisBus; j++)
				{

					myDblArray[k] = cabs(with0->Solution->NodeV[with0->Buses[i - 1]->GetRef(j)]) / BaseFactor;
					k++;
				}
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 10:                                             // Circuit.AllNodeNames
		*myType = 4; //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			for (i = 1; i <= with0->NumBuses; i++)
			{
				BusName = with0->BusList.Get(i);

				for (j = 1; j <= with0->Buses[i - 1]->FNumNodesThisBus; j++)
				{
					WriteStr2Array(BusName + "." + IntToStr(with0->Buses[i - 1]->GetNum(j)));
					WriteStr2Array(Char0());
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 11:                                             // Circuit.SystemY
		// { Return zero length Array if no circuit or no Y matrix}
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] == nullptr)
			myCmplxArray[0] = cmplx(0, 0);
		else if (ActiveCircuit[ActiveActor]->Solution->hY == 0)
			myCmplxArray[0] = cmplx(0, 0);
		else
		{
			
			hY = ActiveCircuit[ActiveActor]->Solution->hY;
			// get the compressed columns out of KLU
			FactorSparseMatrix(hY);
			GetNNZ(hY, &nNZ);
			GetSize(hY, &nBus);
			ColPtr.resize(nBus + 1);
			RowIdx.resize(nNZ);
			cVals.resize(nNZ);
			GetCompressedMatrix(hY, nBus + 1, nNZ, &ColPtr[0], &RowIdx[0], &cVals[0]);

			// allocate a square matrix
			NValues = Sqr(with0->NumNodes);
			myCmplxArray.resize(NValues);// Make variant array for complex

			// the new way, first set all elements to zero
			for (iV = 0; iV < NValues; iV++)
				myCmplxArray[iV] = cmplx(0, 0);
			// then back-fill the non-zero values
			for (j = 0; j <= nBus - 1; j++) /// the zero-based column
			{
				for (p = ColPtr[j]; p <= ColPtr[j + 1] - 1; p++)
				{
					i = RowIdx[p];
					iV = i * nBus + j; // the zero-based, row-wise, complex result index
					myCmplxArray[iV] = cVals[p];
				}
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		break;
	case 12:                                             // Circuit.AllBusDistances
		*myType = 2;
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			
			myDblArray.resize(with0->NumBuses);
			for (i = 1; i <= with0->NumBuses; i++)
			{
				myDblArray[i - 1] = with0->Buses[i - 1]->DistFromMeter;
			}
		}

		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize =myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 13:                                             // Circuit.AllNodeDistances
		*myType = 2;
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			
			myDblArray.resize(with0->NumNodes);
			k = 0;
			for (i = 1; i <= with0->NumBuses; i++)
			{
				for (j = 0; j < (with0->Buses[i - 1]->FNumNodesThisBus); j++)
				{
					myDblArray[k] = with0->Buses[i - 1]->DistFromMeter;
					k++;
				}
			}
		}

		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 14:                                             // Circuit.AllNodeVMagbyPhase
		*myType = 2;
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		Pint = *(int**)myPtr;
		Phase = *Pint;
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			
			// Make a Temporary Array big enough to hold all nodes
			Temp.resize(with0->NumNodes);
			// Find nodes connected to specified phase
			k = 0;
			for (i = 1; i <= with0->NumBuses; i++)
			{
				NodeIdx = with0->Buses[i - 1]->FindIdx(Phase);
				if (NodeIdx > 0)
				{
					Temp[k] = cabs(ActiveCircuit[ActiveActor]->Solution->NodeV[with0->Buses[i - 1]->GetRef(NodeIdx)]);
					k++;
				}
			}
			// Assign to result and free temp array
			myDblArray.resize(k);
			for (i = 0; i < k; i++)
			{
				myDblArray[i] = Temp[i];
			}
			Temp.clear();
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 15:                                             // Circuit.AllNodeVMagPUByPhase
		*myType = 2;
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		Pint = *(int**)myPtr;
		Phase = *Pint;
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			
			// Make a Temporary Array big enough to hold all nodes
			Temp.resize(with0->NumNodes);
			// Find nodes connected to specified phase
			k = 0;
			for (i = 1; i <= with0->NumBuses; i++)
			{
				NodeIdx = with0->Buses[i - 1]->FindIdx(Phase);
				if (NodeIdx > 0)
				{
					if (with0->Buses[i - 1]->kVBase > 0.0)
						BaseFactor = 1000.0 * with0->Buses[i - 1]->kVBase;
					else
						BaseFactor = 1.0;
					Temp[k] = cabs(ActiveCircuit[ActiveActor]->Solution->NodeV[with0->Buses[i - 1]->GetRef(NodeIdx)]) / BaseFactor;
					k++;
				}
			}
			// Assign to result and free temp array
			myDblArray.resize(k);
			for (i = 0; i < k; i++)
			{
				myDblArray[i] = Temp[i];
			}
			Temp.clear();
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 16:                                             // Circuit.AllNodeDistancesByPhase
		*myType = 2;
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		Pint = *(int**)myPtr;
		Phase = *Pint;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			
			// Make a Temporary Array big enough to hold all nodes
			Temp .resize(with0->NumNodes);
			// Find nodes connected to specified phase
			k = 0;
			for (i = 1; i <= with0->NumBuses; i++)
			{
				NodeIdx = with0->Buses[i - 1]->FindIdx(Phase);
				if (NodeIdx > 0)
				{
					Temp[k] = ActiveCircuit[ActiveActor]->Buses[i - 1]->DistFromMeter;
					k++;
				}
			}
			// Assign to result and free temp array
			myDblArray.resize(k);
			for (i = 0; i < k ; i++)
			{
				myDblArray[i] = Temp[i];
			}
			Temp.clear();
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		break;
	case 17:                                             // Circuit.AllNodeNamesByPhase
		*myType = 4; //string
		myStrArray.resize(0);
		Pint = *(int**)myPtr;
		Phase = *Pint;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			
			// Make a Temporary Array big enough to hold all nodes
			Temp2.resize(with0->NumNodes);
			// Find nodes connected to specified phase
			k = 0;
			for (i = 1; i <= with0->NumBuses; i++)
			{
				NodeIdx = with0->Buses[i - 1]->FindIdx(Phase);
				if (NodeIdx > 0)// Node found with this phase number
				{
					Temp2[k] = Format("%s.%d", with0->BusList.Get(i).c_str(), Phase);
					k++;
				}
			}
			// Assign to result and free temp array
			for (i = 0; i < k; i++)
			{
				WriteStr2Array(Temp2[i]);
				WriteStr2Array(Char0());
			}
			Temp2.clear();
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 18:                                             // Circuit.YNodeVArray
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = sizeof(myCmplxArray[0]);
		
		if (ActiveCircuit[ActiveActor] != nullptr && !with0->Solution->NodeV.empty())
		{
			*myPtr = (uintptr_t)(void*)&(with0->Solution->NodeV[1]);
			*mySize = sizeof(ActiveCircuit[ActiveActor]->Solution->NodeV[1]) * with0->NumNodes;
		}
		break;
	case 19:                                             // Circuit.YNodeOrder
		*myType = 4;
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			
			for (i = 1; i <= with0->NumNodes; i++)
			{
				auto with1 = with0->MapNodeToBus[i - 1];
				WriteStr2Array(Format("%s.%-d", UpperCase(with0->BusList.Get(with1.BusRef)).c_str(), with1.NodeNum));
				WriteStr2Array(Char0());
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 20:                                             // Circuit.YCurrents
		
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = sizeof(myCmplxArray[0]);
		if (with0 != nullptr && !with0->Solution->Currents.empty())
		{
			*myPtr = (uintptr_t)(void*)&(with0->Solution->Currents[1]);
			*mySize = sizeof(with0->Solution->Currents[1]) * with0->NumNodes;
		}
		break;
	default:
		*myType = 4; //string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}

}

//--------------------------------------------------------------------------------
// Implements the CktElement interface for the DLL
//--------------------------------------------------------------------------------
// ******************************CktElement Functions************************** 
void CalcSeqCurrents(TDSSCktElement* pActiveElement, complex* i012) 
{
	int			Nvalues = 0,
				i		= 0, 
				j		= 0, 
				k		= 0, 
				iV		= 0;
	complex		IPh[4]	= { cmplx(0,0),cmplx(0,0) , cmplx(0,0) ,cmplx(0,0) }, 
				I012a[4]= { cmplx(0,0),cmplx(0,0) , cmplx(0,0) ,cmplx(0,0) };
	vector <complex> cBuffer;

	auto with0 = ActiveCircuit[ActiveActor];
	auto with1 = pActiveElement;
	Nvalues = with1->Fnphases;
	if (Nvalues != 3) // {Handle non-3 phase elements}
	{
		if (with1->Fnphases == 1 && (with0->PositiveSequence))// {Handle non-3 phase elements}
		{
			Nvalues = with1->Fnconds * with1->Fnterms;
			cBuffer.resize(Nvalues + 1);
			with1->GetCurrents(&cBuffer[1], ActiveActor);

			for (i = 1; i <= with1->Fnterms; i++)
			{
				iV = 0;
				// {Populate only phase 1 quantities in Pos seq}
				for (j = 1; j <= with1->Fnterms; j++)
				{
					k = (j - 1) * with1->Fnconds;
					i012[iV] = CZero;
					i012[iV + 1] = cBuffer[1 + k];  // pos seq is 2nd element in array
					i012[iV + 2] = CZero;
					iV += 3;
				}
			}
			cBuffer.clear();
		}
		else
		{
			for (i = 1; i <= 3 * with1->Fnterms; i++)
			{
				i012[i - 1] = cmplx(-1, 0);  // Signify n/A
			}
		}
	}
	else // for 3-phase elements
	{
		iV = 0;
		Nvalues = with1->Fnconds * with1->Fnterms;
		cBuffer.resize(Nvalues);
		with1->GetCurrents(&cBuffer[0], ActiveActor);
		for (j = 1; j <= with1->Fnterms; j++)
		{
			k = (j - 1) * with1->Fnconds;
			Phase2SymComp(&cBuffer[k], &I012a[1]);
			for (i = 1; i <= 3; i++) // Stuff it in the result array
			{
				i012[iV] = I012a[i];
				iV++;
			}

		}
		cBuffer.clear();
	}
}

void CalcSeqVoltages(TDSSCktElement* pActiveElement, complex* V012) 
{
	int		Nvalues = 0, 
			i		= 0, 
			j		= 0, 
			k		= 0, 
			iV		= 0;
	complex VPh[4]	= { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0) }, 
			V012a[4]= { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0) };

	auto with0 = ActiveCircuit[ActiveActor];
	auto with1 = pActiveElement;
	Nvalues = with1->Fnphases;
	if (Nvalues != 3) // {Handle non-3 phase elements}
	{
		if ((with1->Fnphases == 1) && (with0->PositiveSequence))// {Handle non-3 phase elements}
		{
			//Nvalues = with1->Fnconds * with1->Fnterms;
			//cBuffer = MyAllocMem(sizeof(cBuffer[1]) * Nvalues);
			//with1->GetCurrents(cBuffer, ActiveActor);

			for (i = 1; i <= (with1->Fnterms * 3); i++)
			{
				V012[i - 1] = CZero;// Initialize Result
				iV = 1; // pos seq is 2nd element in array
				// {Populate only phase 1 quantities in Pos seq}
				for (j = 1; j <= with1->Fnterms; j++)
				{
					k = (j - 1) * with1->Fnconds;
					V012[iV] = with0->Solution->NodeV[with1->NodeRef[k]];
					iV += 3;
				}
			}
		}
		else //if neither 3 - phase or pos seq model, just put in - 1.0 for each element
		{
			for (i = 1; i <= 3 * with1->Fnterms; i++)
			{
				V012[i - 1] = cmplx(-1, 0);  // Signify n/A
			}
		}
	}
	else // for 3-phase elements
	{
		iV = 0;
		for (j = 1; j <= with1->Fnterms; j++)
		{
			k = (j - 1) * with1->Fnconds;
			for (i = 1; i <= 3; i++)
			{
				VPh[i] = with0->Solution->NodeV[with1->NodeRef[i + k - 1]];
				Phase2SymComp(&VPh[1], &V012a[1]); // Compute Symmetrical components
			}
			for (i = 1; i <= 3; i++) // Stuff it in the result array
			{
				V012[iV] = V012a[i];
				iV++;
			}
		}
	}
}

bool IsPDElement()
{
	bool result = (ActiveCircuit[ActiveActor]->FActiveCktElement->DSSObjType & 3) == PD_ELEMENT;
	return result;
}
// ******************************int type properties************************* 
int __stdcall CktElementI(int mode, int arg)
{
	int				iControl = 0,
					result = 0,
					i = 0,
					count = 0,
					low = 0,
					numcond = 0,
					n = 0,
					iV = 0;
	TDSSCktElement* pCktElement = nullptr;
	TDSSCktElement* ctrl = nullptr;
	TPCElement*		pPCElem = nullptr;
	TPDElement*		pPDElem = nullptr;
	complex			Volts = cmplx(0,0),
					cResid = cmplx(0, 0);
	bool			BData = false;


	switch (mode)
	{
	case 0:                                    // CktElement.Numterminals
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->FActiveCktElement->Fnterms;
		}
		break;
	case 1:                                    // CktElement.NumConductors
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->FActiveCktElement->Fnconds;
		}
		break;
	case 2:                                    // CktElement.NumPhases
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->FActiveCktElement->Fnphases;
		}
		break;
	case 3:                                    // CktElement.Open
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				with1->ActiveTerminal = &(with1->Terminals[arg - 1]);
				with1->Set_ConductorClosed(3, ActiveActor, false);
			}
		}
		break;
	case 4:                                    // CktElement.Close
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				with1->ActiveTerminal = &(with1->Terminals[arg - 1]);
				with1->Set_ConductorClosed(3, ActiveActor, true);
			}
		}
		break;
	case 5:                                    // CktElement.IsOpen
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			auto with1 = with0->FActiveCktElement;
			with1->ActiveTerminal = &(with1->Terminals[arg - 1]);
			result = 0;
			for (i = 1; i <= with1->Get_NConds(); i++)
			{
				if (!(with0->FActiveCktElement->Get_ConductorClosed(i, ActiveActor, false)))
				{
					result = 1;
					break;
				}
			}
		}
		break;
	case 6:                                    // CktElement.NumProperties
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				result = with1->ParentClass->NumProperties;
			}
		}
		break;
	case 7:                                    // CktElement.HasSwitchControl
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			ctrl = (TDSSCktElement*)ActiveCircuit[ActiveActor]->FActiveCktElement->ControlElementList.Get_First();
			while (ctrl != nullptr)
			{
				switch (ctrl->DSSObjType & CLASSMASK)
				{
				case SWT_CONTROL:
					result = 1;
					break;
				default:
					result = 0;
					break;
				}
				if (result == 1)
					break;
				ctrl = (TDSSCktElement*)ActiveCircuit[ActiveActor]->FActiveCktElement->ControlElementList.Get_Next();

			}
		}
		break;
	case 8:                                    // CktElement.HasVoltControl
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			ctrl = (TDSSCktElement*)ActiveCircuit[ActiveActor]->FActiveCktElement->ControlElementList.Get_First();
			while (ctrl != nullptr)
			{
				switch (ctrl->DSSObjType & CLASSMASK)
				{
				case CAP_CONTROL:
					result = 1;
					break;
				case REG_CONTROL:
					result = 1;
					break;
				default:
					result = 0;
					break;
				}
				if (result == 1)
					break;
				ctrl = (TDSSCktElement*)ActiveCircuit[ActiveActor]->FActiveCktElement->ControlElementList.Get_Next();

			}
		}
		break;
	case 9:                                    // CktElement.NumControls
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->FActiveCktElement->ControlElementList.NumInList;
		}
		break;
	case 10:                                    // CktElement.OCPDevIndex
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			iControl = 1;
			do
			{// cycle through the list of controls until we find a fuse, recloser, or relay
				pCktElement = (TDSSCktElement*)with0->FActiveCktElement->ControlElementList.Get(iControl);
				if (pCktElement != nullptr)
				{
					switch (pCktElement->DSSObjType & CLASSMASK)
					{
					case FUSE_CONTROL:
						result = longInt(iControl);
						break;
					case RECLOSER_CONTROL:
						result = longInt(iControl);
						break;
					case RELAY_CONTROL:
						result = longInt(iControl);
						break;
					}
				}
				iControl++;
			} while (!((iControl > with0->FActiveCktElement->ControlElementList.NumInList) || result > 0));
		}
		break;
	case 11:                                    // CktElement.OCPDevType
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			result = GetOCPDeviceType(with0->FActiveCktElement); // see Utilities
		}
		break;
	case 12:                                    // CktElement.Enabled - read
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveCircuit[ActiveActor]->FActiveCktElement->FEnabled)
			{
				result = 1;
			}
		}
		break;
	case 13:                                    // CktElement.Enabled - Write
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (arg > 0)
				BData = true;
			ActiveCircuit[ActiveActor]->FActiveCktElement->FEnabled = BData;
		}
		break;
	case 14:                                    // CktElement.ActiveVariableidx
		result = -1; // Signifies an error; no variable found
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if (with1->DSSObjType == PC_ELEMENT) // BASECLASSMASK Not added
				{
					pPCElem = (TPCElement*)with0->FActiveCktElement;
					if ((arg > 0) && (arg <= pPCElem->NumVariables()))
					{
						result = 0; // the variable seems to exist
						VarIdx = arg;
					}
				}
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall CktElementF(int mode, double arg)
{
	TDSSCktElement* ctrl = nullptr;
	TPCElement*		pPCElem = nullptr;
	TPDElement*		pPDElem = nullptr;
	bool			BData = false;
	int				Volts = 0,
					i = 0,
					count = 0,
					low = 0,
					numcond = 0,
					n = 0,
					iV = 0;
	complex			cResid = cmplx(0,0);
	vector<complex> cBuffer;
	string			S = "";
	double			result = 0.0;

	switch (mode)
	{
	case 0:                                        // CktElement.NormalAmps - read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->FActiveCktElement->DSSObjType & 3) == PD_ELEMENT)
			{
				auto with1 = (TPDElement*)with0->FActiveCktElement;
				result = with1->NormAmps;
			}
		}
		break;
	case 1:                                        // CktElement.NormalAmps - Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsPDElement())
			{
				auto with0 = ActiveCircuit[ActiveActor];
				auto with1 = (TPDElement*)with0->FActiveCktElement;
				with1->NormAmps = arg;
			}
		}
		break;
	case 2:                                        // CktElement.EmergAmps - read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if ((with0->FActiveCktElement->DSSObjType & 3) == PD_ELEMENT)
			{
				auto with1 = (TPDElement*)with0->FActiveCktElement;
				result = with1->EmergAmps;
			}
		}
		break;
	case 3:                                        // CktElement.EmergAmps - Write
		if (ActiveCircuit[ActiveActor])
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (IsPDElement())
			{
				auto with1 = (TPDElement*)with0->FActiveCktElement;
				with1->EmergAmps = arg;
			}
		}
		break;
	case 4:                                        // CktElement.Variablei 
		result = 0; // means an error; no value set
		i = trunc(arg);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;

				if ((with1->DSSObjType & BaseClassMask) == PC_ELEMENT)
				{
					pPCElem = (TPCElement*)with0->FActiveCktElement;
					if (i > 0 && (i <= pPCElem->NumVariables()))
					{
						result = pPCElem->Get_Variable(i);
					}

				}
			}
		}
		break;
	case 5:                                        // CktElement.SetActiveVariable
		result = -1;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if ((with1->DSSObjType & BaseClassMask) == PC_ELEMENT)
				{
					pPCElem = (TPCElement*)with0->FActiveCktElement;
					if ((VarIdx > 0) && (VarIdx <= pPCElem->NumVariables()))  //Checks that the active Idx is valid
					{
						result = 0; // No error, the variable exists and is set
						pPCElem->Set_Variable(VarIdx, arg);
					}
				}
			}
		}
		break;
	case 6:                                        // CktElement.GetActiveVariable
		result = -1;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if ((with1->DSSObjType & BaseClassMask) == PC_ELEMENT)
				{
					pPCElem = (TPCElement*)with0->FActiveCktElement;
					if ((VarIdx > 0) && (VarIdx <= pPCElem->NumVariables()))  //Checks that the active Idx is valid
					{
						result = pPCElem->Get_Variable(VarIdx);
					}
				}
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall CktElementS(int mode, char* arg)
{
	TDSSCktElement* ctrl = nullptr;
	TPCElement*		pPCElem = nullptr;
	TPDElement*		pPDElem = nullptr;
	bool			BData = false;
	int				Volts = 0,
					i = 0,
					count = 0,
					low = 0,
					numcond = 0,
					n = 0,
					iV = 0;
	complex			cResid = cmplx(0, 0);
	vector<complex> cBuffer;
	string			S = "",
					result = "0";

	switch (mode)
	{
	case 0:                                          // CktElement.Name
		if (ActiveCircuit[ActiveActor])
		{
			auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
			result = with0->ParentClass->Class_Name + "." + with0->LName;
		}
		else
			result = "";
		break;
	case 1:                                          // CktElement.Display - read
		if (ActiveCircuit[ActiveActor])
		{
			auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
			result = with0->Get_DisplayName();
		}
		else
			result = "";
		break;
	case 2:                                          // CktElement.Display - write
		if (ActiveCircuit[ActiveActor])
		{
			ActiveCircuit[ActiveActor]->FActiveCktElement->Set_DisplayName(arg);
		}
		else
			result = "";
		break;
	case 3:                                          // CktElement.GUID
		if (ActiveCircuit[ActiveActor])
		{
			auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
			result = with0->Get_ID();
		}
		else
			result = "";
		break;
	case 4:                                          // CktElement.EnergyMeter
		if (ActiveCircuit[ActiveActor])
		{
			if (ActiveCircuit[ActiveActor]->FActiveCktElement->HasEnergyMeter)
			{
				pPDElem = (TPDElement*)ActiveCircuit[ActiveActor]->FActiveCktElement;
				result = pPDElem->MeterObj->LName;
			}
		}
		break;
	case 5:                                          // CktElement.Controller
		result = "";
		i = StrToInt(arg);
		if (ActiveCircuit[ActiveActor])
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (i > 0 && i <= with0->FActiveCktElement->ControlElementList.NumInList)
			{
				ctrl = (TDSSCktElement*)with0->FActiveCktElement->ControlElementList.Get(i);
				if (ctrl != nullptr)
				{

					result = Format("%s.%s", ctrl->ParentClass->Class_Name.c_str(), ctrl->LName.c_str());

				}
			}
		}
		break;
	case 6:                                          // CktElement.ActiveVariableName
		result = "Error";  // Signifies an error; the variable doesn"t exist
		if (ActiveCircuit[ActiveActor])
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if ((with1->DSSObjType & BaseClassMask) == PC_ELEMENT)
				{
					pPCElem = (TPCElement*)with0->FActiveCktElement;
					VarIdx = pPCElem->LookupVariable(arg);
					if ((VarIdx > 0) && (VarIdx <= pPCElem->NumVariables()))
					{
						result = "OK";  // we are good, the variable seems
					}
				}
			}
		}
		break;
	default:
		result = "Error";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall CktElementV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TDSSCktElement* ctrl	= nullptr;
	TPCElement*		pPCElem = nullptr;
	TPDElement*		pPDElem = nullptr;
	bool			BData	= false;
	int				Volts	= 0,
					i		= 0,
					count	= 0,
					low		= 0,
					numcond = 0,
					n		= 0,
					iV		= 0,
					myInit	= 0,
					myEnd	= 0,
					j		= 0,
					k		= 0,
					NValues = 0;
	complex			cResid	= cmplx(0,0);
	vector<complex>	cBuffer,
					myBuffer;
	pComplexArray	cValues = nullptr;
	complex 		VPh[4]	= { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0) },
					IPh[4]	= { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0) },
					I012[4] = { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0) },
					V012[4] = { cmplx(0,0), cmplx(0,0), cmplx(0,0), cmplx(0,0) };
	polar			CMagAng = {};
	string			S = "";

	switch (mode)
	{
	case 0:                                        // CktElement.NormalAmps - read
		*myType = 4; //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			for (i = 1; i <= with0->FActiveCktElement->Fnterms ; i++)
			{
				WriteStr2Array(with0->FActiveCktElement->GetBus(i));
				WriteStr2Array(Char0());
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 1:                                        // CktElement.BusNames - write
		*myType = 4; //string
		j = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			count = with0->FActiveCktElement->Fnterms;
			for (i = 1; i <= count; i++)
			{
				S = BArray2Str(myPtr, &j);
				if (S.empty())
				{
					break;
				}
				else
					with0->FActiveCktElement->SetBus(i, S);
			}
			*mySize = j;
		}
		break;
	case 2:                                        // CktElement.Voltages
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				numcond = with1->Fnconds * with1->Fnterms;
				myCmplxArray.resize(numcond);
				iV = 0;
				for (i = 1; i <= numcond; i++)
				{
					n = with1->NodeRef[i - 1];
					myCmplxArray[iV] = with0->Solution->NodeV[n]; //ok if = 0
					iV++;
				}
			}
		}
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 3:                                        // CktElement.Currents
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
			numcond = with0->Fnconds * with0->Fnterms;
			myCmplxArray.resize(numcond);
			cBuffer.resize(numcond + 1);
			with0->GetCurrents(&cBuffer[1], ActiveActor);
			iV = 0;
			for (i = 1; i <= numcond ; i++)
			{
				myCmplxArray[iV] = cBuffer[i];
				iV++;
			}
			cBuffer.clear();
		}
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 4:                                        // CktElement.powers
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
			numcond = with0->Fnconds * with0->Fnterms;
			myCmplxArray.resize(numcond);
			cBuffer.resize(numcond + 1);
			with0->GetPhasePower(&cBuffer[1], ActiveActor);
			iV = 0;
			for (i = 1; i <= numcond; i++)
			{
				myCmplxArray[iV] = cmulreal(cBuffer[i], 0.001);
				iV++;
			}
			cBuffer.clear();
		}
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 5:                                        // CktElement.Losses
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				myCmplxArray[0] = with0->FActiveCktElement->Get_Losses(ActiveActor);
			}
		}
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 6:                                        // CktElement.PhaseLosses
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
			numcond = with0->Fnphases;
			myCmplxArray.resize(numcond);
			cBuffer.resize(numcond);
			with0->GetPhaseLosses(numcond, &cBuffer[0], ActiveActor);
			iV = 0;
			for (i = 0; i < numcond; i++)
			{
				myCmplxArray[iV] = cmulreal(cBuffer[i], 0.001);
				iV++;
			}
			cBuffer.clear();
		}
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 7:                                        // CktElement.SeqVoltages
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if (with1->FEnabled)
				{
					try
					{
						myDblArray.resize(with1->Fnterms * 3);
						cBuffer.resize(with1->Fnterms * 3);
						// get complex seq voltages
						CalcSeqVoltages(with0->FActiveCktElement, &cBuffer[0]);
						// return 0 based array
						for (i = 0; i < 3 * with1->Fnterms ; i++)
							myDblArray[i] = cabs(cBuffer[i]);// return mag only
						cBuffer.clear();
					}
					catch (const std::exception&E)
					{
						S = (string)E.what() + CRLF +
							"Element=" + with1->get_Name() + CRLF +
							"Nphases=" + IntToStr(with1->Get_NPhases()) + CRLF +
							"NTerms=" + IntToStr(with1->Get_NTerms()) + CRLF +
							"NConds =" + IntToStr(with1->Get_NConds());
						DoSimpleMsg(S, 5012);
					}
				}
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 8:                                        // CktElement.SeqCurrents
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if (with1->FEnabled)
				{
					try
					{
						myDblArray.resize(with1->Fnterms * 3);
						cBuffer.resize((with1->Fnterms * 3) + 1);
						// get complex seq voltages
						CalcSeqCurrents(with0->FActiveCktElement, &cBuffer[1]);
						// return 0 based array
						for (i = 1; i <= (3 * with1->Fnterms); i++)
							myDblArray[i - 1] = cabs(cBuffer[i]);// return mag only
						cBuffer.clear();
					}
					catch (const std::exception& E)
					{
							S = (string)E.what() + CRLF +
								"Element=" + with1->get_Name() + CRLF +
								"Nphases=" + IntToStr(with1->Get_NPhases()) + CRLF +
								"NTerms=" + IntToStr(with1->Get_NTerms()) + CRLF +
								"NConds =" + IntToStr(with1->Get_NConds());
							DoSimpleMsg(S, 5012);
					}
				}
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 9:                                        // CktElement.SeqPowers
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				myCmplxArray.resize(with1->Fnterms * 3);
				if (with1->Fnphases != 3)
				{
					if (with1->Fnphases == 1 && with0->PositiveSequence)
					{
						numcond = with1->Get_NConds() * with1->Get_NTerms();
						cBuffer.resize(numcond + 1);
						with1->GetCurrents(&cBuffer[1], ActiveActor);
						for (i = 0; i <= 3 * with1->Fnterms; i++)
							myCmplxArray[i] = cmplx(0, 0);// Initialize Result
						count = 2; // Start with kW1 {put only phase 1 quantities in Pos seq}

						for (j = 0; j <= 3 * with1->Fnterms; j++)
						{
							k = (j - 1) * with1->Fnconds;
							n = with1->NodeRef[k];
							VPh[1] = with0->Solution->NodeV[n]; // Get voltage at node
							myCmplxArray[count] = cmulreal(cmul(VPh[1], conjg(cBuffer[k + 1])), 0.003); // Compute power per phase
							count++;
						}
						cBuffer.clear();
					}
					else
					{
						for (i = 0; i < (3 * with1->Fnterms); i++)
						{
							myCmplxArray[count] = cmplx(-1.0, -1.0); // Signify n/A
							count++;
						}
					}
				}
				else
				{
					numcond = with1->Fnconds * with1->Fnterms;
					cBuffer.resize(numcond + 1);
					with1->GetCurrents(&cBuffer[1], ActiveActor);
					count = 0;
					for (j = 1; j <= with1->Fnterms; j++)
					{
						k = (j - 1) * with1->Fnconds;
						for (i = 1; i <= 3; i++)
						{
							VPh[i] = with0->Solution->NodeV[with1->NodeRef[i + k - 1]];
						}
						for (i = 1; i <= 3; i++)
						{
							IPh[i] = cBuffer[k + i];
						}
						Phase2SymComp(&IPh[1], &I012[1]);
						Phase2SymComp(&VPh[1], &V012[1]);
						for (i = 1; i <= 3; i++)
						{
							myCmplxArray[count] = cmulreal(cmul(V012[i], conjg(I012[i])), 0.003);
							count++;
						}
					}
					cBuffer.clear();
				}

			}

		}
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 10:                                        // CktElement.AllPropertyNames
		*myType = 4; //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				auto with2 = with1->ParentClass;
				for (k = 0; k < with2->NumProperties; k++)
				{
					WriteStr2Array(with2->PropertyName[k]);
					WriteStr2Array(Char0());
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 11:                                        // CktElement.Residuals
		*myType = 3;
		myPolarArray.resize(1);
		myPolarArray[0] = ctopolar(CZero);
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
			myPolarArray.resize(with0->Fnterms);// 2 values per terminal
			cBuffer.resize(with0->Yorder + 1);
			with0->GetCurrents(&cBuffer[1], ActiveActor);
			iV = 0;
			for (i = 1; i <= with0->Fnterms; i++)
			{
				cResid = CZero;
				k = (i - 1) * with0->Fnconds;
				for (j = 1; j <= with0->Fnconds; j++)
				{
					k++;
					caccum(cResid, cBuffer[k]);
				}
				myPolarArray[iV] = ctopolardeg(cResid);
				iV++;
			}
			cBuffer.clear();

		}
		*mySize = myPolarArray.size() * sizeof(polar);
		*myPtr = (uintptr_t)(void*)(myPolarArray.data());
		break;
	case 12:                                        // CktElement.YPrim
		*myType = 3;				// Complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
				NValues = sqr(with0->Yorder);
				cValues = with0->GetYPrimValues(ALL_YPRIM);// Get pointer to complex array of values
				if (cValues == nullptr)
				{ // check for unassigned array
					return; // Get outta here
				}
				myCmplxArray.resize(NValues);
				iV = 0;
				for (i = 0; i < NValues; i++)
				{
					myCmplxArray[iV] = cValues[i];
					iV++;
				};

			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		break;
	case 13:                                        // CktElement.CplxSeqVoltages
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if (with1->FEnabled)
				{
					try
					{
						myCmplxArray.resize(with1->Fnterms * 3);
						cValues = new complex[(with1->Fnterms * 3)];
						// get complex seq voltages
						CalcSeqVoltages(with0->FActiveCktElement, cValues);
						// return 0 based array
						iV = 0;
						for (i = 0; i < (3 * with1->Fnterms); i++)
						{
							myCmplxArray[iV] = cValues[i];
							iV++;
						}
						delete[] cValues;
					}
					catch (const std::exception& E)
					{
						S = (string)E.what() + CRLF +
							"Element=" + with1->get_Name() + CRLF +
							"Nphases=" + IntToStr(with1->Get_NPhases()) + CRLF +
							"NTerms=" + IntToStr(with1->Get_NTerms()) + CRLF +
							"NConds =" + IntToStr(with1->Get_NConds());
						DoSimpleMsg(S, 5012);
					}
				}
			}
		}
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 14:                                        // CktElement.CplxSeqCurrents
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if (with1->FEnabled)
				{
					try
					{
						myCmplxArray.resize(with1->Fnterms * 3);
						cValues = new complex[(with1->Fnterms * 3)];
						// get complex seq currents
						CalcSeqCurrents(with0->FActiveCktElement, cValues);
						// return 0 based array
						iV = 0;
						for (i = 0; i < 3 * with1->Fnterms; i++)
						{
							myCmplxArray[iV] = cValues[i];// return mag only
							iV++;
						}
						delete[] cValues;
					}
					catch (const std::exception& E)
					{
							S = (string)E.what() + CRLF +
								"Element=" + with1->get_Name() + CRLF +
								"Nphases=" + IntToStr(with1->Get_NPhases()) + CRLF +
								"NTerms=" + IntToStr(with1->Get_NTerms()) + CRLF +
								"NConds =" + IntToStr(with1->Get_NConds());
							DoSimpleMsg(S, 5012);
					}
				}
			}
		}
		*mySize = myCmplxArray.size() * sizeof(complex);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	case 15:                                        // CktElement.AllVariableNames
		*myType = 4; //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if ((with1->DSSObjType & BaseClassMask) == PC_ELEMENT)
				{
					pPCElem = (TPCElement*)with1;
					for (k = 1; k <= pPCElem->NumVariables(); k++)
					{
						WriteStr2Array(pPCElem->VariableName(k));
						WriteStr2Array(Char0());
					}
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 16:                                        // CktElement.AllVariableValues
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;

		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				if ((with1->DSSObjType & BaseClassMask) == PC_ELEMENT)
				{
					pPCElem = (TPCElement*)with0->FActiveCktElement;
					myDblArray.resize(pPCElem->NumVariables());
					for (k = 1; k <= pPCElem->NumVariables(); k++)
					{
						myDblArray[k - 1] = pPCElem->Get_Variable(k);
					}
				}
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 17:                                        // CktElement.Nodeorder
		*myType = 1; //integer
		myIntArray.resize(1);
		myIntArray[0] = 0;

		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement != nullptr)
			{
				auto with1 = with0->FActiveCktElement;
				k = 0;
				myIntArray.resize(with1->Fnterms * with1->Fnconds);
				for (i = 1; i <= with1->Fnterms; i++)
				{
					for (j = ((i - 1) * with1->Fnconds); j < i * with1->Fnconds; j++)
					{
						myIntArray[k] = GetNodeNum(with1->NodeRef[j]);
						k++;
					}

				}

			}
		}
		*mySize = myIntArray.size() * sizeof(int);
		*myPtr = (uintptr_t)(void*)&(myIntArray[0]);

		break;
	case 18:                                        // CktElement.CurrentsMagAng
		*myType = 3; //complex
		myPolarArray.resize(1);
		myPolarArray[0] = ctopolar(CZero);
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;

			NValues = with0->Fnconds * with0->Fnterms;
			myPolarArray.resize(NValues);
			cBuffer.resize(NValues + 1);
			with0->GetCurrents(&cBuffer[1], ActiveActor);
			iV = 0;
			for (i = 1; i <= NValues; i++)
			{

				myPolarArray[iV] = ctopolardeg(cBuffer[i]);// convert to mag/angle
				iV++;
			}
			cBuffer.clear();

		}
		*mySize = sizeof(polar) * myPolarArray.size();
		*myPtr = (uintptr_t)(void*)(myPolarArray.data());
		break;
	case 19:                                        // CktElement.VoltagesMagAng
		*myType = 3; //complex
		myPolarArray.resize(1);
		myPolarArray[0] = ctopolar(CZero);
		if (ActiveCircuit[ActiveActor] != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->FActiveCktElement)
			{
				auto with1 = with0->FActiveCktElement;
				numcond = with1->Fnconds * with1->Fnterms;
				myPolarArray.resize(numcond);
				iV = 0;
				for (i = 1; i <= numcond; i++)
				{
					n = with1->NodeRef[i - 1];
					myPolarArray[iV] = ctopolardeg(with0->Solution->NodeV[n]);
					iV++;
				}
			}
		}

		*mySize = sizeof(polar) * myPolarArray.size();
		*myPtr = (uintptr_t)(void*)(myPolarArray.data());
		break;
	case 20:                                        // CktElement.TotalPowers
		*myType = 3;
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor]->FActiveCktElement;
			NValues = with0->Fnconds * with0->Fnterms;
			myCmplxArray.resize(with0->Fnterms);
			cBuffer.resize(NValues + 1);
			with0->GetPhasePower(&cBuffer[1], ActiveActor);
			iV = 0;
			myBuffer.resize(with0->Fnterms + 1);
			for (j = 1; j <= with0->Fnterms; j++)
			{
				for (i = 1; i <= with0->Fnconds; i++)
					myBuffer[j - 1] = cadd(myBuffer[j - 1], cBuffer[i + (j - 1) * with0->Fnconds]);

				myCmplxArray[iV] = cmulreal(myBuffer[j - 1], 0.001);
				iV++;
			}
			cBuffer.clear();

		}

		*mySize = sizeof(polar) * myCmplxArray.size();
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		break;
	default:
		myStrArray.resize(0);
		WriteStr2Array("Parameter not identified");
		WriteStr2Array(Char0());
		*myType = 4;					//String
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the CmathLib interface for the DLL
//--------------------------------------------------------------------------------
// ******************************floating point type properties************************* 
double __stdcall CmathLibF(int mode, double arg1, double arg2)
{
	double result = 0.0;// Default return value

	switch (mode)
	{
	case 0:											// CmathLib.Cabs
		result = cabs(cmplx(arg1, arg2));
		break;
	case 1:											// CmathLib.Cdang
		result = cdang(cmplx(arg1, arg2));
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//************************Structure type properties*******************************
void __stdcall CmathLibV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	complex*	pCmplx = nullptr;
	polar*		pPolar  = nullptr;
	double* pDbl = nullptr;
	double		a = 0.0,
				b = 0.0;

	switch (mode)
	{
	case 0:											// CmathLib.cmplx
		pDbl = (double*)myPtr;
		*myType = 3; //complex
		myCmplxArray.resize(1);
		a = *pDbl;
		pDbl++;
		b = *pDbl;
		myCmplxArray[0] = cmplx(a, b);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		break;
	case 1:											// CmathLib.ctopolardeg
		pCmplx = (complex*)myPtr;
		*myType = 3; //complex
		myPolarArray.resize(1);
		myPolarArray[0] = ctopolardeg(*pCmplx);
		*myPtr = (uintptr_t)(void*)(myPolarArray.data());
		*mySize = myPolarArray.size() * sizeof(polar);
		break;
	case 2:											// CmathLib.pdegtocomplex
		pPolar = (polar*)myPtr;
		*myType = 3; //complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = pdegtocomplex(pPolar->mag, pPolar->ang);
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = myCmplxArray.size() * sizeof(myCmplxArray[0]);
		break;
	default:
		*myType = 4; //String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}



}

//--------------------------------------------------------------------------------
// Implements the Generators interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall GeneratorsI(int mode, int arg)
{
	TGeneratorObj* pGen;
	int result = 0;					// Default return value
	switch (mode)
	{
	case    0:						// Generators.First
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pGen = (TGeneratorObj*)ActiveCircuit[ActiveActor]->Generators.Get_First();
			if (ASSIGNED(pGen))
			{
				do
				{
					if (pGen->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pGen);
						result = 1;
					}
					else
					{
						pGen = (TGeneratorObj*)ActiveCircuit[ActiveActor]->Generators.Get_Next();
					}
				} while (!((result == 1) || (!ASSIGNED(pGen))));
			}
		}
		break;
	case	1:						// Generators.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pGen = (TGeneratorObj*)ActiveCircuit[ActiveActor]->Generators.Get_Next();
			if (ASSIGNED(pGen))
			{
				do
				{
					if (pGen->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pGen);
						result = ActiveCircuit[ActiveActor]->Generators.get_myActiveItem();
					}
					else
					{
						pGen = (TGeneratorObj*)ActiveCircuit[ActiveActor]->Generators.Get_Next();
					}
				} while (!((result > 0) || (!ASSIGNED(pGen))));
			}
		}
		break;
	case	2:						// Generators.ForcedON read
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				if ( ((TGeneratorObj*)with0.Get_Active())->get_FForcedON())
					result = 1;
			}
		}
		break;
	case	3:						// Generators.ForcedON Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				if (arg == 1)
				{
					((TGeneratorObj*)with0.Get_Active())->set_FForcedON(true);
				}
				else
				{
					((TGeneratorObj*)with0.Get_Active())->set_FForcedON(false);
				}
			}
		}
		break;
	case	4:						// Generators.Phases read
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				result = ((TGeneratorObj*)with0.Get_Active())->Get_NPhases();
			}
		}
		break;
	case	5:						// Generators.Phases write
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				((TGeneratorObj*)with0.Get_Active())->Set_NPhases(arg);
			}
		}
		break;
	case	6:						// Generators.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Generators.get_myNumList();
		}
		break;
	case	7:						// Generators.Idx read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->Generators.get_myActiveItem();
		}
		break;
	case	8:						// Generators.Idx Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pGen = (TGeneratorObj*)ActiveCircuit[ActiveActor]->Generators.Get(arg);
			if (pGen != nullptr)
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pGen);
			}
		}
		break;
	case	9:						// Generators.Model read
		result = -1;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				result = ((TGeneratorObj*)with0.Get_Active())->GenModel;
			}
		}
		break;
	case	10:						// Generators.Model Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				((TGeneratorObj*)with0.Get_Active())->GenModel = arg;
				if (((TGeneratorObj*)with0.Get_Active())->GenModel == 3)
				{
					ActiveCircuit[ActiveActor]->Solution->SolutionInitialized = false;
				}
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall GeneratorsF(int mode, double arg)
{
	double result = 0.0;					// Default return value
	switch (mode)
	{
	case    0:							// Generators.kV read
		result = -1.0;	//not set
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				result = ((TGeneratorObj*)with0.Get_Active())->GenVars.kVGeneratorBase;
			}
		}
		break;
	case	1:							// Generators.kV Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				((TGeneratorObj*)with0.Get_Active())->Set_PresentkV(arg);
			}
		}
		break;
	case	2:							// Generators.kW read
		result = 0.0;	//not set
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				result = ((TGeneratorObj*)with0.Get_Active())->Get_PresentkW();
			}
		}
		break;
	case	3:							// Generators.kW Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				((TGeneratorObj*)with0.Get_Active())->Set_PresentkW(arg);
			}
		}
		break;
	case	4:							// Generators.kvar read
		result = 0.0;	//not set
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				result = ((TGeneratorObj*)with0.Get_Active())->Get_Presentkvar();
			}
		}
		break;
	case	5:							// Generators.kvar Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				((TGeneratorObj*)with0.Get_Active())->Set_Presentkvar(arg);
			}
		}
		break;
	case	6:							// Generators.PF read
		result = 0.0;	//not set
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				result = ((TGeneratorObj*)with0.Get_Active())->get_PFNominal();
			}
		}
		break;
	case	7:							// Generators.PF Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				((TGeneratorObj*)with0.Get_Active())->Set_PowerFactor(arg);
			}
		}
		break;
	case	8:							// Generators.KVARated read
		result = -1.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				result = ((TGeneratorObj*)with0.Get_Active())->GenVars.kVArating;
			}
		}
		break;
	case	9:							// Generators.KVARated Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				((TGeneratorObj*)with0.Get_Active())->GenVars.kVArating = arg;
			}
		}
		break;
	case	10:							// Generators.Vmaxpu read
		result = -1.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				result = ((TGeneratorObj*)with0.Get_Active())->Vmaxpu;
			}
		}
		break;
	case	11:							// Generators.Vmaxpu Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				((TGeneratorObj*)with0.Get_Active())->Vmaxpu = arg;
			}
		}
		break;
	case	12:							// Generators.Vminpu read
		result = -1.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				result = ((TGeneratorObj*)with0.Get_Active())->Vminpu;
			}
		}
		break;
	case	13:							// Generators.Vminpu Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			if (with0.ActiveItem != 0)
			{
				((TGeneratorObj*)with0.Get_Active())->Vminpu = arg;
			}
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall GeneratorsS(int mode, char* arg)
{
	TGeneratorObj*	pGen;
	int				ActiveSave;
	TGeneratorObj*	Gen;
	std::string		S;
	bool			Found;
	string			result = ""; // Default return value
	switch (mode)
	{
	case 0:							// Generators.Name read
		result = "";
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pGen = (TGeneratorObj*)ActiveCircuit[ActiveActor]->Generators.Get_Active();
			if (pGen != nullptr)
			{
				result = pGen->get_Name();
			}
			else
			{
				result = ""; // signify no name
			}
		}
		break;
	case	1:						// Generators.Name Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->Generators;
			S = std::string(arg);
			Found = false;
			ActiveSave = with0.get_myActiveItem();
			Gen = (TGeneratorObj*)with0.Get_First();
			while (Gen != nullptr)
			{
				if (CompareText(Gen->get_Name(), S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(Gen);
					Found = true;
					break;
				}
				Gen = (TGeneratorObj*)with0.Get_Next();
			}
			if (!Found)
			{
				DoSimpleMsg("Generator \"" + S + "\" Not Found in Active Circuit.", 5003);
				Gen = (TGeneratorObj*)with0.Get(ActiveSave); // Restore active generator
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(Gen);
			}
		}
		break;
	default:
		result = "Error, Parameter not recognized";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall GeneratorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TGeneratorObj* GenElem;
	TGenerator*		GeneratorClass;
	int k;
	switch (mode)
	{
	case    0:						// Generators.AllNames
		*myType = 4;		//String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->Generators.get_myNumList() > 0)
			{
				GenElem = (TGeneratorObj*) with0->Generators.Get_First();
				while (GenElem != nullptr)
				{
					WriteStr2Array(GenElem->get_Name());
					WriteStr2Array(Char0());
					GenElem = (TGeneratorObj*)with0->Generators.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		
		break;
	case	1:						// Generators.RegisterNames
		*myType = 4;		//String
		myStrArray.resize(0);
		GeneratorClass = (TGenerator*)DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("Generator"));
		for (k = 0; k < Generator::NumGenRegisters; k++)
		{
			WriteStr2Array(GeneratorClass->RegisterNames[k]);
			WriteStr2Array(Char0());
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case	2:						// Generators.RegisterValues
		*myType = 2;		//Double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			GenElem = (TGeneratorObj*)(ActiveCircuit[ActiveActor]->Generators.Get_Active());
			if (GenElem != nullptr)
			{
				myDblArray.resize(Generator::NumGenRegisters);
				for (k = 0; k < Generator::NumGenRegisters; k++)
				{
					myDblArray[k] = GenElem->Registers[k];
				}
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		break;
	default:
		*myType = 4;        // String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the DSSElement interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall DSSElementI(int mode, int arg)
{
	int result = 0;
	switch (mode)
	{
	case 0:							  // DSSElement.NumProperties
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			TDSSObject *with0 = (TDSSObject *) ActiveDSSObject[ActiveActor];
			if (with0 != nullptr)
			{
				result = with0->ParentClass->NumProperties;
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall DSSElementS(int mode, char* arg)
{
	string result = "";

	switch (mode)
	{
	case 0:
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			TDSSObject *with0 = (TDSSObject *) ActiveDSSObject[ActiveActor];
			if (with0 != nullptr)
			{
				result = with0->ParentClass->Class_Name + "." + with0->LName;
			}
		}
		break;
	default:
		result = "Error, parameter not recognized";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall DSSElementV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	int k = 0;

	switch (mode)
	{
	case 0:
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			TDSSObject *with0 = (TDSSObject *) ActiveDSSObject[ActiveActor];
			if (with0 != nullptr)
			{
				auto with1 = with0->ParentClass;
				for (k = 0; k < with1->NumProperties; k++)
				{
					WriteStr2Array(with1->PropertyName[k]);
					WriteStr2Array(Char0());
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	default:
		*myType = 4;
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}


	//--------------------------------------------------------------------------------
	// Implements the DSSProgress interface for the DLL
	//--------------------------------------------------------------------------------
int __stdcall DSSProgressI(int mode, int arg)
{
	int result = 0;
	switch (mode)
	{
	case 0:								 // DSSProgress.PctProgress
		if (NoFormsAllowed) {
			break;
		}
		InitProgressForm();
		break;
	case 1:								 // DSSProgress.Show
		if (NoFormsAllowed) {
			InitProgressForm();
			ProgressFormCaption("");
		}
		break;
	case 2:								 // DSSProgress.Close
		if (NoFormsAllowed) {
			break;
		}
		ProgressHide();
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall DSSProgressS(int mode, char* arg)
{
	string result = "0";
	switch (mode)
	{
	case 0:								 // DSSProgress.Caption
		if (NoFormsAllowed) {
			break;
		}
		InitProgressForm();
		ProgressCaption(arg);
		break;
	default:
		result = "Error, parameter not recognized";
		break;

	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}

//--------------------------------------------------------------------------------
// Implements the DSSExecutive interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall DSSExecutiveI(int mode, int arg)
{
	int result = 0;
	switch (mode)
	{
	case 0:                                             // DSSExecutive.NumCommands
		result = NumExecCommands;
		break;
	case 1:                                             // DSSExecutive.NumOptions
		result = NumExecOptions;
		break;
	default:
		result = -1;
		break;
	}

	return result;
}
//******************************String type properties****************************
char* __stdcall DSSExecutiveS(int mode, char* arg)
{
	string result = "";
	int i = 0;
	switch (mode)
	{
	case 0:                                             // DSSExecutive.Command
		i = StrToInt(arg);
		result = ExecCommand[i - 1];
		break;
	case 1:                                             // DSSExecutive.Option
		i = StrToInt(arg);
		result = ExecOption[i - 1];
		break;
	case 2:                                             // DSSExecutive.CommandHelp
		i = StrToInt(arg);
		result = CommandHelp[i - 1];
		break;
	case 3:                                             // DSSExecutive.OptionHelp
		i = StrToInt(arg);
		result = OptionHelp[i - 1];
		break;
	case 4:                                             // DSSExecutive.OptionValue
		i = StrToInt(arg);
		DSSExecutive[ActiveActor]->Set_Command("get " + ExecOption[i - 1]);
		result = GlobalResult;
		break;

	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}

//--------------------------------------------------------------------------------
// Implements the ErrorCode interface for the DLL
//--------------------------------------------------------------------------------
int __cdecl ErrorCode() 
{
	long int result = ErrorNumber;
	ErrorNumber = 0; // Restablecer a 0 después de obtener ErrorNumber
	return result;
}

// ******************************ErrorDesc:******************************
char* __cdecl ErrorDesc() 
{
	// Suponemos que LastErrorMessage es de tipo std::string
	string result = LastErrorMessage;
	LastErrorMessage = ""; // Restablecer a cadena vacía después de obtener el mensaje

	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}

//--------------------------------------------------------------------------------
// Implements the Fuses interface for the DLL
//--------------------------------------------------------------------------------
void Set_ParameterFuse(string parm, string val)
{
	string myCmd = "";
	if (not(ASSIGNED(ActiveCircuit[ActiveActor])))
	{
		return;
	}
	SolutionAbort = false; //Reset for commands entered from outside
	myCmd = Format("Fuse.%s.%s=%s", ((TFuseObj*)(FuseClass->GetActiveObj()))->LName.c_str(), parm.c_str(), val.c_str());
	DSSExecutive[ActiveActor]->Set_Command(myCmd);
}
// ******************************int point type properties************************* 
int __stdcall FusesI(int mode, int arg)
{
	TFuseObj*	pElem = {};
	TFuseObj*	elem = {};
	int			i = 0,
				result = 0;
	TFuseObj*	pFuse = {};

	switch (mode)
	{
	case 0:
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = FuseClass->ElementList.NumInList;
		}
		break;
	case 1:														// Fuses.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TFuseObj*) FuseClass->ElementList.Get_First();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = 1;
					}
					else
						pElem = (TFuseObj*)FuseClass->ElementList.Get_Next();
				} while (!(result == 1 || pElem == nullptr));
			}
		}
		break;
	case 2:														// Fuses.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TFuseObj*)FuseClass->ElementList.Get_Next();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = FuseClass->ElementList.get_myActiveItem();
					}
					else
						pElem = (TFuseObj*)FuseClass->ElementList.Get_Next();
				} while (!(result > 0 || pElem == nullptr));
			}
		}
		break;
	case 3:														// Fuses.MonitoredTerm read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = (TFuseObj*)FuseClass->GetActiveObj();
			if (elem != nullptr)
			{
				result = elem->MonitoredElementTerminal;
			}
		}
		break;
	case 4:														// Fuses.MonitoredTerm write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = (TFuseObj*)FuseClass->GetActiveObj();
			if (elem != nullptr)
			{
				Set_ParameterFuse("monitoredterm", IntToStr(arg));
			}
		}
		break;
	case 5:														// Fuses.SwitchedTerm read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = (TFuseObj*)FuseClass->GetActiveObj();
			if (elem != nullptr)
			{
				result = elem->ElementTerminal;
			}
		}
		break;
	case 6:														// Fuses.SwitchedTerm write
		if(ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = (TFuseObj*)FuseClass->GetActiveObj();
			if (elem != nullptr)
			{
				Set_ParameterFuse("switchedterm", IntToStr(arg));
			}
		}
		break;
	case 7:														// Fuses.Open
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pFuse = (TFuseObj*)FuseClass->GetActiveObj();
			if (pFuse != nullptr)
			{
				for (i = 1; i <= pFuse->FControlledElement->Fnphases; i++)
				{
					pFuse->set_States(i, CTRL_OPEN); // Open all phases
				}
			}
		}
		break;
	case 8:														// Fuses.Close
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pFuse = (TFuseObj*)FuseClass->GetActiveObj();
			if (pFuse != nullptr)
			{
				for (i = 1; i <= pFuse->FControlledElement->Fnphases; i++)
				{
					pFuse->set_States(i, CTRL_CLOSE); // Open all phases
				}
			}
		}
		break;
	case 9:														// Fuses.IsBlown
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = (TFuseObj*)FuseClass->GetActiveObj();
			if (elem != nullptr)
			{
				for (i = 1; i <= elem->Fnphases; i++)
				{
					if (not(elem->FControlledElement->Get_ConductorClosed(i, ActiveActor)))
					{
						result = 1;
					}
				}
			}
		}
		break;
	case 10:													// Fuses.Idx read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = FuseClass->ElementList.ActiveItem;
		}
		break;
	case 11:													// Fuses.Idx write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pFuse = (TFuseObj*)FuseClass->ElementList.Get(arg);
			if (pFuse != nullptr)
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pFuse);
			}
		}
		break;
	case 12:													// Fuses.NumPhases
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pFuse = (TFuseObj*)FuseClass->GetActiveObj();
			if (pFuse != nullptr)
			{
				result = pFuse->Fnphases;
			}
		}
		break;
	case 13:													// Fuses.Reset
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pFuse = (TFuseObj*)FuseClass->GetActiveObj();
			if (pFuse != nullptr)
			{
				pFuse->Reset(ActiveActor);
			}
		}
		break;
	default:
		result = -1;
		break;
	}

	return result;
}
// ******************************floating point type properties************************* 
double __stdcall FusesF(int mode, double arg)
{

	TFuseObj*	elem = nullptr;
	double		result = 0;

	switch (mode)
	{
	case 0:														// Fuses.RatedCurrent read
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->RatedCurrent;
		}
		else
			result = -1;
		break;
	case 1:														// Fuses.RatedCurrent write
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterFuse("RatedCurrent", Format("%.8g ", arg));
		}
		break;
	case 2:														// Fuses.Delay read
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->DelayTime;
		}
		else
			result = -1.0;
		break;
	case 3:														// Fuses.Delay write
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterFuse("Delay", Format("%.8g ", arg));
		}
		break;
	default:
		result = -1.0;
		break;

	}
	return result;
}
//******************************String type properties****************************
char* __stdcall FusesS(int mode, char* arg)
{
	string result = ""; // Default return value
	TFuseObj* elem = {};
	switch (mode)
	{
	case 0:														// Fuses.Name read
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->LName;
		}
		break;
	case 1:														// Fuses.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (FuseClass->SetActive((string)arg))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*) FuseClass->ElementList.Get_Active());
			}
			else
				DoSimpleMsg("Fuse " + (string)arg + " Not Found in Active Circuit.", 77003);
		}
		break;
	case 2:														// Fuses.MonitoredObj read
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->MonitoredElementName;
		}
		break;
	case 3:														// Fuses.MonitoredObj write
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterFuse("monitoredObj", (string)arg);
		}
		break;
	case 4:														// Fuses.SwitchedObj read
		elem = (TFuseObj*)FuseClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->MonitoredElementName;
		}
		break;
	case 5:														// Fuses.SwitchedObj write
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterFuse("SwitchedObj", (string)arg);
		}
		break;
	case 6:														// Fuses.TCCcurve read
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->FuseCurve->LName;
		}
		else
			result = "No Fuse Active!";
		break;
	case 7:														// Fuses.TCCcurve write
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterFuse("FuseCurve", (string)arg);
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall FusesV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TFuseObj* elem = {};
	TPointerList* pList = {};
	int k = 0,
		i = 0,
		LoopLimit = 0;
	string S;

	switch (mode)
	{
	case 0:														// Fuses.AllNames
		*myType = 4; //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (FuseClass->ElementList.NumInList > 0)
			{
				pList = &(FuseClass->ElementList);
				elem = (TFuseObj*)pList->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->LName);
					WriteStr2Array(Char0());
					elem = (TFuseObj*)pList->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 1:														// Fuses.States - read
		*myType = 4; //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = (TFuseObj*)FuseClass->GetActiveObj();
			if (elem != nullptr)
			{
				for (i = 1; i <= elem->FControlledElement->Fnphases; i++)
				{
					if (elem->get_States(i) == CTRL_CLOSE)
					{
						WriteStr2Array("closed");
					}
					else
					{
						WriteStr2Array("open");
					}
					WriteStr2Array(Char0());
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 2:														// Fuses.States - write
		*myType = 4;//string
		k = 0;
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			for (i = 1; i <= elem->FControlledElement->Fnphases; i++)
			{
				S = BArray2Str(myPtr, &k);
				if (S.empty())
				{
					break;
				}
				else
				{
					switch (LowerCase(S)[0])
					{
					case 'o':
						elem->set_States(i, CTRL_OPEN);
						break;
					case 'c':
						elem->set_States(i, CTRL_CLOSE);
						break;
					}
				}
			}
		}
		*mySize = k;
		break;
	case 3:														// Fuses.NormalState - read
		*myType = 4; //String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = (TFuseObj*)FuseClass->GetActiveObj();
			if (elem != nullptr)
			{
				for (i = 1; i <= elem->FControlledElement->Fnphases; i++)
				{
					if (elem->get_States(i) == CTRL_CLOSE)
					{
						WriteStr2Array("closed");
					}
					else
					{
						WriteStr2Array("open");
					}
					WriteStr2Array(Char0());
				} 
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 4:														// Fuses.NormalState - write
		*myType = 4;//string
		k = 0;
		elem = (TFuseObj*)FuseClass->GetActiveObj();
		if (elem != nullptr)
		{
			for (i = 1; i <= elem->FControlledElement->Fnphases; i++)
			{
				S = BArray2Str(myPtr, &k);
				if (S.empty())
				{
					break;
				}
				else
				{
					switch (LowerCase(S)[0])
					{
					case 'o':
						elem->set_States(i, CTRL_OPEN);
						break;
					case 'c':
						elem->set_States(i, CTRL_CLOSE);
						break;
					}
				}
			}
		}
		*mySize = k;
		break;
	default:
		myStrArray.resize(0);
		WriteStr2Array("Parameter not identified");
		WriteStr2Array(Char0());
		*myType = 4;					//String
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;

	}
}

//--------------------------------------------------------------------------------
// Implements the GICSources  interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall GICSourcesI(int mode, int arg)
{
	TGICSourceObj*	pElem = nullptr;
	int				result = 0;

	switch (mode)
	{
	case 0:														// GICSources.Count
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = GICsourceClass->ElementList.NumInList;
		}
		break;
	case 1:														// GICSources.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TGICSourceObj*)GICsourceClass->ElementList.Get_First();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = 1;
					}
					else
						pElem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Next();
				} while (!(result == 1 || pElem == nullptr));
			}
		}
		break;
	case 2:														// GICSources.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Next();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = GICsourceClass->ElementList.ActiveItem;
					}
					else
						pElem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Next();
				} while (!(result > 0 || pElem == nullptr));
			}
		}

		break;
	case 3:														// GICSources.Phases read
		pElem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (pElem != nullptr)
		{
			result = pElem->Fnphases;
		}
		break;
	case 4:														// GICSources.Phases write
		pElem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (pElem != nullptr)
		{
			pElem->Fnphases = arg;
			pElem->Fnconds = arg; // Force reallocation of terminal info
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall GICSourcesF(int mode, double arg)
{
	TGICSourceObj*	elem = nullptr; // Default return value
	double			result = 0.0;

	switch (mode)
	{
	case 0:														// GICSources.EN read
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->ENorth;
		}
		break;
	case 1:														// GICSources.EN write
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->ENorth = arg;
		}
		break;
	case 2:														// GICSources.EE read
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->EEast;
		}
		break;
	case 3:														// GICSources.EE write
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->EEast = arg;
		}
		break;
	case 4:														// GICSources.Lat1 read
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->Lat1;
		}
		break;
	case 5:														// GICSources.Lat1 write
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->Lat1 = arg;
			elem->VoltsSpecified = false;
		}
		break;
	case 6:														// GICSources.Lat2 read
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->Lat2;
		}
		break;
	case 7:														// GICSources.Lat2 write
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->Lat2 = arg;
			elem->VoltsSpecified = false;
		}
		break;
	case 8:														// GICSources.Lat2 write
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->Lon1;
		}
		break;
	case 9:														// GICSources.Lat2 write
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->Lon1 = arg;
			elem->VoltsSpecified = false;
		}
		break;
	case 10:														// GICSources.Lon2 read
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->Lon2;
		}
		break;
	case 11:														// GICSources.Lon2 write
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->Lon2 = arg;
			elem->VoltsSpecified = false;
		}
		break;
	case 12:														// GICSources.Lon2 write
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->Volts;
		}
		break;
	case 13:														// GICSources.Volts write
		elem = (TGICSourceObj*)GICsourceClass->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->Volts = arg;
			elem->VoltsSpecified = false;
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall GICSourcesS(int mode, char* arg)
{
	string	S = "",
			result = "0";   // Default return value;

	switch (mode)
	{
	case 0:														// GICSources.Bus1
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->FActiveCktElement->GetBus(1);
		}
		break;
	case 1:														// GICSources.Bus2
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->FActiveCktElement->GetBus(2);
		}
		break;
	case 2:														// GICSources.Name read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->FActiveCktElement->LName;
		}
		break;
	case 3:														// GICSources.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			S = arg;
			if (GICsourceClass->SetActive(S))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TGICSourceObj*)GICsourceClass->ElementList.Get_Active());
			}
			else
				DoSimpleMsg("Vsource \"" + S + "\" Not Found in Active Circuit.", 77003);
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall GICSourcesV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TGICSourceObj* GICElem = {};
	TPointerList* ElementList = {};
	int k = 0;
	switch (mode)
	{
	case 0:														// GICSources.AllNames
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			ElementList = &(GICsourceClass->ElementList);
			if (ElementList->NumInList > 0)
			{
				GICElem = (TGICSourceObj*)ElementList->Get_First();
				while (GICElem != nullptr)
				{
					WriteStr2Array(GICElem->LName);
					WriteStr2Array(Char0());
					GICElem = (TGICSourceObj*)ElementList->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	default:
		*myType = 4; //string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Isource interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall IsourceI(int mode, int arg)
{
	TIsourceObj* pElem;
	int result = 0;
	switch (mode)
	{
	case	0:						// Isources.Count
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = IsourceClass[ActiveActor]->ElementList.get_myNumList();
		}
		break;
	case	1:					// Isources.First
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TIsourceObj*) IsourceClass[ActiveActor]->ElementList.Get_First();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->Get_Enabled())
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = 1;
					}
					else
					{
						pElem = (TIsourceObj*) IsourceClass[ActiveActor]->ElementList.Get_Next();
					}
				} while (!((result == 1) || (!ASSIGNED(pElem))));
			}
		}
		break;
	case	2:					// Isources.Next
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TIsourceObj*)IsourceClass[ActiveActor]->ElementList.Get_Next();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->Get_Enabled())
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = IsourceClass[ActiveActor]->ElementList.get_myActiveItem();
					}
					else
					{
						pElem = (TIsourceObj*)IsourceClass[ActiveActor]->ElementList.Get_Next();
					}
				} while(!( (result > 0) || (!ASSIGNED(pElem) ) ) ) ;
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall IsourceF(int mode, double arg)
{
	TIsourceObj* elem;
	double result = 0.0;
	switch (mode)
	{
	case	0:					// Isources.Amps read
		result = 0.0;
		elem = (TIsourceObj*) IsourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->Amps;
		}
		break;
	case	1:					// Isources.Amps write
		elem = (TIsourceObj*)IsourceClass[ActiveActor]->GetActiveObj();
		if (elem != nullptr)
		{
			elem->Amps = arg;
		}
		break;
	case	2:					// Isources.AngleDeg read
		result = 0.0;
		elem = (TIsourceObj*)IsourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->Angle;
		}
		break;
	case	3:					// Isources.AngleDeg write
		elem = (TIsourceObj*)IsourceClass[ActiveActor]->GetActiveObj();
		if (elem != nullptr)
		{
			elem->Angle = arg;
		}
		break;
	case	4:					// Isources.Frequency read
		result = 0.0;
		elem = (TIsourceObj*)IsourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->SrcFrequency;
		}
		break;
	case	5:					// Isources.Frequency write
		elem = (TIsourceObj*)IsourceClass[ActiveActor]->GetActiveObj();
		if (elem != nullptr)
		{
			elem->SrcFrequency = arg;
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall IsourceS(int mode, char* arg)
{
	TDSSCktElement* elem = nullptr;
	string result = ""; // Default return value
	switch (mode)
	{
	case	0:					// Isources.Name read
		result = "";
		elem = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
		if (elem != nullptr)
		{
			result = elem->get_Name();
		}
		break;
	case	1:					// Isoruces.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsourceClass[ActiveActor]->SetActive(string(arg)))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)IsourceClass[ActiveActor]->ElementList.Get_Active());
			}
			else
			{
				DoSimpleMsg("Isource \"" + std::string(arg) + "\" Not Found in Active Circuit.", 77003);
			}
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall IsourceV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TIsourceObj* elem = nullptr;
	TPointerList* pList = nullptr;
	switch (mode)
	{
	case	0:					// Isources.AllNames
		*myType = 4;		//String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IsourceClass[ActiveActor]->ElementList.get_myNumList() > 0)
			{
				pList = &(IsourceClass[ActiveActor]->ElementList);
				elem = (TIsourceObj*) pList->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->get_Name());
					WriteStr2Array(Char0());
					elem = (TIsourceObj*)pList->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	default:
		*myType = 4;		//String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}


//--------------------------------------------------------------------------------
// Implements the LineCodes interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall LineCodesI(int mode, int arg)
{
	TLineCodeObj* pLineCode = nullptr;
	int result = 0;
	switch (mode)
	{
	case	0:					// LineCodes.Count
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = LineCodeClass->Get_ElementCount();
		}
		break;
	case	1:					// LineCodes.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = LineCodeClass->Get_First();
		}
		break;
	case	2:					// LineCodes.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = LineCodeClass->Get_Next();
		}
		break;
	case	3:					// LineCodes.Units Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->Units;
		}
		break;
	case	4:					// LineCodes.Units Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			if (arg < 9)
			{
				Parser[ActiveActor]->SetCmdString(Format("units=%s", LineUnitsStr(arg).c_str()));
				with0->Edit(ActiveActor);
			}
			else
				DoSimpleMsg("Invalid line units integer sent via DLL interface.  Please enter a value within range.", 183);
		}
		break;
	case	5:					// LineCodes.Phases Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->get_Fnphases();
		}
		break;
	case	6:					// LineCodes.Phases Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			pLineCode->Set_NPhases(arg);
		}
		break;
	case	7:					// LineCodes.IsZ1Z0
		result = 1;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			if (pLineCode != nullptr)
			{
				if (pLineCode->SymComponentsModel)
				{
					result = 1;
				}
				else
				{
					result = 0;
				}
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall LineCodesF(int mode, double arg)
{
	TLineCodeObj* pLineCode = nullptr;
	double result = 0.0;
	switch (mode)
	{
	case	0:					// LineCodes.R1 Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->R1;
		}
		break;
	case	1:					// LineCodes.R1 Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			Parser[ActiveActor]->SetCmdString(Format("R1=%g", arg));
			with0->Edit(ActiveActor);
		}
		break;
	case	2:					// LineCodes.X1 Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->X1;
		}
		break;
	case	3:					// LineCodes.X1 Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			Parser[ActiveActor]->SetCmdString(Format("X1=%g", arg));
			with0->Edit(ActiveActor);
		}
		break;
	case	4:					// LineCodes.R0 Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->R0;
		}
		break;
	case	5:					// LineCodes.R0 Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			Parser[ActiveActor]->SetCmdString(Format("R0=%g", arg));
			with0->Edit(ActiveActor);
		}
		break;
	case	6:					// LineCodes.X0 Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->X0;
		}
		break;
	case	7:					// LineCodes.X0 Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			Parser[ActiveActor]->SetCmdString(Format("X0=%g", arg));
			with0->Edit(ActiveActor);
		}
		break;
	case	8:					// LineCodes.C1 Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->C1 * 1.0e9;
		}
		break;
	case	9:					// LineCodes.C1 Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			Parser[ActiveActor]->SetCmdString(Format("C1=%g", arg));
			with0->Edit(ActiveActor);
		}
		break;
	case	10:					// LineCodes.C0 Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->C0 * 1.0e9;
		}
		break;
	case	11:					// LineCodes.C0 Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			Parser[ActiveActor]->SetCmdString(Format("C0=%g", arg));
			with0->Edit(ActiveActor);
		}
		break;
	case	12:					// LineCodes.NormAmps Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->NormAmps;
		}
		break;
	case	13:					// LineCodes.NormAmps Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			pLineCode->NormAmps = arg;
		}
		break;
	case	14:					// LineCodes.EmergAmps Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			result = pLineCode->EmergAmps;
		}
		break;
	case	15:					// LineCodes.EmergAmps Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			pLineCode->EmergAmps = arg;
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall LineCodesS(int mode, char* arg)
{
	TLineCodeObj* pLineCode = nullptr;
	string result = ""; // Default return value
	switch (mode)
	{
	case	0:					// LineCodes.Name Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			if (pLineCode != nullptr)
			{
				result = pLineCode->get_Name();
			}
		}
		break;
	case	1:					// LineCodes.Name Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (!(LineCodeClass->SetActive(arg)))
			{
				DoSimpleMsg("LineCode " + (string) arg + " Not Found in Active Circuit.", 51008);
			}
		}
		break;
	default:
		result = "Parameter not identified";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall LineCodesV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TLineCodeObj* pLineCode = nullptr;
	int		i = 0, 
			j = 0, 
			k = 0;
	complex Ztemp = cmplx(0,0);
	double	Factor = 0.0;
	int*	Pint = nullptr;
	double* PDouble =nullptr;
	switch (mode)
	{
	case	0:					// LineCodes.Rmatrix Read
		*myType = 2;			//Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			myDblArray.resize(with0->get_Fnphases() * with0->get_Fnphases());
			k = 0;
			for (i = 1; i <= with0->get_Fnphases(); i++)
			{
				for (j = 1; j <= with0->get_Fnphases(); j++)
				{
					myDblArray[k] = with0->Z->GetElement(i, j).re;
					k++;
				}
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		break;
	case	1:					// LineCodes.Rmatrix Write
		*myType = 2;			///Double
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			PDouble = *(double**)myPtr;
			for (i = 1; i <= with0->get_Fnphases(); i++)
			{
				for (j = 1; j <= with0->get_Fnphases(); j++)
				{
					Ztemp = with0->Z->GetElement(i, j);
					with0->Z->SetElement(i, j, cmplx(*PDouble, Ztemp.im));
					k++;
					PDouble++;
				}
			}
		}
		*mySize = k;
		break;
	case	2:					// LineCodes.Xmatrix Read
		*myType = 2;			//Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			myDblArray.resize(with0->get_Fnphases() * with0->get_Fnphases());
			k = 0;
			for (i = 1; i <= with0->get_Fnphases(); i++)
			{
				for (j = 1; j <= with0->get_Fnphases(); j++)
				{
					myDblArray[k] = with0->Z->GetElement(i, j).im;
					k++;
				}
			}
		}

		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		break;
	case	3:					// LineCodes.Xmatrix Write
		*myType = 2;			///Double
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			PDouble = *(double**)myPtr;
			for (i = 1; i <= with0->get_Fnphases(); i++)
			{
				for (j = 1; j <= with0->get_Fnphases(); j++)
				{
					Ztemp = with0->Z->GetElement(i, j);
					with0->Z->SetElement(i, j, cmplx(Ztemp.re, *PDouble));
					k++;
					PDouble++;
				}
			}
		}
		*mySize = k;
		break;
	case	4:					// LineCodes.Cmatrix Read
		*myType = 2;			//Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			Factor = (TwoPi * with0->BaseFrequency * 1.0e-9);
			myDblArray.resize(with0->get_Fnphases() * with0->get_Fnphases());
			k = 0;
			for (i = 1; i <= with0->get_Fnphases(); i++)
			{
				for (j = 1; j <= with0->get_Fnphases(); j++)
				{
					myDblArray[k] = (with0->YC->GetElement(i, j).im) / Factor;
					k++;
				}

			}

		}

		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		break;
	case	5:					// LineCodes.Cmatrix Write
		*myType = 2;			///Double
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pLineCode = (TLineCodeObj*)LineCodeClass->GetActiveObj();
			auto with0 = pLineCode;
			Factor = TwoPi * with0->BaseFrequency * 1.0e-9;
			PDouble = *(double**)myPtr;
			for (i = 1; i <= with0->get_Fnphases(); i++)
			{
				for (j = 1; j <= with0->get_Fnphases(); j++)
				{
					with0->YC->SetElement(i, j, cmplx(0.0, (*PDouble)*Factor));
					k++;
					PDouble++;
				}
			}
		}
		*mySize = k;
		break;
	case	6:					// LineCodes.AllNames
		*myType = 4;			//String
		myStrArray.resize(0);
		*mySize = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (LineCodeClass->ElementList.get_myNumList() > 0)
			{
				k = 0;
				pLineCode = (TLineCodeObj*)LineCodeClass->ElementList.Get_First();
				while (pLineCode != nullptr)
				{
					WriteStr2Array(pLineCode->get_Name());
					WriteStr2Array(Char0());
					pLineCode = (TLineCodeObj*)LineCodeClass->ElementList.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	default:
		*myType = 4;			//String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}


//--------------------------------------------------------------------------------
// Implements the LoadShape interface for the DLL
//--------------------------------------------------------------------------------

TLoadShapeObj* ActiveLSObject = nullptr;

int __stdcall LoadShapeI(int mode, int arg)
{
	int iElem;
	int result = 0;
	switch (mode)
	{
	case	0:					// LoadShapes.Count
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = LoadShapeClass[ActiveActor]->ElementList.get_myNumList();
		}
		break;
	case	1:					// LoadShapes.First
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			iElem = LoadShapeClass[ActiveActor]->Get_First();
			if (iElem != 0)
			{
				ActiveLSObject = (TLoadShapeObj*)ActiveDSSObject[ActiveActor];
				result = 1;
			}
		}
		break;
	case	2:					// LoadShapes.Next
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			iElem = LoadShapeClass[ActiveActor]->Get_Next();
			if (iElem != 0)
			{
				ActiveLSObject = (TLoadShapeObj*)ActiveDSSObject[ActiveActor];
				result = iElem;
			}
		}
		break;
	case	3:					// LoadShapes.Npts read
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				result = ActiveLSObject->get_FNumPoints();
			}
		}
		break;
	case	4:
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				ActiveLSObject->Set_NumPoints(arg);
			}
		}
		break;
	case	5:					// LoadShapes.Normalize
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				ActiveLSObject->Normalize();
			}
		}
		break;
	case	6:					// LoadShapes.UseActual read
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				if (ActiveLSObject->UseActual)
				{
					result = 1;
				}
			}
		}
		break;
	case	7:					// LoadShapes.UseActual write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				if (arg == 1)
				{
					ActiveLSObject->UseActual = true;
				}
				else
				{
					ActiveLSObject->UseActual = false;
				}
			}
		}
		break;
	default:
		result = -1; // The parameter is not valid
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall LoadShapeF(int mode, double arg)
{
	double result = 0.0;
	switch (mode)
	{
	case	0:					// LoadShapes.HrInterval read
		result = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				result = ActiveLSObject->Interval;
			}
		}
		break;
	case	1:					// LoadShapes.HrInterval write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				ActiveLSObject->Interval = arg;
			}
		}
		break;
	case	2:					// LoadShapes.MinInterval read
		result = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				result = ActiveLSObject->Interval * 60.0;
			}
		}
		break;
	case	3:					// LoadShapes.MinInterval write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				ActiveLSObject->Interval = arg / 60.0;
			}
		}
		break;
	case	4:					// LoadShapes.PBase read
		result = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				result = ActiveLSObject->BaseP;
			}
		}
		break;
	case	5:					// LoadShapes.PBase write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				ActiveLSObject->BaseP = arg;
			}
		}
		break;
	case	6:					// LoadShapes.QBase read
		result = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				result = ActiveLSObject->BaseQ;
			}
		}
		break;
	case	7:					// LoadShapes.QBase write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				ActiveLSObject->BaseQ = arg;
			}
		}
		break;
	case	8:					// LoadShapes.Sinterval read
		result = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				result = ActiveLSObject->Interval * 3600.0;
			}
		}
		break;
	case	9:					// LoadShapes.Sinterval write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				ActiveLSObject->Interval = arg / 3600.0;
			}
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall LoadShapeS(int mode, char* arg)
{
	TLoadShapeObj* elem;
	string result = ""; // Default return value
	switch (mode)
	{
	case	0:						// LoadShapes.Name read
		result = "";
		elem = (TLoadShapeObj*)LoadShapeClass[ActiveActor]->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->get_Name();
		}
		break;
	case	1:					// LoadShapes.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (LoadShapeClass[ActiveActor]->SetActive(std::string(arg)))
			{
				ActiveLSObject = (TLoadShapeObj*)LoadShapeClass[ActiveActor]->ElementList.Get_Active();
				ActiveDSSObject[ActiveActor] = ActiveLSObject;
			}
			else
			{
				DoSimpleMsg("Relay '" + std::string(arg) + "' Not Found in Active Circuit.", 77003);
			}
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall LoadShapeV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TLoadShapeObj*	elem = nullptr;
	int				i = 0, 
					k = 0, 
					LoopLimit = 0;
	TPointerList*	pList = nullptr;
	complex			Sample = cmplx(0,0);
	bool			UseHour = false;
	double*			PDouble = nullptr;

	switch (mode)
	{
	case	0:					// LoadShapes.AllNames
		*myType = 4;		//String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (LoadShapeClass[ActiveActor]->ElementList.get_myNumList() > 0)
			{
				pList = &(LoadShapeClass[ActiveActor]->ElementList);
				elem = (TLoadShapeObj*)pList->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->get_Name());
					WriteStr2Array(Char0());

					// Need to sync this, otherwise we get name for one object, and data for another
					ActiveLSObject = elem;

					elem = (TLoadShapeObj*)pList->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case	1:					// LoadShapes.PMult read
		*myType = 2;		//Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				myDblArray.resize(ActiveLSObject->get_FNumPoints());
				UseHour = (ActiveLSObject->Interval == 0);
				for (k = 1; k <= ActiveLSObject->get_FNumPoints(); k++)
				{
					if (UseHour)
					{
						Sample = ActiveLSObject->GetMult(ActiveLSObject->Hours[k - 1]);
					}
					else
					{
						Sample = ActiveLSObject->GetMult(k * ActiveLSObject->Interval);
					}
					myDblArray[k - 1] = Sample.re;
				}
			}
			else
			{
				DoSimpleMsg("No active Loadshape Object found.", 61001);
			}
		}

		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		break;
	case	2:					// LoadShapes.PMult write
		*myType = 2;
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				auto with0 = ActiveLSObject;
				if (*mySize > with0->get_FNumPoints())
				{
					LoopLimit = with0->get_FNumPoints() - 1;
				}
				else
				{
					LoopLimit = *mySize - 1;
				}
				with0->PMultipliers.resize(with0->get_FNumPoints());
				PDouble = *(double**)myPtr;
				for (i = 0; i <= LoopLimit; i++)
				{
					ActiveLSObject->PMultipliers[k] = *PDouble;
					k++;
					PDouble++;
				}
			}
			else
			{
				DoSimpleMsg("No active Loadshape Object found.", 61002);
			}
		}
		*mySize = k;
		break;
	case	3:					// LoadShapes.QMult read
		*myType = 2;		//Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				if (!(ActiveLSObject->QMultipliers.empty()))
				{
					myDblArray.resize(ActiveLSObject->get_FNumPoints());
					UseHour = (ActiveLSObject->Interval == 0);
					for (k = 1; k <= ActiveLSObject->get_FNumPoints(); k++)
					{
						if (UseHour)
						{
							Sample = ActiveLSObject->GetMult(ActiveLSObject->Hours[k - 1]);
						}
						else
						{
							Sample = ActiveLSObject->GetMult(k * ActiveLSObject->Interval);
						}
						myDblArray[k - 1] = Sample.im;
					}
				}
			}
			else
			{
				DoSimpleMsg("No active Loadshape Object found.", 61001);
			}
		}

		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		break;
	case	4:					// LoadShapes.QMult write
		*myType = 2;		//Double
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				auto with0 = ActiveLSObject;
				if (*mySize > with0->get_FNumPoints())
				{
					LoopLimit = with0->get_FNumPoints() - 1;
				}
				else
				{
					LoopLimit = *mySize - 1;
				}
				with0->QMultipliers.resize(with0->get_FNumPoints());
				PDouble = *(double**)myPtr;
				for (i = 0; i <= LoopLimit; i++)
				{
					ActiveLSObject->QMultipliers[k] = *PDouble;
					k++;
					PDouble++;
				}
			}
			else
			{
				DoSimpleMsg("No active Loadshape Object found.", 61002);
			}
		}
		*mySize = k;
		break;
	case	5:					// LoadShapes.Timearray read
		*myType = 2;		//Double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				if (!(ActiveLSObject->Hours.empty()))
				{
					myDblArray.resize(ActiveLSObject->get_FNumPoints());
					for (k = 0; k < ActiveLSObject->get_FNumPoints(); k++)
					{
						myDblArray[k] = ActiveLSObject->Hours[k];
					}
				}
			}
			else
			{
				DoSimpleMsg("No active Loadshape Object found.", 61001);
			}
		}

		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		break;
	case	6:					 // LoadShapes.Timearray write
		*myType = 2;		//Double
		k = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveLSObject != nullptr)
			{
				auto with0 = ActiveLSObject;
				if (*mySize > with0->get_FNumPoints())
				{
					LoopLimit = with0->get_FNumPoints() - 1;
				}
				else
				{
					LoopLimit = *mySize - 1;
				}
				with0->Hours.resize(with0->get_FNumPoints());
				k = 0;
				PDouble = *(double**)myPtr;
				for (i = 0; i <= LoopLimit; i++)
				{
					ActiveLSObject->Hours[k] = *PDouble;
					k++;
					PDouble++;
				}
			}
			else
			{
				DoSimpleMsg("No active Loadshape Object found.", 61002);
			}
		}
		*mySize = k;
		break;
	default:
		*myType = 4;			//String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Meters interface for the DLL
//--------------------------------------------------------------------------------
int __stdcall MetersI(int mode, int arg)
{
	TEnergyMeterObj* pMeter = {};
	bool AssumeRestoration = false;
	TPDElement* PD_Element = {};
	int result = 0;

	switch (mode)
	{
	case 0:													// Meters.Rmatrix Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_First();
			if (pMeter != nullptr)
			{
				do
				{
					if (pMeter->FEnabled)
					{
						with0->Set_ActiveCktElement(pMeter);
						result = 1;
					}
					else
						pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Next();
				} while (!(result > 0 || pMeter == nullptr));
			}
			else
				result = 0; // signify no more
		}
		break;
	case 1:													// Meters.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Next();
			if (pMeter != nullptr)
			{
				do
				{
					if (pMeter->FEnabled)
					{
						with0->Set_ActiveCktElement(pMeter);
						result = 1;
					}
					else
						pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Next();
				} while (!(result > 0 || pMeter == nullptr));
			}
			else
				result = 0; // signify no more
		}
		break;
	case 2:													// Meters.Reset
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeter = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				pMeter->ResetRegisters();
			}
		}
		break;
	case 3:													// Meters.ResetAll
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			EnergyMeterClass[ActiveActor]->ResetAll(ActiveActor);
		}
		break;
	case 4:													// Meters.Sample
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeter = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				pMeter->TakeSample(ActiveActor);
			}
		}
		break;
	case 5:													// Meters.Save
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeter = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				pMeter->SaveRegisters(ActiveActor);
			}
		}
		break;
	case 6:													// Meters.MeteredTerminal read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeter = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				result = pMeter->MeteredTerminal;
			}
			else
				result = 0;
		}
		break;
	case 7:													// Meters.MeterdTerminal Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeter = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				pMeter->MeteredTerminal = arg;
				pMeter->MeteredElementChanged = true;
				pMeter->RecalcElementData(ActiveActor);
			}
		}
		break;
	case 8:													// Meters.DIFilesAreopen
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = 0;
			if (DIFilesAreOpen[ActiveActor])
			{
				result = 1;  // Global variable                     
			}
		}
		break;
	case 9:													// Meters.SampleAll
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			EnergyMeterClass[ActiveActor]->SampleAll(ActiveActor);
		}
		break;
	case 10:													// Meters.SaveAll
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			EnergyMeterClass[ActiveActor]->SaveAll(ActiveActor);
		}
		break;
	case 11:													// Meters.OpenAllDIFiles
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			EnergyMeterClass[ActiveActor]->OpenAllDIFiles(ActiveActor);
		}
		break;
	case 12:													// Meters.CloseAllDIFiles
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			EnergyMeterClass[ActiveActor]->CloseAllDIFiles(ActiveActor);
		}
		break;
	case 13:													// Meters.CountEndElements
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeter = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				result = pMeter->BranchList->ZoneEndsList->NumEnds;
			}
		}
		break;
	case 14:													// Meters.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->EnergyMeters.NumInList;
		}
		break;
	case 15:													// Meters.CountBranches
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				result = pMeter->SequenceList->NumInList;
			}
		}
		break;
	case 16:													// Meters.SequenceIndex read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				result = pMeter->SequenceList->ActiveItem;
			}
		}
		break;
	case 17:													// Meters.SequenceIndex Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				auto with1 = pMeter;
				if (arg > 0 && arg <= pMeter->SequenceList->NumInList)
				{
					with0->Set_ActiveCktElement((TEnergyMeterObj*)with1->SequenceList->Get(arg));
				}
				else
					DoSimpleMsg(Format("Invalid index for SequenceList: %d. List size is %d.", arg, with1->SequenceList->get_myNumList()), 500501);
			}
		}
		break;
	case 18:													// Meters.DoReliabilityCalc
		AssumeRestoration = false;
		if (arg == 1)
		{
			AssumeRestoration = true;
		}
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter)
			{
				pMeter->CalcReliabilityIndices(AssumeRestoration, ActiveActor);
			}
		}
		break;
	case 19:													// Meters.SeqListSize
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				result = pMeter->SequenceList->NumInList;
			}
		}
		break;
	case 20:													// Meters.TotalCustomers
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				PD_Element = (TPDElement*)pMeter->SequenceList->Get(1);
				if (ASSIGNED(PD_Element))
				{
					auto with1 = PD_Element;

					result = with0->Buses[with1->Terminals[with1->FromTerminal - 1].BusRef - 1]->BusTotalNumCustomers;
				}
			}
		}
		break;
	case 21:   // Meters.NumSections
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
				result = pMeter->SectionCount;
		}
		break;
	case 22:   // Meters.SetActiveSection
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				if ((arg > 0) && (arg <= pMeter->SectionCount))
					pMeter->ActiveSection = arg;
				else
					pMeter->ActiveSection = 0;
			}
		}
		break;
	case 23:   // Meters.OCPDeviceType
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];

			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				auto with1 = pMeter;

				if (with1->ActiveSection > 0)
					result = with1->FeederSections[with1->ActiveSection].OCPDeviceType;
			}
		}
		break;
	case 24:   // Meters.NumSectionCustomers
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				auto with1 = pMeter;
				if (with1->ActiveSection > 0)
					result = with1->FeederSections[with1->ActiveSection].NCustomers;
			}
		}
		break;
	case 25: // Meters.NumSectionBranches
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				auto with1 = pMeter;
				if (with1->ActiveSection > 0)
					result = with1->FeederSections[with1->ActiveSection].NBranches;
			}
		}
		break;
	case 26:  // Meters.SectSeqidx
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				auto with1 = pMeter;
				if (with1->ActiveSection > 0)
					result = with1->FeederSections[with1->ActiveSection].SeqIndex;
			}
		}
		break;
	case 27:   // Meters.SectTotalCust
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				auto with1 = pMeter;
				if (with1->ActiveSection > 0)
					result = with1->FeederSections[with1->ActiveSection].TotalCustomers;
			}
		}
		break;
	default:
			result = -1; // The parameter is not valid
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall MetersF(int mode, double arg)
{
	TEnergyMeterObj* pMeterObj = nullptr;
	double result = 0.0; // Default return value

	switch (mode)
	{
	case 0:													// Meters.SAIFI
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeterObj = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				result = pMeterObj->SAIFI;
			}
		}
		break;
	case 1:													// Meters.SAIFIkW
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeterObj = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				result = pMeterObj->SAIFIkW;
			}
		}
		break;
	case 2:													// Meters.SAIDI
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeterObj = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				result = pMeterObj->SAIDI;
			}
		}
		break;
	case 3:													// Meters.CustInterrupts
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeterObj = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				result = pMeterObj->CustInterrupts;
			}
		}
		break;
	case 4:													// Meters.AvgRepairTime
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeterObj = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				auto with1 = pMeterObj;
				if (with1->ActiveSection > 0)
				{
					result = with1->FeederSections[with1->ActiveSection].AverageRepairTime;
				}

			}
		}
		break;
	case 5:													// Meters.FaultRateXRepairHrs
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeterObj = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				auto with1 = pMeterObj;
				if (with1->ActiveSection > 0)
				{
					result = with1->FeederSections[with1->ActiveSection].SumFltRatesXRepairHrs;
				}
			}
		}
		break;
	case 6:													// Meters.SumBranchFltRates
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeterObj = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				auto with1 = pMeterObj;
				if (with1->ActiveSection > 0)
				{
					result = with1->FeederSections[with1->ActiveSection].SumBranchFltRates;
				}
			}
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall MetersS(int mode, char* arg)
{
	TEnergyMeterObj*	pMeterObj = {};
	int					ActiveSave = 0;
	string				TestStr = "";
	bool				Found = false;
	string				result = "0"; // Default return value

	switch (mode)
	{
	case 0:													// Meters.Name  Read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeterObj = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				result = pMeterObj->LName;
			}
		}
		break;
	case 1:													// Meters.Name Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->EnergyMeters;
			TestStr = arg;
			Found = false;
			ActiveSave = with0.ActiveItem;
			pMeterObj = (TEnergyMeterObj*)with0.Get_First();
			while (pMeterObj != nullptr)
			{
				if (CompareText(pMeterObj->LName, TestStr) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pMeterObj);
					Found = true;
					break;
				}
				pMeterObj = (TEnergyMeterObj*)with0.Get_Next();
			}
			if (not(Found))
			{
				DoSimpleMsg("EnergyMeter \"" + TestStr + "\" Not Found in Active Circuit.", 5005);
				pMeterObj = (TEnergyMeterObj*)with0.Get(ActiveSave);    // Restore active Meter
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pMeterObj);
			}
		}
		break;
	case 2:													// Meters.MeteredElement read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeterObj = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				result = pMeterObj->ElementName;
			}
			else
				result = "";
		}
		break;
	case 3:													// Meters.MeteredElement Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeterObj = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				pMeterObj->ElementName = arg;
				pMeterObj->MeteredElementChanged = true;
				pMeterObj->RecalcElementData(ActiveActor);
			}
		}
		break;
	default:
		result = "Error, Parameter not recognized";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall MetersV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TEnergyMeterObj*	pMeter = nullptr;
	TEnergyMeterObj*	pMeterObj = nullptr;
	TEnergyMeterObj*	MeterElem = nullptr;
	int					BranchCount = 0,
						last = 0,
						k = 0,
						i = 0;
	TDSSCktElement*		cktElem = nullptr;
	TDSSCktElement*		shuntElement = nullptr;
	TDSSCktElement*		pElem = nullptr;
	TDSSCktElement*		elem = nullptr;
	TCktTreeNode*		node = nullptr;
	StringArray*		MyPCEList;
	double*				pDouble = nullptr;


	switch (mode)
	{
	case 0:													// Meters.AllNames
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->EnergyMeters.NumInList > 0)
			{
				MeterElem = (TEnergyMeterObj*)with0->EnergyMeters.Get_First();
				while (MeterElem != nullptr)
				{
					WriteStr2Array(MeterElem->LName);
					WriteStr2Array(Char0());
					MeterElem = (TEnergyMeterObj*)with0->EnergyMeters.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 1:													// Meters.RegisterNames
		*myType = 4; //string
		myStrArray.resize(0);
		pMeter = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
		if (ASSIGNED(pMeter))
		{
			for (k = 0; k <= NumEMRegisters - 1; k++)
			{
				WriteStr2Array(pMeter->RegisterNames[k]);
				WriteStr2Array(Char0());
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 2:													// Meters.RegisterValues
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor])
		{
			pMeter = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (ASSIGNED(pMeter))
			{
				myDblArray.resize(NumEMRegisters);
				for (k = 0; k <= NumEMRegisters - 1; k++)
				{
					myDblArray[k] = pMeter->Registers[k];
				}
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 3:													// Meters.Totals
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor])
		{
			auto with0 = ActiveCircuit[ActiveActor];
			with0->TotalizeMeters();
			myDblArray.resize(NumEMRegisters);
			for (i = 1; i <= NumEMRegisters; i++)
			{
				myDblArray[i - 1] = with0->RegisterTotals[i - 1];
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 4:													// Meters.PeakCurrent read
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor])
		{
			pMeterObj = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				myDblArray.resize(pMeterObj->Fnphases);
				for (k = 0; k < pMeterObj->Fnphases; k++)
				{
					myDblArray[k] = pMeterObj->SensorCurrent[k];
				}
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 5:													// Meters.PeakCurrent write
		*myType = 2; //double
		k = 1;
		if (ActiveCircuit[ActiveActor])
		{
			pMeterObj = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				pDouble = *(double**)myPtr;
				for (i = 0; i < pMeterObj->Fnphases; i++)
				{
					pMeterObj->SensorCurrent[i] = *pDouble;
					k++;
					pDouble++;
				}
			}
		}
		*mySize = k - 1;
		break;
	case 6:													// Meters.CalCurrent read
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor])
		{
			pMeterObj = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				myDblArray.resize(pMeterObj->Fnphases);
				for (k = 0; k < pMeterObj->Fnphases; k++)
				{
					myDblArray[k] = cabs(pMeterObj->CalculatedCurrent[k]);
				}
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 7:													// Meters.CalcCurrent Write
		*myType = 2; //double
		k = 1;
		if (ActiveCircuit[ActiveActor])
		{
			pMeterObj = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				pDouble = *(double**)myPtr;
				for (i = 0; i < pMeterObj->Fnphases; i++)
				{
					pMeterObj->CalculatedCurrent[i] = cmplx(*pDouble, 0.0);
					k++;
					pDouble++;
				}
			}
		}
		*mySize = k - 1;
		break;
	case 8:													// Meters.AllocFactors read
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeterObj = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				myDblArray.resize(pMeterObj->Fnphases);
				for (i = 0; i < pMeterObj->Fnphases; i++)
				{
					myDblArray[k] = pMeterObj->PhsAllocationFactor[k];
				}
			}
		}
		*mySize =myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 9:													// Meters.AllocFactors Write
		*myType = 2; //Double
		k = 1;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMeterObj = (TEnergyMeterObj*)ActiveCircuit[ActiveActor]->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				pDouble = *(double**)myPtr;
				for (i = 0; i < pMeterObj->Fnphases; i++)
				{
					pMeterObj->PhsAllocationFactor[i] = *pDouble; // Just set the real part
					k++;
					pDouble++;
				}
			}
			*mySize = k - 1;
		}
		break;
	case 10:													// Meters.AllEndElements
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeterObj = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				last = pMeterObj->BranchList->ZoneEndsList->NumEnds - 1;
				for (k = 0; k <= last; k++)
				{
					pMeterObj->BranchList->ZoneEndsList->Get(k + 1, node);
					elem = (TDSSCktElement*)node->CktObject;
					WriteStr2Array(Format("%s.%s", elem->ParentClass->Class_Name.c_str(), elem->LName.c_str()));
					WriteStr2Array(Char0());
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 11:													// Meters.AllBranchesInZone
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeterObj = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeterObj != nullptr)
			{
				BranchCount = MetersI(15, 0);

				if (BranchCount > 0)
				{
					pElem = (TDSSCktElement*)pMeterObj->BranchList->Get_First();
					while (pElem != nullptr)
					{
						WriteStr2Array(Format("%s.%s", pElem->ParentClass->Class_Name.c_str(), pElem->LName.c_str()));
						WriteStr2Array(Char0());
						pElem = (TDSSCktElement*)pMeterObj->BranchList->Get_Forward();
					}
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 12:													// Meters.AllPCEinZone
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMeter = (TEnergyMeterObj*)with0->EnergyMeters.Get_Active();
			if (pMeter != nullptr)
			{
				pMeter->GetPCEatZone();
				if ((pMeter->ZonePCE.size() > 0) && !(pMeter->ZonePCE[0].empty()))
				{
					for (k = 0; k < pMeter->ZonePCE.size(); k++)
					{
						WriteStr2Array(pMeter->ZonePCE[k]);
						WriteStr2Array(Char0());

					}
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	default:
		*myType = 4; // string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Monitors interface for the DLL
//--------------------------------------------------------------------------------

struct THeaderRec {
	int Signature;
	int Version;
	int RecordSize;
	int Mode;
	TMonitorStrBuffer dummyRec;
};

void ReadMonitorHeader(THeaderRec* HeaderRec, bool Opt)
{
	TMonitorObj* mypMon = {};
	mypMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
	// This is different since the memory streams are not native to C, it was implemented exernally to mimic its behavior
	auto &with0 = mypMon->MonitorStream;
	auto with1 = HeaderRec;

	with0.begin();
	with0.Read(&(with1->Signature), sizeof(with1->Signature));
	with0.Read(&(with1->Version), sizeof(with1->Version));
	with0.Read(&(with1->RecordSize), sizeof(with1->RecordSize));
	with0.Read(&(with1->Mode), sizeof(with1->Mode));
	with0.Read((uint8_t*)&with1->dummyRec[0], sizeof(with1->dummyRec));
	if (Opt)
	{
		// If opt is false leave monitorstream at end of header record
		mypMon->MonitorStream.end();	// put monitor stream pointer back where it was
	}

}
// ******************************int type properties************************* 
int __stdcall MonitorsI(int mode, int arg)
{
	TMonitorObj*	pMon = {};
	THeaderRec		Header;
	int				result = 0;
	switch (mode)
	{
	case 0:													// Monitors.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_First();
			if (pMon != nullptr)
			{
				do
				{
					if (pMon->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pMon);
						result = 1;
					}
					else
						pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Next();
				} while (!((result == 1) || (pMon == nullptr)));
			}
		}
		break;
	case 1:													// Monitors.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Next();
			if (pMon != nullptr)
			{
				do
				{
					if (pMon->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pMon);
						result = 1;
					}
					else
						pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Next();
				} while (!((result == 1) || (pMon == nullptr)));
			}
		}
		break;
	case 2:													// Monitors.Reset
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				pMon->ResetIt(ActiveActor);
			}
		}
		break;
	case 3:													// Monitors.ResetAll
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			MonitorClass[ActiveActor]->ResetAll(ActiveActor);
		}
		break;
	case 4:													// Monitors.Sample
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				pMon->TakeSample(ActiveActor);
			}
		}
		break;
	case 5:													// Monitors.Save
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				pMon->Save();
			}
		}
		break;
	case 6:													// Monitors.Show
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				pMon->TranslateToCSV(true, ActiveActor);
			}
		}
		break;
	case 7:													// Monitors.Mode read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				result = pMon->Mode;
			}
		}
		break;
	case 8:													// Monitors.Mode write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				pMon->Mode = arg;
				pMon->ResetIt(ActiveActor); // Always reset the monitor after a Mode change
			}
		}
		break;
	case 9:													// Monitors.SampleCount
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			result = pMon->SampleCount;
		}
		break;
	case 10:													// Monitors.SampleAll
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			MonitorClass[ActiveActor]->SampleAll(ActiveActor);
		}
		break;
	case 11:													// Monitors.SaveAll
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			MonitorClass[ActiveActor]->SaveAll(ActiveActor);
		}
		break;
	case 12:													// Monitors.Count
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->Monitors.NumInList;
		}
		break;
	case 13:													// Monitors.Process
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			pMon->PostProcess(ActiveActor);
		}
		break;
	case 14:													// Monitors.ProcessAll
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			MonitorClass[ActiveActor]->PostProcessAll(ActiveActor);
		}
		break;
	case 15:													// Monitors.FileVersion
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			ReadMonitorHeader(&Header, false);// leave at beginning of data;
			result = Header.Version;
		}
		break;
	case 16:													// Monitors.RecordSize
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			ReadMonitorHeader(&Header, true);
			result = Header.RecordSize;
		}
		break;
	case 17:													// Monitors.NumChannels
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			ReadMonitorHeader(&Header, true);
			result = Header.RecordSize;
		}
		break;
	case 18:													// Monitors.Terminal read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				result = pMon->MeteredTerminal;
			}
		}
		break;
	case 19:													// Monitors.Terminal write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				pMon->MeteredTerminal = arg;
				pMon->RecalcElementData(ActiveActor);
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall MonitorsS(int mode, char* arg)
{
	TMonitorObj*	pMon = nullptr;
	THeaderRec		Header;
	int				activesave = 0;
	bool			Found = false;
	string			S = "",
					result = "0"; // Default return value

	switch (mode)
	{
	case 0:													// Monitors.FileName
		result = "";
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				result = pMon->Get_FileName(ActiveActor); // Default return value
			}
		}
		break;
	case 1:													// Monitors.Name read
		result = "";
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				result = pMon->LName; // Default return value
			}
		}
		break;
	case 2:													// Monitors.Name Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0	= ActiveCircuit[ActiveActor]->Monitors;
			S			= arg;
			Found		= false;
			pMon		= (TMonitorObj*)with0.Get_First();
			while (pMon != nullptr)
			{
				if (CompareText(pMon->LName, S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pMon);
					Found = true;
					break;
				}
				pMon = (TMonitorObj*)with0.Get_Next();
			}
			if (not(Found))
			{
				DoSimpleMsg("Monitor \"" + S + "\" Not Found in Active Circuit.", 5004);
				pMon = (TMonitorObj*)with0.Get(activesave);    // Restore active Monerator
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pMon);
			}
		}
		result = "";
		break;
	case 3:													// Monitors.Element read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				result = pMon->ElementName; // Default return value
			}
		}
		break;
	case 4:													// Monitors.Element write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				S = arg;
				pMon->ElementName = S;
				pMon->FPropertyValue[1] = S;
				pMon->RecalcElementData(ActiveActor);
			}
		}
		result = "Error, parameter not valid";
		break;
	default:
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall MonitorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TMonitorObj*	MonitorElem = nullptr;
	TMonitorObj*	pMon = nullptr;
	int				AllocSize = 0,
					i = 0, 
					k = 0, 
					index = 0,
					ListSize = 0;
	THeaderRec		Header;
	string			TempStr = "",
					SaveDelims = "",
					SaveWhiteSpace = "",
					FirstCol = "";
	float hr = 0.0, s = 0.0;
	float sngFreq = 0.0, sngHarm= 0.0;
	vector<float>	SngBuffer;
	int*			PInt = nullptr;


	switch (mode)
	{
	case 0:													// Monitors.AllNames
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->Monitors.NumInList > 0)
			{
				MonitorElem = (TMonitorObj*)with0->Monitors.Get_First();
				while (MonitorElem != nullptr)
				{
					WriteStr2Array(MonitorElem->LName);
					WriteStr2Array(Char0());
					MonitorElem = (TMonitorObj*)with0->Monitors.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 1:													// Monitors.ByteStream
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon != nullptr)
			{
				myStrArray.resize(pMon->MonitorStream.Size());
				pMon->MonitorStream.begin();
				pMon->MonitorStream.Read(&(myStrArray[0]), pMon->MonitorStream.Size());
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 2:													// Monitors.Header
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon->StrBuffer.size() > 0)
			{
				ReadMonitorHeader(&Header, false);// leave at beginning of data;
				ListSize = Header.RecordSize;
				SaveDelims = AuxParser[ActiveActor]->DelimChars;
				AuxParser[ActiveActor]->DelimChars = ",";
				SaveWhiteSpace = AuxParser[ActiveActor]->WhiteSpaceChars;
				AuxParser[ActiveActor]->WhiteSpaceChars = "";
				AuxParser[ActiveActor]->SetCmdString(pMon->StrBuffer.data());
				AuxParser[ActiveActor]->FAutoIncrement = true;
				AuxParser[ActiveActor]->MakeString_();
				AuxParser[ActiveActor]->MakeString_();
				k = 0;
				while (k < ListSize)
				{
					WriteStr2Array(AuxParser[ActiveActor]->MakeString_());
					WriteStr2Array(Char0());
					k++;
				}
				AuxParser[ActiveActor]->FAutoIncrement = false;
				AuxParser[ActiveActor]->DelimChars = SaveDelims;
				AuxParser[ActiveActor]->WhiteSpaceChars = SaveWhiteSpace;
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 3:													// Monitors.dblHour
		*myType = 2; // double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon->SampleCount > 0)
			{
				myDblArray.resize(pMon->SampleCount);
				ReadMonitorHeader(&Header, false);// leave at beginning of data
				AuxParser[ActiveActor]->SetCmdString(pMon->StrBuffer.data());
				AuxParser[ActiveActor]->FAutoIncrement = true;
				FirstCol = AuxParser[ActiveActor]->MakeString_();
				AuxParser[ActiveActor]->FAutoIncrement = false;
				// check first col to see if it is "Hour"
				if (CompareText(FirstCol, "hour") == 0)
				{
					AllocSize = sizeof(float) * Header.RecordSize;
					SngBuffer.resize(Header.RecordSize);
					k = 0;
					auto& monStream = pMon->MonitorStream;
					for (i = 1; i <= pMon->SampleCount; i++)
					{
						monStream.Read(&hr, sizeof(hr));// Hour
						monStream.Read(&s, sizeof(s));// Seconds past the hour
						monStream.Read(&(SngBuffer[0]), AllocSize);
						myDblArray[k] = hr + s / 3600.0;
						k++;
					}
					SngBuffer.clear(); // Dispose of buffer
				}
				else // Not time solution, so return nil array
					pMon->MonitorStream.end();
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 4:													// Monitors.dblFreq
		*myType = 2; // double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon->SampleCount > 0)
			{
				myDblArray.resize(pMon->SampleCount);
				ReadMonitorHeader(&Header, false);// leave at beginning of data
				AuxParser[ActiveActor]->SetCmdString(pMon->StrBuffer.data());
				AuxParser[ActiveActor]->FAutoIncrement = true;
				FirstCol = AuxParser[ActiveActor]->MakeString_();
				AuxParser[ActiveActor]->FAutoIncrement = false;
				// check first col to see if it is "freq"
				if (CompareText(FirstCol, "freq") == 0)
				{
					AllocSize = sizeof(float) * Header.RecordSize;
					SngBuffer.resize(AllocSize);
					k = 0;
					auto& monStream = pMon->MonitorStream;
					for (i = 1; i <= pMon->SampleCount; i++)
					{
						monStream.Read(&sngFreq, sizeof(sngFreq));// frequency
						monStream.Read(&sngHarm, sizeof(sngHarm));// harmonic
						monStream.Read(&(SngBuffer[0]), AllocSize);
						myDblArray[k] = sngFreq;
						k++;
					}
					SngBuffer.clear(); // Dispose of buffer
				}
				else // Not time solution, so return nil array
					pMon->MonitorStream.end();
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 5:													// Monitors.Channel
		*myType = 2; // double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pMon = (TMonitorObj*)ActiveCircuit[ActiveActor]->Monitors.Get_Active();
			if (pMon->SampleCount > 0)
			{
				PInt = *(int**)myPtr;
				index = *PInt;
				myDblArray.resize(pMon->SampleCount);
				ReadMonitorHeader(&Header, false);// leave at beginning of data
				AuxParser[ActiveActor]->SetCmdString(pMon->StrBuffer.data());
				AuxParser[ActiveActor]->FAutoIncrement = true;
				FirstCol = AuxParser[ActiveActor]->MakeString_();
				AuxParser[ActiveActor]->FAutoIncrement = false;
				AuxParser[ActiveActor]->FAutoIncrement = false;
				AllocSize = sizeof(float) * Header.RecordSize;
				SngBuffer.resize(AllocSize);

				k = 0;
				auto& monStream = pMon->MonitorStream;
				for (i = 1; i <= pMon->SampleCount; i++)
				{
					monStream.Read(&hr, sizeof(hr));
					monStream.Read(&s, sizeof(s));
					monStream.Read((&(SngBuffer[0])), AllocSize);

					myDblArray[k] = (double)SngBuffer[index];
					k++;
				}
				SngBuffer.clear(); // Dispose of buffer
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	default:
		*myType = 4;        // String
		myStrArray.clear();
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr	= (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Parallel interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall ParallelI(int mode, int arg)
{
	int result = 0; //Default Return Value
	switch (mode)
	{
	case 0:													// Parallel.NumCPUs 
		result = CPU_Cores;
		break;
	case 1:													// Parallel.NumCores
		result = round(CPU_Cores / 2);
		break;
	case 2:													// Parallel.ActiveActor read
		result = ActiveActor;
		break;
	case 3:													// Parallel.ActiveActor write
		if (arg <= NumOfActors)
		{
			ActiveActor = arg;
		}
		else
			DoSimpleMsg("The actor does not exist", 7002);
		break;
	case 4:													// Parallel.CreateActor
		New_Actor_Slot();
		break;
	case 5:													// Parallel.ActorCPU read
		result = ActorCPU[ActiveActor];
		break;
	case 6:													// Parallel.ActorCPU write
		if (arg < CPU_Cores)
		{
			ActorCPU[ActiveActor] = arg;
			if (ActorHandle[ActiveActor] != nullptr)
			{
				// in this version we don't provide hread affinity support given the cross platform compatibility
				ActorHandle[ActiveActor]->Doterminate(); 
			}
		}
		break;
	case 7:													// Parallel.NumActors
		result = NumOfActors;
		break;
	case 8:													// Parallel.Wait
		if (Parallel_enabled)
		{
			Wait4Actors(0);
		}
		break;
	case 9:													// Parallel.ActiveParallel read
		if (Parallel_enabled)
		{
			result = 1;
		}
		else result = 0;
		break;
	case 10:													// Parallel.ActiveParallel write
		if (arg != 0)
		{
			Parallel_enabled = true;
		}
		else
			Parallel_enabled = false;
		break;
	case 11:													// Parallel.ConcatenateReports Read
		if (ConcatenateReports)
		{
			result = 1;
		}
		else result = 0;
		break;
	case 12:													// Parallel.ConcatenateReports Write
		if (ConcatenateReports)
		{
			ConcatenateReports = true;
		}
		else ConcatenateReports = false;
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//************************Structure type properties*******************************
void __stdcall ParallelV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	int i = 0;

	switch (mode)
	{
	case 0:													// Parallel.ActorProgress
		*myType = 1; //integer
		myIntArray.resize(NumOfActors);
		for (i = 1; i <= NumOfActors; i++)
		{
			myIntArray[i - 1] = ActorPctProgress[i];
		}
		*mySize =myIntArray.size() * sizeof(myIntArray[0]);
		*myPtr = (uintptr_t)(void*)&(myIntArray[0]);
		break;
	case 1:													// Parallel.ActorStatus
		*myType = 1; //integer
		myIntArray.resize(NumOfActors);
		for (i = 1; i <= NumOfActors; i++)
		{
			myIntArray[i - 1] = ActorStatus[i];
		}
		*mySize = myIntArray.size() * sizeof(int);
		*myPtr = (uintptr_t)(void*)&(myIntArray[0]);
		break;
	default:
		*myType = 4; //string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

TParser* ComParser = new TParser;

//--------------------------------------------------------------------------------
// Implements the Parser  interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall ParserI(int mode, int arg)
{

	int result = 0;
	switch (mode)
	{
	case 0:													// Parser.IntValue
		result = ComParser->MakeInteger_();
		break;
	case 1:													// Parser.ResetDelimeters
		ComParser->ResetDelims();
		break;
	case 2:													// Parser.AutoIncrement read
		if (ComParser->FAutoIncrement)
		{
			result = 1;
		}
		break;
	case 3:													// Parser.AutoIncrement write
		if (arg == 1)
		{
			ComParser->FAutoIncrement = true;
		}
		else
			ComParser->FAutoIncrement = false;
		break;
	default:
		int result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall ParserF(int mode, double arg)
{
	double result = 0.0;
	switch (mode)
	{
	case 0:													// Parser.DblValue
		result = ComParser->MakeDouble_();
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall ParserS(int mode, char* arg)
{
	string result = 0;
	switch (mode)
	{
	case 0:													// Parser.CmdString read - CHECK
		result = ComParser->CmdBuffer;
		break;
	case 1:													// Parser.CmdString write
		ComParser->SetCmdString(arg);
		break;
	case 2:													// Parser.NextParam
		result = ComParser->GetNextParam();
		break;
	case 3:													// Parser.StrValue
		result = ComParser->MakeString_();
		break;
	case 4:													// Parser.WhiteSpace read
		result = ComParser->WhiteSpaceChars;
		break;
	case 5:													// Parser.WhiteSpace write
		ComParser->set_WhiteSpaceChars(arg);
		break;
	case 6:													// Parser.BeginQuote read
		result = ComParser->FBeginQuoteChars;
		break;
	case 7:													// Parser.BeginQuote write
		ComParser->set_FBeginQuoteChars(arg);
		break;
	case 8:													// Parser.EndQuote read
		result = ComParser->FEndQuoteChars;
		break;
	case 9:													// Parser.EndQuote write
		ComParser->FEndQuoteChars =arg;
		break;
	case 10:													// Parser.Delimiters read
		result = ComParser->DelimChars;
		break;
	case 11:													// Parser.Delimiters write
		ComParser->set_delimchars(arg);
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall ParserV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	int				i = 0,
					ActualSize = 0,
					ExpectedSize = 0,
					ExpectedOrder = 0,
					MatrixSize = 0;
	vector<double>	VectorBuffer,
					MatrixBuffer;

	switch (mode)
	{
	case 0:													// Parser.Vector
		*myType = 2; //double
		myDblArray.resize(1);
		ExpectedSize = int(*mySize);
		VectorBuffer.resize(ExpectedSize + 1);
		ActualSize = ComParser->ParseAsVector(ExpectedSize, &(VectorBuffer[1]));
		myDblArray.resize(ActualSize);
		for (i = 0; i <= ActualSize - 1; i++)
		{
			myDblArray[i] = VectorBuffer [i + 1];
		}
		VectorBuffer.clear();
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 1:													// Parser.Matrix
		*myType = 2; //double
		myDblArray.resize(1);
		ExpectedOrder = int(*mySize);
		MatrixSize = ExpectedOrder * ExpectedOrder;
		MatrixBuffer.resize(MatrixSize + 1);
		ComParser->ParseAsMatrix(ExpectedOrder, &(MatrixBuffer[1]));
		myDblArray.resize(MatrixSize);
		for (i = 0; i <= MatrixSize - 1; i++)
		{
			myDblArray[i] = MatrixBuffer[i + 1];
		}
		MatrixBuffer.clear();
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 2:													// Parser.SymMatrix
		*myType = 2; //double
		myDblArray.resize(1);
		ExpectedOrder = int(*mySize);
		MatrixSize = ExpectedOrder * ExpectedOrder;
		MatrixBuffer.resize(MatrixSize + 1);
		ComParser->ParseAsSymMatrix(ExpectedOrder, &(MatrixBuffer[1]));
		myDblArray.resize(MatrixSize);
		for (i = 0; i <= MatrixSize - 1; i++)
		{
			myDblArray[i] = MatrixBuffer[i + 1];
		}
		MatrixBuffer.clear();
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	default:
		*myType = 4; //string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size() ;
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the PDElements   interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall PDElementsI(int mode, int arg)
{
	TPDElement* ActivePDElement = nullptr;
	int			result = 0;
	switch (mode)
	{
	case 0:													// PDElementsI.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			result = with0->PDElements.NumInList;
		}
		break;
	case 1:													// PDElementsI.First
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			ActivePDElement = (TPDElement*)with0->PDElements.Get_First();
			if (ActivePDElement != nullptr)
			{
				do
				{
					if (ActivePDElement->FEnabled)
					{
						result = 1;
						with0->Set_ActiveCktElement(ActivePDElement);
					}
					else
						ActivePDElement = (TPDElement*)with0->PDElements.Get_Next();
				} while (!((result == 1) || (ActivePDElement == nullptr)));
			}
		}
		break;
	case 2:													// PDElementsI.Next
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			ActivePDElement = (TPDElement*)with0->PDElements.Get_Next();
			if (ActivePDElement != nullptr)
			{
				do
				{
					if (ActivePDElement->FEnabled)
					{
						result = 1;
						with0->Set_ActiveCktElement(ActivePDElement);
					}
					else
						ActivePDElement = (TPDElement*)with0->PDElements.Get_Next();
				} while (!((result > 0) || (ActivePDElement == nullptr)));
			}
		}
		break;
	case 3:													// PDElementsI.IsShunt
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				if (ActivePDElement->IsShunt)
				{
					result = 1;
				}
			}
		}
		break;
	case 4:													// PDElementsI.NumCustomers
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->BranchNumCustomers;
				
			}
		}
		break;
	case 5:													// PDElementsI.TotalCustomers
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->BranchTotalCustomers;
				
			}
		}
		break;
	case 6:													// PDElementsI.ParentPDElement
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				if (ActivePDElement->ParentPDElement != nullptr)
				{
					with0->Set_ActiveCktElement(ActivePDElement->ParentPDElement);
					result = with0->FActiveCktElement->ClassIndex;
				}								
			}
		}
		break;
	case 7:													// PDElementsI.FromTerminal
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->FromTerminal;				
			}
		}
		break;
	case 8:													// PDElementsI.SectionID
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->BranchSectionID;
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
// ******************************floating point type properties************************* 
double __stdcall PDElementsF(int mode, double arg)
{
	TPDElement* ActivePDElement = {};
	double		result = 0.0;
	switch (mode)
	{
	case 0:													// PDElementsI.FaultRate read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->FaultRate;
			}
		}
		break;
	case 1:													// PDElementsI.FaultRate write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				ActivePDElement->FaultRate = arg;
			}
		}
		break;
	case 2:													// PDElementsI.PctPermanent read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->PctPerm;
			}
		}
		break;
	case 3:													// PDElementsI.PctPermanent write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				ActivePDElement->PctPerm = arg;
			}
		}
		break;
	case 4:													// PDElementsI.Lambda
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->BranchFltRate;
			}
		}
		break;
	case 5:													// PDElementsI.AccumulatedL
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->AccumulatedBrFltRate;
			}
		}
		break;
	case 6:													// PDElementsI.RepairTime
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->HrsToRepair;
			}
		}
		break;
	case 7:													// PDElementsI.TotalMiles
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				result = ActivePDElement->AccumulatedMilesDownStream;
			}
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall PDElementsS(int mode, char* arg)
{
	TPDElement* ActivePDElement = nullptr;
	string		result = "", // Default return value
				TestString = "";

	switch (mode)
	{
	case 0:													// PDElementsI.Name read
		result = ""; // return null if not a PD element
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (dynamic_cast<TPDElement*>(with0->FActiveCktElement) != nullptr)
			{
				ActivePDElement = (TPDElement*)with0->FActiveCktElement;
				auto with1 = ActivePDElement;
				result = Format("%s.%s", with1->ParentClass->Class_Name.c_str(), with1->LName.c_str()); // full name
			}
		}
		break;
	case 1:													// PDElementsI.Name write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			TestString = arg;
			// Search through list of PD Elements until we find this one
			ActivePDElement = (TPDElement*)with0->PDElements.Get_First();
			while (ASSIGNED(ActivePDElement))
			{
				auto with1 = ActivePDElement;
				if (CompareText(TestString, Format("%s.%s", with1->ParentClass->Class_Name.c_str(), with1->LName.c_str())))
				{
					with0->Set_ActiveCktElement(ActivePDElement);
					break;
				}
				ActivePDElement = (TPDElement*)with0->PDElements.Get_Next();
			}
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}

//--------------------------------------------------------------------------------
// Implements the PVsystems    interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall PVsystemsI(int mode, int arg)
{
	TPVsystemObj*	pPVSystem = nullptr;
	int				result = 0;
	
	switch (mode)
	{
	case 0:													// PVsystems.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->PVSystems.NumInList;
		}
		break;
	case 1:													// PVsystems.First
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pPVSystem = (TPVsystemObj*)ActiveCircuit[ActiveActor]->PVSystems.Get_First();
			if (pPVSystem != nullptr)
			{
				do
				{
					if (pPVSystem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pPVSystem);
						result = 1;
					}
					else
						pPVSystem = (TPVsystemObj*)ActiveCircuit[ActiveActor]->PVSystems.Get_Next();
				} while (!((result == 1) || (pPVSystem == nullptr)));
			}
		}
		else
			result = 0;
		break;
	case 2:													// PVsystems.Next
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pPVSystem = (TPVsystemObj*)ActiveCircuit[ActiveActor]->PVSystems.Get_Next();
			if (pPVSystem != nullptr)
			{
				do
				{
					if (pPVSystem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pPVSystem);
						result = ActiveCircuit[ActiveActor]->PVSystems.ActiveItem;
					}
					else
						pPVSystem = (TPVsystemObj*)ActiveCircuit[ActiveActor]->PVSystems.Get_Next();
				} while (!((result > 0) || pPVSystem == nullptr));
			}
		}
		else
			result = 0;
		break;
	case 3:													// PVsystems.Idx read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->PVSystems.ActiveItem;
		}
		else
			result = 0;
		break;
	case 4:													// PVsystems.Idx write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pPVSystem = (TPVsystemObj*)ActiveCircuit[ActiveActor]->PVSystems.Get(arg);
			if (pPVSystem != nullptr)
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pPVSystem);
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}// ******************************floating point type properties************************* 
//******************************Float point type properties****************************
double __stdcall PVsystemsF(int mode, double arg)
{
	double result = 0.0; // Default return value
	switch (mode)
	{
	case 0:													// PVsystems.Irradiance readif (ActiveCircuit[ActiveActor] != nullptr)
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				result = ((TPVsystemObj*)with0.Get_Active())->PVSystemVars.FIrradiance;
			}
		}
		break;
	case 1:													// PVsystems.Irradiance write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				((TPVsystemObj*)with0.Get_Active())->PVSystemVars.FIrradiance = arg;
			}
		}
		break;
	case 2:													// PVsystems.kW
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				result = ((TPVsystemObj*)with0.Get_Active())->Get_PresentkW();
			}
		}
		break;
	case 3:													// PVsystems.kvar read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				result = ((TPVsystemObj*)with0.Get_Active())->Get_Presentkvar();
			}
		}
		break;
	case 4:													// PVsystems.kvar write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				((TPVsystemObj*)with0.Get_Active())->Set_Presentkvar(arg);
			}
		}
		break;
	case 5:													// PVsystems.pf read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				result = ((TPVsystemObj*)with0.Get_Active())->PFNominal;
			}
		}
		break;
	case 6:													// PVsystems.pf write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				((TPVsystemObj*)with0.Get_Active())->PFNominal = arg;
			}
		}
		break;
	case 7:													// PVsystems.KVARated read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				result = ((TPVsystemObj*)with0.Get_Active())->Get_FkVArating();
			}
		}
		break;
	case 8:													// PVsystems.KVARated write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				((TPVsystemObj*)with0.Get_Active())->Set_kVARating(arg);
			}
		}
		break;
	case 9:													// PVsystems.pmpp read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				result = ((TPVsystemObj*)with0.Get_Active())->Get_FPmpp();
			}
		}
		break;
	case 10:												// PVsystems.pmpp write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				((TPVsystemObj*)with0.Get_Active())->Set_Pmpp(arg);
			}
		}
		break;
	case 11:												// PVsystems.IrradianceNow
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			if (with0.ActiveItem != 0)
			{
				result = ((TPVsystemObj*)with0.Get_Active())->Get_PresentIrradiance();
			}
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall PVsystemsS(int mode, char* arg)
{
	TPVsystemObj*	pPVSystem = nullptr;
	TPVsystemObj*	PVSystem = nullptr;
	int				ActiveSave = 0;
	bool			Found = false;
	string			S = "",
					result = "";

	switch (mode)
	{
	case 0:													// PVsystems.Name read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pPVSystem = (TPVsystemObj*)ActiveCircuit[ActiveActor]->PVSystems.Get_Active();
			if (pPVSystem != nullptr)
			{
				result = pPVSystem->LName;
			}
			else 
				result = "";  // signify no name
		}
		break;
	case 1:													// PVsystems.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->PVSystems;
			S = arg;
			Found = false;
			ActiveSave = with0.ActiveItem;
			PVSystem = (TPVsystemObj*)with0.Get_First();
			while (PVSystem != nullptr)
			{
				if (CompareText(PVSystem->LName, S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(PVSystem);
					Found = true;
					break;
				}
				PVSystem = (TPVsystemObj*)with0.Get_Next();
			}
			if (not(Found))
			{
				DoSimpleMsg("PVSystem \"" + S + "\" Not Found in Active Circuit.", 5003);
				PVSystem = (TPVsystemObj*)with0.Get(ActiveSave);// Restore active PVSystem
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(PVSystem);
			}
		}
		break;
	case 2:													// PVsystems.Sensor
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pPVSystem = (TPVsystemObj*)ActiveCircuit[ActiveActor]->PVSystems.Get_Active();
			if (pPVSystem != nullptr && pPVSystem->SensorObj != nullptr)
			{
				result = pPVSystem->SensorObj->get_Name();
			}
			else
				result = "";  // signify no name
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall PVsystemsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TPVsystemObj* PVSystemElem=nullptr;

	switch (mode)
	{
	case 0:													// PVsystems.AllNames
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->PVSystems.NumInList)
			{
				PVSystemElem = (TPVsystemObj*) with0->PVSystems.Get_First();
				while (PVSystemElem != nullptr)
				{
					WriteStr2Array(PVSystemElem->LName);
					WriteStr2Array(Char0());
					PVSystemElem = (TPVsystemObj*) with0->PVSystems.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
	break;
	default:
		*myType = 4;        // String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Reclosers  interface for the DLL  
//--------------------------------------------------------------------------------
// ******************************Help Functions************************* 
void Set_ParameterReC(string parm, string val)
{
	string mycmd = "";
	if ((ASSIGNED(ActiveCircuit[ActiveActor])))
	{
		SolutionAbort = false;// Reset for commands entered from outside
		mycmd = Format("recloser.%s.%s=%s", ((TRecloserObj*)RecloserClass->GetActiveObj())->LName.c_str(), parm.c_str(), val.c_str());
		DSSExecutive[ActiveActor]->Set_Command(mycmd);
	}
}

// ******************************int type properties************************* 
int __stdcall ReclosersI(int mode, int arg)
{
	int result = 0;
	TRecloserObj* pElem = nullptr;
	TRecloserObj* elem = nullptr;
	TRecloserObj* pRecloser = nullptr;


	switch(mode)
	{
	case 0:													// Reclosers.Count
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = RecloserClass->ElementList.NumInList;
		}
		break;
	case 1:													// Reclosers.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TRecloserObj*)RecloserClass->ElementList.Get_First();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = 1;
					}
					else
						pElem = (TRecloserObj*)RecloserClass->ElementList.Get_Next();
				} while (!(result == 1 || pElem == nullptr));
			}
		}
		break;
	case 2:													// Reclosers.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TRecloserObj*)RecloserClass->ElementList.Get_Next();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = RecloserClass->ElementList.ActiveItem;
					}
					else
						pElem = (TRecloserObj*)RecloserClass->ElementList.Get_Next();
				} while (!(result > 0 || pElem == nullptr));
			}
		}
		break;
	case 3:													// Reclosers.MonitoredTerm read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->MonitoredElementTerminal;
		}
		break;
	case 4:													// Reclosers.MonitoredTerm write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("monitoredterm", IntToStr(arg));
		}
		break;
	case 5:													// Reclosers.SwitchedTerm read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->ElementTerminal;
		}
		break;
	case 6:													// Reclosers.SwitchedTerm write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("SwitchedTerm", IntToStr(arg));
		}
		break;
	case 7:													// Reclosers.NumFast read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->NumFast;
		}
		break;
	case 8:													// Reclosers.NumFast write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("numfast", IntToStr(arg));
		}
		break;
	case 9:													// Reclosers.Shots read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->NumReclose + 1;
		}
		break;
	case 10:													// Reclosers.Shots write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("shots", IntToStr(arg));
		}
		break;
	case 11:													// Reclosers.Open -CHECK
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			elem->set_State(CTRL_OPEN);
		}
		break;
	case 12:													// Reclosers.Close
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			elem->set_State(CTRL_CLOSE);
		}
		break;
	case 13:													// Reclosers.Idx read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = RecloserClass->ElementList.ActiveItem;
		}
		else
			result = 0;
		break;
	case 14:													// Reclosers.Idx write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pRecloser = (TRecloserObj*)RecloserClass->ElementList.Get(arg);
			if (pRecloser != nullptr)
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pRecloser);
			}
		}
		else
			result = 0;
		break;
	case 15:													// Reclosers.Reset
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			elem->Reset(ActiveActor);
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall ReclosersF(int mode, double arg)
{
	double result = 0.0; // Default return value
	TRecloserObj* elem = nullptr;

	switch (mode)
	{
	case 0:													// Reclosers.PhaseTrip read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->PhaseTrip;
		}
		break;
	case 1:													// Reclosers.PhaseTrip write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("PhaseTrip", Format("%.g", arg));
		}
		break;
	case 2:													// Reclosers.PhaseInst read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->PhaseInst;
		}
		break;
	case 3:													// Reclosers.PhaseInst write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("Phaseinst", Format("%.g", arg));
		}
		break;
	case 4:													// Reclosers.GroundTrip read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->GroundTrip;
		}
		break;
	case 5:													// Reclosers.GroundTrip write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("GroundTrip", Format("%.g", arg));
		}
		break;
	case 6:													// Reclosers.GroundInst read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->GroundInst;
		}
		break;
	case 7:													// Reclosers.GroundInst write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("GroundInst", Format("%.g", arg));
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall ReclosersS(int mode, char* arg)
{
	TRecloserObj*	elem = nullptr;
	string			result = "", // Default return value
					S = "";

	switch (mode)
	{
	case 0:													// Reclosers.Name read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->LName;
		}
		break;
	case 1:													// Reclosers.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (RecloserClass->SetActive(arg))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TRecloserObj*)RecloserClass->ElementList.Get_Active());
			}
			else
				DoSimpleMsg("Recloser " + (string)arg + " Not Found in Active Circuit.", 77003);

		}
		break;
	case 2:													// Reclosers.MonitoredObj read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->MonitoredElementName;
		}
		break;
	case 3:													// Reclosers.MonitoredObj write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("monitoredObj", (string)arg);
		}
		break;
	case 4:													// Reclosers.SwitchedObj read
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->ElementName;
		}
		break;
	case 5:													// Reclosers.SwitchedObj write
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterReC("SwitchedObj", arg);
		}
		break;
	case 6:													// Reclosers.State readTBD
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			if (elem->get_State() == CTRL_CLOSE)
				result = "closed";
			else
				result = "open";
		}
		break;
	case 7:													// Reclosers.State write TBD
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			S = arg;
			if (LowerCase(S)[0] == 'c')
				elem->set_State(CTRL_CLOSE);
			else
				elem->set_State(CTRL_OPEN);
		}
		break;
	case 8:													// Reclosers.NormalState read  TBD
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			if (elem->FNormalState == CTRL_CLOSE)
				result = "closed";
			else
				result = "open";
		}
		break;
	case 9:													// Reclosers.NormalState write TBD
		elem = (TRecloserObj*)RecloserClass->GetActiveObj();
		if (elem != nullptr)
		{
			S = arg;
			if (LowerCase(S)[0] == 'c')
				elem->set_NormalState(CTRL_CLOSE);
			else
				elem->set_NormalState(CTRL_OPEN);
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall ReclosersV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TRecloserObj* elem = nullptr;
	TPointerList* pList = nullptr;
	int k = 0,
		i = 0;
	switch (mode)
	{
	case 0:													// Reclosers.AllNames
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (RecloserClass->ElementList.NumInList > 0)
			{
				pList = &(RecloserClass->ElementList);
				elem = (TRecloserObj*)pList->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->LName);
					WriteStr2Array(Char0());
					elem = (TRecloserObj*)pList->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 1:													// Reclosers.RecloseIntervals
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0.0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = (TRecloserObj*)RecloserClass->ElementList.Get_Active();
			if (elem != nullptr)
			{
				myDblArray.resize(elem->NumReclose);
				k = 0;
				for (i = 1; i <= elem->NumReclose; i++)
				{
					myDblArray[k] = elem->RecloseIntervals[i - 1];
					k++;
				}
			}
		}
		*mySize = myDblArray.size() * sizeof(myDblArray[0]);
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	default:
		*myType = 4; //string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the ReduceCkt   interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 

int __stdcall ReactorsI(int mode, int arg)
{
    int Result = 0;
    TReactorObj* Elem = nullptr;
    TPointerList pList = {};

	switch (mode)
    {
		case 0:					// Reactors.First
			if (ReactorClass[ActiveActor]->ElementList.NumInList > 0)
			{
				pList = ReactorClass[ActiveActor]->ElementList;
				Elem = (TReactorObj*)pList.Get_First();
				do
				{
					if (Elem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(Elem);
						Result = 1;
					}
					else
						Elem = (TReactorObj*)pList.Get_Next();
				} while ((Result == 0) && (ASSIGNED(Elem)));
			}
			else
				Result = 0; // signify no more
            break;
        case 1:					// Reactors.Next
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
			{

			if (ReactorClass[ActiveActor]->ElementList.NumInList > 0) 
			{
			pList = ReactorClass[ActiveActor]->ElementList;
			Elem = (TReactorObj*)pList.Get_First();
            do
            {
                if (Elem->Get_Enabled())
                {
                    ActiveCircuit[ActiveActor]->Set_ActiveCktElement(Elem);
                    Result = pList.ActiveItem;
                }
                else 
					Elem = (TReactorObj*)pList.Get_Next();
            }
			while( (Result == 0) && (ASSIGNED(Elem)));
			}
			else
				Result = 0;  // signify no more
			}
			break;
        case 2:					// Reactors.Count
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
                Result = ReactorClass[ActiveActor]->ElementList.NumInList;
            break;
        case 3:					// Parallel.read
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
            {
                if (Elem->IsParallel)
					Result = 1;
            }
            break;
        case 4:					// Parallel.write
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
            {
                Elem->IsParallel = (arg > 0);
            }
            break;
		default:
            Result = -1; // The user is asking for the wrong command
            break;
	}
    return Result;
}

//******************************Float point type properties****************************
double __stdcall ReactorsF(int mode, double arg)
{
    double Result = 0.0;
    TReactorObj* Elem = nullptr;
    TPointerList pList = {};

    switch (mode)
    {
		case 0:						// Reactor.kV Read
			Elem   = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
			if (ASSIGNED(Elem)) 
				Result = Elem->kvrating;
			break;
        case 1:						// Reactor.kV Write
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Elem->kvrating = arg;
            break;
        case 2:						// Reactor.kvar Read
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Result = Elem->kvarrating;
            break;
        case 3:						// Reactor.kvar Write
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Elem->kvarrating = arg;
            break;
        case 4:						// Reactor.lmH Read
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Result = Elem->l;
            break;
        case 5:						// Reactor.lmH Write
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Elem->l = arg;
            break;
        case 6:						// Reactor.R Read
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Result = Elem->R;
            break;
        case 7:						// Reactor.R Write
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Elem->R = arg;
            break;
        case 8:						// Reactor.Rp Read
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Result = Elem->Rp;
            break;
        case 9:						// Reactor.Rp Write
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Elem->Rp = arg;
            break;
        case 10:					// Reactor.X Read
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Result = Elem->X;
            break;
        case 11:					// Reactor.X Write
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
                Elem->X = arg;
            break;
        default:
            Result = -1.0; // We got the wrong command
            break;
    }
    return Result;
}

//******************************String type properties*********************************
char* __stdcall ReactorsS(int mode, char* arg)
{
    string	Result = "", // Default return value
			S = "";
    bool	found = false;
    TReactorObj* Elem = nullptr;
    TPointerList pList = {};
    int		activesave = 0,
			k = 0;

	switch (mode)
    {
        case 0:						// Reactor.Name Read
			Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
			if (ASSIGNED(Elem))
			{
			  Result = Elem->get_Name();
			}
			break;
        case 1:						// Reactor.Name Write
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
            { // Search list of Storages in active circuit for name
				if (ReactorClass[ActiveActor]->ElementList.NumInList > 0)
				{
					S           = arg;  // Convert to Pascal String
					found       = false;
					pList       = ReactorClass[ActiveActor]->ElementList;
                    activesave = pList.get_myActiveItem();
                    Elem = (TReactorObj*)pList.Get_First();
                    while (ASSIGNED(Elem))
                    {
                        if (CompareText(Elem->Get_myLName(), S) == 0)
                        {
                            ActiveCircuit[ActiveActor]->Set_ActiveCktElement(Elem);
                            found = true;
                            break;
                        }
                        Elem = (TReactorObj*)pList.Get_Next();
                        
                    }
					if (!found)
                    {
						DoSimpleMsg("Reactor " + S + " Not Found in Active Circuit.", 20003);
                        Elem = (TReactorObj*)pList.Get(activesave); // Restore active Storage
                        ActiveCircuit[ActiveActor]->Set_ActiveCktElement(Elem);
					}
					
				}
			}
            break;
        case 2:							// Reactor.LCurve Read
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
			if (ASSIGNED(Elem))
			{
			  Result = Elem->LCurve;
			}
            break;
        case 3:							// Reactor.LCurve Write
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
            {
                Elem->LCurve = arg;
            }
            break;
        case 4:							// Reactor.RCurve Read
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
            {
                Result = Elem->RCurve;
            }
            break;
        case 5:							// Reactor.RCurve Write
            Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
            if (ASSIGNED(Elem))
            {
                Elem->RCurve = arg;
            }
            break;
    }

	char* presult = new char[Result.size() + 1];
    strcpy(presult, Result.c_str());
    return presult;
}

//************************Structure type properties*******************************
void __stdcall ReactorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
    TReactorObj* Elem = nullptr;
    TPointerList pList = {};
    int		idx = 0,
			i = 0,
			k = 0;
    double* PDouble = nullptr;

	switch (mode)
    {
        case 0:							// Reactors.AllNames 
			*myType  =  4;				// String
			myStrArray.clear();
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
			{
				if (ReactorClass[ActiveActor]->ElementList.NumInList > 0)
				{
					pList = ReactorClass[ActiveActor]->ElementList;

					k = 0;
					Elem = (TReactorObj*)pList.Get_First();
					while (ASSIGNED(Elem))
					{
						WriteStr2Array(Elem->LName);
						WriteStr2Array(Char0());
						k++;
						Elem = (TReactorObj*)pList.Get_Next();
					}
				}
			}

			if (myStrArray.empty()) 
				WriteStr2Array("None");
			*mySize = myStrArray.size();
			*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
			break;
        case 1:							// Reactors.RMatrix read
			*myType  =  2;        // Double
			myDblArray.resize(1);
			myDblArray[0] = 0;
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
			{
				Elem    = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
				if (ASSIGNED(Elem))
				{
					auto with0 = Elem;
					
					if (with0->Rmatrix != nullptr)
					{
						myDblArray.resize(Sqr(with0->Get_NPhases()));
						for (i = 0; i < Sqr(with0->Get_NPhases()); i++) 
						{
							myDblArray[i] =  with0->Rmatrix[i];
						}
					}
					
				}
			}
			*mySize = myDblArray.size() * sizeof(myDblArray[0]);
            *myPtr = (uintptr_t)(void*)&(myDblArray[0]);
            break;
        case 2:							// Reactors.RMatrix write
			*myType  =  2;        // Double
			k =  0;
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
			{
				Elem    =  (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
				if (ASSIGNED(Elem))
				{
					auto with0 = Elem;
					
					realloc(with0->Rmatrix,sizeof(double) *  Sqr(with0->Get_NPhases()));
                    for( i = 0; i < Sqr(with0->Get_NPhases()); i++)
					{
                        PDouble = *(double**)myPtr;
						with0->Rmatrix[i] = *PDouble;
						k++;
						PDouble++;
					}
					with0->Set_YprimInvalid(ActiveActor,true);
				}
			}
			*mySize  =  k;
            break;
        case 3:							// Reactors.XMatrix read
            *myType = 2; // Double
            myDblArray.resize(1);
            myDblArray[0] = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;

                    if (with0->Rmatrix != nullptr)
                    {
                        myDblArray.resize(Sqr(with0->Get_NPhases()));
                        for (i = 0; i < Sqr(with0->Get_NPhases()); i++)
                        {
                            myDblArray[i] = with0->XMatrix[i];
                        }
                    }
                }
            }
            *mySize = myDblArray.size() * sizeof(myDblArray[0]);
            *myPtr = (uintptr_t)(void*)&(myDblArray[0]);
            break;
        case 4:							// Reactors.XMatrix write
            *myType = 2; // Double
            k = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;

                    realloc(with0->XMatrix, sizeof(double) * Sqr(with0->Get_NPhases()));
                    for (i = 0; i < Sqr(with0->Get_NPhases()); i++)
                    {
                        PDouble = *(double**)myPtr;
                        with0->XMatrix[i] = *PDouble;
                        k++;
                        PDouble++;
                    }
                    with0->Set_YprimInvalid(ActiveActor, true);
                }
            }
            *mySize = k;
            break;
        case 5:							// Reactors.Z read
            *myType = 2; // Double
            myDblArray.resize(1);
            myDblArray[0] = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;
					myDblArray.resize(2);
                    myDblArray[0] = with0->Z.re;
                    myDblArray[1] = with0->Z.im;
                }
            }
            *mySize = myDblArray.size() * sizeof(myDblArray[0]);
            *myPtr = (uintptr_t)(void*)&(myDblArray[0]);
            break;
        case 6:							// Reactors.Z write
            *myType = 2; // Double
            k = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;
                    PDouble = *(double**)myPtr;
                    with0->Z.re = *PDouble;
                    PDouble++;
                    with0->Z.im = *PDouble;
                    k = 2;
                    with0->Set_YprimInvalid(ActiveActor, true);
                }
            }
            *mySize = k;
            break;
        case 7:							// Reactors.Z0 read
            *myType = 2; // Double
            myDblArray.resize(1);
            myDblArray[0] = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;
                    myDblArray.resize(2);
                    myDblArray[0] = with0->Z0.re;
                    myDblArray[1] = with0->Z0.im;
                }
            }
            *mySize = myDblArray.size() * sizeof(myDblArray[0]);
            *myPtr = (uintptr_t)(void*)&(myDblArray[0]);
            break;
        case 8:							// Reactors.Z0 write
            *myType = 2; // Double
            k = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;
                    PDouble = *(double**)myPtr;
                    with0->Z0.re = *PDouble;
                    PDouble++;
                    with0->Z0.im = *PDouble;
                    k = 2;
                    with0->Set_YprimInvalid(ActiveActor, true);
                }
            }
            *mySize = k;
            break;
        case 9:							// Reactors.Z1 read
            *myType = 2; // Double
            myDblArray.resize(1);
            myDblArray[0] = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;
                    myDblArray.resize(2);
                    myDblArray[0] = with0->Z1.re;
                    myDblArray[1] = with0->Z1.im;
                }
            }
            *mySize = myDblArray.size() * sizeof(myDblArray[0]);
            *myPtr = (uintptr_t)(void*)&(myDblArray[0]);
            break;
        case 10:						// Reactors.Z1 write
            *myType = 2; // Double
            k = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;
                    PDouble = *(double**)myPtr;
                    with0->Z1.re = *PDouble;
                    PDouble++;
                    with0->Z1.im = *PDouble;
                    k = 2;
                    with0->Set_YprimInvalid(ActiveActor, true);
                }
            }
            *mySize = k;
            break;
        case 11:						// Reactors.Z2 read
            *myType = 2; // Double
            myDblArray.resize(1);
            myDblArray[0] = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;
                    myDblArray.resize(2);
                    myDblArray[0] = with0->Z2.re;
                    myDblArray[1] = with0->Z2.im;
                }
            }
            *mySize = myDblArray.size() * sizeof(myDblArray[0]);
            *myPtr = (uintptr_t)(void*)&(myDblArray[0]);
            break;
        case 12:						// Reactors.Z2 write
            *myType = 2; // Double
            k = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Elem = (TReactorObj*)ReactorClass[ActiveActor]->GetActiveObj();
                if (ASSIGNED(Elem))
                {
                    auto with0 = Elem;
                    PDouble = *(double**)myPtr;
                    with0->Z2.re = *PDouble;
                    PDouble++;
                    with0->Z2.im = *PDouble;
                    k = 2;
                    with0->Set_YprimInvalid(ActiveActor, true);
                }
            }
            *mySize = k;
            break;
        default:
            *myType = 4; // String
            myStrArray.resize(0);
            WriteStr2Array("Error, parameter not recognized");
            WriteStr2Array(Char0());
            *mySize = myStrArray.size();
            *myPtr = (uintptr_t)(void*)&(myStrArray[0]);
            break;
    }
}


string ReduceEditString = "";
string EnergyMeterName = "";
string FirstPDelement = "";

//--------------------------------------------------------------------------------
// Implements the ReduceCkt   interface for the DLL  
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall ReduceCktI(int mode, int arg)
{
	int result = 0;
	switch (mode)
	{
	case 0:													// ReduceCkt.Do1phLaterals
		
		if (EnergyMeterClass[ActiveActor]->SetActive(EnergyMeterName))
		{
			ActiveEnergyMeterObj = (TEnergyMeterObj*)EnergyMeterClass[ActiveActor]->ElementList.Get_Active();
			if (ASSIGNED(ActiveEnergyMeterObj))
			{
				auto with0 = ActiveEnergyMeterObj;
				if (not(ASSIGNED(with0->BranchList)))
				{
					with0->MakeMeterZoneLists(ActiveActor);
					DoRemoveAll_1ph_Laterals(with0->BranchList);
				}
			}
		}
		break;
	case 1:													// ReduceCkt.DoBranchRemove
		if(ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			if (EnergyMeterClass[ActiveActor]->SetActive(EnergyMeterName))
			{
				ActiveEnergyMeterObj = (TEnergyMeterObj*)EnergyMeterClass[ActiveActor]->ElementList.Get_Active();
				if (ASSIGNED(ActiveEnergyMeterObj))
				{
					auto with0 = ActiveEnergyMeterObj;
					if (not(ASSIGNED(with0->BranchList)))
					{
						with0->MakeMeterZoneLists(ActiveActor);
						auto with1 = ActiveCircuit[ActiveActor];
						if (with1->SetElementActive(FirstPDelement) >= 0)
						{
							DoRemoveBranches(with1->Branch_List, (TPDElement*)with1->FActiveCktElement, with1->ReduceLateralsKeepLoad, ReduceEditString);
						}

					}
				}
			}
		}
		
		break;
	case 2:													// ReduceCkt.DoDangling
		if (EnergyMeterClass[ActiveActor]->SetActive(EnergyMeterName))
		{
			ActiveEnergyMeterObj = (TEnergyMeterObj*)EnergyMeterClass[ActiveActor]->ElementList.Get_Active();
			if (ASSIGNED(ActiveEnergyMeterObj))
			{
				auto with0 = ActiveEnergyMeterObj;
				if (not(ASSIGNED(with0->BranchList)))
				{
					with0->MakeMeterZoneLists(ActiveActor);
					DoReduceDangling(with0->BranchList);
				}
			}
		}
		break;
	case 3:													// ReduceCkt.DoDefault
		if (EnergyMeterClass[ActiveActor]->SetActive(EnergyMeterName))
		{
			ActiveEnergyMeterObj = (TEnergyMeterObj*)EnergyMeterClass[ActiveActor]->ElementList.Get_Active();
			if (ASSIGNED(ActiveEnergyMeterObj))
			{
				auto with0 = ActiveEnergyMeterObj;
				if (not(ASSIGNED(with0->BranchList)))
				{
					with0->MakeMeterZoneLists(ActiveActor);
					DoReduceDefault(with0->BranchList);
				}
			}
		}
		break;
	case 4:													// ReduceCkt.DoLoopBreak
		if (EnergyMeterClass[ActiveActor]->SetActive(EnergyMeterName))
		{
			ActiveEnergyMeterObj = (TEnergyMeterObj*)EnergyMeterClass[ActiveActor]->ElementList.Get_Active();
			if (ASSIGNED(ActiveEnergyMeterObj))
			{
				auto with0 = ActiveEnergyMeterObj;
				if (not(ASSIGNED(with0->BranchList)))
				{
					with0->MakeMeterZoneLists(ActiveActor);
					DoBreakLoops(with0->BranchList);
				}
			}
		}
		break;
	case 5:													// ReduceCkt.DoParallelLines
		if (EnergyMeterClass[ActiveActor]->SetActive(EnergyMeterName))
		{
			ActiveEnergyMeterObj = (TEnergyMeterObj*)EnergyMeterClass[ActiveActor]->ElementList.Get_Active();
			if (ASSIGNED(ActiveEnergyMeterObj))
			{
				auto with0 = ActiveEnergyMeterObj;
				if (not(ASSIGNED(with0->BranchList)))
				{
					with0->MakeMeterZoneLists(ActiveActor);
					DoMergeParallelLines(with0->BranchList);
				}
			}
		}
		break;
	case 6:													// ReduceCkt.DoShortLines
		if (EnergyMeterClass[ActiveActor]->SetActive(EnergyMeterName))
		{
			ActiveEnergyMeterObj = (TEnergyMeterObj*)EnergyMeterClass[ActiveActor]->ElementList.Get_Active();
			if (ASSIGNED(ActiveEnergyMeterObj))
			{
				auto with0 = ActiveEnergyMeterObj;
				if (not(ASSIGNED(with0->BranchList)))
				{
					with0->MakeMeterZoneLists(ActiveActor);
					DoReduceShortLines(with0->BranchList);
				}
			}
		}
		break;
	case 7:													// ReduceCkt.DoSwitches
		if (EnergyMeterClass[ActiveActor]->SetActive(EnergyMeterName))
		{
			ActiveEnergyMeterObj = (TEnergyMeterObj*)EnergyMeterClass[ActiveActor]->ElementList.Get_Active();
			if (ASSIGNED(ActiveEnergyMeterObj))
			{
				auto with0 = ActiveEnergyMeterObj;
				if (not(ASSIGNED(with0->BranchList)))
				{
					with0->MakeMeterZoneLists(ActiveActor);
					DoReduceShortLines(with0->BranchList);
				}
			}
		}
		break;
	case 8:													// ReduceCkt.KeepLoad - read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			if (ActiveCircuit[ActiveActor]->ReduceLateralsKeepLoad)
			{
				result = 1;
			}
		}
		break;
	case 9:													// ReduceCkt.KeepLoad - write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			if (arg != 0)
			{
				ActiveCircuit[ActiveActor]->ReduceLateralsKeepLoad = true;
			}
			else
				ActiveCircuit[ActiveActor]->ReduceLateralsKeepLoad = false;
		}
		break;
	default:
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall ReduceCktF(int mode, double arg)
{
	double result = 0.0;

	switch (mode)
	{
	case 0:													// ReduceCkt.Zmag - read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->ReductionZmag;
		}
		break;
	case 1:													// ReduceCkt.Zmag - write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->ReductionZmag = arg;
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall ReduceCktS(int mode, char* arg)
{
	string result = "";
	switch (mode)
	{
	case 0:													// ReduceCkt.EditString - Read
		result = ReduceEditString;
		break;
	case 1:													// ReduceCkt.EditString - Write
		ReduceEditString = arg;
		break;
	case 2:													// ReduceCkt.EnergyMeter - Read
		result = EnergyMeterName;
		break;
	case 3:													// ReduceCkt.EnergyMeter - Write
		EnergyMeterName = arg;
		break;
	case 4:													// ReduceCkt.SaveCircuit
		DSSExecutive[ActiveActor]->Set_Command("Save Circuit Dir = " + (string)arg);
		// Master file name is returned in DSSText.Result
		break;
	case 5:													// ReduceCkt.StartPDElement - Read
		result = FirstPDelement;
		break;
	case 6:													// ReduceCkt.StartPDElement - Write
		FirstPDelement = arg;
		break;
	default:
		result = "Error, not element found";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}

//--------------------------------------------------------------------------------
// Implements the RegControls  interface for the DLL  
//--------------------------------------------------------------------------------
// // ******************************Help Functions************************* 
TRegControlObj* __stdcall ActiveRegControl()
{
	TRegControlObj* result = nullptr;
	if (ActiveCircuit[ActiveActor] != nullptr)
	{
		result = (TRegControlObj*)ActiveCircuit[ActiveActor]->RegControls.Get_Active();
	}
	return result;
}
void Set_ParameterRegC(string parm, string val)
{
	string mycmd = "";
	if ((ASSIGNED(ActiveCircuit[ActiveActor])))
	{
		SolutionAbort = false;// Reset for commands entered from outside
		mycmd = Format("regcontrol.%s.%s=%s", ((TRegControlObj*)ActiveCircuit[ActiveActor]->RegControls.Get_Active())->LName.c_str(), parm.c_str(), val.c_str());
		DSSExecutive[ActiveActor]->Set_Command(mycmd);
	}
}
// ******************************int type properties************************* 
int __stdcall RegControlsI(int mode, int arg)
{
	TRegControlObj* elem = nullptr;
	TPointerList*	lst = nullptr;
	int				result = 0;

	switch (mode)
	{
	case 0:													// RegControls.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->RegControls);
			elem = (TRegControlObj*)lst->Get_First();
			if (elem != nullptr)
			{
				do
				{
					if (elem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = 1;
					}
					else
						elem = (TRegControlObj*)lst->Get_Next();
				} while (!(result == 1 || elem == nullptr));
			}
		}
		break;
	case 1:													// RegControls.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->RegControls);
			elem = (TRegControlObj*)lst->Get_Next();
			if (elem != nullptr)
			{
				do
				{
					if (elem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = lst->ActiveItem;
					}
					else
						elem = (TRegControlObj*)lst->Get_Next();
				} while (!(result > 0 || elem == nullptr));
			}
		}
		break;
	case 2:													// RegControls.TapWinding read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			result = elem->TapWinding;// has the taps
		}
		break;
	case 3:													// RegControls.TapWinding write
		Set_ParameterRegC("TapWinding", IntToStr(arg));
		break;
	case 4:													// RegControls.Winding read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			result = elem->ElementTerminal;// monitored winding
		}
		break;
	case 5:													// RegControls.Winding write
		Set_ParameterRegC("Winding", IntToStr(arg));
		break;
	case 6:													// RegControls.IsReversible read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			if (elem->IsReversible == true)
				result = 1;
		}
		else result = 0;
		break;
	case 7:													// RegControls.IsReversible read
		elem = ActiveRegControl();
		if (arg == 1)
		{
			Set_ParameterRegC("Reversible", "y");
		}
		else 
			Set_ParameterRegC("Reversible", "n");
		break;
	case 8:													// RegControls.IsInverseTime read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			if (elem->FInversetime == true)
				result = 1;
		}
		else result = 0;
		break;
	case 9:													// RegControls.IsInverseTime write
		elem = ActiveRegControl();
		if (arg == 1)
		{
			Set_ParameterRegC("InverseTime", "y");
		}
		else
			Set_ParameterRegC("InverseTime", "n");
		break;
	case 10:												// RegControls.MaxTapChange read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			result = elem->TapLimitPerChange;
		}
		break;
	case 11:												// RegControls.MaxTapChange write
		Set_ParameterRegC("MaxTapChange", IntToStr(arg));
		break;
	case 12:												// RegControls.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->RegControls.NumInList;
		}
		break;
	case 13:												// RegControls.TapNumber read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			result = elem->Get_TapNum();// tap number on the controlled-winding of the transformer controlled by this regcontrol
		}
		break;
	case 14:												// RegControls.TapNumber write
		Set_ParameterRegC("TapNum", IntToStr(arg));
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall RegControlsF(int mode, double arg)
{
	TRegControlObj* elem = nullptr;
	double			result = 0.0;
	switch (mode)
	{
	case 0:													// RegControls.CTPrimary read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			result = elem->CTRating;// has the taps
		}
		break;
	case 1:													// RegControls.CTPrimary write
		Set_ParameterRegC("CTprim", FloatToStr(arg));
		break;
	case 2:													// RegControls.PTRatio read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			result = elem->PTRatio;// has the taps
		}
		break;
	case 3:													// RegControls.PTRatio write
		Set_ParameterRegC("PTratio", FloatToStr(arg));
		break;
	case 4:													// RegControls.ForwardR read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->R;   // has the taps
		}
		break;
	case 5:													// RegControls.ForwardR write
		Set_ParameterRegC("R", FloatToStr(arg));
		break;
	case 6:													// RegControls.ForwardX read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
				
			result = elem->X;   // has the taps
		}
		break;
	case 7:													// RegControls.ForwardX write
		Set_ParameterRegC("X", FloatToStr(arg));
		break;
	case 8:													// RegControls.ReverseR read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->revR ;  
		}
		break;
	case 9:													// RegControls.ReverseR write
		Set_ParameterRegC("RevR", FloatToStr(arg));
		break;
	case 10:												// RegControls.ReverseX read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->revX;
		}
		break;
	case 11:												// RegControls.ReverseX write
		Set_ParameterRegC("RevX", FloatToStr(arg));
		break;
	case 12:												// RegControls.Delay read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->TimeDelay;
		}
		break;
	case 13:												// RegControls.Delay write
		Set_ParameterRegC("Delay", FloatToStr(arg));
		break;
	case 14:												// RegControls.TapDelay read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->TapDelay;
		}
		break;
	case 15:												// RegControls.TapDelay write
		Set_ParameterRegC("TapDelay", FloatToStr(arg));
		break;
	case 16:												// RegControls.VoltageLimit read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->Vlimit;
		}
		break;
	case 17:												// RegControls.VoltageLimit write
		Set_ParameterRegC("Vlimit", FloatToStr(arg));
		break;
	case 18:												// RegControls.ForwardBand read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->Bandwidth;
		}
		break;
	case 19:												// RegControls.ForwardBand write
		Set_ParameterRegC("Band", FloatToStr(arg));
		break;
	case 20:												// RegControls.ForwardVreg read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->Vreg;
		}
		break;
	case 21:												// RegControls.ForwardVreg write
		Set_ParameterRegC("Vreg", FloatToStr(arg));
		break;
	case 22:												// RegControls.ReverseBand read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->revBandwidth;
		}
		break;
	case 23:												// RegControls.ReverseBand write
		Set_ParameterRegC("RevBand", FloatToStr(arg));
		break;
	case 24:												// RegControls.ReverseVreg read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{

			result = elem->revVreg;
		}
		break;
	case 25:												// RegControls.ReverseVreg write
		Set_ParameterRegC("RevVreg", FloatToStr(arg));
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall RegControlsS(int mode, char* arg)
{
	TRegControlObj* elem = nullptr;
	TPointerList* lst = nullptr;
	int				ActiveSave = 0;
	bool			Found = false;
	string			S = "",
					result = "";  // Default return value

	switch (mode)
	{
	case 0:													// RegControls.Name read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			result = elem->LName;
		}
		break;
	case 1:													// RegControls.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->RegControls);
			S = arg;
			Found = false;
			ActiveSave = lst->ActiveItem;
			elem = (TRegControlObj*)lst->Get_First();
			while (elem != nullptr)
			{
				if (CompareText(elem->LName, S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
					Found = true;
					break;
				}
				elem = (TRegControlObj*)lst->Get_Next();
			}
			if (not(Found))
			{
				DoSimpleMsg("RegControl " + S + " Not Found in Active Circuit.", 5003);
				elem = (TRegControlObj*)lst->Get(ActiveSave);
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
			}
		}
		break;
	case 2:													// RegControls.MonitoredBus read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			result = elem->RegulatedBus;
		}
		break;
	case 3:													// RegControls.MonitoredBus write
		Set_ParameterRegC("Bus", (string)arg);
		break;
	case 4:													// RegControls.Transformer read
		elem = ActiveRegControl();
		if (elem != nullptr)
		{
			result = elem->Get_Transformer()->LName;
		}
		break;
	case 5:													// RegControls.Transformer write
		Set_ParameterRegC("Transformer",(string)arg);
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall RegControlsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TRegControlObj* elem = nullptr;
	TPointerList*	lst = nullptr;
	int				k = 0;

	switch (mode)
	{
	case 0:													// RegControls.AllNames
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			lst = &(with0->RegControls);
			if (lst->NumInList > 0)
			{
				elem = (TRegControlObj*)lst->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->LName);
					WriteStr2Array(Char0());
					elem = (TRegControlObj*)lst->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	default:
		*myType = 4; //string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Relays   interface for the DLL  
//--------------------------------------------------------------------------------
 // ******************************Help Functions************************* 
void Set_ParameterRelay(string parm, string val)
{
	string mycmd = "";
	if ((ASSIGNED(ActiveCircuit[ActiveActor])))
	{
		SolutionAbort = false;// Reset for commands entered from outside
		mycmd = Format("Relay.%s.%s=%s", ((TRecloserObj*)RelayClass->GetActiveObj())->LName.c_str(), parm.c_str(), val.c_str());
		DSSExecutive[ActiveActor]->Set_Command(mycmd);
	}
}
// ******************************int type properties************************* 
int __stdcall RelaysI(int mode, int arg)
{
	TRelayObj*	pElem = nullptr;
	TRelayObj*	elem = nullptr;
	TRelayObj*	pRelay = nullptr;
	int			result = 0;

	switch (mode)
	{
	case 0:													// Relays.Count
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = RelayClass->ElementList.NumInList;
		}
		break;
	case 1:													// Relays.First 
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			
			pElem = (TRelayObj*)RelayClass->ElementList.Get_First();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = 1;
					}
					else
						pElem = (TRelayObj*)RelayClass->ElementList.Get_Next();
				} while (!(result == 1 || elem == nullptr));
			}
		}
		break;
	case 2:													// Relays.Next
		if (ActiveCircuit[ActiveActor] != nullptr)
		{

			pElem = (TRelayObj*)RelayClass->ElementList.Get_Next();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = RelayClass->ElementList.ActiveItem;
					}
					else
						pElem = (TRelayObj*)RelayClass->ElementList.Get_Next();
				} while (!(result > 0 || elem == nullptr));
			}
		}
		break;
	case 3:													// Relays.MonitoredTerm read 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->MonitoredElementTerminal;
		}
		break;
	case 4:													// Relays.MonitoredTerm write 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterRelay("monitoredterm", IntToStr(arg));
		}
		break;
	case 5:													// Relays.SwitchedTerm read 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->ElementTerminal;
		}
		break;
	case 6:													// Relays.SwitchedTerm write 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterRelay("SwitchedTerm", IntToStr(arg));
		}
		break;
	case 7:													// Relays.Idx read 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = RelayClass->ElementList.ActiveItem;
		}
		break;
	case 8:													// Relays.Idx write 
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pRelay = (TRelayObj*)RelayClass->ElementList.Get(arg);
			if (pRelay != nullptr)
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pRelay);
			}
		}
		break;
	case 9:													// Relays.Open
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			elem->set_State(CTRL_OPEN);
		}
		break;
	case 10:												// Relays.Close
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			elem->set_State(CTRL_CLOSE);
		}
		break;
	case 11:												// Relays.Reset
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			elem->Reset(ActiveActor);
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall RelaysS(int mode, char* arg)
{
	TRelayObj*	elem = nullptr;
	string		result = "";    // Default return value
	switch (mode)
	{
	case 0:													// Relays.Name read
		elem = (TRelayObj*) RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->LName;
		}
		break;
	case 1:													// Relays.Name write 
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (RelayClass->SetActive((string)arg))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TRelayObj*)RelayClass->ElementList.Get_Active());
			}
			else 
				DoSimpleMsg("Relay " + (string)arg + " Not Found in Active Circuit.", 77003);
		}
		break;
	case 2:													// Relays.MonitoredObj read 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->MonitoredElementName;
		}
		break;
	case 3:													// Relays.MonitoredObj write 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterRelay("monitoredObj", (string)arg);
		}
		break;
	case 4:													// Relays.SwitchedObj read 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			result = elem->ElementName;
		}
		break;
	case 5:													// Relays.SwitchedObj write 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			Set_ParameterRelay("SwitchedObj", (string)arg);
		}
		break;
	case 6:													// Relays.State read 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem->get_State() == CTRL_CLOSE)
		{
			result = "closed";
		}
		else
		{
			result = "open";
		}
		break;
	case 7:													// Relays.State write 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			if (LowerCase((string)arg)[0] == 'c')
			{
				elem->set_State(CTRL_CLOSE);
			}
			else
			{
				elem->set_State(CTRL_OPEN);
			}
		}
		else
			result = "open";
		break;
	case 8:													// Relays.NormalState read 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem->get_NormalState() == CTRL_CLOSE)
		{
			result = "closed";
		}
		else
		{
			result = "open";
		}
		break;
	case 9:													// Relays.NormalState write 
		elem = (TRelayObj*)RelayClass->GetActiveObj();
		if (elem != nullptr)
		{
			if (LowerCase((string)arg)[0] == 'c')
			{
				elem->set_NormalState(CTRL_CLOSE);
			}
			else
			{
				elem->set_NormalState(CTRL_OPEN);
			}
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//******************************String type properties****************************
void __stdcall RelaysV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TRelayObj*		elem = nullptr;
	TPointerList*	pList = nullptr;
	int k = 0;
	switch (mode)
	{
	case 0:													// Relays.AllNames
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (RelayClass->ElementList.NumInList > 0)
			{
				pList = &(RelayClass->ElementList);
				elem = (TRelayObj*)pList->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->LName);
					WriteStr2Array(Char0());
					elem = (TRelayObj*)pList->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	default:
		*myType = 4; //string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Sensors interface for the DLL  
//--------------------------------------------------------------------------------
//  // ******************************Help Functions************************* 
TSensorObj* __stdcall ActiveSensor()
{
	TSensorObj* result = nullptr;
	if (ActiveCircuit[ActiveActor] != nullptr)
	{
		result = (TSensorObj*)ActiveCircuit[ActiveActor]->Sensors.Get_Active();
	}
	return result;
}

void Set_ParameterSensor(string parm, string val)
{
	string mycmd = "";
	if ((ASSIGNED(ActiveCircuit[ActiveActor])))
	{
		SolutionAbort = false;// Reset for commands entered from outside
		mycmd = Format("sensor.%s.%s=%s", ActiveSensor()->LName.c_str(), parm.c_str(), val.c_str());
		DSSExecutive[ActiveActor]->Set_Command(mycmd);
	}
}
// ******************************int type properties************************* 
int __stdcall SensorsI(int mode, int arg)
{
	TSensorObj*		elem = nullptr;
	TPointerList*	lst = nullptr;
	int				result = 0;
	switch (mode)
	{
	case 0:													// Sensors.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Sensors.NumInList;
		}
		break;
	case 1:													// Sensors.First
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->Sensors);
			elem = (TSensorObj*)lst->Get_First();
			if (elem != nullptr)
			{
				do
				{
					if (elem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = 1;
					}
					else
						elem = (TSensorObj*)lst->Get_Next();
				} while (!(result == 1 || elem == nullptr));
			}
		}
		break;
	case 2:													// Sensors.Next
				if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->Sensors);
			elem = (TSensorObj*)lst->Get_Next();
			if (elem != nullptr)
			{
				do
				{
					if (elem->FEnabled)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = lst->ActiveItem;
					}
					else
						elem = (TSensorObj*)lst->Get_Next();
				} while (!(result > 0 || elem == nullptr));
			}
		}
		break;
	case 3:													// Sensors.IsDelta read
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			if (elem->FConn > 0)
				result = 1;
		}
		break;
	case 4:													// Sensors.IsDelta write
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			elem->FConn = int(arg);
		}
		break;
	case 5:													// Sensors.ReverseDelta read
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			if (elem->FDeltaDirection < 0)
				result = 1;
		}
		break;
	case 6:													// Sensors.ReverseDelta write
		if (arg == 1)
			Set_ParameterSensor("DeltaDirection", "-1");
		else
			Set_ParameterSensor("DeltaDirection", "1");
		break;
	case 7:													// Sensors.MeteredTerminal read
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			result = elem->MeteredTerminal;
		}
		break;
	case 8:													// Sensors.MeteredTerminal write
		Set_ParameterSensor("terminal", IntToStr(arg));
		break;
	case 9:													// Sensors.Reset
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			elem->ResetIt();
		}
		break;
	case 10:												// Sensors.ResetAll
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			SensorClass[ActiveActor]->ResetAll(ActiveActor);
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall SensorsF(int mode, double arg)
{
	TSensorObj* elem = nullptr;
	double		result = 0.0;
	switch (mode)
	{
	case 0:													// Sensors.PctError read
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			return elem->pctError;
		}
		break;
	case 1:													// Sensors.PctError write
		Set_ParameterSensor("%error", FloatToStr(arg));
		break;
	case 2:													// Sensors.Weight read
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			return elem->Weight;
		}
		break;
	case 3:													// Sensors.Weight write
		Set_ParameterSensor("weight", FloatToStr(arg));
		break;
	case 4:													// Sensors.kVbase read
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			return elem->kVBase;
		}
		break;
	case 5:													// Sensors.kVbase write
		Set_ParameterSensor("kvbase", FloatToStr(arg));
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall SensorsS(int mode, char* arg)
{
	TSensorObj*		elem = nullptr;
	int				ActiveSave = 0;
	bool			Found = false;
	TPointerList*	lst = nullptr;
	string			S = "",
					result = "";// Default return value

	switch (mode)
	{
	case 0:													// Sensors.Name read
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			result = elem->LName;
		}
		break;
	case 1:													// Sensors.Name write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->Sensors);
			S = arg;
			Found = false;
			ActiveSave = lst->ActiveItem;
			elem = (TSensorObj*)lst->Get_First();
			while (elem != nullptr)
			{
				if (CompareText(elem->LName, S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
					Found = true;
					break;
				}
				elem = (TSensorObj*)lst->Get_Next();
			}
			if (not(Found))
			{
				DoSimpleMsg("Sensor " + S + " Not Found in Active Circuit.", 5003);
				elem = (TSensorObj*)lst->Get(ActiveSave);
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
			}
		}
		break;
	case 2:													// Sensors.MeteredElement read
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			result = elem->ElementName;
		}
		break;
	case 3:													// Sensors.MeteredElement write
		Set_ParameterSensor("element", (string)arg);
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall SensorsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TSensorObj* elem = nullptr;
	int			k = 0,
				i = 0;
	double*		PDouble = nullptr;

	switch (mode)
	{
	case 0:													// Sensors.AllNames
		*myType = 4; //string
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->Sensors.NumInList > 0)
			{
				elem = (TSensorObj*)with0->Sensors.Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->LName);
					WriteStr2Array(Char0());
					elem = (TSensorObj*)with0->Sensors.Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	case 1:													// Sensors.Currents read
		*myType = 2; //double
		myDblArray.resize(0);
		myDblArray[0] = 0;
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			myDblArray.resize(elem->Fnphases);
			for (k = 0; k < elem->Fnphases; k++)
			{
				myDblArray[k] = elem->SensorCurrent[k];
			}
		}
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 2:													// Sensors.Currents write
		*myType = 2; //double
		elem = ActiveSensor();
		k = 1; 
		if (elem != nullptr)
		{
			PDouble = *(double**)myPtr;
			for (i = 0; i < elem->Fnphases; i++)
			{
				elem->SensorCurrent[i] = *PDouble;
				PDouble++;
				k++;
			}
		}
		*mySize = k - 1;
		break;
	case 3:													// Sensors.KVARS read
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			myDblArray.resize(elem->Fnphases);
			for (k = 0; k < elem->Fnphases; k++)
			{
				myDblArray[k] = elem->Sensorkvar[k];
			}
		}
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 4:													// Sensors.KVARS write
		*myType = 2; //double
		elem = ActiveSensor();
		k = 1;
		if (elem != nullptr)
		{
			PDouble = *(double**)myPtr;
			for (i = 1; i <= elem->Fnphases; i++)
			{
				elem->Sensorkvar[i - 1] = *PDouble;
				PDouble ++;
				k++;
			}
		}
		break;
	case 5:													// Sensors.KWS read
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			myDblArray.resize(elem->Fnphases);
			for (k = 0; k <= elem->Fnphases - 1; k++)
			{
				myDblArray[k] = elem->SensorkW[k];
			}
		}
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 6:													// Sensors.KWS write
		*myType = 2; //double
		elem = ActiveSensor();
		k = 1;
		if (elem != nullptr)
		{
			PDouble = *(double**)myPtr;
			for (i = 1; i <= elem->Fnphases; i++)
			{
				elem->SensorkW[i - 1] = *PDouble;
				PDouble++;
				k++;
			}
		}
		break;
	case 7:													// Sensors.AllocationFactor
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		elem = ActiveSensor();
		if (elem != nullptr)
		{
			myDblArray.resize(elem->Fnphases);
			for (k = 0; k < elem->Fnphases; k++)
			{
				myDblArray[k] = elem->PhsAllocationFactor[k];
			}
		}
		*mySize = sizeof(myDblArray) * myDblArray.size();
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	default:
		*myType = 4; //string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Settings   interface for the DLL 
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall SettingsI(int mode, int arg)
{
	int result = 0;
	switch (mode)
	{
	case 0:													// Settings.AllowDuplicates read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveCircuit[ActiveActor]->DuplicatesAllowed)
			{
				result = 1;
			}
		}
		break;
	case 1:													// Settings.AllowDuplicates Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (arg == 1)
			{
				ActiveCircuit[ActiveActor]->DuplicatesAllowed = true;
			}
			else
				ActiveCircuit[ActiveActor]->DuplicatesAllowed = false;
		}
		break;
	case 2:													// Settings.ZoneLock read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveCircuit[ActiveActor]->ZonesLocked)
			{
				result = 1;
			}
		}
		break;
	case 3:													// Settings.ZoneLock Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (arg == 1)
			{
				ActiveCircuit[ActiveActor]->ZonesLocked = true;
			}
			else
				ActiveCircuit[ActiveActor]->ZonesLocked = false;
		}
		break;
	case 4:													// Settings.CktModel read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveCircuit[ActiveActor]->PositiveSequence)
			{
				result = 1;
			}
			else
			{	
				result = 0;
			}
		}
		break;
	case 5:													// Settings.CktModel Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			switch (arg)
			{
			case 2:
				ActiveCircuit[ActiveActor]->PositiveSequence = true;
				break;
			default:
				ActiveCircuit[ActiveActor]->PositiveSequence = false;
			}
		}
		break;
	case 6:													// Settings.Trapezoidal read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (ActiveCircuit[ActiveActor]->TrapezoidalIntegration)
			{
				result = 1;
			}
		}
		break;
	case 7:													// Settings.Trapezoidal Write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			switch (arg)
			{
			case 2:
				ActiveCircuit[ActiveActor]->TrapezoidalIntegration = true;
				break;
			default:
				ActiveCircuit[ActiveActor]->TrapezoidalIntegration = false;
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall SettingsF(int mode, double arg)
{
	double result = 0.0;
	switch (mode)
	{
	case 0:													// Settings.AllocationFactors
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			DoSetAllocationFactors(arg);
		}
		break;
	case 1:													// Settings.NormVminpu read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->NormalMinVolts;
		}
		break;
	case 2:													// Settings.NormVminpu write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->NormalMinVolts = arg;
		}
		break;
	case 3:													// Settings.NormVmaxpu read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->NormalMaxVolts;
		}
		break;
	case 4:													// Settings.NormVmaxpu write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->NormalMaxVolts = arg;
		}
		break;
	case 5:													// Settings.EmergVminpu read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->EmergMinVolts;
		}
		break;
	case 6:													// Settings.EmergVminpu write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->EmergMinVolts = arg;
		}
		break;
	case 7:													// Settings.EmergVmaxpu read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->EmergMaxVolts;
		}
		break;
	case 8:													// Settings.EmergVmaxpu write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->EmergMaxVolts = arg;
		}
		break;
	case 9:													// Settings.UEWeight read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->UEWeight;
		}
		break;
	case 10:												// Settings.UEWeight write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->UEWeight = arg;
		}
		break;
	case 11:												// Settings.LossWeight read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->LossWeight;
		}
		break;
	case 12:												// Settings.LossWeight write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->LossWeight = arg;
		}
		break;
	case 13:												// Settings.PriceSignal read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->PriceSignal;
		}
		break;
	case 14:												// Settings.PriceSignal write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->PriceSignal = arg;
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall SettingsS(int mode, char* arg)
{
	string	result = "";  // Deafult return value
	int		i = 0;

	switch (mode)
	{
	case 0:													// Settings.AutoBusList read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto& with0 = ActiveCircuit[ActiveActor]->AutoAddBusList;
			for (i = 1; i <= with0.ListPtr.size(); i++)
			{
				AppendGlobalResult(with0.Get(i));
				result = GlobalResult;				
			}
		}
		break;
	case 1:													// Settings.AutoBusList write
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			DoAutoAddBusList((string)arg);
		}
		break;
	case 2:													// Settings.PriceCurve read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = ActiveCircuit[ActiveActor]->PriceCurve;
		}
		break;
	case 3:													// Settings.PriceCurve read
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			with0->PriceCurve = (string)arg;
			with0->PriceCurveObj = (TPriceShapeObj*)LoadShapeClass[ActiveActor]->Find(with0->PriceCurve);
			if (with0->PriceCurveObj)
			{
				DoSimpleMsg("Price Curve: " + with0->PriceCurve + " not found.", 5006);
			}
		}
		break;
	default:
		result = "Error, parameter not recognized";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall SettingsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	int		i = 0,
			j = 0,
			Count = 0,
			Num = 0;
	int*	pInt = nullptr;
	double* pDouble = nullptr;
	
	auto with0 = ActiveCircuit[ActiveActor];
	switch (mode)
	{
	case 0:													// Settings.UERegs read
		*myType = 1; //integer
		myIntArray.resize(1);
		myIntArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			myIntArray.resize(ActiveCircuit[ActiveActor]->NumUEregs);
			for (i = 0; i <= ActiveCircuit[ActiveActor]->NumUEregs - 1; i++)
			{
				myIntArray[i] = ActiveCircuit[ActiveActor]->UEregs[i];
			}
		}
		*mySize = sizeof(myIntArray[0]) * myIntArray.size();
		*myPtr = (uintptr_t)(void*)&(myIntArray[0]);
		break;
	case 1:													// Settings.UERegs write
		j = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			ReallocMem(ActiveCircuit[ActiveActor]->UEregs, sizeof(ActiveCircuit[ActiveActor]->UEregs[1]) * (*mySize));
			pInt = *(int**)myPtr;
			for (i = 1; i <= *mySize; i++)
			{
				ActiveCircuit[ActiveActor]->UEregs[j] = *pInt;
				pInt++;
				j++;
			}
		}
		*mySize = j - 1;
		break;
	case 2:													// Settings.LossRegs read
		*myType = 1; //integer
		myIntArray.resize(1);
		myIntArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			myIntArray.resize(ActiveCircuit[ActiveActor]->NumLossRegs);
			for (i = 0; i <= ActiveCircuit[ActiveActor]->NumLossRegs - 1; i++)
			{
				myIntArray[i] = ActiveCircuit[ActiveActor]->LossRegs[i];
			}
		}
		*mySize = sizeof(myIntArray[0]) * myIntArray.size();
		*myPtr = (uintptr_t)(void*)&(myIntArray[0]);
		break;
	case 3:													// Settings.LossRegs write
		j = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			ReallocMem(ActiveCircuit[ActiveActor]->UEregs, sizeof(ActiveCircuit[ActiveActor]->LossRegs[0]) * (*mySize));
			pInt = *(int**)myPtr;
			for (i = 1; i <= (*mySize); i++)
			{
				ActiveCircuit[ActiveActor]->LossRegs[j] = *pInt;
				pInt++;
				j++;
			}
		}
		*mySize = j - 1;
		break;
	case 4:													// Settings.VoltageBases read
		*myType = 2; //double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			
			//{Count the number of voltagebases specified}
			i = 0;
			do
			{
				i++;
			} while (with0->LegalVoltageBases[i - 1] != 0.0);
			Count = i - 1;
			myDblArray.resize(Count);
			for (i = 0; i <= Count - 1; i++)
			{
				myDblArray[i] = with0->LegalVoltageBases[i];
			}
		}
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		break;
	case 5:													// Settings.VoltageBases write
		*myType = 2; //double 
		j = 0;
		Num = *mySize;
		
		with0->LegalVoltageBases.resize(Num + 1);
		pDouble = *(double**)myPtr;
		for (i = 1; i <= (*mySize); i++)
		{
			with0->LegalVoltageBases[j]  = *pDouble;
			pDouble++;
			j++;
		}
		with0->LegalVoltageBases[Num] = 0.0;
		*mySize = j;
		break;
	default:
		*myType = 4; //string
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*mySize = myStrArray.size();
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Solution interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall SolutionI(int mode, int arg)
{
	int i = 0,
		result = 0;

	switch (mode)
	{
	case 0:													// Solution.Solve
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->Solve(ActiveActor);
		}
		break;
	case 1:													// Solution.Mode - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->Get_SolMode();
		}
		break;
	case 2:													// Solution.Mode - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->Set_Mode(arg);
		}
		break;
	case 3:													// Solution.hour - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour;
		}
		break;
	case 4:													// Solution.hour - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour = arg;
		}
		break;
	case 5:													// Solution.Year - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->Fyear;
		}
		break;
	case 6:													// Solution.Year - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->Set_Year(arg);
		}
		break;
	case 7:													// Solution.Iterations
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->Iteration;
		}
		break;
	case 8:													// Solution.MaxIterations - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->MaxIterations;
		}
		break;
	case 9:												// Solution.MaxIterations - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->MaxIterations = arg;
		}
		break;
	case 10:												// Solution.Number - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->NumberOfTimes;
		}
		break;
	case 11:												// Solution.Number - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->NumberOfTimes = arg;
		}
		break;
	case 12:												// Solution.Random - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->RandomType;
		}
		break;
	case 13:												// Solution.Random - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->RandomType = arg;
		}
		break;
	case 14:												// Solution.LoadModel - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->LoadModel;
		}
		break;
	case 15:												// Solution.LoadModel - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor]->Solution;
			with0->LoadModel = arg;
			with0->DefaultLoadModel = with0->LoadModel;
		}
		break;
	case 16:												// Solution.AddType - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->AutoAddObj.AddType;
		}
		break;
	case 17:												// Solution.AddType - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->AutoAddObj.AddType = arg;
		}
		break;
	case 18:												// Solution.Algorithm - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->Algorithm;
		}
		break;
	case 19:												// Solution.Algorithm - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->Algorithm = arg;
		}
		break;
	case 20:												// Solution.ControlMode - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->ControlMode;
		}
		break;
	case 21:												// Solution.ControlMode - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor]->Solution;
			with0->ControlMode = arg;
			with0->DefaultControlMode = with0->ControlMode;
		}
		break;
	case 22:												// Solution.ControlIterations - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->ControlIteration;
		}
		break;
	case 23:												// Solution.ControlIterations - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->ControlIteration = arg;
		}
		break;
	case 24:												// Solution.MaxControlIterations - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->MaxControlIterations;
		}
		break;
	case 25:												// Solution.MaxControlIterations - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->MaxControlIterations = arg;
		}
		break;
	case 26:												// Solution.Sample_DoControlActions
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->Sample_DoControlActions(ActiveActor);
		}
		break;
	case 27:												// Solution.CheckFaultStatus
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->Check_Fault_Status(ActiveActor);
		}
		break;
	case 28:												// Solution.SolveDirect
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->SolveDirect(ActiveActor);
		}
		break;
	case 29:												// Solution.SolvePflow
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->DoPFLOWsolution(ActiveActor);
		}
		break;
	case 30:												// Solution.SolveNoControl
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->SolveCircuit(ActiveActor);
		}
		break;
	case 31:												// Solution.SolvePlusControl
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->SolveCircuit(ActiveActor);
			ActiveCircuit[ActiveActor]->Solution->CheckControls(ActiveActor);
		}
		break;
	case 32:												// Solution.InitSnap
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->SnapShotInit(ActiveActor);
		}
		break;
	case 33:												// Solution.CheckControls
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->CheckControls(ActiveActor);
		}
		break;
	case 34:												// Solution.SampleControlDevices
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->SampleControlDevices(ActiveActor);
		}
		break;
	case 35:												// Solution.DoControlActions
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->DoControlActions(ActiveActor);
		}
		break;
	case 36:												// Solution.BuildYMatrix
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			YMatrix::BuildYMatrix(arg, false, ActiveActor);
		}
		break;
	case 37:												// Solution.SystemYChanged
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			if (ActiveCircuit[ActiveActor]->Solution->SystemYChanged)
			{
				result = 1;
			}
		}
		break;
	case 38:												// Solution.Converged - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			if (ActiveCircuit[ActiveActor]->Solution->ConvergedFlag)
			{
				result = 1;
			}
		}
		break;
	case 39:												// Solution.Converged - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			if (arg == 1)
			{
				ActiveCircuit[ActiveActor]->Solution->ConvergedFlag = true;
				ActiveCircuit[ActiveActor]->Issolved = true;
			}
			else
			{
				ActiveCircuit[ActiveActor]->Solution->ConvergedFlag = false;
				ActiveCircuit[ActiveActor]->Issolved = false;
			}
		}
		break;
	case 40:												// Solution.TotalIterations
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->Iteration;
		}
		break;
	case 41:												// Solution.MostIterationsDone
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->MostIterationsDone;
		}
		break;
	case 42:												// Solution.ControlActionsDone - Read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			if (ActiveCircuit[ActiveActor]->Solution->ControlActionsDone)
			{
				result = 1;
			}
		}
		break;
	case 43:												// Solution.ControlActionsDone - Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			if (arg == 1)
			{
				ActiveCircuit[ActiveActor]->Solution->ControlActionsDone = true;
			}
			else
			{
				ActiveCircuit[ActiveActor]->Solution->ControlActionsDone = false;
			}
		}
		break;
	case 44:												// Solution.FinishTimeStep
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			auto with1 = with0->Solution;
			MonitorClass[ActiveActor]->SampleAll(ActiveActor); // Make all monitors take a sample
			EndOfTimeStepCleanup(ActiveActor);
			with1->Increment_time();
		}
		break;
	case 45:												// Solution.Cleanup
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			EndOfTimeStepCleanup(ActiveActor);
		}
		break;
	case 46:												// Solution.SolveAll
		for (i = 1; i <= NumOfActors; i++)
		{
			ActiveActor = i;
			CmdResult = DoSetCmd(1);
		}
		break;
	case 47:												// Solution.CalcIncMatrix
		ActiveCircuit[ActiveActor]->Solution->Calc_Inc_Matrix(ActiveActor);
		break;
	case 48:												// Solution.CalcIncMatrix_O
		ActiveCircuit[ActiveActor]->Solution->Calc_Inc_Matrix_Org(ActiveActor);
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall SolutionF(int mode, double arg)
{
	double result = 0.0;

	switch (mode)
	{
	case 0:												// Solution.Frequency read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->FFrequency;
		}
		break;
	case 1:												// Solution.Frequency Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->FFrequency = arg;
		}
		break;
	case 2:												// Solution.Seconds read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->DynaVars.T;
		}
		break;
	case 3:												// Solution.Seconds Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->DynaVars.T = arg;
		}
		break;
	case 4:												// Solution.StepSize read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->DynaVars.h;
		}
		break;
	case 5:												// Solution.StepSize Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->DynaVars.h = arg;
			ActiveCircuit[ActiveActor]->Solution->IntervalHrs = arg / 3600.0;
		}
		break;
	case 6:												// Solution.LoadMult read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->FLoadMultiplier;
		}
		break;
	case 7:												// Solution.LoadMult Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->FLoadMultiplier = arg;
		}
		break;
	case 8:												// Solution.Tolerance read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->ConvergenceTolerance;
		}
		break;
	case 9:												// Solution.Tolerance Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->ConvergenceTolerance = arg;
		}
		break;
	case 10:												// Solution.pctgrowth read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			result = (with0->DefaultGrowthRate - 1.0) * 100.0;
		}
		break;
	case 11:												// Solution.pctgrowth Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			with0->DefaultGrowthRate = 1.0 + arg/100.0;
			with0->DefaultGrowthFactor = pow(with0->DefaultGrowthRate, with0->Solution->Fyear - 1);
		}
		break;
	case 12:												// Solution.GenkW read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->AutoAddObj.GenkW;
		}
		break;
	case 13:												// Solution.GenkW Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->AutoAddObj.GenkW = arg;
		}
		break;
	case 14:												// Solution.GenPF read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->AutoAddObj.GenPF;
		}
		break;
	case 15:												// Solution.GenPF Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->AutoAddObj.GenPF = arg;
		}
		break;
	case 16:												// Solution.Capkvar read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->AutoAddObj.Capkvar;
		}
		break;
	case 17:												// Solution.Capkvar Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->AutoAddObj.Capkvar = arg;
		}
		break;
	case 18:												// Solution.GenMult read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->GenMultiplier;
		}
		break;
	case 19:												// Solution.GenMult Writeif (ASSIGNED(ActiveCircuit[ActiveActor]))
	{
		ActiveCircuit[ActiveActor]->GenMultiplier = arg;
	}
		break;
	case 20:												// Solution.dblHour read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->DynaVars.dblHour;
		}
		break;
	case 21:												// Solution.dblHour Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor]->Solution;
			with0->DynaVars.intHour = Trunc(arg);
			with0->DynaVars.dblHour = arg;
			with0->DynaVars.T = (arg - with0->DynaVars.intHour) * 3600.0;
		}
		break;
	case 22:												// Solution.StepSizeMin 
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->DynaVars.h = arg * 60.0;
		}
		break;
	case 23:												// Solution.StepSizeHr
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->DynaVars.h = arg * 3600.0;
		}
		break;
	case 24:												// Solution.Process_Time
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->Solve_Time_Elapsed;
		}
		break;
	case 25:												// Solution.Total_Time read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->Total_Time_Elapsed;
		}
		break;
	case 26:												// Solution.Total_Time Write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			ActiveCircuit[ActiveActor]->Solution->Total_Time_Elapsed = arg;
		}
		break;
	case 27:												// Solution.Time_TimeStep
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Solution->Step_Time_Elapsed;
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall SolutionS(int mode, char* arg)
{
	TLoadShapeObj*	TestLoadShapeOb = nullptr;
	string			result = "";  // Deafult return value
	switch (mode)
	{
	case 0:												// Solution.ModeID
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = GetSolutionModeID();
		}
		else
		{
			result = "";
		}
		break;
	case 1:												// Solution.LDCurve read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->LoadDurCurve;
		}
		else
		{
			result = "";
		}
		break;
	case 2:												// Solution.LDCurve write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			auto with0 = ActiveCircuit[ActiveActor];
			with0->LoadDurCurve = std::string(arg);
			with0->LoadDurCurveObj = (TLoadShapeObj*)LoadShapeClass[ActiveActor]->Find(with0->LoadDurCurve);
			if (with0->LoadDurCurveObj != nullptr)
			{
				DoSimpleMsg("Load - Duration Curve not found.", 5001);
			}
		}
		break;
	case 3:												// Solution.DefaultDaily read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->DefaultDailyShapeObj->get_Name();
		}
		else
		{
			result = "";
		}
		break;
	case 4:												// Solution.DefaultDaily write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			TestLoadShapeOb = (TLoadShapeObj*)LoadShapeClass[ActiveActor]->Find(std::string(arg));
			if (TestLoadShapeOb != nullptr)
			{
				ActiveCircuit[ActiveActor]->DefaultDailyShapeObj = TestLoadShapeOb;
			}
		}
		break;
	case 5:												// Solution.DefaultYearly read
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->DefaultYearlyShapeObj->get_Name();
		}
		else
		{
			result = "";
		}
		break;
	case 6:												// Solution.DefaultYearly write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			TestLoadShapeOb = (TLoadShapeObj*)LoadShapeClass[ActiveActor]->Find(std::string(arg));
			if (TestLoadShapeOb != nullptr)
			{
				ActiveCircuit[ActiveActor]->DefaultYearlyShapeObj = TestLoadShapeOb;
			}
		}
		break;
	default:
		result = "Error, paratemer not recognized";
		break;

	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall SolutionV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	int Counter = 0, 
		i = 0,
		IMIdx = 0,
		Idx = 0,
		ArrSize = 0;

	auto with0 = ActiveCircuit[ActiveActor];
	auto with1 = ActiveCircuit[ActiveActor]->Solution;
	switch (mode)
	{
	case 0:												// Solution.EventLog
		*myType = 4;			//String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			for (i = 0; i < EventStrings[ActiveActor].size(); i++)
			{
				WriteStr2Array(EventStrings[ActiveActor][i]);
				WriteStr2Array(Char0());
			}
		}
		//if (myStrArray.size() == 0)
		//{
		//	WriteStr2Array("None");
		//}
		*myPtr = (uintptr_t)(void*)(myStrArray.data());
		*mySize = myStrArray.size();
		break;
	case 1:												// Solution.IncMatrix
		*myType = 1;			//Integer
		myIntArray.resize(1);
		myIntArray[0] = 0;
		if ((ActiveCircuit[ActiveActor] != nullptr) && (with1->IncMat.len != 0))
		{

			ArrSize = with1->IncMat.NZero() * 3;
			myIntArray.resize(ArrSize);
			Counter = 0;
			IMIdx = 0;
			while (IMIdx < ArrSize)
			{
				for (i = 0; i <= 2; i++)
				{
					myIntArray[IMIdx] = with1->IncMat.data[Counter][i];
					IMIdx++;
				}
				Counter++;
			}
		}
		*mySize = sizeof(myIntArray[0]) * myIntArray.size();
		*myPtr = (uintptr_t)(void*)&(myIntArray[0]);
		break;
	case 2:												// Solution.BusLevels
		*myType = 1;			//Integer
		myIntArray.resize(1);
		myIntArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{

			ArrSize = with1->Inc_Mat_levels.size() - 1;
			myIntArray.resize(ArrSize);
			for (IMIdx = 0; IMIdx < ArrSize; IMIdx++)
			{
				myIntArray[IMIdx] = with1->Inc_Mat_levels[IMIdx];
			}
		}
		*mySize = sizeof(myIntArray[0]) * myIntArray.size();
		*myPtr = (uintptr_t)(void*)&(myIntArray[0]);
		break;
	case 3:												// Solution.IncMatrixRows
		*myType = 4;			//String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor]->Solution;
			ArrSize = with0->Inc_Mat_Rows.size() - 1;
			for (IMIdx = 0; IMIdx <= ArrSize; IMIdx++)
			{
				WriteStr2Array(with0->Inc_Mat_Rows[IMIdx]);
				WriteStr2Array(Char0());
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 4:												// Solution.IncMatrixCols
		*myType = 4;			//String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (IncMat_Ordered)
			{
				ArrSize = with1->Inc_Mat_Cols.size();
				for (IMIdx = 0; IMIdx < ArrSize; IMIdx++)
				{
					WriteStr2Array(with1->Inc_Mat_Rows[IMIdx]);
					WriteStr2Array(Char0());
				}
			}
			else
			{
				for (i = 1; i <= ActiveCircuit[ActiveActor]->NumBuses; i++)
				{
					WriteStr2Array(ActiveCircuit[ActiveActor]->BusList.Get(i));
					WriteStr2Array(Char0());
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 5:												// Solution.Laplacian
		*myType = 1;			//Integer
		myIntArray.resize(1);
		myIntArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr && (with1->Laplacian.len != 0))
		{
			ArrSize = with1->Laplacian.NZero() * 3;
			myIntArray.resize(ArrSize);
			Counter = 0;
			IMIdx = 0;
			while (IMIdx < ArrSize)
			{
				for (i = 0; i <= 2; i++)
				{
					myIntArray[IMIdx] = with1->Laplacian.data[Counter][i];
					IMIdx++;
				}
				Counter++;
			}
		}
		*mySize = sizeof(myIntArray[0]) * myIntArray.size();
		*myPtr = (uintptr_t)(void*)&(myIntArray[0]);

		break;
	default:
		*myType = 4;				//String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Storages interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall StoragesI(int mode, int arg)
{
    TStorageObj* pStorageElem	 = nullptr;
    int			Result			= 0;	// Default return value

	switch (mode)
	{
        case 0:							 // Storages.First
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_First();
                if (ASSIGNED(pStorageElem))
                {
                    do
                    {
                        if (pStorageElem->Get_Enabled())
                        {
                            ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pStorageElem);
                            Result = 1;
                        }
						else
                            pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Next();
                    } while (!((Result == 1) || (!ASSIGNED(pStorageElem))));
                }
            }
        }
        break;
        case 1:							// Storages.Next
        {
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
			{
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Next();
                if (ASSIGNED(pStorageElem))
                {
                    do
                    {
                        if (pStorageElem->Get_Enabled())
                        {
                            ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pStorageElem);
                            Result = ActiveCircuit[ActiveActor]->StorageElements.ActiveItem;
                        }
                        else
                            pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Next();
                    } while (!((Result > 0) || (!ASSIGNED(pStorageElem))));
                }
			}
        }
        break;
        case 2:							// Storages.Count
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor])) 
                Result = ActiveCircuit[ActiveActor]->StorageElements.NumInList;
        }
        break;
        case 3:							// Storages.Idx read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
                Result = ActiveCircuit[ActiveActor]->StorageElements.ActiveItem;
        }
        break;
        case 4:							// Storages.Idx write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get(arg);
                if (ASSIGNED(pStorageElem))
                    ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pStorageElem);
            }
        }
        break;
        case 5:							// Storages.State Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                    Result = pStorageElem->fState;
            }
        }
        break;
        case 6:							// Storages.State Write
        {
            /*   Legal States
                 STORE_CHARGING = -1;
                 STORE_IDLING = 0;
                 STORE_DISCHARGING = 1;
             */
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                    pStorageElem->Set_StorageState(arg);
            }
        }
        break;
        case 7:							// Storages.ControlMode Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    if (pStorageElem->GFM_Mode)
                        Result = 1;
                }
            }
        }
        break;
        case 8:							// Storages.ControlMode Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
	                pStorageElem->GFM_Mode = (arg != 0);
                }
            }
        }
        break;
        case 9:							// Storages.SafeMode
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    if (pStorageElem->myDynVars.SafeMode)
						Result = 1;
                }
            }
        }
        break;
        case 10:						// Storages.VarFollowInverter Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    if (pStorageElem->FVarFollowInverter)
                        Result = 1;
                }
            }
        }
        break;
        case 11:						// Storages.VarFollowInverter Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->FVarFollowInverter = (arg != 1);
                }
            }
        }
        break;
		default:
            Result = -1;					// Just sent the wrong command
        break;
	}

	return Result;
}
//******************************Float point type properties****************************
double __stdcall StoragesF(int mode, double arg)
{
    TStorageObj*	pStorageElem	= nullptr;
    double			Result			= 0;		// Default return value

	switch(mode)
    {
        case 0:							// Storages.puSOC Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
					Result = pStorageElem->StorageVars.kWhStored / pStorageElem->StorageVars.kWhRating;
                }
            }
        }
        break;
        case 1:							// Storages.puSOC Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->StorageVars.kWhStored = pStorageElem->StorageVars.kWhRating * arg;
                }
            }
        }
        break;
        case 2:							// Storages.AmpLimit Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->myDynVars.ILimit;
                }
            }
        }
        break;
        case 3:							// Storages.AmpLimit Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->myDynVars.ILimit = arg;
                }
            }
        }
        break;
        case 4:							// Storages.AmpLimitGain Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->myDynVars.VError;
                }
            }
        }
        break;
        case 5:							// Storages.AmpLimitGain Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->myDynVars.VError = arg;
                }
            }
        }
        break;
        case 6:							// Storages.ChargeTrigger Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->ChargeTrigger;
                }
            }
        }
        break;
        case 7:							// Storages.ChargeTrigger Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->ChargeTrigger = arg;
                }
            }
        }
        break;
        case 8:							// Storages.DisChargeTrigger Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->DischargeTrigger;
                }
            }
        }
        break;
        case 9:							// Storages.DisChargeTrigger Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->DischargeTrigger = arg;
                }
            }
        }
        break;
        case 10:						// Storages.EffCharge Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->pctChargeEff;
                }
            }
        }
        break;
        case 11:						// Storages.EffCharge Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->pctChargeEff = arg;
                }
            }
        }
        break;
        case 12:						// Storages.EffDisCharge Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->pctDischargeEff;
                }
            }
        }
        break;
        case 13:						// Storages.EffDisCharge Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->pctDischargeEff = arg;
                }
            }
        }
        break;
        case 14:						// Storages.kP Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->myDynVars.Kp * 1e3;
                }
            }
        }
        break;
        case 15:						// Storages.kP Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->myDynVars.Kp = arg / 1e3;
                }
            }
        }
        break;
        case 16:						// Storages.kV Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->Get_PresentkV();
                }
            }
        }
        break;
        case 17:						// Storages.kV Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->Set_PresentkV(arg);
                }
            }
        }
        break;
        case 18:						// Storages.kVA Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->StorageVars.FkVArating;
                }
            }
        }
        break;
        case 19:						// Storages.kVA Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->StorageVars.FkVArating = arg;
                }
            }
        }
        break;
        case 20:						// Storages.kvar Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->Get_kvarRequested();
                }
            }
        }
        break;
        case 21:						// Storages.kvar Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->Set_kvarRequested(arg);
                }
            }
        }
        break;
        case 22:						// Storages.kVDC Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->myDynVars.RatedVDC / 1e3;
                }
            }
        }
        break;
        case 23:						// Storages.kVDC Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->myDynVars.RatedVDC = arg  * 1e3;
                }
            }
        }
        break;
        case 24:						// Storages.kW Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->Get_PresentkW();
                }
            }
        }
        break;
        case 25:						// Storages.kW Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->Set_kW(arg);
                }
            }
        }
        break;
        case 26:						// Storages.kWhRated Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->StorageVars.kWhRating;
                }
            }
        }
        break;
        case 27:						// Storages.kWhRated Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->StorageVars.kWhRating = arg;
                }
            }
        }
        break;
        case 28:						// Storages.kWRated Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->StorageVars.kWrating;
                }
            }
        }
        break;
        case 29:						// Storages.kWRated Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->StorageVars.kWrating = arg;
                }
            }
        }
        break;
        case 30:						// Storages.LimitCurrent Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    if (pStorageElem->CurrentLimited)
                        Result = 1;
                }
            }
        }
        break;
        case 31:						// Storages.LimitCurrent Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->CurrentLimited = (arg != 0);
                }
            }
        }
        break;
        case 32:						// Storages.PF Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->PFNominal;
                }
            }
        }
        break;
        case 33:						// Storages.PF Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->PFNominal = arg;
                }
            }
        }
        break;
        case 34:						// Storages.PITol Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->myDynVars.CtrlTol * 100;
                }
            }
        }
        break;
        case 35:						// Storages.PITol Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->myDynVars.CtrlTol = arg / 100;
                }
            }
        }
        break;
        case 36:						// Storages.SafeVoltage Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->myDynVars.SMThreshold;
                }
            }
        }
        break;
        case 37:						// Storages.SafeVoltage Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->myDynVars.SMThreshold = arg;
                }
            }
        }
        break;
        case 38:						// Storages.TimeChargeTrig Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->ChargeTime;
                }
            }
        }
        break;
        case 39:						// Storages.TimeChargeTrig Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    pStorageElem->ChargeTime = arg;
                }
            }
        }
        break;
		default:
            Result = -1.0;
        break;
    }
    return Result;
}
//******************************String type properties****************************
char* __stdcall StoragesS(int mode, char* arg)
{
    TStorageObj*	pStorageElem	= nullptr;
    string			S				= "",
    				Result			= ""; // Default return value
    bool			Found			= false;
    TPointerList*	lst				= nullptr;
    int				ActiveSave		= 0;


	switch (mode)
    {
        case 0:							// Storages.Name Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pStorageElem = (TStorageObj*)ActiveCircuit[ActiveActor]->StorageElements.Get_Active();
                if (ASSIGNED(pStorageElem))
                {
                    Result = pStorageElem->get_Name();
                }
            }
        }
        break;
        case 1:							// Storages.Name Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                lst = &(ActiveCircuit[ActiveActor]->StorageElements);
                S = arg;
                Found = false;
                ActiveSave = lst->get_myActiveItem();
                pStorageElem = (TStorageObj*)lst->Get_First();
                while (pStorageElem != nullptr)
                {
                    if (CompareText(pStorageElem->get_Name(), S) == 0)
                    {
                        ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pStorageElem);
                        Found = true;
                        break;
                    }
                    pStorageElem = (TStorageObj*)lst->Get_Next();
                }
                if (!Found)
                {
                    DoSimpleMsg("Storage '" + S + "' Not Found in Active Circuit.", 5003);
                    pStorageElem = (TStorageObj*)lst->Get(ActiveSave);
                    ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pStorageElem);
                }
            }
        }
        break;
		default:
			Result = "Error, parameter not valid";
    }
    char* presult = new char[Result.size() + 1];
    strcpy(presult, Result.c_str());
    return presult;
}
//************************Structure type properties*******************************
void __stdcall StoragesV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
    TStorageObj*	pStorageElem = nullptr;
    int				k			 = 0;

	switch (mode)
    {
        case 0:							// Storages.AllNames
        {
			*myType = 4; //String
            myStrArray.resize(0);
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                auto with0 = ActiveCircuit[ActiveActor];
                if (with0->StorageElements.NumInList > 0)
                {
                    k = 0;
                    pStorageElem = (TStorageObj*)with0->StorageElements.Get_First();
                    while (ASSIGNED(pStorageElem))
                    {
                        WriteStr2Array(pStorageElem->get_Name());
                        WriteStr2Array(Char0());
                        k++;
                        pStorageElem = (TStorageObj*)with0->StorageElements.Get_Next();
                    }
                }
            }
            if (myStrArray.empty())
				WriteStr2Array("None");
            *myPtr = (uintptr_t)(void*)&(myStrArray[0]);
            *mySize = myStrArray.size();
        }
        break;
        case 1:							// Storages.RegisterNames
        {
            *myType = 4; // String
            myStrArray.resize(0);
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                for (k = 0; k < NumStorageRegisters; k++)
                {
                    WriteStr2Array(StorageClass[ActiveActor]->RegisterNames[k]);
                    WriteStr2Array(Char0());
                }
            }
            if (myStrArray.empty())
                WriteStr2Array("None");
            *myPtr = (uintptr_t)(void*)&(myStrArray[0]);
            *mySize = myStrArray.size();
        }
        break;
        case 2:							// Storages.RegisterValues
        {
			*myType = 2; // Double
            myDblArray.resize(1);
            myDblArray[0] = 0.0;
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
			{
                auto with0 = ActiveCircuit[ActiveActor];
                pStorageElem = (TStorageObj*)with0->FActiveCktElement;
                if (ASSIGNED(pStorageElem))
				{
					myDblArray.resize(NumStorageRegisters);
					for (k = 0; k < NumStorageRegisters; k++)
					{
                        myDblArray[k] = pStorageElem->Registers[k];
					}
				}
			}
            *myPtr = (uintptr_t)(void*)&(myDblArray[0]);
            *mySize = myDblArray.size() * sizeof(myDblArray[0]);
        }
        break;
        default:
        {
            *myType = 4; // String
            myStrArray.resize(0);
            WriteStr2Array("Error, parameter not recognized");
            *myPtr = (uintptr_t)(void*)&(myStrArray[0]);
            *mySize = myStrArray.size();
        }
        break;

    }

    
}

//--------------------------------------------------------------------------------
// Implements the SwtControls  interface for the DLL
//--------------------------------------------------------------------------------
// ******************************Help Functions************************* 

TSwtControlObj* __stdcall ActiveSwtControl()
{
	TSwtControlObj* result = nullptr;
	if (ActiveCircuit[ActiveActor] != nullptr)
	{
		result = (TSwtControlObj*)ActiveCircuit[ActiveActor]->SwtControls.Get_Active();
	}
	return result;
}

void Set_ParameterSwtControl(string parm, string val)
{
	string mycmd = "";
	if ((ASSIGNED(ActiveCircuit[ActiveActor])))
	{
		SolutionAbort = false;// Reset for commands entered from outside
		mycmd = Format("swtcontrol.%s.%s=%s", ActiveSwtControl()->LName.c_str(), parm.c_str(), val.c_str());
		DSSExecutive[ActiveActor]->Set_Command(mycmd);
	}
}
// ******************************int type properties************************* 
int __stdcall SwtControlsI(int mode, int arg)
{
	TSwtControlObj* elem = nullptr;
	TPointerList*	lst = nullptr;
	int				result = 0;

	switch (mode)
	{
	case 0:												// SwtControls.
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->SwtControls);
			elem = (TSwtControlObj*)lst->Get_First();
			if (elem != nullptr)
			{
				do
				{
					if (elem->Get_Enabled())
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = 1;
					}
					else
					{
						elem = (TSwtControlObj*)lst->Get_Next();
					}
				} while (!((result == 1) || (!ASSIGNED(elem))));
			}
		}
		break;
	case 1:												// SwtControls.
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->SwtControls);
			elem = (TSwtControlObj*)lst->Get_Next();
			if (elem != nullptr)
			{
				do
				{
					if (elem->Get_Enabled())
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = lst->get_myActiveItem();
					}
					else
					{
						elem = (TSwtControlObj*)lst->Get_Next();
					}
				} while (!((result > 0) || (!ASSIGNED(elem))));
			}
		}
		break;
	case 2:												// SwtControls.
		result = 0;
		elem = ActiveSwtControl();
		if (elem != nullptr)
		{
			switch (elem->get_ActionCommand()) 
			{
			case CTRL_CLOSE:
				result = 2;
				break;
			case CTRL_OPEN:
				result = 1;
				break;
			}
		}
		break;
	case 3:												// SwtControls.
		elem = ActiveSwtControl();
		if (elem != nullptr)
		{
			switch (arg)
			{
			case	1:
				Set_ParameterSwtControl("Action", "0");
				break;
			case	2:
				Set_ParameterSwtControl("Action", "c");
					break;
			case	3:
				Set_ParameterSwtControl("Lock", "n");
				Set_ParameterSwtControl("Action", "c");
					break;
			case	4:
				Set_ParameterSwtControl("Lock", "y");
				break;
			case	5:
				Set_ParameterSwtControl("Lock", "n");
				break;
			}
		}
		break;
	case 4:												// SwtControls.
		result = 0;
		elem = ActiveSwtControl();
		if (elem != nullptr)
		{
			if (elem->FLocked)
			{
				result = 1;
			}
		}
		break;
	case 5:												// SwtControls.
		if (arg == 1)
		{
			Set_ParameterSwtControl("Lock", "y");
		}
		else
		{
			Set_ParameterSwtControl("Lock", "n");
		}
		break;
	case 6:												// SwtControls.
		result = 0;
		elem = ActiveSwtControl();
		if (elem != nullptr)
		{
			result = elem->ElementTerminal;
		}
		break;
	case 7:												// SwtControls.
		Set_ParameterSwtControl("SwitchedTerm", IntToStr(arg));
		break;
	case 8:												// SwtControls.
		if(ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->SwtControls.get_myNumList();
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall SwtControlsF(int mode, double arg)
{
	TSwtControlObj* elem = nullptr;
	double			result = 0.0;

	switch (mode)
	{
	case 0:												// SwtControls.
		result = 0.0;
		elem = ActiveSwtControl();
		if (elem != nullptr)
		{
			result = elem->TimeDelay;
		}
		break;
	case 1:												// SwtControls.
		Set_ParameterSwtControl("Delay", std::to_string(arg));
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall SwtControlsS(int mode, char* arg)
{
	TSwtControlObj* elem = nullptr;
	int				ActiveSave = 0;
	bool			Found = true;
	TPointerList*	lst = nullptr;
	string			S = "",
					result = "";

	switch (mode)
	{
	case 0:												// SwtControls.
		result = "";
		elem = ActiveSwtControl();
		if (elem != nullptr)
		{
			result = elem->get_Name();
		}
		break;
	case 1:												// SwtControls.
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->SwtControls);
			S = arg;
			Found = false;
			ActiveSave = lst->get_myActiveItem();
			elem = (TSwtControlObj*)lst->Get_First();
			while (elem != nullptr) 
			{
				if (CompareText(elem->get_Name(),S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
					Found = true;
					break;
				}
				elem = (TSwtControlObj*)lst->Get_Next();
			}
			if (!Found)
			{
				DoSimpleMsg("SwtControl " + S + " Not Found in Active Circuit.", 5003);
				elem = (TSwtControlObj*)lst->Get(ActiveSave);
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
			}
		}
		break;
	case 2:												// SwtControls.
		result = "";
		elem = ActiveSwtControl();
		if (elem != nullptr)
		{
			result = elem->ElementName;
		}
		break;
	case 3:												// SwtControls.
		Set_ParameterSwtControl("SwitchedObj",arg);
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall SwtControlsV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TSwtControlObj* elem = nullptr;
	TPointerList*	lst = nullptr;
	int				k = 0;

	switch (mode)
	{
	case 0:												// SwtControls.
		*myType = 4;			//String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->SwtControls.get_myNumList() > 0)
			{
				lst = &(with0->SwtControls);
				elem = (TSwtControlObj*)lst->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->get_Name());
					WriteStr2Array(Char0());
					elem = (TSwtControlObj*)lst->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	default:
		*myType = 4;			//String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Topology   interface for the DLL
//--------------------------------------------------------------------------------
//******************************Help Functions************************* 
TCktTree* ActiveTree()
{
	TCktTree* result = nullptr;
	if (ActiveCircuit[ActiveActor] != nullptr)
	{
		result =&(ActiveCircuit[ActiveActor]->GetTopology());
	}
	return result;
}
TCktTreeNode* ActiveTreeNode()
{
	TCktTree*		topo = nullptr;
	TCktTreeNode*	result = nullptr;
	topo = ActiveTree();
	if (ASSIGNED(topo))
	{
		result = topo->PresentBranch;
	}
	return result;
}
int ForwardBranch()
{
	TCktTree* topo = nullptr;
	int result = 0;
	topo = ActiveTree();
	if (ASSIGNED(topo) && topo->Get_Forward() != nullptr)
	{
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)topo->PresentBranch->CktObject);
		result = 1;
	}
	return result;
}
int ActiveBranch()
{
	TCktTree*		topo = nullptr;
	TCktTreeNode*	node = nullptr;
	int				result = 0;
	topo = ActiveTree();
	node = ActiveTreeNode();
	if (ASSIGNED(topo))
	{
		result = topo->Get_Level();
		if (node != nullptr)
		{
			ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)node->CktObject);
		}
	}
	return result;
}

// ******************************int type properties************************* 
int __stdcall TopologyI(int mode, int arg)
{
	TCktTree*		topo = nullptr;
	TPDElement*		pdElem = nullptr;
	TPDElement*		elm = nullptr;
	TCktTreeNode*	node = nullptr;
	int				result = 0;

	switch (mode)
	{
	case 0:												// Topology.NumLoops
		result = 0;
		topo = ActiveTree();
		if (topo != nullptr)
		{
			result = 0;
			pdElem = (TPDElement*)topo->Get_First();
			while (ASSIGNED(pdElem))
			{
				if (topo->PresentBranch->IsLoopedHere)
				{
					result++;
				}
				pdElem = (TPDElement*)topo->Get_Forward();
			}
		}
		result = result / 2;
		break;
	case 1:												// Topology.NumIsolatedBranches
		result = 0;
		topo = ActiveTree();
		if (ASSIGNED(topo))
		{
			elm = (TPDElement*)ActiveCircuit[ActiveActor]->PDElements.Get_First();
			while (ASSIGNED(elm))
			{
				if (elm->IsIsolated)
				{
					result++;
				}
				elm = (TPDElement*)ActiveCircuit[ActiveActor]->PDElements.Get_Next();
			}
		}
		break;
	case 2:												// Topology.NumIsolatedLoads
		result = 0;
		topo = ActiveTree();
		if (ASSIGNED(topo))
		{
			elm = (TPDElement*)ActiveCircuit[ActiveActor]->PCElements.Get_First();
			while (ASSIGNED(elm))
			{
				if (elm->IsIsolated)
				{
					result++;
				}
				elm = (TPDElement*)ActiveCircuit[ActiveActor]->PCElements.Get_Next();
			}
		}
		break;
	case 3:												// Topology.First
		result = 0;
		topo = ActiveTree();
		if (ASSIGNED(topo))
		{
			if (ASSIGNED(topo->Get_First()))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)topo->PresentBranch->CktObject);
				result = 1;
			}
		}
		break;
	case 4:												// Topology.Next
		result = ForwardBranch();
		break;
	case 5:												// Topology.ActiveBranch
		result = ActiveBranch();
		break;
	case 6:												// Topology.ForwardBranch
		result = ForwardBranch();
		break;
	case 7:												// Topology.BackwardBranch
		result = 0;
		topo = ActiveTree();
		if (ASSIGNED(topo))
		{
			if (ASSIGNED(topo->Get_Backward()))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)topo->PresentBranch->CktObject);
				result = 1;
			}
		}
		break;
	case 8:												// Topology.LoopedBranch
		result = 0;
		node = ActiveTreeNode();
		if (ASSIGNED(node))
		{
			if (node->IsLoopedHere)
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)node->LoopLineObj);
				result = 1;
			}
		}
		break;
	case 9:												// Topology.ParallelBranch
		result = 0;
		node = ActiveTreeNode();
		if (ASSIGNED(node))
		{
			if (node->IsParallel)
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)node->LoopLineObj);
				result = 1;
			}
		}
		break;
	case 10:											// Topology.FirstLoad
		result = 0;
		node = ActiveTreeNode();
		if (ASSIGNED(node))
		{
			elm = (TPDElement*)node->FShuntObjects->Get_First();
			if (ASSIGNED(elm))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elm);
				result = 1;
			}
		}
		break;
	case 11:											// Topology.NextLoad
		result = 0;
		node = ActiveTreeNode();
		if (ASSIGNED(node))
		{
			elm = (TPDElement*)node->FShuntObjects->Get_Next();
			if (ASSIGNED(elm))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elm);
				result = 1;
			}
		}
		break;
	case 12:											// Topology.ActiveLevel
		result = ActiveBranch();
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall TopologyS(int mode, char* arg)
{
	TCktTreeNode*	node = nullptr;
	TDSSCktElement* elm = nullptr;
	TCktTree*		topo = nullptr;
	bool			Found = false;
	TDSSCktElement* elem = nullptr;
	TPDElement*		pdElem = nullptr;
	string			S = "",
					B = "",
					result = "";

	switch (mode)
	{
	case 0:											// Topology.BranchName read
		result = "";
		node = ActiveTreeNode();
		if (ASSIGNED(node))
		{
			elm = (TDSSCktElement*)node->CktObject;
			if (ASSIGNED(elm))
			{
				result = elm->Get_QualifiedName();
			}
		}
		break;
	case 1:											// Topology.BranchName write
		Found = false;
		elem = nullptr;
		S = std::string(arg);
		topo = ActiveTree();
		if (ASSIGNED(topo))
		{
			elem = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
			pdElem = (TPDElement*)topo->Get_First();
			while (ASSIGNED(pdElem))
			{
				if (CompareText(pdElem->Get_QualifiedName(),S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pdElem);
					Found = true;
					break;
				}
				pdElem = (TPDElement*)topo->Get_Forward();
			}
		}
		if (!Found)
		{
			DoSimpleMsg("Branch '" + S + "' Not Found in Active Circuit Topology.", 5003);
			if (ASSIGNED(elem))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
			}
		}
		break;
	case 2:											// Topology.BusName read
		result = "";
		node = ActiveTreeNode();
		if (ASSIGNED(node))
		{
			elm = (TPDElement*)node->CktObject;
			if (ASSIGNED(elm))
			{
				result = elm->Get_FirstBus();
			}
		}
		break;
	case 3:											// Topology.BusName write
		Found = false;
		elem = nullptr;
		S = std::string(arg);
		topo = ActiveTree();
		if (ASSIGNED(topo))
		{
			elem = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
			pdElem =(TPDElement*)topo->Get_First();
			while (ASSIGNED(pdElem) && !Found)
			{
				B = pdElem->Get_FirstBus();
				while (B.size() > 0)
				{
					if (CompareText(B, S) == 0)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pdElem);
						Found = true;
						break;
					}
					B = pdElem->Get_NextBus();
				}
				pdElem = (TPDElement*)topo->Get_Forward();
			}
		}
		if (!Found)
		{
			DoSimpleMsg("Bus " + S + " Not Found in Active Circuit Topology.", 5003);
			if (ASSIGNED(elem))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
			}
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;

	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall TopologyV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TCktTree*	topo = nullptr;
	TPDElement* pdElem = nullptr;
	TPDElement* pdLoop = nullptr;
	int			k = 0, 
				i = 0;
	bool		Found = false;
	TPDElement* elm = nullptr;
	std::vector<string> TStr;

	switch (mode)
	{
	case 0:											// Topology.AllLoopedPairs
		*myType = 4;
		myStrArray.resize(0);
		TStr.resize(1);
		TStr[0] = "NONE";
		k = -1;
		topo = ActiveTree();
		if (topo != nullptr)
		{
			pdElem = (TPDElement*)topo->Get_First();
			while (ASSIGNED(pdElem))
			{
				if (topo->PresentBranch->IsLoopedHere)
				{
					pdLoop = (TPDElement*)topo->PresentBranch->LoopLineObj;
					Found = false;
					i = 1;
					while (i <= k && !Found)
					{
						if (TStr[i-1] == pdElem->Get_QualifiedName() && TStr[i] == pdLoop->Get_QualifiedName())
						{
							Found = true;
						}
						if (TStr[i - 1] == pdLoop->Get_QualifiedName() && TStr[i] == pdElem->Get_QualifiedName())
						{
							Found = true;
						}
						i++;
					}
					if (!Found)
					{
						k = k + 2;
						TStr.resize(k + 1);
						TStr[k - 1] = pdElem->Get_QualifiedName();
						TStr[k] = pdLoop->Get_QualifiedName();
					}
				}
				pdElem = (TPDElement*)topo->Get_Forward();
			}
		}
		if (TStr.size() > 0)
		{
			for (i = 0; i < TStr.size(); i++)
			{
				if (TStr[i] != "")
				{
					WriteStr2Array(TStr[i]);
					WriteStr2Array(Char0());
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 1:											// Topology.AllIsolatedBranches
		*myType = 4;
		myStrArray.resize(0);
		TStr.resize(1);
		TStr[0] = "NONE";
		k = 0;
		topo = ActiveTree();
		if (ASSIGNED(topo))
		{
			elm = (TPDElement*)ActiveCircuit[ActiveActor]->PDElements.Get_First();
			while (ASSIGNED(elm))
			{
				if (elm->IsIsolated)
				{
					TStr[k] = elm->Get_QualifiedName();
					k++;
					if (k > 0)
					{
						TStr.resize(k + 1);
					}
				}
				elm = (TPDElement*)ActiveCircuit[ActiveActor]->PDElements.Get_Next();
			}
		}
		if (TStr.size() > 0)
		{
			for (i = 0; i < TStr.size(); i++)
			{
				if (TStr[i] != "")
				{
					WriteStr2Array(TStr[i]);
					WriteStr2Array(Char0());
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 2:											// Topology.AllIsolatedLoads
		*myType = 4;
		myStrArray.resize(0);
		TStr.resize(1);
		TStr[0] = "NONE";
		k = 0;
		topo = ActiveTree();
		if (ASSIGNED(topo))
		{
			elm = (TPDElement*)ActiveCircuit[ActiveActor]->PCElements.Get_First();
			while (ASSIGNED(elm))
			{
				if (elm->IsIsolated)
				{
					TStr[k] = elm->Get_QualifiedName();
					k++;
					if (k > 0)
					{
						TStr.resize(k + 1);
					}
				}
				elm = (TPDElement*)ActiveCircuit[ActiveActor]->PCElements.Get_Next();
			}
		}
		if (TStr.size() > 0)
		{
			for (i = 0; i < TStr.size(); i++)
			{
				if (TStr[i] != "")
				{
					WriteStr2Array(TStr[i]);
					WriteStr2Array(Char0());
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	default:
		*myType = 4;
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the Transformers   interface for the DLL
//--------------------------------------------------------------------------------
// ******************************Help Functions*************************
TTransfObj* __stdcall ActiveTransformer()
{
	TTransfObj* result = nullptr;
	if (ActiveCircuit[ActiveActor] != nullptr)
	{
		result = (TTransfObj*)ActiveCircuit[ActiveActor]->Transformers.Get_Active();
	}
	return result;
}

void Set_ParameterTransformer(string parm, string val)
{
	string mycmd = "";
	if ((ASSIGNED(ActiveCircuit[ActiveActor])))
	{
		SolutionAbort = false;// Reset for commands entered from outside
		mycmd = Format("transformer.%s.%s=%s", ActiveTransformer()->get_Name().c_str(), parm.c_str(), val.c_str());
		DSSExecutive[ActiveActor]->Set_Command(mycmd);
	}
}
// ******************************int type properties************************* 
int __stdcall TransformersI(int mode, int arg)
{
	TTransfObj*		elem = nullptr;
	TPointerList*	lst = nullptr;
	int result = 0;

	switch (mode)
	{
	case 0:											//Transformers.NumWindings read
		result = 0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->get_NumWindings();
		}
		break;
	case 1:											//Transformers.NumWindings write
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			elem->SetNumWindings(arg);
		}
		break;
	case 2:											//Transformers.Wdg read
		result = 0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->ActiveWinding;
		}
		break;
	case 3:											//Transformers.Wdg write
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			if ((arg > 0) && (arg <= elem->get_NumWindings()))
			{
				elem->ActiveWinding = arg;
			}
		}
		break;
	case 4:											//Transformers.NumTaps read
		result = 0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->Get_NumTaps(elem->ActiveWinding);
		}
		break;
	case 5:											//Transformers.NumTaps write
		Set_ParameterTransformer("NumTaps", IntToStr(arg));
		break;
	case 6:											//Transformers.IsDelta read
		result = 0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			if (elem->Get_WdgConnection(elem->ActiveWinding) > 0)
			{
				result = 1;
			}
		}
		break;
	case 7:											//Transformers.IsDelta write
		if (arg == 1)
		{
			Set_ParameterTransformer("Conn", "Delta");
		}
		else
		{
			Set_ParameterTransformer("Conn", "Wye");
		}
		break;
	case 8:											//Transformers.First
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->Transformers);
			elem = (TTransfObj*)lst->Get_First();
			if (elem != nullptr)
			{
				do
				{
					if (elem->Get_Enabled())
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = 1;
					}
					else
					{
						elem = (TTransfObj*)lst->Get_Next();
					}
				} while (!((result == 1) || (!ASSIGNED(elem))));
			}
		}
		break;
	case 9:											//Transformers.Next
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->Transformers);
			elem = (TTransfObj*)lst->Get_Next();
			if (elem != nullptr)
			{
				do
				{
					if (elem->Get_Enabled())
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
						result = lst->get_myActiveItem();
					}
					else
					{
						elem = (TTransfObj*)lst->Get_Next();
					}
				} while (!((result > 0) || (!ASSIGNED(elem))));
			}
		}
		break;
	case 10:										//Transformers.Count
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = ActiveCircuit[ActiveActor]->Transformers.get_myNumList();
		}
		break;
	case 11:										//Transformers.CoreType read
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->CoreType;
		}
		break;
	case 12:										//Transformers.CoreType write
		elem = ActiveTransformer();
		if (elem !=nullptr)
		{
			elem->CoreType = arg;
			switch (arg)
			{
			case	1:
				elem->strCoreType = "1-phase";
				break;
			case	3:
				elem->strCoreType = "3-leg";
				break;
			case	5:
				elem->strCoreType = "4-leg";
				break;
			case	7:
				elem->strCoreType = "5-leg";
				break;
			case	9:
				elem->strCoreType = "core-1-phase";
				break;
			default:
				elem->strCoreType = "shell";
				break;
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall TransformersF(int mode, double arg)
{
	TTransfObj* elem = nullptr;
	double result = 0.0;

	switch (mode)
	{
	case 0:										//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->Get_WdgResistance(elem->ActiveWinding) * 100.0;
		}
		break;
	case 1:										//Transformers.
		Set_ParameterTransformer("%R", std::to_string(arg));
		break;
	case 2:										//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->Get_PresentTap(elem->ActiveWinding,ActiveActor);
		}
		break;
	case 3:										//Transformers.
		Set_ParameterTransformer("Tap", std::to_string(arg));
		break;
	case 4:										//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->Get_MinTap(elem->ActiveWinding);
		}
		break;
	case 5:										//Transformers.
		Set_ParameterTransformer("MinTap", std::to_string(arg));
		break;
	case 6:										//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->Get_MaxTap(elem->ActiveWinding);
		}
		break;
	case 7:										//Transformers.
		Set_ParameterTransformer("MaxTap", std::to_string(arg));
		break;
	case 8:										//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->WINDING_[elem->ActiveWinding - 1].kVLL;
		}
		break;
	case 9:										//Transformers.
		Set_ParameterTransformer("kv", std::to_string(arg));
		break;
	case 10:									//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->Get_WdgkVA(elem->ActiveWinding);
		}
		break;
	case 11:									//Transformers.
		Set_ParameterTransformer("kva", std::to_string(arg));
		break;
	case 12:									//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->Get_WdgXneutral(elem->ActiveWinding);
		}
		break;
	case 13:									//Transformers.
		Set_ParameterTransformer("Xneut", std::to_string(arg));
		break;
	case 14:									//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->Get_WdgRneutral(elem->ActiveWinding);
		}
		break;
	case 15:									//Transformers.
		Set_ParameterTransformer("Rneut", std::to_string(arg));
		break;
	case 16:									//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->get_XHL() * 100.0;
		}
		break;
	case 17:									//Transformers.
		Set_ParameterTransformer("Xhl", std::to_string(arg));
		break;
	case 18:									//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->get_XHT() * 100.0;
		}
		break;
	case 19:									//Transformers.
		Set_ParameterTransformer("Xht", std::to_string(arg));
		break;
	case 20:									//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->get_XLT() * 100.0;
		}
		break;
	case 21:									//Transformers.
		Set_ParameterTransformer("Xlt", std::to_string(arg));
		break;
	case 22:									//Transformers.
		result = 0.0;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->Get_WdgRdc(elem->ActiveWinding);
		}
		break;
	case 23:									//Transformers.
		Set_ParameterTransformer("RdcOhms", std::to_string(arg));
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall TransformersS(int mode, char* arg)
{
	TTransfObj*		elem = nullptr;
	int				ActiveSave = 0;
	bool			Found = false;
	TPointerList*	lst = nullptr;
	string			S = "",
					result = "";

	switch (mode)
	{
	case 0:										//Transformers.
		result = "";
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->XfmrCode;
		}
		break;
	case 1:										//Transformers.
		Set_ParameterTransformer("XfmrCode", arg);
		break;
	case 2:										//Transformers.
		result = "";
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			elem = (TTransfObj*)ActiveCircuit[ActiveActor]->Transformers.Get_Active();
			if (elem != nullptr)
			{
				result = elem->get_Name();
			}
		}
		break;
	case 3:										//Transformers.
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			lst = &(ActiveCircuit[ActiveActor]->Transformers);
			S = arg;
			Found = false;
			ActiveSave = lst->get_myActiveItem();
			elem = (TTransfObj*)lst->Get_First();
			while (elem != nullptr)
			{
				if (CompareText(elem->get_Name(),S) == 0)
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
					Found = true;
					break;
				}
				elem = (TTransfObj*)lst->Get_Next();
			}
			if (!Found)
			{
				DoSimpleMsg("Transformer '" + S + "' Not Found in Active Circuit.", 5003);
				elem = (TTransfObj*)lst->Get(ActiveSave);
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement(elem);
			}
		}
		break;
	case 4:										//Transformers.
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			result = elem->GetWindingCurrentsResult(ActiveActor);
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall TransformersV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TTransfObj*		elem = nullptr;
	TPointerList*	lst = nullptr;
	int				i = 0,
					iV = 0,
					NumCurrents = 0,
					k = 0;
	vector<complex>	TempCurrentBuffer, 
					TempVoltageBuffer;
	switch (mode)
	{
	case 0:										//Transformers.
		*myType = 4;				//String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			auto with0 = ActiveCircuit[ActiveActor];
			if (with0->Transformers.get_myNumList() > 0)
			{
				lst = &(with0->Transformers);
				k = 0;
				elem = (TTransfObj*)lst->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->get_Name());
					WriteStr2Array(Char0());
					k++;
					elem = (TTransfObj*)lst->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	case 1:										//Transformers.
		*myType = 3;			//Complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		elem = ActiveTransformer();
		if (elem != nullptr && !ActiveCircuit[ActiveActor]->Solution->NodeV.empty())
		{
			if (elem->ActiveWinding > 0 && (elem->ActiveWinding <= elem->get_NumWindings()))
			{
				myCmplxArray.resize(elem->Get_NPhases());
				TempVoltageBuffer.resize(elem->Get_NPhases());
				elem->GetWindingVoltages(elem->ActiveWinding, &(TempVoltageBuffer[0]), ActiveActor);
				for (i = 0; i < elem->Get_NPhases(); i++)
				{
					myCmplxArray[i] = TempVoltageBuffer[i];
				}
				TempVoltageBuffer.clear();
			}
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = sizeof(myCmplxArray[0]) * myCmplxArray.size();
		break;
	case 2:										//Transformers.
		*myType = 3;			//Complex
		myCmplxArray.resize(1);
		myCmplxArray[0] = CZero;
		elem = ActiveTransformer();
		if (elem != nullptr)
		{
			NumCurrents = 2 * elem->Get_NPhases() * elem->get_NumWindings();
			myCmplxArray.resize(NumCurrents);
			TempCurrentBuffer.resize(NumCurrents);
			elem->GetAllWindingCurrents(&(TempCurrentBuffer[0]), ActiveActor);
			for (i = 0; i < NumCurrents; i++)
			{
				myCmplxArray[i] = TempCurrentBuffer[i];
			}
			TempCurrentBuffer.clear();
		}
		*myPtr = (uintptr_t)(void*)(myCmplxArray.data());
		*mySize = sizeof(myCmplxArray[0]) * myCmplxArray.size();
		break;
	default:
		*myType = 4;				//String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}


//--------------------------------------------------------------------------------
// Implements the VSources    interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall VsourcesI(int mode, int arg)
{
	TVsourceObj*	pElem = nullptr;
	TVsourceObj*	elem = nullptr;
	int				result = 0;

	switch (mode)
	{
	case 0:										//VSources.
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			result = VSourceClass[ActiveActor]->ElementList.get_myNumList();
		}
		break;
	case 1:										//VSources.
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_First();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->Get_Enabled())
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = 1;
					}
					else
					{
						pElem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Next();
					}
				} while (!((result == 1) || (!ASSIGNED(elem))));
			}
		}
		break;
	case 2:										//VSources.
		result = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pElem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Next();
			if (pElem != nullptr)
			{
				do
				{
					if (pElem->Get_Enabled())
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pElem);
						result = VSourceClass[ActiveActor]->ElementList.get_myActiveItem();
					}
					else
					{
						pElem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Next();
					}
				} while (!((result > 0) || (!ASSIGNED(elem))));
			}
		}
		break;
	case 3:										//VSources.
		result = 0;
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->Get_NPhases();
		}
		break;
	case 4:										//VSources.
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->GetActiveObj();
		if (elem != nullptr)
		{
			elem->Set_NPhases(arg);
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall VsourcesF(int mode, double arg)
{
	TVsourceObj*	elem = nullptr;
	double			result = 0.0;

	switch (mode)
	{
	case 0:										//VSources.
		result = 0.0;
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->kVBase;
		}
		break;
	case 1:										//VSources.
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->kVBase = arg;
		}
		break;
	case 2:										//VSources.
		result = 0.0;
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->PerUnit;
		}
		break;
	case 3:										//VSources.
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->PerUnit = arg;
		}
		break;
	case 4:										//VSources.
		result = 0.0;
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->Angle;
		}
		break;
	case 5:										//VSources.
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->Angle = arg;
		}
		break;
	case 6:										//VSources.
		result = 0.0;
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			result = elem->SrcFrequency;
		}
		break;
	case 7:										//VSources.
		elem = (TVsourceObj*)VSourceClass[ActiveActor]->ElementList.Get_Active();
		if (elem != nullptr)
		{
			elem->SrcFrequency = arg;
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall VsourcesS(int mode, char* arg)
{
	TDSSCktElement* elem = nullptr;
	string			result = "";    // Default return value

	switch (mode)
	{
	case 0:										//VSources.
		result = "";
		elem = ActiveCircuit[ActiveActor]->get_FActiveCktElement();
		if (elem != nullptr)
		{
			result = elem->get_Name();
		}
		break;
	case 1:										//VSources.
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (VSourceClass[ActiveActor]->SetActive(std::string(arg)))
			{
				ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*)VSourceClass[ActiveActor]->ElementList.Get_Active());
			}
			else
			{
				DoSimpleMsg("Vsource '" + (string)arg + "' Not Found in Active Circuit.", 77003);
			}
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall VsourcesV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TVsourceObj*	elem = nullptr;
	TPointerList*	pList = nullptr;
	int				k = 0;

	switch (mode)
	{
	case 0:										//VSources.
		*myType = 4;				//String
		myStrArray.resize(0);
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			if (VSourceClass[ActiveActor]->ElementList.get_myNumList() > 0)
			{
				pList = &(VSourceClass[ActiveActor]->ElementList);
				elem = (TVsourceObj*)pList->Get_First();
				while (elem != nullptr)
				{
					WriteStr2Array(elem->get_Name());
					WriteStr2Array(Char0());
					elem = (TVsourceObj*) pList->Get_Next();
				}
			}
		}
		if (myStrArray.size() == 0)
		{
			WriteStr2Array("None");
			WriteStr2Array(Char0());
		}
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	default:
		*myType = 4;				//String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}

//--------------------------------------------------------------------------------
// Implements the WindGens interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties*************************
int __stdcall WindGensI(int mode, int arg)
{
    TWindGenObj* WindGenElem = nullptr;
    TPointerList* pList = nullptr; 
    int Result = 0; // Default return value

	switch (mode)
    {
        case 0:									// WindGens.First
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                if (WindGenClass[ActiveActor]->ElementList.NumInList > 0)
                {
                    pList = &WindGenClass[ActiveActor]->ElementList;
                    WindGenElem = (TWindGenObj*)pList->Get_First();
					if (WindGenElem != nullptr)
					{
						do
						{
							if (WindGenElem->FEnabled)
							{
								ActiveCircuit[ActiveActor]->Set_ActiveCktElement(WindGenElem);
								Result = 1;
							}
							else
								WindGenElem = (TWindGenObj*)pList->Get_Next();
						} while (!((Result == 1) || (!ASSIGNED(WindGenElem))));
					}
                }
                else
                    Result = 0; // signify no more
            }
        }
        break;
        case 1:									// WindGens.Next
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                if (WindGenClass[ActiveActor]->ElementList.NumInList > 0)
                {
                    pList = &WindGenClass[ActiveActor]->ElementList;
                    WindGenElem = (TWindGenObj*)pList->Get_Next();
					if (WindGenElem != nullptr)
					{
						do
						{
							if (WindGenElem->FEnabled)
							{
								ActiveCircuit[ActiveActor]->Set_ActiveCktElement(WindGenElem);
								Result = pList->ActiveItem;
							}
							else
								WindGenElem = (TWindGenObj*)pList->Get_Next();
						} while (!((Result > 0) || (!ASSIGNED(WindGenElem))));
					}
                }
                else
                    Result = 0; // signify no more
            }
        }
        break;
        case 2:										// WindGens.Count
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Result = WindGenClass[ActiveActor]->ElementList.NumInList;
            }
        }
        break;
        case 3:										// WindGens.Idx read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                Result = WindGenClass[ActiveActor]->ElementList.ActiveItem;
            }
        }
        break;
        case 4:										// WindGens.Idx Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get(arg);
                if (ASSIGNED(WindGenElem))
                {
                    ActiveCircuit[ActiveActor]->Set_ActiveCktElement(WindGenElem);
                }
            }
        }
        break;
        case 5:										// WindGens.N_WTG Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindModelDyn->N_WTG;
                }
            }
        }
        break;
        case 6:										// WindGens.N_WTG Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindModelDyn->N_WTG = arg;
                }
            }
        }
        break;
        case 7:										// WindGens.NPoles Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = (int)round(WindGenElem->WindGenVars.Poles);
                }
            }
        }
        break;
        case 8:										// WindGens.NPoles Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindGenVars.Poles = (double)arg;
                }
            }
        }
        break;
        case 9:										// WindGens.QFlag Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindModelDyn->QFlg;
                }
            }
        }
        break;
        case 10:									// WindGens.QFlag Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindModelDyn->QFlg = arg;
                }
            }
        }
        break;
        case 11:									// WindGens.QMode Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindModelDyn->QMode;
                }
            }
        }
        break;
        case 12:									// WindGens.QMode Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindModelDyn->QMode = arg;
                }
            }
        }
        break;
        default:
            Result = -1; // The user is asking for the wrong command
        break;
    }

	return Result;


}
//******************************Float point type properties****************************
double __stdcall WindGensF(int mode, double arg)
{
    TWindGenObj* WindGenElem = nullptr;
    double Result = 0.0; // Default return value

	switch (mode)
    {
        case 0:										// WindGens.Ag Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindGenVars.ag;
                }
            }
        }
        break;
        case 1:										// WindGens.Ag Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                   WindGenElem->WindGenVars.ag = arg;
                }
            }
        }
        break;
        case 2:										// WindGens.Cp Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindGenVars.Cp;
                }
            }
        }
        break;
        case 3:										// WindGens.Cp Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindGenVars.Cp = arg;
                }
            }
        }
        break;
        case 4:										// WindGens.kV Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->Get_PresentkV();
                }
            }
        }
        break;
        case 5:										// WindGens.kV Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->Set_PresentkV(arg);
                }
            }
        }
        break;
        case 6:										// WindGens.kVA Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindGenVars.kVArating;
                }
            }
        }
        break;
        case 7:										// WindGens.kVA Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindGenVars.kVArating = arg;
                    WindGenElem->WindModelDyn->EditProp(13, to_string(arg));
                }
            }
        }
        break;
        case 8:										// WindGens.kvar Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->Get_Presentkvar();
                }
            }
        }
        break;
        case 9:										// WindGens.kvar Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->Set_Presentkvar(arg);
                }
            }
        }
        break;
        case 10:									// WindGens.kW Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->Get_PresentkW();
                }
            }
        }
        break;
        case 11:									// WindGens.kW Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->Set_PresentkW(arg);
                }
            }
        }
        break;
        case 12:									// WindGens.Lamda Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindGenVars.Lamda;
                }
            }
        }
        break;
        case 13:									// WindGens.Lamda Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindGenVars.Lamda = arg;
                }
            }
        }
        break;
        case 14:									// WindGens.pd Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindGenVars.pd;
                }
            }
        }
        break;
        case 15:									// WindGens.pd Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindGenVars.pd = arg;
                }
            }
        }
        break;
        case 16:									// WindGens.PF Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->PFNominal;
                }
            }
        }
        break;
        case 17:									// WindGens.PF Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->PFNominal = arg;
                }
            }
        }
        break;
        case 18:									// WindGens.Pss Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindModelDyn->Pss;
                }
            }
        }
        break;
        case 19:									// WindGens.Pss Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindModelDyn->Pss = arg;
                }
            }
        }
        break;
        case 20:									// WindGens.Qss Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindModelDyn->Qss;
                }
            }
        }
        break;
        case 21:									// WindGens.Qss Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindModelDyn->Qss = arg;
                }
            }
        }
        break;
        case 22:									// WindGens.Rad Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindGenVars.Rad;
                }
            }
        }
        break;
        case 23:									// WindGens.Rad Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindGenVars.Rad = arg;
                }
            }
        }
        break;
        case 24:									// WindGens.RThev Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindModelDyn->Rthev;
                }
            }
        }
        break;
        case 25:									// WindGens.RThev Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindModelDyn->Rthev = arg;
                }
            }
        }
        break;
        case 26:									// WindGens.VCutOut Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindGenVars.VCutout;
                }
            }
        }
        break;
        case 27:									// WindGens.VCutOut Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindGenVars.VCutout = arg;
                }
            }
        }
        break;
        case 28:									// WindGens.VCutIn Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindGenVars.VCutin;
                }
            }
        }
        break;
        case 29:									// WindGens.VCutIn Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindGenVars.VCutin = arg;
                }
            }
        }
        break;
        case 30:									// WindGens.Vss Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindModelDyn->Vss;
                }
            }
        }
        break;
        case 31:									// WindGens.Vss Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindModelDyn->Vss = arg;
                }
            }
        }
        break;
        case 32:									// WindGens.WindSpeed Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindModelDyn->vwind;
                }
            }
        }
        break;
        case 33:									// WindGens.WindSpeed Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindModelDyn->vwind = arg;
                }
            }
        }
        break;
        case 34:									// WindGens.XThev Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->WindModelDyn->Xthev;
                }
            }
        }
        break;
        case 35:									// WindGens.XThev Write
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    WindGenElem->WindModelDyn->Xthev = arg;
                }
            }
        }
        break;
        default:
            Result = -1.0; // we got the wrong command
        break;
    }
    return Result;
}
//******************************String type properties****************************
char* __stdcall WindGensS(int mode, char* arg)
{
    TWindGenObj*	WindGenElem = nullptr;
	int				k	=	0,
					ActiveSave = 0;
    TPointerList*	pList = nullptr;
    string			Result = "", // Default return value
					S = "";
    bool			Found = false;

	switch (mode)
    {
        case 0:										// WindGen.Name Read
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    Result = WindGenElem->get_Name();
                }
            }
        }
        break;
        case 1:
        {
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                pList = &(ActiveCircuit[ActiveActor]->WindGens);
                S = arg;
                Found = false;
                ActiveSave = pList->get_myActiveItem();
                WindGenElem = (TWindGenObj*)pList->Get_First();
                while (WindGenElem != nullptr)
                {
                    if (CompareText(WindGenElem->get_Name(), S) == 0)
                    {
                        ActiveCircuit[ActiveActor]->Set_ActiveCktElement(WindGenElem);
                        Found = true;
                        break;
                    }
                    WindGenElem = (TWindGenObj*)pList->Get_Next();
                }
                if (!Found)
                {
                    DoSimpleMsg("WindGen '" + S + "' Not Found in Active Circuit.", 5003);
                    WindGenElem = (TWindGenObj*)pList->Get(ActiveSave);
                    ActiveCircuit[ActiveActor]->Set_ActiveCktElement(WindGenElem);
                }
            }
        }
        break;
        default:
            Result = "Error, parameter not valid";
    }

    char* presult = new char[Result.size() + 1];
    strcpy(presult, Result.c_str());
    return presult;
}
//************************Structure type properties*******************************
void __stdcall WindGensV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
    TWindGenObj*	WindGenElem = nullptr;
    int				k = 0;
    TPointerList*	pList = nullptr;

	switch (mode)
    {
        case 0:									// WindGen.AllNames
        {
			*myType = 4; //string
            myStrArray.resize(0);
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                if (WindGenClass[ActiveActor]->ElementList.NumInList > 0)
                {
                    pList = &(WindGenClass[ActiveActor]->ElementList);
                    k = 0;
                    WindGenElem = (TWindGenObj*)pList->Get_First();
                    while (ASSIGNED(WindGenElem))
                    {
                        WriteStr2Array(WindGenElem->get_Name());
                        WriteStr2Array(Char0());
						k++;
                        WindGenElem = (TWindGenObj*)pList->Get_Next();
                    }
                }
            }
            if (myStrArray.empty())
                WriteStr2Array("None");
            *myPtr = (uintptr_t)(void*)&(myStrArray[0]);
            *mySize = myStrArray.size();
        }
        break;
        case 1:									// WindGen.RegisterNames
        {
            *myType = 4; // string
            myStrArray.resize(0);

			for (k = 0; k < NumWGenRegisters; k++)
            {
                WriteStr2Array(WindGenClass[ActiveActor]->RegisterNames[k]);
                WriteStr2Array(Char0());
            }

			if (myStrArray.empty())
                WriteStr2Array("None");
            *myPtr = (uintptr_t)(void*)&(myStrArray[0]);
            *mySize = myStrArray.size();
        }
        break;
        case 2:									// WindGen.RegisterValues
        {
            *myType = 2; // Double
            myDblArray.resize(1);
            myDblArray[0] = 0;
            if (ASSIGNED(ActiveCircuit[ActiveActor]))
            {
                WindGenElem = (TWindGenObj*)WindGenClass[ActiveActor]->ElementList.Get_Active();
                if (ASSIGNED(WindGenElem))
                {
                    myDblArray.resize(NumWGenRegisters);
                    for (k = 0; k < NumWGenRegisters; k++)
                    {
                        myDblArray[k] = WindGenElem->Registers[k];
                    }
                }
            }
            *myPtr = (uintptr_t)(void*)&(myDblArray[0]);
            *mySize = myDblArray.size() * sizeof(myDblArray[0]);
        }
        break;
		default:								// Something else
            *myType = 4; // string
            myStrArray.resize(0);
            WriteStr2Array("Error, parameter not recognized");
            *myPtr = (uintptr_t)(void*)&(myStrArray[0]);
            *mySize = myStrArray.size();
        break;
    }
}

//--------------------------------------------------------------------------------
// Implements the XYCurves    interface for the DLL
//--------------------------------------------------------------------------------
// ******************************int type properties************************* 
int __stdcall XYCurvesI(int mode, int arg)
{
	TXYcurveObj*	pXYCurve = nullptr;
	int				result = 0;

	switch (mode)
	{
	case 0:										//XYCurves.Count
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = XYCurveClass[ActiveActor]->Get_ElementCount();
		}
		break;
	case 1:										//XYCurves.First
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = XYCurveClass[ActiveActor]->Get_First();
		}
		break;
	case 2:										//XYCurves.Next
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			result = XYCurveClass[ActiveActor]->Get_Next();
		}
		break;
	case 3:										//XYCurves.Npts read
		result = 0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				result = pXYCurve->get_FNumPoints();
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51009);
			}
		}
		break;
	case 4:										//XYCurves.Npts write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				pXYCurve->Set_NumPoints(arg);
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51014);
			}
		}
		break;
	default:
		result = -1;
		break;
	}
	return result;
}
//******************************Float point type properties****************************
double __stdcall XYCurvesF(int mode, double arg)
{
	TXYcurveObj*	pXYCurve = nullptr;
	double			result = 0.0;

	switch (mode)
	{
	case 0:										//XYCurves.X read
		result = 0.0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				result = pXYCurve->Get_X();
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51010);
			}
		}
		break;
	case 1:										//XYCurves.X write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				pXYCurve->Set_X(arg);
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51010);
			}
		}
		break;
	case 2:										//XYCurves.Y read
		result = 0.0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				result = pXYCurve->Get_Y();
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51011);
			}
		}
		break;
	case 3:										//XYCurves.Y write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				pXYCurve->Set_Y(arg);
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51010);
			}
		}
		break;
	case 4:										//XYCurves.Xshift read
		result = 0.0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				result = pXYCurve->FXshift;
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51011);
			}
		}
		break;
	case 5:										//XYCurves.Xshift write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				pXYCurve->FXshift = arg;
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51010);
			}
		}
		break;
	case 6:										//XYCurves.YShift read
		result = 0.0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				result = pXYCurve->FYshift;
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51011);
			}
		}
		break;
	case 7:										//XYCurves.YShift write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				pXYCurve->FYshift = arg;
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51010);
			}
		}
		break;
	case 8:										//XYCurves.XScale read
		result = 0.0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				result = pXYCurve->FXscale;
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51011);
			}
		}
		break;
	case 9:										//XYCurves.XScale write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				pXYCurve->FXscale = arg;
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51010);
			}
		}
		break;
	case 10:									//XYCurves.YScale read
		result = 0.0;
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				result = pXYCurve->FYscale;
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51011);
			}
		}
		break;
	case 11:									//XYCurves.YScale write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				pXYCurve->FYscale = arg;
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51010);
			}
		}
		break;
	default:
		result = -1.0;
		break;
	}
	return result;
}
//******************************String type properties****************************
char* __stdcall XYCurvesS(int mode, char* arg)
{
	TXYcurveObj*	pXYCurve = nullptr;
	string			result ="";

	switch (mode)
	{
	case 0:										//XYCurves.Name read
		result = "";
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				result = pXYCurve->get_Name();
			}
		}
		break;
	case 1:										//XYCurves.Name write
		if (ASSIGNED(ActiveCircuit[ActiveActor]))
		{
			if (! XYCurveClass[ActiveActor]->SetActive(string(arg)))
			{
				DoSimpleMsg("XYCurve '" + string(arg) + "' Not Found in Active Circuit.", 51008);
			}
		}
		break;
	default:
		result = "Error, parameter not valid";
		break;
	}
	char* presult = new char[result.size() + 1];
	strcpy(presult, result.c_str());
	return  presult;
}
//************************Structure type properties*******************************
void __stdcall XYCurvesV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	TXYcurveObj* pXYCurve = nullptr;
	int				k = 0,
		i = 0,
		LoopLimit = 0;
	double* pDouble = nullptr;

	switch (mode)
	{
	case 0:										//XYCurves.x read
		*myType = 2;			//Double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				myDblArray.resize(pXYCurve->get_FNumPoints());
				for (k = 0; k < pXYCurve->get_FNumPoints(); k++)
				{
					myDblArray[k] = pXYCurve->Get_XValue(k + 1);
				}
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51013);
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		break;
	case 1:										//XYCurves.x write
		*myType = 2;
		k = 1;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				LoopLimit = pXYCurve->get_FNumPoints();
				pDouble = *(double**)myPtr;
				for (i = 1; i <= LoopLimit; i++)
				{
					pXYCurve->Set_XValue(k, *pDouble);
					pDouble++;
					k++;
				}
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51015);
			}
		}
		*mySize = k - 1;
		break;
	case 2:										//XYCurves.Y read
		*myType = 2;			//Double
		myDblArray.resize(1);
		myDblArray[0] = 0;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				myDblArray.resize(pXYCurve->get_FNumPoints());
				for (k = 0; k < pXYCurve->get_FNumPoints(); k++)
				{
					myDblArray[k] = pXYCurve->Get_YValue(k + 1);
				}
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51013);
			}
		}
		*myPtr = (uintptr_t)(void*)&(myDblArray[0]);
		*mySize = sizeof(myDblArray[0]) * myDblArray.size();
		break;
	case 3:										//XYCurves.Y write
		*myType = 2;
		k = 1;
		if (ActiveCircuit[ActiveActor] != nullptr)
		{
			pXYCurve = (TXYcurveObj*)XYCurveClass[ActiveActor]->GetActiveObj();
			if (pXYCurve != nullptr)
			{
				LoopLimit = pXYCurve->get_FNumPoints();
				pDouble = *(double**)myPtr;
				for (i = 1; i <= LoopLimit; i++)
				{
					pXYCurve->Set_YValue(k, *pDouble);
					pDouble++;
					k++;
				}
			}
			else
			{
				DoSimpleMsg("No active XYCurve Object found.", 51015);
			}
		}
		*mySize = k - 1;
		break;
	default:
		*myType = 4;				//String
		myStrArray.resize(0);
		WriteStr2Array("Error, parameter not recognized");
		WriteStr2Array(Char0());
		*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
		*mySize = myStrArray.size();
		break;
	}
}

//************************Structure type properties*******************************

int __stdcall CtrlQueueI(int mode, int arg)
{
	int Result = 0;

	switch (mode)
	{
		case 0:				// CtrlQueue.ClearQueue
		{
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
				ActiveCircuit[ActiveActor]->ControlQueue.Clear();
		}
		break;
		case 1:				// CtrlQueue.Delete
		{
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
				ActiveCircuit[ActiveActor]->ControlQueue.Delete(arg, ActiveActor);
		}
		break;
		case 2:				// CtrlQueue.NumActions
		{
			Result = COMControlProxyObj->ActionList.size();
		}
		break;
		case 3:				// CtrlQueue.Action
		{
			auto with0 = COMControlProxyObj;
			if (arg < with0->ActionList.size())
				ActiveAction = (pAction) with0->ActionList[arg - 1];
		}
		break;
		case 4:				// CtrlQueue.ActionCode
		{
			if (ASSIGNED(ActiveAction))
				Result = ActiveAction->ActionCode;
		}
		break;
		case 5:				// CtrlQueue.DeviceHandle
		{
			if (ASSIGNED(ActiveAction))
				Result = ActiveAction->DeviceHandle;
		}
		break;
		case 6:				// CtrlQueue.Show
		{
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
				ActiveCircuit[ActiveActor]->ControlQueue.ShowQueue(DSSDirectory + "COMProxy_ControlQueue.CSV");
		}
		break;
		case 7:				// CtrlQueue.ClearActions
		{
			COMControlProxyObj->ClearActionList();
		}
		break;
		case 8:				// CtrlQueue.PopAction
		{
			Result = COMControlProxyObj->ActionList.size();
			COMControlProxyObj->PopAction();
		}
		break;
		case 9:				// CtrlQueue.Get_QueueSize
		{
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
				Result = ActiveCircuit[ActiveActor]->ControlQueue.Get_QueueSize();
		}
		break;
		case 10:				// CtrlQueue.DoAllQueue
		{
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
				ActiveCircuit[ActiveActor]->ControlQueue.DoAllActions(ActiveActor);
		}
		break;
		default:
			Result = -1;
	}
	return Result;
}

void __stdcall CtrlQueueV(int mode, uintptr_t* myPtr, int* myType, int* mySize)
{
	double	ActionCode = 0.0,
			Hour = 0.0,
			DeviceHandle = 0.0,
			Seconds = 0.0;
	double* pDbl = nullptr;
	int		i = 0,
			Qsize = 0;

	switch (mode)
	{
		case 0:				// CtrlQueue.ClearQueue
		{
			*myType = 4;	// String
			myStrArray.clear();
			Qsize = ActiveCircuit[ActiveActor]->ControlQueue.Get_QueueSize();

			if (Qsize > 0)
			{
				WriteStr2Array("Handle, Hour, Sec, ActionCode, ProxyDevRef, Device");
				WriteStr2Array(Char0());
				for (i = 0; i < Qsize; i++)
				{
					WriteStr2Array(ActiveCircuit[ActiveActor]->ControlQueue.QueueItem(i));
					WriteStr2Array(Char0());
				}
			}
			else
			{
				WriteStr2Array("No events");
				WriteStr2Array(Char0());
			}
			*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
			*mySize = myStrArray.size();
		}
		break;
		case 1:				// CtrlQueue.Push
		{
			*myType = 2;	// Double
			myStrArray.clear();
			pDbl = (double*)myPtr;
			Qsize = 0;
			if (ASSIGNED(ActiveCircuit[ActiveActor]))
			{
				try
				{
					Hour = *pDbl;
					pDbl++;
					Seconds = *pDbl;
					pDbl++;
					ActionCode = *pDbl;
					pDbl++;
					DeviceHandle = *pDbl;
					Qsize = ActiveCircuit[ActiveActor]->ControlQueue.Push(trunc(Hour), Seconds, trunc(ActionCode), trunc(DeviceHandle), COMControlProxyObj, ActiveActor);
				}
				catch (...)
				{
					Qsize = -10001;    // something went wrong
				}
			}
			*mySize = Qsize;
		}
		break;
		default:
		{
			*myType = 4;		// String
			myStrArray.clear();
			WriteStr2Array("Error, parameter not recognized");
			WriteStr2Array(Char0());
			*myPtr = (uintptr_t)(void*)&(myStrArray[0]);
			*mySize = myStrArray.size();
		}
	}
}

char* __stdcall DSSProperties(int mode, char* arg)
{
	string	Result = "";

	switch (mode)
	{
		case 0:				// DSSproperties.Name
		{
			FPropIndex = StrToInt(arg) - 1;
			if (ASSIGNED(ActiveCircuit[ActiveActor]) && (FPropIndex >= 0))
			{
				auto with0 = ((TDSSObject*)ActiveDSSObject[ActiveActor])->ParentClass;
				if (FPropIndex < with0->NumProperties)
					Result = with0->PropertyName[FPropIndex];
			}
		}
		break;
		case 1:				// DSSproperties.Description
		{
			FPropIndex = StrToInt(arg) - 1;
			if (ASSIGNED(ActiveCircuit[ActiveActor]) && (FPropIndex >= 0))
			{
				auto with0 = ((TDSSObject*)ActiveDSSObject[ActiveActor])->ParentClass;
				if (FPropIndex < with0->NumProperties)
					Result = with0->PropertyHelp[FPropIndex];
			}
		}
		break;
		case 2:				// DSSproperties.Value - read
		{
			FPropIndex = StrToInt(arg) - 1;
			if (ASSIGNED(ActiveCircuit[ActiveActor]) && (FPropIndex >= 0))
			{
				auto with0 = (TDSSObject*)ActiveDSSObject[ActiveActor];
				auto with1 = with0->ParentClass;

				if (FPropIndex < with1->NumProperties)
					Result = with0->GetPropertyValue(with1->PropertyIdxMap[FPropIndex]);
			}
		}
		break;
		case 3:				// DSSproperties.Value - write
		{
			if (ASSIGNED(ActiveCircuit[ActiveActor]) && (FPropIndex >= 0))
			{
				auto with0 = (TDSSObject*)ActiveDSSObject[ActiveActor];
				auto with1 = with0->ParentClass;

				if (FPropIndex < with1->NumProperties)
				{
					DSSExecutive[ActiveActor]->Set_Command("Edit " + with1->Class_Name + "." + with0->get_Name() + " " +
															with1->PropertyName[FPropIndex] + "=" + string(arg));
					Result = with0->GetPropertyValue(with1->PropertyIdxMap[FPropIndex]);
				}
			}
		}
		break;
		default:
			Result = "";
		break;
	}

	char* presult = new char[Result.size() + 1];
	strcpy(presult, Result.c_str());
	return  presult;
}

// Call this first
// Save a copy of these in permanent heap memory here before returning
int InitAndGetYparams(uintptr_t* hY, unsignedint* nBus, unsignedint* nNZ)
{
	int result = 0;    // indicates error
	if (ActiveCircuit[ActiveActor] == nullptr) return result;

	Yhandle = ActiveCircuit[ActiveActor]->Solution->hY;
	if (Yhandle == nullptr) 
	{
		DoSimpleMsg("Y Matrix not Built.", 222);
		return result;
	}

	*hY = (uintptr_t) Yhandle;

	FactorSparseMatrix(Yhandle);
	GetNNZ(Yhandle, &NumNZ);
	GetSize(Yhandle, &NumBuses);

	*nBus = NumBuses;
	*nNZ = NumNZ;

	result = 1;
	return result;
}

/* Returns Pointers to column and row and matrix values */
/* Call InitAndGetYparams first to factor the sparse matrix ... */
void GetCompressedYMatrix(uintptr_t hY, unsignedint nBus, unsignedint nNz, int** ColPtr, int** RowIdx, complex** cVals)
{
	// Allocate space on the heap and put the values there
	ReallocMem(YColumns, sizeof(int) * (nBus + 1));
	ReallocMem(YRows, sizeof(int) * nNz);
	ReallocMem(YValues, sizeof(YValues[0]) * nNz);
	// Fill in the memory
	GetCompressedMatrix((klusparseset_t) hY, nBus + 1, nNz, (unsignedint *) YColumns, (unsignedint*) YRows, YValues);

	// Set the pointers in the calling program to the heap variables
	*ColPtr = YColumns;
	*RowIdx = YRows;
	*cVals = YValues;
}

int SystemYChanged(int mode, int arg)
{
	int result = 0;
	switch (mode)
	{
	case 0: if (ActiveCircuit[ActiveActor]->Solution->SystemYChanged) result = 1;  // Read
	break;
	case 1: { // Write
		if (arg == 1) ActiveCircuit[ActiveActor]->Solution->SystemYChanged = true;
		else ActiveCircuit[ActiveActor]->Solution->SystemYChanged = false;
	}
	break;
	}
	return result;
}

int SolveSystem(complex** NodeV)
{
	return ActiveCircuit[ActiveActor]->Solution->SolveSystem(*NodeV, ActiveActor);
}

int UseAuxCurrents(int mode, int arg)
{
	int result = 0;
	switch (mode)
	{
	case 0: if (ActiveCircuit[ActiveActor]->Solution->UseAuxCurrents) result = 1; // Read
	break;
	case 1: { // Write
		if (arg == 1) ActiveCircuit[ActiveActor]->Solution->UseAuxCurrents = true;
		else ActiveCircuit[ActiveActor]->Solution->UseAuxCurrents = false;
	}
	break;
	}
	return result;
}

void AddInAuxCurrents(int SType)
{
	ActiveCircuit[ActiveActor]->Solution->AddInAuxCurrents(SType, ActiveActor);
}

void BuildYMatrixD(int BuildOps, int AllocateVI)
{
	bool AllocateV = false;
	if (AllocateVI == 1) AllocateV = true;
	BuildYMatrix(BuildOps, AllocateV, ActiveActor);
}

void GetPCInjCurr(void)
{
	if (ActiveCircuit[ActiveActor] != nullptr)
		ActiveCircuit[ActiveActor]->Solution->GetPCInjCurr(ActiveActor);
}

void GetSourceInjCurrents(void)
{
	if (ActiveCircuit[ActiveActor] != nullptr)
		ActiveCircuit[ActiveActor]->Solution->GetSourceInjCurrents(ActiveActor);
}

void ZeroInjCurr(void)
{
	if (ActiveCircuit[ActiveActor] != nullptr)
		ActiveCircuit[ActiveActor]->Solution->ZeroInjCurr(ActiveActor);
}

void getVpointer(complex** VvectorPtr)
{
	*VvectorPtr = ActiveCircuit[ActiveActor]->Solution->NodeV.data();
}

void getIpointer(complex** IvectorPtr)
{
	*IvectorPtr = ActiveCircuit[ActiveActor]->Solution->Currents.data();
}
