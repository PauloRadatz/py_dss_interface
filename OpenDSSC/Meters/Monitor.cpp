
#pragma hdrstop

#include "Monitor.h"

#include "Utilities.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "CktElement.h"
#include "Transformer.h"
#include "AutoTrans.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "ShowResults.h"
#include "mathutil.h"
#include "PointerList.h"
#include "TOPExport.h"
#include "Dynamics.h"
#include "Pstcalc.h"
#include "Capacitor.h"
#include "Storage.h"
#include <string>
#include <algorithm>

using namespace std;
using namespace Arraydef;
using namespace AutoTrans;
using namespace Capacitor;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Dynamics;
using namespace MeterClass;
using namespace MeterElement;
using namespace PCElement;
using namespace ParserDel;
using namespace PointerList;
using namespace Pstcalc;
using namespace Storage;
using namespace System;
using namespace TOPExport;
using namespace Transformer;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace Monitor
{

TMonitorObj::TMonitorObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TMonitorObj::TMonitorObj(String ClassName) : inherited(ClassName) {}
TMonitorObj::TMonitorObj() {}




TMonitorObj* ActiveMonitorObj = nullptr;
const int SEQUENCEMASK = 16;
const int MAGNITUDEMASK = 32;
const int POSSEQONLYMASK = 64;
const int ModeMask = 15;
const int NumPropsThisClass = 8; // Updated to 8 classes to include time constant for frequency calc
const int NumSolutionVars = 12;
char dummyRec[256];

/*--------------------------------------------------------------------------*/  // Creates superstructure for all Monitor objects

TDSSMonitor::TDSSMonitor()
{
	;
	Class_Name = "Monitor";
	DSSClassType = DSSClassType + MON_ELEMENT;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

/*--------------------------------------------------------------------------*/

TDSSMonitor::~TDSSMonitor()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TDSSMonitor::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[0] = "element";
	PropertyName[1] = "terminal";
	PropertyName[2] = "mode";
	PropertyName[3] = "action";  // buffer=clear|save
	PropertyName[4] = "residual";  // buffer=clear|save
	PropertyName[5] = "VIPolar";  // V I in mag and angle rather then re and im
	PropertyName[6] = "PPolar";  // Power in power PF rather then power and vars
	PropertyName[7] = "Tf"; // Time constant for low pass fitler (frequency Calculation)
	
	PropertyHelp[0] = "Name (Full Object name) of element to which the monitor is connected.";
	PropertyHelp[1] = "Number of the terminal of the circuit element to which the monitor is connected. "
	           "1 or 2, typically. For monitoring states, attach monitor to terminal 1.";
	PropertyHelp[2] = String("Bitmask integer designating the values the monitor is to capture: ") + CRLF
	           + "0 = Voltages, currents and frequency at designated terminal"
	           + CRLF
	           + "1 = Powers at designated terminal"
	           + CRLF
	           + "2 = Tap Position (Transformer Device only)"
	           + CRLF
	           + "3 = State Variables (PCElements only)"
	           + CRLF
	           + "4 = Flicker level and severity index (Pst) for voltages. No adders apply."
	           + CRLF
	           + "    Flicker level at simulation time step, Pst at 10-minute time step."
	           + CRLF
	           + "5 = Solution variables (Iterations, etc)."
	           + CRLF
	           + "Normally, these would be actual phasor quantities from solution."
	           + CRLF
	           + "6 = Capacitor Switching (Capacitors only)"
	           + CRLF
	           + "7 = Storage state vars (Storage device only)"
	           + CRLF
	           + "8 = All winding currents (Transformer device only)"
	           + CRLF
	           + "9 = Losses, watts and var (of monitored device)"
	           + CRLF
	           + "10 = All Winding voltages (Transformer device only)"
	           + CRLF
	           + "Normally, these would be actual phasor quantities from solution."
	           + CRLF
	           + "11 = All terminal node voltages and line currents of monitored device"
	           + CRLF
	           + "12 = All terminal node voltages LL and line currents of monitored device"
	           + CRLF
	           + "Combine mode with adders below to achieve other results for terminal quantities:"
	           + CRLF
	           + "+16 = Sequence quantities"
	           + CRLF
	           + "+32 = Magnitude only"
	           + CRLF
	           + "+64 = Positive sequence only or avg of all phases"
	           + CRLF
	           + CRLF
	           + "Mix adder to obtain desired results. For example:"
	           + CRLF
	           + "Mode=112 will save positive sequence voltage and current magnitudes only"
	           + CRLF
	           + "Mode=48 will save all sequence voltages and currents, but magnitude only.";
	PropertyHelp[3] = String("{Clear | Save | Take | Process}") + CRLF
	           + "(C)lears or (S)aves current buffer."
	           + CRLF
	           + "(T)ake action takes a sample."
	           + CRLF
	           + "(P)rocesses the data taken so far (e.g. Pst for mode 4)."
	           + CRLF
	           + CRLF
	           + "Note that monitors are automatically reset (cleared) when the Set Mode= command is issued. "
	           + "Otherwise, the user must explicitly reset all monitors (reset monitors command) or individual "
	           + "monitors with the Clear action.";
	PropertyHelp[4] = "{Yes/True | No/False} Default = No.  Include Residual cbannel (sum of all phases) for voltage and current. "
	           "Does not apply to sequence quantity modes or power modes.";
	PropertyHelp[5] = "{Yes/True | No/False} Default = YES. Report voltage and current in polar form (Mag/Angle). (default)  Otherwise, it will be real and imaginary.";
	PropertyHelp[6] = "{Yes/True | No/False} Default = YES. Report power in Apparent power, S, in polar form (Mag/Angle).(default)  Otherwise, is P and Q";
	PropertyHelp[7] = "Time constant for low pass filter during frequency calculations (sec). Default is Tf = 0.05 sec";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TDSSMonitor::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new Monitor and add it to Monitor class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TMonitorObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TDSSMonitor::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int recalc = 0;

  // continue parsing with contents of Parser
  // continue parsing with contents of Parser
	ActiveMonitorObj = ((TMonitorObj*) ElementList.Get_Active());
	ActiveCircuit[ActorID]->Set_ActiveCktElement((TDSSCktElement*) ActiveMonitorObj);
	result = 0;
	recalc = 0;
	/*# with ActiveMonitorObj do */
	{
		auto with0 = ActiveMonitorObj;
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while(Param.size() > 0)
		{
			if(ParamName.size() == 0)
				++ParamPointer;
			else
				ParamPointer = CommandList.Getcommand(ParamName);
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))
				with0->Set_PropertyValue(ParamPointer,Param);
			++recalc;
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 661);
				break;
				case 	1:
				{
					with0->ElementName = ConstructElemName(LowerCase(Param));   // subtitute @var values if any
					with0->Set_PropertyValue(1,with0->ElementName);
				}
				break;
				case 	2:
				with0->MeteredTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->Mode = Parser[ActorID]->MakeInteger_();
				break;
				case 	4:
				{
					Param = LowerCase(Param);
					switch(Param[0])
					{
						case 	L's':
						with0->Save();
						break;
						case 	L'c': case L'r':
						with0->ResetIt(ActorID);
						break;
						case 	L't':
						with0->TakeSample(ActorID);
						break;
						case 	L'p':
						{
							with0->PostProcess(ActorID);
							--recalc;
						}
						break;
						default:
						  ;
						break;
					}
				}
				break;  // buffer
				case 	5:
				with0->IncludeResidual = InterpretYesNo(Param);
				break;
				case 	6:
				with0->VIpolar = InterpretYesNo(Param);
				break;
				case 	7:
				with0->Ppolar = InterpretYesNo(Param);
				break;
				case	8: // BLS
				with0->Tf = Parser[ActorID]->MakeDouble_();
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveMonitorObj, ParamPointer - NumPropsThisClass);
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		if(recalc > 0)
			with0->RecalcElementData(ActorID);
	}
	return result;
}

/*--------------------------------------------------------------------------*/  // Force all monitors in the circuit to reset

void TDSSMonitor::ResetAll(int ActorID)
{
	TMonitorObj* Mon = nullptr;
	Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_First());
	while(Mon != nullptr)
	{
		if( ( (TDSSCktElement*) Mon )->Get_Enabled())
			Mon->ResetIt(ActorID);
		Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_Next());
	}
}

/*--------------------------------------------------------------------------*/  // Force all monitors in the circuit to take a sample

void TDSSMonitor::SampleAll(int ActorID)
{
	TMonitorObj* Mon = nullptr;
// sample all monitors except mode 5 monitors
	Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_First());
	while(Mon != nullptr)
	{
		if(((TDSSCktElement*)Mon)->Get_Enabled())
		{
			if(Mon->Mode != 5)
				Mon->TakeSample(ActorID);
		}
		Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_Next());
	}
}

/*--------------------------------------------------------------------------*/  // Force all mode=5 monitors in the circuit to take a sample

void TDSSMonitor::SampleAllMode5(int ActorID)
{
	TMonitorObj* Mon = nullptr;
// sample all Mode 5 monitors except monitors
	Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_First());
	while(Mon != nullptr)
	{
		if(((TDSSCktElement*)Mon)->Get_Enabled())
		{
			if(Mon->Mode == 5)
				Mon->TakeSample(ActorID);
		}
		Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_Next());
	}
}

/*--------------------------------------------------------------------------*/

void TDSSMonitor::PostProcessAll(int ActorID)
{
	TMonitorObj* Mon = nullptr;
	Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_First());
	while(Mon != nullptr)
	{
		if(((TDSSCktElement*)Mon)->Get_Enabled())
			Mon->PostProcess(ActorID);
		Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_Next());
	}
}

/*--------------------------------------------------------------------------*/     // Force all monitors in the circuit to save their buffers to disk

void TDSSMonitor::SaveAll(int ActorID)
{
	TMonitorObj* Mon = nullptr;
	Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_First());
	while(Mon != nullptr)
	{
		if(((TDSSCktElement*)Mon)->Get_Enabled())
			Mon->Save();
		Mon = ((TMonitorObj*) ActiveCircuit[ActorID]->Monitors.Get_Next());
	}
}

/*--------------------------------------------------------------------------*/

int TDSSMonitor::MakeLike(const String MonitorName)
{
	int result = 0;
	TMonitorObj* OtherMonitor = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Monitor name in the present collection*/
	OtherMonitor = ((TMonitorObj*) Find(MonitorName));
	if(OtherMonitor != nullptr)
		/*# with ActiveMonitorObj do */
		{
			auto with0 = ActiveMonitorObj;
			int stop = 0;
			(with0)->Set_NPhases((OtherMonitor)->Fnphases);
			(with0)->Set_Nconds((OtherMonitor)->Fnconds); // Force Reallocation of terminal stuff
			with0->BufferSize = OtherMonitor->BufferSize;
			with0->ElementName = OtherMonitor->ElementName;
			with0->MeteredElement = OtherMonitor->MeteredElement;  // Pointer to target circuit element
			with0->MeteredTerminal = OtherMonitor->MeteredTerminal;
			with0->Mode = OtherMonitor->Mode;
			with0->IncludeResidual = OtherMonitor->IncludeResidual;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherMonitor->Get_PropertyValue(i));
			}
			with0->BaseFrequency = OtherMonitor->BaseFrequency;
		}
	else
		DoSimpleMsg(String("Error in Monitor MakeLike: \"") + MonitorName
	           + "\" Not Found.", 662);
	return result;
}

/*--------------------------------------------------------------------------*/

int TDSSMonitor::Init(int Handle, int ActorID)
{
	int result = 0;
	TMonitorObj* Mon = nullptr;
	result = 0;
	if(Handle > 0)
	{
		Mon = ((TMonitorObj*) ElementList.Get(Handle));
		Mon->ResetIt(ActorID);
	}
	else
  // Do 'em all
	{
		Mon = ((TMonitorObj*) ElementList.Get_First());
		while(Mon != nullptr)
		{
			Mon->ResetIt(ActorID);
			Mon = ((TMonitorObj*) ElementList.Get_Next());
		}
	}
	return result;
}


/*==========================================================================*/
/*                    TMonitorObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TMonitorObj::TMonitorObj(TDSSClass* ParClass, const String MonitorName)
 : inherited(ParClass),
			BufferSize(1024),
			Hour(0),
			Sec(0.0),
			BufPtr(0),
			NumTransformerCurrents(0),
			NumWindingVoltages(0),
			NumStateVars(0),
			IncludeResidual(false),
			VIpolar(false),
			Ppolar(false),
			FileSignature(0),
			FileVersion(0),
			BaseFrequency(0.0),
			IsFileOpen(false),
			ValidMonitor(false),
			IsProcessed(false),
			Mode(0),
			SampleCount(0),
			myHeaderSize(0),
	        stepCount(0),
			Tf(0.05),
			s(0)
{
	Set_Name(LowerCase(MonitorName));
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	MonitorStream.clear(),
	MonBuffer.clear();
     /*Current Buffer has to be big enough to hold all terminals*/
	CurrentBuffer.clear();
	VoltageBuffer.clear();
	StateBuffer.clear();
	FlickerBuffer.clear();
	SolutionBuffer.clear();
	WdgCurrentsBuffer.clear();
	WdgVoltagesBuffer.clear();
	PhsVoltagesBuffer.clear();
	NumTransformerCurrents = 0;
	BaseFrequency = 60.0;
	Hour = 0;
	Sec = 0.0;
	Mode = 0;  // Standard Mode: V & I, complex values
	MonBuffer.resize( BufferSize );
	BufPtr = 0;
	ElementName = ((TDSSObject*) ActiveCircuit[ActiveActor]->CktElements.Get(1))->get_Name(); // Default to first circuit element (source)
	MeteredElement = nullptr;
	BufferFile = "";
	MonitorStream.clear(); // Create memory stream
	IsFileOpen = false;
	MeteredTerminal = 1;
	IncludeResidual = false;
	VIpolar = true;
	Ppolar = true;
	FileSignature = 43756;
	FileVersion = 1;
	SampleCount = 0;
	IsProcessed = false;
	DSSObjType = ParClass->DSSClassType; //MON_ELEMENT;
	stepCount = 0;                                      
	Tf = 0.05;			                      
	s = 0;
	InitPropertyValues(0);
}

TMonitorObj::~TMonitorObj()
{
	MonitorStream.clear();
	ElementName = "";
	BufferFile = "";
	MonBuffer.clear();
	StateBuffer.clear();
	CurrentBuffer.clear();
	VoltageBuffer.clear();
	FlickerBuffer.clear();
	SolutionBuffer.clear();
	WdgVoltagesBuffer.clear();
	WdgCurrentsBuffer.clear();
	PhsVoltagesBuffer.clear();
	// inherited::Destroy();
}



/*--------------------------------------------------------------------------*/

void ConvertBlanks(String& s)
{
	int BlankPos = 0;
     /* Convert spaces to Underscores */
	BlankPos = Pos(" ", s);
	while(BlankPos > 0)
	{
		s[BlankPos] = L'_';
		BlankPos = Pos(" ", s);
	}
}

/*--------------------------------------------------------------------------*/

void TMonitorObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	int i = 0;
	ValidMonitor = false;
	DevIndex = GetCktElementIndex(ElementName);                   // Global function
	if(DevIndex > 0)                                       // Monitored element must already exist
	{
		MeteredElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		switch((Mode & ModeMask))
		{
			case 	2:
			 case 8:
			 case 10:                                                // Must be transformer
			{
				if((MeteredElement->DSSObjType & CLASSMASK) != XFMR_ELEMENT)
				{
					if((MeteredElement->DSSObjType & CLASSMASK) != AUTOTRANS_ELEMENT)
					{
						DoSimpleMsg(MeteredElement->get_Name() + " is not a transformer!", 663);
						return;
					}
				}
			}
			break;                                                // Must be PCElement
			case 	3:
			{
				if((MeteredElement->DSSObjType & BaseClassMask) != PC_ELEMENT)
				{
					DoSimpleMsg(MeteredElement->get_Name() + " must be a power conversion element (Load or Generator)!", 664);
					return;
				}
			}
			break;                                                // Checking Caps Tap
			case 	6:
			{
				if((MeteredElement->DSSObjType & CLASSMASK) != CAP_ELEMENT)
				{
					DoSimpleMsg(MeteredElement->get_Name() + " is not a capacitor!", 2016001);
					return;
				}
			}
			break;                                                // Checking if the element is a storage device
			case 	7:
			{
				if((MeteredElement->DSSObjType & CLASSMASK) != STORAGE_ELEMENT) /*and ((MeteredElement.DSSObjType And CLASSMASK) <> STORAGE2_ELEMENT)*/
				{
					DoSimpleMsg(MeteredElement->get_Name() + " is not a storage device!", 2016002);
					return;
				}
			}
			break;
			default:
			  ;
			break;
		}
		if(MeteredTerminal > MeteredElement->Get_NTerms())
		{
			DoErrorMsg(String("Monitor: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Respecify terminal no.", 665);
		}
		else
		{
			Set_NPhases(MeteredElement->Get_NPhases());
			Set_Nconds(MeteredElement->Get_NConds());
               // Sets name of i-th terminal's connected bus in monitor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
			SetBus(1, MeteredElement->GetBus(MeteredTerminal));
               // Make a name for the Buffer File
			 /*ActiveCircuit[ActiveActor].CurrentDirectory + */
			BufferFile = CircuitName_[ActorID] + "Mon_" + get_Name() + ".mon";
                 // removed 10/19/99 ConvertBlanks(BufferFile); // turn blanks into '_'

                 /*Allocate Buffers*/
			switch((Mode & ModeMask))
			{
				case 	3:
				{
					if ( !ASSIGNED( ( (TPCElement*)MeteredElement )->DynamicEqObj ) )
						NumStateVars = ((TPCElement*) MeteredElement)->NumVariables();
					else
						NumStateVars = ((TPCElement*)MeteredElement)->DynamicEqObj->get_FNumVars() * ((TPCElement*)MeteredElement)->DynamicEqVals[0].size();
					StateBuffer.resize(NumStateVars);
				}
				break;
				case 	4:
				{
					FlickerBuffer.resize(Get_NPhases());
				}
				break;
				case 	5:
				{
					SolutionBuffer.resize(NumSolutionVars);
				}
				break;
				case 	8:
				{
					if((MeteredElement->DSSObjType & CLASSMASK) == AUTOTRANS_ELEMENT)
						/*# with TAutoTransObj(MeteredElement) do */
						{
							auto with0 = ((TAutoTransObj*) MeteredElement);
							NumTransformerCurrents = 2 * with0->get_NumWindings() * ( with0 )->Get_NPhases();
						}
					else
						/*# with TTransfObj(MeteredElement) do */
						{
							auto with1 = ((TTransfObj*) MeteredElement);
							NumTransformerCurrents = 2 * with1->get_NumWindings() * with1->Get_NPhases();
						}
					WdgCurrentsBuffer.resize(NumTransformerCurrents);
				}
				break;
				case 	10:
				{
					if((MeteredElement->DSSObjType & CLASSMASK) == AUTOTRANS_ELEMENT)
						/*# with TAutoTransObj(MeteredElement) do */
						{
							auto with2 = ((TAutoTransObj*) MeteredElement);
							NumWindingVoltages = with2->get_NumWindings() * ( with2 )->Get_NPhases();
						}
					else
						/*# with TTransfObj(MeteredElement) do */
						{
							auto with3 = ((TTransfObj*) MeteredElement);
							NumWindingVoltages = with3->get_NumWindings() * with3->Get_NPhases();
						}
					WdgVoltagesBuffer.resize(NumWindingVoltages);  // total all phases, all windings
					PhsVoltagesBuffer.resize(Get_NPhases());
				}
				break;
				case 	11:
				{
					CurrentBuffer.resize(MeteredElement->Yorder + 1);
					VoltageBuffer.resize(MeteredElement->Yorder + 1);
				}
				break;
				case 	12:
				{
					CurrentBuffer.resize(MeteredElement->Yorder + 1);
					VoltageBuffer.resize(MeteredElement->Yorder + 1);
				}
				break;
				default:
				CurrentBuffer.resize(MeteredElement->Yorder + 1);
				VoltageBuffer.resize(MeteredElement->Get_NConds() + 1);
				break;
			}
			ClearMonitorStream(ActorID);
			ValidMonitor = true;
		}
	}
	else
	{
		MeteredElement = nullptr;   // element not found
		DoErrorMsg(String("Monitor: \"") + this->get_Name() + "\"", String("Circuit Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 666);
	}
}

void TMonitorObj::MakePosSequence(int ActorID)
{
	if(MeteredElement != nullptr)
	{
		SetBus(1, MeteredElement->GetBus(MeteredTerminal));
		Set_NPhases(MeteredElement->Get_NPhases());
		Set_Nconds(MeteredElement->Get_NConds());
		switch((Mode & ModeMask))
		{
			case 	3:
			{
				NumStateVars = ((TPCElement*) MeteredElement)->NumVariables();
				StateBuffer.resize(NumStateVars);
			}
			break;
			case 	4:
			{
				FlickerBuffer.resize(Get_NPhases());
			}
			break;
			case 	5:
			{
				SolutionBuffer.resize(NumSolutionVars);
			}
			break;
			default:
			CurrentBuffer.resize(MeteredElement->Yorder);
			VoltageBuffer.resize(MeteredElement->Get_NConds());
			break;
		}
		ClearMonitorStream(ActorID);
		ValidMonitor = true;
	}
	inherited::MakePosSequence(ActorID);
}


/*--------------------------------------------------------------------------*/

void TMonitorObj::CalcYPrim(int ActorID)
{


  /*A Monitor is a zero current source; Yprim is always zero.*/
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
}

/*--------------------------------------------------------------------------
        Concatenates the given string into the header buffer
--------------------------------------------------------------------------*/

void TMonitorObj::Add2Header(AnsiString myText)
{
	int i = 0;
	int j = 0;
	int myLen = 0;
	int stop = 0;
	myLen = (int) StrBuffer.size();
	for(stop = (myText.size() - 1), i = 0; i <= stop; i++)
	{
		if(myText[i] != ((AnsiChar) 0))
			StrBuffer.push_back( myText[i] );
	}
}

/*--------------------------------------------------------------------------*/

void TMonitorObj::ClearMonitorStream(int ActorID)
{
	std::vector  <int> PhaseLoc;
	int i = 0;
	int j = 0;
	int IMax = 0;
	int NumVI = 0;
	int RecordSize = 0;
	int IMin = 0;
	bool IsPosSeq = false;
	bool IsPower = false;
	AnsiString NameOfState;
	AnsiString Str_Temp;
	try
	{
		MonitorStream.clear();
		IsProcessed = false;
		SampleCount = 0;
		IsPosSeq = false;
		StrBuffer.clear();
		if(ActiveCircuit[ActorID]->Solution->IsHarmonicModel)
			Add2Header("Freq, Harmonic, ");
		else
			Add2Header("hour, t(sec), ");
		switch((Mode & ModeMask))
		{
			case 	2:
			{
				RecordSize = 1;     // Transformer Taps
				Add2Header("Tap (pu)");
			}
			break;
			case 	3:
			{
				int stop = 0;
				RecordSize = NumStateVars;   // Statevariabes
				for(stop = NumStateVars, i = 1; i <= stop; i++)
				{
					if ( !ASSIGNED( ( ( TPCElement* )MeteredElement )->DynamicEqObj ) )
						NameOfState = AnsiString(((TPCElement*) MeteredElement)->VariableName(i) + ",");
					else
						NameOfState = AnsiString(((TPCElement*)MeteredElement)->DynamicEqObj->Get_VarName(i - 1) + ",");
					Add2Header(NameOfState.c_str());
				}
			}
			break;
			case 	4:
			{
				int stop = 0;
				RecordSize = 2 * Fnphases;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{  //AnsString and pAnsiChar replaced with AnsiString and pAnsiChar to make it compatible with Linux
					Add2Header((AnsiString(String("Flk") + IntToStr(i) + ", Pst" + IntToStr(i))).c_str());
					if(i < Fnphases)
						Add2Header(", ");
				}
			}
			break;
			case 	5:
			{
				RecordSize = NumSolutionVars;
				Add2Header("TotalIterations, ");
				Add2Header("ControlIteration, ");
				Add2Header("MaxIterations, ");
				Add2Header("MaxControlIterations, ");
				Add2Header("Converged, ");
				Add2Header("IntervalHrs, ");
				Add2Header("SolutionCount, ");
				Add2Header("Mode, ");
				Add2Header("Frequency, ");
				Add2Header("Year, ");
				Add2Header("SolveSnap_uSecs, ");
				Add2Header("TimeStep_uSecs, ");
			}
			break;
			case 	6:
			{
				int stop = 0;
				RecordSize = ((TCapacitorObj*) MeteredElement)->Get_FNumSteps();     // Capacitor Taps
				for(stop = RecordSize, i = 1; i <= stop; i++)
				{
					Str_Temp = AnsiString(String("Step_") + IntToStr(i) + ",");
					Add2Header(Str_Temp.c_str());
				}
			}
			break;
			case 	7:
			{
				RecordSize = 5;     // Storage state vars
				Add2Header(("kW output, "));
				Add2Header(("kvar output, "));
				Add2Header(("kW Stored, "));
				Add2Header(("%kW Stored, "));
				Add2Header(("State, "));
			}
			break;   // All winding Currents
			case 	8:
			{
				if((MeteredElement->DSSObjType & CLASSMASK) == AUTOTRANS_ELEMENT)
					/*# with TAutoTransObj(MeteredElement) do */
					{
						auto with0 = ((TAutoTransObj*) MeteredElement);
						int stop = 0;
						RecordSize = NumTransformerCurrents;     // Transformer Winding Currents
						for(stop = ( (TDSSCktElement*) with0 )->Get_NPhases(), i = 1; i <= stop; i++)
						{
							int stop1 = 0;
							for(stop1 = with0->get_NumWindings(), j = 1; j <= stop1; j++)
							{
								Str_Temp = AnsiString(Format("P%dW%d,Deg, ", i, j));
								Add2Header(Str_Temp.c_str());
							}
						}
					}
				else
					/*# with TTransfObj(MeteredElement) do */
					{
						auto with1 = ((TTransfObj*) MeteredElement);
						int stop = 0;
						RecordSize = NumTransformerCurrents;     // Transformer Winding Currents
						for(stop = with1->Get_NPhases(), i = 1; i <= stop; i++)
						{
							int stop1 = 0;
							for(stop1 = with1->get_NumWindings(), j = 1; j <= stop1; j++)
							{
								Str_Temp = AnsiString(Format("P%dW%d,Deg, ", i, j));
								Add2Header(Str_Temp.c_str());
							}
						}
					}
			}
			break; // watts vars of meteredElement
			case 	9:
			{
				RecordSize = 2;
				Add2Header("watts, vars");
			}
			break; // All Winding Voltages
			case 	10:
			{
				if((MeteredElement->DSSObjType & CLASSMASK) == AUTOTRANS_ELEMENT)
					/*# with TAutoTransObj(MeteredElement) do */
					{
						auto with2 = ((TAutoTransObj*) MeteredElement);
						int stop = 0;
						RecordSize = 2 * with2->get_NumWindings() * with2->Get_NPhases();     // Transformer Winding woltages
						for(stop = with2->Get_NPhases(), i = 1; i <= stop; i++)
						{
							int stop1 = 0;
							for(stop1 = with2->get_NumWindings(), j = 1; j <= stop1; j++)
							{
								Str_Temp = AnsiString(Format("P%dW%d,Deg, ", i, j));
								Add2Header(Str_Temp.c_str());
							}
						}
					}
				else
					/*# with TTransfObj(MeteredElement) do */
					{
						auto with3 = ((TTransfObj*) MeteredElement);
						int stop = 0;
						RecordSize = 2 * with3->get_NumWindings() * with3->Get_NPhases();     // Transformer Winding woltages
						for(stop = with3->Get_NPhases(), i = 1; i <= stop; i++)
						{
							int stop1 = 0;
							for(stop1 = with3->get_NumWindings(), j = 1; j <= stop1; j++)
							{
								Str_Temp = AnsiString(Format("P%dW%d,Deg, ", i, j));
								Add2Header(Str_Temp.c_str());
							}
						}
					}
			}
			break; /*All terminal voltages and currents  ******/
			case 	11:
			{
				int stop = 0;
				RecordSize = 2 * 2 * MeteredElement->Yorder;  // V and I

            /*Voltages*/
				for(stop = MeteredElement->Get_NTerms(), j = 1; j <= stop; j++)
				{
					int stop1 = 0;
					for(stop1 = MeteredElement->Get_NConds(), i = 1; i <= stop1; i++)
					{
						Str_Temp = AnsiString(Format("V%dT%d,Deg, ", i, j));
						Add2Header(Str_Temp.c_str());
					}
				}

            /*Currents*/
				for(stop = MeteredElement->Get_NTerms(), j = 1; j <= stop; j++)
				{
					int stop1 = 0;
					for(stop1 = MeteredElement->Get_NConds(), i = 1; i <= stop1; i++)
					{
						Str_Temp = AnsiString(Format("I%dT%d,Deg, ", i, j));
						Add2Header(Str_Temp.c_str());
					}
				}
			}
			break; /*All terminal voltages LL and currents  ******/
			case 	12:
			{
				int stop = 0;
				/*# with MeteredElement do */
				{
					auto with4 = MeteredElement;
					RecordSize = 2 * ((with4->Get_NPhases() * with4->Get_NTerms()) + with4->Yorder);  // V and I
					PhaseLoc.resize( with4->Get_NPhases() + 1 );
				}
            // Creates the map of phase combinations (LL)
				for(stop = MeteredElement->Get_NPhases(), j = 1; j <= stop; j++)
				{
					PhaseLoc[j - 1] = j;
				}
				PhaseLoc[PhaseLoc.size() - 1] = 1;

            /*Voltages*/
				for(stop = MeteredElement->Get_NTerms(), j = 1; j <= stop; j++)
				{
					int stop1 = 0;
					for(stop1 = MeteredElement->Get_NPhases(), i = 1; i <= stop1; i++)
					{
						Str_Temp = AnsiString(Format("V%d-%dT%d,Deg, ", PhaseLoc[i - 1], PhaseLoc[i], j));
						Add2Header(Str_Temp.c_str());
					}
				}

            /*Currents*/
				for(stop = MeteredElement->Get_NTerms(), j = 1; j <= stop; j++)
				{
					int stop1 = 0;
					for(stop1 = MeteredElement->Get_NConds(), i = 1; i <= stop1; i++)
					{
						Str_Temp = AnsiString(Format("I%dT%d,Deg, ", i, j));
						Add2Header(Str_Temp.c_str());
					}
				}
			}
			break;
			default:
			if(((Mode & SEQUENCEMASK) > 0) && (Fnphases == 3))  // Convert to Symmetrical components
			{
				IsPosSeq = true;
				NumVI = 3;
			}
			else
			{
				NumVI = Fnconds;
			}
          // Convert Voltage Buffer to power kW, kvar
			if((Mode & ModeMask) == 1)
				IsPower = true;
			else
				IsPower = false;
			switch((Mode & (MAGNITUDEMASK + POSSEQONLYMASK)))
			{
				case 	32: // Save Magnitudes only
				{
					int stop = 0;
					RecordSize = 0;
					for(stop = NumVI, i = 1; i <= stop; i++)
					{
						RecordSize += 1;
					}
					if(!IsPower)
					{
						int stop = 0;
						for(stop = NumVI, i = 1; i <= stop; i++)
						{
							RecordSize += 1;
						}
						if(IncludeResidual)
							RecordSize += 2;
						for(stop = NumVI, i = 1; i <= stop; i++)
						{
							Add2Header(AnsiString(Format("|V|%d (volts)", i)));
							Add2Header(", ");
						}
						if(IncludeResidual)
						{
							Add2Header("|VN| (volts)");
							Add2Header(", ");
						}
						for(stop = NumVI, i = 1; i <= stop; i++)
						{
							Add2Header((AnsiString(String("|I|") + IntToStr(i) + " (amps)")));
							if(i < NumVI)
								Add2Header(", ");
						}
						if(IncludeResidual)
						{
							Add2Header(",|IN| (amps)");
						}
						RecordSize += 1;
						Add2Header(", Freq (Hz)"); // BLS
					}
					else
  // Power
					{
						int stop = 0;
						for(stop = NumVI, i = 1; i <= stop; i++)
						{
							if(Ppolar)
								Add2Header((AnsiString(String("S") + IntToStr(i) + " (kVA)")));
							else
								Add2Header((AnsiString(String("P") + IntToStr(i) + " (kW)")));
							if(i < NumVI)
								Add2Header(", ");
						}
					}
				}
				break; // Save Pos Seq or Total of all Phases or Total power (Complex)
				case 	64:
				{
					RecordSize = 2;
					if(!IsPower)
					{
						RecordSize = RecordSize + 3;
						if(VIpolar)
							Add2Header("V1, V1ang, I1, I1ang, Freq (Hz)"); // BLS
						else
							Add2Header("V1.re, V1.im, I1.re, I1.im, Freq (Hz)"); // BLS
					}
					else
					{
						if(Ppolar)
							Add2Header("S1 (kVA), Ang ");
						else
							Add2Header("P1 (kW), Q1 (kvar)");
					}
				}
				break;  // Save Pos Seq or Aver magnitude of all Phases of total kVA (Magnitude)
				case 	96:
				{
					RecordSize = 1;
					if(!IsPower)
					{
						RecordSize = RecordSize + 2;
						Add2Header("V, I, Freq (Hz) "); //BLS
					}
					else
  // Power
					{
						if(Ppolar)
							Add2Header("S1 (kVA)");
						else
							Add2Header("P1 (kW)");
					}
				}
				break; // save  V and I in mag and angle or complex kW, kvar
				default:
				RecordSize = NumVI * 2;
				if(!IsPower)
				{
					int stop = 0;
					if(IsPosSeq)
					{
						IMin = 0;
						IMax = NumVI - 1;
					}
					else
					{
						IMin = 1;
						IMax = NumVI;
					}
					RecordSize = RecordSize + NumVI * 2;
					if(IncludeResidual)
						RecordSize += 4;
					for(stop = IMax, i = IMin; i <= stop; i++)
					{
						if(VIpolar)
							Add2Header((AnsiString(String("V") + IntToStr(i) + ", VAngle" + IntToStr(i))));
						else
							Add2Header((AnsiString(String("V") + IntToStr(i) + ".re, V" + IntToStr(i) + ".im")));
						Add2Header(", ");
					}
					if(IncludeResidual)
					{
						if(VIpolar)
							Add2Header("VN, VNAngle");
						else
							Add2Header("VN.re, VN.im");
						Add2Header(", ");
					}
					for(stop = IMax, i = IMin; i <= stop; i++)
					{
						if(VIpolar)
							Add2Header((AnsiString(String("I") + IntToStr(i) + ", IAngle" + IntToStr(i))));
						else
							Add2Header((AnsiString(String("I") + IntToStr(i) + ".re, I" + IntToStr(i) + ".im")));
						if(i < NumVI)
							Add2Header(", ");
					}
					if(IncludeResidual)
					{
						if(VIpolar)
							Add2Header(", IN, INAngle");
						else
							Add2Header(", IN.re, IN.im");
					}
					RecordSize = RecordSize + 1;
					Add2Header(", Freq (Hz)"); // BLS
				}
				else
				{
					int stop = 0;
					if(IsPosSeq)
					{
						IMin = 0;
						IMax = NumVI - 1;
					}
					else
					{
						IMin = 1;
						IMax = NumVI;
					}
					for(stop = IMax, i = IMin; i <= stop; i++)
					{
						if(Ppolar)
							Add2Header((AnsiString(String("S") + IntToStr(i) + " (kVA), Ang" + IntToStr(i))));
						else
							Add2Header((AnsiString(String("P") + IntToStr(i) + " (kW), Q" + IntToStr(i) + " (kvar)")));
						if(i < NumVI)
							Add2Header(", ");
					}
				}
				break;
			}
			break;
		}  /*CASE*/


     // RecordSize is the number of singles in the sample (after the hour and sec)

     // Write Header to Monitor Stream
     // Write ID so we know it is a DSS Monitor file and which version in case we
     // change it down the road
     // Adds NULL character at the end of the header to note the end of the string
		StrBuffer.push_back((AnsiChar) 0);
		myHeaderSize = (int) StrBuffer.size();    // stores the size of the header for further use    
		/*# with MonitorStream do */
		{
			auto& with5 = MonitorStream;
			with5.Write(&FileSignature);
			with5.Write(&FileVersion);
			with5.Write(&RecordSize);
			with5.Write(&Mode);
			with5.Write(dummyRec);       // adds the empty dummy record to avoid
                                                                // killing apps relying on this space
		}

/*    So the file now looks like: (update 05-18-2021)
       FileSignature (4 bytes)    32-bit Integers
       FileVersion   (4)
       RecordSize    (4)
       Mode          (4)
       String        (256) - > this is empty now
      
       hr   (4)       all singles
       Sec  (4)
       Sample  (4*RecordSize)
       ...

 */
	}
	catch (std::exception &e)
	{
		DoErrorMsg("Cannot open Monitor file.", (std::string) e.what(), String("Monitor: \"") + get_Name() + "\"", 670);
	}
}


/*--------------------------------------------------------------------------*/

void TMonitorObj::OpenMonitorStream()
{
	if(!IsFileOpen)
	{
		MonitorStream.end();    // Positioned at End of Stream
		IsFileOpen = true;
	}
}

/*--------------------------------------------------------------------------*/

void TMonitorObj::CloseMonitorStream(int ActorID)
{
	try
	{
		if(IsFileOpen)  // only close open files
		{
			PostProcess(ActorID);
			MonitorStream.begin();   // just move stream position to the beginning
			IsFileOpen = false;
		}
	}
	catch (std::exception &e)
	{
		DoErrorMsg("Cannot close Monitor stream.", (std::string) e.what(), String("Monitor: \"") + get_Name() + "\"", 671);
	}
}

/*--------------------------------------------------------------------------*/

// Saves present buffer to monitor file, resets bufferptrs and continues

void TMonitorObj::Save()
{
	if(!IsFileOpen)
		OpenMonitorStream(); // Position to end of stream
		
     /*Write present monitor buffer to monitorstream*/
	
	MonitorStream.Write(&MonBuffer, BufPtr);
	BufPtr = 0; // reset Buffer for next
}



/*--------------------------------------------------------------------------*/

void TMonitorObj::ResetIt(int ActorID)
{
	BufPtr = 0;
	ClearMonitorStream(ActorID);
}

/*--------------------------------------------------------------------------*/

void TMonitorObj::PostProcess(int ActorID)
{
	if(IsProcessed == false)
	{
		if((Mode == 4) && ( ( MonitorStream.Position() ) > 0))
			DoFlickerCalculations(ActorID);
	}
	IsProcessed = true;
}

/*--------------------------------------------------------------------------*/

void TMonitorObj::TakeSample(int ActorID)
{
	double	dHour = 0.0,
			dSum = 0.0;
	bool	IsPower = false,
			IsSequence = false;
	int		BuffInit = 0,
			BuffEnd = 0,
			i = 0,
			j = 0,
			k = 0,
			myRefIdx = 0,
			CalcEnd = 0,
			NumVI = 0,
			Offset = 0;
	double	Freq = DefaultBaseFreq; // BLS
	complex	VangOld = CZero, // BLS
			VangNew = CZero, // BLS
			ResidualCurr = CZero,
			ResidualVolt = CZero,
			Sum = CZero,
			CplxLosses = CZero;
	complex V012[4/*# range 1..3*/] = { CZero, CZero, CZero, CZero };
	complex I012[4/*# range 1..3*/] = { CZero, CZero, CZero, CZero };

	stepCount = stepCount + 1; // BLS - used for the frequency meter

	if(!(ValidMonitor && Get_Enabled()))
		return;
	++SampleCount;
	Hour = ActiveCircuit[ActorID]->Solution->DynaVars.intHour;
	Sec = ActiveCircuit[ActorID]->Solution->DynaVars.T;
	simdt = ActiveCircuit[ActorID]->Solution->DynaVars.h;
	Offset = (MeteredTerminal - 1) * MeteredElement->Get_NConds();   // Used to index the CurrentBuffer array

   //Save time unless Harmonics mode and then save Frequency and Harmonic
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(with0->IsHarmonicModel)
		{
			double myFreq = with0->get_FFrequency();
			AddDblsToBuffer((pDoubleArray) &myFreq, 1);  // put freq in hour slot as a double
			double myHarm = with0->Harmonic;
			AddDblsToBuffer((pDoubleArray) &myHarm, 1);  // stick harmonic in time slot in buffer
		}
		else
		{
			dHour = (double) Hour;      // convert to double
			AddDblsToBuffer((pDoubleArray)&dHour, 1);  // put hours in buffer as a double
			AddDblsToBuffer((pDoubleArray)&Sec, 1);  // stick time in sec in buffer
		}
	}
	switch((Mode & ModeMask))
	{
		case 	0:
		 case 1:       // Voltage, current. Powers


            // MeteredElement.GetCurrents(CurrentBuffer);
            // To save some time, call ComputeITerminal
		{
			int stop = 0;
			MeteredElement->ComputeIterminal(ActorID);   // only does calc if needed (This calculates all 
			for(stop = MeteredElement->Yorder, i = 1; i <= stop; i++)
			{
				(CurrentBuffer)[i - 1] = (MeteredElement->Iterminal)[i - 1];
			}
			try
			{
				int stop = 0;
				for(stop = Fnconds, i = 1; i <= stop; i++)
				{
                // NodeRef is set by the main Circuit object
                // It is the index of the terminal into the system node list
					(VoltageBuffer)[i - 1] = ActiveCircuit[ActorID]->Solution->NodeV[NodeRef[i - 1]];
				}
			}
			catch (std::exception &e)
			{
				DoSimpleMsg( (std::string) e.what()
	           + CRLF
	           + "NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.", 672);
			}
		}
		break;     // Monitor Transformer Tap Position
		case 	2:
		{
			if((MeteredElement->DSSObjType & CLASSMASK) == AUTOTRANS_ELEMENT)
				/*# with TAutoTransObj(MeteredElement) do */
				{
					auto with1 = ((TAutoTransObj*) MeteredElement);
					AddDblToBuffer(with1->Get_PresentTap(MeteredTerminal, ActorID));
				}
			else
				/*# with TTransfObj(MeteredElement) do */
				{
					auto with2 = ((TTransfObj*) MeteredElement);
					AddDblToBuffer(with2->Get_PresentTap(MeteredTerminal,ActorID));
				}
			return;
		}   // Pick up device state variables
		case 	3:
		{
			((TPCElement*) MeteredElement)->GetAllVariables(&(StateBuffer[0]));
			AddDblsToBuffer(&(StateBuffer[0]), NumStateVars);
			return;
		}   // RMS phase voltages for flicker evaluation
		case 	4:
		{
			try
			{
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					(FlickerBuffer)[i - 1] = ActiveCircuit[ActorID]->Solution->NodeV[(NodeRef)[i - 1]];
				}
			}
			catch (std::exception &e)
			{
				DoSimpleMsg((std::string) e.what()
	           + CRLF
	           + "NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.", 672);
			}
		}
		break;
            /* Capture Solution Variables */
		case 	5:
		{
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with3 = ActiveCircuit[ActorID]->Solution;
				SolutionBuffer.push_back(static_cast<double>(with3->Iteration));
				SolutionBuffer.push_back(static_cast<double>(with3->ControlIteration));
				SolutionBuffer.push_back(static_cast<double>(with3->MaxIterations));
				SolutionBuffer.push_back(static_cast<double>(with3->MaxControlIterations));
				if(with3->ConvergedFlag)
					SolutionBuffer.push_back(1);
				else
					SolutionBuffer.push_back(0);
				SolutionBuffer.push_back(static_cast<double>(with3->IntervalHrs));
				SolutionBuffer.push_back(static_cast<double>(with3->SolutionCount));
				SolutionBuffer.push_back(static_cast<double>(with3->Get_SolMode()));
				SolutionBuffer.push_back(with3->get_FFrequency());
				SolutionBuffer.push_back(static_cast<double>(with3->get_Fyear()));
				SolutionBuffer.push_back(with3->get_Solve_Time_Elapsed());
				SolutionBuffer.push_back(with3->get_Step_Time_Elapsed());
			}
		}
		break;     // Monitor Capacitor State
		case 	6:
		{
			/*# with TCapacitorObj(MeteredElement) do */
			{
				auto with4 = ((TCapacitorObj*) MeteredElement);
				int stop = 0;
				for(stop = with4->Get_FNumSteps(), i = 1; i <= stop; i++)
				{
					AddDblToBuffer((double) with4->get_States(i,ActorID));
				}
			}
			return;
		}     // Monitor Storage Device state variables
		case 	7:
		{
			if((MeteredElement->DSSObjType & CLASSMASK) == STORAGE_ELEMENT)  // Storage Element
			{
				/*# with TStorageObj(MeteredElement) do */
				{
					auto with5 = ((TStorageObj*) MeteredElement);
					AddDblToBuffer(with5->Get_PresentkW());
					AddDblToBuffer(with5->Get_Presentkvar());
					AddDblToBuffer(with5->StorageVars.kWhStored);
					AddDblToBuffer((double((with5->StorageVars.kWhStored)) / (with5->StorageVars.kWhRating)) * 100.0L);
					AddDblToBuffer((double) with5->get_fState());
				}
              /*End
              Else if (MeteredElement.DSSObjType And CLASSMASK) = STORAGE2_ELEMENT Then Begin   // Storage2 Element
                With TStorageObj(MeteredElement) Do Begin
                  AddDblToBuffer(Get_PresentkW());
                  AddDblToBuffer(Get_Presentkvar());
                  AddDblToBuffer(StorageVars.kWhStored);
                  AddDblToBuffer(((StorageVars.kWhStored)/(StorageVars.kWhRating))*100);
                  AddDblToBuffer(get_fState());
                End; */
			}
			return;
		}   // Winding Currents
              // Get all currents in each end of each winding
		case 	8:
		{
			if((MeteredElement->DSSObjType & CLASSMASK) == AUTOTRANS_ELEMENT)
				/*# with TAutoTransObj(MeteredElement) do */
				{
					auto with6 = ((TAutoTransObj*) MeteredElement);
					int stop = 0;
					with6->GetAllWindingCurrents(&(WdgCurrentsBuffer[0]), ActorID);
					ConvertComplexArrayToPolar(&(WdgCurrentsBuffer[0]), NumTransformerCurrents);
                  // Put every other Current into buffer
                  // Current magnitude is same in each end
					k = 1;
					for(stop = ( (TDSSCktElement*) with6 )->Get_NPhases() * with6->get_NumWindings(), i = 1; i <= stop; i++)
					{
						AddDblsToBuffer((pDoubleArray)&(WdgCurrentsBuffer)[k - 1].re, 2);  // Add Mag, Angle
						k = k + 2;
					}
				}
			else
				/*# with TTransfObj(MeteredElement) do */
				{
					auto with7 = ((TTransfObj*) MeteredElement);
					int stop = 0;
					with7->GetAllWindingCurrents(&(WdgCurrentsBuffer[0]), ActorID);
					ConvertComplexArrayToPolar(&(WdgCurrentsBuffer[0]), NumTransformerCurrents);
                  // Put every other Current into buffer
                  // Current magnitude is same in each end
					k = 1;
					for(stop = with7->Get_NPhases() * with7->get_NumWindings(), i = 1; i <= stop; i++)
					{
						AddDblsToBuffer((pDoubleArray)&(WdgCurrentsBuffer)[k - 1].re, 2);  // Add Mag, Angle
						k = k + 2;
					}
                 // AddDblsToBuffer(@WdgCurrentsBuffer^[1].re, NumTransformerCurrents);
				}
			return;
		}  // losses
		case 	9:
		{
			CplxLosses = MeteredElement->Get_Losses(ActorID);
			AddDblToBuffer(CplxLosses.re);
			AddDblToBuffer(CplxLosses.im);
			return;
		}   // Winding Voltages
              // Get all Voltages across each winding and put into buffer
		case 	10:
		{
			if((MeteredElement->DSSObjType & CLASSMASK) == AUTOTRANS_ELEMENT)
				/*# with TAutoTransObj(MeteredElement) do */
				{
					auto with8 = ((TAutoTransObj*) MeteredElement);
					int stop = 0;
					for(stop = with8->get_NumWindings(), i = 1; i <= stop; i++)
					{
						int stop1 = 0;
						with8->GetAutoWindingVoltages(i, &(PhsVoltagesBuffer[0]), ActorID);
						for(stop1 = ( (TDSSCktElement*) with8 )->Get_NPhases(), j = 1; j <= stop1; j++)
						{
							(WdgVoltagesBuffer)[i + (j - 1) * with8->get_NumWindings() - 1] = (PhsVoltagesBuffer)[j - 1];
						}
					}
					ConvertComplexArrayToPolar(&(WdgVoltagesBuffer[0]), NumWindingVoltages);
                  /*Put winding Voltages into Monitor*/
					AddDblsToBuffer((pDoubleArray)&(WdgVoltagesBuffer)[1 - 1].re, 2 * NumWindingVoltages);  // Add Mag, Angle each winding
				}
			else
				/*# with TTransfObj(MeteredElement) do */
				{
					auto with9 = ((TTransfObj*) MeteredElement);
					int stop = 0;
					for(stop = with9->get_NumWindings(), i = 1; i <= stop; i++)
					{
						int stop1 = 0;
						with9->GetWindingVoltages(i, &(PhsVoltagesBuffer[0]), ActorID);
						for(stop1 = with9->Get_NPhases(), j = 1; j <= stop1; j++)
						{
							(WdgVoltagesBuffer)[i + (j - 1) * with9->get_NumWindings() - 1] = (PhsVoltagesBuffer)[j - 1];
						}
					}
					ConvertComplexArrayToPolar(&(WdgVoltagesBuffer[0]), NumWindingVoltages);
                  /*Put winding Voltages into Monitor*/
					AddDblsToBuffer((pDoubleArray)&(WdgVoltagesBuffer)[1 - 1].re, 2 * NumWindingVoltages);  // Add Mag, Angle each winding
				}
			return;
		}    /*Get all terminal voltages and currents of this device*/

            /*Get All node voltages at all terminals*/
		case 	11:
		{
			int stop = 0;
			MeteredElement->ComputeVterminal(ActorID);
			for(stop = MeteredElement->Yorder, i = 1; i <= stop; i++)
			{
				(VoltageBuffer)[i - 1] = (MeteredElement->Vterminal)[i - 1];
			}
			ConvertComplexArrayToPolar(&(VoltageBuffer[0]), MeteredElement->Yorder);
            /*Put Terminal Voltages into Monitor*/
			AddDblsToBuffer((pDoubleArray)&(VoltageBuffer)[1 - 1].re, 2 * MeteredElement->Yorder);

            /*Get all terminsl currents*/
			MeteredElement->ComputeIterminal(ActorID);   // only does calc if needed
			for(stop = MeteredElement->Yorder, i = 1; i <= stop; i++)
			{
				(CurrentBuffer)[i - 1] = (MeteredElement->Iterminal)[i - 1];
			}
			ConvertComplexArrayToPolar(&(CurrentBuffer[0]), MeteredElement->Yorder);
            /*Put Terminal currents into Monitor*/
			AddDblsToBuffer((pDoubleArray)&(CurrentBuffer)[1 - 1].re, 2 * MeteredElement->Yorder);
			return;
		}    /*Get all terminal voltages LL and currents of this device - 05192021*/
		case 	12:
		{
			/*# with MeteredElement do */
			{
				auto with10 = MeteredElement;
              /*Get All node voltages at all terminals*/
				int stop = 0;
				with10->ComputeVterminal(ActorID);
				for(stop = with10->Get_NTerms(), k = 1; k <= stop; k++)
				{   // Adds each term separately
					int stop1 = 0;
					BuffInit = 1 + with10->Get_NPhases() * (k - 1);
					BuffEnd = with10->Get_NPhases() * k;
					for(stop1 = BuffEnd, i = BuffInit; i <= stop1; i++)
					{
						(VoltageBuffer)[i - (BuffInit - 1) - 1] = (with10->Vterminal)[i - 1];
					}
					if(with10->Get_NPhases() == with10->Get_NConds())
						myRefIdx = with10->Get_NPhases() + 1;
					else
						myRefIdx = with10->Get_NConds();

                //Brings the first phase to the last place for calculations
					(VoltageBuffer)[myRefIdx - 1] = (VoltageBuffer)[1 - 1];
                // Calculates the LL voltages
					for(stop1 = with10->Get_NPhases(), i = 1; i <= stop1; i++)
					{
						(VoltageBuffer)[i - 1] = csub((VoltageBuffer)[i - 1], (VoltageBuffer)[i + 1 - 1]);
					}
					ConvertComplexArrayToPolar(&(VoltageBuffer[0]), with10->Yorder);
                /*Put Terminal Voltages into Monitor*/
					AddDblsToBuffer((pDoubleArray)&(VoltageBuffer)[1 - 1].re, 2 * with10->Get_NPhases());
				}

              /*Get all terminsl currents*/
				with10->ComputeIterminal(ActorID);   // only does calc if needed
				for(stop = with10->Yorder, i = 1; i <= stop; i++)
				{
					(CurrentBuffer)[i - 1] = (with10->Iterminal)[i - 1];
				}
				ConvertComplexArrayToPolar(&(CurrentBuffer[0]), with10->Yorder);
              /*Put Terminal currents into Monitor*/
				AddDblsToBuffer((pDoubleArray)&(CurrentBuffer)[1 - 1].re, 2 * with10->Yorder);
				return;
			}
		}
		break;
		default:  // Ignore invalid mask
		return;
		break;
	}
	if(((Mode & SEQUENCEMASK) > 0) && (Fnphases == 3))  // Convert to Symmetrical components
	{
		int stop = 0;
		Phase2SymComp(&(VoltageBuffer[0]), &V012[0]);
		Phase2SymComp(&(CurrentBuffer)[Offset + 1 - 1], &I012[0]);
		NumVI = 3;
		IsSequence = true;
       // Replace voltage and current buffer with sequence quantities
		for(stop = 3, i = 1; i <= stop; i++)
		{
			(VoltageBuffer)[i - 1] = V012[i - 1];
		}
		for(stop = 3, i = 1; i <= stop; i++)
		{
			(CurrentBuffer)[Offset + i - 1] = I012[i - 1];
		}
	}
	else
	{
		NumVI = Fnconds;
		IsSequence = false;
	}
	IsPower = false;  // Init so compiler won't complain
	switch((Mode & ModeMask))
	{
		case 	0:        // Convert to Mag, Angle   and compute residual if required
		{
			IsPower = false;
			if (IncludeResidual)
			{
				if (VIpolar)
				{
					ResidualVolt = ResidualPolar(&VoltageBuffer[1], Fnphases);
					ResidualCurr = ResidualPolar(&CurrentBuffer[Offset + 1], Fnphases);
				}
				else
				{
					ResidualVolt = Residual(&VoltageBuffer[1], Fnphases);
					ResidualCurr = Residual(&CurrentBuffer[Offset + 1], Fnphases);
				}
			}
			if (VIpolar)
			{
				ConvertComplexArrayToPolar(&(VoltageBuffer[0]), NumVI);
				ConvertComplexArrayToPolar((pComplexArray) & (CurrentBuffer)[Offset + 1 - 1], NumVI);    // Corrected 3-11-13
			}
			// commented out for now
			this->Freq = DefaultBaseFreq; //FreqCalc(stepCount, &VoltageBuffer[0], this->Vold, Sec); // BLS - Calculate frequency
		}
		break;     // Convert Voltage Buffer to power kW, kvar or Mag/Angle
		case 	1:
		{
			CalcKPowers(&(VoltageBuffer[0]), &(VoltageBuffer[0]), (pComplexArray) &(CurrentBuffer)[Offset + 1 - 1], NumVI);
			if(IsSequence || ActiveCircuit[ActorID]->PositiveSequence)
				CmulArray(&(VoltageBuffer[0]), 3.0, NumVI); // convert to total power
			if(Ppolar)
				ConvertComplexArrayToPolar(&(VoltageBuffer[0]), NumVI);
			IsPower = true;
		}
		break;
		case 	4:
		{
			IsPower = false;
			ConvertComplexArrayToPolar(&(FlickerBuffer[0]), Fnphases);
		}
		break;
		default:
		  ;
		break;
	}

   // Now check to see what to write to disk
	switch((Mode & (MAGNITUDEMASK + POSSEQONLYMASK)))
	{
		case 	32: // Save Magnitudes only
		{
			int stop = 0;
			for(stop = NumVI, i = 1; i <= stop; i++)
			{
				AddDblToBuffer((VoltageBuffer)[i - 1].re);
			}
			if(IncludeResidual)
				AddDblToBuffer(ResidualVolt.re);
			if(!IsPower)
			{
				int stop = 0;
				for(stop = NumVI, i = 1; i <= stop; i++)
				{
					AddDblToBuffer((CurrentBuffer)[Offset + i - 1].re);
				}
				if(IncludeResidual)
					AddDblToBuffer(ResidualCurr.re);
				AddDblToBuffer(this->Freq); // BLS
			}
		}
		break; // Save Pos Seq or Avg of all Phases or Total power (Complex)
		case 	64:
		{
			if(IsSequence)
			{
				AddDblsToBuffer((pDoubleArray)&(VoltageBuffer)[2 - 1].re, 2);
				if(!IsPower)
					AddDblsToBuffer((pDoubleArray)&(CurrentBuffer)[Offset + 2 - 1].re, 2);
			}
			else
			{
				if(IsPower)
				{
					int stop = 0;
					Sum = CZero;
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						caccum(Sum, (VoltageBuffer)[i - 1]);
					}
					AddDblsToBuffer((pDoubleArray)&Sum.re, 2);
				}
				else
  // Average the phase magnitudes and  sum angles
				{
					int stop = 0;
					Sum = CZero;
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						caccum(Sum, (VoltageBuffer)[i - 1]);
					}
					Sum.re = Sum.re / Fnphases;
					AddDblsToBuffer((pDoubleArray)&Sum.re, 2);
					Sum = CZero;
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						caccum(Sum, (CurrentBuffer)[Offset + i - 1]);
					}   // Corrected 3-11-13
					Sum.re = Sum.re / Fnphases;
					AddDblsToBuffer((pDoubleArray)&Sum.re, 2);
					AddDblToBuffer(this->Freq); // BLS
				}
			}
			
		}
		break;  // Save Pos Seq or Aver magnitude of all Phases of total kVA (Magnitude)
		case 	96:
		{
			if(IsSequence)
			{
				AddDblToBuffer((VoltageBuffer)[2 - 1].re);    // Get_First() double is magnitude
				if(!IsPower)
					AddDblToBuffer((CurrentBuffer)[Offset + 2 - 1].re);
			}
			else
			{
				int stop = 0;
				dSum = 0.0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					dSum = dSum + (VoltageBuffer)[i - 1].re;
				} //Cabs(VoltageBuffer^[i]);
				if(!IsPower)
					dSum = dSum / Fnphases;
				AddDblToBuffer(dSum);
				if(!IsPower)
				{
					int stop = 0;
					dSum = 0.0;
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						dSum = dSum + (CurrentBuffer)[Offset + i - 1].re;
					} //Cabs(CurrentBuffer^[Offset+i]);
					dSum = dSum / Fnphases;
					AddDblToBuffer(dSum);
					AddDblToBuffer(this->Freq); // BLS
				}
			}
		}
		break;
		default:
		switch(Mode)
		{
			case 	4:
			AddDblsToBuffer((pDoubleArray)&(FlickerBuffer)[1 - 1].re, Fnphases * 2);
			break;
			case 	5:
			AddDblsToBuffer((pDoubleArray)&(SolutionBuffer)[1 - 1], NumSolutionVars);
			break;
			default:
			AddDblsToBuffer((pDoubleArray)&(VoltageBuffer)[1 - 1].re, NumVI * 2);
			if(!IsPower)
			{
				if(IncludeResidual)
					AddDblsToBuffer((pDoubleArray) &ResidualVolt, 2);
				AddDblsToBuffer((pDoubleArray)&(CurrentBuffer)[Offset + 1 - 1].re, NumVI * 2);
				if(IncludeResidual)
					AddDblsToBuffer((pDoubleArray) &ResidualCurr, 2);
				AddDblToBuffer(this->Freq); // BLS
			}	
			break;
		}
		break;
	}
}

/*--------------------------------------------------------------------------*/

void TMonitorObj::AddDblsToBuffer(pDoubleArray Dbl, int Ndoubles)
{
	int i = 0;
	int stop = 0;
	for(stop = Ndoubles, i = 1; i <= stop; i++)
	{
		AddDblToBuffer((Dbl)[i - 1]);
	}
}

/*--------------------------------------------------------------------------*/

void TMonitorObj::AddDblToBuffer(double Dbl)
{

    // first check to see if there's enough room
    // if not, save to monitorstream first.
	if(BufPtr == BufferSize)
		Save();
	++BufPtr;
	MonBuffer[BufPtr - 1] = (float) Dbl;
}

void TMonitorObj::DoFlickerCalculations(int ActorID)
{
	int FSignature = 0;
	int FVersion = 0;
	unsigned int RecordSize = 0;
	unsigned int RecordBytes = 0;
	float SngBuffer[100/*# range 1..100*/];
	float hr = 0.0F;
	float s = 0.0F;
	int n = 0;
	int nPst = 0;
	int i = 0;
	int P = 0;
	int bStart = 0;
	std::vector < std::vector <float> > Data; // indexed from zero (time) to FnPhases
	std::vector < std::vector <float> > pst; // indexed from zero to FnPhases - 1
	int ipst = 0;
	float tpst = 0.0F;
	float defaultpst = 0.0F;
	float VBase = 0.0F;
	int BusRef = 0;
	n = SampleCount;
	/*# with MonitorStream do */
	{
		auto &with0 = MonitorStream;
		with0.begin();  // Start at the beginning of the Stream
		with0.Read(&FSignature);
		with0.Read(&FVersion);
		with0.Read(&RecordSize);
		with0.Read(&Mode);
		
		with0.Read(dummyRec);
		bStart = (int) with0.Position();
	}
	RecordBytes = (unsigned int) (sizeof(SngBuffer[1 - 1]) * RecordSize);
	try

    // read rms voltages out of the monitor stream into arrays
	{
		int stop = 0;
		Data.resize( Fnphases + 1 );
		pst.resize( Fnphases );
		for(stop = Fnphases, P = 0; P <= stop; P++)
		{
			Data[P].resize(n);
		}
		i = 1;
		while(!(MonitorStream.Position() >= MonitorStream.Size()))
		{
			/*# with MonitorStream do */
			{
				auto &with1 = MonitorStream;
				int stop = 0;
				with1.Read(&hr);
				with1.Read(&s);
				with1.Read(SngBuffer);
				(Data[0])[i - 1] = (float) (s + 3600.0F * hr);
				for(stop = Fnphases, P = 1; P <= stop; P++)
				{
					(Data[P])[i - 1] = SngBuffer[2 * P - 1 - 1];
				}
				i = i + 1;
			}
		}

    // calculate the flicker level and pst
		nPst = 1 + Trunc((Data[0])[n - 1] / 600.0F); // pst updates every 10 minutes or 600 seconds
		for(stop = Fnphases - 1, P = 0; P <= stop; P++)
		{
			pst[P].resize(nPst);
			BusRef = (MeteredElement->Terminals)[MeteredTerminal - 1].BusRef;
			VBase = (float) (1000.0 * ActiveCircuit[ActorID]->Buses[BusRef - 1]->kVBase);
			FlickerMeter(n, BaseFrequency, VBase, &Data[0][0], &Data[P + 1][0], &pst[P][0]);
		}

    // stuff the flicker level and pst back into the monitor stream
		/*# with MonitorStream do */
		{
			auto &with2 = MonitorStream;
			int stop = 0;
			with2.myPos = bStart;
			tpst = 0.0F;
			ipst = 0;
			defaultpst = 0.0F;
			for(stop = n, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				if(((Data[0])[i - 1] - tpst) >= 600.0)
				{
					++ipst;
					tpst = (Data[0])[i - 1];
				}
				with2.myPos = with2.Position() + 2 * sizeof(hr); // don't alter the time
				for(stop1 = Fnphases, P = 1; P <= stop1; P++)
				{
					with2.Write( &(Data[P - 1][i - 1]), (int) sizeof(Data[P][i - 1]) );
					if((ipst > 0) && (ipst <= nPst))
						with2.Write(&pst[P - 2][ipst - 1], sizeof(pst[P - 2][ipst - 1]));
					else
						with2.Write(&defaultpst);
				}
			}
		}
/* }
	__finally
	{*/

		for(int stop = Fnphases, P = 0; P <= stop; P++)
		{
			Data[P].clear();
		}
		for(stop = Fnphases - 1, P = 0; P <= stop; P++)
		{
			pst[P].clear();
		}
	}
	catch (...)
	{
		//
	}
}

/*--------------------------------------------------------------------------*/

void TMonitorObj::TranslateToCSV(bool Show, int ActorID)
{
	String CSVName;
	TTextRec f = {};
	int FSignature = 0;
	int FVersion = 0;
	float hr = 0.0F;
	unsigned int i = 0;
	int Mode = 0;
	unsigned int Nread = 0;
	PAnsiChar PStr = nullptr;
	unsigned int RecordBytes = 0;
	unsigned int RecordSize = 0;
	float s = 0.0F;
	float SngBuffer[100/*# range 1..100*/] = {0.0};
	Save();  // Save present buffer
	CloseMonitorStream(ActorID);   // Position at beginning
	CSVName = Get_FileName(ActorID);
	try
	{
		AssignFile(f, CSVName);    // Make CSV file
		if(ConcatenateReports && (ActorID != 1))
			Append(f);
		else
			Rewrite(f);
		IOResultToException();
	}
	catch (std::exception &e)
	{
		{
			DoSimpleMsg(String("Error opening CSVFile \"") + CSVName
	           + "\" for writing"
	           + CRLF
	           + (std::string) e.what(), 672);
			return;
		}
	}
	/*# with MonitorStream do */
	{
		auto &with0 = MonitorStream;
		with0.begin();  // Start at the beginning of the Stream
		with0.Read(&FSignature);
		with0.Read(&FVersion);
		with0.Read(&RecordSize);
		with0.Read(&Mode);
		with0.Read(dummyRec);
	}
	PStr = &StrBuffer[0];
	if(!ConcatenateReports || (ActorID == 1))
		WriteLn(f, PStr);
	RecordBytes = (unsigned int) (sizeof(SngBuffer[1 - 1]) * RecordSize);
	try
	{
		try
		{
			while(!(MonitorStream.Position() >= MonitorStream.Size()))
			{
				int stop = 0;
				/*# with MonitorStream do */
				{
					auto& with1 = MonitorStream;
					with1.Read(&hr);
					with1.Read(&s);
					Nread = with1.Read(&SngBuffer[0], RecordBytes);
				}
				if(Nread < RecordBytes)
					break;

				Write(f, Format("%d", (int)hr));          // hours
				Write(f, ", "); Write(f, Format("%.5f", s));      // sec
				for (int i = 0; i <= RecordSize-1; i++)
				{
					Write(f, ", "); Write(f, Format("%-.6g", SngBuffer[i]));
				}				
				WriteLn(f);
			}
		}
		catch (std::exception &e)
		{
			{
				DoSimpleMsg(String("Error Writing CSVFile \"") + CSVName
	           + "\" "
	           + CRLF
	           + (std::string) e.what(), 673);
			}
		}
/* }
	__finally
	{*/
		CloseMonitorStream(ActorID);
		CloseFile(f);
	}
	catch (...)
	{
		//
	}
    if (Show && AutoDisplayShowReport)
		FireOffEditor(CSVName);

	GlobalResult = CSVName;
}

/*--------------------------------------------------------------------------*/  //Get present value of terminal Curr for reports

void TMonitorObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;

/*
  Revised 12-7-99 to return Zero current instead of Monitored element current because
 it was messing up Newton iteration.
*/
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TMonitorObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TMonitorObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int k = 0;
	TDSSCktElement::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
	{
		int stop = 0;
		WriteLn(f);
		{ Write(f, "// BufferSize="); WriteLn(f, BufferSize, 0); }
		{ Write(f, "// Hour="); WriteLn(f, Hour, 0); }
		{ Write(f, "// Sec="); WriteLn(f, Sec, 0); }
		{ Write(f, "// BaseFrequency="); WriteLn(f, BaseFrequency, 0, 1); }
		{ Write(f, "// Bufptr="); WriteLn(f, BufPtr, 0); }
		WriteLn(f, "// Buffer=");
		k = 0;
		for(stop = BufPtr, i = 1; i <= stop; i++)
		{
			{ Write(f, (MonBuffer)[i - 1], 0, 1); Write(f, ", "); }
			++k;
			if(k == (2 + Fnconds * 4))
			{
				WriteLn(f);
				k = 0;
			}
		}
		WriteLn(f);
	}
}

void TMonitorObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1, ""); //'element';
	Set_PropertyValue(2, "1"); //'terminal';
	Set_PropertyValue(3, "0"); //'mode';
	Set_PropertyValue(4, ""); // 'action';  // buffer=clear|save|take|process
	Set_PropertyValue(5, "NO");
	Set_PropertyValue(6, "YES");
	Set_PropertyValue(7, "YES");
	inherited::InitPropertyValues(NumPropsThisClass);
}


/*--------------------------------------------------------------------------*/

void TDSSMonitor::TOPExport(String ObjName)
{
/*
	TStringList* NameList;
	TStringList* cNames;
	pDoubleArray Vbuf;
	pDoubleArray CBuf;
	TMonitorObj* Obj = nullptr;
	int i = 0;
	double MaxTime = 0.0;
	PointerList::TPointerList* ObjList = nullptr;
	bool Hours = false;
	TMonitorStrBuffer StrBuffer;
	PAnsiChar pStrBuffer = nullptr;
	int FVersion = 0;
	int FSignature = 0;
	int iMode = 0;
	unsigned int Nread = 0;
	unsigned int RecordSize = 0;
	unsigned int RecordBytes = 0;
	unsigned int PositionSave = 0;
	float SngBuffer[100]; //# range 1..100
	double Time = 0.0;
	float hr = 0.0F;
	float s = 0.0F;
	String TrialFileName;
	String FileNumber;
     // Create a unique file name
	TrialFileName = GetOutputDirectory() + "TOP_Mon_" + ObjName;
	FileNumber = "";
	i = 0;
	while(FileExists(TrialFileName + FileNumber + ".STO"))
	{
		++i;
		FileNumber = IntToStr(i);
	}
	TOPTransferFile->Filename = TrialFileName + FileNumber + ".STO";
	try
	{
		//TOPTransferFile->Open();
	}
	catch (std::exception &e)
	{
		{
			DoSimpleMsg(String("TOP Transfer File Error: ") + (std::string) e.what(), 674);
			try
			{
				//TOPTransferFile->Close();
              //OK if Error
			}
			catch(...)
			{
				;
			}
			return;
		}
	}

     //Send only fixed interval data
	ObjList = new PointerList::TPointerList(10);
	NameList = new TStringList();
	cNames = new TStringList();

     //Make a List of fixed interval data where the interval is greater than 1 minute
	if(CompareText(ObjName, "ALL") == 0)
	{
		DoSimpleMsg("ALL option not yet implemented.", 675);
     //  Obj := ElementList.Get_First();
     //  While Obj <>  Nil Do Begin
     //     If Obj.Interval>(1.0/60.0) Then ObjList.Add(Obj);
     //     Obj := ElementList.Get_Next();
     //  End;
	}
	else
	{
		Obj = ((TMonitorObj*) Find(ObjName));
		if(Obj != nullptr)
			ObjList->Add(Obj);
		else
			DoSimpleMsg(String("Monitor.") + ObjName + " not found.", 676);
	}

     //If none found, exit
	if(ObjList->get_myNumList() > 0)
	{
		Obj = ((TMonitorObj*) ObjList->Get_First());  //And only
		//with Obj do
		{
			auto with0 = Obj;
			int stop = 0;
			with0->Save();  // Save present buffer
			with0->CloseMonitorStream(ActiveActor);
			pStrBuffer = (PAnsiChar) &(with0->StrBuffer);
			//with MonitorStream do
			{
				auto &with1 = with0->MonitorStream;
				with1.begin();  // Start at the beginning of the Stream
				with1.Read(&FSignature);
				with1.Read(&FVersion);
				with1.Read(&RecordSize);
				with1.Read(&iMode);
				with1.Read(dummyRec);
			}

           //Parse off Channel Names
			AuxParser[ActiveActor]->WhiteSpace = "";
			AuxParser[ActiveActor]->CmdString = pStrBuffer;
			AuxParser[ActiveActor]->GetNextParam();  // pop off two
			AuxParser[ActiveActor]->GetNextParam();
			for(stop = RecordSize, i = 1; i <= stop; i++)
			{
				AuxParser[ActiveActor]->GetNextParam();
				NameList->push_back(AuxParser[ActiveActor]->MakeString_());
			}
			AuxParser[ActiveActor]->ResetDelims();

           //Write TOP Header

          //Find Max number of points
			RecordBytes = (unsigned int) (sizeof(SngBuffer[1 - 1]) * RecordSize);
			Vbuf->resize( RecordSize );  // Put Everything in here for now
			CBuf->resize( 1 );   // just a dummy -- Cbuf is ignored here

           //Get first time value and set the interval to this value
			hr = 0.0F;
			s = 0.0F;
			if(!(with0->MonitorStream.Position() >= with0->MonitorStream.Size()))
				//with MonitorStream do
				{
					auto &with2 = with0->MonitorStream;
					with2.Read(&hr);  // singles
					with2.Read(&s);
					with2.Read(SngBuffer);
				}
           //Set Hours or Seconds for Interval
			Hours = true;
			if((s > 0.0F) && (s < 100.0F))
				Hours = false;
			switch(ActiveCircuit[ActiveActor]->Solution->DynaVars.SolutionMode)
			{
				case 	HARMONICMODE:
				Time = hr;
				break;
				default:
				if(Hours) // in hrs
					Time = hr + s / 3600.0F;
				else
					Time = hr * 3600.0F + s; // in sec
				break;
			}

           //Now find Maxtime in Monitor
			PositionSave = (unsigned int) with0->MonitorStream.Position();
			with0->MonitorStream.end();
			if(!(with0->MonitorStream.Position() >= with0->MonitorStream.Size()))
				//# with MonitorStream do
				{
					auto &with3 = with0->MonitorStream;
					with3.Read(&hr);  // singles
					with3.Read(&s);
					with3.Read(SngBuffer);
				}
			switch(ActiveCircuit[ActiveActor]->Solution->DynaVars.SolutionMode)
			{
				case 	HARMONICMODE:
				MaxTime = hr;
				break;
				default:
				if(Hours) // in hrs
					MaxTime = hr + s / 3600.0F;
				else
					MaxTime = hr * 3600.0F + s; // in sec
				break;
			}

           //Go Back to where we were
			with0->MonitorStream.begin();
			TOPTransferFile->WriteHeader(Time, MaxTime, Time, (int) RecordSize, 0, 16, "DSS (TM), EPRI (R)");
			TOPTransferFile->WriteNames(*NameList, *cNames);

           //Now Process rest of monitor file
			if(!(with0->MonitorStream.Position() >= with0->MonitorStream.Size()))
			{
				do
				{
					int stop = 0;
					for(stop = RecordSize, i = 1; i <= stop; i++)
					{
						(*Vbuf)[i - 1] = SngBuffer[i - 1];
					}
					TOPTransferFile->WriteData(Time, Vbuf, CBuf);
					//# with MonitorStream do
					{
						auto &with4 = with0->MonitorStream;
						with4.Read(&hr);
						with4.Read(&s);
						Nread = with4.Read(SngBuffer);
					}
					if(Nread < RecordBytes)
						break;
					switch(ActiveCircuit[ActiveActor]->Solution->DynaVars.SolutionMode)
					{
						case 	HARMONICMODE:
						Time = hr;
						break;
						default:
						if(Hours) // in hrs
							Time = hr + s / 3600.0F;
						else
							Time = hr * 3600.0F + s; // in sec
						break;
					}
				}
				while(!(with0->MonitorStream.Position() >= with0->MonitorStream.Size()));
			}
			with0->CloseMonitorStream(ActiveActor);
			TOPTransferFile->Close();
			TOPTransferFile->SendToTop();
			Vbuf->clear();
			CBuf->clear();
		}
	}
	delete ObjList; // ObjList->~TPointerList();
	NameList->clear();
	cNames->clear();
	*/
}

String TMonitorObj::Get_FileName(int ActorID)
{
	String result;
	if(ConcatenateReports)
		result = GetOutputDirectory() + CircuitName_[ActorID] + "Mon_" + get_Name() + ".csv";
	else
		result = GetOutputDirectory() + CircuitName_[ActorID] + "Mon_" + get_Name() + "_" + IntToStr(ActorID) + ".csv";
	return result;
}

double TMonitorObj::FreqCalc(int stepCount, complex* V, complex Vo, double time)
{
	double w = (2 * PI * DefaultBaseFreq); // calculate wo
	if (stepCount == 1)
	{
		U1 = 0.0;
		U2 = 0.0;
		U3 = 0.0;
		if (VIpolar)
		{
			this->Vold = *V;
		}
		else
		{
			polar x;
			x = ctopolardeg(*V);
			this->Vold.re = x.mag;
			this->Vold.im = x.ang;
		}
		this->Told = time;
		return DefaultBaseFreq;
	}
	else
	{
		complex Vnew;
		if (VIpolar)
		{
			Vnew = *V;
		}
		else
		{
			polar x;
			x = ctopolardeg(*V);
			Vnew.re = x.mag;
			Vnew.im = x.ang;
		}
		if (Vnew.im < 0)
		{
			Vnew.im = Vnew.im + 90; // If the angle is negative, it has to be pushed back into the range of 0 to 360
		}
		double angle = (Vnew.im - Vo.im) * (PI / 180); // Find the delta between the voltage angles and convert to radians
		double dt = (time - Told); // Find the delta of the time between angles
		double in;
		U3 = angle * 1 / (dt * w); // Compute input into filter.  Use median value if and only if there are 3 or more stepCounts
		if (stepCount > 2)
		{
			std::vector <double> UU;
			UU = { U1,U2,U3 };
			std::sort(UU.begin(), UU.end());
			in = UU[1];
		}
		else
		{
			in = U3;
		}

		// Use PI controller to update the state variable s
		Filter_F.Kp = 1 / (Tf); // Determine proportional gain
		Filter_F.kDen = exp(-1 * abs(dt / Tf)); // Determine the denominator for the Integral of the PI controller
		Filter_F.kNum = (1 - Filter_F.kDen) * Tf; // Determine the numerator for the Integral of the PI controller
		this->s = Filter_F.SolvePI(in); // Solve for the PI controller 
		double ff = (1 + this->s) * (w / (2 * PI)); // Calculate the new frequency

		this->Vold = Vnew; // Set the old voltage angle to the new voltage angle for next iteration of calculations
		this->Told = time; // Set the old time to the new time for next iteration of calculations

		this->U1 = U2;
		this->U2 = U3;
			
		return ff; // return the calculated frequency
	}
}
  //WriteDLLDebugFile('Monitor');




}  // namespace Monitor

