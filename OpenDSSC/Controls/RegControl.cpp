
#pragma hdrstop

#include "RegControl.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "CktElement.h"
#include "Ucmatrix.h"
#include "mathutil.h"
#include <math.h>
#include "Utilities.h"
#include "AutoTrans.h"

using namespace std;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace ControlClass;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace ParserDel;
using namespace System;
using namespace Transformer;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace RegControl
{

TRegControlObj::TRegControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TRegControlObj::TRegControlObj(String ClassName) : inherited(ClassName) {}
TRegControlObj::TRegControlObj() {}


TRegControlObj* ActiveRegControlObj = nullptr;
const int AVGPHASES = -1;
const int MAXPHASE = -2;
const int MINPHASE = -3;
const int ACTION_TAPCHANGE = 0;
const int ACTION_REVERSE = 1;
const int NumPropsThisClass = 32;
std::vector<int> LastChange;
    
/*--------------------------------------------------------------------------*/  // Creates superstructure for all RegControl objects

TRegControl::TRegControl()
{
	int i = 0;
	;
	Class_Name = "RegControl";
	DSSClassType = DSSClassType + REG_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	LastChange.resize(CPU_Cores + 1);
	for(int stop = CPU_Cores, i = 0; i <= stop; i++)
	{
		LastChange[i] = 0;
	}
}

/*--------------------------------------------------------------------------*/

TRegControl::~TRegControl()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TRegControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "transformer";
	PropertyName[2 - 1] = "winding";
	PropertyName[3 - 1] = "vreg";
	PropertyName[4 - 1] = "band";
	PropertyName[5 - 1] = "ptratio";
	PropertyName[6 - 1] = "CTprim";
	PropertyName[7 - 1] = "R";
	PropertyName[8 - 1] = "X";
	PropertyName[9 - 1] = "bus";
	PropertyName[10 - 1] = "delay";
	PropertyName[11 - 1] = "reversible";
	PropertyName[12 - 1] = "revvreg";
	PropertyName[13 - 1] = "revband";
	PropertyName[14 - 1] = "revR";
	PropertyName[15 - 1] = "revX";
	PropertyName[16 - 1] = "tapdelay";
	PropertyName[17 - 1] = "debugtrace";
	PropertyName[18 - 1] = "maxtapchange";
	PropertyName[19 - 1] = "inversetime";
	PropertyName[20 - 1] = "tapwinding";
	PropertyName[21 - 1] = "vlimit";
	PropertyName[22 - 1] = "PTphase";
	PropertyName[23 - 1] = "revThreshold";
	PropertyName[24 - 1] = "revDelay";
	PropertyName[25 - 1] = "revNeutral";
	PropertyName[26 - 1] = "EventLog";
	PropertyName[27 - 1] = "RemotePTRatio";
	PropertyName[28 - 1] = "TapNum";
	PropertyName[29 - 1] = "Reset";
	PropertyName[30 - 1] = "LDC_Z";
	PropertyName[31 - 1] = "rev_Z";
	PropertyName[32 - 1] = "Cogen";
	PropertyHelp[1 - 1] = String("Name of Transformer or AutoTrans element to which the RegControl is connected. " "Do not specify the full object name; \"Transformer\" or \"AutoTrans\" is assumed for " "the object class.  Example:") + CRLF
	           + CRLF
	           + "Transformer=Xfmr1";
	PropertyHelp[2 - 1] = "Number of the winding of the transformer element that the RegControl is monitoring. "
	           "1 or 2, typically.  Side Effect: Sets TAPWINDING property to the same winding.";
	PropertyHelp[3 - 1] = "Voltage regulator setting, in VOLTS, for the winding being controlled.  Multiplying this "
	           "value times the ptratio should yield the voltage across the WINDING of the controlled transformer."
	           " Default is 120.0";
	PropertyHelp[4 - 1] = "Bandwidth in VOLTS for the controlled bus (see help for ptratio property).  Default is 3.0";
	PropertyHelp[5 - 1] = "Ratio of the PT that converts the controlled winding voltage to the regulator control voltage. "
	           "Default is 60.  If the winding is Wye, the line-to-neutral voltage is used.  Else, the line-to-line "
	           "voltage is used. SIDE EFFECT: Also sets RemotePTRatio property.";
	PropertyHelp[6 - 1] = "Rating, in Amperes, of the primary CT rating for which the line amps convert to control rated amps."
	           "The typical default secondary ampere rating is 0.2 Amps (check with manufacturer specs). "
	           "Current at which the LDC voltages match the R and X settings.";
	PropertyHelp[7 - 1] = "R setting on the line drop compensator in the regulator, expressed in VOLTS.";
	PropertyHelp[8 - 1] = "X setting on the line drop compensator in the regulator, expressed in VOLTS.";
	PropertyHelp[9 - 1] = "Name of a bus (busname.nodename) in the system to use as the controlled bus instead of the bus to which the "
	           "transformer winding is connected or the R and X line drop compensator settings.  Do not specify this "
	           "value if you wish to use the line drop compensator settings.  Default is null string. Assumes the base voltage for this "
	           "bus is the same as the transformer winding base specified above. "
	           "Note: This bus (1-phase) WILL BE CREATED by the regulator control upon SOLVE if not defined by some other device. "
	           "You can specify the node of the bus you wish to sample (defaults to 1). "
	           "If specified, the RegControl is redefined as a 1-phase device since only one voltage is used.";
	PropertyHelp[10 - 1] = "Time delay, in seconds, from when the voltage goes out of band to when the tap changing begins. "
	           "This is used to determine which regulator control will act first. Default is 15.  You may specify any "
	           "floating point number to achieve a model of whatever condition is necessary.";
	PropertyHelp[11 - 1] = "{Yes |No*} Indicates whether or not the regulator can be switched to regulate in the reverse direction. Default is No."
	           "Typically applies only to line regulators and not to LTC on a substation transformer.";
	PropertyHelp[12 - 1] = "Voltage setting in volts for operation in the reverse direction.";
	PropertyHelp[13 - 1] = "Bandwidth for operating in the reverse direction.";
	PropertyHelp[14 - 1] = "R line drop compensator setting for reverse direction.";
	PropertyHelp[15 - 1] = "X line drop compensator setting for reverse direction.";
	PropertyHelp[16 - 1] = "Delay in sec between tap changes. Default is 2. This is how long it takes between changes "
	           "after the first change.";
	PropertyHelp[17 - 1] = "{Yes | No* }  Default is no.  Turn this on to capture the progress of the regulator model "
	           "for each control iteration.  Creates a separate file for each RegControl named \"REG_name.CSV\".";
	PropertyHelp[18 - 1] = String("Maximum allowable tap change per control iteration in STATIC control mode.  Default is 16. ") + CRLF
	           + CRLF
	           + "Set this to 1 to better approximate actual control action. "
	           + CRLF
	           + CRLF
	           + "Set this to 0 to fix the tap in the current position.";
	PropertyHelp[19 - 1] = "{Yes | No* } Default is no.  The time delay is adjusted inversely proportional to the amount the voltage is outside the band down to 10%.";
	PropertyHelp[20 - 1] = "Winding containing the actual taps, if different than the WINDING property. Defaults to the same winding as specified by the WINDING property.";
	PropertyHelp[21 - 1] = "Voltage Limit for bus to which regulated winding is connected (e.g. first customer). Default is 0.0. "
	           "Set to a value greater then zero to activate this function.";
	PropertyHelp[22 - 1] = "For multi-phase transformers, the number of the phase being monitored or one of { MAX | MIN} for all phases. Default=1. "
	           "Must be less than or equal to the number of phases. Ignored for regulated bus.";
	PropertyHelp[23 - 1] = "kW reverse power threshold for reversing the direction of the regulator. Default is 100.0 kw.";
	PropertyHelp[24 - 1] = "Time Delay in seconds (s) for executing the reversing action once the threshold for reversing has been exceeded. Default is 60 s.";
	PropertyHelp[25 - 1] = "{Yes | No*} Default is no. Set this to Yes if you want the regulator to go to neutral in the reverse direction or in cogen operation.";
	PropertyHelp[26 - 1] = "{Yes/True* | No/False} Default is YES for regulator control. Log control actions to Eventlog.";
	PropertyHelp[27 - 1] = "When regulating a bus (the Bus= property is set), the PT ratio required to convert actual voltage at the remote bus to control voltage. "
	           "Is initialized to PTratio property. Set this property after setting PTratio.";
	PropertyHelp[28 - 1] = "An integer number indicating the tap position that the controlled transformer winding tap position is currently at, or is being set to.  If being set, and the value is outside the range of the transformer min or max tap,"
	           " then set to the min or max tap position as appropriate. Default is 0";
	PropertyHelp[29 - 1] = "{Yes | No} If Yes, forces Reset of this RegControl.";
	PropertyHelp[30 - 1] = "Z value for Beckwith LDC_Z control option. Volts adjustment at rated control current.";
	PropertyHelp[31 - 1] = "Reverse Z value for Beckwith LDC_Z control option.";
	PropertyHelp[32 - 1] = "{Yes|No*} Default is No. The Cogen feature is activated. Continues looking forward if power "
	           "reverses, but switches to reverse-mode LDC, vreg and band values.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TRegControl::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new RegControl and add it to RegControl class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TRegControlObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TRegControl::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;

	auto Max = [&](int A, int B) -> int 
	{
		int result = 0;
		if(A >= B)
			result = A;
		else
			result = B;
		return result;
	};

  // continue parsing WITH contents of Parser
	ActiveRegControlObj = (TRegControlObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveRegControlObj);
	result = 0;
	/*# with ActiveRegControlObj do */
	{
		auto with0 = ActiveRegControlObj;
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
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 120);
				break;
				case 	1:
				with0->ElementName = String("Transformer.") + LowerCase(Param);
				break;
				case 	2:
				with0->ElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->Vreg = Parser[ActorID]->MakeDouble_();
				break;
				case 	4:
				with0->Bandwidth = Parser[ActorID]->MakeDouble_();
				break;
				case 	5:
				with0->PTRatio = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				with0->CTRating = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				with0->R = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
				with0->X = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
				with0->RegulatedBus = Param;
				break;
				case 	10:
				with0->TimeDelay = Parser[ActorID]->MakeDouble_();
				break;
				case 	11:
				with0->IsReversible = InterpretYesNo(Param);
				break;
				case 	12:
				with0->revVreg = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->revBandwidth = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->revR = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				with0->revX = Parser[ActorID]->MakeDouble_();
				break;
				case 	16:
				with0->TapDelay = Parser[ActorID]->MakeDouble_();
				break;
				case 	17:
				with0->DebugTrace = InterpretYesNo(Param);
				break;
				case 	18:
				with0->TapLimitPerChange = Max(0, Parser[ActorID]->MakeInteger_());
				break;
				case 	19:
				with0->FInversetime = InterpretYesNo(Param);
				break;
				case 	20:
				with0->TapWinding = Parser[ActorID]->MakeInteger_();
				break;
				case 	21:
				{
					with0->Vlimit = Parser[ActorID]->MakeDouble_();
					if(with0->Vlimit > 0.0)
						with0->VLimitActive = true;
					else
						with0->VLimitActive = false;
				}
				break;
				case 	22:
				if(CompareTextShortest(Param, "max") == 0)
					with0->FPTPhase = MAXPHASE;
				else
				{
					if(CompareTextShortest(Param, "min") == 0)
						with0->FPTPhase = MINPHASE;
					else
						with0->FPTPhase = Max(1, Parser[ActorID]->MakeInteger_());
				}
				break;
				case 	23:
				with0->kWRevPowerThreshold = Parser[ActorID]->MakeDouble_();
				break;
				case 	24:
				with0->revDelay = Parser[ActorID]->MakeDouble_();
				break;
				case 	25:
				with0->ReverseNeutral = InterpretYesNo(Param);
				break;
				case 	26:
				with0->ShowEventLog = InterpretYesNo(Param);
				break;
				case 	27:
				with0->RemotePTRatio = Parser[ActorID]->MakeDouble_();
				break;
				case 	28:
				with0->Set_TapNum(Parser[ActorID]->MakeInteger_());
				break;
				case 	29:
				if(InterpretYesNo(Param))  // force a reset
				{
					with0->Reset(ActorID);
					with0->Set_PropertyValue(29,"n"); // so it gets reported properly
				}
				break;
				case 	30:
				with0->LDC_Z = Parser[ActorID]->MakeDouble_();
				break;
				case 	31:
				with0->revLDC_Z = Parser[ActorID]->MakeDouble_();
				break;
				case 	32:
				with0->CogenEnabled = InterpretYesNo(Param);
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveRegControlObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	2:
				{
					with0->TapWinding = with0->ElementTerminal;  // Resets if property re-assigned
					with0->Set_PropertyValue(20,Param);
				}
				break;
				case 	5:
				with0->RemotePTRatio = with0->PTRatio;
				break;  // re-initialise RemotePTRatio whenever PTRatio is set
				case 	17:
				if(with0->DebugTrace)
				{
					AssignFile(with0->Tracefile, GetOutputDirectory() + "REG_" + with0->get_Name() + ".CSV");
					Rewrite(with0->Tracefile);
					IOResultToException();
					WriteLn(with0->Tracefile, "Hour, Sec, ControlIteration, Iterations, LoadMultiplier, Present Tap, Pending Change, Actual Change, Increment, Min Tap, Max Tap");
					CloseFile(with0->Tracefile);
				}
				break;
				case 	23:
				with0->RevPowerThreshold = with0->kWRevPowerThreshold * 1000.0;
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		with0->RecalcElementData(ActorID);
	}  /*With*/
	return result;
}



/*--------------------------------------------------------------------------*/

int TRegControl::MakeLike(const String RegControlName)
{
	int result = 0;
	TRegControlObj* OtherRegControl = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this RegControl name in the present collection*/
	OtherRegControl = ((TRegControlObj*) Find(RegControlName));
	if(OtherRegControl != nullptr)
		/*# with ActiveRegControlObj do */
		{
			auto with0 = ActiveRegControlObj;
			int stop = 0;
			with0->Set_NPhases(OtherRegControl->Fnphases);
			with0->Set_Nconds(OtherRegControl->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherRegControl->ElementName;
			with0->Set_ControlledElement(OtherRegControl->get_FControlledElement());  // Pointer to target circuit element
			with0->ElementTerminal = OtherRegControl->ElementTerminal;
			with0->Vreg = OtherRegControl->Vreg;
			with0->Bandwidth = OtherRegControl->Bandwidth;
			with0->PTRatio = OtherRegControl->PTRatio;
			with0->RemotePTRatio = OtherRegControl->RemotePTRatio;
			with0->CTRating = OtherRegControl->CTRating;
			with0->R = OtherRegControl->R;
			with0->X = OtherRegControl->X;
			with0->RegulatedBus = OtherRegControl->RegulatedBus;
			with0->TimeDelay = OtherRegControl->TimeDelay;
			with0->IsReversible = OtherRegControl->IsReversible;
			with0->revVreg = OtherRegControl->revVreg;
			with0->revBandwidth = OtherRegControl->revBandwidth;
			with0->revR = OtherRegControl->revR;
			with0->revX = OtherRegControl->revX;
			with0->TapDelay = OtherRegControl->TapDelay;
			with0->TapWinding = OtherRegControl->TapWinding;
			with0->FInversetime = OtherRegControl->FInversetime;
			with0->TapLimitPerChange = OtherRegControl->TapLimitPerChange;
			with0->kWRevPowerThreshold = OtherRegControl->kWRevPowerThreshold;
			with0->RevPowerThreshold = OtherRegControl->RevPowerThreshold;
			with0->revDelay = OtherRegControl->revDelay;
			with0->ReverseNeutral = OtherRegControl->ReverseNeutral;
			with0->ShowEventLog = OtherRegControl->ShowEventLog;
    //    DebugTrace     := OtherRegControl.DebugTrace;  Always default to NO
			with0->FPTPhase = OtherRegControl->FPTPhase;
			with0->Set_TapNum(OtherRegControl->Get_TapNum());
			with0->CogenEnabled = OtherRegControl->CogenEnabled;
			with0->LDC_Z = OtherRegControl->LDC_Z;
			with0->revLDC_Z = OtherRegControl->revLDC_Z;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherRegControl->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in RegControl MakeLike: \"") + RegControlName
	           + "\" Not Found.", 121);
	return result;
}




/*==========================================================================*/
/*                    TRegControlObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TRegControlObj::TRegControlObj(TDSSClass* ParClass, const String RegControlName)
 : inherited(ParClass),
			Vreg(120.0),
			Bandwidth(3.0),
			PTRatio(60.0),
			RemotePTRatio(PTRatio),
			CTRating(300.0),
			R(0.0),
			X(0.0),
			LDC_Z(0.0),
			revVreg(120.0),
			revBandwidth(3.0),
			RevPowerThreshold(100000.0),
			kWRevPowerThreshold(100.0),
			revDelay(0.0),
			revR(0.0),
			revX(0.0),
			revLDC_Z(0.0),
			IsReversible(false),
			InReverseMode(false),
			ReversePending(false),
			ReverseNeutral(false),
			CogenEnabled(false),
			InCogenMode(false),
			RevHandle(0),
			RevBackHandle(0),
			LDCActive(false),
			UsingRegulatedBus(false),
			FPendingTapChange(0.0),
			TapDelay(0.0),
			DebugTrace(false),
			Armed(false),
			TapLimitPerChange(0),
			TapWinding(0),
			FInversetime(false),
			Vlimit(0.0),
			VLimitActive(false),
			FPTPhase(0),
			ControlledPhase(0),
			ControlActionHandle(0),
			VBuffer(nullptr),
			cBuffer(nullptr)
{
	Set_Name(LowerCase(RegControlName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	TimeDelay = 15.0;
	FPTPhase = 1;
	LDCActive = false;
	TapDelay = 2.0;
	TapLimitPerChange = 16;
	DebugTrace = false;
	Armed = false;

    /*Reverse mode variables*/
	revR = 0.0;
	revX = 0.0;
	revLDC_Z = 0.0;
	revDelay = 60.0; // Power must be reversed this long before it will reverse
	IsReversible = false;
	ReversePending = false;
	InReverseMode = false;
	ReverseNeutral = false;
	InCogenMode = false;
	CogenEnabled = false;
	RevHandle = 0;
	RevBackHandle = 0;
	ElementName = "";
	Set_ControlledElement(nullptr);
	ElementTerminal = 1;
	TapWinding = ElementTerminal;
	VBuffer = nullptr;
	cBuffer = nullptr;
	DSSObjType = ParClass->DSSClassType; //REG_CONTROL;
	InitPropertyValues(0);
	FInversetime = false;
	RegulatedBus = "";
	Vlimit = 0.0;
	ControlActionHandle = 0;

   //  RecalcElementData;
}

TRegControlObj::~TRegControlObj()
{
	ElementName = "";
	if( VBuffer != nullptr)
		delete[] VBuffer;
	if( cBuffer != nullptr)
		delete[] cBuffer;
	// inherited::Destroy();
}


/*--------------------------------------------------------------------------*/

void TRegControlObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	String TransName;
	String NewElementName;
	if((R != 0.0) || (X != 0.0) || (LDC_Z > 0.0))
		LDCActive = true;
	else
		LDCActive = false;
	if(RegulatedBus.size() == 0)
		UsingRegulatedBus = false;
	else
		UsingRegulatedBus = true;
	DevIndex = GetCktElementIndex(ElementName); // Global FUNCTION
	if(DevIndex == 0) // Try 'AutoTrans' instead of Transformer
	{
		TransName = StripClassName(ElementName);
		NewElementName = String("autotrans.") + TransName;
		DevIndex = GetCktElementIndex(NewElementName);
		if(DevIndex > 0)
			ElementName = NewElementName;
	}
	if(DevIndex > 0)  // RegControled element must already exist
	{
		Set_ControlledElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
		Set_MonitoredElement(get_FControlledElement());  // same for this controller
		if(UsingRegulatedBus)
		{
			Set_NPhases(1);     // Only need one phase
			Set_Nconds(2);
		}
		else
		{
			Set_NPhases(get_FControlledElement()->Get_NPhases());
			Set_Nconds(Fnphases);
			if(FPTPhase > Fnphases)
			{
				FPTPhase = 1;
				Set_PropertyValue(22,"1");
			}
		}
		if((CompareText( get_FControlledElement()->Get_myPName(), "transformer") == 0) || (CompareText( get_FControlledElement()->Get_myPName(), "autotrans") == 0))  // either should work
		{
			if(ElementTerminal > get_FControlledElement()->Get_NTerms())
			{
				DoErrorMsg(String("RegControl: \"") + get_Name() + "\"", "Winding no. \"" "\" does not exist.", "Respecify Monitored Winding no.", 122);
			}
			else

                       // Sets name of i-th terminal's connected bus in RegControl's buslist
                       // This value will be used to set the NodeRef array (see Sample function)
			{
				if(UsingRegulatedBus)   // hopefully this will actually exist
					SetBus(1, RegulatedBus);
				else
					SetBus(1, get_FControlledElement()->GetBus(ElementTerminal));
				VBuffer = new complex[ get_FControlledElement()->Get_NPhases() + 1]; // buffer to hold regulator voltages
				cBuffer = new complex[ get_FControlledElement()->Yorder + 1 ];
			}
		}
		else
		{
			Set_ControlledElement(nullptr);   // we get here if element not found
			DoErrorMsg(String("RegControl: \"") + this->get_Name() + "\"", String("Controlled Regulator Element \"") + ElementName
	           + "\" Is not a transformer.", " Element must be defined previously.", 123);
		}
	}
	else
	{
		Set_ControlledElement(nullptr);   // element not found
		DoErrorMsg(String("RegControl: \"") + this->get_Name() + "\"", String("Transformer Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 124);
	}
}

/*--------------------------------------------------------------------------*/

void TRegControlObj::CalcYPrim(int ActorID)
{

  // leave YPrim as nil and it will be ignored ... zero current source
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}






/*--------------------------------------------------------------------------*/

complex TRegControlObj::GetControlVoltage(pComplexArray VBuffer, int Nphs, double PTRatio)
{
	complex result = {};
	int i = 0;
	double V = 0.0;
	switch(FPTPhase)
	{
		case 	MAXPHASE:
/*
         AVGPHASES: Begin
                        Result := CZERO;
                        FOR i := 1 to Nphs Do Result := Result + Cabs(VBuffer^[i]);
                        Result := CdivReal(Result, (Nphs*PTRatio));
                    End;

*/
		{
			int stop = 0;
			ControlledPhase = 1;
			V = cabs((VBuffer)[ControlledPhase - 1]);
			for(stop = Nphs, i = 2; i <= stop; i++)
			{
				if(cabs((VBuffer)[i - 1]) > V)
				{
					V = cabs((VBuffer)[i - 1]);
					ControlledPhase = i;
				}
			}
			result = cdivreal((VBuffer)[ControlledPhase - 1], PTRatio);
		}
		break;
		case 	MINPHASE:
		{
			int stop = 0;
			ControlledPhase = 1;
			V = cabs((VBuffer)[ControlledPhase - 1]);
			for(stop = Nphs, i = 2; i <= stop; i++)
			{
				if(cabs((VBuffer)[i - 1]) < V)
				{
					V = cabs((VBuffer)[i - 1]);
					ControlledPhase = i;
				}
			}
			result = cdivreal((VBuffer)[ControlledPhase - 1], PTRatio);
		}
		break;
    /*Just use one phase because that's what most controls do.*/
		default:
		result = cdivreal((VBuffer)[FPTPhase - 1], PTRatio);
		ControlledPhase = FPTPhase;
		break;
	}
	return result;
}

void TRegControlObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TRegControlObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------*/

String TRegControlObj::GetPropertyValue(int Index)
{
	String result;
	switch(Index)
	{
		case 	28:
		result = Format("%d", Get_TapNum());
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

/*--------------------------------------------------------------------------*/

void TRegControlObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}

     // Note: The PropertyValue access function calls GetPropertyValue routine.
	if(Complete)
	{
		{ Write(f, "! Bus ="); WriteLn(f, GetBus(1)); }
		WriteLn(f);
	}
}

/*--------------------------------------------------------------------------*/

// Called in STATIC mode
// Changes 70% of the way but at least one tap, subject to maximum allowable tap change

double TRegControlObj::AtLeastOneTap(double ProposedChange, double Increment, int ActorID)
{
	double result = 0.0;
	int NumTaps = 0;
	NumTaps = Trunc(0.7 * Abs(ProposedChange) / Increment);
	if(NumTaps == 0)
		NumTaps = 1;
	if(NumTaps > TapLimitPerChange)
		NumTaps = TapLimitPerChange;
	LastChange[ActorID] = NumTaps;
	if(ProposedChange > 0.0)    // check sign on change
		result = NumTaps * Increment;
	else
	{
		result = -NumTaps * Increment;
		LastChange[ActorID] = -NumTaps;
	}
	return result;
}


/*--------------------------------------------------------------------------*/

double OneInDirectionOf(double& ProposedChange, double Increment, int ActorID)
{
	double result = 0.0;
	LastChange[ActorID] = 0;
	if(ProposedChange > 0.0)
	{
		result = Increment;
		LastChange[ActorID] = 1;
		ProposedChange = ProposedChange - Increment;
	}
	else
	{
		result = -Increment;
		LastChange[ActorID] = -1;
		ProposedChange = ProposedChange + Increment;
	}
	if(Abs(ProposedChange) < 0.9 * Increment)
		ProposedChange = 0.0;
	return result;
}

// Computes the amount of one tap change in the direction of the pending tapchange
// Automatically decrements the proposed change by that amount


/*--------------------------------------------------------------------------*/

// 2-23-00 Modified to change one tap at a time

void TRegControlObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	double TapChangeToMake = 0.0;
	switch(Code)
	{
		case 	ACTION_TAPCHANGE:
		{
			if(DebugTrace)
				/*# with ActiveCircuit[ActorID] do */
				{
					
					RegWriteDebugRecord(Format("+++ %.6g s: Handling TapChange = %.8g", ActiveCircuit[ActorID]->Solution->DynaVars.T, get_FPendingTapChange()));
				}
			if(get_FPendingTapChange() == 0.0)  /*Check to make sure control has not reset*/
				Armed = false;
			else
				/*# with TTransfObj(ControlledElement) do */
				{
					auto with1 = ((TTransfObj*) get_FControlledElement());

                 // Transformer PresentTap property automatically limits tap
					/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
					{
						
						auto with3 = ActiveCircuit[ActorID]->Solution;
						switch(with3->ControlMode)
						{
							case 	CTRLSTATIC:
							{
								TapChangeToMake = AtLeastOneTap(get_FPendingTapChange(), with1->Get_TapIncrement(TapWinding), ActorID);
								if(DebugTrace)
									RegWriteTraceRecord(TapChangeToMake, ActorID);
								with1->Set_PresentTap(TapWinding,ActorID, with1->Get_PresentTap(TapWinding,ActorID) + TapChangeToMake);
								if(ShowEventLog)
									AppendToEventLog(String("Regulator.") + 
									get_FControlledElement()->get_Name(), Format(" Changed %d taps to %-.6g.", LastChange[ActorID], with1->Get_PresentTap(TapWinding,ActorID)), ActorID);
								set_PendingTapChange(0.0);  // Reset to no change.  Program will determine if another needed.
								Armed = false;
							}
							break;
							case 	EVENTDRIVEN:
							{
								TapChangeToMake = OneInDirectionOf(FPendingTapChange, with1->Get_TapIncrement(TapWinding), ActorID);
								if(DebugTrace)
									RegWriteTraceRecord(TapChangeToMake, ActorID);
								with1->Set_PresentTap(TapWinding,ActorID,with1->Get_PresentTap(TapWinding,ActorID) + TapChangeToMake);
								if(get_FPendingTapChange() != 0.0)
									ActiveCircuit[ActorID]->ControlQueue.Push(with3->DynaVars.intHour, with3->DynaVars.T + TapDelay, (EControlAction) 0, 0, this, ActorID);
								else
									Armed = false;
							}
							break;
							case 	TIMEDRIVEN:
							{
								TapChangeToMake = OneInDirectionOf(FPendingTapChange, with1->Get_TapIncrement(TapWinding), ActorID);
								if(DebugTrace)
									RegWriteTraceRecord(TapChangeToMake, ActorID);
								with1->Set_PresentTap(TapWinding,ActorID,with1->Get_PresentTap(TapWinding,ActorID) + TapChangeToMake);
								if(ShowEventLog)
									AppendToEventLog(String("Regulator.") + 
									get_FControlledElement()->get_Name(), Format(" Changed %d tap to %-.6g.", LastChange[ActorID], with1->Get_PresentTap(TapWinding,ActorID)), ActorID);
								if(DebugTrace)
									RegWriteDebugRecord(Format("--- Regulator.%s Changed %d tap to %-.6g.", 
									get_FControlledElement()->get_Name().c_str(), LastChange[ActorID], with1->Get_PresentTap(TapWinding,ActorID)));
								if(get_FPendingTapChange() != 0.0)
									ActiveCircuit[ActorID]->ControlQueue.Push(with3->DynaVars.intHour, with3->DynaVars.T + TapDelay, (EControlAction) 0, 0, this, ActorID);
								else
									Armed = false;
							}
							break;
							case 	MULTIRATE:
							{
								TapChangeToMake = OneInDirectionOf(FPendingTapChange, with1->Get_TapIncrement(TapWinding), ActorID);
								if(DebugTrace)
									RegWriteTraceRecord(TapChangeToMake, ActorID);
								with1->Set_PresentTap(TapWinding,ActorID,with1->Get_PresentTap(TapWinding,ActorID) + TapChangeToMake);
								if(ShowEventLog)
									AppendToEventLog(String("Regulator.") + 
									get_FControlledElement()->get_Name(), Format(" Changed %d tap to %-.6g.", LastChange[ActorID], with1->Get_PresentTap(TapWinding,ActorID)), ActorID);
								if(DebugTrace)
									RegWriteDebugRecord(Format("--- Regulator.%s Changed %d tap to %-.6g.", 
									get_FControlledElement()->get_Name().c_str(), LastChange[ActorID], with1->Get_PresentTap(TapWinding,ActorID)));
								if(get_FPendingTapChange() != 0.0)
									ActiveCircuit[ActorID]->ControlQueue.Push(with3->DynaVars.intHour, with3->DynaVars.T + TapDelay, (EControlAction) 0, 0, this, ActorID);
								else
									Armed = false;
							}
							break;
							default:
							  ;
							break;
						}
					}
				}
		}
		break;  /*ACTION_TAPCHANGE*/  // Toggle reverse mode or Cogen mode flag
		case 	ACTION_REVERSE:
		{
			if(DebugTrace)
				/*# with ActiveCircuit[ActorID] do */
				{
					
					RegWriteDebugRecord( Format( "%-.6g, Handling Reverse Action, ReversePending=",  ActiveCircuit[ActorID]->Solution->DynaVars.dblHour) +
						BoolToStr(ReversePending, true) + ", InReverseMode =" + 
						BoolToStr(InReverseMode, true) + ", InCogenmode = " + BoolToStr(InCogenMode, true) );
				}
			if(ReversePending)        // check to see if action has reset
			{
				if(CogenEnabled)   // Cogen mode takes precedence if present
				{
					if(InCogenMode)
						InCogenMode = false;
					else
						InCogenMode = true;
				}
				else
				{
					if(InReverseMode)
						InReverseMode = false;
					else
						InReverseMode = true;
				}
				ReversePending = false;
			}
		}
		break;  /*ACTION_REVERSE*/
		default:
		  ;
		break;
	}
}

/*This is where it all happens ...*/

void TRegControlObj::sample(int ActorID)
{
	double BoostNeeded = 0.0;
	double Increment = 0.0;
	double Vactual = 0.0;
	double VregTest = 0.0;
	double BandTest = 0.0;
	double Vboost = 0.0;
	double VlocalBus = 0.0;
	double FwdPower = 0.0;
	complex Vcontrol = {};
	complex VLDC = {};
	complex ILDC = {};
	bool TapChangeIsNeeded = false;
	bool LookingForward = false;
	int i = 0;
	int II = 0;
	TTransfObj* ControlledTransformer = nullptr;
	int TransformerConnection = 0;
	ControlledTransformer = ((TTransfObj*) get_FControlledElement());
	if(TapLimitPerChange == 0)
	{
		set_PendingTapChange(0);
		return;
	}
	LookingForward = (!InReverseMode) || InCogenMode; // Always looking forward in cogen mode

     /*Get_First(), check the direction of power flow to see if we need to reverse direction*/
     /*Don't do this if using regulated bus logic*/
	if(!UsingRegulatedBus)
	{
		if(IsReversible || CogenEnabled)
		{
			if(DebugTrace)
				/*# with ActiveCircuit[ActorID] do */
				{
					
					RegWriteDebugRecord( Format( "%-.6g, 2-Looking forward= ", ActiveCircuit[ActorID]->Solution->DynaVars.dblHour) + 
						BoolToStr(LookingForward, true) + " *** Incogenmode=" + BoolToStr(InCogenMode, true) );
				}
			if(LookingForward && (!InCogenMode))   // If looking forward, check to see if we should reverse
			{
				FwdPower = -ControlledTransformer->Get_Power(ElementTerminal, ActorID).re;  // watts
				if(!ReversePending)  // If reverse is already pending, don't send any more messages
				{
					if(FwdPower <  - RevPowerThreshold)
					{
						ReversePending = true;
						/*# with ActiveCircuit[ActorID] do */
						{
							
							RevHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + revDelay, ACTION_REVERSE, 0, this, ActorID);
						}
						if(DebugTrace)
							/*# with ActiveCircuit[ActorID] do */
							{
								
								RegWriteDebugRecord(Format("%-.6g, 1- Pushed Reverse Action, Handle=%d, FwdPower=%.8g", 
								ActiveCircuit[ActorID]->Solution->DynaVars.dblHour, RevHandle, FwdPower));
							}
					}
				}
				if(ReversePending && (FwdPower >=  - RevPowerThreshold)) // Reset  reverse pending
				{
					ReversePending = false; // Reset it if power goes back
					if(RevHandle > 0)
					{
						if(DebugTrace)
							RegWriteDebugRecord(Format("Deleting Reverse Action, Handle=%d", RevHandle));
						ActiveCircuit[ActorID]->ControlQueue.Delete(RevHandle, ActorID);
						RevHandle = 0;   // reset for next time
					}
				}
			}
			else
      // Looking the reverse direction or in cogen mode
   // If reversed look to see if power is back in forward direction
			{
				if(DebugTrace)
					/*# with ActiveCircuit[ActorID] do */
					{
						
						RegWriteDebugRecord( Format( "%-.6g, 3-Looking Forward=", ActiveCircuit[ActorID]->Solution->DynaVars.dblHour) + 
							BoolToStr(LookingForward, true) + " % s * **Incogenmode = % s" + BoolToStr(InCogenMode, true) );
					}
				FwdPower = -ControlledTransformer->Get_Power(ElementTerminal, ActorID).re;  // watts
				if(!ReversePending)
				{
					if(FwdPower > RevPowerThreshold)
					{
						ReversePending = true;
						/*# with ActiveCircuit[ActorID] do */
						{
							
							RevBackHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + revDelay, ACTION_REVERSE, 0, this, ActorID);
						}
						if(DebugTrace)
							/*# with ActiveCircuit[ActorID] do */
							{
								
								RegWriteDebugRecord(Format("%-.6g, 4-Pushed ReverseBack Action to switch back, Handle=%d, FwdPower=%.8g", 
								ActiveCircuit[ActorID]->Solution->DynaVars.dblHour, RevBackHandle, FwdPower));
							}
					}
				}
				if(ReversePending && (FwdPower <= RevPowerThreshold)) // Reset  reverse pending                            Else
				{
					ReversePending = false; // Reset it if power goes back
					if(RevBackHandle > 0)
					{
						if(DebugTrace)
							RegWriteDebugRecord(Format("Deleting ReverseBack Action, Handle=%d", RevBackHandle));
						ActiveCircuit[ActorID]->ControlQueue.Delete(RevBackHandle, ActorID);
						RevBackHandle = 0;   // reset for next time
					}
				}

                  /*Check for special case of Reverse Neutral where regulator is to move to neutral position*/
                  /*Both Cogen Mode and Reverse operation*/
				/*# with ControlledTransformer do */
				{
					auto with6 = ControlledTransformer;
					if(ReverseNeutral)
					{
						if(!Armed)
						{
							set_PendingTapChange(0.0);
							if(Abs(with6->Get_PresentTap(TapWinding,ActorID) - 1.0) > EPSILON)
							{
								Increment = with6->Get_TapIncrement(TapWinding);
								set_PendingTapChange(Round((1.0 - with6->Get_PresentTap(TapWinding,ActorID)) / Increment) * Increment);
								if((get_FPendingTapChange() != 0.0) && !Armed)
									/*# with ActiveCircuit[ActorID] do */
									{
										
										if(DebugTrace)
											RegWriteDebugRecord(Format("*** %.6g s: Pushing TapChange = %.8g, delay= %.8g", 
											ActiveCircuit[ActorID]->Solution->DynaVars.T, get_FPendingTapChange(), TapDelay));
										ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TapDelay, ACTION_TAPCHANGE, 0, this, ActorID);
										Armed = true;
									}
							}
						}
						return;
					}
				}
			} /*Else*/
		}
	}
	if(UsingRegulatedBus)
	{
		int stop = 0;
		TransformerConnection = ControlledTransformer->WINDING_[ElementTerminal - 1].Connection;
		ComputeVterminal(ActorID);   // Computes the voltage at the bus being regulated
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			switch(TransformerConnection)
			{
				case 	0:      // Wye
				{
					(VBuffer)[i - 1] = (Vterminal)[i - 1];
				}
				break;   // Delta
				case 	1:
				{
					II = ControlledTransformer->RotatePhases(i);      // Get next phase in sequence using Transformer Obj rotate
					(VBuffer)[i - 1] = csub((Vterminal)[i - 1], (Vterminal)[II - 1]);
				}
				break;
				default:
				  ;
				break;
			}
		}
		Vcontrol = GetControlVoltage(VBuffer, Fnphases, RemotePTRatio);
	}
	else
	{
		ControlledTransformer->GetWindingVoltages(ElementTerminal, VBuffer, ActorID);
		Vcontrol = GetControlVoltage(VBuffer, Fnphases, PTRatio);
	}

     // Check Vlimit
	if(VLimitActive)
	{
		if(UsingRegulatedBus)
		{
			ControlledTransformer->GetWindingVoltages(ElementTerminal, VBuffer, ActorID);
			VlocalBus = cabs(cdivreal((VBuffer)[1 - 1], PTRatio));
		}
		else
		{
			VlocalBus = cabs(Vcontrol);
		}
	}
	else
	VlocalBus = 0.0; // to get rid of warning message;

     // Check for LDC
	if(!UsingRegulatedBus && LDCActive)
	{
		get_FControlledElement()->GetCurrents(cBuffer, ActorID);
        // Convert current to control current by CTRating
		ILDC = cdivreal((cBuffer)[get_FControlledElement()->Get_NConds() * (ElementTerminal - 1) + ControlledPhase - 1], CTRating);
		if(LDC_Z == 0.0)  // Standard R, X LDC
		{
			if(InReverseMode || InCogenMode)
				VLDC = cmul(cmplx(revR, revX), ILDC);
			else
				VLDC = cmul(cmplx(R, X), ILDC);
			Vcontrol = cadd(Vcontrol, VLDC);   // Direction on ILDC is INTO terminal, so this is equivalent to Vterm - (R+jX)*ILDC
		}
		else
 // Beckwith LDC_Z control mode
		{
			if(InReverseMode || InCogenMode)
				Vcontrol = cmplx((cabs(Vcontrol) - cabs(ILDC) * revLDC_Z), 0.0);
			else
				Vcontrol = cmplx((cabs(Vcontrol) - cabs(ILDC) * LDC_Z), 0.0);   // Just magnitudes
		}
	}
	Vactual = cabs(Vcontrol);   // Assumes looking forward; see below
	/*# with ControlledTransformer do */
	{
		auto with8 = ControlledTransformer;
         // Check for out of band voltage
		if(InReverseMode)
		{
			Vactual = Vactual / with8->Get_PresentTap(TapWinding,ActorID);
			VregTest = revVreg;
			BandTest = revBandwidth;
		}
		else
   // Forward or Cogen Modes
		{
			if(InCogenMode)
			{
				VregTest = revVreg;    // corrected Feb 25, 2021 for Huijuan Li
				BandTest = revBandwidth;
			}
			else
			{
				VregTest = Vreg;
				BandTest = Bandwidth;
			}
		}
		if(Abs(VregTest - Vactual) > (BandTest / 2.0))
			TapChangeIsNeeded = true;
		else
			TapChangeIsNeeded = false;
		if(VLimitActive)
		{
			if(VlocalBus > Vlimit)
				TapChangeIsNeeded = true;
		}
		if(TapChangeIsNeeded)
                // Compute tapchange
		{
			Vboost = (VregTest - Vactual);
			if(VLimitActive)
			{
				if(VlocalBus > Vlimit)
					Vboost = (Vlimit - VlocalBus);
			}
			BoostNeeded = Vboost * PTRatio / with8->Get_BaseVoltage(ElementTerminal);  // per unit Winding boost needed
			Increment = with8->Get_TapIncrement(TapWinding);
			set_PendingTapChange(Round(BoostNeeded / Increment) * Increment);  // Make sure it is an even increment

                /*If Tap is another winding or in REVERSE MODE, it has to move the other way to accomplish the change*/
			if((TapWinding != ElementTerminal) || InReverseMode)
				set_PendingTapChange(-get_FPendingTapChange());

                // Send Initial Tap Change message to control queue
                // Add Delay time to solution control queue
			if((get_FPendingTapChange() != 0.0) && !Armed)
                     // Now see if any tap change is possible in desired direction  Else ignore
			{
				if(get_FPendingTapChange() > 0.0)
				{
					if(with8->Get_PresentTap(TapWinding,ActorID) < with8->Get_MaxTap(TapWinding))
						/*# with ActiveCircuit[ActorID] do */
						{
							auto with100 = ActiveCircuit[ActorID];
							ControlActionHandle = with100->ControlQueue.Push(with100->Solution->DynaVars.intHour, with100->Solution->DynaVars.T + ComputeTimeDelay(Vactual), ACTION_TAPCHANGE, 0, this, ActorID);
							Armed = true;  // Armed to change taps
						}
				}
				else
				{
					if(with8->Get_PresentTap(TapWinding,ActorID) > with8->Get_MinTap(TapWinding))
						/*# with ActiveCircuit[ActorID] do */
						{
							
							ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ComputeTimeDelay(Vactual), ACTION_TAPCHANGE, 0, this, ActorID);
							Armed = true;  // Armed to change taps
						}
				}
			} /*If TapChangeIsNeeded*/
		}
		else
 /*Reset if back in band.*/
		{
			set_PendingTapChange(0.0);
			if(Armed)
			{
				ActiveCircuit[ActorID]->ControlQueue.Delete(ControlActionHandle, ActorID);
				Armed = false;
				ControlActionHandle = 0;
			}
		}
	}
}

TTransfObj* TRegControlObj::Get_Transformer()
{
	TTransfObj* result = nullptr;
	result = ((TTransfObj*) get_FControlledElement());
	return result;
}

int TRegControlObj::Get_Winding()
{
	int result = 0;
	result = TapWinding;
	return result;
}

int TRegControlObj::Get_TapNum()
{
	int result = 0;
	TTransfObj* ctrldTransformer = nullptr;
	int ictrldWinding = 0;
	if(get_FControlledElement() != nullptr)
	{
		ctrldTransformer = Get_Transformer();
		ictrldWinding = Get_Winding();
		/*# with ctrldTransformer do */
		{
			auto with0 = ctrldTransformer;
			result = (int) Round((with0->Get_PresentTap(ictrldWinding,ActiveActor) - (with0->Get_MaxTap(ictrldWinding) + with0->Get_MinTap(ictrldWinding)) / 2.0) / with0->Get_TapIncrement(ictrldWinding));
		}
	}
	else
	result = 0;
	return result;
}

double TRegControlObj::Get_MinTap()
{
	double result = 0.0;
	result = Get_Transformer()->Get_MinTap(TapWinding);
	return result;
}

double TRegControlObj::Get_MaxTap()
{
	double result = 0.0;
	result = Get_Transformer()->Get_MaxTap(TapWinding);
	return result;
}

double TRegControlObj::Get_TapIncrement()
{
	double result = 0.0;
	result = Get_Transformer()->Get_TapIncrement(TapWinding);
	return result;
}

int TRegControlObj::Get_NumTaps()
{
	int result = 0;
	result = Get_Transformer()->Get_NumTaps(TapWinding);
	return result;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_FPendingTapChange()
{
	return FPendingTapChange;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_Vreg()
{
	return Vreg;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_Bandwidth()
{
	return Bandwidth;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_CTRating()
{
	return CTRating;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_PTRatio()
{
	return PTRatio;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_R()
{
	return R;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_X()
{
	return X;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_revR()
{
	return revR;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_revX()
{
	return revX;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_revVreg()
{
	return revVreg;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_revBandwidth()
{
	return revBandwidth;
}

//--------------------------------------------------------------------------

bool TRegControlObj::get_LDCActive()
{
	return LDCActive;
}

//--------------------------------------------------------------------------

bool TRegControlObj::get_IsReversible()
{
	return IsReversible;
}

//--------------------------------------------------------------------------

bool TRegControlObj::get_VLimitActive()
{
	return VLimitActive;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_Vlimit()
{
	return Vlimit;
}

//--------------------------------------------------------------------------

double TRegControlObj::get_TapDelay()
{
	return TapDelay;
}

//--------------------------------------------------------------------------

int TRegControlObj::get_TapLimitPerChange()
{
	return TapLimitPerChange;
}

//--------------------------------------------------------------------------

bool TRegControlObj::get_FInversetime()
{
	return FInversetime;
}

//--------------------------------------------------------------------------

// write a general debug string

void TRegControlObj::RegWriteDebugRecord(String s)
{
	try
	{
		if(!InShowResults)
		{
			Append(Tracefile);
			IOResultToException();
			WriteLn(Tracefile, s);
			CloseFile(Tracefile);
		}
	}
	catch (...)
	{
	}
}

void TRegControlObj::RegWriteTraceRecord(double TapChangeMade, int ActorID)
{
	String Separator;
	try
	{
		if(!InShowResults)
		{
			Separator = ", ";
			Append(Tracefile);
			IOResultToException();
			/*# with TTransfObj(ControlledElement) do */
			{
				auto with0 = ((TTransfObj*) get_FControlledElement());
				{ 
					Write(Tracefile, ActiveCircuit[ActorID]->Solution->DynaVars.intHour, 0); 
					Write(Tracefile, Separator); 
					Write(Tracefile, ActiveCircuit[ActorID]->Solution->DynaVars.T, 0, 3); 
					Write(Tracefile, Separator); 
					Write(Tracefile, ActiveCircuit[ActorID]->Solution->ControlIteration, 0); 
					Write(Tracefile, Separator); 
					Write(Tracefile, ActiveCircuit[ActorID]->Solution->Iteration, 0); 
					Write(Tracefile, Separator); 
					Write(Tracefile, ActiveCircuit[ActorID]->get_FLoadMultiplier(), 6, 2); 
					Write(Tracefile, Separator); 
					Write(Tracefile, with0->Get_PresentTap(ElementTerminal,ActorID), 8, 5);
					Write(Tracefile, Separator); 
					Write(Tracefile, get_FPendingTapChange(), 8, 5);
					Write(Tracefile, Separator); 
					Write(Tracefile, TapChangeMade, 8, 5); 
					Write(Tracefile, Separator); 
					Write(Tracefile, with0->Get_TapIncrement(ElementTerminal), 8, 5);
					Write(Tracefile, Separator); 
					Write(Tracefile, with0->Get_MinTap(ElementTerminal), 8, 5);
					Write(Tracefile, Separator); 
					WriteLn(Tracefile, with0->Get_MaxTap(ElementTerminal), 8, 5);
				}
			}
			CloseFile(Tracefile);
		}
	}
	catch (...)
	{
	} /*Do Nothing*/
}

void TRegControlObj::Reset(int ActorID)
{
	set_PendingTapChange(0.0);
	Armed = false;
}
/*Override standard SaveWrite*/
/*Regcontrol structure not conducive to standard means of saving*/

void TRegControlObj::SaveWrite(TTextRec& f)
{
	int iProp = 0;
   /*Write only properties that were explicitly set in the
   final order they were actually set*/

   // Write Transformer name out first so that it is set for later operations
	iProp = 1;
	if(Length(String(Get_PropertyValue(iProp))) > 0)
		/*# with ParentClass do */
		{
			auto with0 = ParentClass;
			String PropValue = "=" + CheckForBlanks(String(Get_PropertyValue(iProp)) );
			Write( f, with0->PropertyName[with0->RevPropertyIdxMap[iProp - 1] - 1] + PropValue );
		}
	iProp = GetNextPropertySet(0); // Works on ActiveDSSObject
	while(iProp > 0)
		/*# with ParentClass do */
		{
			auto with1 = ParentClass;
			if(iProp != 1)
			{
				if(Length(String(Get_PropertyValue(iProp))) > 0)   // Don't repeat Transformer property
				{
					String PropValue = "=" + CheckForBlanks(String(Get_PropertyValue(iProp)) );
					Write( f, " " + with1->PropertyName[(with1->RevPropertyIdxMap)[iProp - 1] - 1] + PropValue);
				}
			}
			iProp = GetNextPropertySet(iProp);
		}
}

void TRegControlObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,""); //'element';
	Set_PropertyValue(2,"1"); //'terminal';
	Set_PropertyValue(3,"120");
	Set_PropertyValue(4,"3");
	Set_PropertyValue(5,"60");
	Set_PropertyValue(6,"300");
	Set_PropertyValue(7,"0");
	Set_PropertyValue(8,"0");
	Set_PropertyValue(9,"");
	Set_PropertyValue(10,"15");
	Set_PropertyValue(11,"no");
	Set_PropertyValue(12,"120");
	Set_PropertyValue(13,"3");
	Set_PropertyValue(14,"0");
	Set_PropertyValue(15,"0");
	Set_PropertyValue(16,"2");
	Set_PropertyValue(17,"no");
	Set_PropertyValue(18,"16");
	Set_PropertyValue(19,"no");
	Set_PropertyValue(20,"1");
	Set_PropertyValue(21,"0.0");
	Set_PropertyValue(22,"1");
	Set_PropertyValue(23,"100");
	Set_PropertyValue(24,"60");
	Set_PropertyValue(25,"No");
	Set_PropertyValue(26, ShowEventLog ? "YES" : "NO");
	Set_PropertyValue(27,"60");
	Set_PropertyValue(28,"0");
	Set_PropertyValue(29,"NO");
	Set_PropertyValue(30,"0");
	Set_PropertyValue(31,"0");
	Set_PropertyValue(32,"No");
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TRegControlObj::set_PendingTapChange(double Value)
{
	FPendingTapChange = Value;
	DblTraceParameter = Value;
}

void TRegControlObj::Set_TapNum(int Value)
{
	TTransfObj* ctrldTransformer = nullptr;
	int ictrldWinding = 0;
	if(!ASSIGNED(get_FControlledElement()))
		RecalcElementData(ActiveActor);
	if(get_FControlledElement() != nullptr)
	{
		ctrldTransformer = ((TTransfObj*) get_FControlledElement());
		ictrldWinding = Get_Winding();
		/*# with ctrldTransformer do */
		{
			auto with0 = ctrldTransformer;
			with0->Set_PresentTap(ictrldWinding,ActiveActor,Value * with0->Get_TapIncrement(ictrldWinding) + ((with0->Get_MaxTap(ictrldWinding) + with0->Get_MinTap(ictrldWinding)) / 2.0));
		}

// Tap range checking is done in PresentTap
// You can attempt to set the tap at an illegal value but it won't do anything
	}
}

void TRegControlObj::MakePosSequence(int ActorID)
{
	if(get_FControlledElement() != nullptr)
	{
		Set_Enabled(get_FControlledElement()->Get_Enabled());
		if(UsingRegulatedBus)
			Set_NPhases(1);
		else
			Set_NPhases(get_FControlledElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		if((CompareText( ( get_FControlledElement() )->Get_myPName(), "transformer") == 0) || (CompareText( ( get_FControlledElement() )->Get_myPName(), "autotrans") == 0))   // either should work

        // Sets name of i-th terminal's connected bus in RegControl's buslist
        // This value will be used to set the NodeRef array (see Sample function)
		{
			if(UsingRegulatedBus)   // hopefully this will actually exist
				SetBus(1, RegulatedBus);
			else
				SetBus(1, get_FControlledElement()->GetBus(ElementTerminal));
			VBuffer = (pComplexArray)realloc(VBuffer, sizeof(complex) * get_FControlledElement()->Get_NPhases());  // buffer to hold regulator voltages
			cBuffer = (pComplexArray)realloc(cBuffer, sizeof(complex) * get_FControlledElement()->Yorder); 
		}
	}
	inherited::MakePosSequence(ActorID);
}

double TRegControlObj::ComputeTimeDelay(double Vavg)
{
	double result = 0.0;
	if(FInversetime)
		result = TimeDelay / min(10.0, (2.0 * Abs(Vreg - Vavg) / Bandwidth));
	else
		result = TimeDelay;
	return result;
}




}  // namespace RegControl





