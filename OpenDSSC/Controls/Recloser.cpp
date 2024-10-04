
#pragma hdrstop

#include "Recloser.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Ucmatrix.h"
#include "mathutil.h"
#include "Utilities.h"

using namespace std;
using namespace Arraydef;
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
using namespace TCC_Curve;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace Recloser
{

TRecloserObj::TRecloserObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TRecloserObj::TRecloserObj(String ClassName) : inherited(ClassName) {}
TRecloserObj::TRecloserObj() {}


TRecloserObj* ActiveRecloserObj = nullptr;
TRecloser* RecloserClass = nullptr;
const int NumPropsThisClass = 24;
const int Current = 0;  /*Default*/
const int VOLTAGE = 1;
const int REVPOWER = 3;
TDSSClass* TCC_CurveClass = nullptr;

/*General Module Function*/

TTCC_CurveObj* GetTccCurve(const String CurveName)
{
	TTCC_CurveObj* result = nullptr;
	result = ((TTCC_CurveObj*) TCC_CurveClass->Find(CurveName));
	if(result == nullptr)
		DoSimpleMsg(String("TCC Curve object: \"") + CurveName + "\" not found.", 388);
	return result;
}


/*--------------------------------------------------------------------------*/  // Creates superstructure for all Recloser objects

TRecloser::TRecloser()
{
	;
	Class_Name = "Recloser";
	DSSClassType = DSSClassType + RECLOSER_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	TCC_CurveClass = (TDSSClass*)GetDSSClassPtr("TCC_Curve");
	RecloserClass = this;
}

/*--------------------------------------------------------------------------*/

TRecloser::~TRecloser()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TRecloser::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "MonitoredObj";
	PropertyName[2 - 1] = "MonitoredTerm";
	PropertyName[3 - 1] = "SwitchedObj";
	PropertyName[4 - 1] = "SwitchedTerm";
	PropertyName[5 - 1] = "NumFast";
	PropertyName[6 - 1] = "PhaseFast";
	PropertyName[7 - 1] = "PhaseDelayed";
	PropertyName[8 - 1] = "GroundFast";
	PropertyName[9 - 1] = "GroundDelayed";
	PropertyName[10 - 1] = "PhaseTrip";
	PropertyName[11 - 1] = "GroundTrip";
	PropertyName[12 - 1] = "PhaseInst";
	PropertyName[13 - 1] = "GroundInst";
	PropertyName[14 - 1] = "Reset";
	PropertyName[15 - 1] = "Shots";
	PropertyName[16 - 1] = "RecloseIntervals";
	PropertyName[17 - 1] = "Delay";
	PropertyName[18 - 1] = "Action";
	PropertyName[19 - 1] = "TDPhFast";
	PropertyName[20 - 1] = "TDGrFast";
	PropertyName[21 - 1] = "TDPhDelayed";
	PropertyName[22 - 1] = "TDGrDelayed";
	PropertyName[23 - 1] = "Normal";
	PropertyName[24 - 1] = "State";
	PropertyHelp[1 - 1] = "Full object name of the circuit element, typically a line, transformer, load, or generator, "
	           "to which the Recloser's PT and/or CT are connected."
	           " This is the \"monitored\" element. "
	           "There is no default; must be specified.";
	PropertyHelp[2 - 1] = "Number of the terminal of the circuit element to which the Recloser is connected. "
	           "1 or 2, typically.  Default is 1.";
	PropertyHelp[3 - 1] = "Name of circuit element switch that the Recloser controls. "
	           "Specify the full object name."
	           "Defaults to the same as the Monitored element. "
	           "This is the \"controlled\" element.";
	PropertyHelp[4 - 1] = "Number of the terminal of the controlled element in which the switch is controlled by the Recloser. "
	           "1 or 2, typically.  Default is 1.";
	PropertyHelp[5 - 1] = "Number of Fast (fuse saving) operations.  Default is 1. (See \"Shots\")";
	PropertyHelp[6 - 1] = "Name of the TCC Curve object that determines the Phase Fast trip.  Must have been previously defined as a TCC_Curve object."
	           " Default is \"A\". "
	           "Multiplying the current values in the curve by the \"phasetrip\" value gives the actual current.";
	PropertyHelp[7 - 1] = "Name of the TCC Curve object that determines the Phase Delayed trip.  Must have been previously defined as a TCC_Curve object."
	           " Default is \"D\"."
	           "Multiplying the current values in the curve by the \"phasetrip\" value gives the actual current.";
	PropertyHelp[8 - 1] = "Name of the TCC Curve object that determines the Ground Fast trip.  Must have been previously defined as a TCC_Curve object."
	           " Default is none (ignored). "
	           "Multiplying the current values in the curve by the \"groundtrip\" value gives the actual current.";
	PropertyHelp[9 - 1] = "Name of the TCC Curve object that determines the Ground Delayed trip.  Must have been previously defined as a TCC_Curve object."
	           " Default is none (ignored)."
	           "Multiplying the current values in the curve by the \"groundtrip\" value gives the actual current.";
	PropertyHelp[10 - 1] = "Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0.";
	PropertyHelp[11 - 1] = "Multiplier or actual ground amps (3I0) for the ground TCC curve.  Defaults to 1.0.";
	PropertyHelp[12 - 1] = "Actual amps for instantaneous phase trip which is assumed to happen in 0.01 sec + Delay Time. Default is 0.0, which signifies no inst trip. ";
	PropertyHelp[13 - 1] = "Actual amps for instantaneous ground trip which is assumed to happen in 0.01 sec + Delay Time.Default is 0.0, which signifies no inst trip.";
	PropertyHelp[14 - 1] = "Reset time in sec for Recloser.  Default is 15. ";
	PropertyHelp[15 - 1] = "Total Number of fast and delayed shots to lockout.  Default is 4. This is one more than the number of reclose intervals.";
	PropertyHelp[16 - 1] = "Array of reclose intervals.  Default for Recloser is (0.5, 2.0, 2.0) seconds. "
	           "A locked out Recloser must be closed manually (action=close).";
	PropertyHelp[17 - 1] = "Fixed delay time (sec) added to Recloser trip time. Default is 0.0. Used to represent breaker time or any other delay.";
	PropertyHelp[18 - 1] = "DEPRECATED. See \"State\" property";
	PropertyHelp[19 - 1] = "Time dial for Phase Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.";
	PropertyHelp[20 - 1] = "Time dial for Ground Fast trip curve. Multiplier on time axis of specified curve. Default=1.0.";
	PropertyHelp[21 - 1] = "Time dial for Phase Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.";
	PropertyHelp[22 - 1] = "Time dial for Ground Delayed trip curve. Multiplier on time axis of specified curve. Default=1.0.";
	PropertyHelp[23 - 1] = "{Open | Closed} Normal state of the recloser. The recloser reverts to this state for reset, change of mode, etc. "
	           "Defaults to \"State\" if not specificallt declared.";
	PropertyHelp[24 - 1] = "{Open | Closed} Actual state of the recloser. Upon setting, immediately forces state of the recloser, overriding the Recloser control. "
	           "Simulates manual control on recloser. Defaults to Closed. \"Open\" causes the controlled element to open and lock out. \"Closed\" causes the "
	           "controlled element to close and the recloser to reset to its first operation.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TRecloser::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new Recloser and add it to Recloser class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TRecloserObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}
/*--------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------*/

int TRecloser::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;

  // continue parsing WITH contents of Parser
	ActiveRecloserObj = (TRecloserObj*)  ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveRecloserObj);
	result = 0;
	/*# with ActiveRecloserObj do */
	{
		auto with0 = ActiveRecloserObj;
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
				with0->Set_PropertyValue(ParamPointer, Param);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 390);
				break;
				case 	1:
				with0->MonitoredElementName = LowerCase(Param);
				break;
				case 	2:
				with0->MonitoredElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->ElementName = LowerCase(Param);
				break;
				case 	4:
				with0->ElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	5:
				with0->NumFast = Parser[ActorID]->MakeInteger_();
				break;
				case 	6:
				with0->PhaseFast = GetTccCurve(Param);
				break;
				case 	7:
				with0->PhaseDelayed = GetTccCurve(Param);
				break;
				case 	8:
				with0->GroundFast = GetTccCurve(Param);
				break;
				case 	9:
				with0->GroundDelayed = GetTccCurve(Param);
				break;
				case 	10:
				with0->PhaseTrip = Parser[ActorID]->MakeDouble_();
				break;
				case 	11:
				with0->GroundTrip = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->PhaseInst = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->GroundInst = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->ResetTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				with0->NumReclose = Parser[ActorID]->MakeInteger_() - 1;
				break;   // one less than number of shots
				case 	16:
				with0->NumReclose = Parser[ActorID]->ParseAsVector(4, with0->RecloseIntervals);
				break;   // max of 4 allowed
				case 	17:
				with0->DelayTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	19:
				with0->TDPhFast = Parser[ActorID]->MakeDouble_();
				break;
				case 	20:
				with0->TDGrFast = Parser[ActorID]->MakeDouble_();
				break;
				case 	21:
				with0->TDPhDelayed = Parser[ActorID]->MakeDouble_();
				break;
				case 	22:
				with0->TDGrDelayed = Parser[ActorID]->MakeDouble_();
				break;
				case 	23:
				{
					with0->InterpretRecloserState(ActorID, Param, ParamName);   // set normal state
					if(!with0->NormalStateSet)
						with0->NormalStateSet = true;
				}
				break;
				case 	18: case 24:
				with0->InterpretRecloserState(ActorID, Param, ParamName);
				break;    // set state

           // Inherited parameters
				default:
				ClassEdit(ActiveRecloserObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	1:
              /*Default the controlled element to the monitored element*/
				with0->ElementName = with0->MonitoredElementName;
				break;
				case 	2:
				with0->ElementTerminal = with0->MonitoredElementTerminal;
				break;
				case 	18: case 24:
				if(!with0->NormalStateSet)
				{
					with0->NormalStateSet = true;  // normal state will default to state only the 1st state is specified.
					with0->set_NormalState(with0->FPresentState);
				}
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		with0->RecalcElementData(ActorID);
	}
	return result;
}



/*--------------------------------------------------------------------------*/

int TRecloser::MakeLike(const String RecloserName)
{
	int result = 0;
	TRecloserObj* OtherRecloser = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Recloser name in the present collection*/
	OtherRecloser = ((TRecloserObj*) Find(RecloserName));
	if(OtherRecloser != nullptr)
		/*# with ActiveRecloserObj do */
		{
			auto with0 = ActiveRecloserObj;
			int stop = 0;
			with0->Set_NPhases(OtherRecloser->Fnphases);
			with0->Set_Nconds(OtherRecloser->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherRecloser->ElementName;
			with0->ElementTerminal = OtherRecloser->ElementTerminal;
			with0->Set_ControlledElement(OtherRecloser->get_FControlledElement());  // Pointer to target circuit element
			with0->Set_MonitoredElement(OtherRecloser->get_FMonitoredElement());  // Pointer to target circuit element
			with0->MonitoredElementName = OtherRecloser->MonitoredElementName;  // Pointer to target circuit element
			with0->MonitoredElementTerminal = OtherRecloser->MonitoredElementTerminal;  // Pointer to target circuit element
			with0->PhaseDelayed = OtherRecloser->PhaseDelayed;
			with0->GroundDelayed = OtherRecloser->GroundDelayed;
			with0->PhaseFast = OtherRecloser->PhaseFast;
			with0->GroundFast = OtherRecloser->GroundFast;
			with0->PhaseTrip = OtherRecloser->PhaseTrip;
			with0->GroundTrip = OtherRecloser->GroundTrip;
			with0->PhaseInst = OtherRecloser->PhaseInst;
			with0->GroundInst = OtherRecloser->GroundInst;
			with0->ResetTime = OtherRecloser->ResetTime;
			with0->NumReclose = OtherRecloser->NumReclose;
			with0->NumFast = OtherRecloser->NumFast;
			with0->RecloseIntervals = (pDoubleArray) realloc(with0->RecloseIntervals, sizeof(double) * 4);      // Always make a max of 4
			for(stop = with0->NumReclose, i = 1; i <= stop; i++)
			{
				(with0->RecloseIntervals)[i - 1] = (OtherRecloser->RecloseIntervals)[i - 1];
			}
			with0->LockedOut = OtherRecloser->LockedOut;
			with0->FPresentState = OtherRecloser->FPresentState;
			with0->set_NormalState(OtherRecloser->get_NormalState());
			with0->CondOffset = OtherRecloser->CondOffset;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherRecloser->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in Recloser MakeLike: \"") + RecloserName
	           + "\" Not Found.", 391);
	return result;
}




/*==========================================================================*/
/*                    TRecloserObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TRecloserObj::TRecloserObj(TDSSClass* ParClass, const String RecloserName)
 : inherited(ParClass),
			PhaseDelayed(GetTccCurve("d")),
			GroundDelayed(nullptr),
			PhaseFast(nullptr),
			GroundFast(nullptr),
			ResetTime(0.0),
			DelayTime(0.0),
			TDGrDelayed(0.0),
			TDPhDelayed(0.0),
			TDGrFast(0.0),
			TDPhFast(0.0),
			OperationCount(0),
			LockedOut(false),
			ArmedForClose(false),
			ArmedForOpen(false),
			GroundTarget(false),
			PhaseTarget(false),
			NormalStateSet(false),
			CondOffset(0),
			cBuffer(nullptr),
			RecloseIntervals(nullptr),
			NumFast(0),
			NumReclose(0),
			MonitoredElementTerminal(0),
			PhaseTrip(0.0),
			GroundTrip(0.0),
			PhaseInst(0.0),
			GroundInst(0.0)
{
	Set_Name(LowerCase(RecloserName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	ElementName = "";
	Set_ControlledElement(nullptr);
	ElementTerminal = 1;
	MonitoredElementName = "";
	MonitoredElementTerminal = 1;
	Set_MonitoredElement(nullptr);
	PhaseFast = GetTccCurve("a");
	GroundFast = nullptr;
	PhaseTrip = 1.0;
	GroundTrip = 1.0;
	PhaseInst = 0.0;
	GroundInst = 0.0;
	TDGrDelayed = 1.0;
	TDPhDelayed = 1.0;
	TDGrFast = 1.0;
	TDPhFast = 1.0;
	ResetTime = 15.0;
	NumReclose = 3;
	NumFast = 1;
	RecloseIntervals = nullptr;
	RecloseIntervals = new double[4]; // fixed allocation of 4
	(RecloseIntervals)[1 - 1] = 0.5;
	(RecloseIntervals)[2 - 1] = 2.0;
	(RecloseIntervals)[3 - 1] = 2.0;
	FPresentState = CTRL_CLOSE;
	FNormalState = CTRL_CLOSE;
	NormalStateSet = false;
	OperationCount = 1;
	LockedOut = false;
	ArmedForOpen = false;
	ArmedForClose = false;
	GroundTarget = false;
	PhaseTarget = false;
	cBuffer = nullptr; // Complex buffer
	DSSObjType = ParClass->DSSClassType; //cap_CONTROL;
	InitPropertyValues(0);



   //  RecalcElementData;
}

TRecloserObj::~TRecloserObj()
{
	MonitoredElementName = "";
	free(RecloseIntervals);
	free(cBuffer);
	// inherited::Destroy();
}


/*--------------------------------------------------------------------------*/

void TRecloserObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	DevIndex = GetCktElementIndex(MonitoredElementName); // Global function
	if(DevIndex > 0)
	{
		Set_MonitoredElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
		Set_NPhases(get_FMonitoredElement()->Get_NPhases());       // Force number of phases to be same
		if(MonitoredElementTerminal > get_FMonitoredElement()->Get_NTerms())
		{
			DoErrorMsg(String("Recloser: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Re-specify terminal no.", 392);
		}
		else

               // Sets name of i-th terminal's connected bus in Recloser's buslist
		{
			SetBus(1, get_FMonitoredElement()->GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
			cBuffer = (pComplexArray) realloc(cBuffer, sizeof(complex) * get_FMonitoredElement()->Yorder);
			CondOffset = (MonitoredElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
		}
	}

/*Check for existence of Controlled Element*/

         // If previously assigned, reset HasOCPDevice flag in case this is a move
	if(ASSIGNED(get_FControlledElement()))
	{
		get_FControlledElement()->HasOCPDevice = false;
		get_FControlledElement()->HasAutoOCPDevice = false;
	}
	DevIndex = GetCktElementIndex(ElementName); // Global function
	if(DevIndex > 0)  // Both CktElement and monitored element must already exist
	{
		Set_ControlledElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Make the 1 st terminal active

             // If the recloser becomes disabled, leave at False
		if(Get_Enabled())
		{
			get_FControlledElement()->HasOCPDevice = true;  // For Reliability calcs
			get_FControlledElement()->HasAutoOCPDevice = true;  // For Reliability calcs
		}
		if(FPresentState == CTRL_CLOSE)      // Open/Close State of controlled element based on state assigned to the control
		{
			get_FControlledElement()->Set_ConductorClosed(0, ActorID, true);
			LockedOut = false;
			OperationCount = 1;
			ArmedForOpen = false;
		}
		else
		{
			get_FControlledElement()->Set_ConductorClosed(0, ActorID, false);
			LockedOut = true;
			OperationCount = NumReclose + 1;
			ArmedForClose = false;
		}
	}
	else
	{
		Set_ControlledElement(nullptr);   // element not found
		DoErrorMsg(String("Recloser: \"") + this->get_Name() + "\"", String("CktElement Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 393);
	}
}

void TRecloserObj::MakePosSequence(int ActorID)
{
	if(get_FMonitoredElement() != nullptr)
	{
		Set_NPhases(get_FMonitoredElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
		cBuffer = (pComplexArray) realloc(cBuffer, sizeof(complex) * get_FMonitoredElement()->Yorder);
		CondOffset = (ElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
	}
	inherited::MakePosSequence(ActorID);
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}
/*--------------------------------------------------------------------------*/

void TRecloserObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	/*# with ControlledElement do */
	{
		auto with0 = get_FControlledElement();
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal of CktElement to terminal 1
		switch(Code)
		{
			case ( CTRL_OPEN):
			switch(FPresentState)
			{
				case 	CTRL_CLOSE:
				if(ArmedForOpen)   // ignore if we became disarmed in meantime
				{
					get_FControlledElement()->Set_ConductorClosed(0, ActorID, false);   // Open all phases of active terminal
					if(OperationCount > NumReclose)
					{
						LockedOut = true;
						AppendToEventLog(String("Recloser.") + this->get_Name(), "Opened, Locked Out", ActorID);
					}
					else
					{
						if(OperationCount > NumFast)
							AppendToEventLog(String("Recloser.") + this->get_Name(), "Opened, Delayed", ActorID);
						else
							AppendToEventLog(String("Recloser.") + this->get_Name(), "Opened, Fast", ActorID);
					}
					if(PhaseTarget)
						AppendToEventLog(" ", "Phase Target", ActorID);
					if(GroundTarget)
						AppendToEventLog(" ", "Ground Target", ActorID);
					ArmedForOpen = false;
				}
				break; /*nada*/
				default:
				  ;
				break;
			}
			break;
			case ( CTRL_CLOSE):
			switch(FPresentState)
			{
				case 	CTRL_OPEN:
				if(ArmedForClose && !LockedOut)
				{
					get_FControlledElement()->Set_ConductorClosed(0, ActorID, true);    // Close all phases of active terminal
					++OperationCount;
					AppendToEventLog(String("Recloser.") + this->get_Name(), "Closed", ActorID);
					ArmedForClose = false;
				}
				break; /*Nada*/
				default:
				  ;
				break;
			}
			break;
			case ( CTRL_RESET):
			switch(FPresentState)
			{
				case 	CTRL_CLOSE:
				if(!ArmedForOpen)
					OperationCount = 1;
				break;       // Don't reset if we just rearmed
				  /*Nada*/
				default:
				  ;
				break;
			}
			break;
            /*Do Nothing */
			default:
			  ;
			break;
		}
	}
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::InterpretRecloserState(int ActorID, const String Action, const String property_name)
{
	if((LowerCase(&(property_name[0])) == "s") || (LowerCase(&(property_name[0])) == "a"))  // state or action (deprecated)
	{
		switch(LowerCase(Action)[0])
		{
			case 	L'o':
			 case L't':
			FPresentState = CTRL_OPEN;
			break;
			case 	L'c':
			FPresentState = CTRL_CLOSE;
			break;
			default:
			  ;
			break;
		}
	}
	else
 // Normal
	{
		switch(LowerCase(Action)[0])
		{
			case 	L'o':
			 case L't':
			FNormalState = CTRL_OPEN;
			break;
			case 	L'c':
			FNormalState = CTRL_CLOSE;
			break;
			default:
			  ;
			break;
		}
	}
}

/*--------------------------------------------------------------------------*/

void TRecloserObj::sample(int ActorID)
{
	int i = 0;
	double cmag = 0.0;
	complex Csum = {};
	TTCC_CurveObj* GroundCurve = nullptr;
	TTCC_CurveObj* PhaseCurve = nullptr;
	double Groundtime = 0.0;
	double PhaseTime = 0.0;
	double TripTime = 0.0;
	double TimeTest = 0.0;
	double TDPhase = 0.0;
	double TDGround = 0.0;
	get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);
	if(get_FControlledElement()->Get_ConductorClosed(0, ActorID))
		FPresentState = CTRL_CLOSE;
	else
		FPresentState = CTRL_OPEN;
	/*# with MonitoredElement do */
	{
		auto with0 = get_FMonitoredElement();
		if(OperationCount > NumFast)
		{
			GroundCurve = GroundDelayed;
			PhaseCurve = PhaseDelayed;
			TDGround = TDGrDelayed;
			TDPhase = TDPhDelayed;
		}
		else
		{
			GroundCurve = GroundFast;
			PhaseCurve = PhaseFast;
			TDGround = TDGrFast;
			TDPhase = TDPhFast;
		}
		if(FPresentState == CTRL_CLOSE)
		{
			TripTime = -1.0;
			Groundtime = -1.0;
			PhaseTime = -1.0;  /*No trip*/

               // Check largest Current of all phases of monitored element
			get_FMonitoredElement()->GetCurrents(cBuffer, ActorID);

               /*Check Ground Trip, if any*/
			if(GroundCurve != nullptr)
			{
				int stop = 0;
				Csum = CZero;
				for(stop = (with0->Fnphases + CondOffset), i = (1 + CondOffset); i <= stop; i++)
				{
					caccum(Csum, (cBuffer)[i - 1]);
				}
				cmag = cabs(Csum);
				if((GroundInst > 0.0) && (cmag >= GroundInst) && (OperationCount == 1))      // Inst trip on first operation
					Groundtime = 0.01 + DelayTime;
				else
					Groundtime = TDGround * GroundCurve->GetTCCTime(cmag / GroundTrip);
			}
			if(Groundtime > 0.0)
			{
				TripTime = Groundtime;
				GroundTarget = true;
			}

               // If GroundTime > 0 then we have a ground trip
			
               /*Check Phase Trip, if any*/
			if(PhaseCurve != nullptr)
			{
				int stop = 0;
				for(stop = (with0->Fnphases + CondOffset), i = (1 + CondOffset); i <= stop; i++)
				{
					cmag = cabs((cBuffer)[i - 1]);
					if((PhaseInst > 0.0) && (cmag >= PhaseInst) && (OperationCount == 1))
					{
						PhaseTime = 0.01 + DelayTime;  // Inst trip on first operation
						break;
					}
					else
					{
						TimeTest = TDPhase * PhaseCurve->GetTCCTime(cmag / PhaseTrip);
						if(TimeTest > 0.0)
						{
							if(PhaseTime < 0.0)
								PhaseTime = TimeTest;
							else
								PhaseTime = min(PhaseTime, TimeTest);
						}
					}
				}
			}
               // If PhaseTime > 0 then we have a phase trip
			if(PhaseTime > 0.0)
			{
				PhaseTarget = true;
				if(TripTime > 0.0)
					TripTime = min(TripTime, PhaseTime);
				else
					TripTime = PhaseTime;
			}
			if(TripTime > 0.0)
			{
				if(!ArmedForOpen)
					/*# with ActiveCircuit[ActorID] do */
					{
						   // Then arm for an open operation
						ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + DelayTime, CTRL_OPEN, 0, this, ActorID);
						if(OperationCount <= NumReclose)
							ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TripTime + DelayTime + (RecloseIntervals)[OperationCount - 1], CTRL_CLOSE, 0, this, ActorID);
						ArmedForOpen = true;
						ArmedForClose = true;
					}
			}
			else
			{
				if(ArmedForOpen)
					/*# with ActiveCircuit[ActorID] do */
					{
						    // If current dropped below pickup, disarm trip and set for reset
						ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + ResetTime, CTRL_RESET, 0, this, ActorID);
						ArmedForOpen = false;
						ArmedForClose = false;
						GroundTarget = false;
						PhaseTarget = false;
					}
			}
		}  /*IF PresentState=CLOSE*/
	} /*With*/
}



/*--------------------------------------------------------------------------*/

void TRecloserObj::DumpProperties(TTextRec& f, bool Complete)
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
	if(Complete)
	{
		WriteLn(f);
	}
}

String TRecloserObj::GetPropertyValue(int Index)
{
	String result;
	int i = 0;
	result = "";
	switch(Index)
	{
		case 	15:
		result = Format("%d", NumReclose + 1);
		break;
		case 	16:
		{
			int stop = 0;
			result = "(";
			for(stop = NumReclose, i = 1; i <= stop; i++)
			{
				result = result + Format("%-g, ", (RecloseIntervals)[i - 1]);
			}
			result = result + ")";
		}
		break;
		case 	23:
		{
			switch(FNormalState)
			{
				case 	CTRL_OPEN:
				result = "open";
				break;
                  /*CTRL_CLOSE:*/
				default:
				result = "closed";
				break;
			}
		}
		break;
		case 	18:
		 case 24:
		{
			switch(FPresentState)
			{
				case 	CTRL_OPEN:
				result = "open";
				break;
                  /*CTRL_CLOSE:*/
				default:
				result = "closed";
				break;
			}
		}
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TRecloserObj::Reset(int ActorID)
{
	FPresentState = FNormalState;
	ArmedForOpen = false;
	ArmedForClose = false;
	GroundTarget = false;
	PhaseTarget = false;
	if(get_FControlledElement() != nullptr)
	{
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
		switch(FNormalState)
		{
			case 	CTRL_OPEN:
			{
				get_FControlledElement()->Set_ConductorClosed(0, ActiveActor, false); // Open all phases of active terminal
				LockedOut = true;
				OperationCount = NumReclose + 1;
			}
			break;
          /*CTRL_CLOSE:*/
			default:
			get_FControlledElement()->Set_ConductorClosed(0, ActiveActor, true); // Close all phases of active terminal
			LockedOut = false;
			OperationCount = 1;
			break;
		}
	}
}

EControlAction TRecloserObj::get_State()
{
	EControlAction result;
	if(get_FControlledElement() != nullptr)
	{
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
		if(get_FControlledElement()->Get_ConductorClosed(0, ActiveActor)) {
			FPresentState = CTRL_CLOSE;
		} else {
			FPresentState = CTRL_OPEN;
		}
	}
	result = FPresentState;
	return result;
}

void TRecloserObj::set_State(const EControlAction Value)
{
	if(get_State() != Value)
	{
		if(get_FControlledElement() != nullptr)
		{
			get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
			switch(Value)
			{
				case 	CTRL_OPEN:
				{
					get_FControlledElement()->Set_ConductorClosed(0, ActiveActor, false);
					LockedOut = true;
					OperationCount = NumReclose + 1;
					ArmedForClose = false;
				}
				break;
                /*CTRL_CLOSE:*/
				default:
				get_FControlledElement()->Set_ConductorClosed(0, ActiveActor, true);
				LockedOut = false;
				OperationCount = 1;
				ArmedForOpen = false;
				break;
			}
		}
		FPresentState = Value;
	}
}

EControlAction TRecloserObj::get_NormalState()
{
	EControlAction result;
	result = FNormalState;
	return result;
}

void TRecloserObj::set_NormalState(const EControlAction Value)
{
	if(FNormalState != Value)
	{
		FNormalState = Value;
	}
}

void TRecloserObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,""); //'element';
	Set_PropertyValue(2,"1"); //'terminal';
	Set_PropertyValue(3,"");
	Set_PropertyValue(4,"1"); //'terminal';
	Set_PropertyValue(5,IntToStr(NumFast));
	Set_PropertyValue(6,"");
	Set_PropertyValue(7,"");
	Set_PropertyValue(8,"");
	Set_PropertyValue(9,"");
	Set_PropertyValue(10,"1.0");
	Set_PropertyValue(11,"1.0");
	Set_PropertyValue(12,"0");
	Set_PropertyValue(13,"0");
	Set_PropertyValue(14,"15");
	Set_PropertyValue(15,"4");
	Set_PropertyValue(16,"(0.5, 2.0, 2.0)");
	Set_PropertyValue(17,"0.0");
	Set_PropertyValue(18,"closed");
	Set_PropertyValue(19,"1.0");
	Set_PropertyValue(20,"1.0");
	Set_PropertyValue(21,"1.0");
	Set_PropertyValue(22,"1.0");
	Set_PropertyValue(23,"closed");
	Set_PropertyValue(24,"closed");
	inherited::InitPropertyValues(NumPropsThisClass);
}




}  // namespace Recloser





