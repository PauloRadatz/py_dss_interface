
#pragma hdrstop

#include "CapControl.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Ucmatrix.h"
#include "mathutil.h"
#include <math.h>
#include "Utilities.h"

using namespace std;
using namespace Bus;
using namespace CapControlVars;
using namespace CapUserControl;
using namespace Capacitor;
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
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace CapControl
{

TCapControlObj::TCapControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TCapControlObj::TCapControlObj(String ClassName) : inherited(ClassName) {}
TCapControlObj::TCapControlObj() {}


TCapControlObj* ActiveCapControlObj = nullptr;
const int AVGPHASES = -1;
const int MAXPHASE = -2;
const int MINPHASE = -3;
const int NumPropsThisClass = 23;


/*--------------------------------------------------------------------------*/  // Creates superstructure for all CapControl objects

TCapControl::TCapControl()
{
	;
	Class_Name = "CapControl";
	DSSClassType = DSSClassType + CAP_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

/*--------------------------------------------------------------------------*/

TCapControl::~TCapControl()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TCapControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "element";
	PropertyName[2 - 1] = "terminal";
	PropertyName[3 - 1] = "capacitor";
	PropertyName[4 - 1] = "type";
	PropertyName[5 - 1] = "PTratio";
	PropertyName[6 - 1] = "CTratio";
	PropertyName[7 - 1] = "ONsetting";
	PropertyName[8 - 1] = "OFFsetting";
	PropertyName[9 - 1] = "Delay";
	PropertyName[10 - 1] = "VoltOverride";
	PropertyName[11 - 1] = "Vmax";
	PropertyName[12 - 1] = "Vmin";
	PropertyName[13 - 1] = "DelayOFF";
	PropertyName[14 - 1] = "DeadTime";
	PropertyName[15 - 1] = "CTPhase";
	PropertyName[16 - 1] = "PTPhase";
	PropertyName[17 - 1] = "VBus";
	PropertyName[18 - 1] = "EventLog";
	PropertyName[19 - 1] = "UserModel";
	PropertyName[20 - 1] = "UserData";
	PropertyName[21 - 1] = "pctMinkvar";
	PropertyName[22 - 1] = "Reset";
	PropertyName[23 - 1] = "ControlSignal";
	PropertyHelp[1 - 1] = "Full object name of the circuit element, typically a line or transformer, "
	           "to which the capacitor control's PT and/or CT are connected."
	           "There is no default; must be specified.";
	PropertyHelp[2 - 1] = "Number of the terminal of the circuit element to which the CapControl is connected. "
	           "1 or 2, typically.  Default is 1.";
	PropertyHelp[3 - 1] = String("Name of Capacitor element which the CapControl controls. No Default; Must be specified." "Do not specify the full object name; \"Capacitor\" is assumed for " "the object class.  Example:") + CRLF
	           + CRLF
	           + "Capacitor=cap1";
	PropertyHelp[4 - 1] = "{Current | voltage | kvar | PF | time | Follow } Control type.  Specify the ONsetting and OFFsetting "
	           "appropriately with the type of control. (See help for ONsetting)";
	PropertyHelp[5 - 1] = "Ratio of the PT that converts the monitored voltage to the control voltage. "
	           "Default is 60.  If the capacitor is Wye, the 1st phase line-to-neutral voltage is monitored.  Else, the line-to-line "
	           "voltage (1st - 2nd phase) is monitored.";
	PropertyHelp[6 - 1] = "Ratio of the CT from line amps to control ampere setting for current and kvar control types. ";
	PropertyHelp[7 - 1] = String("Value at which the control arms to switch the capacitor ON (or ratchet up a step).  ") + CRLF
	           + CRLF
	           + "Type of Control:"
	           + CRLF
	           + CRLF
	           + "Current: Line Amps / CTratio"
	           + CRLF
	           + "Voltage: Line-Neutral (or Line-Line for delta) Volts / PTratio"
	           + CRLF
	           + "kvar:    Total kvar, all phases (3-phase for pos seq model). This is directional. "
	           + CRLF
	           + "PF:      Power Factor, Total power in monitored terminal. Negative for Leading. "
	           + CRLF
	           + "Time:    Hrs from Midnight as a floating point number (decimal). 7:30am would be entered as 7.5."
			   + CRLF
			   + "Follow: Follows a loadshape(ControlSignal) to determine when to turn ON / OFF the capacitor.If the value is different than 0 the cap will connect to the grid, otherwise, it will be disconnected.";
	PropertyHelp[8 - 1] = "Value at which the control arms to switch the capacitor OFF. (See help for ONsetting)"
	           "For Time control, is OK to have Off time the next day ( < On time)";
	PropertyHelp[9 - 1] = "Time delay, in seconds, from when the control is armed before it sends out the switching "
	           "command to turn ON.  The control may reset before the action actually occurs. "
	           "This is used to determine which capacity control will act first. Default is 15.  You may specify any "
	           "floating point number to achieve a model of whatever condition is necessary.";
	PropertyHelp[10 - 1] = "{Yes | No}  Default is No.  Switch to indicate whether VOLTAGE OVERRIDE is to be considered. "
	           "Vmax and Vmin must be set to reasonable values if this property is Yes.";
	PropertyHelp[11 - 1] = "Maximum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is greater "
	           "than this voltage, the capacitor will switch OFF regardless of other control settings. "
	           "Default is 126 (goes with a PT ratio of 60 for 12.47 kV system).";
	PropertyHelp[12 - 1] = "Minimum voltage, in volts.  If the voltage across the capacitor divided by the PTRATIO is less "
	           "than this voltage, the capacitor will switch ON regardless of other control settings. "
	           "Default is 115 (goes with a PT ratio of 60 for 12.47 kV system).";
	PropertyHelp[13 - 1] = "Time delay, in seconds, for control to turn OFF when present state is ON. Default is 15.";
	PropertyHelp[14 - 1] = "Dead time after capacitor is turned OFF before it can be turned back ON. Default is 300 sec.";
	PropertyHelp[15 - 1] = "Number of the phase being monitored for CURRENT control or one of {AVG | MAX | MIN} for all phases. Default=1. "
	           "If delta or L-L connection, enter the first or the two phases being monitored [1-2, 2-3, 3-1]. "
	           "Must be less than the number of phases. Does not apply to kvar control which uses all phases by default.";
	PropertyHelp[16 - 1] = "Number of the phase being monitored for VOLTAGE control or one of {AVG | MAX | MIN} for all phases. Default=1. "
	           "If delta or L-L connection, enter the first or the two phases being monitored [1-2, 2-3, 3-1]. "
	           "Must be less than the number of phases. Does not apply to kvar control which uses all phases by default.";
	PropertyHelp[17 - 1] = "Name of bus to use for voltage override function. Default is bus at monitored terminal. "
	           "Sometimes it is useful to monitor a bus in another location to emulate various DMS control algorithms.";
	PropertyHelp[18 - 1] = "{Yes/True* | No/False} Default is YES for CapControl. Log control actions to Eventlog.";
	PropertyHelp[19 - 1] = "Name of DLL containing user-written CapControl model, overriding the default model.  Set to \"none\" to negate previous setting. ";
	PropertyHelp[20 - 1] = "String (in quotes or parentheses if necessary) that gets passed to the user-written CapControl model Edit function for defining the data required for that model. ";
	PropertyHelp[21 - 1] = "For PF control option, min percent of total bank kvar at which control will close capacitor switch. Default = 50.";
	PropertyHelp[22 - 1] = "{Yes | No} If Yes, forces Reset of this CapControl.";
	PropertyHelp[23 - 1] = "Is the name of the load shape used for controlling the connection / disconnection of the capacitor to the grid, "
       				   "when the load shape is DIFFERENT than ZERO (0) the capacitor will be ON and connected to the grid."
					   "Otherwise, if the load shape value is EQUAL to ZERO(0) the capacitor bank will be OFF and disconnected from the grid.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TCapControl::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new CapControl and add it to CapControl class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TCapControlObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TCapControl::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;

  // continue parsing WITH contents of Parser
	ActiveCapControlObj = (TCapControlObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveCapControlObj);
	result = 0;
	/*# with ActiveCapControlObj do */
	{
		auto with0 = ActiveCapControlObj;
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
	           + "\"", 352);
				break;
				case 	1:
				with0->ElementName = ConstructElemName(LowerCase(Param));
				break;  // substitute @var value if any
				case 	2:
				with0->ElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->ControlVars.CapacitorName = String("capacitor.") + Param;
				with0->ControlVars.CapacitorNamePtr = with0->ControlVars.CapacitorName.data();
				break;   // will automatically substitute @var value
				case 	4:
				switch(LowerCase(Param)[0])
				{
					case 	L'c':
					with0->ControlType = CURRENTCONTROL;
					break;
					case 	L'v':
					with0->ControlType = VOLTAGECONTROL;
					break;
					case 	L'k':
					with0->ControlType = KVARCONTROL;
					break;
					case 	L't':
					with0->ControlType = TIMECONTROL;
					break;
					case 	L'p':
					with0->ControlType = PFCONTROL;
					break;
					case 	L'f':
					with0->ControlType = FOLLOWCONTROL;
					break;
					default:
					DoSimpleMsg("Unrecognized CapControl Type: " + Param + " (Capcontrol.)" + ActiveCapControlObj->get_Name(), 352);
					break;
				}
				break;
				case 	5:
				with0->ControlVars.PTRatio = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				with0->ControlVars.CTRatio = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				with0->ControlVars.ON_Value = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
				with0->ControlVars.OFF_Value = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
				with0->ControlVars.OnDelay = Parser[ActorID]->MakeDouble_();
				break;
				case 	10:
				with0->ControlVars.Voverride = InterpretYesNo(Param);
				break;
				case 	11:
				with0->ControlVars.Vmax = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->ControlVars.Vmin = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->ControlVars.OFFDelay = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->ControlVars.DeadTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				if(CompareTextShortest(Param, "avg") == 0)
					with0->ControlVars.FCTPhase = AVGPHASES;
				else
				{
					if(CompareTextShortest(Param, "max") == 0)
						with0->ControlVars.FCTPhase = MAXPHASE;
					else
					{
						if(CompareTextShortest(Param, "min") == 0)
							with0->ControlVars.FCTPhase = MINPHASE;
						else
							with0->ControlVars.FCTPhase = max(1, Parser[ActorID]->MakeInteger_());
					}
				}
				break;
				case 	16:
				if(CompareTextShortest(Param, "avg") == 0)
					with0->ControlVars.FPTPhase = AVGPHASES;
				else
				{
					if(CompareTextShortest(Param, "max") == 0)
						with0->ControlVars.FPTPhase = MAXPHASE;
					else
					{
						if(CompareTextShortest(Param, "min") == 0)
							with0->ControlVars.FPTPhase = MINPHASE;
						else
							with0->ControlVars.FPTPhase = max(1, Parser[ActorID]->MakeInteger_());
					}
				}
				break;
				case 	17:
				{
					with0->ControlVars.VoverrideBusSpecified = true;
					with0->ControlVars.VOverrideBusName = Param;
					with0->ControlVars.VOverrideBusNamePtr = with0->ControlVars.VOverrideBusName.data();
				}
				break;
				case 	18:
				with0->ShowEventLog = InterpretYesNo(Param);
				break;
				case 	19:
				with0->UserModel->Set_Name(Parser[ActorID]->MakeString_());
				break;  // Connect to user written model
				case 	20:
				with0->UserModel->Set_Edit(Parser[ActorID]->MakeString_());
				break;  // Send edit string to user model
				case 	21:
				with0->FpctMinkvar = Parser[ActorID]->MakeDouble_();
				break;
				case 	22:
				if(InterpretYesNo(Param))  // force a reset
				{
					with0->Reset(ActorID);
					with0->Set_PropertyValue(22, "n"); // so it gets reported properly
				}
				break;
				case 	23:
					with0->myShapeName = Param;
					break;
           // Inherited parameters
				default:
				ClassEdit(ActiveCapControlObj, ParamPointer - NumPropsThisClass);
				break;
			}


         /*PF Controller changes*/
            auto& with1 = with0->ControlVars;
			if(with0->ControlType == PFCONTROL)
			/*# with ControlVars do */
			{
				switch(ParamPointer)
				{
					case 	1:
					with0->Set_PropertyValue(1, with0->ElementName);
					break;  // Synch up with change
					case 	4:
					{
						with1.PFON_Value = 0.95;     // defaults
						with1.PFOFF_Value = 1.05;
					}
					break;
					case 	7:
					{
						if((with1.ON_Value >=  - 1.0) && (with1.ON_Value <= 1.0))
						{
							if(with1.ON_Value < 0.0)
								with1.PFON_Value = 2.0 + with1.ON_Value;
							else
								with1.PFON_Value = with1.ON_Value;
						}
						else
						{
							DoSimpleMsg(String("Invalid PF ON value for CapControl.") + ActiveCapControlObj->get_Name(), 353);
						}
					}
					break;
					case 	8:
					{
						if((with1.OFF_Value >=  - 1.0) && (with1.OFF_Value <= 1.0))
						{
							if(with1.OFF_Value < 0.0)
								with1.PFOFF_Value = 2.0 + with1.OFF_Value;
							else
								with1.PFOFF_Value = with1.OFF_Value;
						}
						else
						{
							DoSimpleMsg(String("Invalid PF OFF value for CapControl.") + ActiveCapControlObj->get_Name(), 35301);
						}
					}
					break;
					default:
						;
					break;
				}
			}

            switch (ParamPointer)
            {
                case 15:
                {
                    if (with1.FCTPhase > with0->Fnphases)
                    {
                        DoSimpleMsg(Format("Error: Monitored phase(%d) must be less than or equal to number of phases(%d). ", with1.FCTPhase, with0->Fnphases), 35302);
                        with1.FCTPhase = 1;
                    }
                }
                break;
                case 16:
                {
                    if (with1.FPTPhase > with0->Fnphases)
                    {
                        DoSimpleMsg(Format("Error: Monitored phase(%d) must be less than or equal to number of phases(%d). ", with1.FPTPhase, with0->Fnphases), 35303);
                        with1.FPTPhase = 1;
                    }
                }
					break;
				case 	19:
					with0->IsUserModel = with0->UserModel->Get_Exists();
					break;
				case 	23:
					with0->myShapeObj = (TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->myShapeName);
					break;
				default:
				  ;
				break;
			}
			if(with0->IsUserModel)
				with0->ControlType = USERCONTROL;
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		with0->RecalcElementData(ActorID);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TCapControl::MakeLike(const String CapControlName)
{
	int result = 0;
	TCapControlObj* OtherCapControl = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this CapControl name in the present collection*/
	OtherCapControl = ((TCapControlObj*) Find(CapControlName));
	if(OtherCapControl != nullptr)
		/*# with ActiveCapControlObj do */
		{
			auto with0 = ActiveCapControlObj;
			int stop = 0;
			with0->Set_NPhases(OtherCapControl->Fnphases);
			with0->Set_Nconds(OtherCapControl->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherCapControl->ElementName;
			with0->ControlVars.CapacitorName = OtherCapControl->ControlVars.CapacitorName;
			with0->ControlVars.CapacitorNamePtr = with0->ControlVars.CapacitorName.data();
			with0->Set_ControlledElement(OtherCapControl->get_FControlledElement());  // Pointer to target circuit element
			with0->Set_MonitoredElement(OtherCapControl->get_FMonitoredElement());  // Pointer to target circuit element
			with0->ElementTerminal = OtherCapControl->ElementTerminal;
			/*# with ControlVars do */
			{
				auto& with1 = with0->ControlVars;
				with1.PTRatio = OtherCapControl->ControlVars.PTRatio;
				with1.CTRatio = OtherCapControl->ControlVars.CTRatio;
				with0->ControlType = OtherCapControl->ControlType;
				with1.PresentState = OtherCapControl->ControlVars.PresentState;
				with1.ShouldSwitch = OtherCapControl->ControlVars.ShouldSwitch;
				with1.CondOffset = OtherCapControl->ControlVars.CondOffset;
				with1.ON_Value = OtherCapControl->ControlVars.ON_Value;
				with1.OFF_Value = OtherCapControl->ControlVars.OFF_Value;
				with1.PFON_Value = OtherCapControl->ControlVars.PFON_Value;
				with1.PFOFF_Value = OtherCapControl->ControlVars.PFOFF_Value;
				with1.FCTPhase = OtherCapControl->ControlVars.FCTPhase;
				with1.FPTPhase = OtherCapControl->ControlVars.FPTPhase;
				with1.Voverride = OtherCapControl->ControlVars.Voverride;
				with1.VoverrideBusSpecified = OtherCapControl->ControlVars.VoverrideBusSpecified;     // Added 8-11-11
				with1.VOverrideBusName = OtherCapControl->ControlVars.VOverrideBusName;
				with1.VOverrideBusNamePtr = with1.VOverrideBusName.data();
			}
			with0->UserModel->Set_Name(OtherCapControl->UserModel->get_FName());  // Connect to user written models
			with0->IsUserModel = OtherCapControl->IsUserModel;
			with0->FpctMinkvar = OtherCapControl->FpctMinkvar;
			with0->ShowEventLog = OtherCapControl->ShowEventLog;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i, OtherCapControl->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in CapControl MakeLike: \"") + CapControlName
	           + "\" Not Found.", 360);
	return result;
}


/*==========================================================================*/
/*                    TCapControlObj                                        */
/*==========================================================================*/


/*--------------------------------------------------------------------------*/

TCapControlObj::TCapControlObj(TDSSClass* ParClass, const String CapControlName)
 : inherited(ParClass),
			ControlType(CURRENTCONTROL),
			ControlledCapacitor(nullptr),
			IsUserModel(false),
			UserModel(nullptr),
			FpctMinkvar(0.0)
{
	Set_Name(LowerCase(CapControlName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	/*# with ControlVars do */
	cBuffer.clear();
	{
		auto& with0					= ControlVars;
		with0.FCTPhase				= 1;
		with0.FPTPhase				= 1;
		with0.PTRatio				= 60.0;
		with0.CTRatio				= 60.0;
		ControlType					= CURRENTCONTROL;
		with0.OnDelay				= 15.0;
		with0.OFFDelay				= 15.0;
		with0.DeadTime				= 300.0;
		with0.LastOpenTime			= -with0.DeadTime;
		with0.ON_Value				= 300.0;
		with0.OFF_Value				= 200.0;
		with0.PFON_Value			= 0.95;
		with0.PFOFF_Value			= 1.05;
		with0.Voverride				= false;
		with0.VoverrideEvent		= false;
		with0.VoverrideBusSpecified = false;
		with0.VOverrideBusName		= "";   // This is not in public data Struct at this time
		with0.VOverrideBusNamePtr	= nullptr;
		with0.Vmax					= 126;
		with0.Vmin					= 115;
		with0.PresentState			= CTRL_CLOSE;
		with0.ShouldSwitch			= false;
		with0.Armed					= false;
		with0.AvailableSteps		= 0;
		Set_PendingChange(CTRL_NONE);
	}
	PublicDataStruct				= &ControlVars;   // So User-written models can access
	PublicDataSize					= sizeof(TCapControlVars);
	ElementName						= "";
	Set_ControlledElement(nullptr);
	ElementTerminal					= 1;
	ControlVars.CapacitorName		= "";
	ControlVars.CapacitorNamePtr	= nullptr;
	Set_MonitoredElement(nullptr);
	myShapeName						= "";
	myShapeObj						= nullptr;

	FpctMinkvar						= 50.0;
	IsUserModel						= false;
	UserModel						= new TCapUserControl();   // Inits handles, FID
	ControlVars.ControlActionHandle = 0;
	DSSObjType						= ParClass->DSSClassType; //cap_CONTROL;
	InitPropertyValues(0);

   //  RecalcElementData;
}

TCapControlObj::~TCapControlObj()
{
	ElementName = "";
	ControlVars.CapacitorName = "";
	if(!cBuffer.empty())
		cBuffer.clear();
	try
	{
		delete UserModel;
/* }
	__finally
	{*/
		UserModel = nullptr; // do nothing
	}
	catch (...)
	{
		//
	}
	// inherited::Destroy();
}



/*--------------------------------------------------------------------------*/

void TCapControlObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;

/*Check for existence of capacitor*/

// 5-21-01 RCD moved this section ahead of monitored element so Nphases gets defined first
	DevIndex = GetCktElementIndex(ControlVars.CapacitorName); // Global function
	if(DevIndex > 0)  // Both capacitor and monitored element must already exist
	{
		Set_ControlledElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
		ControlledCapacitor = Get_Capacitor();
		Set_NPhases(get_FControlledElement()->Get_NPhases());  // Force number of phases to be same   Added 5/21/01  RCD
		Set_Nconds(Fnphases);
		get_FControlledElement()->Set_ActiveTerminal(1);  // Make the 1 st terminal active
                 // Get control synched up with capacitor
		/*# with ControlledCapacitor do */
		{
			auto with0 = ControlledCapacitor;
			if(ControlVars.AvailableSteps == with0->Get_FNumSteps())
				get_FControlledElement()->Set_ConductorClosed(0, ActorID, false);
			else
				get_FControlledElement()->Set_ConductorClosed(0, ActorID, true);
		}
		if(get_FControlledElement()->Get_ConductorClosed(0, ActorID))
			ControlVars.PresentState = CTRL_CLOSE;
		else
			ControlVars.PresentState = CTRL_OPEN;
	}
	else
	{
		Set_ControlledElement(nullptr);   // element not found
		DoErrorMsg(String("CapControl: \"") + this->get_Name() + "\"", String("Capacitor Element \"") + ControlVars.CapacitorName
	           + "\" Not Found.", " Element must be defined previously.", 361);
	}
	ControlVars.InitialState = ControlVars.PresentState;

/*Check for existence of monitored element - if needed*/
	bool ElmReq = true;
	ElmReq		= ( ElmReq && (ControlType != TIMECONTROL) ) && (ControlType != FOLLOWCONTROL);
	if (ElmReq)
	{
		DevIndex = GetCktElementIndex(ElementName); // Global function
		if (DevIndex > 0)
		{
			Set_MonitoredElement(((TDSSCktElement*)ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
			if (ElementTerminal > get_FMonitoredElement()->Get_NTerms())
			{
				DoErrorMsg(String("CapControl.") + get_Name() + ":", "Terminal no. \"" "\" does not exist.", "Re-specify terminal no.", 362);
			}
			else

				// Sets name of i-th terminal's connected bus in CapControl's buslist
			{
				SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
				// Allocate a buffer bigenough to hold everything from the monitored element
				cBuffer.resize(get_FMonitoredElement()->Yorder + 1);
				ControlVars.CondOffset = (ElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
			}
		}
		else
			DoSimpleMsg(String("Monitored Element in CapControl.") + get_Name()
				+ " does not exist:\""
				+ ElementName
				+ "\"", 363);
	}
	else
	{
		SetBus(1, get_FControlledElement()->GetBus(ElementTerminal));
		// Allocate a buffer bigenough to hold everything from the monitored element
		cBuffer.resize(get_FControlledElement()->Yorder + 1);
		ControlVars.CondOffset = (ElementTerminal - 1) * get_FControlledElement()->Get_NConds(); // for speedy sampling
	}
         /*Alternative override bus*/
	if(ControlVars.VoverrideBusSpecified)
		/*# with ControlVars do */
		{
			auto& with1 = ControlVars;
			with1.VOverrideBusIndex = ActiveCircuit[ActorID]->BusList.Find(with1.VOverrideBusName);
			if(with1.VOverrideBusIndex == 0)
			{
				DoSimpleMsg("CapControl." + get_Name() + ": Voltage override Bus " + with1.VOverrideBusName + " not found. Did you wait until buses were defined? Reverting to default.", 10361);
				with1.VoverrideBusSpecified = false;
			}
		}

         // User model property update, if necessary
	if(UserModel->Get_Exists())
		UserModel->UpdateModel();  // Checks for existence and Selects
}

void TCapControlObj::MakePosSequence(int ActorID)
{
	if(get_FControlledElement() != nullptr)
	{
		Set_Enabled(get_FControlledElement()->Get_Enabled());
		Set_NPhases(get_FControlledElement()->Get_NPhases());
		Set_Nconds(Fnphases);
	}
	if(get_FMonitoredElement() != nullptr)
	{
		SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
		cBuffer.resize(get_FMonitoredElement()->Yorder + 1);
		ControlVars.CondOffset = (ElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
	}
	inherited::MakePosSequence(ActorID);
}

/*--------------------------------------------------------------------------*/

void TCapControlObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}

/*--------------------------------------------------------------------------*/

void TCapControlObj::GetBusVoltages(TDSSBus* pBus, pComplexArray Buff, int ActorID)
{
	int j = 0;
	int k = 0;
	/*# with pBus do */
	{
		auto with0 = pBus;
		if(!(with0->VBus.empty()))    // uses nphases from CapControlObj
		{
			if(!ADiakoptics || (ActorID == 1))
			{
				int stop = 0;
				for(stop = Get_NPhases(), j = 1; j <= stop; j++)
				{
					(cBuffer)[j - 1] = ActiveCircuit[ActorID]->Solution->NodeV[with0->GetRef(j)];
				}
			}
			else
			{
				int stop = 0;
				for(stop = Get_NPhases(), j = 1; j <= stop; j++)
				{  // In the context of actor 1
					(cBuffer)[j - 1] = ActiveCircuit[ActorID]->Solution->VoltInActor1(with0->GetRef(j));
				}
			}
		}
	}
}

// Get current to control on based on type of control specified.

void TCapControlObj::GetControlCurrent(double& ControlCurrent)
{
	int i = 0;
	/*# with ControlVars do */
	{
		auto& with0 = ControlVars;
		switch(with0.FCTPhase)
		{
			case 	AVGPHASES:
			{
				int stop = 0;
				ControlCurrent = 0.0;     // Get avg of all phases
				for(stop = (Fnphases + with0.CondOffset), i = (1 + with0.CondOffset); i <= stop; i++)
				{
					ControlCurrent = ControlCurrent + cabs((cBuffer)[i - 1]);
				}
				ControlCurrent = ControlCurrent / Fnphases / with0.CTRatio;
			}
			break;
			case 	MAXPHASE:
			{
				int stop = 0;
				ControlCurrent = 0.0;     // Get max of all phases
				for(stop = (Fnphases + with0.CondOffset), i = (1 + with0.CondOffset); i <= stop; i++)
				{
					ControlCurrent = max(ControlCurrent, cabs((cBuffer)[i - 1]));
				}
				ControlCurrent = ControlCurrent / with0.CTRatio;
			}
			break;
			case 	MINPHASE:
			{
				int stop = 0;
				ControlCurrent = 1.0e50;     // Get min of all phases
				for(stop = (Fnphases + with0.CondOffset), i = (1 + with0.CondOffset); i <= stop; i++)
				{
					ControlCurrent = min(ControlCurrent, cabs((cBuffer)[i - 1]));
				}
				ControlCurrent = ControlCurrent / with0.CTRatio;
			}
			break;
    /*Just use one phase because that's what most controls do.*/
			default:
			ControlCurrent = cabs((cBuffer)[with0.FCTPhase - 1]) / with0.CTRatio;  // monitored phase only
			break;
		}
	}
}

void TCapControlObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TCapControlObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TCapControlObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
	{
		WriteLn(f);
	}
}


/*--------------------------------------------------------------------------*/

void TCapControlObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	get_FControlledElement()->Set_ActiveTerminal(1);  // Set active terminal of capacitor to terminal 1

        /*Allow user control to do something*/
	switch(ControlType)
	{
		case 	USERCONTROL:
		if(UserModel->Get_Exists())
		{
			UserModel->DoPending(Code, ProxyHdl);
                              // If control action changes last step in service, force update of Yprim and Fstates array
			ControlledCapacitor->set_LastStepInService(ControlVars.LastStepInService);
                              // Usermodel could override Pending change so the rest of this procedure is ignored.
		}
		break;
		default:
		  ;
		break;
	}
	/*# with ControlVars do */
	{
		auto& with0 = ControlVars;
		switch(Get_PendingChange())
		{
			case 	CTRL_OPEN:
			switch(ControlledCapacitor->Get_FNumSteps())
			{
				case 	1:
				{
					if(with0.PresentState == CTRL_CLOSE)
					{
						get_FControlledElement()->Set_ConductorClosed(0, ActorID, false);  // Open all phases of active terminal
						ControlledCapacitor->SubtractStep(ActorID);
						if(ShowEventLog)
							AppendToEventLog("Capacitor." + get_FControlledElement()->get_Name(), "**Opened**", ActorID);
						with0.PresentState = CTRL_OPEN;
						/*# with ActiveCircuit[ActorID].Solution do */
						{
							auto with1 = ActiveCircuit[ActorID]->Solution;
							with0.LastOpenTime = with1->DynaVars.T + 3600.0 * with1->DynaVars.intHour;
						}
					}
				}
				break;
				default:
				if(with0.PresentState == CTRL_CLOSE)      // Do this only if at least one step is closed
				{
					if(!ControlledCapacitor->SubtractStep(ActorID))
					{
						with0.PresentState = CTRL_OPEN;
						get_FControlledElement()->Set_ConductorClosed(0, ActorID, false);   // Open all phases of active terminal
						if(ShowEventLog)
							AppendToEventLog("Capacitor." + get_FControlledElement()->get_Name(), "**Opened**", ActorID);
					}
					else
					{
						if(ShowEventLog)
							AppendToEventLog("Capacitor." + get_FControlledElement()->get_Name(), "**Step Down**", ActorID);
					}
				}
				break;
			}
			break;
			case 	CTRL_CLOSE:
			{
				if(with0.PresentState == CTRL_OPEN)
				{
					get_FControlledElement()->Set_ConductorClosed(0, ActorID, true);    // Close all phases of active terminal
					if(ShowEventLog)
						AppendToEventLog("Capacitor." + get_FControlledElement()->get_Name(), "**Closed**", ActorID);
					with0.PresentState = CTRL_CLOSE;
					ControlledCapacitor->AddStep(ActorID);
				}
				else
				{
					if(ControlledCapacitor->AddStep(ActorID))
					{
						if(ShowEventLog)
							AppendToEventLog("Capacitor." + get_FControlledElement()->get_Name(), "**Step Up**", ActorID);
					}
				}
			}
			break;
            /*Do Nothing for NONE if the control has reset*/
			default:
			  ;
			break;
		}
	}
	/*# with ControlVars do */
	{
		auto& with2				= ControlVars;
		with2.VoverrideEvent	= false;
		with2.ShouldSwitch		= false;
		with2.Armed				= false;   // reset control
	}
}

// Get Voltage used for voltage control based on specified options

void TCapControlObj::GetControlVoltage(double& ControlVoltage)
{
	int i = 0;

	auto NextDeltaPhase = [&](int iPhs) -> int 
	{
		int result = 0;
		result = iPhs + 1;
		if(result > Fnphases)
			result = 1;
		return result;
	};
	/*# with ControlVars do */
	{
		auto& with0 = ControlVars;
		switch(with0.FPTPhase)
		{
			case 	AVGPHASES:
			{
				int stop = 0;
				ControlVoltage = 0.0;
				for(stop = get_FMonitoredElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					ControlVoltage = ControlVoltage + cabs(cBuffer[i - 1]);
				}
				ControlVoltage = ControlVoltage / get_FMonitoredElement()->Get_NPhases() / with0.PTRatio;
			}
			break;
			case 	MAXPHASE:
			{
				int stop = 0;
				ControlVoltage = 0.0;
				for(stop = get_FMonitoredElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					ControlVoltage = max(ControlVoltage, cabs((cBuffer)[i - 1]));
				}
				ControlVoltage = ControlVoltage / with0.PTRatio;
			}
			break;
			case 	MINPHASE:
			{
				int stop = 0;
				ControlVoltage = 1.0e50;
				for(stop = get_FMonitoredElement()->Get_NPhases(), i = 1; i <= stop; i++)
				{
					ControlVoltage = min(ControlVoltage, cabs((cBuffer)[i - 1]));
				}
				ControlVoltage = ControlVoltage / with0.PTRatio;
			}
			break;
    /*Just use one phase because that's what most controls do.*/
    // Use L-L aB if capacitor is delta connected!!
			default:
			switch(((TCapacitorObj*) get_FControlledElement())->Connection)
			{
				case 	1:
				ControlVoltage = cabs(csub((cBuffer)[with0.FPTPhase - 1], (cBuffer)[NextDeltaPhase(with0.FPTPhase) - 1])) / with0.PTRatio;
				break;   // Delta
				default:
				ControlVoltage = cabs((cBuffer)[with0.FPTPhase - 1]) / with0.PTRatio;     // Wye - Default
				break;
			}
			break;
		}
	}
}

/*--------------------------------------------------------------------------*/

void TCapControlObj::sample(int ActorID)
{
	double PF = 0.0;
	double Sabs = 0.0;
	double CurrTest = 0.0;
	double Vtest = 0.0;
	double NormalizedTime = 0.0;
	double Q = 0.0;
	complex s = {};

	auto PF1to2 = [&](const complex& Spower) -> double 
	{
		double result = 0.0;
		Sabs = cabs(Spower);
		if(Sabs != 0.0)
			result = double(Abs(Spower.re)) / Sabs;
		else
			result = 1.0;  // default to unity
		if(Spower.im < 0.0)
			result = 2.0 - result;
		return result;
	};   // return PF in range of 1 to 2
	get_FControlledElement()->Set_ActiveTerminal(1);
	if(get_FControlledElement()->Get_ConductorClosed(0, ActorID))
		ControlVars.PresentState = CTRL_CLOSE;
	else
		ControlVars.PresentState = CTRL_OPEN;
	/*# with MonitoredElement, ControlVars do */
	{
		auto with0 = get_FMonitoredElement();
		auto& with1 = ControlVars;
		with1.ShouldSwitch = false;

         // Get_First() Check voltage override
		if(with1.Voverride)
		{
			if(ControlType != VOLTAGECONTROL)  // Don't bother for voltage control
			{
				if(with1.VoverrideBusSpecified)
				{
					GetBusVoltages( ActiveCircuit[ActorID]->Buses[with1.VOverrideBusIndex - 1], &(cBuffer[0]), ActorID);
				}
				else
				get_FMonitoredElement()->GetTermVoltages(ElementTerminal, &(cBuffer[0]), ActorID);
				GetControlVoltage(Vtest);
				switch(with1.PresentState)
				{
					case 	CTRL_OPEN:
					if(Vtest < with1.Vmin)
					{
						Set_PendingChange(CTRL_CLOSE);
						with1.ShouldSwitch = true;
						with1.VoverrideEvent = true;
						if(ShowEventLog)
							AppendToEventLog(String("Capacitor.") + get_FControlledElement()->get_Name(), Format("Low Voltage Override: %.8g V", Vtest), ActorID);
					}
					break;
					case 	CTRL_CLOSE:
					if(Vtest > with1.Vmax)
					{
						Set_PendingChange(CTRL_OPEN);
						with1.ShouldSwitch = true;
						with1.VoverrideEvent = true;
						if(ShowEventLog)
							AppendToEventLog(String("Capacitor.") + get_FControlledElement()->get_Name(), Format("High Voltage Override: %.8g V", Vtest), ActorID);
					}
					break;
					default:
					  ;
					break;
				}
			}
		}
		if(!with1.ShouldSwitch)
			switch(ControlType)
			{   // Else skip other control evaluations
				case 	CURRENTCONTROL: /*Current*/

                     // Check largest Current of all phases of monitored element
				{
					get_FMonitoredElement()->GetCurrents(&(cBuffer[0]), ActorID);
					GetControlCurrent(CurrTest);
					switch(with1.PresentState)
					{
						case 	CTRL_OPEN:
						if(CurrTest > with1.ON_Value)
						{
							Set_PendingChange(CTRL_CLOSE);
							with1.ShouldSwitch = true;
						}
						else
 // Reset
						Set_PendingChange(CTRL_NONE);
						break;
						case 	CTRL_CLOSE:
						if(CurrTest < with1.OFF_Value)
						{
							Set_PendingChange(CTRL_OPEN);
							with1.ShouldSwitch = true;
						}
						else
						{
							if(ControlledCapacitor->AvailableSteps() > 0)
							{
								if(CurrTest > with1.ON_Value)
								{
									Set_PendingChange(CTRL_CLOSE);
									with1.ShouldSwitch = true;
								}
							}
							else
 // Reset
							Set_PendingChange(CTRL_NONE);
						}
						break;
						default:
						  ;
						break;
					}
				}
				break; /*Voltage*/
				case 	VOLTAGECONTROL:
				{
					get_FMonitoredElement()->GetTermVoltages(ElementTerminal, &(cBuffer[0]), ActorID);
					GetControlVoltage(Vtest);
					switch(with1.PresentState)
					{
						case 	CTRL_OPEN:
						if(Vtest < with1.ON_Value)
						{
							Set_PendingChange(CTRL_CLOSE);
							with1.ShouldSwitch = true;
						}
						else
 // Reset
						Set_PendingChange(CTRL_NONE);
						break;
						case 	CTRL_CLOSE:
						{
							Set_PendingChange(CTRL_NONE);
							if(Vtest > with1.OFF_Value)
							{
								Set_PendingChange(CTRL_OPEN);
								with1.ShouldSwitch = true;
							}
							else
							{
								if(ControlledCapacitor->AvailableSteps() > 0)
								{
									if(Vtest < with1.ON_Value)
									{
										Set_PendingChange(CTRL_CLOSE);
										with1.ShouldSwitch = true;
									}
								}
							}
						}
						break;
						default:
						  ;
						break;
					}
				}
				break; /*kvar*/
                      //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
				case 	KVARCONTROL:
				{
					s = get_FMonitoredElement()->Get_Power(ElementTerminal, ActorID);
					Q = s.im * 0.001;  // kvar
					switch(with1.PresentState)
					{
						case 	CTRL_OPEN:
						if(Q > with1.ON_Value)
						{
							Set_PendingChange(CTRL_CLOSE);
							with1.ShouldSwitch = true;
						}
						else
 // Reset
						Set_PendingChange(CTRL_NONE);
						break;
						case 	CTRL_CLOSE:
						if(Q < with1.OFF_Value)
						{
							Set_PendingChange(CTRL_OPEN);
							with1.ShouldSwitch = true;
						}
						else
						{
							if(ControlledCapacitor->AvailableSteps() > 0)
							{
								if(Q > with1.ON_Value)
								{
									Set_PendingChange(CTRL_CLOSE);  // We can go some more
									with1.ShouldSwitch = true;
								}
							}
							else
 // Reset
							Set_PendingChange(CTRL_NONE);
						}
						break;
						default:
						  ;
						break;
					}
				}
				break;

              /*User Control*/
				case 	USERCONTROL:
				if(UserModel->Get_Exists())   // selects the model associated with this control

                     // Load up test data into the public data record
				{
					with1.SampleP = cmulreal(get_FMonitoredElement()->Get_Power(ElementTerminal, ActorID), 0.001);  // kW kvar
					get_FMonitoredElement()->GetTermVoltages(ElementTerminal, &(cBuffer[0]), ActorID);
					GetControlVoltage(with1.SampleV);
					get_FMonitoredElement()->GetCurrents(&(cBuffer[0]), ActorID);
					GetControlCurrent(with1.SampleCurr);
					with1.NumCapSteps = ControlledCapacitor->Get_FNumSteps();
					with1.AvailableSteps = ControlledCapacitor->AvailableSteps();
					with1.LastStepInService = ControlledCapacitor->Get_FLastStepInService();
					UserModel->sample();   // Sets the switching flags
				}
				break; /*time*/
              /*7-8-10  NormalizeToTOD Algorithm modified to close logic hole between 11 PM and midnight*/
				case 	TIMECONTROL:
				{
					/*# with ActiveCircuit[ActorID].Solution do */
					{
						auto with2 = ActiveCircuit[ActorID]->Solution;
						NormalizedTime = NormalizeToTOD(with2->DynaVars.intHour, with2->DynaVars.T);
					}
                    /* 1/28/09 Code modified to accommodate OFF_Value < ON_Value */
					switch(with1.PresentState)
					{
						case 	CTRL_OPEN:
						if(with1.OFF_Value > with1.ON_Value)
						{
							if((NormalizedTime >= with1.ON_Value) && (NormalizedTime < with1.OFF_Value))
							{
								Set_PendingChange(CTRL_CLOSE);
								with1.ShouldSwitch = true;
							}
							else
 // Reset
							Set_PendingChange(CTRL_NONE);
						}
						else
    // OFF time is next day
						{
							if((NormalizedTime >= with1.ON_Value) && (NormalizedTime < 24.0))
							{
								Set_PendingChange(CTRL_CLOSE);
								with1.ShouldSwitch = true;
							}
							else
 // Reset
							Set_PendingChange(CTRL_NONE);
						}
						break;
						case 	CTRL_CLOSE:
						if(with1.OFF_Value > with1.ON_Value)
						{
							if((NormalizedTime >= with1.OFF_Value) || (NormalizedTime < with1.ON_Value))
							{
								Set_PendingChange(CTRL_OPEN);
								with1.ShouldSwitch = true;
							}
							else
							{
								if(ControlledCapacitor->AvailableSteps() > 0)
								{
									if((NormalizedTime >= with1.ON_Value) && (NormalizedTime < with1.OFF_Value))
									{
										Set_PendingChange(CTRL_CLOSE);  // We can go some more
										with1.ShouldSwitch = true;
									}
								}
								else
 // Reset
								Set_PendingChange(CTRL_NONE);
							}
						}
						else
  // OFF time is next day
						{
							if((NormalizedTime >= with1.OFF_Value) && (NormalizedTime < with1.ON_Value))
							{
								Set_PendingChange(CTRL_OPEN);
								with1.ShouldSwitch = true;
							}
							else
							{
								if(ControlledCapacitor->AvailableSteps() > 0)
								{
									if((NormalizedTime >= with1.ON_Value) && (NormalizedTime < 24.0))
									{
										Set_PendingChange(CTRL_CLOSE);  // We can go some more
										with1.ShouldSwitch = true;
									}
								}
								else
 // Reset
								Set_PendingChange(CTRL_NONE);
							}
						}
						break;
						default:
						  ;
						break;
					}
				}
				break; /*PF*/
                      //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
				case 	PFCONTROL:
				{
					s = get_FMonitoredElement()->Get_Power(ElementTerminal, ActorID);
					PF = PF1to2(s);

                      /*PF is in range of 0 .. 2;  Leading is 1..2*/
                      /*When turning on make sure there is at least half the kvar of the bank*/
					switch(with1.PresentState)
					{
						case 	CTRL_OPEN:
						if((PF < with1.PFON_Value) && (s.im * 0.001 > ControlledCapacitor->Get_Ftotalkvar() * FpctMinkvar * 0.01)) // make sure we don't go too far leading
						{
							Set_PendingChange(CTRL_CLOSE);
							with1.ShouldSwitch = true;
						}
						else
 // Reset
						Set_PendingChange(CTRL_NONE);
						break;
						case 	CTRL_CLOSE:
						if(PF > with1.PFOFF_Value)
						{
							Set_PendingChange(CTRL_OPEN);
							with1.ShouldSwitch = true;
						}
						else
						{
							if(ControlledCapacitor->AvailableSteps() > 0)
							{
								if((PF < with1.PFON_Value) && (s.im * 0.001 > ControlledCapacitor->Get_Ftotalkvar() / ControlledCapacitor->Get_FNumSteps() * 0.5))
								{
									Set_PendingChange(CTRL_CLOSE);  // We can go some more
									with1.ShouldSwitch = true;
								}
							}
							else
 // Reset
							Set_PendingChange(CTRL_NONE);
						}
						break;
						default:
						  ;
						break;
					}
				}
				break;
				case 	FOLLOWCONTROL:
				{
					auto	with3		= ActiveCircuit[ActorID]->Solution;
					double	NextState	= myShapeObj->GetMult(with3->DynaVars.dblHour).re;
					if ( (NextState != 0) == (with1.PresentState == CTRL_OPEN) )
					{
						if (with1.PresentState == CTRL_OPEN)
							Set_PendingChange(CTRL_CLOSE);
						else
							Set_PendingChange(CTRL_OPEN);
						with1.ShouldSwitch = true;
					}
				}
				default:
				  ;
				break;
			}
	}
	/*# with ActiveCircuit[ActorID], ControlVars do */
	{
		
		auto& with4 = ControlVars;
		if(with4.ShouldSwitch && !with4.Armed)
		{
			if(Get_PendingChange() == CTRL_CLOSE)
			{
				if((ActiveCircuit[ActorID]->Solution->DynaVars.T + ActiveCircuit[ActorID]->Solution->DynaVars.intHour * 3600.0 - with4.LastOpenTime) < with4.DeadTime) // delay the close operation
					                      /*2-6-09 Added ONDelay to Deadtime so that all caps do not close back in at same time*/
					TimeDelay = max(with4.OnDelay, (with4.DeadTime + with4.OnDelay) - (ActiveCircuit[ActorID]->Solution->DynaVars.T + ActiveCircuit[ActorID]->Solution->DynaVars.intHour * 3600.0 - with4.LastOpenTime));
				else
					TimeDelay = with4.OnDelay;
			}
			else
			TimeDelay = with4.OFFDelay;
			with4.ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(ActiveCircuit[ActorID]->Solution->DynaVars.intHour, ActiveCircuit[ActorID]->Solution->DynaVars.T + TimeDelay, Get_PendingChange(), 0, this, ActorID);
			with4.Armed = true;
			if(ShowEventLog)
				AppendToEventLog("Capacitor." + get_FControlledElement()->get_Name(), Format("**Armed**, Delay= %.5g sec", TimeDelay), ActorID);
		}
		if(with4.Armed && (Get_PendingChange() == CTRL_NONE))
		{
			ActiveCircuit[ActorID]->ControlQueue.Delete(with4.ControlActionHandle, ActorID);
			with4.Armed = false;
			if(ShowEventLog)
				AppendToEventLog("Capacitor." + get_FControlledElement()->get_Name(), "**Reset**", ActorID);
		}
	}  /*With*/
}

//------------------------------------------------------------------------------------

ECapControlType TCapControlObj::get_ControlType()
{
	return ControlType;
}

//------------------------------------------------------------------------------------

void TCapControlObj::set_ControlType(CapControlVars::ECapControlType value)
{
	ControlType = value;
}

//------------------------------------------------------------------------------------

TCapacitorObj* TCapControlObj::Get_Capacitor()
{
	TCapacitorObj* result = nullptr;
	result = (TCapacitorObj*) get_FControlledElement();
	return result;
}

EControlAction TCapControlObj::Get_PendingChange()
{
	EControlAction result;
	result = ControlVars.FPendingChange;
	return result;
}
// Normalize time to a floating point number representing time of day if Hour > 24
// Resulting time should be 0:00+ to 24:00 inclusive.

double TCapControlObj::NormalizeToTOD(int h, double Sec)
{
	double result = 0.0;
	int HourOfDay = 0;
	if(h > 24)  // creates numbers 1..24
		HourOfDay = (int) (h - ((h - 1) / 24) * 24);
	else
		HourOfDay = h;
	result = HourOfDay + Sec / 3600.0;

   // If the TOD is at least slightly greater than 24:00 wrap around to 0:00
	if(result - 24.0 > EPSILON)
		result = result - 24.0;   // Wrap around
	return result;
}

void TCapControlObj::Reset(int ActorID)
{
	Set_PendingChange(CTRL_NONE);
	get_FControlledElement()->Set_ActiveTerminal(1);
	/*# with ControlVars do */
	{
		auto& with0 = ControlVars;
		switch(with0.InitialState)
		{
			case 	CTRL_OPEN:
			get_FControlledElement()->Set_ConductorClosed(0, ActiveActor, false);
			break;   // Open all phases of active terminal
			case 	CTRL_CLOSE:
			get_FControlledElement()->Set_ConductorClosed(0, ActiveActor, true);
			break;    // Close all phases of active terminal
			default:
			  ;
			break;
		}
		with0.ShouldSwitch = false;
		with0.LastOpenTime = -with0.DeadTime;
		with0.PresentState = with0.InitialState;
	}
}

void TCapControlObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1, "");   //'element';
	Set_PropertyValue(2, "1");   //'terminal';
	Set_PropertyValue(3, "");
	Set_PropertyValue(4, "current");
	Set_PropertyValue(5, "60");
	Set_PropertyValue(6, "60");
	Set_PropertyValue(7, "300");
	Set_PropertyValue(8, "200");
	Set_PropertyValue(9, "15");
	Set_PropertyValue(10, "NO");
	Set_PropertyValue(11, "126");
	Set_PropertyValue(12, "115");
	Set_PropertyValue(13, "15");
	Set_PropertyValue(14, "300");
	Set_PropertyValue(15, "1");
	Set_PropertyValue(16, "1");
	Set_PropertyValue(17, "");
	Set_PropertyValue(18, ShowEventLog ? "YES" : "NO");
	Set_PropertyValue(19, "");
	Set_PropertyValue(20, "");
	Set_PropertyValue(21, "50");
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TCapControlObj::Set_PendingChange(const EControlAction Value)
{
	ControlVars.FPendingChange = Value;
	DblTraceParameter = (double) (Value);
}

// return functions for properties

double TCapControlObj::GetON_Value()
{
	return ControlVars.ON_Value;
}
double TCapControlObj::GetOFF_Value()
{
	return ControlVars.OFF_Value;
}

double TCapControlObj::GetPFON_Value()
{
	return ControlVars.PFON_Value;
}

double TCapControlObj::GetPFOFF_Value()
{
	return ControlVars.PFOFF_Value;
}

double TCapControlObj::GetPTRatio_Value()
{
	return ControlVars.PTRatio;
}

double TCapControlObj::GetCTRatio_Value()
{
	return ControlVars.CTRatio;
}

double TCapControlObj::GetONDelay_Value()
{
	return ControlVars.OnDelay;
}

double TCapControlObj::GetOFFDelay_Value()
{
	return ControlVars.OFFDelay;
}

double TCapControlObj::GetVmin_Value()
{
	return ControlVars.Vmin;
}

double TCapControlObj::GetVmax_Value()
{
	return ControlVars.Vmax;
}

bool TCapControlObj::GetVoverride_Value()
{
	return ControlVars.Voverride;
}

double TCapControlObj::GetDeadTime_Value()
{
	return ControlVars.DeadTime;
}

int TCapControlObj::GetFPTPhase_Value()
{
	return ControlVars.FPTPhase;
}


}  // namespace CapControl






