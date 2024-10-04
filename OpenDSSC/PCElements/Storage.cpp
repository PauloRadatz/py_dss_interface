

#pragma hdrstop

#include "Storage.h"

#include "ParserDel.h"
#include "Circuit.h"
#include "Command.h"
#include <math.h>
#include "mathutil.h"
#include "DSSGlobals.h"
#include "Utilities.h"

using namespace std;
using namespace Arraydef;
using namespace Circuit;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Dynamics;
using namespace LoadShape;
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace StorageVars;
using namespace StoreUserModel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XYCurve;
using namespace mathutil;
using namespace Utilities;

namespace Storage
{

TStorageObj::TStorageObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TStorageObj::TStorageObj(String ClassName) : inherited(ClassName) {}
TStorageObj::TStorageObj() {}


TStorageObj* ActiveStorageObj = nullptr;

/*  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit FUNCTION
    3) add a statement(s) to InitPropertyValues FUNCTION to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, If needed
 = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =*/
const int propKV = 3;
const int propKW = 4;
const int propPF = 5;
const int propMODEL = 6;
const int propYEARLY = 7;
const int propDAILY = 8;
const int propDUTY = 9;
const int propDISPMODE = 10;
const int PropConnection = 11;
const int propKVAR = 12;
const int propPCTR = 13;
const int propPCTX = 14;
const int propIDLEKW = 15;
const int propCLASS = 16;
const int propDISPOUTTRIG = 17;
const int propDISPINTRIG = 18;
const int propCHARGEEFF = 19;
const int propDISCHARGEEFF = 20;
const int propPCTKWOUT = 21;
const int propVMINPU = 22;
const int propVMAXPU = 23;
const int propSTATE = 24;
const int propKVA = 25;
const int propKWRATED = 26;
const int propKWHRATED = 27;
const int propKWHSTORED = 28;
const int propPCTRESERVE = 29;
const int propUSERMODEL = 30;
const int propUSERDATA = 31;
const int propDEBUGTRACE = 32;
const int propPCTKWIN = 33;
const int propPCTSTORED = 34;
const int propCHARGETIME = 35;
const int propDynaDLL = 36;
const int propDynaData = 37;
const int propBalanced = 38;
const int propLimited = 39;
const int propInvEffCurve = 40;
const int propCutin = 41;
const int propCutout = 42;
const int proppctkWrated = 43;
const int propVarFollowInverter = 44;
const int propkvarLimit = 45;
const int propPpriority = 46;
const int propPFpriority = 47;
const int propPminNoVars = 48;
const int propPminkvarLimit = 49;
const int propkvarLimitneg = 50;
const int propIDLEKVAR = 51;
const int propkVDC = 52;
const int propkp = 53;
const int propCtrlTol = 54;
const int propSMT = 55;
const int propSM = 56;
const int propDynEq = 57;
const int propDynOut = 58;
const int propGFM = 59;
const int propAmpsLimit = 60;
const int propAmpsError = 61;
const int NumPropsThisClass = 61; // Make this agree with the last property constant
complex cBuffer[25/*# range 1..24*/];  // Temp buffer for calcs  24-phase Storage element?
complex CDoubleOne = {};

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Storage elements

TStorage::TStorage()
{
	;
	Class_Name = "Storage";
	DSSClassType = DSSClassType + STORAGE_ELEMENT;  // In both PCelement and Storage element list
	ActiveElement = 0;

     // Set Register names
	RegisterNames[0] = "kWh";
	RegisterNames[1] = "kvarh";
	RegisterNames[2] = "Max kW";
	RegisterNames[3] = "Max kVA";
	RegisterNames[4] = "Hours";
	RegisterNames[5] = "Price($)";
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TStorage::~TStorage()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TStorage::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();   /*see DSSClass*/

     // Define Property names
     /*
      Using the AddProperty FUNCTION, you can list the properties here in the order you want
      them to appear when properties are accessed sequentially without tags.   Syntax:

      AddProperty( <name of property>, <index in the EDIT Case statement>, <help text>);

     */
	AddProperty("phases", 1, "Number of Phases, this Storage element.  Power is evenly divided among phases.");
	AddProperty("bus1", 2, "Bus to which the Storage element is connected.  May include specific node specification.");
	AddProperty("kv", propKV, String("Nominal rated (1.0 per unit) voltage, kV, for Storage element. For 2- and 3-phase Storage elements, specify phase-phase kV. " "Otherwise, specify actual kV across each branch of the Storage element. ") + CRLF
	           + CRLF
	           + "If wye (star), specify phase-neutral kV. "
	           + CRLF
	           + CRLF
	           + "If delta or phase-phase connected, specify phase-phase kV.");  // line-neutral voltage//  base voltage
	AddProperty("conn", PropConnection, "={wye|LN|delta|LL}.  Default is wye.");
	AddProperty("kW", propKW, "Get/set the requested kW value. Final kW is subjected to the inverter ratings. A positive value denotes power coming OUT of the element, "
	           "which is the opposite of a Load element. A negative value indicates the Storage element is in Charging state. "
	           "This value is modified internally depending on the dispatch mode.");
	AddProperty("kvar", propKVAR, "Get/set the requested kvar value. Final kvar is subjected to the inverter ratings. Sets inverter to operate in constant kvar mode.");
	AddProperty("pf", propPF, String("Get/set the requested PF value. Final PF is subjected to the inverter ratings. Sets inverter to operate in constant PF mode. Nominally, " "the power factor for discharging (acting as a generator). Default is 1.0. ") + CRLF
	           + CRLF
	           + "Enter negative for leading power factor "
	           + "(when kW and kvar have opposite signs.)"
	           + CRLF
	           + CRLF
	           + "A positive power factor signifies kw and kvar at the same direction.");
	AddProperty("kVA", propKVA, "Indicates the inverter nameplate capability (in kVA). "
	           "Used as the base for Dynamics mode and Harmonics mode values.");
	AddProperty("%Cutin", propCutin, "Cut-in power as a percentage of inverter kVA rating. It is the minimum DC power necessary to turn the inverter ON when it is OFF. "
	           "Must be greater than or equal to %CutOut. Defaults to 2 for PVSystems and 0 for Storage elements which means that the inverter state "
	           "will be always ON for this element.");
	AddProperty("%Cutout", propCutout, "Cut-out power as a percentage of inverter kVA rating. It is the minimum DC power necessary to keep the inverter ON. "
	           "Must be less than or equal to %CutIn. Defaults to 0, which means that, once ON, the inverter state "
	           "will be always ON for this element.");
	AddProperty("EffCurve", propInvEffCurve, "An XYCurve object, previously defined, that describes the PER UNIT efficiency vs PER UNIT of rated kVA for the inverter. "
	           "Power at the AC side of the inverter is discounted by the multiplier obtained from this curve.");
	AddProperty("VarFollowInverter", propVarFollowInverter, "Boolean variable (Yes|No) or (True|False). Defaults to False, which indicates that the reactive power generation/absorption does not respect the inverter status."
	           "When set to True, the reactive power generation/absorption will cease when the inverter status is off, due to DC kW dropping below %CutOut.  The reactive power "
	           "generation/absorption will begin again when the DC kW is above %CutIn.  When set to False, the Storage will generate/absorb reactive power regardless of the status of the inverter.");
	AddProperty("kvarMax", propkvarLimit, "Indicates the maximum reactive power GENERATION (un-signed numerical variable in kvar) for the inverter. Defaults to kVA rating of the inverter.");
	AddProperty("kvarMaxAbs", propkvarLimitneg, "Indicates the maximum reactive power ABSORPTION (un-signed numerical variable in kvar) for the inverter. Defaults to kvarMax.");
	AddProperty("WattPriority", propPpriority, "{Yes/No*/True/False} Set inverter to watt priority instead of the default var priority.");
	AddProperty("PFPriority", propPFpriority, "If set to true, priority is given to power factor and WattPriority is neglected. It works only if operating in either constant PF "
	           "or constant kvar modes. Defaults to False.");
	AddProperty("%PminNoVars", propPminNoVars, "Minimum active power as percentage of kWrated under which there is no vars production/absorption. Defaults to 0 (disabled).");
	AddProperty("%PminkvarMax", propPminkvarLimit, "Minimum active power as percentage of kWrated that allows the inverter to produce/absorb reactive power up to its maximum "
	           "reactive power, which can be either kvarMax or kvarMaxAbs, depending on the current operation quadrant. Defaults to 0 (disabled).");
	AddProperty("kWrated", propKWRATED, "kW rating of power output. Base for Loadshapes when DispMode=Follow. Sets kVA property if it has not been specified yet. "
	           "Defaults to 25.");
	AddProperty("%kWrated", proppctkWrated, "Upper limit on active power as a percentage of kWrated. Defaults to 100 (disabled).");
	AddProperty("kWhrated", propKWHRATED, "Rated Storage capacity in kWh. Default is 50.");
	AddProperty("kWhstored", propKWHSTORED, "Present amount of energy stored, kWh. Default is same as kWhrated.");
	AddProperty("%stored", propPCTSTORED, "Present amount of energy stored, % of rated kWh. Default is 100.");
	AddProperty("%reserve", propPCTRESERVE, String("Percentage of rated kWh Storage capacity to be held in reserve for normal operation. Default = 20. ") + CRLF
	           + "This is treated as the minimum energy discharge level unless there is an emergency. For emergency operation "
	           + "set this property lower. Cannot be less than zero.");
	AddProperty("State", propSTATE, "{IDLING | CHARGING | DISCHARGING}  Get/Set present operational state. In DISCHARGING mode, the Storage element "
	           "acts as a generator and the kW property is positive. The element continues discharging at the scheduled output power level "
	           "until the Storage reaches the reserve value. Then the state reverts to IDLING. "
	           "In the CHARGING state, the Storage element behaves like a Load and the kW property is negative. "
	           "The element continues to charge until the max Storage kWh is reached and then switches to IDLING state. "
	           "In IDLING state, the element draws the idling losses plus the associated inverter losses.");
	AddProperty("%Discharge", propPCTKWOUT, "Discharge rate (output power) in percentage of rated kW. Default = 100.");
	AddProperty("%Charge", propPCTKWIN, "Charging rate (input power) in percentage of rated kW. Default = 100.");
	AddProperty("%EffCharge", propCHARGEEFF, "Percentage efficiency for CHARGING the Storage element. Default = 90.");
	AddProperty("%EffDischarge", propDISCHARGEEFF, "Percentage efficiency for DISCHARGING the Storage element. Default = 90.");
	AddProperty("%IdlingkW", propIDLEKW, "Percentage of rated kW consumed by idling losses. Default = 1.");
	AddProperty("%Idlingkvar", propIDLEKVAR, "Deprecated.");
	AddProperty("%R", propPCTR, "Equivalent percentage internal resistance, ohms. Default is 0. Placed in series with internal voltage source"
	           " for harmonics and dynamics modes. Use a combination of %IdlingkW, %EffCharge and %EffDischarge to account for "
	           "losses in power flow modes.");
	AddProperty("%X", propPCTX, "Equivalent percentage internal reactance, ohms. Default is 50%. Placed in series with internal voltage source"
	           " for harmonics and dynamics modes. (Limits fault current to 2 pu.");
	AddProperty("model", propMODEL, String("Integer code (default=1) for the model to be used for power output variation with voltage. " "Valid values are:") + CRLF
	           + CRLF
	           + "1:Storage element injects/absorbs a CONSTANT power."
	           + CRLF
	           + "2:Storage element is modeled as a CONSTANT IMPEDANCE."
	           + CRLF
	           + "3:Compute load injection from User-written Model.");
	AddProperty("Vminpu", propVMINPU, "Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. "
	           "Below this value, the load model reverts to a constant impedance model.");
	AddProperty("Vmaxpu", propVMAXPU, "Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. "
	           "Above this value, the load model reverts to a constant impedance model.");
	AddProperty("Balanced", propBalanced, "{Yes | No*} Default is No. Force balanced current only for 3-phase Storage. Forces zero- and negative-sequence to zero. ");
	AddProperty("LimitCurrent", propLimited, "Limits current magnitude to Vminpu value for both 1-phase and 3-phase Storage similar to Generator Model 7. For 3-phase, "
	           "limits the positive-sequence current but not the negative-sequence.");
	AddProperty("yearly", propYEARLY, "Dispatch shape to use for yearly simulations.  Must be previously defined "
	           "as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated "
	           "during Yearly solution modes. In the default dispatch mode, "
	           "the Storage element uses this loadshape to trigger State changes.");
	AddProperty("daily", propDAILY, "Dispatch shape to use for daily simulations.  Must be previously defined "
	           "as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, "
	           "the Storage element uses this loadshape to trigger State changes."); // daily dispatch (hourly)
	AddProperty("duty", propDUTY, String("Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. " "Must be previously defined as a Loadshape object. ") + CRLF
	           + CRLF
	           + "Typically would have time intervals of 1-5 seconds. "
	           + CRLF
	           + CRLF
	           + "Designate the number of points to solve using the Set Number=xxxx command. "
	           + "If there are fewer points in the actual shape, the shape is assumed to repeat.");  // as for wind generation
	AddProperty("DispMode", propDISPMODE, String("{DEFAULT | FOLLOW | EXTERNAL | LOADLEVEL | PRICE } Default = \"DEFAULT\". Dispatch mode. ") + CRLF
	           + CRLF
	           + "In DEFAULT mode, Storage element state is triggered to discharge or charge at the specified rate by the "
	           + "loadshape curve corresponding to the solution mode. "
	           + CRLF
	           + CRLF
	           + "In FOLLOW mode the kW output of the Storage element follows the active loadshape multiplier "
	           + "until Storage is either exhausted or full. "
	           + "The element discharges for positive values and charges for negative values.  The loadshape is based on rated kW. "
	           + CRLF
	           + CRLF
	           + "In EXTERNAL mode, Storage element state is controlled by an external Storagecontroller2. "
	           + "This mode is automatically set if this Storage element is included in the element list of a StorageController element. "
	           + CRLF
	           + CRLF
	           + "For the other two dispatch modes, the Storage element state is controlled by either the global default Loadlevel value or the price level. ");
	AddProperty("DischargeTrigger", propDISPOUTTRIG, String("Dispatch trigger value for discharging the Storage. ") + CRLF
	           + "If = 0.0 the Storage element state is changed by the State command or by a StorageController2 object. "
	           + CRLF
	           + "If <> 0  the Storage element state is set to DISCHARGING when this trigger level is EXCEEDED by either the specified "
	           + "Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.");
	AddProperty("ChargeTrigger", propDISPINTRIG, String("Dispatch trigger value for charging the Storage. ") + CRLF
	           + CRLF
	           + "If = 0.0 the Storage element state is changed by the State command or StorageController2 object.  "
	           + CRLF
	           + CRLF
	           + "If <> 0  the Storage element state is set to CHARGING when this trigger level is GREATER than either the specified "
	           + "Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.");
	AddProperty("TimeChargeTrig", propCHARGETIME, "Time of day in fractional hours (0230 = 2.5) at which Storage element will automatically go into charge state. "
	           "Default is 2.0.  Enter a negative time value to disable this feature.");
	AddProperty("class", propCLASS, "An arbitrary integer number representing the class of Storage element so that Storage values may "
	           "be segregated by class."); // integer
	AddProperty("DynaDLL", propDynaDLL, "Name of DLL containing user-written dynamics model, which computes the terminal currents for Dynamics-mode simulations, "
	           "overriding the default model.  Set to \"none\" to negate previous setting. "
	           "This DLL has a simpler interface than the UserModel DLL and is only used for Dynamics mode.");
	AddProperty("DynaData", propDynaData, "String (in quotes or parentheses if necessary) that gets passed to the user-written dynamics model Edit function for defining the data required for that model.");
	AddProperty("UserModel", propUSERMODEL, "Name of DLL containing user-written model, which computes the terminal currents for both power flow and dynamics, "
	           "overriding the default model.  Set to \"none\" to negate previous setting.");
	AddProperty("UserData", propUSERDATA, "String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.");
	AddProperty("debugtrace", propDEBUGTRACE, "{Yes | No }  Default is no.  Turn this on to capture the progress of the Storage model "
	           "for each iteration.  Creates a separate file for each Storage element named \"Storage_name.CSV\".");
	AddProperty("kVDC", propkVDC,
		"Indicates the rated voltage (kV) at the input of the inverter while the storage is discharging. The value is normally greater or equal to the kV base of the Storage device. It is used for dynamics simulation ONLY.");

	AddProperty("Kp", propkp,
		"It is the proportional gain for the PI controller within the inverter. Use it to modify the controller response in dynamics simulation mode.");

	AddProperty("PITol", propCtrlTol,
		"It is the tolerance (%) for the closed loop controller of the inverter. For dynamics simulation mode.");

	AddProperty("SafeVoltage", propSMT,
		"Indicates the voltage level (%) respect to the base voltage level for which the Inverter will operate. If this threshold is violated, the Inverter will enter safe mode (OFF). For dynamic simulation. By default is 80%");

	AddProperty("SafeMode", propSM,
		"(Read only) Indicates whether the inverter entered (Yes) or not (No) into Safe Mode.");
	AddProperty("DynamicEq", propDynEq,
		string("The name of the dynamic equation (DinamicExp) that will be used for defining the dynamic behavior of the generator. ") +
		"if not defined, the generator dynamics will follow the built-in dynamic equation.");
	AddProperty("DynOut", propDynOut,
		string("The name of the variables within the Dynamic equation that will be used to govern the PVSystem dynamics.") +
		"This PVsystem model requires 1 output from the dynamic equation: " + CRLF + CRLF +
		"1. Current." + CRLF +
		"The output variables need to be defined in the same order.");
	AddProperty("ControlMode", propGFM,
		string("Defines the control mode for the inverter. It can be one of {GFM | GFL*}. By default it is GFL (Grid Following Inverter).") +
		" Use GFM (Grid Forming Inverter) for energizing islanded microgrids, but, if the device is conencted to the grid, it is highly recommended to use GFL." + CRLF + CRLF +
		"GFM control mode disables any control action set by the InvControl device.");
	AddProperty("AmpLimit", propAmpsLimit,
		"Is the current limiter per phase for the IBR when operating in GFM mode. This limit is imposed to prevent the IBR to enter into Safe Mode when reaching the IBR power ratings." + CRLF +
		"Once the IBR reaches this value, it remains there without moving into Safe Mode. This value needs to be set lower than the IBR Amps rating.");
	AddProperty("AmpLimitGain", propAmpsError,
		"Use it for fine tunning the current limiter when active, by default is 0.8, it has to be a value between 0.1 and 1. This value allows users to fine tune the IBRs current limiter to match with the user requirements.");

	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // Override default help string
	PropertyHelp[NumPropsThisClass] = "Name of harmonic voltage or current spectrum for this Storage element. "
	           "Current injection is assumed for inverter. "
	           "Default value is \"default\", which is defined when the DSS starts.";
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TStorage::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new Storage element and add it to Storage class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TStorageObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TStorage::SetNcondsForConnection()
{
	/*# with ActiveStorageObj do */
	{
		auto with0 = ActiveStorageObj;
		switch(with0->Connection)
		{
			case 	0:
			with0->Set_Nconds(with0->Fnphases + 1);
			break;
			case 	1:
			switch(with0->Fnphases)
			{
				case 	1:
				 case 2:
				with0->Set_Nconds(with0->Fnphases + 1);
				break; // L-L and Open-delta
				default:
				with0->Set_Nconds(with0->Fnphases);
				break;
			}
			break;
			default:
			  ;
			break;
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TStorage::UpdateAll(int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = ElementList.get_myNumList(), i = 1; i <= stop; i++)
	{
		/*# with TStorageObj(ElementList.Get(i)) do */
		{
			auto with0 = ((TStorageObj*) ElementList.Get(i));
			if(with0->Get_Enabled())
				with0->UpdateStorage(ActorID);
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN

void TStorage::InterpretConnection(const String s)
{
	String TestS;
	/*# with ActiveStorageObj do */
	{
		auto with0 = ActiveStorageObj;
		TestS = LowerCase(s);
		switch(TestS[0])
		{
			case 	L'y':
			 case L'w':
			with0->Connection = 0;
			break;  /*Wye*/
			case 	L'd':
			with0->Connection = 1;
			break;  /*Delta or line-Line*/
			case 	L'l':
			switch(TestS[1])
			{
				case 	L'n':
				with0->Connection = 0;
				break;
				case 	L'l':
				with0->Connection = 1;
				break;
				default:
				  ;
				break;
			}
			break;
			default:
			  ;
			break;
		}
		SetNcondsForConnection();

          /*VBase is always L-N voltage unless 1-phase device or more than 3 phases*/
		switch(with0->Fnphases)
		{
			case 	2:
			 case 3:
			with0->VBase = with0->StorageVars.kVStorageBase * InvSQRT3x1000;
			break;    // L-N Volts
			default:
			with0->VBase = with0->StorageVars.kVStorageBase * 1000.0;   // Just use what is supplied
			break;
		}
		with0->VBase95 = with0->Vminpu * with0->VBase;
		with0->VBase105 = with0->Vmaxpu * with0->VBase;
		with0->Yorder = with0->Fnconds * with0->Fnterms;
		with0->Set_YprimInvalid(ActiveActor,true);
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int InterpretDispMode(const String s)
{
	int result = 0;
	switch(LowerCase(s)[0])
	{
		case 	L'e':
		result = STORE_EXTERNALMODE;
		break;
		case 	L'f':
		result = STORE_FOLLOW;
		break;
		case 	L'l':
		result = STORE_LOADMODE;
		break;
		case 	L'p':
		result = STORE_PRICEMODE;
		break;
		default:
		result = STORE_DEFAULT;
		break;
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

String ReturnDispMode(int iMode)
{
	String result;
	switch(iMode)
	{
		case 	STORE_EXTERNALMODE:
		result = "External";
		break;
		case 	STORE_FOLLOW:
		result = "Follow";
		break;
		case 	STORE_LOADMODE:
		result = "Loadshape";
		break;
		case 	STORE_PRICEMODE:
		result = "Price";
		break;
		default:
		result = "default";
		break;
	}
	return result;
}



//- - - - - - - - - - - - - - -MAIN EDIT FUNCTION - - - - - - - - - - - - - - -

int TStorage::Edit(int ActorID)
{
	int result			= 0,
		i				= 0,
		iCase			= 0,
		ParamPointer	= 0,
		VarIdx;
	String	ParamName	= "",
			Param		= "";

  // continue parsing with contents of Parser
	ActiveStorageObj = (TStorageObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveStorageObj);
	result = 0;
	/*# with ActiveStorageObj do */
	{
		auto with0 = ActiveStorageObj;
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();  // Parse next property off the command line
		Param = Parser[ActorID]->MakeString_();   // Put the string value of the property value in local memory for faster access
		while(Param.size() > 0)
		{
			if(ParamName.size() == 0)       // If it is not a named property, assume the next property
				++ParamPointer;
			else
				ParamPointer = CommandList.Getcommand(ParamName);  // Look up the name in the list for this class
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))   // Update the string value of the property
				( (TDSSObject*) with0 )->Set_PropertyValue((PropertyIdxMap)[ParamPointer - 1], Param);
			else
			{
				VarIdx	=  with0->CheckIfDynVar(ParamName, ActorID);
				if (VarIdx < 0)
					DoSimpleMsg(String("Unknown parameter \"") + ParamName + "\" for Storage \"" + with0->get_Name() + "\"", 560);
			}
			if(ParamPointer > 0)
			{
				iCase = (PropertyIdxMap)[ParamPointer - 1];
				switch(iCase)
				{
					case 	0:
					DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 561);
					break;
					case 	1:
						with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
					break;
					case 	2:
						with0->SetBus(1, Param);
					break;
					case 	propKV:
						with0->Set_PresentkV(Parser[ActorID]->MakeDouble_());
					break;
					case 	propKW:
						with0->Set_kW( Parser[ActorID]->MakeDouble_());
					break;
					case 	propPF:
					{
						with0->Set_Varmode(VARMODEPF);
						with0->PFNominal = Parser[ActorID]->MakeDouble_();
					}
					break;
					case 	propMODEL:
						with0->VoltageModel = Parser[ActorID]->MakeInteger_();
					break;
					case 	propYEARLY:
						with0->YearlyShape = Param;
					break;
					case 	propDAILY:
						with0->DailyShape = Param;
					break;
					case 	propDUTY:
						with0->DutyShape = Param;
					break;
					case 	propDISPMODE:
						with0->DispatchMode = InterpretDispMode(Param);
					break;
					case 	PropConnection:
					InterpretConnection(Param);
					break;
					case 	propKVAR:
					{
						with0->Set_Varmode(VARMODEKVAR);
						with0->Set_kvarRequested(Parser[ActorID]->MakeDouble_());
					}
					break;
					case 	propPCTR:
						with0->pctR = Parser[ActorID]->MakeDouble_();
					break;
					case 	propPCTX:
						with0->pctX = Parser[ActorID]->MakeDouble_();
					break;
					case 	propIDLEKW:
						with0->pctIdlekW = Parser[ActorID]->MakeDouble_();
					break;
					case 	propIDLEKVAR:
					;
					break;  // Do nothing. Deprecated property.
					case 	propCLASS:
						with0->StorageClass = Parser[ActorID]->MakeInteger_();
					break;
					case 	propInvEffCurve:
						with0->InverterCurve = Param;
					break;
					case 	propDISPOUTTRIG:
						with0->DischargeTrigger = Parser[ActorID]->MakeDouble_();
					break;
					case 	propDISPINTRIG:
						with0->ChargeTrigger = Parser[ActorID]->MakeDouble_();
					break;
					case 	propCHARGEEFF:
						with0->pctChargeEff = Parser[ActorID]->MakeDouble_();
					break;
					case 	propDISCHARGEEFF:
						with0->pctDischargeEff = Parser[ActorID]->MakeDouble_();
					break;
					case 	propPCTKWOUT:
						with0->Set_pctkWOut(Parser[ActorID]->MakeDouble_());
					break;
					case 	propCutin:
						with0->FpctCutIn = Parser[ActorID]->MakeDouble_();
					break;
					case 	propCutout:
						with0->FpctCutOut = Parser[ActorID]->MakeDouble_();
					break;
					case 	propVMINPU:
						with0->Vminpu = Parser[ActorID]->MakeDouble_();
					break;
					case 	propVMAXPU:
						with0->Vmaxpu = Parser[ActorID]->MakeDouble_();
					break;
					case 	propSTATE:
						with0->fState = with0->InterpretState(Param);
					break; //****
					case 	propKVA:
					/*# with StorageVars do */
					{
						auto& with1 = with0->StorageVars;
						with1.FkVArating = Parser[ActorID]->MakeDouble_();
						with0->kVASet = true;
						if(!with0->kvarLimitSet)
							with0->StorageVars.Fkvarlimit = with1.FkVArating;
						if(!with0->kvarLimitSet && !with0->kvarLimitNegSet)
							with0->StorageVars.Fkvarlimitneg = with1.FkVArating;
					}
					break;
					case 	propKWRATED:
						with0->StorageVars.kWrating = Parser[ActorID]->MakeDouble_();
					break;
					case 	propKWHRATED:
						with0->StorageVars.kWhRating = Parser[ActorID]->MakeDouble_();
					break;
					case 	propKWHSTORED:
						with0->StorageVars.kWhStored = Parser[ActorID]->MakeDouble_();
					break;
					case 	propPCTRESERVE:
						with0->pctReserve = Parser[ActorID]->MakeDouble_();
					break;
					case 	propUSERMODEL:
						with0->UserModel->Set_Name(Parser[ActorID]->MakeString_());
					break;  // Connect to user written models
					case 	propUSERDATA:
						with0->UserModel->Set_Edit(Parser[ActorID]->MakeString_());
					break;  // Send edit string to user model
					case 	propDEBUGTRACE:
						with0->DebugTrace = InterpretYesNo(Param);
					break;
					case 	propPCTKWIN:
						with0->Set_pctkWIn(Parser[ActorID]->MakeDouble_());
					break;
					case 	propPCTSTORED:
						with0->StorageVars.kWhStored = Parser[ActorID]->MakeDouble_() * 0.01 * with0->StorageVars.kWhRating;
					break;
					case 	propCHARGETIME:
						with0->ChargeTime = Parser[ActorID]->MakeDouble_();
					break;
					case 	propDynaDLL:
						with0->DynaModel->FName = Parser[ActorID]->MakeString_();
					break;
					case 	propDynaData:
						with0->DynaModel->Set_Edit(Parser[ActorID]->MakeString_());
					break;
					case 	proppctkWrated:
						with0->StorageVars.FpctkWrated = Parser[ActorID]->MakeDouble_() / 100.0;
					break;  // convert to pu
					case 	propBalanced:
						with0->ForceBalanced = InterpretYesNo(Param);
					break;
					case 	propLimited:
						with0->CurrentLimited = InterpretYesNo(Param);
					break;
					case 	propVarFollowInverter:
						with0->FVarFollowInverter = InterpretYesNo(Param);
					break;
					case 	propkvarLimit:
					{
						with0->StorageVars.Fkvarlimit = Abs( Parser[ActorID]->MakeDouble_());
						with0->kvarLimitSet = true;
						if(!with0->kvarLimitNegSet)
							with0->StorageVars.Fkvarlimitneg = Abs(with0->StorageVars.Fkvarlimit);
					}
					break;
					case 	propPpriority:
						with0->StorageVars.P_Priority = InterpretYesNo(Param);
					break;  // watt priority flag
					case 	propPFpriority:
						with0->StorageVars.PF_Priority = InterpretYesNo(Param);
					break;
					case 	propPminNoVars:
						with0->FpctPminNoVars = Parser[ActorID]->MakeDouble_();
					break;
					case 	propPminkvarLimit:
						with0->FpctPminkvarLimit = Parser[ActorID]->MakeDouble_();
					break;
					case 	propkvarLimitneg:
					{
						with0->StorageVars.Fkvarlimitneg = Abs( Parser[ActorID]->MakeDouble_());
						with0->kvarLimitNegSet = true;
					}
					break;
					case	propkVDC:
						with0->myDynVars.RatedVDC          = Parser[ActorID]->MakeDouble_() * 1000;
					break;
					case	propkp:
						with0->myDynVars.Kp = Parser[ActorID]->MakeDouble_() / 1000;
					break;
					case	propCtrlTol:
						with0->myDynVars.CtrlTol = Parser[ActorID]->MakeDouble_() / 100;
					break;
					case	propSMT:
						with0->myDynVars.SMThreshold = Parser[ActorID]->MakeDouble_();
					break;
					case	propDynEq:
						with0->DynamicEq = Param;
					break;
					case	propDynOut:
						with0->SetDynOutput(Param);
					break;
					case	propGFM:
					{
						if (LowerCase(Parser[ActorID]->MakeString_()) == "gfm")
						{
							with0->GFM_Mode = true;
							with0->myDynVars.ResetIBR = false;
							if (with0->myDynVars.Vgrid.size() < with0->Get_NPhases())
								with0->myDynVars.Vgrid.resize(with0->Get_NPhases());
						}
						else
							with0->GFM_Mode = false;

						with0->Set_YprimInvalid(ActorID, true);
					}
					break;
                    case 	propAmpsLimit:
                        with0->myDynVars.ILimit = Parser[ActorID]->MakeDouble_();
						break;
                    case	propAmpsError:
                        with0->myDynVars.VError = Parser[ActorID]->MakeDouble_();
						break;
               // Inherited parameters
					default:
					inherited::ClassEdit(ActiveStorageObj, ParamPointer - NumPropsThisClass);
					break;
				}
				switch(iCase)
				{
					case 	1:
					SetNcondsForConnection();
					break;  // Force Reallocation of terminal info
                // (PR) Make sure if we will need it
					                /* removed
                propKW,propPF: Begin
                                 SyncUpPowerQuantities;   // keep kvar nominal up to date with kW and PF

                               End;       */

        /*Set loadshape objects;  returns nil If not valid*/
					case 	propYEARLY:
					with0->YearlyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->YearlyShape));
					break;
					case 	propDAILY:
					with0->DailyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DailyShape));
					break;
					case 	propDUTY:
					with0->DutyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DutyShape));
					break;
					case 	propKWRATED:
					if(!with0->kVASet)
						with0->StorageVars.FkVArating = with0->StorageVars.kWrating;
					break;
					case 	propKWHRATED:
					{
						with0->StorageVars.kWhStored = with0->StorageVars.kWhRating; // Assume fully charged
						with0->kWhBeforeUpdate = with0->StorageVars.kWhStored;
						with0->StorageVars.kWhReserve = with0->StorageVars.kWhRating * with0->pctReserve * 0.01;
					}
					break;
					case 	propPCTRESERVE:
					with0->StorageVars.kWhReserve = with0->StorageVars.kWhRating * with0->pctReserve * 0.01;
					break;
					case 	propInvEffCurve:
						with0->InverterCurveObj = ( (TXYcurveObj*) XYCurveClass[ActorID]->Find(with0->InverterCurve) );
					break;
					case 	propDEBUGTRACE:
					if(with0->DebugTrace)   // Init trace file
					{
						int stop = 0;
						AssignFile(with0->Tracefile, GetOutputDirectory() + "STOR_" + with0->get_Name() + ".CSV");
						Rewrite(with0->Tracefile);
						IOResultToException();
						Write(with0->Tracefile, "t, Iteration, LoadMultiplier, Mode, LoadModel, StorageModel,  Qnominalperphase, Pnominalperphase, CurrentType");
						for(stop = with0->Get_NPhases(), i = 1; i <= stop; i++)
						{
							Write(with0->Tracefile, String(", |Iinj") + IntToStr(i) + "|");
						}
						for(stop = with0->Get_NPhases(), i = 1; i <= stop; i++)
						{
							Write(with0->Tracefile, String(", |Iterm") + IntToStr(i) + "|");
						}
						for(stop = with0->Get_NPhases(), i = 1; i <= stop; i++)
						{
							Write(with0->Tracefile, String(", |Vterm") + IntToStr(i) + "|");
						}
						for(stop = with0->NumVariables(), i = 1; i <= stop; i++)
						{
							{ Write(with0->Tracefile, ", "); Write(with0->Tracefile, with0->VariableName(i)); }
						}
						Write(with0->Tracefile, ",Vthev, Theta");
						WriteLn(with0->Tracefile);
						CloseFile(with0->Tracefile);
					}
					break;
					case 	propUSERMODEL:
						with0->IsUserModel = with0->UserModel->Get_Exists();
					break;
					case 	propDynaDLL:
						with0->IsUserModel = with0->DynaModel->Get_Exists();
					break;
					case	propDynEq:
					{
						with0->DynamicEqObj = (TDynamicExpObj*) TDynamicExpClass[ActorID]->Find(with0->DynamicEq);
						if (ASSIGNED(with0->DynamicEqObj))
						{
							with0->DynamicEqVals.resize(with0->DynamicEqObj->get_FNumVars());
							for (int idx = 0; idx < with0->DynamicEqVals.size(); idx++)
								with0->DynamicEqVals[idx].resize(2);
						}
					}
					break;

//                propPFPriority: For i := 1 to ControlElementList.ListSize Do
//                Begin
//
//                  if TControlElem(ControlElementList.Get(i)).ClassName = 'InvControl'  Then
//                      // Except for VW mode, all other modes (including combined ones) can operate with PF priority
//                      if (TInvControlObj(ControlElementList.Get(i)).Mode <> 'VOLTWATT') Then
//                          StorageVars.PF_Priority := FALSE; // For all other modes
//
//                End;
					default:
					  ;
					break;
				}
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		with0->RecalcElementData(ActorID);
		with0->Set_YprimInvalid(ActorID,true);
	}
	return result;
}

//----------------------------------------------------------------------------


// Copy over essential properties from other object

int TStorage::MakeLike(const String OtherStorageObjName)
{
	int result = 0;
	TStorageObj* OtherStorageObj = nullptr;
	int i = 0;
	result = 0;
     /*See If we can find this line name in the present collection*/
	OtherStorageObj = ((TStorageObj*) Find(OtherStorageObjName));
	if(OtherStorageObj != nullptr)
		/*# with ActiveStorageObj do */
		{
			auto with0 = ActiveStorageObj;
			int stop = 0;
			if(with0->Fnphases != OtherStorageObj->Fnphases)
			{
				with0->Set_NPhases(OtherStorageObj->Fnphases);
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->StorageVars.kVStorageBase = OtherStorageObj->StorageVars.kVStorageBase;
			with0->VBase = OtherStorageObj->VBase;
			with0->Vminpu = OtherStorageObj->Vminpu;
			with0->Vmaxpu = OtherStorageObj->Vmaxpu;
			with0->VBase95 = OtherStorageObj->VBase95;
			with0->VBase105 = OtherStorageObj->VBase105;
			with0->kW_out = OtherStorageObj->kW_out;
			with0->kvar_out = OtherStorageObj->kvar_out;
			with0->Pnominalperphase = OtherStorageObj->Pnominalperphase;
			with0->PFNominal = OtherStorageObj->PFNominal;
			with0->Qnominalperphase = OtherStorageObj->Qnominalperphase;
			with0->Connection = OtherStorageObj->Connection;
			with0->YearlyShape = OtherStorageObj->YearlyShape;
			with0->YearlyShapeObj = OtherStorageObj->YearlyShapeObj;
			with0->DailyShape = OtherStorageObj->DailyShape;
			with0->DailyShapeObj = OtherStorageObj->DailyShapeObj;
			with0->DutyShape = OtherStorageObj->DutyShape;
			with0->DutyShapeObj = OtherStorageObj->DutyShapeObj;
			with0->DispatchMode = OtherStorageObj->DispatchMode;
			with0->InverterCurve = OtherStorageObj->InverterCurve;
			with0->InverterCurveObj = OtherStorageObj->InverterCurveObj;
			with0->StorageClass = OtherStorageObj->StorageClass;
			with0->VoltageModel = OtherStorageObj->VoltageModel;
			with0->fState = OtherStorageObj->fState;
			with0->FStateChanged = OtherStorageObj->FStateChanged;
			with0->kvarLimitSet = OtherStorageObj->kvarLimitSet;
			with0->kvarLimitNegSet = OtherStorageObj->kvarLimitNegSet;
			with0->FpctCutIn = OtherStorageObj->FpctCutIn;
			with0->FpctCutOut = OtherStorageObj->FpctCutOut;
			with0->FVarFollowInverter = OtherStorageObj->FVarFollowInverter;
			with0->StorageVars.Fkvarlimit = OtherStorageObj->StorageVars.Fkvarlimit;
			with0->StorageVars.Fkvarlimitneg = OtherStorageObj->StorageVars.Fkvarlimitneg;
			with0->StorageVars.FkVArating = OtherStorageObj->StorageVars.FkVArating;
			with0->FpctPminNoVars = OtherStorageObj->FpctPminNoVars;
			with0->FpctPminkvarLimit = OtherStorageObj->FpctPminkvarLimit;
			with0->kWOutIdling = OtherStorageObj->kWOutIdling;
			with0->StorageVars.kWrating = OtherStorageObj->StorageVars.kWrating;
			with0->StorageVars.kWhRating = OtherStorageObj->StorageVars.kWhRating;
			with0->StorageVars.kWhStored = OtherStorageObj->StorageVars.kWhStored;
			with0->StorageVars.kWhReserve = OtherStorageObj->StorageVars.kWhReserve;
			with0->kWhBeforeUpdate = OtherStorageObj->kWhBeforeUpdate;
			with0->pctReserve = OtherStorageObj->pctReserve;
			with0->DischargeTrigger = OtherStorageObj->DischargeTrigger;
			with0->ChargeTrigger = OtherStorageObj->ChargeTrigger;
			with0->pctChargeEff = OtherStorageObj->pctChargeEff;
			with0->pctDischargeEff = OtherStorageObj->pctDischargeEff;
			with0->Set_pctkWOut(OtherStorageObj->get_FpctkWout());
			with0->Set_pctkWIn(OtherStorageObj->Get_FpctkWIn());
			with0->pctIdlekW = OtherStorageObj->pctIdlekW;
			with0->pctIdlekvar = OtherStorageObj->pctIdlekvar;
			with0->ChargeTime = OtherStorageObj->ChargeTime;
			with0->pctR = OtherStorageObj->pctR;
			with0->pctX = OtherStorageObj->pctX;
			with0->RandomMult = OtherStorageObj->RandomMult;
			with0->FVWMode = OtherStorageObj->FVWMode;
			with0->FVVMode = OtherStorageObj->FVVMode;
			with0->FDRCMode = OtherStorageObj->FDRCMode;
			with0->FWPMode = OtherStorageObj->FWPMode;
			with0->FWVMode = OtherStorageObj->FWVMode;
			with0->FAVRMode = OtherStorageObj->FAVRMode;
			with0->UserModel->FName = OtherStorageObj->UserModel->FName;  // Connect to user written models
			with0->DynaModel->FName = OtherStorageObj->DynaModel->FName;
			with0->IsUserModel = OtherStorageObj->IsUserModel;
			with0->ForceBalanced = OtherStorageObj->ForceBalanced;
			with0->CurrentLimited = OtherStorageObj->CurrentLimited;
			ClassMakeLike(OtherStorageObj);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				(with0->FPropertyValue)[i - 1] = (OtherStorageObj->FPropertyValue)[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Storage MakeLike: \"") + OtherStorageObjName
	           + "\" Not Found.", 562);
	return result;
}

//----------------------------------------------------------------------------

int TStorage::Init(int Handle, int ActorID)
{
	int result = 0;
	TStorageObj* P = nullptr;
	if(Handle == 0)  // init all
	{
		P = (TStorageObj*) ElementList.Get_First();
		while((P != nullptr))
		{
			P->Randomize(0);
			P = (TStorageObj*) ElementList.Get_Next();
		}
	}
	else
	{
		Set_Active(Handle);
		P = ((TStorageObj*) GetActiveObj());
		P->Randomize(0);
	}
	DoSimpleMsg("Need to implement TStorage.Init", -1);
	result = 0;
	return result;
}

/*--------------------------------------------------------------------------*/  // Force all EnergyMeters in the circuit to reset

void TStorage::ResetRegistersAll()
{
	int Idx = 0;
	Idx = Get_First();
	while(Idx > 0)
	{
		((TStorageObj*) GetActiveObj())->ResetRegisters();
		Idx = Get_Next();
	}
}

/*--------------------------------------------------------------------------*/  // Force all Storage elements in the circuit to take a sample

void TStorage::SampleAll(int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = ElementList.get_myNumList(), i = 1; i <= stop; i++)
	{
		/*# with TStorageObj(ElementList.Get(i)) do */
		{
			auto with0 = ((TStorageObj*) ElementList.Get(i));
			if(with0->Get_Enabled())
				with0->TakeSample(ActorID);
		}
	}
}

//----------------------------------------------------------------------------

TStorageObj::TStorageObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			MaxDynPhaseCurrent(0.0),
			DebugTrace(false),
			fState(0),
			FStateChanged(false),
			FirstSampleAfterReset(false),
			StorageSolutionCount(0),
			StorageFundamental(0.0),
			StorageObjSwitchOpen(false),
			ForceBalanced(false),
			CurrentLimited(false),
			pctR(0.0),
			pctX(0.0),
			OpenStorageSolutionCount(0),
			Pnominalperphase(0.0),
			Qnominalperphase(0.0),
			RandomMult(0.0),
			Reg_Hours(0),
			Reg_kvarh(0),
			Reg_kWh(0),
			Reg_MaxkVA(0),
			Reg_MaxkW(0),
			Reg_Price(0),
			IsUserModel(false),
			UserModel(nullptr),
			DynaModel(nullptr),
			kvarBase(0.0),
			VBase(0.0),
			VBase105(0.0),
			VBase95(0.0),
			Vmaxpu(0.0),
			Vminpu(0.0),
			YPrimOpenCond(nullptr),
			Connection(0),
			DailyShapeObj(nullptr),
			DutyShapeObj(nullptr),
			StorageClass(0),
			VoltageModel(0),
			PFNominal(0.0),
			YearlyShapeObj(nullptr),
			FpctkWout(0.0),
			Fpctkvarout(0.0),
			pctReserve(0.0),
			DispatchMode(0),
			kVANotSet(false),
			kvar_out(0.0),
			kW_out(0.0),
			pctIdlekW(0.0),
			pctIdlekvar(0.0),
			pctChargeEff(0.0),
			pctDischargeEff(0.0),
			DischargeTrigger(0.0),
			ChargeTrigger(0.0),
			ChargeTime(0.0),
			kWhBeforeUpdate(0.0)
{
	int i;

	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType; // + Storage_ELEMENT;  // In both PCelement and Storageelement list
	Set_NPhases(3);
	Fnconds = 4;  // defaults to wye
	Yorder = 0;  // To trigger an initial allocation
	Set_NTerms(1);  // forces allocations
	YearlyShape = "";
	YearlyShapeObj = nullptr;  // If YearlyShapeobj = nil Then the load alway stays nominal * global multipliers
	DailyShape = "";
	DailyShapeObj = nullptr;  // If DaillyShapeobj = nil Then the load alway stays nominal * global multipliers
	DutyShape = "";
	DutyShapeObj = nullptr;  // If DutyShapeobj = nil Then the load alway stays nominal * global multipliers
	InverterCurveObj = nullptr;
	InverterCurve = "";
	Connection = 0;    // Wye (star)
	VoltageModel = 1;  /*Typical fixed kW negative load*/
	StorageClass = 1;
	StorageSolutionCount = -1;  // For keep track of the present solution in Injcurrent calcs
	OpenStorageSolutionCount = -1;
	YPrimOpenCond = nullptr;
	StorageVars.kVStorageBase = 12.47;
	VBase = 7200.0;
	Vminpu = 0.90;
	Vmaxpu = 1.10;
	VBase95 = Vminpu * VBase;
	VBase105 = Vmaxpu * VBase;
	Yorder = Fnterms * Fnconds;
	RandomMult = 1.0;
	Set_Varmode(VARMODEPF);
	FInverterON = true; // start with inverterON
	kVA_exceeded = false;
	FVarFollowInverter = false;
	ForceBalanced = false;
	CurrentLimited = false;
	NumStateVars = NumStorageVariables;
	/*# with StorageVars do */
	{
		auto& with0				= StorageVars;
		auto& with01			= myDynVars;
		with0.kWrating			= 25.0;
		with0.FkVArating		= with0.kWrating;
		with0.kWhRating			= 50;
		with0.kWhStored			= with0.kWhRating;
		kWhBeforeUpdate			= with0.kWhRating;
		with0.kWhReserve		= with0.kWhRating * pctReserve / 100.0;
		with0.Fkvarlimit		= with0.FkVArating;
		with0.Fkvarlimitneg		= with0.FkVArating;
		with0.FpctkWrated		= 1.0;
		with0.P_Priority		= false;
		with0.PF_Priority		= false;
		with0.EffFactor			= 1.0;
		with0.Vreg				= 9999;
		with0.Vavg				= 9999;
		with0.VVOperation		= 9999;
		with0.VWOperation		= 9999;
		with0.DRCOperation		= 9999;
		with0.VVDRCOperation	= 9999;
		with0.WPOperation		= 9999;
		with0.WVOperation		= 9999;
		with01.RatedVDC			= 8000;
		with01.SMThreshold		= 80;
		with01.SafeMode			= false;
		with01.Kp				= 0.00001;
        with01.ILimit = -1;         // No Amps limit
        with01.IComp = 0;
        with01.VError = 0.8;
		PICtrl.resize(0);
	}
	FDCkW = 25.0;
	FpctCutIn = 0.0;
	FpctCutOut = 0.0;
	FpctPminNoVars = -1.0; // Deactivated by default
	FpctPminkvarLimit = -1.0; // Deactivated by default
	Fpf_wp_nominal = 1.0;

     /*Output rating stuff*/
	kvar_out = 0.0;
     // removed kvarBase     := kvar_out;     // initialize
	PFNominal = 1.0;
	pctR = 0.0;
	pctX = 50.0;

     /*Make the StorageVars struct as public*/
	PublicDataStruct = &StorageVars;
	PublicDataSize = sizeof(TStorageVars);
	IsUserModel = false;
	UserModel = new TStoreUserModel();
	DynaModel = new TStoreDynaModel();
	fState = STORE_IDLING;  // Idling and fully charged
	FStateChanged = true;  // Force building of YPrim
	pctReserve = 20.0;  // per cent of kWhRating
	pctIdlekW = 1.0;
	pctIdlekvar = 0.0;
	DischargeTrigger = 0.0;
	ChargeTrigger = 0.0;
	pctChargeEff = 90.0;
	pctDischargeEff = 90.0;
	FpctkWout = 100.0;
	FpctkWin = 100.0;
	ChargeTime = 2.0;   // 2 AM
	kVASet = false;
	kvarLimitSet = false;
	kvarLimitNegSet = false;
	Reg_kWh = 1 - 1;
	Reg_kvarh = 2 - 1;
	Reg_MaxkW = 3 - 1;
	Reg_MaxkVA = 4 - 1;
	Reg_Hours = 5 - 1;
	Reg_Price = 6 - 1;
	DebugTrace = false;
	StorageObjSwitchOpen = false;
	Spectrum = "";  // override base class
	SpectrumObj = nullptr;
	FVWMode = false;
	FVVMode = false;
	FDRCMode = false;
	FWPMode = false;
	FWVMode = false;
	FAVRMode = false;
	InitPropertyValues(0);
	RecalcElementData(ActiveActor);

	for (i = 0; i < NumStorageRegisters; i++)
	{
		Registers[i] = 0.0;
		Derivatives[i] = 0.0;
	}
}


//----------------------------------------------------------------------------

String TStorageObj::DecodeState()
{
	String result;
	switch(fState)
	{
		case 	STORE_CHARGING:
		result = "CHARGING";
		break;
		case 	STORE_DISCHARGING:
		result = "DISCHARGING";
		break;
		default:
		result = "IDLING";
		break;
	}
	return result;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_FkVARating()
{
	return StorageVars.FkVArating;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_FpctkWrated()
{
	return StorageVars.FpctkWrated;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_Fkvarlimit()
{
	return StorageVars.Fkvarlimit;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_Fkvarlimitneg()
{
	return StorageVars.Fkvarlimitneg;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_FpctkWIn()
{
	return FpctkWin;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_InverterLosses()
{
	double Result;

	switch (get_fState())
	{
		case STORE_IDLING: 
			Result = abs(Get_Power(1, ActiveActor).re * 0.001) - abs(Get_DCkW());
			break;
		case STORE_CHARGING: 
			Result = abs(Get_Power(1, ActiveActor).re * 0.001) - abs(Get_DCkW());
			break;
		case STORE_DISCHARGING:
			Result = Get_DCkW() - abs(Get_Power(1, ActiveActor).re * 0.001);
			break;
		default:
			Result = 0.0;
	}

	return Result;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_kWChDchLosses()
{
	double Result;

	auto& with0 = StorageVars;
	
	switch (get_fState())
	{
		case STORE_IDLING: 
			Result = 0.0;
			break;
		case STORE_CHARGING:
			if (abs(Get_DCkW()) - PIdling > 0)
				Result = (abs(Get_DCkW()) - PIdling) * (1.0 - 0.01 * pctChargeEff); // most cases will fall here
			else 
				Result = -1 * (abs(Get_DCkW()) - PIdling) * (1.0 / (0.01 * pctDischargeEff) - 1.0); // exceptional cases when Pidling is higher than Get_DCkW() (net effect is that the ideal Storage will be discharged)
			break;
		case STORE_DISCHARGING: 
			Result = (Get_DCkW() + PIdling) * (1.0 / (0.01 * pctDischargeEff) - 1.0);
			break;
		default :
			Result = 0.0;
	}
	return Result;
}

//----------------------------------------------------------------------------
/*
void TStorageObj::ComputeDCkW()
{
	TCoeff* coefGuess;
	TCoeff* coef;
	(coefGuess)[1] = 0.0;
	(coefGuess)[2] = 0.0;

	(coef)[1] = 1.0;
	(coef)[2] = 1.0;  // just a guess

	FDCkW = Power[1][ActiveActor].re * 0.001;  // Assume ideal inverter

	if (!ASSIGNED(InverterCurveObj))
	{
		// make sure sign is correct
		if (fState = STORE_IDLING)
			FDCkW = abs(FDCkW) * -1;
		else 
			FDCkW = abs(FDCkW) * fState;
	}
	else
	{
		int N_tentatives = 0;
		while (((coef)[1] != (coefGuess)[1]) && ((coef)[2] != (coefGuess)[2]) || (N_tentatives > 9))
		{
			N_tentatives = N_tentatives + 1;
			coefGuess = &( InverterCurveObj.GetCoefficients(abs(FDCkW) / StorageVars.FkVArating) );


			switch (fState)
			{
				case STORE_DISCHARGING: 
					FDCkW = QuadSolver((coefGuess)[1] / StorageVars.FkVArating, (coefGuess)[2], -1.0 * abs(Power[1][ActiveActor].re * 0.001));
					break;
				case STORE_CHARGING: STORE_IDLING:
					FDCkW = abs(FDCkW) * (coefGuess)[2] / (1.0 - ((coefGuess)[1] * abs(FDCkW) / StorageVars.FkVArating));
			}
			// Final coefficients
			coef = &( InverterCurveObj.GetCoefficients(abs(FDCkW) / StorageVars.FkVArating) );
		}

		// make sure sign is correct
		if (fState = STORE_IDLING)
			FDCkW = abs(FDCkW) * -1;
		else 
			FDCkW = abs(FDCkW) * fState;
	}
	free(coefGuess);
	free(coef);
}*/

//----------------------------------------------------------------------------

double TStorageObj::Get_DCkW()
{
	ComputeDCkW();
	return FDCkW;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_VminPu()
{
	return Vminpu;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_pf_wp_nominal(double myPFNom)
{
	Fpf_wp_nominal = myPFNom;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_Pmin()
{
	return -StorageVars.kWrating * Get_FpctkWIn() / 100.0;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_Pmax()
{
	return StorageVars.kWrating * get_FpctkWout() / 100.0;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_qMaxInj()
{
	return StorageVars.Fkvarlimit;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_qMaxAbs()
{
	return StorageVars.Fkvarlimitneg;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_acVmin()
{
	return ( Get_PresentkV() * Vminpu );
}

//----------------------------------------------------------------------------

double TStorageObj::Get_acVmax()
{
	return ( Get_PresentkV() * Vmaxpu );
}

//----------------------------------------------------------------------------

double TStorageObj::Get_acVnom()
{
	return Get_PresentkV();
}

//----------------------------------------------------------------------------

double TStorageObj::Get_pMaxUnderPF()
{
	auto& with0 = StorageVars;
	return sqrt( Sqr(with0.FkVArating) - Sqr(with0.Fkvarlimitneg));
}

//----------------------------------------------------------------------------

double TStorageObj::Get_pMaxOverPF()
{
	auto& with0 = StorageVars;
	return sqrt(Sqr(with0.FkVArating) - Sqr(with0.Fkvarlimit));
}

//----------------------------------------------------------------------------

double TStorageObj::Get_pMaxCharge()
{
	return abs(Get_Pmin());
}

//----------------------------------------------------------------------------

double TStorageObj::Get_sMaxCharge()
{
	return StorageVars.FkVArating;
}

//----------------------------------------------------------------------------

bool TStorageObj::Get_CIMDynamicMode()
{
    bool result = false;

	result = (FVWMode || FVVMode || FWVMode || FAVRMode || FDRCMode);

	return result;
}

//----------------------------------------------------------------------------


// Define default values for the properties

void TStorageObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1, "3");         //'phases';
	Set_PropertyValue(2, GetBus(1));   //'bus1';
	Set_PropertyValue(propKV, Format("%-g", StorageVars.kVStorageBase));
	Set_PropertyValue(propKW, Format("%-g", kW_out));
	Set_PropertyValue(propPF, Format("%-g", PFNominal));
	Set_PropertyValue(propMODEL, "1");
	Set_PropertyValue(propYEARLY, "");
	Set_PropertyValue(propDAILY, "");
	Set_PropertyValue(propDUTY, "");
	Set_PropertyValue(propDISPMODE, "Default");
	Set_PropertyValue(PropConnection, "wye");
	Set_PropertyValue(propKVAR, Format("%-g", Get_Presentkvar()));
	Set_PropertyValue(propPCTR, Format("%-g", pctR));
	Set_PropertyValue(propPCTX, Format("%-g", pctX));
	Set_PropertyValue(propIDLEKW, "1");       // PERCENT
	Set_PropertyValue(propIDLEKVAR, "");   // deprecated
	Set_PropertyValue(propCLASS, "1"); //'class'
	Set_PropertyValue(propDISPOUTTRIG, "0");   // 0 MEANS NO TRIGGER LEVEL
	Set_PropertyValue(propDISPINTRIG, "0");
	Set_PropertyValue(propCHARGEEFF, "90");
	Set_PropertyValue(propDISCHARGEEFF, "90");
	Set_PropertyValue(propPCTKWOUT, "100");
	Set_PropertyValue(propPCTKWIN, "100");
	Set_PropertyValue(propInvEffCurve, "");
	Set_PropertyValue(propCutin, "0");
	Set_PropertyValue(propCutout, "0");
	Set_PropertyValue(propVarFollowInverter, "NO");
	Set_PropertyValue(propVMINPU, "0.90");
	Set_PropertyValue(propVMAXPU, "1.10");
	Set_PropertyValue(propSTATE, "IDLING");
		/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		Set_PropertyValue(propKVA, Format("%-g", StorageVars.FkVArating));
		Set_PropertyValue(propkvarLimit, Format("%-g", with0.Fkvarlimit));
		Set_PropertyValue(propkvarLimitneg, Format("%-g", with0.Fkvarlimitneg));
		Set_PropertyValue(propKWRATED, Format("%-g", with0.kWrating));
		Set_PropertyValue(propKWHRATED, Format("%-g", with0.kWhRating));
		Set_PropertyValue(propKWHSTORED, Format("%-g", with0.kWhStored));
		Set_PropertyValue(propPCTSTORED, Format("%-g", with0.kWhStored / with0.kWhRating * 100.0));
	}
	Set_PropertyValue(propPCTRESERVE, Format("%-g", pctReserve));
	Set_PropertyValue(propCHARGETIME, Format("%-g", ChargeTime));
	Set_PropertyValue(propUSERMODEL, "");  // Usermodel
	Set_PropertyValue(propUSERDATA, "");  // Userdata
	Set_PropertyValue(propDynaDLL, "");  //
	Set_PropertyValue(propDynaData, "");  //
	Set_PropertyValue(propDEBUGTRACE, "NO");
	Set_PropertyValue(propBalanced, "NO");
	Set_PropertyValue(propLimited, "NO");
	Set_PropertyValue(proppctkWrated, "100");  // Included
	Set_PropertyValue(propPpriority, "NO");   // Included
	Set_PropertyValue(propPFpriority, "NO");
	Set_PropertyValue(propkVDC, "8000");
	Set_PropertyValue(propkp,"0.00001");
	Set_PropertyValue(propCtrlTol,"5");
	Set_PropertyValue(propSMT,"80");
	Set_PropertyValue(propSM,"NO");
	Set_PropertyValue(propGFM,"GFL");

	inherited::InitPropertyValues(NumPropsThisClass);
}


//----------------------------------------------------------------------------

String TStorageObj::GetPropertyValue(int Index)
{
	String result = "";

	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		switch(Index)
		{
			case 	propKV:				result = Format("%.6g", StorageVars.kVStorageBase);
				break;
			case 	propKW:				result = Format("%.6g", kW_out);
				break;
			case 	propPF:				result = Format("%.6g", PFNominal);
				break;
			case 	propMODEL:			result = Format("%d", VoltageModel);
				break;
			case 	propYEARLY:			result = YearlyShape;
				break;
			case 	propDAILY:			result = DailyShape;
				break;
			case 	propDUTY:			result = DutyShape;
				break;
			case 	propDISPMODE:		result = ReturnDispMode(DispatchMode);
				break;

          /*propCONNECTION :;*/
			case 	propKVAR:			result = Format("%.6g", kvar_out);
				break;
			case 	propPCTR:			result = Format("%.6g", pctR);
				break;
			case 	propPCTX:			result = Format("%.6g", pctX);
				break;
			case 	propIDLEKW:			result = Format("%.6g", pctIdlekW);
				break;
			case 	propIDLEKVAR:		result = "";
				break;       // deprecated
			          /*propCLASS      = 17;*/
			case 	propInvEffCurve:	result = InverterCurve;
				break;
			case 	propCutin:			result = Format("%.6g", FpctCutIn);
				break;
			case 	propCutout:			result = Format("%.6g", FpctCutOut);
				break;
			case 	propVarFollowInverter:
				if(FVarFollowInverter)
					result = "Yes";
				else
					result = "No";
				break;
			case 	propPminNoVars:		result = Format("%.6g", FpctPminNoVars);
				break;
			case 	propPminkvarLimit:	result = Format("%.6g", FpctPminkvarLimit);
				break;
			case 	propDISPOUTTRIG:	result = Format("%.6g", DischargeTrigger);
				break;
			case 	propDISPINTRIG:		result = Format("%.6g", ChargeTrigger);
				break;
			case 	propCHARGEEFF:		result = Format("%.6g", pctChargeEff);
				break;
			case 	propDISCHARGEEFF:	result = Format("%.6g", pctDischargeEff);
				break;
			case 	propPCTKWOUT:		result = Format("%.6g", get_FpctkWout());
				break;
			case 	propVMINPU:			result = Format("%.6g", Vminpu);
				break;
			case 	propVMAXPU:			result = Format("%.6g", Vmaxpu);
				break;
			case 	propSTATE:			result = DecodeState();
				break;

          /*StorageVars*/
			case 	propKVA:			result = Format("%.6g", with0.FkVArating);
				break;
			case 	propKWRATED:		result = Format("%.6g", with0.kWrating);
				break;
			case 	propKWHRATED:		result = Format("%.6g", with0.kWhRating);
				break;
			case 	propKWHSTORED:		result = Format("%.6g", with0.kWhStored);
				break;
			case 	propPCTRESERVE:		result = Format("%.6g", pctReserve);
				break;
			case 	propUSERMODEL:		result = UserModel->get_FName();
				break;
			case 	propUSERDATA:		result = String("(") + inherited::GetPropertyValue(Index) + ")";
				break;
			case 	proppctkWrated:		result = Format("%.6g", with0.FpctkWrated * 100.0);
				break;
			case 	propDynaDLL:		result = DynaModel->get_FName();
				break;
			case 	propDynaData:		result = String("(") + inherited::GetPropertyValue(Index) + ")";
				break;
          /*propDEBUGTRACE = 33;*/
			case 	propPCTKWIN:		result = Format("%.6g", Get_FpctkWIn());
				break;
			case 	propPCTSTORED:		result = Format("%.6g", with0.kWhStored / with0.kWhRating * 100.0);
				break;
			case 	propCHARGETIME:		result = Format("%.6g", ChargeTime);
				break;
			case 	propBalanced:
				if(ForceBalanced)
					result = "Yes";
				else
					result = "No";
				break;
			case 	propLimited:
				if(CurrentLimited)
					result = "Yes";
				else
					result = "No";
				break;
			case 	propkvarLimit:		result = Format("%.6g", with0.Fkvarlimit);
				break;
			case 	propkvarLimitneg:	result = Format("%.6g", with0.Fkvarlimitneg);
			break;  // take the generic handler
			case	propkVDC:			result = Format("%.6g", myDynVars.RatedVDC / 1000);
				break;
			case	propkp:				result = Format("%.10g", myDynVars.Kp * 1000);
				break;
			case	propCtrlTol:		result = Format("%.6g", myDynVars.CtrlTol * 100);
				break;
			case	propSMT:			result = Format("%.6g", myDynVars.SMThreshold);
				break;
			case	propSM:				if (myDynVars.SafeMode) result = "Yes"; else result = "NO";
				break;
			case	propDynEq:			result = DynamicEq;
				break;
			case	propDynOut:			result = GetDynOutputStr();
				break;
			case	propGFM:			if (GFM_Mode) result = "GFM"; else result = "GFL";
				break;
			default:
			result = inherited::GetPropertyValue(Index);
			break;
		}
	}
	return result;
}


//----------------------------------------------------------------------------

void TStorageObj::Randomize(int Opt)
{
	switch(Opt)
	{
		case 	0:
		RandomMult = 1.0;
		break;
		case 	GAUSSIAN:
		RandomMult = Gauss(YearlyShapeObj->Get_Mean(), YearlyShapeObj->Get_StdDev());
		break;
		case 	UNIFORM:
		RandomMult = (double) Random();
		break;  // number between 0 and 1.0
		case 	LOGNORMAL:
		RandomMult = QuasiLogNormal(YearlyShapeObj->Get_Mean());
		break;
		default:
		  ;
		break;
	}
}

//----------------------------------------------------------------------------

TStorageObj::~TStorageObj()
{
	delete YPrimOpenCond;
	delete UserModel;
	delete DynaModel;
	// inherited::Destroy();
}



//----------------------------------------------------------------------------

void TStorageObj::CalcDailyMult(double hr, int ActorID)
{
	if(DailyShapeObj != nullptr)
	{
		ShapeFactor = DailyShapeObj->GetMult(hr);
	}
	else
	ShapeFactor = CDoubleOne;  // Default to no  variation
	CheckStateTriggerLevel(ShapeFactor.re, ActorID);   // last recourse
}


//----------------------------------------------------------------------------

void TStorageObj::CalcDutyMult(double hr, int ActorID)
{
	if(DutyShapeObj != nullptr)
	{
		ShapeFactor = DutyShapeObj->GetMult(hr);
		CheckStateTriggerLevel(ShapeFactor.re, ActorID);
	}
	else
	CalcDailyMult(hr, ActorID);  // Default to Daily Mult If no duty curve specified
}

//----------------------------------------------------------------------------

void TStorageObj::CalcYearlyMult(double hr, int ActorID)
{
	if(YearlyShapeObj != nullptr)
	{
		ShapeFactor = YearlyShapeObj->GetMult(hr);
		CheckStateTriggerLevel(ShapeFactor.re, ActorID);
	}
	else
	CalcDailyMult(hr, ActorID);  // Defaults to Daily curve
}

//----------------------------------------------------------------------------

void TStorageObj::RecalcElementData(int ActorID)
{
	VBase95 = Vminpu * VBase;
	VBase105 = Vmaxpu * VBase;

   // removed 5/8/17 kvarBase := kvar_out ;  // remember this for Follow Mode
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		YeqDischarge = cmplx((with0.kWrating * 1000.0 / Sqr(VBase) / Fnphases), 0.0);

      // values in ohms for thevenin equivalents
		with0.RThev = pctR * 0.01 * Sqr(Get_PresentkV()) / with0.FkVArating * 1000.0L;      // Changed
		with0.XThev = pctX * 0.01 * Sqr(Get_PresentkV()) / with0.FkVArating * 1000.0L;      // Changed
		CutInkW = FpctCutIn * with0.FkVArating / 100.0;
		CutOutkW = FpctCutOut * with0.FkVArating / 100.0;
		if(FpctPminNoVars <= 0)
			PminNoVars = -1.0;
		else
			PminNoVars = FpctPminNoVars * with0.kWrating / 100.0;
		if(FpctPminkvarLimit <= 0)
			PminkvarLimit = -1.0;
		else
			PminkvarLimit = FpctPminkvarLimit * with0.kWrating / 100.0;

      // efficiencies
		with0.ChargeEff = pctChargeEff * 0.01;
		with0.DisChargeEff = pctDischargeEff * 0.01;
		PIdling = pctIdlekW * with0.kWrating / 100.0;
		if(InverterCurveObj != nullptr)
		{
			kWOutIdling = double(PIdling) / (InverterCurveObj->GetYValue_(double(PIdling) / (with0.FkVArating)));
		}
		else
		kWOutIdling = PIdling; 
	}
	SetNominalStorageOutput(ActorID);

    /*Now check for errors.  If any of these came out nil and the string was not nil, give warning*/
	if(YearlyShapeObj == nullptr)
	{
		if(YearlyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Yearly load shape: \"") + YearlyShape
	           + "\" Not Found.", 563);
	}
	if(DailyShapeObj == nullptr)
	{
		if(DailyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Daily load shape: \"") + DailyShape
	           + "\" Not Found.", 564);
	}
	if(DutyShapeObj == nullptr)
	{
		if(DutyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Duty load shape: \"") + DutyShape
	           + "\" Not Found.", 565);
	}
	if(Spectrum.size() > 0)
	{
		SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
		if(SpectrumObj == nullptr)
			DoSimpleMsg(String("ERROR! Spectrum \"") + Spectrum + "\" Not Found.", 566);
	}
	else
	SpectrumObj = nullptr;

    // Initialize to Zero - defaults to PQ Storage element
    // Solution object will reset after circuit modifications
	InjCurrent = (pComplexArray) realloc(InjCurrent, sizeof(complex) * Yorder);

    /*Update any user-written models*/
	if(UserModel->Get_Exists())
		UserModel->FUpdateModel();  // Checks for existence and Selects
	if(DynaModel->Get_Exists())
		DynaModel->FUpdateModel();  // Checks for existence and Selects
}
//----------------------------------------------------------------------------

void TStorageObj::SetNominalStorageOutput(int ActorID)
{
	ShapeFactor = CDoubleOne;  // init here; changed by curve routine
    // Check to make sure the Storage element is ON
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
	{
		
		auto with1 = ActiveCircuit[ActorID]->Solution;
		if(!(with1->IsDynamicModel || with1->IsHarmonicModel))     // Leave Storage element in whatever state it was prior to entering Dynamic mode

          // Check dispatch to see what state the Storage element should be in
		{
			switch(DispatchMode)
			{
				case 	STORE_EXTERNALMODE:
				;
				break;  // Do nothing
				case 	STORE_LOADMODE:
				CheckStateTriggerLevel(ActiveCircuit[ActorID]->GeneratorDispatchReference, ActorID);
				break;
				case 	STORE_PRICEMODE:
				CheckStateTriggerLevel(ActiveCircuit[ActorID]->PriceSignal, ActorID);
				break; // dispatch off element's loadshapes, If any
				default:
				/*# with Solution do */
				{
					auto with2 = ActiveCircuit[ActorID]->Solution;
					switch(with2->Get_SolMode())
					{
						case 	SNAPSHOT:
						;
						break; /*Just solve for the present kW, kvar*/  // Don't check for state change
						case 	DAILYMODE:
						CalcDailyMult(with2->DynaVars.dblHour, ActorID);
						break; // Daily dispatch curve
						case 	YEARLYMODE:
						CalcYearlyMult(with2->DynaVars.dblHour, ActorID);
						break;
             /*
                MONTECARLO1,
                MONTEFAULT,
                FAULTSTUDY,
                DYNAMICMODE:   ; // {do nothing for these modes}
             */
                                // This mode allows use of one class of load shape
						case 	GENERALTIME:
						{
							switch(ActiveCircuit[ActorID]->ActiveLoadShapeClass)
							{
								case 	USEDAILY:
								CalcDailyMult(with2->DynaVars.dblHour, ActorID);
								break;
								case 	USEYEARLY:
								CalcYearlyMult(with2->DynaVars.dblHour, ActorID);
								break;
								case 	USEDUTY:
								CalcDutyMult(with2->DynaVars.dblHour, ActorID);
								break;
								default:     // default to 1 + j1 if not known
								ShapeFactor = CDoubleOne;
								break;
							}
						}
						break;
                // Assume Daily curve, If any, for the following
						case 	MONTECARLO2: case MONTECARLO3: case LOADDURATION1: case LOADDURATION2:
						CalcDailyMult(with2->DynaVars.dblHour, ActorID);
						break;
						case 	PEAKDAY:
						CalcDailyMult(with2->DynaVars.dblHour, ActorID);
						break;
						case 	DUTYCYCLE:
						CalcDutyMult(with2->DynaVars.dblHour, ActorID);
						break;
                /*AUTOADDFLAG:  ; */
						default:
						  ;
						break;
					}
				}
				break;
			}
			ComputekWkvar();

          /*
           Pnominalperphase is net at the terminal.  If supplying idling losses, when discharging,
           the Storage supplies the idling losses. When charging, the idling losses are subtracting from the amount
           entering the Storage element.
          */
			/*# with StorageVars do */
			{
				auto& with3 = StorageVars;
				Pnominalperphase = 1000.0 * kW_out / Fnphases;
				Qnominalperphase = 1000.0 * kvar_out / Fnphases;
			}
			switch(VoltageModel)
			{
				case 	3:
            //****  Fix this when user model gets connected in
 // Yeq := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim
				;
				break;
             /*
              Yeq no longer used for anything other than this calculation of Yeq95, Yeq105 and
              constant Z power flow model
             */
				default:
				Yeq = cdivreal(cmplx(Pnominalperphase, -Qnominalperphase), Sqr(VBase));   // Vbase must be L-N for 3-phase
				if(Vminpu != 0.0)  // at 95% voltage
					Yeq95 = cdivreal(Yeq, Sqr(Vminpu));
				else
					Yeq95 = Yeq; // Always a constant Z model
				if(Vmaxpu != 0.0)   // at 105% voltage
					Yeq105 = cdivreal(Yeq, Sqr(Vmaxpu));
				else
					Yeq105 = Yeq;
				break;
			}
          /* Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
          */
			/*# with StorageVars do */
			{
				auto& with4 = StorageVars;
				PhaseCurrentLimit = cdivreal(cmplx(Pnominalperphase, Qnominalperphase), VBase95);
				MaxDynPhaseCurrent = cabs(PhaseCurrentLimit);
			}

              /* When we leave here, all the Yeq's are in L-N values*/
		}  /*If  NOT (IsDynamicModel or IsHarmonicModel)*/
	}  /*With ActiveCircuit[ActiveActor]*/

   // If Storage element state changes, force re-calc of Y matrix
	if(FStateChanged)
	{
		Set_YprimInvalid(ActorID,true);
		FStateChanged = false;  // reset the flag
	}
}
// ===========================================================================================

void TStorageObj::ComputekWkvar()
{
	ComputePresentkW();
	ComputeInverterPower(); // apply inverter eff after checking for cutin/cutout
}
//----------------------------------------------------------------------------

void TStorageObj::ComputePresentkW()
{
	int OldState = 0;
	OldState = fState;
	FStateDesired = OldState;
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		switch(fState)
		{
			case 	STORE_CHARGING:
			{
				if(with0.kWhStored < with0.kWhRating)
					switch(DispatchMode)
					{
						case 	STORE_FOLLOW:
						{
							kW_out = with0.kWrating * ShapeFactor.re;
							FpctkWin = Abs( ShapeFactor.re) * 100.0;  // keep %charge updated
						}
						break;
						default:
						kW_out = -with0.kWrating * Get_FpctkWIn() / 100.0;
						break;
					}
				else
					fState = STORE_IDLING;   // all charged up
			}
			break;
			case 	STORE_DISCHARGING:
			{
				if(with0.kWhStored > with0.kWhReserve)
					switch(DispatchMode)
					{
						case 	STORE_FOLLOW:
						{
							kW_out = with0.kWrating * ShapeFactor.re;
							FpctkWout = Abs( ShapeFactor.re) * 100.0;  // keep %discharge updated
						}
						break;
						default:
						kW_out = with0.kWrating * get_FpctkWout() / 100.0;
						break;
					}
				else
					fState = STORE_IDLING;  // not enough Storage to discharge
			}
			break;
			default:
			  ;
			break;
		}
	}

    /*If idling output is only losses*/
	if(fState == STORE_IDLING)
	{
		kW_out = -kWOutIdling;
	}
	if(OldState != fState)
		FStateChanged = true;
}

// ===========================================================================================

void TStorageObj::ComputeInverterPower()
{
	double kVA_Gen = 0.0;
	int OldState = 0;
	double TempPF = 0.0; // temporary power factor
	double Qramp_limit = 0.0;

    // Reset CurrentkvarLimit to kvarLimit
	CurrentkvarLimit = StorageVars.Fkvarlimit;
	CurrentkvarLimitNeg = StorageVars.Fkvarlimitneg;
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		if(InverterCurveObj != nullptr)
		{
			if(fState == STORE_DISCHARGING)
			{
				FCutOutkWAC = CutOutkW * InverterCurveObj->GetYValue_(double(Abs(CutOutkW)) / with0.FkVArating);
				FCutInkWAC = CutInkW * InverterCurveObj->GetYValue_(double(Abs(CutInkW)) / with0.FkVArating);
			}
			else
  // Charging or Idling
			{
				FCutOutkWAC = double(CutOutkW) / InverterCurveObj->GetYValue_(double(Abs(CutOutkW)) / with0.FkVArating);
				FCutInkWAC = double(CutInkW) / InverterCurveObj->GetYValue_(double(Abs(CutInkW)) / with0.FkVArating);
			}
		}
		else
 // Assume Ideal Inverter
		{
			FCutOutkWAC = CutOutkW;
			FCutInkWAC = CutInkW;
		}
		OldState = fState;

      // CutIn/CutOut checking performed on the AC side.
		if(FInverterON)
		{
			if(Abs(kW_out) < FCutOutkWAC)
			{
				FInverterON = false;
				fState = STORE_IDLING;
			}
		}
		else
		{
			if(Abs(kW_out) >= FCutInkWAC)
			{
				FInverterON = true;
			}
			else
			{
				fState = STORE_IDLING;
			}
		}
		if(OldState != fState)
			FStateChanged = true;

      // Set inverter output
		if(FInverterON)
		{
			kWOut_Calc();
		}
		else

        // Idling
		{
			kW_out = -kWOutIdling; // In case it has just turned off due to %CutIn/%CutOut. Necessary to make sure SOC will be kept constant (higher priority than the %CutIn/%CutOut operation)
		}


      // Calculate kvar value based on operation mode (PF or kvar)
		if(fState == STORE_IDLING)      // Should watt-pf with watt=0 be applied here?
        // If in Idling state, check for kvarlimit only
		{
			if(Get_Varmode() == VARMODEPF)
//              kvar_out := 0.0; //kW = 0 leads to kvar = 0 in constant PF Mode
			{
				kvar_out = kW_out * sqrt(1.0L / Sqr(PFNominal) - 1.0L) * Sign(PFNominal);
				if((kvar_out > 0.0) && (Abs( kvar_out) > with0.Fkvarlimit))
					kvar_out = with0.Fkvarlimit;
				else
				{
					if((kvar_out < 0.0) && (Abs( kvar_out) > with0.Fkvarlimitneg))
						kvar_out = with0.Fkvarlimitneg * Sign(Get_kvarRequested());
				}
			}
			else
  // kvarRequested might have been set either internally or by an InvControl
			{
				double kvarReq = Get_kvarRequested();
				if((kvarReq > 0.0) && (Abs(kvarReq) > with0.Fkvarlimit))
					kvar_out = with0.Fkvarlimit;
				else
				{
					if((kvarReq < 0.0) && (Abs( kvarReq) > with0.Fkvarlimitneg))
						kvar_out = with0.Fkvarlimitneg * Sign(kvarReq);
					else
						kvar_out = kvarReq;
				}
			}
		}
		else

        // If in either Charging or Discharging states
		{
			if(Abs( kW_out) < PminNoVars)
			{
				kvar_out = 0.0;  // Check minimum P for Q gen/absorption. if PminNoVars is disabled (-1), this will always be false
				CurrentkvarLimit = 0;
				CurrentkvarLimitNeg = 0.0;  // InvControl uses this.
			}
			else
			{
				if(Get_Varmode() == VARMODEPF)
				{
					if(PFNominal == 1.0)
						kvar_out = 0.0;
					else
					{
						kvar_out = kW_out * sqrt(1.0L / Sqr(PFNominal) - 1.0L) * Sign(PFNominal); //kvar_out desired by constant PF

                        // Check Limits
						if(Abs( kW_out) < PminkvarLimit) // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.

                            // straight line starts at max(PminNoVars, FCutOutkWAC)
                            // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
						{
							if(Abs( kW_out) >= max(PminNoVars, FCutOutkWAC))
							{
								if(kvar_out > 0.0)
								{
									Qramp_limit = double(with0.Fkvarlimit) / PminkvarLimit * Abs( kW_out);   // generation limit
								}
								else
								{
									if(kvar_out < 0.0)
									{
										Qramp_limit = double(with0.Fkvarlimitneg) / PminkvarLimit * Abs( kW_out);   // absorption limit
									}
								}
								if(Abs( kvar_out) > Qramp_limit)
								{
									kvar_out = Qramp_limit * Sign(kW_out) * Sign(PFNominal);
									if(kvar_out > 0)
										CurrentkvarLimit = Qramp_limit;  // For use in InvControl
									if(kvar_out < 0)
										CurrentkvarLimitNeg = Qramp_limit;  // For use in InvControl
								}
							}
						}
						else
						{
							if((Abs( kvar_out) > with0.Fkvarlimit) || (Abs( kvar_out) > with0.Fkvarlimitneg))  // Other cases, check normal kvarLimit and kvarLimitNeg
							{
								if(kvar_out > 0.0)
									kvar_out = with0.Fkvarlimit * Sign(kW_out) * Sign(PFNominal);
								else
									kvar_out = with0.Fkvarlimitneg * Sign(kW_out) * Sign(PFNominal);
								if(with0.PF_Priority) // Forces constant power factor when kvar limit is exceeded and PF Priority is true.
								{
									kW_out = kvar_out * sqrt(1.0 / (1.0L - Sqr(PFNominal)) - 1.0) * Sign(PFNominal);
								}
							}
						}
					}
				}
				else
  // VARMODE kvar

                  // Check limits
				{
					if(Abs( kW_out) < PminkvarLimit) // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.

                      // straight line starts at max(PminNoVars, FCutOutkWAC)
                      // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
					{
						if(Abs( kW_out) >= max(PminNoVars, FCutOutkWAC))
						{
							if(with0.kvarRequested > 0.0)
							{
								Qramp_limit = double(with0.Fkvarlimit) / PminkvarLimit * Abs( kW_out);   // generation limit
								CurrentkvarLimit = Qramp_limit;    // For use in InvControl
							}
							else
							{
								if(with0.kvarRequested < 0.0)
								{
									Qramp_limit = double(with0.Fkvarlimitneg) / PminkvarLimit * Abs( kW_out);   // absorption limit
									CurrentkvarLimitNeg = Qramp_limit;   // For use in InvControl
								}
							}
							if(Abs( with0.kvarRequested) > Qramp_limit)
								kvar_out = Qramp_limit * Sign(with0.kvarRequested);
							else
								kvar_out = with0.kvarRequested;
						}
					}
					else
					{
						double kvarReq = Get_kvarRequested();
						if(((kvarReq > 0.0) && (Abs( kvarReq) > with0.Fkvarlimit)) || ((kvarReq < 0.0) && (Abs( kvarReq) > with0.Fkvarlimitneg)))
						{
							if(kvarReq > 0.0)
								kvar_out = with0.Fkvarlimit * Sign(kvarReq);
							else
								kvar_out = with0.Fkvarlimitneg * Sign(kvarReq);
							if((Get_Varmode() == VARMODEKVAR) && with0.PF_Priority && FWPMode)
							{
								kW_out = Abs( kvar_out) * sqrt(1.0 / (1.0L - Sqr(Fpf_wp_nominal)) - 1.0) * Sign(kW_out);

                      // Forces constant power factor when kvar limit is exceeded and PF Priority is true. Temp PF is calculated based on kvarRequested
                      // PF Priority is not valid if controlled by an InvControl operating in at least one amongst VV and DRC modes
							}
							else
							{
								if(with0.PF_Priority && (!FVVMode || !FDRCMode || !FWVMode))
								{
									if(Abs( with0.kvarRequested) > 0.0)
									{
										TempPF = cos(atan(Abs( (Get_kvarRequested() / kW_out))));
										kW_out = Abs( kvar_out) * sqrt(1.0 / (1.0L - Sqr(TempPF)) - 1.0) * Sign(kW_out);
									}
								}
							}
						}
						else
						kvar_out = Get_kvarRequested();
					}
				}
			}
		}
		if((FInverterON == false) && (FVarFollowInverter == true))
			kvar_out = 0.0;

      // Limit kvar and kW so that kVA of inverter is not exceeded
		kVA_Gen = sqrt(Sqr(kW_out) + Sqr(kvar_out));
		if(kVA_Gen > with0.FkVArating)
		{
			kVA_exceeded = true;

          // Expectional case: When kVA is exceeded and in idling state, we force P priority always
			if(fState == STORE_IDLING)
			{
				kvar_out = sqrt(Sqr(with0.FkVArating) - Sqr(kW_out)) * Sign(kvar_out);

          // Regular Cases
			}
			else
			{
				if((Get_Varmode() == VARMODEPF) && with0.PF_Priority)
            // Operates under constant power factor when kVA rating is exceeded. PF must be specified and PFPriority must be TRUE
				{
					kW_out = with0.FkVArating * Abs( PFNominal) * Sign(kW_out);
					kvar_out = with0.FkVArating * sqrt(1.0L - Sqr(PFNominal)) * Sign(kW_out) * Sign(PFNominal);
				}
				else
				{
					if((Get_Varmode() == VARMODEKVAR) && with0.PF_Priority && FWPMode)
					{
						kW_out = with0.FkVArating * Abs(Fpf_wp_nominal) * Sign(kW_out);
						kvar_out = with0.FkVArating * Abs( sin(acos(Fpf_wp_nominal))) * Sign(Get_kvarRequested());
					}
					else
					{
						if((Get_Varmode() == VARMODEKVAR) && with0.PF_Priority && (!FVVMode || !FDRCMode || !FWVMode))
            // Operates under constant power factor (PF implicitly calculated based on kw and kvar)
						{
							if(Abs( kvar_out) == with0.Fkvarlimit)   // for handling cases when kvar limit and inverter's kVA limit are exceeded
							{
								kW_out = with0.FkVArating * Abs( TempPF) * Sign(kW_out);  // Temp PF has already been calculated at this point
							}
							else
							{
								kW_out = with0.FkVArating * Abs( cos(atan(with0.kvarRequested / kW_out))) * Sign(kW_out);
							}
							kvar_out = with0.FkVArating * Abs( sin(acos(kW_out / with0.FkVArating))) * Sign(Get_kvarRequested());
						}
						else
						{
							if(with0.P_Priority)  // back off the kvar
							{
								if(kW_out > with0.FkVArating)
								{
									kW_out = with0.FkVArating;
									kvar_out = 0.0;
								}
								else
								kvar_out = sqrt(Sqr(with0.FkVArating) - Sqr(kW_out)) * Sign(kvar_out);
							}
							else
							kW_out = sqrt(Sqr(with0.FkVArating) - Sqr(kvar_out)) * Sign(kW_out); // Q Priority   (Default) back off the kW
						}
					}
				}  /*With StorageVars*/
			}
		}
		else
		{
			if(double(Abs( (kVA_Gen - with0.FkVArating))) / with0.FkVArating < 0.0005)
				kVA_exceeded = true;
			else
				kVA_exceeded = false;
		}
	}
}
//----------------------------------------------------------------------------

void TStorageObj::CalcYPrimMatrix(TcMatrix* Ymatrix, int ActorID)
{
	complex Y = {};
	complex Yij = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;
	FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
	FreqMultiplier = FYprimFreq / BaseFrequency;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(with0->IsHarmonicModel) /*IsDynamicModel or*/
       /*Yeq is computed from %R and %X -- inverse of Rthev + j Xthev*/
		{
			int stop = 0;
			Y = Yeq;     // L-N value computed in initial condition routines
			if(Connection == 1)
				Y = cdivreal(Y, 3.0); // Convert to delta impedance
			Y.im = Y.im / FreqMultiplier;
			Yij = cnegate(Y);
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				switch(Connection)
				{
					case 	0:
					{
						Ymatrix->SetElement(i, i, Y);
						Ymatrix->AddElement(Fnconds, Fnconds, Y);
						Ymatrix->SetElemsym(i, Fnconds, Yij);
					}
					break;   /*Delta connection*/
					case 	1:
					{
						int stop1 = 0;
						Ymatrix->SetElement(i, i, Y);
						Ymatrix->AddElement(i, i, Y);  // put it in again
						for(stop1 = i - 1, j = 1; j <= stop1; j++)
						{
							Ymatrix->SetElemsym(i, j, Yij);
						}
					}
					break;
					default:
					  ;
					break;
				}
			}
		}
		else
  //  Regular power flow Storage element model
		
       /*Yeq is always expected as the equivalent line-neutral admittance*/
		{
			switch(fState)
			{
				case 	STORE_CHARGING:		Y = YeqDischarge;
					break;
				case 	STORE_IDLING:		Y = cmplx(0.0, 0.0);
					break;
				case 	STORE_DISCHARGING:
					{
						if (!GFM_Mode)
							Y = cnegate(YeqDischarge);
						else
						{
							myDynVars.RatedkVLL = Get_PresentkV();
							myDynVars.Discharging = ( get_fState() == 1);
							myDynVars.mKVARating = StorageVars.FkVArating;
							myDynVars.CalcGFMYprim(ActorID, Get_NPhases(), Ymatrix);
						}
					}
					break;
				default:
				  ;
				break;
			}

       //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, Change To State=%s, Y=%.8g +j %.8g',[ActiveCircuit[ActiveActor].Solution.dblHour, StateToStr, Y.re, Y.im]));

       // ****** Need to modify the base admittance for real harmonics calcs
			Y.im = Y.im / FreqMultiplier;
			if (!GFM_Mode)
			{
				switch (Connection)
				{
				case 	0:
					/*# with Ymatrix do */
				{
					auto with1 = Ymatrix; // WYE
					int stop = 0;
					Yij = cnegate(Y);
					for (stop = Fnphases, i = 1; i <= stop; i++)
					{
						with1->SetElement(i, i, Y);
						with1->AddElement(Fnconds, Fnconds, Y);
						with1->SetElemsym(i, Fnconds, Yij);
					}
				}
				break;
				case 	1:
					/*# with Ymatrix do */
				{
					auto with2 = Ymatrix;  // Delta  or L-L
					int stop = 0;
					Y = cdivreal(Y, 3.0); // Convert to delta impedance
					Yij = cnegate(Y);
					for (stop = Fnphases, i = 1; i <= stop; i++)
					{
						j = i + 1;
						if (j > Fnconds)
							j = 1;  // wrap around for closed connections
						with2->AddElement(i, i, Y);
						with2->AddElement(j, j, Y);
						with2->AddElemsym(i, j, Yij);
					}
				}
				break;
				default:
					;
					break;
				}
			}
		}
	}  /*ELSE IF Solution.mode*/
}

//----------------------------------------------------------------------------

// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 24.

double TStorageObj::NormalizeToTOD(int h, double Sec)
{
	double result = 0.0;
	int HourOfDay = 0;
	if(h > 23)
		HourOfDay = (h - ((int)(h / 24)) * 24);
	else
		HourOfDay = h;
	result = HourOfDay + Sec / 3600.0;
	if(result > 24.0)
		result = result - 24.0;   // Wrap around
	return result;
}

//----------------------------------------------------------------------------

/*This is where we set the state of the Storage element*/

void TStorageObj::CheckStateTriggerLevel(double Level, int ActorID)
{
	int OldState = 0;
	FStateChanged = false;
	OldState = fState;
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		if(DispatchMode == STORE_FOLLOW)

         // set charge and discharge modes based on sign of loadshape
		{
			if(Level > 0.0 && (with0.kWhStored - with0.kWhReserve) > EPSILON)
				Set_StorageState(STORE_DISCHARGING);
			else if(Level < 0.0 && (with0.kWhStored - with0.kWhRating) < -EPSILON)
				Set_StorageState(STORE_CHARGING);
			else
				Set_StorageState(STORE_IDLING);
		}
		else
   // All other dispatch modes  Just compare to trigger value
		{
			if((ChargeTrigger == 0.0) && (DischargeTrigger == 0.0))
				return;

      // First see If we want to turn off Charging or Discharging State
			switch(fState)
			{
				case 	STORE_CHARGING:
				if(ChargeTrigger != 0.0)
				{
					if((ChargeTrigger < Level) || (with0.kWhStored >= with0.kWhRating))
						fState = STORE_IDLING;
				}
				break;
				case 	STORE_DISCHARGING:
				if(DischargeTrigger != 0.0)
				{
					if((DischargeTrigger > Level) || (with0.kWhStored <= with0.kWhReserve))
						fState = STORE_IDLING;
				}
				break;
				default:
				  ;
				break;
			}

      // Now check to see If we want to turn on the opposite state
			switch(fState)
			{
				case 	STORE_IDLING:
				{
					if((DischargeTrigger != 0.0) && (DischargeTrigger < Level) && (with0.kWhStored > with0.kWhReserve))
						fState = STORE_DISCHARGING;
					else
					{
						if((ChargeTrigger != 0.0) && (ChargeTrigger > Level) && (with0.kWhStored < with0.kWhRating))
							fState = STORE_CHARGING;

                               // Check to see If it is time to turn the charge cycle on If it is not already on.
					}
					if(!(fState == STORE_CHARGING))
					{
						if(ChargeTime > 0.0)
							/*# with ActiveCircuit[ActorID].Solution do */
							{
								auto with1 = ActiveCircuit[ActorID]->Solution;
								if(Abs( (NormalizeToTOD(with1->DynaVars.intHour, with1->DynaVars.T) - ChargeTime)) < with1->DynaVars.h / 3600.0)
									fState = STORE_CHARGING;
							}
					}
				}
				break;
				default:
				  ;
				break;
			}
		}
	}
	if(OldState != fState)
	{
		FStateChanged = true;
		Set_YprimInvalid(ActorID,true);
	}
}

//----------------------------------------------------------------------------

void TStorageObj::CalcYPrim(int ActorID)
{
	int i = 0;

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV Does not fail
	int stop = 0;
	if(Get_YprimInvalid(ActorID,0))
	{
		if(YPrim_Shunt != nullptr)
			delete YPrim_Shunt;
		YPrim_Shunt = new TcMatrix(Yorder);
		if(YPrim_Series != nullptr)
			delete YPrim_Series;
		YPrim_Series = new TcMatrix(Yorder);
		if(YPrim != nullptr)
			delete YPrim;
		YPrim = new TcMatrix(Yorder);
	}
	else
	{
		YPrim_Shunt->Clear();
		YPrim_Series->Clear();
		YPrim->Clear();
	}
	SetNominalStorageOutput(ActorID);
	CalcYPrimMatrix(YPrim_Shunt, ActorID);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
	for(stop = Yorder, i = 1; i <= stop; i++)
	{
		YPrim_Series->SetElement(i, i, cmulreal(YPrim_Shunt->GetElement(i, i), 1.0e-10));
	}
	YPrim->CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
	inherited::CalcYPrim(ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

 /*Add the current into the proper location according to connection*/

 /*Reverse of similar routine in load  (Cnegates are switched)*/

void TStorageObj::StickCurrInTerminalArray(pComplexArray TermArray, const complex& Curr, int i)
{
	int j = 0;
	switch(Connection)
	{
		case 	0:  //Wye
		{
			caccum(TermArray[i - 1], Curr);
			caccum(TermArray[Fnconds - 1], cnegate(Curr)); // Neutral
		}
		break; //DELTA
		case 	1:
		{
			caccum(TermArray[i - 1], Curr);
			j = i + 1;
			if(j > Fnconds)
				j = 1;
			caccum(TermArray[j - 1], cnegate(Curr));
		}
		break;
		default:
		  ;
		break;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TStorageObj::WriteTraceRecord(const String s, int ActorID)
{
	int i = 0;
	try
	{
		if(!InShowResults)
		{
			int stop = 0;
			Append(Tracefile);
			IOResultToException();
			{ Write(Tracefile, Format("%-.g, %d, %-.g, ", 
				ActiveCircuit[ActorID]->Solution->DynaVars.dblHour, 
					ActiveCircuit[ActorID]->Solution->Iteration, 
					ActiveCircuit[ActorID]->get_FLoadMultiplier())); 
			Write(Tracefile, GetSolutionModeID()); 
			Write(Tracefile, ", "); 
			Write(Tracefile, GetLoadModel()); 
			Write(Tracefile, ", "); 
			Write(Tracefile, VoltageModel, 0); 
			Write(Tracefile, ", "); 
			Write(Tracefile, (Qnominalperphase * 3.0 / 1.0e6), 8, 2); 
			Write(Tracefile, ", "); 
			Write(Tracefile, (Pnominalperphase * 3.0 / 1.0e6), 8, 2); 
			Write(Tracefile, ", "); 
			Write(Tracefile, s); 
			Write(Tracefile, ", "); 
			}

			for(stop = Get_NPhases(), i = 1; i <= stop; i++)
			{
				{ Write(Tracefile, (cabs((InjCurrent)[i - 1])), 8, 1); Write(Tracefile, ", "); }
			}
			for(stop = Get_NPhases(), i = 1; i <= stop; i++)
			{
				{ Write(Tracefile, (cabs((Iterminal)[i - 1])), 8, 1); Write(Tracefile, ", "); }
			}
			for(stop = Get_NPhases(), i = 1; i <= stop; i++)
			{
				{ Write(Tracefile, (cabs((Vterminal)[i - 1])), 8, 1); Write(Tracefile, ", "); }
			}
			for(stop = NumVariables(), i = 1; i <= stop; i++)
			{
				Write(Tracefile, Format("%-.g, ", Get_Variable(i)));
			}


   //****        Write(TraceFile,VThevMag:8:1 ,', ', StoreVARs.Theta*180.0/PI);
			WriteLn(Tracefile);
			CloseFile(Tracefile);
		}
	}
	catch (...)
	{
	}
}
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*Compute total terminal current for Constant PQ*/

void TStorageObj::DoConstantPQStorageObj(int ActorID)
{
	int i = 0;
//   CurrIdlingZ,
	complex Curr	= CZero;
	complex VLN		= CZero;
	complex VLL		= CZero;
   //---DEBUG--- S:Complex;
	double VmagLN	= 0.0;
	double VmagLL	= 0.0;
	complex V012[4] = { CZero, CZero, CZero, CZero, };  // Sequence voltages


	auto with0 = ActiveCircuit[ActorID]->Solution;
	//Treat this just like the Load model
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	ZeroITerminal();

    //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, State=%s, Iyprim= %s', [ActiveCircuit[ActiveActor].Solution.dblHour, StateToStr, CmplxArrayToString(InjCurrent, Yprim.Order) ]));

//    CASE FState of
//      STORE_IDLING:  // YPrim current is only current
//             Begin
//                For i := 1 to FNPhases Do
//                Begin
//                    Curr :=  InjCurrent^[i];
//                    StickCurrInTerminalArray(ITerminal, Curr, i);  // Put YPrim contribution into Terminal array taking into account connection
//                    set_ITerminalUpdated(True, ActorID);
//                    StickCurrInTerminalArray(InjCurrent, Cnegate(Curr), i);    // Compensation current is zero since terminal current is same as Yprim contribution
//                    //---DEBUG--- S := Cmul(Vterminal^[i] , Conjg(Iterminal^[i]));  // for debugging below
//                    //---DEBUG--- WriteDLLDebugFile(Format('        Phase=%d, Pnom=%.8g +j %.8g',[i, S.re, S.im ]));
//                End;
//             //---DEBUG--- WriteDLLDebugFile(Format('        Icomp=%s ', [CmplxArrayToString(InjCurrent, Yprim.Order) ]));
//             End;
//    ELSE   // For Charging and Discharging
	CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
	if(ForceBalanced && (Fnphases == 3))  // convert to pos-seq only
	{
		Phase2SymComp(&(Vterminal[0]), &V012[0]);
		V012[0] = CZero; // Force zero-sequence voltage to zero
		V012[2] = CZero; // Force negative-sequence voltage to zero
		SymComp2Phase(&(Vterminal[0]), &V012[0]);  // Reconstitute Vterminal as balanced
	}
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		switch(Connection)
		{
			case 	0:  /*Wye*/
			{
				VLN = (Vterminal)[i - 1];
				VmagLN = cabs(VLN);
				if(VmagLN <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(Yeq95, VLN);
				else
				{
					if(VmagLN > VBase105)  // above 105% use an impedance model
						Curr = cmul(Yeq105, VLN);
					else
						Curr = conjg(cdiv(cmplx(Pnominalperphase, Qnominalperphase), VLN));  // Between 95% -105%, constant PQ
				}
				if(CurrentLimited)
				{
					if(cabs(Curr) > MaxDynPhaseCurrent)
						Curr = conjg(cdiv(PhaseCurrentLimit, cdivreal(VLN, VmagLN)));
				}
			}
			break;  /*Delta*/
			case 	1:
			{
				VLL = (Vterminal)[i - 1];
				VmagLL = cabs(VLL);
				if(Fnphases > 1)
					VmagLN = VmagLL / SQRT3;
				else
					VmagLN = VmagLL;  // L-N magnitude
				if(VmagLN <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(cdivreal(Yeq95, 3.0), VLL);
				else
				{
					if(VmagLN > VBase105)  // above 105% use an impedance model
						Curr = cmul(cdivreal(Yeq105, 3.0), VLL);
					else
						Curr = conjg(cdiv(cmplx(Pnominalperphase, Qnominalperphase), VLL));  // Between 95% -105%, constant PQ
				}
				if(CurrentLimited)
				{
					if(cabs(Curr) * SQRT3 > MaxDynPhaseCurrent)
						Curr = conjg(cdiv(PhaseCurrentLimit, cdivreal(VLL, VmagLN))); // Note VmagLN has sqrt3 factor in it
				}
			}
			break;
			default:
			  ;
			break;
		}

         //---DEBUG--- WriteDLLDebugFile(Format('        Phase=%d, Pnom=%.8g +j %.8g', [i, Pnominalperphase, Qnominalperphase ]));
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

		if (with0->Algorithm == NCIMSOLVE)				// NCIM solution algorithms
			DoPQBusNCIM(ActorID, i, Vterminal[i - 1], Curr);

	}
        //---DEBUG--- WriteDLLDebugFile(Format('        Icomp=%s ', [CmplxArrayToString(InjCurrent, Yprim.Order) ]));
//    END;
}

// -----------------------------------------------------------------------------------
// Updates the Jacobian matrix diagonal and calculates the Amps delta using deltaF
void TStorageObj::DoPQBusNCIM(int ActorID, int i, complex V, complex Curr)
{
	complex Pow = {},
		FaVr = {},
		FaVm = {},
		Temp = {},
		Vc2 = {};

	int LCoords[4][2] = { {0,0}, {1,1}, {0,1}, {1,0} };
	int GCoord = 0;
	auto with0 = ActiveCircuit[ActorID]->Solution;
	Pow = conjg(cmplx(-Pnominalperphase, -Qnominalperphase));
	Vc2 = cmul(conjg(V), conjg(V));
	FaVr = cdiv(cmplx(-1, 0), Vc2);
	FaVm = cdiv(cmplx(0, 1), Vc2);
	GCoord = (NodeRef[i - 1] * 2) - 1;
	// Updates the Jacobian
	for (int j = 0; j < 4; j++)
	{
		// Add the derivatives
		switch (j)
		{
		case 0:													// dImdVr
			Temp.re = cmul(FaVr, Pow).im;
			break;
		case 1:													// dIrdVm
			Temp.re = cmul(FaVm, Pow).re;
			break;
		case 2:													// dImdVm
			Temp.re = cmul(FaVm, Pow).im;
			break;
		case 3:													// dIrdVr
			Temp.re = cmul(FaVr, Pow).re;
			break;
		default:
			break;
		}
		SetMatrixElement(with0->Jacobian, GCoord + LCoords[j][0], GCoord + LCoords[j][1], &Temp);
	}

	// Add current injection contributions to deltaF
	GCoord--;															// Removes the additional index added by DSS
	Iterminal[i - 1]				= Curr;
	with0->deltaF[GCoord].re		= Curr.im + with0->deltaF[GCoord].re;			// Respecting the decoupled distribution
	with0->deltaF[GCoord + 1].re	= Curr.re + with0->deltaF[GCoord + 1].re;		// Prioritizing reactive power over the diagonal
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*constant Z model*/

void TStorageObj::DoConstantZStorageObj(int ActorID)
{
	int i = 0;
	complex Curr = CZero,
			YEQ2 = CZero;
	complex V012[4] = { CZero, CZero, CZero, CZero, };  // Sequence voltages

	auto with0 = ActiveCircuit[ActorID]->Solution;

// Assume Yeq is kept up to date
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
	ZeroITerminal();
	if(Connection == 0)
		YEQ2 = Yeq;
	else
		YEQ2 = cdivreal(Yeq, 3.0);
	if(ForceBalanced && (Fnphases == 3))  // convert to pos-seq only
	{
		Phase2SymComp(&(Vterminal[0]), &V012[0]);
		V012[0] = CZero; // Force zero-sequence voltage to zero
		V012[2] = CZero; // Force negative-sequence voltage to zero
		SymComp2Phase(&(Vterminal[0]), &V012[0]);  // Reconstitute Vterminal as balanced
	}
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		Curr = cmul(YEQ2, (Vterminal)[i - 1]);   // Yeq is always line to neutral
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

		if (with0->Algorithm == NCIMSOLVE)				// NCIM solution algorithms
			DoPQBusNCIM(ActorID, i, Vterminal[i - 1], Curr);
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

/*Compute total terminal Current from User-written model*/

void TStorageObj::DoUserModel(int ActorID)
{
	int i = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	if(UserModel->Get_Exists())    // Check automatically selects the usermodel If true
	{
		UserModel->FCalc(&(Vterminal[0]), &(Iterminal[0]));
		set_ITerminalUpdated(true, ActorID);
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;          // Negate currents from user model for power flow Storage element model
			int stop = 0;
			for(stop = Fnconds, i = 1; i <= stop; i++)
			{
				caccum(InjCurrent[i - 1], cnegate(Iterminal[i - 1]));
			}
		}
	}
	else
	{
		DoSimpleMsg(String("Storage.") + get_Name()
	           + " model designated to use user-written model, but user-written model is not defined.", 567);
	}
}



// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*Compute Total Current and add into InjTemp*/
/*
   For now, just assume the Storage element Thevenin voltage is constant
   for the duration of the dynamic simulation.
*/
/******/

void TStorageObj::DoDynamicMode(int ActorID)
{
	int i				= 0;
	polar PolarN		= topolar(0,0);
	complex Curr		= cmplx(0,0),
			NeutAmps	= cmplx(0,0);
	complex V012[3/*# range 0..2*/] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) };
	complex I012[3/*# range 0..2*/] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) };
	double AngCmp		= 0,
		iActual			= 0;

	auto CalcVthev_Dyn = [&]() -> void 
	{
		/*# with StorageVars do */
		{
			auto& with0 = StorageVars;
			with0.Vthev = pclx(with0.VthevMag, with0.Theta);
		}   // keeps theta constant
	};

/******/  // Test using DESS model
   // Compute Vterminal
	if(DynaModel->Get_Exists())   // do user-written model
		DoDynaModel(ActorID);
	else
	{
		if (!GFM_Mode)
		{
			CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
			ZeroITerminal();
			//{This model has no limitation in the nmber of phasesand is ideally unbalanced(no dq - dv, but is implementable as well)}
			// First, get the phase angles for the currents
			NeutAmps = cmplx(0, 0);
			for (i = 1; i <= Fnphases; i++)
			{
				auto& with0 = myDynVars;
				AngCmp = 0;
				if (with0.it[i - 1] <= with0.iMaxPPhase)
					iActual = with0.it[i - 1];
				else
					iActual = with0.iMaxPPhase;
				//-----------------------------------------------------------
				if (iActual < with0.MinAmps)
					iActual = 0;
				if (fState != 1)
					iActual = (PIdling / with0.Vgrid[i - 1].mag) / Get_NPhases();
				PolarN		= topolar(iActual, with0.Vgrid[i - 1].ang + AngCmp); // Output Current estimated for active power
				Curr		= cnegate(ptocomplex(PolarN));
				NeutAmps	= csub(NeutAmps, Curr);
				Iterminal[i - 1] = Curr;
			}
			if (Fnconds > Fnphases)
				Iterminal[Fnconds - 1] = NeutAmps;
			//{Add it into inj current array}
			for (i = 1; i <= Fnconds; i++)
				caccum(InjCurrent[i - 1], cnegate(Iterminal[i - 1]));
			set_ITerminalUpdated(true, ActorID);
		}
		else
		{
			myDynVars.BaseV = myDynVars.BasekV * 1000 * (myDynVars.it[0] / myDynVars.iMaxPPhase);
			myDynVars.CalcGFMVoltage( ActorID, Get_NPhases(), &(Vterminal[0]));	// needs to be referenced as 0
			YPrim->MVmult(InjCurrent, &(Vterminal[0]));						// needs to be referenced as 1
		}
	}
}
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
// Returns True if any of the inverter phases has reached the current limit
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
bool TStorageObj::CheckAmpsLimit(int ActorID)
{
	complex myCurr;
	double myVolts;
	double NomP;
	double PhaseP;
	double PhaseAmps;
	int i;
	// Check if reaching saturation point in GFM
	bool result = false;
	NomP = myDynVars.ILimit * VBase;
	if (GFM_Mode)
	{
		GetCurrents(&Iterminal[0], ActorID);
		myDynVars.IComp = 0.0;
        for (i = 1; i <= Fnphases; i++)
		{
			myCurr = Iterminal[i - 1];
			PhaseAmps = cabs(myCurr);
			myVolts = ctopolar(ActiveCircuit[ActorID]->Solution->NodeV[NodeRef[i - 1]]).mag;
			PhaseP = PhaseAmps * myVolts;
			if (PhaseP > NomP)
			{
				if (PhaseP > myDynVars.IComp)
					myDynVars.IComp = PhaseP;
				result = true;
			}
		}
	}
	return result;
}
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
// Implements the grid forming inverter control routine for the storage device
void TStorageObj::DoGFM_Mode(int ActorID)
{
	int j			= 0,
		i			= 0;
	double	myW		= 0,
			ZSys 	= 0;

	auto& with0 = ActiveCircuit[ActorID]->Solution;

	myDynVars.BaseV			= VBase;
	myDynVars.Discharging	= (get_fState() == STORE_DISCHARGING);
	if (myDynVars.IComp > 0)
	{
		ZSys = (2 * (VBase * myDynVars.ILimit)) - myDynVars.IComp;
		myDynVars.BaseV = (ZSys / myDynVars.ILimit) * myDynVars.VError;
	}
	myDynVars.CalcGFMVoltage(ActorID, Get_NPhases(), &(Vterminal[0]));
	YPrim->MVmult(InjCurrent, &(Vterminal[0]));

	set_ITerminalUpdated(false, ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TStorageObj::DoDynaModel(int ActorID)
{
	complex DESSCurr[6/*# range 1..6*/];  // Temporary buffer
	int i = 0;
	int j = 0;
// do user written dynamics model
	int stop = 0;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;  // Just pass node voltages to ground and let dynamic model take care of it
		int stop = 0;
		for(stop = Fnconds, i = 1; i <= stop; i++)
		{
			if(!ADiakoptics || (ActorID == 1))
				(Vterminal)[i - 1] = with0->NodeV[NodeRef[i - 1]];
			else
				(Vterminal)[i - 1] = with0->VoltInActor1(NodeRef[i - 1]);
		}
		StorageVars.w_grid = TwoPi * with0->get_FFrequency();
	}
	DynaModel->FCalc(&(Vterminal[0]), (pComplexArray)&DESSCurr);
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(DESSCurr[i - 1]), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, DESSCurr[i - 1], i);  // Put into Terminal array taking into account connection
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*Compute Injection Current Only when in harmonics mode*/

/*Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built*/
/*Vd is the fundamental frequency voltage behind Xd" for phase 1*/

void TStorageObj::DoHarmonicMode(int ActorID)
{
	int i = 0;
	complex e = {};
	double StorageHarmonic = 0.0;
	ComputeVterminal(ActorID);
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		int stop = 0;
		StorageHarmonic = with0->get_FFrequency() / StorageFundamental;
		if(SpectrumObj != nullptr) // Get base harmonic magnitude
			e = cmulreal(SpectrumObj->GetMult(StorageHarmonic), StorageVars.VThevHarm);
		else
			e = CZero;
		RotatePhasorRad(e, StorageHarmonic, StorageVars.ThetaHarm);  // Time shift by fundamental frequency phase shift
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			cBuffer[i - 1] = e;
			if(i < Fnphases)
				RotatePhasorDeg(e, StorageHarmonic, -120.0);  // Assume 3-phase Storage element
		}
	}

   /*Handle Wye Connection*/
	if(Connection == 0)
		cBuffer[Fnconds - 1] = (Vterminal)[Fnconds - 1];  // assume no neutral injection voltage
		
   /*Inj currents = Yprim (E) */
	YPrim->MVmult(InjCurrent, (pComplexArray) &cBuffer);
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TStorageObj::CalcVTerminalPhase(int ActorID)
{
	int i = 0;
	int j = 0;

/* Establish phase voltages and stick in Vterminal*/
	switch(Connection)
	{
		case 	0:
		{
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with0 = ActiveCircuit[ActorID]->Solution;
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					Vterminal[i - 1] = with0->VDiff(NodeRef[i - 1], NodeRef[Fnconds - 1], ActorID);
				}
			}
		}
		break;
		case 	1:
		{
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with1 = ActiveCircuit[ActorID]->Solution;
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					j = i + 1;
					if(j > Fnconds)
						j = 1;
					(Vterminal)[i - 1] = with1->VDiff(NodeRef[i - 1], NodeRef[j - 1], ActorID);
				}
			}
		}
		break;
		default:
		  ;
		break;
	}
	StorageSolutionCount = ActiveCircuit[ActorID]->Solution->SolutionCount;
}



// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
/*
PROCEDURE TStorageObj.CalcVTerminal;
{Put terminal voltages in an array}
Begin
   ComputeVTerminal;
   StorageSolutionCount := ActiveCircuit[ActiveActor].Solution.SolutionCount;
End;
*/


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Calculates Storage element current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

void TStorageObj::CalcStorageModelContribution(int ActorID)
{
	set_ITerminalUpdated(false, ActorID);
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
	{
		
		auto with1 = ActiveCircuit[ActorID]->Solution;
		if(with1->IsDynamicModel)
			DoDynamicMode(ActorID);
		else
		{
			if(with1->IsHarmonicModel && (with1->get_FFrequency() != ActiveCircuit[ActorID]->Fundamental))
				DoHarmonicMode(ActorID);
			else
               //  compute currents and put into InjTemp array; 
			{
				if (GFM_Mode) DoGFM_Mode(ActorID);
				else
					switch(VoltageModel)
					{
						case 	1:	DoConstantPQStorageObj(ActorID);
							break;
						case 	2:	DoConstantZStorageObj(ActorID);
							break;
						case 	3:	DoUserModel(ActorID);
							break;
						default:	DoConstantPQStorageObj(ActorID);  // for now, until we implement the other models.
							break;
					}
			} /*ELSE*/
		}
	} /*WITH*/

   /*When this is Done, ITerminal is up to date*/
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

// Difference between currents in YPrim and total current

void TStorageObj::CalcInjCurrentArray(int ActorID)
{

      // Now Get Injection Currents
	if(StorageObjSwitchOpen)
		ZeroInjCurrent();
	else
		CalcStorageModelContribution(ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Compute total Currents

void TStorageObj::GetTerminalCurrents(pComplexArray Curr, int ActorID)
{
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(IterminalSolutionCount[ActorID] != ActiveCircuit[ActorID]->Solution->SolutionCount)     // recalc the contribution
		{
			if(!StorageObjSwitchOpen)
				CalcStorageModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
		}
		inherited::GetTerminalCurrents(Curr, ActorID);
	}
	if(DebugTrace)
		WriteTraceRecord("TotalCurrent", ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

int TStorageObj::InjCurrents(int ActorID)
{
	int result = 0;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(with0->LoadsNeedUpdating)
			SetNominalStorageOutput(ActorID); // Set the nominal kW, etc for the type of solution being Done
		CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current
		if(DebugTrace)
			WriteTraceRecord("Injection", ActorID);

         // Add into System Injection Current Array
		result = inherited::InjCurrents(ActorID);
	}
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
// Required for operation in GFM mode
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
void TStorageObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	if (GFM_Mode)
	{
		try
		{
			auto& with0 = ActiveCircuit[ActorID]->Solution;
			for (i = 1; i <= Yorder; i++)
			{
				if (!ADiakoptics || (ActorID == 1))
					Vterminal[i - 1] = with0->NodeV[NodeRef[i - 1]];
				else
					Vterminal[i - 1] = with0->VoltInActor1(NodeRef[i - 1]);
			}
			YPrim->MVmult(Curr, &Vterminal[0]);  // Current from Elements in System Y
			// Add Together  with yprim currents
			for (i = 1; i <= Yorder; i++)
				Curr[i - 1] = csub(Curr[i - 1], InjCurrent[i - 1]);
		}
		catch (const std::exception &e)
		{
			DoErrorMsg(String("GetCurrents for Element: \"") + get_Name(), (std::string)e.what(), "Inadequate storage allotted for circuit element.", 327);
		}
	}
	else
		inherited::GetCurrents(Curr, ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
// Returns True if any of the inverter phases is overloaded
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
bool TStorageObj::CheckOLInverter(int ActorID)
{
	complex myCurr		= cmplx(0,0);
	double	MaxAmps		= 0,
			PhaseAmps	= 0;
	int		i			= 0;
	bool	result		= false;

	if (GFM_Mode)
	{
		MaxAmps = ( ( StorageVars.FkVArating * 1000 ) / Get_NPhases() ) / VBase;
		GetCurrents( &( Iterminal[0] ), ActorID );
		for (i = 1; i <= Get_NPhases(); i++)
		{
			myCurr		= Iterminal[i - 1];
			PhaseAmps	= cabs( myCurr );
			if (PhaseAmps > MaxAmps)
			{
				result = true;
				break;
			}
		}
	}

	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Gives the currents for the last solution performed

// Do not call SetNominalLoad, as that may change the load values

void TStorageObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	CalcInjCurrentArray(ActorID);  // Difference between currents in YPrim and total current
	try

   // Copy into buffer array
	{
		int stop = 0;
		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			(Curr)[i - 1] = (InjCurrent)[i - 1];
		}
	}
	catch (std::exception &e)
	{
		DoErrorMsg(String("Storage Object: \"") + get_Name() + "\" in GetInjCurrents FUNCTION.", (std::string) e.what(), "Current buffer not big enough.", 568);
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TStorageObj::ResetRegisters()
{
	int i = 0;
	int stop = 0;
	for(stop = NumStorageRegisters, i = 1; i <= stop; i++)
	{
		Registers[i - 1] = 0.0;
	}
	for(stop = NumStorageRegisters, i = 1; i <= stop; i++)
	{
		Derivatives[i - 1] = 0.0;
	}
	FirstSampleAfterReset = true;  // initialize for trapezoidal integration
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TStorageObj::Integrate(int reg, double Deriv, double Interval, int ActorID)
{
	if(ActiveCircuit[ActorID]->TrapezoidalIntegration)
        /*Trapezoidal Rule Integration*/
	{
		if(!FirstSampleAfterReset)
			Registers[reg] = Registers[reg] + 0.5 * Interval * (Deriv + Derivatives[reg]);
	}
	else
   /*Plain Euler integration*/
	Registers[reg] = Registers[reg] + Interval * Deriv;
	Derivatives[reg] = Deriv;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// Update Energy from metered zone

void TStorageObj::TakeSample(int ActorID)
{
	complex s = {};
	double Smag = 0.0;
	double HourValue = 0.0;

// Compute energy in Storage element branch
	if(Get_Enabled())

     // Only tabulate discharge hours
	{
		if(fState == STORE_DISCHARGING)
		{
			s = cmplx(Get_PresentkW(), Get_Presentkvar());
			Smag = cabs(s);
			HourValue = 1.0;
		}
		else
		{
			s = CZero;
			Smag = 0.0;
			HourValue = 0.0;
		}
		if((fState == STORE_DISCHARGING) || ActiveCircuit[ActorID]->TrapezoidalIntegration)
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with0 = ActiveCircuit[ActorID]->Solution;
        /*Make sure we always integrate for Trapezoidal case
         Don't need to for Gen Off and normal integration*/
				if(ActiveCircuit[ActorID]->PositiveSequence)
				{
					s = cmulreal(s, 3.0);
					Smag = 3.0 * Smag;
				}
				Integrate(Reg_kWh, s.re, with0->IntervalHrs, ActorID);   // Accumulate the power
				Integrate(Reg_kvarh, s.im, with0->IntervalHrs, ActorID);
				SetDragHandRegister(Reg_MaxkW, Abs( s.re));
				SetDragHandRegister(Reg_MaxkVA, Smag);
				Integrate(Reg_Hours, HourValue, with0->IntervalHrs, ActorID);  // Accumulate Hours in operation
				Integrate(Reg_Price, s.re * ActiveCircuit[ActorID]->PriceSignal * 0.001, with0->IntervalHrs, ActorID);  // Accumulate Hours in operation
				FirstSampleAfterReset = false;
			}
	}
}

//---------------------------------------------------------------------------
// Checks if delivering or abosrbing power in GFM control mode
//---------------------------------------------------------------------------
bool TStorageObj::CheckIfDelivering(int ActorID)
{
	int		i		= 0;
	bool	result	= false;
	std::vector<complex> myVolt, myCurr;

	// If in GFM mode, check if we are actually delivering power
	ComputeIterminal(ActorID);
	myVolt.resize(Get_NPhases() + 1);
	myCurr.resize(Get_NPhases() + 1);
	for (i = 1; i <= Get_NPhases(); i++)
	{
		myCurr[i] = Iterminal[i - 1];
		myVolt[i] = ActiveCircuit[ActorID]->Solution->NodeV[NodeRef[i - 1]];
		CalcKPowers(&(myVolt[i]), &(myVolt[i]), &(myCurr[i]), 1);
		result = ( myVolt[i].re < 0 ) || result;
	}

	return result;
}

//----------------------------------------------------------------------------

/*Update Storage levels*/

void TStorageObj::UpdateStorage(int ActorID)
{
	bool	UpdateSt = false;
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		kWhBeforeUpdate = with0.kWhStored;   // keep this for reporting change in Storage as a variable

      /*Assume User model will take care of updating Storage in dynamics mode*/
		if(ActiveCircuit[ActorID]->Solution->IsDynamicModel && IsUserModel)
			return;
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with1 = ActiveCircuit[ActorID]->Solution;
			switch(fState)
			{
				case 	STORE_DISCHARGING:
				{
					UpdateSt = true;
					if (GFM_Mode) UpdateSt = CheckIfDelivering(ActorID);

					if (UpdateSt)
						with0.kWhStored = with0.kWhStored - (Get_DCkW() + Get_kWIdlingLosses()) / with0.DisChargeEff * with1->IntervalHrs;
					else
					{	// We are obsrobing power, let's recharge if needed
						with0.kWhStored = with0.kWhStored + (Get_DCkW() + Get_kWIdlingLosses()) / with0.DisChargeEff * with1->IntervalHrs;
						if (with0.kWhStored > with0.kWhRating) with0.kWhStored = with0.kWhRating;
					}
					if(with0.kWhStored < with0.kWhReserve)
					{
						with0.kWhStored = with0.kWhReserve;
						fState			= STORE_IDLING;  // It's empty Turn it off
						FStateChanged	= true;
						GFM_Mode		= false;
					}
				}
				break;
				case 	STORE_CHARGING:
				{
					if((Abs(Get_DCkW()) - Get_kWIdlingLosses()) >= 0) // 99.9 % of the cases will fall here
					{
						with0.kWhStored = with0.kWhStored + (Abs(Get_DCkW()) - Get_kWIdlingLosses()) * with0.ChargeEff * with1->IntervalHrs;
						if(with0.kWhStored > with0.kWhRating)
						{
							with0.kWhStored = with0.kWhRating;
							fState = STORE_IDLING;  // It's full Turn it off
							FStateChanged = true;
							GFM_Mode = 0;
						}
					}
					else
   // Exceptional cases when the idling losses are higher than the Get_DCkW() such that the net effect is that the
                                 // the ideal Storage will discharge
					{
						with0.kWhStored = with0.kWhStored + (Abs(Get_DCkW()) - Get_kWIdlingLosses()) / with0.DisChargeEff * with1->IntervalHrs;
						if(with0.kWhStored < with0.kWhReserve)
						{
							with0.kWhStored = with0.kWhReserve;
							fState = STORE_IDLING;  // It's empty Turn it off
							FStateChanged = true;
						}
					}
				}
				break;
				case 	STORE_IDLING:
				;
				break;
				default:
				  ;
				break;
			}
		}
	}

      // the update is done at the end of a time step so have to force
      // a recalc of the Yprim for the next time step.  Else it will stay the same.
	if(FStateChanged)
		Set_YprimInvalid(ActorID,true);
}

//----------------------------------------------------------------------------

// Computes actual DCkW to Update Storage SOC

void TStorageObj::ComputeDCkW()
{
	TCoeff coefGuess = {0,0};
	TCoeff coef = { 0,0 };
	int N_tentatives = 0;
	coefGuess[0] = 0.0;
	coefGuess[1] = 0.0;
	coef[0] = 1.0;
	coef[1] = 1.0;  // just a guess
	FDCkW = Get_Power(1, ActiveActor).re * 0.001;  // Assume ideal inverter
	if(InverterCurveObj == nullptr)
    // make sure sign is correct
	{
		if(fState == STORE_IDLING)
			FDCkW = Abs(FDCkW) *  - 1;
		else
			FDCkW = Abs(FDCkW) * fState;
		return;
	}
	N_tentatives = 0;
	while((coef[0] != coefGuess[0]) && (coef[1] != coefGuess[1]) || (N_tentatives > 9))
	{
		N_tentatives = N_tentatives + 1;
		coefGuess = InverterCurveObj->GetCoefficients(Abs(FDCkW) / StorageVars.FkVArating);
		switch(fState)
		{
			case 	STORE_DISCHARGING:
			FDCkW = QuadSolver(coefGuess[0] / StorageVars.FkVArating, coefGuess[1], -1.0 * Abs( (Get_Power(1, ActiveActor).re * 0.001)));
			break;
			case 	STORE_CHARGING:
			 case STORE_IDLING:
			FDCkW = Abs(FDCkW) * coefGuess[1] / (1.0L - (coefGuess[0] * Abs(FDCkW) / StorageVars.FkVArating));
			break;
			default:
			  ;
			break;
		}

      // Final coefficients
		coef = InverterCurveObj->GetCoefficients(Abs(FDCkW) / StorageVars.FkVArating);
	}

    // make sure sign is correct
	if(fState == STORE_IDLING)
		FDCkW = Abs(FDCkW) *  - 1;
	else
		FDCkW = Abs(FDCkW) * fState;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_PresentkW()
{
	double result = 0.0;
	result = Pnominalperphase * 0.001 * Fnphases;
	return result;
}

//----------------------------------------------------------------------------
/*
double TStorageObj::Get_DCkW()
{
	double result = 0.0;
	ComputeDCkW;
	result = FDCkW;
	return result;
}*/

//----------------------------------------------------------------------------

double TStorageObj::Get_kWDesired()
{
	double result = 0.0;
	switch(FStateDesired)
	{
		case 	STORE_CHARGING:
		result = -Get_FpctkWIn() * StorageVars.kWrating / 100.0;
		break;
		case 	STORE_DISCHARGING:
		result = get_FpctkWout() * StorageVars.kWrating / 100.0;
		break;
		case 	STORE_IDLING:
		result = 0.0;
		break;
		default:
		  ;
		break;
	}
	return result;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_StateDesired(int i)
{
	FStateDesired = i;
}

//-----------------------------------------------------------------------------

double TStorageObj::Get_kWTotalLosses()
{
	double result = 0.0;
	result = Get_kWIdlingLosses() + Get_InverterLosses() + Get_kWChDchLosses();
	return result;
}

//----------------------------------------------------------------------------

double TStorageObj::get_PFNominal()
{
	return PFNominal;
}

//----------------------------------------------------------------------------

int TStorageObj::get_fState()
{
	return fState;
}

//----------------------------------------------------------------------------

double TStorageObj::get_FpctkWout()
{
	return FpctkWout;
}

//----------------------------------------------------------------------------

double TStorageObj::get_Fpctkvarout()
{
	return Fpctkvarout;
}

//----------------------------------------------------------------------------

double TStorageObj::get_Vminpu()
{
	return Vminpu;
}

//----------------------------------------------------------------------------
/*
double TStorageObj::Get_InverterLosses()
{
	double result = 0.0;
	result = 0.0;
	//# with StorageVars do
	{
		auto with0 = StorageVars;
		switch(get_fState())
		{
			case 	STORE_IDLING:
			result = Abs( (Power[1][ActiveActor].re * 0.001)) - Abs(Get_DCkW());
			break;
			case 	STORE_CHARGING:
			result = Abs( (Power[1][ActiveActor].re * 0.001)) - Abs(Get_DCkW());
			break;
			case 	STORE_DISCHARGING:
			result = Get_DCkW() - Abs( (Power[1][ActiveActor].re * 0.001));
			break;
			default:
			  ;
			break;
		}
	}
	return result;
}*/

//----------------------------------------------------------------------------

double TStorageObj::Get_kWIdlingLosses()
{
	double result = 0.0;
	if(fState == STORE_IDLING)
	{
		result = Abs(Get_DCkW()); // For consistency keeping with voltage variations
	}
	else
	result = PIdling;
	return result;
}

//----------------------------------------------------------------------------
/*
double TStorageObj::Get_kWChDchLosses()
{
	double result = 0.0;
	result = 0.0;
	//# with StorageVars do
	{
		auto with0 = StorageVars;
		switch(get_fState())
		{
			case 	STORE_IDLING:
			result = 0.0;
			break;
			case 	STORE_CHARGING:
			if(Abs(Get_DCkW()) - PIdling > 0) // most cases will fall here
				result = (Abs(Get_DCkW()) - PIdling) * (1.0 - 0.01 * pctChargeEff);
			else
				result = -1 * (Abs(Get_DCkW()) - PIdling) * (1.0 / (0.01 * pctDischargeEff) - 1.0);
			break;             // exceptional cases when Pidling is higher than DCkW (net effect is that the ideal Storage will be discharged)
			case 	STORE_DISCHARGING:
			result = (Get_DCkW() + PIdling) * (1.0 / (0.01 * pctDischargeEff) - 1.0);
			break;
			default:
			  ;
			break;
		}
	}
	return result;
}*/

//----------------------------------------------------------------------------

void TStorageObj::Update_EfficiencyFactor()
{
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		if(InverterCurveObj == nullptr)
			with0.EffFactor = 1.0;
		else
			with0.EffFactor = InverterCurveObj->GetYValue_(double(Abs(Get_DCkW())) / with0.FkVArating);
	}
}

//----------------------------------------------------------------------------

double TStorageObj::Get_PresentkV()
{
	double result = 0.0;
	result = StorageVars.kVStorageBase;
	return result;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_Presentkvar()
{
	double result = 0.0;
	result = Qnominalperphase * 0.001 * Fnphases;
	return result;
}

//----------------------------------------------------------------------------

void TStorageObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int Idx = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			Idx = (with0->PropertyIdxMap)[i - 1];
			switch(Idx)
			{
				case 	propUSERDATA:
				{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, "=("); Write(f, Get_PropertyValue(Idx)); WriteLn(f, L')'); }
				break;
				case 	propDynaData:
				{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, "=("); Write(f, Get_PropertyValue(Idx)); WriteLn(f, L')'); }
				break;
				default:
				{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(Idx)); }
				break;
			}
		}
	}
	WriteLn(f);
}


//----------------------------------------------------------------------------


// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X

void TStorageObj::InitHarmonics(int ActorID)
{
	int i = 0;
	int j = 0;
	complex e = {};
	complex Va = {};
	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims
	StorageFundamental = ActiveCircuit[ActorID]->Solution->get_FFrequency();  // Whatever the frequency is when we enter here.
	Yeq = cinv(cmplx(StorageVars.RThev, StorageVars.XThev));      // used for current calcs  Always L-N

     /*Compute reference Thevenin voltage from phase 1 current*/
	ComputeIterminal(ActorID);  // Get present value of current
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		switch(Connection)
		{
			case 	0: /*wye - neutral is explicit*/
			{
				if(!ADiakoptics || (ActorID == 1))
					Va = csub(with0->NodeV[NodeRef[0]], with0->NodeV[NodeRef[Fnconds - 1]]);
				else
					Va = csub(with0->VoltInActor1(NodeRef[0]), with0->VoltInActor1(NodeRef[Fnconds - 1]));
			}
			break;  /*delta -- assume neutral is at zero*/
			case 	1:
			{
				if(!ADiakoptics || (ActorID == 1))
					Va = with0->NodeV[NodeRef[0]];
				else
					Va = with0->VoltInActor1(NodeRef[0]);
			}
			break;
			default:
			  ;
			break;
		}
	}
	e = csub(Va, cmul((Iterminal)[1 - 1], cmplx(StorageVars.RThev, StorageVars.XThev)));
	StorageVars.VThevHarm = cabs(e);   // establish base mag and angle
	StorageVars.ThetaHarm = cang(e);
}


//----------------------------------------------------------------------------


// for going into dynamics mode

void TStorageObj::InitStateVars(int ActorID) 
{
	//complex VNeut			= cmplx(0,0);
	//polar	VThevPolar		= topolar(0,0);
	int		i				= 0;
	int		j				= 0;
	//complex V012[3]			= { cmplx(0,0), cmplx(0,0) , cmplx(0,0) };
	//complex I012[3]			= { cmplx(0,0), cmplx(0,0) , cmplx(0,0) };
	//complex Vabc[4]			= { cmplx(0,0), cmplx(0,0) , cmplx(0,0), cmplx(0,0) };
	double	BaseZt			= 0;
	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims

	if (PICtrl.empty() || (PICtrl.size() < Fnphases))
	{
		PICtrl.resize(Fnphases);
		for (i = 0; i < Fnphases; i++)
		{
			PICtrl[i] = TPICtrl();
			PICtrl[i].Kp	= myDynVars.Kp;
			PICtrl[i].kNum	= 0.9502;
			PICtrl[i].kDen	= 0.04979;
		}
	}

	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		with0.Zthev = cmplx(with0.RThev, with0.XThev);
		Yeq = cinv(with0.Zthev);  // used to init state vars
	}
	if(DynaModel->Get_Exists())   // Checks existence and selects
	{
		ComputeIterminal(ActorID);
		ComputeVterminal(ActorID);
		/*# with StorageVars do */
		{
			auto& with1 = StorageVars;
			with1.NumPhases = Fnphases;
			with1.NumConductors = Fnconds;
			with1.w_grid = TwoPi * ActiveCircuit[ActorID]->Solution->get_FFrequency();
		}
		DynaModel->FInit(&(Vterminal[0]), &(Iterminal[0]));
	}
	else if(fState == STORE_DISCHARGING)
	{
		auto with2			= ActiveCircuit[ActorID]->Solution;
		auto& with3			= StorageVars;
		auto& with4			= myDynVars;

		with3.NumPhases		= Fnphases;
		with3.NumConductors = Fnconds;
		with3.Conn			= Connection;

		with4.InitDynArrays(with3.NumPhases);
		if (with3.NumPhases > 1)
			with4.BasekV	= Get_PresentkV() / sqrt(3);
		else
			with4.BasekV	= Get_PresentkV();

		with4.ResetIBR	= false;
		BaseZt			= 0.01 * (sqr(Get_PresentkV()) / Get_FkVARating()) * 1000;
		with4.MaxVS		= (2 - (with4.SMThreshold / 100)) * with4.BasekV * 1000;
		with4.MinVS		= (with4.SMThreshold / 100) * with4.BasekV * 1000;
		with4.MinAmps	= (FpctCutOut / 100) * ((Get_FkVARating() / with4.BasekV) / with3.NumPhases);
		with4.iMaxPPhase= (Get_FkVARating() / with4.BasekV) / with3.NumPhases;

		if (pctX == 0)
			pctX			= 50;
		with3.XThev		= pctX * BaseZt;
		with4.RS		= pctR * BaseZt;
		with3.Zthev		= cmplx(with4.RS, with3.XThev);
		Yeq				= cinv(with3.Zthev);
		ComputePresentkW();
		with4.LS = with3.Zthev.im / (2 * PI * DefaultBaseFreq);
		for (i = 0; i < Get_NPhases(); i++)
		{
			with4.Vgrid[i]	= ctopolar(with2->NodeV[NodeRef[i]]);
			with4.dit[i]	= 0;
			with4.it[i]		= 0;
			with4.m[i]		= ((with4.RS * with4.it[i]) + with4.Vgrid[i].mag) / with4.RatedVDC;
			if (with4.m[i] > 1)
				with4.m[i]		= 1;
			with4.ISPDelta[i] = 0;
			with4.AngDelta[i] = 0;
		}
		if (ASSIGNED(DynamicEqObj))
			for (i = 0; i < DynamicEqVals.size(); i++)
				DynamicEqVals[i][1] = 0;
	}
}

//----------------------------------------------------------------------------


// dynamics mode integration routine

void TStorageObj::IntegrateStates(int ActorID)
{
    //complex TracePower = {};
    vector<complex> myCurr = {};
	int		NumData		= 0,
			IPresent    = 0, // Present amps per phase
			j			= 0,
			i			= 0;
	double	IMaxPhase	= 0,
			OFFVal		= 0;
	bool    GFMUpdate   = 0;

   // Compute Derivatives and Then integrate
	ComputeIterminal(ActorID);
	if(DynaModel->Get_Exists())   // Checks for existence and Selects
		DynaModel->Integrate();
	else
	{
		auto	wSol	= ActiveCircuit[ActorID]->Solution;
		auto&	wSV		= StorageVars;
		auto&	wDynV	= myDynVars;

		ComputePresentkW();
		IMaxPhase = (kW_out / wDynV.BasekV) / wSV.NumPhases;
		for (i = 0; i < wSV.NumPhases; i++)
		{
			if (fState == STORE_DISCHARGING)
			{
				auto& DynaVars = wSol->DynaVars;
				if (DynaVars.IterationFlag == 0) //{ First iteration of new time step }
					wDynV.itHistory[i] = wDynV.it[i] + 0.5 * DynaVars.h * wDynV.dit[i];

				wDynV.Vgrid[i] = ctopolar(wSol->NodeV[NodeRef[i]]);// Voltage at the Inv terminals
				if (!GFM_Mode)
				{
					if ((wDynV.Vgrid[i].mag < wDynV.MinVS) || (wDynV.Vgrid[i].mag > wDynV.MaxVS))
					{
						wDynV.ISP = 0.01;						// turn OFF the inverter
						fState = STORE_IDLING;
						if (wDynV.Vgrid[i].mag > wDynV.MaxVS)
							wDynV.Vgrid[i].mag = wDynV.MaxVS;
					}
					else
						wDynV.ISP = ((kW_out * 1000) / wDynV.Vgrid[i].mag) / wSV.NumPhases;
					if (wDynV.ISP > wDynV.iMaxPPhase)
						wDynV.ISP = wDynV.iMaxPPhase;
				}
				else
				{
					if (wDynV.ResetIBR)	wDynV.VDelta[i] = ((0.001 - (wDynV.Vgrid[i].mag / 1000)) / wDynV.BasekV);
					else				wDynV.VDelta[i] = (wDynV.BasekV - (wDynV.Vgrid[i].mag / 1000)) / wDynV.BasekV;
					
					// Set true to update the IBR
					GFMUpdate = 1;

					// Checks if there is current limit set
					if (wDynV.ILimit > 0)
					{
						myCurr.resize(Fnphases);
						GetCurrents(&(myCurr[0]), ActorID);

						for (int stop = Fnphases, j = 1; j <= stop; j++)
						{
							IPresent = ctopolar(myCurr[j - 1]).mag;
							GFMUpdate = GFMUpdate && (IPresent < wDynV.ILimit * wDynV.VError);
						}
					}

					if (abs(wDynV.VDelta[i]) > wDynV.CtrlTol && GFMUpdate)
					{
						wDynV.ISPDelta[i]	= wDynV.ISPDelta[i] + (IMaxPhase * wDynV.VDelta[i]) * wDynV.Kp * 100;
						if (wDynV.ISPDelta[i] > wDynV.iMaxPPhase)	wDynV.ISPDelta[i] = wDynV.iMaxPPhase;
						else if (wDynV.ISPDelta[i] < 0)				wDynV.ISPDelta[i] = 0.01;
					}
					wDynV.ISP = wDynV.ISPDelta[i];
					wDynV.FixPhaseAngle(ActorID, i);
				}
				if (ASSIGNED(DynamicEqObj))														// Loads values into dynamic expression if any
				{
					NumData = (int)(round((DynamicEqPair.size() / 2)) - 1);
					DynamicEqVals[DynOut[0]][0] = wDynV.it[i];									// brings back the current values/phase
					DynamicEqVals[DynOut[0]][1] = wDynV.dit[i];

					for (j = 0; j <= NumData; j++)
					{
						if (!DynamicEqObj->IsInitVal(DynamicEqPair[(j * 2) + 1]))
						{
							switch (DynamicEqPair[ ( j * 2 ) + 1 ])
							{
							case	2:
								DynamicEqVals[DynamicEqPair[j * 2]][0] = wDynV.Vgrid[i].mag;  // volt per phase
								break;
							case	4:														  // Nothing for this object (current)
								break;
							case	10:
								DynamicEqVals[DynamicEqPair[j * 2]][0] = wDynV.RatedVDC;
								break;
							case	11:
								wDynV.SolveModulation(i, ActorID, &PICtrl[i]);
								DynamicEqVals[DynamicEqPair[j * 2]][0] = wDynV.m[i];
								break;
							default:
								DynamicEqVals[DynamicEqPair[j * 2]][0] = Get_PCEValue(1, DynamicEqPair[(j * 2) + 1], ActorID);
								break;
							}
						}
					}
					DynamicEqObj->SolveEq(&DynamicEqVals);									// solves the differential equation using the given dynamic expression
				}
				else
					wDynV.SolveDynamicStep(i, ActorID, &PICtrl[i]);							// Solves dynamic step for inverter

				// Trapezoidal method
				if (ASSIGNED(DynamicEqObj)) wDynV.dit[i] = DynamicEqVals[DynOut[0]][1];
				wDynV.it[i] = wDynV.itHistory[i] + 0.5 * DynaVars.h * wDynV.dit[i];
			}
			else
			{
				if(wDynV.Vgrid[i].mag >= wDynV.MinVS || wDynV.ResetIBR)
					OFFVal = PIdling / wDynV.Vgrid[i].mag; // To match with idling losses
				else
					OFFVal = 0;

				wDynV.it[i] = OFFVal;
			}

			// Write Dynamics Trace Record
			if (DebugTrace)
			{
				Append(Tracefile);
				IOResultToException();
				WriteLn(Tracefile, Format("t=%-.5g", wSol->DynaVars.T));
				WriteLn(Tracefile, Format("Flag=%d", wSol->DynaVars.IterationFlag));
				WriteLn(Tracefile);
				CloseFile(Tracefile);
			}
		}


	}
}

//----------------------------------------------------------------------------

int TStorageObj::InterpretState(const String s)
{
	int result = 0;
	switch(LowerCase(s)[0])
	{
		case 	L'c':
		result = STORE_CHARGING;
		break;
		case 	L'd':
		result = STORE_DISCHARGING;
		break;
		default:
		result = STORE_IDLING;
		break;
	}
	return result;
}

/* apparently for debugging only
//----------------------------------------------------------------------------
Function TStorageObj.StateToStr:String;
Begin
      CASE FState of
          STORE_CHARGING: Result := 'Charging';
          STORE_IDLING: Result := 'Idling';
          STORE_DISCHARGING: Result := 'Discharging';
      END;
End;
*/

//----------------------------------------------------------------------------

/*Return variables one at a time*/

double TStorageObj::Get_Variable(int i)
{
	double result = 0.0;
	int n = 0;
	int k = 0;
	result = -9999.99;  // error return value; no state vars
	if(i < 1)
		return result;
    // for now, report kWhstored and mode
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		switch(i)
		{
			case 	1:
				result = with0.kWhStored;
				break;
			case 	2:
				if(not GFM_Mode)
				result = (double) fState;
				else
				{
					if (CheckIfDelivering(ActiveActor))
						result = STORE_DISCHARGING;
					else if (with0.kWhStored == with0.kWhRating)
						result = STORE_IDLING;
					else
						result = STORE_CHARGING;
				}
				break;
			case 	3:
			case	4:
			{
				bool A = GFM_Mode && CheckIfDelivering(ActiveActor);
				bool B = (fState == STORE_DISCHARGING) && !(GFM_Mode);
				A = A || B;
				if (i == 4)	A = !A;
				if (A)
					result = abs(Get_Power(1, ActiveActor).re * 0.001);
				else
					result = 0;
			}
				break;
			case 	5:
				result = -1 * Get_Power(1,ActiveActor).im * 0.001;
				break;
			case 	6:
				result = Get_DCkW();
				break;
			case 	7:
				result = Get_kWTotalLosses();
				break; /*Present kW charge or discharge loss incl idle losses*/
			case 	8:
				result = Get_InverterLosses();
				break; /*Inverter Losses*/
			case 	9:
				result = Get_kWIdlingLosses();
				break; /*Present kW Idling Loss*/
			case 	10:
				result = Get_kWChDchLosses();
				break;  // Charge/Discharge Losses
			case 	11:
				result = with0.kWhStored - kWhBeforeUpdate;
				break;
			case 	12:
				{
					Update_EfficiencyFactor();
					result = with0.EffFactor;  //Old: Result    := Get_EfficiencyFactor;
				}
				break;
			case 	13:
				if(FInverterON)
					result = 1.0;
				else
					result = 0.0;
				break;
			case 	14:
				result = with0.Vreg;
				break;
			case 	15:
				result = with0.Vavg;
				break;
			case 	16:
				result = with0.VVOperation;
				break;
			case 	17:
				result = with0.VWOperation;
				break;
			case 	18:
				result = with0.DRCOperation;
				break;
			case 	19:
				result = with0.VVDRCOperation;
				break;
			case 	20:
				result = with0.WPOperation;
				break;
			case 	21:
				result = with0.WVOperation;
				break;
			case 	22:
				result = Get_kWDesired();
				break;
			case 	23:
			if(!(Get_VWmode()))
				result = 9999;
			else
				result = Get_kWRequested();
				break;
			case 	24:
				result = Get_FpctkWrated() * with0.kWrating;
				break;
			case 	25:
				if(kVA_exceeded)
					result = 1.0;
				else
					result = 0.0;
				break;
			default:
				result = myDynVars.Get_InvDynValue(i - 26, Get_NPhases());

			if(UserModel->Get_Exists())   // Checks for existence and Selects
			{
				n = UserModel->FNumVars();
				k = (i - NumStorageVariables);
				if(k <= n)
				{
					result = UserModel->FGetVariable(k);
					return result;
				}
			}
			if(DynaModel->Get_Exists())  // Checks for existence and Selects
			{
				n = DynaModel->FNumVars();
				k = (i - NumStorageVariables);
				if(k <= n)
				{
					result = DynaModel->FGetVariable(k);
					return result;
				}
			}
			break;
		}
	}
	return result;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_Variable(int i, double Value)
{
	int n = 0;
	int k = 0;
	if(i < 1)
		return;  // No variables to set
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		switch(i)
		{
			case 	1:
			with0.kWhStored = Value;
			break;
			case 	2:
			fState = Trunc(Value);
			break;
			case 3: case 4: case 5: case 6: case 7: case 8: case 9: case 10: case 11: case 12:
			 case 13:
			;
			break; /*Do Nothing; read only*/
			case 	14:
				with0.Vreg = Value;
			break;
			case 	15:
				with0.Vavg = Value;
			break;
			case 	16:
				with0.VVOperation = Value;
			break;
			case 	17:
				with0.VWOperation = Value;
			break;
			case 	18:
				with0.DRCOperation = Value;
			break;
			case 	19:
				with0.VVDRCOperation = Value;
			break;
			case 	20:
				with0.WPOperation = Value;
			break;
			case 	21:
				with0.WVOperation = Value;
			break;
			case 22: case 23: case 24: case 25:
			;
			break; /*Do Nothing; read only*/
			default:
			auto& with1 = myDynVars; // Dynamic state variable write

			with1.Set_InvDynValue(i - 26, Value);

			if(UserModel->Get_Exists())    // Checks for existence and Selects
			{
				n = UserModel->FNumVars();
				k = (i - NumStorageVariables);
				if(k <= n)
				{
					UserModel->FSetVariable(k, Value);
					return;
				}
			}
			if(DynaModel->Get_Exists())     // Checks for existence and Selects
			{
				n = DynaModel->FNumVars();
				k = (i - NumStorageVariables);
				if(k <= n)
				{
					DynaModel->FSetVariable(k, Value);
					return;
				}
			}
			break;
		}
	}
}

//----------------------------------------------------------------------------

void TStorageObj::GetAllVariables(pDoubleArray States)
{
/*, N*/
	int i = 0;
	int stop = 0;
	if (!ASSIGNED(DynamicEqObj))
	{
		for (stop = NumStorageVariables, i = 1; i <= stop; i++)
			States[i - 1] = Get_Variable(i);
	}
	else
	{
		for (i = 1; i <= (DynamicEqObj->get_FNumVars() * DynamicEqVals[0].size()); i++)
			States[i - 1] = DynamicEqObj->Get_DynamicEqVal(i - 1, &DynamicEqVals);
	}

	if (UserModel->Get_Exists())    // Checks for existence and Selects
		        /*N := UserModel.FNumVars;*/
		{
			UserModel->FGetAllVars((pDoubleArray)&(States)[NumStorageVariables + 1 - 1]);
		}
	if(DynaModel->Get_Exists())    // Checks for existence and Selects
		        /*N := UserModel.FNumVars;*/
		{
			DynaModel->FGetAllVars((pDoubleArray)&(States)[NumStorageVariables + 1 - 1]);
		}
}

//----------------------------------------------------------------------------

int TStorageObj::NumVariables()
{
	int result = 0;
	result = NumStorageVariables;

     // Get_Exists() does a check and then does a Select
	if(UserModel->Get_Exists())
		result = result + UserModel->FNumVars();
	if(DynaModel->Get_Exists())
		result = result + DynaModel->FNumVars();
	return result;
}

//----------------------------------------------------------------------------

String TStorageObj::VariableName(int i)
{
	String result;
	const int BuffSize = 255;
	int n = 0;
	int I2 = 0;
	AnsiChar Buff[256/*# range 0..BuffSize*/];
	PAnsiChar PName = nullptr;
	if(i < 1)
		return result;  // Someone goofed
	switch(i)
	{
		case 	1:
		result = "kWh";
		break;
		case 	2:
		result = "State";
		break;
//          3:Result  := 'Pnom';
//          4:Result  := 'Qnom';
		case 	3:
		result = "kWOut";
		break;
		case 	4:
		result = "kWIn";
		break;
		case 	5:
		result = "kvarOut";
		break;
		case 	6:
		result = "DCkW";
		break;
		case 	7:
		result = "kWTotalLosses";
		break;
		case 	8:
		result = "kWInvLosses";
		break;
		case 	9:
		result = "kWIdlingLosses";
		break;
		case 	10:
		result = "kWChDchLosses";
		break;
		case 	11:
		result = "kWh Chng";
		break;
		case 	12:
		result = "InvEff";
		break;
		case 	13:
		result = "InverterON";
		break;
		case 	14:
		result = "Vref";
		break;
		case 	15:
		result = "Vavg (DRC)";
		break;
		case 	16:
		result = "VV Oper";
		break;
		case 	17:
		result = "VW Oper";
		break;
		case 	18:
		result = "DRC Oper";
		break;
		case 	19:
		result = "VV_DRC Oper";
		break;
		case 	20:
		result = "WP Oper";
		break;
		case 	21:
		result = "WV Oper";
		break;
		case 	22:
		result = "kWDesired";
		break;
		case 	23:
		result = "kW VW Limit";
		break;
		case 	24:
		result = "Limit kWOut Function";
		break;
		case 	25:
		result = "kVA Exceeded";
		break;
		default:
			result = myDynVars.Get_InvDynName(i - 26);

			if(UserModel->Get_Exists())    // Checks for existence and Selects
			{
				PName = (PAnsiChar) &Buff;
				n = UserModel->FNumVars();
				I2 = i - NumStorageVariables;
				if(I2 <= n)
				{
					UserModel->FGetVarName(I2, PName, (unsigned int) BuffSize);
					result = PName;
					return result;
				}
			}
			if(DynaModel->Get_Exists())   // Checks for existence and Selects
			{
				PName = (PAnsiChar) &Buff;
				n = DynaModel->FNumVars();
				I2 = i - NumStorageVariables; // Relative index
				if(I2 <= n)
				{
					DynaModel->FGetVarName(I2, PName, (unsigned int) BuffSize);
					result = PName;
					return result;
				}
			}
			break;
	}
	return result;
}

//----------------------------------------------------------------------------

void TStorageObj::MakePosSequence(int ActorID)
{
	String s;
	double V = 0.0;
	s = "Phases=1 conn=wye";

  // Make sure voltage is line-neutral
	if((Fnphases > 1) || (Connection != 0))
		V = StorageVars.kVStorageBase / SQRT3;
	else
		V = StorageVars.kVStorageBase;
	s = s + Format(" kV=%-.5g", V);
	if(Fnphases > 1)
	{
		s = s
	           + Format(" kWrating=%-.5g  PF=%-.5g", StorageVars.kWrating / Fnphases, PFNominal);
	}
	Parser[ActorID]->SetCmdString(s);
	Edit(ActorID);
	inherited::MakePosSequence(ActorID);   // write out other properties
}

//----------------------------------------------------------------------------

void TStorageObj::Set_ConductorClosed(int Index, int ActorID, bool Value)
{
	inherited::Set_ConductorClosed(Index, ActorID, Value);

 // Just turn Storage element on or off;
	if(Value)
		StorageObjSwitchOpen = false;
	else
		StorageObjSwitchOpen = true;
}

//----------------------------------------------------------------------------

bool TStorageObj::Get_InverterON()
{
	bool result = false;
	if(FInverterON)
		result = true;
	else
		result = false;
	return result;
}

//----------------------------------------------------------------------------

void TStorageObj::kWOut_Calc()
{
	double limitkWpct = 0.0;
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		FVWStateRequested = false;
		if(fState == STORE_DISCHARGING)
			limitkWpct = with0.kWrating * with0.FpctkWrated;
		else
			limitkWpct = with0.kWrating * with0.FpctkWrated *  - 1;

//          if VWmode and (FState = STORE_DISCHARGING) then if (abs(kwRequested) < abs(limitkWpct)) then limitkWpct :=  kwRequested * sign(kW_Out);
          // VW works only if element is not in idling state.
          // When the VW is working in the 'limiting' region, kWRequested will be positive.
          // When in 'requesting' region, it will be negative.
		if(Get_VWmode() && !(fState == STORE_IDLING))
		{
			if((Get_kWRequested() >= 0.0) && (Abs(Get_kWRequested()) < Abs( limitkWpct)))  // Apply VW limit
			{
				if(fState == STORE_DISCHARGING)
					limitkWpct = Get_kWRequested();
				else
					limitkWpct = (double) (-1 * Get_kWRequested());
			}
			else
			{
				if(Get_kWRequested() < 0.0) // IEEE 1547 Requesting Region (not fully implemented)
				{
					if(fState == STORE_DISCHARGING)
					{
						if(with0.kWhStored < with0.kWhRating)  // let it charge only if enough not fully charged
						{
							fState = STORE_CHARGING;
							kW_out = Get_kWRequested();
						}
						else
						{
							fState = STORE_IDLING;
							kW_out = -kWOutIdling;
						}
					}
					else
  // Charging
					{
						if(with0.kWhStored > with0.kWhReserve)  // let it charge only if enough not fully charged
						{
							fState = STORE_DISCHARGING;
							kW_out = (double) (-1 * Get_kWRequested());
						}
						else
						{
							fState = STORE_IDLING;
							kW_out = -kWOutIdling;
						}
					}
					FStateChanged = true;
					FVWStateRequested = true;

                // Update limitkWpct because state might have been changed
					if(fState == STORE_DISCHARGING)
						limitkWpct = with0.kWrating * with0.FpctkWrated;
					else
						limitkWpct = with0.kWrating * with0.FpctkWrated *  - 1;
				}
			}
		}
		if((limitkWpct > 0) && (kW_out > limitkWpct))
			kW_out = limitkWpct;
		else
		{
			if((limitkWpct < 0) && (kW_out < limitkWpct))
				kW_out = limitkWpct;
		}
	}
}

//----------------------------------------------------------------------------

int TStorageObj::Get_Varmode()
{
	int result = 0;
	result = FvarMode;
	return result;
}

//----------------------------------------------------------------------------

bool TStorageObj::Get_VWmode()
{
	bool result = false;
	if(FVWMode)
		result = true;
	else
		result = false;    // TRUE if volt-watt mode
                                                              //  engaged from InvControl (not ExpControl)
	return result;
}

// ============================================================Get_WPmode===============================

bool TStorageObj::Get_WPmode()
{
	bool result = false;
	if(FWPMode)
		result = true;
	else
		result = false;                                                               //  engaged from InvControl (not ExpControl)
	return result;
}

// ============================================================Get_WVmode===============================

bool TStorageObj::Get_WVmode()
{
	bool result = false;
	if(FWVMode)
		result = true;
	else
		result = false;                                                               //  engaged from InvControl (not ExpControl)
	return result;
}

//----------------------------------------------------------------------------

bool TStorageObj::Get_VVmode()
{
	bool result = false;
	if(FVVMode)
		result = true;
	else
		result = false;
	return result;
}


//----------------------------------------------------------------------------

bool TStorageObj::Get_DRCmode()
{
	bool result = false;
	if(FDRCMode)
		result = true;
	else
		result = false;
	return result;
}

//----------------------------------------------------------------------------

bool TStorageObj::Get_AVRmode()
{
    bool result = false;

    if (FAVRMode)
        result = true;

    return result;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_CutOutkWAC()
{
	double result = 0.0;
	result = FCutOutkWAC;
	return result;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_CutInkWAC()
{
	double result = 0.0;
	result = FCutInkWAC;
	return result;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_pctkWOut(double Value)
{
	FpctkWout = Value;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_pctkWIn(double Value)
{
	FpctkWin = Value;
}

//----------------------------------------------------------------------------

// ===========================================================================================

void TStorageObj::Set_WVmode(bool Value)
{
	FWVMode = Value;
}


// ===========================================================================================

void TStorageObj::Set_WPmode(bool Value)
{
	FWPMode = Value;
}

void TStorageObj::Set_kW(double Value)
{
	if(Value > 0)
	{
		fState = STORE_DISCHARGING;
		FpctkWout = Value / StorageVars.kWrating * 100.0;
	}
	else
	{
		if(Value < 0)
		{
			fState = STORE_CHARGING;
			FpctkWin = double(Abs( Value)) / StorageVars.kWrating * 100.0;
		}
		else
		{
			fState = STORE_IDLING;
		}
	}
}

//----------------------------------------------------------------------------

void TStorageObj::Set_Maxkvar(double Value)
{
	StorageVars.Fkvarlimit = Value;
	Set_PropertyValue(propkvarLimit,Format("%-g", StorageVars.Fkvarlimit));
}

//----------------------------------------------------------------------------

void TStorageObj::Set_Maxkvarneg(double Value)
{
	StorageVars.Fkvarlimitneg = Value;
	Set_PropertyValue(propkvarLimitneg,Format("%-g", StorageVars.Fkvarlimitneg));
}

//----------------------------------------------------------------------------

void TStorageObj::Set_kVARating(double Value)
{
	StorageVars.FkVArating = Value;
	Set_PropertyValue(propKVA,Format("%-g", StorageVars.FkVArating));
}

//----------------------------------------------------------------------------

void TStorageObj::Set_PowerFactor(double Value)
{
	PFNominal = Value;
	Set_Varmode(VARMODEPF);
}

//----------------------------------------------------------------------------

void TStorageObj::Set_Varmode(int Value)
{
	FvarMode = Value;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_VWmode(bool Value)
{
	FVWMode = Value;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_VVmode(bool Value)
{
	FVVMode = Value;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_DRCmode(bool Value)
{
	FDRCMode = Value;
}
//----------------------------------------------------------------------------

void TStorageObj::Set_AVRmode(bool Value)
{
	FAVRMode = Value;
}
//----------------------------------------------------------------------------

void TStorageObj::Set_PresentkV(double Value)
{
	StorageVars.kVStorageBase = Value;
	switch(Fnphases)
	{
		case 	2:
		 case 3:
		VBase = StorageVars.kVStorageBase * InvSQRT3x1000;
		break;
		default:
		VBase = StorageVars.kVStorageBase * 1000.0;
		break;
	}
}

//----------------------------------------------------------------------------

void TStorageObj::Set_kvarRequested(double Value)
{
	FkvarRequested = Value;
}

//----------------------------------------------------------------------------
/*
void TStorageObj::Set_pf_wp_nominal(double Value)
{
	Fpf_wp_nominal = Value;
}
*/
void TStorageObj::Set_kWRequested(double Value)
{
	FkWRequested = Value;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_kW()
{
	double result = 0.0;
	switch(fState)
	{
		case 	STORE_CHARGING:
		result = -Get_FpctkWIn() * StorageVars.kWrating / 100.0;
		break;
		case 	STORE_DISCHARGING:
		result = get_FpctkWout() * StorageVars.kWrating / 100.0;
		break;
		case 	STORE_IDLING:
		result = -kWOutIdling;
		break;
		default:
		  ;
		break;
	}
	return result;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_kWRequested()
{
	double result = 0.0;
	result = FkWRequested;
	return result;
}

//----------------------------------------------------------------------------

double TStorageObj::Get_kvarRequested()
{
	double result = 0.0;
	result = FkvarRequested;
	return result;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_VarFollowInverter(bool Value)
{
	FVarFollowInverter = Value;
}

//----------------------------------------------------------------------------

bool TStorageObj::Get_VarFollowInverter()
{
	bool result = false;
	if(FVarFollowInverter)
		result = true;
	else
		result = false;
	return result;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_pctkWrated(double Value)
{
	StorageVars.FpctkWrated = Value;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_InverterON(bool Value)
{
	FInverterON = Value;
}

//----------------------------------------------------------------------------

void TStorageObj::Set_StorageState(int Value)
{
	int SavedState = 0;
	SavedState = fState;

     // Decline if Storage is at its limits ; set to idling instead
	/*# with StorageVars do */
	{
		auto& with0 = StorageVars;
		switch(Value)
		{
			case 	STORE_CHARGING:
			{
				if(with0.kWhStored < with0.kWhRating)
					fState = Value;
				else
					fState = STORE_IDLING;   // all charged up
			}
			break;
			case 	STORE_DISCHARGING:
			{
				if(with0.kWhStored > with0.kWhReserve)
					fState = Value;
				else
					fState = STORE_IDLING;  // not enough Storage to discharge
			}
			break;
			default:
			fState = STORE_IDLING;
			break;
		}
	}
	if(SavedState != fState)
		FStateChanged = true;

     //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, ---State Set To %s', [ActiveCircuit[ActiveActor].Solution.dblHour, StateToStr ]));
}
//----------------------------------------------------------------------------

void TStorageObj::SetDragHandRegister(int reg, double Value)
{
	if(Value > Registers[reg])
		Registers[reg] = Value;
}

//----------------------------------------------------------------------------


void Storage_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
}

		class 		Storage_unit
		{
		public:
		Storage_unit()
		{
			//AssertSystemInitialization();
			Storage_initialization();
		}
		};
		Storage_unit _Storage_unit;

}  // namespace Storage




