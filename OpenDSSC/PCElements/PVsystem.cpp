

#pragma hdrstop

#include "PVsystem.h"

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
using namespace PVsystemUserModel;
using namespace ParserDel;
using namespace System;
using namespace TempShape;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XYCurve;
using namespace mathutil;
using namespace Utilities;

namespace PVSystem
{

TPVsystemObj::TPVsystemObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TPVsystemObj::TPVsystemObj(String ClassName) : inherited(ClassName) {}
TPVsystemObj::TPVsystemObj() {}


TPVsystemObj* ActivePVsystemObj = nullptr;
// ===========================================================================================
/*
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit FUNCTION
    3) add a statement(s) to InitPropertyValues FUNCTION to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, If needed
*/
// ===========================================================================================
const int propKV				= 3;
const int propIrradiance		= 4;
const int propPF				= 5;
const int propMODEL				= 6;
const int propYEARLY			= 7;
const int propDAILY				= 8;
const int propDUTY				= 9;
const int propTYEARLY			= 10;
const int propTDAILY			= 11;
const int propTDUTY				= 12;
const int PropConnection		= 13;
const int propKVAR				= 14;
const int propPCTR				= 15;
const int propPCTX				= 16;
const int propCLASS				= 17;
const int propInvEffCurve		= 18;
const int propTemp				= 19;
const int propPmpp				= 20;
const int propP_T_Curve			= 21;
const int propCutin				= 22;
const int propCutout			= 23;
const int propVMINPU			= 24;
const int propVMAXPU			= 25;
const int propKVA				= 26;
const int propUSERMODEL			= 27;
const int propUSERDATA			= 28;
const int propDEBUGTRACE		= 29;
const int proppctPmpp			= 30;
const int propBalanced			= 31;
const int propLimited			= 32;
const int propVarFollowInverter = 33;
const int propkvarLimit			= 34;
const int propDutyStart			= 35;
const int propPpriority			= 36;
const int propPFpriority		= 37;
const int propPminNoVars		= 38;
const int propPminkvarLimit		= 39;
const int propkvarLimitneg		= 40;
const int propkVDC				= 41;
const int propkp				= 42;
const int propCtrlTol			= 43;
const int propSMT				= 44;
const int propSM				= 45;
const int propDynEq				= 46;
const int propDynOut			= 47;
const int propGFM				= 48;
const int propAmpsLimit			= 49;
const int propAmpsError			= 50;
const int NumPropsThisClass		= 50; // Make this agree with the last property constant
complex cBuffer[25/*# range 1..24*/];  // Temp buffer for calcs  24-phase PVSystem element?
complex CDoubleOne = {};  // Creates superstructure for all PVSystem elements

TPVSystem::TPVSystem()
{
	;
	Class_Name = "PVSystem";
	DSSClassType = DSSClassType + PVSYSTEM_ELEMENT;  // In both PCelement and PVSystem element list
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

TPVSystem::~TPVSystem()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


void TPVSystem::DefineProperties()
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
	AddProperty("phases", 1, "Number of Phases, this PVSystem element.  Power is evenly divided among phases.");
	AddProperty("bus1", 2, "Bus to which the PVSystem element is connected.  May include specific node specification.");
	AddProperty("kv", propKV, "Nominal rated (1.0 per unit) voltage, kV, for PVSystem element. For 2- and 3-phase PVSystem elements, specify phase-phase kV. "
	           "Otherwise, specify actual kV across each branch of the PVSystem element. "
	           "If 1-phase wye (star or LN), specify phase-neutral kV. "
	           "If 1-phase delta or phase-phase connected, specify phase-phase kV.");  // line-neutral voltage//  base voltage
	AddProperty("irradiance", propIrradiance, "Get/set the present irradiance value in kW/sq-m. Used as base value for shape multipliers. "
	           "Generally entered as peak value for the time period of interest and the yearly, daily, and duty load shape "
	           "objects are defined as per unit multipliers (just like Loads/Generators).");
	AddProperty("Pmpp", propPmpp, "Get/set the rated max power of the PV array for 1.0 kW/sq-m irradiance and a user-selected array temperature. "
	           "The P-TCurve should be defined relative to the selected array temperature.");
	AddProperty("%Pmpp", proppctPmpp, "Upper limit on active power as a percentage of Pmpp.");
	AddProperty("Temperature", propTemp, "Get/set the present Temperature. Used as fixed value corresponding to PTCurve property. "
	           "A multiplier is obtained from the Pmpp-Temp curve and applied to the nominal Get_FPmpp() from the irradiance "
	           "to determine the net array output.");
	AddProperty("pf", propPF, String("Nominally, the power factor for the output power. Default is 1.0. " "Setting this property will cause the inverter to operate in constant power factor mode." "Enter negative when kW and kvar have opposite signs.") + CRLF
	           + "A positive power factor signifies that the PVSystem element produces vars "
	           + CRLF
	           + "as is typical for a generator.  ");
	AddProperty("conn", PropConnection, "={wye|LN|delta|LL}.  Default is wye.");
	AddProperty("kvar", propKVAR, "Get/set the present kvar value.  Setting this property forces the inverter to operate in constant kvar mode.");
	AddProperty("kVA", propKVA, "kVA rating of inverter. Used as the base for Dynamics mode and Harmonics mode values.");
	AddProperty("%Cutin", propCutin, "% cut-in power -- % of kVA rating of inverter. "
	           "When the inverter is OFF, the power from the array must be greater than this for the inverter to turn on.");
	AddProperty("%Cutout", propCutout, "% cut-out power -- % of kVA rating of inverter. "
	           "When the inverter is ON, the inverter turns OFF when the power from the array drops below this value.");
	AddProperty("EffCurve", propInvEffCurve, "An XYCurve object, previously defined, that describes the PER UNIT efficiency vs PER UNIT of rated kVA for the inverter. "
	           "Inverter output power is discounted by the multiplier obtained from this curve.");
	AddProperty("P-TCurve", propP_T_Curve, "An XYCurve object, previously defined, that describes the PV array PER UNIT Get_FPmpp() vs Temperature curve. "
	           "Temperature units must agree with the Temperature property and the Temperature shapes used for simulations. "
	           "The Pmpp values are specified in per unit of the Get_FPmpp() value for 1 kW/sq-m irradiance. "
	           "The value for the temperature at which Get_FPmpp() is defined should be 1.0. "
	           "The net array power is determined by the irradiance * Get_FPmpp() * f(Temperature)");
	AddProperty("%R", propPCTR, "Equivalent percent internal resistance, ohms. Default is 50%. Placed in series with internal voltage source"
	           " for harmonics and dynamics modes. (Limits fault current to about 2 pu if not current limited -- see LimitCurrent) ");
	AddProperty("%X", propPCTX, "Equivalent percent internal reactance, ohms. Default is 0%. Placed in series with internal voltage source"
	           " for harmonics and dynamics modes. ");
	AddProperty("model", propMODEL, String("Integer code (default=1) for the model to use for power output variation with voltage. " "Valid values are:") + CRLF
	           + CRLF
	           + "1:PVSystem element injects a CONSTANT kW at specified power factor."
	           + CRLF
	           + "2:PVSystem element is modeled as a CONSTANT ADMITTANCE."
	           + CRLF
	           + "3:Compute load injection from User-written Model.");
	AddProperty("Vminpu", propVMINPU, "Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. "
	           "Below this value, the load model reverts to a constant impedance model except for Dynamics model. "
	           "In Dynamics mode, the current magnitude is limited to the value the power flow would compute for this voltage.");
	AddProperty("Vmaxpu", propVMAXPU, "Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. "
	           "Above this value, the load model reverts to a constant impedance model.");
	AddProperty("Balanced", propBalanced, "{Yes | No*} Default is No.  Force balanced current only for 3-phase PVSystems. Forces zero- and negative-sequence to zero. ");
	AddProperty("LimitCurrent", propLimited, "Limits current magnitude to Vminpu value for both 1-phase and 3-phase PVSystems similar to Generator Model 7. For 3-phase, "
	           "limits the positive-sequence current but not the negative-sequence.");
	AddProperty("yearly", propYEARLY, "Dispatch shape to use for yearly simulations.  Must be previously defined "
	           "as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated "
	           "during Yearly solution modes. In the default dispatch mode, "
	           "the PVSystem element uses this loadshape to trigger State changes.");
	AddProperty("daily", propDAILY, "Dispatch shape to use for daily simulations.  Must be previously defined "
	           "as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, "
	           "the PVSystem element uses this loadshape to trigger State changes."); // daily dispatch (hourly)
	AddProperty("duty", propDUTY, "Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. "
	           "Must be previously defined as a Loadshape object. "
	           "Typically would have time intervals of 1-5 seconds. "
	           "Designate the number of points to solve using the Set Number=xxxx command. "
	           "If there are fewer points in the actual shape, the shape is assumed to repeat.");  // as for wind generation
	AddProperty("Tyearly", propTYEARLY, "Temperature shape to use for yearly simulations.  Must be previously defined "
	           "as a TShape object. If this is not specified, the Daily dispatch shape, if any, is repeated "
	           "during Yearly solution modes. "
	           "The PVSystem element uses this TShape to determine the Pmpp from the Get_FPmpp() vs T curve. "
	           "Units must agree with the Get_FPmpp() vs T curve.");
	AddProperty("Tdaily", propTDAILY, "Temperature shape to use for daily simulations.  Must be previously defined "
	           "as a TShape object of 24 hrs, typically.  "
	           "The PVSystem element uses this TShape to determine the Pmpp from the Get_FPmpp() vs T curve. "
	           "Units must agree with the Get_FPmpp() vs T curve."); // daily dispatch (hourly)
	AddProperty("Tduty", propTDUTY, "Temperature shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. "
	           "Must be previously defined as a TShape object. "
	           "Typically would have time intervals of 1-5 seconds. "
	           "Designate the number of points to solve using the Set Number=xxxx command. "
	           "If there are fewer points in the actual shape, the shape is assumed to repeat. "
	           "The PVSystem model uses this TShape to determine the Pmpp from the Get_FPmpp() vs T curve. "
	           "Units must agree with the Get_FPmpp() vs T curve.");  // Cloud transient simulation
	AddProperty("class", propCLASS, "An arbitrary integer number representing the class of PVSystem element so that PVSystem values may "
	           "be segregated by class."); // integer
	AddProperty("UserModel", propUSERMODEL, "Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, "
	           "overriding the default model.  Set to \"none\" to negate previous setting.");
	AddProperty("UserData", propUSERDATA, "String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.");
	AddProperty("debugtrace", propDEBUGTRACE, "{Yes | No }  Default is no.  Turn this on to capture the progress of the PVSystem model "
	           "for each iteration.  Creates a separate file for each PVSystem element named \"PVSystem_name.CSV\".");
	AddProperty("VarFollowInverter", propVarFollowInverter, "Boolean variable (Yes|No) or (True|False). Defaults to False which indicates that the reactive power generation/absorption does not respect the inverter status."
	           "When set to True, the PVSystem reactive power generation/absorption will cease when the inverter status is off, due to panel kW dropping below %Cutout.  The reactive power "
	           "generation/absorption will begin again when the panel kW is above %Cutin.  When set to False, the PVSystem will generate/absorb reactive power regardless of the status of the inverter.");
	AddProperty("DutyStart", propDutyStart, "Starting time offset [hours] into the duty cycle shape for this PVSystem, defaults to 0");
	AddProperty("WattPriority", propPpriority, "{Yes/No*/True/False} Set inverter to watt priority instead of the default var priority");
	AddProperty("PFPriority", propPFpriority, "{Yes/No*/True/False} Set inverter to operate with PF priority when in constant PF mode. If \"Yes\", value assigned to \"WattPriority\""
	           " is neglected. If controlled by an InvControl with either Volt-Var or DRC or both functions activated, PF priority is neglected and \"WattPriority\" is considered. Default = No.");
	AddProperty("%PminNoVars", propPminNoVars, "Minimum active power as percentage of Pmpp under which there is no vars production/absorption.");
	AddProperty("%PminkvarMax", propPminkvarLimit, "Minimum active power as percentage of Pmpp that allows the inverter to produce/absorb reactive power up to its kvarMax or kvarMaxAbs.");
	AddProperty("kvarMax", propkvarLimit, "Indicates the maximum reactive power GENERATION (un-signed numerical variable in kvar) for the inverter (as an un-signed value). Defaults to kVA rating of the inverter.");
	AddProperty("kvarMaxAbs", propkvarLimitneg, "Indicates the maximum reactive power ABSORPTION (un-signed numerical variable in kvar) for the inverter (as an un-signed value). Defaults to kVA rating of the inverter.");
	AddProperty("kVDC", propkVDC,
		"Indicates the rated voltage (kV) at the input of the inverter at the peak of PV energy production. The value is normally greater or equal to the kV base of the PV system. It is used for dynamics simulation ONLY.");

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
	PropertyHelp[NumPropsThisClass + 1 - 1] = "Name of harmonic voltage or current spectrum for this PVSystem element. "
	           "A harmonic voltage source is assumed for the inverter. "
	           "Default value is \"default\", which is defined when the DSS starts.";
}

int TPVSystem::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new PVSystem element and add it to PVSystem class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TPVsystemObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

void TPVSystem::SetNcondsForConnection()
{
	/*# with ActivePVsystemObj do */
	{
		auto with0 = ActivePVsystemObj;
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

void TPVSystem::UpdateAll()
{
	int i = 0;
	int stop = 0;
	for(stop = ElementList.get_myNumList(), i = 1; i <= stop; i++)
	{
		/*# with TPVsystemObj(ElementList.Get(i)) do */
		{
			auto with0 = ((TPVsystemObj*) ElementList.Get(i));
			if(with0->Get_Enabled())
				with0->UpdatePVSystem();
		}
	}
}
  // Accepts
  //    delta or LL           (Case insensitive)
  //    Y, wye, or LN

void TPVSystem::InterpretConnection(const String s)
{
	String TestS;
	/*# with ActivePVsystemObj do */
	{
		auto with0 = ActivePVsystemObj;
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
		/*# with PVSystemVars do */
		{
			auto& with1 = with0->PVSystemVars;
			switch(with0->Fnphases)
			{
				case 	2: case 3:
				with0->VBase = with1.kVPVSystemBase * InvSQRT3x1000;
				break;    // L-N Volts
				default:
				with0->VBase = with1.kVPVSystemBase * 1000.0;   // Just use what is supplied
				break;
			}
		}
		with0->VBaseMin = with0->Vminpu * with0->VBase;
		with0->VBaseMax = with0->Vmaxpu * with0->VBase;
		with0->Yorder = with0->Fnconds * with0->Fnterms;
		with0->Set_YprimInvalid(ActiveActor,true);
	}
}

//- - - - - - - - - - - - - - -MAIN EDIT FUNCTION - - - - - - - - - - - - - - -

int TPVSystem::Edit(int ActorID)
{
	int		result			= 0,
			i				= 0,
			iCase			= 0,
			VarIdx			= 0,
			ParamPointer	= 0;
	String	ParamName		= "",
			Param			= "";
  // continue parsing with contents of Parser
	ActivePVsystemObj = (TPVsystemObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActivePVsystemObj);
	result = 0;
	/*# with ActivePVsystemObj do */
	{
		auto with0 = ActivePVsystemObj;
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
				with0->Set_PropertyValue((PropertyIdxMap)[ParamPointer - 1],Param);
			else
			{
				VarIdx = with0->CheckIfDynVar(ParamName, ActorID);
				if (VarIdx < 0)
					DoSimpleMsg(String("Unknown parameter \"") + ParamName + "\" for PVSystem \"" + with0->get_Name() + "\"", 560);
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
						with0->Set_PresentkV(with0->PVSystemVars.kVPVSystemBase); // In case phases have been defined after
					break; // num phases
					case 	2:
						with0->SetBus(1, Param);
					break;
					case 	propKV:
						with0->Set_PresentkV(Parser[ActorID]->MakeDouble_());
					break;
					case 	propIrradiance:
						with0->PVSystemVars.FIrradiance = Parser[ActorID]->MakeDouble_();
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
					case 	propTYEARLY:
						with0->YearlyTShape = Param;
					break;
					case 	propTDAILY:
						with0->DailyTShape = Param;
					break;
					case 	propTDUTY:
						with0->DutyTShape = Param;
					break;
					case 	PropConnection:
						InterpretConnection(Param);
					break;
					case 	propKVAR:
					{
						with0->Set_Varmode(VARMODEKVAR);
						with0->Set_Presentkvar(Parser[ActorID]->MakeDouble_());
					}
					break;
					case 	propPCTR:
						with0->pctR = Parser[ActorID]->MakeDouble_();
					break;
					case 	propPCTX:
						with0->pctX = Parser[ActorID]->MakeDouble_();
					break;
					case 	propCLASS:
						with0->FClass = Parser[ActorID]->MakeInteger_();
					break;
					case 	propInvEffCurve:
						with0->InverterCurve = Param;
					break;
					case 	propTemp:
						with0->PVSystemVars.FTemperature = Parser[ActorID]->MakeDouble_();
					break;
					case 	propPmpp:
						with0->PVSystemVars.FPmpp = Parser[ActorID]->MakeDouble_();
					break;
					case 	propP_T_Curve:
						with0->Power_TempCurve = Param;
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
					case 	propKVA:
					/*# with PVSystemVars do */
					{
						auto& with1 = with0->PVSystemVars;
						with1.FkVArating = Parser[ActorID]->MakeDouble_();
						if(!with0->kvarLimitSet)
							with0->PVSystemVars.Fkvarlimit = with1.FkVArating;
						if(!(with0->kvarLimitSet) && !(with0->kvarLimitNegSet))
							with0->PVSystemVars.Fkvarlimitneg = with1.FkVArating;
					}
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
					case 	proppctPmpp:
						with0->PVSystemVars.FpuPmpp = Parser[ActorID]->MakeDouble_() / 100.0;
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
						with0->PVSystemVars.Fkvarlimit = Abs( Parser[ActorID]->MakeDouble_());
						with0->kvarLimitSet = true;
						if(!with0->kvarLimitNegSet)
							with0->PVSystemVars.Fkvarlimitneg = Abs( with0->PVSystemVars.Fkvarlimit);
					}
					break;
					case 	propDutyStart:
						with0->DutyStart = Parser[ActorID]->MakeDouble_();
					break;
					case 	propPpriority:
						with0->PVSystemVars.P_priority = InterpretYesNo(Param);
					break;  // set watt priority flag
					case 	propPFpriority:
						with0->PVSystemVars.PF_Priority = InterpretYesNo(Param);
					break;
					case 	propPminNoVars:
						with0->FpctPminNoVars = Abs( Parser[ActorID]->MakeDouble_());
					break;
					case 	propPminkvarLimit:
						with0->FpctPminkvarLimit = Abs( Parser[ActorID]->MakeDouble_());
					break;
					case 	propkvarLimitneg:
					{
						with0->PVSystemVars.Fkvarlimitneg = Abs( Parser[ActorID]->MakeDouble_());
						with0->kvarLimitNegSet = true;
					}
					break;
					case	propkVDC:
						with0->myDynVars.RatedVDC = Parser[ActorID]->MakeDouble_() * 1000;
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
					inherited::ClassEdit(ActivePVsystemObj, ParamPointer - NumPropsThisClass);
					break;
				}
				switch(iCase)
				{
					case 	1:
					SetNcondsForConnection();
					break;  // Force Reallocation of terminal info
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
					case 	propTYEARLY:
					with0->YearlyTShapeObj = ((TTShapeObj*) TShapeClass[ActorID]->Find(with0->YearlyTShape));
					break;
					case 	propTDAILY:
					with0->DailyTShapeObj = ((TTShapeObj*) TShapeClass[ActorID]->Find(with0->DailyTShape));
					break;
					case 	propTDUTY:
					with0->DutyTShapeObj = ((TTShapeObj*) TShapeClass[ActorID]->Find(with0->DutyTShape));
					break;
					case 	propInvEffCurve:
					with0->InverterCurveObj = ((TXYcurveObj*) XYCurveClass[ActorID]->Find(with0->InverterCurve));
					break;
					case 	propP_T_Curve:
					with0->Power_TempCurveObj = ((TXYcurveObj*) XYCurveClass[ActorID]->Find(with0->Power_TempCurve));
					break;
					case 	propDEBUGTRACE:
					if(with0->DebugTrace)   // Init trace file
					{
						int stop = 0;
						AssignFile(with0->Tracefile, GetOutputDirectory() + "STOR_" + with0->get_Name() + ".CSV");
						Rewrite(with0->Tracefile);
						IOResultToException();
						Write(with0->Tracefile, "t, Iteration, LoadMultiplier, Mode, LoadModel, PVSystemModel,  Qnominalperphase, Pnominalperphase, CurrentType");
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
						Write(with0->Tracefile, ",Vthev, Theta");
						WriteLn(with0->Tracefile);
						CloseFile(with0->Tracefile);
					}
					break;
					case	propDynEq:
					{
						with0->DynamicEqObj = (TDynamicExpObj*)TDynamicExpClass[ActorID]->Find(with0->DynamicEq);
						if (ASSIGNED(with0->DynamicEqObj))
						{
							with0->DynamicEqVals.resize(with0->DynamicEqObj->get_FNumVars());
							for (int idx = 0; idx < with0->DynamicEqVals.size(); idx++)
								with0->DynamicEqVals[idx].resize(2);
						}
					}
					break;
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
// Copy over essential properties from other object

int TPVSystem::MakeLike(const String OtherPVsystemObjName)
{
	int result = 0;
	TPVsystemObj* OtherPVSystemObj = nullptr;
	int i = 0;
	result = 0;
    /*See If we can find this line name in the present collection*/
	OtherPVSystemObj = ((TPVsystemObj*) Find(OtherPVsystemObjName));
	if(OtherPVSystemObj != nullptr)
	{
		/*# with ActivePVsystemObj do */
		{
			auto with0 = ActivePVsystemObj;
			int stop = 0;
			if(with0->Fnphases != OtherPVSystemObj->Fnphases)
			{
				with0->Set_NPhases(OtherPVSystemObj->Fnphases);
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->PVSystemVars.kVPVSystemBase = OtherPVSystemObj->PVSystemVars.kVPVSystemBase;
			with0->VBase = OtherPVSystemObj->VBase;
			with0->Vminpu = OtherPVSystemObj->Vminpu;
			with0->Vmaxpu = OtherPVSystemObj->Vmaxpu;
			with0->VBaseMin = OtherPVSystemObj->VBaseMin;
			with0->VBaseMax = OtherPVSystemObj->VBaseMax;
			with0->kW_out = OtherPVSystemObj->kW_out;
			with0->kvar_out = OtherPVSystemObj->kvar_out;
			with0->Pnominalperphase = OtherPVSystemObj->Pnominalperphase;
			with0->PFNominal = OtherPVSystemObj->PFNominal;
			with0->Qnominalperphase = OtherPVSystemObj->Qnominalperphase;
			with0->Connection = OtherPVSystemObj->Connection;
			with0->YearlyShape = OtherPVSystemObj->YearlyShape;
			with0->YearlyShapeObj = OtherPVSystemObj->YearlyShapeObj;
			with0->DailyShape = OtherPVSystemObj->DailyShape;
			with0->DailyShapeObj = OtherPVSystemObj->DailyShapeObj;
			with0->DutyShape = OtherPVSystemObj->DutyShape;
			with0->DutyShapeObj = OtherPVSystemObj->DutyShapeObj;
			with0->DutyStart = OtherPVSystemObj->DutyStart;
			with0->YearlyTShape = OtherPVSystemObj->YearlyTShape;
			with0->YearlyTShapeObj = OtherPVSystemObj->YearlyTShapeObj;
			with0->DailyTShape = OtherPVSystemObj->DailyTShape;
			with0->DailyTShapeObj = OtherPVSystemObj->DailyTShapeObj;
			with0->DutyTShape = OtherPVSystemObj->DutyTShape;
			with0->DutyTShapeObj = OtherPVSystemObj->DutyTShapeObj;
			with0->InverterCurve = OtherPVSystemObj->InverterCurve;
			with0->InverterCurveObj = OtherPVSystemObj->InverterCurveObj;
			with0->Power_TempCurve = OtherPVSystemObj->Power_TempCurve;
			with0->Power_TempCurveObj = OtherPVSystemObj->Power_TempCurveObj;
			with0->FClass = OtherPVSystemObj->FClass;
			with0->VoltageModel = OtherPVSystemObj->VoltageModel;
			with0->PVSystemVars.FTemperature = OtherPVSystemObj->PVSystemVars.FTemperature;
			with0->PVSystemVars.FPmpp = OtherPVSystemObj->PVSystemVars.FPmpp;
			with0->FpctCutIn = OtherPVSystemObj->FpctCutIn;
			with0->FpctCutOut = OtherPVSystemObj->FpctCutOut;
			with0->FVarFollowInverter = OtherPVSystemObj->FVarFollowInverter;
			with0->PVSystemVars.Fkvarlimit = OtherPVSystemObj->PVSystemVars.Fkvarlimit;
			with0->PVSystemVars.Fkvarlimitneg = OtherPVSystemObj->PVSystemVars.Fkvarlimitneg;
			with0->FpctPminNoVars = OtherPVSystemObj->FpctPminNoVars;
			with0->FpctPminkvarLimit = OtherPVSystemObj->FpctPminkvarLimit;
			with0->kvarLimitSet = OtherPVSystemObj->kvarLimitSet;
			with0->kvarLimitNegSet = OtherPVSystemObj->kvarLimitNegSet;
			with0->PVSystemVars.FIrradiance = OtherPVSystemObj->PVSystemVars.FIrradiance;
			with0->PVSystemVars.FkVArating = OtherPVSystemObj->PVSystemVars.FkVArating;
			with0->pctR = OtherPVSystemObj->pctR;
			with0->pctX = OtherPVSystemObj->pctX;
			with0->RandomMult = OtherPVSystemObj->RandomMult;
			with0->FVWMode = OtherPVSystemObj->FVWMode;
			with0->FVVMode = OtherPVSystemObj->FVVMode;
			with0->FWPMode = OtherPVSystemObj->FWPMode;
			with0->FWVMode = OtherPVSystemObj->FWPMode;
			with0->FDRCMode = OtherPVSystemObj->FDRCMode;
			with0->FAVRMode = OtherPVSystemObj->FAVRMode;
			with0->UserModel->FName = OtherPVSystemObj->UserModel->FName;  // Connect to user written models
			with0->ForceBalanced = OtherPVSystemObj->ForceBalanced;
			with0->CurrentLimited = OtherPVSystemObj->CurrentLimited;
			ClassMakeLike(OtherPVSystemObj);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				(with0->FPropertyValue)[i - 1] = (OtherPVSystemObj->FPropertyValue)[i - 1];
			}
			result = 1;
		}
	}
	else
	DoSimpleMsg(String("Error in PVSystem MakeLike: \"") + OtherPVsystemObjName
	           + "\" Not Found.", 562);
	return result;
}

int TPVSystem::Init(int Handle, int ActorID)
{
	int result = 0;
	TPVsystemObj* P = nullptr;
	if(Handle == 0)  // init all
	{
		P = (TPVsystemObj*) ElementList.Get_First();
		while((P != nullptr))
		{
			P->Randomize(0);
			P = (TPVsystemObj*) ElementList.Get_Next();
		}
	}
	else
	{
		Set_Active(Handle);
		P = ((TPVsystemObj*) GetActiveObj());
		P->Randomize(0);
	}
	DoSimpleMsg("Need to implement TPVSystem.Init", -1);
	result = 0;
	return result;
}  // Force all EnergyMeters in the circuit to reset

void TPVSystem::ResetRegistersAll()
{
	int Idx = 0;
	Idx = Get_First();
	while((Idx > 0))
	{
		((TPVsystemObj*) GetActiveObj())->ResetRegisters();
		Idx = Get_Next();
	}
}  // Force all active PV System energy meters  to take a sample

void TPVSystem::SampleAll(int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = ElementList.get_myNumList(), i = 1; i <= stop; i++)
	{
		/*# with TPVsystemObj(ElementList.Get(i)) do */
		{
			auto with0 = ((TPVsystemObj*) ElementList.Get(i));
			if(with0->Get_Enabled())
				with0->TakeSample(ActorID);
		}
	}
}

TPVsystemObj::TPVsystemObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			LastThevAngle(0.0),
			DebugTrace(false),
			PVSystemSolutionCount(0),
			PVSystemFundamental(0.0),
			PVsystemObjSwitchOpen(false),
			FirstSampleAfterReset(false),
			PFSpecified(false),
			kvarSpecified(false),
			ForceBalanced(false),
			CurrentLimited(false),
			kvar_out(0.0),
			kW_out(0.0),
			kvarRequested(0.0),
			kWRequested(0.0),
			FpctCutIn(0.0),
			FpctCutOut(0.0),
			FVarFollowInverter(false),
			CutInkW(0.0),
			CutOutkW(0.0),
			FInverterON(false),
			pctR(0.0),
			pctX(0.0),
			OpenPVSystemSolutionCount(0),
			Pnominalperphase(0.0),
			Qnominalperphase(0.0),
			RandomMult(0.0),
			Reg_Hours(0),
			Reg_kvarh(0),
			Reg_kWh(0),
			Reg_MaxkVA(0),
			Reg_MaxkW(0),
			Reg_Price(0),
			TShapeValue(0.0),
			UserModel(nullptr),
			varBase(0.0),
			VBase(0.0),
			VBaseMax(0.0),
			VBaseMin(0.0),
			Vmaxpu(0.0),
			Vminpu(0.0),
			YPrimOpenCond(nullptr),
			FVWMode(false),
			FVWYAxis(0),
			Connection(0),
			DailyShapeObj(nullptr),
			DutyShapeObj(nullptr),
			DutyStart(0.0),
			YearlyShapeObj(nullptr),
			DailyTShapeObj(nullptr),
			DutyTShapeObj(nullptr),
			YearlyTShapeObj(nullptr),
			InverterCurveObj(nullptr),
			Power_TempCurveObj(nullptr),
			FClass(0),
			VoltageModel(0),
			PFNominal(0.0)
{
	int i;

	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType; // + PVSystem_ELEMENT;  // In both PCelement and PVSystemelement list
	Set_NPhases(3);
	Fnconds = 4;  // defaults to wye
	Yorder = 0;  // To trigger an initial allocation
	Set_NTerms(1);  // forces allocations
	YearlyShape = "";
	YearlyShapeObj = nullptr;  // If YearlyShapeobj = nil Then the Irradiance alway stays nominal
	DailyShape = "";
	DailyShapeObj = nullptr;  // If DaillyShapeobj = nil Then the Irradiance alway stays nominal
	DutyShape = "";
	DutyShapeObj = nullptr;  // If DutyShapeobj = nil Then the Irradiance alway stays nominal
	DutyStart = 0.0;
	YearlyTShape = "";
	YearlyTShapeObj = nullptr;  // If YearlyShapeobj = nil Then the Temperature always stays nominal
	DailyTShape = "";
	DailyTShapeObj = nullptr;  // If DaillyShapeobj = nil Then the Temperature always stays nominal
	DutyTShape = "";
	DutyTShapeObj = nullptr;  // If DutyShapeobj = nil Then the Temperature always stays nominal
	InverterCurveObj = nullptr;
	Power_TempCurveObj = nullptr;
	InverterCurve = "";
	Power_TempCurve = "";
	Connection = 0;    // Wye (star, L-N)
	VoltageModel = 1;  /*Typical fixed kW negative load*/
	FClass = 1;
	PVSystemSolutionCount = -1;  // For keep track of the present solution in Injcurrent calcs
	OpenPVSystemSolutionCount = -1;
	YPrimOpenCond = nullptr;
	PVSystemVars.kVPVSystemBase = 12.47;
	VBase = 7200.0;
	Vminpu = 0.90;
	Vmaxpu = 1.10;
	VBaseMin = Vminpu * VBase;
	VBaseMax = Vmaxpu * VBase;
	Yorder = Fnterms * Fnconds;
	RandomMult = 1.0;
	Set_Varmode(VARMODEPF);
	FInverterON = true; // start with inverterON
	FVarFollowInverter = false;
	ForceBalanced = false;
	CurrentLimited = false;
	NumStateVars = NumPVSystemVariables;
	/*# with PVSystemVars do */
	{
		auto& with0				= PVSystemVars;
		auto& with1				= myDynVars;
		with0.FTemperature		= 25.0;
		with0.FIrradiance		= 1.0;  // kW/sq-m
		with0.FkVArating		= 500.0;
		with0.FPmpp				= 500.0;
		with0.FpuPmpp			= 1.0;    // full on
		with0.Vreg				= 9999;
		with0.Vavg				= 9999;
		with0.VVOperation		= 9999;
		with0.VWOperation		= 9999;
		with0.DRCOperation		= 9999;
		with0.VVDRCOperation	= 9999;
		with0.WPOperation		= 9999;
		with0.WVOperation		= 9999;
        //         kW_out_desired  :=9999;
		with0.Fkvarlimit		= with0.FkVArating;
		with0.Fkvarlimitneg		= with0.FkVArating;
		with0.P_priority		= false;    // This is a change from older versions
		with0.PF_Priority		= false;
		with1.RatedVDC			= 8000;
		with1.SMThreshold		= 80;
		with1.SafeMode			= false;
		with1.Kp				= 0.00001;
		with1.ILimit			= -1; // No Amps limit
		with1.IComp				= 0;
		with1.VError			= 0.8;
	}
	FpctCutIn = 20.0;
	FpctCutOut = 20.0;
	FpctPminNoVars = -1.0;
	FpctPminkvarLimit = -1.0;
	Fpf_wp_nominal = 1.0;
    /*Output rating stuff*/
	kW_out = 500.0;
	kvar_out = 0.0;
	PFNominal = 1.0;
	pctR = 50.0;
	pctX = 0.0;
	PublicDataStruct = &PVSystemVars;
	PublicDataSize = sizeof(TPVSystemVars);
	kvarLimitSet = false;
	kvarLimitNegSet = false;
	UserModel = new TPVsystemUserModel();
	Reg_kWh = 1 - 1;
	Reg_kvarh = 2 - 1;
	Reg_MaxkW = 3 - 1;
	Reg_MaxkVA = 4 - 1;
	Reg_Hours = 5 - 1;
	Reg_Price = 6 - 1;
	DebugTrace = false;
	PVsystemObjSwitchOpen = false;
	Spectrum = "";  // override base class
	SpectrumObj = nullptr;
	FVWMode = false;
	FVVMode = false;
	FWVMode = false;
	FWPMode = false;
	FDRCMode = false;
	FAVRMode = false;
	InitPropertyValues(0);
	RecalcElementData(ActiveActor);

	for (i = 0; i < NumPVSystemRegisters; i++)
	{
		Registers[i] = 0.0;
		Derivatives[i] = 0.0;
	}
}
// Define default values for the properties

void TPVsystemObj::InitPropertyValues(int ArrayOffset)
{
	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
		Set_PropertyValue(1,"3");         //'phases';
		Set_PropertyValue(2,GetBus(1));   //'bus1';
		Set_PropertyValue(propKV,Format("%-g", with0.kVPVSystemBase));
		Set_PropertyValue(propIrradiance,Format("%-g", with0.FIrradiance));
		Set_PropertyValue(propPF,Format("%-g", PFNominal));
		Set_PropertyValue(propMODEL,"1");
		Set_PropertyValue(propYEARLY,"");
		Set_PropertyValue(propDAILY,"");
		Set_PropertyValue(propDUTY,"");
		Set_PropertyValue(propTYEARLY,"");
		Set_PropertyValue(propTDAILY,"");
		Set_PropertyValue(propTDUTY,"");
		Set_PropertyValue(PropConnection,"wye");
		Set_PropertyValue(propKVAR,Format("%-g", Get_Presentkvar()));
		Set_PropertyValue(propPCTR,Format("%-g", pctR));
		Set_PropertyValue(propPCTX,Format("%-g", pctX));
		Set_PropertyValue(propCLASS,"1"); //'class'
		Set_PropertyValue(propInvEffCurve,"");
		Set_PropertyValue(propTemp,Format("%-g", with0.FTemperature));
		Set_PropertyValue(propPmpp,Format("%-g", with0.FPmpp));
		Set_PropertyValue(propP_T_Curve,"");
		Set_PropertyValue(propCutin,"20");
		Set_PropertyValue(propCutout,"20");
		Set_PropertyValue(propVarFollowInverter,"NO");
		Set_PropertyValue(propVMINPU,"0.90");
		Set_PropertyValue(propVMAXPU,"1.10");
		Set_PropertyValue(propKVA,Format("%-g", with0.FkVArating));
		Set_PropertyValue(propUSERMODEL,"");  // Usermodel
		Set_PropertyValue(propUSERDATA,"");  // Userdata
		Set_PropertyValue(propDEBUGTRACE,"NO");
		Set_PropertyValue(proppctPmpp,"100");
		Set_PropertyValue(propBalanced,"NO");
		Set_PropertyValue(propLimited,"NO");
		Set_PropertyValue(propkvarLimit,Format("%-g", with0.Fkvarlimit));
		Set_PropertyValue(propkvarLimitneg,Format("%-g", PVSystemVars.Fkvarlimitneg));
		Set_PropertyValue(propPpriority,"NO");
		Set_PropertyValue(propPFpriority,"NO");
        Set_PropertyValue(propkVDC,"8000");
        Set_PropertyValue(propkp,"0.00001");
        Set_PropertyValue(propCtrlTol,"5");
        Set_PropertyValue(propSMT,"80");
        Set_PropertyValue(propSM,"NO");
        Set_PropertyValue(propGFM,"GFL");

	}
	inherited::InitPropertyValues(NumPropsThisClass);
}

String TPVsystemObj::GetPropertyValue(int Index)
{
	String result = "";

	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
		switch(Index)
		{
			case 	propKV:
			result = Format("%.6g", with0.kVPVSystemBase);
			break;
			case 	propIrradiance:
			result = Format("%.6g", with0.FIrradiance);
			break;
			case 	propPF:
			result = Format("%.6g", PFNominal);
			break;
			case 	propMODEL:
			result = Format("%d", VoltageModel);
			break;
			case 	propYEARLY:
			result = YearlyShape;
			break;
			case 	propDAILY:
			result = DailyShape;
			break;
			case 	propDUTY:
			result = DutyShape;
			break;
			case 	propTYEARLY:
			result = YearlyTShape;
			break;
			case 	propTDAILY:
			result = DailyTShape;
			break;
			case 	propTDUTY:
			result = DutyTShape;
			break;
            case    PropConnection:
			if (Connection == 0)
                result = "wye";
			else
				result = "delta";
			break;
			case 	propKVAR:
			result = Format("%.6g", kvar_out);
			break;
			case 	propPCTR:
			result = Format("%.6g", pctR);
			break;
			case 	propPCTX:
			result = Format("%.6g", pctX);
			break;
        /*propCLASS      = 17;*/
			case 	propInvEffCurve:
			result = InverterCurve;
			break;
			case 	propTemp:
			result = Format("%.6g", with0.FTemperature);
			break;
			case 	propPmpp:
			result = Format("%.6g", with0.FPmpp);
			break;
			case 	propP_T_Curve:
			result = Power_TempCurve;
			break;
			case 	propCutin:
			result = Format("%.6g", FpctCutIn);
			break;
			case 	propCutout:
			result = Format("%.6g", FpctCutOut);
			break;
			case 	propVarFollowInverter:
			if(FVarFollowInverter)
				result = "Yes";
			else
				result = "No";
			break;
			case 	propPminNoVars:
			result = Format("%.6g", FpctPminNoVars);
			break;
			case 	propPminkvarLimit:
			result = Format("%.6g", FpctPminkvarLimit);
			break;
			case 	propVMINPU:
			result = Format("%.6g", Vminpu);
			break;
			case 	propVMAXPU:
			result = Format("%.6g", Vmaxpu);
			break;
			case 	propKVA:
			result = Format("%.6g", with0.FkVArating);
			break;
			case 	propUSERMODEL:
			result = UserModel->FName;
			break;
			case 	propUSERDATA:
			result = String("(") + inherited::GetPropertyValue(Index) + ")";
			break;
			case 	proppctPmpp:
			result = Format("%.6g", with0.FpuPmpp * 100.0);
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
			case 	propkvarLimit:
			result = Format("%.6g", with0.Fkvarlimit);
			break;
			case 	propkvarLimitneg:
			result = Format("%.6g", PVSystemVars.Fkvarlimitneg);
			break;
			case 	propDutyStart:
			result = Format("%.6g", DutyStart);
			break;
			case 	propkVDC:
				result = Format("%.6g", myDynVars.RatedVDC / 1000);
				break;
			case 	propkp:
				result = Format("%.10g", myDynVars.Kp * 1000);
				break;
			case 	propCtrlTol:
				result = Format("%.6g", myDynVars.CtrlTol * 100);
				break;
			case 	propSMT:
				result = Format("%.6g", myDynVars.SMThreshold);
				break;
			case 	propSM:
				result = (myDynVars.SafeMode) ? "Yes" : "No";
				break;
			case 	propDynEq:
				result = DynamicEq;
				break;
			case 	propDynOut:
				result = GetDynOutputStr();
				break;
			case 	propGFM:
				result = (GFM_Mode) ? "GFM" : "GFL";
				break;
        /*propDEBUGTRACE = 33;*/  // take the generic handler
			default:
			result = inherited::GetPropertyValue(Index);
			break;
		}
	}
	return result;
}

TPVsystemObj::~TPVsystemObj()
{
	delete YPrimOpenCond;
	delete UserModel;
	// inherited::Destroy();
}


void TPVsystemObj::Randomize(int Opt)
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

void TPVsystemObj::CalcDailyMult(double hr)
{
	if(DailyShapeObj != nullptr)
	{
		ShapeFactor = DailyShapeObj->GetMult(hr);
	}
	else
	ShapeFactor = CDoubleOne;  // Default to no  variation
}

void TPVsystemObj::CalcDailyTemperature(double hr)
{
	if(DailyTShapeObj != nullptr)
	{
		TShapeValue = DailyTShapeObj->GetTemperature(hr);
	}
	else
	TShapeValue = PVSystemVars.FTemperature;  // Default to no  variation
}

void TPVsystemObj::CalcDutyMult(double hr)
{
	if(DutyShapeObj != nullptr)
	{
		ShapeFactor = DutyShapeObj->GetMult(hr + DutyStart);
	}
	else
	CalcDailyMult(hr);  // Default to Daily Mult If no duty curve specified
}

void TPVsystemObj::CalcDutyTemperature(double hr)
{
	if(DutyTShapeObj != nullptr)
	{
		TShapeValue = DutyTShapeObj->GetTemperature(hr);
	}
	else
	CalcDailyTemperature(hr);  // Default to Daily Mult If no duty curve specified
}

void TPVsystemObj::CalcYearlyMult(double hr)
{
	if(YearlyShapeObj != nullptr)
	{
		ShapeFactor = YearlyShapeObj->GetMult(hr + DutyStart);
	}
	else
	CalcDailyMult(hr);  // Defaults to Daily curve
}

void TPVsystemObj::CalcYearlyTemperature(double hr)
{
	if(YearlyTShapeObj != nullptr)
	{
		TShapeValue = YearlyTShapeObj->GetTemperature(hr);
	}
	else
	CalcDailyTemperature(hr);  // Defaults to Daily curve
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
// Required for operation in GFM mode
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
void TPVsystemObj::GetCurrents(pComplexArray Curr, int ActorID)
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
bool TPVsystemObj::CheckOLInverter(int ActorID)
{
	complex myCurr = cmplx(0, 0);
	double	MaxAmps = 0,
			PhaseAmps = 0;
	int		i = 0;
	bool	result = false;
	// Check if reaching saturation point in GFM
	if (GFM_Mode)
	{
		ComputePanelPower();
		MaxAmps = ((PVSystemVars.FkVArating * 1000) / Get_NPhases()) / VBase;
		ComputeIterminal(ActorID);
		for (i = 1; i <= Get_NPhases(); i++)
		{
			myCurr = Iterminal[i - 1];
			PhaseAmps = cabs(myCurr);
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
// Returns True if any of the inverter phases has reached the current limit
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
bool TPVsystemObj::CheckAmpsLimit(int ActorID)
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
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
void TPVsystemObj::DoGFM_Mode(int ActorID)
{
	int j = 0,
		i = 0;
	double	myW = 0,
		ZSys = 0;

	myDynVars.BaseV			= VBase;
	myDynVars.Discharging	= true;

	auto& with0 = ActiveCircuit[ActorID]->Solution;

    /*Initial conditions just in case*/
    if (myDynVars.Vgrid.size() < Fnphases) myDynVars.Vgrid.resize(Fnphases);

    for (i = 1; i <= Fnphases; i++) 
		myDynVars.Vgrid[i - 1] =  ctopolar(with0->NodeV[NodeRef[i - 1]]);
	if (myDynVars.IComp > 0)
	{
		ZSys = (2 * (VBase * myDynVars.ILimit)) - myDynVars.IComp;
		myDynVars.BaseV = (ZSys / myDynVars.ILimit) * myDynVars.VError;
	}
	myDynVars.CalcGFMVoltage(ActorID, Get_NPhases(), &(Vterminal[0]));	// needs to be referenced as 0
	YPrim->MVmult(InjCurrent, &(Vterminal[0]));						// needs to be referenced as 1

	set_ITerminalUpdated(false, ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TPVsystemObj::RecalcElementData(int ActorID)
{
	VBaseMin = Vminpu * VBase;
	VBaseMax = Vmaxpu * VBase;
	varBase = 1000.0 * kvar_out / Fnphases;
	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
        // values in ohms for thevenin equivalents
		with0.RThev = pctR * 0.01 * Sqr(Get_PresentkV()) / with0.FkVArating * 1000.0;
		with0.XThev = pctX * 0.01 * Sqr(Get_PresentkV()) / with0.FkVArating * 1000.0;
		CutInkW = FpctCutIn * with0.FkVArating / 100.0;
		CutOutkW = FpctCutOut * with0.FkVArating / 100.0;
		if(FpctPminNoVars <= 0)
			PminNoVars = -1;
		else
			PminNoVars = FpctPminNoVars * with0.FPmpp / 100.0;
		if(FpctPminkvarLimit <= 0)
			PminkvarLimit = -1;
		else
			PminkvarLimit = FpctPminkvarLimit * with0.FPmpp / 100.0;
	}
	SetNominalPVSystemOuput(ActorID);
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
	if(YearlyTShapeObj == nullptr)
	{
		if(YearlyTShape.size() > 0)
			DoSimpleMsg(String("WARNING! Yearly temperature shape: \"") + YearlyTShape
	           + "\" Not Found.", 5631);
	}
	if(DailyTShapeObj == nullptr)
	{
		if(DailyTShape.size() > 0)
			DoSimpleMsg(String("WARNING! Daily temperature shape: \"") + DailyTShape
	           + "\" Not Found.", 5641);
	}
	if(DutyTShapeObj == nullptr)
	{
		if(DutyTShape.size() > 0)
			DoSimpleMsg(String("WARNING! Duty temperature shape: \"") + DutyTShape
	           + "\" Not Found.", 5651);
	}
	if(Spectrum.size() > 0)
	{
		SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
		if(SpectrumObj == nullptr)
			DoSimpleMsg(String("ERROR! Spectrum \"") + Spectrum + "\" Not Found.", 566);
	}
	else
	SpectrumObj = nullptr;
    // Initialize to Zero - defaults to PQ PVSystem element
    // Solution object will reset after circuit modifications
	InjCurrent = (pComplexArray) realloc(InjCurrent, sizeof(complex) * Yorder);
    /*Update any user-written models*/
	if(UserModel->Get_Exists())
		UserModel->FUpdateModel();
}

void TPVsystemObj::SetNominalPVSystemOuput(int ActorID)
{
	ShapeFactor = CDoubleOne;  // init here; changed by curve routine
	TShapeValue = PVSystemVars.FTemperature; // init here; changed by curve routine
    // Check to make sure the PVSystem element is ON
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
	{
		
		auto with1 = ActiveCircuit[ActorID]->Solution;
		if(!(with1->IsDynamicModel || with1->IsHarmonicModel))     // Leave PVSystem element in whatever state it was prior to entering Dynamic mode

            // Check dispatch to see what state the PVSystem element should be in
		{
			/*# with Solution do */
			{
				auto with2 = ActiveCircuit[ActorID]->Solution;
				switch(with2->Get_SolMode())
				{
					case 	SNAPSHOT:
					;
					break; /*Just solve for the present kW, kvar*/  // Don't check for state change
					case 	DAILYMODE:
					{
						CalcDailyMult(with2->DynaVars.dblHour);
						CalcDailyTemperature(with2->DynaVars.dblHour);
					}
					break;
					case 	YEARLYMODE:
					{
						CalcYearlyMult(with2->DynaVars.dblHour);
						CalcYearlyTemperature(with2->DynaVars.dblHour);
					}
					break;
             /*
                MONTECARLO1,
                MONTEFAULT,
                FAULTSTUDY,
                DYNAMICMODE:   ; // {do nothing yet}
             */
                         // This mode allows use of one class of load shape
					case 	GENERALTIME:
					{
						switch(ActiveCircuit[ActiveActor]->ActiveLoadShapeClass)
						{
							case 	USEDAILY:
							{
								CalcDailyMult(with2->DynaVars.dblHour);
								CalcDailyTemperature(with2->DynaVars.dblHour);
							}
							break;
							case 	USEYEARLY:
							{
								CalcYearlyMult(with2->DynaVars.dblHour);
								CalcYearlyTemperature(with2->DynaVars.dblHour);
							}
							break;
							case 	USEDUTY:
							{
								CalcDutyMult(with2->DynaVars.dblHour);
								CalcDutyTemperature(with2->DynaVars.dblHour);
							}
							break;
							default:     // default to 1 + j1 if not known
							ShapeFactor = CDoubleOne;
							break;
						}
					}
					break;
                // Assume Daily curve, If any, for the following
					case 	MONTECARLO2: case MONTECARLO3: case LOADDURATION1: case LOADDURATION2:
					{
						CalcDailyMult(with2->DynaVars.dblHour);
						CalcDailyTemperature(with2->DynaVars.dblHour);
					}
					break;
					case 	PEAKDAY:
					{
						CalcDailyMult(with2->DynaVars.dblHour);
						CalcDailyTemperature(with2->DynaVars.dblHour);
					}
					break;
					case 	DUTYCYCLE:
					{
						CalcDutyMult(with2->DynaVars.dblHour);
						CalcDutyTemperature(with2->DynaVars.dblHour);
					}
					break;
                /*AUTOADDFLAG:  ; */
					default:
					  ;
					break;
				}
			}
			ComputekWkvar();
			Pnominalperphase = 1000.0 * kW_out / Fnphases;
			Qnominalperphase = 1000.0 * kvar_out / Fnphases;
			switch(VoltageModel)
			{
				case 	3:
              //****  Fix this when user model gets connected in
 // YEQ := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim
				;
				break;
				default:
				Yeq = cdivreal(cmplx(Pnominalperphase, -Qnominalperphase), Sqr(VBase));   // Vbase must be L-N for 3-phase
				if(Vminpu != 0.0)  // at 95% voltage
					YEQ_Min = cdivreal(Yeq, Sqr(Vminpu));
				else
					YEQ_Min = Yeq; // Always a constant Z model
				if(Vmaxpu != 0.0)   // at 105% voltage
					YEQ_Max = cdivreal(Yeq, Sqr(Vmaxpu));
				else
					YEQ_Max = Yeq;
            /* Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
            */
				/*# with PVSystemVars do */
				{
					auto& with3 = PVSystemVars;
					PhaseCurrentLimit = cdivreal(cmplx(Pnominalperphase, Qnominalperphase), VBaseMin);
					with3.MaxDynPhaseCurrent = cabs(PhaseCurrentLimit);
				}
				break;
			}
           /* When we leave here, all the YEQ's are in L-N values*/
		}  /*If  NOT (IsDynamicModel or IsHarmonicModel)*/
	}  /*With ActiveCircuit[ActiveActor]*/
}

// ===========================================================================================

void TPVsystemObj::CalcYPrimMatrix(TcMatrix* Ymatrix, int ActorID)
{
	complex Y = {};
	complex Yij = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;
	FYprimFreq = ActiveCircuit[ActiveActor]->Solution->get_FFrequency();
	FreqMultiplier = FYprimFreq / BaseFrequency;
	/*# with ActiveCircuit[ActiveActor].Solution do */
	{
		auto with0 = ActiveCircuit[ActiveActor]->Solution;
		if(with0->IsHarmonicModel)
          /*YEQ is computed from %R and %X -- inverse of Rthev + j Xthev*/
		{
			int stop = 0;
			Y = Yeq;   // L-N value computed in initial condition routines
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
  //  Regular power flow PVSystem element model
		          /*YEQ is always expected as the equivalent line-neutral admittance*/
		{
			if (!GFM_Mode)
			{
				Y = cnegate(Yeq);   // negate for generation    YEQ is L-N quantity
				// ****** Need to modify the base admittance for real harmonics calcs
				Y.im = Y.im / FreqMultiplier;
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
			else
			{
				// Otherwise, the inverter is in GFM control modem calculation changes
				myDynVars.RatedkVLL		= Get_PresentkV();
				myDynVars.mKVARating	= PVSystemVars.FkVArating;
				myDynVars.CalcGFMYprim(ActorID, Get_NPhases(), Ymatrix);
			}
		}
	}  /*ELSE IF Solution.mode*/
}

void TPVsystemObj::ComputeInverterPower()
{
	double kVA_Gen = 0.0;
	double Qramp_limit = 0.0;
	double TempPF = 0.0;
	double CutOutkWAC = 0.0;
	double CutInkWAC = 0.0;
    // Reset CurrentkvarLimit to kvarLimit
	CurrentkvarLimit = PVSystemVars.Fkvarlimit;
	CurrentkvarLimitNeg = PVSystemVars.Fkvarlimitneg;
	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
		with0.EffFactor = 1.0;
		kW_out = 0.0;
		if(ASSIGNED(InverterCurveObj))
		{
			CutOutkWAC = CutOutkW * InverterCurveObj->GetYValue_(Abs(CutOutkW) / with0.FkVArating);
			CutInkWAC = CutInkW * InverterCurveObj->GetYValue_(Abs(CutInkW) / with0.FkVArating);
		}
		else
  // Assume Ideal Inverter
		{
			CutOutkWAC = CutOutkW;
			CutInkWAC = CutInkW;
		}

        // Determine state of the inverter
		if(FInverterON)
		{
			if(with0.PanelkW < CutOutkW)
			{
				FInverterON = false;
			}
		}
		else
		{
			if(with0.PanelkW >= CutInkW)
			{
				FInverterON = true;
			}
		}
        // set inverter output. Defaults to 100% of the panelkW if no efficiency curve spec'd
		if(FInverterON)
		{
			if(ASSIGNED(InverterCurveObj))
				with0.EffFactor = InverterCurveObj->GetYValue_(with0.PanelkW / with0.FkVArating);  // pu eff vs pu power
			kWOut_Calc();
		}
		else
		{
			kW_out = 0.0;
		}
		if(Abs(kW_out) < PminNoVars)
		{
			kvar_out = 0.0;  // Check minimum P for Q gen/absorption. if PminNoVars is disabled (-1), this will always be false
			CurrentkvarLimit = 0;
			CurrentkvarLimitNeg = 0.0;  // Set current limit to be used by InvControl's Check_Qlimits procedure.
		}
		else
		{
			if(Get_Varmode() == VARMODEPF)
			{
				if(PFNominal == 1.0)
					kvar_out = 0.0;
				else
				{
					kvar_out = kW_out * sqrt(1.0L / Sqr(PFNominal) - 1.0L) * Sign(PFNominal);
                // Check limits
					if(Abs(kW_out) < PminkvarLimit) // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.

                    // straight line starts at max(PminNoVars, CutOutkWAC)
                    // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
					{
						if(Abs(kW_out) >= max(PminNoVars, CutOutkWAC))
						{
							if(kvar_out > 0.0)
							{
								Qramp_limit = with0.Fkvarlimit / PminkvarLimit * Abs(kW_out);   // generation limit
								CurrentkvarLimit = Qramp_limit;  // For use in InvControl
							}
							else
							{
								if(kvar_out < 0.0)
								{
									Qramp_limit = double(PVSystemVars.Fkvarlimitneg) / PminkvarLimit * Abs( kW_out);   // absorption limit
									CurrentkvarLimitNeg = Qramp_limit;  // For use in InvControl
								}
							}
							if(Abs( kvar_out) > Qramp_limit)
								kvar_out = Qramp_limit * Sign(kW_out) * Sign(PFNominal);
						}
					}
					else
					{
						if((Abs( kvar_out) > with0.Fkvarlimit) || (Abs( kvar_out) > PVSystemVars.Fkvarlimitneg))  // Other cases, check normal kvarLimit and kvarLimitNeg
						{
							if(kvar_out > 0.0)
								kvar_out = with0.Fkvarlimit * Sign(kW_out) * Sign(PFNominal);
							else
								kvar_out = PVSystemVars.Fkvarlimitneg * Sign(kW_out) * Sign(PFNominal);
							if(with0.PF_Priority) // Forces constant power factor when kvar limit is exceeded and PF Priority is true.
							{
								kW_out = kvar_out * sqrt(1.0 / (1.0L - Sqr(PFNominal)) - 1.0) * Sign(PFNominal);
							}
						}
					}
				}
			}
			else
     // kvar is specified

            // Check limits
			{
				if(Abs( kW_out) < PminkvarLimit) // straight line limit check. if PminkvarLimit is disabled (-1), this will always be false.

                  // straight line starts at max(PminNoVars, CutOutkWAC)
                  // if CutOut differs from CutIn, take cutout since it is assumed that CutOut <= CutIn always.
				{
					if(Abs( kW_out) >= max(PminNoVars, CutOutkWAC))
					{
						if(kvarRequested > 0.0)
						{
							Qramp_limit = with0.Fkvarlimit / PminkvarLimit * Abs( kW_out);   // generation limit
							CurrentkvarLimit = Qramp_limit;   // For use in InvControl
						}
						else
						{
							if(kvarRequested < 0.0)
							{
								Qramp_limit = double(PVSystemVars.Fkvarlimitneg) / PminkvarLimit * Abs( kW_out);   // absorption limit
								CurrentkvarLimitNeg = Qramp_limit;   // For use in InvControl
							}
						}
						if(Abs( kvarRequested) > Qramp_limit)
							kvar_out = Qramp_limit * Sign(kvarRequested);
						else
							kvar_out = kvarRequested;
					}
				}
				else
				{
					if(((kvarRequested > 0.0) && (Abs( kvarRequested) >= with0.Fkvarlimit)) || ((kvarRequested < 0.0) && (Abs( kvarRequested) >= PVSystemVars.Fkvarlimitneg)))
					{
						if(kvarRequested > 0.0)
							kvar_out = with0.Fkvarlimit * Sign(kvarRequested);
						else
							kvar_out = PVSystemVars.Fkvarlimitneg * Sign(kvarRequested);
						if((Get_Varmode() == VARMODEKVAR) && with0.PF_Priority && FWPMode)
						{
							kW_out = Abs( kvar_out) * sqrt(1.0 / (1.0L - Sqr(Fpf_wp_nominal)) - 1.0) * Sign(kW_out);
                // Forces constant power factor when kvar limit is exceeded and PF Priority is true. Temp PF is calculated based on kvarRequested
						}
						else
						{
							if(with0.PF_Priority && (!FVVMode || !FDRCMode || !FWVMode || !FAVRMode))
							{
								if(Abs( kvarRequested) > 0.0)
								{
									TempPF = cos(atan(Abs( (kvarRequested / kW_out))));
									kW_out = Abs( kvar_out) * sqrt(1.0 / (1.0L - Sqr(TempPF)) - 1.0) * Sign(kW_out);
								}
							}
						}
					}
					else
					kvar_out = kvarRequested;
				}
			}
		}
		if((FInverterON == false) && (FVarFollowInverter == true))
			kvar_out = 0.0;
        // Limit kvar and kW so that kVA of inverter is not exceeded
		kVA_Gen = sqrt(Sqr(kW_out) + Sqr(kvar_out));
		if(kVA_Gen > with0.FkVArating)
		{
			if((Get_Varmode() == VARMODEPF) && with0.PF_Priority)
              // Operates under constant power factor when kVA rating is exceeded. PF must be specified and PFPriority must be TRUE
			{
				kW_out = with0.FkVArating * Abs( PFNominal);
				kvar_out = with0.FkVArating * sqrt(1.0L - Sqr(PFNominal)) * Sign(PFNominal);
			}
			else
			{
				if((Get_Varmode() == VARMODEKVAR) && with0.PF_Priority && FWPMode)
				{
					kW_out = with0.FkVArating * Abs(Fpf_wp_nominal) * Sign(kW_out);
					kvar_out = with0.FkVArating * Abs( sin(acos(Fpf_wp_nominal))) * Sign(kvarRequested);
				}
				else
				{
					if((Get_Varmode() == VARMODEKVAR) && with0.PF_Priority && (!FVVMode || !FDRCMode || !FWVMode || !FAVRMode))
              // Operates under constant power factor (PF implicitly calculated based on kw and kvar)
					{
						if(Abs( kvar_out) == with0.Fkvarlimit)   // for handling cases when kvar limit and inverter's kVA limit are exceeded
						{
							kW_out = with0.FkVArating * Abs( TempPF) * Sign(kW_out);
						}
						else
						{
							kW_out = with0.FkVArating * Abs( cos(atan(kvarRequested / kW_out))) * Sign(kW_out);
						}
						kvar_out = with0.FkVArating * Abs( sin(acos(kW_out / with0.FkVArating))) * Sign(kvarRequested);
					}
					else
					{
						if(with0.P_priority)  // back off the kvar
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
						kW_out = sqrt(Sqr(with0.FkVArating) - Sqr(kvar_out)) * Sign(kW_out);
					}
				}
			}
		}
		if((FInverterON == false) && (FVarFollowInverter == true))
			kvar_out = 0.0;
	}  /*With PVSystemVars*/
}

void TPVsystemObj::ComputekWkvar()
{
	ComputePanelPower();   // apply irradiance
	ComputeInverterPower(); // apply inverter eff after checking for cutin/cutout
}
// ===========================================================================================

void TPVsystemObj::ComputePanelPower()
{
	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
		with0.TempFactor = 1.0;
		if(ASSIGNED(Power_TempCurveObj))
		{
			with0.TempFactor = Power_TempCurveObj->GetYValue_(TShapeValue);  // pu Set_Pmpp vs T (actual)
		}
		with0.PanelkW = with0.FIrradiance * ShapeFactor.re * with0.FPmpp * with0.TempFactor;
	}
}

void TPVsystemObj::CalcYPrim(int ActorID)
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
	SetNominalPVSystemOuput(ActorID);
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
// ===========================================================================================

 /*Add the current into the proper location according to connection*/
 /*Reverse of similar routine in load  (Cnegates are switched)*/

void TPVsystemObj::StickCurrInTerminalArray(pComplexArray TermArray, const complex& Curr, int i)
{
	int j = 0;
	switch(Connection)
	{
		case 	0:  //Wye
		{
			caccum((TermArray)[i - 1], Curr);
			caccum((TermArray)[Fnconds - 1], cnegate(Curr)); // Neutral
		}
		break; //DELTA
		case 	1:
		{
			caccum((TermArray)[i - 1], Curr);
			j = i + 1;
			if(j > Fnconds)
				j = 1;
			caccum((TermArray)[j - 1], cnegate(Curr));
		}
		break;
		default:
		  ;
		break;
	}
}
// ===========================================================================================

void TPVsystemObj::WriteTraceRecord(const String s)
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
			ActiveCircuit[ActiveActor]->Solution->DynaVars.T, ActiveCircuit[ActiveActor]->Solution->Iteration, ActiveCircuit[ActiveActor]->get_FLoadMultiplier())); 
			Write(Tracefile, GetSolutionModeID()); Write(Tracefile, ", "); Write(Tracefile, GetLoadModel()); Write(Tracefile, ", "); 
			Write(Tracefile, VoltageModel, 0); Write(Tracefile, ", "); Write(Tracefile, (Qnominalperphase * 3.0 / 1.0e6), 8, 2); 
			Write(Tracefile, ", "); Write(Tracefile, (Pnominalperphase * 3.0 / 1.0e6), 8, 2); Write(Tracefile, ", "); 
			Write(Tracefile, s); Write(Tracefile, ", "); }
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
			WriteLn(Tracefile);
			CloseFile(Tracefile);
		}
	}
	catch (...)
	{
	}
}

// ===========================================================================================

/*Compute total terminal current for Constant PQ*/

void TPVsystemObj::DoConstantPQPVsystemObj(int ActorID)
{
	int i = 0;
	complex PhaseCurr = {};
	complex DeltaCurr = {};
	complex VLN = {};
	complex VLL = {};
	double VmagLN = 0.0;
	double VmagLL = 0.0;
	complex V012[4] = { CZero, CZero, CZero, CZero, };   // Sequence voltages

	auto with0 = ActiveCircuit[ActorID]->Solution;

    //Treat this just like the Load model
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	ZeroITerminal();
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
				if(CurrentLimited)
                    /*Current-Limited Model*/
				{
					PhaseCurr = conjg(cdiv(cmplx(Pnominalperphase, Qnominalperphase), VLN));
					if(cabs(PhaseCurr) > PVSystemVars.MaxDynPhaseCurrent)
						PhaseCurr = conjg(cdiv(PhaseCurrentLimit, cdivreal(VLN, VmagLN)));
				}
				else

                   /*The usual model*/
				{
					if(VmagLN <= VBaseMin)  // Below Vminpu use an impedance model
						PhaseCurr = cmul(YEQ_Min, VLN);
					else
					{
						if(VmagLN > VBaseMax)  // above Vmaxpu use an impedance model
							PhaseCurr = cmul(YEQ_Max, VLN);
						else
							PhaseCurr = conjg(cdiv(cmplx(Pnominalperphase, Qnominalperphase), VLN));  // Between Vminpu and Vmaxpu, constant PQ
					}
				}
				StickCurrInTerminalArray(&(Iterminal[0]), cnegate(PhaseCurr), i);  // Put into Terminal array taking into account connection
				set_ITerminalUpdated(true, ActorID);
				StickCurrInTerminalArray(InjCurrent, PhaseCurr, i);  // Put into Terminal array taking into account connection
			}
			break;  /*Delta*/
			case 	1:
			{
				VLL = (Vterminal)[i - 1];
				VmagLL = cabs(VLL);
				if(CurrentLimited)
                    /*Current-Limited Model*/
				{
					DeltaCurr = conjg(cdiv(cmplx(Pnominalperphase, Qnominalperphase), VLL));
					if(cabs(DeltaCurr) * SQRT3 > PVSystemVars.MaxDynPhaseCurrent)
						DeltaCurr = conjg(cdiv(PhaseCurrentLimit, cdivreal(VLL, VmagLL / SQRT3)));
				}
				else

                   /*The usual model*/
				{
					switch(Fnphases)
					{
						case 	2:
						 case 3:
						VmagLN = VmagLL / SQRT3;
						break;
						default:
						VmagLN = VmagLL;
						break;
					}
					if(VmagLN <= VBaseMin)  // Below 95% use an impedance model
						DeltaCurr = cmul(cdivreal(YEQ_Min, 3.0), VLL);
					else
					{
						if(VmagLN > VBaseMax)  // above 105% use an impedance model
							DeltaCurr = cmul(cdivreal(YEQ_Max, 3.0), VLL);
						else
							DeltaCurr = conjg(cdiv(cmplx(Pnominalperphase, Qnominalperphase), VLL));  // Between 95% -105%, constant PQ
					}
				}
				StickCurrInTerminalArray(&(Iterminal[0]), cnegate(DeltaCurr), i);  // Put into Terminal array taking into account connection
				set_ITerminalUpdated(true, ActorID);
				StickCurrInTerminalArray(InjCurrent, DeltaCurr, i);  // Put into Terminal array taking into account connection
			}
			break;
			default:
			  ;
			break;
		}
		if (with0->Algorithm == NCIMSOLVE)				// NCIM solution algorithms
			DoPQBusNCIM(ActorID, i, Vterminal[i - 1], DeltaCurr);
	}
}
// ===========================================================================================

/*constant Z model*/

void TPVsystemObj::DoConstantZPVsystemObj(int ActorID)
{
	int i = 0;
	complex Curr = {};
	complex YEQ2 = {};
	complex V012[4] = { CZero, CZero, CZero, CZero, };  // Sequence voltages
	auto with0 = ActiveCircuit[ActorID]->Solution;

    // Assume YEQ is kept up to date
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
	if(ForceBalanced && (Fnphases == 3))  // convert to pos-seq only
	{
		Phase2SymComp(&(Vterminal[0]), &V012[0]);
		V012[0] = CZero; // Force zero-sequence voltage to zero
		V012[2] = CZero; // Force negative-sequence voltage to zero
		SymComp2Phase(&(Vterminal[0]), &V012[0]);  // Reconstitute Vterminal as balanced
	}
	ZeroITerminal();
	if(Connection == 0)        // YEQ is always line to neutral
		YEQ2 = Yeq;
	else
		YEQ2 = cdivreal(Yeq, 3.0);          // YEQ for delta connection
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		Curr = cmul(YEQ2, (Vterminal)[i - 1]);
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

		if (with0->Algorithm == NCIMSOLVE)				// NCIM solution algorithms
			DoPQBusNCIM(ActorID, i, Vterminal[i - 1], Curr);
	}
}

// =================================================================DOUSERMODEL==========================

/*Compute total terminal Current from User-written model*/

void TPVsystemObj::DoUserModel(int ActorID)
{
	int i = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	if(UserModel->Get_Exists())     // Check automatically selects the usermodel If true
	{
		UserModel->FCalc(&(Vterminal[0]), &(Iterminal[0]));
		set_ITerminalUpdated(true, ActorID);
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;          // Negate currents from user model for power flow PVSystem element model
			int stop = 0;
			for(stop = Fnconds, i = 1; i <= stop; i++)
			{
				caccum((InjCurrent)[i - 1], cnegate((Iterminal)[i - 1]));
			}
		}
	}
	else
	DoSimpleMsg(String("PVSystem.") + get_Name()
	           + " model designated to use user-written model, but user-written model is not defined.", 567);
}
// ===============================================================DoDynamicMode============================

/*Compute Total Current and add into InjTemp*/

void TPVsystemObj::DoDynamicMode(int ActorID)
{
	int i = 0;
	complex V012[3/*# range 0..2*/] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) };
	complex I012[3/*# range 0..2*/] = { cmplx(0,0), cmplx(0,0) , cmplx(0,0) };

	double	Theta		= 0.0, // phase angle of thevinen source
			iActual		= 0.0;
	complex NeutAmps	= cmplx(0,0),
			Vthev		= cmplx(0,0);
	polar	PolarN		= ctopolar(cmplx(0,0));

	    /*-------------- Internal Proc -----------------------*/

	auto CalcVthev_Dyn = [&](const complex& V) -> void 
	{

        /*Try to keep in phase with terminal voltage*/
		/*# with PVSystemVars do */
		{
			auto& with0 = PVSystemVars;
			if(cabs(V) > 0.20 * VBase)
				Theta = with0.ThetaDyn + (cang(V) - with0.InitialVAngle);
			else
				Theta = LastThevAngle;
			Vthev = pclx(with0.VthevmagDyn, Theta);
			LastThevAngle = Theta;     // remember this for angle persistence
		}
	};

	if (!GFM_Mode)
	{
		CalcYPrimContribution(InjCurrent, ActorID);   // Init InjCurrent Array  and computes VTerminal
		//{Inj = -Itotal(in) - Yprim * Vtemp}
		if (VoltageModel == 3)
		{
			if (UserModel->Get_Exists())
				UserModel->FCalc(&(Vterminal[0]), &(Iterminal[0])); // returns terminal currents in Iterminal
			else
			{
				DoSimpleMsg(Format("Dynamics model missing for PVSystem.% s ", get_Name().c_str()), 5671);
				SolutionAbort = true;
			}
		}
		else
		{
			ZeroITerminal();
			//{This model has no limitation in the nmber of phasesand is ideally unbalanced(no dq - dv, but is implementable as well)}
			// First, get the phase angles for the currents
			NeutAmps = cmplx(0, 0);
			for (i = 1; i <= Fnphases; i++)
			{
				auto& with0 = myDynVars;

				if (with0.it[i - 1] <= with0.iMaxPPhase || GFM_Mode)
					iActual = with0.it[i - 1];
				else
					iActual = with0.iMaxPPhase;
				//-----------------------------------------------------------

				PolarN = topolar(iActual, with0.Vgrid[i - 1].ang); // Output Current estimated for active power
				Iterminal[i - 1] = cnegate(ptocomplex(PolarN));
				NeutAmps = csub(NeutAmps, Iterminal[i - 1]);
			}
			if (Fnconds > Fnphases)
				Iterminal[Fnconds - 1] = NeutAmps;
			//{Add it into inj current array}
			for (i = 1; i <= Fnconds; i++)
				caccum(InjCurrent[i - 1], cnegate(Iterminal[i - 1]));
			set_ITerminalUpdated(true, ActorID);
		}
	}
	else
	{
		myDynVars.BaseV = myDynVars.BasekV * 1000 * (myDynVars.it[0] / myDynVars.iMaxPPhase);
		myDynVars.CalcGFMVoltage(ActorID, Get_NPhases(), &(Vterminal[0]));	// needs to be referenced as 0
		YPrim->MVmult(InjCurrent, &(Vterminal[0]));						// needs to be referenced as 1
	}
}
// ====================================================================DoHarmonicMode=======================

/*Compute Injection Current Only when in harmonics mode*/
/*Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built*/
/*Vd is the fundamental frequency voltage behind Xd" for phase 1*/

void TPVsystemObj::DoHarmonicMode(int ActorID)
{
	int i = 0;
	complex e = {};
	double PVSystemHarmonic = 0.0;
	ComputeVterminal(ActorID);
	/*# with ActiveCircuit[ActorID].Solution, PVSystemVars do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		auto& with1 = PVSystemVars;
		int stop = 0;
		PVSystemHarmonic = with0->get_FFrequency() / PVSystemFundamental;
		if(SpectrumObj != nullptr) // Get base harmonic magnitude
			e = cmulreal(SpectrumObj->GetMult(PVSystemHarmonic), with1.VThevHarm);
		else
			e = CZero;
		RotatePhasorRad(e, PVSystemHarmonic, with1.ThetaHarm);  // Time shift by fundamental frequency phase shift
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			cBuffer[i - 1] = e;
			if(i < Fnphases)
				RotatePhasorDeg(e, PVSystemHarmonic, -120.0);  // Assume 3-phase PVSystem element
		}
	}
    /*Handle Wye Connection*/
	if(Connection == 0)
		cBuffer[Fnconds - 1] = (Vterminal)[Fnconds - 1];  // assume no neutral injection voltage
		    /*Inj currents = Yprim (E) */
	YPrim->MVmult(InjCurrent, (pComplexArray) &cBuffer);
}
// ===========================================================================================

void TPVsystemObj::CalcVTerminalPhase(int ActorID)
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
					Vterminal[i - 1] = with1->VDiff(NodeRef[i - 1], NodeRef[j - 1], ActorID);
				}
			}
		}
		break;
		default:
		  ;
		break;
	}
	PVSystemSolutionCount = ActiveCircuit[ActorID]->Solution->SolutionCount;
}
// ===========================================================================================
/*
PROCEDURE TPVsystemObj.CalcVTerminal;
{Put terminal voltages in an array}
Begin
   ComputeVTerminal;
   PVSystemSolutionCount := ActiveCircuit[ActiveActor].Solution.SolutionCount;
End;
*/

// ============================================CalcPVSystemModelContribution===============================================

// Calculates PVSystem element current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

void TPVsystemObj::CalcPVSystemModelContribution(int ActorID)
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
			if (GFM_Mode) 
				DoGFM_Mode(ActorID);
			else
			{
				switch(VoltageModel)
				{
					case 	1:
					DoConstantPQPVsystemObj(ActorID);
					break;
					case 	2:
					DoConstantZPVsystemObj(ActorID);
					break;
					case 	3:
					DoUserModel(ActorID);
					break;
					default:
					DoConstantPQPVsystemObj(ActorID);  // for now, until we implement the other models.
					break;
				}
			} /*ELSE*/
		}
	} /*WITH*/
    /*When this is Done, ITerminal is up to date*/
}
// ==========================================CalcInjCurrentArray=================================================

  // Difference between currents in YPrim and total current

void TPVsystemObj::CalcInjCurrentArray(int ActorID)
{

    // Now Get Injection Currents
	if(PVsystemObjSwitchOpen)
		ZeroInjCurrent();
	else
		CalcPVSystemModelContribution(ActorID);
}

// -----------------------------------------------------------------------------------
// Updates the Jacobian matrix diagonal and calculates the Amps delta using deltaF
void TPVsystemObj::DoPQBusNCIM(int ActorID, int i, complex V, complex Curr)
{
	complex Pow = {},
		FaVr = {},
		FaVm = {},
		Temp = {},
		Vc2 = {};

	int LCoords[4][2] = { {0,0}, {1,1}, {0,1}, {1,0} };
	int GCoord	= 0;
	auto with0	= ActiveCircuit[ActorID]->Solution;
	Pow			= conjg(cmplx(-Pnominalperphase, -Qnominalperphase));
	Vc2			= cmul(conjg(V), conjg(V));
	FaVr		= cdiv(cmplx(-1, 0), Vc2);
	FaVm		= cdiv(cmplx(0, 1), Vc2);
	GCoord		= (NodeRef[i - 1] * 2) - 1;
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

// =========================================GetTerminalCurrents==================================================

// Compute total Currents

void TPVsystemObj::GetTerminalCurrents(pComplexArray Curr, int ActorID)
{
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(IterminalSolutionCount[ActorID] != ActiveCircuit[ActorID]->Solution->SolutionCount)     // recalc the contribution
		{
			if(!PVsystemObjSwitchOpen)
				CalcPVSystemModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
		}
		inherited::GetTerminalCurrents(Curr, ActorID);
	}
	if(DebugTrace)
		WriteTraceRecord("TotalCurrent");
}
// ===========================================INJCURRENTS================================================

int TPVsystemObj::InjCurrents(int ActorID)
{
	int result = 0;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(with0->LoadsNeedUpdating)
			SetNominalPVSystemOuput(ActorID); // Set the nominal kW, etc for the type of solution being Done
		CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current
		if(DebugTrace)
			WriteTraceRecord("Injection");
        // Add into System Injection Current Array
		result = inherited::InjCurrents(ActorID);
	}
	return result;
}
// ===========================================================================================

// Gives the currents for the last solution performed
// Do not call SetNominal, as that may change the load values

void TPVsystemObj::GetInjCurrents(pComplexArray Curr, int ActorID)
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
		DoErrorMsg(String("PVSystem Object: \"") + get_Name() + "\" in GetInjCurrents FUNCTION.", (std::string) e.what(), "Current buffer not big enough.", 568);
	}
}

//--------------------------------------------------------------------------------------------

double TPVsystemObj::get_PFNominal()
{
	return PFNominal;
}

//--------------------------------------------------------------------------------------------

double TPVsystemObj::get_Vminpu()
{
	return Vminpu;
}

//--------------------------------------------------------------------------------------------

double TPVsystemObj::get_Fkvarlimitneg()
{
	return PVSystemVars.Fkvarlimitneg;
}

// ===========================================================================================

void TPVsystemObj::ResetRegisters()
{
	int i = 0;
	int stop = 0;
	for(stop = NumPVSystemRegisters, i = 1; i <= stop; i++)
	{
		Registers[i - 1] = 0.0;
	}
	for(stop = NumPVSystemRegisters, i = 1; i <= stop; i++)
	{
		Derivatives[i - 1] = 0.0;
	}
	FirstSampleAfterReset = true;  // initialize for trapezoidal integration
}
// ===========================================================================================

void TPVsystemObj::Integrate(int reg, double Deriv, double Interval)
{
	if(ActiveCircuit[ActiveActor]->TrapezoidalIntegration)
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
// ===========================================================================================

// Update Energy from metered zone

void TPVsystemObj::TakeSample(int ActorID)
{
	complex s = {};
	double Smag = 0.0;
	double HourValue = 0.0;
    // Compute energy in PVSystem element branch
	if(Get_Enabled())
	{
		s = cmplx(Get_PresentkW(), Get_Presentkvar());
		Smag = cabs(s);
		HourValue = 1.0;
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			if(ActiveCircuit[ActorID]->PositiveSequence)
			{
				s = cmulreal(s, 3.0);
				Smag = 3.0 * Smag;
			}
			Integrate(Reg_kWh, s.re, with0->IntervalHrs);   // Accumulate the power
			Integrate(Reg_kvarh, s.im, with0->IntervalHrs);
			SetDragHandRegister(Reg_MaxkW, Abs( s.re));
			SetDragHandRegister(Reg_MaxkVA, Smag);
			Integrate(Reg_Hours, HourValue, with0->IntervalHrs);  // Accumulate Hours in operation
			Integrate(Reg_Price, s.re * ActiveCircuit[ActorID]->PriceSignal * 0.001, with0->IntervalHrs);  //
			FirstSampleAfterReset = false;
		}
	}
}
/*Update PVSystem levels*/

void TPVsystemObj::UpdatePVSystem()
{

    /* Do Nothing*/
}

double TPVsystemObj::Get_PresentkW()
{
	double result = 0.0;
	result = Pnominalperphase * 0.001 * Fnphases;
	return result;
}

double TPVsystemObj::Get_PresentIrradiance()
{
	double result = 0.0;
	result = PVSystemVars.FIrradiance * ShapeFactor.re;
	return result;
}

double TPVsystemObj::Get_PresentkV()
{
	double result = 0.0;
	result = PVSystemVars.kVPVSystemBase;
	return result;
}

double TPVsystemObj::Get_Presentkvar()
{
	double result = 0.0;
	result = Qnominalperphase * 0.001 * Fnphases;
	return result;
}

bool TPVsystemObj::Get_VarFollowInverter()
{
	bool result = false;
	if(FVarFollowInverter)
		result = true;
	else
		result = false;
	return result;
}

void TPVsystemObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int Idx = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;                              // HERE
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			Idx = (with0->PropertyIdxMap)[i - 1];
			switch(Idx)
			{
				case 	propUSERDATA:
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

// ============================================================InitHarmonics===============================

// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X

void TPVsystemObj::InitHarmonics(int ActorID)
{
	int i = 0;
	int j = 0;
	complex e = {};
	complex Va = {};
	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims
	PVSystemFundamental = ActiveCircuit[ActorID]->Solution->get_FFrequency();  // Whatever the frequency is when we enter here.
    /*Compute reference Thevinen voltage from phase 1 current*/
	ComputeIterminal(ActorID);  // Get present value of current
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		switch(Connection)
		{
			case 	0: /*wye - neutral is explicit*/
			{
				if(!ADiakoptics || (ActorID == 1))
					Va = csub(with0->NodeV[(NodeRef)[1 - 1]], with0->NodeV[(NodeRef)[Fnconds - 1]]);
				else
					Va = csub(with0->VoltInActor1((NodeRef)[1 - 1]), with0->VoltInActor1((NodeRef)[Fnconds - 1]));
			}
			break;  /*delta -- assume neutral is at zero*/
			case 	1:
			{
				if(!ADiakoptics || (ActorID == 1))
					Va = with0->NodeV[(NodeRef)[1 - 1]];
				else
					Va = with0->VoltInActor1((NodeRef)[1 - 1]);
			}
			break;
			default:
			  ;
			break;
		}
	}
	/*# with PVSystemVars do */
	{
		auto& with1 = PVSystemVars;
		Yeq = cinv(cmplx(with1.RThev, with1.XThev));           // used for current calcs  Always L-N
		e = csub(Va, cmul((Iterminal)[1 - 1], cmplx(with1.RThev, with1.XThev)));
		with1.VThevHarm = cabs(e);   // establish base mag and angle
		with1.ThetaHarm = cang(e);
	}
}

// ===============================================================InitStateVars============================

// for going into dynamics mode

void TPVsystemObj::InitStateVars(int ActorID)
{

//    VNeut,
	complex Edp = {};
	complex V12 = {};
	double	BaseZt = 0.0;
	int		i		= 0,
			j		= 0;

	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims
	if (PICtrl.empty() || (PICtrl.size() < Fnphases))
	{
		PICtrl.resize(Fnphases);
		for (i = 0; i < Fnphases; i++)
		{
			PICtrl[i] = TPICtrl();
			PICtrl[i].Kp = myDynVars.Kp;
			PICtrl[i].kNum = 0.9502;
			PICtrl[i].kDen = 0.04979;
		}
	}
	myDynVars.SafeMode = false;
	auto with2 = ActiveCircuit[ActorID]->Solution;
	auto& with3 = PVSystemVars;
	auto& with4 = myDynVars;

	switch (ActiveCircuit[ActorID]->ActiveLoadShapeClass)
	{
	case	USEDAILY:
		CalcDailyMult(with2->DynaVars.dblHour);
		CalcDailyTemperature(with2->DynaVars.dblHour);
		break;
	case	USEYEARLY:
		CalcYearlyMult(with2->DynaVars.dblHour);
		CalcYearlyTemperature(with2->DynaVars.dblHour);
		break;
	case	USEDUTY:
		CalcDutyMult(with2->DynaVars.dblHour);
		CalcDutyTemperature(with2->DynaVars.dblHour);
		break;
	default:
		ShapeFactor = CDoubleOne;     // default to 1 + j1 if not known
		break;
	}

	ComputePanelPower();

	with3.NumPhases		= Fnphases;
	with3.NumConductors = Fnconds;
	with3.Conn			= Connection;

	with4.InitDynArrays(with3.NumPhases);
	if (with3.NumPhases > 1)
		with4.BasekV = Get_PresentkV() / sqrt(3);
	else
		with4.BasekV = Get_PresentkV();

	BaseZt				= 0.01 * (sqr(Get_PresentkV()) / with3.FkVArating) * 1000;
	with4.MaxVS			= (2 - (with4.SMThreshold / 100)) * with4.BasekV * 1000;
	with4.MinVS			= (with4.SMThreshold / 100) * with4.BasekV * 1000;
	with4.MinAmps		= (FpctCutOut / 100) * ((with3.FkVArating / with4.BasekV) / with3.NumPhases);
	with4.ResetIBR		= false;
	with4.iMaxPPhase	= (with3.FkVArating / with4.BasekV) / with3.NumPhases;

	if (pctX == 0)
		pctX = 50;
	with3.XThev = pctX * BaseZt;
	with4.RS	= pctR * BaseZt;
	Zthev		= cmplx(with4.RS, with3.XThev);
	Yeq			= cinv(Zthev);
	ComputeIterminal(ActorID);
	with4.LS	= Zthev.im / (2 * PI * DefaultBaseFreq);
	for (i = 0; i < Get_NPhases(); i++)
	{
		with4.Vgrid[i] = ctopolar(with2->NodeV[NodeRef[i]]);
		with4.dit[i] = 0;
		if (GFM_Mode) 
			with4.it[i] = 0;
		else
			with4.it[i] = ((with3.PanelkW * 1000) / with4.Vgrid[i].mag) / Fnphases;
		with4.m[i] = ((with4.RS * with4.it[i]) + with4.Vgrid[i].mag) / with4.RatedVDC;  // Duty factor in terms of actual voltage
		if (with4.m[i] > 1)
			with4.m[i] = 1;
		with4.ISPDelta[i] = 0;
		with4.AngDelta[i] = 0;
	}
	if (ASSIGNED(DynamicEqObj))
		for (i = 0; i < DynamicEqVals.size(); i++)
			DynamicEqVals[i][1] = 0;													// Initializes the memory values for the dynamic equation

}
// ===========================================================================================

// dynamics mode integration routine
// VAR
//    TracePower:Complex;

void TPVsystemObj::IntegrateStates(int ActorID)
{
	int		NumData = 0,
			j		= 0,
			k		= 0,
			i		= 0;
	bool GFMUpdate = false; // To avoid updating the IBR if current limit reached
	double IPresent;
	std::vector<complex>	myCurr;

	// Compute Derivatives and Then integrate
	ComputeIterminal(ActorID);
	if (UserModel->Get_Exists())   // Checks for existence and Selects
		UserModel->Integrate();
	else
	{
		auto	wSol = ActiveCircuit[ActorID]->Solution;
		auto&	wSV =	PVSystemVars;
		auto&	wDynV = myDynVars;

		// Compute actual power output for the PVPanel

		switch (ActiveCircuit[ActorID]->ActiveLoadShapeClass)
		{
		 case	USEDAILY: 
			 CalcDailyMult(wSol->DynaVars.dblHour);  
			 CalcDailyTemperature(wSol->DynaVars.dblHour);  
			 break;
		 case	USEYEARLY: 
			 CalcYearlyMult(wSol->DynaVars.dblHour); 
			 CalcYearlyTemperature(wSol->DynaVars.dblHour); 
			 break;
		case	USEDUTY:   
			CalcDutyMult(wSol->DynaVars.dblHour);   
			CalcDutyTemperature(wSol->DynaVars.dblHour);
			break;
		default:
			 ShapeFactor = CDoubleOne;     // default to 1 + j1 if not known
			 break;
		}
		ComputePanelPower();
		wDynV.iMaxPPhase = (wSV.PanelkW / wDynV.BasekV) / wSV.NumPhases;
		for (i = 0; i < wSV.NumPhases; i++)
		{
			auto& DynaVars = wSol->DynaVars;
			if (DynaVars.IterationFlag == 0) //{ First iteration of new time step }
				wDynV.itHistory[i] = wDynV.it[i] + 0.5 * DynaVars.h * wDynV.dit[i];

			wDynV.Vgrid[i] = ctopolar(wSol->NodeV[NodeRef[i]]);// Voltage at the Inv terminals
			// Compute the actual target (Amps)
			if (!GFM_Mode)
			{
				wDynV.ISP = ((wSV.PanelkW * 1000) / wDynV.Vgrid[i].mag) / wSV.NumPhases;
				if (wDynV.ISP > wDynV.iMaxPPhase)
					wDynV.ISP = wDynV.iMaxPPhase;
				if ((wDynV.Vgrid[i].mag < wDynV.MinVS) || (wDynV.Vgrid[i].mag > wDynV.MaxVS))
					wDynV.ISP = 0.01;
			}
			else
			{
				if (wDynV.ResetIBR)	wDynV.VDelta[i] = ((0.001 - (wDynV.Vgrid[i].mag / 1000)) / wDynV.BasekV);
				else				wDynV.VDelta[i] = (wDynV.BasekV - (wDynV.Vgrid[i].mag / 1000)) / wDynV.BasekV;
				
				GFMUpdate = true;
				if (myDynVars.ILimit > 0)
				{
					myCurr.resize(Fnphases + 1);
					GetCurrents(&myCurr[0], ActorID);
					for (j = 0; j < (Fnphases - 1); j++)
					{
						IPresent = ctopolar(myCurr[j]).mag;
						GFMUpdate = GFMUpdate && (IPresent < (myDynVars.ILimit * myDynVars.VError));
					}
				}
				if (abs(wDynV.VDelta[i]) > wDynV.CtrlTol && GFMUpdate)
				{
					wDynV.ISPDelta[i] = wDynV.ISPDelta[i] + (wDynV.iMaxPPhase * wDynV.VDelta[i]) * wDynV.Kp * 100;
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
						switch (DynamicEqPair[(j * 2) + 1])
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
							DynamicEqVals[DynamicEqPair[j * 2]][0] = Get_PCEValue(1, DynamicEqPair[(j * 2) + 1],ActorID);
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

// ===========================================================Get_Variable================================

/*Return variables one at a time*/

double TPVsystemObj::Get_Variable(int i)
{
	double result = 0.0;
	int n = 0;
	int k = 0;
	result = -9999.99;  // error return value; no state fars
	if(i < 1)
		return result;
    // for now, report kWhstored and mode
	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
		switch(i)
		{
			case 	1:
				result = Get_PresentIrradiance();
			break;
			case 	2:
				result = with0.PanelkW;
			break;
			case 	3:
				result = with0.TempFactor;
			break;
			case 	4:
				result = with0.EffFactor;
			break;
			case 	5:
				result = with0.Vreg;
			break;
			case 	6:
				result = with0.Vavg;
			break;
			case 	7:
				result = with0.VVOperation;
			break;
			case 	8:
				result = with0.VWOperation;
			break;
			case 	9:
				result = with0.DRCOperation;
			break;
			case 	10:
				result = with0.VVDRCOperation;
			break;
			case 	11:
				result = with0.WPOperation;
			break;
			case 	12:
				result = with0.WVOperation;
			break;
			case 	13:
				result = with0.PanelkW * with0.EffFactor;
			break;
			default:
				result = myDynVars.Get_InvDynValue(i - 14, Get_NPhases());
				if(UserModel->Get_Exists())
				{
					n = UserModel->FNumVars();
					k = (i - NumPVSystemVariables);
					if(k <= n)
					{
						result = UserModel->FGetVariable(k);
						return result;
					}
				}
			break;
		}
	}
	return result;
}

bool TPVsystemObj::Get_InverterON()
{
	bool result = false;
	if(FInverterON)
		result = true;
	else
		result = false;
	return result;
}
// ============================================================Get_Varmode===============================

int TPVsystemObj::Get_Varmode()
{
	int result = 0;
	result = FvarMode;
	return result;
}
// ============================================================Get_VWmode===============================

bool TPVsystemObj::Get_VWmode()
{
	bool result = false;
	if(FVWMode)
		result = true;
	else
		result = false;    // TRUE if volt-watt mode                                                            //  engaged from InvControl (not ExpControl)
	return result;
}
// ============================================================Get_VVmode===============================

bool TPVsystemObj::Get_VVmode()
{
	bool result = false;
	if(FVVMode)
		result = true;
	else
		result = false;                                                               //  engaged from InvControl (not ExpControl)
	return result;
}
// ============================================================Get_WPmode===============================

bool TPVsystemObj::Get_WPmode()
{
	bool result = false;
	if(FWPMode)
		result = true;
	else
		result = false;                                                               //  engaged from InvControl (not ExpControl)
	return result;
}
// ============================================================Get_WVmode===============================

bool TPVsystemObj::Get_WVmode()
{
	bool result = false;
	if(FWVMode)
		result = true;
	else
		result = false;                                                               //  engaged from InvControl (not ExpControl)
	return result;
}
// ============================================================Get_DRCmode===============================

bool TPVsystemObj::Get_DRCmode()
{
	bool result = false;
	if(FDRCMode)
		result = true;
	else
		result = false;                                                               //  engaged from InvControl (not ExpControl)
	return result;
}

// ============================================================Get_AVRmode===============================

bool TPVsystemObj::Get_AVRmode()
{
	bool result = false;
	if(FAVRMode)
		result = true;
	else
		result = false;                                                               //  engaged from InvControl (not ExpControl)
	return result;
}
// ============================================================kWOut_Calc===============================

void TPVsystemObj::kWOut_Calc()
{
	double Pac = 0.0;
	double PpctLimit = 0.0;
	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
		Pac = with0.PanelkW * with0.EffFactor;
		if(Get_VWmode() || Get_WVmode())
		{
			if(Pac > kWRequested)
				kW_out = kWRequested;
			else
				kW_out = Pac;
		}
		else
		{
			PpctLimit = with0.FPmpp * with0.FpuPmpp;
			if(Pac > PpctLimit)
				kW_out = PpctLimit;
			else
				kW_out = Pac;
		}
	}
}
// ============================================================Set_Variable===============================

void TPVsystemObj::Set_Variable(int i, double Value)
{
	int n = 0;
	int k = 0;
	if(i < 1)
		return;  // No variables to set
	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
		switch(i)
		{
			case 	1:
			with0.FIrradiance = Value;
			break;
			case 	2:
			;
			break; // Setting this has no effect Read only
			case 	3:
			;
			break; // Setting this has no effect Read only
			case 	4:
			;
			break; // Setting this has no effect Read only
			case 	5:
			with0.Vreg = Value;
			break; // the InvControl or ExpControl will do this
			case 	6:
				with0.Vavg = Value;
			break;
			case 	7:
				with0.VVOperation = Value;
			break;
			case 	8:
				with0.VWOperation = Value;
			break;
			case 	9:
				with0.DRCOperation = Value;
			break;
			case 	10:
				with0.VVDRCOperation = Value;
			break;
			case 	11:
				with0.WPOperation = Value;
			break;
			case 	12:
				with0.WVOperation = Value;
			break;
			case 	13:
			;
			break; //ReadOnly //kW_out_desired := Value;
			default:
			myDynVars.Set_InvDynValue(i - 14, Value);
			if(UserModel->Get_Exists())
			{
				n = UserModel->FNumVars();
				k = (i - NumPVSystemVariables);
				if(k <= n)
				{
					UserModel->FSetVariable(k, Value);
					return;
				}
			}
			break;
		}
	}
}

void TPVsystemObj::Set_Varmode(int Value)
{
	FvarMode = Value;
}
// ===========================================================================================

void TPVsystemObj::Set_VWmode(bool Value)
{
	FVWMode = Value;
}
// ===========================================================================================

void TPVsystemObj::Set_VVmode(bool Value)
{
	FVVMode = Value;
}
// ===========================================================================================

void TPVsystemObj::Set_WVmode(bool Value)
{
	FWVMode = Value;
}

// ===========================================================================================

void TPVsystemObj::Set_WPmode(bool Value)
{
	FWPMode = Value;
}
// ===========================================================================================

void TPVsystemObj::Set_DRCmode(bool Value)
{
	FDRCMode = Value;
}
// ===========================================================================================

void TPVsystemObj::Set_AVRmode(bool Value)
{
	FAVRMode = Value;
}
// ===========================================================================================

void TPVsystemObj::GetAllVariables(pDoubleArray States)
{
/*, N*/
	int i		= 0,
		stop	= 0;

	if (!ASSIGNED(DynamicEqObj))
	{
		for (stop = NumPVSystemVariables, i = 1; i <= stop; i++)
			(States)[i - 1] = Get_Variable(i);
	}
	else
	{
		for (i = 1; i <= (DynamicEqObj->get_FNumVars() * DynamicEqVals[0].size()); i++)
			States[i - 1] = DynamicEqObj->Get_DynamicEqVal(i - 1, &DynamicEqVals);
	}

	if(UserModel->Get_Exists())
		UserModel->FGetAllVars((pDoubleArray)&(States)[NumPVSystemVariables + 1 - 1]);
}
// ===========================================================================================

int TPVsystemObj::NumVariables()
{
	int result = 0;
	result = NumPVSystemVariables;
	if(UserModel->Get_Exists())
		result = result + UserModel->FNumVars();
	return result;
}
// ===========================================================================================

String TPVsystemObj::VariableName(int i)
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
			result = "Irradiance";
		break;
		case 	2:
			result = "PanelkW";
		break;
		case 	3:
			result = "P_TFactor";
		break;
		case 	4:
			result = "Efficiency";
		break;
		case 	5:
			result = "Vreg";
		break;
		case 	6:
			result = "Vavg (DRC)";
		break;
		case 	7:
			result = "volt-var";
		break;
		case 	8:
			result = "volt-watt";
		break;
		case 	9:
			result = "DRC";
		break;
		case 	10:
			result = "VV_DRC";
		break;
		case 	11:
			result = "watt-pf";
		break;
		case 	12:
			result = "watt-var";
		break;
		case 	13:
			result = "kW_out_desired";
		break;
		default:
			result = myDynVars.Get_InvDynName(i - 14);
			if(UserModel->Get_Exists())
			{
				PName = (PAnsiChar) &Buff;
				n = UserModel->FNumVars();
				I2 = i - NumPVSystemVariables;
				if(I2 <= n)
				{
					UserModel->FGetVarName(I2, PName, (unsigned int) BuffSize);
					result = PName;
					return result;
				}
			}
		break;
	}
	return result;
}
// ===========================================================================================

void TPVsystemObj::MakePosSequence(int ActorID)
{
	String s;
	double V = 0.0;
	s = "Phases=1 conn=wye";
	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
        // Make sure voltage is line-neutral
		if((Fnphases > 1) || (Connection != 0))
			V = with0.kVPVSystemBase / SQRT3;
		else
			V = with0.kVPVSystemBase;
		s = s + Format(" kV=%-.5g", V);
		if(Fnphases > 1)
			s = s
	           + Format(" kva=%-.5g  PF=%-.5g", with0.FkVArating / Fnphases, PFNominal);
		Parser[ActorID]->SetCmdString(s);
		Edit(ActorID);
	}
	inherited::MakePosSequence(ActorID);   // write out other properties
}
// ===========================================================================================

void TPVsystemObj::Set_ConductorClosed(int Index, int ActorID, bool Value)
{
	inherited::Set_ConductorClosed(Index, ActorID, Value);
    // Just turn PVSystem element on or off;
	if(Value)
		PVsystemObjSwitchOpen = false;
	else
		PVsystemObjSwitchOpen = true;
}

void TPVsystemObj::Set_Maxkvar(double Value)
{
	PVSystemVars.Fkvarlimit = Value;
	Set_PropertyValue(propkvarLimit,Format("%-g", PVSystemVars.Fkvarlimit));
}

void TPVsystemObj::Set_Maxkvarneg(double Value)
{
	PVSystemVars.Fkvarlimitneg = Value;
	Set_PropertyValue(propkvarLimitneg,Format("%-g", PVSystemVars.Fkvarlimitneg));
}

void TPVsystemObj::Set_kVARating(double Value)
{
	PVSystemVars.FkVArating = Value;
	Set_PropertyValue(propKVA,Format("%-g", PVSystemVars.FkVArating));
}
// ===========================================================================================

void TPVsystemObj::Set_Pmpp(double Value)
{
	PVSystemVars.FPmpp = Value;
	Set_PropertyValue(propPmpp,Format("%-g", PVSystemVars.FkVArating));
}

void TPVsystemObj::Set_PowerFactor(double Value)
{
	PFNominal = Value;
	Set_Varmode(VARMODEPF);
}

void TPVsystemObj::Set_PresentIrradiance(double Value)
{
	PVSystemVars.FIrradiance = Value;
}

void TPVsystemObj::Set_PresentkV(double Value)
{
	/*# with PVSystemVars do */
	{
		auto& with0 = PVSystemVars;
		with0.kVPVSystemBase = Value;
		switch(Fnphases)
		{
			case 	2:
			 case 3:
			VBase = with0.kVPVSystemBase * InvSQRT3x1000;
			break;
			default:
			VBase = with0.kVPVSystemBase * 1000.0;
			break;
		}
	}
}

void TPVsystemObj::Set_VarFollowInverter(bool Value)
{
	FVarFollowInverter = Value;
}

void TPVsystemObj::Set_InverterON(bool Value)
{
	FInverterON = Value;
}

void TPVsystemObj::Set_PresentkW(double Value)
{
	kWRequested = Value;
}

void TPVsystemObj::Set_Presentkvar(double Value)
{
	kvarRequested = Value;
}

void TPVsystemObj::Set_pf_wp_nominal(double Value)
{
	Fpf_wp_nominal = Value;
}

void TPVsystemObj::Set_puPmpp(double Value)
{
	PVSystemVars.FpuPmpp = Value;
}

void TPVsystemObj::SetDragHandRegister(int reg, double Value)
{
	if(Value > Registers[reg])
		Registers[reg] = Value;
}


void PVSystem_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
}

// functions for returning struct data to properties

double TPVsystemObj::Get_FkVArating()
{
	return PVSystemVars.FkVArating;
}

double TPVsystemObj::Get_FPmpp()
{
	return PVSystemVars.FPmpp;
}

double TPVsystemObj::Get_FpuPmpp()
{
	return PVSystemVars.FpuPmpp;
}

double TPVsystemObj::Get_Fkvarlimit()
{
	return PVSystemVars.Fkvarlimit;
}

double TPVsystemObj::Get_FShapefactorRe()
{
	return ShapeFactor.re;
}

// for CIM export

double TPVsystemObj::Get_Pmin()
{
	return std::min(FpctCutIn, FpctCutOut) * Get_FkVArating() / 100.0;
}

double TPVsystemObj::Get_Pmax()
{
	return Get_FPmpp();
}

double TPVsystemObj::Get_qMaxInj()
{
    double result = PVSystemVars.Fkvarlimit;
    if (!kvarLimitSet)
        result = 0.25 * Get_FkVArating(); // unlike storage, defaults to category A

	return result;
}

double TPVsystemObj::Get_qMaxAbs()
{
    double result = PVSystemVars.Fkvarlimitneg;
    if (!kvarLimitNegSet)
        result = 0.25 * Get_FkVArating(); // unlike storage, defaults to category A

	return result;
}

double TPVsystemObj::Get_acVmin()
{
	return Get_PresentkV() * Vminpu;
}

double TPVsystemObj::Get_acVmax()
{
	return Get_PresentkV() * Vmaxpu;
}

double TPVsystemObj::Get_acVnom()
{
	return Get_PresentkV();
}

double TPVsystemObj::Get_pMaxUnderPF()
{
	double q = Get_qMaxAbs();
	auto& with0 = PVSystemVars;
	return sqrt( Sqr(with0.FkVArating) - q * q);
}

double TPVsystemObj::Get_pMaxOverPF()
{
	double q = Get_qMaxInj();
	auto& with0 = PVSystemVars;
	return sqrt(Sqr(with0.FkVArating) - q * q);
}

double TPVsystemObj::Get_pMaxCharge()
{
	return 0.0;
}

double TPVsystemObj::Get_sMaxCharge()
{
	return 0.0;
}

bool TPVsystemObj::Get_CIMDynamicMode()
{
    bool result = false;

	result = FVWMode || FVVMode || FWVMode || FAVRMode || FDRCMode; // FWPMode not in CIM Dynamics

	return result;
}


		class 		PVSystem_unit
		{
		public:
		PVSystem_unit()
		{
			//AssertSystemInitialization();
			PVSystem_initialization();
		}
		};
		PVSystem_unit _PVSystem_unit;


}  // namespace PVSystem




