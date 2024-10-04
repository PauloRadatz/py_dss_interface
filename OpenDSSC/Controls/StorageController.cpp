

#pragma hdrstop

#include "StorageController.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Storage.h"
#include "Ucmatrix.h"
#include "mathutil.h"
#include <math.h>
#include "Dynamics.h"
#include "XYcurve.h"
#include "Utilities.h"
#include "Solution.h"

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
using namespace Dynamics;
using namespace LoadShape;
using namespace ParserDel;
using namespace Storage;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XYCurve;
using namespace mathutil;
using namespace Utilities;

namespace StorageController
{

TStorageControllerObj::TStorageControllerObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TStorageControllerObj::TStorageControllerObj(String ClassName) : inherited(ClassName) {}
TStorageControllerObj::TStorageControllerObj() {}


TStorageControllerObj* ActiveStorageController2Obj = nullptr;
const int propELEMENT = 1;
const int propTERMINAL = 2;
const int propMONPHASE = 3;
const int propKWTARGET = 4;
const int propKWTARGETLOW = 5;
const int propPCTKWBAND = 6;
const int propKWBAND = 7;
const int propPCTKWBANDLOW = 8;
const int propKWBANDLOW = 9;
//    propPFTARGET      = 7;
//    propPFBAND        = 8;
const int propELEMENTLIST = 10;
const int propWEIGHTS = 11;
const int propMODEDISCHARGE = 12;
const int propMODECHARGE = 13;
const int propTIMEDISCHARGETRIGGER = 14;
const int propTIMECHARGETRIGGER = 15;
const int propRATEKW = 16;
//    propRATEKVAR      = 16;
const int propRATECHARGE = 17;
const int propRESERVE = 18;
const int propKWHTOTAL = 19;
const int propKWTOTAL = 20;
const int propKWHACTUAL = 21;
const int propKWACTUAL = 22;
const int propKWNEED = 23;
//    propPARTICIPATION = 24;
const int propYEARLY = 24;
const int propDAILY = 25;
const int propDUTY = 26;
const int propEVENTLOG = 27;
//    propVARDISPATCH   = 29;
const int propINHIBITTIME = 28;
const int propTUPRAMP = 29;
const int propTFLAT = 30;
const int propTDNRAMP = 31;
const int propKWTHRESHOLD = 32;
const int propDispFactor = 33;
const int propRESETLEVEL = 34;
const int propSEASONS = 35;
const int propSEASONTARGETS = 36;
const int propSEASONTARGETSLOW = 37;
const int NumPropsThisClass = 37;

//= = = = = = = = = = = = = = DEFINE CONTROL MODE CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =
const int MODEFOLLOW = 1;
const int MODELOADSHAPE = 2;
const int MODESUPPORT = 3;
const int MODETIME = 4;
const int MODEPEAKSHAVE = 5;
const int MODESCHEDULE = 6;
const int MODEPEAKSHAVELOW = 7;
const int CURRENTPEAKSHAVE = 8;
const int CURRENTPEAKSHAVELOW = 9;

//= = = = = = = = = = = = = = DEFINE OTHER CONSTANTS = = = = = = = = = = = = = = = = = = = = = = = = =
const int RELEASE_INHIBIT = 999;
complex CDoubleOne = {};

/*--------------------------------------------------------------------------*/  // Creates superstructure for all StorageController objects

TStorageController::TStorageController()
{
	;
	Class_Name = "StorageController";
	DSSClassType = DSSClassType + STORAGE_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

/*--------------------------------------------------------------------------*/

TStorageController::~TStorageController()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TStorageController::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[propELEMENT - 1]				= "Element";
	PropertyName[propTERMINAL - 1]				= "Terminal";
	PropertyName[propMONPHASE - 1]				= "MonPhase";
	PropertyName[propKWTARGET - 1]				= "kWTarget";
	PropertyName[propKWTARGETLOW - 1]			= "kWTargetLow";
	PropertyName[propPCTKWBAND - 1]				= "%kWBand";
	PropertyName[propKWBAND - 1]				= "kWBand";
	PropertyName[propPCTKWBANDLOW - 1]			= "%kWBandLow";
	PropertyName[propKWBANDLOW - 1]				= "kWBandLow";
//     PropertyName[propPFTARGET]               := 'PFTarget';
//     PropertyName[propPFBAND]                 := 'PFBand';
	PropertyName[propELEMENTLIST - 1]			= "ElementList";
	PropertyName[propWEIGHTS - 1]				= "Weights";
	PropertyName[propMODEDISCHARGE - 1]			= "ModeDischarge";
	PropertyName[propMODECHARGE - 1]			= "ModeCharge";
	PropertyName[propTIMEDISCHARGETRIGGER - 1]	= "TimeDischargeTrigger";
	PropertyName[propTIMECHARGETRIGGER - 1]		= "TimeChargeTrigger";
	PropertyName[propRATEKW - 1]				= "%RatekW";
//     PropertyName[propRATEKVAR]               := '%Ratekvar';
	PropertyName[propRATECHARGE - 1]			= "%RateCharge";
	PropertyName[propRESERVE - 1]				= "%Reserve";
	PropertyName[propKWHTOTAL - 1]				= "kWhTotal";
	PropertyName[propKWTOTAL - 1]				= "kWTotal";
	PropertyName[propKWHACTUAL - 1]				= "kWhActual";
	PropertyName[propKWACTUAL - 1]				= "kWActual";
	PropertyName[propKWNEED - 1]				= "kWneed";
//     PropertyName[propPARTICIPATION]          := '%Participation';
	PropertyName[propYEARLY - 1]				= "Yearly";
	PropertyName[propDAILY - 1]					= "Daily";
	PropertyName[propDUTY - 1]					= "Duty";
	PropertyName[propEVENTLOG - 1]				= "EventLog";
//     PropertyName[propVARDISPATCH]            := 'VarDispatch';
	PropertyName[propINHIBITTIME - 1]			= "InhibitTime";
	PropertyName[propTUPRAMP - 1]				= "Tup";
	PropertyName[propTFLAT - 1]					= "TFlat";
	PropertyName[propTDNRAMP - 1]				= "Tdn";
	PropertyName[propKWTHRESHOLD - 1]			= "kWThreshold";
	PropertyName[propDispFactor - 1]			= "DispFactor";
	PropertyName[propRESETLEVEL - 1]			= "ResetLevel";
	PropertyName[propSEASONS - 1]				= "Seasons";
	PropertyName[propSEASONTARGETS - 1]			= "SeasonTargets";
	PropertyName[propSEASONTARGETSLOW - 1]		= "SeasonTargetsLow";
	PropertyHelp[propELEMENT - 1] = "Full object name of the circuit element, typically a line or transformer, "
	           "which the control is monitoring. There is no default; Must be specified.";
	PropertyHelp[propTERMINAL - 1] = "Number of the terminal of the circuit element to which the StorageController2 control is connected. "
	           "1 or 2, typically.  Default is 1. Make sure to select the proper direction on the power for the respective dispatch mode.";
	PropertyHelp[propMONPHASE - 1] = "Number of the phase being monitored or one of {AVG | MAX | MIN} for all phases. Default=MAX. "
	           "Must be less than the number of phases. Used in PeakShave, Follow, Support and I-PeakShave discharging modes "
	           "and in PeakShaveLow, I-PeakShaveLow charging modes. For modes based on active power measurements, the value "
	           "used by the control is the monitored one multiplied by the number of phases of the monitored element.";
	PropertyHelp[propKWTARGET - 1] = "kW/kamps target for Discharging. The Storage element fleet is dispatched to try to hold the power/current in band "
	           "at least until the Storage is depleted. The selection of power or current depends on the Discharge mode (PeakShave->kW, I-PeakShave->kamps).";
	PropertyHelp[propKWTARGETLOW - 1] = "kW/kamps target for Charging. The Storage element fleet is dispatched to try to hold the power/current in band "
	           "at least until the Storage is fully charged. The selection of power or current depends on the charge mode (PeakShavelow->kW, I-PeakShavelow->kamps).";
	PropertyHelp[propPCTKWBAND - 1] = "Bandwidth (% of Target kW/kamps) of the dead band around the kW/kamps target value. Default is 2% (+/-1%)."
	           "No dispatch changes are attempted if the power in the monitored terminal stays within this band.";
	PropertyHelp[propKWBAND - 1] = "Alternative way of specifying the bandwidth. (kW/kamps) of the dead band around the kW/kamps target value. Default is 2% of kWTarget (+/-1%)."
	           "No dispatch changes are attempted if the power in the monitored terminal stays within this band.";
	PropertyHelp[propPCTKWBANDLOW - 1] = "Bandwidth (% of kWTargetLow) of the dead band around the kW/kamps low target value. Default is 2% (+/-1%)."
	           "No charging is attempted if the power in the monitored terminal stays within this band.";
	PropertyHelp[propKWBANDLOW - 1] = "Alternative way of specifying the bandwidth. (kW/kamps) of the dead band around the kW/kamps low target value. Default is 2% of kWTargetLow (+/-1%)."
	           "No charging is attempted if the power in the monitored terminal stays within this band.";
//    PropertyHelp[propPFTARGET]          :=
//      'Power Factor target for dispatching the reactive power. Default is 0.96. The reactive power of the storage element fleet is dispatched to try to hold the power factor in band. '+
//      'It is assumed that the storage element inverter can produce kvar up to its kVA limit regardless of storage level.';
//    PropertyHelp[propPFBAND]            :=
//      'Bandwidth of the Target power factor of the monitored element. of the dead band around the kvar target value. Default is 0.04 (+/- 0.02).' +
//      'No dispatch changes of the kvar are attempted If the power factor of the monitored terminal stays within this band.';
	PropertyHelp[propELEMENTLIST - 1] = "Array list of Storage elements to be controlled.  If not specified, all Storage elements in the circuit not presently dispatched by another controller "
	           "are assumed dispatched by this controller.";
	PropertyHelp[propWEIGHTS - 1] = "Array of proportional weights corresponding to each Storage element in the ElementList. "
	           "The needed kW or kvar to get back to center band is dispatched to each Storage element according to these weights. "
	           "Default is to set all weights to 1.0.";
	PropertyHelp[propMODEDISCHARGE - 1] = String("{PeakShave* | Follow | Support | Loadshape | Time | Schedule | I-PeakShave} Mode of operation for the DISCHARGE FUNCTION of this controller. ") + CRLF
	           + CRLF
	           + "In PeakShave mode (Default), the control attempts to discharge Storage to keep power in the monitored element below the kWTarget. "
	           + CRLF
	           + CRLF
	           + "In Follow mode, the control is triggered by time and resets the kWTarget value to the present monitored element power. "
	           + "It then attempts to discharge Storage to keep power in the monitored element below the new kWTarget. See TimeDischargeTrigger."
	           + CRLF
	           + CRLF
	           + "In Support mode, the control operates oppositely of PeakShave mode: Storage is discharged to keep kW power output up near the target. "
	           + CRLF
	           + CRLF
	           + "In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. "
	           + "Storage is discharged when the loadshape value is positive. "
	           + CRLF
	           + CRLF
	           + "In Time mode, the Storage discharge is turned on at the specified %RatekW at the specified discharge trigger time in fractional hours."
	           + CRLF
	           + CRLF
	           + "In Schedule mode, the Tup, TFlat, and Tdn properties specify the up ramp duration, flat duration, and down ramp duration for the schedule. "
	           + "The schedule start time is set by TimeDischargeTrigger and the rate of discharge for the flat part is determined by %RatekW."
	           + CRLF
	           + CRLF
	           + "In I-PeakShave mode, the control attempts to discharge Storage to keep current in the monitored element below the target given in k-amps "
	           + "(thousands of amps), when this control mode is active, the property kWTarget will be expressed in k-amps. ";
	PropertyHelp[propMODECHARGE - 1] = String("{Loadshape | Time* | PeakShaveLow | I-PeakShaveLow} Mode of operation for the CHARGE FUNCTION of this controller. ") + CRLF
	           + CRLF
	           + "In Loadshape mode, both charging and discharging precisely follows the per unit loadshape. "
	           + "Storage is charged when the loadshape value is negative. "
	           + CRLF
	           + CRLF
	           + "In Time mode, the Storage charging FUNCTION is triggered at the specified %RateCharge at the specified charge trigger time in fractional hours."
	           + CRLF
	           + CRLF
	           + "In PeakShaveLow mode, the charging operation will charge the Storage fleet when the power at a"
	           + "monitored element is below a specified KW target (kWTarget_low). The Storage will charge as much power as necessary to keep the power within the deadband around kWTarget_low."
	           + CRLF
	           + CRLF
	           + "In I-PeakShaveLow mode, the charging operation will charge the Storage fleet when the current (Amps) at a"
	           + "monitored element is below a specified amps target (kWTarget_low). The Storage will charge as much power as necessary to keep the amps within the deadband around kWTarget_low. "
	           + "When this control mode is active, the property kWTarget_low will be expressed in k-amps and all the other parameters will be adjusted to match the amps (current) control criteria.";
	PropertyHelp[propTIMEDISCHARGETRIGGER - 1] = "Default time of day (hr) for initiating Discharging of the fleet. During Follow or Time mode discharging is triggered at a fixed time "
	           "each day at this hour. If Follow mode, Storage will be discharged to attempt to hold the load at or below the power level at the time of triggering. "
	           "In Time mode, the discharge is based on the %RatekW property value. "
	           "Set this to a negative value to ignore. Default is 12.0 for Follow mode; otherwise it is -1 (ignored). ";
	PropertyHelp[propTIMECHARGETRIGGER - 1] = "Default time of day (hr) for initiating charging in Time control mode. Set this to a negative value to ignore. Default is 2.0.  (0200)."
	           "When this value is >0 the Storage fleet is set to charging at this time regardless of other control criteria to make sure Storage is "
	           "topped off for the next discharge cycle.";
	PropertyHelp[propRATEKW - 1] = "Sets the kW discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode, SCHEDULE mode, or anytime discharging is triggered "
	           "by time.";
//    PropertyHelp[propRATEKVAR]            :=
//      'Sets the kvar discharge rate in % of rated capacity for each element of the fleet. Applies to TIME control mode or anytime discharging is triggered ' +
//      'by time.' ;
	PropertyHelp[propRATECHARGE - 1] = "Sets the kW charging rate in % of rated capacity for each element of the fleet. Applies to TIME control mode and anytime charging mode is "
	           "entered due to a time trigger.";
	PropertyHelp[propRESERVE - 1] = "Use this property to change the % reserve for each Storage element under control of this controller. This might be used, for example, to "
	           "allow deeper discharges of Storage or in case of emergency operation to use the remainder of the Storage element.";
	PropertyHelp[propKWHTOTAL - 1] = "(Read only). Total rated kWh energy Storage capacity of Storage elements controlled by this controller.";
	PropertyHelp[propKWTOTAL - 1] = "(Read only). Total rated kW power capacity of Storage elements controlled by this controller.";
	PropertyHelp[propKWHACTUAL - 1] = "(Read only). Actual kWh stored of all controlled Storage elements. ";
	PropertyHelp[propKWACTUAL - 1] = "(Read only). Actual kW output of all controlled Storage elements. ";
	PropertyHelp[propKWNEED - 1] = "(Read only). KW needed to meet target.";
//    PropertyHelp[propPARTICIPATION]       :=
//      'Participation factor, %. Default = 100.';
	PropertyHelp[propYEARLY - 1] = "Dispatch loadshape object, If any, for Yearly solution Mode.";
	PropertyHelp[propDAILY - 1] = "Dispatch loadshape object, If any, for Daily solution mode.";
	PropertyHelp[propDUTY - 1] = "Dispatch loadshape object, If any, for Dutycycle solution mode.";
	PropertyHelp[propEVENTLOG - 1] = "{Yes/True | No/False} Default is No. Log control actions to Eventlog.";
//    PropertyHelp[propVARDISPATCH]         :=
//      '{Yes/True | No/False} Default is No. Flag to indicate whether or not to disatch vars as well as watts.';
	PropertyHelp[propINHIBITTIME - 1] = "Hours (integer) to inhibit Discharging after going into Charge mode. Default is 5.";
	PropertyHelp[propTUPRAMP - 1] = "Duration, hrs, of upramp part for SCHEDULE mode. Default is 0.25.";
	PropertyHelp[propTFLAT - 1] = "Duration, hrs, of flat part for SCHEDULE mode. Default is 2.0.";
	PropertyHelp[propTDNRAMP - 1] = "Duration, hrs, of downramp part for SCHEDULE mode. Default is 0.25.";
	PropertyHelp[propKWTHRESHOLD - 1] = "Threshold, kW, for Follow mode. kW has to be above this value for the Storage element "
	           "to be dispatched on. Defaults to 75% of the kWTarget value. Must reset this property after "
	           "setting kWTarget if you want a different value.";
	PropertyHelp[propDispFactor - 1] = String("Defaults to 1 (disabled). Set to any value between 0 and 1 to enable this parameter.") + CRLF
	           + CRLF
	           + "Use this parameter to reduce the amount of power requested by the controller in each control iteration. "
	           + "It can be useful when maximum control iterations are exceeded due to numerical instability such as "
	           + "fleet being set to charging and idling in subsequent control iterations (check the Eventlog). ";
	PropertyHelp[propRESETLEVEL - 1] = "The level of charge required for allowing the storage to discharge again after reaching "
	           "the reserve storage level. After reaching this level, the storage control  will not allow "
	           "the storage device to discharge, forcing the storage to charge. Once the storage reaches this"
	           "level, the storage will be able to discharge again. This value is a number between 0.2 and 1";
	PropertyHelp[propSEASONS - 1] = "With this property the user can"
	           " specify the number of targets to be used by the controller using the list given at \"SeasonTargets\"/"
	           "\"SeasonTargetsLow\", which can be used to dynamically adjust the storage controller during a QSTS"
	           " simulation. The default value is 1. This property needs to be defined before defining SeasonTargets/SeasonTargetsLow.";
	PropertyHelp[propSEASONTARGETS - 1] = "An array of doubles specifying the targets to be used during a QSTS simulation. These targets will take effect"
	           " only if SeasonRating=true. The number of targets cannot exceed the number of seasons defined at the SeasonSignal."
	           "The difference between the targets defined at SeasonTargets and SeasonTargetsLow is that SeasonTargets"
	           " applies to discharging modes, while SeasonTargetsLow applies to charging modes.";
	PropertyHelp[propSEASONTARGETSLOW - 1] = "An array of doubles specifying the targets to be used during a QSTS simulation. These targets will take effect"
	           " only if SeasonRating=true. The number of targets cannot exceed the number of seasons defined at the SeasonSignal."
	           "The difference between the targets defined at SeasonTargets and SeasonTargetsLow is that SeasonTargets"
	           " applies to discharging modes, while SeasonTargetsLow applies to charging modes.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TStorageController::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new StorageController and add it to StorageController class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TStorageControllerObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TStorageController::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int i = 0;
	double casemult = 0.0;

  // continue parsing with contents of Parser
	ActiveStorageController2Obj = (TStorageControllerObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveStorageController2Obj);
	result = 0;
	/*# with ActiveStorageController2Obj do */
	{
		auto with0 = ActiveStorageController2Obj;
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
	           + "\"", 14407);
				break;
				case 	propELEMENT:
				with0->ElementName = LowerCase(Param);
				break;
				case 	propTERMINAL:
				with0->ElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	propMONPHASE:
				if(CompareTextShortest(Param, "avg") == 0)
					with0->FMonPhase = AVG;
				else
				{
					if(CompareTextShortest(Param, "max") == 0)
						with0->FMonPhase = MAXPHASE;
					else
					{
						if(CompareTextShortest(Param, "min") == 0)
							with0->FMonPhase = MINPHASE;
						else
							with0->FMonPhase = max(1, Parser[ActorID]->MakeInteger_());
					}
				}
				break;
				case 	propKWTARGET:
				with0->FkWTarget = Parser[ActorID]->MakeDouble_();
				break;
				case 	propKWTARGETLOW:
				with0->FkWTargetLow = Parser[ActorID]->MakeDouble_();
				break;
				case 	propPCTKWBAND:
				with0->FpctkWBand = Parser[ActorID]->MakeDouble_();
				break;
				case 	propKWBAND:
				with0->FkWBand = Parser[ActorID]->MakeDouble_();
				break;
				case 	propPCTKWBANDLOW:
				with0->FpctkWBandLow = Parser[ActorID]->MakeDouble_();
				break;
				case 	propKWBANDLOW:
				with0->FkWBandLow = Parser[ActorID]->MakeDouble_();
				break;
//            propPFTARGET: FPFTarget        := ConvertPFToPFRange2(Parser[ActorID].MakeDouble_());
//            propPFBAND:   FPFBand          := Parser[ActorID].MakeDouble_();
				case 	propELEMENTLIST:
				InterpretTStringListArray(Param, with0->FStorageNameList);
				break;
				case 	propWEIGHTS:
				{
					with0->FleetSize = with0->FStorageNameList.size();
					if(with0->FleetSize > 0)
					{
						with0->FWeights = (pDoubleArray) realloc(with0->FWeights, sizeof(double) * with0->FleetSize);
						with0->FleetSize = InterpretDblArray(Param, with0->FleetSize, with0->FWeights);
					}
				}
				break;
				case 	propMODEDISCHARGE:
				with0->DischargeMode = with0->InterpretMode(propMODEDISCHARGE, Param);
				break;
				case 	propMODECHARGE:
				with0->ChargeMode = with0->InterpretMode(propMODECHARGE, Param);
				break;
				case 	propTIMEDISCHARGETRIGGER:
				with0->DischargeTriggerTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	propTIMECHARGETRIGGER:
				with0->ChargeTriggerTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	propRATEKW:
				with0->pctKWRate = Parser[ActorID]->MakeDouble_();
				break;
//            propRATEKVAR:    pctkvarRate    := Parser[ActorID].MakeDouble_();
				case 	propRATECHARGE:
				with0->pctChargeRate = Parser[ActorID]->MakeDouble_();
				break;
				case 	propRESERVE:
				with0->pctFleetReserve = Parser[ActorID]->MakeDouble_();
				break;
				case 	propKWHTOTAL:
				;
				break;  // Do nothing (Read ONly)
				case 	propKWTOTAL:
				;
				break;  // Do nothing (Read ONly)
				case 	propKWHACTUAL:
				;
				break;  // Do nothing (Read ONly)
				case 	propKWACTUAL:
				;
				break;  // Do nothing (Read ONly)
				case 	propKWNEED:
				;
				break;  // Do nothing (Read ONly)
//            propPARTICIPATION: ;
				case 	propYEARLY:
				with0->YearlyShape = Param;
				break;
				case 	propDAILY:
				with0->DailyShape = Param;
				break;
				case 	propDUTY:
				with0->DutyShape = Param;
				break;
				case 	propEVENTLOG:
				with0->ShowEventLog = InterpretYesNo(Param);
				break;
//            propVARDISPATCH: DispatchVars := InterpretYesNo(Param);
				case 	propINHIBITTIME:
				with0->InhibitHrs = max(1, Parser[ActorID]->MakeInteger_());
				break;  // >=1
				case 	propTUPRAMP:
				with0->UpRamptime = Parser[ActorID]->MakeDouble_();
				break;
				case 	propTFLAT:
				with0->FlatTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	propTDNRAMP:
				with0->DnrampTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	propKWTHRESHOLD:
				with0->FkWThreshold = Parser[ActorID]->MakeDouble_();
				break;
				case 	propDispFactor:
				if((Parser[ActorID]->MakeDouble_() > 1.0) || (Parser[ActorID]->MakeDouble_() <= 0.0))
					with0->DispFactor = 1.0;
				else
					with0->DispFactor = Parser[ActorID]->MakeDouble_();
				break;
				case 	propRESETLEVEL:
				with0->ResetLevel = Parser[ActorID]->MakeDouble_();
				break;
				case 	propSEASONS:
				with0->Seasons = Parser[ActorID]->MakeInteger_();
				break;
				case 	propSEASONTARGETS:
				{
					if(with0->Seasons > 1)
					{
						with0->SeasonTargets.resize( with0->Seasons );
						with0->Seasons = InterpretDblArray(Param, with0->Seasons, ((pDoubleArray) &(with0->SeasonTargets[0])));
					}
				}
				break;
				case 	propSEASONTARGETSLOW:
				{
					if(with0->Seasons > 1)
					{
						with0->SeasonTargetsLow.resize(with0->Seasons);
						with0->Seasons = InterpretDblArray(Param, with0->Seasons, ((pDoubleArray) &( with0->SeasonTargetsLow[0])));
					}
				}
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveStorageController2Obj, ParamPointer - NumPropsThisClass);
				break;
			}

         // Side effects of setting properties above
			switch(ParamPointer)
			{
				case 	propKWTARGET:
				{
					if(with0->DischargeMode == CURRENTPEAKSHAVE)  // evaluates the discharging mode to apply
                 // a compensation value (for kamps)
						casemult = 1000.0;
					else
						casemult = 1.0;
					with0->FkWThreshold = with0->FkWTarget * 0.75 * casemult;
					with0->HalfkWBand = with0->FpctkWBand / 200.0 * with0->FkWTarget * casemult;
					with0->FkWBand = 2.0 * with0->HalfkWBand;
					with0->FpctkWBand = with0->FkWBand / with0->FkWTarget * 100.0; // sync FpctkWBand
				}
				break;
				case 	propPCTKWBAND:
				{
					if(with0->DischargeMode == CURRENTPEAKSHAVE)  // evaluates the discharging mode to apply
                 // a compensation value (for kamps)
						casemult = 1000.0;
					else
						casemult = 1.0;
					with0->HalfkWBand = with0->FpctkWBand / 200.0 * with0->FkWTarget * casemult;
					with0->FkWBand = 2.0 * with0->HalfkWBand;
					with0->FkWBandSpecified = false;
				}
				break;
				case 	propKWBAND:
				{
					if(with0->DischargeMode == CURRENTPEAKSHAVE)  // evaluates the discharging mode to apply
                 // a compensation value (for kamps)
						casemult = 1000.0;
					else
						casemult = 1.0;
					with0->HalfkWBand = with0->FkWBand / 2.0 * casemult;
					with0->FpctkWBand = with0->FkWBand / with0->FkWTarget * 100.0; // sync FpctkWBand
					with0->FkWBandSpecified = true;
				}
				break;
				case 	propKWTARGETLOW: case propPCTKWBANDLOW:
				{
					if(with0->ChargeMode == CURRENTPEAKSHAVELOW)  // evaluates the charging mode to apply
                 // a compensation value (for kamps)
						casemult = 1000.0;
					else
						casemult = 1.0;
					with0->HalfkWBandLow = with0->FpctkWBandLow / 200.0 * with0->FkWTargetLow * casemult;
					with0->FkWBandLow = with0->HalfkWBandLow * 2.0;
				}
				break;
				case 	propKWBANDLOW:
				{
					if(with0->ChargeMode == CURRENTPEAKSHAVELOW)  // evaluates the charging mode to apply
                 // a compensation value (for kamps)
						casemult = 1000.0;
					else
						casemult = 1.0;
					with0->HalfkWBandLow = with0->FkWBandLow / 2.0 * casemult;
					with0->FpctkWBand = with0->FkWBandLow / with0->FkWTarget * 100.0; // sync FpctkWBandLow
				}
				break;
//            propPFBAND: HalfPFBand := FPFBand / 2.0;
				case 	propMODEDISCHARGE:
				if(with0->DischargeMode == MODEFOLLOW)
					with0->DischargeTriggerTime = 12.0;
				break; // Noon
				case 	propMONPHASE:
				if(with0->FMonPhase > with0->Fnphases)
				{
					DoSimpleMsg(Format("Error: Monitored phase(%d) must be less than or equal to number of phases(%d). ", with0->FMonPhase, with0->Fnphases), 35302);
					with0->FMonPhase = 1;
				}
				break;   // levelize the list
				case 	propELEMENTLIST:
				{
					int stop = 0;
					with0->FleetPointerList->Clear();  // clear this for resetting on first sample
					with0->FleetListChanged = true;
					with0->FElementListSpecified = true;
					with0->FleetSize = with0->FStorageNameList.size();
                // Realloc weights to be same size as possible number of storage elements
					with0->FWeights = (pDoubleArray) realloc(with0->FWeights, sizeof(double) * with0->FleetSize);
					for(stop = with0->FleetSize, i = 1; i <= stop; i++)
					{
						(with0->FWeights)[i - 1] = 1.0;
					}
				}
				break;
				case 	propYEARLY:
				{
					with0->YearlyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->YearlyShape));
					if(with0->YearlyShapeObj == nullptr)
						DoSimpleMsg(String("Yearly loadshape \"") + with0->YearlyShape + "\" not found.", 14404);
				}
				break;
				case 	propDAILY:
				{
					with0->DailyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DailyShape));
					if(with0->DailyShapeObj == nullptr)
						DoSimpleMsg(String("Daily loadshape \"") + with0->DailyShape + "\" not found.", 14405);
				}
				break;
				case 	propDUTY:
				{
					with0->DutyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DutyShape));
					if(with0->DutyShapeObj == nullptr)
						DoSimpleMsg(String("Dutycycle loadshape \"") + with0->DutyShape + "\" not found.", 14406);
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

int TStorageController::MakeLike(const String StorageController2Name)
{
	int result = 0;
	TStorageControllerObj* OtherStorageController2 = nullptr;
	int i = 0;
	result = 0;
   /*See If we can find this StorageController name in the present collection*/
	OtherStorageController2 = ((TStorageControllerObj*) Find(StorageController2Name));
	if(OtherStorageController2 != nullptr)
		/*# with ActiveStorageController2Obj do */
		{
			auto with0 = ActiveStorageController2Obj;
			int stop = 0;
			with0->Set_NPhases(OtherStorageController2->Fnphases);
			with0->Set_Nconds(OtherStorageController2->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherStorageController2->ElementName;
			with0->Set_ControlledElement(OtherStorageController2->get_FControlledElement());  // Pointer to target circuit element
			with0->Set_MonitoredElement(OtherStorageController2->get_FMonitoredElement());  // Pointer to target circuit element
			with0->ElementTerminal = OtherStorageController2->ElementTerminal;
			with0->FMonPhase = OtherStorageController2->FMonPhase;
			with0->CondOffset = OtherStorageController2->CondOffset;
			with0->FkWTarget = OtherStorageController2->FkWTarget;
			with0->FkWTargetLow = OtherStorageController2->FkWTargetLow;
			with0->FkWThreshold = OtherStorageController2->FkWThreshold;
			with0->DispFactor = OtherStorageController2->DispFactor;
			with0->FpctkWBand = OtherStorageController2->FpctkWBand;
			with0->FkWBand = OtherStorageController2->FkWBand;
			with0->FpctkWBandLow = OtherStorageController2->FpctkWBandLow;
			with0->FkWBandLow = OtherStorageController2->FkWBandLow;
//        FPFTarget             := OtherStorageController.FPFTarget;
//        FPFBand               := OtherStorageController.FPFBand;
//        HalfPFBand            := OtherStorageController.HalfPFBand;
			with0->ResetLevel = OtherStorageController2->ResetLevel;
			with0->FkWBandSpecified = OtherStorageController2->FkWBandSpecified;
			with0->FStorageNameList.clear();
			for(stop = OtherStorageController2->FStorageNameList.size(), i = 1; i <= stop; i++)
			{
				with0->FStorageNameList.push_back(OtherStorageController2->FStorageNameList[i - 1]);
			}
			with0->FleetSize = with0->FStorageNameList.size();
			if(with0->FleetSize > 0)
			{
				int stop = 0;
				with0->FWeights = (pDoubleArray) realloc(with0->FWeights, sizeof(double) * with0->FleetSize);
				for(stop = with0->FleetSize, i = 1; i <= stop; i++)
				{
					(with0->FWeights)[i - 1] = (OtherStorageController2->FWeights)[i - 1];
				}
			}
			with0->DischargeMode = OtherStorageController2->DischargeMode;
			with0->ChargeMode = OtherStorageController2->ChargeMode;
			with0->DischargeTriggerTime = OtherStorageController2->DischargeTriggerTime;
			with0->ChargeTriggerTime = OtherStorageController2->ChargeTriggerTime;
			with0->pctKWRate = OtherStorageController2->pctKWRate;
//        pctkvarRate           := OtherStorageController.pctkvarRate;
			with0->pctChargeRate = OtherStorageController2->pctChargeRate;
			with0->pctFleetReserve = OtherStorageController2->pctFleetReserve;
			with0->YearlyShape = OtherStorageController2->YearlyShape;
			with0->DailyShape = OtherStorageController2->DailyShape;
			with0->DutyShape = OtherStorageController2->DutyShape;
//        DispatchVars          := OtherStorageController.DispatchVars;
			with0->ShowEventLog = OtherStorageController2->ShowEventLog;
			with0->InhibitHrs = OtherStorageController2->InhibitHrs;
			with0->UpRamptime = OtherStorageController2->UpRamptime;
			with0->FlatTime = OtherStorageController2->FlatTime;
			with0->DnrampTime = OtherStorageController2->DnrampTime;
			with0->Seasons = OtherStorageController2->Seasons;
			if(with0->Seasons > 1)
			{
				int stop = 0;
				with0->SeasonTargets.resize(with0->Seasons);
				with0->SeasonTargetsLow.resize(with0->Seasons);
				for(stop = (with0->Seasons - 1), i = 0; i <= stop; i++)
				{
					with0->SeasonTargets[i] = OtherStorageController2->SeasonTargets[i];
					with0->SeasonTargetsLow[i] = OtherStorageController2->SeasonTargetsLow[i];
				}
			}


        //**** fill in private properties
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				switch(i)
				{
           // Skip Read only properties
					case 	propKWHTOTAL:
					;
					break; /*Do Nothing*/
					case 	propKWTOTAL:
					;
					break; /*Do Nothing*/
					case 	propKWHACTUAL:
					;
					break; /*Do Nothing*/
					case 	propKWACTUAL:
					;
					break; /*Do Nothing*/
					case 	propKWNEED:
					;
					break; /*Do Nothing*/
					default:
					with0->Set_PropertyValue(i,OtherStorageController2->Get_PropertyValue(i));
					break;
				}
			}
		}
	else
		DoSimpleMsg(String("Error in StorageController2 MakeLike: \"") + StorageController2Name
	           + "\" Not Found.", 370);
	return result;
}




/*==========================================================================*/
/*                    TStorageControllerObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TStorageControllerObj::TStorageControllerObj(TDSSClass* ParClass, const String StorageController2Name)
 : inherited(ParClass),
			FkWTarget(8000.0),
			FkWTargetLow(4000.0),
			FkWThreshold(6000.0),
			FpctkWBand(2.0),
			//FkWBand(HalfkWBand * 2.0),
			FkWBand(0.0),
			FpctkWBandLow(0.0),
			FkWBandLow(0.0),
			HalfkWBand(0.0),
			HalfkWBandLow(0.0),
			TotalWeight(0.0),
			UpRamptime(0.0),
			FlatTime(0.0),
			DnrampTime(0.0),
			UpPlusFlat(0.0),
			UpPlusFlatPlusDn(0.0),
			DischargeTriggerTime(0.0),
			ChargeTriggerTime(0.0),
			pctKWRate(0.0),
			pctChargeRate(0.0),
			LastpctDischargeRate(0.0),
			TotalkWCapacity(0.0),
			TotalkWhCapacity(0.0),
			pctFleetReserve(0.0),
			ResetLevel(0.0),
			kWNeeded(0.0),
			DispFactor(0.0),
			FleetPointerList(nullptr),
			FleetListChanged(false),
			ChargingAllowed(false),
			DischargeTriggeredByTime(false),
			DischargeInhibited(false),
			OutOfOomph(false),
			FElementListSpecified(false),
			Wait4Step(false),
			FkWBandSpecified(false),
			Seasons(0),
			FleetSize(0),
			FleetState(0),
			DischargeMode(0),
			InhibitHrs(0),
			ChargeMode(0),
			FMonPhase(0),
			CondOffset(0),
			YearlyShapeObj(nullptr),
			DailyShapeObj(nullptr),
			DutyShapeObj(nullptr)
{
	Set_Name(LowerCase(StorageController2Name));
	FStorageNameList.clear();
	FWeights = NULL;
	cBuffer.clear(); // Complex buffer
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
	ElementName = "";
	Set_ControlledElement(nullptr);    // not used in this control
	ElementTerminal = 1;
	Set_MonitoredElement(nullptr);
	FMonPhase = MAXPHASE;
	FleetPointerList = new PointerList::TPointerList(20);  // Default size and increment
	FleetSize = 0;
	FleetState = STORE_IDLING;
	DispFactor = 1.0;
	FpctkWBandLow = 2.0;
	HalfkWBand = FpctkWBand / 200.0 * FkWTarget;
	HalfkWBandLow = FpctkWBandLow / 200.0 * FkWTargetLow;
	FkWBandLow = HalfkWBandLow * 2.0;
	TotalWeight = 1.0;

//     FPFTarget                := 0.96;
//     FPFBand                  := 0.04;
//     HalfPFBand               := FPFBand / 2.0;
	kWNeeded = 0.0;
	DischargeMode = MODEPEAKSHAVE;
	ChargeMode = MODETIME;
	DischargeTriggerTime = -1.0;  // disabled
	ChargeTriggerTime = 2.0;   // 2 AM
	FElementListSpecified = false;
	FleetListChanged = true;  // force building of list
	FkWBandSpecified = false;  // adopt pctkWBand by default
	pctKWRate = 20.0;
//     pctkvarRate              := 20.0;
	pctChargeRate = 20.0;
	pctFleetReserve = 25.0;
//     DispatchVars             := FALSE;
	DischargeTriggeredByTime = false;
	DischargeInhibited = false;
	OutOfOomph = false;
	InhibitHrs = 5;   // No. Hours to inhibit discharging after going into charge mode
	UpRamptime = 0.25; // hr
	FlatTime = 2.0;
	DnrampTime = 0.25;
	LastpctDischargeRate = 0.0;
	Wait4Step = false;     // for sync discharge with charge when there is a transition
	ResetLevel = 0.8;
	Seasons = 1;         // For dynamic targets
	SeasonTargets.resize( 1 );
	SeasonTargets[0] = FkWTarget;
	SeasonTargetsLow.resize( 1 );
	SeasonTargetsLow[0] = FkWTargetLow;
	InitPropertyValues(0);
}

TStorageControllerObj::~TStorageControllerObj()
{
	ElementName = "";
	YearlyShape = "";
	DailyShape = "";
	DutyShape = "";
	if(!cBuffer.empty())
		cBuffer.clear();

/*    Don't Do this here!! Disposes of actual object;
       YearlyShapeObj.Free;
       DailyShapeObj.Free;
       DutyShapeObj.Free;
*/
	delete FleetPointerList; // FleetPointerList->~TPointerList();
	FStorageNameList.clear();
	// inherited::Destroy();
}


//----------------------------------------------------------------------------

void TStorageControllerObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(propELEMENT,"");
	Set_PropertyValue(propTERMINAL,"1");
	Set_PropertyValue(propMONPHASE,"MAX");
	Set_PropertyValue(propKWTARGET,"8000");
	Set_PropertyValue(propKWTARGETLOW,"4000");
	Set_PropertyValue(propPCTKWBAND,"2");
	Set_PropertyValue(propPCTKWBANDLOW,"2");
//     Set_PropertyValue(propPFTARGET]             :='.96');
//     Set_PropertyValue(propPFBAND]               :='.04');
	Set_PropertyValue(propELEMENTLIST,"");
	Set_PropertyValue(propWEIGHTS,"");
	Set_PropertyValue(propMODEDISCHARGE,"Follow");
	Set_PropertyValue(propMODECHARGE,"Time");
	Set_PropertyValue(propTIMEDISCHARGETRIGGER,"-1");
	Set_PropertyValue(propTIMECHARGETRIGGER,"2");
	Set_PropertyValue(propRATEKW,"20");
//     Set_PropertyValue(propRATEKVAR]             :='20');
	Set_PropertyValue(propRATECHARGE,"20");
	Set_PropertyValue(propRESERVE,"25");
	Set_PropertyValue(propKWHTOTAL,"");
	Set_PropertyValue(propKWTOTAL,"");
	Set_PropertyValue(propKWACTUAL,"");
	Set_PropertyValue(propKWNEED,"");
//     Set_PropertyValue(propPARTICIPATION]        :='');
	Set_PropertyValue(propYEARLY,"");
	Set_PropertyValue(propDAILY,"");
	Set_PropertyValue(propDUTY,"");
	Set_PropertyValue(propEVENTLOG, ShowEventLog ? "YES" : "NO");
	Set_PropertyValue(propINHIBITTIME,"5");
	Set_PropertyValue(propTUPRAMP,"0.25");
	Set_PropertyValue(propTFLAT,"2.0");
	Set_PropertyValue(propTDNRAMP,"0.25");
	Set_PropertyValue(propKWTHRESHOLD,"4000");
	Set_PropertyValue(propDispFactor,"1.0");
	Set_PropertyValue(propRESETLEVEL,"0.8");
	Set_PropertyValue(propSEASONS,"1");
	Set_PropertyValue(propSEASONTARGETS,"[8000,]");
	Set_PropertyValue(propSEASONTARGETSLOW,"[4000,]");
	inherited::InitPropertyValues(NumPropsThisClass);
}

String TStorageControllerObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	propMONPHASE:
		if(FMonPhase == AVG)
			result = "AVG";
		else
		{
			if(FMonPhase == MAXPHASE)
				result = "MAX";
			else
			{
				if(FMonPhase == MINPHASE)
					result = "MIN";
				else
					result = Format("%d", FMonPhase);
			}
		}
		break;
		case 	propKWTARGET:
		result = Format("%-.6g", FkWTarget);
		break;
		case 	propKWTARGETLOW:
		result = Format("%-.6g", FkWTargetLow);
		break;
		case 	propPCTKWBAND:
		result = Format("%-.6g", FpctkWBand);
		break;
		case 	propKWBAND:
		result = Format("%-.6g", FkWBand);
		break;
		case 	propPCTKWBANDLOW:
		result = Format("%-.6g", FpctkWBandLow);
		break;
		case 	propKWBANDLOW:
		result = Format("%-.6g", FkWBandLow);
		break;
//          propPFTARGET             : Result := Format('%-.6g',[ConvertPFRange2ToPF(FPFTarget)]);
//          propPFBAND               : Result := Format('%-.6g',[FPFBand]);
		case 	propELEMENTLIST:
		result = ReturnElementsList();
		break;
		case 	propWEIGHTS:
		result = ReturnWeightsList();
		break;
		case 	propMODEDISCHARGE:
		result = GetModeString(propMODEDISCHARGE, DischargeMode);
		break;
		case 	propMODECHARGE:
		result = GetModeString(propMODECHARGE, ChargeMode);
		break;
		case 	propTIMEDISCHARGETRIGGER:
		result = Format("%.6g", DischargeTriggerTime);
		break;
		case 	propTIMECHARGETRIGGER:
		result = Format("%.6g", ChargeTriggerTime);
		break;
		case 	propRATEKW:
		result = Format("%-.8g", pctKWRate);
		break;
//          propRATEKVAR             : Result := Format('%-.8g',[pctkvarRate]);
		case 	propRATECHARGE:
		result = Format("%-.8g", pctChargeRate);
		break;
		case 	propRESERVE:
		result = Format("%-.8g", pctFleetReserve);
		break;
		case 	propKWHTOTAL:
		result = GetkWhTotal(TotalkWhCapacity);
		break;
		case 	propKWTOTAL:
		result = GetkWTotal(TotalkWCapacity);
		break;
		case 	propKWHACTUAL:
		result = GetkWhActual();
		break;
		case 	propKWACTUAL:
		result = GetkWActual();
		break;
		case 	propKWNEED:
		result = Format("%-.6g", kWNeeded);
		break;
          /*propPARTICIPATION        : Result := PropertyValue[Index]; */
		case 	propYEARLY:
		result = YearlyShape;
		break;
		case 	propDAILY:
		result = DailyShape;
		break;
		case 	propDUTY:
		result = DutyShape;
		break;
		case 	propEVENTLOG:
		if(ShowEventLog)
			result = "Yes";
		else
			result = "No";
		break;
//          propVARDISPATCH          : If DispatchVars Then Result := 'Yes' Else Result := 'No';
		case 	propINHIBITTIME:
		result = Format("%d", InhibitHrs);
		break;
		case 	propTUPRAMP:
		result = Format("%.6g", UpRamptime);
		break;
		case 	propTFLAT:
		result = Format("%.6g", FlatTime);
		break;
		case 	propTDNRAMP:
		result = Format("%.6g", DnrampTime);
		break;
		case 	propKWTHRESHOLD:
		result = Format("%.6g", FkWThreshold);
		break;
		case 	propDispFactor:
		result = Format("%.6g", DispFactor);
		break;
		case 	propRESETLEVEL:
		result = Format("%.6g", ResetLevel);
		break;
		case 	propSEASONS:
		result = Format("%d", Seasons);
		break;
		case 	propSEASONTARGETS:
		result = ReturnSeasonTarget(1);
		break;
		case 	propSEASONTARGETSLOW:
		result = ReturnSeasonTarget(0);
		break;  // take the generic handler
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

double TStorageControllerObj::Get_FleetkW()
{
	double result = 0.0;
	TStorageObj* pStorage = nullptr;
	int i = 0;
	int stop = 0;
	result = 0.0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		pStorage = (TStorageObj*) FleetPointerList->Get(i);
		result = result + pStorage->Get_PresentkW();
	}
	return result;
}

double TStorageControllerObj::Get_FleetkWh()
{
	double result = 0.0;
	TStorageObj* pStorage = nullptr;
	int i = 0;
	int stop = 0;
	result = 0.0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		pStorage = (TStorageObj*) FleetPointerList->Get(i);
		result = result + pStorage->StorageVars.kWhStored;
	}
	return result;
}

double TStorageControllerObj::Get_FleetkWhRating()
{
	double result = 0.0;
	TStorageObj* pStorage = nullptr;
	int i = 0;
	int stop = 0;
	result = 0.0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		pStorage = (TStorageObj*) FleetPointerList->Get(i);
		result = result + pStorage->StorageVars.kWhRating;
	}
	return result;
}

double TStorageControllerObj::Get_FleetReservekWh()
{
	double result = 0.0;
	TStorageObj* pStorage = nullptr;
	int i = 0;
	int stop = 0;
	result = 0.0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		pStorage = (TStorageObj*) FleetPointerList->Get(i);
		result = result + pStorage->StorageVars.kWhReserve;
	}
	return result;
}

/*--------------------------------------------------------------------------*/

// Recalculate critical element values after changes have been made

void TStorageControllerObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;

        /*Check for existence of monitored element*/
	DevIndex = GetCktElementIndex(ElementName); // Global FUNCTION
	if(DevIndex > 0)
	{
		Set_MonitoredElement((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		if(ElementTerminal > get_FMonitoredElement()->Get_NTerms())
		{
			DoErrorMsg(String("StorageController2: \"") + get_Name() + "\"", "Terminal no. \"" "\" Does not exist.", "Re-specify terminal no.", 371);
		}
		else
		{
			Set_NPhases(get_FMonitoredElement()->Get_NPhases());
			Set_Nconds(Fnphases);

                 // Sets name of i-th terminal's connected bus in StorageController's buslist
			SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));

                 // Allocate a buffer bigenough to hold everything from the monitored element
			cBuffer.resize(get_FMonitoredElement()->Yorder + 1);
			CondOffset = (ElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
		}
	}
	else
	DoSimpleMsg(String("Monitored Element in StorageController2.") + get_Name()
	           + " Does not exist:\""
	           + ElementName
	           + "\"", 372);
	if(FleetListChanged)
	{
		if(!MakeFleetList())
			DoSimpleMsg(String("No unassigned Storage Elements found to assign to StorageController2.") + get_Name(), 37201);
	}
	GetkWTotal(TotalkWCapacity);
	GetkWhTotal(TotalkWhCapacity);
	if(FleetSize > 0)
	{
		SetFleetToExternal();
		SetAllFleetValues();
	}
	UpPlusFlat = UpRamptime + FlatTime;
	UpPlusFlatPlusDn = UpPlusFlat + DnrampTime;
}

void TStorageControllerObj::MakePosSequence(int ActorID)
{
	if(get_FMonitoredElement() != nullptr)
	{
		Set_NPhases(get_FMonitoredElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
		cBuffer.resize(get_FMonitoredElement()->Yorder + 1);
		CondOffset = (ElementTerminal - 1) * get_FMonitoredElement()->Get_NConds(); // for speedy sampling
	}
	inherited::MakePosSequence(ActorID);
}

/*--------------------------------------------------------------------------*/

void TStorageControllerObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}


/*--------------------------------------------------------------------------*/

void TStorageControllerObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TStorageControllerObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

String TStorageControllerObj::GetkWActual()
{
	String result;
	result = Format("%-.8g", Get_FleetkW());
	return result;
}

String TStorageControllerObj::GetkWhActual()
{
	String result;
	result = Format("%-.8g", Get_FleetkWh());
	return result;
}

String TStorageControllerObj::GetkWhTotal(double& Sum)
{
	String result;
	TStorageObj* pStorage = nullptr;
	int i = 0;
	int stop = 0;
	Sum = 0.0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		pStorage = (TStorageObj*) FleetPointerList->Get(i);
		Sum = Sum + pStorage->StorageVars.kWhRating;
	}
	result = Format("%-.8g", Sum);
	return result;
}

String TStorageControllerObj::GetkWTotal(double& Sum)
{
	String result;
	TStorageObj* pStorage = nullptr;
	int i = 0;
	int stop = 0;
	Sum = 0.0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		pStorage = (TStorageObj*) FleetPointerList->Get(i);
		Sum = Sum + pStorage->StorageVars.kWrating;
	}
	result = Format("%-.8g", Sum);
	return result;
}

String TStorageControllerObj::GetModeString(int Opt, int Mode)
{
	String result;
	result = "";
	switch(Opt)
	{
		case 	propMODEDISCHARGE:
		switch(Mode)
		{
			case 	MODEFOLLOW:
			result = "Follow";
			break;
			case 	MODELOADSHAPE:
			result = "Loadshape";
			break;
			case 	MODESUPPORT:
			result = "Support";
			break;
			case 	MODETIME:
			result = "Time";
			break;
			case 	MODEPEAKSHAVE:
			result = "Peakshave";
			break;
			case 	CURRENTPEAKSHAVE:
			result = "I-Peakshave";
			break;
			default:
			result = "UNKNOWN";
			break;
		}
		break;
		case 	propMODECHARGE:
		switch(Mode)
		{
			case 	MODELOADSHAPE:
                   // 1: Result := 'Follow';
			result = "Loadshape";
			break;
                  //  3: Result := 'Support';
			case 	MODETIME:
			result = "Time";
			break;
			case 	MODEPEAKSHAVELOW:
			result = "PeakshaveLow";
			break;
			case 	CURRENTPEAKSHAVELOW:
			result = "I-PeakShaveLow";
			break;
			default:
			result = "UNKNOWN";
			break;
		}
		break;
		default:
		DoSimpleMsg("Unknown Charge/Discharge designation", 14401);
		break;
	}
	return result;
}

/*--------------------------------------------------------------------------*/

void TStorageControllerObj::DumpProperties(TTextRec& f, bool Complete)
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

/*--------------------------------------------------------------------------*/

void TStorageControllerObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{


        /*
           Release  the discharge inhibit .
           Do nothing for other codes
        */
	if((Code == RELEASE_INHIBIT) && (DischargeMode != MODEFOLLOW))
		DischargeInhibited = false;
}
/*
  In SCHEDULE mode we ramp up the storage from zero to the specified pctkWRate.
  This value is held for the flattime or until they  turn themselves
  off when they are either fully discharged, or ramped down

  The discharge trigger time must be greater than 0
*/

void TStorageControllerObj::DoScheduleMode(int ActorID)
{
	double TDiff = 0.0;
	double pctDischargeRate = 0.0;
	pctDischargeRate = 0.0;   // init for test
	if(DischargeTriggerTime > 0.0)
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
               // turn on if time within 1/2 time step
			if(!(FleetState == STORE_DISCHARGING))
			{
				ChargingAllowed = true;
				TDiff = NormalizeToTOD(with0->DynaVars.intHour, with0->DynaVars.T) - DischargeTriggerTime;
				if(Abs( TDiff) < with0->DynaVars.h / 7200.0)
                        /*Time is within 1 time step of the trigger time*/
				{
					if(ShowEventLog)
						AppendToEventLog(String("StorageController2.") + this->get_Name(), "Fleet Set to Discharging (up ramp) by Schedule", ActorID);
					SetFleetToDisCharge();
					SetFleetDesiredState(STORE_DISCHARGING);
					ChargingAllowed = false;
					pctDischargeRate = min(pctKWRate, max(pctKWRate * TDiff / UpRamptime, 0.0));
					SetFleetkWRate(pctDischargeRate);
					DischargeInhibited = false;
					PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);
				}
			}
			else
    // fleet is already discharging
			{
				TDiff = NormalizeToTOD(with0->DynaVars.intHour, with0->DynaVars.T) - DischargeTriggerTime;
				if(TDiff < UpRamptime)
				{
					pctDischargeRate = min(pctKWRate, max(pctKWRate * TDiff / UpRamptime, 0.0));
					SetFleetDesiredState(STORE_DISCHARGING);
					if(pctDischargeRate != LastpctDischargeRate)
					{
						SetFleetkWRate(pctDischargeRate);
						SetFleetToDisCharge();
					}
				}
				else
				{
					if(TDiff < UpPlusFlat)
					{
						pctDischargeRate = pctKWRate;
						SetFleetDesiredState(STORE_DISCHARGING);
						if(pctDischargeRate != LastpctDischargeRate)
							SetFleetkWRate(pctKWRate);  // on the flat part
					}
					else
					{
						if(TDiff > UpPlusFlatPlusDn)
						{
							SetFleetToIdle();
							ChargingAllowed = true;
							pctDischargeRate = 0.0;
							if(ShowEventLog)
								AppendToEventLog(String("StorageController2.") + this->get_Name(), "Fleet Set to Idling by Schedule", ActorID);
						}
						else
  // We're on the down ramp
						{
							TDiff = UpPlusFlatPlusDn - TDiff;
							pctDischargeRate = max(0.0, min(pctKWRate * TDiff / DnrampTime, pctKWRate));
							SetFleetDesiredState(STORE_DISCHARGING);
							SetFleetkWRate(pctDischargeRate);
						}
					}
				}
				if(pctDischargeRate != LastpctDischargeRate)
					PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);
			}  /*If not fleetstate ...*/
		}
	LastpctDischargeRate = pctDischargeRate;   // remember this value
}
/*
  In Time mode we need to only turn the storage elements on. They will turn themselves
  off when they are either fully discharged, fully charged, or receive another command
  from the controller
*/

void TStorageControllerObj::DoTimeMode(int Opt, int ActorID)
{
	double RemainingkWh = 0.0;
	double ReservekWh = 0.0;
	double TotalRatingkWh = 0.0;
	TotalRatingkWh = Get_FleetkWhRating();
	RemainingkWh = Get_FleetkWh();
	ReservekWh = Get_FleetReservekWh();
	switch(Opt)
	{
		case 	1:
		{
			if(DischargeTriggerTime > 0.0)
				/*# with ActiveCircuit[ActorID].Solution do */
				{
					auto with0 = ActiveCircuit[ActorID]->Solution;
                 // turn on if time within 1/2 time step
					if(Abs( (NormalizeToTOD(with0->DynaVars.intHour, with0->DynaVars.T) - DischargeTriggerTime)) < with0->DynaVars.h / 7200.0)
					{
						SetFleetDesiredState(STORE_DISCHARGING);
						if(!(FleetState == STORE_DISCHARGING) && (RemainingkWh > ReservekWh))
                        /*Time is within 1 time step of the trigger time*/
						{
							if(ShowEventLog)
								AppendToEventLog(String("StorageController2.") + this->get_Name(), "Fleet Set to Discharging by Time Trigger", ActorID);
							SetFleetToDisCharge();
							SetFleetkWRate(pctKWRate);
							DischargeInhibited = false;
							if(DischargeMode == MODEFOLLOW)
								DischargeTriggeredByTime = true;
							else
								PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);
						}
					}
					else
					ChargingAllowed = true;
				}
		}
		break; // Discharge mode
		case 	2:
		{
			if(ChargeTriggerTime > 0.0)
				/*# with ActiveCircuit[ActorID].Solution do */
				{
					auto with1 = ActiveCircuit[ActorID]->Solution;
					if(Abs( (NormalizeToTOD(with1->DynaVars.intHour, with1->DynaVars.T) - ChargeTriggerTime)) < with1->DynaVars.h / 7200.0)
					{
						SetFleetDesiredState(STORE_CHARGING);
						if(!(FleetState == STORE_CHARGING) && (RemainingkWh < TotalRatingkWh))
                          /*Time is within 1 time step of the trigger time*/
						{
							if(ShowEventLog)
								AppendToEventLog(String("StorageController2.") + this->get_Name(), "Fleet Set to Charging by Time Trigger", ActorID);
							SetFleetToCharge();
							DischargeInhibited = true;
							OutOfOomph = false;
							PushTimeOntoControlQueue(STORE_CHARGING, ActorID);   // force re-solve at this time step
                          // Push message onto control queue to release inhibit at a later time
							/*# with ActiveCircuit[ActorID] do */
							{
								
								ActiveCircuit[ActorID]->Solution->LoadsNeedUpdating = true; // Force recalc of power parms
								ActiveCircuit[ActorID]->ControlQueue.Push(with1->DynaVars.intHour + InhibitHrs, with1->DynaVars.T, (EControlAction) RELEASE_INHIBIT, 0, this, ActorID);
							}
						}
					}
				}
		}
		break; //Charge mode
		default:
		  ;
		break;
	}
}

//----------------------------------------------------------------------------

// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 23.999999....

double TStorageControllerObj::NormalizeToTOD(int h, double Sec)
{
	double result = 0.0;
	int HourOfDay = 0;
	if(h > 23)
		HourOfDay = (h - ((h / 24)) * 24);
	else
		HourOfDay = h;
	result = HourOfDay + Sec / 3600.0;
	if(result >= 24.0)
		result = result - 24.0;   // Wrap around
	return result;
}
/*
   Push present time onto control queue to force re solve at new dispatch value
*/

void TStorageControllerObj::PushTimeOntoControlQueue(int Code, int ActorID)
{
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
	{
		
		auto with1 = ActiveCircuit[ActorID]->Solution;
		with1->LoadsNeedUpdating = true; // Force recalc of power parms
		ActiveCircuit[ActorID]->ControlQueue.Push(with1->DynaVars.intHour, with1->DynaVars.T, (EControlAction) Code, 0, this, ActorID);
	}
}

/*--------------------------------------------------------------------------*/

double TStorageControllerObj::Get_DynamicTarget(int THigh, int ActorID)
{
	double result = 0.0;
	double Temp = 0.0;
	double temp2 = 0.0;
	int RatingIdx = 0;
	TXYcurveObj* RSignal = nullptr;
	if(SeasonSignal != "")
	{
		RSignal = ((TXYcurveObj*) XYCurveClass[ActorID]->Find(SeasonSignal));
		if(RSignal != nullptr)
			RatingIdx = Trunc(RSignal->GetYValue_((double) ActiveCircuit[ActorID]->Solution->DynaVars.intHour));
		if((RatingIdx <= Seasons) && (Seasons > 1))
		{
			if(THigh == 1)
				result = SeasonTargets[RatingIdx];
			else
				result = SeasonTargetsLow[RatingIdx];
		}
		else
		{
			if(THigh == 1)
				result = FkWTarget;
			else
				result = FkWTargetLow;
		}
	}
	return result;
}


/*--------------------------------------------------------------------------*/

void TStorageControllerObj::DoLoadFollowMode(int ActorID)
{
	int i = 0;
	complex s = {};
	TStorageObj* StorageObj = nullptr;
	bool StorekWChanged = false;
	bool StorekvarChanged = false;
	bool SkipkWDispatch = false;
	pComplexArray VoltsArr;
//   PFDiff,

//   Dispatchkvar,
	double kWhActual = 0.0;
	double ElemVolts = 0.0;
	double Amps = 0.0;
	double AmpsDiff = 0.0;
	double PDiff = 0.0;
	double DispatchkW = 0.0;
	double RemainingkWh = 0.0;
	double CtrlTarget = 0.0;
	double ReservekWh = 0.0;
	double ActualkWDispatch = 0.0;

     // If list is not defined, go make one from all storage elements in circuit
	if(FleetPointerList->get_myNumList() == 0)
		MakeFleetList();
	if(FleetSize > 0)
	{
		StorekWChanged = false;
		StorekvarChanged = false;
		SkipkWDispatch = false;

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
		if(DischargeMode == CURRENTPEAKSHAVE)
		{
			get_FMonitoredElement()->GetCurrents(&(cBuffer[0]), ActorID);
			GetControlCurrent(Amps);

//          Amps := MonitoredElement.MaxCurrent[ElementTerminal,ActorID]; // Max current in active terminal  // old
		}
		else
		GetControlPower(s, ActorID);

       // In case of having seasonal targets
		if(SeasonalRating)
			CtrlTarget = Get_DynamicTarget(1, ActorID);
		else
			CtrlTarget = FkWTarget;
		switch(DischargeMode)
		{
			case 	MODEFOLLOW:
             // Following Load; try to keep load below kW Target
			{
				if(DischargeTriggeredByTime)
				{
					if(ShowEventLog)
						AppendToEventLog(String("StorageController.") + this->get_Name(), Format("Fleet Set to Discharging by Time Trigger; Old kWTarget = %-.6g; New = %-.6g", FkWTarget, s.re * 0.001), ActorID);
					FkWTarget = max(FkWThreshold, s.re * 0.001);  // Capture present kW and reset target
					if(!FkWBandSpecified)
						HalfkWBand = FpctkWBand / 200.0 * FkWTarget;  // Update band to new target if absolute kWBand hasn`t been specified
					DischargeTriggeredByTime = false;  // so we don't come back in here right away
					SetFleetToIdle();
					SetFleetDesiredState(STORE_IDLING);
				}
				PDiff = s.re * 0.001 - FkWTarget;  // Assume S.re is normally positive
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
			}
			break;
             // supporting DG; Try to keep load above kW target
			case 	MODESUPPORT:
			{
				PDiff = s.re * 0.001 + FkWTarget;  // assume S.re is normally negative
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for generator
			}
			break;
			case 	MODEPEAKSHAVE:
			{
				PDiff = s.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
//                                  PFDiff        := ConvertPFToPFRange2(PowerFactor(S)) - FPFTarget;  // for peak shaving
			}
			break;
			case 	CURRENTPEAKSHAVE:
			{
				PDiff = Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
//                                  DispatchVars  :=  False;
			}
			break;
			default:
			PDiff = 0.0;
			break;
		}
		if(DischargeMode == CURRENTPEAKSHAVE)    // convert Pdiff from Amps to kW
		{
			get_FMonitoredElement()->ComputeVterminal(ActorID);
			VoltsArr = &(get_FMonitoredElement()->Vterminal[0]);
			ElemVolts = cabs(VoltsArr[1 - 1]);
			kWNeeded = ((get_FMonitoredElement()->Get_NPhases() * PDiff * ElemVolts) / 1000.0);
//         kWNeeded     :=  ((Pdiff * ElemVolts) / 1000.0);
			AmpsDiff = PDiff;
		}
		else

//         kWNeeded := Pdiff + FleetkW;
		kWNeeded = PDiff;

       /*  kW dispatch  */

       // Check if Fleet is Idling (FleetState is updated only if entire fleet is idling)
		if(!(FleetState == STORE_IDLING))
		{
			int stop = 0;
			for(stop = FleetSize, i = 1; i <= stop; i++)
			{
				StorageObj = (TStorageObj*) FleetPointerList->Get(i);
				if(StorageObj->get_fState() != STORE_IDLING)
					break;
				if(i == FleetSize)
					FleetState = STORE_IDLING;
			}
		}
		if(DischargeInhibited)
			SkipkWDispatch = true;
		else

//         If FleetState = STORE_CHARGING Then
//                   Begin
//                     if Not (Dischargemode = CURRENTPEAKSHAVE) then
//                      Pdiff :=  Pdiff + FleetkW  // ignore overload due to charging
//                     else
//                     Begin
//                       MonitoredElement.ComputeVterminal(ActorID);
//                       VoltsArr     :=  MonitoredElement.Vterminal;
//                       ElemVolts    :=  cabs(VoltsArr^[1]);
//                       Pdiff        :=  Pdiff + (FleetkW * 1000 / ElemVolts);
//                     End;
//                   end;

//           If Not (FleetState = STORE_DISCHARGING) Then  // ignore overload due to charging or idling (trickle charging) - FleetkW < 0
		{
			if(FleetState == STORE_CHARGING)
			{
				if(!(DischargeMode == CURRENTPEAKSHAVE))
					PDiff = PDiff + Get_FleetkW();
				else
				{
					get_FMonitoredElement()->ComputeVterminal(ActorID);
					VoltsArr = &(get_FMonitoredElement()->Vterminal[0]);
					ElemVolts = cabs(VoltsArr[1 - 1]);
					PDiff = PDiff + (Get_FleetkW() * 1000 / (ElemVolts * get_FMonitoredElement()->Get_NPhases()));
//                 Pdiff        :=  Pdiff + (FleetkW * 1000 / (ElemVolts ));
				}
			}
			switch(FleetState)
			{
				case 	STORE_CHARGING: case STORE_IDLING:
				if((PDiff - HalfkWBand < 0.0) || OutOfOomph)  // Don't bother trying to dispatch
				{
					ChargingAllowed = true;
					SkipkWDispatch = true;
					if(OutOfOomph)  // --------------------------------- new 04/20/2020 ----------
					{
						int stop = 0;
						for(stop = FleetSize, i = 1; i <= stop; i++)
						{
							StorageObj = (TStorageObj*) FleetPointerList->Get(i);
							kWhActual = StorageObj->StorageVars.kWhStored / StorageObj->StorageVars.kWhRating;
							OutOfOomph = OutOfOomph && (kWhActual >= ResetLevel);  // If we have more than the 80% we are good to dispatch
						}
						OutOfOomph = !OutOfOomph;  // If everybody in the fleet has at least the 80% of the storage capacity full
					}    // -----------------------------------------------------------------------
				}
				break;

 /*               STORE_DISCHARGING: If (PDiff < 0.0) or OutOfOomph Then
                  Begin   // desired decrease is greater then present output; just cancel
                        SetFleetToIdle;   // also sets presentkW = 0
                        PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
                        ChargingAllowed := TRUE;
                        SkipkWDispatch  := TRUE;
                        Wait4Step       := TRUE; // To tell to the charging section to wait for the next sim step
                                                 // useful when workin with large simulation time steps
                  End;*/
				default:
				  ;
				break;
			}
		}
		if(!SkipkWDispatch)
		{
			RemainingkWh = Get_FleetkWh();
			ReservekWh = Get_FleetReservekWh();
			if(RemainingkWh > ReservekWh)
               //  don't dispatch kW  if not enough storage left or an endless control loop will occur
			{
				if(Abs( PDiff) > HalfkWBand) // Attempt to change storage dispatch
				{
					int stop = 0;
					if(!(FleetState == STORE_DISCHARGING))
					{
						SetFleetToDisCharge();
//                    StorekWChanged:= TRUE;  // if not already discharging, force new power flow.
					}
					if(ShowEventLog)
						AppendToEventLog(String("StorageController.") + this->get_Name(), Format("Attempting to dispatch %-.6g kW with %-.6g kWh remaining and %-.6g kWh reserve.", kWNeeded, RemainingkWh, ReservekWh), ActorID);
					for(stop = FleetSize, i = 1; i <= stop; i++)
					{
						StorageObj = (TStorageObj*) FleetPointerList->Get(i);
						if(DischargeMode == CURRENTPEAKSHAVE) // Current to power
    //  (MonitoredElement.MaxVoltage[ElementTerminal,ActorID] / 1000)
						{
							if( ( (TDSSCktElement*) StorageObj )->Get_NPhases() == 1)
								kWNeeded = StorageObj->Get_PresentkV() * AmpsDiff;
							else

//                          kWNeeded :=  StorageObj.PresentkV * InvSQRT3 * AmpsDiff;
								kWNeeded = StorageObj->Get_PresentkV() * SQRT3 * AmpsDiff;
						}
						/*# with StorageObj do */
						{
							auto with0 = StorageObj;
                      // compute new dispatch value for this storage element ...
							DispatchkW = min(with0->StorageVars.kWrating, (with0->Get_PresentkW() + kWNeeded * DispFactor * (FWeights[i - 1] / TotalWeight))); // Dispatch kWNeeded
							if(DispatchkW <= 0.0) // if kWNeeded is too low, DispatchkW may be negative depending on idling losses. In this case, just set it to idling
							{
								with0->Set_StorageState(STORE_IDLING);  // overrides SetFleetToDischarge
								if(Abs( with0->Get_PresentkW()) - StorageObj->kWOutIdling > EPSILON)  // if not already idling
								{
									with0->SetNominalStorageOutput(ActorID);
									ActualkWDispatch = with0->Get_PresentkW();
									StorekWChanged = true; // if not idling at first, force a new powerflow
									if(ShowEventLog)
										AppendToEventLog(String("StorageController.") + this->get_Name(), Format(("Requesting " + StorageObj->Get_QualifiedName()
	           + " to dispatch %-.6g kW. Setting "
	           + StorageObj->Get_QualifiedName()
	           + " to idling state. Final kWOut is %-.6g kW").c_str(), DispatchkW, ActualkWDispatch), ActorID);
//                          DispatchkW := 0.0;
								}
							}
							else
							{
								if(double(Abs( (with0->Get_kW() - DispatchkW))) / Abs( DispatchkW) > 0.0001) // redispatch only if change requested
								{
									if(DispatchkW < max(with0->Get_CutInkWAC(), with0->Get_CutOutkWAC()))   // Necessary check to avoid the control to go into an infinite loop when DispatchkW is less than CutOutkWAC
									{
										if(with0->Get_InverterON() == true)  // request Dispatch only if the inverter is on (only once).

                                // Next time, the inverter will be OFF and the control won't dispatch a new power
										{
											if(with0->StorageVars.kWhStored > with0->StorageVars.kWhReserve)
											{
												with0->Set_kW(DispatchkW);
												with0->SetNominalStorageOutput(ActorID);
												ActualkWDispatch = with0->Get_PresentkW();
												StorekWChanged = true;     // This is what keeps the control iterations going
												if(ShowEventLog)
													AppendToEventLog(String("StorageController.") + this->get_Name(),
													Format(("Requesting " + StorageObj->Get_QualifiedName()
													+ " to dispatch %-.6g kW, less than CutIn/CutOut."
													+ " Final kWOut is %-.6g kW").c_str(), DispatchkW, ActualkWDispatch), ActorID);
											}
										}
										else


                                      // if inverter is already off, just override discharging state to
                                      // idling and update current kvarlimit for usage by InvControl
										{
											with0->Set_StorageState(STORE_IDLING);     // overrides SetFleetToDischarge
											with0->SetNominalStorageOutput(ActorID); // to update current kvarLimit
											ActualkWDispatch = with0->Get_PresentkW();
											if(ShowEventLog)
												AppendToEventLog(String("StorageController.") + this->get_Name(),
												Format(("Requesting " + StorageObj->Get_QualifiedName()
												+ " to dispatch %-.6g kW, less than CutIn/CutOut."
												+ " Inverter is OFF. Final kWOut is %-.6g kW").c_str(), DispatchkW, ActualkWDispatch), ActorID);
										}
									}
									else
									{
										if(with0->StorageVars.kWhStored > with0->StorageVars.kWhReserve)  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
										{
											with0->Set_kW(DispatchkW);
											with0->SetNominalStorageOutput(ActorID);
											ActualkWDispatch = with0->Get_PresentkW();
											StorekWChanged = true;     // This is what keeps the control iterations going
											if(ShowEventLog)
												AppendToEventLog(String("StorageController.") + this->get_Name(),
												Format(("Requesting " + StorageObj->Get_QualifiedName()
												+ " to dispatch %-.6g kW. Final kWOut is %-.6g kW").c_str(), DispatchkW, ActualkWDispatch), ActorID);
										}
									}
								}
							}
						}
					}
				}
			}
			else
			{
				if(!(FleetState == STORE_IDLING))
				{
					SetFleetToIdle();
					PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
				}
				ChargingAllowed = true;
				OutOfOomph = true;
				if(ShowEventLog)
					AppendToEventLog(String("StorageController.") + this->get_Name(),
					Format("Ran out of OOMPH: %-.6g kWh remaining and %-.6g reserve. Fleet has been set to idling state.",
					RemainingkWh, ReservekWh), ActorID);
			}
		}
		if(StorekWChanged || StorekvarChanged)  // Only push onto controlqueue If there has been a change
			PushTimeOntoControlQueue(STORE_DISCHARGING, ActorID);

       /*Else just continue*/
	}
}

/*--------------------------------------------------------------------------*/
	// This is the peakShaving mode for controlling the charging operation of the storage fleet
	// The objective is to charge the storage fleet when the power at a monitored element is bellow a specified KW target (kWTarget_low)
	// The storage will charge as much power as necessary to keet the power within the deadband around kWTarget_low

  // WILL NOT IMPLEMENT REACTIVE POWER CONTROL FOR NOW

void TStorageControllerObj::DoPeakShaveModeLow(int ActorID)
{
	int i = 0;
	complex s = {};
	pComplexArray VoltsArr = nullptr;
	TStorageObj* StorageObj = nullptr;
	bool StorekWChanged = false;
	bool SkipkWCharge = false;
	double ElemVolts = 0.0;
	double PDiff = 0.0;
	double kWNeeded = 0.0;
	double Amps = 0.0;
	double AmpsDiff = 0.0;
	double ChargekW = 0.0;
	double ActualkWh = 0.0;
	double ActualkW = 0.0;
	double TotalRatingkWh = 0.0;
	double KwtoPercentagekW = 0.0;
	double CtrlTarget = 0.0;
	double ActualkWDispatch = 0.0;
     // If list is not defined, go make one from all storage elements in circuit
	if(FleetPointerList->get_myNumList() == 0)
		MakeFleetList();

//     If (FleetSize>0) And(Not(FleetState = STORE_DISCHARGING)) Then
	if(FleetSize > 0)
	{
		StorekWChanged = false;
		SkipkWCharge = false;
		if(SeasonalRating)
			CtrlTarget = Get_DynamicTarget(0, ActorID);
		else
			CtrlTarget = FkWTargetLow;


       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
		if(ChargeMode == CURRENTPEAKSHAVELOW)
		{
			get_FMonitoredElement()->GetCurrents(&(cBuffer[0]), ActorID);
			GetControlCurrent(Amps);
//         Amps         := MonitoredElement.MaxCurrent[ElementTerminal,ActorID]; // Max current in active terminal
			PDiff = Amps - CtrlTarget * 1000;  // Gets the difference in terms of amps
		}
		else
		{
			GetControlPower(s, ActorID);
			PDiff = s.re * 0.001 - CtrlTarget;  // Assume S.re is normally positive
		}
		ActualkW = Get_FleetkW();
		ActualkWh = Get_FleetkWh();
		TotalRatingkWh = Get_FleetkWhRating();
		if(ChargeMode == CURRENTPEAKSHAVELOW)   // convert Pdiff from Amps to kW
		{
			get_FMonitoredElement()->ComputeVterminal(ActorID);
			VoltsArr = &(get_FMonitoredElement()->Vterminal[0]);
			ElemVolts = cabs((VoltsArr)[1 - 1]);     // LN voltage
			kWNeeded = ((get_FMonitoredElement()->Get_NPhases() * PDiff * ElemVolts) / 1000.0);
//         kWNeeded     :=  (( PDiff * ElemVolts) / 1000.0);
			AmpsDiff = PDiff;
		}
		else

//         kWNeeded := Pdiff + FleetkW;
		kWNeeded = PDiff;


        // Check if Fleet is Idling (FleetState is updated only if entire fleet is idling)
		if(!(FleetState == STORE_IDLING))
		{
			int stop = 0;
			for(stop = FleetSize, i = 1; i <= stop; i++)
			{
				StorageObj = (TStorageObj*) FleetPointerList->Get(i);
				if(StorageObj->get_fState() != STORE_IDLING)
					break;
				if(i == FleetSize)
					FleetState = STORE_IDLING;
			}
		}

//
//           CASE  FleetState of
//                STORE_CHARGING,
//                STORE_IDLING: If (PDiff < 0.0) or OutOfOomph Then
//                  Begin  // Don't bother trying to dispatch
//                       ChargingAllowed  := TRUE;
//                       SkipkWDispatch   := TRUE;
//                  End;
//           END;


//       If Not (FleetState = STORE_CHARGING) Then  // ignore underload due to discharging  (FleetkW > 0) and discount idlings losses (may delay the charging)
		if(FleetState == STORE_DISCHARGING)
		{
			if(!(ChargeMode == CURRENTPEAKSHAVELOW))
				PDiff = PDiff + Get_FleetkW();
			else
			{
				get_FMonitoredElement()->ComputeVterminal(ActorID);
				VoltsArr = &(get_FMonitoredElement()->Vterminal[0]);
				ElemVolts = cabs((VoltsArr)[1 - 1]);
				PDiff = PDiff + (Get_FleetkW() * 1000 / (ElemVolts * get_FMonitoredElement()->Get_NPhases()));   // get actual Pdiff in Currents (discount FleetkW)  (assuming same number of phases of Fleet and Monitored Element)
//                 Pdiff        :=  Pdiff + (FleetkW * 1000 / (ElemVolts ));
			}
		}
		switch(FleetState)
		{
			case 	STORE_DISCHARGING:
			 case STORE_IDLING:
			if((PDiff > 0.0) || (ActualkWh >= TotalRatingkWh) || Wait4Step)  // Don't bother trying to charge
			{
				ChargingAllowed = false;
				SkipkWCharge = true;
				Wait4Step = false;
//                        End;
//          STORE_CHARGING: If (kWNeeded > 0.0) or (ActualkWh>=TotalRatingkWh) // old approach
//          STORE_CHARGING: If (Pdiff + FleetkW > 0.0) or (ActualkWh >= TotalRatingkWh) Then
//                          Begin   // desired decrease (in absolute value) is greater than present output; just cancel
//                                SetFleetToIdle;   // also sets presentkW = 0
//                                PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
//                                ChargingAllowed := FALSE;
//                                SkipkWCharge  := TRUE;
//                          End;
			}
			break;
			default:
			  ;
			break;
		}
		if(!SkipkWCharge)
		{
			if(ActualkWh < TotalRatingkWh)
               //  don't dispatch kW  if fully charged or an endless control loop will occur
			{
				if(Abs( PDiff) > HalfkWBandLow) // Attempt to change storage kW charge
				{
					int stop = 0;
					if(!(FleetState == STORE_CHARGING))
					{
						SetFleetToCharge();
//                      StorekWChanged:= TRUE;  // if not already charging, force new power flow.
					}
  //                       If ShowEventLog Then  AppendToEventLog('StorageController.' + Self.Name, Format('Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.', [kWNeeded, (TotalRatingkWh-ActualkWh), TotalRatingkWh]), ActorID);
					if(ShowEventLog)
						AppendToEventLog(String("StorageController.") + this->get_Name(),
						Format("Attempting to charge %-.6g kW with %-.6g kWh remaining and %-.6g rating.", 
						kWNeeded, (TotalRatingkWh - ActualkWh), TotalRatingkWh), ActorID);
					for(stop = FleetSize, i = 1; i <= stop; i++)
					{
						StorageObj = (TStorageObj*) FleetPointerList->Get(i);
						/*# with StorageObj do */
						{
							auto with0 = StorageObj;

                        // Checks if PDiff needs to be adjusted considering the charging mode
							if(ChargeMode == CURRENTPEAKSHAVELOW)
							{
								if( ( (TDSSCktElement*) StorageObj )->Get_NPhases() == 1)
									kWNeeded = StorageObj->Get_PresentkV() * AmpsDiff;
								else

//                           kWNeeded :=  StorageObj.PresentkV * InvSQRT3 * AmpsDiff;
									kWNeeded = StorageObj->Get_PresentkV() * InvSQRT3 * AmpsDiff;
							}


                       // compute new charging value for this storage element ...
  //                                ChargekW := -1 * Min(StorageVars.kWrating, abs(Get_PresentkW() + Pdiff *(FWeights^[i]/TotalWeight)));  // old approach
							ChargekW = with0->Get_PresentkW() + kWNeeded * ((FWeights)[i - 1] / TotalWeight) * DispFactor; // may be positive or negative
							if(ChargekW < 0)
								ChargekW = max(-1 * with0->StorageVars.kWrating, ChargekW); // check against kVA rating
							if(ChargekW >= 0) // chargekW may be positive if increase in demand is too high.
							{
								with0->Set_StorageState(STORE_IDLING);  // overrides SetFleetToDischarge
								if(Abs( with0->Get_PresentkW()) - StorageObj->kWOutIdling > EPSILON)  // if not already idling
								{
									with0->SetNominalStorageOutput(ActorID);
									ActualkWDispatch = with0->Get_PresentkW();
									StorekWChanged = true; // if not idling at first, force a new powerflow
									if(ShowEventLog)
										AppendToEventLog(String("StorageController.") + this->get_Name(),
										Format(("Requesting " + StorageObj->Get_QualifiedName()
										+ " to dispatch %-.6g kW. Setting "
										+ StorageObj->Get_QualifiedName()
										+ " to idling state. Final kWOut is %-.6g kW").c_str(), ChargekW, ActualkWDispatch), ActorID);
								}
							}
							else

                          //                       If ChargekW <> PresentkW Then    // do only if change requested
							{
								if(Abs( (StorageObj->Get_kW() - ChargekW)) / Abs(ChargekW) > 0.0001)    // do only if change requested
								{
									if(Abs( ChargekW) < max(with0->Get_CutInkWAC(), with0->Get_CutOutkWAC()))   // Necessary check to avoid the control to go into an infinite loop when ChargekW is less than CutOutkWAC
									{
										if(with0->Get_InverterON() == true)  // request Dispatch only if the inverter is on (only once).

                                // Next time the inverter will be OFF and the control won't dispatch a new power
										{
											if(with0->StorageVars.kWhStored > with0->StorageVars.kWhReserve)
											{
												with0->Set_kW(ChargekW);
												with0->SetNominalStorageOutput(ActorID);
												ActualkWDispatch = with0->Get_PresentkW();
												StorekWChanged = true;     // This is what keeps the control iterations going
												if(ShowEventLog)
													AppendToEventLog(String("StorageController.") + this->get_Name(),
													Format(("Requesting " + StorageObj->Get_QualifiedName()
													+ " to dispatch %-.6g kW, less than CutIn/CutOut."
													+ " Final kWOut is %-.6g kW").c_str(), ChargekW, ActualkWDispatch), ActorID);
											}
										}
										else


                                      // if inverter is already off, just override discharging state to
                                      // idling and update current kvarlimit for usage by InvControl
										{
											with0->Set_StorageState(STORE_IDLING);     // overrides SetFleetToCharge
											with0->SetNominalStorageOutput(ActorID); // to update current kvarLimit
											ActualkWDispatch = with0->Get_PresentkW();
											if(ShowEventLog)
												AppendToEventLog(String("StorageController.") + this->get_Name(),
												Format(("Requesting " + StorageObj->Get_QualifiedName()
												+ " to dispatch %-.6g kW, less than CutIn/CutOut."
												+ " Inverter is OFF. Final kWOut is %-.6g kW").c_str(), ChargekW, ActualkWDispatch), ActorID);
										}
									}
									else
									{
										if(with0->StorageVars.kWhStored < with0->StorageVars.kWhRating)  // Attempt to set discharge kW;  Storage element will revert to idling if out of capacity
                                     //StorageObj.PresentkW  :=  ChargekW;
										{
											with0->Set_kW(ChargekW);
											with0->SetNominalStorageOutput(ActorID);
											ActualkWDispatch = with0->Get_PresentkW();
        //                                           KwtoPercentagekW := (ChargekW*100) / StorageVars.kWrating;  // old approach
        //                                           StorageObj.pctkWin := abs(KwtoPercentagekW);                // old approach
											StorekWChanged = true;     // This is what keeps the control iterations going
											if(ShowEventLog)
												AppendToEventLog(String("StorageController.") + this->get_Name(),
												Format(("Requesting " + StorageObj->Get_QualifiedName()
												+ " to dispatch %-.6g kW. Final kWOut is %-.6g kW").c_str(), ChargekW, ActualkWDispatch), ActorID);
										}
									}
								}
							}
						}
					}
				}
			}
			else
			{
				if(!(FleetState == STORE_IDLING))
				{
					SetFleetToIdle();
					PushTimeOntoControlQueue(STORE_IDLING, ActorID);  // force a new power flow solution
				}
				ChargingAllowed = false;
				if(ShowEventLog)
					AppendToEventLog(String("StorageController.") + this->get_Name(),
					Format("Fully charged: %-.6g kWh of rated %-.6g.", ActualkWh, TotalRatingkWh), ActorID);
			}
		}
		if(StorekWChanged)  // Only push onto controlqueue If there has been a change
			PushTimeOntoControlQueue(STORE_CHARGING, ActorID);
       /*Else just continue*/
	}
}

/*--------------------------------------------------------------------------*/

void TStorageControllerObj::sample(int ActorID)
{
	ChargingAllowed = false;
//       UpdateFleetState;
/*
  Check discharge mode first. Then if not discharging, we can check for charging
*/
	Wait4Step = false;        // Initializes the variable for the new control step
	switch(DischargeMode)
	{
		case 	MODEFOLLOW:
		{
			DoTimeMode(1, ActorID);
			DoLoadFollowMode(ActorID);
		}
		break;
		case 	MODELOADSHAPE:
		DoLoadShapeMode(ActorID);
		break;
		case 	MODESUPPORT:
		DoLoadFollowMode(ActorID);
		break;
		case 	MODETIME:
		DoTimeMode(1, ActorID);
		break;
		case 	MODEPEAKSHAVE:
		DoLoadFollowMode(ActorID);
		break;
		case 	CURRENTPEAKSHAVE:
		DoLoadFollowMode(ActorID);
		break;
		case 	MODESCHEDULE:
		DoScheduleMode(ActorID);
		break;
		default:
		DoSimpleMsg(Format("Invalid DisCharging Mode: %d", DischargeMode), 14408);
		break;
	}
	if(ChargingAllowed)
		switch(ChargeMode)
		{
			case 	MODELOADSHAPE:
			;
			break; // DoLoadShapeMode;  already executed above
			case 	MODETIME:
			DoTimeMode(2, ActorID);
			break;
			case 	MODEPEAKSHAVELOW:
			DoPeakShaveModeLow(ActorID);
			break;
			case 	CURRENTPEAKSHAVELOW:
			DoPeakShaveModeLow(ActorID);
			break;
			default:
			DoSimpleMsg(Format("Invalid Charging Mode: %d", ChargeMode), 14409);
			break;
		}
}


//----------------------------------------------------------------------------

void TStorageControllerObj::CalcDailyMult(double hr)
{
	if(DailyShapeObj != nullptr)
	{
		LoadShapeMult = DailyShapeObj->GetMult(hr);
	}
	else
	LoadShapeMult = CDoubleOne;  // Default to no  variation
}


//----------------------------------------------------------------------------

void TStorageControllerObj::CalcDutyMult(double hr)
{
	if(DutyShapeObj != nullptr)
	{
		LoadShapeMult = DutyShapeObj->GetMult(hr);
	}
	else
	CalcDailyMult(hr);  // Default to Daily Mult If no duty curve specified
}

//----------------------------------------------------------------------------

void TStorageControllerObj::CalcYearlyMult(double hr)
{
	if(YearlyShapeObj != nullptr)
	{
		LoadShapeMult = YearlyShapeObj->GetMult(hr);
	}
	else
	CalcDailyMult(hr);  // Defaults to Daily curve
}

//----------------------------------------------------------------------------

void TStorageControllerObj::DoLoadShapeMode(int ActorID)
{
	int FleetStateSaved = 0;
	bool RateChanged = false;
	double NewChargeRate = 0.0;
	double NewkWRate = 0.0;
	double NewkvarRate = 0.0;
	FleetStateSaved = FleetState;
	RateChanged = false;

    // Get multiplier
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		switch(with0->Get_SolMode())
		{
			case 	DAILYMODE:
			CalcDailyMult(with0->DynaVars.dblHour);
			break; // Daily dispatch curve
			case 	YEARLYMODE:
			CalcYearlyMult(with0->DynaVars.dblHour);
			break;
			case 	LOADDURATION2:
			CalcDailyMult(with0->DynaVars.dblHour);
			break;
			case 	PEAKDAY:
			CalcDailyMult(with0->DynaVars.dblHour);
			break;
			case 	DUTYCYCLE:
			CalcDutyMult(with0->DynaVars.dblHour);
			break;
			default:
			  ;
			break;
		}
	}
	if(LoadShapeMult.re < 0.0)
	{
		ChargingAllowed = true;
		NewChargeRate = Abs( LoadShapeMult.re) * 100.0;
		SetFleetDesiredState(STORE_CHARGING);
		if(NewChargeRate != pctChargeRate)
		{
			RateChanged = true;
			pctChargeRate = NewChargeRate;
			SetFleetChargeRate();
			SetFleetToCharge();
		}
	}
	else
	{
		if(LoadShapeMult.re == 0.0)
			SetFleetToIdle();
		else
   // Set fleet to discharging at a rate
		{
			NewkWRate = LoadShapeMult.re * 100.0;
//           NewkvarRate := LoadShapeMult.im * 100.0;
			SetFleetDesiredState(STORE_DISCHARGING);

//           If (NewkWRate <> pctkWRate) or (NewkvarRate <> pctkvarRate) then
			if(NewkWRate != pctKWRate)
           // only set rate if it has changed. otherwise the debugtrace report will not report kWOut correctly.
			{
				RateChanged = true;
				pctKWRate = NewkWRate;
//              pctkvarRate := NewkvarRate;
				SetFleetkWRate(pctKWRate);
//              SetFleetkvarRate(pctkvarRate);
				SetFleetToDisCharge();
				ActiveCircuit[ActorID]->Solution->LoadsNeedUpdating = true; // Force recalc of power parms
			}
		}

    /*Force a new power flow solution if fleet state has changed*/
	}
	if((FleetState != FleetStateSaved) || RateChanged)
		PushTimeOntoControlQueue(0, ActorID);
}

//----------------------------------------------------------------------------

void TStorageControllerObj::SetAllFleetValues()
{
	int i = 0;
	int stop = 0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		/*# with TStorageObj(FleetPointerList.Get(i)) do */
		{
			auto with0 = ((TStorageObj*) FleetPointerList->Get(i));
			with0->Set_pctkWIn(pctChargeRate);
//              Fpctkvarout := pctkvarRate;  CR
			with0->Set_pctkWOut(pctKWRate);
			with0->pctReserve = pctFleetReserve;
		}
	}
}

//----------------------------------------------------------------------------

void TStorageControllerObj::SetFleetChargeRate()
{
	int i = 0;
	int stop = 0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		((TStorageObj*) FleetPointerList->Get(i))->Set_pctkWIn(pctChargeRate);
	}
}

//----------------------------------------------------------------------------
//PROCEDURE TStorageControllerObj.SetFleetkvarRate;
//VAR
//      i   :Integer;
//Begin
//    {For side effects see pctkvarout property of Storage element}
////      For i := 1 to FleetPointerList->get_myNumList() Do
////            TStorageObj(FleetPointerList.Get(i)).pctkvarout := pctkvarRate;
//End;

//----------------------------------------------------------------------------

void TStorageControllerObj::SetFleetkWRate(double pctkw)
{
	int i = 0;
	int stop = 0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		((TStorageObj*) FleetPointerList->Get(i))->Set_pctkWOut(pctkw);
	}
}

//----------------------------------------------------------------------------

void TStorageControllerObj::SetFleetToCharge()
{
	int i = 0;
	int stop = 0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		((TStorageObj*) FleetPointerList->Get(i))->Set_StorageState(STORE_CHARGING);
	}
	FleetState = STORE_CHARGING;
}

//----------------------------------------------------------------------------

void TStorageControllerObj::SetFleetToDisCharge()
{
	int i = 0;
	int stop = 0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		((TStorageObj*) FleetPointerList->Get(i))->Set_StorageState(STORE_DISCHARGING);
	}
	FleetState = STORE_DISCHARGING;
}

//----------------------------------------------------------------------------

void TStorageControllerObj::SetFleetToIdle()
{
	int i = 0;
	int stop = 0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		/*# with TStorageObj(FleetPointerList.Get(i)) do */
		{
			auto with0 = ((TStorageObj*) FleetPointerList->Get(i));
			with0->Set_StorageState(STORE_IDLING);
//                  PresentkW := 0.0;
			with0->Set_kW(0.0);
		}
	}
	FleetState = STORE_IDLING;
}

//-----------------------------------------------------------------------------

void TStorageControllerObj::SetFleetDesiredState(int State)
{
	int i = 0;
	int stop = 0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		((TStorageObj*) FleetPointerList->Get(i))->Set_StateDesired(State);
	}
}

//-----------------------------------------------------------------------------
//
//procedure TStorageControllerObj.Set_PFBand(const Value: Double);
//begin
//      FPFBand    := Value;
//      HalfPFBand := FPFBand / 2.0;
//end;

//----------------------------------------------------------------------------

void TStorageControllerObj::SetFleetToExternal()
{
	int i = 0;
	int stop = 0;
	for(stop = FleetPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		((TStorageObj*) FleetPointerList->Get(i))->DispatchMode = STORE_EXTERNALMODE;
	}
}

//----------------------------------------------------------------------------
/*
  PROCEDURE TStorageControllerObj.SetPctReserve;
  VAR
        i   :Integer;
  Begin
        For i := 1 to FleetPointerList->get_myNumList() Do
              TStorageObj(FleetPointerList.Get(i)).pctReserve := pctFleetReserve;
  End;
*/


//----------------------------------------------------------------------------

int TStorageControllerObj::InterpretMode(int Opt, const String s)
{
	int result = 0;
	result = -1;  // Unknown: error
	switch(Opt)
	{
		case 	propMODEDISCHARGE:
		switch(LowerCase(s)[0])
		{
			case 	L'f':
			result = MODEFOLLOW;
			break;
			case 	L'l':
			result = MODELOADSHAPE;
			break;
			case 	L'p':
			result = MODEPEAKSHAVE;
			break;
			case 	L's':
			if(LowerCase(s)[1] == L'c')
				result = MODESCHEDULE;
			else
				result = MODESUPPORT;
			break;
			case 	L't':
			result = MODETIME;
			break;
			case 	L'i':
			result = CURRENTPEAKSHAVE;
			break;
			default:
			DoSimpleMsg(String("Discharge Mode \"") + s + "\" not recognized.", 14402);
			break;
		}
		break;
		case 	propMODECHARGE:
		switch(LowerCase(s)[0])
		{
			case 	L'l':
                 // 'f': Result := MODEFOLLOW;
			result = MODELOADSHAPE;
			break;
                 // 's': Result := MODESUPPORT;
			case 	L't':
			result = MODETIME;
			break;
			case 	L'p':
			result = MODEPEAKSHAVELOW;
			break;
			case 	L'i':
			result = CURRENTPEAKSHAVELOW;
			break;
			default:
			DoSimpleMsg(String("Charge Mode \"") + s + "\" not recognized.", 14402);
			break;
		}
		break;
		default:
		  ;
		break;
	}
	return result;
}

//----------------------------------------------------------------------------

bool TStorageControllerObj::MakeFleetList()
{
	bool result = false;
	TStorageObj* StorageObj = nullptr;
	int i = 0;
	int stop = 0;
	result = false;
	if(FElementListSpecified)    // Name list is defined - Use it
	{
		int stop = 0;
		FleetPointerList->Clear();
		for(stop = FleetSize, i = 1; i <= stop; i++)
		{
			StorageObj = (TStorageObj*) StorageClass[ActiveActor]->Find( FStorageNameList[i - 1] );
			if(ASSIGNED(StorageObj))
			{
				if( ( (TDSSCktElement*) StorageObj )->Get_Enabled())
					FleetPointerList->Set_New(StorageObj);
			}
			else
			{
				DoSimpleMsg(String("Error: Storage Element \"") + FStorageNameList[i - 1]
	           + "\" not found.", 14403);
				return result;
			}
		}
	}
	else


     /*Search through the entire circuit for enabled Storage Elements and add them to the list*/
	{
		int stop = 0;
		FStorageNameList.clear();
		FleetPointerList->Clear();
		for(stop = StorageClass[ActiveActor]->Get_ElementCount(), i = 1; i <= stop; i++)
		{
			StorageObj =  (TStorageObj*) StorageClass[ActiveActor]->ElementList.Get(i);
        // Look for a storage element not already assigned
			if( StorageObj->Get_Enabled() && (StorageObj->DispatchMode != STORE_EXTERNALMODE))
			{
				FStorageNameList.push_back( StorageObj->get_Name());  // Add to list of names
				FleetPointerList->Set_New(StorageObj);
			}
		}

     /*Allocate uniform weights*/
		FleetSize = FleetPointerList->get_myNumList();
		FWeights = (pDoubleArray) realloc(FWeights, sizeof(double) * FleetSize);
		for(stop = FleetSize, i = 1; i <= stop; i++)
		{
			(FWeights)[i - 1] = 1.0;
		}
	}

   // Add up total weights
	TotalWeight = 0.0;
	for(stop = FleetSize, i = 1; i <= stop; i++)
	{
		TotalWeight = TotalWeight + (FWeights)[i - 1];
	}
	if(FleetPointerList->get_myNumList() > 0)
		result = true;
	FleetListChanged = false;
	return result;
}



//----------------------------------------------------------------------------

void TStorageControllerObj::Reset(int ActorID)
{

  // inherited;
	SetFleetToIdle();

 // do we want to set fleet to 100% charged storage?
}



//----------------------------------------------------------------------------

String TStorageControllerObj::ReturnElementsList()
{
	String result;
	int i = 0;
	int stop = 0;
	if(FleetSize == 0)
	{
		result = "";
		return result;
	}
	result = String("[") + FStorageNameList[0];
	for(stop = FleetSize - 1, i = 1; i <= stop; i++)
	{
		result = result + ", " + FStorageNameList[i];
	}
	result = result + "]";  // terminate the array
	return result;
}

//----------------------------------------------------------------------------

String TStorageControllerObj::ReturnSeasonTarget(int THigh)
{
	String result;
	int i = 0;
	int stop = 0;
	if(Seasons == 1)
	{
		result = "";
		return result;
	}
	result = "[";
	for(stop = (Seasons - 1), i = 0; i <= stop; i++)
	{
		if(THigh == 1)
			result = result + Format("%.6g", SeasonTargets[i]) + ", ";
		else
			result = result + Format("%.6g", SeasonTargetsLow[i]) + ", ";
	}
	result = result + "]";  // terminate the array
	return result;
}


//----------------------------------------------------------------------------

String TStorageControllerObj::ReturnWeightsList()
{
	String result;
	if(FleetSize == 0)
	{
		result = "";
		return result;
	}
	result = GetDSSArray_Real(FleetSize, FWeights);
	return result;
}

//----------------------------------------------------------------------------

// Get power to control based on active power

void TStorageControllerObj::GetControlPower(complex& ControlPower, int ActorID)
{
	int i = 0;
	int ControlPowerPhase = 0;
	double TempPower = 0.0;
	if(get_FMonitoredElement()->Get_NPhases() == 1)
	{
		ControlPower = get_FMonitoredElement()->Get_Power(ElementTerminal, ActorID); // just take the total power (works also for 1ph elements with 2 conductors)
	}
	else
	{
		get_FMonitoredElement()->GetPhasePower(&(cBuffer[0]), ActorID);
		switch(FMonPhase)
		{
			case 	AVG:  // Get avg of all phases
			{
				int stop = 0;
				ControlPower = cmplx(0.0, 0.0);
				for(stop = (get_FMonitoredElement()->Get_NConds() + CondOffset), i = (1 + CondOffset); i <= stop; i++)
				{
					ControlPower = cadd(ControlPower, (cBuffer)[i - 1]);
				}
			}
			break;  // Get abs max of all phases
			case 	MAXPHASE:
			{
				int stop = 0;
				ControlPower = cmplx(0.0, 0.0);
				for(stop = (get_FMonitoredElement()->Get_NConds() + CondOffset), i = (1 + CondOffset); i <= stop; i++)
				{
					TempPower = Abs( (cBuffer)[i - 1].re);
					if(TempPower > Abs( ControlPower.re))
						ControlPower = (cBuffer)[i - 1];
					ControlPowerPhase = i;
				}
                          // Compute equivalent total power of all phases assuming equal to max power in all phases
				ControlPower = cmulreal(ControlPower, (double) Fnphases);
			}
			break; // Get abs min of all phases
			case 	MINPHASE:
			{
				int stop = 0;
				ControlPower = cmplx(1.0e50, 1.0e50);
				for(stop = (get_FMonitoredElement()->Get_NConds() + CondOffset), i = (1 + CondOffset); i <= stop; i++)
				{
					TempPower = Abs( (cBuffer)[i - 1].re);
					if(TempPower < Abs( ControlPower.re))
						ControlPower = (cBuffer)[i - 1];
					ControlPowerPhase = i;
				}
                          // Compute equivalent total power of all phases assuming equal to min power in all phases
				ControlPower = cmulreal(ControlPower, (double) Fnphases);  // sign according to phase with min abs value
			}
			break;
            // Compute equivalent total power of all phases assuming equal to power in selected phases
			default:
			ControlPower = cmulreal((cBuffer)[FMonPhase - 1], (double) Fnphases);  // monitored phase only
			break;
		}
	}

    /*If this is a positive sequence circuit (Fnphases=1),
    then we need to multiply by 3 to get the 3-phase power*/
	if(ActiveCircuit[ActorID]->PositiveSequence)
		ControlPower = cmulreal(ControlPower, 3.0);
}

//----------------------------------------------------------------------------

// Get current to control

void TStorageControllerObj::GetControlCurrent(double& ControlCurrent)
{
	int i = 0;
	switch(FMonPhase)
	{
		case 	AVG:
		{
			int stop = 0;
			ControlCurrent = 0.0;     // Get avg of all phases
			for(stop = (get_FMonitoredElement()->Get_NConds() + CondOffset), i = (1 + CondOffset); i <= stop; i++)
			{
				ControlCurrent = ControlCurrent + cabs(cBuffer[i - 1]);
			}
			ControlCurrent = ControlCurrent / Fnphases;
		}
		break;
		case 	MAXPHASE:
		{
			int stop = 0;
			ControlCurrent = 0.0;     // Get max of all phases
			for(stop = (get_FMonitoredElement()->Get_NConds() + CondOffset), i = (1 + CondOffset); i <= stop; i++)
			{
				ControlCurrent = max(ControlCurrent, cabs(cBuffer[i - 1]));
			}
			ControlCurrent = ControlCurrent;
		}
		break;
		case 	MINPHASE:
		{
			int stop = 0;
			ControlCurrent = 1.0e50;     // Get min of all phases
			for(stop = (get_FMonitoredElement()->Get_NConds() + CondOffset), i = (1 + CondOffset); i <= stop; i++)
			{
				ControlCurrent = min(ControlCurrent, cabs(cBuffer[i - 1]));
			}
			ControlCurrent = ControlCurrent;
		}
		break;
    /*Just use one phase because that's what most controls do.*/
		default:
		ControlCurrent = cabs(cBuffer[FMonPhase - 1]);  // monitored phase only
		break;
	}
}


/*--------------------------------------------------------------------------*/


void StorageController_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
}

		class 		StorageController_unit
		{
		public:
		StorageController_unit()
		{
			//AssertSystemInitialization();
			StorageController_initialization();
		}
		};
		StorageController_unit _StorageController_unit;

}  // namespace StorageController





