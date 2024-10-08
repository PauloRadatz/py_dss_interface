
#pragma hdrstop

#include "InvControl.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Ucmatrix.h"
#include "mathutil.h"
#include <math.h>
#include "Utilities.h"

using namespace std;
using namespace Arraydef;
using namespace Bus;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace ControlClass;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace PCElement;
using namespace PVSystem;
using namespace ParserDel;
using namespace PointerList;
using namespace Storage;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XYCurve;
using namespace mathutil;
using namespace Utilities;

namespace InvControl
{

TInvControlObj::TInvControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TInvControlObj::TInvControlObj(String ClassName) : inherited(ClassName) {}
TInvControlObj::TInvControlObj() {}


TInvControlObj* ActiveInvControlObj = nullptr;
const int	NumPropsThisClass	= 34,
			None				= 0,
			CHANGEVARLEVEL		= 1,
			CHANGEWATTLEVEL		= 2,
			CHANGEWATTVARLEVEL	= 3,
			CHANGEDRCVVARLEVEL	= 4,
			AVGPHASES			= -1,
			MAXPHASE			= -2,
			MINPHASE			= -3;

const double	FLAGDELTAQ			= -1.0,
				FLAGDELTAP			= -1.0,
				DELTAQDEFAULT		= 0.5,
				DELTAPDEFAULT		= 0.5;

    // Modes
const int	NONE_MODE		= 0,
			VOLTVAR			= 1,
			VOLTWATT		= 2,
			DRC				= 3,
			WATTPF			= 4,
			WATTVAR			= 5,
			AVR				= 6,
			GFM				= 7;

// Modes in string type
const std::string myCtrlMode[7] = 
{ "voltvar", "voltwatt", "dynamicreaccurr", "wattpf", "wattvar", "avr", "gfm" };

const std::string myDERTypes[2] =
{ "PVSystem", "Storage" };

const int	PVSys			= 0,
			EStorage		= 1;

    // Combi Modes
const int	NONE_COMBMODE	= 0,
			VV_VW			= 1,
			VV_DRC			= 2;							// Creates superstructure for all InvControl objects

TInvControl::TInvControl()
 : XY_CurveClass((TDSSClass*) GetDSSClassPtr("XYCurve"))
{
	;
	Class_Name = "InvControl";
	DSSClassType = DSSClassType + INV_CONTROL2;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

TInvControl::~TInvControl()
{
	// inherited::Destroy();
}


void TInvControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

    // Define Property names
	PropertyName[0] = "DERList";
	PropertyName[1] = "Mode";
	PropertyName[2] = "CombiMode";
	PropertyName[3] = "vvc_curve1";
	PropertyName[4] = "hysteresis_offset";
	PropertyName[5] = "voltage_curvex_ref";
	PropertyName[6] = "avgwindowlen";
	PropertyName[7] = "voltwatt_curve";

    //following for dynamic reactive current mode
	PropertyName[8] = "DbVMin";
	PropertyName[9] = "DbVMax";
	PropertyName[10] = "ArGraLowV";
	PropertyName[11] = "ArGraHiV";
	PropertyName[12] = "DynReacavgwindowlen";
	PropertyName[13] = "deltaQ_Factor";
	PropertyName[14] = "VoltageChangeTolerance";
	PropertyName[15] = "VarChangeTolerance";
	PropertyName[16] = "VoltwattYAxis";
	PropertyName[17] = "RateofChangeMode";
	PropertyName[18] = "LPFTau";
	PropertyName[19] = "RiseFallLimit";
	PropertyName[20] = "deltaP_Factor";
	PropertyName[21] = "EventLog";
	PropertyName[22] = "RefReactivePower";
	PropertyName[23] = "ActivePChangeTolerance";
	PropertyName[24] = "monVoltageCalc";
	PropertyName[25] = "monBus";
	PropertyName[26] = "MonBusesVbase";
	PropertyName[27] = "voltwattCH_curve";
	PropertyName[28] = "wattpf_curve";
	PropertyName[29] = "wattvar_curve";
	PropertyName[30] = "VV_RefReactivePower";
	PropertyName[31] = "PVSystemList";
	PropertyName[32] = "Vsetpoint";
	PropertyName[33] = "ControlModel";
	PropertyHelp[0] = String("Array list of PVSystem and/or Storage elements to be controlled. " "If not specified, all PVSystem and Storage in the circuit are assumed to be controlled by this control. ") + CRLF
	           + CRLF
	           + "No capability of hierarchical control between two controls for a single element is implemented at this time.";
	PropertyHelp[1] = String("Smart inverter function in which the InvControl will control the PC elements specified in DERList, according to the options below:") + CRLF
	           + CRLF
	           + "Must be one of: {VOLTVAR* | VOLTWATT | DYNAMICREACCURR | WATTPF | WATTVAR | GFM} "
	           + CRLF + "if the user desires to use modes simultaneously, then set the CombiMode property. Setting the Mode to any valid value disables combination mode."
	           + CRLF + CRLF
	           + "In volt-var mode (Default). This mode attempts to CONTROL the vars, according to one or two volt-var curves, depending on the monitored voltages, present active power output, and the capabilities of the PVSystem/Storage. "
	           + CRLF + CRLF
	           + "In volt-watt mode. This mode attempts to LIMIT the watts, according to one defined volt-watt curve, depending on the monitored voltages and the capabilities of the PVSystem/Storage. "
	           + CRLF + CRLF
	           + "In dynamic reactive current mode. This mode attempts to increasingly counter deviations by CONTROLLING vars, depending on the monitored voltages, present active power output, and the capabilities of the of the PVSystem/Storage."
	           + CRLF + CRLF
	           + "In watt-pf mode. This mode attempts to CONTROL the vars, according to a watt-pf curve, depending on the present active power output, and the capabilities of the PVSystem/Storage. "
	           + CRLF + CRLF
	           + "In watt-var mode. This mode attempts to CONTROL the vars, according to a watt-var curve, depending on the present active power output, and the capabilities of the PVSystem/Storage. "
			   + CRLF + CRLF + 
				 "In GFM mode this control will trigger the GFM control routine for the DERs within the DERList.The GFM actiosn will only take place if the pointed DERs are in GFM mode.The controller parameters are locally setup at the DER. ";
//                       CRLF+', any limits set on maximum reactive power. '+
//                       CRLF+'Precedence will be given to either watt production or var production based on the setting of RefReactivePower.'+
	PropertyHelp[2] = String("Combination of smart inverter functions in which the InvControl will control the PC elements in DERList, according to the options below: ") + CRLF
	           + CRLF
	           + "Must be a combination of the following: {VV_VW | VV_DRC}. Default is to not set this property, in which case the single control mode in Mode is active.  "
	           + CRLF
	           + CRLF
	           + "In combined VV_VW mode, both volt-var and volt-watt control modes are active simultaneously.  See help individually for volt-var mode and volt-watt mode in Mode property."
	           + CRLF
	           + "Note that the PVSystem/Storage will attempt to achieve both the volt-watt and volt-var set-points based on the capabilities of the inverter in the PVSystem/Storage (kVA rating, etc), any limits set on maximum active power,"
	           + CRLF
	           + CRLF
	           + "In combined VV_DRC, both the volt-var and the dynamic reactive current modes are simultaneously active.";
//                       CRLF+CRLF+'The volt-var function will attempt to achieve its set-point based on the volt-var curve, and present voltage.  The dynamic '+
//                       CRLF+'reactive power mode function will also be active and it will add or subtract from the reactive power set-point desired by the volt-var function.'+
//                       CRLF+'Note that the precedence of active and reactive power production is defined by the RefReactivePower property.  In no event will the reactive '+
//                       CRLF+'power exceed the maximum var limit of the PVSystem, and the combination of the active and reactive power output will not exceed the kVA rating of '+
//                       CRLF+'the inverter (set in the PVSystem/Storage).';
	PropertyHelp[3] = String("Required for VOLTVAR mode. ") + CRLF
	           + CRLF
	           + "Name of the XYCurve object containing the volt-var curve. The positive values of the y-axis of the volt-var curve represent values in pu of the provided base reactive power. "
	           + "The negative values of the y-axis are values in pu of the absorbed base reactive power. "
	           + CRLF
	           + "Provided and absorbed base reactive power values are defined in the RefReactivePower property"
	           + CRLF
	           + CRLF
	           + "Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem/Storage, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. ";
	PropertyHelp[4] = String("Required for VOLTVAR mode, and defaults to 0. ") + CRLF
	           + CRLF
	           + "for the times when the terminal voltage is decreasing, this is the off-set in per-unit voltage of a curve whose shape is the same as vvc_curve. "
	           + "It is offset by a certain negative value of per-unit voltage, which is defined by the base quantity for the x-axis of the volt-var curve (see help for voltage_curvex_ref)"
	           + CRLF
	           + CRLF
	           + "if the PVSystem/Storage terminal voltage has been increasing, and has not changed directions, utilize vvc_curve1 for the volt-var response. "
	           + CRLF
	           + CRLF
	           + "if the PVSystem/Storage terminal voltage has been increasing and changes directions and begins to decrease, then move from utilizing vvc_curve1 to a volt-var curve of the same shape, but offset by a certain per-unit voltage value. "
	           + CRLF
	           + CRLF
	           + "Maintain the same per-unit available var output level (unless head-room has changed due to change in active power or kva rating of PVSystem/Storage).  Per-unit var values remain the same for this internally constructed second curve (hysteresis curve). "
	           + CRLF
	           + CRLF
	           + "if the terminal voltage has been decreasing and changes directions and begins to increase , then move from utilizing the offset curve, back to the vvc_curve1 for volt-var response, but stay at the same per-unit available vars output level.";
	PropertyHelp[5] = String("Required for VOLTVAR and VOLTWATT modes, and defaults to rated.  Possible values are: {rated|avg|ravg}.  ") + CRLF
	           + CRLF
	           + "Defines whether the x-axis values (voltage in per unit) for vvc_curve1 and the volt-watt curve corresponds to:"
	           + CRLF
	           + CRLF
	           + "rated. The rated voltage for the PVSystem/Storage object (1.0 in the volt-var curve equals rated voltage)."
	           + CRLF
	           + CRLF
	           + "avg. The average terminal voltage recorded over a certain number of prior power-flow solutions."
	           + CRLF
	           + "with the avg setting, 1.0 per unit on the x-axis of the volt-var curve(s) corresponds to the average voltage."
	           + CRLF
	           + "from a certain number of prior intervals.  See avgwindowlen parameter."
	           + CRLF
	           + CRLF
	           + "ravg. Same as avg, with the exception that the avgerage terminal voltage is divided by the rated voltage.";
	PropertyHelp[6] = String("Required for VOLTVAR mode and VOLTWATT mode, and defaults to 0 seconds (0s). ") + CRLF
	           + CRLF
	           + "Sets the length of the averaging window over which the average PVSystem/Storage terminal voltage is calculated. "
	           + CRLF
	           + CRLF
	           + "Units are indicated by appending s, m, or h to the integer value. "
	           + CRLF
	           + CRLF
	           + "The averaging window will calculate the average PVSystem/Storage terminal voltage over the specified period of time, up to and including the last power flow solution. "
	           + CRLF
	           + CRLF
	           + "Note, if the solution stepsize is larger than the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.";
	PropertyHelp[7] = String("Required for VOLTWATT mode. ") + CRLF
	           + CRLF
	           + "Name of the XYCurve object containing the volt-watt curve. "
	           + CRLF
	           + CRLF
	           + "Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the PVSystem/Storage, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. "
	           + CRLF
	           + CRLF
	           + "Units for the y-axis are either in one of the options described in the VoltwattYAxis property. ";
	PropertyHelp[8] = String("Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.95 per-unit voltage (referenced to the PVSystem/Storage object rated voltage or a windowed average value). ") + CRLF
	           + CRLF
	           + "This parameter is the minimum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ";
	PropertyHelp[9] = String("Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1.05 per-unit voltage (referenced to the PVSystem object rated voltage or a windowed average value). ") + CRLF
	           + CRLF
	           + "This parameter is the maximum voltage that defines the voltage dead-band within which no reactive power is allowed to be generated. ";
	PropertyHelp[10] = String("Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  ") + CRLF
	           + CRLF
	           + "This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage capacitive reactive power production is increased as the  percent delta-voltage decreases below DbVMin. "
	           + CRLF
	           + CRLF
	           + "Percent delta-voltage is defined as the present PVSystem/Storage terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem/Storage object. "
	           + CRLF
	           + CRLF
	           + "Note, the moving average voltage for the dynamic reactive current mode is different than the moving average voltage for the volt-watt and volt-var modes.";
	PropertyHelp[11] = String("Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 0.1  ") + CRLF
	           + CRLF
	           + "This is a gradient, expressed in unit-less terms of %/%, to establish the ratio by which percentage inductive reactive power production is increased as the  percent delta-voltage decreases above DbVMax. "
	           + CRLF
	           + CRLF
	           + "Percent delta-voltage is defined as the present PVSystem/Storage terminal voltage minus the moving average voltage, expressed as a percentage of the rated voltage for the PVSystem/Storage object. "
	           + CRLF
	           + CRLF
	           + "Note, the moving average voltage for the dynamic reactive current mode is different than the mmoving average voltage for the volt-watt and volt-var modes.";
	PropertyHelp[12] = String("Required for the dynamic reactive current mode (DYNAMICREACCURR), and defaults to 1 seconds (1s). do not use a value smaller than 1.0 ") + CRLF
	           + CRLF
	           + "Sets the length of the averaging window over which the average PVSystem/Storage terminal voltage is calculated "
	           + "for the dynamic reactive current mode. "
	           + CRLF
	           + CRLF
	           + "Units are indicated by appending s, m, or h to the integer value. "
	           + CRLF
	           + CRLF
	           + "Typically this will be a shorter averaging window than the volt-var and volt-watt averaging window."
	           + CRLF
	           + CRLF
	           + "The averaging window will calculate the average PVSystem/Storage terminal voltage over the specified period of time, up to and including the last power flow solution.  Note, if the solution stepsize is larger than "
	           + "the window length, then the voltage will be assumed to have been constant over the time-frame specified by the window length.";
	PropertyHelp[13] = String("Required for the VOLTVAR and DYNAMICREACCURR modes.  Defaults to -1.0. ") + CRLF
	           + CRLF
	           + "Defining -1.0, OpenDSS takes care internally of delta_Q itself. It tries to improve convergence as well as speed up process"
	           + CRLF
	           + CRLF
	           + "Sets the maximum change (in per unit) from the prior var output level to the desired var output level during each control iteration. "
	           + CRLF
	           + CRLF
	           + CRLF
	           + "if numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, "
	           + "this is an indication of numerical instability (use the EventLog to diagnose). "
	           + CRLF
	           + CRLF
	           + "if the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number "
	           + "of control iterations needed to achieve the control criteria, and move to the power flow solution.";
	PropertyHelp[14] = String("Defaults to 0.0001 per-unit voltage.  This parameter should only be modified by advanced users of the InvControl.  ") + CRLF
	           + CRLF
	           + "Tolerance in pu of the control loop convergence associated to the monitored voltage in pu. "
	           + "This value is compared with the difference of the monitored voltage in pu of the current and previous control iterations of the control loop"
	           + CRLF
	           + CRLF
	           + "This voltage tolerance value plus the var/watt tolerance value (VarChangeTolerance/ActivePChangeTolerance) determine, together, when to stop control iterations by the InvControl. "
	           + CRLF
	           + CRLF
	           + "If an InvControl is controlling more than one PVSystem/Storage, each PVSystem/Storage has this quantity calculated independently, and so an individual "
	           + "PVSystem/Storage may reach the tolerance within different numbers of control iterations.";
	PropertyHelp[15] = String("Required for VOLTVAR and DYNAMICREACCURR modes.  Defaults to 0.025 per unit of the base provided or absorbed reactive power described in the RefReactivePower property " "This parameter should only be modified by advanced users of the InvControl. ") + CRLF
	           + CRLF
	           + "Tolerance in pu of the convergence of the control loop associated with reactive power. "
	           + "For the same control iteration, this value is compared to the difference, as an absolute value (without sign), between the desired reactive power value in pu and the output reactive power in pu of the controlled element."
	           + CRLF
	           + CRLF
	           + "This reactive power tolerance value plus the voltage tolerance value (VoltageChangeTolerance) determine, together, when to stop control iterations by the InvControl.  "
	           + CRLF
	           + CRLF
	           + "If an InvControl is controlling more than one PVSystem/Storage, each PVSystem/Storage has this quantity calculated independently, and so an individual "
	           + "PVSystem/Storage may reach the tolerance within different numbers of control iterations.";
	PropertyHelp[16] = String("Required for VOLTWATT mode.  Must be one of: {PMPPPU* | PAVAILABLEPU| PCTPMPPPU | KVARATINGPU}.  The default is PMPPPU.  ") + CRLF
	           + CRLF
	           + "Units for the y-axis of the volt-watt curve while in volt-watt mode. "
	           + CRLF
	           + CRLF
	           + "When set to PMPPPU. The y-axis corresponds to the value in pu of Get_FPmpp() property of the PVSystem. "
	           + CRLF
	           + CRLF
	           + "When set to PAVAILABLEPU. The y-axis corresponds to the value in pu of the available active power of the PVSystem. "
	           + CRLF
	           + CRLF
	           + "When set to PCTPMPPPU. The y-axis corresponds to the value in pu of the power Pmpp multiplied by 1/100 of the %Get_FPmpp() property of the PVSystem."
	           + CRLF
	           + CRLF
	           + "When set to KVARATINGPU. The y-axis corresponds to the value in pu of the kVA property of the PVSystem.";
	PropertyHelp[17] = String("Required for VOLTWATT and VOLTVAR mode.  Must be one of: {INACTIVE* | LPF | RISEFALL }.  The default is INACTIVE.  ") + CRLF
	           + CRLF
	           + "Auxiliary option that aims to limit the changes of the desired reactive power and the active power limit between time steps, the alternatives are listed below: "
	           + CRLF
	           + CRLF
	           + "INACTIVE. It indicates there is no limit on rate of change imposed for either active or reactive power output. "
	           + CRLF
	           + CRLF
	           + "LPF. A low-pass RC filter is applied to the desired reactive power and/or the active power limit to determine the output power as a function of a time constant defined in the LPFTau property. "
	           + CRLF
	           + CRLF
	           + "RISEFALL. A rise and fall limit in the change of active and/or reactive power expressed in terms of pu power per second, defined in the RiseFallLimit, is applied to the desired reactive power and/or the active power limit. ";
	PropertyHelp[18] = String("Not required. Defaults to 0 seconds. ") + CRLF
	           + CRLF
	           + "Filter time constant of the LPF option of the RateofChangeMode property. "
	           + "The time constant will cause the low-pass filter to achieve 95% of the target value in 3 time constants.";
	PropertyHelp[19] = String("Not required.  Defaults to no limit (-1). Must be -1 (no limit) or a positive value.  ") + CRLF
	           + CRLF
	           + "Limit in power in pu per second used by the RISEFALL option of the RateofChangeMode property."
	           + "The base value for this ramp is defined in the RefReactivePower property and/or in VoltwattYAxis.";
	PropertyHelp[20] = String("Required for the VOLTWATT modes.  Defaults to -1.0. ") + CRLF
	           + CRLF
	           + "Defining -1.0, OpenDSS takes care internally of delta_P itself. It tries to improve convergence as well as speed up process"
	           + CRLF
	           + CRLF
	           + "Defining between 0.05 and 1.0, it sets the maximum change (in unit of the y-axis) from the prior active power output level to the desired active power output level during each control iteration. "
	           + CRLF
	           + CRLF
	           + CRLF
	           + "If numerical instability is noticed in solutions such as active power changing substantially from one control iteration to the next and/or voltages oscillating between two values with some separation, "
	           + "this is an indication of numerical instability (use the EventLog to diagnose). "
	           + CRLF
	           + CRLF
	           + "If the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number "
	           + "of control iterations needed to achieve the control criteria, and move to the power flow solution.";
	PropertyHelp[21] = "{Yes/True* | No/False} Default is YES for InvControl. Log control actions to Eventlog.";
	PropertyHelp[22] = String("Required for any mode that has VOLTVAR, DYNAMICREACCURR and WATTVAR. Defaults to VARAVAL.") + CRLF
	           + CRLF
	           + "Defines the base reactive power for both the provided and absorbed reactive power, according to one of the following options: "
	           + CRLF
	           + CRLF
	           + "VARAVAL. The base values for the provided and absorbed reactive power are equal to the available reactive power."
	           + CRLF
	           + CRLF
	           + "VARMAX: The base values of the provided and absorbed reactive power are equal to the value defined in the kvarMax and kvarMaxAbs properties, respectively.";
	PropertyHelp[23] = String("Required for VOLTWATT. Default is 0.01") + CRLF
	           + CRLF
	           + "Tolerance in pu of the convergence of the control loop associated with active power. "
	           + "For the same control iteration, this value is compared to the difference between the active power limit in pu resulted from the convergence process and the one resulted from the volt-watt function."
	           + CRLF
	           + CRLF
	           + "This reactive power tolerance value plus the voltage tolerance value (VoltageChangeTolerance) determine, together, when to stop control iterations by the InvControl.  "
	           + CRLF
	           + CRLF
	           + "If an InvControl is controlling more than one PVSystem/Storage, each PVSystem/Storage has this quantity calculated independently, and so an individual "
	           + "PVSystem/Storage may reach the tolerance within different numbers of control iterations.";
	PropertyHelp[24] = "Number of the phase being monitored or one of {AVG | MAX | MIN} for all phases. Default=AVG. ";
	PropertyHelp[25] = "Name of monitored bus used by the voltage-dependente control modes. Default is bus of the controlled PVSystem/Storage or Storage.";
	PropertyHelp[26] = "Array list of rated voltages of the buses and their nodes presented in the monBus property. This list may have different line-to-line and/or line-to-ground voltages.";
	PropertyHelp[27] = String("Required for VOLTWATT mode for Storage element in CHARGING state. ") + CRLF
	           + CRLF
	           + "The name of an XYCurve object that describes the variation in active power output (in per unit of maximum active power outut for the Storage). "
	           + CRLF
	           + CRLF
	           + "Units for the x-axis are per-unit voltage, which may be in per unit of the rated voltage for the Storage, or may be in per unit of the average voltage at the terminals over a user-defined number of prior solutions. "
	           + CRLF
	           + CRLF
	           + "Units for the y-axis are either in: (1) per unit of maximum active power output capability of the Storage, or (2) maximum available active power output capability (defined by the parameter: VoltwattYAxis), "
	           + "corresponding to the terminal voltage (x-axis value in per unit). "
	           + CRLF
	           + CRLF
	           + "No default -- must be specified for VOLTWATT mode for Storage element in CHARGING state.";
	PropertyHelp[28] = String("Required for WATTPF mode.") + CRLF
	           + CRLF
	           + "Name of the XYCurve object containing the watt-pf curve."
	           + CRLF
	           + "The positive values of the y-axis are positive power factor values. "
	           + "The negative values of the the y-axis are negative power factor values. "
	           + "When positive, the output reactive power has the same direction of the output active power, and when negative, it has the opposite direction."
	           + CRLF
	           + "Units for the x-axis are per-unit output active power, and the base active power is the Get_FPmpp() for PVSystem and kWrated for Storage."
	           + CRLF
	           + CRLF
	           + "The y-axis represents the power factor and the reference is power factor equal to 0. "
	           + CRLF
	           + CRLF
	           + "For example, if the user wants to define the following XY coordinates: (0, 0.9); (0.2, 0.9); (0.5, -0.9); (1, -0.9)."
	           + CRLF
	           + "Try to plot them considering the y-axis reference equal to unity power factor."
	           + CRLF
	           + CRLF
	           + "The user needs to translate this curve into a plot in which the y-axis reference is equal to 0 power factor."
	           + "It means that two new XY coordinates need to be included, in this case they are: (0.35, 1); (0.35, -1)."
	           + CRLF
	           + "Try to plot them considering the y-axis reference equal to 0 power factor."
	           + CRLF
	           + "The discontinity in 0.35pu is not a problem since var is zero for either power factor equal to 1 or -1.";
	PropertyHelp[29] = String("Required for WATTVAR mode. ") + CRLF
	           + CRLF
	           + "Name of the XYCurve object containing the watt-var curve. The positive values of the y-axis of the watt-var curve represent values in pu of the provided base reactive power. "
	           + "The negative values of the y-axis are values in pu of the absorbed base reactive power. "
	           + CRLF
	           + "Provided and absorbed base reactive power values are defined in the RefReactivePower property."
	           + CRLF
	           + CRLF
	           + "Units for the x-axis are per-unit output active power, and the base active power is the Pmpp for PVSystem and kWrated for Storage.";
	PropertyHelp[30] = "Deprecated, use RefReactivePower instead.";
	PropertyHelp[31] = "Deprecated, use DERList instead.";
	PropertyHelp[32] = "Required for Active Voltage Regulation (AVR).";
	PropertyHelp[33] = "Integer defining the method for moving across the control curve.It can be one of the following : " + CRLF + CRLF
				+ "0 = Linear mode (default)" + CRLF 
				+ "1 = Exponential" + CRLF + CRLF 
				+ "Use this property for better tunning your controllerand improve the controller response in terms of control iterations needed to reach the target." + CRLF
				+ "This property alters the meaning of deltaQ_factor and deltaP_factor properties accroding to its value(Check help).The method can also be combined with the controller tolerance for improving performance.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

int TInvControl::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new InvControl and add it to InvControl class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TInvControlObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

int TInvControl::Edit(int ActorID)
{
	int result			= 0,
		CharPos			= 0,
		ParamPointer	= 0,
		i				= 0,
		j				= 0,
		NNode			= 0;
	String	StrTemp,
			ParamName,
			Param;

	int NodeBuffer[11/*# range 1..10*/];

    // continue parsing with contents of Parser
	ActiveInvControlObj = (TInvControlObj*)  ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveInvControlObj);
	result = 0;
	/*# with ActiveInvControlObj do */
	{
		auto with0 = ActiveInvControlObj;
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
	           + "\"", 364);
				break;
				case 	1:
					InterpretTStringListArray(Param, (with0->FDERNameList));
				break; // Read list of PVSystem and Storage objects in OpenDSS format and add to FDERNameList StringList.
				case 	2:
				{
					StrTemp = Parser[ActorID]->MakeString_();
					j = 0;
					for (i = 0; i < myCtrlMode->size(); i++)
					{
						if (CompareTextShortest(StrTemp, myCtrlMode[i]) == 0)
						{
							with0->ControlMode		= i + 1;
							with0->CombiControlMode = NONE_COMBMODE;
							j						= 1;
							break;
						}
					}
					if (j == 0)
					{
						if (with0->ControlMode == NONE_MODE)
							DoSimpleMsg("Invalid Control Mode selected", 1366);
						with0->CombiControlMode = NONE_COMBMODE;
						SolutionAbort = true;
						return result;
					}
					
				}
				break;
				case 	3:
				{
					if(CompareTextShortest(Parser[ActorID]->MakeString_(), "vv_vw") == 0)
					{
						with0->ControlMode = NONE_MODE;
						with0->CombiControlMode = VV_VW;
					}
					else
					{
						if(CompareTextShortest(Parser[ActorID]->MakeString_(), "vv_drc") == 0)
						{
							with0->ControlMode = NONE_MODE;
							with0->CombiControlMode = VV_DRC;
						}
						else
						{
							if(with0->CombiControlMode == NONE_COMBMODE)
								DoSimpleMsg("Invalid CombiControl Mode selected", 1367);
							with0->CombiControlMode = NONE_COMBMODE;
							SolutionAbort = true;
							return result;
						}
					}
				}
				break;
				case 	4:
				{
					with0->Fvvc_curvename = Parser[ActorID]->MakeString_();
					if(with0->Fvvc_curvename.size() > 0)
					{
						with0->Fvvc_curve = GetXYCurve(with0->Fvvc_curvename, VOLTVAR);
						with0->Fvvc_curve_size = with0->Fvvc_curve->get_FNumPoints();
					}
				}
				break;
				case 	5:
				{
					if(Parser[ActorID]->MakeDouble_() > 0.0)
						DoSimpleMsg(String("Hysteresis offset should be a negative value, or 0 \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 1364);
					else
						with0->Fvvc_curveOffset = Parser[ActorID]->MakeDouble_();
				}
				break;
				case 	6:
				{
					if(CompareTextShortest(Parser[ActorID]->MakeString_(), "rated") == 0)
						with0->FVoltage_CurveX_ref = 0;
					else
					{
						if(CompareTextShortest(Parser[ActorID]->MakeString_(), "avg") == 0)
							with0->FVoltage_CurveX_ref = 1;
						else
						{
							if(CompareTextShortest(Parser[ActorID]->MakeString_(), "ravg") == 0)
								with0->FVoltage_CurveX_ref = 2;
						}
					}
				}
				break;
				case 	7:
					with0->FRollAvgWindowLength = with0->InterpretAvgVWindowLen(Param);
					break;
				case 	8:
				{
					with0->Fvoltwatt_curvename = Parser[ActorID]->MakeString_();
					if(with0->Fvoltwatt_curvename.size() > 0)
					{
						with0->Fvoltwatt_curve = GetXYCurve(with0->Fvoltwatt_curvename, VOLTWATT);
						with0->Fvoltwatt_curve_size = with0->Fvoltwatt_curve->get_FNumPoints();
					}
				}
				break;
				case 	9:
				{
					with0->FDbVMin = Parser[ActorID]->MakeDouble_();
					if((with0->FDbVMax > 0.0) && (with0->FDbVMin > with0->FDbVMax))
					{
						DoSimpleMsg(String("Minimum dead-band voltage value should be less than the maximum dead-band voltage value.  Value set to 0.0 \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 1365);
						with0->FDbVMin = 0.0;
					}
				}
				break;
				case 	10:
				{
					with0->FDbVMax = Parser[ActorID]->MakeDouble_();
					if((with0->FDbVMin > 0.0) && (with0->FDbVMax < with0->FDbVMin))
					{
						DoSimpleMsg(String("Maximum dead-band voltage value should be greater than the minimum dead-band voltage value.  Value set to 0.0 \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 1366);
						with0->FDbVMax = 0.0;
					}
				}
				break;
				case 	11:
					with0->FArGraLowV = Parser[ActorID]->MakeDouble_();
					break;
				case 	12:
					with0->FArGraHiV = Parser[ActorID]->MakeDouble_();
					break;
				case 	13:
					with0->FDRCRollAvgWindowLength = with0->InterpretDRCAvgVWindowLen(Param);
					break;
				case 	14:
					with0->FdeltaQ_factor = Parser[ActorID]->MakeDouble_();
					break;
				case 	15:
					with0->FVoltageChangeTolerance = Parser[ActorID]->MakeDouble_();
					break;
				case 	16:
					with0->FVarChangeTolerance = Parser[ActorID]->MakeDouble_();
					break;
				case 	17:
				{
					if(CompareTextShortest(Parser[ActorID]->MakeString_(), "pavailablepu") == 0)
						with0->FVoltwattYAxis = 0;
					else
					{
						if(CompareTextShortest(Parser[ActorID]->MakeString_(), "pmpppu") == 0)
							with0->FVoltwattYAxis = 1;
						else
						{
							if(CompareTextShortest(Parser[ActorID]->MakeString_(), "pctpmpppu") == 0)
								with0->FVoltwattYAxis = 2;
							else
							{
								if(CompareTextShortest(Parser[ActorID]->MakeString_(), "kvaratingpu") == 0)
									with0->FVoltwattYAxis = 3;
							}
						}
					}
				}
				break;
				case 	18:
				{
					if(CompareTextShortest(Parser[ActorID]->MakeString_(), "inactive") == 0)
						with0->RateofChangeMode = Inactive;
					else
					{
						if(CompareTextShortest(Parser[ActorID]->MakeString_(), "lpf") == 0)
							with0->RateofChangeMode = LPF;
						else
						{
							if(CompareTextShortest(Parser[ActorID]->MakeString_(), "risefall") == 0)
								with0->RateofChangeMode = RISEFALL;
						}
					}
				}
				break;
				case 	19:
				{
					if(Parser[ActorID]->MakeDouble_() > 0)
						with0->FLPFTau = Parser[ActorID]->MakeDouble_();
					else
						with0->RateofChangeMode = Inactive;
				}
				break;
				case 	20:
				{
					if(Parser[ActorID]->MakeDouble_() > 0)
						with0->FRiseFallLimit = Parser[ActorID]->MakeDouble_();
					else
						with0->RateofChangeMode = Inactive;
				}
				break;
				case 	21:
					with0->FdeltaP_factor = Parser[ActorID]->MakeDouble_();
					break;
				case 	22:
					with0->ShowEventLog = InterpretYesNo(Param);
					break;
				case 	23:
				{
					if(CompareTextShortest(Parser[ActorID]->MakeString_(), "varaval") == 0)
						with0->FReacPower_ref = "VARAVAL";
					else
					{
						if(CompareTextShortest(Parser[ActorID]->MakeString_(), "varmax") == 0)
							with0->FReacPower_ref = "VARMAX";
					}
				}
				break;
				case 	24:
					with0->FActivePChangeTolerance = Parser[ActorID]->MakeDouble_();
					break;
				case 	25:
				{
					if(CompareTextShortest(Param, "avg") == 0)
						with0->FMonBusesPhase = AVGPHASES;
					else
					{
						if(CompareTextShortest(Param, "max") == 0)
							with0->FMonBusesPhase = MAXPHASE;
						else
						{
							if(CompareTextShortest(Param, "min") == 0)
								with0->FMonBusesPhase = MINPHASE;
							else
								with0->FMonBusesPhase = max(1, Parser[ActorID]->MakeInteger_());
						}
					}
				}
				break; //FMonBuses     := Param;
				case 	26:
				{
					int stop = 0;
					InterpretTStringListArray(Param, ( with0->FMonBusesNameList ) );
					with0->FMonBuses.resize( with0->FMonBusesNameList.size() );
					with0->FMonBusesNodes.resize( with0->FMonBusesNameList.size() );
					AuxParser[ActiveActor]->SetCmdString(Param);  //Parser[ActorID].MakeString_();  // load AuxParser
					for(stop = with0->FMonBusesNameList.size() - 1, i = 0; i <= stop; i++)
					{
						int stop1 = 0;
						String dummy = AuxParser[ActiveActor]->GetNextParam();  // Gets the next token
						with0->FMonBuses[i] = AuxParser[ActiveActor]->ParseAsBusName(NNode, (pIntegerArray) &NodeBuffer, ActiveActor);
						with0->FMonBusesNodes[i].resize( NNode );
						for(stop1 = NNode - 1, j = 0; j <= stop1; j++)
						{
							with0->FMonBusesNodes[i][j] = NodeBuffer[j];
						}
					}
				}
				break;
				case 	27:
				{
					with0->FMonBusesVbase.resize( with0->FMonBusesNameList.size() );
					Parser[ActiveActor]->ParseAsVector(with0->FMonBusesNameList.size(), &(with0->FMonBusesVbase[0]) );
				}
				break;
				case 	28:
				{
					with0->FvoltwattCH_curvename = Parser[ActorID]->MakeString_();
					if(with0->FvoltwattCH_curvename.size() > 0)
					{
						with0->FvoltwattCH_curve = GetXYCurve(with0->FvoltwattCH_curvename, VOLTWATT);
						with0->FvoltwattCH_curve_size = with0->FvoltwattCH_curve->get_FNumPoints();
					}
				}
				break;
				case 	29:
				{
					with0->Fwattpf_curvename = Parser[ActorID]->MakeString_();
					if(with0->Fwattpf_curvename.size() > 0)
					{
						with0->Fwattpf_curve = GetXYCurve(with0->Fwattpf_curvename, WATTPF);
						with0->Fwattpf_curve_size = with0->Fwattpf_curve->get_FNumPoints();
					}
				}
				break;
				case 	30:
				{
					with0->Fwattvar_curvename = Parser[ActorID]->MakeString_();
					if(with0->Fwattvar_curvename.size() > 0)
					{
						with0->Fwattvar_curve = GetXYCurve(with0->Fwattvar_curvename, WATTVAR);
						with0->Fwattvar_curve_size = with0->Fwattvar_curve->get_FNumPoints();
					}
				}
				break;
				case 	31:
				{
					StrTemp = Parser[ActorID]->MakeString_();
					CharPos = AnsiPos("_", StrTemp);
					if(CharPos != 0)
						StrTemp = StrTemp.substr(0, CharPos - 1);
					if(CompareTextShortest(StrTemp, "varaval") == 0)
						with0->FReacPower_ref = "VARAVAL";
					else
					{
						if(CompareTextShortest(StrTemp, "varmax") == 0)
							with0->FReacPower_ref = "VARMAX";
					}
				}
				break;
				case 	32:
				{
					int stop = 0;
					InterpretTStringListArray(Param, ( with0->FDERNameList ) ); // Read list of PVSystem and Storage objects in OpenDSS format and add to FDERNameList StringList.
                    // Because is using this command from the previous version of InvControl, we assume that the list includes only
                    // PVSystems, so the list is updated
					for(stop = (with0->FDERNameList.size() - 1), CharPos = 0; CharPos <= stop; CharPos++)
					{
						( with0->FDERNameList )[CharPos] = String("PVSystem.") + ( with0->FDERNameList )[CharPos];
					}
				}
				break;
				case 	33:
					with0->Fv_setpoint = Parser[ActorID]->MakeDouble_();
					break;
				case	34:
					with0->CtrlModel = Parser[ActorID]->MakeInteger_();
					break;
                // Inherited parameters
				default:
				ClassEdit(ActiveInvControlObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	1: // re-alloc based on
				{
					with0->FDERPointerList->Clear();
					with0->FListSize = with0->FDERNameList.size();
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

int TInvControl::MakeLike(const String InvControlName)
{
	int result = 0;
	TInvControlObj* OtherInvControl = nullptr;
	int i = 0;
	int j = 0;
	result = 0;
    /*See if we can find this InvControl name in the present collection*/
	OtherInvControl = ((TInvControlObj*) Find(InvControlName));
	if(OtherInvControl != nullptr)
		/*# with ActiveInvControlObj do */
		{
			auto with0 = ActiveInvControlObj;
			int stop = 0;
			with0->Set_NPhases(OtherInvControl->Fnphases);
			with0->Set_Nconds(OtherInvControl->Fnconds); // Force Reallocation of terminal stuff
			for(stop = with0->FDERPointerList->get_myNumList(), i = 1; i <= stop; i++)
			{
				auto& with1 = with0->CtrlVars[i];
				with1.ControlledElement		= OtherInvControl->CtrlVars[i].ControlledElement;
				with1.CondOffset			= OtherInvControl->CtrlVars[i].CondOffset;
				with1.FVBase				= OtherInvControl->CtrlVars[i].FVBase;
				with1.FVarFollowInverter	= OtherInvControl->CtrlVars[i].FVarFollowInverter;
				with1.FInverterON			= OtherInvControl->CtrlVars[i].FInverterON;
				with1.FpresentkW			= OtherInvControl->CtrlVars[i].FpresentkW;
				with1.FkVArating			= OtherInvControl->CtrlVars[i].FkVArating;
				with1.Fpresentkvar			= OtherInvControl->CtrlVars[i].Fpresentkvar;
				with1.Fkvarlimit			= OtherInvControl->CtrlVars[i].Fkvarlimit;
				with1.FkvarLimitNeg			= OtherInvControl->CtrlVars[i].FkvarLimitNeg;
				with1.FCurrentkvarLimit		= OtherInvControl->CtrlVars[i].FCurrentkvarLimit;
				with1.FCurrentkvarLimitNeg	= OtherInvControl->CtrlVars[i].FCurrentkvarLimitNeg;
				with1.FDCkWRated			= OtherInvControl->CtrlVars[i].FDCkWRated;
				with1.FpctDCkWRated			= OtherInvControl->CtrlVars[i].FpctDCkWRated;
				with1.FEffFactor			= OtherInvControl->CtrlVars[i].FEffFactor;
				with1.FDCkW					= OtherInvControl->CtrlVars[i].FDCkW;
				with1.FPPriority			= OtherInvControl->CtrlVars[i].FPPriority;
				with1.FActiveVVCurve		= OtherInvControl->CtrlVars[i].FActiveVVCurve;
			}
			with0->ControlMode				= OtherInvControl->ControlMode;
			with0->CombiControlMode			= OtherInvControl->CombiControlMode;
			with0->FListSize				= OtherInvControl->FListSize;
			with0->Fvvc_curve_size			= OtherInvControl->Fvvc_curve_size;
			with0->Fvvc_curve				= OtherInvControl->Fvvc_curve;
			with0->Fvvc_curvename			= OtherInvControl->Fvvc_curvename;
			with0->Fvvc_curveOffset			= OtherInvControl->Fvvc_curveOffset;
			with0->FVoltage_CurveX_ref		= OtherInvControl->FVoltage_CurveX_ref;
			with0->FDRCVAvgWindowLengthSec	= OtherInvControl->FDRCVAvgWindowLengthSec;
			with0->FVAvgWindowLengthSec		= OtherInvControl->FVAvgWindowLengthSec;
			with0->Fvoltwatt_curve_size		= OtherInvControl->Fvoltwatt_curve_size;
			with0->Fvoltwatt_curve			= OtherInvControl->Fvoltwatt_curve;
			with0->Fvoltwatt_curvename		= OtherInvControl->Fvoltwatt_curvename;
			with0->FvoltwattCH_curve_size	= OtherInvControl->FvoltwattCH_curve_size;
			with0->FvoltwattCH_curve		= OtherInvControl->FvoltwattCH_curve;
			with0->FvoltwattCH_curvename	= OtherInvControl->FvoltwattCH_curvename;
			with0->Fwattpf_curve_size		= OtherInvControl->Fwattpf_curve_size;
			with0->Fwattpf_curve			= OtherInvControl->Fwattpf_curve;
			with0->Fwattpf_curvename		= OtherInvControl->Fwattpf_curvename;
			with0->Fwattvar_curve_size		= OtherInvControl->Fwattvar_curve_size;
			with0->Fwattvar_curve			= OtherInvControl->Fwattvar_curve;
			with0->Fwattvar_curvename		= OtherInvControl->Fwattvar_curvename;
			with0->FDbVMin					= OtherInvControl->FDbVMin;
			with0->pf_wp_nominal			= OtherInvControl->pf_wp_nominal;
			with0->FDbVMax					= OtherInvControl->FDbVMax;
			with0->FArGraLowV				= OtherInvControl->FArGraLowV;
			with0->FArGraHiV				= OtherInvControl->FArGraHiV;
			with0->FRollAvgWindowLength		= OtherInvControl->FRollAvgWindowLength;
			with0->FRollAvgWindowLengthIntervalUnit		= OtherInvControl->FRollAvgWindowLengthIntervalUnit;
			with0->FDRCRollAvgWindowLength	= OtherInvControl->FDRCRollAvgWindowLength;
			with0->FDRCRollAvgWindowLengthIntervalUnit	= OtherInvControl->FDRCRollAvgWindowLengthIntervalUnit;
			with0->FActivePChangeTolerance	= OtherInvControl->FActivePChangeTolerance;
			with0->FdeltaQ_factor			= OtherInvControl->FdeltaQ_factor;
			with0->FdeltaP_factor			= OtherInvControl->FdeltaP_factor;
			with0->FVoltageChangeTolerance	= OtherInvControl->FVoltageChangeTolerance;
			with0->FVarChangeTolerance		= OtherInvControl->FVarChangeTolerance;
			with0->FVoltwattYAxis			= OtherInvControl->FVoltwattYAxis;
			with0->RateofChangeMode			= OtherInvControl->RateofChangeMode;
			with0->FLPFTau					= OtherInvControl->FLPFTau;
			with0->FRiseFallLimit			= OtherInvControl->FRiseFallLimit;
			with0->FMonBusesPhase			= OtherInvControl->FMonBusesPhase;
			with0->FMonBuses				= OtherInvControl->FMonBuses;
			with0->FMonBusesNodes			= OtherInvControl->FMonBusesNodes;
			with0->FMonBusesVbase.resize( with0->FMonBusesNameList.size() );
			for(stop = with0->FMonBusesNameList.size(), j = 1; j <= stop; j++)
			{
				(with0->FMonBusesVbase)[j - 1] = (OtherInvControl->FMonBusesVbase)[j - 1];
			}
			with0->TimeDelay = OtherInvControl->TimeDelay;
			for(stop = with0->ParentClass->NumProperties, j = 1; j <= stop; j++)
			{
				with0->Set_PropertyValue(j, OtherInvControl->Get_PropertyValue(j));
			}
		}
	else
		DoSimpleMsg(String("Error in InvControl MakeLike: \"") + InvControlName
	           + "\" Not Found.", 370);
	return result;
}

/*==========================================================================*/
/*                    TInvControlObj                                        */
/*==========================================================================*/

TInvControlObj::TInvControlObj(TDSSClass* ParClass, const String InvControlName)
 : inherited(ParClass)
{
	Set_Name(LowerCase(InvControlName));
	DSSObjType = ParClass->DSSClassType;
	ElementName = "";
	FDERNameList.clear();
	FMonBusesNameList.clear();
	FListSize = 0;
    /*
     Control elements are zero current sources that attach to a terminal of a
     power-carrying device, but do not alter voltage or current flow.
     Define a default number of phases and conductors here and update in
     RecalcElementData routine if necessary. This allocates arrays for voltages
     and currents and gives more direct access to the values,if needed
    */
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                       // in base class
	ElementTerminal = 1;

    /*Variables for voltages*/
	FVpuSolutionIdx = 0;
    /*Variables for convergence process*/
	FdeltaQ_factor = FLAGDELTAQ;
	FdeltaP_factor = FLAGDELTAP;
	FVoltageChangeTolerance = 0.0001;
	FVarChangeTolerance = 0.025;
	FActivePChangeTolerance = 0.01;

    /*Variables of DER element*/
	FDERNameList.clear();
	FDERPointerList = NULL;
	FDERPointerList = new TPointerList(20);  // Default size and increment

    /*Variables for monitored Bus/buses*/
	FMonBusesNameList.clear();
	FMonBusesPhase = AVGPHASES;
	FMonBuses.clear();
	FMonBusesVbase.clear();
	FMonBusesNodes.clear();

    /*Variables for LPF and RF options*/
	RateofChangeMode = Inactive;
	FLPFTau = 0.001;
	FRiseFallLimit = 0.001;

    /*Variables of the smart inverter functions*/
	FVoltage_CurveX_ref = 0;
	FReacPower_ref = "VARAVAL";
	FVoltwattYAxis = 1;

    // volt-var
	Fvvc_curve_size = 0;
	Fvvc_curve = nullptr;
	Fvvc_curvename = "";
	Fvvc_curveOffset = 0.0;
	Fvvc_curve2 = nullptr;
	FVAvgWindowLengthSec = 1.0;
	FRollAvgWindowLength = 1;
	FRollAvgWindowLengthIntervalUnit = "s";

    // watt-pf
	Fwattpf_curve_size = 0;
	Fwattpf_curve = nullptr;
	Fwattpf_curvename = "";
	pf_wp_nominal = 0.0;

    // watt-var
	Fwattvar_curve_size = 0;
	Fwattvar_curve = nullptr;
	Fwattvar_curvename = "";

    // DRC
	FDbVMin = 0.95;
	FDbVMax = 1.05;
	FArGraLowV = 0.1;
	FArGraHiV = 0.1;
	FDRCRollAvgWindowLength = 1;
	FDRCRollAvgWindowLengthIntervalUnit = "s";
	FDRCVAvgWindowLengthSec = 1.0;

    // volt-watt
	Fvoltwatt_curve_size = 0;
	Fvoltwatt_curve = nullptr;
	Fvoltwatt_curvename = "";
	FvoltwattCH_curve_size = 0;
	FvoltwattCH_curve = nullptr;
	FvoltwattCH_curvename = "";

    // AVR
	Fv_setpoint = 1.0;
	CtrlModel	= 0; // Linear mode
	InitPropertyValues(0);
}

TInvControlObj::~TInvControlObj()
{
	ElementName = "";
	
	FMonBuses.clear();
	FMonBusesNodes.clear();
	CtrlVars.resize(0);
	if(!(FMonBusesVbase.empty()))
		FMonBusesVbase.clear();
	// inherited::Destroy();
}


void TInvControlObj::RecalcElementData(int ActorID)
{
	int i = 0;
	int stop = 0;
	if(FDERPointerList->get_myNumList() == 0)
		MakeDERList();
	if(FDERPointerList->get_myNumList() > 0)
    /*Setting the terminal of the InvControl device to same as the 1st PVSystem/Storage element*/
    /* This sets it to a realistic value to avoid crashes later */
	{
		MonitoredElement = ((TDSSCktElement*) FDERPointerList->Get(1));   // Set MonitoredElement to 1st elemnent in list
		SetBus(1, MonitoredElement->Get_FirstBus());
	}
	for(stop = FDERPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		auto& with1 = CtrlVars[i];
        // User (ControlledElement)[] as the pointer to the PVSystem/Storage elements
		with1.ControlledElement = (TPCElement*) FDERPointerList->Get(i);		// pointer to i-th PVSystem/Storage element
		with1.cBuffer.resize( ( (TDSSCktElement*) with1.ControlledElement )->Yorder + 1 );
		( (TDSSCktElement*) with1.ControlledElement )->Set_ActiveTerminal(1);	// Make the 1 st terminal active
		Set_NPhases(((TDSSCktElement*)(with1.ControlledElement))->Get_NPhases());
		Set_Nconds(Get_NPhases());
		with1.FRollAvgWindow->Set_BuffLength(FRollAvgWindowLength);				// TEMc
		with1.FDRCRollAvgWindow->Set_BuffLength(FDRCRollAvgWindowLength);

        // for all modes other than VW and WATTPF, PF priority is not allowed
		if(( ControlMode != VOLTWATT) && (ControlMode != WATTPF))
		{
			if(((TNamedObject*)(with1.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
				((TPVsystemObj*) (with1.ControlledElement))->PVSystemVars.PF_Priority = false;
			else
			{
				if(((TNamedObject*)(with1.ControlledElement))->Get_myPName() == myDERTypes[EStorage])
					((TStorageObj*) (with1.ControlledElement))->StorageVars.PF_Priority = false;
			}
		}

        //FdeltaQFactor[i]                := FdeltaQ_factor;
        //FdeltaPFactor[i]                := FdeltaP_factor;
		if(FMonBuses.size() == 0)
			FUsingMonBuses = false;
		else
			FUsingMonBuses = true;
		if(ASSIGNED(with1.ControlledElement))
			UpdateDERParameters(i);
		else
		{
			(with1.ControlledElement) = nullptr;
			DoErrorMsg(String("InvControl: \"") + this->get_Name() + "\"", String("Controlled Element \"") + (FDERNameList)[i - 1]
	           + "\" Not Found.", " PVSystem or Storage object must be defined previously.", 361);
		}
	}
}

// ***  This assumes the PVSystem/Storage devices have already been converted to pos seq

void TInvControlObj::MakePosSequence(int ActorID)
{
	if(FDERPointerList->get_myNumList() == 0)
		RecalcElementData(ActorID);
	Set_NPhases(3);
	Set_Nconds(3);
	SetBus(1, MonitoredElement->GetBus(ElementTerminal));
	if(FDERPointerList->get_myNumList() > 0)
    /*Setting the terminal of the InvControl device to same as the 1st PVSystem/Storage element*/
    /* This sets it to a realistic value to avoid crashes later */
	{
		MonitoredElement = ((TDSSCktElement*) FDERPointerList->Get(1));   // Set MonitoredElement to 1st PVSystem/Storage in list
		SetBus(1, MonitoredElement->Get_FirstBus());
		Set_NPhases(MonitoredElement->Get_NPhases());
		Set_Nconds(Get_NPhases());
	}
	inherited::MakePosSequence(ActorID);
}

void TInvControlObj::CalcYPrim(int ActorID)
{

    // leave YPrims as nil and they will be ignored
    // Yprim is zeroed when created.  Leave it as is.
    //  if YPrim=nil then YPrim := TcMatrix.CreateMatrix(Yorder);
}

void TInvControlObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
    // Control is a zero current source
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TInvControlObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
    // Control is a zero current source
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TInvControlObj::DumpProperties(TTextRec& f, bool Complete)
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

void TInvControlObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	int k = 0;
	TPCElement* DERelem = nullptr;
	int stop = 0;
	for(stop = FDERPointerList->get_myNumList(), k = 1; k <= stop; k++)
	{
		auto& with0 = CtrlVars[k];
		DERelem = with0.ControlledElement;

        // Calculates QHeadRoom
		Calc_QHeadRoom(k, ActorID);
		if(with0.QHeadRoom != 0.0)
			with0.FPriorvarspu  = with0.FPriorvars  / with0.QHeadRoom ;

        // Calculates PBase
		Calc_PBase(k, ActorID);
		with0.FPriorWattspu  = with0.FPriorwatts  / with0.PBase ;

        // Calculates kW_out_desiredpu. Used for VW and VV_VW
		with0.kW_out_desiredpu  = with0.kW_out_desired  / with0.PBase ;

        // -------------------Smart Inverter Functions------------------------//
        /*Smart Inverter volt-var function*/
		if ((ControlMode == VOLTVAR) && (CombiControlMode == NONE_COMBMODE) && (Get_PendingChange(k) == CHANGEVARLEVEL))
            // Set var mode to VARMODEKVAR to indicate we might change kvar
		{
			if( ( (TNamedObject*) (with0.ControlledElement) )->Get_myPName() == myDERTypes[PVSys])
			{
				((TPVsystemObj*) DERelem)->Set_VWmode(false);
				((TPVsystemObj*)DERelem)->Set_Varmode(VARMODEKVAR);
				((TPVsystemObj*) DERelem)->Set_VVmode(true);
			}
			else
			{
				((TStorageObj*) DERelem)->Set_VWmode(false);
				((TStorageObj*) DERelem)->Set_Varmode(VARMODEKVAR);
				((TStorageObj*) DERelem)->Set_VVmode(true);
			}

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k]
			CalcQVVcurve_desiredpu(k, ActorID);

            // LPF or RF activated
			if(RateofChangeMode == LPF)
			{
				CalcLPF(k, "VARS", with0.QDesireVVpu , ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
				Check_Qlimits(k, with0.QDesireOptionpu , ActorID);
				with0.QDesireEndpu  = min(Abs(with0.QDesireLimitedpu ), Abs(with0.QDesireOptionpu )) * Sign(with0.QDesireOptionpu );
			}
			else
			{
				if(RateofChangeMode == RISEFALL)
				{
					CalcRF(k, "VARS", with0.QDesireVVpu , ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
					Check_Qlimits(k, with0.QDesireOptionpu , ActorID);
					with0.QDesireEndpu  = min(Abs(with0.QDesireLimitedpu ), Abs(with0.QDesireOptionpu )) * Sign( with0.QDesireOptionpu );
				}
				else

                // Checks kVA (watt priority) and kvarlimit limits
				{
					Check_Qlimits(k, with0.QDesireVVpu , ActorID);
					with0.QDesireEndpu  = min(Abs(with0.QDesireVVpu ), Abs(with0.QDesireLimitedpu )) * Sign(with0.QDesireVVpu );
				}

            // Calculates QDesiredVV[k] through the convergence algorithm
			}
			CalcVoltVar_vars(k, ActorID);

            //--------------------------------------------- end Main process ---------------------------------------------//

            // Sets PVSystem/Storage's kvar_out
			if( ( (TNamedObject*) with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
				((TPVsystemObj*)DERelem)->Set_Presentkvar(with0.QDesiredVV);
			else
				((TStorageObj*) DERelem)->Set_kvarRequested(with0.QDesiredVV);

            // Uptates PresentkW and Presentkvar considering watt and var priorities
			if( ( (TNamedObject*) (with0.ControlledElement) )->Get_myPName() == myDERTypes[PVSys])
			{
				((TPVsystemObj*) DERelem)->SetNominalPVSystemOuput(ActorID);
				if(with0.QDesiredVV  >= 0.0)
					with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
				else
					with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
			}
			else
			{
				((TStorageObj*) DERelem)->SetNominalStorageOutput(ActorID);
				if(with0.QDesiredVV  >= 0.0)
					with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
				else
					with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
			}

            // Values used in convergence
			with0.QoutputVVpu  = with0.Qoutputpu ;
			with0.FAvgpVpuPrior  = with0.FPresentVpu ;

            // Values used in CalcQVVcurve_desiredpu
			if( ( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
			{
				with0.QOld  = ((TPVsystemObj*) DERelem)->Get_Presentkvar();
				with0.QOldVV  = ((TPVsystemObj*) DERelem)->Get_Presentkvar();
				if(ShowEventLog)
					AppendToEventLog(String("InvControl.") + this->get_Name()
	           + ", "
	           + ( ( (TNamedObject*) DERelem) )->Get_QualifiedName(), Format("VOLTVAR mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredVV, ((TPVsystemObj*)DERelem)->Get_Presentkvar()), ActorID);
			}
			else
			{
				with0.QOld		= ((TStorageObj*) DERelem)->Get_Presentkvar();
				with0.QOldVV	= ((TStorageObj*) DERelem)->Get_Presentkvar();
				if(ShowEventLog)
					AppendToEventLog(String("InvControl.") + this->get_Name() + ", " + ((TNamedObject*)DERelem)->Get_QualifiedName(), Format("VOLTVAR mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.", with0.QDesiredVV, ((TStorageObj*)DERelem)->Get_Presentkvar()), ActorID);
			}

        /*Smart Inverter active voltage regulation function*/
		}
		else
		{
			if ((ControlMode == AVR) && (CombiControlMode == NONE_COMBMODE) && (Get_PendingChange(k) == CHANGEVARLEVEL))
            // Set var mode to VARMODEKVAR to indicate we might change kvar
			{
				if( ( (TNamedObject*) with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
				{
					((TPVsystemObj*)DERelem)->Set_VWmode(false);
					((TPVsystemObj*)DERelem)->Set_Varmode(VARMODEKVAR);
					((TPVsystemObj*)DERelem)->Set_AVRmode(true);
				}
				else
				{
					((TStorageObj*)DERelem)->Set_VWmode(false);
					((TStorageObj*)DERelem)->Set_Varmode(VARMODEKVAR);
					((TStorageObj*)DERelem)->Set_AVRmode(true);
				}

            //--------------------------------------------- Main process ---------------------------------------------//
				if(ActiveCircuit[ActorID]->Solution->ControlIteration == 1)
				{
					with0.FAvgpVpuPrior		= with0.FPresentVpu;
					with0.FAvgpAVRVpuPrior	= with0.FPresentVpu;

                 // Sets PVSystem/Storage's kvar_out
					if( ( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
						((TPVsystemObj*) DERelem)->Set_Presentkvar(with0.QHeadRoom  / 2);
					else
						((TStorageObj*)DERelem)->Set_kvarRequested(with0.QHeadRoom / 2);
				}
				else
				{
					if(ActiveCircuit[ActorID]->Solution->ControlIteration == 2)
                // Sets PVSystem/Storage's kvar_out
					{
						if( ( (TNamedObject*) with0.ControlledElement  )->Get_myPName() == myDERTypes[PVSys])
							with0.DQDV  = Abs( (((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom  / (with0.FPresentVpu  - with0.FAvgpVpuPrior )));
						else
							with0.DQDV  = Abs( (double(((TStorageObj*) DERelem)->Get_kvarRequested()) / with0.QHeadRoom  / (with0.FPresentVpu  - with0.FAvgpVpuPrior )));
					}
					else

                // Calculates QDesireAVRpu[k]
					{
						CalcQAVR_desiredpu(k, ActorID);


                // Checks kVA (watt priority) and kvarlimit limits
						Check_Qlimits(k, with0.QDesireAVRpu , ActorID);
						with0.QDesireEndpu  = min(Abs(with0.QDesireAVRpu ), Abs(with0.QDesireLimitedpu )) * Sign(with0.QDesireAVRpu );
						if(Abs( (with0.QDesireEndpu  - with0.QDesireLimitedpu )) < 0.05)
							with0.Fv_setpointLimited  = with0.FPresentVpu ;
						else
							with0.Fv_setpointLimited  = Fv_setpoint;

                // Calculates QDesiredVV[k] through the convergence algorithm
						CalcAVR_vars(k, ActorID);

                //--------------------------------------------- end Main process ---------------------------------------------//

                // Sets PVSystem/Storage's kvar_out
						if(( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
							((TPVsystemObj*) DERelem)->Set_Presentkvar(with0.QDesiredAVR);
						else
							((TStorageObj*)DERelem)->Set_kvarRequested(with0.QDesiredAVR);

                // Uptates PresentkW and Presentkvar considering watt and var priorities
						if(( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
						{
							((TPVsystemObj*) DERelem)->SetNominalPVSystemOuput(ActorID);
							if(with0.QDesiredAVR  >= 0.0)
								with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
							else
								with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
						}
						else
						{
							((TStorageObj*) DERelem)->SetNominalStorageOutput(ActorID);
							if(with0.QDesiredAVR  >= 0.0)
								with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
							else
								with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
						}

              // Values used in convergence
						with0.QoutputAVRpu		= with0.Qoutputpu ;
						with0.FAvgpVpuPrior		= with0.FPresentVpu ;

              // Values used in CalcQVVcurve_desiredpu
						if(( (TNamedObject*) with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
						{
							with0.QOld  = ((TPVsystemObj*) DERelem)->Get_Presentkvar();
							with0.QOldAVR  = ((TPVsystemObj*) DERelem)->Get_Presentkvar();
							if(ShowEventLog)
								AppendToEventLog(String("InvControl.") + this->get_Name()
	           + ", "
	           + ( (TNamedObject*) DERelem)->Get_QualifiedName(), Format("VOLTVAR mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredAVR, ((TPVsystemObj*)DERelem)->Get_Presentkvar()), ActorID);
						}
						else
						{
							with0.QOld  = ((TStorageObj*) DERelem)->Get_Presentkvar();
							with0.QOldAVR  = ((TStorageObj*) DERelem)->Get_Presentkvar();
							if(ShowEventLog)
								AppendToEventLog(String("InvControl.") + this->get_Name() + ", " + ((TNamedObject*)DERelem)->Get_QualifiedName(), Format("VOLTVAR mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.", with0.QDesiredAVR, ((TStorageObj*)DERelem)->Get_Presentkvar()), ActorID);
						}
					}

        /*Smart Inverter watt-pf function*/
				}
			}
			else
			{
				if ((ControlMode == WATTPF) && (CombiControlMode == NONE_COMBMODE) && (Get_PendingChange(k) == CHANGEVARLEVEL))
            // Set var mode to VARMODEKVAR to indicate we might change kvar
				{
					if(( (TNamedObject*) with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
					{
						((TPVsystemObj*)DERelem)->Set_VWmode(false);
						((TPVsystemObj*)DERelem)->Set_Varmode(VARMODEKVAR);
						((TPVsystemObj*)DERelem)->Set_WPmode(true);
					}
					else
					{
						((TStorageObj*)DERelem)->Set_VWmode(false);
						((TStorageObj*)DERelem)->Set_Varmode(VARMODEKVAR);
						((TStorageObj*)DERelem)->Set_WPmode(true);
					}

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireWPpu[k]
					CalcQWPcurve_desiredpu(k, ActorID);

            // Checks kVA (watt priority) and kvarlimit limits
					Check_Qlimits(k, with0.QDesireWPpu , ActorID);
					with0.QDesireEndpu  = min(Abs(with0.QDesireWPpu ), Abs(with0.QDesireLimitedpu )) * Sign(with0.QDesireWPpu );

            // Calculates QDesiredWP[k] through the convergence algorithm
					CalcWATTPF_vars(k, ActorID);

            //--------------------------------------------- end Main process ---------------------------------------------//
            // Sets PVSystem/Storage's pf_wp_nominal
					if(( (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
						((TPVsystemObj*)DERelem)->Set_pf_wp_nominal(pf_wp_nominal);
					else
						((TStorageObj*)DERelem)->Set_kvarRequested(with0.QDesiredWP);

            // Sets PVSystem/Storage's kvar_out
					if((with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
						((TPVsystemObj*)DERelem)->Set_Presentkvar(with0.QDesiredWP);
					else
						((TStorageObj*)DERelem)->Set_kvarRequested(with0.QDesiredWP);

            // Uptates PresentkW and Presentkvar considering watt and var priorities
					if((with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
					{
						((TPVsystemObj*) DERelem)->SetNominalPVSystemOuput(ActorID);
						if(with0.QDesiredWP  >= 0.0)
							with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
						else
							with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
					}
					else
					{
						((TStorageObj*) DERelem)->SetNominalStorageOutput(ActorID);
						if(with0.QDesiredWP  >= 0.0)
							with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
						else
							with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
					}

            // Values used in convergence
					with0.QoutputVVpu		= with0.Qoutputpu ;
					with0.FAvgpVpuPrior		= with0.FPresentVpu ;

            // Values used in CalcQVVcurve_desiredpu
					if(( (TNamedObject*)with0.ControlledElement  )->Get_myPName() == myDERTypes[PVSys])
					{
						with0.QOld		= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
						with0.QOldVV	= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
						if(ShowEventLog)
							AppendToEventLog(String("InvControl.") + this->get_Name()
	           + ", "
	           + DERelem->Get_QualifiedName(), Format("WATTPF mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredWP, ((TPVsystemObj*)DERelem)->Get_Presentkvar()), ActorID);
					}
					else
					{
						with0.QOld		= ((TStorageObj*) DERelem)->Get_Presentkvar();
						with0.QOldVV	= ((TStorageObj*) DERelem)->Get_Presentkvar();
						if(ShowEventLog)
							AppendToEventLog(String("InvControl.") + this->get_Name() + ", " + ((TNamedObject*)DERelem)->Get_QualifiedName(), Format("WATTPF mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.", with0.QDesiredWP, ((TStorageObj*)DERelem)->Get_Presentkvar()), ActorID);
					}

        /*Smart Inverter watt-var function*/
				}
				else
				{
					if ((ControlMode == WATTVAR) && (CombiControlMode == NONE_COMBMODE) && (Get_PendingChange(k) == CHANGEVARLEVEL))
            // Set var mode to VARMODEKVAR to indicate we might change kvar
					{
						if(( (TNamedObject*)with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
						{
							((TPVsystemObj*)DERelem)->Set_VWmode(false);
							((TPVsystemObj*)DERelem)->Set_Varmode(VARMODEKVAR);
							((TPVsystemObj*)DERelem)->Set_WVmode(true);
						}
						else
						{
							((TStorageObj*)DERelem)->Set_VWmode(false);
							((TStorageObj*)DERelem)->Set_Varmode(VARMODEKVAR);
							((TStorageObj*) DERelem)->Set_WVmode(true);
						}

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireWVpu[k]
						CalcQWVcurve_desiredpu(k, ActorID);

            // Checks kVA (watt priority) and kvarlimit limits
						Check_Qlimits_WV(k, with0.QDesireWVpu, ActorID);
						with0.QDesireEndpu = min(Abs(with0.QDesireWVpu), Abs(with0.QDesireLimitedpu)) * Sign(with0.QDesireWVpu);

            // It checks kVA or Q limits and makes sure the final P and Q stay in the watt-var curve (PauloRadatz - 2/16/2021)
						Calc_PQ_WV(k, ActorID);

            //--------------------------------------------- end Main process ---------------------------------------------//

            // Sets PVSystem/Storage's kvar_out
						if(( (TNamedObject*)with0.ControlledElement  )->Get_myPName() == myDERTypes[PVSys])
						{
							((TPVsystemObj*)DERelem)->Set_Presentkvar(with0.QDesiredWV);
							((TPVsystemObj*)DERelem)->Set_PresentkW(with0.PLimitEndpu * min(with0.FkVArating, with0.FDCkWRated));
						}
						else
							((TStorageObj*)DERelem)->Set_kvarRequested(with0.QDesiredWV);

            // Uptates PresentkW and Presentkvar considering watt and var priorities
						if(( (TNamedObject*)with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
						{
							((TPVsystemObj*) DERelem)->SetNominalPVSystemOuput(ActorID);
							if(with0.QDesiredWV  >= 0.0)
								with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
							else
								with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
						}
						else
						{
							((TStorageObj*) DERelem)->SetNominalStorageOutput(ActorID);
							if(with0.QDesiredWV  >= 0.0)
								with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
							else
								with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
						}

            // Values used in convergence
						with0.QoutputVVpu		= with0.Qoutputpu ;
						with0.FAvgpVpuPrior		= with0.FPresentVpu ;

            // Values used in CalcQVVcurve_desiredpu
						if(( (TNamedObject*)with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
						{
							with0.QOld		= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
							with0.QOldVV	= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
							if(ShowEventLog)
								AppendToEventLog(String("InvControl.") + this->get_Name()
	           + ", "
	           + DERelem->Get_QualifiedName(), Format("WATTVAR mode requested PVSystem output var level to**, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredWV, ((TPVsystemObj*)DERelem)->Get_Presentkvar()), ActorID);
						}
						else
						{
							with0.QOld		= ((TStorageObj*) DERelem)->Get_Presentkvar();
							with0.QOldVV	= ((TStorageObj*) DERelem)->Get_Presentkvar();
							if(ShowEventLog)
								AppendToEventLog(String("InvControl.") + this->get_Name() + ", " + DERelem->Get_QualifiedName(), Format("WATTVAR mode requested Storage output var level to **, kvar = %.5g. Actual output set to kvar= %.5g.", with0.QDesiredWV, ((TStorageObj*)DERelem)->Get_Presentkvar()), ActorID);
						}

        /*Smart Inverter DRC function*/
					}
					else
					{
						if ((ControlMode == DRC) && (CombiControlMode == NONE_COMBMODE) && (Get_PendingChange(k) == CHANGEVARLEVEL))

            // Set var mode to VARMODEKVAR to indicate we might change kvar
						{
							if(( (TNamedObject*)with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
							{
								((TPVsystemObj*)DERelem)->Set_VWmode(false);
								((TPVsystemObj*)DERelem)->Set_Varmode(VARMODEKVAR);
								((TPVsystemObj*)DERelem)->Set_DRCmode(true);
							}
							else
							{
								((TStorageObj*)DERelem)->Set_VWmode(false);
								((TStorageObj*)DERelem)->Set_Varmode(VARMODEKVAR);
								((TStorageObj*)DERelem)->Set_DRCmode(true);
							}

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireDRCpu[k]
							CalcQDRC_desiredpu(k, ActorID);

            // LPF or RF activated
							if(RateofChangeMode == LPF)
							{
								CalcLPF(k, "VARS", with0.QDesireDRCpu , ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
								Check_Qlimits(k, with0.QDesireOptionpu , ActorID);
								with0.QDesireEndpu  = min(Abs(with0.QDesireLimitedpu ), Abs(with0.QDesireOptionpu )) * Sign(with0.QDesireOptionpu );
							}
							else
							{
								if(RateofChangeMode == RISEFALL)
								{
									CalcRF(k, "VARS", with0.QDesireDRCpu , ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
									Check_Qlimits(k, with0.QDesireOptionpu , ActorID);
									with0.QDesireEndpu  = min(Abs(with0.QDesireLimitedpu ), Abs(with0.QDesireOptionpu )) * Sign(with0.QDesireOptionpu );
								}
								else

                // Checks kVA (watt priority) and kvarlimit limits
								{
									Check_Qlimits(k, with0.QDesireDRCpu , ActorID);
									with0.QDesireEndpu  = min(Abs(with0.QDesireDRCpu ), Abs(with0.QDesireLimitedpu )) * Sign(with0.QDesireDRCpu );
								}

            // Calculates QDesiredDRC[k]
							}
							CalcDRC_vars(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out
							if(( (TNamedObject*)with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
								((TPVsystemObj*)DERelem)->Set_Presentkvar(with0.QDesiredDRC);
							else
								((TStorageObj*)DERelem)->Set_kvarRequested(with0.QDesiredDRC);

            // Uptates PresentkW and Presentkvar considering watt and var priorities
							if(( (TNamedObject*)with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
							{
								((TPVsystemObj*) DERelem)->SetNominalPVSystemOuput(ActorID);
								if(with0.QDesiredDRC  >= 0.0)
									with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
								else
									with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
							}
							else
							{
								((TStorageObj*) DERelem)->SetNominalStorageOutput(ActorID);
								if(with0.QDesiredDRC  >= 0.0)
									with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
								else
									with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
							}

            // Values used in convergence
							with0.QoutputDRCpu		= with0.Qoutputpu ;
							with0.FAvgpDRCVpuPrior  = with0.FPresentDRCVpu ;

            // Values used in CalcDRC_vars
							if(( (TNamedObject*)with0.ControlledElement  )->Get_myPName() == myDERTypes[PVSys])
							{
								with0.QOld		= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
								with0.QOldDRC	= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
								if(ShowEventLog)
									AppendToEventLog(String("InvControl.") + this->get_Name()
	           + ", "
	           + DERelem->Get_QualifiedName(), Format("DRC mode requested PVSystem output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredDRC, ((TPVsystemObj*)DERelem)->Get_Presentkvar()), ActorID);
							}
							else
							{
								with0.QOld  = ((TStorageObj*) DERelem)->Get_Presentkvar();
								with0.QOldDRC  = ((TStorageObj*) DERelem)->Get_Presentkvar();
								if(ShowEventLog)
									AppendToEventLog(String("InvControl.") + this->get_Name() + ", " + DERelem->Get_QualifiedName(), Format("DRC mode requested Storage output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredDRC, ((TStorageObj*)DERelem)->Get_Presentkvar()), ActorID);
							}

        /*Smart Inverter VV_DRC function*/
						}
						else
						{
							if ((ControlMode == NONE_MODE) && (CombiControlMode == VV_DRC) && (Get_PendingChange(k) == CHANGEDRCVVARLEVEL))

            // Set var mode to VARMODEKVAR to indicate we might change kvar
							{
								if(( (TNamedObject*) with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
								{
									((TPVsystemObj*)DERelem)->Set_VWmode(false);
									((TPVsystemObj*)DERelem)->Set_Varmode(VARMODEKVAR);
									((TPVsystemObj*)DERelem)->Set_VVmode(true);
									((TPVsystemObj*)DERelem)->Set_DRCmode(true);
								}
								else
								{
									((TStorageObj*)DERelem)->Set_VWmode(false);
									((TStorageObj*)DERelem)->Set_Varmode(VARMODEKVAR);
									((TStorageObj*)DERelem)->Set_VVmode(true);
									((TStorageObj*)DERelem)->Set_DRCmode(true);
								}

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k] and  QDesireDRCpu[k]
								CalcQVVcurve_desiredpu(k, ActorID);
								CalcQDRC_desiredpu(k, ActorID);

            // LPF or RF activated
								if(RateofChangeMode == LPF)
								{
									CalcLPF(k, "VARS", with0.QDesireVVpu  + with0.QDesireDRCpu , ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
									Check_Qlimits(k, with0.QDesireOptionpu , ActorID);
									with0.QDesireEndpu  = min(Abs(with0.QDesireLimitedpu ), Abs(with0.QDesireOptionpu )) * Sign(with0.QDesireOptionpu );
								}
								else
								{
									if(RateofChangeMode == RISEFALL)
									{
										CalcRF(k, "VARS", with0.QDesireVVpu  + with0.QDesireDRCpu , ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
										Check_Qlimits(k, with0.QDesireOptionpu , ActorID);
										with0.QDesireEndpu  = min(Abs(with0.QDesireLimitedpu ), Abs(with0.QDesireOptionpu )) * Sign(with0.QDesireOptionpu );
									}
									else

                // Checks kVA (watt priority) and kvarlimit limits
									{
										Check_Qlimits(k, with0.QDesireVVpu  + with0.QDesireDRCpu , ActorID);
										with0.QDesireEndpu  = min(Abs( (with0.QDesireVVpu  + with0.QDesireDRCpu )), Abs(with0.QDesireLimitedpu )) * Sign(with0.QDesireVVpu  + with0.QDesireDRCpu );
									}

            // Calculates QDesiredVVDRC[k]
								}
								CalcVVDRC_vars(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out
								if(( (TNamedObject*)with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
									((TPVsystemObj*)DERelem)->Set_Presentkvar(with0.QDesiredVVDRC);
								else
									((TStorageObj*)DERelem)->Set_kvarRequested(with0.QDesiredVVDRC);

            // Uptates PresentkW and Presentkvar considering watt and var priorities
								if(( (TNamedObject*)with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
								{
									((TPVsystemObj*) DERelem)->SetNominalPVSystemOuput(ActorID);
									if(with0.QDesiredVVDRC  >= 0.0)
										with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
									else
										with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
								}
								else
								{
									((TStorageObj*) DERelem)->SetNominalStorageOutput(ActorID);
									if(with0.QDesiredVVDRC  >= 0.0)
										with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
									else
										with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
								}

            // Values used in convergence
								with0.QoutputVVDRCpu	= with0.Qoutputpu ;
								with0.FAvgpVpuPrior		= with0.FPresentVpu ;
								with0.FAvgpDRCVpuPrior  = with0.FPresentDRCVpu ;

            // Values used in CalcQVVcurve_desiredpu and CalcVVDRC_vars
								if(( (TNamedObject*)with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
								{
									with0.QOld			= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
									with0.QOldVVDRC		= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
									if(ShowEventLog)
										AppendToEventLog(String("InvControl.") + this->get_Name()
	           + ", "
	           + DERelem->Get_QualifiedName(), Format("**VV_DRC mode requested PVSystem output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredVVDRC, ((TPVsystemObj*)DERelem)->Get_Presentkvar()), ActorID);
								}
								else
								{
									with0.QOld			= ((TStorageObj*) DERelem)->Get_Presentkvar();
									with0.QOldVVDRC		= ((TStorageObj*) DERelem)->Get_Presentkvar();
									if(ShowEventLog)
										AppendToEventLog(String("InvControl.") + this->get_Name() + ", " + DERelem->Get_QualifiedName(), Format("**VV_DRC mode requested Storage output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredVVDRC, ((TStorageObj*)DERelem)->Get_Presentkvar()), ActorID);
								}

        /*Smart Inverter volt-watt function*/
							}
							else
							{
								if ((ControlMode == VOLTWATT) && (CombiControlMode == NONE_COMBMODE) && (Get_PendingChange(k) == CHANGEWATTLEVEL))
								{
									if(( (TNamedObject*) with0.ControlledElement )->Get_myPName() == myDERTypes[PVSys])
									{
										((TPVsystemObj*)DERelem)->Set_VWmode(true);
									}
									else
									{
										((TStorageObj*)DERelem)->Set_VWmode(true);
									}

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QVWcurve_limitpu[k]
									CalcPVWcurve_limitpu(k, ActorID);

            // LPF or RF activated
									if(RateofChangeMode == LPF)
									{
										CalcLPF(k, "WATTS", with0.PLimitVWpu , ActorID);
                // Checks kVA (var priority) and pctPmpp limits
										Check_Plimits(k, with0.PLimitOptionpu , ActorID);
										with0.PLimitEndpu  = min(with0.PLimitLimitedpu , with0.PLimitOptionpu );
									}
									else
									{
										if(RateofChangeMode == RISEFALL)
										{
											CalcRF(k, "WATTS", with0.PLimitVWpu , ActorID);
                // Checks kVA (var priority) and pctPmpp limits
											Check_Plimits(k, with0.PLimitOptionpu , ActorID);
											with0.PLimitEndpu  = min(with0.PLimitLimitedpu , with0.PLimitOptionpu );
										}
										else

                // Checks kVA (var priority) and pctPmpp limits
										{
											Check_Plimits(k, with0.PLimitVWpu , ActorID);
											with0.PLimitEndpu  = min(Abs(with0.PLimitLimitedpu ), Abs(with0.PLimitVWpu )) * Sign(with0.PLimitVWpu );
										}

            // Calculates PLimitVW[k] through the convergence algorithm
									}
									CalcVoltWatt_watts(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kW_out
									if(( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
									{
										((TPVsystemObj*)DERelem)->Set_PresentkW(with0.PLimitVW);

                // Uptates PresentkW and Presentkvar considering watt and var priorities
										((TPVsystemObj*) DERelem)->SetNominalPVSystemOuput(ActorID);
									}
									else
									{
										((TStorageObj*)DERelem)->Set_kWRequested(with0.PLimitVW);

                // Uptates PresentkW and Presentkvar considering watt and var priorities
										((TStorageObj*) DERelem)->SetNominalStorageOutput(ActorID);
									}


            // Values used in convergence
									with0.FAvgpVpuPrior		= with0.FPresentVpu ;
									with0.POldVWpu			= with0.PLimitVW  / with0.PBase ;

            // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
									if(( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
									{
										if((Abs(with0.PLimitVW ) > 0.0) && (double(Abs( (((TPVsystemObj*) DERelem)->Get_PresentkW() - with0.PLimitVW))) / with0.PLimitVW  > 0.0001))
											with0.FVWOperation  = 0; // 0.01% is the value chosen at the moment
										if(ShowEventLog)
											AppendToEventLog(String("InvControl.") + this->get_Name()
										   + ", "
										   + DERelem->Get_QualifiedName(), Format("**VOLTWATT mode set PVSystem kw output limit to **, kw= %.5g. Actual output is kw= %.5g.", with0.PLimitVW, ((TPVsystemObj*)DERelem)->Get_PresentkW()), ActorID);
									}
									else
									{
										if(double(Abs(Abs( ((TStorageObj*) DERelem)->Get_PresentkW()) - with0.PLimitVW )) / with0.PLimitVW  > 0.0001)
											with0.FVWOperation  = 0; // 0.01% is the value chosen at the moment
										if(ShowEventLog)
											AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
											DERelem->Get_QualifiedName(), Format("**VOLTWATT mode set Storage kw output limit to ** kw= %.5g. Actual output is kw= %.5g.", with0.PLimitVW, ((TStorageObj*)DERelem)->Get_PresentkW()), ActorID);
									}
								}
								else
								{
									if ((ControlMode == NONE_MODE) && (CombiControlMode == VV_VW) && (Get_PendingChange(k) == CHANGEWATTVARLEVEL))
									{
										if(( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
										{
											((TPVsystemObj*)DERelem)->Set_VWmode(true);
											((TPVsystemObj*)DERelem)->Set_Varmode(VARMODEKVAR);
											((TPVsystemObj*)DERelem)->Set_VVmode(true);
										}
										else
										{
											((TStorageObj*)DERelem)->Set_VWmode(true);
											((TStorageObj*)DERelem)->Set_Varmode(VARMODEKVAR);
											((TStorageObj*)DERelem)->Set_VVmode(true);
										}

            //--------------------------------------------- Main process ---------------------------------------------//

            // Calculates QDesireVVpu[k] and QVWcurve_limitpu
										CalcPVWcurve_limitpu(k, ActorID);
										CalcQVVcurve_desiredpu(k, ActorID);

            // LPF or RF activated
										if(RateofChangeMode == LPF)
										{
											CalcLPF(k, "VARS", with0.QDesireVVpu , ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
											Check_Qlimits(k, with0.QDesireOptionpu , ActorID);
											with0.QDesireEndpu  = min(Abs(with0.QDesireLimitedpu ), Abs(with0.QDesireOptionpu )) * Sign(with0.QDesireOptionpu );
											CalcLPF(k, "WATTS", with0.PLimitVWpu , ActorID);
                // Checks kVA (var priority) and pctPmpp limits
											Check_Plimits(k, with0.PLimitOptionpu , ActorID);
											with0.PLimitEndpu  = min(with0.PLimitLimitedpu , with0.PLimitOptionpu );
										}
										else
										{
											if(RateofChangeMode == RISEFALL)
											{
												CalcRF(k, "VARS", with0.QDesireVVpu , ActorID);
                // Checks kVA (watt priority) and kvarlimit limits
												Check_Qlimits(k, with0.QDesireOptionpu , ActorID);
												with0.QDesireEndpu  = min(Abs(with0.QDesireLimitedpu ), Abs(with0.QDesireOptionpu )) * Sign(with0.QDesireOptionpu );
												CalcRF(k, "WATTS", with0.PLimitVWpu , ActorID);
                // Checks kVA (var priority) and pctPmpp limits
												Check_Plimits(k, with0.PLimitOptionpu , ActorID);
												with0.PLimitEndpu  = min(with0.PLimitLimitedpu , with0.PLimitOptionpu );
											}
											else

                // Checks kVA (watt priority) and kvarlimit limits
											{
												Check_Qlimits(k, with0.QDesireVVpu , ActorID);
												with0.QDesireEndpu  = min(Abs(with0.QDesireVVpu ), Abs(with0.QDesireLimitedpu )) * Sign(with0.QDesireVVpu );

                // Checks kVA (var priority) and pctPmpp limits
												Check_Plimits(k, with0.PLimitVWpu , ActorID);
												with0.PLimitEndpu  = min(Abs(with0.PLimitLimitedpu ), Abs(with0.PLimitVWpu )) * Sign(with0.PLimitVWpu );
											}

            // Calculates PLimitVW[k] and QDesiredVV[k] through the convergence algorithm
										}
										CalcVoltWatt_watts(k, ActorID);
										CalcVoltVar_vars(k, ActorID);

            //--------------------------------------------- end main process ---------------------------------------------//

            // Sets DER kvar_out and kW_out
										if(( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
										{
											((TPVsystemObj*) DERelem)->Set_Presentkvar(with0.QDesiredVV);
											((TPVsystemObj*) DERelem)->Set_PresentkW(with0.PLimitVW);
										}
										else
										{
											((TStorageObj*)DERelem)->Set_kvarRequested(with0.QDesiredVV);
											((TStorageObj*)DERelem)->Set_kWRequested(with0.PLimitVW);
										}

            // Uptates PresentkW and Presentkvar considering watt and var priorities
										if(( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
										{
											((TPVsystemObj*) DERelem)->SetNominalPVSystemOuput(ActorID);
											if(with0.QDesiredVV  >= 0.0)
												with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
											else
												with0.Qoutputpu  = ((TPVsystemObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
										}
										else
										{
											((TStorageObj*) DERelem)->SetNominalStorageOutput(ActorID);
											if(with0.QDesiredVV  >= 0.0)
												with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoom ;
											else
												with0.Qoutputpu  = ((TStorageObj*) DERelem)->Get_Presentkvar() / with0.QHeadRoomNeg ;
										}

            // Values used in convergence
										with0.QoutputVVpu		= with0.Qoutputpu ;
										with0.FAvgpVpuPrior		= with0.FPresentVpu ;
										with0.POldVWpu			= with0.PLimitVW  / with0.PBase ;

            // Values used in CalcQVVcurve_desiredpu
										if(( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
										{
											with0.QOld		= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
											with0.QOldVV	= ((TPVsystemObj*) DERelem)->Get_Presentkvar();
											if(ShowEventLog)
												AppendToEventLog(String("InvControl.") + this->get_Name()
											   + ", "
											   + DERelem->Get_QualifiedName(), Format("**VV_VW mode requested PVSystem output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredVV, ((TPVsystemObj*)DERelem)->Get_Presentkvar()), ActorID);
										}
										else
										{
											with0.QOld		= ((TStorageObj*) DERelem)->Get_Presentkvar();
											with0.QOldVV	= ((TStorageObj*) DERelem)->Get_Presentkvar();
											if(ShowEventLog)
												AppendToEventLog(String("InvControl.") + this->get_Name() + ", " + ((TNamedObject*)DERelem)->Get_QualifiedName(), Format("**VV_VW mode requested Storage output var level to **, kvar= %.5g. Actual output set to kvar= %.5g.", with0.QDesiredVV, ((TStorageObj*)DERelem)->Get_Presentkvar()), ActorID);
										}

            // Flag has to do set to 0 when kW_out is lower than Ptemp (max power allowed from volt-watt function)
										if(( (TNamedObject*) (with0.ControlledElement)  )->Get_myPName() == myDERTypes[PVSys])
										{
											if(double(Abs( (((TPVsystemObj*) DERelem)->Get_PresentkW() - with0.PLimitVW))) / with0.PLimitVW  > 0.0001)
												with0.FVWOperation  = 0; // 0.01% is the value chosen at the moment
											if(ShowEventLog)
												AppendToEventLog(String("InvControl.") + this->get_Name()
											   + ", "
											   + ((TNamedObject*) DERelem)->Get_QualifiedName(), Format("**VV_VW mode set PVSystem kw output limit to **, kw= %.5g. Actual output is kw= %.5g.", with0.PLimitVW, ((TPVsystemObj*)DERelem)->Get_PresentkW()), ActorID);
										}
										else
										{
											if(double(Abs(Abs( ((TStorageObj*) DERelem)->Get_PresentkW()) - with0.PLimitVW )) / with0.PLimitVW  > 0.0001)
												with0.FVWOperation  = 0; // 0.01% is the value chosen at the moment
											if(ShowEventLog)
												AppendToEventLog(String("InvControl.") + this->get_Name() + ", " + ((TNamedObject*)DERelem)->Get_QualifiedName(), Format("**VV_VW mode set Storage kw output limit to** kw= %.5g. Actual output is kw= %.5g.", with0.PLimitVW, ((TStorageObj*)DERelem)->Get_PresentkW()), ActorID);
										}
									}
									else
									{
										if (ControlMode == GFM)
										{
											auto& WS = ActiveCircuit[ActiveActor]->Solution;

											bool DER_OL = false;
											if (with0.ControlledElement->GFM_Mode)
											{
												if (with0.ControlledElement->Get_myPName() == myDERTypes[EStorage])
												{
													if (((TStorageObj*)with0.ControlledElement)->myDynVars.ILimit <= 0)              // If there is no Amps limit, check OL
													{
														if (((TStorageObj*)with0.ControlledElement)->CheckOLInverter(ActorID))
														{
															if (!WS->IsDynamicModel)
															{
																DER_OL = true;
																((TStorageObj*)with0.ControlledElement)->Set_StorageState(0);        // It's burning, Turn it off
																((TStorageObj*)with0.ControlledElement)->FStateChanged = true;
															}
															else
																((TStorageObj*)with0.ControlledElement)->myDynVars.ResetIBR = true;     // The dynamic alg will take it to safety
														}
													}
												}
												else
												{
													if (!WS->IsDynamicModel)
														DER_OL = ((TPVsystemObj*)with0.ControlledElement)->CheckOLInverter(ActorID);
													else
													{
														if (((TPVsystemObj*)with0.ControlledElement)->CheckOLInverter(ActorID))
															((TPVsystemObj*)with0.ControlledElement)->myDynVars.ResetIBR = true;
													}
												}
												if (DER_OL)
												{
													with0.ControlledElement->GFM_Mode		= false;
													with0.ControlledElement->Set_YprimInvalid(ActorID, true);
												}
											}
										}
									}
								}
							}
						}
					}
				}
			}
		}
		ActiveCircuit[ActorID]->Solution->LoadsNeedUpdating = true;
		Set_PendingChange(None, k);
		DERelem = nullptr;
	}
}

 int TInvControlObj::NextDeltaPhase(int iphs, int i)
{
     int Result = iphs + 1;
    if (Result > CtrlVars[i].NCondsDER)
        Result = 1;

	return Result;
 }

void TInvControlObj::GetmonVoltage(int ActorID, double& Vpresent, int i, double BaseKV, int connection)
{
	int j = 0;
	TDSSBus* rBus = nullptr;
	int NumNodes = 0;
	complex V = {};
	complex vi = {};
	complex vj = {};


	auto& withi = CtrlVars[i];
	if(FUsingMonBuses)
	{
		int stop = 0;
		for(stop =  FMonBuses.size() - 1, j = 0; j <= stop; j++)
		{
			FMonBusesIndex = ActiveCircuit[ActorID]->BusList.Find((FMonBuses)[j]);
			rBus = ActiveCircuit[ActorID]->Buses[FMonBusesIndex - 1];
			if(FMonBusesNodes[j].size() == 2)
			{
				if(!ADiakoptics || (ActorID == 1))
				{
					vi = (ActiveCircuit[ActorID]->Solution->NodeV[rBus->GetRef(FMonBusesNodes[j][0])]);
					vj = (ActiveCircuit[ActorID]->Solution->NodeV[rBus->GetRef(FMonBusesNodes[j][1])]);
				}
				else
				{
					vi = (ActiveCircuit[ActorID]->Solution->VoltInActor1(rBus->GetRef(FMonBusesNodes[j][0])));
					vj = (ActiveCircuit[ActorID]->Solution->VoltInActor1(rBus->GetRef(FMonBusesNodes[j][1])));
				}
				withi.cBuffer[j] = cmulreal(csub(vi, vj), BaseKV * 1000.0 / (FMonBusesVbase)[j + 1 - 1]);
				V = withi.cBuffer[j];
			}
			else
			{
				if(!ADiakoptics || (ActorID == 1))
					withi.cBuffer[j] = cmulreal(ActiveCircuit[ActorID]->Solution->NodeV[rBus->GetRef(FMonBusesNodes[j][0])], BaseKV * 1000.0 / (FMonBusesVbase)[j + 1 - 1]);
				else
					withi.cBuffer[j] = cmulreal(ActiveCircuit[ActorID]->Solution->VoltInActor1(rBus->GetRef(FMonBusesNodes[j][0])), BaseKV * 1000.0 / (FMonBusesVbase)[j + 1 - 1]);
				V = withi.cBuffer[j];
			}
		}
		switch(FMonBusesPhase)
		{
			case 	AVGPHASES:
			{
				int stop = 0;
				Vpresent = 0.0;
				for(stop =  FMonBuses.size() - 1, j = 0; j <= stop; j++)
				{
					Vpresent = Vpresent + cabs(withi.cBuffer[j]);
				}
				Vpresent = Vpresent / FMonBuses.size();
			}
			break;
			case 	MAXPHASE:
			{
				int stop = 0;
				Vpresent = 0.0;
				for(stop =  FMonBuses.size() - 1, j = 0; j <= stop; j++)
				{
					Vpresent = max(Vpresent, cabs(withi.cBuffer[j]));
				}
			}
			break;
			case 	MINPHASE:
			{
				int stop = 0;
				Vpresent = 1.0e50;
				for(stop =  FMonBuses.size() - 1, j = 0; j <= stop; j++)
				{
					Vpresent = min(Vpresent, cabs(withi.cBuffer[j]));
				}
			}
			break;
			default:
			Vpresent = cabs(withi.cBuffer[FMonBusesPhase - 1]);
			break;
		}
	}
	else
	{
		int stop = 0;
		( withi.ControlledElement )->ComputeVterminal(ActorID);
		NumNodes = (  (withi.ControlledElement) )->Get_NPhases();
		for(stop = NumNodes, j = 1; j <= stop; j++)
		{
			//withi.cBuffer[j - 1] = (withi.ControlledElement)->Vterminal[j - 1]; // change proposed by Celso Rocha, 10/04/2024
            switch (connection)
            {
				case 1: 
					withi.cBuffer[j - 1] = csub((withi.ControlledElement)->Vterminal[j - 1], (withi.ControlledElement)->Vterminal[NextDeltaPhase(j, i) - 1]);
					break;
				default:
					withi.cBuffer[j - 1] = (withi.ControlledElement)->Vterminal[j - 1];
					break;
            }
		}
		switch(FMonBusesPhase)
		{
			case 	AVGPHASES:
			{
				int stop = 0;
				Vpresent = 0.0;
				for(stop = NumNodes, j = 1; j <= stop; j++)
				{
					Vpresent = Vpresent + cabs(withi.cBuffer[j - 1]);
				}
				Vpresent = Vpresent / NumNodes;
			}
			break;
			case 	MAXPHASE:
			{
				int stop = 0;
				Vpresent = 0.0;
				for(stop = NumNodes, j = 1; j <= stop; j++)
				{
					Vpresent = max(Vpresent, cabs(withi.cBuffer[j - 1]));
				}
			}
			break;
			case 	MINPHASE:
			{
				int stop = 0;
				Vpresent = 1.0e50;
				for(stop = NumNodes, j = 1; j <= stop; j++)
				{
					Vpresent = min(Vpresent, cabs(withi.cBuffer[j - 1]));
				}
			}
			break;
			default:
			Vpresent = cabs(withi.cBuffer[FMonBusesPhase - 1]);
			break;
		}
	}
}

void TInvControlObj::UpdateDERParameters(int i)
{
	/*# with (ControlledElement)[i] do */
	{
		auto& withi = CtrlVars[i];
		if( ( (TNamedObject*) (withi.ControlledElement) )->Get_myPName() == "PVSystem")
		{
			/*# with TPVsystemObj((ControlledElement)[i]) do */
			{
				auto with1 = ((TPVsystemObj*) (withi.ControlledElement) );
				withi.CondOffset			= ( ( (TDSSCktElement*) with1 )->Get_NTerms() - 1) * withi.NCondsDER ; // for speedy sampling
				withi.FVBase				= with1->VBase;
				withi.FVarFollowInverter	= with1->Get_VarFollowInverter();
				withi.FInverterON			= with1->Get_InverterON();
				withi.FpresentkW			= with1->Get_PresentkW();
				withi.FkVArating			= with1->Get_FkVArating();
				withi.Fpresentkvar			= with1->Get_Presentkvar();
				withi.Fkvarlimit			= with1->Get_Fkvarlimit();
				withi.FkvarLimitNeg			= with1->get_Fkvarlimitneg();
				withi.FCurrentkvarLimit		= with1->CurrentkvarLimit;	
				withi.FCurrentkvarLimitNeg  = with1->CurrentkvarLimitNeg;
				withi.FDCkWRated			= with1->Get_FPmpp();
				withi.FpctDCkWRated			= with1->Get_FpuPmpp();
				withi.FEffFactor			= with1->PVSystemVars.EffFactor;
				withi.FDCkW					= with1->PVSystemVars.PanelkW;
				withi.FPPriority			= with1->PVSystemVars.P_priority;
			}
		}
		else
		{
			if(( (TNamedObject*) (withi.ControlledElement)  )->Get_myPName() == myDERTypes[EStorage])
			{
				/*# with TStorageObj((ControlledElement) ) do */
				{
					auto with2					= ((TStorageObj*) (withi.ControlledElement) );
					withi.FVBase				= with2->VBase;
					withi.FVarFollowInverter	= with2->Get_VarFollowInverter();
					withi.FInverterON			= with2->Get_InverterON();
					withi.FpresentkW			= with2->Get_PresentkW();
					withi.FkVArating			= with2->Get_FkVARating();
					withi.Fpresentkvar			= with2->Get_Presentkvar();
					withi.Fkvarlimit			= with2->Get_Fkvarlimit();
					withi.FkvarLimitNeg			= with2->Get_Fkvarlimitneg();
					withi.FCurrentkvarLimit		= with2->CurrentkvarLimit;
					withi.FCurrentkvarLimitNeg  = with2->CurrentkvarLimitNeg;
					withi.FDCkWRated			= with2->StorageVars.kWrating;
					withi.FpctDCkWRated			= with2->Get_FpctkWrated();
					withi.FEffFactor			= with2->StorageVars.EffFactor;
					withi.FDCkW					= 0.0; // not using it (using TStorageObj.DCkW directly)
					withi.FPPriority			= with2->StorageVars.P_Priority;
				}
			}
		}
	}
}

void TInvControlObj::sample(int ActorID)
{
	int				i = 0;
	double			BaseKV = 0.0,
					Vpresent = 0.0;
	TPVsystemObj*	PVSyst = nullptr;
	TStorageObj*	Storage = nullptr;
    // if list is not defined, go make one from all PVSystem/Storage in circuit
	if(FDERPointerList->get_myNumList() == 0)
		RecalcElementData(ActorID);
	if(FListSize > 0)
        // if an InvControl controls more than one PVSystem/Storage, control each one
        // separately based on the PVSystem/Storage's terminal voltages, etc.
	{
		int stop = 0;
		for(stop = FDERPointerList->get_myNumList(), i = 1; i <= stop; i++)
		{
			auto& withi = CtrlVars[i];
			UpdateDERParameters(i);
			if(( (TNamedObject*) (withi.ControlledElement) )->Get_myPName() == myDERTypes[PVSys])
				PVSyst = (TPVsystemObj*) withi.ControlledElement;
			else
				Storage = (TStorageObj*) withi.ControlledElement;
			BaseKV = withi.FVBase / 1000.0; // It's a line-to-ground voltage

			if (ASSIGNED(PVSyst))
				GetmonVoltage(ActorID, Vpresent, i, BaseKV,PVSyst->Connection);
            else
				GetmonVoltage(ActorID, Vpresent, i, BaseKV, Storage->Connection);

            // for reporting Vpriorpu correctly in EventLog (this update is normally perform at DoPendingAction)
			if(ActiveCircuit[ActorID]->Solution->ControlIteration == 1)
			{
				withi.FAvgpVpuPrior		= withi.FPresentVpu ;
				withi.FAvgpDRCVpuPrior  = withi.FPresentDRCVpu ;
			}
			withi.kW_out_desired  = withi.FpresentkW ; // necessary to update kW_out_desired at every control iteration for Storage with SC

            // Help says that it must be used just for vv and vw
            // convert to per-unit on bus' kvbase, or
            // if using averaging window values, then set prior voltage to averaging window
			if((FVoltage_CurveX_ref == 1) && (withi.FRollAvgWindow->Get_AvgVal() != 0.0))
				withi.FPresentVpu  = Vpresent / (withi.FRollAvgWindow->Get_AvgVal());
			else
			{
				if((FVoltage_CurveX_ref == 2) && (withi.FRollAvgWindow->Get_AvgVal() != 0.0))
					withi.FPresentVpu  = (withi.FRollAvgWindow->Get_AvgVal()) / (BaseKV * 1000.0);
				else
					withi.FPresentVpu  = Vpresent / (BaseKV * 1000.0);
			}
			withi.FPresentDRCVpu  = Vpresent / (BaseKV * 1000.0);

            // Sets internal variables of controlled element.
            // FVreg is the pu voltage used in the volt-var and volt-watt curves
			FVreg = withi.FPresentVpu;
			// First, determine what control mode are we
			if (CombiControlMode != 0)
			{
				// IT's CombiControl mode
				switch (CombiControlMode)
				{
				case	VV_DRC:
					// Sets internal variables of controlled element.
					// FVVDRCOperation is a flag which indicates if VVDRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
					if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
					{
						PVSyst->Set_Variable(5, FVreg);
						PVSyst->Set_Variable(6, withi.FDRCRollAvgWindow->Get_AvgVal() / (BaseKV * 1000.0)); // save rolling average voltage in monitor
						PVSyst->Set_Variable(10, withi.FVVDRCOperation);
					}
					else
					{
						Storage->Set_Variable(14, FVreg);
						Storage->Set_Variable(15, withi.FDRCRollAvgWindow->Get_AvgVal() / (BaseKV * 1000.0)); // save rolling average voltage in monitor
						Storage->Set_Variable(19, withi.FVVDRCOperation);
					}
					// if inverter is off then exit
					if ((withi.FInverterON == false) && (withi.FVarFollowInverter == true))
						continue;

					// if the volt-var curve does not exist, exit
					if (Fvvc_curvename.size() == 0)
					{
						DoSimpleMsg("XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.", 382);
						return;
					}
					if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
					{
						PVSyst->Set_VVmode(true);
						PVSyst->Set_DRCmode(true);
					}
					else
					{
						Storage->Set_VVmode(true);
						Storage->Set_DRCmode(true);
					}
					//DRC triggers
					if (withi.priorDRCRollAvgWindow == 0.0)
					{
						if ((Abs((withi.FPresentDRCVpu - withi.FAvgpDRCVpuPrior)) > FVoltageChangeTolerance) || (Abs((withi.FPresentVpu - withi.FAvgpVpuPrior)) > FVoltageChangeTolerance))
							// Resets DER state variable only if it has not converged yet
						{
							withi.FVVDRCOperation = 0.0;
							Set_PendingChange(CHANGEDRCVVARLEVEL, i);
							/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
							{
								auto& with0 = ActiveCircuit[ActorID]->Solution->DynaVars;
								ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with0.intHour, with0.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
							}
							if (ShowEventLog)
								AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
									withi.ControlledElement->Get_QualifiedName(), Format("**Ready to change var output due to DRC trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g", withi.FPresentDRCVpu, withi.FAvgpDRCVpuPrior), ActorID);
						}
					}
					//Trigger from volt-var mode
					if (((Abs((withi.FPresentVpu - withi.FAvgpVpuPrior)) > FVoltageChangeTolerance) || (Abs((withi.FPresentDRCVpu - withi.FAvgpDRCVpuPrior)) > FVoltageChangeTolerance) || ((Abs(Abs(withi.QoutputVVDRCpu) - Abs(withi.QDesireEndpu)) > FVarChangeTolerance))) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1))
						// Resets DER state variable only if it has not converged yet
					{
						withi.FVVDRCOperation = 0.0;
						Set_PendingChange(CHANGEDRCVVARLEVEL, i);
						/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
						{
							auto& with1 = ActiveCircuit[ActorID]->Solution->DynaVars;
							ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with1.intHour, with1.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
						}
						if (ShowEventLog)
							AppendToEventLog(String("InvControl.") + this->get_Name() + ", " + ((TNamedObject*)withi.ControlledElement)->Get_QualifiedName(), Format("**Ready to change VV_DRC output due to volt-var trigger in VV_DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g", withi.FPresentVpu, withi.FAvgpVpuPrior), ActorID);
					}
					break;
				case	VV_VW:
					// Sets internal variables of controlled element.
					// FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
					// FVWOperation is a flag which indicates if volt-watt function operates or not
					// Combined modes operation is shown through TWO flags. It allows us to verify which of the individual function operates or not
					if (((TNamedObject*)withi.ControlledElement)->Get_myPName() == myDERTypes[PVSys])
					{
						PVSyst->Set_Variable(5, FVreg);
						PVSyst->Set_Variable(7, withi.FVVOperation);
						PVSyst->Set_Variable(8, withi.FVWOperation);
					}
					else
					{
						Storage->Set_Variable(14, FVreg);
						Storage->Set_Variable(16, withi.FVVOperation);
						Storage->Set_Variable(17, withi.FVWOperation);
					}

					// if inverter is off then exit
					if ((withi.FInverterON == false) && (withi.FVarFollowInverter == true))
						continue;
					// if volt-watt curve does not exist, exit
					if (((TNamedObject*)withi.ControlledElement)->Get_myPName() == myDERTypes[PVSys])
					{
						if (Fvoltwatt_curvename.size() == 0)
						{
							DoSimpleMsg("XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.", 381);
							return;
						}
					}
					else
					{
						if ((Fvoltwatt_curvename.size() == 0) && (FvoltwattCH_curvename.size() == 0))
						{
							DoSimpleMsg("XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.", 381);
							return;
						}
					}
					// if the volt-var curve does not exist, exit
					if (Fvvc_curvename.size() == 0)
					{
						DoSimpleMsg("XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.", 382);
						return;
					}
					if (((TNamedObject*)withi.ControlledElement)->Get_myPName() == myDERTypes[PVSys])
					{
						PVSyst->Set_VVmode(true);
						PVSyst->Set_VWmode(true);
					}
					else
					{
						Storage->Set_VVmode(true);
						Storage->Set_VWmode(true);
					}
					// Trigger from volt-watt mode
					if ((Abs((withi.FPresentVpu - withi.FAvgpVpuPrior)) > FVoltageChangeTolerance) || (Abs((withi.PLimitEndpu - withi.POldVWpu)) > FActivePChangeTolerance) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1))

						// Resets DER state variable only if it has not converged yet
					{
						withi.FVWOperation = 0;
						Set_PendingChange(CHANGEWATTVARLEVEL, i);
						/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
						{
							auto& with2 = ActiveCircuit[ActorID]->Solution->DynaVars;
							ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with2.intHour, with2.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
						}
						if (ShowEventLog)
							AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
								withi.ControlledElement->Get_QualifiedName(), Format("**Ready to change VV_VW output due to volt-watt trigger**, Vavgpu= %.5g, VPriorpu=%.5g", withi.FPresentVpu, withi.FAvgpVpuPrior), ActorID);
					}
					//Trigger from volt-var mode
					if (((Abs((withi.FPresentVpu - withi.FAvgpVpuPrior)) > FVoltageChangeTolerance) || ((Abs(Abs(withi.Qoutputpu) - Abs(withi.QDesireEndpu)) > FVarChangeTolerance))) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1))

						// Resets DER state variable only if it has not converged yet
					{
						withi.FVVOperation = 0;
						Set_PendingChange(CHANGEWATTVARLEVEL, i);
						/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
						{
							auto& with3 = ActiveCircuit[ActorID]->Solution->DynaVars;
							ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with3.intHour, with3.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
						}
						if (ShowEventLog)
							AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
								withi.ControlledElement->Get_QualifiedName(), Format("**Ready to change VV_VW output due to volt-var trigger**, Vavgpu= %.5g, VPriorpu=%.5g", withi.FPresentVpu, withi.FAvgpVpuPrior), ActorID);
					}
					break;
				default:
					//Do nothing
					break;
				}
			}
			else
			{
				if (ControlMode != 0)
				{
					switch (ControlMode)
					{
						case	VOLTWATT:   // volt-watt control mode
							// Sets internal variables of controlled element.
							// FVWOperation is a flag which indicates if volt-watt function operates or not
							if (withi.ControlledElement->Get_myPName() == myDERTypes[PVSys])
							{
								PVSyst->Set_Variable(5, FVreg);
								PVSyst->Set_Variable(8, withi.FVWOperation);
							}
							else
							{
								Storage->Set_Variable(14, FVreg);
								Storage->Set_Variable(17, withi.FVWOperation);
							}
							if (withi.FInverterON == false)
								continue;
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
							{
								if (Fvoltwatt_curvename.size() == 0)
								{
									DoSimpleMsg("XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.", 381);
									return;
								}
							}
							else
							{
								if ((Fvoltwatt_curvename.size() == 0) && (FvoltwattCH_curvename.size() == 0))
								{
									DoSimpleMsg("XY Curve object representing voltwatt_curve does not exist or is not tied to InvControl.", 381);
									return;
								}
							}
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
								PVSyst->Set_VWmode(true);
							else
								Storage->Set_VWmode(true);
							if ((Abs((withi.FPresentVpu - withi.FAvgpVpuPrior)) > FVoltageChangeTolerance) || (Abs((withi.PLimitEndpu - withi.POldVWpu)) > FActivePChangeTolerance) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1))

								// Resets DER state variable only if it has not converged yet
							{
								withi.FVWOperation = 0;
								Set_PendingChange(CHANGEWATTLEVEL, i);
								/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
								{
									auto& with4 = ActiveCircuit[ActorID]->Solution->DynaVars;
									ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with4.intHour, with4.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
								}
								if (ShowEventLog)
									AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
										withi.ControlledElement->Get_QualifiedName(), Format("**Ready to limit watt output due to VOLTWATT mode**, Vavgpu= %.5g, VPriorpu=%.5g", withi.FPresentVpu, withi.FAvgpVpuPrior), ActorID);
							}
							break;
						case	AVR:    // Active voltage regulation control mode
							// Sets internal variables of PVSystem/Storage.
							// FAVROperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
									// if inverter is off then exit
							if ((withi.FInverterON == false) && (withi.FVarFollowInverter == true))
								continue;
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
								PVSyst->Set_AVRmode(true);
							else
								Storage->Set_VVmode(true);

							//Trigger from AVR mode
							if (((Abs((withi.FPresentVpu - withi.FAvgpVpuPrior)) > FVoltageChangeTolerance) || ((Abs(Abs(withi.QoutputAVRpu) - Abs(withi.QDesireEndpu)) > FVarChangeTolerance)) || (Abs((withi.FPresentVpu - withi.Fv_setpointLimited)) > FVoltageChangeTolerance)) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1))

								// Resets DER state variable only if it has not converged yet
							{
								withi.FAVROperation = 0;
								Set_PendingChange(CHANGEVARLEVEL, i);
								/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
								{
									auto& with5 = ActiveCircuit[ActorID]->Solution->DynaVars;
									ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with5.intHour, with5.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
								}
								if (ShowEventLog)
									AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
										withi.ControlledElement->Get_QualifiedName(), Format("**Ready to change var output due to AVR trigger in AVR mode**, Vavgpu= %.5g, VPriorpu=%.5g, Vsetpoint=%.5g, VsetpointLimited=%.5g", withi.FPresentVpu, withi.FAvgpVpuPrior, Fv_setpoint, withi.Fv_setpointLimited), ActorID);
							}
							break;
						case	VOLTVAR:  // volt-var control mode
							// Sets internal variables of PVSystem/Storage.
							// FVVOperation is a flag which indicates if volt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
							{
								PVSyst->Set_Variable(5, FVreg);
								PVSyst->Set_Variable(7, withi.FVVOperation);
							}
							else
							{
								Storage->Set_Variable(14, FVreg);
								Storage->Set_Variable(16, withi.FVVOperation);
							}

							// if inverter is off then exit
							if ((withi.FInverterON == false) && (withi.FVarFollowInverter == true))
								continue;
							if (Fvvc_curvename.size() == 0)
							{
								DoSimpleMsg("XY Curve object representing vvc1_curve does not exist or is not tied to InvControl.", 382);
								return;
							}
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
								PVSyst->Set_VVmode(true);
							else
								Storage->Set_VVmode(true);

							//Trigger from volt-var mode
							if (((Abs((withi.FPresentVpu - withi.FAvgpVpuPrior)) > FVoltageChangeTolerance) || ((Abs(Abs(withi.QoutputVVpu) - Abs(withi.QDesireEndpu)) > FVarChangeTolerance))) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1))

								// Resets DER state variable only if it has not converged yet
							{
								withi.FVVOperation = 0;
								Set_PendingChange(CHANGEVARLEVEL, i);
								/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
								{
									auto& with6 = ActiveCircuit[ActorID]->Solution->DynaVars;
									ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with6.intHour, with6.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
								}
								if (ShowEventLog)
									AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
										withi.ControlledElement->Get_QualifiedName(), Format("**Ready to change var output due to volt-var trigger in volt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g", withi.FPresentVpu, withi.FAvgpVpuPrior), ActorID);
							}
							break;
						case	WATTPF:  // watt-pf control mode
							// Sets internal variables of PVSystem/Storage.
							// FWPOperation is a flag which indicates if watt-pf function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
							{
								PVSyst->Set_Variable(5, FVreg);
								PVSyst->Set_Variable(11, withi.FWPOperation);
							}
							else
							{
								Storage->Set_Variable(14, FVreg);
								Storage->Set_Variable(16, withi.FWPOperation);
							}

							// if inverter is off then exit
							if ((withi.FInverterON == false) && (withi.FVarFollowInverter == true))
								continue;
							if (Fwattpf_curvename.size() == 0)
							{
								DoSimpleMsg("XY Curve object representing wattpf_curve does not exist or is not tied to InvControl.", 382);
								return;
							}
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == "PVSystem")
								PVSyst->Set_WPmode(true);
							else
								Storage->Set_WPmode(true);

							//Trigger from volt-var mode
							if (((Abs((withi.FPresentVpu - withi.FAvgpVpuPrior)) > FVoltageChangeTolerance) || ((Abs(Abs(withi.QoutputVVpu) - Abs(withi.QDesireEndpu)) > FVarChangeTolerance))) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1))

								// Resets DER state variable only if it has not converged yet
							{
								withi.FWPOperation = 0;
								Set_PendingChange(CHANGEVARLEVEL, i);
								/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
								{
									auto& with7 = ActiveCircuit[ActorID]->Solution->DynaVars;
									ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with7.intHour, with7.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
								}
								if (ShowEventLog)
									AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
										withi.ControlledElement->Get_QualifiedName(), Format("**Ready to change var output due to watt-pf trigger in watt-pf mode**, Vavgpu= %.5g, VPriorpu=%.5g", withi.FPresentVpu, withi.FAvgpVpuPrior), ActorID);
							}
							break;
						case	WATTVAR:		// watt-var control mode
							// Sets internal variables of PVSystem/Storage.
							// FWVOperation is a flag which indicates if watt-var function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
							{
								PVSyst->Set_Variable(5, FVreg);
								PVSyst->Set_Variable(12, withi.FWVOperation);        //CHANGE HERE
							}
							else
							{
								Storage->Set_Variable(14, FVreg);
								Storage->Set_Variable(16, withi.FWVOperation);
							}

							// if inverter is off then exit
							if ((withi.FInverterON == false) && (withi.FVarFollowInverter == true))
								continue;
							if (Fwattvar_curvename.size() == 0)
							{
								DoSimpleMsg("XY Curve object representing wattvar_curve does not exist or is not tied to InvControl.", 382);
								return;
							}
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
								PVSyst->Set_WVmode(true);
							else
								Storage->Set_WVmode(true);

							//Trigger from volt-var mode
							if (((Abs((withi.FPresentVpu - withi.FAvgpVpuPrior)) > FVoltageChangeTolerance) || ((Abs(Abs(withi.QoutputVVpu) - Abs(withi.QDesireEndpu)) > FVarChangeTolerance))) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1))

								// Resets DER state variable only if it has not converged yet
							{
								withi.FWVOperation = 0;
								Set_PendingChange(CHANGEVARLEVEL, i);
								/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
								{
									auto& with8 = ActiveCircuit[ActorID]->Solution->DynaVars;
									ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with8.intHour, with8.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
								}
								if (ShowEventLog)
									AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
										withi.ControlledElement->Get_QualifiedName(), Format("**Ready to change var output due to watt-var trigger in watt-var mode**, Vavgpu= %.5g, VPriorpu=%.5g", withi.FPresentVpu, withi.FAvgpVpuPrior), ActorID);
							}
							break;
						case	DRC:			// dynamic reactive current control mode
							// Sets internal variables of PVSystem/Storage.
							// FDRCOperation is a flag which indicates if DRC function operates or not (-1=absorbing Q, 1=injecting Q, 0=No operation)
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
							{
								PVSyst->Set_Variable(5, FVreg);
								PVSyst->Set_Variable(6, withi.FDRCRollAvgWindow->Get_AvgVal() / (BaseKV * 1000.0)); // save rolling average voltage in monitor
								PVSyst->Set_Variable(9, withi.FDRCOperation);
							}
							else
							{
								Storage->Set_Variable(14, FVreg);
								Storage->Set_Variable(15, withi.FDRCRollAvgWindow->Get_AvgVal() / (BaseKV * 1000.0)); // save rolling average voltage in monitor
								Storage->Set_Variable(18, withi.FDRCOperation);
							}

							// if inverter is off then exit
							if ((withi.FInverterON == false) && (withi.FVarFollowInverter == true))
								continue;

							//DRC triggers
							if (withi.priorDRCRollAvgWindow == 0.0)
							{
								if ((Abs((withi.FPresentDRCVpu - withi.FAvgpDRCVpuPrior)) > FVoltageChangeTolerance))

									// Resets DER state variable only if it has not converged yet
								{
									withi.FDRCOperation = 0;
									Set_PendingChange(CHANGEVARLEVEL, i);
									/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
									{
										auto& with9 = ActiveCircuit[ActorID]->Solution->DynaVars;
										ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with9.intHour, with9.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
									}
									if (ShowEventLog)
										AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
											withi.ControlledElement->Get_QualifiedName(), Format("**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g", withi.FPresentDRCVpu, withi.FAvgpDRCVpuPrior), ActorID);
								}
							}
							if (((TNamedObject*)(withi.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
								PVSyst->Set_DRCmode(true);
							else
								Storage->Set_DRCmode(true);
							if ((Abs((withi.FPresentDRCVpu - withi.FAvgpDRCVpuPrior)) > FVoltageChangeTolerance) || (Abs(Abs(withi.QoutputDRCpu) - Abs(withi.QDesireEndpu)) > FVarChangeTolerance) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1)) // TEMc; also tried checking against QDesireEndpu
							{
								Set_PendingChange(CHANGEVARLEVEL, i);
								/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
								{
									auto& with10 = ActiveCircuit[ActorID]->Solution->DynaVars;
									ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with10.intHour, with10.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
								}
								if (ShowEventLog)
									AppendToEventLog(String("InvControl.") + this->get_Name() + ", " +
										withi.ControlledElement->Get_QualifiedName(), Format("**Ready to change var output due to DRC trigger in DRC mode**, Vavgpu= %.5g, VPriorpu=%.5g, QoutPU=%.3g, QDesiredEndpu=%.3g", withi.FPresentDRCVpu, withi.FAvgpDRCVpuPrior, withi.QoutputDRCpu, withi.QDesireEndpu), ActorID);
							}
							break;
						case	GFM:		// Grid forming inverter supervision
						{
							auto& WS = ActiveCircuit[ActorID]->Solution;
							bool Valid = false;
							if (withi.ControlledElement->GFM_Mode)
							{
								// Check if it's in GFM mode
								if (withi.ControlledElement->Get_myPName() == myDERTypes[EStorage])
								{
									// storage case
									if (((TStorageObj*)withi.ControlledElement)->get_fState() == 1)                       // Check if it's in discharging mode
									{
										if (((TStorageObj*)withi.ControlledElement)->myDynVars.ILimit > 0)
											Valid = ((TStorageObj*)withi.ControlledElement)->CheckAmpsLimit(ActorID);           // Checks if reached the Amps limit
										else
											Valid = ((TStorageObj*)withi.ControlledElement)->CheckOLInverter(ActorID);          // Checks if Inv OL
									}
									Valid = Valid && !(((TStorageObj*)withi.ControlledElement)->myDynVars.ResetIBR);     // Check if we are not resetting
								}
								else
								{
									// PVSystem case
									if (((TPVsystemObj*)withi.ControlledElement)->myDynVars.ILimit > 0)
										Valid = ((TPVsystemObj*)withi.ControlledElement)->CheckAmpsLimit(ActorID);            // Checks if reached the Amps limit
									else
										Valid = ((TPVsystemObj*)withi.ControlledElement)->CheckOLInverter(ActorID);           // Checks if Inv OL
									Valid = Valid && !(((TPVsystemObj*)withi.ControlledElement)->myDynVars.ResetIBR);      // Check if we are not resetting
								}

								if (Valid)
								{
									auto& DynV = WS->DynaVars;
									ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(
										DynV.intHour, DynV.T + TimeDelay, (EControlAction)Get_PendingChange(i), 0, this, ActorID);
								}
							}
						}
							break;
						default:
							// Do nothing
							break;
					}
				}
			}
		}
	}
}

void TInvControlObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1, ""); //PVSystem/Storage list
	Set_PropertyValue(2, "VOLTVAR"); // initial mode
	Set_PropertyValue(3, ""); // initial combination mode
	Set_PropertyValue(4, "");
	Set_PropertyValue(5, "0");
	Set_PropertyValue(6, "rated");
	Set_PropertyValue(7, "0s");
	Set_PropertyValue(8, "NONE"); // voltwatt_curve
	Set_PropertyValue(9, "0.95");  //'DbVMin';
	Set_PropertyValue(10, "1.05");  // 'DbVMax';
	Set_PropertyValue(11, "0.1");  // 'ArGraLowV';
	Set_PropertyValue(12, "0.1");  // 'ArGraHiV';
	Set_PropertyValue(13, "0s"); // 'Rollingavgwindowlen';
	Set_PropertyValue(14, FloatToStr(FLAGDELTAQ)); // FdeltaQFactor
	Set_PropertyValue(15, "0.0001"); //VoltageChangeTolerance
	Set_PropertyValue(16, "0.025"); // Varchangetolerance
	Set_PropertyValue(17, "PMPPPU"); // Voltwatt y axis units
	Set_PropertyValue(18, "INACTIVE"); //rate of change limit
	Set_PropertyValue(19, "0.0"); // LPF tau constant, in seconds
	Set_PropertyValue(20, "-1.0"); // Rise/fall Limit
	Set_PropertyValue(21, FloatToStr(FLAGDELTAP)); // FdeltaPFactor
	Set_PropertyValue(22, ShowEventLog ? "YES" : "NO"); // show event log
	Set_PropertyValue(23, "VARAVAL"); // y-axis reference (and power precedence) for volt-var
	Set_PropertyValue(24, "0.01");
	Set_PropertyValue(28, "NONE"); // voltwattCH_curve
	inherited::InitPropertyValues(NumPropsThisClass);
}

bool TInvControlObj::MakeDERList()
{
	bool result = false;
	TDSSClass* PVSysClass = nullptr;
	TDSSClass* StorageClass = nullptr;
	TPVsystemObj* PVSyst = nullptr;
	TStorageObj* Storage = nullptr;
	TPCElement* DERelem = nullptr;
	int i = 0;
	int j = 0;
	int stop = 0;
	result = false;
	PVSysClass = (TDSSClass*) GetDSSClassPtr("PVSystem");
	StorageClass = (TDSSClass*) GetDSSClassPtr("Storage");
	if (FListSize > 0)    // Name list is defined - Use it
	{
		CtrlVars.resize(FListSize + 1);

		for (i = 1; i <= FListSize; i++)
		{
			auto& withi = CtrlVars[i];
			
			withi.FVpuSolution.resize(3);
			withi.cBuffer.resize(7);

			if (StripExtension(LowerCase(String((FDERNameList)[i - 1]))) == LowerCase(myDERTypes[PVSys]))
			{
				PVSyst = ((TPVsystemObj*)PVSysClass->Find(StripClassName(String((FDERNameList)[i - 1]))));
				if (ASSIGNED(PVSyst))
				{
					if (((TDSSCktElement*)PVSyst)->Get_Enabled())
						FDERPointerList->Set_New(PVSyst);
				}
				else
				{
					DoSimpleMsg(String("Error: PVSystem Element \"") + (FDERNameList)[i - 1]
						+ "\" not found.", 14403);
					return result;
				}
			}
			else
			{
				if (StripExtension(LowerCase(String((FDERNameList)[i - 1]))) == LowerCase(myDERTypes[EStorage]))
				{
					Storage = ((TStorageObj*)StorageClass->Find(StripClassName(String((FDERNameList)[i - 1]))));
					if (ASSIGNED(Storage))
					{
						if (((TDSSCktElement*)Storage)->Get_Enabled())
							FDERPointerList->Set_New(Storage);
					}
					else
					{
						DoSimpleMsg(String("Error: Storage Element \"") + (FDERNameList)[i - 1]
							+ "\" not found.", 14403);
						return result;
					}
				}
			}
		}
	}
	else

        /*Search through the entire circuit for enabled PVSystem and Storage objects and add them to the list*/
        // Adding PVSystem elements
	{
		int stop = 0;
		for(stop = PVSysClass->Get_ElementCount(), i = 1; i <= stop; i++)
		{
			PVSyst = (TPVsystemObj*) PVSysClass->ElementList.Get(i);
			if( ( (TDSSCktElement*) PVSyst )->Get_Enabled())
				FDERPointerList->Set_New(PVSyst);
			FDERNameList.push_back( ( (TNamedObject*) PVSyst )->Get_QualifiedName());
		}
        // Adding Storage elements
		for(stop = StorageClass->Get_ElementCount(), i = 1; i <= stop; i++)
		{
			Storage = (TStorageObj*) StorageClass->ElementList.Get(i);
			if( ( (TDSSCktElement*) Storage )->Get_Enabled())
				FDERPointerList->Set_New(Storage);
			FDERNameList.push_back( ( (TNamedObject*) Storage )->Get_QualifiedName());
		}
		FListSize = FDERPointerList->get_myNumList();
		CtrlVars.resize(FListSize + 1);


	}  /*else*/

    //Initialize arrays
	for(stop = FListSize, i = 1; i <= stop; i++)
	{
		if(StripExtension(LowerCase(String((FDERNameList)[i - 1]))) == LowerCase(myDERTypes[PVSys]))
		{
			PVSyst = ((TPVsystemObj*) PVSysClass->Find(StripClassName(String((FDERNameList)[i - 1]))));
			if(PVSyst != nullptr)
				DERelem = ((TPCElement*) PVSyst);
		}
		else
		{
			Storage = ((TStorageObj*) StorageClass->Find(StripClassName(String((FDERNameList)[i - 1]))));
			if(Storage != nullptr)
				DERelem = ((TPCElement*) Storage);
		}

		auto& withi		= CtrlVars[i];
		// Sets the constants for the PI controller
        withi.PICtrl = TPICtrl();
		withi.PICtrl.Kp = 1; // Uses deltaQ-factor as sample time for tunning the controller

		withi.FVpuSolution.resize(3);
		withi.cBuffer.resize(7);
		
		for (j = 1; j < 7; j++) withi.cBuffer[j] = CZero;
		
		withi.FRollAvgWindow	= new TRollAvgWindow;
		withi.FDRCRollAvgWindow = new TRollAvgWindow;

		Set_NTerms( ( (TDSSCktElement*) DERelem )->Get_NTerms());
		withi.CondOffset		= 0;
		withi.NPhasesDER		= DERelem->Get_NPhases();
		withi.NCondsDER			= DERelem->Get_NConds();
		withi.FAvgpVpuPrior		= 0.0;
		withi.FAvgpDRCVpuPrior	= 0.0;
		withi.FPresentVpu		= 0.0;
		withi.FPresentDRCVpu	= 0.0;
		withi.QDesiredVV		= 0.0;
		withi.QDesiredWP		= 0.0;
		withi.QDesiredWV		= 0.0;
		withi.QOld				= -1.0;
		withi.QOldVV			= -1.0;
		if (!ASSIGNED(PVSyst))	withi.QOldAVR = 0.0;
		else					withi.QOldAVR = -PVSyst->get_Fkvarlimitneg() / 2.0;
		withi.QOldDRC			= -1.0;
		withi.QOldVVDRC			= -1.0;
		withi.QDesiredDRC		= 0.0;
		withi.QDesiredVVDRC		= 0.0;
		withi.PLimitVW			= 0.0;
		withi.POldVWpu			= 0.0;
		withi.PBase				= 0.0;
		withi.QHeadRoom			= 0.0;
		withi.QHeadRoomNeg		= 0.0;
		withi.Qoutputpu			= 0.0;
		withi.QoutputVVpu		= 0.0;
		withi.QoutputAVRpu		= 0.0;
		withi.QoutputDRCpu		= 0.0;
		withi.QoutputVVDRCpu	= 0.0;
		withi.QDesireEndpu		= 0.0;
		withi.QDesireVVpu		= 0.0;
		withi.QDesireWPpu		= 0.0;
		withi.QDesireWVpu		= 0.0;
		withi.QDesireAVRpu		= 0.0;
		withi.QDesireLimitedpu	= 0.0;
		withi.QDesireOptionpu	= 0.0;
		withi.PLimitVWpu		= 0.0;
		withi.PLimitLimitedpu	= 0.0;
		withi.PLimitEndpu		= 0.0;
		withi.PLimitOptionpu	= 0.0;
		withi.QDesireDRCpu		= 0.0;

		withi.FdeltaQFactor		= DELTAQDEFAULT;
		withi.FdeltaPFactor		= DELTAPDEFAULT;
		withi.DeltaV_old		= -1.0;

		withi.deltaVDynReac		= 0.0;
		withi.FlagChangeCurve	= false;
		withi.FActiveVVCurve	= 1;
		withi.priorRollAvgWindow	= 0.0;
		withi.priorDRCRollAvgWindow	= 0.0;
		withi.FPriorWattspu		= 0.0;
		withi.FPriorwatts		= 0.0;
		withi.FPriorPLimitOptionpu	= 0.0;
		withi.FPriorQDesireOptionpu	= 0.0;
		withi.kW_out_desiredpu	= 0.0;
		withi.kW_out_desired	= 0.0;
		withi.FPriorvarspu		= 0.0;
		withi.FPriorvars		= 0.0;

		withi.FFlagVWOperates	= false;

		withi.FVVOperation		= 0.0;
		withi.FVWOperation		= 0.0;
		withi.FDRCOperation		= 0.0;
		withi.FVVDRCOperation	= 0.0;
		withi.FWPOperation		= 0.0;
		withi.FWVOperation		= 0.0;
		withi.FAVROperation		= 0.0;

		for (j = 1; j <= 2; j++)  withi.FVpuSolution[j] = 0.0;

		withi.FPendingChange	= None;
		withi.FVBase			= 0.0;
		withi.FVarFollowInverter= false;
		withi.FInverterON		= true;
		withi.FpresentkW		= 0.0;
		withi.FkVArating		= 0.0;
		withi.Fpresentkvar		= 0.0;
		withi.Fkvarlimit		= 0.0;
		withi.FkvarLimitNeg		= 0.0;
		withi.FCurrentkvarLimit	= 0.0;
		withi.FCurrentkvarLimitNeg	= 0.0;
		withi.FDCkWRated		= 0.0;
		withi.FpctDCkWRated		= 0.0;
		withi.FEffFactor		= 0.0;
		withi.FDCkW				= 0.0;
		withi.FPPriority		= false;
		withi.DQDV				= 0.0;
		withi.Fv_setpointLimited= 0.0;
		withi.FAvgpAVRVpuPrior	= 0.0;
	} /*for*/
	RecalcElementData(ActiveActor);
	if(FDERPointerList->get_myNumList() > 0)
		result = true;
	return result;
}

void TInvControlObj::Reset(int ActorID)
{

    // inherited;
}

TXYcurveObj* TInvControl::GetXYCurve(const String CurveName, int InvControlMode)
{
	TXYcurveObj* result = nullptr;
	int i = 0;
	result = ((TXYcurveObj*) XY_CurveClass->Find(CurveName));
	if(result == nullptr)
	{
		DoSimpleMsg(String("XY Curve object: \"") + CurveName
	           + "\" representing VOLTWATT or VOLTVAR curve (depending on mode) not found.", 380);
		return result;
	}


    // if VOLTWATT control mode then check for any negative watt values (pu)
    // and values greater than 1.0 per-unit (=100 percent output)
	if(InvControlMode == VOLTWATT)
	{
		int stop = 0;
		for(stop = result->get_FNumPoints(), i = 1; i <= stop; i++)
		{
			if((result->Get_YValue(i) < 0.0) || (result->Get_YValue(i) > 1.0))
			{
				DoSimpleMsg(String("XY Curve object: \"") + CurveName
	           + "\" has active power value(s) greater than 1.0 per-unit or less than -1.0 per-unit.  Not allowed for VOLTWATT control mode for PVSystem/Storages", 381);
				result = nullptr;
				break;
			}
		}
	}

    // if WATTPF control mode then check for any negative pf values
    // and values greater than 1.0
	if(InvControlMode == WATTPF)
	{
		int stop = 0;
		for(stop = result->get_FNumPoints(), i = 1; i <= stop; i++)
		{
			if((result->Get_YValue(i) <  - 1.0) || (result->Get_YValue(i) > 1.0))
			{
				DoSimpleMsg(String("XY Curve object: \"") + CurveName
	           + "\" has power factor value(s) greater than 1.0 or less than -1.0.  Not allowed for WATTPF control mode for PVSystem/Storages", 381);
				result = nullptr;
				break;
			}
		}
	}

    // if WATTVAR control mode then check for any negative pf values
    // and values greater than 1.0
	if(InvControlMode == WATTVAR)
	{
		int stop = 0;
		for(stop = result->get_FNumPoints(), i = 1; i <= stop; i++)
		{
			if((result->Get_YValue(i) <  - 1.0) || (result->Get_YValue(i) > 1.0))
			{
				DoSimpleMsg(String("XY Curve object: \"") + CurveName
	           + "\" has reactive power value(s) greater than 1.0 per-unit or less than -1.0 per-unit.  Not allowed for WATTVAR control mode for PVSystem/Storages", 381);
				result = nullptr;
				break;
			}
		}
	}
	return result;
}

int TInvControlObj::InterpretAvgVWindowLen(const String s)
{
	int result = 0;
	int Code = 0;
	Char ch = L'\0';
	String S2;
    /*Try to convert and see if we get an error*/
	Val(s, &result, Code);
	if(Code == 0)
	{
		FRollAvgWindowLengthIntervalUnit = "s"; // Only a number was specified, so must be seconds
		FVAvgWindowLengthSec = result * 1.0;
		return result;
	}

    /*Error occurred so must have a units specifier*/
	ch = s[s.size() - 1];  // get last character
	S2 = s.substr(0, s.size() - 1);
	Val(S2, &result, Code);
	if(Code > 0)   /*check for error*/
	{
		FRollAvgWindowLengthIntervalUnit = "s"; // Only a number was specified, so must be seconds
		FVAvgWindowLengthSec = 1.0;
		result = 1;
		DoSimpleMsg(String("Error in specification of Voltage Averaging Window Length: ") + s, 1134);
		return result;
	}
	switch(ch)
	{
		case 	L'h':
		{
			FRollAvgWindowLengthIntervalUnit = "h";
			FVAvgWindowLengthSec = result * 3600.0;
		}
		break;
		case 	L'm':
		{
			FRollAvgWindowLengthIntervalUnit = "m";
			FVAvgWindowLengthSec = result * 60.0;
		}
		break;
		case 	L's':
		{
			FRollAvgWindowLengthIntervalUnit = "s";
			FVAvgWindowLengthSec = result * 1.0;
		}
		break;
		default:
		FRollAvgWindowLengthIntervalUnit = "s";
		FVAvgWindowLengthSec = result * 1.0;
		result = 0; // Don't change it
		DoSimpleMsg(String("Error in specification of voltage sample interval size: \"") + s
	           + "\" Units can only be h, m, or s (single char only) ", 99934);
		break;
	}
	return result;
}

int TInvControlObj::InterpretDRCAvgVWindowLen(const String s)
{
	int result = 0;
	int Code = 0;
	Char ch = L'\0';
	String S2;
    /*Try to convert and see if we get an error*/
	Val(s, &result, Code);
	if(Code == 0)
	{
		FDRCRollAvgWindowLengthIntervalUnit = "s"; // Only a number was specified, so must be seconds
		FDRCVAvgWindowLengthSec = result * 1.0;
		return result;
	}

    /*Error occurred so must have a units specifier*/
	ch = s[s.size() - 1];  // get last character
	S2 = s.substr(0, s.size() - 1);
	Val(S2, &result, Code);
	if(Code > 0)   /*check for error*/
	{
		FDRCRollAvgWindowLengthIntervalUnit = "s"; // Only a number was specified, so must be seconds
		FDRCVAvgWindowLengthSec = 1.0;
		result = 1;
		DoSimpleMsg(String("Error in specification of Voltage Averaging Window Length: ") + s, 1134);
		return result;
	}
	switch(ch)
	{
		case 	L'h':
		{
			FDRCRollAvgWindowLengthIntervalUnit = "h";
			FDRCVAvgWindowLengthSec = result * 3600.0;
		}
		break;
		case 	L'm':
		{
			FDRCRollAvgWindowLengthIntervalUnit = "m";
			FDRCVAvgWindowLengthSec = result * 60.0;
		}
		break;
		case 	L's':
		{
			FDRCRollAvgWindowLengthIntervalUnit = "s";
			FDRCVAvgWindowLengthSec = result * 1.0;
		}
		break;
		default:
		FDRCRollAvgWindowLengthIntervalUnit = "s";
		FDRCVAvgWindowLengthSec = result * 1.0;
		result = 0; // Don't change it
		DoSimpleMsg(String("Error in specification of voltage sample interval size: \"") + s
	           + "\" Units can only be h, m, or s (single char only) ", 99934);
		break;
	}
	return result;
}

String TInvControlObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	1:
		result = ReturnElementsList();
		break;
//      2 :
//        begin
//          if ControlMode = VOLTVAR then Result := VOLTVAR;
//          if ControlMode = VOLTWATT then Result := VOLTWATT;
//          if ControlMode = DRC then Result := DRC;
//        end;
		case 	4:
		result = Fvvc_curvename;
		break;
		case 	5:
		result = Format("%-.6g", Fvvc_curveOffset);
		break;
		case 	6:
		{
			if(FVoltage_CurveX_ref == 0)
				result = "rated";
			else
			{
				if(FVoltage_CurveX_ref == 1)
					result = "avg";
				else
				{
					if(FVoltage_CurveX_ref == 2)
						result = "avgrated";
				}
			}
		}
		break;
		case 	7:
		result = Format("%d", FRollAvgWindowLength) + FRollAvgWindowLengthIntervalUnit;
		break;
		case 	8:
		result = Fvoltwatt_curvename;
		break;
		case 	9:
		result = Format("%.6g", FDbVMin);
		break;
		case 	10:
		result = Format("%.6g", FDbVMax);
		break;
		case 	11:
		result = Format("%.6g", FArGraLowV);
		break;
		case 	12:
		result = Format("%.6g", FArGraHiV);
		break;
		case 	13:
		result = Format("%d", FDRCRollAvgWindowLength ) + FDRCRollAvgWindowLengthIntervalUnit;
		break;
		case 	14:
		result = Format("%.6g", FdeltaQ_factor);
		break;
		case 	15:
		result = Format("%.6g", FVoltageChangeTolerance);
		break;
		case 	16:
		result = Format("%.6g", FVarChangeTolerance);
		break;
		case 	17:
		{
			if(FVoltwattYAxis == 0)
				result = "PAVAILABLEPU";
			if(FVoltwattYAxis == 1)
				result = "PMPPPU";
			if(FVoltwattYAxis == 2)
				result = "PCTPMPPPU";
			if(FVoltwattYAxis == 3)
				result = "KVARATINGPU";
		}
		break;
		case 	18:
		{
			if(RateofChangeMode == Inactive)
				result = "INACTIVE";
			else
			{
				if(RateofChangeMode == LPF)
					result = "LPF";
				else
				{
					if(RateofChangeMode == RISEFALL)
						result = "RISEFALL";
				}
			}
		}
		break;
		case 	21:
		result = Format("%.6g", FdeltaP_factor);
		break;
		case 	23:
		result = FReacPower_ref;
		break;
		case 	24:
		result = Format("%.6g", FActivePChangeTolerance);
		break;
		case 	28:
		result = FvoltwattCH_curvename;
		break;
        case    34:
        result = Format("%d", CtrlModel);
		break;
		default:  // take the generic handler
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

String TInvControlObj::ReturnElementsList()
{
	String result;
	int i = 0;
	int stop = 0;
	if(FListSize == 0)
	{
		result = "";
		return result;
	}
	result = String("[") + (FDERNameList)[0];
	for(stop = FListSize - 1, i = 1; i <= stop; i++)
	{
		result = result + ", " + String((FDERNameList)[i]);    // we need to pass the full name..
	}
	result = result + "]";  // terminate the array
	return result;
}

void TInvControlObj::Set_Enabled(bool Value)
{
	inherited::Set_Enabled(Value);

    /*Reset controlled PVSystem/Storages to original PF*/
}

void TInvControlObj::Set_PendingChange(int Value, int DevIndex)
{
	CtrlVars[DevIndex].FPendingChange = Value;
	DblTraceParameter = (double) Value;
}

void TInvControlObj::UpdateInvControl(int i, int ActorID)
{
	int j = 0;
	int k = 0;
	double solnvoltage = 0.0;
	std::vector <complex> tempVbuffer	= {};
	TPVsystemObj* PVSyst				= nullptr;
	TStorageObj* Storage				= nullptr;
	double BaseKV = 0.0;
	int stop = 0;
	for(stop = FDERPointerList->get_myNumList(), j = 1; j <= stop; j++)
	{
		auto& withj = CtrlVars[j];
          // only update solution idx one time through this routine
		int stop1 = 0;
		if((j == 1) && (i == 1))
              //update solution voltage in per-unit for hysteresis
		{
			if(FVpuSolutionIdx == 2)
				FVpuSolutionIdx = 1;
			else
				FVpuSolutionIdx = FVpuSolutionIdx + 1;
		}
		if( ( (TNamedObject*) (withj.ControlledElement) )->Get_myPName() == myDERTypes[PVSys])
			PVSyst = (TPVsystemObj*) (withj.ControlledElement);
		else
			Storage = (TStorageObj*) (withj.ControlledElement);
		BaseKV = withj.FVBase / 1000.0;

          //             FPriorvars[j]  := PVSys.Presentkvar;
          //             FPriorWatts[j]  := PVSys.PresentkW;
		withj.FPriorPLimitOptionpu	= withj.PLimitOptionpu;
		withj.FPriorQDesireOptionpu = withj.QDesireOptionpu;

          // Used to update the VW resquested kW
		if(((TNamedObject*)(withj.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
			PVSyst->Set_VWmode(false);
		else
			Storage->Set_VWmode(false);
		if(((TNamedObject*)(withj.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
			PVSyst->Set_VVmode(false);
		else
			Storage->Set_VVmode(false);
		if(((TNamedObject*)(withj.ControlledElement))->Get_myPName() == myDERTypes[PVSys])
			PVSyst->Set_DRCmode(false);
		else
			Storage->Set_DRCmode(false);
		withj.FFlagVWOperates = false;

          // Reset DQDV - We might not need it
		withj.DQDV = 0.0;

          // Reset the operation flags for the new time step
		withj.FVVOperation		= 0;
		withj.FVWOperation		= 0;
		withj.FDRCOperation		= 0;
		withj.FVVDRCOperation	= 0;
		withj.FWPOperation		= 0;
		withj.FWVOperation		= 0;
		withj.FAVROperation		= 0;

          // Reinitialize convergence arrays.
          //FdeltaQFactor[j] := DELTAQDEFAULT;
		withj.FdeltaPFactor = DELTAPDEFAULT;

          // allocated enough memory to buffer to hold voltages and initialize to cZERO
		tempVbuffer.resize( ( withj.ControlledElement)->Get_NConds() * sizeof(complex) );
		for(stop1 = ( (TDSSCktElement*) (withj.ControlledElement) )->Get_NConds(), k = 1; k <= stop1; k++)
			tempVbuffer[k - 1] = CZero;

		withj.priorRollAvgWindow	= withj.FRollAvgWindow->Get_AvgVal();
		withj.priorDRCRollAvgWindow = withj.FDRCRollAvgWindow->Get_AvgVal();

          // compute the present terminal voltage
		( (TDSSCktElement*) withj.ControlledElement )->ComputeVterminal(ActorID);
          //PVSys.Set_Variable(5,FDRCRollAvgWindow[j].Get_AvgVal); // save rolling average voltage in monitor
		solnvoltage = 0.0;

		if (ASSIGNED(PVSyst))
			GetmonVoltage(ActorID, solnvoltage, j, BaseKV, PVSyst->Connection);
		else
			GetmonVoltage(ActorID, solnvoltage, j, BaseKV, Storage->Connection);

          //for k := 1 to localControlledElement.Yorder do tempVbuffer[k] := localControlledElement.Vterminal^[k];


          //for k := 1 to localControlledElement.Nphases do solnvoltage := solnvoltage + Cabs(tempVbuffer[k]);
          //solnvoltage := solnvoltage / (localControlledElement.Nphases*1.0); // average of voltages if more than one phase

          // add present power flow solution voltage to the rolling average window
		withj.FRollAvgWindow->Add(solnvoltage, ActiveCircuit[ActorID]->Solution->DynaVars.h, FVAvgWindowLengthSec);
		withj.FDRCRollAvgWindow->Add(solnvoltage, ActiveCircuit[ActorID]->Solution->DynaVars.h, FDRCVAvgWindowLengthSec);
		withj.FVpuSolution[FVpuSolutionIdx] = solnvoltage / ((ActiveCircuit[ActorID]->Buses[withj.ControlledElement->Terminals[0].BusRef - 1]->kVBase) * 1000.0);
		tempVbuffer.resize(0);   // Clean up memory
	}
}

//-------------------------------------------------------------------------------------------------

int TInvControlObj::get_ControlMode()
{
	return ControlMode;
}

//-------------------------------------------------------------------------------------------------


int TInvControlObj::get_CombiControlMode()
{
	return CombiControlMode;
}

//-------------------------------------------------------------------------------------------------

int TInvControlObj::get_FVoltage_CurveX_ref()
{
	return FVoltage_CurveX_ref;
}

//-------------------------------------------------------------------------------------------------

int TInvControlObj::get_FRollAvgWindowLength()
{
	return FRollAvgWindowLength;
}

//-------------------------------------------------------------------------------------------------

int TInvControlObj::get_FDRCRollAvgWindowLength()
{
	return FDRCRollAvgWindowLength;
}

//-------------------------------------------------------------------------------------------------

int TInvControlObj::get_FMonBusesPhase()
{
	return FMonBusesPhase;
}

//-------------------------------------------------------------------------------------------------

int TInvControlObj::get_FVoltwattYAxis()
{
	return FVoltwattYAxis;
}

//-------------------------------------------------------------------------------------------------


TStringList* TInvControlObj::get_FDERNameList()
{
	return &FDERNameList;
}

//-------------------------------------------------------------------------------------------------

TStringList* TInvControlObj::get_FMonBusesNameList()
{
	return &FMonBusesNameList;
}

//-------------------------------------------------------------------------------------------------


String TInvControlObj::get_Fvvc_curvename()
{
	return Fvvc_curvename;
}

//-------------------------------------------------------------------------------------------------

String TInvControlObj::get_Fvoltwatt_curvename()
{
	return Fvoltwatt_curvename;
}

//-------------------------------------------------------------------------------------------------

String TInvControlObj::get_FvoltwattCH_curvename()
{
	return FvoltwattCH_curvename;
}

//-------------------------------------------------------------------------------------------------

String TInvControlObj::get_FReacPower_ref()
{
	return FReacPower_ref;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_Fvvc_curveOffset()
{
	return Fvvc_curveOffset;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FDbVMin()
{
	return FDbVMin;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FDbVMax()
{
	return FDbVMax;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FArGraLowV()
{
	return FArGraLowV;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FArGraHiV()
{
	return FArGraHiV;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FdeltaQ_factor()
{
	return FdeltaQ_factor;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FVoltageChangeTolerance()
{
	return FVoltageChangeTolerance;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FVarChangeTolerance()
{
	return FVarChangeTolerance;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FLPFTau()
{
	return FLPFTau;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FRiseFallLimit()
{
	return FRiseFallLimit;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FdeltaP_factor()
{
	return FdeltaP_factor;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_FActivePChangeTolerance()
{
	return FActivePChangeTolerance;
}

//-------------------------------------------------------------------------------------------------

double TInvControlObj::get_Fv_setpoint()
{
	return Fv_setpoint;
}

//-------------------------------------------------------------------------------------------------

pDoubleArray TInvControlObj::get_FMonBusesVbase()
{
	return &(FMonBusesVbase[0]);
}

//-------------------------------------------------------------------------------------------------


int TInvControlObj::Get_PendingChange(int DevIndex)
{
	int result	= 0;
	result		= CtrlVars[DevIndex].FPendingChange;
	return result;
}

void TInvControlObj::CalcVoltWatt_watts(int j, int ActorID)
{
	double DeltaPpu = 0.0;
	auto& withj		= CtrlVars[j];
  // PLimitEndpu[j] <= abs(kW_out_desiredpu[j] will always be true when we are in 'resquest' region of VW
  // That's what we want. In this region, VW will work similarly to VV. So we need to move slowly towards the VW curve point.
	if(((withj.PLimitEndpu < 1.0) && (withj.PLimitEndpu <= Abs(withj.kW_out_desiredpu))) || (withj.FFlagVWOperates))
	{
		if(ActiveCircuit[ActorID]->Solution->ControlIteration == 1)
			withj.POldVWpu		= Abs(withj.kW_out_desiredpu); // take abs(kW_out_desiredpu) because might be in charging mode.
		withj.FFlagVWOperates = true;

        // PLimitEndpu might be negative here in 'requesting' region. Do we need to give POldVW a sign in this case?
        // Yes, it will naturally evolve to a negative value with the process. It will always positive only in the 1st control iteration.
		DeltaPpu = withj.PLimitEndpu - withj.POldVWpu;
		if(FdeltaP_factor == FLAGDELTAP)
			Change_deltaP_factor(ActorID, j);
		else
			withj.FdeltaPFactor = FdeltaP_factor;
		withj.PLimitVW = (withj.POldVWpu + DeltaPpu * withj.FdeltaPFactor) * withj.PBase;
	}
	else
	{
		withj.PLimitVW = withj.PLimitEndpu * withj.PBase;
	}
}

void TInvControlObj::Check_Plimits(int j, double P, int ActorID)
{
	double P_Ppriority			= 0.0;
	double pctDCkWRatedlimit	= 0.0;
	auto& withj					= CtrlVars[j];

	withj.PLimitLimitedpu	= 1.0; // Not limited

    // volt-watt states
	if(P < 1.0)
		withj.FVWOperation = 1.0;
	pctDCkWRatedlimit = withj.FpctDCkWRated * withj.FDCkWRated;

    // PLimitEndpu should be less than the P avaliable under var priority   (works for VV_VW)
	if(withj.FPPriority == false)
	{
		P_Ppriority = sqrt(Sqr(withj.FkVArating) - Sqr(withj.Fpresentkvar));
		if(P_Ppriority < (Abs( P) * withj.PBase))   // P might be negative in requesting region for storage
		{
			withj.PLimitLimitedpu	= P_Ppriority / withj.PBase * Sign(P);
			withj.FVWOperation		= 0.0; // kVA exceeded under watt priority
		}
	}

    // PLimitEndpu should be less than pctPmpp
	if((Abs( P) * withj.PBase) > pctDCkWRatedlimit)
	{
		withj.FVWOperation		= 0.0; // pctPmpp exceeded under watt priority
		withj.PLimitLimitedpu	= pctDCkWRatedlimit / withj.PBase * Sign(P);
	}
}

void TInvControlObj::CalcVoltVar_vars(int j, int ActorID)
{
	double DeltaQ	= 0.0;
	auto& withj		= CtrlVars[j];
	if(withj.FlagChangeCurve == false)
	{
		if(withj.QDesireEndpu >= 0.0)
			DeltaQ = withj.QDesireEndpu * withj.QHeadRoom;
		else
			DeltaQ = withj.QDesireEndpu * withj.QHeadRoomNeg;
		if (CtrlModel == 0)
		{
			DeltaQ = DeltaQ - withj.QOldVV;
			if (FdeltaQ_factor == FLAGDELTAQ)
				Change_deltaQ_factor(ActorID, j);
			else
				withj.FdeltaQFactor = FdeltaQ_factor;
			withj.QDesiredVV = withj.QOldVV + DeltaQ * withj.FdeltaQFactor;
		}
		else
		{
			withj.PICtrl.kDen = exp(-1 * abs(FdeltaQ_factor));
			withj.PICtrl.kNum = 1 - withj.PICtrl.kDen;
			withj.QDesiredVV = withj.PICtrl.SolvePI(DeltaQ);
		}
    // else, stay at present var output level
	}
	else
	{
		withj.QDesiredVV = withj.Fpresentkvar;
	}
}

void TInvControlObj::CalcAVR_vars(int j, int ActorID)
{
	double DeltaQ	= 0.0;
	auto& withj		= CtrlVars[j];

	if(withj.QDesireEndpu >= 0.0)
		DeltaQ = withj.QDesireEndpu * withj.QHeadRoom;
	else
		DeltaQ = withj.QDesireEndpu * withj.QHeadRoomNeg;
	if (CtrlModel == 0)
	{
		DeltaQ = DeltaQ - withj.QOldAVR;
		if (FdeltaQ_factor == FLAGDELTAQ)
			Change_deltaQ_factor(ActorID, j);
		else
			withj.FdeltaQFactor = FdeltaQ_factor;
		withj.QDesiredAVR = withj.QOldAVR + 0.2 * DeltaQ;
	}
	else
	{
		withj.PICtrl.kDen = exp(-1 * abs(FdeltaQ_factor));
		withj.PICtrl.kNum = 1 - withj.PICtrl.kDen;
		withj.QDesiredAVR = withj.PICtrl.SolvePI(DeltaQ);
	}
}

void TInvControlObj::CalcWATTPF_vars(int j, int ActorID)
{
	auto& withj = CtrlVars[j];
	if(withj.QDesireEndpu >= 0.0)
		withj.QDesiredWP = withj.QDesireEndpu * withj.QHeadRoom;
	else
		withj.QDesiredWP = withj.QDesireEndpu * withj.QHeadRoomNeg;
}

void TInvControlObj::CalcWATTVAR_vars(int j, int ActorID)
{
	auto& withj = CtrlVars[j];
	if(withj.QDesireEndpu >= 0.0)
		withj.QDesiredWV = withj.QDesireEndpu * withj.QHeadRoom;
	else
		withj.QDesiredWV = withj.QDesireEndpu * withj.QHeadRoomNeg;
}

void TInvControlObj::CalcDRC_vars(int j, int ActorID)
{
	double DeltaQ	= 0.0;
	auto& withj		= CtrlVars[j];

	if(withj.QDesireEndpu >= 0.0)
		DeltaQ = withj.QDesireEndpu * withj.QHeadRoom;
	else
		DeltaQ = withj.QDesireEndpu * withj.QHeadRoomNeg;
	if (CtrlModel == 0)
	{
		DeltaQ = DeltaQ - withj.QOldDRC;
		if (FdeltaQ_factor == FLAGDELTAQ)
			Change_deltaQ_factor(ActorID, j);
		else
			withj.FdeltaQFactor = FdeltaQ_factor;
		withj.QDesiredDRC = withj.QOldDRC + DeltaQ * withj.FdeltaQFactor;
	}
	else
	{
		withj.PICtrl.kDen = exp(-1 * abs(FdeltaQ_factor));
		withj.PICtrl.kNum = 1 - withj.PICtrl.kDen;
		withj.QDesiredDRC = withj.PICtrl.SolvePI(DeltaQ);
	}
}

void TInvControlObj::CalcVVDRC_vars(int j, int ActorID)
{
	double DeltaQ	= 0.0;
	auto& withj		= CtrlVars[j];

	if(withj.QDesireEndpu >= 0.0)
		DeltaQ = withj.QDesireEndpu * withj.QHeadRoom;
	else
		DeltaQ = withj.QDesireEndpu * withj.QHeadRoomNeg;

	if (CtrlModel == 0)
	{
		DeltaQ = DeltaQ - withj.QOldVVDRC;
		if (FdeltaQ_factor == FLAGDELTAQ)
			Change_deltaQ_factor(ActorID, j);
		else
			withj.FdeltaQFactor = FdeltaQ_factor;
		withj.QDesiredVVDRC = withj.QOldVVDRC + DeltaQ * withj.FdeltaQFactor;
	}
	else
	{
		withj.PICtrl.kDen = exp(-1 * abs(FdeltaQ_factor));
		withj.PICtrl.kNum = 1 - withj.PICtrl.kDen;
		withj.QDesiredVVDRC = withj.PICtrl.SolvePI(DeltaQ);
	}
}

void TInvControlObj::Calc_PBase(int j, int ActorID)
{
	TPCElement* DERelem = nullptr;
	auto& withj			= CtrlVars[j];

	DERelem = (withj.ControlledElement);
	if( ( (TNamedObject*) DERelem )->Get_myPName() == myDERTypes[PVSys])
	{
		if(FVoltwattYAxis == 0)
			withj.PBase = withj.FDCkW * withj.FEffFactor;
		else
		{
			if(FVoltwattYAxis == 1)
				withj.PBase = withj.FDCkWRated;
			else
			{
				if(FVoltwattYAxis == 2)
					withj.PBase = withj.FDCkWRated * withj.FpctDCkWRated;
				else
				{
					if(FVoltwattYAxis == 3)
						withj.PBase = withj.FkVArating;
				}
			}
		}
	}
	else
	{
		if(FVoltwattYAxis == 0)
			withj.PBase = ((TStorageObj*) DERelem)->Get_DCkW() * withj.FEffFactor;
		else
		{
			if(FVoltwattYAxis == 1)
				withj.PBase = withj.FDCkWRated;
			else
			{
				if(FVoltwattYAxis == 2)
					withj.PBase = withj.FDCkWRated * withj.FpctDCkWRated;
				else
				{
					if(FVoltwattYAxis == 3)
						withj.PBase = withj.FkVArating;
				}
			}
		}
	}
}

void TInvControlObj::CalcLPF(int m, String powertype, double LPF_desiredpu, int ActorID)
{
	double Alpha	= 0.0;
	auto& withm		= CtrlVars[m];

    // Applies the LPF:
    //  Return value is in kvar for VARS
    //  Return value is in puPmpp for WATTS

    // Qoutput(t) = Qdesired(t) x {1- exp[-(t-t0)/tau]} + Qoutput(t-t0) x exp[-(t-t0)/tau]
    // calculate the alpha constant: alpha = exp[-(t-t0)/tau]
	Alpha = exp(-1.0 * ActiveCircuit[ActorID]->Solution->DynaVars.h / FLPFTau);
	if(powertype == "VARS")
		withm.QDesireOptionpu = LPF_desiredpu * (1 - Alpha) + withm.FPriorQDesireOptionpu * Alpha;
	if(powertype == "WATTS")
		withm.PLimitOptionpu = LPF_desiredpu * (1 - Alpha) + withm.FPriorPLimitOptionpu * Alpha;
}

void TInvControlObj::CalcRF(int m, String powertype, double RF_desiredpu, int ActorID)
{
	auto& withm = CtrlVars[m];
    // Applies the Rise/Fall limiting function:
	if(powertype == "VARS")
        // rate of change rise/fall limit
	{
		if((RF_desiredpu - withm.FPriorQDesireOptionpu) > (FRiseFallLimit * ActiveCircuit[ActorID]->Solution->DynaVars.h))
			withm.QDesireOptionpu = withm.FPriorQDesireOptionpu + FRiseFallLimit * ActiveCircuit[ActorID]->Solution->DynaVars.h;
		else
		{
			if((RF_desiredpu - withm.FPriorQDesireOptionpu) < (-1 * FRiseFallLimit * ActiveCircuit[ActorID]->Solution->DynaVars.h))
				withm.QDesireOptionpu = withm.FPriorQDesireOptionpu - FRiseFallLimit * ActiveCircuit[ActorID]->Solution->DynaVars.h;
			else
				withm.QDesireOptionpu = RF_desiredpu;
		}
	}
	if(powertype == "WATTS")
        // rate of change rise/fall limit
	{
		if((RF_desiredpu - withm.FPriorPLimitOptionpu) > (FRiseFallLimit * ActiveCircuit[ActorID]->Solution->DynaVars.h))
			withm.PLimitOptionpu = withm.FPriorPLimitOptionpu + (FRiseFallLimit * ActiveCircuit[ActorID]->Solution->DynaVars.h);
		else
		{
			if((RF_desiredpu - withm.FPriorPLimitOptionpu) < (-1 * FRiseFallLimit * ActiveCircuit[ActorID]->Solution->DynaVars.h))
				withm.PLimitOptionpu = withm.FPriorPLimitOptionpu - (FRiseFallLimit * ActiveCircuit[ActorID]->Solution->DynaVars.h);
			else
				withm.PLimitOptionpu = RF_desiredpu;
		}
	}
}

void TInvControlObj::CalcPVWcurve_limitpu(int j, int ActorID)
{
	auto& withj = CtrlVars[j];

	if( ( (TNamedObject*) (withj.ControlledElement) )->Get_myPName() == myDERTypes[PVSys])
		withj.PLimitVWpu = Fvoltwatt_curve->GetYValue_(withj.FPresentVpu);
	else
	{
		if(((TStorageObj*) (withj.ControlledElement))->get_fState() == STORE_DISCHARGING)
		{
			if(((TStorageObj*) (withj.ControlledElement))->FVWStateRequested)
				withj.PLimitVWpu = FvoltwattCH_curve->GetYValue_(withj.FPresentVpu);
			else
				withj.PLimitVWpu = Fvoltwatt_curve->GetYValue_(withj.FPresentVpu);
		}
		else
		{
			if((((TStorageObj*) (withj.ControlledElement))->get_fState() == STORE_CHARGING) && (FvoltwattCH_curve != nullptr))
			{
				if(((TStorageObj*) (withj.ControlledElement))->FVWStateRequested)
					withj.PLimitVWpu = Fvoltwatt_curve->GetYValue_(withj.FPresentVpu);
				else
 // try with positive PlimitVWpu
					withj.PLimitVWpu = FvoltwattCH_curve->GetYValue_(withj.FPresentVpu);
			}
			else
				withj.PLimitVWpu = 1.0; // don't limit if in idling state
		}
	}
}

void TInvControlObj::CalcQVVcurve_desiredpu(int j, int ActorID)
{
	double voltagechangesolution	= 0.0;
	double QPresentpu				= 0.0;
	double VpuFromCurve				= 0.0;
	auto& withj						= CtrlVars[j];

	withj.QDesireVVpu = 0.0;
	if(withj.Fpresentkvar >= 0.0)
		QPresentpu = withj.Fpresentkvar / withj.QHeadRoom;
	else
		QPresentpu = withj.Fpresentkvar / withj.QHeadRoomNeg;
	voltagechangesolution = 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
	if((ActiveCircuit[ActorID]->Solution->DynaVars.dblHour * 3600.0 / ActiveCircuit[ActorID]->Solution->DynaVars.h) < 3.0)
		voltagechangesolution = 0.0;
	else
	{
		if(FVpuSolutionIdx == 1)
			voltagechangesolution = withj.FVpuSolution[1] - withj.FVpuSolution[2];
		else
		{
			if(FVpuSolutionIdx == 2)
				voltagechangesolution = withj.FVpuSolution[2] - withj.FVpuSolution[1];
		}

    // if no hysteresis (Fvvc_curveOffset == 0), then just look up the value
    // from the volt-var curve
	}
	if(Fvvc_curveOffset == 0.0)  // no hysteresis
	{
		withj.QDesireVVpu = Fvvc_curve->GetYValue_(withj.FPresentVpu); // end of logic for the no-hysteresis case

    // else if we're going in the positive direction and on curve 1, stay
    // with curve 1
	}
	else
	{
		if((voltagechangesolution > 0) && (withj.FActiveVVCurve == 1))
		{
			if(withj.FlagChangeCurve == true)
			{
				VpuFromCurve = Fvvc_curve->GetXValue(QPresentpu);
				if(Abs( (withj.FPresentVpu - VpuFromCurve)) < FVoltageChangeTolerance / 2.0)
				{
					withj.QDesireVVpu = Fvvc_curve->GetYValue_(withj.FPresentVpu);      //Y value = in per-unit of headroom
					withj.FlagChangeCurve = false;
				}
				else
				{
					withj.QDesireVVpu = QPresentpu;            // (PR) look at here
					withj.FlagChangeCurve = false;
				}
			}
			else
			{
				withj.QDesireVVpu = Fvvc_curve->GetYValue_(withj.FPresentVpu);      //Y value = in per-unit of headroom
			}

    // with hysteresis if we're going in the positive direction on voltages
    // from last two power flow solutions, and we're using curve 2, keep vars
    // the same, and change to curve1 active
		}
		else
		{
			if((voltagechangesolution > 0) && (withj.FActiveVVCurve == 2))
			{
				withj.QDesireVVpu		= QPresentpu;
				withj.FActiveVVCurve	= 1;
				withj.FlagChangeCurve	= true;

    // with hysteresis if we're going in the negative direction on voltages
    // from last two power flow solutions, and we're using curve 2, either
    // lookup the vars for the voltage we're at (with offset on curve1),
    // or if we've not just changed curves, stay at the current p.u.
    // var output
			}
			else
			{
				if((voltagechangesolution < 0) && (withj.FActiveVVCurve == 2))
				{
					if(withj.FlagChangeCurve == true)
					{
						VpuFromCurve = Fvvc_curve->GetXValue(QPresentpu);
						VpuFromCurve = VpuFromCurve - Fvvc_curveOffset;
						if(Abs( (withj.FPresentVpu - VpuFromCurve)) < FVoltageChangeTolerance / 2.0)
						{
							withj.QDesireVVpu = Fvvc_curve->GetYValue_(withj.FPresentVpu - Fvvc_curveOffset);      //Y value = in per-unit of headroom
							withj.FlagChangeCurve = false;
						}
						else
						{
							withj.QDesireVVpu = QPresentpu;
							withj.FlagChangeCurve = false;
						}
					}
					else
					{
						withj.QDesireVVpu = Fvvc_curve->GetYValue_(withj.FPresentVpu - Fvvc_curveOffset);      //Y value = in per-unit of headroom
					}

    // with hysteresis if we're going in the negative direction on voltages
    // from last two power flow solutions, and we're using curve 1, then
    // stay wjth present output vars and make curve2 active, set curve change
    // flag
				}
				else
				{
					if((voltagechangesolution < 0) && (withj.FActiveVVCurve == 1))
					{
						withj.QDesireVVpu		= QPresentpu;
						withj.FActiveVVCurve	= 2;
						withj.FlagChangeCurve	= true;


    // if no change in voltage from one powerflow to the next, then
    // do one of the following
					}
					else
					{
						if((voltagechangesolution == 0) && (withj.FActiveVVCurve == 1) && (withj.FlagChangeCurve == false))
						{
							withj.QDesireVVpu = Fvvc_curve->GetYValue_(withj.FPresentVpu);
						}
						else
						{
							if((voltagechangesolution == 0) && (withj.FlagChangeCurve == true))
							{
								withj.QDesireVVpu = QPresentpu;
							}
							else
							{
								if((voltagechangesolution == 0) && (withj.FActiveVVCurve == 2) && (withj.FlagChangeCurve == false))
								{
									withj.QDesireVVpu = Fvvc_curve->GetYValue_(withj.FPresentVpu - Fvvc_curveOffset);
								}
							}
						}
					}
				}
			}
		}
	}
}

void TInvControlObj::CalcQWVcurve_desiredpu(int j, int ActorID)
{
	double voltagechangesolution	= 0.0;
	double PBase					= 0.0;
	auto& withj						= CtrlVars[j];

	withj.QDesireWVpu = 0.0;
	voltagechangesolution = 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
	if((ActiveCircuit[ActorID]->Solution->DynaVars.dblHour * 3600.0 / ActiveCircuit[ActorID]->Solution->DynaVars.h) < 3.0)
		voltagechangesolution = 0.0;
	else
	{
		if(FVpuSolutionIdx == 1)
			voltagechangesolution = withj.FVpuSolution[1] - withj.FVpuSolution[2];
		else
		{
			if(FVpuSolutionIdx == 2)
				voltagechangesolution = withj.FVpuSolution[2] - withj.FVpuSolution[1];
		}
	}
	PBase = min(withj.FkVArating, withj.FDCkWRated); // Should include DC-to-AC and kW-to-KVA ratios to avoid to quick fix like this
	withj.QDesireWVpu = Fwattvar_curve->GetYValue_(withj.FDCkW * withj.FEffFactor * withj.FpctDCkWRated / PBase);
}

void TInvControlObj::CalcQAVR_desiredpu(int j, int ActorID)
{
	double voltagechangesolution	= 0.0;
	double DQ						= 0.0;
	double QPresentpu				= 0.0;
	double DQmax					= 0.0;
	double DeltaV					= 0.0;
	double V						= 0.0;
	auto& withj						= CtrlVars[j];

	DQmax				= 0.1 * withj.Fkvarlimit / withj.QHeadRoomNeg;
	withj.QDesireAVRpu	= 0.0;

//    if (((Fv_setpoint - FAvgpVpuPrior[j]) > 0) and (ActiveCircuit[ActorID].Solution.ControlIteration = 3)) then
//      Fpresentkvar[j] := 0 //abs(Fpresentkvar[j])
//    else
//      Fpresentkvar[j] := 0; //-1 * abs(Fpresentkvar[j]);

//    if (ActiveCircuit[ActorID].Solution.ControlIteration = 3) then
//    begin
//      Fpresentkvar[j] := 0; //abs(Fpresentkvar[j])
//      if (((Fv_setpoint - FAvgpAVRVpuPrior[j]) > 0)) then
//        Fpresentkvar[j] := abs(Fpresentkvar[j])
//      else
//        Fpresentkvar[j] := -1 * abs(Fpresentkvar[j]);
//
//    end;
	if(withj.Fpresentkvar >= 0.0)
		QPresentpu = withj.Fpresentkvar / withj.QHeadRoom;
	else
		QPresentpu = withj.Fpresentkvar / withj.QHeadRoomNeg;
	if(ActiveCircuit[ActorID]->Solution->ControlIteration == 3)
	{
		V = withj.FAvgpAVRVpuPrior;
		QPresentpu		= 0.0;
		withj.QOldAVR	= 0.0;
	}
	else
	V = withj.FPresentVpu;
	voltagechangesolution = 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
	if((ActiveCircuit[ActorID]->Solution->DynaVars.dblHour * 3600.0 / ActiveCircuit[ActorID]->Solution->DynaVars.h) < 3.0)
		voltagechangesolution = 0.0;
	else
	{
		if(FVpuSolutionIdx == 1)
			voltagechangesolution = withj.FVpuSolution[1] - withj.FVpuSolution[2];
		else
		{
			if(FVpuSolutionIdx == 2)
				voltagechangesolution = withj.FVpuSolution[2] - withj.FVpuSolution[1];
		}

//    if (abs(FPresentVpu[j] - FAvgpVpuPrior[j]) = FVoltageChangeTolerance) then  DQ := 0.0
//    else if Fv_setpoint <= FPresentVpu[j] then DQ := - abs((Fv_setpoint - FPresentVpu[j] / QHeadRoom[j]) * (QPresentpu - Fkvarlimitneg[j]) / (FPresentVpu[j] - FAvgpVpuPrior[j]))
//    else DQ := abs((Fv_setpoint - FPresentVpu[j]) * (QPresentpu - Fkvarlimit[j] / QHeadRoomNeg[j]) / (FPresentVpu[j] - FAvgpVpuPrior[j]));
//
//    If (DQ > Fkvarlimit[j] / QHeadRoom[j]) then DQ := Fkvarlimit[j] / QHeadRoom[j]
//    else if (DQ > Fkvarlimitneg[j] / QHeadRoomNeg[j]) then DQ := - Fkvarlimitneg[j] / QHeadRoomNeg[j];
//
//    QDesireAVRpu[j] := QPresentpu + DQ;
	}
	DeltaV = Abs( (Fv_setpoint - withj.FAvgpVpuPrior));
	if((Abs( DeltaV) < 0.005) && (withj.FdeltaQFactor > 0.2))
		withj.FdeltaQFactor = withj.FdeltaQFactor + 0.1;
	else
	{
		if((Abs( DeltaV) < 0.02) && (withj.FdeltaQFactor > 0.2))
			withj.FdeltaQFactor = withj.FdeltaQFactor + 0.05;
		else
		{
			if((Abs( DeltaV) > 0.02) && (withj.FdeltaQFactor < 0.9))
				withj.FdeltaQFactor = withj.FdeltaQFactor - 0.05;
			else
			{
				if((Abs( DeltaV) < 0.05) && (withj.FdeltaQFactor < 0.9))
					withj.FdeltaQFactor = withj.FdeltaQFactor - 0.1;
			}
		}
	}
	withj.FdeltaQFactor = 0.2;
	withj.DeltaV_old = Abs( (withj.FPresentVpu - withj.FAvgpVpuPrior));
	if(withj.FPresentVpu - withj.FAvgpVpuPrior == 0)
		DQ = 0;
	else
		DQ = withj.FdeltaQFactor * withj.DQDV * (Fv_setpoint - V);
	if(Abs( DQ) > DQmax)
	{
		if(DQ < 0.0)
			DQ = -DQmax;
		else
			DQ = DQmax;
	}
	withj.QDesireAVRpu = QPresentpu + DQ;
}

void TInvControlObj::CalcQWPcurve_desiredpu(int j, int ActorID)
{
	double voltagechangesolution	= 0.0;
	double P						= 0.0;
	bool PF_Priority				= false;
	double QDesiredWP				= 0.0;
	auto& withj						= CtrlVars[j];
    // Pbase                                    :Double;
	withj.QDesireWPpu		= 0.0;
	voltagechangesolution	= 0.0;

    // for first two seconds, keep voltagechangesolution equal to zero
    // we don't have solutions from the time-series power flow, yet
	if((ActiveCircuit[ActorID]->Solution->DynaVars.dblHour * 3600.0 / ActiveCircuit[ActorID]->Solution->DynaVars.h) < 3.0)
		voltagechangesolution = 0.0;
	else
	{
		if(FVpuSolutionIdx == 1)
			voltagechangesolution = withj.FVpuSolution[1] - withj.FVpuSolution[2];
		else
		{
			if(FVpuSolutionIdx == 2)
				voltagechangesolution = withj.FVpuSolution[2] - withj.FVpuSolution[1];
		}

    // Pbase = min(FpctDCkWRated[j] / FDCkWRated[j], FkVARating[j])
	}
	pf_wp_nominal = Fwattpf_curve->GetYValue_(withj.FDCkW * withj.FEffFactor * withj.FpctDCkWRated / withj.FDCkWRated);
	if( ( (TNamedObject*) (withj.ControlledElement) )->Get_myPName() == myDERTypes[PVSys])
		PF_Priority = ((TPVsystemObj*) (withj.ControlledElement))->PVSystemVars.PF_Priority;
	else
	{
		if( ( (TNamedObject*) (withj.ControlledElement) )->Get_myPName() == myDERTypes[EStorage])
			PF_Priority = ((TStorageObj*) (withj.ControlledElement))->StorageVars.PF_Priority;
	}
	if((withj.FPPriority == false) && (PF_Priority == false))
		P = withj.FDCkW * withj.FEffFactor * withj.FpctDCkWRated;
	else
		P = withj.kW_out_desired;
	QDesiredWP = P * sqrt(double(1) / (pf_wp_nominal * pf_wp_nominal) - 1) * Sign(pf_wp_nominal);
	if(QDesiredWP >= 0.0)
		withj.QDesireWPpu = QDesiredWP / withj.QHeadRoom;
	else
		withj.QDesireWPpu = QDesiredWP / withj.QHeadRoomNeg;
}

void TInvControlObj::CalcQDRC_desiredpu(int j, int ActorID)
{
	double BaseKV	= 0.0;
	auto& withj		= CtrlVars[j];

	withj.QDesireDRCpu	= 0.0;
	BaseKV				= withj.FVBase / 1000.0; // It's a line-to-ground voltage

    // calculate deltaV quantity in per-unit from subtracting the rolling average
    // value (in p.u.) from the present p.u. terminal voltage (average of line-ground)
    // if more than one phase
	if((withj.FDRCRollAvgWindow->Get_AvgVal() / (BaseKV * 1000.0)) == 0.0L)
		withj.deltaVDynReac = 0;
	else
		withj.deltaVDynReac = withj.FPresentDRCVpu - withj.FDRCRollAvgWindow->Get_AvgVal() / (BaseKV * 1000.0);

    // if below the lower deadband and deltaV quantity is non-zero then
    // calculate desired pu var output. In per-unit of kva rating (also
    // ampere rating), per report specifications.
	if((withj.deltaVDynReac != 0) && (withj.FPresentDRCVpu < FDbVMin))

    // if above the upper deadband and deltaV quantity is non-zero then
    // calculate desired pu var output. In per-unit of kva rating (also
    // ampere rating), per report specifications.
		withj.QDesireDRCpu = -withj.deltaVDynReac * FArGraLowV;
	else
	{
		if((withj.deltaVDynReac != 0) && (withj.FPresentDRCVpu > FDbVMax))
			withj.QDesireDRCpu = -withj.deltaVDynReac * FArGraHiV;
		else
		{
			if(withj.deltaVDynReac == 0.0)
				withj.QDesireDRCpu = 0.0;
		}
	}
	if(ActiveCircuit[ActorID]->Solution->DynaVars.T == 1)
		withj.QDesireDRCpu = 0.0;
}

void TInvControlObj::Check_Qlimits_WV(int j, double Q, int ActorID)
{
	double Q_Ppriority				= 0.0;
	double currentkvarlimitpu		= 0.0;
	double currentkvarlimitnegpu	= 0.0;
	double FOperation				= 0.0;
	double Error					= 0.0;
	auto& withj						= CtrlVars[j];

    // Will organize this part into functions later

    // states
	Error = 0;
	if(ControlMode == WATTVAR)
		Error = 0.005;
	if(Q <  - Error)
		FOperation = -1.0;
	else
	{
		if(Q > Error)
			FOperation = 1.0;
		else
			FOperation = 0.0;
	}
	withj.QDesireLimitedpu	= 1.0; // Not limited
	currentkvarlimitpu		= withj.FCurrentkvarLimit / withj.QHeadRoom;
	currentkvarlimitnegpu	= withj.FCurrentkvarLimitNeg / withj.QHeadRoomNeg;
	if(currentkvarlimitpu > withj.QDesireLimitedpu)
		currentkvarlimitpu		= withj.QDesireLimitedpu;
	if(currentkvarlimitnegpu > withj.QDesireLimitedpu)
		currentkvarlimitnegpu	= withj.QDesireLimitedpu;

    // Q curve desiredpu should be less than currentkvarlimit(neg)
	if((Q > 0.0) && (Abs( Q) >= Abs( currentkvarlimitpu)))
	{
		FOperation = 0.2 * Sign(Q); // When kvarlimit is exceeded
		withj.QDesireLimitedpu = currentkvarlimitpu * Sign(Q);
	}
	else
	{
		if((Q < 0.0) && (Abs( Q) >= Abs( currentkvarlimitnegpu)))
		{
			FOperation = 0.2 * Sign(Q); // When kvarlimitneg is exceeded
			withj.QDesireLimitedpu = currentkvarlimitnegpu * Sign(Q);
		}

    // States Flags
	}
	if(ControlMode == WATTVAR)
		withj.FWVOperation = FOperation;
}

void TInvControlObj::Calc_PQ_WV(int j, int ActorID)
{
	double QPratio						= 0.0;
	TCoeff Coeff;
	double pre_S						= 0.0;
	double var_limit_operation_value	= 0.0;
	double Qbase						= 0.0;
	double Qbasesign					= 0.0;
	double PBase						= 0.0;
	double A							= 0.0;
	double B							= 0.0;
	double C							= 0.0;
	double a_line						= 0.0;
	double b_line						= 0.0;
	auto& withj							= CtrlVars[j];

	PBase = min(withj.FkVArating, withj.FDCkWRated);
	if(withj.QDesiredWV >= 0.0)
	{
		Qbase = withj.QHeadRoom;
		Qbasesign = 1.0;
	}
	else
	{
		Qbase = withj.QHeadRoomNeg;
		Qbasesign = -1.0;
	}
	var_limit_operation_value = 0.2;
	if(Abs(withj.FWVOperation) == var_limit_operation_value)
		withj.PLimitEndpu = Fwattvar_curve->GetXValue(withj.QDesireEndpu);
	else
		withj.PLimitEndpu = 1.0;
	CalcWATTVAR_vars(j, ActorID);

    // Qdesiredpu should be less than the Q avaliable under watt priority  (works just for varmax)
	if(sqrt(Sqr(withj.FDCkW * withj.FEffFactor * withj.FpctDCkWRated * withj.PLimitEndpu) + Sqr(withj.QDesiredWV)) > withj.FkVArating)
	{
		Coeff = Fwattvar_curve->GetCoefficients(withj.FDCkW * withj.FEffFactor * withj.FpctDCkWRated / PBase);
		a_line = Coeff[0] * Qbase / PBase;
		b_line = Coeff[1] * Qbase;
		A = 1.0 + Sqr(a_line);
		B = 2 * a_line * b_line;
		C = Sqr(b_line) - Sqr(withj.FkVArating);
		withj.PLimitEndpu = (-B + sqrt(Sqr(B) - 4.0 * A * C)) / (2 * A * PBase);
		withj.QDesireEndpu = Fwattvar_curve->GetYValue_(withj.PLimitEndpu);
	}
	CalcWATTVAR_vars(j, ActorID);
}

void TInvControlObj::Check_Qlimits(int j, double Q, int ActorID)
{
	double Q_Ppriority				= 0.0;
	double currentkvarlimitpu		= 0.0;
	double currentkvarlimitnegpu	= 0.0;
	double FOperation				= 0.0;
	double Error					= 0.0;
	auto& withj						= CtrlVars[j];
    // states
	Error = 0;
	if(ControlMode == VOLTVAR)
		Error = 0.005;
	if(ControlMode == WATTPF)
		Error = 0.005;
	if(ControlMode == WATTVAR)
		Error = 0.005;
	if(ControlMode == DRC)
		Error = 0.0005;
	if(ControlMode == AVR)
		Error = 0.005;
	if(CombiControlMode == VV_DRC)
		Error = 0.005;
	if(CombiControlMode == VV_VW)
		Error = 0.005;
	if(Q <  - Error)
		FOperation = -1.0;
	else
	{
		if(Q > Error)
			FOperation = 1.0;
		else
			FOperation = 0.0;
	}
	withj.QDesireLimitedpu	= 1.0; // Not limited
	currentkvarlimitpu		= withj.FCurrentkvarLimit / withj.QHeadRoom;
	currentkvarlimitnegpu	= withj.FCurrentkvarLimitNeg / withj.QHeadRoomNeg;
	if(currentkvarlimitpu > withj.QDesireLimitedpu)
		currentkvarlimitpu		= withj.QDesireLimitedpu;
	if(currentkvarlimitnegpu > withj.QDesireLimitedpu)
		currentkvarlimitnegpu	= withj.QDesireLimitedpu;

    // Q curve desiredpu should be less than currentkvarlimit(neg)
	if((Q > 0.0) && (Abs( Q) >= Abs( currentkvarlimitpu)))
	{
		FOperation				= 0.2 * Sign(Q); // When kvarlimit is exceeded
		withj.QDesireLimitedpu	= currentkvarlimitpu * Sign(Q);
	}
	else
	{
		if((Q < 0.0) && (Abs( Q) >= Abs( currentkvarlimitnegpu)))
		{
			FOperation				= 0.2 * Sign(Q); // When kvarlimitneg is exceeded
			withj.QDesireLimitedpu	= currentkvarlimitnegpu * Sign(Q);
		}

    // Qdesiredpu should be less than the Q avaliable under watt priority  (works just for varmax)
	}
	if(withj.FPPriority && ((FReacPower_ref == "VARMAX") || (ControlMode == WATTPF)))
	{
		if(Q >= 0.0)
			Q_Ppriority = sqrt(Sqr(withj.FkVArating) - Sqr(withj.FpresentkW)) / withj.QHeadRoom;
		else
			Q_Ppriority = sqrt(Sqr(withj.FkVArating) - Sqr(withj.FpresentkW)) / withj.QHeadRoomNeg;
		if((Abs( Q_Ppriority) < Abs(withj.QDesireLimitedpu)) && (Abs( Q_Ppriority) < Abs( Q)))
		{
			FOperation = 0.6 * Sign(Q); // kVA exceeded under watt priority is considered above
			if((Abs( Q) < (0.01 / 100)) || (Abs( Q_Ppriority) < EPSILON))
				FOperation = 0.0;
			withj.QDesireLimitedpu = Q_Ppriority * Sign(Q);
		}
	}


    // States Flags
	if(ControlMode == VOLTVAR)
		withj.FVVOperation		= FOperation;
	if(ControlMode == WATTPF)
		withj.FWPOperation		= FOperation;
	if(ControlMode == WATTVAR)
		withj.FWVOperation		= FOperation;
	if(ControlMode == DRC)
		withj.FDRCOperation		= FOperation;
	if(ControlMode == AVR)
		withj.FAVROperation		= FOperation;
	if(CombiControlMode == VV_DRC)
		withj.FVVDRCOperation	= FOperation;
	if(CombiControlMode == VV_VW)
		withj.FVVOperation		= FOperation;
}

void TInvControlObj::Calc_QHeadRoom(int j, int ActorID)
{
	auto& withj = CtrlVars[j];

	if(FReacPower_ref == "VARAVAL")
	{
		if(Abs(withj.FpresentkW) < withj.FkVArating)
			withj.QHeadRoom = sqrt(Sqr(withj.FkVArating) - Sqr(withj.FpresentkW));
		else
			withj.QHeadRoom = 0.0;
		withj.QHeadRoomNeg = withj.QHeadRoom;
	}
	if((FReacPower_ref == "VARMAX") || (ControlMode == WATTPF))
	{
		withj.QHeadRoom		= withj.Fkvarlimit;
		withj.QHeadRoomNeg	= withj.FkvarLimitNeg;
	}
	if(withj.QHeadRoom == 0.0)
		withj.QHeadRoom		= withj.Fkvarlimit;
	if(withj.QHeadRoomNeg == 0.0)
		withj.QHeadRoomNeg	= withj.FkvarLimitNeg;
}

void TInvControlObj::Change_deltaQ_factor(int ActorID, int j)
{
	double DeltaV	= 0.0;
	auto& withj		= CtrlVars[j];

	DeltaV = Abs( (withj.FPresentVpu - withj.FAvgpVpuPrior));
	if(withj.DeltaV_old >= 0.0)
	{
		if((Abs( DeltaV) > 0.8 * withj.DeltaV_old) && (withj.FdeltaQFactor > 0.2))
			withj.FdeltaQFactor = withj.FdeltaQFactor - 0.1;
		else
		{
			if((Abs( DeltaV) > 0.6 * withj.DeltaV_old) && (withj.FdeltaQFactor > 0.2))
				withj.FdeltaQFactor = withj.FdeltaQFactor - 0.05;
			else
			{
				if((Abs( DeltaV) < 0.2 * withj.DeltaV_old) && (withj.FdeltaQFactor < 0.9))
					withj.FdeltaQFactor = withj.FdeltaQFactor + 0.1;
				else
				{
					if((Abs( DeltaV) < 0.4 * withj.DeltaV_old) && (withj.FdeltaQFactor < 0.9))
						withj.FdeltaQFactor = withj.FdeltaQFactor + 0.05;
				}
			}
		}
	}
	withj.DeltaV_old = Abs( (withj.FPresentVpu - withj.FAvgpVpuPrior));
}

void TInvControlObj::Change_deltaP_factor(int ActorID, int j)
{
	double DeltaV	= 0.0;
	auto& withj		= CtrlVars[j];

	DeltaV = Abs( (withj.FPresentVpu - withj.FAvgpVpuPrior));
	if(withj.DeltaV_old >= 0.0)
	{
		if((Abs( DeltaV) > 0.9 * withj.DeltaV_old) && (withj.FdeltaPFactor > 0.2))
			withj.FdeltaPFactor = withj.FdeltaPFactor - 0.1;
		else
		{
			if((Abs( DeltaV) > 0.8 * withj.DeltaV_old) && (withj.FdeltaPFactor > 0.1))
				withj.FdeltaPFactor = withj.FdeltaPFactor - 0.05;
			else
			{
				if((Abs( DeltaV) < 0.2 * withj.DeltaV_old) && (withj.FdeltaPFactor < 0.9))
					withj.FdeltaPFactor = withj.FdeltaPFactor + 0.05;
				else
				{
					if((Abs( DeltaV) < 0.1 * withj.DeltaV_old) && (withj.FdeltaPFactor < 0.9))
						withj.FdeltaPFactor = withj.FdeltaPFactor + 0.1;
				}
			}
		}
	}
	withj.DeltaV_old = Abs( (withj.FPresentVpu - withj.FAvgpVpuPrior));
}


//Called at end of main power flow solution loop

void TInvControl::UpdateAll(int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = ElementList.get_myNumList(), i = 1; i <= stop; i++)
	{
		/*# with TInvControlObj(ElementList.Get(i)) do */
		{
			auto with0 = ((TInvControlObj*) ElementList.Get(i));
			if(with0->Get_Enabled())
				with0->UpdateInvControl(i, ActorID);
		}
	}
}

void TRollAvgWindow::Add(double IncomingSampleValue, double IncomingSampleTime, double VAvgWindowLengthSec)
{
	if((sample.size() > 0) && (bufferfull))
	{
		runningsumsample = runningsumsample - sample.front();
		sample.pop_front();
		if(BufferLength == 0)
		{
			IncomingSampleValue = 0.0;
		}
		sample.push_back(IncomingSampleValue);
		runningsumsample = runningsumsample + IncomingSampleValue;
		runningsumsampletime = runningsumsampletime - sampletime.front();
		sampletime.pop_front();
		sampletime.push_back(IncomingSampleTime);
		runningsumsampletime = runningsumsampletime + IncomingSampleTime;
	}
	else
	{
		if(BufferLength == 0)
		{
			IncomingSampleValue = 0.0;
		}
		sample.push_back(IncomingSampleValue);
		runningsumsample = runningsumsample + IncomingSampleValue;
		sampletime.push_back(IncomingSampleTime);
		runningsumsampletime = runningsumsampletime + IncomingSampleTime;
		if(runningsumsampletime > VAvgWindowLengthSec)
			bufferfull = true;
		if(sample.size() == BufferLength)
			bufferfull = true;
	}
}

TRollAvgWindow::TRollAvgWindow()
 : 			runningsumsample(0.0),
			runningsumsampletime(0.0),
			BufferLength(0),
			bufferfull(false)
{
	sample.clear();
	sampletime.clear();
}

TRollAvgWindow::~TRollAvgWindow()
{
	sample.clear();
	sampletime.clear();
	// inherited;
}


void TRollAvgWindow::Set_BuffLength(int Value)
{
	BufferLength = Value;
}

double TRollAvgWindow::Get_AvgVal()
{
	double result = 0.0;
	if(sample.size() == 0)
		result = 0.0;
	else
		result = runningsumsample / sample.size();
	return result;
}

double TRollAvgWindow::Get_AccumSec()
{
	double result = 0.0;
	if(sample.size() == 0)
		result = 0.0;
	else
		result = runningsumsampletime;
	return result;
}




}  // namespace InvControl





