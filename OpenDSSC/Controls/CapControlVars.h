#ifndef CapControlVarsH
#define CapControlVarsH

#include "System.h"
#include "Sysutils.h"

#include "Ucomplex.h"
#include "ControlElem.h"
#include "ControlActionsDefs.h"

namespace CapControlVars
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*Header file for CapControlVars*/

/* For user DLL, import Definitions of control actions directly */
enum ECapControlType {CURRENTCONTROL,
                      VOLTAGECONTROL,
                      KVARCONTROL,
                      TIMECONTROL,
                      PFCONTROL,
					  FOLLOWCONTROL,
                      USERCONTROL };



  /*Fixed record structure for Public CapControl variables*/
#pragma pack (push, 1)


struct TCapControlVars
{
	int FCTPhase;
	int FPTPhase;   // "ALL" is -1
	double ON_Value;
	double OFF_Value;
	double PFON_Value;
	double PFOFF_Value;
	double CTRatio;
	double PTRatio;
	double OnDelay;
	double OFFDelay;
	double DeadTime;
	double LastOpenTime;
	bool Voverride;
	bool VoverrideEvent;
	bool VoverrideBusSpecified;     // Added 8-11-11
	int VOverrideBusIndex;
	double Vmax;
	double Vmin;
	EControlAction FPendingChange;
	bool ShouldSwitch;  // True: action is pending
	bool Armed;  // Control is armed for switching unless reset
	EControlAction PresentState;
	EControlAction InitialState;
	Ucomplex::complex SampleP;        // two 64-bit numbers, kW, kvar
	double SampleV;
	double SampleCurr;
	int NumCapSteps;
	int AvailableSteps;   // available steps in controlled capacitor
	int LastStepInService;   // Change this to force an update of cap states
	char const* VOverrideBusNamePtr; // Required for backwards compatibility, points to VOverrideBusName's data
	char const* CapacitorNamePtr; // Required for backwards compatibility, points to CapacitorName's data
	int ControlActionHandle;
	int CondOffset; // Offset for monitored terminal

	String VOverrideBusName;
	String CapacitorName;
};
#pragma pack (pop)



}  // namespace CapControlVars

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CapControlVars;
#endif

#endif // CapControlVarsH





