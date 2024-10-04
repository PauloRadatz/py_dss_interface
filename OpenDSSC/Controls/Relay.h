#ifndef RelayH
#define RelayH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "TCC_Curve.h"
#include <math.h>
#include "d2c_structures.h"
#include "ControlActionsDefs.h"

namespace Relay
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
    Created 8-24-00 from CktElement Control
    9-20-00 Implemented Voltage relay and updated arming logic for all relays
    10-31-00 Added Event Logging
    11-1-00 Added shots=
    3-7-03  Added new property definition process
            Added Neg seq relays and Generic relay
            Added capability to monitor PC Element variable
    2-16-04 Fixed address bug in symmetrical component transformation in 46 relay
    5-1-06 Added Time Dial to Phase and ground
    2-9-21  Added distance (21) and incremental distance (TD21) functions
*/
/*
  A Relay is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  A Relay is defined by a New command:

  New Relay.Name=myname Element=devclass.name terminal=[ 1|2|...] Switch = devclass.name   terminal=[ 1|2|...]
  Type = [current | voltage]
  Phase = TCCCurve
  Ground = TCCCurve
  OverVolt = TCCcurve
  UnderVolt = TCCCurve
  PhaseTrip =  Multipliers times curve
  GroundTrip =
  PhaseInst  =
  GroundInst =
  RecloseIntervals= (array of times, sec);
  ResetTime =

  CktElement to be controlled must already exist.

  Voltage relay is a definite time relay that operates after the voltage stays out of bounds
  for a fixed time interval.  It will then reclose a set time after the voltage comes back in the normal range.

*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TRelay : public ControlClass::TControlClass
{
	friend class TRelayObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
	DSSClass::TDSSClass* TCC_CurveClass;
protected:
	void DefineProperties();
	virtual int MakeLike(const String RelayName);
public:
	TRelay();
	virtual ~TRelay();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
	TCC_Curve::TTCC_CurveObj* GetTccCurve(const String CurveName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TRelayObj : public ControlElem::TControlElem
{
	friend class TRelay;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
	int ControlType;


            /*OverCurrent Relay*/
	TCC_Curve::TTCC_CurveObj* PhaseCurve;
	TCC_Curve::TTCC_CurveObj* GroundCurve;
	double PhaseTrip;
	double GroundTrip;
	double PhaseInst;
	double GroundInst;
	Arraydef::pDoubleArray RecloseIntervals;
	int NumReclose;
	double ResetTime;
	double Delay_Time;
	double Breaker_time;
	double TDPhase;
	double TDGround;
	String RelayTarget;


            /*over/Under Voltage Relay*/                 // Curves assumed in per unit of base voltage
	TCC_Curve::TTCC_CurveObj* OVcurve;
	TCC_Curve::TTCC_CurveObj* UVCurve;   // line-neut volts base
	double VBase;
	double kVBase;

            /*46 Relay  Neg Seq Current*/
	double PickupAmps46;
	double PctPickup46;
	double BaseAmps46;
	double Isqt46;

            /*47 Relay*/
	double PickupVolts47;
	double PctPickup47;

            /*Distance Relay*/
	double Z1Mag;
	double Z1Ang;
	double Z0Mag;
	double Z0Ang;
	double Mphase;
	double Mground;
	Ucomplex::complex Dist_Z1;
	Ucomplex::complex Dist_Z0;
	Ucomplex::complex Dist_K0;
	bool Dist_Reverse;
            /*TD21 Relay*/           // present ring buffer index into td21_h
        // index to one cycle back, and next write location
	int td21_i;
	int td21_next;
	int td21_pt; // number of time samples in td21_h
	int td21_stride;  // length of a time sample in td21_h
	int td21_quiet;   // wait this many samples after an operation
	Ucomplex::pComplexArray td21_h; // VI history pts, vi, phases
	Ucomplex::pComplexArray td21_Uref; // reference (pre-fault) voltages
	Ucomplex::pComplexArray td21_dV; // incremental voltages
	Ucomplex::pComplexArray td21_dI; // incremental currents
	   
	/*Directional Overcurrent Relay*/
	double DOC_TiltAngleLow;  // Tilt angle for low-current trip line
	double DOC_TiltAngleHigh;  // Tilt angle for high-current trip line
	double DOC_TripSetLow;  // Trip setting for low-current trip line
	double DOC_TripSetHigh;  // Trip setting for high-current trip line
	double DOC_TripSetMag;  // Current magnitude trip setting (define a circle for the relay characteristics)
	double DOC_DelayInner;  // Delay for trip in inner region of the DOC characteristic
	double DOC_PhaseTripInner; // Multiplier for TCC Curve for tripping in inner region of the DOC characteristic
	double DOC_TDPhaseInner; // Time Dial for DOC_PhaseTripInner
	bool DOC_P1Blocking; // Block trip if there is no net balanced reverse active power

	TTCC_CurveObj* DOC_PhaseCurveInner;  // TCC Curve for tripping in inner zone of the DOC characteristic

            /*Generic Relay*/
	double OverTrip;
	double UnderTrip;
	EControlAction FPresentState;
	EControlAction FNormalState;
	int OperationCount;
	bool LockedOut;
	bool ArmedForClose;
	bool ArmedForOpen;
	bool ArmedForReset;
	bool PhaseTarget;
	bool GroundTarget;
	bool NormalStateSet;
	double NextTriptime;
	int LastEventHandle;
	int CondOffset; // Offset for monitored terminal
	Ucomplex::pComplexArray cBuffer; // Complexarray buffer for an operating quantity
	Ucomplex::pComplexArray cvBuffer; // for distance and td21 voltages, using cBuffer for hte currents
	bool DebugTrace;
	void InterpretRelayState(int ActorID, const String Action, const String property_name);
	EControlAction get_State();
	void set_State(const EControlAction Value);
	EControlAction get_NormalState();
	void set_NormalState(const EControlAction Value);
	void InterpretRelayType(const String s);
	void OvercurrentLogic(int ActorID);
	void VoltageLogic(int ActorID);
	void RevPowerLogic(int ActorID);
	void NegSeq46Logic(int ActorID);
	void NegSeq47Logic(int ActorID);
	void GenericLogic(int ActorID);
	void DistanceLogic(int ActorID);
	void TD21Logic(int ActorID);
	void DirectionalOvercurrentLogic(int ActorID);
	void GetControlPower(Ucomplex::complex &ControlPower, int ActorID);

public:
	String MonitoredElementName;
	int MonitoredElementTerminal;
	TRelayObj(DSSClass::TDSSClass* ParClass, const String RelayName);
	virtual ~TRelayObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a Relay
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TRelayObj(DSSClass::TDSSClass* ParClass);
	TRelayObj(String ClassName);
	TRelayObj();
};
extern TRelayObj* ActiveRelayObj;
extern TRelay* RelayClass;

/*--------------------------------------------------------------------------*/


}  // namespace Relay

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Relay;
#endif

#endif // RelayH





