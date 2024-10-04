#ifndef StorageControllerH
#define StorageControllerH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "PointerList.h"
#include "LoadShape.h"
#include "d2c_structures.h"

namespace StorageController
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
  A StorageController is a control element that is connected to a terminal of another
  circuit element and sends dispatch  signals to a fleet of energy storage elements it controls

  A StorageController is defined by a New command:

  New StorageController.Name=myname Element=devclass.name terminal=[ 1|2|...] Elementlist = (elem1  elem2 ...)

  or ... ElementList = [File=filename] where storage class elements are listed one to a line
  If omitted, all storage elements found in the active circuit are included by default and controlled as a fleet.

  Added new control mode for charging 12/19/2018
  Proposed by Valentin Rigoni

*/
const int AVG = -1;
const int MAXPHASE = -2;
const int MINPHASE = -3;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TStorageController : public ControlClass::TControlClass
{
	friend class TStorageControllerObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String StorageController2Name);
public:
	TStorageController();
	virtual ~TStorageController();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TStorageControllerObj : public ControlElem::TControlElem
{
	friend class TStorageController;
public:
	typedef ControlElem::TControlElem inherited;	
private:
//            FPFTarget,                  // Range on this is 0..2 where 1..2 is leading

//            HalfPFBand,
//            FPFBand,

//            pctkvarRate,
	double FkWTarget;
	double FkWTargetLow;
	double FkWThreshold;
	double FpctkWBand;
	double FkWBand;
	double FpctkWBandLow;
	double FkWBandLow;
	double HalfkWBand;
	double HalfkWBandLow;
	double TotalWeight;
	double UpRamptime;
	double FlatTime;
	double DnrampTime;
	double UpPlusFlat;
	double UpPlusFlatPlusDn;
	double DischargeTriggerTime;
	double ChargeTriggerTime;
	double pctKWRate;
	double pctChargeRate;
	double LastpctDischargeRate;
	double TotalkWCapacity;
	double TotalkWhCapacity;
	double pctFleetReserve;
	double ResetLevel;
	double kWNeeded;
	double DispFactor;  // for slower convergence
	TStringList FStorageNameList;
	PointerList::TPointerList* FleetPointerList;
	Arraydef::TRatingsArray SeasonTargets;
	Arraydef::TRatingsArray SeasonTargetsLow;
	Arraydef::pDoubleArray FWeights;
	std::vector <complex> cBuffer;    // Complex Array buffer

//            DispatchVars,
	bool FleetListChanged;
	bool ChargingAllowed;
	bool DischargeTriggeredByTime;
	bool DischargeInhibited;
	bool OutOfOomph;
	bool FElementListSpecified;
	bool Wait4Step;
	bool FkWBandSpecified;  // true if kWBand specified as an absolute value (for use in Follow Discharge Mode to update the target)
	int Seasons;
	int FleetSize;
	int FleetState;
	int DischargeMode;
	int InhibitHrs;
	int ChargeMode;
	int FMonPhase;
	int CondOffset;
	String YearlyShape;  // ='fixed' means no variation  on all the time
	LoadShape::TLoadShapeObj* YearlyShapeObj;  // Shape for this Storage element
	String DailyShape;  // Daily (24 HR) Storage element shape
	LoadShape::TLoadShapeObj* DailyShapeObj;  // Daily Storage element Shape for this load
	String DutyShape;  // Duty cycle load shape for changes typically less than one hour
	LoadShape::TLoadShapeObj* DutyShapeObj;  // Shape for this Storage element
	Ucomplex::complex LoadShapeMult;

           // PROCEDURE SetPctReserve;
	void SetAllFleetValues();
	void SetFleetkWRate(double pctkw);
//            PROCEDURE SetFleetkvarRate(pctkvar:Double);
	void SetFleetChargeRate();
	void SetFleetToCharge();
	void SetFleetToDisCharge();
	void SetFleetToIdle();
	void SetFleetToExternal();
	void SetFleetDesiredState(int State);
	int InterpretMode(int Opt, const String s);
	String GetModeString(int Opt, int Mode);
	String GetkWTotal(double& Sum);
	String GetkWhTotal(double& Sum);
	String GetkWhActual();
	String GetkWActual();
	void CalcYearlyMult(double hr);
	void CalcDailyMult(double hr);
	void CalcDutyMult(double hr);
	String ReturnSeasonTarget(int THigh);
	String ReturnElementsList();
	String ReturnWeightsList();
	bool MakeFleetList();
	void DoLoadFollowMode(int ActorID);
	void DoLoadShapeMode(int ActorID);
	void DoTimeMode(int Opt, int ActorID);
	void DoScheduleMode(int ActorID);
	void DoPeakShaveModeLow(int ActorID);
	void PushTimeOntoControlQueue(int Code, int ActorID);
	double NormalizeToTOD(int h, double Sec);
	void GetControlPower(Ucomplex::complex& ControlPower, int ActorID);
	void GetControlCurrent(double& ControlCurrent);
//            procedure Set_PFBand(const Value: Double);
	double Get_FleetkW();
	double Get_FleetkWh();
	double Get_FleetkWhRating();
	double Get_FleetReservekWh();
	double Get_DynamicTarget(int THigh, int ActorID);
public:
	TStorageControllerObj(DSSClass::TDSSClass* ParClass, const String StorageController2Name);
	virtual ~TStorageControllerObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a StorageController
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);

//           Property PFBand   :Double   Read FPFBand  Write  Set_PFBand;
	TStorageControllerObj(DSSClass::TDSSClass* ParClass);
	TStorageControllerObj(String ClassName);
	TStorageControllerObj();
};
extern TStorageControllerObj* ActiveStorageController2Obj;

/*--------------------------------------------------------------------------*/


}  // namespace StorageController

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace StorageController;
#endif

#endif // StorageControllerH





