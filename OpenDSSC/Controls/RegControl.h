#ifndef RegControlH
#define RegControlH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "Transformer.h"
#include "d2c_structures.h"



namespace RegControl
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Change Log
   1-28-00 Created
   4-29-00 fixed problem with NumPhases = # phases of controlled element
   12/17/01 Added LDC logic
   12/18/01 Added MaxTapChange property and logic
   6/18/11 Updated Rev Power logic
   12/4/2018  Added autotransformer control
*/

/*
  A RegControl is a control element that is connected to a terminal of another
  circuit element that must be a transformer.

  A RegControl is defined by a New command:

  New RegControl.Name=myname Transformer = name Terminal=[1,2,...] Controlledbus=name etc...

  Transformer to be controlled must already exist.
*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TRegControl : public ControlClass::TControlClass
{
	friend class TRegControlObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String RegControlName);
public:
	TRegControl();
	virtual ~TRegControl();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TRegControlObj : public ControlElem::TControlElem
{
	friend class TRegControl;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
	double Vreg;
	double Bandwidth;
	double PTRatio;
	double RemotePTRatio;
	double CTRating;
	double R;
	double X;
	double LDC_Z;

        /*Reverse Power Variables*/   // W
	double revVreg;
	double revBandwidth;
	double RevPowerThreshold;
	double kWRevPowerThreshold;
	double revDelay;
	double revR;
	double revX;
	double revLDC_Z;
	bool IsReversible;
	bool InReverseMode;
	bool ReversePending;
	bool ReverseNeutral;
	bool CogenEnabled;
	bool InCogenMode;
	int RevHandle;
	int RevBackHandle;
	bool LDCActive;
	bool UsingRegulatedBus;
	String RegulatedBus;   // amount of tap change pending
	double FPendingTapChange;
	double TapDelay;   // delay between taps
	bool DebugTrace;
	bool Armed;
	System::TTextRec Tracefile;
	int TapLimitPerChange;
	int TapWinding;  // Added 7-19-07
	bool FInversetime;
	double Vlimit;
	bool VLimitActive;
	int FPTPhase;
	int ControlledPhase;
	int ControlActionHandle;
	Ucomplex::pComplexArray VBuffer;
	Ucomplex::pComplexArray cBuffer;
	Transformer::TTransfObj* Get_Transformer();
	int Get_Winding();
        // CIM accessors
	double Get_MinTap();
	double Get_MaxTap();
	double Get_TapIncrement();
	int Get_NumTaps();
	int Get_TapNum();
	void RegWriteTraceRecord(double TapChangeMade, int ActorID);
	void RegWriteDebugRecord(String s);
	void set_PendingTapChange(double Value);
	double AtLeastOneTap(double ProposedChange, double Increment, int ActorID);
	double ComputeTimeDelay(double Vavg);
	Ucomplex::complex GetControlVoltage(Ucomplex::pComplexArray VBuffer, int Nphs, double PTRatio);
	void Set_TapNum(int Value);
public:
	TRegControlObj(DSSClass::TDSSClass* ParClass, const String RegControlName);
	virtual ~TRegControlObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a RegControl
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual void SaveWrite(System::TTextRec& f);

	double get_FPendingTapChange();
	double get_Vreg();
	double get_Bandwidth();
	double get_CTRating();
	double get_PTRatio();
	double get_R();
	double get_X();
	double get_revR();
	double get_revX();
	double get_revVreg();
	double get_revBandwidth();
	bool get_LDCActive();
	bool get_IsReversible();
	bool get_VLimitActive();
	double get_Vlimit();
	double get_TapDelay();
	int get_TapLimitPerChange();
	bool get_FInversetime();
	
       // CIM XML accessors
	TRegControlObj(DSSClass::TDSSClass* ParClass);
	TRegControlObj(String ClassName);
	TRegControlObj();
};
extern TRegControlObj* ActiveRegControlObj;

/*--------------------------------------------------------------------------*/


}  // namespace RegControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace RegControl;
#endif

#endif // RegControlH





