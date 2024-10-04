#ifndef RecloserH
#define RecloserH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlActionsDefs.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "TCC_Curve.h"
#include <math.h>
#include "d2c_structures.h"

namespace Recloser
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
    Created 11-1-00 from Relay Control


*/
/*
  A Recloser is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  CktElement to be controlled must already exist.

  7-18-2002  Fixed typos in help
  5-1-2006  Added Time Delays to be compatible with relays

*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TRecloser : public ControlClass::TControlClass
{
	friend class TRecloserObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String RecloserName);
public:
	TRecloser();
	virtual ~TRecloser();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TRecloserObj : public ControlElem::TControlElem
{
	friend class TRecloser;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
	TCC_Curve::TTCC_CurveObj* PhaseDelayed;
	TCC_Curve::TTCC_CurveObj* GroundDelayed;
	TCC_Curve::TTCC_CurveObj* PhaseFast;
	TCC_Curve::TTCC_CurveObj* GroundFast;
	double ResetTime;
	double DelayTime;
	double TDGrDelayed;
	double TDPhDelayed;
	double TDGrFast;
	double TDPhFast;
	EControlAction FPresentState;
	EControlAction FNormalState;
	int OperationCount;
	bool LockedOut;
	bool ArmedForClose;
	bool ArmedForOpen;
	bool GroundTarget;
	bool PhaseTarget;
	bool NormalStateSet;
	int CondOffset; // Offset for monitored terminal
	Ucomplex::pComplexArray cBuffer;    // Complexarray buffer
	void InterpretRecloserState(int ActorID, const String Action, const String property_name);
	EControlAction get_State();
	void set_State(const EControlAction Value);
	EControlAction get_NormalState();
	void set_NormalState(const EControlAction Value);
public:
	Arraydef::pDoubleArray RecloseIntervals;
	int NumFast;
	int NumReclose;
	String MonitoredElementName;
	int MonitoredElementTerminal;
	double PhaseTrip;
	double GroundTrip;
	double PhaseInst;
	double GroundInst;
	TRecloserObj(DSSClass::TDSSClass* ParClass, const String RecloserName);
	virtual ~TRecloserObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a Recloser
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TRecloserObj(DSSClass::TDSSClass* ParClass);
	TRecloserObj(String ClassName);
	TRecloserObj();
};
extern TRecloserObj* ActiveRecloserObj;
extern TRecloser* RecloserClass;


/*--------------------------------------------------------------------------*/


}  // namespace Recloser

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Recloser;
#endif

#endif // RecloserH





