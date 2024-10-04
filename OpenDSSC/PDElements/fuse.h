#ifndef fuseH
#define fuseH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "ControlActionsDefs.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "TCC_Curve.h"
#include <math.h>
#include "d2c_structures.h"



namespace fuse
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
    Created 11-1-00 from Recloser Control
*/
/*
  A Fuse is a control element that is connected to a terminal of a
  circuit element and controls the switches in the same or another terminal.

  The control is usually placed in the
  terminal of a line or transformer, but it could be any element

  CktElement to be controlled must already exist.

*/
const int FUSEMAXDIM = 6;
typedef EControlAction StateArray[6/*# range 1..FUSEMAXDIM*/];
typedef StateArray* pStateArray;  // 0 = open 1 = close

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TFuse : public ControlClass::TControlClass
{
	friend class TFuseObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String FuseName);
public:
	TFuse();
	virtual ~TFuse();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TFuseObj : public ControlElem::TControlElem
{
	friend class TFuse;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
	CktElement::TDSSCktElement* MonitoredElement;
	int hAction[6/*# range 1..FUSEMAXDIM*/];         // handle to control queue actions
	pStateArray FPresentState;
	pStateArray FNormalState;
	bool ReadyToBlow[6/*# range 1..FUSEMAXDIM*/];
	int CondOffset; // Offset for monitored terminal
	Ucomplex::pComplexArray cBuffer;    // Complexarray buffer
	bool NormalStateSet;
	void InterpretFuseState(int ActorID, const String Param, const String property_name);
	EControlAction get_States(int Idx);
	void set_States(int Idx, const EControlAction Value);
	EControlAction get_NormalStates(int Idx);
	void set_NormalStates(int Idx, const EControlAction Value);
public:
	TCC_Curve::TTCC_CurveObj* FuseCurve;
	double RatedCurrent;
	double DelayTime;
	String MonitoredElementName;
	int MonitoredElementTerminal;
	TFuseObj(DSSClass::TDSSClass* ParClass, const String FuseName);
	virtual ~TFuseObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a Fuse
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Phs, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TFuseObj(DSSClass::TDSSClass* ParClass);
	TFuseObj(String ClassName);
	TFuseObj();
};
extern TFuseObj* ActiveFuseObj;
extern TFuse* FuseClass;

/*--------------------------------------------------------------------------*/


}  // namespace fuse

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace fuse;
#endif

#endif // fuseH




