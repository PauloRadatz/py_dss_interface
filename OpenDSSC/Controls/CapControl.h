#ifndef CapControlH
#define CapControlH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "Bus.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "Capacitor.h"
#include "LoadShape.h"
#include "CapControlVars.h"
#include "CapUserControl.h"
#include "d2c_structures.h"


namespace CapControl
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Change Log
   2-14-00 Created

   3-1-00  Added Voltage override
   5/21/01  Fixed bug with number of phases
   5/30/01  Eliminated extra event queue reports
*/

/*
  A CapControl is a control element that is connected to a terminal of another
  circuit element and controls a capacitor.  The control is usually placed in the
  terminal of a line or transformer, although a voltage control device could be placed
  in the terminal of the capacitor it controls

  A CapControl is defined by a New command:

  New CapControl.Name=myname Element=devclass.name terminal=[ 1|2|...] Capacitor = name

  Capacitor to be controlled must already exist.
*/




// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TCapControl : public ControlClass::TControlClass
{
	friend class TCapControlObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String CapControlName);
public:
	TCapControl();
	virtual ~TCapControl();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TCapControlObj : public ControlElem::TControlElem
{
	friend class TCapControl;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
	CapControlVars::ECapControlType ControlType;
	CapControlVars::TCapControlVars ControlVars;
	Capacitor::TCapacitorObj* ControlledCapacitor;
	std::vector <complex> cBuffer;    // Complexarray buffer
	bool IsUserModel;
	CapUserControl::TCapUserControl* UserModel;
	double FpctMinkvar;
	Capacitor::TCapacitorObj* Get_Capacitor(); // Pointer to controlled Capacitor
	double NormalizeToTOD(int h, double Sec);
	void Set_PendingChange(const EControlAction Value);
	EControlAction Get_PendingChange();
	void GetControlVoltage(double& ControlVoltage);
	void GetControlCurrent(double& ControlCurrent);
	void GetBusVoltages(Bus::TDSSBus* pBus, Ucomplex::pComplexArray Buff, int ActorID);

	double GetON_Value();
	double GetOFF_Value();
	double GetPFON_Value();
	double GetPFOFF_Value();
	double GetPTRatio_Value();
	double GetCTRatio_Value();
	double GetONDelay_Value();
	double GetOFFDelay_Value();
	double GetVmin_Value();
	double GetVmax_Value();
	bool GetVoverride_Value();
	double GetDeadTime_Value();
	int GetFPTPhase_Value();

public:
	String			myShapeName;															// Name of the shape for this controller
	TLoadShapeObj*	myShapeObj;																// Shape for this controller

	TCapControlObj(DSSClass::TDSSClass* ParClass, const String CapControlName);
	virtual ~TCapControlObj();
	virtual void MakePosSequence(int ActorID);									// Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);											// Always Zero for a CapControl
	virtual void sample(int ActorID);											// Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);			// Do the action that is pending from last sample
	virtual void Reset(int ActorID);												// Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);			// Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);		// Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

	ECapControlType get_ControlType();
	void set_ControlType(CapControlVars::ECapControlType value);

       // for CIM export, which doesn't yet use the delays, CT, PT, and voltage override
	  //  (properties removed, use getters/setters)

	TCapControlObj(DSSClass::TDSSClass* ParClass);
	TCapControlObj(String ClassName);
	TCapControlObj();
};
extern TCapControlObj* ActiveCapControlObj;

/*--------------------------------------------------------------------------*/


}  // namespace CapControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CapControl;
#endif

#endif // CapControlH






