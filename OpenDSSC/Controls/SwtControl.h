#ifndef SwtControlH
#define SwtControlH

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
#include "d2c_structures.h"

namespace SwtControl
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------*/

class TSwtControl : public ControlClass::TControlClass
{
	friend class TSwtControlObj;
public:
	typedef ControlClass::TControlClass inherited;	
protected:
	void DefineProperties();
	virtual int MakeLike(const String SwtControlName);
public:
	TSwtControl();
	virtual ~TSwtControl();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

class TSwtControlObj : public ControlElem::TControlElem
{
	friend class TSwtControl;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
	EControlAction FPresentState;
	EControlAction FNormalState;
	EControlAction ActionCommand;
	EControlAction LockCommand;
	bool FLocked;
	bool Armed;
	void InterpretSwitchAction(const String Action);
	void set_NormalState(const EControlAction Value);
	void set_Flocked(bool Value);
	void Set_LastAction(const EControlAction Value);
	void Set_PresentState(const EControlAction Value);
public:
	TSwtControlObj(DSSClass::TDSSClass* ParClass, const String SwtControlName);
	virtual ~TSwtControlObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a SwtControl
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

	EControlAction get_FNormalState();
	EControlAction get_FPresentState();
	bool get_FLocked();
	EControlAction get_ActionCommand();

	TSwtControlObj(DSSClass::TDSSClass* ParClass);
	TSwtControlObj(String ClassName);
	TSwtControlObj();
};
extern TSwtControlObj* ActiveSwtControlObj;

/*--------------------------------------------------------------------------*/


}  // namespace SwtControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace SwtControl;
#endif

#endif // SwtControlH





