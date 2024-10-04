#ifndef UPFCControlH
#define UPFCControlH

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
#include "d2c_structures.h"

namespace UPFCControl
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
  A UPFCControl is a control element that is connected to a terminal of another
  circuit element and sends dispatch kW signals to a set of generators it controls

  A UPFCControl is defined by a New command:

  New UPFCControl.Name=myname Element=devclass.name terminal=[ 1|2|...] CapacitorList = (gen1  gen2 ...)

 
*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TUPFCControl : public ControlClass::TControlClass
{
	friend class TUPFCControlObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String UPFCControlName);
public:
	TUPFCControl();
	virtual ~TUPFCControl();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TUPFCControlObj : public ControlElem::TControlElem
{
	friend class TUPFCControl;
public:
	typedef ControlElem::TControlElem inherited;	
//private:
	TStringList FUPFCNameList;
	PointerList::TPointerList* FUPFCList;
	int FListSize;
	Arraydef::pDoubleArray FWeights;
	double TotalWeight;
public:
	TUPFCControlObj(DSSClass::TDSSClass* ParClass, const String UPFCControlName);
	virtual ~TUPFCControlObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a UPFCControl
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	bool MakeUPFCList();

	PointerList::TPointerList* get_FUPFCList();
	void set_FUPFCList(PointerList::TPointerList* myPtr);

	int get_FListSize();
	void set_FListSize(int mySize);

      // Public properties
	TUPFCControlObj(DSSClass::TDSSClass* ParClass);
	TUPFCControlObj(String ClassName);
	TUPFCControlObj();
};
extern TUPFCControlObj* ActiveUPFCControlObj;

/*--------------------------------------------------------------------------*/


}  // namespace UPFCControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace UPFCControl;
#endif

#endif // UPFCControlH





