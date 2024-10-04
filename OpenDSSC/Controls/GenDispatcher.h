#ifndef GenDispatcherH
#define GenDispatcherH

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

namespace GenDispatcher
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
  A GenDispatcher is a control element that is connected to a terminal of another
  circuit element and sends dispatch kW signals to a set of generators it controls

  A GenDispatcher is defined by a New command:

  New GenDispatcher.Name=myname Element=devclass.name terminal=[ 1|2|...] CapacitorList = (gen1  gen2 ...)

 
*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TGenDispatcher : public ControlClass::TControlClass
{
	friend class TGenDispatcherObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String GenDispatcherName);
public:
	TGenDispatcher();
	virtual ~TGenDispatcher();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TGenDispatcherObj : public ControlElem::TControlElem
{
	friend class TGenDispatcher;
public:
	typedef ControlElem::TControlElem inherited;	
private:
	double FkWLimit;
	double FkWBand;
	double HalfkWBand;
	double Fkvarlimit;
	double TotalWeight;
	int FListSize;
	TStringList* FGeneratorNameList;
	PointerList::TPointerList* FGenPointerList;
	Arraydef::pDoubleArray FWeights;
public:
	TGenDispatcherObj(DSSClass::TDSSClass* ParClass, const String GenDispatcherName);
	virtual ~TGenDispatcherObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a GenDispatcher
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	bool MakeGenList();
	TGenDispatcherObj(DSSClass::TDSSClass* ParClass);
	TGenDispatcherObj(String ClassName);
	TGenDispatcherObj();
};
extern TGenDispatcherObj* ActiveGenDispatcherObj;

/*--------------------------------------------------------------------------*/


}  // namespace GenDispatcher

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace GenDispatcher;
#endif

#endif // GenDispatcherH





