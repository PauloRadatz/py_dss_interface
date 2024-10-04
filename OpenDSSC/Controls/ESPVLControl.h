#ifndef ESPVLControlH
#define ESPVLControlH

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


namespace ESPVLControl
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
  An ESPVLControl is a control element that is connected to a terminal of another
  circuit element (a PVSystem) and sends dispatch kW signals to a set of Storage Elements it controls

  An ESPVLControl is either a System Controller or a Local Controller, set by the "Type" property.
  A System Controller controls one or more Local Controllers
  A Local Controller controls one or more PVSystem elements and one or more Storage elements.

  An ESPVLControl is defined by a New command:

  New ESPVLControl.Name=myname Element=devclass.name terminal=[ 1|2|...] StorageList = (gen1  gen2 ...)

 
*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TESPVLControl : public ControlClass::TControlClass
{
	friend class TESPVLControlObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String ESPVLControlName);
public:
	TESPVLControl();
	virtual ~TESPVLControl();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TESPVLControlObj : public ControlElem::TControlElem
{
	friend class TESPVLControl;
public:
	typedef ControlElem::TControlElem inherited;	
private:
	int fType;   /*1=System controller; 2=Local controller*/

     /*System Controller Variables*/

            // Local Controllers under supervision of System Controller
	int FLocalControlListSize;
	TStringList* FLocalControlNameList;
	PointerList::TPointerList* FLocalControlPointerList;
	Arraydef::pDoubleArray FLocalControlWeights;


     /*Local Controller Variables*/

             // PVSystems under supervision of this Local Controller
	int FPVsystemListSize;
	TStringList* FPVSystemNameList;
	PointerList::TPointerList* FPVSystemPointerList;
	Arraydef::pDoubleArray FPVSystemWeights;

             // Storage Devices under supervision of this Local Controller
	int FStorageListSize;
	TStringList* FStorageNameList;
	PointerList::TPointerList* FStoragePointerList;
	Arraydef::pDoubleArray FStorageWeights;

// dead band control parameters
	double FkWLimit;
	double FkWBand;
	double HalfkWBand;
	double Fkvarlimit;
	double TotalWeight;



  //          YearlyShape     :String;  // ='fixed' means no variation  on all the time
   //         YearlyShapeObj  :TLoadShapeObj;  // Shape for this Storage element
	String DailyForecastShape;  // Daily (24 HR) Storage element shape
	LoadShape::TLoadShapeObj* DailyForecasstShapeObj;  // Daily Storage element Shape for this load
  //          DutyShape       :String;  // Duty cycle load shape for changes typically less than one hour
  //          DutyShapeObj    :TLoadShapeObj;  // Shape for this Storage element
	Ucomplex::complex LoadShapeMult;
public:
	TESPVLControlObj(DSSClass::TDSSClass* ParClass, const String ESPVLControlName);
	virtual ~TESPVLControlObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a ESPVLControl
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	bool MakeLocalControlList();
	TESPVLControlObj(DSSClass::TDSSClass* ParClass);
	TESPVLControlObj(String ClassName);
	TESPVLControlObj();
};
extern TESPVLControl* ESPVLControlClass;
extern TESPVLControlObj* ActiveESPVLControlObj;

/*--------------------------------------------------------------------------*/


}  // namespace ESPVLControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ESPVLControl;
#endif

#endif // ESPVLControlH





