#ifndef VVControlH
#define VVControlH

#include <System.hpp>
#include "../Support/d2c_system.h"

#include "../Shared/Command.h"
#include "../Controls/ControlClass.h"
#include "../Controls/ControlElem.h"
#include "../Common/CktElement.h"
#include "../Common/Bus.h"
#include "../Common/DSSClass.h"
#include "../shared/Arraydef.h"
#include "../Shared/Ucomplex.h"
#include "../Common/Utilities.h"
#include "../General/XYcurve.h"
#include "../Shared/PointerList.h"
#include <System.Classes.hpp>
#include "../Support/d2c_sysfile.h"

namespace VVControl
{



// added using GenDispatcher as a template
/*
  ----------------------------------------------------------
  Copyright (c) 2010, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
  A VVCControl is a control element that is connected to a terminal of another
  circuit element and sends dispatch kW signals and kvar to a set of generators it controls

  A VVCControl is defined by a New command:

  New VVCControl.Name=myname Element=devclass.name terminal=[ 1|2|...] GenList = (gen1  gen2 ...)


*/
  // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TVVControl : public ControlClass::TControlClass
{
	friend class TVVControlObj;
public:
	typedef ControlClass::TControlClass inherited;	
private:
	DSSClass::TDSSClass* XY_CurveClass;
protected:
	void DefineProperties();
	virtual int MakeLike(const String VVCControlName);
public:
	TVVControl();
	virtual ~TVVControl();
	virtual int Edit(); // uses global parser
	virtual int NewObject(const String ObjName);
	XYCurve::TXYcurveObj* GetVVCCurve(const String CurveName);
};

  // = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TVVControlObj : public ControlElem::TControlElem
{
	friend class TVVControl;
public:
	typedef ControlElem::TControlElem inherited;	
private:
    // kw limit at the monitored element
 // kvar limit at the monitored element
 // tolerance of voltage change from one solution to the
    // next for the voltage at the monitored element - in pu
	double Fvvc_Vmaxpu;
	double Fvvc_Vminpu;
	double Fkva_rating;
	double FkW_rating;
	double Fkvar_fulloutput;
	double Fpf;
	double FDelay;
	double Fdelayoff;
	double FkW_ramp_rate;
	double Fkvar_ramp_rate;
	double FkW_limit;
	double Fkvar_limit;
	double DeltaVTolerance;
	double TotalWeight;
	double QOldDeliver;
	double Qdeliver;
	double QNew;
	double VavgpuPrior;
	double Vavgpu;
	double presentHour;
	int ControlActionHandle;
	int FListSize;
	System::Classes::TStringList* FGeneratorNameList;
	PointerList::TPointerList* FGenPointerList;
	Arraydef::pDoubleArray FWeights;
	int Fvvc_curve_size;
	XYCurve::TXYcurveObj* Fvvc_curve;
	int FPendingChange;
	double FdeltaQ_factor;
	CktElement::TDSSCktElement* MonitoredElement;
	Ucomplex::pComplexArray cBuffer; // Complexarray buffer
	int CondOffset; // Offset for monitored terminal
	void Set_PendingChange(int Value);
public:
	__property int PendingChange = { read = FPendingChange, write = Set_PendingChange };
	TVVControlObj(DSSClass::TDSSClass* ParClass, const String VVCControlName);
	virtual ~TVVControlObj();
	virtual void MakePosSequence(); // Make a positive Sequence Model
	virtual void RecalcElementData();
	virtual void CalcYPrim(); // Always Zero for a VVCControl
	virtual void sample(); // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl);
    // Do the action that is pending from last sample
	virtual void Reset(); // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr);
    // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr);
    // Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(d2c_system::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	bool MakeGenList();
	String ReturnGensList();
	String ReturnWeightsList();
	String ReturnVVCurve();
	TVVControlObj(DSSClass::TDSSClass* ParClass);
	TVVControlObj(String ClassName);
	TVVControlObj();
};
extern TVVControlObj* ActiveVVCControlObj;

  /* -------------------------------------------------------------------------- */


}  // namespace VVControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace VVControl;
#endif

#endif // VVControlH





