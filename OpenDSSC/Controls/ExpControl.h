#ifndef ExpControlH
#define ExpControlH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "PVsystem.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "Dynamics.h"
#include "PointerList.h"
#include "d2c_structures.h"

class TIEEE1547Controller;

namespace ExpControl
{



/*
  ----------------------------------------------------------
  Copyright (c) 2015-2022, University of Pittsburgh
  Copyright (c) 2019-2022, Battelle Memorial Institute
  All rights reserved.
  ----------------------------------------------------------

  Notes: adapted and simplified from InvControl for adaptive controller research
*/

class TExpControl : public ControlClass::TControlClass
{
	friend class TExpControlObj;
public:
	typedef ControlClass::TControlClass inherited;	
protected:
	void DefineProperties();
	virtual int MakeLike(const String ExpControlName);
public:
	TExpControl();
	virtual ~TExpControl();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
	void UpdateAll(int ActorID);
};

class TExpControlObj : public ControlElem::TControlElem
{
	friend class TExpControl;
	friend class ::TIEEE1547Controller;	
public:
	typedef ControlElem::TControlElem inherited;	
private:
	int ControlActionHandle;
	std::vector <TPVsystemObj*> ControlledElement;    // list of pointers to controlled PVSystem elements

            // PVSystemList information
	int FListSize;
        TStringList* FPVSystemNameList;
        TStringList* FDERNameList;
	PointerList::TPointerList* FPVSystemPointerList;

            // working storage for each PV system under management
	double* FPriorVpu;
	double* FPresentVpu;
	int* FPendingChange;
	double* FVregs;
	double* FLastIterQ; // for DeltaQFactor
	double* FLastStepQ; // for FOpenTau
	double* FTargetQ;
	bool* FWithinTol;

            // temp storage for biggest PV system, not each one
	std::vector <complex> cBuffer;

            // user-supplied parameters (also PVSystemList and EventLog)
	double FVregInit;
	double FSlope;
	double FVregTau;
	double FQbias;
	double FVregMin;
	double FVregMax;
	double FQmaxLead;
	double FQmaxLag;
	double FdeltaQ_factor;
	double FVoltageChangeTolerance; // no user adjustment
	double FVarChangeTolerance;     // no user adjustment
	bool FPreferQ;
	double FTresponse;
	double FOpenTau;
	void Set_PendingChange(int Value, int DevIndex);
	int Get_PendingChange(int DevIndex);
	String ReturnElementsList();
	void UpdateExpControl(int i, int ActorID);
protected:
	virtual void Set_Enabled(bool Value);
public:
	TExpControlObj(DSSClass::TDSSClass* ParClass, const String ExpControlName);
	virtual ~TExpControlObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for an ExpControl

            // Sample control quantities and set action times in Control Queue
	virtual void sample(int ActorID);

            // Do the action that is pending from last sample
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	bool MakePVSystemList();
	virtual String GetPropertyValue(int Index);
	TExpControlObj(DSSClass::TDSSClass* ParClass);
	TExpControlObj(String ClassName);
	TExpControlObj();
};
extern TExpControlObj* ActiveExpControlObj;

/*--------------------------------------------------------------------------*/


}  // namespace ExpControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ExpControl;
#endif

#endif // ExpControlH





