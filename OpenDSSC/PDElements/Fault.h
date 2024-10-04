#ifndef FaultH
#define FaultH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "PDClass.h"
#include "PDElement.h"
#include "Ucmatrix.h"
#include "Arraydef.h"






namespace Fault
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   3-1-00 Restored old Dump
          Removed 1.e6 multiplier (where did this come from???)
   9-??-00 Added Temporary fault logic       
   9-22-00 Revised Is_ON logic
   7-2-01 Corrected default bus2 phase designation
          Force rebuilding of bus lists if num phases changed
*/

/*
 Fault object:

   One or more faults can be placed across any two buses in the circuit.
   Like the capacitor, the second bus defaults to the ground node of the
   same bus that bus1 is connected to.

   The fault is basically an uncoupled, multiphase resistance branch.  however,
   you may also specify it as NODAL CONDUCTANCE (G) matrix, which will give you
   complete control of a complex fault situation.

   To eliminate a fault from the system after it has been defined, disable it.

   In Monte Carlo Fault mode, the fault resistance is varied by the % std dev specified
   If %Stddev is specified as zero (default), the resistance is varied uniformly.

   Fault may have its "ON" time specified (defaults to 0). When Time (t) exceeds this value, the
   fault will be enabled.  Else it is disabled.

   Fault may be designated as Temporary.  That is, after it is enabled, it will disable itself
   if the fault current drops below the MinAmps value.
*/

class TFault : public PDClass::TPDClass
{
	friend class TFaultObj;
public:
	typedef PDClass::TPDClass inherited;	
private:
	void DoGmatrix(int ActorID);
	void FltSetBus1(const String s);
protected:
	void DefineProperties();
	virtual int MakeLike(const String FaultName);
public:
	TFault();
	virtual ~TFault();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TFaultObj : public PDELement::TPDElement
{
	friend class TFault;
public:
	typedef PDELement::TPDElement inherited;	
private:
	double MinAmps;
	bool IsTemporary;
	bool Cleared;
	bool Is_ON;
	bool Bus2Defined;
	double On_Time;
	double RandomMult;
	bool FaultStillGoing(int ActorID);
protected:
	double G;         // single G per phase (line rating) if Gmatrix not specified
	Arraydef::pDoubleArray Gmatrix;  // If not nil then overrides G
	double StdDev;  // per unit stddev
	int SpecType;
public:
	TFaultObj(DSSClass::TDSSClass* ParClass, const String FaultName);
	virtual ~TFaultObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	void Randomize(int ActorID);
	void CheckStatus(int ControlMode, int ActorID);
	void Reset();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TFaultObj(DSSClass::TDSSClass* ParClass);
	TFaultObj(String ClassName);
	TFaultObj();
};
extern TFaultObj* ActiveFaultObj;


}  // namespace Fault

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Fault;
#endif

#endif // FaultH




