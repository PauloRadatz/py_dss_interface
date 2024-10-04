#ifndef CapacitorH
#define CapacitorH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "PDClass.h"
#include "PDElement.h"
#include "Ucmatrix.h"
#include "Arraydef.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "Ucomplex.h"



namespace Capacitor
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*   4-17-00  Made IsShunt Public
    12-7-04 Added Reactance in series with each capacitor

*/
/*Basic  capacitor

  Implemented as a two-terminal constant impedance (Power Delivery Element)

  Bus2 connection defaults to 0 node of Bus1 (if Bus2 has the default bus connection
  at the time Bus1 is defined.  Therefore, if only Bus1 is specified, a shunt capacitor results.
  If delta connected, Bus2 is set to node zero of Bus1 and nothing is returned in the lower
  half of YPrim - all zeroes.

  If an ungrounded wye is desired, explicitly set Bus2= and set all nodes the same,
    e.g. Bus1.4.4.4   (uses 4th node of Bus1 as neutral point)
        or BusNew.1.1.1  (makes a new bus for the neutral point)
  You must specify the nodes or you will get a series capacitor!

  A series capacitor is specified simply by setting bus2 and declaring the connection
  to be Wye.  If the connection is specified as delta, nothing will be connected to Bus2.
  In fact the number of terminals is set to 1.

  Capacitance may be specified as:

     1.  kvar and kv ratings at base frequency.  impedance.  Specify kvar as total for
         all phases (all cans assumed equal). For 1-phase, kV = capacitor can kV rating.
         For 2 or 3-phase, kV is line-line three phase. For more than 3 phases, specify
         kV as actual can voltage.
     2.  Capacitance in uF to be used in each phase.  If specified in this manner,
         the given value is always used whether wye or delta.
     3.  A nodal C matrix (like a nodal admittance matrix).
         If conn=wye then 2-terminal through device
         If conn=delta then 1-terminal.
         Microfarads.

*/

class TCapacitor : public PDClass::TPDClass
{
	friend class TCapacitorObj;
public:
	typedef PDClass::TPDClass inherited;	
private:
	void DoCmatrix(int ActorID);
	void InterpretConnection(const String s);
	void CapSetBus1(const String s);
protected:
	virtual int MakeLike(const String CapacitorName);
	void DefineProperties();  // Add Properties of this class to propName
public:
	TCapacitor();
	virtual ~TCapacitor();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TCapacitorObj : public PDELement::TPDElement
{
	friend class TCapacitor;
public:
	typedef PDELement::TPDElement inherited;	
//private:
	std::vector <double> FC;
	std::vector <double> FXL;
	std::vector <double> Fkvarrating;
	std::vector <double> FR;
	std::vector <double> FHarm;  // single C per phase (line rating) if Cmatrix not specified
	std::vector <longInt> FStates;
	double Ftotalkvar;
	double kvrating;
	int FNumSteps;
	int FLastStepInService;
	std::vector <double> Cmatrix;  // If not nil then overrides C
	bool DoHarmonicRecalc;
	bool Bus2Defined;
	bool FNormAmpsSpecified;
	bool FEmergAmpsSpecified;

	int SpecType;
	int NumTerm;   // Flag used to indicate The number of terminals
	int get_States(int Idx, int ActorID);
	void set_States(int Idx, int ActorID, int Value);
	void set_LastStepInService(int Value);
	void ProcessHarmonicSpec(const String Param);
	void ProcessStatesSpec(const String Param);
	void MakeYprimWork(Ucmatrix::TcMatrix* YprimWork, int iStep, int ActorID);
	void set_NumSteps(int Value); // 1=kvar, 2=Cuf, 3=Cmatrix
public:
	int Connection;   // 0 or 1 for wye (default) or delta, respectively
	TCapacitorObj(DSSClass::TDSSClass* ParClass, const String CapacitorName);
	virtual ~TCapacitorObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	bool AddStep(int ActorID);
	bool SubtractStep(int ActorID);
	int AvailableSteps();
	void FindLastStepInService();
	// properties for getting/setting variables.
	int Get_NumTerm();
	int Get_FLastStepInService();
	int Get_FNumSteps();
	double Get_kvrating();
	double Get_Ftotalkvar();

	TCapacitorObj(TDSSClass* ParClass);
	TCapacitorObj(String ClassName);
	TCapacitorObj();
};
extern TCapacitorObj* ActiveCapacitorObj;
extern TCapacitor* CapacitorClass;


}  // namespace Capacitor

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Capacitor;
#endif

#endif // CapacitorH




