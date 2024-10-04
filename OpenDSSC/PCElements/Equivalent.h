#ifndef EquivalentH
#define EquivalentH

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Spectrum.h"
#include "Arraydef.h"
#include "d2c_structures.h"

namespace Equivalent
{


 /*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Multi terminal, multi-phase Short Circuit (Thevinen) Equivalent

  Enter positive and zero short circuit impedance matrices
  And Voltage behind the equivalent
*/
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TEquivalent : public PCClass::TPCClass
{
	friend class TEquivalentObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherSource);
	void InterpretAllBuses(const String s);
public:
	TEquivalent();
	virtual ~TEquivalent();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TEquivalentObj : public PCElement::TPCElement
{
	friend class TEquivalent;
public:
	typedef PCElement::TPCElement inherited;	
//private:
	double kVBase;
	double Vmag;
	double PerUnit;
	double Angle;
	double EquivFrequency;
	Arraydef::pDoubleArray R1;
	Arraydef::pDoubleArray X1;
	Arraydef::pDoubleArray R0;
	Arraydef::pDoubleArray X0;
	bool NeedToDoRecalc;
	void GetVterminalForSource();
	void ReallocRX();
	void ParseDblMatrix(Arraydef::pDoubleArray Mat);
	int DoTerminalsDef(int n);
public:
	Ucmatrix::TcMatrix* Z;  // Base Frequency Series Z matrix
	Ucmatrix::TcMatrix* Zinv;
	TEquivalentObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TEquivalentObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	TEquivalentObj(DSSClass::TDSSClass* ParClass);
	TEquivalentObj(String ClassName);
	TEquivalentObj();
};
extern TEquivalentObj* ActiveEquivalentObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace Equivalent

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Equivalent;
#endif

#endif // EquivalentH




