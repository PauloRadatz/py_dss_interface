#ifndef VSourceH
#define VSourceH

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Spectrum.h"
#include "LoadShape.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "Dynamics.h"
#include "Command.h"




namespace VSource
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
 2-17-00  Change Log
          Added Angle to VMag calculation

 6-18-00  Added ability to do specify impedance in ohms or short circuit current
 5-17-01 Moved Spectrum to Base class
 2-10-09 Converted to 2-terminal voltage source

*/
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TVsource : public PCClass::TPCClass
{
	friend class TVsourceObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
	void VsourceSetBus1(const String s);
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherSource);
public:
	TVsource();
	virtual ~TVsource();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TVsourceObj : public TPCElement
{
	friend class TVsource;
public:
	typedef PCElement::TPCElement inherited;	
//private:
	double MVAsc3;
	double MVAsc1;
	double Isc3;
	double Isc1;
	int ZSpecType;
	double R1;
	double X1;  // Pos Seq Z
	double R2;
	double X2;  // Neg Seq Z
	double R0;
	double X0;  // Zero Seq Z
	double X1R1;
	double X0R0;
	double BaseMVA;
	Ucomplex::complex puZideal;
	Ucomplex::complex puZ1;
	Ucomplex::complex puZ0;
	Ucomplex::complex puZ2;
	double ZBase;
	bool Bus2Defined;
	bool Z1Specified;
	bool puZ1Specified;
	bool puZ0Specified;
	bool puZ2Specified;
	bool Z2Specified;
	bool Z0Specified;
	bool IsQuasiIdeal;  // Use puZideal for power flow
	int ScanType;
	int SequenceType;
	Ucomplex::complex ShapeFactor;
	bool ShapeIsActual;
	void GetVterminalForSource(int ActorID);
	void CalcDailyMult(double hr);
	void CalcDutyMult(double hr);
	void CalcYearlyMult(double hr);
	bool InterpretSourceModel(const String s);
public:
	Ucmatrix::TcMatrix* Z;  // Base Frequency Series Z matrix
	Ucmatrix::TcMatrix* Zinv;
	double Vmag;
	double kVBase;
	double PerUnit;
	double Angle;
	double SrcFrequency;
	String DailyShape;         // Daily (24 HR) load shape
	LoadShape::TLoadShapeObj* DailyShapeObj;  // Daily load Shape FOR this load
	String DutyShape;         // Duty cycle load shape FOR changes typically less than one hour
	LoadShape::TLoadShapeObj* DutyShapeObj;  // Shape for this load
	String YearlyShape;  // ='fixed' means no variation  exempt from variation
	LoadShape::TLoadShapeObj* YearlyShapeObj;  // Shape for this load
	TVsourceObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TVsourceObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual int InjCurrents(int ActorID);
	void CalcInjCurrAtBus(pComplexArray Curr, int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	TVsourceObj(DSSClass::TDSSClass* ParClass);
	TVsourceObj(String ClassName);
	TVsourceObj();
};
extern TVsourceObj* ActiveVsourceObj;
/*    VSourceClass    : TVsource;*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace VSource

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace VSource;
#endif

#endif // VSourceH




