#ifndef ISourceH
#define ISourceH

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Spectrum.h"
#include "LoadShape.h"
#include "d2c_structures.h"



namespace ISource
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*  Ideal current source

   Stick'em on wherever you want as many as you want

   ISource maintains a positive sequence for harmonic scans.  If you want zero sequence,
   use three single-phase ISource.


 10-25-00  Created from Vsource
 5-17-02  Moved spectrum to base class
 2-19-03 Added Phaseshift variable for n-phase elements

*/
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TIsource : public PCClass::TPCClass
{
	friend class TIsourceObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
	void IsourceSetBus1(const String s);
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherSource);
public:
	TIsource();
	virtual ~TIsource();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TIsourceObj : public PCElement::TPCElement
{
	friend class TIsource;
public:
	typedef PCElement::TPCElement inherited;	
private:
	double FphaseShift;
	bool ShapeIsActual;
	Ucomplex::complex ShapeFactor;
	bool Bus2Defined;
	Ucomplex::complex GetBaseCurr(int ActorID);
	void CalcDailyMult(double hr);
	void CalcDutyMult(double hr);
	void CalcYearlyMult(double hr);
public:
	double Amps;
	double Angle;
	double SrcFrequency;
	int ScanType;
	int SequenceType;
	double PerUnit;
	String DailyShape;         // Daily (24 HR) load shape
	LoadShape::TLoadShapeObj* DailyShapeObj;  // Daily load Shape FOR this load
	String DutyShape;         // Duty cycle load shape FOR changes typically less than one hour
	LoadShape::TLoadShapeObj* DutyShapeObj;  // Shape for this load
	String YearlyShape;  // ='fixed' means no variation  exempt from variation
	LoadShape::TLoadShapeObj* YearlyShapeObj;  // Shape for this load
	TIsourceObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TIsourceObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TIsourceObj(DSSClass::TDSSClass* ParClass);
	TIsourceObj(String ClassName);
	TIsourceObj();
};
extern TIsourceObj* ActiveIsourceObj;
    /*IsourceClass:TISource;*/

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace ISource

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ISource;
#endif

#endif // ISourceH




