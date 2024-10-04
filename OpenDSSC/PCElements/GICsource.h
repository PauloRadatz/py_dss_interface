#ifndef GICsourceH
#define GICsourceH

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Line.h"
#include "d2c_structures.h"



namespace GICsource
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Develpoed from Isource and GICLine May 2018
*/
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TGICsource : public PCClass::TPCClass
{
	friend class TGICSourceObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherSource);
public:
	TGICsource();
	virtual ~TGICsource();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TGICSourceObj : public PCElement::TPCElement
{
	friend class TGICsource;
public:
	typedef PCElement::TPCElement inherited;	
private:
	double FphaseShift;
	bool Bus2Defined;
	double Vmag;
	double Angle;
	double SrcFrequency;
	String LineName;
	Line::TLineObj* pLineElem;  // Pointer to associated Line
	double Vn;
	double VE;  // components of vmag
	Line::TLine* LineClass;
	void GetVterminalForSource(int ActorID);
	double Compute_VLine();
public:
	double ENorth;
	double EEast;
	double Lat1;
	double Lon1;
	double Lat2;
	double Lon2;
	double Volts;
	bool VoltsSpecified;
	TGICSourceObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TGICSourceObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	TGICSourceObj(DSSClass::TDSSClass* ParClass);
	TGICSourceObj(String ClassName);
	TGICSourceObj();
};
extern TGICSourceObj* ActiveGICsourceObj;
extern TGICsource* GICsourceClass;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace GICsource

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace GICsource;
#endif

#endif // GICsourceH




