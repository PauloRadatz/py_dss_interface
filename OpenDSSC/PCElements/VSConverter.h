#ifndef VSConverterH
#define VSConverterH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "XYcurve.h"
#include "ParserDel.h"
#include "MyDSSClassDefs.h"
#include "DSSClassDefs.h"
#include "Dynamics.h"
#include "mathutil.h"
#include <math.h>





namespace VSConverter
{



/*
  ----------------------------------------------------------
  Copyright (c) 2013-2022, University of Pittsburgh
  All rights reserved.
  ----------------------------------------------------------
*/

class TVSConverter : public PCClass::TPCClass
{
	friend class TVSConverterObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
	void VscSetBus1(const String s);
protected:
	void DefineProperties();
	virtual int MakeLike(const String VSCName);
public:
	TVSConverter();
	virtual ~TVSConverter();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TVSConverterObj : public PCElement::TPCElement
{
	friend class TVSConverter;
public:
	typedef PCElement::TPCElement inherited;	
//private:
	double FkVac;
	double FkVdc;
	double FkW;
	double FM;
	double FD;
	double FRac;
	double FXac;
	double FrefVac;
	double FrefVdc;
	double FrefPac;
	double FrefQac;
	double FMinM;
	double FMaxM;
	double FMaxIac;
	double FMaxIdc;
	int FMode;
	int FNdc;
	Ucomplex::pComplexArray LastCurrents; // state memory for GetInjCurrents
public:
	TVSConverterObj(DSSClass::TDSSClass* ParClass, const String FaultName);
	virtual ~TVSConverterObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);

      // these three functions make it a PCElement
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void MakePosSequence(int ActorID);
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TVSConverterObj(DSSClass::TDSSClass* ParClass);
	TVSConverterObj(String ClassName);
	TVSConverterObj();
};
extern TVSConverterObj* ActiveVSConverterObj;


}  // namespace VSConverter

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace VSConverter;
#endif

#endif // VSConverterH




