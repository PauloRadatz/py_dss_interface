#ifndef GICTransformerH
#define GICTransformerH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "PDClass.h"
#include "PDElement.h"
#include "Ucmatrix.h"
#include "Arraydef.h"
#include "XYcurve.h"



namespace GICTransformer
{



/*
  ----------------------------------------------------------
  Copyright (c) 2011-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   6-21-2011 Created from Fault Object
*/

/*

   Special resistance-only model of transformers for geomagnetically-induced current (GIC) studies
*/

class TGICTransformer : public PDClass::TPDClass
{
	friend class TGICTransformerObj;
public:
	typedef PDClass::TPDClass inherited;	
private:
	void GICTransSetBusH(const String s);
	void GICTransSetBusX(const String s);
protected:
	void DefineProperties();
	virtual int MakeLike(const String GICTransName);
public:
	TGICTransformer();
	virtual ~TGICTransformer();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TGICTransformerObj : public PDELement::TPDElement
{
	friend class TGICTransformer;
public:
	typedef PDELement::TPDElement inherited;	
private:
	double G1;
	double G2;         // single G per phase (line rating)
	int SpecType;
	double FMVARating;
	String FVarCurve;
	XYCurve::TXYcurveObj* FVarCurveObj;
	double FpctR1;
	double FpctR2;
	double FZbase1;
	double FZbase2;
	bool FkVSpecified;
	bool FpctRSpecified;
	bool KSpecified;
	double FKFactor;
	double FkV1;
	double FkV2;
public:
	TGICTransformerObj(DSSClass::TDSSClass* ParClass, const String FaultName);
	virtual ~TGICTransformerObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	void WriteVarOutputRecord(System::TTextRec& f, int ActorID); // Add a record to the ouput file based on present GIC
	TGICTransformerObj(DSSClass::TDSSClass* ParClass);
	TGICTransformerObj(String ClassName);
	TGICTransformerObj();
};
extern TGICTransformerObj* ActiveGICTransformerObj;


}  // namespace GICTransformer

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace GICTransformer;
#endif

#endif // GICTransformerH




