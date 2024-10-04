#ifndef CableDataH
#define CableDataH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "ConductorData.h"


namespace CableData
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

const int NumCableClassProps = 4;

class TCableData : public TConductorData
{
	friend class TCableDataObj;
public:
	typedef ConductorData::TConductorData inherited;	
private:
protected:
	void CountProperties();
	void DefineProperties();
	int ClassEdit(const void* activeObj, int ParamPointer);
	void ClassMakeLike(const void* OtherObj);
public:
	TCableData();
	virtual ~TCableData();
};

class TCableDataObj : public ConductorData::TConductorDataObj
{
	friend class TCableData;
public:
	typedef ConductorData::TConductorDataObj inherited;	
//private:
	double FEpsR;
        // next 3 use parent RadiusUnits
	double FInsLayer;
	double FDiaIns;
	double FDiaCable;
public:
	double get_FEpsR();
	double get_FDiaIns();
	double get_FDiaCable();
	double get_FInsLayer();

	TCableDataObj(DSSClass::TDSSClass* ParClass, const String CableDataName);
	virtual ~TCableDataObj();
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	virtual int GetNumProperties(int ArrayOffset);
	TCableDataObj(DSSClass::TDSSClass* ParClass);
	TCableDataObj(String ClassName);
	TCableDataObj();
};


}  // namespace CableData

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CableData;
#endif

#endif // CableDataH





