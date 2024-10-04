#ifndef CNDataH
#define CNDataH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "ConductorData.h"
#include "CableData.h"


namespace CNData
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TCNData : public CableData::TCableData
{
	friend class TCNDataObj;
public:
	typedef CableData::TCableData inherited;	
//private:
	String Get_Code();  // Returns active line code String
	void Set_Code(const String Value);  // sets the  active CNData
//protected:
	void DefineProperties();
	virtual int MakeLike(const String CNName);
public:
	TCNData();
	virtual ~TCNData();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TCNDataObj : public CableData::TCableDataObj
{
	friend class TCNData;
public:
	typedef CableData::TCableDataObj inherited;	
//private:
	int FkStrand;
	double FDiaStrand;
	double FGmrStrand;
	double FRStrand;
public:

	int get_FkStrand();
	double get_FDiaStrand();
	double get_FGmrStrand();
	double get_FRStrand();

	TCNDataObj(DSSClass::TDSSClass* ParClass, const String CNDataName);
	virtual ~TCNDataObj();
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
    virtual String GetPropertyValue(int Index);
	virtual int GetNumProperties(int ArrayOffset);
	TCNDataObj(DSSClass::TDSSClass* ParClass);
	TCNDataObj(String ClassName);
	TCNDataObj();
};


}  // namespace CNData

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CNData;
#endif

#endif // CNDataH





