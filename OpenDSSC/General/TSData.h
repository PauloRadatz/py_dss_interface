#ifndef TSDataH
#define TSDataH

#include "System.h"
//#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "CableData.h"
#include "ConductorData.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "LineUnits.h"
#include "d2c_structures.h"


namespace TSData
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TTSData : public TCableData
{
	friend class TTSDataObj;
public:
	typedef CableData::TCableData inherited;	
//private:
	String Get_Code();
	void Set_Code(const String Value);
protected:
	void DefineProperties();
	virtual int MakeLike(const String TSName);
public:
	TTSData();
	virtual ~TTSData();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);

       // Set this property to point ActiveTSDataObj to the right value
};

class TTSDataObj : public CableData::TCableDataObj
{
	friend class TTSData;
public:
	typedef CableData::TCableDataObj inherited;	
//private:
	double FDiaShield;
	double FTapeLayer;
	double FTapeLap;
public:

	double get_FDiaShield();
	double get_FTapeLayer();
	double get_FTapeLap();

	TTSDataObj(DSSClass::TDSSClass* ParClass, const String TSDataName);
	virtual ~TTSDataObj();

	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	virtual int GetNumProperties(int ArrayOffset);
	TTSDataObj(DSSClass::TDSSClass* ParClass);
	TTSDataObj(String ClassName);
	TTSDataObj();
};


}  // namespace TSData

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace TSData;
#endif

#endif // TSDataH





