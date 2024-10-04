#ifndef WireDataH
#define WireDataH

#include "System.h"
#include "Sysutils.h"
#include "Command.h"
#include "ConductorData.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "LineUnits.h"


namespace WireData
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Used for overhead line impedances.
*/

class TWireData : public TConductorData
{
	friend class TWireDataObj;
public:
	typedef TConductorData inherited;	
//private:
	String Get_Code();  // Returns active line code String
	void Set_Code(const String Value);  // sets the  active WireData
protected:
	void DefineProperties();
	virtual int MakeLike(const String WireName);
public:
	TWireData();
	virtual ~TWireData();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);

      // Set this property to point ActiveWireDataObj to the right value
};

class TWireDataObj : public ConductorData::TConductorDataObj
{
	friend class TWireData;
public:
	typedef ConductorData::TConductorDataObj inherited;	
	TWireDataObj(DSSClass::TDSSClass* ParClass, const String WireDataName);
	virtual ~TWireDataObj();
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	virtual int GetNumProperties(int ArrayOffset);
	TWireDataObj(DSSClass::TDSSClass* ParClass);
	TWireDataObj(String ClassName);
	TWireDataObj();
};


}  // namespace WireData

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace WireData;
#endif

#endif // WireDataH





