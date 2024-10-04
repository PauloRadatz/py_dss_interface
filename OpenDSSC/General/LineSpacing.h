#ifndef LineSpacingH
#define LineSpacingH

#include "System.h"
#include "Sysutils.h"

#include "Arraydef.h"
#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "d2c_structures.h"

namespace LineSpacing
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
enum SpcParmChoice {X,
                    h };

class TLineSpacing : public TDSSClass
{
	friend class TLineSpacingObj;
public:
	typedef TDSSClass inherited;	
//private:
	String Get_Code();  // Returns active line code String
	void Set_Code(const String Value);  // sets the  active LineSpacing
	void InterpretArray(const String s, SpcParmChoice Which);
protected:
	void DefineProperties();
	virtual int MakeLike(const String LineName);
public:
	TLineSpacing();
	virtual ~TLineSpacing();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TLineSpacingObj : public DSSObject::TDSSObject
{
	friend class TLineSpacing;
public:
	typedef DSSObject::TDSSObject inherited;	
//private:
	int Fnconds;
	int Fnphases;
	Arraydef::pDoubleArray FX;
	Arraydef::pDoubleArray FY;
	int FUnits;
	bool DataChanged;
	void set_Nwires(int Value);

        // CIM Accessors
	double Get_FX(int i);
	double Get_FY(int i);
public:
	TLineSpacingObj(DSSClass::TDSSClass* ParClass, const String LineSpacingName);
	virtual ~TLineSpacingObj();
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

	int get_Fnconds();
	int get_Fnphases();
	int get_FUnits();

        // CIM XML accessors
	TLineSpacingObj(DSSClass::TDSSClass* ParClass);
	TLineSpacingObj(String ClassName);
	TLineSpacingObj();
};
extern TLineSpacingObj* ActiveLineSpacingObj;


}  // namespace LineSpacing

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace LineSpacing;
#endif

#endif // LineSpacingH





