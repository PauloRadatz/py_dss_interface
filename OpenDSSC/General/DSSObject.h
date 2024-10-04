#ifndef DSSObjectH
#define DSSObjectH

#include "System.h"

#include "Arraydef.h"
#include "NamedObject.h"
#include "DSSClass.h"
#include "Sysutils.h"


namespace DSSObject
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TDSSObject : public TNamedObject
{
public:
	typedef TNamedObject inherited;	
//private:
	std::string Get_PropertyValue(int Index);
	void Set_PropertyValue(int Index, const String Value);
	std::string get_Name();
	void Set_Name(const String Value);
//protected:
	int PropSeqCount;
	std::vector <std::string> FPropertyValue;
	std::vector <int> PrpSequence;
	int GetNextPropertySet(int Idx);
public:
	int DSSObjType; // PD, PC, Monitor, CondCode, etc.
	DSSClass::TDSSClass* ParentClass;
	int ClassIndex;    // Index into the class collection list
	bool HasBeenSaved;
	bool Flag;  // General purpose Flag for each object  don't assume inited
	TDSSObject(DSSClass::TDSSClass* ParClass);
	virtual ~TDSSObject();
	int Edit(int ActorID);  // Allow Calls to edit from object itself
	
      /*Get actual values of properties*/
	virtual std::string GetPropertyValue(int Index);  // Use dssclass.propertyindex to get index by name
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual void SaveWrite(System::TTextRec& f);
	void ClearPropSeqArray();
	TDSSObject(DSSClass::TDSSClass* ParClass, std::string ClassName);
	TDSSObject(std::string ClassName);
	TDSSObject();
};


}  // namespace DSSObject

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace DSSObject;
#endif

#endif // DSSObjectH





