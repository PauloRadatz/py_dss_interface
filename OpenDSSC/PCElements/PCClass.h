#ifndef PCClassH
#define PCClassH

#include "System.h"
#include "Sysutils.h"

#include "CktElementClass.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"


namespace PCClass
{



/*$M+*/
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TPCClass : public CktElementClass::TCktElementClass
{
public:
	typedef CktElementClass::TCktElementClass inherited;	
private:
protected:
	int ClassEdit(const void* ActivePCObj, int ParamPointer);
	void ClassMakeLike(const void* OtherObj);
	void CountProperties();  // Add no. of intrinsic properties
	void DefineProperties();  // Add Properties of this class to propName
public:
	int NumPCClassProps;
	TPCClass();
	virtual ~TPCClass();
//__published:
};


}  // namespace PCClass

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PCClass;
#endif

#endif // PCClassH




