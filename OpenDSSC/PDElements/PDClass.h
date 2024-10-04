#ifndef PDClassH
#define PDClassH

#include "System.h"
#include "CktElementClass.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"

namespace PDClass
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*$M+*/

class TPDClass : public CktElementClass::TCktElementClass
{
public:
	typedef CktElementClass::TCktElementClass inherited;	
private:
protected:
	int ClassEdit(const void* ActivePDObj, int ParamPointer);
	void ClassMakeLike(const void* OtherObj);
	void CountProperties();  // Add no. of intrinsic properties
	void DefineProperties();  // Add Properties of this class to propName
public:
	int NumPDClassProps;
	TPDClass();
	virtual ~TPDClass();
//__published:
};


}  // namespace PDClass

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PDClass;
#endif

#endif // PDClassH




