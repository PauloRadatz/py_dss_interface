#ifndef ControlClassH
#define ControlClassH

#include "System.h"
#include "Sysutils.h"

#include "CktElementClass.h"

#include "ControlElem.h"

namespace ControlClass
{


/*
   ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
   Base for control classes
*/

/*$M+*/

class TControlClass : public CktElementClass::TCktElementClass
{
public:
	typedef CktElementClass::TCktElementClass inherited;	
private:
protected:
	int ClassEdit(const void* ActiveControlObj, int ParamPointer);
	void ClassMakeLike(const void* OtherObj);
	void CountProperties();  // Add no. of intrinsic properties
	void DefineProperties();  // Add Properties of this class to propName
public:
	int NumControlClassProps;
	TControlClass();
	virtual ~TControlClass();
//__published:
};


}  // namespace ControlClass

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ControlClass;
#endif

#endif // ControlClassH





