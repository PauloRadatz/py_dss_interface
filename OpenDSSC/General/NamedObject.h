#ifndef NamedObjectH
#define NamedObjectH

#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"


namespace NamedObject
{


/*
  ----------------------------------------------------------
  Copyright (c) 2009-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
typedef GUID TUuid;    // this is a GUID compliant to RFC 4122, v4

class TNamedObject : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
public:
	std::string PName;  // path name, or class name for DSS
	std::string LName;  // localName is unique within a class, like the old FName
	std::string DName;  // for optional display, does not have to be unique
	TUuid* puuid;  // compliant to RFC 4122, v4
	std::string Get_QualifiedName();
	std::string Get_DisplayName();
	void Set_DisplayName(const std::string Value);
	const TUuid& Get_UUID();
	std::string Get_ID();
	std::string Get_CIM_ID();
	void Set_UUID(const TUuid& Value);
	std::string Get_myPName();
	void Set_myPName(std::string Value);
	std::string Get_myLName();
	void Set_myLName(std::string Value);
public:
	TNamedObject(std::string ClassName);
	virtual ~TNamedObject();
	TNamedObject();
};
int CreateUUID4(TUuid& UUID);
TUuid StringToUUID(std::string s);
std::string UUIDToString(const TUuid& UUID);
String UUIDToCIMString(const TUuid& cUUID);


}  // namespace NamedObject

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace NamedObject;
#endif

#endif // NamedObjectH





