#pragma hdrstop

#include "NamedObject.h"

#ifndef windows
#include <uuid/uuid.h> // uuid_generate
#include <cstring> // std::memcpy
#endif




using namespace std;
using namespace System;

namespace NamedObject
{

	TNamedObject::TNamedObject() {}

	string TNamedObject::Get_myPName()
	{
		return PName;
	}


	void TNamedObject::Set_myPName(string Value)
	{
		PName = Value;
	}

	string TNamedObject::Get_myLName()
	{
		return LName;
	}

	void TNamedObject::Set_myLName(std::string Value)
	{
		LName = Value;
	}



	int CreateUUID4(TUuid& UUID)
	{
#ifdef windows
		int result = 0;
		result = CoCreateGuid(&UUID);
#else
		// Note: This code looks peculiar because uuid_t is not a
		// struct.  It is instead a typedef to unsigned char [16].
		uuid_t _UUID;
		uuid_generate(_UUID); // This function returns void.
		static_assert(sizeof(_UUID) == sizeof(UUID),"Wrong _UUID!");
		std::memcpy(&UUID, &_UUID[0], sizeof(_UUID));

		// CoCreateGuid returns S_OK to indicate success, which on MS
		// Windows, appears to be 0.  Too bad no one checks our return
		// value...
		int result = 0; // emulate S_OK
#endif
		UUID.Data3 = (WORD)((UUID.Data3 & 0x0FFF) | 0x4000);   // place a 4 at character 13
		UUID.Data4[0] = (unsigned char)((UUID.Data4[0] & 0x3F) | 0x80); // character 17 to be 8, 9, A or B
		return result;
	}

	TUuid StringToUUID(string s)
	{
		TUuid result = {};
		result = StringToGuid(s);
		return result;
	}

	String UUIDToString(const TUuid& UUID)
	{
		String result;
		result = GuidToString(UUID);
		return result;
	}

	String UUIDToCIMString(const TUuid& cUUID)
	{
		TUuid UUID = cUUID;
		String result;
		String s;
		s = GuidToString(UUID);
		result = s.substr(1, s.length() - 2);
		return result;
	}

	TNamedObject::TNamedObject(string ClassName)
		: PName(ClassName),
		LName(""),
		DName(""),
		puuid(nullptr)
	{
		;
	}

	TNamedObject::~TNamedObject()
	{
	//	if (ASSIGNED(puuid))
	//		delete puuid;
		// inherited::Destroy();
	}


	void TNamedObject::Set_DisplayName(const string Value)
	{
		DName = Value;
	}

	String TNamedObject::Get_DisplayName()
	{
		String result;
		if (DName == "")
			result = PName + "_" + LName;
		else
			result = DName;
		return result;
	}

	String TNamedObject::Get_QualifiedName()
	{
		String result;
		result = PName + "." + LName;
		return result;
	}

	void TNamedObject::Set_UUID(const TUuid& Value)
	{
		if (puuid == nullptr)
			puuid = new GUID;
		(*puuid) = Value;
	}

	const TUuid& TNamedObject::Get_UUID()
	{
		if (puuid == nullptr)
		{
			puuid = new TUuid;
			CreateUUID4(*puuid);
		}
		return *puuid;
	}

	String TNamedObject::Get_ID()
	{
		String result;
		result = GuidToString(Get_UUID());
		return result;
	}

	String TNamedObject::Get_CIM_ID()
	{
		String result;
		result = UUIDToCIMString(Get_UUID());
		return result;
	}
}










