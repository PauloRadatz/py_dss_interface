//#include <vcl.h>
#pragma hdrstop

#include "IniRegSave.h"

using namespace std;


namespace IniRegSave
{

TIniRegSave::TIniRegSave() {}



//{$IFDEF MSWINDOWS}

TIniRegSave::TIniRegSave(const String Name)
/* : FSection(L"MainSect"),
			FIniFile(nullptr)*/
{
/*	FName = Name;
	FIniFile = new TRegIniFile(Name);*/
}

TIniRegSave::~TIniRegSave()
{
	// inherited;
}


bool TIniRegSave::ReadBool(const String key, bool Default)
{
	bool result = false;
//	result = FIniFile->ReadBool(FSection, key, Default);
	return result;
}

int TIniRegSave::ReadInteger(const String key, int Default)
{
	int result = 0;
	//result = FIniFile->ReadInteger(FSection, key, Default);
	return result;
}

String TIniRegSave::ReadString(const String key, const String Default)
{
	String result;
	//result = FIniFile->ReadString(FSection, key, Default);
	return result;
}

void TIniRegSave::Set_FSection(const String Value)
{
	FSection = Value;
}

void TIniRegSave::WriteBool(const String key, bool Value)
{
//	FIniFile->WriteBool(FSection, key, Value);
}

void TIniRegSave::WriteInteger(const String key, int Value)
{
//	FIniFile->WriteInteger(FSection, key, Value);
}

void TIniRegSave::WriteString(const String key, String Value)
{
//	FIniFile->WriteString(FSection, key, Value);
}

string TIniRegSave::get_FSection()
{
	return FSection;
}

void TIniRegSave::ClearSection()
{
	//FIniFile->EraseSection(FSection);
}
//{$ENDIF}




}  // namespace IniRegSave




