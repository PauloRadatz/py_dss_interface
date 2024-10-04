#ifndef IniRegSaveH
#define IniRegSaveH

#include "System.h"

//#include <System.Win.Registry.hpp>

namespace IniRegSave
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/* Simple unit to create an Ini file equivalent in the registry.
  By default, creates a key under HKEY_CURRENT_USER

  Typically, you will want to move the key under 'Software' by creating as

  MyIniFile := TIniRegSave.Create('\Software\myprogramname');

  But it'll work anywhere.

*/
//{$IFDEF MSWINDOWS}

class TIniRegSave : public System::TObject
{
public:
	typedef TObject inherited;	
	String FSection;
	String FName;
//	TIniFile* FIniFile;
//private:
	void Set_FSection(const String Value);
	std::string get_FSection();
    /* Private declarations */
public:
    /* Public declarations */
	void ClearSection();
	void WriteBool(const String key, bool Value);
	void WriteInteger(const String key, int Value);
	void WriteString(const String key, String Value);
	bool ReadBool(const String key, bool Default);
	int ReadInteger(const String key, int Default);
	String ReadString(const String key, const String Default);
	TIniRegSave(const String Name);
	virtual ~TIniRegSave();
	TIniRegSave();
};
//{$ENDIF}


}  // namespace IniRegSave

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace IniRegSave;
#endif

#endif // IniRegSaveH




