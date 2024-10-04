
#pragma hdrstop

#include "WindGenUserModel.h"

#include "DSSGlobals.h"

using namespace std;
using namespace Arraydef;
using namespace DSSGlobals;
using namespace Dynamics;
using namespace System;
using namespace Ucomplex;
//using namespace WindGen;
using namespace WindGenVars;

namespace WindGenUserModel
{

TWindGenUserModel::TWindGenUserModel() {}



/* TGenUserModel */

void* TWindGenUserModel::CheckFuncError(void* Addr, String FuncName)
{
	void* result = nullptr;
	if(Addr == nullptr)
	{
		DoSimpleMsg(String("WindGen User Model Does Not Have Required Function: ") + FuncName, 569);
		FuncError = true;
	}
	result = Addr;
	return result;
}

TWindGenUserModel::TWindGenUserModel(pTWindGenVars ActiveWindGenVars)
 : FHandle(0),
			FID(0),
			FuncError(false),
			FActiveWindGenVars(nullptr)
{
	FID = 0;
	FName = "";
	FActiveWindGenVars = ActiveWindGenVars;
}

TWindGenUserModel::~TWindGenUserModel()
{
	if(FID != 0)
	{
		FDelete(FID);       // Clean up all memory associated with this instance
		//free(&FHandle);
	}
	// inherited;
}


bool TWindGenUserModel::Get_Exists()
{
	bool result = false;
	if(FID != 0)
	{
		result = true;
		select();    /*Automatically select if true*/
	}
	else
	result = false;
	return result;
}

void TWindGenUserModel::Integrate()
{
	FSelect(FID);
	FIntegrate();
}

void TWindGenUserModel::select()
{
	FSelect(FID);
}

void TWindGenUserModel::Set_Edit(const String Value)
{
	if(FID != 0)
		FEdit((System::PAnsiChar) Value.c_str(), Value.size());
        // Else Ignore
}

//----------------------------------------------------------------

String TWindGenUserModel::get_FName()
{
	return FName;
}

//----------------------------------------------------------------

void TWindGenUserModel::Set_Name(const String Value)
{


    /*If Model already points to something, then free it*/
	if((HMODULE) FHandle != 0)
	{
		if(FID != 0)
		{
			FDelete(FID);
			FName = "";
			FID = 0;
		}
		//free(&FHandle);
	}

        /*If Value is blank or zero-length, bail out.*/
	if((Value.size() == 0) || (TrimLeft(Value).size() == 0))
		return;
	if(CompareText(Value, "none") == 0)
		return;
	FHandle = (NativeUInt) LoadLibrary(Value.c_str());   // Default LoadLibrary and PChar must agree in expected type
	if((HMODULE) FHandle == 0) // Try again with full path name
	{
		FHandle = (NativeUInt) LoadLibrary((DSSDirectory + Value).c_str());
	}
	if((HMODULE) FHandle == 0)
		DoSimpleMsg(String("WindGen User Model ") + Value
	           + " Not Loaded. DSS Directory = "
	           + DSSDirectory, 570);
	else
	{
		FName = Value;

            // Now set up all the procedure variables			
		FuncError = false;
		FNew = (WindGenUserModelProc__0) CheckFuncError((void *) GetProcAddress( (HMODULE) FHandle, "New"), "New");
		if(!FuncError)
			FSelect = (WindGenUserModelProc__2) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, "Select" ), "Select");
		if(!FuncError)
			FInit = (WindGenUserModelProc__4) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, "Init"), "Init");
		if(!FuncError)
			FCalc = (WindGenUserModelProc__5) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Calc")), "Calc");
		if(!FuncError)
			FIntegrate = (WindGenUserModelProc__6) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Integrate")), "Integrate");
		if(!FuncError)
			FSave = (WindGenUserModelProc__8) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Save")), "Save");
		if(!FuncError)
			fRestore = (WindGenUserModelProc__9) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Restore")), "Restore");
		if(!FuncError)
			FEdit = (WindGenUserModelProc__3) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Edit")), "Edit");
		if(!FuncError)
			FUpdateModel = (WindGenUserModelProc__7) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("UpdateModel")), "UpdateModel");
		if(!FuncError)
			FDelete = (WindGenUserModelProc__1) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Delete")), "Delete");
		if(!FuncError)
			FNumVars = (WindGenUserModelProc__10) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("NumVars")), "NumVars");
		if(!FuncError)
			FGetAllVars = (WindGenUserModelProc__11) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetAllVars")), "GetAllVars");
		if(!FuncError)
			FGetVariable = (WindGenUserModelProc__12) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVariable")), "GetVariable");
		if(!FuncError)
			FSetVariable = (WindGenUserModelProc__13) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("SetVariable")), "SetVariable");
		if(!FuncError)
			FGetVarName = (WindGenUserModelProc__14) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVarName")), "GetVarName");
		if(FuncError)
		{
			FreeLibrary((HMODULE) FHandle);
			FID = 0;
			FHandle = 0;
			FName = "";
		}
		else
		{
			FID = FNew((*FActiveWindGenVars), ActiveCircuit[ActiveActor]->Solution->DynaVars, CallBackRoutines);  // Create new instance of user model
		}
	}
}




}  // namespace WindGenUserModel




