
#pragma hdrstop

#include "StoreUserModel.h"

#include "DSSGlobals.h"

using namespace std;
using namespace Arraydef;
using namespace DSSGlobals;
using namespace Dynamics;
using namespace Storage;
using namespace System;
using namespace Ucomplex;

namespace StoreUserModel
{



/* TStoreUserModel */

void* TStoreUserModel::CheckFuncError(void* Addr, String FuncName)
{
	void* result = nullptr;
	if(Addr == nullptr)
	{
		DoSimpleMsg(String("Storage User Model DLL Does Not Have Required Function: ") + FuncName, 1569);
		FuncError = true;
	}
	result = Addr;
	return result;
}

TStoreUserModel::TStoreUserModel()
 : FHandle(0),
			FID(0),
			FuncError(false)
{
	FID = 0;
	FName = "";
}

TStoreUserModel::~TStoreUserModel()
{
	// inherited;
	if(FID != 0)
	{
		FDelete(FID);       // Clean up all memory associated with this instance
		FreeLibrary( (HMODULE) FHandle);
	}
}

//------------------------------------------------------------------------------

std::string TStoreUserModel::get_FName()
{
	return FName;
}

//------------------------------------------------------------------------------

std::string TStoreDynaModel::get_FName()
{
	return FName;
}

//------------------------------------------------------------------------------


bool TStoreUserModel::Get_Exists()
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

void TStoreUserModel::Integrate()
{
	FSelect(FID);
	FIntegrate();
}

void TStoreUserModel::select()
{
	FSelect(FID);
}

void TStoreUserModel::Set_Edit(const String Value)
{
	if(FID != 0)
		FEdit((PAnsiChar) Value.c_str(), Value.size());
        // Else Ignore
}

void TStoreUserModel::Set_Name(const String Value)
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
		FreeLibrary((HMODULE) FHandle);
	}

        /*If Value is blank or zero-length, bail out.*/
	if((Value.size() == 0) || (TrimLeft(Value).size() == 0))
		return;
	if(CompareText(Value, "none") == 0)
		return;
	FHandle = (NativeUInt) LoadLibrary(Value.c_str());      // Default LoadLibrary and PChar must agree in expected type
	if((HMODULE) FHandle == 0)
             // Try again with full path name
	{
		FHandle = (NativeUInt) LoadLibrary((DSSDirectory + Value).c_str());
	}
	if((HMODULE) FHandle == 0)
		DoSimpleMsg(String("Storage User Model ") + Value
	           + " Not Loaded. DSS Directory = "
	           + DSSDirectory, 1570);
	else
	{
		FName = Value;

            // Now set up all the procedure variables
		FuncError = false;
		FNew = (StoreUserModelProc__13) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("New")), "New");
		if(!FuncError)
			FSelect = (StoreUserModelProc__15) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Select")), "Select");
		if(!FuncError)
			FInit = (StoreUserModelProc__17) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Init")), "Init");
		if(!FuncError)
			FCalc = (StoreUserModelProc__18) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Calc")), "Calc");
		if(!FuncError)
			FIntegrate = (StoreUserModelProc__19) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Integrate")), "Integrate");
		if(!FuncError)
			FSave = (StoreUserModelProc__21) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Save")), "Save");
		if(!FuncError)
			fRestore = (StoreUserModelProc__22) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Restore")), "Restore");
		if(!FuncError)
			FEdit = (StoreUserModelProc__16) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Edit")), "Edit");
		if(!FuncError)
			FUpdateModel = (StoreUserModelProc__20) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("UpdateModel")), "UpdateModel");
		if(!FuncError)
			FDelete = (StoreUserModelProc__14) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Delete")), "Delete");
		if(!FuncError)
			FNumVars = (StoreUserModelProc__23) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("NumVars")), "NumVars");
		if(!FuncError)
			FGetAllVars = (StoreUserModelProc__24) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetAllVars")), "GetAllVars");
		if(!FuncError)
			FGetVariable = (StoreUserModelProc__25) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVariable")), "GetVariable");
		if(!FuncError)
			FSetVariable = (StoreUserModelProc__26) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("SetVariable")), "SetVariable");
		if(!FuncError)
			FGetVarName = (StoreUserModelProc__27) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVarName")), "GetVarName");
		if(FuncError)
		{
			FreeLibrary((HMODULE) FHandle);
			FID = 0;
			FHandle = 0;
			FName = "";
		}
		else
		{
			FID = FNew(ActiveCircuit[ActiveActor]->Solution->DynaVars, CallBackRoutines);  // Create new instance of user model
		}
	}
}

/*============================= TSTOREDYNAMODEL ================================================================*/

/* TStoreDynaModel */

void* TStoreDynaModel::CheckFuncError(void* Addr, String FuncName)
{
	void* result = nullptr;
	if(Addr == nullptr)
	{
		DoSimpleMsg(String("Storage User Dynamic DLL Does Not Have Required Function: ") + FuncName, 1569);
		FuncError = true;
	}
	result = Addr;
	return result;
}

TStoreDynaModel::TStoreDynaModel()
 : FHandle(0),
			FID(0),
			FuncError(false)
{
	FID = 0;
	FName = "";
}

TStoreDynaModel::~TStoreDynaModel()
{
	if(FID != 0)
	{
		FDelete(FID);       // Clean up all memory associated with this instance
		FreeLibrary((HMODULE) FHandle);
	}
	// inherited;
}


bool TStoreDynaModel::Get_Exists()
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

void TStoreDynaModel::Integrate()
{
	FSelect(FID);
	FIntegrate();
}

void TStoreDynaModel::select()
{
	FSelect(FID);
}

void TStoreDynaModel::Set_Edit(const String Value)
{
	if(FID != 0)
		FEdit((PAnsiChar) Value.c_str(), Value.size());
}

void TStoreDynaModel::Set_Name(const String Value)
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
		FreeLibrary((HMODULE) FHandle);
	}

        /*If Value is blank or zero-length, bail out.*/
	if((Value.size() == 0) || (TrimLeft(Value).size() == 0))
		return;
	if(CompareText(Value, "none") == 0)
		return;
	FHandle = (NativeUInt) LoadLibrary(Value.c_str());      // Default LoadLibrary and PChar must agree in expected type
	if((HMODULE) FHandle == 0)
             // Try again with full path name
	{
		FHandle = (NativeUInt) LoadLibrary((DSSDirectory + Value).c_str());
	}
	if((HMODULE) FHandle == 0)
		DoSimpleMsg(String("Storage User-written Dynamics Model ") + Value
	           + " Not Loaded. DSS Directory = "
	           + DSSDirectory, 1570);
	else
	{
		FName = Value;

            // Now set up all the procedure variables
		FuncError = false;
		FNew = (StoreUserModelProc__0) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("New")), "New");
		if(!FuncError)
			FSelect = (StoreUserModelProc__2) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Select")), "Select");
		if(!FuncError)
			FInit = (StoreUserModelProc__4) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Init")), "Init");
		if(!FuncError)
			FCalc = (StoreUserModelProc__5) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Calc")), "Calc");
		if(!FuncError)
			FIntegrate = (StoreUserModelProc__6) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Integrate")), "Integrate");
		if(!FuncError)
			FEdit = (StoreUserModelProc__3) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Edit")), "Edit");
		if(!FuncError)
			FUpdateModel = (StoreUserModelProc__7) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("UpdateModel")), "UpdateModel");
		if(!FuncError)
			FDelete = (StoreUserModelProc__1) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Delete")), "Delete");
		if(!FuncError)
			FNumVars = (StoreUserModelProc__8) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("NumVars")), "NumVars");
		if(!FuncError)
			FGetAllVars = (StoreUserModelProc__9) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetAllVars")), "GetAllVars");
		if(!FuncError)
			FGetVariable = (StoreUserModelProc__10) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVariable")), "GetVariable");
		if(!FuncError)
			FSetVariable = (StoreUserModelProc__11) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("SetVariable")), "SetVariable");
		if(!FuncError)
			FGetVarName = (StoreUserModelProc__12) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVarName")), "GetVarName");
		if(FuncError)
		{
			FreeLibrary((HMODULE) FHandle);
			FID = 0;
			FHandle = 0;
			FName = "";
		}
		else
		{
			FID = FNew(ActiveCircuit[ActiveActor]->Solution->DynaVars, CallBackRoutines);  // Create new instance of user model
		}
	}
}




}  // namespace StoreUserModel




