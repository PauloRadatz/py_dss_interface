
#pragma hdrstop

#include "PVSystemUserModel.h"

#include "Ucomplex.h"
#include "Arraydef.h"
#include "PVsystem.h"
#include "DSSGlobals.h"

using namespace std;
using namespace Arraydef;
using namespace DSSGlobals;
using namespace Dynamics;
using namespace PVSystem;
using namespace System;
using namespace Ucomplex;

namespace PVsystemUserModel
{



/* TPVsystemUserModel */

void* TPVsystemUserModel::CheckFuncError(void* Addr, String FuncName)
{
	void* result = nullptr;
	if(Addr == nullptr)
	{
		DoSimpleMsg(String("PVSystem User Model Does Not Have Required Function: ") + FuncName, 1569);
		FuncError = true;
	}
	result = Addr;
	return result;
}

TPVsystemUserModel::TPVsystemUserModel()
 : FHandle(0),
			FID(0),
			FuncError(false)
{
	FID = 0;
	FName = "";
}

//------------------------------------------------------------------------------------

String TPVsystemUserModel::get_FName()
{
	return FName;
}

//------------------------------------------------------------------------------------

TPVsystemUserModel::~TPVsystemUserModel()
{
	if(FID != 0)
	{
		FDelete(FID);       // Clean up all memory associated with this instance
		FreeLibrary((HMODULE) FHandle);
	}
	// inherited;
}


bool TPVsystemUserModel::Get_Exists()
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

void TPVsystemUserModel::Integrate()
{
	FSelect(FID);
	FIntegrate();
}

void TPVsystemUserModel::select()
{
	FSelect(FID);
}

void TPVsystemUserModel::Set_Edit(const String Value)
{
	if(FID != 0)
		FEdit((PAnsiChar) Value.c_str(), Value.size());
        // Else Ignore
}

void TPVsystemUserModel::Set_Name(const String Value)
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
		FHandle = (NativeUInt) LoadLibrary( (DSSDirectory + Value).c_str() );
	}
	if((HMODULE) FHandle == 0)
		DoSimpleMsg(String("PVSystem User Model ") + Value
	           + " Not Loaded. DSS Directory = "
	           + DSSDirectory, 1570);
	else
	{
		FName = Value;

            // Now set up all the procedure variables
		FuncError = false;
		FNew = (PVsystemUserModelProc__0) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("New")), "New");
		if(!FuncError)
			FSelect = (PVsystemUserModelProc__2) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Select")), "Select");
		if(!FuncError)
			FInit = (PVsystemUserModelProc__4) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Init")), "Init");
		if(!FuncError)
			FCalc = (PVsystemUserModelProc__5) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Calc")), "Calc");
		if(!FuncError)
			FIntegrate = (PVsystemUserModelProc__6) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Integrate")), "Integrate");
		if(!FuncError)
			FSave = (PVsystemUserModelProc__8) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Save")), "Save");
		if(!FuncError)
			fRestore = (PVsystemUserModelProc__9) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Restore")), "Restore");
		if(!FuncError)
			FEdit = (PVsystemUserModelProc__3) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Edit")), "Edit");
		if(!FuncError)
			FUpdateModel = (PVsystemUserModelProc__7) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("UpdateModel")), "UpdateModel");
		if(!FuncError)
			FDelete = (PVsystemUserModelProc__1) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Delete")), "Delete");
		if(!FuncError)
			FNumVars = (PVsystemUserModelProc__10) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("NumVars")), "NumVars");
		if(!FuncError)
			FGetAllVars = (PVsystemUserModelProc__11) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetAllVars")), "GetAllVars");
		if(!FuncError)
			FGetVariable = (PVsystemUserModelProc__12) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVariable")), "GetVariable");
		if(!FuncError)
			FSetVariable = (PVsystemUserModelProc__13) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("SetVariable")), "SetVariable");
		if(!FuncError)
			FGetVarName = (PVsystemUserModelProc__14) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVarName")), "GetVarName");
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




}  // namespace PVsystemUserModel




