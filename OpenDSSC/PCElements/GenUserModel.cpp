
#pragma hdrstop

#include "GenUserModel.h"

#include "generator.h"
#include "DSSGlobals.h"

using namespace std;
using namespace Arraydef;
using namespace DSSGlobals;
using namespace Dynamics;
using namespace Generator;
using namespace GeneratorVars;
using namespace System;
using namespace Ucomplex;

namespace GenUserModel
{

TGenUserModel::TGenUserModel() {}



/* TGenUserModel */

void* TGenUserModel::CheckFuncError(void* Addr, String FuncName)
{
	void* result = nullptr;
	if(Addr == nullptr)
	{
		DoSimpleMsg(String("Generator User Model Does Not Have Required Function: ") + FuncName, 569);
		FuncError = true;
	}
	result = Addr;
	return result;
}

TGenUserModel::TGenUserModel(pTGeneratorVars ActiveGeneratorVars)
 : FHandle(0),
			FID(0),
			FuncError(false),
			FActiveGeneratorVars(nullptr)
{
	FID = 0;
	FName = "";
	FActiveGeneratorVars = ActiveGeneratorVars;
}

TGenUserModel::~TGenUserModel()
{
	if(FID != 0)
	{
		FDelete(FID);       // Clean up all memory associated with this instance
		FreeLibrary((HMODULE) FHandle);
	}
	// inherited;
}

//---------------------------------------------------------------

String TGenUserModel::get_FName()
{
	return FName;
}

//---------------------------------------------------------------

bool TGenUserModel::Get_Exists()
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

void TGenUserModel::Integrate()
{
	FSelect(FID);
	FIntegrate();
}

void TGenUserModel::select()
{
	FSelect(FID);
}

void TGenUserModel::Set_Edit(const String Value)
{
	if(FID != 0)
		FEdit((PAnsiChar) Value.c_str(), Value.size());
        // Else Ignore
}

void TGenUserModel::Set_Name(const String Value)
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
	FHandle = (NativeUInt) LoadLibrary(Value.c_str());   // Default LoadLibrary and PChar must agree in expected type
	if((HMODULE) FHandle == 0) // Try again with full path name
	{
		FHandle = (NativeUInt) LoadLibrary((DSSDirectory + Value).c_str());
	}
	if((HMODULE) FHandle == 0)
		DoSimpleMsg(String("Generator User Model ") + Value
	           + " Not Loaded. DSS Directory = "
	           + DSSDirectory, 570);
	else
	{
		FName = Value;

            // Now set up all the procedure variables
		FuncError = false;
		FNew = (GenUserModelProc__0) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("New")), "New");
		if(!FuncError)
			FSelect = (GenUserModelProc__2) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Select")), "Select");
		if(!FuncError)
			FInit = (GenUserModelProc__4) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Init")), "Init");
		if(!FuncError)
			FCalc = (GenUserModelProc__5) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Calc")), "Calc");
		if(!FuncError)
			FIntegrate = (GenUserModelProc__6) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Integrate")), "Integrate");
		if(!FuncError)
			FSave = (GenUserModelProc__8) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Save")), "Save");
		if(!FuncError)
			fRestore = (GenUserModelProc__9) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Restore")), "Restore");
		if(!FuncError)
			FEdit = (GenUserModelProc__3) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Edit")), "Edit");
		if(!FuncError)
			FUpdateModel = (GenUserModelProc__7) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("UpdateModel")), "UpdateModel");
		if(!FuncError)
			FDelete = (GenUserModelProc__1) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("Delete")), "Delete");
		if(!FuncError)
			FNumVars = (GenUserModelProc__10) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("NumVars")), "NumVars");
		if(!FuncError)
			FGetAllVars = (GenUserModelProc__11) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetAllVars")), "GetAllVars");
		if(!FuncError)
			FGetVariable = (GenUserModelProc__12) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVariable")), "GetVariable");
		if(!FuncError)
			FSetVariable = (GenUserModelProc__13) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("SetVariable")), "SetVariable");
		if(!FuncError)
			FGetVarName = (GenUserModelProc__14) CheckFuncError((void *) GetProcAddress((HMODULE) FHandle, ("GetVarName")), "GetVarName");
		if(FuncError)
		{
			FreeLibrary((HMODULE) FHandle);
			FID = 0;
			FHandle = 0;
			FName = "";
		}
		else
		{
			FID = FNew((*FActiveGeneratorVars), ActiveCircuit[ActiveActor]->Solution->DynaVars, CallBackRoutines);  // Create new instance of user model
		}
	}
}




}  // namespace GenUserModel




