
#pragma hdrstop

#include "CapUserControl.h"

#include "DSSGlobals.h"

using namespace std;
using namespace DSSGlobals;
using namespace System;


namespace CapUserControl
{



/* TCapUserControl */

void* TCapUserControl::CheckFuncError(void* Addr, String FuncName)
{
	void* result = nullptr;
	if(Addr == nullptr)
	{
		DoSimpleMsg(String("CapControl User Model Does Not Have Required Function: ") + FuncName, 569);
		FuncError = true;
	}
	result = Addr;
	return result;
}

TCapUserControl::TCapUserControl()
 : FHandle(0),
			FID(0),
			FuncError(false)
{
	FID = 0;
	FName = "";
}

TCapUserControl::~TCapUserControl()
{
	try
	{
		if(FID != 0)
			FDelete(FID);  // Clean up all memory associated with this instance
/* }
	__finally
	{*/
		if( (HMODULE) FHandle != 0)
			FreeLibrary( (HMODULE) FHandle);
	}
	catch (...)
	{
		//
	}
	// inherited;
}

//----------------------------------------------------------------------------

String TCapUserControl::get_FName()
{
	return FName;
}

//----------------------------------------------------------------------------

// do the pending control Action

void TCapUserControl::DoPending(int Code, int ProxyHdl)
{
	int varCode = 0;
	int varProxyHdl = 0;
	if(FID != 0)
	{
		varCode = Code; // Can't pass a const
		varProxyHdl = ProxyHdl;
		FDoPending(varCode, varProxyHdl);
	}
}

bool TCapUserControl::Get_Exists()
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
// Sample the cap control

void TCapUserControl::sample()
{
	if(FID != 0)
		FSample();
}

void TCapUserControl::select()
{
	FSelect(FID);
}

void TCapUserControl::Set_Edit(const String Value)
{
	if(FID != 0)
		FEdit(PAnsiChar(Value.c_str()), Value.size());
}

void TCapUserControl::Set_Name(const String Value)
{


    /*If Model already points to something, then free it*/
	if( (HMODULE) FHandle != 0)
	{
		if(FID != 0)
		{
			FDelete(FID);
			FName = "";
			FID = 0;
		}
		FreeLibrary( (HMODULE) FHandle);
	}

        /*If Value is blank or zero-length, bail out.*/
	if((Value.size() == 0) || (TrimLeft(Value).size() == 0))
		return;
	if(CompareText(Value, "none") == 0)
		return;
	FHandle = (NativeUInt) LoadLibrary(Value.c_str());   // Default LoadLibrary and PChar must agree in expected type
	if( (HMODULE) FHandle == 0) // Try again with full path name
	{
		FHandle = (NativeUInt) LoadLibrary( (DSSDirectory + Value).c_str() );
	}
	if( (HMODULE) FHandle == 0)
		DoSimpleMsg(String("CapControl User Model ") + Value
	           + " Load Library Failed. DSS Directory = "
	           + DSSDirectory, 570);
	else
	{
		FName = Value;

            // Now set up all the procedure variables
		FuncError = false;
		FNew = (CapUserControlProc__0) CheckFuncError((void *) GetProcAddress( (HMODULE) FHandle, ("New")), "New");
		if(!FuncError)
			FSelect = (CapUserControlProc__2) CheckFuncError((void *) GetProcAddress( (HMODULE) FHandle, ("Select")), "Select");
		if(!FuncError)
			FSample = (CapUserControlProc__4) CheckFuncError((void *) GetProcAddress( (HMODULE) FHandle, ("Sample")), "Sample");
		if(!FuncError)
			FDoPending = (CapUserControlProc__5) CheckFuncError((void *) GetProcAddress( (HMODULE) FHandle, ("DoPending")), "DoPending");
		if(!FuncError)
			FEdit = (CapUserControlProc__6) CheckFuncError((void *) GetProcAddress( (HMODULE) FHandle, ("Edit")), "Edit");
		if(!FuncError)
			FUpdateModel = (CapUserControlProc__3) CheckFuncError((void *) GetProcAddress( (HMODULE) FHandle, ("UpdateModel")), "UpdateModel");
		if(!FuncError)
			FDelete = (CapUserControlProc__1) CheckFuncError((void *) GetProcAddress( (HMODULE) FHandle, ("Delete")), "Delete");
		if(FuncError)
		{
			if(!FreeLibrary( (HMODULE) FHandle))
				DoSimpleMsg(String("Error Freeing DLL: ") + FName, 10570);  // decrement the reference count
			FID = 0;
			FHandle = 0;
			FName = "";
		}
		else
		{
			FID = FNew(CallBackRoutines);  // Create new instance of user model
		}
	}
}

void TCapUserControl::UpdateModel()
{
	if(FID != 0)
		FUpdateModel();
}




}  // namespace CapUserControl





