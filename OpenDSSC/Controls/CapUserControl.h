#ifndef CapUserControlH
#define CapUserControlH

#include "System.h"
#include "Sysutils.h"

#include "CapControlVars.h"
#include "Dynamics.h"
#include "DSSCallBackRoutines.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "d2c_structures.h"

namespace CapUserControl
{



/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

    Interface to user-written CapControl DLL
*/


/*$M+*/
/*
  ----------------------------------------------------------
  Copyright (c) 2012, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  3-31-12
  Converted from GenUserModel.Pas

*/

class TCapUserControl : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
	System::NativeUInt FHandle;  // Handle to DLL containing user model
	int FID;    // ID of this instance of the user model
         // OK for this to be Wide String, since not passed to DLLs
	String FName;    // Name of the DLL file containing user model
	bool FuncError;


         /*These functions should only be called by the object itself*/// Make a new instance
	typedef int (__stdcall *CapUserControlProc__0)(TDSSCallBacks&);
	CapUserControlProc__0 FNew;  // deletes specified instance
	typedef void (__stdcall *CapUserControlProc__1)(int&);
	CapUserControlProc__1 FDelete;    // Select active instance
	typedef int (__stdcall *CapUserControlProc__2)(int&);
	CapUserControlProc__2 FSelect; // Called when props of CapControl updated
	typedef void (__stdcall *CapUserControlProc__3)();
	CapUserControlProc__3 FUpdateModel;
	typedef void (__stdcall *CapUserControlProc__4)();
	CapUserControlProc__4 FSample;
	typedef void (__stdcall *CapUserControlProc__5)(int&, int&);
	CapUserControlProc__5 FDoPending;
	// this property loads library (if needed), sets the procedure variables, and makes a new instance
	// old reference is freed first
	// Wide string OK here
	void Set_Name(const String Value);
	void* CheckFuncError(void* Addr, String FuncName);
	void Set_Edit(const String Value);
	bool Get_Exists();
protected:
public: // send string to user model to handle
	typedef void (__stdcall *CapUserControlProc__6)(System::PAnsiChar, unsigned int);
	CapUserControlProc__6 FEdit;
	void select();
	void UpdateModel();
	void DoPending(int Code, int ProxyHdl);
	void sample();
	TCapUserControl();
	virtual ~TCapUserControl();

	String get_FName();

//__published:
};


}  // namespace CapUserControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CapUserControl;
#endif

#endif // CapUserControlH





