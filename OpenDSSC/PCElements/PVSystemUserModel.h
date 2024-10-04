#ifndef PVsystemUserModelH
#define PVsystemUserModelH

#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"
#include "Dynamics.h"
#include "DSSCallBackRoutines.h"



namespace PVsystemUserModel
{



/*$M+*/
/*
  ----------------------------------------------------------
  Copyright (c) 2009-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TPVsystemUserModel : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
	System::NativeUInt FHandle;  // Handle to DLL containing user model
	int FID;    // ID of this instance of the user model
	String FName;    // Name of the DLL file containing user model
	bool FuncError;


         /*These functions should only be called by the object itself*/// Make a new instance
	typedef int (__stdcall *PVsystemUserModelProc__0)(TDynamicsRec&, TDSSCallBacks&);
	PVsystemUserModelProc__0 FNew;  // deletes specified instance
	typedef void (__stdcall *PVsystemUserModelProc__1)(int&);
	PVsystemUserModelProc__1 FDelete;    // Select active instance
	typedef int (__stdcall *PVsystemUserModelProc__2)(int&);
	PVsystemUserModelProc__2 FSelect;
	void Set_Name(const String Value);
	void* CheckFuncError(void* Addr, String FuncName);
	void Set_Edit(const String Value);
	bool Get_Exists();

	String get_FName();

protected:
public: // send string to user model to handle
	typedef void (__stdcall *PVsystemUserModelProc__3)(System::PAnsiChar, unsigned int);
	PVsystemUserModelProc__3 FEdit;   // For dynamics
	typedef void (__stdcall *PVsystemUserModelProc__4)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
	PVsystemUserModelProc__4 FInit; // returns Currents or sets Pshaft
	typedef void (__stdcall *PVsystemUserModelProc__5)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
	PVsystemUserModelProc__5 FCalc; // Integrates any state vars
	typedef void (__stdcall *PVsystemUserModelProc__6)();
	PVsystemUserModelProc__6 FIntegrate; // Called when props of generator updated
	

        /*Save and restore data*/
	typedef void (__stdcall *PVsystemUserModelProc__7)();
	PVsystemUserModelProc__7 FUpdateModel;
	typedef void (__stdcall *PVsystemUserModelProc__8)();
	PVsystemUserModelProc__8 FSave;

        /*Monitoring functions*/
	typedef void (__stdcall *PVsystemUserModelProc__9)();
	PVsystemUserModelProc__9 fRestore;
	typedef int (__stdcall *PVsystemUserModelProc__10)();
	PVsystemUserModelProc__10 FNumVars;  // Get all vars
	typedef void (__stdcall *PVsystemUserModelProc__11)(Arraydef::pDoubleArray);
	PVsystemUserModelProc__11 FGetAllVars;// Get a particular var
	typedef double (__stdcall *PVsystemUserModelProc__12)(int&);
	PVsystemUserModelProc__12 FGetVariable;
	typedef void (__stdcall *PVsystemUserModelProc__13)(int&, double&);
	PVsystemUserModelProc__13 FSetVariable;

        // this property loads library (if needed), sets the procedure variables, and makes a new instance
        // old reference is freed first
	typedef void (__stdcall *PVsystemUserModelProc__14)(int&, System::PAnsiChar, unsigned int);
	PVsystemUserModelProc__14 FGetVarName;
	void select();
	void Integrate();
	TPVsystemUserModel();
	virtual ~TPVsystemUserModel();
//__published:
};


}  // namespace PVsystemUserModel

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PVsystemUserModel;
#endif

#endif // PVsystemUserModelH




