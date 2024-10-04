#ifndef GenUserModelH
#define GenUserModelH

#include "System.h"
#include "Sysutils.h"

#include "GeneratorVars.h"
#include "Dynamics.h"
#include "DSSCallBackRoutines.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "d2c_structures.h"

namespace GenUserModel
{



/*$M+*/
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  7-7-10
  Modified to allow DLLS to retain ANSI string type in Edit function and Var names
  Nominally all strings passed to DLLS are ASCII to make it easier to write code in other languages
  and legacy defaults

*/

class TGenUserModel : public System::TObject
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
	typedef int (__stdcall *GenUserModelProc__0)(GeneratorVars::TGeneratorVars&, Dynamics::TDynamicsRec&, TDSSCallBacks&);
	GenUserModelProc__0 FNew;  // deletes specified instance
	typedef void (__stdcall *GenUserModelProc__1)(int&);
	GenUserModelProc__1 FDelete;    // Select active instance
	typedef int (__stdcall *GenUserModelProc__2)(int&);
	GenUserModelProc__2 FSelect;
	void Set_Name(const String Value);
	void* CheckFuncError(void* Addr, String FuncName);
	void Set_Edit(const String Value);
	bool Get_Exists();
	String get_FName();

protected:
public: // send string to user model to handle
	typedef void (__stdcall *GenUserModelProc__3)(System::PAnsiChar, unsigned int);
	GenUserModelProc__3 FEdit;   // For dynamics
	typedef void (__stdcall *GenUserModelProc__4)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
	GenUserModelProc__4 FInit; // returns Currents or sets Pshaft
	typedef void (__stdcall *GenUserModelProc__5)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
	GenUserModelProc__5 FCalc; // Integrates any state vars
	typedef void (__stdcall *GenUserModelProc__6)();
	GenUserModelProc__6 FIntegrate; // Called when props of generator updated
	typedef void (__stdcall *GenUserModelProc__7)();
	GenUserModelProc__7 FUpdateModel;
	GeneratorVars::pTGeneratorVars FActiveGeneratorVars;

        /*Save and restore data*/
	typedef void (__stdcall *GenUserModelProc__8)();
	GenUserModelProc__8 FSave;

        /*Monitoring functions*/
	typedef void (__stdcall *GenUserModelProc__9)();
	GenUserModelProc__9 fRestore;
	typedef int (__stdcall *GenUserModelProc__10)();
	GenUserModelProc__10 FNumVars;  // Get all vars
	typedef void (__stdcall *GenUserModelProc__11)(Arraydef::pDoubleArray);
	GenUserModelProc__11 FGetAllVars;// Get a particular var
	typedef double (__stdcall *GenUserModelProc__12)(int&);
	GenUserModelProc__12 FGetVariable;
	typedef void (__stdcall *GenUserModelProc__13)(int&, double&);
	GenUserModelProc__13 FSetVariable;

        // this property loads library (if needed), sets the procedure variables, and makes a new instance
        // old reference is freed first
        // Wide string OK here
	typedef void (__stdcall *GenUserModelProc__14)(int&, System::PAnsiChar, unsigned int);
	GenUserModelProc__14 FGetVarName;
	void select();
	void Integrate();
	TGenUserModel(GeneratorVars::pTGeneratorVars ActiveGeneratorVars);
	virtual ~TGenUserModel();
//__published:
public:
	TGenUserModel();
};


}  // namespace GenUserModel

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace GenUserModel;
#endif

#endif // GenUserModelH




