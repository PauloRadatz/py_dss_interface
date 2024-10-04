#ifndef WindGenUserModelH
#define WindGenUserModelH

#include "System.h"

#include "WindGenVars.h"
#include "Dynamics.h"
#include "DSSCallBackRoutines.h"
#include "Ucomplex.h"
#include "Arraydef.h"

//#include "WindGen.h"
#include "Sysutils.h"

namespace WindGenUserModel
{



	/*$M+*/
	/*
	  ----------------------------------------------------------
	  Copyright (c) 2022, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------



	*/

	class TWindGenUserModel : public System::TObject
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
		typedef int (__stdcall *WindGenUserModelProc__0)(WindGenVars::TWindGenVars&, Dynamics::TDynamicsRec&, TDSSCallBacks&);
		WindGenUserModelProc__0 FNew;  // deletes specified instance
		typedef void (__stdcall *WindGenUserModelProc__1)(int&);
		WindGenUserModelProc__1 FDelete;    // Select active instance
		typedef int (__stdcall *WindGenUserModelProc__2)(int&);
		WindGenUserModelProc__2 FSelect;
		void Set_Name(const String Value);
		void* CheckFuncError(void* Addr, String FuncName);
		void Set_Edit(const String Value);
		bool Get_Exists();
	protected:
	public: // send string to user model to handle
		typedef void (__stdcall *WindGenUserModelProc__3)(System::PAnsiChar, unsigned int);
		WindGenUserModelProc__3 FEdit;   // For dynamics
		typedef void (__stdcall *WindGenUserModelProc__4)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
		WindGenUserModelProc__4 FInit; // returns Currents or sets Pshaft
		typedef void (__stdcall *WindGenUserModelProc__5)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
		WindGenUserModelProc__5 FCalc; // Integrates any state vars
		typedef void (__stdcall *WindGenUserModelProc__6)();
		WindGenUserModelProc__6 FIntegrate; // Called when props of WindGen updated
		typedef void (__stdcall *WindGenUserModelProc__7)();
		WindGenUserModelProc__7 FUpdateModel;
		WindGenVars::pTWindGenVars FActiveWindGenVars;

			/*Save and restore data*/
		typedef void (__stdcall *WindGenUserModelProc__8)();
		WindGenUserModelProc__8 FSave;

			/*Monitoring functions*/
		typedef void (__stdcall *WindGenUserModelProc__9)();
		WindGenUserModelProc__9 fRestore;
		typedef int (__stdcall *WindGenUserModelProc__10)();
		WindGenUserModelProc__10 FNumVars;  // Get all vars
		typedef void (__stdcall *WindGenUserModelProc__11)(Arraydef::pDoubleArray);
		WindGenUserModelProc__11 FGetAllVars;// Get a particular var
		typedef double (__stdcall *WindGenUserModelProc__12)(int&);
		WindGenUserModelProc__12 FGetVariable;
		typedef void (__stdcall *WindGenUserModelProc__13)(int&, double&);
		WindGenUserModelProc__13 FSetVariable;

		String get_FName();

			// this property loads library (if needed), sets the procedure variables, and makes a new instance
			// old reference is freed first
			// Wide string OK here
		typedef void (__stdcall *WindGenUserModelProc__14)(int&, System::PAnsiChar, unsigned int);
		WindGenUserModelProc__14 FGetVarName;
		void select();
		void Integrate();
		TWindGenUserModel(WindGenVars::pTWindGenVars ActiveWindGenVars);
		virtual ~TWindGenUserModel();
	//__published:
	public:
		TWindGenUserModel();
	};


}  // namespace WindGenUserModel

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace WindGenUserModel;
#endif

#endif // WindGenUserModelH

