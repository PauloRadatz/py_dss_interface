#ifndef StoreUserModelH
#define StoreUserModelH

#include "System.h"
#include "Sysutils.h"

#include "StorageVars.h"
#include "Dynamics.h"
#include "DSSCallBackRoutines.h"
#include "DSSCallBackStructDef.h"
#include "Ucomplex.h"
#include "Arraydef.h"

//#include "Storage.h"


namespace StoreUserModel
{



	/*$M+*/
	/*
	  ----------------------------------------------------------
	  Copyright (c) 2009-2022, Electric Power Research Institute, Inc.
	  All rights reserved.
	  ----------------------------------------------------------

	  This module contains two interfaces:
	  1) Standard user written model and
	  2) dynamics only DLL

	*/

	  // Interface for Dynamics-only user-written model

	class TStoreDynaModel
	{
	public:
		typedef TObject inherited;	
	//private:
		System::NativeUInt FHandle;  // Handle to DLL containing user model
		int FID;    // ID of this instance of the user model
		String FName;    // Name of the DLL file containing user model
		bool FuncError;

			 /*These functions should only be called by the object itself*/// Make a new instance
		typedef int (__stdcall *StoreUserModelProc__0)(Dynamics::TDynamicsRec&, TDSSCallBacks&);
		StoreUserModelProc__0 FNew;  // deletes specified instance
		typedef void (__stdcall *StoreUserModelProc__1)(int&);
		StoreUserModelProc__1 FDelete;    // Select active instance
		typedef int (__stdcall *StoreUserModelProc__2)(int&);
		StoreUserModelProc__2 FSelect;
		void Set_Name(const String Value);
		void* CheckFuncError(void* Addr, String FuncName);
		void Set_Edit(const String Value);
		bool Get_Exists();

		String get_FName();
	
	public: // send string to user model to handle
		typedef void (__stdcall *StoreUserModelProc__3)(System::PAnsiChar, unsigned int);
		StoreUserModelProc__3 FEdit;   // For dynamics
		typedef void (__stdcall *StoreUserModelProc__4)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
		StoreUserModelProc__4 FInit; // returns Currents or sets Pshaft
		typedef void (__stdcall *StoreUserModelProc__5)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
		StoreUserModelProc__5 FCalc; // Integrates any state vars
		typedef void (__stdcall *StoreUserModelProc__6)();
		StoreUserModelProc__6 FIntegrate; // Called when props of generator updated
	
			/*Monitoring functions*/
		typedef void (__stdcall *StoreUserModelProc__7)();
		StoreUserModelProc__7 FUpdateModel;
		typedef int (__stdcall *StoreUserModelProc__8)();
		StoreUserModelProc__8 FNumVars;  // Get all vars
		typedef void (__stdcall *StoreUserModelProc__9)(Arraydef::pDoubleArray);
		StoreUserModelProc__9 FGetAllVars;// Get a particular var
		typedef double (__stdcall *StoreUserModelProc__10)(int&);
		StoreUserModelProc__10 FGetVariable;
		typedef void (__stdcall *StoreUserModelProc__11)(int&, double&);
		StoreUserModelProc__11 FSetVariable;

			// this property loads library (if needed), sets the procedure variables, and makes a new instance
			// old reference is freed first
		typedef void (__stdcall *StoreUserModelProc__12)(int&, System::PAnsiChar, unsigned int);
		StoreUserModelProc__12 FGetVarName;
		void select();
		void Integrate();
		TStoreDynaModel();
		virtual ~TStoreDynaModel();
	//__published:
	};




	  // Interface for general user-written model that includes power flow calcs as well as dynamics

	class TStoreUserModel
	{
	public:
		typedef TObject inherited;	
	//private:
		System::NativeUInt FHandle;  // Handle to DLL containing user model
		int FID;    // ID of this instance of the user model
		String FName;    // Name of the DLL file containing user model
		bool FuncError;


			 /*These functions should only be called by the object itself*/// Make a new instance
		typedef int (__stdcall *StoreUserModelProc__13)(Dynamics::TDynamicsRec&, TDSSCallBacks&);
		StoreUserModelProc__13 FNew;  // deletes specified instance
		typedef void (__stdcall *StoreUserModelProc__14)(int&);
		StoreUserModelProc__14 FDelete;    // Select active instance
		typedef int (__stdcall *StoreUserModelProc__15)(int&);
		StoreUserModelProc__15 FSelect;
		void Set_Name(const String Value);
		void* CheckFuncError(void* Addr, String FuncName);
		void Set_Edit(const String Value);
		bool Get_Exists();

		std::string get_FName();

	protected:
	public: // send string to user model to handle
		typedef void (__stdcall *StoreUserModelProc__16)(System::PAnsiChar, unsigned int);
		StoreUserModelProc__16 FEdit;   // For dynamics
		typedef void (__stdcall *StoreUserModelProc__17)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
		StoreUserModelProc__17 FInit; // returns Currents or sets Pshaft
		typedef void (__stdcall *StoreUserModelProc__18)(Ucomplex::pComplexArray, Ucomplex::pComplexArray);
		StoreUserModelProc__18 FCalc; // Integrates any state vars
		typedef void (__stdcall *StoreUserModelProc__19)();
		StoreUserModelProc__19 FIntegrate; // Called when props of generator updated
	
			/*Save and restore data*/
		typedef void (__stdcall *StoreUserModelProc__20)();
		StoreUserModelProc__20 FUpdateModel;
		typedef void (__stdcall *StoreUserModelProc__21)();
		StoreUserModelProc__21 FSave;

			/*Monitoring functions*/
		typedef void (__stdcall *StoreUserModelProc__22)();
		StoreUserModelProc__22 fRestore;
		typedef int (__stdcall *StoreUserModelProc__23)();
		StoreUserModelProc__23 FNumVars;  // Get all vars
		typedef void (__stdcall *StoreUserModelProc__24)(Arraydef::pDoubleArray);
		StoreUserModelProc__24 FGetAllVars;// Get a particular var
		typedef double (__stdcall *StoreUserModelProc__25)(int&);
		StoreUserModelProc__25 FGetVariable;
		typedef void (__stdcall *StoreUserModelProc__26)(int&, double&);
		StoreUserModelProc__26 FSetVariable;

			// this property loads library (if needed), sets the procedure variables, and makes a new instance
			// old reference is freed first
		typedef void (__stdcall *StoreUserModelProc__27)(int&, System::PAnsiChar, unsigned int);
		StoreUserModelProc__27 FGetVarName;
		void select();
		void Integrate();
		TStoreUserModel();
		virtual ~TStoreUserModel();
	//__published:
	};


}  // namespace StoreUserModel

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace StoreUserModel;
#endif

#endif // StoreUserModelH




