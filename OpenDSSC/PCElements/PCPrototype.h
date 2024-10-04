#ifndef PCPrototypeH
#define PCPrototypeH

#include <System.hpp>
#include "../Support/d2c_system.h"

#include "../Common/DSSClass.h"
#include "../PCElements/PCClass.h"
#include "../PCElements/PCElement.h"
#include "../Shared/Ucmatrix.h"
#include "../Shared/Ucomplex.h"
#include "../shared/Arraydef.h"
#include "../General/LoadShape.h"
#include "../General/GrowthShape.h"
#include "../General/Spectrum.h"
#include "../Shared/Dynamics.h"
#include "../Support/d2c_sysfile.h"

namespace PCPrototype
{



// Prototype for PC Element class

// Do a global Replace on "PCPrototype" to the name of your class to start

/*
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*   Change Log

*/

/*
   Description

   This is a prototype of a Power Converstion (PC) element.

   PC elements are Load, Generator, Vsource, Isource, etc. PC elements are
   used to model devices that convert the power delivered by the Power Delivery (PD)
   elements into some other form.  PC elements are generally considered to be
   in shunt with the power system and are the terminations of the power flow
   while PD elements are considered to be in series with the power flow path.

   Both PC and PD elements are represpented by their primitive Y matrices. PC elements
   are also used to model the nonlinear devices in the system (see the Load model). They
   differ from PD elements in that they have a current injection source in parallel with
   the primitive Y matrix.

*/

/*Add other modules accessed by this class*/   // Base class for most DSS objects
    // Base class for collection manager for PC elements
  // Base class for PC  Elements
     // Unit for managing complex matrice (for Yprim, etc)
     // Complex math functions, type definitions
     // definitions of basic DSS arrays

    // common modules used in PC elements
    // class for supporting/representing loadshapes
  // Class for holding growth shapes
     // Definitions for harmonic spectra
     // for elements that interact with dynamics variables


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

/* Collection manager for this class of element */

class TPCPrototype : public PCClass::TPCClass
{
	friend class TPCPrototypeObj;
public:
	typedef PCClass::TPCClass inherited;	
private:

      /*These private functions are generally helper functions for Edit procedure*/

      /* A typical function */
	void SetNcondsForConnection();
protected:
	void DefineProperties();    // Define the property names and help strings
	virtual int MakeLike(const String OtherPCPrototypeName);  // copy properties of another similar object
public:
	TPCPrototype();
	virtual ~TPCPrototype();
	virtual int Edit();      // Definition of the main property editing function 
	virtual int Init(int Handle);  // Initialize by handle (index), if necessary
	virtual int NewObject(const String ObjName); // This function is called by the DSS New command
	
     /*any public functions that might be called from other elements*/
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

/* Class definition for this class of element*/

class TPCPrototypeObj : public PCElement::TPCElement
{
	friend class TPCPrototype;
public:
	typedef PCElement::TPCElement inherited;	
private:

      /*Private variables of this class*/
       // a typical private variable:
	Ucomplex::complex Yeq;   // Y at nominal voltage

       // a typical procedure if user models are supported
	void DoUserModel();

       // many PC elements have a proc like this to map computed currents into appropriate array
	void StickCurrInTerminalArray(Ucomplex::pComplexArray TermArray, const Ucomplex::complex& Curr, int i);
protected:

        /*A couple of virtual procedures you can override*/
	virtual void Set_ConductorClosed(int Index, bool Value);
	virtual void GetTerminalCurrents(Ucomplex::pComplexArray Curr);
public:

        /*Variables and functions accessed by DSS and other objects*/
	TPCPrototypeObj(DSSClass::TDSSClass* ParClass, const String PCPrototypeObjName);
	virtual ~TPCPrototypeObj();
	virtual void RecalcElementData();   // Generally called after Edit is complete to recompute variables
	virtual void CalcYPrim();   // Calculate Primitive Y matrix 

        // Injection current management functions (unique to PC Elements)
	      // This is how the DSS represents elements with nonlinear characteristics
        // Inj currents are the difference between the desired total terminal currents and the
        // currents that result from the linear admittance matrix of the element
	virtual int InjCurrents();
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr);

      	// State variable management functions, if any
        // You can omit these if your PC element model is not using these
        // Default behavior is to basically do nothing
	virtual int NumVariables();
	virtual void GetAllVariables(Arraydef::pDoubleArray States);
	virtual double Get_Variable(int i);
	virtual void Set_Variable(int i, double Value);
	virtual String VariableName(int i);

        // Support for Dynamics Mode
	virtual void InitStateVars();
	virtual void IntegrateStates();

        // Support for Harmonics Mode
	virtual void InitHarmonics();
	virtual void MakePosSequence();  // Make a positive Sequence Model, if possible

       // Functions required for managing values of properties
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(d2c_system::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);

       /*Put any class properties here*/
       /*Use properties when some method must be executed when a value is set or retrieved*/

       /*   Example (from Load)
         Property ConnectedkVA        :Double Read FConnectedkVA        Write Set_ConnectedkVA;
       */
	TPCPrototypeObj(DSSClass::TDSSClass* ParClass);
	TPCPrototypeObj(String ClassName);
	TPCPrototypeObj();
};
extern TPCPrototypeObj* ActivePCPrototypeObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace PCPrototype

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PCPrototype;
#endif

#endif // PCPrototypeH




