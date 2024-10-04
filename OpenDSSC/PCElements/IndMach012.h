#ifndef IndMach012H
#define IndMach012H

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "LoadShape.h"
#include "GrowthShape.h"
#include "Spectrum.h"
#include "Dynamics.h"
#include "GeneratorVars.h"
#include "d2c_structures.h"



namespace IndMach012
{



// Symmetrical component Induction Machine model

//    ************  DRAFT Version 2 ******************************

/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*   Change Log

   November 10, 2016

   Created by
     Andres Ovalle
     Celso Rocha

*/

/*
   Description

   This is a Power Converstion (PC) element.

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

/* Collection manager for this class of element */   /* Notes Andres: -- definition of the class -- */

class TIndMach012 : public PCClass::TPCClass
{
	friend class TIndMach012Obj;
public:
	typedef PCClass::TPCClass inherited;	
private:

      /*These private functions are generally helper functions for Edit procedure*/

      /* A typical function */
	void SetNcondsForConnection();
protected:
	void DefineProperties();    // Define the property names and help strings
	virtual int MakeLike(const String OtherIndMach012Name);  // copy properties of another similar object
public:
	TIndMach012();
	virtual ~TIndMach012();
	virtual int Edit(int ActorID);      // Definition of the main property editing function
	virtual int Init(int Handle, int ActorID);  // Initialize by handle (index), if necessary
	virtual int NewObject(const String ObjName); // This function is called by the DSS New command
	
     /*any public functions that might be called from other elements*/
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

/* Class definition for this class of element*/
typedef Ucomplex::complex TSymCompArray[3];
    //pTDynamicsRec =  ^TDynamicsRec;
    //pTGeneratorVars = ^TGeneratorVars;

class TIndMach012Obj : public PCElement::TPCElement
{
	friend class TIndMach012;
public:
	typedef PCElement::TPCElement inherited;	
private:

      /*Private variables of this class*/
	int Connection;  /*0 = line-neutral; 1=Delta*/
	Ucomplex::complex Yeq;   // Y at nominal voltage
        // Pos seq slip
  // limit for slip to prevent solution blowing up
  // for power flow
	
        /*Dynamics variables*/ // Rotor time constant
	double puRs;
	double puXs;
	double puRr;
	double puXr;
	double puXm;
	double S1;
	double S2;
	double MaxSlip;
	double dSdP;
	double Xopen;
	double Xp;
	double T0p;
	bool InDynamics;    // Keep the last computed voltages and currents
	Ucomplex::complex Zs;
	Ucomplex::complex ZM;
	Ucomplex::complex Zr;
	Ucomplex::complex Is1;
	Ucomplex::complex Ir1;
	Ucomplex::complex V1;
	Ucomplex::complex Is2;
	Ucomplex::complex Ir2;
	Ucomplex::complex V2;

        /*Complex variables for dynamics*/
	Ucomplex::complex E1;
	Ucomplex::complex E1n;
	Ucomplex::complex dE1dt;
	Ucomplex::complex dE1dtn;
	Ucomplex::complex E2;
	Ucomplex::complex E2n;
	Ucomplex::complex dE2dt;
	Ucomplex::complex dE2dtn;
	Ucomplex::complex Zsp;
	bool FirstIteration;
	bool FixedSlip;
	double RandomMult;
	int IndMach012SolutionCount;
	bool IndMach012SwitchOpen;

        // Debugging
	System::TTextRec Tracefile;
	bool DebugTrace;
	GeneratorVars::TGeneratorVars MachineData;    // Use generator variable structure

        // Andres: NEW variables from generator
	bool MachineON;
	Ucomplex::complex ShapeFactor;
	bool ShapeIsActual;
        // Andres: end NEW variables from generator
	double VBase;
	double kWBase;
	void InterpretOption(String s);
	void set_Localslip(double Value);
	void Get_PFlowModelCurrent(const Ucomplex::complex& V, double s, Ucomplex::complex& Istator, Ucomplex::complex& Irotor);
	void Get_DynamicModelCurrent();
	void Set_Slip(double Value);
	double GetRotorLosses();
	double GetStatorLosses();
	double Compute_dSdP();
	void Randomize(int Opt);
	void InitModel(const TSymCompArray& V012, const TSymCompArray& I012);
	void CalcYPrimMatrix(Ucmatrix::TcMatrix* Ymatrix, int ActorID);
	void CalcIndMach012ModelContribution(int ActorID);
	void CalcInjCurrentArray(int ActorID);
	void DoIndMach012Model(int ActorID);
	void CalcModel(Ucomplex::pComplexArray V, Ucomplex::pComplexArray i, int ActorID);

        // Andres: NEW procedures from generator
	void CalcDailyMult(double hr);
	void CalcYearlyMult(double hr);
	void CalcDutyMult(double hr);
        // Andres: NEW procedures from generator
	void InitTraceFile();
	void WriteTraceRecord(int ActorID);
	double Get_PresentkV();
	void Set_PresentkV(double Value);
	void SetPowerkW(double PkW);
protected:

        /*A couple of virtual procedures you can override*/
	virtual void Set_ConductorClosed(int Index, int ActorID, bool Value);
	virtual void GetTerminalCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	void DoDynamicMode(int ActorID);
	void DoHarmonicMode(int ActorID);
public:

        /*Variables and functions accessed by DSS and other objects*/

        // Andres: new variables from generator
	String DailyDispShape;  // Daily (24 HR) Generator shape
	LoadShape::TLoadShapeObj* DailyDispShapeObj;  // Daily Generator Shape for this load
	LoadShape::TLoadShapeObj* DutyShapeObj;  // Shape for this generator
	String DutyShape;  //
	String YearlyShape;  // ='fixed' means no variation  on all the time
	LoadShape::TLoadShapeObj* YearlyShapeObj;  // Shape for this Generator
        // Andres: New variables from generator
	TIndMach012Obj(DSSClass::TDSSClass* ParClass, const String IndMach012ObjName);
	virtual ~TIndMach012Obj();
	virtual void RecalcElementData(int ActorID);   // Generally called after Edit is complete to recompute variables
	virtual void CalcYPrim(int ActorID);   // Calculate Primitive Y matrix
	void Integrate(int ActorID);
	void CalcDynamic(TSymCompArray V012, TSymCompArray I012);
	void CalcPFlow(TSymCompArray V012, TSymCompArray I012);
	void SetNominalPower(int ActorID);

        // Injection current management functions (unique to PC Elements)
	      // This is how the DSS represents elements with nonlinear characteristics
        // Inj currents are the difference between the desired total terminal currents and the
        // currents that result from the linear admittance matrix of the element
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);

      	// State variable management functions, if any
        // You can omit these if your PC element model is not using these
        // Default behavior is to basically do nothing
	virtual int NumVariables();
	virtual void GetAllVariables(Arraydef::pDoubleArray States);
	virtual double Get_Variable(int i);
	virtual void Set_Variable(int i, double Value);
	virtual String VariableName(int i);

        // Support for Dynamics Mode
	virtual void InitStateVars(int ActorID);
	virtual void IntegrateStates(int ActorID);

        // Support for Harmonics Mode
	virtual void InitHarmonics(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model, if possible

       // Functions required for managing values of properties
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	double get_S1();

       //Property Variable[i:Integer]:Double Read Get_Variable Write Set_Variable;
	
       /*Put any class properties here*/
       /*Use properties when some method must be executed when a value is set or retrieved*/

       /*   Example (from Load)
         Property ConnectedkVA        :Double Read FConnectedkVA        Write Set_ConnectedkVA;
       */
	TIndMach012Obj(DSSClass::TDSSClass* ParClass);
	TIndMach012Obj(String ClassName);
	TIndMach012Obj();
};
extern TIndMach012* IndMach012Class;
extern TIndMach012Obj* ActiveIndMach012Obj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace IndMach012

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace IndMach012;
#endif

#endif // IndMach012H




