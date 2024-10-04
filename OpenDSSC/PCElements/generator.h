#ifndef GeneratorH
#define GeneratorH

#include "System.h"
#include "Sysutils.h"

#include "GeneratorVars.h"
#include "GenUserModel.h"
#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LoadShape.h"
#include "GrowthShape.h"
#include "Spectrum.h"
#include "Arraydef.h"
#include "Dynamics.h"
#include "d2c_structures.h"
#include "ControlElem.h"
#include "mathutil.h"




namespace Generator
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2023, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*   Change Log

    11/30/99 Added new properties to support conventional load flow
              Vset, Qmin, Qmax
    12/1/99 Split out ComputeYsc(ibus)
            Added Code to estimate DQDV
    12/2/99 Fixed bug in CalcYPrimMatrix - same bug as was in Load
    12/6/99 revised 95% - 105% limits - same as Load
    1-8-00 made voltage limites variable just like the Load.  Added vminpu
           and vmaxpu properties and modified YEq95, etc.
    2-2-00 Trapezoidal integration option
    2-28-00 Corrected Errors in Take Sample function
    8-23-00 Added FixedQ models; Added Price register and related dispatchmode
    8-24-00 Fixed Pnominalperphase so that it is never 0.0 to avoid divide by zero error
    9-20-00 Added InitStateVars  Function for Dynamics mode
    10-6-00 Fixed error in TakeSample for positive sequence model
    10-25-00 Added Spectrum   and code for Harmonic mode analysis
    10-27-00 Deleted GetCurrents Override;
    3-7-01 Fixed bug related to setting kvar=  (Index wrong)
    3-27-01 Added check to prevent divide by zero on calculation of PFNominal
    5-17-01 moved spectrum editing back to base class
    7-2-01 Corrected TakeSample to integrate only when GenON instead of S>0
           Also corrected kVA Max for Positive Seq only
    8-14-01 Added price signal integration, which had been omitted
            Fixed TakeSample so it would integrate on Trapezoidal when not GenON
    1-17/02 Fixed sign error for Type 5 model.
    7/11/02 Added code to change Yprim when generator changes ON/OFF state
    7/30/02 Fixed problem with propertyvalues and maxkvar
    11/08/02  Added Dynamics model
    11/11/02 Add user-written exciter and Shaft Models
    3/6/03   Revised user-written dll interface.
             added control terminal code for PCELement override.
    3-17-03  Revised user-written models and harmonic models
    5-11-09  Added properties to support kW, kvar, PV, and kV  through COM
    8-28-13 Forced re-initializing solution if Model 3 generator added.
    7-??-18 Corrected Generator Model 7 1-phase Model
*/
/*
  The generator is essentially a negative load that can be dispatched.

  If the dispatch value (DispValue) is 0, the generator always follows the
  appropriate dispatch curve, which are simply load curves. If DispValue>0 then
  the generator only comes on when the global circuit load multiplier exceeds
  DispValue.  When the generator is on, it always follows the dispatch curve
  appropriate for the type of solution being performed.

  If you want to model a generator that is fully on whenever it is dispatched on,
  simply designate "Status=Fixed".  The default is "Status=Variable" (i.e., it follows
  a dispatch curve.  You could also define a dispatch curve that is always 1.0.

  Generators have their own energy meters that record:
  1. Total kwh
  2. Total kvarh
  3. Max kW
  4. Max kVA
  5. Hours in operation
  6. Price * kwH

  Generator meters reset with the circuit energy meters and take a sample with
  the circuit energy meters as well. The Energy meters also used trapezoidal integration
  so that they are compatible with Load-Duration simulations.

  Generator models are:
  1. Constant P, Q  (* dispatch curve, if appropriate).
  2. Constant Z  (For simple solution)
  3. Constant P, |V|  like a standard power flow
  4. Constant P, Fixed Q  (vars)
  5. Constant P, Fixed Q  (reactance)
  6. User model
  7. Approximate Inverter model

  Most of the time you will use #1 for planning studies.

*/

//  The Generator is assumed balanced over the no. of phases defined

// If you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation (i.e. multiplier = 1.0 always)
//    Daily:   Defaults to No variation
//    Dutycycle: Defaults to Daily shape
const int NumGenRegisters = 6;    // Number of energy meter registers
const int NumGenVariables = 6;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TGenerator : public PCClass::TPCClass
{
	friend class TGeneratorObj;
public:
	typedef PCClass::TPCClass inherited;	
//private:
	void InterpretConnection(const String s);
	void SetNcondsForConnection();
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherGeneratorName);
public:
	String RegisterNames[6/*# range 1..NumGenRegisters*/];
	TGenerator();
	virtual ~TGenerator();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	void ResetRegistersAll(int ActorID);
	void SampleAll(int ActorID);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TGeneratorObj : public PCElement::TPCElement
{
	friend class TGenerator;
public:
	typedef PCElement::TPCElement inherited;	
//private:
// Moved to GeneratorVars        Zthev           :Complex;
	Ucomplex::complex Yeq;   // at nominal
	Ucomplex::complex Yeq95;   // at 95%
	Ucomplex::complex Yeq105;   // at 105%
	Ucomplex::complex Edp;
	Ucomplex::complex PhaseCurrentLimit;
	double Model7MaxPhaseCurr;
	double Model7LastAngle;
	bool DebugTrace;
	double DeltaQMax;  // Max allowable var change on Model=3 per iteration
	int DispatchMode;
	double DispatchValue;
	double DQDV;
	double DQDVSaved;
	bool FForcedON;
	bool FirstSampleAfterReset;
	bool IsFixed;   // if Fixed, always at base value
	int GeneratorSolutionCount;
	double GenFundamental;  /*Thevinen equivalent voltage mag and angle reference for Harmonic model*/
	bool GenON;           /*Indicates whether generator is currently on*/
	bool GenSwitchOpen;
	bool kVANotSet;
	double LastGrowthFactor;
	int LastYear;   // added for speedup so we don't have to search for growth factor a lot
	int OpenGeneratorSolutionCount;
	double PVFactor;  // deceleration Factor for computing vars for PV generators
	double RandomMult;
	int Reg_Hours,
		Reg_kvarh,
		Reg_kWh,
		Reg_MaxkVA,
		Reg_MaxkW,
		Reg_Price;
	Ucomplex::complex ShapeFactor;
// moved to GeneratorVars        Thetaharm       :Double;  {Thevinen equivalent voltage angle reference for Harmonic model}
	System::TTextRec Tracefile;
	GenUserModel::TGenUserModel* UserModel;
	GenUserModel::TGenUserModel* ShaftModel;   /*User-Written Models*/
	double	V_Avg,
			V_Remembered,
			var_Remembered,
			varBase, // Base vars per phase
			varMax,
			varMin,
			VBase,  // Base volts suitable for computing currents
			VBase105,
			VBase95;
	Ucomplex::complex Vthev;  /*Thevinen equivalent voltage (complex) for dynamic model*/
// moved to GeneratorVars        Vthevharm       :Double;  {Thevinen equivalent voltage mag reference for Harmonic model}
// moved to GeneratorVars        VthevMag        :Double;    {Thevinen equivalent voltage for dynamic model}
	Ucmatrix::TcMatrix* YPrimOpenCond;  // To handle cases where one conductor of load is open ; We revert to admittance for inj currents
	double YQFixed;  // Fixed value of y for type 7 load
	bool	ShapeIsActual,
			ForceBalanced;
	void CalcDailyMult(double hr);
	void CalcDutyMult(double hr);  // now incorporates DutyStart offset
	void CalcGenModelContribution(int ActorID);
	void CalcInjCurrentArray(int ActorID);
	void CalcVterminal(int ActorID);
	void CalcVTerminalPhase(int ActorID);
	void CalcVthev_Dyn();      // 3-phase Voltage behind transient reactance
	void CalcVthev_Dyn_Mod7(const Ucomplex::complex& V);
	void CalcYearlyMult(double hr);
	void CalcYPrimMatrix(Ucmatrix::TcMatrix* Ymatrix, int ActorID);
	void DoConstantPQGen(int ActorID);
	void DoConstantZGen(int ActorID);
	void DoCurrentLimitedPQ(int ActorID);
	void DoPVBus(int ActorID);
	void DoDynamicMode(int ActorID);
	void DoFixedQGen(int ActorID);
	void DoFixedQZGen(int ActorID);
	void DoHarmonicMode(int ActorID);
	void DoPVTypeGen(int ActorID);
	void DoUserModel(int ActorID);
	bool CheckOnFuel(double Deriv, double Interval, int ActorID);
	void Integrate(int reg, double Deriv, double Interval, int ActorID);
	void SetDragHandRegister(int reg, double Value);
	void StickCurrInTerminalArray(Ucomplex::pComplexArray TermArray, const Ucomplex::complex& Curr, int i);
	void WriteTraceRecord(const String s, int ActorID);
	void SyncUpPowerQuantities();
	double Get_PresentkW();
	double Get_Presentkvar();
	double Get_PresentkV();
	void Set_PresentkV(double Value);
	void Set_Presentkvar(double Value);
	void Set_PresentkW(double Value);
	void Set_PowerFactor(double Value);
	void SetkWkvar(double PkW, double Qkvar);
	// Variables associated with EMP mode
	ControlElem::TControlElem* gen_model;
	ControlElem::TControlElem* exc_model;
	ControlElem::TControlElem* turb_model;
protected:
	virtual void Set_ConductorClosed(int Index, int ActorID, bool Value);
	virtual void GetTerminalCurrents(Ucomplex::pComplexArray Curr, int ActorID);
public:
	int Connection;  /*0 = line-neutral; 1=Delta*/
	String DailyDispShape;  // Daily (24 HR) Generator shape
	LoadShape::TLoadShapeObj* DailyDispShapeObj;  // Daily Generator Shape for this load
	String DutyShape;  // Duty cycle load shape for changes typically less than one hour
	LoadShape::TLoadShapeObj* DutyShapeObj; // Shape for this generator
	double DutyStart;						// starting time offset into the DutyShape [hrs] for this generator
	int		GenClass,
			GenModel,						// Variation with voltage
			NCIMIdx;						// Index for voltage regulation when solving with NCIM algorithm
	GeneratorVars::TGeneratorVars GenVars; /*State Variables*/
	double kvarBase;
	double kvarMax;
	double kvarMin;
	double kWBase;
	double PFNominal;
	double Vpu;   // per unit Target voltage for generator with voltage control
	double Vmaxpu;
	double Vminpu;
// Fuel related variables
	bool GenActive;
	bool UseFuel;
	double FuelkWh;
	double pctFuel;
	double pctReserve;
	bool generatorInitialized;
	bool exciterInitialized;
	bool turbineInitialized;


// moved to GeneratorVars        VTarget         :Double;  // Target voltage for generator with voltage control
	String YearlyShape;  // ='fixed' means no variation  on all the time
	LoadShape::TLoadShapeObj* YearlyShapeObj;  // Shape for this Generator
	double Registers[6/*# range 1..NumGenRegisters*/];
	double Derivatives[6/*# range 1..NumGenRegisters*/];
	TGeneratorObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TGeneratorObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual int NumVariables();
	virtual void GetAllVariables(Arraydef::pDoubleArray States);
	virtual double Get_Variable(int i);
	virtual void Set_Variable(int i, double Value);
	virtual String VariableName(int i);
	void SetNominalGeneration(int ActorID);
	void Randomize(int Opt);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform
	void ResetRegisters();
	void TakeSample(int ActorID);

        // Procedures for setting the DQDV used by the Solution Object
	void InitDQDVCalc();
	void BumpUpQ();
	void RememberQV(int ActorID);
	void CalcDQDV(int ActorID);
	void ResetStartPoint();

        // Support for Dynamics Mode
	virtual void InitStateVars(int ActorID);
	virtual void IntegrateStates(int ActorID);
	virtual int InitializeStates(int ActorID);
	virtual void CalculateRate(int ActorID);
	virtual void StateIntegration(int ActorID);
	virtual	void StateIntegration_correction(int ActorID);
	virtual bool IsGenerator();
	virtual bool CheckForGeneratorModel();

        // Support for Harmonics Mode
	virtual void InitHarmonics(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);

	bool get_FForcedON();
	void set_FForcedON(bool Value);
	double get_PFNominal();
	void GetCurrents(pComplexArray Curr, int ActorID);   // overrides the standard procedure for model 8

	//__declspec (property (get = Get_PresentkW, put = Set_PresentkW ) )  double PresentkW;
	//__declspec (property (get = Get_Presentkvar, put = Set_Presentkvar ) )  double Presentkvar;
	//__declspec (property (get = get_ForcedON, put = set_FForcedON ) )  bool ForcedON;
	//__declspec (property (get = Get_PresentkV, put = Set_PresentkV ) )  double PresentkV;
	//__declspec (property (get = get_PFNominal, put = Set_PowerFactor ) )  double Powerfactor;
	TGeneratorObj(DSSClass::TDSSClass* ParClass);
	TGeneratorObj(String ClassName);
	TGeneratorObj();

	// Functions for NCIM solution algorithm
	void InitPVBusJac(int ActorID);

	// New auxiliary functions 
	complex GetVterminal(int ActorID);
};
extern TGeneratorObj* ActiveGeneratorObj;
extern TGenerator* GeneratorClass;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace Generator

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Generator;
#endif

#endif // GeneratorH




