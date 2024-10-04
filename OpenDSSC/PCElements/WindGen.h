#ifndef WindGenH
#define WindGenH

#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"

#include "WindGenVars.h"
#include "WindGenUserModel.h"
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
#include "ParserDel.h"
#include "Command.h"
#include <math.h>
#include "mathutil.h"
#include "DSSClassDefs.h"
#include "WTG3_Model.h"
#include "XYcurve.h"




namespace WindGen
{


/*
  ----------------------------------------------------------
  Copyright (c) 2024, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*   Change Log

   2/26/21 Created from   Generator.pas
   3/25/21 Removed Generator-related properties  (e.g., Fuel variables)

*/
/*
  In power flow modes, the WindGen element is essentially a negative load that can be dispatched.
*/

//  The WindGen is assumed balanced over the no. of phases defined

// If you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation (i.e. multiplier = 1.0 always)
//    Daily:   Defaults to No variation
//    Dutycycle: Defaults to Daily shape

const int NumWGenRegisters = 6;    // Number of energy meter registers
const int NumWGenVariables = 22;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TWindGen : public PCClass::TPCClass
{
	friend class TWindGenObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
	void InterpretConnection(const String s);
	void SetNcondsForConnection();
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherWindGenName);
public:
	String RegisterNames[6/*# range 1..NumGenRegisters*/];
	TWindGen();
	virtual ~TWindGen();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	void ResetRegistersAll(int ActorID);
	void SampleAll(int ActorID);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TWindGenObj : public PCElement::TPCElement
{
	friend class TWindGen;
public:
	typedef PCElement::TPCElement inherited;	
//private:
// Moved to WindGenVars        Zthev           :Complex;
	Ucomplex::complex Yeq;   // at nominal
	Ucomplex::complex Yeq95;   // at 95%
	Ucomplex::complex Yeq105;   // at 105%
	Ucomplex::complex Edp;
	Ucomplex::complex PhaseCurrentLimit;
	double Model7MaxPhaseCurr;
	double Model7LastAngle;
	bool DebugTrace;
	double DeltaQMax;  // Max allowable var change on Model=3 per iteration
	double DQDV;
	double DQDVSaved;
	bool FForcedON;
	bool FirstSampleAfterReset;
	bool IsFixed;   // if Fixed, always at base value
	int WindGenSolutionCount;
	double GenFundamental;  /*Thevinen equivalent voltage mag and angle reference for Harmonic model*/
	bool GenON;           /*Indicates whether WindGen is currently on*/
	bool GenSwitchOpen;
	bool kVANotSet;
	double LastGrowthFactor;
	int LastYear;   // added for speedup so we don't have to search for growth factor a lot
	int OpenWindGenSolutionCount;
	double PVFactor;  // deceleration Factor for computing vars for PV WindGens
	double RandomMult;
	int Reg_Hours;
	int Reg_kvarh;
	int Reg_kWh;
	int Reg_MaxkVA;
	int Reg_MaxkW;
	int Reg_Price;
	Ucomplex::complex ShapeFactor;
// moved to WindGenVars        Thetaharm       :Double;  {Thevinen equivalent voltage angle reference for Harmonic model}

	System::TTextRec Tracefile;

	TWindGenUserModel *UserModel;
	TWindGenUserModel *ShaftModel;   /*User-Written Models*/

	double V_Avg;
	double V_Remembered;
	double var_Remembered;
	double varBase; // Base vars per phase
	double varMax;
	double varMin;
	double VBase;  // Base volts suitable for computing currents
	double VBase105;
	double VBase95;
	Ucomplex::complex Vthev;  /*Thevinen equivalent voltage (complex) for dynamic model*/
// moved to WindGenVars        Vthevharm       :Double;  {Thevinen equivalent voltage mag reference for Harmonic model}
// moved to WindGenVars        VthevMag        :Double;    {Thevinen equivalent voltage for dynamic model}
	Ucmatrix::TcMatrix* YPrimOpenCond;  // To handle cases where one conductor of load is open ; We revert to admittance for inj currents
	double YQFixed;  // Fixed value of y for type 7 load
	bool ShapeIsActual;
	bool ForceBalanced;

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
	void DoDynamicMode(int ActorID);
	void DoFixedQGen(int ActorID);
	void DoFixedQZGen(int ActorID);
	void DoHarmonicMode(int ActorID);
	void DoPVTypeGen(int ActorID);
	void DoUserModel(int ActorID);
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
//protected:
	virtual void Set_ConductorClosed(int Index, int ActorID, bool Value);
	virtual void GetTerminalCurrents(Ucomplex::pComplexArray Curr, int ActorID);
public:
    TGE_WTG3_Model* WindModelDyn;
	int				Connection;			/*0 = line-neutral; 1=Delta*/
	String			DailyDispShape;		// Daily (24 HR) WindGen shape
	TLoadShapeObj*	DailyDispShapeObj;  // Daily WindGen Shape for this load
	String			DutyShape;			// Duty cycle load shape for changes typically less than one hour
	TLoadShapeObj*	DutyShapeObj;		// Shape for this WindGen
	double			DutyStart;			// starting time offset into the DutyShape [hrs] for this WindGen
	int				GenClass;		
	int				GenModel;			// Variation with voltage
	TWindGenVars	WindGenVars;		/*State Variables*/
    double			kvarBase,
					kvarMax,
					kvarMin,
					kWBase,
					PFNominal,
					Vpu,				// per unit Target voltage for WindGen with voltage control
					Vmaxpu,
					Vminpu;
    String			VV_Curve;
    TXYcurveObj*	VV_CurveObj;
    TXYcurveObj*	Loss_CurveObj;

	bool			GenActive;
// Fuel variables from Generator model removed

// moved to WindGenVars        VTarget         :Double;  // Target voltage for WindGen with voltage control
	String			YearlyShape;  // ='fixed' means no variation  on all the time
	TLoadShapeObj*	YearlyShapeObj;  // Shape for this WindGen
	double			Registers[6/*# range 1..NumGenRegisters*/];
	double			Derivatives[6/*# range 1..NumGenRegisters*/];

	TWindGenObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TWindGenObj();

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

    // Support for Harmonics Mode
	virtual void InitHarmonics(int ActorID);

	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model

	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);

	virtual int CheckIfDynVar(string myVar, int ActorID);
    virtual void SetDynOutput(string myVar);
    virtual string GetDynOutputStr();

	bool get_FForcedON();
	void set_FForcedON(bool myState);
	double get_PFNominal();

	TWindGenObj(DSSClass::TDSSClass* ParClass);
	TWindGenObj(String ClassName);
	TWindGenObj();
};
extern TWindGenObj* ActiveWindGenObj;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace WindGen

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace WindGen;
#endif

#endif // WindGenH




