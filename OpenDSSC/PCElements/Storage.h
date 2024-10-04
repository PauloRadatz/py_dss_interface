#ifndef StorageH
#define StorageH

#include "System.h"
#include "Sysutils.h"

#include "StorageVars.h"
#include "StoreUserModel.h"
#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LoadShape.h"
#include "Spectrum.h"
#include "Arraydef.h"
#include "Dynamics.h"
#include "d2c_structures.h"
#include "DSSClassDefs.h"
#include "XYcurve.h"
#include "InvDynamics.h"
#include "mathutil.h"


namespace Storage
{



/*
  ----------------------------------------------------------
  Copyright (c) 2009-2023, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*   Change Log

    10/04/2009 Created from Generator Model


  To Do:
    Make connection to User model
    Yprim for various modes
    Define state vars and dynamics mode behavior
    Complete Harmonics mode algorithm (generator mode is implemented)
*/
/*
  The storage element is essentially a generator that can be dispatched
  to either produce power or consume power commensurate with rating and
  amount of stored energy.

  The storage element can also produce or absorb vars within the kVA rating of the inverter.
  That is, a StorageController object requests kvar and the storage element provides them if
  it has any capacity left. The storage element can produce/absorb kvar while idling.
*/

//  The Storage element is assumed balanced over the no. of phases defined
const int NumStorageRegisters = 6;    // Number of energy meter registers
const int NumStorageVariables = 25 + 9;    // No state variables

       
//= = = = = = = = = = = = = = DEFINE STATES = = = = = = = = = = = = = = = = = = = = = = = = =
const int STORE_CHARGING = -1;
const int STORE_IDLING = 0;
const int STORE_DISCHARGING = 1;
//= = = = = = = = = = = = = = DEFINE DISPATCH MODES = = = = = = = = = = = = = = = = = = = = = = = = =
const int STORE_DEFAULT = 0;
const int STORE_LOADMODE = 1;
const int STORE_PRICEMODE = 2;
const int STORE_EXTERNALMODE = 3;
const int STORE_FOLLOW = 4;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TStorage : public PCClass::TPCClass
{
	friend class TStorageObj;
public:
	typedef PCClass::TPCClass inherited;	
//private:
	void InterpretConnection(const String s);
	void SetNcondsForConnection();
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherStorageObjName);
public:
	String RegisterNames[6/*# range 1..NumStorageRegisters*/];
	TStorage();
	virtual ~TStorage();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	void ResetRegistersAll();
	void SampleAll(int ActorID);
	void UpdateAll(int ActorID);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TStorageObj : public PCElement::TPCElement
{
	friend class TStorage;
public:
	typedef PCElement::TPCElement inherited;	
//private:
	Ucomplex::complex Yeq;   // at nominal
	Ucomplex::complex Yeq95;   // at 95%
	Ucomplex::complex Yeq105;   // at 105%
	Ucomplex::complex YeqIdling;   // in shunt representing idle impedance
	double PIdling;
	Ucomplex::complex YeqDischarge;   // equiv at rated power of storage element only
	Ucomplex::complex PhaseCurrentLimit;
	double MaxDynPhaseCurrent;
	bool DebugTrace;
	int fState;
	bool FStateChanged;
	bool FirstSampleAfterReset;
	int StorageSolutionCount;
	double StorageFundamental;  /*Thevinen equivalent voltage mag and angle reference for Harmonic model*/
	bool StorageObjSwitchOpen;
	bool ForceBalanced;
	bool CurrentLimited;
	double pctR;
	double pctX;
	int OpenStorageSolutionCount;
	double Pnominalperphase;
	double Qnominalperphase;
	double RandomMult;
	int Reg_Hours;
	int Reg_kvarh;
	int Reg_kWh;
	int Reg_MaxkVA;
	int Reg_MaxkW;
	int Reg_Price;
	Ucomplex::complex ShapeFactor;
	System::TTextRec Tracefile;
	bool IsUserModel;

	TStoreUserModel *UserModel;   /*User-Written Models*/
	TStoreDynaModel *DynaModel;

	double kvarBase;  // Base vars per phase
	double VBase;  // Base volts suitable for computing currents
	double VBase105;
	double VBase95;
	double Vmaxpu;
	double Vminpu;
	Ucmatrix::TcMatrix* YPrimOpenCond;
	void CalcDailyMult(double hr, int ActorID);
	void CalcDutyMult(double hr, int ActorID);
	void CalcStorageModelContribution(int ActorID);
	void CalcInjCurrentArray(int ActorID);

	void ComputePresentkW();
	void ComputeInverterPower();

	void ComputekWkvar();
	void ComputeDCkW();

        /*PROCEDURE CalcVterminal;*/
	void CalcVTerminalPhase(int ActorID);
	void CalcYearlyMult(double hr, int ActorID);
	void CalcYPrimMatrix(Ucmatrix::TcMatrix* Ymatrix, int ActorID);
	void DoConstantPQStorageObj(int ActorID);
	void DoConstantZStorageObj(int ActorID);
	void DoDynamicMode(int ActorID);
	void DoHarmonicMode(int ActorID);
	void DoUserModel(int ActorID);
	void DoDynaModel(int ActorID);
	void Integrate(int reg, double Deriv, double Interval, int ActorID);
	void SetDragHandRegister(int reg, double Value);
	void StickCurrInTerminalArray(Ucomplex::pComplexArray TermArray, const Ucomplex::complex& Curr, int i);
	void WriteTraceRecord(const String s, int ActorID);
	void SyncUpPowerQuantities();
	void SetKWandKvarOut();
	void CheckStateTriggerLevel(double Level, int ActorID);
	void UpdateStorage(int ActorID);    // Update Storage elements based on present kW and IntervalHrs variable
	double NormalizeToTOD(int h, double Sec);
	int InterpretState(const String s);
//        FUNCTION  StateToStr:String;
	String DecodeState();
	double Get_PresentkW();
	double Get_Presentkvar();
	double Get_PresentkV();
	void Set_PresentkV(double Value);
	void Set_Presentkvar(double Value);
	void Set_PresentkW(double Value);
	void Set_PowerFactor(double Value);
	void Set_StorageState(int Value);
	void Set_pctkvarOut(double Value);
	void Set_pctkWOut(double Value);
	double Get_kWTotalLosses();
	double Get_kWIdlingLosses();

	void kWOut_Calc();

protected:
	virtual void Set_ConductorClosed(int Index, int ActorID, bool Value);
	virtual void GetTerminalCurrents(Ucomplex::pComplexArray Curr, int ActorID);
public:
	int Connection;  /*0 = line-neutral; 1=Delta*/
	String DailyShape;  // Daily (24 HR) Storage element shape
	LoadShape::TLoadShapeObj* DailyShapeObj;  // Daily Storage element Shape for this load
	String DutyShape;  // Duty cycle load shape for changes typically less than one hour
	LoadShape::TLoadShapeObj* DutyShapeObj;  // Shape for this Storage element
	int StorageClass;
	int VoltageModel;   // Variation with voltage
	double PFNominal;
	String YearlyShape;  // ='fixed' means no variation  on all the time
	LoadShape::TLoadShapeObj* YearlyShapeObj;  // Shape for this Storage element
	StorageVars::TStorageVars StorageVars;
	InvDynamics::TInvDynamicVars myDynVars;
	double FpctkWout;   // percent of kW rated output currently dispatched
	double Fpctkvarout;
	double pctReserve;
	int DispatchMode;
	bool kVANotSet;
	double kvar_out;
	double kW_out;
	double pctIdlekW;

	double kWOutIdling;

	double pctIdlekvar;
	double pctChargeEff;
	double pctDischargeEff;
	double DischargeTrigger;
	double ChargeTrigger;
	double ChargeTime;
	double kWhBeforeUpdate;
	double Registers[6];
	double Derivatives[6];
	//-----------------Inverter type vars-----------------------------------------------
	std::string InverterCurve;
	TXYcurveObj* InverterCurveObj;

	bool FVWStateRequested;

	double CurrentkvarLimit;
	double CurrentkvarLimitNeg;
	double FpctkWin;

	// Variables for InvControl's Volt-Watt function
	bool FVWMode; //boolean indicating if under volt-watt control mode from InvControl (not ExpControl)
	bool FVVMode; //boolean indicating if under volt-var mode from InvControl
	bool FDRCMode; //boolean indicating if under DRC mode from InvControl
	bool FWPMode; //boolean indicating if under watt-pf mode from InvControl
	bool FWVMode; //boolean indicating if under watt-var mode from InvControl
    bool FAVRMode; //boolean indicating whether under AVR mode from ExpControl (or InvControl, but that does not seem to be implemented yet)

	double FkvarRequested;
	double FkWRequested;
	int FvarMode;
	double FDCkW;
	double Fpf_wp_nominal;

	// Variables for Inverter functionalities
	double FpctCutIn;
	double FpctCutOut;
	bool FVarFollowInverter;
	double CutInkW;
	double CutOutkW;

	double FCutOutkWAC;  // CutInkW  reflected to the AC side of the inverter
	double FCutInkWAC;   // CutOutkW reflected to the AC side of the inverter

	Integer FStateDesired;  // Stores desired state (before any change due to kWh limits or %CutIn/%CutOut

	bool FInverterON;
	double FpctPminNoVars;
	double FpctPminkvarLimit;
	double PminNoVars;
	double PminkvarLimit;
	double kVA_exceeded;

	std::vector<TPICtrl> PICtrl;

	bool kvarLimitSet;
	bool kvarLimitNegSet;
	bool kVASet;



	TStorageObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TStorageObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual int NumVariables();
	virtual void GetAllVariables(Arraydef::pDoubleArray States);
	virtual double Get_Variable(int i);
	virtual void Set_Variable(int i, double Value);
	virtual String VariableName(int i);
	void Randomize(int Opt);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform
	void ResetRegisters();
	void TakeSample(int ActorID);

        // Support for Dynamics Mode
	virtual void InitStateVars(int ActorID);
	virtual void IntegrateStates(int ActorID);

        // Support for Harmonics Mode
	virtual void InitHarmonics(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);

	double get_PFNominal();
	int get_fState();
	double get_FpctkWout();
	double get_Fpctkvarout();
	double get_Vminpu();

//---------------------------------------------------------------------------------------------------

	double Get_kW();
	void Set_kW(double Value);
	double Get_kWDesired();
	void Set_StateDesired(int Value);
	double Get_kWRequested();
	void Set_kWRequested(double Value);
	double Get_kvarRequested();
	void Set_kvarRequested(double Value);
	double Get_FkVARating();
	void Set_kVARating(double Value);
	double Get_FpctkWrated();
	void Set_pctkWrated(double Value);
	int Get_Varmode();
	void Set_Varmode(int Value);
	bool Get_VWmode();
	void Set_VWmode(bool Value);
	bool Get_VVmode();
	void Set_VVmode(bool Value);
	bool Get_WPmode();
	void Set_WPmode(bool Value);
	bool Get_WVmode();
	void Set_WVmode(bool Value);
	bool Get_DRCmode();
	void Set_DRCmode(bool Value);
	bool Get_AVRmode();
	void Set_AVRmode(bool Value);
	bool Get_InverterON();
	bool Get_CIMDynamicMode();
	void Set_InverterON(bool Value);
	double Get_CutOutkWAC();
	double Get_CutInkWAC();
	bool Get_VarFollowInverter();
	void Set_VarFollowInverter(bool Value);
	double Get_Fkvarlimit();
	void Set_Maxkvar(double Value);
	double Get_Fkvarlimitneg();
	void Set_Maxkvarneg(double Value);

	void SetNominalStorageOutput(int ActorID);

	double Get_FpctkWIn();
	void Set_pctkWIn(double myMaxkvar);
	double Get_InverterLosses();
	double Get_kWChDchLosses();
	void Update_EfficiencyFactor();

	double Get_DCkW();
	double Get_VminPu();
	void Set_pf_wp_nominal(double myPFNom);
	double Get_Pmin();
	double Get_Pmax();
	double Get_qMaxInj();
	double Get_qMaxAbs();
	double Get_acVmin();
	double Get_acVmax();
	double Get_acVnom();
	double Get_pMaxUnderPF();
	double Get_pMaxOverPF();
	double Get_pMaxCharge();
	double Get_sMaxCharge();
	void DoGFM_Mode(int ActorID);
	void GetCurrents(pComplexArray Curr, int ActorID);
	bool CheckAmpsLimit(int ActorID);
	bool CheckOLInverter(int ActorID);
	bool CheckIfDelivering(int ActorID);

	// Functions for NCIM solution algorithm
	void DoPQBusNCIM(int ActorID, int i, complex V, complex Curr);

//---------------------------------------------------------------------------------------------------

	//__declspec (property (get = Get_kW, put = Set_kW))  double kW;
	//__declspec (property (get = Get_kWDesired))  double kWDesired;
	//__declspec (property (put = Set_StateDesired))  int StateDesired;
	//__declspec (property (get = Get_kWRequested, put = Set_kWRequested))  double kWRequested;
	//__declspec (property (get = Get_kvarRequested, put = Set_kvarRequested))  double kvarRequested;
	//__declspec (property (get = Get_FkVARating, put = Set_kVARating))  double kVARating;
	//__declspec (property (get = Get_FpctkWrated, put = Set_pctkWrated))  double pctkWrated;
	//__declspec (property (get = Get_Varmode, put = Set_Varmode))  int Varmode;
	//__declspec (property (get = Get_VWmode, put = Set_VWmode))  bool VWmode;
	//__declspec (property (get = Get_VVmode, put = Set_VVmode))  bool VVmode;
	//__declspec (property (get = Get_WPmode, put = Set_WPmode))  bool WPmode;
	//__declspec (property (get = Get_WVmode, put = Set_WVmode))  bool WVmode;
	//__declspec (property (get = Get_DRCmode, put = Set_DRCmode))  bool DRCmode;
	//__declspec (property (get = Get_InverterON, put = Set_InverterON))  bool InverterON;
	//__declspec (property (get = Get_CutOutkWAC))  double CutOutkWAC;
	//__declspec (property (get = Get_CutInkWAC))  double CutInkWAC;
	//__declspec (property (get = Get_VarFollowInverter, put = Set_VarFollowInverter))  bool VarFollowInverter;
	//__declspec (property (get = Get_Fkvarlimit, put = Set_Maxkvar))  double kvarLimit;
	//__declspec (property (get = Get_Fkvarlimitneg, put = Set_Maxkvarneg))  double kvarLimitneg;
	//__declspec (property (get = Get_FpctkWIn, put = Set_pctkWIn))  double PctkWIn;
	//__declspec (property (get = Get_InverterLosses))  double kWInverterLosses;
	//__declspec (property (get = Get_kWChDchLosses))  double kWChDchLosses;
	//__declspec (property (get = Get_DCkW))  double DCkW;
	//__declspec (property (get = Get_VminPu))  double MinModelVoltagePU;
	//__declspec (property (put = Set_pf_wp_nominal))  double pf_wp_nominal;
	//__declspec (property (get = Get_Pmin))  double Pmin;
	//__declspec (property (get = Get_Pmax))  double Pmax;
	//__declspec (property (get = Get_qMaxInj))  double qMaxInj;
	//__declspec (property (get = Get_qMaxAbs))  double qMaxAbs;
	//__declspec (property (get = Get_acVmin))  double acVmin;
	//__declspec (property (get = Get_acVmax))  double acVmax;
	//__declspec (property (get = Get_acVnom))  double acVnom;
	//__declspec (property (get = Get_pMaxUnderPF))  double pMaxUnderPF;
	//__declspec (property (get = Get_pMaxOverPF))  double pMaxOverPF;
	//__declspec (property (get = Get_pMaxCharge))  double pMaxCharge;
	//__declspec (property (get = Get_sMaxCharge))  double apparentPowerChargeMax;

	//__declspec (property (get = Get_PresentkW, put = Set_PresentkW ) )  double PresentkW;
	//__declspec (property (get = Get_Presentkvar, put = Set_Presentkvar ) )  double Presentkvar;
	//__declspec (property (get = Get_PresentkV, put = Set_PresentkV ) )  double PresentkV;

	//__declspec (property (get = get_PFNominal, put = Set_PowerFactor ) )  double Powerfactor;
	//__declspec (property (get = get_fState, put = Set_StorageState ) )  int StorageState;
	//__declspec (property (get = get_FpctkWout, put = Set_pctkWOut ) )  double PctkWOut;
	//__declspec (property (get = get_Fpctkvarout, put = Set_pctkvarOut ) )  double PctkVarOut;

	//__declspec (property (get = Get_kWTotalLosses ) )  double kWTotalLosses;
	//__declspec (property (get = Get_kWIdlingLosses ) )  double kWIdlingLosses;

	//__declspec (property (get = get_Vminpu ) )  double MinModelVoltagePU;
	TStorageObj(TDSSClass* ParClass);
	TStorageObj(String ClassName);
	TStorageObj();
};
extern TStorageObj* ActiveStorageObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace Storage

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Storage;
#endif

#endif // StorageH




