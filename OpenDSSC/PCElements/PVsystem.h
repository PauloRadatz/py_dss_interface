#ifndef PVSystemH
#define PVSystemH

#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"

#include "PVSystemUserModel.h"
#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LoadShape.h"
#include "TempShape.h"
#include "XYcurve.h"
#include "Spectrum.h"
#include "Arraydef.h"
#include "Dynamics.h"
#include "DSSClassDefs.h"
#include "InvDynamics.h"
#include "mathutil.h"


namespace PVSystem
{



/*
  ----------------------------------------------------------
  Copyright (c) 2011-2023, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*   Change Log

    1/28/2011 Created from Storage Model


  To Do:
    Make connection to User model
    Yprim for various modes
    Define state vars and dynamics mode behavior
    Complete Harmonics mode algorithm (generator mode is implemented)
*/
/*
  The PVsystem element is essentially a generator that consists of a PV panel and an inverter.

  The PVsystem element can also produce or absorb vars within the kVA rating of the inverter.
  // WGS: Updated 9/24/2015 to allow for simultaneous modes and additional functionality in the InvControl.

  09/11/2022 Compatibility with dynamics simulation added
  10/28/2022 Grid forming inverter capabilities added
*/

//  The PVsystem element is assumed balanced over the no. of phases defined
const int NumPVSystemRegisters = 6;    // Number of energy meter registers
const int NumPVSystemVariables = 22;    // Includes dynamics state variables - added on 09/15/2022.
const int VARMODEPF = 0;
const int VARMODEKVAR = 1;

/*Struct to pass basic data to user-written DLLs*/
#pragma pack (push, 1)


struct TPVSystemVars
{
	double FkVArating;
	double kVPVSystemBase;
	double RThev;
	double XThev;
	double VThevHarm;  /*Thevinen equivalent voltage mag  for Harmonic model*/
	double VthevmagDyn;  /*Thevinen equivalent voltage mag  reference for Dynamics model*/
	double ThetaHarm;  /*Thevinen equivalent  angle reference for Harmonic model*/
	double ThetaDyn;  /*Thevinen equivalent  angle reference for Dynamics model*/
	double InitialVAngle;  /*initial terminal voltage angle when entering dynamics mode*/
	double EffFactor;
	double TempFactor;
	double PanelkW; //computed
	double FTemperature;
	double FPmpp;
	double FpuPmpp;
	double FIrradiance;
	double MaxDynPhaseCurrent;
	double Fkvarlimit; //maximum kvar output of the PVSystem (unsigned)
	double Fkvarlimitneg;
	// Variables set from InvControl. They are results of monitor in mode 3
	double Vreg; // will be set from InvControl or ExpControl
	double Vavg;
	double VVOperation;
	double VWOperation;
	double DRCOperation;
	double VVDRCOperation;
	double WPOperation;
	double WVOperation;
	//        kW_out_desired   :Double;

		/*32-bit integers*/
	int NumPhases;   /*Number of phases*/
	int NumConductors;/*Total Number of conductors (wye-connected will have 4)*/
	int Conn;   // 0 = wye; 1 = Delta
	bool P_priority;  // default False // added 10/30/2018
	bool PF_Priority;  // default False // added 1/29/2019
};
#pragma pack (pop)


// ===========================================================================================

class TPVSystem : public PCClass::TPCClass
{
	friend class TPVsystemObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
	void InterpretConnection(const String s);
	void SetNcondsForConnection();
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherPVsystemObjName);
public:
	String RegisterNames[6/*# range 1..NumPVSystemRegisters*/];
	TPVSystem();
	virtual ~TPVSystem();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	void ResetRegistersAll();
	void SampleAll(int ActorID);
	void UpdateAll();
};

// ===========================================================================================

class TPVsystemObj : public PCElement::TPCElement
{
	friend class TPVsystem;
public:
	typedef PCElement::TPCElement inherited;	
//private:
	Ucomplex::complex Yeq;   // at nominal
	Ucomplex::complex YEQ_Min;   // at Vmin
	Ucomplex::complex YEQ_Max;   // at VMax
	Ucomplex::complex PhaseCurrentLimit;
	Ucomplex::complex Zthev;
	double LastThevAngle;
	bool DebugTrace;
	int PVSystemSolutionCount;
	double PVSystemFundamental;  /*Thevinen equivalent voltage mag and angle reference for Harmonic model*/
	bool PVsystemObjSwitchOpen;
	bool FirstSampleAfterReset;
	bool PFSpecified;
	bool kvarSpecified;
	bool ForceBalanced;
	bool CurrentLimited;
	double kvar_out;
	double kW_out;
	double kvarRequested;
	double Fpf_wp_nominal;
	double kWRequested;
	int FvarMode;
	double FpctCutIn;
	double FpctCutOut;
	bool FVarFollowInverter;
	double CutInkW;
	double CutOutkW;
	bool FInverterON;
	double FpctPminNoVars;
	double FpctPminkvarLimit;
	double PminNoVars;
	double PminkvarLimit;
	double pctR;
	double pctX;
	int OpenPVSystemSolutionCount;
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
	double TShapeValue;
	System::TTextRec Tracefile;
	PVsystemUserModel::TPVsystemUserModel* UserModel;   /*User-Written Models*/
	double varBase; // Base vars per phase
	double VBase;  // Base volts suitable for computing currents
	double VBaseMax;
	double VBaseMin;
	double Vmaxpu;
	double Vminpu;
	Ucmatrix::TcMatrix* YPrimOpenCond;
	bool FVWMode; //boolean indicating if under volt-watt control mode from InvControl (not ExpControl)
	bool FVVMode; //boolean indicating if under volt-var mode from InvControl
	bool FWVMode; //boolean indicating if under watt-var mode from InvControl
	bool FWPMode; //boolean indicating if under watt-pf mode from InvControl
	bool FDRCMode; //boolean indicating if under DRC mode from InvControl
	bool FAVRMode;

	int FVWYAxis;  // integer value indicating that whether y-axis of watts is in %Pmpp or %PAvailable
                                  // 1 = %Pmpp, 0=%PAvailable.  Default is 1 such that pctPmpp user-settable
                                  // property will correctly operate on Pmpp (NOT PAvailable)
	void CalcDailyMult(double hr);  // now incorporates DutyStart offset
	void CalcDutyMult(double hr);
	void CalcYearlyMult(double hr);  // now incorporates DutyStart offset
	void CalcDailyTemperature(double hr);
	void CalcDutyTemperature(double hr);
	void CalcYearlyTemperature(double hr);
	void ComputePanelPower();
	void ComputeInverterPower();
	void ComputekWkvar();
	void CalcPVSystemModelContribution(int ActorID);   // This is where the power gets computed
	void CalcInjCurrentArray(int ActorID);
        /*PROCEDURE CalcVterminal;*/
	void CalcVTerminalPhase(int ActorID);
	void CalcYPrimMatrix(Ucmatrix::TcMatrix* Ymatrix, int ActorID);
	void DoConstantPQPVsystemObj(int ActorID);
	void DoConstantZPVsystemObj(int ActorID);
	void DoDynamicMode(int ActorID);
	void DoHarmonicMode(int ActorID);
	void DoUserModel(int ActorID);
	void Integrate(int reg, double Deriv, double Interval);
	void SetDragHandRegister(int reg, double Value);
	void StickCurrInTerminalArray(Ucomplex::pComplexArray TermArray, const Ucomplex::complex& Curr, int i);
	void WriteTraceRecord(const String s);

        // PROCEDURE SetKWandKvarOut;
	void UpdatePVSystem();    // Update PVSystem elements based on present kW and IntervalHrs variable
	double Get_PresentkW();
	double Get_Presentkvar();
	double Get_PresentkV();
	double Get_PresentIrradiance();
	void Set_PresentkV(double Value);
	void Set_Presentkvar(double Value);
	void Set_PresentkW(double Value);
	void Set_PowerFactor(double Value);
	void Set_PresentIrradiance(double Value);
	void Set_pf_wp_nominal(double Value);
	void Set_kVARating(double Value);
	void Set_Pmpp(double Value);
	void Set_puPmpp(double Value);
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

	//int Get_VWYAxis();
	//void Set_VWYAxis(int Value);
	void kWOut_Calc();

	// get implementation for properties
	double Get_FkVArating();
	double Get_FPmpp();
	double Get_FpuPmpp();
	double Get_Fkvarlimit();
	double Get_FShapefactorRe();

protected:
	virtual void Set_ConductorClosed(int Index, int ActorID, bool Value);
	virtual void GetTerminalCurrents(Ucomplex::pComplexArray Curr, int ActorID);
public:
	TPVSystemVars PVSystemVars;
	InvDynamics::TInvDynamicVars myDynVars;
	double CurrentkvarLimit;
	double CurrentkvarLimitNeg;
	int Connection;  /*0 = line-neutral; 1=Delta*/
	String DailyShape;  // Daily (24 HR) PVSystem element irradiance shape
	LoadShape::TLoadShapeObj* DailyShapeObj;  // Daily PVSystem element irradianceShape for this load
	String DutyShape;  // Duty cycle irradiance shape for changes typically less than one hour
	LoadShape::TLoadShapeObj* DutyShapeObj;  // irradiance Shape for this PVSystem element
	double DutyStart; // starting time offset into the DutyShape [hrs] for this PVsystem
	String YearlyShape;  //
	LoadShape::TLoadShapeObj* YearlyShapeObj;  // Yearly irradiance Shape for this PVSystem element
	String DailyTShape;
	TempShape::TTShapeObj* DailyTShapeObj;
	String DutyTShape;
	TempShape::TTShapeObj* DutyTShapeObj;
	String YearlyTShape;
	TempShape::TTShapeObj* YearlyTShapeObj;
	String InverterCurve;
	XYCurve::TXYcurveObj* InverterCurveObj;
	String Power_TempCurve;
	XYCurve::TXYcurveObj* Power_TempCurveObj;
	bool kvarLimitSet;
	bool kvarLimitNegSet;
	std::vector<TPICtrl> PICtrl;

	int FClass;
	int VoltageModel;   // Variation with voltage
	double PFNominal;
	double Registers[6/*# range 1..NumPVSystemRegisters*/];
	double Derivatives[6/*# range 1..NumPVSystemRegisters*/];
	TPVsystemObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TPVsystemObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual int NumVariables();
	virtual void GetAllVariables(Arraydef::pDoubleArray States);
	virtual double Get_Variable(int i);
	virtual void Set_Variable(int i, double Value);
	virtual String VariableName(int i);
	bool Get_InverterON();
	void Set_InverterON(bool Value);
	bool Get_VarFollowInverter();
	void Set_VarFollowInverter(bool Value);
	void Set_Maxkvar(double Value);
	void Set_Maxkvarneg(double Value);
	void SetNominalPVSystemOuput(int ActorID);
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
	void DoGFM_Mode(int ActorID);
	void GetCurrents(pComplexArray Curr, int ActorID);
	bool CheckAmpsLimit(int ActorID);
	bool CheckOLInverter(int ActorID);

	//--------------------------------------------------------------------------------------------------------
	
	double get_PFNominal();
	double get_Vminpu();
	double get_Fkvarlimitneg();

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
	bool Get_CIMDynamicMode();

	//--------------------------------------------------------------------------------------------------------
	// Functions for NCIM solution algorithm
	void DoPQBusNCIM(int ActorID, int i, complex V, complex Curr);

	TPVsystemObj(DSSClass::TDSSClass* ParClass);
	TPVsystemObj(String ClassName);
	TPVsystemObj();
};
extern TPVsystemObj* ActivePVsystemObj;

// ===========================================================================================


}  // namespace PVSystem

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PVSystem;
#endif

#endif // PVSystemH




