#ifndef LoadH
#define LoadH

#include "System.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LoadShape.h"
#include "GrowthShape.h"
#include "Spectrum.h"
#include "Arraydef.h"

namespace Load
{



/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

//  The load is assumed balanced over the no. of phases defined
// To model unbalanced loads, define separate single-phase loads

// IF you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation or Daily when Daily is defined
//    Daily:   Defaults to No variation  (i.e. multiplier = 1.0 always)
//    Dutycycle: Defaults to Daily shape
//    Growth: Circuit default growth factor

/*   Change Log
    10/7/99  RCD  Tightened up default load shape code and corrected comments
    11-22-99  Fixed bug in CalcYPrimMatrix
    12-5-99  Changed PQ load limits to 95% to 105%
    1-8-99 Made PQ load limits a variable (added vminpu, vmaxpu properties)
    2-1-00 Added normal and emergency voltage ratings to override system settings when <> 0
    4-17-00 Added XFKVA and AllocationFactor properties and associated code.
    8-26-00 Added exemption from LoadMult code (exemptfromLDcurve)
    9-19-00 Changed the way UE and EEN computed for low voltage
    10-25-00  Added Spectrum
    10-27-00 Implemented Harmonic current  and Harmonic Mode stuff
    3-27-01 Added check to prevent divide by zero on calculation of PFNominal
    5-17-01 Moved Spectrum definition back to PCElement
    2-18-03 Changed Rneut default to -1
            Created a Y_Series with small conductances on the diagonal so that calcV doesn't fail
    9-23-08 Added CVR Factors
    10-14-08 Added kWh and Cfactor. Modified behavior of AllocationFactor to simplify State Estimation
    4/1/14 Added Vlowpu property to make solution converge better at very low voltages
    1/7/15 Added puXHarm and XRHarm properties to help model motor load for harmonic studies
    3/16/16 Added PFSpecified to account for problems when UseActual is specified and no Qmult specified
    1/10/18 Celso/Paulo mods for low-voltage transition for Model 5
*/
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TLoad : public PCClass::TPCClass
{
	friend class TLoadObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
	void InterpretConnection(const String s);
	void SetNcondsForConnection();
protected:
	virtual int MakeLike(const String OtherLoadName);
	void DefineProperties();  // Add Properties of this class to propName
public:
	TLoad();
	virtual ~TLoad();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TLoadObj : public PCElement::TPCElement
{
	friend class TLoad;
public:
	typedef PCElement::TPCElement inherited;	
//private:
	bool PFChanged;
	double FAllocationFactor;   // For all types of allocation
	double FkVAAllocationFactor;   // for connected kVA specification
	double FConnectedkVA;
	double FkWh;
	double FkWhDays;
	double FCFactor;   // For kWh billed spec
	double FAvgkW;
	std::vector <complex> FPhaseCurr; // this is the intermediate current computed in each power flow mode.
	std::vector <double> HarmAng;  // References for Harmonics mode
	std::vector <double> HarmMag;
	double LastGrowthFactor;
	int LastYear;   // added FOR speedup so we don't have to search FOR growth factor a lot
	double LoadFundamental;
	int LoadSolutionCount;
	int OpenLoadSolutionCount;
	double RandomMult;
	Ucomplex::complex ShapeFactor;
	double varBase;  // Base vars per phase
	double varNominal;
	double VBase;  // Base volts suitable for computing currents
	double VBase105;
	double VBase95;
	double VBaseLow;
	double WNominal;  // Nominal Watts per phase
	Ucomplex::complex Yeq;   // at nominal
	Ucomplex::complex Yeq105;
	Ucomplex::complex Yeq105I; // ***Added by Celso & Paulo
	Ucomplex::complex Yeq95;
	Ucomplex::complex Yneut;
	Ucmatrix::TcMatrix* YPrimOpenCond;  // To handle cases where one conductor of load is open
	double YQFixed;   // Fixed value of y FOR type 7 load
	double FpuXHarm;   // puX for harmonics solution.
	double FXRHarmRatio;   // X/R at fundamental

        // formerly private, now read-only properties for COM access
	double FpuMean;
	double FpuStdDev;
	double FCVRwattFactor;
	double FCVRvarFactor;
	double Vmaxpu;
	double VminEmerg;  // overrides system settings IF <> 0.0
	double VminNormal;
	double Vminpu;
	double VLowpu; // below this voltage, resorts to linear @ Yeq

        // For interpolating currents between VbaseLow and Vbase95
	Ucomplex::complex ILow;
	Ucomplex::complex I95;
	Ucomplex::complex IBase; // at nominal  ***Added by Celso & Paulo
	Ucomplex::complex M95; // complex slope of line between Low and 95
	Ucomplex::complex M95I; // complex slope of line between Low and 95 for Constant I  **Added by Celso & Paulo
	bool ExemptFromLDCurve;
	bool FIXED;   // IF Fixed, always at base value
	bool ShapeIsActual;
	bool PFSpecified;  // Added 3-16-16 to fix problem with UseActual
	int FnZIPV;
	bool AllTerminalsClosed();
	void CalcDailyMult(double hr);
	void CalcDutyMult(double hr);
	void CalcInjCurrentArray(int ActorID);
	void CalcLoadModelContribution(int ActorID);
	void CalcVTerminalPhase(int ActorID, bool CheckAlg = false);
	void CalcYearlyMult(double hr);
	void CalcCVRMult(double hr);
	void CalcYPrimMatrix(Ucmatrix::TcMatrix* Ymatrix, int ActorID);
	void DoConstantILoad(int ActorID);
	void DoConstantPQLoad(int ActorID);
	void DoConstantZLoad(int ActorID);
	void DoFixedQ(int ActorID);
	void DoFixedQZ(int ActorID);
	void DoHarmonicMode(int ActorID);
	void DoCVRModel(int ActorID);
	void DoZIPVModel(int ActorID);
	void SetZIPVSize(int n);
	void DoMotorTypeLoad(int ActorID);
	double GrowthFactor(int Year, int ActorID);
	void StickCurrInTerminalArray(Ucomplex::pComplexArray TermArray, const Ucomplex::complex& Curr, int i);
	Ucomplex::complex InterpolateY95_YLow(double Vmag);
	Ucomplex::complex InterpolateY95I_YLow(double Vmag); // ***Added by Celso & Paulo
	bool Get_Unserved(int ActorID);
	void Set_kVAAllocationFactor(double Value);
	void Set_ConnectedkVA(double Value);
	void ComputeAllocatedLoad();
        /*Set kWh properties ...*/
	void Set_CFactor(double Value);
	void Set_kWh(double Value);
	void Set_kWhDays(double Value);
	void Set_AllocationFactor(double Value);
	void SetkWkvar(double PkW, double Qkvar);
	void set_nZIPV(int Value);
protected:
	virtual void GetTerminalCurrents(Ucomplex::pComplexArray Curr, int ActorID);
public:
	int Connection;  /*     0 = line-neutral; 1=Delta*/
	String DailyShape;         // Daily (24 HR) load shape
	LoadShape::TLoadShapeObj* DailyShapeObj;  // Daily load Shape FOR this load
	String DutyShape;         // Duty cycle load shape FOR changes typically less than one hour
	LoadShape::TLoadShapeObj* DutyShapeObj;  // Shape for this load
	double EEN_Factor;         // is overloaded  Factor is the amount of overload
	String GrowthShape;         // (year, Multiplier from previous year)
	GrowthShape::TGrowthShapeObj* GrowthShapeObj;  // Shape for this Growth  Curve
	bool HasBeenAllocated;
	double kWBase;
	double kVABase;
	double kWref;
	double kVARref;
	double kvarBase;
	double kVLoadBase;
	int LoadClass;
	int NumCustomers;
	int LoadSpecType;  // 0=kW, PF;  1= kw, kvar;  2=kva, PF
	double PFNominal;
	double Rneut;
	double UE_Factor;  // These are set to > 0 IF a line in the critical path
	double Xneut;  // Neutral impedance
	String YearlyShape;  // ='fixed' means no variation  exempt from variation
	LoadShape::TLoadShapeObj* YearlyShapeObj;  // Shape for this load
	String CVRshape;
	LoadShape::TLoadShapeObj* CVRShapeObj;
	Arraydef::pDoubleArray ZIPV;  // Made public 5-20-2013
	double puSeriesRL;
	double RelWeighting;
	int FLoadModel;   // Variation with voltage
	          /*  1 = Constant kVA (P,Q always in same ratio)
             2 = Constant impedance
             3 = Constant P, Quadratic Q (Mostly motor)
             4 = Linear P, Quadratic Q  (Mixed motor/resistive Use this for CVR studies
             5 = Constant |I|
             6 = Constant P (Variable); Q is fixed value (not variable)
             7 = Constant P (Variable); Q is fixed Z (not variable)
             8 = ZIPV (3 real power coefficients, 3 reactive, Vcutoff)
          */
	TLoadObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TLoadObj();
	bool Get_ExceedsNormal(int ActorID);
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void InitHarmonics(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	void SetNominalLoad(int ActorID);
	void Randomize(int Opt);
                  // 0 = reset to 1.0
                  // 1 = Gaussian around mean and std Dev
                  // 2 = uniform
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	void UpdateVoltageBases();
	//************************************* returns to properties***************************************************
	double get_FAllocationFactor();
	double get_FCFactor();
	double get_FkWhDays();
	double get_FkWh();
	double get_FConnectedkVA();
	double get_FkVAAllocationFactor();
	double get_FpuMean();
	double get_FpuStdDev();
	double get_FCVRwattFactor();
	double get_FCVRvarFactor();
	double get_Vmaxpu();
	double get_VminEmerg();
	double get_VminNormal();
	double get_Vminpu();
	bool get_ExemptFromLDCurve();
	bool get_FIXED();
	int get_FnZIPV();
	bool get_PFSpecified();

//        Property ExceedsNormal[ActorID:Integer] :Boolean Read Get_ExceedsNormal(ActorID:Integer);

        /*Allocate load from connected kva or kWh billing*/
	TLoadObj(DSSClass::TDSSClass* ParClass);
	TLoadObj(String ClassName);
	TLoadObj();
};
extern TLoadObj* ActiveLoadObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace Load

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Load;
#endif

#endif // LoadH




