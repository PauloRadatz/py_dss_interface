#ifndef InvControlH
#define InvControlH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "Bus.h"
#include "PCElement.h"
#include "PVsystem.h"
#include "Storage.h"
#include "StorageVars.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "XYcurve.h"
#include "Dynamics.h"
#include "PointerList.h"
#include "mathutil.h"
#include "InvDynamics.h"
#include "d2c_structures.h"
#include <queue>

class TIEEE1547Controller;

namespace InvControl
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2023,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
  A InvControl is a control element that is connected to a terminal of another
  circuit element and sends kW and/or kvar signals to a set of PVSystem objects it controls

  A InvControl is defined by a New command:

  New InvControl.Name=myname PVSystemList = (pvsystem1  PVSystem ...)

Notes:
  WGS (11/26/2012): Using dynamic arrays for many private variables in this unit.
  Although dynamic arrays begin at 0 (by definition in Delphi),
  this unit is using 1 to numberelements in all for loops - the 0th
  element is un-used (except for Strings) in this unit.
  All dynamic arrays are set to length numberelements+1 in the appropriate dimension.
  All dynamic arrays are Finalize'd in the destroy procedure.

  // Updated 9/24/2015 to allow for simultaneous modes and additional functionality
*/
enum ERateofChangeMode {Inactive,
                        LPF,
                        RISEFALL };

class TRollAvgWindow : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
	std::deque<double> sample;
	std::deque<double> sampletime;
	double runningsumsample;
	double runningsumsampletime;
	int BufferLength;
	bool bufferfull;
	double Get_AvgVal();
	double Get_AccumSec();
	void Set_BuffLength(int Value);
public:
	TRollAvgWindow();
	virtual ~TRollAvgWindow();
	void Add(double IncomingSampleValue, double IncomingSampleTime, double VAvgWindowLengthSec);
};

class TInvControl : public ControlClass::TControlClass
{
	friend class TInvControlObj;
	friend class TRollAvgWindow;
public:
	typedef ControlClass::TControlClass inherited;	
private:
	DSSClass::TDSSClass* XY_CurveClass;
protected:
	void DefineProperties();
	virtual int MakeLike(const String InvControlName);
public:
	TInvControl();
	virtual ~TInvControl();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);
	XYCurve::TXYcurveObj* GetXYCurve(const String CurveName, int InvControlMode);
	void UpdateAll(int ActorID);
};

class TInvControlObj : public ControlElem::TControlElem
{
	friend class TInvControl;
	friend class ::TIEEE1547Controller;	
public:
	typedef ControlElem::TControlElem inherited;	
private:
	int ControlMode;
	int CombiControlMode;
	int ControlActionHandle;
	
	CktElement::TDSSCktElement* MonitoredElement;  // First DER element for now (the first element from ControlledElement TPointerList)

	struct TInvVars
	{
		int		CondOffset,											// Offset for monitored terminal
				NPhasesDER,
				NCondsDER,
				FPendingChange,
				FActiveVVCurve;
		std::vector <complex>	cBuffer;							// Complex array buffer
		PCElement::TPCElement*	ControlledElement;
		std::vector <double>	FVpuSolution;

		double					FAvgpVpuPrior,
								FAvgpDRCVpuPrior,
								FPresentVpu,
								FPresentDRCVpu,
								QDesiredVV,							// volt-var new set-point
								QDesiredWP,							// watt-pf new set-point
								QDesiredWV,							// watt-var new set-point
								QDesiredAVR,
								QOld,
								QOldVV,
								QOldAVR,
								QOldDRC,
								QOldVVDRC,
								QDesiredDRC,						//dynamic reactive power new set-point
								QDesiredVVDRC,
								QHeadRoom,							/*Variables of functions that CONTROL reactive power*/
								QHeadRoomNeg,
								PBase,
								Qoutputpu,
								QoutputVVpu,
								QoutputDRCpu,
								QoutputVVDRCpu,
								QoutputAVRpu,
								QDesireEndpu,						// Q value used in the convergency algorithm
								QDesireVVpu,						// Q desired caculated in volt-var curve
								QDesireWPpu,						// Q desired caculated in watt-pf curve
								QDesireWVpu,						// Q desired caculated in watt-var curve
								QDesireDRCpu,						// Q desired from the DRC equation
								QDesireAVRpu,
								QDesireLimitedpu,					// Calculates possible Q considering kVA (watt priority) and kvarlimit limits
								QDesireOptionpu,					// Calculates Q Limit considering LPF and RF
								PLimitEndpu,
								PLimitVWpu,
								PLimitLimitedpu,
								PLimitOptionpu,
								deltaVDynReac,
								PLimitVW,
								POldVWpu,
								FdeltaQFactor,
								FdeltaPFactor,
								DeltaV_old,
								FDRCRollAvgWindowpu,
								priorRollAvgWindow,
								priorDRCRollAvgWindow,
								FPriorWattspu,
								FPriorwatts,
								FPriorPLimitOptionpu,
								FPriorQDesireOptionpu,
								kW_out_desiredpu,
								kW_out_desired,
								FPriorvarspu,
								FPriorvars,
		/*Flags used to record function states. They are interval variables of DER*/
								FVVOperation,
								FVWOperation,
								FDRCOperation,
								FVVDRCOperation,
								FWPOperation,
								FWVOperation,
								FAVROperation,
								FVBase,
								FpresentkW,
								FkVArating,
								Fpresentkvar,
								Fkvarlimit,
								FkvarLimitNeg,
								FCurrentkvarLimit,
								FCurrentkvarLimitNeg,
								FDCkWRated,							// Pmpp for PVSystem, kWRated for Storage
								FpctDCkWRated,						// pctPmpp for PVSystem, pctkWRated for Storage
								FEffFactor,
								FDCkW,								// PanelkW for PVSystem, DCkW for Storage
								DQDV,								// Active voltage regulation (AVR)
								Fv_setpointLimited,					// Active voltage regulation (AVR)
								FAvgpAVRVpuPrior;					// Active voltage regulation (AVR)

		TRollAvgWindow* FRollAvgWindow;
		TRollAvgWindow*			FDRCRollAvgWindow;
		bool					FlagChangeCurve,
								FFlagVWOperates,					// Flag enabled when volt-watt Pdesired is less than 1. So volt-watt algorithm starts to work
								FVarFollowInverter,
								FInverterON,
								FPPriority;

		TPICtrl					PICtrl;
	};
	
      /*Variables for voltages*/
	double FVreg;

	int FVpuSolutionIdx;
      /*Variables for convergence process*/
	double FdeltaQ_factor;
	double FdeltaP_factor;

	double FVoltageChangeTolerance;
	double FVarChangeTolerance;
	double FActivePChangeTolerance;

      /*Variables of DER element*/
	PointerList::TPointerList* FDERPointerList;
	int FListSize;
	TStringList FDERNameList;

      /*Variables for monitored Bus/buses*/
	TStringList FMonBusesNameList;
	int FMonBusesPhase;
	bool FUsingMonBuses;
	std::vector <std::string>  FMonBuses;
	int FMonBusesIndex;
	std::vector <double> FMonBusesVbase;
	std::vector < vector <int> > FMonBusesNodes;

      /*Variables for LPF and RF options*/
	ERateofChangeMode RateofChangeMode;
	double FLPFTau;
	double FRiseFallLimit;
      /*Variables of the smart inverter functions*/
	int FVoltage_CurveX_ref;  // valid values are 0: = Vref (rated), 1:= avg
	String FReacPower_ref;
	int FVoltwattYAxis; // 1 = %Pmpp, 0 = %Available power

      // volt-var
	int Fvvc_curve_size; // length of the individual curve
	XYCurve::TXYcurveObj* Fvvc_curve;
	String Fvvc_curvename;
	double Fvvc_curveOffset;
	XYCurve::TXYcurveObj* Fvvc_curve2;

	double FVAvgWindowLengthSec; // rolling average window length in seconds
	
	int FRollAvgWindowLength;

	String FRollAvgWindowLengthIntervalUnit;

      // watt-pf
	int Fwattpf_curve_size;
	XYCurve::TXYcurveObj* Fwattpf_curve;
	String Fwattpf_curvename;
	double pf_wp_nominal;

      // watt-var
	int Fwattvar_curve_size;
	XYCurve::TXYcurveObj* Fwattvar_curve;
	String Fwattvar_curvename;

      // DRC
	double FDbVMin;
	double FDbVMax;
	double FArGraLowV;
	double FArGraHiV;
	
	int FDRCRollAvgWindowLength;
	String FDRCRollAvgWindowLengthIntervalUnit;
	
	double FDRCVAvgWindowLengthSec; // rolling average window length in seconds

      // volt-watt
	int Fvoltwatt_curve_size;
	XYCurve::TXYcurveObj* Fvoltwatt_curve;
	String Fvoltwatt_curvename;

      // volt-watt (charging)
	int FvoltwattCH_curve_size;
	XYCurve::TXYcurveObj* FvoltwattCH_curve;
	String FvoltwattCH_curvename;
	int CtrlModel;
	std::vector<TInvVars> CtrlVars;
      // Active voltage regulation (AVR)
	double Fv_setpoint;

      /*Functions and Procedures*/
	void Set_PendingChange(int Value, int DevIndex);
	int Get_PendingChange(int DevIndex);
	int InterpretAvgVWindowLen(const String s);
	int InterpretDRCAvgVWindowLen(const String s);
	String ReturnElementsList();
	void UpdateInvControl(int i, int ActorID);
	void UpdateDERParameters(int i);
	void CalcVoltWatt_watts(int j, int ActorID);
	void CalcQVVcurve_desiredpu(int j, int ActorID);
	void CalcQWPcurve_desiredpu(int j, int ActorID);
	void CalcQWVcurve_desiredpu(int j, int ActorID);
	void CalcQDRC_desiredpu(int j, int ActorID);
	void CalcQAVR_desiredpu(int j, int ActorID);
	void Check_Qlimits(int j, double Q, int ActorID);
	void Check_Qlimits_WV(int j, double Q, int ActorID);
	void Calc_PQ_WV(int j, int ActorID);
	void Calc_QHeadRoom(int j, int ActorID);
	void CalcVoltVar_vars(int j, int ActorID);
	void CalcAVR_vars(int j, int ActorID);
	void CalcWATTPF_vars(int j, int ActorID);
	void CalcWATTVAR_vars(int j, int ActorID);
	void CalcDRC_vars(int j, int ActorID);
	void CalcVVDRC_vars(int j, int ActorID);
	void CalcLPF(int m, String powertype, double LPF_desiredpu, int ActorID);
	void CalcRF(int m, String powertype, double RF_desiredpu, int ActorID);
	void Calc_PBase(int j, int ActorID);
	void Check_Plimits(int j, double P, int ActorID);
	void CalcPVWcurve_limitpu(int j, int ActorID);
	void GetmonVoltage(int ActorID, double& Vpresent, int i, double BaseKV, int connection);
	void Change_deltaQ_factor(int ActorID, int j);
	void Change_deltaP_factor(int ActorID, int j);
    int NextDeltaPhase(int iphs, int i);

    protected:
	virtual void Set_Enabled(bool Value);
public:
	//System::TMemoryManagerState MyMemoryManagerState;
	TInvControlObj(DSSClass::TDSSClass* ParClass, const String InvControlName);
	virtual ~TInvControlObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a InvControl

      // Sample control quantities and set action times in Control Queue
	virtual void sample(int ActorID);

      // do the action that is pending from last sample
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);
	virtual void Reset(int ActorID);  // Reset to initial defined state
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injection currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	bool MakeDERList();
	virtual String GetPropertyValue(int Index);

	int get_ControlMode();
	int get_CombiControlMode();
	int get_FVoltage_CurveX_ref();
	int get_FRollAvgWindowLength();
	int get_FDRCRollAvgWindowLength();
	int get_FMonBusesPhase();
	int get_FVoltwattYAxis();

	TStringList* get_FDERNameList();
	TStringList* get_FMonBusesNameList();

	String get_Fvvc_curvename();
	String get_Fvoltwatt_curvename();
	String get_FvoltwattCH_curvename();
	String get_FReacPower_ref();

	double get_Fvvc_curveOffset();
	double get_FDbVMin();
	double get_FDbVMax();
	double get_FArGraLowV();
	double get_FArGraHiV();
	double get_FdeltaQ_factor();
	double get_FVoltageChangeTolerance();
	double get_FVarChangeTolerance();
	double get_FLPFTau();
	double get_FRiseFallLimit();
	double get_FdeltaP_factor();
	double get_FActivePChangeTolerance();
	double get_Fv_setpoint();
	pDoubleArray get_FMonBusesVbase();




      /*Properties that give access to this Class variables*/
      //property RateofChangeMode          : String             read
      //property EventLog                  : String             read

      // Need to include the new modes here
	TInvControlObj(DSSClass::TDSSClass* ParClass);
	TInvControlObj(String ClassName);
	TInvControlObj();
};
extern TInvControlObj* ActiveInvControlObj;


}  // namespace InvControl

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace InvControl;
#endif

#endif // InvControlH





