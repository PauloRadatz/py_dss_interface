#ifndef EnergyMeterH
#define EnergyMeterH

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "MeterClass.h"
#include "MeterElement.h"
#include "CktElement.h"
#include "PDElement.h"
#include "Arraydef.h"
#include "PointerList.h"
#include "CktTree.h"
#include "Ucomplex.h"
#include "Feeder.h"
#include "Load.h"
#include "generator.h"
#include "XYcurve.h"
#include "Command.h"
#include "d2c_structures.h"





namespace EnergyMeter
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
     This class of device accumulates the energy of the voltage and current in the
     terminal of the device to which it is connected.

     It is an intelligent energy meter capable of measuring losses of all
     devices within its "zone".

     The Zone is determined automatically after a circuit change.  The Zone starts on the
     opposite side of the branch on which the meter is located and continues in the same
     direction through the network until
       a) an open point is encountered
       b) an open terminal or switch is encountered
       c) another energy meter is encountered
       d) a branch that is already included in a zone is encountered

     It keeps track of kwh, kvarh, UE,  EEN, Losses, etc., having registers FOR each
     of these quantities.

     In EEN/UE calculations, line overload takes precedence.

     If the Max Zone kW limits are specified, then these replace the line overload UE/EEN numbers.
     These limits were added so that the user can override line limits in cases
     such as networks where it is difficult to judge the UE from the individual
     line limits.

     Only the maximum |kVA| overload is accumulated, not all.  Loads downline from
     an overload are marked WITH a factor representing the degree of overload.  This
     is used to compute EEN/UE FOR loads.

     FOR low voltages, the full kW FOR loads below the emergency min voltage are counted.
     The EEN is proportioned based on how low the voltage is.

     Emergency min voltage must be less than normal min voltage.

*/

/*                 CHANGE LOG

8-3-99  Added Option property
        Revised EEN/UE computation to do either total or excess
8-4-99 Save always rewrites file now and returns file name.

11-11-99 Fixed bug in Take sample to use the maxvalue of the overload_EEN

1-4-99  Modified tree checking to avoid picking up the same load more than once
        Fixed bugs in sampling load EEN/UE
        Modified Overload UE; added kwnormal, kwemerg properties for whole zone

1-28-00 Changed to derived from Meter Element
2-2-00  Trapezoidal Integration option
4-14-00 Added load allocation algorithm
4-17-00 Removed shunt capacitors from meter zones
5-3-00  Corrected Zone kW, kvar accumulation to be actual power not target power
5-29-00 Fixed problem with Nphases not being set right for 1-phase devices.
6-15-01 Added Zonelist and LocalOnly options
7/6/01  Added Voltage Only option for Load UE calcs.
7/19/01 Added Totalizer Function for meterclass
7/24/01 Added Generator registers and code for adding generators to zone lists.
        Changed to use zone loads and gens even if local only. If you only want the local
        measurements, specify a null zone manually.
8/2/01  Fixed hole in Local only options.
4/29/03 Added ReduceZone Function
2/7/07  Fixed overload formulas
9/18/08 Added load loss and no load loss registers  and aux registers
11/8/08 Revamped TakeSample to fix bugs with Demand Interval reporting
8/8/13  Added initial reliability calcs
3/27/2018 Corrected SAIDI calcs
*/

/*$WARN UNIT_PLATFORM OFF*/
const int NumEMVbase = 7;
const int NumEMRegisters = 32 + 5 * NumEMVbase;   // Total Number of energy meter registers
    /*Fixed Registers*/
const int Reg_kWh = 1 - 1;
const int Reg_kvarh = 2 - 1;
const int Reg_MaxkW = 3 - 1;
const int Reg_MaxkVA = 4 - 1;
const int Reg_ZonekWh = 5 - 1;
const int Reg_Zonekvarh = 6 - 1;
const int Reg_ZoneMaxkW = 7 - 1;
const int Reg_ZoneMaxkVA = 8 - 1;
const int Reg_OverloadkWhNorm = 9 - 1;    // Max overload
const int Reg_OverloadkWhEmerg = 10 - 1;
const int Reg_LoadEEN = 11 - 1;
const int Reg_LoadUE = 12 - 1;  // Energy served below normal voltage
const int Reg_ZoneLosseskWh = 13 - 1;
const int Reg_ZoneLosseskvarh = 14 - 1;
const int Reg_LossesMaxkW = 15 - 1;
const int Reg_LossesMaxkvar = 16 - 1;
const int Reg_LoadLosseskWh = 17 - 1;
const int Reg_LoadLosseskvarh = 18 - 1;
const int Reg_NoLoadLosseskWh = 19 - 1;
const int Reg_NoLoadLosseskvarh = 20 - 1;
const int Reg_MaxLoadLosses = 21 - 1;
const int Reg_MaxNoLoadLosses = 22 - 1;
const int Reg_LineLosseskWh = 23 - 1;
const int Reg_TransformerLosseskWh = 24 - 1;
const int Reg_LineModeLineLoss = 25 - 1;    // for 3-phase feeder lines
const int Reg_ZeroModeLineLoss = 26 - 1;
const int Reg_3_phaseLineLoss = 27 - 1;
const int Reg_1_phaseLineLoss = 28 - 1;
const int Reg_GenkWh = 29 - 1;
const int Reg_Genkvarh = 30 - 1;
const int Reg_GenMaxkW = 31 - 1;
const int Reg_GenMaxkVA = 32 - 1;
const int Reg_VBaseStart = 32 - 1;  // anchor for the voltage base loss registers
typedef double TRegisterArray[231/*# range 1..NumEMRegisters*/];

    //  --------- Feeder Section Definition -----------

struct TFeederSection
{
	int OCPDeviceType;  // 1=Fuse; 2=Recloser; 3=Relay
	int NCustomers;
	int NBranches;
	int TotalCustomers;
	int SeqIndex;  // index of pdelement with OCP device at head of section
	double AverageRepairTime;
	double SectFaultRate;
	double SumFltRatesXRepairHrs;
	double SumBranchFltRates;
};
typedef TFeederSection FeederSectionArray;
typedef FeederSectionArray* pFeederSections;   // Dummy dimension
    // 0 is an OK index RCD 5/14/2021
    //  --------- Feeder Section Definition -----------

class TSystemMeter : public System::TObject
{
	friend class TEnergyMeter;
public:
	typedef TObject inherited;	
private:
	double kWh;
	double dkWh;
	double kvarh;
	double dkvarh;
	double peakkW;
	double peakkVA;
	double Losseskwh;
	double dLosseskWh;
	double Losseskvarh;
	double dlosseskvarh;
	double PeakLosseskW;
	bool FirstSampleAfterReset;
	bool This_Meter_DIFileIsOpen;
	System::TTextRec SystemDIFile;
	Ucomplex::complex cPower;
	Ucomplex::complex cLosses;
	void Clear();
	void Integrate(double& reg, double Value, double& Deriv, int ActorID);
	void WriteRegisters(System::TTextRec& f, int ActorID);
	void WriteRegisterNames(System::TTextRec& f);
protected:
	void OpenDemandIntervalFile(int ActorID);
	void WriteDemandIntervalData(int ActorID);
	void CloseDemandIntervalFile(int ActorID);
	void AppendDemandIntervalFile(int ActorID);
public:
	void TakeSample(int ActorID);
	void Reset();
	void Save(int ActorID);
	TSystemMeter();
	virtual ~TSystemMeter();
};    // derive strait from base class

class TEnergyMeter : public MeterClass::TMeterClass
{
	friend class TEnergyMeterObj;
public:
	typedef MeterClass::TMeterClass inherited;	
//private:
 //****       GeneratorClass        : TGenerator;
	bool FSaveDemandInterval;
	bool FDI_Verbose;
	System::TTextRec FOverLoadFile;
	System::TTextRec FVoltageFile;
	void ProcessOptions(const String Opts);
	void Set_SaveDemandInterval(int ActorID, bool Value);
	bool Get_SaveDemandInterval(int ActorID);
	void CreateMeterTotals(int ActorID);
	void CreateFDI_Totals(int ActorID);
	void ClearDI_Totals();
	void WriteTotalsFile(int ActorID);
	void OpenOverloadReportFile(int ActorID);
	void OpenVoltageReportFile(int ActorID);
	void WriteOverloadReport(int ActorID);
	void WriteVoltageReport(int ActorID);
	void InterpretRegisterMaskArray(TRegisterArray Mask, int ActorID);
	void Set_DI_Verbose(int ActorID, bool Value);
	bool Get_DI_Verbose(int ActorID);
protected:
	void DefineProperties();
	virtual int MakeLike(const String EnergyMeterName);
	void SetHasMeterFlag(int ActorID);
public:
	TRegisterArray DI_RegisterTotals;
	String DI_Dir;
	System::TTextRec FDI_Totals;
	System::TTextRec FMeterTotals;
	TSystemMeter* SystemMeter;
	bool Do_OverloadReport;
	bool Do_VoltageExceptionReport;
	bool OverLoadFileIsOpen;
	bool VoltageFileIsOpen;
	TEnergyMeter();
	virtual ~TEnergyMeter();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	void ResetMeterZonesAll(int ActorID);
	virtual void ResetAll(int ActorID);  // Reset all meters in active circuit to zero
	virtual void SampleAll(int ActorID);   // Force all meters in active circuit to sample
	virtual void SaveAll(int ActorID);
	void AppendAllDIFiles(int ActorID);
	void OpenAllDIFiles(int ActorID);
	void CloseAllDIFiles(int ActorID);
};

class TEnergyMeterObj : public MeterElement::TMeterElement
{
	friend class TEnergyMeter;
public:
	typedef MeterElement::TMeterElement inherited;	
//private:
	bool FirstSampleAfterReset;
	bool ExcessFlag;
	bool ZoneIsRadial;
	bool VoltageUEOnly;
	bool LocalOnly;
	bool HasFeeder;
	bool FLosses;
	bool FLineLosses;
	bool FXfmrLosses;
	bool FSeqLosses;
	bool F3PhaseLosses;
	bool FVBaseLosses;
	TFeederObj* FeederObj;   // not used at present
	pStringArray DefinedZoneList;
	int DefinedZoneListSize;

       /*Limits on the entire load in the zone for networks where UE cannot be determined
        by the individual branches*/
	double MaxZonekVA_Norm;
	double MaxZonekVA_Emerg;

       /*Voltage bases in the Meter Zone*/
	vector <double> VBaseTotalLosses;    // allocated array
	vector <double> VBaseLineLosses;
	vector <double> VBaseLoadLosses;
	vector <double> VBaseNoLoadLosses;
	vector <double> VBaseLoad;
	vector <double> VBaseList;
	int VBaseCount;
	int MaxVBaseCount;

       /* Arrays for phase voltage report  */
	vector <double> VphaseMax;
	vector <double> VPhaseMin;
	vector <double> VPhaseAccum;
	vector <int> VPhaseAccumCount;
	System::TTextRec VPhase_File;
	bool VPhaseReportFileIsOpen;

       /*Demand Interval File variables*/
	System::TTextRec DI_File;
	bool This_Meter_DIFileIsOpen;
	void Integrate(int reg, double Deriv, double Interval, int ActorID);
	void SetDragHandRegister(int reg, double Value);
	double Accumulate_Load(Load::TLoadObj* pLoad, double& TotalZonekW, double& TotalZonekvar, double& TotalLoad_EEN, double& TotalLoad_UE, int ActorID);
	void Accumulate_Gen(Generator::TGeneratorObj* pGen, double& TotalZonekW, double& TotalZonekvar, int ActorID);
	void CalcBusCoordinates(CktTree::TCktTreeNode* StartBranch, int FirstCoordRef, int SecondCoordRef, int LineCount);
	int AddToVoltBaseList(int BusRef, int ActorID);
	String MakeDIFileName(int ActorID);
	String MakeVPhaseReportFileName(int ActorID);
	void AssignVoltBaseRegisterNames();

    // Not used   Procedure MakeFeederObj;
    // Not used   Procedure RemoveFeederObj;
	void TotalupDownstreamCustomers();
protected:
	void OpenDemandIntervalFile(int ActorID);
	void WriteDemandIntervalData(int ActorID);
	void CloseDemandIntervalFile(int ActorID);
	void AppendDemandIntervalFile(int ActorID);
public:
	TBytesStream* DI_MHandle;
	TBytesStream* PHV_MHandle;
	std::vector< std::string> RegisterNames;
	bool FPhaseVoltageReport;
	CktTree::TCktTree* BranchList;      // Pointers to all circuit elements in meter's zone
	PointerList::TPointerList* SequenceList;  // Pointers to branches in sequence from meter to ends
	PointerList::TPointerList* LoadList;  // Pointers to Loads in the Meter zone to aid reliability calcs
	TRegisterArray Registers;
	TRegisterArray Derivatives;
	TRegisterArray TotalsMask;

        // Reliability data for Head of Zone
	double SAIFI;     // For this Zone - based on number of customers
	double SAIFIkW;     // For this Zone - based on kW load
	double SAIDI;
	double CAIDI;
	double CustInterrupts;
	bool AssumeRestoration;

        // Source reliability
	double Source_NumInterruptions; // Annual interruptions for upline circuit
	double Source_IntDuration; // Aver interruption duration of upline circuit
	int SectionCount;
	int ActiveSection;  // For COM interface to index into FeederSections array
	vector <FeederSectionArray> FeederSections;
	vector<String> ZonePCE;
	TEnergyMeterObj(DSSClass::TDSSClass* ParClass, const String EnergyMeterName);
	virtual ~TEnergyMeterObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model, reset nphases
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); //Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	void ResetRegisters();
	virtual void TakeSample(int ActorID);
	void SaveRegisters(int ActorID);
	void MakeMeterZoneLists(int ActorID);
	void ZoneDump(int ActorID);
	void InterpolateCoordinates();
	void EnableFeeder();
	void AllocateLoad(int ActorID);
	void ReduceZone(int ActorID);  // Reduce Zone by eliminating buses and merging lines
	void SaveZone(const String DirName);
	void GetPCEatZone();
	void CalcReliabilityIndices(bool AssumeRestoration_in, int ActorID);
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TEnergyMeterObj(DSSClass::TDSSClass* ParClass);
	TEnergyMeterObj(String ClassName);
	TEnergyMeterObj();
};
extern TEnergyMeterObj* ActiveEnergyMeterObj;
  /* RegisterNameList      :TCommandList; */


}  // namespace EnergyMeter

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace EnergyMeter;
#endif

#endif // EnergyMeterH





