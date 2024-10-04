
#pragma hdrstop

#include "EnergyMeter.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Bus.h"
#include "mathutil.h"
#include "Ucmatrix.h"
#include "Utilities.h"
#include "PCElement.h"
#include "StackDef.h"
#include "Circuit.h"
#include "Line.h"
#include "LineUnits.h"
#include "ReduceAlgs.h"
#include <math.h>
#include "MemoryMap_lib.h"
#include "dirsep.h"

using namespace std;
using namespace Arraydef;
using namespace Bus;
using namespace Circuit;
using namespace CktElement;
using namespace CktTree;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Feeder;
using namespace Generator;
using namespace Line;
using namespace LineUnits;
using namespace Load;
using namespace MemoryMap_lib;
using namespace MeterClass;
using namespace MeterElement;
using namespace PCElement;
using namespace PDELement;
using namespace ParserDel;
using namespace PointerList;
using namespace ReduceAlgs;
using namespace StackDef;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XYCurve;
using namespace mathutil;
using namespace Utilities;

namespace EnergyMeter
{

TEnergyMeterObj::TEnergyMeterObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TEnergyMeterObj::TEnergyMeterObj(String ClassName) : inherited(ClassName) {}
TEnergyMeterObj::TEnergyMeterObj() {}


TEnergyMeterObj* ActiveEnergyMeterObj = nullptr;

//{$UNDEF DEBUG}
const int NumPropsThisClass = 24;
double Delta_Hrs = 0.0;
   // adjacency lists for PC and PD elements at each bus, built for faster searches
TAdjArray BusAdjPC; // also includes shunt PD elements
TAdjArray BusAdjPD;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/*#inline*/
int jiIndex(int i, int j)
{
	int result = 0;
	result = (j - 1) * 3 + i;
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure FOR all EnergyMeter objects

TEnergyMeter::TEnergyMeter()
 : FSaveDemandInterval(false),
			FDI_Verbose(false),
			SystemMeter(nullptr),
			Do_OverloadReport(false),
			Do_VoltageExceptionReport(false),
			OverLoadFileIsOpen(false),
			VoltageFileIsOpen(false)
{
	int Idx = 0;
	Class_Name = "EnergyMeter";
	DSSClassType = DSSClassType + ENERGY_METER;
	ActiveElement = 0;

     /*Initialice demand interval options to off*/
	OverLoadFileIsOpen = false;
	VoltageFileIsOpen = false;
	Do_OverloadReport = false;
	Do_VoltageExceptionReport = false;
	DI_Dir = "";
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);

//{$IFDEF MSWINDOWS}
 //****    GeneratorClass := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('generator'));
//{$ENDIF}
	SystemMeter = new TSystemMeter();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TEnergyMeter::~TEnergyMeter()
{
	delete SystemMeter;

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TEnergyMeter::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "element";
	PropertyName[2 - 1] = "terminal";
	PropertyName[3 - 1] = "action";
	PropertyName[4 - 1] = "option";
	PropertyName[5 - 1] = "kVAnormal";
	PropertyName[6 - 1] = "kVAemerg";
	PropertyName[7 - 1] = "peakcurrent";
	PropertyName[8 - 1] = "Zonelist";
	PropertyName[9 - 1] = "LocalOnly";
	PropertyName[10 - 1] = "Mask";
	PropertyName[11 - 1] = "Losses";
	PropertyName[12 - 1] = "LineLosses";
	PropertyName[13 - 1] = "XfmrLosses";
	PropertyName[14 - 1] = "SeqLosses";
	PropertyName[15 - 1] = "3phaseLosses";
	PropertyName[16 - 1] = "VbaseLosses"; // segregate losses by voltage base
	PropertyName[17 - 1] = "PhaseVoltageReport"; // Compute Avg phase voltages in zone
	PropertyName[18 - 1] = "Int_Rate";
	PropertyName[19 - 1] = "Int_Duration";
	PropertyName[20 - 1] = "SAIFI";    // Read only
	PropertyName[21 - 1] = "SAIFIkW";    // Read only
	PropertyName[22 - 1] = "SAIDI";    // Read only
	PropertyName[23 - 1] = "CAIDI";    // Read only
	PropertyName[24 - 1] = "CustInterrupts";    // Read only

/*     PropertyName^[11 - 1] := 'Feeder';  **** removed - not used*/
	PropertyHelp[1 - 1] = "Name (Full Object name) of element to which the monitor is connected.";
	PropertyHelp[2 - 1] = "Number of the terminal of the circuit element to which the monitor is connected. "
	           "1 or 2, typically.";
	PropertyHelp[3 - 1] = String("{Clear (reset) | Save | Take | Zonedump | Allocate | Reduce} ") + CRLF
	           + CRLF
	           + "(A)llocate = Allocate loads on the meter zone to match PeakCurrent."
	           + CRLF
	           + "(C)lear = reset all registers to zero"
	           + CRLF
	           + "(R)educe = reduces zone by merging lines (see Set Keeplist & ReduceOption)"
	           + CRLF
	           + "(S)ave = saves the current register values to a file."
	           + CRLF
	           + "   File name is \"MTR_metername.CSV\"."
	           + CRLF
	           + "(T)ake = Takes a sample at present solution"
	           + CRLF
	           + "(Z)onedump = Dump names of elements in meter zone to a file"
	           + CRLF
	           + "   File name is \"Zone_metername.CSV\".";
	PropertyHelp[4 - 1] = String("Enter a string ARRAY of any combination of the following. Options processed left-to-right:") + CRLF
	           + CRLF
	           + "(E)xcess : (default) UE/EEN is estimate of energy over capacity "
	           + CRLF
	           + "(T)otal : UE/EEN is total energy after capacity exceeded"
	           + CRLF
	           + "(R)adial : (default) Treats zone as a radial circuit"
	           + CRLF
	           + "(M)esh : Treats zone as meshed network (not radial)."
	           + CRLF
	           + "(C)ombined : (default) Load UE/EEN computed from combination of overload and undervoltage."
	           + CRLF
	           + "(V)oltage : Load UE/EEN computed based on voltage only."
	           + CRLF
	           + CRLF
	           + "Example: option=(E, R)";
	PropertyHelp[5 - 1] = "Upper limit on kVA load in the zone, Normal configuration. Default is 0.0 (ignored). "
	           "Overrides limits on individual lines for overload EEN. "
	           "With \"LocalOnly=Yes\" option, uses only load in metered branch.";
	PropertyHelp[6 - 1] = "Upper limit on kVA load in the zone, Emergency configuration. Default is 0.0 (ignored). "
	           "Overrides limits on individual lines for overload UE. "
	           "With \"LocalOnly=Yes\" option, uses only load in metered branch.";
	PropertyHelp[7 - 1] = "ARRAY of current magnitudes representing the peak currents measured at this location "
	           "for the load allocation function.  Default is (400, 400, 400). Enter one current for each phase";
	PropertyHelp[8 - 1] = String("ARRAY of full element names for this meter's zone.  Default is for meter to find it's own zone. " "If specified, DSS uses this list instead.  Can access the names in a single-column text file.  Examples: ") + CRLF
	           + CRLF
	           + "zonelist=[line.L1, transformer.T1, Line.L3 - 1] "
	           + CRLF
	           + "zonelist=(file=branchlist.txt)";
	PropertyHelp[9 - 1] = "{Yes | No}  Default is NO.  If Yes, meter considers only the monitored element "
	           "for EEN and UE calcs.  Uses whole zone for losses.";
	PropertyHelp[10 - 1] = "Mask for adding registers whenever all meters are totalized.  Array of floating point numbers "
	           "representing the multiplier to be used for summing each register from this meter. "
	           "Default = (1, 1, 1, 1, ... ).  You only have to enter as many as are changed (positional). "
	           "Useful when two meters monitor same energy, etc.";
	PropertyHelp[11 - 1] = "{Yes | No}  Default is YES. Compute Zone losses. If NO, then no losses at all are computed.";
	PropertyHelp[12 - 1] = "{Yes | No}  Default is YES. Compute Line losses. If NO, then none of the losses are computed.";
	PropertyHelp[13 - 1] = "{Yes | No}  Default is YES. Compute Transformer losses. If NO, transformers are ignored in loss calculations.";
	PropertyHelp[14 - 1] = "{Yes | No}  Default is YES. Compute Sequence losses in lines and segregate by line mode losses and zero mode losses.";
	PropertyHelp[15 - 1] = "{Yes | No}  Default is YES. Compute Line losses and segregate by 3-phase and other (1- and 2-phase) line losses. ";
	PropertyHelp[16 - 1] = "{Yes | No}  Default is YES. Compute losses and segregate by voltage base. If NO, then voltage-based tabulation is not reported.";
	PropertyHelp[17 - 1] = "{Yes | No}  Default is NO.  Report min, max, and average phase voltages for the zone and tabulate by voltage base. "
	           "Demand Intervals must be turned on (Set Demand=true) and voltage bases must be defined for this property to take effect. "
	           "Result is in a separate report file.";
	PropertyHelp[18 - 1] = "Average number of annual interruptions for head of the meter zone (source side of zone or feeder).";
	PropertyHelp[19 - 1] = "Average annual duration, in hr, of interruptions for head of the meter zone (source side of zone or feeder).";
	PropertyHelp[20 - 1] = "(Read only) Makes SAIFI result available via return on query (? energymeter.myMeter.SAIFI.";
	PropertyHelp[21 - 1] = "(Read only) Makes SAIFIkW result available via return on query (? energymeter.myMeter.SAIFIkW.";
	PropertyHelp[22 - 1] = "(Read only) Makes SAIDI result available via return on query (? energymeter.myMeter.SAIDI.";
	PropertyHelp[23 - 1] = "(Read only) Makes CAIDI result available via return on query (? energymeter.myMeter.CAIDI.";
	PropertyHelp[24 - 1] = "(Read only) Makes Total Customer Interrupts value result available via return on query (? energymeter.myMeter.CustInterrupts.";
/**** Not used in present version
      PropertyHelp[11]:= '{Yes/True | No/False}  Default is NO. If set to Yes, a Feeder object is created corresponding to ' +
                         'the energymeter.  Feeder is enabled if Radial=Yes; diabled if Radial=No.  Feeder is ' +
                         'synched automatically with the meter zone.  Do not create feeders for zones in meshed transmission systems.';
*****/
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TEnergyMeter::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TEnergyMeterObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TEnergyMeter::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	bool DoRecalc = false;

  // continue parsing WITH contents of Parser
  // continue parsing WITH contents of Parser
	ActiveEnergyMeterObj = (TEnergyMeterObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement((TDSSCktElement*) ActiveEnergyMeterObj);
	result = 0;
	DoRecalc = false;
	/*# with ActiveEnergyMeterObj do */
	{
		auto with0 = ActiveEnergyMeterObj;
		with0->MeteredElementChanged = false;
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while(Param.size() > 0)
		{
			if(ParamName.size() == 0)
				++ParamPointer;
			else
				ParamPointer = CommandList.Getcommand(ParamName);
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))
				with0->Set_PropertyValue(ParamPointer,Param);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 520);
				break;
				case 	1:
				with0->ElementName = LowerCase(Param);
				break;
				case 	2:
				with0->MeteredTerminal = Parser[ActorID]->MakeInteger_();
				break;  /*Actions*/
				case 	3:
				{
					Param = LowerCase(Param);
					switch(Param[0])
					{
						case 	L'a':
						with0->AllocateLoad(ActorID);
						break;
						case 	L'c':
						with0->ResetRegisters();
						break;
						case 	L'r':
						with0->ReduceZone(ActorID);
						break;
						case 	L's':
						with0->SaveRegisters(ActorID);
						break;
						case 	L't':
						with0->TakeSample(ActorID);
						break;
						case 	L'z':
						with0->ZoneDump(ActorID);
						break;
						default:
						  ;
						break;
					}
				}
				break;
				case 	4:
				ProcessOptions(Param);
				break;
				case 	5:
				with0->MaxZonekVA_Norm = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				with0->MaxZonekVA_Emerg = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				Parser[ActorID]->ParseAsVector(with0->Fnphases, &(with0->SensorCurrent[0]));
				break;   // Inits to zero
				case 	8:
				InterpretAndAllocStrArray(Param, with0->DefinedZoneListSize, with0->DefinedZoneList);
				break;
				case 	9:
				with0->LocalOnly = InterpretYesNo(Param);
				break;
				case 	10:
				InterpretRegisterMaskArray(with0->TotalsMask, ActorID);
				break;
				case 	11:
				with0->FLosses = InterpretYesNo(Param);
				break;
				case 	12:
				with0->FLineLosses = InterpretYesNo(Param);
				break;
				case 	13:
				with0->FXfmrLosses = InterpretYesNo(Param);
				break;
				case 	14:
				with0->FSeqLosses = InterpretYesNo(Param);
				break;
				case 	15:
				with0->F3PhaseLosses = InterpretYesNo(Param);
				break;
				case 	16:
				with0->FVBaseLosses = InterpretYesNo(Param);
				break;
				case 	17:
				with0->FPhaseVoltageReport = InterpretYesNo(Param);
				break;
				case 	18:
				with0->Source_NumInterruptions = Parser[ActorID]->MakeDouble_();
				break; // Annual interruptions for upline circuit
				case 	19:
				with0->Source_IntDuration = Parser[ActorID]->MakeDouble_();
				break; // hours
				case 	20:
				with0->Set_PropertyValue(20,"");
				break;  // placeholder, do nothing just throw value away if someone tries to set it.
				case 	21:
				with0->Set_PropertyValue(21,"");
				break;  // placeholder, do nothing just throw value away if someone tries to set it.
				case 	22:
				with0->Set_PropertyValue(22,"");
				break;  // placeholder, do nothing just throw value away if someone tries to set it.
				case 	23:
				with0->Set_PropertyValue(23,"");
				break;  // placeholder, do nothing just throw value away if someone tries to set it.
				case 	24:
				with0->Set_PropertyValue(24,"");
				break;  // placeholder, do nothing just throw value away if someone tries to set it.
				           /****11: HasFeeder := InterpretYesNo(Param); ***/
				default:
				inherited::ClassEdit(ActiveEnergyMeterObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	1: case 2:
				{
					with0->MeteredElementChanged = true;
					DoRecalc = true;
				}
				break;
             /****11: If HasFeeder Then DoRecalc := True Else RemoveFeederObj; */
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		if(DoRecalc)
			with0->RecalcElementData(ActorID);   // When some basic data have changed
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TEnergyMeter::MakeLike(const String EnergyMeterName)
{
	int result = 0;
	TEnergyMeterObj* OtherEnergyMeter = nullptr;
	int i = 0;
	result = 0;
   /*See IF we can find this EnergyMeter name in the present collection*/
	OtherEnergyMeter = ((TEnergyMeterObj*) Find(EnergyMeterName));
	if(OtherEnergyMeter != nullptr)
		/*# with ActiveEnergyMeterObj do */
		{
			auto with0 = ActiveEnergyMeterObj;
			int stop = 0;
			with0->Set_NPhases(OtherEnergyMeter->Fnphases);
			with0->Set_Nconds(OtherEnergyMeter->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherEnergyMeter->ElementName;
			with0->MeteredElement = OtherEnergyMeter->MeteredElement;  // Pointer to target circuit element
			with0->MeteredTerminal = OtherEnergyMeter->MeteredTerminal;
			with0->ExcessFlag = OtherEnergyMeter->ExcessFlag;
			with0->MaxZonekVA_Norm = OtherEnergyMeter->MaxZonekVA_Norm;
			with0->MaxZonekVA_Emerg = OtherEnergyMeter->MaxZonekVA_Emerg;

       // Reliability
			with0->Source_NumInterruptions = OtherEnergyMeter->Source_NumInterruptions;
			with0->Source_IntDuration = OtherEnergyMeter->Source_IntDuration;
			FreeStringArray(with0->DefinedZoneList, with0->DefinedZoneListSize);
			with0->DefinedZoneListSize = OtherEnergyMeter->DefinedZoneListSize;
			with0->DefinedZoneList = AllocStringArray(with0->DefinedZoneListSize);
       // Copy Strings over (actually incr ref count on string)
			for(stop = with0->DefinedZoneListSize, i = 1; i <= stop; i++)
			{
				with0->DefinedZoneList[i] = OtherEnergyMeter->DefinedZoneList[i];
			}
			with0->LocalOnly = OtherEnergyMeter->LocalOnly;
			with0->VoltageUEOnly = OtherEnergyMeter->VoltageUEOnly;

       /*Boolean Flags*/
			with0->FLosses = OtherEnergyMeter->FLosses;
			with0->FLineLosses = OtherEnergyMeter->FLineLosses;
			with0->FXfmrLosses = OtherEnergyMeter->FXfmrLosses;
			with0->FSeqLosses = OtherEnergyMeter->FSeqLosses;
			with0->F3PhaseLosses = OtherEnergyMeter->F3PhaseLosses;
			with0->FVBaseLosses = OtherEnergyMeter->FVBaseLosses;
			with0->FPhaseVoltageReport = OtherEnergyMeter->FPhaseVoltageReport;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				if(i < 20)
         // Skip Read Only properties
					with0->Set_PropertyValue(i,OtherEnergyMeter->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in EnergyMeter MakeLike: \"") + EnergyMeterName
	           + "\" Not Found.", 521);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TEnergyMeter::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TEnergyMeter.Init", -1);
	result = 0;
	return result;
}

/*--------------------------------------------------------------------------*/  // Force all EnergyMeters in the circuit to reset their meter zones

void TEnergyMeter::ResetMeterZonesAll(int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
	TDSSCktElement* pCktElement = nullptr;
	TPDElement* PDElem = nullptr;
	TPCElement* pcElem = nullptr;
	int i = 0;
	/*# with ActiveCircuit[ActorID] do */
	{
		
		int stop = 0;
		if(ActiveCircuit[ActorID]->EnergyMeters.get_myNumList() == 0)
			return;  // Do not do anything

    // initialize the Checked Flag FOR all circuit Elements
		pCktElement = (TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get_First();
		while((pCktElement != nullptr))
		{
			/*# with pCktElement do */
			{
				auto with1 = pCktElement;
				int stop = 0;
				with1->Checked = false;
				with1->IsIsolated = true;
				for(stop = with1->Get_NTerms(), i = 1; i <= stop; i++)
				{
					with1->Terminals[i - 1].Checked = false;
				}
			}
			pCktElement = (TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get_Next();
		}

    /*Clear some things that will be set by the Meter Zone*/
		PDElem = (TPDElement*) ActiveCircuit[ActorID]->PDElements.Get_First();
		while(PDElem != nullptr)
		{
			PDElem->MeterObj = nullptr;
			PDElem->SensorObj = nullptr;
			PDElem->ParentPDElement = nullptr;
			PDElem = (TPDElement*) ActiveCircuit[ActorID]->PDElements.Get_Next();
		}
		pcElem = (TPCElement*) ActiveCircuit[ActorID]->PCElements.Get_First();
		while(pcElem != nullptr)
		{
			pcElem->MeterObj = nullptr;
			pcElem->SensorObj = nullptr;
			pcElem = (TPCElement*) ActiveCircuit[ActorID]->PCElements.Get_Next();
		}

    // Set up the bus adjacency lists for faster searches to build meter zone lists.
		BuildActiveBusAdjacencyLists(BusAdjPD, BusAdjPC, ActorID); 

    //Set Hasmeter flag for all cktelements
		SetHasMeterFlag(ActorID);
		SensorClass[ActorID]->SetHasSensorFlag();  // Set all Sensor branch flags, too.

    // initialize the Checked Flag for all Buses
		for(stop = ActiveCircuit[ActorID]->NumBuses, i = 1; i <= stop; i++)
		{
			ActiveCircuit[ActorID]->Buses[i - 1]->BusChecked = false;
		}
		for(stop = ActiveCircuit[ActorID]->EnergyMeters.get_myNumList(), i = 1; i <= stop; i++)
		{
			mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get(i);
			mtr->MakeMeterZoneLists(ActorID);
		}
		FreeAndNilBusAdjacencyLists(BusAdjPD, BusAdjPC);
	}
}

/*--------------------------------------------------------------------------*/  // Force all EnergyMeters in the circuit to reset

void TEnergyMeter::ResetAll(int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
	String CasePath;
	if(DIFilesAreOpen[ActorID])
		CloseAllDIFiles(ActorID);
	if(FSaveDemandInterval)
	{
		CasePath = OutputDirectory[ActorID] + ActiveCircuit[ActorID]->get_FCaseName();
          /*Make directories to save data*/
		if(!DirectoryExists(CasePath))
		{
			try
			{
				MkDir(CasePath);
			}
			catch (std::exception &e)
			{
				DoSimpleMsg(String("Error making  Directory: \"") + CasePath
	           + "\". "
	           + (std::string) e.what(), 522);
			}
		}
		DI_Dir = CasePath + DIRSEP_STR "DI_yr_" + Trim(IntToStr(ActiveCircuit[ActorID]->Solution->get_Fyear()));
		if(!DirectoryExists(DI_Dir))
		{
			try
			{
				MkDir(DI_Dir);
			}
			catch (std::exception &e)
			{
				DoSimpleMsg(String("Error making Demand Interval Directory: \"") + DI_Dir
	           + "\". "
	           + (std::string) e.what(), 523);
			}
		}
		CreateFDI_Totals(ActorID);
	}
	mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
	while(mtr != nullptr)
	{
		mtr->ResetRegisters();
		mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_Next();
	}
	SystemMeter->Reset();


      // Reset Generator Objects, too
	GeneratorClass->ResetRegistersAll(ActorID);
	StorageClass[ActorID]->ResetRegistersAll();
//      Storage2Class[ActorID]->ResetRegistersAll();
	PVSystemClass[ActorID]->ResetRegistersAll();
//      PVSystem2Class[ActorID]->ResetRegistersAll();
}

/*--------------------------------------------------------------------------*/  // Force all EnergyMeters in the circuit to take a sample

void TEnergyMeter::SampleAll(int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
	int i = 0;
	mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
	while(mtr != nullptr)
	{
		if(mtr->Get_Enabled())
			mtr->TakeSample(ActorID);
		mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_Next();
	}
	SystemMeter->TakeSample(ActorID);
	if(FSaveDemandInterval)  /*Write Totals Demand interval file*/
	{
		int stop = 0;
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			WriteintoMem(TDI_MHandle[ActorID], with0->DynaVars.dblHour);
		}
		for (i = 0; i < NumEMRegisters; i++)
		{
			WriteintoMem(TDI_MHandle[ActorID], DI_RegisterTotals[i]);
		}
		std::string s(1, (Char)10);
		WriteintoMemStr(TDI_MHandle[ActorID], s) ;
		ClearDI_Totals();
		if(OverLoadFileIsOpen)
			WriteOverloadReport(ActorID);
		if(VoltageFileIsOpen)
			WriteVoltageReport(ActorID);
	}

      // Sample Generator ans Storage Objects, too
	GeneratorClass->SampleAll(ActorID);
	StorageClass[ActorID]->SampleAll(ActorID);  // samples energymeter part of storage elements (not update)
//      Storage2Class[ActorID]->SampleAll(ActorID);
	PVSystemClass[ActorID]->SampleAll(ActorID);
//      PVSystem2Class[ActorID]->SampleAll(ActorID);
}

/*--------------------------------------------------------------------------*/  // Force all EnergyMeters in the circuit to take a sample

void TEnergyMeter::SaveAll(int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
	mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
	while(mtr != nullptr)
	{
		if(mtr->Get_Enabled())
			mtr->SaveRegisters(ActorID);
		mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_Next();
	}
	SystemMeter->Save(ActorID);
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TEnergyMeter Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TEnergyMeterObj::TEnergyMeterObj(TDSSClass* ParClass, const String EnergyMeterName)
 : inherited(ParClass),
			FirstSampleAfterReset(false),
			ExcessFlag(false),
			ZoneIsRadial(false),
			VoltageUEOnly(false),
			LocalOnly(false),
			HasFeeder(false),
			FLosses(false),
			FLineLosses(false),
			FXfmrLosses(false),
			FSeqLosses(false),
			F3PhaseLosses(false),
			FVBaseLosses(false),
			FeederObj(nullptr),
			DefinedZoneListSize(0),
			MaxZonekVA_Norm(0.0),
			MaxZonekVA_Emerg(0.0),
			VPhaseReportFileIsOpen(false),
			This_Meter_DIFileIsOpen(false),
			DI_MHandle(nullptr),
			PHV_MHandle(nullptr),
			FPhaseVoltageReport(false),
			BranchList(nullptr),
			SequenceList(nullptr),
			LoadList(nullptr),
			SAIFI(0.0),
			SAIFIkW(0.0),
			SAIDI(0.0),
			CAIDI(0.0),
			CustInterrupts(0.0),
			Source_NumInterruptions(0.0),
			Source_IntDuration(0.0),
			SectionCount(0),
			ActiveSection(0)
{
	int i = 0;
	int stop = 0;
	VPhaseAccumCount.clear();
	RegisterNames.resize(NumEMRegisters);
	Set_Name(LowerCase(EnergyMeterName));
	DSSObjType = ParClass->DSSClassType; //ENERGY_METER;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors in base class
	ExcessFlag = true;  // Default to Excess energy FOR UE
	ElementName = String("Vsource.") + ((TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get(1))->get_Name(); // Default to first circuit element (source)
	MeteredElement = nullptr;
	BranchList = nullptr;  // initialize to NIL, set later when inited
	SequenceList = NULL;
	LoadList = NULL;
	This_Meter_DIFileIsOpen = false;
	VPhaseReportFileIsOpen = false;
	InitPropertyValues(0);
	DefinedZoneList = NULL;
	VBaseTotalLosses.clear();
	VBaseLineLosses.clear();
	VBaseLoadLosses.clear();
	VBaseNoLoadLosses.clear();
	VBaseLoad.clear();
	VBaseList.clear();
	VBaseCount = 0;
	MaxVBaseCount = 0;
	VphaseMax.clear();
	VPhaseMin.clear();
	VPhaseAccum.clear();
	AssumeRestoration = false;

     // Max zone kW limits ignored unless the user provides a rating
	MaxZonekVA_Norm = 0.0;
	MaxZonekVA_Emerg = 0.0;

     // Zone reliability variables
	SAIFI = 0.0;     // For this Zone
	SAIFIkW = 0.0;
	SAIDI = 0.0;
	CAIDI = 0.0;
	CustInterrupts = 0.0;
	Source_NumInterruptions = 0.0; // Annual interruptions for upline circuit
	Source_IntDuration = 0.0; // Aver interruption duration of upline circuit
	ZoneIsRadial = true;
	HasFeeder = false; // Not used; leave as False
	FeederObj = nullptr;  // initialize to not assigned
	DefinedZoneList = NULL;
	DefinedZoneListSize = 0;
	FLosses = true;   /*Loss Reporting switches*/
	FLineLosses = true;
	FXfmrLosses = true;
	FSeqLosses = true;
	F3PhaseLosses = true;
	FVBaseLosses = true;
	FPhaseVoltageReport = false;
	VBaseList.clear();
	VBaseTotalLosses.clear();
	VBaseLineLosses.clear();
	VBaseLoadLosses.clear();
	VBaseNoLoadLosses.clear();
	VBaseLoad.clear();
	VBaseCount = 0;
	MaxVBaseCount = ((NumEMRegisters - Reg_VBaseStart) / 5);
	VBaseList.resize( MaxVBaseCount + 1 );
	VBaseTotalLosses.resize(MaxVBaseCount + 1);
	VBaseLineLosses.resize(MaxVBaseCount + 1);
	VBaseLoadLosses.resize(MaxVBaseCount + 1);
	VBaseNoLoadLosses.resize(MaxVBaseCount + 1);
	VBaseLoad.resize(MaxVBaseCount + 1);

//  Init pointers to Nil before allocating
	VphaseMax.clear();
	VPhaseMin.clear();
	VPhaseAccum.clear();
	VPhaseAccumCount.clear();

     // Arrays for phase voltage report
	VphaseMax.resize(3 * MaxVBaseCount + 1);
	VPhaseMin.resize(3 * MaxVBaseCount + 1);
	VPhaseAccum.resize(3 * MaxVBaseCount + 1);
	VPhaseAccumCount.resize(3 * MaxVBaseCount + 1);
	LocalOnly = false;
	VoltageUEOnly = false;

//*************No append files by default***************************************
	OV_Append[ActiveActor] = false;
	VR_Append[ActiveActor] = false;
	DI_Append[ActiveActor] = false;
	SDI_Append[ActiveActor] = false;
	TDI_Append[ActiveActor] = false;
	SM_Append[ActiveActor] = false;
	EMT_Append[ActiveActor] = false;
	PHV_Append[ActiveActor] = false;
	FM_Append[ActiveActor] = false;

     // Set Register names  that correspond to the register quantities
	RegisterNames[1 - 1] = "kWh";
	RegisterNames[2 - 1] = "kvarh";
	RegisterNames[3 - 1] = "Max kW";
	RegisterNames[4 - 1] = "Max kVA";
	RegisterNames[5 - 1] = "Zone kWh";
	RegisterNames[6 - 1] = "Zone kvarh";
	RegisterNames[7 - 1] = "Zone Max kW";
	RegisterNames[8 - 1] = "Zone Max kVA";
	RegisterNames[9 - 1] = "Overload kWh Normal";
	RegisterNames[10 - 1] = "Overload kWh Emerg";
	RegisterNames[11 - 1] = "Load EEN";
	RegisterNames[12 - 1] = "Load UE";
	RegisterNames[13 - 1] = "Zone Losses kWh";
	RegisterNames[14 - 1] = "Zone Losses kvarh";
	RegisterNames[15 - 1] = "Zone Max kW Losses";
	RegisterNames[16 - 1] = "Zone Max kvar Losses";
	RegisterNames[17 - 1] = "Load Losses kWh";
	RegisterNames[18 - 1] = "Load Losses kvarh";
	RegisterNames[19 - 1] = "No Load Losses kWh";
	RegisterNames[20 - 1] = "No Load Losses kvarh";
	RegisterNames[21 - 1] = "Max kW Load Losses";
	RegisterNames[22 - 1] = "Max kW No Load Losses";
	RegisterNames[23 - 1] = "Line Losses";
	RegisterNames[24 - 1] = "Transformer Losses";
	RegisterNames[25 - 1] = "Line Mode Line Losses";
	RegisterNames[26 - 1] = "Zero Mode Line Losses";
	RegisterNames[27 - 1] = "3-phase Line Losses";
	RegisterNames[28 - 1] = "1- and 2-phase Line Losses";
	RegisterNames[29 - 1] = "Gen kWh";
	RegisterNames[30 - 1] = "Gen kvarh";
	RegisterNames[31 - 1] = "Gen Max kW";
	RegisterNames[32 - 1] = "Gen Max kVA";
     /*Registers for capturing losses by base voltage, names assigned later*/
	for(stop = NumEMRegisters, i = Reg_VBaseStart + 1; i < stop; i++)
	{
		RegisterNames[i] = "";
	}
	ResetRegisters();
	for(stop = NumEMRegisters, i = 1; i <= stop; i++)
	{
		TotalsMask[i - 1] = 1.0;
	}
	AllocateSensorArrays();
	for(i = 0; i < Fnphases; i++)
	{
		SensorCurrent[i] = 400.0;
	}
	FeederSections.clear();
	ActiveSection = 0;
	DI_MHandle = nullptr;
	TDI_MHandle[ActiveActor] = nullptr;
	SM_MHandle[ActiveActor] = nullptr;
	EMT_MHandle[ActiveActor] = nullptr;
	PHV_MHandle = nullptr;
	FM_MHandle[ActiveActor] = nullptr;
	ZonePCE.resize( 1 );
	ZonePCE[0] = "";

    // RecalcElementData;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TEnergyMeterObj::~TEnergyMeterObj()
{
	int i = 0;
	int stop = 0;
	VBaseList.clear();
	VBaseTotalLosses.clear();
	VBaseLineLosses.clear();;
	VBaseLoadLosses.clear();;
	VBaseNoLoadLosses.clear();;
	VBaseLoad.clear();;
     // Arrays for phase voltage report
	VphaseMax.clear();;
	VPhaseMin.clear();;
	VPhaseAccum.clear();;
	VPhaseAccumCount.clear();;
	for(stop = NumEMRegisters, i = 1; i <= stop; i++)
	{
		RegisterNames[i - 1] = "";
	}
	if(ASSIGNED(BranchList))
		delete BranchList;
	if(ASSIGNED(SequenceList))
		delete SequenceList;
	if(ASSIGNED(LoadList))
		delete LoadList;
	FreeStringArray(DefinedZoneList, DefinedZoneListSize);
	if(!(FeederSections.empty()))
		FeederSections.clear();
	if(DI_MHandle != nullptr)
		delete DI_MHandle;
	if (SM_MHandle[ActiveActor] != nullptr)
	{
		delete SM_MHandle[ActiveActor];
		SM_MHandle[ActiveActor] = nullptr; // This vector is managed by DSSGlobals and not our object, so be sure to indicate we've freed this memory.
	}
	if(EMT_MHandle[ActiveActor] != nullptr)
	{
		delete EMT_MHandle[ActiveActor];
		EMT_MHandle[ActiveActor] = nullptr; // This vector is managed by DSSGlobals and not our object, so be sure to indicate we've freed this memory.
	}
	if(PHV_MHandle != nullptr)
		delete PHV_MHandle;
	if(FM_MHandle[ActiveActor] != nullptr)
	{
		delete FM_MHandle[ActiveActor];
		FM_MHandle[ActiveActor] = nullptr; // This vector is managed by DSSGlobals and not our object, so be sure to indicate we've freed this memory.
	}
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TEnergyMeterObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	DevIndex = GetCktElementIndex(ElementName);   // Global function
	if(DevIndex > 0)  // Monitored element must already exist
	{
		MeteredElement = (TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex); // Get pointer to metered element
         /*MeteredElement must be a PDElement*/
		if(!(dynamic_cast<TPDElement*>(MeteredElement)))
		{
			MeteredElement = nullptr;   // element not found
			DoErrorMsg(String("EnergyMeter: \"") + this->get_Name() + "\"", String("Circuit Element \"") + ElementName
	           + "\" is not a Power Delivery (PD) element.", " Element must be a PD element.", 525);
			return;
		}
		if(MeteredTerminal > MeteredElement->Get_NTerms())
		{
			DoErrorMsg(String("EnergyMeter: \"") + get_Name() + "\"", String("Terminal no. \"") + IntToStr(MeteredTerminal) + "\" does not exist.", "Respecify terminal no.", 524);
		}
		else
		{
			if(MeteredElementChanged)
               // Sets name of i-th terminal's connected bus in monitor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
			{
				SetBus(1, MeteredElement->GetBus(MeteredTerminal));
				Set_NPhases(MeteredElement->Get_NPhases());
				Set_Nconds(MeteredElement->Get_NConds());
				AllocateSensorArrays();

                 // If we come through here, throw branchlist away
				if(BranchList != nullptr)
					delete BranchList;
				BranchList = nullptr;
			}

             /****If HasFeeder Then MakeFeederObj;  // OK to call multiple times  */
		}
	}
	else
	{
		MeteredElement = nullptr;   // element not found
		DoErrorMsg(String("EnergyMeter: \"") + this->get_Name() + "\"", String("Circuit Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 525);
	}
}

void TEnergyMeterObj::MakePosSequence(int ActorID)
{
	if(MeteredElement != nullptr)
	{
		SetBus(1, MeteredElement->GetBus(MeteredTerminal));
		Set_NPhases(MeteredElement->Get_NPhases());
		Set_Nconds(MeteredElement->Get_NConds());
		AllocateSensorArrays();
		if(BranchList != nullptr)
			delete BranchList;
		BranchList = nullptr;
	}
  /***If HasFeeder Then MakeFeederObj;*/
	TDSSCktElement::MakePosSequence(ActorID);
}

String TEnergyMeterObj::MakeVPhaseReportFileName(int ActorID)
{
	String result;
	result = EnergyMeterClass[ActorID]->DI_Dir
	           + DIRSEP_STR
	           + get_Name()
	           + "_PhaseVoltageReport_"
	           + IntToStr(ActorID)
	           + ".CSV";
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TEnergyMeterObj::ResetRegisters()
{
	int i = 0;
	int stop = 0;
	for(stop = NumEMRegisters, i = 1; i <= stop; i++)
	{
		Registers[i - 1] = 0.0;
	}
	for(stop = NumEMRegisters, i = 1; i <= stop; i++)
	{
		Derivatives[i - 1] = 0.0;
	}
   /*Initialize DragHand registers to some big negative number*/
	Registers[Reg_MaxkW] = -1.0e50;
	Registers[Reg_MaxkVA] = -1.0e50;
	Registers[Reg_ZoneMaxkW] = -1.0e50;
	Registers[Reg_ZoneMaxkVA] = -1.0e50;
	Registers[Reg_MaxLoadLosses] = -1.0e50;
	Registers[Reg_MaxNoLoadLosses] = -1.0e50;
	Registers[Reg_LossesMaxkW] = -1.0e50;
	Registers[Reg_LossesMaxkvar] = -1.0e50;
	Registers[Reg_GenMaxkW] = -1.0e50;
	Registers[Reg_GenMaxkVA] = -1.0e50;
	FirstSampleAfterReset = true;  // initialize for trapezoidal integration
   // Removed .. open in solution loop See Solve Yearly If EnergyMeterClass.SaveDemandInterval Then OpenDemandIntervalFile;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TEnergyMeterObj::CalcYPrim(int ActorID)
{


 // YPrim is all zeros.  Just leave as NIL so it is ignored.
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TEnergyMeterObj::SaveRegisters(int ActorID)
{
	String CSVName;
	TTextRec f = {};
	int i = 0;
	try
	{
		CSVName = String("MTR_") + get_Name() + ".CSV";
		AssignFile(f, GetOutputDirectory() + CSVName);
		Rewrite(f);
		IOResultToException();
		GlobalResult = CSVName;
		SetLastResultFile(CSVName);
	}
	catch (std::exception &e)
	{
		{
			DoSimpleMsg(String("Error opening Meter File \"") + CRLF
	           + CSVName
	           + "\": "
	           + (std::string) e.what(), 526);
			return;
		}
	}
	try

//       Writeln(F,'**** NEW RECORD ****');
	{
		int stop = 0;
		{ Write(f, "Year, "); Write(f, ActiveCircuit[ActorID]->Solution->get_Fyear(), 0); WriteLn(f, L','); }
		for(stop = NumEMRegisters, i = 1; i <= stop; i++)
		{
			{ Write(f, L'\"'); Write(f, RegisterNames[i - 1]); Write(f, "\","); WriteLn(f, Registers[i - 1], 0, 0); }
		}
/* }
	__finally
	{*/
		CloseFile(f);
	}
	catch (...)
	{
		//
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TEnergyMeterObj::Integrate(int reg, double Deriv, double Interval, int ActorID)
{
	if(ActiveCircuit[ActorID]->TrapezoidalIntegration)
        /*Trapezoidal Rule Integration*/
	{
		if(!FirstSampleAfterReset)
			Registers[reg] = Registers[reg] + 0.5 * Interval * (Deriv + Derivatives[reg]);
	}
	else
 /*Plain Euler integration*/
	{
		Registers[reg] = Registers[reg] + Interval * Deriv;
	}

/* Set the derivatives so that the proper value shows up in Demand Interval Files
  and prepare for next time step in Trapezoidal integration */
	Derivatives[reg] = Deriv;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// Update registers from metered zone
// Assumes one time period has taken place since last sample.

void TEnergyMeterObj::TakeSample(int ActorID)
{
	int i = 0;
	int j = 0;
	int l = 0;
	int Idx = 0;    // Lines only  for now
	complex S_Local = CZero;
	complex S_Totallosses = CZero;
	complex S_LoadLosses = CZero;
	complex S_NoLoadLosses = CZero;
	complex TotalLoadLosses = CZero;
	complex TotalNoLoadLosses = CZero;
	complex TotalLineLosses = CZero;
	complex TotalTransformerLosses = CZero;
	complex TotalLineModeLosses = CZero;
	complex TotalZeroModeLosses = CZero;
	complex Total3phaseLosses = CZero;
	complex Total1phaseLosses = CZero;
	complex TotalLosses = CZero;
	TPDElement* CktElem = nullptr;
	TPDElement* ParenElem = nullptr;
	TPCElement* pcElem = nullptr;
	TLoadObj* pLoad = nullptr;
	TGeneratorObj* pGen = nullptr;
   // doubles
	double MaxExcesskWNorm = 0.0;
	double MaxExcesskWEmerg = 0.0;
	double EEN = 0.0;
	double UE = 0.0;
	double ZonekW = 0.0;
	double TotalZonekW = 0.0;
	double TotalZonekvar = 0.0;
	double TotalLoad_EEN = 0.0;
	double TotalLoad_UE = 0.0;
	double TotalGenkw = 0.0;
	double TotalGenkvar = 0.0;
	double LoadkVA = 0.0;
	double GenkVA = 0.0;
	double S_Local_kVA = 0.0;
	double load_kw = 0.0;
	complex S_PosSeqLosses = CZero;
	complex S_ZeroSeqLosses = CZero;
	complex S_NegSeqLosses = CZero;
	double puV = 0.0;

// Compute energy in branch  to which meter is connected

     //----MeteredElement.ActiveTerminalIdx := MeteredTerminal;  // needed for Excess kVA calcs
	int stop = 0;
	S_Local = cmulreal(MeteredElement->Get_Power(MeteredTerminal, ActorID), 0.001);
	S_Local_kVA = cabs(S_Local);
	Delta_Hrs = ActiveCircuit[ActorID]->Solution->IntervalHrs;
	Integrate(Reg_kWh, S_Local.re, Delta_Hrs, ActorID);   // Accumulate the power
	Integrate(Reg_kvarh, S_Local.im, Delta_Hrs, ActorID);
	SetDragHandRegister(Reg_MaxkW, S_Local.re);   // 3-10-04 removed abs()
	SetDragHandRegister(Reg_MaxkVA, S_Local_kVA);

// Compute Maximum overload energy in all branches in zone
// and mark all load downline from an overloaded branch as unserved
// If localonly, check only metered element
	TotalLosses = CZero;     // Initialize loss accumulators
	TotalLoadLosses = CZero;
	TotalNoLoadLosses = CZero;
	TotalLineLosses = CZero;
	TotalLineModeLosses = CZero;
	TotalZeroModeLosses = CZero;
	Total3phaseLosses = CZero;
	Total1phaseLosses = CZero;
	TotalTransformerLosses = CZero;

     // Init all voltage base loss accumulators
	for(i = 1; i <= MaxVBaseCount; i++)
	{
		VBaseTotalLosses[i] = 0.0;
		VBaseLineLosses[i] = 0.0;
		VBaseLoadLosses[i] = 0.0;
		VBaseNoLoadLosses[i] = 0.0;
		VBaseLoad[i] = 0.0;
	}

     // Phase Voltage arrays
	if(FPhaseVoltageReport)
	{
		for(i = 1; i <= MaxVBaseCount; i++)
		{
			if((VBaseList)[i] > 0.0)
			{
				int stop1 = 0;
				for(stop1 = 3, j = 1; j <= stop1; j++)
				{
					VphaseMax[jiIndex(j, i)] = 0.0;
					VPhaseMin[jiIndex(j, i)] = 9999.0;
					VPhaseAccum[jiIndex(j, i)] = 0.0;
					VPhaseAccumCount[jiIndex(j, i)] = 0;   // Keep track of counts for average
				}
			}
		}
	}
	CktElem = (TPDElement*) BranchList->Get_First();
	MaxExcesskWNorm = 0.0;
	MaxExcesskWEmerg = 0.0;

     /*--------------------------------------------------------------------------*/
     /*------------------------ Local Zone  Only --------------------------------*/
     /*--------------------------------------------------------------------------*/
	if(LocalOnly)
	{
		CktElem = (TPDElement*) MeteredElement;
		MaxExcesskWNorm = Abs( CktElem->Get_ExcessKVANorm(MeteredTerminal,ActorID).re);
		MaxExcesskWEmerg = Abs( CktElem->Get_ExcessKVAEmerg(MeteredTerminal, ActorID).re);
	}
	else
	{
		while(CktElem != nullptr)

     /*--------------------------------------------------------------------------*/
     /*--------Cyle Through Entire Zone Setting EEN/UE --------------------------*/
     /*--------------------------------------------------------------------------*/       // loop thru all ckt elements on zone
		{
			/*# with CktElem do */
			{
				auto with0 = CktElem;
				with0->Set_ActiveTerminal(BranchList->PresentBranch->FromTerminal);
         // Invoking this property sets the Overload_UE flag in the PD Element
				EEN = Abs( with0->Get_ExcessKVANorm(with0->get_FActiveTerminal(),ActorID).re);
				UE = Abs( with0->Get_ExcessKVAEmerg(with0->get_FActiveTerminal(),ActorID).re);
			}

         /*For radial circuits just keep the maximum overload; for mesh, add 'em up*/
			if(ZoneIsRadial)
			{
				if(UE > MaxExcesskWEmerg)
					MaxExcesskWEmerg = UE;
				if(EEN > MaxExcesskWNorm)
					MaxExcesskWNorm = EEN;
			}
			else
			{
				MaxExcesskWEmerg = MaxExcesskWEmerg + UE;
				MaxExcesskWNorm = MaxExcesskWNorm + EEN;
			}

         // Even if this branch is not overloaded, if the parent element is overloaded
         // mark load on this branch as unserved also
         // Use the larger of the two factors
			ParenElem = ((TPDElement*) BranchList->Get_Parent());
			if(ParenElem != nullptr)
			{
				vector <double> myVec0 = { CktElem->OverLoad_EEN, ParenElem->OverLoad_EEN };
				CktElem->OverLoad_EEN = MaxValue(&myVec0);
				vector <double> myVec1 = { CktElem->Overload_UE, ParenElem->Overload_UE };
				CktElem->Overload_UE = MaxValue(&myVec1);
			}

         // Mark loads (not generators) by the degree of overload if the meter's zone is to be considered radial
         // This overrides and supercedes the load's own determination of unserved based on voltage
         // If voltage only is to be used for Load UE/EEN, don't mark (set to 0.0 and load will calc UE based on voltage)
			pcElem = (TPCElement*) BranchList->Get_FirstObject();
			while((pcElem != nullptr))
			{
				if( ( ( (TDSSCktElement*) pcElem )->DSSObjType & CLASSMASK) == LOAD_ELEMENT)
				{
					pLoad = (TLoadObj*) pcElem;
					if((CktElem->OverLoad_EEN > 0.0) && (ZoneIsRadial) && !(VoltageUEOnly))
						pLoad->EEN_Factor = CktElem->OverLoad_EEN;
					else
						pLoad->EEN_Factor = 0.0;
					if((CktElem->Overload_UE > 0.0) && (ZoneIsRadial) && !(VoltageUEOnly))
						pLoad->UE_Factor = CktElem->Overload_UE;
					else
						pLoad->UE_Factor = 0.0;
				}
				pcElem = ((TPCElement*) BranchList->Get_NextObject());
			}
			CktElem = ((TPDElement*) BranchList->Get_Forward());
		}


     // Get the Losses, and unserved bus energies
	}
	
	TotalZonekW = 0.0;
	TotalZonekvar = 0.0;
	TotalLoad_EEN = 0.0;
	TotalLoad_UE = 0.0;
	TotalGenkw = 0.0;
	TotalGenkvar = 0.0;
	

     /*--------------------------------------------------------------------------*/
     /*--------       Cycle Through Zone Accumulating Load and Losses    --------*/
     /*--------------------------------------------------------------------------*/
	CktElem = (TPDElement*) BranchList->Get_First();
	while ((CktElem != nullptr))
	{
		TLoadObj*		pLoad	= nullptr;
		TGeneratorObj*	pGen	= nullptr;
		pcElem = (TPCElement*)BranchList->Get_FirstObject();
		while (pcElem != nullptr)
		{
			int myElem = pcElem->DSSObjType & CLASSMASK;
			switch (myElem)
			{
			case LOAD_ELEMENT:
				if (!LocalOnly)
				{
					pLoad = (TLoadObj*)pcElem;
					load_kw = Accumulate_Load(pLoad, TotalZonekW, TotalZonekvar, TotalLoad_EEN, TotalLoad_UE, ActorID);
					if (FVBaseLosses)
					{
						auto with1 = BranchList->PresentBranch;
						if (with1->VoltBaseIndex > 0)
							VBaseLoad[with1->VoltBaseIndex] = VBaseLoad[with1->VoltBaseIndex] + load_kw;
					}
				}
				break;
			case GEN_ELEMENT:
				pGen = (TGeneratorObj*)pcElem;
				Accumulate_Gen(pGen, TotalGenkw, TotalGenkvar, ActorID);
				break;
			default:
			{
			/* {Ignore other types of PC Elements}*/
			}
				break;
			}
			pcElem = (TPCElement*)BranchList->Get_NextObject();
		}
		if(FLosses)  // Compute and Report Losses
			
           /*Get losses from the present circuit element*/
			{
				CktElem->GetLosses(S_Totallosses, S_LoadLosses, S_NoLoadLosses, ActorID);  // returns watts, vars
           /*Convert to kW*/
				cmulrealaccum(S_Totallosses, 0.001);
				cmulrealaccum(S_LoadLosses, 0.001);
				cmulrealaccum(S_NoLoadLosses, 0.001);
           /*Update accumulators*/
				caccum(TotalLosses, S_Totallosses); // Accumulate total losses in meter zone
				caccum(TotalLoadLosses, S_LoadLosses);  // Accumulate total load losses in meter zone
				caccum(TotalNoLoadLosses, S_NoLoadLosses); // Accumulate total no load losses in meter zone

           /*Line and Transformer Elements*/
				if(IslineElement((TDSSCktElement*) CktElem) && FLineLosses)
				{
					caccum(TotalLineLosses, S_Totallosses); // Accumulate total losses in meter zone
					if(FSeqLosses)
					{
						CktElem->GetSeqLosses(S_PosSeqLosses, S_NegSeqLosses, S_ZeroSeqLosses, ActorID);
						caccum(S_PosSeqLosses, S_NegSeqLosses);  // add line modes together
						cmulrealaccum(S_PosSeqLosses, 0.001); // convert to kW
						cmulrealaccum(S_ZeroSeqLosses, 0.001);
						caccum(TotalLineModeLosses, S_PosSeqLosses);
						caccum(TotalZeroModeLosses, S_ZeroSeqLosses);
					}
               /*Separate Line losses into 3- and "1-phase" losses*/
					if(F3PhaseLosses)
					{
						if(CktElem->Get_NPhases() == 3)
							caccum(Total3phaseLosses, S_Totallosses);
						else
							caccum(Total1phaseLosses, S_Totallosses);
					}
				}
				else
				{
					if(IsTransformerElement(CktElem) && FXfmrLosses)
					{
						caccum(TotalTransformerLosses, S_Totallosses); // Accumulate total losses in meter zone
					}
				}
				if(FVBaseLosses)
				/*# with BranchList.PresentBranch do */
				{
					auto with2 = BranchList->PresentBranch;
					if(with2->VoltBaseIndex > 0)
					{
						VBaseTotalLosses[with2->VoltBaseIndex] = VBaseTotalLosses[with2->VoltBaseIndex] + S_Totallosses.re;
						if(IslineElement(CktElem))
							VBaseLineLosses[with2->VoltBaseIndex] = VBaseLineLosses[with2->VoltBaseIndex] + S_Totallosses.re;
						else
						{
							if(IsTransformerElement(CktElem))
							{
								VBaseLoadLosses[with2->VoltBaseIndex] = VBaseLoadLosses[with2->VoltBaseIndex] + S_LoadLosses.re;
								VBaseNoLoadLosses[with2->VoltBaseIndex] = VBaseNoLoadLosses[with2->VoltBaseIndex] + S_NoLoadLosses.re;
							}
						}
					}
				}

           // Compute min, max, and average pu voltages for 1st 3 phases  (nodes designated 1, 2, or 3)
				if(FPhaseVoltageReport)
				/*# with BranchList.PresentBranch do */
				{
					auto with3 = BranchList->PresentBranch;
					if(with3->VoltBaseIndex > 0)
					/*# with ActiveCircuit[ActorID] do */
					{
								
						if(ActiveCircuit[ActorID]->Buses[with3->FromBusReference - 1]->kVBase > 0.0)
						{
							int stop = 0;
							for(stop = ActiveCircuit[ActorID]->Buses[with3->FromBusReference - 1]->get_FNumNodesThisBus(), i = 1; i <= stop; i++)
							{
								j = ActiveCircuit[ActorID]->Buses[with3->FromBusReference - 1]->GetNum(i);
								if((j > 0) && (j < 4))
								{
									if(!ADiakoptics || (ActorID == 1))
										puV = cabs(ActiveCircuit[ActorID]->Solution->NodeV[ActiveCircuit[ActorID]->Buses[with3->FromBusReference - 1]->GetRef(i)]) / ActiveCircuit[ActorID]->Buses[with3->FromBusReference - 1]->kVBase;
									else
										puV = cabs(ActiveCircuit[ActorID]->Solution->VoltInActor1(ActiveCircuit[ActorID]->Buses[with3->FromBusReference - 1]->GetRef(i))) / ActiveCircuit[ActorID]->Buses[with3->FromBusReference - 1]->kVBase;
									Idx = jiIndex(j, with3->VoltBaseIndex);
									if(puV > (VphaseMax)[Idx])
									{
										(VphaseMax)[jiIndex(j, with3->VoltBaseIndex)] = puV;
                    // VmaxBus := FromBusReference;
									}
									if(puV < (VPhaseMin)[Idx])
									{
										(VPhaseMin)[jiIndex(j, with3->VoltBaseIndex)] = puV;
                    // VminBus := FromBusReference;
									}
									DblInc((VPhaseAccum)[jiIndex(j, with3->VoltBaseIndex)], puV);
									++(VPhaseAccumCount)[jiIndex(j, with3->VoltBaseIndex)];   // Keep track of counts for average
								}
							}
						}
					}
				}
			}  /*If FLosses*/
		CktElem = ((TPDElement*) BranchList->Get_Forward());
	}

     /*NOTE: Integrate proc automatically sets derivatives array*/
	Integrate(Reg_LoadEEN, TotalLoad_EEN, Delta_Hrs, ActorID);
	Integrate(Reg_LoadUE, TotalLoad_UE, Delta_Hrs, ActorID);

     /*Accumulate losses in appropriate registers*/
	Integrate(Reg_ZoneLosseskWh, TotalLosses.re, Delta_Hrs, ActorID);
	Integrate(Reg_ZoneLosseskvarh, TotalLosses.im, Delta_Hrs, ActorID);
	Integrate(Reg_LoadLosseskWh, TotalLoadLosses.re, Delta_Hrs, ActorID);
	Integrate(Reg_LoadLosseskvarh, TotalLoadLosses.im, Delta_Hrs, ActorID);
	Integrate(Reg_NoLoadLosseskWh, TotalNoLoadLosses.re, Delta_Hrs, ActorID);
	Integrate(Reg_NoLoadLosseskvarh, TotalNoLoadLosses.im, Delta_Hrs, ActorID);
	Integrate(Reg_LineLosseskWh, TotalLineLosses.re, Delta_Hrs, ActorID);
	Integrate(Reg_LineModeLineLoss, TotalLineModeLosses.re, Delta_Hrs, ActorID);
	Integrate(Reg_ZeroModeLineLoss, TotalZeroModeLosses.re, Delta_Hrs, ActorID);
	Integrate(Reg_3_phaseLineLoss, Total3phaseLosses.re, Delta_Hrs, ActorID);
	Integrate(Reg_1_phaseLineLoss, Total1phaseLosses.re, Delta_Hrs, ActorID);
	Integrate(Reg_TransformerLosseskWh, TotalTransformerLosses.re, Delta_Hrs, ActorID);
	for(i = 1; i <= MaxVBaseCount; i++)
	{
		Integrate(Reg_VBaseStart + i, VBaseTotalLosses[i], Delta_Hrs, ActorID);
		Integrate(Reg_VBaseStart + (1 * MaxVBaseCount) + i, VBaseLineLosses[i], Delta_Hrs, ActorID);
		Integrate(Reg_VBaseStart + (2 * MaxVBaseCount) + i, VBaseLoadLosses[i], Delta_Hrs, ActorID);
		Integrate(Reg_VBaseStart + (3 * MaxVBaseCount) + i, VBaseNoLoadLosses[i], Delta_Hrs, ActorID);
		Integrate(Reg_VBaseStart + (4 * MaxVBaseCount) + i, VBaseLoad[i], Delta_Hrs, ActorID);
	}


     /*--------------------------------------------------------------------------*/
     /*---------------   Total Zone Load and Generation -------------------------*/
     /*--------------------------------------------------------------------------*/
	Integrate(Reg_ZonekWh, TotalZonekW, Delta_Hrs, ActorID);
	Integrate(Reg_Zonekvarh, TotalZonekvar, Delta_Hrs, ActorID);
	Integrate(Reg_GenkWh, TotalGenkw, Delta_Hrs, ActorID);
	Integrate(Reg_Genkvarh, TotalGenkvar, Delta_Hrs, ActorID);
	GenkVA = sqrt(Sqr(TotalGenkvar) + Sqr(TotalGenkw));
	LoadkVA = sqrt(Sqr(TotalZonekvar) + Sqr(TotalZonekW));

     /*--------------------------------------------------------------------------*/
     /*---------------   Set Drag Hand Registers  ------------------------------*/
     /*--------------------------------------------------------------------------*/
	SetDragHandRegister(Reg_LossesMaxkW, Abs(TotalLosses.re));
	SetDragHandRegister(Reg_LossesMaxkvar, Abs(TotalLosses.im));
	SetDragHandRegister(Reg_MaxLoadLosses, Abs(TotalLoadLosses.re));
	SetDragHandRegister(Reg_MaxNoLoadLosses, Abs(TotalNoLoadLosses.re));
	SetDragHandRegister(Reg_ZoneMaxkW, TotalZonekW); // Removed abs()  3-10-04
	SetDragHandRegister(Reg_ZoneMaxkVA, LoadkVA);
     /*Max total generator registers*/
	SetDragHandRegister(Reg_GenMaxkW, TotalGenkw); // Removed abs()  3-10-04
	SetDragHandRegister(Reg_GenMaxkVA, GenkVA);

     /*--------------------------------------------------------------------------*/
     /*---------------------   Overload Energy  ---------------------------------*/
     /*--------------------------------------------------------------------------*/
     /*Overload energy for the entire zone*/
	if(LocalOnly)
		ZonekW = S_Local.re;
	else
		ZonekW = TotalZonekW;

     /*Either the max excess kW of any PD element or the excess over zone limits*/

     /*regs 9 and 10*/
     /*Fixed these formulas 2-7-07 per discussions with Daniel Brooks */
	if(MaxZonekVA_Norm > 0.0)
	{
		if(S_Local_kVA == 0.0)
			S_Local_kVA = MaxZonekVA_Norm;
		std::vector <double> myVec3 = { 0.0, (ZonekW * (1.0 - MaxZonekVA_Norm / S_Local_kVA) ) };
		Integrate(Reg_OverloadkWhNorm, MaxValue(&myVec3), Delta_Hrs, ActorID);
	}
	else
	{
		Integrate(Reg_OverloadkWhNorm, MaxExcesskWNorm, Delta_Hrs, ActorID);
	}
	if(MaxZonekVA_Emerg > 0.0)
	{
		if(S_Local_kVA == 0.0)
			S_Local_kVA = MaxZonekVA_Emerg;
		std::vector <double> myVec4 = { 0.0, ( ZonekW * ( 1.0 - MaxZonekVA_Emerg / S_Local_kVA ) ) };
		Integrate(Reg_OverloadkWhEmerg, MaxValue(&myVec4), Delta_Hrs, ActorID);
	}
	else
	{
		Integrate(Reg_OverloadkWhEmerg, MaxExcesskWEmerg, Delta_Hrs, ActorID);
	}
	FirstSampleAfterReset = false;
	if(EnergyMeterClass[ActorID]->Get_SaveDemandInterval(ActorID))
		WriteDemandIntervalData(ActorID);
}

/*---------------------------------------------------------------------------------*/

void TEnergyMeterObj::TotalupDownstreamCustomers()
{
	int i = 0;
  /*, Accumulator*/
 // PresentNode: TCktTreeNode;
	TPDElement* CktElem = nullptr;
	int stop = 0;
	if(!ASSIGNED(BranchList))
	{
		DoSimpleMsg("Meter Zone Lists need to be built. Do Solve or Makebuslist first!", 529);
		return;
	}

    /*Init totsls and checked flag*/
	CktElem = (TPDElement*) SequenceList->Get_First();
	while(CktElem != nullptr)
	{
		( (TDSSCktElement*) CktElem )->Checked = false;
		CktElem->BranchTotalCustomers = 0;
		CktElem = (TPDElement*) SequenceList->Get_Next();
	}

  /*This algorithm could be made more efficient with a Sequence list*/
    /*********
     For i := 1 to Branchlist.ZoneEndsList.NumEnds Do
     Begin
       {Busref := } Branchlist.ZoneEndsList.Get(i, PresentNode);
       If PresentNode <> Nil Then
       Begin
          CktElem     := PresentNode.CktObject;
          if Not CktElem.Checked  then    // don't do a zone end element more than once
          Begin
            CktElem.Checked := TRUE;
            Accumulator := CktElem.NumCustomers;
            Repeat  {Trace back to the source}

                Inc(CktElem.TotalCustomers, Accumulator);
                PresentNode := PresentNode.Get_Parent();
                If PresentNode=Nil Then Break;
                CktElem     := PresentNode.CktObject;
                If not CktElem.Checked Then Begin   // avoid double counting
                   Inc(Accumulator, CktElem.NumCustomers);
                   CktElem.Checked := TRUE;
                End;

            Until FALSE;
          End;
       End;
     End; {For}
     *******/

     // Backward Sweep  -  Order is guaranteed to process end branches first
     // sum numcustomers branch by branch
	for(stop = 1, i = SequenceList->get_myNumList(); i >= stop; i--)
	{
		CktElem = (TPDElement*) SequenceList->Get(i);
		if(!( (TDSSCktElement*) CktElem )->Checked)
			/*# with CktElem do */
			{
				auto with0 = CktElem;    // Avoid double counting
				( (TDSSCktElement*) CktElem )->Checked = true;
				with0->BranchTotalCustomers += with0->BranchNumCustomers;
				if (with0->ParentPDElement != nullptr)
				{
					if (!(with0->HasOCPDevice && AssumeRestoration && with0->HasAutoOCPDevice))
						with0->ParentPDElement->BranchTotalCustomers += with0->BranchTotalCustomers;
				}
			}
	}  /*For i*/
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// Set the HasMeter Flag for all cktElement;

void TEnergyMeter::SetHasMeterFlag(int ActorID)
{
	int i = 0;
	TEnergyMeterObj* ThisMeter = nullptr;
	TDSSCktElement* CktElem = nullptr;
   /*Initialize all to FALSE*/
	int stop = 0;
	/*# with ActiveCircuit[ActorID] do */
	{
		
		CktElem = (TDSSCktElement*) ActiveCircuit[ActorID]->PDElements.Get_First();
		while(CktElem != nullptr)
		{
			CktElem->HasEnergyMeter = false;
			CktElem = (TDSSCktElement*) ActiveCircuit[ActorID]->PDElements.Get_Next();
		}  /*WHILE*/
	} /*WITH*/
	for(stop = ActiveCircuit[ActorID]->EnergyMeters.get_myNumList(), i = 1; i <= stop; i++)
	{
		ThisMeter = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get(i);
		/*# with ThisMeter do */
		{
			auto with1 = ThisMeter;
			if(with1->Get_Enabled() && (with1->MeteredElement != nullptr))
				with1->MeteredElement->HasEnergyMeter = true;
		}
	}   /*FOR*/
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This gets fired off whenever the buslists are rebuilt
// Must be updated whenever there is a change in the circuit

void TEnergyMeterObj::MakeMeterZoneLists(int ActorID)
{
	int TestBusNum = 0;
	int ZoneListCounter = 0;
	int j = 0;
	int iTerm = 0;
	int iPC = 0;
	int iPD = 0;
	TDSSCktElement* ActiveBranch = nullptr;
	TPDElement* TestElement = nullptr;
	TPCElement* pPCelem = nullptr;
	TLoadObj* pLoad = nullptr;
	bool IsFeederEnd = false;
	TList* adjLst;
	unsigned int PCElementType = 0;
	int stop = 0;
	ZoneListCounter = 0;
	VBaseCount = 0; /*Build the voltage base list over in case a base added or deleted*/
	for(j = 1; j <= MaxVBaseCount; j++)
	{
		VBaseList[j - 1] = 0.0;
	}
	if(BranchList != nullptr)
		delete BranchList;
	if(Get_Enabled())
    // Make a new branch list
	{
		BranchList = new TCktTree();     /*Instantiates ZoneEndsList, too*/
	}
	else
	{
		BranchList = nullptr;
		return;
	}

  // Get Started
	if(ASSIGNED(MeteredElement))
		BranchList->Set_New(MeteredElement);
	else
   // oops
	{
		DoSimpleMsg(String("Metered Element for EnergyMeter ") + get_Name()
	           + " not defined.", 527);
		return;
	}

  /*Initialize SensorObj property of the first branch to this TMeterElement Object.
   Before starting, all sensorObj definitions are cleared in PCElements and PDElements. The
   SensorObj property is passed down to the Load objects for LoadAllocation and State Estimation
  */
	if(dynamic_cast<TPDElement*>(MeteredElement))
		/*# with TPDElement(MeteredElement) do */
		{
			auto with0 = ((TPDElement*) MeteredElement);
			with0->SensorObj = this;
			with0->MeterObj = this;
		}
	else
	{
		if(dynamic_cast<TPCElement*>(MeteredElement))
			/*# with TPCElement(MeteredElement) do */
			{
				auto with1 = ((TPCElement*) MeteredElement);
				with1->SensorObj = this;
				with1->MeterObj = this;
			}
	}
	MeteredElement->Terminals[MeteredTerminal - 1].Checked = true;
	/*# with BranchList.PresentBranch do */
	{
		auto with2 = BranchList->PresentBranch;
    // This bus is the head of the feeder or zone; do not mark as radial bus
		with2->FromBusReference = MeteredElement->Terminals[MeteredTerminal - 1].BusRef;
		ActiveCircuit[ActorID]->Buses[with2->FromBusReference - 1]->DistFromMeter = 0.0;
		with2->VoltBaseIndex = AddToVoltBaseList(with2->FromBusReference, ActorID);
		with2->FromTerminal = MeteredTerminal;
		if(dynamic_cast<TPDElement*>(MeteredElement))
			((TPDElement*) MeteredElement)->FromTerminal = MeteredTerminal;
	}

  // Check off this element so we don't use it  again
	/*# with MeteredElement do */
	{
		auto with3 = MeteredElement;
		with3->Checked = true;
		with3->IsIsolated = false;
	}

  // Make SequenceList for use in reliability calcs or anything that
  // needs to run through the tree quickly in a radial sequence
	if(ASSIGNED(SequenceList))
		delete SequenceList;
	SequenceList = new PointerList::TPointerList(1024); //make it a big initial allocation
	if(ASSIGNED(LoadList))
		delete LoadList;
	LoadList = new PointerList::TPointerList(1024); //make it a big initial allocation

  // Now start looking for other branches
  // Finds any branch connected to the TestBranch and adds it to the list
  // Goes until end of circuit, another energy meter, an open terminal, or disabled device.
	ActiveBranch = MeteredElement;

  /* ****************  MAIN LOOP ******************************/
	while(ActiveBranch != nullptr)
	{
		int stop = 0;
		SequenceList->Add(ActiveBranch); // When done, this should be the correct order.
		/*# with BranchList.PresentBranch do */
		{
			auto with4 = BranchList->PresentBranch;
			with4->IsLoopedHere = false;
			with4->IsParallel = false;
			with4->IsDangling = true;  // Unless we find something connected to it
			with4->VoltBaseIndex = AddToVoltBaseList(with4->FromBusReference, ActorID);
		}
		((TPDElement*) ActiveBranch)->BranchNumCustomers = 0;   // Init counter
		for(stop = ActiveBranch->Get_NTerms(), iTerm = 1; iTerm <= stop; iTerm++)
		{
			if(!(ActiveBranch->Terminals[iTerm - 1].Checked))
				/*# with ActiveCircuit[ActorID] do */
				{
					
        // Now find all loads and generators connected to the bus on this end of branch
        // attach them as generic objects to cktTree node.
					int stop1 = 0;
					TestBusNum = ActiveBranch->Terminals[iTerm - 1].BusRef;
					/*# with BranchList.PresentBranch do */
					{
						auto with6 = BranchList->PresentBranch;
						with6->Set_ToBusReference(TestBusNum);   // Add this as a "to" bus reference
						if(IslineElement(ActiveBranch))   // Convert to consistent units (km)
							ActiveCircuit[ActorID]->Buses[TestBusNum - 1]->DistFromMeter = ActiveCircuit[ActorID]->Buses[with6->FromBusReference - 1]->DistFromMeter + ((TLineObj*) ActiveBranch)->Len * ConvertLineUnits(((TLineObj*) ActiveBranch)->LengthUnits, UNITS_KM);
						else
							ActiveCircuit[ActorID]->Buses[TestBusNum - 1]->DistFromMeter = ActiveCircuit[ActorID]->Buses[with6->FromBusReference - 1]->DistFromMeter;
					}
					adjLst = &(BusAdjPC[TestBusNum]);
					for(stop1 = adjLst->size() - 1, iPC = 0; iPC <= stop1; iPC++)
					{
						TDSSCktElement* elem = (TDSSCktElement*) (*adjLst)[iPC];
						
						// Note: we cannot use these directly with a plain C cast, casting
						// a pointer can corrupt the memory with delayed issues, hard to locate.
						pPCelem = dynamic_cast<TPCElement*>(elem);
						TPDElement* pPDelem = dynamic_cast<TPDElement*>(elem);

            //  IF pPCelem.Enabled Then Begin   only enabled elements in the search list
						if(!elem->Checked)
						{
							BranchList->PresentBranch->IsDangling = false;   // Something is connected here
                // Is this a load or a generator or a Capacitor or reactor??
							PCElementType = (unsigned int) (elem->DSSObjType & CLASSMASK);
							if((PCElementType == LOAD_ELEMENT) || (PCElementType == GEN_ELEMENT) || (PCElementType == PVSYSTEM_ELEMENT) || (PCElementType == STORAGE_ELEMENT) || (PCElementType == CAP_ELEMENT) || (PCElementType == REACTOR_ELEMENT))
//                OR (PCElementType = PVSYSTEM2_ELEMENT)

//                OR (PCElementType = STORAGE2_ELEMENT)
  // Capacitor and Reactor put on the PC list if IsShunt=TRUE
							{
								BranchList->Set_NewObject(elem); // This adds element to the Shunt list in CktTree
								elem->Checked = true;    // So we don't pick this element up again
								elem->IsIsolated = false;
								elem->Set_ActiveTerminal(1);
                      /*Totalize Number of Customers if Load Type*/
								if((pLoad = dynamic_cast<TLoadObj*>(elem)))
								{
									((TPDElement*) ActiveBranch)->BranchNumCustomers += pLoad->NumCustomers;
									LoadList->Add(pLoad);  // Add to list of loads in this zone.)
								}
                      /*If object does not have a sensor attached, it acquires the sensor of its parent branch*/
								if(!elem->HasSensorObj)
								{
									if (pPCelem) pPCelem->SensorObj = ((TPDElement*)ActiveBranch)->SensorObj;
									if (pPDelem) pPDelem->SensorObj = ((TPDElement*)ActiveBranch)->SensorObj;
								}
								if (pPCelem) pPCelem->MeterObj = this;
								if (pPDelem) pPDelem->MeterObj = this;
							} /*IF*/
						}
					}

        // Now find all branches connected to this bus that we haven't found already
        // Do not include in this zone if branch has open terminals or has another meter
					if(DefinedZoneListSize == 0)  // Search tree for connected branches (default)
					{
						int stop1 = 0;
						IsFeederEnd = true;
						adjLst = &(BusAdjPD[TestBusNum]);
						for(stop1 = adjLst->size() - 1, iPD = 0; iPD <= stop1; iPD++)
						{
							TestElement = ((TPDElement*) (*adjLst)[iPD]);  // Only enabled objects are in this list
            // **** See ResetMeterZonesAll
							if(!(((TDSSCktElement*) TestElement) == ActiveBranch))
							{
								if(!( (TDSSCktElement*) TestElement )->HasEnergyMeter)  // Skip self
  // Stop at other meters  so zones don't interfere
								{
									int stop2 = 0;
									for(stop2 = ((TDSSCktElement*)TestElement)->Get_NTerms(), j = 1; j <= stop2; j++)
									{     // Check each terminal
										if(TestBusNum == (((TDSSCktElement*)TestElement)->Terminals)[j - 1].BusRef)
										{
											BranchList->PresentBranch->IsDangling = false; // We found something it was connected to
                    /*Check for loops and parallel branches and mark them*/
											if(((TDSSCktElement*)TestElement)->Checked)
												/*# with BranchList.PresentBranch do */
												{
													auto with7 = BranchList->PresentBranch;     /*This branch is on some meter's list already */
													with7->IsLoopedHere = true; /*It's a loop*/
													with7->LoopLineObj = TestElement;
													if(IslineElement(ActiveBranch) && IslineElement(((TDSSCktElement*)TestElement)))
													{
														if(CheckParallel(ActiveBranch, ((TDSSCktElement*)TestElement)))
															with7->IsParallel = true; /*It's paralleled with another line*/
													}
												}
											else
  // push TestElement onto stack and set properties
											{
												IsFeederEnd = false;  // for interpolation
												BranchList->AddNewChild(TestElement, TestBusNum, j);  // Add new child to the branchlist
												/*# with TestElement do */
												{
													auto with8 = TestElement;
													(((TDSSCktElement*) with8)->Terminals)[j - 1].Checked = true;
													with8->FromTerminal = j;
													((TDSSCktElement*)with8)->Checked = true;
													((TDSSCktElement*)with8)->IsIsolated = false;
                                  /*Branch inherits sensor of upline branch if it doesn't have its own*/
													if(!((TDSSCktElement*)with8)->HasSensorObj)
														with8->SensorObj = ((TPDElement*) ActiveBranch)->SensorObj;
													with8->MeterObj = this;   // Set meterobj to this meter
													with8->ParentPDElement = ((TPDElement*) ActiveBranch);  // record the parent so we can easily back up for reconductoring, etc.
												}
												break;
											} /*Else*/
										} /*IF TestBusNum*/
									}  /*FOR terminals*/
								} /*ELSE*/
							}
						} /*FOR iPD*/
						if(IsFeederEnd)
							BranchList->ZoneEndsList->Add(BranchList->PresentBranch, TestBusNum);
             /*This is an end of the feeder and testbusnum is the end bus*/
					}
					else
   // Zone is manually specified; Just add next element in list as a child
					{
						++ZoneListCounter;
						while(ZoneListCounter <= DefinedZoneListSize)
						{
							if(ActiveCircuit[ActorID]->SetElementActive((DefinedZoneList)[ZoneListCounter - 1]) == 0) // Not Found. Let's search for another
								++ZoneListCounter;
							else
							{
								TestElement = (TPDElement*) ActiveCircuit[ActorID]->get_FActiveCktElement();
								if(!((TDSSCktElement*)TestElement)->Get_Enabled())  // Lets ignore disabled devices
									++ZoneListCounter;
								else
								{
									if((((TDSSCktElement*)TestElement)->DSSObjType & BaseClassMask) != PD_ELEMENT)  // Lets ignore non-PD elements
										++ZoneListCounter;
									else
										BranchList->AddNewChild(TestElement, 0, 0); // add it as a child to the previous element
									break;
								}
							}
						} // while
					}
				}  /*WITH Active Circuit*/
		}   /*FOR iTerm*/
		ActiveBranch = ((TDSSCktElement*) BranchList->Get_Forward());   // Sets PresentBranch
  /* ****************  END MAIN LOOP ******************************/
	}
	TotalupDownstreamCustomers();

  /****If HasFeeder Then FeederObj.InitializeFeeder(BranchList);   // Synchronize the feeder definition */
	AssignVoltBaseRegisterNames();
}

/*--------------------------------------------------------------------------*/  //Get present value of terminal Curr FOR reports

void TEnergyMeterObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		Curr[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TEnergyMeterObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		Curr[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TEnergyMeterObj::ZoneDump(int ActorID)
{
	String CSVName;
	TTextRec f = {};
	TPDElement* PDElem = nullptr;
	TDSSCktElement* LoadElem = nullptr;
	try
	{
		CSVName = String("Zone_") + get_Name() + ".CSV";
		AssignFile(f, GetOutputDirectory() + CSVName);
		Rewrite(f);
		IOResultToException();
		GlobalResult = CSVName;
		SetLastResultFile(CSVName);
	}
	catch (std::exception &e)
	{
		{
			DoSimpleMsg(String("Error opening File \"") + CSVName + "\": " + (std::string) e.what(), 528);
			return;
		}
	}
	try
	{
		WriteLn(f, "Level, Branch, Bus1, Bus2, Distance");
		if(BranchList != nullptr)
		{
			PDElem = ((TPDElement*) BranchList->Get_First());
			while(PDElem != nullptr)
				/*# with ActiveCircuit[ActorID] do */
				{
					
                  /*BusList.Get(BranchList.PresentBranch.ToBusReference),*/
					WriteLn(f, Format("%d, %s.%s, %s, %s, %10.4f", 
						BranchList->Get_Level(),
							PDElem->ParentClass->get_myClass_name().c_str(),
							PDElem->get_Name().c_str(),
							PDElem->Get_FirstBus().c_str(),
							PDElem->Get_NextBus().c_str(),
							ActiveCircuit[ActorID]->Buses[BranchList->PresentBranch->Get_ToBusReference() - 1]->DistFromMeter));
					BranchList->PresentBranch->ResetToBusList();
					LoadElem = ((TDSSCktElement*) BranchList->Get_FirstObject());
					while(LoadElem != nullptr)
					{
						{ Write(f, "-1, "); 
						WriteLn(f, LoadElem->ParentClass->get_myClass_name() +
							"." +
							LoadElem->get_Name() +
							", " +
							LoadElem->Get_FirstBus()); 
						}
						LoadElem = ((TDSSCktElement*) BranchList->Get_NextObject());
					}
					PDElem = ((TPDElement*) BranchList->Get_Forward());
				}
		}
/* }
	__finally
	{*/
		CloseFile(f);
	}
	catch (...)
	{
		//
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TEnergyMeterObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	TPDElement* PDElem = nullptr;
	TDSSCktElement* LoadElem = nullptr;
	TDSSCktElement::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			switch(i)
			{
				case 	4:     // option
				{
					{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, "=("); }
					if(ExcessFlag)
						Write(f, "E,");
					else
						Write(f, "T,");
					if(ZoneIsRadial)
						Write(f, " R,");
					else
						Write(f, " M,");
					if(VoltageUEOnly)
						Write(f, " V");
					else
						Write(f, " C");
					WriteLn(f, L')');
				}
				break;
				case 	7:
				{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, "=("); Write(f, Get_PropertyValue(i)); WriteLn(f, L')'); }
				break;
				default:
				{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
				break;
			}
		}
	}
	if(Complete)
	{
		int stop = 0;
		WriteLn(f, "Registers");
		for(stop = NumEMRegisters, i = 1; i <= stop; i++)
		{
			{ Write(f, L'\"'); Write(f, RegisterNames[i - 1]); Write(f, "\" = "); WriteLn(f, Registers[i - 1], 0, 0); }
		}
		WriteLn(f);
		WriteLn(f, "Branch List:");
		if(BranchList != nullptr)
		{
			PDElem = ((TPDElement*) BranchList->Get_First());
			while(PDElem != nullptr)
			{
				{ Write(f, "Circuit Element = "); WriteLn(f, ( (TDSSCktElement*) PDElem )->get_Name()); }
				LoadElem = ((TDSSCktElement*) BranchList->Get_FirstObject());
				while(LoadElem != nullptr)
				{
					{ Write(f, "   Shunt Element = "); Write(f, LoadElem->ParentClass->get_myClass_name()); Write(f, L'.'); WriteLn(f, LoadElem->get_Name()); }
					LoadElem = ((TDSSCktElement*) BranchList->Get_NextObject());
				}
				PDElem = ((TPDElement*) BranchList->Get_Forward());
			}
		}
	}
}

void TEnergyMeter::ProcessOptions(const String Opts)
{
	String S1;
	String S2;
	AuxParser[ActiveActor]->SetCmdString(Opts);  // Load up aux Parser

    /*Loop until no more options found*/
	/*# with ActiveEnergyMeterObj do */
	{
		auto with0 = ActiveEnergyMeterObj;
		do
		{
			S1 = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
			S2 = LowerCase(AuxParser[ActiveActor]->MakeString_());
			if(S2.size() > 0)
				switch(S2[1])
				{
					case 	L'e':
					with0->ExcessFlag = true;
					break;
					case 	L't':
					with0->ExcessFlag = false;
					break;
					case 	L'r':
					with0->ZoneIsRadial = true;
					break;
					case 	L'm':
					with0->ZoneIsRadial = false;
					break;
					case 	L'c':
					with0->VoltageUEOnly = false;
					break;
					case 	L'v':
					with0->VoltageUEOnly = true;
					break;
					default:
					  ;
					break;
				}
		}
		while(!(S2.size() == 0));
	}
}
/*Add to VoltBase list if not already there and return index*/

int TEnergyMeterObj::AddToVoltBaseList(int BusRef, int ActorID)
{
	int result = 0;
	int i = 0;
	/*# with ActiveCircuit[ActorID]->Buses^[BusRef] do */
	{
		
		int stop = 0;
		for(stop = VBaseCount, i = 1; i <= stop; i++)
		{
			if(Abs((1.0 - ActiveCircuit[ActorID]->Buses[BusRef - 1]->kVBase / (VBaseList)[i])) < 0.01)    // < 1% difference
			{
				result = i;
				return result;
			}
		}
		if((ActiveCircuit[ActorID]->Buses[BusRef - 1]->kVBase > 0.0) && (VBaseCount < MaxVBaseCount))
		{
			++VBaseCount; /*ActiveCircuit[ActorID].Buses^[BusRef].*/
			(VBaseList)[VBaseCount] = ActiveCircuit[ActorID]->Buses[BusRef - 1]->kVBase;
			result = VBaseCount;
		}
		else
		result = 0;
	}
	return result;
}

void TEnergyMeterObj::AllocateLoad(int ActorID)
{
	int ConnectedPhase = 0;
	TPDElement* CktElem = nullptr;
	TLoadObj* LoadElem = nullptr;


/*PREREQUISITE: EXECUTE CALCALLOCATIONFACTORS FOR ALL ENERGYMETERS AND SENSORS*/
/*****Done in calling procedure  now ***   CalcAllocationFactors;*/     /*for this meter. Inherited from Meterelement*/
/*See ExecHelper*/

    /* Now go through the meter's zone and adjust the loads.

      While the AllocationFactor property is adjusted for all loads, it will only
      have an effect on loads defined with either the XFKVA property or the
      kWh property.

      Loads have a SensorObj property that points to its upstream sensor that has the adjustments for
      the allocation factors.  This is established in the MakeMeterZoneLists proc in this Unit.

      Sensors consist of EnergyMeters, which drive the load allocation process and Sensor objects that
      are simply voltage and current measuring points.  A Sensor may be attached to a line or transformer
      or it may be connected directly to a load.
     */
	CktElem = ((TPDElement*) BranchList->Get_First());
	while(CktElem != nullptr)
	{
		LoadElem = ((TLoadObj*) BranchList->Get_FirstObject());
		while((LoadElem != nullptr))
		{
			if( ( ( LoadElem )->DSSObjType & CLASSMASK) == LOAD_ELEMENT)
				switch(((TDSSCktElement*)LoadElem)->Get_NPhases())
				{  // only for loads not other shunts
					case 	1:
					/*# with LoadElem do */
					{
						auto with0 = LoadElem;
                 /*For Single phase loads, allocate based on phase factor, else average factor*/
						ConnectedPhase = (ActiveCircuit[ActorID]->MapNodeToBus)[ (with0->NodeRef)[1 - 1] - 1 ].NodeNum;
						if((ConnectedPhase > 0) && (ConnectedPhase < 4))
						{
							if ( ( ( with0->SensorObj ) )->Get_NPhases() == 1)   // Restrict to phases 1..3
								with0->Set_AllocationFactor(with0->get_FAllocationFactor() * (with0->SensorObj->PhsAllocationFactor)[0]);
							else
								with0->Set_AllocationFactor(with0->get_FAllocationFactor() * (with0->SensorObj->PhsAllocationFactor)[ConnectedPhase - 1]);
						}
					}
					break;
					default:
					/*# with LoadElem do */
					{
						auto with1 = LoadElem;
						with1->Set_AllocationFactor(with1->get_FAllocationFactor() * with1->SensorObj->AvgAllocFactor);
					}
					break;
				}  /*CASE*/
			LoadElem = ((TLoadObj*) BranchList->Get_NextObject());
		}   /*While Loadelem*/
		CktElem = ((TPDElement*) BranchList->Get_Forward());    /*Go on down the tree*/
	}  /*While CktElem*/
}

void TEnergyMeterObj::InitPropertyValues(int ArrayOffset)
{
	int i = 0;
	String s;
	int stop = 0;
	Set_PropertyValue(1,""); //'element';
	Set_PropertyValue(2,"1"); //'terminal';
	Set_PropertyValue(3,"clear"); //'action';
	Set_PropertyValue(4,"(E, R, C)"); //'Option';
	Set_PropertyValue(5,"0.0"); //'kWnormal';
	Set_PropertyValue(6,"0.0"); //'kwEmerg';
	Set_PropertyValue(7,"(400, 400, 400)"); //'PeakCurrent';
	Set_PropertyValue(8,""); // ZoneList
	Set_PropertyValue(9,"No");
     /*Define mask as 1 for all registers*/
	s = "[";
	for(stop = NumEMRegisters, i = 1; i <= stop; i++)
	{
		s = s + "1 ";
	}
	Set_PropertyValue(10,s + "]");
	Set_PropertyValue(11,"Yes");
	Set_PropertyValue(12,"Yes");
	Set_PropertyValue(13,"Yes");
	Set_PropertyValue(14,"Yes");
	Set_PropertyValue(15,"Yes"); // segregate losses by voltage base
	Set_PropertyValue(16,"Yes");
	Set_PropertyValue(17,"No");
	Set_PropertyValue(18,"0");
	Set_PropertyValue(19,"0");
	Set_PropertyValue(20,"0");
	Set_PropertyValue(21,"0");
	Set_PropertyValue(22,"0");
	Set_PropertyValue(23,"0");
	Set_PropertyValue(24,"0");
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TEnergyMeterObj::Accumulate_Gen(TGeneratorObj* pGen, double& TotalZonekW, double& TotalZonekvar, int ActorID)
{
	complex s = {};
     //----pGen.ActiveTerminalIdx := 1;
	s = cnegate(cmulreal( ( (TDSSCktElement*) pGen )->Get_Power(1, ActorID), 0.001));
	TotalZonekW = TotalZonekW + s.re;
	TotalZonekvar = TotalZonekvar + s.im;
}

double TEnergyMeterObj::Accumulate_Load(TLoadObj* pLoad, double& TotalZonekW, double& TotalZonekvar, double& TotalLoad_EEN, double& TotalLoad_UE, int ActorID)
{
	double result = 0.0;
	complex S_Load = cmplx(0,0);
	double kW_Load = 0.0;
	double Load_EEN = 0.0;
	double Load_UE = 0.0;
	/*# with pLoad do */
	{
		auto with0 = pLoad;
       //----ActiveTerminalIdx := 1;
		S_Load = cmulreal( ((TDSSCktElement*) pLoad )->Get_Power(1, ActorID), 0.001);   // Get Power in Terminal 1
		kW_Load = S_Load.re;
		result = kW_Load;

       /*Accumulate load in zone*/
		TotalZonekW = TotalZonekW + kW_Load;
		TotalZonekvar = TotalZonekvar + S_Load.im;

       /*always integrate even if the value is 0.0
        otherwise the Integrate function is not correct*/
       /*Invoking the ExceedsNormal and Unserved Properties causes the factors to be computed*/
		if(ExcessFlag)   // Return Excess load as EEN/UE
		{
			if (with0->Get_ExceedsNormal(ActorID))
				Load_EEN = kW_Load * with0->EEN_Factor;// *0.3194;
			else
				Load_EEN = 0.0;
			if (with0->Get_Unserved(ActorID))
				Load_UE = kW_Load * with0->UE_Factor;// *0.1536;
			else
				Load_UE = 0.0;
		}
		else
    // Return TOTAL load as EEN/UE
		{
			if(with0->Get_ExceedsNormal(ActorID))
				Load_EEN = kW_Load;
			else
				Load_EEN = 0.0;
			if(with0->Get_Unserved(ActorID))
				Load_UE = kW_Load;
			else
				Load_UE = 0.0;
		}
		TotalLoad_EEN = TotalLoad_EEN + Load_EEN;
		TotalLoad_UE = TotalLoad_UE + Load_UE;
	} /*WITH*/
	return result;
}

/*Reduce the zone by merging lines*/

void TEnergyMeterObj::ReduceZone(int ActorID)
{

 // Make  sure zone list is built
	if(!ASSIGNED(BranchList))
		MakeMeterZoneLists(ActorID);
	switch(ActiveCircuit[ActorID]->ReductionStrategy)
	{
		case 	rsShortlines:
		DoReduceShortLines(BranchList);
		break;    /*See ReduceAlgs.Pas*/
         /*rsTapEnds:       DoReduceTapEnds (BranchList);*/
		case 	rsMergeParallel:
		DoMergeParallelLines(BranchList);
		break;
		case 	rsDangling:
		DoReduceDangling(BranchList);
		break;
		case 	rsBreakLoop:
		DoBreakLoops(BranchList);
		break;
		case 	rsSwitches:
		DoReduceSwitches(BranchList);
		break;
		case 	rsLaterals:
		DoRemoveAll_1ph_Laterals(BranchList);
		break;
       /*Default*/
		default:
		DoReduceDefault(BranchList);
		break;
	}
/* Feeder Code removed
    // Resynchronize with Feeders
    If HasFeeder Then FeederObj.InitializeFeeder (Branchlist);
*/
}
/*Start at the ends of the zone and work toward the start
 interpolating between known coordinates*/

void TEnergyMeterObj::InterpolateCoordinates()
{
	int i = 0;
	int BusRef = 0;
	int FirstCoordRef = 0;
	int SecondCoordRef = 0;
	int LineCount = 0;
	TCktTreeNode* PresentNode = nullptr;
	TCktTreeNode* StartNode = nullptr;
	TDSSCktElement* CktElem = nullptr;
	if(!ASSIGNED(BranchList))
	{
		DoSimpleMsg("Meter Zone Lists need to be built. Do Solve or Makebuslist first!", 529);
		return;
	}
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		int stop = 0;
		for(stop = BranchList->ZoneEndsList->NumEnds, i = 1; i <= stop; i++)
		{
			BusRef = BranchList->ZoneEndsList->Get(i, PresentNode);
			FirstCoordRef = BusRef;
			SecondCoordRef = FirstCoordRef;  /*so compiler won't issue stupid warning*/
       /*Find a bus with a coordinate*/
			if(!ActiveCircuit[ActiveActor]->Buses[BusRef - 1]->CoordDefined)
			{
				while(!ActiveCircuit[ActiveActor]->Buses[PresentNode->FromBusReference - 1]->CoordDefined)
				{
					PresentNode = PresentNode->Get_Parent();
					if(PresentNode == nullptr)
						break;
				}
				if(PresentNode != nullptr)
					FirstCoordRef = PresentNode->FromBusReference;
			}
			while(PresentNode != nullptr)

          /*Back up until we find another Coord defined*/
			{
				LineCount = 0;   /*number of line segments in this segment*/
				StartNode = PresentNode;
				CktElem = ((TDSSCktElement*) PresentNode->CktObject);
				if(FirstCoordRef != PresentNode->FromBusReference)   /*Handle special case for end branch*/
				{
					if(ActiveCircuit[ActiveActor]->Buses[PresentNode->FromBusReference - 1]->CoordDefined)
						FirstCoordRef = PresentNode->FromBusReference;
					else
						++LineCount;
				}
				do
				{
					CktElem->Checked = true;
					PresentNode = PresentNode->Get_Parent();
					if(PresentNode == nullptr)
						break;
					CktElem = ((TDSSCktElement*) PresentNode->CktObject);
					SecondCoordRef = PresentNode->FromBusReference;
					++LineCount;
				}
				while(!(ActiveCircuit[ActiveActor]->Buses[SecondCoordRef - 1]->CoordDefined || CktElem->Checked));
				if((PresentNode != nullptr) && (LineCount > 1))
				{
					if(ActiveCircuit[ActiveActor]->Buses[SecondCoordRef - 1]->CoordDefined)
					{
						CalcBusCoordinates(StartNode, FirstCoordRef, SecondCoordRef, LineCount);
					}
					else
					break; /*While - went as far as we could go this way*/
				}
				FirstCoordRef = SecondCoordRef;
			}
		} /*For*/
	} /*With*/
}

void TEnergyMeterObj::CalcBusCoordinates(TCktTreeNode* StartBranch, int FirstCoordRef, int SecondCoordRef, int LineCount)
{
	double X = 0.0;
	double Y = 0.0;
	double Xinc = 0.0;
	double Yinc = 0.0;
	if(LineCount == 1)
		return;  /*Nothing to do!*/
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		Xinc = (ActiveCircuit[ActiveActor]->Buses[FirstCoordRef - 1]->x - ActiveCircuit[ActiveActor]->Buses[SecondCoordRef - 1]->x) / LineCount;
		Yinc = (ActiveCircuit[ActiveActor]->Buses[FirstCoordRef - 1]->y - ActiveCircuit[ActiveActor]->Buses[SecondCoordRef - 1]->y) / LineCount;
		X = ActiveCircuit[ActiveActor]->Buses[FirstCoordRef - 1]->x;
		Y = ActiveCircuit[ActiveActor]->Buses[FirstCoordRef - 1]->y;

       /****Debug*/
   /*    If ((X<10.0) and (y<10.0)) or
          ((Buses^[SecondCoordRef].X<10.0) and (Buses^[SecondCoordRef].Y<10.0)) Then
       Begin
          X := y;  // Stopping point
       End;
     */
     
     /*Either start with the "to" end of StartNode or the "from" end;*/
		if(FirstCoordRef != StartBranch->FromBusReference)  // Start with "to" end
		{
			X = X - Xinc;
			Y = Y - Yinc;
			ActiveCircuit[ActiveActor]->Buses[StartBranch->FromBusReference - 1]->x = X;
			ActiveCircuit[ActiveActor]->Buses[StartBranch->FromBusReference - 1]->y = Y;
			ActiveCircuit[ActiveActor]->Buses[StartBranch->FromBusReference - 1]->CoordDefined = true;
			--LineCount;
		}
		while(LineCount > 1)
		{
			X = X - Xinc;
			Y = Y - Yinc;
			StartBranch = StartBranch->Get_Parent(); // back up the tree
			ActiveCircuit[ActiveActor]->Buses[StartBranch->FromBusReference - 1]->x = X;
			ActiveCircuit[ActiveActor]->Buses[StartBranch->FromBusReference - 1]->y = Y;
			ActiveCircuit[ActiveActor]->Buses[StartBranch->FromBusReference - 1]->CoordDefined = true;
			--LineCount;
		}
	}
}




/*--------------------------- CalcReliabilityIndices ----------------------------*/

void TEnergyMeterObj::CalcReliabilityIndices(bool AssumeRestoration_in, int ActorID)
{
	TPDElement*		PD_Elem = nullptr;
	TFeederSection	pSection = {};
	int				Idx = 0,
					stop = 0;
	TDSSBus*		pBus = nullptr;
	double			dblNcusts = 0.0,
					dblkW = 0.0;

	if(!ASSIGNED(SequenceList))
	{
		DoSimpleMsg("Energymeter." + get_Name() + " Zone not defined properly.", 52901);
		return;
	}

	// Update the number of customers
	AssumeRestoration = AssumeRestoration_in;
	TotalupDownstreamCustomers();
  // Zero reliability accumulators
	for(stop = 1, Idx = SequenceList->get_myNumList(); Idx >= stop; Idx--)
	{
		((TPDElement*) SequenceList->Get(Idx))->ZeroReliabilityAccums();
	}

    // Backward sweep calculating failure rates
	for(stop = 1, Idx = SequenceList->get_myNumList(); Idx >= stop; Idx--)
	{
		/*# with TPDElement(SequenceList->Get(Idx)) do */
		{
			auto with0 = ((TPDElement*) SequenceList->Get(Idx));
			with0->CalcFltRate();    // Calc failure rate for this element
			with0->AccumFltRate();
		}
	}

    // Forward sweep to get number of interruptions
       // Initialize number of interruptions and Duration
	PD_Elem = ((TPDElement*) SequenceList->Get(1));
	pBus = ActiveCircuit[ActorID]->Buses[ PD_Elem->Terminals[PD_Elem->FromTerminal - 1].BusRef - 1];
	pBus->Bus_Num_Interrupt = Source_NumInterruptions;
	pBus->BusCustInterrupts = Source_NumInterruptions * pBus->BusTotalNumCustomers;
	pBus->Bus_Int_Duration = Source_IntDuration;

       // init for defining sections
	SectionCount = 0;
	pBus->BusSectionID = SectionCount; // section before 1st OCP device is zero
	for(stop = SequenceList->get_myNumList(), Idx = 1; Idx <= stop; Idx++)
	{
		((TPDElement*) SequenceList->Get(Idx))->CalcNum_Int(SectionCount, AssumeRestoration_in);
	}
	if(SectionCount == 0)   // Error - no OCP devices
	{
		DoSimpleMsg("Error: No Overcurrent Protection device (Relay, Recloser, or Fuse) defined. Aborting Reliability calc.", 52902);
		return;
	}

       // Now have number of sections  so allocate FeederSections array
	FeederSections.resize(SectionCount + 1);
	for(stop = SectionCount, Idx = 0; Idx <= stop; Idx++)
	{
		/*# with FeederSections^[Idx] do */
		{
			      // Should be only place idx=0
			FeederSections[Idx].OCPDeviceType = 0;    // 1=Fuse; 2=Recloser; 3=Relay
			FeederSections[Idx].AverageRepairTime = 0.0;
			FeederSections[Idx].SumFltRatesXRepairHrs = 0.0;
			FeederSections[Idx].SumBranchFltRates = 0.0;
			FeederSections[Idx].NCustomers = 0;
			FeederSections[Idx].TotalCustomers = 0;
			FeederSections[Idx].SectFaultRate = 0.0;
			FeederSections[Idx].NBranches = 0;
			FeederSections[Idx].SeqIndex = 0;
		}
	}


    // Now do Backward sweep calculating N*Fault rates
	for(stop = 1, Idx = SequenceList->get_myNumList(); Idx >= stop; Idx--)
	{
		PD_Elem = (TPDElement*) SequenceList->Get(Idx);
		PD_Elem->CalcCustInterrupts();
		// Populate the Section properties
		pSection = FeederSections[PD_Elem->BranchSectionID];
		pSection.NCustomers += PD_Elem->BranchNumCustomers; // Sum up num Customers on this Section
		pSection.NBranches += 1; // Sum up num branches on this Section
		pBus =  ActiveCircuit[ActorID]->Buses[PD_Elem->Terminals[PD_Elem->ToTerminal - 1].BusRef - 1] ;
		DblInc(pSection.SumBranchFltRates, pBus->Bus_Num_Interrupt * PD_Elem->BranchFltRate);
		DblInc(pSection.SumFltRatesXRepairHrs, (pBus->Bus_Num_Interrupt * PD_Elem->BranchFltRate * PD_Elem->HrsToRepair));
		if( PD_Elem->HasOCPDevice)  // set Section properties
		{
			pSection.OCPDeviceType = GetOCPDeviceType(PD_Elem);
			pSection.SeqIndex = Idx;  // index of pdelement with OCP device at head of section
			pSection.TotalCustomers = PD_Elem->BranchTotalCustomers;
			pSection.SectFaultRate = PD_Elem->AccumulatedBrFltRate;
		}
		FeederSections[PD_Elem->BranchSectionID] = pSection;  // put it back
	}

          /* Compute Avg Interruption duration of each Section  except 0 Section*/
	for(stop = SectionCount, Idx = 1; Idx <= stop; Idx++)
	{
		/*# with FeederSections^[Idx] do */
		{
			
			FeederSections[Idx].AverageRepairTime = FeederSections[Idx].SumFltRatesXRepairHrs / FeederSections[Idx].SumBranchFltRates;
		}
	}

          /* Set Bus_int_Duration */
	/*# with ActiveCircuit[ActorID] do */
	{
		
		int stop = 0;
		for(stop = ActiveCircuit[ActorID]->NumBuses, Idx = 1; Idx <= stop; Idx++)
		{
			pBus = ActiveCircuit[ActorID]->Buses[Idx - 1];
			if(pBus->BusSectionID > 0)
				pBus->Bus_Int_Duration = Source_IntDuration + FeederSections[pBus->BusSectionID].AverageRepairTime;
		}
	}  

        /* Compute SAIFI based on numcustomers and load kW */
        /* SAIFI is weighted by specified load weights */
        /* SAIFI is for the EnergyMeter Zone */
	SAIFI = 0.0;
	CAIDI = 0.0;
	SAIFIkW = 0.0;
	dblNcusts = 0.0;
	dblkW = 0.0;
	CustInterrupts = 0.0;

        // Use LoadList for SAIFI calculation
	/*# with ActiveCircuit[ActorID] do */
	{
		
		int stop = 0;
		for(stop = LoadList->get_myNumList(), Idx = 1; Idx <= stop; Idx++)
		{ // all loads in meter zone

      // Compute CustInterrupts based on interrupts at each load
			/*# with TLoadObj(LoadList.Get(Idx)) do */
			{
				auto with5 = ((TLoadObj*) LoadList->Get(Idx));
				pBus = ActiveCircuit[ActorID]->Buses[ with5->Terminals[1 - 1].BusRef - 1]; // pointer to Load's bus
				CustInterrupts = CustInterrupts + with5->NumCustomers * with5->RelWeighting * pBus->Bus_Num_Interrupt;
				SAIFIkW = SAIFIkW + with5->kWBase * with5->RelWeighting * pBus->Bus_Num_Interrupt;
				DblInc(dblNcusts, with5->NumCustomers * with5->RelWeighting);
              // total up weighted numcustomers
				DblInc(dblkW, with5->kWBase * with5->RelWeighting); // total up weighted kW
              // Set BusCustDurations for Branch reliability export
				pBus->BusCustDurations = (pBus->BusTotalNumCustomers + with5->NumCustomers) * with5->RelWeighting * pBus->Bus_Int_Duration * pBus->Bus_Num_Interrupt;
			}
		}
	}

        // Compute SAIDI from Sections list
	SAIDI = 0.0;
	for(stop = SectionCount, Idx = 1; Idx <= stop; Idx++)
	{
		/*# with FeederSections^[Idx] do */
		{
			    // ignore idx=0
			SAIDI = SAIDI + FeederSections[Idx].SectFaultRate * FeederSections[Idx].AverageRepairTime * FeederSections[Idx].TotalCustomers;
		}
	}
	if(dblNcusts > 0.0)
	{
		SAIFI = CustInterrupts / dblNcusts; // Normalize to total number of customers
		SAIDI = SAIDI / dblNcusts; // Normalize to total number of customers
	}
	if(SAIFI > 0.0)
		CAIDI = SAIDI / SAIFI;
	if(dblkW > 0.0)
		SAIFIkW = SAIFIkW / dblkW; // Normalize to total kW
}

/*-------------------------------------------------------------------------------*/

String TEnergyMeterObj::GetPropertyValue(int Index)
{
	String result;
	switch(Index)
	{
		case 	4:
		 case 7:
		result = "(";
		break;
		default:
		result = "";
		break;
	}
	switch(Index)
	{
		case 	4:     // option
		{
			if(ExcessFlag)
				result = result + "E,";
			else
				result = result + "T,";
			if(ZoneIsRadial)
				result = result + " R,";
			else
				result = result + " M,";
			if(VoltageUEOnly)
				result = result + " V";
			else
				result = result + " C";
		}
		break;
		case 	20:
		result = Format("%.11g", SAIFI);
		break;
		case 	21:
		result = Format("%.11g", SAIFIkW);
		break;
		case 	22:
		result = Format("%.11g", SAIDI);
		break;
		case 	23:
		result = Format("%.11g", CAIDI);
		break;
		case 	24:
		result = Format("%.11g", CustInterrupts);
		break;
		default:
		result = result + inherited::GetPropertyValue(Index);
		break;
	}
	switch(Index)
	{
		case 	4:
		 case 7:
		result = result + ")";
		break;
		default:
		  ;
		break;
	}
	return result;
}

void TEnergyMeterObj::SaveZone(const String DirName)
{
	TDSSCktElement* CktElem = nullptr;
	TDSSCktElement* shuntElement = nullptr;
	TLoadObj* LoadElement = nullptr;
	TDSSCktElement* pControlElem = nullptr;
	TTextRec FBranches = {};
	TTextRec FShunts = {};
	TTextRec FLoads = {};
	TTextRec FGens = {};
	TTextRec FCaps = {};
	TTextRec FXfmrs = {};
	int NBranches = 0;
	int NShunts = 0;
	int Nloads = 0;
	int NGens = 0;
	int NCaps = 0;
	int NXfmrs = 0;
 /*We are in the directory indicated by dirname*/

/*Run down the zone and write each element into a file*/
	if(BranchList != nullptr)
    /*Open some files:*/
	{
		try
		{
			AssignFile(FBranches, "Branches.dss");     // Both lines and transformers
			Rewrite(FBranches);
			IOResultToException();
			NBranches = 0;
		}
		catch (std::exception &e)
		{
			{
				DoSimpleMsg(String("Error creating Branches.dss for Energymeter: ") + this->get_Name()
	           + ". "
	           + (std::string) e.what(), 530);
				CloseFile(FBranches); System::InOutRes = 0;
				return;
			}
		}
		try
		{
			AssignFile(FXfmrs, "Transformers.dss");     // Both lines and transformers
			Rewrite(FXfmrs);
			IOResultToException();
			NXfmrs = 0;
		}
		catch (std::exception &e)
		{
			{
				DoSimpleMsg(String("Error creating Transformers.dss for Energymeter: ") + this->get_Name()
	           + ". "
	           + (std::string) e.what(), 53001);
				CloseFile(FXfmrs); System::InOutRes = 0;
				return;
			}
		}
		try
		{
			AssignFile(FShunts, "Shunts.dss");
			Rewrite(FShunts);
			IOResultToException();
			NShunts = 0;
		}
		catch (std::exception &e)
		{
			{
				DoSimpleMsg(String("Error creating Shunts.dss for Energymeter: ") + this->get_Name()
	           + ". "
	           + (std::string) e.what(), 531);
				CloseFile(FShunts); System::InOutRes = 0;
				return;
			}
		}
		try
		{
			AssignFile(FLoads, "Loads.dss");
			Rewrite(FLoads);
			IOResultToException();
			Nloads = 0;
		}
		catch (std::exception &e)
		{
			{
				DoSimpleMsg(String("Error creating Loads.dss for Energymeter: ") + this->get_Name()
	           + ". "
	           + (std::string) e.what(), 532);
				CloseFile(FLoads); System::InOutRes = 0;
				return;
			}
		}
		try
		{
			AssignFile(FGens, "Generators.dss");
			Rewrite(FGens);
			IOResultToException();
			NGens = 0;
		}
		catch (std::exception &e)
		{
			{
				DoSimpleMsg(String("Error creating Generators.dss for Energymeter: ") + this->get_Name()
	           + ". "
	           + (std::string) e.what(), 533);
				CloseFile(FGens); System::InOutRes = 0;
				return;
			}
		}
		try
		{
			AssignFile(FCaps, "Capacitors.dss");
			Rewrite(FCaps);
			IOResultToException();
			NCaps = 0;
		}
		catch (std::exception &e)
		{
			{
				DoSimpleMsg(String("Error creating Capacitors.dss for Energymeter: ") + this->get_Name()
	           + ". "
	           + (std::string) e.what(), 534);
				CloseFile(FCaps); System::InOutRes = 0;
				return;
			}
		}
		CktElem = ((TDSSCktElement*) BranchList->Get_First());
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			while(CktElem != nullptr)
			{
				if(CktElem->Get_Enabled())
				{
					ActiveCircuit[ActiveActor]->Set_ActiveCktElement(CktElem);
					if((CktElem->DSSObjType & CLASSMASK) == XFMR_ELEMENT)
					{
						++NXfmrs;
						WriteActiveDSSObject(FXfmrs, "New");     // sets HasBeenSaved := TRUE
						if(CktElem->HasControl)
						{
							pControlElem = (TDSSCktElement*) CktElem->ControlElementList.Get_First();
							while(pControlElem != nullptr)
							{
								ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pControlElem);
								WriteActiveDSSObject(FXfmrs, "New");  //  regulator control ...Also, relays, switch controls
								pControlElem = (TDSSCktElement*) CktElem->ControlElementList.Get_Next();
							}
						}
					}
					else
  /*Mostly LINE elements*/
					{
						++NBranches;
						WriteActiveDSSObject(FBranches, "New");     // sets HasBeenSaved := TRUE
						if(CktElem->HasControl)
						{
							pControlElem = (TDSSCktElement*) CktElem->ControlElementList.Get_First();
							while(pControlElem != nullptr)
							{
								ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pControlElem);
								WriteActiveDSSObject(FBranches, "New");  //  regulator control ...Also, relays, switch controls
								pControlElem = (TDSSCktElement*) CktElem->ControlElementList.Get_Next();
							}
						}
					}
					shuntElement = ((TDSSCktElement*) BranchList->Get_FirstObject());
					while(shuntElement != nullptr)
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(shuntElement);
						if((shuntElement->DSSObjType & CLASSMASK) == LOAD_ELEMENT)
						{
							LoadElement = ((TLoadObj*) shuntElement);
							if(LoadElement->HasBeenAllocated)
                       /*Manually set the allocation factor so it shows up*/
							{
								Parser[ActiveActor]->SetCmdString(String("allocationfactor=") + Format("%-.4g", LoadElement->get_FAllocationFactor()));
								( (TDSSObject*) LoadElement )->Edit(ActiveActor);
							}
							ActiveCircuit[ActiveActor]->Set_ActiveCktElement(shuntElement); // reset in case Edit mangles it
							++Nloads;
							WriteActiveDSSObject(FLoads, "New");
						}
						else
						{
							if((shuntElement->DSSObjType & CLASSMASK) == GEN_ELEMENT)
							{
								++NGens;
								WriteActiveDSSObject(FGens, "New");
								if(shuntElement->HasControl)
								{
									pControlElem = (TDSSCktElement*) shuntElement->ControlElementList.Get_First();
									while(pControlElem != nullptr)
									{
										ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pControlElem);
										WriteActiveDSSObject(FGens, "New");
										pControlElem = (TDSSCktElement*) shuntElement->ControlElementList.Get_Next();
									}
								}
							}
							else
							{
								if((shuntElement->DSSObjType & CLASSMASK) == CAP_ELEMENT)
								{
									++NCaps;
									WriteActiveDSSObject(FCaps, "New");
									if(shuntElement->HasControl)
									{
										pControlElem = (TDSSCktElement*) shuntElement->ControlElementList.Get_First();
										while(pControlElem != nullptr)
										{
											ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pControlElem);
											WriteActiveDSSObject(FCaps, "New");
											pControlElem = (TDSSCktElement*) shuntElement->ControlElementList.Get_Next();
										}
									}
								}
								else
								{
									++NShunts;
									WriteActiveDSSObject(FShunts, "New");
								}
							}
						}
						shuntElement = ((TDSSCktElement*) BranchList->Get_NextObject());
					}
				} /*if enabled*/
				CktElem = ((TDSSCktElement*) BranchList->Get_Forward());
			}
		}/*WHILE*/
		CloseFile(FBranches);
		CloseFile(FXfmrs);
		CloseFile(FShunts);
		CloseFile(FLoads);
		CloseFile(FGens);
		CloseFile(FCaps);

     /*If any records were written to the file, record their relative names*/
		if(NBranches > 0)
			SavedFileList[ActiveActor].push_back(DirName + DIRSEP_STR "Branches.dss");
		else
			DeleteFile("Branches.dss");
		if(NXfmrs > 0)
			SavedFileList[ActiveActor].push_back(DirName + DIRSEP_STR "Transformers.dss");
		else
			DeleteFile("Transformers.dss");
		if(NShunts > 0)
			SavedFileList[ActiveActor].push_back(DirName + DIRSEP_STR "Shunts.dss");
		else
			DeleteFile("Shunts.dss");
		if(Nloads > 0)
			SavedFileList[ActiveActor].push_back(DirName + DIRSEP_STR "Loads.dss");
		else
			DeleteFile("Loads.dss");
		if(NGens > 0)
			SavedFileList[ActiveActor].push_back(DirName + DIRSEP_STR "Generators.dss");
		else
			DeleteFile("Generators.dss");
		if(NCaps > 0)
			SavedFileList[ActiveActor].push_back(DirName + DIRSEP_STR "Capacitors.dss");
		else
			DeleteFile("Capacitors.dss");
	} /*IF*/
}

void TEnergyMeterObj::GetPCEatZone()
{
	TDSSCktElement* CktElem = nullptr;
	TDSSCktElement* shuntElement = nullptr;
	std::vector <String> myPCEList;
	TEnergyMeterObj* pMeter = nullptr;
	if(ActiveCircuit[ActiveActor] != nullptr)
	{
		ZonePCE.resize( 0 );
		/*# with ActiveCircuit[ActiveActor] do */
		{
			
			if(BranchList != nullptr)
			{
				CktElem = ((TDSSCktElement*) BranchList->Get_First());
				while(CktElem != nullptr)
				{
					if(CktElem->Get_Enabled())
					{
						ActiveCircuit[ActiveActor]->Set_ActiveCktElement(CktElem);
						shuntElement = ((TDSSCktElement*) BranchList->Get_FirstObject());
						while(shuntElement != nullptr)
						{
							ActiveCircuit[ActiveActor]->Set_ActiveCktElement(shuntElement);
							ZonePCE.resize( ZonePCE.size() + 1);
							ZonePCE[ZonePCE.size() - 1] = shuntElement->Get_myPName() + "." + shuntElement->get_Name();
							shuntElement = ((TDSSCktElement*) BranchList->Get_NextObject());
						}
					}
					CktElem = ((TDSSCktElement*) BranchList->Get_Forward());
				}
			}
		}
	}
	if (ZonePCE.size() == 0)
	{
		ZonePCE.resize( 1 );
		ZonePCE[0] = "";
	}	
}

void TEnergyMeterObj::SetDragHandRegister(int reg, double Value)
{
	if(Value > Registers[reg])
	{
		Registers[reg] = Value;
		Derivatives[reg] = Value;  // Use this for   demand interval data;
	}
}

void TEnergyMeterObj::CloseDemandIntervalFile(int ActorID)
{
	int i = 0;
	try
	{
		if(This_Meter_DIFileIsOpen)
		{
			if(DI_MHandle != nullptr)
				CloseMHandler(DI_MHandle, MakeDIFileName(ActorID), DI_Append[ActorID]);
			This_Meter_DIFileIsOpen = false;
			if(PHV_MHandle != nullptr)
			{
				if(VPhaseReportFileIsOpen)
					CloseMHandler(PHV_MHandle, MakeVPhaseReportFileName(ActorID), PHV_Append[ActorID]);
			}
			VPhaseReportFileIsOpen = false;
		}
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error Closing Demand Interval file for Meter \"") + get_Name()
	           + "\"", 534);
	}


     /*Write Registers to Totals File*/
	/*# with EnergyMeterClass[ActorID] do */
	{
		
		int stop = 0;
		WriteintoMemStr(EMT_MHandle[ActorID], String("\"") + this->get_Name() + "\"");
		for(stop = NumEMRegisters, i = 1; i <= stop; i++)
		{
			WriteintoMem(EMT_MHandle[ActorID], Registers[i - 1]);
		}
		WriteintoMemStr(EMT_MHandle[ActorID], CHAR10);
	}
}

void TEnergyMeterObj::OpenDemandIntervalFile(int ActorID)
{
	int i = 0;
	int j = 0;
	double VBase = 0.0;
	try
	{
		if(This_Meter_DIFileIsOpen)
			CloseDemandIntervalFile(ActorID);
		if(EnergyMeterClass[ActorID]->Get_DI_Verbose(ActorID))
		{
			int stop = 0;
			This_Meter_DIFileIsOpen = true;
			if(DI_MHandle != nullptr)
				delete DI_MHandle;
			DI_MHandle = Create_Meter_Space("\"Hour\"");
			for(stop = NumEMRegisters, i = 1; i <= stop; i++)
			{
				WriteintoMemStr(DI_MHandle, String(", \"") + RegisterNames[i - 1] + "\"");
			}
			WriteintoMemStr(DI_MHandle, CHAR10);

         /*Phase Voltage Report, if requested*/
			if(FPhaseVoltageReport)
			{
				int stop = 0;
				if(PHV_MHandle != nullptr)
					delete PHV_MHandle;
				PHV_MHandle = Create_Meter_Space("\"Hour\"");
				VPhaseReportFileIsOpen = true;
				for(stop = MaxVBaseCount, i = 1; i <= stop; i++)
				{
					VBase = (VBaseList)[i] * SQRT3;
					if(VBase > 0.0)
					{
						int stop1 = 0;
						for(stop1 = 3, j = 1; j <= stop1; j++)
						{
							WriteintoMemStr(PHV_MHandle, Format(", %.3gkV_Phs_%d_Max", VBase, j));
						}
						for(stop1 = 3, j = 1; j <= stop1; j++)
						{
							WriteintoMemStr(PHV_MHandle, Format(", %.3gkV_Phs_%d_Min", VBase, j));
						}
						for(stop1 = 3, j = 1; j <= stop1; j++)
						{
							WriteintoMemStr(PHV_MHandle, Format(", %.3gkV_Phs_%d_Avg", VBase, j));
						}
					}
				}
              // WriteintoMemStr(PHV_MHandle, ', Min Bus, MaxBus' + Char(10));  // This has not been recorded
				WriteintoMemStr(PHV_MHandle, CHAR10);  // This has not been recorded
			}
		}
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error opening demand interval file \"") + get_Name()
	           + ".CSV"
	           + " for writing."
	           + CRLF
	           + (std::string) e.what(), 535);
	}
}

void TEnergyMeterObj::WriteDemandIntervalData(int ActorID)
{
	int i = 0;
	int j = 0;

	auto MyCount_Avg = [&](double Value, int Count) -> double 
	{
		double result = 0.0;
		if(Count == 0)
			result = 0.0;
		else
			result = Value / Count;
		return result;
	};
	if(EnergyMeterClass[ActorID]->Get_DI_Verbose(ActorID) && This_Meter_DIFileIsOpen)
	{
		int stop = 0;
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			WriteintoMem(DI_MHandle, with0->DynaVars.dblHour);
		}
		for(stop = NumEMRegisters, i = 1; i <= stop; i++)
		{
			WriteintoMem(DI_MHandle, Derivatives[i - 1]);
		}
		WriteintoMemStr(DI_MHandle, CHAR10);
	}

      /*Add to Class demand interval registers*/
	/*# with EnergyMeterClass[ActorID] do */
	{
		for (i = 0; i < NumEMRegisters; i++)
		{
			EnergyMeterClass[ActorID]->DI_RegisterTotals[i] = EnergyMeterClass[ActorID]->DI_RegisterTotals[i] + Derivatives[i] * TotalsMask[i];
		}
	}


      /*Phase Voltage Report, if requested*/
	if(VPhaseReportFileIsOpen)
	{
		int stop = 0;
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with2 = ActiveCircuit[ActorID]->Solution;
			WriteintoMem(PHV_MHandle, with2->DynaVars.dblHour);
		}
		for(stop = MaxVBaseCount, i = 1; i <= stop; i++)
		{
			if((VBaseList)[i] > 0.0)
			{
				int stop1 = 0;
				for(stop1 = 3, j = 1; j <= stop1; j++)
				{
					WriteintoMem(PHV_MHandle, 0.001 * (VphaseMax)[jiIndex(j, i)]);
				}
				for(stop1 = 3, j = 1; j <= stop1; j++)
				{
					WriteintoMem(PHV_MHandle, 0.001 * (VPhaseMin)[jiIndex(j, i)]);
				}
				for(stop1 = 3, j = 1; j <= stop1; j++)
				{
					WriteintoMem(PHV_MHandle, 0.001 * MyCount_Avg((VPhaseAccum)[jiIndex(j, i)], (VPhaseAccumCount)[jiIndex(j, i)]));
				}
			}
		}
		WriteintoMemStr(PHV_MHandle, CHAR10);
	}
}

void TEnergyMeter::CloseAllDIFiles(int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
	if(FSaveDemandInterval)
        /*While closing DI files, write all meter registers to one file*/
	{
		try
		{
			CreateMeterTotals(ActorID);
		}
		catch (std::exception &e)
		{
			DoSimpleMsg(String("Error on Rewrite of totals file: ") + (std::string) e.what(), 536);
		}

        /*Close all the DI file for each meter*/
		mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
		while(mtr != nullptr)
		{
			if(mtr->Get_Enabled())
				mtr->CloseDemandIntervalFile(ActorID);
			mtr =(TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_Next();
		}
		WriteTotalsFile(ActorID);  // Sum all energymeter registers to "Totals.CSV"
		SystemMeter->CloseDemandIntervalFile(ActorID);
		SystemMeter->Save(ActorID);
		if(EMT_MHandle[ActorID] != nullptr)
			CloseMHandler(EMT_MHandle[ActorID], DI_Dir + DIRSEP_STR "EnergyMeterTotals_" + IntToStr(ActorID) + ".CSV", EMT_Append[ActorID]);
		EMT_MHandle[ActorID] = nullptr;
		if(TDI_MHandle[ActorID] != nullptr)
			CloseMHandler(TDI_MHandle[ActorID], DI_Dir + DIRSEP_STR "DI_Totals_" + IntToStr(ActorID) + ".CSV", TDI_Append[ActorID]);
		DIFilesAreOpen[ActorID] = false;
		if(OverLoadFileIsOpen)
		{
			if(OV_MHandle[ActorID] != nullptr)
				CloseMHandler(OV_MHandle[ActorID], EnergyMeterClass[ActorID]->DI_Dir + DIRSEP_STR "DI_Overloads_" + IntToStr(ActorID) + ".CSV", OV_Append[ActorID]);
			OverLoadFileIsOpen = false;
		}
		if(VoltageFileIsOpen)
		{
			if(VR_MHandle[ActorID] != nullptr)
				CloseMHandler(VR_MHandle[ActorID], EnergyMeterClass[ActorID]->DI_Dir + DIRSEP_STR "DI_VoltExceptions_" + IntToStr(ActorID) + ".CSV", VR_Append[ActorID]);
			VoltageFileIsOpen = false;
		}
	}
}

void TEnergyMeterObj::AppendDemandIntervalFile(int ActorID)
{
	String FileNm;

  /*Only called if "SaveDemandInterval"*/
	if(This_Meter_DIFileIsOpen)
		return;
	try
	{
		if(EnergyMeterClass[ActorID]->FDI_Verbose)
		{
			FileNm = MakeDIFileName(ActorID);   // Creates directory if it doesn't exist
			if(FileExists(FileNm))
				DI_Append[ActorID] = true;
			else
				DI_Append[ActorID] = false;
			if(DI_MHandle != nullptr)
				delete DI_MHandle;
			DI_MHandle = Create_Meter_Space(" ");
			This_Meter_DIFileIsOpen = true;
		}
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error opening demand interval file \"") + get_Name()
	           + ".CSV"
	           + " for appending."
	           + CRLF
	           + (std::string) e.what(), 537);
	}
}

void TEnergyMeterObj::AssignVoltBaseRegisterNames()
{
	int i = 0;
	int ireg = 0;
	double VBase = 0.0;
	int stop = 0;
	ireg = 1;
	for(stop = MaxVBaseCount, i = 1; i <= stop; i++)
	{
		if((VBaseList)[i] > 0.0)
		{
			VBase = (VBaseList)[i] * SQRT3;
			RegisterNames[i + Reg_VBaseStart] = Format("%.3g kV Losses", VBase);
			RegisterNames[i + 1 * MaxVBaseCount + Reg_VBaseStart] = Format("%.3g kV Line Loss", VBase);
			RegisterNames[i + 2 * MaxVBaseCount + Reg_VBaseStart] = Format("%.3g kV Load Loss", VBase);
			RegisterNames[i + 3 * MaxVBaseCount + Reg_VBaseStart] = Format("%.3g kV No Load Loss", VBase);
			RegisterNames[i + 4 * MaxVBaseCount + Reg_VBaseStart] = Format("%.3g kV Load Energy", VBase);
		}
		else
		{
			RegisterNames[i + Reg_VBaseStart] = Format("Aux%d", ireg);
			++ireg;
			RegisterNames[i + 1 * MaxVBaseCount + Reg_VBaseStart] = Format("Aux%d", ireg);
			++ireg;
			RegisterNames[i + 2 * MaxVBaseCount + Reg_VBaseStart] = Format("Aux%d", ireg);
			++ireg;
			RegisterNames[i + 3 * MaxVBaseCount + Reg_VBaseStart] = Format("Aux%d", ireg);
			++ireg;
			RegisterNames[i + 4 * MaxVBaseCount + Reg_VBaseStart] = Format("Aux%d", ireg);
			++ireg;
		}
	}
	for(stop = NumEMRegisters, i = 1 + Reg_VBaseStart + 5 * MaxVBaseCount; i < stop; i++)
	{
		RegisterNames[i - 1] = Format("Aux%d", ireg);
		++ireg;
	}
}

void TEnergyMeter::AppendAllDIFiles(int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
	String FileNm;
	if(FSaveDemandInterval)
	{
		ClearDI_Totals();  // clears accumulator arrays
		mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
		while(mtr != nullptr)
		{
			if(mtr->Get_Enabled())
				mtr->AppendDemandIntervalFile(ActorID);
			mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_Next();
		}
		SystemMeter->AppendDemandIntervalFile(ActorID);

          /*Open FDI_Totals*/
		try
		{
			FileNm = DI_Dir + DIRSEP_STR "DI_Totals_" + IntToStr(ActorID) + ".CSV";
              /*File Must Exist*/
			if(FileExists(FileNm))
				TDI_Append[ActorID] = true;
			CreateFDI_Totals(ActorID);
		}
		catch (std::exception &e)
		{
			DoSimpleMsg(String("Error opening demand interval file \"") + get_myClass_name()
	           + ".CSV"
	           + " for appending."
	           + CRLF
	           + (std::string) e.what(), 538);
		}
		DIFilesAreOpen[ActorID] = true;
	}/*IF*/
}

String TEnergyMeterObj::MakeDIFileName(int ActorID)
{
	String result;
	result = EnergyMeterClass[ActorID]->DI_Dir + DIRSEP_STR + this->get_Name() + "_" + IntToStr(ActorID) + ".CSV";
	return result;
}

void TEnergyMeter::Set_SaveDemandInterval(int ActorID, bool Value)
{
	FSaveDemandInterval = Value;
	ResetAll(ActorID);
}

bool TEnergyMeter::Get_SaveDemandInterval(int ActorID)
{
	bool result = false;
	result = FSaveDemandInterval;
	return result;
}

void TEnergyMeter::WriteOverloadReport(int ActorID)
{
	TPDElement* PDElem = nullptr;
	double EmergAmps = 0.0;
	double NormAmps = 0.0;
	double cMax = 0.0;
	TEnergyMeterObj* mtr = nullptr;
	String ClassName;
	TXYcurveObj* RSignal = nullptr;
	int i = 0;
	int j = 0;
	int k = 0;
	int RatingIdx = 0;
	std::vector <double> cVector;
	std::vector <double> cBuffer;
/*
  Scans the active circuit for overloaded PD elements and writes each to a file
  This is called only if in Demand Interval (DI) mode and the file is open.
*/
/*    Prepares everything for using seasonal ratings if required*/
	if(SeasonalRating)
	{
		if(SeasonSignal != "")
		{
			RSignal = ((TXYcurveObj*) XYCurveClass[ActorID]->Find(SeasonSignal));
			if(RSignal != nullptr)
				RatingIdx = Trunc(RSignal->GetYValue_(ActiveCircuit[ActorID]->Solution->DynaVars.intHour));
			else
				SeasonalRating = false;   // The XYCurve defined doesn't exist
		}
		else
		SeasonalRating = false;    // The user didn't define the seasonal signal
	}

 /* CHECK PDELEMENTS ONLY*/
	PDElem = (TPDElement*) ActiveCircuit[ActorID]->PDElements.Get_First();
	while(PDElem != nullptr)
	{
		if( (PDElem->Get_Enabled()) &&  !(PDElem->IsShunt) )   // Ignore shunts
		{
			if((PDElem->NormAmps > 0.0) || (PDElem->EmergAmps > 0.0))
			{
				PDElem->ComputeIterminal(ActorID);
				cMax = PDElem->MaxTerminalOneIMag(ActorID); // For now, check only terminal 1 for overloads

             // Section introduced in 02/20/2019 for allowing the automatic change of ratings
             // when the seasonal ratings option is active
				ClassName = LowerCase(PDElem->Get_myPName());
             //if SeasonalRating and (ClassName = 'line') and (PDElem.NumAmpRatings > 1) then
				if(SeasonalRating && (PDElem->NumAmpRatings > 1))  // Includes all PDE
				{
					if(RatingIdx > PDElem->NumAmpRatings)
					{
						NormAmps = PDElem->NormAmps;
						EmergAmps = PDElem->EmergAmps;
					}
					else
					{
						NormAmps = PDElem->AmpRatings[RatingIdx];
						EmergAmps = PDElem->AmpRatings[RatingIdx];
					}
				}
				else
				{
					NormAmps = PDElem->NormAmps;
					EmergAmps = PDElem->EmergAmps;
				}
				if((cMax > NormAmps) || (cMax > EmergAmps))

              // Gets the currents for the active Element
				{
					int stop = 0;
					cBuffer.resize( PDElem->Get_NPhases() * PDElem->Get_NTerms() + 1 );
					PDElem->Get_Current_Mags(&(cBuffer[0]), ActorID);
					cVector.resize( 3 + 1 ); // for storing
					for(stop = 3, i = 1; i <= stop; i++)
					{
						cVector[i] = 0.0;
					}
					if(PDElem->Get_NPhases() < 3)
					{
						int stop = 0;
						ClassName = PDElem->Get_FirstBus();
						j = AnsiPos(".", ClassName);     // Removes the name of the bus
						ClassName = ClassName.substr(j, ClassName.size());
						for(stop = 3, i = 1; i <= stop; i++)
						{
							j = AnsiPos(".", ClassName);   // goes for the phase Number
							if(j == 0)
							{
								k = StrToInt(ClassName);
								cVector[k] = cBuffer[i - 1];
								break;
							}
							else
							{
								k = StrToInt(ClassName.substr(0, j - 1));
								cVector[k] = cBuffer[i - 1];
								ClassName = ClassName.substr(j, ClassName.size());
							}
						}
					}
					else
					{
						int stop = 0;
						for(stop = 3, i = 1; i <= stop; i++)
						{
							cVector[i] = cBuffer[i - 1];
						}
					}
					/*# with ActiveCircuit[ActorID].Solution do */
					{
						auto with0 = ActiveCircuit[ActorID]->Solution;
						WriteintoMem(OV_MHandle[ActorID], with0->DynaVars.dblHour);
					}
					WriteintoMemStr(OV_MHandle[ActorID], String(", ") + FullName( PDElem ) );
					WriteintoMem(OV_MHandle[ActorID], NormAmps);
					WriteintoMem(OV_MHandle[ActorID], EmergAmps);
					if(PDElem->NormAmps > 0.0)
						WriteintoMem(OV_MHandle[ActorID], cMax / NormAmps * 100.0);
					else
						WriteintoMem(OV_MHandle[ActorID], 0.0);
					if(PDElem->EmergAmps > 0.0)
						WriteintoMem(OV_MHandle[ActorID], cMax / EmergAmps * 100.0);
					else
						WriteintoMem(OV_MHandle[ActorID], 0.0);
					/*# with ActiveCircuit[ActorID] do */
					{
						 // Find bus of first terminal
						WriteintoMem(OV_MHandle[ActorID], ActiveCircuit[ActorID]->Buses[ ActiveCircuit[ActorID]->MapNodeToBus[ ( PDElem->NodeRef)[1 - 1] - 1 ].BusRef - 1]->kVBase);
					}
              // Adds the currents in Amps per phase at the end of the report
					for(stop = 3, i = 1; i <= stop; i++)
					{
						WriteintoMem(OV_MHandle[ActorID], cVector[i]);
					}
					WriteintoMemStr(OV_MHandle[ActorID], " " + CHAR10);
				}
			} /* */
		}
		PDElem = (TPDElement*) ActiveCircuit[ActorID]->PDElements.Get_Next();
	}
}

void TEnergyMeter::ClearDI_Totals()
{
	int i;
	for (i = 0; i < NumEMRegisters; i++)
	{
		DI_RegisterTotals[i] = 0.0;
	}
}

void TEnergyMeter::CreateFDI_Totals(int ActorID)
{
	int i = 0;
	TEnergyMeterObj* mtr = nullptr;
	try
	{
		if(TDI_MHandle[ActorID] != nullptr)
			delete TDI_MHandle[ActorID];
		TDI_MHandle[ActorID] = Create_Meter_Space("Time");
		mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();  // just get the first one
		if(ASSIGNED(mtr))
		{
			int stop = 0;
			for(stop = NumEMRegisters, i = 1; i <= stop; i++)
			{
				WriteintoMemStr(TDI_MHandle[ActorID], String(", \"") + mtr->RegisterNames[i - 1] + "\"");
			}
		}
		WriteintoMemStr(TDI_MHandle[ActorID], CHAR10);
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error creating: \"") + DI_Dir
	           + DIRSEP_STR "DI_Totals_"
	           + IntToStr(ActorID)
	           + ".CSV\": "
	           + (std::string) e.what(), 539);
	}
}

/* TSystemMeter */

void TSystemMeter::AppendDemandIntervalFile(int ActorID)
{
	String FileNm;

  /*Only called if "SaveDemandInterval"*/
	if(This_Meter_DIFileIsOpen)
		return;
	try
	{
		FileNm = EnergyMeterClass[ActorID]->DI_Dir + DIRSEP_STR "DI_SystemMeter_" + IntToStr(ActorID) + ".CSV";
		AssignFile(SystemDIFile, FileNm);
      /*File Must Exist*/
		if(FileExists(FileNm))
			;
		else

//        DI_MMFView:=  MapFile2Memory(EnergyMeterClass.DI_Dir+'\DI_SystemMeter.CSV', DI_MMFHandle);
//        DI_Cursor :=  GetMMFCursor(DI_MMFView);
			OpenDemandIntervalFile(ActorID);

		IOResultToException();
		This_Meter_DIFileIsOpen = true;
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error opening demand interval file \"") + FileNm
	           + " for appending."
	           + CRLF
	           + (std::string) e.what(), 540);
	}
}

void TSystemMeter::Clear()
{
	kWh = 0.0;
	kvarh = 0.0;
	peakkW = 0.0;
	peakkVA = 0.0;
	Losseskwh = 0.0;
	Losseskvarh = 0.0;
	PeakLosseskW = 0.0;
	dkWh = 0.0;
	dkvarh = 0.0;
	dLosseskWh = 0.0;
	dlosseskvarh = 0.0;
	FirstSampleAfterReset = true;
}

void TSystemMeter::CloseDemandIntervalFile(int ActorID)
{
	String File_Path;
	TEnergyMeterObj* mtr = nullptr;
	if(This_Meter_DIFileIsOpen)
	{
		File_Path = EnergyMeterClass[ActorID]->DI_Dir + DIRSEP_STR "DI_SystemMeter_" + IntToStr(ActorID) + ".CSV";
		CloseMHandler(SDI_MHandle[ActorID], File_Path, SDI_Append[ActorID]);
		This_Meter_DIFileIsOpen = false;
	}
}

TSystemMeter::TSystemMeter()
 : kWh(0.0),
			dkWh(0.0),
			kvarh(0.0),
			dkvarh(0.0),
			peakkW(0.0),
			peakkVA(0.0),
			Losseskwh(0.0),
			dLosseskWh(0.0),
			Losseskvarh(0.0),
			dlosseskvarh(0.0),
			PeakLosseskW(0.0),
			FirstSampleAfterReset(false),
			This_Meter_DIFileIsOpen(false)
{
	Clear();
	This_Meter_DIFileIsOpen = false;
	SDI_MHandle[ActiveActor] = nullptr;
	TDI_MHandle[ActiveActor] = nullptr;
	VR_MHandle[ActiveActor] = nullptr;
	OV_MHandle[ActiveActor] = nullptr;
}

TSystemMeter::~TSystemMeter()
{
	if(SDI_MHandle[ActiveActor] != nullptr)
		delete SDI_MHandle[ActiveActor];
	if(TDI_MHandle[ActiveActor] != nullptr)
		delete TDI_MHandle[ActiveActor];
	if(VR_MHandle[ActiveActor] != nullptr)
		delete VR_MHandle[ActiveActor];
	if(OV_MHandle[ActiveActor] != nullptr)
		delete OV_MHandle[ActiveActor];
	// inherited;
}


void TSystemMeter::Integrate(double& reg, double Value, double& Deriv, int ActorID)
{
	if(ActiveCircuit[ActorID]->TrapezoidalIntegration)
        /*Trapezoidal Rule Integration*/
	{
		if(!FirstSampleAfterReset)
			reg = reg + 0.5 * ActiveCircuit[ActorID]->Solution->IntervalHrs * (Value + Deriv);
	}
	else
   /*Plain Euler integration*/
	reg = reg + ActiveCircuit[ActorID]->Solution->IntervalHrs * Value;
	Deriv = Value;
}

void TSystemMeter::OpenDemandIntervalFile(int ActorID)
{
	String F_header;
	TEnergyMeterObj* mtr = nullptr;
	try
	{
		if(This_Meter_DIFileIsOpen)
			delete SDI_MHandle[ActorID];
		This_Meter_DIFileIsOpen = true;
		if(SDI_MHandle[ActorID] != nullptr)
			delete SDI_MHandle[ActorID];
		SDI_MHandle[ActorID] = Create_Meter_Space("\"Hour\", ");
		WriteintoMemStr(SDI_MHandle[ActorID], "kWh, kvarh, \"Peak kW\", \"peak kVA\", \"Losses kWh\", \"Losses kvarh\", \"Peak Losses kW\"" + 
	           CHAR10);
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error opening demand interval file \"DI_SystemMeter.CSV\"  for writing.") + CRLF
	           + (std::string) e.what(), 541);
	}
}

void TSystemMeter::Reset()
{
	Clear();
   // removed - open in solution If EnergyMeterClass.SaveDemandInterval Then OpenDemandIntervalFile;
}

void TSystemMeter::Save(int ActorID)
{
	TTextRec f = {};
	String CSVName;
	String Folder;
	TEnergyMeterObj* mtr = nullptr;
	try
	{
		CSVName = String("SystemMeter_") + IntToStr(ActorID) + ".CSV";
       /*If we are doing a simulation and saving interval data, create this in the
        same directory as the demand interval data*/
		if(EnergyMeterClass[ActorID]->Get_SaveDemandInterval(ActorID))
			Folder = EnergyMeterClass[ActorID]->DI_Dir + DIRSEP_STR;
		else
			Folder = GetOutputDirectory();
		GlobalResult = CSVName;
		SetLastResultFile(CSVName);
	}
	catch (std::exception &e)
	{
		{
			DoSimpleMsg(String("Error opening System Meter File \"") + CRLF
	           + CSVName
	           + "\": "
	           + (std::string) e.what(), 542);
			return;
		}
	}
	try
	{
		if(SM_MHandle[ActorID] != nullptr)
			delete SM_MHandle[ActorID];
		SM_MHandle[ActorID] = Create_Meter_Space("Year, ");
		WriteintoMemStr(SM_MHandle[ActorID], "kWh, kvarh, \"Peak kW\", \"peak kVA\", \"Losses kWh\", \"Losses kvarh\", \"Peak Losses kW\"" + 
	          CHAR10);
		WriteintoMemStr(SM_MHandle[ActorID], IntToStr(ActiveCircuit[ActorID]->Solution->get_Fyear()));
		WriteRegisters(f, ActorID);
		WriteintoMemStr(SM_MHandle[ActorID], CHAR10);
/* }
	__finally
	{*/
		CloseMHandler(SM_MHandle[ActorID], Folder + CSVName, SM_Append[ActorID]);
		SM_MHandle[ActorID] = nullptr;
	}
	catch (...)
	{
		//
	}
}

void TSystemMeter::TakeSample(int ActorID)
{


  /*Get total system energy out of the sources*/
	cPower = cmulreal(GetTotalPowerFromSources(ActorID), 0.001);  // convert to kW
	Integrate(kWh, cPower.re, dkWh, ActorID);
	Integrate(kvarh, cPower.im, dkvarh, ActorID);  // TODO TEMc
	peakkW = max(cPower.re, peakkW);
	peakkVA = max(cabs(cPower), peakkVA); 

  /*Get total circuit losses*/
	cLosses = ActiveCircuit[ActorID]->Get_Losses(ActorID);  // PD Elements except shunts
	cLosses = cmulreal(cLosses, 0.001);  // convert to kW
	Integrate(Losseskwh, cLosses.re, dLosseskWh, ActorID);
	Integrate(Losseskvarh, cLosses.im, dlosseskvarh, ActorID);  // TODO TEMc
	PeakLosseskW = max(cLosses.re, PeakLosseskW);
	FirstSampleAfterReset = false;
	if(This_Meter_DIFileIsOpen)
		WriteDemandIntervalData(ActorID);
}

void TEnergyMeter::CreateMeterTotals(int ActorID)
{
	int i = 0;
	TEnergyMeterObj* mtr = nullptr;
	if(EMT_MHandle[ActorID] != nullptr)
		delete EMT_MHandle[ActorID];
	EMT_MHandle[ActorID] = Create_Meter_Space("Name");
	mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
	if(ASSIGNED(mtr))
	{
		int stop = 0;
		for(stop = NumEMRegisters, i = 1; i <= stop; i++)
		{
			WriteintoMemStr(EMT_MHandle[ActorID], String(", \"") + mtr->RegisterNames[i - 1] + "\"");
		}
	}
	WriteintoMemStr(EMT_MHandle[ActorID], CHAR10);
}

void TSystemMeter::WriteDemandIntervalData(int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		WriteintoMem(SDI_MHandle[ActorID], with0->DynaVars.dblHour);
	}
	WriteintoMem(SDI_MHandle[ActorID], cPower.re);
	WriteintoMem(SDI_MHandle[ActorID], cPower.im);
	WriteintoMem(SDI_MHandle[ActorID], peakkW);
	WriteintoMem(SDI_MHandle[ActorID], peakkVA);
	WriteintoMem(SDI_MHandle[ActorID], cLosses.re);
	WriteintoMem(SDI_MHandle[ActorID], cLosses.im);
	WriteintoMem(SDI_MHandle[ActorID], PeakLosseskW);
	WriteintoMemStr(SDI_MHandle[ActorID], CHAR10);
}

void TSystemMeter::WriteRegisterNames(TTextRec& f)
{

// Does nothing
}

void TSystemMeter::WriteRegisters(TTextRec& f, int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
	WriteintoMem(SM_MHandle[ActorID], kWh);
	WriteintoMem(SM_MHandle[ActorID], kvarh);
	WriteintoMem(SM_MHandle[ActorID], peakkW);
	WriteintoMem(SM_MHandle[ActorID], peakkVA);
	WriteintoMem(SM_MHandle[ActorID], Losseskwh);
	WriteintoMem(SM_MHandle[ActorID], Losseskvarh);
	WriteintoMem(SM_MHandle[ActorID], PeakLosseskW);
}

void TEnergyMeter::Set_DI_Verbose(int ActorID, bool Value)
{
	FDI_Verbose = Value;
	ResetAll(ActorID);
}

bool TEnergyMeter::Get_DI_Verbose(int ActorID)
{
	bool result = false;
	result = FDI_Verbose;
	return result;
}

void TEnergyMeter::WriteTotalsFile(int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
	TRegisterArray Regsum;
	int i = 0;
	TTextRec f = {};
  /*Sum up all registers of all meters and write to Totals.CSV*/
	int stop = 0;
	for(stop = NumEMRegisters, i = 1; i <= stop; i++)
	{
		Regsum[i - 1] = 0.0;
	}
	mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
	while(mtr != nullptr)
	{
		if(mtr->Get_Enabled())
			/*# with mtr do */
			{
				auto with0 = mtr;
				int stop = 0;
				for(stop = NumEMRegisters, i = 1; i <= stop; i++)
				{
					Regsum[i - 1] = Regsum[i - 1] + with0->Registers[i - 1] * with0->TotalsMask[i - 1];
				}
			}
		mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_Next();
	}
	try
     // Writes the file
	{
		int stop = 0;
		if(FM_MHandle[ActorID] != nullptr)
			delete FM_MHandle[ActorID];
		FM_MHandle[ActorID] = Create_Meter_Space("Year");
		mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
		if(ASSIGNED(mtr))
		{
			int stop = 0;
			for(stop = NumEMRegisters, i = 1; i <= stop; i++)
			{
				WriteintoMemStr(FM_MHandle[ActorID], String(", \"") + mtr->RegisterNames[i - 1] + "\"");
			} //Write(F,', "', mtr.RegisterNames[i - 1],'"');
		}
		WriteintoMemStr(FM_MHandle[ActorID], CHAR10);
		WriteintoMemStr(FM_MHandle[ActorID], IntToStr(ActiveCircuit[ActorID]->Solution->get_Fyear()));
		for(stop = NumEMRegisters, i = 1; i <= stop; i++)
		{
			WriteintoMem(FM_MHandle[ActorID], Regsum[i - 1]);
		}
		WriteintoMemStr(FM_MHandle[ActorID], CHAR10);
		CloseMHandler(FM_MHandle[ActorID], DI_Dir + DIRSEP_STR "Totals_" + IntToStr(ActorID) + ".CSV", FM_Append[ActorID]);
		FM_MHandle[ActorID] = nullptr;
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error writing demand interval file Totals_") + IntToStr(ActorID)
	           + ".CSV."
	           + CRLF
	           + (std::string) e.what(), 543);
	}
}

void TEnergyMeter::WriteVoltageReport(int ActorID)
{
	int i = 0;
	int j = 0;
	int l = 0;
	double Vmagpu = 0.0;
	int UnderCount = 0;
	int OverCount = 0;
	double OverVmax = 0.0;
	double UnderVmin = 0.0;
	int MinBus = 0;
	int MaxBus = 0;
	bool BusCounted = false;
	TEnergyMeterObj* mtr = nullptr;
     /*For any bus with a defined voltage base, test for > Vmax or < Vmin*/
	OverCount = 0;
	UnderCount = 0;
	MinBus = 0;
	MaxBus = 0;
	/*# with ActiveCircuit[ActorID] do */
	{
		
		int stop = 0;
		OverVmax = ActiveCircuit[ActorID]->NormalMinVolts;
		UnderVmin = ActiveCircuit[ActorID]->NormalMaxVolts;
		for(stop = ActiveCircuit[ActorID]->NumBuses, i = 1; i <= stop; i++)
		{
			/*# with ActiveCircuit[ActorID]->Buses^[i] do */
			{
				
				BusCounted = false;
				if(ActiveCircuit[ActorID]->Buses[i - 1]->kVBase > 1.0)          // Primary Nodes first
				{
					int stop1 = 0;
					for(stop1 = ActiveCircuit[ActorID]->Buses[i - 1]->get_FNumNodesThisBus(), j = 1; j <= stop1; j++)
					{
						if(!ADiakoptics || (ActorID == 1))
							Vmagpu = cabs(ActiveCircuit[ActorID]->Solution->NodeV[ActiveCircuit[ActorID]->Buses[i - 1]->GetRef(j)]) / ActiveCircuit[ActorID]->Buses[i - 1]->kVBase * 0.001;
						else
							Vmagpu = cabs(ActiveCircuit[ActorID]->Solution->VoltInActor1(ActiveCircuit[ActorID]->Buses[i - 1]->GetRef(j))) / ActiveCircuit[ActorID]->Buses[i - 1]->kVBase * 0.001;
						if(Vmagpu > 0.1) // ignore neutral buses
						{
							if(Vmagpu < UnderVmin)
							{
								UnderVmin = Vmagpu;
								MinBus = i;
							}
							if(Vmagpu > OverVmax)
							{
								OverVmax = Vmagpu;
								MaxBus = i;
							}
							if(Vmagpu < ActiveCircuit[ActorID]->NormalMinVolts)
							{
								if(!BusCounted)     // Don't count more than once
								{
									++UnderCount;
									BusCounted = true;
								}
							}
							else
							{
								if(Vmagpu > ActiveCircuit[ActorID]->NormalMaxVolts)
								{
									if(!BusCounted)
									{
										++OverCount;
										BusCounted = true;
									}
								}
							}
						}
					}
				}
			}
		} /*For i*/
		/*# with Solution do */
		{
			auto with2 = ActiveCircuit[ActorID]->Solution;
			WriteintoMem(VR_MHandle[ActorID], with2->DynaVars.dblHour);
		}
		WriteintoMemStr(VR_MHandle[ActorID], String(", ") + IntToStr(UnderCount));
		WriteintoMem(VR_MHandle[ActorID], UnderVmin);
		WriteintoMemStr(VR_MHandle[ActorID], String(", ") + IntToStr(OverCount));
		WriteintoMem(VR_MHandle[ActorID], OverVmax);
		WriteintoMemStr(VR_MHandle[ActorID], String(", ") + ActiveCircuit[ActorID]->BusList.Get((unsigned int) MinBus));
		WriteintoMemStr(VR_MHandle[ActorID], String(", ") + ActiveCircuit[ActorID]->BusList.Get((unsigned int) MaxBus));

     // Klugy but it works
     // now repeat for buses under 1 kV
		OverCount = 0;
		UnderCount = 0;
		MinBus = 0;
		MaxBus = 0;
		OverVmax = ActiveCircuit[ActorID]->NormalMinVolts;
		UnderVmin = ActiveCircuit[ActorID]->NormalMaxVolts;
		for(stop = ActiveCircuit[ActorID]->NumBuses, i = 1; i <= stop; i++)
		{
			/*# with ActiveCircuit[ActorID]->Buses^[i] do */
			{
				
				BusCounted = false;
				if((ActiveCircuit[ActorID]->Buses[i - 1]->kVBase > 0.0) && (ActiveCircuit[ActorID]->Buses[i - 1]->kVBase <= 1.0))
				{
					int stop1 = 0;
					for(stop1 = ActiveCircuit[ActorID]->Buses[i - 1]->get_FNumNodesThisBus(), j = 1; j <= stop1; j++)
					{
						if(!ADiakoptics || (ActorID == 1))
							Vmagpu = cabs(ActiveCircuit[ActorID]->Solution->NodeV[ActiveCircuit[ActorID]->Buses[i - 1]->GetRef(j)]) / ActiveCircuit[ActorID]->Buses[i - 1]->kVBase * 0.001;
						else
							Vmagpu = cabs(ActiveCircuit[ActorID]->Solution->VoltInActor1(ActiveCircuit[ActorID]->Buses[i - 1]->GetRef(j))) / ActiveCircuit[ActorID]->Buses[i - 1]->kVBase * 0.001;
						if(Vmagpu > 0.1) // ignore neutral buses
						{
							if(Vmagpu < UnderVmin)
							{
								UnderVmin = Vmagpu;
								MinBus = i;
							}
							if(Vmagpu > OverVmax)
							{
								OverVmax = Vmagpu;
								MaxBus = i;
							}
							if(Vmagpu < ActiveCircuit[ActorID]->NormalMinVolts)
							{
								if(!BusCounted)     // Don't count more than once
								{
									++UnderCount;
									BusCounted = true;
								}
							}
							else
							{
								if(Vmagpu > ActiveCircuit[ActorID]->NormalMaxVolts)
								{
									if(!BusCounted)
									{
										++OverCount;
										BusCounted = true;
									}
								}
							}
						}
					}
				}
			}
		} /*For i*/
		WriteintoMemStr(VR_MHandle[ActorID], String(", ") + IntToStr(UnderCount));
		WriteintoMem(VR_MHandle[ActorID], UnderVmin);
		WriteintoMemStr(VR_MHandle[ActorID], String(", ") + IntToStr(OverCount));
		WriteintoMem(VR_MHandle[ActorID], OverVmax);
		WriteintoMemStr(VR_MHandle[ActorID], String(", ") + ActiveCircuit[ActorID]->BusList.Get((unsigned int) MinBus));
		WriteintoMemStr(VR_MHandle[ActorID], String(", ") + ActiveCircuit[ActorID]->BusList.Get((unsigned int) MaxBus));
		WriteintoMemStr(VR_MHandle[ActorID], CHAR10);
	}
}

void TEnergyMeter::InterpretRegisterMaskArray(TRegisterArray Mask, int ActorID)
{
	int i = 0;
	int n = 0;
	int stop = 0;
	n = Parser[ActorID]->ParseAsVector(NumEMRegisters, *(pDoubleArray*) & Mask);
	for(stop = NumEMRegisters, i = n + 1; i <= stop; i++)
	{
		Mask[i - 1] = 1.0;
	}  // Set the rest to 1
}

/* Feeder object code commented out
procedure TEnergyMeterObj.MakeFeederObj;
begin
  If Assigned(MeteredElement) Then Begin

    FeederClass.NewObject(Name);  // NewObject creates only if not existent. Else Inits  and desynchs
    FeederObj := ActiveCircuit[ActiveActor].ActiveCktElement as TFeederObj;
    FeederObj.SetBus (1, MeteredElement.GetBus(MeteredTerminal));
    FeederObj.Nphases := MeteredElement.NPhases;
    FeederObj.Nconds  := MeteredElement.Nconds;
    FeederObj.Enabled := ActiveCircuit[ActiveActor].RadialSolution;

  End
  Else DoSimpleMsg('Error: Attempted to make Feeder Obj without instantiating Metered Element in Energymeter.'+name,544);
end;
*/
/*  Feeder object code commented out
procedure TEnergyMeterObj.RemoveFeederObj;
begin

    If Assigned(FeederObj) Then Begin
       FeederObj.Enabled := FALSE;
       FeederObj.SetCktElementFeederFlags (FALSE);
    End;

end;
*/
// HasFeeder has to be true before feederObj will be re-enabled.

void TEnergyMeterObj::EnableFeeder()
{

/*  Feeder object code commented out  HasFeeder can never be true
    If HasFeeder Then Begin
        If Not Assigned(FeederObj) Then MakeFeederObj
        Else FeederObj.Enabled := TRUE;
        FeederObj.SetCktElementFeederFlags (TRUE);
    End;
*/
}
/*Similar to Append, by creates the files.*/

void TEnergyMeter::OpenAllDIFiles(int ActorID)
{
	TEnergyMeterObj* mtr = nullptr;
  // Filenm:String;
	if(FSaveDemandInterval)
	{
		ClearDI_Totals();  // clears accumulator arrays
		mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
		while(mtr != nullptr)
		{
			if(mtr->Get_Enabled())
				mtr->OpenDemandIntervalFile(ActorID);
			mtr = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_Next();
		}
		SystemMeter->OpenDemandIntervalFile(ActorID);

          /*Optional Exception Reporting*/
		if(Do_OverloadReport)
			OpenOverloadReportFile(ActorID);
		if(Do_VoltageExceptionReport)
			OpenVoltageReportFile(ActorID);

          /*Open FDI_Totals*/
		try
		{
			CreateFDI_Totals(ActorID);
		}
		catch (std::exception &e)
		{
			DoSimpleMsg(String("Error creating the memory space for demand interval \"") + get_myClass_name()
	           + ".CSV"
	           + " for appending."
	           + CRLF
	           + (std::string) e.what(), 538);
		}
		DIFilesAreOpen[ActorID] = true;
	}/*IF*/
}

void TEnergyMeter::OpenOverloadReportFile(int ActorID)
{
	try
	{
		if(OverLoadFileIsOpen)
			delete OV_MHandle[ActorID];
		OverLoadFileIsOpen = true;
		if(OV_MHandle[ActorID] != nullptr)
			delete OV_MHandle[ActorID];
		OV_MHandle[ActorID] = Create_Meter_Space("\"Hour\", \"Element\", \"Normal Amps\", \"Emerg Amps\", \"% Normal\", \"% Emerg\", \"kVBase\", \"I1(A)\", \"I2(A)\", \"I3(A)\"" +
	           CHAR10);
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error creating memory space (Overload report) for writing.") + CRLF
	           + (std::string) e.what(), 541);
	}
}

void TEnergyMeter::OpenVoltageReportFile(int ActorID)
{
	try
	{
		if(VoltageFileIsOpen)
			delete VR_MHandle[ActorID];
		VoltageFileIsOpen = true;
		if(VR_MHandle[ActorID] != nullptr)
			delete VR_MHandle[ActorID];
		VR_MHandle[ActorID] = Create_Meter_Space("\"Hour\", \"Undervoltages\", \"Min Voltage\", \"Overvoltage\", \"Max Voltage\", \"Min Bus\", \"Max Bus\"");
		WriteintoMemStr(VR_MHandle[ActorID], ", \"LV Undervoltages\", \"Min LV Voltage\", \"LV Overvoltage\", \"Max LV Voltage\", \"Min LV Bus\", \"Max LV Bus\"" +
	           CHAR10);
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error creating memory space (Voltage report) for writing.") + CRLF
	           + (std::string) e.what(), 541);
	}
}

  /*RegisterNameList := TCommandList.Create(['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Zone kWh',
  'Zone kvarh', 'Zone Max kW','Zone Max kVA','Overload kWh Normal','Overload kWh Emerg','Load EEN',
  'Load UE', 'Zone Losses kWh', 'Zone Losses kvarh', 'Zone Max kW Losses', 'Zone Max kvar Losses',
  'Gen kWh', 'Gen kvarh', 'Gen Max kW', 'Gen Max kVA']); */




}  // namespace EnergyMeter





