
#pragma hdrstop

#include "PDElement.h"
#include "DSSGlobals.h"

using namespace std;
using namespace Arraydef;
using namespace Bus;
using namespace CktElement;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace MeterElement;
using namespace System;
using namespace Ucomplex;

namespace PDELement
{

TPDElement::TPDElement(String ClassName) : inherited(ClassName) {}
TPDElement::TPDElement() {}



/*---------Summing Utility proc-------*/

/*#inline*/
void accumsum(double& A, double B)
{
	A = A + B;
}
/*------------------------------------*/

void TPDElement::AccumFltRate()
{
	TDSSBus* FromBus = nullptr;
	TDSSBus* ToBus = nullptr;
	/*# with ActiveCircuit[ActiveActor] do */
	{
		auto with0 = ActiveCircuit[ActiveActor];
		if(FromTerminal == 2)
			ToTerminal = 1;
		else
			ToTerminal = 2;

        /*Get fault Rate for TO bus and add it to this section failure rate*/
		ToBus = with0->Buses[Terminals[ToTerminal - 1].BusRef - 1];
		AccumulatedBrFltRate = ToBus->BusFltRate + BranchFltRate;
		FromBus = with0->Buses[Terminals[FromTerminal - 1].BusRef - 1];
		FromBus->BusTotalNumCustomers = FromBus->BusTotalNumCustomers + BranchTotalCustomers;
		AccumulatedMilesDownStream = ToBus->BusTotalMiles + MilesThisLine;
		accumsum(FromBus->BusTotalMiles, AccumulatedMilesDownStream);

        /*Compute accumulated to FROM Bus; if a fault interrupter, assume it isolates all downline faults*/
		if(!HasOCPDevice)
            // accumlate it to FROM bus
		{
			accumsum(FromBus->BusFltRate, AccumulatedBrFltRate);
		}
	}
}   /*Virtual function  -- LINE is different, for one*/

void TPDElement::CalcFltRate()
{

      /*Default base algorithm for radial fault rate calculation*/
      /*May be overridden by specific device class behavior*/
	BranchFltRate = FaultRate * PctPerm * 0.01;
}

void TPDElement::CalcCustInterrupts()
{
	TDSSBus* FromBus = nullptr;
	FromBus = ActiveCircuit[ActiveActor]->Buses[Terminals[FromTerminal - 1].BusRef - 1];
	/*# with FromBus do */
	{
		auto& with0 = FromBus;
		accumsum(with0->BusCustInterrupts, with0->Bus_Num_Interrupt * BranchTotalCustomers);
	}
}
/*This is called on the forward sweep to set the number of interruptions at the To bus.*/

void TPDElement::CalcNum_Int(int& SectionCount, bool AssumeRestoration)
{
	TDSSBus* FromBus = nullptr;
	TDSSBus* ToBus = nullptr;
	/*# with ActiveCircuit[ActiveActor] do */
	{
		auto with0 = ActiveCircuit[ActiveActor];
		if(FromTerminal == 2)
			ToTerminal = 1;
		else
			ToTerminal = 2;
		ToBus = with0->Buses[Terminals[ToTerminal - 1].BusRef - 1];
		FromBus = with0->Buses[Terminals[FromTerminal - 1].BusRef - 1];

        // If no interrupting device then the downline bus will have the same num of interruptions
		ToBus->Bus_Num_Interrupt = FromBus->Bus_Num_Interrupt;

        /* If Interrupting device (on FROM side)then downline bus will have
          additional interruptions  ---- including for fused lateral
         If assuming restoration and the device is an automatic device, the To bus will be
         interrupted only for  faults on the main section, not including fused sections.
        */
		if(HasOCPDevice)
		{
			if (AssumeRestoration && HasAutoOCPDevice)
			{
				/*To Bus will be interrupted only for faults on this section.
				 AccumulatedBrFltRate does not include Branches down from
				 Branches with OCP devics*/
				ToBus->Bus_Num_Interrupt = AccumulatedBrFltRate;
			}
			else
				accumsum(ToBus->Bus_Num_Interrupt, AccumulatedBrFltRate);

            /*If there is an OCP device on this PDElement, this is the
             beginning of a new section.*/
			SectionCount++;
			ToBus->BusSectionID = SectionCount; // Assign it to the new section
		}
		else
		{
			ToBus->BusSectionID = FromBus->BusSectionID;   // else it's in the same section
		}
		BranchSectionID = ToBus->BusSectionID;
	}
}

TPDElement::TPDElement(TDSSClass* ParClass)
 : inherited(ParClass),
			NormAmps(0.0),
			EmergAmps(0.0),
			FaultRate(0.0),
			PctPerm(0.0),
			BranchFltRate(0.0),
			AccumulatedBrFltRate(0.0),
			MilesThisLine(0.0),
			AccumulatedMilesDownStream(0.0),
			HrsToRepair(0.0),
			FromTerminal(0),
			ToTerminal(0),
			IsShunt(false),
			BranchNumCustomers(0),
			BranchTotalCustomers(0),
			BranchCustWeight(0.0),
			BranchSectionID(0),
			ParentPDElement(nullptr),
			MeterObj(nullptr),
			SensorObj(nullptr),
			Overload_UE(0.0),
			OverLoad_EEN(0.0),
			NumAmpRatings(0)
{
	IsShunt = false;
	FromTerminal = 1;
	BranchNumCustomers = 0;
	BranchTotalCustomers = 0;
	AccumulatedBrFltRate = 0.0;
	MilesThisLine = 0.0;
	SensorObj = nullptr;
	MeterObj = nullptr;
	ParentPDElement = nullptr;
	DSSObjType = PD_ELEMENT;
	NumAmpRatings = 1;
	AmpRatings.resize( 1 );  // Initialized here
	AmpRatings[0] = 1000;
}

TPDElement::~TPDElement()
{
	// inherited::Destroy();
}


void TPDElement::CalcNumCustomers(int ActorID, TPDElement* pElem)
{

	int k = 0,
		kk = 0,
		j = 0;

	kk = ActiveCircuit[ActorID]->Loads.get_myNumList(); // List of ActiveLoads in Circuit
	pElem->BranchNumCustomers = 0;
	pElem->BranchTotalCustomers = 0;
	
	string name = pElem->get_Name();
	int i = 0;

	std::vector<std::string> BpElem = pElem->FBusNames; // BusNames
	int* TotalCust = 0; 

	for (i = 1; i <= kk; i++)
	{
		TPCElement* Loads = (TPCElement*)ActiveCircuit[ActiveActor]->Loads.Get(i);
		std::vector<std::string> LoadsV = Loads->FBusNames;

		for (j = 0; j < BpElem.size(); j++)
		{
			if (BpElem[j] == LoadsV[0]) //Count the NumCustomers in the Bus
			{
				if (name.find("line") != std::string::npos)
				{
					k++;
					pElem->BranchNumCustomers = k;
				}
				pElem->BranchTotalCustomers++;
			}
		}
			
			
	
	}
}


void TPDElement::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int j = 0;
	try
	{
		if(Get_Enabled())
		{
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with0 = ActiveCircuit[ActorID]->Solution;
				int stop = 0;
				for(stop = Yorder, i = 1; i <= stop; i++)
				{
					if(!ADiakoptics || (ActorID == 1))
						Vterminal[i - 1] = with0->NodeV[NodeRef[i - 1]];
					else
    // In the contenxt of actor 1 voltages
						Vterminal[i - 1] = with0->VoltInActor1(NodeRef[i - 1]);
				}
			}
			YPrim->MVmult(Curr, &(Vterminal[0]));
		}
		else
		{
			int stop = 0;
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				Curr[i - 1] = CZero;
			}
		}
	}
	catch (std::exception &e)
	{
		DoErrorMsg((String("Trying to Get Currents for Element: ") + get_Name() + "."), (std::string) e.what(), "Has the circuit been solved?", 660);
	}
}

//- - - - - - - - - - - - - - - - - - - - - -

complex TPDElement::Get_ExcessKVANorm(int idxTerm, int ActorID)
{
	complex result	= CZero;
	double	Factor	= 0.0;
	complex kVA		= CZero;

	if((NormAmps == 0.0) || !Get_Enabled())
	{
		OverLoad_EEN = 0.0;  // bug fixed 1/10/00
		result		 = CZero;
		return result;
	}
	kVA		= cmulreal(Get_Power(idxTerm, ActorID), 0.001);  // Also forces computation of Current into Itemp
	Factor = (MaxTerminalOneIMag(ActorID) / NormAmps - 1.0);
	if(Factor > 0.0)
	{
		OverLoad_EEN	= Factor;
		Factor			= 1.0 - 1.0 / (Factor + 1.0);   // To get factor
		result			= cmulreal(kVA, Factor);
	}
	else
	{
		OverLoad_EEN	= 0.0;
		result			= CZero;
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - -

complex TPDElement::Get_ExcessKVAEmerg(int idxTerm, int ActorID)
{
	complex result = {};
	double Factor = 0.0;
	complex kVA = {};
	if((EmergAmps == 0.0) || !Get_Enabled())
	{
		Overload_UE = 0.0;  // bug fixed 1/10/00
		result = CZero;
		return result;
	}
	kVA = cmulreal(Get_Power(idxTerm, ActorID), 0.001);  // Also forces computation of Current into Itemp
	Factor = (MaxTerminalOneIMag(ActorID) / EmergAmps - 1.0);
	if(Factor > 0.0)
	{
		Overload_UE = Factor;
		Factor = 1.0 - 1.0 / (Factor + 1.0);  // To get Excess
		result = cmulreal(kVA, Factor);
	}
	else
	{
		Overload_UE = 0.0;
		result = CZero;
	}
	return result;
}

void TPDElement::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(ArrayOffset + 1,"400");  //Normamps
	Set_PropertyValue(ArrayOffset + 2,"600");  //emergamps
	Set_PropertyValue(ArrayOffset + 3,"0.1");  //Fault rate
	Set_PropertyValue(ArrayOffset + 4,"20");   // Pct Perm
	Set_PropertyValue(ArrayOffset + 5,"3");    // Hrs to repair
	inherited::InitPropertyValues(ArrayOffset + 5);
}

void TPDElement::ZeroReliabilityAccums()
{
	TDSSBus* FromBus = nullptr;
	FromBus = ActiveCircuit[ActiveActor]->Buses[(Terminals)[FromTerminal - 1].BusRef - 1];
	/*# with FromBus do */
	{
		auto with0 = FromBus;
		with0->BusCustInterrupts = 0.0;
		with0->BusFltRate = 0.0;
		with0->BusTotalNumCustomers = 0;
		with0->BusTotalMiles = 0.0;
		with0->BusCustDurations = 0.0;
		with0->Bus_Num_Interrupt = 0.0;
		with0->BusSectionID = -1; // signify not set
	}
}




}  // namespace PDELement




