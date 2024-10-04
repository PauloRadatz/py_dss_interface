
#pragma hdrstop

#include "ExpControl.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Ucmatrix.h"
#include "mathutil.h"
#include <math.h>
#include "Utilities.h"

using namespace std;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace ControlClass;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace PVSystem;
using namespace ParserDel;
using namespace PointerList;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace ExpControl
{

TExpControlObj::TExpControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TExpControlObj::TExpControlObj(String ClassName) : inherited(ClassName) {}
TExpControlObj::TExpControlObj() {}


TExpControlObj* ActiveExpControlObj = nullptr;
const int NumPropsThisClass = 14;
const int None = 0;
const int CHANGEVARLEVEL = 1;

/*--------------------------------------------------------------------------*/  // Creates superstructure for all ExpControl objects

TExpControl::TExpControl()
{
	;
	Class_Name = "ExpControl";
	DSSClassType = DSSClassType + EXP_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

/*--------------------------------------------------------------------------*/

TExpControl::~TExpControl()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TExpControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "PVSystemList";
	PropertyName[2 - 1] = "Vreg";
	PropertyName[3 - 1] = "Slope";
	PropertyName[4 - 1] = "VregTau";
	PropertyName[5 - 1] = "Qbias";
	PropertyName[6 - 1] = "VregMin";
	PropertyName[7 - 1] = "VregMax";
	PropertyName[8 - 1] = "QmaxLead";
	PropertyName[9 - 1] = "QmaxLag";
	PropertyName[10 - 1] = "EventLog";
	PropertyName[11 - 1] = "DeltaQ_factor";
	PropertyName[12 - 1] = "PreferQ";
	PropertyName[13 - 1] = "Tresponse";
	PropertyName[14 - 1] = "DERList";
	PropertyHelp[1 - 1] = String("Array list of PVSystems to be controlled.") + CRLF
	           + CRLF
	           + "If not specified, all PVSystems in the circuit are assumed to be controlled by this ExpControl.";
	PropertyHelp[2 - 1] = String("Per-unit voltage at which reactive power is zero; defaults to 1.0.") + CRLF
	           + CRLF
	           + "This may dynamically self-adjust when VregTau > 0, limited by VregMin and VregMax."
	           + "If imput as 0, Vreg will be initialized from a snapshot solution with no inverter Q."
	           + "The equilibrium point of reactive power is also affected by Qbias";
	PropertyHelp[3 - 1] = String("Per-unit reactive power injection / per-unit voltage deviation from Vreg; defaults to 50.") + CRLF
	           + CRLF
	           + "Unlike InvControl, base reactive power is constant at the inverter kva rating.";
	PropertyHelp[4 - 1] = String("Time constant for adaptive Vreg. Defaults to 1200 seconds.") + CRLF
	           + CRLF
	           + "When the control injects or absorbs reactive power due to a voltage deviation from the Q=0 crossing of the volt-var curve, "
	           + "the Q=0 crossing will move toward the actual terminal voltage with this time constant. "
	           + "Over time, the effect is to gradually bring inverter reactive power to zero as the grid voltage changes due to non-solar effects. "
	           + "If zero, then Vreg stays fixed. "
	           + "IEEE1547-2018 requires adjustability from 300s to 5000s";
	PropertyHelp[5 - 1] = String("Equilibrium per-unit reactive power when V=Vreg; defaults to 0.") + CRLF
	           + CRLF
	           + "Enter > 0 for lagging (capacitive) bias, < 0 for leading (inductive) bias.";
	PropertyHelp[6 - 1] = "Lower limit on adaptive Vreg; defaults to 0.95 per-unit";
	PropertyHelp[7 - 1] = "Upper limit on adaptive Vreg; defaults to 1.05 per-unit";
	PropertyHelp[8 - 1] = String("Limit on leading (inductive) reactive power injection, in per-unit of base kva; defaults to 0.44." "For Category A inverters per P1547/D7, set this value to 0.25.") + CRLF
	           + CRLF
	           + "Regardless of QmaxLead, the reactive power injection is still "
	           + "limited by dynamic headroom when actual real power output exceeds 0%";
	PropertyHelp[9 - 1] = String("Limit on lagging (capacitive) reactive power injection, in per-unit of base kva; defaults to 0.44.") + CRLF
	           + CRLF
	           + "For Category A inverters per P1547/D7, set this value to 0.25."
	           + "Regardless of QmaxLag, the reactive power injection is still "
	           + "limited by dynamic headroom when actual real power output exceeds 0%";
	PropertyHelp[10 - 1] = "{Yes/True* | No/False} Default is No for ExpControl. Log control actions to Eventlog.";
	PropertyHelp[11 - 1] = String("Convergence parameter; Defaults to 0.7. ") + CRLF
	           + CRLF
	           + "Sets the maximum change (in per unit) from the prior var output level to the desired var output level during each control iteration. "
	           + "If numerical instability is noticed in solutions such as var sign changing from one control iteration to the next and voltages oscillating between two values with some separation, "
	           + "this is an indication of numerical instability (use the EventLog to diagnose). "
	           + "If the maximum control iterations are exceeded, and no numerical instability is seen in the EventLog of via monitors, then try increasing the value of this parameter to reduce the number "
	           + "of control iterations needed to achieve the control criteria, and move to the power flow solution.";
	PropertyHelp[12 - 1] = String("{Yes/True* | No/False} Default is No for ExpControl.") + CRLF
	           + CRLF
	           + "Curtails real power output as needed to meet the reactive power requirement. "
	           + "IEEE1547-2018 requires Yes, but the default is No for backward compatibility of OpenDSS models.";
	PropertyHelp[13 - 1] = String("Open-loop response time for changes in Q.") + CRLF
	           + CRLF
	           + "The value of Q reaches 90% of the target change within Tresponse, which "
	           + "corresponds to a low-pass filter having tau = Tresponse / 2.3026. "
	           + "The behavior is similar to LPFTAU in InvControl, but here the response time is "
	           + "input instead of the time constant. "
	           + "IEEE1547-2018 default is 10s for Catagory A and 5s for Category B, "
	           + "adjustable from 1s to 90s for both categories. However, the default is 0 for "
	           + "backward compatibility of OpenDSS models.";
    PropertyHelp[14 - 1] = "Alternative to PVSystemList for CIM export and import." + CRLF + CRLF 
			   + "However, storage is not actually implemented yet. " 
			   + "Use fully qualified PVSystem names.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TExpControl::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new ExpControl and add it to ExpControl class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TExpControlObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TExpControl::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	ActiveExpControlObj = (TExpControlObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveExpControlObj);
	result = 0;
	/*# with ActiveExpControlObj do */
	{
		auto with0 = ActiveExpControlObj;
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
				with0->Set_PropertyValue(ParamPointer, Param);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 364);
				break;
				case 	1:
				{
					InterpretTStringListArray(Param, *(with0->FPVSystemNameList));
					// Reset FDERNameList
					with0->FDERNameList->clear();
					for(int stop = with0->FPVSystemNameList->size(), i = 0; i < stop; i++)
					{
						with0->FDERNameList->push_back("PVSystem." + (*with0->FPVSystemNameList)[i]);
					}
					with0->FPVSystemPointerList->Clear(); // clear this for resetting on first sample
					with0->FListSize = with0->FPVSystemNameList->size();
				}
				break;
				case 	2:
				if(Parser[ActorID]->MakeDouble_() >= 0)
					with0->FVregInit = Parser[ActorID]->MakeDouble_();
				break;
				case 	3:
				if(Parser[ActorID]->MakeDouble_() > 0)
					with0->FSlope = Parser[ActorID]->MakeDouble_();
				break;
				case 	4:
				if(Parser[ActorID]->MakeDouble_() >= 0)
					with0->FVregTau = Parser[ActorID]->MakeDouble_();
				break; // zero means fixed Vreg
				case 	5:
				with0->FQbias = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				if(Parser[ActorID]->MakeDouble_() > 0)
					with0->FVregMin = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				if(Parser[ActorID]->MakeDouble_() > 0)
					with0->FVregMax = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
				if(Parser[ActorID]->MakeDouble_() >= 0)
					with0->FQmaxLead = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
				if(Parser[ActorID]->MakeDouble_() >= 0)
					with0->FQmaxLag = Parser[ActorID]->MakeDouble_();
				break;
				case 	10:
				with0->ShowEventLog = InterpretYesNo(Param);
				break;
				case 	11:
				with0->FdeltaQ_factor = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->FPreferQ = InterpretYesNo(Param);
				break;
				case 	13:
				if(Parser[ActorID]->MakeDouble_() >= 0)
					with0->FTresponse = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				InterpretTStringListArray(Param, *(with0->FDERNameList));
				// Reset FPVSystemNameList
				with0->FPVSystemNameList->clear();
				for(int stop = (*with0->FDERNameList).size(), i = 0; i < stop; i++)
				{
					with0->FPVSystemNameList->push_back((*with0->FDERNameList)[i]);
				}
				with0->FPVSystemPointerList->Clear();
				with0->FListSize = with0->FPVSystemNameList->size(); // To match the new size
				break;
        // Inherited parameters
				default:
				ClassEdit(ActiveExpControlObj, ParamPointer - NumPropsThisClass);
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		with0->RecalcElementData(ActorID);
	}
	return result;
}

int TExpControl::MakeLike(const String ExpControlName)
{
	int result = 0;
	TExpControlObj* OtherExpControl = nullptr;
	int i = 0;
	int j = 0;
	result = 0;
   /*See if we can find this ExpControl name in the present collection*/
	OtherExpControl = ((TExpControlObj*) Find(ExpControlName));
	if(OtherExpControl != nullptr)
		/*# with ActiveExpControlObj do */
		{
			auto with0 = ActiveExpControlObj;
			int stop = 0;
			with0->Set_NPhases(OtherExpControl->Fnphases);
			with0->Set_Nconds(OtherExpControl->Fnconds); // Force Reallocation of terminal stuff
			for(stop = with0->FPVSystemPointerList->get_myNumList(), i = 1; i <= stop; i++)
			{
				with0->ControlledElement[i] = OtherExpControl->ControlledElement[i];
				with0->FWithinTol[i] = OtherExpControl->FWithinTol[i];
			}
			with0->FListSize = OtherExpControl->FListSize;
			with0->FVoltageChangeTolerance = OtherExpControl->FVoltageChangeTolerance;
			with0->FVarChangeTolerance = OtherExpControl->FVarChangeTolerance;
			with0->FVregInit = OtherExpControl->FVregInit;
			with0->FSlope = OtherExpControl->FSlope;
			with0->FVregTau = OtherExpControl->FVregTau;
			with0->FQbias = OtherExpControl->FQbias;
			with0->FVregMin = OtherExpControl->FVregMin;
			with0->FVregMax = OtherExpControl->FVregMax;
			with0->FQmaxLead = OtherExpControl->FQmaxLead;
			with0->FQmaxLag = OtherExpControl->FQmaxLag;
			with0->FdeltaQ_factor = OtherExpControl->FdeltaQ_factor;
			with0->FPreferQ = OtherExpControl->FPreferQ;
			with0->FTresponse = OtherExpControl->FTresponse;
			with0->FOpenTau = with0->FTresponse / 2.3026;  // not sure if RecalcElementData will be invoked from the call stack
			for(stop = with0->ParentClass->NumProperties, j = 1; j <= stop; j++)
			{
				with0->Set_PropertyValue(j, OtherExpControl->Get_PropertyValue(j));
			}
		}
	else
		DoSimpleMsg(String("Error in ExpControl MakeLike: \"") + ExpControlName
	           + "\" Not Found.", 370);
	return result;
}

/*==========================================================================*/
/*                    TExpControlObj                                        */
/*==========================================================================*/

TExpControlObj::TExpControlObj(TDSSClass* ParClass, const String ExpControlName)
 : inherited(ParClass),
			ControlActionHandle(0),
			FListSize(0),
			FVregInit(0.0),
			FSlope(0.0),
			FVregTau(0.0),
			FQbias(0.0),
			FVregMin(0.0),
			FVregMax(0.0),
			FQmaxLead(0.0),
			FQmaxLag(0.0),
			FdeltaQ_factor(0.0),
			FVoltageChangeTolerance(0.0),
			FVarChangeTolerance(0.0),
			FPreferQ(false),
			FTresponse(0.0),
			FOpenTau(0.0)
{
	Set_Name(LowerCase(ExpControlName));
	DSSObjType = ParClass->DSSClassType;
	ElementName = "";
	//FPVSystemNameList->clear();
	//FPVSystemPointerList = NULL,

     /*
       Control elements are zero current sources that attach to a terminal of a
       power-carrying device, but do not alter voltage or current flow.
       Define a default number of phases and conductors here and update in
       RecalcElementData routine if necessary. This allocates arrays for voltages
       and currents and gives more direct access to the values, if needed
     */
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
     // This general feature should not be used for ExpControl,
     // because it controls more than one PVSystem
	ShowEventLog = false;
	ControlledElement.clear();
	//FPVSystemNameList->clear();
	FPVSystemPointerList = NULL;
	cBuffer.clear();
	FPriorVpu = NULL;
	FPresentVpu = NULL;
	FPendingChange = NULL;
	FLastIterQ = NULL;
	FLastStepQ = NULL;
	FTargetQ = NULL;
	FWithinTol = NULL;
	FVoltageChangeTolerance = 0.0001;  // per-unit
	FVarChangeTolerance = 0.0001;  // per-unit
	FDERNameList = new TStringList();
	FPVSystemNameList = new TStringList();
	FPVSystemPointerList = new PointerList::TPointerList(20);  // Default size and increment

	ElementTerminal = 1;

     // user parameters for dynamic Vreg
	FVregInit = 1.0; // 0 means to find it during initialization
	FSlope = 50.0;
	FVregTau = 1200.0;
	FVregs = NULL;
	FQbias = 0.0;
	FVregMin = 0.95;
	FVregMax = 1.05;
	FQmaxLead = 0.44;
	FQmaxLag = 0.44;
	FdeltaQ_factor = 0.7; // only on control iterations, not the final solution
	FPreferQ = false;
	FTresponse = 0.0;
	FOpenTau = 0.0;

     //generic for control
	FPendingChange = NULL;
	InitPropertyValues(0);
}

TExpControlObj::~TExpControlObj()
{
	ElementName = "";
	ControlledElement.clear();
	cBuffer.clear();
    FDERNameList->clear();
    FPVSystemNameList->clear();
    FPVSystemPointerList = NULL;
	free(FPriorVpu);
	free(FPresentVpu);
	free(FPendingChange);
	free(FLastIterQ);
	free(FLastStepQ);
	free(FTargetQ);
	free(FWithinTol);
	free(FVregs);
	// inherited::Destroy();
}


void TExpControlObj::RecalcElementData(int ActorID)
{
	int i = 0;
	int maxord = 0;
	int stop = 0;
	FOpenTau = FTresponse / 2.3026;
	if(FPVSystemPointerList->get_myNumList() == 0)
		MakePVSystemList();
	if(FPVSystemPointerList->get_myNumList() > 0)
    /*Setting the terminal of the ExpControl device to same as the 1st PVSystem element*/
	{
		Set_MonitoredElement(((TDSSCktElement*) FPVSystemPointerList->Get(1)));   // Set MonitoredElement to 1st PVSystem in lise
		SetBus(1, get_FMonitoredElement()->Get_FirstBus());
	}
	maxord = 0; // will be the size of cBuffer
	for(stop = FPVSystemPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
        // User ControlledElement[] as the pointer to the PVSystem elements
		ControlledElement[i] = ((TPVsystemObj*) FPVSystemPointerList->Get(i));  // pointer to i-th PVSystem
		Set_NPhases(( (TDSSCktElement*) ControlledElement[i] )->Get_NPhases());  // TEMC TODO - what if these are different sizes (same concern exists with InvControl)
		Set_Nconds(Get_NPhases());
		if(ControlledElement[i] == nullptr)
			DoErrorMsg(String("ExpControl: \"") + this->get_Name() + "\"", String("Controlled Element \"") + ( *FPVSystemNameList )[i - 1]
	           + "\" Not Found.", " PVSystem object must be defined previously.", 361);
		if(((TDSSCktElement*)ControlledElement[i])->Yorder > maxord)
			maxord = ((TDSSCktElement*)ControlledElement[i])->Yorder;
		((TDSSCktElement*)ControlledElement[i])->Set_ActiveTerminal(1); // Make the 1 st terminal active
	}
	if(maxord > 0)
		cBuffer.resize( maxord );
}
// ***  This assumes the PVSystem devices have already been converted to pos seq

void TExpControlObj::MakePosSequence(int ActorID)
{
	if(FPVSystemPointerList->get_myNumList() == 0)
		RecalcElementData(ActorID);
  // TEMC - from here to inherited was copied from InvControl
	Set_NPhases(3);
	Set_Nconds(3);
	SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
	if(FPVSystemPointerList->get_myNumList() > 0)
    /*Setting the terminal of the ExpControl device to same as the 1st PVSystem element*/
    /* This sets it to a realistic value to avoid crashes later */
	{
		Set_MonitoredElement(((TDSSCktElement*) FPVSystemPointerList->Get(1)));   // Set MonitoredElement to 1st PVSystem in lise
		SetBus(1, get_FMonitoredElement()->Get_FirstBus());
		Set_NPhases(get_FMonitoredElement()->Get_NPhases());
		Set_Nconds(Get_NPhases());
	}
	inherited::MakePosSequence(ActorID);
}

void TExpControlObj::CalcYPrim(int ActorID)
{
}

void TExpControlObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
// Control is a zero current source
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TExpControlObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
// Control is a zero current source
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TExpControlObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
	{
		WriteLn(f);
	}
}

void TExpControlObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	int i = 0;
	double Qset = 0.0;
	double DeltaQ = 0.0;
	double Qmaxpu = 0.0;
	double Qpu = 0.0;
	double Qbase = 0.0;
	double Qinvmaxpu = 0.0;
	double Plimit = 0.0;
	double DT = 0.0;
	TPVsystemObj* PVSys = nullptr;
	int stop = 0;
	for(stop = FPVSystemPointerList->get_myNumList(), i = 1; i <= stop; i++)
	{
		PVSys = ControlledElement[i];   // Use local variable in loop
		if(Get_PendingChange(i) == CHANGEVARLEVEL)
		{
			PVSys->Set_VWmode(false);
			( (TDSSCktElement*) PVSys )->Set_ActiveTerminal(1); // Set active terminal of PVSystem to terminal 1
			PVSys->Set_Varmode(VARMODEKVAR);  // Set var mode to VARMODEKVAR to indicate we might change kvar
			FTargetQ[i] = 0.0;
			Qbase = PVSys->Get_FkVArating();
			Qinvmaxpu = PVSys->Get_Fkvarlimit() / Qbase;
			Qpu = PVSys->Get_Presentkvar() / Qbase; // no change for now
			if(FWithinTol[i] == false)
        // look up Qpu from the slope crossing at Vreg, and add the bias
			{
				Qpu = -FSlope * (FPresentVpu[i] - FVregs[i]) + FQbias;
				if(ShowEventLog)
					AppendToEventLog(String("ExpControl.") + this->get_Name() + "," +
						PVSys->get_Name(), Format(" Setting Qpu= %.5g at FVreg= %.5g, Vpu= %.5g",Qpu, FVregs[i], FPresentVpu[i]), ActorID);
			}

      // apply limits on Qpu, then define the target in kVAR
			PVSys->SetNominalPVSystemOuput(ActorID); // as does InvControl
			if(FPreferQ)
				Qmaxpu = 1.0;
			else
				Qmaxpu = sqrt(1.0L - Sqr(PVSys->Get_PresentkW() / Qbase)); // dynamic headroom
			if(Qmaxpu > Qinvmaxpu)
				Qmaxpu = Qinvmaxpu;
			if(Abs(Qpu) > Qmaxpu)
				Qpu = Qmaxpu * Sign(Qpu);
			if(Qpu <  - FQmaxLead)
				Qpu = -FQmaxLead;
			if(Qpu > FQmaxLag)
				Qpu = FQmaxLag;
			FTargetQ[i] = Qbase * Qpu;
			if(FPreferQ)
			{
				Plimit = Qbase * sqrt(1 - Qpu * Qpu);
				if(Plimit < PVSys->Get_PresentkW())
				{
					if(ShowEventLog)
						AppendToEventLog(String("ExpControl.") + this->get_Name() + "," + ( (TDSSObject*) PVSys )->get_Name(), Format(" curtailing %.3f to %.3f kW",PVSys->Get_PresentkW(), Plimit), ActorID);
					PVSys->Set_PresentkW(Plimit);
					PVSys->Set_puPmpp(Plimit / PVSys->Get_FPmpp());
				}
			}

      // put FTargetQ through the low-pass open-loop filter
			if(FOpenTau > 0.0 && ActiveCircuit[ActorID]->Solution->ControlMode != CTRLSTATIC)
			{
				DT = ActiveCircuit[ActorID]->Solution->DynaVars.h;
				FTargetQ[i] = FLastStepQ[i] + (FTargetQ[i] - FLastStepQ[i]) * (1 - exp(-DT / FOpenTau)); // TODO - precalculate?
			}

      // only move the non-bias component by deltaQ_factor in this control iteration
			DeltaQ = FTargetQ[i] - FLastIterQ[i];
			Qset = FLastIterQ[i] + DeltaQ * FdeltaQ_factor;
 //     Qset := FQbias * Qbase;
			if(PVSys->Get_Presentkvar() != Qset)
				PVSys->Set_Presentkvar(Qset);
			if(ShowEventLog)
				AppendToEventLog(String("ExpControl.") + this->get_Name() + "," + PVSys->get_Name(), Format(" Setting PVSystem output kvar= %.5g",PVSys->Get_Presentkvar()), ActorID);
			FLastIterQ[i] = Qset;
			FPriorVpu[i] = FPresentVpu[i];
			ActiveCircuit[ActorID]->Solution->LoadsNeedUpdating = true;
      // Force recalc of power parms
			Set_PendingChange(None, i);
		}
	}
}

void TExpControlObj::sample(int ActorID)
{
	int i = 0;
	int j = 0;
	double BaseKV = 0.0;
	double Vpresent = 0.0;
	double Verr = 0.0;
	double Qerr = 0.0;
	TPVsystemObj* PVSys = nullptr;
  // If list is not defined, go make one from all PVSystem in circuit
	if(FPVSystemPointerList->get_myNumList() == 0)
		RecalcElementData(ActorID);
	if(FListSize > 0)
    // If an ExpControl controls more than one PV, control each one
    // separately based on the PVSystem's terminal voltages, etc.
	{
		int stop = 0;
		for(stop = FPVSystemPointerList->get_myNumList(), i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			PVSys = ControlledElement[i];   // Use local variable in loop
      // Calculate the present average voltage  magnitude
			PVSys->ComputeVterminal(ActorID);
			for(stop1 = PVSys->Yorder, j = 1; j <= stop1; j++)
			{
				cBuffer[j - 1] = (PVSys)->Vterminal[j - 1];
			}
			BaseKV = ActiveCircuit[ActorID]->Buses[((TDSSCktElement*)PVSys)->Terminals[0].BusRef - 1]->kVBase;

			Vpresent = 0;
			for(stop1 = ((TDSSCktElement*)PVSys)->Get_NPhases(), j = 1; j <= stop1; j++)
			{
				Vpresent = Vpresent + cabs(cBuffer[j - 1]);
			}
			FPresentVpu[i] = (Vpresent / ((TDSSCktElement*)PVSys)->Get_NPhases()) / (BaseKV * 1000.0);
      // if initializing with Vreg=0 in static mode, we want to FIND Vreg
			if((ActiveCircuit[ActorID]->Solution->ControlMode == CTRLSTATIC) && (FVregInit <= 0.0))
			{
				FVregs[i] = FPresentVpu[i];
				
				if (FVregs[i] < FVregMin)
				{
					FVregs[i] = FVregMin;
					FVregInit = 0.01; // Don't let it outside the band
				}
				if (FVregs[i] > FVregMax)
				{
					FVregs[i] = FVregMax;
					FVregInit = 0.01; // Don't let it outside the band
				}
			}
      // both errors are in per-unit
			Verr = Abs(FPresentVpu[i] - FPriorVpu[i]);
			Qerr = double(Abs( (PVSys->Get_Presentkvar() - FTargetQ[i])) / PVSys->Get_FkVArating());
      // process the sample
			if((PVSys->Get_InverterON() == false) && (PVSys->Get_VarFollowInverter() == true)) // not injecting
			{
				if((FVregTau > 0.0) && (FVregs[i] <= 0.0))
					FVregs[i] = FPresentVpu[i]; // wake up to the grid voltage, otherwise track it while not injecting
				continue;
			}
			PVSys->Set_VWmode(false);
			if((Verr > FVoltageChangeTolerance) || (Qerr > FVarChangeTolerance) || (ActiveCircuit[ActorID]->Solution->ControlIteration == 1))
			{
				FWithinTol[i] = false;
				Set_PendingChange(CHANGEVARLEVEL, i);
				/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
				{
					auto& with0 = ActiveCircuit[ActorID]->Solution->DynaVars;
					ControlActionHandle = ActiveCircuit[ActorID]->ControlQueue.Push(with0.intHour, with0.T + TimeDelay, (EControlAction) Get_PendingChange(i), 0, this, ActorID);
				}
				if(ShowEventLog)
					AppendToEventLog(String("ExpControl.") + this->get_Name() + " " + PVSys->get_Name(), Format(" outside Hit Tolerance, Verr= %.5g, Qerr=%.5g",Verr, Qerr), ActorID);
			}
			else
			{
				FWithinTol[i] = true;
				if(ShowEventLog)
					AppendToEventLog(String("ExpControl.") + this->get_Name() + " " + PVSys->get_Name(), Format(" within Hit Tolerance, Verr= %.5g, Qerr=%.5g",Verr, Qerr), ActorID);
			}
		}  /*For*/
	} /*If FlistSize*/
}

void TExpControlObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1, "");      // PVSystem list
	Set_PropertyValue(2, "1");     // initial Vreg
	Set_PropertyValue(3, "50");    // slope
	Set_PropertyValue(4, "1200.0");// VregTau
	Set_PropertyValue(5, "0");     // Q bias
	Set_PropertyValue(6, "0.95");  // Vreg min
	Set_PropertyValue(7, "1.05");  // Vreg max
	Set_PropertyValue(8, "0.44");  // Qmax leading
	Set_PropertyValue(9, "0.44");  // Qmax lagging
	Set_PropertyValue(10, ShowEventLog ? "YES" : "NO");    // write event log?
	Set_PropertyValue(11, "0.7");   // DeltaQ_factor
	Set_PropertyValue(12, "no");    // PreferQ
    Set_PropertyValue(13, "0"); // TResponse
    Set_PropertyValue(14, ""); // Der Name List
	inherited::InitPropertyValues(NumPropsThisClass);
}

bool TExpControlObj::MakePVSystemList()
{
	bool result = false;
	TDSSClass* PVSysClass = nullptr;
	TPVsystemObj* PVSys = nullptr;
	int i = 0;
	int stop = 0;
	result = false;
	PVSysClass = (TDSSClass*)GetDSSClassPtr("PVsystem");
	if(FListSize > 0)    // Name list is defined - Use it
	{
		int stop = 0;
		ControlledElement.resize( FListSize + 1 );  // Use this as the main pointer to PVSystem Elements
		FPriorVpu = new double[FListSize + 1];
		FPresentVpu = new double[FListSize + 1];
		FPendingChange = new int[FListSize + 1];
		FLastIterQ = new double[FListSize + 1];
		FLastStepQ = new double[FListSize + 1];
		FTargetQ = new double[FListSize + 1];
		FWithinTol = new bool[FListSize + 1];
		FVregs = new double[FListSize + 1];
		for(stop = FListSize, i = 1; i <= stop; i++)
		{
			PVSys = ((TPVsystemObj*) PVSysClass->Find(String( (*FPVSystemNameList )[i - 1])));
			if(ASSIGNED(PVSys) && ( (TDSSCktElement*) PVSys )->Get_Enabled())
			{
				FPVSystemPointerList->Set_New(PVSys);
				PVSys->Set_AVRmode(true);
				PVSys->Set_Variable(5, FVregInit);
			}
		}
	}
	else

     /*Search through the entire circuit for enabled pvsysten objects and add them to the list*/
	{
		int stop = 0;
		for(stop = PVSysClass->Get_ElementCount(), i = 1; i <= stop; i++)
		{
			PVSys = (TPVsystemObj*) PVSysClass->ElementList.Get(i);
			if( ( (TDSSCktElement*) PVSys )->Get_Enabled())
				FPVSystemPointerList->Set_New(PVSys);
			FPVSystemNameList->push_back( ( (TDSSObject*) PVSys )->get_Name());
		}
		FListSize = FPVSystemPointerList->get_myNumList();
		ControlledElement.resize( FListSize + 1 );
		FPriorVpu = new double[FListSize + 1];
		FPresentVpu = new double[FListSize + 1];
		FPendingChange = new int[FListSize + 1];
		FLastIterQ = new double[FListSize + 1];
		FLastStepQ = new double[FListSize + 1];
		FTargetQ = new double[FListSize + 1];
		FWithinTol = new bool[FListSize + 1];
		FVregs = new double[FListSize + 1];
	}  /*Else*/

  //Initialize arrays
	for(stop = FListSize, i = 1; i <= stop; i++)
	{
//    PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);
//    Set_NTerms(PVSys.NTerms); // TODO - what is this for?
		FPriorVpu[i] = 0.0;
		FPresentVpu[i] = 0.0;
		FLastIterQ[i] = -1.0;
		FLastStepQ[i] = -1.0;
		FTargetQ[i] = 0.0;
		FWithinTol[i] = false;
		FVregs[i] = FVregInit;
		FPendingChange[i] = None;
	} /*For*/
	RecalcElementData(ActiveActor);
	if(FPVSystemPointerList->get_myNumList() > 0)
		result = true;
	return result;
}

void TExpControlObj::Reset(int ActorID)
{

  // inherited;
}

String TExpControlObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	1:
		result = ReturnElementsList();
		break;
		case 	2:
		result = Format("%.6g",FVregInit);
		break;
		case 	3:
		result = Format("%.6g",FSlope);
		break;
		case 	4:
		result = Format("%.6g",FVregTau);
		break;
		case 	5:
		result = Format("%.6g",FQbias);
		break;
		case 	6:
		result = Format("%.6g",FVregMin);
		break;
		case 	7:
		result = Format("%.6g",FVregMax);
		break;
		case 	8:
		result = Format("%.6g",FQmaxLead);
		break;
		case 	9:
		result = Format("%.6g",FQmaxLag);
		break;
		case 	11:
		result = Format("%.6g",FdeltaQ_factor);
		break;
		case 	12:
		if(FPreferQ)
			result = "yes";
		else
			result = "no";
		break;
		case 	13:
		result = Format("%.6g",FTresponse);
		break;
    // 10 skipped, EventLog always went to the default handler
  // take the generic handler
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

String TExpControlObj::ReturnElementsList()
{
	String result;
	int i = 0;
	int stop = 0;
	if(FListSize == 0)
	{
		result = "";
		return result;
	}
	result = String("[") + ( *FPVSystemNameList )[0];
	for(stop = FListSize - 1, i = 1; i <= stop; i++)
	{
		result = result + ", " + String(( *FPVSystemNameList )[i]);
	}
	result = result + "]";  // terminate the array
	return result;
}

void TExpControlObj::Set_Enabled(bool Value)
{
	inherited::Set_Enabled(Value);
  /*Reset controlled PVSystems to original PF*/
}

void TExpControlObj::Set_PendingChange(int Value, int DevIndex)
{
	FPendingChange[DevIndex] = Value;
	DblTraceParameter = (double) Value;
}

void TExpControlObj::UpdateExpControl(int i, int ActorID)
{
	int j = 0;
	TPVsystemObj* PVSys = nullptr;
	double DT = 0.0;
	double Verr = 0.0; // for DYNAMICVREG
	int stop = 0;
	for(stop = FPVSystemPointerList->get_myNumList(), j = 1; j <= stop; j++)
	{
		PVSys = ControlledElement[j];
		FLastStepQ[j] = PVSys->Get_Presentkvar();
		if(FVregTau > 0.0)
		{
			DT = ActiveCircuit[ActorID]->Solution->DynaVars.h;
			Verr = FPresentVpu[j] - FVregs[j];
			FVregs[j] = FVregs[j] + Verr * (1 - exp(-DT / FVregTau)); // TODO - precalculate?
		}
		else
		{
			Verr = 0.0;
		}
		if(FVregs[j] < FVregMin)
			FVregs[j] = FVregMin;
		if(FVregs[j] > FVregMax)
			FVregs[j] = FVregMax;
		PVSys->Set_Variable(5, FVregs[j]);
		if(ShowEventLog)
			AppendToEventLog(String("ExpControl.") + this->get_Name() + "," + ( (TDSSObject*) PVSys )->get_Name(), Format(" Setting new Vreg= %.5g Vpu=%.5g Verr=%.5g",FVregs[j], FPresentVpu[j], Verr), ActorID);
	}
}

int TExpControlObj::Get_PendingChange(int DevIndex)
{
	int result = 0;
	result = FPendingChange[DevIndex];
	return result;
}

//Called at end of main power flow solution loop

void TExpControl::UpdateAll(int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = ElementList.get_myNumList(), i = 1; i <= stop; i++)
	{
		/*# with TExpControlObj(ElementList.Get(i)) do */
		{
			auto with0 = ((TExpControlObj*) ElementList.Get(i));
			if(with0->Get_Enabled())
				with0->UpdateExpControl(i, ActorID);
		}
	}
}




}  // namespace ExpControl





