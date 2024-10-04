#include <vcl.h>
#pragma hdrstop

#include "VVControl.h"
#include "../Parser/ParserDel.h"
#include "../Common/DSSClassDefs.h"
#include "../MyDSSClassDefs.h"
#include "../Common/DSSGlobals.h"
#include "../Common/Circuit.h"
#include "../PCElements/generator.h"
#include <System.SysUtils.hpp>
#include "../Shared/Ucmatrix.h"
#include "../Shared/mathutil.h"
#include <System.Math.hpp>

using namespace std;
using namespace d2c_system;
using namespace Arraydef;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace ControlClass;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Generator;
using namespace MyDSSClassDefs;
using namespace ParserDel;
using namespace System;
using namespace System::Classes;
using namespace System::Math;
using namespace System::Sysutils;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XYCurve;
using namespace mathutil;
using namespace utilities;

namespace VVControl
{

TVVControlObj::TVVControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TVVControlObj::TVVControlObj(String ClassName) : inherited(ClassName) {}
TVVControlObj::TVVControlObj() {}


TVVControlObj* ActiveVVCControlObj = nullptr;
const int NumPropsThisClass = 19;
const int None = 0;
const int CHANGEVARLEVEL = 1;

  /* -------------------------------------------------------------------------- */ // Creates superstructure for all VVCControl objects

TVVControl::TVVControl()
 : XY_CurveClass(GetDSSClassPtr(L"XYCurve"))
{
	;
	Class_Name = "VVControl";
	DSSClassType = DSSClassType + VV_CONTROL;
	DefineProperties();
	CommandList = new TCommandList(SLICE((*PropertyName), NumProperties));
	CommandList->set_AbbrevAllowed(true);
}

/* -------------------------------------------------------------------------- */

TVVControl::~TVVControl()
{
	// inherited::Destroy();
}


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TVVControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties(); // Get inherited property count
	AllocatePropertyArrays();

  // Define Property names
	(*PropertyName)[1 - 1] = L"Element";
	(*PropertyName)[2 - 1] = L"Terminal";
	(*PropertyName)[3 - 1] = L"vvc_Vmaxpu";
	(*PropertyName)[4 - 1] = L"vvc_Vminpu";
	(*PropertyName)[5 - 1] = L"kva_rating";
	(*PropertyName)[6 - 1] = L"kW_rating";
	(*PropertyName)[7 - 1] = L"kvar_full_output";
	(*PropertyName)[8 - 1] = L"pf";
	(*PropertyName)[9 - 1] = L"delay";
	(*PropertyName)[10 - 1] = L"delayoff";
	(*PropertyName)[11 - 1] = L"kW_ramp_rate";
	(*PropertyName)[12 - 1] = L"kvar_ramp_rate";
	(*PropertyName)[13 - 1] = L"kW_limit";
	(*PropertyName)[14 - 1] = L"kvar_limit";
	(*PropertyName)[15 - 1] = L"GenList";
	(*PropertyName)[16 - 1] = L"Weights";
	(*PropertyName)[17 - 1] = L"NumPts";
	(*PropertyName)[18 - 1] = L"VVC_curve";
	(*PropertyName)[19 - 1] = L"deltaQ_factor";
	(*PropertyHelp)[1 - 1] = L"Full object name of the circuit element, typically a line or transformer, "
	           L"which the control is monitoring. There is no default; must be specified.";
	(*PropertyHelp)[2 - 1] = L"Number of the terminal of the circuit element to which the VVCControl control is connected. "
	           L"1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.";
	(*PropertyHelp)[3 - 1] = L"Default = 0.90.  Minimum per unit voltage for which the vvccurve volts property is assumed to apply. "
	           L"Below this value, the var output is zero (i.e., the unit will not operate).";
	(*PropertyHelp)[4 - 1] = L"Default = 1.10.  Maximum per unit voltage for which the vvccurve volts property is assumed to apply. "
	           L"Above this value, the var output is zero (i.e., the unit will not operate).";
	(*PropertyHelp)[5 - 1] = L"Default = 1.2 times the kW_rating of the unit.  Maximum steady-state apparent power output.";
	(*PropertyHelp)[6 - 1] = L"Default = 4.0.  Maximum steady-state active power output of the unit under control.";
	(*PropertyHelp)[7 - 1] = L"Max kvar to be delivered through the element.  Corresponds to the +/- 1.0 per-unit var value in the volt/var curve.";
	(*PropertyHelp)[8 - 1] = L"Displacement power factor set-point of the inverter (modeled as a generator).  PF set-point will not cause delivered kvar to exceed the maximum kvar limit.";
	(*PropertyHelp)[9 - 1] = L"Delay in seconds for switching ON the inverter (modeled as a generator). Default is 0.0 s";
	(*PropertyHelp)[10 - 1] = L"Delay in seconds for switching OFF the inverter (modeled as a generator). Default is 0.0 s";
	(*PropertyHelp)[11 - 1] = L"Ramp rate in kW per second for turning ON and OFF the inverter.  Ramps the kW from 0 or other full to kW_rating over x seconds. Default is -1 denoting immediate switch ON/OFF, after optional delay";
	(*PropertyHelp)[12 - 1] = L"Ramp rate in kvar per second for turning ON and OFF the inverter.  Ramps the kW from 0 or other full to kvar_limit over x seconds. Default is -1 denoting immediate switch ON/OFF, after optional delay";
	(*PropertyHelp)[13 - 1] = L"kW Limit for the monitored element. The generators are dispatched to hold the active power to attempt to achieve this value.";
	(*PropertyHelp)[14 - 1] = L"kvar Limit for the monitored element. The generators are dispatched to hold the reactive power to attempt to achieve this value.";
	(*PropertyHelp)[15 - 1] = L"Array list of generators to be dispatched.  If not specified, all generators in the circuit are assumed dispatchable.";
	(*PropertyHelp)[16 - 1] = L"Array of proportional weights corresponding to each generator in the GenList. The needed kW to get back to center band is dispatched to each generator according to these weights. Default is to set all weights to 1.0.";
	(*PropertyHelp)[17 - 1] = L"Number of points expected to be in the volt curve or the var curve (XYcurve object).";
	(*PropertyHelp)[18 - 1] = L"Name of the volt-var curve that has been previously defined using the XYcurve object.";
	(*PropertyHelp)[19 - 1] = L"The maximum change in per-unit from the prior var output to the var output indicated by the volt-var curve (XYcurve object).";
	ActiveProperty = NumPropsThisClass;
	inherited::DefineProperties(); // Add defs of inherited properties to bottom of list
}

/* -------------------------------------------------------------------------- */

TXYcurveObj* TVVControl::GetVVCCurve(const String CurveName)
{
	TXYcurveObj* result = nullptr;
	result = ((TXYcurveObj*) XY_CurveClass->Find(CurveName));
	if(result == nullptr)
		DoSimpleMsg(String(L"XY Curve object: \"") + CurveName + L"\" not found.", 380);
	return result;
}

/* -------------------------------------------------------------------------- */

/* -------------------------------------------------------------------------- */

int TVVControl::NewObject(const String ObjName)
{
	int result = 0;
  // Make a new VVCControl and add it to VVCControl class list
	/*# with ActiveCircuit do */
	{
		auto& with0 = ActiveCircuit;
		ActiveCktElement = new TVVControlObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/* -------------------------------------------------------------------------- */

int TVVControl::Edit()
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int i = 0;

  // continue parsing WITH contents of Parser
	ActiveVVCControlObj = &ElementList.Active;
	ActiveCircuit.ActiveCktElement = ActiveVVCControlObj;
	result = 0;
	/*# with ActiveVVCControlObj do */
	{
		auto with0 = ActiveVVCControlObj;
		ParamPointer = 0;
		ParamName = Parser.NextParam;
		Param = Parser.StrValue;
		while(Param.Length() > 0)
		{
			if(ParamName.Length() == 0)
				++ParamPointer;
			else
				ParamPointer = CommandList->Getcommand(ParamName);
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))
				with0->PropertyValue[ParamPointer] = Param;
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String(L"Unknown parameter \"") + ParamName
	           + L"\" for Object \""
	           + Class_Name
	           + L"."
	           + with0->Name
	           + L"\"", 364);
				break;
				case 	1:
				with0->ElementName = LowerCase(Param);
				break;
				case 	2:
				with0->ElementTerminal = Parser.IntValue;
				break;
				case 	3:
				with0->Fvvc_Vmaxpu = Parser.DblValue;
				break;
				case 	4:
				with0->Fvvc_Vminpu = Parser.DblValue;
				break;
				case 	5:
				with0->Fkva_rating = Parser.DblValue;
				break;
				case 	6:
				with0->FkW_rating = Parser.DblValue;
				break;
				case 	7:
				with0->Fkvar_fulloutput = Parser.DblValue;
				break;
				case 	8:
				with0->Fpf = Parser.DblValue;
				break;
				case 	9:
				with0->FDelay = Parser.DblValue;
				break;
				case 	10:
				with0->Fdelayoff = Parser.DblValue;
				break;
				case 	11:
				with0->FkW_ramp_rate = Parser.DblValue;
				break;
				case 	12:
				with0->Fkvar_ramp_rate = Parser.DblValue;
				break;
				case 	13:
				with0->FkW_limit = Parser.DblValue;
				break;
				case 	14:
				with0->Fkvar_limit = Parser.DblValue;
				break;
				case 	15:
				InterpretTStringListArray(Param, with0->FGeneratorNameList);
				break;
				case 	16:
				{
					with0->FListSize = with0->FGeneratorNameList->Count;
					if(with0->FListSize > 0)
					{
						with0->FWeights = (pDoubleArray) ReallocMemory(with0->FWeights, sizeof((*with0->FWeights)[1 - 1]) * with0->FListSize);
						InterpretDblArray(Param, with0->FListSize, with0->FWeights);
					}
				}
				break;
				case 	17:
				with0->Fvvc_curve_size = Parser.IntValue;
				break;
				case 	18:
				with0->Fvvc_curve = GetVVCCurve(Param);
				break;
				case 	19:
				with0->FdeltaQ_factor = Parser.DblValue;
				break;
        // Inherited parameters
				default:
				ClassEdit(ActiveVVCControlObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	15: // re-alloc based on
				{
					int stop = 0;
					with0->FGenPointerList.Clear; // clear this for resetting on first sample
					with0->FListSize = with0->FGeneratorNameList->Count;
					with0->FWeights = (pDoubleArray) ReallocMemory(with0->FWeights, sizeof((*with0->FWeights)[1 - 1]) * with0->FListSize);
					for(stop = with0->FListSize, i = 1; i <= stop; i++)
					{
						(*with0->FWeights)[i - 1] = 1.0;
					}
				}
				break; // re-set the number vvc_curve_size property to the number
            // of points in the curve
				case 	18:
				{
					if(with0->Fvvc_curve->NumPoints != with0->Fvvc_curve_size)
						with0->Fvvc_curve_size = with0->Fvvc_curve->NumPoints;
				}
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser.NextParam;
			Param = Parser.StrValue;
		}
		with0->RecalcElementData();
	}
	return result;
}

/* -------------------------------------------------------------------------- */

int TVVControl::MakeLike(const String VVCControlName)
{
	int result = 0;
	TVVControlObj* OtherVVCControl = nullptr;
	int i = 0;
	result = 0;
  /* See if we can find this VVCControl name in the present collection */
	OtherVVCControl = ((TVVControlObj*) Find(VVCControlName));
	if(OtherVVCControl != nullptr)
		/*# with ActiveVVCControlObj do */
		{
			auto with0 = ActiveVVCControlObj;
			int stop = 0;
			with0->NPhases = OtherVVCControl->Fnphases;
			with0->NConds = OtherVVCControl->Fnconds; // Force Reallocation of terminal stuff
			with0->ElementName = OtherVVCControl->ElementName;
			with0->ControlledElement = OtherVVCControl->ControlledElement;
      // Pointer to target circuit element
			with0->MonitoredElement = OtherVVCControl->MonitoredElement;
      // Pointer to monitored circuit element
			with0->ElementTerminal = OtherVVCControl->ElementTerminal;
			with0->CondOffset = OtherVVCControl->CondOffset;
			with0->DeltaVTolerance = OtherVVCControl->DeltaVTolerance;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->PropertyValue[i] = OtherVVCControl->PropertyValue[i];
			}
		}
	else
		DoSimpleMsg(String(L"Error in VVCControl MakeLike: \"") + VVCControlName
	           + L"\" Not Found.", 370);
	return result;
}

/* ========================================================================== */
/* TVVControlObj */
/* ========================================================================== */

/* -------------------------------------------------------------------------- */

TVVControlObj::TVVControlObj(TDSSClass* ParClass, const String VVCControlName)
 : inherited(ParClass),
			Fvvc_Vmaxpu(1.1),
			Fvvc_Vminpu(0.9),
			Fkva_rating(7.0),
			FkW_rating(5.83),
			Fkvar_fulloutput(3.86),
			Fpf(0.83),
			FDelay(0.0),
			Fdelayoff(0.0),
			FkW_ramp_rate(-1.0),
			Fkvar_ramp_rate(-1.0),
			FkW_limit(10000),
			Fkvar_limit(FkW_limit / 2.0),
			DeltaVTolerance(0.00001),
			TotalWeight(1.0),
			QOldDeliver(0.0),
			Qdeliver(0.0),
			QNew(0.0),
			VavgpuPrior(0.0),
			Vavgpu(0.0),
			presentHour(0.0),
			ControlActionHandle(0),
			FListSize(0),
			FGeneratorNameList(nullptr),
			FGenPointerList(nullptr),
			FWeights(nullptr),
			Fvvc_curve_size(0),
			Fvvc_curve(nullptr),
			FPendingChange(0),
			FdeltaQ_factor(0.0),
			MonitoredElement(nullptr),
			cBuffer(nullptr),
			CondOffset(0)
{
	Name = LowerCase(VVCControlName);
	DSSObjType = ParClass->DSSClassType;
	NPhases = 1; // Directly set conds and phases
	Fnconds = 3;
	NTerms = 1; // this forces allocation of terminals and conductors
  // in base class
	ElementName = L"";
	ControlledElement = nullptr;
	ElementTerminal = 1;
	MonitoredElement = nullptr;
	FGeneratorNameList = new TStringList();
	FWeights = nullptr;
	FGenPointerList = new PointerList::TPointerList(20);
  // Default size and increment
	FListSize = 0;
	FdeltaQ_factor = 0.1;
	Qdeliver = 1.0;
	QOldDeliver = 0.0;
	QNew = 0.0;
	VavgpuPrior = 0.0;
	Vavgpu = 0.0;
	presentHour = -1.0;
	InitPropertyValues(0);
	Fvvc_curve = nullptr;
	Fvvc_curve_size = 0;
	PendingChange = None;
}

TVVControlObj::~TVVControlObj()
{
	ElementName = L"";
	// inherited::Destroy();
}


/* -------------------------------------------------------------------------- */

void TVVControlObj::RecalcElementData()
{
	int DevIndex = 0;

  /* Check for existence of monitored element */
	DevIndex = GetCktElementIndex(ElementName); // Global function
	if(DevIndex > 0)
	{
		MonitoredElement = ActiveCircuit.CktElements.Get(DevIndex);
		if(ElementTerminal > MonitoredElement->NTerms)
		{
			DoErrorMsg(String(L"VVCControl: \"") + Name + L"\"", String(L"Terminal no. \"") + Format(L"%-d", ARRAYOFCONST((ElementTerminal)))
	           + L"\" does not exist.", L"Re-specify terminal no.", 371);
		}
		else

      // Sets name of i-th terminal's connected bus in VVCControl's buslist
		{
			SetBus(1, MonitoredElement->GetBus(ElementTerminal));
		}
		cBuffer = (pComplexArray) ReallocMemory(cBuffer, sizeof((*cBuffer)[1 - 1]) * MonitoredElement->Yorder);
		CondOffset = (ElementTerminal - 1) * MonitoredElement->NConds;
    // for speedy sampling
	}
	else
	DoSimpleMsg(String(L"Monitored Element in VVCControl.") + Name
	           + L" does not exist:\""
	           + ElementName
	           + L"\"", 372);
	if(FGenPointerList.get_myNumList() == 0)
		MakeGenList();
	DevIndex = GetCktElementIndex(String(L"generator.") + FGeneratorNameList->Strings[0]);
  // Global function
	if(DevIndex > 0)
  // right now we only support one controlled element (generator) per vvcontrol
 // Controlled element must already exist
	{
		ControlledElement = ActiveCircuit.CktElements.Get(DevIndex);
		ControlledElement->ActiveTerminalIdx = 1; // Make the 1 st terminal active
    // Get control synched up with capacitor
	}
	else
	{
		ControlledElement = nullptr; // element not found
		DoErrorMsg(String(L"VVControl: \"") + this->Name + L"\"", String(L"Controlled Element \"") + FGeneratorNameList->Strings[0]
	           + L"\" Not Found.", L" Element must be defined previously.", 361);
	}
}

void TVVControlObj::MakePosSequence()
{
	if(ControlledElement != nullptr)
	{
		Enabled = ControlledElement->Enabled;
		NPhases = ControlledElement->NPhases;
		Set_Nconds(Fnphases);
	}
	if(MonitoredElement != nullptr)
	{
		SetBus(1, MonitoredElement->GetBus(ElementTerminal));
    // Allocate a buffer bigenough to hold everything from the monitored element
    // ReAllocMem(cBuffer, SizeOF(cBuffer^[1]) * MonitoredElement.Yorder );
    // CondOffset := (ElementTerminal-1) * MonitoredElement.NConds; // for speedy sampling
	}
	inherited::MakePosSequence();
}

/* -------------------------------------------------------------------------- */

void TVVControlObj::CalcYPrim()
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  // IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}

/* -------------------------------------------------------------------------- */

void TVVControlObj::GetCurrents(pComplexArray Curr)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(*Curr)[i - 1] = CZero;
	}
}

void TVVControlObj::GetInjCurrents(pComplexArray Curr)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(*Curr)[i - 1] = CZero;
	}
}

/* -------------------------------------------------------------------------- */

void TVVControlObj::DumpProperties(d2c_system::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, L"~ "); Write(f, (*with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, PropertyValue[i]); }
		}
	}
	if(Complete)
	{
		WriteLn(f);
	}
}

/* -------------------------------------------------------------------------- */

void TVVControlObj::DoPendingAction(int Code, int ProxyHdl)
{
	int i = 0;
	double DeltaQ = 0.0;
	double QHeadRoom = 0.0;
	double Qdesiredpu = 0.0;
	double QNeeded = 0.0;
	double PPresentGenOutput = 0.0;
	double QPresentGenOutput = 0.0;
	double QMonitoredElement = 0.0;
	double Genkvar = 0.0;
	complex SMonitoredElement = {};
	complex SPresentGenOutput = {};
	TGeneratorObj* Gen = nullptr;
	Genkvar = 0.0;
  // we need P and/or we need Q
	if(PendingChange == CHANGEVARLEVEL)
	{
		SMonitoredElement = MonitoredElement->Get_Power(ElementTerminal, ActiveActor); // s is in va
    // PMonitoredElement := SMonitoredElement.re;
		QMonitoredElement = SMonitoredElement.im;

    // PNeeded := FkW_limit*1000 - PMonitoredElement;
		QNeeded = Fkvar_limit * 1000 - QMonitoredElement;
    // If the generator list is not defined, go make one
		if(FGenPointerList.get_myNumList() == 0)
			MakeGenList();
		ControlledElement->ActiveTerminalIdx = 1; // Set active terminal of generator to terminal 1
		if(QNeeded != 0.0)
		{
			int stop = 0;
			for(stop = FListSize, i = 1; i <= stop; i++)
			{
				SPresentGenOutput = ControlledElement->Get_Power(1, ActiveActor);
        // s is in va; we want terminal 1 of the generator
				PPresentGenOutput = SPresentGenOutput.re;
				QPresentGenOutput = SPresentGenOutput.im;

        // q desired pu is the desired output based on the avg pu voltage on the
        // monitored element
				Qdesiredpu = Fvvc_curve->GetYValue(Vavgpu);      //Y value = var in pu

        // The var 'head-room' available on the inverter given its rating
        // and present kW output
				if(Abs( PPresentGenOutput) > Fkva_rating * 1000.0)
				{
					QHeadRoom = 0.0;
				}
				else
				QHeadRoom = Sqrt(Sqr(Fkva_rating * 1000.0) - Sqr(PPresentGenOutput));
				Qdeliver = Min(Abs( (Fkvar_fulloutput * 1000.0)), Abs( QHeadRoom));
				Qdeliver = Qdeliver * Qdesiredpu;
				DeltaQ = Qdeliver - QOldDeliver;

        // only allow a small movement from old delivered (prior gen Q)
        // to the desired delivered Q
				QNew = QOldDeliver + DeltaQ * FdeltaQ_factor;
				if(QNew != QPresentGenOutput)
				{
					Gen = FGenPointerList.Get(i);
					Genkvar = Sign(QNew) * (Min(Abs( (Fkvar_limit * 1000.0)), Abs( QNew))) / 1000.0L;
					if(Genkvar != Gen->kvarBase)
					{
						Gen->Set_Presentkvar(Genkvar);
					}
				}
				AppendToEventLog(String(L"VoltVarControl.") + this->Name, Format(L"**Set var output level to**, kvar= %.5g", ARRAYOFCONST((Genkvar))));
			} // end for i equals 1 to number of generators under this control
		} // end if vars needed is not equal to zero
    // WriteDLLDebugFile(Self.Name+','+Format('%-.5g',[ActiveCircuit.Solution.dblHour])+','+Format('%-.5g',[QPresentGenOutput])+','+Format('%-.5g',[Qdeliver])+','+Format('%-.5g',[DeltaQ])+','+Format('%-.5g',[Qnew])+','+Format('%-.5g',[Gen.Presentkvar*1000.0])+','+Format('%-.5g',[QoldDeliver])+','+Format('%-.5g',[PPresentGenOutput])+','+Format('%-.5g',[Vavgpu])+','+Format('%-.5g',[Vavgpuprior])+','+Format('%-.5g',[Qdesiredpu]));
		QOldDeliver = QNew;
		VavgpuPrior = Vavgpu;
		ActiveCircuit.Solution.LoadsNeedUpdating = true;
    // Force recalc of power parms
		Set_PendingChange(None); // end if PendingChange = CHANGEVARLEVEL
	}
	else
 // else set PendingChange to NONE
	{
		Set_PendingChange(None);
	}
}

/* -------------------------------------------------------------------------- */

void TVVControlObj::sample()
{
	int i = 0;
	double BaseKV = 0.0;
	double Vavg = 0.0;
  // If list is not defined, go make one for all generators in circuit
	if(FGenPointerList.get_myNumList() == 0)
		MakeGenList();
	if((FListSize > 0) && (Fvvc_curve_size > 0))

    // if(presentHour <> ActiveCircuit.Solution.dblHour) then begin
    // WriteDLLDebugFile(Self.Name+','+Format('%-.5g',[ActiveCircuit.Solution.dblHour])+','+Format('%-.5g',[QPresentGenOutput])+','+Format('%-.5g',[Qdeliver])+','+Format('%-.5g',[QNew])+','+Format('%-.5g',[Gen.Presentkvar*1000.0])+','+Format('%-.5g',[QoldDeliver])+','+Format('%-.5g',[PPresentGenOutput])+','+Format('%-.5g',[Vavgpu])+','+Format('%-.5g',[Vavgpuprior]));
    // presentHour = ActiveCircuit.Solution.dblHour;
    // end;
	{
		int stop = 0;
		MonitoredElement->ComputeVterminal();
		cBuffer = MonitoredElement->Vterminal;

    // get the basekV for the monitored bus
		BaseKV = ActiveCircuit.Buses[Terminals[ElementTerminal - 1].BusRef - 1].kVBase;
		Vavg = 0;

    // Calculate the average voltage
		for(stop = MonitoredElement->NPhases, i = 1; i <= stop; i++)
		{
			Vavg = Vavg + cabs((*cBuffer)[i - 1]);
		}

    // and convert to pu
		Vavgpu = (Vavg / MonitoredElement->NPhases) / (BaseKV * 1000.0);
		TimeDelay = FDelay;
    // and
    // if (ActiveCircuit.Solution.ControlIteration < ActiveCircuit.Solution.MaxControlIterations) then
    // begin
		if((Abs( (Vavgpu - VavgpuPrior)) > DeltaVTolerance) || (Abs(Abs( Qdeliver) - Abs( QNew)) > 0.5))
		{
			Set_PendingChange(CHANGEVARLEVEL);
      // ActiveCircuit.Solution.LoadsNeedUpdating := TRUE; // Force recalc of power parms
			ControlActionHandle = ActiveCircuit.ControlQueue.Push(ActiveCircuit.Solution.DynaVars.intHour, ActiveCircuit.Solution.DynaVars.T + TimeDelay, PendingChange, 0, this);
			AppendToEventLog(String(L"VoltVarControl.") + this->Name, Format(L"**Ready to change var output**, Vavgpu= %.5g sec,", ARRAYOFCONST((Vavgpu))));
    // end;
		}
		else
		{
			ActiveCircuit.ControlQueue.Delete(ControlActionHandle);
			AppendToEventLog(String(L"VoltVarControl.") + this->Name, L"**DONE**");
		}
	}
	else
	{
		DoSimpleMsg(L"Could not find any generators, or the vvc curve size is zero.  Please correct in your script.", 1234);
	}
}

void TVVControlObj::InitPropertyValues(int ArrayOffset)
{
	PropertyValue[1] = L""; // 'element';
	PropertyValue[2] = L'1'; // 'terminal';
	PropertyValue[3] = L"1.1"; // vmax_pu of the inverter
	PropertyValue[4] = L"0.9"; // vmin_pu of the inverter
	PropertyValue[5] = L"7.0";
	PropertyValue[6] = L"5.83";
	PropertyValue[7] = L"3.5";
	PropertyValue[8] = L"0.83";
	PropertyValue[9] = L"0.0";
	PropertyValue[10] = L"0.0";
	PropertyValue[11] = L"-1.0";
	PropertyValue[12] = L"-1.0";
	PropertyValue[13] = L"1000.0"; // kw_limit through the monitored element
	PropertyValue[14] = L"500.0"; // kvar_limit through the monitored element
	PropertyValue[15] = L"";
	PropertyValue[16] = L"";
	PropertyValue[17] = L'0'; // curve size
	PropertyValue[18] = L"none"; // volt-var curve
	PropertyValue[19] = L"0.1"; // deltaQ_factor
	inherited::InitPropertyValues(NumPropsThisClass);
}

// need to edit this for the :  WGS
// ----------------------------------------------------------------------------

String TVVControlObj::GetPropertyValue(int Index)
{
	String result;
	result = L"";
	switch(Index)
	{
		case 	1:
		result = MonitoredElement->DisplayName;
		break;
		case 	2:
		result = Format(L"%-d", ARRAYOFCONST((ElementTerminal)));
		break;
		case 	3:
		result = Format(L"%-.3g", ARRAYOFCONST((Fvvc_Vmaxpu)));
		break;
		case 	4:
		result = Format(L"%-.3g", ARRAYOFCONST((Fvvc_Vminpu)));
		break;
		case 	5:
		result = Format(L"%-.3g", ARRAYOFCONST((Fkva_rating)));
		break;
		case 	6:
		result = Format(L"%-.3g", ARRAYOFCONST((FkW_rating)));
		break;
		case 	7:
		result = Format(L"%-.3g", ARRAYOFCONST((Fkvar_fulloutput)));
		break;
		case 	8:
		result = Format(L"%-.3g", ARRAYOFCONST((Fpf)));
		break;
		case 	9:
		result = Format(L"%-.3g", ARRAYOFCONST((FDelay)));
		break;
		case 	10:
		result = Format(L"%-.3g", ARRAYOFCONST((Fdelayoff)));
		break;
		case 	11:
		result = Format(L"%-.3g", ARRAYOFCONST((FkW_ramp_rate)));
		break;
		case 	12:
		result = Format(L"%-.3g", ARRAYOFCONST((Fkvar_ramp_rate)));
		break;
		case 	13:
		result = Format(L"%-.3g", ARRAYOFCONST((FkW_limit)));
		break;
		case 	14:
		result = Format(L"%-.3g", ARRAYOFCONST((Fkvar_limit)));
		break;
		case 	15:
		result = ReturnGensList();
		break;
		case 	16:
		result = ReturnWeightsList();
		break;
		case 	17:
		result = Format(L"%-d", ARRAYOFCONST((Fvvc_curve_size)));
		break;
		case 	18:
		result = ReturnVVCurve();
		break;
		case 	19:
		result = Format(L"%-.3g", ARRAYOFCONST((FdeltaQ_factor)));
		break; // take the generic handler
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

bool TVVControlObj::MakeGenList()
{
	bool result = false;
	TDSSClass* GenClass = nullptr;
	TGeneratorObj* Gen = nullptr;
	int i = 0;
	int stop = 0;
	result = false;
	GenClass = GetDSSClassPtr(L"generator");
	if(FListSize > 0) // Name list is defined - Use it
	{
		int stop = 0;
		for(stop = FListSize, i = 1; i <= stop; i++)
		{
			Gen = ((TGeneratorObj*) GenClass->Find(String(FGeneratorNameList->Strings[i - 1])));
			if(ASSIGNED(Gen) && Gen->Enabled)
				FGenPointerList.New() = Gen;
		}
	}
	else

    /* Search through the entire circuit for enabled generators and add them to the list */
	{
		int stop = 0;
		for(stop = GenClass->ElementCount, i = 1; i <= stop; i++)
		{
			Gen = GenClass->ElementList.Get((void*&)i);
			if(Gen->Enabled)
				FGenPointerList.New() = Gen;
			FGeneratorNameList->Add(Gen->DisplayName);
		}

    /* Allocate uniform weights */
		FListSize = FGenPointerList.get_myNumList();
		FWeights = (pDoubleArray) ReallocMemory(FWeights, sizeof((*FWeights)[1 - 1]) * FListSize);
		for(stop = FListSize, i = 1; i <= stop; i++)
		{
			(*FWeights)[i - 1] = 1.0;
		}
	}

  // Add up total weights
	TotalWeight = 0.0;
	for(stop = FListSize, i = 1; i <= stop; i++)
	{
		TotalWeight = TotalWeight + (*FWeights)[i - 1];
	}
	if(FGenPointerList.get_myNumList() > 0)
		result = true;
	return result;
}

// -----------------------------------------------------------------------------

String TVVControlObj::ReturnGensList()
{
	String result;
	int i = 0;
	int stop = 0;
	if(FListSize == 0)
	{
		result = L"";
		return result;
	}
	result = String(L"[") + FGeneratorNameList->Strings[0];
	for(stop = FListSize - 1, i = 1; i <= stop; i++)
	{
		result = result + L", " + String(FGeneratorNameList->Strings[i]);
	}
	result = result + L"]"; // terminate the array
	return result;
}

// ----------------------------------------------------------------------------

String TVVControlObj::ReturnWeightsList()
{
	String result;
	int i = 0;
	int stop = 0;
	if(FListSize == 0)
	{
		result = L"";
		return result;
	}
	result = String(L"[") + Format(L"%-.6g", ARRAYOFCONST(((*FWeights)[1 - 1])));
	for(stop = FListSize, i = 2; i <= stop; i++)
	{
		result = result + Format(L", %-.6g", ARRAYOFCONST(((*FWeights)[i - 1])));
	}
	result = result + L"]"; // terminate the array
	return result;
}

// ----------------------------------------------------------------------------

String TVVControlObj::ReturnVVCurve()
{
	String result;
	int i = 0;
	int stop = 0;
	if(Fvvc_curve_size == 0)
	{
		result = L"";
		return result;
	}
	result = String(L"[{") + Format(L"%-.3g,", ARRAYOFCONST((Fvvc_curve->Get_XValue(1))))
	           + Format(L"%-.3g", ARRAYOFCONST((Fvvc_curve->Get_YValue(1))))
	           + L"},";
	for(stop = Fvvc_curve_size, i = 2; i <= stop; i++)
	{
		result = result
	           + Format(L"{ %-.3g,", ARRAYOFCONST((Fvvc_curve->Get_XValue(i))))
	           + Format(L"%-.3g", ARRAYOFCONST((Fvvc_curve->Get_YValue(i))))
	           + L"},";
	}
	result = result + L"]"; // terminate the array
	return result;
}

void TVVControlObj::Reset()
{
	PendingChange = None;
}

void TVVControlObj::Set_PendingChange(int Value)
{
	FPendingChange = Value;
	DblTraceParameter = (double) Value;
}




}  // namespace VVControl





