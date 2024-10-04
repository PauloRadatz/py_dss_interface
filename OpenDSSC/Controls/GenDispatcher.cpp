
#pragma hdrstop

#include "GenDispatcher.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "generator.h"
#include "Ucmatrix.h"
#include "mathutil.h"
#include <math.h>
#include "Utilities.h"

using namespace std;
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
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace GenDispatcher
{

TGenDispatcherObj::TGenDispatcherObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TGenDispatcherObj::TGenDispatcherObj(String ClassName) : inherited(ClassName) {}
TGenDispatcherObj::TGenDispatcherObj() {}


TGenDispatcherObj* ActiveGenDispatcherObj = nullptr;
const int NumPropsThisClass = 6;


/*--------------------------------------------------------------------------*/  // Creates superstructure for all GenDispatcher objects

TGenDispatcher::TGenDispatcher()
{
	;
	Class_Name = "GenDispatcher";
	DSSClassType = DSSClassType + GEN_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

/*--------------------------------------------------------------------------*/

TGenDispatcher::~TGenDispatcher()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGenDispatcher::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "Element";
	PropertyName[2 - 1] = "Terminal";
	PropertyName[3 - 1] = "kWLimit";
	PropertyName[4 - 1] = "kWBand";
	PropertyName[5 - 1] = "kvarlimit";
	PropertyName[6 - 1] = "GenList";
	PropertyName[7 - 1] = "Weights";
	PropertyHelp[1 - 1] = "Full object name of the circuit element, typically a line or transformer, "
	           "which the control is monitoring. There is no default; must be specified.";
	PropertyHelp[2 - 1] = "Number of the terminal of the circuit element to which the GenDispatcher control is connected. "
	           "1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.";
	PropertyHelp[3 - 1] = "kW Limit for the monitored element. The generators are dispatched to hold the power in band.";
	PropertyHelp[4 - 1] = "Bandwidth (kW) of the dead band around the target limit."
	           "No dispatch changes are attempted if the power in the monitored terminal stays within this band.";
	PropertyHelp[5 - 1] = "Max kvar to be delivered through the element.  Uses same dead band as kW.";
	PropertyHelp[6 - 1] = "Array list of generators to be dispatched.  If not specified, all generators in the circuit are assumed dispatchable.";
	PropertyHelp[7 - 1] = "Array of proportional weights corresponding to each generator in the GenList."
	           " The needed kW to get back to center band is dispatched to each generator according to these weights. "
	           "Default is to set all weights to 1.0.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TGenDispatcher::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new GenDispatcher and add it to GenDispatcher class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TGenDispatcherObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TGenDispatcher::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int i = 0;

  // continue parsing WITH contents of Parser
	ActiveGenDispatcherObj = (TGenDispatcherObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveGenDispatcherObj);
	result = 0;
	/*# with ActiveGenDispatcherObj do */
	{
		auto with0 = ActiveGenDispatcherObj;
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
				with0->ElementName = LowerCase(Param);
				break;
				case 	2:
				with0->ElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->FkWLimit = Parser[ActorID]->MakeDouble_();
				break;
				case 	4:
				with0->FkWBand = Parser[ActorID]->MakeDouble_();
				break;
				case 	5:
				with0->Fkvarlimit = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				InterpretTStringListArray(Param, *(with0->FGeneratorNameList));
				break;
				case 	7:
				{
					with0->FListSize = with0->FGeneratorNameList->size();
					if(with0->FListSize > 0)
					{
						with0->FWeights = (pDoubleArray)realloc(with0->FWeights, sizeof(double) * with0->FListSize);
						with0->FListSize = InterpretDblArray(Param, with0->FListSize, with0->FWeights);
					}
				}
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveGenDispatcherObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	4:
				with0->HalfkWBand = with0->FkWBand / 2.0;
				break;   // levelize the list
				case 	6:
				{
					int stop = 0;
					with0->FGenPointerList->Clear();  // clear this for resetting on first sample
					with0->FListSize = with0->FGeneratorNameList->size();
					with0->FWeights = (pDoubleArray)realloc(with0->FWeights, sizeof(double) * with0->FListSize);
					for(stop = with0->FListSize, i = 1; i <= stop; i++)
					{
						(with0->FWeights)[i - 1] = 1.0;
					}
				}
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		with0->RecalcElementData(ActorID);
	}
	return result;
}



/*--------------------------------------------------------------------------*/

int TGenDispatcher::MakeLike(const String GenDispatcherName)
{
	int result = 0;
	TGenDispatcherObj* OtherGenDispatcher = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this GenDispatcher name in the present collection*/
	OtherGenDispatcher = ((TGenDispatcherObj*) Find(GenDispatcherName));
	if(OtherGenDispatcher != nullptr)
		/*# with ActiveGenDispatcherObj do */
		{
			auto with0 = ActiveGenDispatcherObj;
			int stop = 0;
			with0->Set_NPhases(OtherGenDispatcher->Fnphases);
			with0->Set_Nconds(OtherGenDispatcher->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherGenDispatcher->ElementName;
			with0->Set_ControlledElement(OtherGenDispatcher->get_FControlledElement());  // Pointer to target circuit element
			with0->Set_MonitoredElement(OtherGenDispatcher->get_FMonitoredElement());  // Pointer to target circuit element
			with0->ElementTerminal = OtherGenDispatcher->ElementTerminal;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i, OtherGenDispatcher->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in GenDispatcher MakeLike: \"") + GenDispatcherName
	           + "\" Not Found.", 370);
	return result;
}




/*==========================================================================*/
/*                    TGenDispatcherObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TGenDispatcherObj::TGenDispatcherObj(TDSSClass* ParClass, const String GenDispatcherName)
 : inherited(ParClass),
			FkWLimit(8000.0),
			FkWBand(100.0),
			HalfkWBand(FkWBand / 2.0),
			Fkvarlimit(FkWLimit / 2.0),
			TotalWeight(0.0),
			FListSize(0),
			FGeneratorNameList(nullptr),
			FGenPointerList(nullptr),
			FWeights(nullptr)
{
	Set_Name(LowerCase(GenDispatcherName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	ElementName = "";
	Set_ControlledElement(nullptr);  // not used in this control
	ElementTerminal = 1;
	Set_MonitoredElement(nullptr);
	FGeneratorNameList = new TStringList();
	FWeights = nullptr;
	FGenPointerList = new PointerList::TPointerList(20);  // Default size and increment
	FListSize = 0;
	TotalWeight = 1.0;
	InitPropertyValues(0);
}

TGenDispatcherObj::~TGenDispatcherObj()
{
	ElementName = "";
	// inherited::Destroy();
}


/*--------------------------------------------------------------------------*/

void TGenDispatcherObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;


/*Check for existence of monitored element*/
	DevIndex = GetCktElementIndex(ElementName); // Global function
	if(DevIndex > 0)
	{
		Set_MonitoredElement((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		if(ElementTerminal > get_FMonitoredElement()->Get_NTerms())
		{
			DoErrorMsg(String("GenDispatcher: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Re-specify terminal no.", 371);
		}
		else

               // Sets name of i-th terminal's connected bus in GenDispatcher's buslist
		{
			SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
		}
	}
	else
	DoSimpleMsg(String("Monitored Element in GenDispatcher.") + get_Name()
	           + " does not exist:\""
	           + ElementName
	           + "\"", 372);
}

void TGenDispatcherObj::MakePosSequence(int ActorID)
{
	if(get_FMonitoredElement() != nullptr)
	{
		Set_NPhases(get_FControlledElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
	}
	inherited::MakePosSequence(ActorID);
}

/*--------------------------------------------------------------------------*/

void TGenDispatcherObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}






/*--------------------------------------------------------------------------*/

void TGenDispatcherObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TGenDispatcherObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TGenDispatcherObj::DumpProperties(TTextRec& f, bool Complete)
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


/*--------------------------------------------------------------------------*/

void TGenDispatcherObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{


        /*Do Nothing*/
}

/*--------------------------------------------------------------------------*/

void TGenDispatcherObj::sample(int ActorID)
{
	int i = 0;
	double PDiff = 0.0;
	double QDiff = 0.0;
	complex s = {};
	TGeneratorObj* Gen = nullptr;
	bool GenkWChanged = false;
	bool Genkvarchanged = false;
	double GenkW = 0.0;
	double Genkvar = 0.0;
     // If list is not define, go make one from all generators in circuit
	if(FGenPointerList->get_myNumList() == 0)
		MakeGenList();
	if(FListSize > 0)

       //----MonitoredElement.ActiveTerminalIdx := ElementTerminal;
	{
		s = get_FMonitoredElement()->Get_Power(ElementTerminal, ActorID);  // Power in active terminal
		PDiff = s.re * 0.001 - FkWLimit;
		QDiff = s.im * 0.001 - Fkvarlimit;

       // Redispatch the vars.
		GenkWChanged = false;
		Genkvarchanged = false;
		if(Abs(PDiff) > HalfkWBand) // Redispatch Generators
          // PDiff is kW needed to get back into band
		{
			int stop = 0;
			for(stop = FListSize, i = 1; i <= stop; i++)
			{
				Gen = (TGeneratorObj*) FGenPointerList->Get(i);
              // compute new dispatch value for this generator ...
				GenkW = max(1.0, (Gen->kWBase + PDiff * ((FWeights)[i - 1] / TotalWeight)));
				if(GenkW != Gen->kWBase)
				{
					Gen->kWBase = GenkW;
					GenkWChanged = true;
				}
			}
		}
		if(Abs(QDiff) > HalfkWBand) // Redispatch Generators
          // QDiff is kvar needed to get back into band
		{
			int stop = 0;
			for(stop = FListSize, i = 1; i <= stop; i++)
			{
				Gen = (TGeneratorObj*) FGenPointerList->Get(i);
              // compute new dispatch value for this generator ...
				Genkvar = max(0.0, (Gen->kvarBase + QDiff * ((FWeights)[i - 1] / TotalWeight)));
				if(Genkvar != Gen->kvarBase)
				{
					Gen->kvarBase = Genkvar;
					Genkvarchanged = true;
				}
			}
		}
		if(GenkWChanged || Genkvarchanged)
			/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
			{
				
				auto with1 = ActiveCircuit[ActorID]->Solution;  // Only push onto controlqueue if there has been a change
				with1->LoadsNeedUpdating = true; // Force recalc of power parms
            // Push present time onto control queue to force re solve at new dispatch value
				ActiveCircuit[ActorID]->ControlQueue.Push(with1->DynaVars.intHour, with1->DynaVars.T, (EControlAction) 0, 0, this, ActorID);
			}
       

       /*Else just continue*/
	}
}

void TGenDispatcherObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1, "");   //'element';
	Set_PropertyValue(2, "1");   //'terminal';
	Set_PropertyValue(3, "8000");
	Set_PropertyValue(4, "100");
	Set_PropertyValue(5, "0");
	Set_PropertyValue(6, "");
	Set_PropertyValue(7, "");
	inherited::InitPropertyValues(NumPropsThisClass);
}

bool TGenDispatcherObj::MakeGenList()
{
	bool result = false;
	TDSSClass* GenClass = nullptr;
	TGeneratorObj* Gen = nullptr;
	int i = 0;
	int stop = 0;
	result = false;
	GenClass = (TDSSClass*)GetDSSClassPtr("generator");
	if(FListSize > 0)    // Name list is defined - Use it
	{
		int stop = 0;
		for(stop = FListSize, i = 1; i <= stop; i++)
		{
			Gen = ((TGeneratorObj*) GenClass->Find( String( (*FGeneratorNameList)[i - 1] ) ) );
			if(ASSIGNED(Gen) && ( (TDSSCktElement*) Gen )->Get_Enabled())
				FGenPointerList->Set_New(Gen);
		}
	}
	else

     /*Search through the entire circuit for enabled generators and add them to the list*/
	{
		int stop = 0;
		for(stop = GenClass->Get_ElementCount(), i = 1; i <= stop; i++)
		{
			Gen = (TGeneratorObj*) GenClass->ElementList.Get(i);
			if( ( (TDSSCktElement*) Gen )->Get_Enabled())
				FGenPointerList->Set_New(Gen);
		}

     /*Allocate uniform weights*/
		FListSize = FGenPointerList->get_myNumList();
		FWeights = (pDoubleArray)realloc(FWeights, sizeof(double) * FListSize);
		for(stop = FListSize, i = 1; i <= stop; i++)
		{
			(FWeights)[i - 1] = 1.0;
		}
	}

   // Add up total weights
	TotalWeight = 0.0;
	for(stop = FListSize, i = 1; i <= stop; i++)
	{
		TotalWeight = TotalWeight + (FWeights)[i - 1];
	}
	if(FGenPointerList->get_myNumList() > 0)
		result = true;
	return result;
}

void TGenDispatcherObj::Reset(int ActorID)
{

  // inherited;
}




}  // namespace GenDispatcher





