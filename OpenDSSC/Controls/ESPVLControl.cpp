
#pragma hdrstop

#include "ESPVLControl.h"

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
using namespace LoadShape;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace ESPVLControl
{

TESPVLControlObj::TESPVLControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TESPVLControlObj::TESPVLControlObj(String ClassName) : inherited(ClassName) {}
TESPVLControlObj::TESPVLControlObj() {}


TESPVLControl* ESPVLControlClass = nullptr;
TESPVLControlObj* ActiveESPVLControlObj = nullptr;
const int NumPropsThisClass = 12;


/*--------------------------------------------------------------------------*/  // Creates superstructure for all ESPVLControl objects

TESPVLControl::TESPVLControl()
{
	;
	Class_Name = "ESPVLControl";
	DSSClassType = DSSClassType + ESPVL_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	ESPVLControlClass = this;
}

/*--------------------------------------------------------------------------*/

TESPVLControl::~TESPVLControl()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TESPVLControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "Element";
	PropertyName[2 - 1] = "Terminal";
	PropertyName[3 - 1] = "Type";
	PropertyName[4 - 1] = "kWBand";
	PropertyName[5 - 1] = "kvarlimit";
	PropertyName[6 - 1] = "LocalControlList";
	PropertyName[7 - 1] = "LocalControlWeights";
	PropertyName[8 - 1] = "PVSystemList";
	PropertyName[9 - 1] = "PVSystemWeights";
	PropertyName[10 - 1] = "StorageList";
	PropertyName[11 - 1] = "StorageWeights";
	PropertyName[12 - 1] = "Forecast";
	PropertyHelp[1 - 1] = "Full object name of the circuit element, typically a line or transformer, "
	           "which the control is monitoring. There is no default; must be specified.";
	PropertyHelp[2 - 1] = "Number of the terminal of the circuit element to which the ESPVLControl control is connected. "
	           "1 or 2, typically.  Default is 1. Make sure you have the direction on the power matching the sign of kWLimit.";
	PropertyHelp[3 - 1] = "Type of controller.  1= System Controller; 2= Local controller. ";
	PropertyHelp[4 - 1] = "Bandwidth (kW) of the dead band around the target limit."
	           "No dispatch changes are attempted if the power in the monitored terminal stays within this band.";
	PropertyHelp[5 - 1] = "Max kvar to be delivered through the element.  Uses same dead band as kW.";
	PropertyHelp[6 - 1] = "Array list of ESPVLControl local controller objects to be dispatched by System Controller. "
	           "If not specified, all ESPVLControl devices with type=local in the circuit not attached to another "
	           "controller are assumed to be part of this controller's fleet.";
	PropertyHelp[7 - 1] = "Array of proportional weights corresponding to each ESPVLControl local controller in the LocalControlList.";
	PropertyHelp[8 - 1] = "Array list of PVSystem objects to be dispatched by a Local Controller. ";
	PropertyHelp[9 - 1] = "Array of proportional weights corresponding to each PVSystem in the PVSystemList.";
	PropertyHelp[10 - 1] = "Array list of Storage objects to be dispatched by Local Controller. ";
	PropertyHelp[11 - 1] = "Array of proportional weights corresponding to each Storage object in the StorageControlList.";
	PropertyHelp[12 - 1] = "Loadshape object containing daily forecast.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TESPVLControl::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new ESPVLControl and add it to ESPVLControl class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TESPVLControlObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TESPVLControl::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int i = 0;

  // continue parsing WITH contents of Parser
	ActiveESPVLControlObj = (TESPVLControlObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveESPVLControlObj);
	result = 0;
	/*# with ActiveESPVLControlObj do */
	{
		auto with0 = ActiveESPVLControlObj;
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
				switch(LowerCase(Param)[0])
				{
					case 	L's':
					with0->fType = 1;
					break;    /*for System Controller*/
					case 	L'l':
					with0->fType = 2;
					break;    /*for Local Controller*/
					default:
					  ;
					break;
				}
				break;
				case 	4:
				with0->FkWBand = Parser[ActorID]->MakeDouble_();
				break;
				case 	5:
				with0->Fkvarlimit = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				InterpretTStringListArray(Param, *(with0->FLocalControlNameList));
				break;
				case 	7:
				{
					with0->FLocalControlListSize = with0->FLocalControlNameList->size();
					if(with0->FLocalControlListSize > 0)
					{
						with0->FLocalControlWeights = (pDoubleArray) realloc(with0->FLocalControlWeights, sizeof(double) * with0->FLocalControlListSize);
						with0->FLocalControlListSize = InterpretDblArray(Param, with0->FLocalControlListSize, with0->FLocalControlWeights);
					}
				}
				break;
				case 	8:
				InterpretTStringListArray(Param, *(with0->FPVSystemNameList));
				break;
				case 	9:
				{
					with0->FPVsystemListSize = with0->FPVSystemNameList->size();
					if(with0->FPVsystemListSize > 0)
					{
						with0->FPVSystemWeights = (pDoubleArray)realloc(with0->FPVSystemWeights, sizeof(double) * with0->FPVsystemListSize);
						with0->FPVsystemListSize = InterpretDblArray(Param, with0->FPVsystemListSize, with0->FPVSystemWeights);
					}
				}
				break;
				case 	10:
				InterpretTStringListArray(Param, *(with0->FStorageNameList));
				break;
				case 	11:
				{
					with0->FStorageListSize = with0->FStorageNameList->size();
					if(with0->FStorageListSize > 0)
					{
						with0->FStorageWeights = (pDoubleArray)realloc(with0->FStorageWeights, sizeof(double) * with0->FStorageListSize);
						with0->FStorageListSize = InterpretDblArray(Param, with0->FStorageListSize, with0->FStorageWeights);
					}
				}
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveESPVLControlObj, ParamPointer - NumPropsThisClass);
				break;
			}

         // Side Effects
			switch(ParamPointer)
			{
				case 	6:   // levelize the list
				{
					int stop = 0;
					with0->FLocalControlPointerList->Clear();  // clear this for resetting on first sample
					with0->FLocalControlListSize = with0->FLocalControlNameList->size();
					with0->FLocalControlWeights = (pDoubleArray)realloc(with0->FLocalControlWeights, sizeof(double) * with0->FLocalControlListSize);
					for(stop = with0->FLocalControlListSize, i = 1; i <= stop; i++)
					{
						(with0->FLocalControlWeights)[i - 1] = 1.0;
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

int TESPVLControl::MakeLike(const String ESPVLControlName)
{
	int result = 0;
	TESPVLControlObj* OtherESPVLControl = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this ESPVLControl name in the present collection*/
	OtherESPVLControl = ((TESPVLControlObj*) Find(ESPVLControlName));
	if(OtherESPVLControl != nullptr)
		/*# with ActiveESPVLControlObj do */
		{
			auto with0 = ActiveESPVLControlObj;
			int stop = 0;
			with0->Set_NPhases(OtherESPVLControl->Fnphases);
			with0->Set_Nconds(OtherESPVLControl->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherESPVLControl->ElementName;
			with0->Set_ControlledElement(OtherESPVLControl->get_FControlledElement());  // Pointer to target circuit element
			with0->Set_MonitoredElement(OtherESPVLControl->get_FMonitoredElement());  // Pointer to target circuit element
			with0->ElementTerminal = OtherESPVLControl->ElementTerminal;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i, OtherESPVLControl->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in ESPVLControl MakeLike: \"") + ESPVLControlName
	           + "\" Not Found.", 370);
	return result;
}




/*==========================================================================*/
/*                    TESPVLControlObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TESPVLControlObj::TESPVLControlObj(TDSSClass* ParClass, const String ESPVLControlName)
 : inherited(ParClass),
			fType(0),
			FLocalControlListSize(0),
			FLocalControlNameList(nullptr),
			FLocalControlPointerList(nullptr),
			FLocalControlWeights(nullptr),
			FPVsystemListSize(0),
			FPVSystemNameList(nullptr),
			FPVSystemPointerList(nullptr),
			FPVSystemWeights(nullptr),
			FStorageListSize(0),
			FStorageNameList(nullptr),
			FStoragePointerList(nullptr),
			FStorageWeights(nullptr),
			FkWLimit(0.0),
			FkWBand(0.0),
			HalfkWBand(0.0),
			Fkvarlimit(0.0),
			TotalWeight(0.0),
			DailyForecasstShapeObj(nullptr)
{
	Set_Name(LowerCase(ESPVLControlName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	ElementName = "";
	Set_ControlledElement(nullptr);  // not used in this control
	ElementTerminal = 1;
	Set_MonitoredElement(nullptr);
	FLocalControlNameList = new TStringList();
	FLocalControlWeights = nullptr;
	FLocalControlPointerList = new PointerList::TPointerList(20);  // Default size and increment
	FLocalControlListSize = 0;
	FPVSystemNameList = new TStringList();
	FPVSystemWeights = nullptr;
	FPVSystemPointerList = new PointerList::TPointerList(20);  // Default size and increment
	FPVsystemListSize = 0;
	FStorageNameList = new TStringList();
	FStorageWeights = nullptr;
	FStoragePointerList = new PointerList::TPointerList(20);  // Default size and increment
	FStorageListSize = 0;
	FkWLimit = 8000.0;
	FkWBand = 100.0;
	TotalWeight = 1.0;
	HalfkWBand = FkWBand / 2.0;
	InitPropertyValues(0);
	Fkvarlimit = FkWLimit / 2.0;


   //  RecalcElementData;
}

TESPVLControlObj::~TESPVLControlObj()
{
	ElementName = "";
	// inherited::Destroy();
}


/*--------------------------------------------------------------------------*/

void TESPVLControlObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;


/*Check for existence of monitored element*/
	DevIndex = GetCktElementIndex(ElementName); // Global function
	if(DevIndex > 0)
	{
		Set_MonitoredElement((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		if(ElementTerminal > get_FMonitoredElement()->Get_NTerms())
		{
			DoErrorMsg(String("ESPVLControl: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Re-specify terminal no.", 371);
		}
		else

               // Sets name of i-th terminal's connected bus in ESPVLControl's buslist
		{
			SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
		}
	}
	else
	DoSimpleMsg(String("Monitored Element in ESPVLControl.") + get_Name()
	           + " does not exist:\""
	           + ElementName
	           + "\"", 372);
}

void TESPVLControlObj::MakePosSequence(int ActorID)
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

void TESPVLControlObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}






/*--------------------------------------------------------------------------*/

void TESPVLControlObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TESPVLControlObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TESPVLControlObj::DumpProperties(TTextRec& f, bool Complete)
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

void TESPVLControlObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{


        /*Do Nothing*/
}

/*--------------------------------------------------------------------------*/

void TESPVLControlObj::sample(int ActorID)
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
	if(FLocalControlPointerList->get_myNumList() == 0)
		MakeLocalControlList();
	if(FLocalControlListSize > 0)

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
			for(stop = FLocalControlListSize, i = 1; i <= stop; i++)
			{
				Gen = (TGeneratorObj*) FLocalControlPointerList->Get(i);
              // compute new dispatch value for this generator ...
				GenkW = max(1.0, (Gen->kWBase + PDiff * ((FLocalControlWeights)[i - 1] / TotalWeight)));
				if(GenkW != Gen->kWBase)
				{
					Gen->kWBase = GenkW;
					GenkWChanged = true;
				}
			}
		}
      /*
       If Abs(QDiff) > HalfkWBand Then Begin // Redispatch Generators
          // QDiff is kvar needed to get back into band
          For i := 1 to FLocalControlListSize Do Begin
              Gen := FLocalControlPointerList.Get(i);
              // compute new dispatch value for this generator ...
              Genkvar := Max(0.0, (Gen.kvarBase + QDiff *(FWeights^[i]/TotalWeight)));
              If Genkvar <> Gen.kvarBase Then Begin
                  Gen.kvarBase := Genkvar;
                  Genkvarchanged := TRUE;
              End;
          End;
       End;

       If GenkWChanged or Genkvarchanged Then  // Only push onto controlqueue if there has been a change
          With ActiveCircuit, ActiveCircuit.Solution Do Begin
            LoadsNeedUpdating := TRUE; // Force recalc of power parms
            // Push present time onto control queue to force re solve at new dispatch value
            ControlQueue.Push(DynaVars.intHour, DynaVars.t, 0, 0, Self);
          End;
      */

       /*Else just continue*/
	}
}

void TESPVLControlObj::InitPropertyValues(int ArrayOffset)
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

bool TESPVLControlObj::MakeLocalControlList()
{
	bool result = false;
	TESPVLControlObj* pESPVLControl = nullptr;
	int i = 0;
	result = false;
	if(fType == 1)    // only for System controller
	{
		int stop = 0;
		if(FLocalControlListSize > 0)    // Name list is defined - Use it
		{
			int stop = 0;
			for(stop = FLocalControlListSize, i = 1; i <= stop; i++)
			{
				pESPVLControl = ((TESPVLControlObj*) ESPVLControlClass->Find(String( ( *FLocalControlNameList )[i - 1])));
				if(ASSIGNED(pESPVLControl) && pESPVLControl->Get_Enabled())
					FLocalControlPointerList->Set_New(pESPVLControl);
			}
		}
		else

         /*Search through the entire circuit for enabled generators and add them to the list*/
		{
			int stop = 0;
			for(stop = ESPVLControlClass->Get_ElementCount(), i = 1; i <= stop; i++)
			{
				pESPVLControl = (TESPVLControlObj*) ESPVLControlClass->ElementList.Get(i);
				if(pESPVLControl->Get_Enabled())
					FLocalControlPointerList->Set_New(pESPVLControl);
			}

         /*Allocate uniform weights*/
			FLocalControlListSize = FLocalControlPointerList->get_myNumList();
			FLocalControlWeights = (pDoubleArray)realloc(FLocalControlWeights, sizeof(double) * FLocalControlListSize);
			for(stop = FLocalControlListSize, i = 1; i <= stop; i++)
			{
				(FLocalControlWeights)[i - 1] = 1.0;
			}
		}

       // Add up total weights    ??????
		TotalWeight = 0.0;
		for(stop = FLocalControlListSize, i = 1; i <= stop; i++)
		{
			TotalWeight = TotalWeight + (FLocalControlWeights)[i - 1];
		}
		if(FLocalControlPointerList->get_myNumList() > 0)
			result = true;
	}
	return result;
}

void TESPVLControlObj::Reset(int ActorID)
{

  // inherited;
}




}  // namespace ESPVLControl





