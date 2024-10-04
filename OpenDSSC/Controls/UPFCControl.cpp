
#pragma hdrstop

#include "UPFCControl.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "UPFC.h"
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
using namespace ParserDel;
using namespace System;
using namespace UPFC;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace UPFCControl
{

TUPFCControlObj::TUPFCControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TUPFCControlObj::TUPFCControlObj(String ClassName) : inherited(ClassName) {}
TUPFCControlObj::TUPFCControlObj() {}


TUPFCControlObj* ActiveUPFCControlObj = nullptr;
const int NumPropsThisClass = 1;


/*--------------------------------------------------------------------------*/  // Creates superstructure for all UPFCControl objects

TUPFCControl::TUPFCControl()
{
	;
	Class_Name = "UPFCControl";
	DSSClassType = DSSClassType + UPFC_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

/*--------------------------------------------------------------------------*/

TUPFCControl::~TUPFCControl()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TUPFCControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "UPFCList";
	PropertyHelp[1 - 1] = "The list of all the UPFC devices to be controlled by this controller, "
	           "If left empty, this control will apply for all UPFCs in the model.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TUPFCControl::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new UPFCControl and add it to UPFCControl class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TUPFCControlObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*--------------------------------------------------------------------------*/

int TUPFCControl::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int i = 0;

  // continue parsing WITH contents of Parser
	ActiveUPFCControlObj = (TUPFCControlObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveUPFCControlObj);
	result = 0;
	/*# with ActiveUPFCControlObj do */
	{
		auto with0 = ActiveUPFCControlObj;
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
	           + "\"", 364);
				break;
				case 	1:
				InterpretTStringListArray(Param, with0->FUPFCNameList);
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveUPFCControlObj, ParamPointer - NumPropsThisClass);
				break;
			}
		}
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
	}

  //RecalcElementData(ActorID);
	return result;
}



/*--------------------------------------------------------------------------*/

int TUPFCControl::MakeLike(const String UPFCControlName)
{
	int result = 0;
	TUPFCControlObj* OtherUPFCControl = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this UPFCControl name in the present collection*/
	OtherUPFCControl = ((TUPFCControlObj*) Find(UPFCControlName));
	if(OtherUPFCControl != nullptr)
		/*# with ActiveUPFCControlObj do */
		{
			auto with0 = ActiveUPFCControlObj;
			int stop = 0;
			with0->Set_NPhases(OtherUPFCControl->Fnphases);
			with0->Set_Nconds(OtherUPFCControl->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherUPFCControl->ElementName;
			with0->Set_ControlledElement(OtherUPFCControl->get_FControlledElement());  // Pointer to target circuit element
			with0->Set_MonitoredElement(OtherUPFCControl->get_FMonitoredElement());  // Pointer to target circuit element
			with0->ElementTerminal = OtherUPFCControl->ElementTerminal;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherUPFCControl->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in UPFCControl MakeLike: \"") + UPFCControlName
	           + "\" Not Found.", 370);
	return result;
}




/*==========================================================================*/
/*                    TUPFCControlObj                                           */
/*==========================================================================*/
/*--------------------------------------------------------------------------*/

TUPFCControlObj::TUPFCControlObj(TDSSClass* ParClass, const String UPFCControlName)
 : inherited(ParClass),
			FUPFCNameList(),
			FUPFCList(new PointerList::TPointerList(20)),
			FListSize(0),
			FWeights(nullptr),
			TotalWeight(0.0)
{
	Set_Name(LowerCase(UPFCControlName));
	DSSObjType = ParClass->DSSClassType;
	TotalWeight = 1.0;
	FWeights = nullptr;
}

TUPFCControlObj::~TUPFCControlObj()
{
	ElementName = "";
	// inherited::Destroy();
}

/*--------------------------------------------------------------------------*/

PointerList::TPointerList* TUPFCControlObj::get_FUPFCList()
{
	return FUPFCList;
}

/*--------------------------------------------------------------------------*/

void TUPFCControlObj::set_FUPFCList(PointerList::TPointerList* myPtr)
{
	FUPFCList = myPtr;
}

/*--------------------------------------------------------------------------*/

int TUPFCControlObj::get_FListSize()
{
	return FListSize;
}

/*--------------------------------------------------------------------------*/

void TUPFCControlObj::set_FListSize(int mySize)
{
	FListSize = mySize;
}

/*--------------------------------------------------------------------------*/

void TUPFCControlObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
/*Check for existence of monitored element*/
	DevIndex = GetCktElementIndex(ElementName); // Global function
	if(DevIndex > 0)
	{
		Set_MonitoredElement((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		if(ElementTerminal > get_FMonitoredElement()->Get_NTerms())
		{
			DoErrorMsg(String("UPFCControl: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Re-specify terminal no.", 371);
		}
		else

     // Sets name of i-th terminal's connected bus in UPFCControl's buslist
		{
			SetBus(1, get_FMonitoredElement()->GetBus(ElementTerminal));
		}
	}
	else
	DoSimpleMsg(String("Monitored Element in UPFCControl.") + get_Name()
	           + " does not exist:\""
	           + ElementName
	           + "\"", 372);
}

/*--------------------------------------------------------------------------*/

void TUPFCControlObj::MakePosSequence(int ActorID)
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

void TUPFCControlObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}

/*--------------------------------------------------------------------------*/

void TUPFCControlObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TUPFCControlObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TUPFCControlObj::DumpProperties(TTextRec& f, bool Complete)
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

void TUPFCControlObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	bool Update = false;
	int i = 0;
	TDSSClass* MyClass = nullptr;
	TUPFCObj* myUPFC = nullptr;
	if(FListSize > 0)
	{
		int stop = 0;
		for(stop = FListSize, i = 1; i <= stop; i++)
		{
			myUPFC = (TUPFCObj*) FUPFCList->Get(i);
			myUPFC->UploadCurrents(ActorID);
		}
	}
}

/*--------------------------------------------------------------------------*/

void TUPFCControlObj::sample(int ActorID)
{
	bool Update = false;
	int i = 0;
	TDSSClass* MyClass = nullptr;
	TUPFCObj* myUPFC = nullptr;
  // If list is not define, go make one from all generators in circuit
	if(FUPFCList->get_myNumList() == 0)
		MakeUPFCList();
	Update = false;
	if(FListSize > 0)
	{
		int stop = 0;
		for(stop = FListSize, i = 1; i <= stop; i++)
		{
			myUPFC = (TUPFCObj*) FUPFCList->Get(i);
			Update = Update || myUPFC->CheckStatus(ActorID);
		}
   /*Checks if at least one UPFC needs to be updated*/
		if(Update)
		{
			/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
			{
				
				auto with1 = ActiveCircuit[ActorID]->Solution;
				ActiveCircuit[ActorID]->ControlQueue.Push(with1->DynaVars.intHour, with1->DynaVars.T, (EControlAction) 0, 0, this, ActorID);
			}
		}
	}
}

void TUPFCControlObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"[]");   //'UPFC List';
	inherited::InitPropertyValues(NumPropsThisClass);
}

bool TUPFCControlObj::MakeUPFCList()
{
	bool result = false;
	TDSSClass* MyClass = nullptr;
	TUPFCObj* UPFC = nullptr;
	int i = 0;
	int stop = 0;
	result = false;
  // Clears everything
	FUPFCNameList.clear();
	FUPFCList->Clear();
	MyClass = (TDSSClass*) GetDSSClassPtr("upfc");
	if(FListSize > 0)    // Name list is defined - Use it
	{
		int stop = 0;
		for(stop = FListSize, i = 1; i <= stop; i++)
		{
			UPFC = (TUPFCObj*) MyClass->Find(String((FUPFCNameList)[i - 1]));
			if(ASSIGNED(UPFC) && ( (TDSSCktElement*) UPFC )->Get_Enabled())
				FUPFCList->Set_New((void*) UPFC);
		}
	}
	else
  // No list given
	
   /*Search through the entire circuit for enabled generators and add them to the list*/
	{
		int stop = 0;
		for(stop = MyClass->Get_ElementCount(), i = 1; i <= stop; i++)
		{
			UPFC = (TUPFCObj*) MyClass->ElementList.Get(i);
      // Checks if it's enabled
			if(( (TDSSCktElement*) UPFC )->Get_Enabled())
				FUPFCList->Set_New(UPFC);
		}

    /*Allocate uniform weights*/
		FListSize = FUPFCList->get_myNumList();
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
	if(FUPFCList->get_myNumList() > 0)
		result = true;
	return result;
}

void TUPFCControlObj::Reset(int ActorID)
{

  // inherited;
}




}  // namespace UPFCControl





