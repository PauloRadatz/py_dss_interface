
#pragma hdrstop

#include "SwtControl.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Utilities.h"
#include "Solution.h"

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
using namespace ParserDel;
using namespace Solution;
using namespace System;
using namespace Ucomplex;
using namespace Utilities;

namespace SwtControl
{

TSwtControlObj::TSwtControlObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TSwtControlObj::TSwtControlObj(String ClassName) : inherited(ClassName) {}
TSwtControlObj::TSwtControlObj() {}


TSwtControlObj* ActiveSwtControlObj = nullptr;
const int NumPropsThisClass = 8;  // Creates superstructure for all SwtControl objects

TSwtControl::TSwtControl()
{
	;
	Class_Name = "SwtControl";
	DSSClassType = DSSClassType + SWT_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//----------------------------------------------------------------------------------------

TSwtControl::~TSwtControl()
{
	// inherited::Destroy();
}


void TSwtControl::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();   /*see DSSClass*/
	PropertyName[1 - 1] = "SwitchedObj";
	PropertyName[2 - 1] = "SwitchedTerm";
	PropertyName[3 - 1] = "Action";
	PropertyName[4 - 1] = "Lock";
	PropertyName[5 - 1] = "Delay";
	PropertyName[6 - 1] = "Normal";
	PropertyName[7 - 1] = "State";
	PropertyName[8 - 1] = "Reset";
	PropertyHelp[1 - 1] = "Name of circuit element switch that the SwtControl operates. "
	           "Specify the full object class and name.";
	PropertyHelp[2 - 1] = "Terminal number of the controlled element switch. " "1 or 2, typically.  Default is 1.";
	PropertyHelp[3 - 1] = "{Open | Close}  After specified delay time, and if not locked, causes the controlled switch to open or close. ";
	PropertyHelp[4 - 1] = "{Yes | No} Delayed action. Sends CTRL_LOCK or CTRL_UNLOCK message to control queue. "
	           "After delay time, controlled switch is locked in its present open / close state or unlocked. "
	           "Switch will not respond to either manual (Action) or automatic (COM interface) control or internal OpenDSS Reset when locked.";
	PropertyHelp[5 - 1] = "Operating time delay (sec) of the switch. Defaults to 120.";
	PropertyHelp[6 - 1] = "{Open | Closed] Normal state of the switch. If not Locked, the switch reverts to this state for reset, change of mode, etc."
	           " Defaults to first Action or State specified if not specifically declared.";
	PropertyHelp[7 - 1] = "{Open | Closed] Present state of the switch. Upon setting, immediately forces state of switch.";
	PropertyHelp[8 - 1] = "{Yes | No} If Yes, forces Reset of switch to Normal state and removes Lock independently of any internal "
	           "reset command for mode change, etc.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

int TSwtControl::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new SwtControl and add it to SwtControl class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TSwtControlObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

int TSwtControl::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int DevIndex = 0;

  // continue parsing WITH contents of Parser
	ActiveSwtControlObj = (TSwtControlObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveSwtControlObj);
	result = 0;
	/*# with ActiveSwtControlObj do */
	{
		auto with0 = ActiveSwtControlObj;
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
           /*internal SwtControl Property commands*/
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 382);
				break;
				case 	1:
				with0->ElementName = LowerCase(Param);
				break;
				case 	2:
				with0->ElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->InterpretSwitchAction(Param);
				break;
				case 	4:
				with0->set_Flocked(InterpretYesNo(Param));
				break;
				case 	5:
				with0->TimeDelay = Parser[ActorID]->MakeDouble_();
				break;    // set the normal state
				case 	6:
				{
					with0->InterpretSwitchAction(Param);
					with0->set_NormalState(with0->ActionCommand);
				}
				break;    // set the present state
				case 	7:
				{
					with0->InterpretSwitchAction(Param);
					with0->Set_PresentState(with0->ActionCommand);
				}
				break;
				case 	8:
				if(InterpretYesNo(Param))  // force a reset
				{
					with0->set_Flocked(false);
					with0->Reset(ActorID);
					with0->Set_PropertyValue(8,"n");
				}
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveSwtControlObj, ParamPointer - NumPropsThisClass);
				break;
			}

         /*supplemental actions*/
			switch(ParamPointer)
			{
				case 	3:
				if(with0->get_FNormalState() == CTRL_NONE)

             // Default to first action specified for legacy scripts
					with0->set_NormalState(with0->ActionCommand);
				break;
				case 	4:
				if(with0->get_FLocked())
					with0->LockCommand = CTRL_LOCK;
				else
					with0->LockCommand = CTRL_UNLOCK;
				break;
				case 	7:
				{
					if(with0->get_FNormalState() == CTRL_NONE)
						with0->set_NormalState(with0->get_FPresentState());
					DevIndex = GetCktElementIndex(with0->ElementName);   // Set Controlled element
					if(DevIndex > 0)
					{
						with0->Set_ControlledElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
						if(with0->get_FControlledElement() != nullptr)
						{
							with0->get_FControlledElement()->Set_ActiveTerminal(with0->ElementTerminal);
							switch(with0->get_FPresentState())
							{
								case 	CTRL_OPEN:     // Force state
								with0->get_FControlledElement()->Set_ConductorClosed(0, ActorID, false);
								break;
								case 	CTRL_CLOSE:
								with0->get_FControlledElement()->Set_ConductorClosed(0, ActorID, true);
								break;
								default:
								  ;
								break;
							}
						}
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

int TSwtControl::MakeLike(const String SwtControlName)
{
	int result = 0;
	TSwtControlObj* OtherSwtControl = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this SwtControl name in the present collection*/
	OtherSwtControl = ((TSwtControlObj*) Find(SwtControlName));
	if(OtherSwtControl != nullptr)
		/*# with ActiveSwtControlObj do */
		{
			auto with0 = ActiveSwtControlObj;
			int stop = 0;
			with0->Set_NPhases(OtherSwtControl->Fnphases);
			with0->Set_Nconds(OtherSwtControl->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherSwtControl->ElementName;
			with0->ElementTerminal = OtherSwtControl->ElementTerminal;
			with0->Set_ControlledElement(OtherSwtControl->get_FControlledElement());  // Pointer to target circuit element
			with0->TimeDelay = OtherSwtControl->TimeDelay;
			with0->set_Flocked(OtherSwtControl->get_FLocked());
			with0->Set_PresentState(OtherSwtControl->get_FPresentState());
			with0->set_NormalState(OtherSwtControl->get_FNormalState());
			with0->ActionCommand = OtherSwtControl->ActionCommand;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherSwtControl->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in SwtControl MakeLike: \"") + SwtControlName
	           + "\" Not Found.", 383);
	return result;
}

/*==========================================================================*/
/*                    TSwtControlObj                                           */
/*==========================================================================*/

TSwtControlObj::TSwtControlObj(TDSSClass* ParClass, const String SwtControlName)
 : inherited(ParClass),
			FLocked(false),
			Armed(false)
{
	Set_Name(LowerCase(SwtControlName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors in base class
	ElementName = "";
	Set_ControlledElement(nullptr);
	ElementTerminal = 1;
	Set_PresentState(CTRL_CLOSE);  // default to closed
	set_NormalState(CTRL_NONE);   // default to unspecified; set on first setting action or anything
	ActionCommand = get_FPresentState();
	LockCommand = CTRL_NONE;
	set_Flocked(false);
	Armed = false;
	TimeDelay = 120.0; // 2 minutes
	InitPropertyValues(0);
}

TSwtControlObj::~TSwtControlObj()
{
	// inherited::Destroy();
}


void TSwtControlObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	DevIndex = GetCktElementIndex(ElementName);
	if(DevIndex > 0)
	{
		Set_ControlledElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
		Set_NPhases(get_FControlledElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);
		get_FControlledElement()->HasSwtControl = true;  // For Reliability calcs
/*
    if not Locked then
      Case PresentState of
        CTRL_OPEN: ControlledElement.Closed[0] := FALSE;
        CTRL_CLOSE: ControlledElement.Closed[0] := TRUE;
      End;

*/
    // attach controller bus to the switch bus - no space allocated for monitored variables
		SetBus(1, get_FControlledElement()->GetBus(ElementTerminal));
	}
	else
	{
		Set_ControlledElement(nullptr);   // element not found
		DoErrorMsg(String("SwtControl: \"") + this->get_Name() + "\"", String("CktElement Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 387);
	}
}

void TSwtControlObj::MakePosSequence(int ActorID)
{
	if(get_FControlledElement() != nullptr)
	{
		Set_NPhases(get_FControlledElement()->Get_NPhases());
		Set_Nconds(Fnphases);
		SetBus(1, get_FControlledElement()->GetBus(ElementTerminal));
	}
	inherited::MakePosSequence(ActorID);
}

/*--------------------------------------------------------------------------*/

void TSwtControlObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil
}

void TSwtControlObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TSwtControlObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TSwtControlObj::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{
	EControlAction ctrl_code;
	ctrl_code = EControlAction(Code);  // change type
	get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);
	switch(ctrl_code)
	{
		case 	CTRL_LOCK:
		set_Flocked(true);
		break;
		case 	CTRL_UNLOCK:
		set_Flocked(false);
		break;
		default:
		if(!get_FLocked())
		{
			if((Code == ( CTRL_OPEN)) && (get_FPresentState() == CTRL_CLOSE))
			{
				get_FControlledElement()->Set_ConductorClosed(0, ActorID, false); // Open all phases of active terminal
				Set_PresentState(CTRL_OPEN);
				AppendToEventLog(String("SwtControl.") + this->get_Name(), "Opened", ActorID);
			}
			if((Code == ( CTRL_CLOSE)) && (get_FPresentState() == CTRL_OPEN))
			{
				get_FControlledElement()->Set_ConductorClosed(0, ActorID, true);    // Close all phases of active terminal
				Set_PresentState(CTRL_CLOSE);
				AppendToEventLog(String("SwtControl.") + this->get_Name(), "Closed", ActorID);
			}
			Armed = false;  // reset the switch
		}
		break;
	}
}

void TSwtControlObj::InterpretSwitchAction(const String Action)
{
	if(!get_FLocked())
	{
		switch(LowerCase(Action)[0])
		{
			case 	L'o':
			ActionCommand = CTRL_OPEN;
			break;    // default is closed
			default:
			ActionCommand = CTRL_CLOSE;
			break;
		}

    /*   Changed to delayed action
    if ControlledElement <> nil then begin
      ControlledElement.ActiveTerminalIdx := ElementTerminal;
      Case PresentState of
        CTRL_OPEN: ControlledElement.Closed[0] := FALSE;
        CTRL_CLOSE: ControlledElement.Closed[0] := TRUE;
      End;
    End;
    */
	}
}

//-------------------------------------------------------------------------------------

EControlAction TSwtControlObj::get_FNormalState()
{
	return FNormalState;
}

//-------------------------------------------------------------------------------------

EControlAction TSwtControlObj::get_FPresentState()
{
	return FPresentState;
}

//-------------------------------------------------------------------------------------

bool TSwtControlObj::get_FLocked()
{
	return FLocked;
}

//-------------------------------------------------------------------------------------

EControlAction TSwtControlObj::get_ActionCommand()
{
	return ActionCommand;
}

//-------------------------------------------------------------------------------------

void TSwtControlObj::sample(int ActorID)
{


// push on the Lock command if any at the present time delay
	if(LockCommand != CTRL_NONE)
		/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
		{
			
			auto with1 = ActiveCircuit[ActorID]->Solution;
			ActiveCircuit[ActorID]->ControlQueue.Push(with1->DynaVars.intHour, with1->DynaVars.T + TimeDelay, LockCommand, 0, this, ActorID);
			LockCommand = CTRL_NONE;  // reset the lock command for next time
		}
	if((ActionCommand != get_FPresentState()) && !Armed)
		/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
		{
			
			auto with3 = ActiveCircuit[ActorID]->Solution;   // we need to operate this switch
			ActiveCircuit[ActorID]->ControlQueue.Push(with3->DynaVars.intHour, with3->DynaVars.T + TimeDelay, ActionCommand, 0, this, ActorID);
			Armed = true;
		}
  /*ControlledElement.ActiveTerminalIdx := ElementTerminal;
  IF  ControlledElement.Closed [0]      // Check state of phases of active terminal
  THEN PresentState := CTRL_CLOSE
  ELSE PresentState := CTRL_OPEN; */
}

void TSwtControlObj::set_Flocked(bool Value)
{
	FLocked = Value;
}

void TSwtControlObj::Set_LastAction(const EControlAction Value)
{
	ActionCommand = Value;
}

void TSwtControlObj::set_NormalState(const EControlAction Value)
{
	FNormalState = Value;
}

void TSwtControlObj::Set_PresentState(const EControlAction Value)
{
	FPresentState = Value;
}

void TSwtControlObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue((with0->PropertyIdxMap)[i - 1])); }
		}
	}
	if(Complete)
		WriteLn(f);
}

String TSwtControlObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	1:
		result = ElementName;
		break;
		case 	2:
		result = Format("%d", ElementTerminal);
		break;
		case 	3:
		switch(ActionCommand)
		{
			case 	CTRL_OPEN:
			result = "open";
			break;
          /*CTRL_CLOSE:*/
			default:
			result = "close";
			break;
		}
		break;
		case 	4:
		if(get_FLocked())
			result = "Yes";
		else
			result = "No";
		break;
		case 	5:
		result = Format("%-.7g", TimeDelay);
		break;
		case 	6:
		switch(FNormalState)
		{
			case 	CTRL_OPEN:
			result = "open";
			break;
          /*CTRL_CLOSE:*/
			default:
			result = "closed";
			break;
		}
		break;
		case 	7:
		{
			get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);
			if(get_FControlledElement()->Get_ConductorClosed(0, ActiveActor))
				result = "Closed";
			else
				result = "open";
		}
		break;
		case 	8:
		result = "n";
		break;  // Always no; yes is executed immediately
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TSwtControlObj::Reset(int ActorID)
{
	if(!get_FLocked())
	{
		Set_PresentState(get_FNormalState());
		ActionCommand = get_FPresentState();
		Armed = false;
		if(get_FControlledElement() != nullptr)
		{
			get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
			switch(FNormalState)
			{
				case 	CTRL_OPEN:
				get_FControlledElement()->Set_ConductorClosed(0, ActiveActor, false);
				break;
            /*CTRL_CLOSE:*/
				default:
				get_FControlledElement()->Set_ConductorClosed(0, ActiveActor, true);  // Close all phases of active terminal
				break;
			}
		}
	}
}

void TSwtControlObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,""); //'element';
	Set_PropertyValue(2,"1"); //'terminal';
	Set_PropertyValue(3,"c");
	Set_PropertyValue(4,"n");
	Set_PropertyValue(5,"120.0");
	Set_PropertyValue(6,"c");
	Set_PropertyValue(7,"c");
	Set_PropertyValue(8,"n");
	inherited::InitPropertyValues(NumPropsThisClass);
}




}  // namespace SwtControl





