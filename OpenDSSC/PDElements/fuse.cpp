
#pragma hdrstop

#include "fuse.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Ucmatrix.h"
#include "mathutil.h"
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
using namespace ParserDel;
using namespace System;
using namespace TCC_Curve;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace fuse
{

TFuseObj::TFuseObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TFuseObj::TFuseObj(String ClassName) : inherited(ClassName) {}
TFuseObj::TFuseObj() {}


TFuseObj* ActiveFuseObj = nullptr;
TFuse* FuseClass = nullptr;
const int NumPropsThisClass = 10;
TDSSClass* TCC_CurveClass = nullptr;

/*General Module Function*/

TTCC_CurveObj* GetTccCurve(const String CurveName)
{
	TTCC_CurveObj* result = nullptr;
	result = ((TTCC_CurveObj*) TCC_CurveClass->Find(CurveName));
	if(result == nullptr)
		DoSimpleMsg(String("TCC Curve object: \"") + CurveName + "\" not found.", 401);
	return result;
}


/*--------------------------------------------------------------------------*/  // Creates superstructure for all Fuse objects

TFuse::TFuse()
{
	;
	Class_Name = "Fuse";
	DSSClassType = DSSClassType + FUSE_CONTROL;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	TCC_CurveClass = (TDSSClass*) GetDSSClassPtr("TCC_Curve");
	FuseClass = this;
}

/*--------------------------------------------------------------------------*/

TFuse::~TFuse()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TFuse::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "MonitoredObj";
	PropertyName[2 - 1] = "MonitoredTerm";
	PropertyName[3 - 1] = "SwitchedObj";
	PropertyName[4 - 1] = "SwitchedTerm";
	PropertyName[5 - 1] = "FuseCurve";
	PropertyName[6 - 1] = "RatedCurrent";
	PropertyName[7 - 1] = "Delay";
	PropertyName[8 - 1] = "Action";
	PropertyName[9 - 1] = "Normal";
	PropertyName[10 - 1] = "State";
	PropertyHelp[1 - 1] = "Full object name of the circuit element, typically a line, transformer, load, or generator, "
	           "to which the Fuse is connected."
	           " This is the \"monitored\" element. "
	           "There is no default; must be specified.";
	PropertyHelp[2 - 1] = "Number of the terminal of the circuit element to which the Fuse is connected. "
	           "1 or 2, typically.  Default is 1.";
	PropertyHelp[3 - 1] = "Name of circuit element switch that the Fuse controls. "
	           "Specify the full object name."
	           "Defaults to the same as the Monitored element. "
	           "This is the \"controlled\" element.";
	PropertyHelp[4 - 1] = "Number of the terminal of the controlled element in which the switch is controlled by the Fuse. "
	           "1 or 2, typically.  Default is 1.  Assumes all phases of the element have a fuse of this type.";
	PropertyHelp[5 - 1] = "Name of the TCC Curve object that determines the fuse blowing.  Must have been previously defined as a TCC_Curve object."
	           " Default is \"Tlink\". "
	           "Multiplying the current values in the curve by the \"RatedCurrent\" value gives the actual current.";
	PropertyHelp[6 - 1] = "Multiplier or actual phase amps for the phase TCC curve.  Defaults to 1.0.";
	PropertyHelp[7 - 1] = "Fixed delay time (sec) added to Fuse blowing time determined from the TCC curve. Default is 0.0. Used to represent fuse clearing time or any other delay.";
	PropertyHelp[8 - 1] = "DEPRECATED. See \"State\" property.";
	PropertyHelp[9 - 1] = "ARRAY of strings {Open | Closed} representing the Normal state of the fuse in each phase of the controlled element. "
	           "The fuse reverts to this state for reset, change of mode, etc. "
	           "Defaults to \"State\" if not specifically declared.";
	PropertyHelp[10 - 1] = "ARRAY of strings {Open | Closed} representing the Actual state of the fuse in each phase of the controlled element. "
	           "Upon setting, immediately forces state of fuse(s). Simulates manual control on Fuse. Defaults to Closed for all phases.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TFuse::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new Fuse and add it to Fuse class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TFuseObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}
/*--------------------------------------------------------------------------*/


/*--------------------------------------------------------------------------*/

int TFuse::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int DevIndex = 0;
	int i = 0;

  // continue parsing WITH contents of Parser
	ActiveFuseObj = (TFuseObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveFuseObj);
	result = 0;
	/*# with ActiveFuseObj do */
	{
		auto with0 = ActiveFuseObj;
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
				( (TDSSObject*) with0 )->Set_PropertyValue(ParamPointer,Param);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + ( (TDSSObject*) with0 )->get_Name()
	           + "\"", 402);
				break;
				case 	1:
				with0->MonitoredElementName = LowerCase(Param);
				break;
				case 	2:
				with0->MonitoredElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->ElementName = LowerCase(Param);
				break;
				case 	4:
				with0->ElementTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	5:
				with0->FuseCurve = GetTccCurve(Param);
				break;
				case 	6:
				with0->RatedCurrent = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				with0->DelayTime = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
				{
					with0->InterpretFuseState(ActorID, Param, ParamName);  // set the normal state
					if(!with0->NormalStateSet)
						with0->NormalStateSet = true;
				}
				break;
				case 	8: case 10:
				with0->InterpretFuseState(ActorID, Param, ParamName);
				break;  // set the present state

           // Inherited parameters
				default:
				ClassEdit(ActiveFuseObj, ParamPointer - NumPropsThisClass);
				break;
			}

         /*Supplemental Actions*/
			switch(ParamPointer)
			{
				case 	1:
              /*Default the controlled element to the monitored element*/
				with0->ElementName = with0->MonitoredElementName;
				break;
				case 	2:
				with0->ElementTerminal = with0->MonitoredElementTerminal;
				break;
				case 	10:
				{
					int stop = 0;
					for(stop = ( (TDSSCktElement*) with0 )->Fnphases, i = 1; i <= stop; i++)
					{
						if(!with0->NormalStateSet)
							(*with0->FNormalState)[i - 1] = (*with0->FPresentState)[i - 1];
					}
					with0->NormalStateSet = true;   // normal state will default to state only the 1st state is specified.
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

int TFuse::MakeLike(const String FuseName)
{
	int result = 0;
	TFuseObj* OtherFuse = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Fuse name in the present collection*/
	OtherFuse = ((TFuseObj*) Find(FuseName));
	if(OtherFuse != nullptr)
		/*# with ActiveFuseObj do */
		{
			auto with0 = ActiveFuseObj;
			int stop = 0;
			( (TDSSCktElement*) with0 )->Set_NPhases(( (TDSSCktElement*) OtherFuse )->Fnphases);
			( (TDSSCktElement*) with0 )->Set_Nconds(( (TDSSCktElement*) OtherFuse )->Fnconds); // Force Reallocation of terminal stuff
			(with0)->ElementName = OtherFuse->ElementName;
			(with0)->ElementTerminal = OtherFuse->ElementTerminal;
			(with0)->Set_ControlledElement(OtherFuse->get_FControlledElement());  // Pointer to target circuit element
			(with0)->MonitoredElement = OtherFuse->MonitoredElement;  // Pointer to target circuit element
			(with0)->MonitoredElementName = OtherFuse->MonitoredElementName;  // Pointer to target circuit element
			(with0)->MonitoredElementTerminal = OtherFuse->MonitoredElementTerminal;  // Pointer to target circuit element
			(with0)->FuseCurve = OtherFuse->FuseCurve;
			(with0)->RatedCurrent = OtherFuse->RatedCurrent;

        // can't copy action handles
			for(stop = min(FUSEMAXDIM, with0->get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
			{
				(*with0->FPresentState)[i - 1] = (*OtherFuse->FPresentState)[i - 1];
				(*with0->FNormalState)[i - 1] = (*OtherFuse->FNormalState)[i - 1];
			}
			with0->CondOffset = OtherFuse->CondOffset;
			for(stop = ( (TDSSObject*) with0 )->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				( (TDSSObject*) with0 )->Set_PropertyValue(i,( (TDSSObject*) OtherFuse )->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in Fuse MakeLike: \"") + FuseName + "\" Not Found.", 403);
	return result;
}




/*==========================================================================*/
/*                    TFuseObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TFuseObj::TFuseObj(TDSSClass* ParClass, const String FuseName)
 : inherited(ParClass),
			MonitoredElement(nullptr),
			FPresentState(nullptr),
			FNormalState(nullptr),
			CondOffset(0),
			cBuffer(nullptr),
			NormalStateSet(false),
			FuseCurve(nullptr),
			RatedCurrent(0.0),
			DelayTime(0.0),
			MonitoredElementTerminal(0)
{
	int i = 0;
	int stop = 0;
	Set_Name(LowerCase(FuseName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	ElementName = "";
	Set_ControlledElement(nullptr);
	ElementTerminal = 1;
	MonitoredElementName = "";
	MonitoredElementTerminal = 1;
	FuseCurve = GetTccCurve("tlink");
	RatedCurrent = 1.0;
	FPresentState = nullptr;
	FNormalState = nullptr;

     // Reallocate arrays  (Must be initialized to nil for first call)
	FPresentState = (pStateArray) realloc(FPresentState, sizeof((*FPresentState)[1 - 1]) * Fnphases);
	FNormalState = (pStateArray) realloc(FNormalState, sizeof((*FNormalState)[1 - 1]) * Fnphases);
	for(stop = min(FUSEMAXDIM, Fnphases), i = 1; i <= stop; i++)
	{
		(*FPresentState)[i - 1] = CTRL_CLOSE;
		(*FNormalState)[i - 1] = CTRL_CLOSE;  // default to present state;
		ReadyToBlow[i - 1] = false;
		hAction[i - 1] = 0;
	}
	NormalStateSet = false;
	cBuffer = nullptr; // Complex buffer
	DSSObjType = ParClass->DSSClassType; //cap_CONTROL;
	InitPropertyValues(0);

   //  RecalcElementData;
}

TFuseObj::~TFuseObj()
{
	MonitoredElementName = "";
	FPresentState = (pStateArray) realloc(FPresentState, 0);
	FNormalState = (pStateArray) realloc(FNormalState, 0);
	cBuffer = (pComplexArray) realloc(cBuffer, 0);
	// inherited::Destroy();
}


/*--------------------------------------------------------------------------*/

void TFuseObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	int i = 0;
	DevIndex = GetCktElementIndex(MonitoredElementName); // Global function
	if(DevIndex > 0)
	{
		MonitoredElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		Set_NPhases(MonitoredElement->Get_NPhases());       // Force number of phases to be same
		if(Fnphases > FUSEMAXDIM)
			DoSimpleMsg(String("Warning: Fuse ") + this->get_Name()
	           + ": Number of phases > Max fuse dimension.", 404);
		if(MonitoredElementTerminal > MonitoredElement->Get_NTerms())
		{
			DoErrorMsg(String("Fuse: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Re-specify terminal no.", 404);
		}
		else

               // Sets name of i-th terminal's connected bus in Fuse's buslist
		{
			SetBus(1, MonitoredElement->GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
			cBuffer = (pComplexArray) realloc(cBuffer, sizeof((cBuffer)[1 - 1]) * MonitoredElement->Yorder);
			CondOffset = (MonitoredElementTerminal - 1) * MonitoredElement->Get_NConds(); // for speedy sampling
		}
	}

/*Check for existence of Controlled Element*/

         // If previously assigned, reset HasOCPDevice flag in case this is a move
	if(ASSIGNED(get_FControlledElement()))
		get_FControlledElement()->HasOCPDevice = false;
	DevIndex = GetCktElementIndex(ElementName); // Global function
	if(DevIndex > 0)  // Both CktElement and monitored element must already exist
	{
		int stop = 0;
		Set_ControlledElement(((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex)));
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Make the 1 st terminal active
		if(Get_Enabled())
			get_FControlledElement()->HasOCPDevice = true;  // For Reliability calcs

               // Open/Close State of controlled element based on state assigned to the control
		for(stop = min(FUSEMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			if((*FPresentState)[i - 1] == CTRL_OPEN)
			{
				get_FControlledElement()->Set_ConductorClosed(i, ActorID, false);
			}
			else
			{
				get_FControlledElement()->Set_ConductorClosed(i, ActorID, true);
			}
		}
		for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
		{
			hAction[i - 1] = 0;
		}
		for(stop = min(FUSEMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			ReadyToBlow[i - 1] = false;
		}
	}
	else
	{
		Set_ControlledElement(nullptr);   // element not found
		DoErrorMsg(String("Fuse: \"") + this->get_Name() + "\"", String("CktElement Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 405);
	}
}

/*--------------------------------------------------------------------------*/

void TFuseObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}

/*--------------------------------------------------------------------------*/

void TFuseObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}
/*--------------------------------------------------------------------------*/

void TFuseObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/
// Do what we're instructed by the control queue
// Theoretically, there shouldn't be anything on the queue unless we have to do something
/*Only legal action is to open one phase*/

void TFuseObj::DoPendingAction(int Phs, int ProxyHdl, int ActorID)
{
	if(Phs <= FUSEMAXDIM)
		/*# with ControlledElement do */
		{
			auto with0 = get_FControlledElement();
			get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal of CktElement to terminal 1
			switch((*FPresentState)[Phs - 1])
			{
				case 	CTRL_CLOSE:
				if(ReadyToBlow[Phs - 1])   // ignore if we became disarmed in meantime
				{
					get_FControlledElement()->Set_ConductorClosed(Phs, ActorID, false);   // Open phases of active terminal
					AppendToEventLog(String("Fuse.") + this->get_Name(), String("Phase ") + IntToStr(Phs) + " Blown", ActorID);
					hAction[Phs - 1] = 0;
				}
				break;
            /*Do Nothing */
				default:
				  ;
				break;
			}
		}
}

/*--------------------------------------------------------------------------*/

void TFuseObj::InterpretFuseState(int ActorID, const String Param, const String property_name)
{
	int i = 0;
	String DataStr1;
	String DataStr2;
	if(LowerCase( property_name )[0] == 'a') // action (deprecated)  Will be removed
	{
		int stop = 0;
		for(stop = FUSEMAXDIM, i = 1; i <= stop; i++)
		{
			switch(LowerCase(Param)[0])
			{
				case 	L'o':
				set_States(i, CTRL_OPEN);
				break;
				case 	L'c':
				set_States(i, CTRL_CLOSE);
				break;
				default:
				  ;
				break;
			}
		}
	}
	else
	{
		AuxParser[ActorID]->SetCmdString(Param);  // Load up Parser
		DataStr1 = AuxParser[ActorID]->GetNextParam();  // ignore
		DataStr2 = AuxParser[ActorID]->MakeString_();
		i = 1;
		while((DataStr2.size() > 0) && (i < FUSEMAXDIM))
		{
			if(LowerCase( property_name )[0] == 's')  // state
			{
				switch(LowerCase(DataStr2)[0])
				{
					case 	L'o':
					set_States(i, CTRL_OPEN);
					break;
					case 	L'c':
					set_States(i, CTRL_CLOSE);
					break;
					default:
					  ;
					break;
				}
			}
			else
 // 'normal'
			{
				switch(LowerCase(DataStr2)[0])
				{
					case 	L'o':
					set_NormalStates(i, CTRL_OPEN);
					break;
					case 	L'c':
					set_NormalStates(i, CTRL_CLOSE);
					break;
					default:
					  ;
					break;
				}
			}
			DataStr1 = AuxParser[ActorID]->GetNextParam();  // ignore
			DataStr2 = AuxParser[ActorID]->MakeString_();
			++i;
		}
	}
}

/*--------------------------------------------------------------------------*/

void TFuseObj::sample(int ActorID)
{
	int i = 0;
	double cmag = 0.0;
	double TripTime = 0.0;
	get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);
	MonitoredElement->GetCurrents(cBuffer, ActorID);
	/*# with MonitoredElement do */
	{
		auto with0 = MonitoredElement;
		int stop = 0;
		for(stop = min(FUSEMAXDIM, MonitoredElement->Get_NPhases()), i = 1; i <= stop; i++)
		{
			if(get_FControlledElement()->Get_ConductorClosed(i, ActorID))
				(*FPresentState)[i - 1] = CTRL_CLOSE;
			else
				(*FPresentState)[i - 1] = CTRL_OPEN;
			if((*FPresentState)[i - 1] == CTRL_CLOSE)
			{
				TripTime = -1.0;

               /*Check Phase Trip, if any*/
				if(FuseCurve != nullptr)
				{
					cmag = cabs((cBuffer)[i - 1]);
					TripTime = FuseCurve->GetTCCTime(cmag / RatedCurrent);
				}
				if(TripTime > 0.0)
				{
					if(!ReadyToBlow[i - 1])
						/*# with ActiveCircuit[ActorID] do */
						{
							  // Then arm for an open operation
						auto with0 = ActiveCircuit[ActorID];
							hAction[i - 1] = ActiveCircuit[ActorID]->ControlQueue.Push(with0->Solution->DynaVars.intHour, 
								with0->Solution->DynaVars.T + TripTime + DelayTime, (EControlAction) i, 0, this, ActorID);
							ReadyToBlow[i - 1] = true;
						} /*With*/
				}
				else
				{
					if(ReadyToBlow[i - 1])  //  Current has dropped below pickup and it hasn't blown yet
					{
						ActiveCircuit[ActorID]->ControlQueue.Delete(hAction[i - 1], ActorID);  // Delete the fuse blow action
						ReadyToBlow[i - 1] = false;
					}
				}
			}  /*IF PresentState=CLOSE*/
		}
	} /*With*/
}

void TFuseObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	TDSSCktElement::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, "="); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
		WriteLn(f);
}

String TFuseObj::GetPropertyValue(int Index)
{
	String result;
	int i = 0;
	switch(Index)
	{
		case 9: case 10:
		result = "[";
		break;
		default:
		result = "";
		break;
	}
	switch(Index)
	{
		case 	10:
		if(get_FControlledElement() != nullptr)  // Special cases
		{
			int stop = 0;
			for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
			{
				switch((*FPresentState)[i - 1])
				{
					case 	CTRL_OPEN:
					result = result + "open" + ", ";
					break;
                      /*CTRL_CLOSE:*/
					default:
					result = result + "closed" + ", ";
					break;
				}
			}
		}
		break;
		case 	9:
		if(get_FControlledElement() != nullptr)
		{
			int stop = 0;
			for(stop = get_FControlledElement()->Get_NPhases(), i = 1; i <= stop; i++)
			{
				switch((*FNormalState)[i - 1])
				{
					case 	CTRL_OPEN:
					result = result + "open" + ", ";
					break;
                      /*CTRL_CLOSE:*/
					default:
					result = result + "closed" + ", ";
					break;
				}
			}
		}
		break;
		default:
		result = TDSSCktElement::GetPropertyValue(Index);
		break;
	}
	switch(Index)
	{
		case 9: case 10:
		result = result + "]";
		break;
		default:
		  ;
		break;
	}
	return result;
}

void TFuseObj::Reset(int ActorID)
{
	int i = 0;
	if(get_FControlledElement() != nullptr)
	{
		int stop = 0;
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
		for(stop = min(FUSEMAXDIM, get_FControlledElement()->Get_NPhases()), i = 1; i <= stop; i++)
		{
			(*FPresentState)[i - 1] = (*FNormalState)[i - 1];  // reset to normal state
			ReadyToBlow[i - 1] = false;
			hAction[i - 1] = 0;
			switch((*FNormalState)[i - 1])
			{
				case 	CTRL_OPEN:
				get_FControlledElement()->Set_ConductorClosed(i, ActiveActor, false);
				break;
            /*CTRL_CLOSE:*/
				default:
				get_FControlledElement()->Set_ConductorClosed(i, ActiveActor, true);
				break;
			}
		}
	}
}

EControlAction TFuseObj::get_States(int Idx)
{
	EControlAction result;
	if(get_FControlledElement() != nullptr)
	{
		get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
		if(get_FControlledElement()->Get_ConductorClosed(Idx, ActiveActor))
		{
            /*TRUE:*/
			(*FPresentState)[Idx - 1] = CTRL_CLOSE;
		} else {
			/* FALSE */
			(*FPresentState)[Idx - 1] = CTRL_OPEN;
		}
	}
	result = (*FPresentState)[Idx - 1];
	return result;
}

void TFuseObj::set_States(int Idx, const EControlAction Value)
{
	if(get_States(Idx) != Value)
	{
		if(get_FControlledElement() != nullptr)
		{
			get_FControlledElement()->Set_ActiveTerminal(ElementTerminal);  // Set active terminal
			switch(Value)
			{
				case 	CTRL_OPEN:
				get_FControlledElement()->Set_ConductorClosed(Idx, ActiveActor, false);
				break;
                /*CTRL_CLOSE:*/
				default:
				get_FControlledElement()->Set_ConductorClosed(Idx, ActiveActor, true);
				break;
			}
		}
		(*FPresentState)[Idx - 1] = Value;
	}
}

EControlAction TFuseObj::get_NormalStates(int Idx)
{
	EControlAction result;
	result = (*FNormalState)[Idx - 1];
	return result;
}

void TFuseObj::set_NormalStates(int Idx, const EControlAction Value)
{
	if((*FNormalState)[Idx - 1] != Value)
	{
		(*FNormalState)[Idx - 1] = Value;
	}
}

void TFuseObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,""); //'element';
	Set_PropertyValue(2,"1"); //'terminal';
	Set_PropertyValue(3,"");
	Set_PropertyValue(4,"1"); //'terminal';
	Set_PropertyValue(5,"Tlink");
	Set_PropertyValue(6,"1.0");
	Set_PropertyValue(7,"0");
	Set_PropertyValue(8,"");  // action
	Set_PropertyValue(9,"[close, close, close]");  // normal
	Set_PropertyValue(10,"[close,close,close]");  // state
	TDSSCktElement::InitPropertyValues(NumPropsThisClass);
}




}  // namespace fuse

