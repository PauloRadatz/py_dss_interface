
#pragma hdrstop

#include "Capacitor.h"

#include "DSSGlobals.h"
#include "Utilities.h"

using namespace std;
using namespace Arraydef;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace PDClass;
using namespace PDELement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace Utilities;

namespace Capacitor
{

TCapacitorObj::TCapacitorObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TCapacitorObj::TCapacitorObj(String ClassName) : inherited(ClassName) {}
TCapacitorObj::TCapacitorObj() {}


TCapacitorObj* ActiveCapacitorObj = nullptr;
TCapacitor* CapacitorClass = nullptr;
const int NumPropsThisClass = 13;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Capacitor objects

TCapacitor::TCapacitor()
{
	;
	Class_Name = "Capacitor";
	DSSClassType = DSSClassType + CAP_ELEMENT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	CapacitorClass = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TCapacitor::~TCapacitor()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TCapacitor::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "bus1";
	PropertyName[2 - 1] = "bus2";
	PropertyName[3 - 1] = "phases";
	PropertyName[4 - 1] = "kvar";
	PropertyName[5 - 1] = "kv";
	PropertyName[6 - 1] = "conn";
	PropertyName[7 - 1] = "cmatrix";
	PropertyName[8 - 1] = "cuf";
	PropertyName[9 - 1] = "R";
	PropertyName[10 - 1] = "XL";
	PropertyName[11 - 1] = "Harm";
	PropertyName[12 - 1] = "Numsteps";
	PropertyName[13 - 1] = "states";

     // define Property help values
	PropertyHelp[1 - 1] = String("Name of first bus of 2-terminal capacitor. Examples:") + CRLF
	           + "bus1=busname"
	           + CRLF
	           + "bus1=busname.1.2.3"
	           + CRLF
	           + CRLF
	           + "If only one bus specified, Bus2 will default to this bus, Node 0, "
	           + "and the capacitor will be a Yg shunt bank.";
	PropertyHelp[2 - 1] = String("Name of 2nd bus. Defaults to all phases connected " "to first bus, node 0, (Shunt Wye Connection) " "except when Bus2 explicitly specified. ") + CRLF
	           + CRLF
	           + "Not necessary to specify for delta (LL) connection.";
	PropertyHelp[3 - 1] = "Number of phases.";
	PropertyHelp[4 - 1] = "Total kvar, if one step, or ARRAY of kvar ratings for each step.  Evenly divided among phases. See rules for NUMSTEPS.";
	PropertyHelp[5 - 1] = "For 2, 3-phase, kV phase-phase. Otherwise specify actual can rating.";
	PropertyHelp[6 - 1] = "={wye | delta |LN |LL}  Default is wye, which is equivalent to LN";
	PropertyHelp[7 - 1] = String("Nodal cap. matrix, lower triangle, microfarads, of the following form:") + CRLF
	           + CRLF
	           + "cmatrix=\"c11 | -c21 c22 | -c31 -c32 c33\""
	           + CRLF
	           + CRLF
	           + "All steps are assumed the same if this property is used.";
	PropertyHelp[8 - 1] = String("ARRAY of Capacitance, each phase, for each step, microfarads.") + CRLF
	           + "See Rules for NumSteps.";
	PropertyHelp[9 - 1] = "ARRAY of series resistance in each phase (line), ohms. Default is 0.0";
	PropertyHelp[10 - 1] = "ARRAY of series inductive reactance(s) in each phase (line) for filter, ohms at base frequency. Use this OR \"h\" property to define filter. Default is 0.0.";
	PropertyHelp[11 - 1] = "ARRAY of harmonics to which each step is tuned. Zero is interpreted as meaning zero reactance (no filter). Default is zero.";
	PropertyHelp[12 - 1] = "Number of steps in this capacitor bank. Default = 1. Forces reallocation of the capacitance, reactor, and states array.  Rules: "
	           "If this property was previously =1, the value in the kvar property is divided equally among the steps. The kvar property "
	           "does not need to be reset if that is accurate.  If the Cuf or Cmatrix property was used previously, all steps are set to the value of the first step. "
	           "The states property is set to all steps on. All filter steps are set to the same harmonic. "
	           "If this property was previously >1, the arrays are reallocated, but no values are altered. You must SUBSEQUENTLY assign all array properties.";
	PropertyHelp[13 - 1] = "ARRAY of integers {1|0} states representing the state of each step (on|off). Defaults to 1 when reallocated (on). "
	           "Capcontrol will modify this array as it turns steps on or off.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

	PropertyHelp[NumPropsThisClass] = PropertyHelp[NumPropsThisClass] + " Defaults to 135 % of per - phase rated current.";
	PropertyHelp[NumPropsThisClass + 1] = PropertyHelp[NumPropsThisClass + 1] + " Defaults to 180 % of per - phase rated current.";
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TCapacitor::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		auto with0 = ActiveCircuit[ActiveActor];
		with0->Set_ActiveCktElement(new TCapacitorObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TCapacitor::DoCmatrix(int ActorID)
{
	int OrderFound = 0;
	int j = 0;
	pDoubleArray MatBuffer = nullptr;
	/*# with ActiveCapacitorObj do */
	{
		auto with0 = ActiveCapacitorObj;
		MatBuffer = new double[ with0->Fnphases * with0->Fnphases ];
		OrderFound = Parser[ActorID]->ParseAsSymMatrix( ( (TDSSCktElement*) with0 )->Fnphases, MatBuffer);
		if(OrderFound > 0)    // Parse was successful
			    /*C*/
			{
				int stop = 0;
				with0->Cmatrix.resize(with0->Fnphases * with0->Fnphases);
				for(stop = ( (TDSSCktElement*) with0 )->Fnphases * ( (TDSSCktElement*) with0 )->Fnphases, j = 1; j <= stop; j++)
				{
					(with0->Cmatrix)[j - 1] = 1.0e-6 * (MatBuffer)[j - 1];
				}
			}
		free(MatBuffer); //# FreeMemory accepts one parameter only;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN

void TCapacitor::InterpretConnection(const String s)
{
	String TestS;
	/*# with ActiveCapacitorObj do */
	{
		auto with0 = ActiveCapacitorObj;
		TestS = LowerCase(s);
		switch(TestS[1 - 1])
		{
			case 	L'y':
			 case L'w':
			with0->Connection = 0;
			break;  /*Wye*/
			case 	L'd':
			with0->Connection = 1;
			break;  /*Delta or line-Line*/
			case 	L'l':
			switch(TestS[2 - 1])
			{
				case 	L'n':
				with0->Connection = 0;
				break;
				case 	L'l':
				with0->Connection = 1;
				break;
				default:
				  ;
				break;
			}
			break;
			default:
			  ;
			break;
		}
		switch(with0->Connection)
		{
			case 	1:
			{
				int		myshift = 0;
				with0->Set_NTerms(1);		// Force reallocation of terminals
				if (with0->Fnphases < 3)
					myshift = 1;

				with0->Set_Nconds(with0->Fnphases + myshift);
			}
			break;  
			case 	0:
			if(with0->Fnterms != 2)
			{
				with0->Set_NTerms(2);
			}
			with0->Set_Nconds(with0->Fnphases);
			break;
			default:
			  ;
			break;
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TCapacitor::CapSetBus1(const String s)
{
	String S2;
	int i = 0;
	int dotpos = 0;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0
	/*# with ActiveCapacitorObj do */
	{
		auto with0 = ActiveCapacitorObj;
		with0->SetBus(1, s);

     // Default Bus2 to zero node of Bus1 unless it is previously defined. (Grounded-Y connection)
		if(!with0->Bus2Defined && (with0->Fnterms == 2))   // Fixed Bus 9-21-19

       // Strip node designations from S
		{
			int stop = 0;
			dotpos = Pos(".", s);
			if(dotpos > 0)
				S2 = s.substr(0, dotpos - 1);
			else
				S2 = s.substr(0, s.size());  // copy up to Dot
			for(stop = with0->Fnphases, i = 1; i <= stop; i++)
			{
				S2 = S2 + ".0";
			}   // append series of ".0"'s
			with0->SetBus(2, S2);    // default setting for Bus2
			with0->IsShunt = true;
		}
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TCapacitor::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int i = 0;
	result = 0;
  // continue parsing with contents of Parser
	ActiveCapacitorObj = (TCapacitorObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveCapacitorObj);  // use property to set this value
	/*# with ActiveCapacitorObj do */
	{
		auto with0 = ActiveCapacitorObj;
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
	           + "\" for Object \"Capacitor."
	           + with0->get_Name()
	           + "\"", 450);
				break;
				case 	1:
				CapSetBus1(Param);
				break;
				case 	2:
				{
					with0->SetBus(2, Param);
					with0->NumTerm = 2;    // Specifies that the capacitor is not connected to ground
				}
				break;/* Numphases := Parser.MakeInteger_()*/
				case 	3:
				;
				break;  // see below
				case 	4:
				with0->FNumSteps = InterpretDblArray(Param, with0->FNumSteps, &(with0->Fkvarrating[0]));
				break;
				case 	5:
				with0->kvrating = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				InterpretConnection(Param);
				break;
				case 	7:
				DoCmatrix(ActorID);
				break;
				case 	8:
				with0->FNumSteps = InterpretDblArray(Param, with0->FNumSteps, &(with0->FC[0]));
				break;
				case 	9:
				with0->FNumSteps = InterpretDblArray(Param, with0->FNumSteps, &(with0->FR[0]));
				break;
				case 	10:
				with0->FNumSteps = InterpretDblArray(Param, with0->FNumSteps, &(with0->FXL[0]));
				break;
				case 	11:
				with0->ProcessHarmonicSpec(Param);
				break;
				case 	12:
				with0->set_NumSteps(Parser[ActorID]->MakeInteger_());
				break;
				case 	13:
				with0->ProcessStatesSpec(Param);
				break;
            // Inherited Property Edits
				default:
				inherited::ClassEdit(ActiveCapacitorObj, ParamPointer - NumPropsThisClass);
				break;
			}

         // Some specials ...
			switch(ParamPointer)
			{
				case 	1:
				{
					with0->Set_PropertyValue(2,with0->GetBus(2));   // this gets modified
					with0->PrpSequence[2 - 1] = 0; // Reset this for save function
				}
				break;
				case 	2:
				if(CompareText(StripExtension(with0->GetBus(1)), StripExtension(with0->GetBus(2))) != 0)
				{
					with0->IsShunt = false;
					with0->Bus2Defined = true;
				}
				break;
				case 	3:
				if(with0->Fnphases != Parser[ActorID]->MakeInteger_())
				{
					with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
					if (with0->Connection == 1)
						with0->Set_Nconds(with0->Fnphases + 1);
					else
						with0->Set_Nconds(with0->Fnphases);  // Force Reallocation of terminal info
					with0->Yorder = with0->Fnterms * with0->Fnconds;
				}
				break;
				case 	4:
				with0->SpecType = 1;
				break;
				case 	7:
				with0->SpecType = 3;
				break;
				case 	8:
				{
					int stop = 0;
					with0->SpecType = 2;
					for(stop = with0->FNumSteps, i = 1; i <= stop; i++)
					{
						(with0->FC)[i - 1] = (with0->FC)[i - 1] * 1.0e-6;
					}
				}
				break;
				case 	10:
				{
					int stop = 0;
					for(stop = with0->FNumSteps, i = 1; i <= stop; i++)
					{
						if((with0->FXL)[i - 1] != 0.0)
						{
							if((with0->FR)[i - 1] == 0.0)
								(with0->FR)[i - 1] = double(Abs((int) (with0->FXL)[i - 1])) / 1000.0;
						}
					}  // put in something so it doesn't fail
					with0->DoHarmonicRecalc = false;  // XL is specified
				}
				break;
				case	(NumPropsThisClass + 1):
					with0->FNormAmpsSpecified = true;
				break;
				case	(NumPropsThisClass + 2):
					with0->FEmergAmpsSpecified = true;
				break;
				default:
				  ;
				break;
			}

         //YPrim invalidation on anything that changes impedance values
			switch(ParamPointer)
			{
				case 3: case 4: case 5: case 6: case 7: case 8:
					with0->Set_YprimInvalid(ActorID,true);
				break;
				case 	12: case 13:
					with0->Set_YprimInvalid(ActorID,true);
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

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TCapacitor::MakeLike(const String CapacitorName)
{
	int result = 0;
	TCapacitorObj* OtherCapacitor = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Capacitor name in the present collection*/
	OtherCapacitor = ((TCapacitorObj*) Find(CapacitorName));
	if(OtherCapacitor != nullptr)
		/*# with ActiveCapacitorObj do */
		{
			auto with0 = ActiveCapacitorObj;
			int stop = 0;
			if((with0)->Fnphases != (OtherCapacitor)->Fnphases)
			{
				(with0)->Set_NPhases((OtherCapacitor)->Fnphases);
				(with0)->Set_Nconds((with0)->Fnphases); // force reallocation of terminals and conductors
				(with0)->Yorder = (with0)->Fnconds * (with0)->Fnterms;
				(with0)->Set_YprimInvalid(ActiveActor,true);
			}
			with0->set_NumSteps(OtherCapacitor->Get_FNumSteps());
			for(stop = with0->FNumSteps, i = 1; i <= stop; i++)
			{
				(with0->FC)[i - 1] = (OtherCapacitor->FC)[i - 1];
				(with0->Fkvarrating)[i - 1] = (OtherCapacitor->Fkvarrating)[i - 1];
				(with0->FR)[i - 1] = (OtherCapacitor->FR)[i - 1];
				(with0->FXL)[i - 1] = (OtherCapacitor->FXL)[i - 1];
				(with0->FXL)[i - 1] = (OtherCapacitor->FXL)[i - 1];
				(with0->FHarm)[i - 1] = (OtherCapacitor->FHarm)[i - 1];
				(with0->FStates)[i - 1] = (OtherCapacitor->FStates)[i - 1];
			}
			with0->kvrating = OtherCapacitor->kvrating;
			with0->Connection = OtherCapacitor->Connection;
			with0->SpecType = OtherCapacitor->SpecType;
			if(OtherCapacitor->Cmatrix.empty())
				with0->Cmatrix.clear();
			else
			{
				int stop = 0;
				with0->Cmatrix.resize(with0->Fnphases * with0->Fnphases);
				for(stop = (with0)->Fnphases * (with0)->Fnphases, i = 1; i <= stop; i++)
				{
					(with0->Cmatrix)[i - 1] = (OtherCapacitor->Cmatrix)[i - 1];
				}
			}
			ClassMakeLike(OtherCapacitor);  // Take care of inherited class properties
			for(stop = (with0)->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				(with0)->Set_PropertyValue(i,(OtherCapacitor)->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Capacitor MakeLike: \"") + CapacitorName
	           + "\" Not Found.", 451);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TCapacitor::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TCapacitor.Init", 452);
	result = 0;
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TCapacitor Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TCapacitorObj::TCapacitorObj(TDSSClass* ParClass, const String CapacitorName)
 : inherited(ParClass),
			Ftotalkvar(0.0),
			kvrating(0.0),
			FNumSteps(0),
			FLastStepInService(0),
			DoHarmonicRecalc(false),
			Bus2Defined(false),
			SpecType(0),
			NumTerm(0),
			Connection(0)
{
	Set_Name(LowerCase(CapacitorName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(2);  // Force allocation of terminals and conductors
	SetBus(2, (GetBus(1) + ".0.0.0"));  // Default to grounded wye
	IsShunt = true;  // defaults to shunt capacitor
	Cmatrix.clear();
	FC.clear();
	FXL.clear();
	Fkvarrating.clear();
	FR.clear();
	FHarm.clear();
	FStates.clear();

     /*Initialize these pointers to Nil so reallocmem will work reliably*/
	set_NumSteps(1);  // Initial Allocation for the Arrays, too
	set_LastStepInService(FNumSteps);
	FC.resize(FNumSteps);
	FXL.resize(FNumSteps);
	Fkvarrating.resize(FNumSteps);
	FR.resize(FNumSteps);
	FHarm.resize(FNumSteps);
	FStates.resize(FNumSteps);
	InitDblArray(FNumSteps, &FR[0], 0.0);
	InitDblArray(FNumSteps, &FXL[0], 0.0);
	InitDblArray(FNumSteps, &FHarm[0], 0.0);
	InitDblArray(FNumSteps, &Fkvarrating[0], 1200.0);
	FStates[1 - 1] = 1;
	kvrating = 12.47;
	InitDblArray(FNumSteps, &FC[0], 1.0L / (TwoPi * BaseFrequency * Sqr(kvrating) * 1000.0L / Fkvarrating[1 - 1]));
	Connection = 0;   // 0 or 1 for wye (default) or delta, respectively
	SpecType = 1; // 1=kvar, 2=Cuf, 3=Cmatrix
	NormAmps = Fkvarrating[1 - 1] * SQRT3 / kvrating * 1.35;   // 135%
	EmergAmps = NormAmps * 1.8 / 1.35;   //180%

	FNormAmpsSpecified	= false;
	FEmergAmpsSpecified = false;

	FaultRate = 0.0005;
	PctPerm = 100.0;
	HrsToRepair = 3.0;
	Yorder = Fnterms * Fnconds;
	DoHarmonicRecalc = false;
	Bus2Defined = false;
	RecalcElementData(ActiveActor);
	NumTerm = 1;
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TCapacitorObj::~TCapacitorObj()
{
	Cmatrix.clear();
	FC.clear();
	FXL.clear();
	Fkvarrating.clear();
	FR.clear();
	FHarm.clear();
	FStates.clear();
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TCapacitorObj::RecalcElementData(int ActorID)
{
	double KvarPerPhase = 0.0;
	double PhasekV = 0.0;
	double W = 0.0;
	int i = 0;
	Ftotalkvar = 0.0;
	PhasekV = 1.0;
	W = TwoPi * BaseFrequency;
	switch(SpecType)
	{
		case 	1: // kvar
		{
			int stop = 0;
			switch(Connection)
			{
				case 	1:  // Line-to-Line
				{
					PhasekV = kvrating;
				}
				break;
				default:
				switch(Fnphases)
				{
					case 	2:
					 case 3:
					PhasekV = kvrating / SQRT3;
					break;  // Assume three phase system
					default:
					PhasekV = kvrating;
					break;
				}
				break;
			}
			for(stop = FNumSteps, i = 1; i <= stop; i++)
			{
				FC[i - 1] = 1.0L / (W * Sqr(PhasekV) * 1000.0L / (Fkvarrating[1 - 1] / Fnphases));
			}
			for(stop = FNumSteps, i = 1; i <= stop; i++)
			{
				Ftotalkvar = Ftotalkvar + Fkvarrating[i - 1];
			}
		}
		break; // Cuf
		case 	2:
		{
			int stop = 0;
			switch(Connection)
			{
				case 	1:  // Line-to-Line
				{
					PhasekV = kvrating;
				}
				break;
				default:
				switch(Fnphases)
				{
					case 	2:
					 case 3:
					PhasekV = kvrating / SQRT3;
					break;  // Assume three phase system
					default:
					PhasekV = kvrating;
					break;
				}
				break;
			}
			for(stop = FNumSteps, i = 1; i <= stop; i++)
			{
				Ftotalkvar = Ftotalkvar + W * FC[i - 1] * Sqr(PhasekV) / 1000.0L;
			}
		}
		break; // Cmatrix
           // Nothing to do
		case 	3:
		break;
		default:
		  ;
		break;
	}
	if(DoHarmonicRecalc)
	{
		int stop = 0;
		for(stop = FNumSteps, i = 1; i <= stop; i++)
		{  // If harmonic specified, compute filter reactance
			if(FHarm[i - 1] != 0.0)
				FXL[i - 1] = (1.0 / (W * FC[i - 1])) / Sqr(FHarm[i - 1]);
			else
				FXL[i - 1] = 0.0;   // Assume 0 harmonic means no filter
			if(FR[i - 1] == 0.0)
				FR[i - 1] = FXL[i - 1] / 1000.0;
		}
	}
	KvarPerPhase = Ftotalkvar / Fnphases;
	if (!FNormAmpsSpecified)	NormAmps	= KvarPerPhase / PhasekV * 1.35;
	if (!FEmergAmpsSpecified)	EmergAmps	= KvarPerPhase / PhasekV * 1.8;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TCapacitorObj::CalcYPrim(int ActorID)
{
	int i = 0;
	TcMatrix* YPrimTemp = nullptr;
	TcMatrix* YprimWork = nullptr;

// Normally build only Yprim Shunt, but if there are 2 terminals and
// Bus1 <> Bus 2
	int stop = 0;
	if(Get_YprimInvalid(ActorID,0))    // Reallocate YPrim if something has invalidated old allocation
	{
		if(YPrim_Shunt != nullptr)
			delete YPrim_Shunt; //YPrim_Shunt->~TcMatrix();
		YPrim_Shunt = new TcMatrix(Yorder);
		if(YPrim_Series != nullptr)
			delete YPrim_Series; //YPrim_Series->~TcMatrix();
		YPrim_Series = new TcMatrix(Yorder);
		if(YPrim != nullptr)
			delete YPrim; //YPrim->~TcMatrix();
		YPrim = new TcMatrix(Yorder);
	}
	else
	{
		YPrim_Series->Clear(); // zero out YPrim
		YPrim_Shunt->Clear(); // zero out YPrim
		YPrim->Clear();
	}
	if(IsShunt)
		YPrimTemp = YPrim_Shunt;
	else
		YPrimTemp = YPrim_Series;
	YprimWork = new TcMatrix(Yorder);
	for(stop = FNumSteps, i = 1; i <= stop; i++)
	{
		if(FStates[i - 1] == 1)
		{
			MakeYprimWork(YprimWork, i, ActorID);
			YPrimTemp->AddFrom(YprimWork);
		}
	}
	delete YprimWork;

   // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
	if(IsShunt)
	{
		int stop = 0;
		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			YPrim_Series->SetElement(i, i, cmulreal(YPrim_Shunt->GetElement(i, i), 1.0e-10));
		}
	}
	YPrim->CopyFrom(YPrimTemp);

    /*Don't Free YPrimTemp - It's just a pointer to an existing complex matrix*/
	TDSSCktElement::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

void TCapacitorObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int j = 0;
	TDSSCktElement::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		{ Write(f, "~ "); Write(f, with0->PropertyName[1 - 1]); Write(f, L'='); WriteLn(f, Get_FirstBus()); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[2 - 1]); Write(f, L'='); WriteLn(f, Get_NextBus()); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[3 - 1]); Write(f, L'='); WriteLn(f, Fnphases, 0); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[4 - 1]); Write(f, L'='); WriteLn(f, GetPropertyValue(4)); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[5 - 1]); Write(f, L'='); WriteLn(f, kvrating, 0, 3); }
		switch(Connection)
		{
			case 	0:
			{ Write(f, "~ "); Write(f, with0->PropertyName[6 - 1]); WriteLn(f, "=wye"); }
			break;
			case 	1:
			{ Write(f, "~ "); Write(f, with0->PropertyName[6 - 1]); WriteLn(f, "=delta"); }
			break;
			default:
			  ;
			break;
		}
		if(!Cmatrix.empty())
		{
			int stop = 0;
			{ Write(f, with0->PropertyName[7 - 1]); Write(f, "= ("); }
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(stop1 = i, j = 1; j <= stop1; j++)
				{
					{ Write(f, ((Cmatrix)[(i - 1) * Fnphases + j - 1] * 1.0e6), 0, 3); Write(f, L' '); }
				}
				if(i != Fnphases)
					Write(f, L'|');
			}
			WriteLn(f, L')');
		}
		{ Write(f, "~ "); Write(f, with0->PropertyName[8 - 1]); Write(f, L'='); WriteLn(f, GetPropertyValue(8)); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[9 - 1]); Write(f, L'='); WriteLn(f, GetPropertyValue(9)); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[10 - 1]); Write(f, L'='); WriteLn(f, GetPropertyValue(10)); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[11 - 1]); Write(f, L'='); WriteLn(f, GetPropertyValue(11)); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[12 - 1]); Write(f, L'='); WriteLn(f, FNumSteps); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[13 - 1]); Write(f, L'='); WriteLn(f, GetPropertyValue(13)); }
		for(stop = with0->NumProperties, i = NumPropsThisClass + 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
		if(Complete)
		{
			{ Write(f, "SpecType="); WriteLn(f, SpecType, 0); }
		}
	}
}

void TCapacitorObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,GetBus(2));
	Set_PropertyValue(3,"3");
	Set_PropertyValue(4,"1200");
	Set_PropertyValue(5,"12.47");
	Set_PropertyValue(6,"wye");
	Set_PropertyValue(7,"");
	Set_PropertyValue(8,"");
	Set_PropertyValue(9,"0");
	Set_PropertyValue(10,"0");
	Set_PropertyValue(11,"0");
	Set_PropertyValue(12,"1");
	Set_PropertyValue(13,"1"); // states
	inherited::InitPropertyValues(NumPropsThisClass);

       // Override Inherited properties
       //  Override Inherited properties
	Set_PropertyValue(NumPropsThisClass + 1,Format("%g", NormAmps));
	Set_PropertyValue(NumPropsThisClass + 2,Format("%g", EmergAmps));
	Set_PropertyValue(NumPropsThisClass + 3,Str_Real(FaultRate, 0));
	Set_PropertyValue(NumPropsThisClass + 4,Str_Real(PctPerm, 0));
	Set_PropertyValue(NumPropsThisClass + 5,Str_Real(HrsToRepair, 0));
	ClearPropSeqArray();
}

void TCapacitorObj::MakePosSequence(int ActorID)
{
	String s;
	double KvarPerPhase = 0.0;
	double PhasekV = 0.0;
	double CS = 0.0;
	double CM = 0.0;
	int i = 0;
	int j = 0;
    /*If FnPhases>1 Then -- do same for 1-phase, too*/
	s = " ";
	switch(SpecType)
	{
		case 	1: // kvar
		{
			int stop = 0;
			if((Fnphases > 1) || (Connection != 0))
				PhasekV = kvrating / SQRT3;
			else
				PhasekV = kvrating;
			s = String("Phases=1 ") + Format(" kV=%-.5g kvar=(", PhasekV);

              // 1-6-16  do caps Like load ...
			for(stop = FNumSteps, i = 1; i <= stop; i++)
			{
				KvarPerPhase = Fkvarrating[i - 1] / 3.0;  // divide the total kvar equally among3 phases.../Fnphases;
				s = s + Format(" %-.5g", KvarPerPhase);
			}
			s = s + ")";

              /*Leave R as specified*/
		}
		break; //
		case 	2:
		{
			s = "Phases=1 ";
		}
		break;
		case 	3:
		if(Fnphases > 1) //  C Matrix
		{
			int stop = 0;
			s = "Phases=1 ";
              // R1
			CS = 0.0;   // Avg Self
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				CS = CS + (Cmatrix)[(i - 1) * Fnphases + i - 1];
			}
			CS = CS / Fnphases;
			CM = 0.0;     //Avg mutual
			for(stop = Fnphases, i = 2; i <= stop; i++)
			{
				int stop1 = 0;
				for(stop1 = Fnphases, j = i; j <= stop1; j++)
				{
					CM = CM + (Cmatrix)[(i - 1) * Fnphases + j - 1];
				}
			}
			CM = CM / (Fnphases * (Fnphases - 1.0) / 2.0);

			s = s + Format(" Cuf=%-.5g", (CS - CM));
		}
		break;
		default:
		  ;
		break;
	}
	Parser[ActorID]->SetCmdString(s);
	Edit(ActorID);
	inherited::MakePosSequence(ActorID);
}

int TCapacitorObj::get_States(int Idx, int ActorID)
{
	int result = 0;
	result = FStates[Idx - 1];
	return result;
}

void TCapacitorObj::set_States(int Idx, int ActorID, int Value)
{
	if(FStates[Idx - 1] != Value)
	{
		FStates[Idx - 1] = Value;
		Set_YprimInvalid(ActorID,true);
	}
}

//--------------------------------------------------------------------------------------

int TCapacitorObj::Get_NumTerm()
{
	return NumTerm;
}

//--------------------------------------------------------------------------------------

int TCapacitorObj::Get_FLastStepInService()
{
	return FLastStepInService;
}

//--------------------------------------------------------------------------------------

int TCapacitorObj::Get_FNumSteps()
{
	return FNumSteps;
}

//--------------------------------------------------------------------------------------

double TCapacitorObj::Get_kvrating()
{
	return kvrating;
}

//--------------------------------------------------------------------------------------

double TCapacitorObj::Get_Ftotalkvar()
{
	return Ftotalkvar;
}

/*
 Special case for changing from 1 to more ..  Automatically make a new bank
*/

void TCapacitorObj::set_NumSteps(int Value)
{
	double StepSize = 0.0;
	double RStep = 0.0;
	double XLstep = 0.0;
	int i = 0;
  /*reallocate all arrays associated with steps */
	if((FNumSteps != Value) && (Value > 0))
	{
		RStep = 0.0;
		XLstep = 0.0;
		if(FNumSteps == 1)
          /*Save total values to be divided up*/
		{
			Ftotalkvar = Fkvarrating[1 - 1];
			RStep = FR[1 - 1] * Value;
			XLstep = FXL[1 - 1] * Value;
		}

      // Reallocate arrays  (Must be initialized to nil for first call)
		FC.resize(Value + 1);
		FXL.resize(Value + 1);
		Fkvarrating.resize(Value + 1);
		FR.resize(Value + 1);
		FHarm.resize(Value + 1);
		FStates.resize(Value + 1);

      // Special case for FNumSteps=1
		if(FNumSteps == 1)
		{
			int stop = 0;
			switch(SpecType)
			{
				case 	1:  // kvar        {We'll make a multi-step bank of same net size as at present}
				{
					int stop = 0;
					StepSize = Ftotalkvar / Value;
					for(stop = Value, i = 1; i <= stop; i++)
					{
						Fkvarrating[i - 1] = StepSize;
					}
				}
				break;  // Cuf           {We'll make a multi-step bank with all the same as first}
				case 	2:
				{
					int stop = 0;
					for(stop = Value, i = 2; i <= stop; i++)
					{
						FC[i - 1] = FC[1 - 1];
					}  // Make same as first step
				}
				break;  // Cmatrix  {We'll make a multi-step bank with all the same as first}
                 // Nothing to do since all will be the same
				case 	3:
				break;
				default:
				  ;
				break;
			}
			switch(SpecType)
			{
				case 	1:
				{
					int stop = 0;
					for(stop = Value, i = 1; i <= stop; i++)
					{
						FR[i - 1] = RStep;
					}
					for(stop = Value, i = 1; i <= stop; i++)
					{
						FXL[i - 1] = XLstep;
					}
				}
				break;   // Make R and XL same as first step
				case 	2: case 3:
				{
					int stop = 0;
					for(stop = Value, i = 2; i <= stop; i++)
					{
						FR[i - 1] = FR[1 - 1];
					}
					for(stop = Value, i = 2; i <= stop; i++)
					{
						FXL[i - 1] = FXL[1 - 1];
					}
				}
				break;
				default:
				  ;
				break;
			}
			for(stop = Value, i = 1; i <= stop; i++)
			{
				FStates[i - 1] = 1;
			}   // turn 'em all ON
			set_LastStepInService(Value);
			for(stop = Value, i = 2; i <= stop; i++)
			{
				FHarm[i - 1] = FHarm[1 - 1];
			}  // tune 'em all the same as first
		}
	}
	FNumSteps = Value;
}

void TCapacitorObj::ProcessHarmonicSpec(const String Param)
{
	FNumSteps = InterpretDblArray(Param, FNumSteps, &(FHarm[0]));
	DoHarmonicRecalc = true;
}
// Find the last step energized

void TCapacitorObj::FindLastStepInService()
{
	int i = 0;
	int stop = 0;
	FLastStepInService = 0;
	for(i = FNumSteps - 1; i >= 0; i--)
	{
		if(FStates[i] == 1)
		{
			FLastStepInService = i;
			break;
		}
	}
}
// force the last step in service to be a certain value

void TCapacitorObj::set_LastStepInService(int Value)
{
	int i = 0;
	int stop = 0;
	for(stop = Value, i = 1; i <= stop; i++)
	{
		FStates[i - 1] = 1;
	}
     // set remainder steps, if any, to 0
	for(stop = FNumSteps, i = Value + 1; i <= stop; i++)
	{
		FStates[i - 1] = 0;
	}

     // Force rebuild of YPrims if necessary.
	if(Value != FLastStepInService)
		Set_YprimInvalid(ActiveActor,true);
	FLastStepInService = Value;
}

void TCapacitorObj::ProcessStatesSpec(const String Param)
{
	vector <longInt> TArray;
	TArray.resize(FStates.size());
	FNumSteps = InterpretIntArray(Param, FNumSteps, &TArray[0]);
	for (int i = 0; i < FNumSteps; i++)
		FStates[i] = TArray[i];
	FindLastStepInService();
}

/* call this routine only if step is energized*/

void TCapacitorObj::MakeYprimWork(TcMatrix* YprimWork, int iStep, int ActorID)
{
	complex Value = {};
	complex Value2 = {};
	complex ZL = {};
	int i = 0;
	int j = 0;
	int IOffset = 0;
	double W = 0.0;
	double FreqMultiple = 0.0;
	bool HasZL = false;
	/*# with YprimWork do */
	{
		auto with0 = YprimWork;
		FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
		FreqMultiple = FYprimFreq / BaseFrequency;
		W = TwoPi * FYprimFreq;
		if((FR[iStep - 1] + Abs((int) FXL[iStep - 1])) > 0.0)
			HasZL = true;
		else
			HasZL = false;
		if(HasZL)
		{
			ZL = cmplx(FR[iStep - 1], FXL[iStep - 1] * FreqMultiple);
		}

    /* Now, Put C into in Yprim matrix */
		switch(SpecType)
		{
			case 	1:
			case	2:
			{
				Value = cmplx(0.0, FC[iStep - 1] * W);
				switch(Connection)
				{
					case 	1:   // Line-Line
					{
						int stop = 0;
						for(stop = Fnphases, i = 1; i <= stop; i++)
						{
							j = i + 1;
							if (j > Fnconds)
								j = 1;		// wrap around for closed connections
							with0->AddElement(i, i, Value);
							with0->AddElement(j, j, Value);
							with0->AddElemsym(i, j, cnegate(Value));
						}
                // Remainder of the matrix is all zero
					}
					break;
					default:
					int stop = 0;
					if(HasZL)
						Value = cinv(cadd(ZL, cinv(Value))); // add in ZL
					Value2 = cnegate(Value);
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						with0->SetElement(i, i, Value);     // Elements are only on the diagonals
						with0->SetElement(i + Fnphases, i + Fnphases, Value);
						with0->SetElemsym(i, i + Fnphases, Value2);
					}
					break;
				}
			}
			break;    // C matrix specified
			case 	3:
			{
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					IOffset = (i - 1) * Fnphases;
					for(stop1 = Fnphases, j = 1; j <= stop1; j++)
					{
						Value = cmplx(0.0, (Cmatrix)[(IOffset + j) - 1] * W);
						with0->SetElement(i, j, Value);
						with0->SetElement(i + Fnphases, j + Fnphases, Value);
						Value = cnegate(Value);
						with0->SetElemsym(i, j + Fnphases, Value);
					}
				}
			}
			break;
			default:
			  ;
			break;
		}

      /*Add line reactance for filter reactor, if any*/
		if(HasZL)
			switch(SpecType)
			{
				case 	1: case 2:
				switch(Connection)
				{
					case 	1: /*Line-Line*/
                         /*Add a little bit to each phase so it will invert*/
					{
						int stop = 0;
						for(stop = Fnphases, i = 1; i <= stop; i++)
						{
							with0->SetElement(i, i, cmulreal(with0->GetElement(i, i), 1.000001));
						}
						with0->Invert();
						for(stop = Fnphases, i = 1; i <= stop; i++)
						{
							Value = cadd(ZL, with0->GetElement(i, i));
							with0->SetElement(i, i, Value);
						}
						with0->Invert();
					}
					break; /*WYE - just put ZL in series*/
                      /*DO Nothing; Already in - see above*/
					default:
					  ;
					break;
				}
				break;
				case 	3:
				{
					int stop = 0;
					with0->Invert();
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						Value = cadd(ZL, with0->GetElement(i, i));
						with0->SetElement(i, i, Value);
					}
					with0->Invert();
				}
				break;
				default:
				  ;
				break;
			}
	} /*With YPRIM*/
}

String TCapacitorObj::GetPropertyValue(int Index)
{
	String result;
	int i = 0;
	pDoubleArray FTemp = nullptr;

	result = "";
	switch(Index)
	{
		case 	1:  // Special cases
		result = GetBus(1);
		break;
		case 	2:
		result = GetBus(2);
		break;
		case 	4:
		result = GetDSSArray_Real(FNumSteps, &Fkvarrating[0]);
		break;
		case 	5:
		{
			result = Format("%-g", kvrating);
		}
		break;
		case 	8:
		{
			int stop = 0;
			FTemp = (pDoubleArray) realloc(FTemp, sizeof(double) * FNumSteps);
			for(stop = FNumSteps, i = 1; i <= stop; i++)
			{
				FTemp[i - 1] = FC[i - 1] * 1.0e6;
			}  // To microfarads
			result = GetDSSArray_Real(FNumSteps, FTemp);
			free(FTemp); // throw away temp storage
		}
		break;
		case 	9:
		result = GetDSSArray_Real(FNumSteps, &FR[0]);
		break;
		case 	10:
		result = GetDSSArray_Real(FNumSteps, &FXL[0]);
		break;
		case 	11:
		result = GetDSSArray_Real(FNumSteps, &FHarm[0]);
		break;
		case 	13:
		result = GetDSSArray_Integer(FNumSteps, &FStates[0]);
		break;
		case 	14:
		{

			result = Format("%g", NormAmps);
		}
		break;
		case 	15:
		{
			result = Format("%g", EmergAmps);
		}
		break;
		default:
		result = TDSSCktElement::GetPropertyValue(Index);
		break;
	}
	return result;
}

bool TCapacitorObj::AddStep(int ActorID)
{
	bool result = false;
     // Start with last step in service and see if we can add more.  If not return FALSE
	if(Get_FLastStepInService() == FNumSteps)
		result = false;
	else
	{
		++FLastStepInService;
		set_States(FLastStepInService,ActorID,1);
		result = true;
	}
	return result;
}

bool TCapacitorObj::SubtractStep(int ActorID)
{
	bool result = false;
	if(Get_FLastStepInService() == 0)
		result = false;
	else
	{
		set_States(FLastStepInService,ActorID,0);
		--FLastStepInService;
		if(Get_FLastStepInService() == 0)
			result = false;
		else
			result = true;   // signify bank OPEN
	}
	return result;
}

int TCapacitorObj::AvailableSteps()
{
	int result = 0;
	result = FNumSteps - Get_FLastStepInService();
	return result;
}




}  // namespace Capacitor




