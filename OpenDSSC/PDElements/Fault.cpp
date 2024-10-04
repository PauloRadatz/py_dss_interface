
#pragma hdrstop

#include "Fault.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Dynamics.h"
#include "Ucomplex.h"
#include "mathutil.h"
#include "Utilities.h"
#include "Circuit.h"

using namespace std;
using namespace Arraydef;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Dynamics;
using namespace PDClass;
using namespace PDELement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace Fault
{

TFaultObj::TFaultObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TFaultObj::TFaultObj(String ClassName) : inherited(ClassName) {}
TFaultObj::TFaultObj() {}


TFaultObj* ActiveFaultObj = nullptr;
const int NumPropsThisClass = 9;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Fault objects

TFault::TFault()
{
	;
	Class_Name = "Fault";
	DSSClassType = FAULTOBJECT + NON_PCPD_ELEM;  // Only in Fault object class
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TFault::~TFault()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TFault::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "bus1";
	PropertyName[2 - 1] = "bus2";
	PropertyName[3 - 1] = "phases";
	PropertyName[4 - 1] = "r";
	PropertyName[5 - 1] = "%stddev";
	PropertyName[6 - 1] = "Gmatrix";
	PropertyName[7 - 1] = "ONtime";
	PropertyName[8 - 1] = "temporary";
	PropertyName[9 - 1] = "MinAmps";

     // define Property help values
	PropertyHelp[1 - 1] = String("Name of first bus. Examples:") + CRLF
	           + CRLF
	           + "bus1=busname"
	           + CRLF
	           + "bus1=busname.1.2.3"
	           + CRLF
	           + CRLF
	           + "Bus2 automatically defaults to busname.0,0,0 unless it was previously defined. ";
	PropertyHelp[2 - 1] = String("Name of 2nd bus of the 2-terminal Fault object. Defaults to all phases connected " "to first bus, node 0, if not specified. (Shunt Wye Connection to ground reference)") + CRLF
	           + CRLF
	           + "That is, the Fault defaults to a ground fault unless otherwise specified.";
	PropertyHelp[3 - 1] = "Number of Phases. Default is 1.";
	PropertyHelp[4 - 1] = "Resistance, each phase, ohms. Default is 0.0001. Assumed to be Mean value if gaussian random mode."
	           "Max value if uniform mode.  A Fault is actually a series resistance "
	           "that defaults to a wye connection to ground on the second terminal.  You "
	           "may reconnect the 2nd terminal to achieve whatever connection.  Use "
	           "the Gmatrix property to specify an arbitrary conductance matrix.";
	PropertyHelp[5 - 1] = "Percent standard deviation in resistance to assume for Monte Carlo fault (MF) solution mode for GAUSSIAN distribution. Default is 0 (no variation from mean).";
	PropertyHelp[6 - 1] = "Use this to specify a nodal conductance (G) matrix to represent some arbitrary resistance network. "
	           "Specify in lower triangle form as usual for DSS matrices.";
	PropertyHelp[7 - 1] = "Time (sec) at which the fault is established for time varying simulations. Default is 0.0 "
	           "(on at the beginning of the simulation)";
	PropertyHelp[8 - 1] = "{Yes | No} Default is No.  Designate whether the fault is temporary.  For Time-varying simulations, "
	           "the fault will be removed if the current through the fault drops below the MINAMPS criteria.";
	PropertyHelp[9 - 1] = "Minimum amps that can sustain a temporary fault. Default is 5.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TFault::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TFaultObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TFault::DoGmatrix(int ActorID)
{
	int OrderFound = 0;
	int j = 0;
	pDoubleArray MatBuffer = nullptr;
	/*# with ActiveFaultObj do */
	{
		auto with0 = ActiveFaultObj;
		MatBuffer = new double[ with0->Fnphases * with0->Fnphases ];
		OrderFound = Parser[ActorID]->ParseAsSymMatrix( with0->Fnphases, MatBuffer);
		if(OrderFound > 0)    // Parse was successful
			    /*X*/
			{
				int stop = 0;
				with0->Gmatrix = (pDoubleArray) realloc(with0->Gmatrix, sizeof(double) * with0->Fnphases * with0->Fnphases);
				for(stop = ( with0 )->Fnphases * ( with0 )->Fnphases, j = 1; j <= stop; j++)
				{
					(with0->Gmatrix)[j - 1] = (MatBuffer)[j - 1];
				}
			}
		free(MatBuffer); //# FreeMemory accepts one parameter only;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TFault::FltSetBus1(const String s)
{
	String S2;
	int dotpos = 0;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0
	/*# with ActiveFaultObj do */
	{
		auto& with0 = ActiveFaultObj;
		with0->SetBus(1, s);

     // Default Bus2 to zero node of Bus1 unless previously defined explicitly. (Wye Grounded connection)
		if(!with0->Bus2Defined)
         // Strip node designations from S
		{
			dotpos = Pos(".", s);
			if(dotpos > 0)  // copy up to Dot
				S2 = s.substr(0, dotpos - 1);
			else
				S2 = s.substr(0, s.size());
			S2 = S2 + ".0.0.0";     // Set Default for up to 3 phases
			with0->SetBus(2, S2);
			with0->IsShunt = true;
		}
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TFault::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int PhasesTemp = 0;
	result = 0;
  // continue parsing with contents of Parser
	ActiveFaultObj = (TFaultObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveFaultObj);  // use property to set this value
	/*# with ActiveFaultObj do */
	{
		auto with0 = ActiveFaultObj;
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
	           + "\"", 350);
				break;
				case 	1:
				FltSetBus1(Param);
				break;
				case 	2:
					with0->SetBus(2, Param);
				break;
				case 	3:
				;
				break;/*Numphases := Parser.MakeInteger_();*/  // see below
				case 	4:
				{
					with0->G = Parser[ActorID]->MakeDouble_();
					if(with0->G != 0.0)
						with0->G = 1.0 / with0->G;
					else
						with0->G = 10000.0;  // Default to a low resistance
				}
				break;
				case 	5:
				with0->StdDev = Parser[ActorID]->MakeDouble_() * 0.01;
				break;
				case 	6:
				DoGmatrix(ActorID);
				break;
				case 	7:
				with0->On_Time = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
				with0->IsTemporary = InterpretYesNo(Param);
				break;
				case 	9:
				with0->MinAmps = Parser[ActorID]->MakeDouble_();
				break;
           // Inherited
				default:
					ClassEdit(ActiveFaultObj, ParamPointer - NumPropsThisClass);
				break;
			}

         // Some specials ...
			switch(ParamPointer)
			{
				case 	1:
					with0->Set_PropertyValue(2,with0->GetBus(2));
				break;  // Bus2 gets modified if bus1 is
				case 	2:
				if(CompareText(StripExtension(with0->GetBus(1) ), StripExtension( with0->GetBus(2))) != 0 )
				{
					with0->IsShunt = false;
					with0->Bus2Defined = true;
				}
				break;
				case 	3:
				{
					PhasesTemp = Parser[ActorID]->MakeInteger_();
					if( with0->Fnphases != PhasesTemp)
					{
						with0->Set_NPhases(PhasesTemp);
						with0->Set_Nconds(with0->Fnphases);  // Force Reallocation of terminal info
						ActiveCircuit[ActorID]->Set_BusNameRedefined(true);  // Set Global Flag to signal circuit to rebuild busdefs
					}
				}
				break;
				case 	4:
				with0->SpecType = 1;
				break;
				case 	6:
				with0->SpecType = 2;
				break;
				case 	7:
				if(with0->On_Time > 0.0)
					with0->Is_ON = false;
				break;   // Assume fault will be on later
				default:
				  ;
				break;
			}

         //YPrim invalidation on anything that changes impedance values
			switch(ParamPointer)
			{
				case 	3: case 4: case 6:
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

int TFault::MakeLike(const String FaultName)
{
	int result = 0;
	TFaultObj* OtherFault = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Fault name in the present collection*/
	OtherFault = ((TFaultObj*) Find(FaultName));
	if(OtherFault != nullptr)
		/*# with ActiveFaultObj do */
		{
			auto with0 = ActiveFaultObj;
			int stop = 0;
			if(((TDSSCktElement*)with0)->Fnphases != ( (TDSSCktElement*) OtherFault )->Fnphases)
			{
				with0->Fnphases = OtherFault->Fnphases;
				with0->Set_Nconds(with0->Fnphases); // force reallocation of terminals and conductors
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->BaseFrequency = OtherFault->BaseFrequency;
			with0->G = OtherFault->G;
			with0->SpecType = OtherFault->SpecType;
			with0->MinAmps = OtherFault->MinAmps;
			with0->IsTemporary = OtherFault->IsTemporary;
			with0->Cleared = OtherFault->Cleared;
			with0->Is_ON = OtherFault->Is_ON;
			with0->On_Time = OtherFault->On_Time;
			if(OtherFault->Gmatrix == nullptr)
				with0->Gmatrix = NULL;
			else
			{
				int stop = 0;
				with0->Gmatrix = (pDoubleArray) realloc(with0->Gmatrix, sizeof(double) * with0->Fnphases * with0->Fnphases);
				for(stop = with0->Fnphases * with0->Fnphases, i = 1; i <= stop; i++)
				{
					(with0->Gmatrix)[i - 1] = (OtherFault->Gmatrix)[i - 1];
				}
			}
			ClassMakeLike(OtherFault);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherFault->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Fault MakeLike: \"") + FaultName + "\" Not Found.", 351);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TFault::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TFault.Init", -1);
	result = 0;
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TFault Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TFaultObj::TFaultObj(TDSSClass* ParClass, const String FaultName)
 : inherited(ParClass),
			MinAmps(5.0),
			IsTemporary(false),
			Cleared(false),
			Is_ON(true),
			Bus2Defined(false),
			On_Time(0.0),
			RandomMult(0.0),
			G(0.0),
			Gmatrix(nullptr),
			StdDev(0.0),
			SpecType(0)
{
	DSSObjType = ParClass->DSSClassType; //FAULTOBJECT + NON_PCPD_ELEM;  // Only in Fault object class
	Set_Name(LowerCase(FaultName));

     // Default to SLG fault
	Set_NPhases(1);  // Directly set conds and phases
	Fnconds = 1;
	Set_NTerms(2);  // Force allocation of terminals and conductors
	SetBus(2, (GetBus(1) + ".0"));  // Default to grounded
	IsShunt = true;
	Gmatrix = nullptr;
	G = 10000.0;
	SpecType = 1; // G  2=Gmatrix
	Bus2Defined = false;
	On_Time = 0.0;  // Always enabled at the start of a solution.
	RandomMult = 1;
	NormAmps = 0.0;
	EmergAmps = 0.0;
	FaultRate = 0.0;
	PctPerm = 100.0;
	HrsToRepair = 0.0;
	InitPropertyValues(0);
	Yorder = Fnterms * Fnconds;
	RecalcElementData(ActiveActor);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TFaultObj::~TFaultObj()
{
	free(Gmatrix);
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TFaultObj::RecalcElementData(int ActorID)
{


// Nothing to do
}

double Cube(double X)
{
	double result = 0.0;
	result = X * X * X;
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// called from solveMontefault Procedure

void TFaultObj::Randomize(int ActorID)
{
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		switch(with0->RandomType)
		{
			case 	GAUSSIAN:
			RandomMult = Gauss(1.0, StdDev);
			break;
			case 	UNIFORM:
			RandomMult = (double) Random();
			break;
			case 	LOGNORMAL:
			RandomMult = QuasiLogNormal(1.0);
			break;
			default:
			RandomMult = 1.0;
			break;
		}
	}

     // Give the multiplier some skew to approximate more uniform/Gaussian current distributions
     //  RandomMult :=  Cube(RandomMult);   removed 12/7/04
	Set_YprimInvalid(ActorID,true);    // force rebuilding of matrix
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TFaultObj::CalcYPrim(int ActorID)
{
	complex Value = {};
	complex Value2 = {};
	int i = 0;
	int j = 0;
	int IOffset = 0;
	TcMatrix* YPrimTemp = nullptr;
	if(Get_YprimInvalid(ActorID,0))    // Reallocate YPrim if something has invalidated old allocation
	{
		if(YPrim_Series != nullptr)
			delete YPrim_Series;
		YPrim_Series = new TcMatrix(Yorder);
		if(YPrim_Shunt != nullptr)
			delete YPrim_Shunt;
		YPrim_Shunt = new TcMatrix(Yorder);
		if(YPrim != nullptr)
			delete YPrim;
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

  // make sure randommult is 1.0 if not solution mode MonteFault
	if(ActiveCircuit[ActorID]->Solution->Get_SolMode() != MONTEFAULT)
		RandomMult = 1.0;
	if(RandomMult == 0.0)
		RandomMult = 0.000001;
	/*# with YPrimTemp do */
	{
		auto with0 = YPrimTemp;

    /* Now, Put in Yprim matrix */

    /*If the fault is not ON, the set zero conductance*/
		switch(SpecType)
		{
			case 	1:
			{
				int stop = 0;
				if(Is_ON)
					Value = cmplx(G / RandomMult, 0.0);
				else
					Value = CZero;
				Value2 = cnegate(Value);
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					with0->SetElement(i, i, Value);     // Elements are only on the diagonals
					with0->SetElement(i + Fnphases, i + Fnphases, Value);
					with0->SetElemsym(i, i + Fnphases, Value2);
				}
			}
			break;    // G matrix specified
			case 	2:
			{
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					IOffset = (i - 1) * Fnphases;
					for(stop1 = Fnphases, j = 1; j <= stop1; j++)
					{
						if(Is_ON)
							Value = cmplx((Gmatrix)[(IOffset + j) - 1] / RandomMult, 0.0);
						else
							Value = CZero;
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
	} /*With YPRIM*/
	YPrim->CopyFrom(YPrimTemp);
	TDSSCktElement::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

void TFaultObj::DumpProperties(TTextRec& f, bool Complete)
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
		{ Write(f, "~ "); Write(f, with0->PropertyName[4 - 1]); Write(f, L'='); WriteLn(f, (1.0 / G), 0, 2); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[5 - 1]); Write(f, L'='); WriteLn(f, (StdDev * 100.0), 0, 1); }
		if(Gmatrix != nullptr)
		{
			int stop = 0;
			{ Write(f, "~ "); Write(f, with0->PropertyName[6 - 1]); Write(f, "= ("); }
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(stop1 = i, j = 1; j <= stop1; j++)
				{
					{ Write(f, ((Gmatrix)[(i - 1) * Fnphases + j - 1]), 0, 3); Write(f, L' '); }
				}
				if(i != Fnphases)
					Write(f, L'|');
			}
			WriteLn(f, L')');
		}
		{ Write(f, "~ "); Write(f, with0->PropertyName[7 - 1]); Write(f, L'='); WriteLn(f, On_Time, 0, 3); }
		if(IsTemporary)
			{ Write(f, "~ "); Write(f, with0->PropertyName[8 - 1]); WriteLn(f, "= Yes"); }
		else
			{ Write(f, "~ "); Write(f, with0->PropertyName[8 - 1]); WriteLn(f, "= No"); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[9 - 1]); Write(f, L'='); WriteLn(f, MinAmps, 0, 1); }
		for(stop = with0->NumProperties, i = NumPropsThisClass; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
		if(Complete)
		{
			{ Write(f, "// SpecType="); WriteLn(f, SpecType, 0); }
		}
	}
}

void TFaultObj::CheckStatus(int ControlMode, int ActorID)
{
	switch(ControlMode)
	{   /*Leave it however it is defined by other processes*/
		case 	CTRLSTATIC:
		break;
		case 	EVENTDRIVEN:
		 case MULTIRATE:
		 case TIMEDRIVEN:
		{
			if(!Is_ON)   /*Turn it on unless it has been previously cleared*/
			{
				if((PresentTimeInSec(ActorID) > On_Time) && !Cleared)
				{
					Is_ON = true;
					Set_YprimInvalid(ActorID,true);
					AppendToEventLog(String("Fault.") + get_Name(), "**APPLIED**", ActorID);
				}
			}
			else
			{
				if(IsTemporary)
				{
					if(!FaultStillGoing(ActorID))
					{
						Is_ON = false;
						Cleared = true;
						Set_YprimInvalid(ActorID,true);
						AppendToEventLog(String("Fault.") + get_Name(), "**CLEARED**", ActorID);
					}
				}
			}
		}
		break;
		default:
		  ;
		break;
	}
}

bool TFaultObj::FaultStillGoing(int ActorID)
{
	bool result = false;
	int i = 0;
	int stop = 0;
	ComputeIterminal(ActorID);
	result = false;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		if(cabs((Iterminal)[i - 1]) > MinAmps)
		{
			result = true;
			return result;
		}
	}
	return result;
}

void TFaultObj::Reset()
{
	Cleared = false;
}

void TFaultObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,GetBus(2));
	Set_PropertyValue(3,"1");
	Set_PropertyValue(4,"0.0001");
	Set_PropertyValue(5,"0");
	Set_PropertyValue(6,"");
	Set_PropertyValue(7,"0.0");
	Set_PropertyValue(8,"no");
	Set_PropertyValue(9,"5.0");
	inherited::InitPropertyValues(NumPropsThisClass);

     // Override Inherited Properties
	Set_PropertyValue(NumPropsThisClass + 1,"0");  //Normamps
	Set_PropertyValue(NumPropsThisClass + 2,"0");  //emergamps
	Set_PropertyValue(NumPropsThisClass + 3,"0");  //Fault rate
	Set_PropertyValue(NumPropsThisClass + 4,"0");   // Pct Perm
	Set_PropertyValue(NumPropsThisClass + 5,"0");    // Hrs to repair
}

String TFaultObj::GetPropertyValue(int Index)
{
	String result;
	int i = 0;
	int j = 0;
	switch(Index)
	{
		case 	6:
		{
			result = "(";
			if(ASSIGNED(Gmatrix))
			{
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{      // G matrix
					int stop1 = 0;
					for(stop1 = i, j = 1; j <= stop1; j++)
					{
						result = result + Format("%-g", (Gmatrix)[(i - 1) * Fnphases + j - 1]) + " ";
					}
					if(i < Fnphases)
						result = result + "|";
				}
			}
			result = result + ")";
		}
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TFaultObj::MakePosSequence(int ActorID)
{
	if(Fnphases != 1)
	{
		Parser[ActorID]->SetCmdString("Phases=1");
		Edit(ActorID);
	}
	inherited::MakePosSequence(ActorID);
}




}  // namespace Fault




