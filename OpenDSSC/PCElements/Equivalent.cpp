
#pragma hdrstop

#include "Equivalent.h"

#include "ParserDel.h"
#include "Circuit.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Utilities.h"
#include "Command.h"

using namespace std;
using namespace Arraydef;
using namespace Circuit;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace Utilities;

namespace Equivalent
{

TEquivalentObj::TEquivalentObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TEquivalentObj::TEquivalentObj(String ClassName) : inherited(ClassName) {}
TEquivalentObj::TEquivalentObj() {}


TEquivalentObj* ActiveEquivalentObj = nullptr;
const int NumPropsThisClass = 16;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TEquivalent::TEquivalent()
{
	;
	Class_Name = "Equivalent";
	DSSClassType = SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TEquivalent::~TEquivalent()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TEquivalent::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "terminals";
	PropertyName[2 - 1] = "buses";
	PropertyName[3 - 1] = "basekv";
	PropertyName[4 - 1] = "pu";
	PropertyName[5 - 1] = "angle";
	PropertyName[6 - 1] = "frequency";
	PropertyName[7 - 1] = "phases";
	PropertyName[8 - 1] = "R1";
	PropertyName[9 - 1] = "X1";
	PropertyName[10 - 1] = "R0";
	PropertyName[11 - 1] = "X0";

     // define Property help values
	PropertyHelp[1 - 1] = "Number of terminals.  Default =1. Set this BEFORE defining matrices.";
	PropertyHelp[2 - 1] = String("Array of Bus Names to which equivalent source is connected.") + CRLF
	           + "buses=(b1 b2 b3)";
	PropertyHelp[3 - 1] = "Base Source kV, usually L-L unless you are making a positive-sequence model"
	           "in which case, it will be L-N.";
	PropertyHelp[4 - 1] = String("Per unit of the base voltage that the source is actually operating at.") + CRLF
	           + "\"pu=1.05\"";
	PropertyHelp[5 - 1] = "Phase angle in degrees of first phase: e.g.,Angle=10.3";
	PropertyHelp[6 - 1] = "Source frequency.  Defaults to  60 Hz.";
	PropertyHelp[7 - 1] = "Number of phases.  Defaults to 3.";
	PropertyHelp[8 - 1] = "Positive-sequence resistance matrix, lower triangle.";
	PropertyHelp[9 - 1] = "Positive-sequence reactance matrix, lower triangle.";
	PropertyHelp[10 - 1] = "Zero-sequence resistance matrix, lower triangle.";
	PropertyHelp[11 - 1] = "Zero-sequence reactance matrix, lower triangle.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // Override help string
	PropertyHelp[NumPropsThisClass + 1 - 1] = "Name of harmonic spectrum for this source.  Default is \"defaultvsource\", which is defined when the DSS starts.";
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TEquivalent::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new voltage source and add it to Equivalent class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TEquivalentObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TEquivalent::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
  // continue parsing with contents of Parser
	ActiveEquivalentObj = (TEquivalentObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement((TDSSCktElement*) ActiveEquivalentObj);
	result = 0;
	/*# with ActiveEquivalentObj do */
	{
		auto with0 = ActiveEquivalentObj;
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
	           + "\" for Object \"Equivalent."
	           + with0->get_Name()
	           + "\"", 800);
				break;
				case 	1:
				( (TDSSCktElement*) with0 )->Set_NTerms(with0->DoTerminalsDef(Parser[ActorID]->MakeInteger_()));
				break;  // This will allocate a bunch of stuff
				case 	2:
				InterpretAllBuses(Param);
				break;
				case 	3:
				with0->kVBase = Parser[ActorID]->MakeDouble_();
				break; // basekv
				case 	4:
				with0->PerUnit = Parser[ActorID]->MakeDouble_();
				break; // pu
				case 	5:
				with0->Angle = Parser[ActorID]->MakeDouble_();
				break; // Ang
				case 	6:
				with0->EquivFrequency = Parser[ActorID]->MakeDouble_();
				break; // freq
				case 	7:
				{
					((TDSSCktElement*)with0)->Set_NPhases(Parser[ActorID]->MakeInteger_()); // num phases
					((TDSSCktElement*)with0)->Set_Nconds(((TDSSCktElement*)with0)->Fnphases);  // Force Reallocation of terminal info
				}
				break;
				case 	8:
				with0->ParseDblMatrix(with0->R1);
				break;
				case 	9:
				with0->ParseDblMatrix(with0->X1);
				break;
				case 	10:
				with0->ParseDblMatrix(with0->R0);
				break;
				case 	11:
				with0->ParseDblMatrix(with0->X0);
				break;
				default:
				inherited::ClassEdit(ActiveEquivalentObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	1: case 8: case 9: case 10: case 11:
				with0->NeedToDoRecalc = true;
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}

    // RecalcElementData;
		((TDSSCktElement*)with0)->Set_YprimInvalid(ActorID,true);
	}
	return result;
}

//----------------------------------------------------------------------------

int TEquivalent::MakeLike(const String OtherSource)
{
	int result = 0;
	TEquivalentObj* OtherEquivalent = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line name in the present collection*/
	OtherEquivalent = ((TEquivalentObj*) Find(OtherSource));
	if(OtherEquivalent != nullptr)
		/*# with ActiveEquivalentObj do */
		{
			auto with0 = ActiveEquivalentObj;
			int stop = 0;
			if(((with0)->Fnphases != (OtherEquivalent)->Fnphases) || ((with0)->Fnterms != (OtherEquivalent)->Fnterms))
			{
				int stop = 0;
				(with0)->Set_NTerms(with0->DoTerminalsDef((OtherEquivalent)->Fnterms));
				(with0)->Set_NPhases((OtherEquivalent)->Fnphases);
				(with0)->Set_Nconds((with0)->Fnphases);  // Forces reallocation of terminal stuff
				(with0)->Yorder = (with0)->Fnconds * (with0)->Fnterms;
				(with0)->Set_YprimInvalid(ActiveActor,true);
				for(stop = (with0)->Fnterms, i = 1; i <= stop; i++)
				{
					(with0->R1)[i - 1] = (OtherEquivalent->R1)[i - 1];
				}
				for(stop = (with0)->Fnterms, i = 1; i <= stop; i++)
				{
					(with0->R0)[i - 1] = (OtherEquivalent->R0)[i - 1];
				}
				for(stop = (with0)->Fnterms, i = 1; i <= stop; i++)
				{
					(with0->X1)[i - 1] = (OtherEquivalent->X1)[i - 1];
				}
				for(stop = (with0)->Fnterms, i = 1; i <= stop; i++)
				{
					(with0->X0)[i - 1] = (OtherEquivalent->X0)[i - 1];
				}
				if(with0->Z != nullptr)
					delete with0->Z;
				if(with0->Zinv != nullptr)
					delete with0->Zinv;
				with0->Z = new TcMatrix(((TDSSCktElement*)with0)->Fnphases);
				with0->Zinv = new TcMatrix(((TDSSCktElement*)with0)->Fnphases);
			}
			with0->Z->CopyFrom(OtherEquivalent->Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
			with0->Vmag = OtherEquivalent->Vmag;
			with0->kVBase = OtherEquivalent->kVBase;
			with0->PerUnit = OtherEquivalent->PerUnit;
			with0->Angle = OtherEquivalent->Angle;
			with0->EquivFrequency = OtherEquivalent->EquivFrequency;
			ClassMakeLike(OtherEquivalent);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->FPropertyValue[i - 1] = OtherEquivalent->FPropertyValue[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Equivalent MakeLike: \"") + OtherSource
	           + "\" Not Found.", 801);
	return result;
}

//----------------------------------------------------------------------------

int TEquivalent::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TEquivalent.Init", -1);
	result = 0;
	return result;
}

//----------------------------------------------------------------------------

TEquivalentObj::TEquivalentObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			kVBase(115.0),
			Vmag(0.0),
			PerUnit(0.0),
			Angle(0.0),
			EquivFrequency(0.0),
			R1(nullptr),
			X1(nullptr),
			R0(nullptr),
			X0(nullptr),
			NeedToDoRecalc(false),
			Z(nullptr),
			Zinv(nullptr)
{
	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	Set_NPhases(3);
	Fnconds = 3;
	Set_NTerms(1);
	Z = nullptr;
	Zinv = nullptr;
     /*Basefrequency := 60.0;*/ // set in base class
	R1 = nullptr;
	X1 = nullptr;
	R0 = nullptr;
	X0 = nullptr;
	ReallocRX();
	(R1)[1 - 1] = 1.65;
	(X1)[1 - 1] = 6.6;
	(R0)[1 - 1] = 1.9;
	(X0)[1 - 1] = 5.7;
	PerUnit = 1.0;
	EquivFrequency = BaseFrequency;
	Angle = 0.0;
	Spectrum = "defaultvsource";
	InitPropertyValues(0);
	Yorder = Fnterms * Fnconds;
	RecalcElementData(ActiveActor);
}


//----------------------------------------------------------------------------

TEquivalentObj::~TEquivalentObj()
{
	free(Z);
	free(Zinv);
	free(R1);
	free(R0);
	free(X1);
	free(X0);
	// inherited::Destroy();
}


//----------------------------------------------------------------------------

void TEquivalentObj::RecalcElementData(int ActorID)
{
	complex Zs = {};
	complex ZM = {};
	int i = 0;
	int j = 0;
	int II = 0;
	int jj = 0;
	int IOffset = 0;
	int joffset = 0;
	int indx = 0;

	auto Idx = [&](int A, int B) -> int 
	{
		int result = 0;
		result = (B - 1) * Fnterms + A;
		return result;
	};
	int stop = 0;
	if(Z != nullptr)
		delete Z;
	if(Zinv != nullptr)
		delete Zinv;

    // For a Source, nphases = ncond, for now
	Z = new TcMatrix(Fnphases * Fnterms);
	Zinv = new TcMatrix(Fnphases * Fnterms);

     // Build Z matrix for all phases
	for(stop = Fnterms, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Fnterms, j = 1; j <= stop1; j++)
		{
			int stop2 = 0;
			indx = Idx(i, j);
			Zs = cdivreal(cmplx(2.0 * (R1)[indx - 1] + (R0)[indx - 1], 2.0 * (X1)[indx - 1] + (X0)[indx - 1]), 3.0);
			ZM = cdivreal(cmplx((R0)[indx - 1] - (R1)[indx - 1], (X0)[indx - 1] - (X1)[indx - 1]), 3.0);
			IOffset = (i - 1) * Fnphases;
			joffset = (j - 1) * Fnphases;
			for(stop2 = Fnphases, II = 1; II <= stop2; II++)
			{
				int stop3 = 0;
				for(stop3 = II, jj = 1; jj <= stop3; jj++)
				{
					if(II == jj)
						Z->SetElement(II + IOffset, jj + joffset, Zs);
					else
					{
						Z->SetElement(II + IOffset, jj + joffset, ZM);
						Z->SetElement(jj + IOffset, II + joffset, ZM);  // set other offdiagonal in this submatrix
					}
				}
			}
		}
	}

   // Voltage source properties
	switch(Fnphases)
	{
		case 	1:
		Vmag = kVBase * PerUnit * 1000.0;
		break;
		default:
		Vmag = kVBase * PerUnit * 1000.0 / 2.0 / sin((180.0 / Fnphases) * DSSGlobals::PI / 180.0L);
		break;
	}
	SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
	if(SpectrumObj == nullptr)
	{
		DoSimpleMsg(String("Spectrum Object \"") + Spectrum
	           + "\" for Device Equivalent."
	           + get_Name()
	           + " Not Found.", 802);
	}
	InjCurrent = (pComplexArray)realloc(InjCurrent, sizeof(complex) * Yorder);;
	NeedToDoRecalc = false;
}

//----------------------------------------------------------------------------

void TEquivalentObj::CalcYPrim(int ActorID)
{
	complex Value = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;

 // Build only YPrim Series
	int stop = 0;
	if(Get_YprimInvalid(ActorID,0))
	{
		if(YPrim_Series != nullptr)
			delete YPrim_Series;
		YPrim_Series = new TcMatrix(Yorder);
		if(YPrim != nullptr)
			delete YPrim;
		YPrim = new TcMatrix(Yorder);
	}
	else
	{
		YPrim_Series->Clear();
		YPrim->Clear();
	}
	if(NeedToDoRecalc)
		RecalcElementData(ActorID);
	FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
	FreqMultiplier = FYprimFreq / BaseFrequency;

     /* Put in Series RL matrix Adjusted for frequency */
	for(stop = Yorder, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Yorder, j = 1; j <= stop1; j++)
		{
			Value = Z->GetElement(i, j);
			Value.im = Value.im * FreqMultiplier;  /*Modify from base freq*/
			Zinv->SetElement(i, j, Value);
		}
	}
	Zinv->Invert();  /*Invert in place*/
	if(Zinv->InvertError > 0)       /*If error, put in Large series conductance*/
	{
		int stop = 0;
		DoErrorMsg("TEquivalentObj.CalcYPrim", String("Matrix Inversion Error for Equivalent \"") + get_Name()
	           + "\"", "Invalid impedance specified. Replaced with small resistance.", 803);
		Zinv->Clear();
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			Zinv->SetElement(i, i, cmplx(1.0 / EPSILON, 0.0));
		}
	}
	YPrim_Series->CopyFrom(Zinv);
	YPrim->CopyFrom(YPrim_Series);

     /*Now Account for Open Conductors*/
     /*For any conductor that is open, zero out row and column*/
	TDSSCktElement::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

//====================================

void TEquivalentObj::GetVterminalForSource()
{
	int i = 0;
	complex Vharm = {};
	double EquivHarm = 0.0;
	try


  /*This formulation will theoretically handle voltage sources of any number of phases assuming they are
   equally displaced in time.*/
	{
		switch(Fnphases)
		{
			case 	1:
			Vmag = kVBase * PerUnit * 1000.0;
			break;
			default:
			Vmag = kVBase * PerUnit * 1000.0 / 2.0 / sin((180.0 / Fnphases) * DSSGlobals::PI / 180.0L);
			break;
		}
		/*# with ActiveCircuit[ActiveActor].Solution do */
		{
			auto with0 = ActiveCircuit[ActiveActor]->Solution;
			if(with0->IsHarmonicModel)
			{
				int stop = 0;
				EquivHarm = with0->get_FFrequency() / EquivFrequency;
				Vharm = cmulreal(SpectrumObj->GetMult(EquivHarm), Vmag);  // Base voltage for this harmonic
				RotatePhasorDeg(Vharm, EquivHarm, Angle);  // Rotate for phase 1 shift
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					(Vterminal)[i - 1] = Vharm;
					if(i < Fnphases)
						RotatePhasorDeg(Vharm, EquivHarm, -360.0 / Fnphases);
				}
			}
			else
			{
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					(Vterminal)[i - 1] = pdegtocomplex(Vmag, (360.0 + Angle - (i - 1) * 360.0 / Fnphases));
				}
			}
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Error computing Voltages for Equivalent.") + get_Name()
	           + ". Check specification. Aborting.", 804);
		if(In_Redirect)
			Redirect_Abort = true;
	}
}

//====================================

int TEquivalentObj::InjCurrents(int ActorID)
{
	int result = 0;
	GetInjCurrents(InjCurrent, ActorID);

/*This is source injection*/
	result = inherited::InjCurrents(ActorID); // Add into system array
	return result;
}

//====================================

void TEquivalentObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	try
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
     //FOR i := 1 TO (Nterms * NConds) DO Vtemp^[i] := V^[NodeRef^[i]];
     // This is safer    12/7/99
			int stop = 0;
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				(Vterminal)[i - 1] = with0->NodeV[(NodeRef)[i - 1]];
			}
			YPrim->MVmult(Curr, &(Vterminal[0]));
			GetInjCurrents(&(ComplexBuffer[0]), ActorID);  // Get present value of inj currents
      // Add Together  with yprim currents
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				(Curr)[i - 1] = csub((Curr)[i - 1], (ComplexBuffer)[i - 1]);
			}
		}  /*With*/
	}
	catch (std::exception &e)
	{
		DoErrorMsg((String("GetCurrents for Element: ") + get_Name() + "."), (std::string) e.what(), "Inadequate storage allotted for circuit element.", 805);
	}
}


//====================================

void TEquivalentObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	GetVterminalForSource();
	YPrim->MVmult(Curr, &(Vterminal[0])); /*I = Y V*/
	set_ITerminalUpdated(false, ActorID);
}

void TEquivalentObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int j = 0;
	complex C = {};
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
		int stop = 0;
		WriteLn(f);
		{ Write(f, "BaseFrequency="); WriteLn(f, BaseFrequency, 0, 1); }
		{ Write(f, "VMag="); WriteLn(f, Vmag, 0, 2); }
		WriteLn(f, "Z Matrix=");
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = i, j = 1; j <= stop1; j++)
			{
				C = Z->GetElement(i, j);
				{ Write(f, C.re, 0, 3); Write(f, " + j"); Write(f, C.im, 0, 3); }
			}
			WriteLn(f);
		}
	}
}

void TEquivalentObj::InitPropertyValues(int ArrayOffset)
{

/*

    PropertyName[1] := 'terminals';
     PropertyName[2] := 'buses';
     PropertyName[3] := 'basekv';
     PropertyName[4] := 'pu';
     PropertyName[5] := 'angle';
     PropertyName[6] := 'frequency';
     PropertyName[7] := 'phases';
     PropertyName[8] := 'R1';
     PropertyName[9] := 'X1';
     PropertyName[10] := 'R0';
     PropertyName[11] := 'X0';
*/
     /*PropertyValue Allocated in DSSObject.Create*/
	Set_PropertyValue(1,"1");
	Set_PropertyValue(2,GetBus(1));
	Set_PropertyValue(3,"115");
	Set_PropertyValue(4,"1");
	Set_PropertyValue(5,"0");
	Set_PropertyValue(6,"60");
	Set_PropertyValue(7,"3");
	Set_PropertyValue(8,"1.65");
	Set_PropertyValue(9,"6.6");
	Set_PropertyValue(10,"1.9");
	Set_PropertyValue(11,"5.7");
	inherited::InitPropertyValues(NumPropsThisClass);
}

String TEquivalentObj::GetPropertyValue(int Index)
{
	String result;
	switch(Index)
	{
		case 	1:
		break;
		default:
		result = TDSSCktElement::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TEquivalentObj::MakePosSequence(int ActorID)
{
	String s;


/// ????
	s = "Phases=1 ";
	s = s + Format("BasekV=%-.5g ",kVBase / SQRT3);
	s = s + Format("R1=%-.5g ",(R1)[0]);
	s = s + Format("X1=%-.5g ",(X1)[0]);
	Parser[ActorID]->SetCmdString(s);
	Edit(ActorID);
	TDSSCktElement::MakePosSequence(ActorID);
}

int TEquivalentObj::DoTerminalsDef(int n)
{
	int result = 0;
	result = Fnterms;
	if(n != Fnterms)
	{
		if(n > 0)
			ReallocRX();
	}
	return result;
}

// Parse input string as an array

void TEquivalentObj::ParseDblMatrix(pDoubleArray Mat)
{
	Parser[ActiveActor]->ParseAsSymMatrix(Fnterms, Mat);
}

void TEquivalentObj::ReallocRX()
{
	R1 = (pDoubleArray)realloc(R1, sizeof(double) * Sqr(Fnterms));
	X1 = (pDoubleArray)realloc(X1, sizeof(double) * Sqr(Fnterms));
	R0 = (pDoubleArray)realloc(R0, sizeof(double) * Sqr(Fnterms));
	X0 = (pDoubleArray)realloc(X0, sizeof(double) * Sqr(Fnterms));
}
//  routine expecting all winding connections expressed in one array of strings

void TEquivalent::InterpretAllBuses(const String s)
{
	String S1;
	String BusNam;
	int i = 0;
	AuxParser[ActiveActor]->SetCmdString(s);  // Load up Parser

    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	/*# with ActiveEquivalentObj do */
	{
		auto with0 = ActiveEquivalentObj;
		int stop = 0;
		for(stop = with0->Fnterms, i = 1; i <= stop; i++)
		{
			S1 = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
			BusNam = AuxParser[ActiveActor]->MakeString_();
			if(BusNam.size() > 0)
				with0->SetBus(i, BusNam);
		}
	}
}




}  // namespace Equivalent




