
#pragma hdrstop

#include "LineSpacing.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Ucomplex.h"
#include "Utilities.h"
#include "LineUnits.h"

using namespace std;

using namespace Arraydef;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace LineUnits;
using namespace ParserDel;
using namespace System;
using namespace Ucomplex;
using namespace Utilities;

namespace LineSpacing
{

TLineSpacingObj::TLineSpacingObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TLineSpacingObj::TLineSpacingObj(String ClassName) : inherited(ClassName) {}
TLineSpacingObj::TLineSpacingObj() {}


TLineSpacingObj* ActiveLineSpacingObj = nullptr;
const int NumPropsThisClass = 5;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TLineSpacing::TLineSpacing()
{
	;
	Class_Name = "LineSpacing";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineSpacing::~TLineSpacing()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLineSpacing::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();
	(PropertyName)[1 - 1] = "nconds";
	(PropertyName)[2 - 1] = "nphases";
	(PropertyName)[3 - 1] = "x";
	(PropertyName)[4 - 1] = "h";
	(PropertyName)[5 - 1] = "units";
	(PropertyHelp)[1 - 1] = "Number of wires in this geometry. Default is 3. Triggers memory allocations. Define first!";
	(PropertyHelp)[2 - 1] = "Number of retained phase conductors. If less than the number of wires, list the retained phase coordinates first.";
	(PropertyHelp)[3 - 1] = "Array of wire X coordinates.";
	(PropertyHelp)[4 - 1] = "Array of wire Heights.";
	(PropertyHelp)[5 - 1] = "Units for x and h: {mi|kft|km|m|Ft|in|cm } Initial default is \"ft\", but defaults to last unit defined";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineSpacing::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] =  new TLineSpacingObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

void TLineSpacing::InterpretArray(const String s, SpcParmChoice Which)
{
	String Str;
	int i = 0;
	AuxParser[ActiveActor]->SetCmdString(s);
	/*# with ActiveLineSpacingObj do */
	{
		auto with0 = ActiveLineSpacingObj;
		int stop = 0;
		for(stop = with0->get_Fnconds(), i = 1; i <= stop; i++)
		{
			String dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
			Str = AuxParser[ActiveActor]->MakeString_();
			if(Str.length() > 0)
				switch(Which)
				{
					case 	X:
					(with0->FX)[i - 1] = AuxParser[ActiveActor]->MakeDouble_();
					break;
					case 	h:
					(with0->FY)[i - 1] = AuxParser[ActiveActor]->MakeDouble_();
					break;
					default:
					  ;
					break;
				}
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineSpacing::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveLineSpacingObj = ((TLineSpacingObj*) ElementList.Get_Active());
	ActiveDSSObject[ActorID] = ActiveLineSpacingObj;
	/*# with ActiveLineSpacingObj do */
	{
		auto with0 = ActiveLineSpacingObj;
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while(Param.length() > 0)
		{
			if(ParamName.length() == 0)
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
	           + "\"", 10101);
				break;
				case 	1:
				with0->set_Nwires(Parser[ActorID]->MakeInteger_());
				break;  // Use property value to force reallocations
				case 	2:
				with0->Fnphases = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				InterpretArray(Param, X);
				break;
				case 	4:
				InterpretArray(Param, h);
				break;
				case 	5:
				with0->FUnits = GetUnitsCode(Param);
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveLineSpacingObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 1: case 2: case 3: case 4: case 5:
				with0->DataChanged = true;
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineSpacing::MakeLike(const String LineName)
{
	int result = 0;
	TLineSpacingObj* OtherLineSpacing = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line code in the present collection*/
	OtherLineSpacing = ((TLineSpacingObj*) Find(LineName));
	if(OtherLineSpacing != nullptr)
		/*# with ActiveLineSpacingObj do */
		{
			auto with0 = ActiveLineSpacingObj;
			int stop = 0;
			with0->set_Nwires(OtherLineSpacing->get_Fnconds());   // allocates
			with0->Fnphases = OtherLineSpacing->get_Fnphases();
			for(stop = with0->Fnconds, i = 1; i <= stop; i++)
			{
				(with0->FX)[i - 1] = (OtherLineSpacing->FX)[i - 1];
			}
			for(stop = with0->Fnconds, i = 1; i <= stop; i++)
			{
				(with0->FY)[i - 1] = (OtherLineSpacing->FY)[i - 1];
			}
			with0->FUnits = OtherLineSpacing->FUnits;
			with0->DataChanged = true;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherLineSpacing->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in LineSpacing MakeLike: \"") + LineName
	           + "\" Not Found.", 102);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineSpacing::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TLineSpacing.Init", -1);
	result = 0;
	return result;
}  // Returns active line code string

String TLineSpacing::Get_Code()
{
	String result;
	result = ((TLineSpacingObj*) ElementList.Get_Active())->get_Name();
	return result;
}  // sets the  active LineSpacing

void TLineSpacing::Set_Code(const String Value)
{
	TLineSpacingObj* LineSpacingObj = nullptr;
	ActiveLineSpacingObj = nullptr;
	LineSpacingObj = ((TLineSpacingObj*) ElementList.Get_First());
	while(LineSpacingObj != nullptr)
	{
		if(CompareText(LineSpacingObj->get_Name(), Value) == 0)
		{
			ActiveLineSpacingObj = LineSpacingObj;
			return;
		}
		LineSpacingObj = ((TLineSpacingObj*) ElementList.Get_Next());
	}
	DoSimpleMsg(String("LineSpacing: \"") + Value + "\" not Found.", 103);
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineSpacing Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineSpacingObj::TLineSpacingObj(TDSSClass* ParClass, const String LineSpacingName)
 : inherited(ParClass),
			Fnconds(0),
			Fnphases(0)
{
	Set_Name(LowerCase(LineSpacingName));
	DSSObjType = ParClass->DSSClassType;
	DataChanged = true;
	FX = nullptr;
	FY = nullptr;
	FUnits = UNITS_FT;
	set_Nwires(3);  // Allocates terminals
	Fnphases = 3;
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineSpacingObj::~TLineSpacingObj()
{
	free(FY);
	free(FX);
	// inherited::Destroy();
}


void TLineSpacingObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = 5, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, GetPropertyValue(i)); }
		}
	}
}

String ArrayString(pDoubleArray PF, int n)
{
	String result;
	int i = 0;
	String R;
	int stop = 0;
	R = "[";
	if(n > 0)
		R = R + Format("%-g", PF[1 - 1]);
	for(stop = n, i = 2; i <= stop; i++)
	{
		R = R + Format(",%-g", PF[i - 1]);
	}
	result = R + "]";
	return result;
}

/*Return Property Value for Active index*/

String TLineSpacingObj::GetPropertyValue(int Index)
{
	String result;
	switch(Index)
	{
		case 	3:
		result = ArrayString(FX, Fnconds);
		break;
		case 	4:
		result = ArrayString(FY, Fnconds);
		break;
		case 	5:
		result = LineUnitsStr(FUnits);
		break;
     // Inherited parameters
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

double TLineSpacingObj::Get_FX(int i)
{
	double result = 0.0;
	if(i <= Fnconds)
		result = FX[i - 1];
	else
		result = 0.0;
	return result;
}

double TLineSpacingObj::Get_FY(int i)
{
	double result = 0.0;
	if(i <= Fnconds)
		result = FY[i - 1];
	else
		result = 0.0;
	return result;
}

//------------------------------------------------------------------------------------

int TLineSpacingObj::get_Fnconds()
{
	return Fnconds;
}

//------------------------------------------------------------------------------------

int TLineSpacingObj::get_Fnphases()
{
	return Fnphases;
}

//------------------------------------------------------------------------------------

int TLineSpacingObj::get_FUnits()
{
	return FUnits;
}

//------------------------------------------------------------------------------------

void TLineSpacingObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"3");
	Set_PropertyValue(2,"3");
	Set_PropertyValue(3,"0");
	Set_PropertyValue(4,"32");
	Set_PropertyValue(5,"ft");
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TLineSpacingObj::set_Nwires(int Value)
{
	int ncondsPrev = Fnconds;
	Fnconds = Value;
	FX = (pDoubleArray) realloc(FX, sizeof(double) * Fnconds);
	FY = (pDoubleArray) realloc(FY, sizeof(double) * Fnconds);
	FUnits = UNITS_FT;
	for (int i = ncondsPrev; i < Fnconds; ++i)
	{
		FX[i] = 0.0;
		FY[i] = 0.0;
	}
}




}  // namespace LineSpacing





