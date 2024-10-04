
#pragma hdrstop

#include "CNData.h"
#include "ParserDel.h"
#include "DSSGlobals.h"
#include "DSSClassDefs.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "LineUnits.h"

using namespace std;
using namespace Arraydef;
using namespace CableData;
using namespace Command;
using namespace ConductorData;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace LineUnits;
using namespace ParserDel;
using namespace System;
using namespace Ucomplex;

namespace CNData
{

TCNDataObj::TCNDataObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TCNDataObj::TCNDataObj(String ClassName) : inherited(ClassName) {}
TCNDataObj::TCNDataObj() {}


const int NumPropsThisClass = 4;  // Creates superstructure for all Line objects

TCNData::TCNData()
{
	;
	Class_Name = "CNData";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

TCNData::~TCNData()
{
	// inherited::Destroy();
}


void TCNData::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();
	(PropertyName)[1 - 1] = "k";
	(PropertyName)[2 - 1] = "DiaStrand";
	(PropertyName)[3 - 1] = "GmrStrand";
	(PropertyName)[4 - 1] = "Rstrand";
	(PropertyHelp)[1 - 1] = "Number of concentric neutral strands; default is 2";
	(PropertyHelp)[2 - 1] = "Diameter of a concentric neutral strand; same units as core conductor radius; no default.";
	(PropertyHelp)[3 - 1] = "Geometric mean radius of a concentric neutral strand; same units as core conductor GMR; defaults to 0.7788 * CN strand radius.";
	(PropertyHelp)[4 - 1] = "AC resistance of a concentric neutral strand; same units as core conductor resistance; no default.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

int TCNData::NewObject(const String ObjName)
{
	int result = 0;
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TCNDataObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

int TCNData::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveConductorDataObj = (TConductorDataObj*) ElementList.Get_Active();
	ActiveDSSObject[ActiveActor] = ActiveConductorDataObj;
	/*# with TCNDataObj(ActiveConductorDataObj) do */
	{
		auto with0 = ((TCNDataObj*) ActiveConductorDataObj);
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
	           + "\"", 101);
				break;
				case 	1:
				with0->FkStrand = Parser[ActorID]->MakeInteger_();
				break;
				case 	2:
				with0->FDiaStrand = Parser[ActorID]->MakeDouble_();
				break;
				case 	3:
				with0->FGmrStrand = Parser[ActorID]->MakeDouble_();
				break;
				case 	4:
				with0->FRStrand = Parser[ActorID]->MakeDouble_();
				break;
        // Inherited parameters
				default:
				ClassEdit(ActiveConductorDataObj, ParamPointer - NumPropsThisClass);
				break;
			}

      /*Set defaults*/
			switch(ParamPointer)
			{
				case 	2:
				if(with0->FGmrStrand <= 0.0)
					with0->FGmrStrand = 0.7788 * 0.5 * with0->FDiaStrand;
				break;
				default:
				  ;
				break;
			}

      /*Check for critical errors*/
			switch(ParamPointer)
			{
				case 	1:
				if(with0->FkStrand < 2)
					DoSimpleMsg(String("Error: Must have at least 2 concentric neutral strands for CNData ") + with0->get_Name(), 999);
				break;
				case 	2:
				if(with0->FDiaStrand <= 0.0)
					DoSimpleMsg(String("Error: Neutral strand diameter must be positive for CNData ") + with0->get_Name(), 999);
				break;
				case 	3:
				if(with0->FGmrStrand <= 0.0)
					DoSimpleMsg(String("Error: Neutral strand GMR must be positive for CNData ") + with0->get_Name(), 999);
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

int TCNData::MakeLike(const String CNName)
{
	int result = 0;
	TCNDataObj* OtherData = nullptr;
	int i = 0;
	result = 0;
	OtherData = ((TCNDataObj*) Find(CNName));
	if(OtherData != nullptr)
		/*# with TCNDataObj(ActiveConductorDataObj) do */
		{
			auto with0 = ((TCNDataObj*) ActiveConductorDataObj);
			int stop = 0;
			with0->FkStrand = OtherData->FkStrand;
			with0->FDiaStrand = OtherData->FDiaStrand;
			with0->FGmrStrand = OtherData->FGmrStrand;
			with0->FRStrand = OtherData->FRStrand;
			ClassMakeLike(OtherData);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherData->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Concentric Neutral MakeLike: \"") + CNName
	           + "\" Not Found.", 102);
	return result;
}

int TCNData::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TCNData.Init", -1);
	result = 0;
	return result;
}  // Returns active line code string

String TCNData::Get_Code()
{
	String result;
	result = ((TCNDataObj*) ElementList.Get_Active())->get_Name();
	return result;
}  // sets the  active CNData

void TCNData::Set_Code(const String Value)
{
	TCNDataObj* CNDataObj = nullptr;
	ActiveConductorDataObj = nullptr;
	CNDataObj = (TCNDataObj*) ElementList.Get_First();
	while(CNDataObj != nullptr)
	{
		if(CompareText(CNDataObj->get_Name(), Value) == 0)
		{
			ActiveConductorDataObj = CNDataObj;
			return;
		}
		CNDataObj = (TCNDataObj*) ElementList.Get_Next();
	}
	DoSimpleMsg(String("CNData: \"") + Value + "\" not Found.", 103);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TCNData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TCNDataObj::TCNDataObj(TDSSClass* ParClass, const String CNDataName)
 : inherited(ParClass, CNDataName),
			FkStrand(2),
			FDiaStrand(-1.0),
			FGmrStrand(-1.0),
			FRStrand(-1.0)
{
	Set_Name(LowerCase(CNDataName));
	DSSObjType = ParClass->DSSClassType;
	InitPropertyValues(0);
}

TCNDataObj::~TCNDataObj()
{
	// inherited::Destroy();
}

//-------------------------------------------------------------------------------------------

int TCNDataObj::get_FkStrand()
{
	return FkStrand;
}

//-------------------------------------------------------------------------------------------

double TCNDataObj::get_FDiaStrand()
{
	return FDiaStrand;
}

//-------------------------------------------------------------------------------------------

double TCNDataObj::get_FGmrStrand()
{
	return FGmrStrand;
}

//-------------------------------------------------------------------------------------------

double TCNDataObj::get_FRStrand()
{
	return FRStrand;
}

//-------------------------------------------------------------------------------------------


void TCNDataObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = NumPropsThisClass, i = 1; i <= stop; i++)
		{
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[i - 1]); System::Write(f, L'='); }
			switch(i)
			{
				case 	1:
				System::WriteLn(f, Format("%d", FkStrand));
				break;
				case 	2:
				System::WriteLn(f, Format("%.6g", FDiaStrand));
				break;
				case 	3:
				System::WriteLn(f, Format("%.6g", FGmrStrand));
				break;
				case 	4:
				System::WriteLn(f, Format("%.6g", FRStrand));
				break;
				default:
				  ;
				break;
			}
		}
	}
}

String TCNDataObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	1:
		result = Format("%d", FkStrand);
		break;
		case 	2:
		result = Format("%.6g", FDiaStrand);
		break;
		case 	3:
		result = Format("%.6g", FGmrStrand);
		break;
		case 	4:
		result = Format("%.6g", FRStrand);
		break;
		default:
		result = inherited::GetPropertyValue(Index - NumPropsThisClass);
		break;
	}
	return result;
}

int TCNDataObj::GetNumProperties(int ArrayOffset)
{
	return inherited::GetNumProperties(NumPropsThisClass + ArrayOffset);
}

void TCNDataObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"2");
	Set_PropertyValue(2,"-1");
	Set_PropertyValue(3,"-1");
	Set_PropertyValue(4,"-1");
	inherited::InitPropertyValues(ArrayOffset + NumPropsThisClass);
}




}  // namespace CNData





