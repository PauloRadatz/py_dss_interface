
#pragma hdrstop

#include "CableData.h"
#include "ParserDel.h"
#include "DSSGlobals.h"
#include "DSSClassDefs.h"

#include "Ucomplex.h"
#include "Arraydef.h"
#include "LineUnits.h"

using namespace std;
using namespace Arraydef;
using namespace ConductorData;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace LineUnits;
using namespace ParserDel;
using namespace System;
using namespace Ucomplex;

namespace CableData
{

TCableDataObj::TCableDataObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TCableDataObj::TCableDataObj(String ClassName) : inherited(ClassName) {}
TCableDataObj::TCableDataObj() {}

  // Creates superstructure for all Line objects

TCableData::TCableData()
{
	;
	DSSClassType = DSS_OBJECT;
}

TCableData::~TCableData()
{
	// inherited::Destroy();
}


void TCableData::CountProperties()
{
	NumProperties = NumProperties + NumCableClassProps;
	inherited::CountProperties();
}

void TCableData::DefineProperties()
{
	(PropertyName)[ActiveProperty + 1] = "EpsR";
	(PropertyName)[ActiveProperty + 2] = "InsLayer";
	(PropertyName)[ActiveProperty + 3] = "DiaIns";
	(PropertyName)[ActiveProperty + 4] = "DiaCable";
	(PropertyHelp)[ActiveProperty + 1] = "Insulation layer relative permittivity; default is 2.3.";
	(PropertyHelp)[ActiveProperty + 2] = "Insulation layer thickness; same units as radius; no default. "
	           "With DiaIns, establishes inner radius for capacitance calculation.";
	(PropertyHelp)[ActiveProperty + 3] = "Diameter over insulation layer; same units as radius; no default. "
	           "Establishes outer radius for capacitance calculation.";
	(PropertyHelp)[ActiveProperty + 4] = "Diameter over cable; same units as radius; no default.";
	ActiveProperty = ActiveProperty + NumCableClassProps;
	inherited::DefineProperties();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TCableData::ClassEdit(const void* activeObj, int ParamPointer)
{
	int result = 0;
	result = 0;
  // continue parsing with contents of Parser
	if(ParamPointer > 0)
		/*# with TCableDataObj(activeObj) do */
		{
			auto with0 = ((TCableDataObj*) activeObj);
			switch(ParamPointer)
			{
				case 	1:
				with0->FEpsR = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	2:
				with0->FInsLayer = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	3:
				with0->FDiaIns = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	4:
				with0->FDiaCable = Parser[ActiveActor]->MakeDouble_();
				break;
				default:
				inherited::ClassEdit(activeObj, ParamPointer - NumCableClassProps);
				break;
			}
      /*Check for critical errors*/
			switch(ParamPointer)
			{
				case 	1:
				if(with0->FEpsR < 1.0)
					DoSimpleMsg(String("Error: Insulation permittivity must be greater than one for CableData ") + with0->get_Name(), 999);
				break;
				case 	2:
				if(with0->FInsLayer <= 0.0)
					DoSimpleMsg(String("Error: Insulation layer thickness must be positive for CableData ") + with0->get_Name(), 999);
				break;
				case 	3:
				if(with0->FDiaIns <= 0.0)
					DoSimpleMsg(String("Error: Diameter over insulation layer must be positive for CableData ") + with0->get_Name(), 999);
				break;
				case 	4:
				if(with0->FDiaCable <= 0.0)
					DoSimpleMsg(String("Error: Diameter over cable must be positive for CableData ") + with0->get_Name(), 999);
				break;
				default:
				  ;
				break;
			}
		}
	return result;
}

void TCableData::ClassMakeLike(const void* OtherObj)
{
	TCableDataObj* OtherCableData = nullptr;
	OtherCableData = ((TCableDataObj*) OtherObj);
	/*# with TCableDataObj(ActiveDSSObject[ActiveActor]) do */
	{
		auto with0 = ((TCableDataObj*) ActiveDSSObject[ActiveActor]);
		with0->FEpsR = OtherCableData->FEpsR;
		with0->FInsLayer = OtherCableData->FInsLayer;
		with0->FDiaIns = OtherCableData->FDiaIns;
		with0->FDiaCable = OtherCableData->FDiaCable;
	}
	inherited::ClassMakeLike(OtherObj);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TConductorData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TCableDataObj::TCableDataObj(TDSSClass* ParClass, const String CableDataName)
 : inherited(ParClass, CableDataName),
			FEpsR(2.3),
			FInsLayer(-1.0),
			FDiaIns(-1.0),
			FDiaCable(-1.0)
{
	Set_Name(LowerCase(CableDataName));
	DSSObjType = ParClass->DSSClassType;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


TCableDataObj::~TCableDataObj()
{
	// inherited::Destroy();
}


void TCableDataObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = NumCableClassProps, i = 1; i <= stop; i++)
		{
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[GetNumProperties(0) - NumCableClassProps + i - 1]); System::Write(f, L'='); }
			switch(i)
			{
				case 	1:
				System::WriteLn(f, Format("%.3g",FEpsR));
				break;
				case 	2:
				System::WriteLn(f, Format("%.6g",FInsLayer));
				break;
				case 	3:
				System::WriteLn(f, Format("%.6g",FDiaIns));
				break;
				case 	4:
				System::WriteLn(f, Format("%.6g",FDiaCable));
				break;
				default:
				  ;
				break;
			}
		}
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

double TCableDataObj::get_FEpsR()
{
	return FEpsR;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

double TCableDataObj::get_FDiaIns()
{
	return FDiaIns;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

double TCableDataObj::get_FDiaCable()
{
	return FDiaCable;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

double TCableDataObj::get_FInsLayer()
{
	return FInsLayer;
}


String TCableDataObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	1:
		result = Format("%.3g", FEpsR);
		break;
		case 	2:
		result = Format("%.6g", FInsLayer);
		break;
		case 	3:
		result = Format("%.6g", FDiaIns);
		break;
		case 	4:
		result = Format("%.6g", FDiaCable);
		break;
		default:
		result = inherited::GetPropertyValue(Index - NumCableClassProps);
		break;
	}
	return result;
}

int TCableDataObj::GetNumProperties(int ArrayOffset)
{
	return NumCableClassProps + ArrayOffset;
}

void TCableDataObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(ArrayOffset + 1,"2.3");
	Set_PropertyValue(ArrayOffset + 2,"-1");
	Set_PropertyValue(ArrayOffset + 3,"-1");
	Set_PropertyValue(ArrayOffset + 4,"-1");
	inherited::InitPropertyValues(ArrayOffset + 4);
}




}  // namespace CableData





