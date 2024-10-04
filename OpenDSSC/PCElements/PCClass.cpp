
#pragma hdrstop

#include "PCClass.h"

#include "DSSGlobals.h"
#include "Utilities.h"
#include "PCElement.h"

using namespace std;
using namespace CktElementClass;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace Utilities;

namespace PCClass
{



TPCClass::TPCClass()
 : NumPCClassProps(1)
{
	;
	DSSClassType = PC_ELEMENT;
}

TPCClass::~TPCClass()
{
	// inherited::Destroy();
}


void TPCClass::CountProperties()
{
	NumProperties = NumProperties + NumPCClassProps;
	inherited::CountProperties();
}

// Define the properties for the base power delivery element class

void TPCClass::DefineProperties()
{
	PropertyName[ActiveProperty + 1] = "spectrum";
	PropertyHelp[ActiveProperty + 1] = "Name of harmonic spectrum for this device.";
	ActiveProperty = ActiveProperty + NumPCClassProps;
	inherited::DefineProperties();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TPCClass::ClassEdit(const void* ActivePCObj, int ParamPointer)
{
	int result = 0;
	result = 0;
  // continue parsing with contents of Parser
	if(ParamPointer > 0)
		/*# with TPCElement(ActivePCObj) do */
		{
			auto with0 = ((TPCElement*) ActivePCObj);
			switch(ParamPointer)
			{
				case 	1:
				with0->Spectrum = Parser[ActiveActor]->MakeString_();
				break;
				default:
				inherited::ClassEdit(ActivePCObj, ParamPointer - NumPCClassProps);
				break;
			}
		}
	return result;
}

void TPCClass::ClassMakeLike(const void* OtherObj)
{
	TPCElement* OtherPCObj = nullptr;
	OtherPCObj = ((TPCElement*) OtherObj);
	/*# with TPCElement(ActiveDSSObject[ActiveActor]) do */
	{
		auto with0 = ((TPCElement*) ActiveDSSObject[ActiveActor]);
		with0->Spectrum = OtherPCObj->Spectrum;
		with0->SpectrumObj = OtherPCObj->SpectrumObj;
	}
	inherited::ClassMakeLike(OtherObj);
}




}  // namespace PCClass




