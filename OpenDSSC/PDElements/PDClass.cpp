
#pragma hdrstop

#include "PDClass.h"

#include "PDElement.h"
#include "DSSGlobals.h"
#include "Utilities.h"


using namespace std;
using namespace CktElementClass;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace PDELement;
using namespace ParserDel;
using namespace System;
using namespace Utilities;

namespace PDClass
{



TPDClass::TPDClass()
 : NumPDClassProps(5)
{
	;
	DSSClassType = PD_ELEMENT;
}


TPDClass::~TPDClass()
{
	// inherited::Destroy();
}


void TPDClass::CountProperties()
{
	NumProperties = NumProperties + NumPDClassProps;
	inherited::CountProperties();
}

// Define the properties for the base power delivery element class

void TPDClass::DefineProperties()
{
	PropertyName[ActiveProperty + 1] = "normamps";
	PropertyName[ActiveProperty + 2] = "emergamps";
	PropertyName[ActiveProperty + 3] = "faultrate";
	PropertyName[ActiveProperty + 4] = "pctperm";
	PropertyName[ActiveProperty + 5] = "repair";
	PropertyHelp[ActiveProperty + 1] = "Normal rated current.";
	PropertyHelp[ActiveProperty + 2] = "Maximum or emerg current.";
	PropertyHelp[ActiveProperty + 3] = "Failure rate per year.";
	PropertyHelp[ActiveProperty + 4] = "Percent of failures that become permanent.";
	PropertyHelp[ActiveProperty + 5] = "Hours to repair.";
	ActiveProperty = ActiveProperty + NumPDClassProps;
	inherited::DefineProperties();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TPDClass::ClassEdit(const void* ActivePDObj, int ParamPointer)
{
	int result = 0;
	result = 0;
  // continue parsing with contents of Parser
	if(ParamPointer > 0)
		/*# with TPDElement(ActivePDObj) do */
		{
			auto with0 = ((TPDElement*) ActivePDObj);
			switch(ParamPointer)
			{
				case 	1:
				with0->NormAmps = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	2:
				with0->EmergAmps = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	3:
				with0->FaultRate = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	4:
				with0->PctPerm = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	5:
				with0->HrsToRepair = Parser[ActiveActor]->MakeDouble_();
				break;
				default:
				inherited::ClassEdit(ActivePDObj, ParamPointer - NumPDClassProps);
				break;
			}
		}
	return result;
}

void TPDClass::ClassMakeLike(const void* OtherObj)
{
	TPDElement* OtherPDObj = nullptr;
	OtherPDObj = ((TPDElement*) OtherObj);
	/*# with TPDElement(ActiveDSSObject[ActiveActor]) do */
	{
		auto with0 = ((TPDElement*) ActiveDSSObject[ActiveActor]);
		with0->NormAmps = OtherPDObj->NormAmps;
		with0->EmergAmps = OtherPDObj->EmergAmps;
		with0->FaultRate = OtherPDObj->FaultRate;
		with0->PctPerm = OtherPDObj->PctPerm;
		with0->HrsToRepair = OtherPDObj->HrsToRepair;
	}
	inherited::ClassMakeLike(OtherObj);
}




}  // namespace PDClass




