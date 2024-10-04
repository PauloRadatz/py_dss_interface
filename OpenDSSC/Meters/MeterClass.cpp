
#pragma hdrstop

#include "MeterClass.h"
#include "MeterElement.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"

using namespace std;
using namespace CktElementClass;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace MeterElement;
using namespace ParserDel;
using namespace System;

namespace MeterClass
{



	TMeterClass::TMeterClass()
	 : NumMeterClassProps(0)
	{
		;
		DSSClassType = METER_ELEMENT;
	}

	TMeterClass::~TMeterClass()
	{
		// inherited::Destroy();
	}


	void TMeterClass::CountProperties()
	{
		NumProperties = NumProperties + NumMeterClassProps;
		inherited::CountProperties();
	}

	// Define the properties for the base power delivery element class

	void TMeterClass::DefineProperties()
	{

	   // no properties
		 // PropertyName^[ActiveProperty + 1] := 'propname';
		 // PropertyHelp^[ActiveProperty + 1] := 'prop help';
		ActiveProperty = ActiveProperty + NumMeterClassProps;
		inherited::DefineProperties();
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	int TMeterClass::ClassEdit(const void* ActiveMeterObj, int ParamPointer)
	{
		int result = 0;
		result = 0;
	  // continue parsing with contents of Parser
		if(ParamPointer > 0)
			/*# with TMeterElement(ActiveMeterObj) do */
			{
				auto with0 = ((TMeterElement*) ActiveMeterObj);

		  //CASE ParamPointer OF
		   //1: BaseFrequency := Parser.Dblvalue;
		   //ELSE

		  //END;
				inherited::ClassEdit(ActiveMeterObj, ParamPointer - NumMeterClassProps);
			}
		return result;
	}

	//Var
	//   OtherMeterObj : TMeterElement;

	void TMeterClass::ClassMakeLike(const void* OtherObj)
	{


	//     OtherMeterObj := TMeterElement(OtherObj);
		TMeterElement((TDSSClass*) OtherObj);
		 //With TPCElement(ActiveDSSObject) Do
		 //Begin
		 //  value:= OtherMeterObj.value;
		 //End;
	}

	void TMeterClass::ResetAll(int ActorID)
	{
		DoSimpleMsg(String("Programming Error: Base MeterClass.ResetAll Reached for Class: ") + get_myClass_name(), 760);
	}

	void TMeterClass::SampleAll(int ActorID)
	{
		DoSimpleMsg(String("Programming Error: Base MeterClass.SampleAll Reached for Class: ") + get_myClass_name(), 761);
	}

	void TMeterClass::SaveAll(int ActorID)
	{
		DoSimpleMsg(String("Programming Error: Base MeterClass.SaveAll Reached for Class: ") + get_myClass_name(), 762);
	}




}  // namespace MeterClass





