
#pragma hdrstop

#include "ControlClass.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"

using namespace std;
using namespace CktElementClass;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace ParserDel;
using namespace System;

namespace ControlClass
{



TControlClass::TControlClass()
 : NumControlClassProps(0)
{
	;
	DSSClassType = CTRL_ELEMENT;
}

TControlClass::~TControlClass()
{
	// inherited::Destroy();
}


void TControlClass::CountProperties()
{
	NumProperties = NumProperties + NumControlClassProps;
	inherited::CountProperties();
}

// Define the properties for the base power delivery element class

void TControlClass::DefineProperties()
{

   // no properties
     // PropertyName^[ActiveProperty + 1] := 'propname';
     // PropertyHelp^[ActiveProperty + 1] := 'prop help';
	ActiveProperty = ActiveProperty + NumControlClassProps;
	inherited::DefineProperties();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TControlClass::ClassEdit(const void* ActiveControlObj, int ParamPointer)
{
	int result = 0;
	result = 0;
  // continue parsing with contents of Parser
	if(ParamPointer > 0)
		/*# with TControlElem(ActiveControlObj) do */
		{
			auto with0 = ((TControlElem*) ActiveControlObj);

      //CASE ParamPointer OF
       //1: BaseFrequency := Parser.Dblvalue;
       //ELSE

      //END;
			inherited::ClassEdit(ActiveControlObj, ParamPointer - NumControlClassProps);
		}
	return result;
}

//Var
//   OtherControlObj : TControlElem;

void TControlClass::ClassMakeLike(const void* OtherObj)
{


//   OtherControlObj := TControlElem(OtherObj);
	TControlElem(((TDSSClass*) OtherObj));

     //With TPCElement(ActiveDSSObject) Do
     //Begin
     //  value:= OtherControlObj.value;
     //End;
}




}  // namespace ControlClass





