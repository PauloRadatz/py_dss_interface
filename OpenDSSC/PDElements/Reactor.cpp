
#pragma hdrstop

#include "Reactor.h"

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
using namespace XYCurve;
using namespace mathutil;
using namespace Utilities;

namespace Reactor
{

TReactorObj::TReactorObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TReactorObj::TReactorObj(String ClassName) : inherited(ClassName) {}
TReactorObj::TReactorObj() {}


TReactorObj* ActiveReactorObj = nullptr;
const int NumPropsThisClass = 19;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Reactor objects

TReactor::TReactor()
{
	;
	Class_Name = "Reactor";
	DSSClassType = DSSClassType + REACTOR_ELEMENT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);

	ReactorClass[ActiveActor] = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TReactor::~TReactor()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TReactor::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "bus1";
	PropertyName[2 - 1] = "bus2";
	PropertyName[3 - 1] = "phases";
	PropertyName[4 - 1] = "kvar";
	PropertyName[5 - 1] = "kv";
	PropertyName[6 - 1] = "conn";
	PropertyName[7 - 1] = "Rmatrix";
	PropertyName[8 - 1] = "Xmatrix";
	PropertyName[9 - 1] = "Parallel";
	PropertyName[10 - 1] = "R";
	PropertyName[11 - 1] = "X";
	PropertyName[12 - 1] = "Rp";
	PropertyName[13 - 1] = "Z1";
	PropertyName[14 - 1] = "Z2";
	PropertyName[15 - 1] = "Z0";
	PropertyName[16 - 1] = "Z";
	PropertyName[17 - 1] = "RCurve";
	PropertyName[18 - 1] = "LCurve";
	PropertyName[19 - 1] = "LmH";

     // define Property help values
	PropertyHelp[1 - 1] = String("Name of first bus. Examples:") + CRLF
	           + "bus1=busname"
	           + CRLF
	           + "bus1=busname.1.2.3"
	           + CRLF
	           + CRLF
	           + "Bus2 property will default to this bus, node 0, unless previously specified. "
	           + "Only Bus1 need be specified for a Yg shunt reactor.";
	PropertyHelp[2 - 1] = String("Name of 2nd bus. Defaults to all phases connected " "to first bus, node 0, (Shunt Wye Connection) " "except when Bus2 is specifically defined.") + CRLF
	           + CRLF
	           + "Not necessary to specify for delta (LL) connection";
	PropertyHelp[3 - 1] = "Number of phases.";
	PropertyHelp[4 - 1] = "Total kvar, all phases.  Evenly divided among phases. Only determines X. Specify R separately";
	PropertyHelp[5 - 1] = "For 2, 3-phase, kV phase-phase. Otherwise specify actual coil rating.";
	PropertyHelp[6 - 1] = "={wye | delta |LN |LL}  Default is wye, which is equivalent to LN. If Delta, then only one terminal.";
	PropertyHelp[7 - 1] = "Resistance matrix, lower triangle, ohms at base frequency. Order of the matrix is the number of phases. "
	           "Mutually exclusive to specifying parameters by kvar or X.";
	PropertyHelp[8 - 1] = "Reactance matrix, lower triangle, ohms at base frequency. Order of the matrix is the number of phases. "
	           "Mutually exclusive to specifying parameters by kvar or X.";
	PropertyHelp[9 - 1] = "{Yes | No}  Default=No. Indicates whether Rmatrix and Xmatrix are to be considered in parallel. "
	           "Default is series. For other models, specify R and Rp.";
	PropertyHelp[10 - 1] = "Resistance (in series with reactance), each phase, ohms. "
	           "This property applies to REACTOR specified by either kvar or X. See also help on Z.";
	PropertyHelp[11 - 1] = "Reactance, each phase, ohms at base frequency. See also help on Z and LmH properties.";
	PropertyHelp[12 - 1] = "Resistance in parallel with R and X (the entire branch). Assumed infinite if not specified.";
	PropertyHelp[13 - 1] = String("Positive-sequence impedance, ohms, as a 2-element array representing a complex number. Example: ") + CRLF
	           + CRLF
	           + "Z1=[1, 2]  ! represents 1 + j2 "
	           + CRLF
	           + CRLF
	           + "If defined, Z1, Z2, and Z0 are used to define the impedance matrix of the REACTOR. "
	           + "Z1 MUST BE DEFINED TO USE THIS OPTION FOR DEFINING THE MATRIX."
	           + CRLF
	           + CRLF
	           + "Side Effect: Sets Z2 and Z0 to same values unless they were previously defined.";
	PropertyHelp[14 - 1] = String("Negative-sequence impedance, ohms, as a 2-element array representing a complex number. Example: ") + CRLF
	           + CRLF
	           + "Z2=[1, 2]  ! represents 1 + j2 "
	           + CRLF
	           + CRLF
	           + "Used to define the impedance matrix of the REACTOR if Z1 is also specified. "
	           + CRLF
	           + CRLF
	           + "Note: Z2 defaults to Z1 if it is not specifically defined. If Z2 is not equal to Z1, the impedance matrix is asymmetrical.";
	PropertyHelp[15 - 1] = String("Zer0-sequence impedance, ohms, as a 2-element array representing a complex number. Example: ") + CRLF
	           + CRLF
	           + "Z0=[3, 4]  ! represents 3 + j4 "
	           + CRLF
	           + CRLF
	           + "Used to define the impedance matrix of the REACTOR if Z1 is also specified. "
	           + CRLF
	           + CRLF
	           + "Note: Z0 defaults to Z1 if it is not specifically defined. ";
	PropertyHelp[16 - 1] = String("Alternative way of defining R and X properties. Enter a 2-element array representing R +jX in ohms. Example:") + CRLF
	           + CRLF
	           + "Z=[5  10]   ! equivalent to R=5  X=10 ";
	PropertyHelp[17 - 1] = "Name of XYCurve object, previously defined, describing per-unit variation of phase resistance, R, vs. frequency. Applies to resistance specified by R or Z property. "
	           "If actual values are not known, R often increases by approximately the square root of frequency.";
	PropertyHelp[18 - 1] = "Name of XYCurve object, previously defined, describing per-unit variation of phase inductance, L=X/w, vs. frequency. Applies to reactance specified by X, LmH, Z, or kvar property."
	           "L generally decreases somewhat with frequency above the base frequency, approaching a limit at a few kHz.";
	PropertyHelp[19 - 1] = "Inductance, mH. Alternate way to define the reactance, X, property.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

	PropertyHelp[NumPropsThisClass]		= PropertyHelp[NumPropsThisClass] + " Defaults to per - phase rated current when reactor is specified with rated power and voltage.";
	PropertyHelp[NumPropsThisClass + 1] = PropertyHelp[NumPropsThisClass + 1] + " Defaults to 135 % of per - phase rated current when reactor is specified with rated power and voltage.";
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TReactor::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TReactorObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TReactor::DoMatrix(pDoubleArray Matrix, int ActorID)
{
	int OrderFound = 0;
	int j = 0;
	pDoubleArray MatBuffer = nullptr;
	/*# with ActiveReactorObj do */
	{
		auto with0 = ActiveReactorObj;
		MatBuffer = new double[ with0->Fnphases * with0->Fnphases];
		OrderFound = Parser[ActorID]->ParseAsSymMatrix( ( (TDSSCktElement*) with0 )->Fnphases, MatBuffer);
		if(OrderFound > 0)    // Parse was successful Else don't change Matrix
			    /*X*/
			{
				int stop = 0;
				Matrix = (pDoubleArray) realloc(Matrix, sizeof(double) * with0->Fnphases * with0->Fnphases);
				for(stop = with0->Fnphases * with0->Fnphases, j = 1; j <= stop; j++)
				{
					(Matrix)[j - 1] = (MatBuffer)[j - 1];
				}
			}
		free(MatBuffer);
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN

void TReactor::InterpretConnection(const String s)
{
	String TestS;
	/*# with ActiveReactorObj do */
	{
		auto with0 = ActiveReactorObj;
		TestS = LowerCase(s);
		switch(TestS[0])
		{
			case 	L'y':
			case L'w':
				with0->Connection = 0;
			break;  /*Wye*/
			case 	L'd':
				with0->Connection = 1;
			break;  /*Delta or line-Line*/
			case 	L'l':
			switch(TestS[1])
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
		int myshift = 0;
		switch(with0->Connection)
		{
			case 	1:		// Force reallocation of terminals
			{
				with0->Set_NTerms(1);
				if (with0->Fnphases < 3)
					myshift++;
				with0->Set_Nconds(with0->Fnphases + myshift);
			}
			break;  
			case 	0:
			{
				if (with0->Fnterms != 2)
					with0->Set_NTerms(2);
				with0->Set_Nconds(with0->Fnphases);
			}
			break;
			default:
			  ;
			break;
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TReactor::ReactorSetbus1(const String s)
{
	String S2;
	int i = 0;
	int dotpos = 0;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0
	/*# with ActiveReactorObj do */
	{
		auto with0 = ActiveReactorObj;
		with0->SetBus(1, s);

     // Default Bus2 to zero node of Bus1 if not already defined. (Wye Grounded connection)
		if(!with0->Bus2Defined)
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
			}
			with0->SetBus(2, S2);
			with0->IsShunt = true;
		}
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TReactor::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveReactorObj = (TReactorObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement((TDSSCktElement*) ActiveReactorObj);  // use property to set this value
	/*# with ActiveReactorObj do */
	{
		auto with0 = ActiveReactorObj;
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
	           + ( (TDSSCktElement*) with0 )->get_Name()
	           + "\"", 230);
				break;
				case 	1:
				ReactorSetbus1(Param);
				break;
				case 	2:
				{
					( (TDSSCktElement*) with0 )->SetBus(2, Param);
				}
				break;/* Numphases := Parser.MakeInteger_()*/
				case 	3:
				;
				break;  // see below
				case 	4:
				with0->kvarrating = Parser[ActorID]->MakeDouble_();
				break;
				case 	5:
				with0->kvrating = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				InterpretConnection(Param);
				break;
				case 	7:
				DoMatrix(with0->Rmatrix, ActorID);
				break;
				case 	8:
				DoMatrix(with0->XMatrix, ActorID);
				break;
				case 	9:
				with0->IsParallel = InterpretYesNo(Param);
				break;
				case 	10:
				with0->R = Parser[ActorID]->MakeDouble_();
				break;
				case 	11:
				with0->X = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->Rp = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->Z1 = InterpretComplex(Param);
				break;
				case 	14:
				with0->Z2 = InterpretComplex(Param);
				break;
				case 	15:
				with0->Z0 = InterpretComplex(Param);
				break;
				case 	16:
				with0->Z = InterpretComplex(Param);
				break;
				case 	17:
				with0->RCurve = Param;
				break;
				case 	18:
				with0->LCurve = Param;
				break;
				case 	19:
				with0->l = Parser[ActorID]->MakeDouble_() / 1000.0;
				break;  // convert from mH to H

            // Inherited Property Edits
				default:
				inherited::ClassEdit(ActiveReactorObj, ParamPointer - NumPropsThisClass);
				break;
			}

         // Some specials ...
			switch(ParamPointer)
			{
				case 	1:
				{
					with0->Set_PropertyValue(2,with0->GetBus(2));   // this gets modified
					with0->PrpSequence[2 - 1] = 0;       // Reset this for save function
				}
				break;
				case 	2:
				if(CompareText(StripExtension( with0->GetBus(1)), StripExtension( with0->GetBus(2))) != 0)
				{
					with0->IsShunt = false;
					with0->Bus2Defined = true;
				}
				break;
				case 	3:
				if( with0->Fnphases != Parser[ActorID]->MakeInteger_())
				{
					with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
					int lclshift = 0;
					if (with0->Connection == 1)
						lclshift++;
					with0->Set_Nconds(with0->Fnphases + lclshift);  // Force Reallocation of terminal info
					with0->Yorder		= with0->Fnterms * with0->Fnconds;
				}
				break;
				case 	4:
				with0->SpecType = 1;
				break;   // X specified by kvar, kV
				case 	7: case 8:
				with0->SpecType = 3;
				break;
				case 	11:
				with0->SpecType = 2;
				break;   // X specified directly rather than computed from kvar
				case 	12:
				with0->RpSpecified = true;
				break;
				case 	13:
				{
					with0->SpecType = 4;    // have to set Z1 to get this mode
					if(!with0->Z2Specified)
						with0->Z2 = with0->Z1;
					if(!with0->Z0Specified)
						with0->Z0 = with0->Z1;
				}
				break;
				case 	14:
				with0->Z2Specified = true;
				break;
				case 	15:
				with0->Z0Specified = true;
				break;
				case 	16:
				{
					with0->R = with0->Z.re;
					with0->X = with0->Z.im;
					with0->SpecType = 2;
				}
				break;
				case 	17:
				with0->RCurveObj = ((TXYcurveObj*) XYCurveClass[ActorID]->Find(with0->RCurve));
				break;
				case 	18:
				with0->LCurveObj = ((TXYcurveObj*) XYCurveClass[ActorID]->Find(with0->LCurve));
				break;
				case 	19:
				{
					with0->SpecType = 2;
					with0->X = with0->l * TwoPi * with0->BaseFrequency;
				}
				break;
				case	(NumPropsThisClass + 1):
					with0->FNormAmpsSpecified	= true; // Normamps
				break;
				case	(NumPropsThisClass + 2): 
					with0->FEmergAmpsSpecified	= true; // Emergamps
				break;
				default:
				  ;
				break;
			}

         //YPrim invalidation on anything that changes impedance values
			switch(ParamPointer)
			{
				case 3: case 4: case 5: case 6: case 7: case 8: case 9: case 10: case 11: case 12:
				 case 13: case 14: case 15: case 16:
					 with0->Set_YprimInvalid(ActorID,true);
				break;
				case 	17:
				if(with0->RCurveObj == nullptr)
					DoSimpleMsg(String("Resistance-frequency curve XYCurve.") + with0->RCurve
	           + " not Found.", 2301);
				break;
				case 	18:
				if(with0->LCurveObj == nullptr)
					DoSimpleMsg(String("Inductance-frequency curve XYCurve.") + with0->LCurve
	           + " not Found.", 2301);
				break;
				case 	19:
					( (TDSSCktElement*) with0 )->Set_YprimInvalid(ActorID,true);
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

int TReactor::MakeLike(const String ReactorName)
{
	int result = 0;
	TReactorObj* OtherReactor = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Reactor name in the present collection*/
	OtherReactor = ((TReactorObj*) Find(ReactorName));
	if(OtherReactor != nullptr)
		/*# with ActiveReactorObj do */
		{
			auto with0 = ActiveReactorObj;
			int stop = 0;
			if( ( (TDSSCktElement*) with0 )->Fnphases != ( (TDSSCktElement*) OtherReactor )->Fnphases)
			{
				( (TDSSCktElement*) with0 )->Set_NPhases(( (TDSSCktElement*) OtherReactor )->Fnphases);
				( (TDSSCktElement*) with0 )->Set_Nconds(( (TDSSCktElement*) with0 )->Fnphases); // force reallocation of terminals and conductors
				( (TDSSCktElement*) with0 )->Yorder = ( (TDSSCktElement*) with0 )->Fnconds * ((TDSSCktElement*)with0)->Fnterms;
				( (TDSSCktElement* )with0 )->Set_YprimInvalid(ActiveActor,true);
			}
			with0->R = OtherReactor->R;
			with0->X = OtherReactor->X;
			with0->Rp = OtherReactor->Rp;
			with0->RpSpecified = OtherReactor->RpSpecified;
			with0->IsParallel = OtherReactor->IsParallel;
			with0->kvarrating = OtherReactor->kvarrating;
			with0->kvrating = OtherReactor->kvrating;
			with0->Connection = OtherReactor->Connection;
			with0->SpecType = OtherReactor->SpecType;
			with0->Z = OtherReactor->Z;
			with0->Z1 = OtherReactor->Z1;
			with0->Z2 = OtherReactor->Z2;
			with0->Z0 = OtherReactor->Z0;
			with0->Z2Specified = OtherReactor->Z2Specified;
			with0->Z0Specified = OtherReactor->Z0Specified;
			with0->RCurve = OtherReactor->RCurve;
			with0->RCurveObj = OtherReactor->RCurveObj;
			with0->LCurve = OtherReactor->LCurve;
			with0->LCurveObj = OtherReactor->LCurveObj;
			if(OtherReactor->Rmatrix == nullptr)
				with0->Rmatrix = NULL;
			else
			{
				int stop = 0;
				with0->Rmatrix = (pDoubleArray) realloc(with0->Rmatrix, sizeof(double) * with0->Fnphases * with0->Fnphases);
				for(stop = with0->Fnphases * with0->Fnphases, i = 1; i <= stop; i++)
				{
					(with0->Rmatrix)[i - 1] = (OtherReactor->Rmatrix)[i - 1];
				}
			}
			if(OtherReactor->XMatrix == nullptr)
				with0->XMatrix = NULL;
			else
			{
				int stop = 0;
				with0->XMatrix = (pDoubleArray) realloc(with0->XMatrix, sizeof(double) * with0->Fnphases * with0->Fnphases);
				for(stop = ( (TDSSCktElement*) with0 )->Fnphases * ( (TDSSCktElement*) with0 )->Fnphases, i = 1; i <= stop; i++)
				{
					(with0->XMatrix)[i - 1] = (OtherReactor->XMatrix)[i - 1];
				}
			}
			ClassMakeLike(OtherReactor);  // Take care of inherited class properties
			for(stop = ( (TDSSCktElement*) with0 )->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				( (TDSSCktElement*) with0 )->Set_PropertyValue(i,( (TDSSCktElement*) OtherReactor )->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Reactor MakeLike: \"") + ReactorName
	           + "\" Not Found.", 231);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TReactor::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TReactor.Init", -1);
	result = 0;
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TReactor Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TReactorObj::TReactorObj(TDSSClass* ParClass, const String ReactorName)
 : inherited(ParClass),
			R(0.0),
			Rp(0.0),
			Gp(0.0),
			X(0.0),
			l(0.0),
			kvarrating(0.0),
			kvrating(0.0),
			Rmatrix(nullptr),
			Gmatrix(nullptr),
			XMatrix(nullptr),
			Bmatrix(nullptr),
			Connection(0),
			SpecType(0),
			IsParallel(false),
			RpSpecified(false),
			Bus2Defined(false),
			Z2Specified(false),
			Z0Specified(false),
			RCurveObj(nullptr),
			LCurveObj(nullptr)
{
	Set_Name(LowerCase(ReactorName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(2);  // Force allocation of terminals and conductors
	SetBus(2, (GetBus(1) + ".0.0.0"));  // Default to grounded wye
	IsShunt				= true;
	Rmatrix				= nullptr;
	XMatrix				= nullptr;
	Gmatrix				= nullptr;
	Bmatrix				= nullptr;
	kvarrating			= 100.0;
	kvrating			= 12.47;
	X					= Sqr(kvrating) * 1000.0L / kvarrating;
	IsParallel			= false;
	RpSpecified			= false;
	Bus2Defined			= false;
	Z2Specified			= false;
	Z0Specified			= false;
	Connection			= 0;   // 0 or 1 for wye (default) or delta, respectively
	SpecType			= 1; // 1=kvar, 2=Cuf, 3=Cmatrix
	NormAmps			= kvarrating * SQRT3 / kvrating;
	EmergAmps			= NormAmps * 1.35;
	FNormAmpsSpecified	= false;
	FEmergAmpsSpecified = false;
	FaultRate			= 0.0005;
	PctPerm				= 100.0;
	HrsToRepair			= 3.0;
	Yorder				= Fnterms * Fnconds;
	RCurve				= "";
	RCurveObj			= nullptr;
	LCurve				= "";
	LCurveObj			= nullptr;
	RecalcElementData(ActiveActor);
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TReactorObj::~TReactorObj()
{
	free(Rmatrix);
	free(XMatrix);
	free(Gmatrix);
	free(Bmatrix);
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TReactorObj::RecalcElementData(int ActorID)
{
	double KvarPerPhase = 0.0;
	double PhasekV = 0.0;
	int i = 0;
	int CheckError = 0;
	switch(SpecType)
	{
		case 	1: // kvar
		{
			KvarPerPhase = kvarrating / Fnphases;
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
			X = Sqr(PhasekV) * 1000.0L / KvarPerPhase;
			l = X / TwoPi / BaseFrequency;
          /*Leave R as specified*/
			if (!FNormAmpsSpecified) NormAmps	= KvarPerPhase / PhasekV;
			if (!FEmergAmpsSpecified) EmergAmps = KvarPerPhase / PhasekV * 1.35;
		}
		break; // R + j X
          // Nothing much to do
		case 	2:
		{
			l = X / TwoPi / BaseFrequency;
		}
		break; // Matrices
		case 	3:
		break;
		default:
		  ;
		break;
	}
	if(RpSpecified && (Rp != 0.0))
		Gp = 1.0 / Rp;
	else
		Gp = 0.0; // default to 0,0 if Rp=0;
	if(IsParallel && (SpecType == 3))
	{
		int stop = 0;
		Gmatrix = (pDoubleArray) realloc(Gmatrix, sizeof(double) * Fnphases * Fnphases);
		Bmatrix = (pDoubleArray) realloc(Bmatrix, sizeof(double) * Fnphases * Fnphases);

         /*Copy Rmatrix to Gmatrix and Invert*/
		for(stop = Fnphases * Fnphases, i = 1; i <= stop; i++)
		{
			(Gmatrix)[i - 1] = (Rmatrix)[i - 1];
		}
// should be Gmatrix         ETKInvert(Rmatrix, Fnphases, CheckError);
		ETKInvert(Gmatrix, Fnphases, CheckError);
		if(CheckError > 0)
		{
			int stop = 0;
			DoSimpleMsg(String("Error inverting R Matrix for Reactor.") + get_Name()
	           + " - G is zeroed.", 232);
			for(stop = Fnphases * Fnphases, i = 1; i <= stop; i++)
			{
				Gmatrix[i - 1] = 0.0;
			}
		}

         /*Copy Xmatrix to Bmatrix and Invert*/
		for(stop = Fnphases * Fnphases, i = 1; i <= stop; i++)
		{
			Bmatrix[i - 1] = -XMatrix[i - 1];
		}
		ETKInvert(Bmatrix, Fnphases, CheckError);
		if(CheckError > 0)
		{
			int stop = 0;
			DoSimpleMsg(String("Error inverting X Matrix for Reactor.") + get_Name()
	           + " - B is zeroed.", 233);
			for(stop = Fnphases * Fnphases, i = 1; i <= stop; i++)
			{
				Bmatrix[i - 1] = 0.0;
			}
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TReactorObj::CalcYPrim(int ActorID)
{
	complex Value = CZero,
			Value1 = CZero,
			Value2 = CZero,
			Calpha1 = CZero,
			Calpha2 = CZero;
 //  Y0, Y1, Y2 : Complex;
	int i = 0;
	int j = 0;
	int Idx = 0;
	double FreqMultiplier = 0.0;
	pComplexArray Zvalues = nullptr;/*, Ymatrix */
	TcMatrix* YPrimTemp = nullptr;
	TcMatrix* Zmatrix = nullptr;
	double RValue = 0.0;
	double LValue = 0.0;

// Normally build only Yprim Shunt, but if there are 2 terminals and
// Bus1 <> Bus 2
	if(Get_YprimInvalid(ActorID,0))    // Reallocate YPrim if something has invalidated old allocation
	{
		if(YPrim_Shunt != nullptr)
			delete YPrim_Shunt;
		YPrim_Shunt = new TcMatrix(Yorder);
		if(YPrim_Series != nullptr)
			delete YPrim_Series;
		YPrim_Series = new TcMatrix(Yorder);
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
	/*# with YPrimTemp do */
	{
		auto with0 = YPrimTemp;
		FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
		FreqMultiplier = FYprimFreq / BaseFrequency;

     /*If GIC simulation, Resistance Only */
		if(ActiveCircuit[ActorID]->Solution->get_FFrequency() < 0.51)    // 0.5 Hz is cutoff
		{
			if(X > 0.0)
			{
				if(R <= 0.0)
					R = X / 50.0;   // Assume X/R = 50
			}
			FYprimFreq = 0.0;        // Set these to 0.0 for GIC model
			FreqMultiplier = 0.0;    // sets reactance part to zero
		}

    /* Now, Put in Yprim matrix */
		switch(SpecType)
		{
			case 	1:
			case	2:   /*Some form of R and X specified*/
               // Adjust for frequency
			{
				if(ASSIGNED(RCurveObj))
					RValue = R * RCurveObj->GetYValue_(FYprimFreq);
				else
					RValue = R;
				if(ASSIGNED(LCurveObj))
					LValue = l * LCurveObj->GetYValue_(FYprimFreq);
				else
					LValue = l;
				Value = cinv(cmplx(RValue, LValue * TwoPi * FYprimFreq));
               // Add in Rp Value if specified
				if(RpSpecified)
					caccum(Value, cmplx(Gp, 0.0));
				switch(Connection)
				{
					case 	1:   // Line-Line
					{
						int stop = 0;
						for(stop = Fnphases, i = 1; i <= stop; i++)
						{
							j = i + 1;
							if (j > Fnconds)
								j = 1;
							with0->AddElement(i, i, Value);
							with0->AddElement(j, j, Value);
							with0->AddElemsym(i, j, cnegate(Value));
						}
                        // Remainder of the matrix is all zero
					}
					break;
					default:
					int stop = 0;
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						with0->SetElement(i, i, Value);     // Elements are only on the diagonals
						with0->SetElement(i + Fnphases, i + Fnphases, Value);
						with0->SetElemsym(i, i + Fnphases, cnegate(Value));
					}
					break;
				}
			}
			break;    // Z matrix specified
			            /*Compute Z matrix*/

             /* Put in Parallel R & L */
			case 	3:
			{
				if(IsParallel)  /*Build Z as a Y Matrix*/
				{
					int stop = 0;
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						int stop1 = 0;
						for(stop1 = Fnphases, j = 1; j <= stop1; j++)
						{
							Idx = (j - 1) * Fnphases + i;
                       /*FreqMultiplier = 0 signifies GIC model where we only need R part*/
							if(FreqMultiplier > 0.0)
								Value = cmplx(Gmatrix[Idx - 1], Bmatrix[Idx - 1] / FreqMultiplier);
							else
								Value = cmplx(Gmatrix[Idx - 1], 0.0);
							with0->SetElement(i, j, Value);
							with0->SetElement(i + Fnphases, j + Fnphases, Value);
							with0->SetElemsym(i, j + Fnphases, cnegate(Value));
						}
					}
				}
				else
   /*For Series R and X*/
				{
					int stop = 0;
					Zmatrix = new TcMatrix(Fnphases);
					Zvalues = Zmatrix->GetValuesArrayPtr(Fnphases);  // So we can stuff array fast
                 /* Put in Series R & L */
					for(stop = Fnphases * Fnphases, i = 1; i <= stop; i++)
					{
                   // Correct the impedances for frequency
						Zvalues[i - 1] = cmplx(Rmatrix[i - 1], XMatrix[i - 1] * FreqMultiplier);
					}
					Zmatrix->Invert();  /*Invert in place - is now Ymatrix*/
					if(Zmatrix->InvertError > 0)       /*If error, put in tiny series conductance*/
					{
						int stop = 0;
						DoErrorMsg("TReactorObj.CalcYPrim", String("Matrix Inversion Error for Reactor \"") + get_Name()
	           + "\"", "Invalid impedance specified. Replaced with tiny conductance.", 234);
						Zmatrix->Clear();
						for(stop = Fnphases, i = 1; i <= stop; i++)
						{
							Zmatrix->SetElement(i, i, cmplx(EPSILON, 0.0));
						}
					}
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						int stop1 = 0;
						for(stop1 = Fnphases, j = 1; j <= stop1; j++)
						{
							Value = Zmatrix->GetElement(i, j);
							with0->SetElement(i, j, Value);
							with0->SetElement(i + Fnphases, j + Fnphases, Value);
							with0->SetElemsym(i, j + Fnphases, cnegate(Value));
						}
					}
					delete Zmatrix;
				}
			}
			break;  // Symmetrical component Z's specified
			
/***

   parallel doesn't make sense
              If IsParallel Then
               Begin

                 If Cabs(Z0) > 0.0 Then Y0 := Cinv(Z0) Else Y0 := Cmplx(1.0e12, 0.0);
                 If Cabs(Z1) > 0.0 Then Y1 := Cinv(Z1) Else Y1 := Cmplx(1.0e12, 0.0);
                 If Cabs(Z2) > 0.0 Then Y2 := Cinv(Z2) Else Y2 := Cmplx(1.0e12, 0.0);

                  {Assumes the sequence networks are in parallel}
                 Ymatrix := TcMatrix.CreateMatrix(Fnphases);

                // diagonal elements  -- all the same
                 If Fnphases=1 Then // assume positive sequence only model
                     Value := Y1
                 Else
                     Value := Cadd(Y2, Cadd(Y1, Y0));

                 Value.im := Value.im / FreqMultiplier; // Correct the impedances for frequency
                 Value    := CdivReal(Value, 3.0);
                 With Ymatrix Do FOR i := 1 to Fnphases  Do SetElement(i, i, Value);



                 If FnPhases = 3 Then     // otherwise undefined
                 Begin
                     Calpha1 := Conjg(Calpha);   // Change it to agree with textbooks
                     Calpha2 := Cmul(Calpha1, Calpha1);  // Alpha squared  = 1 /_ 240 = 1/_-120
                     Value2  := Cadd(Cmul(Calpha2,Y2),Cadd(Cmul(Calpha1, Y1), Y0));
                     Value1  := Cadd(Cmul(Calpha2,Y1),Cadd(Cmul(Calpha1, Y2), Y0));

                     Value1.im := Value1.im / FreqMultiplier; // Correct the impedances for frequency
                     Value2.im := Value2.im / FreqMultiplier; // Correct the impedances for frequency

                     Value1 := CdivReal(Value1, 3.0);
                     Value2 := CdivReal(Value2, 3.0);
                     With Ymatrix Do Begin
                       //Lower Triangle
                         SetElement(2, 1, Value1);
                         SetElement(3, 1, Value2);
                         SetElement(3, 2, Value1);
                       //Upper Triangle
                         SetElement(1, 2, Value2);
                         SetElement(1, 3, Value1);
                         SetElement(2, 3, Value2);
                     End;
                 End;

                 FOR i := 1 to Fnphases Do  BEGIN       // could be asymmetric
                    FOR j := 1 to Fnphases Do  BEGIN
                       Value := Ymatrix.GetElement(i,j);
                       SetElement(i, j, Value);
                       SetElement(i+Fnphases, j+Fnphases, Value);
                       SetElement(i, j+Fnphases, Cnegate(Value));
                       SetElement(i+Fnphases, j, Cnegate(Value));
                     END;
                  END;

                  Ymatrix.Free;

               End
               Else Begin
***/
                /*Series R+jX */
			case 	4:
			{
				int stop = 0;
				Zmatrix = new TcMatrix(Fnphases);

                 // diagonal elements  -- all the same
				if(Fnphases == 1) // assume positive sequence only model
					Value = Z1;
				else
					Value = cadd(Z2, cadd(Z1, Z0));
				Value.im = Value.im * FreqMultiplier; // Correct the impedances for frequency
				Value = cdivreal(Value, 3.0);
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					Zmatrix->SetElement(i, i, Value);
				}
				if(Fnphases == 3)     // otherwise undefined


                   // There are two possible off-diagonal elements  if Z1 <> Z2
                   // Calpha is defined as 1 /_ -120 instead of 1 /_ 120
				{
					Calpha1 = conjg(CALPHA);   // Change it to agree with textbooks
					Calpha2 = cmul(Calpha1, Calpha1);  // Alpha squared  = 1 /_ 240 = 1/_-120
					Value2 = cadd(cmul(Calpha2, Z2), cadd(cmul(Calpha1, Z1), Z0));
					Value1 = cadd(cmul(Calpha2, Z1), cadd(cmul(Calpha1, Z2), Z0));
					Value1.im = Value1.im * FreqMultiplier; // Correct the impedances for frequency
					Value2.im = Value2.im * FreqMultiplier; // Correct the impedances for frequency
					Value1 = cdivreal(Value1, 3.0);
					Value2 = cdivreal(Value2, 3.0);
					/*# with Zmatrix do */
					{
						auto with1 = Zmatrix;
                     //Lower Triangle
						with1->SetElement(2, 1, Value1);
						with1->SetElement(3, 1, Value2);
						with1->SetElement(3, 2, Value1);
                     //Upper Triangle
						with1->SetElement(1, 2, Value2);
						with1->SetElement(1, 3, Value1);
						with1->SetElement(2, 3, Value2);
					}
				}
				Zmatrix->Invert();  /*Invert in place - is now Ymatrix*/
				if(Zmatrix->InvertError > 0)       /*If error, put in tiny series conductance*/
				{
					int stop = 0;
					DoErrorMsg("TReactorObj.CalcYPrim", String("Matrix Inversion Error for Reactor \"") + get_Name()
	           + "\"", "Invalid impedance specified. Replaced with tiny conductance.", 234);
					Zmatrix->Clear();
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						Zmatrix->SetElement(i, i, cmplx(EPSILON, 0.0));
					}
				}
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					for(stop1 = Fnphases, j = 1; j <= stop1; j++)
					{
						Value = Zmatrix->GetElement(i, j);
						with0->SetElement(i, j, Value);
						with0->SetElement(i + Fnphases, j + Fnphases, Value);
						with0->SetElement(i, j + Fnphases, cnegate(Value));
						with0->SetElement(i + Fnphases, j, cnegate(Value));
					}
				}
				delete Zmatrix;
			}
			break;
       //    END;
			default:
			  ;
			break;
		}
	} /*With YPRIM*/

    // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
	if(IsShunt)
	{
		if((Get_NPhases() == 1) && (!ActiveCircuit[ActorID]->PositiveSequence))
		{
			int stop = 0;
			for(stop = Yorder, i = 1; i <= stop; i++)
			{  // assume a neutral or grounding reactor; Leave diagonal in the circuit
				YPrim_Series->SetElement(i, i, YPrim_Shunt->GetElement(i, i));
			}
		}
		else
		{
			int stop = 0;
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				YPrim_Series->SetElement(i, i, cmulreal(YPrim_Shunt->GetElement(i, i), 1.0e-10));
			}
		}
	}
	YPrim->CopyFrom(YPrimTemp);
    /*Don't Free YPrimTemp - It's just a pointer to an existing complex matrix*/
	inherited::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

void TReactorObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int j = 0;
	int k = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, k = 1; k <= stop; k++)
		{
			switch(k)
			{
				case 	7:
				if(Rmatrix != nullptr)  // was 'CASE i of' - good example of reason to remove all warnings 
				{
					int stop1 = 0;
					{ Write(f, with0->PropertyName[k - 1]); Write(f, "= ("); }
					for(stop1 = Fnphases, i = 1; i <= stop1; i++)
					{
						int stop2 = 0;
						for(stop2 = Fnphases, j = 1; j <= stop2; j++)
						{
							{ Write(f, Format("%-.5g",Rmatrix[(i - 1) * Fnphases + j - 1])); Write(f, L' '); }
						}
						if(i != Fnphases)
							Write(f, L'|');
					}
					WriteLn(f, L')');
				}
				break;
				case 	8:
				if(XMatrix != nullptr)
				{
					int stop1 = 0;
					{ Write(f, with0->PropertyName[k - 1]); Write(f, "= ("); }
					for(stop1 = Fnphases, i = 1; i <= stop1; i++)
					{
						int stop2 = 0;
						for(stop2 = Fnphases, j = 1; j <= stop2; j++)
						{
							{ Write(f, Format("%-.5g",XMatrix[(i - 1) * Fnphases + j - 1])); Write(f, L' '); }
						}
						if(i != Fnphases)
							Write(f, L'|');
					}
					WriteLn(f, L')');
				}
				break;
				case 	13:
				WriteLn(f, Format("~ Z1=[%-.8g, %-.8g]",Z1.re, Z1.im));
				break;
				case 	14:
				WriteLn(f, Format("~ Z2=[%-.8g, %-.8g]",Z2.re, Z2.im));
				break;
				case 	15:
				WriteLn(f, Format("~ Z0=[%-.8g, %-.8g]",Z0.re, Z0.im));
				break;
				case 	16:
				WriteLn(f, Format("~ Z =[%-.8g, %-.8g]",R, X));
				break;
				case 	19:
				WriteLn(f, Format("~ LmH=%-.8g",l * 1000.0));
				break;
				default:
				{ Write(f, "~ "); Write(f, with0->PropertyName[k - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(k)); }
				break;
			}
		}
	}
}

void TReactorObj::GetLosses(complex& TotalLosses, complex& LoadLosses, complex& NoLoadLosses, int ActorID)
{
	int i = 0;
	int j = 0;

  /*Only report No Load Losses if Rp defined and Reactor is a shunt device;
   Else do default behavior.*/
	if(RpSpecified && IsShunt && (Rp != 0.0))
	{
		TotalLosses = Get_Losses(ActorID);  // Side effect: computes Iterminal and Vterminal
     /*Compute losses in Rp Branch from voltages across shunt element -- node to ground*/
		NoLoadLosses = CZero;
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			int stop = 0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				if(!ADiakoptics || (ActorID == 1))
				{
					/*# with NodeV^[NodeRef^[i]] do */
					{
						  // V^2/Rp
						caccum(NoLoadLosses, cmplx((Sqr(with0->NodeV[NodeRef[i - 1]].re) + Sqr(with0->NodeV[NodeRef[i - 1]].im)) / Rp, 0.0));
					}
				}
				else
				{
					/*# with VoltInActor1(NodeRef^[i]) do */
					{
						auto with2 = with0->VoltInActor1(NodeRef[i - 1]);  // V^2/Rp
						caccum(NoLoadLosses, cmplx((Sqr(with2.re) + Sqr(with2.im)) / Rp, 0.0));
					}
				}
			}
		}
		if(ActiveCircuit[ActorID]->PositiveSequence)
			cmulreal(NoLoadLosses, 3.0);
		LoadLosses = csub(TotalLosses, NoLoadLosses);  // Subtract no load losses from total losses
	}
	else
	inherited::GetLosses(TotalLosses, LoadLosses, NoLoadLosses, ActorID);   /*do the default Cktelement behaviors*/
}

String TReactorObj::GetPropertyValue(int Index)
{
	String result;
	switch(Index)
	{
		case 	10:
		result = Format("%-.8g",R);
		break;
		case 	11:
		result = Format("%-.8g",X);
		break;
          /*Special cases for array properties*/
		case 	13:
		result = Format("[%-.8g, %-.8g]",Z1.re, Z1.im);
		break;
		case 	14:
		result = Format("[%-.8g, %-.8g]",Z2.re, Z2.im);
		break;
		case 	15:
		result = Format("[%-.8g, %-.8g]",Z0.re, Z0.im);
		break;
		case 	16:
		result = Format("[%-.8g, %-.8g]",R, X);
		break;
		case 	19:
		result = Format("%-.8g",l * 1000.0);
		break;
		case	(NumPropsThisClass + 1): 
		result = Format("%g", NormAmps);
		break;
		case	(NumPropsThisClass + 2): 
		result = Format("%g", EmergAmps);
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TReactorObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,GetBus(2));
	Set_PropertyValue(3,"3");
	Set_PropertyValue(4,"1200");
	Set_PropertyValue(5,"12.47");
	Set_PropertyValue(6,"wye");
	Set_PropertyValue(7,"");
	Set_PropertyValue(8,"");
	Set_PropertyValue(9,"NO");  // Parallel
	Set_PropertyValue(10,"0");  // R series
	Set_PropertyValue(11,Format("%-.6g",X));  //X
	Set_PropertyValue(12,"0");  //Rp
	Set_PropertyValue(13,"[0 0]");  //Z1
	Set_PropertyValue(14,"[0 0]");  //Z2
	Set_PropertyValue(15,"[0 0]");  //Z0
	Set_PropertyValue(16,"[0 0]");  //Z
	Set_PropertyValue(17,"");
	Set_PropertyValue(18,"");
	Set_PropertyValue(19,Format("%-.8g",X / TwoPi / BaseFrequency * 1000.0));  //X
	inherited::InitPropertyValues(NumPropsThisClass);

     //  Override Inherited properties
	Set_PropertyValue(NumPropsThisClass + 1,Str_Real(NormAmps, 0));
	Set_PropertyValue(NumPropsThisClass + 2,Str_Real(EmergAmps, 0));
	Set_PropertyValue(NumPropsThisClass + 3,Str_Real(FaultRate, 0));
	Set_PropertyValue(NumPropsThisClass + 4,Str_Real(PctPerm, 0));
	Set_PropertyValue(NumPropsThisClass + 5,Str_Real(HrsToRepair, 0));
	ClearPropSeqArray();
}

void TReactorObj::MakePosSequence(int ActorID)
{
	String s;
	double KvarPerPhase = 0.0;
	double PhasekV = 0.0;
	double Rs = 0.0;
	double Rm = 0.0;
	int i = 0;
	int j = 0;
    /*If FnPhases>1 Then */
	s = " ";
	switch(SpecType)
	{
		case 	1: // kvar
		{
			KvarPerPhase = kvarrating / 3.0;  // divide among 3 phases Fnphases;
			if((Fnphases > 1) || (Connection != 0))
				PhasekV = kvrating / SQRT3;
			else
				PhasekV = kvrating;
			s = String("Phases=1 ") + Format(" kV=%-.5g kvar=%-.5g", PhasekV, KvarPerPhase);
              /*Leave R as specified*/
		}
		break; // R + j X
		case 	2:
		{
			s = "Phases=1 ";
		}
		break;
		case 	3:
		if(Fnphases > 1) // Matrices
		{
			int stop = 0;
			s = "Phases=1 ";
              // R1
			Rs = 0.0;   // Avg Self
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				Rs = Rs + Rmatrix[(i - 1) * Fnphases + i - 1];
			}
			Rs = Rs / Fnphases;
			Rm = 0.0;     //Avg mutual
			for(stop = Fnphases, i = 2; i <= stop; i++)
			{
				int stop1 = 0;
				for(stop1 = Fnphases, j = i; j <= stop1; j++)
				{
					Rm = Rm + Rmatrix[(i - 1) * Fnphases + j - 1];
				}
			}
			Rm = Rm / (Fnphases * (Fnphases - 1.0) / 2.0);
			s = s + Format(" R=%-.5g",(Rs - Rm));

              // X1
			Rs = 0.0;   // Avg Self
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				Rs = Rs + XMatrix[(i - 1) * Fnphases + i - 1];
			}
			Rs = Rs / Fnphases;
			Rm = 0.0;     //Avg mutual
			for(stop = Fnphases, i = 2; i <= stop; i++)
			{
				int stop1 = 0;
				for(stop1 = Fnphases, j = i; j <= stop1; j++)
				{
					Rm = Rm + XMatrix[(i - 1) * Fnphases + j - 1];
				}
			}
			Rm = Rm / (Fnphases * (Fnphases - 1.0) / 2.0);
			s = s + Format(" X=%-.5g",(Rs - Rm));
		}
		break; // symmetrical components  Z1 specified
		case 	4:
		{
			s = "Phases=1 ";
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




}  // namespace Reactor




