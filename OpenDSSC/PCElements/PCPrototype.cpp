
#include <vcl.h>
#pragma hdrstop

#include "PCPrototype.h"
#include "../Parser/ParserDel.h"
#include "../Common/DSSClassDefs.h"
#include "../Common/DSSGlobals.h"
#include "../Common/Circuit.h"
#include "../Shared/Command.h"
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include "../Shared/mathutil.h"
#include "../Common/Utilities.h"

using namespace std;
using namespace d2c_system;
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
using namespace System::Math;
using namespace System::Sysutils;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace utilities;

namespace PCPrototype
{

TPCPrototypeObj::TPCPrototypeObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TPCPrototypeObj::TPCPrototypeObj(String ClassName) : inherited(ClassName) {}
TPCPrototypeObj::TPCPrototypeObj() {}


TPCPrototypeObj* ActivePCPrototypeObj = nullptr;

/*Typical Uses Clause -- not all may not be needed*/     // DSS parser
  // Where class is instantiated
    // Global DSS variables
       // If access to circuit variables is needed
       // DSS command and property support module
      // Delphi misc utility functions
          // Delphi Math functions
      // DSS Math utilities
     // DSS misc utility functions
const int NumPropsThisClass = 36; // Set this constant to the actual number of properties you define
  // Define any useful module vars here, for example:
complex cBuffer[24/*# range 1..24*/];  // Temp buffer for complex math calcs; allows up to 24-phase models.
complex CDoubleOne = {};   // 1 + j1  (see Initialization section below)

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates main collection handler for all PCPrototype objects

TPCPrototype::TPCPrototype()
{
	;  // make the base class  and init DSSClassType

     // Specify class name and bit mask ID for this class type
     // PCPROTOTYPE_ELEMENT must be defined in DSSClassDefs as nn*8
     // First 3 bits are used for base class type (DSSClassType)
	Class_Name = L"PCPrototype";
	DSSClassType = DSSClassType + PCPROTOTYPE_ELEMENT;
	ActiveElement = 0;   // no active elements yet; init to 0

     /*Initialize any other special variables here*/
	DefineProperties();   // This is where the properties for this class are defined

     // Use the Command processor to manage property names
     // PropertyName is an array of String defined in DefineProperties
	CommandList = new TCommandList(SLICE((*PropertyName), NumProperties));
	CommandList->set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TPCPrototype::~TPCPrototype()
{


    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This is where the properties are defined, assigned names, indexes, and help strings
// The Help strings will automatically show up when the Help is invoked

void TPCPrototype::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();   /*see DSSClass*/

     // Refer to other classes for alternative methods of assigning properties
     // This example uses the AddProperty function to assign Name, Index, and Help string
     // in one statement.

     // First argument is string name of the property
     // Second argument is the index for the CASE statement
     // Third argument is help string

     // DSS properties are accessed in sequence if the property name is not explicitly specified.
     // The advantage of using the AddProperty function is that you may change the sequence simply
     // by shuffling the order of the definitions and you do not have to change the index in the CASE
     // statement in the EDIT function

     // Define Property names, for example (from Generator class)
	AddProperty(L"phases", 1, L"Number of Phases, this PCPrototype.  Power is evenly divided among phases.");
	AddProperty(L"bus1", 2, L"Bus to which the PCPrototype is connected.  May include specific node specification.");
	AddProperty(L"kv", 3, L"Nominal rated (1.0 per unit) voltage, kV, for PCPrototype. For 2- and 3-phase PCPrototypes, specify phase-phase kV. "
	           L"Otherwise, specify actual kV across each branch of the PCPrototype. "
	           L"If wye (star), specify phase-neutral kV. "
	           L"If delta or phase-phase connected, specify phase-phase kV.");  // line-neutral voltage//  base voltage
	AddProperty(L"kW", 4, String(L"Total base kW for the PCPrototype.  A positive value denotes power coming OUT of the element, ") + CRLF
	           + L"which is the opposite of a load. This value is modified depending on the dispatch mode. "
	           + L"Unaffected by the global load multiplier and growth curves. "
	           + L"If you want there to be more generation, you must add more PCPrototypes or change this value.");




                    /*...*/
                    /*etc.*/




     // Finally, we have to pick up any properties that were inherited
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // You can optionally override default help string of an inherited property, for example
	(*PropertyHelp)[NumPropsThisClass + 1 - 1] = L"Name of harmonic voltage or current spectrum for this PCPrototype. "
	           L"Voltage behind Xd\" for machine - default. Current injection for inverter. "
	           L"Default value is \"default\", which is defined when the DSS starts.";
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This function is called  by the DSS whenever a New PCPrototype... command is encountered

int TPCPrototype::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new PCPrototype and add it to PCPrototype class list
	/*# with ActiveCircuit do */
	{
		auto& with0 = ActiveCircuit;
		ActiveCktElement = new TPCPrototypeObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[0]);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This is a typical helper function found in many DSS circuit element class
// for defining the number of conductors per terminal (Nconds) based on Y or delta connection

void TPCPrototype::SetNcondsForConnection()
{
	/*# with ActivePCPrototypeObj do */
	{
		auto with0 = ActivePCPrototypeObj;
		switch(Connection)
		{
			case 	0:
			with0->Set_Nconds(Fnphases + 1);
			break;  // Wye connection (has a neutral terminal)
			case 	1:
			switch(with0->Fnphases)
			{
				case 	1:
				 case 2:        // Delta connection
				with0->Set_Nconds(Fnphases + 1);
				break; // L-L and Open-delta
				default:
				with0->Set_Nconds(with0->Fnphases);    // no neutral for this connection
				break;
			}
			break;
			default:
			  ;
			break;
		}
	}
}



//- - - - - - - - - - - - -MAIN EDIT FUNCTION  - - - - - - - - - - - - - - -


// This function is the heart of the property managment for this class

int TPCPrototype::Edit()
{
	int result = 0;     // Define some local vars for handling parser results
	int i = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;

// The Edit function starts where the Parser is presently pointing and
// manages the parsing of the rest of the command line in the parser.

// The DSS executive processes the command verb on the front of the line and
// then passes control to the appropriate Edit function

  // set the present element active
  // and continue parsing with contents of Parser
	ActivePCPrototypeObj = &ElementList.Get_Active();
	ActiveCircuit.ActiveCktElement = ActivePCPrototypeObj;
	result = 0;
	/*# with ActivePCPrototypeObj do */
	{
		auto with0 = ActivePCPrototypeObj;
     // peel off the next token on the edit line
		ParamPointer = 0;
		ParamName = Parser.NextParam;
		Param = Parser.StrValue;
		while(Param.Length() > 0)

         // Find the index for the CASE statement
         // If property is not named, just increment the index to the next property
		{
			if(ParamName.Length() == 0)
				++ParamPointer;
			else
				ParamPointer = CommandList->Getcommand(ParamName);

         // Update the PropertyValy for this property
         // Actual index is mapped via PropertyIdxMap array for this class
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))
				with0->PropertyValue[(*PropertyIdxMap)[ParamPointer - 1]] = Param;
			else
				DoSimpleMsg(String(L"Unknown parameter \"") + ParamName
	           + L"\" for PCPrototype \""
	           + with0->Name
	           + L"\"", 560);

         // --------------- MAIN CASE STATEMENT ----------------------
			if(ParamPointer > 0)
				switch((*PropertyIdxMap)[ParamPointer - 1])
				{
         // since we used AddProperty function to define properties, have to
         // use PropertyIdxMap to map to the correct Case index
					case 	0:
					DoSimpleMsg(String(L"Unknown parameter \"") + ParamName
	           + L"\" for Object \""
	           + Class_Name
	           + L"."
	           + with0->Name
	           + L"\"", 561);
					break;
					case 	1:
					with0->NPhases = Parser.IntValue;
					break; // num phases
					case 	2:
					with0->SetBus(1, Param);
					break;
					case 	3:
					Set_PresentkV(Parser.DblValue);
					break;

            /*...*/
            /*etc.*/

            /*One case for each property*/
           // Handle Inherited properties
					default:
					ClassEdit(ActivePCPrototypeObj, ParamPointer - NumPropsThisClass);
					break;
				}

         // ---------------- SIDE EFFECTS CASE STATEMENT ---------------------
         // This case statment handles any side effects from setting a property
         // (for example, from Generator)
			if(ParamPointer > 0)
				switch((*PropertyIdxMap)[ParamPointer - 1])
				{
					case 	1:
					SetNcondsForConnection();
					break;  // Force Reallocation of terminal info

            // keep kvar nominal up to date with kW and PF
					case 	4: case 5:
					SyncUpPowerQuantities;
					break;

           /*etc.*/
					default:
					  ;
					break;
				}

         // Get next token off Parser and continue editing properties
			ParamName = Parser.NextParam;
			Param = Parser.StrValue;
		}

     // After editing is complete, the typical next step is to call the RecalcElementData function
		with0->RecalcElementData();
		with0->YPrimInvalid[ActorID] = true; // Setting this flag notifies the DSS that something has changed
                           // and the Yprim will have to be rebuilt
	}
	return result;
}

//----------------------------------------------------------------------------


// This function should be defined to handle the Like property inherited from
// the base class.

// The function copies the essential properties of another object of this class

int TPCPrototype::MakeLike(const String OtherPCPrototypeName)
{
	int result = 0;
	TPCPrototypeObj* OtherPCPrototype = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this PCPrototype name in the present collection*/
	OtherPCPrototype = ((TPCPrototypeObj*) Find(OtherPCPrototypeName));
	if(OtherPCPrototype != nullptr)
		/*# with ActivePCPrototypeObj do */
		{
			auto with0 = ActivePCPrototypeObj;   // skip if not found

       // You should first set the basic circuit element properties, for example
			int stop = 0;
			if(with0->Fnphases != OtherPCPrototype->Fnphases)
			{
				with0->NPhases = OtherPCPrototype->Fnphases;
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->YPrimInvalid[ActorID] = true;
			}

       // Then set other property values, for example from Generator
			GenVars.kVGeneratorBase = OtherPCPrototype->GenVars.kVGeneratorBase;
			VBase = OtherPCPrototype->VBase;
			kWBase = OtherPCPrototype->kWBase;
			kvarBase = OtherPCPrototype->kvarBase;
			UserModel.Name = OtherPCPrototype->UserModel.Name;  // Connect to user written models

       /*...*/
       /*etc.*/




       // Do inherited properties
			ClassMakeLike(OtherPCPrototype);

       // Finally initialize all the property value strings to be the same as
       // the copied element
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				(*with0->FPropertyValue)[i - 1] = (*OtherPCPrototype->FPropertyValue)[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String(L"Error in Load MakeLike: \"") + OtherPCPrototypeName
	           + L"\" Not Found.", 562);
	return result;
}

//----------------------------------------------------------------------------


// Optional function if you want to do anything to initialize objects of this class

int TPCPrototype::Init(int Handle)
{
	int result = 0;
	TPCPrototypeObj* P = nullptr;

  /* For example: set up for randomization*/
	if(Handle == 0)  // init all
	{
		P = &ElementList.Get_First();
		while((P != nullptr))
		{
			P->Randomize(0);
			P = &ElementList.Get_Next();
		}
	}
	else
	{
		Active = Handle;
		P = ((TPCPrototypeObj*) GetActiveObj());
		P->Randomize(0);
	}
	result = 0;
	return result;
}


//------------------------- MAIN OBJECT CONSTRUCTOR ---------------------

TPCPrototypeObj::TPCPrototypeObj(TDSSClass* ParClass, const String PCPrototypeObjName)
 : inherited(ParClass)
{
	Name = LowerCase(PCPrototypeObjName);
	DSSObjType = ParClass->DSSClassType; // Same as Parent Class

     // Set some basic circuit element properties
	NPhases = 3;  // typical DSS default for a circuit element
	Fnconds = 4;  // defaults to wye
	Yorder = 0;  // To trigger an initial allocation
	NTerms = 1;  // forces allocations of terminal quantities

   /*Initialize variables for this object, for example*/
	kvarMax = kvarBase * 2.0;
	kvarMin = -kvarMax;
	PFNominal = 0.88;
	YearlyShape = L"";
	YearlyShapeObj = nullptr;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
	Vpu = 1.0;
	VBase = 7200.0;
	Vminpu = 0.90;
	Vmaxpu = 1.10;
	VBase95 = Vminpu * VBase;
	VBase105 = Vmaxpu * VBase;
	Yorder = Fnterms * Fnconds;


     /*etc.*/




     // If you support a user-written DLL, Initialize here
     // For example, from Generator
	UserModel = new TGenUserModel(GenVars);



     // call the procedure to set the initial property string values
	InitPropertyValues(0);

     // Update anything that has to be calculated from property values
	RecalcElementData();
}


//----------------------------------------------------------------------------


// Free everything here that needs to be freed
// If you allocated anything, dispose of it here

TPCPrototypeObj::~TPCPrototypeObj()
{
	delete UserModel;  // If you have a user-written DLL
	// inherited::Destroy();   // This will take care of most common circuit element arrays, etc.
}


//----------------------------------------------------------------------------


// typical proc for handling randomization in DSS fashion

void TPCPrototypeObj::Randomize(int Opt)
{
	switch(Opt)
	{
		case 	0:
		RandomMult = 1.0;
		break;
		case 	GAUSSIAN:
		RandomMult = Gauss(YearlyShapeObj.Mean(), YearlyShapeObj.StdDev());
		break;
		case 	UNIFORM:
		RandomMult = Random();
		break;  // number between 0 and 1.0
		case 	LOGNORMAL:
		RandomMult = QuasiLogNormal(YearlyShapeObj.Mean());
		break;
		default:
		  ;
		break;
	}
}


//----------------------------------------------------------------------------


// Anything that needs to re-calculated  after an Edit

void TPCPrototypeObj::RecalcElementData()
{



    /*For example:*/
	VBase95 = Vminpu * VBase;
	VBase105 = Vmaxpu * VBase;

    /*...*/
    /*etc.*/

    // For example, find specified Spectrum object  and report error if not found
	SpectrumObj = SpectrumClass.Find(Spectrum);
	if(SpectrumObj == nullptr)
		DoSimpleMsg(String(L"ERROR! Spectrum \"") + Spectrum + L"\" Not Found.", 566);

    // For PC elements, a good idea to reallocate InjCurrent in case Yorder has changed
	InjCurrent = (pComplexArray) ReallocMemory(InjCurrent, sizeof((*InjCurrent)[1 - 1]) * Yorder);

    /*Update any user-written models, for example*/
	if(UserModel.Get_Exists())
		UserModel.FUpdateModel;
}

//----------------------------------------------------------------------------


/*A typical helper function for PC elements to assist in the computation
 of Yprim
*/

void TPCPrototypeObj::CalcYPrimMatrix(TcMatrix* Ymatrix)
{
	complex Y = {};
	complex Yij = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;
	FYprimFreq = ActiveCircuit.Solution.Frequency;
	FreqMultiplier = FYprimFreq / BaseFrequency;  // ratio to adjust reactances for present solution frequency
	/*# with ActiveCircuit.Solution do */
	{
		auto& with0 = ActiveCircuit.Solution;
		if(IsDynamicModel | IsHarmonicModel)
   // for Dynamics and Harmonics modes use constant equivalent Y

       // Example from Generator
       // If the generator is on, use equivalent Y else some very small value
		{
			int stop = 0;
			if(GenON)   // L-N value computed in initialization routines
				Y = Yeq;
			else
				Y = cmplx(Epsilon, 0.0);
			if(Connection == 1)
				Y = cdivreal(Y, 3.0); // Convert to delta impedance
			Y.im = Y.im / FreqMultiplier;  // adjust for frequency
			Yij = cnegate(Y);
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				switch(Connection)
				{
					case 	0:
					{
						Ymatrix->SetElement(i, i, Y);  // sets the element
						Ymatrix->AddElement(Fnconds, Fnconds, Y);  // sums the element
						Ymatrix->SetElemsym(i, Fnconds, Yij);
					}
					break;   /*Delta connection*/
					case 	1:
					{
						int stop1 = 0;
						Ymatrix->SetElement(i, i, Y);
						Ymatrix->AddElement(i, i, Y);  // put it in again
						for(stop1 = i - 1, j = 1; j <= stop1; j++)
						{
							Ymatrix->SetElemsym(i, j, Yij);
						}
					}
					break;
					default:
					  ;
					break;
				}
			}
		}
		else


    //  Typical code for a regular power flow  model
    //  Example from Generator object
		
       /*Yeq is typically expected as the equivalent line-neutral admittance*/
		{
			Y = cnegate(Yeq);  // negate for generation    Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
			Y.im = Y.im / FreqMultiplier;
			switch(Connection)
			{
				case 	0:
				/*# with Ymatrix do */
				{
					auto with1 = Ymatrix; // WYE
					int stop = 0;
					Yij = cnegate(Y);
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						with1->SetElement(i, i, Y);
						with1->AddElement(Fnconds, Fnconds, Y);
						with1->SetElemsym(i, Fnconds, Yij);
					}
				}
				break;
				case 	1:
				/*# with Ymatrix do */
				{
					auto with2 = Ymatrix;  // Delta  or L-L
					int stop = 0;
					Y = cdivreal(Y, 3.0); // Convert to delta impedance
					Yij = cnegate(Y);
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						j = i + 1;
						if(j > Fnconds)
							j = 1;  // wrap around for closed connections
						with2->AddElement(i, i, Y);
						with2->AddElement(j, j, Y);
						with2->AddElemsym(i, j, Yij);
					}
				}
				break;
				default:
				  ;
				break;
			}
		}
	}  /*ELSE IF Solution.mode*/
}


//----------------------------------------------------------------------------


// Required routine to calculate the primitive Y matrix for this element

// This example uses a helper function (CalcYPrimMatrix) to keep the code
// here clean

void TPCPrototypeObj::CalcYPrim()
{
	int i = 0;

/*
  There are three Yprim matrices that could be computed:

     YPrim_Series:  Used for zero-load solutions to initialize the first guess
     YPrim_Shunt:   Equivalent Y in shunt with power system
                    For PC elements, this is typically the main YPrim
     YPrim:         Generally the sum of the other two; the total YPrim
*/

     // Typical PC Elements build only shunt Yprim
     // Also, build a dummy Yprim Series so that CalcVoltagebases does not fail

     // First clear present value; redefine if necessary
     // Note: Complex matrix (TcMatrix -- see uCmatrix.pas) is used for this
	int stop = 0;
	if(YPrimInvalid[ActorID])
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
		YPrim_Shunt->Clear();
		YPrim_Series->Clear();
		YPrim->Clear();
	}

     /*do whatever you have to do to determine Yeq here*/

     // call helper routine to compute YPrim_Shunt
	CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on a small fraction of the diagonals of YPrim_shunt
     // so that CalcVoltages doesn't fail
     // This is just one of a number of possible strategies but seems to work most of the time
	for(stop = Yorder, i = 1; i <= stop; i++)
	{
		YPrim_Series->SetElement(i, i, cmulreal(YPrim_Shunt->GetElement(i, i), 1.0e-10));
	}

     // copy YPrim_shunt into YPrim; That's all that is needed for most PC Elements
	YPrim->CopyFrom(YPrim_Shunt);

     // Account for Open Conductors -- done in base class
	inherited::CalcYPrim();
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Most PC Elements should have a routine like this to make the current injections into the proper place

 /*Add the current into the proper array position according to connection*/

 /*
  This example is from GENERATOR
  Reverse of similar routine in LOAD  (Cnegates are switched)
 */

void TPCPrototypeObj::StickCurrInTerminalArray(pComplexArray TermArray, const complex& Curr, int i)
{
	int j = 0;
	switch(Connection)
	{
		case 	0:  //Wye
		{
			caccum((*TermArray)[i - 1], Curr);
			caccum((*TermArray)[Fnconds - 1], cnegate(Curr)); // Neutral
		}
		break; //DELTA
		case 	1:
		{
			caccum((*TermArray)[i - 1], Curr);
			j = i + 1;
			if(j > Fnconds)
				j = 1;
			caccum((*TermArray)[j - 1], cnegate(Curr));
		}
		break;
		default:
		  ;
		break;
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*
 This routine is an example from Generator to illustrate how to compute currents
 for PC elements. This is a common constant P + jQ model.
*/

/*Compute the total terminal current for Constant PQ*/

void TPCPrototypeObj::DoConstantPQGen()
{
	int i = 0;
	complex Curr = {};
	complex V = {};
	double Vmag = 0.0;

    // First compute the contribution due to Yprim matrix
	int stop = 0;
	CalcYPrimContribution(InjCurrent);

    // Zero out the Iterminal array to hold the results of this calculation
	ZeroITerminal();
    // get actual voltage across each phase of the load  and put into VTerminal array
	CalcVTerminalPhase;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = (*Vterminal)[i - 1];
		Vmag = cabs(V);
		switch(Connection)
		{
			case 	0:  /*Wye*/
			{
				if(Vmag <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(Yeq95, V);
				else
				{
					if(Vmag > VBase105)  // above 105% use an impedance model
						Curr = cmul(Yeq105, V);
					else
						/*# with GenVars do */
						{
							auto& with0 = GenVars;
							Curr = conjg(cdiv(cmplx(Pnominalperphase, Qnominalperphase), V));
						}  // Between 95% -105%, constant PQ
				}
			}
			break;  /*Delta*/
			case 	1:
			{
				Vmag = Vmag / SQRT3;  // L-N magnitude
				if(Vmag <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(cdivreal(Yeq95, 3.0), V);
				else
				{
					if(Vmag > VBase105)  // above 105% use an impedance model
						Curr = cmul(cdivreal(Yeq105, 3.0), V);
					else
						/*# with GenVars do */
						{
							auto& with1 = GenVars;
							Curr = conjg(cdiv(cmplx(Pnominalperphase, Qnominalperphase), V));
						}  // Between 95% -105%, constant PQ
				}
			}
			break;
			default:
			  ;
			break;
		}

       // Add into ITerminal (initialized above)
		StickCurrInTerminalArray(Iterminal, cnegate(Curr), i);  // Put into Terminal array taking into account connection
		IterminalUpdated = true;
       // Now, add into InjCurrent array. This is used in the Normal solution algorithm
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/* This is an example taken from Generator illustrating how a PC element might
  handle Dynamics mode with a Thevenin equivalent

  Also illustrates the computation of symmetrical component values
*/

/*Compute Total Current and add into InjTemp*/

void TPCPrototypeObj::DoDynamicMode()
{
	int i = 0;
	complex V012[4] = { CZero, CZero, CZero, CZero, };
	complex I012[4] = { CZero, CZero, CZero, CZero, };

   // Start off by getting the current in the admittance branch of the model
	int stop = 0;
	CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   /*Inj = -Itotal (in) - Yprim*Vtemp*/
	/*# with GenVars do */
	{
		auto& with0 = GenVars;

        // Convert Terminal voltages to symmetrical component
		Phase2SymComp(Vterminal, &V012[0]);

        // Positive Sequence Contribution to Iterminal

        // Compute present value of VThev
		CalcVthev_Dyn;  // Update for latest phase angle

        // Positive Sequence Contribution to Iterminal
        // Use Xd' for pos seq;  Xd" for neg seq
		I012[1] = cdiv(csub(V012[1], Vthev), cmplx(0.0, Xdp));
		I012[2] = cdiv(V012[2], cmplx(0.0, Xdpp));
		if(Connection == 1)
			I012[0] = CZero;
		else
			I012[0] = cdiv(V012[0], cmplx(0.0, Xdpp));

        // Convert back to phase components
		SymComp2Phase(Iterminal, &I012[0]);

        // Neutral current, if any
		if(Connection == 0)
			(*Iterminal)[Fnconds - 1] = cnegate(cmulreal(I012[0], 3.0));
	}
	IterminalUpdated = true;

   /*Add it into inj current array*/
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		caccum((*InjCurrent)[i - 1], cnegate((*Iterminal)[i - 1]));
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*
  Example taken from Generator illustrating how a PC element might handle
  current calcs for Harmonics mode

  Note: Generator objects assume a Thevenin model (voltage behind and impedance)
        while Load objects assume the Spectrum applies to a Norton model injection current
*/

/*Compute Injection Current Only when in harmonics mode*/

/*Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built*/
/*Vd is the fundamental frequency voltage behind Xd" for phase 1*/

void TPCPrototypeObj::DoHarmonicMode()
{
	int i = 0;
	complex e = {};
	double GenHarmonic = 0.0;

   // Set the VTerminal array
	ComputeVterminal();
	/*# with ActiveCircuit.Solution do */
	{
		auto& with0 = ActiveCircuit.Solution;
		int stop = 0;
		GenHarmonic = double(Frequency) / GenFundamental; // harmonic based on the fundamental for this object
        // get the spectrum multiplier and multiply by the V thev (or Norton current for load objects)
		e = cmulreal(SpectrumObj->GetMult(GenHarmonic), VThevHarm); // Get base harmonic magnitude
		RotatePhasorRad(e, GenHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift

        // Put the values in a temp complex buffer
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			cBuffer[i - 1] = e;
			if(i < Fnphases)
				RotatePhasorDeg(e, GenHarmonic, -120.0);  // Assume 3-phase PCPrototype
		}
	}

   /*Handle Wye Connection*/
	if(Connection == 0)
		cBuffer[Fnconds - 1] = (*Vterminal)[Fnconds - 1];  // assume no neutral injection voltage

   // In this case the injection currents are simply Yprim(frequency) times the voltage buffer
   // Refer to Load.Pas for load-type objects
		   /*Inj currents = Yprim (E) */
	YPrim->MVmult(InjCurrent, (pComplexArray) &cBuffer);
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*
  Many PC Element models will contain a Proc like this to compute terminal voltages
  differently for Y or Delta connections
*/

void TPCPrototypeObj::CalcVTerminalPhase()
{
	int i = 0;
	int j = 0;

/* Establish phase voltages and stick in Vterminal*/
	switch(Connection)
	{
		case 	0:
		{
			/*# with ActiveCircuit.Solution do */
			{
				auto& with0 = ActiveCircuit.Solution;
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					(*Vterminal)[i - 1] = VDiff((*NodeRef)[i - 1], (*NodeRef)[Fnconds - 1]);
				}
			}
		}
		break;
		case 	1:
		{
			/*# with ActiveCircuit.Solution do */
			{
				auto& with1 = ActiveCircuit.Solution;
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					j = i + 1;
					if(j > Fnconds)
						j = 1;
					(*Vterminal)[i - 1] = VDiff((*NodeRef)[i - 1], (*NodeRef)[j - 1]);
				}
			}
		}
		break;
		default:
		  ;
		break;
	}


   // It is often advantageous to keep track of which solution VTerminal applies to
   // You can use this to avoid unnecessary recalcs of Vterminal if the solution hasn't changed
	PCPrototypeSolutionCount = ActiveCircuit.Solution.SolutionCount;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/* this is just the standard routine to put terminal voltages in an array
  But it also keeps track of the solution count for computational efficiency
*/

void TPCPrototypeObj::CalcVterminal()
{
	ComputeVterminal();
	PCPrototypeSolutionCount = ActiveCircuit.Solution.SolutionCount;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Main dispatcher for computing PC Element currnts

// Calculates PCPrototype current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

void TPCPrototypeObj::CalcPCPrototypeModelContribution()
{
	IterminalUpdated = false;
	/*# with ActiveCircuit, ActiveCircuit.Solution do */
	{
		auto& with0 = ActiveCircuit;
		auto& with1 = ActiveCircuit.Solution;
		if(IsDynamicModel)
			DoDynamicMode;
		else
		{
			if(IsHarmonicModel && (Frequency != Fundamental))
				DoHarmonicMode;
			else

           //  compute currents and put into InjTemp array;
			{
				switch(PCPrototypeModel)
				{
					case 	1:
					DoConstantPQGen;
					break;  // for example, from Generator
					 /*etc.*/
					case 	2:
					;
					break;  // Different models

              // Put a default model proc call here
					default:
					  ;
					break;
				}
			} /*ELSE*/
		}
	} /*WITH*/

   /*When this is done, ITerminal is up to date*/
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Main procedure for controlling computation of InjCurrent array

// InjCurrent is difference between currents in YPrim and total terminal current

void TPCPrototypeObj::CalcInjCurrentArray()
{


// You usually will want some logic like this

       // If the element is open, just zero the array and return
	if(PCPrototypeSwitchOpen)
       // otherwise, go to a routine that manages the calculation
		ZeroInjCurrent();
	else
		CalcPCPrototypeModelContribution;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// This function controls the calculation of the total terminal currents

// Note that it only does something if the solution count has changed.
// Otherwise, Iterminal array already contains the currents

void TPCPrototypeObj::GetTerminalCurrents(pComplexArray Curr)
{
	/*# with ActiveCircuit.Solution do */
	{
		auto& with0 = ActiveCircuit.Solution;
		if(IterminalSolutionCount != ActiveCircuit.Solution.SolutionCount)     // recalc the contribution
          // You will likely want some logic like this
		{
			if(!PCPrototypeSwitchOpen)
				CalcPCPrototypeModelContribution;  // Adds totals in Iterminal as a side effect
		}
		inherited::GetTerminalCurrents(Curr); // add in inherited contribution
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Required function for managing computing of InjCurrents

int TPCPrototypeObj::InjCurrents()
{
	int result = 0;
	/*# with ActiveCircuit.Solution do */
	{
		auto& with0 = ActiveCircuit.Solution;

      // Generators and Loads use logic like this:
      //   If LoadsNeedUpdating Then SetNominalGeneration; // Set the nominal kW, etc for the type of solution being done

       // call the main function for doing calculation
		CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current


       // Add into System Injection Current Array
		result = inherited::InjCurrents();
	}
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Gets the currents for the last solution performed

// Do not call anything that may change the basic element values from the last solution

void TPCPrototypeObj::GetInjCurrents(pComplexArray Curr)
{
	int i = 0;
	CalcInjCurrentArray;  // Difference between currents in YPrim and total current
	try
    // an exception here generally means an array boundary overrun
   // Copy into buffer array
	{
		int stop = 0;
		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			(*Curr)[i - 1] = (*InjCurrent)[i - 1];
		}
	}
	catch(Exception* e)
	{
		DoErrorMsg(String(L"PCPrototype Object: \"") + Name
	           + L"\" in GetInjCurrents function.", e->Message, L"Current buffer not big enough.", 568);
	}
}
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =




// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*
 This procedure is require to respond to various commands such as Dump that
 write all the device's property values to a file.
*/

void TPCPrototypeObj::DumpProperties(d2c_system::TTextRec& f, bool Complete)
{
	int i = 0;
	int Idx = 0;
	inherited::DumpProperties(f, Complete);

    /*Write out any specials here, usually preceded by a "!"*/
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			Idx = (with0->PropertyIdxMap)[i - 1]; // Map to get proper index into property value array
			switch(Idx)
			{
				case 	34: case 36:

          /*Trap any specials here, such as values that are array properties, for example*/
				{ Write(f, L"~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L"=("); Write(f, PropertyValue[Idx]); WriteLn(f, L')'); }
				break;
				default:
				{ Write(f, L"~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, PropertyValue[Idx]); }
				break;
			}
		}
	}
	WriteLn(f);
}

/*Procedure to initialize for Harmonics solution*/

/*This example is extracted from Generator and constructs a Thevinen equivalent.
 Refer to Load for how to do a Norton equivalent
 */

void TPCPrototypeObj::InitHarmonics()
{
	complex e = {};
	complex Va = {};
	YPrimInvalid[ActorID] = true;  // Force rebuild of YPrims
	GenFundamental = ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.
	/*# with GenVars do */
	{
		auto& with0 = GenVars;

         // Xd" is used for harmonics analysis for generators
		Yeq = cinv(cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

         /*Compute reference Thevinen voltage from phase 1 current*/
		if(GenON)
		{
			ComputeIterminal();  // Get present value of current
			/*# with ActiveCircuit.Solution do */
			{
				auto& with1 = ActiveCircuit.Solution;
				switch(Connection)
				{
					case 	0: /*wye - neutral is explicit*/
					{
						Va = csub(NodeV[(*NodeRef)[1 - 1]], NodeV[(*NodeRef)[Fnconds - 1]]);
					}
					break;  /*delta -- assume neutral is at zero*/
					case 	1:
					{
						Va = NodeV[(*NodeRef)[1 - 1]];
					}
					break;
					default:
					  ;
					break;
				}
			}
			e = csub(Va, cmul((*Iterminal)[1 - 1], cmplx(0.0, Xdpp)));
			VThevHarm = cabs(e);   // establish base mag and angle
			ThetaHarm = cang(e);
		}
		else

           // If Generator is off, just set to zero
		{
			VThevHarm = 0.0;
			ThetaHarm = 0.0;
		}
	}
}

// required procedure to initialize the string value of the properties

void TPCPrototypeObj::InitPropertyValues(int ArrayOffset)
{

   // Some examples
	PropertyValue[1] = L'3';        //'phases';
	PropertyValue[2] = GetBus(1);  //'bus1';
	PropertyValue[3] = L"12.47";
     /*...*/
	PropertyValue[26] = Format(L"%-g", ARRAYOFCONST((GenVars.kVArating)));
     /*...*/
	PropertyValue[33] = L"";   // null string


/*Call inherited function to init inherited property values*/
	inherited::InitPropertyValues(NumPropsThisClass);
}

// Initialize state variables, principally for Dynamics analysis

// Example here is standard Generator model; Refer to other modules for other approaches.
// This model uses symmetrical components

void TPCPrototypeObj::InitStateVars()
{
	complex VNeut = {};
	complex Edp = {};
	int i = 0;
	complex V012[3/*# range 0..2*/];
	complex I012[3/*# range 0..2*/];
	complex Vabc[3/*# range 1..3*/];
	YPrimInvalid[ActorID] = true;  // Force rebuild of YPrims
	/*# with GenVars do */
	{
		auto& with0 = GenVars;
		Yeq = cinv(cmplx(0.0, Xdp));

     /*Compute nominal Positive sequence voltage behind transient reactance*/
		if(GenON)
			/*# with ActiveCircuit.Solution do */
			{
				auto& with1 = ActiveCircuit.Solution;
				int stop = 0;
				ComputeIterminal();
				Phase2SymComp(Iterminal, (pComplexArray) &I012);
         // Voltage behind Xdp  (transient reactance), volts
				switch(Connection)
				{
					case 	0:
					VNeut = NodeV[(*NodeRef)[Fnconds - 1]];
					break;
					default:
					VNeut = CZero;
					break;
				}
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					Vabc[i - 1] = NodeV[(*NodeRef)[i - 1]];
				}   // Wye Voltage
				Phase2SymComp((pComplexArray) &Vabc, (pComplexArray) &V012);
				Edp = csub(V012[1], cmul(I012[1], cmplx(0.0, Xdp)));    // Pos sequence
				VthevMag = cabs(Edp);
				Theta = cang(Edp);
				dTheta = 0.0;
				w0 = TwoPi * ActiveCircuit.Solution.Frequency;
         // recalc Mmass and D in case the frequency has changed
				/*# with GenVars do */
				{
					auto& with2 = GenVars;
					GenVars.Mmass = 2.0 * GenVars.Hmass * GenVars.kVArating * 1000.0 / (w0);   // M = W-sec
					D = Dpu * kVArating * 1000.0 / (w0);
				}
				Pshaft = -Get_Power(1, ActiveActor).re; // Initialize Pshaft to present power Output
				Speed = 0.0;    // relative to synch speed
				dSpeed = 0.0;

         // Init User-written models
         //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
				/*# with ActiveCircuit.Solution do */
				{
					auto& with3 = ActiveCircuit.Solution;
					if(GenModel == 6)
					{
						if(UserModel.Get_Exists())
							UserModel.FInit(Vterminal, Iterminal);
					}
				}
			}
		else
		{
			Vthev = CZero;
			Theta = 0.0;
			dTheta = 0.0;
			w0 = 0;
			Speed = 0.0;
			dSpeed = 0.0;
		}
	}  /*With*/
}

/*
  This is a virtual function. You do not need to write this routine
  if you are not integrating state variables in dynamics mode.
*/

// Integrate state variables for Dynamics analysis
// Example from Generator

// Illustrates use of debug tracing

// Present technique is a predictor-corrector trapezoidal rule

void TPCPrototypeObj::IntegrateStates()
{
	complex TracePower = {};
   // Compute Derivatives and then integrate
	ComputeIterminal();

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)
	/*# with ActiveCircuit.Solution, GenVars do */
	{
		auto& with0 = ActiveCircuit.Solution;
		auto& with1 = GenVars;


    // handling of iteration flag
		/*# with DynaVars do */
		{
			auto& with2 = DynaVars;
			if(IterationFlag == 0) /*First iteration of new time step*/
			{
				ThetaHistory = Theta + 0.5 * h * dTheta;
				SpeedHistory = Speed + 0.5 * h * dSpeed;
			}
		}

      // Compute shaft dynamics
		TracePower = TerminalPowerIn(Vterminal, Iterminal, Fnphases);
		dSpeed = (Pshaft + TracePower.re - D * Speed) / Mmass;
		dTheta = Speed;

     // Trapezoidal method
		/*# with DynaVars do */
		{
			auto& with3 = DynaVars;
			Speed = SpeedHistory + 0.5 * h * dSpeed;
			Theta = ThetaHistory + 0.5 * h * dTheta;
		}

      // Write Dynamics Trace Record
		if(DebugTrace)
		{
			Append(Tracefile);
			IOResultToException();
			{ Write(Tracefile); Write(Format(L"t=%-.5g ", ARRAYOFCONST((DynaVars.T)))); }
			{ Write(Tracefile); Write(Format(L" Flag=%d ", ARRAYOFCONST((DynaVars.IterationFlag)))); }
			{ Write(Tracefile); Write(Format(L" Speed=%-.5g ", ARRAYOFCONST((Speed)))); }
			{ Write(Tracefile); Write(Format(L" dSpeed=%-.5g ", ARRAYOFCONST((dSpeed)))); }
			{ Write(Tracefile); Write(Format(L" Pshaft=%-.5g ", ARRAYOFCONST((Pshaft)))); }
			{ Write(Tracefile); Write(Format(L" P=%-.5g Q= %-.5g", ARRAYOFCONST((TracePower.re, TracePower.im)))); }
			{ Write(Tracefile); Write(Format(L" M=%-.5g ", ARRAYOFCONST((Mmass)))); }
			WriteLn(Tracefile);
			CloseFile(Tracefile);
		}

      // Handline of user models, if any
		if(GenModel == 6)
		{
			if(UserModel.Get_Exists())
				UserModel.Integrate;
		}
	}
}

/*
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
*/

/*
 Return i-th state variable one at a time
 Mainly for reports
*/

double TPCPrototypeObj::Get_Variable(int i)
{
	double result = 0.0;
	int n = 0;
	int k = 0;
	n = 0;
	result = -9999.99;  // error return value
	if(i < 1)
		return result;  // Someone goofed
	/*# with GenVars do */
	{
		auto& with0 = GenVars;
		switch(i)
		{
			case 	1:
    // for example, the intrinsic state variables of a Generator
    // change to whatever is appropriate to report in desired units
			result = double((w0 + Speed)) / TwoPi;
			break;  // Frequency, Hz
			case 	2:
			result = (Theta) * RadiansToDegrees;
			break;  // Report in Deg
			case 	3:
			result = cabs(Vthev) / VBase;
			break;      // Report in pu
			case 	4:
			result = Pshaft;
			break;
			case 	5:
			result = dSpeed * RadiansToDegrees;
			break; // Report in Deg      57.29577951
			case 	6:
			result = dTheta;
			break;
			default:
			if(UserModel.Get_Exists())
			{
				n = UserModel.FNumVars;
				k = (i - NumPCPrototypeVariables);
				if(k <= n)
				{
					result = UserModel.FGetVariable(k);
					return result;
				}
			}
			break;
		}
	}
	return result;
}

/*
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
*/

// Sets i-th state variable to specified Value

void TPCPrototypeObj::Set_Variable(int i, double Value)
{
	int n = 0;
	int k = 0;
	n = 0;
	if(i < 1)
		return;  // Someone goofed
	/*# with GenVars do */
	{
		auto& with0 = GenVars;
		switch(i)
		{
			case 	1:
      // for example, the intrinsic state vars of a generator
      // change to appropriate values
			Speed = (Value - w0) * TwoPi;
			break;
			case 	2:
			Theta = Value / RadiansToDegrees;
			break; // deg to rad
			case 	3:
			;
			break;// meaningless to set Vd := Value * vbase; // pu to volts
			case 	4:
			Pshaft = Value;
			break;
			case 	5:
			dSpeed = Value / RadiansToDegrees;
			break;
			case 	6:
			dTheta = Value;
			break;
			default:
			if(UserModel.Get_Exists())
			{
				n = UserModel.FNumVars;
				k = (i - NumPCPrototypeVariables);
				if(k <= n)
				{
					UserModel.FSetVariable(k, Value);
					return;
				}
			}
			break;
		}
	}
}

/*
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
*/

// Return all state variables in double array (allocated by calling function)

void TPCPrototypeObj::GetAllVariables(pDoubleArray States)
{
	int i = 0;
	int n = 0;
	int stop = 0;
	n = 0;
	for(stop = NumPCPrototypeVariables, i = 1; i <= stop; i++)
	{
		(*States)[i - 1] = Variable[i];
	}
	if(UserModel.Get_Exists())
	{
		n = UserModel.FNumVars;
		UserModel.FGetAllVars(&(*States)[NumPCPrototypeVariables + 1 - 1]);
	}
}

/*
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
*/

// Return the number of state variables

// Note: it is not necessary to define any state variables

int TPCPrototypeObj::NumVariables()
{
	int result = 0;
	result = NumPCPrototypeVariables;
	if(UserModel.Get_Exists())
		result = result + UserModel.FNumVars;
	return result;
}

/*
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
*/

// Returns the i-th state variable in a string

String TPCPrototypeObj::VariableName(int i)
{
	String result;
	const int BuffSize = 255;
	int n = 0;
	int I2 = 0;
	Char Buff[256/*# range 0..BuffSize*/];
	PChar PName = nullptr;
	n = 0;
	if(i < 1)
		return result;  // Someone goofed
	switch(i)
	{
		case 	1:
    // For example, these are the 6 intrinsic state variables of a generator
    // Change to appropriate names
		result = L"Frequency";
		break;
		case 	2:
		result = L"Theta (Deg)";
		break;
		case 	3:
		result = L"Vd";
		break;
		case 	4:
		result = L"PShaft";
		break;
		case 	5:
		result = L"dSpeed (Deg/sec)";
		break;
		case 	6:
		result = L"dTheta (Deg)";
		break;
		default:
		if(UserModel.Get_Exists())
		{
			PName = (PChar) &Buff;
			n = UserModel.FNumVars;
			I2 = i - NumPCPrototypeVariables;
			if(I2 <= n)
			{
				UserModel.FGetVarName(I2, PName, BuffSize);
				result = PName;
				return result;
			}
		}
		break;
	}
	return result;
}

// Return i-th property value as a string

String TPCPrototypeObj::GetPropertyValue(int Index)
{
	String result;
	result = L"";   // Init the string
	switch(Index)
	{
		case 	4:
         // Put special cases here
         // often a good idea to convert numeric values to strings, for example
		result = Format(L"%.6g", ARRAYOFCONST((kWBase)));
		break;
		case 	5:
		result = Format(L"%.6g", ARRAYOFCONST((PFNominal)));
		break;

         /*...*/
         // Some values must be enclosed in parens or brackets
		case 	34:
		result = String(L"(") + inherited::GetPropertyValue(Index) + L")";
		break;

         // The default is to just return the current string value of the property
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

/*
  This is a virtual function. You do not need to write this routine
  if the base class function will suffice.
*/

// Routine to convert existing three-phase models to a single-phase positive-
// sequence model

void TPCPrototypeObj::MakePosSequence()
{
	String s;
	double V = 0.0;

/*
     The usual technique is to create a new property editing string
     based on the present values of properties. Once the string is
     created, it is pushed into the Parser and the Edit routine for this
     class is invoked.

     Thus, the positive sequence model is created in memory. Do a
     "Save Circuit" command to save the model that is created. Some
     editing of the resulting scripts will likely be required. Not all
     elements have an obvious positive sequence equivalent.
*/


 // example from Generator class
 // Modify as necessary
	s = L"Phases=1 conn=wye";    // Positive sequence model is 1-phase wye

  // Make sure voltage is line-neutral
	if((Fnphases > 1) || (Connection != 0))
		V = double(GenVars.kVGeneratorBase) / SQRT3;
	else
		V = GenVars.kVGeneratorBase;
	s = s + Format(L" kV=%-.5g", ARRAYOFCONST((V)));

  // Divide the load by no. phases
	if(Fnphases > 1)
	{
		s = s + Format(L" kW=%-.5g  PF=%-.5g", ARRAYOFCONST((double(kWBase) / Fnphases, PFNominal)));
		if(((*PrpSequence)[19 - 1] != 0) || ((*PrpSequence)[20 - 1] != 0))
			s = s
	           + Format(L" maxkvar=%-.5g  minkvar=%-.5g", ARRAYOFCONST((double(kvarMax) / Fnphases, double(kvarMin) / Fnphases)));
		if((*PrpSequence)[26 - 1] > 0)
			s = s + Format(L" kva=%-.5g  ", ARRAYOFCONST((double(GenVars.kVArating) / Fnphases)));
		if((*PrpSequence)[27 - 1] > 0)
			s = s + Format(L" MVA=%-.5g  ", ARRAYOFCONST((double(GenVars.kVArating) / 1000.0 / Fnphases)));
	}
	Parser.CmdString = s;   // Push the string into the Parser object
	Edit();    // Invoke the Edit method for this class
	inherited::MakePosSequence();  // sets the terminal bus references, must do after editing number of phases
}

// Routine for handling Open/Close procedures

void TPCPrototypeObj::Set_ConductorClosed(int Index, bool Value)
{
	inherited::Set_ConductorClosed(Index, Value);

 // In this example from Generator, just turn the object on or off;
	if(Value)
		PCPrototypeSwitchOpen = false;
	else
		PCPrototypeSwitchOpen = true;
}

// Initialize any variables here


  // For Example:  1 + j 1


void PCPrototype_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
}

		class 		PCPrototype_unit
		{
		public:
		PCPrototype_unit()
		{
			//AssertSystemInitialization();
			PCPrototype_initialization();
		}
		};
		PCPrototype_unit _PCPrototype_unit;

}  // namespace PCPrototype




