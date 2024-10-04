#pragma hdrstop

#include "IndMach012.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Command.h"
#include "mathutil.h"
#include "Utilities.h"

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
using namespace Dynamics;
using namespace GeneratorVars;
using namespace LoadShape;
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace IndMach012
{

TIndMach012Obj::TIndMach012Obj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TIndMach012Obj::TIndMach012Obj(String ClassName) : inherited(ClassName) {}
TIndMach012Obj::TIndMach012Obj() {}


TIndMach012* IndMach012Class = nullptr;
TIndMach012Obj* ActiveIndMach012Obj = nullptr;

/*Typical Uses Clause -- not all may not be needed*/     // DSS parser
  // Where class is instantiated
    // Global DSS variables
       // If access to circuit variables is needed
       // DSS command and property support module
      // Delphi misc utility functions
          // Delphi Math functions
      // DSS Math utilities
     // DSS misc utility functions
const int NumPropsThisClass = 21; // Set this constant to the actual number of properties you define
const int NumIndMach012Variables = 22;  // Define any useful module vars here, for example:
complex cBuffer[24/*# range 1..24*/];  // Temp buffer for complex math calcs; allows up to 24-phase models.
complex CDoubleOne = {};   // 1 + j1  (see Initialization section below)

String CmplxArrayToString(pComplexArray cpxarray, int Count)
{
	String result;
	int i = 0;

	auto AppendMagAngle = [&]() -> void 
	{
		result = result
	           + Format(" (%.6g, %.5g)", cabs((cpxarray)[i - 1]), cdang((cpxarray)[i - 1] ));
	};
	result = "[NULL]";
	if(Count > 0)
	{
		int stop = 0;
		result = Format("[%.6g +j %.6g", (cpxarray)[1 - 1].re, (cpxarray)[1 - 1].im);
		i = 1;
		AppendMagAngle();
		for(stop = Count, i = 2; i <= stop; i++)
		{
			result = result
	           + Format(", %.6g +j %.6g", (cpxarray)[i - 1].re, (cpxarray)[i - 1].im);
			AppendMagAngle();
		}
		result = result + "]";
	}
	return result;
}
// Put array values in brackets separated by commas.
// Special version that appends magnitude and angle.


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates main collection handler for all IndMach012 objects

TIndMach012::TIndMach012()
{
	;  // make the base class  and init DSSClassType

     // Specify class name and bit mask ID for this class type
     // IndMach012_ELEMENT must be defined in DSSClassDefs as nn*8
     // First 3 bits are used for base class type (DSSClassType)
	Class_Name = "IndMach012";
	DSSClassType = DSSClassType + INDMACH012_ELEMENT;
	ActiveElement = 0;   // no active elements yet; init to 0

     /*Initialize any other special variables here*/
	DefineProperties();   // This is where the properties for this class are defined

     // Use the Command processor to manage property names
     // PropertyName is an array of String defined in DefineProperties
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	IndMach012Class = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TIndMach012::~TIndMach012()
{


    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This is where the properties are defined, assigned names, indexes, and help strings
// The Help strings will automatically show up when the Help is invoked

void TIndMach012::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
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
	PropertyName[1 - 1] = "phases";
	PropertyName[2 - 1] = "bus1";
	PropertyName[3 - 1] = "kv";
	PropertyName[4 - 1] = "kW";
	PropertyName[5 - 1] = "pf";
	PropertyName[6 - 1] = "conn";
	PropertyName[7 - 1] = "kVA";
	PropertyName[8 - 1] = "H";
	PropertyName[9 - 1] = "D";
	PropertyName[10 - 1] = "puRs";
	PropertyName[11 - 1] = "puXs";
	PropertyName[12 - 1] = "puRr";
	PropertyName[13 - 1] = "puXr";
	PropertyName[14 - 1] = "puXm";
	PropertyName[15 - 1] = "Slip";
	PropertyName[16 - 1] = "MaxSlip";
	PropertyName[17 - 1] = "SlipOption";
	PropertyName[18 - 1] = "Yearly";
	PropertyName[19 - 1] = "Daily";
	PropertyName[20 - 1] = "Duty";
	PropertyName[21 - 1] = "Debugtrace";
	PropertyHelp[1 - 1] = "Number of Phases, this Induction Machine.  ";
	PropertyHelp[2 - 1] = "Bus to which the Induction Machine is connected.  May include specific node specification.";
	PropertyHelp[3 - 1] = "Nominal rated (1.0 per unit) voltage, kV. For 2- and 3-phase machines, specify phase-phase kV. "
	           "Otherwise, specify actual kV across each branch of the machine. "
	           "If wye (star), specify phase-neutral kV. "
	           "If delta or phase-phase connected, specify phase-phase kV.";  // line-neutral voltage//  base voltage
	PropertyHelp[4 - 1] = String("Shaft Power, kW, for the Induction Machine.  A positive value denotes power for a load. ") + CRLF
	           + "Negative value denotes an induction generator. ";
	PropertyHelp[5 - 1] = "[Read Only] Present power factor for the machine. ";
	PropertyHelp[6 - 1] = "Connection of stator: Delta or Wye. Default is Delta.";
	PropertyHelp[7 - 1] = "Rated kVA for the machine.";
	PropertyHelp[8 - 1] = "Per unit mass constant of the machine.  MW-sec/MVA.  Default is 1.0.";
	PropertyHelp[9 - 1] = "Damping constant.  Usual range is 0 to 4. Default is 1.0.  Adjust to get damping in Dynamics mode,";
	PropertyHelp[10 - 1] = "Per unit stator resistance. Default is 0.0053.";
	PropertyHelp[11 - 1] = "Per unit stator leakage reactance. Default is 0.106.";
	PropertyHelp[12 - 1] = "Per unit rotor  resistance. Default is 0.007.";
	PropertyHelp[13 - 1] = "Per unit rotor leakage reactance. Default is 0.12.";
	PropertyHelp[14 - 1] = "Per unit magnetizing reactance.Default is 4.0.";
	PropertyHelp[15 - 1] = "Initial slip value. Default is 0.007";
	PropertyHelp[16 - 1] = "Max slip value to allow. Default is 0.1. Set this before setting slip.";
	PropertyHelp[17 - 1] = "Option for slip model. One of {fixedslip | variableslip*  }";
	PropertyHelp[18 - 1] = "LOADSHAPE object to use for yearly simulations.  Must be previously defined "
	           "as a Loadshape object. Is set to the Daily load shape "
	           " when Daily is defined.  The daily load shape is repeated in this case. "
	           "Set Status=Fixed to ignore Loadshape designation. "
	           "Set to NONE to reset to no loadahape. "
	           "The default is no variation.";
	PropertyHelp[19 - 1] = "LOADSHAPE object to use for daily simulations.  Must be previously defined "
	           "as a Loadshape object of 24 hrs, typically. "
	           "Set Status=Fixed to ignore Loadshape designation. "
	           "Set to NONE to reset to no loadahape. "
	           "Default is no variation (constant) if not defined. "
	           "Side effect: Sets Yearly load shape if not already defined.";
	PropertyHelp[20 - 1] = "LOADSHAPE object to use for duty cycle simulations.  Must be previously defined "
	           "as a Loadshape object.  Typically would have time intervals less than 1 hr. "
	           "Designate the number of points to solve using the Set Number=xxxx command. "
	           "If there are fewer points in the actual shape, the shape is assumed to repeat."
	           "Set to NONE to reset to no loadahape. "
	           "Set Status=Fixed to ignore Loadshape designation. "
	           " Defaults to Daily curve If not specified.";
	PropertyHelp[21 - 1] = "[Yes | No*] Write DebugTrace file.";


     /* add properties here */

     // Finally, we have to pick up any properties that were inherited
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // You can optionally override default help string of an inherited property, for example
	PropertyHelp[NumPropsThisClass + 1 - 1] = "Name of harmonic voltage or current spectrum for this IndMach012. "
	           "Voltage behind Xd\" for machine - default. Current injection for inverter. "
	           "Default value is \"default\", which is defined when the DSS starts.";
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This function is called  by the DSS whenever a New IndMach012... command is encountered

int TIndMach012::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new IndMach012 and add it to IndMach012 class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TIndMach012Obj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This is a typical helper function found in many DSS circuit element class
// for defining the number of conductors per terminal (Nconds) based on Y or delta connection

void TIndMach012::SetNcondsForConnection()
{
	/*# with ActiveIndMach012Obj do */
	{
		auto with0 = ActiveIndMach012Obj;
		switch(with0->Connection)
		{
			case 	0:
			with0->Set_Nconds(with0->Fnphases);
			break;  // Neutral is not connected for induction machine
			case 	1:
			switch(with0->Fnphases)
			{
				case 	1:
				 case 2:        // Delta connection
				with0->Set_Nconds(with0->Fnphases + 1);
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


//- - - - - - - - - - - - - MAIN EDIT FUNCTION  - - - - - - - - - - - - - - -
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// This function is the heart of the property managment for this class

int TIndMach012::Edit(int ActorID)
{
	int result = 0;     // Define some local vars for handling parser results
	int ParamPointer = 0;
	String ParamName;
	String Param;

// The Edit function starts where the Parser is presently pointing and
// manages the parsing of the rest of the command line in the parser.

// The DSS executive processes the command verb on the front of the line and
// then passes control to the appropriate Edit function


  // set the present element active
  // and continue parsing with contents of Parser
	ActiveIndMach012Obj = (TIndMach012Obj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveIndMach012Obj);
	result = 0;
	/*# with ActiveIndMach012Obj do */
	{
		auto with0 = ActiveIndMach012Obj;
     // peel off the next token on the edit line
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while(Param.size() > 0)

         // Find the index for the CASE statement
         // If property is not named, just increment the index to the next property
		{
			if(ParamName.size() == 0)
				++ParamPointer;
			else
				ParamPointer = CommandList.Getcommand(ParamName);

         // Update the PropertyValy for this property
         // Actual index is mapped via PropertyIdxMap array for this class
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))
				with0->Set_PropertyValue((PropertyIdxMap)[ParamPointer - 1],Param);
			else
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for IndMach012 \""
	           + with0->get_Name()
	           + "\"", 560);

         // --------------- MAIN CASE STATEMENT ----------------------
			if(ParamPointer > 0)
				switch((PropertyIdxMap)[ParamPointer - 1])
				{
         // since we used AddProperty function to define properties, have to
         // use PropertyIdxMap to map to the correct Case index
					case 	0:
					DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 561);
					break;
					case 	1:
					with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
					break; // num phases
					case 	2:
					with0->SetBus(1, Param);
					break;
					case 	3:
					with0->Set_PresentkV(Parser[ActorID]->MakeDouble_());
					break;
					case 	4:
					with0->kWBase = Parser[ActorID]->MakeDouble_();
					break;
					case 	5:
					;
					break; // Do nothing; read only power factor    := Parser.MakeDouble_();
					case 	6:
					InterpretConnection(Parser[ActorID]->MakeString_());
					break;
					case 	7:
					with0->MachineData.kVArating = Parser[ActorID]->MakeDouble_();
					break;
					case 	8:
					with0->MachineData.Hmass = Parser[ActorID]->MakeDouble_();
					break;
					case 	9:
					with0->MachineData.D = Parser[ActorID]->MakeDouble_();
					break;
					case 	10:
					with0->puRs = Parser[ActorID]->MakeDouble_();
					break;
					case 	11:
					with0->puXs = Parser[ActorID]->MakeDouble_();
					break;
					case 	12:
					with0->puRr = Parser[ActorID]->MakeDouble_();
					break;
					case 	13:
					with0->puXr = Parser[ActorID]->MakeDouble_();
					break;
					case 	14:
					with0->puXm = Parser[ActorID]->MakeDouble_();
					break;
					case 	15:
					with0->Set_Slip(Parser[ActorID]->MakeDouble_());
					break;
					case 	16:
					with0->MaxSlip = Parser[ActorID]->MakeDouble_();
					break;
					case 	17:
					with0->InterpretOption(Parser[ActorID]->MakeString_());
					break;
					case 	18:
					with0->YearlyShape = Param;
					break;
					case 	19:
					with0->DailyDispShape = Param;
					break;
					case 	20:
					with0->DutyShape = Param;
					break;
					case 	21:
					with0->DebugTrace = InterpretYesNo(Param);
					break;
           // Handle Inherited properties
					default:
					inherited::ClassEdit(ActiveIndMach012Obj, ParamPointer - NumPropsThisClass);
					break;
				}

         // ---------------- SIDE EFFECTS CASE STATEMENT ---------------------
         // This case statment handles any side effects from setting a property
         // (for example, from Generator)
			if(ParamPointer > 0)
				switch((PropertyIdxMap)[ParamPointer - 1])
				{
					case 	1:
					SetNcondsForConnection();
					break;  // Force Reallocation of terminal info
					case 	18:
					{
						with0->YearlyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->YearlyShape));
						if(ASSIGNED(with0->YearlyShapeObj))
							/*# with YearlyShapeObj do */
							{
								auto with1 = with0->YearlyShapeObj;
								if(with1->UseActual)
									with0->SetPowerkW(with1->MaxP);
							}
					}
					break;
					case 	19:
					{
						with0->DailyDispShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DailyDispShape));
						if(ASSIGNED(with0->DailyDispShapeObj))
							/*# with DailyDispShapeObj do */
							{
								auto with2 = with0->DailyDispShapeObj;
								if(with2->UseActual)
									with0->SetPowerkW(with2->MaxP);
							}
					}
					break;
					case 	20:
					{
						with0->DutyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DutyShape));
						if(ASSIGNED(with0->DutyShapeObj))
							/*# with DutyShapeObj do */
							{
								auto with3 = with0->DutyShapeObj;
								if(with3->UseActual)
									with0->SetPowerkW(with3->MaxP);
							}
					}
					break;
					default:
					  ;
					break;
				}

         // Get next token off Parser and continue editing properties
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}

     // After editing is complete, the typical next step is to call the RecalcElementData function
		with0->RecalcElementData(ActorID);
		with0->Set_YprimInvalid(ActorID,true); // Setting this flag notifies the DSS that something has changed
                           // and the Yprim will have to be rebuilt
	}
	return result;
}

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------


// This function should be defined to handle the Like property inherited from
// the base class.

// The function copies the essential properties of another object of this class

int TIndMach012::MakeLike(const String OtherIndMach012Name)
{
	int result = 0;
	TIndMach012Obj* OtherIndMach012 = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this IndMach012 name in the present collection*/
	OtherIndMach012 = ((TIndMach012Obj*) Find(OtherIndMach012Name));
	if(OtherIndMach012 != nullptr)
		/*# with ActiveIndMach012Obj do */
		{
			auto with0 = ActiveIndMach012Obj;   // skip if not found

       // You should first set the basic circuit element properties, for example
			int stop = 0;
			if(with0->Fnphases != OtherIndMach012->Fnphases)
			{
				with0->Set_NPhases(OtherIndMach012->Fnphases);
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->Set_PresentkV(OtherIndMach012->Get_PresentkV());
			with0->kWBase = OtherIndMach012->kWBase;
			with0->puRs = OtherIndMach012->puRs;
			with0->puRr = OtherIndMach012->puRr;
			with0->puXr = OtherIndMach012->puXr;
			with0->puXm = OtherIndMach012->puXm;
			with0->puXs = OtherIndMach012->puXs;
			with0->MaxSlip = OtherIndMach012->MaxSlip;
			with0->MachineData.kVArating = OtherIndMach012->MachineData.kVArating;
			with0->MachineData.Hmass = OtherIndMach012->MachineData.Hmass;
			with0->MachineData.D = OtherIndMach012->MachineData.D;

       // Do inherited properties
			ClassMakeLike(OtherIndMach012);

       // Finally initialize all the property value strings to be the same as
       // the copied element
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				if(i != 5)
          // Skip read only properties
					with0->FPropertyValue[i - 1] = OtherIndMach012->FPropertyValue[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Load MakeLike: \"") + OtherIndMach012Name
	           + "\" Not Found.", 562);
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// Optional function if you want to do anything to initialize objects of this class

int TIndMach012::Init(int Handle, int ActorID)
{
	int result = 0;
	TIndMach012Obj* P = nullptr;
	if(Handle == 0)  // init all
	{
		P = (TIndMach012Obj*) ElementList.Get_First();
		while((P != nullptr))
		{
			P->Randomize(0);
			P = (TIndMach012Obj*) ElementList.Get_Next();
		}
	}
	else
	{
		Set_Active(Handle);
		P = ((TIndMach012Obj*) GetActiveObj());
		P->Randomize(0);
	}
	DoSimpleMsg("Need to implement TIndMach012.Init", -1);
	result = 0;
	return result;
}


//------------------------- MAIN OBJECT CONSTRUCTOR ---------------------

//----------------------------------------------------------------------------

TIndMach012Obj::TIndMach012Obj(TDSSClass* ParClass, const String IndMach012ObjName)
 : inherited(ParClass),
			Connection(1),
			puRs(0.0),
			puXs(0.0),
			puRr(0.0),
			puXr(0.0),
			puXm(0.0),
			S1(0.0),
			S2(0.0),
			MaxSlip(0.0),
			dSdP(0.0),
			Xopen(0.0),
			Xp(0.0),
			T0p(0.0),
			InDynamics(false),
			FirstIteration(false),
			FixedSlip(false),
			RandomMult(0.0),
			IndMach012SolutionCount(0),
			IndMach012SwitchOpen(false),
			DebugTrace(false),
			MachineON(false),
			ShapeIsActual(false),
			VBase(0.0),
			kWBase(0.0),
			DailyDispShapeObj(nullptr),
			DutyShapeObj(nullptr),
			YearlyShapeObj(nullptr)
{
	Set_Name(LowerCase(IndMach012ObjName));
	DSSObjType = ParClass->DSSClassType; // Same as Parent Class

     // Set some basic circuit element properties
	Set_NPhases(3);  // typical DSS default for a circuit element
	Fnconds = 3;  // defaults to delta
	Yorder = 0;  // To trigger an initial allocation
	Set_NTerms(1);  // forces allocations of terminal quantities
	kWBase = 1000.0;
	YearlyShape = "";
	YearlyShapeObj = nullptr;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
	DailyDispShape = "";
	DailyDispShapeObj = nullptr;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
	DutyShape = "";
	DutyShapeObj = nullptr;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
	DebugTrace = false;
	Yorder = Fnterms * Fnconds;
	ShapeIsActual = false;
	IndMach012SwitchOpen = false;
	MachineData.kVGeneratorBase = 12.47;
	MachineData.kVArating = kWBase * 1.2;
	/*# with MachineData do */
	{
		auto& with0 = MachineData;
		with0.Hmass = 1.0;       //  W-sec/VA rating
		with0.Theta = 0.0;
		with0.w0 = TwoPi * BaseFrequency;
		with0.Speed = 0.0;  // relative speed
		with0.dSpeed = 0.0;
		with0.D = 1.0;
		with0.XRdp = 20.0;   // not used for indmach

           // newly added
		with0.Conn = Connection;
		with0.NumPhases = Fnphases;
		with0.NumConductors = Fnconds;
	}

    /*---- end note Andres: from dll model ----*/

    /*Typical machine impedance data*/
	puRs = 0.0053;
	puXs = 0.106;
	puRr = 0.007;
	puXr = 0.12;
	puXm = 4.0;

      // Set slip local and make generator model agree
	MaxSlip = 0.1;  // 10% slip limit     - set this before setting slip
	Set_Slip(0.007);   // About 1 pu power
	FixedSlip = false;  // Allow Slip to float to match specified power
	InDynamics = false;

     // call the procedure to set the initial property string values
	InitPropertyValues(0);

     // Update anything that has to be calculated from property values
	RecalcElementData(ActiveActor);
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// Free everything here that needs to be freed
// If you allocated anything, dispose of it here

TIndMach012Obj::~TIndMach012Obj()
{
	// inherited::Destroy();   // This will take care of most common circuit element arrays, etc.
}



//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::RecalcElementData(int ActorID)
{
	double Rs = 0.0;
	double Xs = 0.0;
	double rr = 0.0;
	double Xr = 0.0;
	double XM = 0.0;
	double ZBase = 0.0;
	/*# with MachineData do */
	{
		auto& with0 = MachineData;
		ZBase = Sqr(with0.kVGeneratorBase) / with0.kVArating * 1000.0;
		with0.Conn = Connection;
		with0.NumPhases = Fnphases;
		with0.NumConductors = Fnconds;
	}
	Rs = puRs * ZBase;
	Xs = puXs * ZBase;
	rr = puRr * ZBase;
	Xr = puXr * ZBase;
	XM = puXm * ZBase;
	Zs = cmplx(Rs, Xs);
	ZM = cmplx(0.0, XM);
	Zr = cmplx(rr, Xr);
	Xopen = Xs + XM;
	Xp = Xs + (Xr * XM) / (Xr + XM);
	Zsp = cmplx(Rs, Xp);
    //Yeq := Cinv(Zsp);   // for Yprim  for dynamics
    //Yeq := Cmplx(1.0/ZBase, -0.5/Zbase);   // vars are half the watts
	Yeq = cmplx(0.0, -1.0 / ZBase);   // vars only for power flow
	T0p = (Xr + XM) / (MachineData.w0 * rr);
	dSdP = Compute_dSdP();
	Is1 = CZero;
	V1 = CZero;
	Is2 = CZero;
	V2 = CZero;
	FirstIteration = true;
	InjCurrent = (pComplexArray) realloc(InjCurrent, sizeof(complex) * Yorder);
	SetNominalPower(ActorID);
	if(CompareText(YearlyShape, "none") == 0)
		YearlyShape = "";
	if(CompareText(DailyDispShape, "none") == 0)
		DailyDispShape = "";
	if(CompareText(DutyShape, "none") == 0)
		DutyShape = "";
	if(YearlyShapeObj == nullptr)
	{
		if(YearlyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Yearly load shape: \"") + YearlyShape
	           + "\" Not Found.", 563);
	}
	if(DailyDispShapeObj == nullptr)
	{
		if(DailyDispShape.size() > 0)
			DoSimpleMsg(String("WARNING! Daily load shape: \"") + DailyDispShape
	           + "\" Not Found.", 564);
	}
	if(DutyShapeObj == nullptr)
	{
		if(DutyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Duty load shape: \"") + DutyShape
	           + "\" Not Found.", 565);
	}
	SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
	if(SpectrumObj == nullptr)
		DoSimpleMsg(String("ERROR! Spectrum \"") + Spectrum + "\" Not Found.", 566);
	if(DebugTrace)
		InitTraceFile();
}

double TIndMach012Obj::Get_PresentkV()
{
	double result = 0.0;
	result = MachineData.kVGeneratorBase;
	return result;
}

void TIndMach012Obj::Set_PresentkV(double Value)
{
	/*# with MachineData do */
	{
		auto& with0 = MachineData;
		with0.kVGeneratorBase = Value;
		switch(Fnphases)
		{
			case 	2:
			 case 3:
			VBase = with0.kVGeneratorBase * InvSQRT3x1000;
			break;
			default:
			VBase = with0.kVGeneratorBase * 1000.0;
			break;
		}
	}
}

void TIndMach012Obj::InterpretOption(String s)
{
	switch(UpperCase(s)[1])
	{
		case 	L'F':
		FixedSlip = true;
		break;
		case 	L'V':
		FixedSlip = false;
		break;
		default:
		  ;
		break;
	}
}

//----------------------------------------------------------------------------

void TIndMach012Obj::SetPowerkW(double PkW)
{
	kWBase = PkW;
}

//----------------------------------------------------------------------------
//--------------------- MAIN CALC ROUTINES -----------------------------------

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::Integrate(int ActorID)
{
	double H2 = 0.0;
	/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
	{
		auto& with0 = ActiveCircuit[ActorID]->Solution->DynaVars;
		if(with0.IterationFlag == 0)  // on predictor step
		{
			E1n = E1;            // update old values
			dE1dtn = dE1dt;
			E2n = E2;
			dE2dtn = dE2dt;
		}

     // Derivative of E
      // dEdt = -jw0SE' - (E' - j(X-X')I')/T0'
		dE1dt = csub(cmul(cmplx(0.0, -MachineData.w0 * S1), E1), cdivreal(csub(E1, cmul(cmplx(0.0, (Xopen - Xp)), Is1)), T0p));
		dE2dt = csub(cmul(cmplx(0.0, -MachineData.w0 * S2), E2), cdivreal(csub(E2, cmul(cmplx(0.0, (Xopen - Xp)), Is2)), T0p));

      // Trapezoidal Integration
		H2 = with0.h * 0.5;
		E1 = cadd(E1n, cmulreal(cadd(dE1dt, dE1dtn), H2));
		E2 = cadd(E2n, cmulreal(cadd(dE2dt, dE2dtn), H2));
	}
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::CalcDynamic(TSymCompArray V012, TSymCompArray I012)
{

      /*In dynamics mode, slip is allowed to vary*/
	InDynamics = true;
	V1 = V012[1];   // Save for variable calcs
	V2 = V012[2];
      /*Gets slip from shaft speed*/
	/*# with MachineData do */
	{
		auto& with0 = MachineData;
		set_Localslip((-with0.Speed) / with0.w0);
	}
	Get_DynamicModelCurrent();

     //  Get_ModelCurrent(V2, S2, Is2, Ir2);
	I012[1] = Is1;    // Save for variable calcs
	I012[2] = Is2;
	I012[0] = cmplx(0.0, 0.0);
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::CalcPFlow(TSymCompArray V012, TSymCompArray I012)
{
	double P_Error = 0.0;
	V1 = V012[1];   // Save for variable calcs
	V2 = V012[2];
	InDynamics = false;
	if(FirstIteration)
	{
		Get_PFlowModelCurrent(V1, S1, Is1, Ir1);  // initialize Is1
		FirstIteration = false;
	}

      /*If Fixed slip option set, then use the value set by the user*/
	if(!FixedSlip)
	{
		P_Error = MachineData.Pnominalperphase - cmul(V1, conjg(Is1)).re;
		set_Localslip(S1 + dSdP * P_Error);   // make new guess at slip
	}
	Get_PFlowModelCurrent(V1, S1, Is1, Ir1);
	Get_PFlowModelCurrent(V2, S2, Is2, Ir2);
	I012[1] = Is1;    // Save for variable calcs
	I012[2] = Is2;
	I012[0] = cmplx(0.0, 0.0);
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// typical proc for handling randomization in DSS fashion

void TIndMach012Obj::Randomize(int Opt)
{
	switch(Opt)
	{
		case 	0:
		RandomMult = 1.0;
		break;
    //   GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
		case 	UNIFORM:
		RandomMult = (double) Random();
		break;  // number between 0 and 1.0
     //  LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
		default:
		  ;
		break;
	}
}


/*-------------------------------------------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------------------------------------------*/

// Init for Dynamics mode

void TIndMach012Obj::InitModel(const TSymCompArray& V012, const TSymCompArray& I012)
{


   // Compute Voltage behind transient reactance and set derivatives to zero
  // *** already done *** E1 := csub(V012[1], cmul(I012[1], Zsp));
	dE1dt = CZero;
	E1n = E1;
	dE1dtn = dE1dt;
	E2 = csub(V012[2], cmul(I012[2], Zsp));
	dE2dt = CZero;
	E2n = E2;
	dE2dtn = dE2dt;
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::InitStateVars(int ActorID)
{
	int i = 0;
	TSymCompArray V012 = {};
	TSymCompArray I012 = {};
	complex Vabc[4] = { CZero, CZero ,CZero ,CZero };

	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims
	/*# with MachineData do */
	{
		auto& with0 = MachineData;

     /*Compute nominal Positive sequence voltage behind transient reactance*/
		if(MachineON)
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with1 = ActiveCircuit[ActorID]->Solution;
				Yeq = cinv(Zsp);
				TPCElement::ComputeIterminal(ActorID);
				switch(Fnphases)
				{
					case 	1:
					{
						if(!ADiakoptics || (ActorID == 1))
							E1 = csub(csub(with1->NodeV[(NodeRef)[1 - 1]], with1->NodeV[(NodeRef)[2 - 1]]), cmul((Iterminal)[1 - 1], Zsp));
						else
							E1 = csub(csub(with1->VoltInActor1((NodeRef)[1 - 1]), with1->VoltInActor1((NodeRef)[2 - 1])), cmul((Iterminal)[1 - 1], Zsp));
					}
					break;
                 // Calculate E1 based on Pos Seq only
					case 	3:
					{
						int stop = 0;
						Phase2SymComp(&(Iterminal[0]), &I012[0]);   // terminal currents

                     // Voltage behind Zsp  (transient reactance), volts
						for(stop = Fnphases, i = 1; i <= stop; i++)
						{
							if(!ADiakoptics || (ActorID == 1))
								Vabc[i - 1] = with1->NodeV[(NodeRef)[i - 1]];
							else
								Vabc[i - 1] = with1->VoltInActor1((NodeRef)[i - 1]);   // Wye Voltage
						}
						Phase2SymComp(&Vabc[0], &V012[0]);
						E1 = csub(V012[1], cmul(I012[1], Zsp));    // Pos sequence
					}
					break;
					default:
					DoSimpleMsg(Format(("Dynamics mode is implemented only for 1- or 3-phase Motors. IndMach012." + with1->get_Name()
					+ " has %d phases.").c_str(), Fnphases), 5672);
					SolutionAbort = true;
					break;
				}
				InitModel(V012, I012); // E2, etc

         // Shaft variables
				with0.Theta = cang(E1);
				with0.dTheta = 0.0;
				with0.w0 = TwoPi * ActiveCircuit[ActorID]->Solution->get_FFrequency();
         // recalc Mmass and D in case the frequency has changed
				/*# with MachineData do */
				{
					auto& with2 = MachineData;
					with2.Mmass = 2.0 * with2.Hmass * with2.kVArating * 1000.0 / (with2.w0);   // M = W-sec
					with2.D = with2.Dpu * with2.kVArating * 1000.0 / (with2.w0);
				}
				with0.Pshaft = Get_Power(1, ActorID).re; // Initialize Pshaft to present power consumption of motor
				with0.Speed = -get_S1() * with0.w0;    // relative to synch speed
				with0.dSpeed = 0.0;
				if(DebugTrace)     // Put in a separator record
				{
					Append(Tracefile);
					IOResultToException();
					WriteLn(Tracefile);
					WriteLn(Tracefile, "*************** Entering Dynamics Mode ***********************");
					WriteLn(Tracefile);
					Close(Tracefile);
				}
			}
		else
		{
			with0.Theta = 0.0;
			with0.dTheta = 0.0;
			with0.w0 = 0;
			with0.Speed = 0.0;
			with0.dSpeed = 0.0;
		}
	}  /*With*/
}

//----------------------------------------------------------------------------

double TIndMach012Obj::get_S1()
{
	return S1;
}

//----------------------------------------------------------------------------


/*A typical helper function for PC elements to assist in the computation
 of Yprim
*/

void TIndMach012Obj::CalcYPrimMatrix(TcMatrix* Ymatrix, int ActorID)
{
	complex Y = {};
	complex Yij = {};
	complex Yadder = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;
	FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
	FreqMultiplier = FYprimFreq / BaseFrequency;  // ratio to adjust reactances for present solution frequency
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(with0->IsDynamicModel || with0->IsHarmonicModel)
   // for Dynamics and Harmonics modes use constant equivalent Y
		{
			int stop = 0;
			if(MachineON)   // L-N value computed in initialization routines
				Y = Yeq;
			else
				Y = cmplx(EPSILON, 0.0);
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
                 /*
                   Ymatrix.AddElement(Fnconds, Fnconds, Y);  // sums the element
                   Ymatrix.SetElemsym(i, Fnconds, Yij);
                 */
					}
					break;   /*Delta connection*/
					case 	1:
					{
						int stop1 = 0;
						Yadder = cmulreal(Y, 1.000001);  // to prevent floating delta
						Ymatrix->SetElement(i, i, cadd(Y, Yadder));   // add a little bit to diagonal
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
    //  Borrowed from Generator object
		
       /*Yeq is typically expected as the equivalent line-neutral admittance*/
		{
			Y = Yeq;  //     Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
			Y.im = Y.im / FreqMultiplier;
			switch(Connection)
			{
				case 	0:
				/*# with Ymatrix do */
				{
					auto with1 = Ymatrix; // WYE
					int stop = 0;
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						with1->SetElement(i, i, Y);
                     /*
                     AddElement(Fnconds, Fnconds, Y);
                     SetElemsym(i, Fnconds, Yij);
                     */
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

/*--- Notes Andres: Added according to IndMach012.dll model */

double TIndMach012Obj::Compute_dSdP()
{
	double result = 0.0;
// dSdP based on rated slip and rated voltage
	V1 = cmplx(MachineData.kVGeneratorBase * 1000.0 / 1.732, 0.0);
	if(S1 != 0.0)
		Get_PFlowModelCurrent(V1, S1, Is1, Ir1);
	result = S1 / cmul(V1, conjg(Is1)).re;
	return result;
}

//----------------------------------------------------------------------------


// Required routine to calculate the primitive Y matrix for this element

// This example uses a helper function (CalcYPrimMatrix) to keep the code
// here clean

void TIndMach012Obj::CalcYPrim(int ActorID)
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
	if(Get_YprimInvalid(ActorID,0))
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


     // call helper routine to compute YPrim_Shunt
	CalcYPrimMatrix(YPrim_Shunt, ActorID);

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
	TDSSCktElement::CalcYPrim(ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
/*Compute total terminal Current */

void TIndMach012Obj::DoIndMach012Model(int ActorID)
{
	int i = 0;
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcModel(&(Vterminal[0]), &(Iterminal[0]), ActorID);
	set_ITerminalUpdated(true, ActorID);
	for(stop = Get_NPhases(), i = 1; i <= stop; i++)
	{
		caccum((InjCurrent)[i - 1], cnegate((Iterminal)[i - 1]));
	}
	if(DebugTrace)
		WriteTraceRecord(ActorID);
} // given voltages returns currents

void TIndMach012Obj::CalcModel(pComplexArray V, pComplexArray i, int ActorID)
{
	TSymCompArray V012 = {};
	TSymCompArray I012 = {};

    // Convert abc voltages to 012
	Phase2SymComp(V, &V012[0]);

    // compute I012
	switch(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode)
	{
		case 	DYNAMICMODE:
		{
			CalcDynamic(V012, I012);
		}
		break;  /*All other modes are power flow modes*/
		default:
		CalcPFlow(V012, I012);
		break;
	}
	SymComp2Phase(i, &I012[0]);       // convert back to I abc
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/* This is an example taken from Generator illustrating how a PC element might
  handle Dynamics mode with a Thevenin equivalent

  Also illustrates the computation of symmetrical component values
*/

/*Compute Total Current and add into InjTemp*/

void TIndMach012Obj::DoDynamicMode(int ActorID)
{
	int i = 0;

   // Start off by getting the current in the admittance branch of the model
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array

   /*Inj = -Itotal (in) - Yprim*Vtemp*/
	CalcModel(&(Vterminal[0]), &(Iterminal[0]), ActorID);
	set_ITerminalUpdated(true, ActorID);
	for(stop = Get_NPhases(), i = 1; i <= stop; i++)
	{
		caccum((InjCurrent)[i - 1], cnegate((Iterminal)[i - 1]));
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

void TIndMach012Obj::DoHarmonicMode(int ActorID)
{
	int i = 0;
	complex e = {};
	double GenHarmonic = 0.0;

   // Set the VTerminal array
	ComputeVterminal(ActorID);
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		int stop = 0;
		GenHarmonic = with0->get_FFrequency() / BaseFrequency; // harmonic based on the fundamental for this object
        // get the spectrum multiplier and multiply by the V thev (or Norton current for load objects)
      // ???  E := CmulReal(SpectrumObj.GetMult(GenHarmonic), VThevHarm); // Get base harmonic magnitude
      // ???  RotatePhasorRad(E, GenHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift

        // Put the values in a temp complex buffer
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			cBuffer[i - 1] = e;
			if(i < Fnphases)
				RotatePhasorDeg(e, GenHarmonic, -120.0);  // Assume 3-phase IndMach012
		}
	}

   /*Handle Wye Connection*/
	if(Connection == 0)
		cBuffer[Fnconds - 1] = (Vterminal)[Fnconds - 1];  // assume no neutral injection voltage

   // In this case the injection currents are simply Yprim(frequency) times the voltage buffer
   // Refer to Load.Pas for load-type objects
		   /*Inj currents = Yprim (E) */
	YPrim->MVmult(InjCurrent, (pComplexArray) &cBuffer);
}




// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Main dispatcher for computing PC Element currnts

// Calculates IndMach012 current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

void TIndMach012Obj::CalcIndMach012ModelContribution(int ActorID)
{
	set_ITerminalUpdated(false, ActorID);
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
	{
		
		auto with1 = ActiveCircuit[ActorID]->Solution;
		if(with1->IsDynamicModel)
			DoDynamicMode(ActorID);
		else
		{
			if(with1->IsHarmonicModel && (with1->get_FFrequency() != ActiveCircuit[ActorID]->Fundamental))
				DoHarmonicMode(ActorID);
			else
				DoIndMach012Model(ActorID);
		}
	} /*WITH*/

   /*When this is done, ITerminal is up to date*/
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

// Main procedure for controlling computation of InjCurrent array

// InjCurrent is difference between currents in YPrim and total terminal current

void TIndMach012Obj::CalcInjCurrentArray(int ActorID)
{


// You usually will want some logic like this

       // If the element is open, just zero the array and return
	if(IndMach012SwitchOpen)

       // otherwise, go to a routine that manages the calculation
		ZeroInjCurrent();
	else
		CalcIndMach012ModelContribution(ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

// This function controls the calculation of the total terminal currents

// Note that it only does something if the solution count has changed.
// Otherwise, Iterminal array already contains the currents

void TIndMach012Obj::GetTerminalCurrents(pComplexArray Curr, int ActorID)
{
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(IterminalSolutionCount[ActorID] != ActiveCircuit[ActorID]->Solution->SolutionCount)     // recalc the contribution
          // You will likely want some logic like this
		{
			if(!IndMach012SwitchOpen)
				CalcIndMach012ModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
		}
		inherited::GetTerminalCurrents(Curr, ActorID); // add in inherited contribution
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

// Required function for managing computing of InjCurrents

int TIndMach012Obj::InjCurrents(int ActorID)
{
	int result = 0;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;

      // Generators and Loads use logic like this:
		if(with0->LoadsNeedUpdating)
			SetNominalPower(ActorID); // Set the nominal kW, etc for the type of solution being done

       // call the main function for doing calculation
		CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current

      // If (DebugTrace) Then WriteTraceRecord;

       // Add into System Injection Current Array
		result = inherited::InjCurrents(ActorID);
	}
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// Set shaft power

void TIndMach012Obj::SetNominalPower(int ActorID)
{
	double Factor = 0.0;
	bool MachineOn_Saved = false;
	MachineOn_Saved = MachineON;
	ShapeFactor = CDoubleOne;
    // Check to make sure the generation is ON
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
	{
		
		auto with1 = ActiveCircuit[ActorID]->Solution;
		if(!(with1->IsDynamicModel || with1->IsHarmonicModel))     // Leave machine in whatever state it was prior to entering Dynamic mode
		{
			MachineON = true;   // Init to on then check if it should be off
		}
		if(!MachineON)
         // If Machine is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
		{
			MachineData.Pnominalperphase = -0.1 * kWBase / Fnphases;
          // Pnominalperphase   := 0.0;
			MachineData.Qnominalperphase = 0.0;   // This really doesn't matter
		}
		else
    // Generator is on, compute it's nominal watts and vars
		{
			/*# with Solution do */
			{
				auto with2 = ActiveCircuit[ActorID]->Solution;
				switch(with2->Get_SolMode())
				{
					case 	SNAPSHOT:
					Factor = 1.0;
					break;
					case 	DAILYMODE:
					{
						Factor = 1.0; // Daily dispatch curve
						CalcDailyMult(with2->DynaVars.dblHour);
					}
					break;
					case 	YEARLYMODE:
					{
						Factor = 1.0;
						CalcYearlyMult(with2->DynaVars.dblHour);
					}
					break;
					case 	DUTYCYCLE:
					{
						Factor = 1.0;
						CalcDutyMult(with2->DynaVars.dblHour);
					}
					break;   // General sequential time simulation
					case 	GENERALTIME: case DYNAMICMODE:
					{
						Factor = 1.0;
                                   // This mode allows use of one class of load shape
						switch(ActiveCircuit[ActorID]->ActiveLoadShapeClass)
						{
							case 	USEDAILY:
							CalcDailyMult(with2->DynaVars.dblHour);
							break;
							case 	USEYEARLY:
							CalcYearlyMult(with2->DynaVars.dblHour);
							break;
							case 	USEDUTY:
							CalcDutyMult(with2->DynaVars.dblHour);
							break;
							default:     // default to 1 + j1 if not known
							ShapeFactor = CDoubleOne;
							break;
						}
					}
					break;
					case 	MONTECARLO1: case MONTEFAULT: case FAULTSTUDY:
					Factor = 1.0;
					break;
					case 	MONTECARLO2: case MONTECARLO3: case LOADDURATION1: case LOADDURATION2:
					{
						Factor = 1.0;
						CalcDailyMult(with2->DynaVars.dblHour);
					}
					break;
					case 	PEAKDAY:
					{
						Factor = 1.0;
						CalcDailyMult(with2->DynaVars.dblHour);
					}
					break;
					case 	AUTOADDFLAG:
					Factor = 1.0;
					break;
					default:
					Factor = 1.0;
					break;
				}
			}
			if(!(with1->IsDynamicModel || with1->IsHarmonicModel))         //******
			{
				if(ShapeIsActual)
					MachineData.Pnominalperphase = 1000.0 * ShapeFactor.re / Fnphases;
				else
					MachineData.Pnominalperphase = 1000.0 * kWBase * Factor * ShapeFactor.re / Fnphases;

                // cannot dispatch vars in induction machine
                // you get what you get
			}
		} /*ELSE GenON*/
	}  /*With ActiveCircuit*/

   // If machine state changes, force re-calc of Y matrix
	if(MachineON != MachineOn_Saved)
		Set_YprimInvalid(ActorID,true);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

void TIndMach012Obj::CalcDailyMult(double hr)
{
	if(DailyDispShapeObj != nullptr)
	{
		ShapeFactor = DailyDispShapeObj->GetMult(hr);
		ShapeIsActual = DailyDispShapeObj->UseActual;
	}
	else
	ShapeFactor = CDoubleOne;  // Default to no daily variation
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::CalcDutyMult(double hr)
{
	if(DutyShapeObj != nullptr)
	{
		ShapeFactor = DutyShapeObj->GetMult(hr);
		ShapeIsActual = DutyShapeObj->UseActual;
	}
	else
	CalcDailyMult(hr);  // Default to Daily Mult if no duty curve specified
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::CalcYearlyMult(double hr)
{

/*Yearly curve is assumed to be hourly only*/
	if(YearlyShapeObj != nullptr)
	{
		ShapeFactor = YearlyShapeObj->GetMult(hr);
		ShapeIsActual = YearlyShapeObj->UseActual;
	}
	else
	ShapeFactor = CDoubleOne;  // Defaults to no variation
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

// Gets the currents for the last solution performed

// Do not call anything that may change the basic element values from the last solution

void TIndMach012Obj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	CalcInjCurrentArray(ActorID);  // Difference between currents in YPrim and total current
	try
    // an exception here generally means an array boundary overrun
   // Copy into buffer array
	{
		int stop = 0;
		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			(Curr)[i - 1] = (InjCurrent)[i - 1];
		}
	}
	catch (std::exception &e)
	{
		DoErrorMsg(String("IndMach012 Object: \"") + get_Name() + "\" in GetInjCurrents function.", (std::string) e.what(), "Current buffer not big enough.", 568);
	}
}
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------
/*
 This procedure is require to respond to various commands such as Dump that
 write all the device's property values to a file.
*/

void TIndMach012Obj::DumpProperties(TTextRec& f, bool Complete)
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
				{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, "=("); Write(f, Get_PropertyValue(Idx)); WriteLn(f, L')'); }
				break;
				default:
				{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(Idx)); }
				break;
			}
		}
	}
	WriteLn(f);
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/*Procedure to initialize for Harmonics solution*/

/*This example is extracted from Generator and constructs a Thevinen equivalent.
 Refer to Load for how to do a Norton equivalent
 */

void TIndMach012Obj::InitHarmonics(int ActorID)
{
	complex e = {};
	complex Va = {};
	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims

/****
     GenFundamental := ActiveCircuit.Solution.Frequency ;  // Whatever the frequency is when we enter here.

     With GenVars Do Begin

         // Xd" is used for harmonics analysis for generators
         Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

         {Compute reference Thevinen voltage from phase 1 current}

         IF GenON Then
           Begin

             ComputeIterminal;  // Get present value of current

             With ActiveCircuit.solution Do
             Case Connection of
               0: Begin {wye - neutral is explicit}
                    Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]]);
                  End;
               1: Begin  {delta -- assume neutral is at zero}
                    Va := NodeV^[NodeRef^[1]];
                  End;
             End;

             E         := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
             Vthevharm := Cabs(E);   // establish base mag and angle
             ThetaHarm := Cang(E);
           End
         ELSE  Begin
           // If Generator is off, just set to zero
             Vthevharm := 0.0;
             ThetaHarm := 0.0;
         End;
     End;
 ***/
}

// ******************* PROPERTY VALUES   *******************

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// required procedure to initialize the string value of the properties

void TIndMach012Obj::InitPropertyValues(int ArrayOffset)
{

   // Some examples
	Set_PropertyValue(1,"3");        //'phases';
	Set_PropertyValue(2,GetBus(1));  //'bus1';
	Set_PropertyValue(3,"12.47");
	Set_PropertyValue(4,"100");
	Set_PropertyValue(5,".80");
	Set_PropertyValue(6,"Delta");
	Set_PropertyValue(7,Format("%-g", MachineData.kVArating));
	Set_PropertyValue(8,Format("%-g", MachineData.Hmass));
	Set_PropertyValue(9,Format("%-g", MachineData.D));
	Set_PropertyValue(10,"0.0053");
	Set_PropertyValue(11,"0.106");
	Set_PropertyValue(12,"0.007");
	Set_PropertyValue(13,"0.12");
	Set_PropertyValue(14,"4.0");
	Set_PropertyValue(15,"0.007");
	Set_PropertyValue(16,"0.1");
	Set_PropertyValue(17,"variable");
	Set_PropertyValue(18,"");
	Set_PropertyValue(19,"");
	Set_PropertyValue(20,"");     /*...*/
	Set_PropertyValue(21,"NO");

/*Call inherited function to init inherited property values*/
	inherited::InitPropertyValues(NumPropsThisClass);
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// Return i-th property value as a string

String TIndMach012Obj::GetPropertyValue(int Index)
{
	String result;
	result = "";   // Init the string
	switch(Index)
	{
		case 	4:
         // Put special cases here
         // often a good idea to convert numeric values to strings, for example
		result = Format("%.6g", kWBase);
		break;
		case 	5:
		result = Format("%.6g", PowerFactor(Get_Power(1, ActiveActor)));
		break;
		case 	7:
		result = Format("%.6g", MachineData.kVArating);
		break;
		case 	8:
		result = Format("%.6g", MachineData.Hmass);
		break;
		case 	9:
		result = Format("%.6g", MachineData.D);
		break;
		case 	15:
		result = Format("%.6g", get_S1());
		break;
		case 	18:
		result = YearlyShape;
		break;
		case 	19:
		result = DailyDispShape;
		break;
		case 	20:
		result = DutyShape;
		break;
         /*...*/

         // The default is to just return the current string value of the property
		default:
		result = TDSSCktElement::GetPropertyValue(Index);
		break;
	}
	return result;
}

// ******************* END PROPERTY VALUES   *******************



//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/*
  This is a virtual function. You do not need to write this routine
  if you are not integrating state variables in dynamics mode.
*/

// Integrate state variables for Dynamics analysis
// Example from Generator

// Illustrates use of debug tracing

// Present technique is a predictor-corrector trapezoidal rule

void TIndMach012Obj::IntegrateStates(int ActorID)
{
	complex TracePower = {};
   // Compute Derivatives and then integrate
	TDSSCktElement::ComputeIterminal(ActorID);
	/*# with ActiveCircuit[ActorID].Solution, MachineData do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		auto& with1 = MachineData;
		/*# with DynaVars do */
		{
			auto& with2 = with0->DynaVars;
			if(with2.IterationFlag == 0) /*First iteration of new time step*/
			{
				with1.ThetaHistory = with1.Theta + 0.5 * with2.h * with1.dTheta;
				with1.SpeedHistory = with1.Speed + 0.5 * with2.h * with1.dSpeed;
			}
		}

      // Compute shaft dynamics
		TracePower = TerminalPowerIn(&(Vterminal[0]), &(Iterminal[0]), Fnphases); // in watts
		with1.dSpeed = (TracePower.re - with1.Pshaft - Abs((int) (with1.D * with1.Speed))) / with1.Mmass;
		with1.dTheta = with1.Speed;

     // Trapezoidal method
		/*# with DynaVars do */
		{
			auto& with3 = with0->DynaVars;
			with1.Speed = with1.SpeedHistory + 0.5 * with3.h * with1.dSpeed;
			with1.Theta = with1.ThetaHistory + 0.5 * with3.h * with1.dTheta;
		}
		if(DebugTrace)
			WriteTraceRecord(ActorID);
		Integrate(ActorID);
	}
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::Get_DynamicModelCurrent()
{
	Is1 = cdiv(csub(V1, E1), Zsp); // I = (V-E')/Z'
	Is2 = cdiv(csub(V2, E2), Zsp); // I = (V-E')/Z'

    // rotor current  Ir1= Is1-Vm/jXm
	Ir1 = csub(Is1, cdiv(csub(V1, cmul(Is1, Zsp)), ZM));
	Ir2 = csub(Is2, cdiv(csub(V2, cmul(Is2, Zsp)), ZM));
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::Get_PFlowModelCurrent(const complex& V, double s, complex& Istator, complex& Irotor)
{
	double RL = 0.0;
	complex ZRotor = {};
	complex numerator = {};
	complex Zmotor = {};
	if(s != 0.0)
		RL = Zr.re * (1.0 - s) / s;
	else
		RL = Zr.re * 1.0e6;
	ZRotor = cadd(cmplx(RL, 0.0), Zr);
	numerator = cmul(ZM, ZRotor);
	Zmotor = cadd(Zs, cdiv(numerator, cadd(ZRotor, ZM)));
	Istator = cdiv(V, Zmotor);
    /*Ir = Is -(V-ZsIs)/Zm*/
	Irotor = csub(Istator, cdiv(csub(V, cmul(Zs, Istator)), ZM));
}

//----------------------------------------------------------------------------
// ********************** VARIABLES ***************************************
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
/*
  Return the number of state variables

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
  Note: it is not necessary to define any state variables
*/

int TIndMach012Obj::NumVariables()
{
	int result = 0;
	result = NumIndMach012Variables;
	return result;
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/*
  Returns the i-th state variable in a string

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
*/

String TIndMach012Obj::VariableName(int i)
{
	String result;
	if(i < 1)
		return result;  // This means Someone goofed
	switch(i)
	{
		case 	1:
		result = "Frequency";
		break;
		case 	2:
		result = "Theta (deg)";
		break;
		case 	3:
		result = "E1";
		break;
		case 	4:
		result = "Pshaft";
		break;
		case 	5:
		result = "dSpeed (deg/sec)";
		break;
		case 	6:
		result = "dTheta (deg)";
		break;
		case 	7:
		result = "Slip";
		break;
		case 	8:
		result = "puRs";
		break;
		case 	9:
		result = "puXs";
		break;
		case 	10:
		result = "puRr";
		break;
		case 	11:
		result = "puXr";
		break;
		case 	12:
		result = "puXm";
		break;
		case 	13:
		result = "Maxslip";
		break;
		case 	14:
		result = "Is1";
		break;
		case 	15:
		result = "Is2";
		break;
		case 	16:
		result = "Ir1";
		break;
		case 	17:
		result = "Ir2";
		break;
		case 	18:
		result = "Stator Losses";
		break;
		case 	19:
		result = "Rotor Losses";
		break;
		case 	20:
		result = "Shaft Power (hp)";
		break;
		case 	21:
		result = "Power Factor";
		break;
		case 	22:
		result = "Efficiency (%)";
		break;
		default:
		  ;
		break;
	}
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

double TIndMach012Obj::Get_Variable(int i)
{
	double result = 0.0;
	result = -9999.99;   // Error Value
	/*# with MachineData do */
	{
		auto& with0 = MachineData;
		switch(i)
		{
			case 	1:
			result = (with0.w0 + with0.Speed) / TwoPi;
			break;  // Frequency, Hz
			case 	2:
			result = (with0.Theta) * RadiansToDegrees;
			break;  // Report in Deg
			case 	3:
			result = cabs(E1) / VBase;
			break;      // Report in pu
			case 	4:
			result = with0.Pshaft;
			break;
			case 	5:
			result = with0.dSpeed * RadiansToDegrees;
			break; // Report in Deg      57.29577951
			case 	6:
			result = with0.dTheta;
			break;
			case 	7:
			result = get_S1();
			break;
			case 	8:
			result = puRs;
			break;
			case 	9:
			result = puXs;
			break;
			case 	10:
			result = puRr;
			break;
			case 	11:
			result = puXr;
			break;
			case 	12:
			result = puXm;
			break;
			case 	13:
			result = MaxSlip;
			break;
			case 	14:
			result = cabs(Is1);
			break;
			case 	15:
			result = cabs(Is2);
			break;
			case 	16:
			result = cabs(Ir1);
			break;
			case 	17:
			result = cabs(Ir2);
			break;
			case 	18:
			result = GetStatorLosses();
			break;
			case 	19:
			result = GetRotorLosses();
			break;  // Shaft Power  (hp)
			case 	20:
			{
				result = 3.0 / 746.0 * (Sqr(cabs(Ir1)) * (1.0 - S1) / S1 + Sqr(cabs(Ir2)) * (1.0 - S2) / S2) * Zr.re;
			}
			break;
			case 	21:
			result = PowerFactor(Get_Power(1, ActiveActor));
			break;
			case 	22:
			result = (1.0 - (GetStatorLosses() + GetRotorLosses()) / Get_Power(1, ActiveActor).re) * 100.0;
			break;    // Efficiency
			default:
			  ;
			break;
		}
	}
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::Set_Variable(int i, double Value)
{
	switch(i)
	{
		case 	7:
		Set_Slip(Value);
		break;
		case 	8:
		puRs = Value;
		break;
		case 	9:
		puXs = Value;
		break;
		case 	10:
		puRr = Value;
		break;
		case 	11:
		puXr = Value;
		break;
		case 	12:
		puXm = Value;
		break;
        /*Do Nothing for other variables: they are read only*/
		default:
		  ;
		break;
	}
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
/*
  Return all state variables in double array (allocated by calling function)

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
*/

void TIndMach012Obj::GetAllVariables(pDoubleArray States)
{
	int i = 0;
	int n = 0;
	int stop = 0;
	n = 0;
	for(stop = NumIndMach012Variables, i = 1; i <= stop; i++)
	{
		(States)[i - 1] = Get_Variable(i);
	}
}

// ********************** END VARIABLES ***************************************




//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

double TIndMach012Obj::GetRotorLosses()
{
	double result = 0.0;
	result = 3.0 * (Sqr(Ir1.re) + Sqr(Ir1.im) + Sqr(Ir2.re) + Sqr(Ir2.im)) * Zr.re;
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

double TIndMach012Obj::GetStatorLosses()
{
	double result = 0.0;
	result = 3.0 * (Sqr(Is1.re) + Sqr(Is1.im) + Sqr(Is2.re) + Sqr(Is2.im)) * Zs.re;
	return result;
}



//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/*
  This is a virtual function. You do not need to write this routine
  if the base class function will suffice.
*/

// Routine to convert existing three-phase models to a single-phase positive-
// sequence model

void TIndMach012Obj::MakePosSequence(int ActorID)
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
	s = "Phases=1 conn=wye";    // Positive sequence model is 1-phase wye

  /****

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0) Then   V :=  GenVars.kVGeneratorBase/SQRT3
  Else V :=  GenVars.kVGeneratorBase;

  S := S + Format(' kV=%-.5g',[V]);

  // Divide the load by no. phases
  If Fnphases>1 Then
  Begin
      S := S + Format(' kW=%-.5g  PF=%-.5g',[kWbase/Fnphases, PFNominal]);
      If (PrpSequence^[19]<>0) or (PrpSequence^[20]<>0) Then S := S + Format(' maxkvar=%-.5g  minkvar=%-.5g',[kvarmax/Fnphases, kvarmin/Fnphases]);
      If PrpSequence^[26]>0 Then S := S + Format(' kva=%-.5g  ',[genvars.kvarating/Fnphases]);
      If PrpSequence^[27]>0 Then S := S + Format(' MVA=%-.5g  ',[genvars.kvarating/1000.0/Fnphases]);
  End;

  Parser.CmdString := S;   // Push the string into the Parser object
  Edit;    // Invoke the Edit method for this class

  inherited;  // sets the terminal bus references, must do after editing number of phases

  ***/
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// Routine for handling Open/Close procedures

void TIndMach012Obj::Set_ConductorClosed(int Index, int ActorID, bool Value)
{
	TDSSCktElement::Set_ConductorClosed(Index, ActorID, Value);
	if(Value)
		IndMach012SwitchOpen = false;
	else
		IndMach012SwitchOpen = true;
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::set_Localslip(double Value)
{

	auto Sign = [&](double X) -> double 
	{
		double result = 0.0;
		if(X < 0.0)
			result = -1.0;
		else
			result = 1.0;
		return result;
	};
	S1 = Value;
	if(!InDynamics)
	{
		if(Abs((int) S1) > MaxSlip)
			S1 = Sign(S1) * MaxSlip;   // Put limits on the slip  unless dynamics
	}
	S2 = 2.0 - S1;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::Set_Slip(double Value)
{
	set_Localslip(Value);
	MachineData.Speed = MachineData.w0 * (-S1); // make motor speed agree
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::InitTraceFile()
{
	AssignFile(Tracefile, get_Name() + "_IndMach012_Trace.CSV");
	Rewrite(Tracefile);
	IOResultToException();
	Write(Tracefile, "Time, Iteration, S1, |IS1|, |IS2|, |E1|, |dE1dt|, |E2|, |dE2dt|, |V1|, |V2|, Pshaft, Pin, Speed, dSpeed");
	WriteLn(Tracefile);
	CloseFile(Tracefile);
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TIndMach012Obj::WriteTraceRecord(int ActorID)
{
	Append(Tracefile);
	IOResultToException();
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		Write(Tracefile, Format("%-.6g, %d, %-.6g, ", with0->DynaVars.dblHour * 3600.0, with0->Iteration, S1));
	}
	Write(Tracefile, Format("%-.6g, %-.6g, ", cabs(Is1), cabs(Is2)));
	Write(Tracefile, Format("%-.6g, %-.6g, %-.6g, %-.6g, ", cabs(E1), cabs(dE1dt), cabs(E2), cabs(dE2dt)));
	Write(Tracefile, Format("%-.6g, %-.6g, ", cabs(V1), cabs(V2)));
	Write(Tracefile, Format("%-.6g, %-.6g, ", MachineData.Pshaft, Get_Power(1, ActorID).re));
	Write(Tracefile, Format("%-.6g, %-.6g, ", MachineData.Speed, MachineData.dSpeed));
	WriteLn(Tracefile);
	CloseFile(Tracefile);
}

// Initialize any variables here


  // For Example:  1 + j 1


void IndMach012_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
}

		class 		IndMach012_unit
		{
		public:
		IndMach012_unit()
		{
			//AssertSystemInitialization();
			IndMach012_initialization();
		}
		};
		IndMach012_unit _IndMach012_unit;

}  // namespace IndMach012




