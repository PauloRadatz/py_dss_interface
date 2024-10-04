

#pragma hdrstop

#include "VSource.h"

#include "Circuit.h"
#include "DSSGlobals.h"
#include "Utilities.h"

using namespace std;
using namespace Circuit;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Dynamics;
using namespace LoadShape;
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace Utilities;

namespace VSource
{

TVsourceObj::TVsourceObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TVsourceObj::TVsourceObj(String ClassName) : inherited(ClassName) {}
TVsourceObj::TVsourceObj() {}


TVsourceObj* ActiveVsourceObj = nullptr;
const int NumPropsThisClass = 31;
complex CDoubleOne = {};

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TVsource::TVsource()
{
	;
	Class_Name = "Vsource";
	DSSClassType = SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	VSourceClass[ActiveActor] = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TVsource::~TVsource()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TVsource::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "bus1";
	PropertyName[2 - 1] = "basekv";
	PropertyName[3 - 1] = "pu";
	PropertyName[4 - 1] = "angle";
	PropertyName[5 - 1] = "frequency";
	PropertyName[6 - 1] = "phases";
	PropertyName[7 - 1] = "MVAsc3";
	PropertyName[8 - 1] = "MVAsc1";
	PropertyName[9 - 1] = "x1r1";
	PropertyName[10 - 1] = "x0r0";
	PropertyName[11 - 1] = "Isc3";
	PropertyName[12 - 1] = "Isc1";
	PropertyName[13 - 1] = "R1";
	PropertyName[14 - 1] = "X1";
	PropertyName[15 - 1] = "R0";
	PropertyName[16 - 1] = "X0";
	PropertyName[17 - 1] = "ScanType";
	PropertyName[18 - 1] = "Sequence";
	PropertyName[19 - 1] = "bus2";
	PropertyName[20 - 1] = "Z1";
	PropertyName[21 - 1] = "Z0";
	PropertyName[22 - 1] = "Z2";
	PropertyName[23 - 1] = "puZ1";
	PropertyName[24 - 1] = "puZ0";
	PropertyName[25 - 1] = "puZ2";
	PropertyName[26 - 1] = "baseMVA";
	PropertyName[27 - 1] = "Yearly";
	PropertyName[28 - 1] = "Daily";
	PropertyName[29 - 1] = "Duty";
	PropertyName[30 - 1] = "Model";
	PropertyName[31 - 1] = "puZideal";


     // define Property help values
	PropertyHelp[1 - 1] = String("Name of bus to which the main terminal (1) is connected.") + CRLF
	           + "bus1=busname"
	           + CRLF
	           + "bus1=busname.1.2.3"
	           + CRLF
	           + CRLF
	           + "The VSOURCE object is a two-terminal voltage source (thevenin equivalent). "
	           + "Bus2 defaults to Bus1 with all phases connected to ground (node 0) unless previously specified. This is a Yg connection. "
	           + "If you want something different, define the Bus2 property ezplicitly.";
	PropertyHelp[2 - 1] = "Base Source kV, usually phase-phase (L-L) unless you are making a positive-sequence model or 1-phase model "
	           "in which case, it will be phase-neutral (L-N) kV.";
	PropertyHelp[3 - 1] = String("Per unit of the base voltage that the source is actually operating at.") + CRLF
	           + "\"pu=1.05\"";
	PropertyHelp[4 - 1] = "Phase angle in degrees of first phase: e.g.,Angle=10.3";
	PropertyHelp[5 - 1] = "Source frequency.  Defaults to system default base frequency.";
	PropertyHelp[6 - 1] = "Number of phases.  Defaults to 3.";
	PropertyHelp[7 - 1] = "MVA Short circuit, 3-phase fault. Default = 2000. "
	           "Z1 is determined by squaring the base kv and dividing by this value. "
	           "For single-phase source, this value is not used.";
	PropertyHelp[8 - 1] = "MVA Short Circuit, 1-phase fault. Default = 2100. "
	           "The \"single-phase impedance\", Zs, is determined by squaring the base kV and dividing by this value. "
	           "Then Z0 is determined by Z0 = 3Zs - 2Z1.  For 1-phase sources, Zs is used directly. "
	           "Use X0R0 to define X/R ratio for 1-phase source.";
	PropertyHelp[9 - 1] = "Positive-sequence  X/R ratio. Default = 4.";
	PropertyHelp[10 - 1] = "Zero-sequence X/R ratio.Default = 3.";
	PropertyHelp[11 - 1] = String("Alternate method of defining the source impedance. ") + CRLF
	           + "3-phase short circuit current, amps.  Default is 10000.";
	PropertyHelp[12 - 1] = String("Alternate method of defining the source impedance. ") + CRLF
	           + "single-phase short circuit current, amps.  Default is 10500.";
	PropertyHelp[13 - 1] = String("Alternate method of defining the source impedance. ") + CRLF
	           + "Positive-sequence resistance, ohms.  Default is 1.65.";
	PropertyHelp[14 - 1] = String("Alternate method of defining the source impedance. ") + CRLF
	           + "Positive-sequence reactance, ohms.  Default is 6.6.";
	PropertyHelp[15 - 1] = String("Alternate method of defining the source impedance. ") + CRLF
	           + "Zero-sequence resistance, ohms.  Default is 1.9.";
	PropertyHelp[16 - 1] = String("Alternate method of defining the source impedance. ") + CRLF
	           + "Zero-sequence reactance, ohms.  Default is 5.7.";
	PropertyHelp[17 - 1] = "{pos*| zero | none} Maintain specified sequence for harmonic solution. Default is positive sequence. "
	           "Otherwise, angle between phases rotates with harmonic.";
	PropertyHelp[18 - 1] = "{pos*| neg | zero} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. "
	           "Default is positive sequence. ";
	PropertyHelp[19 - 1] = String("Name of bus to which 2nd terminal is connected.") + CRLF
	           + "bus2=busname"
	           + CRLF
	           + "bus2=busname.1.2.3"
	           + CRLF
	           + CRLF
	           + "Default is Bus1.0.0.0 (grounded wye connection)";
	PropertyHelp[20 - 1] = String("Positive-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: ") + CRLF
	           + CRLF
	           + "Z1=[1, 2 - 1]  ! represents 1 + j2 "
	           + CRLF
	           + CRLF
	           + "If defined, Z1, Z2, and Z0 are used to define the impedance matrix of the VSOURCE. "
	           + "Z1 MUST BE DEFINED TO USE THIS OPTION FOR DEFINING THE MATRIX."
	           + CRLF
	           + CRLF
	           + "Side Effect: Sets Z2 and Z0 to same values unless they were previously defined.";
	PropertyHelp[21 - 1] = String("Zero-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: ") + CRLF
	           + CRLF
	           + "Z0=[3, 4 - 1]  ! represents 3 + j4 "
	           + CRLF
	           + CRLF
	           + "Used to define the impedance matrix of the VSOURCE if Z1 is also specified. "
	           + CRLF
	           + CRLF
	           + "Note: Z0 defaults to Z1 if it is not specifically defined. ";
	PropertyHelp[22 - 1] = String("Negative-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: ") + CRLF
	           + CRLF
	           + "Z2=[1, 2 - 1]  ! represents 1 + j2 "
	           + CRLF
	           + CRLF
	           + "Used to define the impedance matrix of the VSOURCE if Z1 is also specified. "
	           + CRLF
	           + CRLF
	           + "Note: Z2 defaults to Z1 if it is not specifically defined. If Z2 is not equal to Z1, the impedance matrix is asymmetrical.";
	PropertyHelp[23 - 1] = "2-element array: e.g., [1  2 - 1]. An alternate way to specify Z1. See Z1 property. Per-unit positive-sequence impedance on base of Vsource BasekV and BaseMVA.";
	PropertyHelp[24 - 1] = "2-element array: e.g., [1  2 - 1]. An alternate way to specify Z0. See Z0 property. Per-unit zero-sequence impedance on base of Vsource BasekV and BaseMVA.";
	PropertyHelp[25 - 1] = "2-element array: e.g., [1  2 - 1]. An alternate way to specify Z2. See Z2 property. Per-unit negative-sequence impedance on base of Vsource BasekV and BaseMVA.";
	PropertyHelp[26 - 1] = "Default value is 100. Base used to convert values specifiied with puZ1, puZ0, and puZ2 properties to ohms on kV base specified by BasekV property.";
	PropertyHelp[27 - 1] = String("LOADSHAPE object to use for the per-unit voltage for YEARLY-mode simulations. Set the Mult property of the LOADSHAPE " "to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.") + CRLF
	           + CRLF
	           + "Must be previously defined as a LOADSHAPE object. "
	           + CRLF
	           + CRLF
	           + "Is set to the Daily load shape when Daily is defined.  The daily load shape is repeated in this case. "
	           + "Set to NONE to reset to no loadahape for Yearly mode. "
	           + "The default is no variation.";
	PropertyHelp[28 - 1] = String("LOADSHAPE object to use for the per-unit voltage for DAILY-mode simulations. Set the Mult property of the LOADSHAPE " "to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.") + CRLF
	           + CRLF
	           + "Must be previously defined as a LOADSHAPE object. "
	           + CRLF
	           + CRLF
	           + "Sets Yearly curve if it is not already defined.   "
	           + "Set to NONE to reset to no loadahape for Yearly mode. "
	           + "The default is no variation.";
	PropertyHelp[29 - 1] = String("LOADSHAPE object to use for the per-unit voltage for DUTYCYCLE-mode simulations. Set the Mult property of the LOADSHAPE " "to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.") + CRLF
	           + CRLF
	           + "Must be previously defined as a LOADSHAPE object. "
	           + CRLF
	           + CRLF
	           + "Defaults to Daily load shape when Daily is defined.   "
	           + "Set to NONE to reset to no loadahape for Yearly mode. "
	           + "The default is no variation.";
	PropertyHelp[30 - 1] = "{Thevenin* | Ideal}  Specifies whether the Vsource is to be considered a Thevenin short circuit model or a quasi-ideal voltage source. If Thevenin, the Vsource uses the impedances defined for all calculations. "
	           "If \"Ideal\", the model uses a small impedance on the diagonal of the impedance matrix for the fundamental base frequency power flow only. Then switches to actual Thevenin model for other frequencies. ";
	PropertyHelp[31 - 1] = "2-element array: e.g., [1  2 - 1]. The pu impedance to use for the quasi-ideal voltage source model. Should be a very small impedances. Default is [1e-6, 0.001 - 1]. Per-unit impedance on base of Vsource BasekV and BaseMVA. "
	           "If too small, solution may not work. Be sure to check the voltage values and powers.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // Override help string
	PropertyHelp[NumPropsThisClass + 1] = "Name of harmonic spectrum for this source.  Default is \"defaultvsource\", which is defined when the DSS starts.";
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TVsource::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new voltage source and add it to Vsource class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TVsourceObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TVsource::VsourceSetBus1(const String s)
{
	String S2;
	int i = 0;
	int dotpos = 0;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0
	/*# with ActiveVsourceObj do */
	{
		auto with0 = ActiveVsourceObj;
		( with0 )->SetBus(1, s);
		if(!with0->Bus2Defined) // Default Bus2 to zero node of Bus1. (Grounded-Y connection)

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
			}   // append series of ".0"'s
			with0->SetBus(2, S2);    // default setting for Bus2
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TVsource::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	complex Ztemp = {};
  // continue parsing with contents of Parser
	ActiveVsourceObj = (TVsourceObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveVsourceObj);
	result = 0;
	/*# with ActiveVsourceObj do */
	{
		auto with0 = ActiveVsourceObj;
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
	           + "\" for Object \"VSource."
	           + with0->get_Name()
	           + "\"", 320);
				break;
				case 	1:
				VsourceSetBus1(Param);
				break;   // special handling of Bus 1
				case 	2:
				with0->kVBase = Parser[ActorID]->MakeDouble_();
				break; // basekv
				case 	3:
				with0->PerUnit = Parser[ActorID]->MakeDouble_();
				break; // pu
				case 	4:
				with0->Angle = Parser[ActorID]->MakeDouble_();
				break; // Ang
				case 	5:
				with0->SrcFrequency = Parser[ActorID]->MakeDouble_();
				break; // freq
				case 	6:
				{
					with0->Set_NPhases(Parser[ActorID]->MakeInteger_()); // num phases
					with0->Set_Nconds(with0->Fnphases);  // Force Reallocation of terminal info
				}
				break;
				case 	7:
				with0->MVAsc3 = Parser[ActorID]->MakeDouble_();
				break; // MVAsc3
				case 	8:
				with0->MVAsc1 = Parser[ActorID]->MakeDouble_();
				break; // MVAsc1
				case 	9:
				with0->X1R1 = Parser[ActorID]->MakeDouble_();
				break; // X1/R1
				case 	10:
				with0->X0R0 = Parser[ActorID]->MakeDouble_();
				break; // X0/R0
				case 	11:
				with0->Isc3 = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->Isc1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->R1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->X1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				with0->R0 = Parser[ActorID]->MakeDouble_();
				break;
				case 	16:
				with0->X0 = Parser[ActorID]->MakeDouble_();
				break;
				case 	17:
				switch(UpperCase(Param)[1])
				{
					case 	L'P':
					with0->ScanType = 1;
					break;
					case 	L'Z':
					with0->ScanType = 0;
					break;
					case 	L'N':
					with0->ScanType = -1;
					break;
					default:
					DoSimpleMsg(String("Unknown Scan Type for \"") + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\": "
	           + Param, 321);
					break;
				}
				break;
				case 	18:
				switch(UpperCase(Param)[1])
				{
					case 	L'P':
					with0->SequenceType = 1;
					break;
					case 	L'Z':
					with0->SequenceType = 0;
					break;
					case 	L'N':
					with0->SequenceType = -1;
					break;
					default:
					DoSimpleMsg(String("Unknown Sequence Type for \"") + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\": "
	           + Param, 321);
					break;
				}
				break;
				case 	19:
				with0->SetBus(2, Param);
				break;
				case 	20:
				Ztemp = InterpretComplex(Param);
				break;
				case 	21:
				Ztemp = InterpretComplex(Param);
				break;
				case 	22:
				Ztemp = InterpretComplex(Param);
				break;
				case 	23:
				with0->puZ1 = InterpretComplex(Param);
				break;
				case 	24:
				with0->puZ0 = InterpretComplex(Param);
				break;
				case 	25:
				with0->puZ2 = InterpretComplex(Param);
				break;
				case 	26:
				with0->BaseMVA = Parser[ActorID]->MakeDouble_();
				break;
				case 	27:
				with0->YearlyShape = Param;
				break;
				case 	28:
				with0->DailyShape = Param;
				break;
				case 	29:
				with0->DutyShape = Param;
				break;
				case 	30:
				with0->IsQuasiIdeal = with0->InterpretSourceModel(Param);
				break;
				case 	31:
				with0->puZideal = InterpretComplex(Param);
				break;
				default:
				inherited::ClassEdit(ActiveVsourceObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	20:
				{
					with0->R1 = Ztemp.re;
					with0->X1 = Ztemp.im;
					with0->Z1Specified = true;
                     // default values for Z2, Z0
					if(!with0->Z2Specified)
					{
						with0->R2 = with0->R1;
						with0->X2 = with0->X1;
					}
					if(!with0->Z0Specified)
					{
						with0->R0 = with0->R1;
						with0->X0 = with0->X1;
					}
				}
				break;
				case 	21:
				{
					with0->R0 = Ztemp.re;
					with0->X0 = Ztemp.im;
					with0->Z0Specified = true;
				}
				break;
				case 	22:
				{
					with0->R2 = Ztemp.re;
					with0->X2 = Ztemp.im;
					with0->Z2Specified = true;
				}
				break;
				case 	23:
				{
					with0->puZ1Specified = true;
                     // default values for Z2, Z0
					if(!with0->puZ2Specified)
					{
						with0->puZ2 = with0->puZ1;
					}
					if(!with0->puZ0Specified)
					{
						with0->puZ0 = with0->puZ1;
					}
				}
				break;
				case 	24:
				with0->puZ0Specified = true;
				break;
				case 	25:
				with0->puZ2Specified = true;
				break;
    /*Set shape objects;  returns nil if not valid*/
    /*Sets the kW and kvar properties to match the peak kW demand from the Loadshape*/
				case 	27:
				with0->YearlyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->YearlyShape));
				break;
				case 	28:
				{
					with0->DailyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DailyShape));
                  /*If Yearly load shape is not yet defined, make it the same as Daily*/
					if(with0->YearlyShapeObj == nullptr)
						with0->YearlyShapeObj = with0->DailyShapeObj;
				}
				break;
				case 	29:
				with0->DutyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DutyShape));
				break;
				default:
				  ;
				break;
			}
			switch(ParamPointer)
			{
				case 	13:
				with0->R2 = with0->R1;
				break;
				case 	14:
				with0->X2 = with0->X1;
				break;
				default:
				  ;
				break;
			}
         // Set the Z spec type switch depending on which was specified.
			switch(ParamPointer)
			{
				case 	7: case 8:
				with0->ZSpecType = 1;
				break;  // MVAsc
				case 	11: case 12:
				with0->ZSpecType = 2;
				break;  // Isc
				case 13: case 14: case 15: case 16:
				with0->ZSpecType = 3;
				break; // Specified in Ohms
				case 	19:
				with0->Bus2Defined = true;
				break;
				case 20: case 21: case 22: case 23: case 24: case 25:
				with0->ZSpecType = 3;
				break;
				default:
				  ;
				break;
			}
			switch(ParamPointer)
			{
				case 	2:
				with0->ZBase = Sqr(with0->kVBase) / with0->BaseMVA;
				break;
				case 	23:
				{
					with0->Z1Specified = true;
					with0->puZ1Specified = true;
				}
				break;
				case 	24:
				with0->puZ0Specified = true;
				break;
				case 	25:
				with0->puZ2Specified = true;
				break;
				case 	26:
				with0->ZBase = Sqr(with0->kVBase) / with0->BaseMVA;
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		with0->RecalcElementData(ActorID);
		with0->Set_YprimInvalid(ActorID,true);
	}
	return result;
}

//----------------------------------------------------------------------------

int TVsource::MakeLike(const String OtherSource)
{
	int result = 0;
	TVsourceObj* OtherVSource = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line name in the present collection*/
	OtherVSource = ((TVsourceObj*) Find(OtherSource));
	if(OtherVSource != nullptr)
		/*# with ActiveVsourceObj do */
		{
			auto with0 = ActiveVsourceObj;
			int stop = 0;
			if(with0->Fnphases != OtherVSource->Fnphases)
			{
				with0->Set_NPhases(OtherVSource->Fnphases);
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
				if(with0->Z != nullptr)
					delete with0->Z;
				if(with0->Zinv != nullptr)
					delete with0->Zinv;
				with0->Z = new TcMatrix(with0->Fnphases);
				with0->Zinv = new TcMatrix(with0->Fnphases);
			}
			with0->Z->CopyFrom(OtherVSource->Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
			with0->Vmag = OtherVSource->Vmag;
			with0->kVBase = OtherVSource->kVBase;
			with0->BaseMVA = OtherVSource->BaseMVA;
			with0->PerUnit = OtherVSource->PerUnit;
			with0->Angle = OtherVSource->Angle;
			with0->MVAsc3 = OtherVSource->MVAsc3;
			with0->MVAsc1 = OtherVSource->MVAsc1;
			with0->ScanType = OtherVSource->ScanType;
			with0->SequenceType = OtherVSource->SequenceType;
			with0->SrcFrequency = OtherVSource->SrcFrequency;
			with0->ZSpecType = OtherVSource->ZSpecType;
			with0->R1 = OtherVSource->R1;
			with0->X1 = OtherVSource->X1;
			with0->R2 = OtherVSource->R2;
			with0->X2 = OtherVSource->X2;
			with0->R0 = OtherVSource->R0;
			with0->X0 = OtherVSource->X0;
			with0->X1R1 = OtherVSource->X1R1;
			with0->X0R0 = OtherVSource->X0R0;
			with0->BaseMVA = OtherVSource->BaseMVA;
			with0->puZ1 = OtherVSource->puZ1;
			with0->puZ0 = OtherVSource->puZ0;
			with0->puZ2 = OtherVSource->puZ2;
			with0->ZBase = OtherVSource->ZBase;
			with0->Bus2Defined = OtherVSource->Bus2Defined;
			with0->Z1Specified = OtherVSource->Z1Specified;
			with0->Z2Specified = OtherVSource->Z2Specified;
			with0->Z0Specified = OtherVSource->Z0Specified;
			with0->puZ0Specified = OtherVSource->puZ0Specified;
			with0->puZ1Specified = OtherVSource->puZ1Specified;
			with0->puZ2Specified = OtherVSource->puZ2Specified;
			with0->IsQuasiIdeal = OtherVSource->IsQuasiIdeal;
			with0->puZideal = OtherVSource->puZideal;

        /*Loadshape stuff*/
			with0->ShapeIsActual = OtherVSource->ShapeIsActual;
			with0->DailyShape = OtherVSource->DailyShape;
			with0->DailyShapeObj = OtherVSource->DailyShapeObj;
			with0->DutyShape = OtherVSource->DutyShape;
			with0->DutyShapeObj = OtherVSource->DutyShapeObj;
			with0->YearlyShape = OtherVSource->YearlyShape;
			with0->YearlyShapeObj = OtherVSource->YearlyShapeObj;
			ClassMakeLike(OtherVSource);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				(with0->FPropertyValue)[i - 1] = (OtherVSource->FPropertyValue)[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Vsource MakeLike: \"") + OtherSource
	           + "\" Not Found.", 322);
	return result;
}

//----------------------------------------------------------------------------

int TVsource::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TVsource.Init", -1);
	result = 0;
	return result;
}

//=============================================================================

TVsourceObj::TVsourceObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			MVAsc3(2000.0),
			MVAsc1(2100.0),
			Isc3(10000.0),
			Isc1(10540.0),
			ZSpecType(0),
			R1(0.0),
			X1(0.0),
			R2(0.0),
			X2(0.0),
			R0(0.0),
			X0(0.0),
			X1R1(0.0),
			X0R0(0.0),
			BaseMVA(0.0),
			ZBase(0.0),
			Bus2Defined(false),
			Z1Specified(false),
			puZ1Specified(false),
			puZ0Specified(false),
			puZ2Specified(false),
			Z2Specified(false),
			Z0Specified(false),
			IsQuasiIdeal(false),
			ScanType(0),
			SequenceType(0),
			ShapeIsActual(false),
			Z(nullptr),
			Zinv(nullptr),
			Vmag(0.0),
			kVBase(0.0),
			PerUnit(0.0),
			Angle(0.0),
			SrcFrequency(0.0),
			DailyShapeObj(nullptr),
			DutyShapeObj(nullptr),
			YearlyShapeObj(nullptr)
{
	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	Set_NPhases(3);
	Fnconds = 3;
	Set_NTerms(2);   // Now a 2-terminal device
	Z = nullptr;
	Zinv = nullptr;
     /*Basefrequency := 60.0;*/ // set in base class
	ZSpecType = 1; // default to MVAsc
	R1 = 1.65;
	X1 = 6.6;
	R2 = R1;
	X2 = X1;
	R0 = 1.9;
	X0 = 5.7;
	X1R1 = 4.0;
	X0R0 = 3.0;
	PerUnit = 1.0;  // per unit voltage, not impedance
	kVBase = 115.0;
	BaseMVA = 100.0;
	ZBase = Sqr(kVBase) / BaseMVA;
	SrcFrequency = BaseFrequency;
	Angle = 0.0;
	ScanType = 1;
	SequenceType = 1;
	Bus2Defined = false;
	Z1Specified = false;
	Z2Specified = false;
	Z0Specified = false;
	puZ0Specified = false;
	puZ2Specified = false;
	puZ1Specified = false;
	IsQuasiIdeal = false;  // you have to turn this flag ON
	puZideal = cmplx(1.0e-6, 0.001);  // default ideal source pu impedance
	Spectrum = "defaultvsource";
	ShapeIsActual = false;
	YearlyShape = "";
	YearlyShapeObj = nullptr;  // IF YearlyShapeobj = nil THEN the Vsource alway stays nominal
	DailyShape = "";
	DailyShapeObj = nullptr;  // IF DaillyShapeobj = nil THEN the Vsource alway stays nominal
	DutyShape = "";
	DutyShapeObj = nullptr;  // IF DutyShapeobj = nil THEN the Vsource alway stays nominal
	InitPropertyValues(0);
	Yorder = Fnterms * Fnconds;
	RecalcElementData(ActiveActor);
}


//=============================================================================

TVsourceObj::~TVsourceObj()
{
	delete Z;
	delete Zinv;
	// inherited::Destroy();
}


//----------------------------------------------------------------------------


// interpret Model type

bool TVsourceObj::InterpretSourceModel(const String s)
{
	bool result = false;
	if(CompareTextShortest(s, "ideal") == 0)
		result = true;
	else
		result = false;
	return result;
}

//=============================================================================

void TVsourceObj::RecalcElementData(int ActorID)
{
	complex Zs = {};
	complex ZM = {};
	complex Z1 = {};
	complex Z2 = {};
	complex Z0 = {};
	complex Value = {};
	complex Value1 = {};
	complex Value2 = {};
	complex Calpha1 = {};
	complex Calpha2 = {};
	int i = 0;
	int j = 0;
	double Factor = 0.0;
	double Rs = 0.0;
	double Xs = 0.0;
	double Rm = 0.0;
	double XM = 0.0;
	if(Z != nullptr)
		delete Z;
	if(Zinv != nullptr)
		delete Zinv;

    // For a Source, nphases = ncond, for now
	Z = new TcMatrix(Fnphases);
	Zinv = new TcMatrix(Fnphases);
	if(Fnphases == 1)
		Factor = 1.0;
	else
		Factor = SQRT3;
	Rs = 0.0;
	Rm = 0.0;
	Xs = 0.1;
	XM = 0.0;

    /*Calculate the short circuit impedance and make all other spec types agree*/
	switch(ZSpecType)
	{
		case 	1:  // MVAsc
		{
			X1 = Sqr(kVBase) / MVAsc3 / sqrt(1.0L + 1.0L / Sqr(X1R1));
          //  Xs   := Sqr(KvBase) / MVAsc1/sqrt(1.0 + 1.0/Sqr(X0R0)); // Approx
			R1 = X1 / X1R1;
			R2 = R1;  // default Z2 = Z1
			X2 = X1;
			Isc3 = MVAsc3 * 1000.0 / (SQRT3 * kVBase);
			Isc1 = MVAsc1 * 1000.0 / (Factor * kVBase);

        //  Compute R0, X0
			R0 = QuadSolver((1.0L + Sqr(X0R0)), (4.0 * (R1 + X1 * X0R0)), (4.0 * (R1 * R1 + X1 * X1) - Sqr(3.0 * kVBase * 1000.0 / Factor / Isc1)));
			X0 = R0 * X0R0;

            // for Z matrix
			Xs = (2.0 * X1 + X0) / 3.0;
			Rs = (2.0 * R1 + R0) / 3.0;
			Rm = (R0 - R1) / 3.0;
			XM = (X0 - X1) / 3.0;
		}
		break;  // Isc
		case 	2:
		{
			MVAsc3 = SQRT3 * kVBase * Isc3 / 1000.0;
			MVAsc1 = Factor * kVBase * Isc1 / 1000.0;
			X1 = Sqr(kVBase) / MVAsc3 / sqrt(1.0L + 1.0L / Sqr(X1R1));
			R1 = X1 / X1R1;
			R2 = R1;  // default Z2 = Z1
			X2 = X1;
        //  Compute R0, X0
			R0 = QuadSolver((1.0L + Sqr(X0R0)), (4.0 * (R1 + X1 * X0R0)), (4.0 * (R1 * R1 + X1 * X1) - Sqr(3.0 * kVBase * 1000.0 / Factor / Isc1)));
			X0 = R0 * X0R0;

            // for Z matrix
			Xs = (2.0 * X1 + X0) / 3.0;
			Rs = (2.0 * R1 + R0) / 3.0;
			Rm = (R0 - R1) / 3.0;
			XM = (X0 - X1) / 3.0;
		}
		break;  // Z1, Z2, Z0    Specified

            // Compute Z1, Z2, Z0 in ohms if Z1 is specified in pu
		case 	3:
		{
			if(puZ1Specified)
			{
				R1 = puZ1.re * ZBase;
				X1 = puZ1.im * ZBase;
				R2 = puZ2.re * ZBase;
				X2 = puZ2.im * ZBase;
				R0 = puZ0.re * ZBase;
				X0 = puZ0.im * ZBase;
			}

            // Compute equivalent Isc3, Isc1, MVAsc3, MVAsc1 values;
			Isc3 = kVBase * 1000.0 / SQRT3 / cabs(cmplx(R1, X1));

            // compute nominal values for case where Z1=Z2
            // we won't necessarily use it to build Yprim matrix if Z2 <> Z1
			if(Fnphases == 1)  // Force Z0 and Z2 to be Z1 so Zs is same as Z1
			{
				R0 = R1;
				X0 = X1;
				R2 = R1;
				X2 = X1;
			}
			Rs = (2.0 * R1 + R0) / 3.0;
			Xs = (2.0 * X1 + X0) / 3.0;
			Isc1 = kVBase * 1000.0 / Factor / cabs(cmplx(Rs, Xs));
			MVAsc3 = SQRT3 * kVBase * Isc3 / 1000.0;
			MVAsc1 = Factor * kVBase * Isc1 / 1000.0;
			XM = Xs - X1;
			Rs = (2.0 * R1 + R0) / 3.0;
			Rm = (R0 - R1) / 3.0;
		}
		break;
		default:
		  ;
		break;
	}

    /*Update property Value array*/
     /* Don't change a specified value; only computed ones*/
	if((R1 == R2) && (X1 == X2))
    // Symmetric Matrix Case
	{
		int stop = 0;
		Zs = cmplx(Rs, Xs);
		ZM = cmplx(Rm, XM);
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			Z->SetElement(i, i, Zs);
			for(stop1 = i - 1, j = 1; j <= stop1; j++)
			{
				Z->SetElemsym(i, j, ZM);
			}
		}
	}
	else

    // Asymmetric Matrix case where Z2 <> Z1
	{
		int stop = 0;
		Z1 = cmplx(R1, X1);
		Z2 = cmplx(R2, X2);
		Z0 = cmplx(R0, X0);

         // Diagonals  (all the same)
		Value = cadd(Z2, cadd(Z1, Z0));   // Z1 + Z2 + Z0
		Value = cdivreal(Value, 3.0);
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			Z->SetElement(i, i, Value);
		}

         // Off-Diagonals
		if(Fnphases == 3)     // otherwise undefined


             // There are two possible off-diagonal elements  if Z1 <> Z2
             // Calpha is defined as 1 /_ -120 instead of 1 /_ 120
		{
			Calpha1 = conjg(CALPHA);           // Change Calpha to agree with textbooks
			Calpha2 = cmul(Calpha1, Calpha1);  // Alpha squared  = 1 /_ 240 = 1/_-120
             //(Z0 + aZ1 + a2 Z2)/3
			Value2 = cadd(cmul(Calpha2, Z2), cadd(cmul(Calpha1, Z1), Z0));
             //(Z0 + a2 Z1 + aZ2)/3
			Value1 = cadd(cmul(Calpha2, Z1), cadd(cmul(Calpha1, Z2), Z0));
             // Apply 1/3 ...
			Value1 = cdivreal(Value1, 3.0);
			Value2 = cdivreal(Value2, 3.0);
			/*# with Z do */
			{
				auto with0 = Z;
               //Lower Triangle
				with0->SetElement(2, 1, Value1);
				with0->SetElement(3, 1, Value2);
				with0->SetElement(3, 2, Value1);
               //Upper Triangle
				with0->SetElement(1, 2, Value2);
				with0->SetElement(1, 3, Value1);
				with0->SetElement(2, 3, Value2);
			}
		}
	}

  // if not specified, compute a value for for puZ1 for display in formedit
	if(!(puZ1Specified || puZ0Specified || puZ2Specified) && (ZBase > 0.0))
	{
		puZ1.re = R1 / ZBase;
		puZ1.im = X1 / ZBase;
		puZ2.re = R2 / ZBase;
		puZ2.im = X2 / ZBase;
		puZ0.re = R0 / ZBase;
		puZ0.im = X0 / ZBase;
	}
	switch(Fnphases)
	{
		case 	1:
		Vmag = kVBase * PerUnit * 1000.0;
		break;
		default:
		Vmag = kVBase * PerUnit * 1000.0 / 2.0 / sin((180.0 / Fnphases) * DSSGlobals::PI / 180.0L);
		break;
	}
	SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
	if(SpectrumObj == nullptr)
	{
		DoSimpleMsg(String("Spectrum Object \"") + Spectrum
	           + "\" for Device Vsource."
	           + get_Name()
	           + " Not Found.", 324);
	}

    /*Now check for errors.  If any of these came out nil and the string was not nil, give warning*/
	if(CompareText(YearlyShape, "none") == 0)
		YearlyShape = "";
	if(CompareText(DailyShape, "none") == 0)
		DailyShape = "";
	if(CompareText(DutyShape, "none") == 0)
		DutyShape = "";
	if(YearlyShapeObj == nullptr)
	{
		if(YearlyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Vsource Yearly load shape: \"") + YearlyShape
	           + "\" Not Found.", 34583);
	}
	if(DailyShapeObj == nullptr)
	{
		if(DailyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Vsource Daily load shape: \"") + DailyShape
	           + "\" Not Found.", 34584);
	}
	if(DutyShapeObj == nullptr)
	{
		if(DutyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Vsource Duty load shape: \"") + DutyShape
	           + "\" Not Found.", 34585);
	}
	InjCurrent = (pComplexArray) realloc(InjCurrent, sizeof(complex) * Yorder);
}

//=============================================================================

void TVsourceObj::CalcYPrim(int ActorID)
{
	complex Value = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;

 // Build only YPrim Series
	int stop = 0;
	if(Get_YprimInvalid(ActorID,0))
	{
		if(YPrim_Series != nullptr)
			delete YPrim_Series;
		YPrim_Series = new TcMatrix(Yorder);
		if(YPrim != nullptr)
			delete YPrim;
		YPrim = new TcMatrix(Yorder);
	}
	else
	{
		YPrim_Series->Clear();
		YPrim->Clear();
	}
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		FYprimFreq = with0->get_FFrequency();
		FreqMultiplier = FYprimFreq / BaseFrequency;

       /***** Quasi Ideal Source for fundamental power flow*****/
		if(((FreqMultiplier - 1.0) < EPSILON) && IsQuasiIdeal && (!with0->IsHarmonicModel))
            // Ideal Source approximation -- impedance matrix is diagonal matrix only
		{
			int stop = 0;
			Zinv->Clear();
			Value = cmulreal(puZideal, ZBase);  // convert to ohms
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				Zinv->SetElement(i, i, Value);
			}
		}
		else
		{
			int stop = 0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{

       /***** Normal Thevenin Source *****/
         /* Put in Series RL Adjusted for frequency -- Use actual values */
				int stop1 = 0;
				for(stop1 = Fnphases, j = 1; j <= stop1; j++)
				{
					Value = Z->GetElement(i, j);
					Value.im = Value.im * FreqMultiplier;  /*Modify from base freq*/
					Zinv->SetElement(i, j, Value);
				}
			}
		}
	}
	Zinv->Invert();  /*Invert in place*/
	if(Zinv->InvertError > 0)       /*If error, put in Large series conductance*/
	{
		int stop = 0;
		DoErrorMsg("TVsourceObj.CalcYPrim", String("Matrix Inversion Error for Vsource \"") + get_Name()
	           + "\"", "Invalid impedance specified. Replaced with small resistance.", 325);
		Zinv->Clear();
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			Zinv->SetElement(i, i, cmplx(1.0 / EPSILON, 0.0));
		}
	}

   // YPrim_Series.CopyFrom(Zinv);
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Fnphases, j = 1; j <= stop1; j++)
		{
			Value = Zinv->GetElement(i, j);
			YPrim_Series->SetElement(i, j, Value);
			YPrim_Series->SetElement(i + Fnphases, j + Fnphases, Value);
          //YPrim_series.SetElemsym(i + FNPhases, j, CNegate(Value))
			YPrim_Series->SetElement(i, j + Fnphases, cnegate(Value));
			YPrim_Series->SetElement(i + Fnphases, j, cnegate(Value));
		}
	}
	YPrim->CopyFrom(YPrim_Series);
     
     /*Now Account for Open Conductors*/
     /*For any conductor that is open, zero out row and column*/
	inherited::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

//=============================================================================

void TVsourceObj::GetVterminalForSource(int ActorID)
{
	int i = 0;
	complex Vharm = {};
	double SrcHarmonic = 0.0;
	try


  /*
   This formulation will theoretically handle voltage sources of
   any number of phases assuming they are
   equally displaced in time.
  */
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			ShapeIsActual = false;

          /*Modify magnitude based on a LOADSHAPE if assigned*/
			switch(with0->Get_SolMode())
			{
				case 	DAILYMODE:
               /*Uses same logic as LOAD*/
				{
					CalcDailyMult(with0->DynaVars.dblHour); // set Shapefactor.re = Pmult(t) or PerUnit
				}
				break;
				case 	YEARLYMODE:
				{
					CalcYearlyMult(with0->DynaVars.dblHour);
				}
				break;
				case 	DUTYCYCLE:
				{
					CalcDutyMult(with0->DynaVars.dblHour);
				}
				break;
                                 // This mode allows use of one class of load shape in DYNAMIC mode
				case 	DYNAMICMODE:
				{
					switch(ActiveCircuit[ActorID]->ActiveLoadShapeClass)
					{
						case 	USEDAILY:
						CalcDailyMult(with0->DynaVars.dblHour);
						break;
						case 	USEYEARLY:
						CalcYearlyMult(with0->DynaVars.dblHour);
						break;
						case 	USEDUTY:
						CalcDutyMult(with0->DynaVars.dblHour);
						break;
						default:
						ShapeFactor = cmplx(PerUnit, 0.0);     // default to 1 + j0 if not known
						break;
					}
				}
				break;
				default:
				  ;
				break;
			}
			if((with0->Get_SolMode() == DAILYMODE) || (with0->Get_SolMode() == YEARLYMODE) || (with0->Get_SolMode() == DUTYCYCLE) || (with0->Get_SolMode() == DYNAMICMODE))     /*If a loadshape mode simulation*/  /*Loadshape cases*/
			{
				if(ShapeIsActual)
					Vmag = 1000.0 * ShapeFactor.re;
				else
					switch(Fnphases)
					{
						case 	1:
						Vmag = kVBase * ShapeFactor.re * 1000.0;
						break;
						default:
						Vmag = kVBase * ShapeFactor.re * 1000.0 / 2.0 / sin((180.0 / Fnphases) * DSSGlobals::PI / 180.0L);
						break;
					}
			}
			else
			switch(Fnphases)
			{  // Normal Case
				case 	1:
				Vmag = kVBase * PerUnit * 1000.0;
				break;
				default:
				Vmag = kVBase * PerUnit * 1000.0 / 2.0 / sin((180.0 / Fnphases) * DSSGlobals::PI / 180.0L);
				break;
			}
			if(with0->IsHarmonicModel)
			{
				int stop = 0;
				SrcHarmonic = with0->get_FFrequency() / SrcFrequency;
				Vharm = cmulreal(SpectrumObj->GetMult(SrcHarmonic), Vmag);  // Base voltage for this harmonic
				RotatePhasorDeg(Vharm, SrcHarmonic, Angle);  // Rotate for phase 1 shift
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					(Vterminal)[i - 1] = Vharm;
					(Vterminal)[i + Fnphases - 1] = CZero;
					if(i < Fnphases)
					{
						switch(ScanType)
						{
							case 	1:
							RotatePhasorDeg(Vharm, 1.0, -360.0 / Fnphases);
							break; // maintain pos seq
							case 	0:
							;
							break;  // Do nothing for Zero Sequence; All the same
							default:
							RotatePhasorDeg(Vharm, SrcHarmonic, -360.0 / Fnphases); // normal rotation
							break;
						}
					}
				}
			}
			else
  // non-harmonic modes
			{
				int stop = 0;
				if(Abs((int) (with0->get_FFrequency() - SrcFrequency)) > EPSILON2)
					Vmag = 0.0;  // Solution Frequency and Source Frequency don't match!
					         /*NOTE: RE-uses VTerminal space*/
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					switch(SequenceType)
					{
						case -1:
						(Vterminal)[i - 1] = pdegtocomplex(Vmag, (360.0 + Angle + (i - 1) * 360.0 / Fnphases));
						break;  // neg seq
						case 	0:
						(Vterminal)[i - 1] = pdegtocomplex(Vmag, (360.0 + Angle));
						break;   // all the same for zero sequence
						default:
						(Vterminal)[i - 1] = pdegtocomplex(Vmag, (360.0 + Angle - (i - 1) * 360.0 / Fnphases));
						break;
					}
					(Vterminal)[i + Fnphases - 1] = CZero;    // See comments in GetInjCurrents
				}
			}
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Error computing Voltages for Vsource.") + get_Name()
	           + ". Check specification. Aborting.", 326);
		if(In_Redirect)
			Redirect_Abort = true;
	}
}

//===========================================================================

int TVsourceObj::InjCurrents(int ActorID)
{
	int result = 0;
	GetInjCurrents(InjCurrent, ActorID);

/*This is source injection*/
	result = inherited::InjCurrents(ActorID); // Add into system array
	return result;
}

//===========================================================================

void TVsourceObj::CalcInjCurrAtBus(pComplexArray Curr, int ActorID)
{
	vector <complex> ElmCurrents;							            // For storing the currents of the PDE
	string BusName = StripExtension(FBusNames[0]);						// Gets the name of the bus we are connected to
	auto with1 = ActiveCircuit[ActorID];

	auto ActiveElem = with1->FActiveCktElement;							// saves whatever the active ckt element is
	//  First, inspect the currents based on PDE
	DynStringArray myList = with1->getPDEatBus(BusName);		        // Obtains the list of PDE connected to the Bus
	int myTerm = 0;											            // The terminal of the PDE connected to the Bus

	for (int idx = 0; idx < Yorder; idx++)
		Curr[idx] = CZero;

	auto ActivePDE = ActiveElem;	

	for (int idx = 0; idx < myList.size(); idx++)						// We go through all the devices
	{
		myTerm = 0;														// Restarts the terminal
		with1->SetElementActive(myList[idx]);
		ActivePDE = with1->FActiveCktElement;							// To prevent super long statements

		ElmCurrents.resize((ActivePDE->Yorder) + 1);
		ActivePDE->GetCurrents(&(ElmCurrents[0]), ActorID);

		for (int j = 0; j < ActivePDE->Fnphases; j++)
		{
			if (BusName == StripExtension(ActivePDE->FBusNames[j]))
				break;
			myTerm++;
		}

		for (int j = 0; j < Fnphases; j++)
		{
			Curr[j] = csub(Curr[j], ElmCurrents[(myTerm * int(ActivePDE->Yorder / 2)) + j]);
		}

	}

	ElmCurrents.clear();
	myList.clear();

	// Now check the PCE at the same Bus
	myList = with1->getPCEatBus(BusName);		                        // Obtains the list of PCE connected to the Bus
	myTerm = 0;											                // The terminal of the PCE connected to the Bus
	string myName = LowerCase(ParentClass->Class_Name + "." + get_Name());

	for (int idx = 0; idx < myList.size(); idx++)						// We go through all the devices
	{
		if (LowerCase(myList[idx]) != myName)
		{
			with1->SetElementActive(myList[idx]);
			auto ActivePCE = with1->FActiveCktElement;			        // To prevent super long statements

			ElmCurrents.resize((ActivePCE->Yorder) + 1);
			ActivePCE->GetCurrents(&(ElmCurrents[0]), ActorID);

			for (int j = 0; j < ActivePCE->Fnphases; j++)
			{
				if (BusName == StripExtension(ActivePCE->FBusNames[j]))
					break;
				myTerm++;
			}
			for (int j = 0; j < Fnphases; j++)
			{
				Curr[j] = cadd(ElmCurrents[(myTerm * ActivePCE->Fnphases) + j], Curr[j]);
			}
		}
	}
	with1->Set_ActiveCktElement(ActiveElem);								// returns the active ckt element to the ckt instance
}

//===========================================================================

void TVsourceObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	//complex Vterminal[6];
	try
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			 //FOR i := 1 TO (Nterms * NConds) DO Vtemp^[i] := V^[NodeRef^[i]];
			 // This is safer    12/7/99
			if ((ActiveCircuit[ActiveActor]->Solution->Algorithm == NCIMSOLVE) && (NodeRef[0] == 1))
			{
				CalcInjCurrAtBus(Curr, ActorID);
			}
			else
			{
				int stop = 0;
				for (stop = Yorder, i = 1; i <= stop; i++)
				{
					if (!ADiakoptics || (ActorID == 1))
					{
						Vterminal[i - 1] = with0->NodeV[NodeRef[i - 1]];
					}
					else
						Vterminal[i - 1] = with0->VoltInActor1(NodeRef[i - 1]);
				}
				YPrim->MVmult(Curr, &(Vterminal[0]));  // Current from Elements in System Y
				GetInjCurrents(&(ComplexBuffer[0]), ActorID);  // Get present value of inj currents
				// Add Together  with yprim currents
				for (stop = Yorder, i = 1; i <= stop; i++)
				{
					Curr[i - 1] = csub(Curr[i - 1], ComplexBuffer[i - 1]);
				}
			}
		}  /*With*/
	}
	catch (std::exception &e)
	{
		DoErrorMsg((String("GetCurrents for Element: ") + get_Name() + "."), (std::string) e.what(), "Inadequate storage allotted for circuit element.", 327);
	}
}


//=============================================================================

void TVsourceObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{


   /* source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |Vsource  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   */
	GetVterminalForSource(ActorID);  // gets voltage vector above
	YPrim->MVmult(Curr, &(Vterminal[0]));
	set_ITerminalUpdated(false, ActorID);
}

//=============================================================================

void TVsourceObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int j = 0;
	complex C = {};
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
	{
		int stop = 0;
		WriteLn(f);
		{ Write(f, "BaseFrequency="); WriteLn(f, BaseFrequency, 0, 1); }
		{ Write(f, "VMag="); WriteLn(f, Vmag, 0, 2); }
		WriteLn(f, "Z Matrix=");
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = i, j = 1; j <= stop1; j++)
			{
				C = Z->GetElement(i, j);
				Write(f, Format("%.8g +j %.8g ", C.re, C.im));
			}
			WriteLn(f);
		}
	}
}


//=============================================================================

void TVsourceObj::InitPropertyValues(int ArrayOffset)
{


     /*PropertyValue Allocated in DSSObject.Create*/
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,"115");
	Set_PropertyValue(3,"1");
	Set_PropertyValue(4,"0");
	Set_PropertyValue(5,Format("%d", Round(ActiveCircuit[ActiveActor]->Fundamental)));
	Set_PropertyValue(6,"3");
	Set_PropertyValue(7,"2000");
	Set_PropertyValue(8,"2100");
	Set_PropertyValue(9,"4");
	Set_PropertyValue(10,"3");
	Set_PropertyValue(11,"10000");
	Set_PropertyValue(12,"10500");
	Set_PropertyValue(13,"1.65");
	Set_PropertyValue(14,"6.6");
	Set_PropertyValue(15,"1.9");
	Set_PropertyValue(16,"5.7");
	Set_PropertyValue(17,"Pos");
	Set_PropertyValue(18,"Pos");
	Set_PropertyValue(19,GetBus(2));
	Set_PropertyValue(20,"[ 0 0 ]");
	Set_PropertyValue(21,"[ 0 0 ]");
	Set_PropertyValue(22,"[ 0 0 ]");
	Set_PropertyValue(23,"[ 0 0 ]");
	Set_PropertyValue(24,"[ 0 0 ]");
	Set_PropertyValue(25,"[ 0 0 ]");
	Set_PropertyValue(26,"100");
	Set_PropertyValue(27,"");
	Set_PropertyValue(28,"");
	Set_PropertyValue(29,"");
	Set_PropertyValue(30,"Thevenin");
	Set_PropertyValue(31,"[1.0e-6, 0.001]");
	inherited::InitPropertyValues(NumPropsThisClass);

}

//=============================================================================

String TVsourceObj::GetPropertyValue(int Index)
{
	String result;
	switch(Index)
	{
		case 	1:
		result = GetBus(1);
		break;
		case 	4:
		result = Format("%-.5g", Angle);
		break;
		case 	7:
		result = Format("%-.5g", MVAsc3);
		break;
		case 	8:
		result = Format("%-.5g", MVAsc1);
		break;
		case 	11:
		result = Format("%-.5g", Isc3);
		break;
		case 	12:
		result = Format("%-.5g", Isc1);
		break;
		case 	13:
		result = Format("%-.5g", R1);
		break;
		case 	14:
		result = Format("%-.5g", X1);
		break;
		case 	15:
		result = Format("%-.5g", R0);
		break;
		case 	16:
		result = Format("%-.5g", X0);
		break;
		case 	19:
		result = GetBus(2);
		break;
		case 	20:
		result = Format("[%-.8g, %-.8g]", R1, X1);
		break;
		case 	21:
		result = Format("[%-.8g, %-.8g]", R0, X0);
		break;
		case 	22:
		result = Format("[%-.8g, %-.8g]", R2, X2);
		break;
		case 	23:
		result = Format("[%-.8g, %-.8g]", puZ1.re, puZ1.im);
		break;
		case 	24:
		result = Format("[%-.8g, %-.8g]", puZ0.re, puZ0.im);
		break;
		case 	25:
		result = Format("[%-.8g, %-.8g]", puZ2.re, puZ2.im);
		break;
		case 	26:
		result = Format("%-.5g", BaseMVA);
		break;
		case 	31:
		result = Format("[%-.8g, %-.8g]", puZideal.re, puZideal.im);
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}


//----------------------------------------------------------------------------

void TVsourceObj::CalcDailyMult(double hr)
{
	if(DailyShapeObj != nullptr)
	{
		ShapeFactor = DailyShapeObj->GetMult(hr);
		ShapeIsActual = DailyShapeObj->UseActual;
	}
	else
	ShapeFactor = cmplx(PerUnit, 0.0); // Default to no daily variation
}


//----------------------------------------------------------------------------

void TVsourceObj::CalcDutyMult(double hr)
{
	if(DutyShapeObj != nullptr)
	{
		ShapeFactor = DutyShapeObj->GetMult(hr);
		ShapeIsActual = DutyShapeObj->UseActual;
	}
	else
	CalcDailyMult(hr);  // Default to Daily Mult IF no duty curve specified
}

//----------------------------------------------------------------------------

void TVsourceObj::CalcYearlyMult(double hr)
{

/*Yearly curve is assumed to be hourly only*/
	if(YearlyShapeObj != nullptr)
	{
		ShapeFactor = YearlyShapeObj->GetMult(hr);
		ShapeIsActual = YearlyShapeObj->UseActual;
	}
	else
	ShapeFactor = cmplx(PerUnit, 0.0); // Defaults to no variation
}

//=============================================================================

void TVsourceObj::MakePosSequence(int ActorID)
{
	String s;
	s = "Phases=1 ";
	s = s + Format("BasekV=%-.5g ", kVBase / SQRT3);
	s = s + Format("R1=%-.5g ", R1);
	s = s + Format("X1=%-.5g ", X1);
	Parser[ActorID]->SetCmdString(s);
	Edit(ActorID);
	inherited::MakePosSequence(ActorID);
}


void VSource_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
}

		class 		VSource_unit
		{
		public:
		VSource_unit()
		{
			//AssertSystemInitialization();
			VSource_initialization();
		}
		};
		VSource_unit _VSource_unit;

}  // namespace VSource




