

#pragma hdrstop

#include "Line.h"

#include "ControlElem.h"
#include "DSSGlobals.h"
#include "Utilities.h"
#include "mathutil.h"
#include "Circuit.h"


using namespace std;
using namespace Arraydef;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace ConductorData;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace LineCode;
using namespace LineGeometry;
using namespace LineSpacing;
using namespace LineUnits;
using namespace PDClass;
using namespace PDELement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace Line
{

TLineObj::TLineObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TLineObj::TLineObj(String ClassName) : inherited(ClassName) {}
TLineObj::TLineObj() {}


TLineObj* ActiveLineObj = nullptr;
TLineGeometry* LineGeometryClass = nullptr;
const int NumPropsThisClass = 30;
    //  MaxPhases = 20; // for fixed buffers
complex CAP_EPSILON = {};
const double ONE_THIRD = 1.0 / 3.0;  // Do this to get more precision in next few statements

TLineCode* LineCodeClass = nullptr;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TLine::TLine()
{
	;
	Class_Name = "Line";
	DSSClassType = DSSClassType + LINE_ELEMENT; // in both PDElement list and Linesection lists
	ActiveElement = 0;
	LineCodeClass = nullptr;
	LineGeometryClass = nullptr;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLine::~TLine()
{


    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLine::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "bus1";
	PropertyName[2 - 1] = "bus2";
	PropertyName[3 - 1] = "linecode";
	PropertyName[4 - 1] = "length";
	PropertyName[5 - 1] = "phases";
	PropertyName[6 - 1] = "r1";
	PropertyName[7 - 1] = "x1";
	PropertyName[8 - 1] = "r0";
	PropertyName[9 - 1] = "x0";
	PropertyName[10 - 1] = "C1";
	PropertyName[11 - 1] = "C0";
	PropertyName[12 - 1] = "rmatrix";
	PropertyName[13 - 1] = "xmatrix";
	PropertyName[14 - 1] = "cmatrix";
	PropertyName[15 - 1] = "Switch";
	PropertyName[16 - 1] = "Rg";
	PropertyName[17 - 1] = "Xg";
	PropertyName[18 - 1] = "rho";
	PropertyName[19 - 1] = "geometry";
	PropertyName[20 - 1] = "units";
	PropertyName[21 - 1] = "spacing";
	PropertyName[22 - 1] = "wires";
	PropertyName[23 - 1] = "EarthModel";
	PropertyName[24 - 1] = "cncables";
	PropertyName[25 - 1] = "tscables";
	PropertyName[26 - 1] = "B1";
	PropertyName[27 - 1] = "B0";
	PropertyName[28 - 1] = "Seasons";
	PropertyName[29 - 1] = "Ratings";
	PropertyName[30 - 1] = "LineType";

     // define Property help values
	PropertyHelp[1 - 1] = String("Name of bus to which first terminal is connected.") + CRLF
	           + "Example:"
	           + CRLF
	           + "bus1=busname   (assumes all terminals connected in normal phase order)"
	           + CRLF
	           + "bus1=busname.3.1.2.0 (specify terminal to node connections explicitly)";
	PropertyHelp[2 - 1] = "Name of bus to which 2nd terminal is connected.";
	PropertyHelp[3 - 1] = String("Name of linecode object describing line impedances.") + CRLF
	           + "If you use a line code, you do not need to specify the impedances here. "
	           + "The line code must have been PREVIOUSLY defined. "
	           + "The values specified last will prevail over those specified earlier (left-to-right "
	           + "sequence of properties).  You can subsequently change the number of phases if symmetrical component quantities are specified."
	           + "If no line code or impedance data are specified, the line object "
	           + "defaults to 336 MCM ACSR on 4 ft spacing.";
	PropertyHelp[4 - 1] = "Length of line. Default is 1.0. If units do not match the impedance data, specify \"units\" property. ";
	PropertyHelp[5 - 1] = "Number of phases, this line.";
	PropertyHelp[6 - 1] = "Positive-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition. See also Rmatrix.";
	PropertyHelp[7 - 1] = "Positive-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition.  See also Xmatrix";
	PropertyHelp[8 - 1] = "Zero-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition.";
	PropertyHelp[9 - 1] = "Zero-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition.";
	PropertyHelp[10 - 1] = "Positive-sequence capacitance, nf per unit length.  Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition. See also Cmatrix and B1.";
	PropertyHelp[11 - 1] = "Zero-sequence capacitance, nf per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition.See also B0.";
	PropertyHelp[12 - 1] = "Resistance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. "
	           "May be used to specify the impedance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix "
	           "forces program to use the matrix values for line impedance definition. For balanced line models, you may "
	           "use the standard symmetrical component data definition instead.";
	PropertyHelp[13 - 1] = "Reactance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. "
	           "May be used to specify the impedance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix "
	           "forces program to use the matrix values for line impedance definition.  For balanced line models, you may "
	           "use the standard symmetrical component data definition instead.";
	PropertyHelp[14 - 1] = "Nodal Capacitance matrix, lower triangle, nf per unit length.Order of the matrix is the number of phases. "
	           "May be used to specify the shunt capacitance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix "
	           "forces program to use the matrix values for line impedance definition.  For balanced line models, you may "
	           "use the standard symmetrical component data definition instead.";
	PropertyHelp[15 - 1] = String("{y/n | T/F}  Default= no/false.  Designates this line as a switch for graphics and algorithmic purposes. ") + CRLF
	           + "SIDE EFFECT: Sets r1 = 1.0; x1 = 1.0; r0 = 1.0; x0 = 1.0; c1 = 1.1 ; c0 = 1.0;  length = 0.001; You must reset if you want something different.";
	PropertyHelp[16 - 1] = "Carson earth return resistance per unit length used to compute impedance values at base frequency. "
	           "Default is 0.01805 = 60 Hz value in ohms per kft (matches default line impedances). "
	           "This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. "
	           "If not, set both Rg and Xg = 0.";
	PropertyHelp[17 - 1] = "Carson earth return reactance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. "
	           "Default is 0.155081 = 60 Hz value in ohms per kft (matches default line impedances). "
	           "This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. "
	           "If not, set both Rg and Xg = 0.";
	PropertyHelp[18 - 1] = "Default=100 meter ohms.  Earth resitivity used to compute earth correction factor. Overrides Line geometry definition if specified.";
	PropertyHelp[19 - 1] = "Geometry code for LineGeometry Object. Supercedes any previous definition of line impedance. "
	           "Line constants are computed for each frequency change or rho change. CAUTION: may alter number of phases. "
	           "You cannot subsequently change the number of phases unless you change how the line impedance is defined.";
	PropertyHelp[20 - 1] = "Length Units = {none | mi|kft|km|m|Ft|in|cm } Default is None - assumes length units match impedance units.";
	PropertyHelp[21 - 1] = String("Reference to a LineSpacing for use in a line constants calculation.") + CRLF
	           + "Must be used in conjunction with the Wires property."
	           + CRLF
	           + "Specify this before the wires property.";
	PropertyHelp[22 - 1] = String("Array of WireData names for use in an overhead line constants calculation.") + CRLF
	           + "Must be used in conjunction with the Spacing property."
	           + CRLF
	           + "Specify the Spacing first, and \"ncond\" wires."
	           + CRLF
	           + "May also be used to specify bare neutrals with cables, using \"ncond-nphase\" wires.";
	PropertyHelp[23 - 1] = "One of {Carson | FullCarson | Deri}. Default is the global value established with the Set EarthModel command. "
	           "See the Options Help on EarthModel option. This is used to override the global value for this line. This "
	           "option applies only when the \"geometry\" property is used.";
	PropertyHelp[24 - 1] = String("Array of CNData names for use in a cable constants calculation.") + CRLF
	           + "Must be used in conjunction with the Spacing property."
	           + CRLF
	           + "Specify the Spacing first, using \"nphases\" cncables."
	           + CRLF
	           + "You may later specify \"nconds-nphases\" wires for separate neutrals";
	PropertyHelp[25 - 1] = String("Array of TSData names for use in a cable constants calculation.") + CRLF
	           + "Must be used in conjunction with the Spacing property."
	           + CRLF
	           + "Specify the Spacing first, using \"nphases\" tscables."
	           + CRLF
	           + "You may later specify \"nconds-nphases\" wires for separate neutrals";
	PropertyHelp[26 - 1] = "Alternate way to specify C1. MicroS per unit length";
	PropertyHelp[27 - 1] = "Alternate way to specify C0. MicroS per unit length";
	PropertyHelp[28 - 1] = "Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the \"Ratings\" property.";
	PropertyHelp[29 - 1] = String("An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert") + CRLF
	           + "multiple ratings to change during a QSTS simulation to evaluate different ratings in lines.";
	PropertyHelp[30 - 1] = String("Code designating the type of line. ") + CRLF
	           + "One of: OH, UG, UG_TS, UG_CN, SWT_LDBRK, SWT_FUSE, SWT_SECT, SWT_REC, SWT_DISC, SWT_BRK, SWT_ELBOW, BUSBAR"
	           + CRLF
	           + CRLF
	           + "OpenDSS currently does not use this internally. For whatever purpose the user defines. Default is OH.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
	PropertyHelp[NumPropsThisClass + 3 - 1] = "Failure rate PER UNIT LENGTH per year. Length must be same units as LENGTH property. Default is 0.1 fault per unit length per year.";
	PropertyHelp[NumPropsThisClass + 4 - 1] = PropertyHelp[NumPropsThisClass + 4 - 1] + " Default is 20.";
	PropertyHelp[NumPropsThisClass + 5 - 1] = PropertyHelp[NumPropsThisClass + 5 - 1] + " Default is 3 hr.";
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLine::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TLineObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

void TLineObj::UpdatePDProperties()
{
	String TempStr;
	int j = 0;
	int stop = 0;
	Set_PropertyValue(28,Format("%-d", NumAmpRatings));
	TempStr = "[";
	for(int stop = NumAmpRatings, j = 1; j <= stop; j++)
	{
		TempStr = TempStr + FloatToStrF(AmpRatings[j - 1], ffGeneral, 8, 4) + ",";
	}
	TempStr = TempStr + "]";
	Set_PropertyValue(29,TempStr);
	Set_PropertyValue(NumPropsThisClass + 1,to_string(NormAmps));
	Set_PropertyValue(NumPropsThisClass + 2,to_string(EmergAmps));
  // commented out 8/26/2014
  // PropertyValue[NumPropsThisClass + 3] := Format('%-g', [FaultRate]);
  // PropertyValue[NumPropsThisClass + 4] := Format('%-g', [PctPerm]);
  // PropertyValue[NumPropsThisClass + 5] := Format('%-g', [HrsToRepair]);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLineObj::FetchLineCode(const String Code)
{
	TLineCodeObj* LineCodeObj = nullptr;
	int i = 0;
	if(LineCodeClass == nullptr)
		LineCodeClass = (TLineCode*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("linecode"));
	if(LineCodeClass->SetActive(Code))
	{
		LineCodeObj = ((TLineCodeObj*) LineCodeClass->GetActiveObj());
		CondCode = LowerCase(Code);

       // Frequency compensation takes place in calcYPrim.
		BaseFrequency = LineCodeObj->BaseFrequency;
       /*Copy impedances from line code, but do not recalc because symmetrical
        component z's may not match what's in matrix*/
		if(LineCodeObj->SymComponentsModel)
		{
			R1 = LineCodeObj->R1;
			X1 = LineCodeObj->X1;
			R0 = LineCodeObj->R0;
			X0 = LineCodeObj->X0;
			C1 = LineCodeObj->C1;
			C0 = LineCodeObj->C0;
			SymComponentsModel = true;
		}
		else
		SymComponentsModel = false;


       // Earth return impedances used to compensate for frequency
		Rg = LineCodeObj->Rg;
		Xg = LineCodeObj->Xg;
		rho = LineCodeObj->rho;
		KXg = Xg / log(658.5L * sqrt(rho / BaseFrequency));
		FLineCodeUnits = LineCodeObj->Units;
		FLineCodeSpecified = true;
		FUnitsConvert = ConvertLineUnits(FLineCodeUnits, LengthUnits);
		NormAmps = LineCodeObj->NormAmps;
		EmergAmps = LineCodeObj->EmergAmps;
		NumAmpRatings = LineCodeObj->NumAmpRatings;
		AmpRatings = LineCodeObj->AmpRatings;

       // These three properties should not come from the Linecode
       //   But can vary from line section to line section
       // commented out 8/26/2014
       // FaultRate := LineCodeObj.FaultRate;
       // PctPerm   := LineCodeObj.PctPerm;
       // HrsToRepair := LineCodeObj.HrsToRepair;
		UpdatePDProperties();
		if(Fnphases != LineCodeObj->Fnphases)
		{
			Set_NPhases(LineCodeObj->Fnphases);
			ReallocZandYcMatrices();
		}
		if(!SymComponentsModel)        // Copy matrices
		{
			Z->CopyFrom(LineCodeObj->Z);
         /*Zinv.CopyFrom(LineCodeObj.Zinv);*/  // no need to copy Zinv
			YC->CopyFrom(LineCodeObj->YC);
		}
		else
		RecalcElementData(ActiveActor);    // Compute matrices
		Set_Nconds(Fnphases);  // Force Reallocation of terminal info
       //Fnconds := Fnphases;
		Yorder = Fnconds * Fnterms;
       // Set_YprimInvalid[ActorID] := True;  (set in Edit; this is redundant)
		FLineType = LineCodeObj->FLineType;
	}
	else
	DoSimpleMsg("Line Code:" + Code + " not found for Line object Line." + get_Name(), 180);
}



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLine::DoRmatrix(int ActorID)
{
	int OrderFound = 0;
	int Norder = 0;
	int j = 0;
	pDoubleArray MatBuffer;
	pComplexArray Zvalues;
	/*# with ActiveLineObj do */
	{
		auto with0 = ActiveLineObj;
       /*Added 3-17-15 in case Z and Yc do not get allocated to the proper value*/
		if(with0->Z->get_Norder() != with0->Fnphases)
			with0->ReallocZandYcMatrices();
		MatBuffer = new double[with0->Fnphases * with0->Fnphases];
		OrderFound = Parser[ActorID]->ParseAsSymMatrix(with0->Fnphases, MatBuffer);
		if(OrderFound > 0)    // Parse was successful
			    /*R*/
			{
				Zvalues = with0->Z->GetValuesArrayPtr(Norder);
				if(Norder == with0->Fnphases)
				{
					int stop = 0;
					for(int stop = with0->Fnphases * with0->Fnphases, j = 1; j <= stop; j++)
					{
						(Zvalues)[j - 1].re = (MatBuffer)[j - 1];
					}
				}
			}
		delete[] MatBuffer; //# FreeMemory accepts one parameter only;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLine::DoXmatrix(int ActorID)
{
	int OrderFound = 0;
	int Norder = 0;
	int j = 0;
	pDoubleArray MatBuffer;
	pComplexArray Zvalues;
	/*# with ActiveLineObj do */
	{
		auto with0 = ActiveLineObj;
		if(with0->Z->get_Norder() != with0->Fnphases)
			with0->ReallocZandYcMatrices();
		MatBuffer = new double[with0->Fnphases * with0->Fnphases];
		OrderFound = Parser[ActorID]->ParseAsSymMatrix(with0->Fnphases, MatBuffer);
		if(OrderFound > 0)    // Parse was successful
			    /*X*/
			{
				Zvalues = with0->Z->GetValuesArrayPtr(Norder);
				if(Norder == with0->Fnphases)
				{
					int stop = 0;
					for(int stop = with0->Fnphases * with0->Fnphases, j = 1; j <= stop; j++)
					{
						(Zvalues)[j - 1].im = (MatBuffer)[j - 1];
					}
				}
			}
		delete[] MatBuffer; //# FreeMemory accepts one parameter only;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLine::DoCmatrix(int ActorID)
{
	int OrderFound = 0;
	int Norder = 0;
	int j = 0;
	pDoubleArray MatBuffer;
	pComplexArray YValues;
	double Factor = 0.0;
	/*# with ActiveLineObj do */
	{
		auto with0 = ActiveLineObj;
		if(with0->Z->get_Norder() != with0->Fnphases)
			with0->ReallocZandYcMatrices();
		MatBuffer = new double[with0->Fnphases * with0->Fnphases];
		OrderFound = Parser[ActorID]->ParseAsSymMatrix(with0->Fnphases, MatBuffer);
		if(OrderFound > 0)    // Parse was successful
			    /*X*/
			{
				Factor = TwoPi * with0->BaseFrequency * 1.0e-9;
				YValues = with0->YC->GetValuesArrayPtr(Norder);
				if(Norder == with0->Fnphases)
				{
					int stop = 0;
					for(int stop = with0->Fnphases * with0->Fnphases, j = 1; j <= stop; j++)
					{
						(YValues)[j - 1].im = Factor * (MatBuffer)[j - 1];
					}
				}
			}
		delete[] MatBuffer; //# FreeMemory accepts one parameter only;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// A Line Defaults to 3-phases and some typical symmetrical component data
/*
 Line impedances are specified in per unit length and are multiplied by the length
 when the primitive Y matrix is computed.

 You may specify the impedances of the line either by symmetrical components or
 by R, X, and nodal C matrices  (also per unit length).

 All C's is entered in nano farads.

 The ultimate values are in the matrices.  If you specify matrices, then the symmetrical
 component values are ignored.  However, if you change any of the symmetrical component values
 the matrices will be recomputed.  It is assumed you want to use symmetrical component values.
 Don't mix data entry by matrix and by symmetrical components.

 Note that if you change the number of phases, the matrices are reallocated and reinitialized
 with whatever is currently in the symmetrical component data.

*/

int TLine::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int NewLengthUnits = 0;
	result = 0;
  // continue parsing with contents of Parser
	ActiveLineObj = (TLineObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveLineObj);  // use property to set this value
	/*# with ActiveLineObj do */
	{
		auto with0 = ActiveLineObj;
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
	           + "\" for Object \"Line."
	           + with0->get_Name()
	           + "\"", 181);
				break;
				case 	1:
				with0->SetBus(1, Param);
				break;
				case 	2:
				with0->SetBus(2, Param);
				break;
				case 	3:
				with0->FetchLineCode(Param);
				break;  // Define line by conductor code
				case 	4:
				with0->Len = Parser[ActorID]->MakeDouble_();
				break; /*Nphases: See below*/
				case 	5:
				;
				break;
				case 	6:
				with0->R1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				with0->X1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
				with0->R0 = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
				with0->X0 = Parser[ActorID]->MakeDouble_();
				break;
				case 	10:
				{
					with0->C1 = Parser[ActorID]->MakeDouble_() * 1.0e-9;
					with0->FCapSpecified = true;
				}
				break; // Convert from nano to farads
				case 	11:
				{
					with0->C0 = Parser[ActorID]->MakeDouble_() * 1.0e-9;
					with0->FCapSpecified = true;
				}
				break;
				case 	12:
				DoRmatrix(ActorID);
				break;
				case 	13:
				DoXmatrix(ActorID);
				break;
				case 	14:
				{
					DoCmatrix(ActorID);
					with0->FCapSpecified = true;
				}
				break;
				case 	15:
				with0->IsSwitch = InterpretYesNo(Param);
				break;
				case 	16:
				with0->Rg = Parser[ActorID]->MakeDouble_();
				break;
				case 	17:
				with0->Xg = Parser[ActorID]->MakeDouble_();
				break;
				case 	18:
				{
					with0->rho = Parser[ActorID]->MakeDouble_();
					with0->FrhoSpecified = true;
				}
				break;
				case 	19:
				with0->FetchGeometryCode(Param);
				break; // Update units conversion factor that might have been changed previously
				case 	20:
				{
					NewLengthUnits = GetUnitsCode(Param);
					if(with0->FLineCodeSpecified)
						with0->FUnitsConvert = ConvertLineUnits(with0->FLineCodeUnits, NewLengthUnits);
					else
						with0->FUnitsConvert = with0->FUnitsConvert * ConvertLineUnits(with0->LengthUnits, NewLengthUnits);
					with0->LengthUnits = NewLengthUnits;
					with0->FUserLengthUnits = with0->LengthUnits;
				}
				break;
				case 	21:
				with0->FetchLineSpacing(Param);
				break;
				case 	22:
				with0->FetchWireList(Param);
				break;
				case 	23:
				with0->FEarthModel = InterpretEarthModel(Param);
				break;
				case 	24:
				with0->FetchCNCableList(Param);
				break;
				case 	25:
				with0->FetchTSCableList(Param);
				break;
				case 	26:
				{
					with0->C1 = Parser[ActorID]->MakeDouble_() / (TwoPi * with0->BaseFrequency) * 1.0e-6 / with0->FUnitsConvert;
					with0->FCapSpecified = true;
				}
				break;
				case 	27:
				{
					with0->C0 = Parser[ActorID]->MakeDouble_() / (TwoPi * with0->BaseFrequency) * 1.0e-6 / with0->FUnitsConvert;
					with0->FCapSpecified = true;
				}
				break;
				case 	28:
				{
					with0->NumAmpRatings = Parser[ActorID]->MakeInteger_();
					with0->AmpRatings.resize( with0->NumAmpRatings );
				}
				break;
				case 	29:
				{
					with0->AmpRatings.resize( with0->NumAmpRatings );
					Param = Parser[ActiveActor]->MakeString_();
					with0->NumAmpRatings = InterpretDblArray(Param, with0->NumAmpRatings, &( with0->AmpRatings[0]));
				}
				break;
				case 	30:
				with0->FLineType = LineTypeList.Getcommand(Param);
				break;
            // Inherited Property Edits
				default:
				ClassEdit(ActiveLineObj, ParamPointer - NumPropsThisClass);
				break;
			}

         // Side Effects ...
			switch(ParamPointer)
			{
				case 	3:
				{
					with0->SpacingSpecified = false;
					if(with0->GeometrySpecified == true)
						with0->KillGeometrySpecified();
					with0->GeometrySpecified = false;
				}
				break;     // for Reliability calcs -- see PDElement.Pas
				case 	4: case 20:
				with0->MilesThisLine = with0->Len * ConvertLineUnits(with0->LengthUnits, UNITS_MILES);
				break; /*Change the number of phases ... only valid if SymComponentsModel=TRUE*/
				case 	5:
				if(with0->Fnphases != Parser[ActorID]->MakeInteger_())
				{
					if((!with0->GeometrySpecified) && with0->SymComponentsModel)  // ignore change of nphases if geometry used
					{
						with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
						with0->Set_Nconds(with0->Fnphases);  // Force Reallocation of terminal info
						with0->Yorder = with0->Fnterms * with0->Fnconds;
                 /*Set_YprimInvalid[ActorID] := True;*/  // now set below
						with0->RecalcElementData(ActorID);  // Reallocate Z, etc.
					}
					else
					{
						DoSimpleMsg(String("Illegal change of number of phases for Line.") + with0->get_Name(), 18101);
					}
				}
				break;
				case 6: case 7: case 8: case 9: case 10: case 11: case 26: case 27:
				{
					with0->FLineCodeSpecified = false;
					with0->KillGeometrySpecified();
					with0->KillSpacingSpecified();
					with0->ResetLengthUnits();
					with0->SymComponentsChanged = true;
					with0->SymComponentsModel = true;
				}
				break;
				case 12: case 13: case 14:
				{
					with0->FLineCodeSpecified = false;
					with0->SymComponentsModel = false;
					with0->ResetLengthUnits();
					with0->KillGeometrySpecified();
					with0->KillSpacingSpecified();
				}
				break;
				case 	15:
				if(with0->IsSwitch)
				{
					with0->SymComponentsChanged = true;
					with0->Set_YprimInvalid(ActorID,true);
					with0->GeometrySpecified = false;
					with0->SpacingSpecified = false;
					with0->R1 = 1.0;
					with0->X1 = 1.0;
					with0->R0 = 1.0;
					with0->X0 = 1.0;
					with0->C1 = 1.1 * 1.0e-9;
					with0->C0 = 1.0 * 1.0e-9;
					with0->Len = 0.001;
					with0->ResetLengthUnits();
				}
				break;
				case 17: case 18:
				with0->KXg = with0->Xg / log(658.5L * sqrt(with0->rho / with0->BaseFrequency));
				break;
				case 	19:
				{
					with0->GeometrySpecified = true;
					with0->SymComponentsModel = false;
					with0->SymComponentsChanged = false;
				}
				break;
				case 21: case 22: case 24: case 25:
				{
					if(ASSIGNED(with0->FLineSpacingObj) && (!with0->FLineWireData.empty()))
					{
						with0->SpacingSpecified = true;
						with0->SymComponentsModel = false;
						with0->SymComponentsChanged = false;
						with0->KillGeometrySpecified();
						with0->FRatingsSpecified = false;
					}
					with0->Set_YprimInvalid(ActorID,true);
				}
				break;
				case 28: case 29: case 31: case 32: with0->FRatingsSpecified = true; break;
				default:
				  ;
				break;
			}

         //YPrim invalidation on anything that changes impedance values
			switch(ParamPointer)
			{
				case 3: case 4: case 5: case 6: case 7: case 8: case 9: case 10: case 11: case 12:
				 case 13: case 14:
				with0->Set_YprimInvalid(ActorID,true);
				break;
				case 	18:
				if(with0->GeometrySpecified && ASSIGNED(with0->FLineGeometryObj))
					with0->FLineGeometryObj->Set_RhoEarth(with0->rho);
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}

     // If SymComponentsChanged THEN RecalcElementData;
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLine::MakeLike(const String LineName)
{
	int result = 0;
	TLineObj* OtherLine = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line name in the present collection*/
	OtherLine = ((TLineObj*) Find(LineName));
	if(OtherLine != nullptr)
		/*# with ActiveLineObj do */
		{
			auto with0 = ActiveLineObj;
			int stop = 0;
			if(with0->Fnphases != OtherLine->Fnphases)
			{
				with0->Set_NPhases(OtherLine->Fnphases);
				with0->Set_Nconds(with0->Fnphases); // force reallocation of terminals and conductors
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
				if(with0->Z != nullptr)
					delete with0->Z;
				if(with0->Zinv != nullptr)
					delete with0->Zinv;
				if(with0->YC != nullptr)
					delete with0->YC;

         // For a line, nphases = ncond, for now
				with0->Z = new TcMatrix(with0->Fnphases);
				with0->Zinv = new TcMatrix(with0->Fnphases);
				with0->YC = new TcMatrix(with0->Fnphases);
			}
			with0->Z->CopyFrom(OtherLine->Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
			with0->YC->CopyFrom(OtherLine->YC);
			with0->R1 = OtherLine->R1;
			with0->X1 = OtherLine->X1;
			with0->R0 = OtherLine->R0;
			with0->X0 = OtherLine->X0;
			with0->C1 = OtherLine->C1;
			with0->C0 = OtherLine->C0;
			with0->Len = OtherLine->Len;
			with0->SymComponentsModel = OtherLine->SymComponentsModel;
			with0->FCapSpecified = OtherLine->FCapSpecified;
			ClassMakeLike(OtherLine);  // Take care of inherited class properties
			for(int stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->FPropertyValue[i - 1] = OtherLine->FPropertyValue[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Line MakeLike: \"") + LineName + "\" Not Found.", 182);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLine::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TLine.Init", -1);
	result = 0;
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLine Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineObj::TLineObj(TDSSClass* ParClass, const String LineName)
 : inherited(ParClass),
			FZFrequency(-1.0),
			FLineCodeUnits(0),
			FUnitsConvert(0.0),
			FLineGeometryObj(nullptr),
			FLineSpacingObj(nullptr),
			FWireDataSize(0),
			FPhaseChoice(Overhead),
			FrhoSpecified(false),
			FLineCodeSpecified(false),
			FEarthModel(0),
			FCapSpecified(false),
			FLineType(0),
			FUserLengthUnits(0),
			Zinv(nullptr),
			Z(nullptr),
			YC(nullptr),
			R1(0.0),
			X1(0.0),
			R0(0.0),
			X0(0.0),
			C1(0.0),
			C0(0.0),
			Len(0.0),
			Rg(0.0),
			Xg(0.0),
			KXg(0.0),
			rho(0.0),
			GeneralPlotQuantity(0.0),
			GeometrySpecified(false),
			SpacingSpecified(false),
			SymComponentsChanged(false),
			SymComponentsModel(false),
			IsSwitch(false)
{
	Set_Name(LowerCase(LineName));
	DSSObjType = ParClass->DSSClassType; // DSSObjType + LINESECTION; // in both PDElement list and Linesection lists
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(2);  // Force allocation of terminals and conductors
	IsSwitch = false;
	R1 = 0.0580;  //ohms per 1000 ft
	X1 = 0.1206;
	R0 = 0.1784;
	X0 = 0.4047;
	C1 = 3.4E-9;  // nf per 1000ft
	C0 = 1.6E-9;
	Len = 1.0;   // 1 kFt
	Z = nullptr;
	Zinv = nullptr;
	YC = nullptr;
	CondCode = "";
	Rg = 0.01805;    //ohms per 1000 ft
	Xg = 0.155081;
	rho = 100.0;
	KXg = Xg / log(658.5L * sqrt(rho / BaseFrequency));
	FrhoSpecified = false;
	FCapSpecified = false;
	FLineWireData.clear();
     /*Basefrequency := 60.0;*/  // set in base class
	NormAmps = 400.0;
	EmergAmps = 600.0;
	PctPerm = 20.0;
	FaultRate = 0.1; // per mile per year
	HrsToRepair = 3.0;
	SymComponentsChanged = false;
	SymComponentsModel = true;
	GeometrySpecified = false;
	GeometryCode = "";
	LengthUnits = UNITS_NONE; // Assume everything matches
	FUserLengthUnits = UNITS_NONE;
	FUnitsConvert = 1.0;
	FLineCodeUnits = UNITS_NONE;
	FLineCodeSpecified = false;
	FEarthModel = DefaultEarthModel;
	FLineType = 1;  // Default to OH  Line
	FRatingsSpecified = false;
	SpacingSpecified = false;
	FLineSpacingObj = nullptr;
	FWireDataSize = 0;
	FPhaseChoice = unknown;
	SpacingCode = "";
	FLineGeometryObj = nullptr;
	InitPropertyValues(0);
	Yorder = Fnterms * Fnconds;
	RecalcElementData(ActiveActor);
	NumAmpRatings = 1;
	AmpRatings.resize( NumAmpRatings );
	AmpRatings[0] = NormAmps;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineObj::~TLineObj()
{
	if(ASSIGNED(Z))
		delete Z;
	if(ASSIGNED(Zinv))
		delete Zinv;
	if(ASSIGNED(YC))
		delete YC;
	FLineWireData.clear();
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLineObj::ReallocZandYcMatrices()
{
	if(Z != nullptr)
		delete Z;
	if(Zinv != nullptr)
		delete Zinv;
	if(YC != nullptr)
		delete YC;

    // For a line, nphases = ncond, for now
	Z = new TcMatrix(Fnphases);
	Zinv = new TcMatrix(Fnphases);
	YC = new TcMatrix(Fnphases);
}
// do long line correction for len and frequwnen

void TLineObj::DoLongLine(double Frequency, double R, double X, double C, double& R_h, double& X_h, double& C_h, double& G_h)
// Do long line correction for len and desired frequency
// Updated the procedure to correct for any frequency. Moved usage to CalcYPrim.
{
	complex Zs, Zm, Ys, Ym, Zc;
	complex GammaL, ExpP, ExpM, SinhGL, CoshGL;

	G_h = EPSILON; // Adding a tiny conductance to avoid skipping correction on lines with C1=0
	Zs = cmplx(R * Len, X * Len * Frequency / BaseFrequency);  // Use X1 for the desired frequency

	Ys = cmplx(G_h, TwoPi * Frequency * C * Len);
	// Apply the long-line correction to obtain Zm and Ym
	// Rearrange things to express as in Arrillaga's book. no difference to original DoLongLine.
	GammaL = csqrt(cmul(Zs, Ys));
	Zc = csqrt(cdiv(Zs, Ys));
	ExpP = cmulreal(cmplx(cos(GammaL.im), sin(GammaL.im)), exp(GammaL.re));
	ExpM = cinv(ExpP);

	SinhGL = cmulreal(csub(ExpP, ExpM), 0.5);
	CoshGL = cmulreal(cadd(ExpP, ExpM), 0.5);
	Zm = cmul(Zc, SinhGL);
	Ym = cmulreal(cmul(cinv(Zc), cdiv(csub(CoshGL, cmplx(1.0, 0.0)), SinhGL)), 2.0);

	// Update values for tested frequency and adjusted for long-line
	// do not replace original X1, R1, C1. We use these locally where the procedure is called.
	R_h = Zm.re / Len;
	X_h = Zm.im / Len;  // X1_h output is already accounting for new frequency
	C_h = Ym.im / Len / TwoPi / Frequency;
	G_h = Ym.re;
}

/*
  This routine is only called when the symmetrical component data have changed
  It computes the values for Z and Yc in ohms per unit length

  Can also compute long line correction for 1-phase pos sequence line models
*/

void TLineObj::RecalcElementData(int ActorID)
{
	complex Zs = CZero,
			ZM = CZero,
			Ys = CZero,
			YM = CZero,
			Ztemp = CZero,
			GammaL = CZero,
			ExpP = CZero,
			ExpM = CZero,
			Exp2P = CZero,
			Exp2M = CZero,
			SinhGL = CZero,
			Tanh2GL = CZero;
	int		i = 0,
			j = 0;
	double	Yc1 = 0.0,
			Yc0 = 0.0;

	int stop = 0;
	ReallocZandYcMatrices();

    /*Only time this is called is if symmetrical components are specified*/
	Ztemp = cmulreal(cmplx(R1, X1), 2.0);
    /*Handle special case for 1-phase line and/or pos seq model */
	if((Fnphases == 1) || ActiveCircuit[ActorID]->PositiveSequence)
	// Long line correction has been moved to CalcYPrim as it must be performed frequency-wise (not only for base freq).
	{
		// Zero sequence the same as positive sequence
		R0 = R1;
		X0 = X1;
		C0 = C1;
	}
	Zs = cmulreal(cadd(Ztemp, cmplx(R0, X0)), ONE_THIRD);
	ZM = cmulreal(csub(cmplx(R0, X0), cmplx(R1, X1)), ONE_THIRD);
	Yc1 = TwoPi * BaseFrequency * C1;
	Yc0 = TwoPi * BaseFrequency * C0;
	Ys = cmulreal(cadd(cmulreal(cmplx(0.0, Yc1), 2.0), cmplx(0.0, Yc0)), ONE_THIRD);
	YM = cmulreal(csub(cmplx(0.0, Yc0), cmplx(0.0, Yc1)), ONE_THIRD);
	for(int stop = Fnphases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		Z->SetElement(i, i, Zs);
		YC->SetElement(i, i, Ys);
		for(int stop1 = i - 1, j = 1; j <= stop1; j++)
		{
			Z->SetElemsym(i, j, ZM);
			YC->SetElemsym(i, j, YM);
		}
	}
	SymComponentsChanged = false;

    // values in ohms per unit length
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLineObj::CalcFltRate()
{

  // inherited;

  // Assume Faultrate specified in same units as length
	BranchFltRate = FaultRate * PctPerm * 0.01 * Len;
}

void TLineObj::CalcYPrim(int ActorID)
{
	// Added a few definitions for long-line correction frequency-wise
	complex Zs, Zm, Ys, Ym, Ztemp;
	double Yc1, Yc0, R1_h, X1_h, C1_h, G1_h, R0_h, X0_h, C0_h, G0_h;

	complex			Value = CZero;
	pComplexArray	ZinvValues = nullptr;
	pComplexArray	Zvalues = nullptr;
	pComplexArray	YValues = nullptr;
	double FreqMultiplier = 1.0;
	double XgMod = 0.0;
	double LengthMultiplier = 1.0;
	int i = 0;
	int j = 0;
	int k = 0;
	int Norder = 0;

	if(SymComponentsChanged)
      /*Try to catch inadvertent user error when they forget to specify C1 and C0 */
      /*Check to see if user has spec'd C1 and C0. If not, adjust default values for new length units*/
	{
		if(!FCapSpecified)
		{
			C1 = C1 / ConvertLineUnits(UNITS_KFT, LengthUnits); // were defined in kft
			C0 = C0 / ConvertLineUnits(UNITS_KFT, LengthUnits);
			FCapSpecified = true;   // so we don't do it again
		}
		RecalcElementData(ActorID);
	}
	ClearYPrim();


    // Build Series YPrim
	/*# with YPrim_Series do */
	{
		auto& with0 = YPrim_Series;

         /*Build Zmatrix*/
		if(GeometrySpecified)
		{
			FMakeZFromGeometry(ActiveCircuit[ActorID]->Solution->get_FFrequency()); // Includes length in proper units
			if(SolutionAbort)
				return;
		}
		else
		{
			if(SpacingSpecified)
			{
				FMakeZFromSpacing(ActiveCircuit[ActorID]->Solution->get_FFrequency()); // Includes length in proper units
				if(SolutionAbort)
					return;
			}
			else
  // Z is from line code or specified in line data
               // In this section Z is assumed in ohms per unit length
			{
				// If positive sequence, long-line correction can be taken into account here
				// It needs to be recalculated for every frequency
				if (SymComponentsModel && ActiveCircuit[ActorID]->LongLineCorrection) // Should only enter here if sequence components model
				{
					// These values are specific for the harmonic being tested (adjust for frequency)
					R1_h = R1;
					X1_h = X1 * FYprimFreq / BaseFrequency; // Adjust for frequency here
					C1_h = C1;
					G1_h = 0.0;  // DoLongLine uses a tiny conductance to avoid skipping case where C1=0
					// Do the same for zero sequence
					R0_h = R0;
					X0_h = X0 * FYprimFreq / BaseFrequency; // Adjust for frequency here
					C0_h = C0;
					G0_h = 0.0;  // DoLongLine uses a tiny conductance to avoid skipping case where C0=0

					// long-line equivalent PI
					// To avoid errors in higher freqs, we shouldn't skip cases with C1=0. Critical to match IEEE 14bus harmonics benchmark.
					// Do long-line correction for tested frequency for +seq mode
					// Use R1_h, X1_h, C1_h, G1_h to correct Y prim
					DoLongLine(FYprimFreq, R1, X1, C1, R1_h, X1_h, C1_h, G1_h);

					if ((Fnphases > 1) && !ActiveCircuit[ActorID]->PositiveSequence)
					{
						// apply long line correction to 0seq mode as well
						DoLongLine(FYprimFreq, R0, X0, C0, R0_h, X0_h, C0_h, G0_h);
					}
					else
					{
						// zero sequence the same as positive sequence
						R0_h = R1_h;
						X0_h = X1_h;
						C0_h = C1_h;
						G0_h = G1_h;
					}

					Ztemp = cmulreal(cmplx(R1_h, X1_h), 2.0);
					Zs = cmulreal(cadd(Ztemp, cmplx(R0_h, X0_h)), ONE_THIRD);
					Zm = cmulreal(csub(cmplx(R0_h, X0_h), cmplx(R1_h, X1_h)), ONE_THIRD);

					Yc1 = TwoPi * FYprimFreq * C1_h;
					Yc0 = TwoPi * FYprimFreq * C0_h;

					Ys = cmulreal(cadd(cmulreal(cmplx(G1_h, Yc1), 2.0), cmplx(G0_h, Yc0)), ONE_THIRD);
					Ym = cmulreal(csub(cmplx(G0_h, Yc0), cmplx(G1_h, Yc1)), ONE_THIRD);

					for (i = 1; i <= Fnphases; i++)
					{
						Z->SetElement(i, i, Zs);
						YC->SetElement(i, i, Ys);
						for (j = 1; j <= i - 1; j++)
						{
							Z->SetElemsym(i, j, Zm);
							YC->SetElemsym(i, j, Ym);
						}
					}

					// Put in Series RL
					Zvalues = Z->GetValuesArrayPtr(Norder);
					ZinvValues = Zinv->GetValuesArrayPtr(Norder);
					// Correct the impedances for length and frequency
					// Rg increases with frequency
					// Xg modified by ln of sqrt(1/f)
					if (Xg != 0.0) 
						XgMod = 0.5 * KXg * log(FreqMultiplier);
					else 
						XgMod = 0.0;

					for (i = 1; i < Norder * Norder; i++)
					{
						// Apply freq multiplier only to XgMod as we have already accounted for freq adjustment above
						ZinvValues[i - 1] = cmplx((Zvalues[i - 1].re + Rg * (FreqMultiplier - 1.0)) * LengthMultiplier, (Zvalues[i - 1].im - XgMod * FreqMultiplier) * LengthMultiplier);
					}
				}
				else
				{
					// Original piece of code is kept here
					int stop = 0;
					LengthMultiplier = Len / FUnitsConvert;   // convert to per unit length
					FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
					FreqMultiplier = FYprimFreq / BaseFrequency;

				   /* Put in Series RL */
					Zvalues = Z->GetValuesArrayPtr(Norder);
					ZinvValues = Zinv->GetValuesArrayPtr(Norder);
				   // Correct the impedances for length and frequency
				   // Rg increases with frequency
				   // Xg modified by ln of sqrt(1/f)
					if(Xg != 0.0)
						XgMod = 0.5 * KXg * log(FreqMultiplier);
					else
						XgMod = 0.0;
					for(int stop = Norder * Norder, i = 1; i <= stop; i++)
					{
						(ZinvValues)[i - 1] = cmplx(((Zvalues)[i - 1].re + Rg * (FreqMultiplier - 1.0)) * LengthMultiplier, ((Zvalues)[i - 1].im - XgMod) * LengthMultiplier * FreqMultiplier);
					}
				}
				Zinv->Invert();  /*Invert Z in place to get values to put in Yprim*/
			}

      /*At this point have Z and Zinv in proper values including length*/
      /*If GIC simulation, convert Zinv back to sym components, R Only */
		}
		if(ActiveCircuit[ActorID]->Solution->get_FFrequency() < 0.51)     // 0.5 Hz is cutoff
			ConvertZinvToPosSeqR();
		if(Zinv->InvertError > 0)
                 /*If error, put in tiny series conductance*/
// TEMc - shut this up for the CDPSM connectivity profile test, or whenever else it gets annoying
		{
			int stop = 0;
			DoErrorMsg("TLineObj.CalcYPrim", String("Matrix Inversion Error for Line \"") + get_Name() + "\"", "Invalid impedance specified. Replaced with tiny conductance.", 183);
			Zinv->Clear();
			for(int stop = Fnphases, i = 1; i <= stop; i++)
			{
				Zinv->SetElement(i, i, cmplx(EPSILON, 0.0));
			}
		}
		else
		{
			int stop = 0;
			for(int stop = Fnphases, i = 1; i <= stop; i++)
			{
           /* Now, Put in Yprim_Series matrix */
				int stop1 = 0;
				for(int stop1 = Fnphases, j = 1; j <= stop1; j++)
				{
					Value = Zinv->GetElement(i, j);
					with0->SetElement(i, j, Value);
					with0->SetElement(i + Fnphases, j + Fnphases, Value);
					Value = cnegate(Value);
					with0->SetElemsym(i, j + Fnphases, Value);
				}
			}
		}
	}   /*With Yprim_series*/
	YPrim->CopyFrom(YPrim_Series);      // Initialize YPrim for series impedances

     // 10/3/2006 moved this to after the copy to Yprim so it doesn't affect normal line model capacitance
        // 3-30-04  ----- Rev 2-4-09 to include both sides of line
        // Increase diagonal elements of both sides of line so that we will avoid isolated bus problem
        // add equivalent of 10 kvar capacitive at 345 kV
	/*# with YPrim_Series do */
	{
		auto with1 = YPrim_Series;
		int stop = 0;
		for(int stop = Yorder, i = 1; i <= stop; i++)
		{
			with1->AddElement(i, i, CAP_EPSILON);
		}
	}

     // Now Build the Shunt admittances and add into YPrim
	if(ActiveCircuit[ActorID]->Solution->get_FFrequency() > 0.51)
		/*# with YPrim_Shunt do */
		{
			auto with2 = YPrim_Shunt;   // Skip Capacitance for GIC
			

         /*Put half the Shunt Capacitive Admittance at each end*/
			YValues = YC->GetValuesArrayPtr(Norder);
			if(GeometrySpecified || SpacingSpecified)

            /*Values are already compensated for length and frequency*/
			{
				int stop = 0;
				k = 0;
				for(int stop = Fnphases, j = 1; j <= stop; j++)
				{
					int stop1 = 0;
					for(int stop1 = Fnphases, i = 1; i <= stop1; i++)
					{
						++k;    // Assume matrix in col order (1,1  2,1  3,1 ...)
						Value = cdivreal((YValues)[k - 1], 2.0);  // half at each end ...
						with2->AddElement(i, j, Value);
						with2->AddElement(i + Fnphases, j + Fnphases, Value);
					}
				}
			}
			else

             /*Regular line model - values computed per unit length at base frequency*/
			{
				int stop = 0;
				k = 0;
				for(int stop = Fnphases, j = 1; j <= stop; j++)
				{
					int stop1 = 0;
					for(int stop1 = Fnphases, i = 1; i <= stop1; i++)
					{
						++k;    // Assume matrix in col order (1,1  2,1  3,1 ...)
						if (SymComponentsModel && ActiveCircuit[ActorID]->LongLineCorrection) // Should only enter here if sequence components model
						{
							// If we enter here, frequency adjustment has already been applied above during Z and Yc recalculation (and also affected by long-line correction)
							Value = cmplx(YValues[k - 1].re / 2.0, YValues[k - 1].im * LengthMultiplier / 2.0);
						}
						else
						{
							Value = cmplx(0.0, YValues[k - 1].im * LengthMultiplier * FreqMultiplier / 2.0);
						}
							
						with2->AddElement(i, j, Value);
						with2->AddElement(i + Fnphases, j + Fnphases, Value);
					}
				}
			}

         /*Now Account for Open Conductors*/
         /*For any conductor that is open, zero out row and column*/
		} /*With YPRIM*/
	YPrim->AddFrom(YPrim_Shunt);
	inherited::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

void TLineObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int j = 0;
	String Rslt;
	double LengthMult = 0.0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[1 - 1]); Write(f, L'='); WriteLn(f, Get_FirstBus()); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[2 - 1]); Write(f, L'='); WriteLn(f, Get_NextBus()); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[3 - 1]); Write(f, L'='); WriteLn(f, CondCode); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[4 - 1]); Write(f, L'='); WriteLn(f, Len, 0, 3); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[5 - 1]); Write(f, L'='); WriteLn(f, Fnphases, 0); }
		if(SymComponentsModel)
			Rslt = Format("%-.7g", R1 / FUnitsConvert);
		else
			Rslt = "----";
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[6 - 1]); Write(f, L'='); WriteLn(f, Rslt); }
		if(SymComponentsModel)
			Rslt = Format("%-.7g", X1 / FUnitsConvert);
		else
			Rslt = "----";
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[7 - 1]); Write(f, L'='); WriteLn(f, Rslt); }
		if(SymComponentsModel)
			Rslt = Format("%-.7g", R0 / FUnitsConvert);
		else
			Rslt = "----";
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[8 - 1]); Write(f, L'='); WriteLn(f, Rslt); }
		if(SymComponentsModel)
			Rslt = Format("%-.7g", X0 / FUnitsConvert);
		else
			Rslt = "----";
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[9 - 1]); Write(f, L'='); WriteLn(f, Rslt); }
		if(SymComponentsModel)
			Rslt = Format("%-.7g", C1 * 1.0E9 / FUnitsConvert);
		else
			Rslt = "----";
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[10 - 1]); Write(f, L'='); WriteLn(f, Rslt); }
		if(SymComponentsModel)
			Rslt = Format("%-.7g", C0 * 1.0E9 / FUnitsConvert);
		else
			Rslt = "----";
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[11 - 1]); Write(f, L'='); WriteLn(f, Rslt); }

     // If GeometrySpecified Or SpacingSpecified then length is embedded in Z and Yc    4-9-2020
		if(GeometrySpecified || SpacingSpecified)
			LengthMult = Len;
		else
			LengthMult = 1.0;
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[12 - 1]); Write(f, L'='); Write(f, L'\"'); }
		for(int stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(int stop1 = Fnphases, j = 1; j <= stop1; j++)
			{
				{ Write(f, (double(Z->GetElement(i, j).re) / LengthMult / FUnitsConvert), 0, 9); Write(f, L' '); }
			}
			Write(f, L'|');
		}
		WriteLn(f, L'\"');
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[13 - 1]); Write(f, L'='); Write(f, L'\"'); }
		for(int stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(int stop1 = Fnphases, j = 1; j <= stop1; j++)
			{
				{ Write(f, (double(Z->GetElement(i, j).im) / LengthMult / FUnitsConvert), 0, 9); Write(f, L' '); }
			}
			Write(f, L'|');
		}
		WriteLn(f, L'\"');
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[14 - 1]); Write(f, L'='); Write(f, L'\"'); }
		for(int stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(int stop1 = Fnphases, j = 1; j <= stop1; j++)
			{
				{ Write(f, (double(YC->GetElement(i, j).im) / TwoPi / BaseFrequency / LengthMult / FUnitsConvert * 1.0E9), 0, 3); Write(f, L' '); }
			}
			Write(f, L'|');
		}
		WriteLn(f, L'\"');
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[15 - 1]); Write(f, L'='); }
		if(IsSwitch)
			WriteLn(f, "true");
		else
			WriteLn(f, "false");

         /*Dump the rest by default*/
		for(int stop = with0->NumProperties, i = 16; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
}


/************ Placeholder for Line module No Load Loss procedure **********/

void TLineObj::GetLosses(complex& TotalLosses, complex& LoadLosses, complex& NoLoadLosses, int ActorID)
{


  /*For Now, we'll just do the default behavior until we implement shunt losses*/
	inherited::GetLosses(TotalLosses, LoadLosses, NoLoadLosses, ActorID);
}

String TLineObj::GetPropertyValue(int Index)
{
	String result;
	int k = 0;
	int i = 0;
	int j = 0;
	double Factor = 0.0;
	String TempStr;
	switch(Index)
	{
		case 12: case 13: case 14:
		result = "[";
		break;
		default:
		result = "";
		break;
	}
       /*Report Impedance values in ohms per unit length of present length units*/
	switch(Index)
	{
		case 	1:
		result = GetBus(1);
		break;
		case 	2:
		result = GetBus(2);
		break;
		case	3: 
		if (FLineCodeSpecified) result = CondCode; else result = "";
		break;
		case 	4:
		result = Format("%-.7g", Len);
		break;
		case 	5:
		result = Format("%d", Fnphases);
		break;
		case 	6:
		if(SymComponentsModel)
			result = Format("%-.7g", R1 / FUnitsConvert);
		else
			result = "----";
		break;
		case 	7:
		if(SymComponentsModel)
			result = Format("%-.7g", X1 / FUnitsConvert);
		else
			result = "----";
		break;
		case 	8:
		if(SymComponentsModel)
			result = Format("%-.7g", R0 / FUnitsConvert);
		else
			result = "----";
		break;
		case 	9:
		if(SymComponentsModel)
			result = Format("%-.7g", X0 / FUnitsConvert);
		else
			result = "----";
		break;
		case 	10:
		if(SymComponentsModel)
			result = Format("%-.7g", C1 * 1.0E9 / FUnitsConvert);
		else
			result = "----";
		break;
		case 	11:
		if(SymComponentsModel)
			result = Format("%-.7g", C0 * 1.0E9 / FUnitsConvert);
		else
			result = "----";
		break;
		case 	12:
		for(int stop = Fnconds, i = 1; i <= stop; i++)
		{   // R matrix
			int stop1 = 0;
			for(int stop1 = i, j = 1; j <= stop1; j++)
			{  // report in per unit Length in length units
				if(GeometrySpecified || SpacingSpecified)
					result = result + Format("%-.7g", double(Z->GetElement(i, j).re) / Len) + " ";
				else
					result = result + Format("%-.7g", double(Z->GetElement(i, j).re) / FUnitsConvert) + " ";
			}
			if(i < Fnconds)
				result = result + "|";
		}
		break;
		case 	13:
		for(int stop = Fnconds, i = 1; i <= stop; i++)
		{      // X matrix
			int stop1 = 0;
			for(int stop1 = i, j = 1; j <= stop1; j++)
			{
				if(GeometrySpecified || SpacingSpecified)
					result = result + Format("%-.7g", double(Z->GetElement(i, j).im) / Len) + " ";
				else
					result = result + Format("%-.7g", double(Z->GetElement(i, j).im) / FUnitsConvert) + " ";
			}
			if(i < Fnconds)
				result = result + "|";
		}
		break;  // CMatrix  nf
		case 	14:
		{
			int stop = 0;
			Factor = TwoPi * BaseFrequency * 1.0e-9;
			for(int stop = Fnconds, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = i, j = 1; j <= stop1; j++)
				{
					if(GeometrySpecified || SpacingSpecified)
						result = result + Format("%-.7g", double(YC->GetElement(i, j).im) / Factor / Len) + " ";
					else
						result = result
	           + Format("%-.7g", double(YC->GetElement(i, j).im) / Factor / FUnitsConvert)
	           + " ";
				}
				if(i < Fnconds)
					result = result + "|";
			}
		}
		break;
		case 	15:
		if(IsSwitch)
			result = "True";
		else
			result = "False";
		break;
		case 	16:
		result = Format("%-g", Rg);
		break;
		case 	17:
		result = Format("%-g", Xg);
		break;
		case 	18:
		result = Format("%-g", rho);
		break;
		case	19:
		if (GeometrySpecified) result = GeometryCode; else result = "";
		break;
		case 	20:
		result = LineUnitsStr(LengthUnits);
		break;
		case 	23:
		result = GetEarthModel(FEarthModel);
		break;
		case 	26:
		if(SymComponentsModel)
			result = Format("%.7g", TwoPi * BaseFrequency * C1 * 1.0e6);
		else
			result = "----";
		break;
		case 	27:
		if(SymComponentsModel)
			result = Format("%.7g", TwoPi * BaseFrequency * C0 * 1.0e6);
		else
			result = "----";
		break;
		case 	28:
		result = IntToStr(NumAmpRatings);
		break;
		case 	29:
		{
			int stop = 0;
			TempStr = "[";
			for(int stop = NumAmpRatings, k = 1; k <= stop; k++)
			{
				TempStr = TempStr + FloatToStrF(AmpRatings[k - 1], ffGeneral, 8, 4) + ",";
			}
			TempStr = TempStr + "]";
			result = TempStr;
		}
		break;
		case 	30:
		result = LineTypeList.Get(FLineType);
		break;

           // Intercept FaultRate, PctPerm, and HourstoRepair
		case 	33:
		result = Format("%-g", FaultRate);
		break;
		case 	34:
		result = Format("%-g", PctPerm);
		break;
		case 	35:
		result = Format("%-g", HrsToRepair);
		break;
		case 	36:
		result = Format("%-g", BaseFrequency);
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	switch(Index)
	{
		case 12: case 13: case 14:
		result = result + "]";
		break;
		default:
		  ;
		break;
	}
	return result;
}

/* Only consider 3-phase branches with Pos seq >> Neg seq
  Otherwise, we don't know whether it is a 3-phase line or just a line with 3 phases
*/

void TLineObj::GetSeqLosses(complex& PosSeqLosses, complex& NegSeqLosses, complex& ZeroSeqLosses, int ActorID)
{
	int i = 0;
	int j = 0;
	int k = 0;
	int l = 0;
	complex Vph[3]	= { CZero, CZero, CZero };
	complex V012[3] = { CZero, CZero, CZero };
	complex I012[3] = { CZero, CZero, CZero };
	PosSeqLosses = CZero;
	NegSeqLosses = CZero;
	ZeroSeqLosses = CZero;

    /*Method: sum seq powers going into each terminal
    */
	if(Fnphases == 3)   /*3-phase lines only*/
	{
		int stop = 0;
		ComputeIterminal(ActorID);
		for(int stop = 2, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			k = (i - 1) * Fnphases + 1;
			for(int stop1 = 2, j = 0; j <= stop1; j++)
			{
				if(!ADiakoptics || (ActorID == 1))
					Vph[j] = ActiveCircuit[ActorID]->Solution->NodeV[(NodeRef)[k + j - 1]];
				else
					Vph[j] = ActiveCircuit[ActorID]->Solution->VoltInActor1((NodeRef)[k + j - 1]);
			}
			Phase2SymComp(&Vph[0], &V012[0]);
			Phase2SymComp((pComplexArray) &(Iterminal)[k - 1], &I012[0]);
			caccum(PosSeqLosses, cmul(V012[1], conjg(I012[1])));
			caccum(NegSeqLosses, cmul(V012[2], conjg(I012[2]))); // accumulate both line modes
			caccum(ZeroSeqLosses, cmul(V012[0], conjg(I012[0])));
		}
		cmulrealaccum(PosSeqLosses, 3.0);
		cmulrealaccum(NegSeqLosses, 3.0);
		cmulrealaccum(ZeroSeqLosses, 3.0);
	}
}

void TLineObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,GetBus(2));
	Set_PropertyValue(3,"");
	Set_PropertyValue(4,"1.0");  // '5.28'; Changed 2/17/00
	Set_PropertyValue(5,"3");
	Set_PropertyValue(6,".058");
	Set_PropertyValue(7,".1206");
	Set_PropertyValue(8,".1784");
	Set_PropertyValue(9,".4047");
	Set_PropertyValue(10,"3.4");
	Set_PropertyValue(11,"1.6");
	Set_PropertyValue(12,"");
	Set_PropertyValue(13,"");
	Set_PropertyValue(14,"");
	Set_PropertyValue(15,"false");
	Set_PropertyValue(16,"0.01805");
	Set_PropertyValue(17,"0.155081");
	Set_PropertyValue(18,"100");
	Set_PropertyValue(19,"");
	Set_PropertyValue(20,"NONE");
	Set_PropertyValue(21,"");
	Set_PropertyValue(22,"");
	Set_PropertyValue(23,GetEarthModel(SIMPLECARSON));
	Set_PropertyValue(24,"");
	Set_PropertyValue(25,"");
	Set_PropertyValue(26,"1.2818"); // B1  microS
	Set_PropertyValue(27,"0.60319"); // B0  microS
	Set_PropertyValue(28,"1");      // 1 Season
	Set_PropertyValue(29,"[400]");  // 1 Season
	Set_PropertyValue(30,"OH"); // Overhead line default
	inherited::InitPropertyValues(NumPropsThisClass);

      // Override Inherited properties  just in case
	Set_PropertyValue(NumPropsThisClass + 1,"400");  //Normamps
	Set_PropertyValue(NumPropsThisClass + 2,"600");  //emergamps
	Set_PropertyValue(NumPropsThisClass + 3,"0.1");  //Fault rate
	Set_PropertyValue(NumPropsThisClass + 4,"20");   // Pct Perm
	Set_PropertyValue(NumPropsThisClass + 5,"3");    // Hrs to repair
	ClearPropSeqArray();
}

void TLineObj::MakePosSequence(int ActorID)
{
	String s;
	double C1_new = 0.0;
	double CS = 0.0;
	double CM = 0.0;
	double LengthMult = 0.0;
	complex Z1 = {};
	complex Zs = {};
	complex ZM = {};
	int i = 0;
	int j = 0;
// set to single phase and make sure R1, X1, C1 set.
// If already single phase, let alone
	if(Fnphases > 1)
    // Kill certain propertyvalue elements to get a cleaner looking save
	{
		int stop = 0;
		(PrpSequence)[3 - 1] = 0;
		for(int stop = 14, i = 6; i <= stop; i++)
		{
			(PrpSequence)[i - 1] = 0;
		}

    // If GeometrySpecified Or SpacingSpecified then length is embedded in Z and Yc    4-9-2020
		if(GeometrySpecified || SpacingSpecified)
			LengthMult = Len;
		else
			LengthMult = 1.0;
		if(IsSwitch)
		{
			s = " R1=1 X1=1 C1=1.1 Phases=1 Len=0.001";
		}
		else
		{
			if(SymComponentsModel)  // keep the same Z1 and C1
			{
				Z1.re = R1;
				Z1.im = X1;
				C1_new = C1 * 1.0E9; // convert to nF
			}
			else
 // matrix was input directly, or built from physical data
        // average the diagonal and off-dialgonal elements
			{
				int stop = 0;
				Zs = CZero;
				for(int stop = Fnphases, i = 1; i <= stop; i++)
				{
					caccum(Zs, Z->GetElement(i, i));
				}
				Zs = cdivreal(Zs, (Fnphases * LengthMult));
				ZM = CZero;
				for(int stop = Fnphases - 1, i = 1; i <= stop; i++)
				{     // Corrected 6-21-04
					int stop1 = 0;
					for(int stop1 = Fnphases, j = i + 1; j <= stop1; j++)
					{
						caccum(ZM, Z->GetElement(i, j));
					}
				}
				ZM = cdivreal(ZM, (LengthMult * Fnphases * (Fnphases - 1.0) / 2.0));
				Z1 = csub(Zs, ZM);

        // Do same for Capacitances
				CS = 0.0;
				for(int stop = Fnphases, i = 1; i <= stop; i++)
				{
					CS = CS + YC->GetElement(i, i).im;
				}
				CM = 0.0;
				for(int stop = Fnphases - 1, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					for(int stop1 = Fnphases, j = i + 1; j <= stop1; j++)
					{    // corrected 4-9-2020
						CM = CM + YC->GetElement(i, j).im;
					}
				}
				C1_new = (CS - CM) / TwoPi / BaseFrequency / (LengthMult * Fnphases * (Fnphases - 1.0) / 2.0) * 1.0E9; // nanofarads

        // compensate for length units
				Z1 = cdivreal(Z1, FUnitsConvert);
				C1_new = C1_new / FUnitsConvert;
			}
			s = Format(" R1=%-.5g  %-.5g  C1=%-.5g Phases=1", Z1.re, Z1.im, C1_new);
		}
    // Conductor Current Ratings
		s = s
	           + Format(" Normamps=%-.5g  %-.5g", NormAmps, EmergAmps);
    // Repeat the Length Units to compensate for unexpected reset
		s = s + " Units=" + LineUnitsStr(LengthUnits);
		Parser[ActorID]->SetCmdString(s);
		Edit(ActorID);
	}
	inherited::MakePosSequence(ActorID);
}

// Cleaned up and corrected 3-17-15

// Merge this line with another line and disble the other line.

bool TLineObj::MergeWith(TLineObj* OtherLine, bool SERIES)
{
	bool				result = false;
	pComplexArray		Values1 = nullptr;
	pComplexArray		Values2 = nullptr;
	int					Order1 = 0,
						Order2 = 0,
						i = 0,
						j = 0,
						Common1 = 0,
						Common2 = 0,
						TestBusNum = 0,
						LenUnitsSaved = 0;
	double				TotalLen = 0.0,
						wnano = 0.0,
						LenSelf = 0.0,
						LenOther = 0.0;
	String				s = "",
						NewName = "";
	complex				NewZ = CZero;


	if(OtherLine != nullptr)
	{
		if(Fnphases != OtherLine->Fnphases)
			return result;  // Can't merge
		LenUnitsSaved = LengthUnits;
		Set_YprimInvalid(ActiveActor,true);

      // Redefine property values to make it appear that line was defined this way originally using matrices
		if(SERIES)
			TotalLen = Len + OtherLine->Len * ConvertLineUnits(OtherLine->LengthUnits, LengthUnits);
		else
			TotalLen = 1.0;
		if(SERIES)
           /* redefine the bus connections*/

           // Find the bus in common between the two lines
		{
			Common1 = 0;
			Common2 = 0;
			i = 1;
			while((Common1 == 0) && (i <= 2))
			{
				int stop = 0;
				TestBusNum = (ActiveCircuit[ActiveActor]->MapNodeToBus)[NodeRef[(i - 1) * Fnconds] - 1].BusRef;
				for(int stop = 2, j = 1; j <= stop; j++)
				{
					if((ActiveCircuit[ActiveActor]->MapNodeToBus)[OtherLine->NodeRef[(j - 1) * OtherLine->Get_NConds()] - 1].BusRef == TestBusNum)
					{
						Common1 = i;
						Common2 = j;
						break;
					}
				}
				++i;
			}
			if(Common1 == 0)
				return result;  // There's been an error; didn't find anything in common
				
           /*Redefine the bus connections, eliminating the common bus*/
			switch(Common1)
			{
				case 	1:
				switch(Common2)
				{
					case 	1:
					s = String("Bus1=\"") + OtherLine->GetBus(2) + "\"";
					break;
					case 	2:
					s = String("Bus1=\"") + OtherLine->GetBus(1) + "\"";
					break;
					default:
					  ;
					break;
				}
				break;
				case 	2:
				switch(Common2)
				{
					case 	1:
					s = String("Bus2=\"") + OtherLine->GetBus(2) + "\"";
					break;
					case 	2:
					s = String("Bus2=\"") + OtherLine->GetBus(1) + "\"";
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
			Parser[ActiveActor]->SetCmdString(s);
			Edit(ActiveActor);
		} /*If Series*/

      /*Rename the line*/
		if(SERIES) //StripExtension(GetBus(1)) + '~'  + StripExtension(GetBus(2))
			NewName = OtherLine->get_Name() + "~" + get_Name();
		else
			NewName = StripExtension(GetBus(1)) + "||" + StripExtension(GetBus(2));

       /*Update ControlElement Connections to This Line*/
		UpdateControlElements(String("line.") + NewName, String("line.") + get_Name());
		UpdateControlElements(String("line.") + NewName, String("line.") + OtherLine->get_Name());
		Set_Name(NewName);
		if(SERIES)
			IsSwitch = false; // not allowed on series merge.
			
       /*Now Do the impedances*/
		LenSelf = Len / FUnitsConvert;  // in units of the R X Data
		LenOther = OtherLine->Len / OtherLine->FUnitsConvert;

       /* If both lines are Symmmetrical Components, just merge the R1..C0 values*/
       /* This will catch many 3-phase lines since this is a common way to define lines*/
		if(SymComponentsModel && OtherLine->SymComponentsModel && (Get_NPhases() == 3))   /*------------------------- Sym Component Model ----------------------------------*/
		{
			if(SERIES)
			{
				s = String(" R1=") + Format("%-g", (R1 * LenSelf + OtherLine->R1 * LenOther) / TotalLen);     // Ohms per unit length of this line length units
				s = s + Format(" %-g", (X1 * LenSelf + OtherLine->X1 * LenOther) / TotalLen);
				s = s + Format(" %-g", (R0 * LenSelf + OtherLine->R0 * LenOther) / TotalLen);
				s = s + Format(" %-g", (X0 * LenSelf + OtherLine->X0 * LenOther) / TotalLen);
				s = s + Format(" %-g", (C1 * LenSelf + OtherLine->C1 * LenOther) / TotalLen * 1.0E9);
				s = s + Format(" %-g", (C0 * LenSelf + OtherLine->C0 * LenOther) / TotalLen * 1.0E9);
			}
			else
   /*parallel*/
			{
				if(IsSwitch)   /*Leave as is if switch; just dummy z anyway*/
					s = "";
				else
				{
					if(OtherLine->IsSwitch)   /*This will take care of setting Z's*/
						s = " switch=yes";
					else

/********** Will This work with Length multiplier?  did it ever work? **************************/
					{
						NewZ = ParallelZ(cmplx(R1 * Len, X1 * Len), cmplx(OtherLine->R1 * OtherLine->Len, OtherLine->X1 * OtherLine->Len));
						s = String(" R1=") + Format("%-g %-g ", NewZ.re, NewZ.im);
						NewZ = ParallelZ(cmplx(R0 * Len, X0 * Len), cmplx(OtherLine->R0 * OtherLine->Len, OtherLine->X0 * OtherLine->Len));
						s = String(" R0=") + Format("%-g %-g ", NewZ.re, NewZ.im);
						s = s + Format(" %-g", (C1 * Len + OtherLine->C1 * OtherLine->Len) / TotalLen * 1.0E9);
						s = s + Format(" %-g", (C0 * Len + OtherLine->C0 * OtherLine->Len) / TotalLen * 1.0E9);
					}
				}
			}
			Parser[ActiveActor]->SetCmdString(s);   // This reset the length units
			Edit(ActiveActor);

          // update length units
			Parser[ActiveActor]->SetCmdString(Format(" Length=%-g  Units=", TotalLen) + LineUnitsStr(LenUnitsSaved));
			Edit(ActiveActor);

          // Update symmetrical Components computation
          // (Only time this function is called is for sym comp update -- computes Z and Yc)
			RecalcElementData(ActiveActor);
		}
		else
		{
			if(!SERIES)  /*------------- Matrix Model for anything other than Symmetrical Components -------------------------*/ /*We'll assume lines are equal for now*/
				TotalLen = Len / 2.0;
			else
  /*Matrices were defined*/

               // Merge Z matrices
			{
				int stop = 0;
				Values1 = Z->GetValuesArrayPtr(Order1);
				Values2 = OtherLine->Z->GetValuesArrayPtr(Order2);
				if(Order1 != Order2)
					return result;  // OOps.  Lines not same size for some reason

               // If Geometry specified, length is already included; so reset to 1.0
				if(GeometrySpecified || SpacingSpecified)
					LenSelf = 1.0;
				if(OtherLine->GeometrySpecified || OtherLine->SpacingSpecified)
					LenOther = 1.0;

               // Z <= (Z1 + Z2 )/TotalLen   to get equiv ohms per unit length
				for(int stop = Order1 * Order1, i = 1; i <= stop; i++)
				{
					(Values1)[i - 1] = cdivreal(cadd(cmulreal((Values1)[i - 1], LenSelf), cmulreal((Values2)[i - 1], LenOther)), TotalLen);
				}

               // Merge Yc matrices
				Values1 = YC->GetValuesArrayPtr(Order1);
				Values2 = OtherLine->YC->GetValuesArrayPtr(Order2);
				if(Order1 != Order2)
					return result;  // OOps.  Lines not same size for some reason
				for(int stop = Order1 * Order1, i = 1; i <= stop; i++)
				{
					(Values1)[i - 1] = cdivreal(cadd(cmulreal((Values1)[i - 1], LenSelf), cmulreal((Values2)[i - 1], LenOther)), TotalLen);
				}

               /*R Matrix*/
				s = "Rmatrix=[";
				for(int stop = Order1, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					for(int stop1 = i, j = 1; j <= stop1; j++)
					{
						s = s + Format(" %-g", Z->GetElement(i, j).re);
					}
					s = s + " | ";
				}
				s = s + "] Xmatrix=[";
               /*X Matrix*/
				for(int stop = Order1, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					for(int stop1 = i, j = 1; j <= stop1; j++)
					{
						s = s + Format(" %-g", Z->GetElement(i, j).im);
					}
					s = s + " | ";
				}
				s = s + "]";
				Parser[ActiveActor]->SetCmdString(s);
				Edit(ActiveActor);

               /*C Matrix*/
				wnano = TwoPi * BaseFrequency / 1.0E9;
				s = "Cmatrix=[";
				for(int stop = Order1, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					for(int stop1 = i, j = 1; j <= stop1; j++)
					{
						s = s + Format(" %-g", YC->GetElement(i, j).im / wnano );
					}   // convert from mhos to nanofs
					s = s + " | ";
				}
				s = s + "] ";
				Parser[ActiveActor]->SetCmdString(s);
				Edit(ActiveActor);

               // update length units
				Parser[ActiveActor]->SetCmdString(Format(" Length=%-g  Units=", TotalLen) + LineUnitsStr(LenUnitsSaved));
				Edit(ActiveActor);
			}  /*Matrix definition*/
		}
		OtherLine->Set_Enabled(false);  // Disable the Other Line
		result = true;
	}
	else
	DoSimpleMsg("Error in Line Merge: Attempt to merge with invalid (nil) line object found.", 184);
	return result;
}

void TLineObj::UpdateControlElements(const String NewName, const String OldName)
{
	TControlElem* pControlElem = nullptr;
	pControlElem = ((TControlElem*) ActiveCircuit[ActiveActor]->DSSControls.Get_First());
	while(pControlElem != nullptr)
	{
		if(CompareText(OldName, pControlElem->ElementName) == 0)
		{
			Parser[ActiveActor]->SetCmdString(String(" Element=") + NewName);  // Change name of the property
			( (TDSSObject*) pControlElem )->Edit(ActiveActor);
		}
		pControlElem = ((TControlElem*) ActiveCircuit[ActiveActor]->DSSControls.Get_Next());
	}
}

void TLineObj::FetchLineSpacing(const String Code)
{
	if( LineSpacingClass[ActiveActor]->SetActive(Code))
	{
		FLineSpacingObj = (TLineSpacingObj*)LineSpacingClass[ActiveActor]->GetActiveObj();
		FLineCodeSpecified = false;
		KillGeometrySpecified();
		SpacingCode = LowerCase(Code);

      // need to establish Yorder before FMakeZFromSpacing
		Set_NPhases(FLineSpacingObj->get_Fnphases());
		Set_Nconds(Fnphases);  // Force Reallocation of terminal info
		Yorder = Fnconds * Fnterms;
		Set_YprimInvalid(ActiveActor,true);       // Force Rebuild of Y matrix
	}
	else
	DoSimpleMsg(String("Line Spacing object ") + Code + " not found.(LINE." + get_Name() + ")", 181011);
}

void TLineObj::FetchWireList(const String Code)
{
	bool RatingsInc = false;
	int NewNumRat = 0;
	int j = 0;
	int i = 0;
	int iStart = 0;
	TRatingsArray NewRatings;
	int stop = 0;
	if(!ASSIGNED(FLineSpacingObj))
		DoSimpleMsg(String("You must assign the LineSpacing before the Wires Property (LINE.") + get_Name()
	           + ").", 18102);
	if(FPhaseChoice == unknown) // it's an overhead line
	{
		FLineCodeSpecified = false;
		KillGeometrySpecified();
		FWireDataSize = FLineSpacingObj->get_Fnconds();
		FLineWireData.resize(FWireDataSize);
		iStart = 1;
		FPhaseChoice = Overhead;
	}
	else
 // adding bare neutrals to an underground line - TODO what about repeat invocation?
	{
		iStart = FLineSpacingObj->get_Fnphases() + 1;
	}
	AuxParser[ActiveActor]->SetCmdString(Code);
	NewNumRat = 1;
	RatingsInc = false;             // So far we don't know if there are seasonal ratings
	for(int stop = FLineSpacingObj->get_Fnconds(), i = iStart; i <= stop; i++)
	{
		String dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
		WireDataClass[ActiveActor]->Set_Code(AuxParser[ActiveActor]->MakeString_());
		if(ASSIGNED(ActiveConductorDataObj))
		{
			FLineWireData[i - 1] = ActiveConductorDataObj;
			if(FLineWireData[i - 1]->NumAmpRatings > NewNumRat)
			{
				NewRatings = Copy( FLineWireData[i - 1]->AmpRatings  );   // Have to be same type to be assignable
				NewNumRat = sizeof(NewRatings) + 1;
				RatingsInc = true;         // Yes, there are seasonal ratings
			}
			NormAmps = FLineWireData[i - 1]->NormAmps;
			EmergAmps = FLineWireData[i - 1]->EmergAmps;
		}
		else
		DoSimpleMsg(String("Wire \"") + AuxParser[ActiveActor]->MakeString_()
	           + "\" was not defined first (LINE."
	           + get_Name()
	           + ").", 18103);
	}
	if(RatingsInc)
	{
		AmpRatings = Copy(NewRatings);     /***** NewRatings disappears when it goes out of scope*/
		NumAmpRatings = NewNumRat;
	}
	UpdatePDProperties();
}

void TLineObj::FetchCNCableList(const String Code)
{
	int i = 0;
	int stop = 0;
	FLineCodeSpecified = false;
	KillGeometrySpecified();
	if(!ASSIGNED(FLineSpacingObj))
		DoSimpleMsg(String("Must assign the LineSpacing before CN cables.(LINE.") + get_Name()
	           + ")", 18104);
	FPhaseChoice = ConcentricNeutral;
	FLineWireData.resize(FLineSpacingObj->get_Fnconds());
	AuxParser[ActiveActor]->SetCmdString(Code);
	for(int stop = FLineSpacingObj->get_Fnphases(), i = 1; i <= stop; i++)
	{ // fill extra neutrals later
		String dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
		CNDataClass[ActiveActor]->Set_Code(AuxParser[ActiveActor]->MakeString_());
		if(ASSIGNED(ActiveConductorDataObj))
			FLineWireData[i - 1] = ActiveConductorDataObj;
		else
			DoSimpleMsg(String("CN cable ") + AuxParser[ActiveActor]->MakeString_()
	           + " was not defined first.(LINE."
	           + get_Name()
	           + ")", 18105);
	}
}

void TLineObj::FetchTSCableList(const String Code)
{
	int i = 0;
	int stop = 0;
	FLineCodeSpecified = false;
	KillGeometrySpecified();
	if(!ASSIGNED(FLineSpacingObj))
		DoSimpleMsg(String("Must assign the LineSpacing before TS cables.(LINE.") + get_Name()
	           + ")", 18106);
	FPhaseChoice = TapeShield;
	FLineWireData.resize(FLineSpacingObj->get_Fnconds());
	AuxParser[ActiveActor]->SetCmdString(Code);
	for(int stop = FLineSpacingObj->get_Fnphases(), i = 1; i <= stop; i++)
	{ // fill extra neutrals later
		String dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
		TSDataClass[ActiveActor]->Set_Code(AuxParser[ActiveActor]->MakeString_());
		if(ASSIGNED(ActiveConductorDataObj))
			FLineWireData[i - 1] = ActiveConductorDataObj;
		else
			DoSimpleMsg(String("TS cable ") + AuxParser[ActiveActor]->MakeString_()
	           + " was not defined first. (LINE."
	           + get_Name()
	           + ")", 18107);
	}
}

void TLineObj::FetchGeometryCode(const String Code)
{
	int i = 0;
	if(LineGeometryClass == nullptr)
		LineGeometryClass = (TLineGeometry*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("LineGeometry"));
	if(LineGeometryClass->SetActive(Code))
	{
		FLineCodeSpecified = false;  // Cancel this flag
		SpacingSpecified = false;
		FLineGeometryObj = ((TLineGeometryObj*) LineGeometryClass->GetActiveObj());
		FZFrequency = -1.0;  // Init to signify not computed
		GeometryCode = LowerCase(Code);
		if(FrhoSpecified)
			FLineGeometryObj->Set_RhoEarth(rho);
		if (!FRatingsSpecified)
		{
			NormAmps = FLineGeometryObj->NormAmps;
			EmergAmps = FLineGeometryObj->EmergAmps;
			UpdatePDProperties();
		}
		Set_NPhases(FLineGeometryObj->get_Nconds());
		Set_Nconds(Fnphases);  // Force Reallocation of terminal info
		Yorder = Fnconds * Fnterms;
		Set_YprimInvalid(ActiveActor,true);       // Force Rebuild of Y matrix
		NumAmpRatings = FLineGeometryObj->NumAmpRatings;
		AmpRatings = Copy( FLineGeometryObj->AmpRatings );
		FLineType = FLineGeometryObj->FLineType;
	}
	else
	DoSimpleMsg(String("Line Geometry Object:") + Code
	           + " not found. (LINE."
	           + get_Name()
	           + ")", 18108);
} // make new Z, Zinv, Yc, etc

void TLineObj::FMakeZFromGeometry(double f)
{
	if(f == FZFrequency)
		return;  // Already Done for this frequency, no need to do anything
	if(ASSIGNED(FLineGeometryObj))
         /*This will make a New Z; Throw away present allocations*/
	{
		if(ASSIGNED(Z))
		{
			delete Z;
			Z = nullptr;
		}
		if(ASSIGNED(Zinv))
		{
			delete Zinv;
			Zinv = nullptr;
		}
		if(ASSIGNED(YC))
		{
			delete YC;
			YC = nullptr;
		}
		ActiveEarthModel[ActiveActor] = FEarthModel;
		Z = FLineGeometryObj->Get_Zmatrix(f,Len,LengthUnits);
		YC = FLineGeometryObj->Get_YCmatrix(f,Len,LengthUnits);
          /*Init Zinv*/
		if(ASSIGNED(Z))
		{
			Zinv = new TcMatrix(Z->get_Norder());  // Either no. phases or no. conductors
			Zinv->CopyFrom(Z);
			Zinv->Invert();  /*Invert Z in place to get values to put in Yprim*/
		}

          // Z and YC are actual total impedance for the line;
		FZFrequency = f;
	}
} // make new Z, Zinv, Yc, etc

void TLineObj::FMakeZFromSpacing(double f)
{
	TLineGeometryObj* pGeo = nullptr;
	if(f == FZFrequency)
		return;  // Already Done for this frequency, no need to do anything
	if(ASSIGNED(Z))
	{
		delete Z;
		Z = nullptr;
	}
	if(ASSIGNED(Zinv))
	{
		delete Zinv;
		Zinv = nullptr;
	}
	if(ASSIGNED(YC))
	{
		delete YC;
		YC = nullptr;
	}

  // make a temporary LineGeometry to calculate line constants
	if(LineGeometryClass == nullptr)
		LineGeometryClass = (TLineGeometry*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("LineGeometry"));
	pGeo = new TLineGeometryObj(LineGeometryClass, get_Name());
	pGeo->LoadSpacingAndWires(FLineSpacingObj, FLineWireData); // this sets OH, CN, or TS
	if(FrhoSpecified)
		pGeo->Set_RhoEarth(rho);
	NormAmps = pGeo->NormAmps;
	EmergAmps = pGeo->EmergAmps;
	UpdatePDProperties();

	ActiveEarthModel[ActiveActor] = FEarthModel;

	Z = pGeo->Get_Zmatrix(f,Len,LengthUnits);
	YC = pGeo->Get_YCmatrix(f,Len,LengthUnits);
	if(ASSIGNED(Z))
	{
		Zinv = new TcMatrix(Z->get_Norder());  // Either no. phases or no. conductors
		Zinv->CopyFrom(Z);
		Zinv->Invert();  /*Invert Z in place to get values to put in Yprim*/
	}
	delete pGeo;
	FZFrequency = f;
}

void TLineObj::KillGeometrySpecified()
{

/*Indicate No Line Geometry specification if this is called*/
	if(GeometrySpecified)
	{
		FLineGeometryObj = nullptr;
		FZFrequency = -1.0;
		GeometrySpecified = false;
	}
}

void TLineObj::KillSpacingSpecified()
{
	if(SpacingSpecified)
	{
		FLineSpacingObj = nullptr;
		FLineWireData.clear();
		FPhaseChoice = unknown;
		FZFrequency = -1.0;
		SpacingSpecified = false;
	}
}

void TLineObj::ClearYPrim()
{

 // Line Object needs both Series and Shunt YPrims built
	if(Get_YprimInvalid(ActiveActor,0)) // Reallocate YPrim if something has invalidated old allocation
	{
		if(YPrim_Series != nullptr)
			delete YPrim_Series;
		if(YPrim_Shunt != nullptr)
			delete YPrim_Shunt;
		if(YPrim != nullptr)
			delete YPrim;
		YPrim_Series = new TcMatrix(Yorder);
		YPrim_Shunt = new TcMatrix(Yorder);
		YPrim = new TcMatrix(Yorder);
	}
	else
	{
		YPrim_Series->Clear();   // zero out YPrim Series
		YPrim_Shunt->Clear();    // zero out YPrim Shunt
		YPrim->Clear();          // zero out YPrim
	}
}

// For GIC Analysis, use only real part of Z

void TLineObj::ConvertZinvToPosSeqR()
{
	complex Z1 = {};
	complex Zs = {};
	complex ZM = {};
	int i = 0;
	int j = 0;

// re-invert Zinv
	int stop = 0;
	Zinv->Invert();
// Now Zinv is back to Z with length included

    // average the diagonal and off-dialgonal elements
	Zs = Zinv->AvgDiagonal();
	ZM = Zinv->AvgOffDiagonal();
	Z1 = csub(Zs, ZM);
	Z1.im = 0.0;  // ignore X part
	Zinv->Clear();
	for(int stop = Zinv->get_Norder(), i = 1; i <= stop; i++)
	{
		Zinv->SetElement(i, i, Z1);
	}   // Set Diagonals
	Zinv->Invert();  // back to zinv for inserting in Yprim
}
/*If specify the impedances always assume the length units match*/

void TLineObj::ResetLengthUnits()
{
	FUnitsConvert = 1.0;
	LengthUnits = UNITS_NONE;  // but do not erase FUserLengthUnits, in case of CIM export
}

int TLineObj::NumConductorData()
{
	int result = 0;
	result = 0;
	if (!FLineWireData.empty())
		result = FLineSpacingObj->get_Fnconds();
	if(ASSIGNED(FLineGeometryObj))
		result = FLineGeometryObj->get_Fnconds();
	return result;
}

TConductorDataObj* TLineObj::FetchConductorData(int i)
{
	TConductorDataObj* result = nullptr;
	result = nullptr;
	if (!FLineWireData.empty())
	{
		if(i <= FLineSpacingObj->get_Fnconds())
			result = FLineWireData[i - 1];
	}
	else
	{
		if(ASSIGNED(FLineGeometryObj))
		{
			if(i <= FLineGeometryObj->get_Fnconds())
				result = FLineGeometryObj->Get_ConductorData(i);
		}
	}
	return result;
}


void Line_initialization()
{
	CAP_EPSILON = cmplx(0.0, 4.2E-8);  // 5 kvar of capacitive reactance at 345 kV to avoid open line problem
}

		class 		Line_unit
		{
		public:
		Line_unit()
		{
			//AssertSystemInitialization();
			Line_initialization();
		}
		};
		Line_unit _Line_unit;

}  // namespace Line

