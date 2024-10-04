
#pragma hdrstop

#include "GICLine.h"

#include "ParserDel.h"
#include "Circuit.h"
#include "MyDSSClassDefs.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Utilities.h"
#include "Command.h"

using namespace std;
using namespace Circuit;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace MyDSSClassDefs;
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace Utilities;

namespace GICLine
{

TGICLineObj::TGICLineObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TGICLineObj::TGICLineObj(String ClassName) : inherited(ClassName) {}
TGICLineObj::TGICLineObj() {}


TGICLineObj* ActiveGICLineObj = nullptr;
const int NumPropsThisClass = 15;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TGICLine::TGICLine()
{
	;
	Class_Name = "GICLine";
	DSSClassType = GIC_Line + PC_ELEMENT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TGICLine::~TGICLine()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGICLine::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1- 1 ] = "bus1";
	PropertyName[2- 1 ] = "bus2";
	PropertyName[3- 1 ] = "Volts";
	PropertyName[4- 1 ] = "Angle";
	PropertyName[5- 1 ] = "frequency";
	PropertyName[6- 1 ] = "phases";
	PropertyName[7- 1 ] = L'R';
	PropertyName[8- 1 ] = L'X';
	PropertyName[9- 1 ] = L'C';
  //   PropertyName[10] := 'ScanType';
  //   PropertyName[11] := 'Sequence';
	PropertyName[10- 1 ] = "EN";
	PropertyName[11- 1 ] = "EE";
	PropertyName[12- 1 ] = "Lat1";
	PropertyName[13- 1 ] = "Lon1";
	PropertyName[14- 1 ] = "Lat2";
	PropertyName[15- 1 ] = "Lon2";

     // define Property help values
	PropertyHelp[1- 1 ] = String("Name of bus to which the main terminal (1) is connected.") + CRLF
	           + "bus1=busname"
	           + CRLF
	           + "bus1=busname.1.2.3";
	PropertyHelp[2- 1 ] = String("Name of bus to which 2nd terminal is connected.") + CRLF
	           + "bus2=busname"
	           + CRLF
	           + "bus2=busname.1.2.3"
	           + CRLF
	           + CRLF
	           + "No Default; must be specified.";
	PropertyHelp[3- 1 ] = String("Voltage magnitude, in volts, of the GIC voltage induced across this line. " "When spedified, voltage source is assumed defined by Voltage and Angle properties. ") + CRLF
	           + CRLF
	           + "Specify this value"
	           + CRLF
	           + CRLF
	           + "OR"
	           + CRLF
	           + CRLF
	           + "EN, EE, lat1, lon1, lat2, lon2. "
	           + CRLF
	           + CRLF
	           + "Not both!!  Last one entered will take precedence. "
	           + "Assumed identical in each phase of the Line object.";
	PropertyHelp[4- 1 ] = "Phase angle in degrees of first phase. Default=0.0.  See Voltage property";
	PropertyHelp[5- 1 ] = "Source frequency.  Defaults to 0.1 Hz.";
	PropertyHelp[6- 1 ] = "Number of phases.  Defaults to 3.";
	PropertyHelp[7- 1 ] = "Resistance of line, ohms of impedance in series with GIC voltage source. ";
	PropertyHelp[8- 1 ] = "Reactance at base frequency, ohms. Default = 0.0. This value is generally not important for GIC studies but may be used if desired.";
	PropertyHelp[9- 1 ] = "Value of line blocking capacitance in microfarads. Default = 0.0, implying that there is no line blocking capacitor.";
 //    PropertyHelp[10] := '{pos | zero* | none} Maintain specified sequence for harmonic solution. Default is ZERO sequence. '+
 //                        'Otherwise, angle between phases rotates with harmonic.';
 //    PropertyHelp[11] := '{pos | neg | zero*} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. '+
 //                        'Default is ZERO sequence. ';
	PropertyHelp[10- 1 ] = "Northward Electric field (V/km). If specified, Voltage and Angle are computed from EN, EE, lat and lon values.";
	PropertyHelp[11- 1 ] = "Eastward Electric field (V/km).  If specified, Voltage and Angle are computed from EN, EE, lat and lon values.";
	PropertyHelp[12- 1 ] = "Latitude of Bus1 (degrees)";
	PropertyHelp[13- 1 ] = "Longitude of Bus1 (degrees)";
	PropertyHelp[14- 1 ] = "Latitude of Bus2 (degrees)";
	PropertyHelp[15- 1 ] = "Longitude of Bus2 (degrees)";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // Override help string
	PropertyHelp[NumPropsThisClass + 1] = "Inherited Property for all PCElements. Name of harmonic spectrum for this source.  Default is \"defaultvsource\", which is defined when the DSS starts.";
	PropertyHelp[NumPropsThisClass + 2] = "Inherited Property for all PCElements. Base frequency for specification of reactance value.";
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGICLine::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new voltage source and add it to GICLine class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TGICLineObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGICLine::GICLineSetBus1(const String s)
{
	String S2;
	int dotpos = 0;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0
	/*# with ActiveGICLineObj do */
	{
		auto with0 = ActiveGICLineObj;
		with0->SetBus(1, s);

     // Strip node designations from S
		dotpos = Pos(".", s);
		if(dotpos > 0)
			S2 = s.substr(0, dotpos - 1);
		else
			S2 = s.substr(0, s.size());  // copy up to Dot
		with0->SetBus(2, S2);    // default setting for Bus2  is same as Bus1
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGICLine::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
  // continue parsing with contents of Parser
	ActiveGICLineObj = (TGICLineObj*)  ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveGICLineObj);
	result = 0;
	/*# with ActiveGICLineObj do */
	{
		auto with0 = ActiveGICLineObj;
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
				GICLineSetBus1(Param);
				break;   // special handling of Bus 1
				case 	2:
				with0->SetBus(2, Param);
				break;
				case 	3:
				with0->Volts = Parser[ActorID]->MakeDouble_();
				break; // basekv
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
				with0->R = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
				with0->X = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
				with0->C = Parser[ActorID]->MakeDouble_();
				break;

    /*     10:  Case Uppercase(Param)[1] of
                  'P': ScanType :=  1;
                  'Z': ScanType :=  0;
                  'N': ScanType := -1;
                ELSE
                   DoSimpleMsg('Unknown Scan Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
           11:  Case Uppercase(Param)[1] of
                  'P': Sequencetype :=  1;
                  'Z': Sequencetype :=  0;
                  'N': Sequencetype := -1;
                ELSE
                   DoSimpleMsg('Unknown Sequence Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
    */
				case 	10:
				with0->ENorth = Parser[ActorID]->MakeDouble_();
				break;
				case 	11:
				with0->EEast = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->Lat1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->Lon1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->Lat2 = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				with0->Lon2 = Parser[ActorID]->MakeDouble_();
				break;
				default:
				inherited::ClassEdit(ActiveGICLineObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	3: case 4:
				with0->VoltsSpecified = true;
				break;
				case 10: case 11: case 12: case 13: case 14: case 15:
				with0->VoltsSpecified = false;
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

int TGICLine::MakeLike(const String OtherLine)
{
	int result = 0;
	TGICLineObj* OtherGICLine = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line name in the present collection*/
	OtherGICLine = ((TGICLineObj*) Find(OtherLine));
	if(OtherGICLine != nullptr)
		/*# with ActiveGICLineObj do */
		{
			auto with0 = ActiveGICLineObj;
			int stop = 0;
			if(with0->Fnphases != OtherGICLine->Fnphases)
			{
				with0->Set_NPhases(OtherGICLine->Fnphases);
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
			with0->Z->CopyFrom(OtherGICLine->Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
			with0->R = OtherGICLine->R;
			with0->X = OtherGICLine->X;
			with0->C = OtherGICLine->C;
			with0->Volts = OtherGICLine->Volts;
			with0->Angle = OtherGICLine->Angle;
			with0->SrcFrequency = OtherGICLine->SrcFrequency;
			with0->ScanType = OtherGICLine->ScanType;
			with0->SequenceType = OtherGICLine->SequenceType;
			ClassMakeLike(OtherGICLine);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->FPropertyValue[i - 1] = OtherGICLine->FPropertyValue[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in GICLine MakeLike: \"") + OtherLine
	           + "\" Not Found.", 322);
	return result;
}

//----------------------------------------------------------------------------

int TGICLine::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TGICLine.Init", -1);
	result = 0;
	return result;
}

//=============================================================================

double TGICLineObj::Compute_VLine()
{
	double result = 0.0;
	double pHi = 0.0;
	double DeltaLat = 0.0;
	double DeltaLon = 0.0;
	pHi = (Lat2 + Lat1) / 2.0 * (double(DSSGlobals::PI) / 180.0);   // deg to radians
	DeltaLat = Lat2 - Lat1;
	DeltaLon = Lon2 - Lon1;
	VE = (111.133 - 0.56 * cos(2.0 * pHi)) * DeltaLat * ENorth;
	Vn = (111.5065L - 0.1872L * cos(2.0 * pHi)) * cos(pHi) * DeltaLon * EEast;
	result = Vn + VE;
	return result;
}

TGICLineObj::TGICLineObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			Angle(0.0),
			Volts(0.0),
			Vmag(0.0),
			SrcFrequency(0.0),
			R(0.0),
			X(0.0),
			C(0.0),
			ENorth(0.0),
			EEast(0.0),
			Lat1(0.0),
			Lon1(0.0),
			Lat2(0.0),
			Lon2(0.0),
			Vn(0.0),
			VE(0.0),
			ScanType(0),
			SequenceType(0),
			VoltsSpecified(false),
			Z(nullptr),
			Zinv(nullptr)
{
	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	Set_NPhases(3);
	Fnconds = 3;
	Set_NTerms(2);   // Now a 2-terminal device
	Z = nullptr;
	Zinv = nullptr;
     /*Basefrequency := 60.0;*/ // set in base class
	R = 1.0;
	X = 0.0;
	C = 0.0;
	ENorth = 1.0;
	EEast = 1.0;
	Lat1 = 33.613499;
	Lon1 = -87.373673;
	Lat2 = 33.547885;
	Lon2 = -86.074605;
	VoltsSpecified = false;
	SrcFrequency = 0.1;  // Typical GIC study frequency
	ScanType = 0;
	SequenceType = 0; // default to zero sequence (same voltage induced in all phases)
	Spectrum = "";  // no default
	InitPropertyValues(0);
	Yorder = Fnterms * Fnconds;
	RecalcElementData(ActiveActor);
}


//=============================================================================

TGICLineObj::~TGICLineObj()
{
	delete Z;
	delete Zinv;
	// inherited::Destroy();
}


//=============================================================================

void TGICLineObj::RecalcElementData(int ActorID)
{
	complex Zs = {};
	complex ZM = {};
	int i = 0;
	int j = 0;
	int stop = 0;
	if(Z != nullptr)
		delete Z;
	if(Zinv != nullptr)
		delete Zinv;

    // For a Source, nphases = ncond, for now
	Z = new TcMatrix(Fnphases);
	Zinv = new TcMatrix(Fnphases);

    /*Update property Value array*/
     /* Don't change a specified value; only computed ones*/
	Zs = cmplx(R, X);
	ZM = CZero;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		Z->SetElement(i, i, Zs);
		for(stop1 = i - 1, j = 1; j <= stop1; j++)
		{
			Z->SetElemsym(i, j, ZM);
		}
	}
	if(!VoltsSpecified)
		Volts = Compute_VLine();
	Vmag = Volts;
	SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
	if((SpectrumObj == nullptr) && (Spectrum.size() > 0))
	{
		DoSimpleMsg(String("Spectrum Object \"") + Spectrum
	           + "\" for Device GICLine."
	           + get_Name()
	           + " Not Found.", 324);
	}
	InjCurrent = (pComplexArray)realloc(InjCurrent, sizeof(complex) * Yorder);
}

//=============================================================================

void TGICLineObj::CalcYPrim(int ActorID)
{
	complex Value = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;
	double xc = 0.0;

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
	FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
	FreqMultiplier = FYprimFreq / BaseFrequency;

     /* Put in Series RL Adjusted for frequency */
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Fnphases, j = 1; j <= stop1; j++)
		{
			Value = Z->GetElement(i, j);
			Value.im = Value.im * FreqMultiplier;  /*Modify from base freq*/
			Zinv->SetElement(i, j, Value);
		}
	}
	if(C > 0.0) // Add 1/wC into diagonals of Zinv
	{
		int stop = 0;
		xc = -1.0 / (TwoPi * FYprimFreq * C * 1.0e-6);
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			Zinv->AddElement(i, i, cmplx(0.0, xc));
		}
	}
	Zinv->Invert();  /*Invert in place*/
	if(Zinv->InvertError > 0)       /*If error, put in Large series conductance*/
	{
		int stop = 0;
		DoErrorMsg("TGICLineObj.CalcYPrim", String("Matrix Inversion Error for GICLine \"") + get_Name()
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
			YPrim_Series->SetElemsym(i + Fnphases, j, cnegate(Value));
		}
	}
	YPrim->CopyFrom(YPrim_Series);

     /*Now Account for Open Conductors*/
     /*For any conductor that is open, zero out row and column*/
	TDSSCktElement::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

//=============================================================================

void TGICLineObj::GetVterminalForSource()
{
	int i = 0;
	complex Vharm = {};
	double SrcHarmonic = 0.0;
	try


  /*This formulation will theoretically handle voltage sources of any number of phases assuming they are
   equally displaced in time.*/
	{
		Vmag = Volts;
		/*# with ActiveCircuit[ActiveActor].Solution do */
		{
			auto with0 = ActiveCircuit[ActiveActor]->Solution;
			if(with0->IsHarmonicModel && (SpectrumObj != nullptr))
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
  // non-harmonic modes or no spectrum
			{
				int stop = 0;
				if(Abs((int) (with0->get_FFrequency() - SrcFrequency)) > EPSILON2)
					Vmag = 0.0;  // Solution Frequency and Source Frequency don't match!
					       /*NOTE: RE-uses VTerminal space*/
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					switch(SequenceType)
					{
						case -1:   // Always 0 for GIC
						(Vterminal)[i - 1] = pdegtocomplex(Vmag, (360.0 + Angle + (i - 1) * 360.0 / Fnphases));
						break;  // neg seq
						case 	0:
						(Vterminal)[i - 1] = pdegtocomplex(Vmag, (360.0 + Angle));
						break;   // all the same for zero sequence
						default:
						(Vterminal)[i - 1] = pdegtocomplex(Vmag, (360.0 + Angle - (i - 1) * 360.0 / Fnphases));
						break;
					}
                // bottom part of the vector is zero
					(Vterminal)[i + Fnphases - 1] = CZero;    // See comments in GetInjCurrents
				}
			}
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Error computing Voltages for GICLine.") + get_Name()
	           + ". Check specification. Aborting.", 326);
		if(In_Redirect)
			Redirect_Abort = true;
	}
}

//===========================================================================

int TGICLineObj::InjCurrents(int ActorID)
{
	int result = 0;
	GetInjCurrents(InjCurrent, ActorID);

/*This is source injection*/
	result = inherited::InjCurrents(ActorID); // Add into system array
	return result;
}

//===========================================================================

void TGICLineObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	try
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			int stop = 0;
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				if(!ADiakoptics || (ActorID == 1))
					(Vterminal)[i - 1] = with0->NodeV[(NodeRef)[i - 1]];
				else
					(Vterminal)[i - 1] = with0->VoltInActor1((NodeRef)[i - 1]);
			}
			YPrim->MVmult(Curr, &(Vterminal[0]));  // Current from Elements in System Y
			GetInjCurrents(&(ComplexBuffer[0]), ActorID);  // Get present value of inj currents
      // Add Together  with yprim currents
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				(Curr)[i - 1] = csub((Curr)[i - 1], (ComplexBuffer)[i - 1]);
			}
		}  /*With*/
	}
	catch (std::exception &e)
	{
		DoErrorMsg((String("GetCurrents for Element: ") + get_Name() + "."), (std::string) e.what(), "Inadequate storage allotted for circuit element.", 327);
	}
}


//=============================================================================

void TGICLineObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{


   /* source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |GICLine  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   */
	GetVterminalForSource();  // gets voltage vector above
	YPrim->MVmult(Curr, &(Vterminal[0]));
	set_ITerminalUpdated(false, ActorID);
}

//=============================================================================

void TGICLineObj::DumpProperties(TTextRec& f, bool Complete)
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
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
	{
		int stop = 0;
		WriteLn(f);
		{ Write(f, "BaseFrequency="); WriteLn(f, BaseFrequency, 0, 1); }
		{ Write(f, "Volts="); WriteLn(f, Volts, 0, 2); }
		{ Write(f, "VMag="); WriteLn(f, Vmag, 0, 2); }
		{ Write(f, "VE="); WriteLn(f, VE, 0, 4); }
		{ Write(f, "VN="); WriteLn(f, Vn, 0, 4); }
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

void TGICLineObj::InitPropertyValues(int ArrayOffset)
{


     /*PropertyValue Allocated in DSSObject.Create*/
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,GetBus(2));
	Set_PropertyValue(3,"0.0");
	Set_PropertyValue(4,"0");
	Set_PropertyValue(5,"0.1");
	Set_PropertyValue(6,"3");
	Set_PropertyValue(7,"1.0");
	Set_PropertyValue(8,"0");
	Set_PropertyValue(9,"0");

   //  Set_PropertyValue(10] := 'zero');
  //   Set_PropertyValue(11] := 'zero');
	Set_PropertyValue(10,"1.0");
	Set_PropertyValue(11,"1.0");
	Set_PropertyValue(12,"33.613499");
	Set_PropertyValue(13,"-87.373673");
	Set_PropertyValue(14,"33.547885");
	Set_PropertyValue(15,"-86.074605");
	inherited::InitPropertyValues(NumPropsThisClass);
}

//=============================================================================

String TGICLineObj::GetPropertyValue(int Index)
{
	String result;
	switch(Index)
	{
		case 	1:
		result = GetBus(1);
		break;
		case 	2:
		result = GetBus(2);
		break;
		case 	3:
		result = Format("%.8g", Volts);
		break;
		case 	4:
		result = Format("%.8g", Angle);
		break;
		case 	5:
		result = Format("%.8g", SrcFrequency);
		break;
		default:
		result = TDSSCktElement::GetPropertyValue(Index);
		break;
	}
	return result;
}

//=============================================================================

void TGICLineObj::MakePosSequence(int ActorID)
{
	String s;
	s = "Phases=1 ";
	s = s + Format("Voltage=%-.8g  Angle=%=.5g", Volts, Angle);
	s = s + Format("R=%-.8g ", R);
	s = s + Format("X=%-.8g ", X);
	Parser[ActorID]->SetCmdString(s);
	Edit(ActorID);
	TDSSCktElement::MakePosSequence(ActorID);
}




}  // namespace GICLine




