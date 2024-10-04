
#pragma hdrstop

#include "Isource.h"

#include "ParserDel.h"
#include "Circuit.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Utilities.h"
#include "Command.h"
#include "Dynamics.h"

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

namespace ISource
{

TIsourceObj::TIsourceObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TIsourceObj::TIsourceObj(String ClassName) : inherited(ClassName) {}
TIsourceObj::TIsourceObj() {}


TIsourceObj* ActiveIsourceObj = nullptr;
int NumPropsThisClass = 0;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TIsource::TIsource()
{
	;
	Class_Name = "Isource";
	DSSClassType = SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	IsourceClass[ActiveActor] = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TIsource::~TIsource()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TIsource::DefineProperties()
{
	NumPropsThisClass = 11;
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "bus1";
	PropertyName[2 - 1] = "amps";
	PropertyName[3 - 1] = "angle";
	PropertyName[4 - 1] = "frequency";
	PropertyName[5 - 1] = "phases";
	PropertyName[6 - 1] = "scantype";
	PropertyName[7 - 1] = "sequence";
	PropertyName[8 - 1] = "Yearly";
	PropertyName[9 - 1] = "Daily";
	PropertyName[10 - 1] = "Duty";
	PropertyName[11 - 1] = "Bus2";

     // define Property help values
	PropertyHelp[1 - 1] = String("Name of bus to which source is connected.") + CRLF
	           + "bus1=busname"
	           + CRLF
	           + "bus1=busname.1.2.3";
	PropertyHelp[2 - 1] = "Magnitude of current source, each phase, in Amps.";
	PropertyHelp[3 - 1] = String("Phase angle in degrees of first phase: e.g.,Angle=10.3.") + CRLF
	           + "Phase shift between phases is assumed 120 degrees when "
	           + "number of phases <= 3";
	PropertyHelp[4 - 1] = "Source frequency.  Defaults to  circuit fundamental frequency.";
	PropertyHelp[5 - 1] = "Number of phases.  Defaults to 3. For 3 or less, phase shift is 120 degrees.";
	PropertyHelp[6 - 1] = "{pos*| zero | none} Maintain specified sequence for harmonic solution. Default is positive sequence. "
	           "Otherwise, angle between phases rotates with harmonic.";
	PropertyHelp[7 - 1] = "{pos*| neg | zero} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. "
	           "Default is positive sequence. ";
	PropertyHelp[8 - 1] = String("LOADSHAPE object to use for the per-unit current for YEARLY-mode simulations. Set the Mult property of the LOADSHAPE " "to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual Amp.") + CRLF
	           + CRLF
	           + "Must be previously defined as a LOADSHAPE object. "
	           + CRLF
	           + CRLF
	           + "Is set to the Daily load shape when Daily is defined.  The daily load shape is repeated in this case. "
	           + "Set to NONE to reset to no loadahape for Yearly mode. "
	           + "The default is no variation.";
	PropertyHelp[9 - 1] = String("LOADSHAPE object to use for the per-unit current for DAILY-mode simulations. Set the Mult property of the LOADSHAPE " "to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual A.") + CRLF
	           + CRLF
	           + "Must be previously defined as a LOADSHAPE object. "
	           + CRLF
	           + CRLF
	           + "Sets Yearly curve if it is not already defined.   "
	           + "Set to NONE to reset to no loadahape for Yearly mode. "
	           + "The default is no variation.";
	PropertyHelp[10 - 1] = String("LOADSHAPE object to use for the per-unit current for DUTYCYCLE-mode simulations. Set the Mult property of the LOADSHAPE " "to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual A.") + CRLF
	           + CRLF
	           + "Must be previously defined as a LOADSHAPE object. "
	           + CRLF
	           + CRLF
	           + "Defaults to Daily load shape when Daily is defined.   "
	           + "Set to NONE to reset to no loadahape for Yearly mode. "
	           + "The default is no variation.";
	PropertyHelp[11 - 1] = String("Name of bus to which 2nd terminal is connected.") + CRLF
	           + "bus2=busname"
	           + CRLF
	           + "bus2=busname.1.2.3"
	           + CRLF
	           + CRLF
	           + "Default is Bus1.0.0.0 (grounded-wye connection)";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // Override help string
	PropertyHelp[NumPropsThisClass + 1] = "Harmonic spectrum assumed for this source.  Default is \"default\".";
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TIsource::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new voltage source and add it to Isource class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TIsourceObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TIsource::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
  // continue parsing with contents of Parser
	ActiveIsourceObj = (TIsourceObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveIsourceObj);
	result = 0;
	/*# with ActiveIsourceObj do */
	{
		auto with0 = ActiveIsourceObj;
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
				   + with0->get_Name()
				   + "\"", 330);
				break;
				case 	1:
				IsourceSetBus1(Param);
				break;
				case 	2:
				with0->Amps = Parser[ActorID]->MakeDouble_();
				break;
				case 	3:
				with0->Angle = Parser[ActorID]->MakeDouble_();
				break; // Ang
				case 	4:
				with0->SrcFrequency = Parser[ActorID]->MakeDouble_();
				break; // freq
				case 	5:
				{
					with0->Set_NPhases(Parser[ActorID]->MakeInteger_()); // num phases
					switch(with0->Fnphases)
					{
						case 	1:
						with0->FphaseShift = 0.0;
						break;
						case 	2: case 3:
						with0->FphaseShift = 120.0;
						break;     // higher order systems
						default:
						with0->FphaseShift = 360.0 / with0->Fnphases;
						break;
					}
					with0->Set_Nconds(with0->Fnphases);  // Force Reallocation of terminal info
				}
				break;
				case 	6:
				switch(UpperCase(Param)[0])
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
					   + Param, 331);
					break;
				}
				break;
				case 	7:
				switch(UpperCase(Param)[0])
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
					   + Param, 331);
					break;
				}
				break;
				case 	8:
				with0->YearlyShape = Param;
				break;
				case 	9:
				with0->DailyShape = Param;
				break;
				case 	10:
				with0->DutyShape = Param;
				break;
				case 	11:
				with0->SetBus(2, Param);
				break;
				default:
				inherited::ClassEdit(ActiveIsourceObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	8:
            /*Set shape objects;  returns nil if not valid*/
            /*Sets the kW and kvar properties to match the peak kW demand from the Loadshape*/
				with0->YearlyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->YearlyShape));
				break;
				case 	9:
				{
					with0->DailyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DailyShape));
                  /*If Yearly load shape is not yet defined, make it the same as Daily*/
					if(with0->YearlyShapeObj == nullptr)
						with0->YearlyShapeObj = with0->DailyShapeObj;
				}
				break;
				case 	10:
				with0->DutyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DutyShape));
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

int TIsource::MakeLike(const String OtherSource)
{
	int result = 0;
	TIsourceObj* OtherIsource = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line name in the present collection*/
	OtherIsource = ((TIsourceObj*) Find(OtherSource));
	if(OtherIsource != nullptr)
		/*# with ActiveIsourceObj do */
		{
			auto with0 = ActiveIsourceObj;
			int stop = 0;
			if(with0->Fnphases != OtherIsource->Fnphases)
			{
				with0->Set_NPhases(OtherIsource->Fnphases);
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->Amps = OtherIsource->Amps;
			with0->Angle = OtherIsource->Angle;
			with0->SrcFrequency = OtherIsource->SrcFrequency;
			with0->ScanType = OtherIsource->ScanType;
			with0->SequenceType = OtherIsource->SequenceType;
			with0->ShapeIsActual = OtherIsource->ShapeIsActual;
			with0->DailyShape = OtherIsource->DailyShape;
			with0->DailyShapeObj = OtherIsource->DailyShapeObj;
			with0->DutyShape = OtherIsource->DutyShape;
			with0->DutyShapeObj = OtherIsource->DutyShapeObj;
			with0->YearlyShape = OtherIsource->YearlyShape;
			with0->YearlyShapeObj = OtherIsource->YearlyShapeObj;
			with0->Bus2Defined = OtherIsource->Bus2Defined;
			ClassMakeLike(OtherIsource); // set spectrum,  base frequency
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherIsource->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Isource MakeLike: \"") + OtherSource
	           + "\" Not Found.", 332);
	return result;
}

//----------------------------------------------------------------------------

int TIsource::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TIsource.Init", -1);
	result = 0;
	return result;
}

void TIsource::IsourceSetBus1(const String s)
{
	String	S2 = "";
	int		i = 0,
			dotpos = 0;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0
	/*# with ActiveIsourceObj do */
	{
		auto with0 = ActiveIsourceObj;
		with0->SetBus(1, s);
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

//----------------------------------------------------------------------------

TIsourceObj::TIsourceObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			FphaseShift(120.0),
			ShapeIsActual(false),
			Bus2Defined(false),
			Amps(0.0),
			Angle(0.0),
			SrcFrequency(0.0),
			ScanType(0),
			SequenceType(0),
			PerUnit(0.0),
			DailyShapeObj(nullptr),
			DutyShapeObj(nullptr),
			YearlyShapeObj(nullptr)
{
	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType; // SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	Set_NPhases(3);
	Fnconds = 3;
	Set_NTerms(2);
	Amps = 0.0;
	Angle = 0.0;
	PerUnit = 1.0;  // for future use if pu property added,
	SrcFrequency = BaseFrequency;
	ScanType = 1;  // Pos Sequence
	SequenceType = 1;
	Bus2Defined = false;
	InitPropertyValues(0);
	YearlyShape = "";
	YearlyShapeObj = nullptr;  // IF YearlyShapeobj = nil THEN the Vsource alway stays nominal
	DailyShape = "";
	DailyShapeObj = nullptr;  // IF DaillyShapeobj = nil THEN the Vsource alway stays nominal
	DutyShape = "";
	DutyShapeObj = nullptr;  // IF DutyShapeobj = nil THEN the Vsource alway stays nominal
	Yorder = Fnterms * Fnconds;
	RecalcElementData(ActiveActor);
}


//----------------------------------------------------------------------------

TIsourceObj::~TIsourceObj()
{
	// inherited::Destroy();
}


//----------------------------------------------------------------------------

void TIsourceObj::RecalcElementData(int ActorID)
{
	SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
	if(SpectrumObj == nullptr)
	{
		DoSimpleMsg(String("Spectrum Object \"") + Spectrum
	           + "\" for Device Isource."
	           + get_Name()
	           + " Not Found.", 333);
	}
	InjCurrent = (pComplexArray) realloc(InjCurrent, sizeof(complex) * Yorder);
}

//----------------------------------------------------------------------------

void TIsourceObj::CalcYPrim(int ActorID)
{


 // Build only YPrim Series
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


     /*Yprim = 0  for Ideal Current Source;  just leave it zeroed*/

     /*Now Account for Open Conductors*/
     /*For any conductor that is open, zero out row and column*/
	TDSSCktElement::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

complex TIsourceObj::GetBaseCurr(int ActorID)
{
	complex result = {};
	double SrcHarmonic = 0.0;
	double NAmps = 0.0;
	try
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			if(with0->IsHarmonicModel)
  /*Get first Phase Current*/
			{
				SrcHarmonic = with0->get_FFrequency() / SrcFrequency;
				result = cmulreal(SpectrumObj->GetMult(SrcHarmonic), Amps);  // Base current for this harmonic
				RotatePhasorDeg(result, SrcHarmonic, Angle);
			}
			else
			{
				switch(with0->Get_SolMode())
				{
					case 	DAILYMODE:
               /*Uses same logic as LOAD*/
					{
						CalcDailyMult(with0->DynaVars.dblHour);
					}
					break;
					case 	EMPDAILYMODE:
					{
						CalcDailyMult(with0->DynaVars.dblHour);
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
							ShapeFactor = cmplx(1.0, 0.0);     // default to 1 + j0 if not known
							break;
						}
					}
					break;
					default:
					  ;
					break;
				}
				NAmps = Amps;
				if((with0->Get_SolMode() == DAILYMODE) || (with0->Get_SolMode() == YEARLYMODE) || (with0->Get_SolMode() == DUTYCYCLE) || (with0->Get_SolMode() == DYNAMICMODE))     /*If a loadshape mode simulation*/
					NAmps = Amps * ShapeFactor.re;
				if(Abs((int) (with0->get_FFrequency() - SrcFrequency)) < EPSILON2)
					result = pdegtocomplex(NAmps, Angle);
				else
					result = CZero;
			}
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Error computing current for Isource.") + get_Name()
	           + ". Check specification. Aborting.", 334);
		if(In_Redirect)
			Redirect_Abort = true;
	}
	return result;
}

/*Sum Currents directly into solution array*/

int TIsourceObj::InjCurrents(int ActorID)
{
	int result = 0;
	GetInjCurrents(InjCurrent, ActorID);
	result = inherited::InjCurrents(ActorID);  // Adds into system array
	return result;
}

/*Total currents into a device*/

void TIsourceObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	try
	{
		int stop = 0;
		GetInjCurrents(&(ComplexBuffer[0]), ActorID);  // Get present value of inj currents
      // Add Together  with yprim currents
		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			(Curr)[i - 1] = cnegate((ComplexBuffer)[i - 1]);
		}
	}
	catch (std::exception &e)
	{
		DoErrorMsg((String("GetCurrents for Isource Element: ") + get_Name() + "."), (std::string) e.what(), "Inadequate storage allotted for circuit element?", 335);
	}
}

/*Fill Up an array of injection currents*/

void TIsourceObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	complex BaseCurr = {};
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		int stop = 0;
		BaseCurr = GetBaseCurr(ActorID);   // this func applies spectrum if needed
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			(Curr)[i - 1] = BaseCurr;
			(Curr)[i + Fnphases - 1] = cnegate(BaseCurr);  // 2nd Terminal
			if(i < Fnphases)
			{
				if(with0->IsHarmonicModel)
					switch(ScanType)
					{
						case 	1:
						RotatePhasorDeg(BaseCurr, 1.0, -FphaseShift);
						break; // maintain positive sequence for isource
						case 	0:
						;
						break;  // Do not rotate for zero sequence
						default: // rotate by frequency
						                     /*Harmonic 1 will be pos; 2 is neg; 3 is zero, and so on.*/
						RotatePhasorDeg(BaseCurr, with0->Harmonic, -FphaseShift);
						break;
					}
				else
					switch(SequenceType)
					{
						case -1:
						RotatePhasorDeg(BaseCurr, 1.0, FphaseShift);
						break; // Neg seq
						case 	0:
						;
						break;  // Do not rotate for zero sequence
						default:
						RotatePhasorDeg(BaseCurr, 1.0, -FphaseShift); // Maintain pos seq
						break;
					}
			}
		}
	}
}

void TIsourceObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
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
		WriteLn(f);
		WriteLn(f);
	}
}

void TIsourceObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,"0");
	Set_PropertyValue(3,"0");
	Set_PropertyValue(4,Format("%-.6g", SrcFrequency));
	Set_PropertyValue(5,"3");
	Set_PropertyValue(6,"pos");
	Set_PropertyValue(7,"pos");
	Set_PropertyValue(8,"");
	Set_PropertyValue(9,"");
	Set_PropertyValue(10,"");
	Set_PropertyValue(11,"");
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TIsourceObj::MakePosSequence(int ActorID)
{
	if(Fnphases > 1)
	{
		Parser[ActorID]->SetCmdString("phases=1");
		Edit(ActorID);
	}
	TDSSCktElement::MakePosSequence(ActorID);
}

void TIsourceObj::CalcDailyMult(double hr)
{
	if(DailyShapeObj != nullptr)
	{
		ShapeFactor = DailyShapeObj->GetMult(hr);
		ShapeIsActual = DailyShapeObj->UseActual;
	}
	else
	ShapeFactor = cmplx(PerUnit, 0.0); // CDOUBLEONE;  // Default to no daily variation
}

void TIsourceObj::CalcDutyMult(double hr)
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

void TIsourceObj::CalcYearlyMult(double hr)
{

/*Yearly curve is assumed to be hourly only*/
	if(YearlyShapeObj != nullptr)
	{
		ShapeFactor = YearlyShapeObj->GetMult(hr);
		ShapeIsActual = YearlyShapeObj->UseActual;
	}
	else
	ShapeFactor = cmplx(PerUnit, 0.0); // CDOUBLEONE;   // Defaults to no variation
}




}  // namespace ISource




