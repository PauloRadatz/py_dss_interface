
#pragma hdrstop

#include "GICsource.h"

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
using namespace Line;
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace Utilities;

namespace GICsource
{

TGICSourceObj::TGICSourceObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TGICSourceObj::TGICSourceObj(String ClassName) : inherited(ClassName) {}
TGICSourceObj::TGICSourceObj() {}


TGICSourceObj* ActiveGICsourceObj = nullptr;
TGICsource* GICsourceClass = nullptr;
int NumPropsThisClass = 0;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TGICsource::TGICsource()
{
	;
	Class_Name = "GICsource";
	DSSClassType = SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	GICsourceClass = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TGICsource::~TGICsource()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGICsource::DefineProperties()
{
	NumPropsThisClass = 10;
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "Volts";
	PropertyName[2 - 1] = "angle";
	PropertyName[3 - 1] = "frequency";
	PropertyName[4 - 1] = "phases";
	PropertyName[5 - 1] = "EN";
	PropertyName[6 - 1] = "EE";
	PropertyName[7 - 1] = "Lat1";
	PropertyName[8 - 1] = "Lon1";
	PropertyName[9 - 1] = "Lat2";
	PropertyName[10 - 1] = "Lon2";

     // define Property help values
	PropertyHelp[1 - 1] = String("Voltage magnitude, in volts, of the GIC voltage induced across the associated line. " "When specified, induced voltage is assumed defined by Voltage and Angle properties. ") + CRLF
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
	PropertyHelp[2 - 1] = "Phase angle in degrees of first phase. Default=0.0.  See Voltage property";
	PropertyHelp[3 - 1] = "Source frequency.  Defaults to  0.1 Hz. So GICSource=0 at power frequency.";
	PropertyHelp[4 - 1] = "Number of phases.  Defaults to 3. All three phases are assumed in phase (zero sequence)";
	PropertyHelp[5 - 1] = "Northward Electric field (V/km). If specified, Voltage and Angle are computed from EN, EE, lat and lon values.";
	PropertyHelp[6 - 1] = "Eastward Electric field (V/km).  If specified, Voltage and Angle are computed from EN, EE, lat and lon values.";
	PropertyHelp[7 - 1] = "Latitude of Bus1 of the line(degrees)";
	PropertyHelp[8 - 1] = "Longitude of Bus1 of the line (degrees)";
	PropertyHelp[9 - 1] = "Latitude of Bus2 of the line (degrees)";
	PropertyHelp[10 - 1] = "Longitude of Bus2 of the line (degrees)";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // Override help string
	PropertyHelp[NumPropsThisClass + 1] = "Not used.";
	PropertyHelp[NumPropsThisClass + 2] = "Not used.";
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGICsource::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new voltage source and add it to GICsource class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TGICSourceObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGICsource::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
  // continue parsing with contents of Parser
	ActiveGICsourceObj = (TGICSourceObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveGICsourceObj);
	result = 0;
	/*# with ActiveGICsourceObj do */
	{
		auto with0 = ActiveGICsourceObj;
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
				with0->Volts = Parser[ActorID]->MakeDouble_();
				break;
				case 	2:
				with0->Angle = Parser[ActorID]->MakeDouble_();
				break; // Ang
				case 	3:
				with0->SrcFrequency = Parser[ActorID]->MakeDouble_();
				break; // freq   Usually 0.1 Hz
				case 	4:
				{
					with0->Set_NPhases(Parser[ActorID]->MakeInteger_()); // num phases
					with0->FphaseShift = 0.0;     // Zero Sequence
					with0->Set_Nconds(with0->Fnphases);  // Force Reallocation of terminal info
				}
				break;
				case 	5:
				with0->ENorth = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				with0->EEast = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				with0->Lat1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
				with0->Lon1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
				with0->Lat2 = Parser[ActorID]->MakeDouble_();
				break;
				case 	10:
				with0->Lon2 = Parser[ActorID]->MakeDouble_();
				break;
				default:
				inherited::ClassEdit(ActiveGICsourceObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	1: case 2:
				with0->VoltsSpecified = true;
				break;
				case 5: case 6: case 7: case 8: case 9: case 10:
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

int TGICsource::MakeLike(const String OtherSource)
{
	int result = 0;
	TGICSourceObj* OtherGICsource = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line name in the present collection*/
	OtherGICsource = ((TGICSourceObj*) Find(OtherSource));
	if(OtherGICsource != nullptr)
		/*# with ActiveGICsourceObj do */
		{
			auto with0 = ActiveGICsourceObj;
			int stop = 0;
			if(with0->Fnphases != OtherGICsource->Fnphases)
			{
				with0->Set_NPhases(OtherGICsource->Fnphases);
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->Volts = OtherGICsource->Volts;
			with0->Angle = OtherGICsource->Angle;
			with0->SrcFrequency = OtherGICsource->SrcFrequency;
			with0->LineName = OtherGICsource->LineName;
			with0->ENorth = OtherGICsource->ENorth;
			with0->EEast = OtherGICsource->EEast;
			with0->Lat1 = OtherGICsource->Lat1;
			with0->Lon1 = OtherGICsource->Lon1;
			with0->Lat2 = OtherGICsource->Lat2;
			with0->Lon2 = OtherGICsource->Lon2;
			with0->Bus2Defined = OtherGICsource->Bus2Defined;
			ClassMakeLike(OtherGICsource); // set spectrum,  base frequency
			with0->Spectrum = "";  // Spectrum not allowed
			with0->SpectrumObj = nullptr;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherGICsource->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in GICsource MakeLike: \"") + OtherSource
	           + "\" Not Found.", 332);
	return result;
}

//----------------------------------------------------------------------------

int TGICsource::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TGICsource.Init", -1);
	result = 0;
	return result;
}


//----------------------------------------------------------------------------

TGICSourceObj::TGICSourceObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			FphaseShift(0.0),
			Bus2Defined(false),
			Vmag(0.0),
			Angle(0.0),
			SrcFrequency(0.0),
			pLineElem(nullptr),
			Vn(0.0),
			VE(0.0),
			LineClass(nullptr),
			ENorth(0.0),
			EEast(0.0),
			Lat1(0.0),
			Lon1(0.0),
			Lat2(0.0),
			Lon2(0.0),
			Volts(0.0),
			VoltsSpecified(false)
{
	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType; // SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	LineName = get_Name();  // GICsource name must be same as associated Line
	LineClass = (TLine*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("Line"));
	Set_NPhases(3);
	Fnconds = 3;
	Set_NTerms(2);   // 4/27/2018 made a 2-terminal I source
	Volts = 0.0;
	Angle = 0.0;
	ENorth = 1.0;
	EEast = 1.0;
	Lat1 = 33.613499;
	Lon1 = -87.373673;
	Lat2 = 33.547885;
	Lon2 = -86.074605;
	VoltsSpecified = false;
	SrcFrequency = 0.1;   // this is the GIC source
	InitPropertyValues(0);
	Yorder = Fnterms * Fnconds;
    // Don't do This here RecalcElementData;
	Spectrum = "";  // Spectrum not allowed
	SpectrumObj = nullptr;
}


//----------------------------------------------------------------------------

TGICSourceObj::~TGICSourceObj()
{
	LineName = "";
	// inherited::Destroy();
}


//=============================================================================

double TGICSourceObj::Compute_VLine()
{
	double result = 0.0;
	double pHi = 0.0;
	double DeltaLat = 0.0;
	double DeltaLon = 0.0;
	pHi = (Lat2 + Lat1) / 2.0 * (double(DSSGlobals::PI) / 180.0);   // deg to radians
	DeltaLat = Lat1 - Lat2; // switched 11-20 to get pos GIC for pos ENorth
	DeltaLon = Lon1 - Lon2;
	VE = (111.133 - 0.56 * cos(2.0 * pHi)) * DeltaLat * ENorth;
	Vn = (111.5065L - 0.1872L * cos(2.0 * pHi)) * cos(pHi) * DeltaLon * EEast;
	result = Vn + VE;
	return result;
}

//----------------------------------------------------------------------------

void TGICSourceObj::RecalcElementData(int ActorID)
{
	String GICBus;
	String LineBus2;
	pLineElem = ((TLineObj*) ( (TDSSClass*) LineClass )->Find(LineName));
	if(pLineElem == nullptr)
	{
		DoSimpleMsg(String("Line Object \"") + LineName
	           + "\" associated with GICsource."
	           + get_Name()
	           + " Not Found. Make sure you define it first.", 333);
	}
	else
	{
		LineBus2 = ( (TDSSCktElement*) pLineElem )->GetBus(2);

         // If LineBus2 already begins with GIC, Don't insert the GIC Bis
		if(CompareTextShortest("GIC_", LineBus2) != 0)
             // Define buses -- inserting a new bus GIC_{LineName}
		{
			GICBus = String("GIC_") + LineName;
			SetBus(1, GICBus);
			SetBus(2, LineBus2);
             // Redefine the bus2 spec for LineElem
			Parser[ActorID]->SetCmdString(String("Bus2=") + GICBus);
			( (TDSSObject*) pLineElem )->Edit(ActorID);  // invoke the Line's editor to process Parser
		}
		Bus2Defined = true;
		if(!VoltsSpecified)
			Volts = Compute_VLine();
	}
	InjCurrent = (pComplexArray)realloc(InjCurrent, sizeof(complex) * Yorder);
}

//----------------------------------------------------------------------------

void TGICSourceObj::CalcYPrim(int ActorID)
{
	double Rs = 0.0;
	double Rm = 0.0;
	double Rzero = 0.0;
	int i = 0;
	complex Value = {};
	complex NegValue = {};

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


     /*
       Assume 0.0001 ohms resistance for GIC Source
     */
	Value = cmplx(10000.0, 0.0);
	NegValue = cnegate(Value);
	/*# with YPrim_Series do */
	{
		auto with0 = YPrim_Series;
		int stop = 0;
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			with0->SetElement(i, i, Value);
			with0->SetElement(i + Fnphases, i + Fnphases, Value);
			with0->SetElemsym(i, i + Fnphases, NegValue);
		}
	}
	YPrim->CopyFrom(YPrim_Series);      // Initialize YPrim for series impedances
/* ****************************************************************************
    {
       Compute R0 of associated line
       Average diagonals of Z matrix and off-diagonals
       Z0=Zs + 2 Zm
     }

      IF abs(ActiveCircuit.Solution.Frequency - SrcFrequency) < EPSILON2 THEN Begin
          Rs := 0.0;  { zero the accumulators}
          Rm := 0.0;
          With pLineElem Do
          Begin
            for i := 1 to NPhases do Begin
              Rs := Rs + Z.GetElement(i,i).re;
              for j := i+1 to NPhases  do
                  Rm := Rm + Z.GetElement(i,j).re;
            End;
            Rs := Rs / NPhases;
            Rm := Rm / (NPhases * (NPhases-1)/2);
            Rzero :=  (Rs + 2.0*Rm) * Len / UnitsConvert;  // Total for entire line
            Gzero := 1.0/Rzero; // zero-sequence conductance of line

          End;
      End;
   ***********************************************************************************
 */

     /*Now Account for Open Conductors*/
     /*For any conductor that is open, zero out row and column*/
	TDSSCktElement::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

void TGICSourceObj::GetVterminalForSource(int ActorID)
{
	double Vmag = 0.0;
	int i = 0;
	try

     // If the solution frequency not 0.1 Hz, source is shorted.
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			int stop = 0;
			if(Abs((int) (with0->get_FFrequency() - SrcFrequency)) < EPSILON2)
				Vmag = Volts;
			else
				Vmag = 0.0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				(Vterminal)[i - 1] = pdegtocomplex(Vmag, (Angle));   // all the same for zero sequence
                 // bottom part of the vector is zero
				(Vterminal)[i + Fnphases - 1] = CZero;    // See comments in GetInjCurrents
			}
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Error computing current for GICsource.") + get_Name()
	           + ". Check specification. Aborting.", 334);
		if(In_Redirect)
			Redirect_Abort = true;
	}
}

/*Sum Currents directly into solution array*/

int TGICSourceObj::InjCurrents(int ActorID)
{
	int result = 0;
	GetInjCurrents(InjCurrent, ActorID);
	result = inherited::InjCurrents(ActorID);  // Adds into system array
	return result;
}

/*Total currents into a device*/

void TGICSourceObj::GetCurrents(pComplexArray Curr, int ActorID)
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
		DoErrorMsg((String("GetCurrents for GICsource Element: ") + get_Name() + "."), (std::string) e.what(), "Inadequate storage allotted for circuit element?", 335);
	}
}

  /* source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |GICLineVolts  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   */

void TGICSourceObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	GetVterminalForSource(ActorID);    // only at 0.1 Hz
	YPrim->MVmult(Curr, &(Vterminal[0]));
	set_ITerminalUpdated(false, ActorID);
}

String TGICSourceObj::GetPropertyValue(int Index)
{
	String result;
	switch(Index)
	{
		case 	1:
		result = Format("%.8g", Volts);
		break;
		case 	2:
		result = Format("%.8g", Angle);
		break;
		case 	3:
		result = Format("%.8g", SrcFrequency);
		break;
		default:
		result = TDSSCktElement::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TGICSourceObj::DumpProperties(TTextRec& f, bool Complete)
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

void TGICSourceObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"0");
	Set_PropertyValue(2,"0");
	Set_PropertyValue(3,Format("%-.6g", SrcFrequency));
	Set_PropertyValue(4,"3");
	Set_PropertyValue(5,"1.0");
	Set_PropertyValue(6,"1.0");
	Set_PropertyValue(7,"33.613499");
	Set_PropertyValue(8,"-87.373673");
	Set_PropertyValue(9,"33.547885");
	Set_PropertyValue(10,"-86.074605");
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TGICSourceObj::MakePosSequence(int ActorID)
{
	if(Fnphases > 1)
	{
		Parser[ActorID]->SetCmdString("phases=1");
		Edit(ActorID);
	}
	TDSSCktElement::MakePosSequence(ActorID);
}




}  // namespace GICsource




