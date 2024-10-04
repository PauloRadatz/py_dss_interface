

#pragma hdrstop

#include "UPFC.h"

#include "Circuit.h"
#include "Utilities.h"
#include "DSSGlobals.h"
#include "Solution.h"
#include "UPFCControl.h"


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
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace Solution;
using namespace System;
using namespace UPFCControl;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XYCurve;
using namespace YMatrix;
using namespace Utilities;

namespace UPFC
{

TUPFCObj::TUPFCObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TUPFCObj::TUPFCObj(String ClassName) : inherited(ClassName) {}
TUPFCObj::TUPFCObj() {}


TUPFCObj* ActiveUPFCObj = nullptr;
TUPFC* UPFC_class = nullptr;
const int propLossCurve = 11;
const int NumPropsThisClass = 17;
const int NumUPFCVariables = 14;
complex CDoubleOne = {};

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TUPFC::TUPFC()
{
	;
	Class_Name = "UPFC";
	DSSClassType = PC_ELEMENT + UPFC_ELEMENT;  // UPFC  is PC Element
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	UPFC_class = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TUPFC::~TUPFC()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TUPFC::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "bus1";
	PropertyName[2 - 1] = "bus2";
	PropertyName[3 - 1] = "refkv";
	PropertyName[4 - 1] = "pf";
	PropertyName[5 - 1] = "frequency";
	PropertyName[6 - 1] = "phases";
	PropertyName[7 - 1] = "Xs";
	PropertyName[8 - 1] = "Tol1";
	PropertyName[9 - 1] = "Mode";
	PropertyName[10 - 1] = "VpqMax";
	PropertyName[11 - 1] = "LossCurve";
	PropertyName[12 - 1] = "VHLimit";
	PropertyName[13 - 1] = "VLLimit";
	PropertyName[14 - 1] = "CLimit";
	PropertyName[15 - 1] = "refkv2";
	PropertyName[16 - 1] = "kvarLimit";
	PropertyName[17 - 1] = "Element";


     // define Property help values
	PropertyHelp[1 - 1] = String("Name of bus to which the input terminal (1) is connected.") + CRLF
	           + "bus1=busname.1.3"
	           + CRLF
	           + "bus1=busname.1.2.3";
	PropertyHelp[2 - 1] = String("Name of bus to which the output terminal (2) is connected.") + CRLF
	           + "bus2=busname.1.2"
	           + CRLF
	           + "bus2=busname.1.2.3";
	PropertyHelp[3 - 1] = String("Base Voltage expected at the output of the UPFC") + CRLF
	           + CRLF
	           + "\"refkv=0.24\"";
	PropertyHelp[4 - 1] = "Power factor target at the input terminal.";
	PropertyHelp[5 - 1] = "UPFC working frequency.  Defaults to system default base frequency.";
	PropertyHelp[6 - 1] = "Number of phases.  Defaults to 1 phase (2 terminals, 1 conductor per terminal).";
	PropertyHelp[7 - 1] = "Reactance of the series transformer of the UPFC, ohms (default=0.7540 ... 2 mH)";
	PropertyHelp[8 - 1] = String("Tolerance in pu for the series PI controller") + CRLF
	           + "Tol1=0.02 is the format used to define 2% tolerance (Default=2%)";
	PropertyHelp[9 - 1] = String("Integer used to define the control mode of the UPFC: ") + CRLF
	           + CRLF
	           + "0 = Off, "
	           + CRLF
	           + "1 = Voltage regulator, "
	           + CRLF
	           + "2 = Phase angle regulator, "
	           + CRLF
	           + "3 = Dual mode"
	           + CRLF
	           + "4 = It is a control mode where the user can set two different set points to create a secure GAP,"
	           + " these references must be defined in the parameters RefkV and RefkV2. The only restriction when "
	           + "setting these values is that RefkV must be higher than RefkV2. "
	           + CRLF
	           + "5 = In this mode the user can define the same GAP using two set points as in control mode 4. The "
	           + "only difference between mode 5 and mode 4 is that in mode 5, the UPFC controller performs dual control"
	           + " actions just as in control mode 3";
	PropertyHelp[10 - 1] = "Maximum voltage (in volts) delivered by the series voltage source (Default = 24 V)";
	PropertyHelp[11 - 1] = "Name of the XYCurve for describing the losses behavior as a function of the voltage at the input of the UPFC";
	PropertyHelp[12 - 1] = "High limit for the voltage at the input of the UPFC, if the voltage is above this value the UPFC turns off. This value is specified in Volts (default 300 V)";
	PropertyHelp[13 - 1] = "low limit for the voltage at the input of the UPFC, if voltage is below this value the UPFC turns off. This value is specified in Volts (default 125 V)";
	PropertyHelp[14 - 1] = "Current Limit for the UPFC, if the current passing through the UPFC is higher than this value the UPFC turns off. This value is specified in Amps (Default 265 A)";
	PropertyHelp[15 - 1] = String("Base Voltage expected at the output of the UPFC for control modes 4 and 5.") + CRLF
	           + CRLF
	           + "This reference must be lower than refkv, see control modes 4 and 5 for details";
	PropertyHelp[16 - 1] = "Maximum amount of reactive power (kvar) that can be absorved by the UPFC (Default = 5)";
	PropertyHelp[17 - 1] = String("The name of the PD element monitored when operating with reactive power compensation. Normally, it should be the ") +
		"PD element immediately upstream the UPFC. The element must be defined including the class, e.g. Line.myline.";

	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // Override help string
	PropertyHelp[NumPropsThisClass + 1 - 1] = "Name of harmonic spectrum for this source.  Default is \"defaultUPFC\", which is defined when the DSS starts.";
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TUPFC::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new voltage source and add it to UPFC class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TUPFCObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TUPFC::Edit(int ActorID)
{
	int Devindex;
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
//>>>   ZTemp        : Complex;

  // continue parsing with contents of Parser
	ActiveUPFCObj = (TUPFCObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveUPFCObj);
	result = 0;
	/*# with ActiveUPFCObj do */
	{
		auto with0 = ActiveUPFCObj;
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
	           + "\" for Object \"UPFC."
	           + with0->get_Name()
	           + "\"", 320);
				break;
				case 	1:
				with0->SetBus(1, Param);
				break;  // special handling of Bus 1
				case 	2:
				with0->SetBus(2, Param);
				break;     // special handling of Bus 2
				case 	3:
				with0->VRef = Parser[ActorID]->MakeDouble_();
				break; // kv Output reference
				case 	4:
				with0->PF = Parser[ActorID]->MakeDouble_();
				break; // power factor
				case 	5:
				with0->Freq = Parser[ActorID]->MakeDouble_();
				break; // Freq
				case 	6:
				{
					with0->Set_NPhases(Parser[ActorID]->MakeInteger_()); // num phases
					with0->Set_Nconds(with0->Fnphases);  // Force Reallocation of terminal info
					with0->OutCurr.resize( with0->Get_NPhases() );
					with0->InCurr.resize( with0->Get_NPhases() );
				}
				break;
				case 	7:
				with0->Xs = Parser[ActorID]->MakeDouble_();
				break; // Xs
				case 	8:
				with0->Tol1 = Parser[ActorID]->MakeDouble_();
				break; // Tolerance Ctrl 2
				case 	9:
				with0->ModeUPFC = Parser[ActorID]->MakeInteger_();
				break;
				case 	10:
				with0->Vpqmax = Parser[ActorID]->MakeDouble_();
				break;
				case 	propLossCurve:
				with0->LossCurve = Param;
				break;
				case 	12:
				with0->VHLimit = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->VLLimit = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->CLimit = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				with0->VRef2 = Parser[ActorID]->MakeDouble_();
				break;
				case 	16:
				with0->KVARLim = Parser[ActorID]->MakeDouble_();
				break;
				case	17:
				{
					with0->MonElm = LowerCase(Param);
					Devindex = GetCktElementIndex(with0->MonElm);
					if (Devindex > 0)
						with0->myElm = (TDSSCktElement*) ActiveCircuit[ActiveActor]->CktElements.Get(Devindex);
					else
						DoSimpleMsg("Monitored Element for UPFC operation does not exist:\"" + with0->MonElm + "\"", 9002);
				}
				break;
				default:
				inherited::ClassEdit(ActiveUPFCObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	propLossCurve:
				with0->UPFCLossCurveObj = ((TXYcurveObj*) XYCurveClass[ActorID]->Find(with0->LossCurve));
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

int TUPFC::MakeLike(const String OtherSource)
{
	int result = 0;
	TUPFCObj* OtherUPFC = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line name in the present collection*/
	OtherUPFC = ((TUPFCObj*) Find(OtherSource));
	if(OtherUPFC != nullptr)
		/*# with ActiveUPFCObj do */
		{
			auto with0 = ActiveUPFCObj;
			int stop = 0;
			if(with0->Fnphases != OtherUPFC->Fnphases)
			{
				with0->Set_NPhases(OtherUPFC->Fnphases);
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
			with0->Z->CopyFrom(OtherUPFC->Z);
			with0->VRef = OtherUPFC->VRef;
			with0->PF = OtherUPFC->PF;
			with0->Xs = OtherUPFC->Xs;
			with0->Tol1 = OtherUPFC->Tol1;
			with0->ZBase = OtherUPFC->ZBase;
			with0->Freq = OtherUPFC->Freq;
			with0->ModeUPFC = OtherUPFC->ModeUPFC;
			with0->Vpqmax = OtherUPFC->Vpqmax;
			with0->LossCurve = OtherUPFC->LossCurve;
			with0->UPFCLossCurveObj = with0->UPFCLossCurveObj;
			with0->VHLimit = OtherUPFC->VHLimit;
			with0->VLLimit = OtherUPFC->VLLimit;
			with0->CLimit = OtherUPFC->CLimit;
			with0->VRef2 = OtherUPFC->VRef2;
			with0->KVARLim = OtherUPFC->KVARLim;
			ClassMakeLike(OtherUPFC);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->FPropertyValue[i - 1] = OtherUPFC->FPropertyValue[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in UPFC MakeLike: \"") + OtherSource + "\" Not Found.", 322);
	return result;
}

//----------------------------------------------------------------------------

int TUPFC::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TUPFC.Init", -1);
	result = 0;
	return result;
}

//=============================================================================

TUPFCObj::TUPFCObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			VRef(0.24),
			PF(1.0),
			Xs(0.7540),
			Tol1(0.0),
			ZBase(0.0),
			Freq(0.0),
			VHLimit(0.0),
			VLLimit(0.0),
			CLimit(0.0),
			UPFCON(false),
			VRef2(0.0),
			VRefD(0.0),
			KVARLim(0.0),
			Losses(0.0),
			QIdeal(0.0),
			ModeUPFC(0),
			Vpqmax(0.0),
			SyncFlag(false),
			SF2(false),
			UPFCLossCurveObj(nullptr),
			Z(nullptr),
			Zinv(nullptr),
			Vmag(0.0)
{
	int i = 0;
	Sr0 = NULL;
	Sr1 = NULL;
	TDSSClass* MyClass = nullptr;
	TUPFCControlObj* myCtrl = nullptr;
	int stop = 0;
	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
	Set_NPhases(1);
	Fnconds = 1;   // number conductors per terminal
	Set_NTerms(2);   // A 2-terminal device
	Z = nullptr;
	Zinv = nullptr;
	Tol1 = 0.02;
	Freq = 60.0;
	Set_Enabled(true);
	ModeUPFC = 1;
	Vpqmax = 24.0;     // From the data provided
	LossCurve = "";
	UPFCLossCurveObj = nullptr;
	VHLimit = 300.0;
	VLLimit = 125.0;
	CLimit = 265.0;
	UPFCON = true;
	VRef2 = 0.0;
	KVARLim = 5;
	myElm = nullptr;

	QIdeal = 0.0;

     // Initialize shift registers
	Sr0 = (pComplexArray) realloc(Sr0, sizeof(complex) * Fnphases);
	Sr1 = (pComplexArray) realloc(Sr1, sizeof(complex) * Fnphases); 
	for(stop = Get_NPhases(), i = 1; i <= stop; i++)
	{
		(Sr0)[i - 1] = CZero;
	} //For multiphase model
	for(stop = Get_NPhases(), i = 1; i <= stop; i++)
	{
		(Sr1)[i - 1] = CZero;
	} //For multiphase model
	for(stop = Get_NPhases(), i = 1; i <= stop; i++)
	{
		ERR0[i - 1] = 0;
	} //For multiphase model
	InitPropertyValues(0);
	OutCurr.resize( Get_NPhases());
	InCurr.resize( Get_NPhases());
	for(stop = Get_NPhases(), i = 1; i <= stop; i++)
	{
		OutCurr[i - 1] = CZero; //For multiphase model
		InCurr[i - 1] = CZero; //For multiphase model
	}

     // If there is a controller, sets the flag for it to consider de new UPFC
	MyClass = (TDSSClass*) GetDSSClassPtr("upfccontrol");
	if(MyClass->Get_ElementCount() > 0)
	{
		myCtrl = ((TUPFCControlObj*) MyClass->ElementList.Get(1));
		myCtrl->get_FUPFCList()->Clear();
		myCtrl->set_FListSize(0);
	}
	Yorder = Fnterms * Fnconds;
	RecalcElementData(ActiveActor);
}


//=============================================================================

TUPFCObj::~TUPFCObj()
{
	delete Z; // Z->~TcMatrix();
	delete Zinv; // Zinv->~TcMatrix();
	free(Sr0);
	free(Sr1);
	// inherited::Destroy();
}


//=============================================================================

void TUPFCObj::RecalcElementData(int ActorID)
{
	complex Z1 = {};
	complex Value = {};
	int i = 0;
	int stop = 0;
	if(Z != nullptr)
		delete Z;
	if(Zinv != nullptr)
		delete Zinv;

    // For a Source, nphases = ncond, for now
	Z = new TcMatrix(Fnphases);
	Zinv = new TcMatrix(Fnphases);
	QIdeal = 0.0;

    /*Update property Value array*/
     /* Don't change a specified value; only computed ones*/
	Z1 = cmplx(0, Xs);
     // Diagonals  (all the same)
	Value = Z1;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		Z->SetElement(i, i, Value);
	}
	Sr0 = (pComplexArray)realloc(Sr0, sizeof(complex) * Fnphases);
	Sr1 = (pComplexArray)realloc(Sr1, sizeof(complex) * Fnphases);
	InjCurrent = (pComplexArray)realloc(InjCurrent, sizeof(complex) * Yorder);
}



//=============================================================================

void TUPFCObj::CalcYPrim(int ActorID)
{
	complex Value = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;

// Calc UPFC Losses
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
	Zinv->Invert();  /*Invert in place*/
	if(Zinv->InvertError > 0)       /*If error, put in Large series conductance*/
	{
		int stop = 0;
		DoErrorMsg("TUPFCObj.CalcYPrim", String("Matrix Inversion Error for UPFC \"") + get_Name() + "\"", "Invalid impedance specified. Replaced with small resistance.", 325);
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
	TDSSCktElement::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

//=============================================================================

double TUPFCObj::CalcUPFCLosses(double Vpu)
{
	double result = 0.0;

//  Calculates the Active power losses at the input of the device
//  By using the Load powers, the approach is based in the data provided
	result = UPFCLossCurveObj->GetYValue_(Vpu);
	return result;
}


//===========================================================================

int TUPFCObj::InjCurrents(int ActorID)
{
	int result = 0;
	GetInjCurrents(InjCurrent, ActorID);

/*This is source injection*/
	result = inherited::InjCurrents(ActorID); // Add into system array
	return result;
}

//===========================================================================
//Taken from ISources due to the kind of model
//===========================================================================
//Calculates the output current for the UPFC device
/*
        Vbin   Xs  Vbout
     <---*--=======--*--->
         |           |
 I input ^           ^ I output
         |           |

  4 modes of operation:
  mode 0: UPFC Off
  mode 1: UPFC in voltage regulation mode
  mode 2: UPFC in reactive power compensation mode
  mode 3: Mode 1 and 2 working together
*/

complex TUPFCObj::GetOutputCurr(int Cond, int ActorID)
{
	complex result = {};
	double Error = 0.0;
	double TError = 0.0;
	double VinMag = 0.0;
	double RefH = 0.0;
	double RefL = 0.0;
	polar Vpolar = {};
	complex vtemp = {};
	complex CurrOut = {};
	try
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			UPFCON = true;
		}
		VinMag = cabs(Vbin);
		if((VinMag > VHLimit) || (VinMag < VLLimit))   // Check Limits (Voltage)
		{
			UPFCON = false;
			CurrOut = cmplx(0, 0);
		}
		else
                                                       // Limits OK
		{
			switch(ModeUPFC)
			{
				case 	0:
				{
					CurrOut = cmplx(0, 0); //UPFC off
				}
				break;              //UPFC as a voltage regulator
				case 	1:
				{
					Vpolar = ctopolar(Vbout);
					Error = Abs(1 - Abs(Vpolar.mag / (VRef * 1000)));
					if(Error > Tol1)
					{
						vtemp = csub(Vbout, Vbin);
						Vpolar = ctopolar(Vbin);
						TError = (VRef * 1000) - Vpolar.mag;
						if(TError > Vpqmax)
							TError = Vpqmax;
						else
						{
							if(TError <  - Vpqmax)
								TError = -Vpqmax;
						}
						Vpolar = topolar(TError, Vpolar.ang);
						vtemp = csub(ptocomplex(Vpolar), vtemp); //Calculates Vpq
						CurrOut = cadd((Sr0)[Cond - 1], cdiv(vtemp, cmplx(0, Xs)));
						(Sr0)[Cond - 1] = CurrOut;
					}
					else
					{
						CurrOut = (Sr0)[Cond - 1];
					}
				}
				break;
				case 	2:
				CurrOut = cmplx(0, 0);
				break; //UPFC as a phase angle regulator
              //UPFC in Dual mode Voltage and Phase angle regulator
				case 	3:
				{
					Vpolar = ctopolar(Vbout);
					Error = Abs(1 - Abs(Vpolar.mag / (VRef * 1000)));
					if(Error > Tol1)
					{
						vtemp = csub(Vbout, Vbin);
						Vpolar = ctopolar(Vbin);
						TError = (VRef * 1000) - Vpolar.mag;
						if(TError > Vpqmax)
							TError = Vpqmax;
						else
						{
							if(TError <  - Vpqmax)
								TError = -Vpqmax;
						}
						Vpolar = topolar(TError, Vpolar.ang);
						vtemp = csub(ptocomplex(Vpolar), vtemp); //Calculates Vpq
						CurrOut = cadd((Sr0)[Cond - 1], cdiv(vtemp, cmplx(0, Xs)));
						(Sr0)[Cond - 1] = CurrOut;
						SyncFlag = false;
					}
					else
					{
						CurrOut = (Sr0)[Cond - 1];
						SyncFlag = true;
					}
				}
				break;                // Double reference control mode (only voltage control)
				case 	4:
				{
					Vpolar = ctopolar(Vbin);       // Takes the input voltage to verify the operation
              // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
					RefH = (VRef * 1000) + (VRef * 1000 * Tol1);
					RefL = (VRef2 * 1000) - (VRef2 * 1000 * Tol1);
					if((Vpolar.mag > RefH) || (Vpolar.mag < RefL))
                // Sets the New reference by considering the value at the input of the device
					{
						if(Vpolar.mag > RefH)
							VRefD = VRef;
						else
						{
							if(Vpolar.mag < RefL)
								VRefD = VRef2;
                // Starts the control routine for voltage control only
						}
						Vpolar = ctopolar(Vbout);
						Error = Abs(1 - Abs(Vpolar.mag / (VRefD * 1000)));
						if(Error > Tol1)
						{
							vtemp = csub(Vbout, Vbin);
							Vpolar = ctopolar(Vbin);
							TError = (VRefD * 1000) - Vpolar.mag;
							if(TError > Vpqmax)
								TError = Vpqmax;
							else
							{
								if(TError <  - Vpqmax)
									TError = -Vpqmax;
							}
							Vpolar = topolar(TError, Vpolar.ang);
							vtemp = csub(ptocomplex(Vpolar), vtemp); //Calculates Vpq
							CurrOut = cadd((Sr0)[Cond - 1], cdiv(vtemp, cmplx(0, Xs)));
							(Sr0)[Cond - 1] = CurrOut;
						}
						else
						{
							CurrOut = (Sr0)[Cond - 1];
						}
						SF2 = true;   // Normal control routine
					}
					else
					{
						CurrOut = cmplx(0, 0); //UPFC off
						(Sr0)[Cond - 1] = CurrOut;
						SF2 = false;   // Says to the other controller to do nothing
					}
				}
				break;                // Double reference control mode (Dual mode)
				case 	5:
				{
					Vpolar = ctopolar(Vbin);       // Takes the input voltage to verify the operation
              // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
					RefH = (VRef * 1000) + (VRef * 1000 * Tol1);
					RefL = (VRef2 * 1000) - (VRef2 * 1000 * Tol1);
					if((Vpolar.mag > RefH) || (Vpolar.mag < RefL))
                // Sets the New reference by considering the value at the input of the device
					{
						if(Vpolar.mag > RefH)
							VRefD = VRef;
						else
						{
							if(Vpolar.mag < RefL)
								VRefD = VRef2;
                // Starts standard control (the same as Dual control mode)
						}
						Vpolar = ctopolar(Vbout);
						Error = Abs(1 - Abs(Vpolar.mag / (VRefD * 1000)));
						if(Error > Tol1)
						{
							vtemp = csub(Vbout, Vbin);
							Vpolar = ctopolar(Vbin);
							TError = (VRefD * 1000) - Vpolar.mag;
							if(TError > Vpqmax)
								TError = Vpqmax;
							else
							{
								if(TError <  - Vpqmax)
									TError = -Vpqmax;
							}
							Vpolar = topolar(TError, Vpolar.ang);
							vtemp = csub(ptocomplex(Vpolar), vtemp); //Calculates Vpq
							CurrOut = cadd((Sr0)[Cond - 1], cdiv(vtemp, cmplx(0, Xs)));
							(Sr0)[Cond - 1] = CurrOut;
							SyncFlag = false;
						}
						else
						{
							CurrOut = (Sr0)[Cond - 1];
							SyncFlag = true;
						}
						SF2 = true;   // Normal control routine
					}
					else
					{
						CurrOut = cmplx(0, 0); //UPFC off
						(Sr0)[Cond - 1] = CurrOut;
						SF2 = false;   // Says to the other controller to do nothing
						SyncFlag = false;
					}
				}
				break;
				default:
				DoSimpleMsg("Control mode not regognized for UPFC", 790);
				break;
			}
		}
		result = CurrOut;
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
//============================================================================

complex TUPFCObj::CalcUPFCPowers(int ModeUP, int Cond)
{
	complex result = {};
	switch(ModeUP)
	{
		case 	1:                                                //Dual mode
		{
			IUPFC = cdiv(csub(Vbout, Vbin), cmplx(0, Xs));
//            SOut=cmul(Vbout,conjg(cadd(IUPFC,SR0[Cond])))     // Just if you want to know the power at the output
			result = cnegate(cmul(Vbin, conjg(cadd(IUPFC, (Sr1)[Cond - 1]))));
		}
		break;                                              //StatCOM
		case 	2:
		{
			IUPFC = cdiv(csub(Vbin, Vbout), cmplx(0, Xs));
			result = cmul(Vbin, conjg(IUPFC));
		}
		break;
		default:
		  ;
		break;
	}
	return result;
}


//============================================================================
//Calculates the input current to absorb reactive power from UPFC
/*
        Vbin   Xs  Vbout
     <---*--=======--*--->
         |           |
 I input ^           ^ I output
         |           |
*/

complex TUPFCObj::GetinputCurr(int Cond, int ActorID)
{
	complex result = {};
	complex CurrIn = {};
	complex Ctemp = {};
	double s = 0.0;
	try
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			if(UPFCON)
  /*Get first Phase Current*/
			{
				switch(ModeUPFC)
				{
					case 	0:
					{
						CurrIn = cmplx(0, 0);
						UPFC_Power = CZero;
					}
					break;                     // Voltage regulation mode
					case 	1:
					{
						CurrIn = CZero;
						Ctemp = conjg(cmul(cdiv(Vbout, Vbin), conjg((Sr0)[Cond - 1]))); //Balancing powers
						Losses = CalcUPFCLosses(cabs(Vbin) / (VRef * 1000));
						CurrIn = cnegate(cmplx((Ctemp.re * Losses), Sr0[Cond - 1].im));
//						CurrIn = cnegate(cmplx(Losses * (Sr0)[Cond - 1].re, (Sr0)[Cond - 1].im)); // This change removes power balance, dangerous
						(Sr1)[Cond - 1] = CurrIn;
					}
					break;                    // Reactive compensation mode
					case 	2:
					{
						UPFC_Power = CalcUPFCPowers(2, 0);
						s = Abs(UPFC_Power.re) / PF;
						QIdeal = UPFC_Power.im - sqrt(1 - PF * PF) * s;   //This is the expected compensating reactive power
						if(QIdeal > (KVARLim * 1000))
							QIdeal = KVARLim * 1000;
						CurrIn = conjg(cdiv(cmplx(0, QIdeal), Vbin)); //Q in terms of current  *** conjg
					}
					break;                    // Dual mode
					case 	3:
					{
						CurrIn = CZero;
						Ctemp = conjg(cmul(cdiv(Vbout, Vbin), conjg((Sr0)[Cond - 1]))); //Balancing powers
						Losses = CalcUPFCLosses(cabs(Vbin) / (VRef * 1000));

						
						CurrIn = cnegate(cmplx((Ctemp.re * Losses), Sr0[Cond - 1].im));
//						CurrIn = cnegate(cmplx(Losses * (Sr0)[Cond - 1].re, (Sr0)[Cond - 1].im)); // This change removes power balance, dangerous
						(Sr1)[Cond - 1] = CurrIn;
						if(SyncFlag)
                    // Starts Power Calculations to copensate the reactive power
						{
							UPFC_Power = CalcUPFCPowers(1, Cond);
							s = Abs(UPFC_Power.re) / PF;
							QIdeal = UPFC_Power.im - sqrt(1 - PF * PF) * s;   //This is the expected compensating reactive power
							if(QIdeal > (KVARLim * 1000))
								QIdeal = KVARLim * 1000;
							CurrIn = cadd(conjg(cdiv(cmplx(0, QIdeal), Vbin)), (Sr1)[Cond - 1]); //Q in terms of current  *** conjg
                    // This partial result is added to the one obtained previously to balance the control loop
						}
					}
					break;                   // Two band reference Mode   (Only Voltage control mode)
					case 	4:
					{
						if(SF2)    // Normal control routine considering the dynamic reference
						{
							CurrIn = CZero;
							Ctemp = conjg(cmul(cdiv(Vbout, Vbin), conjg((Sr0)[Cond - 1]))); //Balancing powers
							Losses = CalcUPFCLosses(cabs(Vbin) / (VRefD * 1000));
							CurrIn = cnegate(cmplx((Ctemp.re * Losses), Sr0[Cond - 1].im));
//							CurrIn = cnegate(cmplx(Losses * (Sr0)[Cond - 1].re, (Sr0)[Cond - 1].im)); // This change removes power balance, dangerous
							(Sr1)[Cond - 1] = CurrIn;
						}
						else
   // Do nothing, aparently the input voltage is OK
						{
							CurrIn = cmplx(0, 0);
							(Sr0)[Cond - 1] = CurrIn;
							UPFC_Power = CZero;
						}
					}
					break;                    // Two band reference mode (Dual control mode)
					case 	5:
					{
						if(SF2)
						{
							CurrIn = CZero;
							Ctemp = conjg(cmul(cdiv(Vbout, Vbin), conjg((Sr0)[Cond - 1]))); //Balancing powers
							Losses = CalcUPFCLosses(cabs(Vbin) / (VRefD * 1000));
							CurrIn = cnegate(cmplx((Ctemp.re * Losses), Sr0[Cond - 1].im));
//							CurrIn = cnegate(cmplx(Losses * (Sr0)[Cond - 1].re, (Sr0)[Cond - 1].im)); // This change removes power balance, dangerous
							(Sr1)[Cond - 1] = CurrIn;
						}
						else
   // Do nothing, aparently the input voltage is OK
						{
							CurrIn = CZero;
							(Sr1)[Cond - 1] = CurrIn;
							UPFC_Power = CZero;
						}
                    //Always corrects PF
						if(SyncFlag)
                    // Starts Power Calculations to compensate the reactive power
						{
							UPFC_Power = CalcUPFCPowers(1, Cond);
							s = Abs(UPFC_Power.re) / PF;
							QIdeal = UPFC_Power.im - sqrt(1 - PF * PF) * s;   //This is the expected compensating reactive power
							if(QIdeal > (KVARLim * 1000))
								QIdeal = KVARLim * 1000;
							CurrIn = cadd(conjg(cdiv(cmplx(0, QIdeal), Vbin)), (Sr1)[Cond - 1]); //Q in terms of current  *** conjg
                    // This partial result is added to the one obtained previously to balance the control loop
						}
					}
					break;
					default:
					  ;
					break;
				}
			}
			else
			CurrIn = cmplx(0, 0);
		}
		result = CurrIn;
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
//===========================================================================


/*Fill Up an array of injection currents*/

void TUPFCObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		int stop = 0;
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			if(!ADiakoptics || (ActorID == 1))
			{
				Vbin = (with0->NodeV)[(NodeRef)[i - 1]];           //Gets voltage at the input of UPFC Cond i
				Vbout = (with0->NodeV)[(NodeRef)[i + Fnphases - 1]];  //Gets voltage at the output of UPFC Cond i
			}
			else
			{
				Vbin = with0->VoltInActor1((NodeRef)[i - 1]);           //Gets voltage at the input of UPFC Cond i
				Vbout = with0->VoltInActor1((NodeRef)[i + Fnphases - 1]);  //Gets voltage at the output of UPFC Cond i      
			}

//    These functions were modified to follow the UPFC Dynamic
//    (Different from VSource)
			(Curr)[i + Fnphases - 1] = OutCurr[i - 1];
			(Curr)[i - 1] = InCurr[i - 1];
		}
	}
}

//===========================================================================
//|     Checks if the monitored pf is out of range, returns true if so      |
//===========================================================================

bool TUPFCObj::CheckPFStatus(int ActorID)
{
	double mypf, S;
	complex MonPower;
	bool result = false;
	if (myElm != nullptr)
	{
		MonPower = myElm->Get_Power(1, ActorID);
		S = sqrt(MonPower.re * MonPower.re + MonPower.im * MonPower.im);
		mypf = MonPower.re / S;
		mypf = abs(PF - mypf);    // calculates the difference to the target
		result = (mypf / PF) > Tol1;
	}
	return result;
}

//===========================================================================
//|     Checks if the UPFC control needs an update, returns true if so      |
//===========================================================================

bool TUPFCObj::CheckStatus(int ActorID)
{
	bool result = false;
	int i = 0;
	double Error = 0.0;
	double TError = 0.0;
	double VinMag = 0.0;
	double RefH = 0.0;
	double RefL = 0.0;
	polar Vpolar = {};
	complex vtemp = {};
	complex CurrOut = {};
	result = false;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		UPFCON = true;
	}
	VinMag = cabs(Vbin);
	if((VinMag > VHLimit) || (VinMag < VLLimit))   // Check Limits (Voltage)
	{
		UPFCON = false;
		CurrOut = cmplx(0, 0);
	}
	else
                                                       // Limits OK
	{
		switch(ModeUPFC)
		{
			case 	0:
			{
				// UPFC off, CurrOut = cmplx(0, 0) was not used
			}
			break;              //UPFC as a voltage regulator
			case 	1:
			{
				Vpolar = ctopolar(Vbout);
				Error = Abs(1 - Abs(Vpolar.mag / (VRef * 1000)));
				if(Error > Tol1)
					result = true;
			}
			break;
			case 	2:
				result = CheckPFStatus(ActorID);
			break; //UPFC as a phase angle regulator
              //UPFC in Dual mode Voltage and Phase angle regulator
			case 	3:
			{
				Vpolar = ctopolar(Vbout);
				Error = Abs(1 - Abs(Vpolar.mag / (VRef * 1000)));
				if(Error > Tol1)
					result = true;
				else
					result = CheckPFStatus(ActorID);
			}
			break;                // Double reference control mode (only voltage control)
			case 	4:
			{
				Vpolar = ctopolar(Vbin);       // Takes the input voltage to verify the operation
          // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
				RefH = (VRef * 1000) + (VRef * 1000 * Tol1);
				RefL = (VRef2 * 1000) - (VRef2 * 1000 * Tol1);
				if((Vpolar.mag > RefH) || (Vpolar.mag < RefL))
            // Sets the New reference by considering the value at the input of the device
				{
					if(Vpolar.mag > RefH)
						VRefD = VRef;
					else
					{
						if(Vpolar.mag < RefL)
							VRefD = VRef2;
            // Starts the control routine for voltage control only
					}
					Vpolar = ctopolar(Vbout);
					Error = Abs(1 - Abs(Vpolar.mag / (VRefD * 1000)));
					if(Error > Tol1)
						result = true;
				}
			}
			break;                // Double reference control mode (Dual mode)
			case 	5:
			{
				Vpolar = ctopolar(Vbin);       // Takes the input voltage to verify the operation
          // Verifies if the Voltage at the input is out of the gap defined with VRef and VRef2
				RefH = (VRef * 1000) + (VRef * 1000 * Tol1);
				RefL = (VRef2 * 1000) - (VRef2 * 1000 * Tol1);
				if((Vpolar.mag > RefH) || (Vpolar.mag < RefL))
            // Sets the New reference by considering the value at the input of the device
				{
					if(Vpolar.mag > RefH)
						VRefD = VRef;
					else
					{
						if(Vpolar.mag < RefL)
							VRefD = VRef2;
            // Starts standard control (the same as Dual control mode)
					}
					Vpolar = ctopolar(Vbout);
					Error = Abs(1 - Abs(Vpolar.mag / (VRefD * 1000)));
					if(Error > Tol1)
						result = true;   // In case we need a control action
					else
						result = CheckPFStatus(ActorID);
				}
			}
			break;
			default:
			  ;
			break;
		}
	}
	return result;
}


//===========================================================================
//|      Uploads the calculated currents into memeory for further use       |
//===========================================================================

void TUPFCObj::UploadCurrents(int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		OutCurr[i - 1] = GetOutputCurr(i, ActorID);
		InCurr[i - 1] = GetinputCurr(i, ActorID);
	}
}

//===========================================================================

void TUPFCObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	try
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			int stop = 0;
			ComputeVterminal(ActorID);
			YPrim->MVmult(Curr, &(Vterminal[0]));            // Current from Elements in System Y
			GetInjCurrents(&(ComplexBuffer[0]), ActorID);   // Get present value of inj currents
//       Add Together  with yprim currents
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

void TUPFCObj::DumpProperties(TTextRec& f, bool Complete)
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
        // Writeln(F,'VMag=',VMag:0:2);
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

void TUPFCObj::InitPropertyValues(int ArrayOffset)
{


     /*PropertyValue Allocated in DSSObject.Create*/
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,GetBus(2));
	Set_PropertyValue(3,"0.24");
	Set_PropertyValue(4,"1");
	Set_PropertyValue(5,Format("%d", Round(ActiveCircuit[ActiveActor]->Fundamental)));
	Set_PropertyValue(6,"3");
	Set_PropertyValue(7,"0.7540");  // 2mH inductance
	Set_PropertyValue(8,"0.02");
	Set_PropertyValue(9,"1");
	Set_PropertyValue(10,"24");
	Set_PropertyValue(11,"");
	inherited::InitPropertyValues(NumPropsThisClass);
}

//=============================================================================

String TUPFCObj::GetPropertyValue(int Index)
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
		result = Format("%-.5g", VRef);
		break;
		case 	4:
		result = Format("%-.5g", PF);
		break;
		case 	5:
		result = Format("%-.5g", Freq);
		break;
		case 	7:
		result = Format("%-.5g", Xs);
		break;
		case 	8:
		result = Format("%-.5g", Tol1);
		break;
		case 	9:
		result = Format("%d", ModeUPFC);
		break;
		case 	10:
		result = Format("%-.5g", Vpqmax);
		break;
		case 	propLossCurve:
		result = LossCurve;
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

/*Var
        S:String;
*/

void TUPFCObj::MakePosSequence(int ActorID)
{

 /*
        S :='Phases=1 ';
        S := S + Format('BasekV=%-.5g ', [kVbase/SQRT3]);
        S := S + Format('R1=%-.5g ', [R1]);
        S := S + Format('X1=%-.5g ', [X1]);

        Parser.CmdString := S;
        Edit;

        inherited;
 */
}

// ======================== BEGIN STATE VARIABLES ===============================

int TUPFCObj::NumVariables()
{
	int result = 0;
	result = NumUPFCVariables;
	return result;
}

void TUPFCObj::Set_Variable(int i, double Value)
{

  // inherited;
	switch(i)
	{
		case 	1:
		ModeUPFC = (int) Round(Value);
		break;
		case 	2:
		;
		break; // can't set this one  -readonly
		case 	3:
		;
		break; // can't set this one  -readonly
		case 	4:
		;
		break; // can't set this one  -readonly
		case 	5:
		;
		break; // can't set this one  -readonly
		case 	6:
		;
		break; // can't set this one  -readonly
		case 	7:
		;
		break; // can't set this one  -readonly
		case 	8:
		;
		break; // can't set this one  -readonly
		case 	9:
		;
		break; // can't set this one  -readonly
		case 	10:
		;
		break; // can't set this one  -readonly
		case 	11:
		(Sr0)[1 - 1].re = Value;
		break;
		case 	12:
		(Sr0)[1 - 1].im = Value;
		break;
		case 	13:
		(Sr1)[1 - 1].re = Value;
		break;
		case 	14:
		(Sr1)[1 - 1].im = Value;
		break;
		default:
		  ;
		break;
	}
}

double TUPFCObj::Get_Variable(int i)
{
	double result = 0.0;
	result = -1.0;
	switch(i)
	{
		case 	1:
		result = (double) ModeUPFC;
		break;
		case 	2:
		result = cabs(IUPFC);
		break;
		case 	3:
		result = Vbin.re;
		break;
		case 	4:
		result = Vbin.im;
		break;
		case 	5:
		result = Vbout.re;
		break;
		case 	6:
		result = Vbout.im;
		break;
		case 	7:
		result = Losses;
		break;
		case 	8:
		result = UPFC_Power.re;
		break;
		case 	9:
		result = UPFC_Power.im;
		break;
		case 	10:
		result = QIdeal;
		break;
		case 	11:
		result = (Sr0)[1 - 1].re;
		break;
		case 	12:
		result = (Sr0)[1 - 1].im;
		break;
		case 	13:
		result = (Sr1)[1 - 1].re;
		break;
		case 	14:
		result = (Sr1)[1 - 1].im;
		break;
		default:
		  ;
		break;
	}
	return result;
}

void TUPFCObj::GetAllVariables(pDoubleArray States)
{
	int i = 0;
	int stop = 0;
	for(stop = NumUPFCVariables, i = 1; i <= stop; i++)
	{
		(States)[i - 1] = Get_Variable(i);
	}
}

String TUPFCObj::VariableName(int i)
{
	String result;
	if(i < 1)
		return result;  // Someone goofed
	switch(i)
	{
		case 	1:
		result = "ModeUPFC";
		break;
		case 	2:
		result = "IUPFC";
		break;
		case 	3:
		result = "Re{Vbin}";
		break;
		case 	4:
		result = "Im{Vbin}";
		break;
		case 	5:
		result = "Re{Vbout}";
		break;
		case 	6:
		result = "Im{Vbout}";
		break;
		case 	7:
		result = "Losses";
		break;
		case 	8:
		result = "P_UPFC";
		break;
		case 	9:
		result = "Q_UPFC";
		break;
		case 	10:
		result = "Qideal";
		break;
		case 	11:
		result = "Re{Sr0^[1]}";
		break;
		case 	12:
		result = "Im{Sr0^[1]}";
		break;
		case 	13:
		result = "Re{Sr1^[1]}";
		break;
		case 	14:
		result = "Im{Sr1^[1]}";
		break;
		default:
		  ;
		break;
	}
	return result;
}

// ======================== END STATE VARIABLES ===============================


void UPFC_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
}

		class 		UPFC_unit
		{
		public:
		UPFC_unit()
		{
			//AssertSystemInitialization();
			UPFC_initialization();
		}
		};
		UPFC_unit _UPFC_unit;

}  // namespace UPFC




