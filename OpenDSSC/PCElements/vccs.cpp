

#pragma hdrstop

#include "vccs.h"

#include "Circuit.h"
#include "DSSGlobals.h"
#include "Utilities.h"
#include "Solution.h"

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
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace Solution;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XYCurve;
using namespace Utilities;

namespace VCCS
{

TVCCSObj::TVCCSObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TVCCSObj::TVCCSObj(String ClassName) : inherited(ClassName) {}
TVCCSObj::TVCCSObj() {}


TVCCSObj* ActiveVCCSObj = nullptr;
TVCCS* VCCSClass = nullptr;
int NumPropsThisClass = 0;
complex Alpha1 = {};
complex Alpha2 = {};

// helper functions for ring buffer indexing, 1..len

int MapIdx(int Idx, int Len)
{
	int result = 0;
	while(Idx <= 0)
		Idx = Idx + Len;
	result = Idx % (Len + 1);
	if(result == 0)
		result = 1;
	return result;
}

int OffsetIdx(int Idx, int Offset, int Len)
{
	int result = 0;
	result = MapIdx(Idx + Offset, Len);
	return result;
}  // Creates superstructure for all Line objects

TVCCS::TVCCS()
 : XY_CurveClass((TDSSClass*) GetDSSClassPtr("XYCurve"))
{
	;
	Class_Name = "VCCS";
	DSSClassType = VCCS_ELEMENT + PC_ELEMENT; // participates in dynamics
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	VCCSClass = this;
}

TVCCS::~TVCCS()
{
	// inherited::Destroy();
}


void TVCCS::DefineProperties()
{
	NumPropsThisClass = 13;
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "bus1";
	PropertyName[2 - 1] = "phases";
	PropertyName[3 - 1] = "prated";
	PropertyName[4 - 1] = "vrated";
	PropertyName[5 - 1] = "ppct";
	PropertyName[6 - 1] = "bp1";
	PropertyName[7 - 1] = "bp2";
	PropertyName[8 - 1] = "filter";
	PropertyName[9 - 1] = "fsample";
	PropertyName[10 - 1] = "rmsmode";
	PropertyName[11 - 1] = "imaxpu";
	PropertyName[12 - 1] = "vrmstau";
	PropertyName[13 - 1] = "irmstau";

     // define Property help values
	PropertyHelp[1 - 1] = String("Name of bus to which source is connected.") + CRLF
	           + "bus1=busname"
	           + CRLF
	           + "bus1=busname.1.2.3";
	PropertyHelp[2 - 1] = "Number of phases.  Defaults to 1.";
	PropertyHelp[3 - 1] = "Total rated power, in Watts.";
	PropertyHelp[4 - 1] = "Rated line-to-line voltage, in Volts";
	PropertyHelp[5 - 1] = "Steady-state operating output, in percent of rated.";
	PropertyHelp[6 - 1] = "XYCurve defining the input piece-wise linear block.";
	PropertyHelp[7 - 1] = "XYCurve defining the output piece-wise linear block.";
	PropertyHelp[8 - 1] = "XYCurve defining the digital filter coefficients (x numerator, y denominator).";
	PropertyHelp[9 - 1] = "Sample frequency [Hz} for the digital filter.";
	PropertyHelp[10 - 1] = "True if only Hz is used to represent a phase-locked loop (PLL), ignoring the BP1, BP2 and time-domain transformations. Default is no.";
	PropertyHelp[11 - 1] = "Maximum output current in per-unit of rated; defaults to 1.1";
	PropertyHelp[12 - 1] = "Time constant in sensing Vrms for the PLL; defaults to 0.0015";
	PropertyHelp[13 - 1] = "Time constant in producing Irms from the PLL; defaults to 0.0015";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // Override help string
	PropertyHelp[NumPropsThisClass + 1 - 1] = "Harmonic spectrum assumed for this source.  Default is \"default\".";
}

int TVCCS::NewObject(const String ObjName)
{
	int result = 0;
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TVCCSObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

int TVCCS::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
  // continue parsing with contents of Parser
	ActiveVCCSObj = (TVCCSObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveVCCSObj);
	result = 0;
	/*# with ActiveVCCSObj do */
	{
		auto with0 = ActiveVCCSObj;
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
				with0->SetBus(1, Param);
				break;
				case 	2:
				{
					with0->Set_NPhases(Parser[ActorID]->MakeInteger_()); // num phases
					with0->Set_Nconds(with0->Fnphases);  // Force Reallocation of terminal info
				}
				break;
				case 	3:
				with0->Prated = Parser[ActorID]->MakeDouble_();
				break;
				case 	4:
				with0->Vrated = Parser[ActorID]->MakeDouble_();
				break;
				case 	5:
				with0->Ppct = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				{
					with0->Fbp1_name = Parser[ActorID]->MakeString_();
					if(with0->Fbp1_name.size() > 0)
					{
						with0->Fbp1 = ((TXYcurveObj*) XY_CurveClass->Find(with0->Fbp1_name));
					}
				}
				break;
				case 	7:
				{
					with0->Fbp2_name = Parser[ActorID]->MakeString_();
					if(with0->Fbp2_name.size() > 0)
					{
						with0->Fbp2 = ((TXYcurveObj*) XY_CurveClass->Find(with0->Fbp2_name));
					}
				}
				break;
				case 	8:
				{
					with0->Ffilter_name = Parser[ActorID]->MakeString_();
					if(with0->Ffilter_name.size() > 0)
					{
						with0->FFilter = ((TXYcurveObj*) XY_CurveClass->Find(with0->Ffilter_name));
					}
				}
				break;
				case 	9:
				with0->FsampleFreq = Parser[ActorID]->MakeDouble_();
				break;
				case 	10:
				with0->FrmsMode = InterpretYesNo(Param);
				break;
				case 	11:
				with0->FmaxIpu = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->FvrmsTau = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->FirmsTau = Parser[ActorID]->MakeDouble_();
				break;
				default:
				inherited::ClassEdit(ActiveVCCSObj, ParamPointer - NumPropsThisClass);
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

int TVCCS::MakeLike(const String OtherSource)
{
	int result = 0;
	TVCCSObj* OtherVCCS = nullptr;
	int i = 0;
	result = 0;
  /*See if we can find this line name in the present collection*/
	OtherVCCS = ((TVCCSObj*) Find(OtherSource));
	if(OtherVCCS != nullptr)
		/*# with ActiveVCCSObj do */
		{
			auto with0 = ActiveVCCSObj;
			int stop = 0;
			if(with0->Fnphases != OtherVCCS->Fnphases)
			{
				with0->Set_NPhases(OtherVCCS->Fnphases);
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->Prated = OtherVCCS->Prated;
			with0->Vrated = OtherVCCS->Vrated;
			with0->Ppct = OtherVCCS->Ppct;
			with0->Fbp1 = OtherVCCS->Fbp1;
			with0->Fbp2 = OtherVCCS->Fbp2;
			with0->FFilter = OtherVCCS->FFilter;
			with0->Fbp1_name = OtherVCCS->Fbp1_name;
			with0->Fbp2_name = OtherVCCS->Fbp2_name;
			with0->Ffilter_name = OtherVCCS->Ffilter_name;
			with0->FsampleFreq = OtherVCCS->FsampleFreq;
			with0->FrmsMode = OtherVCCS->FrmsMode;
			with0->FmaxIpu = OtherVCCS->FmaxIpu;
			with0->FvrmsTau = OtherVCCS->FvrmsTau;
			with0->FirmsTau = OtherVCCS->FirmsTau;
			ClassMakeLike(OtherVCCS); // set spectrum,  base frequency
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherVCCS->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in VCCS MakeLike: \"") + OtherSource + "\" Not Found.", 332);
	return result;
}

int TVCCS::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TVCCS.Init", -1);
	result = 0;
	return result;
}

TVCCSObj::TVCCSObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			Fbp1(nullptr),
			Fbp2(nullptr),
			FFilter(nullptr),
			BaseCurr(0.0),
			BaseVolt(0.0),
			FsampleFreq(0.0),
			Fwinlen(0),
			Ffiltlen(0),
			Irated(0.0),
			Fkv(0.0),
			Fki(0.0),
			FrmsMode(false),
			FmaxIpu(0.0),
			FvrmsTau(0.0),
			FirmsTau(0.0),
			S1(0.0),
			S2(0.0),
			S3(0.0),
			S4(0.0),
			S5(0.0),
			S6(0.0),
			Y2(nullptr),
			Z(nullptr),
			whist(nullptr),
			zlast(nullptr),
			wlast(nullptr),
			sIdxU(0),
			sIdxY(0),
			y2sum(0.0),
			Ppct(0.0),
			Prated(0.0),
			Vrated(0.0)
{
	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(1);
	Fnconds = 1;
	Set_NTerms(1);
	Prated = 250.0;
	Vrated = 208.0;
	Ppct = 100.0;
	FsampleFreq = 5000.0;
	Fkv = 1.0;
	Fki = 1.0;
	FrmsMode = false;
	FmaxIpu = 1.1;
	FvrmsTau = 0.0015;
	FirmsTau = 0.0015;
	Fwinlen = 0;
	Ffilter_name = "";
	Fbp1_name = "";
	Fbp2_name = "";
	Y2 = nullptr;
	Z = nullptr;
	whist = nullptr;
	zlast = nullptr;
	wlast = nullptr;
	InitPropertyValues(0);
	Yorder = Fnterms * Fnconds;
	RecalcElementData(ActiveActor);
}

TVCCSObj::~TVCCSObj()
{
	free(Y2);
	free(Z);
	free(whist);
	free(wlast);
	free(zlast);
	// inherited::Destroy();
}


void TVCCSObj::RecalcElementData(int ActorID)
{
	SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
	if(SpectrumObj == nullptr)
	{
		DoSimpleMsg(String("Spectrum Object \"") + Spectrum
	           + "\" for Device VCCS."
	           + get_Name()
	           + " Not Found.", 333);
	}
	InjCurrent = (pComplexArray) realloc(InjCurrent, sizeof(complex) * Yorder);
	Irated = Prated / Vrated / Fnphases;
	BaseVolt = Vrated;
	if(Fnphases == 3)
	{
		Irated = Irated * sqrt(3.0L);
		BaseVolt = BaseVolt / sqrt(3.0L);
	}
	BaseCurr = 0.01 * Ppct * Irated;
	Fkv = 1.0 / BaseVolt / sqrt(2.0L);
	Fki = BaseCurr * sqrt(2.0L);
	if(Ffilter_name.size() > 0)
	{
		Ffiltlen = FFilter->get_FNumPoints();
		Fwinlen = Trunc(FsampleFreq / BaseFrequency);
		Y2 = (pDoubleArray) realloc(Y2, sizeof(double) * Fwinlen);
		Z = (pDoubleArray)realloc(Z, sizeof(double) * Ffiltlen);
		whist = (pDoubleArray)realloc(whist, sizeof(double) * Ffiltlen);
		wlast = (pDoubleArray)realloc(wlast, sizeof(double) * Ffiltlen);
		zlast = (pDoubleArray)realloc(zlast, sizeof(double) * Ffiltlen);
	}
}

void TVCCSObj::CalcYPrim(int ActorID)
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
/*Sum Currents directly into solution array*/

int TVCCSObj::InjCurrents(int ActorID)
{
	int result = 0;
	GetInjCurrents(InjCurrent, ActorID);
	result = inherited::InjCurrents(ActorID);  // Adds into system array
	return result;
}
/*Total currents into a device*/

void TVCCSObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	try
	{
		int stop = 0;
		GetInjCurrents(&(ComplexBuffer[0]), ActorID);  // Get present value of inj currents
    // Add Together with yprim currents
		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			(Curr)[i - 1] = cnegate((ComplexBuffer)[i - 1]);
		}
	}
	catch (std::exception &e)
	{
		DoErrorMsg((String("GetCurrents for VCCS Element: ") + get_Name() + "."), (std::string) e.what(), "Inadequate storage allotted for circuit element?", 335);
	}
}

void TVCCSObj::UpdateSequenceVoltage()
{
	if(Fnphases == 3)
		sV1 = cdivreal(cadd((Vterminal)[1 - 1], cadd(cmul(Alpha1, (Vterminal)[2 - 1]), cmul(Alpha2, (Vterminal)[3 - 1]))), 3.0);
	else
		sV1 = (Vterminal)[1 - 1];
}

void TVCCSObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	complex I1 = {};
	if(!Get_ConductorClosed(1, ActorID))
	{
		int stop = 0;
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			(Curr)[i - 1] = CZero;
		}
		return;
	}
	ComputeVterminal(ActorID);
	UpdateSequenceVoltage();
//  IterminalUpdated := FALSE;
	if(ActiveSolutionObj->IsDynamicModel)
	{
		if(FrmsMode)
		{
			I1 = pdegtocomplex(S4 * BaseCurr, cdang(sV1));
			switch(Fnphases)
			{
				case 	1:
				(Curr)[1 - 1] = I1;
				break;
				case 	3:
				{
					(Curr)[1 - 1] = I1;
					(Curr)[2 - 1] = cmul(I1, Alpha2);
					(Curr)[3 - 1] = cmul(I1, Alpha1);
				}
				break;
				default:
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					(Curr)[i - 1] = pdegtocomplex(S4 * BaseCurr, cdang((Vterminal)[i - 1]));
				}
				break;
			}
		}
		else
		{
			int stop = 0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				(Curr)[i - 1] = pdegtocomplex(S3 * BaseCurr, cdang((Vterminal)[i - 1]));
			}
		}
	}
	else
	{
		int stop = 0;
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			(Curr)[i - 1] = pdegtocomplex(BaseCurr, cdang((Vterminal)[i - 1]));
		}
	}
}

void TVCCSObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
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
		WriteLn(f);
		WriteLn(f);
	}
}

void TVCCSObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,"1");
	Set_PropertyValue(3,"250");
	Set_PropertyValue(4,"208");
	Set_PropertyValue(5,"100");
	Set_PropertyValue(6,"NONE");
	Set_PropertyValue(7,"NONE");
	Set_PropertyValue(8,"NONE");
	Set_PropertyValue(9,"5000");
	Set_PropertyValue(10,"no");
	Set_PropertyValue(11,"1.1");
	Set_PropertyValue(12,"0.0015");
	Set_PropertyValue(13,"0.0015");
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TVCCSObj::MakePosSequence(int ActorID)
{
	if(Fnphases > 1)
	{
		Parser[ActorID]->SetCmdString("phases=1");
		Edit(ActorID);
	}
	TDSSCktElement::MakePosSequence(ActorID);
}

// support for DYNAMICMODE
// NB: in phasor mode, use load convention for OpenDSS
 // stop injecting if the terminal opens

void TVCCSObj::ShutoffInjections()
{
	int i = 0;
	int stop = 0;
	for(stop = Ffiltlen, i = 1; i <= stop; i++)
	{
		(whist)[i - 1] = 0.0;
		(wlast)[i - 1] = 0.0;
		(Z)[i - 1] = 0.0;
		(zlast)[i - 1] = 0.0;
	}
	for(stop = Fwinlen, i = 1; i <= stop; i++)
	{
		(Y2)[i - 1] = 0.0;
	}
	S1 = 0;
	S2 = 0;
	S3 = 0;
	S4 = 0;
	S5 = 0;
	S6 = 0;
}

void TVCCSObj::InitPhasorStates(int ActorID)
{
	int i = 0;
	int k = 0;
	int stop = 0;
	inherited::ComputeIterminal(ActorID);
	S1 = cabs((Vterminal)[1 - 1]) / BaseVolt;
	S4 = cabs((Iterminal)[1 - 1]) / BaseCurr;
	S2 = S4;
	S3 = S4;
	S5 = 0;
	S6 = 0;
	sV1 = cmplx(1.0, 0.0);
	vlast = cdivreal((Vterminal)[1 - 1], BaseVolt);

  // initialize the history terms for HW model source convention
	for(stop = Ffiltlen, i = 1; i <= stop; i++)
	{
		(whist)[i - 1] = S1;
		(wlast)[i - 1] = S1;
	}
	for(stop = Fwinlen, i = 1; i <= stop; i++)
	{
		k = i - Fwinlen + Ffiltlen;
		if(k > 0)
		{
			(Z)[k - 1] = S4; // HW history with load convention
			(zlast)[k - 1] = (Z)[k - 1];
		}
	}
  // initialize the ring buffer indices; these increment by 1 before actual use
	sIdxU = 0;
	sIdxY = 0;
}

// support for DYNAMICMODE
// NB: The test data and HW model used source convention (I and V in phase)
//     However, OpenDSS uses the load convention

void TVCCSObj::InitStateVars(int ActorID)
{
	double D = 0.0;
	double wt = 0.0;
	double wd = 0.0;
	double Val = 0.0;
	double iang = 0.0;
	double vang = 0.0;
	int i = 0;
	int k = 0;
  // initialize outputs from the terminal conditions
	int stop = 0;
	if(FrmsMode)
	{
		InitPhasorStates(ActorID);
		return;
	}
	inherited::ComputeIterminal(ActorID);
	iang = cang((Iterminal)[1 - 1]);
	vang = cang((Vterminal)[1 - 1]);
	S1 = cabs((Vterminal)[1 - 1]) / BaseVolt;
	S3 = cabs((Iterminal)[1 - 1]) / BaseCurr;
	S2 = S3;
	S4 = S3;
	S5 = 0;
	S6 = 0;
	sV1 = cmplx(1.0, 0.0);
	vlast = cdivreal((Vterminal)[1 - 1], BaseVolt);

  // initialize the history terms for HW model source convention
	D = double(1) / FsampleFreq;
	wd = 2 * DSSGlobals::PI * ActiveSolutionObj->get_FFrequency() * D;
	for(stop = Ffiltlen, i = 1; i <= stop; i++)
	{
		wt = vang - wd * (Ffiltlen - i);
		(whist)[i - 1] = 0;
		(whist)[i - 1] = Fbp1->GetYValue_(S1 * cos(wt));
		(wlast)[i - 1] = (whist)[i - 1];
	}
	for(stop = Fwinlen, i = 1; i <= stop; i++)
	{
		wt = iang - wd * (Fwinlen - i);
		Val = S3 * cos(wt);  // current by passive sign convention
		(Y2)[i - 1] = Val * Val;
		k = i - Fwinlen + Ffiltlen;
		if(k > 0)
		{
			(Z)[k - 1] = -Fbp2->GetXValue(Val); // HW history with generator convention
			(zlast)[k - 1] = (Z)[k - 1];
		}
	}

  // initialize the ring buffer indices; these increment by 1 before actual use
	sIdxU = 0;
	sIdxY = 0;
}

void TVCCSObj::IntegratePhasorStates(int ActorID)
{
	double Vpu = 0.0;
	double ipwr = 0.0;
	double IMax = 0.0;
	double h = 0.0;
	double D = 0.0;
	int iu = 0;
	int i = 0;
	int k = 0;
	int nstep = 0;
	int corrector = 0;
	inherited::ComputeIterminal(ActorID);
	UpdateSequenceVoltage();
	Vpu = cabs(sV1) / BaseVolt;
	if(Vpu > 0.0)
	{
		int stop = 0;
		h = ActiveSolutionObj->DynaVars.h;
		corrector = ActiveSolutionObj->DynaVars.IterationFlag;
		nstep = Trunc(1E-6 + h * FsampleFreq);
    // Vrms from LPF
		D = Vpu - S1;
		S1 = S1 + D * (1.0 - exp(-h / FvrmsTau));
    // rms current to maintain power
		ipwr = BaseCurr / S1;
		IMax = FmaxIpu * Irated;
		if(ipwr > IMax)
			ipwr = IMax;
		S2 = ipwr / BaseCurr;
    // Hout
//    s3 := s2;
		iu = sIdxU;
		for(stop = Ffiltlen, k = 1; k <= stop; k++)
		{
			(Z)[k - 1] = (zlast)[k - 1];
			(whist)[k - 1] = (wlast)[k - 1];
		}
		for(stop = nstep, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			iu = OffsetIdx(iu, 1, Ffiltlen);
			(whist)[iu - 1] = S2;
      // apply the filter and second PWL block
			(Z)[iu - 1] = 0;
			for(stop1 = Ffiltlen, k = 1; k <= stop1; k++)
			{
				(Z)[iu - 1] = (Z)[iu - 1] + FFilter->Get_YValue(k) * (whist)[MapIdx(iu - k + 1, Ffiltlen) - 1];
			}
			for(stop1 = Ffiltlen, k = 2; k <= stop1; k++)
			{
				(Z)[iu - 1] = (Z)[iu - 1] - FFilter->Get_XValue(k) * (Z)[MapIdx(iu - k + 1, Ffiltlen) - 1];
			}
			S3 = (Z)[iu - 1];
		}
    // Irms through LPF
		D = S3 - S4;
		S4 = S4 + D * (1.0 - exp(-h / FirmsTau));
		if(corrector == 1)
		{
			int stop = 0;
			sIdxU = iu;
			for(stop = Ffiltlen, k = 1; k <= stop; k++)
			{
				(zlast)[k - 1] = (Z)[k - 1];
				(wlast)[k - 1] = (whist)[k - 1];
			}
		}
	}
}


// this is called twice per dynamic time step; predictor then corrector

void TVCCSObj::IntegrateStates(int ActorID)
{
	double T = 0.0;
	double h = 0.0;
	double D = 0.0;
	double f = 0.0;
	double W = 0.0;
	double wt = 0.0;
	double vre = 0.0;
	double vim = 0.0;
	double vin = 0.0;
	double Scale = 0.0;
	double Y = 0.0;
	int nstep = 0;
	int i = 0;
	int k = 0;
	int corrector = 0;
	complex vnow = {};
	int iu = 0;
	int IY = 0; // local copies of sIdxU and sIdxY for predictor
	int stop = 0;
	if(!Get_ConductorClosed(1, ActorID))
	{
		ShutoffInjections();
		return;
	}
	if(FrmsMode)
	{
		IntegratePhasorStates(ActorID);
		return;
	}
	inherited::ComputeIterminal(ActorID);
	T = ActiveSolutionObj->DynaVars.T;
	h = ActiveSolutionObj->DynaVars.h;
	f = ActiveSolutionObj->get_FFrequency();
	corrector = ActiveSolutionObj->DynaVars.IterationFlag;
	D = double(1) / FsampleFreq;
	nstep = Trunc(1E-6 + h / D);
	W = 2 * DSSGlobals::PI * f;
	vnow = cdivreal((Vterminal)[1 - 1], BaseVolt);
	vin = 0;
	Y = 0;
	iu = sIdxU;
	IY = sIdxY;
	for(stop = Ffiltlen, k = 1; k <= stop; k++)
	{
		(Z)[k - 1] = (zlast)[k - 1];
		(whist)[k - 1] = (wlast)[k - 1];
	}
	for(stop = nstep, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		iu = OffsetIdx(iu, 1, Ffiltlen);
    // push input voltage waveform through the first PWL block
		Scale = 1.0 * i / nstep;
		vre = vlast.re + (vnow.re - vlast.re) * Scale;
		vim = vlast.im + (vnow.im - vlast.im) * Scale;
		wt = W * (T - h + i * D);
		vin = (vre * cos(wt) + vim * sin(wt));
		(whist)[iu - 1] = Fbp1->GetYValue_(vin);
    // apply the filter and second PWL block
		(Z)[iu - 1] = 0;
		for(stop1 = Ffiltlen, k = 1; k <= stop1; k++)
		{
			(Z)[iu - 1] = (Z)[iu - 1] + FFilter->Get_YValue(k) * (whist)[MapIdx(iu - k + 1, Ffiltlen) - 1];
		}
		for(stop1 = Ffiltlen, k = 2; k <= stop1; k++)
		{
			(Z)[iu - 1] = (Z)[iu - 1] - FFilter->Get_XValue(k) * (Z)[MapIdx(iu - k + 1, Ffiltlen) - 1];
		}
		Y = Fbp2->GetYValue_((Z)[iu - 1]);
    // updating outputs
		if((corrector == 1) && (Abs((int) Y) > S4))
			S4 = Abs((int) Y); // catching the fastest peaks
    // update the RMS
		IY = OffsetIdx(IY, 1, Fwinlen);
		(Y2)[IY - 1] = Y * Y;  // brute-force RMS update
		if(i == nstep)
		{
			int stop1 = 0;
			y2sum = 0.0;
			for(stop1 = Fwinlen, k = 1; k <= stop1; k++)
			{
				y2sum = y2sum + (Y2)[k - 1];
			}
			S3 = sqrt(2.0 * y2sum / Fwinlen); // TODO - this is the magnitude, what about angle?
		}
	}
	if(corrector == 1)
	{
		int stop = 0;
		sIdxU = iu;
		sIdxY = IY;
		vlast = vnow;
		S1 = vin;
		S5 = (whist)[sIdxU - 1];
		S6 = (Z)[sIdxU - 1];
		S2 = Y;
		for(stop = Ffiltlen, k = 1; k <= stop; k++)
		{
			(zlast)[k - 1] = (Z)[k - 1];
			(wlast)[k - 1] = (whist)[k - 1];
		}
	}
}

int TVCCSObj::NumVariables()
{
	int result = 0;
	result = 6;
	return result;
}

void TVCCSObj::GetAllVariables(pDoubleArray States)
{
	int i = 0;
	int stop = 0;
	for(stop = 6, i = 1; i <= stop; i++)
	{
		(States)[i - 1] = Get_Variable(i);
	}  // property maps to Get_Variable below
}

String TVCCSObj::VariableName(int i)
{
	String result;
	result = "";
	if(FrmsMode)
	{
		switch(i)
		{
			case 	1:
			result = "Vrms";
			break;
			case 	2:
			result = "Ipwr";
			break;
			case 	3:
			result = "Hout";
			break;
			case 	4:
			result = "Irms";
			break;
			case 	5:
			result = "NA";
			break;
			case 	6:
			result = "NA";
			break;
			default:
			  ;
			break;
		}
	}
	else
	{
		switch(i)
		{
			case 	1:
			result = "Vwave";
			break;
			case 	2:
			result = "Iwave";
			break;
			case 	3:
			result = "Irms";
			break;
			case 	4:
			result = "Ipeak";
			break;
			case 	5:
			result = "BP1out";
			break;
			case 	6:
			result = "Hout";
			break;
			default:
			  ;
			break;
		}
	}
	return result;
}

double TVCCSObj::Get_Variable(int i)
{
	double result = 0.0;
	result = 0;
	switch(i)
	{
		case 	1:
		result = S1;
		break;
		case 	2:
		result = S2;
		break;
		case 	3:
		result = S3;
		break;
		case 	4:
		result = S4;
		break;
		case 	5:
		result = S5;
		break;
		case 	6:
		result = S6;
		break;
		default:
		  ;
		break;
	}
	return result;
}

void TVCCSObj::Set_Variable(int i, double Value)
{
	switch(i)
	{
		case 	1:
		S1 = Value;
		break;
		case 	2:
		S2 = Value;
		break;
		case 	3:
		S3 = Value;
		break;
		case 	4:
		S4 = Value;
		break;
		case 	5:
		S5 = Value;
		break;
		case 	6:
		S6 = Value;
		break;
		default:
		  ;
		break;
	}
}


void VCCS_initialization()
{
	Alpha1 = cmplx(-0.5, 0.5L * sqrt(3.0L));  // 1 at 120 degrees
	Alpha2 = cmplx(-0.5, -Alpha1.im);       // 1 at 240 degrees
}

		class 		VCCS_unit
		{
		public:
		VCCS_unit()
		{
			//AssertSystemInitialization();
			VCCS_initialization();
		}
		};
		VCCS_unit _VCCS_unit;

}  // namespace VCCS




