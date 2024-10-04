
#pragma hdrstop

#include "VSConverter.h"

#include "Circuit.h"
#include "DSSGlobals.h"

using namespace std;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Dynamics;
using namespace MyDSSClassDefs;
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace VSConverter
{

TVSConverterObj::TVSConverterObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TVSConverterObj::TVSConverterObj(String ClassName) : inherited(ClassName) {}
TVSConverterObj::TVSConverterObj() {}


TVSConverterObj* ActiveVSConverterObj = nullptr;
const int NumPropsThisClass = 19;
const int VSC_FIXED = 0;
const int VSC_PACVAC = 1;
const int VSC_PACQAC = 2;
const int VSC_VDCVAC = 3;
const int VSC_VDCQAC = 4;

// =====================================================
// Class Methods
// =====================================================

TVSConverter::TVSConverter()
{
	;
	Class_Name = "VSConverter";
	DSSClassType = VS_CONVERTER + PC_ELEMENT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

TVSConverter::~TVSConverter()
{
	// inherited::Destroy();
}


void TVSConverter::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();
	AllocatePropertyArrays();
	PropertyName[1 - 1] = "phases";
	PropertyName[2 - 1] = "Bus1";
	PropertyName[3 - 1] = "kVac";
	PropertyName[4 - 1] = "kVdc";
	PropertyName[5 - 1] = "kW";
	PropertyName[6 - 1] = "Ndc";
	PropertyName[7 - 1] = "Rac";
	PropertyName[8 - 1] = "Xac";
	PropertyName[9 - 1] = "m0";
	PropertyName[10 - 1] = "d0";
	PropertyName[11 - 1] = "Mmin";
	PropertyName[12 - 1] = "Mmax";
	PropertyName[13 - 1] = "Iacmax";
	PropertyName[14 - 1] = "Idcmax";
	PropertyName[15 - 1] = "Vacref";
	PropertyName[16 - 1] = "Pacref";
	PropertyName[17 - 1] = "Qacref";
	PropertyName[18 - 1] = "Vdcref";
	PropertyName[19 - 1] = "VscMode";
	PropertyHelp[1 - 1] = "Number of AC plus DC conductors. Default is 4. AC phases numbered before DC conductors.";
	PropertyHelp[2 - 1] = "Name of converter bus, containing both AC and DC conductors. Bus2 is always ground.";
	PropertyHelp[3 - 1] = "Nominal AC line-neutral voltage in kV. Must be specified > 0.";
	PropertyHelp[4 - 1] = "Nominal DC voltage in kV. Must be specified > 0.";
	PropertyHelp[5 - 1] = "Nominal converter power in kW. Must be specified > 0.";
	PropertyHelp[6 - 1] = "Number of DC conductors. Default is 1. DC conductors numbered after AC phases.";
	PropertyHelp[7 - 1] = String("AC resistance (ohms) for the converter transformer, plus any series reactors. Default is 0.") + CRLF
	           + "Must be 0 for Vac control mode.";
	PropertyHelp[8 - 1] = String("AC reactance (ohms) for the converter transformer, plus any series reactors. Default is 0.") + CRLF
	           + "Must be 0 for Vac control mode. Must be >0 for PacVac, PacQac or VacVdc control mode.";
	PropertyHelp[9 - 1] = "Fixed or initial value of the modulation index. Default is 0.5.";
	PropertyHelp[10 - 1] = "Fixed or initial value of the power angle in degrees. Default is 0.";
	PropertyHelp[11 - 1] = "Minimum value of modulation index. Default is 0.1.";
	PropertyHelp[12 - 1] = "Maximum value of modulation index. Default is 0.9.";
	PropertyHelp[13 - 1] = "Maximum value of AC line current, per-unit of nominal. Default is 2.";
	PropertyHelp[14 - 1] = "Maximum value of DC current, per-unit of nominal. Default is 2.";
	PropertyHelp[15 - 1] = String("Reference AC line-to-neutral voltage, RMS Volts. Default is 0.") + CRLF
	           + "Applies to PacVac and VdcVac control modes, influencing m.";
	PropertyHelp[16 - 1] = String("Reference total AC real power, Watts. Default is 0.") + CRLF
	           + "Applies to PacVac and PacQac control modes, influencing d.";
	PropertyHelp[17 - 1] = String("Reference total AC reactive power, Vars. Default is 0.") + CRLF
	           + "Applies to PacQac and VdcQac control modes, influencing m.";
	PropertyHelp[18 - 1] = String("Reference DC voltage, Volts. Default is 0.") + CRLF
	           + "Applies to VdcVac control mode, influencing d.";
	PropertyHelp[19 - 1] = "Control Mode (Fixed|PacVac|PacQac|VdcVac|VdcQac). Default is Fixed.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();
}

int TVSConverter::NewObject(const String ObjName)
{
	int result = 0;
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TVSConverterObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

void TVSConverter::VscSetBus1(const String s)
{
	String S2;
	int i = 0;
	int dotpos = 0;
	/*# with ActiveVSConverterObj do */
	{
		auto with0 = ActiveVSConverterObj;
		int stop = 0;
		with0->SetBus(1, s);
		dotpos = Pos(".", s);
		if(dotpos > 0)
			S2 = s.substr(0, dotpos - 1);
		else
			S2 = s.substr(0, s.size());
		for(stop = with0->Fnphases, i = 1; i <= stop; i++)
		{
			S2 = S2 + ".0";
		}
		with0->SetBus(2, S2); // default setting for Bus2=Bus1.0.0.0.0
	}
}

int TVSConverter::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	String tok;
	result = 0;
	ActiveVSConverterObj = (TVSConverterObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveVSConverterObj);  // use property to set this value
	/*# with ActiveVSConverterObj do */
	{
		auto with0 = ActiveVSConverterObj;
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
	           + "\"", 350);
				break;
				case 	1:
					if(with0->Fnphases != Parser[ActorID]->MakeInteger_())
					{
						with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
						with0->Set_Nconds(with0->Fnphases);
						ActiveCircuit[ActorID]->Set_BusNameRedefined(true);
					}
				break;
				case 	2:
					VscSetBus1(Param);
				break;
				case 	3:
					with0->FkVac = Parser[ActorID]->MakeDouble_();
				break;
				case 	4:
					with0->FkVdc = Parser[ActorID]->MakeDouble_();
				break;
				case 	5:
					with0->FkW = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
					with0->FNdc = Parser[ActorID]->MakeInteger_();
				break;
				case 	7:
					with0->FRac = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
					with0->FXac = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
					with0->FM = Parser[ActorID]->MakeDouble_();
				break;
				case 	10:
					with0->FD = Parser[ActorID]->MakeDouble_();
				break;
				case 	11:
					with0->FMinM = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
					with0->FMaxM = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
					with0->FMaxIac = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
					with0->FMaxIdc = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
					with0->FrefVac = Parser[ActorID]->MakeDouble_();
				break;
				case 	16:
					with0->FrefPac = Parser[ActorID]->MakeDouble_();
				break;
				case 	17:
					with0->FrefQac = Parser[ActorID]->MakeDouble_();
				break;
				case 	18:
					with0->FrefVdc = Parser[ActorID]->MakeDouble_();
				break;
				case 	19:
				{
					tok = UpperCase( Param.substr(4) );
					if(CompareStr( tok.substr(1), "F") == 0)
						with0->FMode = VSC_FIXED;
					else
					{
						if(CompareStr(tok, "PACV") == 0)
							with0->FMode = VSC_PACVAC;
						else
						{
							if(CompareStr(tok, "PACQ") == 0)
								with0->FMode = VSC_PACQAC;
							else
							{
								if(CompareStr(tok, "VDCV") == 0)
									with0->FMode = VSC_VDCVAC;
								else
								{
									if(CompareStr(tok, "VDCQ") == 0)
										with0->FMode = VSC_VDCQAC;
									else
										with0->FMode = VSC_FIXED;
								}
							}
						}
					}
				}
				break;
				default:
				inherited::ClassEdit(ActiveVSConverterObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 1: case 2: case 3: case 4: case 5: case 6: case 7: case 8: case 9: case 10:
				 case 11: case 12: case 13: case 14: case 15: case 16:
				with0->Set_YprimInvalid(ActorID,true);
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

int TVSConverter::MakeLike(const String VSCName)
{
	int result = 0;
	TVSConverterObj* OtherVSC = nullptr;
	int i = 0;
	result = 0;
	OtherVSC = ((TVSConverterObj*) Find(VSCName));
	if(OtherVSC != nullptr)
		/*# with ActiveVSConverterObj do */
		{
			auto with0 = ActiveVSConverterObj;
			int stop = 0;
			if(with0->Fnphases != OtherVSC->Fnphases)
			{
				with0->Fnphases = OtherVSC->Fnphases;
				with0->Fnterms = OtherVSC->Fnterms;
				with0->Set_Nconds(with0->Fnphases);
				with0->FNdc = OtherVSC->FNdc;
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
				with0->FkVac = OtherVSC->FkVac;
				with0->FkVdc = OtherVSC->FkVdc;
				with0->FkW = OtherVSC->FkW;
				with0->FRac = OtherVSC->FRac;
				with0->FXac = OtherVSC->FXac;
				with0->FM = OtherVSC->FM;
				with0->FD = OtherVSC->FD;
				with0->FMinM = OtherVSC->FMinM;
				with0->FMaxM = OtherVSC->FMaxM;
				with0->FMaxIac = OtherVSC->FMaxIac;
				with0->FMaxIdc = OtherVSC->FMaxIdc;
				with0->FrefVac = OtherVSC->FrefVac;
				with0->FrefPac = OtherVSC->FrefPac;
				with0->FrefQac = OtherVSC->FrefQac;
				with0->FrefVdc = OtherVSC->FrefVdc;
				with0->FMode = OtherVSC->FMode;
			}
			with0->BaseFrequency = OtherVSC->BaseFrequency;
			ClassMakeLike(OtherVSC);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherVSC->Get_PropertyValue(i));
			}
			result = 1;
		} // with
	else
		DoSimpleMsg(String("Error in VSConverter MakeLike: \"") + VSCName
	           + "\" Not Found.", 351);
	return result;
}

int TVSConverter::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TVSConverter.Init", -1);
	result = 0;
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      Object Methods
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TVSConverterObj::TVSConverterObj(TDSSClass* ParClass, const String FaultName)
 : inherited(ParClass),
			FkVac(1.0),
			FkVdc(1.0),
			FkW(1.0),
			FM(0.5),
			FD(0.0),
			FRac(0.0),
			FXac(0.0),
			FrefVac(0.0),
			FrefVdc(0.0),
			FrefPac(0.0),
			FrefQac(0.0),
			FMinM(0.0),
			FMaxM(0.0),
			FMaxIac(0.0),
			FMaxIdc(0.0),
			FMode(0),
			FNdc(0),
			LastCurrents(nullptr)
{
	DSSObjType = ParClass->DSSClassType;
	Set_Name(LowerCase(FaultName));
	LastCurrents = nullptr;

  // typically the first 3 "phases" are AC, and the last one is DC
	Set_NPhases(4);
	Fnconds = 4;
	Set_NTerms(2); // two-terminal device, like the voltage source
	FNdc = 1;
	FMode = VSC_FIXED;
	FRac = EPSILON;
	FXac = 0.0;
	FrefVac = 0.0;
	FrefPac = 0.0;
	FrefQac = 0.0;
	FrefVdc = 0.0;
	FMinM = 0.1;
	FMaxM = 0.9;
	FMaxIac = 2.0;
	FMaxIdc = 2.0;
	InitPropertyValues(0);
	Yorder = Fnterms * Fnconds;
	RecalcElementData(ActiveActor);
}

TVSConverterObj::~TVSConverterObj()
{
	free(LastCurrents);
	//inherited::~TPCElement();
}


void TVSConverterObj::RecalcElementData(int ActorID)
{
	int i = 0;
	int stop = 0;
	if((FRac == 0.0) && (FXac == 0.0))
		FRac = EPSILON;
	InjCurrent = (pComplexArray) realloc(InjCurrent, sizeof(complex) * Yorder);
	LastCurrents = (pComplexArray)realloc(LastCurrents, sizeof(complex) * Yorder);
	for(stop = Yorder, i = 1; i <= stop; i++)
	{
		(LastCurrents)[i - 1] = CZero;
	}
}

void TVSConverterObj::CalcYPrim(int ActorID)
{
	complex Value = {};
	complex Value2 = {};
	double FreqMultiplier = 0.0;
	int i = 0;
// build YPrim_Series non-zero for just the AC phases, and it will be diagonal
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

  // calculate the AC voltage source admittance
	FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
	FreqMultiplier = FYprimFreq / BaseFrequency;
	Value.re = FRac;
	Value.im = FXac * FreqMultiplier;
	Value = cinv(Value);
	Value2 = cnegate(Value);
	/*# with YPrim_Series do */
	{
		auto with0 = YPrim_Series;
		int stop = 0;
		for(stop = (Fnphases - FNdc), i = 1; i <= stop; i++)
		{
			with0->SetElement(i, i, Value);
			with0->SetElement(i + Fnphases, i + Fnphases, Value);
			with0->SetElemsym(i, i + Fnphases, Value2);
		}
	}
	YPrim->CopyFrom(YPrim_Series);
	TDSSCktElement::CalcYPrim(ActorID); // may open some conductors
	Set_YprimInvalid(ActorID,false);
}

int TVSConverterObj::InjCurrents(int ActorID)
{
	int result = 0;
	GetInjCurrents(InjCurrent, ActorID);
	result = inherited::InjCurrents(ActorID); // Add into system array
	return result;
}

void TVSConverterObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	try
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			int stop = 0;
			ComputeVterminal(ActorID);
      // add the injection currents from both AC and DC nodes, to the
      // currents from Yprim elements, which should be zero at the DC nodes
			YPrim->MVmult(Curr, &(Vterminal[0]));
			GetInjCurrents(&(ComplexBuffer[0]), ActorID);
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				(Curr)[i - 1] = csub((Curr)[i - 1], (ComplexBuffer)[i - 1]);
				(LastCurrents)[i - 1] = (Curr)[i - 1];
			}
		}
	}
	catch (std::exception &e)
	{
		DoErrorMsg((String("GetCurrents for Element: ") + get_Name() + "."), (std::string) e.what(), "Inadequate storage allotted for circuit element.", 327);
	}
}

void TVSConverterObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	complex Vmag = {};
	complex Vdc = {};
	complex Sphase = {};
	complex Stotal = {};
	double Pac = 0.0;
	double Deg = 0.0;
	double Idc = 0.0;
	double Idclim = 0.0;
	double Iaclim = 0.0;
	double Itmag = 0.0;
	int i = 0;
	int Nac = 0;

   /* AC Voltage source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |Vsource  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   */
	int stop = 0;
	Nac = Fnphases - FNdc;
	Idclim = FMaxIdc * FkW / FkVdc;
	Iaclim = FMaxIac * FkW / FkVac / Nac;

  // obtain the terminal control quantities
	ComputeVterminal(ActorID);
	set_ITerminalUpdated(false, ActorID);
	GetTerminalCurrents(&(Iterminal[0]), ActorID);
//  for i := 1 to Nac do begin
//    Itmag := cabs(Iterminal^[i]);
//    if Itmag > Iaclim then begin
//      Itmag := Iaclim / Itmag;
//      Iterminal^[i].re := Iterminal^[i].re * Itmag;
//      Iterminal^[i].im := Iterminal^[i].im * Itmag;
//    end;
//  end;

  // do the AC voltage source injection - dependent voltage sources kept in ComplexBuffer
	Vdc = (Vterminal)[Fnphases - 1];
	if((Vdc.re == 0.0) && (Vdc.im == 0.0))
		Vdc.re = 1000.0 * FkVdc;
	Vmag = cmulreal(Vdc, 0.353553 * FM);
	RotatePhasorDeg(Vmag, 1.0, FD);
	(ComplexBuffer)[1 - 1] = Vmag;
	Deg = -360.0 / Nac;
	for(stop = Nac, i = 2; i <= stop; i++)
	{
		RotatePhasorDeg(Vmag, 1.0, Deg);
		(ComplexBuffer)[i - 1] = Vmag;
	}
	(ComplexBuffer)[Fnphases - 1] = CZero;
	YPrim->MVmult(Curr, &(ComplexBuffer[0]));

  // calculate the converter AC power, exclusive of the losses, using LastCurrents
	Stotal.re = 0.0;
	Stotal.im = 0.0;
	for(stop = Nac, i = 1; i <= stop; i++)
	{
//    Sphase := Cmul (ComplexBuffer^[i], Conjg(LastCurrents^[i]));
		Sphase = cmul((ComplexBuffer)[i - 1], conjg((Iterminal)[i - 1]));
		Stotal = cadd(Stotal, Sphase);
	}
	Pac = Stotal.re;
//  Qac := Stotal.im;
	if(Pac == 0.0)
		Pac = 1000.0 * FkW;

  // DC current source injection
	Idc = Pac / cabs(Vdc);
	if(Idc > Idclim)
		Idc = Idclim;
	if(Idc <  - Idclim)
		Idc = -Idclim;
	(Curr)[Fnphases - 1] = cmplx(Idc, 0.0);
	(Curr)[2 * Fnphases - 1] = cmplx(-Idc, 0.0);
	set_ITerminalUpdated(false, ActorID);
}

void TVSConverterObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		{ Write(f, "~ "); Write(f, with0->PropertyName[1 - 1]); Write(f, L'='); WriteLn(f, Fnphases, 0); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[2 - 1]); Write(f, L'='); WriteLn(f, Get_FirstBus()); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[3 - 1]); Write(f, L'='); WriteLn(f, FkVac, 8, 1); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[4 - 1]); Write(f, L'='); WriteLn(f, FkVdc, 8, 1); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[5 - 1]); Write(f, L'='); WriteLn(f, FkW, 8, 1); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[6 - 1]); Write(f, L'='); WriteLn(f, FNdc, 0); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[7 - 1]); Write(f, L'='); WriteLn(f, FRac, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[8 - 1]); Write(f, L'='); WriteLn(f, FXac, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[9 - 1]); Write(f, L'='); WriteLn(f, FM, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[10 - 1]); Write(f, L'='); WriteLn(f, FD, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[11 - 1]); Write(f, L'='); WriteLn(f, FMinM, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[12 - 1]); Write(f, L'='); WriteLn(f, FMaxM, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[13 - 1]); Write(f, L'='); WriteLn(f, FMaxIac, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[14 - 1]); Write(f, L'='); WriteLn(f, FMaxIdc, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[15 - 1]); Write(f, L'='); WriteLn(f, FrefVac, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[16 - 1]); Write(f, L'='); WriteLn(f, FrefPac, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[17 - 1]); Write(f, L'='); WriteLn(f, FrefQac, 0, 4); }
		{ Write(f, "~ "); Write(f, with0->PropertyName[18 - 1]); Write(f, L'='); WriteLn(f, FrefVdc, 0, 4); }
		switch(FMode)
		{
			case 	VSC_FIXED:
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[19 - 1]); WriteLn(f, "= Fixed"); }
			break;
			case 	VSC_PACVAC:
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[19 - 1]); WriteLn(f, "= PacVac"); }
			break;
			case 	VSC_PACQAC:
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[19 - 1]); WriteLn(f, "= PacQac"); }
			break;
			case 	VSC_VDCVAC:
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[19 - 1]); WriteLn(f, "= VdcVac"); }
			break;
			case 	VSC_VDCQAC:
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[19 - 1]); WriteLn(f, "= VdcQac"); }
			break;
			default:
			  ;
			break;
		}
		for(stop = with0->NumProperties, i = NumPropsThisClass + 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
}

void TVSConverterObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"4");
	Set_PropertyValue(2,GetBus(1));
	Set_PropertyValue(3,"1");
	Set_PropertyValue(4,"1");
	Set_PropertyValue(5,"1");
	Set_PropertyValue(6,"1");
	Set_PropertyValue(7,"0");
	Set_PropertyValue(8,"0");
	Set_PropertyValue(9,"0.5");
	Set_PropertyValue(10,"0");
	Set_PropertyValue(11,"0.1");
	Set_PropertyValue(12,"0.9");
	Set_PropertyValue(13,"0");
	Set_PropertyValue(14,"0");
	Set_PropertyValue(15,"0");
	Set_PropertyValue(16,"0");
	Set_PropertyValue(17,"0");
	Set_PropertyValue(18,"0");
	Set_PropertyValue(19,"FIXED");
	inherited::InitPropertyValues(NumPropsThisClass);
}

String TVSConverterObj::GetPropertyValue(int Index)
{
	String result;
	switch(Index)
	{
		case 	1:
		result = Format("%d", Get_NPhases());
		break;
		case 	2:
		result = GetBus(1);
		break;
		case 	3:
		result = Format("%.8g", FkVac);
		break;
		case 	4:
		result = Format("%.8g", FkVdc);
		break;
		case 	5:
		result = Format("%.8g", FkW);
		break;
		case 	6:
		result = Format("%d", FNdc);
		break;
		case 	7:
		result = Format("%.8g", FRac);
		break;
		case 	8:
		result = Format("%.8g", FXac);
		break;
		case 	9:
		result = Format("%.8g", FM);
		break;
		case 	10:
		result = Format("%.8g", FD);
		break;
		case 	11:
		result = Format("%.8g", FMinM);
		break;
		case 	12:
		result = Format("%.8g", FMaxM);
		break;
		case 	13:
		result = Format("%.8g", FMaxIac);
		break;
		case 	14:
		result = Format("%.8g", FMaxIdc);
		break;
		case 	15:
		result = Format("%.8g", FrefVac);
		break;
		case 	16:
		result = Format("%.8g", FrefPac);
		break;
		case 	17:
		result = Format("%.8g", FrefQac);
		break;
		case 	18:
		result = Format("%.8g", FrefVdc);
		break;
		case 	19:
		switch(FMode)
		{
			case 	VSC_FIXED:
			result = "Fixed";
			break;
			case 	VSC_PACVAC:
			result = "PacVac";
			break;
			case 	VSC_PACQAC:
			result = "PacQac";
			break;
			case 	VSC_VDCVAC:
			result = "VdcVac";
			break;
			case 	VSC_VDCQAC:
			result = "VdcQac";
			break;
			default:
			  ;
			break;
		}
		break;
		default:
		result = TDSSCktElement::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TVSConverterObj::MakePosSequence(int ActorID)
{
	if(Fnphases != 2)
	{
		Parser[ActorID]->SetCmdString("Phases=2");
		Edit(ActorID);
		Parser[ActorID]->SetCmdString("Ndc=1");
		Edit(ActorID);
	}
	TDSSCktElement::MakePosSequence(ActorID);
}




}  // namespace VSConverter




