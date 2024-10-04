
#pragma hdrstop

#include "GICTransformer.h"

#include "ParserDel.h"
#include "MyDSSClassDefs.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Dynamics.h"
#include "Ucomplex.h"
#include "mathutil.h"
#include "Utilities.h"
#include "Circuit.h"

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
using namespace PDClass;
using namespace PDELement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XYCurve;
using namespace mathutil;
using namespace Utilities;

namespace GICTransformer
{

TGICTransformerObj::TGICTransformerObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TGICTransformerObj::TGICTransformerObj(String ClassName) : inherited(ClassName) {}
TGICTransformerObj::TGICTransformerObj() {}


TGICTransformerObj* ActiveGICTransformerObj = nullptr;
const int NumPropsThisClass = 15;
const int SPEC_GSU = 1;
const int SPEC_AUTO = 2;
const int SPEC_YY = 3;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Fault objects

TGICTransformer::TGICTransformer()
{
	;
	Class_Name = "GICTransformer";
	DSSClassType = GIC_Transformer + PD_ELEMENT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TGICTransformer::~TGICTransformer()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGICTransformer::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "BusH";
	PropertyName[2 - 1] = "BusNH";
	PropertyName[3 - 1] = "BusX";
	PropertyName[4 - 1] = "BusNX";
	PropertyName[5 - 1] = "phases";
	PropertyName[6 - 1] = "Type";
	PropertyName[7 - 1] = "R1";
	PropertyName[8 - 1] = "R2";
	PropertyName[9 - 1] = "KVLL1";
	PropertyName[10 - 1] = "KVLL2";
	PropertyName[11 - 1] = "MVA";
	PropertyName[12 - 1] = "VarCurve";
	PropertyName[13 - 1] = "%R1";
	PropertyName[14 - 1] = "%R2";
	PropertyName[15 - 1] = "K";

     // define Property help values
	PropertyHelp[1 - 1] = String("Name of High-side(H) bus. Examples:") + CRLF
	           + "BusH=busname"
	           + CRLF
	           + "BusH=busname.1.2.3";
	PropertyHelp[2 - 1] = "Name of Neutral bus for H, or first, winding. Defaults to all phases connected "
	           "to H-side bus, node 0, if not specified and transformer type is either GSU or YY. "
	           "(Shunt Wye Connection to ground reference)"
	           "For Auto, this is automatically set to the X bus.";
	PropertyHelp[3 - 1] = "Name of Low-side(X) bus, if type=Auto or YY. ";
	PropertyHelp[4 - 1] = "Name of Neutral bus for X, or Second, winding. Defaults to all phases connected "
	           "to X-side bus, node 0, if not specified. (Shunt Wye Connection to ground reference)";
	PropertyHelp[5 - 1] = "Number of Phases. Default is 3.";
	PropertyHelp[6 - 1] = "Type of transformer: {GSU* | Auto | YY}. Default is GSU.";
	PropertyHelp[7 - 1] = "Resistance, each phase, ohms for H winding, (Series winding, if Auto). Default is 0.0001. If ";
	PropertyHelp[8 - 1] = "Resistance, each phase, ohms for X winding, (Common winding, if Auto). Default is 0.0001. ";
	PropertyHelp[9 - 1] = "Optional. kV LL rating for H winding (winding 1). Default is 500. Required if you are going to export vars for power flow analysis "
	           "or enter winding resistances in percent.";
	PropertyHelp[10 - 1] = "Optional. kV LL rating for X winding (winding 2). Default is 138. Required if you are going to export vars for power flow analysis "
	           "or enter winding resistances in percent..";
	PropertyHelp[11 - 1] = "Optional. MVA Rating assumed Transformer. Default is 100. Used for computing vars due to GIC and "
	           "winding resistances if kV and MVA ratings are specified.";
	PropertyHelp[12 - 1] = "Optional. XYCurve object name. Curve is expected as TOTAL pu vars vs pu GIC amps/phase. Vars are in pu of the MVA property. No Default value. "
	           "Required only if you are going to export vars for power flow analysis. "
	           "See K property.";
	PropertyHelp[13 - 1] = String("Optional. Percent Resistance, each phase, for H winding (1), (Series winding, if Auto). Default is 0.2. ") + CRLF
	           + CRLF
	           + "Alternative way to enter R1 value. It is the actual resistances in ohmns that matter. MVA and kV should be specified.";
	PropertyHelp[14 - 1] = String("Optional. Percent Resistance, each phase, for X winding (2), (Common winding, if Auto). Default is 0.2. ") + CRLF
	           + CRLF
	           + "Alternative way to enter R2 value. It is the actual resistances in ohms that matter. MVA and kV should be specified.";
	PropertyHelp[15 - 1] = String("Mvar K factor. Default way to convert GIC Amps in H winding (winding 1) to Mvar. Default is 2.2. " "Commonly-used simple multiplier for estimating Mvar losses for power flow analysis. ") + CRLF
	           + CRLF
	           + "Mvar = K * kvLL * GIC per phase / 1000 "
	           + CRLF
	           + CRLF
	           + "Mutually exclusive with using the VarCurve property and pu curves."
	           + "If you specify this (default), VarCurve is ignored.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGICTransformer::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TGICTransformerObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGICTransformer::GICTransSetBusH(const String s)
{
	String S2;
	int dotpos = 0;

   // Set Bus2 = BusH1.0.0.0
	/*# with ActiveGICTransformerObj do */
	{
		auto with0 = ActiveGICTransformerObj;
		( (TDSSCktElement*) with0 )->SetBus(1, s);

     // Default Bus2 to zero node of Bus1. (Wye Grounded connection)

     // Strip node designations from S
		dotpos = Pos(".", s);
		if(dotpos > 0)  // copy up to Dot
			S2 = s.substr(0, dotpos - 1);
		else
			S2 = s.substr(0, s.size());
		S2 = S2 + ".0.0.0";     // Set Default for up to 3 phases
		( (TDSSCktElement*) with0 )->SetBus(2, S2);
		with0->IsShunt = true;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGICTransformer::GICTransSetBusX(const String s)
{
	String S2;
	int dotpos = 0;

   // Special handling for Bus X
   // Make sure we have enough terminals defined
   // Set Bus2 = Bus1.0.0.0
	/*# with ActiveGICTransformerObj do */
	{
		auto with0 = ActiveGICTransformerObj;
		if( ( (TDSSCktElement*) with0 )->Get_NTerms() != 4)   // have to have 4 terminals to set this property
		{
			( (TDSSCktElement*) with0 )->Set_NTerms(4);
			( (TDSSCktElement*) with0 )->Set_Nconds(( (TDSSCktElement*) with0 )->Fnphases); // force reallocation of terminals and conductors
		}
		( (TDSSCktElement*) with0 )->SetBus(3, s);

     // Default Bus4 to zero node of Bus3. (Wye Grounded connection)

     // Strip node designations from S
		dotpos = Pos(".", s);
		if(dotpos > 0)  // copy up to Dot
			S2 = s.substr(0, dotpos - 1);
		else
			S2 = s.substr(0, s.size());
		S2 = S2 + ".0.0.0";     // Set Default for up to 3 phases
		( (TDSSCktElement*) with0 )->SetBus(4, S2);
		with0->IsShunt = true;
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGICTransformer::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveGICTransformerObj = (TGICTransformerObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveGICTransformerObj);  // use property to set this value
	/*# with ActiveGICTransformerObj do */
	{
		auto with0 = ActiveGICTransformerObj;
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
				( (TDSSCktElement*) with0 )->Set_PropertyValue(ParamPointer,Param);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + ( (TDSSCktElement*) with0 )->get_Name()
	           + "\"", 350);
				break;
				case 	1:
				GICTransSetBusH(Param);
				break;
				case 	2:
					( (TDSSCktElement*) with0 )->SetBus(2, Param);
				break;
				case 	3:
				GICTransSetBusX(Param);
				break;
				case 	4:
					( (TDSSCktElement*) with0 )->SetBus(4, Param);
				break;
				case 	5:
				;
				break; // see below
				case 	6:
				switch(UpperCase(Param)[0])
				{
					case 	L'G':
					with0->SpecType = SPEC_GSU;
					break;
					case 	L'A':
					with0->SpecType = SPEC_AUTO;
					break;
					case 	L'Y':
					with0->SpecType = SPEC_YY;
					break;
					default:
					  ;
					break;
				}
				break;
				case 	7:
				{
					with0->G1 = Parser[ActorID]->MakeDouble_();
					if(with0->G1 != 0.0)
						with0->G1 = 1.0 / with0->G1;
					else
						with0->G1 = 10000.0;  // Default to a low resistance
				}
				break;
				case 	8:
				{
					with0->G2 = Parser[ActorID]->MakeDouble_();
					if(with0->G2 != 0.0)
						with0->G2 = 1.0 / with0->G2;
					else
						with0->G2 = 10000.0;  // Default to a low resistance
				}
				break;
				case 	9:
				with0->FkV1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	10:
				with0->FkV2 = Parser[ActorID]->MakeDouble_();
				break;
				case 	11:
				with0->FMVARating = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->FVarCurve = Param;
				break;
				case 	13:
				with0->FpctR1 = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->FpctR2 = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				with0->FKFactor = Parser[ActorID]->MakeDouble_();
				break;
           // Inherited
				default:
				inherited::ClassEdit(ActiveGICTransformerObj, ParamPointer - NumPropsThisClass);
				break;
			}

         // Some specials ...
			switch(ParamPointer)
			{
				case 	1:
					( (TDSSCktElement*) with0 )->Set_PropertyValue(2,( (TDSSCktElement*) with0 )->GetBus(2));
				break;  // Bus2 gets modified if bus1 is set
				case 	3:
				{
					( (TDSSCktElement*) with0 )->Set_PropertyValue(4,( (TDSSCktElement*) with0 )->GetBus(4));  // Bus4 gets modified if bus3(X) is set
					if(with0->SpecType == SPEC_AUTO)   // automatically make up series-to-common connection
					{
						( (TDSSCktElement*) with0 )->SetBus(2, ( (TDSSCktElement* )with0 )->GetBus(3));
						( (TDSSCktElement*) with0 )->Set_PropertyValue(2,( (TDSSCktElement*) with0 )->GetBus(2));
					}
				}
				break;
				case 	5:
				if( ( (TDSSCktElement*) with0 )->Fnphases != Parser[ActorID]->MakeInteger_())
				{
					( (TDSSCktElement*) with0 )->Set_NPhases(Parser[ActorID]->MakeInteger_());
					( (TDSSCktElement*) with0 )->Set_Nconds(( (TDSSCktElement*) with0 )->Fnphases);  // Force Reallocation of terminal info if different size
					ActiveCircuit[ActiveActor]->Set_BusNameRedefined(true);  // Set Global Flag to signal circuit to rebuild busdefs
				}
				break;
				case 	6:
				switch(with0->SpecType)
				{
					case 	SPEC_AUTO:
					{
						if( ( (TDSSCktElement*) with0 )->Get_NTerms() == 2)
						{
							( (TDSSCktElement*) with0 )->Set_NTerms(4);
							( (TDSSCktElement*) with0 )->Set_Nconds(( (TDSSCktElement*) with0 )->Fnphases);
						}
						( (TDSSCktElement*) with0 )->SetBus(2, ( (TDSSCktElement*) with0 )->GetBus(3));
					}
					break;
					default:
					  ;
					break;
				}
				break;
				case 7: case 8:
				with0->FpctRSpecified = false;
				break;
				case 9: case 10:
				with0->FkVSpecified = true;
				break;
				case 	12:
				{
					with0->FVarCurveObj = ((TXYcurveObj*) XYCurveClass[ActorID]->Find(with0->FVarCurve));
					with0->KSpecified = false;
				}
				break;
				case 13: case 14:
				with0->FpctRSpecified = true;
				break;
				case 	15:
				with0->KSpecified = true;
				break;
				default:
				  ;
				break;
			}

         //YPrim invalidation on anything that changes impedance values or no. of terminals
			switch(ParamPointer)
			{
				case 3: case 4: case 5: case 6: case 7: case 8:
					( (TDSSCktElement*) with0 )->Set_YprimInvalid(ActorID,true);
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

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGICTransformer::MakeLike(const String GICTransName)
{
	int result = 0;
	TGICTransformerObj* OtherGICTrans = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Fault name in the present collection*/
	OtherGICTrans = ((TGICTransformerObj*) Find(GICTransName));
	if(OtherGICTrans != nullptr)
		/*# with ActiveGICTransformerObj do */
		{
			auto with0 = ActiveGICTransformerObj;
			int stop = 0;
			if(((TDSSCktElement*)with0)->Fnphases != ((TDSSCktElement*)OtherGICTrans)->Fnphases)
			{
				((TDSSCktElement*)with0)->Fnphases = ((TDSSCktElement*)OtherGICTrans)->Fnphases;
				((TDSSCktElement*)with0)->Fnterms = ((TDSSCktElement*)OtherGICTrans)->Fnterms;
				((TDSSCktElement*)with0)->Set_Nconds(((TDSSCktElement*)with0)->Fnphases); // force reallocation of terminals and conductors
				((TDSSCktElement*)with0)->Yorder = ((TDSSCktElement*)with0)->Fnconds * ((TDSSCktElement*)with0)->Fnterms;
				((TDSSCktElement*)with0)->Set_YprimInvalid(ActiveActor,true);
			}
			((TDSSCktElement*)with0)->BaseFrequency = ((TDSSCktElement*)OtherGICTrans)->BaseFrequency;
			with0->G1 = OtherGICTrans->G1;
			with0->G2 = OtherGICTrans->G2;
			with0->SpecType = OtherGICTrans->SpecType;
			with0->FMVARating = OtherGICTrans->FMVARating;
			with0->FVarCurve = OtherGICTrans->FVarCurve;
			with0->FVarCurveObj = OtherGICTrans->FVarCurveObj;
			with0->FkV1 = OtherGICTrans->FkV1;
			with0->FkV2 = OtherGICTrans->FkV2;
			with0->FpctR1 = OtherGICTrans->FpctR1;
			with0->FpctR2 = OtherGICTrans->FpctR2;
			with0->FpctRSpecified = OtherGICTrans->FpctRSpecified;
			with0->FkVSpecified = OtherGICTrans->FkVSpecified;
			with0->FZbase1 = OtherGICTrans->FZbase1;
			with0->FZbase2 = OtherGICTrans->FZbase2;
			with0->FKFactor = OtherGICTrans->FKFactor;
			with0->KSpecified = OtherGICTrans->KSpecified;
			ClassMakeLike(OtherGICTrans);
			for(stop = ((TDSSCktElement*)with0)->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				((TDSSCktElement*)with0)->Set_PropertyValue(i,((TDSSCktElement*)OtherGICTrans)->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in GICTransformer MakeLike: \"") + GICTransName
	           + "\" Not Found.", 351);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGICTransformer::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TGICTransformer.Init", -1);
	result = 0;
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TGICTransformer Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TGICTransformerObj::TGICTransformerObj(TDSSClass* ParClass, const String FaultName)
 : inherited(ParClass),
			G1(10000.0),
			G2(10000.0),
			SpecType(SPEC_GSU),
			FMVARating(100.0),
			FVarCurve(""),
			FVarCurveObj(nullptr),
			FpctR1(0.2),
			FpctR2(0.2),
			FZbase1(0.0),
			FZbase2(0.0),
			FkVSpecified(false),
			FpctRSpecified(false),
			KSpecified(false),
			FKFactor(0.0),
			FkV1(0.0),
			FkV2(0.0)
{
	DSSObjType = ParClass->DSSClassType;
	Set_Name(LowerCase(FaultName));
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(2);  // Force allocation of terminals and conductors
	SetBus(2, (GetBus(1) + ".0"));  // Default to grounded
	IsShunt = true;
	FkVSpecified = false;
	FkV1 = 500.0;
	FkV2 = 138.0;
	FKFactor = 2.2;
	KSpecified = true;
	NormAmps = 0.0;
	EmergAmps = 0.0;
	FaultRate = 0.0;
	PctPerm = 100.0;
	HrsToRepair = 0.0;
	InitPropertyValues(0);
	Yorder = Fnterms * Fnconds;
	FpctRSpecified = true;  // Force computation of G1, G2
	RecalcElementData(ActiveActor);
	FpctRSpecified = false;  // Turn flag off
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TGICTransformerObj::~TGICTransformerObj()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGICTransformerObj::RecalcElementData(int ActorID)
{
	FZbase1 = Sqr(FkV1) / FMVARating;
	FZbase2 = Sqr(FkV2) / FMVARating;
	if(FpctRSpecified)
	{
		G1 = 100.0 / (FZbase1 * FpctR1);
		G2 = 100.0 / (FZbase2 * FpctR1);
	}
	else
	{
		FpctR1 = 100.0 / (FZbase1 * G1);
		FpctR2 = 100.0 / (FZbase2 * G2);
	}
}

//- - - - - - - - VAR EXPORT - - - - - - - - - - - - - - - - - - - -

void TGICTransformerObj::WriteVarOutputRecord(TTextRec& f, int ActorID)
{
	complex Curr = {};
	double MVarMag = 0.0;
	double GICperPhase = 0.0;
	double puCurrMag = 0.0;
	int i = 0;
	int stop = 0;
	ComputeIterminal(ActorID);
	Curr = CZero;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		caccum(Curr, (Iterminal)[i - 1]);
	}
	GICperPhase = cabs(Curr) / Fnphases;
	if(KSpecified)
	{
		MVarMag = FKFactor * FkV1 * GICperPhase / 1000.0;
	}
	else
	{
		if(ASSIGNED(FVarCurveObj))
                // MVA = sqrt(3) * kVLL * I/1000
                // pu A per phase (Avg)
		{
			puCurrMag = GICperPhase / (FMVARating * 1000.0 / FkV1 / SQRT3);
			MVarMag = FVarCurveObj->GetYValue_(puCurrMag) * FMVARating / SQRT2;
		}
		else
		MVarMag = 0.0;
	}
	WriteLn(f, Format("%s, %.8g, %.8g", GetBus(1).c_str(), MVarMag, GICperPhase) );
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGICTransformerObj::CalcYPrim(int ActorID)
{
	complex Value = {};
	complex Value2 = {};
	int i = 0;
	TcMatrix* YPrimTemp = nullptr;
	if(Get_YprimInvalid(ActorID,0))    // Reallocate YPrim if something has invalidated old allocation
	{
		if(YPrim_Series != nullptr)
			delete YPrim_Series;
		YPrim_Series = new TcMatrix(Yorder);
		if(YPrim_Shunt != nullptr)
			delete YPrim_Shunt;
		YPrim_Shunt = new TcMatrix(Yorder);
		if(YPrim != nullptr)
			delete YPrim;
		YPrim = new TcMatrix(Yorder);
	}
	else
	{
		YPrim_Series->Clear(); // zero out YPrim
		YPrim_Shunt->Clear(); // zero out YPrim
		YPrim->Clear();
	}
	if(IsShunt)
		YPrimTemp = YPrim_Shunt;
	else
		YPrimTemp = YPrim_Series;

  // make sure randommult is 1.0 if not solution mode MonteFault
	/*# with YPrimTemp do */
	{
		auto with0 = YPrimTemp;

    /* Now, Put in Yprim matrix */

    /*If the fault is not ON, the set zero conductance*/
		switch(SpecType)
		{
			case 	SPEC_GSU:
			{
				int stop = 0;
				Value = cmplx(G1, 0.0);
				Value2 = cnegate(Value);
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					with0->SetElement(i, i, Value);     // Elements are only on the diagonals
					with0->SetElement(i + Fnphases, i + Fnphases, Value);
					with0->SetElemsym(i, i + Fnphases, Value2);
				}  /*For*/
			}
			break;
                // Terminals 1 and 2
			case 	SPEC_AUTO:
			{
				int stop = 0;
				Value = cmplx(G1, 0.0);
				Value2 = cnegate(Value);
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					with0->SetElement(i, i, Value);     // Elements are only on the diagonals
					with0->SetElement(i + Fnphases, i + Fnphases, Value);
					with0->SetElemsym(i, i + Fnphases, Value2);
				}  /*For*/
                // Terminals 3 and 4
				Value = cmplx(G2, 0.0);
				Value2 = cnegate(Value);
				for(stop = 3 * Fnphases, i = (2 * Fnphases + 1); i <= stop; i++)
				{
					with0->SetElement(i, i, Value);     // Elements are only on the diagonals
					with0->SetElement(i + Fnphases, i + Fnphases, Value);
					with0->SetElemsym(i, i + Fnphases, Value2);
				}  /*For*/
			}
			break;
                // Terminals 1 and 2
			case 	SPEC_YY:
			{
				int stop = 0;
				Value = cmplx(G1, 0.0);
				Value2 = cnegate(Value);
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					with0->SetElement(i, i, Value);     // Elements are only on the diagonals
					with0->SetElement(i + Fnphases, i + Fnphases, Value);
					with0->SetElemsym(i, i + Fnphases, Value2);
				}  /*For*/
                // Terminals 3 and 4
				Value = cmplx(G2, 0.0);
				Value2 = cnegate(Value);
				for(stop = 3 * Fnphases, i = (2 * Fnphases + 1); i <= stop; i++)
				{
					with0->SetElement(i, i, Value);     // Elements are only on the diagonals
					with0->SetElement(i + Fnphases, i + Fnphases, Value);
					with0->SetElemsym(i, i + Fnphases, Value2);
				}  /*For*/
			}
			break;
			default:
			  ;
			break;
		}
	} /*With YPRIM*/
	YPrim->CopyFrom(YPrimTemp);
	inherited::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

void TGICTransformerObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[1 - 1]); Write(f, L'='); WriteLn(f, Get_FirstBus()); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[2 - 1]); Write(f, L'='); WriteLn(f, Get_NextBus()); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[3 - 1]); Write(f, L'='); WriteLn(f, Get_NextBus()); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[4 - 1]); Write(f, L'='); WriteLn(f, Get_NextBus()); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[5 - 1]); Write(f, L'='); WriteLn(f, Fnphases, 0); }
		switch(SpecType)
		{
			case 	SPEC_GSU:
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[6 - 1]); WriteLn(f, "= GSU"); }
			break;
			case 	SPEC_AUTO:
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[6 - 1]); WriteLn(f, "= AUTO"); }
			break;
			case 	SPEC_YY:
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[6 - 1]); WriteLn(f, "= YY"); }
			break;
			default:
			  ;
			break;
		}
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[7 - 1]); Write(f, L'='); WriteLn(f, Format("%.8g", 1.0 / G1)); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[8 - 1]); Write(f, L'='); WriteLn(f, Format("%.8g", 1.0 / G2)); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[9 - 1]); Write(f, L'='); WriteLn(f, FkV1, 0, 2); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[10 - 1]); Write(f, L'='); WriteLn(f, FkV2, 0, 2); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[11 - 1]); Write(f, L'='); WriteLn(f, FMVARating, 0, 2); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[12 - 1]); Write(f, L'='); WriteLn(f, FVarCurve); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[13 - 1]); Write(f, L'='); WriteLn(f, Format("%.8g", FpctR1)); }
		{ Write(f, "~ "); Write(f, (with0->PropertyName)[14 - 1]); Write(f, L'='); WriteLn(f, Format("%.8g", FpctR2)); }
		for(stop = with0->NumProperties, i = NumPropsThisClass + 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
}

void TGICTransformerObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,GetBus(1));
	Set_PropertyValue(2,GetBus(2));
	Set_PropertyValue(3,GetBus(3));
	Set_PropertyValue(4,GetBus(4));
	Set_PropertyValue(5,"3");
	Set_PropertyValue(6,"GSU");
	Set_PropertyValue(7,"0.0001");
	Set_PropertyValue(8,"0.0001");
	Set_PropertyValue(9,"500");
	Set_PropertyValue(10,"138");
	Set_PropertyValue(11,"100");
	Set_PropertyValue(12,"");
	Set_PropertyValue(13,"0.2");
	Set_PropertyValue(14,"0.2");
	Set_PropertyValue(15,"2.2");
	inherited::InitPropertyValues(NumPropsThisClass);

     // Override Inherited Properties
	Set_PropertyValue(NumPropsThisClass + 1,"0");  //Normamps
	Set_PropertyValue(NumPropsThisClass + 2,"0");  //emergamps
	Set_PropertyValue(NumPropsThisClass + 3,"0");  //Fault rate
	Set_PropertyValue(NumPropsThisClass + 4,"0");   // Pct Perm
	Set_PropertyValue(NumPropsThisClass + 5,"0");    // Hrs to repair
}

String TGICTransformerObj::GetPropertyValue(int Index)
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
		result = GetBus(3);
		break;
		case 	4:
		result = GetBus(4);
		break;
		case 	5:
		result = Format("%d", Get_NPhases());
		break;
		case 	7:
		result = Format("%.8g", 1.0 / G1);
		break;
		case 	8:
		result = Format("%.8g", 1.0 / G2);
		break;
		case 	9:
		result = Format("%.8g", FkV1);
		break;
		case 	10:
		result = Format("%.8g", FkV2);
		break;
		case 	11:
		result = Format("%.8g", FMVARating);
		break;
		case 	12:
		result = FVarCurve;
		break;
		case 	13:
		result = Format("%.8g", FpctR1);
		break;
		case 	14:
		result = Format("%.8g", FpctR2);
		break;
		case 	15:
		result = Format("%.8g", FKFactor);
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TGICTransformerObj::MakePosSequence(int ActorID)
{
	if(Fnphases != 1)
	{
		Parser[ActorID]->SetCmdString("Phases=1");
		Edit(ActorID);
	}
	inherited::MakePosSequence(ActorID);
}




}  // namespace GICTransformer




