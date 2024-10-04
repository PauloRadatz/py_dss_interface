
#pragma hdrstop

#include "AutoTrans.h"

#include "Circuit.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Utilities.h"
#include "XfmrCode.h"

using namespace std;
using namespace Arraydef;
using namespace CktElement;
using namespace CktElementClass;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace PDClass;
using namespace PDELement;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace XfmrCode;
using namespace Utilities;

namespace AutoTrans
{

TAutoWinding::TAutoWinding() {}
TAutoTransObj::TAutoTransObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TAutoTransObj::TAutoTransObj(String ClassName) : inherited(ClassName) {}
TAutoTransObj::TAutoTransObj() {}


TAutoTransObj* ActiveAutoTransObj = nullptr;
TAutoTrans* AutoTransClass = nullptr; /*AUTOTRANDEBUG*/
TXfmrCode* XfmrCodeClass = nullptr;
const int NumPropsThisClass = 42;
const int WYE = 0;
const int delta = 1;
const int SERIES = 2;


int XSCSize(int NumWindings)
{
	return ((NumWindings - 1) * NumWindings) / 2;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all AutoTrans objects

TAutoTrans::TAutoTrans()
{
	;
	Class_Name = "AutoTrans";
	DSSClassType = DSSClassType + AUTOTRANS_ELEMENT; // override PDElement   (kept in both actually)
	ActiveElement = 0;
	XfmrCodeClass = nullptr;
	DefineProperties();

     /*Make space for AutoTrans property list*/
	std::string* slc = Slice( (PropertyName), NumProperties);
	CommandList = TCommandList( slc, NumProperties );
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);     /*Allow property list abbreviations*/
	AutoTransClass = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TAutoTrans::~TAutoTrans()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TAutoTrans::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

/* Define Property names  */
	PropertyName[1 - 1] = "phases";
	PropertyName[2 - 1] = "windings";

   // Winding Definition
	PropertyName[3 - 1] = "wdg";
	PropertyName[4 - 1] = "bus";
	PropertyName[5 - 1] = "conn";
	PropertyName[6 - 1] = "kV"; // FOR 2-and 3- phase always kVLL ELSE actual winding KV
	PropertyName[7 - 1] = "kVA";
	PropertyName[8 - 1] = "tap";
	PropertyName[9 - 1] = "%R";
	PropertyName[10 - 1] = "Rdcohms";
	PropertyName[11 - 1] = "Core";

   // General Data
	PropertyName[12 - 1] = "buses";
	PropertyName[13 - 1] = "conns";
	PropertyName[14 - 1] = "kVs";
	PropertyName[15 - 1] = "kVAs";
	PropertyName[16 - 1] = "taps";
	PropertyName[17 - 1] = "XHX";
	PropertyName[18 - 1] = "XHT";
	PropertyName[19 - 1] = "XXT";
	PropertyName[20 - 1] = "XSCarray";  // x12 13 14... 23 24.. 34 ..
	PropertyName[21 - 1] = "thermal";
	PropertyName[22 - 1] = "n";
	PropertyName[23 - 1] = "m";
	PropertyName[24 - 1] = "flrise";
	PropertyName[25 - 1] = "hsrise";
	PropertyName[26 - 1] = "%loadloss";
	PropertyName[27 - 1] = "%noloadloss";
	PropertyName[28 - 1] = "normhkVA";
	PropertyName[29 - 1] = "emerghkVA";
	PropertyName[30 - 1] = "sub";  // =y/n
	PropertyName[31 - 1] = "MaxTap";
	PropertyName[32 - 1] = "MinTap";
	PropertyName[33 - 1] = "NumTaps";
	PropertyName[34 - 1] = "subname";
	PropertyName[35 - 1] = "%imag";
	PropertyName[36 - 1] = "ppm_antifloat";
	PropertyName[37 - 1] = "%Rs";
	PropertyName[38 - 1] = "bank";
	PropertyName[39 - 1] = "XfmrCode";
	PropertyName[40 - 1] = "XRConst";
	PropertyName[41 - 1] = "LeadLag";
	PropertyName[42 - 1] = "WdgCurrents";


     // define Property help values
	PropertyHelp[1 - 1] = "Number of phases this AutoTrans. Default is 3.";
	PropertyHelp[2 - 1] = "Number of windings, this AutoTranss. (Also is the number of terminals) "
	           "Default is 2. This property triggers memory allocation for the AutoTrans and will cause other properties to revert to default values.";
   // Winding Definition
	PropertyHelp[3 - 1] = "Set this = to the number of the winding you wish to define.  Then set "
	           "the values for this winding.  Winding 1 is always the Series winding. "
	           "Winding 2 is always Common winding (wye connected). "
	           "Repeat for each winding.  Alternatively, use "
	           "the array collections (buses, kVAs, etc.) to define the windings.  Note: "
	           "reactances are BETWEEN pairs of windings; they are not the property of a single winding.";
	PropertyHelp[4 - 1] = "Bus connection spec for this winding.";
	PropertyHelp[5 - 1] = String("Connection of this winding {Series, wye*, Delta, LN, LL }. Default is \"wye\" with the neutral solidly grounded. ") + CRLF
	           + "For AutoTrans, Winding 1 is always Series and Winding 2 (the Common winding) is always Wye. "
	           + CRLF
	           + "If only 2 windings, no need to specify connections.";
	PropertyHelp[6 - 1] = "For 2-or 3-phase, enter phase-phase kV rating.  Otherwise, kV rating of the actual winding. "
	           "Specify H terminal kV rating for Series winding.";
	PropertyHelp[7 - 1] = "Base kVA rating of the winding. Side effect: forces change of max normal and emerg kVA ratings."
	           "If 2-winding AutoTrans, forces other winding to same value. "
	           "When winding 1 is defined, all other windings are defaulted to the same rating "
	           "and the first two winding resistances are defaulted to the %loadloss value.";
	PropertyHelp[8 - 1] = "Per unit tap that this winding is on.";
	PropertyHelp[9 - 1] = "Percent ac resistance this winding.  This value is for the power flow model."
	           "Is derived from the full load losses in the transformer test report.";
	PropertyHelp[10 - 1] = "Winding dc resistance in OHMS. Specify this for GIC analysis. From transformer test report (divide by number of phases). "
	           "Defaults to 85% of %R property (the ac value that includes stray losses).";
	PropertyHelp[11 - 1] = "{Shell*|5-leg|3-Leg|1-phase} Core Type. Used for GIC analysis in auxiliary programs. Not used inside OpenDSS.";

   // General Data
	PropertyHelp[12 - 1] = String("Use this to specify all the bus connections at once using an array. Example:") + CRLF
	           + CRLF
	           + "New AutoTrans.T1 buses=[Hbus, Xbus]";
	PropertyHelp[13 - 1] = String("Use this to specify all the Winding connections at once using an array. Example:") + CRLF
	           + CRLF
	           + "New AutoTrans.T1 buses=[Hbus, Xbus] "
	           + "~ conns=(series, wye)";
	PropertyHelp[14 - 1] = String("Use this to specify the kV ratings of all windings at once using an array. Example:") + CRLF
	           + CRLF
	           + "New AutoTrans.T1 buses=[Hbus, Xbus] "
	           + CRLF
	           + "~ conns=(series, wye)"
	           + CRLF
	           + "~ kvs=(115, 12.47)"
	           + CRLF
	           + CRLF
	           + "See kV= property for voltage rules.";
	PropertyHelp[15 - 1] = "Use this to specify the kVA ratings of all windings at once using an array.";
	PropertyHelp[16 - 1] = "Use this to specify the p.u. tap of all windings at once using an array.";
	PropertyHelp[17 - 1] = "Use this to specify the percent reactance, H-L (winding 1 to winding 2).  Use "
	           "for 2- or 3-winding AutoTranss. On the kVA base of winding 1(H-X). See also X12.";
	PropertyHelp[18 - 1] = "Use this to specify the percent reactance, H-T (winding 1 to winding 3).  Use "
	           "for 3-winding AutoTranss only. On the kVA base of winding 1(H-X). See also X13.";
	PropertyHelp[19 - 1] = "Use this to specify the percent reactance, L-T (winding 2 to winding 3).  Use "
	           "for 3-winding AutoTranss only. On the kVA base of winding 1(H-X).  See also X23.";
	PropertyHelp[20 - 1] = String("Use this to specify the percent reactance between all pairs of windings as an array. " "All values are on the kVA base of winding 1.  The order of the values is as follows:") + CRLF
	           + CRLF
	           + "(x12 13 14... 23 24.. 34 ..)  "
	           + CRLF
	           + CRLF
	           + "There will be n(n-1)/2 values, where n=number of windings.";
	PropertyHelp[21 - 1] = "Thermal time constant of the AutoTrans in hours.  Typically about 2.";
	PropertyHelp[22 - 1] = "n Exponent for thermal properties in IEEE C57.  Typically 0.8.";
	PropertyHelp[23 - 1] = "m Exponent for thermal properties in IEEE C57.  Typically 0.9 - 1.0";
	PropertyHelp[24 - 1] = "Temperature rise, deg C, for full load.  Default is 65.";
	PropertyHelp[25 - 1] = "Hot spot temperature rise, deg C.  Default is 15.";
	PropertyHelp[26 - 1] = "Percent load loss at full load. The %R of the High and Low windings (1 and 2) are adjusted to agree at rated kVA loading.";
	PropertyHelp[27 - 1] = "Percent no load losses at rated excitatation voltage. Default is 0. Converts to a resistance in parallel with the magnetizing impedance in each winding.";
	PropertyHelp[28 - 1] = "Normal maximum kVA rating of H winding (winding 1+2).  Usually 100% - 110% of"
	           "maximum nameplate rating, depending on load shape. Defaults to 110% of kVA rating of Winding 1.";
	PropertyHelp[29 - 1] = "Emergency (contingency)  kVA rating of H winding (winding 1+2).  Usually 140% - 150% of"
	           "maximum nameplate rating, depending on load shape. Defaults to 150% of kVA rating of Winding 1.";
	PropertyHelp[30 - 1] = "={Yes|No}  Designates whether this AutoTrans is to be considered a substation." "Default is No.";  // =y/n
	PropertyHelp[31 - 1] = "Max per unit tap for the active winding.  Default is 1.10";
	PropertyHelp[32 - 1] = "Min per unit tap for the active winding.  Default is 0.90";
	PropertyHelp[33 - 1] = "Total number of taps between min and max tap.  Default is 32 (16 raise and 16 lower taps about the neutral position). The neutral position is not counted.";
	PropertyHelp[34 - 1] = "Substation Name. Optional. Default is null. If specified, printed on plots";
	PropertyHelp[35 - 1] = "Percent magnetizing current. Default=0.0. Magnetizing branch is in parallel with windings in each phase. Also, see \"ppm_antifloat\".";
	PropertyHelp[36 - 1] = "Default=1 ppm.  Parts per million of AutoTrans winding VA rating connected to ground to protect against accidentally floating a winding without a reference. "
	           "If positive then the effect is adding a very large reactance to ground.  If negative, then a capacitor.";
	PropertyHelp[37 - 1] = String("Use this property to specify all the winding ac %resistances using an array. Example:") + CRLF
	           + CRLF
	           + "New AutoTrans.T1 buses=[Hibus, lowbus] "
	           + "~ %Rs=(0.2  0.3)";
	PropertyHelp[38 - 1] = "Name of the bank this transformer is part of, for CIM, MultiSpeak, and other interfaces.";
	PropertyHelp[39 - 1] = "Name of a library entry for transformer properties. The named XfmrCode must already be defined.";
	PropertyHelp[40 - 1] = "={Yes|No} Default is NO. Signifies whether or not the X/R is assumed contant for harmonic studies.";
	PropertyHelp[41 - 1] = "{Lead | Lag (default) | ANSI (default) | Euro } Designation in mixed Delta-wye connections the "
	           "relationship between HV to LV winding. Default is ANSI 30 deg lag, e.g., Dy1 of Yd1 vector group. "
	           "To get typical European Dy11 connection, specify either \"lead\" or \"Euro\"";
	PropertyHelp[42 - 1] = "(Read only) Makes winding currents available via return on query (? AutoTrans.TX.WdgCurrents). "
	           "Order: Phase 1, Wdg 1, Wdg 2, ..., Phase 2 ...";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TAutoTrans::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TAutoTransObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);  // Return index of AutoTrans in AutoTrans list
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/*
  A Transf Defaults to 3-phases, 2-windings (both wye)
*/

int TAutoTrans::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	int i = 0;
	String ParamName;  /*For parsing property names*/
	String Param;
  // continue parsing cmdline presently in Parser
	
  /*Make this object the active circuit element*/
	ActiveAutoTransObj = (TAutoTransObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveAutoTransObj);  // use property to set this value
	result = 0;
	/*# with ActiveAutoTransObj do */
	{
		auto with0 = ActiveAutoTransObj;
		with0->XHXChanged = false;
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
	           + "\" for Object \"AutoTrans."
	           + ((TDSSCktElement*)with0)->get_Name()
	           + "\"", 100110);
				break;
				case 	1:
					((TDSSCktElement*)with0)->Set_NPhases(Parser[ActorID]->MakeInteger_());
				break;
				case 	2:
				with0->SetNumWindings(Parser[ActorID]->MakeInteger_());
				break; // Reallocate stuff if bigger
				case 	3:
				SetActiveWinding(Parser[ActorID]->MakeInteger_());
				break;
				case 	4:
				with0->SetBusAuto(with0->ActiveWinding, Param, ActorID);
				break;
				case 	5:
				InterpretAutoConnection(Param);
				break;
				case 	6:
				(with0->WINDING_)[with0->ActiveWinding - 1].kVLL = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				(with0->WINDING_)[with0->ActiveWinding - 1].kVA = Parser[ActorID]->MakeDouble_();
				break;
				case 	8:
				(with0->WINDING_)[with0->ActiveWinding - 1].puTap = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
				(with0->WINDING_)[with0->ActiveWinding - 1].Rpu = Parser[ActorID]->MakeDouble_() * 0.01;
				break;  // %R
				case 	10:
				(with0->WINDING_)[with0->ActiveWinding - 1].RdcOhms = Parser[ActorID]->MakeDouble_();
				break;
				case 	11:
				with0->strCoreType = Param;
				break;
				case 	12:
				InterpretAllBuses(Param, ActorID);
				break;
				case 	13:
				InterpretAllConns(Param);
				break;
				case 	14:
				InterpretAllkVRatings(Param);
				break;
				case 	15:
				InterpretAllkVARatings(Param);
				break;
				case 	16:
				InterpretAllTaps(Param);
				break;
				case 	17:
				with0->puXHX = TrapZero(Parser[ActorID]->MakeDouble_(), 7.0) * 0.01;
				break;
				case 	18:
				with0->puXHT = TrapZero(Parser[ActorID]->MakeDouble_(), 35.0) * 0.01;
				break;
				case 	19:
				with0->puXXT = TrapZero(Parser[ActorID]->MakeDouble_(), 30.0) * 0.01;
				break;
				case 	20:
				Parser[ActorID]->ParseAsVector(XSCSize(with0->NumWindings), (pDoubleArray) (with0->puXSC.data()));
				break;
				case 	21:
				with0->ThermalTimeConst = Parser[ActorID]->MakeDouble_();
				break;
				case 	22:
				with0->n_thermal = Parser[ActorID]->MakeDouble_();
				break;
				case 	23:
				with0->m_thermal = Parser[ActorID]->MakeDouble_();
				break;
				case 	24:
				with0->FLrise = Parser[ActorID]->MakeDouble_();
				break;
				case 	25:
				with0->HSrise = Parser[ActorID]->MakeDouble_();
				break;
				case 	26:
				with0->pctLoadLoss = Parser[ActorID]->MakeDouble_();
				break;
				case 	27:
				with0->pctNoLoadLoss = Parser[ActorID]->MakeDouble_();
				break;
				case 	28:
				with0->NormMaxHkVA = Parser[ActorID]->MakeDouble_();
				break;
				case 	29:
				with0->EmergMaxHkVA = Parser[ActorID]->MakeDouble_();
				break;
				case 	30:
				with0->IsSubstation = InterpretYesNo(Param);
				break;
				case 	31:
				(with0->WINDING_)[with0->ActiveWinding - 1].MaxTap = Parser[ActorID]->MakeDouble_();
				break;
				case 	32:
				(with0->WINDING_)[with0->ActiveWinding - 1].MinTap = Parser[ActorID]->MakeDouble_();
				break;
				case 	33:
				(with0->WINDING_)[with0->ActiveWinding - 1].NumTaps = Parser[ActorID]->MakeInteger_();
				break;
				case 	34:
				with0->SubstationName = Param;
				break;
				case 	35:
				with0->pctImag = Parser[ActorID]->MakeDouble_();
				break;
				case 	36:
				with0->ppm_FloatFactor = Parser[ActorID]->MakeDouble_() * 1.0e-6;
				break;
				case 	37:
				InterpretAllRs(Param);
				break; /*XfmrBank := Param*/
				case 	38:
				;
				break;
				case 	39:
				with0->FetchXfmrCode(Param);
				break;    // Do nothing until we define auto code
				case 	40:
				with0->XRConst = InterpretYesNo(Param);
				break;
				case 	41:
				with0->HVLeadsLV = InterpretLeadLag(Param);
				break;
				case 	42:
				( (TDSSCktElement*) with0 )->Set_PropertyValue(45,"");
				break;  // placeholder, do nothing just throw value away if someone tries to set it.

           // Inherited properties
				default:
				inherited::ClassEdit(ActiveAutoTransObj, ParamPointer - NumPropsThisClass);
				break;
			}

         /*Take care of properties that require some additional work,*/
			switch(ParamPointer)
			{
				case 	1:
					((TDSSCktElement*)with0)->Set_Nconds(2 * ((TDSSCktElement*)with0)->Fnphases);
				break;  // Force redefinition of number of conductors and reallocation of matrices
         // YPrim is built with windings not connected.  Connected in NodeRef
          // default all winding kVAs to first winding so latter Donot have to be specified
				case 	7:
				if(with0->ActiveWinding == 1)
				{
					int stop = 0;
					for(int stop = with0->NumWindings, i = 2; i <= stop; i++)
					{
						(with0->WINDING_)[i - 1].kVA = (with0->WINDING_)[1 - 1].kVA;
					}
					with0->NormMaxHkVA = 1.1 * (with0->WINDING_)[1 - 1].kVA;    // Defaults for new winding rating.
					with0->EmergMaxHkVA = 1.5 * (with0->WINDING_)[1 - 1].kVA;
				}
				else
				{
					if(with0->NumWindings == 2)
					{
						(with0->WINDING_)[1 - 1].kVA = (with0->WINDING_)[2 - 1].kVA;  // For 2-winding, force both kVAs to be same
					}
				}
				break;
           // Update LoadLosskW if winding %r changed. Using only windings 1 and 2
				case 	9:
				with0->pctLoadLoss = ((with0->WINDING_)[1 - 1].Rpu + (with0->WINDING_)[2 - 1].Rpu) * 100.0;
				break;
				case 	10:
				(with0->WINDING_)[with0->ActiveWinding - 1].RdcSpecified = true;
				break;
				case 	11:
				with0->CoreType = InterpretCoreType(Param);
				break; // Assign integer number
				case 	15:
				{
					with0->NormMaxHkVA = 1.1 * (with0->WINDING_)[1 - 1].kVA;    // Defaults for new winding rating.
					with0->EmergMaxHkVA = 1.5 * (with0->WINDING_)[1 - 1].kVA;
				}
				break;
				case 17: case 18: case 19:
				with0->XHXChanged = true;
				break;
				case 	20:
				for(int stop = XSCSize(with0->NumWindings), i = 1; i <= stop; i++)
				{
					(with0->puXSC)[i - 1] = (with0->puXSC)[i - 1] * 0.01;
				}
				break;  // Convert to per unit
    // Assume load loss is split evenly  between windings 1 and 2
				case 	26:
				{
					(with0->WINDING_)[1 - 1].Rpu = with0->pctLoadLoss / 2.0 / 100.0;
					(with0->WINDING_)[2 - 1].Rpu = (with0->WINDING_)[1 - 1].Rpu;
				}
				break;
				case 	37:
				with0->pctLoadLoss = ((with0->WINDING_)[1 - 1].Rpu + (with0->WINDING_)[2 - 1].Rpu) * 100.0;
				break;  // Update
				case 	38:
				DoSimpleMsg("Bank Property not used with AutoTrans object.", 100130);
				break;
				case 	39:
				DoSimpleMsg("XFmrCode Property not used with AutoTrans object.", 100131);
				break;
				default:
				  ;
				break;
			}

         //YPrim invalidation on anything that changes impedance values
			switch(ParamPointer)
			{
				case 5: case 6: case 7: case 8: case 9: case 10: case 11: case 12: case 13: case 14:
				 case 15: case 16: case 17: case 18: case 19:
				( (TDSSCktElement*) with0 )->Set_YprimInvalid(ActorID,true);
				break;
				case 26: case 27:
					((TDSSCktElement*)with0)->Set_YprimInvalid(ActorID,true);
				break;
				case 35: case 36: case 37:
					((TDSSCktElement*)with0)->Set_YprimInvalid(ActorID,true);
				break;
				default:
				  ;
				break;
			}

         /*Advance to next property on input line*/
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		with0->RecalcElementData(ActorID);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TAutoTrans::SetActiveWinding(int W)
{
	/*# with ActiveAutoTransObj do */
	{
		auto with0 = ActiveAutoTransObj;
		if((W > 0) && (W <= with0->NumWindings))
			with0->ActiveWinding = W;
		else
			DoSimpleMsg(String("Wdg parameter invalid for \"") + ((TDSSCktElement*)with0)->get_Name()
	           + "\"", 100112);
	}
}

double TAutoTrans::TrapZero(double Value, double DefaultValue)
{
	double result = 0.0;
	if(Value == 0.0)
	{
		DoSimpleMsg(String("Zero Reactance specified for AutoTrans.") + ( (TDSSCktElement*) ActiveAutoTransObj )->get_Name(), 10011201);
		result = DefaultValue;
	}
	else
	result = Value;
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN

void TAutoTrans::InterpretAutoConnection(const String s)
{
	/*# with ActiveAutoTransObj do */
	{
		auto with0 = ActiveAutoTransObj;
		/*# with with0->WINDING^[ActiveWinding] do */
		{
			
			switch(with0->ActiveWinding)
			{
				case 	1:
				(with0->WINDING_)[with0->ActiveWinding - 1].Connection = SERIES;
				break;  // Get_First() Winding always Series
				case 	2:
				(with0->WINDING_)[with0->ActiveWinding - 1].Connection = WYE;
				break;  // Second Winding is always Common and Wye
				default:
				switch(LowerCase(s)[0])
				{
					case 	L'y': case L'w':
					(with0->WINDING_)[with0->ActiveWinding - 1].Connection = WYE;
					break;  /*Wye*/
					case 	L'd':
					(with0->WINDING_)[with0->ActiveWinding - 1].Connection = delta;
					break;  /*Delta or line-Line*/
					case 	L'l':
					switch(LowerCase(s)[1])
					{
						case 	L'n':
						(with0->WINDING_)[with0->ActiveWinding - 1].Connection = WYE;
						break;
						case 	L'l':
						(with0->WINDING_)[with0->ActiveWinding - 1].Connection = delta;
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
				break;
			}
		}
		((TDSSCktElement*)with0)->Yorder = ((TDSSCktElement*)with0)->Fnconds * ((TDSSCktElement*)with0)->Fnterms;
		((TDSSCktElement*)with0)->Set_YprimInvalid(ActiveActor,true);
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//  routine expecting all winding connections expressed in one array of strings

void TAutoTrans::InterpretAllConns(const String s)
{
	String S1;
	String S2;
	int i = 0;
	AuxParser[ActiveActor]->SetCmdString(s);  // Load up Parser

    /*Loop for no more than the expected number of windings*/
	/*# with ActiveAutoTransObj do */
	{
		auto with0 = ActiveAutoTransObj;
		int stop = 0;
		for(int stop = with0->NumWindings, i = 1; i <= stop; i++)
		{
			with0->ActiveWinding = i;
			S1 = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
			S2 = AuxParser[ActiveActor]->MakeString_();
			if(S2.size() > 0)
				InterpretAutoConnection(S2);
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//  routine expecting all winding bus connections expressed in one array of strings

void TAutoTrans::InterpretAllBuses(const String s, int ActorID)
{
	String BusNam;
	int i = 0;
	AuxParser[ActorID]->SetCmdString(s);  // Load up Parser

    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	/*# with ActiveAutoTransObj do */
	{
		auto with0 = ActiveAutoTransObj;
		int stop = 0;
		for(int stop = with0->NumWindings, i = 1; i <= stop; i++)
		{
			with0->ActiveWinding = i;
			String dummy = AuxParser[ActorID]->GetNextParam(); // ignore any parameter name  not expecting any
			BusNam = AuxParser[ActorID]->MakeString_();
			if(BusNam.size() > 0)
				with0->SetBusAuto(with0->ActiveWinding, BusNam, ActorID);
		}
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//  routine expecting all winding bus connections expressed in one array of strings

bool TAutoTrans::InterpretLeadLag(const String s)
{
	bool result = false;
	result = false;   // default to ANSI 30 Deg Lag if can't understand S
	if(CompareTextShortest(s, "lead") == 0)
		result = true;
	else
	{
		if(CompareTextShortest(s, "euro") == 0)
			result = true;
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//  routine expecting all winding kV ratings expressed in one array of strings

void TAutoTrans::InterpretAllkVRatings(const String s)
{
	String DataStr;
	int i = 0;
	AuxParser[ActiveActor]->SetCmdString(s);  // Load up Parser

    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	/*# with ActiveAutoTransObj do */
	{
		auto with0 = ActiveAutoTransObj;
		int stop = 0;
		for(int stop = with0->NumWindings, i = 1; i <= stop; i++)
		{
			with0->ActiveWinding = i;
			String dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
			DataStr = AuxParser[ActiveActor]->MakeString_();
			if(DataStr.size() > 0)
				(with0->WINDING_)[with0->ActiveWinding - 1].kVLL = AuxParser[ActiveActor]->MakeDouble_();
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//  routine expecting all winding ratings expressed in one array of strings

void TAutoTrans::InterpretAllkVARatings(const String s)
{
	String DataStr;
	int i = 0;
	AuxParser[ActiveActor]->SetCmdString(s);  // Load up Parser

    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	/*# with ActiveAutoTransObj do */
	{
		auto with0 = ActiveAutoTransObj;
		int stop = 0;
		for(int stop = with0->NumWindings, i = 1; i <= stop; i++)
		{
			with0->ActiveWinding = i;
			String dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
			DataStr = AuxParser[ActiveActor]->MakeString_();
			if(DataStr.size() > 0)
				(with0->WINDING_)[with0->ActiveWinding - 1].kVA = AuxParser[ActiveActor]->MakeDouble_();
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//  routine expecting all winding ratings expressed in one array of strings

void TAutoTrans::InterpretAllRs(const String s)
{
	String DataStr;
	int i = 0;
	AuxParser[ActiveActor]->SetCmdString(s);  // Load up Parser

    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	/*# with ActiveAutoTransObj do */
	{
		auto with0 = ActiveAutoTransObj;
		int stop = 0;
		for(int stop = with0->NumWindings, i = 1; i <= stop; i++)
		{
			with0->ActiveWinding = i;
			String dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
			DataStr = AuxParser[ActiveActor]->MakeString_();
			if(DataStr.size() > 0)
				(with0->WINDING_)[with0->ActiveWinding - 1].Rpu = AuxParser[ActiveActor]->MakeDouble_() * 0.01;
		}
	}
}



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//  routine expecting all winding taps expressed in one array of strings

void TAutoTrans::InterpretAllTaps(const String s)
{
	String DataStr;
	int i = 0;
	AuxParser[ActiveActor]->SetCmdString(s);  // Load up Parser

    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	/*# with ActiveAutoTransObj do */
	{
		auto with0 = ActiveAutoTransObj;
		int stop = 0;
		for(int stop = with0->NumWindings, i = 1; i <= stop; i++)
		{
			with0->ActiveWinding = i;
			String dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name,  not expecting any
			DataStr = AuxParser[ActiveActor]->MakeString_();
			if(DataStr.size() > 0)
				(with0->WINDING_)[with0->ActiveWinding - 1].puTap = AuxParser[ActiveActor]->MakeDouble_();
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TAutoTrans::MakeLike(const String AutoTransfName)
{
	int result = 0;
	TAutoTransObj* OtherTransf = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Transf name in the present collection*/
	OtherTransf = ((TAutoTransObj*) Find(AutoTransfName));
	if(OtherTransf != nullptr)
		/*# with ActiveAutoTransObj do */
		{
			auto with0 = ActiveAutoTransObj;
			int stop = 0;
			((TDSSCktElement*)with0)->Set_NPhases(( (TDSSCktElement*) OtherTransf )->Fnphases);
			with0->SetNumWindings(OtherTransf->NumWindings);
			((TDSSCktElement*)with0)->Set_Nconds(2 * ((TDSSCktElement*)with0)->Fnphases); // forces reallocation of terminals and conductors
			((TDSSCktElement*)with0)->Yorder = ((TDSSCktElement*)with0)->Fnconds * ((TDSSCktElement*)with0)->Fnterms;
			((TDSSCktElement*)with0)->Set_YprimInvalid(ActiveActor,true);
			for(int stop = with0->NumWindings, i = 1; i <= stop; i++)
			{
				/*# with with0->WINDING^[i] do */
				{
					
					(with0->WINDING_)[i - 1].Connection = (OtherTransf->WINDING_)[i - 1].Connection;
					(with0->WINDING_)[i - 1].kVLL = (OtherTransf->WINDING_)[i - 1].kVLL;
					with0->kVSeries = OtherTransf->kVSeries;
					(with0->WINDING_)[i - 1].VBase = (OtherTransf->WINDING_)[i - 1].VBase;
					(with0->WINDING_)[i - 1].kVA = (OtherTransf->WINDING_)[i - 1].kVA;
					(with0->WINDING_)[i - 1].puTap = (OtherTransf->WINDING_)[i - 1].puTap;
					(with0->WINDING_)[i - 1].Rpu = (OtherTransf->WINDING_)[i - 1].Rpu;
					(with0->WINDING_)[i - 1].RdcOhms = (OtherTransf->WINDING_)[i - 1].RdcOhms;
					(with0->WINDING_)[i - 1].RdcSpecified = (OtherTransf->WINDING_)[i - 1].RdcSpecified;
             // copy the taps
					(with0->WINDING_)[i - 1].TapIncrement = (OtherTransf->WINDING_)[i - 1].TapIncrement;
					(with0->WINDING_)[i - 1].MinTap = (OtherTransf->WINDING_)[i - 1].MinTap;
					(with0->WINDING_)[i - 1].MaxTap = (OtherTransf->WINDING_)[i - 1].MaxTap;
					(with0->WINDING_)[i - 1].NumTaps = (OtherTransf->WINDING_)[i - 1].NumTaps;
				}
			}
			with0->SetTermRef();
			with0->puXHX = OtherTransf->puXHX;
			with0->puXHT = OtherTransf->puXHT;
			with0->puXXT = OtherTransf->puXXT;
			for(int stop = XSCSize(with0->NumWindings), i = 1; i <= stop; i++)
			{
				(with0->puXSC)[i - 1] = (OtherTransf->puXSC)[i - 1];
			}
			with0->ZB->CopyFrom(OtherTransf->ZB);
			with0->Y_1Volt->CopyFrom(OtherTransf->Y_1Volt);
			with0->Y_Term->CopyFrom(OtherTransf->Y_Term);
			with0->Y_1Volt_NL->CopyFrom(OtherTransf->Y_1Volt_NL);
			with0->Y_Term_NL->CopyFrom(OtherTransf->Y_Term_NL);
			with0->ThermalTimeConst = OtherTransf->ThermalTimeConst;
			with0->n_thermal = OtherTransf->n_thermal;
			with0->m_thermal = OtherTransf->m_thermal;
			with0->FLrise = OtherTransf->FLrise;
			with0->HSrise = OtherTransf->HSrise;
			with0->pctLoadLoss = OtherTransf->pctLoadLoss;
			with0->pctNoLoadLoss = OtherTransf->pctNoLoadLoss;
			with0->NormMaxHkVA = OtherTransf->NormMaxHkVA;
			with0->EmergMaxHkVA = OtherTransf->EmergMaxHkVA;
			with0->XRConst = OtherTransf->XRConst;
			with0->XfmrBank = OtherTransf->XfmrBank;
			with0->XfmrCode = OtherTransf->XfmrCode;
			ClassMakeLike(OtherTransf);
			for(int stop = ((TDSSCktElement*)with0)->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				if(i != 45)
          // skip read only properties
					((TDSSCktElement*)with0)->Set_PropertyValue(i,( (TDSSCktElement*) OtherTransf )->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in AutoTrans MakeLike: \"") + AutoTransfName
	           + "\" Not Found.", 100113);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TAutoTrans::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TAutoTrans.Init", -1);
	result = 0;
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TAutoTrans Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TAutoTransObj::TAutoTransObj(TDSSClass* ParClass, const String TransfName)
 : inherited(ParClass),
			DeltaDirection(1),
			ppm_FloatFactor(0.000001),
			pctImag(0.0),
			XRConst(false),
			NumWindings(0),
			MaxWindings(0),
			puXHX(0.0),
			puXHT(0.0),
			puXXT(0.0),
			ZBase(0.0),
			puXSC(3, 0.0),
			VABase(0.0),
			kVSeries(0.0),
			ZB(nullptr),
			Y_1Volt(nullptr),
			Y_Term(nullptr),
			Y_1Volt_NL(nullptr),
			Y_Term_NL(nullptr),
			Y_Terminal_Freqmult(0.0),
			NormMaxHkVA(0.0),
			EmergMaxHkVA(0.0),
			ThermalTimeConst(0.0),
			n_thermal(0.0),
			m_thermal(0.0),
			FLrise(0.0),
			HSrise(0.0),
			pctLoadLoss(0.0),
			pctNoLoadLoss(0.0),
			HVLeadsLV(false),
			XHXChanged(false),
			ActiveWinding(0),
			IsSubstation(false),
			CoreType(0)
{
	int i = 0;
	int stop = 0;
	Set_Name(LowerCase(TransfName));
	DSSObjType = ParClass->DSSClassType; //DSSObjType + XFMR; // override PDElement   (kept in both actually)
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 2 * Fnphases; // 2 conductors per phase; let NodeRef connect neutral, etc.
	SetNumWindings(2);  // must do this after setting number of phases
	ActiveWinding = 1;
	Set_NTerms(NumWindings);  // Force allocation of terminals and conductors
	puXHX = 0.10;
	puXHT = 0.35;
	puXXT = 0.30;
	XHXChanged = true;  // Set flag to for calc of XSC array from XHL, etc.
	SubstationName = "";
	XfmrBank = "";
	XfmrCode = "";
	VABase = (WINDING_)[1 - 1].kVA * 1000.0;
	ThermalTimeConst = 2.0;
	n_thermal = 0.8;
	m_thermal = 0.8;
	FLrise = 65.0;
	HSrise = 15.0;  // Hot spot rise
	NormMaxHkVA = 1.1 * (WINDING_)[1 - 1].kVA;
	EmergMaxHkVA = 1.5 * (WINDING_)[1 - 1].kVA;
	pctLoadLoss = 2.0 * (WINDING_)[1 - 1].Rpu * 100.0; //  assume two windings for init'ing
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		(WINDING_)[i - 1].ComputeAntiFloatAdder(ppm_FloatFactor, VABase / Fnphases);
	}

  /*Default the no load properties to zero*/
	pctNoLoadLoss = 0.0;
	FaultRate = 0.007;
	IsSubstation = false;
	HVLeadsLV = false; // Defaults to ANSI connection
	Y_Terminal_Freqmult = 0.0;
	Yorder = Fnterms * Fnconds;
	InitPropertyValues(0);
	RecalcElementData(ActiveActor);
}
// Added Jan 3, 2019

void TAutoTransObj::SetBusAuto(int iwdg, const String s, int ActorID)
{
	int NNodes[50/*# range 1..50*/]; // big integer buffer
	int NumNodes = 0;
	int dotpos = 0;
	int FirstNeut = 0; // Get_First() Neutral Node
	int II = 0;
	String strBusName;
	String strNewBusName;
// For winding 2 set all nodes on second end of winding to same as 1st value
// so all neutral ends of common winding get connected to same neutral node
	switch(iwdg)
	{
		case 	2:
		{
			int stop = 0;
			for(int stop = Get_NPhases(), II = 1; II <= stop; II++)
			{
				NNodes[II - 1] = II;
			} // set up buffer with defaults
               // Default all other conductors to a ground connection
               // If user wants them ungrounded, must be specified explicitly!
			for(int stop = Get_NConds(), II = Get_NPhases() + 1; II <= stop; II++)
			{
				NNodes[II - 1] = 0;
			}
			AuxParser[ActorID]->set_Token(s); // load up AuxParser
			strBusName = AuxParser[ActorID]->ParseAsBusName(NumNodes, (pIntegerArray) &NNodes, ActorID);

              // Check for non-zero neutral specification
			if(NNodes[Get_NPhases() + 1 - 1] > 0)
                  // Reconstruct new bus name
			{
				int stop = 0;
				strNewBusName = strBusName;
				for(int stop = Get_NPhases(), II = 1; II <= stop; II++)
				{
					strNewBusName = strNewBusName + Format(".%d", NNodes[II - 1]);
				}
				for(int stop = Get_NConds(), II = Get_NPhases() + 1; II <= stop; II++)
				{
					strNewBusName = strNewBusName + Format(".%d", NNodes[Get_NPhases() + 1 - 1]);
				}
				SetBus(iwdg, strNewBusName);
			}
			else
			SetBus(iwdg, s);
		}
		break;
		default:
		SetBus(iwdg, s);  // all other windings
		break;
	}
}
// Overrides standard function

void TAutoTransObj::SetNodeRef(int iTerm, Arraydef::pIntegerArray NodeRefArray)
{
	int i = 0;
	inherited::SetNodeRef(iTerm, NodeRefArray);

  // Now fixup noderefs for series winding of AutoTrans
  // Warning **** Magic happens here
  // Redefine 2nd node of Series winding to same as first node of 2nd winding (common winding)
	if(iTerm == 2)
	{
		if((WINDING_)[1 - 1].Connection == SERIES)    // If series winding
		{
			int stop = 0;
			for(int stop = Fnphases, i = 1; i <= stop; i++)
			{
				(NodeRef)[Fnphases + i - 1] = (NodeRef)[i + Fnconds - 1];
				(Terminals[iTerm - 1].TermNodeRef)[Fnphases + i - 1] = (NodeRef)[i + Fnconds - 1];
			}
		}
	}
}

void TAutoTransObj::SetNumWindings(int n)
{
	int i = 0;
	int OldWdgSize = 0;
	int NewWdgSize = 0;
	if(n > 1)
	{
		int stop = 0;
		// Free old winding objects
		WINDING_.clear();
		OldWdgSize = XSCSize(NumWindings);
		NumWindings = n;
		MaxWindings = n;
		NewWdgSize = XSCSize(NumWindings);
		Fnconds = 2 * Fnphases;
		Set_NTerms(NumWindings);

		WINDING_.resize(MaxWindings);  // Reallocate collector array
		for(int stop = MaxWindings, i = 1; i <= stop; i++)
		{
			(WINDING_)[i - 1] = TAutoWinding(i);
		}

      // array of short circuit measurements between pairs of windings
		puXSC.resize(NewWdgSize);
		for(int stop = NewWdgSize, i = OldWdgSize + 1; i <= stop; i++)
		{
			(puXSC)[i - 1] = 0.30;
		}
		TermRef.resize(2 * NumWindings * Fnphases);

      /*Reallocate impedance matrices*/
		delete ZB;
		delete Y_1Volt;
		delete Y_1Volt_NL;
		delete Y_Term;
		delete Y_Term_NL;
		ZB = new TcMatrix(NumWindings - 1);
		Y_1Volt = new TcMatrix(NumWindings);
		Y_1Volt_NL = new TcMatrix(NumWindings);
		Y_Term = new TcMatrix(2 * NumWindings);
		Y_Term_NL = new TcMatrix(2 * NumWindings);
	}
	else
	DoSimpleMsg(String("Invalid number of windings: (") + IntToStr(n)
	           + ") for AutoTrans."
	           + get_Name(), 100111);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TAutoTransObj::~TAutoTransObj()
{
    /*Throw away stuff allocated for this object*/

	if (ZB != nullptr)			delete ZB; // ZB->~TcMatrix();
	if (Y_1Volt != nullptr)		delete Y_1Volt; // Y_1Volt->~TcMatrix();
	if (Y_1Volt_NL != nullptr)	delete Y_1Volt_NL; // Y_1Volt_NL->~TcMatrix();
	if (Y_Term != nullptr)		delete Y_Term; // Y_Term->~TcMatrix();
	if (Y_Term_NL != nullptr)	delete Y_Term_NL; // Y_Term_NL->~TcMatrix();
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TAutoTransObj::RecalcElementData(int ActorID)
{
	int i = 0;
	int ihvolt = 0;
	double VFactor = 0.0;

  // Determine Delta Direction
   // If high voltage is delta, delta leads y
   // If high voltage is wye, delta lags wye
	int stop = 0;
	if((WINDING_)[1 - 1].Connection == (WINDING_)[2 - 1].Connection)
		DeltaDirection = 1;
	else
	{
		if((WINDING_)[1 - 1].Connection == SERIES)  // Auto
			DeltaDirection = 1;
		else
		{
			if((WINDING_)[1 - 1].kVLL >= (WINDING_)[2 - 1].kVLL)
				ihvolt = 1;
			else
				ihvolt = 2;
			switch((WINDING_)[ihvolt - 1].Connection)
			{
				case 	WYE:
				if(HVLeadsLV)
					DeltaDirection = -1;
				else
					DeltaDirection = 1;
				break;
				case 	delta:
				if(HVLeadsLV)
					DeltaDirection = 1;
				else
					DeltaDirection = -1;
				break;
         // ---old code --- If Winding^[2].Connection = 0 Then DeltaDirection := -1 Else DeltaDirection := 1;
				default:
				  ;
				break;
			}
		}
	}
	SetTermRef();   // Re-establish TermRef IF num windings or connection changed
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		/*# with WINDING^[i] do */
		{
			
			if((WINDING_)[i - 1].NumTaps > 0)
				(WINDING_)[i - 1].TapIncrement = ((WINDING_)[i - 1].MaxTap - (WINDING_)[i - 1].MinTap) / (WINDING_)[i - 1].NumTaps;
			else
				(WINDING_)[i - 1].TapIncrement = 0.0;
		}
	}
	if(XHXChanged)
     /*should only happen for 2- and 3-winding AutoTranss*/
	{
		if(NumWindings <= 3)
		{
			int stop = 0;
			for(int stop = XSCSize(NumWindings), i = 1; i <= stop; i++)
			{
				switch(i)
				{
					case 	1:
					puXSC[1 - 1] = puXHX;
					break;
					case 	2:
					(puXSC)[2 - 1] = puXHT;
					break;
					case 	3:
					(puXSC)[3 - 1] = puXXT;
					break;
					default:
					  ;
					break;
				}
			}
		}
		XHXChanged = false;
	}

   // Set winding voltage bases (in volts)
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		/*# with WINDING^[i] do */
		{
			
			switch((WINDING_)[i - 1].Connection)
			{  // Get the actual turns voltage base for each winding
				case 	WYE:
				switch(Fnphases)
				{
					case 	2:
					 case 3:   // Wye
					(WINDING_)[i - 1].VBase = (WINDING_)[i - 1].kVLL * InvSQRT3x1000;
					break;   // assume 3-phase for 2-phase designation
					default:
					(WINDING_)[i - 1].VBase = (WINDING_)[i - 1].kVLL * 1000.0;
					break;
				}
				break;
				case 	delta:
				(WINDING_)[i - 1].VBase = (WINDING_)[i - 1].kVLL * 1000.0;
				break;     // delta
                            // Series winding for Auto  Should be Winding[1]
				case 	SERIES:
				{
					switch(Fnphases)
					{
						case 	2:
						 case 3:
						kVSeries = ((WINDING_)[i - 1].kVLL - (WINDING_)[2 - 1].kVLL) / SQRT3;
						break;
						default:
						kVSeries = (WINDING_)[i - 1].kVLL - (WINDING_)[2 - 1].kVLL;
						break;
					}
					if(kVSeries == 0.0)
						kVSeries = (WINDING_)[i - 1].kVLL * 0.0001; // In case series has same voltage as common
					(WINDING_)[i - 1].VBase = kVSeries * 1000.0;
				}
				break;
				default:
				  ;
				break;
			}
		}
	}

   /*Base rating of Winding 1 */
	VABase = (WINDING_)[1 - 1].kVA * 1000.0;

   // Set Rdc parameters for each winding.
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		/*# with WINDING^[i] do */
		{
			
			if((WINDING_)[i - 1].RdcSpecified)
				(WINDING_)[i - 1].Rdcpu = (WINDING_)[i - 1].RdcOhms / (Sqr((WINDING_)[i - 1].VBase) / VABase);
			else
			{
				(WINDING_)[i - 1].Rdcpu = 0.85 * (WINDING_)[i - 1].Rpu; // default to 85% of the ac value (does not include stray loss)
				(WINDING_)[i - 1].RdcOhms = (WINDING_)[i - 1].Rdcpu * Sqr((WINDING_)[i - 1].VBase) / VABase;
			}
		}
	}
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		(WINDING_)[i - 1].ComputeAntiFloatAdder(ppm_FloatFactor, VABase / Fnphases);
	}

   /* Normal and Emergency terminal current Rating for UE check*/
	VFactor = 1.0;  // ensure initialization
	switch((WINDING_)[1 - 1].Connection)
	{
		case 	WYE:
		VFactor = (WINDING_)[1 - 1].VBase * 0.001;
		break;   // wye
		case 	delta:
		switch(Fnphases)
		{
			case 	1:
			VFactor = (WINDING_)[1 - 1].VBase * 0.001;
			break;
			case 	2:
			 case 3:
			VFactor = (WINDING_)[1 - 1].VBase * 0.001 / SQRT3;
			break;
			default:
			VFactor = (WINDING_)[1 - 1].VBase * 0.001 * 0.5 / sin(double(DSSGlobals::PI) / Fnphases);
			break;
		}
		break;
		case 	2:
		VFactor = (WINDING_)[1 - 1].VBase * 0.001;
		break;   // Series Winding
		default:
		  ;
		break;
	}

     /*Divide per phase kVA by voltage to neutral*/
	NormAmps = NormMaxHkVA / Fnphases / VFactor;
	EmergAmps = EmergMaxHkVA / Fnphases / VFactor;
	CalcY_Terminal(1.0, ActorID);   // Calc Y_Terminal at base frequency
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

/*Override standard SaveWrite*/
/*AutoTrans structure not conducive to standard means of saving*/

void TAutoTransObj::SaveWrite(TTextRec& f)
{
	int iProp = 0;
	int i = 0;
   /*Write only properties that were explicitly set in the
   final order they were actually set*/
	iProp = GetNextPropertySet(0); // Works on ActiveDSSObject
	while(iProp > 0)
	{
		/*# with ParentClass do */
		{
			auto with0 = ParentClass;
			switch((with0->RevPropertyIdxMap)[iProp - 1])
			{
       /*Trap wdg= and write out array properties instead*/
				case 	3:   // if WDG= was ever used write out arrays ...
				{
					int stop = 0;
					for(int stop = 16, i = 12; i <= stop; i++)
					{
						String PropValue = "=" + GetPropertyValue(i);
						Write( f, " " + with0->PropertyName[i-1] + PropValue );
					}
					for(int stop = NumWindings, i = 1; i <= stop; i++)
					{
						Write(f, Format(" wdg=%d %%R=%.7g", i, WINDING_[i - 1].Rpu * 100.0));
					}
				}
				break; /*do Nothing*/
				case 4: case 5: case 6: case 7: case 8: case 9:
				;
				break; // Ignore these properties; use arrays instead
				default:
				if(Length(String(Get_PropertyValue(iProp))) > 0)
				{
					String PropValue = "=" + CheckForBlanks(String(Get_PropertyValue(iProp) ) );
					Write( f, " " + with0->PropertyName[with0->RevPropertyIdxMap[iProp - 1] - 1] + PropValue );
				}
				break;
			}
		}
		iProp = GetNextPropertySet(iProp);
	}
}

//----------------------------------------------------------------------------

int TAutoTransObj::get_NumWindings()
{
	return NumWindings;
}

// sets an array which maps the two conductors of each winding to the
// phase and neutral conductors of the AutoTrans according to the winding connection

void TAutoTransObj::SetTermRef()
{
	int i = 0;
	int j = 0;
	int k = 0;
	int NumCondPerPhase = 0;
	k = 0;
	switch(Fnphases)
	{
		case 	1:
		for(int stop = NumWindings, j = 1; j <= stop; j++)
		{
			++k;
			(TermRef)[k - 1] = (j - 1) * Fnconds + 1;  // fNconds = 2
			++k;
			(TermRef)[k - 1] = j * Fnconds;
		}
		break;


/***  Typical array for 3-phase auto
This builds the YPrim and the NodeRef array maps it into Y
       TermRef^[1] := 1;
       TermRef^[2] := 4;
       TermRef^[3] := 7;
       TermRef^[4] := 10;
       TermRef^[5] := 2;
       TermRef^[6] := 5;
       TermRef^[7] := 8;
       TermRef^[8] := 11;
       TermRef^[9] := 3;
       TermRef^[10] := 6;
       TermRef^[11] := 9;
       TermRef^[12] := 12;

********/
		default:
		int stop = 0;
		for(int stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(int stop1 = NumWindings, j = 1; j <= stop1; j++)
			{
				++k;
				switch((WINDING_)[j - 1].Connection)
				{
					case 	WYE:  // Wye
					{
						TermRef[k - 1] = (j - 1) * Fnconds + i;
						++k;
						TermRef[k - 1] = TermRef[k - 1 - 1] + Fnphases;
					}
					break;
  /***** WILL THIS WORK for 2-PHASE OPEN DELTA ???? Need to check this sometime*/  // Delta
					case 	delta:
					{
						(TermRef)[k - 1] = (j - 1) * Fnconds + i;
						++k;
						(TermRef)[k - 1] = (j - 1) * Fnconds + RotatePhases(i);  // connect to next phase in sequence
					}
					break; // Series Winding for Auto Transfomer
					case 	SERIES:
					{
						(TermRef)[k - 1] = i;
						++k;
						(TermRef)[k - 1] = i + Fnphases;
					}
					break;
					default:
					  ;
					break;
				} /*CASE connection*/
			}
		}
		break;
	} /*CASE Fnphases*/
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TAutoTransObj::CalcYPrim(int ActorID)
{
	double FreqMultiplier = 0.0;
	if(Get_YprimInvalid(ActorID,0))
         // Reallocate YPrim if something has invalidated old allocation
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
  /*Same size as last time; just zero out to start over*/
	{
		YPrim_Series->Clear(); // zero out YPrim
		YPrim_Shunt->Clear(); // zero out YPrim
		YPrim->Clear();
	}

    // Set frequency multipliers for this calculation
	FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
	FreqMultiplier = FYprimFreq / BaseFrequency;
    // Check for rebuilding Y_Terminal; Only rebuild if freq is different than last time
	if(FreqMultiplier != Y_Terminal_Freqmult)
		CalcY_Terminal(FreqMultiplier, ActorID);
	BuildYPrimComponent(YPrim_Series, Y_Term);
	BuildYPrimComponent(YPrim_Shunt, Y_Term_NL);


    /*Combine the two Yprim components into Yprim*/
	YPrim->CopyFrom(YPrim_Series);
	YPrim->AddFrom(YPrim_Shunt);

    /*Now Account for Open Conductors*/
    /*For any conductor that is open, zero out row and column*/
	inherited::CalcYPrim(ActorID);
	Set_YprimInvalid(ActorID,false);
}

void TAutoTransObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int j = 0;
	TcMatrix* ZBtemp = nullptr;
	int stop = 0;
	inherited::DumpProperties(f, Complete);

    /*Basic Property Dump*/
	{ Write(f, "~ "); Write(f, "NumWindings="); WriteLn(f, NumWindings, 0); }
	{ Write(f, "~ "); Write(f, "phases="); WriteLn(f, Fnphases, 0); }
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		/*# with WINDING^[i] do */
		{
			
			if(i == 1)
				{ Write(f, "~ "); Write(f, "Wdg="); Write(f, i, 0); Write(f, " bus="); WriteLn(f, Get_FirstBus()); }
			else
				{ Write(f, "~ "); Write(f, "Wdg="); Write(f, i, 0); Write(f, " bus="); WriteLn(f, Get_NextBus()); }
			switch((WINDING_)[i - 1].Connection)
			{
				case 	WYE:
				WriteLn(f, "~ conn=wye");
				break;
				case 	delta:
				WriteLn(f, "~ conn=delta");
				break;
				case 	SERIES:
				WriteLn(f, "~ conn=Series");
				break;
				default:
				  ;
				break;
			}
			WriteLn(f, Format("~ kv=%.7g", (WINDING_)[i - 1].kVLL));
			WriteLn(f, Format("~ kVA=%.7g", (WINDING_)[i - 1].kVA));
			WriteLn(f, Format("~ tap=%.7g", (WINDING_)[i - 1].puTap));
			WriteLn(f, Format("~ %%r=%.7g", (WINDING_)[i - 1].Rpu * 100.0));
			WriteLn(f, Format("~ Rdcohms=%.7g", (WINDING_)[i - 1].RdcOhms));
		}
	}
	{ Write(f, "~ "); Write(f, "XHL="); WriteLn(f, puXHX * 100.0, 0, 3); }
	{ Write(f, "~ "); Write(f, "XHT="); WriteLn(f, puXHT * 100.0, 0, 3); }
	{ Write(f, "~ "); Write(f, "XLT="); WriteLn(f, puXXT * 100.0, 0, 3); }
	{ Write(f, "~ "); Write(f, "X12="); WriteLn(f, puXHX * 100.0, 0, 3); }
	{ Write(f, "~ "); Write(f, "X13="); WriteLn(f, puXHT * 100.0, 0, 3); }
	{ Write(f, "~ "); Write(f, "X23="); WriteLn(f, puXXT * 100.0, 0, 3); }
	Write(f, "~ Xscmatrix= \"");
	for(int stop = XSCSize(NumWindings), i = 1; i <= stop; i++)
	{
		{ Write(f, (puXSC)[i - 1] * 100.0, 0, 2); Write(f, L' '); }
	}
	WriteLn(f, L'\"');
	{ Write(f, "~ "); Write(f, "NormMAxHkVA="); WriteLn(f, NormMaxHkVA, 0, 0); }
	{ Write(f, "~ "); Write(f, "EmergMAxHkVA="); WriteLn(f, EmergMaxHkVA, 0, 0); }
	{ Write(f, "~ "); Write(f, "thermal="); WriteLn(f, ThermalTimeConst, 0, 1); }
	{ Write(f, "~ "); Write(f, "n="); WriteLn(f, n_thermal, 0, 1); }
	{ Write(f, "~ "); Write(f, "m="); WriteLn(f, m_thermal, 0, 1); }
	{ Write(f, "~ "); Write(f, "flrise="); WriteLn(f, FLrise, 0, 0); }
	{ Write(f, "~ "); Write(f, "hsrise="); WriteLn(f, HSrise, 0, 0); }
	{ Write(f, "~ "); Write(f, "%loadloss="); WriteLn(f, pctLoadLoss, 0, 0); }
	{ Write(f, "~ "); Write(f, "%noloadloss="); WriteLn(f, pctNoLoadLoss, 0, 0); }
	for(int stop = NumPropsThisClass, i = 28; i <= stop; i++)
	{
		{ Write(f, "~ "); Write(f, (ParentClass->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
	}
	/*# with ParentClass do */
	{
		auto with1 = ParentClass;
		int stop = 0;
		for(int stop = with1->NumProperties, i = NumPropsThisClass + 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, (with1->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
	{
		int stop = 0;
		WriteLn(f);
		ZBtemp = new TcMatrix(NumWindings - 1);
		ZBtemp->CopyFrom(ZB);
		ZBtemp->Invert();
		WriteLn(f, "ZB:");
		/*# with ZBtemp do */
		{
			auto with2 = ZBtemp;
			int stop = 0;
			for(int stop = NumWindings - 1, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = i, j = 1; j <= stop1; j++)
				{
					Write(f, Format("%g ", with2->GetElement(i, j).re));
				}
				WriteLn(f);
			}
			for(int stop = NumWindings - 1, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = i, j = 1; j <= stop1; j++)
				{
					Write(f, Format("%g ", with2->GetElement(i, j).im));
				}
				WriteLn(f);
			}
		}  /*WITH*/
		delete ZBtemp;
		WriteLn(f);
		WriteLn(f, "ZB: (inverted)");
		/*# with ZB do */
		{
			auto with3 = ZB;
			int stop = 0;
			for(int stop = NumWindings - 1, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = i, j = 1; j <= stop1; j++)
				{
					{ Write(f, with3->GetElement(i, j).re, 0, 4); Write(f, L' '); }
				}
				WriteLn(f);
			}
			for(int stop = NumWindings - 1, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = i, j = 1; j <= stop1; j++)
				{
					{ Write(f, with3->GetElement(i, j).im, 0, 4); Write(f, L' '); }
				}
				WriteLn(f);
			}
		}  /*WITH*/
		WriteLn(f);
		WriteLn(f, "Y_OneVolt");
		/*# with Y_1Volt do */
		{
			auto with4 = Y_1Volt;
			int stop = 0;
			for(int stop = NumWindings, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = i, j = 1; j <= stop1; j++)
				{
					{ Write(f, with4->GetElement(i, j).re, 0, 4); Write(f, L' '); }
				}
				WriteLn(f);
			}
			for(int stop = NumWindings, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = i, j = 1; j <= stop1; j++)
				{
					{ Write(f, with4->GetElement(i, j).im, 0, 4); Write(f, L' '); }
				}
				WriteLn(f);
			}
		}
		WriteLn(f);
		WriteLn(f, "Y_Terminal");
		/*# with Y_Term do */
		{
			auto with5 = Y_Term;
			int stop = 0;
			for(int stop = 2 * NumWindings, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = i, j = 1; j <= stop1; j++)
				{
					{ Write(f, with5->GetElement(i, j).re, 0, 4); Write(f, L' '); }
				}
				WriteLn(f);
			}
			for(int stop = 2 * NumWindings, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = i, j = 1; j <= stop1; j++)
				{
					{ Write(f, with5->GetElement(i, j).im, 0, 4); Write(f, L' '); }
				}
				WriteLn(f);
			}
		}
		WriteLn(f);
		Write(f, "TermRef= ");
		for(int stop = 2 * NumWindings * Fnphases, i = 1; i <= stop; i++)
		{
			{ 
				Write(f, to_string(TermRef[i - 1]));
				Write(f, L' '); }
		}
		WriteLn(f);
	}
}

void TAutoWinding::ComputeAntiFloatAdder(double PPM_Factor, double VABase1ph)
{
	Y_PPM = -PPM_Factor / (Sqr(VBase) / VABase1ph) / 2.0L;  //12-11-12 divided by two
       // put half on each terminal of the winding.
}
/*
   Make a new winding
*/

TAutoWinding::TAutoWinding(int iWinding)
 : Connection(0),
			kVLL(0.0),
			VBase(0.0),
			kVA(0.0),
			puTap(0.0),
			Rpu(0.0),
			Rdcpu(0.0),
			RdcOhms(0.0),
			RdcSpecified(false),
			Y_PPM(0.0),
			TapIncrement(0.0),
			MinTap(0.0),
			MaxTap(0.0),
			NumTaps(0)
{
	;
	switch(iWinding)
	{
		case 	1:
		{
			Connection = SERIES;  // Get_First() Winding is Series Winding
			kVLL = 115.0;
		}
		break;
		default:
		Connection = WYE;
		kVLL = 12.47;
		break;
	}
	VBase = kVLL / SQRT3 * 1000.0;
	kVA = 1000.0;
	puTap = 1.0;
	Rpu = 0.002;
	Rdcpu = Rpu * 0.85;  // default value
	RdcOhms = Sqr(kVLL) / (kVA / 1000.0) * Rdcpu;
	RdcSpecified = false;
	ComputeAntiFloatAdder(1.0e-6, kVA / 3.0 / 1000.0);     //  1 PPM
	TapIncrement = 0.00625;
	NumTaps = 32;
	MaxTap = 1.10;
	MinTap = 0.90;
}

double TAutoTransObj::Get_PresentTap(int i, int ActorID)
{
	double result = 0.0;
	if((i > 0) && (i <= NumWindings))
		result = (WINDING_)[i - 1].puTap;
	else
		result = 0.0;
	return result;
}

void TAutoTransObj::Set_PresentTap(int i, int ActorID, double Value)
{
	double TempVal = 0.0;
	if((i > 0) && (i <= NumWindings))
		/*# with WINDING^[i] do */
		{
			
           /*Range Checking*/
			TempVal = Value;
			if(TempVal < (WINDING_)[i - 1].MinTap)
				TempVal = (WINDING_)[i - 1].MinTap;
			else
			{
				if(TempVal > (WINDING_)[i - 1].MaxTap)
					TempVal = (WINDING_)[i - 1].MaxTap;
			}
			if(TempVal != (WINDING_)[i - 1].puTap)    /*Only if there's been a change*/
			{
				(WINDING_)[i - 1].puTap = TempVal;
				Set_YprimInvalid(ActorID,true);  // this property triggers setting SystemYChanged=true
				RecalcElementData(ActorID);
			}
		}
}

double TAutoTransObj::Get_WdgResistance(int i)
{
	double result = 0.0;
	if((i > 0) && (i <= NumWindings))
		result = (WINDING_)[i - 1].Rpu;
	else
		result = 0.0;
	return result;
}

double TAutoTransObj::Get_WdgkVA(int i)
{
	double result = 0.0;
	if((i > 0) && (i <= NumWindings))
		result = (WINDING_)[i - 1].kVA;
	else
		result = 0.0;
	return result;
}

double TAutoTransObj::Get_WdgYPPM(int i)
{
	double result = 0.0;
	if((i > 0) && (i <= NumWindings))
		result = (WINDING_)[i - 1].Y_PPM;
	else
		result = 0.0;
	return result;
}

double TAutoTransObj::Get_Xsc(int i)
{
	double result = 0.0;
	int IMax = 0;
	IMax = XSCSize(NumWindings);
	if((i > 0) && (i <= IMax))
		result = (puXSC)[i - 1];
	else
		result = 0.0;
	return result;
}

int TAutoTransObj::Get_WdgConnection(int i)
{
	int result = 0;
	if((i > 0) && (i <= NumWindings))
		result = (WINDING_)[i - 1].Connection;
	else
		result = 0;
	return result;
}

double TAutoTransObj::Get_MinTap(int i)
{
	double result = 0.0;
	if((i > 0) && (i <= NumWindings))
		result = (WINDING_)[i - 1].MinTap;
	else
		result = 0.0;
	return result;
}

double TAutoTransObj::Get_MaxTap(int i)
{
	double result = 0.0;
	if((i > 0) && (i <= NumWindings))
		result = (WINDING_)[i - 1].MaxTap;
	else
		result = 0.0;
	return result;
}

int TAutoTransObj::Get_NumTaps(int i)
{
	int result = 0;
	if((i > 0) && (i <= NumWindings))
		result = (WINDING_)[i - 1].NumTaps;
	else
		result = 0;
	return result;
}

double TAutoTransObj::Get_TapIncrement(int i)
{
	double result = 0.0;
	if((i > 0) && (i <= NumWindings))
		result = (WINDING_)[i - 1].TapIncrement;
	else
		result = 0.0;
	return result;
}

/*
  Return a vector of complex currents for each Winding of all phases

  Iterm = Yterm * Vterm

  Yterm order is 2*NumWindings.  Each phase has same Yterm.
  Vterm order is 2*NumWindings .

  Calculate Iterm phase-by-phase and concatenate into CurrBuffer.
*/

void TAutoTransObj::GetAllWindingCurrents(pComplexArray CurrBuffer, int ActorID)
{
	int jphase = 0;
	int k = 0;
	int iPhase = 0;
	int iWind = 0;
	int i = 0;
	pComplexArray VTerm = nullptr;
	pComplexArray iTerm = nullptr;
	pComplexArray ITerm_NL = nullptr;
	try
	{
		int stop = 0;
		VTerm = new complex[2 * NumWindings];
		iTerm = new complex[2 * NumWindings];
		ITerm_NL = new complex[2 * NumWindings];

     /*Load up Vterminal - already allocated for all cktelements*/
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			if(!(with0->NodeV.empty()))
			{
				int stop = 0;
				for(int stop = Yorder, i = 1; i <= stop; i++)
				{
					if(!ADiakoptics || (ActorID == 1))
						(Vterminal)[i - 1] = with0->NodeV[(NodeRef)[i - 1]];
					else
						(Vterminal)[i - 1] = with0->VoltInActor1((NodeRef)[i - 1]);
				}
			}
			else
			{
				int stop = 0;
				for(int stop = Yorder, i = 1; i <= stop; i++)
				{
					(Vterminal)[i - 1] = CZero;
				}
			}
		}
		k = 0;
		for(int stop = Fnphases, iPhase = 1; iPhase <= stop; iPhase++)
		{
			int stop1 = 0;
			for(int stop1 = NumWindings, iWind = 1; iWind <= stop1; iWind++)
			{
				i = 2 * iWind - 1;
				switch((WINDING_)[iWind - 1].Connection)
				{
					case 	WYE:   // Wye  (Common winding usually)
					{
						(VTerm)[i - 1] = (Vterminal)[iPhase + (iWind - 1) * Fnconds - 1];
						(VTerm)[i + 1 - 1] = (Vterminal)[iPhase + (iWind - 1) * Fnconds + Fnphases - 1];
					}
					break;   // Delta
					case 	delta:
					{
						jphase = RotatePhases(iPhase);      // Get next phase in sequence
						(VTerm)[i - 1] = (Vterminal)[iPhase + (iWind - 1) * Fnconds - 1];
						(VTerm)[i + 1 - 1] = (Vterminal)[jphase + (iWind - 1) * Fnconds - 1];
					}
					break;    // Series Winding
					case 	SERIES:
					{
						(VTerm)[i - 1] = (Vterminal)[iPhase + (iWind - 1) * Fnconds - 1];
						(VTerm)[i + 1 - 1] = (Vterminal)[iPhase + Fnphases - 1];
					}
					break;
					default:
					  ;
					break;
				} /*CASE*/
			}
			Y_Term->MVmult(iTerm, VTerm);  // ITerm = Y_Term Vterm
			Y_Term_NL->MVmult(ITerm_NL, VTerm);// no load part
        // Add into Currbuffer
			for(int stop1 = 2 * NumWindings, i = 1; i <= stop1; i++)
			{
				k = k + 1;
				(CurrBuffer)[k - 1] = cadd((iTerm)[i - 1], (ITerm_NL)[i - 1]);
			}
		}
		VTerm = (pComplexArray) realloc(VTerm, 0);
		iTerm = (pComplexArray) realloc(iTerm, 0);
		ITerm_NL = (pComplexArray) realloc(ITerm_NL, 0);
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error filling voltage buffer in GetAllWindingCurrents for Circuit Element:AutoTrans.") + get_Name()
	           + CRLF
	           + "Probable Cause: Invalid definition of element."
	           + CRLF
	           + "System Error Message: "
	           + (std::string) e.what(), 100115);
	}
}

// Returns string mag, angle

String TAutoTransObj::GeTAutoWindingCurrentsResult(int ActorID)
{
	String result;
	pComplexArray WindingCurrents = nullptr;
	int i = 0;
	int j = 0;
	int k = 0;
	int stop = 0;
	WindingCurrents = new complex[ 2 * Fnphases * NumWindings];
	GetAllWindingCurrents(WindingCurrents, ActorID);
	result = "";
	k = 0;
	for(int stop = Fnphases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(int stop1 = NumWindings, j = 1; j <= stop1; j++)
		{
			k = k + 1;
			result = result
	           + Format("%.7g, (%.5g), ", cabs((WindingCurrents)[k - 1]), cdang((WindingCurrents)[k - 1]));
			k = k + 1;
           // Skip currents from other end of the winding
		}
	}
	free(WindingCurrents);  // throw away temp array
	return result;
}

//  Voltages across indicated winding
// Fill Vbuffer array which must be adequately allocated by calling routine
// Order is Number of Phases

void TAutoTransObj::GetAutoWindingVoltages(int iWind, pComplexArray VBuffer, int ActorID)
{
	int i = 0;
	int II = 0;
	int k = 0;
	int NeutTerm = 0;
	try


     /*return Zero if winding number improperly specified*/
	{
		int stop = 0;
		if((iWind < 1) || (iWind > NumWindings))
		{
			int stop = 0;
			for(int stop = Fnconds, i = 1; i <= stop; i++)
			{
				(VBuffer)[i - 1] = CZero;
			}
			return;
		}

     /*Load up VTerminal - already allocated for all cktelements*/
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			int stop = 0;
			for(int stop = Yorder, i = 1; i <= stop; i++)
			{
				if(!ADiakoptics || (ActorID == 1))
					(Vterminal)[i - 1] = with0->NodeV[(NodeRef)[i - 1]];
				else
					(Vterminal)[i - 1] = with0->VoltInActor1((NodeRef)[i - 1]);
			}
		}
		k = (iWind - 1) * Fnconds;    // Offset for winding
		NeutTerm = Fnphases + k + 1;
		for(int stop = Fnphases, i = 1; i <= stop; i++)
		{
			switch((WINDING_)[iWind - 1].Connection)
			{
				case 	WYE:      // Wye
				{
					(VBuffer)[i - 1] = csub((Vterminal)[i + k - 1], (Vterminal)[NeutTerm - 1]);
				}
				break;   // Delta
				case 	delta:
				{
					II = RotatePhases(i);      // Get next phase in sequence
					(VBuffer)[i - 1] = csub((Vterminal)[i + k - 1], (Vterminal)[II + k - 1]);
				}
				break;      // Series   (winding 1)
				case 	SERIES:
				{
					(VBuffer)[i - 1] = csub((Vterminal)[i + k - 1], (Vterminal)[i + Fnconds - 1]);
				}
				break;
				default:
				  ;
				break;
			}
		} /*CASE*/
	}
	catch (std::exception &e)
	{
		DoSimpleMsg(String("Error filling voltage buffer in GeTAutoWindingVoltages for Circuit Element:AutoTrans.") + get_Name()
			+ CRLF
			+ "Probable Cause: Invalid definition of element."
			+ CRLF
			+ "System Error Message: "
			+ (std::string) e.what(), 100114);
	}
}

double TAutoTransObj::Get_BaseVoltage(int i)
{
	double result = 0.0;
	if((i < 1) || (i > NumWindings))
		result = (WINDING_)[1 - 1].VBase;
	else
		result = (WINDING_)[i - 1].VBase;
	return result;
}

/*============================== GetLosses Override ===============================*/

void TAutoTransObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int j = 0;
	int stop = 0;
	inherited::GetCurrents(Curr, ActorID);

  // Combine Series (wdg 1) and Common winding (2) Currents to get X Terminal Currents
	for(int stop = Fnphases, i = 1; i <= stop; i++)
	{
		caccum((Curr)[i + Fnconds - 1], (Curr)[i + Fnphases - 1]);
	}
}

void TAutoTransObj::GetLosses(complex& TotalLosses, complex& LoadLosses, complex& NoLoadLosses, int ActorID)
{
	pComplexArray cTempIterminal = nullptr;
	int i = 0;
  /*inherited;*/

  /*Calculates losses in watts, vars*/
	int stop = 0;
	TotalLosses = Get_Losses(ActorID);   // Side effect: computes Iterminal

  /*Compute No load losses in Yprim_Shunt*/
	cTempIterminal = new complex[Yorder];
	ComputeVterminal(ActorID);
	YPrim_Shunt->MVmult(cTempIterminal, &(Vterminal[0]));
  /*No Load Losses are sum of all powers coming into YPrim_Shunt from each terminal*/
	NoLoadLosses = CZero;
	for(int stop = Yorder, i = 1; i <= stop; i++)
	{
		caccum(NoLoadLosses, cmul((Vterminal)[i - 1], conjg((cTempIterminal)[i - 1])));
	}
	LoadLosses = csub(TotalLosses, NoLoadLosses);
	cTempIterminal = (pComplexArray) realloc(cTempIterminal, 0);
}

/* gets the property for the active winding ; Set the active winding before calling*/

String TAutoTransObj::GetPropertyValue(int Index)
{
	String result;
	int i = 0;
	switch(Index)
	{
		case 12: case 13: case 14: case 15: case 16:
		 case 20:
		 case 37:
		result = "[";
		break;
		default:
		result = "";
		break;
	}
	switch(Index)
	{
		case 	1:
		result = IntToStr(Get_NPhases());
		break;
		case 	2:
		result = IntToStr(NumWindings);
		break;
		case 	3:
		result = IntToStr(ActiveWinding);
		break;  // return active winding
		case 	4:
		result = GetBus(ActiveWinding);
		break;    // return bus spec for active winding
		case 	5:
		switch((WINDING_)[ActiveWinding - 1].Connection)
		{
			case 	0:
			result = "wye ";
			break;
			case 	1:
			result = "Delta ";
			break;
			case 	2:
			result = "Series";
			break;
			default:
			  ;
			break;
		}
		break;
		case 	6:
		result = Format("%.7g", (WINDING_)[ActiveWinding - 1].kVLL);
		break;
		case 	7:
		result = Format("%.7g", (WINDING_)[ActiveWinding - 1].kVA);
		break;
		case 	8:
		result = Format("%.7g", (WINDING_)[ActiveWinding - 1].puTap);
		break;
		case 	9:
		result = Format("%.7g", (WINDING_)[ActiveWinding - 1].Rpu * 100.0);
		break;   // %R
		case 	10:
		result = Format("%.7g", (WINDING_)[ActiveWinding - 1].RdcOhms);
		break;
		case 	11:
		switch(CoreType)
		{
			case 	0:
			result = "shell";
			break;
			case 	1:
			result = "1-phase";
			break;
			case 	3:
			result = "3-leg";
			break;
			case 	5:
			result = "5-Leg";
			break;
			default:
			  ;
			break;
		}
		break;
		case 	12:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			result = result + GetBus(i) + ", ";
		}
		break;
		case 	13:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			switch((WINDING_)[i - 1].Connection)
			{
				case 	0:
				result = result + "wye, ";
				break;
				case 	1:
				result = result + "delta, ";
				break;
				case 	2:
				result = result + "Series, ";
				break;
				default:
				  ;
				break;
			}
		}
		break;
		case 	14:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			result = result + Format("%.7g, ", (WINDING_)[i - 1].kVLL);
		}
		break;
		case 	15:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			result = result + Format("%.7g, ", (WINDING_)[i - 1].kVA);
		}
		break;
		case 	16:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			result = result + Format("%.7g, ", (WINDING_)[i - 1].puTap);
		}
		break;// InterpretAllTaps(Param);
		case 	17:
		result = Format("%.7g", puXHX * 100.0);
		break;
		case 	18:
		result = Format("%.7g", puXHT * 100.0);
		break;
		case 	19:
		result = Format("%.7g", puXXT * 100.0);
		break;
		case 	20:
		for(int stop = XSCSize(NumWindings), i = 1; i <= stop; i++)
		{
			result = result + Format("%-g, ", (puXSC)[i - 1] * 100.0);
		}
		break;// Parser.ParseAsVector(((NumWindings - 1)*NumWindings div 2), Xsc);
		case 	26:
		result = Format("%.7g", pctLoadLoss);
		break;
		case 	27:
		result = Format("%.7g", pctNoLoadLoss);
		break;
		case 	28:
		result = Format("%.7g", NormMaxHkVA);
		break;
		case 	29:
		result = Format("%.7g", EmergMaxHkVA);
		break;
		case 	31:
		result = Format("%.7g", (WINDING_)[ActiveWinding - 1].MaxTap);
		break;
		case 	32:
		result = Format("%.7g", (WINDING_)[ActiveWinding - 1].MinTap);
		break;
		case 	33:
		result = Format("%-d", (WINDING_)[ActiveWinding - 1].NumTaps);
		break;
		case 	35:
		result = Format("%.7g", pctImag);
		break;
		case 	36:
		result = Format("%.7g", ppm_FloatFactor / 1.0e-6);
		break;
		case 	37:
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			result = result + Format("%.7g, ", (WINDING_)[i - 1].Rpu * 100.0);
		}
		break;
		case 	40:
		if(XRConst)
			result = "YES";
		else
			result = "NO";
		break;
		case 	42:
		result = GeTAutoWindingCurrentsResult(ActiveActor);
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}

        // Overrides
	switch((Index - NumPropsThisClass))
	{
		case 	1:
		result = Format("%-.5g", NormAmps);
		break;  //Normamps
		case 	2:
		result = Format("%-.5g", EmergAmps);
		break;  //emergamps
		default:
		  ;
		break;
	}
	switch(Index)
	{
		case 12: case 13: case 14: case 15: case 16:
		 case 20:
		 case 37:
		result = result + "]";
		break;
		default:
		  ;
		break;
	}
	return result;
}

void TAutoTransObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"3"); //'phases';
	Set_PropertyValue(2,"2"); //'windings';
   // Winding Definition
	Set_PropertyValue(3,"1"); //'wdg';
	Set_PropertyValue(4,GetBus(1)); //'bus';
	Set_PropertyValue(5,"Series"); // 'conn';
	Set_PropertyValue(6,"115"); // IF 2or 3-phase:  phase-phase    ELSE actual winding
	Set_PropertyValue(7,"100000");
	Set_PropertyValue(8,"1.0");
	Set_PropertyValue(9,"0.2");
	Set_PropertyValue(10,"0.2645"); // 0.002 pu @ 115 kV, 100 MVA
	Set_PropertyValue(11,"shell");

   // General Data
	Set_PropertyValue(12,"");
	Set_PropertyValue(13,"");
	Set_PropertyValue(14,""); // IF 1-phase: actual winding rating; ELSE phase-phase
	Set_PropertyValue(15,""); // IF 1-phase: actual winding rating; ELSE phase-phase
	Set_PropertyValue(16,"");
	Set_PropertyValue(17,"7");
	Set_PropertyValue(18,"35");
	Set_PropertyValue(19,"30");
	Set_PropertyValue(20,"");  // x12 13 14... 23 24.. 34 ..
	Set_PropertyValue(21,"2");
	Set_PropertyValue(22,".8");
	Set_PropertyValue(23,".8");
	Set_PropertyValue(24,"65");
	Set_PropertyValue(25,"15");
	Set_PropertyValue(26,Format("%.7g", pctLoadLoss));
	Set_PropertyValue(27,Format("%.7g", pctNoLoadLoss));    // Defaults to zero
	Set_PropertyValue(28,"");
	Set_PropertyValue(29,"");
	Set_PropertyValue(30,"n");  // =y/n
	Set_PropertyValue(31,"1.10");
	Set_PropertyValue(32,"0.90");
	Set_PropertyValue(33,"32");
	Set_PropertyValue(34,"");
	Set_PropertyValue(35,"0");
	Set_PropertyValue(36,"1");
	Set_PropertyValue(37,"");
	Set_PropertyValue(38,"");
	Set_PropertyValue(39,"");
	Set_PropertyValue(40,"NO");
	Set_PropertyValue(41,"Lag");
	Set_PropertyValue(42,"0");
	inherited::InitPropertyValues(NumPropsThisClass);

      // Override some Inherited properties
	Set_PropertyValue(NumPropsThisClass + 1,"400");  //Normamps
	Set_PropertyValue(NumPropsThisClass + 2,"600");  //emergamps
	Set_PropertyValue(NumPropsThisClass + 3,"0.007");  //Fault rate
	Set_PropertyValue(NumPropsThisClass + 4,"100");   // Pct Perm
	Set_PropertyValue(NumPropsThisClass + 5,"36");    // Hrs to repair
	ClearPropSeqArray();    // so the overrides don't show up on save
}
// For Delta connections or Line-Line voltages

int TAutoTransObj::RotatePhases(int iPhs)
{
	int result = 0;
	result = iPhs + DeltaDirection;

     // make sure result is within limits
	if(Fnphases > 2)
         // Assumes 2 phase delta is open delta
	{
		if(result > Fnphases)
			result = 1;
		if(result < 1)
			result = Fnphases;
	}
	else
	{
		if(result < 1)
			result = 3;    // For 2-phase delta, next phase will be 3rd phase
	}
	return result;
}
/*
  Converts default 3-phase AutoTrans model into equivalent positive-sequence
  using scripting
*/

void TAutoTransObj::MakePosSequence(int ActorID)
{
	int IW = 0;
	int i = 0;
	int n = 0;
	String s;
	int Nodes[50/*# range 1..50*/]; // big integer buffer
	bool OnPhase1 = false;

  /*Get_First(), determine if we can convert this one.*/
	int stop = 0;
	if((Fnphases == 1) || (Fnphases == 2)) //disable if any terminal not connected to phase one
	{
		int stop = 0;
		for(int stop = NumWindings, IW = 1; IW <= stop; IW++)
		{
			int stop1 = 0;
			OnPhase1 = false;
       /*Load up auxiliary parser*/
			AuxParser[ActorID]->SetCmdString(GetBus(IW));
			String dummy = AuxParser[ActorID]->GetNextParam();
			s = AuxParser[ActorID]->ParseAsBusName(n, (pIntegerArray) &Nodes, ActorID);
			if(n == 0)
				OnPhase1 = true;
			for(int stop1 = n, i = 1; i <= stop1; i++)
			{
				if(Nodes[i - 1] == 1)
					OnPhase1 = true;
			}
			if(!OnPhase1)
			{
				Set_Enabled(false);   // We won't use this one
				return;
			}
		}
	}

   /*Construct AutoTrans definition string */
	s = "Phases=1  Conns=(";
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		s = s + "Wye ";
	}
	s = s + ")  buses=(";
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		s = s + GetBus(i) + " ";
	}
	s = s + ")  kVS=(";
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		/*# with WINDING^[i] do */
		{
			
			if((Get_NPhases() > 1) || ((WINDING_)[i - 1].Connection != WYE))
				s = s + Format(" %-.5g", (WINDING_)[i - 1].kVLL / SQRT3);
			else
				s = s + Format(" %-.5g", (WINDING_)[i - 1].kVLL);
		}
	}
	s = s + ")  kVAs=(";
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		/*# with WINDING^[i] do */
		{
			
			s = s + Format(" %-.5g", (WINDING_)[i - 1].kVA / Fnphases);
		}
	}
	s = s + ")";
	s = s
	           + " NormHkVA="
	           + Format(" %-.5g %-.5g", NormMaxHkVA / Fnphases, EmergMaxHkVA / Fnphases);
	Parser[ActorID]->SetCmdString(s);
	Edit(ActorID);
	inherited::MakePosSequence(ActorID);
}

void TAutoTransObj::BuildYPrimComponent(TcMatrix* YPrim_Component, TcMatrix* Y_Terminal)
{
	int NW2 = 0;
	int i = 0;
	int k = 0;
	complex Value = {};
	int j = 0;
	/*# with YPrim_Component do */
	{
		auto with0 = YPrim_Component;
    /* Now, Put in Yprim matrix */
    /*have to add every element of Y_terminal into Yprim somewhere*/
		int stop = 0;
		NW2 = 2 * NumWindings;
		for (i = 1; i <= NW2; i++)
		{
			int stop1 = 0;
			for (j = 1; j <= i; j++)
			{
				int stop2 = 0;
				Value = Y_Terminal->GetElement(i, j);
        // This value goes in Yprim nphases times
				for (k = 0; k < Fnphases; k++)
				{
					with0->AddElemsym((TermRef)[i + k * NW2 - 1], (TermRef)[j + k * NW2 - 1], Value);
				}
			}
		}
	}
}

double TAutoTransObj::Get_BasekVLL(int i)
{
	double result = 0.0;
	result = (WINDING_)[i - 1].kVLL;
	return result;
}
// Build YTerminal considering only resistance and no coupling to other winding.

void TAutoTransObj::GICBuildYTerminal()
{
	int i = 0;
	int j = 0;
	int Idx = 0;
	complex Yr = {};
	TTextRec f = {};
	complex Yadder = {};
	int stop = 0;
	Y_Term->Clear();
	Y_Term_NL->Clear();
	for(int stop = NumWindings, i = 1; i <= stop; i++)
	{
		Yr = cmplx(1.0 / ((WINDING_)[i - 1].RdcOhms), 0.0); // convert to Siemens
		/*# with Y_Term do */
		{
			auto with0 = Y_Term;
			Idx = 2 * i - 1;
			with0->SetElement(Idx, Idx, Yr);
			with0->SetElement(Idx + 1, Idx + 1, Yr);
			with0->SetElemsym(Idx, Idx + 1, cnegate(Yr));   // set off-diagonals
		}
	}

    /*For GIC add a small *Conductance* to both conductors of each winding so that
    the matrix will always invert even if the user neglects to define a voltage
    reference on all sides*/
	if(ppm_FloatFactor != 0.0)
		/*# with Y_Term do */
		{
			auto with1 = Y_Term;
			int stop = 0;
			for(int stop = NumWindings, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				Yadder = cmplx(-(WINDING_)[i - 1].Y_PPM, 0.0);    // G + j0
				for(int stop1 = (2 * i), j = (2 * i - 1); j <= stop1; j++)
				{
					with1->SetElement(j, j, cadd(with1->GetElement(j, j), Yadder));
				}
/*           SetElement(j, j, CmulReal_im(GetElement(j, j) , ppm_FloatFactorPlusOne));*/
			}
		}
}

void TAutoTransObj::CalcY_Terminal(double FreqMult, int ActorID)
{
	int i = 0;
	int j = 0;
	int k = 0;
	vector <complex>	A;
	vector <complex>	ctempArray1;
	vector <complex>	ctempArray2;
	complex				cMinusOne = CZero;
	TcMatrix* At = nullptr;
	complex				Yadder = CZero;
	double				Rmult = 0.0;
	double ZCorrected = 0.0;
	double puXst, Vs, Vc;
    /*Function to fix a specification of a pu tap of 0.0*/
    /*Regcontrol can attempt to force zero tap position in some models*/

	auto ZeroTapFix = [&](double tapvalue) -> double 
	{
		double result = 0.0;
		if(tapvalue == 0.0)
			result = 0.0001;
		else
			result = tapvalue;
		return result;
	};

    // check for GIC build
	if(ActiveCircuit[ActorID]->Solution->get_FFrequency() < 0.51)
         /*Build Yterminal for GIC ~dc simulation*/
		GICBuildYTerminal();
	else
  /*Normal Y matrix build*/
	{
		int stop = 0;
		if(XRConst)
			Rmult = FreqMult;
		else
			Rmult = 1.0;


  // Construct ZBMatrix;
		ZB->Clear();
		ZBase = 1.0L / (VABase / Fnphases); // base ohms on 1.0 volt basis
       // Adjust specified XSC by SQR(1 + Vcommon/Vseries)
		ZCorrected = ZBase * Sqr(1.0 + (WINDING_)[2 - 1].VBase / (WINDING_)[1 - 1].VBase); // Correction factor for Series
		// since the losses are correct as they are, mimic Dommel (6.50) for Zst or puXSC[2], without disturbing Zbase or Zcorrected
		// Dommel: Xst = Xhl*Vh*Vl/(Vh-Vl)^2 + Xht*Vh/(Vh-Vl) - Xlt*Vl/(Vh-Vl)
		//             = Xhl*(Vs+Vc)*Vc/Vs^2 + Xht*(Vs+Vc)/Vs - Xlt*Vc/Vs
		if (NumWindings > 2)
		{
			Vc = WINDING_[2 - 1].VBase;
			Vs = WINDING_[1 - 1].VBase;
			puXst = puXSC[1 - 1] * (Vs + Vc) * Vc / Vs / Vs + puXSC[2 - 1] * (Vs + Vc) / Vs - puXSC[3 - 1] * Vc / Vs;
		}
		else
		{
			puXst = 0.0;
		}

		for(int stop = NumWindings - 1, i = 1; i <= stop; i++)
		{
          /* convert pu to ohms on one volt base as we go... */
			if(i == 1)
				ZB->SetElement(i, i, cmulreal(cmplx(Rmult * ((WINDING_)[1 - 1].Rpu + (WINDING_)[i + 1 - 1].Rpu), FreqMult * (puXSC)[i - 1]), ZCorrected));
			else if (i == 2)
				ZB->SetElement(i, i, cmulreal(cmplx(Rmult * ((WINDING_)[1 - 1].Rpu + (WINDING_)[i + 1 - 1].Rpu), FreqMult * puXst), ZBase));
			else
				ZB->SetElement(i, i, cmulreal(cmplx(Rmult * ((WINDING_)[1 - 1].Rpu + (WINDING_)[i + 1 - 1].Rpu), FreqMult * (puXSC)[i - 1]), ZBase));
		}

       // Off diagonals
		k = NumWindings;
		/*# with ZB do */
		{
			auto with0 = ZB;
			int stop = 0;
			for(int stop = NumWindings - 1, i = 1; i <= stop; i++)
			{
				int stop1 = 0;
				for(int stop1 = NumWindings - 1, j = i + 1; j <= stop1; j++)
				{
					with0->SetElemsym(i, j, cmulreal(csub(cadd(with0->GetElement(i, i), with0->GetElement(j, j)), cmulreal(cmplx(Rmult * ((WINDING_)[i + 1 - 1].Rpu + (WINDING_)[j + 1 - 1].Rpu), FreqMult * (puXSC)[k - 1]), ZBase)), 0.5));
					++k;
				}
			}
		}

  /*******************************DEBUG*******************************************************/  
  /*******************************************************************************************/
		ZB->Invert();   // mhos on one volt base
		if(ZB->InvertError > 0)
		{
			int stop = 0;
			DoErrorMsg("TAutoTransObj.CalcYPrim", String("Matrix Inversion Error for AutoTrans \"") + get_Name()
	           + "\"", "Invalid impedance specified. Replaced with tiny conductance to ground.", 117);
			ZB->Clear();
			for(int stop = ZB->get_Norder(), i = 1; i <= stop; i++)
			{
				ZB->SetElement(i, i, cmplx(EPSILON, 0.0));
			}
		}

  /*******************************DEBUG*******************************************************/  
  /*******************************************************************************************/

   // Now construct Y_Oneturn = AT * ZB.Invert * A
		   /*     -1 1 0 ...
     A = -1 0 1 ..   order:  N-1 x N   N = NumWindings
         ...
                           -1 -1 ...
     AT = Transpose of A =  1  0 ...    N X N-1
                            0  1 ..
   */
		Y_1Volt->Clear();
		Y_1Volt_NL->Clear();

     /*Allocate temp complex arrays*/
		ctempArray1.resize(NumWindings * 2);
		ctempArray2.resize(NumWindings * 2);
		A.resize(NumWindings * 2);
		cMinusOne = cmplx(-1.0, 0.0);
		At = new TcMatrix(NumWindings);
		for(int stop = NumWindings - 1, i = 1; i <= stop; i++)
		{
			At->SetElement(i + 1, i, cONE);
		}
		for(int stop = NumWindings - 1, i = 1; i <= stop; i++)
		{
			At->SetElement(1, i, cMinusOne);
		}
		(ctempArray1)[NumWindings - 1] = CZero;
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			if(i == 1)
			{
				int stop1 = 0;
				for(int stop1 = NumWindings - 1, k = 1; k <= stop1; k++)
				{
					(A)[k - 1] = cMinusOne;
				}
			}
			else
			{
				int stop1 = 0;
				for(int stop1 = NumWindings - 1, k = 1; k <= stop1; k++)
				{
					if(k == (i - 1))
						(A)[k - 1] = cONE;
					else
						(A)[k - 1] = CZero;
				}
			}
			ZB->MVmult(&ctempArray1[0], &A[0]); /*Zb.invert * A*/
			At->MVmult(&ctempArray2[0], &ctempArray1[0]); /*AT * Result*/
			for (j = 1; j <= NumWindings; j++)
			{
				Y_1Volt->SetElement(j, i, (ctempArray2)[j - 1]);
			}
		}

   /*Add magnetizing Reactance to 2nd winding, assuming it is closest to the core
    Add both resistive element representing core losses and a reactive element representing
    magnetizing current
   */
		Y_1Volt_NL->AddElement(2, 2, cmplx((pctNoLoadLoss / 100.0 / ZBase), -pctImag / 100.0 / ZBase / FreqMult));

  /*******************************DEBUG*******************************************************/  
  /*******************************************************************************************/
     // should have admittance of one phase of the AutoTrans on a one-volt, wye-connected base

     // Now make into terminal admittance matrix and correct for actual voltage ratings
     // Y_Terminal = AT * Y_onevolt * A  where V_onevolt = A * V_terminal
		delete At;
		Y_Term->Clear();
		Y_Term_NL->Clear();
		At = new TcMatrix(NumWindings * 2);

     // 8/22/2013 Added ZeroTapFix so that regcontrol can set a tap to zero
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			/*# with WINDING^[i] do */
			{
				
				At->SetElement(2 * i - 1, i, cmplx(1.0 / ((WINDING_)[i - 1].VBase * ZeroTapFix((WINDING_)[i - 1].puTap)), 0.0));
			}
		}
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			/*# with WINDING^[i] do */
			{
				
				At->SetElement(2 * i, i, cmplx(-1.0 / ((WINDING_)[i - 1].VBase * ZeroTapFix((WINDING_)[i - 1].puTap)), 0.0));
			}
		}
		for(int stop = 2 * NumWindings, i = 1; i <= stop; i++)
		{
			(ctempArray1)[i - 1] = CZero;
		}
		for(int stop = 2 * NumWindings, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(int stop1 = NumWindings, k = 1; k <= stop1; k++)
			{
				/*# with WINDING^[k] do */
				{
					
					if(i == (2 * k - 1))
						(A)[k - 1] = cmplx((1.0 / ((WINDING_)[k - 1].VBase * ZeroTapFix((WINDING_)[k - 1].puTap))), 0.0);
					else
					{
						if(i == 2 * k)
							(A)[k - 1] = cmplx((-1.0 / ((WINDING_)[k - 1].VBase * ZeroTapFix((WINDING_)[k - 1].puTap))), 0.0);
						else
							(A)[k - 1] = CZero;
					}
				}
			}
       /*Main AutoTrans part*/
			Y_1Volt->MVmult(&ctempArray1[0], &A[0]);
			At->MVmult(&ctempArray2[0], &ctempArray1[0]);    /*AT * Result*/
			for(int stop1 = 2 * NumWindings, j = 1; j <= stop1; j++)
			{
				Y_Term->SetElement(j, i, (ctempArray2)[j - 1]);
			}
       /*No Load part*/
			Y_1Volt_NL->MVmult(&ctempArray1[0], &A[0]);
			At->MVmult(&ctempArray2[0], &ctempArray1[0]);    /*AT * Result*/
			for(int stop1 = 2 * NumWindings, j = 1; j <= stop1; j++)
			{
				Y_Term_NL->SetElement(j, i, (ctempArray2)[j - 1]);
			}
		}

  /*******************************DEBUG*******************************************************/  
  /*******************************************************************************************/

     /*Add a small Admittance to both conductors of each winding so that
    the matrix will always invert even if the user neglects to define a voltage
    reference on all sides*/
		if(ppm_FloatFactor != 0.0)
			/*# with Y_Term do */
			{
				auto with4 = Y_Term;
				int stop = 0;
				for(int stop = NumWindings, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					Yadder = cmplx(0.0, (WINDING_)[i - 1].Y_PPM);
					for(int stop1 = (2 * i), j = (2 * i - 1); j <= stop1; j++)
					{
						with4->SetElement(j, j, cadd(with4->GetElement(j, j), Yadder));
					}
/*           SetElement(j, j, CmulReal_im(GetElement(j, j) , ppm_FloatFactorPlusOne));*/
				}
			}

/*******************************DEBUG*******************************************************/  
  /*******************************************************************************************/
		delete At;
	}
	Y_Terminal_Freqmult = FreqMult;
}
// Uses strandard Xfmrcode, but forces connection of first two windings.

void TAutoTransObj::FetchXfmrCode(const String Code)
{
	TXfmrCodeObj* Obj = nullptr;
	int i = 0;
	if(XfmrCodeClass == nullptr)
		XfmrCodeClass = (TXfmrCode*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("xfmrcode"));
	if(XfmrCodeClass->SetActive(Code))
	{
		int stop = 0;
		Obj = ((TXfmrCodeObj*) XfmrCodeClass->GetActiveObj());
		XfmrCode = LowerCase(Code);
    // set sizes and copy parameters
		Set_NPhases(Obj->Fnphases);
		SetNumWindings(Obj->NumWindings);
		Set_Nconds(Fnphases + 1); // forces reallocation of terminals and conductors
		for(int stop = NumWindings, i = 1; i <= stop; i++)
		{
			/*# with WINDING^[i] do */
			{
				
				switch(i)
				{
					case 	1:
					(WINDING_)[i - 1].Connection = SERIES;
					break;  // No Choice for 1st two
					case 	2:
					(WINDING_)[i - 1].Connection = WYE;
					break;  // Wye
					default:
					(WINDING_)[i - 1].Connection = Obj->WINDING_[i - 1].Connection;
					break;
				}
				(WINDING_)[i - 1].kVLL = Obj->WINDING_[i - 1].kVLL;
				(WINDING_)[i - 1].VBase = Obj->WINDING_[i - 1].VBase;
				(WINDING_)[i - 1].kVA = (double) Obj->WINDING_[i - 1].kVA;
				(WINDING_)[i - 1].puTap = Obj->WINDING_[i - 1].puTap;
				(WINDING_)[i - 1].Rpu = Obj->WINDING_[i - 1].Rpu;
				(WINDING_)[i - 1].RdcOhms = Obj->WINDING_[i - 1].RdcOhms;
				(WINDING_)[i - 1].RdcSpecified = true; //This will force calc of Rdcpu
				(WINDING_)[i - 1].TapIncrement = Obj->WINDING_[i - 1].TapIncrement;
				(WINDING_)[i - 1].MinTap = Obj->WINDING_[i - 1].MinTap;
				(WINDING_)[i - 1].MaxTap = Obj->WINDING_[i - 1].MaxTap;
				(WINDING_)[i - 1].NumTaps = Obj->WINDING_[i - 1].NumTaps;
			}
		}
		SetTermRef();
		puXHX = Obj->XHL;
		puXHT = Obj->XHT;
		puXXT = Obj->XLT;
		for(int stop = XSCSize(NumWindings), i = 1; i <= stop; i++)
		{
			(puXSC)[i - 1] = (Obj->XSC)[i - 1];
		}
		ThermalTimeConst = Obj->ThermalTimeConst;
		n_thermal = Obj->n_thermal;
		m_thermal = Obj->m_thermal;
		FLrise = Obj->FLrise;
		HSrise = Obj->HSrise;
		pctLoadLoss = Obj->pctLoadLoss;
		pctNoLoadLoss = Obj->pctNoLoadLoss;
		pctImag = Obj->pctImag;  // Omission corrected 12-14-18
		NormMaxHkVA = Obj->NormMaxHkVA;
		EmergMaxHkVA = Obj->EmergMaxHkVA;
		ppm_FloatFactor = Obj->ppm_FloatFactor;
		Yorder = Fnconds * Fnterms;
		Set_YprimInvalid(ActiveActor,true);
		Y_Terminal_Freqmult = 0.0;
		RecalcElementData(ActiveActor);
	}
	else
	DoSimpleMsg(String("Xfmr Code:") + Code + " not found.", 100180);
}




}  // namespace AutoTrans

