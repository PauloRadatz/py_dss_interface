

#pragma hdrstop

#include "Load.h"

#include "DSSGlobals.h"
#include <cstdlib> // malloc


using namespace std;


namespace Load
{

TLoadObj::TLoadObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TLoadObj::TLoadObj(String ClassName) : inherited(ClassName) {}
TLoadObj::TLoadObj() {}


TLoadObj* ActiveLoadObj = nullptr;
const int NumPropsThisClass = 38;
complex CDoubleOne = {};

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure FOR all Line objects

TLoad::TLoad()
{
	;
	Class_Name = "Load";
	DSSClassType = DSSClassType + LOAD_ELEMENT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLoad::~TLoad()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLoad::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "phases";
	PropertyName[2 - 1] = "bus1";
	PropertyName[3 - 1] = "kV";  //
	PropertyName[4 - 1] = "kW";
	PropertyName[5 - 1] = "pf";
	PropertyName[6 - 1] = "model";
	PropertyName[7 - 1] = "yearly";
	PropertyName[8 - 1] = "daily";
	PropertyName[9 - 1] = "duty";
	PropertyName[10 - 1] = "growth";
	PropertyName[11 - 1] = "conn";
	PropertyName[12 - 1] = "kvar";
	PropertyName[13 - 1] = "Rneut"; // IF entered -, assume open
	PropertyName[14 - 1] = "Xneut";
	PropertyName[15 - 1] = "status";  // fixed or variable
	PropertyName[16 - 1] = "class";  // integer
	PropertyName[17 - 1] = "Vminpu";  // Min pu voltage for which model applies
	PropertyName[18 - 1] = "Vmaxpu";  // Max pu voltage for which model applies
	PropertyName[19 - 1] = "Vminnorm";  // Min pu voltage normal load
	PropertyName[20 - 1] = "Vminemerg";  // Min pu voltage emergency rating
	PropertyName[21 - 1] = "xfkVA";  // Service transformer rated kVA
	PropertyName[22 - 1] = "allocationfactor";  // allocation factor  for xfkVA
	PropertyName[23 - 1] = "kVA";  // specify load in kVA and PF
	PropertyName[24 - 1] = "%mean";  // per cent default mean
	PropertyName[25 - 1] = "%stddev";  // per cent default standard deviation
	PropertyName[26 - 1] = "CVRwatts";  // Percent watts reduction per 1% reduction in voltage from nominal
	PropertyName[27 - 1] = "CVRvars";  // Percent vars reduction per 1% reduction in voltage from nominal
	PropertyName[28 - 1] = "kwh";   // kwh billing
	PropertyName[29 - 1] = "kwhdays";   // kwh billing period (24-hr days)
	PropertyName[30 - 1] = "Cfactor";   // multiplier from kWh avg to peak kW
	PropertyName[31 - 1] = "CVRcurve";   // name of curve to use for yearly CVR simulations
	PropertyName[32 - 1] = "NumCust";   // Number of customers, this load
	PropertyName[33 - 1] = "ZIPV";      // array of 7 coefficients
	PropertyName[34 - 1] = "%SeriesRL";      // pct of Load that is series R-L
	PropertyName[35 - 1] = "RelWeight";      // Weighting factor for reliability
	PropertyName[36 - 1] = "Vlowpu";      // Below this value resort to constant Z model = Yeq
	PropertyName[37 - 1] = "puXharm";      // pu Reactance for Harmonics, if specifies
	PropertyName[38 - 1] = "XRharm";      // X/R at fundamental for series R-L model for hamonics
/*
  Typical Motor Parameters for motor
      Xpu = 0.20
      X/r typically 3-6 for normal motors; higher for high-eff motors
*/

     // define Property help values
	PropertyHelp[1 - 1] = "Number of Phases, this load.  Load is evenly divided among phases.";
	PropertyHelp[2 - 1] = "Bus to which the load is connected.  May include specific node specification.";
	PropertyHelp[3 - 1] = "Nominal rated (1.0 per unit) voltage, kV, for load. For 2- and 3-phase loads, specify phase-phase kV. "
	           "Otherwise, specify actual kV across each branch of the load. "
	           "If wye (star), specify phase-neutral kV. "
	           "If delta or phase-phase connected, specify phase-phase kV.";  // line-neutral voltage
	PropertyHelp[4 - 1] = String("Total base kW for the load.  Normally, you would enter the maximum kW for the load for the first year " "and allow it to be adjusted by the load shapes, growth shapes, and global load multiplier.") + CRLF
	           + CRLF
	           + "Legal ways to define base load:"
	           + CRLF
	           + "kW, PF"
	           + CRLF
	           + "kW, kvar"
	           + CRLF
	           + "kVA, PF"
	           + CRLF
	           + "XFKVA * Allocationfactor, PF"
	           + CRLF
	           + "kWh/(kWhdays*24) * Cfactor, PF";
	PropertyHelp[5 - 1] = "Load power factor.  Enter negative for leading powerfactor (when kW and kvar have opposite signs.)";
	PropertyHelp[6 - 1] = String("Integer code for the model to use for load variation with voltage. " "Valid values are:") + CRLF
	           + CRLF
	           + "1:Standard constant P+jQ load. (Default)"
	           + CRLF
	           + "2:Constant impedance load. "
	           + CRLF
	           + "3:Const P, Quadratic Q (like a motor)."
	           + CRLF
	           + "4:Nominal Linear P, Quadratic Q (feeder mix). Use this with CVRfactor."
	           + CRLF
	           + "5:Constant Current Magnitude"
	           + CRLF
	           + "6:Const P, Fixed Q"
	           + CRLF
	           + "7:Const P, Fixed Impedance Q"
	           + CRLF
	           + "8:ZIPV (7 values)"
	           + CRLF
	           + CRLF
	           + "For Types 6 and 7, only the P is modified by load multipliers.";
	PropertyHelp[7 - 1] = "LOADSHAPE object to use for yearly simulations.  Must be previously defined "
	           "as a Loadshape object. Is set to the Daily load shape "
	           " when Daily is defined.  The daily load shape is repeated in this case. "
	           "Set Status=Fixed to ignore Loadshape designation. "
	           "Set to NONE to reset to no loadahape. "
	           "The default is no variation.";
	PropertyHelp[8 - 1] = "LOADSHAPE object to use for daily simulations.  Must be previously defined "
	           "as a Loadshape object of 24 hrs, typically. "
	           "Set Status=Fixed to ignore Loadshape designation. "
	           "Set to NONE to reset to no loadahape. "
	           "Default is no variation (constant) if not defined. "
	           "Side effect: Sets Yearly load shape if not already defined.";
	PropertyHelp[9 - 1] = "LOADSHAPE object to use for duty cycle simulations.  Must be previously defined "
	           "as a Loadshape object.  Typically would have time intervals less than 1 hr. "
	           "Designate the number of points to solve using the Set Number=xxxx command. "
	           "If there are fewer points in the actual shape, the shape is assumed to repeat."
	           "Set to NONE to reset to no loadahape. "
	           "Set Status=Fixed to ignore Loadshape designation. "
	           " Defaults to Daily curve If not specified.";
	PropertyHelp[10 - 1] = "Characteristic  to use for growth factors by years.  Must be previously defined "
	           "as a Growthshape object. Defaults to circuit default growth factor (see Set Growth command).";
	PropertyHelp[11 - 1] = "={wye or LN | delta or LL}.  Default is wye.";
	PropertyHelp[12 - 1] = "Specify the base kvar for specifying load as kW & kvar.  Assumes kW has been already defined.  Alternative to specifying the power factor.  Side effect: "
	           " the power factor and kVA is altered to agree.";
	PropertyHelp[13 - 1] = "Default is -1. Neutral resistance of wye (star)-connected load in actual ohms. "
	           "If entered as a negative value, the neutral can be open, or floating, or it can be connected to "
	           "node 0 (ground), which is the usual default. "
	           "If >=0 be sure to explicitly specify the node connection for the neutral, or last, conductor. "
	           "Otherwise, the neutral impedance will be shorted to ground.";
	PropertyHelp[14 - 1] = "Neutral reactance of wye(star)-connected load in actual ohms.  May be + or -.";
	PropertyHelp[15 - 1] = "={Variable | Fixed | Exempt}.  Default is variable. If Fixed, no load multipliers apply;  however, growth "
	           "multipliers do apply.  All multipliers apply to Variable loads.  Exempt loads are not "
	           "modified by the global load multiplier, such as in load duration curves, etc.  Daily multipliers "
	           "do apply, so setting this property to Exempt is a good way to represent industrial load that stays the same"
	           " day-after-day for the period study.";  // fixed or variable
	PropertyHelp[16 - 1] = "An arbitrary integer number representing the class of load so that load values may "
	           "be segregated by load value. Default is 1; not used internally.";
	PropertyHelp[17 - 1] = "Default = 0.95.  Minimum per unit voltage for which the MODEL is assumed to apply. Lower end of normal voltage range."
	           "Below this value, the load model reverts to a constant impedance model that matches the model at the transition voltage. "
	           "See also \"Vlowpu\" which causes the model to match Model=2 below the transition voltage.";
	PropertyHelp[18 - 1] = "Default = 1.05.  Maximum per unit voltage for which the MODEL is assumed to apply. "
	           "Above this value, the load model reverts to a constant impedance model.";
	PropertyHelp[19 - 1] = "Minimum per unit voltage for load EEN evaluations, Normal limit.  Default = 0, which defaults to system \"vminnorm\" "
	           "property (see Set Command under Executive).  If this property is specified, it ALWAYS "
	           "overrides the system specification. This allows you to have different criteria for different loads. "
	           "Set to zero to revert to the default system value.";
	PropertyHelp[20 - 1] = "Minimum per unit voltage for load UE evaluations, Emergency limit.  Default = 0, which defaults to system \"vminemerg\" "
	           "property (see Set Command under Executive).  If this property is specified, it ALWAYS "
	           "overrides the system specification. This allows you to have different criteria for different loads. "
	           "Set to zero to revert to the default system value.";
	PropertyHelp[21 - 1] = "Default = 0.0.  Rated kVA of service transformer for allocating loads based on connected kVA "
	           "at a bus. Side effect:  kW, PF, and kvar are modified. See help on kVA.";
	PropertyHelp[22 - 1] = "Default = 0.5.  Allocation factor for allocating loads based on connected kVA "
	           "at a bus. Side effect:  kW, PF, and kvar are modified by multiplying this factor times the XFKVA (if > 0).";
	PropertyHelp[23 - 1] = String("Specify base Load in kVA (and power factor)") + CRLF
	           + CRLF
	           + "Legal ways to define base load:"
	           + CRLF
	           + "kW, PF"
	           + CRLF
	           + "kW, kvar"
	           + CRLF
	           + "kVA, PF"
	           + CRLF
	           + "XFKVA * Allocationfactor, PF"
	           + CRLF
	           + "kWh/(kWhdays*24) * Cfactor, PF";
	PropertyHelp[24 - 1] = "Percent mean value for load to use for monte carlo studies if no loadshape is assigned to this load. Default is 50.";
	PropertyHelp[25 - 1] = "Percent Std deviation value for load to use for monte carlo studies if no loadshape is assigned to this load. Default is 10.";
	PropertyHelp[26 - 1] = String("Percent reduction in active power (watts) per 1% reduction in voltage from 100% rated. Default=1. ") + CRLF
	           + " Typical values range from 0.4 to 0.8. Applies to Model=4 only."
	           + CRLF
	           + " Intended to represent conservation voltage reduction or voltage optimization measures.";
	PropertyHelp[27 - 1] = String("Percent reduction in reactive power (vars) per 1% reduction in voltage from 100% rated. Default=2. ") + CRLF
	           + " Typical values range from 2 to 3. Applies to Model=4 only."
	           + CRLF
	           + " Intended to represent conservation voltage reduction or voltage optimization measures.";
	PropertyHelp[28 - 1] = "kWh billed for this period. Default is 0. See help on kVA and Cfactor and kWhDays.";
	PropertyHelp[29 - 1] = "Length of kWh billing period in days (24 hr days). Default is 30. Average demand is computed using this value.";   // kwh billing period (24-hr days)
	PropertyHelp[30 - 1] = "Factor relating average kW to peak kW. Default is 4.0. See kWh and kWhdays. See kVA.";   // multiplier from kWh avg to peak kW
	PropertyHelp[31 - 1] = "Default is NONE. Curve describing both watt and var factors as a function of time. "
	           "Refers to a LoadShape object with both Mult and Qmult defined. "
	           "Define a Loadshape to agree with yearly or daily curve according to the type of analysis being done. "
	           "If NONE, the CVRwatts and CVRvars factors are used and assumed constant.";
	PropertyHelp[32 - 1] = "Number of customers, this load. Default is 1.";
	PropertyHelp[33 - 1] = String("Array of 7 coefficients:") + CRLF
	           + CRLF
	           + " Get_First() 3 are ZIP weighting factors for real power (should sum to 1)"
	           + CRLF
	           + " Next 3 are ZIP weighting factors for reactive power (should sum to 1)"
	           + CRLF
	           + " Last 1 is cut-off voltage in p.u. of base kV; load is 0 below this cut-off"
	           + CRLF
	           + " No defaults; all coefficients must be specified if using model=8.";
	PropertyHelp[34 - 1] = "Percent of load that is series R-L for Harmonic studies. Default is 50. Remainder is assumed to be parallel R and L. "
	           "This can have a significant impact on the amount of damping observed in Harmonics solutions.";
	PropertyHelp[35 - 1] = String("Relative weighting factor for reliability calcs. Default = 1. Used to designate high priority loads such as hospitals, etc. ") + CRLF
	           + CRLF
	           + "Is multiplied by number of customers and load kW during reliability calcs.";
	PropertyHelp[36 - 1] = "Default = 0.50.  Per unit voltage at which the model switches to same as constant Z model (model=2). "
	           "This allows more consistent convergence at very low voltaes due to opening switches or solving for fault situations.";
	PropertyHelp[37 - 1] = String("Special reactance, pu (based on kVA, kV properties), for the series impedance branch in the load model for HARMONICS analysis. " "Generally used to represent motor load blocked rotor reactance. " "If not specified (that is, set =0, the default value), the series branch is computed from the percentage of the " "nominal load at fundamental frequency specified by the %SERIESRL property. ") + CRLF
	           + CRLF
	           + "Applies to load model in HARMONICS mode only."
	           + CRLF
	           + CRLF
	           + "A typical value would be approximately 0.20 pu based on kVA * %SeriesRL / 100.0.";
	PropertyHelp[38 - 1] = "X/R ratio of the special harmonics mode reactance specified by the puXHARM property at fundamental frequency. Default is 6. ";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
	PropertyHelp[NumPropsThisClass + 1] = "Name of harmonic current spectrum for this load.  Default is \"defaultload\", which is defined when the DSS starts.";
}
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLoad::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new load object and add it to Load class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TLoadObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLoad::SetNcondsForConnection()
{
	/*# with ActiveLoadObj do */
	{
		auto with0 = ActiveLoadObj;
		switch(with0->Connection)
		{
			case 	0:
			with0->Set_Nconds(with0->Fnphases + 1);
			break;
			case 	1:
			switch(with0->Fnphases)
			{
				case 	1:
				 case 2:
				with0->Set_Nconds(with0->Fnphases + 1);
				break; // L-L and Open-delta
				default:
				with0->Set_Nconds(with0->Fnphases);
				break;
			}
			break;  /*nada*/
			default:
			  ;
			break;
		}
	}
}

//---------------------------------------------------------------------------

double TLoadObj::get_FAllocationFactor()
{
	return FAllocationFactor;
}

//---------------------------------------------------------------------------

double TLoadObj::get_FCFactor()
{
	return FCFactor;
}

//---------------------------------------------------------------------------

double TLoadObj::get_FkWhDays()
{
	return FkWhDays;
}

//---------------------------------------------------------------------------

double TLoadObj::get_FkWh()
{
	return FkWh;
}

//---------------------------------------------------------------------------

double TLoadObj::get_FConnectedkVA()
{
	return FConnectedkVA;
}

//---------------------------------------------------------------------------

double TLoadObj::get_FkVAAllocationFactor()
{
	return FkVAAllocationFactor;
}

//---------------------------------------------------------------------------

double TLoadObj::get_FpuMean()
{
	return FpuMean;
}

//---------------------------------------------------------------------------

double TLoadObj::get_FpuStdDev()
{
	return FpuStdDev;
}

//---------------------------------------------------------------------------

double TLoadObj::get_FCVRwattFactor()
{
	return FCVRwattFactor;
}

//---------------------------------------------------------------------------

double TLoadObj::get_FCVRvarFactor()
{
	return FCVRvarFactor;
}

//---------------------------------------------------------------------------

double TLoadObj::get_Vmaxpu()
{
	return Vmaxpu;
}

//---------------------------------------------------------------------------

double TLoadObj::get_VminEmerg()
{
	return VminEmerg;
}

//---------------------------------------------------------------------------

double TLoadObj::get_VminNormal()
{
	return VminNormal;
}

//---------------------------------------------------------------------------

double TLoadObj::get_Vminpu()
{
	return Vminpu;
}

//---------------------------------------------------------------------------

bool TLoadObj::get_ExemptFromLDCurve()
{
	return ExemptFromLDCurve;
}

//---------------------------------------------------------------------------

bool TLoadObj::get_FIXED()
{
	return FIXED;
}

//---------------------------------------------------------------------------

int TLoadObj::get_FnZIPV()
{
	return FnZIPV;
}

//---------------------------------------------------------------------------

bool TLoadObj::get_PFSpecified()
{
	return PFSpecified;
}

//---------------------------------------------------------------------------


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// Accepts     (checks only min number of chars required}
//    delta or LL           (Case insensitive)
//    Y, wye, or LN

void TLoad::InterpretConnection(const String s)
{
	String TestS;
	/*# with ActiveLoadObj do */
	{
		auto with0 = ActiveLoadObj;
		TestS = LowerCase(s);
		switch(TestS[0])
		{
			case 	L'y':
			 case L'w':
			with0->Connection = 0;
			break;  /*Wye*/
			case 	L'd':
			with0->Connection = 1;
			break;  /*Delta or line-Line*/
			case 	L'l':
			switch(TestS[2])
			{
				case 	L'n':
				with0->Connection = 0;
				break;
				case 	L'l':
				with0->Connection = 1;
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
		SetNcondsForConnection();
		switch(with0->Connection)
		{
			case 	1:
			with0->VBase = with0->kVLoadBase * 1000.0;
			break;
			default:
			switch(with0->Fnphases)
			{
				case 	2: case 3:
				with0->VBase = with0->kVLoadBase * InvSQRT3x1000;
				break;
				default:
				with0->VBase = with0->kVLoadBase * 1000.0;
				break;
			}
			break;
		}
		with0->VBase95 = with0->Vminpu * with0->VBase;
		with0->VBase105 = with0->Vmaxpu * with0->VBase;
		with0->VBaseLow = with0->VLowpu * with0->VBase;
		with0->Yorder = with0->Fnconds * with0->Fnterms;
		with0->Set_YprimInvalid(ActiveActor,true);
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLoad::Edit(int ActorID)
{
	int		result = 0,
			ParamPointer = 0;
	String	ParamName = "",
			Param = "";
  // continue parsing WITH contents of Parser
	ActiveLoadObj = (TLoadObj *) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveLoadObj);
	result = 0;
	/*# with ActiveLoadObj do */
	{
		auto with0 = ActiveLoadObj;
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while((Param.size() > 0))
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
	           + "\"", 580);
				break;
				case 	1:
				with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
				break; // num phases
				case 	2:
				with0->SetBus(1, Param);
				break;
				case 	3:
				with0->kVLoadBase = Parser[ActorID]->MakeDouble_();
				break;
				case 	4:
				with0->kWBase = Parser[ActorID]->MakeDouble_();
				break;
				case 	5:
				with0->PFNominal = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				with0->FLoadModel = Parser[ActorID]->MakeInteger_();
				break;
				case 	7:
				with0->YearlyShape = Param;
				break;
				case 	8:
				with0->DailyShape = Param;
				break;
				case 	9:
				with0->DutyShape = Param;
				break;
				case 	10:
				with0->GrowthShape = Param;
				break;
				case 	11:
				InterpretConnection(Param);
				break;
				case 	12:
				with0->kvarBase = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->Rneut = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->Xneut = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				switch(LowerCase(Param)[0])
				{
					case 	L'f':
					{
						with0->FIXED = true;
						with0->ExemptFromLDCurve = false;
					}
					break;
					case 	L'e':
					{
						with0->FIXED = false;
						with0->ExemptFromLDCurve = true;
					}
					break;
					default:
					with0->FIXED = false;
					with0->ExemptFromLDCurve = false;
					break;
				}
				break;
				case 	16:
				with0->LoadClass = Parser[ActorID]->MakeInteger_();
				break;
				case 	17:
				with0->Vminpu = Parser[ActorID]->MakeDouble_();
				break;
				case 	18:
				with0->Vmaxpu = Parser[ActorID]->MakeDouble_();
				break;
				case 	19:
				with0->VminNormal = Parser[ActorID]->MakeDouble_();
				break;
				case 	20:
				with0->VminEmerg = Parser[ActorID]->MakeDouble_();
				break;
				case 	21:
				with0->Set_ConnectedkVA(Parser[ActorID]->MakeDouble_());
				break;
				case 	22:
				with0->Set_kVAAllocationFactor(Parser[ActorID]->MakeDouble_());
				break;
				case 	23:
				with0->kVABase = Parser[ActorID]->MakeDouble_();
				break;
				case 	24:
				with0->FpuMean = Parser[ActorID]->MakeDouble_() / 100.0;
				break;
				case 	25:
				with0->FpuStdDev = Parser[ActorID]->MakeDouble_() / 100.0;
				break;
				case 	26:
				with0->FCVRwattFactor = Parser[ActorID]->MakeDouble_();
				break;
				case 	27:
				with0->FCVRvarFactor = Parser[ActorID]->MakeDouble_();
				break;
				case 	28:
				with0->Set_kWh(Parser[ActorID]->MakeDouble_());
				break;
				case 	29:
				with0->Set_kWhDays(Parser[ActorID]->MakeDouble_());
				break;
				case 	30:
				with0->Set_CFactor(Parser[ActorID]->MakeDouble_());
				break;
				case 	31:
				with0->CVRshape = Param;
				break;
				case 	32:
				with0->NumCustomers = Parser[ActorID]->MakeInteger_();
				break;
				case 	33:
				{
					with0->SetZIPVSize(7);
					Parser[ActorID]->ParseAsVector(7, with0->ZIPV);
				}
				break;
				case 	34:
				with0->puSeriesRL = Parser[ActorID]->MakeDouble_() / 100.0;
				break;
				case 	35:
				with0->RelWeighting = Parser[ActorID]->MakeDouble_();
				break;
				case 	36:
				with0->VLowpu = Parser[ActorID]->MakeDouble_();
				break;
				case 	37:
				with0->FpuXHarm = Parser[ActorID]->MakeDouble_();
				break;  // 0 means not set
				case 	38:
				with0->FXRHarmRatio = Parser[ActorID]->MakeDouble_();
				break;
           // Inherited edits
				default:
				inherited::ClassEdit(ActiveLoadObj, ParamPointer - NumPropsThisClass);
				break;
			}

         // << SIDE EFFECTS >>
         // keep kvar nominal up to date WITH kW and PF
			switch(ParamPointer)
			{
				case 	1:
				{
					SetNcondsForConnection();  // Force Reallocation of terminal info
					with0->UpdateVoltageBases();
				}
				break;
				case 	3:
				with0->UpdateVoltageBases();
				break;
				case 	4:
				{
					with0->LoadSpecType = 0;
					with0->kWref = with0->kWBase;
				}
				break;
				case 	5:
				{
					with0->PFChanged = true;
					with0->PFSpecified = true;
				}
				break;
    /*Set shape objects;  returns nil if not valid*/
    /*Sets the kW and kvar properties to match the peak kW demand from the Loadshape*/
				case 	7:
				{
					with0->YearlyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->YearlyShape));
					if(ASSIGNED(with0->YearlyShapeObj))
						/*# with YearlyShapeObj do */
						{
							auto with1 = with0->YearlyShapeObj;
							if(with1->UseActual)
							{
								with0->kWref = with0->kWBase;
								with0->kVARref = with0->kvarBase;
								with0->SetkWkvar(with1->MaxP, with1->MaxQ);
							}
						}
				}
				break;
				case 	8:
				{
					with0->DailyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DailyShape));
					if(ASSIGNED(with0->DailyShapeObj))
						/*# with DailyShapeObj do */
						{
							auto with2 = with0->DailyShapeObj;
							if(with2->UseActual)
								with0->SetkWkvar(with2->MaxP, with2->MaxQ);
						}
                /*If Yearly load shape is not yet defined, make it the same as Daily*/
					if(with0->YearlyShapeObj == nullptr)
						with0->YearlyShapeObj = with0->DailyShapeObj;
				}
				break;
				case 	9:
				{
					with0->DutyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DutyShape));
					if(ASSIGNED(with0->DutyShapeObj))
						/*# with DutyShapeObj do */
						{
							auto with3 = with0->DutyShapeObj;
							if(with3->UseActual)
								with0->SetkWkvar(with3->MaxP, with3->MaxQ);
						}
				}
				break;
				case 	10:
				with0->GrowthShapeObj = ((TGrowthShapeObj*) GrowthShapeClass[ActorID]->Find(with0->GrowthShape));
				break;
				case 	12:
				{
					with0->LoadSpecType = 1;
					with0->PFSpecified = false;
					with0->kVARref = with0->kvarBase;
				}
				break;// kW, kvar
				 /**** see set_xfkva, etc           21, 22: LoadSpectype := 3;  // XFKVA*AllocationFactor, PF  */
				case 	23:
				with0->LoadSpecType = 2;
				break;  // kVA, PF
				 /**** see set_kwh, etc           28..30: LoadSpecType := 4;  // kWh, days, cfactor, PF */
				case 	31:
				with0->CVRShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->CVRshape));
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

int TLoad::MakeLike(const String OtherLoadName)
{
	int result = 0;
	TLoadObj* OtherLoad = nullptr;
	int i = 0;
	result = 0;
   /*See IF we can find this line name in the present collection*/
	OtherLoad = ((TLoadObj*) Find(OtherLoadName));
	if(OtherLoad != nullptr)
		/*# with ActiveLoadObj do */
		{
			auto with0 = ActiveLoadObj;
			int stop = 0;
			with0->Connection = OtherLoad->Connection;
			if(with0->Fnphases != OtherLoad->Fnphases)
			{
				with0->Set_NPhases(OtherLoad->Fnphases);
				SetNcondsForConnection(); // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->kVLoadBase = OtherLoad->kVLoadBase;
			with0->VBase = OtherLoad->VBase;
			with0->VLowpu = OtherLoad->VLowpu;
			with0->Vminpu = OtherLoad->Vminpu;
			with0->Vmaxpu = OtherLoad->Vmaxpu;
			with0->VBaseLow = OtherLoad->VBaseLow;
			with0->VBase95 = OtherLoad->VBase95;
			with0->VBase105 = OtherLoad->VBase105;
			with0->kWBase = OtherLoad->kWBase;
			with0->kVABase = OtherLoad->kVABase;
			with0->kvarBase = OtherLoad->kvarBase;
			with0->LoadSpecType = OtherLoad->LoadSpecType;
			with0->WNominal = OtherLoad->WNominal;
			with0->PFNominal = OtherLoad->PFNominal;
			with0->varNominal = OtherLoad->varNominal;
			with0->Rneut = OtherLoad->Rneut;
			with0->Xneut = OtherLoad->Xneut;
			with0->CVRshape = OtherLoad->CVRshape;
			with0->CVRShapeObj = OtherLoad->CVRShapeObj;
			with0->DailyShape = OtherLoad->DailyShape;
			with0->DailyShapeObj = OtherLoad->DailyShapeObj;
			with0->DutyShape = OtherLoad->DutyShape;
			with0->DutyShapeObj = OtherLoad->DutyShapeObj;
			with0->YearlyShape = OtherLoad->YearlyShape;
			with0->YearlyShapeObj = OtherLoad->YearlyShapeObj;
			with0->GrowthShape = OtherLoad->GrowthShape;
			with0->GrowthShapeObj = OtherLoad->GrowthShapeObj;
//        Spectrum       := OtherLoad.Spectrum;       in base class now
//       SpectrumObj    := OtherLoad.SpectrumObj;
			with0->LoadClass = OtherLoad->LoadClass;
			with0->NumCustomers = OtherLoad->NumCustomers;
			with0->FLoadModel = OtherLoad->FLoadModel;
			with0->FIXED = OtherLoad->FIXED;
			with0->ExemptFromLDCurve = OtherLoad->ExemptFromLDCurve;
			with0->FkVAAllocationFactor = OtherLoad->FkVAAllocationFactor;
			with0->FConnectedkVA = OtherLoad->FConnectedkVA;
			with0->FCVRwattFactor = OtherLoad->FCVRwattFactor;
			with0->FCVRvarFactor = OtherLoad->FCVRvarFactor;
			with0->ShapeIsActual = OtherLoad->ShapeIsActual;
			with0->puSeriesRL = OtherLoad->puSeriesRL;
			with0->RelWeighting = OtherLoad->RelWeighting;
			with0->SetZIPVSize(OtherLoad->get_FnZIPV());
			for(stop = with0->FnZIPV, i = 1; i <= stop; i++)
			{
				(with0->ZIPV)[i] = (OtherLoad->ZIPV)[i];
			}
			ClassMakeLike(OtherLoad);  // Take care of inherited class properties
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherLoad->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Load MakeLike: \"") + OtherLoadName
	           + "\" Not Found.", 581);
	return result;
}

//----------------------------------------------------------------------------

int TLoad::Init(int Handle, int ActorID)
{
	int result = 0;
	TLoadObj* P = nullptr;
	if(Handle == 0)  // init all load objects
	{
		P = (TLoadObj*) ElementList.Get_First();
		while(P != nullptr)
		{
			P->Randomize(0);
			P = (TLoadObj*) ElementList.Get_Next();
		}
	}
	else
	{
		Set_Active(Handle);
		P = ((TLoadObj*) GetActiveObj());
		P->Randomize(0);
	}
	DoSimpleMsg("Need to finish implementation TLoad.Init", -1);
	result = 0;
	return result;
}

//----------------------------------------------------------------------------

TLoadObj::TLoadObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			PFChanged(false),
			FAllocationFactor(0.0),
			FkVAAllocationFactor(0.0),
			FConnectedkVA(0.0),
			FkWh(0.0),
			FkWhDays(0.0),
			FCFactor(0.0),
			FAvgkW(0.0),
			LastGrowthFactor(0.0),
			LastYear(0),
			LoadFundamental(0.0),
			LoadSolutionCount(0),
			OpenLoadSolutionCount(0),
			RandomMult(0.0),
			varBase(0.0),
			varNominal(0.0),
			VBase(0.0),
			VBase105(0.0),
			VBase95(0.0),
			VBaseLow(0.0),
			WNominal(0.0),
			YPrimOpenCond(nullptr),
			YQFixed(0.0),
			FpuXHarm(0.0),
			FXRHarmRatio(0.0),
			FpuMean(0.0),
			FpuStdDev(0.0),
			FCVRwattFactor(0.0),
			FCVRvarFactor(0.0),
			Vmaxpu(0.0),
			VminEmerg(0.0),
			VminNormal(0.0),
			Vminpu(0.0),
			VLowpu(0.0),
			ExemptFromLDCurve(false),
			FIXED(false),
			ShapeIsActual(false),
			PFSpecified(false),
			FnZIPV(0),
			Connection(0),
			DailyShapeObj(nullptr),
			DutyShapeObj(nullptr),
			EEN_Factor(0.0),
			GrowthShapeObj(nullptr),
			HasBeenAllocated(false),
			kWBase(0.0),
			kVABase(0.0),
			kWref(0.0),
			kVARref(0.0),
			kvarBase(0.0),
			kVLoadBase(0.0),
			LoadClass(0),
			NumCustomers(0),
			LoadSpecType(0),
			PFNominal(0.0),
			Rneut(0.0),
			UE_Factor(0.0),
			Xneut(0.0),
			YearlyShapeObj(nullptr),
			CVRShapeObj(nullptr),
			ZIPV(nullptr),
			puSeriesRL(0.0),
			RelWeighting(0.0),
			FLoadModel(0)
{
	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType;
	Fnphases = 3;
	Fnconds = 4;  // defaults to wye  so it has a 4th conductor
	Yorder = 0;  // To trigger an initial allocation
	Set_NTerms(1);  // forces allocations
	kWBase = 10.0;
	kvarBase = 5.0;
	PFNominal = 0.88;
	kVABase = kWBase / PFNominal;
	LoadSpecType = 0;
	Rneut = -1.0;  // signify neutral is open
	Xneut = 0.0;
	YearlyShape = "";
	YearlyShapeObj = nullptr;  // IF YearlyShapeobj = nil THEN the load alway stays nominal * global multipliers
	DailyShape = "";
	DailyShapeObj = nullptr;  // IF DaillyShapeobj = nil THEN the load alway stays nominal * global multipliers
	DutyShape = "";
	DutyShapeObj = nullptr;  // IF DutyShapeobj = nil THEN the load alway stays nominal * global multipliers
	GrowthShape = "";
	GrowthShapeObj = nullptr;  // IF grwothshapeobj = nil THEN the load alway stays nominal * global multipliers
	CVRshape = "";
	CVRShapeObj = nullptr;
	Connection = 0;    // Wye (star)
	FLoadModel = 1;  // changed from 2 RCD {easiest to solve}
	LoadClass = 1;
	NumCustomers = 1;
	LastYear = 0;
	FCVRwattFactor = 1.0;
	FCVRvarFactor = 2.0;
	RelWeighting = 1.0;
	LastGrowthFactor = 1.0;
	FkVAAllocationFactor = 0.5;
	FAllocationFactor = FkVAAllocationFactor;
	HasBeenAllocated = false;
	ShapeIsActual = false;
	PFSpecified = false;  // default to not specified by PF property
	LoadSolutionCount = -1;  // for keeping track of the present solution in Injcurrent calcs
	OpenLoadSolutionCount = -1;
	YPrimOpenCond = nullptr;
	FConnectedkVA = 0.0;  // Loadspectype=3
	FkWh = 0.0;  // Loadspectype=4
	FCFactor = 4.0;
	FkWhDays = 30.0;
	VminNormal = 0.0;    // indicates for program to use Circuit quantities
	VminEmerg = 0.0;
	kVLoadBase = 12.47;
	VBase = 7200.0;
	VLowpu = 0.50;
	Vminpu = 0.95;
	Vmaxpu = 1.05;
	VBaseLow = VLowpu * VBase;
	VBase95 = Vminpu * VBase;
	VBase105 = Vmaxpu * VBase;
	Yorder = Fnterms * Fnconds;
	RandomMult = 1.0;
	FIXED = false;
	ExemptFromLDCurve = false;
	FpuXHarm = 0.0;  // zero signifies not specified.
	FXRHarmRatio = 6.0;
	FpuMean = 0.5;
	FpuStdDev = 0.1;
	UE_Factor = 0.0;
	EEN_Factor = 0.0;
	Spectrum = "defaultload";  // override base class definition
	HarmMag.clear();
	HarmAng.clear();
	puSeriesRL = 0.50;
	ZIPV = nullptr;
	SetZIPVSize(0);
	FPhaseCurr.clear();  // storage for intermediate current computation
                          // allocated in Recalcelementdata
	InitPropertyValues(0);
	RecalcElementData(ActiveActor);
}


//----------------------------------------------------------------------------

TLoadObj::~TLoadObj()
{
	delete YPrimOpenCond;
	HarmMag.clear();
	HarmAng.clear();
	free(ZIPV);
	FPhaseCurr.clear();
	// inherited::Destroy();
}


void TLoadObj::SetZIPVSize(int n)
{
	FnZIPV = n;
	ZIPV = (pDoubleArray) realloc(ZIPV, sizeof(double) * FnZIPV);
}

//----------------------------------------------------------------------------

void TLoadObj::Randomize(int Opt)
{
	switch(Opt)
	{
		case 	0:
		RandomMult = 1.0;
		break;
		case 	GAUSSIAN:
		if(ASSIGNED(YearlyShapeObj))
			RandomMult = Gauss(YearlyShapeObj->Get_Mean(), YearlyShapeObj->Get_StdDev());
		else
			RandomMult = Gauss(FpuMean, FpuStdDev);
		break;
		case 	UNIFORM:
		RandomMult = (double) Random();
		break;  // number between 0 and 1.0
		case 	LOGNORMAL:
		if(ASSIGNED(YearlyShapeObj))
			RandomMult = QuasiLogNormal(YearlyShapeObj->Get_Mean());
		else
			RandomMult = QuasiLogNormal(FpuMean);
		break;
       /*nada*/
		default:
		  ;
		break;
	}
}

//----------------------------------------------------------------------------

void TLoadObj::CalcDailyMult(double hr)
{
	if(DailyShapeObj != nullptr)
	{
		ShapeFactor = DailyShapeObj->GetMult(hr);
		ShapeIsActual = DailyShapeObj->UseActual;
	}
	else
	ShapeFactor = cmplx(1.0, 1.0);  // Default to no daily variation
}


//----------------------------------------------------------------------------

void TLoadObj::CalcDutyMult(double hr)
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

void TLoadObj::CalcYearlyMult(double hr)
{

/*Yearly curve is assumed to be hourly only*/
	if(YearlyShapeObj != nullptr)
	{
		ShapeFactor = YearlyShapeObj->GetMult(hr);
		ShapeIsActual = YearlyShapeObj->UseActual;
	}
	else
	ShapeFactor = cmplx(1.0, 1.0);
                          // Defaults to no variation
}

//----------------------------------------------------------------------------

void TLoadObj::CalcCVRMult(double hr)
{
	complex CVRFactor = {};
  /*CVR curve is assumed to be used in a yearly simulation*/
	if(CVRShapeObj != nullptr)
	{
		CVRFactor = CVRShapeObj->GetMult(hr);    /*Complex*/
		FCVRwattFactor = CVRFactor.re;
		FCVRvarFactor = CVRFactor.im;
	}
   /*Else FCVRWattFactor, etc. remain unchanged*/
}

//----------------------------------------------------------------------------

double TLoadObj::GrowthFactor(int Year, int ActorID)
{
	double result = 0.0;
	if(Year == 0)  // default all to 1 in year 0 ; use base values
		LastGrowthFactor = 1.0;
	else
	{
		if(GrowthShapeObj == nullptr)
			LastGrowthFactor = ActiveCircuit[ActorID]->DefaultGrowthFactor;
		else
		{
			if(Year != LastYear)    // Search growthcurve
				LastGrowthFactor = GrowthShapeObj->GetMult(Year);
		}
	}
	result = LastGrowthFactor;  // for Now
	return result;
}


//----------------------------------------------------------------------------

void TLoadObj::SetkWkvar(double PkW, double Qkvar)
{
	kWBase = PkW;
	kvarBase = Qkvar;
	if(PFSpecified)
		LoadSpecType = 0;
	else
		LoadSpecType = 1;
}

void TLoadObj::SetNominalLoad(int ActorID)
{
	double Factor = 0.0;
	ShapeFactor = CDoubleOne;
	ShapeIsActual = false;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(FIXED)
		{
			Factor = GrowthFactor(with0->get_Fyear(), ActorID);   // For fixed loads, consider only growth factor
		}
		else
		switch(with0->Get_SolMode())
		{
			case 	SNAPSHOT:
			 case HARMONICMODE:
			if(ExemptFromLDCurve)
				Factor = GrowthFactor(with0->get_Fyear(), ActorID);
			else
				Factor = ActiveCircuit[ActorID]->get_FLoadMultiplier() * GrowthFactor(with0->get_Fyear(), ActorID);
			break;
			case 	DAILYMODE:
			{
				Factor = GrowthFactor(with0->get_Fyear(), ActorID);
				if(!ExemptFromLDCurve)
					Factor = Factor * ActiveCircuit[ActorID]->get_FLoadMultiplier();
				CalcDailyMult(with0->DynaVars.dblHour);
			}
			break;
			case 	EMPDAILYMODE:
			{
				Factor = GrowthFactor(with0->get_Fyear(), ActorID);
				if(!ExemptFromLDCurve)
					Factor = Factor * ActiveCircuit[ActorID]->get_FLoadMultiplier();
				CalcDailyMult(with0->DynaVars.dblHour);
			}
			break;
			case 	YEARLYMODE:
			{
				Factor = ActiveCircuit[ActorID]->get_FLoadMultiplier() * GrowthFactor(with0->get_Fyear(), ActorID);
				CalcYearlyMult(with0->DynaVars.dblHour);
				if(FLoadModel == 4)
					CalcCVRMult(with0->DynaVars.dblHour);
			}
			break;
			case 	DUTYCYCLE:
			{
				Factor = GrowthFactor(with0->get_Fyear(), ActorID);
				if(!ExemptFromLDCurve)
					Factor = Factor * ActiveCircuit[ActorID]->get_FLoadMultiplier();
				CalcDutyMult(with0->DynaVars.dblHour);
			}
			break;
			case 	GENERALTIME:
			 case DYNAMICMODE:
			{
				Factor = GrowthFactor(with0->get_Fyear(), ActorID);
				if(!ExemptFromLDCurve)
					Factor = Factor * ActiveCircuit[ActorID]->get_FLoadMultiplier();
                           // This mode allows use of one class of load shape
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
					default:     // default to 1 + j1 if not known
					ShapeFactor = CDoubleOne;
					break;
				}
			}
			break;
			case 	MONTECARLO1:
			{
				Randomize(with0->RandomType);
				Factor = RandomMult * GrowthFactor(with0->get_Fyear(), ActorID);
				if(!ExemptFromLDCurve)
					Factor = Factor * ActiveCircuit[ActorID]->get_FLoadMultiplier();
			}
			break;
			case 	MONTECARLO2:
			 case MONTECARLO3:
			 case LOADDURATION1:
			 case LOADDURATION2:
			{
				Factor = GrowthFactor(with0->get_Fyear(), ActorID);
				CalcDailyMult(with0->DynaVars.dblHour);
				if(!ExemptFromLDCurve)
					Factor = Factor * ActiveCircuit[ActorID]->get_FLoadMultiplier();
			}
			break;
			case 	PEAKDAY:
			{
				Factor = GrowthFactor(with0->get_Fyear(), ActorID);
				CalcDailyMult(with0->DynaVars.dblHour);
			}
			break;
			case 	AUTOADDFLAG:
			Factor = GrowthFactor(with0->get_Fyear(), ActorID);
			break;  // Loadmult = 1.0 by default
			default:    // defaults to Base kW * growth
			Factor = GrowthFactor(with0->get_Fyear(), ActorID);
			break;
		}
	}
	if(ShapeIsActual)
	{
		WNominal = 1000.0 * ShapeFactor.re / Fnphases;
		varNominal = 0.0; // initialize  for unity PF  and check for change
		if(ShapeFactor.im != 0.0)   // Qmult was specified
			varNominal = 1000.0 * ShapeFactor.im / Fnphases;
		else
		{
			if(PFSpecified && (PFNominal != 1.0))  // Qmult not specified but PF was
  // user specified the PF for this load
			{
				varNominal = WNominal * sqrt((1.0L / Sqr(PFNominal) - 1.0L));
				if(PFNominal < 0.0) // watts and vare are in opposite directions
					varNominal = -varNominal;
			}
		}
	}
	else
	{
		WNominal = 1000.0 * kWBase * Factor * ShapeFactor.re / Fnphases;
		varNominal = 1000.0 * kvarBase * Factor * ShapeFactor.im / Fnphases;
	}
	Yeq = cdivreal(cmplx(WNominal, -varNominal), Sqr(VBase));
	if(Vminpu != 0.0)   // at 95% voltage
		Yeq95 = cdivreal(Yeq, Sqr(Vminpu));
	else
		Yeq95 = CZero;
	if(Vmaxpu != 0.0)   // at 105% voltage
		Yeq105 = cdivreal(Yeq, Sqr(Vmaxpu));
	else
		Yeq105 = Yeq;
	if(Vmaxpu != 0.0)   // at 105% voltage for Constant I ***Added by Celso & Paulo
		Yeq105I = cdivreal(Yeq, Vmaxpu);
	else
		Yeq105I = Yeq;                    // **Added by Celso & Paulo
		
    /*New code to help with convergence at low voltages*/
	ILow = (cmulreal(Yeq, VBaseLow));
	I95 = (cmulreal(Yeq95, VBase95));
	M95 = cdivreal(csub(I95, ILow), (VBase95 - VBaseLow)); // (I95 - ILow)/(Vbase95 - VbaseLow);
	M95 = cdivreal(csub(I95, ILow), (VBase95 - VBaseLow)); // (I95 - ILow)/(Vbase95 - VbaseLow);    ***Added by Celso & Paulo
	IBase = (cmulreal(Yeq, VBase));                          // ***Added by Celso & Paulo
	M95I = cdivreal(csub(IBase, ILow), (VBase95 - VBaseLow)); // (IBase - ILow)/(Vbase95 - VbaseLow);    ***Added by Celso & Paulo
}

//----------------------------------------------------------------------------

void TLoadObj::RecalcElementData(int ActorID)
{
	VBaseLow = VLowpu * VBase;
	VBase95 = Vminpu * VBase;
	VBase105 = Vmaxpu * VBase;



    /*Set kW and kvar from root values of kVA and PF*/
	switch(LoadSpecType)
	{
		case 	0:  /*kW, PF*/
		{
			kvarBase = kWBase * sqrt(1.0L / Sqr(PFNominal) - 1.0L);
			if(PFNominal < 0.0)
				kvarBase = -kvarBase;
			kVABase = sqrt(Sqr(kWBase) + Sqr(kvarBase));
		}
		break;  /*kW, kvar -- need to set PFNominal*/
		case 	1:
		{
			kVABase = sqrt(Sqr(kWBase) + Sqr(kvarBase));
			if(kVABase > 0.0)
			{
				PFNominal = kWBase / kVABase;
               /*If kW and kvar are different signs, PF is negative*/
				if(kvarBase != 0.0)
					PFNominal = PFNominal * Sign(kWBase * kvarBase);
			}
          /*Else leave it as it is*/
		}
		break;  /*kVA, PF*/
		case 	2:
		{
			kWBase = kVABase * Abs(PFNominal);
			kWref = kWBase;
			kvarBase = kWBase * sqrt(1.0L / Sqr(PFNominal) - 1.0L);
			kVARref = kvarBase;
			if(PFNominal < 0.0)
				kvarBase = -kvarBase;
		}
		break;
		case 	3:
		 case 4:
		if(PFChanged)  // Recompute kvarBase
		{
			kvarBase = kWBase * sqrt(1.0L / Sqr(PFNominal) - 1.0L);
			if(PFNominal < 0.0)
				kvarBase = -kvarBase;
			kVABase = sqrt(Sqr(kWref) + Sqr(kVARref));
		}
		break;

           
/* done automagically in Property set...      3, 4: ComputeAllocatedLoad;    */
		default:
		  ;
		break;
	}
	SetNominalLoad(ActorID);

    /*Now check for errors.  IF any of these came out nil and the string was not nil, give warning*/
	if(CompareText(YearlyShape, "none") == 0)
		YearlyShape = "";
	if(CompareText(DailyShape, "none") == 0)
		DailyShape = "";
	if(CompareText(DutyShape, "none") == 0)
		DutyShape = "";
	if(YearlyShapeObj == nullptr)
	{
		if(YearlyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Yearly load shape: \"") + YearlyShape
	           + "\" Not Found.", 583);
	}
	if(DailyShapeObj == nullptr)
	{
		if(DailyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Daily load shape: \"") + DailyShape
	           + "\" Not Found.", 584);
	}
	if(DutyShapeObj == nullptr)
	{
		if(DutyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Duty load shape: \"") + DutyShape
	           + "\" Not Found.", 585);
	}
	if(GrowthShapeObj == nullptr)
	{
		if(GrowthShape.size() > 0)
			DoSimpleMsg(String("WARNING! Yearly Growth shape: \"") + GrowthShape
	           + "\" Not Found.", 586);
	}
	if(CVRShapeObj == nullptr)
	{
		if(CVRshape.size() > 0)
			DoSimpleMsg(String("WARNING! CVR Shape shape: \"") + CVRshape
	           + "\" Not Found.", 586);
	}
	SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
	if(SpectrumObj == nullptr)
		DoSimpleMsg(String("ERROR! Spectrum \"") + Spectrum + "\" Not Found.", 587);
	if(Rneut < 0.0)  // flag FOR open neutral
		Yneut = cmplx(0.0, 0.0);
	else
	{
		if((Rneut == 0.0) && (Xneut == 0.0)) // Solidly Grounded
  // 1 microohm resistor
			Yneut = cmplx(1.0e6, 0.0);
		else
			Yneut = cinv(cmplx(Rneut, Xneut));
	}
	varBase = 1000.0 * kvarBase / Fnphases;
	YQFixed = -varBase / Sqr(VBase);
	InjCurrent = static_cast<complex*>(realloc(InjCurrent, sizeof(complex) * Yorder));
	FPhaseCurr.resize( Fnphases + 1);
	PFChanged = false;
}

//----------------------------------------------------------------------------

void TLoadObj::CalcYPrimMatrix(TcMatrix* Ymatrix, int ActorID)
{
	complex Y = {};
	complex Yij = {};
	complex ZSeries = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;
	double XseriesOhms = 0.0;
	FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
	FreqMultiplier = FYprimFreq / BaseFrequency;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(with0->IsHarmonicModel && (with0->get_FFrequency() != ActiveCircuit[ActorID]->Fundamental))     // Harmonic Mode  and other than fundamental frequency
		{
			if(ActiveCircuit[ActorID]->NeglectLoadY)
                     /*Just a small value so things don't die and we get the actual injection current out the terminal*/
			{
				Y = cmplx(EPSILON, 0.0);
			}
			else

                // compute equivalent Y assuming some of the load is series R-L and the rest is parallel R-L

                   // Parallel R-L part of the Load model for harmonics mode
                   // Based in equivalent Y at 100% voltage
			{
				Y = cmulreal(Yeq, (1.0 - puSeriesRL));
				Y.im = Y.im / FreqMultiplier;  /*Correct reactive part for frequency*/

                   // Series-connected R-L part
				if(puSeriesRL != 0.0)
				{
					if(FpuXHarm > 0.0)   // compute Zseries from special harmonic reactance for representing motors.
                             // the series branch is assumed to represent the motor
					{
						XseriesOhms = Sqr(kVLoadBase) * 1000.0L / (kVABase * puSeriesRL) * FpuXHarm;
						ZSeries = cmplx(XseriesOhms / FXRHarmRatio, XseriesOhms);
					}
					else
    // Compute Zseries from nominal load value
					ZSeries = cinv(cmulreal(Yeq, puSeriesRL));
					ZSeries.im = ZSeries.im * FreqMultiplier;  /*Correct reactive part for frequency*/
					Y = cadd(cinv(ZSeries), Y); // convert to admittance and add into Y
				}
			}
		}
		else
   // not Harmonic mode
		{
			Y = Yeq;
			Y.im = Y.im / FreqMultiplier;  /*Correct reactive part for frequency*/
		}
	}
	Yij = cnegate(Y);
	switch(Connection)
	{
		case 	0: // WYE
		{
			int stop = 0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				Ymatrix->SetElement(i, i, Y);
				Ymatrix->AddElement(Fnconds, Fnconds, Y);
				Ymatrix->SetElemsym(i, Fnconds, Yij);
			}
			Ymatrix->AddElement(Fnconds, Fnconds, Yneut);  // Neutral

               /* If neutral is floating, make sure there is some small
                 connection to ground  by increasing the last diagonal slightly */
			if(Rneut < 0.0)
				Ymatrix->SetElement(Fnconds, Fnconds, cmulreal(Ymatrix->GetElement(Fnconds, Fnconds), 1.000001));
		}
		break;  // Delta  or L-L
		case 	1:
		{
			int stop = 0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				j = i + 1;
				if(j > Fnconds)
					j = 1;  // wrap around for closed connections
				Ymatrix->AddElement(i, i, Y);
				Ymatrix->AddElement(j, j, Y);
				Ymatrix->AddElemsym(i, j, Yij);   // get both off-diagonal elements
			}
		}
		break;
		default:
		  ;
		break;
	}
}


//----------------------------------------------------------------------------



// If doing an analysis that requires the load to be modeled as an impedance
// then put all in.

void TLoadObj::CalcYPrim(int ActorID)
{
	int i = 0;

// Build only YPrim Shunt for a Load  then Copy to YPrim
// Build a dummy Yprim Series so that CalcV does not fail
	int stop = 0;
	if(Get_YprimInvalid(ActorID,0))
	{
		if(YPrim_Shunt != nullptr)
			delete YPrim_Shunt; // YPrim_Shunt->~TcMatrix();
		if(YPrim_Series != nullptr)
			delete YPrim_Series; // YPrim_Series->~TcMatrix();
		if(YPrim != nullptr)
			delete YPrim; // YPrim->~TcMatrix();
		YPrim_Series = new TcMatrix(Yorder);
		YPrim_Shunt = new TcMatrix(Yorder);
		YPrim = new TcMatrix(Yorder);
	}
	else
	{
		YPrim_Shunt->Clear();
		YPrim_Series->Clear();
		YPrim->Clear();
	}
	if(ActiveCircuit[ActorID]->Solution->LoadModel == POWERFLOW)
	{
		SetNominalLoad(ActorID);         // same as admittance model
		CalcYPrimMatrix(YPrim_Shunt, ActorID);
	}
	else
   // ADMITTANCE model wanted
	{
		SetNominalLoad(ActorID);
		CalcYPrimMatrix(YPrim_Shunt, ActorID);
	}

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
	for(stop = Yorder, i = 1; i <= stop; i++)
	{
		YPrim_Series->SetElement(i, i, cmulreal(YPrim_Shunt->GetElement(i, i), 1.0e-10));
	}
	YPrim->CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
	inherited::CalcYPrim(ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

 /*Put the current into the proper location according to connection*/

void TLoadObj::StickCurrInTerminalArray(pComplexArray TermArray, const complex& Curr, int i)
{
	int j = 0;
	switch(Connection)
	{
		case 	0:  //Wye
		{
			caccum(TermArray[i - 1], cnegate(Curr));
			caccum(TermArray[Fnconds - 1], Curr); // Neutral
		}
		break; //DELTA
		case 	1:
		{
			caccum(TermArray[i - 1], cnegate(Curr));
			j = i + 1;
			if(j > Fnconds)
				j = 1;  // rotate the phases
			caccum(TermArray[j - 1], Curr);
		}
		break;
		default:
		  ;
		break;
	}
}

void TLoadObj::UpdateVoltageBases()
{
	/*# with ActiveLoadObj do */
	{
		auto with0 = ActiveLoadObj;
		switch(with0->Connection)
		{
			case 	1:
			with0->VBase = with0->kVLoadBase * 1000.0;
			break;  /*wye*/
			default:
			switch(with0->Fnphases)
			{
				case 	2: case 3:
				with0->VBase = with0->kVLoadBase * InvSQRT3x1000;
				break;
				default:
				with0->VBase = with0->kVLoadBase * 1000.0; /*1-phase or unknown*/
				break;
			}
			break;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TLoadObj::DoConstantPQLoad(int ActorID)
{
	int		i		= 0;
	complex Curr	= CZero,
			V		= CZero;
	double	Vmag	= 0.0;
	int		stop	= 0;
	auto	with0	= ActiveCircuit[ActorID]->Solution;

	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID, true); // get actual voltage across each phase of the load
	ZeroITerminal();
	
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = Vterminal[i - 1];
		Vmag = cabs(V);
		if (Vmag <= VBaseLow)											// Below VbaseZ resort to linear equal to Yprim contribution
		{
			Curr = cmul(Yeq, V);
		}
		else
		{
			if (Vmag <= VBase95)										//  Voltage between Vminpu and Vlow
			{
				Curr = cmul(InterpolateY95_YLow(Vmag), V);
			}
			else
			{
				if (Vmag > VBase105)									// above 105% use an impedance model
				{
					Curr = cmul(Yeq105, V);
				}
				else
					Curr = conjg(cdiv(cmplx(WNominal, varNominal), V));
			}  // Above 95%, constant PQ

		// Save this value in case the Load value is different than the terminal value (see InitHarmonics)
		}
		FPhaseCurr[i - 1] = Curr;
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TLoadObj::DoConstantZLoad(int ActorID)
{
	int i = 0;
	complex Curr = {};

// Assume Yeq is kept up to date
	int stop = 0;
	auto with0 = ActiveCircuit[ActorID]->Solution;

	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID, true); // get actual voltage across each phase of the load
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		Curr = cmul(Yeq, Vterminal[i - 1]);

		// Save this value in case the Load value is different than the terminal value (see InitHarmonics)
		FPhaseCurr[i - 1] = Curr;
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

// Constant P, quadratic Q

void TLoadObj::DoMotorTypeLoad(int ActorID)
{
	int i = 0;
	complex Curr = {};
	complex V = {};
	double	Vmag	= 0.0;
	int		stop	= 0;
	auto	with0	= ActiveCircuit[ActorID]->Solution;

	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID, true); // get actual voltage across each phase of the load
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = Vterminal[i - 1];
		Vmag = cabs(V);
		if(Vmag <= VBaseLow)  // Below VbaseZ resort to linear equal to Yprim contribution
			Curr = cmul(Yeq, V);
		else
		{
			if(Vmag <= VBase95)   //  Voltage between Vminpu and Vlow
				Curr = cmul(InterpolateY95_YLow(Vmag), V);
			else
			{
				if(Vmag > VBase105)  // above 105% use an impedance model
					Curr = cmul(Yeq105, V);
				else
				{
					Curr = conjg(cdiv(cmplx(WNominal, 0.0), V));  // Above 95%, constant P
					caccum(Curr, cmul(cmplx(0.0, Yeq.im), V));  // add in Q component of current
				}
			}

      // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
		}
		FPhaseCurr[i - 1] = Curr;
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
}

// Constant Current Load

void TLoadObj::DoConstantILoad(int ActorID)
{
	int i = 0;
	complex V		= {};
	double Vmag		= 0.0;
	complex Curr	= {};

// Computes the current assuming the voltage mag is Vbase
// Just uses the phase angle off the voltage
	
/*
   Injection = [s/v]* = [ (P+jQ)/(Vbase * V/|V|)]*
*/
	int stop		= 0;
	auto	with0	= ActiveCircuit[ActorID]->Solution;

	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID, true); // get actual voltage across each phase of the load
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = Vterminal[i - 1];
		Vmag = cabs(V);
		if(Vmag <= VBaseLow)  // Below VbaseZ resort to linear equal to Yprim contribution
			Curr = cmul(Yeq, V);
		else
		{
			if(Vmag <= VBase95)   //  Voltage between Vminpu and Vlow    ***Added by Celso & Paulo
				Curr = cmul(InterpolateY95I_YLow(Vmag), V);
			else
			{
				if(Vmag > VBase105)  // above 105% use an impedance model
					Curr = cmul(Yeq105I, V);
				else
				{
					Curr = conjg(cdiv(cmplx(WNominal, varNominal), cmulreal(cdivreal(V, Vmag), VBase)));
				}
			}

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
		}
		FPhaseCurr[i - 1] = Curr;
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

	}
}

void TLoadObj::DoZIPVModel(int ActorID)
{
	int i = 0;
	complex Curr	= {};
	complex CurrZ	= {};
	complex CurrI	= {};
	complex CurrP	= {};
	complex V		= {};
	double	Vmag	= 0.0;
	double	vx		= 0.0;
	double	evx		= 0.0;
	double	yv		= 0.0;
	int		stop	= 0;
	auto	with0	= ActiveCircuit[ActorID]->Solution;

	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID, true); // get actual voltage across each phase of the load
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = Vterminal[i - 1];
		Vmag = cabs(V);

    /* May 31, 2016 changed to linear model below VbaseLow -- converges better for low voltage*/
		if(Vmag <= VBaseLow)  // Below VbaseZ resort to linear equal to Yprim contribution
			Curr = cmul(Yeq, V);
		else
		{
			if(Vmag <= VBase95)
			{
				CurrZ = cmul(cmplx(Yeq.re * ZIPV[1 - 1], Yeq.im * ZIPV[4 - 1]), V);    // ***Changed by Celso & Paulow
				CurrP = cmul(cmplx(InterpolateY95_YLow(Vmag).re * ZIPV[3 - 1], InterpolateY95_YLow(Vmag).im * ZIPV[6 - 1]), V);   // ***Changed by Celso & Paulo
				CurrI = cmul(cmplx(InterpolateY95I_YLow(Vmag).re * ZIPV[2 - 1], InterpolateY95I_YLow(Vmag).im * ZIPV[5 - 1]), V);  // ***Changed by Celso & Paulo
				Curr = cadd(CurrZ, cadd(CurrI, CurrP));   // ***Changed by Celso & Paulo
			}
			else
			{
				if(Vmag > VBase105)
				{
					CurrZ = cmul(cmplx(Yeq.re * ZIPV[1 - 1], Yeq.im * ZIPV[4 - 1]), V);   // ***Changed by Celso & Paulo
					CurrP = cmul(cmplx(Yeq105.re * ZIPV[3 - 1], Yeq105.im * ZIPV[6 - 1]), V);         // ***Changed by Celso & Paulo
					CurrI = cmul(cmplx(Yeq105I.re * ZIPV[2 - 1], Yeq105I.im * ZIPV[5 - 1]), V);       // ***Changed by Celso & Paulo
					Curr = cadd(CurrZ, cadd(CurrI, CurrP));
				}
				else
				{
					CurrZ = cmul(cmplx(Yeq.re * ZIPV[1 - 1], Yeq.im * ZIPV[4 - 1]), V);
					CurrI = conjg(cdiv(cmplx(WNominal * ZIPV[2 - 1], varNominal * ZIPV[5 - 1]), cmulreal(cdivreal(V, cabs(V)), VBase)));
					CurrP = conjg(cdiv(cmplx(WNominal * ZIPV[3 - 1], varNominal * ZIPV[6 - 1]), V));
					Curr = cadd(CurrZ, cadd(CurrI, CurrP));
				}

      // low-voltage drop-out
			}
			if(ZIPV[6] > 0.0)
			{
				vx = 500.0 * (Vmag / VBase - ZIPV[6]);
				evx = exp(2 * vx);
				yv = 0.5 * (1 + (evx - 1) / (evx + 1));
				Curr = cmulreal(Curr, yv);
			}
		}

    // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
		FPhaseCurr[i - 1] = Curr;
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

// Linear P, quadratic Q

void TLoadObj::DoCVRModel(int ActorID)
{
	int i = 0;
	complex V			= {};
	complex Curr		= {};
	complex Cvar		= {};  // var current
	double	WattFactor	= 0.0;
	double	VarFactor	= 0.0;
	double	Vmag		= 0.0;
	double	VRatio		= 0.0;
	auto	with0		= ActiveCircuit[ActorID]->Solution;

	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID, true); // get actual voltage across each phase of the load
	ZeroITerminal();
	try
	{
		int stop = 0;
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			V = Vterminal[i - 1];
			Vmag = cabs(V);
			if(Vmag <= VBaseLow)  // Below VbaseZ resort to linear equal to Yprim contribution
				Curr = cmul(Yeq, V);
			else
			{
				if(Vmag <= VBase95)   //  Voltage between Vminpu and Vlow
					Curr = cmul(InterpolateY95_YLow(Vmag), V);
				else
				{
					if(Vmag > VBase105)  // above 105% use an impedance model
						Curr = cmul(Yeq105, V);
					else
					{
						VRatio = Vmag / VBase;    // vbase is l-n FOR wye and l-l FOR delta


              // Linear factor adjustment does not converge for some reason while power adjust does easily
                 // WattFactor := (1.0 + FCVRwattFactor*(Vmag/VBase - 1.0));
						if(FCVRwattFactor != 1.0)
							WattFactor = pow(VRatio, FCVRwattFactor);
						else
							WattFactor = VRatio;  // old value (in error): 1.0;
						if(WattFactor > 0.0)
							Curr = conjg(cdiv(cmplx(WNominal * WattFactor, 0.0), V));
						else
							Curr = CZero; // P component of current
						if(Vmag == 0.0)    // Trap divide by zero error
							              /*Compute Q component of current*/
							Cvar = CZero;
						else
						{
							if(FCVRvarFactor == 2.0)  /*Check for easy, quick ones first*/
							{
								Cvar = cmul(cmplx(0.0, Yeq.im), V); // 2 is same as Constant impedance
							}
							else
							{
								if(FCVRvarFactor == 3.0)
								{
									VarFactor = pow(VRatio, 3);
									Cvar = conjg(cdiv(cmplx(0.0, varNominal * VarFactor), V));
								}
								else

                  /*Other Var factor code here if not squared or cubed*/
								{
									VarFactor = pow(VRatio, FCVRvarFactor);
									Cvar = conjg(cdiv(cmplx(0.0, varNominal * VarFactor), V));
								}
							}
						}
						caccum(Curr, Cvar);  // add in Q component of current
					}
				}

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
			}
			FPhaseCurr[i - 1] = Curr;
			StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
			set_ITerminalUpdated(true, ActorID);
			StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
		}
	}
	catch (std::exception &e)
	{
		{
			DoSimpleMsg("Error in Load." + get_Name() + ": " + (std::string)e.what(), 5871);
			throw ;
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

// Constant P, Fixed Q  Q is always kvarBase

void TLoadObj::DoFixedQ(int ActorID)
{
	int i = 0;
	complex Curr	= {};
	complex V		= {};
	double	Vmag	= 0.0;
	int		stop	= 0;
	auto	with0	= ActiveCircuit[ActorID]->Solution;

	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID, true); // get actual voltage across each phase of the load
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = Vterminal[i - 1];
		Vmag = cabs(V);
		if(Vmag <= VBaseLow)  // Below VbaseZ resort to linear equal to Yprim contribution
			Curr = cmul(Yeq, V);
		else
		{
			if(Vmag <= VBase95)  // Below 95% use an impedance model
				Curr = cmul(cmplx(Yeq95.re, YQFixed), V);
			else
			{
				if(Vmag > VBase105)  // above 105% use an impedance model
					Curr = cmul(cmplx(Yeq105.re, YQFixed), V);
				else
				{
					Curr = conjg(cdiv(cmplx(WNominal, varBase), V));
				}
			}

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
		}
		FPhaseCurr[i - 1] = Curr;
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

// Constant P, Fixed Q  Q is always a fixed Z derived from kvarBase

void TLoadObj::DoFixedQZ(int ActorID)
{
	int		i		= 0;
	complex Curr	= {};
	complex V		= {};
	double	Vmag	= 0.0;
	int		stop	= 0;
	auto	with0	= ActiveCircuit[ActorID]->Solution;

	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID, true); // get actual voltage across each phase of the load
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = Vterminal[i - 1];
		Vmag = cabs(V);
		if(Vmag <= VBaseLow)  // Below VbaseZ resort to linear equal to Yprim contribution
			Curr = cmul(Yeq, V);
		else
		{
			if(Vmag <= VBase95)  // Below 95% use an impedance model
				Curr = cmul(cmplx(Yeq95.re, YQFixed), V);
			else
			{
				if(Vmag > VBase105)
					Curr = cmul(cmplx(Yeq105.re, YQFixed), V);
				else
				{
					Curr = conjg(cdiv(cmplx(WNominal, 0.0), V)); // P component of current
					caccum(Curr, cmul(cmplx(0.0, YQFixed), V));  // add in Q component of current
				}
			}

        // Save this value in case the Load value is different than the terminal value (see InitHarmonics)
		}
		FPhaseCurr[i - 1] = Curr;
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

/*Compute Injection Current Only when in harmonics mode*/
/*Assumes spectrum is an ideal current source based on the fundamental current and spectrum*/

void TLoadObj::DoHarmonicMode(int ActorID)
{
	int i = 0;
	complex Curr = {};
	complex Mult = {};
	double LoadHarmonic = 0.0;

   /*Don't calc Vterminal here because it could be undefined!*/
	ZeroInjCurrent();
	ZeroITerminal();
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		int stop = 0;
		LoadHarmonic = with0->get_FFrequency() / LoadFundamental;    // Loadfundamental = frequency of solution when Harmonic mode entered
		Mult = SpectrumObj->GetMult(LoadHarmonic);
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			Curr = cmulreal(Mult, HarmMag[i]); // Get base harmonic magnitude
			RotatePhasorDeg(Curr, LoadHarmonic, HarmAng[i]);   // Time shift by fundamental
          // don't need to save Curr here like we do in Power Flow modes
			StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into InjCurrent array taking into account connection
			StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
          // NOTE: This is the value of ITerminal a Monitor will capture in Harmonics mode .. it captures the harmonic injection
			set_ITerminalUpdated(true, ActorID);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

bool TLoadObj::AllTerminalsClosed()
{
	bool result = false;
	int i = 0;
	int j = 0;
	int stop = 0;
	result = true;
	for(stop = Get_NTerms(), i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Get_NConds(), j = 1; j <= stop1; j++)
		{
			if(! ( Terminals[i - 1].Conductors[j - 1].Closed ))
			{
				result = false;
				return result;
			}
		}
	}
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TLoadObj::CalcVTerminalPhase(int ActorID, bool CheckAlg)
{
	int i = 0;
	int j = 0;

/* Establish phase voltages and stick in Vtemp*/
	auto with0 = ActiveCircuit[ActorID]->Solution;
	if ( (with0->Algorithm == NCIMSOLVE) && CheckAlg)
	{
		for (i = 1; i <= Fnphases; i++)
			Vterminal[i - 1] = with0->NodeV[NodeRef[i - 1]];
	}
	else
	{
		switch (Connection)
		{
		case 	0:
		{
			{
				for (i = 1; i <= Fnphases; i++)
					Vterminal[i - 1] = with0->VDiff(NodeRef[i - 1], NodeRef[Fnconds - 1], ActorID);
			}
		}
		break;
		case 	1:
		{
			{
				for (i = 1; i <= Fnphases; i++)
				{
					j = i + 1;
					if (j > Fnconds)
						j = 1;
					Vterminal[i - 1] = with0->VDiff(NodeRef[i - 1], NodeRef[j - 1], ActorID);
				}
			}
		}
		break;
		default:
			;
			break;
		}
	}
	LoadSolutionCount = ActiveCircuit[ActorID]->Solution->SolutionCount;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

// Calculates total load current and adds it properly into the InjCurrent array

// Need to implement DynamicMode sometime ...

void TLoadObj::CalcLoadModelContribution(int ActorID)
{
	set_ITerminalUpdated(false, ActorID);
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
	{
		
		auto with1 = ActiveCircuit[ActorID]->Solution;
          /*IF      IsDynamicModel THEN  DoDynamicMode
          ELSE*/
		if(with1->IsHarmonicModel && (with1->get_FFrequency() != ActiveCircuit[ActorID]->Fundamental))
			DoHarmonicMode(ActorID);
		else
			switch(FLoadModel)
			{
           //  compute total Load currents and Add into InjCurrent array;
				case 	1:
				DoConstantPQLoad(ActorID);
				break; // normal load-flow type load
				case 	2:
				DoConstantZLoad(ActorID);
				break;
				case 	3:
				DoMotorTypeLoad(ActorID);
				break;  // Constant P, Quadratic Q;
				case 	4:
				DoCVRModel(ActorID);
				break;       // mixed motor/resistive load   with CVR factors
				case 	5:
				DoConstantILoad(ActorID);
				break;
				case 	6:
				DoFixedQ(ActorID);
				break;         // Fixed Q
				case 	7:
				DoFixedQZ(ActorID);
				break;        // Fixed, constant Z Q
				case 	8:
				DoZIPVModel(ActorID);
				break;
				default:
				DoConstantZLoad(ActorID);     // FOR now, until we implement the other models.
				break;
			}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Fill InjCurrent array with the current values to use for injections.

void TLoadObj::CalcInjCurrentArray(int ActorID)
{
	int i = 0;
	int j = 0;
	int k = 0;

// IF a terminal is open, THEN standard load models don't apply, so check it out first
	if(AllTerminalsClosed())

// Now Get Injection Currents
	{
		CalcLoadModelContribution(ActorID);
	}
	else


   /// THIS MAY NOT WORK !!! WATCH FOR BAD RESULTS

   // some terminals not closed  use admittance model FOR injection
	{
		int stop = 0;
		if(OpenLoadSolutionCount != ActiveCircuit[ActorID]->Solution->SolutionCount)

      // Rebuild the Yprimopencond IF a new solution because values may have changed.

        // only reallocate when necessary
		{
			if(YPrimOpenCond == nullptr)
				YPrimOpenCond = new TcMatrix(Yorder);
			else
				YPrimOpenCond->Clear();
			if(YPrimOpenCond->get_Norder() != Yorder)
			{
				delete YPrimOpenCond;
				YPrimOpenCond = new TcMatrix(Yorder);
			}
			CalcYPrimMatrix(YPrimOpenCond, ActorID);

        /*Now Account FOR the Open Conductors*/
        /*For any conductor that is open, zero out row and column*/
			/*# with YPrimOpenCond do */
			{
				auto with0 = YPrimOpenCond;
				int stop = 0;
				k = 0;
				for(stop = Fnterms, i = 1; i <= stop; i++)
				{
					int stop1 = 0;
					for(stop1 = Fnconds, j = 1; j <= stop1; j++)
					{
						if(! ( Terminals[i - 1].Conductors[j - 1].Closed ) )
						{
							with0->ZeroRow(j + k);
							with0->ZeroCol(j + k);
							with0->SetElement(j + k, j + k, cmplx(1.0e-12, 0.0));  // In case node gets isolated
						}
					}
					k = k + Fnconds;
				}
			}
			OpenLoadSolutionCount = ActiveCircuit[ActorID]->Solution->SolutionCount;
		}
		ComputeVterminal(ActorID);
		YPrimOpenCond->MVmult(&(ComplexBuffer[0]), &(Vterminal[0]));
		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			(ComplexBuffer)[i] = cnegate((ComplexBuffer)[i]);
		}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Always return total terminal currents in the Curr array

void TLoadObj::GetTerminalCurrents(pComplexArray Curr, int ActorID)
{
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(IterminalSolutionCount[ActorID] != ActiveCircuit[ActorID]->Solution->SolutionCount)     // recalc the contribution
		{
			CalcLoadModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
		}
		inherited::GetTerminalCurrents(Curr, ActorID);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Get the injection currents and add them directly into the Currents array

int TLoadObj::InjCurrents(int ActorID)
{
	int result = 0;
	result = 0;
	if(Get_Enabled())
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			if (with0->LoadsNeedUpdating)
				SetNominalLoad(ActorID);					// Set the nominal kW, etc. for the type of solution being done
			CalcInjCurrentArray(ActorID);
			result = inherited::InjCurrents(ActorID);		// Add into Global Currents Array

		}
	return result;
}
/*
  For Vmag between V95 and Vlow, interpolate for equivalent  Y
*/

complex TLoadObj::InterpolateY95_YLow(double Vmag)
{
	complex result = {};
	result = cdivreal(cadd(ILow, cmulreal(M95, Vmag - VBaseLow)), Vmag);   //(Ilow + M95 * (Vmag - VBaseLow))/Vmag)

/*****
    WriteDLLDebugFile(Format('Iter=%d, Name="%s", Vmag=%.6g, Yeq=%.6g +j %.6g',
             [ActiveCircuit.Solution.iteration, Name, Vmag, Result.re, Result.im]));
 */
	return result;
}      // ***Added by Celso & Paulo
/*
  For Vmag between V95 and Vlow, interpolate for equivalent  Y
*/

complex TLoadObj::InterpolateY95I_YLow(double Vmag)
{
	complex result = {};
	result = cdivreal(cadd(ILow, cmulreal(M95I, Vmag - VBaseLow)), Vmag);   //(Ilow + M95I * (Vmag - VBaseLow))/Vmag)   // ***Changed by Celso & Paulo

/*****
    WriteDLLDebugFile(Format('Iter=%d, Name="%s", Vmag=%.6g, Yeq=%.6g +j %.6g',
             [ActiveCircuit[ActiveActor].Solution.iteration, Name, Vmag, Result.re, Result.im]));
 */
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Gets the injection  currents for the last solution performed
// Do not call SetNominalLoad, as that may change the load values

void TLoadObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	try
	{
		if(Get_Enabled())
		{
			int stop = 0;
			CalcInjCurrentArray(ActorID);
       // Copy into buffer array
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				(Curr)[i] = (InjCurrent)[i];
			}
		}
		else
		{
			int stop = 0;
			for(stop = Yorder, i = 1; i <= stop; i++)
			{
				(Curr)[i] = CZero;
			}
		}
	}
	catch (std::exception &e)
	{
		DoErrorMsg(String("Load Object: \"") + get_Name() + "\" in GetInjCurrents FUNCTION.", (std::string) e.what(), "Current buffer may not big enough.", 588);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

bool TLoadObj::Get_Unserved(int ActorID)
{
	bool result = false;
	int i = 0;
	double Vpu = 0.0;
	double Vmag = 0.0;
	double NormMinCriteria = 0.0;
	double EmergMinCriteria = 0.0;
  /*  Line overload takes precedence.
     Assumes that low voltage is due to overloaded line.
     IF voltage is below Emergency minumum, it is counted as  unserved.
  */
	int stop = 0;
	result = false;
	if(UE_Factor > 0.0)
	{
		result = true;
		return result;
	}

     /*ELSE Check Voltages*/
	if(LoadSolutionCount != ActiveCircuit[ActorID]->Solution->SolutionCount)
		CalcVTerminalPhase(ActorID);

     // Get the lowest of the Phase voltages
	Vpu = VBase;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		Vmag = cabs(Vterminal[i - 1]);
		if(Vmag < Vpu)
			Vpu = Vmag;
	}
	Vpu = Vpu / VBase;
	if(VminNormal != 0.0)
		NormMinCriteria = VminNormal;
	else
		NormMinCriteria = ActiveCircuit[ActorID]->NormalMinVolts;
	if(VminEmerg != 0.0)
		EmergMinCriteria = VminEmerg;
	else
		EmergMinCriteria = ActiveCircuit[ActorID]->EmergMinVolts;
	if(Vpu < EmergMinCriteria)
	{
		result = true;
         //UE_Factor := 1.0;
         // 9-19-00 RCD  let UE_Factor start small and grow linearly at same slope
         // as EEN_Factor
		UE_Factor = (EmergMinCriteria - Vpu) / (NormMinCriteria - EmergMinCriteria);
		return result;
	}
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

bool TLoadObj::Get_ExceedsNormal(int ActorID)
{
	bool result = false;
	int i = 0;
	double Vpu = 0.0;
	double Vmag = 0.0;

  /*  Line overload takes precedence.
     Assumes that low voltage is due to overloaded line.
     IF voltage is below Normal minumum, it is counted as unserved in proportion
     to the difference between the normal and emergency voltage limits.
  */
	double NormMinCriteria = 0.0;
	double EmergMinCriteria = 0.0;

/* 1-4-00  Added Vpu*/
	int stop = 0;
	result = false;
	if(EEN_Factor > 0.0)
	{
		result = true;
		return result;
	}   // Check line overload
	if(LoadSolutionCount != ActiveCircuit[ActorID]->Solution->SolutionCount)
		CalcVTerminalPhase(ActorID);

     // Get the lowest of the Phase voltages
	Vpu = VBase;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		Vmag = cabs(Vterminal[i-1]);
		if(Vmag < Vpu)
			Vpu = Vmag;
	}
	Vpu = Vpu / VBase;
	if(VminNormal != 0.0)
		NormMinCriteria = VminNormal;
	else
		NormMinCriteria = ActiveCircuit[ActorID]->NormalMinVolts;
	if(VminEmerg != 0.0)
		EmergMinCriteria = VminEmerg;
	else
		EmergMinCriteria = ActiveCircuit[ActorID]->EmergMinVolts;
	if(Vpu < NormMinCriteria)
	{
		EEN_Factor = (NormMinCriteria - Vpu) / (NormMinCriteria - EmergMinCriteria);
       // 9-19-00 RCD  Let EEN factor grow linearly at same slope
       // IF EEN_Factor > 1.0 THEN EEN_Factor := 1.0;
		result = true;
		return result;
	}
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TLoadObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int j = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			switch(i)
			{
				case 	4:
				{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, kWBase, 8, 1); }
				break;
				case 	5:
				{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, PFNominal, 5, 3); }
				break;
				case 	12:
				{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, kvarBase, 8, 1); }
				break;
				case 	22:
				{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, FkVAAllocationFactor, 5, 3); }
				break;
				case 	23:
				{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, kVABase, 8, 1); }
				break;
				case 	33:
				{
					int stop1 = 0;
					{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); }
					for(stop1 = get_FnZIPV(), j = 1; j <= stop1; j++)
					{
						{ Write(f, (ZIPV)[j], 0, 2); Write(f, L' '); }
					}
					WriteLn(f, L'\"');
				}
				break;
				case 	34:
				{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, (puSeriesRL * 100.0), 8, 1); }
				break;
				default:
				{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
				break;
			}
		}
	}
}

void TLoadObj::Set_kVAAllocationFactor(double Value)
{
	FkVAAllocationFactor = Value;
	FAllocationFactor = Value;
	LoadSpecType = 3;
	ComputeAllocatedLoad();
	HasBeenAllocated = true;
}
/*This procedure is used by the energymeter allocateload function to adjust load allocation factors*/

void TLoadObj::Set_AllocationFactor(double Value)
{
	FAllocationFactor = Value;
	switch(LoadSpecType)
	{
		case 	3:
		FkVAAllocationFactor = Value;
		break;
		case 	4:
		FCFactor = Value;
		break;
		default:
		  ;
		break;
	}
	ComputeAllocatedLoad();  // update kWbase
	HasBeenAllocated = true;
}

void TLoadObj::Set_CFactor(double Value)
{
	FCFactor = Value;
	FAllocationFactor = Value;
	LoadSpecType = 4;
	ComputeAllocatedLoad();
	HasBeenAllocated = true;
}

void TLoadObj::Set_ConnectedkVA(double Value)
{
	FConnectedkVA = Value;
	LoadSpecType = 3;
	FAllocationFactor = FkVAAllocationFactor;
	ComputeAllocatedLoad();
}

void TLoadObj::Set_kWh(double Value)
{
	FkWh = Value;
	LoadSpecType = 4;
	FAllocationFactor = FCFactor;
	ComputeAllocatedLoad();
}

void TLoadObj::Set_kWhDays(double Value)
{
	FkWhDays = Value;
	LoadSpecType = 4;
	ComputeAllocatedLoad();
}

void TLoadObj::set_nZIPV(int Value)
{
	SetZIPVSize(Value);
}

void TLoadObj::ComputeAllocatedLoad()
{

/*Fixed loads defined by kW, kvar or kW, pf are ignored*/
	switch(LoadSpecType)
	{
		case 	3:
		if(FConnectedkVA > 0.0)
		{
			kWBase = FConnectedkVA * FkVAAllocationFactor * Abs(PFNominal);
			kvarBase = kWBase * sqrt(1.0L / Sqr(PFNominal) - 1.0L);
			if(PFNominal < 0.0)
				kvarBase = -kvarBase;
		}
		break;
		case 	4:
		{
			FAvgkW = FkWh / (FkWhDays * 24);
			kWBase = FAvgkW * FCFactor;
			kvarBase = kWBase * sqrt(1.0L / Sqr(PFNominal) - 1.0L);
			if(PFNominal < 0.0)
				kvarBase = -kvarBase;
		}
		break;
		default:
		  ;
		break;
	}
}
/*
   Get the present terminal currents and store for harmonics base reference;
*/

void TLoadObj::InitHarmonics(int ActorID)
{

     /*Currents:pComplexArray;*/
	int i = 0;
     /*Make Sure there's enuff memory*/
	int stop = 0;
	HarmMag.resize(Fnphases + 1);
	HarmAng.resize(Fnphases + 1);

     // Currents := AllocMem(Sizeof(Currents^[1])*Yorder);   // to hold currents
	LoadFundamental = ActiveCircuit[ActorID]->Solution->get_FFrequency();

     // GetCurrents(Currents); // Use FPhaseCurr from most recent pflow solution
     /*Store the currents at fundamental frequency.
      The spectrum is applied to these.
     */
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		(HarmMag)[i] = cabs(FPhaseCurr[i - 1]);
		(HarmAng)[i] = cdang(FPhaseCurr[i - 1]);
	}

     // ReallocMem(Currents, 0);  // get rid of temp space
}

void TLoadObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"3");              //'phases';
	Set_PropertyValue(2,GetBus(1));         //'bus1';
	Set_PropertyValue(3,"12.47");
	Set_PropertyValue(4,"10");
	Set_PropertyValue(5,".88");
	Set_PropertyValue(6,"1");
	Set_PropertyValue(7,"");
	Set_PropertyValue(8,"");
	Set_PropertyValue(9,"");
	Set_PropertyValue(10,"");
	Set_PropertyValue(11,"wye");
	Set_PropertyValue(12,"5");
	Set_PropertyValue(13,"-1"); // 'rneut'; // IF entered -, assume open or user defined
	Set_PropertyValue(14,"0");  //'xneut';
	Set_PropertyValue(15,"variable"); //'status';  // fixed or variable
	Set_PropertyValue(16,"1"); //class
	Set_PropertyValue(17,"0.95");
	Set_PropertyValue(18,"1.05");
	Set_PropertyValue(19,"0.0");
	Set_PropertyValue(20,"0.0");
	Set_PropertyValue(21,"0.0");
	Set_PropertyValue(22,"0.5");  // Allocation Factor
	Set_PropertyValue(23,"11.3636");
	Set_PropertyValue(24,"50");
	Set_PropertyValue(25,"10");
	Set_PropertyValue(26,"1");  // CVR watt factor
	Set_PropertyValue(27,"2");  // CVR var factor
	Set_PropertyValue(28,"0");  // kwh bulling
	Set_PropertyValue(29,"30");  // kwhdays
	Set_PropertyValue(30,"4");  // Cfactor
	Set_PropertyValue(31,"");  // CVRCurve
	Set_PropertyValue(32,"1");  // NumCust
	Set_PropertyValue(33,"");  // ZIPV coefficient array
	Set_PropertyValue(34,"50");  // %SeriesRL
	Set_PropertyValue(35,"1");  // RelWeighting
	Set_PropertyValue(36,"0.5");  // VZpu
	Set_PropertyValue(37,"0.0");  // puXharm
	Set_PropertyValue(38,"6.0");  // XRHarm
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TLoadObj::MakePosSequence(int ActorID)
{
	String s;
	double V = 0.0;
	char Buff[1000];
	s = "Phases=1 conn=wye";

  // Make sure voltage is line-neutral
	if((Fnphases > 1) || (Connection != 0))
		V = kVLoadBase / SQRT3;
	else
		V = kVLoadBase;
	s = s + Format(" kV=%-.5g", V);

/* OLD Method
  // Divide the load by no. phases
  If Fnphases>1 Then
  Begin
      S := S + Format(' kW=%-.5g  kvar=%-.5g',[kWbase/Fnphases, kvarbase/Fnphases]);
      If FConnectedKVA>0.0 Then
         S := S + Format(' xfkVA=%-.5g  ',[FConnectedkVA/Fnphases]);
  End;
*/

// New Method: Assume load is distributed equally among the 3 phases -- works better
//1-5-2016 RCD
	s = s + Format(" kW=%-.5g  kvar=%-.5g", kWBase / 3.0, kvarBase / 3.0);
	if(FConnectedkVA > 0.0)
		s = s + Format(" xfkVA=%-.5g  ", FConnectedkVA / 3.0);
	Parser[ActorID]->SetCmdString(s);
	Edit(ActorID);
	TDSSCktElement::MakePosSequence(ActorID);
}

String TLoadObj::GetPropertyValue(int Index)
{
	String result = "";
	int i = 0;
	switch(Index)
	{
		case 	2:
		result = GetBus(1);
		break;
		case 	3:
		result = Format("%-.14f", kVLoadBase);
		break;
		case 	4:
		result = Format("%-.14f", kWBase);
		break;
		case 	5:
		result = Format("%-.4g", PFNominal);
		break;
		case 	7:
		result = YearlyShape;
		break;
		case 	8:
		result = DailyShape;
		break;
		case 	9:
		result = DutyShape;
		break;
		case 	12:
		result = Format("%-.14f", kvarBase);
		break;
		case 	22:
		result = Format("%-.14f", FkVAAllocationFactor);
		break;
		case 	23:
		result = Format("%-.14f", kVABase);
		break;
		case 	30:
		result = Format("%-.4g", FCFactor);
		break;
		case 	33:
		{
			int stop = 0;
			result = "";
			for(stop = get_FnZIPV(), i = 1; i <= stop; i++)
			{
				result = result + Format(" %-g", (ZIPV)[i]);
			}
		}
		break;
		case 	34:
		result = Format("%-.14f", puSeriesRL * 100.0);
		break;
		case 	35:
		result = Format("%-.14f", RelWeighting);
		break;
		case 	36:
		result = Format("%-.14f", VLowpu);
		break;
		case 	37:
		result = Format("%-.14f", FpuXHarm);
		break;
		case 	38:
		result = Format("%-.14f", FXRHarmRatio);
		break;
		default:
			result = TDSSCktElement::GetPropertyValue(Index);
		break;
	}
	return result;
}


void Load_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
}

		class 		Load_unit
		{
		public:
		Load_unit()
		{
			//AssertSystemInitialization();
			Load_initialization();
		}
		};
		Load_unit _Load_unit;

}  // namespace Load




