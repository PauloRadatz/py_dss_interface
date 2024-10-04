

#pragma hdrstop

#include "WindGen.h"

#include "Circuit.h"
#include "DSSGlobals.h"
#include "Utilities.h"
#include "d2c_structures.h"

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
using namespace LoadShape;
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace WindGenUserModel;
using namespace WindGenVars;
using namespace mathutil;
using namespace Utilities;

namespace WindGen
{

TWindGenObj::TWindGenObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TWindGenObj::TWindGenObj(String ClassName) : inherited(ClassName) {}
TWindGenObj::TWindGenObj() {}


TWindGenObj* ActiveWindGenObj = nullptr;

const int NumPropsThisClass = 44;  // removed Fuel variables
  // Dispatch modes
const int Default = 0;
const int LOADMODE = 1;
complex cBuffer[25];  // Temp buffer for calcs  24-phase WindGen?
complex CDoubleOne = cmplx(1,1);
//    TwoPI3:Double;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TWindGen::TWindGen()
{
	;
	Class_Name ="WindGen";
	DSSClassType = DSSClassType + WINDGEN_ELEMENT;  // In both PCelement and Genelement list
	ActiveElement = 0;

     // Set Register names
	RegisterNames[1 - 1] ="kWh";
	RegisterNames[2 - 1] ="kvarh";
	RegisterNames[3 - 1] ="Max kW";
	RegisterNames[4 - 1] ="Max kVA";
	RegisterNames[5 - 1] ="Hours";
	RegisterNames[6 - 1] ="$";
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
    WindGenClass[ActiveActor] = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TWindGen::~TWindGen()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TWindGen::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();   /*see DSSClass*/

     // Define Property names
	PropertyName[1 - 1] ="phases";
	PropertyHelp[1 - 1] ="Number of Phases, this WindGen.  Power is evenly divided among phases.";

	PropertyName[2 - 1] ="bus1";
	PropertyHelp[2 - 1] ="Bus to which the WindGen is connected.  May include specific node specification.";

	PropertyName[3 - 1] ="kv";
	PropertyHelp[3 - 1] ="Nominal rated (1.0 per unit) voltage, kV, for WindGen. For 2- and 3-phase WindGens, specify phase-phase kV. "
	          "Otherwise, for phases=1 or phases>3, specify actual kV across each branch of the WindGen. "
	          "If wye (star), specify phase-neutral kV. "
	          "If delta or phase-phase connected, specify phase-phase kV.";

	PropertyName[4 - 1] ="kW";
	PropertyHelp[4 - 1] = String("Total base kW for the WindGen.  A positive value denotes power coming OUT of the element, ") + CRLF
	           +"which is the opposite of a load. This value is modified depending on the dispatch mode. "
	           +"Unaffected by the global load multiplier and growth curves. "
	           +"If you want there to be more generation, you must add more WindGens or change this value.";

	PropertyName[5 - 1] ="PF";
	PropertyHelp[5 - 1] = String("WindGen power factor. Default is 0.80. Enter negative for leading powerfactor ""(when kW and kvar have opposite signs.)") + CRLF
	           +"A positive power factor for a WindGen signifies that the WindGen produces vars "
	           + CRLF
	           +"as is typical for a synchronous WindGen.  Induction machines would be "
	           + CRLF
	           +"generally specified with a negative power factor.";

	PropertyName[6 - 1] ="model";
	PropertyHelp[6 - 1] = String("Integer code for the model to use for generation variation with voltage. ""Valid values are:") + CRLF
	           + CRLF
	           +"1:WindGen injects a constant kW at specified power factor."
	           + CRLF
	           +"2:WindGen is modeled as a constant admittance."
	           + CRLF
	           +"3:Const kW, constant kV.  Voltage-regulated model."
	           + CRLF
	           +"4:Const kW, Fixed Q (Q never varies)"
	           + CRLF
	           +"5:Const kW, Fixed Q(as a constant reactance)"
	           + CRLF
	           +"6:Compute load injection from User-written Model.(see usage of Xd, Xdp)";

	PropertyName[7 - 1] ="yearly";
	PropertyHelp[7 - 1] ="Dispatch shape to use for yearly-mode simulations.  Must be previously defined "
	          "as a Loadshape object. If this is not specified, a constant value is assumed (no variation). "
	          "Set to NONE to reset to no loadahape. "
	          "Nominally for 8760 simulations.  If there are fewer points in the designated shape than "
	          "the number of points in the solution, the curve is repeated.";

	PropertyName[8 - 1] ="daily";
	PropertyHelp[8 - 1] ="Dispatch shape to use for daily-mode simulations.  Must be previously defined "
	          "as a Loadshape object of 24 hrs, typically."
	          "Set to NONE to reset to no loadahape. "; // daily dispatch (hourly)

	PropertyName[9 - 1] ="duty";
	PropertyHelp[9 - 1] ="Load shape to use for duty cycle dispatch simulations such as for wind or solar generation. "
	          "Must be previously defined as a Loadshape object. "
	          "Typically would have time intervals less than 1 hr -- perhaps, in seconds. "
	          "Set to NONE to reset to no loadahape. "
	          "Designate the number of points to solve using the Set Number=xxxx command. "
	          "If there are fewer points in the actual shape, the shape is assumed to repeat.";  // as for wind generation

	PropertyName[10 - 1] ="conn";
	PropertyHelp[10 - 1] ="={wye|LN|delta|LL}.  Default is wye.";

	PropertyName[11 - 1] ="kvar";
	PropertyHelp[11 - 1] ="Specify the base kvar.  Alternative to specifying the power factor.  Side effect: "
	          " the power factor value is altered to agree based on present value of kW.";

	PropertyName[12 - 1] ="class";
	PropertyHelp[12 - 1] ="An arbitrary integer number representing the class of WindGen so that WindGen values may "
	          "be segregated by class."; // integer

	PropertyName[13 - 1] ="debugtrace";
	PropertyHelp[13 - 1] ="{Yes | No }  Default is no.  Turn this on to capture the progress of the WindGen model "
                          "for each iteration. Creates a separate file for each WindGen named \"GEN_name.CSV\""; // per unit set point voltage for power flow model

	PropertyName[14 - 1] ="Vminpu";
	PropertyHelp[14 - 1] ="Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. "
	          "Below this value, the Windgen model reverts to a constant impedance model. For model 7, the current is "
	          "limited to the value computed for constant power at Vminpu.";

	PropertyName[15 - 1] ="Vmaxpu";
	PropertyHelp[15 - 1] ="Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. "
	          "Above this value, the Windgen model reverts to a constant impedance model.";

	PropertyName[16 - 1] ="kVA";
	PropertyHelp[16 - 1] ="kVA rating of electrical machine. Defaults to 1.2* kW if not specified. Applied to machine or inverter definition for Dynamics mode solutions. ";

	PropertyName[17 - 1] ="MVA";
	PropertyHelp[17 - 1] ="MVA rating of electrical machine.  Alternative to using kVA=.";

	PropertyName[18 - 1] ="UserModel";
	PropertyHelp[18 - 1] ="Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, "
	          "overriding the default model.  Set to \"none\" to negate previous setting.";

	PropertyName[19 - 1] ="UserData";
	PropertyHelp[19 - 1] ="String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.";

	PropertyName[20 - 1] ="DutyStart";
	PropertyHelp[20 - 1] ="Starting time offset [hours] into the duty cycle shape for this WindGen, defaults to 0";

	PropertyName[21 - 1] ="DynamicEq";
	PropertyHelp[21 - 1] ="The name of the dynamic equation (DinamicExp) that will be used for defining the dynamic behavior of the generator. "
                          "if not defined, the generator dynamics will follow the built-in dynamic equation.";

	PropertyName[22 - 1] ="DynOut";
	PropertyHelp[22 - 1] ="The name of the variables within the Dynamic equation that will be used to govern the generator dynamics."
						  "This generator model requires 2 outputs from the dynamic equation: "
						  + CRLF + CRLF + 
						  "1. Shaft speed (velocity) relative to synchronous speed." + CRLF + 
					      "2. Shaft, or power, angle (relative to synchronous reference frame)." + CRLF + CRLF + 
						  "The output variables need to be defined in the same order.";
        
	PropertyName[23 - 1] = "Rthev";
    PropertyHelp[23 - 1] = "per unit Thevenin equivalent R.";

	PropertyName[24 - 1] = "Xthev";
    PropertyHelp[24 - 1] = "per unit Thevenin equivalent X.";

	PropertyName[25 - 1] = "Vss";
    PropertyHelp[25 - 1] = "Steady state voltage magnitude.";

	PropertyName[26 - 1] = "Pss";
    PropertyHelp[26 - 1] = "Steady state output real power.";

	PropertyName[27 - 1] = "Qss";
    PropertyHelp[27 - 1] = "Steady state output reactive power.";

    PropertyName[28 - 1] = "vwind";
    PropertyHelp[28 - 1] = "Wind speed in m/s";

    PropertyName[29 - 1] = "QMode";
    PropertyHelp[29 - 1] = "Q control mode (0:Q, 1:PF, 2:VV).";

    PropertyName[30 - 1] = "SimMechFlg";
    PropertyHelp[30 - 1] = "1 to simulate mechanical system. Otherwise (0) only uses the electrical system. For dynamics simulation purposes.";

    PropertyName[31 - 1] = "APCFlg";
    PropertyHelp[31 - 1] = "1 to enable active power control.";

    PropertyName[32 - 1] = "QFlg";
    PropertyHelp[32 - 1] = "1 to enable reactive power and voltage control.";

    PropertyName[33 - 1] = "delt0";
    PropertyHelp[33 - 1] = "User defined internal simulation step.";

    PropertyName[34 - 1] = "N_WTG";
    PropertyHelp[34 - 1] = "Number of WTG in aggregation.";

    PropertyName[35 - 1] = "VV_Curve";
    PropertyHelp[35 - 1] = "Name of the XY curve defining the control curve for implementing Vol-var control with this inverter.";

    PropertyName[36 - 1] = "Ag";
    PropertyHelp[36 - 1] = "Gearbox ratio (Default 1/90).";

    PropertyName[37 - 1] = "Cp";
    PropertyHelp[37 - 1] = "Turbine performance coefficient (deafult 0.41).";

    PropertyName[38 - 1] = "Lamda";
    PropertyHelp[38 - 1] = "Tip speed ratio (Default 7.95).";

    PropertyName[39 - 1] = "P";
    PropertyHelp[39 - 1] = "Number of pole pairs of the induction generator (Default 2).";

    PropertyName[40 - 1] = "pd";
    PropertyHelp[40 - 1] = "Air density in kg/m3 (Default 1.225).";

    PropertyName[41 - 1] = "PLoss";
    PropertyHelp[41 - 1] = "Name of the XYCurve object describing the active power losses in pct versus the wind speed.";

    PropertyName[42 - 1] = "Rad";
    PropertyHelp[42 - 1] = "Rotor radius in meters (Default 40).";

    PropertyName[43 - 1] = "VCutIn";
    PropertyHelp[43 - 1] = "Cut-in speed for the wind generator (m/s - default 5).";

    PropertyName[44 - 1] = "VCutOut";
    PropertyHelp[44 - 1] = "Cut-out speed for the wind generator (m/s - default 23).";

      /*Removed Fuel-related variables 40-44 from Generator model*/
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // Override default help string
	PropertyHelp[NumPropsThisClass + 1 - 1] ="Name of harmonic voltage or current spectrum for this WindGen. "
	          "Voltage behind Xd\" for machine - default. Current injection for inverter. "
	          "Default value is \"default\", which is defined when the DSS starts.";
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TWindGen::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new WindGen and add it to WindGen class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TWindGenObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TWindGen::SetNcondsForConnection()
{
	/*# with ActiveWindGenObj do */
	{
		auto with0 = ActiveWindGenObj;
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
			break;
			default:
			  ;
			break;
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN

void TWindGen::InterpretConnection(const String s)
{
	String TestS;
	/*# with ActiveWindGenObj do */
	{
		auto with0 = ActiveWindGenObj;
		TestS = LowerCase(s);
		switch(TestS[1])
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

            /*VBase is always L-N voltage unless 1-phase device or more than 3 phases*/
		/*# with WindGenVars do */
		{
			auto& with1 = with0->WindGenVars;
			switch(with0->Fnphases)
			{ /*CASE Connection OF
              1: VBase := kVWindGenBase * 1000.0 ;
              Else*/
				case 	2: case 3:
				with0->VBase = with1.kVWindGenBase * InvSQRT3x1000;
				break;    // L-N Volts
				default:
				with0->VBase = with1.kVWindGenBase * 1000.0;   // Just use what is supplied
				break;
			}
		}
            /*End;*/
		with0->VBase95 = with0->Vminpu * with0->VBase;
		with0->VBase105 = with0->Vmaxpu * with0->VBase;
		with0->Yorder = with0->Fnconds * with0->Fnterms;
		with0->Set_YprimInvalid(ActiveActor,true);
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int InterpretDispMode(const String s)
{
	int result = 0;
	switch(LowerCase(s)[0])
	{
		case 	L'l':
		result = LOADMODE;
		break;
		default:
		result = Default;
		break;
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TWindGen::Edit(int ActorID)
{
	int result = 0;
	int i = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
  // continue parsing with contents of Parser
	ActiveWindGenObj = (TWindGenObj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveWindGenObj);
	result = 0;
	/*# with ActiveWindGenObj do */
	{
		auto with0 = ActiveWindGenObj;
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
				with0->Set_PropertyValue((PropertyIdxMap)[ParamPointer - 1],Param);
			else
				DoSimpleMsg("Unknown parameter \"" + ParamName + "\" for WindGen \"" + with0->get_Name() + "\"", 560);
			if(ParamPointer > 0)
				switch((PropertyIdxMap)[ParamPointer - 1])
				{
					case 	0:
						DoSimpleMsg("Unknown parameter \"" + ParamName + "\" for Object \"" + Class_Name + "." + with0->get_Name() + "\"", 561);
					break;
					case 	1:
						with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
					break; // num phases
					case 	2:
						with0->SetBus(1, Param);
					break;
					case 	3:
						with0->Set_PresentkV(Parser[ActorID]->MakeDouble_());
					break;
					case 	4:
						with0->kWBase = Parser[ActorID]->MakeDouble_();
					break;
					case 	5:
						with0->PFNominal = Parser[ActorID]->MakeDouble_();
					break;
					case 	6:
						with0->GenModel = Parser[ActorID]->MakeInteger_();
					break;
					case 	7:
						with0->YearlyShape = Param;
					break;
					case 	8:
						with0->DailyDispShape = Param;
					break;
					case 	9:
						with0->DutyShape = Param;
					break;
					case 	10:
						InterpretConnection(Param);
					break;
					case 	11:
						with0->Set_Presentkvar(Parser[ActorID]->MakeDouble_());
					break;
                    case	12:
                        with0->GenClass = Parser[ActorID]->MakeInteger_();
                    break;
					case 	14:
						with0->Vminpu = Parser[ActorID]->MakeDouble_();
					break;
					case 	15:
						with0->Vmaxpu = Parser[ActorID]->MakeDouble_();
					break;
                    case	16:
                    {
                        with0->WindModelDyn->EditProp(13, Param);
                        with0->WindGenVars.kVArating = Parser[ActorID]->MakeDouble_();
                    }
					break;
                    case	17:
                    {
                        with0->WindGenVars.kVArating = Parser[ActorID]->MakeDouble_() * 1e3;  // 'MVA'
                        with0->WindModelDyn->EditProp(13, to_string(with0->WindGenVars.kVArating));
                    }
					break;
					case 	18:
						with0->UserModel->Set_Name(Parser[ActorID]->MakeString_());
					break;  // Connect to user written models
					case 	19:
						with0->UserModel->Set_Edit(Parser[ActorID]->MakeString_());
					break;  // Send edit string to user model
					case 	20:
						with0->DutyStart = Parser[ActorID]->MakeDouble_();
					break;
					case 	21:
						with0->DynamicEq = Param;
					break;
                    case 22:
                        with0->SetDynOutput(Param);
                    break;
                    case 23:
                        with0->WindModelDyn->EditProp(1,Param);
                    break;
                    case 24:
                        with0->WindModelDyn->EditProp(2, Param);
                    break;
                    case 25:
                        with0->WindModelDyn->EditProp(3, Param);
                    break;
                    case 26:
                        with0->WindModelDyn->EditProp(4, Param);
                    break;
                    case 27:
                        with0->WindModelDyn->EditProp(5, Param);
                    break;
                    case 28:
                        with0->WindModelDyn->EditProp(6, Param);
                    break;
                    case 29:
                        with0->WindModelDyn->EditProp(7, Param);
                    break;
                    case 30:
                        with0->WindModelDyn->EditProp(8, Param);
                    break;
                    case 31:
                        with0->WindModelDyn->EditProp(9, Param);
                    break;
                    case 32:
                        with0->WindModelDyn->EditProp(10, Param);
                    break;
                    case 33:
                        with0->WindModelDyn->EditProp(12, Param);
                    break;
                    case 34:
                        with0->WindModelDyn->EditProp(22, Param);
                    break;
                    case 35:
                        with0->VV_Curve = Param;
                    break;
                    case 36:
                        with0->WindGenVars.ag = Parser[ActorID]->MakeDouble_();
                    break;
                    case 37:
                        with0->WindGenVars.Cp = Parser[ActorID]->MakeDouble_();
                    break;
                    case 38:
                        with0->WindGenVars.Lamda = Parser[ActorID]->MakeDouble_();
                    break;
                    case 39:
                        with0->WindGenVars.Poles = Parser[ActorID]->MakeDouble_();
                    break;
                    case 40:
                        with0->WindGenVars.pd = Parser[ActorID]->MakeDouble_();
                    break;
                    case 41:
                        with0->WindGenVars.PLoss = Parser[ActorID]->MakeString_();
                    break;
                    case 42:
                        with0->WindGenVars.Rad = Parser[ActorID]->MakeDouble_();
                    break;
                    case 43:
                        with0->WindGenVars.VCutin = Parser[ActorID]->MakeDouble_();
                    break;
                    case 44:
                        with0->WindGenVars.VCutout = Parser[ActorID]->MakeDouble_();
                    break;
           // Inherited parameters
					default:
						inherited::ClassEdit(ActiveWindGenObj, ParamPointer - NumPropsThisClass);
					break;
				}
			if(ParamPointer > 0)
				switch((PropertyIdxMap)[ParamPointer - 1])
				{
					case 	1:
					SetNcondsForConnection();
					break;  // Force Reallocation of terminal info

            // keep kvar nominal up to date with kW and PF
					case 	4: case 5:
					with0->SyncUpPowerQuantities();
					break;

            // if a model 3 WindGen added, force calc of dQdV
					case 	6:
					if(with0->GenModel == 3)
						ActiveCircuit[ActorID]->Solution->SolutionInitialized = false;
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
									with0->SetkWkvar(with1->MaxP, with1->MaxQ);
							}
					}
					break;
					case 	8:
					{
						with0->DailyDispShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DailyDispShape));
						if(ASSIGNED(with0->DailyDispShapeObj))
							/*# with DailyDispShapeObj do */
							{
								auto with2 = with0->DailyDispShapeObj;
								if(with2->UseActual)
									with0->SetkWkvar(with2->MaxP, with2->MaxQ);
							}
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
					case 	13:
					if(with0->DebugTrace)
					{
						int stop = 0;
						AssignFile(with0->Tracefile, GetOutputDirectory() +"WINDGEN_" + with0->get_Name() +".CSV");
						Rewrite(with0->Tracefile);
						IOResultToException();
						Write(with0->Tracefile,"t, Iteration, LoadMultiplier, Mode, LoadModel, GenModel, dQdV, Avg_Vpu, Vdiff, MQnominalperphase, MPnominalperphase, CurrentType");
						for(stop = with0->Get_NPhases(), i = 1; i <= stop; i++)
						{
							Write(with0->Tracefile, String(", |Iinj") + IntToStr(i) +"|");
						}
						for(stop = with0->Get_NPhases(), i = 1; i <= stop; i++)
						{
							Write(with0->Tracefile, String(", |Iterm") + IntToStr(i) +"|");
						}
						for(stop = with0->Get_NPhases(), i = 1; i <= stop; i++)
						{
							Write(with0->Tracefile, String(", |Vterm") + IntToStr(i) +"|");
						}
						Write(with0->Tracefile,",Vthev, Theta");
						WriteLn(with0->Tracefile);
						CloseFile(with0->Tracefile);
					}
					break;
					case 	16: case 17:
					with0->kVANotSet = false;
					break;
					case	21:
					{
                        with0->DynamicEqObj = (TDynamicExpObj*) TDynamicExpClass[ActorID]->Find(with0->DynamicEq);
                        if (ASSIGNED(with0->DynamicEqObj))
                        {
                            with0->DynamicEqVals.resize(with0->DynamicEqObj->get_FNumVars());
                            for (int idx = 0; idx < with0->DynamicEqVals.size(); idx++)
                                with0->DynamicEqVals[idx].resize(2);
                        }
					}
                    break;
                    case 35:		// get the Volt-var control curve
                    {
                        with0->VV_CurveObj = (TXYcurveObj*) XYCurveClass[ActorID]->Find(with0->VV_Curve);
                        if (ASSIGNED(with0->VV_CurveObj))
                        {
                            auto with1 = with0->VV_CurveObj;
                            for (int idx = 1; idx <= with1->FNumPoints; idx++)
                            {
                                with0->WindModelDyn->EditProp(13 + idx, to_string(with1->Get_XValue(idx)));
                                with0->WindModelDyn->EditProp(17 + idx, to_string(with1->Get_YValue(idx)));
                            }
                        }
                        else
                            DoSimpleMsg("Volt-var control curve \"" + with0->VV_Curve + "\" not found, make sure that it was not defined before this element", 565);
                    }
                    break;
                    case 41: // get the losses curve
                    {
                        with0->Loss_CurveObj = (TXYcurveObj*)XYCurveClass[ActorID]->Find(with0->WindGenVars.PLoss);
                        if (!(ASSIGNED(with0->Loss_CurveObj)))
							DoSimpleMsg("Losses curve \"" + with0->VV_Curve + "\" not found, make sure that it was not defined before this element", 566);
                    }
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

int TWindGen::MakeLike(const String OtherWindGenName)
{
	int result = 0;
	TWindGenObj* OtherWindGen = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line name in the present collection*/
	OtherWindGen = ((TWindGenObj*) Find(OtherWindGenName));
	if(OtherWindGen != nullptr)
		/*# with ActiveWindGenObj do */
		{
			auto with0 = ActiveWindGenObj;
			int stop = 0;
			if(with0->Fnphases != OtherWindGen->Fnphases)
			{
				with0->Set_NPhases(OtherWindGen->Fnphases);
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->WindGenVars.kVWindGenBase = OtherWindGen->WindGenVars.kVWindGenBase;
			with0->VBase = OtherWindGen->VBase;
			with0->Vminpu = OtherWindGen->Vminpu;
			with0->Vmaxpu = OtherWindGen->Vmaxpu;
			with0->VBase95 = OtherWindGen->VBase95;
			with0->VBase105 = OtherWindGen->VBase105;
			with0->kWBase = OtherWindGen->kWBase;
			with0->kvarBase = OtherWindGen->kvarBase;
			with0->WindGenVars.Pnominalperphase = OtherWindGen->WindGenVars.Pnominalperphase;
			with0->PFNominal = OtherWindGen->PFNominal;
			with0->WindGenVars.Qnominalperphase = OtherWindGen->WindGenVars.Qnominalperphase;
			with0->varMin = OtherWindGen->varMin;
			with0->varMax = OtherWindGen->varMax;
			with0->Connection = OtherWindGen->Connection;
     //  Rneut          := OtherWindGen.Rneut;
      // Xneut          := OtherWindGen.Xneut;
			with0->YearlyShape = OtherWindGen->YearlyShape;
			with0->YearlyShapeObj = OtherWindGen->YearlyShapeObj;
			with0->DailyDispShape = OtherWindGen->DailyDispShape;
			with0->DailyDispShapeObj = OtherWindGen->DailyDispShapeObj;
			with0->DutyShape = OtherWindGen->DutyShape;
			with0->DutyShapeObj = OtherWindGen->DutyShapeObj;
			with0->DutyStart = OtherWindGen->DutyStart;
			with0->GenClass = OtherWindGen->GenClass;
			with0->GenModel = OtherWindGen->GenModel;
			with0->IsFixed = OtherWindGen->IsFixed;
			with0->WindGenVars.VTarget = OtherWindGen->WindGenVars.VTarget;
			with0->Vpu = OtherWindGen->Vpu;
			with0->kvarMax = OtherWindGen->kvarMax;
			with0->kvarMin = OtherWindGen->kvarMin;
			with0->FForcedON = OtherWindGen->FForcedON;
			with0->kVANotSet = OtherWindGen->kVANotSet;
			with0->WindGenVars.kVArating = OtherWindGen->WindGenVars.kVArating;
			with0->WindGenVars.puXd = OtherWindGen->WindGenVars.puXd;
			with0->WindGenVars.puXdp = OtherWindGen->WindGenVars.puXdp;
			with0->WindGenVars.puXdpp = OtherWindGen->WindGenVars.puXdpp;
			with0->WindGenVars.Hmass = OtherWindGen->WindGenVars.Hmass;
			with0->WindGenVars.Theta = OtherWindGen->WindGenVars.Theta;
			with0->WindGenVars.Speed = OtherWindGen->WindGenVars.Speed;
			with0->WindGenVars.w0 = OtherWindGen->WindGenVars.w0;
			with0->WindGenVars.dSpeed = OtherWindGen->WindGenVars.dSpeed;
			with0->WindGenVars.D = OtherWindGen->WindGenVars.D;
			with0->WindGenVars.Dpu = OtherWindGen->WindGenVars.Dpu;
			with0->WindGenVars.XRdp = OtherWindGen->WindGenVars.XRdp;
			with0->UserModel->FName = OtherWindGen->UserModel->FName;  // Connect to user written models
			with0->ShaftModel->FName = OtherWindGen->ShaftModel->FName;
			ClassMakeLike(OtherWindGen);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				(with0->FPropertyValue)[i - 1] = (OtherWindGen->FPropertyValue)[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Load MakeLike: \"") + OtherWindGenName
	           +"\" Not Found.", 562);
	return result;
}

//----------------------------------------------------------------------------

int TWindGen::Init(int Handle, int ActorID)
{
	int result = 0;
	TWindGenObj* P = nullptr;
	if(Handle == 0)  // init all
	{
		P = (TWindGenObj*) ElementList.Get_First();
		while((P != nullptr))
		{
			P->Randomize(0);
			P = (TWindGenObj*) ElementList.Get_Next();
		}
	}
	else
	{
		Set_Active(Handle);
		P = ((TWindGenObj*) GetActiveObj());
		P->Randomize(0);
	}
	DoSimpleMsg("Need to implement TWindGen.Init", -1);
	result = 0;
	return result;
}

/*--------------------------------------------------------------------------*/  // Force all EnergyMeters in the circuit to reset

void TWindGen::ResetRegistersAll(int ActorID)
{
	TWindGenObj* pGen = nullptr;
	pGen = ((TWindGenObj*) ActiveCircuit[ActorID]->WindGens.Get_First());
	while((pGen != nullptr))
	{
		pGen->ResetRegisters();
		pGen = ((TWindGenObj*) ActiveCircuit[ActorID]->WindGens.Get_Next());
	}
}

/*--------------------------------------------------------------------------*/  // Force all EnergyMeters in the circuit to take a sample

void TWindGen::SampleAll(int ActorID)
{
	TWindGenObj* pGen = nullptr;
	pGen = ((TWindGenObj*) ActiveCircuit[ActorID]->WindGens.Get_First());
	while(pGen != nullptr)
	{
		if(pGen->Get_Enabled())
			pGen->TakeSample(ActorID);
		pGen = ((TWindGenObj*) ActiveCircuit[ActorID]->WindGens.Get_Next());
	}
}

//----------------------------------------------------------------------------

TWindGenObj::TWindGenObj(TDSSClass* ParClass, const String SourceName)
 : inherited(ParClass),
			Model7MaxPhaseCurr(0.0),
			Model7LastAngle(0.0),
			DebugTrace(false),
			DeltaQMax(0.0),
			DQDV(0.0),
			DQDVSaved(0.0),
			FForcedON(false),
			FirstSampleAfterReset(false),
			IsFixed(false),
			WindGenSolutionCount(0),
			GenFundamental(0.0),
			GenON(false),
			GenSwitchOpen(false),
			kVANotSet(false),
			LastGrowthFactor(0.0),
			LastYear(0),
			OpenWindGenSolutionCount(0),
			PVFactor(0.0),
			RandomMult(0.0),
			Reg_Hours(0),
			Reg_kvarh(0),
			Reg_kWh(0),
			Reg_MaxkVA(0),
			Reg_MaxkW(0),
			Reg_Price(0),
			UserModel(nullptr),
			ShaftModel(nullptr),
			V_Avg(0.0),
			V_Remembered(0.0),
			var_Remembered(0.0),
			varBase(0.0),
			varMax(0.0),
			varMin(0.0),
			VBase(0.0),
			VBase105(0.0),
			VBase95(0.0),
			YPrimOpenCond(nullptr),
			YQFixed(0.0),
			ShapeIsActual(false),
			ForceBalanced(false),
			Connection(0),
			DailyDispShapeObj(nullptr),
			DutyShapeObj(nullptr),
			DutyStart(0.0),
			GenClass(0),
			GenModel(0),
			kvarBase(0.0),
			kvarMax(0.0),
			kvarMin(0.0),
			kWBase(0.0),
			PFNominal(0.0),
			Vpu(0.0),
			Vmaxpu(0.0),
			Vminpu(0.0),
			GenActive(false),
			YearlyShapeObj(nullptr)
{
	int i;

	Set_Name(LowerCase(SourceName));
	DSSObjType = ParClass->DSSClassType; // + WINDGEN_ELEMENT;  // In both PCelement and Genelement list
	Set_NPhases(3);
	Fnconds = 4;  // defaults to wye
	Yorder = 0;  // To trigger an initial allocation
	Set_NTerms(1);  // forces allocations
	kWBase = 1000.0;
	kvarBase = 60.0;
	kvarMax = kvarBase * 2.0;
	kvarMin = -kvarMax;
	PFNominal = 0.88;
	YearlyShape ="";
	YearlyShapeObj = nullptr;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
	DailyDispShape ="";
	DailyDispShapeObj = nullptr;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
	DutyShape ="";
	DutyShapeObj = nullptr;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
	DutyStart = 0.0;
	Connection = 0;    // Wye (star)
	GenModel = 1;  /*Typical fixed kW negative load*/
	GenClass = 1;
	LastYear = 0;
	LastGrowthFactor = 1.0;
	DQDVSaved = 0.0;  // Initialize this here.  Allows WindGens to be turned off and on
	WindGenSolutionCount = -1;  // For keep track of the present solution in Injcurrent calcs
	OpenWindGenSolutionCount = -1;
	YPrimOpenCond = nullptr;
	WindGenVars.kVWindGenBase = 12.47;
	Vpu = 1.0;
	WindGenVars.VTarget = 1000.0 * Vpu * WindGenVars.kVWindGenBase / SQRT3;  /*Line-to-Neutral target*/
	VBase = 7200.0;
	Vminpu = 0.90;
	Vmaxpu = 1.10;
	VBase95 = Vminpu * VBase;
	VBase105 = Vmaxpu * VBase;
	Yorder = Fnterms * Fnconds;
	RandomMult = 1.0;
	IsFixed = false;

     /*Machine rating stuff*/
	WindGenVars.kVArating = kWBase * 1.2;
	kVANotSet = true;  // Flag for default value for kVA
	/*# with WindGenVars do */
	{
		auto& with0 = WindGenVars;
		with0.puXd = 1.0;
		with0.puXdp = 0.28;
		with0.puXdpp = 0.20;
		with0.XD = with0.puXd * Sqr(with0.kVWindGenBase) * 1000.0L / with0.kVArating;
		with0.Xdp = with0.puXdp * Sqr(with0.kVWindGenBase) * 1000.0L / with0.kVArating;
		with0.Xdpp = with0.puXdpp * Sqr(with0.kVWindGenBase) * 1000.0L / with0.kVArating;
		with0.Hmass = 1.0;       //  W-sec/VA rating
		with0.Theta = 0.0;
		with0.w0 = TwoPi * BaseFrequency;
		with0.Speed = 0.0;
		with0.dSpeed = 0.0;
		with0.D = 1.0;
		with0.XRdp = 20.0;

        // Added for the wind generator specifically
        with0.PLoss = "";
        with0.ag	= 0.0111111111111111111;
        with0.Cp	= 0.41;
        with0.Lamda = 7.95;
        with0.Poles = 2;
        with0.pd    = 1.225;
        with0.Rad   = 40;
        with0.VCutin = 5;
        with0.VCutout = 23;
        with0.Pm    = 0;
        with0.Ps    = 0;
        with0.Pr    = 0;
        with0.Pg    = 0;
        with0.s     = 0;
	}

     /*Advertise Genvars struct as public*/
	PublicDataStruct = ((void*) &WindGenVars);
	PublicDataSize = sizeof(TWindGenVars);
	UserModel = new TWindGenUserModel(&WindGenVars);
	ShaftModel = new TWindGenUserModel(&WindGenVars);

  // Register values inherited from Generator model
	Reg_kWh = 1 - 1;
	Reg_kvarh = 2 - 1;
	Reg_MaxkW = 3 - 1;
	Reg_MaxkVA = 4 - 1;
	Reg_Hours = 5  - 1;
	Reg_Price = 6 - 1;
	PVFactor = 0.1;
	DebugTrace = false;
	FForcedON = false;
	GenSwitchOpen = false;
	ShapeIsActual = false;
	ForceBalanced = false;

	Spectrum ="defaultgen";  // override base class

	GenActive = true;   // variable to use if needed

	     // Creates the Dynamic model for the Wind Turbine
	WindModelDyn = new TGE_WTG3_Model(WindGenVars, ActiveCircuit[ActiveActor]->Solution->DynaVars);
    WindModelDyn->EditProp(6, "12");
    WindModelDyn->QMode = 0;

	Loss_CurveObj = nullptr;
    VV_CurveObj = nullptr;

	InitPropertyValues(0);
	RecalcElementData(ActiveActor);

	for (i = 0; i < NumWGenRegisters; i++)
	{
		Registers[i] = 0.0;
		Derivatives[i] = 0.0;
	}
}


//----------------------------------------------------------------------------

TWindGenObj::~TWindGenObj()
{
	delete YPrimOpenCond;
	delete UserModel;
	delete ShaftModel;
	// inherited::Destroy();
}


//----------------------------------------------------------------------------

void TWindGenObj::Randomize(int Opt)
{
	switch(Opt)
	{
		case 	0:
		RandomMult = 1.0;
		break;
		case 	GAUSSIAN:
		RandomMult = Gauss(YearlyShapeObj->Get_Mean(), YearlyShapeObj->Get_StdDev());
		break;
		case 	UNIFORM:
		RandomMult = (double) Random();
		break;  // number between 0 and 1.0
		case 	LOGNORMAL:
		RandomMult = QuasiLogNormal(YearlyShapeObj->Get_Mean());
		break;
		default:
		  ;
		break;
	}
}

//----------------------------------------------------------------------------
/*Evaluates if the value provided corresponds to a constant value or to an operand
 for calculating the value using the simulation results*/

int TWindGenObj::CheckIfDynVar(String myVar, int ActorID)
{
    int result = 0;
    int myOp = 0; // Operator found

    String myValue; // Value entered by the user
    result = -1;
    if ((DynamicEqObj != NULL))
    {
        result = DynamicEqObj->Get_Var_Idx(myVar);
        if ((result >= 0) && (result < 50000))
        {
            myValue = Parser[ActorID]->MakeString_();
            if (DynamicEqObj->Check_If_CalcValue(myValue, myOp))
            {
                // Adss the pair (var index + operand index)
                DynamicEqPair.push_back(result);
                DynamicEqPair.push_back(myOp);
            }
            else // Otherwise, move the value to the values array
                DynamicEqVals[result][0] = Parser[ActorID]->MakeDouble_();
        }
        else
            result = -1; // in case is a constant
    }
    return result;
}

//----------------------------------------------------------------------------
/*Obtains the indexes of the given variables to use them as reference for setting
the dynamic output for the generator*/

void TWindGenObj::SetDynOutput(String myVar)
{
    int VarIdx = 0, 
		idx = 0;
    TStringList myStrArray;

    if (DynamicEqObj != NULL) // Making sure we have a dynamic eq linked
    {
        // First, set the length for the index array, 2 variables in this case
        DynOut.resize(2);
        InterpretTStringListArray(myVar, myStrArray);
        // ensuring they are lower case
        for (int stop = 1, idx = 0; idx <= stop; idx++)
        {
            myStrArray[idx] = LowerCase(myStrArray[idx]);
            VarIdx = DynamicEqObj->Get_Out_Idx(myStrArray[idx]);
            if (VarIdx < 0)
                // Being here means that the given name doesn't exist or is a constant
                DoSimpleMsg("DynamicExp variable \"" + myStrArray[idx] + "\" not found or not defined as an output.", 50008);
            else
                DynOut[idx] = VarIdx;
        }
        myStrArray.clear();
    }
    else
        DoSimpleMsg(String("A DynamicExp object needs to be assigned to this element before this declaration: DynOut = [") + myVar + "]", 50007);
}

//----------------------------------------------------------------------------
/*Returns the names of the variables to be used as outputs for the dynamic expression*/

String TWindGenObj::GetDynOutputStr()
{
    String result = "[";
    int idx = 0;
    
    if (DynamicEqObj != nullptr) // Making sure we have a dynamic eq linked
    {
        for (int stop = DynOut.size() - 1, idx = 0; idx <= stop; idx++)
            result = result + DynamicEqObj->Get_VarName(DynOut[idx]) + ",";
    }
    result = result + "]"; // Close array str
    return result;
}

//----------------------------------------------------------------------------

void TWindGenObj::CalcDailyMult(double hr)
{
	if(DailyDispShapeObj != nullptr)
	{
		ShapeFactor = DailyDispShapeObj->GetMult(hr);
		ShapeIsActual = DailyDispShapeObj->UseActual;
	}
	else
	ShapeFactor = CDoubleOne;  // Default to no daily variation
}


//----------------------------------------------------------------------------

void TWindGenObj::CalcDutyMult(double hr)
{
	if(DutyShapeObj != nullptr)
	{
		ShapeFactor = DutyShapeObj->GetMult(hr + DutyStart);
		ShapeIsActual = DutyShapeObj->UseActual;
	}
	else
	CalcDailyMult(hr);  // Default to Daily Mult if no duty curve specified
}

//----------------------------------------------------------------------------

void TWindGenObj::CalcYearlyMult(double hr)
{

/*Yearly curve is assumed to be hourly only*/
	if(YearlyShapeObj != nullptr)
	{
		ShapeFactor = YearlyShapeObj->GetMult(hr);
		ShapeIsActual = YearlyShapeObj->UseActual;
	}
	else
	ShapeFactor = CDoubleOne;  // Defaults to no variation
}



//----------------------------------------------------------------------------

void TWindGenObj::SetNominalGeneration(int ActorID)
{
    complex myV = CZero;
	double	VMag = 0.0, 
			VMagTmp = 0.0,	
			LeadLag = 0.0, 
			kVATmp = 0.0, 
			kvarCalc = 0.0, 
			myLosses = 0.0, 
			Factor = 0.0;
	bool	GenOn_Saved = false;
	int i = 0;


	GenOn_Saved = GenON;
	ShapeFactor = cmplx(WindModelDyn->vwind, 0);
	// Check to make sure the generation is ON
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
    auto with0 = ActiveCircuit[ActorID];
    auto with1 = ActiveCircuit[ActorID]->Solution;
	{
		{
			kvarCalc = 0.0;
			GenON = true; // The first assumption is that the generator is ON

			// first, get wind speed (Factor)
			if (IsFixed)
			{
				Factor = 1.0; // for fixed WindGens, set constant
			}
			else
			{
				switch (with1->Get_SolMode())
				{
				case SNAPSHOT:
					Factor = with0->GenMultiplier;
					break;
				case DAILYMODE:
				{
					Factor = with0->GenMultiplier;
					CalcDailyMult(with1->DynaVars.dblHour); // Daily dispatch curve
				}
				break;
				case YEARLYMODE:
				{
					Factor = with0->GenMultiplier;
					CalcYearlyMult(with1->DynaVars.dblHour);
				}
				break;
				case DUTYCYCLE:
				{
                    Factor = with0->GenMultiplier;
					CalcDutyMult(with1->DynaVars.dblHour);
				}
				break;
				case GENERALTIME:
				case // General sequential time simulation
					DYNAMICMODE:
				{
                    Factor = with0->GenMultiplier;
					// This mode allows use of one class of load shape
					switch (with0->ActiveLoadShapeClass)
					{
					case USEDAILY:
						CalcDailyMult(with1->DynaVars.dblHour);
						break;
					case USEYEARLY:
						CalcYearlyMult(with1->DynaVars.dblHour);
						break;
					case USEDUTY:
						CalcDutyMult(with1->DynaVars.dblHour);
						break;
					default:
						ShapeFactor = cmplx(WindModelDyn->vwind, 0); // default to the wind speed set by default
					}
				}
				break;
				case MONTECARLO1:
				case MONTEFAULT:
				case FAULTSTUDY:
					Factor = with0->GenMultiplier * 1.0;
					break;
				case MONTECARLO2:
				case MONTECARLO3:
				case LOADDURATION1:
				case LOADDURATION2:
				{
					Factor = with0->GenMultiplier;
					CalcDailyMult(with1->DynaVars.dblHour);
				}
				break;
				case PEAKDAY:
				{
					Factor = with0->GenMultiplier;
					CalcDailyMult(with1->DynaVars.dblHour);
				}
				break;
				case AUTOADDFLAG:
					Factor = 1.0;
					break;
				default:
					Factor = with0->GenMultiplier;
				}
			}
			WindModelDyn->vwind = ShapeFactor.re;
			if ((ShapeFactor.re > WindGenVars.VCutout) || (ShapeFactor.re < WindGenVars.VCutin))
			{
				WindGenVars.Pnominalperphase = 0.001 * kWBase;
				WindGenVars.Qnominalperphase = 0.0;
				WindGenVars.Pm = 0.0;
				WindGenVars.Pg = 0.0;
				WindGenVars.Ps = 0.0;
				WindGenVars.Pr = 0.0;
				WindGenVars.s = 0.0;
			}
			else
			{
				if (!(with1->IsDynamicModel || with1->IsHarmonicModel)) //******
				{
					// start by getting the losses from the provided curve (if any)
					if (ASSIGNED(Loss_CurveObj))
						myLosses = Loss_CurveObj->GetYValue_(WindModelDyn->vwind);
					else
						myLosses = 0.0; // no losses given that the curve was not provided
					LeadLag = 1;
					/*# with WindGenVars do */
					{
                        auto& with2 = WindGenVars;

						with2.Pm = 0.5 * with2.pd * PI * pow(with2.Rad, 2) * pow(ShapeFactor.re, 3) * with2.Cp;
                        myLosses = with2.Pm * myLosses / 100;
                        with2.Pg = (with2.Pm - myLosses) / 1e3; // in kW
                        if (with2.Pg > kWBase)
                            with2.Pg = kWBase; // Generation limits
                        with2.s = 1 - ((with2.Poles * ShapeFactor.re * with2.Lamda) / (with2.w0 * with2.ag * with2.Rad));
                        with2.Ps = with2.Pg / (1 - with2.s);
                        with2.Pr = with2.Ps * with2.s;
                        with2.Pnominalperphase = (1e3 * Factor * with2.Pg) / Fnphases;
						// Now check for Q depending on QMode
						switch (WindModelDyn->QMode)
						{
							case 1: // PF
							{
                                kvarCalc = pow((with2.Pg / Abs(PFNominal)), 2) - pow(with2.Pg, 2);
								kvarCalc = sqrt(kvarCalc);
                                kVATmp = sqrt(pow(with2.Pg, 2) + pow(kvarCalc, 2));
                                if (kVATmp > with2.kVArating) // Check saturation
									kvarCalc = kvarBase;
								if (PFNominal < 0)
									LeadLag = -1.0;
							}
							break;
							case 2: // Volt-var ctrl
							{
								if (!(NodeRef.empty()))
								{
									// get the highest voltage done locally given with whatever is on memory
									for (int stop = WindGenVars.NumPhases, i = 1; i <= stop; i++)
									{
										myV = with1->NodeV[NodeRef[i - 1]];
										VMagTmp = ctopolar(myV).mag;
										if (VMagTmp > VMag)
											VMag = VMagTmp;
									}
									VMag = VMag / VBase; // in pu

									// start by getting the losses from the provided curve (if any)
									if ((VV_CurveObj != nullptr))
										VMagTmp = VV_CurveObj->GetYValue_(VMag);
									else
										VMagTmp = 0.0; // no losses given that the curve was not provided
								}
								else
									VMagTmp = 0.0;

								// Calculates Q based on the
								kvarCalc = kvarBase * VMagTmp;
								if (Abs(kvarCalc) > kvarBase)
								{
									kvarCalc = kvarBase;
									if (VMagTmp < 0)
										LeadLag = -1.0;
								}
							}
							break;
							default:
								kvarCalc = 0;
						}
						WindGenVars.Qnominalperphase = 1e3 * kvarCalc * LeadLag * Factor / Fnphases;
					}
				}
			}
		}
	}
    if (!(with1->IsDynamicModel || with1->IsHarmonicModel))    //******
    {
        // build the Y primitive eq
        switch (GenModel)
        {
			case 6:
				Yeq = cinv(cmplx(0, -WindGenVars.XD));	// Gets negated in CalcYPrim
            break;
            default:
				auto with4 = WindGenVars;
                Yeq = cdivreal(cmplx(with4.Pnominalperphase, -with4.Qnominalperphase), sqr(VBase)); // Vbase must be L-N for 3-phase
                if (Vminpu != 0)
                    Yeq95 = cdivreal(Yeq, sqr(Vminpu)); // at 95% voltage
                else
					Yeq95 = Yeq; // Always a constant Z model
                if (Vmaxpu != 0)
                    Yeq105 = cdivreal(Yeq, sqr(Vmaxpu)); // at 105% voltage
                else
                    Yeq105 = Yeq; 
            break;
        }
    }

}

//----------------------------------------------------------------------------

void TWindGenObj::RecalcElementData(int ActorID)
{
	VBase95 = Vminpu * VBase;
	VBase105 = Vmaxpu * VBase;
	varBase = 1000.0 * kvarBase / Fnphases;
	varMin = 1000.0 * kvarMin / Fnphases;
	varMax = 1000.0 * kvarMax / Fnphases;

    /*Populate data structures used for interchange with user-written models.*/
	/*# with WindGenVars do */
	{
		auto& with0 = WindGenVars;
		with0.XD =	with0.puXd * 1000.0 * Sqr(with0.kVWindGenBase) / with0.kVArating;
		with0.Xdp = with0.puXdp * 1000.0 * Sqr(with0.kVWindGenBase) / with0.kVArating;
		with0.Xdpp = with0.puXdpp * 1000.0 * Sqr(with0.kVWindGenBase) / with0.kVArating;
		with0.Conn = Connection;
		with0.NumPhases = Fnphases;
		with0.NumConductors = Fnconds;

		if (!kVANotSet)
        {
            kWBase = (with0.kVArating * Abs(PFNominal));
            kvarBase = sqrt(sqr(with0.kVArating) - sqr(kWBase));
        }
        else
        {
			with0.kVArating = kWBase / Abs(PFNominal);
            WindModelDyn->EditProp(13, to_string(with0.kVArating));
        }
	}
	SetNominalGeneration(ActorID);

    /*Now check for errors.  If any of these came out nil and the string was not nil, give warning*/
	if(CompareText(YearlyShape,"none") == 0)
		YearlyShape ="";
	if(CompareText(DailyDispShape,"none") == 0)
		DailyDispShape ="";
	if(CompareText(DutyShape,"none") == 0)
		DutyShape ="";
	if(YearlyShapeObj == nullptr)
	{
		if(YearlyShape.size() > 0)
			DoSimpleMsg("WARNING! Yearly load shape: \"" + YearlyShape + "\" Not Found.", 563);
	}
	if(DailyDispShapeObj == nullptr)
	{
		if(DailyDispShape.size() > 0)
			DoSimpleMsg("WARNING! Daily load shape: \"" + DailyDispShape + "\" Not Found.", 564);
	}
	if(DutyShapeObj == nullptr)
	{
		if(DutyShape.size() > 0)
			DoSimpleMsg("WARNING! Duty load shape: \"" + DutyShape + "\" Not Found.", 565);
	}
	SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
	if(SpectrumObj == nullptr)
		DoSimpleMsg("ERROR! Spectrum \"" + Spectrum +"\" Not Found.", 566);
	YQFixed = -varBase / Sqr(VBase);   //10-17-02  Fixed negative sign
	WindGenVars.VTarget = Vpu * 1000.0 * WindGenVars.kVWindGenBase;
	if(Fnphases > 1)
		WindGenVars.VTarget = WindGenVars.VTarget / SQRT3;

    // Initialize to Zero - defaults to PQ WindGen
    // Solution object will reset after circuit modifications
	DQDV = DQDVSaved;         // for Model = 3
	DeltaQMax = (varMax - varMin) * 0.10;  // Limit to 10% of range
	InjCurrent = (pComplexArray)realloc(InjCurrent, sizeof(complex) * Yorder);

    /*Update any user-written models*/
	if(UserModel->Get_Exists())
		UserModel->FUpdateModel();
	if(ShaftModel->Get_Exists())
		ShaftModel->FUpdateModel();

	if (ASSIGNED(WindModelDyn))
		WindModelDyn->ReCalcElementData();
}

void TWindGenObj::CalcYPrimMatrix(Ucmatrix::TcMatrix* Ymatrix, int ActorID)
{
    complex Y	= CZero, 
			Yij = CZero;
    int		i = 0, 
			j = 0;
    double	FreqMultiplier = 0.0;
    double	WTGZLV = 0.0;

	auto with0 = ActiveCircuit[ActorID];
    auto with1 = with0->Solution;

    FYprimFreq = with1->get_FFrequency();
    FreqMultiplier = FYprimFreq / BaseFrequency;
    /*# with ActiveCircuit[ActorID].Solution do */

    if (with1->IsDynamicModel || with1->IsHarmonicModel)
    {
        if (GenON)
        {
            /*# with WindModelDyn do */
            auto with2 = WindModelDyn;
            {
                WTGZLV = sqr(Get_PresentkV()) * 1e3 / WindGenVars.kVArating;
                Y = cmplx(EPSILON, (-with2->N_WTG) / (with2->Xthev * WTGZLV)); // Yeq  // L-N value computed in initial condition routines
            }
        }
        else
            Y = cmplx(EPSILON, 0.0);
        if (Connection == 1)
            Y = cdivreal(Y, 3.0); // Convert to delta impedance
        Y.im = double(Y.im) / FreqMultiplier;
        Yij = cnegate(Y);
        for (int stop = Fnphases, i = 1; i <= stop; i++)
        {
            switch (Connection)
            {
				case 0:
				{
					Ymatrix->SetElement(i, i, Y);
					Ymatrix->AddElement(Fnconds, Fnconds, Y);
					Ymatrix->SetElemsym(i, Fnconds, Yij);
				}
				break;
				case 1:
				{ /*Delta connection*/
					Ymatrix->SetElement(i, i, Y);
					Ymatrix->AddElement(i, i, Y); // put it in again
					for (int stop = i - 1, j = 1; j <= stop; j++)
						Ymatrix->SetElemsym(i, j, Yij);
				}
				break;
                default:
                break;
            }
        }

        /**** Removed Neutral / Neutral may float

         IF Connection = 0 Then   With Ymatrix Do  // Take care of neutral issues
           Begin
             AddElement(Fnconds, Fnconds, YNeut);  // Add in user specified Neutral Z, if any
             // Bump up neutral-ground in case neutral ends up floating
             SetElement(Fnconds, Fnconds, CmulReal(GetElement(Fnconds, Fnconds), 1.000001));
           End;

        */
    }
    else
    { //  Regular power flow WindGen model

        /*Yeq is always expected as the equivalent line-neutral admittance*/
        Y = cnegate(Yeq); // negate for generation    Yeq is L-N quantity
                          // ****** Need to modify the base admittance for real harmonics calcs
        Y.im = (Y.im) / FreqMultiplier;
        auto with3 = Ymatrix;
        switch (Connection)
        {
			case 0:
            /*# with Ymatrix do */

            { // WYE
                Yij = cnegate(Y);
                for (int stop = Fnphases, i = 1; i <= stop; i++)
                {
                    with3->SetElement(i, i, Y);
                    with3->AddElement(Fnconds, Fnconds, Y);
                    with3->SetElemsym(i, Fnconds, Yij);
                }
            }
            break;
			case 1:
            /*# with Ymatrix do */
            { // Delta  or L-L
                Y = cdivreal(Y, 3.0); // Convert to delta impedance
                Yij = cnegate(Y);
                for (int stop = Fnphases, i = 1; i <= stop; i++)
                {
                    j = i + 1;
                    if (j > Fnconds)
                        j = 1; // wrap around for closed connections
                    with3->AddElement(i, i, Y);
                    with3->AddElement(j, j, Y);
                    with3->AddElemsym(i, j, Yij);
                }
            }
            break;
            default:
            break;
        }
    } /*ELSE IF Solution.mode*/
}


//----------------------------------------------------------------------------

void TWindGenObj::CalcYPrim(int ActorID)
{
	int i = 0;

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV does not fail
	int stop = 0;
	if(Get_YprimInvalid(ActorID,0))
	{
		if(YPrim_Shunt != nullptr)
			delete YPrim_Shunt;
		YPrim_Shunt = new TcMatrix(Yorder);
		if(YPrim_Series != nullptr)
			delete YPrim_Series;
		YPrim_Series = new TcMatrix(Yorder);
		if(YPrim != nullptr)
			delete YPrim;
		YPrim = new TcMatrix(Yorder);
	}
	else
	{
		YPrim_Shunt->Clear();
		YPrim_Series->Clear();
		YPrim->Clear();
	}
	if(ActiveCircuit[ActorID]->Solution->LoadModel == POWERFLOW)
     
        // 12-7-99 we'll start with Yeq in system matrix
	{
		SetNominalGeneration(ActorID);
		CalcYPrimMatrix(YPrim_Shunt, ActorID);
	}
	else


         // ADMITTANCE model wanted
	{
		SetNominalGeneration(ActorID);
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

 /*Add the current into the proper location according to connection*/

 /*Reverse of similar routine in load  (Cnegates are switched)*/

void TWindGenObj::StickCurrInTerminalArray(pComplexArray TermArray, const complex& Curr, int i)
{
	int j = 0;
	switch(Connection)
	{
		case 	0:  //Wye
		{
			caccum((TermArray)[i - 1], Curr);
			caccum((TermArray)[Fnconds - 1], cnegate(Curr)); // Neutral
		}
		break; //DELTA
		case 	1:
		{
			caccum((TermArray)[i - 1], Curr);
			j = i + 1;
			if(j > Fnconds)
				j = 1;
			caccum((TermArray)[j - 1], cnegate(Curr));
		}
		break;
		default:
		  ;
		break;
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TWindGenObj::WriteTraceRecord(const String s, int ActorID)
{
	int i = 0;
	try
	{
		if(!InShowResults)
		{
			int stop = 0;
			Append(Tracefile);
			IOResultToException();
			{ Write(Tracefile, Format("%-.g, %d, %-.g, ", 
				ActiveCircuit[ActorID]->Solution->DynaVars.T + ActiveCircuit[ActorID]->Solution->DynaVars.intHour * 3600.0, 
					ActiveCircuit[ActorID]->Solution->Iteration,
					ActiveCircuit[ActorID]->get_FLoadMultiplier())); 

			Write(Tracefile, GetSolutionModeID()); 
			Write(Tracefile,", "); Write(Tracefile, 
				GetLoadModel()); Write(Tracefile,", "); 
			Write(Tracefile, GenModel, 0); 
			Write(Tracefile,", "); 
			Write(Tracefile, DQDV, 8, 0); 
			Write(Tracefile,", "); 
			Write(Tracefile, (V_Avg * 0.001732 / WindGenVars.kVWindGenBase), 8, 3); 
			Write(Tracefile,", "); 
			Write(Tracefile, (WindGenVars.VTarget - V_Avg), 9, 1); 
			Write(Tracefile,", "); 
			Write(Tracefile, (WindGenVars.Qnominalperphase * 3.0 / 1.0e6), 8, 2); 
			Write(Tracefile,", "); 
			Write(Tracefile, (WindGenVars.Pnominalperphase * 3.0 / 1.0e6), 8, 2); 
			Write(Tracefile,", "); 
			Write(Tracefile, s); 
			Write(Tracefile,", "); }

			for(stop = Get_NPhases(), i = 1; i <= stop; i++)
			{
				{ Write(Tracefile, (cabs((InjCurrent)[i - 1])), 8, 1); Write(Tracefile,", "); }
			}
			for(stop = Get_NPhases(), i = 1; i <= stop; i++)
			{
				{ Write(Tracefile, (cabs((Iterminal)[i - 1])), 8, 1); Write(Tracefile,", "); }
			}
			for(stop = Get_NPhases(), i = 1; i <= stop; i++)
			{
				{ Write(Tracefile, (cabs((Vterminal)[i - 1])), 8, 1); Write(Tracefile,", "); }
			}
			{ Write(Tracefile, WindGenVars.VthevMag, 8, 1); Write(Tracefile,", "); Write(Tracefile, WindGenVars.Theta * 180.0 / DSSGlobals::PI); }
			WriteLn(Tracefile);
			CloseFile(Tracefile);
		}
	}
	catch (...)
	{
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*Compute total terminal current for Constant PQ*/

void TWindGenObj::DoConstantPQGen(int ActorID)
{
	int i = 0;
	complex Curr = CZero;
	complex V = CZero;
	double Vmag = 0.0;
//   V012,I012 :Array[0..2] of Complex;
//   Iabc :Array[1..3] of Complex;

     //Treat this just like the Load model
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    for (i = 0; i < Fnconds; i++)
        InjCurrent[i] = CZero;

	ZeroITerminal();

	CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load

	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = (Vterminal)[i - 1];
		Vmag = cabs(V);
		switch(Connection)
		{
			case 	0:  /*Wye*/
			{
				if(Vmag <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(Yeq95, V);
				else
				{
					if(Vmag > VBase105)  // above 105% use an impedance model
						Curr = cmul(Yeq105, V);
					else
						/*# with WindGenVars do */
						{
							auto with0 = WindGenVars;
							Curr = conjg(cdiv(cmplx(with0.Pnominalperphase, with0.Qnominalperphase), V));
						}  // Between 95% -105%, constant PQ
				}
			}
			break;  /*Delta*/
			case 	1:
			{
				switch(Fnphases)
				{
					case 	2:
					 case 3:
					Vmag = Vmag / SQRT3;
					break;  // L-N magnitude
					
                        /*leave Vmag as is*/
					default:
					  ;
					break;
				}
				if(Vmag <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(cdivreal(Yeq95, 3.0), V);
				else
				{
					if(Vmag > VBase105)  // above 105% use an impedance model
						Curr = cmul(cdivreal(Yeq105, 3.0), V);
					else
						/*# with WindGenVars do */
						{
							auto& with1 = WindGenVars;
							Curr = conjg(cdiv(cmplx(with1.Pnominalperphase, with1.Qnominalperphase), V));
						}  // Between 95% -105%, constant PQ
				}
			}
			break;
			default:
			  ;
			break;
		}
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
    /*END;*/
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TWindGenObj::DoConstantZGen(int ActorID)
{
	int i = 0;
	complex Curr = {};
	complex YEQ2 = {};

// Assume Yeq is kept up to date
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
	ZeroITerminal();
	if(Connection == 0)
		YEQ2 = Yeq;
	else
		YEQ2 = cdivreal(Yeq, 3.0);
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		Curr = cmul(YEQ2, (Vterminal)[i - 1]);   // Yeq is always line to neutral
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

/*Compute total terminal current for Constant P,|V|*/

// Constant P, constant |V|

void TWindGenObj::DoPVTypeGen(int ActorID)
{
	int i = 0;
	double DQ = 0.0;
	complex Curr = {};
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the WindGen
	ZeroITerminal();

    // Guess at a new var output value
	V_Avg = 0.0;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V_Avg = V_Avg + cabs((Vterminal)[i - 1]);
	}
	if(Connection == 1)
		V_Avg = V_Avg / (SQRT3 * Fnphases);
	else
		V_Avg = V_Avg / Fnphases;

   // 12-9-99 added empirical 0.7 factor to improve iteration
   // 12-17-99 changed to 0.1 because first guess was consistently too high
	DQ = PVFactor * DQDV * (WindGenVars.VTarget - V_Avg);   // Vtarget is L-N
	if(Abs((int) DQ) > DeltaQMax)
	{
		if(DQ < 0.0)
			DQ = -DeltaQMax;
		else
			DQ = DeltaQMax;
	}
	/*# with WindGenVars do */
	{
		auto& with0 = WindGenVars;
		with0.Qnominalperphase = with0.Qnominalperphase + DQ;
	}

   /* Test Limits*/
	/*# with WindGenVars do */
	{
		auto& with1 = WindGenVars;
		int stop = 0;
		if(with1.Qnominalperphase > varMax)
			with1.Qnominalperphase = varMax;
		else
		{
			if(with1.Qnominalperphase < varMin)
				with1.Qnominalperphase = varMin;

       // Compute injection currents using W and var values
       // Do not use comstant Z models outside normal range
       // Presumably the var source will take care of the voltage problems
		}
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			Curr = conjg(cdiv(cmplx(with1.Pnominalperphase, with1.Qnominalperphase), (Vterminal)[i - 1]));
			StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
			set_ITerminalUpdated(true, ActorID);
			StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
		}
	} /*With*/
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*Compute total terminal current for Fixed Q*/
// Constant P, Fixed Q  Q is always kvarBase

void TWindGenObj::DoFixedQGen(int ActorID)
{
	int i = 0;
	complex Curr = {};
	complex V = {};
	double Vmag = 0.0;
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = (Vterminal)[i - 1];
		Vmag = cabs(V);
		switch(Connection)
		{
			case 	0:
			{
				if(Vmag <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(cmplx(Yeq95.re, YQFixed), V);
				else
				{
					if(Vmag > VBase105)  // above 105% use an impedance model
						Curr = cmul(cmplx(Yeq105.re, YQFixed), V);
					else
						Curr = conjg(cdiv(cmplx(WindGenVars.Pnominalperphase, varBase), V));
				}
			}
			break;
			case 	1:
			{
				switch(Fnphases)
				{
					case 	2:
					 case 3:
					Vmag = Vmag / SQRT3;
					break;  // L-N magnitude
					
                    /*leave Vmag as is*/
					default:
					  ;
					break;
				}
				if(Vmag <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(cmplx(Yeq95.re / 3.0, YQFixed / 3.0), V);
				else
				{
					if(Vmag > VBase105)  // above 105% use an impedance model
						Curr = cmul(cmplx(Yeq105.re / 3.0, YQFixed / 3.0), V);
					else
						Curr = conjg(cdiv(cmplx(WindGenVars.Pnominalperphase, varBase), V));
				}
			}
			break;
			default:
			  ;
			break;
		}
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*Compute total terminal current for */
// Constant P, Fixed Q  Q is always a fixed Z derived from kvarBase

void TWindGenObj::DoFixedQZGen(int ActorID)
{
	int i = 0;
	complex Curr = {};
	complex V = {};
	double Vmag = 0.0;
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V = (Vterminal)[i - 1];
		Vmag = cabs(V);
		switch(Connection)
		{
			case 	0:
			{
				if(Vmag <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(cmplx(Yeq95.re, YQFixed), V);
				else
				{
					if(Vmag > VBase105)
						Curr = cmul(cmplx(Yeq105.re, YQFixed), V);
					else
					{
						Curr = conjg(cdiv(cmplx(WindGenVars.Pnominalperphase, 0.0), V)); // P component of current
						caccum(Curr, cmul(cmplx(0.0, YQFixed), V));  // add in Q component of current
					}
				}
			}
			break;
			case 	1:
			{
				switch(Fnphases)
				{
					case 	2:
					 case 3:
					Vmag = Vmag / SQRT3;
					break;  // L-N magnitude
					
                      /*leave Vmag as is*/
					default:
					  ;
					break;
				}
				if(Vmag <= VBase95)  // Below 95% use an impedance model
					Curr = cmul(cmplx(Yeq95.re / 3.0, YQFixed / 3.0), V);
				else
				{
					if(Vmag > VBase105)
						Curr = cmul(cmplx(Yeq105.re / 3.0, YQFixed / 3.0), V);
					else
					{
						Curr = conjg(cdiv(cmplx(WindGenVars.Pnominalperphase, 0.0), V)); // P component of current
						caccum(Curr, cmul(cmplx(0.0, YQFixed / 3.0), V));  // add in Q component of current
					}
				}
			}
			break;
			default:
			  ;
			break;
		}
		StickCurrInTerminalArray(&(Iterminal[0]), cnegate(Curr), i);  // Put into Terminal array taking into account connection
		set_ITerminalUpdated(true, ActorID);
		StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
	} /*FOR*/
}
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

/*Compute total terminal Current from User-written model*/

void TWindGenObj::DoUserModel(int ActorID)
{
	int i = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	if(UserModel->Get_Exists())    // Check automatically selects the usermodel if true

         //AppendToEventLog('Wnominal=', Format('%-.5g',[Pnominalperphase]));
	{
		UserModel->FCalc(&(Vterminal[0]), &(Iterminal[0]));
		set_ITerminalUpdated(true, ActorID);
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;          // Negate currents from user model for power flow WindGen model
			int stop = 0;
			for(stop = Fnconds, i = 1; i <= stop; i++)
			{
				caccum((InjCurrent)[i - 1], cnegate((Iterminal)[i - 1]));
			}
		}
	}
	else
	{
		DoSimpleMsg(String("WindGen.") + get_Name()
	           +" model designated to use user-written model, but user-written model is not defined.", 567);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

/*Compute total terminal current for Constant PQ, but limit to max current below
 Vminpu*/

void TWindGenObj::DoCurrentLimitedPQ(int ActorID)
{
	int i = 0;
	complex PhaseCurr	= CZero;
	complex DeltaCurr	= CZero;
	complex VLN			= CZero;
	complex VLL			= CZero;
	double	VmagLN = 0.0;
	double	VmagLL = 0.0;
	complex V012[4] = { CZero, CZero, CZero, CZero, };  // Sequence voltages

     //Treat this just like the Load model
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
	if(ForceBalanced && (Fnphases == 3))    // convert to pos-seq only
	{
		Phase2SymComp(&(Vterminal[0]), &V012[0]);
		V012[0] = CZero; // Force zero-sequence voltage to zero
		V012[2] = CZero; // Force negative-sequence voltage to zero
		SymComp2Phase(&(Vterminal[0]), &V012[0]);  // Reconstitute Vterminal as balanced
	}
	ZeroITerminal();
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		switch(Connection)
		{
			case 	0:
			{
				VLN = (Vterminal)[i - 1];   // VTerminal is LN for this connection
				VmagLN = cabs(VLN);
				/*# with WindGenVars do */
				{
					auto& with0 = WindGenVars;
					PhaseCurr = conjg(cdiv(cmplx(with0.Pnominalperphase, with0.Qnominalperphase), VLN));
				}
				if(cabs(PhaseCurr) > Model7MaxPhaseCurr)
					PhaseCurr = conjg(cdiv(PhaseCurrentLimit, cdivreal(VLN, VmagLN)));
				StickCurrInTerminalArray(&(Iterminal[0]), cnegate(PhaseCurr), i);  // Put into Terminal array taking into account connection
				set_ITerminalUpdated(true, ActorID);
				StickCurrInTerminalArray(InjCurrent, PhaseCurr, i);  // Put into Terminal array taking into account connection
			}
			break;
			case 	1:
			{
				VLL = (Vterminal)[i - 1];     // VTerminal is LL for this connection
				VmagLL = cabs(VLL);
				switch(Fnphases)
				{
					case 	2:
					 case 3:   // 2 or 3 phase WindGen model 7
					{
						/*# with WindGenVars do */
						{
							auto& with1 = WindGenVars;
							DeltaCurr = conjg(cdiv(cmplx(with1.Pnominalperphase, with1.Qnominalperphase), VLL));
						}
						if(cabs(DeltaCurr) * SQRT3 > Model7MaxPhaseCurr)
							DeltaCurr = conjg(cdiv(PhaseCurrentLimit, cdivreal(VLL, VmagLL / SQRT3)));
					}
					break;  // 1-phase WindGen model 7
					default:
					/*# with WindGenVars do */
					{
						auto& with2 = WindGenVars;
						DeltaCurr = conjg(cdiv(cmplx(with2.Pnominalperphase, with2.Qnominalperphase), VLL));
					}
					if(cabs(DeltaCurr) > Model7MaxPhaseCurr)
						DeltaCurr = conjg(cdiv(PhaseCurrentLimit, cdivreal(VLL, VmagLL)));
					break;
				}
				StickCurrInTerminalArray(&(Iterminal[0]), cnegate(DeltaCurr), i);  // Put into Terminal array taking into account connection
				set_ITerminalUpdated(true, ActorID);
				StickCurrInTerminalArray(InjCurrent, DeltaCurr, i);  // Put into Terminal array taking into account connection
			}
			break;
			default:
			  ;
			break;
		}
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*Compute Total Current and add into InjTemp*/

void TWindGenObj::DoDynamicMode(int ActorID)
{
	int i = 0;
	complex V012[4] = { CZero, CZero, CZero, CZero, };
	complex I012[4] = { CZero, CZero, CZero, CZero, };
	int stop = 0;
	//CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array  and computes VTerminal L-N
    ComputeVterminal(ActorID);
   
    for (i = 0; i < Get_NConds(); i++)
        InjCurrent[i] = CZero;

	/*Inj = -Itotal (in) - Yprim*Vtemp*/
	switch(GenModel)
	{
		case 	6:
		if(UserModel->Get_Exists())       // auto selects model
			   /*We have total currents in Iterminal*/
			{
				UserModel->FCalc(&(Vterminal[0]), &(Iterminal[0]));  // returns terminal currents in Iterminal
			}
		else
		{
			DoSimpleMsg("Dynamics model missing for WindGen." + get_Name() + " ", 5671);
			DSSGlobals::SolutionAbort = true;
		}
		break;
		default:
            WindModelDyn->CalcDynamic(&(Vterminal[0]), &(Iterminal[0]));
		break;
	}
	set_ITerminalUpdated(true, ActorID);

    /*Add it into inj current array*/
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		caccum((InjCurrent)[i - 1], cnegate((Iterminal)[i - 1]));
	}

   /*Take Care of any shaft model calcs*/
	if((GenModel == 6) && ShaftModel->Get_Exists())      // auto selects model
           // Compute Mech Power to shaft
	{
		ShaftModel->FCalc(&(Vterminal[0]), &(Iterminal[0]));     // Returns pshaft at least
	}
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*Compute Injection Current Only when in harmonics mode*/

/*Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built*/
/*Vd is the fundamental frequency voltage behind Xd" for phase 1*/

void TWindGenObj::DoHarmonicMode(int ActorID)
{
	int i = 0;
	complex e = {};
	double GenHarmonic = 0.0;
	ComputeVterminal(ActorID);
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		int stop = 0;
		GenHarmonic = with0->get_FFrequency() / GenFundamental;
		e = cmulreal(SpectrumObj->GetMult(GenHarmonic), WindGenVars.VThevHarm); // Get base harmonic magnitude
		RotatePhasorRad(e, GenHarmonic, WindGenVars.ThetaHarm);  // Time shift by fundamental frequency phase shift
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			cBuffer[i - 1] = e;
			if(i < Fnphases)
				RotatePhasorDeg(e, GenHarmonic, -120.0);  // Assume 3-phase WindGen
		}
	}

   /*Handle Wye Connection*/
	if(Connection == 0)
		cBuffer[Fnconds - 1] = (Vterminal)[Fnconds - 1];  // assume no neutral injection voltage
		
   /*Inj currents = Yprim (E) */
	YPrim->MVmult(InjCurrent, (pComplexArray) &cBuffer);
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TWindGenObj::CalcVTerminalPhase(int ActorID)
{
	int i = 0;
	int j = 0;

/* Establish phase voltages and stick in Vterminal*/
	switch(Connection)
	{
		case 	0:
		{
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with0 = ActiveCircuit[ActorID]->Solution;
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					(Vterminal)[i - 1] = with0->VDiff((NodeRef)[i - 1], (NodeRef)[Fnconds - 1], ActorID);
				}
			}
		}
		break;
		case 	1:
		{
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with1 = ActiveCircuit[ActorID]->Solution;
				int stop = 0;
				for(stop = Fnphases, i = 1; i <= stop; i++)
				{
					j = i + 1;
					if(j > Fnconds)
						j = 1;
					(Vterminal)[i - 1] = with1->VDiff((NodeRef)[i - 1], (NodeRef)[j - 1], ActorID);
				}
			}
		}
		break;
		default:
		  ;
		break;
	}
	WindGenSolutionCount = ActiveCircuit[ActorID]->Solution->SolutionCount;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


/*Put terminal voltages in an array*/

void TWindGenObj::CalcVterminal(int ActorID)
{
	ComputeVterminal(ActorID);
	WindGenSolutionCount = ActiveCircuit[ActorID]->Solution->SolutionCount;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

// Calculates WindGen current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

void TWindGenObj::CalcGenModelContribution(int ActorID)
{
	set_ITerminalUpdated(false, ActorID);
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
	{
		
		auto with1 = ActiveCircuit[ActorID]->Solution;
		if(with1->IsDynamicModel)
			DoDynamicMode(ActorID);
		else
		{
			if(with1->IsHarmonicModel && (with1->get_FFrequency() != ActiveCircuit[ActorID]->Fundamental))
				DoHarmonicMode(ActorID);
			else

           //  compute currents and put into InjTemp array;
			{
				switch(GenModel)
				{
					case 	1:
					DoConstantPQGen(ActorID);
					break;
					case 	2:
					DoConstantZGen(ActorID);
					break;
					case 	3:
					DoPVTypeGen(ActorID);
					break;  // Constant P, |V|
					case 	4:
					DoFixedQGen(ActorID);
					break;
					case 	5:
					DoFixedQZGen(ActorID);
					break;
					case 	6:
					DoUserModel(ActorID);
					break;
					case 	7:
					DoCurrentLimitedPQ(ActorID);
					break;
					default:
					DoConstantPQGen(ActorID);  // for now, until we implement the other models.
					break;
				}
			} /*ELSE*/
		}
	} /*WITH*/

   /*When this is done, ITerminal is up to date*/
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -



// Difference between currents in YPrim and total current

void TWindGenObj::CalcInjCurrentArray(int ActorID)
{

      

// Now Get Injection Currents
	if(GenSwitchOpen)
		ZeroInjCurrent();
	else
		CalcGenModelContribution(ActorID);

}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Compute total Currents

void TWindGenObj::GetTerminalCurrents(pComplexArray Curr, int ActorID)
{
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(IterminalSolutionCount[ActorID] != ActiveCircuit[ActorID]->Solution->SolutionCount)     // recalc the contribution
		{
			if(!GenSwitchOpen)
				CalcGenModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
		}
		inherited::GetTerminalCurrents(Curr, ActorID);
	}
	if(DebugTrace)
		WriteTraceRecord("TotalCurrent", ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

int TWindGenObj::InjCurrents(int ActorID)
{
	int result = 0;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(with0->LoadsNeedUpdating)
			SetNominalGeneration(ActorID); // Set the nominal kW, etc for the type of solution being done
		CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current
		if(DebugTrace)
			WriteTraceRecord("Injection", ActorID);

       // Add into System Injection Current Array
		result = inherited::InjCurrents(ActorID);
	}
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Gives the currents for the last solution performed

// Do not call SetNominalLoad, as that may change the load values

void TWindGenObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	CalcInjCurrentArray(ActorID);  // Difference between currents in YPrim and total current
	try

   // Copy into buffer array
	{
		int stop = 0;
		for(stop = Yorder, i = 1; i <= stop; i++)
		{
			(Curr)[i - 1] = (InjCurrent)[i - 1];
		}
	}
	catch (std::exception &e)
	{
		DoErrorMsg(String("WindGen Object: \"") + get_Name() +"\" in GetInjCurrents function.", (std::string) e.what(), "Current buffer not big enough.", 568);
	}
}
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TWindGenObj::ResetRegisters()
{
	int i = 0;
	int stop = 0;
	for(stop = NumWGenRegisters, i = 1; i <= stop; i++)
	{
		Registers[i - 1] = 0.0;
	}
	for(stop = NumWGenRegisters, i = 1; i <= stop; i++)
	{
		Derivatives[i - 1] = 0.0;
	}
	FirstSampleAfterReset = true;  // initialize for trapezoidal integration
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TWindGenObj::Integrate(int reg, double Deriv, double Interval, int ActorID)
{
	if(ActiveCircuit[ActorID]->TrapezoidalIntegration)
        /*Trapezoidal Rule Integration*/
	{
		if(!FirstSampleAfterReset)
			Registers[reg] = Registers[reg] + 0.5 * Interval * (Deriv + Derivatives[reg]);
	}
	else
   /*Plain Euler integration*/
	Registers[reg] = Registers[reg] + Interval * Deriv;
	Derivatives[reg] = Deriv;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// Update Energy from metered zone

void TWindGenObj::TakeSample(int ActorID)
{
	complex s = {};
	double Smag = 0.0;
	double HourValue = 0.0;

// Compute energy in WindGen branch
	if(Get_Enabled())
	{
		if(GenON)
		{
			s = cmplx(Get_PresentkW(), Get_Presentkvar());
			Smag = cabs(s);
			HourValue = 1.0;
		}
		else
		{
			s = CZero;
			Smag = 0.0;
			HourValue = 0.0;
		}
		if(GenON || ActiveCircuit[ActorID]->TrapezoidalIntegration)
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with0 = ActiveCircuit[ActorID]->Solution;
      /*Make sure we always integrate for Trapezoidal case
       Don't need to for Gen Off and normal integration*/
				if(ActiveCircuit[ActorID]->PositiveSequence)
				{
					s = cmulreal(s, 3.0);
					Smag = 3.0 * Smag;
				}
				Integrate(Reg_kWh, s.re, with0->IntervalHrs, ActorID);   // Accumulate the power
				Integrate(Reg_kvarh, s.im, with0->IntervalHrs, ActorID);
				SetDragHandRegister(Reg_MaxkW, Abs((int) s.re));
				SetDragHandRegister(Reg_MaxkVA, Smag);
				Integrate(Reg_Hours, HourValue, with0->IntervalHrs, ActorID);  // Accumulate Hours in operation
				Integrate(Reg_Price, s.re * ActiveCircuit[ActorID]->PriceSignal * 0.001, with0->IntervalHrs, ActorID);  // Accumulate Hours in operation
				FirstSampleAfterReset = false;
			}
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

double TWindGenObj::Get_PresentkW()
{
	double result = 0.0;
	result = WindGenVars.Pnominalperphase * 0.001 * Fnphases;
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

double TWindGenObj::Get_PresentkV()
{
	double result = 0.0;
	result = WindGenVars.kVWindGenBase;
	return result;
}

double TWindGenObj::Get_Presentkvar()
{
	double result = 0.0;
	result = WindGenVars.Qnominalperphase * 0.001 * Fnphases;
	return result;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TWindGenObj::InitDQDVCalc()
{
	DQDV = 0.0;
	WindGenVars.Qnominalperphase = 0.5 * (varMax + varMin);   // avg of the limits
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

/*Bump up vars by 10% of range for next calc*/

void TWindGenObj::BumpUpQ()
{
	/*# with WindGenVars do */
	{
		auto& with0 = WindGenVars;
		with0.Qnominalperphase = with0.Qnominalperphase + 0.1 * (varMax - varMin);
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TWindGenObj::RememberQV(int ActorID)
{
	int i = 0;
	int stop = 0;
	var_Remembered = WindGenVars.Qnominalperphase;
	CalcVterminal(ActorID);
	V_Avg = 0.0;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V_Avg = V_Avg + cabs((Vterminal)[i - 1]);
	}
	V_Avg = V_Avg / Fnphases;
	V_Remembered = V_Avg;
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TWindGenObj::CalcDQDV(int ActorID)
{
	double VDiff = 0.0;
	int i = 0;
	int stop = 0;
	CalcVterminal(ActorID);
	V_Avg = 0.0;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		V_Avg = V_Avg + cabs((Vterminal)[i - 1]);
	}
	V_Avg = V_Avg / Fnphases;
	VDiff = V_Avg - V_Remembered;
	if(VDiff != 0.0)
		DQDV = (WindGenVars.Qnominalperphase - var_Remembered) / VDiff;
	else
		DQDV = 0.0;  // Something strange has occured
                       // this will force a de facto P,Q model
	DQDVSaved = DQDV;  //Save for next time  Allows WindGen to be enabled/disabled during simulation
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TWindGenObj::ResetStartPoint()
{
	WindGenVars.Qnominalperphase = 1000.0 * kvarBase / Fnphases;
}


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

void TWindGenObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int Idx = 0;
	inherited::DumpProperties(f, Complete);
	{ Write(f,"!DQDV="); WriteLn(f, DQDV, 10, 2); }
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			Idx = (with0->PropertyIdxMap)[i - 1];
			switch(Idx)
			{
				case 	34: case 36:
				{ Write(f,"~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f,"=("); Write(f, Get_PropertyValue(Idx)); WriteLn(f, L')'); }
				break;
				case 	44:  // This one has no variable associated, not needed
				{ Write(f,"~ "); Write(f, (with0->PropertyName)[i - 1]); WriteLn(f,"=False"); }
				break;
				default:
				{ Write(f,"~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(Idx)); }
				break;
			}
		}
	}
	WriteLn(f);
}

void TWindGenObj::InitHarmonics(int ActorID)
{
	complex e = {};
	complex Va = {};
	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims
	GenFundamental = ActiveCircuit[ActorID]->Solution->get_FFrequency();  // Whatever the frequency is when we enter here.
	/*# with WindGenVars do */
	{
		auto& with0 = WindGenVars;
		Yeq = cinv(cmplx(0.0, with0.Xdpp));      // used for current calcs  Always L-N

         /*Compute reference Thevinen voltage from phase 1 current*/
		if(GenON)
		{
			ComputeIterminal(ActorID);  // Get present value of current
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with1 = ActiveCircuit[ActorID]->Solution;
				switch(Connection)
				{
					case 	0: /*wye - neutral is explicit*/
					{
						if(!ADiakoptics || (ActorID == 1))
							Va = csub(with1->NodeV[(NodeRef)[1 - 1]], with1->NodeV[(NodeRef)[Fnconds - 1]]);
						else
							Va = csub(with1->VoltInActor1((NodeRef)[1 - 1]), with1->VoltInActor1((NodeRef)[Fnconds - 1]));
					}
					break;  /*delta -- assume neutral is at zero*/
					case 	1:
					{
						if(!ADiakoptics || (ActorID == 1))
							Va = with1->NodeV[(NodeRef)[1 - 1]];
						else
							Va = with1->VoltInActor1((NodeRef)[1 - 1]);
					}
					break;
					default:
					  ;
					break;
				}
			}
			e = csub(Va, cmul((Iterminal)[1 - 1], cmplx(0.0, with0.Xdpp)));
			with0.VThevHarm = cabs(e);   // establish base mag and angle
			with0.ThetaHarm = cang(e);
		}
		else
		{
			with0.VThevHarm = 0.0;
			with0.ThetaHarm = 0.0;
		}
	}
}

void TWindGenObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"3");				//'phases';
	Set_PropertyValue(2,GetBus(1));         //'bus1';
	Set_PropertyValue(3,"12.47");			//kV
	Set_PropertyValue(4,"100");				//kW
	Set_PropertyValue(5,".80");				//PF
	Set_PropertyValue(6,"1");				//model
	Set_PropertyValue(7,"");				//yearly
	Set_PropertyValue(8,"");				//daily
	Set_PropertyValue(9,"");				//duty
	Set_PropertyValue(10,"wye");			//conn
	Set_PropertyValue(11,"60.0");			//kvar
	Set_PropertyValue(12,"100");			//class
	Set_PropertyValue(13,"no");				//debugtrace
	Set_PropertyValue(14,"0.90");			//VMinPu
	Set_PropertyValue(15,"1.1");			//VMaxPu;
    Set_PropertyValue(16, Format("%-g", WindGenVars.kVArating));			//kVA
    Set_PropertyValue(17, Format("%-g", WindGenVars.kVArating * 1e-3));		//MVA
	Set_PropertyValue(18, "");				//UserModel
    Set_PropertyValue(19, "");				//UserData
    Set_PropertyValue(20, "0.0");			//DutyStart
	Set_PropertyValue(21,"");				//DynamicEq
	Set_PropertyValue(22,"");				//DynOut
    Set_PropertyValue(23, Format("%-g", WindModelDyn->Rthev));				//RThev
    Set_PropertyValue(24, Format("%-g", WindModelDyn->Xthev));				//XThev
    Set_PropertyValue(25, Format("%-g", WindModelDyn->Vss));				//Vss
    Set_PropertyValue(26, Format("%-g", WindModelDyn->Pss));				//Pss
    Set_PropertyValue(27, Format("%-g", WindModelDyn->Qss));				//Qss
    Set_PropertyValue(28, Format("%-g", WindModelDyn->vwind));				//VWind
    Set_PropertyValue(29, Format("%-g", WindModelDyn->QMode));				//QMode
    Set_PropertyValue(30, Format("%-g", WindModelDyn->SimMechFlg));			//SimMechFlg
    Set_PropertyValue(31, Format("%-g", WindModelDyn->APCFLG));				//APCFlg
    Set_PropertyValue(32, Format("%-g", WindModelDyn->QFlg));				//QFlg
    Set_PropertyValue(33, Format("%-g", WindModelDyn->delt0));				//delt0
    Set_PropertyValue(34, Format("%-g", WindModelDyn->N_WTG));				//N_WTG
	Set_PropertyValue(35,"");												//VV_Curve
    Set_PropertyValue(36, Format("%-g", WindGenVars.ag));					//Ag
    Set_PropertyValue(37, Format("%-g", WindGenVars.Cp));					//Cp
    Set_PropertyValue(38, Format("%-g", WindGenVars.Lamda));				//Lamda
    Set_PropertyValue(39, Format("%-g", WindGenVars.Poles));				//Poles
    Set_PropertyValue(40, Format("%-g", WindGenVars.pd));					//pd
    Set_PropertyValue(41, WindGenVars.PLoss);								//PLoss
    Set_PropertyValue(42, Format("%-g", WindGenVars.Rad));					//Rad
    Set_PropertyValue(43, Format("%-g", WindGenVars.VCutin));				//VCutIn
    Set_PropertyValue(44, Format("%-g", WindGenVars.VCutout));				//VCutOut
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TWindGenObj::InitStateVars(int ActorID)
{

    /*VNeut,*/
	int i = 0;
	complex V012[4] = { CZero, CZero, CZero, CZero, },
			I012[4] = { CZero, CZero, CZero, CZero, },
			Vabc[4] = { CZero, CZero, CZero, CZero, };
	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims
	/*# with WindGenVars do */
	{
		auto& with0 = WindGenVars;
		switch(GenModel)
		{
			case 	7:
				with0.Zthev = cmplx(with0.Xdp, 0.0);
			break; // use Xd' as an equivalent R for the inverter
			default:
				with0.Zthev = cmplx(with0.Xdp / with0.XRdp, with0.Xdp);
			break;
		}
		Yeq = cinv(with0.Zthev);

     /*Compute nominal Positive sequence voltage behind transient reactance*/
		if(GenON)
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with1 = ActiveCircuit[ActorID]->Solution;
			ComputeIterminal(ActorID);
			switch(Fnphases)
			{
				case 	1:
				{
					if(!ADiakoptics || (ActorID == 1))
						Edp = csub(csub(with1->NodeV[(NodeRef)[1 - 1]], with1->NodeV[(NodeRef)[2 - 1]]), cmul((Iterminal)[1 - 1], with0.Zthev));
					else
						Edp = csub(csub(with1->VoltInActor1((NodeRef)[1 - 1]), with1->VoltInActor1((NodeRef)[2 - 1])), cmul((Iterminal)[1 - 1], with0.Zthev));
					with0.VthevMag = cabs(Edp);
				}
				break;
                // Calculate Edp based on Pos Seq only
				case 	3:
				{
					int stop = 0;
					Phase2SymComp(&(Iterminal[0]), &I012[0]);
                    // Voltage behind Xdp  (transient reactance), volts
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						if(!ADiakoptics || (ActorID == 1))
							Vabc[i] = with1->NodeV[(NodeRef)[i - 1]];
						else
							Vabc[i] = with1->VoltInActor1((NodeRef)[i - 1]);
					}   // Wye Voltage
					Phase2SymComp(&Vabc[1], &V012[0]);
					Edp = csub(V012[1], cmul(I012[1], with0.Zthev));    // Pos sequence
					with0.VthevMag = cabs(Edp);
				}
				break;
				default:
				DoSimpleMsg(Format(("Dynamics mode is implemented only for 1- or 3-phase WindGens. WindGen." + with1->get_Name()
	        +" has %d phases.").c_str(), Fnphases), 5672);
				SolutionAbort = true;
				break;
			}

			if (!ASSIGNED(DynamicEqObj))
			{
                // Shaft variables
				// Theta is angle on Vthev[1] relative to system reference
				// Theta  := Cang(Vthev^[1]);   // Assume source at 0
				with0.Theta = cang(Edp);
                if (GenModel == 7)
					Model7LastAngle = with0.Theta;

				with0.dTheta = 0.0;
				with0.w0 = TwoPi * ActiveCircuit[ActorID]->Solution->get_FFrequency();
                // recalc Mmass and D in case the frequency has changed
                with0.Mmass = 2.0 * with0.Hmass * with0.kVArating * 1000.0 / (with0.w0); // M = W-sec
                with0.D = with0.Dpu * with0.kVArating * 1000.0 / (with0.w0);
                
				with0.Pshaft = -Get_Power(1, ActorID).re; // Initialize Pshaft to present power Output

				with0.Speed = 0.0; // relative to synch speed
				with0.dSpeed = 0.0;

				// Init User-written models
                // Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
				
				auto with4 = ActiveCircuit[ActorID]->Solution;
                
				if (GenModel == 6)
                {
                    if (UserModel->Get_Exists())
                        UserModel->FInit(&(Vterminal[0]), &(Iterminal[0]));
                    if (ShaftModel->Get_Exists())
                        ShaftModel->FInit(&(Vterminal[0]), &(Iterminal[0]));
                }
                else
                    WindModelDyn->Init(&(Vterminal[0]), &(Iterminal[0]));
			}
            else
            {
                // Initializes the memory values for the dynamic equation
				for( i = 0; i < DynamicEqVals.size(); i++) 
					DynamicEqVals[i][1] = 0.0;
				// Check for initial conditions using calculated values (P0, Q0)
				int NumData	= (DynamicEqPair.size() / 2) - 1;
				  
				for (i = 0; i <= NumData; i++)
                {
                    if (DynamicEqObj->IsInitVal(DynamicEqPair[(i * 2) + 1]))
                    {
                        switch (DynamicEqPair[(i * 2) + 1])
                        {
							case 9:
							{
								DynamicEqVals[DynamicEqPair[i * 2]][0] = cang(Edp);

								if (GenModel == 7)
									Model7LastAngle = DynamicEqVals[DynamicEqPair[i * 2]][0];
							}
							break;
                            default:
								DynamicEqVals[DynamicEqPair[i * 2]][0] = Get_PCEValue(1, DynamicEqPair[(i * 2) + 1], ActorID);
                        }
                    }
                }
            }
		}
		else
		{
			Vthev = CZero;
			with0.Theta = 0.0;
			with0.dTheta = 0.0;
			with0.w0 = 0;
			with0.Speed = 0.0;
			with0.dSpeed = 0.0;
		}
	}  /*With*/
}

void TWindGenObj::IntegrateStates(int ActorID)
{
	complex TracePower = CZero;
   // Compute Derivatives and then integrate
	ComputeIterminal(ActorID);

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)
	/*# with ActiveCircuit[ActorID].Solution, WindGenVars do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		auto& with1 = WindGenVars;

		if (!ASSIGNED(DynamicEqObj))
		{
            /*# with DynaVars do */
            {
                auto& with2 = with0->DynaVars;
                if (with2.IterationFlag == 0) /*Get_First() iteration of new time step*/
                {
                    with1.ThetaHistory = with1.Theta + 0.5 * with2.h * with1.dTheta;
                    with1.SpeedHistory = with1.Speed + 0.5 * with2.h * with1.dSpeed;
                }
            }
            // Compute shaft dynamics
            TracePower = TerminalPowerIn(&(Vterminal[0]), &(Iterminal[0]), Fnphases);
            with1.dSpeed = (with1.Pshaft + TracePower.re - with1.D * with1.Speed) / with1.Mmass;
            //      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
            with1.dTheta = with1.Speed;
            // Trapezoidal method
            /*# with DynaVars do */
            {
                auto& with3 = with0->DynaVars;
                with1.Speed = with1.SpeedHistory + 0.5 * with3.h * with1.dSpeed;
                with1.Theta = with1.ThetaHistory + 0.5 * with3.h * with1.dTheta;
            }

            // Write Dynamics Trace Record
            if (DebugTrace)
            {
                Append(Tracefile);
                IOResultToException();
                Write(Tracefile, Format("t=%-.5g ", with0->DynaVars.T));
                Write(Tracefile, Format(" Flag=%d ", with0->DynaVars.IterationFlag));
                Write(Tracefile, Format(" Speed=%-.5g ", with1.Speed));
                Write(Tracefile, Format(" dSpeed=%-.5g ", with1.dSpeed));
                Write(Tracefile, Format(" Pshaft=%-.5g ", with1.Pshaft));
                Write(Tracefile, Format(" P=%-.5g Q= %-.5g", TracePower.re, TracePower.im));
                Write(Tracefile, Format(" M=%-.5g ", with1.Mmass));
                WriteLn(Tracefile);
                CloseFile(Tracefile);
            }

            if (GenModel == 6)
            {
                if (UserModel->Get_Exists())
                    UserModel->Integrate();
                if (ShaftModel->Get_Exists())
                    ShaftModel->Integrate();
            }
            else
            {
                WindModelDyn->Integrate();
            }
		}
        else
        {
            // Dynamics using an external equation
            auto With5 = with0->DynaVars;
            
			if (With5.IterationFlag == 0)
            {
				//{ First iteration of new time step }
				with1.SpeedHistory = DynamicEqVals[DynOut[0]][0] + 0.5 * h * DynamicEqVals[DynOut[0]][1]; // first speed
				with1.ThetaHistory = DynamicEqVals[DynOut[1]][0] + 0.5 * h * DynamicEqVals[DynOut[1]][1]; // then angle
            }
            // Check for initial conditions using calculated values (P, Q, VMag, VAng, IMag, IAng)
			int NumData = round(DynamicEqPair.size() / 2) - 1;
            for (int i = 0; i <= NumData; i++)
            {
                if (!DynamicEqObj->IsInitVal(DynamicEqPair[(i * 2) + 1]))
                { // it's not intialization
                switch( DynamicEqPair[(i * 2) + 1])
                {
                    case 0:
						DynamicEqVals[DynamicEqPair[i * 2]][0] = -TerminalPowerIn(&(Vterminal[0]), &(Iterminal[0]), Fnphases).re;
                    break;
					case	1: 
						DynamicEqVals[DynamicEqPair[i * 2]][0] = -TerminalPowerIn(&(Vterminal[0]), &(Iterminal[0]), Fnphases).im;
					break;
					default:
						DynamicEqVals[DynamicEqPair[i * 2]][0] = Get_PCEValue(1, DynamicEqPair[(i * 2) + 1], ActorID);
                    break;
                }
				}
            }

			// solves the differential equation using the given values
            DynamicEqObj->SolveEq(&DynamicEqVals);
            // Trapezoidal method   - Places the calues in the same vars to keep the code consistent
            with1.Speed = with1.SpeedHistory + 0.5 * h * DynamicEqVals[DynOut[0]][1];
			with1.Theta = with1.ThetaHistory + 0.5 * h * DynamicEqVals[DynOut[1]][1];
            
            // saves the new integration values in memoryspace
            DynamicEqVals[DynOut[0]][0] = with1.Speed;
            DynamicEqVals[DynOut[1]][0] = with1.Theta;
        }
	}
}
/*Return variables one at a time*/

double TWindGenObj::Get_Variable(int i)
{
    double	result = -9999.99;	// error return value
    int		N = 0, 
			k = 0;

    if (i < 1)
        return result; // Someone goofed
    if (i < 19)
        result = WindModelDyn->Get_Variable(i);
    else
    {
        switch (i)
        {
        case 19:
            result = WindGenVars.Pg;
            break;
        case 20:
            result = WindGenVars.Ps;
            break;
        case 21:
            result = WindGenVars.Pr;
            break;
        case 22:
            result = WindGenVars.s;
            break;
        default:
            result = -9999.99;
        }
    }
    if (UserModel->Get_Exists())
    {
        N = UserModel->FNumVars();
        k = (i - NumWGenVariables);
        if (k <= N)
        {
            result = UserModel->FGetVariable(k);
            return result;
        }
    }

    /*If we get here, must be in the Shaft Model if anywhere*/
    if (ShaftModel->Get_Exists())
    {
        k = i - (NumWGenVariables + N);
        if (k > 0)
            result = ShaftModel->FGetVariable(k);
    }
    return result;

}

void TWindGenObj::Set_Variable(int i, double Value)
{
	int n = 0;
	int k = 0;
	n = 0;
	if(i < 1)
		return;  // Someone goofed
	/*# with WindGenVars do */
	{
		auto& with0 = WindGenVars;
		switch(i)
		{
			case 	1:
			with0.Speed = (Value - with0.w0) * TwoPi;
			break;
			case 	2:
			with0.Theta = Value / RadiansToDegrees;
			break; // deg to rad
			case 	3:
			;
			break;// meaningless to set Vd := Value * vbase; // pu to volts
			case 	4:
			with0.Pshaft = Value;
			break;
			case 	5:
			with0.dSpeed = Value / RadiansToDegrees;
			break;
			case 	6:
			with0.dTheta = Value;
			break;
			default:
			if(UserModel->Get_Exists())
			{
				n = UserModel->FNumVars();
				k = (i - NumWGenVariables);
				if(k <= n)
				{
					UserModel->FSetVariable(k, Value);
					return;
				}
			}
         // If we get here, must be in the shaft model
			if(ShaftModel->Get_Exists())
			{
				k = (i - (NumWGenVariables + n));
				if(k > 0)
					ShaftModel->FSetVariable(k, Value);
			}
			break;
		}
	}
}

void TWindGenObj::GetAllVariables(pDoubleArray States)
{
	int i = 0;
	int n = 0;
	int stop = 0;

	if (!ASSIGNED(DynamicEqObj))
    {
		for (stop = NumWGenVariables, i = 1; i <= stop; i++)
		{
			(States)[i - 1] = Get_Variable(i);
		}
    }
    else
    {
        for (stop = DynamicEqObj->get_FNumVars(), i = 1; i <= stop; i++)
        {
            (States)[i - 1] = DynamicEqObj->Get_DynamicEqVal(i - 1, &DynamicEqVals);
        }
    }

	if(UserModel->Get_Exists())
	{
		n = UserModel->FNumVars();
		UserModel->FGetAllVars((pDoubleArray)&(States)[NumWGenVariables + 1 - 1]);
	}
	if(ShaftModel->Get_Exists())
	{
		ShaftModel->FGetAllVars((pDoubleArray)&(States)[NumWGenVariables + 1 + n - 1]);
	}
}

int TWindGenObj::NumVariables()
{
	int result = 0;
	result = NumWGenVariables;
	if(UserModel->Get_Exists())
		result = result + UserModel->FNumVars();
	if(ShaftModel->Get_Exists())
		result = result + ShaftModel->FNumVars();
	return result;
}

//---------------------------------------------------------------------------

bool TWindGenObj::get_FForcedON()
{
	return FForcedON;
}

//---------------------------------------------------------------------------

void TWindGenObj::set_FForcedON(bool myState)
{
	FForcedON = myState;
}

//---------------------------------------------------------------------------

double TWindGenObj::get_PFNominal()
{
	return PFNominal;
}

//---------------------------------------------------------------------------

String TWindGenObj::VariableName(int i)
{
	String result;
	const int BuffSize = 255;
	int n = 0;
	int I2 = 0;
	AnsiChar Buff[256/*# range 0..BuffSize*/];
	PAnsiChar PName = nullptr;
	n = 0;
	if(i < 1)
		return result;  // Someone goofed
	switch(i)
	{
		case 	1:
			result ="userTrip";
		break;
		case 	2:
			result ="wtgTrip";
		break;
		case 	3:
			result ="Pcurtail";
		break;
		case 	4:
			result ="Pcmd";
		break;
		case 	5:
			result ="Pgen";
		break;
		case 	6:
			result ="Qcmd";
		break;
        case	7:
            result = "Qgen";
        break;
        case	8:
            result = "Vref";
        break;
        case	9:
            result = "Vmag";
        break;
        case	10:
            result = "vwind";
        break;
        case	11:
            result = "WtRef";
        break;
        case	12:
            result = "WtAct";
        break;
        case	13:
            result = "dOmg";
        break;
        case	14:
            result = "dFrqPuTest";
        break;
        case	15:
            result = "QMode";
        break;
        case	16:
            result = "Qref";
        break;
        case	17:
            result = "PFref";
        break;
        case	18:
            result = "thetaPitch";
        break;
        case	19:
            result = "Pg";
        break;
        case	20:
            result = "Ps";
        break;
        case	21:
            result = "Pr";
        break;
        case	22:
            result = "s";
        break;
		default:
		if(UserModel->Get_Exists())  // Checks for existence and Selects
		{
			PName = (PAnsiChar) &Buff;
			n = UserModel->FNumVars();
			I2 = i - NumWGenVariables;
			if(I2 <= n)
                 // DLL functions require AnsiString (AnsiString) type
			{
				UserModel->FGetVarName(I2, PName, (unsigned int) BuffSize);
				result = PName;
				return result;
			}
		}
		if(ShaftModel->Get_Exists())
		{
			PName = (PAnsiChar) &Buff;
			I2 = i - NumWGenVariables - n;
			if(I2 > 0)
				UserModel->FGetVarName(I2, PName, (unsigned int) BuffSize);
			result = PName;
		}
		break;
	}
	return result;
}

String TWindGenObj::GetPropertyValue(int Index)
{
	String Result;
	Result ="";
	switch(Index)
	{
        case 3: Result = Format("%.6g", Get_PresentkV());
            break;
        case 4 : Result = Format("%.6g", kWBase);
            break;
        case 5 : Result = Format("%.6g", PFNominal);
            break;
        case 7 : Result = YearlyShape;
            break;
        case 8 : Result = DailyDispShape;
            break;
        case 9 : Result = DutyShape;
            break;
        case 10:
            if (Connection == 0)
                Result = "wye";
            else
                Result = "delta";
            break;
        case 11 : Result = Format("%.6g", Get_Presentkvar());
            break;
        case 12 : Result = Format("%.d", GenClass);
            break;
        case 13:
            if (DebugTrace)
                Result = "Yes";
            else
				Result = "No";
            break;
        case 14 : Result = Format("%.6g", Vminpu);
            break;
        case 15 : Result = Format("%.6g", Vmaxpu);
            break;
        case 16 : Result = Format("%.6g", WindGenVars.kVArating);
            break;
        case 17 : Result = Format("%.6g", (WindGenVars.kVArating / 1e3));
            break;
        case 18:
            Result = UserModel->get_FName();
            break;
        case 20 : Result = Format("%.6g", DutyStart);
            break;
        case 21 : Result = DynamicEq;
            break;
        case 22 : Result = GetDynOutputStr();
            break;
        case 23 : Result = Format("%.6g", WindModelDyn->Rthev);
            break;
        case 24 : Result = Format("%.6g", WindModelDyn->Xthev);
            break;
        case 25 : Result = Format("%.6g", WindModelDyn->Vss);
            break;
        case 26 : Result = Format("%.6g", WindModelDyn->Pss);
            break;
        case 27 : Result = Format("%.6g", WindModelDyn->Qss);
            break;
        case 28 : Result = Format("%.6g", WindModelDyn->vwind);
            break;
        case 29 : Result = Format("%.d", WindModelDyn->QMode);
            break;
        case 30 : Result = Format("%.d", WindModelDyn->SimMechFlg);
            break;
        case 31 : Result = Format("%.d", WindModelDyn->APCFLG);
            break;
        case 32:  Result = Format("%.d", WindModelDyn->QFlg);
            break;
        case 33:  Result = Format("%.6g", WindModelDyn->delt0);
            break;
        case 34:  Result = Format("%.d", WindModelDyn->N_WTG);
            break;
        case 35 : Result = VV_Curve;
            break;
        case 36 : Result = Format("%.6g", WindGenVars.ag);
            break;
        case 37 : Result = Format("%.6g", WindGenVars.Cp);
            break;
        case 38 : Result = Format("%.6g", WindGenVars.Lamda);
            break;
        case 39 : Result = Format("%.6g", WindGenVars.Poles);
            break;
        case 40 : Result = Format("%.6g", WindGenVars.pd);
            break;
        case 41 : Result = WindGenVars.PLoss;
            break;
        case 42 : Result = Format("%.6g", WindGenVars.Rad);
            break;
        case 43 : Result = Format("%.6g", WindGenVars.VCutin);
            break;
        case 44 : Result = Format("%.6g", WindGenVars.VCutout);
            break;
		default:
		Result = inherited::GetPropertyValue(Index);
		break;
	}
	return Result;
}

void TWindGenObj::MakePosSequence(int ActorID)
{
	String s;
	double V = 0.0;
	s ="Phases=1 conn=wye";

  // Make sure voltage is line-neutral
	if((Fnphases > 1) || (Connection != 0))
		V = WindGenVars.kVWindGenBase / SQRT3;
	else
		V = WindGenVars.kVWindGenBase;
	s = s + Format(" kV=%-.5g", V);

  // Divide the load by no. phases
	if(Fnphases > 1)
	{
		s = s + Format(" kW=%-.5g  PF=%-.5g", kWBase / Fnphases, PFNominal);
		if((PrpSequence[19 - 1] != 0) || (PrpSequence[20 - 1] != 0 ))
			s = s
	           + Format(" maxkvar=%-.5g  minkvar=%-.5g", kvarMax / Fnphases, kvarMin / Fnphases);
		if((PrpSequence)[26 - 1] > 0)
			s = s + Format(" kva=%-.5g  ", WindGenVars.kVArating / Fnphases);
		if((PrpSequence)[27 - 1] > 0)
			s = s + Format(" MVA=%-.5g  ", WindGenVars.kVArating / 1000.0 / Fnphases);
	}
	Parser[ActorID]->SetCmdString(s);
	Edit(ActorID);
	inherited::MakePosSequence(ActorID);
}

void TWindGenObj::Set_ConductorClosed(int Index, int ActorID, bool Value)
{
	inherited::Set_ConductorClosed(Index, ActorID, Value);

 // Just turn WindGen on or off;
	if(Value)
		GenSwitchOpen = false;
	else
		GenSwitchOpen = true;
}

void TWindGenObj::Set_PowerFactor(double Value)
{
	PFNominal = Value;
	SyncUpPowerQuantities();
}

void TWindGenObj::Set_PresentkV(double Value)
{
	/*# with WindGenVars do */
	{
		auto& with0 = WindGenVars;
		with0.kVWindGenBase = Value;
		switch(Fnphases)
		{
			case 	2:
			 case 3:
			VBase = with0.kVWindGenBase * InvSQRT3x1000;
			break;
			default:
			VBase = with0.kVWindGenBase * 1000.0;
			break;
		}
	}
}

void TWindGenObj::Set_Presentkvar(double Value)
{
	double kVA_Gen = 0.0;
	kvarBase = Value;
	WindGenVars.Qnominalperphase = 1000.0 * kvarBase / Fnphases; // init to something reasonable
	kVA_Gen = sqrt(Sqr(kWBase) + Sqr(kvarBase));
	if(kVA_Gen != 0.0)
		PFNominal = kWBase / kVA_Gen;
	else
		PFNominal = 1.0;
	if((kWBase * kvarBase) < 0.0)
		PFNominal = -PFNominal;
	kvarMax = 2.0 * kvarBase;
	kvarMin = -kvarMax;
}

void TWindGenObj::Set_PresentkW(double Value)
{
	kWBase = Value;
	SyncUpPowerQuantities();
}

void TWindGenObj::SyncUpPowerQuantities()
{


   // keep kvar nominal up to date with kW and PF
	if(PFNominal != 0.0)
	{
		kvarBase = kWBase * sqrt(1.0L / Sqr(PFNominal) - 1.0L);
		WindGenVars.Qnominalperphase = 1000.0 * kvarBase / Fnphases;
		kvarMax = 2.0 * kvarBase;
		kvarMin = -kvarMax;
		if(PFNominal < 0.0)
			kvarBase = -kvarBase;
		if(kVANotSet)
			WindGenVars.kVArating = kWBase * 1.2;
	}
}

void TWindGenObj::SetDragHandRegister(int reg, double Value)
{
	if(Value > Registers[reg])
		Registers[reg] = Value;
}

void TWindGenObj::SetkWkvar(double PkW, double Qkvar)
{
	kWBase = PkW;
	Set_Presentkvar(Qkvar);
}

void TWindGenObj::CalcVthev_Dyn()
{
	if(GenSwitchOpen)
		WindGenVars.VthevMag = 0.0;
	Vthev = pclx(WindGenVars.VthevMag, WindGenVars.Theta);
}
/*Adjust VThev to be in phase with V, if possible*/
/*
 If the voltage magnitude drops below 15% or so, the accuracy of determining the
 phase angle gets flaky. This algorithm approximates the action of a PLL that will
 hold the last phase angle until the voltage recovers.
*/

void TWindGenObj::CalcVthev_Dyn_Mod7(const complex& V)
{
	double Model7angle = 0.0;
	if(GenSwitchOpen)
		WindGenVars.VthevMag = 0.0;
   /*
      For Phases=1, Vbase is voltage across the terminals.
      Else it is LN voltage.
   */
	if(cabs(V) > 0.2 * VBase)
		Model7angle = cang(V);
	else
		Model7angle = Model7LastAngle;
	Vthev = pclx(WindGenVars.VthevMag, Model7angle);
	Model7LastAngle = Model7angle;
}


void WindGen_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
//   TWOPI3     := twopi/3.0;
}

		class 		WindGen_unit
		{
		public:
		WindGen_unit()
		{
			//AssertSystemInitialization();
			WindGen_initialization();
		}
		};
		WindGen_unit _WindGen_unit;

}  // namespace WindGen




