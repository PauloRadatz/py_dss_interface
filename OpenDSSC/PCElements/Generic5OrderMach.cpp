

#pragma hdrstop

#include "Generic5OrderMach.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Command.h"
#include <math.h>
#include "mathutil.h"
#include "Utilities.h"

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
using namespace GeneratorVars;
using namespace LoadShape;
using namespace PCClass;
using namespace PCElement;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace Generic5OrderMach
{

TGeneric5Obj::TGeneric5Obj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TGeneric5Obj::TGeneric5Obj(String ClassName) : inherited(ClassName) {}
TGeneric5Obj::TGeneric5Obj() {}


TGeneric5* Generic5Class = nullptr;
TGeneric5Obj* ActiveGeneric5Obj = nullptr;
/*Typical Uses Clause -- not all may not be needed*/     // DSS parser
  // Where class is instantiated
    // Global DSS variables
       // If access to circuit variables is needed
       // DSS command and property support module
      // Delphi misc utility functions
          // Delphi Math functions
      // DSS Math utilities
     // DSS misc utility functions
const int NumPropsThisClass = 48;//44;//24;//23; // Set this constant to the actual number of properties you define   add grpnum
const int NumGeneric5Variables = 36;//33;//25;//24;
const int Norder = 6;  // Define any useful module vars here, for example:
complex cBuffer[24/*# range 1..24*/];  // Temp buffer for complex math calcs; allows up to 24-phase models.
complex CDoubleOne = {};   // 1 + j1  (see Initialization section below)

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates main collection handler for all IndMach012 objects

TGeneric5::TGeneric5()
{
	;  // make the base class  and init DSSClassType

     // Specify class name and bit mask ID for this class type
     // IndMach012_ELEMENT must be defined in DSSClassDefs as nn*8
     // First 3 bits are used for base class type (DSSClassType)
	Class_Name = "Generic5";
	DSSClassType = DSSClassType + Generic5OrderMach_ELEMENT;
	ActiveElement = 0;   // no active elements yet; init to 0

     /*Initialize any other special variables here*/
	DefineProperties();   // This is where the properties for this class are defined

     // Use the Command processor to manage property names
     // PropertyName is an array of String defined in DefineProperties
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	Generic5Class = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TGeneric5::~TGeneric5()
{


    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This is where the properties are defined, assigned names, indexes, and help strings
// The Help strings will automatically show up when the Help is invoked

void TGeneric5::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	inherited::CountProperties();   // Get inherited property count
	AllocatePropertyArrays();   /*see DSSClass*/

     // Refer to other classes for alternative methods of assigning properties
     // This example uses the AddProperty function to assign Name, Index, and Help string
     // in one statement.

     // First argument is string name of the property
     // Second argument is the index for the CASE statement
     // Third argument is help string

     // DSS properties are accessed in sequence if the property name is not explicitly specified.
     // The advantage of using the AddProperty function is that you may change the sequence simply
     // by shuffling the order of the definitions and you do not have to change the index in the CASE
     // statement in the EDIT function
	PropertyName[1 - 1] = "phases";
	PropertyName[2 - 1] = "bus1";
	PropertyName[3 - 1] = "kv";
	PropertyName[4 - 1] = "kW";
	PropertyName[5 - 1] = "pf";
	PropertyName[6 - 1] = "conn";
	PropertyName[7 - 1] = "kVA";
	PropertyName[8 - 1] = "H";
	PropertyName[9 - 1] = "D";
	PropertyName[10 - 1] = "P_ref1kW";
	PropertyName[11 - 1] = "P_ref2kW";
	PropertyName[12 - 1] = "P_ref3kW";
	PropertyName[13 - 1] = "V_ref1kVLN";
	PropertyName[14 - 1] = "V_ref2kVLN";
	PropertyName[15 - 1] = "V_ref3kVLN";
	PropertyName[16 - 1] = "MaxSlip";
	PropertyName[17 - 1] = "SlipOption";
	PropertyName[18 - 1] = "Yearly";
	PropertyName[19 - 1] = "Daily";
	PropertyName[20 - 1] = "Duty";
	PropertyName[21 - 1] = "Debugtrace";
	PropertyName[22 - 1] = "P_refkW";
	PropertyName[25 - 1] = "V_refkVLN";
	PropertyName[23 - 1] = "Q_refkVAr";
	PropertyName[24 - 1] = "Cluster_num";
	PropertyName[26 - 1] = "ctrl_mode";
     ///////////////////////////////////////////
     /// contrl mode
     ///    ctrl_mode =0; phases = 3;  // pos avg control---p_ref, V_ref, Q_ref
     ///    ctrl_mode =1; phases = 1; bus1 = 452.1;      ---p_ref1, V_ref1, Q_ref1
     ///    ctrl_mode =2; phases = 1; bus1 = 452.2;      ---p_ref2, V_ref2, Q_ref2
     ///    ctrl_mode =3; phases = 1; bus1 = 452.3;      ---p_ref3, V_ref3, Q_ref3
     ///    ctrl_mode =4; phases = 3; bus1 = 452.2;      ---p_ref1,2,3, V_ref1,2,3, Q_ref1,2,3
	PropertyName[27 - 1] = "QV_flag";
	PropertyName[28 - 1] = "kcd";//Idi control gain
	PropertyName[29 - 1] = "kcq";//Iqi control gain to delta V
	PropertyName[30 - 1] = "kqi";//Iqi control gain to delta Q
	PropertyName[31 - 1] = "Q_ref1kVAr";
	PropertyName[32 - 1] = "Q_ref2kVAr";
	PropertyName[33 - 1] = "Q_ref3kVAr";
	PropertyName[34 - 1] = "PmaxkW"; //
	PropertyName[35 - 1] = "PminkW";
	PropertyName[36 - 1] = "PQpriority";
	PropertyName[37 - 1] = "PmppkW";
	PropertyName[38 - 1] = "Pfctr1";
	PropertyName[39 - 1] = "Pfctr2";
	PropertyName[40 - 1] = "Pfctr3";
	PropertyName[41 - 1] = "Pfctr4";
	PropertyName[42 - 1] = "Pfctr5";
	PropertyName[43 - 1] = "Pfctr6";
	PropertyName[44 - 1] = "PbiaskW";
	PropertyName[45 - 1] = "CC_Switch";
	PropertyName[46 - 1] = "kcq_drp2";
	PropertyName[47 - 1] = "Volt_Trhd";
	PropertyName[48 - 1] = "droop";

     //PropertyName[46] := 'Num_in_Cluster. Num_in_Cluster = 1~33';
	PropertyHelp[1 - 1] = "Number of Phases, this Induction Machine.  ";
	PropertyHelp[2 - 1] = "Bus to which the Induction Machine is connected.  May include specific node specification.";
	PropertyHelp[3 - 1] = "Nominal rated (1.0 per unit) voltage, kV. For 2- and 3-phase machines, specify phase-phase kV. "
	           "Otherwise, specify actual kV across each branch of the machine. "
	           "If wye (star), specify phase-neutral kV. "
	           "If delta or phase-phase connected, specify phase-phase kV.";  // line-neutral voltage//  base voltage
	PropertyHelp[4 - 1] = "Shaft Power, kW, for the Induction Machine. Output limit of a DG";//A positive value denotes power for a //load. ';//+CRLF+
                        //'Negative value denotes an induction generator. ';
	PropertyHelp[5 - 1] = "[Read Only] Present power factor for the machine. ";
	PropertyHelp[6 - 1] = "Connection of stator: Delta or Wye. Default is Delta.";
	PropertyHelp[7 - 1] = "Rated kVA for the machine.";
	PropertyHelp[8 - 1] = "Per unit mass constant of the machine.  MW-sec/MVA.  Default is 1.0.";
	PropertyHelp[9 - 1] = "Damping constant.  Usual range is 0 to 4. Default is 1.0.  Adjust to get damping in Dynamics mode,";
	PropertyHelp[10 - 1] = "P_ref1kW = 10, goes to P_ref1, unit kW, 1st phase set power";
	PropertyHelp[11 - 1] = "P_ref2kW = 10, goes to P_ref2, unit kW, 2nd phase set power";
	PropertyHelp[12 - 1] = "P_ref3kW = 10, goes to P_ref3, unit kW, 3rd phase set power";
	PropertyHelp[13 - 1] = "V_ref1kVLN = 2.16, 1st phase set V, (Unit kV, L-N value): V mode will work if QV_flag =1(by default) V_ref is set which is prior to Q_ref ";
	PropertyHelp[14 - 1] = "V_ref2kVLN = 2.16, 2nd phase set V, (Unit kV, L-N value): V mode will work if QV_flag =1(by default) V_ref is set which is prior to Q_ref ";
	PropertyHelp[15 - 1] = "V_ref3kVLN = 2.16, 3rd phase set V, (Unit kV, L-N value): V mode will work if QV_flag =1(by default) V_ref is set which is prior to Q_ref ";
	PropertyHelp[16 - 1] = "Max slip value to allow. Default is 0.1. Set this before setting slip.";
	PropertyHelp[17 - 1] = "Option for slip model. One of {fixedslip | variableslip*  }";
	PropertyHelp[18 - 1] = "LOADSHAPE object to use for yearly simulations.  Must be previously defined "
	           "as a Loadshape object. Is set to the Daily load shape "
	           " when Daily is defined.  The daily load shape is repeated in this case. "
	           "Set Status=Fixed to ignore Loadshape designation. "
	           "Set to NONE to reset to no loadahape. "
	           "The default is no variation.";
	PropertyHelp[19 - 1] = "LOADSHAPE object to use for daily simulations.  Must be previously defined "
	           "as a Loadshape object of 24 hrs, typically. "
	           "Set Status=Fixed to ignore Loadshape designation. "
	           "Set to NONE to reset to no loadahape. "
	           "Default is no variation (constant) if not defined. "
	           "Side effect: Sets Yearly load shape if not already defined.";
	PropertyHelp[20 - 1] = "LOADSHAPE object to use for duty cycle simulations.  Must be previously defined "
	           "as a Loadshape object.  Typically would have time intervals less than 1 hr. "
	           "Designate the number of points to solve using the Set Number=xxxx command. "
	           "If there are fewer points in the actual shape, the shape is assumed to repeat."
	           "Set to NONE to reset to no loadahape. "
	           "Set Status=Fixed to ignore Loadshape designation. "
	           " Defaults to Daily curve If not specified.";
	PropertyHelp[21 - 1] = "[Yes | No*] Write DebugTrace file.";
	PropertyHelp[22 - 1] = "P_refkW = 10, goes to P_ref. Ref P Value (kW). P_ref has prority to kW which is nomimal value. (Incide variable P_ref is W)";
	PropertyHelp[25 - 1] = "V_refkVLN = 2.16, pos sequence set V. V_ref (Unit kV, L-N value): V mode will work if QV_flag =1(by default) V_ref is set which is prior to Q_ref";
	PropertyHelp[23 - 1] = "Q_refkVAr=10. Unit Qvar. Ref Q kVAr Value: work only when V_ref is not set";
	PropertyHelp[24 - 1] = "Cluster_num: has to be coincident with Fmonitor attached. Default value is 0";
     /* add properties here */
	PropertyHelp[26 - 1] = String("ctrl mode:     /// contrl mode    " " ///    ctrl_mode =0; phases = 3;  // pos avg control---p_ref, V_ref, Q_ref    \\n ") + CRLF
	           + " ///    ctrl_mode =1; phases = 1; bus1 = 452.1;      ---p_ref1, V_ref1, Q_ref1 \\n"
	           + CRLF
	           + "///    ctrl_mode =2; phases = 1; bus1 = 452.2;      ---p_ref2, V_ref2, Q_ref2 \\n"
	           + CRLF
	           + "///    ctrl_mode =3; phases = 1; bus1 = 452.3;      ---p_ref3, V_ref3, Q_ref3 \\n"
	           + CRLF
	           + "///    ctrl_mode =4; phases = 3; bus1 = 452.2;      ---p_ref1,2,3, V_ref1,2,3, Q_ref1,2,3";
	PropertyHelp[27 - 1] = "QV_flag : 0-Q_ref mode; 1- V_ref mode";
	PropertyHelp[28 - 1] = "kcd: Idi control gain";
	PropertyHelp[29 - 1] = "kcq: Iqi control gain to delta V";
	PropertyHelp[30 - 1] = "kqi: Iqi control gain to delta Q";
	PropertyHelp[31 - 1] = "Q_ref1kVAr=10. Unit Qvar. Ref Q kVAr Value: work only when V_ref is not set";
	PropertyHelp[32 - 1] = "Q_ref2kVAr=10. Unit Qvar. Ref Q kVAr Value: work only when V_ref is not set";
	PropertyHelp[33 - 1] = "Q_ref3kVAr=10. Unit Qvar. Ref Q kVAr Value: work only when V_ref is not set";
	PropertyHelp[34 - 1] = String("PmaxkW = 100, goes to Pmax, unit kW, set max active power output; Operation limit of active power for DG") + CRLF
	           + "  Pmax should be less than or equal to kW";
	PropertyHelp[35 - 1] = "PminkW = 10, goes to Pmin, unit kW; Operation limit of active power for DG";
	PropertyHelp[36 - 1] = "PQpriority, goes to PQpriority, define how to set Qmax. 0: Q,1: P ";
	PropertyHelp[37 - 1] = String("Set_PmppkW(100, goes to Pmpp, unit kW, input Get_FPmpp() to calculate kW);") + CRLF
	           + "  kW := (Pmpp + Pbias)*Pfctr1*Pfctr2*Pfctr3*Pfctr4*Pfctr5*Pfctr6;"
	           + CRLF
	           + "Pbias = 0 by default, Pfctr*=1 by default; These properties will overwrite kW.";
	PropertyHelp[38 - 1] = "Pfctr1 = 0.16, see PmppkW";
	PropertyHelp[39 - 1] = "Pfctr2 = 1, 1 by default, see PmppkW";
	PropertyHelp[40 - 1] = "Pfctr3 = 1, 1 by default, see PmppkW";
	PropertyHelp[41 - 1] = "Pfctr4= 1, 1 by default, see PmppkW";
	PropertyHelp[42 - 1] = "Pfctr5 =1, 1 by default, see PmppkW";
	PropertyHelp[43 - 1] = "Pfctr6 = 1, 1 by default, see PmppkW";
	PropertyHelp[44 - 1] = "Pbias = -0.1, 0 by default, see PmppkW";
	PropertyHelp[45 - 1] = String("CC_Switch: default value is false.") + CRLF
	           + "CC_Switch = true --cooperate control on"
	           + CRLF
	           + "CC_Switch = false -- cooperate control off";
	PropertyHelp[46 - 1] = "kcq_drp2. the droop gain: 0.0~0.1";
	PropertyHelp[47 - 1] = "Volt_Trhd. 0.~0.05. 0 means v has to follow v_ref";
	PropertyHelp[48 - 1] = "droop type: integer: 2- Q = kcq_drp2 * (1-v_dg). others: integral droop with kcq.";
     // Finally, we have to pick up any properties that were inherited
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list

     // You can optionally override default help string of an inherited property, for example
	PropertyHelp[NumPropsThisClass + 1 - 1] = "Name of harmonic voltage or current spectrum for this IndMach012. "
	           "Voltage behind Xd\" for machine - default. Current injection for inverter. "
	           "Default value is \"default\", which is defined when the DSS starts.";
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This function is called  by the DSS whenever a New IndMach012... command is encountered

int TGeneric5::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new IndMach012 and add it to IndMach012 class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TGeneric5Obj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This is a typical helper function found in many DSS circuit element class
// for defining the number of conductors per terminal (Nconds) based on Y or delta connection

void TGeneric5::SetNcondsForConnection()
{
	/*# with ActiveGeneric5Obj do */
	{
		auto with0 = ActiveGeneric5Obj;
		switch(with0->Connection)
		{
			case 	0:
			with0->Set_Nconds(with0->Fnphases);
			break;  // Neutral is not connected for induction machine
			case 	1:
			switch(with0->Fnphases)
			{
				case 	1:
				 case 2:        // Delta connection
				with0->Set_Nconds(with0->Fnphases + 1);
				break; // L-L and Open-delta
				default:
				with0->Set_Nconds(with0->Fnphases);    // no neutral for this connection
				break;
			}
			break;
			default:
			  ;
			break;
		}
	}
}


//- - - - - - - - - - - - - MAIN EDIT FUNCTION  - - - - - - - - - - - - - - -
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// This function is the heart of the property managment for this class

int TGeneric5::Edit(int ActorID)
{
	int result = 0;     // Define some local vars for handling parser results
	int ParamPointer = 0;
	String ParamName;
	String Param;

// The Edit function starts where the Parser is presently pointing and
// manages the parsing of the rest of the command line in the parser.

// The DSS executive processes the command verb on the front of the line and
// then passes control to the appropriate Edit function


  // set the present element active
  // and continue parsing with contents of Parser
	ActiveGeneric5Obj = (TGeneric5Obj*) ElementList.Get_Active();
	ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveGeneric5Obj);
	result = 0;
	/*# with ActiveGeneric5Obj do */
	{
		auto with0 = ActiveGeneric5Obj;
     // peel off the next token on the edit line
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while(Param.size() > 0)

         // Find the index for the CASE statement
         // If property is not named, just increment the index to the next property
		{
			if(ParamName.size() == 0)
				++ParamPointer;
			else
				ParamPointer = CommandList.Getcommand(ParamName);

         // Update the PropertyValy for this property
         // Actual index is mapped via PropertyIdxMap array for this class
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))
				with0->Set_PropertyValue((PropertyIdxMap)[ParamPointer - 1],Param);
			else
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Generic5 \""
	           + with0->get_Name()
	           + "\"", 560);

         // --------------- MAIN CASE STATEMENT ----------------------
			if(ParamPointer > 0)
				switch((PropertyIdxMap)[ParamPointer - 1])
				{
         // since we used AddProperty function to define properties, have to
         // use PropertyIdxMap to map to the correct Case index
					case 	0:
					DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 561);
					break;
					case 	1:
					with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
					break; // num phases
					case 	2:
					{
						with0->SetBus(1, Param);      //'bus1 = 8.1.2.3'
                  //if True then
					}
					break;
					case 	3:
					with0->Set_PresentkV(Parser[ActorID]->MakeDouble_());
					break;
					case 	4:
					{
						with0->kWBase = Parser[ActorID]->MakeDouble_();
						if((with0->PMax < 0) || (with0->PMax > with0->kWBase * 1000))
							with0->PMax = with0->kWBase * 1000;
						if(with0->PQpriority == 1)
							with0->PMax = with0->kWBase * 1000;
					}
					break;
					case 	5:
					;
					break; // Do nothing; read only power factor    := Parser.MakeDouble_();
					case 	6:
					InterpretConnection(Parser[ActorID]->MakeString_());
					break;
					case 	7:
					with0->MachineData.kVArating = Parser[ActorID]->MakeDouble_();
					break;
            //8: MachineData.Hmass   := Parser.MakeDouble_();
            //9: MachineData.D       := Parser.MakeDouble_();
					case 	10:
					with0->P_ref1 = 1000 * Parser[ActorID]->MakeDouble_();
					break;  //for phase ctrl unit kW to W
					case 	11:
					with0->P_ref2 = 1000 * Parser[ActorID]->MakeDouble_();
					break;  //for phase ctrl
					case 	12:
					with0->P_ref3 = 1000 * Parser[ActorID]->MakeDouble_();
					break;  //for phase ctrl
					case 	13:
					with0->V_ref1 = 1000 * Parser[ActorID]->MakeDouble_();
					break;  //for phase ctrl unit kV to V
					case 	14:
					with0->V_ref2 = 1000 * Parser[ActorID]->MakeDouble_();
					break;  //for phase ctrl
					case 	15:
					with0->V_ref3 = 1000 * Parser[ActorID]->MakeDouble_();
					break;  //for phase ctrl
					case 	16:
					with0->MaxSlip = Parser[ActorID]->MakeDouble_();
					break;
					case 	17:
					;
					break;//InterpretOption(Parser.MakeString_());
					case 	18:
					with0->YearlyShape = Param;
					break;
					case 	19:
					with0->DailyDispShape = Param;
					break;
					case 	20:
					with0->DutyShape = Param;
					break;
					case 	21:
					with0->DebugTrace = InterpretYesNo(Param);
					break;
              /**/
					case 	22:
					with0->Set_P_Ref(Parser[ActorID]->MakeDouble_(), ActorID);
					break;//to norm value W from kW(in script)//for avg ctrl    1000*PrefKw/3;
					case 	23:
					with0->Set_Q_Ref(Parser[ActorID]->MakeDouble_());
					break;//to VA from kVA(in script)         //for avg ctrl    1000*QrefKVAr/3;
					case 	24:
					with0->Cluster_num = Parser[ActorID]->MakeInteger_();
					break;
					case 	25:
					with0->Set_V_Ref(Parser[ActorID]->MakeDouble_());
					break;//kV  to V                          //for avg ctrl    1000*VrefkV;
					case 	26:
					with0->ctrl_mode = Parser[ActorID]->MakeInteger_();
					break;
                ///////////////////////////////////////////
    ///
    ///////////////////////////////////////////
					case 	27:
					{
						with0->QV_flag = Parser[ActorID]->MakeInteger_();
					}
					break;  // QV_flag_0 :=QV_flag
					case 	28:
					with0->kcd = Parser[ActorID]->MakeDouble_();
					break;
					case 	29:
					with0->kcq = Parser[ActorID]->MakeDouble_();
					break;
					case 	30:
					with0->kqi = Parser[ActorID]->MakeDouble_();
					break;
					case 	31:
					with0->Q_ref1 = 1000 * Parser[ActorID]->MakeDouble_();
					break; //for phase ctrl unit kVar to Var
					case 	32:
					with0->Q_ref2 = 1000 * Parser[ActorID]->MakeDouble_();
					break; //for phase ctrl
					case 	33:
					with0->Q_ref3 = 1000 * Parser[ActorID]->MakeDouble_();
					break; //for phase ctrl
					case 	34:
					{
						with0->PMax = 1000 * Parser[ActorID]->MakeDouble_(); //Pmax has to be less then kW in script
						with0->PMax_phase = with0->PMax / with0->Fnphases;
					}
					break;
					case 	35:
					{
						with0->PMin = 1000 * Parser[ActorID]->MakeDouble_();  //for phase ctrl
						with0->Pmin_phase = with0->PMax / with0->Fnphases;
					}
					break;
					case 	36:
					with0->PQpriority = Parser[ActorID]->MakeInteger_();
					break;  //
					case 	37:
					with0->Pmpp = 1000 * Parser[ActorID]->MakeDouble_();
					break;  //for pmpp kW
					case 	38:
					with0->Pfctr1 = Parser[ActorID]->MakeDouble_();
					break;  //for pmpp
					case 	39:
					with0->Pfctr2 = Parser[ActorID]->MakeDouble_();
					break;  //for pmpp
					case 	40:
					with0->Pfctr3 = Parser[ActorID]->MakeDouble_();
					break; //for pmpp
					case 	41:
					with0->Pfctr4 = Parser[ActorID]->MakeDouble_();
					break; //for pmpp
					case 	42:
					with0->Pfctr5 = Parser[ActorID]->MakeDouble_();
					break; //for  pmpp
					case 	43:
					with0->Pfctr6 = Parser[ActorID]->MakeDouble_();
					break; //for pmpp
					case 	44:
					with0->Pbias = 1000 * Parser[ActorID]->MakeDouble_();
					break; //for pmpp
					case 	45:
					with0->CC_Switch = InterpretYesNo(Parser[ActorID]->MakeString_());
					break; //  yes, true, y ,t; or no, false, n, f
					case 	46:
					with0->kcq_drp2 = Parser[ActorID]->MakeDouble_();
					break; //cluster num
					case 	47:
					with0->Volt_Trhd = Parser[ActorID]->MakeDouble_();
					break;
					case 	48:
					with0->droop = Parser[ActorID]->MakeInteger_();
					break;
           // Handle Inherited properties
					default:
					inherited::ClassEdit(ActiveGeneric5Obj, ParamPointer - NumPropsThisClass);
					break;
				}

         // ---------------- SIDE EFFECTS CASE STATEMENT ---------------------
         // This case statment handles any side effects from setting a property
         // (for example, from Generator)
			if(ParamPointer > 0)
				switch((PropertyIdxMap)[ParamPointer - 1])
				{
					case 	1:
					SetNcondsForConnection();
					break;  // Force Reallocation of terminal info
					case 	18:
					{
						with0->YearlyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->YearlyShape));
						if(ASSIGNED(with0->YearlyShapeObj))
							/*# with YearlyShapeObj do */
							{
								auto with1 = with0->YearlyShapeObj;
								if(with1->UseActual)
									with0->SetPowerkW(with1->MaxP);
							}
					}
					break;
					case 	19:
					{
						with0->DailyDispShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DailyDispShape));
						if(ASSIGNED(with0->DailyDispShapeObj))
							/*# with DailyDispShapeObj do */
							{
								auto with2 = with0->DailyDispShapeObj;
								if(with2->UseActual)
									with0->SetPowerkW(with2->MaxP);
							}
					}
					break;
					case 	20:
					{
						with0->DutyShapeObj = ((TLoadShapeObj*) LoadShapeClass[ActorID]->Find(with0->DutyShape));
						if(ASSIGNED(with0->DutyShapeObj))
							/*# with DutyShapeObj do */
							{
								auto with3 = with0->DutyShapeObj;
								if(with3->UseActual)
									with0->SetPowerkW(with3->MaxP);
							}
					}
					break;
					default:
					  ;
					break;
				}

          //if Get_FPmpp() and fctrs are defined
			if((ParamPointer >= 37) && (ParamPointer <= 44))
			{
				with0->Update_kWbase_by_Fctrs();// Update Pmax
                                    //will cover direct Pmax input by these
			}

         // Get next token off Parser and continue editing properties
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}

     // After editing is complete, the typical next step is to call the RecalcElementData function
		     /*---------------*/
     //if QV_switch = 1 then //
     //begin
          //QV_flag := QV_flag_0;
          //QV_switch := 0;// wait next limit break
     //end;
		with0->Update_PQlimits();
     /*----------------*/
		with0->RecalcElementData(ActorID);
		with0->Set_YprimInvalid(ActorID,true); // Setting this flag notifies the DSS that something has changed
                           // and the Yprim will have to be rebuilt
	}
	return result;
}

//----------------------------------------------------------------------------
// dont use this 0114-2018 by Ying
//----------------------------------------------------------------------------


// This function should be defined to handle the Like property inherited from
// the base class.

// The function copies the essential properties of another object of this class

int TGeneric5::MakeLike(const String OtherIndMach012Name)
{
	int result = 0;
	TGeneric5Obj* OtherIndMach012 = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this IndMach012 name in the present collection*/
	OtherIndMach012 = ((TGeneric5Obj*) Find(OtherIndMach012Name));
	if(OtherIndMach012 != nullptr)
		/*# with ActiveGeneric5Obj do */
		{
			auto with0 = ActiveGeneric5Obj;   // skip if not found

       // You should first set the basic circuit element properties, for example
			int stop = 0;
			if(with0->Fnphases != OtherIndMach012->Fnphases)
			{
				with0->Set_NPhases(OtherIndMach012->Fnphases);
				with0->Set_Nconds(with0->Fnphases);  // Forces reallocation of terminal stuff
				with0->Yorder = with0->Fnconds * with0->Fnterms;
				with0->Set_YprimInvalid(ActiveActor,true);
			}
			with0->Set_PresentkV(OtherIndMach012->Get_PresentkV());
			with0->kWBase = OtherIndMach012->kWBase;
			with0->puRs = OtherIndMach012->puRs;
			with0->puRr = OtherIndMach012->puRr;
			with0->puXr = OtherIndMach012->puXr;
			with0->puXm = OtherIndMach012->puXm;
			with0->puXs = OtherIndMach012->puXs;
			with0->MaxSlip = OtherIndMach012->MaxSlip;
			with0->MachineData.kVArating = OtherIndMach012->MachineData.kVArating;
			with0->MachineData.Hmass = OtherIndMach012->MachineData.Hmass;
			with0->MachineData.D = OtherIndMach012->MachineData.D;

       // Do inherited properties
			ClassMakeLike(OtherIndMach012);

       // Finally initialize all the property value strings to be the same as
       // the copied element
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				(with0->FPropertyValue)[i - 1] = (OtherIndMach012->FPropertyValue)[i - 1];
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Load MakeLike: \"") + OtherIndMach012Name
	           + "\" Not Found.", 562);
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// Optional function if you want to do anything to initialize objects of this class

int TGeneric5::Init(int Handle, int ActorID)
{
	int result = 0;
	TGeneric5Obj* P = nullptr;
	if(Handle == 0)  // init all
	{
		P = (TGeneric5Obj*) ElementList.Get_First();
		while((P != nullptr))
		{
			P->Randomize(0);
			P = (TGeneric5Obj*) ElementList.Get_Next();
		}
	}
	else
	{
		Set_Active(Handle);
		P = ((TGeneric5Obj*) GetActiveObj());
		P->Randomize(0);
	}
	DoSimpleMsg("Need to implement TGeneric5.Init", -1);
	result = 0;
	return result;
}

//------------------------- MAIN OBJECT CONSTRUCTOR ---------------------

TGeneric5Obj::TGeneric5Obj(TDSSClass* ParClass, const String Generic5ObjName)
 : inherited(ParClass),
			Connection(1),
			puRs(0.0),
			puXs(0.0),
			puRr(0.0),
			puXr(0.0),
			puXm(0.0),
			MaxSlip(0.0),
			Xopen(0.0),
			Xp(0.0),
			T0p(0.0),
			NumOrderX(0),
			NumOrderY(0),
			X_var(nullptr),
			dX_vardt(nullptr),
			X_varn(nullptr),
			dX_vardtn(nullptr),
			Y_out_var(nullptr),
			V_in_var(nullptr),
			pV_f_CC(nullptr),
			CC_Switch(false),
			Amm(nullptr),
			Bmn(nullptr),
			Cnm(nullptr),
			Dnn(nullptr),
			InDynamics(false),
			id(0.0),
			Iq(0.0),
			flag_dyna_Id_chg(false),
			dIddt(0.0),
			dIqdt(0.0),
			Idn(0.0),
			Iqn(0.0),
			dIddtn(0.0),
			dIqdtn(0.0),
			kcd(0.0),
			kcq(0.0),
			kcq_drp2(0.0),
			Volt_Trhd(0.0),
			droop(0),
			kqi(0.0),
			vi1(0.0),
			vi2(0.0),
			vi1n(0.0),
			vi2n(0.0),
			dvi1dt(0.0),
			dvi2dt(0.0),
			dvi1dtn(0.0),
			dvi2dtn(0.0),
			Id_ref(0.0),
			Iq_ref(0.0),
			P_ref(0.0),
			Q_ref(0.0),
			V_ref(0.0),
			DPX(0.0),
			ctrl_mode(0),
			P_DG(0.0),
			Q_DG(0.0),
			V_DG(0.0),
			Theta_DG(0.0),
			QV_flag(0),
			P_DG1(0.0),
			P_DG2(0.0),
			P_dg3(0.0),
			Q_DG1(0.0),
			Q_dg2(0.0),
			Q_dg3(0.0),
			V_DG1(0.0),
			V_DG2(0.0),
			V_DG3(0.0),
			V_theta1(0.0),
			V_theta2(0.0),
			V_theta3(0.0),
			ID1(0.0),
			Iq1(0.0),
			ID2(0.0),
			Iq2(0.0),
			Id3(0.0),
			Iq3(0.0),
			P_ref1(0.0),
			P_ref2(0.0),
			P_ref3(0.0),
			Q_ref1(0.0),
			Q_ref2(0.0),
			Q_ref3(0.0),
			V_ref1(0.0),
			V_ref2(0.0),
			V_ref3(0.0),
			PMax(0.0),
			PMax_phase(0.0),
			PMin(0.0),
			Pmin_phase(0.0),
			Qmax(0.0),
			Qmax_phase(0.0),
			Qmin(0.0),
			Qmin_phase(0.0),
			IdMax_phase(0.0),
			IqMax_phase(0.0),
			PQpriority(0),
			Freq(0.0),
			z_dfs_plot(0.0),
			FirstIteration(false),
			FixedSlip(false),
			DQDV(0.0),
			RandomMult(0.0),
			Generic5SwitchOpen(false),
			DebugTrace(false),
			MachineON(false),
			ShapeIsActual(false),
			VBase(0.0),
			kWBase(0.0),
			Pmpp(0.0),
			Pbias(0.0),
			Pfctr1(0.0),
			Pfctr2(0.0),
			Pfctr3(0.0),
			Pfctr4(0.0),
			Pfctr5(0.0),
			Pfctr6(0.0),
			Alpha(0.0),
			dAlpha(0.0),
			Gradient(0.0),
			Alpha1(0.0),
			Alpha2(0.0),
			Alpha3(0.0),
			dAlpha1(0.0),
			dAlpha2(0.0),
			dAlpha3(0.0),
			Gradient1(0.0),
			Gradient2(0.0),
			Gradient3(0.0),
			AlphaP(0.0),
			AlphaP1(0.0),
			AlphaP2(0.0),
			AlphaP3(0.0),
			GradientP(0.0),
			GradientP1(0.0),
			GradientP2(0.0),
			GradientP3(0.0),
			DailyDispShapeObj(nullptr),
			DutyShapeObj(nullptr),
			YearlyShapeObj(nullptr)
{
	int i = 0;
	int j = 0;
//----------------------------------------------------------------------------
	int stop = 0;
	Set_Name(LowerCase(Generic5ObjName));
	DSSObjType = ParClass->DSSClassType; // Same as Parent Class

     // Set some basic circuit element properties
	Set_NPhases(3);  // typical DSS default for a circuit element
	Fnconds = 3;  // defaults to delta
	Yorder = 0;  // To trigger an initial allocation
	Set_NTerms(1);  // forces allocations of terminal quantities
	kWBase = (double) -1;//00; // has to be set in DSS scripts
	YearlyShape = "";
	YearlyShapeObj = nullptr;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
	DailyDispShape = "";
	DailyDispShapeObj = nullptr;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
	DutyShape = "";
	DutyShapeObj = nullptr;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
	DebugTrace = false;
	FMonObj = nullptr;
	Yorder = Fnterms * Fnconds;
	ShapeIsActual = false;
	Generic5SwitchOpen = false;
	MachineData.kVGeneratorBase = 12.47;
	MachineData.kVArating = kWBase * 1.2;
	/*# with MachineData do */
	{
		auto& with0 = MachineData;
		with0.Hmass = 1.0;       //  W-sec/VA rating
		with0.Theta = 0.0;
		with0.w0 = TwoPi * BaseFrequency;
		with0.Speed = 0.0;  // relative speed
		with0.dSpeed = 0.0;
		with0.D = 1.0;
		with0.XRdp = 20.0;   // not used for indmach

           // newly added
		with0.Conn = Connection;
		with0.NumPhases = Fnphases;
		with0.NumConductors = Fnconds;
	}

    /*---- end note Andres: from dll model ----*/

    /*Typical machine impedance data*/
	puRs = 0.0053;
	puXs = 0.106;
	puRr = 0.007;
	puXr = 0.12;
	puXm = 4.0;

      // Set slip local and make generator model agree
	MaxSlip = 0.1;  // 10% slip limit     - set this before setting slip
      //Slip := -0.007;   // About 1 pu power
	FixedSlip = false;  // Allow Slip to float to match specified power
	InDynamics = false;

     // call the procedure to set the initial property string values
	InitPropertyValues(0);
      //NumOrder := 2;
	NumOrderX = Norder;//2;// //  system order
	NumOrderY = Norder; //2;////  system output Y order
      /*A,B,C,D, X_var, Y_out_var, V_in_var matrix*/
	Amm = new double[Norder * Norder];// dot X = Ax +Bu
	Bmn = new double[Norder * Norder];// suppose Y and U have the same dimesion. Square
	Cnm = new double[Norder * Norder];
	Dnn = new double[Norder * Norder];
	X_var = new double[Norder];
	dX_vardt = new double[Norder];
	X_varn = new double[Norder];   // for trapezoid integration
	dX_vardtn = new double[Norder];  // for trapezoid integration
	Y_out_var = new double[Norder];
	V_in_var = new double[Norder];
	pV_f_CC = new double[Norder];
    //Allocate ABCDXYV
    /*A,B matrix, X_var*/  //5 order system
	for(stop = Norder, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Norder, j = 1; j <= stop1; j++)
		{
			(Amm)[(i - 1) * Norder + j - 1] = 0.0;//CMPLX(0.0, 0.0);
              //if j=i then
              //     Amm^[(i-1)*nOrder +j] := 1;//Amm := 0;
		}
		for(stop1 = Norder, j = 1; j <= stop1; j++)
		{
			(Bmn)[(i - 1) * Norder + j - 1] = 0.0;//CMPLX(0.0, 0.0);
			if(j == i)
				(Bmn)[(i - 1) * Norder + j - 1] = 1;
		}
		(X_var)[i - 1] = 0.0;//CMPLX(0.0, 0.0);
		(dX_vardt)[i - 1] = 0.0;// derivatives
		(X_varn)[i - 1] = 0.0;// for trapezoid
		(dX_vardtn)[i - 1] = 0.0;// derivatives
	}
    /*C,D Matrix, Y, V*/
	for(stop = Norder, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Norder, j = 1; j <= stop1; j++)
		{
			(Dnn)[(i - 1) * Norder + j - 1] = 0.0;//CMPLX(0.0, 0.0);
		}
		for(stop1 = Norder, j = 1; j <= stop1; j++)
		{
			(Cnm)[(i - 1) * Norder + j - 1] = 0.0;//CMPLX(0.0, 0.0);
			if(i == j)
				(Cnm)[(i - 1) * Norder + j - 1] = 1;
		}
		(Y_out_var)[i - 1] = 0.0;//CMPLX(0.0, 0.0);
		(V_in_var)[i - 1] = 0.0;//CMPLX(0.0, 0.0);
		(pV_f_CC)[i - 1] = 0.0;
	}
	P_ref = 0;
	V_ref = 1;
	id = 0;
	Iq = 0;//default current
	ID1 = 0;
	Iq1 = 0;
	ID2 = 0;
	Iq2 = 0;
	Id3 = 0;
	Iq3 = 0;//
     /**/
	kcd = 0.1;
	kcq = 0.1;
	kqi = 0.1; //for local control gain in vi1, vi2
	Volt_Trhd = 0.0;
     //Id_ref := 1;
     //Iq_ref := 0;
	Cluster_num = 0;//by default.
     // Update anything that has to be calculated from property values
	DQDV = 1;//
	ctrl_mode = 0;// avg contrl by default
	QV_flag = 0;
     //QV_flag_0 := QV_flag;
     /*------------------*/
     //PQ max
	PMax = (double) -1; // Activity power output limit  MachineData.kVArating :=  1.2*kWbase; Pmax, Smax
	PMax_phase = PMax / Fnphases; //limit per phase
	PMin = 0;  //(0, default)
	Pmin_phase = PMin / Fnphases; //
	Qmax = 1000 * MachineData.kVArating; //Reactive power output limit
	Qmax_phase = Qmax / Fnphases;
	Qmin = -Qmax; //(-Qmax, default)
	Qmin_phase = Qmin / Fnphases; //
	PQpriority = 1;//P priority
	Pmpp = 1;//Pmpp, default value is 1.0);
	Pbias = 0; //Pbias, default value is 0.0;
	Pfctr1 = 1;//factors, default value all are 1.0;
	Pfctr2 = 1;
	Pfctr3 = 1;
	Pfctr4 = 1;
	Pfctr5 = 1;
	Pfctr6 = 1;
     /*------------------*/
	kcq_drp2 = 0;
	CC_Switch = false;
	flag_dyna_Id_chg = false;
	z_dfs_plot = 0.0;
	RecalcElementData(ActiveActor);
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// Free everything here that needs to be freed
// If you allocated anything, dispose of it here

TGeneric5Obj::~TGeneric5Obj()
{


    //A, B matrix
	if(ASSIGNED(Amm))
		delete[] Amm;
	if(ASSIGNED(Bmn))
		delete[] Bmn;
	if(ASSIGNED(Cnm))
		delete[] Cnm;
	if(ASSIGNED(Dnn))
		delete[] Dnn;
	if(ASSIGNED(X_var))
		delete[] X_var;
	if(ASSIGNED(dX_vardt))
		delete[] dX_vardt;
	if(ASSIGNED(X_varn))
		delete[] X_varn;
	if(ASSIGNED(dX_vardtn))
		delete[] dX_vardtn;
	if(ASSIGNED(Y_out_var))
		delete[] Y_out_var;
	if(ASSIGNED(V_in_var))
		delete[] V_in_var;
	delete[] pV_f_CC;
	// inherited::Destroy();   // This will take care of most common circuit element arrays, etc.
}



//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TGeneric5Obj::RecalcElementData(int ActorID)
{
	double Rs = 0.0;
	double Xs = 0.0;
	double rr = 0.0;
	double Xr = 0.0;
	double XM = 0.0;
	double ZBase = 0.0;
	bool modetest = false;
	int numPhase = 0;
	int dotpos = 0;
	String StrTemp;
	/*# with MachineData do */
	{
		auto& with0 = MachineData;
		ZBase = Sqr(with0.kVGeneratorBase) / with0.kVArating * 1000.0;
		with0.Conn = Connection;
		with0.NumPhases = Fnphases;
		with0.NumConductors = Fnconds;
	}
	Rs = puRs * ZBase;
	Xs = puXs * ZBase;
	rr = puRr * ZBase;
	Xr = puXr * ZBase;
	XM = puXm * ZBase;
	Zs = cmplx(Rs, Xs);
	ZM = cmplx(0.0, XM);
	Zr = cmplx(rr, Xr);
	Xopen = Xs + XM;
	Xp = Xs + (Xr * XM) / (Xr + XM);
	Zsp = cmplx(Rs, Xp);
    //Yeq := Cinv(Zsp);   // for Yprim  for dynamics
    //Yeq := Cmplx(1.0/ZBase, -0.5/Zbase);   // vars are half the watts
	Yeq = cmplx(0.0, -1.0 / ZBase);   // vars only for power flow
	T0p = (Xr + XM) / (MachineData.w0 * rr);

 //   dSdP := Compute_dSdP;
	Is1 = CZero;
	V1 = CZero;
	Is2 = CZero;
	V2 = CZero;
	FirstIteration = true;
	InjCurrent = (pComplexArray) realloc(InjCurrent, sizeof(complex) * Yorder);
	SetNominalPower(ActorID);
    ///////////////////////////////////////////
    /// contrl mode
    ///    ctrl_mode =0; phases = 3;  // pos avg control---p_ref, V_ref, Q_ref
    ///    ctrl_mode =1; phases = 1; bus1 = 452.1;      ---p_ref1, V_ref1, Q_ref1
    ///    ctrl_mode =2; phases = 1; bus1 = 452.2;      ---p_ref2, V_ref2, Q_ref2
    ///    ctrl_mode =3; phases = 1; bus1 = 452.3;      ---p_ref3, V_ref3, Q_ref3
    ///    ctrl_mode =4; phases = 3; bus1 = 452.2;      ---p_ref1,2,3, V_ref1,2,3, Q_ref1,2,3
    ///
    ///////////////////////////////////////////
	modetest = false;
	if(((ctrl_mode == 0) || (ctrl_mode == 4)) && (Fnphases == 3))
		modetest = true;
	else
	{
		if(Fnphases == 1)
		{
			if((ctrl_mode == 1) || (ctrl_mode == 2) || (ctrl_mode == 3))
			{
				StrTemp = Get_FirstBus();  //only one
				dotpos = Pos(".", StrTemp);
				if(dotpos != 0)
				{
					numPhase = Sysutils::StrToInt(Trim(StrTemp.substr(dotpos, 1))); // Bus Name . node Num
					if(numPhase == ctrl_mode)
						modetest = true;
				}
			}
		}
	}
	if(modetest == false)
		DoSimpleMsg("ctrl_mode and bus node connection dont match, see help for generic5.ctrl_mode", 561);
    //////////////////////////////////////////////
    //if cluster_num >= 1 then      // assign the virtue leader to this DG
     //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num);
     //if function 'get' fails , return nil
    //////////////////////////////////////////////
	if(CompareText(YearlyShape, "none") == 0)
		YearlyShape = "";
	if(CompareText(DailyDispShape, "none") == 0)
		DailyDispShape = "";
	if(CompareText(DutyShape, "none") == 0)
		DutyShape = "";
	if(YearlyShapeObj == nullptr)
	{
		if(YearlyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Yearly load shape: \"") + YearlyShape
	           + "\" Not Found.", 563);
	}
	if(DailyDispShapeObj == nullptr)
	{
		if(DailyDispShape.size() > 0)
			DoSimpleMsg(String("WARNING! Daily load shape: \"") + DailyDispShape
	           + "\" Not Found.", 564);
	}
	if(DutyShapeObj == nullptr)
	{
		if(DutyShape.size() > 0)
			DoSimpleMsg(String("WARNING! Duty load shape: \"") + DutyShape
	           + "\" Not Found.", 565);
	}
	SpectrumObj = ((TSpectrumObj*) SpectrumClass[ActorID]->Find(Spectrum));
	if(SpectrumObj == nullptr)
		DoSimpleMsg(String("ERROR! Spectrum \"") + Spectrum + "\" Not Found.", 566);
	if(DebugTrace)
		InitTraceFile();
}
 /*
Procedure TGeneric5Obj.CalcABMatrix  ;
//var
//      i,j     :Integer;
begin
       //this is useless. All things is to be done in updateabcd
end;


procedure TGeneric5Obj.InterpretOption(s: String);
begin
     Case Uppercase(s)[1] of
       'F': Fixedslip := TRUE;
       'V': Fixedslip := FALSE;
     Else

     End;
end;

//---------------------------------------------------------------------------- */

void TGeneric5Obj::SetPowerkW(double PkW)
{
	kWBase = PkW;
}

void TGeneric5Obj::Set_PresentkV(double Value)
{
	/*# with MachineData do */
	{
		auto& with0 = MachineData;
		with0.kVGeneratorBase = Value;
		switch(Fnphases)
		{
			case 	2:
			 case 3:
			VBase = with0.kVGeneratorBase * InvSQRT3x1000;
			break;
			default:
			VBase = with0.kVGeneratorBase * 1000.0;
			break;
		}
	}
}

double TGeneric5Obj::Get_PresentkV()
{
	double result = 0.0;
	result = MachineData.kVGeneratorBase;
	return result;
}

//----------------------------------------------------------------------------
//--------------------- MAIN CALC ROUTINES -----------------------------------

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TGeneric5Obj::Integrate(int ActorID)
{
	double H2 = 0.0;
	if(ctrl_mode == 0)
	{
		/*# with ActiveCircuit[ActorID].Solution.DynaVars do */
		{
			auto& with0 = ActiveCircuit[ActorID]->Solution->DynaVars;
			if(with0.IterationFlag == 0)  // on predictor step
			{
				Idn = id;
				Iqn = Iq;
				dIddtn = dIddt;
				dIqdtn = dIqdt;
				vi1n = vi1;
				vi2n = vi2;
				dvi1dtn = dvi1dt;
				dvi2dtn = dvi2dt;
			}
			update_system_abcd(); //vi1, vi2 calculation
			H2 = with0.h * 0.5;
			Id_ref = kcd * P_ref / V_DG;//active
			Iq_ref = Q_DG / V_DG; //V_ref ~= 1;  reactive
          //vi1 :=    kcq* (Iq_ref - Iq);
			DPX = P_ref - P_DG;
			vi1 = 1 * kcd * DPX / V_DG;//+ kcq* (Iq_ref - Iq);//+ //active + additional control
          //vi2 := kcd* (Id_ref - Id); //reactive
			dvi1dt = kcd * (DPX);
          //dvi2dt :=  -kcd* (V_ref-V_DG) ;    //voltage droop control
			if(QV_flag == 1)     //voltage droop control
				vi2 = kcq * (V_ref - V_DG);
			else
				vi2 = kqi * (Q_ref - Q_DG);
          /*--------------------------------------------*/
			dIddt = vi1;//1; //active P
			dIqdt = vi2;//2; //reactive Q
			id = Idn + H2 * (dIddt + dIddtn);
			Iq = Iqn + H2 * (dIqdt + dIqdtn);
		}
	}
}
/*-------------------------------------------------------------------------------*/
/*integrate with ABCD*/
//----------------------------------------------------------------------------

void TGeneric5Obj::IntegrateABCD(int ActorID)
{
	double H2 = 0.0;
	int i = 0;
	int j = 0;
	int stop = 0;
	if(ActiveCircuit[ActorID]->Solution->DynaVars.IterationFlag == 0)
	{
		int stop = 0;
		for(stop = NumOrderX, i = 1; i <= stop; i++)
		{
			(X_varn)[i - 1] = (X_var)[i - 1];
			(dX_vardtn)[i - 1] = (dX_vardt)[i - 1];
		}
	}
	update_system_abcd(); //Matrix ABCD calculation if they are state-dependant
	update_controlinput(ActorID); //vi1, vi2 calculation,
                                // co control strategies from network vfi can be done here
            //dX_vardt calculation
	for(stop = NumOrderX, i = 1; i <= stop; i++)
	{ //  numOrderX, numOrderY should be less than norder 5
		int stop1 = 0;
		(dX_vardt)[i - 1] = 0.0;
		for(stop1 = NumOrderY, j = 1; j <= stop1; j++)
		{
			(dX_vardt)[i - 1] = (dX_vardt)[i - 1] + (Amm)[Norder * (i - 1) + j - 1] * (X_var)[j - 1] + (Bmn)[Norder * (i - 1) + j - 1] * (V_in_var)[j - 1];
                                          //cooperate control if exist is involved in
		}
	}

            // Trapezoidal Integration
	H2 = ActiveCircuit[ActorID]->Solution->DynaVars.h * 0.5;
	for(stop = NumOrderX, i = 1; i <= stop; i++)
	{
		(X_var)[i - 1] = (X_varn)[i - 1] + H2 * ((dX_vardtn)[i - 1] + (dX_vardt)[i - 1]);
	}
            /*----------------*/
            //Y=CX to be added
	            /*----------------*/
         ///
         ///  the following is to connect with calcDynamic or CalcDynamicVIabc
         ///  because Id, Iq; Id1, Iq1, Id2, Iq2, Id3, Iq3 will be used there
	if(ctrl_mode == 0) //pos seq control
	{
		id = (X_var)[1 - 1];//can be put in calcdyna, so the integrate is just for X_var
		Iq = (X_var)[2 - 1];
	}
	else
   // all other ctrl_mode's are phase control modes
	{
		ID1 = (X_var)[1 - 1];//1st phase, or the only phase if fnphases=1
		Iq1 = (X_var)[2 - 1];//can be put in calcdyna in futher, so the integrate is just for X_var
		ID2 = (X_var)[3 - 1];//2nd phase; zero if single phase
		Iq2 = (X_var)[4 - 1];
		Id3 = (X_var)[5 - 1];//3rd phase; zero if single phase
		Iq3 = (X_var)[6 - 1];
	}
}
/*-------------------------------------------------------------------------------*/
//This part deals with the control input,  is based on the voltage measurement

void TGeneric5Obj::update_controlinput(int ActorID)
{
	int j = 0;
	double temp_pref = 0.0;
	double temp_qref = 0.0;
	double temp_vref = 0.0;
	double Pref3 = 0.0;
    //Update_PQlimits;
    ///////////////////////////////////
    //local control input and alpha gradient calculation
    ///////////////////////////////////
    //gradient, gradient1, gradient2, gradient3
    //   gradient will be calculated in FMonitor because of Bii, Q_Di etc
    //calculate_gradient; //alpha, and gradients，
                        //V_DG, Q_DG have been updated either in 'init' or in 'calcdynamic'
	int stop = 0;
	if(ctrl_mode == 0) //pos seq control mode

        //Id
        // P and Q control
		        /*
        //Iq
        if QV_flag=1 then
          vi2 := kcq* (V_ref - V_DG) //reactive V_ref control
        else
          vi2 := kqi* (Q_ref - Q_DG); //reactive Q_ref control
        */
        // vi1, vi2 local gradient
        //  vi1, vi2 =0, local gradient calculated outside
		{
			vi1 = 0;
			vi2 = 0;    // local gradient calculated IN fMONITOR Node
        /*---if in curtailment P_ref has to be changed here-----*/
			if((ActiveCircuit[ActorID]->Solution->bCurtl == true) && (FMonObj->ld_fm_info[0].b_ctrl_hghst == true))
                        //if (ActiveCircuit.Solution.bCurtl=true) then // this will cause oscillation
			{
				Pref3 = V_DG * id; //Here, P_ref will never go out of limits.
                                        //if cuitailment is needed, update P_ref here; then vi1 will be 0
                 //check limits
				if(Pref3 > PMax)
				{
					Pref3 = PMax; //set real power change during the simulation
				}
				else
				{
					if(Pref3 < PMin)
					{
						Pref3 = PMin;
					}
				}
				P_ref = Pref3 / 3.0;
			}
        /*--use vi1 to follow p_ref--*/
			DPX = Fnphases * P_ref - P_DG;
			vi1 = 100 * kcd * DPX / V_DG;  //pref control is 100 times faster than Curtailment
        //update V_in_var
			(V_in_var)[1 - 1] = vi1;
			(V_in_var)[2 - 1] = vi2;
		}
	else
 //phases control mode
	{
		if(Fnphases == 3)
        //12
		{
			DPX = P_ref1 - P_DG1;
			vi1 = kcd * DPX / V_DG1;
              //Iq
			if(QV_flag == 1)
			{
				if(CC_Switch == false)    //droop
 //reactive V_ref control
                  //gradient
					vi2 = kcq * (V_ref1 - V_DG1);
				else
					vi2 = Qmax_phase / V_DG1 * (-kcq * Gradient1);
				if((Q_DG1 >= Qmax_phase) || (Q_DG1 <= Qmin_phase))  // switch control mode to Q_ref control

                    //QV_flag := 0;
				{
					if(Q_DG1 >= Qmax_phase)
					{
						Q_ref1 = Qmax_phase; // set Q_ref
						Q_ref2 = Qmax_phase; // set Q_ref
						Q_ref3 = Qmax_phase; // set Q_ref
					}
					else
					{
						Q_ref1 = Qmin_phase;
						Q_ref2 = Qmin_phase;
						Q_ref3 = Qmin_phase;
					}
					vi2 = kqi * (Q_ref1 - Q_DG1); //reactive Q_ref control
				}
			}
			else
			vi2 = kqi * (Q_ref1 - Q_DG1); //reactive Q_ref control
              //update V_in_var
			(V_in_var)[1 - 1] = vi1;
			(V_in_var)[2 - 1] = vi2;
        //34
			DPX = P_ref2 - P_DG2;
			vi1 = kcd * DPX / V_DG2;
              //Iq
			if(QV_flag == 1)
			{
				if(CC_Switch == false)    //droop
 //reactive V_ref control
                  //gradient
					vi2 = kcq * (V_ref2 - V_DG2);
				else
					vi2 = Qmax_phase / V_DG2 * (-kcq * Gradient2);
				if((Q_dg2 >= Qmax_phase) || (Q_dg2 <= Qmin_phase))  // switch control mode to Q_ref control

                    //QV_flag:=0;
				{
					if(Q_dg2 >= Qmax_phase)
					{
						Q_ref1 = Qmax_phase; // set Q_ref
						Q_ref2 = Qmax_phase; // set Q_ref
						Q_ref3 = Qmax_phase; // set Q_ref
					}
					else
					{
						Q_ref1 = Qmin_phase;
						Q_ref2 = Qmin_phase;
						Q_ref3 = Qmin_phase;
					}
					vi2 = kqi * (Q_ref2 - Q_dg2); //reactive Q_ref control
				}
			}
			else
			vi2 = kqi * (Q_ref2 - Q_dg2); //reactive Q_ref control
              //update V_in_var
			(V_in_var)[3 - 1] = vi1;
			(V_in_var)[4 - 1] = vi2;
        //56
			DPX = P_ref3 - P_dg3;
			vi1 = kcd * DPX / V_DG3;
              //Iq
			if(QV_flag == 1)
			{
				if(CC_Switch == false)    //droop
 //reactive V_ref control
                  //gradient
					vi2 = kcq * (V_ref3 - V_DG3);
				else
					vi2 = Qmax_phase / V_DG3 * (-kcq * Gradient3);
				if((Q_dg3 >= Qmax_phase) || (Q_dg3 <= Qmin_phase))  // switch control mode to Q_ref control

                    //QV_flag := 0;
				{
					if(Q_dg3 >= Qmax_phase)
					{
						Q_ref1 = Qmax_phase; // set Q_ref
						Q_ref2 = Qmax_phase; // set Q_ref
						Q_ref3 = Qmax_phase; // set Q_ref
					}
					else
					{
						Q_ref1 = Qmin_phase;
						Q_ref2 = Qmin_phase;
						Q_ref3 = Qmin_phase;
					}
					vi2 = kqi * (Q_ref3 - Q_dg3); //reactive Q_ref control
				}
			}
			else
			vi2 = kqi * (Q_ref3 - Q_dg3); //reactive Q_ref control
              //update V_in_var
			(V_in_var)[5 - 1] = vi1;
			(V_in_var)[6 - 1] = vi2;
		}
		else
		{
			if(Fnphases == 1)
              //choose ref
			{
				switch(ctrl_mode)
				{
					case 	1:
					{
						temp_pref = P_ref1;
						temp_qref = Q_ref1;
						temp_vref = V_ref1;
					}
					break;
					case 	2:
					{
						temp_pref = P_ref2;
						temp_qref = Q_ref2;
						temp_vref = V_ref2;
					}
					break;
					case 	3:
					{
						temp_pref = P_ref3;
						temp_qref = Q_ref3;
						temp_vref = V_ref3;
					}
					break;
					default:
					  ;
					break;
				}
				DPX = temp_pref - P_DG1;
				vi1 = kcd * DPX / V_DG1;
              //Iq
				if(QV_flag == 1)
				{
					if(CC_Switch == false)    //droop
 //reactive V_ref control
						vi2 = kcq * (temp_vref - V_DG);
					else
						vi2 = Qmax_phase / V_DG1 * (-kcq * Gradient1);
					if((Q_DG1 >= Qmax_phase) || (Q_DG1 <= Qmin_phase))  // switch control mode to Q_ref control

                    //QV_flag := 0;
					{
						if(Q_DG1 >= Qmax_phase) // set Q_ref
							temp_qref = Qmax_phase;
						else
							temp_qref = Qmin_phase;
                    //
						vi2 = kqi * (temp_qref - Q_DG); //reactive Q_ref control
                    //send Qref back
                    //q_ref1 := temp_qref;
                    //q_ref2 := temp_qref;
                    //q_ref3 := temp_qref;
					}
				}
				else
				vi2 = kqi * (temp_qref - Q_DG); //reactive Q_ref control
              //update V_in_var
				(V_in_var)[1 - 1] = vi1;
				(V_in_var)[2 - 1] = vi2;
			}
		}
	}
    /*--------------------------------*/
    // cooperate part is done here
    //pVinput^[j];
	update_pV_f_CC(ActorID); //update pV_f_CC which is cooperate control
    /*--------------------------------*/
//implement cooperate control
	for(stop = NumOrderX, j = 1; j <= stop; j++)
	{
		(V_in_var)[j - 1] = (V_in_var)[j - 1] + (pV_f_CC)[j - 1];
	}
}  //for power flow

void TGeneric5Obj::update_pV_f_CC_M2(int ActorID)
{
	int j = 0;
	int num_vleader = 0;
	double Bii = 0.0;
	if(CC_Switch == false)
	{
		int stop = 0;
		for(stop = NumOrderX, j = 1; j <= stop; j++)
		{
			(pV_f_CC)[j - 1] = 0.0;
		}
		return;
	}

            //avg ctrl, under V120, I120
      //////////////////////////////////
	if(FMonObj != nullptr)
 //try
	{
		num_vleader = 1;
            /////////////////////////////////
		if(ctrl_mode == 0)
                       //u = gradient + pV_f_CC; pV_f_CC = -alpha + sum(alpha_j)
		{
			Bii = ActiveCircuit[ActorID]->Solution->NodeYii[(NodeRef)[1 - 1]].im;
                        // Q ctrl with v_ref
                      //  pV_f_CC^[2] := FmonObj.Calc_Alpha_M2(ndNumincluster,0,NodeRef^[1],Bii,kcq,Volt_Trhd); // for dIddt, diqdt
                        // Q ctrl with loss
                        //pV_f_CC^[2] := FmonObj.Calc_Alpha_L(ndNumincluster,0,NodeRef^[1],Bii,kcq,Volt_Trhd);
			(pV_f_CC)[2 - 1] = FMonObj->Calc_Alpha_LnM2(NdNumInCluster, 0, (NodeRef)[1 - 1], Bii, kcq, Volt_Trhd, ActorID);
                        // pV_f_CC^[2] := alpha * Qmax / v ;
                        //P ctrl
                        //pV_f_CC^[1] := FmonObj.Calc_AlphaP(ndNumincluster,0); // for dIddt, diqdt
			(pV_f_CC)[1 - 1] = 0;
                  // phases control
		}
		else
		{
			if(Fnphases == 3)
			{
				(pV_f_CC)[6 - 1] = 0.0;
                        //u = gradient + pV_f_CC; pV_f_CC = -alpha + sum(alpha_j)
				Bii = ActiveCircuit[ActorID]->Solution->NodeYii[(NodeRef)[1 - 1]].im;
				(pV_f_CC)[2 - 1] = FMonObj->Calc_Alpha_M2(NdNumInCluster, 1, (NodeRef)[1 - 1], Bii, kcq, Volt_Trhd, ActorID);
                        //pV_f_CC^[2] := FmonObj.Calc_Alpha_L(ndNumincluster,1,NodeRef^[1],Bii,kcq,Volt_Trhd);
                        //pV_f_CC^[2] := FmonObj.Calc_Alpha_LnM2(ndNumincluster,1,NodeRef^[1],Bii,kcq,Volt_Trhd);
				(pV_f_CC)[1 - 1] = FMonObj->Calc_AlphaP(NdNumInCluster, 1, ActorID);
				Bii = ActiveCircuit[ActorID]->Solution->NodeYii[(NodeRef)[2 - 1]].im;
				(pV_f_CC)[4 - 1] = FMonObj->Calc_Alpha_M2(NdNumInCluster, 2, (NodeRef)[2 - 1], Bii, kcq, Volt_Trhd, ActorID);
                        //pV_f_CC^[4] := FmonObj.Calc_Alpha_L(ndNumincluster,2,NodeRef^[2],Bii,kcq,Volt_Trhd);
                        //pV_f_CC^[4] := FmonObj.Calc_Alpha_LnM2(ndNumincluster,2,NodeRef^[2],Bii,kcq,Volt_Trhd);
				(pV_f_CC)[3 - 1] = FMonObj->Calc_AlphaP(NdNumInCluster, 2, ActorID);
				Bii = ActiveCircuit[ActorID]->Solution->NodeYii[(NodeRef)[3 - 1]].im;
				(pV_f_CC)[6 - 1] = FMonObj->Calc_Alpha_M2(NdNumInCluster, 3, (NodeRef)[3 - 1], Bii, kcq, Volt_Trhd, ActorID);
                        //pV_f_CC^[6] := FmonObj.Calc_Alpha_L(ndNumincluster,3,NodeRef^[3],Bii,kcq,Volt_Trhd);
                        //pV_f_CC^[6] := FmonObj.Calc_Alpha_LnM2(ndNumincluster,3,NodeRef^[3],Bii,kcq,Volt_Trhd);
				(pV_f_CC)[5 - 1] = FMonObj->Calc_AlphaP(NdNumInCluster, 3, ActorID);
                        //pV_f_CC[1-6]； // for dIddt1, diqdt1,dIddt2, diqdt2,dIddt3, diqdt3
			}
			else
			{
				if(Fnphases == 1)
                        //if ctrl_mode=1 then
				{
					Bii = ActiveCircuit[ActorID]->Solution->NodeYii[(NodeRef)[1 - 1]].im;
					(pV_f_CC)[2 - 1] = FMonObj->Calc_Alpha_M2(NdNumInCluster, ctrl_mode, (NodeRef)[1 - 1], Bii, kcq, Volt_Trhd, ActorID); // for dIddt1, diqdt1
                        //pV_f_CC^[2] :=  FmonObj.Calc_Alpha_L(ndNumincluster,ctrl_mode,NodeRef^[1],Bii,kcq,Volt_Trhd); // for dIddt1, diqdt1
                        //pV_f_CC^[2] :=  FmonObj.Calc_Alpha_LnM2(ndNumincluster,ctrl_mode,NodeRef^[1],Bii,kcq,Volt_Trhd);
					(pV_f_CC)[1 - 1] = FMonObj->Calc_AlphaP(NdNumInCluster, ctrl_mode, ActorID);
				}
			}
		}
	}
} //used in dynamic mode to update alpha

void TGeneric5Obj::update_pV_f_CC(int ActorID)
{
	int p_mode = 0;
	int j = 0;
	int num_vleader = 0;
	double Bii = 0.0;
	double us_i = 0.0;
	double ul_i = 0.0;
	if(CC_Switch == false)    //no control at all   //local gradient control will be set by communication matrix
	{
		int stop = 0;
		for(stop = NumOrderX, j = 1; j <= stop; j++)
		{
			(pV_f_CC)[j - 1] = 0.0;
		}
		return;
	}

            //avg ctrl, under V120, I120
      //////////////////////////////////
	if(FMonObj != nullptr)
 //try
	{
		num_vleader = 1;
            /////////////////////////////////
		if(ctrl_mode == 0)
		{
			p_mode = 0;
			if(FMonObj != nullptr)
				p_mode = FMonObj->Get_P_mode(ActorID);
             //u = gradient + pV_f_CC; pV_f_CC = -alpha + sum(alpha_j)
			Bii = ActiveCircuit[ActorID]->Solution->NodeYii[(NodeRef)[1 - 1]].im;
			if(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode == DYNAMICMODE)
                    //Ip control
			{
				if(FMonObj->ld_fm_info[0].b_Curt_Ctrl == true) // curtailment algorithm
				{
					ul_i = FMonObj->Calc_ul_P(NdNumInCluster, 0);
					us_i = kcd * FMonObj->Calc_Gradient_ct_P(NdNumInCluster, 0, ActorID);
					GradientP = us_i;
					if(CC_Switch == false)   //local
						(pV_f_CC)[1 - 1] = 0.0;
					else
					{
						(pV_f_CC)[1 - 1] = ul_i + us_i;
						(pV_f_CC)[1 - 1] = (pV_f_CC)[1 - 1] * PMax / V_DG;
					}
				}

                    //if pMode then
				if((p_mode == 1) && (CC_Switch == true)) //if delta P = p_trans_ref - p_trans
 //balance p_trans
				{
					(pV_f_CC)[1 - 1] = FMonObj->Calc_AlphaP(NdNumInCluster, 0, ActorID);//new alfa_p
					(pV_f_CC)[1 - 1] = (pV_f_CC)[1 - 1] - AlphaP; //derivative of alfa_p
                           //use us_i to calculate the frequncy
					us_i = -FMonObj->omg_fm; //frequency droop
					(pV_f_CC)[1 - 1] = ((pV_f_CC)[1 - 1] + us_i) * PMax / V_DG; // derivative of Ip in dynamic mode,
                           //use us_i to
				}

                    //Iq control
				ul_i = FMonObj->Calc_fm_ul_0(NdNumInCluster, 0, (NodeRef)[1 - 1], Bii, kcq, Volt_Trhd, ActorID);
				us_i = FMonObj->Calc_fm_us_0(NdNumInCluster, 0, (NodeRef)[1 - 1], Bii, kcq, Volt_Trhd, ActorID);
				Gradient = us_i;
				if(FMonObj->ld_fm_info[0].b_Curt_Ctrl == false)
					;
				else
 // if curtailment for this cluster is off
 // if curtailment for this cluster is on
                        //Q will try to boost the voltage while P is decreasing
				{
					if((ActiveCircuit[ActorID]->Solution->bCurtl == true) && (Gradient == 0.0))
						us_i = -GradientP * PMax / Qmax;
				}
				if(CC_Switch == false)   //local
				{
					(pV_f_CC)[2 - 1] = us_i;
				}
				else
  // cc_switch is on
				{
					(pV_f_CC)[2 - 1] = ul_i + us_i; //cc  //attack comes in ul_i (FmonObj.Calc_fm_ul_0)
				}
				(pV_f_CC)[2 - 1] = (pV_f_CC)[2 - 1] * Qmax / V_DG;
			}
			else
  //power flow

                    //alphaP: p ratio

                    //if pMode then
			{
				if((p_mode == 1) || (FMonObj->ld_fm_info[0].b_Curt_Ctrl == true))
					(pV_f_CC)[1 - 1] = FMonObj->Calc_AlphaP(NdNumInCluster, 0, ActorID);
				else
					(pV_f_CC)[1 - 1] = 0.0;
                    //alpha : q ratio
				(pV_f_CC)[2 - 1] = FMonObj->Calc_Alpha_M2(NdNumInCluster, 0, (NodeRef)[1 - 1], Bii, kcq, Volt_Trhd, ActorID); // for dIddt, diqdt
			}
		}
	}
}
//var
//  j : integer;

void TGeneric5Obj::InfoPublish(int ActorID)
{
	Update_PQlimits();
	if(FMonObj != nullptr)
	{
		/*# with FMonObj->pNodeFMs^[NdNumInCluster] do */
		{
			auto& with0 = (FMonObj->pNodeFMs)[NdNumInCluster];
			switch(ctrl_mode)
			{
				case 	1:
				{
					with0.vl_V1 = V_DG1;//Phase A or the first phase if there are less than 3 phases
					with0.vl_p_DG1 = P_DG1;
					with0.vl_q_DG1 = Q_DG1;
                    //vl_Alpha1_dg := Alpha1;
                    //vl_AlphaP1_dg := AlphaP1;
					with0.vl_V_ref1_dg = V_ref1;
				}
				break;
				case 	2:
				{
					with0.vl_V2 = V_DG2;//Phase B if exists
					with0.vl_p_DG2 = P_DG2;
					with0.vl_q_DG2 = Q_dg2;
                    //vl_Alpha2_dg := Alpha2;
                    //vl_AlphaP2_dg := AlphaP2;
					with0.vl_V_ref2_dg = V_ref2;
				}
				break;
				case 	3:
				{
					with0.vl_V3 = V_DG3;//Phase c if exists
					with0.vl_p_DG3 = P_dg3;
					with0.vl_q_DG3 = Q_dg3;
                    //vl_Alpha3_dg := Alpha3;
                    //vl_AlphaP3_dg := AlphaP3;
					with0.vl_V_ref3_dg = V_ref3;
				}
				break;
				case 	0:
				{
					with0.vl_V = V_DG;  //0 seq    , will be used in FmonObj.Agnt_smpl
					with0.vl_p_DG = P_DG;
					with0.vl_q_DG = Q_DG;
                    //
					Alpha = Q_DG / Qmax;
					with0.vl_Alpha_dg = Alpha;  // update first, will be used in FmonObj.Agnt_smpl
                    //P control
					AlphaP = P_DG / PMax;
					with0.vl_AlphaP_dg = AlphaP;
                    //
					with0.vl_V_ref_dg = V_ref;
					if(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode == DYNAMICMODE)
					{
						z_dfs_plot = with0.z_dfs; // defense value
					}
				}
				break;
				case 	4:
				{
					with0.vl_V1 = V_DG1;//Phase A or the first phase if there are less than 3 phases
					with0.vl_p_DG1 = P_DG1;
					with0.vl_q_DG1 = Q_DG1;
					with0.vl_V2 = V_DG2;//Phase B if exists
					with0.vl_p_DG2 = P_DG2;
					with0.vl_q_DG2 = Q_dg2;
					with0.vl_V3 = V_DG3;//Phase c if exists
					with0.vl_p_DG3 = P_dg3;
					with0.vl_q_DG3 = Q_dg3;
					with0.vl_V_ref1_dg = V_ref1;
					with0.vl_V_ref2_dg = V_ref2;
					with0.vl_V_ref3_dg = V_ref3;

                    //vl_Alpha1_dg := Alpha1;
                    //vl_AlphaP1_dg := AlphaP1;
                    //vl_V_ref1_dg := V_ref1;
                    //vl_Alpha2_dg := Alpha2;
                    //vl_AlphaP2_dg := AlphaP2;
                    //vl_V_ref2_dg := V_ref2;
                    //vl_Alpha3_dg := Alpha3;
                    //vl_AlphaP3_dg := AlphaP3;
                    //vl_V_ref3_dg := V_ref3;
				}
				break;
				default:
				  ;
				break;
			}
			with0.vl_Qmax_dg = Qmax;
			with0.vl_Qmax_phase_dg = Qmax_phase;
			with0.vl_Pmax_dg = PMax;
			with0.vl_Pmax_phase_dg = PMax_phase;
			with0.vl_CC_switch_dg = CC_Switch;
			with0.vl_QV_flag_dg = QV_flag;
			with0.vl_kcd_dg = kcd;
			with0.vl_kcq_dg = kcq;
			with0.vl_volt_thrd_dg = Volt_Trhd;
		}
	}
         /*with FmonObj2.pNodeFMs^[NdNuminCluster2] do
         begin

          vl_V := V_DG;
          vl_P_DG := P_DG;
          vl_Q_DG := Q_DG;
          vl_V1 := V_DG1;//Phase A or the first phase if there are less than 3 phases
          vl_P_DG1 := P_DG1;
          vl_Q_DG1 := Q_DG1;
          vl_V2 := V_DG2;//Phase B if exists
          vl_P_DG2 := P_DG2;
          vl_Q_DG2 := Q_DG2;
          vl_V3 := V_DG3;//Phase c if exists
          vl_P_DG3 := P_DG3;
          vl_Q_DG3 := Q_DG3;
          vl_Qmax_dg := Qmax;
          vl_Qmax_phase_dg := Qmax_Phase;
          vl_Pmax_dg := Pmax;
          vl_Pmax_phase_dg := Pmax_Phase;
          vl_Alpha_dg := alpha;
          vl_Alpha1_dg := Alpha1;
          vl_Alpha2_dg := Alpha2;
          vl_Alpha3_dg := Alpha3;
          vl_AlphaP_dg := alphaP;
          vl_AlphaP1_dg := AlphaP1;
          vl_AlphaP2_dg := AlphaP2;
          vl_AlphaP3_dg := AlphaP3;
          vl_V_ref_dg := V_ref;
          vl_V_ref1_dg := V_ref1;
          vl_V_ref2_dg := V_ref2;
          vl_V_ref3_dg := V_ref3;
          vl_CC_switch_dg := CC_switch;
          vl_QV_flag_dg := QV_flag;

          end; */
	if(FMonObj2 != nullptr)
		;
}

void TGeneric5Obj::Set_P_Ref(double PrefkW, int ActorID)
{
	P_ref = (double) (1000 * PrefkW / Fnphases);   //nphases
	P_ref1 = P_ref;
	P_ref2 = P_ref;
	P_ref3 = P_ref;
	if(PrefkW > kWBase)
		DoSimpleMsg("P ref should be leq than kW", 562);
	if(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode == DYNAMICMODE) //if P_ref changed in the dynamic simulation
                                                              //The sudden change of Id has to be applied
	{
		flag_dyna_Id_chg = true;
	}
    //
}

void TGeneric5Obj::Set_Q_Ref(double QrefkVAr)
{

    //
	Q_ref = (double) (1000 * QrefkVAr / Fnphases);  //nphases
	Q_ref1 = Q_ref;
	Q_ref2 = Q_ref;
	Q_ref3 = Q_ref;
}

void TGeneric5Obj::Set_V_Ref(double VrefkV)
{

    //
	V_ref = 1000 * VrefkV;
	V_ref1 = V_ref;
	V_ref2 = V_ref;
	V_ref3 = V_ref;
}

void TGeneric5Obj::Update_kWbase_by_Fctrs()
{
	kWBase = (Pmpp + Pbias) * Pfctr1 * Pfctr2 * Pfctr3 * Pfctr4 * Pfctr5 * Pfctr6;
	kWBase = kWBase / 1000;
	Update_PQlimits();
}

void TGeneric5Obj::update_system_abcd()
{

    //2 order system example
    //Amn := Zeros;
    //Bmn := I;
    //Cnm := I;
	   /*-Done in the initial part-*/
}

void TGeneric5Obj::UpdateAlpha_qi()
{

      //Update_PQlimits;
	if(Qmax > EPSILON)
          //pos control
	{
		Alpha = Q_DG / Qmax;
          // phase control
		Alpha1 = Q_DG1 / Qmax_phase;
		if(Fnphases == 3)
		{
			Alpha2 = Q_dg2 / Qmax_phase;
			Alpha3 = Q_dg3 / Qmax_phase;
		}
	}
}

void TGeneric5Obj::Update_PQlimits()
{
	if(PQpriority == 1) //P prior  by default
	{
		if((PMax <= 0) || (PMax > kWBase))
			PMax = kWBase * 1000;// first value is set to be kWbase;   when kWbase is set, Pmax will be update in edit;
		PMax = kWBase * 1000;//if PQpriority=1 then
            //if (Pmax <0 )or (Pmax > kWBase*1000) then Pmax := kWBase*1000;
            //Pmax
            //Pmax; should be what it is
            //P_DG
		if(1000 * MachineData.kVArating >= P_DG)   //  Pmax P_DG
 // PMax*PMax)//
			Qmax = sqrt(MachineData.kVArating * 1000 * MachineData.kVArating * 1000 - P_DG * P_DG);
		else
			Qmax = EPSILON;  //error when used as demoninator
		Qmin = -Qmax;
	}
	else
	{
		if(PQpriority == 0) //Q prior
		{
			Qmax = MachineData.kVArating;
			Qmin = -MachineData.kVArating;
			PMax = min(sqrt(MachineData.kVArating * 1000 * MachineData.kVArating * 1000 - Q_DG * Q_DG), kWBase * 1000); //which one is smaller
			PMin = 0;
             //Pmax_phase := Pmax / fnphases;
             //Pmin_phase := Pmin / fnphases;
		}
        //if machinedata.kVArating <= 0 then
        //begin
        //  if fmonobj <> nil then
        //    Qmax := fmonobj.CalcAvgQmax;     //if the node is without control, Qmax := cluster avg
        //end;
	}
	PMax_phase = PMax / Fnphases;
	Pmin_phase = PMin / Fnphases;
	Qmax_phase = Qmax / Fnphases;
	Qmin_phase = Qmin / Fnphases;
      //used for limit currents derivative
	IdMax_phase = PMax_phase / (VBase);// vBase := kVGeneratorBase*InvSQRT31000
	IqMax_phase = Qmax_phase / (VBase);
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TGeneric5Obj::CalcDynamic(TSymCompArray5 V012, TSymCompArray5 I012, int ActorID)
{
	double Pref3 = 0.0;
      //Update_Pqlimits;// confirm P, Q limits
	if(ctrl_mode == 0)
	{
		InDynamics = true;
		V1 = V012[1];   // Save for variable calcs
		V2 = V012[2];
		V_DG = cabs(V1);
		Theta_DG = cang(V1);


      /*P*/  //P_DG follows ref, and allows sudden change
		P_DG = V_DG * id; //update P_DG
           // if P_DG, Q_DG exceed the limits
		if(P_DG > PMax)
		{
			P_DG = PMax; //set real power change during the simulation
			id = P_DG / V_DG;    //set Id
			Idn = id;
			(X_var)[1 - 1] = id;
			(X_varn)[1 - 1] = Idn;
			(dX_vardtn)[1 - 1] = 0.0;
		}
		else
		{
			if(P_DG < PMin)
			{
				P_DG = PMin;
				id = P_DG / V_DG;    //set Id
				Idn = id;
				(X_var)[1 - 1] = id;
				(X_varn)[1 - 1] = Idn;
				(dX_vardtn)[1 - 1] = 0.0;
			}

                 /*Q*/
		}
		Q_DG = V_DG * Iq;
		if(Q_DG >= Qmax)
		{
			Q_DG = Qmax;
			Iq = Q_DG / V_DG;
			Iqn = Iq;
			(X_var)[2 - 1] = Iq;
			(X_varn)[2 - 1] = Iqn;
			(dX_vardtn)[1 - 1] = 0.0;
		}
		else
		{
			if(Q_DG <= Qmin)
			{
				Q_DG = Qmin;
				Iq = Q_DG / V_DG;
				Iqn = Iq;
				(X_var)[2 - 1] = Iq;
				(X_varn)[2 - 1] = Iqn;
				(dX_vardtn)[1 - 1] = 0.0;
			}
		}
		Get_DynamicModelCurrent(); //Iq Iq does not change, Is1 := cmplx(Id, Iq)*1<angle Is2 := CZERO
               //Id and Iq are divided by/3.0 to be I012                     // Is2 is calculated here(In XY domain), will be used as I012
                                    // sqrt(Iq*Iq +Id*Id),Theta_DG - arctan(Iq/Id)
		AlphaP = P_DG / PMax;
		Alpha = Q_DG / Qmax;
		I012[1] = Is1;    // Id and Iq /3.0
		I012[2] = Is2;
		I012[0] = cmplx(0.0, 0.0); //force balance
           /*change direction*/ //added by dahei  input should be negtive
		I012[1] = cnegate(I012[1]);
		I012[2] = cnegate(I012[2]);
		I012[0] = cnegate(I012[0]);

           //CalGradient: will be published into FMonitor
	}
}   //failed, Q_Di and Bii are not available inside DD
                                        // will be done in FMonitor

void TGeneric5Obj::CalGradient()
{
	double Den = 0.0;
      //Gradient :=
  // will be done in FMonitor
}
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
//   Vabc is the terminal voltages of the connecting bus
//   Iabc will be returned as the currents injection into network
//   This func will be called after integrate, so 'Id1, Iq1,  Id2, Iq2,  Id3, Iq3' have been integrated for current time step
//   ,whcih means at the end of integration 'Id1, Iq1,  Id2, Iq2,  Id3, Iq3' shoulbe be valued
//   ' P_DG1, P_DG2,P_dg3, Q_DG1, Q_dg2,Q_dg3' will also be calculated in this func
//    'V_DG1, V_DG2,V_dg3'

void TGeneric5Obj::CalcDynamicVIabc(pComplexArray Vabc, pComplexArray Iabc, int ActorID)
{
	complex tempV1 = {};
	complex tempV2 = {};
	complex tempV3 = {};
	complex Curr1 = {};
	complex Curr2 = {};
	complex Curr3 = {};
	double temp_pref = 0.0;
	double temp_qref = 0.0;
	double temp_vref = 0.0;
	double tempAngleR = 0.0;
      /*----------------*/
      //Update_Pqlimits;// confirm P, Q limits
	      /*----------------*/
	if(ctrl_mode == 0)
		;
	else

          //will never be used
          //will be in CalcDynamic
 // avg ctrl
 //direct phase ctrl
	{
		if(Fnphases == 3)
               //3-phase ctrl
		{
			InDynamics = true;
			tempV1 = (Vabc)[1 - 1];   // Save for variable calcs
			tempV2 = (Vabc)[2 - 1];
			tempV3 = (Vabc)[3 - 1];
			V_DG1 = cabs(tempV1);
			V_DG2 = cabs(tempV2);
			V_DG3 = cabs(tempV3);
			V_theta1 = cang(tempV1);
			V_theta2 = cang(tempV2);
			V_theta3 = cang(tempV3);
               ///
               ///  Model currents Iabc injectted into network by Id1, Iq1, Id2, Iq2, Id3, Iq3, Vabc/////
                //Id1, Iq1
			if(ID1 == 0.0)
				tempAngleR = double(DSSGlobals::PI) / 2;
			else
				tempAngleR = atan(Iq1 / ID1);
			Curr1 = pclx(sqrt(Iq1 * Iq1 + ID1 * ID1), V_theta1 - tempAngleR);//with respect to Q_axis
                //Id2, Iq2
			if(ID2 == 0.0)
				tempAngleR = double(DSSGlobals::PI) / 2;
			else
				tempAngleR = atan(Iq2 / ID2);
			Curr2 = pclx(sqrt(Iq2 * Iq2 + ID2 * ID2), V_theta2 - tempAngleR);//with respect to Q_axis
                //Id3, Iq3
			if(Id3 == 0.0)
				tempAngleR = double(DSSGlobals::PI) / 2;
			else
				tempAngleR = atan(Iq3 / Id3);
			Curr3 = pclx(sqrt(Iq3 * Iq3 + Id3 * Id3), V_theta3 - tempAngleR);//with respect to Q_axis
               //////////////////////////////////////////////////////
               ///Update power at current time step
			P_DG1 = V_DG1 * ID1;
			Q_DG1 = V_DG1 * Iq1;
			P_DG2 = V_DG2 * ID2;
			Q_dg2 = V_DG2 * Iq2;
			P_dg3 = V_DG3 * Id3;
			Q_dg3 = V_DG3 * Iq3;
                //sum
			P_DG = P_DG1 + P_DG2 + P_dg3;  //element output
			Q_DG = Q_DG1 + Q_dg2 + Q_dg3;
               ////////////////////////////////
             // inject into network
			(Iabc)[1 - 1] = Curr1;
			(Iabc)[2 - 1] = Curr2;
			(Iabc)[3 - 1] = Curr3;
               /*change direction*/ //added by dahei  input should be negtive
			(Iabc)[1 - 1] = cnegate((Iabc)[1 - 1]);
			(Iabc)[2 - 1] = cnegate((Iabc)[2 - 1]);
			(Iabc)[3 - 1] = cnegate((Iabc)[3 - 1]);
		}
		else
		{
            //no consideration for 2-phase DG
			if(Fnphases == 1)
                //1-phase ctrl
			{
				InDynamics = true;
				tempV1 = (Vabc)[1 - 1];   // Save for variable calcs
				V_DG1 = cabs(tempV1);
				V_theta1 = cang(tempV1);
               ///
               ///  Model currents Iabc injectted into network by Id1, Iq1, Id2, Iq2, Id3, Iq3, Vabc/////
                //Id1, Iq1
				if(ID1 == 0.0)
					tempAngleR = double(DSSGlobals::PI) / 2;
				else
					tempAngleR = atan(Iq1 / ID1);
				Curr1 = pclx(sqrt(Iq1 * Iq1 + ID1 * ID1), V_theta1 - tempAngleR);//with respect to Q_axis

               //////////////////////////////////////////////////////
               ///Update power at current time step
				P_DG1 = V_DG1 * ID1;
				Q_DG1 = V_DG1 * Iq1;
                //sum
				P_DG = P_DG1;//
				Q_DG = Q_DG1;
               ////////////////////////////////
             // inject into network
				(Iabc)[1 - 1] = Curr1;
               /*change direction*/ //added by dahei  input should be negtive
				(Iabc)[1 - 1] = cnegate((Iabc)[1 - 1]);
			}
		}
	}
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

//Var P_Error:Double;

void TGeneric5Obj::CalcPFlow(TSymCompArray5 V012, TSymCompArray5 I012, int ActorID)
{


//   i  : Integer;
//   DQ : Double;
	complex Curr = {};
	int p_mode = 0;
	if(ctrl_mode == 0)   //duplicate all codes as avg ctrl, under V120, I120
	{
		V1 = V012[1];   // Save for variable calcs
		V2 = V012[2];
		if(cabs(V1) == 0.0)
			V1 = cmplx(1, 0);   //in Case the first step
		InDynamics = false;
             // Guess at a new var output value
		V_DG = cabs(V1);
		Theta_DG = cang(V1);
             //this should be the the online system index, has to be improved by
             //TGeneric5Obj.RememberQV
             //TGeneric5Obj.CalcDQDV
             //

             /*----real power is control by Pref in DG----*/
		update_pV_f_CC(ActorID);  //AlphaP, Alpha
		p_mode = 0;
		if(FMonObj != nullptr)
			p_mode = FMonObj->Get_P_mode(ActorID);
		if((p_mode == 1) && (CC_Switch == true)) //if delta P = p_trans_ref - p_trans
 //balance p_trans
		{
			AlphaP = (pV_f_CC)[1 - 1]; //alpha_p
			P_DG = PMax * AlphaP;
		}
		else
		{
			P_DG = Fnphases * P_ref; // local
		}
		if(P_DG > PMax)
		{
			P_DG = PMax;
		}
		else
		{
			if(P_DG < PMin)
			{
				P_DG = PMin;
			}
		}
		AlphaP = P_DG / PMax;
             /*--- real power is controled above --*/
		if(QV_flag == 0)    //P_ref, Q_ref
			Curr = conjg(cdiv(cmplx(P_DG / 3.0, Q_ref), V1));
		else
                 //P_ref, V_ref
		{
			if(ActiveCircuit[ActorID]->Solution->Iteration == 1)
			{
				Iq = 0; //In power flow, start value of Iq for each power flow
			}

                  //update_pV_f_CC(ActorID);  //Alpha
			Alpha = (pV_f_CC)[2 - 1];  // only when not dynamode
			Q_DG = Qmax * Alpha;
			Curr = conjg(cdiv(cmplx(P_DG / 3.0, Q_DG / 3.0), V1));
                  /*----------------*/
		}
		I012[1] = Curr;    // Save for variable calcs
		I012[2] = cmplx(0.0, 0.0);//force to be balanced output DG
		I012[0] = cmplx(0.0, 0.0);
             /*change direction*///added by dahei
		I012[1] = cnegate(I012[1]);
		I012[2] = cnegate(I012[2]);
		I012[0] = cnegate(I012[0]); // avg ctrl
	}
	else
 //direct phase ctrl
	{
		if(Fnphases == 3)
			;
		else
		{
                    //3-phase ctrl

                    //1-phase ctrl

                //no consideration for 2-phase DG
			if(Fnphases == 1)
				;
		}
	}
}
//----------------------------------------------------------------------------

void TGeneric5Obj::CalcPFlowVIabc(pComplexArray Vabc, pComplexArray Iabc, int ActorID)
{
	complex tempV1 = {};
	complex tempV2 = {};
	complex tempV3 = {};
	complex Curr1 = {};
	complex Curr2 = {};
	complex Curr3 = {};
	double temp_pref = 0.0;
	double temp_qref = 0.0;
	double temp_vref = 0.0;
	double temp_alpha = 0.0;
	double flmt = 0.0;
	int p_mode = 0;
	flmt = 0.9;
	Update_PQlimits(); //  Pmax_phase, Qmax_phase will be used in the following steps
	update_pV_f_CC_M2(ActorID);  // pV_f_CC, updated from virtual leader
                          // Q ctrl: 3-phase,  pV_f_CC^[2], [4], [6]
                          // 1-phase,  pV_f_CC^[2]
                          // P ctrl: 3-phase,  pV_f_CC^[1], [3], [5]
                          // 1-phase,  pV_f_CC^[1]
	if(Fnphases == 3)
	{
		tempV1 = (Vabc)[1 - 1];  // Save for variable calcs //assume Vabc[1][2][3] is ABC!
		tempV2 = (Vabc)[2 - 1];
		tempV3 = (Vabc)[3 - 1];
		if(cabs(tempV1) == 0)
			tempV1 = cmplx(1, 0);
		if(cabs(tempV2) == 0)
			tempV2 = cmplx(1, 0);
		if(cabs(tempV3) == 0)
			tempV3 = cmplx(1, 0);
	}
	else
	{
		if(Fnphases == 1)
		{
			tempV1 = (Vabc)[1 - 1];  // Save for variable calcs //assume Vabc[1][2][3] is ABC!
			tempV2 = cmplx(1, 0);
			tempV3 = cmplx(1, 0);
		}
	}
	V_DG1 = cabs(tempV1);   // Save for variable calcs
	V_DG2 = cabs(tempV2);
	V_DG3 = cabs(tempV3);
      /*----real power is control by Pref in DG----*/
	P_DG1 = P_ref1;
	P_DG2 = P_ref2;
	P_dg3 = P_ref3;

       /**/
      //calculate_gradient; //alpha, dalpha, and gradients
      //alpha is implemented in M2
	p_mode = 0;
	if(FMonObj != nullptr)
		p_mode = FMonObj->Get_P_mode(ActorID);
	if((p_mode == 1) && (CC_Switch == true)) //if delta P = p_trans_ref - p_trans
	{
		switch(ctrl_mode)
		{
			case 	1:
			{
				AlphaP1 = (pV_f_CC)[1 - 1];
				P_DG1 = P_DG1 + PMax_phase * AlphaP1;
                          //p_DG1 :=  Pmax_phase * AlphaP1;
			}
			break;
			case 	2:
			{
				AlphaP2 = (pV_f_CC)[1 - 1];//if single phase only    pV_f_CC^[1] and pV_f_CC^[2]
				P_DG2 = P_DG2 + PMax_phase * AlphaP2;
                          //p_DG2 :=  Pmax_phase * AlphaP2;
			}
			break;
			case 	3:
			{
				AlphaP3 = (pV_f_CC)[1 - 1]; //if single phase only    pV_f_CC^[1] and pV_f_CC^[2]
				P_dg3 = P_dg3 + PMax_phase * AlphaP3;
                          //p_DG3 :=  Pmax_phase * AlphaP3;
			}
			break;
			case 	4:
			{
				AlphaP1 = (pV_f_CC)[1 - 1];
				P_DG1 = P_DG1 + PMax_phase * AlphaP1;
                          //p_DG1 :=  Pmax_phase * AlphaP1;
				AlphaP2 = (pV_f_CC)[3 - 1];
				P_DG2 = P_DG2 + PMax_phase * AlphaP2;
                          //p_DG2 :=  Pmax_phase * AlphaP2;
				AlphaP3 = (pV_f_CC)[5 - 1];
				P_dg3 = P_dg3 + PMax_phase * AlphaP3;
                          //p_DG3 :=  Pmax_phase * AlphaP3;
			}
			break;
			default:
			  ;
			break;
		}
		if(P_DG1 > PMax_phase)
		{
			P_DG1 = PMax_phase;
		}
		else
		{
			if(P_DG1 < Pmin_phase)
			{
				P_DG1 = Pmin_phase;
			}
		}
		if(P_DG2 > PMax_phase)
		{
			P_DG2 = PMax_phase;
		}
		else
		{
			if(P_DG2 < Pmin_phase)
			{
				P_DG2 = Pmin_phase;
			}
		}
		if(P_dg3 > PMax_phase)
		{
			P_dg3 = PMax_phase;
		}
		else
		{
			if(P_dg3 < Pmin_phase)
			{
				P_dg3 = Pmin_phase;
			}
		}
		Update_PQlimits(); //  Qmax_phase will be updated accordingly
	}
            // calc P_DG
	switch(ctrl_mode)
	{
		case 	1:
		{
			P_DG = P_DG1;
		}
		break;
		case 	2:
		{
			P_DG = P_DG2;
		}
		break;
		case 	3:
		{
			P_DG = P_dg3;
		}
		break;
		case 	4:
		{
			P_DG = P_DG1 + P_DG2 + P_dg3;
		}
		break;
		default:
		  ;
		break;
	}
      /*Q Control*/
	if(ctrl_mode == 0)
		;
	else

          //will never be used
          //will be in CalcPFlow
 // avg ctrl
 //direct phase ctrl
	{
		if(Fnphases == 3)
		            //3-phase ctrl
              // V_Theta1 := cang(V1);
              // V_Theta2 := cang(V2);
              // V_Theta3 := cang(V3);
		{
			InDynamics = false;

               //if (P_Mode = 1) and  then
               //real power control

               // Guess at a new var output value
			if(QV_flag == 0)    //P_ref, Q_ref
			{
				Curr1 = conjg(cdiv(cmplx(P_DG1, Q_ref1), tempV1));  //currents A,B,C
				Curr2 = conjg(cdiv(cmplx(P_DG2, Q_ref2), tempV2));
				Curr3 = conjg(cdiv(cmplx(P_dg3, Q_ref3), tempV3));
			}
			else
                 //P_ref, V_ref

                  //phase A
                  //1 st ireration Iq := 0;
			{
				if(ActiveCircuit[ActorID]->Solution->Iteration == 1)
				{
					Iq1 = 0; //In power flow, start value of Iq for each power flow
					Iq2 = 0;
					Iq3 = 0;
				}                                                      //should be taken care of here
				if(CC_Switch == false)    //droop                                                   //Q_DG starts from 0

                      ///////////integral droop
				{
					dIqdt = kcq * (V_ref1 - V_DG1) / ActiveCircuit[ActorID]->Solution->Iteration;
					if(Abs((int) (V_ref1 - V_DG1)) <= Volt_Trhd * V_ref1)
						dIqdt = 0.0;

                                        //if abs(dIqdt)> flmt*Iqmax_phase then dIqdt := sign(dIqdt)*flmt*Iqmax_phase;
					Iq1 = Iq1 + dIqdt;
					Q_DG1 = V_DG1 * Iq1;
					if(droop == 2)
                      /////////////////}
						Q_DG1 = kcq_drp2 * (V_ref1 - V_DG1) * 1000 * MachineData.kVArating / 0.05 / V_ref1;
                  /*gradient control*/
				}
				else
                       // cooperative control
                    //dIqdt := Qmax_phase/ V_DG1 * ( - kcq*Gradient1);
                    //dIqdt := dIqdt / ActiveCircuit.Solution.Iteration;
                    //dIqdt := dIqdt + Qmax_phase/ V_DG1 * (pV_f_CC[2]);//dIqdt1
                    //second method
                    //calc alpha
                    //calc Q
                    //Iq1 := Iq1 + dIqdt;
                    //Q_DG1 :=  V_DG1 * Iq1;
				{
					Alpha1 = (pV_f_CC)[2 - 1];
					Q_DG1 = Qmax_phase * Alpha1;
				}
                  /*----------------*/
                  //if abs(dIqdt)> flmt*Iqmax_phase then dIqdt := sign(dIqdt)*flmt*Iqmax_phase;

                  //If (Q_DG1 >= Qmax_phase) then Q_DG1 := Qmax_phase; //limit check only one phase
                  //If Q_DG1 <= Qmin_phase then Q_DG1:= Qmin_phase;

                  //phase B
				if(CC_Switch == false)    //droop

                      ///////////integral droop
				{
					dIqdt = kcq * (V_ref2 - V_DG2) / ActiveCircuit[ActorID]->Solution->Iteration;  // ref control
					if(Abs((int) (V_ref2 - V_DG2)) <= Volt_Trhd * V_ref2)
						dIqdt = 0.0;
					Iq2 = Iq2 + dIqdt;     //In power flow, Iq starts from 0;
					Q_dg2 = V_DG2 * Iq2;
					if(droop == 2)
                      /////////////////}
						Q_dg2 = kcq_drp2 * (V_ref2 - V_DG2) * 1000 * MachineData.kVArating / 0.05 / V_ref1;
                  /*gradient control*/
				}
				else

                      //dIqdt := Qmax_phase/ V_DG2 * ( - kcq*gradient2); //self gradient
                      //dIqdt := dIqdt / ActiveCircuit.Solution.Iteration;
                      //dIqdt := dIqdt + Qmax_phase/ V_DG2 * (pV_f_CC[4]);//dIqdt2
                      //Iq2 := Iq2 + dIqdt;     //In power flow, Iq starts from 0;
                      //Q_DG2 :=  V_DG2 * Iq2;
				{
					Alpha2 = (pV_f_CC)[4 - 1];
					Q_dg2 = Qmax_phase * Alpha2;
				}
                  //if abs(dIqdt)> flmt*Iqmax_phase then dIqdt := sign(dIqdt)*flmt*Iqmax_phase;
				                  /*----------------*/

                  //If Q_DG2 >= Qmax_phase then Q_DG2:= Qmax_phase; //limit check only one phase
                  //If Q_DG2 <= Qmin_phase then Q_DG2:= Qmin_phase;

                  //phase C
				if(CC_Switch == false)    //droop

                  ///////////integral droop
				{
					dIqdt = kcq * (V_ref3 - V_DG3) / ActiveCircuit[ActorID]->Solution->Iteration;
					if(Abs((int) (V_ref3 - V_DG3)) <= Volt_Trhd * V_ref3)
						dIqdt = 0.0;
					Iq3 = Iq3 + dIqdt;     //In power flow, Iq starts from 0;
					Q_dg3 = V_DG3 * Iq3;
					if(droop == 2)  /////////////////}
						Q_dg3 = kcq_drp2 * (V_ref3 - V_DG3) * 1000 * MachineData.kVArating / 0.05 / V_ref1;
				}
				else

                   /*gradient control*/
                      //dIqdt := Qmax_phase/ V_DG3 * ( - kcq*gradient3);
                      //dIqdt := dIqdt / ActiveCircuit.Solution.Iteration;
                      //dIqdt := dIqdt + Qmax_phase/ V_DG3 * (pV_f_CC[6]);//dIqdt3
                      //Iq3 := Iq3 + dIqdt;     //In power flow, Iq starts from 0;
                      //Q_DG3 :=  V_DG3 * Iq3;
				{
					Alpha3 = (pV_f_CC)[6 - 1];
					Q_dg3 = Qmax_phase * Alpha3;
				}

                  /// code bellow is for each phase working seperately
				if(Q_DG1 > Qmax_phase)
				{
					Q_DG1 = Qmax_phase;
				}
				else
				{
					if(Q_DG1 < Qmin_phase)
					{
						Q_DG1 = Qmin_phase;
					}
				}
				Curr1 = conjg(cdiv(cmplx(P_DG1, Q_DG1), tempV1));
				if(Q_dg2 > Qmax_phase)
				{
					Q_dg2 = Qmax_phase;
				}
				else
				{
					if(Q_dg2 < Qmin_phase)
					{
						Q_dg2 = Qmin_phase;
					}
				}
				Curr2 = conjg(cdiv(cmplx(P_DG2, Q_dg2), tempV2));
				if(Q_dg3 > Qmax_phase)
				{
					Q_dg3 = Qmax_phase;
				}
				else
				{
					if(Q_dg3 < Qmin_phase)
					{
						Q_dg3 = Qmin_phase;
					}
				}
				Curr3 = conjg(cdiv(cmplx(P_dg3, Q_dg3), tempV3));
                  /////////////////////////////////////////////////////
			}
			Q_DG = Q_DG1 + Q_dg2 + Q_dg3;//
			(Iabc)[1 - 1] = Curr1;    // Save for variable calcs
			(Iabc)[2 - 1] = Curr2;
			(Iabc)[3 - 1] = Curr3;
               /*change direction*///added by dahei
			(Iabc)[1 - 1] = cnegate((Iabc)[1 - 1]);
			(Iabc)[2 - 1] = cnegate((Iabc)[2 - 1]);
			(Iabc)[3 - 1] = cnegate((Iabc)[3 - 1]);
		}
		else
		{
			if(Fnphases == 1)
                //1-phase ctrl

               //tempV1 := Vabc[1];  // Save for variable calcs //assume Vabc[1][2][3] is ABC!
			{
				V_DG2 = V_DG1;   // Save for variable calcs, just in case of other use
				V_DG3 = V_DG1;
              // V_Theta1 := cang(V1);
				InDynamics = false;
               // Guess at a new var output value
				switch(ctrl_mode)
				{
					case 	1:
					{
						temp_pref = P_DG1;//
						temp_qref = Q_ref1;
						temp_vref = V_ref1;
						Alpha1 = (pV_f_CC)[2 - 1]; //1 phase, only first one. coincident with dynamic calc
						temp_alpha = Alpha1;
					}
					break;
					case 	2:
					{
						temp_pref = P_DG2;
						temp_qref = Q_ref2;
						temp_vref = V_ref2;
						Alpha2 = (pV_f_CC)[2 - 1];
						temp_alpha = Alpha2;
					}
					break;
					case 	3:
					{
						temp_pref = P_dg3;
						temp_qref = Q_ref3;
						temp_vref = V_ref3;
						Alpha3 = (pV_f_CC)[2 - 1];
						temp_alpha = Alpha3;
					}
					break;
					default:
					  ;
					break;
				}
				if(QV_flag == 0)    //P_ref, Q_ref
				{
					Curr1 = conjg(cdiv(cmplx(temp_pref, temp_qref), tempV1));  //currents A,B,C
				}
				else
                 //P_ref, V_ref
    // QV_flag=1

                  //phase 1
                  //1 st ireration Iq := 0;
				{
					if(ActiveCircuit[ActorID]->Solution->Iteration == 1)
					{
						Iq1 = 0; //In power flow, start value of Iq for each power flow
					}
					if(CC_Switch == false)    //droop

                    ///////////integral droop
					{
						dIqdt = kcq * (temp_vref - V_DG1) / ActiveCircuit[ActorID]->Solution->Iteration;
						if(Abs((int) (V_ref1 - V_DG1)) <= Volt_Trhd * V_ref1)
							dIqdt = 0.0;
						Iq1 = Iq1 + dIqdt;     //In power flow, Iq starts from 0;
						temp_qref = V_DG1 * Iq1;
						if(droop == 2)
                    /////////////////}
							temp_qref = kcq_drp2 * (temp_vref - V_DG1) * 1000 * MachineData.kVArating / 0.05 / V_ref1;
					}
					else

                   /*gradient control*/
					{
						temp_qref = Qmax_phase * temp_alpha;
					}
                  /*----------------*/
					if(temp_qref > Qmax_phase)  // switch control mode to Q_ref control
						temp_qref = Qmax_phase;
					else
					{
						if(temp_qref < Qmin_phase)
							temp_qref = Qmin_phase;
					}
					Curr1 = conjg(cdiv(cmplx(temp_pref, temp_qref), tempV1));
					switch(ctrl_mode)
					{
						case 	1:
                              //P_ref1 := temp_pref ;
						{
							Q_DG1 = temp_qref;
                              //v_ref1 :=  temp_vref;
							Alpha1 = temp_alpha;
						}
						break;
                              //P_ref2 :=  temp_pref;
						case 	2:
						{
							Q_dg2 = temp_qref;
                              //v_ref2 := temp_vref ;
							Alpha2 = temp_alpha;
						}
						break;
                              //P_ref3 :=  temp_pref  ;
						case 	3:
						{
							Q_dg3 = temp_qref;
                              //v_ref3 :=  temp_vref  ;
							Alpha3 = temp_alpha;
						}
						break;
						default:
						  ;
						break;
					}

         // else
         // begin
            //no consideration for 2-phase DG
				}
				(Iabc)[1 - 1] = Curr1;    // Save for variable calcs
               /*change direction*///added by dahei
				(Iabc)[1 - 1] = cnegate((Iabc)[1 - 1]);
			} //phase =1
		}
	} //direct phase ctrl
    //InfoPublish;    // publish data into fmonitor
}
//*()//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// typical proc for handling randomization in DSS fashion

void TGeneric5Obj::Randomize(int Opt)
{
	switch(Opt)
	{
		case 	0:
		RandomMult = 1.0;
		break;
    //   GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
		case 	UNIFORM:
		RandomMult = (double) Random();
		break;  // number between 0 and 1.0
     //  LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
		default:
		  ;
		break;
	}
}


/*-------------------------------------------------------------------------------------------------------------*/
/*-------------------------------------------------------------------------------------------------------------*/

// Init for Dynamics mode

void TGeneric5Obj::InitModel(const TSymCompArray5& V012, const TSymCompArray5& I012)
{
	if(ctrl_mode == 0)   //duplicate all codes as avg ctrl
	{
		id = P_DG / V_DG; //make sure V_DG has been calc beforehand
		Iq = Q_DG / V_DG;   //change P_ref/Q_ref to P_DG/Q_DG dahei 1-16-18
		Idn = id;
		Iqn = Iq;
		Id_ref = id;// local; may need to be changed in futher
		Iq_ref = Iq;//
         //  P_ref :=  Id_ref *v_DG;//local
         //  V_ref := v_DG;//local
           /*-initiate ABCD XY-*/
		(X_var)[1 - 1] = id;
		(X_var)[2 - 1] = Iq;
		dIddt = 0;
		dIqdt = 0;
		dIddtn = 0;
		dIqdtn = 0;
		dvi1dt = 0;
		dvi1dtn = 0;
		dvi2dt = 0;
		dvi2dtn = 0;
           // the global part
	}
}
/*-------------------------------------------------------------------------------------------------------------*/

void TGeneric5Obj::InitModelVIabc(int ActorID)
{
	pComplexArray cBuffer = nullptr;
	cBuffer = (pComplexArray) realloc(cBuffer, sizeof(complex) * Fnphases);//define cBuffer
	GetPhasePower(cBuffer, ActorID);
	P_DG1 = (cBuffer)[1 - 1].re; //first phase or the only one
	Q_DG1 = (cBuffer)[1 - 1].im;
        ///
        //InitModelVIabc(@Vabc, @Iabc); //
	ID1 = P_DG1 / V_DG1;
	Iq1 = Q_DG1 / V_DG1;
         /*-initiate ABCD XY-*/
	(X_var)[1 - 1] = ID1;
	(X_var)[2 - 1] = Iq1;
	if(Fnphases == 3) //for 3 phase control the bellow is needed
	{
		P_DG2 = (cBuffer)[2 - 1].re;
		Q_dg2 = (cBuffer)[2 - 1].im;
		P_dg3 = (cBuffer)[3 - 1].re;
		Q_dg3 = (cBuffer)[3 - 1].im;
		ID2 = P_DG2 / V_DG2; //
		Iq2 = Q_dg2 / V_DG2;
		Id3 = P_dg3 / V_DG3; //
		Iq3 = Q_dg3 / V_DG3;
		(X_var)[3 - 1] = ID2;
		(X_var)[4 - 1] = Iq2;
		(X_var)[5 - 1] = Id3;
		(X_var)[6 - 1] = Iq3;
	}
	free(cBuffer);//free cBuffer
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TGeneric5Obj::InitStateVars(int ActorID)
{
	int i = 0;
	TSymCompArray5 V012 = {};
	TSymCompArray5 I012 = {};
	complex Vabc[4] = { CZero,CZero ,CZero ,CZero },
			Iabc[4] = { CZero,CZero ,CZero ,CZero };
	pComplexArray cBuffer = nullptr;

	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims
	/*# with MachineData do */
	{
		auto& with0 = MachineData;

           /*Compute nominal Positive sequence voltage behind transient reactance*/
		if(MachineON)
			/*# with ActiveCircuit[ActorID].Solution do */
			{
				auto with1 = ActiveCircuit[ActorID]->Solution;
				Yeq = cinv(Zsp);
				TDSSCktElement::ComputeIterminal(ActorID);
				switch(Fnphases)
				{
					case 	1:
                            //E1      := Csub( CSub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[2]]) , Cmul(ITerminal^[1], Zsp));
					{
						int stop = 0;
						for(stop = Fnphases, i = 1; i <= stop; i++)
						{
							if(!ADiakoptics || (ActorID == 1))
								Vabc[i - 1] = with1->NodeV[(NodeRef)[i - 1]];
							else
								Vabc[i - 1] = with1->VoltInActor1((NodeRef)[i - 1]);
						}   // Wye Voltage
					}
					break;
                       // Calculate E1 based on Pos Seq only
					case 	3:
					{
						int stop = 0;
						Phase2SymComp(&(Iterminal[0]), &I012[1]);   // terminal currents

                           // Voltage behind Zsp  (transient reactance), volts
						for(stop = Fnphases, i = 1; i <= stop; i++)
						{
							if(!ADiakoptics || (ActorID == 1))
								Vabc[i - 1] = with1->NodeV[(NodeRef)[i - 1]];
							else
								Vabc[i - 1] = with1->VoltInActor1((NodeRef)[i - 1]);
						}
						Phase2SymComp(&Vabc[0], &V012[1]);
                           //E1  := Csub( V012[1] , Cmul(I012[1], Zsp));    // Pos sequence
					}
					break;
					default:
					DoSimpleMsg(Format(("Dynamics mode is implemented only for 1- or 3-phase Motors. IndMach012." + with1->get_Name()
	           + " has %d phases.").c_str(), Fnphases), 5672);
					SolutionAbort = true;
					break;
				}
				with0.dTheta = 0.0;
				with0.w0 = TwoPi * ActiveCircuit[ActorID]->Solution->get_FFrequency();
               // recalc Mmass and D in case the frequency has changed
				/*# with MachineData do */
				{
					auto& with2 = MachineData;
					with2.Mmass = 2.0 * with2.Hmass * with2.kVArating * 1000.0 / (with2.w0);   // M = W-sec
					with2.D = with2.Dpu * with2.kVArating * 1000.0 / (with2.w0);
				}
				with0.Pshaft = 0 - Get_Power(1, ActorID).re;//P_DG;//Power[1].re; // Initialize Pshaft to present power consumption of motor

               //Speed := -LocalSlip * w0;    // relative to synch speed
				with0.dSpeed = 0.0;
               /**/
				if(DebugTrace)     // Put in a separator record
				{
					Append(Tracefile);
					IOResultToException();
					WriteLn(Tracefile);
					WriteLn(Tracefile, "*************** Entering Dynamics Mode ***********************");
					WriteLn(Tracefile);
					Close(Tracefile);
				}
			}
		else
		{
			with0.Theta = 0.0;
			with0.dTheta = 0.0;
			with0.w0 = 0;
			with0.Speed = 0.0;
			with0.dSpeed = 0.0;
               /**/
               //Id
		}
	}  /*With*/
  ///
  ///  from here, let us deal with ctrl_mode and everything  related to control
	if(ctrl_mode == 0)   //Pos seq contrl
		
               /**/
		{
			V_DG = cabs(V012[1]);// Pos Seq Control
			Theta_DG = cang(V012[1]);
               //P_DG := 0-Power[1].re/3.0; //1-terminal, for gen has only one terminal
                                     // div 3.0 ---% 1-11-2018. each phase
			P_DG = 0 - Get_Power(1, ActorID).re;
               //Q_DG := 0-Power[1].im/3.0;
			Q_DG = 0 - Get_Power(1, ActorID).im;
               //V_ref := V_DG ; //1;//
			P_ref = P_DG / 3;
			Q_ref = Q_DG / 3;
			InitModel(V012, I012); // E2, etc , Id Iq etc
               //init alpha array
//               if fmonobj <> nil then
//                      fmonobj.Init_delay_array(ndNumincluster);
               // Shaft variables
               //Theta  := Cang(E1) ;
		}
	else
   //ctrl_mode <> 0   =1,2,3,4
        //Vabc
	{
		V_DG1 = cabs(Vabc[1 - 1]);   // Save for variable calcs
		V_DG2 = cabs(Vabc[2 - 1]);
		V_DG3 = cabs(Vabc[3 - 1]);
		cBuffer = (pComplexArray) realloc(cBuffer, sizeof(complex) * Fnphases);//define cBuffer
		GetPhasePower(cBuffer, ActorID);
		P_DG1 = 0.0 - (cBuffer)[1 - 1].re; //first phase or the only one
		Q_DG1 = 0.0 - (cBuffer)[1 - 1].im;
        ///
        //InitModelVIabc(@Vabc, @Iabc); //
		ID1 = P_DG1 / V_DG1;
		Iq1 = Q_DG1 / V_DG1;
         /*-initiate ABCD XY-*/
		(X_var)[1 - 1] = ID1;
		(X_var)[2 - 1] = Iq1;
		if(Fnphases == 3) //for 3 phase control the bellow is needed
		{
			P_DG2 = 0.0 - (cBuffer)[2 - 1].re;
			Q_dg2 = 0.0 - (cBuffer)[2 - 1].im;
			P_dg3 = 0.0 - (cBuffer)[3 - 1].re;
			Q_dg3 = 0.0 - (cBuffer)[3 - 1].im;
			ID2 = P_DG2 / V_DG2; //
			Iq2 = Q_dg2 / V_DG2;
			Id3 = P_dg3 / V_DG3; //
			Iq3 = Q_dg3 / V_DG3;
			(X_var)[3 - 1] = ID2;
			(X_var)[4 - 1] = Iq2;
			(X_var)[5 - 1] = Id3;
			(X_var)[6 - 1] = Iq3;
		}
		free(cBuffer);//free cBuffer
	}
  //if QV_switch = 1 then //
    // begin
          //QV_flag := QV_flag_0;
          //QV_switch := 0;// wait next limit break
    // end;
	Update_PQlimits();
}

//----------------------------------------------------------------------------


/*A typical helper function for PC elements to assist in the computation
 of Yprim
*/

void TGeneric5Obj::CalcYPrimMatrix(TcMatrix* Ymatrix, int ActorID)
{
	complex Y = {};
	complex Yij = {};
	complex Yadder = {};
	int i = 0;
	int j = 0;
	double FreqMultiplier = 0.0;
	FYprimFreq = ActiveCircuit[ActorID]->Solution->get_FFrequency();
	FreqMultiplier = FYprimFreq / BaseFrequency;  // ratio to adjust reactances for present solution frequency
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(with0->IsDynamicModel || with0->IsHarmonicModel)
   // for Dynamics and Harmonics modes use constant equivalent Y
		{
			int stop = 0;
			if(MachineON)   // L-N value computed in initialization routines
				Y = Yeq;
			else
				Y = cmplx(EPSILON, 0.0);
			if(Connection == 1)
				Y = cdivreal(Y, 3.0); // Convert to delta impedance
			Y.im = Y.im / FreqMultiplier;  // adjust for frequency
			Yij = cnegate(Y);
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				switch(Connection)
				{
					case 	0:
					{
						Ymatrix->SetElement(i, i, Y);  // sets the element
                 /*
                   Ymatrix.AddElement(Fnconds, Fnconds, Y);  // sums the element
                   Ymatrix.SetElemsym(i, Fnconds, Yij);
                 */
					}
					break;   /*Delta connection*/
					case 	1:
					{
						int stop1 = 0;
						Yadder = cmulreal(Y, 1.000001);  // to prevent floating delta
						Ymatrix->SetElement(i, i, cadd(Y, Yadder));   // add a little bit to diagonal
						Ymatrix->AddElement(i, i, Y);  // put it in again
						for(stop1 = i - 1, j = 1; j <= stop1; j++)
						{
							Ymatrix->SetElemsym(i, j, Yij);
						}
					}
					break;
					default:
					  ;
					break;
				}
			}
		}
		else


    //  Typical code for a regular power flow  model
    //  Borrowed from Generator object
		
       /*Yeq is typically expected as the equivalent line-neutral admittance*/
		{
			Y = Yeq;  //     Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
			Y.im = Y.im / FreqMultiplier;
			switch(Connection)
			{
				case 	0:
				/*# with Ymatrix do */
				{
					auto with1 = Ymatrix; // WYE
					int stop = 0;
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						with1->SetElement(i, i, Y);
                     /*
                     AddElement(Fnconds, Fnconds, Y);
                     SetElemsym(i, Fnconds, Yij);
                     */
					}
				}
				break;
				case 	1:
				/*# with Ymatrix do */
				{
					auto with2 = Ymatrix;  // Delta  or L-L
					int stop = 0;
					Y = cdivreal(Y, 3.0); // Convert to delta impedance
					Yij = cnegate(Y);
					for(stop = Fnphases, i = 1; i <= stop; i++)
					{
						j = i + 1;
						if(j > Fnconds)
							j = 1;  // wrap around for closed connections
						with2->AddElement(i, i, Y);
						with2->AddElement(j, j, Y);
						with2->AddElemsym(i, j, Yij);
					}
				}
				break;
				default:
				  ;
				break;
			}
		}
	}  /*ELSE IF Solution.mode*/
}


//----------------------------------------------------------------------------

void TGeneric5Obj::CalcYPrim(int ActorID)
{
	int i = 0;
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


     // call helper routine to compute YPrim_Shunt
	CalcYPrimMatrix(YPrim_Shunt, ActorID);

     // Set YPrim_Series based on a small fraction of the diagonals of YPrim_shunt
     // so that CalcVoltages doesn't fail
     // This is just one of a number of possible strategies but seems to work most of the time
	for(stop = Yorder, i = 1; i <= stop; i++)
	{
		YPrim_Series->SetElement(i, i, cmulreal(YPrim_Shunt->GetElement(i, i), 1.0e-10));
	}

     // copy YPrim_shunt into YPrim; That's all that is needed for most PC Elements
	YPrim->CopyFrom(YPrim_Shunt);

     // Account for Open Conductors -- done in base class
	TDSSCktElement::CalcYPrim(ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
/*Compute total terminal Current */

void TGeneric5Obj::DoGeneric5Model(int ActorID)
{
	int i = 0;
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
	CalcModel(&(Vterminal[0]), &(Iterminal[0]), ActorID);

   //IterminalUpdated := TRUE;
	set_ITerminalUpdated(true, ActorID);
	for(stop = Get_NPhases(), i = 1; i <= stop; i++)
	{
		caccum((InjCurrent)[i - 1], cnegate((Iterminal)[i - 1]));
	}
	if(DebugTrace)
		WriteTraceRecord(ActorID);
} // given voltages returns currents

void TGeneric5Obj::CalcModel(pComplexArray V, pComplexArray i, int ActorID)
{
	TSymCompArray5 V012 = {};
	TSymCompArray5 I012 = {};
	if(ctrl_mode == 0)
        // Convert abc voltages to 012
	{
		Phase2SymComp(V, &V012[0]);

        // compute I012
		switch(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode)
		{
			case 	DYNAMICMODE:
			{
				CalcDynamic(V012, I012, ActorID);
			}
			break;  /*All other modes are power flow modes*/
			default:
			CalcPFlow(V012, I012, ActorID);
			break;
		}
		SymComp2Phase(i, (pComplexArray) &I012);       // convert back to I abc
 // avg ctrl
	}
	else
 //direct phase ctrl
	{
		if(Fnphases == 3)
                //3-phase ctrl
                        // use Vterminal Iterminal directly instead of computing 120
		{
			switch(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode)
			{
				case 	DYNAMICMODE:
				{
					CalcDynamicVIabc(V, i, ActorID);  //if ((ctrl_mode=4)and (fnphases=3))
				}
				break;  /*All other modes are power flow modes*/
				default:
				CalcPFlowVIabc(V, i, ActorID);  // //if ((ctrl_mode=4)and (fnphases=3))
				break;
			}
		}
		else
		{
            //no consideration for 2-phase DG
			if(Fnphases == 1)
                //1-phase ctrl
                          // use Vterminal Iterminal directly instead of computing 120
                          // actually there is no 120 for single phase
			{
				switch(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode)
				{
					case 	DYNAMICMODE:
					{
						CalcDynamicVIabc(V, i, ActorID);  //if (fnphases=1)
					}
					break;  /*All other modes are power flow modes*/
					default:
					CalcPFlowVIabc(V, i, ActorID);  // //if (fnphases=1)
					break;
				}
			}
		}
	}
    /*--------pullish info--------*/
	if(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode == DYNAMICMODE)
                 //dynamode
         //if ActiveCircuit.Issolved = true then
	{
		if(FMonObj != nullptr)
			InfoPublish(ActorID);
	}
	else
         //power flow
	{
		if(FMonObj != nullptr)
			InfoPublish(ActorID);
	}

    /*----------------------------*/
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/* This is an example taken from Generator illustrating how a PC element might
  handle Dynamics mode with a Thevenin equivalent

  Also illustrates the computation of symmetrical component values
*/

/*Compute Total Current and add into InjTemp*/

void TGeneric5Obj::DoDynamicMode(int ActorID)
{
	int i = 0;

   // Start off by getting the current in the admittance branch of the model
	int stop = 0;
	CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array

   /*Inj = -Itotal (in) - Yprim*Vtemp*/
	CalcModel(&(Vterminal[0]), &(Iterminal[0]), ActorID);

   //IterminalUpdated := TRUE;
	set_ITerminalUpdated(true, ActorID);
	for(stop = Get_NPhases(), i = 1; i <= stop; i++)
	{
		caccum((InjCurrent)[i - 1], cnegate((Iterminal)[i - 1]));
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
/*Do not support Harmonic for now*/

/*
  Example taken from Generator illustrating how a PC element might handle
  current calcs for Harmonics mode

  Note: Generator objects assume a Thevenin model (voltage behind and impedance)
        while Load objects assume the Spectrum applies to a Norton model injection current
*/

/*Compute Injection Current Only when in harmonics mode*/

/*Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built*/
/*Vd is the fundamental frequency voltage behind Xd" for phase 1*/

void TGeneric5Obj::DoHarmonicMode(int ActorID)
{
	int i = 0;
	complex e = {};
	double GenHarmonic = 0.0;

   // Set the VTerminal array
	ComputeVterminal(ActorID);
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		int stop = 0;
		GenHarmonic = with0->get_FFrequency() / BaseFrequency; // harmonic based on the fundamental for this object
        // get the spectrum multiplier and multiply by the V thev (or Norton current for load objects)
      // ???  E := CmulReal(SpectrumObj.GetMult(GenHarmonic), VThevHarm); // Get base harmonic magnitude
      // ???  RotatePhasorRad(E, GenHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift

        // Put the values in a temp complex buffer
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			cBuffer[i - 1] = e;
			if(i < Fnphases)
				RotatePhasorDeg(e, GenHarmonic, -120.0);  // Assume 3-phase IndMach012
		}
	}

   /*Handle Wye Connection*/
	if(Connection == 0)
		cBuffer[Fnconds - 1] = (Vterminal)[Fnconds - 1];  // assume no neutral injection voltage

   // In this case the injection currents are simply Yprim(frequency) times the voltage buffer
   // Refer to Load.Pas for load-type objects
		   /*Inj currents = Yprim (E) */
	YPrim->MVmult(InjCurrent, (pComplexArray) &cBuffer);
}




// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -


// Main dispatcher for computing PC Element currnts

// Calculates IndMach012 current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

void TGeneric5Obj::CalcGeneric5ModelContribution(int ActorID)
{

  //IterminalUpdated := FALSE;
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
				DoGeneric5Model(ActorID);
		}
	} /*WITH*/

   /*When this is done, ITerminal is up to date*/
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

// Main procedure for controlling computation of InjCurrent array

// InjCurrent is difference between currents in YPrim and total terminal current

void TGeneric5Obj::CalcInjCurrentArray(int ActorID)
{


// You usually will want some logic like this

       // If the element is open, just zero the array and return
	if(Generic5SwitchOpen)

       // otherwise, go to a routine that manages the calculation
		ZeroInjCurrent();
	else
		CalcGeneric5ModelContribution(ActorID);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

// This function controls the calculation of the total terminal currents

// Note that it only does something if the solution count has changed.
// Otherwise, Iterminal array already contains the currents

void TGeneric5Obj::GetTerminalCurrents(pComplexArray Curr, int ActorID)
{
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		if(IterminalSolutionCount[ActorID] != ActiveCircuit[ActorID]->Solution->SolutionCount)     // recalc the contribution
          // You will likely want some logic like this
		{
			if(!Generic5SwitchOpen)
				CalcGeneric5ModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
		}
		inherited::GetTerminalCurrents(Curr, ActorID); // add in inherited contribution
	}
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

// Required function for managing computing of InjCurrents

int TGeneric5Obj::InjCurrents(int ActorID)
{
	int result = 0;
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;

      // Generators and Loads use logic like this:
		if(with0->LoadsNeedUpdating)
			SetNominalPower(ActorID); // Set the nominal kW, etc for the type of solution being done

       // call the main function for doing calculation
		CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current

      // If (DebugTrace) Then WriteTraceRecord;

       // Add into System Injection Current Array
		result = inherited::InjCurrents(ActorID);
	}
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// Set shaft power

void TGeneric5Obj::SetNominalPower(int ActorID)
{
	double Factor = 0.0;
	bool MachineOn_Saved = false;
	MachineOn_Saved = MachineON;
	ShapeFactor = CDoubleOne;
    // Check to make sure the generation is ON
	/*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
	{
		
		auto with1 = ActiveCircuit[ActorID]->Solution;
		if(!(with1->IsDynamicModel || with1->IsHarmonicModel))     // Leave machine in whatever state it was prior to entering Dynamic mode
		{
			MachineON = true;   // Init to on then check if it should be off
		}
		if(!MachineON)
         // If Machine is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
		{
			MachineData.Pnominalperphase = -0.1 * kWBase / Fnphases;
          // Pnominalperphase   := 0.0;
			MachineData.Qnominalperphase = 0.0;   // This really doesn't matter
		}
		else
    // Generator is on, compute it's nominal watts and vars
		{
			/*# with Solution do */
			{
				auto with2 = ActiveCircuit[ActorID]->Solution;
				switch(with2->Get_SolMode())
				{
					case 	SNAPSHOT:
					Factor = 1.0;
					break;
					case 	DAILYMODE:
					{
						Factor = 1.0; // Daily dispatch curve
						CalcDailyMult(with2->DynaVars.dblHour);
					}
					break;
					case 	YEARLYMODE:
					{
						Factor = 1.0;
						CalcYearlyMult(with2->DynaVars.dblHour);
					}
					break;
					case 	DUTYCYCLE:
					{
						Factor = 1.0;
						CalcDutyMult(with2->DynaVars.dblHour);
					}
					break;   // General sequential time simulation
					case 	GENERALTIME: case DYNAMICMODE:
					{
						Factor = 1.0;
                                   // This mode allows use of one class of load shape
						switch(ActiveCircuit[ActorID]->ActiveLoadShapeClass)
						{
							case 	USEDAILY:
							CalcDailyMult(with2->DynaVars.dblHour);
							break;
							case 	USEYEARLY:
							CalcYearlyMult(with2->DynaVars.dblHour);
							break;
							case 	USEDUTY:
							CalcDutyMult(with2->DynaVars.dblHour);
							break;
							default:     // default to 1 + j1 if not known
							ShapeFactor = CDoubleOne;
							break;
						}
					}
					break;
					case 	MONTECARLO1: case MONTEFAULT: case FAULTSTUDY:
					Factor = 1.0;
					break;
					case 	MONTECARLO2: case MONTECARLO3: case LOADDURATION1: case LOADDURATION2:
					{
						Factor = 1.0;
						CalcDailyMult(with2->DynaVars.dblHour);
					}
					break;
					case 	PEAKDAY:
					{
						Factor = 1.0;
						CalcDailyMult(with2->DynaVars.dblHour);
					}
					break;
					case 	AUTOADDFLAG:
					Factor = 1.0;
					break;
					default:
					Factor = 1.0;
					break;
				}
			}
			if(!(with1->IsDynamicModel || with1->IsHarmonicModel))         //******
			{
				if(ShapeIsActual)
					MachineData.Pnominalperphase = 1000.0 * ShapeFactor.re / Fnphases;
				else
					MachineData.Pnominalperphase = 1000.0 * kWBase * Factor * ShapeFactor.re / Fnphases;

                // cannot dispatch vars in induction machine
                // you get what you get
			}
		} /*ELSE GenON*/
	}  /*With ActiveCircuit*/

   // If machine state changes, force re-calc of Y matrix
	if(MachineON != MachineOn_Saved)
		Set_YprimInvalid(ActorID,true);
}

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

void TGeneric5Obj::CalcDailyMult(double hr)
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

//----------------------------------------------------------------------------

void TGeneric5Obj::CalcDutyMult(double hr)
{
	if(DutyShapeObj != nullptr)
	{
		ShapeFactor = DutyShapeObj->GetMult(hr);
		ShapeIsActual = DutyShapeObj->UseActual;
	}
	else
	CalcDailyMult(hr);  // Default to Daily Mult if no duty curve specified
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TGeneric5Obj::CalcYearlyMult(double hr)
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

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------

// Gets the currents for the last solution performed

// Do not call anything that may change the basic element values from the last solution

void TGeneric5Obj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	CalcInjCurrentArray(ActorID);  // Difference between currents in YPrim and total current
	try
    // an exception here generally means an array boundary overrun
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
		DoErrorMsg(String("IndMach012 Object: \"") + get_Name() + "\" in GetInjCurrents function.", (std::string) e.what(), "Current buffer not big enough.", 568);
	}
}
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------
/*
 This procedure is require to respond to various commands such as Dump that
 write all the device's property values to a file.
*/

void TGeneric5Obj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int Idx = 0;
	inherited::DumpProperties(f, Complete);

    /*Write out any specials here, usually preceded by a "!"*/
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			Idx = (with0->PropertyIdxMap)[i - 1]; // Map to get proper index into property value array
			switch(Idx)
			{
				case 	34: case 36:
          /*Trap any specials here, such as values that are array properties, for example*/
				{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, "=("); Write(f, Get_PropertyValue(Idx)); WriteLn(f, L')'); }
				break;
				default:
				{ Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(Idx)); }
				break;
			}
		}
	}
	WriteLn(f);
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/*Procedure to initialize for Harmonics solution*/

/*This example is extracted from Generator and constructs a Thevinen equivalent.
 Refer to Load for how to do a Norton equivalent
 */

//Var
//  E, Va:complex;

void TGeneric5Obj::InitHarmonics(int ActorID)
{
	Set_YprimInvalid(ActorID,true);  // Force rebuild of YPrims

/****
     GenFundamental := ActiveCircuit.Solution.Frequency ;  // Whatever the frequency is when we enter here.

     With GenVars Do Begin

         // Xd" is used for harmonics analysis for generators
         Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

         {Compute reference Thevinen voltage from phase 1 current}

         IF GenON Then
           Begin

             ComputeIterminal;  // Get present value of current

             With ActiveCircuit.solution Do
             Case Connection of
               0: Begin {wye - neutral is explicit}
                    Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]]);
                  End;
               1: Begin  {delta -- assume neutral is at zero}
                    Va := NodeV^[NodeRef^[1]];
                  End;
             End;

             E         := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
             Vthevharm := Cabs(E);   // establish base mag and angle
             ThetaHarm := Cang(E);
           End
         ELSE  Begin
           // If Generator is off, just set to zero
             Vthevharm := 0.0;
             ThetaHarm := 0.0;
         End;
     End;
 ***/
}

// ******************* PROPERTY VALUES   *******************

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// required procedure to initialize the string value of the properties

void TGeneric5Obj::InitPropertyValues(int ArrayOffset)
{

   // Some examples
	Set_PropertyValue(1,"3");        //'phases';
	Set_PropertyValue(2,GetBus(1));  //'bus1';
	Set_PropertyValue(3,"12.47");
	Set_PropertyValue(4,"100");
	Set_PropertyValue(5,".80");
	Set_PropertyValue(6,"Delta");
	Set_PropertyValue(7,Format("%-g", MachineData.kVArating));
	Set_PropertyValue(8,Format("%-g", MachineData.Hmass));
	Set_PropertyValue(9,Format("%-g", MachineData.D));
	Set_PropertyValue(10,"0.0053");
	Set_PropertyValue(11,"0.106");
	Set_PropertyValue(12,"0.007");
	Set_PropertyValue(13,"0.12");
	Set_PropertyValue(14,"4.0");
	Set_PropertyValue(15,"0.007");
	Set_PropertyValue(16,"0.1");
	Set_PropertyValue(17,"variable");
	Set_PropertyValue(18,"");
	Set_PropertyValue(19,"");
	Set_PropertyValue(20,"");     /*...*/
	Set_PropertyValue(21,"NO");
	Set_PropertyValue(24,"1");//GrpNum
	Set_PropertyValue(25,"1");//V_ref
	Set_PropertyValue(26,"0");//control mode

/*Call inherited function to init inherited property values*/
	inherited::InitPropertyValues(NumPropsThisClass);
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// Return i-th property value as a string

String TGeneric5Obj::GetPropertyValue(int Index)
{
	String result;
	result = "";   // Init the string
	switch(Index)
	{
		case 	4:
         // Put special cases here
         // often a good idea to convert numeric values to strings, for example
		result = Format("%.6g", kWBase);
		break;
		case 	5:
		result = Format("%.6g", PowerFactor(Get_Power(1, ActiveActor)));
		break;
		case 	7:
		result = Format("%.6g", MachineData.kVArating);
		break;
		case 	8:
		result = Format("%.6g", MachineData.Hmass);
		break;
		case 	9:
		result = Format("%.6g", MachineData.D);
		break;
         //15:  Result := Format('%.6g', [localslip]);
		case 	18:
		result = YearlyShape;
		break;
		case 	19:
		result = DailyDispShape;
		break;
		case 	20:
		result = DutyShape;
		break;
         /**/
		case 	24:
		result = Format("%d", Cluster_num);
		break;
         /*...*/

         // The default is to just return the current string value of the property
		default:
		result = TDSSCktElement::GetPropertyValue(Index);
		break;
	}
	return result;
}

// ******************* END PROPERTY VALUES   *******************



//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/*
  This is a virtual function. You do not need to write this routine
  if you are not integrating state variables in dynamics mode.
*/

// Integrate state variables for Dynamics analysis
// Example from Generator

// Illustrates use of debug tracing

// Present technique is a predictor-corrector trapezoidal rule

void TGeneric5Obj::IntegrateStates(int ActorID)
{
	complex TracePower = {};
   // Compute Derivatives and then integrate
	TDSSCktElement::ComputeIterminal(ActorID);
	/*# with ActiveCircuit[ActorID].Solution, MachineData do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
		auto& with1 = MachineData;
		/*# with DynaVars do */
		{
			auto& with2 = with0->DynaVars; /*First iteration of new time step*/
          //ThetaHistory := Theta + 0.5*h*dTheta;
          //SpeedHistory := Speed + 0.5*h*dSpeed;
			if(with2.IterationFlag == 0)
				;
		}

      // Compute shaft dynamics
		TracePower = TerminalPowerIn(&(Vterminal[0]), &(Iterminal[0]), Fnphases); // in watts
      //dSpeed := (TracePower.re - Pshaft - abs(D*Speed)) / Mmass;
      //dTheta  := Speed ;
		with1.Pshaft = P_DG;  // P_DG is calculated in CalcDynamic or CalcDynamicVIabc
                       //
     // Trapezoidal method

       //Speed := SpeedHistory + 0.5*h*dSpeed;
       //Theta := ThetaHistory + 0.5*h*dTheta;
		if(DebugTrace)
			WriteTraceRecord(ActorID);
		IntegrateABCD(ActorID);
      //Integrate;
	}
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TGeneric5Obj::Get_DynamicModelCurrent()
{
	double Temp = 0.0;
	if(id == 0.0)
		Temp = double(DSSGlobals::PI) / 2;
	else
		Temp = atan(Iq / id);
	Is1 = pclx(sqrt(Iq * Iq + id * id), Theta_DG - Temp);//with respect to Q_axis
    //Is1 := cmul(Is1,cmplex());// Put this into XY domine
	Is1 = cdivreal(Is1, 3.0);     //here we need to divide all values back to Network
    //Is1 := Cdiv(Csub(V1, E1),Zsp); // I = (V-E')/Z'
    //Is2 := Cdiv(Csub(V2, E2),Zsp); // I = (V-E')/Z'
	Is2 = cmplx(0, 0); //force balance
    // rotor current  Ir1= Is1-Vm/jXm
	Ir1 = Is1;
    //Ir1 := Csub(Is1 ,Cdiv( Csub(V1, cmul(Is1, Zsp)), Zm ));
    //Ir2 := Csub(Is2 ,Cdiv( Csub(V2, cmul(Is2, Zsp)), Zm ));
	Ir2 = cmplx(0, 0);
}


//----------------------------------------------------------------------------
// ********************** VARIABLES ***************************************
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
/*
  Return the number of state variables

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
  Note: it is not necessary to define any state variables
*/

int TGeneric5Obj::NumVariables()
{
	int result = 0;
	result = NumGeneric5Variables;
	return result;
}


//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/*
  Returns the i-th state variable in a string

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
*/

String TGeneric5Obj::VariableName(int i)
{
	String result;
	if(i < 1)
		return result;  // This means Someone goofed
	switch(i)
	{
		case 	1:
		result = "V_DG";
		break;//pos seq value
		case 	2:
		result = "P_DG";
		break;
		case 	3:
		result = "Q_DG";
		break;
		case 	4:
		result = "V_DG1";
		break;//Phase A or the first phase if there are less than 3
		case 	5:
		result = "P_DG1";
		break;
		case 	6:
		result = "Q_DG1";
		break;
		case 	7:
		result = "V_DG2";
		break;//Phase B if exists
		case 	8:
		result = "P_DG2";
		break;
		case 	9:
		result = "Q_DG2";
		break;
		case 	10:
		result = "V_DG3";
		break;//phase C if exists
		case 	11:
		result = "P_DG3";
		break;
		case 	12:
		result = "Q_DG3";
		break;
		case 	13:
		result = "Qmax";
		break;
		case 	14:
		result = "Qmax_Phase";
		break;
		case 	15:
		result = "Pmax";
		break;
		case 	16:
		result = "Pmax_Phase";
		break;
		case 	17:
		result = "Alpha";
		break;
		case 	18:
		result = "Alpha1";
		break;
		case 	19:
		result = "Alpha2";
		break;
		case 	20:
		result = "Alpha3";
		break;
		case 	21:
		result = "AlphaP";
		break;
		case 	22:
		result = "AlphaP1";
		break;
		case 	23:
		result = "AlphaP2";
		break;
		case 	24:
		result = "AlphaP3";
		break;
		case 	25:
		result = "V_ref";
		break; //Voltage object
		case 	26:
		result = "kVA";
		break; //kVArating
		case 	27:
		result = "kW";
		break; //kVArating
		case 	28:
		result = "cluster_num";
		break;
		case 	29:
		result = "NdNumInCluster";
		break;
		case 	30:
		result = "ctrl_mode";
		break;
		case 	31:
		result = "Gradient";
		break;
		case 	32:
		result = "Id";
		break;
		case 	33:
		result = "Iq";
		break;
		case 	34:
		result = "P_set";
		break;
		case 	35:
		result = "Frequency";
		break;
		case 	36:
		result = "Defense";
		break;
		default:
		  ;
		break;
	}
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

double TGeneric5Obj::Get_Variable(int i)
{
	double result = 0.0;
	result = -9999.99;   // Error Value

    //With MachineData Do
	switch(i)
	{
		case 	1:
		result = V_DG;
		break;
		case 	2:
		result = P_DG / 1000;
		break;//kW
		case 	3:
		result = Q_DG / 1000;
		break;
		case 	4:
		result = V_DG1;
		break;//Phase A or the first phase if there are less than 3 phases
		case 	5:
		result = P_DG1 / 1000;
		break;
		case 	6:
		result = Q_DG1 / 1000;
		break;
		case 	7:
		result = V_DG2;
		break;//Phase B if exists
		case 	8:
		result = P_DG2 / 1000;
		break;
		case 	9:
		result = Q_dg2 / 1000;
		break;
		case 	10:
		result = V_DG3;
		break;//Phase c if exists
		case 	11:
		result = P_dg3 / 1000;
		break;
		case 	12:
		result = Q_dg3 / 1000;
		break;
		case 	13:
		result = Qmax / 1000;
		break;
		case 	14:
		result = Qmax_phase / 1000;
		break;
		case 	15:
		result = PMax / 1000;
		break;
		case 	16:
		result = PMax_phase / 1000;
		break;
		case 	17:
		result = Alpha;
		break;
		case 	18:
		result = Alpha1;
		break;
		case 	19:
		result = Alpha2;
		break;
		case 	20:
		result = Alpha3;
		break;
		case 	21:
		result = AlphaP;
		break;
		case 	22:
		result = AlphaP1;
		break;
		case 	23:
		result = AlphaP2;
		break;
		case 	24:
		result = AlphaP3;
		break;
		case 	25:
		result = V_ref;
		break;
		case 	26:
		result = MachineData.kVArating / 1000;
		break;
		case 	27:
		result = kWBase / 1000;
		break;
		case 	28:
		result = (double) Cluster_num;
		break;
		case 	29:
		result = (double) NdNumInCluster + 1;
		break;
		case 	30:
		result = (double) ctrl_mode;
		break;
		case 	31:
		result = Gradient;
		break;
		case 	32:
		result = id;
		break;
		case 	33:
		result = Iq;
		break;
		case 	34:
		result = P_ref * 3.0;
		break;
		case 	35:
		{
			Freq = ActiveCircuit[ActiveActor]->Solution->get_FFrequency();
			if(FMonObj != nullptr)
				Freq = Freq + FMonObj->omg_fm;//fmonobj.comp_omg;//fmonobj.omg_fm;  //
			result = Freq;
		}
		break;
		case 	36:
		{
			result = 0.0;
			if(FMonObj != nullptr)
				result = z_dfs_plot;
		}
		break;
		default:
		  ;
		break;
	}
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TGeneric5Obj::Set_Variable(int i, double Value)
{
	switch(i)
	{
		case 	1:
		V_DG = Value;
		break;
		case 	2:
		P_DG = Value;
		break;
		case 	3:
		Q_DG = Value;
		break;
		case 	4:
		V_DG1 = Value;
		break;//Phase A or the first phase if there are less than 3 phases
		case 	5:
		P_DG1 = Value;
		break;
		case 	6:
		Q_DG1 = Value;
		break;
		case 	7:
		V_DG2 = Value;
		break;//Phase B if exists
		case 	8:
		P_DG2 = Value;
		break;
		case 	9:
		Q_dg2 = Value;
		break;
		case 	10:
		V_DG3 = Value;
		break;//Phase c if exists
		case 	11:
		P_dg3 = Value;
		break;
		case 	12:
		Q_dg3 = Value;
		break;
		case 	13:
		Qmax = Value;
		break;
		case 	14:
		Qmax_phase = Value;
		break;
		case 	15:
		PMax = Value;
		break;
		case 	16:
		PMax_phase = Value;
		break;
		case 	17:
		Alpha = Value;
		break;
		case 	18:
		Alpha1 = Value;
		break;
		case 	19:
		Alpha2 = Value;
		break;
		case 	20:
		Alpha3 = Value;
		break;
		case 	21:
		AlphaP = Value;
		break;
		case 	22:
		AlphaP1 = Value;
		break;
		case 	23:
		AlphaP2 = Value;
		break;
		case 	24:
		AlphaP3 = Value;
		break;
		case 	25:
		V_ref = Value;
		break;
		case 	26:
		MachineData.kVArating = Value;
		break;
		case 	27:
		kWBase = Value;
		break;
		case 	28:
		{
			((TPCElement*) this)->Cluster_num = Trunc(Value);


          //if cluster_num >= 1 then      // assign the virtue leader to this DG
          //begin
              //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num); // it works only if cluster_num starts from 1 and being consecutively

              // move this piece of codes to Fmonitor, InitFM
              //ActiveCircuit.Fmonitors.First;
              //FMonObj := ActiveCircuit.Fmonitors.Active;
              //if FMonObj. then


          //end;

     //if function 'get' fails , return nil
		}
		break;
		case 	29:
		((TPCElement*) this)->NdNumInCluster = Trunc(Value) - 1;
		break;
		case 	30:
		((TPCElement*) this)->nVLeaders = Trunc(Value);
		break;
		case 	31:
		((TPCElement*) this)->cluster_num2 = Trunc(Value);
		break;
		case 	32:
		((TPCElement*) this)->NdNumInCluster2 = Trunc(Value) - 1;
		break;
        /*Do Nothing for other variables: they are read only*/
		default:
		  ;
		break;
	}
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
/*
  Return all state variables in double array (allocated by calling function)

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
*/

void TGeneric5Obj::GetAllVariables(pDoubleArray States)
{
	int i = 0;
    //N:Integer;

//     N := 0;
	int stop = 0;
	for(stop = NumGeneric5Variables, i = 1; i <= stop; i++)
	{
		(States)[i - 1] = Get_Variable(i);
	}
}

// ********************** END VARIABLES ***************************************




//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

double TGeneric5Obj::GetRotorLosses()
{
	double result = 0.0;
	result = 3.0 * (Sqr(Ir1.re) + Sqr(Ir1.im) + Sqr(Ir2.re) + Sqr(Ir2.im)) * Zr.re;
	return result;
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

double TGeneric5Obj::GetStatorLosses()
{
	double result = 0.0;
	result = 3.0 * (Sqr(Is1.re) + Sqr(Is1.im) + Sqr(Is2.re) + Sqr(Is2.im)) * Zs.re;
	return result;
}



//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

/*
  This is a virtual function. You do not need to write this routine
  if the base class function will suffice.
*/

// Routine to convert existing three-phase models to a single-phase positive-
// sequence model

void TGeneric5Obj::MakePosSequence(int ActorID)
{
	String s;
//    V :Double;
	

/*
     The usual technique is to create a new property editing string
     based on the present values of properties. Once the string is
     created, it is pushed into the Parser and the Edit routine for this
     class is invoked.

     Thus, the positive sequence model is created in memory. Do a
     "Save Circuit" command to save the model that is created. Some
     editing of the resulting scripts will likely be required. Not all
     elements have an obvious positive sequence equivalent.
*/


 // example from Generator class
 // Modify as necessary
	s = "Phases=1 conn=wye";    // Positive sequence model is 1-phase wye

  /****

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0) Then   V :=  GenVars.kVGeneratorBase/SQRT3
  Else V :=  GenVars.kVGeneratorBase;

  S := S + Format(' kV=%-.5g',[V]);

  // Divide the load by no. phases
  If Fnphases>1 Then
  Begin
      S := S + Format(' kW=%-.5g  PF=%-.5g',[kWbase/Fnphases, PFNominal]);
      If (PrpSequence^[19]<>0) or (PrpSequence^[20]<>0) Then S := S + Format(' maxkvar=%-.5g  minkvar=%-.5g',[kvarmax/Fnphases, kvarmin/Fnphases]);
      If PrpSequence^[26]>0 Then S := S + Format(' kva=%-.5g  ',[genvars.kvarating/Fnphases]);
      If PrpSequence^[27]>0 Then S := S + Format(' MVA=%-.5g  ',[genvars.kvarating/1000.0/Fnphases]);
  End;

  Parser.CmdString := S;   // Push the string into the Parser object
  Edit;    // Invoke the Edit method for this class

  inherited;  // sets the terminal bus references, must do after editing number of phases

  ***/
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

// Routine for handling Open/Close procedures

void TGeneric5Obj::Set_ConductorClosed(int Index, int ActorID, bool Value)
{
	TDSSCktElement::Set_ConductorClosed(Index, ActorID, Value);
	if(Value)
		Generic5SwitchOpen = false;
	else
		Generic5SwitchOpen = true;
}


//----------------------------------------------------------------------------
/*procedure TGeneric5Obj.set_Localslip(const Value: Double);
//----------------------------------------------------------------------------

  Function Sign(const x:Double):Double;
  Begin If x<0.0 then Result := -1.0 Else Result := 1.0; End;

begin
     S1 := Value;
     If Not InDynamics Then If Abs(S1)>MaxSlip Then S1 := Sign(S1)*MaxSlip;   // Put limits on the slip  unless dynamics
     S2 := 2.0 - S1;
end;
 */
//----------------------------------------------------------------------------
/*procedure TGeneric5Obj.Set_Slip(const Value: Double);
//----------------------------------------------------------------------------
begin
        LocalSlip := Value;
        MachineData.Speed := MachineData.w0 *  (-S1); // make motor speed agree
end;
*/
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TGeneric5Obj::InitTraceFile()
{
	AssignFile(Tracefile, get_Name() + "_IndMach012_Trace.CSV");
	Rewrite(Tracefile);
	IOResultToException();
	Write(Tracefile, "Time, Iteration, S1, |IS1|, |IS2|, |E1|, |dE1dt|, |E2|, |dE2dt|, |V1|, |V2|, Pshaft, Pin, Speed, dSpeed");
	WriteLn(Tracefile);
	CloseFile(Tracefile);
}

//----------------------------------------------------------------------------

//----------------------------------------------------------------------------

void TGeneric5Obj::WriteTraceRecord(int ActorID)
{
	Append(Tracefile);
	IOResultToException();
	/*# with ActiveCircuit[ActorID].Solution do */
	{
		auto with0 = ActiveCircuit[ActorID]->Solution;
      //Write(TraceFile, Format('%-.6g, %d, %-.6g, ',[Dynavars.dblHour*3600.0, Iteration, S1]));
		Write(Tracefile, Format("%-.6g, %-.6g, ", cabs(Is1), cabs(Is2)));
	}
      //Write(TraceFile, Format('%-.6g, %-.6g, %-.6g, %-.6g, ', [Cabs(E1), Cabs(dE1dt), Cabs(E2), Cabs(dE2dt)]));
	Write(Tracefile, Format("%-.6g, %-.6g, ", cabs(V1), cabs(V2)));
	Write(Tracefile, Format("%-.6g, %-.6g, ", MachineData.Pshaft, Get_Power(1, ActorID).re));
	Write(Tracefile, Format("%-.6g, %-.6g, ", MachineData.Speed, MachineData.dSpeed));
	WriteLn(Tracefile);
	CloseFile(Tracefile);
}

// Initialize any variables here


  // For Example:  1 + j 1


void Generic5OrderMach_initialization()
{
	CDoubleOne = cmplx(1.0, 1.0);
}

		class 		Generic5OrderMach_unit
		{
		public:
		Generic5OrderMach_unit()
		{
			//AssertSystemInitialization();
			Generic5OrderMach_initialization();
		}
		};
		Generic5OrderMach_unit _Generic5OrderMach_unit;

}  // namespace Generic5OrderMach




