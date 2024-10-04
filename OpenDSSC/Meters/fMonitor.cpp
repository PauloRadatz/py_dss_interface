
#pragma hdrstop

#include "fMonitor.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "CktTree.h"
#include "CktElement.h"
#include "Transformer.h"
#include "PCElement.h"
#include "PDElement.h"
#include "Ucmatrix.h"
#include "ShowResults.h"
#include "mathutil.h"
#include "TOPExport.h"
#include "Dynamics.h"
#include "Pstcalc.h"
#include "Terminal.h"
#include "Generic5OrderMach.h"
#include "Capacitor.h"
#include "Load.h"
#include "Utilities.h"

using namespace std;
using namespace Arraydef;
using namespace Capacitor;
using namespace Circuit;
using namespace CktElement;
using namespace CktTree;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Dynamics;
using namespace Generic5OrderMach;
using namespace LD_fm_infos;
using namespace Load;
using namespace MeterClass;
using namespace MeterElement;
using namespace PCElement;
using namespace PDELement;
using namespace ParserDel;
using namespace Pstcalc;
using namespace System;
using namespace TOPExport;
using namespace Transformer;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace VLNodeVars;
using namespace mathutil;
using namespace Utilities;

namespace Fmonitor
{

TFMonitorObj::TFMonitorObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass) {}
TFMonitorObj::TFMonitorObj(String ClassName) : inherited(ClassName) {}
TFMonitorObj::TFMonitorObj() {}


TFMonitorObj* ActiveFMonitorObj = nullptr;
    /**/
const int SEQUENCEMASK = 17;
const int MAGNITUDEMASK = 32;
const int POSSEQONLYMASK = 64;
const int ModeMask = 15;
const int NumPropsThisClass = 28;//22;//21;//20;//17; //12;// 9; //8;//7;//add P_ref_one
const int NumSolutionVars = 12;
TFMonitorStrBuffer StrBuffer;

/*--------------------------------------------------------------------------*/  // Creates superstructure for all Monitor objects

TDSSFMonitor::TDSSFMonitor()
{
	;
	Class_Name = "FMonitor";
	DSSClassType = DSSClassType + FMON_ELEMENT;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

/*--------------------------------------------------------------------------*/

TDSSFMonitor::~TDSSFMonitor()
{
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TDSSFMonitor::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();

     // Define Property names
	PropertyName[1 - 1] = "element";
	PropertyName[2 - 1] = "terminal";
	PropertyName[3 - 1] = "mode";
	PropertyName[4 - 1] = "action";  // buffer=clear|save
	PropertyName[5 - 1] = "residual";  // buffer=clear|save
	PropertyName[6 - 1] = "VIPolar";  // V I in mag and angle rather then re and im
	PropertyName[7 - 1] = "PPolar";  // Power in power PF rather then power and vars
	PropertyName[8 - 1] = "P_trans_ref";
	PropertyName[9 - 1] = "V_Sensor";
	PropertyName[10 - 1] = "P_Sensor";
	PropertyName[11 - 1] = "Node_num";
	PropertyName[12 - 1] = "Cluster_num";
	PropertyName[13 - 1] = "Total_Clusters";
	PropertyName[14 - 1] = "Nodes";
	PropertyName[15 - 1] = "CommVector";
	PropertyName[16 - 1] = "ElemTableLine";
	PropertyName[17 - 1] = "P_Mode";  //real power control mode
	PropertyName[18 - 1] = "CommDelayVector";
	PropertyName[19 - 1] = "T_intvl_smpl";  //real power control mode
	PropertyName[20 - 1] = "MaxLocalMem";
	PropertyName[21 - 1] = "Volt_limits_pu";// set limits for this cluster {0,1.05, 0.95}
	PropertyName[22 - 1] = "b_Curt_Ctrl";// set P curtailment on/off
	PropertyName[23 - 1] = "up_dly";// delay time to communicate to upper level
	PropertyName[24 - 1] = "virtual_ld_node";// delay time to communicate to upper level
	PropertyName[25 - 1] = "EGen";//equivalent generator: Egen = {kVA, M, D, Tau, K_i}
	PropertyName[26 - 1] = "attack_defense"; // define attack and defense:  attack_defense = {atk = true , dfs = false , atk_time = 0.5 , atk_node_num = 1 , d_atk0 = 0.1 , beta_dfs = 30, D_beta = 1}
	PropertyName[27 - 1] = "Comm_hide"; // define attack and defense:  attack_defense = {atk = true , dfs = false , atk_time = 0.5 , atk_node_num = 1 , d_atk0 = 0.1 , beta_dfs = 30, D_beta = 1}
	PropertyName[28 - 1] = "Comm_node_hide"; // define attack and defense:  attack_defense = {atk = true , dfs = false , atk_time = 0.5 , atk_node_num = 1 , d_atk0 = 0.1 , beta_dfs = 30, D_beta = 1}
	PropertyHelp[1 - 1] = "Name (Full Object name) of element to which the monitor is connected.";
	PropertyHelp[2 - 1] = "Number of the terminal of the circuit element to which the monitor is connected. "
	           "1 or 2, typically. For monitoring states, attach monitor to terminal 1.";
	PropertyHelp[3 - 1] = String("Bitmask integer designating the values the monitor is to capture: ") + CRLF
	           + "0 = Voltages and currents"
	           + CRLF
	           + "1 = Powers"
	           + CRLF
	           + "2 = Tap Position (Transformers only)"
	           + CRLF
	           + "3 = State Variables (PCElements only)"
	           + CRLF
	           + "4 = Flicker level and severity index (Pst) for voltages. No adders apply."
	           + CRLF
	           + "    Flicker level at simulation time step, Pst at 10-minute time step."
	           + CRLF
	           + "5 = Solution variables (Iterations, etc)."
	           + CRLF
	           + CRLF
	           + "Normally, these would be actual phasor quantities from solution."
	           + CRLF
	           + "6 = Capacitor Switching (Capacitors only)"
	           + CRLF
	           + "Combine with adders below to achieve other results for terminal quantities:"
	           + CRLF
	           + "+16 = Sequence quantities"
	           + CRLF
	           + "+32 = Magnitude only"
	           + CRLF
	           + "+64 = Positive sequence only or avg of all phases"
	           + CRLF
	           + CRLF
	           + "Mix adder to obtain desired results. For example:"
	           + CRLF
	           + "Mode=112 will save positive sequence voltage and current magnitudes only"
	           + CRLF
	           + "Mode=48 will save all sequence voltages and currents, but magnitude only.";
	PropertyHelp[4 - 1] = String("{Clear | Save | Take | Process}") + CRLF
	           + "(C)lears or (S)aves current buffer."
	           + CRLF
	           + "(T)ake action takes a sample."
	           + CRLF
	           + "(P)rocesses the data taken so far (e.g. Pst for mode 4)."
	           + CRLF
	           + CRLF
	           + "Note that monitors are automatically reset (cleared) when the Set Mode= command is issued. "
	           + "Otherwise, the user must explicitly reset all monitors (reset monitors command) or individual "
	           + "monitors with the Clear action.";
	PropertyHelp[5 - 1] = "{Yes/True | No/False} Default = No.  Include Residual cbannel (sum of all phases) for voltage and current. "
	           "Does not apply to sequence quantity modes or power modes.";
	PropertyHelp[6 - 1] = "{Yes/True | No/False} Default = YES. Report voltage and current in polar form (Mag/Angle). (default)  Otherwise, it will be real and imaginary.";
	PropertyHelp[7 - 1] = "{Yes/True | No/False} Default = YES. Report power in Apparent power, S, in polar form (Mag/Angle).(default)  Otherwise, is P and Q";
	PropertyHelp[8 - 1] = "P_trans_ref: P ref value for metered element(unit kW)";
	PropertyHelp[9 - 1] = String("V_Sensor") + CRLF + "Enable voltage sensor";
	PropertyHelp[10 - 1] = String("P_Sensor") + CRLF + "Enable power sensor";
	PropertyHelp[11 - 1] = String("Node_num") + CRLF + "Assign a node number within a cluster";
	PropertyHelp[12 - 1] = "Cluster_num";
	PropertyHelp[13 - 1] = String("Total_Clusters.") + CRLF
	           + "Define the total number of groups in a circuit"
	           + CRLF
	           + "Just use for the first defined FMonitor";
	PropertyHelp[14 - 1] = "Nodes connected to this FMonitor. Example:(Nodes=33)";
	PropertyHelp[15 - 1] = String("CommVector of this FMonitor. ") + CRLF
	           + "The first entry of this vector is the number of "
	           + CRLF
	           + "Example:(CommVector={2,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0})"
	           + CRLF
	           + "The example show node #2 can communicate to node #1,#2,#3";
	PropertyHelp[16 - 1] = String("ElemTableLine of the each node within this cluster. ") + CRLF
	           + "The first entry of this vector is the number of node within cluster "
	           + CRLF
	           + "The second entry of this vector is element name "
	           + CRLF
	           + "The third entry of this vector is terminal number "
	           + CRLF
	           + "The fourth entry of this vector is voltage sensor "
	           + CRLF
	           + "Example:(ElemTable={2,Line.1,1,1})"
	           + CRLF
	           + "The example show node #2 Element";
	PropertyHelp[17 - 1] = String("0 = real Power controlled by each p_ref on each DG") + CRLF
	           + "1 = real Power on MeteredElem controlled by DGs according to P_trans_ref"
	           + CRLF
	           + "2 = Not defined"
	           + CRLF
	           + "3 = Not defined";
	PropertyHelp[18 - 1] = String("CommDelayVector of this FMonitor. ") + CRLF
	           + "The first entry of this vector is the number of the node."
	           + CRLF
	           + "Example:(CommVector={2,t1,0,t2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0})"
	           + CRLF
	           + "The example show node #2 can communicate to node #1 and #3 with time delay t1 and t2 seperately";
	PropertyHelp[19 - 1] = String("T_intvl_smpl: ") + CRLF
	           + "The imformation of each agent will be sampled at each T_comm time. Unit is second."
	           + CRLF
	           + "T_intvl_smpl is also the minimal communication time between neighbor nodes."
	           + CRLF
	           + "If T_intvl_smpl=0.0, no delay for the communication is enabled in the simulation.";
	PropertyHelp[20 - 1] = "MaxLocalMem: the max number of local memory siza. No larger than 99";
	PropertyHelp[21 - 1] = String("Volt_limits_pu: exmaple \"Volt_limits_pu={a0,a1, a2}\"") + CRLF
	           + "a0: the phase number, 0 means pos. seq; a1: upper voltage limit of this cluster, usually 1.05;"
	           + CRLF
	           + "a2: upper voltage limit of this cluster, usually 0.95";// set limits for this cluster {0,1.05, 0.95}
// set P curtailment on/off
	PropertyHelp[22 - 1] = String("b_Curt_Ctrl:set P curtailment on/off;") + CRLF
	           + "b_Curt_Ctrl=True: P curtailment will be implemented according to the system voltage (default);"
	           + CRLF
	           + "b_Curt_Ctrl=False: P curtailment will not be implemented.";
	PropertyHelp[23 - 1] = String("up_dly: delay time to upper level. For example: \"up_dly := 0.05\"") + CRLF
	           + "It can be used to simulate the time delay between clusters";
	PropertyName[24 - 1] = "virtual_ld_node: which node talks to upper level. virtual_ld_node=1";
	PropertyHelp[25 - 1] = String(" EGen = {kVA_fm, M_fm, D_fm, Tau_fm, Ki_fm,init_time}") + CRLF
	           + "where equations are:"
	           + CRLF
	           + "(1):delta''=omega"
	           + CRLF
	           + "(1):M_fm * omega''=puPm - puPe - D_fm*omega"
	           + CRLF
	           + "(1):Tau_fm*Pm ''=Ki_fm * omega "
	           + CRLF
	           + "puPm = Pm / kVA_fm, puPe = Pe/ kVAM_fm;"
	           + CRLF
	           + "everything is zero within init_time(default value is 0.5s);"
	           + CRLF
	           + "k_dltP is the coordinator for PV control input: u_i = k_dltP * pu_DltP + omg_fm.";
	PropertyHelp[26 - 1] = String("Define attack and defense:  attack_defense = {atk , dfs , atk_time , atk_node_num  , d_atk0  , beta_dfs, D_beta, D_p }.") + CRLF
	           + "attack_defense has to be defined after ''nodes'."
	           + CRLF
	           + "Example: attack_defense = { true , false , 0.5 , 1 , 0.1 , 5, 1 , 1}."
	           + CRLF
	           + "Example: (1) under attack); (2) defense is off; (3) attack starts at 0.5s; (4) attack is on node 1;"
	           + CRLF
	           + "(5) initial value of attack: d_0 = 0.1; (6) beta = 5;"
	           + CRLF
	           + "(7) D_bata is used as a multiplier on \\phi;"
	           + CRLF
	           + "(8) D_p is used as the attack on gradient contol: D_p = 1, which is normal; D_p=-1, gradient control work on the oppesite.";
	PropertyHelp[27 - 1] = "Comm_hide={...}. It is defined like CommVector."; // define attack and defense:  attack_defense = {atk = true , dfs = false , atk_time = 0.5 , atk_node_num = 1 , d_atk0 = 0.1 , beta_dfs = 30, D_beta = 1}
	PropertyHelp[28 - 1] = "Comm_node_hide={...}. It is defined like CommVector."; // define attack and defense:  attack_defense = {atk = true , dfs = false , atk_time = 0.5 , atk_node_num = 1 , d_atk0 = 0.1 , beta_dfs = 30, D_beta = 1}
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*--------------------------------------------------------------------------*/

int TDSSFMonitor::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new Monitor and add it to Monitor class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement((TDSSCktElement*) new TFMonitorObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}
/*--------------------------------------------------------------------------*/

int TDSSFMonitor::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int recalc = 0;
	int i = 0;

  // continue parsing with contents of Parser
  // continue parsing with contents of Parser
	ActiveFMonitorObj = ((TFMonitorObj*) ElementList.Get_Active());
	ActiveCircuit[ActorID]->Set_ActiveCktElement((TDSSCktElement*) ActiveFMonitorObj);
	result = 0;
	recalc = 0;
	/*# with ActiveFMonitorObj do */
	{
		auto with0 = ActiveFMonitorObj;
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
			++recalc;
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 661);
				break;
				case 	1:
				{
					with0->ElementName = ConstructElemName(LowerCase(Param));   // subtitute @var values if any
					with0->Set_PropertyValue(1,with0->ElementName);
				}
				break;
				case 	2:
				with0->MeteredTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->Mode = Parser[ActorID]->MakeInteger_();
				break;
				case 	4:
				{
					Param = LowerCase(Param);
					switch(Param[1])
					{
						case 	L's':
						with0->Save();
						break;
						case 	L'c': case L'r':
						with0->ResetIt(ActorID);
						break;
						case 	L't':
						with0->TakeSample(ActorID);
						break;
                    //'p': begin PostProcess(ActorID); dec(recalc) end
						default:
						  ;
						break;
					}
				}
				break;  // buffer
				case 	5:
				with0->IncludeResidual = InterpretYesNo(Param);
				break;
				case 	6:
				with0->VIpolar = InterpretYesNo(Param);
				break;
				case 	7:
				with0->Ppolar = InterpretYesNo(Param);
				break;
				case 	8:
				with0->P_trans_ref = 1000 * Parser[ActorID]->MakeDouble_();
				break;//kW for ref, unit of p_trans_ref is 'W'
				case 	9:
				with0->V_Sensor = Parser[ActorID]->MakeInteger_();
				break;//Voltage Sensor: Binary
				case 	10:
				with0->P_Sensor = Parser[ActorID]->MakeInteger_();
				break;//Power sensor : Binary
            //11: Node_num := Parser[ActorID].MakeInteger_();//Node number : integer
				case 	12:
				with0->Cluster_num = Parser[ActorID]->MakeInteger_();
				break;//group number: integer
				case 	13:
				with0->Total_Clusters = Parser[ActorID]->MakeInteger_();
				break;//Total number of the groups: integer
				case 	14:
				with0->Set_nodes_for_fm(Parser[ActorID]->MakeInteger_());
				break;//Nodes. Innitiate the structure
				case 	15:
				with0->Set_CommVector(Param);
				break;//
				case 	16:
				with0->Set_ElemTable_line(Param);
				break;//
				case 	17:
				with0->p_mode = Parser[ActorID]->MakeInteger_();
				break;
				case 	18:
				with0->Set_CommDelayVector(Param);
				break;//
				case 	19:
				{
					int stop = 0;
					with0->T_intvl_smpl = Parser[ActorID]->MakeDouble_(); //
					for(stop = with0->Nodes, i = 0; i < stop; i++)
					{
						with0->ResetDelaySteps(i);
					}
				}
				break;
				case 	20:
				with0->MaxLocalMem = Parser[ActorID]->MakeInteger_();
				break;
				case 	21:
				with0->Set_volt_lmt_clstr(Param);
				break;
				case 	22:
				with0->ld_fm_info[0].b_Curt_Ctrl = InterpretYesNo(Param);
				break; //curtailment
				case 	23:
				{
					with0->up_dly = Parser[ActorID]->MakeDouble_();
					if(with0->T_intvl_smpl != 0.0)
					{
						if(Frac(with0->up_dly / with0->T_intvl_smpl) != 0.0L)
							with0->nup_dlys = Trunc(with0->up_dly / with0->T_intvl_smpl);
						else
							with0->nup_dlys = Trunc(with0->up_dly / with0->T_intvl_smpl) + 1;
					}
					else
					with0->nup_dlys = 0;
				}
				break;
				case 	24:
				with0->virtual_Ld_Nd = Parser[ActorID]->MakeInteger_() - 1;
				break;
				case 	25:
				with0->Set_EquivalentGenerator(Param);
				break;
				case 	26:
				with0->Set_atk_dfs(Param);
				break;
				case 	27:
				with0->Set_CommVector_Hide(Param);
				break;//
				case 	28:
				with0->Set_CommVector_NodeHide(Param);
				break;//

           // Inherited parameters
				default:
				ClassEdit(ActiveFMonitorObj, ParamPointer - NumPropsThisClass);
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		if(recalc > 0)
			with0->RecalcElementData(ActorID);
	}
	return result;
}

/*--------------------------------------------------------------------------*/  // Force all monitors in the circuit to reset

void TDSSFMonitor::ResetAll(int ActorID)
{
	TFMonitorObj* Mon = nullptr;
	Mon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_First());
	while(Mon != nullptr)
	{
		if( ( (TDSSCktElement*) Mon )->Get_Enabled())
			Mon->ResetIt(ActorID);
		Mon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_Next());
	}
}

/*--------------------------------------------------------------------------*/  // Force all monitors in the circuit to take a sample

void TDSSFMonitor::SampleAll(int ActorID)
{
	TFMonitorObj* Mon = nullptr;
// sample all monitors except mode 5 monitors
	Mon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_First());
	while(Mon != nullptr)
	{
		if(((TDSSCktElement*)Mon)->Get_Enabled())
		{
			if(Mon->Mode != 5)
				Mon->TakeSample(ActorID);
		}
		Mon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_Next());
	}
      //ProcessFvalue;
}
//This function is used to measure total net power of a cluster
 //all FMs

void TDSSFMonitor::update_sys_ld_info(int ActorID)
{
	TFMonitorObj* FMon = nullptr;
	double vtemp = 0.0;
	double dv_lwst = 0.0;
	ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_hghst = -999999;
	ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_lwst = 9999999;
	FMon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_First());
	while(FMon != nullptr)

          //update all agents information:
              //synchronous: voltage to agents
              //asynchronous: aphga, ahphaP, highest/lowest voltage
	{
		if( ( (TDSSCktElement*) FMon )->Get_Enabled())
		{
			FMon->update_node_info_each_time_step(ActorID); //update old z_dfs, vl_alpha_dgn
			FMon->update_ld_dly(ActorID); //with delay
		}
          //
		          /*Update cluster info to center*/
		if(ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_hghst < FMon->ld_fm_info[0].volt_hghst)
		{
			ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_hghst = FMon->ld_fm_info[0].volt_hghst;
			ActiveCircuit[ActorID]->Solution->LD_FM[0].ndnum_hghst = FMon->ld_fm_info[0].ndnum_hghst;
			ActiveCircuit[ActorID]->Solution->LD_FM[0].clstr_num_hghst = FMon->Cluster_num;
			ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_hgh_lmt = FMon->ld_fm_info[0].volt_hgh_lmt;
			ActiveCircuit[ActorID]->Solution->LD_FM[0].b_ctrl_hghst = FMon->ld_fm_info[0].b_ctrl_hghst;
		}
		if(ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_lwst > FMon->ld_fm_info[0].volt_lwst)
		{
			ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_lwst = FMon->ld_fm_info[0].volt_lwst;
			ActiveCircuit[ActorID]->Solution->LD_FM[0].ndnum_lwst = FMon->ld_fm_info[0].ndnum_lwst;
			ActiveCircuit[ActorID]->Solution->LD_FM[0].clstr_num_lwst = FMon->Cluster_num;
			ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_lw_lmt = FMon->ld_fm_info[0].volt_lw_lmt;
			ActiveCircuit[ActorID]->Solution->LD_FM[0].b_ctrl_lwst = FMon->ld_fm_info[0].b_ctrl_lwst;
		}

          /*---- curtailment ----- bCurtl := t/f for overall system ------*/
          //curtailment is needed or not
		if(FMon->ld_fm_info[0].volt_hghst > 1.0)     //need curtailment
			FMon->ld_fm_info[0].b_ctrl_hghst = true;
		else
			FMon->ld_fm_info[0].b_ctrl_hghst = false;
          /*---- each cluster may have their own ---*/
		FMon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_Next());
	}
      //curtailment is needed or not
	vtemp = (ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_hghst - ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_lwst);//p.u.
	dv_lwst = ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_lwst - ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_lw_lmt;//0.95; // must greater than 0.0
	if(dv_lwst < 0.0)
	{
		ActiveCircuit[ActorID]->Solution->bCurtl = true;  //curtailment
	}
	else
	{
		ActiveCircuit[ActorID]->Solution->bCurtl = false;//dont need curtailment
	}
}// calculte frequency for each cluster

void TDSSFMonitor::Calc_P_freq(int ActorID)
{
	TFMonitorObj* FMon = nullptr;
     //ActiveCircuit[ActorID].Solution.LD_FM[0].freq  := 0; //saved for system frequency
     //ActiveCircuit[ActorID].Solution.LD_FM[0].delta := 0; //saved for angle of the inertia center of a cluster
	FMon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_First());
	while(FMon != nullptr)
	{
		if(((TDSSCktElement*)FMon)->Get_Enabled())
		{
			if(FMon->eg_defed == true)
				FMon->Calc_P_freq_fm(ActorID); //w
		}
		FMon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_Next());
          //
	}
}

void TDSSFMonitor::update_atks(int ActorID)
{
	TFMonitorObj* FMon = nullptr;
	FMon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_First());
	while(FMon != nullptr)
	{
		if(((TDSSCktElement*)FMon)->Get_Enabled() && (FMon->atk == true))
			FMon->update_attack(ActorID); //w
		FMon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_Next());
          //
	}
}

void TDSSFMonitor::update_defense_layer(int ActorID)
{
	TFMonitorObj* FMon = nullptr;
	FMon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_First());
	while(FMon != nullptr)
	{
		if(((TDSSCktElement*)FMon)->Get_Enabled() && (FMon->dfs == true))
			FMon->update_defense(ActorID); //w
		FMon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_Next());
          //
	}
}

/*--------------------------------------------------------------------------*/
/*
Procedure TDSSFMonitor.PostProcessAll(ActorID : Integer);
VAR
   Mon:TFMonitorObj;
Begin
   Mon := ActiveCircuit[ActorID].FMonitors.Get_First();
   WHILE Mon<>Nil DO Begin
       If Mon.Enabled Then Mon.PostProcess;
       Mon := ActiveCircuit[ActorID].FMonitors.Get_Next();
   End;
End;
*/
/*--------------------------------------------------------------------------*/     // Force all monitors in the circuit to save their buffers to disk

void TDSSFMonitor::SaveAll(int ActorID)
{
	TFMonitorObj* Mon = nullptr;
	Mon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_First());
	while(Mon != nullptr)
	{
		if(((TDSSCktElement*)Mon)->Get_Enabled())
			Mon->Save();
		Mon = ((TFMonitorObj*) ActiveCircuit[ActorID]->FMonitors.Get_Next());
	}
}

/*--------------------------------------------------------------------------*/

int TDSSFMonitor::MakeLike(const String MonitorName)
{
	int result = 0;
	TFMonitorObj* OtherMonitor = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Monitor name in the present collection*/
	OtherMonitor = ((TFMonitorObj*) Find(MonitorName));
	if(OtherMonitor != nullptr)
		/*# with ActiveFMonitorObj do */
		{
			auto with0 = ActiveFMonitorObj;
			int stop = 0;
			((TDSSCktElement*)with0)->Set_NPhases(((TDSSCktElement*)OtherMonitor)->Fnphases);
			((TDSSCktElement*)with0)->Set_Nconds(((TDSSCktElement*)OtherMonitor)->Fnconds); // Force Reallocation of terminal stuff
			with0->BufferSize = OtherMonitor->BufferSize;
			with0->ElementName = OtherMonitor->ElementName;
			with0->MeteredElement = OtherMonitor->MeteredElement;  // Pointer to target circuit element
			with0->MeteredTerminal = OtherMonitor->MeteredTerminal;
			with0->Mode = OtherMonitor->Mode;
			with0->IncludeResidual = OtherMonitor->IncludeResidual;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherMonitor->Get_PropertyValue(i));
			}
			with0->BaseFrequency = OtherMonitor->BaseFrequency;
		}
	else
		DoSimpleMsg(String("Error in Monitor MakeLike: \"") + MonitorName
	           + "\" Not Found.", 662);
	return result;
}

/*--------------------------------------------------------------------------*/

int TDSSFMonitor::Init(int Handle, int ActorID)
{
	int result = 0;
	TFMonitorObj* Mon = nullptr;
	result = 0;
	if(Handle > 0)
	{
		Mon = ((TFMonitorObj*) ElementList.Get(Handle));
		Mon->ResetIt(ActorID);
	}
	else
  // Do 'em all
	{
		Mon = ((TFMonitorObj*) ElementList.Get_First());
		while(Mon != nullptr)
		{
			Mon->ResetIt(ActorID);
			Mon = ((TFMonitorObj*) ElementList.Get_Next());
		}
	}
	return result;
}


/*==========================================================================*/
/*                    TFMonitorObj                                           */
/*==========================================================================*/



/*--------------------------------------------------------------------------*/

TFMonitorObj::TFMonitorObj(TDSSClass* ParClass, const String MonitorName)
 : inherited(ParClass),
			Nodes(33),
			pCommMatrix(nullptr),
			P_trans_ref(0.0),
			p_mode(0),
			BufferSize(0),
			Hour(0),
			Sec(0.0),
			MonBuffer(nullptr),
			BufPtr(0),
			CurrentBuffer(nullptr),
			VoltageBuffer(nullptr),
			NumStateVars(0),
			StateBuffer(nullptr),
			FlickerBuffer(nullptr),
			SolutionBuffer(nullptr),
			IncludeResidual(false),
			VIpolar(false),
			Ppolar(false),
			FileSignature(0),
			FileVersion(0),
			BaseFrequency(0.0),
			F_Value_one(0.0),
			F_Value_one_V(nullptr),
			F_Value_one_S(nullptr),
			Fvalue_P(0.0),
			Fvalue_Q(0.0),
			F_P_one(0.0),
			F_Q_one(0.0),
			P_ref_one(0.0),
			Node_num(0),
			V_Sensor(0),
			P_Sensor(0),
			Cluster_num(0),
			Total_Clusters(0),
			T_intvl_smpl(0.0),
			MaxLocalMem(0),
			Smpl_stps(0),
			pCommDelayMatrix(nullptr),
			pCommDelaySteps(nullptr),
			kVA_fm(0.0),
			M_fm(0.0),
			D_fm(0.0),
			Tau_fm(0.0),
			Ki_fm(0.0),
			Pm_fm(0.0),
			init_time(0.0),
			k_dltP(0.0),
			up_dly(0.0),
			nup_dlys(0),
			virtual_Ld_Nd(0),
			d_atk_inited(false),
			z_dfs_inited(false),
			atk_node_num(0),
			atk_time(0.0),
			beta_dfs(0.0),
			D_beta(0.0),
			D_p(0.0),
			dlt_z0(0.0),
			pCommHide(nullptr),
			pCommNode_Hide(nullptr),
			Bus_code(0),
			NodeNum(0),
			Node_Ref(0),
			IsFileOpen(false),
			ValidMonitor(false),
			IsProcessed(false),
			pNodeFMs(nullptr),
			Mode(0),
			SampleCount(0),
			eg_defed(false),
			dlt_fm(0.0),
			omg_fm(0.0),
			comp_omg(0.0),
			atk(false),
			dfs(false)
{
	int i = 0;
	int stop = 0;
	Set_Name(LowerCase(MonitorName));
	DSSObjType = ParClass->DSSClassType;
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	pNodeFMs = nullptr;
     //pNodeFMs := AllocMem(Sizeof(pNodeFMs^[1] )* 1);

     /*Current Buffer has to be big enough to hold all terminals*/
	CurrentBuffer = nullptr;
	VoltageBuffer = nullptr;
	StateBuffer = nullptr;
	FlickerBuffer = nullptr;
	SolutionBuffer = nullptr;
	BaseFrequency = 60.0;
	Hour = 0;
	Sec = 0.0;
	Mode = 0;  // Standard Mode: V & I, complex values
	BufferSize = 1024;       // Makes a 4K buffer
	MonBuffer = (pSingleArray) malloc( sizeof(float) * BufferSize );
	BufPtr = 0;
	ElementName = ((TDSSObject*) ActiveCircuit[ActiveActor]->CktElements.Get(1))->get_Name(); // Default to first circuit element (source)
	MeteredElement = nullptr;
	BufferFile = "";

     //MonitorStream := TMemoryStream.Create; // Create memory stream
	IsFileOpen = false;
	MeteredTerminal = 1;
	IncludeResidual = false;
	VIpolar = true;
	Ppolar = true;
	FileSignature = 43756;
	FileVersion = 1;
	SampleCount = 0;
	IsProcessed = false;
	DSSObjType = ParClass->DSSClassType; //MON_ELEMENT;
	InitPropertyValues(0);
	pCommMatrix = (pSmallIntArray) malloc( sizeof(pCommMatrix[0]) * Nodes * Nodes);
     /**/
	F_Value_one_V = (pDoubleArray)malloc(sizeof(double) * 999);
	F_Value_one_S = (pComplexArray)malloc(sizeof(complex) * 999);
     /**/
	T_intvl_smpl = 0;
	MaxLocalMem = 10;
	pCommDelayMatrix = (pDoubleArray)malloc(sizeof(double) * Nodes * Nodes);
	pCommDelaySteps = (pSmallIntArray)malloc(sizeof(pCommMatrix[0]) * Nodes * Nodes);
	pCommHide = (pSmallIntArray)malloc(sizeof(pCommMatrix[0]) * Nodes * Nodes);
	pCommNode_Hide = (pSmallIntArray)malloc(sizeof(pCommMatrix[0]) * Nodes * Nodes);

     /*leader information*/
	for(stop = 3, i = 0; i <= stop; i++)
	{
		ld_fm_info[i].ndnum_hghst = 0;
		ld_fm_info[i].b_ctrl_hghst = false;        //small number that can never be true
		ld_fm_info[i].volt_hghst = -1.0;
		ld_fm_info[i].volt_hgh_lmt = 1.05;
		ld_fm_info[i].Pinjec_hghst = 0.0;
		ld_fm_info[i].ndnum_lwst = 0;
		ld_fm_info[i].b_ctrl_lwst = false;
		ld_fm_info[i].volt_lw_lmt = 0.95;
		ld_fm_info[i].volt_lwst = 9999999999.0;   //large nunber can never be true
		ld_fm_info[i].Pinjec_lwst = 0.0;
		ld_fm_info[i].volt_avg = 0.0;
		ld_fm_info[i].total_pg = 0.0;
		ld_fm_info[i].total_pl = 0.0;
		ld_fm_info[i].b_Curt_Ctrl = false;
	}
	virtual_Ld_Nd = 0;
	nup_dlys = 0;
     //bCurtl_Clstr := false;
     /*end of leader initialization*/
     //virtual generator for frequency
	eg_defed = false;
	kVA_fm = 0.0;
	M_fm = 0.0;
	D_fm = 0.0;
	Tau_fm = 0.0;
	Ki_fm = 0.0;
	dlt_fm = 0.0;
	omg_fm = 0.0;
	Pm_fm = 0.0;
	init_time = 0.5;
	comp_omg = 0.0;
     // when the attack time starts
	atk = false;
	atk_time = 0.5;
	atk_node_num = 1;
	d_atk_inited = false;
	z_dfs_inited = false;
	D_beta = 1;
	D_p = 1;
	dlt_z0 = 0.0;
}

TFMonitorObj::~TFMonitorObj()
{

     //MonitorStream.Free;
	ElementName = "";
	BufferFile = "";
	free(MonBuffer);
	free(StateBuffer);
	free(CurrentBuffer);
	free(VoltageBuffer);
	free(FlickerBuffer);
	free(SolutionBuffer);
     /**/
	if(ASSIGNED(F_Value_one_V))
		free(F_Value_one_V);
	if(ASSIGNED(F_Value_one_S))
		free(F_Value_one_S);
     /**/
	delete[] pNodeFMs;
	free(pCommMatrix);
	free(pCommDelayMatrix);
	free(pCommDelaySteps);
     //
	free(pCommHide);
	free(pCommNode_Hide);
	// inherited::Destroy();
}



/*--------------------------------------------------------------------------*/

void ConvertBlanks(String& s)
{
	int BlankPos = 0;
     /* Convert spaces to Underscores */
	BlankPos = Pos(" ", s);
	while(BlankPos > 0)
	{
		s[BlankPos] = L'_';
		BlankPos = Pos(" ", s);
	}
}

/*--------------------------------------------------------------------------*/

void TFMonitorObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	int i = 0;
	ValidMonitor = false;
	DevIndex = GetCktElementIndex(ElementName);                   // Global function
	if(DevIndex > 0)                                       // Monitored element must already exist
	{
		MeteredElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		switch((Mode & ModeMask))
		{
			case 	2:                                                // Must be transformer
			{
				if((MeteredElement->DSSObjType & CLASSMASK) != XFMR_ELEMENT)
				{
					DoSimpleMsg(MeteredElement->get_Name() + " is not a transformer!", 663);
					return;
				}
			}
			break;                                                // Must be PCElement
			case 	3:
			{
				if((MeteredElement->DSSObjType & BaseClassMask) != PC_ELEMENT)
				{
					DoSimpleMsg(MeteredElement->get_Name() + " must be a power conversion element (Load or Generator)!", 664);
					return;
				}
			}
			break;                                                // Checking Caps Tap
			case 	6:
			{
				if((MeteredElement->DSSObjType & CLASSMASK) != CAP_ELEMENT)
				{
					DoSimpleMsg(MeteredElement->get_Name() + " is not a capacitor!", 2016001);
					return;
				}
			}
			break;
			default:
			  ;
			break;
		}
		if(MeteredTerminal > MeteredElement->Get_NTerms())
		{
			DoErrorMsg(String("FMonitor: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Respecify terminal no.", 665);
		}
		else
		{
			Set_NPhases(MeteredElement->Get_NPhases());
			Set_Nconds(MeteredElement->Get_NConds());

               // Sets name of i-th terminal's connected bus in monitor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
			SetBus(1, MeteredElement->GetBus(MeteredTerminal));
			switch((Mode & ModeMask))
			{
				case 	3:
				{
					NumStateVars = ((TPCElement*) MeteredElement)->NumVariables();
					StateBuffer = (pDoubleArray)realloc(StateBuffer, sizeof(double) * NumStateVars);
				}
				break;
				case 	4:
				{
					FlickerBuffer = (pComplexArray)realloc(FlickerBuffer, sizeof(complex) * Get_NPhases());
				}
				break;
				case 	5:
				{
					SolutionBuffer = (pDoubleArray)realloc(SolutionBuffer, sizeof(double) * NumSolutionVars);
				}
				break;
				default:
				CurrentBuffer = (pComplexArray)realloc(CurrentBuffer, sizeof(complex) * MeteredElement->Yorder);
				VoltageBuffer = (pComplexArray)realloc(VoltageBuffer, sizeof(complex) * MeteredElement->Get_NConds());
				break;
			}

                 //ClearMonitorStream;
			ValidMonitor = true;
		}
	}
	else
	{
		MeteredElement = nullptr;   // element not found
		DoErrorMsg(String("Monitor: \"") + this->get_Name() + "\"", String("Circuit Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 666);
	}
         /**/
         /**/
}

void TFMonitorObj::MakePosSequence(int ActorID)
{
	if(MeteredElement != nullptr)
	{
		SetBus(1, MeteredElement->GetBus(MeteredTerminal));
		Set_NPhases(MeteredElement->Get_NPhases());
		Set_Nconds(MeteredElement->Get_NConds());
		switch((Mode & ModeMask))
		{
			case 	3:
			{
				NumStateVars = ((TPCElement*) MeteredElement)->NumVariables();
				StateBuffer = (pDoubleArray)realloc(StateBuffer, sizeof(double) * NumStateVars);
			}
			break;
			case 	4:
			{
				FlickerBuffer = (pComplexArray)realloc(FlickerBuffer, sizeof(complex) * Get_NPhases());
			}
			break;
			case 	5:
			{
				SolutionBuffer = (pDoubleArray)realloc(SolutionBuffer, sizeof(double) * NumSolutionVars);
			}
			break;
			default:
			CurrentBuffer = (pComplexArray)realloc(CurrentBuffer, sizeof(complex) * MeteredElement->Yorder);
			VoltageBuffer = (pComplexArray)realloc(VoltageBuffer, sizeof(complex) * MeteredElement->Get_NConds());
			break;
		}
    //ClearMonitorStream;
		ValidMonitor = true;
	}
	TDSSCktElement::MakePosSequence(ActorID);
}


/*--------------------------------------------------------------------------*/

void TFMonitorObj::CalcYPrim(int ActorID)
{


  /*A Monitor is a zero current source; Yprim is always zero.*/
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
}

/*--------------------------------------------------------------------------*/

// Saves present buffer to monitor file, resets bufferptrs and continues

void TFMonitorObj::Save()
{


     //If NOT IsFileOpen THEN OpenMonitorStream; // Position to end of stream
	
     /*Write present monitor buffer to monitorstream*/
     //MonitorStream.Write(MonBuffer^, SizeOF(MonBuffer^[1]) * BufPtr);
	BufPtr = 0; // reset Buffer for next
}
/*--------------------------------------------------------------------------*/

void TFMonitorObj::Set_nodes_for_fm(int intNodes)
{
	int i = 0;
	int j = 0;
  //
	int stop = 0;
	Nodes = intNodes;//initalize the size according to nodes
	if(pNodeFMs != nullptr)
	{
		delete[] pNodeFMs;
		pNodeFMs = nullptr;
	}
	if(pCommMatrix != nullptr)
		free(pCommMatrix);
	if(pCommDelayMatrix != nullptr)
		free(pCommDelayMatrix);
	if(pCommDelaySteps != nullptr)
		free(pCommDelaySteps);
	if(pCommHide != nullptr)
		free(pCommHide);
	if(pCommNode_Hide != nullptr)
		free(pCommNode_Hide);
	
	pNodeFMs = new TVLNodeVars[intNodes];
	pCommMatrix = (pSmallIntArray)malloc(sizeof(pCommMatrix[0]) * intNodes * intNodes);
	pCommHide = (pSmallIntArray)malloc(sizeof(pCommHide[0]) * intNodes * intNodes);
	pCommNode_Hide = (pSmallIntArray)malloc(sizeof(pCommNode_Hide[0]) * intNodes * intNodes);
	pCommDelayMatrix = (pDoubleArray)malloc(sizeof(pCommDelayMatrix[0]) * intNodes * intNodes);
	pCommDelaySteps = (pSmallIntArray)malloc(sizeof(pCommDelaySteps[0]) * intNodes * intNodes);
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Nodes, j = 0; j < stop1; j++)
		{
			(pCommDelayMatrix)[Nodes * i + j] = 0.0;
			pCommMatrix[Nodes * i + j] = 0;
			pCommHide[Nodes * i + j] = 0;
			pCommNode_Hide[Nodes * i + j] = 0;
			pCommDelaySteps[Nodes * i + j] = 0;
		}
	}
}
/*--------------------------------------------------------------------------*/

void TFMonitorObj::Set_volt_lmt_clstr(String strParam)
{
	String dummy;
	int i = 0;
	double Datahgh = 0.0;
	double datalw = 0.0;
	int iPhasenum = 0;
	AuxParser[ActiveActor]->SetCmdString(strParam);  // Load up Parser
	dummy = AuxParser[ActiveActor]->GetNextParam(); // the first entry is the No. of iNode
	iPhasenum = AuxParser[ActiveActor]->MakeInteger_(); //node number defined in cluster
	dummy = AuxParser[ActiveActor]->GetNextParam(); // high limit
	Datahgh = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); // low limit
	datalw = AuxParser[ActiveActor]->MakeDouble_();
	switch(iPhasenum)
	{
		case 	0:
		{
			ld_fm_info[0].volt_hgh_lmt = Datahgh;
			ld_fm_info[0].volt_lw_lmt = datalw;
		}
		break;
		case 	1:
		;
		break;
		case 	2:
		;
		break;
		case 	3:
		;
		break;
		default:
		  ;
		break;
	}
}

void TFMonitorObj::Set_CommVector(String strParam)
{
	String dummy;
	String TempStr;
	String DataStr;                      // the min if Nodes or the length of the vector
	int i = 0;
	int j = 0;
	int IMin = 0;
	int iNodeNum = 0;
	int stop = 0;
	AuxParser[ActiveActor]->SetCmdString(strParam);  // Load up Parser
    //iMin := min(Nodes, )
    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	dummy = AuxParser[ActiveActor]->GetNextParam(); // the first entry is the No. of iNode
	iNodeNum = AuxParser[ActiveActor]->MakeInteger_() - 1; //node number defined in cluster
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
		DataStr = AuxParser[ActiveActor]->MakeString_();
		if(DataStr.size() > 0)
		{
			(pCommMatrix)[iNodeNum * Nodes + i] = (short int) AuxParser[ActiveActor]->MakeInteger_();
			(pCommHide)[iNodeNum * Nodes + i] = (short int) AuxParser[ActiveActor]->MakeInteger_();       //default
			(pCommNode_Hide)[iNodeNum * Nodes + i] = (short int) AuxParser[ActiveActor]->MakeInteger_();  //default
		}
	}

// Updates the value of the property for future queries
// Added y Davis 02072019
	TempStr = "";
	for(stop = Nodes, j = 0; j < stop; j++)
	{
		int stop1 = 0;
		iNodeNum = j;
		TempStr = TempStr + IntToStr(iNodeNum + 1) + ",";
		for(stop1 = Nodes, i = 0; i < stop1; i++)
		{
			TempStr = TempStr + IntToStr(pCommMatrix[iNodeNum * Nodes + i]) + ",";
		}
		TempStr = TempStr + "|";
	}
	( (TDSSObject*) ActiveDSSObject[ActiveActor] )->Set_PropertyValue(15,TempStr);
}

void TFMonitorObj::Set_CommVector_Hide(String strParam)
{
	String dummy;
	String DataStr;
	int i = 0;
	int IMin = 0; // the min if Nodes or the length of the vector
	int iNodeNum = 0;
	int stop = 0;
	AuxParser[ActiveActor]->SetCmdString(strParam);  // Load up Parser
    //iMin := min(Nodes, )
    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	dummy = AuxParser[ActiveActor]->GetNextParam(); // the first entry is the No. of iNode
	iNodeNum = AuxParser[ActiveActor]->MakeInteger_() - 1; //node number defined in cluster
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
		DataStr = AuxParser[ActiveActor]->MakeString_();
		if(DataStr.size() > 0)
			pCommHide[iNodeNum * Nodes + i] = (short int) AuxParser[ActiveActor]->MakeInteger_();
	}
}

void TFMonitorObj::Set_CommVector_NodeHide(String strParam)
{
	String dummy;
	String DataStr;
	int i = 0;
	int IMin = 0; // the min if Nodes or the length of the vector
	int iNodeNum = 0;
	int stop = 0;
	AuxParser[ActiveActor]->SetCmdString(strParam);  // Load up Parser
    //iMin := min(Nodes, )
    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	dummy = AuxParser[ActiveActor]->GetNextParam(); // the first entry is the No. of iNode
	iNodeNum = AuxParser[ActiveActor]->MakeInteger_() - 1; //node number defined in cluster
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
		DataStr = AuxParser[ActiveActor]->MakeString_();
		if(DataStr.size() > 0)
			pCommNode_Hide[iNodeNum * Nodes + i] = (short int) AuxParser[ActiveActor]->MakeInteger_();
	}
}

void TFMonitorObj::Set_CommDelayVector(String strParam)
{
	String dummy;
	String TempStr;
	String DataStr;
	int i = 0;
	int j = 0;
	int iNodeNum = 0;
	int stop = 0;
	AuxParser[ActiveActor]->SetCmdString(strParam);  // Load up Parser
    //iMin := min(Nodes, )
    /*Loop for no more than the expected number of windings;  Ignore omitted values*/
	dummy = AuxParser[ActiveActor]->GetNextParam(); // the first entry is the No. of iNode
	iNodeNum = AuxParser[ActiveActor]->MakeInteger_() - 1; //node number defined in cluster
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		dummy = AuxParser[ActiveActor]->GetNextParam(); // ignore any parameter name  not expecting any
		DataStr = AuxParser[ActiveActor]->MakeString_();
		if(DataStr.size() > 0)
			pCommDelayMatrix[iNodeNum * Nodes + i] = AuxParser[ActiveActor]->MakeDouble_();
	}
	ResetDelaySteps(iNodeNum);  //Use pCommDelayMatrix^ to calculate pCommDelaySteps^

// Updates the value of the property for future queries
// Added y Davis 02072019
	TempStr = "";
	for(stop = Nodes, j = 0; j < stop; j++)
	{
		int stop1 = 0;
		iNodeNum = j;
		TempStr = TempStr + IntToStr(iNodeNum + 1) + ",";
		for(stop1 = Nodes, i = 0; i < stop1; i++)
		{
			TempStr = TempStr + FloatToStr(pCommDelayMatrix[iNodeNum * Nodes + i]) + ",";
		}
		TempStr = TempStr + "|";
	}
	( (TDSSObject*) ActiveDSSObject[ActiveActor] )->Set_PropertyValue(18,TempStr);
}

void TFMonitorObj::Set_EquivalentGenerator(String strParam)
{
	String dummy;
	String DataStr;
	int i = 0;
	int iNodeNum = 0;
	AuxParser[ActiveActor]->SetCmdString(strParam);  // Load up Parser
	dummy = AuxParser[ActiveActor]->GetNextParam(); // the first entry is kVA
	kVA_fm = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); //
	M_fm = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); //
	D_fm = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); //
	Tau_fm = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); //
	Ki_fm = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); // init_time
	init_time = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); // k_dltP is the coordinator
	k_dltP = AuxParser[ActiveActor]->MakeDouble_();
	if(kVA_fm * M_fm * D_fm * Tau_fm * Ki_fm != 0.0)
		eg_defed = true; //eg_defed := false by default
}

void TFMonitorObj::Set_atk_dfs(String strParam)
{
	String dummy;
	String DataStr;
	int i = 0;
	int iNodeNum = 0;
	AuxParser[ActiveActor]->SetCmdString(strParam);  // Load up Parser
	dummy = AuxParser[ActiveActor]->GetNextParam(); //       atk
	DataStr = AuxParser[ActiveActor]->MakeString_();
	atk = InterpretYesNo(DataStr);
	dummy = AuxParser[ActiveActor]->GetNextParam(); //       dfs
	DataStr = AuxParser[ActiveActor]->MakeString_();
	dfs = InterpretYesNo(DataStr);
	dummy = AuxParser[ActiveActor]->GetNextParam(); //       atk_time
	atk_time = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); //       atk_node_num
	atk_node_num = AuxParser[ActiveActor]->MakeInteger_() - 1;
	dummy = AuxParser[ActiveActor]->GetNextParam(); //       d_atk0
	(pNodeFMs)[atk_node_num].d_atk0 = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); //       beta_dfs
	beta_dfs = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); //       D_beta
	D_beta = AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam(); //       direction of gradient control
	D_p = AuxParser[ActiveActor]->MakeDouble_();
}
/*--------------------------------------------------------------------------*/

void TFMonitorObj::Set_ElemTable_line(String strParam)
{
	String dummy;
	String TempStr;
	String DataStr;
	int i = 0;
	int iNodeNum = 0;
	int stop = 0;
	AuxParser[ActiveActor]->SetCmdString(strParam);  // Load up Parser
	dummy = AuxParser[ActiveActor]->GetNextParam(); // the first entry is the number of the iNode
	iNodeNum = AuxParser[ActiveActor]->MakeInteger_() - 1; //node number defined in the cluster
	dummy = AuxParser[ActiveActor]->GetNextParam(); // the first entry is the number of the iNode
	(pNodeFMs)[iNodeNum].vl_strBusName = AuxParser[ActiveActor]->MakeString_(); //node number defined in the cluster
	dummy = AuxParser[ActiveActor]->GetNextParam();
	(pNodeFMs)[iNodeNum].vl_strMeasuredName = AuxParser[ActiveActor]->MakeString_(); //Element name load into data str
               //
               //pNodeFMs^[iNodeNum].vl_strName_dg := pNodeFMs^[iNodeNum].vl_strMeasuredName;
               //
	dummy = AuxParser[ActiveActor]->GetNextParam();
	(pNodeFMs)[iNodeNum].vl_terminalNum = AuxParser[ActiveActor]->MakeInteger_();  //Terminal number load into data str
	dummy = AuxParser[ActiveActor]->GetNextParam();
	(pNodeFMs)[iNodeNum].vl_V_ref_dg = 1000 * AuxParser[ActiveActor]->MakeDouble_();
	dummy = AuxParser[ActiveActor]->GetNextParam();
	(pNodeFMs)[iNodeNum].vl_kc_ul_dg = AuxParser[ActiveActor]->MakeDouble_();
               //2.402
	Init_nodeFM(iNodeNum, ActiveActor);
// Updates the value of the property for future queries
// Added y Davis 02072019
	TempStr = "";
	for(stop = iNodeNum, i = 0; i <= stop; i++)
	{
		TempStr = TempStr
	           + IntToStr(i + 1)
	           + ","
	           + (pNodeFMs)[i].vl_strBusName
	           + ","
	           + (pNodeFMs)[i].vl_strMeasuredName
	           + ","
	           + IntToStr((pNodeFMs)[i].vl_terminalNum)
	           + ","
	           + FloatToStr((pNodeFMs)[i].vl_V_ref_dg)
	           + ","
	           + FloatToStr((pNodeFMs)[iNodeNum].vl_kc_ul_dg)
	           + "|";
	}
	( (TDSSObject*) ActiveDSSObject[ActiveActor] )->Set_PropertyValue(16,TempStr);
}

void TFMonitorObj::Get_PQ_DI(int i_NodeNum, int ActorID)
{
	int DevIndex = 0;
	int i = 0;
	int j = 0;
	int Num = 0;
	TDSSCktElement* PElement = nullptr;
	TLoadObj* pLoad = nullptr;
	pComplexArray cBuffer = nullptr;
	/*# with pNodeFMs^[i_NodeNum] do */
	{
		
		switch((pNodeFMs)[i_NodeNum].ldType)
		{
			case 	0:// one 3 phase or 2 phase load
			{
				int stop = 0;
				PElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->PCElements.Get((pNodeFMs)[i_NodeNum].ldIdx));
				Num = PElement->Get_NPhases();
				cBuffer = (pComplexArray) malloc(sizeof(complex) * Num);
				PElement->GetPhasePower(cBuffer, ActorID);// power
				for(stop = Num, j = 1; j <= stop; j++)
				{
					i = (PElement->Terminals[0].TermNodeRef)[j - 1];
					switch((ActiveCircuit[ActorID]->MapNodeToBus)[i - 1].NodeNum)
					{
						case 	1:
						{
							(pNodeFMs)[i_NodeNum].vl_P_Di1 = (cBuffer)[1].re;
							(pNodeFMs)[i_NodeNum].vl_Q_Di1 = (cBuffer)[1].im;
						}
						break;
						case 	2:
						{
							(pNodeFMs)[i_NodeNum].vl_P_Di1 = (cBuffer)[2].re;
							(pNodeFMs)[i_NodeNum].vl_Q_Di1 = (cBuffer)[2].im;
						}
						break;
						case 	3:
						{
							(pNodeFMs)[i_NodeNum].vl_P_Di1 = (cBuffer)[3].re;
							(pNodeFMs)[i_NodeNum].vl_Q_Di1 = (cBuffer)[3].im;
						}
						break;
						default:
						  ;
						break;
					}
				}
			}
			break;
			case 	1:
			 case 2:
			 case 3:
			{
				PElement = nullptr;
				if((pNodeFMs)[i_NodeNum].ldIdx1 > 0)
				{
					int stop = 0;
					PElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->PCElements.Get((pNodeFMs)[i_NodeNum].ldIdx1));
					Num = PElement->Get_NPhases();
					cBuffer = (pComplexArray)realloc(cBuffer, sizeof(complex) * Num);
					PElement->GetPhasePower(cBuffer, ActorID);// power
					for(stop = Num, j = 1; j <= stop; j++)
					{
						i = (PElement->Terminals[0].TermNodeRef)[j - 1];
						switch((ActiveCircuit[ActorID]->MapNodeToBus)[i - 1].NodeNum)
						{
							case 	1:
							{
								(pNodeFMs)[i_NodeNum].vl_P_Di1 = (cBuffer)[1].re;
								(pNodeFMs)[i_NodeNum].vl_Q_Di1 = (cBuffer)[1].im;
							}
							break;
							case 	2:
							{
								(pNodeFMs)[i_NodeNum].vl_P_Di1 = (cBuffer)[1].re;
								(pNodeFMs)[i_NodeNum].vl_Q_Di1 = (cBuffer)[1].im;
							}
							break;
							case 	3:
							{
								(pNodeFMs)[i_NodeNum].vl_P_Di1 = (cBuffer)[1].re;
								(pNodeFMs)[i_NodeNum].vl_Q_Di1 = (cBuffer)[1].im;
							}
							break;
							default:
							  ;
							break;
						}
					}
				}
				if((pNodeFMs)[i_NodeNum].ldIdx2 > 0)
				{
					int stop = 0;
					PElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->PCElements.Get((pNodeFMs)[i_NodeNum].ldIdx2));
					Num = PElement->Get_NPhases();
					cBuffer = (pComplexArray)realloc(cBuffer, sizeof(complex) * Num);
					PElement->GetPhasePower(cBuffer, ActorID);// power
					for(stop = Num, j = 1; j <= stop; j++)
					{
						i = PElement->Terminals[0].TermNodeRef[j - 1];
						switch((ActiveCircuit[ActorID]->MapNodeToBus)[i - 1].NodeNum)
						{
							case 	1:
							{
								(pNodeFMs)[i_NodeNum].vl_P_Di2 = (cBuffer)[1].re;
								(pNodeFMs)[i_NodeNum].vl_Q_Di2 = (cBuffer)[1].im;
							}
							break;
							case 	2:
							{
								(pNodeFMs)[i_NodeNum].vl_P_Di2 = (cBuffer)[1].re;
								(pNodeFMs)[i_NodeNum].vl_Q_Di2 = (cBuffer)[1].im;
							}
							break;
							case 	3:
							{
								(pNodeFMs)[i_NodeNum].vl_P_Di2 = (cBuffer)[1].re;
								(pNodeFMs)[i_NodeNum].vl_Q_Di2 = (cBuffer)[1].im;
							}
							break;
							default:
							  ;
							break;
						}
					}
				}
				if((pNodeFMs)[i_NodeNum].ldIdx3 > 0)
				{
					int stop = 0;
					PElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->PCElements.Get((pNodeFMs)[i_NodeNum].ldIdx3));
					Num = PElement->Get_NPhases();
					cBuffer = (pComplexArray)realloc(cBuffer, sizeof(complex) * Num);
					PElement->GetPhasePower(cBuffer, ActorID);// power
					for(stop = Num, j = 1; j <= stop; j++)
					{
						i = (PElement->Terminals[0].TermNodeRef)[j - 1];
						switch((ActiveCircuit[ActorID]->MapNodeToBus)[i - 1].NodeNum)
						{
							case 	1:
							{
								(pNodeFMs)[i_NodeNum].vl_P_Di3 = (cBuffer)[1].re;
								(pNodeFMs)[i_NodeNum].vl_Q_Di3 = (cBuffer)[1].im;
							}
							break;
							case 	2:
							{
								(pNodeFMs)[i_NodeNum].vl_P_Di3 = (cBuffer)[1].re;
								(pNodeFMs)[i_NodeNum].vl_Q_Di3 = (cBuffer)[2].im;
							}
							break;
							case 	3:
							{
								(pNodeFMs)[i_NodeNum].vl_P_Di3 = (cBuffer)[1].re;
								(pNodeFMs)[i_NodeNum].vl_Q_Di3 = (cBuffer)[3].im;
							}
							break;
							default:
							  ;
							break;
						}
					}
				}
			}
			break;
//            2:
//            begin
//
//            end;
//            3:
//            begin
//
//            end
			default:
			  ;
			break;
		}
	}
}

void TFMonitorObj::Init_nodeFM(int iNodeNum, int ActorID)
{
	String StrTemp;
	int DevIndex = 0;
	int i = 0;
	int j = 0;
	int PCindex_ld = 0;
	TDSSCktElement* PElem = nullptr;
	TGeneric5Obj* pDG = nullptr;
	TLoadObj* pLd = nullptr;
    //pPDElem : TPDElement;
	TAdjArray lstPC;
	int Num = 0;
	complex ctmp = {};
	pComplexArray cBuffer = nullptr;

    //init all info of this node
	/*# with pNodeFMs^[iNodeNum] do */
	{
		
    //1
		int stop = 0;
		StrTemp = LowerCase((pNodeFMs)[iNodeNum].vl_strBusName);
		(pNodeFMs)[iNodeNum].Bus_Idx = ActiveCircuit[ActorID]->BusList.Find(StrTemp);
		DevIndex = GetCktElementIndex((pNodeFMs)[iNodeNum].vl_strMeasuredName);                   // Global function
		if(DevIndex > 0)                                       // Monitored element must already exist
		{
			PElem = ((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		}
		else
		{
			return;
		}
		(pNodeFMs)[iNodeNum].vl_ndphases = PElem->Get_NPhases();
		(pNodeFMs)[iNodeNum].vl_basevolt = ActiveCircuit[ActorID]->Buses[(pNodeFMs)[iNodeNum].Bus_Idx - 1]->kVBase * 1000;
		(pNodeFMs)[iNodeNum].vl_phase_num_dg = -1; //-1 -- no dg under this nodes;0 --3 phases by default
		for(stop = 3, j = 1; j <= stop; j++)
		{
			(pNodeFMs)[iNodeNum].vl_nodeType_phase[j - 1] = 2;
		}// by default not dg
		/*# with ActiveCircuit[ActorID] do */
		{
			
			PElem = ((TDSSCktElement*) ActiveCircuit[ActorID]->PCElements.Get_First());
			PCindex_ld = ActiveCircuit[ActorID]->PCElements.get_myActiveItem();
			while(PElem != nullptr)
			{
				if(PElem->Get_Enabled())
				{
					if(PElem->ActiveTerminal->BusRef == (pNodeFMs)[iNodeNum].Bus_Idx)
					{
						if(LowerCase(((TDSSObject*)PElem)->Get_myPName()).find("generic5") != String::npos)

                                  //vl_nodeType should be define per phase
						{
							(pNodeFMs)[iNodeNum].vl_ndphases_dg = PElem->Get_NPhases(); //1 or 3
                                                                     // under 1 bus, there can be either 3 phase DG or 1 phase dg
                                                                     //set Cluster defination for DG
                                                                     // one 3phase dg
                                                                     // 1,2 or 3  1-phase dgs under each phase
							pDG = ((TGeneric5Obj*) PElem);
							Num = Trunc(pDG->Get_Variable(30)); //ctrl_mode
							switch(Num)
							{
								case 	1:
								(pNodeFMs)[iNodeNum].vl_nodeType_phase[0] = 1;
								break;
								case 	2:
								(pNodeFMs)[iNodeNum].vl_nodeType_phase[1] = 1;
								break;
								case 	3:
								(pNodeFMs)[iNodeNum].vl_nodeType_phase[2] = 1;
								break;
								default:
								int stop = 0;
								for(stop = 3, j = 0; j < stop; j++)
								{
									(pNodeFMs)[iNodeNum].vl_nodeType_phase[j] = 1;
								} // //ctrl_mode = 4 or 0
								break;
							}
                                   //pDG
							if(pDG->FMonObj == nullptr) // first cluster
							{
								pDG->Set_Variable(28, (double) Cluster_num); //28:  TPCElement(self).cluster_num :=  trunc(Value);
                                                                         //if cluster_num >= 1 then      // assign the virtue leader to this DG
								pDG->FMonObj = this;    //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num); cluster_num can not be used for 'Get'
								pDG->Set_Variable(29, (double) (iNodeNum + 1)); //29:  TPCElement(self).NdNumInCluster := trunc(Value) ;
								pDG->Set_Variable(30, 1);         //TPCElement(self).nVLeaders := trunc(Value) ;
							}
							else
    // the second virtual leader // which means if the 2nd one will always be the one being overwritten
							{
                                            //
								if(Cluster_num != Round(pDG->Get_Variable(28)))
								{
									pDG->Set_Variable(31, (double) Cluster_num); //28:  TPCElement(self).cluster_num2 :=  trunc(Value);
                                                                         //if cluster_num >= 1 then      // assign the virtue leader to this DG
									pDG->FMonObj2 = this;             //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num); cluster_num can not be used for 'Get'
									pDG->Set_Variable(32, (double) (iNodeNum + 1));    //29:  TPCElement(self).NdNumInCluster2 := trunc(Value) ;
									pDG->Set_Variable(30, 2);           //  TPCElement(self).nVLeaders := trunc(Value) ;
								}
							}
							(pNodeFMs)[iNodeNum].vl_phase_num_dg = 0; //3 phases by default
							if((pNodeFMs)[iNodeNum].vl_ndphases_dg == 1)
								(pNodeFMs)[iNodeNum].vl_phase_num_dg = Trunc(pDG->Get_Variable(30));  //if vl_nodeType=1, and vl_ndphases=1,phase_num =1,2,3 0- this node has 3 phases
                                                                         // 30, ctrl_mode, is the phase number of this Generic5
						}
						(pNodeFMs)[iNodeNum].ldType = -1;
						(pNodeFMs)[iNodeNum].ldIdx = -1;
						(pNodeFMs)[iNodeNum].ldIdx1 = -1;
						(pNodeFMs)[iNodeNum].ldIdx2 = -1;
						(pNodeFMs)[iNodeNum].ldIdx3 = -1;
						if(LowerCase(((TDSSObject*)PElem)->Get_myPName()).find("load") != String::npos)
						{
							Num = PElem->Get_NPhases();
							pLd = ((TLoadObj*) PElem);
                            //Devindex :=
							if(Num == 3)
							{
								(pNodeFMs)[iNodeNum].ldIdx = PCindex_ld;
								(pNodeFMs)[iNodeNum].ldType = 0;
							}
							else
							{
								if(Num == 2)
								{
									(pNodeFMs)[iNodeNum].ldIdx = PCindex_ld;
									(pNodeFMs)[iNodeNum].ldType = 0;
								}
								else
								{
									if(Num == 1)
									{
										i = (PElem->Terminals[0].TermNodeRef)[0];
										switch((ActiveCircuit[ActorID]->MapNodeToBus)[i - 1].NodeNum)
										{
											case 	1:
											(pNodeFMs)[iNodeNum].ldIdx1 = PCindex_ld;
											break;
											case 	2:
											(pNodeFMs)[iNodeNum].ldIdx2 = PCindex_ld;
											break;
											case 	3:
											(pNodeFMs)[iNodeNum].ldIdx3 = PCindex_ld;
											break;
											default:
											  ;
											break;
										}
										if((pNodeFMs)[iNodeNum].ldType < 1)
											(pNodeFMs)[iNodeNum].ldType = 1;
										else
										{
											if((pNodeFMs)[iNodeNum].ldType >= 1)
												(pNodeFMs)[iNodeNum].ldType = (pNodeFMs)[iNodeNum].ldType + 1;
										}
										if((pNodeFMs)[iNodeNum].ldType >= 3)
											(pNodeFMs)[iNodeNum].ldType = 3;
									}
								}
                            //

//                            cBuffer := Allocmem(sizeof(cBuffer^[1])*num);
//                            pElem.GetPhasePower(cBuffer);// power
//                            for j := 1 to num do
//                                     begin
//                                       i := pElem.Terminals^[1].TermNodeRef^[j];
//                                       case activecircuit.MapNodeToBus^[i].NodeNum of
//                                       1: begin vl_P_Di1 := cBuffer^[1].re; vl_Q_Di1 := cBuffer^[1].im;  end;
//                                       2: begin vl_P_Di1 := cBuffer^[2].re; vl_Q_Di1 := cBuffer^[2].im;  end;
//                                       3: begin vl_P_Di1 := cBuffer^[3].re; vl_Q_Di1 := cBuffer^[3].im;  end;
//                                       end;
//                                     end;
//
//                            Reallocmem(cBuffer,0);
							}
						}
					}
				}
				PElem = ((TDSSCktElement*) ActiveCircuit[ActorID]->PCElements.Get_Next());
				PCindex_ld = ActiveCircuit[ActorID]->PCElements.get_myActiveItem();
			}
		}
        //lstPC := ActiveCircuit.GetBusAdjacentPCLists;
        //activecircuit.MapNodeToBus
        //num := lstPC.NumShuntObjects;

   //2


         // will be overwritten if this nodeFM is a 1-phase Generic5
        ////// update cluster_num and node number of this DG in this cluster
		        /*if vl_nodeType=1 then   // this only work for DG
        begin
             //set
             pDG := TGeneric5Obj(pElement);
             //pDG
             if pDG.FMonObj = nil then // first cluster
             begin

                 pDG.Set_Variable(28,cluster_num); //28:  TPCElement(self).cluster_num :=  trunc(Value);
                                                   //if cluster_num >= 1 then      // assign the virtue leader to this DG
                 pDG.FMonObj := self;    //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num); cluster_num can not be used for 'Get'
                 pDG.Set_Variable(29,iNodeNum); //29:  TPCElement(self).NdNumInCluster := trunc(Value) ;
                 pDG.Set_Variable(30,1);         //TPCElement(self).nVLeaders := trunc(Value) ;
             end else
             begin    // the second virtual leader // which means if the 2nd one will always be the one being overwritten

                 pDG.Set_Variable(31,cluster_num); //28:  TPCElement(self).cluster_num2 :=  trunc(Value);
                                                   //if cluster_num >= 1 then      // assign the virtue leader to this DG
                 pDG.FMonObj2 := self;             //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num); cluster_num can not be used for 'Get'
                 pDG.Set_Variable(32,iNodeNum);    //29:  TPCElement(self).NdNumInCluster2 := trunc(Value) ;
                 pDG.Set_Variable(30,2);           //  TPCElement(self).nVLeaders := trunc(Value) ;
             end;
             if vl_ndphases_dg=1 then
                vl_phase_num_dg := trunc(pDG.Get_Variable(30));  //if vl_nodeType=1, and vl_ndphases=1,phase_num =1,2,3 0- this node has 3 phases
                                                       // 30, ctrl_mode, is the phase number of this Generic5
        end;  */
        /*if vl_nodeType=2 then   // line and tranformer
        begin
             //set
             //pPDelem := TPDElement(pElement);
             //pDG
        end;
        */
        ///
		(pNodeFMs)[iNodeNum].vl_V_ref1_dg = (pNodeFMs)[iNodeNum].vl_V_ref_dg;//:=1000*2.4; 1000*2.4; V_ref2:=1000*2.4; V_ref3 :=1000*2.4;// norminal value with respect to p.u. 1  //must be set by initialization
		(pNodeFMs)[iNodeNum].vl_V_ref2_dg = (pNodeFMs)[iNodeNum].vl_V_ref_dg;
		(pNodeFMs)[iNodeNum].vl_V_ref3_dg = (pNodeFMs)[iNodeNum].vl_V_ref_dg;
        //kcq := 1.0; // the step size gain of agent i //has to be defined befor used

        ///  other properties if needed
        //ndphases : integer; //how many phases of this device on this node; not those of node
		(pNodeFMs)[iNodeNum].vl_CC_switch_dg = false; // cooperate control switch. true, cooperate control is on
		(pNodeFMs)[iNodeNum].vl_PF_flag_dg = 0;//1, real power control is on
		(pNodeFMs)[iNodeNum].vl_QV_flag_dg = 0;//1, volt/var control is on
		(pNodeFMs)[iNodeNum].vl_volt_thrd_dg = 0.03;
		(pNodeFMs)[iNodeNum].vl_Alpha_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Alpha1_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Alpha2_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Alpha3_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Gradient_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Gradient1_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Gradient2_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Gradient3_dg = 0;
		(pNodeFMs)[iNodeNum].vl_AlphaP_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Alpha_dgn = 0;
		(pNodeFMs)[iNodeNum].vl_AlphaP1_dg = 0;
		(pNodeFMs)[iNodeNum].vl_AlphaP2_dg = 0;
		(pNodeFMs)[iNodeNum].vl_AlphaP3_dg = 0;
		(pNodeFMs)[iNodeNum].vl_GradientP_dg = 0;
		(pNodeFMs)[iNodeNum].vl_GradientP1_dg = 0;
		(pNodeFMs)[iNodeNum].vl_GradientP2_dg = 0;
		(pNodeFMs)[iNodeNum].vl_GradientP3_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Pmax_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Qmax_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Pmax_phase_dg = 0;
		(pNodeFMs)[iNodeNum].vl_Qmax_phase_dg = 0;
		(pNodeFMs)[iNodeNum].vl_V_base_dg = 1000 * 2.4;
		(pNodeFMs)[iNodeNum].vl_V = 1000 * 2.4;
		(pNodeFMs)[iNodeNum].vl_V1 = 1000 * 2.4;
		(pNodeFMs)[iNodeNum].vl_V2 = 1000 * 2.4;
		(pNodeFMs)[iNodeNum].vl_V3 = 1000 * 2.4;
		(pNodeFMs)[iNodeNum].vl_Q_Di = 0.0;
		(pNodeFMs)[iNodeNum].vl_Q_Di1 = 0.0;
		(pNodeFMs)[iNodeNum].vl_Q_Di2 = 0.0;
		(pNodeFMs)[iNodeNum].vl_Q_Di3 = 0.0;
		(pNodeFMs)[iNodeNum].vl_P_Di = 0.0;
		(pNodeFMs)[iNodeNum].vl_P_Di1 = 0.0;
		(pNodeFMs)[iNodeNum].vl_P_Di2 = 0.0;
		(pNodeFMs)[iNodeNum].vl_P_Di3 = 0.0;
		(pNodeFMs)[iNodeNum].vl_SmplCnt = 0;
		(pNodeFMs)[iNodeNum].vl_crnt_smp_time = 0.0;
        // attack and defense
		(pNodeFMs)[iNodeNum].d_atk = 0.0;
		(pNodeFMs)[iNodeNum].z_dfs = 0.0;
		(pNodeFMs)[iNodeNum].z_dfsn = 0.0;
		(pNodeFMs)[iNodeNum].d_atk0 = 0.0;

		// init the remaining to zero
		(pNodeFMs)[iNodeNum].vl_V_c = CZero;
		(pNodeFMs)[iNodeNum].vl_V_1c = CZero;
		(pNodeFMs)[iNodeNum].vl_V_2c = CZero;
		(pNodeFMs)[iNodeNum].vl_V_3c = CZero;
		(pNodeFMs)[iNodeNum].vl_q_DG = 0;
	}
}

void TFMonitorObj::Get_PDElem_terminal_voltage(int nd_num_in_cluster, String devName, int Tern_num, int ActorID)
{
	TPowerTerminal* tempTerminal = nullptr;
	int i = 0;
	int DevIndex = 0;
	int j = 0;
	TDSSCktElement* tempElement = nullptr;
    //VAR
   //pElem:TDSSCktElement;
	int phase_num = 0;
	double vabs = 0.0;
    //V012 :TSymCompArray5;
	complex V012[3]		= { cmplx(0,0), cmplx(0,0) , cmplx(0,0) };
	complex VaVbVc[4]	= { cmplx(0,0), cmplx(0,0) , cmplx(0,0) , cmplx(0,0) };
	int stop = 0;
	DevIndex = GetCktElementIndex(devName);                   // Global function
	if(DevIndex > 0)                                       // Monitored element must already exist
	{
		tempElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
	}
              //
	tempTerminal = &(tempElement->Terminals)[Tern_num - 1];
	for(stop = tempElement->Get_NPhases(), j = 1; j <= stop; j++)
	{// how many phases of this element
		i = (tempTerminal->TermNodeRef)[j - 1];  // global node number
		phase_num = (ActiveCircuit[ActorID]->MapNodeToBus)[i - 1].NodeNum;
		if(!ADiakoptics || (ActorID == 1))
			vabs = cabs(ActiveCircuit[ActorID]->Solution->NodeV[i]);
		else
			vabs = cabs(ActiveCircuit[ActorID]->Solution->VoltInActor1(i));
		if(phase_num == 1) // phase A
		{
			(pNodeFMs)[nd_num_in_cluster].vl_V1 = vabs;
			if(!ADiakoptics || (ActorID == 1))
				(pNodeFMs)[nd_num_in_cluster].vl_V_1c = ActiveCircuit[ActorID]->Solution->NodeV[i];
			else
				(pNodeFMs)[nd_num_in_cluster].vl_V_1c = ActiveCircuit[ActorID]->Solution->VoltInActor1(i);
		}
		else
		{
			if(phase_num == 2)    //phase B
			{
				(pNodeFMs)[nd_num_in_cluster].vl_V2 = vabs;
				if(!ADiakoptics || (ActorID == 1))
					(pNodeFMs)[nd_num_in_cluster].vl_V_2c = ActiveCircuit[ActorID]->Solution->NodeV[i];
				else
					(pNodeFMs)[nd_num_in_cluster].vl_V_2c = ActiveCircuit[ActorID]->Solution->VoltInActor1(i);
			}
			else
			{
				if(phase_num == 3)    //phase c
				{
					(pNodeFMs)[nd_num_in_cluster].vl_V3 = vabs;
					if(!ADiakoptics || (ActorID == 1))
						(pNodeFMs)[nd_num_in_cluster].vl_V_3c = ActiveCircuit[ActorID]->Solution->NodeV[i];
					else
						(pNodeFMs)[nd_num_in_cluster].vl_V_3c = ActiveCircuit[ActorID]->Solution->VoltInActor1(i);
				}
			}
		}
	}
	if(tempElement->Get_NPhases() == 3)
	{
		VaVbVc[1] = (pNodeFMs)[nd_num_in_cluster].vl_V_1c;//phase A
		VaVbVc[2] = (pNodeFMs)[nd_num_in_cluster].vl_V_2c;
		VaVbVc[3] = (pNodeFMs)[nd_num_in_cluster].vl_V_3c;
		Phase2SymComp(&VaVbVc[1],&V012[0]);  // Convert abc voltages to 012
		(pNodeFMs)[nd_num_in_cluster].vl_V = cabs(V012[1]);  //pos. seq. Voltage
	}
} //PD nodes

void TFMonitorObj::update_all_nodes_info(int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		/*# with pNodeFMs^[i] do */
		{
			
              //if vl_nodeType = 1 then // generic dg
              //begin
                   // do nothing; because the info is updated from the other end (sent by DGs)
              //end
              //else if vl_nodeType = 2 then // PD elements: lines, xformers, etc..
              //begin
                   //Update_PD_Node_Info(i,vl_strMeasuredName,vl_terminalNum);
			Get_PDElem_terminal_voltage(i, (pNodeFMs)[i].vl_strMeasuredName, (pNodeFMs)[i].vl_terminalNum, ActorID);
                   //Calc_Alpha_for_PDNode(i ); // calc all alpha, alpha1, alpha2, alph3 of all pdelement nodes
              //end;
		}
	}
}
/*--------------------------------------------------------------------------*/

void TFMonitorObj::ResetDelaySteps(int iNodeNum)
{
	int j = 0;
	int Tmp = 0;
          //calc delay array
	int stop = 0;
	for(stop = Nodes, j = 0; j < stop; j++)
	{
		if((T_intvl_smpl == 0.0) || ((pCommDelayMatrix)[iNodeNum * Nodes + j] == 0.0))
			(pCommDelaySteps)[iNodeNum * Nodes + j] = 0;
		else
		{
			Tmp = Trunc((pCommDelayMatrix)[iNodeNum * Nodes + j] / T_intvl_smpl);
			if(Frac((pCommDelayMatrix)[iNodeNum * Nodes + j] / T_intvl_smpl) == 0.0L)
				(pCommDelaySteps)[iNodeNum * Nodes + j] = (short int) Tmp;
			else
				(pCommDelaySteps)[iNodeNum * Nodes + j] = (short int) (Tmp + 1);
                      //How many delays for communication
		}
	}
}

void TFMonitorObj::ResetIt(int ActorID)
{
	int iTmp = 0;
	BufPtr = 0;
     //ClearMonitorStream;
	if(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode == DYNAMICMODE)
          //calc Delay_stps for sampling
	{
		int stop = 0;
		if(T_intvl_smpl == 0.0)  //No delay.
			Smpl_stps = 0;
		else
		{
			iTmp = Trunc(T_intvl_smpl / ActiveCircuit[ActorID]->Solution->DynaVars.h);
			if(Frac(T_intvl_smpl / ActiveCircuit[ActorID]->Solution->DynaVars.h) == 0.0L)
				Smpl_stps = iTmp;
			else
				Smpl_stps = iTmp + 1;// uper
		}
		for(stop = Nodes, iTmp = 0; iTmp < stop; iTmp++)
		{
			(pNodeFMs)[iTmp].vl_SmplCnt = 0;
			(pNodeFMs)[iTmp].vl_crnt_smp_time = ActiveCircuit[ActorID]->Solution->DynaVars.intHour * 3600 + ActiveCircuit[ActorID]->Solution->DynaVars.T;
			Init_delay_array(iTmp, ActorID); // in DYNAMICMODE, init alpha array
		}
	}
}

/*--------------------------------------------------------------------------*/
/*
Procedure TFMonitorObj.PostProcess;
Begin
  if IsProcessed = FALSE then begin
    if (mode = 4) and (MonitorStream.Position > 0) then DoFlickerCalculations;
  end;
  IsProcessed := TRUE;
End;

{--------------------------------------------------------------------------*/
/*
Procedure TFMonitorObj.GetFvalue;
VAR
    dHour             :Double;
    dSum              :Double;
    i,j               :Integer;
    IsPower           :Boolean;
    IsSequence        :Boolean;
    NumVI             :Integer;
    Offset            :Integer;
    ResidualCurr      :Complex;
    ResidualVolt      :Complex;
    Sum               :Complex;
    V012,I012         :Array[1..3] of Complex;

     Monitor : TFMonitorObj;


Begin

   If Not (ValidMonitor and Enabled) Then Exit;

   //inc(SampleCount);

   Hour := ActiveCircuit.Solution.DynaVars.intHour;
   Sec :=  ActiveCircuit.Solution.Dynavars.t;

   Offset := (MeteredTerminal-1)  * MeteredElement.NConds;

   //Save time unless Harmonics mode and then save Frequency and Harmonic
   WITH ActiveCircuit.Solution Do
     IF IsHarmonicModel Then Begin
         AddDblsToBuffer(@Frequency, 1);  // put freq in hour slot as a double
         AddDblsToBuffer(@Harmonic ,1);  // stick harmonic in time slot in buffer
     End
     ELSE Begin
         dHour := Hour;      // convert to double
         AddDblsToBuffer(@dHour, 1);  // put hours in buffer as a double
         AddDblsToBuffer(@Sec, 1);  // stick time in sec in buffer
     End;

   CASE  (Mode AND MODEMASK) of

     0,1:       // Voltage, current. Powers
       Begin

            // MeteredElement.GetCurrents(CurrentBuffer);
            // To save some time, call ComputeITerminal
            MeteredElement.ComputeIterminal;   // only does calc if needed
            For i := 1 to MeteredElement.Yorder Do CurrentBuffer^[i] := MeteredElement.Iterminal^[i];

            TRY

                Bus_code := Activeterminal.BusRef;
                Monitor := ActiveCircuit.FMonitors.Active;
              //Detect non-existed phase======================================================
              for j := 1 to 3 do
                 begin
                   NodeNum := ActiveCircuit.Buses^[Bus_code].Find(j);
                   if NodeNum=0 then
                   begin
                    F_Value_one_V^[j] := 9999;
                   end
                   else
                   begin

                    for i := 1 to Fnphases do
                       begin
                         Node_Ref := Monitor.ActiveTerminal.TermNodeRef^[i];
                         if Node_Ref=NodeNum then  F_Value_one_V^[j] := cabs(ActiveCircuit.Solution.NodeV^[NodeRef^[i]])/ActiveCircuit.Solution.NodeVbase^[NodeRef^[i]];
                       end;

                   end;
                 end;

              FOR i := 1 to Fnconds DO
              Begin
                // It is the index of the terminal into the system node list
                  //Meteredelement.
                  f_value_one := cabs(ActiveCircuit.Solution.NodeV^[NodeRef^[1]]); //A value of voltage on first phase
                  f_value_one := f_value_one/ActiveCircuit.Solution.NodeVbase^[NodeRef^[i]] ;
                  VoltageBuffer^[i] := ActiveCircuit.Solution.NodeV^[NodeRef^[i]];
              End;
            EXCEPT
               On E:Exception Do DoSimpleMsg(E.Message + CRLF + 'NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.', 672);
            END;
       End;

     2: Begin     // Monitor Transformer Tap Position

              With TTransfObj(MeteredElement) Do Begin
                   //AddDblToBuffer(PresentTap[MeteredTerminal]);
              End;
              Exit;  // Done with this mode now.
        End;

     3: Begin   // Pick up device state variables
              TPCElement(MeteredElement).GetAllVariables(StateBuffer);
              AddDblsToBuffer(StateBuffer, NumStateVars);
              Exit; // Done with this mode now
        End;

     4: Begin   // RMS phase voltages for flicker evaluation
            TRY
              FOR i := 1 to Fnphases DO Begin
                  FlickerBuffer^[i] := ActiveCircuit.Solution.NodeV^[NodeRef^[i]];
              End;
            EXCEPT
               On E:Exception Do DoSimpleMsg(E.Message + CRLF + 'NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.', 672);
            END;
        End;

     5: Begin
            (* Capture Solution Variables *)
            With ActiveCircuit.Solution Do Begin
             SolutionBuffer^[1]   :=  Iteration;
             SolutionBuffer^[2]   :=  ControlIteration;
             SolutionBuffer^[3]   :=  MaxIterations;
             SolutionBuffer^[4]   :=  MaxControlIterations;
             If ConvergedFlag then SolutionBuffer^[5] := 1 else SolutionBuffer^[5] := 0;
             SolutionBuffer^[6]   :=  IntervalHrs;
             SolutionBuffer^[7]   :=  SolutionCount;
             SolutionBuffer^[8]   :=  Mode;
             SolutionBuffer^[9]   :=  Frequency;
             SolutionBuffer^[10]  :=  Year;
             SolutionBuffer^[11]  :=  Time_Solve;
             SolutionBuffer^[12]  :=  Time_Step;
            End;

        End;

     6: Begin     // Monitor Transformer Tap Position

              With TCapacitorObj(MeteredElement) Do Begin
                  for i := 1 to NumSteps do
                    begin
                      //AddDblToBuffer(States[i]);
                    end;
              End;
              Exit;  // Done with this mode now.
        End;
        //
     Else Exit  // Ignore invalid mask

   End;


   IF ((Mode AND SEQUENCEMASK)>0) And (Fnphases=3)
   THEN Begin  // Convert to Symmetrical components
       Phase2SymComp(VoltageBuffer, @V012);
       Phase2SymComp(@CurrentBuffer^[Offset + 1], @I012);
       NumVI      := 3;
       IsSequence := TRUE;
       // Replace voltage and current buffer with sequence quantities
       FOR i := 1 to 3 DO VoltageBuffer^[i]         := V012[i];
       FOR i := 1 to 3 DO CurrentBuffer[Offset + i] := I012[i];
   End
   ELSE Begin
       NumVI      :=Fnconds;
       IsSequence := FALSE;
   End;

   IsPower := False;  // Init so compiler won't complain
   CASE  (Mode AND MODEMASK) of
     0: Begin        // Convert to Mag, Angle  // and compute residual if required
          IsPower := FALSE;
          IF IncludeResidual THEN Begin
             If VIPolar Then Begin
                 ResidualVolt := ResidualPolar(@VoltageBuffer^[1], Fnphases);
                 ResidualCurr := ResidualPolar(@CurrentBuffer^[Offset+1], Fnphases);
             End Else Begin
                 ResidualVolt := Residual(@VoltageBuffer^[1], Fnphases);
                 ResidualCurr := Residual(@CurrentBuffer^[Offset+1], Fnphases);
             End;
          End;
          If VIPolar Then Begin
             ConvertComplexArrayToPolar(VoltageBuffer, NumVI);
             ConvertComplexArrayToPolar(@CurrentBuffer^[Offset+1], NumVI );    // Corrected 3-11-13

             ConvertComplexArrayToPolar(F_Value_one_S, NumVI);

          End;


          //Calulate power in mode 0
          CalckPowers(F_Value_one_S, VoltageBuffer, @CurrentBuffer^[Offset+1], NumVI);
          IF (IsSequence OR ActiveCircuit.PositiveSequence) THEN  CmulArray(F_Value_one_S, 3.0, NumVI); // convert to total power
           Fvalue_P :=0 ;
           Fvalue_Q :=0 ;
          FOR i := 1 to NumVI DO
          begin
            Fvalue_P := Fvalue_P + F_Value_one_S^[i].re;
            Fvalue_Q := Fvalue_Q + F_Value_one_S^[i].im;
          end;


        End;
     1: Begin     // Convert Voltage Buffer to power kW, kvar or Mag/Angle
          CalckPowers(VoltageBuffer, VoltageBuffer, @CurrentBuffer^[Offset+1], NumVI);
          IF (IsSequence OR ActiveCircuit.PositiveSequence) THEN  CmulArray(VoltageBuffer, 3.0, NumVI); // convert to total power
          f_p_one :=0 ;
          f_q_one :=0 ;
          FOR i := 1 to NumVI DO
          begin
            f_p_one := f_p_one + VoltageBuffer^[i].re;
            f_q_one := f_q_one + VoltageBuffer^[i].im;
          end;

          If Ppolar Then ConvertComplexArrayToPolar(VoltageBuffer, NumVI);
          IsPower := TRUE;
        End;
     4: Begin
          IsPower := FALSE;
          ConvertComplexArrayToPolar(FlickerBuffer, Fnphases);
        End
   Else
   End;


End;
 */
/**/
/*--------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*/
/*
Procedure TFMonitorObj.AddDblsToBuffer( Dbl:pDoubleArray; Ndoubles:Integer);

VAR
   i:Integer;

Begin
   //FOR i := 1 to Ndoubles DO AddDblToBuffer(Dbl^[i]);
End;
*/

/*--------------------------------------------------------------------------*/
/*
Procedure TFMonitorObj.AddDblToBuffer(const Dbl:Double);

Begin
    // first check to see if there's enough room
    // if not, save to monitorstream first.
    IF BufPtr=BufferSize THEN Save;
    Inc(BufPtr);
    MonBuffer^[BufPtr]:=Dbl;
End;
*/
/*
Procedure TFMonitorObj.DoFlickerCalculations;
var
  FSignature  :Integer;
  Fversion    :Integer;
  RecordSize  :Cardinal;
  RecordBytes :Cardinal;
  SngBuffer   :Array[1..100] of Single;
  hr          :single;
  s           :single;
  N           :Integer;
  Npst        :Integer;
  i, p        :Integer;
  bStart      :Integer;
  data        :Array of pSingleArray; // indexed from zero (time) to FnPhases
  pst         :Array of pSingleArray; // indexed from zero to FnPhases - 1
  ipst        :integer;
  tpst        :single;
  defaultpst  :single;
  Vbase       :single;
  busref      :integer;
begin
  N := SampleCount;
  With MonitorStream Do Begin
    Seek(0, soFromBeginning);  // Start at the beginning of the Stream
    Read( Fsignature, Sizeof(Fsignature));
    Read( Fversion,   Sizeof(Fversion));
    Read( RecordSize, Sizeof(RecordSize));
    Read( Mode,       Sizeof(Mode));
    Read( StrBuffer,  Sizeof(StrBuffer));
    bStart := Position;
  End;
  RecordBytes := Sizeof(SngBuffer[1]) * RecordSize;
  Try
    // read rms voltages out of the monitor stream into arrays
    SetLength (data, Fnphases + 1);
    SetLength (pst, Fnphases);
    for p := 0 to FnPhases do data[p] := AllocMem (Sizeof(SngBuffer[1]) * N);
    i := 1;
    while Not (MonitorStream.Position>=MonitorStream.Size) do Begin
      With MonitorStream Do Begin
        Read( hr, SizeOf(hr));
        Read( s,  SizeOf(s));
        Read(SngBuffer, RecordBytes);
        data[0][i] := s + 3600.0 * hr;
        for p := 1 to FnPhases do data[p][i] := SngBuffer[2*p];
        i := i + 1;
      End;
    End;

    // calculate the flicker level and pst
    Npst := 1 + Trunc (data[0][N] / 600.0); // pst updates every 10 minutes or 600 seconds
    for p := 0 to FnPhases-1 do begin
      pst[p] := AllocMem (Sizeof(SngBuffer[1]) * Npst);
      busref := MeteredElement.Terminals[MeteredTerminal].BusRef;
      Vbase := 1000.0 * ActiveCircuit.Buses^[busref].kVBase;
      FlickerMeter (N, BaseFrequency, Vbase, data[0], data[p+1], pst[p]);
    end;

    // stuff the flicker level and pst back into the monitor stream
    with MonitorStream do begin
      Position := bStart;
      tpst:=0.0;
      ipst:=0;
      defaultpst:=0;
      for i := 1 to N do begin
        if (data[0][i] - tpst) >= 600.0 then begin
          inc(ipst);
          tpst:=data[0][i];
        end;
        Position:=Position + 2 * SizeOf(hr); // don't alter the time
        for p := 1 to FnPhases do begin
          Write (data[p][i], sizeof(data[p][i]));
          if (ipst > 0) and (ipst <= Npst) then
            Write (pst[p-1][ipst], sizeof(pst[p-1][ipst]))
          else
            Write (defaultpst, sizeof(defaultpst))
        end;
      end;
    end;
  Finally
    for p := 0 to FnPhases do ReAllocMem (data[p], 0);
    for p := 0 to FnPhases-1 do ReAllocMem (pst[p], 0);
  end;
end;
*/
/*--------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------*/  //Get present value of terminal Curr for reports

void TFMonitorObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;

/*
  Revised 12-7-99 to return Zero current instead of Monitored element current because
 it was messing up Newton iteration.
*/
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

void TFMonitorObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*--------------------------------------------------------------------------*/

void TFMonitorObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	int k = 0;
	TDSSCktElement::DumpProperties(f, Complete);
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
		{ Write(f, "// BufferSize="); WriteLn(f, BufferSize, 0); }
		{ Write(f, "// Hour="); WriteLn(f, Hour, 0); }
		{ Write(f, "// Sec="); WriteLn(f, Sec, 0); }
		{ Write(f, "// BaseFrequency="); WriteLn(f, BaseFrequency, 0, 1); }
		{ Write(f, "// Bufptr="); WriteLn(f, BufPtr, 0); }
		WriteLn(f, "// Buffer=");
		k = 0;
		for(stop = BufPtr, i = 1; i <= stop; i++)
		{
			{ Write(f, (MonBuffer)[i], 0, 1); Write(f, ", "); }
			++k;
			if(k == (2 + Fnconds * 4))
			{
				WriteLn(f);
				k = 0;
			}
		}
		WriteLn(f);
	}
}

void TFMonitorObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,""); //'element';
	Set_PropertyValue(2,"1"); //'terminal';
	Set_PropertyValue(3,"0"); //'mode';
	Set_PropertyValue(4,""); // 'action';  // buffer=clear|save|take|process
	Set_PropertyValue(5,"NO");
	Set_PropertyValue(6,"YES");
	Set_PropertyValue(7,"YES");
     /**/
	Set_PropertyValue(8,"0");//ref power
	Set_PropertyValue(9,"0");// default voltage sensor value
	Set_PropertyValue(10,"0");// default power sensor value
	Set_PropertyValue(11,"1");// default node number value
	Set_PropertyValue(12,"1");// default by group 1
	Set_PropertyValue(13,"1");// default number of groups
     /**/
	TDSSCktElement::InitPropertyValues(NumPropsThisClass);
}


/*--------------------------------------------------------------------------*/


/*----------------------------------------------------------------------*/

//

//must be called after self gradient calc
// self gradient calc is in 'Update _pd_node_info'

void TFMonitorObj::Calc_Alpha_for_PDNode(int NodeNum)
{
	int j = 0;
	int phase_num = 0;
	double sum_Sij_j = 0.0;
	double TempAlpha = 0.0;
     //if phase_num=0 then
     //     result := 0.0 //alpha
     //else if phase_num=1 then
     //     result:= 0.0  //alpha1
     //     else if phase_num=2 then
     //          result := 0.0 //alpha2
     //          else if phase_num=3 then
     //               result := 0.0; //alpha3
     //update nodes information before calculating alpha_i
     //update_all_nodes_info;

     // calclate alpha
	/*# with pNodeFMs^[NodeNum] do */
	{
		
		int stop = 0;
		for(stop = (pNodeFMs)[NodeNum].vl_ndphases_dg, phase_num = 1; phase_num <= stop; phase_num++)
		{
			switch(phase_num)
			{
				case 	1: //phase A
				{
					int stop1 = 0;
					TempAlpha = 0.0;//init as zero
					sum_Sij_j = 0.0;
					for(stop1 = Nodes, j = 0; j < stop1; j++)
					{
                 //for j := 1 to NodeNumofDG-1 do

                          //if pnodeFMs^[j].vl_phase_num=phase_num then
                          //begin
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNum * Nodes + j] * (pNodeFMs)[j].vl_Alpha1_dg;
						sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNum * Nodes + j];
                          //end;
					}
                 //for j := NodeNumofDG + 1 to Nodes do
                 //begin
                 //         Alpha1 := Alpha1 + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].Alpha1   ;
                 //         sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 //end;
					(pNodeFMs)[NodeNum].vl_Alpha1_dg = TempAlpha / sum_Sij_j;
					(pNodeFMs)[NodeNum].vl_Alpha1_dg = (pNodeFMs)[NodeNum].vl_Alpha1_dg - (pNodeFMs)[NodeNum].vl_kcq_dg * (pNodeFMs)[NodeNum].vl_Gradient1_dg;
                 //result := Alpha1;
				}
				break; //phase B
				case 	2:
				{
					int stop1 = 0;
					TempAlpha = 0.0;//init as zero
					sum_Sij_j = 0.0;
					for(stop1 = Nodes, j = 0; j < stop1; j++)
					{
                 //for j := 1 to NodeNumofDG-1 do
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNum * Nodes + j] * (pNodeFMs)[j].vl_Alpha2_dg;
						sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNum * Nodes + j];
					}
                 //for j := NodeNumofDG + 1 to Nodes do
                 //begin
                 //         Alpha2 := Alpha2 + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].Alpha2   ;
                 //         sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 //end;
					(pNodeFMs)[NodeNum].vl_Alpha2_dg = TempAlpha / sum_Sij_j;
					(pNodeFMs)[NodeNum].vl_Alpha2_dg = (pNodeFMs)[NodeNum].vl_Alpha2_dg - (pNodeFMs)[NodeNum].vl_kcq_dg * (pNodeFMs)[NodeNum].vl_Gradient2_dg;
                 //result := Alpha2;
				}
				break; //phase C
				case 	3:
				{
					int stop1 = 0;
					TempAlpha = 0.0;//init as zero
					sum_Sij_j = 0.0;
					for(stop1 = Nodes, j = 0; j < stop1; j++)
					{
                 //for j := 1 to NodeNumofDG-1 do
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNum * Nodes + j] * (pNodeFMs)[j].vl_Alpha3_dg;
						sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNum * Nodes + j];
					}
                 //for j := NodeNumofDG + 1 to Nodes do
                 //begin
                 //         Alpha3 := Alpha3 + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].Alpha3   ;
                 //         sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 //end;
					(pNodeFMs)[NodeNum].vl_Alpha3_dg = TempAlpha / sum_Sij_j;
					(pNodeFMs)[NodeNum].vl_Alpha3_dg = (pNodeFMs)[NodeNum].vl_Alpha3_dg - (pNodeFMs)[NodeNum].vl_kcq_dg * (pNodeFMs)[NodeNum].vl_Gradient3_dg;
                 //result := Alpha3;
				}
				break; //pos seq value
				case 	0:
				{
					int stop1 = 0;
					TempAlpha = 0.0;//init as zero
					sum_Sij_j = 0.0;
					for(stop1 = Nodes, j = 0; j < stop1; j++)
					{
                 //for j := 1 to NodeNumofDG-1 do
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNum * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dg;
						sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNum * Nodes + j];
					}
                 //for j := NodeNumofDG + 1 to Nodes do
                 //begin
                 //         Alpha := Alpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].Alpha   ;
                 //         sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 //end;
					(pNodeFMs)[NodeNum].vl_Alpha_dg = TempAlpha / sum_Sij_j;
					(pNodeFMs)[NodeNum].vl_Alpha_dg = (pNodeFMs)[NodeNum].vl_Alpha_dg - (pNodeFMs)[NodeNum].vl_kcq_dg * (pNodeFMs)[NodeNum].vl_Gradient_dg;
                 //result := Alpha;
				}
				break;
				default:
				  ;
				break;
			}
		}
	}
}
/*------------------------*/
//////////////////////////

int TFMonitorObj::Get_P_mode(int ActorID)
{
	int result = 0;
	result = p_mode;
	return result;
}
// AlphaP Gradient or Pref
  // NodeNuminClstr: node number in cluster

double TFMonitorObj::Get_power_trans(int ActorID)
{
	double result = 0.0;
	int i = 0;
	int j = 0;
	int k = 0;
	TPowerTerminal* pTerminal = nullptr;
	pComplexArray Curr = nullptr;
      //tempCplx := MeteredElement.power[MeteredTerminal];
	int stop = 0;
	((TPDElement*) MeteredElement)->GetCurrents(&(MeteredElement->Iterminal[0]), ActorID); //Curr
	pTerminal = &((MeteredElement->Terminals)[MeteredTerminal - 1]);
	tempCplx = CZero;
	k = (MeteredTerminal - 1) * MeteredElement->Get_NConds();
	for(stop = MeteredElement->Get_NConds(), j = 1; j <= stop; j++)
	{// how many conds of this element
		i = (pTerminal->TermNodeRef)[j - 1];  // global node number
		if(!ADiakoptics || (ActorID == 1))//power
			caccum(tempCplx, cmul(ActiveCircuit[ActorID]->Solution->NodeV[i], conjg((MeteredElement->Iterminal)[k + j - 1])));
		else
			caccum(tempCplx, cmul(ActiveCircuit[ActorID]->Solution->VoltInActor1(i), conjg((MeteredElement->Iterminal)[k + j - 1])));
	}
	result = tempCplx.re;
	return result;
}
//calculate the gradient for alpha i

double TFMonitorObj::Calc_Grdt_for_Alpha(int NodeNuminClstr, int phase_num, int ActorID)
{
	double result = 0.0;
	complex vtemp = {};
	complex ctmp = {};
	double Tmp = 0.0;
	double Gij = 0.0;
	double Bij = 0.0;
	double Gii = 0.0;
	double Bii = 0.0;
	int DevIndex = 0;
	int i = 0;
	int j = 0;
	int k = 0;
	int jTempTerminal = 0;
	TDSSCktElement* PElem = nullptr;
	int nodeRefi = 0;// ref number of this node
	int nodeRefj = 0;// ref number of the upper node
	double Den = 0.0;
      //pNodeFMs^[NodeNuminClstr].vl_strMeasuredName;//element followed by this bus
	int stop = 0;
	DevIndex = GetCktElementIndex((pNodeFMs)[NodeNuminClstr].vl_strMeasuredName);
	if(DevIndex > 0)                                       // Monitored element must already exist
	{
		PElem = ((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
	}
	if(PElem != nullptr) //want to get voltages from the other side of the device
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			int stop = 0;
			for(stop = PElem->Yorder, i = 1; i <= stop; i++)
			{
				if(!ADiakoptics || (ActorID == 1))
					(PElem->Vterminal)[i - 1] = with0->NodeV[(PElem->NodeRef)[i - 1]];
				else
					(PElem->Vterminal)[i - 1] = with0->VoltInActor1((PElem->NodeRef)[i - 1]);
			}
		}
	}
	else
	result = 0.0;
      //k is the terminal number of this end
	k = (pNodeFMs)[NodeNuminClstr].vl_terminalNum;
      //this is the other end jTempTerminal
	if(k == 1)
		jTempTerminal = 2;
	else
		jTempTerminal = 1;
      //k := (iTempTerminal -1)*MeteredElement.NConds;
      //
      //find the voltage of this phase on this terminal
	for(stop = PElem->Get_NPhases(), i = 1; i <= stop; i++)
	{// how many conds of this element
		j = ((PElem->Terminals)[jTempTerminal - 1].TermNodeRef)[i - 1];
		if((ActiveCircuit[ActorID]->MapNodeToBus)[j - 1].NodeNum == phase_num)
		{
			nodeRefj = j;                                   // node ref of the other end of this element and this phase
			if(!ADiakoptics || (ActorID == 1))
				vtemp = ActiveCircuit[ActorID]->Solution->NodeV[nodeRefj];
			else
				vtemp = ActiveCircuit[ActorID]->Solution->VoltInActor1(nodeRefj);
			nodeRefi = ((PElem->Terminals)[k - 1].TermNodeRef)[i - 1]; // node ref of this node
		}
	} //  cannot deal with pos seq
	if(phase_num == 0)
		;
	ctmp = ActiveCircuit[ActorID]->Solution->Get_Yij(nodeRefi, nodeRefj);
	Gij = ctmp.re;
	Bij = ctmp.im;
	ctmp = ActiveCircuit[ActorID]->Solution->Get_Yij(nodeRefi, nodeRefi);
            //ctmp := activecircuit.Solution.NodeYii^[nodeRefi];
	Gii = ctmp.re;
	Bii = ctmp.im;
	/*# with pNodeFMs^[NodeNuminClstr] do */
	{
		
		switch(phase_num)
		{
			case 	0: //pos seq
			{
				result = (pNodeFMs)[NodeNuminClstr].vl_Gradient_dg; //  can not deal with that
			}
			break;
              /*den := vl_Q_DG1 - vl_Q_Di1- vl_V1*vl_V1* Bii;   // pos ctrl: Bii use the first one
              tmp := vl_V1-cabs(vTemp);

              vl_gradient1_dg := vl_V1 * tmp;

              if abs(den)<epsilon then vl_gradient1_dg := 0.0
              else
                vl_gradient1_dg := vl_gradient1_dg /(den);
              */
			case 	1:
			{
				Den = (pNodeFMs)[NodeNuminClstr].vl_q_DG1 - (pNodeFMs)[NodeNuminClstr].vl_Q_Di1 - (pNodeFMs)[NodeNuminClstr].vl_V1 * (pNodeFMs)[NodeNuminClstr].vl_V1 * Bii;   // pos ctrl: Bii use the first one
				Tmp = ((pNodeFMs)[NodeNuminClstr].vl_V1 - cabs(vtemp) * cos(cang(vtemp) - cang((pNodeFMs)[NodeNuminClstr].vl_V_1c)));
				(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = (pNodeFMs)[NodeNuminClstr].vl_V1 * Tmp;
				if(Abs((int) Den) < EPSILON)
					(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = 0.0;
				else
					(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg / (Den);
				Den = (pNodeFMs)[NodeNuminClstr].vl_p_DG1 - (pNodeFMs)[NodeNuminClstr].vl_P_Di1 - (pNodeFMs)[NodeNuminClstr].vl_V1 * (pNodeFMs)[NodeNuminClstr].vl_V1 * Gii;
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_V1 * cabs(vtemp) * sin(cang(vtemp) - cang((pNodeFMs)[NodeNuminClstr].vl_V_1c));
				if(Abs((int) Den) < EPSILON)
					Tmp = 0;
				else
					Tmp = Tmp / Den;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg - Tmp;

              /*
              den := vl_V1*(Gij * sin(cang(vl_V_1c)-cang(vTemp)) - Bij * cos(cang(vl_V_1c)-cang(vTemp)));
              tmp := cabs(vTemp) - vl_V1*cos(cang(vTemp)-cang(vl_V_1c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient1_dg :=vl_gradient1_dg + tmp;

              den := Gij * cos(cang(vl_V_1c)-cang(vTemp)) + Bij * sin(cang(vl_V_1c)-cang(vTemp));
              tmp := sin(cang(vTemp)-cang(vl_V_1c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient1_dg :=vl_gradient1_dg - tmp;
              */
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_Qmax_phase_dg * Gij;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg * Tmp;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg / ((pNodeFMs)[NodeNuminClstr].vl_V_ref1_dg * (pNodeFMs)[NodeNuminClstr].vl_V_ref1_dg);//PU value??
             //////////////////////////////////////////////////////////
             //vl_gradient_dg := (vl_V_ref_dg-vl_v)*vl_V/(den)/(vl_V_ref_dg*vl_V_ref_dg);  //*vl_Qmax, 0311-by dahei
             //       vl_gradient_dg := (beta*vl_V_ref_dg*vl_V_ref_dg* abs(Bii)*100/j)*vl_gradient_dg;
             ////////////////////////////////////////////////////////////////////////////////////
				result = (pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg; //
			}
			break;
               /*den := vl_Q_DG2 - vl_Q_Di2- vl_V2*vl_V2* Bii;   // pos ctrl: Bii use the first one
              tmp := vl_V2-cabs(vTemp);

              vl_gradient2_dg := vl_V2 * tmp;

              if abs(den)<epsilon then vl_gradient2_dg := 0.0
              else
                vl_gradient2_dg := vl_gradient2_dg /(den);

              */
			case 	2:
			{
				Den = (pNodeFMs)[NodeNuminClstr].vl_q_DG2 - (pNodeFMs)[NodeNuminClstr].vl_Q_Di2 - (pNodeFMs)[NodeNuminClstr].vl_V2 * (pNodeFMs)[NodeNuminClstr].vl_V2 * Bii;   // pos ctrl: Bii use the first one
				Tmp = ((pNodeFMs)[NodeNuminClstr].vl_V2 - cabs(vtemp) * cos(cang(vtemp) - cang((pNodeFMs)[NodeNuminClstr].vl_V_2c)));
				(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = (pNodeFMs)[NodeNuminClstr].vl_V2 * Tmp;
				if(Abs((int) Den) < EPSILON)
					(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = 0.0;
				else
					(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg / (Den);
				Den = (pNodeFMs)[NodeNuminClstr].vl_p_DG2 - (pNodeFMs)[NodeNuminClstr].vl_P_Di2 - (pNodeFMs)[NodeNuminClstr].vl_V2 * (pNodeFMs)[NodeNuminClstr].vl_V2 * Gii;
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_V2 * cabs(vtemp) * sin(cang(vtemp) - cang((pNodeFMs)[NodeNuminClstr].vl_V_2c));
				if(Abs((int) Den) < EPSILON)
					Tmp = 0;
				else
					Tmp = Tmp / Den;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg - Tmp;

              /*
              den := vl_V2*(Gij * sin(cang(vl_V_2c)-cang(vTemp)) - Bij * cos(cang(vl_V_2c)-cang(vTemp)));
              tmp := cabs(vTemp) - vl_V2*cos(cang(vTemp)-cang(vl_V_2c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient2_dg :=vl_gradient2_dg + tmp;

              den := Gij * cos(cang(vl_V_2c)-cang(vTemp)) + Bij * sin(cang(vl_V_2c)-cang(vTemp));
              tmp := sin(cang(vTemp)-cang(vl_V_2c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient2_dg :=vl_gradient2_dg - tmp;
               */
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_Qmax_phase_dg * Gij;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg * Tmp;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg / ((pNodeFMs)[NodeNuminClstr].vl_V_ref2_dg * (pNodeFMs)[NodeNuminClstr].vl_V_ref2_dg);//PU value??
			}
			break;
              /*den := vl_Q_DG3 - vl_Q_Di3- vl_V3*vl_V3* Bii;   // pos ctrl: Bii use the first one
              tmp := vl_V3-cabs(vTemp) ;

              vl_gradient3_dg := vl_V3 * tmp;

              if abs(den)<epsilon then vl_gradient3_dg := 0.0
              else
                vl_gradient3_dg := vl_gradient3_dg /(den);

               */
			case 	3:
			{
				Den = (pNodeFMs)[NodeNuminClstr].vl_q_DG3 - (pNodeFMs)[NodeNuminClstr].vl_Q_Di3 - (pNodeFMs)[NodeNuminClstr].vl_V3 * (pNodeFMs)[NodeNuminClstr].vl_V3 * Bii;   // pos ctrl: Bii use the first one
				Tmp = ((pNodeFMs)[NodeNuminClstr].vl_V3 - cabs(vtemp) * cos(cang(vtemp) - cang((pNodeFMs)[NodeNuminClstr].vl_V_3c)));
				(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = (pNodeFMs)[NodeNuminClstr].vl_V3 * Tmp;
				if(Abs((int) Den) < EPSILON)
					(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = 0.0;
				else
					(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg / (Den);
				Den = (pNodeFMs)[NodeNuminClstr].vl_p_DG3 - (pNodeFMs)[NodeNuminClstr].vl_P_Di3 - (pNodeFMs)[NodeNuminClstr].vl_V3 * (pNodeFMs)[NodeNuminClstr].vl_V3 * Gii;
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_V3 * cabs(vtemp) * sin(cang(vtemp) - cang((pNodeFMs)[NodeNuminClstr].vl_V_3c));
				if(Abs((int) Den) < EPSILON)
					Tmp = 0;
				else
					Tmp = Tmp / Den;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg - Tmp;

              /*
              den := vl_V3*(Gij * sin(cang(vl_V_3c)-cang(vTemp)) - Bij * cos(cang(vl_V_3c)-cang(vTemp)));
              tmp := cabs(vTemp) - vl_V3*cos(cang(vTemp)-cang(vl_V_3c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient3_dg :=vl_gradient3_dg + tmp;

              den := Gij * cos(cang(vl_V_3c)-cang(vTemp)) + Bij * sin(cang(vl_V_3c)-cang(vTemp));
              tmp := sin(cang(vTemp)-cang(vl_V_3c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient3_dg :=vl_gradient3_dg - tmp;
              */
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_Qmax_phase_dg * Gij;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg * Tmp;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg / ((pNodeFMs)[NodeNuminClstr].vl_V_ref3_dg * (pNodeFMs)[NodeNuminClstr].vl_V_ref3_dg);//PU value??
			}
			break;
			default:
			  ;
			break;
		}
	}
      //result :=  tempCplx.re ;
	return result;
}

//calculate the gradient for alpha i

double TFMonitorObj::Calc_Grdt_for_Alpha_vivj(int NodeNuminClstr, int phase_num, int ActorID)
{
	double result = 0.0;
	complex vtemp = {};
	complex ctmp = {};
	double Tmp = 0.0;
	double Gij = 0.0;
	double Bij = 0.0;
	double Gii = 0.0;
	double Bii = 0.0;
	int DevIndex = 0;
	int i = 0;
	int j = 0;
	int k = 0;
	int jTempTerminal = 0;
	TDSSCktElement* PElem = nullptr;
	int nodeRefi = 0;// ref number of this node
	int nodeRefj = 0;// ref number of the upper node
	double Den = 0.0;
      //pNodeFMs^[NodeNuminClstr].vl_strMeasuredName;//element followed by this bus
	int stop = 0;
	DevIndex = GetCktElementIndex((pNodeFMs)[NodeNuminClstr].vl_strMeasuredName);
	if(DevIndex > 0)                                       // Monitored element must already exist
	{
		PElem = ((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
	}
	if(PElem != nullptr) //want to get voltages from the other side of the device
	{
		/*# with ActiveCircuit[ActorID].Solution do */
		{
			auto with0 = ActiveCircuit[ActorID]->Solution;
			int stop = 0;
			for(stop = PElem->Yorder, i = 1; i <= stop; i++)
			{
				if(!ADiakoptics || (ActorID == 1))
					(PElem->Vterminal)[i - 1] = with0->NodeV[(PElem->NodeRef)[i - 1]];
				else
					(PElem->Vterminal)[i - 1] = with0->VoltInActor1((PElem->NodeRef)[i - 1]);
			}
		}
	}
	else
	result = 0.0;
      //k is the terminal number of this end
	k = (pNodeFMs)[NodeNuminClstr].vl_terminalNum;
      //this is the other end jTempTerminal
	if(k == 1)
		jTempTerminal = 2;
	else
		jTempTerminal = 1;
      //k := (iTempTerminal -1)*MeteredElement.NConds;
      //
      //find the voltage of this phase on this terminal
	for(stop = PElem->Get_NPhases(), i = 1; i <= stop; i++)
	{// how many conds of this element
		j = ((PElem->Terminals)[jTempTerminal - 1].TermNodeRef)[i - 1];
		if((ActiveCircuit[ActorID]->MapNodeToBus)[j - 1].NodeNum == phase_num)
		{
			nodeRefj = j;                                   // node ref of the other end of this element and this phase
			if(!ADiakoptics || (ActorID == 1))
				vtemp = ActiveCircuit[ActorID]->Solution->NodeV[nodeRefj];
			else
				vtemp = ActiveCircuit[ActorID]->Solution->VoltInActor1(nodeRefj);
			nodeRefi = ((PElem->Terminals)[k - 1].TermNodeRef)[i - 1]; // node ref of this node
		}
	} //  cannot deal with pos seq
	if(phase_num == 0)
		;
	ctmp = ActiveCircuit[ActorID]->Solution->Get_Yij(nodeRefi, nodeRefj);
	Gij = ctmp.re;
	Bij = ctmp.im;
	ctmp = ActiveCircuit[ActorID]->Solution->Get_Yij(nodeRefi, nodeRefi);
            //ctmp := activecircuit.Solution.NodeYii^[nodeRefi];
	Gii = ctmp.re;
	Bii = ctmp.im;
	/*# with pNodeFMs^[NodeNuminClstr] do */
	{
		
		switch(phase_num)
		{
			case 	0: //pos seq
			{
				result = (pNodeFMs)[NodeNuminClstr].vl_Gradient_dg; //  can not deal with that
			}
			break;
			case 	1:
			{
				Den = (pNodeFMs)[NodeNuminClstr].vl_q_DG1 - (pNodeFMs)[NodeNuminClstr].vl_Q_Di1 - (pNodeFMs)[NodeNuminClstr].vl_V1 * (pNodeFMs)[NodeNuminClstr].vl_V1 * Bii;   // pos ctrl: Bii use the first one
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_V1 - cabs(vtemp);
				(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = (pNodeFMs)[NodeNuminClstr].vl_V1 * Tmp;
				if(Abs((int) Den) < EPSILON)
					(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = 0.0;
				else
					(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg / (Den);
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_Qmax_phase_dg * Gij;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg * Tmp;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg / ((pNodeFMs)[NodeNuminClstr].vl_V_ref1_dg * (pNodeFMs)[NodeNuminClstr].vl_V_ref1_dg);//PU value??
             //////////////////////////////////////////////////////////
             //vl_gradient_dg := (vl_V_ref_dg-vl_v)*vl_V/(den)/(vl_V_ref_dg*vl_V_ref_dg);  //*vl_Qmax, 0311-by dahei
             //       vl_gradient_dg := (beta*vl_V_ref_dg*vl_V_ref_dg* abs(Bii)*100/j)*vl_gradient_dg;
             ////////////////////////////////////////////////////////////////////////////////////
				result = (pNodeFMs)[NodeNuminClstr].vl_Gradient1_dg; //
			}
			break;
			case 	2:
			{
				Den = (pNodeFMs)[NodeNuminClstr].vl_q_DG2 - (pNodeFMs)[NodeNuminClstr].vl_Q_Di2 - (pNodeFMs)[NodeNuminClstr].vl_V2 * (pNodeFMs)[NodeNuminClstr].vl_V2 * Bii;   // pos ctrl: Bii use the first one
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_V2 - cabs(vtemp);
				(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = (pNodeFMs)[NodeNuminClstr].vl_V2 * Tmp;
				if(Abs((int) Den) < EPSILON)
					(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = 0.0;
				else
					(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg / (Den);
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_Qmax_phase_dg * Gij;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg * Tmp;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient2_dg / ((pNodeFMs)[NodeNuminClstr].vl_V_ref2_dg * (pNodeFMs)[NodeNuminClstr].vl_V_ref2_dg);//PU value??
			}
			break;
			case 	3:
			{
				Den = (pNodeFMs)[NodeNuminClstr].vl_q_DG3 - (pNodeFMs)[NodeNuminClstr].vl_Q_Di3 - (pNodeFMs)[NodeNuminClstr].vl_V3 * (pNodeFMs)[NodeNuminClstr].vl_V3 * Bii;   // pos ctrl: Bii use the first one
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_V3 - cabs(vtemp);
				(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = (pNodeFMs)[NodeNuminClstr].vl_V3 * Tmp;
				if(Abs((int) Den) < EPSILON)
					(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = 0.0;
				else
					(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg / (Den);
				Tmp = (pNodeFMs)[NodeNuminClstr].vl_Qmax_phase_dg * Gij;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg * Tmp;
				(pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg = (pNodeFMs)[NodeNuminClstr].vl_Gradient3_dg / ((pNodeFMs)[NodeNuminClstr].vl_V_ref3_dg * (pNodeFMs)[NodeNuminClstr].vl_V_ref3_dg);//PU value??
			}
			break;
			default:
			  ;
			break;
		}
	}
      //result :=  tempCplx.re ;
	return result;
}  // NodeNuminClstr: node number in cluster

double TFMonitorObj::Calc_GP_AlphaP(int phase_num, int ActorID)
{
	double result = 0.0;
	double PGtemp = 0.0;
	double PTemp = 0.0;
       //tempCplx
	PTemp = Get_power_trans(ActorID);  // get power on trans point
      //tempCplx := cnegate(tempCplx);//
      //if pNodeFMs^[NodeNuminClstr].vl_V_ref_dg<>0 then
      //  PGtemp  := -(p_trans_ref - ptemp )/Activecircuit.solution.t;//pNodeFMs^[NodeNuminClstr].vl_V_ref_dg // pu value?
      //else
      //if pNodeFMs^[NodeNuminClstr].vl_Pmax_dg<>0.0 then
        //PGtemp  := -(p_trans_ref - ptemp )//-(p_trans_ref - ptemp )/pNodeFMs^[NodeNuminClstr].vl_Pmax_dg  // should use the total load
        //else  PGtemp  := 0.0 ;
	if(eg_defed == true) // D_fm is damping plus droop
		PGtemp = -(P_trans_ref - PTemp) / (kVA_fm * 1000) * k_dltP;
	else
		PGtemp = -(P_trans_ref - PTemp) / 1000 * k_dltP; // kVA_fm = 1 kVA
	switch(phase_num)
	{
		case 	0: //pos seq
		{
			result = PGtemp; //
		}
		break;
		case 	1:
		{
			result = PGtemp; //
		}
		break;
		case 	2:
		{
			result = PGtemp; //
		}
		break;
		case 	3:
		{
			result = PGtemp; //
		}
		break;
		default:
		  ;
		break;
	}
	return result;
}
//////////////////////////
/*-------------------------*/  // NodeNuminClstr: node number in cluster

double TFMonitorObj::Calc_AlphaP(int NodeNuminClstr, int phase_num, int ActorID)
{
	double result = 0.0;
	int nn = 0;
	int j = 0;
	double den_dij = 0.0;
	double TempAlpha = 0.0;
     //alphaP = avg (alphaP) + Beta * Gp
	nn = NodeNuminClstr;
	switch(phase_num)
	{
		case 	0: //pos seq

              //1.calculate d_ij*alpha_j summation
		{
			int stop = 0;
			den_dij = 0;
			TempAlpha = 0;
			for(stop = Nodes, j = 0; j < stop; j++)
			{
				if((pNodeFMs)[j].vl_ndphases_dg == 3)   //only 3 phase nodes
				{
					den_dij = den_dij + (pCommMatrix)[nn * Nodes + j];
					TempAlpha = TempAlpha + (pCommMatrix)[nn * Nodes + j] * (pNodeFMs)[j].vl_AlphaP_dg;
				}
			}
			if(den_dij == 0)
				TempAlpha = 0.0;
			else
			{
				TempAlpha = TempAlpha / den_dij;
			}
			(pNodeFMs)[nn].vl_Gradient_dg = this->Calc_GP_AlphaP(phase_num, ActorID);
			(pNodeFMs)[nn].vl_AlphaP_dg = TempAlpha + (pNodeFMs)[nn].vl_kcd_dg * (pNodeFMs)[nn].vl_Gradient_dg / ActiveCircuit[ActorID]->Solution->Iteration;

                //disturbance
			(pNodeFMs)[nn].vl_AlphaP_dg = (pNodeFMs)[nn].vl_AlphaP_dg;
			if((pNodeFMs)[nn].vl_AlphaP_dg > 1)
				(pNodeFMs)[nn].vl_AlphaP_dg = 1;
			if((pNodeFMs)[nn].vl_AlphaP_dg < 0)
				(pNodeFMs)[nn].vl_AlphaP_dg = 0;
			result = (pNodeFMs)[NodeNuminClstr].vl_AlphaP_dg;
		}
		break;
		default:
		  ;
		break;
	}
	/*# with pNodeFMs^[nn] do */
	{
		
		switch(phase_num)
		{
			case 	1: //pos seq

              //1.calculate d_ij*alpha_j summation
			{
				int stop = 0;
				den_dij = 0;
				TempAlpha = 0;
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_phase_num_dg == phase_num))     //only count dgs with 3 phases or 1 phase that is same number

                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
					{
						den_dij = den_dij + (pCommMatrix)[NodeNuminClstr * Nodes + j];
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNuminClstr * Nodes + j] * (pNodeFMs)[j].vl_AlphaP1_dg;
                              //end;
					}
				}
				if(den_dij == 0)
					TempAlpha = 0.0;
				else
				{
					TempAlpha = TempAlpha / den_dij;
				}
				(pNodeFMs)[nn].vl_GradientP1_dg = Calc_GP_AlphaP(phase_num, ActorID);
				(pNodeFMs)[nn].vl_AlphaP1_dg = TempAlpha + (pNodeFMs)[nn].vl_kcd_dg * (pNodeFMs)[nn].vl_GradientP1_dg / ActiveCircuit[ActorID]->Solution->Iteration;
				if((pNodeFMs)[nn].vl_AlphaP1_dg > 1)
					(pNodeFMs)[nn].vl_AlphaP1_dg = 1;
				if((pNodeFMs)[nn].vl_AlphaP1_dg <  - 1)
					(pNodeFMs)[nn].vl_AlphaP1_dg = (double) -1;
				result = (pNodeFMs)[nn].vl_AlphaP1_dg;
			}
			break;
                 //1.calculate d_ij*alpha_j summation
			case 	2:
			{
				int stop = 0;
				den_dij = 0;
				TempAlpha = 0;
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_phase_num_dg == phase_num))     //only count dgs with 3 phases or 1 phase that is same number

                                //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                //begin
					{
						den_dij = den_dij + (pCommMatrix)[NodeNuminClstr * Nodes + j];
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNuminClstr * Nodes + j] * (pNodeFMs)[j].vl_AlphaP2_dg;
                                //end;
					}
				}
				if(den_dij == 0)
					TempAlpha = 0.0;
				else
				{
					TempAlpha = TempAlpha / den_dij;
				}
				(pNodeFMs)[nn].vl_GradientP2_dg = Calc_GP_AlphaP(phase_num, ActorID);
				(pNodeFMs)[nn].vl_AlphaP2_dg = TempAlpha + (pNodeFMs)[nn].vl_kcd_dg * (pNodeFMs)[nn].vl_GradientP2_dg / ActiveCircuit[ActorID]->Solution->Iteration;
				if((pNodeFMs)[nn].vl_AlphaP2_dg > 1)
					(pNodeFMs)[nn].vl_AlphaP2_dg = 1;
				if((pNodeFMs)[nn].vl_AlphaP2_dg <  - 1)
					(pNodeFMs)[nn].vl_AlphaP2_dg = (double) -1;
				result = (pNodeFMs)[nn].vl_AlphaP2_dg;
			}
			break;
                  //1.calculate d_ij*alpha_j summation
			case 	3:
			{
				int stop = 0;
				den_dij = 0;
				TempAlpha = 0;
                  //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_phase_num_dg == phase_num))     //only count dgs with 3 phases or 1 phase that is same number

                                //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                //begin
					{
						den_dij = den_dij + (pCommMatrix)[NodeNuminClstr * Nodes + j];
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNuminClstr * Nodes + j] * (pNodeFMs)[j].vl_AlphaP3_dg;
                                //end;
					}
				}
				if(den_dij == 0)
					TempAlpha = 0.0;
				else
				{
					TempAlpha = TempAlpha / den_dij;
				}
				(pNodeFMs)[nn].vl_GradientP3_dg = Calc_GP_AlphaP(phase_num, ActorID);
				(pNodeFMs)[nn].vl_AlphaP3_dg = TempAlpha + (pNodeFMs)[nn].vl_kcd_dg * (pNodeFMs)[nn].vl_GradientP3_dg / ActiveCircuit[ActorID]->Solution->Iteration;
				if((pNodeFMs)[nn].vl_AlphaP3_dg > 1)
					(pNodeFMs)[nn].vl_AlphaP3_dg = 1;
				if((pNodeFMs)[nn].vl_AlphaP3_dg <  - 1)
					(pNodeFMs)[nn].vl_AlphaP3_dg = (double) -1;
				result = (pNodeFMs)[nn].vl_AlphaP3_dg;
			}
			break;
			default:
			  ;
			break;
		}
	}
	return result;
}
/*----------------------------------------------------------------------------------*/
//only work for Generic5 nodefm

//NodeNumofDG = NodeNuminClstr

double TFMonitorObj::Calc_Alpha_M2(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID)
{
	double result = 0.0;
	int i = 0;
	int j = 0;
	double sum_Sij_j = 0.0;
	double Den = 0.0;
	double Alpha = 0.0;
	double den_dij = 0.0;
	double dii = 0.0;
	double TempAlpha = 0.0;
	double tmp1 = 0.0;
	double tmp2 = 0.0;
	double tmp3 = 0.0;
	double tmp4 = 0.0;
	double tmp5 = 0.0;
	double tmp6 = 0.0;
	double tmp7 = 0.0;
	update_all_nodes_info(ActorID);     // update voltages on all buses
	/*# with pNodeFMs^[NodeNumofDG] do */
	{
		
		switch(phase_num)
		{
			case 	0: //pos seq

              //1.calculate d_ij*alpha_j summation
			{
				int stop = 0;
				den_dij = 0;
				TempAlpha = 0;
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					if((pNodeFMs)[j].vl_ndphases_dg == 3)   //only 3 phase nodes

                                    //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                    //begin
					{
						den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dg;
                                    //end;
					}
				}
				if(den_dij == 0)
					TempAlpha = 0.0;
				else
				{
					TempAlpha = TempAlpha / den_dij;

                    //2.calculate gradient----------------
                    //Bii := ActiveCircuit.Solution.NodeYii^[dbNodeRef].im;
                    //Load.ActiveLoadObj.kvarBase;
					Den = (pNodeFMs)[NodeNumofDG].vl_q_DG - (pNodeFMs)[NodeNumofDG].vl_Q_Di - (pNodeFMs)[NodeNumofDG].vl_V * (pNodeFMs)[NodeNumofDG].vl_V * Bii;   // pos ctrl: Bii use the first one
					if(Abs((int) Den) < EPSILON)
						Den = EPSILON;
					(pNodeFMs)[NodeNumofDG].vl_Gradient_dg = ((pNodeFMs)[NodeNumofDG].vl_V_ref_dg - (pNodeFMs)[NodeNumofDG].vl_V) * (pNodeFMs)[NodeNumofDG].vl_V / (Den) / ((pNodeFMs)[NodeNumofDG].vl_V_ref_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref_dg);  //*vl_Qmax, 0311-by dahei
					j = ActiveCircuit[ActorID]->Solution->Iteration;
					(pNodeFMs)[NodeNumofDG].vl_Gradient_dg = (beta * (pNodeFMs)[NodeNumofDG].vl_V_ref_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref_dg * Abs((int) Bii) * 100 / j) * (pNodeFMs)[NodeNumofDG].vl_Gradient_dg;
					if(Abs((int) ((pNodeFMs)[NodeNumofDG].vl_V_ref_dg - (pNodeFMs)[NodeNumofDG].vl_V)) <= Volt_Trhd * (pNodeFMs)[NodeNumofDG].vl_V_ref_dg)
						(pNodeFMs)[NodeNumofDG].vl_Gradient_dg = 0.0;

                    //calculate final alpha----------------
				}
				Alpha = (pNodeFMs)[NodeNumofDG].vl_kc_ul_dg * TempAlpha + (pNodeFMs)[NodeNumofDG].vl_Gradient_dg;
				if(Alpha > 1)
					Alpha = 1;
				if(Alpha <  - 1)
					Alpha = (double) -1;
				(pNodeFMs)[NodeNumofDG].vl_Alpha_dg = Alpha;
				result = Alpha;
			}
			break;
              //1.calculate d_ij*alpha_j summation
			case 	1:
			{
				int stop = 0;
				den_dij = 0;
				TempAlpha = 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_nodeType_phase[1] == 1))     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
 //this phase has DG

                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
					{
						den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha1_dg;
                              //end;
					}
				}
				if(den_dij == 0)
					TempAlpha = 0.0;
				else
				{
					TempAlpha = TempAlpha / den_dij;
                    //2.calculate gradient----------------
                    //Bii := ActiveCircuit.Solution.NodeYii^[dbNodeRef].im;
					Den = (pNodeFMs)[NodeNumofDG].vl_q_DG1 - (pNodeFMs)[NodeNumofDG].vl_Q_Di1 - (pNodeFMs)[NodeNumofDG].vl_V1 * (pNodeFMs)[NodeNumofDG].vl_V1 * Bii;   // pos ctrl: Bii use the first one
					if(Abs((int) Den) < EPSILON)
						Den = EPSILON;
					tmp1 = (pNodeFMs)[NodeNumofDG].vl_q_DG1;
					tmp2 = (pNodeFMs)[NodeNumofDG].vl_Q_Di1;
					tmp3 = (pNodeFMs)[NodeNumofDG].vl_V1;
					tmp4 = (pNodeFMs)[NodeNumofDG].vl_Qmax_dg;
					tmp5 = (pNodeFMs)[NodeNumofDG].vl_V_ref1_dg;
					j = ActiveCircuit[ActorID]->Solution->Iteration;
					(pNodeFMs)[NodeNumofDG].vl_Gradient1_dg = ((pNodeFMs)[NodeNumofDG].vl_V_ref1_dg - (pNodeFMs)[NodeNumofDG].vl_V1) * (pNodeFMs)[NodeNumofDG].vl_V1 / (Den) / ((pNodeFMs)[NodeNumofDG].vl_V_ref1_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
					(pNodeFMs)[NodeNumofDG].vl_Gradient1_dg = (beta * (pNodeFMs)[NodeNumofDG].vl_V_ref1_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref1_dg * Abs((int) Bii) * 100 / j) * (pNodeFMs)[NodeNumofDG].vl_Gradient1_dg;
                    //vl_gradient1 := (beta)*vl_gradient1;
					if(Abs((int) ((pNodeFMs)[NodeNumofDG].vl_V_ref1_dg - (pNodeFMs)[NodeNumofDG].vl_V1)) <= Volt_Trhd * (pNodeFMs)[NodeNumofDG].vl_V_ref1_dg)
						(pNodeFMs)[NodeNumofDG].vl_Gradient1_dg = 0.0;
                    //calculate final alpha----------------
				}
				(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = TempAlpha + (pNodeFMs)[NodeNumofDG].vl_Gradient1_dg;
				tmp6 = (pNodeFMs)[NodeNumofDG].vl_Gradient1_dg;
				tmp7 = (pNodeFMs)[NodeNumofDG].vl_Alpha1_dg;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha1_dg > 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = 1;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha1_dg <  - 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = (double) -1;
				result = (pNodeFMs)[NodeNumofDG].vl_Alpha1_dg;
			}
			break;
                 //1.calculate d_ij*alpha_j summation
			case 	2:
			{
				int stop = 0;
				den_dij = 0;
				TempAlpha = 0;
                  //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_nodeType_phase[2] == 1))     //only count dgs with 3 phases or 1 phase that is same number
                              //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
 //this phase has DG

                                //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                //begin
					{
						den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha2_dg;
                                //end;
					}
				}
				if(den_dij == 0)
					TempAlpha = 0.0;
				else
				{
					TempAlpha = TempAlpha / den_dij;

                      //2.calculate gradient----------------
                      //Bii := ActiveCircuit.Solution.NodeYii^[dbNodeRef].im;
					Den = (pNodeFMs)[NodeNumofDG].vl_q_DG2 - (pNodeFMs)[NodeNumofDG].vl_Q_Di2 - (pNodeFMs)[NodeNumofDG].vl_V2 * (pNodeFMs)[NodeNumofDG].vl_V2 * Bii;   // pos ctrl: Bii use the first one
					if(Abs((int) Den) < EPSILON)
						Den = EPSILON;
					j = ActiveCircuit[ActorID]->Solution->Iteration;
					(pNodeFMs)[NodeNumofDG].vl_Gradient2_dg = ((pNodeFMs)[NodeNumofDG].vl_V_ref2_dg - (pNodeFMs)[NodeNumofDG].vl_V2) * (pNodeFMs)[NodeNumofDG].vl_V2 / (Den) / ((pNodeFMs)[NodeNumofDG].vl_V_ref2_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref2_dg);        //*vl_Qmax
					(pNodeFMs)[NodeNumofDG].vl_Gradient2_dg = (beta * (pNodeFMs)[NodeNumofDG].vl_V_ref2_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref2_dg * Abs((int) Bii) * 100 / j) * (pNodeFMs)[NodeNumofDG].vl_Gradient2_dg;
                      //vl_gradient2 := (beta)*vl_gradient2;
					if(Abs((int) ((pNodeFMs)[NodeNumofDG].vl_V_ref2_dg - (pNodeFMs)[NodeNumofDG].vl_V2)) <= Volt_Trhd * (pNodeFMs)[NodeNumofDG].vl_V_ref2_dg)
						(pNodeFMs)[NodeNumofDG].vl_Gradient2_dg = 0.0;
                      //calculate final alpha----------------
				}
				(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = TempAlpha + (pNodeFMs)[NodeNumofDG].vl_Gradient2_dg;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha2_dg > 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = 1;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha2_dg <  - 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = (double) -1;
				result = (pNodeFMs)[NodeNumofDG].vl_Alpha2_dg;
			}
			break;
                  //1.calculate d_ij*alpha_j summation
			case 	3:
			{
				int stop = 0;
				den_dij = 0;
				TempAlpha = 0;
                  //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_nodeType_phase[2] == 1))     //only count dgs with 3 phases or 1 phase that is same number
                              //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
 //this phase has DG

                                //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                //begin
					{
						den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
						TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha3_dg;
                                //end;
					}
				}
				if(den_dij == 0)
					TempAlpha = 0.0;
				else
				{
					TempAlpha = TempAlpha / den_dij;

                      //2.calculate gradient----------------
                      //Bii := ActiveCircuit.Solution.NodeYii^[dbNodeRef].im;
					Den = (pNodeFMs)[NodeNumofDG].vl_q_DG3 - (pNodeFMs)[NodeNumofDG].vl_Q_Di3 - (pNodeFMs)[NodeNumofDG].vl_V3 * (pNodeFMs)[NodeNumofDG].vl_V3 * Bii;   // pos ctrl: Bii use the first one
					if(Abs((int) Den) < EPSILON)
						Den = EPSILON;
					j = ActiveCircuit[ActorID]->Solution->Iteration;
					(pNodeFMs)[NodeNumofDG].vl_Gradient3_dg = ((pNodeFMs)[NodeNumofDG].vl_V_ref3_dg - (pNodeFMs)[NodeNumofDG].vl_V3) * (pNodeFMs)[NodeNumofDG].vl_V3 / (Den) / ((pNodeFMs)[NodeNumofDG].vl_V_ref3_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref3_dg);        //*vl_Qmax
					(pNodeFMs)[NodeNumofDG].vl_Gradient3_dg = (beta * (pNodeFMs)[NodeNumofDG].vl_V_ref3_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref3_dg * Abs((int) Bii) * 100 / j) * (pNodeFMs)[NodeNumofDG].vl_Gradient3_dg;
                      //vl_gradient3 := (beta)*vl_gradient3;
					if(Abs((int) ((pNodeFMs)[NodeNumofDG].vl_V_ref3_dg - (pNodeFMs)[NodeNumofDG].vl_V3)) <= Volt_Trhd * (pNodeFMs)[NodeNumofDG].vl_V_ref3_dg)
						(pNodeFMs)[NodeNumofDG].vl_Gradient3_dg = 0.0;
                      //calculate final alpha----------------
				}
				(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = TempAlpha + (pNodeFMs)[NodeNumofDG].vl_Gradient3_dg;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha3_dg > 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = 1;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha3_dg <  - 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = (double) -1;
				result = (pNodeFMs)[NodeNumofDG].vl_Alpha3_dg;
			}
			break;
			default:
			  ;
			break;
		}
	}
     //result := 0;
	return result;
}
//will be call in Generic5
//calculate subgradient for DG 'NodeNumofDG' phase 'phase_num'

double TFMonitorObj::Calc_Alpha_LnM2(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID)
{
	double result = 0.0;
	double Lambda0 = 0.0;
	double Lambda = 0.0;
	double Tmp = 0.0;
	double tmp1 = 0.0;
	Lambda0 = 0.1;
	Lambda = 1.0;
	Tmp = 0.0;
     //tmp := Calc_Alpha_L(NodeNumofDG, phase_num, dbNodeRef, Bii,beta,Volt_Trhd) ;
	tmp1 = Calc_Alpha_M2(NodeNumofDG, phase_num, dbNodeRef, Bii, beta, Volt_Trhd, ActorID);
	result = (1 - Lambda) * Tmp + Lambda * tmp1;
     //result := Calc_Alpha_L_vivj(NodeNumofDG, phase_num, dbNodeRef, Bii,beta,Volt_Trhd) ;
	return result;
}

double TFMonitorObj::Calc_Alpha_L_vivj(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID)
{
	double result = 0.0;
	int i = 0;
	int j = 0;
	double sum_Sij_j = 0.0;
	double Den = 0.0;
	double Alpha = 0.0;
	double den_dij = 0.0;
	double dii = 0.0;
	double TempAlpha = 0.0;
	double Tmp = 0.0;
	double dynBeta = 0.0;
  //tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: double;
	update_all_nodes_info(ActorID);     // update voltages on all buses
	Get_PQ_DI(NodeNumofDG, ActorID); // load measure
      // calclate alpha
     //with pnodeFMs^[NodeNumofDG] do
	switch(phase_num)
	{
		case 	1: //phase A
              //1.calculate d_ij*alpha_j summation
		{
			int stop = 0;
			den_dij = 0;
			TempAlpha = 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
			for(stop = Nodes, j = 0; j < stop; j++)
			{
				if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_nodeType_phase[1] == 1))     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
 //this phase has DG

                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
				{
					den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
					TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha1_dg;
                              //end;
				}
			}
			if(den_dij == 0)
				TempAlpha = 0.0;
			else
			{
				TempAlpha = TempAlpha / den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
			}
			/*# with pNodeFMs^[NodeNumofDG] do */
			{
				
				Tmp = Calc_Grdt_for_Alpha_vivj(NodeNumofDG, phase_num, ActorID);//  vl_gradient1_dg updated inside
				if((pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg != 0)
					dynBeta = (beta * Abs((int) Bii) * 100 / j) * (pNodeFMs)[NodeNumofDG].vl_V_ref1_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref1_dg / (pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg;
				(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = TempAlpha + dynBeta * (pNodeFMs)[NodeNumofDG].vl_Gradient1_dg;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha1_dg > 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = 1;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha1_dg <  - 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = (double) -1;
			}
			result = (pNodeFMs)[NodeNumofDG].vl_Alpha1_dg;
		}
		break; //phase B
              //1.calculate d_ij*alpha_j summation
		case 	2:
		{
			int stop = 0;
			den_dij = 0;
			TempAlpha = 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
			for(stop = Nodes, j = 0; j < stop; j++)
			{
				if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_nodeType_phase[2] == 1))     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
 //this phase has DG

                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
				{
					den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
					TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha2_dg;
                              //end;
				}
			}
			if(den_dij == 0)
				TempAlpha = 0.0;
			else
			{
				TempAlpha = TempAlpha / den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
			}
			/*# with pNodeFMs^[NodeNumofDG] do */
			{
				
				Tmp = Calc_Grdt_for_Alpha_vivj(NodeNumofDG, phase_num, ActorID);
				if((pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg != 0)
					dynBeta = (beta * Abs((int) Bii) * 100 / j) / (pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref2_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref2_dg; //
				(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = TempAlpha + dynBeta * (pNodeFMs)[NodeNumofDG].vl_Gradient2_dg;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha2_dg > 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = 1;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha2_dg <  - 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = (double) -1;
			}
			result = (pNodeFMs)[NodeNumofDG].vl_Alpha2_dg;
		}
		break; //phase C
                 //1.calculate d_ij*alpha_j summation
		case 	3:
		{
			int stop = 0;
			den_dij = 0;
			TempAlpha = 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
			for(stop = Nodes, j = 0; j < stop; j++)
			{
				if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_nodeType_phase[2] == 1))     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
 //this phase has DG

                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
				{
					den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
					TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha3_dg;
                              //end;
				}
			}
			if(den_dij == 0)
				TempAlpha = 0.0;
			else
			{
				TempAlpha = TempAlpha / den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
			}
			/*# with pNodeFMs^[NodeNumofDG] do */
			{
				
				Tmp = Calc_Grdt_for_Alpha_vivj(NodeNumofDG, phase_num, ActorID);
				if((pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg != 0)
					dynBeta = (beta * Abs((int) Bii) * 100 / j) / (pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref3_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref3_dg; //
				(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = TempAlpha + dynBeta * (pNodeFMs)[NodeNumofDG].vl_Gradient3_dg;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha3_dg > 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = 1;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha3_dg <  - 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = (double) -1;
			}
			result = (pNodeFMs)[NodeNumofDG].vl_Alpha3_dg;
		}
		break; //pos seq value
		case 	0:
		break;
		default:
		  ;
		break;
	}
	return result;
}

double TFMonitorObj::Calc_Alpha_L(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID)
{
	double result = 0.0;
	int i = 0;
	int j = 0;
	double sum_Sij_j = 0.0;
	double Den = 0.0;
	double Alpha = 0.0;
	double den_dij = 0.0;
	double dii = 0.0;
	double TempAlpha = 0.0;
	double Tmp = 0.0;
	double dynBeta = 0.0;
  //tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: double;
	update_all_nodes_info(ActorID);     // update voltages on all buses
	Get_PQ_DI(NodeNumofDG, ActorID); // load measure
      // calclate alpha
     //with pnodeFMs^[NodeNumofDG] do
	switch(phase_num)
	{
		case 	1: //phase A
              //1.calculate d_ij*alpha_j summation
		{
			int stop = 0;
			den_dij = 0;
			TempAlpha = 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
			for(stop = Nodes, j = 0; j < stop; j++)
			{
				if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_nodeType_phase[1] == 1))     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
 //this phase has DG

                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
				{
					den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
					TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha1_dg;
                              //end;
				}
			}
			if(den_dij == 0)
				TempAlpha = 0.0;
			else
			{
				TempAlpha = TempAlpha / den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
			}
			/*# with pNodeFMs^[NodeNumofDG] do */
			{
				
				Tmp = Calc_Grdt_for_Alpha(NodeNumofDG, phase_num, ActorID);//  vl_gradient1_dg updated inside
				if((pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg != 0)
					dynBeta = (beta * Abs((int) Bii) * 100 / j) * (pNodeFMs)[NodeNumofDG].vl_V_ref1_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref1_dg / (pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg;
				(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = TempAlpha + dynBeta * (pNodeFMs)[NodeNumofDG].vl_Gradient1_dg;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha1_dg > 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = 1;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha1_dg <  - 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = (double) -1;
			}
			result = (pNodeFMs)[NodeNumofDG].vl_Alpha1_dg;
		}
		break; //phase B
              //1.calculate d_ij*alpha_j summation
		case 	2:
		{
			int stop = 0;
			den_dij = 0;
			TempAlpha = 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
			for(stop = Nodes, j = 0; j < stop; j++)
			{
				if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_nodeType_phase[2] == 1))     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
 //this phase has DG

                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
				{
					den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
					TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha2_dg;
                              //end;
				}
			}
			if(den_dij == 0)
				TempAlpha = 0.0;
			else
			{
				TempAlpha = TempAlpha / den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
			}
			/*# with pNodeFMs^[NodeNumofDG] do */
			{
				
				Tmp = Calc_Grdt_for_Alpha(NodeNumofDG, phase_num, ActorID);
				if((pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg != 0)
					dynBeta = (beta * Abs((int) Bii) * 100 / j) / (pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref2_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref2_dg; //
				(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = TempAlpha + dynBeta * (pNodeFMs)[NodeNumofDG].vl_Gradient2_dg;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha2_dg > 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = 1;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha2_dg <  - 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = (double) -1;
			}
			result = (pNodeFMs)[NodeNumofDG].vl_Alpha2_dg;
		}
		break; //phase C
                 //1.calculate d_ij*alpha_j summation
		case 	3:
		{
			int stop = 0;
			den_dij = 0;
			TempAlpha = 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
			for(stop = Nodes, j = 0; j < stop; j++)
			{
				if(((pNodeFMs)[j].vl_ndphases_dg == 3) || ((pNodeFMs)[j].vl_nodeType_phase[2] == 1))     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
 //this phase has DG

                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
				{
					den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
					TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha3_dg;
                              //end;
				}
			}
			if(den_dij == 0)
				TempAlpha = 0.0;
			else
			{
				TempAlpha = TempAlpha / den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
			}
			/*# with pNodeFMs^[NodeNumofDG] do */
			{
				
				Tmp = Calc_Grdt_for_Alpha(NodeNumofDG, phase_num, ActorID);
				if((pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg != 0)
					dynBeta = (beta * Abs((int) Bii) * 100 / j) / (pNodeFMs)[NodeNumofDG].vl_Qmax_phase_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref3_dg * (pNodeFMs)[NodeNumofDG].vl_V_ref3_dg; //
				(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = TempAlpha + dynBeta * (pNodeFMs)[NodeNumofDG].vl_Gradient3_dg;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha3_dg > 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = 1;
				if((pNodeFMs)[NodeNumofDG].vl_Alpha3_dg <  - 1)
					(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = (double) -1;
			}
			result = (pNodeFMs)[NodeNumofDG].vl_Alpha3_dg;
		}
		break; //pos seq value
		case 	0:
		break;
		default:
		  ;
		break;
	}
	return result;
}

/*----------------------------------------------------------------------------------*/

double TFMonitorObj::Calc_sum_dij_Alphaj(int NodeNumofDG, int phase_num, int ActorID)
{
	double result = 0.0;
	int j = 0;
	double sum_Sij_j = 0.0;
	update_all_nodes_info(ActorID);

     // calclate alpha
	/*# with pNodeFMs^[NodeNumofDG] do */
	{
		
		switch(phase_num)
		{
			case 	1: //phase A
			{
				int stop = 0;
				(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = 0.0;//init as zero
				sum_Sij_j = 0.0;
                 //for j := 1 to Nodes do
				for(stop = NodeNumofDG, j = 0; j < stop; j++)
				{
					(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha1_dg + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha1_dg;
					sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNumofDG * Nodes + j];
				}
				for(stop = Nodes, j = NodeNumofDG + 1; j < stop; j++)
				{
					(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha1_dg + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha1_dg;
					sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNumofDG * Nodes + j];
				}
				(pNodeFMs)[NodeNumofDG].vl_Alpha1_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha1_dg / sum_Sij_j;
                 //Alpha1 := Alpha1 - kcq* gradient1;
				result = (pNodeFMs)[NodeNumofDG].vl_Alpha1_dg;
			}
			break; //phase B
			case 	2:
			{
				int stop = 0;
				(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = 0.0;//init as zero
				sum_Sij_j = 0.0;
                 //for j := 1 to Nodes do
				for(stop = NodeNumofDG, j = 0; j < stop; j++)
				{
					(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha2_dg + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha2_dg;
					sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNumofDG * Nodes + j];
				}
				for(stop = Nodes, j = NodeNumofDG + 1; j < stop; j++)
				{
					(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha2_dg + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha2_dg;
					sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNumofDG * Nodes + j];
				}
				(pNodeFMs)[NodeNumofDG].vl_Alpha2_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha2_dg / sum_Sij_j;
                 //Alpha2 := Alpha2 - kcq* gradient2;
				result = (pNodeFMs)[NodeNumofDG].vl_Alpha2_dg;
			}
			break; //phase C
			case 	3:
			{
				int stop = 0;
				(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = 0.0;//init as zero
				sum_Sij_j = 0.0;
                 //for j := 1 to Nodes do
				for(stop = NodeNumofDG, j = 0; j < stop; j++)
				{
					(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha3_dg + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha3_dg;
					sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNumofDG * Nodes + j];
				}
				for(stop = Nodes, j = NodeNumofDG + 1; j < stop; j++)
				{
					(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha3_dg + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha3_dg;
					sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNumofDG * Nodes + j];
				}
				(pNodeFMs)[NodeNumofDG].vl_Alpha3_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha3_dg / sum_Sij_j;
                 //Alpha3 := Alpha3 - kcq* gradient3;
				result = (pNodeFMs)[NodeNumofDG].vl_Alpha3_dg;
			}
			break; //pos seq value
			case 	0:
			{
				int stop = 0;
				(pNodeFMs)[NodeNumofDG].vl_Alpha_dg = 0.0;//init as zero
				sum_Sij_j = 0.0;
                 //for j := 1 to Nodes do
				for(stop = NodeNumofDG, j = 0; j < stop; j++)
				{
					(pNodeFMs)[NodeNumofDG].vl_Alpha_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha_dg + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dg;
					sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNumofDG * Nodes + j];
				}
				for(stop = Nodes, j = NodeNumofDG + 1; j < stop; j++)
				{
					(pNodeFMs)[NodeNumofDG].vl_Alpha_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha_dg + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dg;
					sum_Sij_j = sum_Sij_j + (pCommMatrix)[NodeNumofDG * Nodes + j];
				}
				(pNodeFMs)[NodeNumofDG].vl_Alpha_dg = (pNodeFMs)[NodeNumofDG].vl_Alpha_dg / sum_Sij_j;
                 //Alpha := Alpha - kcq* gradient;
				result = (pNodeFMs)[NodeNumofDG].vl_Alpha_dg;
			}
			break;
			default:
			  ;
			break;
		}
	}
	return result;
}

double TFMonitorObj::AvgPmax()
{
	double result = 0.0;
	int i = 0;
	int k = 0;
	int stop = 0;
	result = 0.0;
	k = 1;
     //nodes;//all nodes included;
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		/*# with pNodeFMs^[i] do */
		{
			
			if(((pNodeFMs)[i].vl_PF_flag_dg == 1) && ((pNodeFMs)[i].vl_CC_switch_dg == true)) //
			{
				result = result + (pNodeFMs)[i].vl_Pmax_dg;
				result = result / k;
				k = k + 1;
			}
		}
	}
	return result;
}

double TFMonitorObj::AvgQmax()
{
	double result = 0.0;
	int i = 0;
	int k = 0;
	int stop = 0;
	result = 0.0;
	k = 1;
     //nodes;//all nodes included;
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		/*# with pNodeFMs^[i] do */
		{
			
			if(((pNodeFMs)[i].vl_QV_flag_dg == 1) && ((pNodeFMs)[i].vl_CC_switch_dg == true)) //volt/var control is on
			{
				result = result + (pNodeFMs)[i].vl_Qmax_dg;
				result = result / k;
				k = k + 1;
			}
		}
	}
	return result;
}
/*----------------------------------------------------------------------*/
/**/

String TFMonitorObj::Get_FileName(int ActorID)
{
	String result;
	result = GetOutputDirectory() + CircuitName_[ActorID] + "Mon_" + get_Name() + ".csv";
	return result;
}

double TFMonitorObj::Calc_fm_ul_0(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID)
{
	double result = 0.0;
	int dly = 0;
	int i = 0;
	int j = 0;
	double Den = 0.0;
	double den_dij = 0.0;
	double TempAlpha = 0.0;
	double Tmp = 0.0;
	double dfs_hide = 0.0;

        //update_all_nodes_info;     // update voltages on all buses
        //with pNodeFMs^[NodeNumofDG] do
       // begin
	switch(phase_num)
	{
		case 	0: //pos seq

          //1.calculate d_ij*alpha_j summation
		{
			den_dij = 0;
			TempAlpha = 0.0;
            /*-----------------------------------*/
            //no delay
			if(T_intvl_smpl == 0.0)
                // communication
			{
				int stop = 0;
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) && (((pNodeFMs)[j].vl_nodeType_phase[0] + (pNodeFMs)[j].vl_nodeType_phase[1] + (pNodeFMs)[j].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //this phase has DG
					{
						den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
						if(j != atk_node_num) // regular nodes

                             //Sumation of all alpha s
						{
							TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dgn;
						}
						else

                         // attack and defense -------------------------------------
   // node under attack
                             //Sumation of all alpha s
						{
							if((atk == true) && (ActiveCircuit[ActorID]->Solution->DynaVars.T >= atk_time))
								TempAlpha = TempAlpha + D_p * (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dgn;
							else
  // attack starts from here
								TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dgn;
						} /*--attack and defense ends---------------------------------*/
                         // attack and defense
						                         /*-----------------------------------*/
						if((atk == true) && (ActiveCircuit[ActorID]->Solution->DynaVars.T >= atk_time))
                            //and (ActiveCircuit[ActorID].Solution.DynaVars.IterationFlag = 1)

                              // if being attacked
						{
							TempAlpha = TempAlpha + (pNodeFMs)[j].d_atk;      // attack is added on
						}
                         /*--attack and defense ends---------------------------------*/
					}
				}
            //with delay
			}
			else
			{
				int stop = 0;
				for(stop = NodeNumofDG, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) && (((pNodeFMs)[j].vl_nodeType_phase[0] + (pNodeFMs)[j].vl_nodeType_phase[1] + (pNodeFMs)[j].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //this phase has DG
					{
						den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
                         //how many steps of delay from node j to node 'NodeNumofDG'
						dly = (pCommDelaySteps)[NodeNumofDG * Nodes + j];
						if(dly == 0)
						{
							TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dg;
						}
						else
						{
							TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_smpl_dg[1][dly];
						}
					}
				}
				j = NodeNumofDG;
				if(((pNodeFMs)[j].vl_ndphases_dg == 3) && (((pNodeFMs)[j].vl_nodeType_phase[0] + (pNodeFMs)[j].vl_nodeType_phase[1] + (pNodeFMs)[j].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //this phase has DG
				{
					den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
					TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dg;
				}
				for(stop = Nodes, j = NodeNumofDG + 1; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) && (((pNodeFMs)[j].vl_nodeType_phase[0] + (pNodeFMs)[j].vl_nodeType_phase[1] + (pNodeFMs)[j].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //this phase has DG
					{
						den_dij = den_dij + (pCommMatrix)[NodeNumofDG * Nodes + j];
						dly = (pCommDelaySteps)[NodeNumofDG * Nodes + j];
						if(dly == 0)
						{
							TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_Alpha_dg;
						}
						else
						{
							TempAlpha = TempAlpha + (pCommMatrix)[NodeNumofDG * Nodes + j] * (pNodeFMs)[j].vl_smpl_dg[1][dly];
						}
					}
				}
			}

            /*-----------------------------------*/
            // from sumation to ul
			if(den_dij == 0)
				TempAlpha = 0.0;
			else
			{
				TempAlpha = TempAlpha / den_dij;
			}
            // if this node is the node under attack, change the sign of that
			if((NodeNumofDG == atk_node_num) && (atk == true) && (ActiveCircuit[ActorID]->Solution->DynaVars.T >= atk_time))
			{
				Tmp = (TempAlpha - D_p * (pNodeFMs)[NodeNumofDG].vl_Alpha_dgn);
			}
			else
			Tmp = (TempAlpha - (pNodeFMs)[NodeNumofDG].vl_Alpha_dgn);

                 // attack and defense
			                 /*-----------------------------------*/
			if((atk == true) && (ActiveCircuit[ActorID]->Solution->DynaVars.T >= atk_time))
			{
				dfs_hide = organise_dfs_node(NodeNumofDG);  // x_i'  =  A_i x + {{ \beta K_i z }}+ \beta B_i x_0 + d_i
                     //TempAlpha := TempAlpha + beta_dfs * tmp;  // defense is added on
                     /*--attack and defense ends---------------------------------*/
				Tmp = Tmp + beta_dfs * dfs_hide;
			}
            //Tolerance of alpha_i alpha_j
			if(Abs((int) Tmp) <= Volt_Trhd * 0.01)
                 //Result := 0.0
				;
			else
				result = Tmp * (pNodeFMs)[NodeNumofDG].vl_kc_ul_dg;
            // if there is attck
		}
		break;
		default:
		  ;
		break;
	}
       //end;
       //result := 0;
	return result;
}

double TFMonitorObj::Calc_fm_us_0(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID)
{
	double result = 0.0;
	int i = 0;
	int j = 0;
	double Den = 0.0;
	double Tmp = 0.0;
	double V = 0.0;
	double VRef = 0.0;
	double den_dij = 0.0;
	double tempUl = 0.0;
	double pHi = 0.0;
        //update voltage
	j = NodeNumofDG;
	Get_PDElem_terminal_voltage(NodeNumofDG, (pNodeFMs)[NodeNumofDG].vl_strMeasuredName, (pNodeFMs)[NodeNumofDG].vl_terminalNum, ActorID);
        //calc gradient
        //with pNodeFMs^[NodeNumofDG] do
        //begin
	V = (pNodeFMs)[NodeNumofDG].vl_V;
	VRef = (pNodeFMs)[NodeNumofDG].vl_V_ref_dg;
	switch(phase_num)
	{
		case 	0: //pos seq
		{
			Den = Abs((int) ((pNodeFMs)[NodeNumofDG].vl_q_DG - (pNodeFMs)[NodeNumofDG].vl_Q_Di - V * V * Bii));   // pos ctrl: Bii use the first one
			if(Abs((int) Den) < EPSILON)
				Den = EPSILON;
			(pNodeFMs)[NodeNumofDG].vl_Gradient_dg = (VRef - V) * V / (Den) / (VRef * VRef);//*vl_Qmax_dg;//
			(pNodeFMs)[NodeNumofDG].vl_Gradient_dg = (beta * VRef * VRef) * Abs((int) Bii) * 100 * (pNodeFMs)[NodeNumofDG].vl_Gradient_dg;

                  //(beta* abs(Bii)*100/j)/vl_Qmax_phase_dg*
			if(Abs((int) (VRef - V)) <= Volt_Trhd * VRef)
				(pNodeFMs)[NodeNumofDG].vl_Gradient_dg = 0.0;
			Tmp = Abs((int) (VRef - V));
			if((pNodeFMs)[NodeNumofDG].vl_Gradient_dg > 1)
				(pNodeFMs)[NodeNumofDG].vl_Gradient_dg = 1;
			if((pNodeFMs)[NodeNumofDG].vl_Gradient_dg <  - 1)
				(pNodeFMs)[NodeNumofDG].vl_Gradient_dg = (double) -1;
			result = (pNodeFMs)[NodeNumofDG].vl_Gradient_dg;


                  // the following only works for the node under attack
			if(j == atk_node_num)
                    // in dynamic simulation
			{
				if(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode == DYNAMICMODE)
                       // attack and defense has been set
				{
					if((atk == true) && (dfs == true))
                        //if current time is over attack time
					{
						if(ActiveCircuit[ActorID]->Solution->DynaVars.T > atk_time)
                            // if the attack is of the second type, then phi =0
						{
							if((pNodeFMs)[atk_node_num].d_atk0 == 0)
								pHi = 0.0;
							else

                              // if the attack is of the first type, then phi =0
							{
								if(beta_dfs != 0)
                                          //Set a coeffient for beta_dfs
                                          //if NodeNumofDG<>atk_node_num then // only for those nodes not attacked
								{
									int stop = 0;
									den_dij = 0;
									tempUl = 0.0;
									for(stop = Nodes, i = 0; i < stop; i++)
									{
										if(((pNodeFMs)[i].vl_ndphases_dg == 3) && (((pNodeFMs)[i].vl_nodeType_phase[0] + (pNodeFMs)[i].vl_nodeType_phase[1] + (pNodeFMs)[i].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //this phase has DG

                                                 //Sumation of all Z and alpha s
										{
											den_dij = den_dij + (pCommMatrix)[j * Nodes + i];
											tempUl = tempUl + (pCommMatrix)[j * Nodes + i] * (pNodeFMs)[i].vl_Alpha_dgn;
										}
                                              ///
									}
                                          // average
									if(den_dij == 0)
									{
										tempUl = 0.0;
									}
									else
									{
										tempUl = tempUl / den_dij;
									}
									tempUl = (tempUl - (pNodeFMs)[j].vl_Alpha_dgn);
                                          // calculate phi
									pHi = Coef_Phi(Abs((int) tempUl));
								}
							}
                                    //
							result = D_p * (1 + pHi * beta_dfs) * (pNodeFMs)[NodeNumofDG].vl_Gradient_dg;
						}
					}
				}
			}

                  //if result >1 then result := 1 ;
                  //if result <-1 then result := -1 ;
		}
		break;
		default:
		  ;
		break;
	}
       //end;
	return result;
}

double TFMonitorObj::Coef_Phi(double X)
{
	double result = 0.0;
	double X1 = 0.0;
	double X2 = 0.0;
	double X3 = 0.0;
	double Y1 = 0.0;
	double Y2 = 0.0;
	double Y3 = 0.0;
	double y0 = 0.0;
	double overall = 0.0;
	overall = D_beta;
	X1 = 0.005;
	X2 = 0.01;
	X3 = 0.05;
	Y1 = 1.0;
	Y2 = 0.5;
	Y3 = 0.0;
	y0 = 1.0;
	if(X <= X1)
		result = X * (Y1 - y0) / X1 + y0;
	else
	{
		if(X <= X2)
			result = Y1 + (X - X1) * (Y2 - Y1) / (X2 - X1);
		else
		{
			if(X <= X3)
				result = Y2 + (X - X2) * (Y3 - Y2) / (X3 - X2);
			else
				result = Y1;
		}


     //result := 1.0 ;
	}
	result = overall * result;
	return result;
}

double TFMonitorObj::Getgradient(int j, int phase_num, double Bii, double Volt_Trhd)
{
	double result = 0.0;
	double V = 0.0;
	double VRef = 0.0;
	double Den = 0.0;
	double Tmp = 0.0;
	V = (pNodeFMs)[j].vl_V;
	VRef = (pNodeFMs)[j].vl_V_ref_dg;
	switch(phase_num)
	{
		case 	0: //pos seq
		{
			Den = Abs((int) ((pNodeFMs)[j].vl_q_DG - (pNodeFMs)[j].vl_Q_Di - V * V * Bii));   // pos ctrl: Bii use the first one
			if(Abs((int) Den) < EPSILON)
				Den = EPSILON;
			Tmp = (VRef - V) * V / (Den) / (VRef * VRef);//*vl_Qmax_dg;//


                   //(beta* abs(Bii)*100/j)/vl_Qmax_phase_dg*

                  //if abs(vref - v)<= Volt_Trhd*vref then tmp := 0.0;
			if(Tmp > 1)
				Tmp = 1;
			if(Tmp <  - 1)
				Tmp = (double) -1;
			result = Tmp;
		}
		break;
		default:
		  ;
		break;
	}
	return result;
} //abandoned

void TFMonitorObj::Agnt_smpl(int NodeNumofDG, int phase_num, int ActorID)
{
	double crnt_time = 0.0;
	int i = 0;
    //if True then
	if(ActiveCircuit[ActorID]->Solution->DynaVars.IterationFlag == 1)   /*1= Same Time Step as last iteration*/
          //
	{
		if((pNodeFMs)[NodeNumofDG].vl_SmplCnt == 0)//the first step
		{
			int stop = 0;
			for(stop = MaxLocalMem, i = 1; i <= stop; i++)
			{
				(pNodeFMs)[NodeNumofDG].vl_smpl_dg[1][i] = (pNodeFMs)[NodeNumofDG].vl_Alpha_dg;
			}
			(pNodeFMs)[NodeNumofDG].vl_smpl_dg[2][i] = (pNodeFMs)[NodeNumofDG].vl_AlphaP_dg;
		}
          //
		crnt_time = ActiveCircuit[ActorID]->Solution->DynaVars.intHour * 3600 + ActiveCircuit[ActorID]->Solution->DynaVars.T;
          //Move the array only at the first time-step

          //if t_k greater or equal to current sample time plus smp interval, do another sample
		if(crnt_time >= (T_intvl_smpl + (pNodeFMs)[NodeNumofDG].vl_crnt_smp_time))
              //if Trunc(crnt_time/T_intvl_smpl) >= pNodeFMs^[NodeNumofDG].vl_SmplCnt +1 then
              //begin
                 //save alf into the first entry of smpl_ary for communication
                 //alpha
		{
			int stop = 0;
			(pNodeFMs)[NodeNumofDG].vl_smpl_dg[1][0] = (pNodeFMs)[NodeNumofDG].vl_Alpha_dg;// [0] is the newest value
                                                                                                // [0] and [1] are always the same
                 //alphaP
			(pNodeFMs)[NodeNumofDG].vl_smpl_dg[2][0] = (pNodeFMs)[NodeNumofDG].vl_AlphaP_dg;//
                 //0 seq voltage
                  //pNoddeFMs^[NodeNumofDG].vl_smpl_dg[3][0] := pNodeFMs^[NodeNumofDG].vl_V;//
			for(stop = MaxLocalMem - 1, i = 0; i <= stop; i++)
			{ // [0]->[1],[MaxLocalMem-1]->[MaxLocalMem]
				(pNodeFMs)[NodeNumofDG].vl_smpl_dg[1][MaxLocalMem - i] = (pNodeFMs)[NodeNumofDG].vl_smpl_dg[1][MaxLocalMem - i];
				(pNodeFMs)[NodeNumofDG].vl_smpl_dg[2][MaxLocalMem - i] = (pNodeFMs)[NodeNumofDG].vl_smpl_dg[2][MaxLocalMem - i];
			}

                 //vl_SmplCnt increase
			++(pNodeFMs)[NodeNumofDG].vl_SmplCnt;
                  //update vl_crnt_time
			(pNodeFMs)[NodeNumofDG].vl_crnt_smp_time = crnt_time;
              //end;
		}
	}
}

void TFMonitorObj::Init_delay_array(int NodeNumofDG, int ActorID)
{
	int i = 0;
              //measure all voltages
	int stop = 0;
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		Get_PDElem_terminal_voltage(i, (pNodeFMs)[i].vl_strMeasuredName, (pNodeFMs)[i].vl_terminalNum, ActorID);
	}
              // inti delay array
	for(stop = MaxLocalMem, i = 0; i <= stop; i++)
	{
                   //alpha
		(pNodeFMs)[NodeNumofDG].vl_smpl_dg[1][i] = (pNodeFMs)[NodeNumofDG].vl_Alpha_dg;
                   //alphaP
		(pNodeFMs)[NodeNumofDG].vl_smpl_dg[2][i] = (pNodeFMs)[NodeNumofDG].vl_AlphaP_dg;
                   // vl_v, which is the 0 seq. voltage
		(pNodeFMs)[NodeNumofDG].vl_smpl_dg[3][i] = (pNodeFMs)[NodeNumofDG].vl_V;
	}
}  // NodeNuminClstr: node number in cluster

double TFMonitorObj::Calc_Gradient_ct_P(int NodeNuminClstr, int phase_num, int ActorID)
{
	double result = 0.0;
	double dvDGtemp = 0.0;
	double Grdnt_P = 0.0;
       //tempCplx
	dvDGtemp = ((pNodeFMs)[NodeNuminClstr].vl_V - (pNodeFMs)[NodeNuminClstr].vl_V_ref_dg) / (pNodeFMs)[NodeNuminClstr].vl_V_ref_dg; //
       // if this DG is above 1.05, then it should have P curtail gradient
       //if ( ActiveCircuit.Solution.bCurtl=true ) and (dvDGtemp>0.0) then //overall system need control
	if(ActiveCircuit[ActorID]->Solution->bCurtl == true)
	{
		if(ld_fm_info[0].b_Curt_Ctrl == true) // if false, the curtailment will be zero for any node in this cluster

                              //- activecircuit.Solution.LD_FM[0].volt_lw_lmt);
			Grdnt_P = (ActiveCircuit[ActorID]->Solution->LD_FM[0].volt_lwst - 1.0);
	}
	else
	Grdnt_P = 0.0;
      //!!!!!!!!!!!!!
	switch(phase_num)
	{
		case 	0: //pos seq
		{
			result = Grdnt_P; //
		}
		break;
		case 	1:
		{
			result = Grdnt_P; //
		}
		break;
		case 	2:
		{
			result = Grdnt_P; //
		}
		break;
		case 	3:
		{
			result = Grdnt_P; //
		}
		break;
		default:
		  ;
		break;
	}
	return result;
}
//////////////////////////
/*-------------------------*/  // NodeNuminClstr: node number in cluster //with delay

double TFMonitorObj::Calc_ul_P(int NodeNuminClstr, int phase_num)
{
	double result = 0.0;
	int j = 0;
	double den_dij = 0.0;
	double TempAlphaP = 0.0;
	int dly = 0;
     //alphaP = sum (alphaP) + Beta * Gp

     //with pNodeFMs^[NodeNuminClstr] do
     // begin
	switch(phase_num)
	{
		case 	0: //pos seq

              //1.calculate d_ij*alpha_j summation
		{
			den_dij = 0;
			TempAlphaP = 0;
                 /*-----------------------------------*/
          //no delay
			if(T_intvl_smpl == 0.0)
			{
				int stop = 0;
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) && (((pNodeFMs)[j].vl_nodeType_phase[0] + (pNodeFMs)[j].vl_nodeType_phase[1] + (pNodeFMs)[j].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
  // has 3-phase DG
      //                         if (pnodeFMs^[j].vl_ndphases_dg = 3) then   //only 3 phase nodes

                                          //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                          //begin
					{
						den_dij = den_dij + (pCommMatrix)[NodeNuminClstr * Nodes + j];
						TempAlphaP = TempAlphaP + (pCommMatrix)[NodeNuminClstr * Nodes + j] * (pNodeFMs)[j].vl_AlphaP_dg;
                                          //end;
					}
				}
			}
			else

           // with delay
			{
				int stop = 0;

				for(stop = NodeNuminClstr, j = 0; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) && (((pNodeFMs)[j].vl_nodeType_phase[0] + (pNodeFMs)[j].vl_nodeType_phase[1] + (pNodeFMs)[j].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //has 3-phase  DG
					{
						den_dij = den_dij + (pCommMatrix)[NodeNuminClstr * Nodes + j];
                             //how many steps of delay from node j to node 'NodeNumofDG'
						dly = (pCommDelaySteps)[NodeNuminClstr * Nodes + j];
						if(dly == 0)
						{
							TempAlphaP = TempAlphaP + (pCommMatrix)[NodeNuminClstr * Nodes + j] * (pNodeFMs)[j].vl_AlphaP_dg;
						}
						else
						{
							TempAlphaP = TempAlphaP + (pCommMatrix)[NodeNuminClstr * Nodes + j] * (pNodeFMs)[j].vl_smpl_dg[2][dly];
						}
					}
				}
				j = NodeNuminClstr;
				if(((pNodeFMs)[j].vl_ndphases_dg == 3) && (((pNodeFMs)[j].vl_nodeType_phase[0] + (pNodeFMs)[j].vl_nodeType_phase[1] + (pNodeFMs)[j].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //has 3-phase DG
				{
					den_dij = den_dij + (pCommMatrix)[NodeNuminClstr * Nodes + j];
					TempAlphaP = TempAlphaP + (pCommMatrix)[NodeNuminClstr * Nodes + j] * (pNodeFMs)[j].vl_AlphaP_dg;
				}
				for(stop = Nodes, j = NodeNuminClstr + 1; j < stop; j++)
				{
					if(((pNodeFMs)[j].vl_ndphases_dg == 3) && (((pNodeFMs)[j].vl_nodeType_phase[0] + (pNodeFMs)[j].vl_nodeType_phase[1] + (pNodeFMs)[j].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //has 3-phase DG
					{
						den_dij = den_dij + (pCommMatrix)[NodeNuminClstr * Nodes + j];
						dly = (pCommDelaySteps)[NodeNuminClstr * Nodes + j];
						if(dly == 0)
						{
							TempAlphaP = TempAlphaP + (pCommMatrix)[NodeNuminClstr * Nodes + j] * (pNodeFMs)[j].vl_AlphaP_dg;
						}
						else
						{
							TempAlphaP = TempAlphaP + (pCommMatrix)[NodeNuminClstr * Nodes + j] * (pNodeFMs)[j].vl_smpl_dg[2][dly];
						}
					}
				}
			}
			if(den_dij == 0)
				TempAlphaP = 0.0;
			else
			{
				TempAlphaP = TempAlphaP / den_dij;   //the average
			}
              //Tolerance of alphap_i alphap_j
			TempAlphaP = TempAlphaP - (pNodeFMs)[NodeNuminClstr].vl_AlphaP_dg; //uL for cooperative control of active power
			if(Abs((int) TempAlphaP) < 0.002)
				result = 0.0;
			else
				result = TempAlphaP;
		}
		break;
		default:
		  ;
		break;
	}
     // end;
	return result;
} //all nodes , p.u. value

void TFMonitorObj::update_node_info_each_time_step(int ActorID)
{
	int Den = 0;
	int i = 0;
	double v0_tmp = 0.0;
	int stop = 0;
	dlt_z0 = 0.0;
	Den = 0;
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		if(((pNodeFMs)[i].vl_ndphases_dg == 3) && (((pNodeFMs)[i].vl_nodeType_phase[0] + (pNodeFMs)[i].vl_nodeType_phase[1] + (pNodeFMs)[i].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
		{
			(pNodeFMs)[i].vl_Alpha_dgn = (pNodeFMs)[i].vl_Alpha_dg;
			(pNodeFMs)[i].z_dfsn = (pNodeFMs)[i].z_dfs;
			dlt_z0 = dlt_z0 + (pNodeFMs)[i].vl_Gradient_dg;
		}
		Den = Den + 1;
	}

     //sumation or average
	if(Den != 0)
		dlt_z0 = -dlt_z0 / Den; // gredient (v-vref)
}
//Calculate equivalent omega and delta

void TFMonitorObj::Calc_P_freq_fm(int ActorID)
{
	double domg = 0.0;
	double ddlt = 0.0;
	double dPm = 0.0;
	double DeltaP = 0.0;
	double Tmp = 0.0;
      //first time initialization
	if(ActiveCircuit[ActorID]->Solution->DynaVars.T < init_time)
		Pm_fm = this->Get_power_trans(ActorID);

      //preparation : calculate Delta P
	Tmp = this->Get_power_trans(ActorID);
	DeltaP = Pm_fm - Tmp;
      //derivatives
      //ddlt := omg_fm;
	domg = (DeltaP / (kVA_fm * 1000) - D_fm * omg_fm) / M_fm;
	dPm = -Ki_fm * omg_fm * (kVA_fm * 1000) / Tau_fm;
      //integral
	if(ActiveCircuit[ActorID]->Solution->Get_SolMode() == DYNAMICMODE)
          //dlt_fm := dlt_fm + ddlt * ActiveCircuit[ActorID].Solution.DynaVars.h;
	{
		Pm_fm = Pm_fm + dPm * ActiveCircuit[ActorID]->Solution->DynaVars.h;
		omg_fm = omg_fm + domg * ActiveCircuit[ActorID]->Solution->DynaVars.h;
	}
	comp_omg = omg_fm + DeltaP / (kVA_fm * 1000) / D_fm; //comp_omg is (\Delta f + \Delta P / B)
} //all nodes , p.u. value

void TFMonitorObj::update_ld_dly(int ActorID)
{
	int i = 0;
	int j = 0;
	int ndlys = 0;
	double v0_tmp = 0.0;
	double crnt_time = 0.0;
	int stop = 0;
	ld_fm_info[0].volt_avg = 0.0; //recalculate voltage average
	ld_fm_info[0].volt_lwst = 999999; //search new value at each round
	ld_fm_info[0].volt_hghst = (double) -99999;
	for(stop = Nodes, i = 0; i < stop; i++)
	{
           //update vl_v1/v2/v3, vl_v_1c/v_2c/v_3c, update vl_v for node i
		Get_PDElem_terminal_voltage(i, (pNodeFMs)[i].vl_strMeasuredName, (pNodeFMs)[i].vl_terminalNum, ActorID);
           //synchronous sampling
		if(T_intvl_smpl == 0.0)
                 // pNodeFMs^[i].vl_smpl_dg[i][j] is not used
		{
			v0_tmp = (pNodeFMs)[i].vl_V / ((pNodeFMs)[i].vl_basevolt);
		}
		else

           //asynchronous sampling
                 //update pNodeFMs^[i].vl_smpl_dg[i][j] first
		{
			if((pNodeFMs)[i].vl_SmplCnt == 0)//the first step
			{
				int stop1 = 0;
				for(stop1 = MaxLocalMem, j = 0; j <= stop1; j++)
				{
                                 //alphas
					(pNodeFMs)[i].vl_smpl_dg[1][j] = (pNodeFMs)[i].vl_Alpha_dg;
				}
				(pNodeFMs)[i].vl_smpl_dg[2][j] = (pNodeFMs)[i].vl_AlphaP_dg;
                                 //voltage
				(pNodeFMs)[i].vl_smpl_dg[3][j] = (pNodeFMs)[i].vl_V;  // 0 seq.
			}
                 //
			crnt_time = ActiveCircuit[ActorID]->Solution->DynaVars.intHour * 3600 + ActiveCircuit[ActorID]->Solution->DynaVars.T;
                 //Move the array only at the first time-step

                 //if t_k greater or equal to current sample time plus smp interval, do another sample
			if(crnt_time >= (T_intvl_smpl + (pNodeFMs)[i].vl_crnt_smp_time))

                     //if Trunc(crnt_time/T_intvl_smpl) >= pNodeFMs^[NodeNumofDG].vl_SmplCnt +1 then
                     //begin
                        //save alf into the first entry of smpl_ary for communication
                        //alpha
			{
				int stop1 = 0;
				(pNodeFMs)[i].vl_smpl_dg[1][0] = (pNodeFMs)[i].vl_Alpha_dg;// [0] is the newest value
                        //alphaP
				(pNodeFMs)[i].vl_smpl_dg[2][0] = (pNodeFMs)[i].vl_AlphaP_dg;//
                        // VL_V  //0 seq voltage
				(pNodeFMs)[i].vl_smpl_dg[3][0] = (pNodeFMs)[i].vl_V;//


                         //pNoddeFMs^[NodeNumofDG].vl_smpl_dg[3][0] := pNodeFMs^[NodeNumofDG].vl_V;//
				for(stop1 = MaxLocalMem - 1, j = 0; j <= stop1; j++)
				{ // [0]->[1],[MaxLocalMem-1]->[MaxLocalMem]
					(pNodeFMs)[i].vl_smpl_dg[1][MaxLocalMem - j] = (pNodeFMs)[i].vl_smpl_dg[1][MaxLocalMem - j];
					(pNodeFMs)[i].vl_smpl_dg[2][MaxLocalMem - j] = (pNodeFMs)[i].vl_smpl_dg[2][MaxLocalMem - j];
					(pNodeFMs)[i].vl_smpl_dg[3][MaxLocalMem - j] = (pNodeFMs)[i].vl_smpl_dg[3][MaxLocalMem - j];
				}

                        //vl_SmplCnt increase
				++(pNodeFMs)[i].vl_SmplCnt;
                         //update vl_crnt_time
				(pNodeFMs)[i].vl_crnt_smp_time = crnt_time;
                     //end;
			}
                 // delay steps from agent to virtual leader
			ndlys = (pCommDelaySteps)[virtual_Ld_Nd * Nodes + i];
                 // total delay steps: ndlys+nup_dlys
                 //if pnodefms^[i].vl_basevolt <> 0.0 then
			v0_tmp = (pNodeFMs)[i].vl_smpl_dg[3][ndlys + nup_dlys] / ((pNodeFMs)[i].vl_basevolt);
		}
           //update highest voltage
		if(ld_fm_info[0].volt_hghst < v0_tmp)
		{
			ld_fm_info[0].volt_hghst = v0_tmp;
			ld_fm_info[0].ndnum_hghst = i;
		}
            //update lowest voltage
		if(ld_fm_info[0].volt_lwst > v0_tmp)
		{
			ld_fm_info[0].volt_lwst = v0_tmp;
			ld_fm_info[0].ndnum_lwst = i;
		}

           //other information should be updated?
           //
		ld_fm_info[0].volt_avg = ld_fm_info[0].volt_avg + v0_tmp;  //p.u.
	}
     //avg of valtage
	ld_fm_info[0].volt_avg = ld_fm_info[0].volt_avg / Nodes;
}

//attack and defense
 // update d_i

void TFMonitorObj::update_attack(int ActorID)
{
	double dlt_d = 0.0;
	int j = 0;
      //attack and defense at this step
	       /*-----------------------------------*/
	if(atk == false)
		return;
	dlt_d = 0.0; // no dynamic for now
	if((atk == true) && (ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode == DYNAMICMODE) && (ActiveCircuit[ActorID]->Solution->DynaVars.T >= atk_time))
           // initialization first, only once
	{
		int stop = 0;
		if(d_atk_inited == false)
		{
			int stop = 0;
			for(stop = Nodes, j = 0; j < stop; j++)
			{
				if(j == atk_node_num) // only the node being attacked is affected
				{
					(pNodeFMs)[j].d_atk = (pNodeFMs)[atk_node_num].d_atk0; //the
				}
			}
			d_atk_inited = true;
		}
           // attack
		for(stop = Nodes, j = 0; j < stop; j++)
		{
			if(j == atk_node_num) // only the node being attacked is affected
			{
				(pNodeFMs)[j].d_atk = (pNodeFMs)[j].d_atk + ActiveCircuit[ActorID]->Solution->DynaVars.h * dlt_d;
			}
		}
	}
	else
	{
		int stop = 0;
		for(stop = Nodes, j = 0; j < stop; j++)
		{
           // no attack
			(pNodeFMs)[j].d_atk = 0.0;
		}
	}
}// update z_i

void TFMonitorObj::update_defense(int ActorID)
{
	double dlt_z = 0.0;
	int j = 0;
	double Bii = 0.0;
	int den_dij = 0;
	int den_dij_z = 0;
	double tempZ = 0.0;
	double TempAlpha = 0.0;
	int DevIndex = 0;
	int ndref = 0;
	TDSSCktElement* tempElement = nullptr;
	TPowerTerminal* tempTerminal = nullptr;
	int i = 0;
	if(dfs == false)                                           // if no defense
     //or (ActiveCircuit[ActorID].Solution.DynaVars.t < atk_time) // if no attack
		return;
	if(ActiveCircuit[ActorID]->Solution->DynaVars.SolutionMode == DYNAMICMODE)
	{
		if(ActiveCircuit[ActorID]->Solution->DynaVars.T <= atk_time)
             // IF THERE IS NO ATTACK YET, Z FOLLOWS ALPHA
		{
			int stop = 0;
			for(stop = Nodes, j = 0; j < stop; j++)
			{
				(pNodeFMs)[j].z_dfs = (pNodeFMs)[j].vl_Alpha_dg; //the let z : alpha
				(pNodeFMs)[j].z_dfsn = (pNodeFMs)[j].z_dfs;
			}
		}
		if(ActiveCircuit[ActorID]->Solution->DynaVars.T >= atk_time)
              //calculate the initial value for z_dfs
		{
			int stop = 0;
			if(z_dfs_inited == false)
			{
				int stop = 0;
				for(stop = Nodes, j = 0; j < stop; j++)
				{
					(pNodeFMs)[j].z_dfs = (pNodeFMs)[j].vl_Alpha_dgn; //the let z : alpha
					(pNodeFMs)[j].z_dfsn = (pNodeFMs)[j].z_dfs;
				}
                     // has been initiated
				z_dfs_inited = true;
			}


              //update for each node
			for(stop = Nodes, j = 0; j < stop; j++)
			{
                  // x_i'  =  A_i x + \beta K_i z + \beta B_i x_0 + d_i
                  // z_i'  =  H_i Z + \beta G_i x + \beta D_i x_0
                  // calculate z_i
                  //////////////////////
                  // derivative calculation
				int stop1 = 0;
				dlt_z = dlt_z0; //dlt_z0 will be update at each time step by average of gradient     actually this is -us_i
                                /*
                                dlt_z := 0.0;
              //(1)/\beta D_i x_0
                                ndref := 1;

                                Bii := 1.0;
                                Devindex := GetCktElementIndex(pNodeFMs[j].vl_strMeasuredName) ;                   // Global function
                                IF DevIndex>0 THEN Begin                                       // Monitored element must already exist
                                    tempElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex) ;
                                    tempTerminal := tempElement.Terminals^[pNodeFMs[j].vl_terminalNum] ;
                                    ndref := tempTerminal.TermNodeRef^[1] ;
                                    Bii := ActiveCircuit[ActorID].Solution.NodeYii[tempTerminal.TermNodeRef^[1] ].im ;
                                end;

                                //D_beta := 0.05;

                                dlt_z :=  Getgradient(j,0,Bii,pNodeFMs[j].vl_volt_thrd_dg) ;
                                */

              //(2,3)/////(2)/ /  calculate H_i Z ; H_i = A_i;  //(3)/  calculate  \beta G_i x ; G_i = A_i
                  // j is the outer loop
                  // pCommMatrix is used as the matrix for H_i , G_i, K_i
				den_dij = 0;
				tempZ = 0.0;
				TempAlpha = 0.0;
				for(stop1 = Nodes, i = 0; i < stop1; i++)
				{
					if(((pNodeFMs)[i].vl_ndphases_dg == 3) && (((pNodeFMs)[i].vl_nodeType_phase[0] + (pNodeFMs)[i].vl_nodeType_phase[1] + (pNodeFMs)[i].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //this phase has DG

                         //Sumation of all Z and alpha s
					{
						den_dij_z = den_dij + (pCommHide)[j * Nodes + i];
						tempZ = tempZ + (pCommHide)[j * Nodes + i] * (pNodeFMs)[i].z_dfsn;
						den_dij = den_dij + (pCommMatrix)[j * Nodes + i];
						TempAlpha = TempAlpha + (pCommMatrix)[j * Nodes + i] * (pNodeFMs)[i].vl_Alpha_dgn;
					}
                      ///
				}
                  // average
				if(den_dij == 0)
				{
					tempZ = 0.0;
					TempAlpha = 0.0;
				}
				else
				{
					tempZ = tempZ / den_dij_z;
					TempAlpha = TempAlpha / den_dij;
				}
				tempZ = (tempZ - (pNodeFMs)[j].z_dfsn);
				TempAlpha = (TempAlpha - (pNodeFMs)[j].vl_Alpha_dgn);

                  // z_i'  =  H_i Z + \beta G_i x + \beta D_i x_0
				dlt_z = tempZ + beta_dfs * TempAlpha;//- beta_dfs *dlt_z0;// - pNodeFMs^[j].z_dfsn/den_dij;// - 0.1* pNodeFMs[j].z_dfs ; //+ pNodeFMs[j].vl_kcq_dg*dlt_z0 ;
                  // integration
                  //if abs(dlt_z) < 0.003 then dlt_z := 0.0 ;
				(pNodeFMs)[j].z_dfs = (pNodeFMs)[j].z_dfsn + dlt_z * ActiveCircuit[ActorID]->Solution->DynaVars.h;
			}
		}
	}
}    // calculate K_i z  // x_i'  =  A_i x + \beta K_i z + \beta B_i x_0 + d_i

double TFMonitorObj::organise_dfs_node(int j)
{
	double result = 0.0;
	int i = 0;
	int den_dij = 0;
	double tempZ = 0.0;
      // x_i'  =  A_i x - \beta K_i z + \beta B_i x_0 + d_i
      // z_i'  =  H_i Z + \beta G_i x + \beta D_i x_0

      // this function is to calculate
      // K_i z
	int stop = 0;
	den_dij = 0;
	tempZ = 0.0;
	for(stop = Nodes, i = 0; i < stop; i++)
	{
		if(((pNodeFMs)[i].vl_ndphases_dg == 3) && (((pNodeFMs)[i].vl_nodeType_phase[0] + (pNodeFMs)[i].vl_nodeType_phase[1] + (pNodeFMs)[i].vl_nodeType_phase[2]) == 3))   //only 3 phase nodes
 //this phase has DG
		{
			den_dij = den_dij + (pCommMatrix)[j * Nodes + i];
			tempZ = tempZ + (pCommMatrix)[j * Nodes + i] * (pNodeFMs)[i].z_dfsn;
		}
	}

      //average
	if(den_dij == 0)
		tempZ = 0.0;
	else
	{
		tempZ = tempZ / den_dij;
	}
	result = -(tempZ - (pNodeFMs)[j].z_dfsn);// - pNodeFMs^[j].z_dfsn/den_dij; // should be ZERO at last
      //result := TempZ;
      //what if defens is zdfs
      //result := pNodeFMs^[j].z_dfs;
	return result;
}
  //WriteDLLDebugFile('Monitor');




}  // namespace Fmonitor

