#ifndef FmonitorH
#define FmonitorH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "MeterClass.h"
#include "MeterElement.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "PointerList.h"
#include "VLNodeVars.h"
#include "LD_fm_infos.h"
#include "d2c_structures.h"

namespace Fmonitor
{



/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Change Log
   By Ying @UCF 10/27/2017

*/
typedef AnsiChar TFMonitorStrBuffer[256/*# range 1..256*/];
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   /*This has to be named TDSSMonitor because Delphi has a TMonitor Class and the compiler will get confused*/

class TDSSFMonitor : public MeterClass::TMeterClass
{
	friend class TFMonitorObj;
public:
	typedef MeterClass::TMeterClass inherited;	
private:
protected:
	void DefineProperties();
	virtual int MakeLike(const String MonitorName);
public:
	TDSSFMonitor();
	virtual ~TDSSFMonitor();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	virtual void ResetAll(int ActorID);
	virtual void SampleAll(int ActorID);  // Force all monitors to take a sample
       //Procedure SampleAllMode5;  // Sample just Mode 5 monitors
	virtual void SaveAll(int ActorID);   // Force all monitors to save their buffers to disk
	       /*update FM leader information*/
	void update_sys_ld_info(int ActorID); //all FMs
	void Calc_P_freq(int ActorID);// calculte frequency for each cluster
       //attack and defense
	void update_atks(int ActorID);
	void update_defense_layer(int ActorID);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TFMonitorObj : public MeterElement::TMeterElement
{
	friend class TDSSFMonitor;
public:
	typedef MeterElement::TMeterElement inherited;	
private:
       //pTVLeaderVars : TPointerList;   //save all nodes information
       //VL_node : TVLnodeVars;//  leader node of this cluster
	int Nodes; //nodes of this cluster  \\default nodes := 33;
	Arraydef::pSmallIntArray pCommMatrix;// communication matrix of this cluster
	double P_trans_ref; // Power Ref on metered elemet, if mode =1 real power of this cluster will be used
	int p_mode;
	Ucomplex::complex tempCplx;
	int BufferSize;
	int Hour;
	double Sec;    // last time entered in the buffer
	Arraydef::pSingleArray MonBuffer;
	int BufPtr;  // point to present (last) element in buffer must be incremented to add
	Ucomplex::pComplexArray CurrentBuffer;
	Ucomplex::pComplexArray VoltageBuffer;
	int NumStateVars;
	Arraydef::pDoubleArray StateBuffer;
	Ucomplex::pComplexArray FlickerBuffer; // store phase voltages in polar form
                                       // then convert to re=flicker level, update every time step
                                       //             and im=Pst, update every 10 minutes
	Arraydef::pDoubleArray SolutionBuffer;
	bool IncludeResidual;
	bool VIpolar;
	bool Ppolar;
	int FileSignature;
	int FileVersion;
	double BaseFrequency;
       /*-------------*/
	double F_Value_one;//test;
       //Voltages
	Arraydef::pDoubleArray F_Value_one_V; //Measured voltage by each FMonitor(p.u.)
	Ucomplex::pComplexArray F_Value_one_S; //Measured apparent power for each phase by each Fmonitor
	double Fvalue_P;  //This variable is used to store total measure three-phase active power of any Fmonitor
	double Fvalue_Q;  //This variable is used to store total measure three-phase reactive power of any Fmonitor
	double F_P_one;
	double F_Q_one;//measured power
	double P_ref_one; //the ref Power for this point
	int Node_num;  // Node number within the cluster
	int V_Sensor;  // Voltage sensor enable variable
	int P_Sensor;   // Power sensor enable variable
	int Cluster_num;  // the group number for this
	int Total_Clusters; //Total Number of the Groups in a circuit

       //communication time
	double T_intvl_smpl; //Sampling interval.
	int MaxLocalMem; //Max number of local memory, no large than 99
	int Smpl_stps; // T_Comm/ ActiveCircuit.Solution.Dynavars.h.
	Arraydef::pDoubleArray pCommDelayMatrix;  //
	Arraydef::pSmallIntArray pCommDelaySteps;// Communication delay step matrix of this cluster

       // define properties for equivalent generator for simulate frequency
       //eg_defed : boolean; //moved to public
               //default 0.5s to flat the initial condition
                  // determine the input of PV: u_i = k_dltP * \Delta P + omg_fm
       //delay to uppper level
	double kVA_fm;
	double M_fm;
	double D_fm;
	double Tau_fm;
	double Ki_fm;
	double Pm_fm;
	double init_time;
	double k_dltP;
	double up_dly;         //in seconds
                //nup_dlys := up_dly / t_intvl_smpl;
	int nup_dlys;
	int virtual_Ld_Nd; // denotes which node talks to upper level
                                // default by 1;

       // attack and defense
	bool d_atk_inited;
	bool z_dfs_inited;        // for attack initialization if attack is dynamic
	int atk_node_num;        //default no. 1;
                            //when the attack starts to work, default by 0.5s.
                            //defense index
                              //parameter for Kc (gradient control)
                                 //attack on gradient control: 1: no attack; -1: make the gradient control work to the oppesite
	double atk_time;
	double beta_dfs;
	double D_beta;
	double D_p;
	double dlt_z0;
	Arraydef::pSmallIntArray pCommHide; // communication matrix of this cluster
	Arraydef::pSmallIntArray pCommNode_Hide; // communication matrix of this cluster

       //
	int Bus_code;
	int NodeNum;
	int Node_Ref;
       /*------------*/
	String BufferFile;  // Name of file for catching buffer overflow
	bool IsFileOpen;
	bool ValidMonitor;
	bool IsProcessed;

       //Procedure AddDblsToBuffer(Dbl:pDoubleArray; Ndoubles:Integer);
       //Procedure AddDblToBuffer(const Dbl:Double);

       //Procedure DoFlickerCalculations;  // call from CloseMonitorStream
	void Set_nodes_for_fm(int intNodes);//initiate the structure of this FMon
	void Set_CommVector(String strParam);
	void Set_CommVector_Hide(String strParam);
	void Set_CommVector_NodeHide(String strParam);
       //
	void Set_volt_lmt_clstr(String strParam);
	void Set_CommDelayVector(String strParam);
	void ResetDelaySteps(int iNodeNum);

       //attack and defense
	void update_attack(int ActorID); // update d_i
	void update_defense(int ActorID);// update z_i
	double organise_dfs_node(int j);// update z_i

       //Function  fm_defense(i : integer): double;    // calculate K_i z
	void Set_atk_dfs(String strParam);
	void Set_EquivalentGenerator(String strParam);
       //
	void Set_ElemTable_line(String strParam);
	void Init_nodeFM(int iNodeNum, int ActorID);
       //Procedure push_voltage;
	void Get_PDElem_terminal_voltage(int nd_num_in_cluster, String devName, int Tern_num, int ActorID); //
	void Calc_Alpha_for_PDNode(int NodeNum);
	void update_all_nodes_info(int ActorID);
	double AvgPmax();
	double AvgQmax();
	void Get_PQ_DI(int i_NodeNum, int ActorID);
	double Calc_Grdt_for_Alpha(int NodeNuminClstr, int phase_num, int ActorID);
	double Calc_Grdt_for_Alpha_vivj(int NodeNuminClstr, int phase_num, int ActorID);
	double Getgradient(int j, int phase_num, double Bii, double Volt_Trhd);
	double Calc_GP_AlphaP(int phase_num, int ActorID);
	double Get_power_trans(int ActorID);
	double Coef_Phi(double X);  // a coeffient
public:
	VLNodeVars::TVLNodeVars* pNodeFMs;
	int Mode;
       //MonitorStream :TMemoryStream;
	int SampleCount;  // This is the number of samples taken
	       /*-- overview information about this cluster--*/
	LD_fm_infos::TLD_fm_infos ld_fm_info[4/*# range 0..3*/];
       // define properties for equivalent generator for simulate frequency
	bool eg_defed; //moved to public
	double dlt_fm;
	double omg_fm;
	double comp_omg; //

       // define properties for attack and defense
	bool atk; //default = false
	bool dfs; //default = false
	TFMonitorObj(DSSClass::TDSSClass* ParClass, const String MonitorName);
	virtual ~TFMonitorObj();
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model, reset nphases
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);    // Always Zero for a monitor
       //Procedure TakeSample(ActorID : Integer);         Override; // Go add a sample to the buffer
	       /**/
       //Procedure GetFvalue;
       //Function PhaseDetect(Bus_name:string):Integer;
	       /**/
	void ResetIt(int ActorID);
	void Save();     // Saves present buffer to file
       //Procedure PostProcess(ActorID : Integer); // calculates Pst or other post-processing
       //
	double Calc_sum_dij_Alphaj(int NodeNumofDG, int phase_num, int ActorID);
       //unified voltage (1-V_dg)^2
	double Calc_Alpha_M2(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID);
       //minimize loss
	double Calc_Alpha_L(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID);
	double Calc_Alpha_L_vivj(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID);
	double Calc_Alpha_LnM2(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID);
	double Calc_AlphaP(int NodeNuminClstr, int phase_num, int ActorID);
	int Get_P_mode(int ActorID);

       //Zero seq.
	double Calc_fm_ul_0(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID);
	double Calc_fm_us_0(int NodeNumofDG, int phase_num, int dbNodeRef, double Bii, double beta, double Volt_Trhd, int ActorID);

       //
	void Agnt_smpl(int NodeNumofDG, int phase_num, int ActorID); // sample data of this node at each  t_intvl_smpl
	void Init_delay_array(int NodeNumofDG, int ActorID);
       /*For real power control-dynamic simu*/
	double Calc_ul_P(int NodeNuminClstr, int phase_num);
	double Calc_Gradient_ct_P(int NodeNuminClstr, int phase_num, int ActorID);  // curtailment
	       /*--*/
	void update_node_info_each_time_step(int ActorID); //all nodes in the cluster
       //Procedure  update_ld_info( ActorID: integer); //all nodes in the cluster
	void update_ld_dly(int ActorID); // all nodes in this cluster with delay
	void Calc_P_freq_fm(int ActorID);// calculte frequency for each cluster
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present value of terminal Curr
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);   // Returns Injextion currents
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	String Get_FileName(int ActorID);

      // Property CSVFileName:String Read Get_FileName;
	TFMonitorObj(DSSClass::TDSSClass* ParClass);
	TFMonitorObj(String ClassName);
	TFMonitorObj();
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
extern TFMonitorObj* ActiveFMonitorObj;

/*--------------------------------------------------------------------------*/


}  // namespace Fmonitor

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Fmonitor;
#endif

#endif // FmonitorH





