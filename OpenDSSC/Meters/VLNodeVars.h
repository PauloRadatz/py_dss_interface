#ifndef VLNodeVarsH
#define VLNodeVarsH

#include "System.h"
#include "Sysutils.h"

#include "Ucomplex.h"

namespace VLNodeVars
{



/*
  ----------------------------------------------------------
  Copyright (c) 2017 - 2022, Electric Power Research Institute, Inc.
  Added by Ying.
  ----------------------------------------------------------

  Definition of Fmonitor (virtue leader) Public Data Record
*/
      //PointerList;

   /*Fmonitor public data/state variable structure*/

   //value ot save communication delay
   //TDelays = packed record
typedef double Tdlys[100/*# range 0..99*/]; //max 99

   //end;
#pragma pack (push, 1)

   //properties for Node
struct TVLNodeVars;
typedef TVLNodeVars* pNodeVar;

struct TVLNodeVars
{
	String vl_strBusName;
	String vl_strMeasuredName;
	int vl_terminalNum;
	int vl_ndphases;
	double vl_basevolt;
	short int vl_nodeType_phase[3]; // range 1..3, set by TFMonitorObj.Init_nodeFM :
                                  //for each phase
                                  //1, dg under it; 2, no dg there
                                  //if a dg is connected, it is 1;
	double vl_V;
	double vl_V1;
	double vl_V2;
	double vl_V3;
	int Bus_Idx;      // has to be updated before being used
                                // it is related to YMatrix
	Ucomplex::complex Yii[4/*# range 0..3*/]; //each phase  123 - ABC,  0 - pos seq
	Ucomplex::complex Yij[4/*# range 0..3*/]; //each phase  123 - ABC,  0 - pos seq

        // complex voltage
	Ucomplex::complex vl_V_c;
	Ucomplex::complex vl_V_1c;
	Ucomplex::complex vl_V_2c;
	Ucomplex::complex vl_V_3c;
   //Properties for DG
	String vl_strName_dg;
        //vl_terminalNum : integer;
	int vl_ndphases_dg;   // set by TFMonitorObj.Init_nodeFM: 1,3
	int vl_phase_num_dg; //   set by TFMonitorObj.Init_nodeFM, 123--abc 0- this node has 3-phase
                            //if vl_nodeType=1, and vl_ndphases=1,phase_num =1,2,3
        //SmallIntArray = Array[1..100] of SmallInt;
        //vl_strBusName : string;
	bool vl_CC_switch_dg;// cooperate control switch. true, cooperate control is on
	int vl_PF_flag_dg;//1, real power control is on
	int vl_QV_flag_dg;//1, volt/var control is on
	double vl_volt_thrd_dg;
        //vl_phase_select
	double vl_Alpha_dg;
	double vl_Alpha1_dg;
	double vl_Alpha2_dg;
	double vl_Alpha3_dg;
	double vl_Alpha_dgn;
	double vl_Gradient_dg;
	double vl_Gradient1_dg;
	double vl_Gradient2_dg;
	double vl_Gradient3_dg;

        // communication array for alpha and others can be improved
	Tdlys vl_smpl_dg[6/*# range 1..6*/]; //1: alpha; 2: alphaP; 3, bus voltage 0 seq. ; 4,5,6: bus voltage ABC

        //
	int vl_SmplCnt;  //sample count for this agent
	double vl_crnt_smp_time; //time for current sample at this agent
	double vl_AlphaP_dg;
	double vl_AlphaP1_dg;
	double vl_AlphaP2_dg;
	double vl_AlphaP3_dg;
	double vl_GradientP_dg;
	double vl_GradientP1_dg;
	double vl_GradientP2_dg;
	double vl_GradientP3_dg;
	double vl_Pmax_dg;
	double vl_Qmax_dg;
	double vl_Pmax_phase_dg;
	double vl_Qmax_phase_dg;
	double vl_V_base_dg;
	double vl_V_ref_dg;
	double vl_V_ref1_dg;
	double vl_V_ref2_dg;
	double vl_V_ref3_dg;// nominal value with respect to p.u. 1  //must be set by initialization
	double vl_kcq_dg; // the step size gain of agent i //has to be defined befor used
	double vl_p_DG;
	double vl_p_DG1;
	double vl_p_DG2;
	double vl_p_DG3;
	double vl_kcd_dg; // the step size gain of agent i //has to be defined befor used
	double vl_kc_ul_dg; // the cooperative gain for agent i
	double vl_q_DG;
	double vl_q_DG1;
	double vl_q_DG2;
	double vl_q_DG3;
   //Properties for Loads
	int ldType; //-1: noload; 0: one 3phase or 2phase load; 1, 2, 3: 1,2 or 3 single loads;
	int ldIdx;
	int ldIdx1;
	int ldIdx2;
	int ldIdx3;
	double vl_Q_Di; //all load reactive power except DG
	double vl_Q_Di1; //
	double vl_Q_Di2; //
	double vl_Q_Di3; //
	double vl_P_Di; //all load reactive power except DG
	double vl_P_Di1; //
	double vl_P_Di2; //
	double vl_P_Di3; //
       // vl_NodeRef : integer;// for global use

       // attack and defense
	double d_atk;
	double z_dfs;
	double z_dfsn;
	double d_atk0;
};
#pragma pack (pop)

}  // namespace VLNodeVars

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace VLNodeVars;
#endif

#endif // VLNodeVarsH





