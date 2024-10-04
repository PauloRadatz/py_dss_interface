#ifndef WTG3_ModelH
#define WTG3_ModelH

#include "System.h"

#include "Ucomplex.h"
#include "Dynamics.h"
#include "math.h"
#include "Ucomplex.h"
#include "mathutil.h"
#include "ParserDel.h"
#include "Command.h"
#include "WindGenVars.h"
#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LoadShape.h"
#include "GrowthShape.h"
#include "Spectrum.h"
#include "Arraydef.h"
#include "Dynamics.h"
#include "d2c_structures.h"
#include "ControlElem.h"
#include "mathutil.h"


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2024, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  Definition of WindGen Public Data Record for passing to DLLs and other object
*/

/*WindGen public data/state variable structure*/


namespace WTG3_Model
{
    struct TWTG3_Model;
    typedef TWTG3_Model* pTWTG3_Model;

    typedef complex TSymCompArray[3];
    typedef complex TPhArray[4];
    typedef TDynamicsRec* pTDynamicsRec;
    typedef TWindGenVars* pTWindGenVars; 

    const int NumProperties = 23;   // motor model parameters

    const int NumVariables = 18;    // runtime variables


    class TGE_WTG3_Model : public PCElement::TPCElement
    {
        typedef TObject inherited;

    public:
    //private:
        // ratings
        double ratedHz, ratedKVA, ratedOmg, ratedKVll, ratedVln, ratedAmp;

        // filter time constant
        double TfltPQM;
        double TfltVfbk;
        double VmeasMax, ImeasMax;
        // PLL
        double KpPLL, KiPLL, dOmgLim;
        double VdPos, VqPos, VdNeg, VqNeg;
        double VdFbkPos, VqFbkPos, VdFbkNeg, VqFbkNeg;
        double IdPos, IqPos, IdNeg, IqNeg;
        double dOmg, Vang;
        // PQ priority control
        double QordMax, QordMin, Iphl, Iqhl, ImaxTD;
        double TfltIqmxvUp, TfltIqmxvDn;
        double Iqmxv, Ipmx, Iqmx, Ipmn, Iqmn;
        // Active and reactive power regulator
        double V1_VoltVar, V2_VoltVar, V3_VoltVar, V4_VoltVar;
        double Q1_VoltVar, Q2_VoltVar, Q3_VoltVar, Q4_VoltVar;
        double VCurveVoltVar[6 /*# range 0..5*/], QCurveVoltVar[6 /*# range 0..5*/];
        double Qref, PFref, rrlQcmd;
        double PordMax, PordMin, Pcurtail, Pord, Pcmd;
        double KpQreg, KiQreg, VrefMin, VrefMax;
        double Qcmd, errQgen;
        // Voltage regulator
        double KpVreg, KiVreg;
        double Vref, errVmag;
        // LVPL logic
        double TfltVmagLVPL, TfltPplvLim0;
        double V0LVPL, P0LVPL, V1LVPL, P1LVPL, MaxTrq;
        double TfltPplvLimUp, TfltPplvLimDn;
        double VmagLVPL, PplvLim0, PplvLim;
        // LVQL logic
        double TfltVmagLVQL;
        double V0LVQL, I0LVQL, V1LVQL, I1LVQL;
        double TfltIqlvLimUp, TfltIqlvLimDn;
        double IqLimAsymFlt;
        double VmagLVQL, IqlvLim;
        // Ireg
        double TfltIcmdPos, KpIregPos, KiIregPos, rrlIqCmd;
        double KpIregNeg, KiIregNeg, AngIregNeg, dE2Lim, E2magLim;
        double IdCmdPos, IqCmdPos, IdCmdNeg, IqCmdNeg;
        double Iplv, Iqlv;
        double errIdPos, errIqPos, errIdNeg, errIqNeg;
        // fault detection
        double VthrsAsymFlt, TthrsAsymFlt;
        int AsymFltFlag;
        double TmrAsymFlt;
        int underSpeedTrip, wtgTrip;
        int userTrip;

        // Aerodynamic model
        double KbAero, HalfRhoArAero;
        double AlphaAero[5 /*# range 0..4*/][5 /*# range 0..4*/];
        double WtOpt, PmechMax;
        // torque regulator
        double WtRefMin, WtRefMax;
        double TfltWtRef, KpTrqReg, KiTrqReg, TrqRefMax, TrqRefMin;
        double TfltPinp, PinpMax, PinpMin, rrlPinp, TfltErrPinp;
        double WtRef, errWt, errWtOld, Pinp1, Pinp, TrqRef, errPinp, errPinpFlt;
        // pitch control
        double KpPitchCtrl, KiPitchCtrl, KpPitchComp, KiPitchComp;
        double thetaPitchMax, thetaPitchMin, TfltPitch, rrlThetaPitch;
        double thetaPitch, thetaPitch0, errPstl, Pmech, PmechAvl;
        double TfltPavlAPC;
        double FrqTableAPC[5 /*# range 0..4*/], PwrTableAPC[5 /*# range 0..4*/];
        double TfltPsetAPC, TdelayAPC;
        double PavlAPC, PsetAPC, PadeAPC, Pstl;
        // wind inertia
        double dbWindInertia, TfltDFrqWindInertia, KWindInertia, TfltDPinpWindInertia;
        double dPinpMax, dPinpMin, rruDPinp, rrdDPinp;
        double dFrqPuTest, dFrqWindInertia, y3Lpf, dPinpWindInertia;
        // swing model
        double WtBase, Hwtg, Dshaft;
        double Wt, dWt;
        // regulator output
        double dEmax, dEmin;
        double EdPos, EqPos, EdNeg, EqNeg;
        // integrator
        double intg_x[12 /*# range 0..11*/], intg_d[12 /*# range 0..11*/], intg_d_old[12 /*# range 0..11*/];
        int DebugTrace;
        TTextRec TraceFile;
        double debugVar[10 /*# range 1..10*/];
        double Get_Variable(int i);
        void  Set_Variable(int i, const double Value);
        void  abc2seq(complex* abc, complex* seq, double ang);
        void  seq2abc(complex* abc, complex* seq, double ang);
        complex MagLimiter(complex x, double magmin, double magMax);
        double LinearInterp(double* xTable, int xTable_maxidx, double* yTable, int yTable_maxidx, double x);
        double CalcCp(double Theta, double lmbda);
        double CalcPmech(double Theta, double wrotor, double spdwind);
        double CalcWtRef(double elePwr);
        void   Instrumentation(pComplexArray V, pComplexArray i);
        void   PllLogic();
        void   PQPriority(int PQFlag);
        void   LVPL();
        void   LVQL();
        void   RealPowerReg();
        void   ReactivePowerReg();
        void   VoltageReg();
        void   CurrentReg();
        void   CurrentLimiting();
        void   FaultDetection();
        void   AeroMPPT();
        void   AeroDynamic();
        void   TorqueReg();
        void   PitchControl();
        void   APCLogic();
        void   WindInertia();
        void   SwingModel();
        void   CalcCurrent(pComplexArray i);
        void   DoHelpCmd();
        void   InitTraceFile();
        void   WriteTraceRecord();

    //protected:
    public:
        // simulation time setup
        double tsim, deltSim, delt0, delt;
        int nRec, nIterLF;
        int QMode, QFlg;
        // active power control
        int APCFLG;
        // simulate mechanical system
        int SimMechFlg;
        // number of WTG
        int N_WTG;
        // terminal impedance
        double Xthev, Rthev;
        complex Zthev;
        // terminal voltage and current
        TPhArray Vabc, Iabc, Eabc;
        TSymCompArray V012, I012, E012;
        double Vmag, VmagMin;
        double Emag, Eang;
        complex Sele;
        double Pele, Qele;
        double Pgen, Qgen;
        // steady state conditions for initialization
        double Vss, Pss, Qss;
        // wind speed
        double vwind;
        pTDynamicsRec DynaData;
        pTWindGenVars GenData;
        void  Init(pComplexArray V, pComplexArray i);
        void  Edit(); // Uses ModelParser
        void  EditProp(int ParamPointer, String StrVal);
        void  Integrate();
        void  CalcDynamic(pComplexArray V, pComplexArray i);
        void  CalcPFlow(pComplexArray V, pComplexArray i);
        void  ReCalcElementData();
        TGE_WTG3_Model(TWindGenVars& GenVars, TDynamicsRec& DynaVars);
        virtual ~TGE_WTG3_Model();
    };

    extern TGE_WTG3_Model* ActiveModel;
} 

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace WTG3_Model;
#endif

#endif // WTG3_ModelH