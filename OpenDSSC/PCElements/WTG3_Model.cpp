

#pragma hdrstop

#include "WTG3_Model.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Utilities.h"

namespace WTG3_Model
{

    TGE_WTG3_Model* ActiveModel = nullptr;
    TParser ModelParser;

    /* ------------------------------------------------------------------------------------------------------------- */
    /* Model Class code */
    /* ------------------------------------------------------------------------------------------------------------- */

    /* TGE_WTG3_Model */
    /* ------------------------------------------------------------------------------------------------------------- */

    TGE_WTG3_Model::TGE_WTG3_Model(TWindGenVars& GenVars, TDynamicsRec& DynaVars)
        /* ------------------------------------------------------------------------------------------------------------- */
        :
        ratedHz(0.0),
        ratedKVA(0.0),
        ratedOmg(0.0),
        ratedKVll(0.0),
        ratedVln(0.0),
        ratedAmp(0.0),
        TfltPQM(0.0),
        TfltVfbk(0.0),
        VmeasMax(0.0),
        ImeasMax(0.0),
        KpPLL(0.0),
        KiPLL(0.0),
        dOmgLim(0.0),
        VdPos(0.0),
        VqPos(0.0),
        VdNeg(0.0),
        VqNeg(0.0),
        VdFbkPos(0.0),
        VqFbkPos(0.0),
        VdFbkNeg(0.0),
        VqFbkNeg(0.0),
        IdPos(0.0),
        IqPos(0.0),
        IdNeg(0.0),
        IqNeg(0.0),
        dOmg(0.0),
        Vang(0.0),
        QordMax(0.0),
        QordMin(0.0),
        Iphl(0.0),
        Iqhl(0.0),
        ImaxTD(0.0),
        TfltIqmxvUp(0.0),
        TfltIqmxvDn(0.0),
        Iqmxv(0.0),
        Ipmx(0.0),
        Iqmx(0.0),
        Ipmn(0.0),
        Iqmn(0.0),
        V1_VoltVar(0.0),
        V2_VoltVar(0.0),
        V3_VoltVar(0.0),
        V4_VoltVar(0.0),
        Q1_VoltVar(0.0),
        Q2_VoltVar(0.0),
        Q3_VoltVar(0.0),
        Q4_VoltVar(0.0),
        Qref(0.0),
        PFref(0.0),
        rrlQcmd(0.0),
        PordMax(0.0),
        PordMin(0.0),
        Pcurtail(0.0),
        Pord(0.0),
        Pcmd(0.0),
        KpQreg(0.0),
        KiQreg(0.0),
        VrefMin(0.0),
        VrefMax(0.0),
        Qcmd(0.0),
        errQgen(0.0),
        KpVreg(0.0),
        KiVreg(0.0),
        Vref(0.0),
        errVmag(0.0),
        TfltVmagLVPL(0.0),
        TfltPplvLim0(0.0),
        V0LVPL(0.0),
        P0LVPL(0.0),
        V1LVPL(0.0),
        P1LVPL(0.0),
        MaxTrq(0.0),
        TfltPplvLimUp(0.0),
        TfltPplvLimDn(0.0),
        VmagLVPL(0.0),
        PplvLim0(0.0),
        PplvLim(0.0),
        TfltVmagLVQL(0.0),
        V0LVQL(0.0),
        I0LVQL(0.0),
        V1LVQL(0.0),
        I1LVQL(0.0),
        TfltIqlvLimUp(0.0),
        TfltIqlvLimDn(0.0),
        IqLimAsymFlt(0.0),
        VmagLVQL(0.0),
        IqlvLim(0.0),
        TfltIcmdPos(0.0),
        KpIregPos(0.0),
        KiIregPos(0.0),
        rrlIqCmd(0.0),
        KpIregNeg(0.0),
        KiIregNeg(0.0),
        AngIregNeg(0.0),
        dE2Lim(0.0),
        E2magLim(0.0),
        IdCmdPos(0.0),
        IqCmdPos(0.0),
        IdCmdNeg(0.0),
        IqCmdNeg(0.0),
        Iplv(0.0),
        Iqlv(0.0),
        errIdPos(0.0),
        errIqPos(0.0),
        errIdNeg(0.0),
        errIqNeg(0.0),
        VthrsAsymFlt(0.0),
        TthrsAsymFlt(0.0),
        AsymFltFlag(0),
        TmrAsymFlt(0.0),
        underSpeedTrip(0),
        wtgTrip(0),
        userTrip(0),
        KbAero(0.0),
        HalfRhoArAero(0.0),
        WtOpt(0.0),
        PmechMax(0.0),
        WtRefMin(0.0),
        WtRefMax(0.0),
        TfltWtRef(0.0),
        KpTrqReg(0.0),
        KiTrqReg(0.0),
        TrqRefMax(0.0),
        TrqRefMin(0.0),
        TfltPinp(0.0),
        PinpMax(0.0),
        PinpMin(0.0),
        rrlPinp(0.0),
        TfltErrPinp(0.0),
        WtRef(0.0),
        errWt(0.0),
        errWtOld(0.0),
        Pinp1(0.0),
        Pinp(0.0),
        TrqRef(0.0),
        errPinp(0.0),
        errPinpFlt(0.0),
        KpPitchCtrl(0.0),
        KiPitchCtrl(0.0),
        KpPitchComp(0.0),
        KiPitchComp(0.0),
        thetaPitchMax(0.0),
        thetaPitchMin(0.0),
        TfltPitch(0.0),
        rrlThetaPitch(0.0),
        thetaPitch(0.0),
        thetaPitch0(0.0),
        errPstl(0.0),
        Pmech(0.0),
        PmechAvl(0.0),
        TfltPavlAPC(0.0),
        TfltPsetAPC(0.0),
        TdelayAPC(0.0),
        PavlAPC(0.0),
        PsetAPC(0.0),
        PadeAPC(0.0),
        Pstl(0.0),
        dbWindInertia(0.0),
        TfltDFrqWindInertia(0.0),
        KWindInertia(0.0),
        TfltDPinpWindInertia(0.0),
        dPinpMax(0.0),
        dPinpMin(0.0),
        rruDPinp(0.0),
        rrdDPinp(0.0),
        dFrqPuTest(0.0),
        dFrqWindInertia(0.0),
        y3Lpf(0.0),
        dPinpWindInertia(0.0),
        WtBase(0.0),
        Hwtg(0.0),
        Dshaft(0.0),
        Wt(0.0),
        dWt(0.0),
        dEmax(0.0),
        dEmin(0.0),
        EdPos(0.0),
        EqPos(0.0),
        EdNeg(0.0),
        EqNeg(0.0),
        DebugTrace(0),
        tsim(0.0),
        deltSim(0.0),
        delt0(0.000050),
        delt(0.0),
        nRec(0),
        nIterLF(0),
        QMode(0),
        QFlg(0),
        APCFLG(0),
        SimMechFlg(0),
        N_WTG(0),
        Xthev(0.0),
        Rthev(0.0),
        Vmag(0.0),
        VmagMin(0.0),
        Emag(0.0),
        Eang(0.0),
        Pele(0.0),
        Qele(0.0),
        Pgen(0.0),
        Qgen(0.0),
        Vss(0.0),
        Pss(0.0),
        Qss(0.0),
        vwind(0.0),
        DynaData(nullptr),
        GenData(nullptr)
    {
        /* default parameter values */
        ratedHz = 60;
        ratedKVA = 3600;
        ratedKVll = 0.69;
        //
        N_WTG = 1;
        //
        Vss = 1;
        Pss = 1;
        Qss = 0;
        vwind = 14;
        //
        Xthev = 0.05;
        Rthev = 0.0;
        //
        SimMechFlg = 1;
        APCFLG = 0;
        QFlg = 1;
        //
        TfltPQM = 0.02;
        TfltVfbk = 0.001;
        VmeasMax = 2.0;
        ImeasMax = 2.0;
        //
        KpPLL = 60;
        KiPLL = 300;
        //
        QordMax = 0.436;
        QordMin = -0.436;
        Iphl = 1.24;
        Iqhl = 1.25;
        ImaxTD = 1.25;
        TfltIqmxvUp = 0.016;
        TfltIqmxvDn = 0.160;
        //
        PordMin = 0.0;
        PordMax = 1.12;
        Pcurtail = 1.12;
        //
        QMode = 0; // 0 -> Constant Q, 1 -> Constant PF, 2 -> Volt-Var
        // IEEE 1547-2018 CAT-B Volt-Var curve
        V1_VoltVar = 0.92;
        V2_VoltVar = 0.98;
        V3_VoltVar = 1.02;
        V4_VoltVar = 1.08;
        Q1_VoltVar = 0.44;
        Q2_VoltVar = 0.0;
        Q3_VoltVar = 0.0;
        Q4_VoltVar = -0.44;
        // Q regulator
        rrlQcmd = 0.2;
        KpQreg = 0.0;
        KiQreg = 0.2;
        VrefMax = 1.1;
        VrefMin = 0.9;
        //
        KpVreg = 0;
        KiVreg = 40;
        //
        TfltVmagLVPL = 0.002;
        TfltPplvLim0 = 0.01;
        V0LVPL = 0.4875;
        P0LVPL = 0.0;
        V1LVPL = 0.9;
        P1LVPL = 1.13625;
        MaxTrq = (ratedKVA * 1000 / 1454 / 2 / PI * 60) * 1.1931;
        TfltPplvLimUp = 0.160;
        TfltPplvLimDn = 0.016;
        //
        TfltVmagLVQL = 0.01;
        V0LVQL = 0.5;
        I0LVQL = 0.9;
        V1LVQL = 0.9;
        I1LVQL = 0.79;
        TfltIqlvLimUp = 0.016;
        TfltIqlvLimDn = 0.160;
        IqLimAsymFlt = 0.447;
        //
        TfltIcmdPos = 0.002;
        // KpIregPos := 0.9*Xthev;
        // KiIregPos := 100*KpIregPos;
        rrlIqCmd = 0.5;
        //
        // KiIregNeg := KiIregPos*0.1;
        AngIregNeg = 65 * PI / 180;
        dE2Lim = 0.05;
        E2magLim = 0.105;
        //
        dEmax = 0.1;
        dEmin = -0.1;
        //
        VthrsAsymFlt = 30 / 0.69 * 1000 * sqrt(2) / sqrt(3);
        TthrsAsymFlt = 0.03;
        //
        KbAero = 69.5;
        HalfRhoArAero = 0.00145;
        AlphaAero[0][0] = -0.41909;
        AlphaAero[0][1] = 0.21808;
        AlphaAero[0][2] = -0.012406;
        AlphaAero[0][3] = -0.00013365;
        AlphaAero[0][4] = 0.000011524;
        AlphaAero[1][0] = -0.067606;
        AlphaAero[1][1] = 0.060405;
        AlphaAero[1][2] = -0.013934;
        AlphaAero[1][3] = 0.0010683;
        AlphaAero[1][4] = -0.000023895;
        AlphaAero[2][0] = 0.015727;
        AlphaAero[2][1] = -0.010996;
        AlphaAero[2][2] = 0.0021495;
        AlphaAero[2][3] = -0.00014855;
        AlphaAero[2][4] = 2.7937E-06;
        AlphaAero[3][0] = -0.00086018;
        AlphaAero[3][1] = 0.00057051;
        AlphaAero[3][2] = -0.00010479;
        AlphaAero[3][3] = 5.9924E-06;
        AlphaAero[3][4] = -8.9194E-08;
        AlphaAero[4][0] = 0.000014787;
        AlphaAero[4][1] = -9.4839E-06;
        AlphaAero[4][2] = 1.6167E-06;
        AlphaAero[4][3] = -7.1535E-08;
        AlphaAero[4][4] = 4.9686E-10;
        //
        WtRefMin = 0;
        WtRefMax = 1.2;
        TfltWtRef = 60;
        KpTrqReg = 3;
        KiTrqReg = 0.6;
        TrqRefMax = 1.2;
        TrqRefMin = 0.08;
        TfltPinp = 0.05;
        PinpMax = 1.12;
        PinpMin = 0.04;
        rrlPinp = 0.45;
        TfltErrPinp = 1.0;
        //
        KpPitchCtrl = 150;
        KiPitchCtrl = 25;
        KpPitchComp = 3;
        KiPitchComp = 30;
        thetaPitchMax = 27;
        thetaPitchMin = 0;
        TfltPitch = 0.3;
        rrlThetaPitch = 10;
        //
        TfltPavlAPC = 0.15;
        FrqTableAPC[0] = 0.96;
        FrqTableAPC[1] = 0.996;
        FrqTableAPC[2] = 1.004;
        FrqTableAPC[3] = 1.04;
        FrqTableAPC[4] = 1.0662;
        PwrTableAPC[0] = 1.0;
        PwrTableAPC[1] = 0.95;
        PwrTableAPC[2] = 0.95;
        PwrTableAPC[3] = 0.40;
        PwrTableAPC[4] = 0.0;
        TfltPsetAPC = 5;
        TdelayAPC = 0.15;
        //
        dbWindInertia = 0.0025;
        TfltDFrqWindInertia = 1;
        KWindInertia = 10;
        TfltDPinpWindInertia = 5.5;
        dPinpMax = 0.5;
        dPinpMin = 0;
        rruDPinp = 0.1;
        rrdDPinp = 1;
        //
        // WtBase := 2*PI*(ratedHz/3);
        Hwtg = 5.23;
        Dshaft = 0.0;
        //
        DebugTrace = 0;
        //
        GenData = &GenVars; // Make pointer to data in main DSS
        DynaData = &DynaVars;
        ReCalcElementData();
    }

     TGE_WTG3_Model::~TGE_WTG3_Model()
    {
        // todo check:  inherited::Destroy();
    }
    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::Edit()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        int ParamPointer = 0;
        String ParamName = "";
        String Param = "";
        /* This DLL has a version of the DSS Parser compiled into it directly because it
          was written on the same platform as the DSS. Otherwise, one should use the Callbacks. */
        ParamPointer = 0;
        ParamName = ModelParser.GetNextParam();
        Param = ModelParser.MakeString_();
        while (Param.size() > 0)
        {
            if (ParamName.size() == 0)
            {
                if (CompareText(Param, "help") == 0)
                    ParamPointer = 23;
                else
                    ParamPointer++;
            }
            else
                ParamPointer = CommandList.Getcommand(ParamName);
            switch (ParamPointer)
            {
            case
                // 0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Object "'+Name+'"');
                1:
                Rthev = ModelParser.MakeDouble_();
                break;
            case 2:
                Xthev = ModelParser.MakeDouble_();
                break;
            case 3:
                Vss = ModelParser.MakeDouble_();
                break;
            case 4:
                Pss = ModelParser.MakeDouble_();
                break;
            case 5:
                Qss = ModelParser.MakeDouble_();
                break;
            case 6:
                vwind = ModelParser.MakeDouble_();
                break;
            case 7:
                QMode = ModelParser.MakeInteger_();
                break;
            case 8:
                SimMechFlg = ModelParser.MakeInteger_();
                break;
            case 9:
                APCFLG = ModelParser.MakeInteger_();
                break;
            case 10:
                QFlg = ModelParser.MakeInteger_();
                break;
            case 11:
                DebugTrace = ModelParser.MakeInteger_();
                break;
            case 12:
                delt0 = ModelParser.MakeDouble_();
                break;
            case 13:
                ratedKVA = ModelParser.MakeDouble_();
                break;
            case 14:
                V1_VoltVar = ModelParser.MakeDouble_();
                break;
            case 15:
                V2_VoltVar = ModelParser.MakeDouble_();
                break;
            case 16:
                V3_VoltVar = ModelParser.MakeDouble_();
                break;
            case 17:
                V4_VoltVar = ModelParser.MakeDouble_();
                break;
            case 18:
                Q1_VoltVar = ModelParser.MakeDouble_();
                break;
            case 19:
                Q2_VoltVar = ModelParser.MakeDouble_();
                break;
            case 20:
                Q3_VoltVar = ModelParser.MakeDouble_();
                break;
            case 21:
                Q4_VoltVar = ModelParser.MakeDouble_();
                break;
            case 22:
                N_WTG = ModelParser.MakeInteger_();
                break;
            case 23:
                DoHelpCmd();
                break; // whatever the option, do help
            default:
                break;
            }
            ParamName = ModelParser.GetNextParam();
            Param = ModelParser.MakeString_();
        }
        ReCalcElementData();
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::EditProp(int ParamPointer, String StrVal)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        String Param;
        switch (ParamPointer)
        {
        case
            // 0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Object "'+Name+'"');
            1:
            Rthev = StrToFloat(StrVal);
            break;
        case 2:
            Xthev = StrToFloat(StrVal);
            break;
        case 3:
            Vss = StrToFloat(StrVal);
            break;
        case 4:
            Pss = StrToFloat(StrVal);
            break;
        case 5:
            Qss = StrToFloat(StrVal);
            break;
        case 6:
            vwind = StrToFloat(StrVal);
            break;
        case 7:
            QMode = StrToInt(StrVal);
            break;
        case 8:
            SimMechFlg = StrToInt(StrVal);
            break;
        case 9:
            APCFLG = StrToInt(StrVal);
            break;
        case 10:
            QFlg = StrToInt(StrVal);
            break;
        case 11:
            DebugTrace = StrToInt(StrVal);
            break;
        case 12:
            delt0 = StrToFloat(StrVal);
            break;
        case 13:
            ratedKVA = StrToFloat(StrVal);
            break;
        case 14:
            V1_VoltVar = StrToFloat(StrVal);
            break;
        case 15:
            V2_VoltVar = StrToFloat(StrVal);
            break;
        case 16:
            V3_VoltVar = StrToFloat(StrVal);
            break;
        case 17:
            V4_VoltVar = StrToFloat(StrVal);
            break;
        case 18:
            Q1_VoltVar = StrToFloat(StrVal);
            break;
        case 19:
            Q2_VoltVar = StrToFloat(StrVal);
            break;
        case 20:
            Q3_VoltVar = StrToFloat(StrVal);
            break;
        case 21:
            Q4_VoltVar = StrToFloat(StrVal);
            break;
        case 22:
            N_WTG = StrToInt(StrVal);
            break;
        case 23:
            DoHelpCmd();
            break; // whatever the option, do help
        default:
            break;
        }
        ReCalcElementData();
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::ReCalcElementData()
    /* ------------------------------------------------------------------------------------------------------------- */
    {

        // execution order: Create(Recalc) -> CalcPFlow -> Init
        ratedOmg = 2 * PI * ratedHz;
        ratedVln = ratedKVll / sqrt(3.0) * 1000;
        ratedAmp = double(ratedKVA) * 1000 / ratedVln / 3;
        MaxTrq = (double(double(double(ratedKVA))) * 1000 / 1454 / 2 / PI * 60) * 1.1931;
        Zthev = cmplx(Rthev, Xthev);
        dOmgLim = 0.2 * ratedOmg;

        // current regulator parameters
        KpIregPos = 0.9 * Xthev;
        KiIregPos = 25. * KpIregPos;
        //
        KpIregNeg = KpIregPos * 1.5;
        KiIregNeg = KiIregPos * 1.5;

        // volt-var curve
        VCurveVoltVar[0] = max(0.0, V1_VoltVar - 0.2);
        VCurveVoltVar[1] = V1_VoltVar;
        VCurveVoltVar[2] = V2_VoltVar;
        VCurveVoltVar[3] = V3_VoltVar;
        VCurveVoltVar[4] = V4_VoltVar;
        VCurveVoltVar[5] = min(2.0, V4_VoltVar + 0.2);
        QCurveVoltVar[0] = Q1_VoltVar;
        QCurveVoltVar[1] = Q1_VoltVar;
        QCurveVoltVar[2] = Q2_VoltVar;
        QCurveVoltVar[3] = Q3_VoltVar;
        QCurveVoltVar[4] = Q4_VoltVar;
        QCurveVoltVar[5] = Q4_VoltVar;

        // turbine rotation speed base
        WtBase = 2 * PI * (ratedHz / 3);

        // 1.5MW parameters
        if (ratedKVA < 2000)
        {
            Hwtg = 4.94;
            KbAero = 56.6;
            HalfRhoArAero = 0.00159;
        }

        // time steps
        deltSim = DynaData->h;
        nRec = trunc(int(double(DynaData->h) / delt0 / 2) * 2 + 1);
        delt = double(DynaData->h) / nRec;
        tsim = DynaData->T;
        nIterLF = 100;

        // initialize trace file
        if (DebugTrace == 1)
            InitTraceFile();
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    complex TGE_WTG3_Model::MagLimiter(complex x, double magmin, double magMax)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        complex result = CZero;
        result = pclx(max(magmin, min(magMax, cabs(x))), cang(x));
        return result;
    }


/* ------------------------------------------------------------------------------------------------------------- */

    double TGE_WTG3_Model::LinearInterp(double* xTable, int xTable_maxidx, double* yTable, int yTable_maxidx, double x)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double  result = 0.0;
        int     iLeft = 0, 
                iRight = 0, 
                ii = 0;

        iLeft = 0 /*# Low(xTable) */;
        iRight = xTable_maxidx /*# High(xTable) */;
        result = yTable[iLeft];
        if (x < xTable[iLeft])
            result = yTable[iLeft];
        else
        {
            if (x > xTable[iRight])
                result = yTable[iRight];
            else
            {
                for (int stop = iRight - 1, ii = iLeft; ii <= stop; ii++)
                    if ((x >= xTable[ii]) && (x <= xTable[ii + 1]))
                    {
                        result = ((yTable[ii + 1] - yTable[ii]) / (xTable[ii + 1] - xTable[ii])) * (x - xTable[ii]) + yTable[ii];
                        break;
                    }
            }
        }
        return result;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::abc2seq(complex* abc, complex* seq, double ang)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        int ii = 0;
        complex temp;
        // phase to sequence conversion
        Phase2SymComp(abc, seq);
        // rotation of the sequence components
        temp = cmplx(cos(-ang), sin(-ang));
        for (int stop = 2, ii = 0; ii <= stop; ii++)
        {
            (seq[ii]) = cmul((seq[ii]), temp);
        }
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::seq2abc(complex* abc, complex* seq, double ang)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        int ii = 0;
        complex temp;
        // sequence to phase conversion
        SymComp2Phase(abc, seq);
        // rotation of the sequence components
        temp = cmplx(cos(ang), sin(ang));
        for (int stop = 3, ii = 1; ii <= stop; ii++)
        {
            abc[ii - 1] = cmul(abc[ii - 1], temp);
        }
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::Instrumentation(pComplexArray V, pComplexArray i)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        int ii = 0;
        double ktemp = 0.0;
        // per-unitize abc voltage and current
        for (int stop = 3, ii = 1; ii <= stop; ii++)
        {
            Vabc[ii - 1] = MagLimiter(cdivreal(V[ii - 1], ratedVln), 0, VmeasMax);
            // Iabc = -(I/AmpBase+Vabc/Zthev)
            Iabc[ii - 1] = MagLimiter(csub(cdivreal(i[ii - 1], -ratedAmp * N_WTG), cdiv(Vabc[ii - 1], Zthev)), 0, ImeasMax);
        }

        // phase to sequence conversion
        abc2seq(&(Vabc[0]), &(V012[0]), Vang);
        abc2seq(&(Iabc[0]), &(I012[0]), Vang);

        // get rid of zero sequence component in voltage
        V012[0] = cmplx(0, 0);
        seq2abc(&(Vabc[0]), &(V012[0]), Vang);

        // voltage magnitude
        Vmag = cabs(V012[1]);

        // minimum voltage for fault ride through
        VmagMin = min(min(cabs(Vabc[1 - 1]), cabs(Vabc[2 - 1])), cabs(Vabc[3 - 1]));

        // calculate output power
        Sele = cmplx(0, 0);
        for (int stop = 3, ii = 1; ii <= stop; ii++)
            Sele = cadd(Sele, cdivreal(cmul(Vabc[ii - 1], conjg(Iabc[ii - 1])), 3));
        Pele = Sele.re;
        Qele = Sele.im;
        ktemp = min(1.0, deltSim / TfltPQM);
        Pgen = Pgen + (Pele - Pgen) * ktemp;
        Qgen = Qgen + (Qele - Qgen) * ktemp;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::CalcPFlow(pComplexArray V, pComplexArray i)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        complex Vtemp = CZero, 
                Itemp = CZero, 
                Etemp = CZero;
        double kCnvg = 0.0;
        // instrumentation
        Instrumentation(V, i);

        // solve Emag and Eang
        if (nIterLF == 1)
        {
            Vtemp = cmulreal(cdivreal(V012[1], max(0.000001, cabs(V012[1]))), Vss);
            Emag = cabs(Vtemp);
            Eang = cang(Vtemp);
        }
        else
            Vtemp = V012[1];
        Itemp = conjg(cdiv(cmplx(Pss, Qss), Vtemp));
        Etemp = cadd(Vtemp, cmul(Zthev, Itemp));
        if (nIterLF < 10)
        {
            kCnvg = max(0.4, min(1.0, 1 - (nIterLF - 1) * 0.1));
            Emag = max(0.0, min(2.0, Emag + (cabs(Etemp) - Emag) * kCnvg));
            Eang = Eang + (cang(Etemp) - Eang) * kCnvg;
        }
        nIterLF = nIterLF + 1;

        // update output
        E012[0] = cmplx(0, 0);
        E012[1] = cmplx(Emag * cos(Eang), Emag * sin(Eang));
        E012[2] = cmplx(0, 0);
        CalcCurrent(i);
    }

    
/* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::Init(pComplexArray V, pComplexArray i)
    /* ------------------------------------------------------------------------------------------------------------- */
    // Init for Dynamics mode
    // execution order: Create(Recalc) -> CalcPFlow -> Init

    {
        int ii = 0;
        double eIter = 0.0;
        double kIter = 0.0;
        // check for available wind power and update initial power condition
        AeroMPPT();
        if (Pss > PmechMax)
        // not enough wind power to support Pss, update Pss
        {
            Pss = PmechMax;
            Wt = WtOpt;
        }
        else
            Wt = CalcWtRef(Pss);
        // run iteration to solve for thetaPitch
        thetaPitch = thetaPitchMax / 2;
        eIter = 0.01;
        kIter = 10;
        for (int stop = 10, ii = 1; ii <= stop; ii++)
        {
            AeroDynamic();
            if (abs(Pmech - Pss) < eIter)
                break;
            else
            {
                thetaPitch = thetaPitch + kIter * (Pmech - Pss);
                thetaPitch = min(thetaPitchMax, max(thetaPitchMin, thetaPitch));
            }
        }

        // run a load flow
        nIterLF = 1;
        CalcPFlow(V, i);

        // initialize control variables
        VdFbkPos = Vss;
        VqFbkPos = 0;
        VdFbkNeg = 0;
        VqFbkNeg = 0;
        dOmg = 0;
        Vang = cang(Vabc[1 - 1]);
        VqPos = 0;
        PplvLim0 = P1LVPL;
        PplvLim = PplvLim0;
        IqlvLim = I0LVQL;
        VmagLVPL = VdFbkPos;
        VmagLVQL = VdFbkPos;
        Pord = Pss;
        Pgen = Pss;
        IdCmdPos = Pss / Vss;
        Iplv = IdCmdPos;
        if ((QMode == 0) || (QMode == 1))
            Qcmd = Qss;
        else
            Qcmd = LinearInterp(VCurveVoltVar, MAXIDX(VCurveVoltVar), QCurveVoltVar, MAXIDX(QCurveVoltVar), Vss);
        PFref = abs(Pss) / max(0.000001, sqrt(Pss * Pss + Qss * Qss));
        if (Qss < 0)
            PFref = -PFref;
        Qgen = Qcmd;
        errQgen = 0;
        Vref = Vss;
        IqCmdPos = -Qcmd / Vss;
        Iqlv = IqCmdPos;
        Iqmxv = QordMax / Vss;
        errVmag = 0;
        errIdPos = 0;
        errIqPos = 0;
        EdPos = Emag * cos(Eang);
        EqPos = Emag * sin(Eang);
        // negative sequence current regulator
        errIdNeg = 0;
        errIqNeg = 0;
        EdNeg = 0;
        EqNeg = 0;
        // fault detection
        underSpeedTrip = 0;
        wtgTrip = 0;
        AsymFltFlag = 0;
        TmrAsymFlt = 0;
        // torque regulator
        WtRef = Wt;
        errWt = 0;
        Pinp1 = Pss;
        Pinp = Pss;
        TrqRef = Pss / Wt;
        errPinp = 0;
        errPinpFlt = 0;
        // pitch control
        errPstl = 0;
        thetaPitch0 = thetaPitch;
        // active power control
        // Pcurtail := Pss;
        PavlAPC = Pss;
        PsetAPC = Pss;
        PadeAPC = Pss;
        Pstl = Pss;
        // wind inertia
        dFrqPuTest = 0;
        dFrqWindInertia = 0;
        y3Lpf = 0;
        dPinpWindInertia = 0;
        // swing model
        dWt = Wt - 1;

        // initialize integrator
        for (int stop = /*# High( intg_x ) */ 11, ii = /*# Low( intg_x ) */ 0; ii <= stop; ii++)
        {
            intg_x[ii] = 0;
            intg_d[ii] = 0;
            intg_d_old[ii] = 0;
        }
        intg_x[0] = dOmg;
        intg_x[1] = Vang;
        intg_x[2] = 0;
        intg_x[3] = 0;
        intg_x[4] = 0;
        intg_x[5] = 0;
        intg_x[6] = IqCmdPos;
        intg_x[7] = Vref;
        intg_x[8] = TrqRef;
        intg_x[9] = thetaPitch;
        intg_x[10] = 0;
        intg_x[11] = dWt;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::PllLogic()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double VqPosOld = 0.0;
        double kFltTemp = 0.0;
        double drvTemp = 0.0;
        // PI regulator (Vq to dFrq)
        VqPosOld = VqPos;
        VdPos = V012[1].re;
        VqPos = V012[1].im;
        drvTemp = KiPLL * VqPos + KpPLL * (VqPos - VqPosOld) / deltSim;
        dOmg = max(-dOmgLim, min(dOmgLim, dOmg + drvTemp * deltSim));
        // integrator (dFrq to Vang)
        Vang = Vang + dOmg * deltSim;
        // other sequence components
        VdNeg = V012[2].re;
        VqNeg = -V012[2].im;
        IdPos = I012[1].re;
        IqPos = I012[1].im;
        IdNeg = I012[2].re;
        IqNeg = -I012[2].im;
        // LPF on voltage feedback
        kFltTemp = min(1.0, double(DynaData->h) / TfltVfbk);
        VdFbkPos = VdFbkPos + (VdPos - VdFbkPos) * kFltTemp;
        VqFbkPos = VqFbkPos + (VqPos - VqFbkPos) * kFltTemp;
        VdFbkNeg = VdFbkNeg + (VdNeg - VdFbkNeg) * kFltTemp;
        VqFbkNeg = VqFbkNeg + (VqNeg - VqFbkNeg) * kFltTemp;
    }

/* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::PQPriority(int PQFlag)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double  y0 = 0.0,
                temp = 0.0;

        y0 = min(Iqhl, max(QordMax, (QordMax - 2.15) * Vmag + 2.15));
        if (y0 > Iqmxv)
            temp = min(1.0, delt / TfltIqmxvUp);
        else
            temp = min(1.0, delt / TfltIqmxvDn);
        Iqmxv = Iqmxv + (y0 - Iqmxv) * temp;
        Iqmxv = min(Iqhl, max(0.0, Iqmxv));
        if (PQFlag == 1)
        {
            // P priority
            Ipmx = min(ImaxTD, Iphl);
            Iqmx = min(Iqhl, sqrt(max(0.0, sqr(ImaxTD) - sqr(Iplv))));
        }
        else
        {
            Iqmx = min(ImaxTD, Iqmxv);
            Ipmx = min(Iphl, sqrt(max(0.0, sqr(ImaxTD) - sqr(Iqlv))));
        }
        Ipmn = -Ipmx;
        Iqmn = -Iqmx;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::LVPL()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double  temp = 0.0;
        double  y0 = 0.0, 
                y0Lim = 0.0, 
                y1 = 0.0, 
                y1Sub = 0.0, 
                y2 = 0.0;

        // low pass filter on Vmag
        temp = min(1.0, delt / TfltVmagLVPL);
        VmagLVPL = VmagLVPL + (VmagMin - VmagLVPL) * temp;
        // LVPL curve interpolation
        y0 = (P1LVPL - P0LVPL) / (V1LVPL - V0LVPL) * (VmagLVPL - V0LVPL) + P0LVPL;
        y0 = max(P0LVPL, min(P1LVPL, y0));
        y0 = y0 + 0.02;
        // upper limit to y0
        if (AsymFltFlag == 1)
            y0Lim = 0.75;
        else
            y0Lim = max(0.5, min(2.0, Pord));
        y1 = min(y0Lim, y0);
        // subtraction term to y1
        if ((Vmag < (0.91 - 0.05)) && (Vmag > (0.65 - 0.05)))
            y1Sub = 0.2;
        else
            y1Sub = 0.0;
        y1 = max(0.0, y1 - y1Sub);
        // limit on torque
        y2 = min(MaxTrq * Wt * WtBase / ratedKVA / 1000, y1);
        // low pass filter
        if (y2 > PplvLim)
            temp = min(1.0, delt / TfltPplvLimUp);
        else
            temp = min(1.0, delt / TfltPplvLimDn);
        PplvLim = PplvLim + (y2 - PplvLim) * temp;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::LVQL()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double temp = 0.0;
        double y0 = 0.0;
        // low pass filter on Vmag
        temp = min(1.0, delt / TfltVmagLVQL);
        VmagLVQL = VmagLVQL + (VmagMin - VmagLVQL) * temp;
        // LVQL curve interpolation
        y0 = (I1LVQL - I0LVQL) / (V1LVQL - V0LVQL) * (VmagLVQL - V0LVQL) + I0LVQL;
        y0 = min(I0LVQL, max(I1LVQL, y0));
        // low pass filter
        if (y0 > IqlvLim)
            temp = min(1.0, delt / TfltIqlvLimUp);
        else
            temp = min(1.0, delt / TfltIqlvLimDn);
        IqlvLim = IqlvLim + (y0 - IqlvLim) * temp;
        if (AsymFltFlag == 1)
            IqlvLim = IqLimAsymFlt;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::RealPowerReg()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        // active power regulator
        Pcmd = max(PordMin, min(PordMax, min(PplvLim, Pord)));
        Pcmd = min(Pcurtail, Pcmd);
        IdCmdPos = Pcmd / max(0.000001, Vmag);
        IdCmdPos = max(Ipmn, min(Ipmx, IdCmdPos));
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::ReactivePowerReg()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double Qord = 0.0, errQgenOld = 0.0;
        double temp = 0.0;
        // calculation of desired Q output
        if (QMode == 0)
            // constant Q mode
            Qord = Qref;
        else if (QMode == 1)
        // constant PF mode (negative means absorption)
        {
            Qord = sqrt(1 - PFref * PFref) / max(0.000001, abs(PFref)) * Pgen;
            if (PFref < 0)
                Qord = -Qord;
        }
        else
            // volt-var mode
            Qord = LinearInterp(VCurveVoltVar, MAXIDX(VCurveVoltVar), QCurveVoltVar, MAXIDX(QCurveVoltVar), Vmag);
        // hard limiter on Qord
        Qord = min(QordMax, max(QordMin, Qord));
        // ramp rate limiter on Qcmd
        temp = rrlQcmd * delt;
        Qcmd = min(Qcmd + temp, max(Qcmd - temp, Qord));

        if (Qcmd <= -0.0078)
            Qcmd = Qcmd;

        // reactive power regulator
        errQgenOld = errQgen;
        errQgen = Qcmd - Qgen;
        intg_d[7] = KiQreg * errQgen + KpQreg * (errQgen - errQgenOld) / delt;
        intg_x[7] = max(VrefMin, min(VrefMax, intg_x[7]));
        Vref = intg_x[7];
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::VoltageReg()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double errVmagOld = 0.0;
        errVmagOld = errVmag;
        errVmag = -(Vref - Vmag);
        intg_d[6] = KiVreg * errVmag + KpVreg * (errVmag - errVmagOld) / delt;
        intg_x[6] = max(Iqmn, min(Iqmx, intg_x[6]));
        IqCmdPos = intg_x[6];
    }


/* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::CurrentReg()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double  ktemp = 0.0, 
                errIdPosOld = 0.0, 
                errIqPosOld = 0.0,
                errIdNegOld = 0.0, 
                errIqNegOld = 0.0,
                dE2Real = 0.0, 
                dE2Imag = 0.0;
        complex tempE2 = CZero;

        // positive sequence current regulator
        ktemp = min(1.0, delt / TfltIcmdPos);
        Iplv = Iplv + (IdCmdPos - Iplv) * ktemp;
        Iqlv = min(IqlvLim, max(-IqlvLim, Iqlv + (IqCmdPos - Iqlv) * ktemp));
        // anti windup
        if (((Iqlv == IqlvLim) && (IqCmdPos > Iqlv)) || ((Iqlv == -IqlvLim) && (IqCmdPos < Iqlv)))
        {
            intg_d[6] = 0;
            intg_d[7] = 0;
        }

        // PI regulator for IdPos
        errIdPosOld = errIdPos;
        errIdPos = Iplv - IdPos;
        intg_d[2] = KiIregPos * errIdPos + KpIregPos * (errIdPos - errIdPosOld) / delt;
        intg_x[2] = max(dEmin, min(dEmax, intg_x[2]));
        EdPos = intg_x[2] + Rthev * Iplv - Xthev * Iqlv + VdFbkPos;
        // PI regulator for IqPos
        errIqPosOld = errIqPos;
        errIqPos = Iqlv - IqPos;
        intg_d[3] = KiIregPos * errIqPos + KpIregPos * (errIqPos - errIqPosOld) / delt;
        intg_x[3] = max(dEmin, min(dEmax, intg_x[3]));
        EqPos = intg_x[3] + Rthev * Iqlv + Xthev * Iplv + VqFbkPos;

        // negative sequence current regulator
        // be carefule with signs: E2=Ed-jEq, I2=Id-jIq
        IdCmdNeg = 0;
        IqCmdNeg = 0;
        // PI regulator
        errIdNegOld = errIdNeg;
        errIdNeg = IdCmdNeg - IdNeg;
        intg_d[4] = KiIregNeg * errIdNeg + KpIregNeg * (errIdNeg - errIdNegOld) / delt;
        errIqNegOld = errIqNeg;
        errIqNeg = IqCmdNeg - IqNeg;
        intg_d[5] = KiIregNeg * errIqNeg + KpIregNeg * (errIqNeg - errIqNegOld) / delt;
        // limiter on integrator
        intg_x[4] = min(dE2Lim, max(-dE2Lim, intg_x[4]));
        intg_x[5] = min(dE2Lim, max(-dE2Lim, intg_x[5]));
        // angle rotation (for better damping)
        dE2Real = intg_x[4] * cos(AngIregNeg) + intg_x[5] * sin(AngIregNeg);
        dE2Imag = intg_x[4] * sin(AngIregNeg) - intg_x[5] * cos(AngIregNeg);
        // V2 feedforwarding and E2 magnitude limiting
        tempE2 = cmplx(VdFbkNeg + dE2Real, -VqFbkNeg + dE2Imag);
        tempE2 = MagLimiter(tempE2, 0, E2magLim);
        EdNeg = tempE2.re;
        EqNeg = -tempE2.im;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::Integrate()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        int ii = 0;
        for (int stop = 11, ii = 0; ii <= stop; ii++)
        {
            intg_x[ii] = intg_x[ii] + delt / 2 * (intg_d_old[ii] + intg_d[ii]);
            intg_d_old[ii] = intg_d[ii];
        }
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::CurrentLimiting()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double I2Max = 0.0;
        E012[0] = cmplx(0, 0);
        // current limit on positive sequence
        E012[1] = cmplx(EdPos, EqPos);
        I012[1] = cdiv(csub(E012[1], V012[1]), Zthev);
        if (cabs(I012[1]) > ImaxTD)
        {
            I012[1] = cmulreal(cdivreal(I012[1], max(0.000001, cabs(I012[1]))), ImaxTD);
            E012[1] = cadd(V012[1], cmul(I012[1], Zthev));
        }
        // current limit on negative sequence
        E012[2] = cmplx(EdNeg, -EqNeg);
        I012[2] = cdiv(csub(E012[2], V012[2]), Zthev);
        I2Max = max(0.0, 1.1 - cabs(I012[1]));
        if (cabs(I012[2]) > I2Max)
        {
            I012[2] = cmulreal(cdivreal(I012[2], max(0.000001, cabs(I012[2]))), I2Max);
            E012[2] = cadd(V012[2], cmul(I012[2], Zthev));
        }
    }


/* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::FaultDetection()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        if (cabs(V012[2]) > VthrsAsymFlt)
            TmrAsymFlt = TmrAsymFlt + deltSim;
        else
        {
            TmrAsymFlt = 0;
            AsymFltFlag = 0;
        }
        if (TmrAsymFlt > TthrsAsymFlt)
            AsymFltFlag = 1;

        // under speed fault
        if (Wt < 0.1)
            underSpeedTrip = 1;

        // tripping of WTG (more tripping logics to be added in the future)
        if ((userTrip == 1) || (underSpeedTrip == 1))
            wtgTrip = 1;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    double TGE_WTG3_Model::CalcCp(double Theta, double lmbda)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double result = 0.0;
        int ii = 0, jj = 0;
        result = 0;
        for (int stop = 4, ii = 0; ii <= stop; ii++)
            for (int stop = 4, jj = 0; jj <= stop; jj++)
                result = result + AlphaAero[ii][jj] * pow(Theta, ii) * pow(lmbda, jj);
        return result;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    double TGE_WTG3_Model::CalcPmech(double Theta, double wrotor, double spdwind)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double result = 0.0;
        double lmbda = 0.0, Cp = 0.0;
        lmbda = min(20.0, max(0.0, wrotor / max(0.01, spdwind) * KbAero));
        Cp = CalcCp(Theta, lmbda);
        result = min(1.2, HalfRhoArAero * pow(spdwind, 3) * Cp);
        return result;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::AeroMPPT()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double WtList[101 /*# range 0..100*/], PmechList[101 /*# range 0..100*/];
        double tempWt = 0.0, stepWt = 0.0;
        int ii = 0, max_ii = 0;
        stepWt = (WtRefMax - WtRefMin) / 100;
        for (int stop = 100, ii = 0; ii <= stop; ii++)
        {
            tempWt = WtRefMin + ii * stepWt;
            WtList[ii] = tempWt;
            PmechList[ii] = CalcPmech(0, tempWt, vwind);
        }
        // find optimal Wt and maximum Pmech
        PmechMax = -100000;
        max_ii = 0;
        for (int stop = 100, ii = 0; ii <= stop; ii++)
            if (PmechList[ii] > PmechMax)
            {
                max_ii = ii;
                PmechMax = PmechList[ii];
            }
        WtOpt = WtList[max_ii];
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::AeroDynamic()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        Pmech = CalcPmech(thetaPitch, Wt, vwind);
        PmechAvl = CalcPmech(0.001, Wt, vwind);
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    double TGE_WTG3_Model::CalcWtRef(double elePwr)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double result = 0.0;
        double temp = 0.0;
        temp = min(1.0, elePwr);
        result = max(WtRefMin, min(WtRefMax, -0.75 * temp * temp + 1.59 * temp + 0.63));
        return result;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::TorqueReg()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double  y1 = 0.0, 
                y2 = 0.0, 
                temp = 0.0,
                PinpSat = 0.0, 
                errPinpHpf = 0.0;

        y1 = CalcWtRef(Pele);
        // low pass filter
        temp = min(1.0, delt / TfltWtRef);
        WtRef = WtRef + (y1 - WtRef) * temp;
        // PI regulator
        errWtOld = errWt;
        errWt = Wt - WtRef;
        intg_d[8] = KiTrqReg * errWt + KpTrqReg * (errWt - errWtOld) / delt;
        intg_x[8] = max(TrqRefMin, min(TrqRefMax, intg_x[8]));
        TrqRef = intg_x[8];
        // convert torque to power
        y2 = TrqRef * Wt;
        // low pass filter on Pinp
        temp = min(1.0, delt / TfltPinp);
        Pinp1 = min(PinpMax, max(PinpMin, Pinp1 + (y2 - Pinp1) * temp));
        // ramp rate limiter
        temp = rrlPinp * delt;
        Pinp = min(Pinp + temp, max(Pinp - temp, Pinp1));
        // power response rate limit
        PinpSat = min(Pstl, max(0.0, Pinp));
        errPinp = Pinp - PinpSat;
        // high pass filter on errPinp
        temp = min(1.0, delt / TfltErrPinp);
        errPinpFlt = errPinpFlt + (errPinp - errPinpFlt) * temp;
        errPinpHpf = errPinp - errPinpFlt;
        // get the final Pord
        Pord = PinpSat + errPinpHpf + dPinpWindInertia;
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::PitchControl()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double x1 = 0.0, y1 = 0.0, y2 = 0.0, errPstlOld = 0.0, temp = 0.0;
        // Note: this function should go after torque regulator where errWt and errWtOld are calculated
        // PI regulator for pitch control
        intg_d[9] = KiPitchCtrl * errWt + KpPitchCtrl * (errWt - errWtOld) / delt;
        // PI regulator for pitch compensator
        errPstlOld = errPstl;
        errPstl = Pinp - Pstl;
        intg_d[10] = KiPitchComp * errPstl + KpPitchComp * (errPstl - errPstlOld) / delt;
        // anti-windup
        x1 = intg_d[9] + intg_d[10];
        y1 = intg_x[9] + intg_x[10];
        if (((y1 >= thetaPitchMax) && (x1 > 0)) || ((y1 <= thetaPitchMin) && (x1 < 0)))
        {
            intg_d[9] = 0;
            intg_d[10] = 0;
        }
        y2 = max(thetaPitchMin, min(thetaPitchMax, y1));
        // low pass filter
        temp = min(1.0, delt / TfltPitch);
        thetaPitch0 = thetaPitch0 + (y2 - thetaPitch0) * temp;
        // ramp rate limiter
        temp = rrlThetaPitch * delt;
        thetaPitch = min(thetaPitch + temp, max(thetaPitch - temp, thetaPitch0));
    }



/* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::APCLogic()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double  y1 = 0.0, 
                y2 = 0.0, 
                y3 = 0.0, 
                y4 = 0.0, 
                temp = 0.0, 
                gridFrq = 0.0, 
                PmechMax = 0.0, 
                PmechMin = 0.0;

        y1 = min(1.0, max(0.000001, PmechAvl));
        // low pass filter on available power
        temp = min(1.0, delt / TfltPavlAPC);
        PavlAPC = PavlAPC + (y1 - PavlAPC) * temp;
        // power curtailment
        temp = max(0.4, min(1.0, Pcurtail / PavlAPC));
        PwrTableAPC[1] = temp;
        PwrTableAPC[2] = temp;
        // power frequency curve
        gridFrq = 1 + dOmg / ratedOmg;
        y2 = LinearInterp(FrqTableAPC, MAXIDX(FrqTableAPC), PwrTableAPC, MAXIDX(PwrTableAPC), gridFrq);
        y3 = PavlAPC * y2;
        // low pass filter on set power
        temp = min(1.0, delt / TfltPsetAPC);
        PsetAPC = PsetAPC + (y3 - PsetAPC) * temp;
        // APCFLG
        if (APCFLG == 0)
            y4 = Pcurtail;
        else
            y4 = PsetAPC;
        // enforce user-defined PmechMax in normal condition
        if ((gridFrq >= FrqTableAPC[1]) && (gridFrq <= FrqTableAPC[2]))
            PmechMax = 1.0;
        else
            PmechMax = 1.2;
        PmechMin = 0.2;
        y4 = min(PmechMax, max(PmechMin, y4));
        // Pade delay function
        temp = min(1.0, delt / TdelayAPC * 2);
        PadeAPC = PadeAPC + (y4 - PadeAPC) * temp;
        Pstl = min(PmechMax, max(PmechMin, 2 * PadeAPC - y4));
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::WindInertia()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double  y1 = 0.0, 
                y2 = 0.0, 
                y3 = 0.0, 
                y4 = 0.0, 
                temp = 0.0, 
                temp1 = 0.0, 
                temp2 = 0.0;

        y1 = -dOmg / ratedOmg + dFrqPuTest;
        // deadband
        y2 = max(0.0, y1 - dbWindInertia);
        // low pass filter
        temp = min(1.0, delt / TfltDFrqWindInertia);
        dFrqWindInertia = dFrqWindInertia + (y2 - dFrqWindInertia) * temp;
        // multiplier
        y3 = dFrqWindInertia * KWindInertia;
        // high pass filter
        temp = min(1.0, delt / TfltDPinpWindInertia);
        y3Lpf = y3Lpf + (y3 - y3Lpf) * temp;
        y4 = min(dPinpMax, max(dPinpMin, y3 - y3Lpf));
        // ramp rate limiter
        temp1 = rruDPinp * delt;
        temp2 = rrdDPinp * delt;
        dPinpWindInertia = min(dPinpWindInertia + temp1, max(dPinpWindInertia - temp2, y4));
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::SwingModel()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double  Tmech = 0.0, 
                Tele = 0.0, 
                Tdamp = 0.0;

        Tmech = Pmech / Wt;
        Tele = Pele / Wt;
        Tdamp = Dshaft * dWt;
        intg_d[11] = double((Tmech - Tele - Tdamp)) / 2 / Hwtg;
        dWt = intg_x[11];
        Wt = max(0.01, 1.0 + dWt);
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::CalcCurrent(pComplexArray i)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        int ii = 0;
        // sequence to phase
        seq2abc(&(Eabc[0]), &(E012[0]), Vang);

        // Thevenin to Norton (current injection)
        for (int stop = 3, ii = 1; ii <= stop; ii++)
            i[ii - 1] = cmulreal(cdiv(Eabc[ii - 1], Zthev), -ratedAmp * N_WTG);
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::CalcDynamic(pComplexArray V, pComplexArray i)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        int ii = 0;
        deltSim = DynaData->h;

        // instrumentation
        Instrumentation(V, i);
        // PLL
        PllLogic();
        // fault detection
        FaultDetection();

        // start small time step iteration on when time proceeds
        if ((DynaData->T > tsim) && (DynaData->IterationFlag == 1))
        {
            nRec = trunc(int(double(DynaData->h) / delt0 / 2) * 2 + 1);
            delt = double(DynaData->h) / nRec;
            tsim = DynaData->T;

            if (wtgTrip == 0)
            {
                for (int stop = nRec, ii = 1; ii <= stop; ii++)
                {
                    // PQ Priority
                    PQPriority(0);

                    // real power regulation
                    RealPowerReg();

                    // reactive power and voltage regulation
                    if (QFlg == 0)
                    {
                        IqCmdPos = Qcmd / max(0.000001, Vmag);
                        IqCmdPos = max(Iqmn, min(Iqmx, IqCmdPos));
                    }
                    else
                    {
                        ReactivePowerReg();
                        VoltageReg();
                    }

                    // Current regulator
                    LVPL();
                    LVQL();
                    CurrentReg();
                    if (SimMechFlg > 0)
                    {
                        AeroDynamic();
                        TorqueReg();
                        PitchControl();
                        APCLogic();
                        WindInertia();
                        SwingModel();
                    }

                    // perform integration
                    Integrate();

                    // current limiting logic
                    CurrentLimiting();
                }
            }
            else
            {
                E012[0] = V012[0];
                E012[1] = V012[1];
                E012[2] = V012[2];
            }
            CalcCurrent(i);
            if (DebugTrace == 1)
                WriteTraceRecord();
        }
    }


/* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::DoHelpCmd()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        String HelpStr;
        AnsiString AnsiHelpStr;
        String CRLF;
        CRLF = "\x0d\x0a";
        HelpStr = String("Rthev= per unit Thevenin equivalent R.") + CRLF;
        HelpStr = HelpStr + "Xthev= per unit Thevenin equivalent X." + CRLF;
        HelpStr = HelpStr + "Vss= steady state voltage magnitude." + CRLF;
        HelpStr = HelpStr + "Pss= steady state output real power." + CRLF;
        HelpStr = HelpStr + "Qss= steady state output reactive power." + CRLF;
        HelpStr = HelpStr + "vwind= wind speed in m/s" + CRLF;
        HelpStr = HelpStr + "QMOde= Q control mode (0:Q, 1:PF, 2:VV)" + CRLF;
        HelpStr = HelpStr + "SimMechFlg= 1 to simulate mechanical system" + CRLF;
        HelpStr = HelpStr + "APCFlg= 1 to enable active power control" + CRLF;
        HelpStr = HelpStr + "QFlg= 1 to enable reactive power and voltage control" + CRLF;
        HelpStr = HelpStr + "DebugTrace= 1 to save dynamic simulation result in csv file" + CRLF;
        HelpStr = HelpStr + "delt0= user defined internal simulation step" + CRLF;
        HelpStr = HelpStr + "ratedKVA= WTG power rating (either 3600 or 1500)" + CRLF;
        HelpStr = HelpStr + "V#_VoltVar= V points on Volt-Var curve" + CRLF;
        HelpStr = HelpStr + "Q#_VoltVar= Q points on Volt-Var curve" + CRLF;
        HelpStr = HelpStr + "N_WTG= number of WTG in aggregation" + CRLF;
        HelpStr = HelpStr + "Help: this help message.";
        AnsiHelpStr = ((AnsiString)HelpStr); // Implicit typecast
    }

    /* ------------------------------------------------------------------------------------------------------------- */
    double TGE_WTG3_Model::Get_Variable(int i)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        double result = 0.0;
        result = -9999;
        switch (i)
        {
        case 1:
            result = userTrip;
            break;
        case 2:
            result = wtgTrip;
            break;
        case 3:
            result = Pcurtail;
            break;
        case 4:
            result = Pcmd;
            break;
        case 5:
            result = Pgen;
            break;
        case 6:
            result = Qcmd;
            break;
        case 7:
            result = Qgen;
            break;
        case 8:
            result = Vref;
            break;
        case 9:
            result = Vmag;
            break;
        case 10:
            result = vwind;
            break;
        case 11:
            result = WtRef;
            break;
        case 12:
            result = Wt;
            break;
        case 13:
            result = dOmg;
            break;
        case 14:
            result = dFrqPuTest;
            break;
        case 15:
            result = QMode;
            break;
        case 16:
            result = Qref;
            break;
        case 17:
            result = PFref;
            break;
        case 18:
            result = thetaPitch;
            break;
        default:
            break;
        }
        return result;
    }

    /* ------------------------------------------------------------------------------------------------------------- */
    void TGE_WTG3_Model::Set_Variable(int i, const double Value)
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        switch (i)
        {
        case 1:
            userTrip = round(Value);
            break;
        case 3:
            Pcurtail = Value;
            break;
        case 10:
            vwind = Value;
            break;
        case 14:
            dFrqPuTest = Value;
            break;
        case 15:
            QMode = round(Value);
            break;
        case 16:
            Qref = Value;
            break;
        case 17:
            PFref = Value;
            break;
        default:
            break;
            /* Do Nothing for other variables: they are read only */
        }
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::InitTraceFile()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        String headerStr;
        AssignFile(TraceFile, "GE_WTG3_Trace.CSV");
        Rewrite(TraceFile);
        headerStr = String("Time,Iteration,delt,nRec,ratedVln,ratedAmp,") + "vwind,thetaPitch,WtRef,Wt,Pmech,Pcmd,Pele,Pgen,Qcmd,Qele,Qgen," + "Vref,Vmag,VdPos,VqPos,VdNeg,VqNeg,IdPos,IqPos,IdNeg,IqNeg,dOmg," + "debug1,debug2,debug3,debug4,debug5,debug6,debug7,debug8,debug9,debut10";
        Write(TraceFile, headerStr);
        WriteLn(TraceFile);
        CloseFile(TraceFile);
    }

    /* ------------------------------------------------------------------------------------------------------------- */

    void TGE_WTG3_Model::WriteTraceRecord()
    /* ------------------------------------------------------------------------------------------------------------- */
    {
        // AssignFile(TraceFile, 'GE_WTG3_Trace.CSV');
        Append(TraceFile);
        Write(TraceFile, DynaData->T);
        Write(TraceFile, ',');
        Write(TraceFile, DynaData->IterationFlag);
        Write(TraceFile, ',');
        Write(TraceFile, delt);
        Write(TraceFile, ',');
        Write(TraceFile, nRec);
        Write(TraceFile, ',');
        Write(TraceFile, ratedVln);
        Write(TraceFile, ',');
        Write(TraceFile, ratedAmp);
        Write(TraceFile, ',');
        Write(TraceFile, vwind);
        Write(TraceFile, ',');
        Write(TraceFile, thetaPitch);
        Write(TraceFile, ',');
        Write(TraceFile, WtRef);
        Write(TraceFile, ',');
        Write(TraceFile, Wt);
        Write(TraceFile, ',');
        Write(TraceFile, Pmech);
        Write(TraceFile, ',');
        Write(TraceFile, Pcmd);
        Write(TraceFile, ',');
        Write(TraceFile, Pele);
        Write(TraceFile, ',');
        Write(TraceFile, Pgen);
        Write(TraceFile, ',');
        Write(TraceFile, Qcmd);
        Write(TraceFile, ',');
        Write(TraceFile, Qele);
        Write(TraceFile, ',');
        Write(TraceFile, Qgen);
        Write(TraceFile, ',');
        Write(TraceFile, Vref);
        Write(TraceFile, ',');
        Write(TraceFile, Vmag);
        Write(TraceFile, ',');
        Write(TraceFile, VdPos);
        Write(TraceFile, ',');
        Write(TraceFile, VqPos);
        Write(TraceFile, ',');
        Write(TraceFile, VdNeg);
        Write(TraceFile, ',');
        Write(TraceFile, VqNeg);
        Write(TraceFile, ',');
        Write(TraceFile, IdPos);
        Write(TraceFile, ',');
        Write(TraceFile, IqPos);
        Write(TraceFile, ',');
        Write(TraceFile, IdNeg);
        Write(TraceFile, ',');
        Write(TraceFile, IqNeg);
        Write(TraceFile, ',');
        Write(TraceFile, dOmg);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[1 - 1]);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[2 - 1]);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[3 - 1]);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[4 - 1]);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[5 - 1]);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[6 - 1]);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[7 - 1]);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[8 - 1]);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[9 - 1]);
        Write(TraceFile, ',');
        Write(TraceFile, debugVar[10 - 1]);
        WriteLn(TraceFile);
        CloseFile(TraceFile);
    }

}
