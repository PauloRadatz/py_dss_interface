
#pragma hdrstop

#include "InvDynamics.h"
#include "DSSGlobals.h"

using namespace std;

namespace InvDynamics
{

    int NumInvDynVars = 9;

    // Returns the value of the given state variable using the index lo localize it

    double TInvDynamicVars::Get_InvDynValue(int myindex, int NumPhases)
    {
        double result = 0.0;
        switch (myindex)
        {
        case 0:
            if (Vgrid.size() > 0)
                result = Vgrid[NumPhases - 1].mag;
            else
                result = 0;
            break;
        case 1:
            if (dit.size() > 0)
                result = dit[NumPhases - 1];
            else
                result = 0;
            break;
        case 2:
            if (it.size() > 0)
                result = it[NumPhases - 1];
            else
                result = 0;
            break;
        case 3:
            if (itHistory.size() > 0)
                result = itHistory[NumPhases - 1];
            else
                result = 0;
            break;
        case 4:
            result = RatedVDC;
            break;
        case 5:
            if (m.size() > 0)
                result = m[0];
            else
                result = 0;
            break;
        case 6:
            result = ISP;
            break;
        case 7:
            result = LS;
            break;
        case 8:
            result = iMaxPPhase;
            break;
        default:
            result = 0;
        }
        return result;
    }

    // Sets the value for the state variable indicated in the index



    void TInvDynamicVars::Set_InvDynValue(int myindex, double myValue)
    {
        switch (myindex)
        {
        case 0:
            break;  // Read only
        case 1:
            dit[0] = myValue;
            break;
        case 2:
            it[0] = myValue;
            break;
        case 3:
            itHistory[0] = myValue;
            break;
        case 4:
            RatedVDC = myValue;
            break;
        case 5:
            m[0] = myValue;
            break;
        case 6:
            break; // Read only
        case 7:
            LS = myValue;
            break;
        case 8:
            iMaxPPhase = myValue;
            break;
        default:
            // Do nothing
            break;
        }
    }

    // Returns the name of the state variable located by the given index



    String TInvDynamicVars::Get_InvDynName(int myindex)
    {
        String result;
        switch (myindex)
        {
        case 0:
            result = "Grid voltage";
            break;
        case 1:
            result = "di/dt";
            break;
        case 2:
            result = "it";
            break;
        case 3:
            result = "it History";
            break;
        case 4:
            result = "Rated VDC";
            break;
        case 5:
            result = "Avg duty cycle";
            break;
        case 6:
            result = "Target (Amps)";
            break;
        case 7:
            result = "Series L";
            break;
        case 8:
            result = "Max. Amps (phase)";
            break;
        default:
            result = "Unknown variable";
        }
        return result;
    }

    // Solves the derivative term of the differential equation to be integrated



    void TInvDynamicVars::SolveDynamicStep(int I, int ActorID, PPICtrl PICtrl)
    {
        double myDCycle = 0.0, iDelta = 0.0, iErrorPct = 0.0, iError = 0.0;
        /*# with ActiveCircuit[ActorID].Solution do */
        {
            SolveModulation(I, ActorID, PICtrl);
            if (SafeMode)
                dit[I] = 0;
            else
                dit[I] = ((m[I] * RatedVDC) - (RS * it[I]) - Vgrid[I].mag) / LS;  // Solves derivative
        }
    }

    // Calculates and stores the averaged modulation factor for controlling the inverter output



    void TInvDynamicVars::SolveModulation(int I, int ActorID, PPICtrl PICtrl)
    {
        double myDCycle = 0.0, iDelta = 0.0, iErrorPct = 0.0, iError = 0.0;
        /*# with ActiveCircuit[ActorID].Solution do */
        auto with0 = ActiveCircuit[ActorID]->Solution;
        {
            if (with0->DynaVars.IterationFlag != 0)
            {                                                     // duty cycle at time h
                iError = (ISP - it[I]);                          // Only recalculated on the second iter
                iErrorPct = iError / ISP;
                if (Abs(iErrorPct) > CtrlTol)
                {
                    iDelta = PICtrl->SolvePI(iError);
                    myDCycle = m[I] + iDelta;
                    if ((Vgrid[I].mag > MinVS) || (MinVS == 0))
                    {
                        if (SafeMode || SfModePhase[I])
                        {
                            //Coming back from safe operation, need to boost duty cycle
                            m[I] = ((RS * it[I]) + Vgrid[I].mag) / RatedVDC;
                            SafeMode = false;
                            SfModePhase[I] = false;
                        }
                        else
                            if ((myDCycle <= 1) && (myDCycle > 0))
                                m[I] = myDCycle;
                    }
                    else
                    {
                        m[I] = 0;
                        it[I] = 0;
                        itHistory[I] = 0;
                        SafeMode = true;
                        SfModePhase[I] = true;
                    }
                }
            }
        }
    }
    //---------------------------------------------------------------------------------------
    //|             Calculates the current phasors to match with the target (v)             |
    //---------------------------------------------------------------------------------------


    void TInvDynamicVars::FixPhaseAngle(int ActorID, int idx)
    {
        double myError = 0.0;
        // Corrects the phase angle
        AngDelta[idx] = AngDelta[idx] + ((double((idx * TwoPi)) / -3) - Vgrid[idx].ang);
        Vgrid[idx].ang = AngDelta[idx];
    }

    //---------------------------------------------------------------------------------------
    //| Calculates the equivalent short circuit impedance for the inverter operating in GFM |
    //| Similar to the calculation user for VSource, replacing some variables with constants|
    //---------------------------------------------------------------------------------------


    void TInvDynamicVars::CalcGFMYprim(int ActorID, int Nphases, pTcMatrix YMatrix)
    {
        TcMatrix Z;
        complex Zs, Zm;
        double a = 0.0, b = 0.0, c = 0.0, R0 = 0.0, X0 = 0.0, X1 = 0.0, R1 = 0.0, R2 = 0.0, X2 = 0.0, Isc1 = 0.0, Xs = 0.0, RS = 0.0, Rm = 0.0, Xm = 0.0;
        int I = 0, j = 0;
        Z = TcMatrix(YMatrix->get_Norder());
        X1 = (double(sqr(RatedkVLL)) / mKVARating) / sqrt(1.0 + 0.0625);
        R1 = X1 / 4; // Uses defaults
        R2 = R1;     // default Z2 = Z1
        X2 = X1;
        Isc1 = (mKVARating * 1000.0 / (sqrt(3) * RatedkVLL)) / Nphases;
        //  Compute R0, X0
        a = 10;
        b = (4.0 * (R1 + X1 * 3));
        c = (4.0 * (R1 * R1 + X1 * X1) - Sqr(double((sqrt(3) * RatedkVLL * 1000.0)) / Isc1));
        R0 = QuadSolver(a, b, c);
        X0 = R0 * 3;
        // for Z matrix
        Xs = (2.0 * X1 + X0) / 3.0;
        RS = (2.0 * R1 + R0) / 3.0;
        Rm = (R0 - R1) / 3.0;
        Xm = (X0 - X1) / 3.0;
        Zs = cmplx(RS, Xs);
        Zm = cmplx(Rm, Xm);
        for (int stop = Nphases, I = 1; I <= stop; I++)
        {
            Z.SetElement(I, I, Zs);
            for (int stop = I - 1, j = 1; j <= stop; j++)
            {
                Z.SetElemsym(I, j, Zm);
            }
        }
        Z.Invert();
        YMatrix->CopyFrom(&Z);
    }
    //---------------------------------------------------------------------------------------
    //|   Calculates the voltage magnitudes and angles for facilitating GFM control mode    |
    //---------------------------------------------------------------------------------------


    void TInvDynamicVars::CalcGFMVoltage(int ActorID, int Nphases, pComplexArray x, double angle)
    {
        double  refAngle    = 0.0;
        int     I           = 0;

        refAngle        = angle;
        for (int stop = Nphases, I = 1; I <= stop; I++)
            x[I - 1] = pdegtocomplex(BaseV, (360.0 + refAngle - double(((I - 1) * 360.0)) / Nphases));
    }
    //---------------------------------------------------------------------------------------
    //|  Initializes all the local vectors using the number of phases given by the caller    |
    //---------------------------------------------------------------------------------------


    void TInvDynamicVars::InitDynArrays(int Nphases)
    {
        int I = 0;
        dit.resize(Nphases);     // Includes the current and past values
        it.resize(Nphases);
        itHistory.resize(Nphases);
        Vgrid.resize(Nphases);
        m.resize(Nphases);
        VDelta.resize(Nphases);
        ISPDelta.resize(Nphases);
        AngDelta.resize(Nphases);
        SfModePhase.resize(Nphases);
        for (int stop = (Nphases - 1), I = 0; I <= stop; I++)
        // {
            SfModePhase[I] = false;
            // it[I] = 0;
            // itHistory[I] = 0;
            // dit[I] = 0;
            // m[I] = 0;
        // }
        SafeMode = false;
    }
}// namespace InvDynamics