#ifndef InvDynamicsH
#define InvDynamicsH


#include "System.h"

#include "Ucomplex.h"
#include "mathutil.h"
#include "Ucmatrix.h"


namespace InvDynamics
{

    /*Structure for hosting data and solving for each inverter based element*/
    struct TInvDynamicVars 
    {
        std::vector< polar >    Vgrid;      // Grid voltage at the point of connection per phase
        std::vector< double >   dit,        // Current's first derivative per phase
                                it,         // Current's integration per phase
                                itHistory,  // Shift register for it
                                VDelta,     // for black start operation (GFM)
                                ISPDelta,   // for moving the current target per phase in black start operation (GFM)
                                AngDelta,   // For correcting the phase angle
                                m;          // Average duty cycle per phase
        double  iMaxPPhase  = 0.0,
                Kp          = 0.0,          // PI controller gain
                CtrlTol     = 0.0,          // Control loop tolerance
                SMThreshold = 0.0,          // Voltage threshold for entering into safe mode
                RatedVDC    = 0.0,          // Rated DC voltage at the inverter's input
                LS          = 0.0,          // Series inductance, careful, it cannot be 0 in dyn mode
                RS          = 0.0,          // Series resistance (filter)
                BasekV      = 0.0,          // BAse kV depending on the number of phases
                BaseV       = 0.0,          // For GFM mode, avoid messing with things
                MaxVS       = 0.0,          // Max Voltage at the inverter terminal to safely operate the inverter
                MinVS       = 0.0,          // Min Voltage at the inverter terminal to safely operate the inverter
                MinAmps     = 0.0,          // Min amps required for exporting energy
                mKVARating  = 0.0,          // For GFM impedance calculation and avoid messing with other calcs
                RatedkVLL   = 0.0,          // As defined when decalred the element, for GFM impedance calculation and avoid messing with other calcs
                ILimit      = 0.0,          // Created for limiting the output current without entering into safe mode
                ISP         = 0.0,          // Current setpoint according to the actual DER kW
                IComp       = 0.0,          // For storing the compensation value when the Amps limiter is active
                VError      = 0.0;          // Stores the systemic error correction factor for current limiting
          
        bool    Discharging = false,        // To verify if the storage device is discharging
                ResetIBR    = false,        // flag for forcing the IBR to turn OFF
                SafeMode    = false;        // To indicate weather the Inverter has entered into safe mode
        std::vector< bool > SfModePhase;    // To identify when to restart the phase

        double Get_InvDynValue(int myindex, int NumPhases);
        std::string Get_InvDynName(int myindex);
        void Set_InvDynValue(int myindex, double myValue);
        void SolveDynamicStep(int I, int ActorID, PPICtrl PICtrl);
        void SolveModulation(int I, int ActorID, PPICtrl PICtrl);
        void FixPhaseAngle(int ActorID, int idx);
        void CalcGFMYprim(int ActorID, int Nphases, pTcMatrix YMatrix);
        void CalcGFMVoltage(int ActorID, int Nphases, pComplexArray x, double angle = 0);
        void InitDynArrays(int Nphases);
    };

    extern int NumInvDynVars;
} // namespace InvDynamics

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace InvDynamics;
#endif

#endif //  InvDynamicsH