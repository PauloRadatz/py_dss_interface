#ifndef ConductorH
#define ConductorH

#include "System.h"
#include "Sysutils.h"
#include <string>

/*
   ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/


//#include <System.hpp>

/*USES
    TCC_Curve;*/


//class TConductor;

namespace Conductor
{

    class TConductor {
    private:
        std::string TCCName;
        //TCC:TTCC_Curve;  // pointer to TCC curve or nil
        double AmbientTemp;
        double Accum_Isqt; // Accumulated I2t
         //   ThermalConstant:Double;  // Meaure of how fast heat can be conducted away
        void Set_Ambient(double Value);
        void Set_TCCname(const String Value);
        std::string Get_TCCName();
    public:
        bool Closed;    // change this variable to indicate open or closed switch
        bool FuseBlown;
        void CalcIsqt(double CurrentMag);  // Computes whether conductor has burned down
        void ResetIsqt();  // restore the conductor and reset the i2t calcs
        TConductor();
        virtual ~TConductor();
    };


    typedef std::vector <TConductor> pTConductorArray;
    typedef std::vector <TConductor> TConductorArray;

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Conductor;
#endif

#endif //  ConductorH









