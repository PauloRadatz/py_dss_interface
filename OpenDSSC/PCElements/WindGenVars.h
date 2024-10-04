#ifndef WindGenVarsH
#define WindGenVarsH

#include "System.h"

#include "Ucomplex.h"

namespace WindGenVars
{



/*
  ----------------------------------------------------------
  Copyright (c) 2008-2024, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

  Definition of WindGen Public Data Record for passing to DLLs and other object
*/

   /*WindGen public data/state variable structure*/
#pragma pack (push, 1)

struct TWindGenVars;
typedef TWindGenVars* pTWindGenVars;

struct TWindGenVars
{
      /*Direct-Axis voltage magnitude & angle*/         /*present Shaft Power and relative Speed, rad/sec, difference from Synchronous speed, w0*/
                    /*actual speed = Speed + w0*/      /*Per unit mass constant*/      /*Mass constant actual values (Joule-sec/rad*/     /*Actual and per unit damping factors*/   /*machine Reactances, ohms*/   /*machine Reactances, per unit*/     /*Derivatives of Theta and Speed*/   /*history variables for integration*/  /*Target P and Q for power flow solution, watts, vars*/
	double	Theta,
			Pshaft,
			Speed,
			w0,
			Hmass,
			Mmass,
			D,
			Dpu,
			kVArating,
			kVWindGenBase,
			XD,
			Xdp,
			Xdpp,
			puXd,
			puXdp,
			puXdpp,
			dTheta,
			dSpeed,
			ThetaHistory,
			SpeedHistory,
			Pnominalperphase,
			Qnominalperphase;    /* All Doubles */

        /*32-bit integers*/       /*Number of phases*/   /*Total Number of conductors (wye-connected will have 4)*/
	int		NumPhases,
			NumConductors,
			Conn;			// 0 = wye; 1 = Delta
	
        /* Revisons (additions) to structure ...
          Later additions are appended to end of the structure so that
          previously compiled DLLs do not break
          */
	double	VthevMag,		/*Thevinen equivalent voltage for dynamic model*/
			VThevHarm,		/*Thevinen equivalent voltage mag reference for Harmonic model*/
			ThetaHarm,		/*Thevinen equivalent voltage angle reference for Harmonic model*/
			VTarget;		// Target voltage for WindGen with voltage control
	complex Zthev;
	double	XRdp;			// Assumed X/R for Xd'
    String  PLoss;			// Name of the XY curve describing the active power losses for the turbine
    double  ag,				// Garbox ratio
            Cp,				// Turbine performance coefficient
            Lamda,			// Tip speed ratio
            Poles,			// Number of poles of the induction generator
            pd,				// Air density
            Rad,			// Rotor radius
            VCutin,			// Cut-in speed for the wind generator
            VCutout,		// Cut-out speed for the wind generator
            Pm,				// mechanical power (steady-state)
            Ps,				// Stator active power
            Pr,				// Rotor active power
            Pg,				// Total power output
            s;				// generator pitch
};
#pragma pack (pop)



}  // namespace WindGenVars

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace WindGenVars;
#endif

#endif // WindGenVarsH




