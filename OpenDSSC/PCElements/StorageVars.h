#ifndef StorageVarsH
#define StorageVarsH

#include "System.h"

#include "Ucomplex.h"

namespace StorageVars
{



/*
  ----------------------------------------------------------
  Copyright (c) 2009-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------

       Definition of Storage Public Data Record for passing to DLLs and other object
*/

/*Struct to pass basic data to user-written DLLs*/
#pragma pack (push, 1)


struct TStorageVars
{
	double kWrating;
	double kWhRating;
	double kWhStored;
	double kWhReserve;
	double ChargeEff;
	double DisChargeEff;
	double kVArating;
	double kVStorageBase;
	double kvarRequested;
	double RThev;
	double XThev;
	// Inverter Related Properties
	double FkVArating;
	double Fkvarlimit;
	double Fkvarlimitneg;
	bool P_Priority;
	bool PF_Priority;
	double FpctkWrated;
	double EffFactor;


	// Interaction with InvControl
	double Vreg;
	double Vavg;
	double VVOperation;
	double VWOperation;
	double DRCOperation;
	double VVDRCOperation;
	double WPOperation;
	double WVOperation;
        // Dynamics variables
	Ucomplex::complex Vthev;  /*Thevenin equivalent voltage (complex) for dynamic model*/
	Ucomplex::complex Zthev;
	double VThevHarm;  /*Thevenin equivalent voltage mag and angle reference for Harmonic model*/
	double ThetaHarm;  /*Thevenin equivalent voltage mag and angle reference for Harmonic model*/
	double VthevMag;    /*Thevenin equivalent voltage for dynamic model*/
	double Theta;   /*Power angle between voltage and current*/
	double w_grid;   /*Grid frequency*/
	double TotalLosses;
	double IdlingLosses;

                /*32-bit integers*/       /*Number of phases*/   /*Total Number of conductors (wye-connected will have 4)*/
	int NumPhases;
	int NumConductors;
	int Conn;   // 0 = wye; 1 = Delta
};
#pragma pack (pop)



}  // namespace StorageVars

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace StorageVars;
#endif

#endif // StorageVarsH




