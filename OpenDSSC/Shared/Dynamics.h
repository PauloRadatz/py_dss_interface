#ifndef DynamicsH
#define DynamicsH

#include "System.h"
#include "Sysutils.h"


namespace Dynamics
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Definitions of constants and structures for the Solution object and user-written dynamic models*/
const int NumSolutionModes = 16;

    /*Solution modes*/
const int SNAPSHOT = 0;
const int DAILYMODE = 1;
const int YEARLYMODE = 2;  // 8760 hour
const int MONTECARLO1 = 3;
const int LOADDURATION1 = 4;
const int PEAKDAY = 5;
const int DUTYCYCLE = 6;
const int DIRECT = 7;
const int MONTEFAULT = 8;  // Monte Carlo Fault Study
const int FAULTSTUDY = 9;  // Run through all buses and compute Voc and Zsc; Then ask for fault current.
const int MONTECARLO2 = 10;
const int MONTECARLO3 = 11;
const int LOADDURATION2 = 12;
const int AUTOADDFLAG = 13;
const int DYNAMICMODE = 14;
const int HARMONICMODE = 15;
const int GENERALTIME = 16;
const int HARMONICMODET = 17; // Adds the variable for the Sequential-time harmonics mode
const int EMPMODE = 18;
const int EMPDAILYMODE = 19;



   /*Variables needed for dynamics and user-written models.*/
#pragma pack (push, 1)

         /*time vars*/

struct TDynamicsRec
{
     // Time step size in sec for dynamics
     // sec from top of hour
	double h;
	double T;
	double tstart;
	double tstop;
	int IterationFlag;  /*0=New Time Step; 1= Same Time Step as last iteration*/
	int SolutionMode;   //  PEAKSNAP, DAILYMODE, YEARLYMODE, MONTECARLO, etc.  (see DSSGlobals)
	int intHour;  // time, in hours as an integer
	double dblHour;   // time, in hours as a floating point number including fractional part
};
#pragma pack (pop)



}  // namespace Dynamics

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Dynamics;
#endif

#endif // DynamicsH




