#ifndef SolutionAlgsH
#define SolutionAlgsH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/* Solution Algorithms*/

/*
   9-20-00  Added SolveDynamic

   1/22/01 Added SolutionAbort Check wherever solution in a potentially long loop
   4/2/04  Updated SolutionAbort to work through redirect files  and long scripts
*/

namespace SolutionAlgs
{
	//#include <System.hpp>
	int SolveMonte1(int ActorID);   // Solve Monte Carlo Solution

	int SolveMonte2(int ActorID);   // Solve Monte Carlo Solution

	int SolveMonte3(int ActorID);   // Solve Monte Carlo Solution

	int SolveMonteFault(int ActorID);  // Solve Monte Carlo Fault Study

	int SolveFaultStudy(int ActorID);  // Full Fault Study

	int SolveDaily(int ActorID);    // Solve Following Daily Cycle

	int SolvePeakDay(int ActorID);   // Solve Following Daily Cycle at peak load

	int SolveYearly(int ActorID);   // Solve Following Yearly Cycle

	int SolveDuty(int ActorID);     // Solve Following Duty Cycle

	int SolveDynamic(int ActorID);  // Solve Dynamics

	int SolveEMP(int ActorID);  // Solve EMP
	
	int SolveEMPDaily(int ActorID);  // Solve EMP

	int SolveLD1(int ActorID);      // solve Load-Duration Curve, 1

	int SolveLD2(int ActorID);      // solve Load-Duration Curve, 2

	int SolveHarmonic(int ActorID);
	int SolveHarmonicT(int ActorID);  // Sequential-Time Harmonics, Added 07-06-2015

	int SolveHarmTime(int ActorID);  // solve harmonics vs time (like general time mode) created by Davis Montenegro 25/06/2014

	int SolveGeneralTime(int ActorID);
	void ComputeYsc(int iB, int ActorID);
	void ComputeAllYsc(int ActorID);
	void IntegratePCStates(int ActorID);
	void EndOfTimeStepCleanup(int ActorID);
	void FinishTimeStep(int ActorID);

	void InitializePCStates(int ActorID);
	void CalculateStateDerivatives(int ActorID);
	void StateIntegration(int ActorID);
	void StateIntegration_correction(int ActorID);
	bool GeneratorsHaveDynamicModel(int ActorID);
}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace SolutionAlgs;
#endif

#endif //  SolutionAlgsH








