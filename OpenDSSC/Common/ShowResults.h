#ifndef ShowResultsH
#define ShowResultsH

#include "System.h"
#include "Sysutils.h"

using namespace std;

/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
   5-30-00 Added code for handling positive sequence mode
*/
namespace ShowResults
{


	//#include <System.hpp>
	void ShowVoltages(String Filenm, bool LL, int ShowOptionCode);
	void ShowCurrents(String Filenm, bool ShowResidual, int ShowOptionCode);
	void ShowPowers(String Filenm, int Opt, int ShowOptionCode);
	void ShowBusPowers(String Filenm, String BusName, int Opt, int ShowOptionCode);
	void ShowFaultStudy(String Filenm);
	void ShowElements(String Filenm, String ClassName);
	void ShowBuses(String Filenm);
	void ShowMeters(String Filenm);
	void ShowGenMeters(String Filenm);
	void ShowMeterZone(String Filenm);
	void ShowLosses(String Filenm);
	void ShowRegulatorTaps(String Filenm);
	void ShowOverloads(String Filenm);
	void ShowUnserved(String Filenm, bool UE_Only);
	void ShowVariables(String Filenm);
	void ShowIsolated(String Filenm);
	void ShowRatings(String Filenm);
	void ShowLoops(String Filenm);
	void ShowLineConstants(String Filenm, double Freq, int Units, double Rho);
	void ShowYPrim(String Filenm);
	void ShowY(String Filenm);
	void ShowTopology(String Fileroot); // summary and tree-view to separate files

	void ShowNodeCurrentSum(String Filenm);
	void ShowkVBaseMismatch(String Filenm);
	void ShowDeltaV(String Filenm);
	void ShowControlledElements(String Filenm);
	void ShowResult(String Filenm);
	void ShowEventLog(String Filenm);
	void ShowPV2PQGen(String Filenm);

} // namespace ShowResults

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ShowResults;
#endif

#endif //  ShowResultsH








