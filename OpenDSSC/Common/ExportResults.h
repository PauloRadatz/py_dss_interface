#ifndef ExportResultsH
#define ExportResultsH

/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
   2-25-00 Created
   5-30-00 Added code for handling positive sequence mode
*/


//#include <System.hpp>

#include "EnergyMeter.h"
#include "XYcurve.h"

namespace ExportResults
{


	void ExportVoltages(String Filenm);
	void ExportSeqVoltages(String Filenm);
	void ExportCurrents(String Filenm);
	void ExportEstimation(String Filenm);
	void ExportSeqCurrents(String Filenm);
	void ExportPowers(String Filenm, int Opt);
	void ExportPbyphase(String Filenm, int Opt);
	void ExportSeqPowers(String Filenm, int Opt);
	void ExportFaultStudy(String Filenm);
	void ExportMeters(String Filenm);
	void ExportGenMeters(String Filenm);
	void ExportPVSystemMeters(String Filenm);
	//Procedure ExportPVSystem2Meters(FileNm:String);

	void ExportStorageMeters(String Filenm);
	//Procedure ExportStorage2Meters(FileNm:String);

	void ExportLoads(String Filenm);
	void ExportCapacity(String Filenm);
	void ExportOverloads(String Filenm);
	void ExportUnserved(String Filenm, bool UE_Only);
	void ExportYprim(String Filenm);
	void ExportY(String Filenm, bool TripletOpt);
	void ExportSeqZ(String Filenm);
	void ExportBusCoords(String Filenm);
	void ExportLosses(String Filenm);
	void ExportUuids(String Filenm);
	void ExportCounts(String Filenm);
	void ExportSummary(String Filenm);
	void ExportProfile(String Filenm, int PhasesToPlot);
	void ExportEventLog(String Filenm);
	void ExportVoltagesElements(String Filenm);
	void ExportGICMvar(String Filenm);
	void ExportBusReliability(String Filenm);
	void ExportBranchReliability(String Filenm);
	void ExportNodeNames(String Filenm);
	void ExportTaps(String Filenm);
	void ExportNodeOrder(String Filenm);
	void ExportElemCurrents(String Filenm);
	void ExportElemVoltages(String Filenm);
	void ExportElemPowers(String Filenm);
	void ExportResult(String Filenm);
	void ExportYNodeList(String Filenm);
	void ExportYVoltages(String Filenm);
	void ExportYCurrents(String Filenm);
	void ExportSections(String Filenm, TEnergyMeterObj pMeter);
	void ExportErrorLog(String Filenm);
	void ExportIncMatrix(String Filenm);
	void ExportIncMatrixRows(String Filenm);
	void ExportIncMatrixCols(String Filenm);
	void ExportBusLevels(String Filenm);
	void ExportLaplacian(String Filenm);
	void ExportZLL(String Filenm);
	void ExportZCC(String Filenm);
	void ExportY4(String Filenm);
	void ExportC(String Filenm);
	void ExportJacobian(String Filenm);
	void ExportdeltaF(String Filenm);
	void ExportdeltaZ(String Filenm);

}// namespace ExportResults

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ExportResults;
#endif

#endif //  ExportResultsH








