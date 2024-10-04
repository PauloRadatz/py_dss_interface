#ifndef ExecHelperH
#define ExecHelperH

#include "System.h"
#include "Sysutils.h"

#include "d2c_structures.h"

#include "Command.h"
#include "Arraydef.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "Monitor.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Utilities.h"
#include "Solution.h"
#include "EnergyMeter.h"
#include "generator.h"
#include "LoadShape.h"
#include "Load.h"
#include "PCElement.h"
#include "CktElement.h"
#include "Ucomplex.h"
#include "mathutil.h"
#include "Bus.h"
#include "SolutionAlgs.h"
#include "CmdForms.h"
#include "ExecCommands.h"
#include "Executive.h"
#include "Dynamics.h"
#include "Capacitor.h"
#include "Reactor.h"
#include "Line.h"
#include "LineUnits.h"
#include <math.h>
#include "CktElementClass.h"
#include "Sensor.h"
#include "ExportCIMXML.h"
#include "NamedObject.h"
#include "Pstcalc.h"
#include "PDElement.h"
#include "ReduceAlgs.h"
#include "Ucmatrix.h"
#include "XfmrCode.h"
#include "vccs.h"
#include "RegControl.h"
#include "CapControl.h"
#include "WindGen.h"
#include "GenDispatcher.h"
#include "StorageController.h"
#include "Relay.h"
#include "Recloser.h"
#include "fuse.h"
#include "SwtControl.h"
#include "UPFC.h"
#include "UPFCControl.h"
#include "ESPVLControl.h"
#include "IndMach012.h"
#include "GICsource.h"
#include "AutoTrans.h"
#include "GICLine.h"
#include "GICTransformer.h"
#include "VSConverter.h"
#include "Generic5OrderMach.h"



namespace ExecHelper
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Functions for performing DSS Exec Commands and Options*/
/*
 8-17-00  Updated Property Dump to handle wildcards
 10-23-00 Fixed EnergyMeters iteration error in DoAllocateLoadsCmd
 7/6/01  Fixed autobuslist command parsing of file
 7/19/01 Added DoMeterTotals
 8/1/01 Revised the Capacity Command return values
 9/12/02 Added Classes and UserClasses
 3/29/03 Implemented DoPlotCmd and Buscoords
 4/24/03  Implemented Keep list and other stuff related to circuit reduction
*/

/*$WARN UNIT_PLATFORM OFF*/
int DoNewCmd();
int DoEditCmd();
int DoBatchEditCmd();
int DoSelectCmd();
int DoMoreCmd();
int DoRedirect(bool IsCompile);
int DoSaveCmd();
int DoSampleCmd(int ActorID);
int DoSolveCmd();
int DoEnableCmd();
int DoDisableCmd();
int DoOpenCmd();
int DoResetCmd(int ActorID);
int DoNextCmd();
int DoFormEditCmd();
int DoClassesCmd();
int DoUserClassesCmd();
int DoHelpCmd();
int DoClearCmd();
int DoClearAllCmd();
int DoReduceCmd();
int DoInterpolateCmd();
int DoCloseCmd();
int DoResetMonitors(int ActorID);
int DoFileEditCmd();
int DoQueryCmd();
int DoResetMeters(int ActorID);
void DoAboutBox();
int DoSetVoltageBases(int ActorID);
int DoSetkVBase();
void DoLegalVoltageBases();
void DoAutoAddBusList(const String s);
void DoKeeperBusList(const String s);
void DoSetReduceStrategy(const String s);
void DoSetAllocationFactors(double X);
void DoSetCFactors(double X);
int DovoltagesCmd(bool PerUnit);
int DocurrentsCmd();
int DopowersCmd(int Total);
int DoseqvoltagesCmd();
int DoseqcurrentsCmd();
int DoseqpowersCmd();
int DolossesCmd();
int DophaselossesCmd();
int DocktlossesCmd();
int DoAllocateLoadsCmd(int ActorID);
int DoHarmonicsList(const String s);
int DoMeterTotals();
int DoCapacityCmd();
int DoZscCmd(bool Zmatrix);
int DoZsc10Cmd();
int DoZscRefresh(int ActorID);
int DoZsc012Cmd();
int DoBusCoordsCmd(bool SwapXY, int CoordType);
int DoUuidsCmd();
int DoSetLoadAndGenKVCmd();
int DoVarValuesCmd();
int DoVarNamesCmd();
int DoMakePosSeq();
int DoAlignFileCmd();
int DoTOPCmd();
int DoRotateCmd();
int DoVDiffCmd();
int DoSummaryCmd();
int DoDistributeCmd();
int DoDI_PlotCmd();
int DoCompareCasesCmd();
int DoYearlyCurvesCmd();
int DoVisualizeCmd();
int DoCloseDICmd();
int DoADOScmd();
int DoEstimateCmd();
int DoReconductorCmd();
int DoAddMarkerCmd();
int DoCvrtLoadshapesCmd();
int DoNodeDiffCmd();
int DoRephaseCmd();
int DoSetBusXYCmd();
int DoUpdateStorageCmd();
 //         FUNCTION DoUpdateStorage2Cmd:Integer;
int DoPstCalc();
int DoValVarCmd();
int DoLambdaCalcs();
int DoVarCmd();
int DoNodeListCmd();
int DoRemoveCmd();
int DoFNCSPubCmd();
void DoSetNormal(double pctNormal);
void Set_Time();
void ParseObjName(const String FullName, String& ObjName, String& PropName);
void GetObjClassAndName(String& ObjClass, String& ObjName);
int AddObject(const String ObjType, const String Name);
int EditObject(const String ObjType, const String Name);
void SetActiveCircuit(const String cktname);
int SetActiveCktElement();
int DoPropertyDump();


}  // namespace ExecHelper

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ExecHelper;
#endif

#endif // ExecHelperH




