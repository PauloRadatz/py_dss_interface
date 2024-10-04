#ifndef UtilitiesH
#define UtilitiesH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/


/*
  12-18-2002 RCD Converted Eventlog to in-memory rather than file
*/

#include <algorithm>
//#include "Arraydef.h"
#include "Capacitor.h"
#include "CktElement.h"
#include "CmdForms.h"
#include "ControlElem.h"
#include "DSSClass.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "DSSObject.h"
#include "Dynamics.h"
#include "EnergyMeter.h"
#include "ExecCommands.h"
#include "Executive.h"
#include "ExecOptions.h"
#include "Fault.h"
#include "Feeder.h"
#include "generator.h"
#include "HashList.h"
#include <iomanip>
#include "Line.h"
#include "Load.h"
#include "LoadShape.h"
#include <math.h>
#include "ParserDel.h"
#include "PCElement.h"
#include "PDElement.h"
#include "Reactor.h"
#include "Solution.h"
#include <sstream>
#include <string>
#include "System.h"
#include "Sysutils.h"
#include "Ucomplex.h"
#include "Ucmatrix.h"

namespace Utilities
{
	enum LineStyle {
		psSolid,
		psDash,
		psDot,
		psDashDot,
		psDashDotDot,
		psClear,
		psInsideFrame		
	};

	const int
		clAqua = 0xFFFF00,
		clBlack = 0x000000,
		clBlue = 0xFF0000,
		clDkGray = 0x808080,
		clFuchsia = 0xFF00FF,
		clGray = 0x808080,
		clGreen = 0x008000,
		clLime = 0x00FF00,
		clLtGray = 0xC0C0C0,
		clMaroon = 0x000080,
		clNavy = 0x800000,
		clOlive = 0x008080,
		clPurple = 0x800080,
		clRed = 0x0000FF,
		clSilver = 0xC0C0C0,
		clTeal = 0x808000,
		clWhite = 0xFFFFFF,
		clYellow = 0x00FFFF;

	/// Map IOResult from d2c to a C++ exception; required for compatibility.
	/// Without this, we never get exceptions in case of file errors.
	void IOResultToException();

	int CompareTextShortest(const String s1, const String s2);
	void FireOffEditor(String Filenm);
	void ShowReport(bool ShowR);
	void DoDOSCmd(String CmdString);
	String StripExtension(const String S);
	String StripClassName(const String S);  // Return only element name sans class.

	String GetNodeString(const String BusName);
	String Pad(const String S, int Width);
	String Paddots(const String S, int Width);
	String PadTrunc(const String S, int Width);
	String IntArrayToString(pIntegerArray iarray, int count);
	String DblArrayToString(pDoubleArray dblarray, int count);
	String CmplxArrayToString(pComplexArray cpxarray, int count);
	String EncloseQuotes(const String S);
	void ShowMessageBeep(const String S);
	String FullName(TDSSCktElement* pElem);

	/*Parsing Utilities*/
	void ParseObjectClassandName(const String FullObjName, String& ClassName, String& ObjName);
	void ParseIntArray(pIntegerArray iarray, int& count, const String S);
	int InterpretSolveMode(const String S);
	int InterpretControlMode(const String S);
	int InterpretLoadModel(const String S);
	bool InterpretYesNo(const String S);
	int InterpretRandom(const String S);
	int InterpretAddType(const String S);
	int InterpretConnection(const String S);
	int InterpretSolveAlg(const String S);
	bool InterpretCktModel(const String S);
	void InitDblArray(int NumValues, pDoubleArray Xarray, double Value);
	void InitIntArray(int NumValues, pIntegerArray Xarray, int Value);
	int InterpretDblArray(const String S, int MaxValues, pDoubleArray ResultArray);
	int InterpretIntArray(const String S, int MaxValues, pIntegerArray ResultArray);
	void InterpretAndAllocStrArray(const String S, int& Size, pStringArray ResultArray);
	void InterpretTStringListArray(const String S, TStringList& ResultList);
	double InterpretTimeStepSize(const String S);
	int InterpretLoadShapeClass(const String S);
	int InterpretEarthModel(const String S);
	int InterpretColorName(const String S);
	complex InterpretComplex(const String S);
	String ConstructElemName(const String Param);
	int InterpretCoreType(const String str);
	String GetSolutionModeID();
	String GetSolutionModeIDName(int idx);
	String GetControlModeID();
	String GetRandomModeID();
	String GetLoadModel();
	String GetActiveLoadShapeClass();
	String GetDSSArray_Real(int n, pDoubleArray dbls);
	String GetDSSArray_Integer(int n, pIntegerArray ints);
	String GetEarthModel(int n);
	int GetOCPDeviceType(TDSSCktElement* pElem);
	String GetOCPDeviceTypeString(int icode);
	// Addition to deal with Memory mapped data

	double InterpretDblArrayMMF(unsigned char* myMap, int FileType, int Column, int Index, int DataSize);

	/*misc functions*/
	int DoExecutiveCommand(const String S);
	int GetCktElementIndex(const String FullObjName);
	bool IsShuntElement(const TDSSCktElement* elem);
	bool IslineElement(const TDSSCktElement* elem);
	bool IsTransformerElement(const TDSSCktElement* elem);
	// --- moved to ReduceAlgs 2/13/19 Function  IsStubLine(const Elem:TDSSCktElement):Boolean;

	bool CheckParallel(const TDSSCktElement* Line1, const TDSSCktElement* Line2);
	bool AllTerminalsClosed(TDSSCktElement* ThisElement);
	String Str_Real(const double Value, int NumDecimals);
	void DumpAllDSSCommands(String& Filename);
	void DumpAllocationFactors(String& Filename);
	void DumpComplexMatrix(Textfile& F, TcMatrix* AMatrix);
	double NearestBasekV(double kV);
	double PresentTimeInSec(int ActorID);
	int DoResetFaults();
	int DoResetControls();
	void DoResetKeepList();
	int GetNodeNum(int NodeRef);
	void InitStringToNull(String& S);
	complex CmulReal_im(const complex A, const double Mult);  // Multiply only imaginary part by a real
	//FUNCTION IsValidNumericField(const NumberField:TEdit):Boolean;

	double MaxdblArrayValue(int npts, pDoubleArray dbls);
	int iMaxAbsdblArrayValue(int npts, pDoubleArray dbls);
	double QuadSolver(const double A, const double b, const double c); // returns largest of two answers


	/*Save Function Helper*/

	bool WriteClassFile(TDSSClass &Dss_Class, String Filename, bool IsCktElement);
	bool WriteVsourceClassFile(TDSSClass &Dss_Class, bool IsCktElement);
	void WriteActiveDSSObject(Textfile& F, String NeworEdit);
	String CheckForBlanks(const String &S);
	bool RewriteAlignedFile(const String Filename);

	/*Event Log*/
	void ClearEventLog();
	void AppendToEventLog(const String opdev, const String Action, const int ActorID);
	void LogThisEvent(const String EventName, const int ActorID);
	void ClearErrorLog();

	/*Routines for doing common things to complex numbers*/
	void RotatePhasorDeg(complex& Phasor, const double h, const double AngleDeg);
	void RotatePhasorRad(complex& Phasor, const double h, const double AngleRad);
	void ConvertComplexArrayToPolar(const pComplexArray Buffer, int n);
	void ConvertComplexArrayToPowerandPF(const pComplexArray Buffer, int n);
	complex Residual(void* p, int Nph);
	complex ResidualPolar(void* p, int Nph);
	double PowerFactor(const complex S);
	double ConvertPFToPFRange2(const double Value);
	double ConvertPFRange2ToPF(const double Value);
	void CmulArray(pComplexArray pc, double Multiplier, int Size);  // Multiply a complex array times a double

	/*Support for going in and out of Dynamics Mode and Harmonics Mode*/

	void CalcInitialMachineStates();
	void InvalidateAllPCElements();
	bool InitializeForHarmonics(int ActorID);
	bool SavePresentVoltages();
	bool RetrieveSavedVoltages();
	double GetMaxPUVoltage();
	double GetMinPUVoltage(bool IgnoreNeutrals);
	complex GetTotalPowerFromSources(int ActorID);
	longInt GetMaxCktElementSize();
	int GetUniqueNodeNumber(const String sBusName, int StartNode);

	/*TraceBack Functions*/
	bool IsPathBetween(TPDElement* FromLine, TPDElement* ToLine);
	void TraceAndEdit(TPDElement* FromLine, TPDElement* ToLine, int NPhases, String EditStr);
	void GoForwardAndRephase(TPDElement* FromLine, const String PhaseString, const String EditStr, const String ScriptFileName, bool TransStop);
	void MakeDistributedGenerators(double kW, double PF, String How, int Skip, String Fname, bool DoGenerators);
	void Obfuscate();


	/*Feeder Utilities*/ // not currently used

	void EnableFeeders();
	void DisableFeeders();
	void InitializeFeeders();
	void ForwardSweepAllFeeders();
	void BackwardSweepAllFeeders();

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Utilities;
#endif

#endif //  UtilitiesH








