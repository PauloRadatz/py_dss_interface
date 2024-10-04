#ifndef DSSGlobalsH
#define DSSGlobalsH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/


/* Change Log
 8-14-99  SolutionAbort Added

 10-12-99 AutoAdd constants added;
 4-17-00  Added IsShuntCapacitor routine, Updated constants
 10-08-02 Moved Control Panel Instantiation and show to here
 11-6-02  Removed load user DLL because it was causing a conflict
*/

/*$WARN UNIT_PLATFORM OFF*/

#include "System.h"
#include "DSSClassDefs.h"
#include "DSSObject.h"
#include "DSSClass.h"
#include "ParserDel.h"
#include "HashList.h"
#include "PointerList.h"
#include "PDElement.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "CktElement.h"
#include "Circuit.h"
#include "IniRegSave.h"
#include "Solution.h"
#include "Spectrum.h"
#include "LoadShape.h"
#include "TempShape.h"
#include "PriceShape.h"
#include "XYcurve.h"
#include "GrowthShape.h"
#include "Monitor.h"
#include "EnergyMeter.h"
#include "Sensor.h"
#include "TCC_Curve.h"
#include "Feeder.h"
#include "WireData.h"
#include "CNData.h"
#include "TSData.h"
#include "LineSpacing.h"
#include "TempShape.h"
#include "Storage.h"
#include "PVsystem.h"
#include "WindGen.h"
#include "Isource.h"
#include "InvControl.h"
#include "ExpControl.h"
#ifndef linux
#include <shellapi.h>
#define NOMINMAX
#include <windows.h>
#else
#include "windows2posix.h"
#endif
#include <iostream> 
#include <fstream>  
#include <stdlib.h>
#include <stdio.h>   
#ifndef linux
#include <urlmon.h>
#include <shlobj.h>
#endif
#include "YMatrix.h"     // by Dahei
#include "fMonitor.h"
#include "VSource.h"
#include "Executive.h"
#include "ExecOptions.h"
#include "IniRegSave.h"
#include "Isource.h"
#include "CmdForms.h"
#include "myCmdUtils.h"
#include "Sysutils.h"
#include "DynamicExp.h"
#include <regex>
#include <string>
#include <algorithm>
#include <atomic>

#define _USE_MATH_DEFINES

#include <math.h>

using namespace std;
using namespace Executive;

namespace DSSGlobals
{

//    class TProgressActor;
    typedef unsigned int unsignedint;


    const std::string CRLF = "\n"; // cross-platform

    const double PI = M_PI; //3.141592653589793238462643383279502884L;
    const double TwoPi = 2.0 * PI;
    const double RadiansToDegrees = 180.0 / PI;
    const double EPSILON = 1.0e-12;   // Default tiny floating point

    const double EPSILON2 = 1.0e-3;   // Default for Real number mismatch testing

     const int POWERFLOW = 1;  // Load model types for solution

     const int ADMITTANCE = 2;

     const std::string CHAR10(1, (char)10);

    // For YPrim matrices

     const int ALL_YPRIM = 0;
     const int SERIES = 1;
     const int SHUNT = 2;

    /*Control Modes*/
     const int CONTROLSOFF = -1;
     const int EVENTDRIVEN = 1;
     const int TIMEDRIVEN = 2;
     const int MULTIRATE = 3;
     const int CTRLSTATIC = 0;

    /*Randomization Constants*/
     const int GAUSSIAN = 1;
     const int UNIFORM = 2;
     const int LOGNORMAL = 3;

    /*Autoadd Constants*/
     const int GENADD = 1;
     const int CAPADD = 2;

    /*ERRORS*/
     const int SOLUTION_ABORT = 99;

    /*For General Sequential Time Simulations*/
     const int USEDAILY = 0;
     const int USEYEARLY = 1;
     const int USEDUTY = 2;
     const int USENONE = -1;

    /*Earth Model*/
     const int SIMPLECARSON = 1;
     const int FULLCARSON = 2;
     const int DERI = 3;

    /*Profile Plot Constants*/
     const int PROFILE3PH = 9999; // some big number > likely no. of phases

     const int PROFILEALL = 9998;
     const int PROFILEALLPRI = 9997;
     const int PROFILELLALL = 9996;
     const int PROFILELLPRI = 9995;
     const int PROFILELL = 9994;
     const int PROFILEPUKM = 9993;  // not mutually exclusive to the other choices 9999..9994

     const int PROFILE120KFT = 9992;  // not mutually exclusive to the other choices 9999..9994


    class TProgressActor      // Global actor for progress form
    {
//        typedef TThread inherited;
    public:
        TProgressActor() /*# overload */;
        virtual void Execute();
        virtual void Doterminate();
        ~TProgressActor()

            //*******************************Private components*****************************
            ;
    protected:
        std::string FMessage, Msg_Cmd;
        //*******************************Public components******************************
    public:
    };



    extern bool DLLFirstTime;
    extern TTextRec DLLDebugFile;
    extern String ProgramName;
    extern TIniRegSave DSS_Registry; // Registry   (See Executive)

       // Global variables for the OpenDSS Viewer

    extern bool DSS_Viz_installed; // OpenDSS viewer (flag to mark a local installation)

    extern String DSS_Viz_path;
    extern bool DSS_Viz_enable;

    // Global variables for OpenDSS-GIS

    extern bool DSS_GIS_installed; // OpenDSS-GIS (flag to mark a local installation)

    extern String DSS_GIS_path;
    extern bool IsDLL, NoFormsAllowed;
    extern std::vector < TDSSCircuit* > ActiveCircuit;
    extern std::vector < TDSSClass* > ActiveDSSClass;
    extern std::vector < int > LastClassReferenced;  // index of class of last thing edited

    extern std::vector < void* > ActiveDSSObject;
    extern int NumCircuits;
    extern int MaxCircuits;
    extern int MaxBusLimit; // Set in Validation

    extern int MaxAllocationIterations;
    extern TPointerList Circuits;
    extern std::vector < TPointerList > DSSObjs;
    extern std::vector < TParser* > AuxParser;  // Auxiliary parser for use by anybody for reparsing values

    //{****} DebugTrace:TextFile;

    extern bool ErrorPending;
    extern int CmdResult, ErrorNumber;
    extern String LastErrorMessage;
    extern int DefaultEarthModel;
    extern std::vector < int > ActiveEarthModel;
    extern String LastFileCompiled;
    extern bool LastCommandWasCompile;
    extern complex CALPHA;  /*120-degree shift constant*/
    extern double SQRT2;
    extern double SQRT3;
    extern double InvSQRT3;
    extern double InvSQRT3x1000;
    extern bool SolutionAbort;
    extern bool InShowResults;
    extern bool Redirect_Abort;
    extern bool In_Redirect;
    extern std::vector < bool > DIFilesAreOpen;
    extern bool AutoShowExport;
    extern bool AutoDisplayShowReport;
    extern std::vector < bool > SolutionWasAttempted;
    extern String GlobalHelpString;
    extern String GlobalPropertyValue;
    extern String GlobalResult;
    extern String LastResultFile;
    extern String VersionString;
    extern bool LogQueries;
    extern bool QueryFirstTime;
    extern String QueryLogFileName;
    extern TTextRec QueryLogFile;
    extern String DefaultEditor;     // normally, Notepad

    extern int DefaultFontSize;
    extern String DefaultFontName;
    extern TFontStyles DefaultFontStyles;
    extern String DSSFileName;     // Name of current exe or DLL

    extern String DSSDirectory;     // where the current exe resides

    extern String StartupDirectory;     // Where we started

    extern std::vector < String > DataDirectory;     // used to be DSSDataDirectory

    extern std::vector < String > OutputDirectory;     // output files go here, same as DataDirectory if writable

    extern std::vector < String > CircuitName_;     // Name of Circuit with a "_" appended

    extern std::vector < pComplexArray > ActiveYPrim; // Created to solve the problems

    extern double DefaultBaseFreq;
    extern double DaisySize;

    // Some commonly used classes   so we can find them easily

    extern std::vector < TLoadShape* > LoadShapeClass;
    extern std::vector < TTShape* > TShapeClass;
    extern std::vector < TPriceShape* > PriceShapeClass;
    extern std::vector < TXYcurve* > XYCurveClass;
    extern std::vector < TGrowthShape *> GrowthShapeClass;
    extern std::vector < TSpectrum* > SpectrumClass;
    extern std::vector < TDSSSolution* > SolutionClass;
    extern std::vector < TEnergyMeter* > EnergyMeterClass;
    extern std::vector < TDSSFMonitor* > FMonitorClass;      // By dahei UCF
    extern std::vector < TDynamicExp* > TDynamicExpClass;
       // FeederClass        :TFeeder;

    extern std::vector < TDSSMonitor* > MonitorClass;
    extern std::vector < TSensor* > SensorClass;
    extern std::vector < TTCC_Curve* > TCC_CurveClass;
    extern std::vector < TWireData* > WireDataClass;
    extern std::vector < TCNData* > CNDataClass;
    extern std::vector < TTSData* > TSDataClass;
    extern std::vector < TLineSpacing* > LineSpacingClass;
    extern std::vector < TStorage* > StorageClass;
    extern std::vector < TPVSystem* > PVSystemClass;
    extern std::vector <TWindGen*> WindGenClass;
    extern std::vector <TReactor*> ReactorClass;
    extern std::vector < TInvControl* > InvControlClass;
    extern std::vector < TExpControl* > ExpControlClass;
    extern std::vector < TVsource* > ActiveVSource;   // created on 01/14/2019 to facilitate actors to modify VSources while simulating

    extern std::vector < TStringList > EventStrings;
    extern std::vector < TStringList > SavedFileList;
    extern std::vector < TStringList > ErrorStrings;
    extern std::vector < TPointerList > DSSClassList; // pointers to the base class types

    extern std::vector < THashList > ClassNames;
    extern bool UpdateRegistry;  // update on program exit

#ifdef windows
    extern __int64 CPU_Freq;          // Used to store the CPU frequency
#else
    extern int64_t CPU_Freq;          // Used to store the CPU frequency
#endif
    extern bool EventLogDefault;
    extern int32 CPU_Cores;
    extern int NumNUMA;        // To store the number of NUMA nodes (should be the same as sockets)

    extern int32 CPU_Physical;
    extern int ActiveActor;
    extern int NumOfActors;
    extern std::vector < int > ActorCPU;
    extern std::vector < std::atomic<int> > ActorStatus;
    extern std::vector < int > ActorProgressCount;
    //extern TProgress* ActorProgress;
    extern std::vector < int > ActorPctProgress;
    extern std::vector < TSolver* > ActorHandle;

    extern string RepTermination;

    //***********************A-Diakoptics suite globals*****************************
       // To indicate if the tearing process will take place using the link branches given by the user

    extern bool AllActors, ADiakoptics, ADiak_Init, ADiak_PCInj, UseUserLinks, Parallel_enabled, ConcatenateReports, ProgressCmd, IncMat_Ordered;
    extern std::vector < TParser* > Parser;
    extern std::vector < TEvent > ActorMA_Msg;  // Array to handle the events of each actor

       // Default ports

    extern int DSSPrgPort, DSSGISPort;


    /********************************************************************************
    *    Nomenclature:                                                             *
    *                  OV_ Overloads                                               *
    *                  VR_ Voltage report                                          *
    *                  DI_ Demand interval for each meter. Moved to EnergyMeter.pas*
    *                  SDI_ System Demand interval                                 *
    *                  TDI_ DI Totals                                              *
    *                  FM_  Meter Totals                                           *
    *                  SM_  System Meter                                           *
    *                  EMT_  Energy Meter Totals                                   *
    *                  PHV_  Phase Voltage Report. Moved to EnergyMeter.pas        *
    *     These prefixes are applied to the variables of each file mapped into     *
    *     Memory using the MemoryMap_Lib                                           *
    ********************************************************************************
    */
    extern std::vector < TBytesStream* > OV_MHandle;  // a. Handle to the file in memory

    extern std::vector < TBytesStream* >  VR_MHandle;
    extern std::vector < TBytesStream* >  SDI_MHandle;
    extern std::vector < TBytesStream* >  TDI_MHandle;
    extern std::vector < TBytesStream* >  SM_MHandle;
    extern std::vector < TBytesStream* >  EMT_MHandle;
    extern std::vector < TBytesStream* >  FM_MHandle;

    //*********** Flags for appending Files*****************************************

    extern std::vector < bool > OV_Append;
    extern std::vector < bool > VR_Append;
    extern std::vector < bool > DI_Append;
    extern std::vector < bool > SDI_Append;
    extern std::vector < bool > TDI_Append;
    extern std::vector < bool > SM_Append;
    extern std::vector < bool > EMT_Append;
    extern std::vector < bool > PHV_Append;
    extern std::vector < bool > FM_Append;

    //***********************Seasonal QSTS variables********************************

    extern bool SeasonalRating;    // Tells the energy meter if the seasonal rating feature is active

    extern String SeasonSignal;     // Stores the name of the signal for selecting the rating dynamically

    extern std::vector < TExecutive* > DSSExecutive;
    extern TDSSClasses* DSSClasses;
    extern std::vector < TIsource* > IsourceClass;
    extern std::vector < TVsource* > VSourceClass;

    //************************ Progress actor Global defs***************************

    extern bool DSSProgressFrm, IsProgressON;
    extern std::vector < TProgressActor* > Progress_Actor;
    extern String DSSProgressPath;

    //************************ OpenDSS-GIS Global defs***************************

    extern bool IsGISON;
    extern String GISThickness, GISColor;
    extern pDoubleArray GISCoords;

    //************************ Line related Global defs***************************

    extern TCommandList LineTypeList;

    //********************* Globals for DirectDLL Interface ***********************

    extern vector<uint8_t>     myStrArray;
    extern vector<double>   myDblArray;
    extern vector<complex>  myCmplxArray;
    extern vector<polar>    myPolarArray;
    extern vector<int>      myIntArray;
    extern int		FPropIndex; // for DSSProperties API

    // For functions previously in DYMatrix.pas
    extern void* Yhandle;
    extern unsignedint NumNZ, NumBuses;
    extern int* YColumns;
    extern int* YRows;
    extern complex* YValues;


    void WriteStr2Array(String myStr);
    String BArray2Str(void* myPtr, int* idx);
    String Char0();

    void DoErrorMsg(const String S, const String Emsg, const String ProbCause, int ErrNum);
    void DoSimpleMsg(const String S, int ErrNum);
    void DoThreadSafeMsg(const String S, int ErrNum);
    void ClearAllCircuits();
    void SetObject(const String param);
    int SetActiveBus(const String BusName);
    void SetDataPath(const String PathName);
    void SetLastResultFile(const String Fname);
    void MakeNewCircuit(const String Name);
    void AppendGlobalResult(const String S);
    void AppendGlobalResultCRLF(String S);  // Separate by CRLF

    void ResetQueryLogFile();
    void WriteQueryLogFile(const String Prop, const String S);
    void WriteDLLDebugFile(const String S);
    void ReadDSS_Registry();
    void WriteDSS_Registry();
    bool IsDSSDLL(String Fname);
    std::string GetOutputDirectory();
    void MyReallocMem(void*& p, int newsize);
    void* MyAllocMem(unsignedint nbytes);
    void New_Actor_Slot();
    void New_Actor(int ActorID);
    void Wait4Actors(int WType);
    void DoClone();
    void Delay(int TickTime);
    void GetDefaultPorts();
#ifdef windows
    void Show_COM_Help();
#endif
    string GetLineTypes();
    void DSSGlobals_initialization();
    void DSSGlobals_finalization();

} // DSSGlobals


#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace DSSGlobals;
#endif

#endif //  DSSGlobalsH








