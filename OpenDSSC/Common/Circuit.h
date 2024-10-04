#ifndef CircuitH
#define CircuitH
/*
   ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
 Change Log
   10-12-99 Added DuplicatesAllowed and ZonesLocked
   10-24-99 Added Losses Property
   12-15-99 Added Default load shapes and generator dispatch reference
   4-17=00  Add Loads List
   5-30-00  Added Positive Sequence Flag
   8-24-00  Added PriceCurve stuff   Updated 3-6-11
   8-1-01  Modified Compute Capacity to report up to loadmult=1
   9-25-15  Fixed broken repository
*/

/*$WARN UNIT_PLATFORM OFF*/


#include "System.h"
#include "Sysutils.h"

#include "Solution.h"
#include "Arraydef.h"
#include "HashList.h"
#include "PointerList.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "Bus.h"
#include "LoadShape.h"
#include "PriceShape.h"
#include "Ucomplex.h"
#include "PointerList.h"
#include "AutoAdd.h"
#include "EnergyMeter.h"
#include "NamedObject.h"
#include "CktTree.h"
#include "Monitor.h"
#include "PCClass.h"
#include "PDClass.h"
#include "MeTIS_Exec.h"
#include "ControlQueue.h"
#include <math.h>
#include <regex>
#include "Sparse_Math.h"
#include "d2c_structures.h"
#include <iostream>
#include <fstream>
#include <string>
#include <cstring>




namespace Circuit
{

    enum TReductionStrategy {
        rsDefault,
        rsShortlines,
        rsMergeParallel,
        rsBreakLoop,
        rsDangling,
        rsSwitches,
        rsLaterals
    };

    struct CktElementDef {
        int CktElementClass;
        int devHandle;
    };

    typedef CktElementDef CktElementDefArray[1/*# range 1..1*/];
    typedef std::vector<CktElementDef> pCktElementDefArray;



    // for adding markers to Plot


    class TBusMarker : public TObject
        // Must be defined before calling circuit plot
    {
        typedef TObject inherited;
        friend class TDSSCircuit;
    private:
    public:
        String BusName;
        int AddMarkerColor, AddMarkerCode, AddMarkerSize;
        TBusMarker();
        virtual ~TBusMarker();
    };


    class TDSSCircuit : public TNamedObject {
        typedef TNamedObject inherited;
        friend class TBusMarker;
    public:
        std::vector <longInt> NodeBuffer;
        int NodeBufferMax;
        bool FBusNameRedefined;
        TDSSCktElement* FActiveCktElement;
        String FCaseName;

        // Temp arrays for when the bus swap takes place
        pTBusArray SavedBuses;
        pStringArray SavedBusNames;
        int SavedNumBuses;
        double FLoadMultiplier;  // global multiplier for every load
        bool AbortBusProcess;
        TCktTree* Branch_List; // topology from the first source, lazy evaluation
        TAdjArray BusAdjPC;
        TAdjArray BusAdjPD; // bus adjacency lists of PD and PC elements
        void AddDeviceHandle(int Handle);
        void AddABus();
        void AddANodeBus();
        int AddBus(const String BusName, int NNodes, int ActorID);
        void Set_ActiveCktElement(TDSSCktElement* Value);
        void Set_BusNameRedefined(bool Value);
        complex Get_Losses(int ActorID); //Total Circuit losses
        void Set_LoadMultiplier(double Value);
        void SaveBusInfo();
        void RestoreBusInfo();
        bool SaveMasterFile();
        bool SaveDSSObjects();
        bool SaveFeeders();
        bool SaveBusCoords();
        bool SaveGISCoords();
        bool SaveVoltageBases();
        void ReallocDeviceList(int ActorID);
        void Set_CaseName(const String Value);
        String Get_Name();
    public:
        int ActiveBusIndex;
        double Fundamental;    // fundamental and default base frequency
        bool Control_BusNameRedefined;  // Flag for use by control elements to detect redefinition of buses
        THashList BusList, AutoAddBusList, DeviceList;
        pCktElementDefArray DeviceRef;  //Type and handle of device
        bool LongLineCorrection;
                // lists of pointers to different elements by class

                //by dahei

                //Generic5OrderMach,
                //

                // Storage2Elements,

                // PVSystems2,
       // added for CIM XML export
        TPointerList Faults;
        TPointerList PDElements;
        TPointerList PCElements;
        TPointerList DSSControls;
        TPointerList Sources;
        TPointerList MeterElements;
        TPointerList Sensors;
        TPointerList Monitors;
        TPointerList FMonitors;
        TPointerList EnergyMeters;
        TPointerList Generators;
        TPointerList WindGens;
        TPointerList StorageElements;
        TPointerList PVSystems;
        TPointerList Substations;
        TPointerList Transformers; 
        TPointerList CapControls;
        TPointerList RegControls;
        TPointerList Lines;
        TPointerList Loads;
        TPointerList ShuntCapacitors;
        TPointerList AutoTransformers;
        TPointerList Feeders;
        TPointerList Reactors;
        TPointerList Relays;
        TPointerList Fuses;
        TPointerList Reclosers;
        TPointerList SwtControls;
        TPointerList InvControls2;
        TPointerList ExpControls;
        TPointerList CktElements;

        // BHSL
        TPointerList Gencls;
        TPointerList Genrou;


        TPointerList ExcSexs;
        TPointerList Tgov;

        ControlQueue::TControlQueue ControlQueue;
        TSolutionObj* Solution;
        TAutoAdd AutoAddObj;

        // For AutoAdd stuff
        double UEWeight, LossWeight;
        int NumUEregs, NumLossRegs;
        pIntegerArray UEregs, LossRegs;
        double CapacityStart, CapacityIncrement;   // flag for trapezoidal integratio
        bool TrapezoidalIntegration, LogEvents;
        String LoadDurCurve;
        TLoadShapeObj* LoadDurCurveObj;
        String PriceCurve;
        TPriceShapeObj* PriceCurveObj;
        int NumDevices, NumBuses, NumNodes;
        int MaxDevices, MaxBuses, MaxNodes;
        int IncDevices, IncBuses, IncNodes;

        // Variables for the tearing Algorithm
                   // Used for the user to stablish the coverage for the algorithm
        double Coverage, Actual_Coverage;   // Indicates the actual coverage of the circuit after running the tearing algorithm
        std::vector < int > Longest_paths;   //Stores the coordinates of the longest paths in the circuit
        std::vector < int > Path_Idx;   //Stores the indexes from where the areas where formed on the linearized graph
        std::vector < int > Buses_Covered;   //Stores the number of buses (estimated - 1 quadrant) per path
        std::vector < int > Path_Size;   //Stores the estimated size of each path
        std::vector < int > New_Graph;   //Stores the latest weighted graph
        int Num_SubCkts;            // Stores the number of subcircuits for tearing the circuit when executing the "tear_Circuit" command
        std::vector < String > Link_Branches;    // Stores the names of the Link branches for Diakoptics
        std::vector < String > PConn_Names;    // Stores the names of the buses (bus1) of the link branches
        std::vector < double > PConn_Voltages;    // Stores the voltages at the point of connection of the subcircuits
        std::vector < int > Locations;   // Stores the indexes of the locations
        std::vector < String > BusZones;

        // Variables for Diakoptics
        TSparse_Complex ContoursT;    //  Contours matrix transposed
        TSparse_Complex Contours;    //  Contours matrix
        TSparse_Complex ZLL;    //  Link branch matrix
        TSparse_Complex ZCT;    //  The transformation matrix (to go from one to other domain)
        TSparse_Complex ZCC;    //  Interconnections matrix
        TSparse_Complex Y4;    //  The inverse of the interconnections matrix
        TSparse_Complex V_0;    //  The voltages of the partial solutions
        TSparse_Complex Ic;    //  The complementary Currents vector
        int VIndex;  // To store the index of the sub-circuit in the interconnected system
        int VLength;  // To store the length of the sub-circuit in the interconnected system
        bool AD_Init;    // This is used only by the A-Diakoptics coordiantor (ID = 1)

                // Bus and Node stuff
        pTBusArray Buses;
        pTNodeBusArray MapNodeToBus;

        // Flags
        bool Issolved;
        bool DuplicatesAllowed;
        bool ZonesLocked;
        bool MeterZonesComputed;
        bool PositiveSequence;  // Model is to be interpreted as Pos seq
        bool NeglectLoadY;

        // Voltage limits
        double NormalMinVolts, NormalMaxVolts, EmergMaxVolts, EmergMinVolts;  //per unit voltage restraints for this circuit
        std::vector <double> LegalVoltageBases;

        // Global circuit multipliers
 // global multiplier for every generator
        double GeneratorDispatchReference, DefaultGrowthFactor, DefaultGrowthRate, GenMultiplier, HarmMult;
        complex DefaultHourMult;
        double PriceSignal; // price signal for entire circuit

                // EnergyMeter Totals
        TRegisterArray RegisterTotals;
        TLoadShapeObj* DefaultDailyShapeObj;
        TLoadShapeObj* DefaultYearlyShapeObj;
        String CurrentDirectory;
        TReductionStrategy ReductionStrategy;
        /*ReductionMaxAngle,*/
        double ReductionZmag;
        bool ReduceLateralsKeepLoad;
        String ReductionStrategyString;
        double PctNormalFactor;

        /*------Plot Marker Circuit Globals---------*/
        int NodeMarkerCode;
        int NodeMarkerWidth;
        int SwitchMarkerCode;
        int TransMarkerSize;
        int CapMarkerSize;
        int RegMarkerSize;
        int PVMarkerSize;
        int StoreMarkerSize;
        int FuseMarkerSize;
        int RecloserMarkerSize;
        int RelayMarkerSize;
        int TransMarkerCode;
        int CapMarkerCode;
        int RegMarkerCode;
        int PVMarkerCode;
        int StoreMarkerCode;
        int FuseMarkerCode;
        int RecloserMarkerCode;
        int RelayMarkerCode;
        bool MarkSwitches;
        bool MarkTransformers;
        bool MarkCapacitors;
        bool MarkRegulators;
        bool MarkPVSystems;
        // MarkPVSystems2   :Boolean;
        bool MarkStorage;
        // MarkStorage2     :Boolean;
        bool MarkFuses;
        bool MarkReclosers;
        bool MarkRelays;
        int NumCircuits;
        TList BusMarkerList;  // list of buses to mark

                /*---------------------------------*/
        int ActiveLoadShapeClass;
        TStringList MeTISZones;                      // The list for assigning a zone to a bus after tearing
        TDSSCircuit(const String aName);
        virtual ~TDSSCircuit();
        void AddCktElement(int Handle);  // Adds last DSS object created to circuit
        void ClearBusMarkers();
        void TotalizeMeters();
        bool ComputeCapacity(int ActorID);
        bool Save(String Dir);
        void ProcessBusDefs(int ActorID);
        void ReProcessBusDefs(int ActorID);
        void DoResetMeterZones(int ActorID);
        int SetElementActive(const String FullObjectName);
        void InvalidateAllPCElements();
        void DebugDump(Textfile& F);

        // Access to topology from the first source
        TCktTree& GetTopology();
        void FreeTopology();
        TAdjArray GetBusAdjacentPDLists(int ActorID);
        TAdjArray GetBusAdjacentPCLists(int ActorID);
        int Tear_Circuit();                  // Tears the circuit considering the number of Buses of the original Circuit
        String Create_MeTIS_graph();                    // Generates the graph dscribing the model for MeTiS
        String Create_MeTIS_Zones(String Filename); // Executes MeTiS and loads the zones into memory for further use
        void AggregateProfiles(String mode);
        void Disable_All_DER();
        void Save_SubCircuits(bool AddISrc);
        DynStringArray getPCEatBus(String BusName);
        DynStringArray getPDEatBus(String BusName);
        String ReportPCEatBus(String BusName);
        String ReportPDEatBus(String BusName);
        String get_Line_Bus(String LName, int NBus);
        void get_longest_path();
        int Append2PathsArray(std::vector < int >* New_Path, int New_Path_maxidx);//  appends a new path to the array and returns the index(1D)
        void Normalize_graph();
        void Get_paths_4_Coverage();                             // Calculates the paths inside the graph
        TDSSCktElement* get_FActiveCktElement();
        double get_FLoadMultiplier();
        bool get_FBusNameRedefined();
        std::string get_FCaseName();
                                                                              // To guarantee the desired coverage when tearing the system
                // Arrange the files of the subcircuits to make them independent
        void Format_SubCircuits(String Path, int NumCkts, bool AddISrc);
        // Appends single phase ISources at the end of the given
        void AppendIsources(String myPath, int BusNum, String LinkBranch);
    };

} // namespace Circuit

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Circuit;
#endif

#endif //  CircuitH









