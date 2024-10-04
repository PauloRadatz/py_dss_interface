#pragma hdrstop

#include "Circuit.h"

#include "DSSGlobals.h"
#include <string>
#include <algorithm>
#include "dirsep.h"

using namespace std;

//----------------------------------------------------------------------------

namespace Circuit
{

    TDSSCircuit::TDSSCircuit(const String aName)

        // Var Retval:Integer;

        : inherited("Circuit"),
        NodeBufferMax(0),
        FBusNameRedefined(false),
        SavedNumBuses(0),
        FLoadMultiplier(0.0),
        AbortBusProcess(false),
        ActiveBusIndex(0),
        Fundamental(0.0),
        Control_BusNameRedefined(false),
        UEWeight(0.0),
        LossWeight(0.0),
        NumUEregs(0),
        NumLossRegs(0),
        CapacityStart(0.0),
        CapacityIncrement(0.0),
        TrapezoidalIntegration(false),
        LogEvents(false),
        NumDevices(0),
        NumBuses(0),
        NumNodes(0),
        MaxDevices(0),
        MaxBuses(0),
        MaxNodes(0),
        IncDevices(0),
        IncBuses(0),
        IncNodes(0),
        Coverage(0.0),
        Actual_Coverage(0.0),
        Num_SubCkts(0),
        VIndex(0),
        VLength(0),
        AD_Init(false),
        Issolved(false),
        DuplicatesAllowed(false),
        ZonesLocked(false),
        MeterZonesComputed(false),
        PositiveSequence(false),
        NeglectLoadY(false),
        NormalMinVolts(0.0),
        NormalMaxVolts(0.0),
        EmergMaxVolts(0.0),
        EmergMinVolts(0.0),
        GeneratorDispatchReference(0.0),
        DefaultGrowthFactor(0.0),
        DefaultGrowthRate(0.0),
        GenMultiplier(0.0),
        HarmMult(0.0),
        PriceSignal(0.0),
        ReductionStrategy(rsDefault),
        ReductionZmag(0.0),
        ReduceLateralsKeepLoad(false),
        PctNormalFactor(0.0),
        NodeMarkerCode(0),
        NodeMarkerWidth(0),
        SwitchMarkerCode(0),
        TransMarkerSize(0),
        CapMarkerSize(0),
        RegMarkerSize(0),
        PVMarkerSize(0),
        StoreMarkerSize(0),
        FuseMarkerSize(0),
        RecloserMarkerSize(0),
        RelayMarkerSize(0),
        TransMarkerCode(0),
        CapMarkerCode(0),
        RegMarkerCode(0),
        PVMarkerCode(0),
        StoreMarkerCode(0),
        FuseMarkerCode(0),
        RecloserMarkerCode(0),
        RelayMarkerCode(0),
        MarkSwitches(false),
        MarkTransformers(false),
        MarkCapacitors(false),
        MarkRegulators(false),
        MarkPVSystems(false),
        MarkStorage(false),
        MarkFuses(false),
        MarkReclosers(false),
        MarkRelays(false),
        NumCircuits(0),
        ActiveLoadShapeClass(0)
    {
        DeviceRef.clear();
        SolutionClass[ActiveActor]->NewObject(Get_Name());
        Solution = ActiveSolutionObj;
        Set_myLName(LowerCase(aName));
        Set_CaseName(aName);  // Default case name to circuitname
        // Sets CircuitName_
        Fundamental = DefaultBaseFreq;
        Set_ActiveCktElement(NULL);
        ActiveBusIndex = 0;    // Always a bus

        // initial allocations increased from 100 to 1000 to speed things up
        MaxBuses = 1000;  // good sized allocation to start
        MaxDevices = 1000;
        MaxNodes = 3 * MaxBuses;
        IncDevices = 1000;
        IncBuses = 1000;
        IncNodes = 3000;

        // Allocate some nominal sizes
        BusList = THashList(900);  // Bus name list Nominal size to start; gets reallocated
        DeviceList = THashList(900);
        AutoAddBusList = THashList(100);
        NumBuses = 0;  // Eventually allocate a single source
        NumDevices = 0;
        NumNodes = 0;
        Faults = TPointerList(2);
        CktElements = TPointerList(1000);
        PDElements = TPointerList(1000);
        PCElements = TPointerList(1000);
        DSSControls = TPointerList(10);
        Sources = TPointerList(10);
        MeterElements = TPointerList(20);
        Monitors = TPointerList(20);
        /*by Dahei*/
        FMonitors = TPointerList(20);
        /**/
        EnergyMeters = TPointerList(5);
        Sensors = TPointerList(5);
        Generators = TPointerList(5);
        WindGens = TPointerList(5);
        StorageElements = TPointerList(5);

        PVSystems = TPointerList(5);
        InvControls2 = TPointerList(5);
        ExpControls = TPointerList(5);
        Feeders = TPointerList(10);
        Substations = TPointerList(5);
        Transformers = TPointerList(10);
        AutoTransformers = TPointerList(10);
        CapControls = TPointerList(10);
        SwtControls = TPointerList(50);
        RegControls = TPointerList(5);
        Lines = TPointerList(1000);
        Loads = TPointerList(1000);
        ShuntCapacitors = TPointerList(20);
        Reactors = TPointerList(5);
        Reclosers = TPointerList(10);
        Relays = TPointerList(10);
        Fuses = TPointerList(50);

        // BHSL
        Gencls = TPointerList(50);
        Genrou = TPointerList(50);
        //JT
        ExcSexs = TPointerList(50);
        Tgov = TPointerList(50);

        Buses.resize(MaxBuses);
        MapNodeToBus.resize(MaxNodes);
        DeviceRef.resize(static_cast<size_t>(MaxDevices) + 1);
        ControlQueue = TControlQueue();
        LegalVoltageBases.resize(8);
        // Default Voltage Bases
        LegalVoltageBases[0] = 0.208;
        LegalVoltageBases[1] = 0.480;
        LegalVoltageBases[2] = 12.47;
        LegalVoltageBases[3] = 24.9;
        LegalVoltageBases[4] = 34.5;
        LegalVoltageBases[5] = 115.0;
        LegalVoltageBases[6] = 230.0;
        LegalVoltageBases[7] = 0.0;  // terminates array
        ActiveLoadShapeClass = USENONE; // Signify not set
        NodeBufferMax = 50;
        NodeBuffer.resize(static_cast<size_t>(NodeBufferMax) + 1); // A place to hold the nodes

        // Init global circuit load and harmonic source multipliers
        FLoadMultiplier = 1.0;
        GenMultiplier = 1.0;
        HarmMult = 1.0;
        PriceSignal = 25.0;   // $25/MWH

        // Factors for Autoadd stuff
        UEWeight = 1.0;  // Default to weighting UE same as losses
        LossWeight = 1.0;
        NumUEregs = 1;
        NumLossRegs = 1;
        UEregs = NULL;  // set to something so it wont break reallocmem
        LossRegs = NULL;
        UEregs = new longInt[NumUEregs];
        LossRegs = new longInt[NumLossRegs];
        UEregs[1 - 1] = 10;   // Overload UE
        LossRegs[1 - 1] = 13;   // Zone Losses
        CapacityStart = 0.9;     // for Capacity search
        CapacityIncrement = 0.005;
        LoadDurCurve = "";
        LoadDurCurveObj = NULL;
        PriceCurve = "";
        PriceCurveObj = NULL;

        // Flags
        DuplicatesAllowed = false;
        ZonesLocked = false;   // Meter zones recomputed after each change
        MeterZonesComputed = false;
        PositiveSequence = false;
        NeglectLoadY = false;
        LongLineCorrection = false;
        NormalMinVolts = 0.95;
        NormalMaxVolts = 1.05;
        EmergMaxVolts = 1.08;
        EmergMinVolts = 0.90;
        NodeMarkerCode = 16;
        NodeMarkerWidth = 1;
        MarkSwitches = false;
        MarkTransformers = false;
        MarkCapacitors = false;
        MarkRegulators = false;
        MarkPVSystems = false;
        // MarkPVSystems2   := FALSE;
        MarkStorage = false;
        // MarkStorage2     := FALSE;
        MarkFuses = false;
        MarkReclosers = false;
        SwitchMarkerCode = 5;
        TransMarkerCode = 35;
        CapMarkerCode = 38;
        RegMarkerCode = 17; //47;
        PVMarkerCode = 15;
        StoreMarkerCode = 9;
        FuseMarkerCode = 25;
        RecloserMarkerCode = 17;
        RelayMarkerCode = 17;
        TransMarkerSize = 1;
        CapMarkerSize = 3;
        RegMarkerSize = 5; //1;
        PVMarkerSize = 1;
        StoreMarkerSize = 1;
        FuseMarkerSize = 1;
        RecloserMarkerSize = 5;
        RelayMarkerSize = 5;
        BusMarkerList.clear();
        TrapezoidalIntegration = false;  // Default to Euler method
        LogEvents = false;
        GeneratorDispatchReference = 0.0;
        DefaultGrowthRate = 1.025;
        DefaultGrowthFactor = 1.0;
        DefaultDailyShapeObj = ((TLoadShapeObj*)LoadShapeClass[ActiveActor]->Find("default"));
        DefaultYearlyShapeObj = ((TLoadShapeObj*)LoadShapeClass[ActiveActor]->Find("default"));
        CurrentDirectory = "";
        Set_BusNameRedefined(true);  // set to force rebuild of buslists, nodelists
        SavedBuses.clear();
        SavedBusNames = NULL;
        ReductionStrategy = rsDefault;
        //     ReductionMaxAngle := 15.0;
        ReductionZmag = 0.02;
        NumCircuits = 0;
        ReduceLateralsKeepLoad = true;

        /*Misc objects*/
        AutoAddObj = TAutoAdd();
        Branch_List = NULL;
        BusAdjPC.clear();
        BusAdjPD.clear();

        // tearing algorithm vars initialization
        Coverage = 0.9;      // 90% coverage expected by default
        Actual_Coverage = -1;       //No coverage
        Num_SubCkts = CPU_Cores - 1;
        Longest_paths.clear();
        Path_Idx.clear();
        Buses_Covered.clear();
        Path_Size.clear();

        // Diakoptics variables
        Contours = TSparse_Complex();
        ZLL = TSparse_Complex();
        ZCC = TSparse_Complex();
        ZCT = TSparse_Complex();
        Y4 = TSparse_Complex();
        V_0 = TSparse_Complex();
        Ic = TSparse_Complex();
    }


    TDSSCircuit::~TDSSCircuit()
    {
        int i = 0;
        TDSSCktElement* pCktElem;
        String ElemName;
        for (int stop = NumDevices, i = 1; i <= stop; i++)
        {
            try
            {
                pCktElem = (TDSSCktElement*)CktElements.Get(i);
                //                ElemName := pCktElem.ParentClass.name + '.' + pCktElem.Name;
                ElemName = ((TDSSObject*)pCktElem)->Get_myPName() + "." + ((TDSSObject*)pCktElem)->get_Name();
                delete pCktElem;  //pCktElem->~TDSSCktElement();
            }
            catch (exception& E)
            {
                DoSimpleMsg("Exception Freeing Circuit Element:" + ElemName + CRLF + (std::string)E.what(), 423);
            }
        }
        for (int stop = NumBuses, i = 1; i <= stop; i++)
            delete Buses[i - 1];  // added 10-29-00
        /*(DeviceRef, 0);
        ReallocMem( Buses, 0 );
        ReallocMem( MapNodeToBus, 0 );
        ReallocMem( NodeBuffer, 0 );
        ReallocMem( UEregs, 0 );
        ReallocMem( LossRegs, 0 );
        ReallocMem( LegalVoltageBases, 0 );*/   // sounds irrelevant in this context - I may be wrong
        delete Solution;
        // Storage2Elements.Free;
        // PVSystems2.Free;
        /*by Dahei*/
        /**/
        ClearBusMarkers();
        FreeTopology();

        //  Release all ADiakoptics matrixes
          /*Contours.Free;     // problematic for now 12/15/2021
          ZLL.Free;
          ZCC.Free;
          ZCT.Free;
          Y4.Free;
          V_0.Free;
          Ic.Free;*/
          // todo check:  inherited::Destroy;
        if (SavedBusNames != NULL)
            delete[] SavedBusNames;

        delete[] UEregs;
        delete[] LossRegs;

    }

    /********************************************************************************
    *           Routine created to empty a recently created folder                 *
    *********************************************************************************/


    void DelFilesFromDir(String Directory, String FileMask, bool DelSubDirs)
    {
        /*
        String SourceLst;
        TSHFileOpStruct FOS;
        FillChar( FOS, sizeof( FOS ), 0 );
        FOS.wFunc = FO_DELETE;
        SourceLst = Directory + DIRSEP_STR + FileMask + "\x00";
        FOS.pFrom = SourceLst.c_str();
        if ( ! DelSubDirs )
          FOS.fFlags = FOS.fFlags | FOF_FILESONLY;
        // Remove the next line if you want a confirmation dialog box
        FOS.fFlags = FOS.fFlags | FOF_NOCONFIRMATION;
        // Add the next line for a "silent operation" (no progress box)
        FOS.fFlags = FOS.fFlags | FOF_SILENT;
        SHFileOperation( FOS );
        */
    }

    /********************************************************************************
    *         This routine retuns the index of the element within the array        *
    *********************************************************************************/
#define MAXIDX(x) (sizeof(x)/sizeof(x[0]))-1


    int get_element_Idx(pIntegerArray graph_in, int graph_in_maxidx, int element)
    {
        int result = 0;

        // To indicate that the element was found

        bool Found = false, End_Flag = false;
        int Graph_size = 0, Local_idx = 0;
        result = -1;     // In case the element is not in the array
        End_Flag = true;   //  To control the algorithm execution (while based)
        Local_idx = 0;
        Found = false;  //  Not found yet
        Graph_size = (graph_in_maxidx + 1);
        while ((End_Flag) && (Local_idx < Graph_size))
        {
            if ((graph_in)[Local_idx] == element)
            {
                End_Flag = false;
                Found = true;
            }
            else
            {
                Local_idx++;
            }
        }
        if (Found)
            result = Local_idx;
        return result;
    }
    /********************************************************************************
    *         This routine calculates the longest path within a linearized         *
    *         graph considering the zero level buses as the beginning of           *
    *         new path                                                             *
    *********************************************************************************/


    void TDSSCircuit::get_longest_path()
    {
        bool End_Flag = false;    //  Terminates the process
        //  Stores the Index value of the current level   

        int Current_Idx = 0    //  Stores the current level traced
            , Current_level = 0;
        /*# with Solution do */
        TSolutionObj with0 = *Solution;
        {
            Current_level = MaxIntValue(&with0.Inc_Mat_levels);                    //  Init level
            Current_Idx = get_element_Idx((pIntegerArray)&with0.Inc_Mat_levels, MAXIDX(with0.Inc_Mat_levels), Current_level);  //  Init Index
            End_Flag = true;
            New_Graph.clear();
            while (End_Flag)
            {
                //Checks the termination cirteria
                if ((Current_level > with0.Inc_Mat_levels[Current_Idx]) || (with0.Inc_Mat_levels[Current_Idx] == 0))
                    End_Flag = false;
                // Is the current bus part of the new backbone?
                if (with0.Inc_Mat_levels[Current_Idx] == Current_level)
                {
                    Current_level--;
                    New_Graph.push_back(Current_Idx);
                }
                Current_Idx--;
            }
        }
    }
    /********************************************************************************
    *   This routine appends an array to the paths array and returns its index     *
    *********************************************************************************/


    int TDSSCircuit::Append2PathsArray(std::vector < int >* New_Path, int New_Path_maxidx)
    {
        int result = 0;
        int Local_idx = 0;
        result = Longest_paths.size();
        for (int stop = New_Path_maxidx /*# High(New_Path) */, Local_idx = 0; Local_idx <= stop; Local_idx++)
        {
            Longest_paths.push_back((*New_Path)[Local_idx]);
        }
        return result;
    }

    void TDSSCircuit::AggregateProfiles(String mode)
    {
        //TBD
    }
    /********************************************************************************
    *     This routine normalizes the Inc_matrix levels                            *
    *********************************************************************************/


    void TDSSCircuit::Normalize_graph()
    {

        // To set the active level

        int Curr_level = 0                      //
            , idx = 0;
        bool Ref_detected = false;                      // To detect if there is a zero
        Curr_level = -1;                          // Initializing values
        Ref_detected = false;
        /*# with Solution do */
        TSolutionObj with0 = *Solution;
        {
            for (int stop = (with0.Inc_Mat_levels.size() - 1), idx = 0; idx <= stop; idx++)     // Sweeps the whole graph
            {
                if (with0.Inc_Mat_levels[idx] == 0)
                    Ref_detected = true;
                else
                {
                    if ((Curr_level >= with0.Inc_Mat_levels[idx]) || Ref_detected)
                    {
                        Ref_detected = false;
                        Curr_level = with0.Inc_Mat_levels[idx] - 1;
                        with0.Inc_Mat_levels[idx] = 1;
                    }
                    else
                        with0.Inc_Mat_levels[idx] = with0.Inc_Mat_levels[idx] - Curr_level;
                }
            }
        }
    }

    /********************************************************************************
    *  Traces the paths (0) in the graph to guarantee the desired coverage         *
    *********************************************************************************/
#define MAXIDX(x) (sizeof(x)/sizeof(x[0]))-1


    void TDSSCircuit::Get_paths_4_Coverage()
    {

        //  For storing temoprary doubles

        double DBLTemp = 0.0                           //  Stores the number of buses contained in the system
            , Sys_Size = 0.0;
        bool SMEnd = false;                          //  Terminates the state machine

        int i = 0                          // The current state of the state machine
            , State = 0;
        std::vector < int > Candidates;                // Array for 0 level buses idx
        TSolutionObj with0 = *Solution;
        {
            SMEnd = true;
            State = 0;
            Sys_Size = ((double)with0.Inc_Mat_Cols.size());
            Buses_Covered.resize(1);
            Path_Idx.resize(1);
            Actual_Coverage = -1;
            while (SMEnd)                                            // The state machine starts
            {
                switch (State)
                {
                case 0:
                {                                             // Processes the first path
                    Candidates.clear();
                    for (int stop = ((with0.Inc_Mat_levels.size()) - 1), i = 0; i <= stop; i++)     //Extracts the 0 Level Buses
                    {
                        if (with0.Inc_Mat_levels[i] == 0)
                        {
                            Candidates.push_back(i);
                        }
                    }
                    Longest_paths.clear();
                    Buses_Covered[0] = MaxIntValue(&Candidates);       //  Extracts the maximum level covered
                    Path_Idx[0] = Append2PathsArray(&Candidates, MAXIDX(Candidates)); //  No shifting in the graph
                    State = 1;                             //  Go to the next state
                }
                break;
                case 1:
                {                                                  // Extracts a new path from the longest branch to
                    get_longest_path();                                    // the backbone (Zeros)
                    Path_Idx.push_back(Append2PathsArray(&New_Graph, MAXIDX(New_Graph))); //  Adds the new candidates
                    //  Estimates the amount of buses covered in this path
                    Buses_Covered.push_back(New_Graph[0] - New_Graph.back());
                    // Replaces the latest path with 0 in the Bus levels array
                    for (int stop = (Longest_paths.size() - 1), i = Path_Idx[Path_Idx.back()]; i <= stop; i++)
                        with0.Inc_Mat_levels[Longest_paths[i]] = 0;
                    Normalize_graph();
                    // remains in the same state
                }
                break;
                }
                //  Checks the coverage index to stablish if is necessary to keep tracing paths to increase the coverage
                DBLTemp = 0.0;
                for (int stop = (Buses_Covered.size() - 1), i = 0; i <= stop; i++)
                    DBLTemp = DBLTemp + (0.0 + Buses_Covered[i]);
                DBLTemp = DBLTemp / Sys_Size;
                /*      If the New coverage is different from the previous one and is below the expected coverage keep going
                       The first criteria is to avoid keep working on a path that will not contribute to improve the coverage*/
                if ((DBLTemp != Actual_Coverage) && (DBLTemp >= Coverage))
                    SMEnd = false;
                Actual_Coverage = DBLTemp;
            }
        }
    }

    /********************************************************************************
    *     Appends single phase ISources to the each node of bus specified          *
    *     if the given linkBranch. This actions take place within the given file.  *
    ********************************************************************************
    */

    TDSSCktElement* TDSSCircuit::get_FActiveCktElement()
    {
        return FActiveCktElement;
    }

    void TDSSCircuit::AppendIsources(String myPath, int BusNum, String LinkBranch)
    {
        size_t jj = 0;
        int kk = 0;
        String text, BusName;
        TTextRec myFile;
        System::AssignFile(myFile, myPath);
        System::Append(myFile);
        IOResultToException();
        /*# with ActiveCircuit[ActiveActor] do */
        {
            TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
            {
                SetElementActive(LinkBranch);
                BusName = with0->get_FActiveCktElement()->GetBus(BusNum);
                jj = BusName.find('.');     // removes the dot
                if (jj != String::npos)
                    BusName = BusName.substr(0, jj);
                SetActiveBus(BusName);
                Bus::TDSSBus& pBus = *Buses[ActiveBusIndex];
                for (int stop = pBus.get_FNumNodesThisBus(), kk = 1; kk <= stop; kk++)
                {
                    text = "New ISource." + IntToStr(BusNum) + "_" + IntToStr(kk) + " phases=1 bus1=" + BusName + "." + IntToStr(kk) + " amps=0.000001 angle=0";
                    System::WriteLn(myFile, text);
                }
            }
        }
        System::CloseFile(myFile);
    }

    /********************************************************************************
    * This routine reads the master file of the torn circuit and creates the       *
    *  header definitions for declaring separate subcircuits in OpenDSS            *
    *           The flag AddISrc indicates if its necessary to create              *
    *           Isources at the edges of the link branches, the ISource            *
    *           magnitude is equal to 0.000001, angle 0 (for A-Diakoptics)         *
    *********************************************************************************/


    void TDSSCircuit::Format_SubCircuits(String Path, int NumCkts, bool AddISrc)
    {
        TTextRec myFile;
        String Temp_txt, Temp_txt2, text;
        std::vector < String > Xtra, File_Struc;
        bool Str_Found = false;
        size_t Local_Temp = 0;
        int FS_Idx = 0, FS_Idx1 = 0, FS_Idx2 = 0;
        String Reference[6/*# range 0..5*/];
        Reference[0] = "Redirect EnergyM";
        Reference[1] = "Redirect Monitor";
        Reference[2] = "MakeBu";
        Reference[3] = "Redirect BusVolta";
        Reference[4] = "Buscoords busco";
        Reference[5] = "Redirect zone";
        // Reads the master file
        System::AssignFile(myFile, Path + DIRSEP_STR "master.dss");
        System::Reset(myFile);                                        // Prepares for reading
        IOResultToException();
        File_Struc.clear();
        FS_Idx = 0;
        while (!Eof(myFile))                              // Extracts the file content as an array of strings
        {
            System::ReadLn(myFile, text);
            File_Struc.push_back(text);
            FS_Idx++;
        }
        System::CloseFile(myFile);
        //  Creates the copy for the interconnected system
        Xtra.clear();
        System::AssignFile(myFile, Path + DIRSEP_STR "Master_Interconnected.dss");
        System::Rewrite(myFile);                                      // Prepares for writing
        IOResultToException();
        for (int stop = (File_Struc.size() - 1), FS_Idx = 0; FS_Idx <= stop; FS_Idx++)
        {
            Str_Found = false;
            for (int stop = 5, FS_Idx1 = 0; FS_Idx1 <= stop; FS_Idx1++)
            {
                Local_Temp = File_Struc[FS_Idx].find(Reference[FS_Idx1]);
                Str_Found = (Local_Temp != String::npos) || Str_Found;
            }
            if (Str_Found)
            {
                Xtra.push_back(File_Struc[FS_Idx]);
            }
            else
                System::WriteLn(myFile, File_Struc[FS_Idx]);
        }
        // Adds the zones and the rest to the file
        for (int stop = (Xtra.size() - 1), FS_Idx = 0; FS_Idx <= stop; FS_Idx++)
        {
            System::WriteLn(myFile, Xtra[FS_Idx]);
        }
        System::CloseFile(myFile);

        // removes the unnecessary information from the master file (deletes the other zones)
        System::AssignFile(myFile, Path + DIRSEP_STR "master.dss");
        System::Rewrite(myFile);                                      // Prepares for writing
        IOResultToException();
        for (int stop = (File_Struc.size() - 1), FS_Idx = 0; FS_Idx <= stop; FS_Idx++)
        {
            Local_Temp = File_Struc[FS_Idx].find("Redirect zone");
            if (Local_Temp == String::npos)
            {
                Local_Temp = File_Struc[FS_Idx].find("Redirect EnergyM");
                if (Local_Temp == String::npos)
                {
                    Local_Temp = File_Struc[FS_Idx].find("Redirect Monitor");
                    if (Local_Temp == String::npos)
                        System::WriteLn(myFile, File_Struc[FS_Idx]);
                }
            }
        }
        System::CloseFile(myFile);

        // Adds Isources at the link branch edges if requested
        if (AddISrc)
            AppendIsources(Path + DIRSEP_STR "master.dss", 1, Link_Branches[1]);

        // Copies the support files to the zones directories
        FS_Idx = 0;
        while (FS_Idx != -1)
        {
            Local_Temp = File_Struc[FS_Idx].find("Redirect zone");
            if (Local_Temp == String::npos)
            {
                Local_Temp = File_Struc[FS_Idx].find("Redirect ");
                if (Local_Temp != String::npos)
                {
                    text = regex_replace(File_Struc[FS_Idx], std::regex("Redirect "), "");
                    for (int stop = NumCkts, FS_Idx1 = 2; FS_Idx1 <= stop; FS_Idx1++)
                        CopyFile(AnsiString(Path + DIRSEP_STR + text).c_str(), AnsiString(Path + DIRSEP_STR "zone_" + IntToStr(FS_Idx1) + DIRSEP_STR + text).c_str(), true);
                }
                FS_Idx++;
            }
            else
                FS_Idx = -1;                             // Ends the routine
        }
        // Creates the master file for each subcircuit
        for (int stop = NumCkts, FS_Idx = 2; FS_Idx <= stop; FS_Idx++)
        {
            System::AssignFile(myFile, Path + DIRSEP_STR "zone_" + IntToStr(FS_Idx) + DIRSEP_STR "master.dss");
            System::Rewrite(myFile);
            IOResultToException();
            System::WriteLn(myFile, "Clear");
            System::WriteLn(myFile, "New Circuit.Zone_" + IntToStr(FS_Idx));
            FS_Idx1 = 2;
            while (FS_Idx1 != -1)                      // Writes the global files
            {
                Local_Temp = File_Struc[FS_Idx1].find("Redirect zone");
                if (Local_Temp == String::npos)
                {
                    System::WriteLn(myFile, File_Struc[FS_Idx1]);
                    FS_Idx1++;
                }
                else
                    FS_Idx1 = -1;
            }
            for (int stop = (File_Struc.size() - 1), FS_Idx1 = 0; FS_Idx1 <= stop; FS_Idx1++)   // Writes the zone files
            {
                Local_Temp = File_Struc[FS_Idx1].find("Redirect zone_" + IntToStr(FS_Idx));
                if (Local_Temp != String::npos)
                {
                    text = regex_replace(File_Struc[FS_Idx1], std::regex("zone_" + IntToStr(FS_Idx) + DIRSEP_STR), "");
                    System::WriteLn(myFile, text);
                }
            }
            System::CloseFile(myFile);

            // Adds Isources at the link branch edges if requested
            if (AddISrc)
            {
                text = Path + DIRSEP_STR "zone_" + IntToStr(FS_Idx) + DIRSEP_STR "master.dss";
                AppendIsources(text, 2, Link_Branches[FS_Idx]);
                // If there is another link branch, means that this zone conencts with other through ZCC
                // Add Another current source at the point of connection
                if (Link_Branches.size() > FS_Idx)
                    AppendIsources(text, 1, Link_Branches[FS_Idx]);
            }
        }
        // Sets the properties of the VSource on each subcricuit based on the latest voltage measured
        FS_Idx1 = 0;
        for (int stop = NumCkts, FS_Idx = 1; FS_Idx <= stop; FS_Idx++)
        {
            if (FS_Idx == 1)
                System::AssignFile(myFile, Path + DIRSEP_STR "VSource.dss");
            else
                System::AssignFile(myFile, Path + DIRSEP_STR "zone_" + IntToStr(FS_Idx) + DIRSEP_STR "VSource.dss");
            System::Rewrite(myFile);
            IOResultToException();
            for (int stop = 3, FS_Idx2 = 1; FS_Idx2 <= stop; FS_Idx2++)
            {
                if (FS_Idx2 == 1)
                {
                    Temp_txt = "source";
                    Temp_txt2 = "Edit ";
                }
                else
                {
                    Temp_txt = "Vph_" + IntToStr(FS_Idx2);
                    Temp_txt2 = "New ";
                }
                text = Temp_txt2 + "Vsource." + Temp_txt + " bus1=" + PConn_Names[FS_Idx] + "." + IntToStr(FS_Idx2) + " phases=1 pu=1.0" + " basekv=" + FloatToStrF(static_cast<double>(PConn_Voltages[FS_Idx1]), ffGeneral, 8, 3) + " angle="
                    + FloatToStrF(static_cast<double>(PConn_Voltages[static_cast<std::vector<double, std::allocator<double>>::size_type>(FS_Idx1) + 1]), ffGeneral, 8, 3) + " R1=0 X1=0.001 R0=0 X0=0.001";

                System::WriteLn(myFile, text);
                FS_Idx1 = FS_Idx1 + 2;
            }
            System::CloseFile(myFile);
        }
    }

    /********************************************************************************
    *        Saves the subcircuits created in memory into the hard drive           *
    *        The flag AddISrc indicates if its necessary to create                 *
    *        Isources at the edges of the link branches, the ISource               *
    *        magnitude is equal to 0.000001, angle 0 (for A-Diakoptics)            *
    ********************************************************************************
    */


    void TDSSCircuit::Save_SubCircuits(bool AddISrc)
    {
        String Fileroot;
        // Prepares everything to save the base of the torn circuit on a separate folder
        Fileroot = GetCurrentDir();
        Fileroot = Fileroot + DIRSEP_STR "Torn_Circuit";
        CreateDir(Fileroot);                        // Creates the folder for storing the modified circuit
        DelFilesFromDir(Fileroot, "*", true);         // Removes all the files inside the new directory (if exists)
        DSSExecutive[ActiveActor]->Set_Command(String("save circuit Dir=\"") + Fileroot + "\"");
        // This routine extracts and modifies the file content to separate the subsystems as OpenDSS projects indepedently
        Format_SubCircuits(Fileroot, Locations.size(), AddISrc);
    }

    /********************************************************************************
    *       Delivers the name of the bus at the specific line and terminal         *
    ********************************************************************************/


    String TDSSCircuit::get_Line_Bus(String LName, int NBus)
    {
        String result;
        int i = 0, activesave = 0;
        TLineObj* pLine;
        String S;
        bool Found = false;
        String* NBuses;
        NBuses = new std::string[2];
        if (ActiveCircuit[ActiveActor] != NULL)
        {      // Search list of Lines in active circuit for name
          /*# with ActiveCircuit[ActiveActor].Lines do */
            TPointerList with0 = ActiveCircuit[ActiveActor]->Lines;
            {
                S = LName;  // Convert to Pascal String
                Found = false;
                activesave = with0.get_myActiveItem();
                pLine = (TLineObj*)with0.Get_First();
                while (pLine != NULL)
                {
                    if (CompareText(((TDSSObject*)pLine)->get_Name(), S) == 0)
                    {
                        ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLine);
                        Found = true;
                        break;
                    }
                    pLine = (TLineObj*)with0.Get_Next();
                }
                if (!Found)
                {
                    DoSimpleMsg(String("Line \"") + S + "\" Not Found in Active Circuit.", 5008);
                    pLine = (TLineObj*)with0.Get(activesave);    // Restore active Line
                    ActiveCircuit[ActiveActor]->Set_ActiveCktElement(pLine);
                }
            }
            for (int stop = ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Get_NTerms(), i = 1; i <= stop; i++)
                NBuses[i] = ActiveCircuit[ActiveActor]->get_FActiveCktElement()->GetBus(i);
            // returns the name of the desired bus
            result = NBuses[NBus];
        }
        return result;
    }

    /********************************************************************************
    *        Generates the graph file for MeTIS within the project's folder        *
    ********************************************************************************/


    String TDSSCircuit::Create_MeTIS_graph()
    {
        String result;
        bool exists = false;
        int myIntVar = 0, k = 0, jj = 0, i = 0;
        String myClass, myName, Filename;
        TTextRec F;
        std::vector < String > myPDEList, MyGraph;
        std::vector < int > MyIdx;
        /*# with Solution do */
        TSolutionObj with0 = *Solution;
        {
            // Calculates the incidence matrix and laplacian to generate the graph file to be
            // send to MeTiS
            with0.Calc_Inc_Matrix_Org(ActiveActor);                       //Calculates the ordered incidence matrix
            // Initializes the METIS related variables
            myPDEList.clear();
            MyGraph.clear();
            with0.Laplacian = with0.IncMat.Transpose();                        // Transposes the Incidence Matrix
            with0.Laplacian = with0.Laplacian.multiply(&(with0.IncMat));                // Laplacian Matrix calculated
            // Filters the incidence matrix to remove duplicated branches (parallel)
            for (int stop = (with0.Inc_Mat_Cols.size() - 1), i = 0; i <= stop; i++)
            {
                MyIdx.clear();
                myName = with0.Inc_Mat_Cols[i];
                // first, get the name of all PDE conencted to this Bus
                for (int stop = (with0.IncMat.NZero() - 1), jj = 0; jj <= stop; jj++)
                {
                    if (with0.IncMat.data[jj][1] == i)
                    {
                        // Check if this is not a parallel branch
                        exists = false;
                        if (MyIdx.size() > 1) // Only if it's not the first time
                        {
                            for (int stop = (MyIdx.size() - 2) / 2, k = 0; k <= stop; k++)
                            {
                                // Checks for the other terminal
                                if (jj < (with0.IncMat.data.size() - 1))
                                {
                                    if (static_cast<size_t>(jj + 1) < with0.IncMat.data.size() && static_cast<size_t>(1) < with0.IncMat.data[static_cast<size_t>(jj) + 1].size()) {
                                        myIntVar = with0.IncMat.data[static_cast<size_t>(jj + 1)][static_cast<size_t>(1)];
                                    }
                                    else
                                        myIntVar = with0.IncMat.data[jj][1];
                                }
                                else
                                    myIntVar = with0.IncMat.data[jj][1];
                                if (MyIdx[static_cast<size_t>(k) * 2] == myIntVar)
                                {
                                    exists = true;
                                    break;
                                }
                            }
                        }
                        if (!exists)
                        {
                            // Stores the name of the PDE
                            myName = with0.Inc_Mat_Rows[with0.IncMat.data[jj][0]];
                            myPDEList.push_back(myName);
                            //myPDEList.size() = myPDEList.size() + 1;
                            // Checks for the other terminal
                            if (jj < (with0.IncMat.data.size() - 1))
                            {
                                if (with0.IncMat.data[static_cast<size_t>(jj) + 1][0] == with0.IncMat.data[jj][0])
                                    MyIdx[MyIdx.size()] = with0.IncMat.data[static_cast<size_t>(jj) + 1][1];
                                else
                                    MyIdx[MyIdx.size()] = with0.IncMat.data[jj][1];
                            }
                            else
                                MyIdx.push_back(with0.IncMat.data[jj][1]);
                            //MyIdx.Length = MyIdx.Length + 1;
                            // Now, get the number of Phases
                            myIntVar = myName.find(".");
                            myClass = myName.substr(0, static_cast<size_t>(myIntVar));
                            // if transformer, the weigth is the lowest
                            if (myClass != "Transformer")
                            {
                                ActiveCircuit[ActiveActor]->SetElementActive(myName);
                                myIntVar = ActiveCircuit[ActiveActor]->get_FActiveCktElement()->Get_NPhases();
                            }
                            else
                                myIntVar = 1;
                            MyIdx.push_back(myIntVar);
                            //MyIdx.Length = MyIdx.Length + 1;
                        }
                    }
                }
                //MyIdx.Length = MyIdx.Length - 1;
                myName = "";
                for (int stop = (MyIdx.size() - 1), jj = 0; jj <= stop; jj++)
                    myName = myName + IntToStr(MyIdx[jj]) + " ";
                MyGraph.push_back(myName);
                //MyGraph.Length = MyGraph.Length + 1;
            }
            MyGraph.resize(MyGraph.size() - 1);
            myPDEList.resize(myPDEList.size() - 1);
            /********************************************************************************
                Generates the graph file
            *********************************************************************************/
            // Get_First(), get the number of branches in the model excluding parallel branches
            jj = 0;
            for (int stop = (with0.Inc_Mat_Rows.size() - 1), i = 0; i <= stop; i++)
            {
                // check if it's on the list
                for (int stop = (myPDEList.size() - 1), k = 0; k <= stop; k++)
                {
                    if (LowerCase(with0.Inc_Mat_Rows[i]) == LowerCase(myPDEList[k]))
                    {
                        jj++;
                        break;
                    }
                }
            }
            Filename = GetOutputDirectory() + CircuitName_[ActiveActor] + ".graph";
            System::AssignFile(F, Filename);
            System::Rewrite(F);
            IOResultToException();
            System::WriteLn(F, IntToStr(with0.Inc_Mat_Cols.size()) + " " + IntToStr(jj) + " 1"); // it should be the rank of the incidence matrix
            for (int stop = (MyGraph.size() - 1), i = 1; i <= stop; i++)
                System::WriteLn(F, MyGraph[i]);
            System::CloseFile(F);
        }
        result = Filename;
        return result;
    }

    /********************************************************************************
    *    Executes MeTIS and gets the names of the link branches between zones      *
    *********************************************************************************/


    String TDSSCircuit::Create_MeTIS_Zones(String Filename)
    {
        String result;
        bool Flag;
        // For debugging

        String TreeNm, MeTISCmd, BusName, Terminal, TextCmd, PDElement;                                     // Active Location

        int NodeIdx = 0, Num_Pieces = 0, Location_idx = 0, j = 0, jj = 0, dbg = 0, dbg2 = 0, i = 0;
        TFileSearchReplace* Replacer;
        Num_Pieces = Num_SubCkts;
        /*# with Solution do */
        TSolutionObj with0 = *Solution;
        {
            /********************************************************************************************/
        //    if Num_pieces <= 8 then MeTISCmd   :=  'kmetis.exe'  // For less than 8 zones use pMeTIS
        //    else MeTISCmd   :=  'kmetis.exe';                    // For more than 8 zonez use k-Way (kMeTIS)
            // In the past we use to use pmetis and kmetis, however, in our latest update we realized kmetis is enough
            // update 09-24-2020 by Davis Montenegro
            MeTISCmd = "kmetis.exe";
            /********************************************************************************************/
            if (FileExists(AnsiString(Filename + ".part." + IntToStr(Num_Pieces)).c_str()))    // Checks if the file exists before
                DeleteFile(AnsiString(Filename + ".part." + IntToStr(Num_Pieces)).c_str());
            do
            {
                TextCmd = RunMeTIS(DSSDirectory + MeTISCmd + " \"" + Filename + "\" " + IntToStr(Num_Pieces));  // Executes MeTIS
                Flag = (TextCmd.find("I detected an error") == String::npos);
                if (Flag)       // The # of edges was wrong, use the one proposed by MeTIS
                {
                    TextCmd = GetNumEdges(TextCmd);                     // Gest the # of edges proposed by MeTIS
                    jj = IntToStr(with0.Inc_Mat_Cols.size()).size() + 2;// Caculates the index for replacing the number in the Graph File
                    // Replaces the old data with the new at the file header
                    Replacer = new TFileSearchReplace(Filename);
                    try
                    {
                        TReplaceFlags myreplaceFlags;
                        Replacer->Replace(IntToStr(with0.Inc_Mat_Cols.size()) + " " + IntToStr(with0.Inc_Mat_Cols.size() - 1), IntToStr(with0.Inc_Mat_Cols.size()) +
                            " " + TextCmd, myreplaceFlags);
                        //        }
                        //        __finally
                        //        {
                        delete Replacer;
                    }
                    catch (...)
                    {
                        // Nothing to match with the DElphi implementation
                    }
                }
            } while (!(!Flag));
            /********************************************************************************************/
            // Verifies if there was no error executing MeTIS and the zones file was created
            if ((TextCmd != "**Error**") && FileExists((Filename + ".part." + IntToStr(Num_Pieces)).c_str()))
            {
                std::string mypath = Filename + ".part." + IntToStr(Num_Pieces);
                std::ifstream inf(mypath.c_str());
                std::string line;
                while (getline(inf, line))
                    MeTISZones.push_back(line);

                std::vector < std::string > temp;
                TextCmd = MeTISZones[1];
                temp.push_back(TextCmd);
                auto it2 = temp.begin();
                auto it3 = temp.end();
                auto it = MeTISZones.begin();
                MeTISZones.erase(it);
                MeTISZones.insert(it, it2, it3);
                Locations.resize(1);
                BusZones.resize(1);
                for (int stop = (MeTISZones.size() - 1), i = 0; i <= stop; i++)
                {
                    if (i == 0)
                    {
                        Locations[i] = 0;
                        BusZones[i] = MeTISZones[i];
                    }
                    else
                    {
                        if (MeTISZones[i] != BusZones[BusZones.size()])   // Moving to another zone in the file
                        {
                            j = 0;
                            if (i < (MeTISZones.size() - 1))                // If not lower means the zone is only 1 bus
                                j = ((int)(MeTISZones[i] == MeTISZones[static_cast<size_t>(i) + 1]));
                            if (j == 1)                                     // Varifies that the zone is big enough
                            {
                                j = 0;                                      // Verifies that this zone hasn't been counted before
                                for (int stop = (BusZones.size() - 1), jj = 0; jj <= stop; jj++)
                                {
                                    if (MeTISZones[i] == BusZones[jj])
                                    {
                                        j++;
                                        break;
                                    }
                                }
                                if (j == 0)                                   // Is not in the list, add the new location
                                {
                                    //                Locations.Length = Locations.Length + 1;    // not needed with the adopted structures
                                    //                BusZones.Length = BusZones.Length + 1;
                                    Locations.push_back(i);
                                    BusZones.push_back(MeTISZones[i]);
                                }
                            }
                        }
                    }
                }
            }
            for (int stop = (Locations.size() - 1), j = 0; j <= stop; j++)
                Locations[j]++; //Adjust the location coords
        }
        result = TextCmd;
        return result;
    }

    /********************************************************************************
    *                   Disables all DER present in the model                      *
    *********************************************************************************/


    void TDSSCircuit::Disable_All_DER()
    {
        int myDERIdx = 0, MyIdx = 0, DevClassIndex = 0;
        vector < String > myDERList;
        myDERList.resize(3);
        {
            String Circuit__0[3] = { "PVSystem" ,"Generator" , "Storage" };
            for (int stop = 2, MyIdx = 0; MyIdx <= stop; MyIdx++)
                myDERList[MyIdx] = Circuit__0[MyIdx];
        }
        for (int stop = (myDERList.size() - 1), myDERIdx = 0; myDERIdx <= stop; myDERIdx++)
        {
            DevClassIndex = ClassNames[ActiveActor].Find(myDERList[myDERIdx]);
            LastClassReferenced[ActiveActor] = DevClassIndex;
            ActiveDSSClass[ActiveActor] = (TDSSClass*)DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
            if (ActiveDSSClass[ActiveActor]->Get_ElementCount() > 0)
            {
                MyIdx = ActiveDSSClass[ActiveActor]->Get_First();
                do
                {
                    get_FActiveCktElement()->Set_Enabled(false);
                    MyIdx = ActiveDSSClass[ActiveActor]->Get_Next();
                } while (!(MyIdx <= 0));
            }
        }
    }
    /********************************************************************************
    *    Returns the list of all PDE connected to the bus nam given at BusName     *
    *********************************************************************************/


    DynStringArray TDSSCircuit::getPDEatBus(String BusName)
    {
        DynStringArray result;
        TDSSClass* Dss_Class;
        int j = 0, i = 0;
        vector < String > myBus;
        DynStringArray myBusList;
        myBus.resize(2);
        result.clear();

        BusName = LowerCase(BusName);
        for (int stop = DSSClassList[ActiveActor].get_myNumList(), i = 1; i <= stop; i++)
        {
            Dss_Class = (TDSSClass*)DSSClassList[ActiveActor].Get(i);
            if (dynamic_cast<TCktElementClass*>(Dss_Class))
            {
                // Checks if it is a PCE class
                if (dynamic_cast<TPDClass*>(Dss_Class))
                {
                    // If it is, checks all the elements to verify if one or more are
                    // connected to the bus given
                    Dss_Class->Get_First();
                    for (int stop = Dss_Class->Get_ElementCount(), j = 1; j <= stop; j++)
                    {
                        myBus[0] = LowerCase(StripExtension(get_FActiveCktElement()->GetBus(1)));
                        myBus[1] = LowerCase(StripExtension(get_FActiveCktElement()->GetBus(2)));
                        if (((myBus[0] == BusName) || (myBus[1] == BusName)) && (myBus[0] != myBus[1]))
                        {
                            result.push_back(Dss_Class->get_myClass_name() + "." + ((TDSSObject*)get_FActiveCktElement())->get_Name());
                        }
                        Dss_Class->Get_Next();
                    }
                }
            }
        }
        if (result.empty())
        {
            result.resize(1);
            result[0] = "None";
        }
        return result;
    }
    /********************************************************************************
    *    Returns the list of all PCE connected to the bus nam given at BusName     *
    *********************************************************************************/


    DynStringArray TDSSCircuit::getPCEatBus(String BusName)
    {
        DynStringArray result;
        TDSSClass* Dss_Class;
        int j = 0, i = 0;
        String myBus;
        DynStringArray myBusList;
        result.clear();

        BusName = LowerCase(BusName);
        for (int stop = DSSClassList[ActiveActor].get_myNumList(), i = 1; i <= stop; i++)
        {
            Dss_Class = (TDSSClass*)DSSClassList[ActiveActor].Get(i);
            if (dynamic_cast<TCktElementClass*>(Dss_Class))
            {
                // Checks if it is a PCE class
                if ((dynamic_cast<TPCClass*>(Dss_Class) != nullptr) || (Dss_Class->get_myClass_name() == "Capacitor") || (Dss_Class->get_myClass_name() == "Reactor"))
                {
                    // If it is, checks all the elements to verify if one or more are
                    // connected to the bus given
                    Dss_Class->Get_First();
                    for (int stop = Dss_Class->Get_ElementCount(), j = 1; j <= stop; j++)
                    {
                        myBus = LowerCase(StripExtension(get_FActiveCktElement()->GetBus(1)));
                        if (myBus == BusName)
                        {
                            result.push_back(Dss_Class->get_myClass_name() + "." + ((TDSSObject*)get_FActiveCktElement())->get_Name());
                        }
                        Dss_Class->Get_Next();
                    }
                }
            }
        }
        if (result.empty())
        {
            result.resize(1);
            result[0] = "None";
        }
        return result;
    }

    /********************************************************************************
    *             Gets all PCE at given bus and returns the list as string         *
    *********************************************************************************/


    String TDSSCircuit::ReportPCEatBus(String BusName)
    {
        String result;
        int i = 0;
        DynStringArray myPCEList;
        myPCEList = getPCEatBus(BusName);
        result = "";
        for (int stop = (myPCEList.size() - 1), i = 0; i <= stop; i++)
            if (myPCEList[i] != "")
                result = result + myPCEList[i] + ",";
        return result;
    }

    /********************************************************************************
    *             Gets all PDE at given bus and returns the list as string         *
    *********************************************************************************/


    String TDSSCircuit::ReportPDEatBus(String BusName)
    {
        String result;
        int i = 0;
        DynStringArray myPDEList;
        myPDEList = getPDEatBus(BusName);
        result = "";
        for (int stop = (myPDEList.size() - 1), i = 0; i <= stop; i++)
            if (myPDEList[i] != "")
                result = result + myPDEList[i] + ",";
        return result;
    }

    /********************************************************************************
    *         This routine tears the circuit into many pieces as CPUs are          *
    *         available in the local computer (in the best case)                   *
    *********************************************************************************/


    int TDSSCircuit::Tear_Circuit()
    {
        int result = 0;
        bool FileCreated = false;
        TTextRec Ftree{}, F{};                                           // For debugging

        String TreeNm, BusName, Terminal, TextCmd, PDElement, Filename;                                     // Active Location

        int NodeIdx = 0                          // Generic counter variables
            , Num_Pieces = 0, Location_idx = 0, j = 0, jj = 0, dbg = 0, dbg2 = 0, k = 0, l = 0, i = 0;
        int* Candidates = new int[1];                 // Array for 0 level buses idx

        TEnergyMeterObj* EMeter;
        polar Volts;
        vector <double> Term_volts;                  // To verify the connection of the branch

        TVsourceObj pVSource;
        TFileSearchReplace* Replacer = nullptr;
        DynStringArray myPDEList;


        if (UseUserLinks && (Link_Branches.size() > 0))
            Num_Pieces = Link_Branches.size() + 1;
        else
            Num_Pieces = Num_SubCkts;
        /*# with Solution do */
        TSolutionObj with0 = *Solution;
        {
            if (UseUserLinks && Link_Branches.size() > 0) // usando ramas de enlace ingresadas manualmente
                // using link branches entered manually
            {
                with0.Calc_Inc_Matrix_Org(ActiveActor);                       //Calculates the ordered incidence matrix
                Locations.resize(Link_Branches.size());
                Locations[0] = 0;
                for (int stop = (Link_Branches.size() - 1), i = 1; i <= stop; i++)
                    /*# with Solution do */
                    Locations[i] = with0.get_PDE_Bus1_Location(Link_Branches[i]);
            }
            else
            {
                Filename = Create_MeTIS_graph();
                TextCmd = Create_MeTIS_Zones(Filename);
                UseUserLinks = false;
            }
            /********************************************************************************************/
            // Verifies if there was no error executing MeTIS and the zones file was created
            if (((TextCmd != "**Error**") && FileExists((Filename + ".part." + IntToStr(Num_Pieces)).c_str())) || UseUserLinks)
            {
                //***********The directory is ready for storing the new circuit****************
                EMeter = (TEnergyMeterObj*)EnergyMeters.Get_First();
                while (EMeter != NULL)
                {
                    ((TDSSCktElement*)EMeter)->Set_Enabled(false);
                    EMeter = (TEnergyMeterObj*)EnergyMeters.Get_Next();
                }
                //************ Creates the meters at the tearing locations  ********************
                result = 1;                                  // Resets the result variable (Return)
                PConn_Voltages.resize(Locations.size() * 6);        //  Sets the memory space for storing the voltage at the point of conn
                Link_Branches.resize(Locations.size());           //  Sets the memory space for storing the link branches names
                PConn_Names.resize(Locations.size());             //  Sets the memory space for storing the Bus names
                SolutionAbort = false;
                j = 0;
                for (int stop = (Locations.size() - 1), i = 0; i <= stop; i++)
                {
                    if (i > 0)
                    {
                        result++;
                        // Gets the name of the PDE for placing the EnergyMeter
                        /*# with Solution do */
                        {
                            PDElement = with0.Inc_Mat_Rows[with0.get_IncMatrix_Row(Locations[i])];
                            Link_Branches[i] = PDElement;
                            dbg = with0.get_IncMatrix_Col(Locations[i]);      // Temporary stores the given location
                            // Checks the branch orientation across the feeder by substracting the voltages around the branch
                            // Start with Bus 1
                            Term_volts.resize(2);
                            for (int stop = 1, dbg = 0; dbg <= stop; dbg++)
                            {
                                BusName = with0.Inc_Mat_Cols[with0.Active_Cols[dbg]];
                                SetActiveBus(BusName);           // Activates the Bus
                                TDSSBus& pBus = *Buses[static_cast<size_t>(ActiveBusIndex) - 1];
                                jj = 1;
                                // this code so nodes come out in order from smallest to larges
                                do
                                {
                                    NodeIdx = pBus.FindIdx(jj);   // Get the index of the Node that matches jj
                                    jj++;
                                } while (!(NodeIdx > 0));
                                Volts = ctopolardeg(Solution->NodeV[pBus.GetRef(NodeIdx)]);  // referenced to pBus
                                Term_volts[dbg] = Volts.mag;
                            }

                            // Determines the best place to connect the EnergyMeter
                            Term_volts[0] = Term_volts[0] - Term_volts[1];
                            jj = Link_Branches[i].find(".");
                            BusName = get_Line_Bus(Link_Branches[i].substr(jj + 1), 2);
                            jj = BusName.find(".");     // removes the dot
                            if (jj > 0)
                                BusName = BusName.substr(0, static_cast<size_t>(jj));
                            Terminal = "terminal=1";
                            PConn_Names[i] = BusName;
                            SetActiveBus(BusName);           // Activates the Bus
                            TDSSBus& pBus2 = *(Buses[ActiveBusIndex]);
                            for (int stop = 3, jj = 1; jj <= stop; jj++)
                            {
                                // this code so nodes come out in order from smallest to larges
                                NodeIdx = pBus2.FindIdx(jj);   // Get the index of the Node that matches jj
                                Volts = ctopolardeg(Solution->NodeV[pBus2.GetRef(NodeIdx)]);  // referenced to pBus
                                PConn_Voltages[j] = (double(Volts.mag) / 1000);
                                j++;
                                PConn_Voltages[j] = Volts.ang;
                                j++;
                            }
                        }
                        // Generates the OpenDSS Command;
                        DSSExecutive[ActiveActor]->Set_Command("New EnergyMeter.Zone_" + IntToStr(i + 1) + " element=" + PDElement + " " + Terminal + " option=R action=C");
                    }
                    else
                    {
                        // The reference bus (Actor 1)
                        BusName = with0.Inc_Mat_Cols[0];
                        PConn_Names[i] = BusName;
                        SetActiveBus(BusName);           // Activates the Bus
                        TDSSBus& pBus3 = *(Buses[ActiveBusIndex]);
                        // Stores the voltages for the Reference bus first
                        for (int stop = 3, jj = 1; jj <= stop; jj++)
                        {
                            // this code so nodes come out in order from smallest to larges
                            NodeIdx = pBus3.FindIdx(jj);   // Get the index of the Node that matches jj
                            Volts = ctopolardeg(Solution->NodeV[pBus3.GetRef(NodeIdx)]);  // referenced to pBus
                            PConn_Voltages[j] = (double(Volts.mag) / 1000);
                            j++;
                            PConn_Voltages[j] = Volts.ang;
                            j++;
                        }
                    }
                }
            }
            else
            {
                if (TextCmd == "**Error**")
                    DoErrorMsg("Tear_Circuit", "MeTIS cannot start.", "The MeTIS program (pmetis.exe/kmetis.exe) cannot be executed/found.", 7006);
                else
                    DoErrorMsg("Tear_Circuit", "The graph file is incorrect.", String("MeTIS cannot process the graph file because is incorrect") + "(The number of edges is incorrect).", 7007);
            }
        }
        return result;
    }

    //----------------------------------------------------------------------------

    //----------------------------------------------------------------------------


    void TDSSCircuit::ProcessBusDefs(int ActorID)
    {
        String BusName;
        int NNodes = 0, NP = 0, Ncond = 0, i = 0, j = 0, iTerm = 0, retval = 0;
        bool NodesOK = false;
        /*# with ActiveCktElement do */
        TDSSCktElement* with0 = get_FActiveCktElement();
        {
            NP = with0->Get_NPhases();
            Ncond = with0->Get_NConds();
            Parser[ActorID]->set_Token(with0->Get_FirstBus());     // use parser functions to decode
            for (int stop = with0->Get_NTerms(), iTerm = 1; iTerm <= stop; iTerm++)
            {
                NodesOK = true;
                // Assume normal phase rotation  for default
                for (int stop = NP, i = 1; i <= stop; i++)
                    NodeBuffer[i - 1] = i; // set up buffer with defaults

                // Default all other conductors to a ground connection
                // If user wants them ungrounded, must be specified explicitly!
                for (int stop = Ncond, i = NP + 1; i <= stop; i++)
                    NodeBuffer[i - 1] = 0;

                // Parser will override bus connection if any specified
                BusName = Parser[ActorID]->ParseAsBusName(NNodes, (pIntegerArray) & (NodeBuffer[0]), ActorID);

                // Check for error in node specification
                for (int stop = NNodes, j = 1; j <= stop; j++)
                {
                    if (NodeBuffer[j - 1] < 0)
                    {
                        retval = DSSMessageDlg("Error in Node specification for Element: \"" + with0->ParentClass->get_myClass_name() + "." + Get_Name() + "\"" + CRLF + "Bus Spec: \"" + Parser[ActorID]->get_Token() + "\"", false);
                        NodesOK = false;
                        if (retval == -1)
                        {
                            AbortBusProcess = true;
                            AppendGlobalResult("Aborted bus process.");
                            return;
                        }
                        break;
                    }
                }

                // Node -Terminal Connnections
                // Caution: Magic -- AddBus replaces values in nodeBuffer to correspond
                // with global node reference number.
                if (NodesOK)
                {
                    with0->Set_ActiveTerminal(iTerm);
                    with0->ActiveTerminal->BusRef = AddBus(BusName, Ncond, ActorID);
                    with0->SetNodeRef(iTerm, (pIntegerArray) & (NodeBuffer[0]));  // for active circuit
                }
                Parser[ActorID]->set_Token(with0->Get_NextBus());
            }
        }
    }


    //----------------------------------------------------------------------------



    void TDSSCircuit::AddABus()
    {
        if (NumBuses > MaxBuses)
        {
            MaxBuses += IncBuses;
            Buses.resize(MaxBuses);
        }
    }

    //----------------------------------------------------------------------------



    void TDSSCircuit::AddANodeBus()
    {
        if (NumNodes > MaxNodes)
        {
            MaxNodes += IncNodes;
            MapNodeToBus.resize(MaxNodes);
        }
    }

    //----------------------------------------------------------------------------



    int TDSSCircuit::AddBus(const String BusName, int NNodes, int ActorID)
    {
        int result = 0;
        int NodeRef = 0, i = 0;

        // Trap error in bus name
        if (BusName.size() == 0)
        {  // Error in busname
            DoErrorMsg("TDSSCircuit.AddBus", "BusName for Object \"" + get_FActiveCktElement()->get_Name() + "\" is null.", "Error in definition of object.", 424);
            for (int stop = get_FActiveCktElement()->Get_NConds(), i = 1; i <= stop; i++)
                NodeBuffer[i - 1] = 0;
            result = 0;
            return result;
        }
        result = BusList.Find(BusName);
        if (result == 0)
        {
            result = BusList.Add(BusName);    // Result is index of bus
            NumBuses++;
            AddABus();   // Allocates more memory if necessary
            Buses[static_cast<size_t>(NumBuses) - 1] = new TDSSBus();

        }

        /*Define nodes belonging to the bus*/
        /*Replace Nodebuffer values with global reference number*/
      /*# with Buses^[Result] do */
        {
            auto with0 = Buses[static_cast<size_t>(result) - 1];
            {
                for (int stop = NNodes, i = 1; i <= stop; i++)
                {
                    NodeRef = with0->Add(NodeBuffer[i - 1], ActorID);
                    if (NodeRef == NumNodes)
                    {  // This was a new node so Add a NodeToBus element ????
                        AddANodeBus();   // Allocates more memory if necessary
                        MapNodeToBus[static_cast<size_t>(NumNodes) - 1].BusRef = result;
                        MapNodeToBus[static_cast<size_t>(NumNodes) - 1].NodeNum = NodeBuffer[i - 1];
                    }
                    NodeBuffer[i - 1] = NodeRef;  //  Swap out in preparation to setnoderef call
                }
            }
        }
        return result;
    }

    //----------------------------------------------------------------------------



    void TDSSCircuit::AddDeviceHandle(int Handle)
    {
        if (NumDevices > MaxDevices)
        {
            MaxDevices = MaxDevices + IncDevices;
            DeviceRef.resize(static_cast<size_t>(MaxDevices) + 1);
        }
        DeviceRef[static_cast<size_t>(NumDevices) - 1].devHandle = Handle;
        DeviceRef[static_cast<size_t>(NumDevices) - 1].CktElementClass = LastClassReferenced[ActiveActor];
    }


    //----------------------------------------------------------------------------



    int TDSSCircuit::SetElementActive(const String FullObjectName)

        // Fast way to set a cktelement active

    {
        int result = 0;
        int Devindex = 0;
        int DevClassIndex = 0;
        String DevType, DevName;
        result = 0;
        ParseObjectClassandName(FullObjectName, DevType, DevName);
        DevClassIndex = ClassNames[ActiveActor].Find(DevType);
        if (DevClassIndex == 0)
            DevClassIndex = LastClassReferenced[ActiveActor];
        if (!DevName.empty())
        {
            Devindex = DeviceList.Find(DevName) - 1;
            while (Devindex >= 0)
            {
                if (DeviceRef[Devindex].CktElementClass == DevClassIndex)   // we got a match
                {
                    ActiveDSSClass[ActiveActor] = (TDSSClass*)DSSClassList[ActiveActor].Get(DevClassIndex);
                    LastClassReferenced[ActiveActor] = DevClassIndex;
                    result = DeviceRef[Devindex].devHandle;
                    // ActiveDSSClass[ActiveActor].Active := Result;
                   //  ActiveCktElement := ActiveDSSClass.GetActiveObj;
                    Set_ActiveCktElement((TDSSCktElement*)(CktElements.Get(result)));
                    break;
                }
                Devindex = DeviceList.FindNext() - 1;   // Could be duplicates
            }
        }
        CmdResult = result;
        return result;
    }

    //----------------------------------------------------------------------------



    void TDSSCircuit::Set_ActiveCktElement(TDSSCktElement* Value)
    {
        FActiveCktElement = Value;
        ActiveDSSObject[ActiveActor] = Value;
    }

    //----------------------------------------------------------------------------



    void TDSSCircuit::AddCktElement(int Handle)
    {

        // Update lists that keep track of individual circuit elements
        NumDevices++;

        // Resize DeviceList if no. of devices greatly exceeds allocation
        if (((unsigned int)NumDevices) > 2 * DeviceList.InitialAllocation)
            ReallocDeviceList(ActiveActor);
        DeviceList.Add(((TDSSObject*)get_FActiveCktElement())->get_Name());
        CktElements.Add(get_FActiveCktElement());

        /*Build Lists of PC and PD elements*/
        switch (((TDSSObject*)get_FActiveCktElement())->DSSObjType & BaseClassMask)
        {
        case PD_ELEMENT:	PDElements.Add(get_FActiveCktElement());
            break;
        case PC_ELEMENT:	PCElements.Add(get_FActiveCktElement());
            break;
        case CTRL_ELEMENT:	DSSControls.Add(get_FActiveCktElement());
            break;
        case METER_ELEMENT:	MeterElements.Add(get_FActiveCktElement());
            break;
        default: {}
               /*Nothing*/
        }
        /*Build  lists of Special elements and generic types*/
        switch (((TDSSObject*)get_FActiveCktElement())->DSSObjType & CLASSMASK)
        {
        case MON_ELEMENT: 	Monitors.Add(get_FActiveCktElement());
            break;
        case ENERGY_METER:	EnergyMeters.Add(get_FActiveCktElement());
            break;
        case SENSOR_ELEMENT:	Sensors.Add(get_FActiveCktElement());
            break;
        case GEN_ELEMENT:   Generators.Add(get_FActiveCktElement());
            break;
        case SOURCE:	Sources.Add(get_FActiveCktElement());
            break;
        case CAP_CONTROL:   CapControls.Add(get_FActiveCktElement());
            break;
        case SWT_CONTROL:   SwtControls.Add(get_FActiveCktElement());
            break;
        case REG_CONTROL:   RegControls.Add(get_FActiveCktElement());
            break;
        case LOAD_ELEMENT:   Loads.Add(get_FActiveCktElement());
            break;
        case CAP_ELEMENT:   ShuntCapacitors.Add(get_FActiveCktElement());
            break;
        case REACTOR_ELEMENT:   Reactors.Add(get_FActiveCktElement());
            break;
        case RELAY_CONTROL:	Relays.Add(get_FActiveCktElement());
            break;
        case FUSE_CONTROL:	Fuses.Add(get_FActiveCktElement());
            break;
        case RECLOSER_CONTROL:	Reclosers.Add(get_FActiveCktElement());
            break;
        case FMON_ELEMENT:	FMonitors.Add(get_FActiveCktElement());
            break;
        case WINDGEN_ELEMENT:	WindGens.Add(get_FActiveCktElement());
            break;

            /* Keep Lines, Transformer, and Lines and Faults in PDElements and separate lists
              so we can find them quickly.*/
        case AUTOTRANS_ELEMENT:	AutoTransformers.Add(get_FActiveCktElement());
            break;
        case XFMR_ELEMENT:	Transformers.Add(get_FActiveCktElement());
            break;
        case LINE_ELEMENT:	Lines.Add(get_FActiveCktElement());
            break;
        case FAULTOBJECT:	Faults.Add(get_FActiveCktElement());
            break;
        case FEEDER_ELEMENT:	Feeders.Add(get_FActiveCktElement());
            break;
        case STORAGE_ELEMENT:	StorageElements.Add(get_FActiveCktElement());
            break;
            //       STORAGE2_ELEMENT:Storage2Elements.Add(ActiveCktElement);

        case PVSYSTEM_ELEMENT:	PVSystems.Add(get_FActiveCktElement());
            break;
            //       PVSYSTEM2_ELEMENT:PVSystems2.Add(ActiveCktElement);
            //       INV_CONTROL      :InvControls.Add(ActiveCktElement);

        case INV_CONTROL2:	InvControls2.Add(get_FActiveCktElement());
            break;
        case EXP_CONTROL:	ExpControls.Add(get_FActiveCktElement());
            break;
        }

        // AddDeviceHandle(Handle); // Keep Track of this device result is handle
        AddDeviceHandle(CktElements.get_myNumList()); // Handle is global index into CktElements
        get_FActiveCktElement()->Set_Handle(CktElements.get_myNumList());
    }

    void TDSSCircuit::DoResetMeterZones(int ActorID)
    {

        /* Do this only if meterzones unlocked .  Normally, Zones will remain unlocked
          so that all changes to the circuit will result in rebuilding the lists*/
        if (!MeterZonesComputed || !ZonesLocked)
        {
            if (LogEvents)
                LogThisEvent("Resetting Meter Zones", ActorID);
            EnergyMeterClass[ActorID]->ResetMeterZonesAll(ActorID);
            MeterZonesComputed = true;
            if (LogEvents)
                LogThisEvent("Done Resetting Meter Zones", ActorID);
        }
        FreeTopology();
    }

    //----------------------------------------------------------------------------



    void TDSSCircuit::SaveBusInfo()
    {
        int i = 0;

        /*Save existing bus definitions and names for info that needs to be restored*/
        SavedBuses.resize(NumBuses);
        if (SavedBusNames != NULL)
            delete[] SavedBusNames;
        SavedBusNames = new String[NumBuses];
        for (int stop = NumBuses, i = 1; i <= stop; i++)
        {
            SavedBuses[static_cast<size_t>(i) - 1] = Buses[static_cast<size_t>(i) - 1];
            (SavedBusNames)[i - 1] = BusList.Get(i);
        }
        SavedNumBuses = NumBuses;
    }

    //----------------------------------------------------------------------------



    void TDSSCircuit::RestoreBusInfo()
    {
        int i = 0, j = 0, idx = 0, jdx = 0;

        // Restore  kV bases, other values to buses still in the list
        for (int stop = SavedNumBuses, i = 1; i <= stop; i++)
        {
            idx = BusList.Find((SavedBusNames)[i - 1]);
            if (idx != 0)
                /*# with Buses^[idx - 1] do */
            {
                auto with0 = Buses[static_cast<size_t>(idx) - 1];
                {
                    TDSSBus& pBus(*(SavedBuses[static_cast<size_t>(i) - 1]));
                    with0->kVBase = pBus.kVBase;
                    with0->x = pBus.x;
                    with0->y = pBus.y;
                    with0->CoordDefined = pBus.CoordDefined;
                    with0->lat = pBus.lat;
                    with0->longitude = pBus.longitude;
                    with0->GISCoordDefined = pBus.GISCoordDefined;
                    with0->Keep = pBus.Keep;
                    /*Restore Voltages in new bus def that existed in old bus def*/
                    if (!(pBus.VBus.empty()))
                    {
                        for (int stop = pBus.get_FNumNodesThisBus(), j = 1; j <= stop; j++)
                        {
                            jdx = with0->FindIdx(pBus.GetNum(j));  // Find index in new bus for j-th node  in old bus
                            if (jdx > 0)
                                with0->VBus[jdx - 1] = pBus.VBus[j - 1];
                        }
                    }
                }
            }
            (SavedBusNames)[i - 1] = ""; // De-allocate string
        }
        if ((SavedBuses.size() != 0))
            for (int stop = SavedNumBuses, i = 1; i <= stop; i++)
                delete SavedBuses[i - 1];  // gets rid of old bus voltages, too
        //ReallocMem( SavedBuses, 0 );
        //ReallocMem( SavedBusNames, 0 );
    }

    //----------------------------------------------------------------------------



    void TDSSCircuit::ReProcessBusDefs(int ActorID)

        // Redo all Buslists, nodelists

    {
        TDSSCktElement* CktElementSave;
        TDSSCktElement* CktElementTmp;
        int i = 0;
        if (LogEvents)
            LogThisEvent("Reprocessing Bus Definitions", ActorID);
        AbortBusProcess = false;
        SaveBusInfo();  // So we don't have to keep re-doing this
        // Keeps present definitions of bus objects until new ones created

        // get rid of old bus lists
        BusList = THashList(NumDevices);  // won't have many more buses than this
        NumBuses = 0;  // Leave allocations same, but start count over
        NumNodes = 0;

        // Now redo all enabled circuit elements
        CktElementSave = get_FActiveCktElement();
        Set_ActiveCktElement((TDSSCktElement*)CktElements.Get_First());
        while (get_FActiveCktElement() != NULL)
        {
            if (get_FActiveCktElement()->Get_Enabled())
                ProcessBusDefs(ActorID);
            if (AbortBusProcess)
                return;
            Set_ActiveCktElement((TDSSCktElement*)CktElements.Get_Next());
            CktElementTmp = get_FActiveCktElement();
        }
        Set_ActiveCktElement(CktElementSave);  // restore active circuit element
        for (int stop = NumBuses, i = 1; i <= stop; i++)
            Buses[static_cast<size_t>(i) - 1]->AllocateBusVoltages();
        for (int stop = NumBuses, i = 1; i <= stop; i++)
            Buses[static_cast<size_t>(i) - 1]->AllocateBusVoltages();
        RestoreBusInfo();     // frees old bus info, too
        DoResetMeterZones(ActorID);  // Fix up meter zones to correspond
        Set_BusNameRedefined(false);  // Get ready for next time
    }

    //----------------------------------------------------------------------------



    void TDSSCircuit::Set_BusNameRedefined(bool Value)
    {
        FBusNameRedefined = Value;
        if (Value)
        {
            Solution->SystemYChanged = true;  // Force Rebuilding of SystemY if bus def has changed
            Control_BusNameRedefined = true;  // So controls will know buses redefined
        }
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    complex TDSSCircuit::Get_Losses(int ActorID)
    {
        complex result;
        TPDElement* PDElem;

        /*Return total losses in all PD Elements*/
        PDElem = (TPDElement*)PDElements.Get_First();
        result = CZero;
        while (PDElem != NULL)
        {
            if (PDElem->Get_Enabled())
            {
                /*Ignore Shunt Elements*/
                if (!PDElem->IsShunt)
                    caccum(result, PDElem->Get_Losses(ActorID));
            }
            PDElem = (TPDElement*)PDElements.Get_Next();
        }
        return result;
    }
    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCircuit::DebugDump(Textfile& F)
    {
        int i = 0, j = 0;
        System::Write(F, "NumBuses= "); WriteLn(NumBuses, 0);
        System::Write(F, "NumNodes= "); WriteLn(NumNodes, 0);
        System::Write(F, "NumDevices= "); WriteLn(NumDevices, 0);
        System::WriteLn(F, "BusList:");
        for (int stop = NumBuses, i = 1; i <= stop; i++)
        {
            System::Write(F, "  "); Write(F, Pad(BusList.Get(i), 12));
            System::Write(F, " (");
            Write(F, Buses[static_cast<size_t>(i) - 1]->get_FNumNodesThisBus(), 0);
            Write(F, " Nodes)");
            for (int stop = Buses[static_cast<size_t>(i) - 1]->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
            {
                System::Write(F, ' ');
                Write(Buses[static_cast<size_t>(i) - 1]->GetNum(j), 0);
            }
            WriteLn(F);
        }
        System::WriteLn(F, "DeviceList:");
        for (int stop = NumDevices, i = 1; i <= stop; i++)
        {
            System::Write(F, "  "); Write(F, Pad(DeviceList.Get(i), 12));
            Set_ActiveCktElement((TDSSCktElement*)CktElements.Get(i));
            if (!get_FActiveCktElement()->Get_Enabled())
            {
                System::Write(F, "  DISABLED");
            }
            WriteLn(F);
        }
        System::WriteLn(F, "NodeToBus Array:");
        for (int stop = NumNodes, i = 1; i <= stop; i++)
        {
            j = MapNodeToBus[static_cast<size_t>(i) - 1].BusRef;
            System::Write(F, "  ");
            System::Write(F, i, 2);
            System::Write(F, ' ');
            System::Write(F, j, 2);
            System::Write(F, " (=");
            System::Write(F, BusList.Get(j));
            System::Write(F, ".");
            System::Write(F, MapNodeToBus[static_cast<size_t>(i) - 1].NodeNum, 0);
            System::Write(F, ")");
            WriteLn(F);
        }
    }

    //- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -



    void TDSSCircuit::InvalidateAllPCElements()
    {
        TDSSCktElement* p;
        p = (TDSSCktElement*)PCElements.Get_First();
        while (p != NULL)
        {
            p->Set_YprimInvalid(ActiveActor, true);
            p = (TDSSCktElement*)PCElements.Get_Next();
        }
        Solution->SystemYChanged = true;  // Force rebuild of matrix on next solution
    }


    // - - ------------------------------------------------------



    void TDSSCircuit::Set_LoadMultiplier(double Value)
    {
        if (Value != FLoadMultiplier)   // We may have to change the Y matrix if the load multiplier  has changed
            switch (Solution->LoadModel)
            {
            case ADMITTANCE:
                InvalidateAllPCElements();
                break;
            default: {}
                   /*nada*/
            }
        FLoadMultiplier = Value;
    }


    void TDSSCircuit::TotalizeMeters()

        /* Totalize all energymeters in the problem*/
    {
        TEnergyMeterObj* pEM;
        int i = 0;
        for (int stop = NumEMRegisters, i = 1; i <= stop; i++)
            RegisterTotals[i - 1] = 0.;
        pEM = (TEnergyMeterObj*)EnergyMeters.Get_First();
        while (pEM != NULL)
            /*# with pEM do */
        {
            for (int stop = NumEMRegisters, i = 1; i <= stop; i++)
                RegisterTotals[i - 1] = RegisterTotals[i - 1] + pEM->Registers[i - 1] * pEM->TotalsMask[i - 1];
            pEM = (TEnergyMeterObj*)EnergyMeters.Get_Next();
        }
    }

    //----------------------------------------------------------------------------

    double TDSSCircuit::get_FLoadMultiplier()
    {
        return FLoadMultiplier;
    }

    //----------------------------------------------------------------------------

    bool TDSSCircuit::get_FBusNameRedefined()
    {
        return FBusNameRedefined;
    }

    //----------------------------------------------------------------------------

    std::string TDSSCircuit::get_FCaseName()
    {
        return FCaseName;
    }

    //----------------------------------------------------------------------------

    double SumSelectedRegisters(const TRegisterArray mtrRegisters, pIntegerArray Regs, int count)
    {
        double result = 0.0;
        int i = 0;
        result = 0.0;
        for (int stop = count, i = 1; i <= stop; i++)
        {
            result = result + mtrRegisters[(Regs)[i - 1]];
        }
        return result;
    }


    bool TDSSCircuit::ComputeCapacity(int ActorID)
    {
        bool result = false;
        bool CapacityFound = false;
        result = false;
        if (EnergyMeters.get_myNumList() == 0)
        {
            DoSimpleMsg("Cannot compute system capacity with EnergyMeter objects!", 430);
            return result;
        }
        if (NumUEregs == 0)
        {
            DoSimpleMsg("Cannot compute system capacity with no UE resisters defined.  Use SET UEREGS=(...) command.", 431);
            return result;
        }
        Solution->Set_Mode(SNAPSHOT);
        Set_LoadMultiplier(CapacityStart);
        CapacityFound = false;
        do
        {
            EnergyMeterClass[ActorID]->ResetAll(ActorID);
            Solution->Solve(ActorID);
            EnergyMeterClass[ActorID]->SampleAll(ActorID);
            TotalizeMeters();

            // Check for non-zero in UEregs
            if (SumSelectedRegisters(RegisterTotals, UEregs, NumUEregs) != 0.0)
                CapacityFound = true;
            // LoadMultiplier is a property ...
            if (!CapacityFound)
                Set_LoadMultiplier(get_FLoadMultiplier() + CapacityIncrement);
        } while (!((get_FLoadMultiplier() > 1.0) || CapacityFound));

        if (get_FLoadMultiplier() > 1.0)
            Set_LoadMultiplier(1.0);
        result = true;
        return result;
    }

    /*Save the present circuit - Enabled devices only*/
    bool TDSSCircuit::Save(String Dir)
    {
        bool result = false;
        int i = 0;
        bool Success = false;
        String CurrDir, SaveDir;
        result = false;

        // Make a new subfolder in the present folder based on the circuit name and
        // a unique sequence number
        SaveDir = GetCurrentDir();  // remember where to come back to
        Success = false;
        if (Dir.size() == 0)
        {
            Dir = Get_Name();
            CurrDir = Dir;
            for (int stop = 999, i = 0; i <= stop; i++)  // Find a unique dir name
            {
                if (!DirectoryExists(CurrDir))
                {
                    if (CreateDir(CurrDir))
                    {
                        SetCurrentDir(CurrDir);
                        Success = true;
                        break;
                    }
                }
                CurrDir = Dir + Format("%.3d", i);
            }
        }
        else
        {
            if (!DirectoryExists(Dir))
            {
                CurrDir = Dir;
                if (CreateDir(CurrDir))
                {
                    SetCurrentDir(CurrDir);
                    Success = true;
                }
            }
            else
            {  // Exists - overwrite
                CurrDir = Dir;
                SetCurrentDir(CurrDir);
                Success = true;
            }
        }
        if (!Success)
        {
            DoSimpleMsg(String("Could not create a folder \"") + Dir + "\" for saving the circuit.", 432);
            return result;
        }
        SavedFileList[ActiveActor].clear();  /*This list keeps track of all files saved*/

        // Initialize so we will know when we have saved the circuit elements
        for (int stop = CktElements.get_myNumList(), i = 1; i <= stop; i++)
            ((TDSSObject*)(CktElements.Get(i)))->HasBeenSaved = false;

        // Initialize so we don't save a class twice
        for (int stop = DSSClassList[ActiveActor].get_myNumList(), i = 1; i <= stop; i++)
            ((TDSSClass*)(DSSClassList[ActiveActor].Get(i)))->Saved = false;

        /*Ignore Feeder Class -- gets saved with Energymeters*/
       // FeederClass.Saved := TRUE;  // will think this class is already saved

        /*Define voltage sources first*/
        Success = WriteVsourceClassFile((*(TDSSClass*)GetDSSClassPtr("vsource")), true);
        /*Write library files so that they will be available to lines, loads, etc*/
        /*Use default filename=classname*/
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("wiredata")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("cndata")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("tsdata")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("linegeometry")), "", false);
        // If Success Then Success :=  WriteClassFile(GetDssClassPtr('linecode'),'', FALSE);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("linespacing")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("linecode")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("xfmrcode")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("loadshape")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("TShape")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("priceshape")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("growthshape")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("XYcurve")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("TCC_Curve")), "", false);
        if (Success)
            Success = WriteClassFile((*(TDSSClass*)GetDSSClassPtr("Spectrum")), "", false);
        if (Success)
            Success = SaveFeeders(); // Save feeders first
        if (Success)
            Success = SaveDSSObjects();  // Save rest ot the objects
        if (Success)
            Success = SaveVoltageBases();
        if (Success)
            Success = SaveBusCoords();
        if (Success)
            Success = SaveGISCoords();
        if (Success)
            Success = SaveMasterFile();
        /*
              If Success Then DoSimpleMsg('Circuit saved in directory: ' + GetCurrentDir, 433)
                       Else DoSimpleMsg('Error attempting to save circuit in ' + GetCurrentDir, 434);
            */
        if (Success)
            GlobalResult = GetCurrentDir() + DIRSEP_STR "Master.DSS";
        else
            GlobalResult = "Error 434 attempting to save circuit in " + GetCurrentDir();

        // Return to Original directory
        SetCurrentDir(SaveDir);
        result = true;
        return result;
    }

    //----------------------------------------------------------------------------



    bool TDSSCircuit::SaveDSSObjects()
    {
        bool result = false;
        TDSSClass* Dss_Class;
        int i = 0;
        result = false;

        // Write Files for all populated DSS Classes  Except Solution Class
        for (int stop = DSSClassList[ActiveActor].get_myNumList(), i = 1; i <= stop; i++)
        {
            Dss_Class = (TDSSClass*)DSSClassList[ActiveActor].Get(i);
            if ((Dss_Class == SolutionClass[ActiveActor]) || Dss_Class->Saved)
                continue;   // Cycle to next
            /*use default filename=classname*/
            if (!WriteClassFile(*Dss_Class, "", (dynamic_cast<TCktElementClass*>(Dss_Class))))
                return result;  // bail on error
            Dss_Class->Saved = true;
        }
        result = true;
        return result;
    }
    


    bool TDSSCircuit::SaveVoltageBases()
    {
        bool result = false;
        TTextRec F;
        int i = 0;
        String VBases;
        result = false;
        try
        {
            AssignFile(F, "BusVoltageBases.DSS");
            Rewrite(F);
            IOResultToException();

            //        For i := 1 to NumBuses do
            //          If Buses^[i].kVBase > 0.0 Then
            //            Writeln(F, Format('SetkVBase Bus=%s  kvln=%.7g ', [BusList.Get(i), Buses^[i].kVBase]));
            DSSExecutive[ActiveActor]->Set_Command("get voltagebases");
            VBases = GlobalResult;
            WriteLn(F, String("Set Voltagebases=") + VBases);
            WriteLn(F, "CalcVoltagebases");
            CloseFile(F);
            result = true;
        }
        catch (exception& E)
        {
            DoSimpleMsg("Error Saving BusVoltageBases File: " + (std::string)E.what(), 43501);
        }
        return result;
    }
   


    bool TDSSCircuit::SaveMasterFile()
    {
        bool result = false;
        TTextRec F;
        int i = 0;
        result = false;
        try
        {
            AssignFile(F, "Master.DSS");
            Rewrite(F);
            IOResultToException();
            WriteLn(F, "Clear");
            WriteLn(F, String("New Circuit.") + Get_Name());
            WriteLn(F);
            if (PositiveSequence)
                WriteLn(F, "Set Cktmodel=Positive");
            if (DuplicatesAllowed)
                WriteLn(F, "set allowdup=yes");
            WriteLn(F);

            // Write Redirect for all populated DSS Classes  Except Solution Class
            for (int stop = SavedFileList[ActiveActor].size(), i = 1; i <= stop; i++)
            {
                Write(F, "Redirect "); WriteLn(F, SavedFileList[ActiveActor][static_cast<size_t>(i) - 1]);
            }
            WriteLn(F, "MakeBusList");
            WriteLn(F, "Redirect BusVoltageBases.dss  ! set voltage bases");
            if (FileExists("buscoords.dss"))
            {
                WriteLn(F, "Buscoords buscoords.dss");
            }
            if (FileExists("GIScoords.dss"))
            {
                WriteLn(F, "GIScoords GIScoords.dss");
            }
            CloseFile(F);
            result = true;
        }
        catch (exception& E)
        {
            DoSimpleMsg("Error Saving Master File: " + (std::string)E.what(), 435);
        }
        return result;
    }


    bool TDSSCircuit::SaveFeeders()
    {
        bool result = false;
        int i = 0;
        String SaveDir, CurrDir;
        TEnergyMeterObj* Meter;
        result = true;
        /*Write out all energy meter  zones to separate subdirectories*/
        SaveDir = GetCurrentDir();
        for (int stop = EnergyMeters.get_myNumList(), i = 1; i <= stop; i++)
        {
            Meter = ((TEnergyMeterObj*)EnergyMeters.Get(i)); // Recast pointer
            CurrDir = ((TDSSObject*)Meter)->get_Name();
            if (((TDSSCktElement*)Meter)->Get_Enabled())         // Only active meters
            {
                if (DirectoryExists(CurrDir))
                {
                    SetCurrentDir(CurrDir);
                    Meter->SaveZone(CurrDir);
                    SetCurrentDir(SaveDir);
                }
                else
                {
                    if (CreateDir(CurrDir))
                    {
                        SetCurrentDir(CurrDir);
                        Meter->SaveZone(CurrDir);
                        SetCurrentDir(SaveDir);
                    }
                    else
                    {
                        DoSimpleMsg(String("Cannot create directory: ") + CurrDir, 436);
                        result = false;
                        SetCurrentDir(SaveDir);  // back to whence we came
                        break;
                    }
                }
            }
        }  /*For*/
        return result;
    }


    bool TDSSCircuit::SaveBusCoords()
    {
        bool result = false;
        TTextRec F;
        int i = 0;
        result = false;
        try
        {
            AssignFile(F, "BusCoords.dss");
            Rewrite(F);
            IOResultToException();
            for (int stop = NumBuses, i = 1; i <= stop; i++)
            {
                if (Buses[static_cast<size_t>(i) - 1]->CoordDefined)
                {
                    Write(F, CheckForBlanks(BusList.Get(i))); WriteLn(F, Format(", %-g, %-g", Buses[static_cast<size_t>(i) - 1]->x, Buses[static_cast<size_t>(i) - 1]->y));
                }
            }
            CloseFile(F);
            result = true;
        }
        catch (std::exception &E)
        {
            DoSimpleMsg("Error creating Buscoords.dss.", 437);
        }
        return result;
    }

    //----------------------------------------------------------------------------



    bool TDSSCircuit::SaveGISCoords()
    {
        bool result = false;
        TTextRec F;
        int i = 0;
        result = false;
        try
        {
            AssignFile(F, "GISCoords.dss");
            Rewrite(F);
            IOResultToException();
            for (int stop = NumBuses, i = 1; i <= stop; i++)
            {
                if (Buses[static_cast<size_t>(i) - 1]->CoordDefined)
                {
                    Write(F, CheckForBlanks(BusList.Get(i))); WriteLn(F, Format(", %-g, %-g", Buses[static_cast<size_t>(i) - 1]->lat, Buses[static_cast<size_t>(i) - 1]->longitude));
                }
            }
            CloseFile(F);
            result = true;
        }
        catch (std::exception &E)
        {
            DoSimpleMsg("Error creating Buscoords.dss.", 437);
        }
        return result;
    }


    void TDSSCircuit::ReallocDeviceList(int ActorID)
    {
        THashList TempList;
        int i = 0;
        /*Reallocate the device list to improve the performance of searches*/
        if (LogEvents)
            LogThisEvent("Reallocating Device List", ActorID);
        TempList = THashList(2 * NumDevices);
        for (int stop = DeviceList.Get_NumElements(), i = 1; i <= stop; i++)
        {
            TempList.Add(DeviceList.Get(i));
        }
        DeviceList = TempList;
    }


    void TDSSCircuit::Set_CaseName(const String Value)
    {
        FCaseName = Value;
        CircuitName_[ActiveActor] = Value + "_";
    }


    String TDSSCircuit::Get_Name()
    {
        String result;
        result = Get_myLName();
        return result;
    }


    TAdjArray TDSSCircuit::GetBusAdjacentPDLists(int ActorID)
    {
        TAdjArray result;
        if (BusAdjPD.empty())
            BuildActiveBusAdjacencyLists(BusAdjPD, BusAdjPC, ActorID);
        result = BusAdjPD;

        if (BusAdjPD.empty())
            BuildActiveBusAdjacencyLists(BusAdjPD, BusAdjPC, ActorID);
        result = BusAdjPD;

        return result;
    }


    TAdjArray TDSSCircuit::GetBusAdjacentPCLists(int ActorID)
    {
        TAdjArray result;
        if (BusAdjPC.empty())
            BuildActiveBusAdjacencyLists(BusAdjPD, BusAdjPC, ActorID);
        result = BusAdjPC;
        return result;
    }


    TCktTree& TDSSCircuit::GetTopology()
    {
        int i = 0;
        TDSSCktElement* elem;
        if (!(Branch_List != NULL))
        {
            /*Initialize all Circuit Elements and Buses to not checked, then build a new tree*/
            elem = (TDSSCktElement*)CktElements.Get_First();
            while ((elem != NULL))
            {
                elem->Checked = false;
                for (int stop = elem->Get_NTerms(), i = 1; i <= stop; i++)
                    elem->Terminals[static_cast<size_t>(i) - 1].Checked = false;
                elem->IsIsolated = true; // till proven otherwise
                elem = (TDSSCktElement*)CktElements.Get_Next();
            }
            for (int stop = NumBuses, i = 1; i <= stop; i++)
                Buses[static_cast<size_t>(i) - 1]->BusChecked = false;
            Branch_List = GetIsolatedSubArea((TDSSCktElement*)Sources.Get_First(), true);  // calls back to build adjacency lists
        }
        return *Branch_List;
    }


    void TDSSCircuit::FreeTopology()
    {
        if ((Branch_List != NULL))
            delete Branch_List;
        Branch_List = NULL;
        if ((BusAdjPC.empty()))
            FreeAndNilBusAdjacencyLists(BusAdjPD, BusAdjPC);
    }


    void TDSSCircuit::ClearBusMarkers()
    {
        int i = 0;
        for (int stop = BusMarkerList.size(), i = 1; i <= stop; i++)
            free((TBusMarker*)BusMarkerList[static_cast<size_t>(i) - 1]);
        BusMarkerList.clear();
    }

    /*====================================================================*/
    /* TBusMarker */
    /*====================================================================*/


    TBusMarker::TBusMarker()
        : BusName(""),
        AddMarkerColor(0),
        AddMarkerCode(4),
        AddMarkerSize(1)
    {
        // inherited::Create();
    }


    TBusMarker::~TBusMarker()
    {
        BusName = "";
        // todo check:  inherited::Destroy();
    }

} //namespace Circuit







