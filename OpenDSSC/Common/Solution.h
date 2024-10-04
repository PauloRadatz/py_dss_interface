#ifndef SolutionH
#define SolutionH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/* Change Log
 8-14-99 Added progress display and abort on longer solution types
 11-3-99 added calc voltage base
 11-21-99 modified to  calc the voltage bases at the current load level set by the user.
 12-1-99 Added code to estimate starting point for P-V Generators
 12-2-99 Made more properties visible
 12-6-99 Merged properties with Set Command and removed from here
 12-15-99 Added global generatordispatchreference
 1-8-00   Fixed bug in autoadd generators to work with new generator model
          set vminpu=0 and vmaxpu=1000000
 1-30-00 to 2-1-00 Implemented control action check in solution
 2-19-00 Frequency changed reset to FALSE after being used (was causing all YPrims to be recomputed)
 2-23-00 Modified so that reset of meters and monitors is done upon setting the solution mode property.
         After that the user must reset else the monitors just accumulate.
 3-20-00 Fixed bug with setting generator disp reference - made uniform for all types
 6-11-00 Split into two modules + moved auto add stuff to AutoAdd
 9-20-00 Added Dynamic Mode
 10-25-00 Added Fundamental Freq and other stuff for Harmonics Solution
 5-30-01  Added control iterations check, mostIterationsdone.
          Fixed bug with controls off doing the solution too many times.

 8-14-01 Reset IntervalHrs on Mode change
 7-11-02 Added check for system Y change after computing currents

 9-28-03 Redefined V to NodeV and changed from an array from 1..n to 0..n where
         0-th element is alway ground(complex zero volts).
 8-14-06 Revised power flow initialization; removed forward/backward sweep

 9-14-16 Added SampleTheMeters Flag to allow sampling energy meters in Time and DutyCycle mode

*/


#include "System.h"
#include "Sysutils.h"
#ifndef windows
#include <time.h> // Assume POSIX so we have clock_gettime
static inline BOOL QueryPerformanceCounter(struct timespec *t) {
	// Emulate MS Windows function with clock_gettime.  The MS function
	// returns 0 on failure, and nonzero on success, but clock_gettime
	// returns 0 on success.
	return 0==clock_gettime(CLOCK_MONOTONIC, t);
}
static inline double operator-(const struct timespec &a, const struct timespec &b) {
	//a.tv_sec + a.tv_nsec/1e9 - ( b.tv_sec + b.tv_nsec/1e9 )
	//a.tv_sec - b.tv_sec + (a.tv_nsec - b.tv_nsec)/1e9
	return double(a.tv_sec - b.tv_sec) + double(a.tv_nsec - b.tv_nsec)/1e9;
}
#endif

#include "Ucomplex.h"
#include "Arraydef.h"
#include "Command.h"
#include "Monitor.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Dynamics.h"
#include "EnergyMeter.h"
#include "VSource.h"
#include "Isource.h"

#include "Sparse_Math.h"

#include "CktElement.h"
#include <string>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <functional>
#include "klusolve.h" // klusparseset_t
#include <queue>


namespace Solution
{

    class EControlProblem;
    class ESolveError;
    class TDSSSolution;
    struct TLDs_sys_fms;
    class TSolutionObj;
    class TSolver;


    const int NORMALSOLVE   = 0;
    const int NEWTONSOLVE   = 1;
    const int NCIMSOLVE     = 2;
    // Constants for the actor's messaging

    const int SIMULATE = 0;
    const int EXIT_ACTOR = 1;
    const int INIT_ADIAKOPTICS = 2;  // Initializes the environment for the children actors
    const int SOLVE_AD1 = 3;  // solves for the actors with Power Injections -> E(0)
    const int SOLVE_AD2 = 4;  // Solves the sub-system and adds to the partial solution
    const int CALC_INJ_CURR = 5;  // Uses the total solution to estiamte the injection currents
    const int DO_CTRL_ACTIONS = 6;  // Does the control actions distributedly
    const int ZEROIVECTOR = 8;  // Zeroes the actor's I std::vector
    const int GETCURRINJ = 9;  // Gets the current injections for the actor and uploades them in the local I std::vector
    const int CHECKYBUS = 10; // Rebuilds the YBus if needed at local level
    const int CHECK_FAULT = 11; // Checks the fault status at local level
    const int GETCTRLMODE = 12; // Sync the local control mode with actor 1
    const int ALL_ACTORS = 0; // Wait flag for all the actors
    const int AD_ACTORS = 1; // Wait flag to wait only for the A-Diakoptics actors

    // Constants for the NCIM solution algorithm
    const int PQ_Node   = 0; // For indicating if the node is PQ (NCIM solver)
    const int PV_Node   = 1; // For indicating if the node is PV (NCIM solver)

    class EControlProblem : public std::runtime_error {
        typedef std::runtime_error inherited;
        EControlProblem(const String &Msg);
    };


    class ESolveError : public std::runtime_error {
        typedef std::runtime_error inherited;
        ESolveError(const String &Msg);
    };  // Raised when solution aborted



    typedef complex* TNodeVarray;
    typedef complex* pNodeVarray;
    ///////////////////////////////////
      /*define LD_FM_Arry-by dahei*/

#pragma pack(push, 1)
     //properties for Nodes
           // highest voltage node
    struct TLDs_sys_fms {
        int clstr_num_hghst;
        int ndnum_hghst;
        bool b_ctrl_hghst; //can contribute more to the high volt problem
        double volt_hghst;    //p.u.
        double volt_hgh_lmt;  //p.u.
        double Pinjec_hghst; //net P injection on this node
                 // lowest voltage node
        int clstr_num_lwst;
        int ndnum_lwst;
        bool b_ctrl_lwst; //can contribute more to the high volt problem
        double volt_lwst;   //p.u.
        double volt_lw_lmt;  //p.u.
        double Pinjec_lwst; // net P injection on this node
    };
#pragma pack(pop)
    //////////////////////////////////////



    class TDSSSolution : public DSSClass::TDSSClass {
        typedef TDSSClass inherited;
        friend class EControlProblem;
        friend class ESolveError;
        friend class TSolutionObj;
        friend class TSolver;
//    private:
        //       CommandList:TCommandlist;
    protected:
        void DefineProperties();
    public:
        TDSSSolution();
        virtual ~TDSSSolution();
        virtual int Edit(int ActorID);
        virtual int Init(int Handle, int ActorID);
        virtual int NewObject(const String ObjName);
    };


    //typedef void ( __closure * TInfoMessageCall )( const String );  // Creates the procedure for sending a message

#ifndef NULL
#define NULL 0
#endif
    class TSolver    // pending to define thread behavior and inheriatance
    {
        //typedef TThread inherited;
        friend class EControlProblem;
        friend class ESolveError;
        friend class TDSSSolution;
        friend class TSolutionObj;

        class TMessageQueue
        {
            std::mutex m_mutex;
            std::condition_variable m_condVar;
            std::queue<int> m_queue;                     // A queue for messaging to actors, the aim is to reduce inconsistency
        public:
            // These lines prevent TMessageQueue objects from being copied:
            TMessageQueue(const TMessageQueue&) = delete;
            TMessageQueue& operator=(const TMessageQueue&) = delete;
            // Normal constructor:
            TMessageQueue() = default; // Let the compiler give us the default.
            int recv();
            void send(int);
        };
        TMessageQueue MyMessages;

    public:
        TSolver(int local_CPU, int ID) /*# overload */;
        virtual void Execute();
        virtual void Doterminate();
        virtual ~TSolver();

            //*******************************Private components*****************************
    protected:
        String FMessage, Msg_Cmd;
        //TInfoMessageCall UINotifier, FInfoProc;
        int ActorID, MsgType;
        TEvent UIEvent, ActorMsg;                          // used to know if the actors require a partial solution
        bool AD_Init, ActorActive, Processing;
        TThread ActorThread;
        void Start_Diakoptics();
        void Notify_Main();
        bool Get_Processing();
        void Set_Processing(bool Nval);
        int Get_CPU();
        void Set_CPU(int CPU);
        void IndexBuses();           // Locates the actor buses within the bus array in Actor 1 (interconnected)
        bool HasInjObj();    // returns true if the actor has natural injection objects
        void ZeroLocalV();           // Sets the local voltage std::vector (solution) equal to zero

      //*******************************Public components******************************
    public:
        void Send_Message(int Msg);
        void CallCallBack();
    };


    class TSolutionObj : public TDSSObject {
    public:
        typedef TDSSObject inherited;
        friend class EControlProblem;
        friend class ESolveError;
        friend class TDSSSolution;
        friend class TSolver;
        //  private:
        pNodeVarray dV;   // Array of delta V for Newton iteration
        double FFrequency;
        // //by Dahei
        unsigned int nNZ_yii;       //how many lines in Yii
        std::vector <longInt> pColIdx_Yii;
        std::vector <longInt> pRowIdx_Yii;//array of LongWord;  //cols and rows
        std::vector <complex> pcVals_Yii;   //vals of yii
        TSparse_Complex* MathYBus;          //  FOr operating the Y bus matrix using sparse algebra
             // =========
        bool Converged(int ActorID);
        bool OK_for_Dynamics(const int Value);
        bool OK_for_Harmonics(const int Value);
        void DoNewtonSolution(int ActorID);
        void DoNormalSolution(int ActorID);
        void DoNCIMSolution(int ActorID);
        //       PROCEDURE GetMachineInjCurrents;
        void SetGeneratordQdV(int ActorID);
        void SumAllCurrents(int ActorID);
        void Set_Frequency(const double Value);
        void Set_Mode(const int Value);
        int Get_SolMode();
        void Set_Year(const int Value);
        void Set_Total_Time(const double Value);
        //=============================================================================================================================================================
        /* NCIM algorithm rotuines and variables */
        vector <complex>        deltaZ,                                     // delta for Injection currents
                                deltaF,                                     // delta for Voltages
                                pNodePower,                                 // Array of complex storing the total power per node
                                pGenPower,                                  // Stores the total generation power per iteration
                                NCIMY,                                      // Stores the Non-zero values of the YBus Marix for multiplication
                                pNodeLimits;                                // Stores the total Q limits for PV buses using all the nodes in the model
        bool                    InitGenQ;                                   // Used to initialize variables the first time the algorithm runs or needs to be reinitialized
        vector <unsigned int>   NCIMYRow,                                   // Rows index of the Non-zero values of the Y Bus matrix
                                NCIMYCol;                                   // Cols index of the Non-zero values of Y
        vector <int>            pNodeType,                                  // Array with the node type (PQ/PV)
                                pNodeNumGen;                                // Stores the number of generators per node for further use
        vector <double>         pNodePVTarget;                              // Array with the target (voltage) of the PV Buses
        vector <int>            PVBusIdx,                                   // Stores the PVBus current index when indexing the jacobian matrix
                                PV2PQList;                                  // To list the generators converted from PV to PQ when the condition is forced
        klusparseset_t          Jacobian;                                   // Sparse Jacobian matrix
        bool                    NCIMRdy;                                    // Indicates if the NCIM environment and structures are initialized
        bool                    IgnoreQLimit;                               // To indicate if the user wants to ignore the Q limits for generators
        double                  GenGainNCIM;                                // Global gain for reactive power injection/absorption when using NCIM
        longInt                 NCIMNodes;                                  // Stores the number of nodes within the YBus matrix with only PDE
        void LoadYBusNCIM(int ActorID); // Loads the Y bus admittance matrix into another structure for linear algebra purposes
        void CalcInjCurr(int ActorID, bool InitGenQ);            // Calculates the injection currents using the actual voltages ( I = Y * V )
        int GetNumGenerators(int ActorID, bool InitQ);           // Gets and initializes all the generators in the model as PV buses
        void BuildJacobian(int ActorID);                         // BUilds the jacobian matrix
        int InitNCIM(int ActorID, bool InitY);                   // Host all the initialization routines for NCIM
        void DOForceFlatStart(int ActorID);                      // Forces the voltage vector to a flat start (magnitude only).
        void InitNCIMVectors(int ActorID);                       // Initializes the vectors for the node total power in NCIM
        void GetNCIMPowers(int ActorID);                         // Populate the total power vector before solving 
        // Apply the PV bus current injection for NCIM
        void DoPVBusNCIM(int ActorID, int i, double VTarget, complex Power); 
        // Apply the PQ bus current injection for NCIM
        void DoPQBusNCIM(int ActorID, int i, complex V, complex Power);
        // Apply the COnstant impedance bus current injection for NCIM
        void DoZBusNCIM(int ActorID, int i, complex V, TcMatrix* YPrim);

        void ApplyCurrNCIM(int ActorID);                                    // Apply the current injections before solving NCIM
        void UpdateGenQ(int ActorID);                            // Updates the reacitve power delta for all the generators in the model.
        void InitPQGen(int ActorID);                                        // Initializes the generators declared as PQ type
        void DistGenClusters(int ActorID);                       // Distributes the reactive power among clustered generators
        void ReversePQ2PV(int ActorID);                          // Reverses the generators converted from PV 2 PQ for the next solution 
        //=============================================================================================================================================================
    public:
        int Algorithm;      // NORMALSOLVE or NEWTONSOLVE
        pComplexArray AuxCurrents;// For injections like AutoAdd
        bool ControlActionsDone;
        int ControlIteration, ControlMode;      // EVENTDRIVEN, TIMEDRIVEN
        double ConvergenceTolerance;
        bool ConvergedFlag;
        int DefaultControlMode;      // EVENTDRIVEN, TIMEDRIVEN
        int DefaultLoadModel;      // 1=POWERFLOW  2=ADMITTANCE
        bool DoAllHarmonics;
        bool DynamicsAllowed;
        TDynamicsRec DynaVars;
        std::vector <double> ErrorSaved;
        bool FirstIteration, FrequencyChanged;      // Flag set to true if something has altered the frequency
        int Fyear;
        double Harmonic;
        pDoubleArray HarmonicList;
        int HarmonicListSize;                            /*Handle for main (system) Y matrix*/                            /*Handle for series Y matrix*/
        klusparseset_t hYsystem, hYseries, hY;   /*either hYsystem or hYseries*/
        double IntervalHrs;       // Solution interval since last solution, hrs.
        int Iteration, LoadModel;      // 1=POWERFLOW  2=ADMITTANCE
                           // Flag to allow sampling of EnergyMeters
        bool VoltageBaseChanged, SampleTheMeters, SeriesYInvalid, SolutionInitialized, SystemYChanged, UseAuxCurrents, PreserveNodeVoltages, IsDynamicModel, IsHarmonicModel, LastSolutionWasDirect, LoadsNeedUpdating;                           // Index of the actor within the interconnected model (ADiakoptics)
                             // Number of times to solve
                                //  0 = none; 1 = gaussian; 2 = UNIFORM
                             // Counter incremented for each solution
        bool DynamicsInitialized;
        int ActorVIdx, NumberOfTimes, RandomType, SolutionCount, MaxIterations, MinIterations, MostIterationsDone, MaxControlIterations;
        double MaxError;
        std::vector <double> NodeVbase;
        std::vector <double> VmagSaved;

        /*Voltage and Current Arrays*/                               // Main System Voltage Array   allows NodeV^[0]=0
        vector <complex> NodeV, Currents;  // Main System Currents Array

             /*A-Diakoptics variables*/                             // Used to store the partial solution voltage
        vector <complex> Node_dV, Ic_Local;  // Used to store the complementary curret

      //******************************************************************************
                                      // Incidence sparse matrix
        Tsparse_matrix IncMat, Laplacian; // Laplacian sparse matrix
             /*by Dahei for FMonitor*/
                    /*------------------*/
        std::vector <complex> NodeYii;         // Main System Y = G + jB, Bii for all nodes
        bool NodeYiiEmpty;
        /*Leaders of all FMonitors*/
        int clstr_num_hghst, clstr_num_lwst;
        TLDs_sys_fms LD_FM[4/*# range 0..3*/];
        bool bCurtl;
        //****************************Timing variables**********************************
#if ! ( defined( WIN32 ) || defined( WIN64 ) )
        struct timespec SolveStartTime;
        struct timespec SolveEndtime;
        struct timespec GStartTime;
        struct timespec Gendtime;
        struct timespec LoopEndtime;
#else
        LARGE_INTEGER SolveStartTime;
        LARGE_INTEGER SolveEndtime;
        LARGE_INTEGER GStartTime;
        LARGE_INTEGER Gendtime;
        LARGE_INTEGER LoopEndtime;
#endif
        double Total_Time_Elapsed;
        double Solve_Time_Elapsed;
        double Total_Solve_Time_Elapsed;
        double Step_Time_Elapsed;
        //******************************************************************************
        // ActiveCell of the Incidence Matrix:
        // [0] = row
        // [1] = col
        // [2] = value
        int ActiveIncCell[3/*# range 0..2*/];
        //******************************************************************************
        // IncMatrix Row and column descriptors
        // Rows array (array of strings that tells what is the order of the PDElements)
        // Columns array (array of strigns with the names of the cols of the Inc matrix)'
        // Levels array (array of integers that describes the proximity level for each
        // bus to the circuit's backbone)
        std::vector < std::string > Inc_Mat_Rows;
        std::vector < std::string > Inc_Mat_Cols;
        std::vector < int > Inc_Mat_levels;
        int temp_counter;
        std::vector < int > Active_Cols;
        std::vector < int > Active_Cols_Idx;
        //******************************************************************************
        //********************Diakoptics solution mode variables************************
        bool ADiakoptics_ready;
        int ADiakoptics_Actors;
        std::vector <int> LocalBusIdx;
        //      AD_IBus                 : TList<integer>;       // Location of the Current injection bus
        //      AD_ISrcIdx              : TList<integer>;       // Locator of the ISource bus in actor 1
        //******************************************************************************
        TSolutionObj(DSSClass::TDSSClass* ParClass, const String solutionname);
        virtual ~TSolutionObj();
        void ZeroAuxCurrents(int ActorID);
        int SolveZeroLoadSnapShot(int ActorID);
        void DoPFLOWsolution(int ActorID);
        void Solve(int ActorID);                // Main Solution dispatch
        void SnapShotInit(int ActorID);
        int SolveSnap(int ActorID);    // solve for now once
        int SolveDirect(int ActorID);  // solve for now once, direct solution
        int SolveYDirect(int ActorID); // Similar to SolveDirect; used for initialization
        int SolveCircuit(int ActorID); // SolveSnap sans control iteration
        void CheckControls(int ActorID);       // Snapshot checks with matrix rebuild
        void SampleControlDevices(int ActorID);
        void DoControlActions(int ActorID);
        void Sample_DoControlActions(int ActorID);    // Sample and Do
        void Check_Fault_Status(int ActorID);
        int SolveAD(int ActorID, bool Initialize);    // solve one of the A-Diakoptics stages locally
        void SetGeneratorDispRef(int ActorID);
        void SetVoltageBases(int ActorID);
        void SaveVoltages();
        void UpdateVBus(int ActorID); // updates voltages for each bus    from NodeV
        void RestoreNodeVfromVbus();  // opposite   of updatebus
        complex VDiff(int i, int j, int ActorID);  // Difference between two node voltages
             /*by Dahei*/
        void Get_Yiibus(); // updates voltages for each bus    from NodeV
        complex Get_Yij(int node_ref_i, int node_ref_j); // get Gij + j Bij
             /**/
        virtual void InitPropertyValues(int ArrayOffset);
        virtual void DumpProperties(TTextRec& F, bool Complete);
        void WriteConvergenceReport(const String Fname);
        void Update_dblHour();
        void Increment_time();
        void UpdateLoopTime();
        double get_FFrequency();
        int get_Fyear();

        double get_Solve_Time_Elapsed();
        double get_Total_Solve_Time_Elapsed();
        double get_Step_Time_Elapsed();
        double get_Total_Time_Elapsed();

        // Procedures that use to be private before 01-20-2016
        void AddInAuxCurrents(int SolveType, int ActorID);
        int SolveSystem(pNodeVarray V, int ActorID);
        void GetPCInjCurr(int ActorID, bool GFMOnly = false);
        void GetSourceInjCurrents(int ActorID);
        void ZeroInjCurr(int ActorID);
        void Upload2IncMatrix();
        void Calc_Inc_Matrix(int ActorID);                // Calculates the incidence matrix for the Circuit
        void Calc_Inc_Matrix_Org(int ActorID);            // Calculates the incidence matrix hierarchically organized for the Circuit
        int get_IncMatrix_Row(int Col);          // Gets the index of the Row connected to the specified Column
        int get_IncMatrix_Col(int Row);          // Gets the index of the Column connected to the specified Row
        int CheckLocationIdx(int Idx);           // Evaluates the area covered by the tearing point to see if there is a better one
        int get_PDE_Bus1_Location(String myPDE);     // Gets the index of myPDE -> bus1 within the Inc matrix
        void AddLines2IncMatrix(int ActorID);             // Adds the Lines to the Incidence matrix arrays
        void AddXfmr2IncMatrix(int ActorID);              // Adds the Xfmrs to the Incidence matrix arrays
        void AddSeriesCap2IncMatrix(int ActorID);         // Adds capacitors in series to the Incidence matrix arrays
        void AddSeriesReac2IncMatrix(int ActorID);        // Adds Reactors in series to the Incidence matrix arrays
        void SendCmd2Actors(int Msg);                     // Sends a message to other actors different than 1
        void UploadV2Master(int ActorID);                 // Uploads the local solution into the master's (actor 1) voltage array
        void UpdateISrc(int ActorID);                     // Updates the local ISources using the dat available at Ic for actor 1
        complex VoltInActor1(int NodeIdx);          // returns the voltage indicated in NodeIdx in the context of the actor 1
    };
    /*==========================================================================*/


    extern TSolutionObj* ActiveSolutionObj;

}

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Solution;
#endif

#endif //  SolutionH







