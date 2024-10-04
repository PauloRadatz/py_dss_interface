
#pragma hdrstop

#include <chrono>
#include "Solution.h"

#include "SolutionAlgs.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "CmdForms.h"
#include "PDElement.h"
#include "ControlElem.h"
#include "Fault.h"
#include "AutoAdd.h"
#include "YMatrix.h"
#include "Load.h"
#include "CktTree.h"
#include "ParserDel.h"
#include "generator.h"
#include "Capacitor.h"
//#include "ImplGlobals.h"  // to fire events
#include <math.h>
#include "Circuit.h"
#include "Utilities.h"
#include "klusolve.h"
#include "PointerList.h"
#include "Line.h"
#include "Transformer.h"
#include "Reactor.h"
#include "Diakoptics.h"
#include "d2c_structures.h"
#include "ExecHelper.h"
#include "dirsep.h"


TSolutionObj* ActiveSolutionObj = NULL;



const int NumPropsThisClass = 1;


namespace Solution
{

// ===========================================================================================

    TSolutionObj* ActiveSolutionObj;

    EControlProblem::EControlProblem(const String &Msg) : inherited(Msg) {}
    ESolveError::ESolveError(const String &Msg) : inherited(Msg) {}

    TDSSSolution::TDSSSolution( )  // Collection of all solution objects

    {
      ;
      Class_Name = "Solution";
      DSSClassType = DSS_OBJECT + HIDDEN_ELEMENT;
      ActiveElement = 0;
      DefineProperties();
      std::string* slc = Slice( PropertyName, NumProperties );
      CommandList = TCommandList(slc , NumProperties);
      delete[] slc;
      CommandList.set_AbbrevAllowed(true);
    }

    // ===========================================================================================



    TDSSSolution::~TDSSSolution( )
    {
        // ElementList and  CommandList freed in inherited destroy
      // todo check:  inherited::Destroy;
    }

    // ===========================================================================================



    void TDSSSolution::DefineProperties( )
    {
      NumProperties = NumPropsThisClass;
      CountProperties();   // Get inherited property count
      AllocatePropertyArrays();


         // Define Property names
      PropertyName[1 - 1] = "-------";


         // define Property help values
      PropertyHelp[1 - 1] = "Use Set Command to set Solution properties.";
      ActiveProperty = NumPropsThisClass - 1;
      inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
    }

    // ===========================================================================================



    int TDSSSolution::NewObject( const String ObjName )
    {
      int result = 0;
        // Make a new Solution Object and add it to Solution class list
      ActiveSolutionObj = new TSolutionObj( this, ObjName );
        // this one is different than the rest of the objects.
      result = AddObjectToList( ActiveSolutionObj );
      return result;
    }

    // ===========================================================================================


    // ===========================================================================================
    TSolutionObj::TSolutionObj( TDSSClass* ParClass, const String solutionname )
     : inherited(ParClass),
        dV(NULL),
       nNZ_yii(0),
       Algorithm(0),
       ControlActionsDone(false),
       ControlIteration(0),
       ControlMode(0),
       ConvergenceTolerance(0.0),
       ConvergedFlag(false),
       DefaultControlMode(0),
       DefaultLoadModel(0),
       DoAllHarmonics(false),
       DynamicsAllowed(false),
       FirstIteration(false),
       FrequencyChanged(false),
       Fyear(0),
       Harmonic(0.0),
       HarmonicListSize(0),
       IntervalHrs(0.0),
       Iteration(0),
       LoadModel(0),
       VoltageBaseChanged(false),
       SampleTheMeters(false),
       SeriesYInvalid(false),
       SolutionInitialized(false),
       DynamicsInitialized(false),
       SystemYChanged(false),
       UseAuxCurrents(false),
       PreserveNodeVoltages(false),
       IsDynamicModel(false),
       IsHarmonicModel(false),
       LastSolutionWasDirect(false),
       LoadsNeedUpdating(false),
       ActorVIdx(0),
       NumberOfTimes(0),
       RandomType(0),
       SolutionCount(0),
       MaxIterations(0),
       MinIterations(0),
       MostIterationsDone(0),
       MaxControlIterations(0),
       MaxError(0.0),
       NodeYiiEmpty(false),
       clstr_num_hghst(0),
       clstr_num_lwst(0),
       bCurtl(false),
       Total_Time_Elapsed(0.0),
       Solve_Time_Elapsed(0.0),
       Total_Solve_Time_Elapsed(0.0),
       Step_Time_Elapsed(0.0),
       temp_counter(0),
       ADiakoptics_ready(false),
       ADiakoptics_Actors(0)
    {
      Set_Name(LowerCase( solutionname ));

    //    i := SetLogFile ('c:\\temp\\KLU_Log.txt', 1);

      SolveStartTime    = { 0 };
      SolveEndtime      = { 0 };
      GStartTime        = { 0 };
      Gendtime          = { 0 };
      LoopEndtime       = { 0 };
      Fyear = 0;
      DynaVars.intHour = 0;
      DynaVars.T = 0.0;
      DynaVars.dblHour = 0.0;
      DynaVars.tstart = 0.0;
      DynaVars.tstop = 0.0;
        //duration := 0.0;
      DynaVars.h = 0.001;  // default for dynasolve
      LoadsNeedUpdating = true;
      VoltageBaseChanged = true;  // Forces Building of convergence check arrays
      MaxIterations = 15;
      MinIterations = 2;
      MaxControlIterations = 10;
      ConvergenceTolerance = 0.0001;
      ConvergedFlag = false;
      SampleTheMeters = false;  // Flag to tell solution algorithm to sample the Energymeters
      IsDynamicModel = false;
      IsHarmonicModel = false;
      if (DefaultBaseFreq == 0)
          DefaultBaseFreq = 60;
      FFrequency = -1; // initialize to avoid potential issues and warnings
      Set_Frequency(DefaultBaseFreq);
        /*Fundamental := 60.0; Moved to Circuit and used as default base frequency*/
      Harmonic = 1.0;
      FrequencyChanged = true;  // Force Building of YPrim matrices
      DoAllHarmonics = true;
      FirstIteration = true;
      DynamicsAllowed = false;
      SystemYChanged = true;
      SeriesYInvalid = true;
      NodeV.clear();
      Currents.clear();
      Node_dV.clear();
      Ic_Local.clear();
      NodeYii.clear();


        /*Define default harmonic list*/
      HarmonicListSize = 5;
      HarmonicList = new double[ HarmonicListSize ];
      HarmonicList[0] = 1.0;
      HarmonicList[1] = 5.0;
      HarmonicList[2] = 7.0;
      HarmonicList[3] = 11.0;
      HarmonicList[4] = 13.0;
      SolutionInitialized = false;
      LoadModel     = POWERFLOW;
      DefaultLoadModel = LoadModel;
      LastSolutionWasDirect = false;
      hYseries      = 0;
      hYsystem      = 0;
      hY            = 0;
      Jacobian      = 0;
      NodeV.clear();
      dV            = NULL;
      Currents.clear();
      AuxCurrents   = NULL;
      VmagSaved.clear();
      ErrorSaved.clear();
      NodeVbase.clear();
      UseAuxCurrents = false;
      SolutionCount = 0;
      DynaVars.SolutionMode = SNAPSHOT;
      ControlMode   = CTRLSTATIC;
      DefaultControlMode = ControlMode;
      Algorithm     = NORMALSOLVE;
      RandomType    = GAUSSIAN;  // default to gaussian
      NumberOfTimes = 100;
      IntervalHrs   = 1.0;
      InitPropertyValues( 0 );
      ADiakoptics_ready = false;   // A-Diakoptics needs to be initialized
  
      /*   --pending to implement--DM
  
      if ( !( ActorMA_Msg[ActiveActor] != NULL ) )
        ActorMA_Msg[ActiveActor] = TEvent( NULL, true, false, "" );   */
      pColIdx_Yii.clear();
      pRowIdx_Yii.clear();
      pcVals_Yii.clear();
      deltaF.clear();
      deltaZ.clear();
      NCIMY.clear();
      NCIMYRow.clear();
      NCIMYCol.clear();
      NCIMRdy = false;
      PV2PQList.clear();
      IgnoreQLimit = false;
      GenGainNCIM = 1.0;
      InitGenQ = true;
    }

    // ===========================================================================================


    TSolutionObj::~TSolutionObj( )
    {
      free(AuxCurrents);
      Currents.clear();
      //dV                = (pNodeVarray)     realloc(dV, 0);
      free(dV);
      ErrorSaved.clear();
      NodeV.clear();
      NodeVbase.clear();
      VmagSaved.clear();
      if ( hYsystem != 0 )
        DeleteSparseSet( hYsystem );
      if ( hYseries != 0 )
        DeleteSparseSet( hYseries );
          /*by Dahei: */
      NodeYii.clear();  // for bii
      pColIdx_Yii.clear();
      pRowIdx_Yii.clear();
      pcVals_Yii.clear();
          /*---------------------------*/
    //      SetLogFile ('c:\\temp\\KLU_Log.txt', 0);
      delete[] HarmonicList;
      //ActorMA_Msg[ActiveActor].SetEvent;

    // Sends a message to the working actor
      if ( ActorHandle[ActiveActor] != NULL )
      {
        delete ActorHandle[ActiveActor];
        ActorHandle[ActiveActor] = NULL;
      }
      //free(ActorMA_Msg[ActiveActor]);
      ActorMA_Msg[ActiveActor] = 0;
      // todo check:  inherited::Destroy;
      Node_dV.clear();
      Ic_Local.clear();
      NCIMY.clear();
      NCIMYRow.clear();
      NCIMYCol.clear();
    }


    // ===========================================================================================



    int TDSSSolution::Edit( int ActorID )
    {
      int result = 0;
      result = 0;
      ActiveSolutionObj = ActiveCircuit[ActorID]->Solution;
      /*# with ActiveSolutionObj do */
      {

           // This is all we do here now...
        ActiveSolutionObj->Solve( ActorID );
      }  /*WITH*/
      return result;
    }

    // ===========================================================================================



    void TSolutionObj::Solve( int ActorID )
    {
      //TScriptEdit ScriptEd;
      ActiveCircuit[ActorID]->Issolved = false;
      SolutionWasAttempted[ActorID] = true;

    /*Check of some special conditions that must be met before executing solutions*/
      if ( ActiveCircuit[ActorID]->EmergMinVolts >= ActiveCircuit[ActorID]->NormalMinVolts )
      {
        DoThreadSafeMsg( "Error: Emergency Min Voltage Must Be Less Than Normal Min Voltage!" + CRLF + "Solution Not Executed.", 480 );
        return;
      }
      if ( SolutionAbort )
      {
        GlobalResult = "Solution aborted.";
        CmdResult = SOLUTION_ABORT;
        ErrorNumber = CmdResult;
        return;
      }
      try
      {
    /*Main solution Algorithm dispatcher*/
        /*# with ActiveCircuit[ActorID] do */
        {
          TDSSCircuit* with0 = ActiveCircuit[ActorID];
          {
            switch ( get_Fyear() )
            {
              case 0:
                with0->DefaultGrowthFactor = 1.0;
              break;    // RCD 8-17-00
            default:
                with0->DefaultGrowthFactor = pow(with0->DefaultGrowthRate, ( get_Fyear() - 1 ) );
            }
          }
        }  
        // Creates the actor again in case of being terminated due to an error before
        if ((!ActorHandle[ActorID]->ActorActive) || (ActorHandle[ActorID] == NULL))
        {
          if ( !ActorHandle[ActorID]->ActorActive)
            free(ActorHandle[ActorID]);
          New_Actor( ActorID );
        }

        // Resets the event for receiving messages from the active actor
          // Updates the status of the Actor in the GUI
        ActorStatus[ActorID] = 0;    // Global to indicate that the actor is busy
        //ActorMA_Msg[ActorID]->ResetEvent;
        QueryPerformanceCounter( &GStartTime );
        if ( ! NoFormsAllowed )
        {
          if ( ! IsProgressON && DSSProgressFrm )
          {
            switch ( DynaVars.SolutionMode )
            {
              case YEARLYMODE: case DUTYCYCLE: case LOADDURATION1: case LOADDURATION2: case HARMONICMODE: case HARMONICMODET:
              {
                if ( Progress_Actor[ActorID] != NULL )
                {
                  //Progress_Actor[ActorID]->Terminate;
                  Progress_Actor[ActorID] = NULL;
                }
                //Progress_Actor = TProgressActor.Create( );
              }
              break;
                default:
                {
                    // Just other simulation modes, nothing to do
                }
            }
          }
        }

          // Sends message to start the Simulation
        ActorHandle[ActorID]->Send_Message( SIMULATE );
          // If the parallel mode is not active, Waits until the actor finishes
        if (!Parallel_enabled)
        {
          Wait4Actors( ALL_ACTORS );
        }
      }
      catch( std::exception & E )
      {
        DoThreadSafeMsg( "Error Encountered in Solve: " + (std::string) E.what(), 482 );
        SolutionAbort = true;
      }
    }

    // ===========================================================================================



    bool TSolutionObj::Converged( int ActorID )
    {
        bool result = false;
        int i = 0;
        double VMag = 0.0;

        if (ActiveCircuit[ActorID]->Solution->Algorithm != NCIMSOLVE)
        {

            // base convergence on voltage magnitude
            MaxError = 0.0;
            for (int stop = ActiveCircuit[ActorID]->NumNodes, i = 1; i <= stop; i++)
            {
                if (!ADiakoptics || (ActorID == 1))
                    VMag = cabs(NodeV[i]);
                else
                    VMag = cabs(VoltInActor1(i));

                /* If base specified, use it; otherwise go on present magnitude  */
                if (NodeVbase[i] > 0.0)
                    ErrorSaved[i] = Abs(VMag - VmagSaved[i]) / NodeVbase[i];
                else
                    if (VMag != 0.0)
                        ErrorSaved[i] = Abs(1.0 - (VmagSaved[i] / VMag));
                VmagSaved[i] = VMag;  // for next go-'round
                MaxError = max(MaxError, ErrorSaved[i]);  // update max error
            }
            if (MaxError <= ConvergenceTolerance)
                result = true;

        }
        else
        {
            for (i = 0; i < deltaF.size(); i++)
            {
                result = Abs(deltaF[i].re) <= ConvergenceTolerance;
                if (!result)
                    break;
            }

        }

        ConvergedFlag = result;
        return result;
    }


    // ===========================================================================================



    void TSolutionObj::GetSourceInjCurrents( int ActorID )

    // Add in the contributions of all source type elements to the global solution vector InjCurr

    {
      TDSSCktElement* pElem;
      /*# with ActiveCircuit[ActorID] do */
      {
        auto with0 = ActiveCircuit[ActorID];
        {
          pElem = (TDSSCktElement*) with0->Sources.Get_First();
          while ( pElem != NULL )
          {
            if ( pElem->Get_Enabled() )
              pElem->InjCurrents( ActorID ); // uses NodeRef to add current into InjCurr Array;
            pElem = (TDSSCktElement*) with0->Sources.Get_Next();
          }

          // Adds GFM PCE as well
          GetPCInjCurr(ActorID, true);
        }
      }
    }

    // ===========================================================================================



    void TSolutionObj::SetGeneratorDispRef( int ActorID )

    // Set the global generator dispatch reference

    {
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        switch ( DynaVars.SolutionMode )
        {
          case SNAPSHOT:
            with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor;
          break;
          case YEARLYMODE:
              with0->GeneratorDispatchReference = with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
          case DAILYMODE:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
          case DUTYCYCLE:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
          case GENERALTIME:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
          case DYNAMICMODE:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor;
          break;
          case EMPMODE:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor;
          break;
          case EMPDAILYMODE:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor;
          break;
          case HARMONICMODE:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor;
          break;
          case MONTECARLO1:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor;
          break;
          case MONTECARLO2:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
          case MONTECARLO3:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
          case PEAKDAY:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
          case LOADDURATION1:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
          case LOADDURATION2:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
          case DIRECT:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor;
          break;
          case MONTEFAULT:
              with0->GeneratorDispatchReference = 1.0;
          break;  // Monte Carlo Fault Cases solve  at peak load only base case

          case FAULTSTUDY:
              with0->GeneratorDispatchReference = 1.0;
          break;
          case AUTOADDFLAG:
              with0->GeneratorDispatchReference = with0->DefaultGrowthFactor;
          break;   // peak load only

          case HARMONICMODET:
              with0->GeneratorDispatchReference = with0->get_FLoadMultiplier() * with0->DefaultGrowthFactor * with0->DefaultHourMult.re;
          break;
        default:
          DoSimpleMsg( "Unknown solution mode.", 483 );
        }
      }
    }

    // ===========================================================================================



    void TSolutionObj::SetGeneratordQdV( int ActorID )
    {
      TGeneratorObj* pGen;
      bool Did_One = false;
      double GenDispSave = 0.0;
      Did_One = false;

         // Save the generator dispatch level and set on high enough to
         // turn all generators on
      GenDispSave = ActiveCircuit[ActorID]->GeneratorDispatchReference;
      ActiveCircuit[ActorID]->GeneratorDispatchReference = 1000.0;
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          pGen = (TGeneratorObj*) with0->Generators.Get_First();
          while ( pGen != NULL )
          {
            if ( ( (TDSSCktElement*) pGen )->Get_Enabled() )
            {

                  // for PV generator models only ...
              if ( pGen->GenModel == 3 )
              {
                pGen->InitDQDVCalc();

                       // solve at base var setting
                Iteration = 0;
                do
                {
                  Iteration++;
                  ZeroInjCurr( ActorID );
                  GetSourceInjCurrents( ActorID );
                  pGen->InjCurrents( ActorID );   // get generator currents with nominal vars
                  SolveSystem( &NodeV[0], ActorID);
                }
                while ( ! ( Converged( ActorID ) || ( Iteration >= MaxIterations ) ) );
                pGen->RememberQV( ActorID );  // Remember Q and V
                pGen->BumpUpQ();

                       // solve after changing vars
                Iteration = 0;
                do
                {
                  Iteration++;
                  ZeroInjCurr( ActorID );
                  GetSourceInjCurrents( ActorID );
                  pGen->InjCurrents( ActorID );   // get generator currents with nominal vars
                  SolveSystem( &NodeV[0], ActorID);
                }
                while ( ! ( Converged( ActorID ) || ( Iteration >= MaxIterations ) ) );
                pGen->CalcDQDV( ActorID ); // bssed on remembered Q and V and present values of same
                pGen->ResetStartPoint();
                Did_One = true;
              }
            }
            pGen = (TGeneratorObj*)with0->Generators.Get_Next();
          }
        }
      }

         // Restore generator dispatch reference
      ActiveCircuit[ActorID]->GeneratorDispatchReference = GenDispSave;
      try
      {
        if        // Reset Initial Solution
        ( Did_One )
          SolveZeroLoadSnapShot( ActorID );
      }
      catch( std::exception & E )
      {
      {
        DoThreadSafeMsg( "From SetGenerator DQDV, SolveZeroLoadSnapShot: " + CRLF + (std::string) E.what() + CheckYMatrixforZeroes( ActorID ), 7071 );
        //throw @ESolveError ::( "Aborting" );
      }
      }
    }

    // ===========================================================================================



    void TSolutionObj::SendCmd2Actors( int Msg )
    {
      int i = 0;
      for ( int stop = NumOfActors, i = 2; i <= stop; i++)
      {
        ActorStatus[i] = 0;
        ActorHandle[i]->Send_Message( Msg );
      }
      Wait4Actors( AD_ACTORS );
    }
    // ===========================================================================================
    void TSolutionObj::DoNormalSolution( int ActorID )

    /* Normal fixed-point solution

       Vn+1 = [Y]-1 Injcurr

       Where Injcurr includes only PC elements  (loads, generators, etc.)
       i.e., the shunt elements.

       Injcurr are the current injected INTO the NODE
            (need to reverse current direction for loads)
    */
    {
      int i = 0;
      Iteration = 0;
     /***** Main iteration loop *****/
      {
        auto with0 = ActiveCircuit[ActorID];
//        DOForceFlatStart(ActorID);
        do
        {
          Iteration++;
          if ( with0->LogEvents )
            LogThisEvent( "Solution Iteration " + IntToStr( Iteration ), ActorID );
          if ( ( ! ADiakoptics ) || ( ActorID != 1 ) )             // Normal simulation
          {                                                   // In A-Diakoptics, all other actors do normal solution
        /* Get injcurrents for all PC devices  */
            ZeroInjCurr( ActorID );
            GetSourceInjCurrents( ActorID );  // sources
            GetPCInjCurr( ActorID );  // Get the injection currents from all the power conversion devices and feeders

           // The above call could change the primitive Y matrix, so have to check
            if ( SystemYChanged )
            {
              BuildYMatrix( WHOLEMATRIX, false, ActorID );  // Does not realloc V, I
            }
            /*by Dahei*/
            if ( NodeYiiEmpty )
              Get_Yiibus();
            if ( UseAuxCurrents )
              AddInAuxCurrents( NORMALSOLVE, ActorID );

          // Solve for voltages                      {Note:NodeV[0] = 0 + j0 always}
            if ( with0->LogEvents )
              LogThisEvent( "Solve Sparse Set DoNormalSolution ...", ActorID );
            SolveSystem( &NodeV[0], ActorID);
            LoadsNeedUpdating = false;
          }
          else
          {
            ADiak_PCInj = true;
            Solve_Diakoptics( );             // A-Diakoptics
          }
        }
        while ( ! ( ( Converged( ActorID ) && ( Iteration >= MinIterations ) ) || ( Iteration >= MaxIterations ) ) );
      }
    }

    // ===========================================================================================
    void TSolutionObj::DoNCIMSolution(int ActorID)
    {
        /* Implements the N conductor current injection method (NCIM) for solving the power flow problem.

         This mehtod is a Newton-Raphson like solution method, and is implemented here to address
         transmission system-like simulations. For more info, check:

         https://www.sciencedirect.com/science/article/abs/pii/S0142061512004310

        */
        auto        with0 = ActiveCircuit[ActorID];
        bool        Solved = false;
        complex     dV = CZero;

        Iteration = 0;                                                  // Initializes iteration counter

        if (InitGenQ)                                                   // If the system needs to be initialized
        {
            InitPQGen(ActorID);                                         // Initialize PQ like generators
            PV2PQList.clear();
        }

        if (SystemYChanged || !NCIMRdy)
            NCIMNodes = InitNCIM(ActorID, InitGenQ);                       // Initializes the NCIM environment vars and structures (takes time)

        /***** Main iteration loop *****/
        do
        {
            Iteration++;
            CalcInjCurr(ActorID, InitGenQ);                                   // Calc Injection currents using the latest solution ( I = Y * V )
            BuildJacobian(ActorID);                                 // Resets the jacobian's diagonal for the next iteration
            GetNCIMPowers(ActorID);                                 // Populate the total power vector
            ApplyCurrNCIM(ActorID);                                 // Adjust Jacobian and populate the currents vector
            if (with0->LogEvents)
                LogThisEvent("Solve Power flow DoNCIMSolution ...", ActorID);
            // Solves the Jacobian
            SolveSparseSet(Jacobian, &(deltaZ[0]), &(deltaF[0]));

            //Updates the Voltage vector
            int dVIdx = 0;
            dV = CZero;
            for (int i = 1; i <= with0->NumNodes; i++)
            {
                dVIdx = (i - 1) * 2;
                dV = cmplx(deltaZ[dVIdx].re, deltaZ[dVIdx + 1].re);
                NodeV[i] = csub(NodeV[i], dV);
            }
            Solved = Converged(ActorID);
            // Updates the Generator's Q using the calculated deltaQ
            UpdateGenQ(ActorID);
            InitGenQ = false;

        } while (!((Solved && (Iteration >= MinIterations)) || (Iteration >= MaxIterations)));


        DistGenClusters(ActorID);       // Distributes the power among all the clustered generators (if any)
        // To reverse what we did for the next simulation step
        // ReversePQ2PV(ActorID); - not needed for now (04/01/2024)

    }

    // ===========================================================================================
    void TSolutionObj::DoNewtonSolution( int ActorID )

    /* Newton Iteration

       Vn+1 =  Vn - [Y]-1 Termcurr

       Where Termcurr includes currents from all elements and we are
       attempting to get the  currents to sum to zero at all nodes.

       Termcurr is the sum of all currents going INTO THE TERMINALS of
       the elements.

       For PD Elements, Termcurr = Yprim*V

       For Loads, Termcurr = (Sload/V)*
       For Generators, Termcurr = -(Sgen/V)*

    */
    {
      int i = 0;
      /*# with ActiveCircuit[ActorID] do */
      {
        auto with0 = ActiveCircuit[ActorID];
        {
          ReallocMem( dV, sizeof( dV[1] ) * ( with0->NumNodes + 1 ) ); // Make sure this is always big enough
          if ( ControlIteration == 1 )
            GetPCInjCurr( ActorID );  // Update the load multipliers for this solution
          Iteration = 0;
          do
          {
            Iteration++;
            SolutionCount++;    // SumAllCurrents Uses ITerminal  So must force a recalc

            // Get sum of currents at all nodes for all  devices
            ZeroInjCurr( ActorID );
            SumAllCurrents( ActorID );

               // Call to current calc could change YPrim for some devices
            if ( SystemYChanged )
            {
              BuildYMatrix( WHOLEMATRIX, false, ActorID );   // Does not realloc V, I
            }
              /*by Dahei*/
            if ( NodeYiiEmpty )
              Get_Yiibus();  //
            if ( UseAuxCurrents )
              AddInAuxCurrents( NEWTONSOLVE, ActorID );

            // Solve for change in voltages
            SolveSystem( dV, ActorID );
            LoadsNeedUpdating = false;

             // Compute new guess at voltages
            for ( int stop = with0->NumNodes, i = 1; i <= stop; i++)     // 0 node is always 0
              /*# with NodeV^[i] do */
              {
                complex with1 = NodeV[i];
                {
                  with1.re = with1.re - dV[i].re;
                  with1.im = with1.im - dV[i].im;
                }
              }
          }
          while ( ! ( ( Converged( ActorID ) && ( Iteration >= MinIterations ) ) || ( Iteration >= MaxIterations ) ) );
        }
      }
    }

    // ===========================================================================================
/*  Populates the current injections vector and updates the jacobian matrix as needed */

    void TSolutionObj::ApplyCurrNCIM(int ActorID)
    {
        auto with0 = ActiveCircuit[ActorID]->Solution;
        for (int i = 1; i < pNodePower.size(); i++)
        {
            if ((pNodePower[i].re != 0) || (pNodePower[i].im != 0))
            {
                if (pNodeType[i] == PV_Node)
                    DoPVBusNCIM(ActorID, i, pNodePVTarget[i], pNodePower[i]);
                else
                    DoPQBusNCIM(ActorID, i, with0->NodeV[i], pNodePower[i]);
            }
        }
    }

    // ===========================================================================================
/*  Populates the total power vector before solving the NCIM algorithm */

    void TSolutionObj::GetNCIMPowers(int ActorID)
    {
        TDSSCktElement* pElem = nullptr;
        TCapacitorObj* pCap = nullptr;
        TReactorObj* pReact = nullptr;
        bool            valid = false;
        int             NodeIdx = 0;

        auto with0 = ActiveCircuit[ActorID];
        /* Get inj currents from all enabled PC devices */
        {
            {
                complex LdPower = CZero,
                    LdVolt = CZero,
                    GenS = CZero;
                pElem = (TDSSCktElement*)with0->PCElements.Get_First();
                while (pElem != NULL)
                {
                    /*# with pElem do */
                    if (pElem->Get_Enabled())
                    {
                        for (int idx = 1; idx <= pElem->Fnphases; idx++)
                        {
                            NodeIdx = pElem->NodeRef[idx - 1];
                            switch (pElem->DSSObjType & CLASSMASK)
                            {
                            case LOAD_ELEMENT:
                            {
                                LdPower = cmplx(((TLoadObj*)pElem)->WNominal, ((TLoadObj*)pElem)->varNominal);
                                LdVolt = with0->Solution->NodeV[NodeIdx];

                                if (((TLoadObj*)pElem)->FLoadModel == 2)
                                    DoZBusNCIM(ActorID, NodeIdx, LdVolt, ((TLoadObj*)pElem)->YPrim);
                                else
                                {
                                    if (pNodeType[NodeIdx] == PV_Node)
                                        pNodePower[NodeIdx] = csub(pNodePower[NodeIdx], LdPower);
                                    else
                                        pNodePower[NodeIdx] = cadd(pNodePower[NodeIdx], LdPower);
                                    for (int idx = 0; idx < pElem->Get_NPhases(); idx++)
                                        pElem->Iterminal[idx] = conjg(cdiv(LdPower, LdVolt));
                                }
                            }
                            break;
                            case GEN_ELEMENT:
                            {
                                auto  with2 = (TGeneratorObj*)pElem;
                                auto& with1 = with2->GenVars;

                                // Checks the reactive power limits in this node
                                switch (with2->GenModel)
                                {
                                case 3:         // Generator is a PV bus
                                {
                                    with1.Qnominalperphase = with1.deltaQNom[idx - 1];

                                    GenS = cmplx(with1.Pnominalperphase, with1.Qnominalperphase);
                                    if (pNodeType[NodeIdx] == PQ_Node)
                                        pNodePower[NodeIdx] = cnegate(pNodePower[NodeIdx]);

                                    pNodeType[NodeIdx] = PV_Node;                   // Forces the node to be PV
                                    pNodePower[NodeIdx] = cadd(GenS, pNodePower[NodeIdx]);
                                    pGenPower[NodeIdx] = cadd(GenS, pGenPower[NodeIdx]);

                                    pNodePVTarget[NodeIdx] = with1.VTarget;             // Updates the target for the Bus, just in case
                                    PVBusIdx[NodeIdx] = with2->NCIMIdx + idx;           // Stores the generator IDX in the NCIM array
                                }
                                break;
                                case 4:         // Generator acts like PQ bus
                                {
                                    LdVolt = with0->Solution->NodeV[NodeIdx];
                                    if (with1.deltaQNom.empty())
                                        GenS = cmplx(with1.Pnominalperphase, with1.Qnominalperphase);
                                    else
                                        GenS = cmplx(with1.Pnominalperphase, with1.deltaQNom[0]);
                                    if (pNodeType[NodeIdx] == PQ_Node)
                                        pNodePower[NodeIdx] = csub(pNodePower[NodeIdx], GenS);
                                    else
                                        pNodePower[NodeIdx] = cadd(pNodePower[NodeIdx], GenS);

                                    pGenPower[NodeIdx] = cadd(GenS, pGenPower[NodeIdx]);
                                }
                                break;
                                default:     // Constant impedance
                                {
                                    LdVolt = with0->Solution->NodeV[NodeIdx];
                                    DoZBusNCIM(ActorID, NodeIdx, LdVolt, with2->YPrim);
                                }
                                break;
                                }
                            }
                            break;
                            case FAULTOBJECT:
                            {
                                auto  with2 = (TFaultObj*)pElem;
                                LdVolt = with0->Solution->NodeV[NodeIdx];
                                DoZBusNCIM(ActorID, NodeIdx, LdVolt, with2->YPrim);
                            }
                            break;
                            default:
                            {
                                // Ignore the others
                            }
                            break;
                            }
                        }
                    }
                    pElem = (TDSSCktElement*)with0->PCElements.Get_Next();
                }
            }
        }
    }
    // ===========================================================================================
/*  Apply the PV bus current injection for NCIM */

    void TSolutionObj::DoPVBusNCIM(int ActorID, int i, double VTarget, complex Power)
    {
        complex Pow = CZero,
                FaVr = CZero,
                FaVm = CZero,
                Temp = CZero,
                Vc2 = CZero;
        double	myVal = 0.0;
        int		LCoords[4][2] = { {0,0}, {1,1}, {0,1}, {1,0} };
        int		GCoord = 0, GCoordY = 0;
        auto	with0 = ActiveCircuit[ActorID]->Solution;

        Pow = conjg(Power);
        complex V = with0->NodeV[i];
        complex PowN = Power;
        complex Curr = conjg(cdiv(PowN, V));
        Vc2 = cmul(conjg(V), conjg(V));
        FaVr = cdiv(cmplx(-1, 0), Vc2);
        FaVm = cdiv(cmplx(0, 1), Vc2);
        GCoord = (i * 2) - 1;
        // Updates the Jacobian
        for (int j = 0; j < 4; j++)
        {
            // Add the derivatives
            switch (j)
            {
            case 0:													// dImdVr
                Temp.re = -1.0 * cmul(FaVr, Pow).im;
                break;
            case 1:													// dIrdVm
                Temp.re = -1.0 * cmul(FaVm, Pow).re;
                break;
            case 2:													// dImdVm
                Temp.re = -1.0 * cmul(FaVm, Pow).im;
                break;
            case 3:													// dIrdVr
                Temp.re = -1.0 * cmul(FaVr, Pow).re;
                break;
            default:
                break;
            }
            SetMatrixElement(with0->Jacobian, GCoord + LCoords[j][0], GCoord + LCoords[j][1], &Temp);
        }

        // Add current injection contributions to deltaF
        GCoord--;																		// Removes the additional index added by DSS
        with0->deltaF[GCoord].re = with0->deltaF[GCoord].re - Curr.im;			        // Respecting the decoupled distribution
        with0->deltaF[GCoord + 1].re = with0->deltaF[GCoord + 1].re - Curr.re;		    // Prioritizing reactive power over the diagonal

        // Add delta V to deltaF in the voltage regulation subsection
        double VMag = ctopolar(V).mag;
        GCoord = (ActiveCircuit[ActorID]->NumNodes * 2) + PVBusIdx[i] - 1;
        double VError = VTarget - VMag;

        with0->deltaF[GCoord - 1].re = VError;

        // Calculate the voltage regulation coefficients (Z)
        GCoordY = (i * 2) - 1;
        for (int j = 0; j < 2; j++)
        {
            // Adds the regulation coefficients
            if (j == 0)
                myVal = -1 * V.re / VMag;
            else
                myVal = -1 * V.im / VMag;
            Temp = cmplx(myVal, 0);
            SetMatrixElement(with0->Jacobian, GCoord, GCoordY + j, &Temp);
        }
        // Calculate the power regulation coefficients (X)
        double den = pow(VMag, 2);
        for (int j = 0; j < 2; j++)
        {
            // Adds the regulation coefficients
            if (j == 0)
                myVal = conjg(V).re / den;
            else
                myVal = conjg(V).im / den;
            Temp = cmplx(myVal, 0);
            SetMatrixElement(with0->Jacobian, GCoordY + j, GCoord, &Temp);
        }
    }

    // ===========================================================================================
/*  Apply the PQ bus current injection for NCIM */

    void TSolutionObj::DoPQBusNCIM(int ActorID, int i, complex V, complex Power)
    {
        complex Pow = CZero,
                FaVr = CZero,
                FaVm = CZero,
                Temp = CZero,
                Vc2 = CZero;

        int LCoords[4][2] = { {0,0}, {1,1}, {0,1}, {1,0} };
        int GCoord = 0;
        auto with0 = ActiveCircuit[ActorID]->Solution;

        Pow = conjg(Power);
        Vc2 = cmul(conjg(V), conjg(V));
        FaVr = cdiv(cmplx(-1.0, 0), Vc2);
        FaVm = cdiv(cmplx(0, 1.0), Vc2);
        complex Curr = CZero;
        Curr = conjg(cdiv(Power, V));
        GCoord = (i * 2) - 1;
        // Updates the Jacobian
        for (int j = 0; j < 4; j++)
        {
            // Add the derivatives
            switch (j)
            {
            case 0:													// dImdVr
                Temp.re = cmul(FaVr, Pow).im;
                break;
            case 1:													// dIrdVm
                Temp.re = cmul(FaVm, Pow).re;
                break;
            case 2:													// dImdVm
                Temp.re = cmul(FaVm, Pow).im;
                break;
            case 3:													// dIrdVr
                Temp.re = cmul(FaVr, Pow).re;
                break;
            default:
                break;
            }
            SetMatrixElement(with0->Jacobian, GCoord + LCoords[j][0], GCoord + LCoords[j][1], &Temp);
        }

        // Add current injection contributions to deltaF
        GCoord--;															        // Removes the additional index added by DSS
        with0->deltaF[GCoord].re = with0->deltaF[GCoord].re + Curr.im;			    // Respecting the decoupled distribution
        with0->deltaF[GCoord + 1].re = with0->deltaF[GCoord + 1].re + Curr.re;		// Prioritizing reactive power over the diagonal
    }

    // ===========================================================================================
/*  Apply the Constannt impedance bus current injection for NCIM */

    void TSolutionObj::DoZBusNCIM(int ActorID, int i, complex V, TcMatrix* YPrim)
    {
        complex Temp = CZero;

        int LCoords[4][2] = { {0,0}, {1,1}, {0,1}, {1,0} };
        int GCoord = 0;
        auto with0 = ActiveCircuit[ActorID]->Solution;

        complex Curr = CZero;
        Curr = cmul(V, YPrim->Values[0]);
        GCoord = (i * 2) - 1;
        // Updates the Jacobian
        for (int j = 0; j < 4; j++)
        {
            // Add the derivatives
            switch (j)
            {
            case 0:													// dImdVr
                Temp.re = YPrim->Values[0].im;
                break;
            case 1:													// dIrdVm
                Temp.re = -1 * (YPrim->Values[0].im);
                break;
            case 2:													// dImdVm
                Temp.re = YPrim->Values[0].re;
                break;
            case 3:													// dIrdVr
                Temp.re = YPrim->Values[0].re;
                break;
            default:
                break;
            }
            SetMatrixElement(with0->Jacobian, GCoord + LCoords[j][0], GCoord + LCoords[j][1], &Temp);
        }

        // Add current injection contributions to deltaF
        GCoord--;															        // Removes the additional index added by DSS
        with0->deltaF[GCoord].re = with0->deltaF[GCoord].re + Curr.im;			    // Respecting the decoupled distribution
        with0->deltaF[GCoord + 1].re = with0->deltaF[GCoord + 1].re + Curr.re;		// Prioritizing reactive power over the diagonal
    }
    // ===========================================================================================
/*  Initializes the node vectors used for storing the total power per node during the simulation */

    void TSolutionObj::InitNCIMVectors(int ActorID)
    {
        pNodePower.resize(1);
        pGenPower.resize(1);
        pNodeType.resize(1);
        pNodePVTarget.resize(1);
        PVBusIdx.resize(1);
        pNodeLimits.resize(1);
        pNodeNumGen.resize(1);

        pNodePower[0]   = CZero;
        pNodeType[0]    = -1;       // means ignore

        auto with0      = ActiveCircuit[ActorID];
        string myBName  = "";
        for (int i = 0; i < with0->NumBuses; i++)
        {
            myBName = with0->BusList.Get(i + 1);
            auto with1 = with0->Buses[i];
            for (int j = 0; j < with1->FNumNodesThisBus; j++)
            {
                pNodePower.push_back(CZero);
                pGenPower.push_back(CZero);
                pNodeType.push_back(PQ_Node);   // Initially, all the buses are PQ
                PVBusIdx.push_back(0);
                pNodePVTarget.push_back(0);
                pNodeLimits.push_back(CZero);
                pNodeNumGen.push_back(0);
            }
        }
    }

    // ===========================================================================================
    /*Forces the voltage vector to a flat start (magnitude only)*/

    void TSolutionObj::DOForceFlatStart(int ActorID)
    {
        auto        with0 = ActiveCircuit[ActorID];

        // Sets the initial solution using the calculated angles and the buses voltage bases
        polar TempPolar = ctopolar(CZero);
        double mykVBase = 0.0;
        double myAng[3] = { 0.0, 4 * PI / 3, 2 * PI / 3 };
        int AIdx = 0;

        // Ignores the nodes attached to the slack bus
        int SlackNumNodes = 1; // with0->Buses[with0->MapNodeToBus[0].BusRef - 1]->get_FNumNodesThisBus() + 1;
        for (int i = SlackNumNodes; i <= with0->NumNodes; i++)
        {
            auto with1 = with0->MapNodeToBus[i - 1];
            mykVBase = (with0->Buses[with1.BusRef - 1]->kVBase) * 1000;
            TempPolar = ctopolar(NodeV[i]);
            TempPolar.mag = mykVBase;
            TempPolar.ang = myAng[AIdx];
            NodeV[i] = ptocomplex(TempPolar);
            AIdx++;
            if (AIdx >= 3)
                AIdx = 0;
        }
        // Now add the slack bus data
        auto pElem = (TVsourceObj*)ActiveCircuit[ActorID]->CktElements.Get_First();
        TempPolar.mag = ((pElem->kVBase * 1000) / SQRT3) * pElem->PerUnit;
        double BaseAng = pElem->Angle * PI / 180;
        for (int i = 1; i <= 3; i++)
        {
            TempPolar.ang = pElem->Angle * PI / 180 + myAng[i - 1];
            NodeV[i] = ptocomplex(TempPolar);
        }
    }

    // ===========================================================================================
/*  Initializes the registries for generators declared as PQ buses (Mode 4) by loading up their deltaQ
    with the q nominal per phase given at the generator's declaration*/
    void TSolutionObj::InitPQGen(int ActorID)
    {
        auto            with0 = ActiveCircuit[ActorID];
        TGeneratorObj* pGen = nullptr;
        int             NumGens = with0->Generators.get_myNumList();

        pGen = (TGeneratorObj*)with0->Generators.Get_First();
        for (int i = 0; i < NumGens; i++)
        {
            if (pGen->Get_Enabled() && (pGen->GenModel != 3))
            {
                pGen->GenVars.deltaQNom.resize(1);
                pGen->GenVars.deltaQNom[0] = pGen->GenVars.Qnominalperphase;
                pGen->GenVars.deltaQNomPtr = pGen->GenVars.deltaQNom.data();
            }
            pGen = (TGeneratorObj*)with0->Generators.Get_Next();
        }
    }

    // ===========================================================================================
/*  St the end of the solution step, distributes the reactive power among existing clusters of generators*/
    void TSolutionObj::DistGenClusters(int ActorID)
    {
        auto            with0 = ActiveCircuit[ActorID];
        TGeneratorObj* pGen = nullptr;
        int             NumGens = with0->Generators.get_myNumList();
        complex         Volt = CZero;
        double          QLocal = 0.0;

        pGen = (TGeneratorObj*)with0->Generators.Get_First();
        for (int idx = 1; idx <= NumGens; idx++)
        {
            if (pGen->Get_Enabled())
            {
                if ((pNodeNumGen[pGen->NodeRef[0]] > 1) && ((pGen->GenModel == 3) || (pGen->GenModel == 4)))
                {
                    for (int j = 1; j <= pGen->Get_NPhases(); j++)
                    {
                        QLocal = Abs(pGen->GenVars.Pnominalperphase / pGenPower[pGen->NodeRef[j - 1]].re) * pGenPower[pGen->NodeRef[j - 1]].im;

                        Volt = NodeV[pGen->NodeRef[j - 1]];
                        pGen->Iterminal[j - 1] = cnegate(conjg(cdiv(cmplx(pGen->GenVars.Pnominalperphase, QLocal), Volt)));
                    }
                }
            }
            pGen = (TGeneratorObj*)with0->Generators.Get_Next();
        }
    }

    // ===========================================================================================
/*  Reverses to model 3 all the gnerators turned into model 4 automatically when the option AvoidPV2PQ is
    disabled, this to take the model back to its original values after these type of changes take place*/
    void TSolutionObj::ReversePQ2PV(int ActorID)
    {
        auto            with0 = ActiveCircuit[ActorID];
        TGeneratorObj* pGen = nullptr;
        int             NumGens = with0->Generators.get_myNumList(),
            GenIdx = 0;

        pGen = (TGeneratorObj*)with0->Generators.Get_First();
        for (int i = 0; i < NumGens; i++)
        {
            auto it = find(PV2PQList.begin(), PV2PQList.end(), GenIdx);
            // If element was found 
            if (it != PV2PQList.end())
                pGen->GenModel = 3;             // Takes it back to model 3

            GenIdx++;
            pGen = (TGeneratorObj*)with0->Generators.Get_Next();
        }
    }

    // ===========================================================================================
/*  Initializes NCIM is when required, all the structures are created and redefined in this algorithm
    returns the number of buses for the system */
    int TSolutionObj::InitNCIM(int ActorID, bool InitY)
    {
        auto        with0 = ActiveCircuit[ActorID];
        unsigned int NNodes = 0;
        int         i = 0;

        // 1. Calculate the Y Bus, PDE only
        BuildYMatrix(PDE_ONLY, false, ActorID);           // Does not realloc V, I
        InitNCIMVectors(ActorID);
        // 2. Performs a flat solution to get the initial voltage estimation
        ZeroInjCurr(ActorID);                               // All to 0
        GetSourceInjCurrents(ActorID);                      // sources
        // Solve for voltages                               {Note:NodeV[0] = 0 + j0 always}
        if (with0->LogEvents)
            LogThisEvent("Solve Sparse Set DoNCIMSolution ...", ActorID);
        if (InitY)
        {
            // Estimate the initial values for the solution
            SolveSystem(&NodeV[0], ActorID);
            // 3. Move the Y bus matrix into its sparse lib equivalent for linear algebra ops
            DOForceFlatStart(ActorID);
        }
        // Gets the number of buses for the system
        GetSize(hY, &NNodes);

        // 4. Setup the Y admittance matrix equivalent for lienar algebra orperations
        LoadYBusNCIM(ActorID);
        NCIMRdy = true;
        return NNodes;
    }

    // ===========================================================================================
    /* Loads the active Y bus matrix (sparse) into its equivalent for sparse linear algebra ops */
    void TSolutionObj::LoadYBusNCIM(int ActorID)
    {
        unsigned int        NBus = 0, nNZ = 0;
        vector <longInt>    ColPtr,
                            RowIdx;
        vector <complex>    cVals;
        double              re = 0.0, im = 0.0;
        unsigned int        col = 0, Row = 0;

        if (ActiveCircuit[ActorID] == NULL)
            return;
        klusparseset_t myhY = hY;
        if (!myhY)
        {
            DoSimpleMsg("Y Matrix not Built.", 222);
        }
        else
        {
            // this compresses the entries if necessary - no extra work if already solved
            FactorSparseMatrix(myhY);
            GetNNZ(myhY, &nNZ);
            GetSize(myhY, &NBus); // we should already know this

            NCIMYCol.resize(nNZ);
            NCIMYRow.resize(nNZ);
            NCIMY.resize(nNZ);
            GetTripletMatrix(myhY, nNZ, &(NCIMYRow[0]), &(NCIMYCol[0]), &(NCIMY[0]));
        }
    }

    /* Calculates the injection currents based on the voltages at the nodes using
       I = YE, this is later used for estimating the convergence in terms of power
    */

    void TSolutionObj::CalcInjCurr(int ActorID, bool InitGenQ)
    {
        longInt         NBus = 0;
        complex         myvalue = cmplx(0, 0);

        int GSize = (NCIMNodes * 2) + GetNumGenerators(ActorID, InitGenQ);

        // 4. Resize the input/output vectors
        deltaF.resize(GSize);                               // Resizes the InjCurr mismatch vector to host also voltage control
        deltaZ.resize(GSize);                               // Resizes the voltage mismatch vector including delta Q spaces

        for (int j = 0; j < deltaF.size(); j++)
        {
            deltaF[j] = CZero;
        }

        // Multiplies the latest solution (V) by the Y Matrix
        for (int i = 0; i < NCIMY.size(); i++)
        {
            // First the value found
            myvalue = cmul(NCIMY[i], NodeV[NCIMYCol[i] + 1]);
            deltaF[NCIMYRow[i] * 2].re += myvalue.im;
            deltaF[(NCIMYRow[i] * 2) + 1].re += myvalue.re;
        }

        // The first 6 elements are equal to 0
        for (int i = 0; i < 6; i++)
            deltaF[i] = CZero;
    }

    /*  Gets the number of generators in the modeland their number of phases
    Returns the number of generators times their number of phases
    This form allocating memory within the Jacobian matrix for voltage control (PV buses)
    Use it ONLY for initializing the structures within the NCIM algorithm*/
    int TSolutionObj::GetNumGenerators(int ActorID, bool InitQ)
    {
        TGeneratorObj* pGen = nullptr;
        int             NumGens = ActiveCircuit[ActorID]->Generators.get_myNumList(),
            BIdx = 0,
            result = 0;
        vector <int>    BusRefs;
        double          qMax = 0.0,
            qMin = 0.0;
        bool            Add2Limits = false;

        BusRefs.clear();
        // Restarts the generator related values
        for (int idx = 0; idx < pNodeNumGen.size(); idx++)
        {
            pNodeNumGen[idx] = 0;
            pNodeLimits[idx] = CZero;
        }

        if (NumGens > 0)
        {
            pGen = (TGeneratorObj*)ActiveCircuit[ActorID]->Generators.Get_First();
            for (int i = 0; i < NumGens; i++)
            {
                Add2Limits = false;
                if (pGen->Get_Enabled())
                {
                    qMax = (pGen->kvarMax * 1e3) / pGen->Get_NPhases();       // Stores the Q limits for further use
                    qMin = (pGen->kvarMin * 1e3) / pGen->Get_NPhases();
                    if (pGen->GenModel == 3)
                    {
                        if (InitQ)
                        {
                            pGen->GenVars.deltaQNom.resize(pGen->Get_NPhases());
                            pGen->GenVars.deltaQNomPtr = pGen->GenVars.deltaQNom.data();
                            for (int k = 0; k < pGen->Get_NPhases(); k++)
                                pGen->GenVars.deltaQNom[k] = 0.0;                     // Initializes delta Q = 0 for all the generators (PV buses)
                        }
                        if ((pGen->kvarMax == 0) && (pGen->kvarMin == 0))
                        {
                            pGen->GenModel = 4;
                            if (InitQ)
                                PV2PQList.push_back(i);
                        }
                        else
                        {
                            BIdx = Find(&BusRefs, pGen->NodeRef[0]);
                            if (BIdx < 0)
                            {
                                pGen->NCIMIdx = result + 1;                               // It'll be used later by the generator to locate its voltage control signals (PV bus)
                                result = result + pGen->Get_NPhases();
                                for (int j = 1; j <= pGen->Get_NPhases(); j++)
                                    BusRefs.push_back(pGen->NodeRef[j - 1]);
                            }
                            else
                                pGen->NCIMIdx = BIdx + 1;

                            Add2Limits = true;
                        }
                    }
                    else
                    {
                        Add2Limits = (pGen->GenModel == 4);
                    }

                    if (Add2Limits)
                    {
                        for (int j = 1; j <= pGen->Get_NPhases(); j++)
                        {
                            pNodeLimits[pGen->NodeRef[j - 1]] = cadd(pNodeLimits[pGen->NodeRef[j - 1]], cmplx(qMax, qMin));
                            pNodeNumGen[pGen->NodeRef[j - 1]]++;
                        }
                    }
                }
                pGen = (TGeneratorObj*)ActiveCircuit[ActorID]->Generators.Get_Next();
            }
        }
        return result;
    }

    /*  Updates the reactive power delivery for generators model 3,
        this will be reflected in the next solution step. */
    void TSolutionObj::UpdateGenQ(int ActorID)
    {
        TGeneratorObj* pGen = nullptr;
        auto            with0 = ActiveCircuit[ActorID];
        int             NumGens = with0->Generators.get_myNumList(),
                        GenIdx = 0,             // Index of the generator within the node space
                        Shift = 0;              // shift of the Q delta within the solution space
        complex         Volt = CZero;           // Votlage at the generator's terminals (per phase)
        vector <double> QDelta;                 // Vector to copy deltaZ and assign Q updates incrementally
        double          qMax = 0.0,             // For storing the Q max limit of the active generator
                        qMin = 0.0,             // For storing the Q min limit of the active generator
                        GenQ = 0.0,             // Temporary register for storing the unbound expected Q for the active generator  
                        VNode = 0.0,            // To remporarily store the voltage at the active Node
                        myVMax = 0.0;           // Stores the active generator's scheduled voltage
        bool            myPVOK = true,
            myPQOK = true;
        vector<int>     qNodeRef,
                        qNodeRefPQ,
                        PQChecked;
        int             BIdx = 0;

        if (NumGens > 0)
        {
            GenIdx = (ActiveCircuit[ActorID]->NumNodes * 2);

            QDelta.clear();
            QDelta.push_back(0);                        // leaves the first one as zero, to avoid subtractions in the below
            for (int i = GenIdx; i < deltaZ.size(); i++)
                QDelta.push_back(-1.0 * deltaZ[i].re);         // Moves deltaZ (only delta Q section) into the backup vector

            qNodeRef.clear();
            qNodeRefPQ.clear();
            PQChecked.clear();
            pGen = (TGeneratorObj*)ActiveCircuit[ActorID]->Generators.Get_First();
            for (int i = 0; i < NumGens; i++)
            {
                if (pGen->Get_Enabled())
                {
                    if (pGen->GenModel == 3)
                    {
                        myPVOK = true;
                        BIdx = Find(&qNodeRef, pGen->NodeRef[0]);
                        if (BIdx < 0)
                        {
                            for (int j = 0; j < pGen->Get_NPhases(); j++)
                            {
                                qMax = pNodeLimits[pGen->NodeRef[j]].re; // gets the upper kvar limit per phase
                                qMin = pNodeLimits[pGen->NodeRef[j]].im; // gets the lower kvar limit per phase
                                // Update the current at the gnerator's terminal for reporting purposes

                                Volt = NodeV[pGen->NodeRef[j]];
                                // Updates Q per generator
                                Shift = pGen->NCIMIdx + j;
                                GenQ = pGen->GenVars.deltaQNom[j] + (QDelta[Shift] * GenGainNCIM);

                                if (!IgnoreQLimit)
                                {
                                    if (GenQ >= 0)
                                        myPVOK = myPVOK && (GenQ < qMax);
                                    else
                                        myPVOK = myPVOK && (GenQ > qMin);
                                }
                                else
                                {
                                    if ((pGen->kvarMax == 0) && (pGen->kvarMin == 0))  // this if the limits are 0
                                        GenQ = 0;
                                }
                                QDelta[Shift] = 0;
                                pGen->GenVars.deltaQNom[j] = GenQ;
                                pGen->Iterminal[j] = cnegate(conjg(cdiv(cmplx(pGen->GenVars.Pnominalperphase, pGen->GenVars.deltaQNom[j]), Volt)));
                            }
                        }
                        else
                            myPVOK = false;

                        //-------------- Changes the model type for generator if needed ----------------------------
                        if (!myPVOK)
                        {
                            pGen->GenModel = 4;                     // If exceeds the limits changes the generator to model 4 (PQ bus)
                            auto& with3 = pGen->GenVars;

                            if (BIdx < 0)
                            {   // add all the node refs to the temp array if not there already
                                for (int j = 0; j < pGen->Get_NPhases(); j++)
                                    qNodeRef.push_back(pGen->NodeRef[j]);

                                qMax = pNodeLimits[pGen->NodeRef[0]].re; // gets the upper kvar limit per phase
                                qMin = pNodeLimits[pGen->NodeRef[0]].im; // gets the lower kvar limit per phase
                            }
                            else
                            {
                                qMax = 0.0;
                                qMin = 0.0;
                            }
                            for (int j = 0; j < pGen->Get_NPhases(); j++)
                            {
                                if (with3.deltaQNom[0] >= 0)            // and fixes the values for the next solution try
                                    with3.deltaQNom[j] = qMax;
                                else
                                    with3.deltaQNom[j] = qMin;
                            }

                            PV2PQList.push_back(i);
                        }
                    }
                    else
                    {
                        if (pGen->GenModel == 4)
                        {
                            if ((pGen->kvarMax != 0) && (pGen->kvarMin != 0))
                            {
                                myPQOK = true;
                                BIdx = Find(&qNodeRefPQ, pGen->NodeRef[0]);
                                if (BIdx < 0)
                                {
                                    int Checked = Find(&PQChecked, pGen->NodeRef[0]);
                                    if (Checked < 0)
                                    {
                                        myVMax = pGen->VBase * pGen->Vpu;
                                        for (int j = 0; j < pGen->Get_NPhases(); j++)
                                        {
                                            Volt = NodeV[pGen->NodeRef[j]];
                                            VNode = ctopolar(Volt).mag;
                                            if (pGen->GenVars.deltaQNom[0] > 0)
                                                myPQOK = myPQOK && (VNode <= myVMax);
                                            else
                                                myPQOK = myPQOK && (VNode >= myVMax);

                                            PQChecked.push_back(pGen->NodeRef[j]);
                                        }
                                    }
                                }
                                else
                                    myPQOK = false;             // belongs to a cluster and needs to be changed
                                //----------------------------- this in case we need to go back to PV ---------------------------
                                if (!myPQOK)
                                {
                                    pGen->GenModel = 3;
                                    auto& with3 = pGen->GenVars;

                                    for (int j = 0; j < pGen->Get_NPhases(); j++)
                                    {
                                        if (BIdx < 0)
                                        {
                                            qMax = pNodeLimits[pGen->NodeRef[j]].re; // gets the upper kvar limit per phase
                                            qMin = pNodeLimits[pGen->NodeRef[j]].im; // gets the lower kvar limit per phase
                                            qNodeRefPQ.push_back(pGen->NodeRef[j]);
                                        }
                                        else
                                        {
                                            qMax = 0;                                   // If it's part of a cluster it needs to inject only P 
                                            qMin = 0;
                                        }
                                        if (with3.deltaQNom[0] >= 0)                    // and fixes the values for the next solution try
                                            with3.deltaQNom[j] = qMax;
                                        else
                                            with3.deltaQNom[j] = qMin;
                                    }

                                    int PQIdx = Find(&PV2PQList, i);
                                    if (PQIdx >= 0)
                                    {   // If the generator is indexed in the list of converter PV buses, remove it
                                        vector <int> IdxTmp;
                                        IdxTmp.clear();
                                        for (int j = 0; j < PV2PQList.size(); j++)
                                        {
                                            if (j != PQIdx)
                                                IdxTmp.push_back(PV2PQList[j]);
                                        }
                                        PV2PQList = IdxTmp;
                                        IdxTmp.clear();
                                    }
                                }
                            }
                        }
                        // Update currents for all the other gen models
                        for (int j = 0; j < pGen->Get_NPhases(); j++)
                        {
                            Volt = NodeV[pGen->NodeRef[j]];
                            pGen->Iterminal[j] = cnegate(conjg(cdiv(cmplx(pGen->GenVars.Pnominalperphase, pGen->GenVars.deltaQNom[0]), Volt)));
                        }
                    }
                }
                pGen = (TGeneratorObj*)ActiveCircuit[ActorID]->Generators.Get_Next();
            }
        }
    }

    // ===========================================================================================
    /* Builds the Jacobian matrix using the data already allocated within the Y Bus matrix */
    void TSolutionObj::BuildJacobian(int ActorID)
    {
        int GCoords[4][2] = { {0, 0}, {0, 1}, {1, 0}, {1, 1} };
        double Values[4] = { 0 ,0 ,0 ,0 };
        int GRow = 0,
            GCol = 0;
        complex myValue = cmplx(0, 0);

        if (Jacobian != nullptr)
        {
            DeleteSparseSet(Jacobian);
            Jacobian = nullptr;
        }
        Jacobian = NewSparseSet(deltaF.size());
        for (int i = 0; i < NCIMY.size(); i++)
        {
            GRow = NCIMYRow[i] * 2;
            GCol = NCIMYCol[i] * 2;
            if ((GRow == GCol) && (GRow < 6))
            {
                // This is a diagonal for the swing bus, always 1
                myValue.re = 1;
                GRow++;                                                     // Needed to match with the indexes within the library
                GCol++;
                SetMatrixElement(Jacobian, GRow, GCol, &myValue);
                SetMatrixElement(Jacobian, GRow + 1, GCol + 1, &myValue);
            }
            else
            {
                if ((GRow >= 6) && (GCol >= 6))                             // Elements beyond the swing bus
                {
                    Values[0] = NCIMY[i].im;                // B
                    Values[1] = NCIMY[i].re;                // G
                    Values[2] = NCIMY[i].re;                // G
                    Values[3] = (-1) * NCIMY[i].im;         // -B
                    GRow++;                                                 // Needed to match with the indexes within the library
                    GCol++;
                    for (int j = 0; j < 4; j++)
                    {
                        myValue.re = Values[j];
                        SetMatrixElement(Jacobian, (GCoords[j][0] + GRow), (GCoords[j][1] + GCol), &myValue);
                    }
                }
            }
        }

        // Add the Voltage regulation cells to the Jacobian for later use by PV buses
        // Update 03/05/2024 - not needed any more

        TGeneratorObj* pGen;
        int NumGens = ActiveCircuit[ActorID]->Generators.get_myNumList();

        if (NumGens > 0)
        {
            pGen = (TGeneratorObj*)ActiveCircuit[ActorID]->Generators.Get_First();
            for (int i = 0; i < NumGens; i++)
            {
                if (pGen->Get_Enabled() && (pGen->GenModel == 3))
                    pGen->InitPVBusJac(ActorID);

                pGen = (TGeneratorObj*)ActiveCircuit[ActorID]->Generators.Get_Next();
            }
        }

        // Clears the total power vector
        for (int j = 0; j < pNodePower.size(); j++)
        {
            pNodePower[j] = CZero;
            pGenPower[j] = CZero;
            pNodeType[j] = PQ_Node;
        }
    }

    // ===========================================================================================

    void TSolutionObj::DoPFLOWsolution( int ActorID )
    {
      SolutionCount++;    //Unique number for this solution
      if ( VoltageBaseChanged )
        InitializeNodeVbase( ActorID ); // for convergence test
      if ( ! SolutionInitialized )
      {
        if ( ActiveCircuit[ActorID]->LogEvents )
          LogThisEvent( "Initializing Solution", ActorID );
        try
        {
            //SolveZeroLoadSnapShot;
          SolveYDirect( ActorID );  // 8-14-06 This should give a better answer than zero load snapshot
        }
        catch( std::exception & E )
        {
        {
                DoThreadSafeMsg("From DoPFLOWsolution.SolveYDirect: " + CRLF + (std::string) E.what() + CheckYMatrixforZeroes(ActorID), 7072);
          //throw @ESolveError ::( "Aborting" );
        }
        }
        if ( SolutionAbort )
          return; // Initialization can result in abort
        try
        {
            if (Algorithm != NCIMSOLVE)
                SetGeneratordQdV( ActorID );  // Set dQdV for Model 3 generators
        }
        catch( std::exception & E )
        {
        {
          DoThreadSafeMsg( "From DoPFLOWsolution.SetGeneratordQdV: " + CRLF + (std::string) E.what() + CheckYMatrixforZeroes( ActorID ), 7073 );
          //throw @ESolveError ::( "Aborting" );
        }
        }

            /* The above resets the active sparse set to hY */
        SolutionInitialized = true;
      }
      switch ( Algorithm )
      {
        case    NEWTONSOLVE:
            DoNewtonSolution( ActorID );
        break;
        case    NCIMSOLVE:
            DoNCIMSolution(ActorID);
        break;
        default:
            DoNormalSolution(ActorID);
        break;
      }
      ActiveCircuit[ActorID]->Issolved = ConvergedFlag;
      LastSolutionWasDirect = false;
    }

    // ===========================================================================================



    int TSolutionObj::SolveZeroLoadSnapShot( int ActorID )

    // Solve without load for initialization purposes;

    {
      int result = 0;
      result = 0;
      if ( SystemYChanged || SeriesYInvalid )
      {
        BuildYMatrix( SERIESONLY, true, ActorID );   // Side Effect: Allocates V
      }
      pTBusArray withtmp = ActiveCircuit[ActorID]->Buses;
      SolutionCount++;    //Unique number for this solution
      ZeroInjCurr( ActorID );   // Side Effect: Allocates InjCurr
      GetSourceInjCurrents( ActorID );    // Vsource, Isource and VCCS only

        /*Make the series Y matrix the active matrix*/
      if ( hYseries == 0 )
          DoThreadSafeMsg( "Series Y matrix not built yet in SolveZeroLoadSnapshot." , 50004);
      hY = hYseries;
      if ( ActiveCircuit[ActiveActor]->LogEvents )
        LogThisEvent( "Solve Sparse Set ZeroLoadSnapshot ...", ActorID );
      SolveSystem( &NodeV[0], ActorID);  // also sets voltages in radial part of the circuit if radial solution

        /* Reset the main system Y as the solution matrix*/
      if ( hYsystem && ! SolutionAbort )
        hY = hYsystem;
      return result;
    }

    // ===========================================================================================



    void TSolutionObj::SetVoltageBases( int ActorID )

    // Set voltage bases using voltage at first node (phase) of a bus

    {
      int i = 0;
      bool bZoneCalc = false, bZoneLock = false;
      try
      {
        // don't allow the meter zones to auto-build in this load flow solution, because the
        // voltage bases are not available yet
        bZoneCalc = ActiveCircuit[ActorID]->MeterZonesComputed;
        bZoneLock = ActiveCircuit[ActorID]->ZonesLocked;
        ActiveCircuit[ActorID]->MeterZonesComputed = true;
        ActiveCircuit[ActorID]->ZonesLocked = true;
        SolveZeroLoadSnapShot( ActorID );
        /*# with ActiveCircuit[ActorID] do */
        {
          auto with0 = ActiveCircuit[ActorID];
          for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            /*# with Buses^[i] do */
            {
              auto& with1 = with0->Buses[i - 1];
              with1->kVBase = NearestBasekV( cabs( NodeV[ with1->GetRef( 1 ) ] ) * 0.001732 ) / SQRT3;
            }
        }  // l-n base kV
        InitializeNodeVbase( ActorID );      // for convergence test
        ActiveCircuit[ActorID]->Issolved = true;

        // now build the meter zones
        ActiveCircuit[ActorID]->MeterZonesComputed = bZoneCalc;
        ActiveCircuit[ActorID]->ZonesLocked = bZoneLock;
        ActiveCircuit[ActorID]->DoResetMeterZones( ActorID );
      }
      catch( std::exception & E )
      {
      {
        DoSimpleMsg( "From SetVoltageBases.SolveZeroLoadSnapShot: " + CRLF + (std::string) E.what() + CheckYMatrixforZeroes( ActorID ), 7075 );
        //throw @ESolveError ::( "Aborting" );
      }
      }
    }


    void TSolutionObj::SnapShotInit( int ActorID )
    {
      SetGeneratorDispRef( ActorID );
      ControlIteration = 0;
      ControlActionsDone = false;
      MostIterationsDone = 0;
      LoadsNeedUpdating = true;  // Force the loads to update at least once
    }


    void TSolutionObj::CheckControls( int ActorID )
    {
      int i = 0;
      if ( ! ADiakoptics || ( ActorID != 1 ) )
      {
        if ( ControlIteration < MaxControlIterations )
        {
          if ( ConvergedFlag )
          {
            if ( ActiveCircuit[ActorID]->LogEvents )
              LogThisEvent( "Control Iteration " + IntToStr( ControlIteration ), ActorID );
            Sample_DoControlActions( ActorID );
            Check_Fault_Status( ActorID );
          }
          else
            ControlActionsDone = true; // Stop solution process if failure to converge
        }
        if ( SystemYChanged )
        {
            if (Algorithm == NCIMSOLVE)
                NCIMRdy = false;
            else
                BuildYMatrix( WHOLEMATRIX, false, ActorID ); // Rebuild Y matrix, but V stays same
            
        }
        /*by Dahei*/
        if ( NodeYiiEmpty )
          Get_Yiibus();
      }
      else
      {
        if ( ControlIteration < MaxControlIterations )
        {
          if ( ActiveCircuit[ActorID]->LogEvents )
            LogThisEvent( "Control Iteration " + IntToStr( ControlIteration ), ActorID );
          SendCmd2Actors( DO_CTRL_ACTIONS );
          // Checks if there are pending ctrl actions at the actors
          ControlActionsDone = true;
          for ( int stop = NumOfActors, i = 2; i <= stop; i++)
            ControlActionsDone = ControlActionsDone && ActiveCircuit[i]->Solution->ControlActionsDone;
        }
      }
    }

    // ===========================================================================================



    int TSolutionObj::SolveAD( int ActorID, bool Initialize )  // solves a step for Adiakoptics locally

    {
      int result = 0;
      int i = 0;
      if ( Initialize )
      {
        ZeroInjCurr( ActorID );
        GetSourceInjCurrents( ActorID );  // sources
        if ( ADiak_PCInj )
        {
          LoadsNeedUpdating = true;  // Force the loads to update at least once
          GetPCInjCurr( ActorID );  // Get the injection currents from all the power conversion devices and feeders
        }
        else
          if ( IsDynamicModel || IsHarmonicModel )
          {
            LoadsNeedUpdating = true;  // Force the loads to update at least once
            GetPCInjCurr( ActorID ); // for direct solve
          }
         // The above call could change the primitive Y matrix, so have to check
        // The above call could change the primitive Y matrix, so have to check
        if ( SystemYChanged )
        {
          BuildYMatrix( WHOLEMATRIX, false, ActorID );  // Does not realloc V, I
        }
        /*by Dahei*/
        if ( NodeYiiEmpty )
          Get_Yiibus();
        if ( UseAuxCurrents )
          AddInAuxCurrents( NORMALSOLVE, ActorID );
      }
      else
        UpdateISrc( ActorID );

      // Solve for voltages                      {Note:NodeV[0] = 0 + j0 always}
      SolveSystem( &(ActiveCircuit[1]->Solution->NodeV[0]), ActorID);
      LoadsNeedUpdating = false;
      LastSolutionWasDirect = true;
      ActiveCircuit[ActorID]->Issolved = true;
      return result;
    }

    // ===========================================================================================



    int TSolutionObj::SolveSnap( int ActorID )  // solve for now once

    {
      longInt result = 0;
      int TotalIterations = 0;
    //      if Solution then
      SnapShotInit( ActorID );
      TotalIterations = 0;
      QueryPerformanceCounter( &SolveStartTime );
      do
      {
        ControlIteration++;
        result = SolveCircuit( ActorID );  // Do circuit solution w/o checking controls
           /*Now Check controls*/
//        if ( IsDLL )
//         Fire_CheckControls;
        CheckControls( ActorID );

           /*For reporting max iterations per control iteration*/
        if ( Iteration > MostIterationsDone )
          MostIterationsDone = Iteration;
        TotalIterations = TotalIterations + Iteration;
      }
      while ( ! ( ControlActionsDone || ( ControlIteration >= MaxControlIterations ) ) );
      if ( ! ControlActionsDone && ( ControlIteration >= MaxControlIterations ) )
      {
        DoThreadSafeMsg( "Warning Max Control Iterations Exceeded. " + CRLF + "Tip: Show Eventlog to debug control settings.", 485 );
        SolutionAbort = true;   // this will stop this message in dynamic power flow modes
      }
      if ( ActiveCircuit[ActorID]->LogEvents )
        LogThisEvent( "Solution Done", ActorID );
      //if ( isDLL )
      //  Fire_StepControls;
      QueryPerformanceCounter( &SolveEndtime );
#ifndef windows
      Solve_Time_Elapsed = SolveEndtime - SolveStartTime;
#else
      Solve_Time_Elapsed = ( double( ( SolveEndtime.QuadPart ) - ( SolveStartTime.QuadPart ) ) / CPU_Freq ) * 1000000;
#endif
      Iteration = TotalIterations;  /* so that it reports a more interesting number */
      return result;
    }

    // ===========================================================================================



    int TSolutionObj::SolveDirect( int ActorID )  // solve for now once, direct solution

    {
      int result = 0;
      result = 0;
      LoadsNeedUpdating = true;  // Force possible update of loads and generators
      QueryPerformanceCounter( &SolveStartTime );
      SolutionCount++;   // Unique number for this solution
      if ( ! ADiakoptics || ( ActorID != 1 ) )
      {
        if ( SystemYChanged )
        {
          BuildYMatrix( WHOLEMATRIX, true, ActorID );   // Side Effect: Allocates V
        }
        /*by Dahei*/
        if ( NodeYiiEmpty )
          Get_Yiibus();  //
        ZeroInjCurr( ActorID );   // Side Effect: Allocates InjCurr
        GetSourceInjCurrents( ActorID );

        // Pick up PCELEMENT injections for Harmonics mode and Dynamics mode
        // Ignore these injections for powerflow; Use only admittance in Y matrix
        if ( IsDynamicModel || IsHarmonicModel )
          GetPCInjCurr( ActorID );
        if   // Solve with Zero injection current
        ( SolveSystem( &NodeV[0], ActorID) == 1)
        {
          ActiveCircuit[ActorID]->Issolved = true;
          ConvergedFlag = true;
        }
      }
      else
      {
        ADiak_PCInj = false;
        Solve_Diakoptics( );             // A-Diakoptics
        ActiveCircuit[ActorID]->Issolved = true;
        ConvergedFlag = true;
      }
      QueryPerformanceCounter( &SolveEndtime );
#ifndef windows
      Solve_Time_Elapsed = SolveEndtime - SolveStartTime;
#else
      Solve_Time_Elapsed = ( double( ( SolveEndtime.QuadPart - SolveStartTime.QuadPart ) ) / CPU_Freq ) * 1000000;
#endif
      Total_Time_Elapsed = Total_Time_Elapsed + Solve_Time_Elapsed;
      Iteration = 1;
      LastSolutionWasDirect = true;
      return result;
    }


    int TSolutionObj::SolveCircuit( int ActorID )
    {
      int result = 0;
      result = 0;
      if ( LoadModel == ADMITTANCE )
        try
        {
          SolveDirect( ActorID );     // no sense horsing around when it's all admittance
        }
        catch( std::exception & E )
        {
        {
          DoThreadSafeMsg( "From SolveSnap.SolveDirect: " + CRLF + (std::string) E.what() + CheckYMatrixforZeroes( ActorID ), 7075 );
          //throw @ESolveError ::( "Aborting" );
        }
        }
      else
      {
        try
        {
          if ( SystemYChanged )
          {
            if ( ! ADiakoptics || ( ActorID != 1 ) )
              BuildYMatrix( WHOLEMATRIX, true, ActorID );   // Side Effect: Allocates V
          }
                  /*by Dahei: Get Y matrix for solution*/
          if ( NodeYiiEmpty )
            Get_Yiibus();   //IF SystemYChanged
                  /*-------------------------------------*/
          DoPFLOWsolution( ActorID );
        }
        catch( std::exception & E )
        {
        {
          DoThreadSafeMsg( "From SolveSnap.DoPflowSolution: " + CRLF + (std::string) E.what() + CheckYMatrixforZeroes( ActorID ), 7074 );
          //throw @ESolveError ::( "Aborting" );
        }
        }
      }
      return result;
    }

    // ===========================================================================================



    void TSolutionObj::ZeroInjCurr( int ActorID )
    {
      int i = 0;
      for ( int stop = ActiveCircuit[ActorID]->NumNodes, i = 0; i <= stop; i++)
        Currents[i] = CZero;
    }
    // ===========================================================================================



    void TSolutionObj::Upload2IncMatrix( )
    {
      int CIdx = 0;
      // Uploads the values to the incidence matrix
      IncMat.Insert( ( ActiveIncCell[0] - 1 ), ( ActiveIncCell[1] - 2 ), ActiveIncCell[2] );
      ActiveIncCell[2] = - 1;
    }
    // ===========================================================================================



    void TSolutionObj::AddLines2IncMatrix( int ActorID )
    {
      String LineBus;
      TLineObj* elem;
      int TermIdx = 0, CIdx = 0;
      size_t BusdotIdx = 0;
      bool EndFlag = false;
      int counter = 0;
    // This rouitne adds the Lines to the incidence matrix vectors
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          elem = (TLineObj*) with0->Lines.Get_First();
    //    Counter     :=  0;
          while ( elem != NULL )
          {
            if ( elem->Get_Enabled() )
            {
              ActiveIncCell[2] = 1;
              temp_counter++;
              Inc_Mat_Rows.resize(temp_counter);
              Inc_Mat_Rows[temp_counter - 1] = "Line." + elem->get_Name();
              for ( int stop = 2, TermIdx = 1; TermIdx <= stop; TermIdx++)
              {
                LineBus = elem->GetBus( TermIdx );
                BusdotIdx = LineBus.find( '.');
                if ( BusdotIdx != String::npos )
                  LineBus = LineBus.substr( 0, BusdotIdx );  // removes the dot from the Bus Name
                // Evaluates the position of the Bus in the array
                ActiveIncCell[1] = 1;
                EndFlag = true;
                while ( ( ActiveIncCell[1] <= with0->NumBuses ) && ( EndFlag ) )
                {
                  if ( LineBus == with0->BusList.Get( ActiveIncCell[1] ) )
                    EndFlag = false;
                  ActiveIncCell[1] = ActiveIncCell[1] + 1;
                }
                Upload2IncMatrix();
    //            inc(Counter);
              }
              ActiveIncCell[0]++;
            }
            else
            {
              counter = 0;
            }
            elem = (TLineObj*) with0->Lines.Get_Next();
          }
    //    Counter :=  Counter;
        }
      }
    }
    // ===========================================================================================



    void TSolutionObj::AddXfmr2IncMatrix( int ActorID )
    {
      String LineBus;
      TTransfObj* elem;
      int TermIdx = 0, CIdx = 0;
      size_t BusdotIdx = 0;
      bool EndFlag = false;
      int counter = 0;
    // This rouitne adds the Transformers to the incidence matrix vectors
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          TPointerList& lst = ActiveCircuit[ActorID]->Transformers;
          elem = (TTransfObj*) lst.Get_First();
          while ( elem != NULL )
          {
            if ( elem->Get_Enabled() )
            {
              ActiveIncCell[2] = 1;
              temp_counter++;
              Inc_Mat_Rows.resize(temp_counter);
              Inc_Mat_Rows[temp_counter - 1] = "Transformer." + elem->get_Name();
              for ( int stop = elem->get_NumWindings(), TermIdx = 1; TermIdx <= stop; TermIdx++)
              {
                LineBus = elem->GetBus( TermIdx );
                BusdotIdx = LineBus.find( '.' );
                if ( BusdotIdx != String::npos )
                  LineBus = LineBus.substr( 0, BusdotIdx );  // removes the dot from the Bus Name
                // Evaluates the position of the Bus in the array
                ActiveIncCell[1] = 1;
                EndFlag = true;
                while ( ( ActiveIncCell[1] <= with0->NumBuses ) && ( EndFlag ) )
                {
                  if ( LineBus == with0->BusList.Get( ActiveIncCell[1] ) )
                    EndFlag = false;
                  ActiveIncCell[1] = ActiveIncCell[1] + 1;
                }
                Upload2IncMatrix();
              }
              ActiveIncCell[0]++;
            }
            elem = (TTransfObj*) lst.Get_Next();
          }
        }
      }
    }

    // ===========================================================================================



    void TSolutionObj::AddSeriesCap2IncMatrix( int ActorID )
    {
      String CapBus;
      TCapacitorObj* elem;
      int CapTermIdx = 0, CIdx = 0;
      size_t BusdotIdx = 0;
      bool CapEndFlag = false;
    // This rouitne adds the series capacitors to the incidence matrix vectors
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          TPointerList& lst = with0->ShuntCapacitors;
          elem = (TCapacitorObj*) lst.Get_First();
          while ( elem != NULL )
          {
            if ( elem->Get_NumTerm() > 1 )
            {
              if ( elem->Get_Enabled() )
              {
                temp_counter++;
                Inc_Mat_Rows.resize(temp_counter);
                Inc_Mat_Rows[temp_counter - 1] = "Capacitor." + elem->get_Name();
                ActiveIncCell[2] = 1;
                for ( int stop = 2, CapTermIdx = 1; CapTermIdx <= stop; CapTermIdx++)
                {
                  CapBus = elem->GetBus( CapTermIdx );
                  BusdotIdx = CapBus.find( '.' );
                  if ( BusdotIdx != String::npos )
                    CapBus = CapBus.substr( 0, BusdotIdx );  // removes the dot from the Bus Name
                // Evaluates the position of the Bus in the array
                  ActiveIncCell[1] = 1;
                  CapEndFlag = true;
                  while ( ( ActiveIncCell[1] <= with0->NumBuses ) && ( CapEndFlag ) )
                  {
                    if ( CapBus == with0->BusList.Get( ActiveIncCell[1] ) )
                      CapEndFlag = false;
                    ActiveIncCell[1] = ActiveIncCell[1] + 1;
                  }
                  Upload2IncMatrix();
                }
                ActiveIncCell[0]++;
              }
            }
            elem = (TCapacitorObj*) lst.Get_Next();
          }
        }
      }
    }
    // ===========================================================================================



    void TSolutionObj::AddSeriesReac2IncMatrix( int ActorID )
    {
      String RBus;
      int elem = 0, DevClassIndex = 0;
      int TermIdx = 0, CIdx = 0;
      size_t BusdotIdx = 0;
      bool EndFlag = false;
    // This rouitne adds the series reactors to the incidence matrix vectors
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          DevClassIndex = ClassNames[ActorID].Find( "reactor" );
          LastClassReferenced[ActorID] = DevClassIndex;
          ActiveDSSClass[ActorID] = (TDSSClass*) DSSClassList[ActorID].Get( LastClassReferenced[ActorID] );
          elem = ActiveDSSClass[ActorID]->Get_First();
          while ( elem != 0 )
          {
            RBus = with0->get_FActiveCktElement()->GetBus( 2 );
            BusdotIdx = RBus.find( ".0" );
            if ( BusdotIdx == String::npos )
            {
              temp_counter++;
              Inc_Mat_Rows.resize(temp_counter);
              Inc_Mat_Rows[temp_counter - 1] = "Reactor." + with0->get_FActiveCktElement()->get_Name();
              ActiveIncCell[2] = 1;
              for ( int stop = 2, TermIdx = 1; TermIdx <= stop; TermIdx++)
              {
                RBus = with0->get_FActiveCktElement()->GetBus( TermIdx );
                BusdotIdx = RBus.find( '.' );
                if ( BusdotIdx != String::npos )
                  RBus = RBus.substr( 0, BusdotIdx );  // removes the dot from the Bus Name
              // Evaluates the position of the Bus in the array
                ActiveIncCell[1] = 1;
                EndFlag = true;
                while ( ( ActiveIncCell[1] <= with0->NumBuses ) && ( EndFlag ) )
                {
                  if ( RBus == with0->BusList.Get( ActiveIncCell[1] ) )
                    EndFlag = false;
                  ActiveIncCell[1] = ActiveIncCell[1] + 1;
                }
                Upload2IncMatrix();
              }
            }
            elem = ActiveDSSClass[ActorID]->Get_Next();
            ActiveIncCell[0]++;
          }
        }
      }
    }
    //*********Routine for extracting the Branch to Node incidence matrix***********
    //*     The order depends on the way the lines, xfmr, series cap and reactors  *
    //******************************************************************************


    void TSolutionObj::Calc_Inc_Matrix( int ActorID )
    {
      int dlong = 0;
      // If the sparse matrix obj doesn't exists creates it, otherwise deletes the content
      //if ( IncMat == NULL )
        IncMat = Tsparse_matrix();
      //else
        IncMat.Reset();
      if ( ActiveCircuit[ActorID] != NULL )
        /*# with ActiveCircuit[ActorID] do */
        {
//          TDSSCircuit* with0 = ActiveCircuit[ActorID];
//          {
            temp_counter = 0;
            ActiveIncCell[0] = 1;           // Activates row 1 of the incidence matrix
          // Now we proceed to evaluate the link branches
            AddLines2IncMatrix( ActorID );      // Includes the Lines
            AddXfmr2IncMatrix( ActorID );       // Includes the Xfmrs
            AddSeriesCap2IncMatrix( ActorID );  // Includes Series Cap
            AddSeriesReac2IncMatrix( ActorID ); // Includes Series Reactors
            IncMat_Ordered = false;
//          }
        }
    }

    /********************************************************************************
    *          This function returns the index of bus 1 with the Incidence         *
    *                          matrix for the given PDE                            *
    *********************************************************************************/


    int TSolutionObj::get_PDE_Bus1_Location( String myPDE )
    {
      int result = 0;
      int i = 0;
      size_t j = 0;
      String myBUS;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
        {
          with0->SetElementActive( myPDE );
          myBUS = with0->get_FActiveCktElement()->GetBus( 2 );
          j = myBUS.find( '.' );
          if ( j != String::npos )
            myBUS = myBUS.substr( 0, j );
          for ( int stop = (Inc_Mat_Cols.size() - 1), i = 0; i <= stop; i++)
            if ( Inc_Mat_Cols[i] == myBUS )
              break;
          result = i;
        }
      }
      return result;
    }

    /********************************************************************************
    * This function delivers the Row index connected to the Column at the input    *
    *                   Inside the B2N incidence Matrix                            *
    *********************************************************************************/


    int TSolutionObj::get_IncMatrix_Row( int Col )
    {
      int result = 0;
      bool Tflag = false;
      int idx_1 = 0;
      result = - 1;
      Tflag = true;
      for ( int stop = ( IncMat.NZero() - 1 ), idx_1 = 1; idx_1 <= stop; idx_1++)    //Looks for the Column in the IncMatrix
      {
        if ( ( IncMat.data[idx_1][1] == Col ) && Tflag )
        {
          result = IncMat.data[idx_1][0];
          Tflag = false;
        }
      }
      return result;
    }
    /********************************************************************************
    * This function delivers the Column index connected to the Row at the input    *
    *                   Inside the B2N incidence Matrix                            *
    *********************************************************************************/


    int TSolutionObj::get_IncMatrix_Col( int Row )
    {
      int result = 0;
      bool Tflag = false;
      int idx_1 = 0;
      result = - 1;
      Tflag = true;    // Detection Flag
      for ( int stop = ( IncMat.NZero() - 1 ), idx_1 = 1; idx_1 <= stop; idx_1++)    //Looks for the row in the IncMatrix
      {
        if ( ( IncMat.data[idx_1][0] == Row ) && Tflag )
        {
          Active_Cols.resize(2);
          Active_Cols_Idx.resize(2);
          Active_Cols[0] = IncMat.data[idx_1][1];     //Stores the indexes of both columns for the link branch
          Active_Cols[1] = IncMat.data[idx_1 + 1][1]; //In case they need to be used in the future by the caller
          Active_Cols_Idx[0] = IncMat.data[idx_1 - 1][2]; //Stores the indexes of both columns for the link branch
          Active_Cols_Idx[1] = IncMat.data[idx_1][2];     //In case they need to be used in the future by the caller
          result = IncMat.data[idx_1][1];
          Tflag = false;
        }
      }
      return result;
    }

    //*********Routine for extracting the Branch to Node incidence matrix***********
    //*     Organized hierarchically. This routine also calculates the             *
    //*     Levels vector for defining the proximity of the bus to the circuit's   *
    //*     Backbone. To do it, this routine uses the CktTree class                *
    //******************************************************************************


    void TSolutionObj::Calc_Inc_Matrix_Org( int ActorID )
    {


    //  Ftree       : TextFile;                           // For debugging

      TPDElement* pdElem;
      TCktTree topo;
    //  TreeNm,                                           // For debugging
    //  FileRoot,                                         // For debugging

      String PDE_Name;                            // Name of the PDElement

      std::vector<String> PDE_Buses; // Buses of the PDElement
      //String* PDE_Buses;                   

      std::vector <int>  Temp_Array;                  // Local Shared variable
                                                // Current number of levels for the active Bus
                                                      // Default counter
                                                      // Default counter
                                                     // Default counter
                                              // Number of Zero level Buses
                                              // Local Shared variable

      int nLevels = 0                           // PDElements index
      , i = 0, j = 0, j2 = 0, ZeroLevel = 0, BusdotIdx = 0, Row = 0, Col = 0, val = 0, nPDE = 0;
      size_t BusdotIdx2 = 0;
      try
      {
        if ( ActiveCircuit[ActorID] != NULL )
        {
    //      TreeNm := FileRoot + 'TopoTree_Cols.csv';   // For debuging
          topo = ActiveCircuit[ActiveActor]->GetTopology();
          nLevels = 0;
          nPDE = 0;
          Inc_Mat_Cols.resize( 0 );
          //Init the spaser matrix
//          if ( IncMat == NULL )
            IncMat = Tsparse_matrix();
//          else
            IncMat.Reset();
          ActiveIncCell[0] = - 1;           // Activates row 1 of the incidence matrix
          if (( topo.Get_First() != NULL ) )
          {
            pdElem = (TPDElement*) topo.Get_First();
            while (( pdElem != NULL ) )
            {
              nLevels = topo.Get_Level();
              PDE_Name = pdElem->ParentClass->get_myClass_name() + "." + pdElem->get_Name();
    //******************Gets the buses to which the PDE is connected****************
              /*# with ActiveCircuit[ActorID] do */
              {
                TDSSCircuit* with0 = ActiveCircuit[ActorID];
                {
                  with0->SetElementActive( PDE_Name );
                  PDE_Buses.resize(with0->get_FActiveCktElement()->Get_NTerms());
                  for ( int stop = with0->get_FActiveCktElement()->Get_NTerms(), i = 1; i <= stop; i++)
                  {
                    PDE_Buses[i - 1] = with0->get_FActiveCktElement()->GetBus( i );
                    BusdotIdx2 = PDE_Buses[i - 1].find( '.' );
                    if ( BusdotIdx2 != String::npos )
                      PDE_Buses[i - 1] = PDE_Buses[i - 1].substr( 0, BusdotIdx2 );  // removes the dot from the Bus Name
                  }
                  if ( Inc_Mat_Cols.size() == 0 )  //Get_First() iteration so the Cols array will be loaded
                  {
                    Inc_Mat_Cols.resize( 1 );
                    Inc_Mat_levels.resize( 1 );
                    Inc_Mat_Cols[0] = PDE_Buses[0];
                    Inc_Mat_levels[0] = nLevels;
                  }
                  else                               //The Cols array is populated with something
                  {
                    nPDE++;
                    Inc_Mat_Rows.resize( nPDE );
                    Inc_Mat_Rows[nPDE - 1] = PDE_Name;
                    for ( int stop = with0->get_FActiveCktElement()->Get_NTerms() - 1, j = 0; j <= stop; j++)
                    {
                      Row = ActiveIncCell[0];                 //Sets the row
                      BusdotIdx = - 1;               // Flag to not create a new variable
                      for ( int stop = (Inc_Mat_Cols.size() - 1), i = 0; i <= stop; i++)   // Checks if the bus already exists in the Cols array
                        if ( Inc_Mat_Cols[i] == PDE_Buses[j] )
                          BusdotIdx = i;
                      if ( BusdotIdx >= 0 )
                        Col = BusdotIdx;   //Sets the Col
                      else
                      {
                        Inc_Mat_Cols.push_back( PDE_Buses[j] );
                        Inc_Mat_levels.push_back( nLevels );
                        Col = Inc_Mat_Cols.size() - 1; //Sets the Col
                      }
                      if ( j == 0 )
                        val = 1; //Sets the value
                      else
                        val = - 1;
                      IncMat.Insert( Row, Col, val );
                    }
                  }
                }
              }
              ActiveIncCell[0]++;
              pdElem = (TPDElement*) topo.Get_Forward();
            }
          }
    /********************************************************************************
    *   Now the levels array needs to be reprocessed to get the 0 level buses,     *
    *   they are on a continuous path from the feeder head to the feeder end       *
    *********************************************************************************/
          BusdotIdx = MaxIntValue( &Inc_Mat_levels );
          for ( int stop = Inc_Mat_levels.size(), i = 0; i <= stop; i++)
            if ( Inc_Mat_levels[i] == BusdotIdx )
              nLevels = i;
          for ( int stop = BusdotIdx - 1, j = 1; j <= stop; j++)
          {
            for ( int stop = nLevels, i = 0; i <= stop; i++)
            {
              if ( Inc_Mat_levels[i] == j )
                ZeroLevel = i;
            }
            Inc_Mat_levels[ZeroLevel] = 0;
          }
    //**********Normalize the branches of the level between zero level buses********
          BusdotIdx = 0;
          j = 0;
          ZeroLevel = 0;
          Temp_Array.resize( 0 );
          for ( int stop = (Inc_Mat_levels.size() - 1), i = 0; i <= stop; i++)
          {
            if ( Inc_Mat_levels[i] == 0 )
            {
              if ( Temp_Array.size() > 0 )    // The array subset is large enough for
              {                             //Normalizing it
                BusdotIdx = MinIntValue( &Temp_Array ) - 1;
                for ( int stop = ( Temp_Array.size() + ZeroLevel - 1 ), j2 = ZeroLevel; j2 <= stop; j2++)
                  Inc_Mat_levels[j2] = Inc_Mat_levels[j2] - BusdotIdx;
                Temp_Array.resize( 0 );
              }
              ZeroLevel = i + 1;
            }
            else
            {
              Temp_Array.push_back( Inc_Mat_levels[i] );
            }
          }
    //************Verifies is something else was missing at the end*****************
          if ( ZeroLevel < ( Inc_Mat_levels.size() - 1 ) )
          {
            BusdotIdx = 0;                                                // Counter for defining the level
            j = 0;                                                // Stores the previous value (shift reg)
            for ( int stop = (Inc_Mat_levels.size() - 1), j2 = ZeroLevel; j2 <= stop; j2++)
            {
              if ( Inc_Mat_levels[j2] >= j )
                BusdotIdx++;
              else
              {
                ActiveIncCell[1] = get_IncMatrix_Row( j2 );                //Looks for the Column in the IncMatrix
                if ( ActiveIncCell[1] < 0 )                                //Checks if the col was located (just in case)
                  BusdotIdx = 1;
                else
                {
                  ActiveIncCell[2] = get_IncMatrix_Col( ActiveIncCell[1] );  //Looks for the row in the IncMatrix
                  if ( Active_Cols[0] == j2 )
                    BusdotIdx = Inc_Mat_levels[Active_Cols[1]] + 1;
                  else
                    BusdotIdx = Inc_Mat_levels[ActiveIncCell[2]] + 1;
                }
              }
              j = Inc_Mat_levels[j2];
              Inc_Mat_levels[j2] = BusdotIdx;
            }
          }
          IncMat_Ordered = true;
        }
      }
//      __finally
      catch(...)
      {
          // to match with the try def in delphi
      }
    }

    /********************************************************************************
    *           Routine created to empty a recently created folder                 *
    *********************************************************************************/


    void DelFilesFromDir( String Directory, String FileMask, bool DelSubDirs )
    {
      String SourceLst;
      /*
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
    *   This routine evaluates if the current location is the best or if its       *
    *   Necessary to move back one PDE just to cover a wider area                  *
    *********************************************************************************/


    int TSolutionObj::CheckLocationIdx( int Idx )
    {
      int result = 0;
      if ( Inc_Mat_levels[Idx - 1] == 0 )
        result = Idx - 1;
      else
        result = Idx;
      return result;
    }

    //----------------------------------------------------------------------------



    int TDSSSolution::Init( int Handle, int ActorID )
    {
      int result = 0;
      DoSimpleMsg( "Need to implement TSolution.Init", - 1 );
      result = 0;
      return result;
    }

    // ===========================================================================================

    double TSolutionObj::get_Solve_Time_Elapsed()
    {
        return Solve_Time_Elapsed;
    }

    // ===========================================================================================

    double TSolutionObj::get_Total_Solve_Time_Elapsed()
    {
        return Total_Solve_Time_Elapsed;
    }

    // ===========================================================================================

    double TSolutionObj::get_Step_Time_Elapsed()
    {
        return Step_Time_Elapsed;
    }

    // ===========================================================================================

    double TSolutionObj::get_Total_Time_Elapsed()
    {
        return Total_Time_Elapsed;
    }

    // ===========================================================================================



    void TSolutionObj::GetPCInjCurr( int ActorID, bool GFMOnly)
    {
      TDSSCktElement*   pElem = nullptr;
      bool              valid = false;

    /* Get inj currents from all enabled PC devices */
      /*# with ActiveCircuit[ActorID] do */
      {
        auto with0 = ActiveCircuit[ActorID];
        {
          pElem = (TDSSCktElement *) with0->PCElements.Get_First();
          while ( pElem != NULL )
          {
              valid = !(GFMOnly ^ pElem->GFM_Mode);

            /*# with pElem do */
            if ( pElem->Get_Enabled() && valid )
                pElem->InjCurrents( ActorID ); // uses NodeRef to add current into InjCurr Array;
            pElem = (TDSSCktElement*) with0->PCElements.Get_Next();
          }
        }
      }
    }

    void TSolutionObj::DumpProperties( TTextRec& F, bool Complete )
    {
      int i = 0

       // for dumping the matrix in compressed columns
      , j = 0;
      unsigned int p = 0;
      klusparseset_t hY;
      unsigned int nBus = 0, nNZ = 0;
      std::vector < unsigned int > ColPtr;
      std::vector < unsigned int >  RowIdx;
      complex* cVals;
      WriteLn( F, "! OPTIONS" );

      // Inherited DumpProperties(F,Complete);
      Write( F, "! NumNodes = " ); WriteLn( F, ActiveCircuit[ActiveActor]->NumNodes, 0 );

        /*WITH ParentClass Do
         FOR i := 1 to NumProperties Do
         Begin
            Writeln(F,'Set ',PropertyName^[i],'=',PropertyValue^[i]);
         End;
         */
      Write( F, "Set Mode=" ); WriteLn( F, GetSolutionModeID() );
      Write( F, "Set ControlMode=" ); WriteLn( F, GetControlModeID() );
      Write( F, "Set Random=" ); WriteLn( F, GetRandomModeID() );
      Write( F, "Set hour=" ); WriteLn( F, DynaVars.intHour, 0 );
      Write( F, "Set sec=" ); WriteLn( F, Format( "%-g",  DynaVars.T ));
      Write( F, "Set year=" ); WriteLn( F, get_Fyear(), 0 );
      Write( F, "Set frequency=" ); WriteLn( F, Format( "%-g",  get_FFrequency() ));
      Write( F, "Set stepsize=" ); WriteLn( F, Format( "%-g",  DynaVars.h ));
      Write( F, "Set number=" ); WriteLn( F, NumberOfTimes, 0 );
      Write( F, "Set circuit=" ); WriteLn( F, ActiveCircuit[ActiveActor]->Get_Name() );
      Write( F, "Set editor=" ); WriteLn( F, DefaultEditor );
      Write( F, "Set tolerance=" ); WriteLn( F, Format( "%-g",  ConvergenceTolerance ));
      Write( F, "Set maxiterations=" ); WriteLn( F, MaxIterations, 0 );
      Write( F, "Set miniterations=" ); WriteLn( F, MinIterations, 0 );
      Write( F, "Set loadmodel=" ); WriteLn( F, GetLoadModel() );
      Write( F, "Set loadmult=" ); WriteLn( F, Format( "%-g",  ActiveCircuit[ActiveActor]->get_FLoadMultiplier() ));
      Write( F, "Set Normvminpu=" ); WriteLn( F, Format( "%-g",  ActiveCircuit[ActiveActor]->NormalMinVolts ));
      Write( F, "Set Normvmaxpu=" ); WriteLn( F, Format( "%-g",  ActiveCircuit[ActiveActor]->NormalMaxVolts ));
      Write( F, "Set Emergvminpu=" ); WriteLn( F, Format( "%-g",  ActiveCircuit[ActiveActor]->EmergMinVolts ));
      Write( F, "Set Emergvmaxpu=" ); WriteLn( F, Format( "%-g",  ActiveCircuit[ActiveActor]->EmergMaxVolts ));
      Write( F, "Set %mean=" ); WriteLn( F, Format( "%-.4g",  ActiveCircuit[ActiveActor]->DefaultDailyShapeObj->Get_Mean() * 100.0 ));
      Write( F, "Set %stddev=" ); WriteLn( F, Format( "%-.4g",  ActiveCircuit[ActiveActor]->DefaultDailyShapeObj->Get_StdDev() * 100.0 ));
      Write( F, "Set LDCurve=" ); WriteLn( F, ActiveCircuit[ActiveActor]->LoadDurCurve );  // Load Duration Curve
      Write( F, "Set %growth=" ); WriteLn( F, Format( "%-.4g",  ( ( ActiveCircuit[ActiveActor]->DefaultGrowthRate - 1.0 ) * 100.0 ) ));  // default growth rate
      /*# with ActiveCircuit[ActiveActor].AutoAddObj do */
      {
        auto& with1 = ActiveCircuit[ActiveActor]->AutoAddObj;
        Write( F, "Set genkw=" ); WriteLn( F, Format( "%-g",  with1.GenkW ));
        Write( F, "Set genpf=" ); WriteLn( F, Format( "%-g",  with1.GenPF ));
        Write( F, "Set capkvar=" ); WriteLn( F, Format( "%-g",  with1.Capkvar ));
        Write( F, "Set addtype=" );
        switch (with1.AddType )
        {
          case GENADD:
            WriteLn( F, "generator" );
          break;
          case CAPADD:
            WriteLn( F, "capacitor" );
          break;
        }
      }
      Write( F, "Set allowduplicates=" );
      if ( ActiveCircuit[ActiveActor]->DuplicatesAllowed )
        WriteLn( F, "Yes" );
      else
        WriteLn( F, "No" );
      Write( F, "Set zonelock=" );
      if ( ActiveCircuit[ActiveActor]->ZonesLocked )
        WriteLn( F, "Yes" );
      else
        WriteLn( F, "No" );
      Write( F, "Set ueweight=" ); WriteLn( F, ActiveCircuit[ActiveActor]->UEWeight, 8, 2 );
      Write( F, "Set lossweight=" ); WriteLn( F, ActiveCircuit[ActiveActor]->LossWeight, 8, 2 );
      Write( F, "Set ueregs=" ); WriteLn( F, IntArrayToString( ActiveCircuit[ActiveActor]->UEregs, ActiveCircuit[ActiveActor]->NumUEregs ) );
      Write( F, "Set lossregs=" ); WriteLn( F, IntArrayToString( ActiveCircuit[ActiveActor]->LossRegs, ActiveCircuit[ActiveActor]->NumLossRegs ) );
      Write( F, "Set voltagebases=(" );  //  changes the default voltage base rules
      i = 0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
        while ((with0->LegalVoltageBases)[i] > 0.0 )
        {
          Write( F, (with0->LegalVoltageBases)[i], 10, 2 );
          i++;
        }
      }
      WriteLn( F, ')' );
      switch ( Algorithm )
      {
        case NORMALSOLVE:
          WriteLn( F, "Set algorithm=normal" );
        break;
        case NEWTONSOLVE:
          WriteLn( F, "Set algorithm=newton" );
        break;
      }
      Write( F, "Set Trapezoidal=" );
      if ( ActiveCircuit[ActiveActor]->TrapezoidalIntegration )
        WriteLn( F, "yes" );
      else
        WriteLn( F, "no" );
      Write( F, "Set genmult=" ); WriteLn( F, Format( "%-g",  ActiveCircuit[ActiveActor]->GenMultiplier ));
      Write( F, "Set Basefrequency=" ); WriteLn( F, Format( "%-g",  ActiveCircuit[ActiveActor]->Fundamental ));
      Write( F, "Set harmonics=(" );  //  changes the default voltage base rules
      if ( DoAllHarmonics )
        Write( F, "ALL" );
      else
        for ( int stop = HarmonicListSize, i = 1; i <= stop; i++)
          Write( F, Format( "%-g, ",  (HarmonicList)[i] ));
      WriteLn( F, ')' );
      Write( F, "Set maxcontroliter=" ); WriteLn( F, MaxControlIterations, 0 );
      WriteLn( F );
      if ( Complete )
        /*# with ActiveCircuit[ActiveActor] do */
        {
          TDSSCircuit* with2 = ActiveCircuit[ActiveActor];
          {
            hY = /*# Solution::*/ with2->Solution->hY;

          // get the compressed columns out of KLU
            FactorSparseMatrix( hY ); // no extra work if already done
            GetNNZ( hY, &nNZ );
            GetSize( hY, &nBus );
            ColPtr.resize( nBus + 1 );
            RowIdx.resize( nNZ );
            cVals = new complex[nNZ];
            GetCompressedMatrix( hY, nBus + 1, nNZ, &ColPtr[0], &RowIdx[0], cVals);
            WriteLn( F, "System Y Matrix (Lower Triangle by Columns)" );
            WriteLn( F );
            WriteLn( F, "  Row  Col               G               B" );
            WriteLn( F );

          // traverse the compressed column format
            for ( int stop = nBus - 1, j = 0; j <= stop; j++)
            { /// the zero-based column
              for ( int stop = ColPtr[j + 1] - 1, p = ColPtr[j]; p <= stop; p++)
              {
                i = RowIdx[p];  // the zero-based row
                WriteLn( F, Format( "[%4d,%4d] = %12.5g + j%12.5g",  i + 1, j + 1, cVals[p].re, cVals[p].im ));
              }
            }
          }
        }
    }


    complex TSolutionObj::VDiff( int i, int j, int ActorID )
    {
      complex result;
      int k = 0;
      if ( ADiakoptics && ( ActorID != 1 ) )
        result = csub( VoltInActor1( i ), VoltInActor1( j ) );  // V1-V2
      else
        result = csub( NodeV[i], NodeV[j] );  // V1-V2
      return result;
    }

    void TSolutionObj::WriteConvergenceReport(const String Fname)
    {
        int i = 0;
        TTextRec F;
        try
        {
            AssignFile(F, Fname);
            Rewrite(F);
            IOResultToException();
            WriteLn(F);
            WriteLn(F, "-------------------");
            WriteLn(F, "Convergence Report:");
            WriteLn(F, "-------------------");
            WriteLn(F, "\"Bus.Node\", \"Error\", \"|V|\",\"Vbase\"");
            /*# with ActiveCircuit[ActiveActor] do */
            {
                TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
                for (int stop = with0->NumNodes, i = 1; i <= stop; i++)
                    /*# with MapNodeToBus^[i] do */
                {
                    TNodeBus with1 = with0->MapNodeToBus[i-1];
                    {
                        Write(F, '\"'); Write(F, Pad((with0->BusList.Get(with1.BusRef) + "." + IntToStr(with1.NodeNum) + "\""), 18));
                        Write(F, ", "); Write(F, Format("%8.5f", (ErrorSaved)[i]));
                        Write(F, ", "); Write(F, (VmagSaved)[i],  1, 5);
                        Write(F, ", "); Write(F,  (NodeVbase)[i], 1, 5);
                        WriteLn(F);
                    }
                }
            }
            WriteLn(F);
            Write(F, "Max Error = "); WriteLn(F, Format("%8.5f", MaxError));
        //}
        //      __finally
        //      {
        CloseFile(F);
        if (AutoDisplayShowReport)
            FireOffEditor(Fname);
        }
        catch (...)
        {
           // matches with delphi, there is no finally in native C
        }
    }

    // =========================================================================================== =



    void TSolutionObj::SumAllCurrents( int ActorID )
    {
      TDSSCktElement* pElem;
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          pElem = (TDSSCktElement*) with0->CktElements.Get_First();
          while ( pElem != NULL )
          {
            pElem->SumCurrents( ActorID );   // sum terminal currents into system Currents Array
            pElem = (TDSSCktElement*) with0->CktElements.Get_Next();
          }
        }
      }
    }

    // =========================================================================================== =



    void TSolutionObj::DoControlActions( int ActorID )
    {
      int XHour = 0;
      double XSec = 0.0;
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          switch ( ControlMode )
          {
            case CTRLSTATIC:
            {  //  execute the nearest set of control actions but leaves time where it is
              if ( with0->ControlQueue.IsEmpty() )
                ControlActionsDone = true;
              else
                  with0->ControlQueue.DoNearestActions( XHour, XSec, ActorID ); // ignore time advancement
            }
            break;
            case EVENTDRIVEN:
            {  //  execute the nearest set of control actions and advance time to that time
                     // **** Need to update this to set the "Intervalhrs" variable for EnergyMeters for Event-Driven Simulation ****
              if // these arguments are var type
              ( ! with0->ControlQueue.DoNearestActions( DynaVars.intHour, DynaVars.T, ActorID ) )
                ControlActionsDone = true;// Advances time to the next event
            }
            break;
            case TIMEDRIVEN:
            {   // Do all actions having an action time <= specified time
              if ( ! with0->ControlQueue.DoActions( DynaVars.intHour, DynaVars.T, ActorID ) )
                ControlActionsDone = true;
            }
            break;
            case MULTIRATE:
            {  //  execute the nearest set of control actions but leaves time where it is
              if ( ! with0->ControlQueue.DoMultiRate( DynaVars.intHour, DynaVars.T, ActorID ) )
                ControlActionsDone = true;
            }
            break;
          }
        }
      }
    }

    // =========================================================================================== =



    void TSolutionObj::SampleControlDevices( int ActorID )
    {
      TControlElem* ControlDevice;
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          ControlDevice = NULL;
          try
          {
                // Sample all controls and set action times in control Queue
            ControlDevice = (TControlElem*) with0->DSSControls.Get_First();
            while ( ControlDevice != NULL )
            {
              if ( ControlDevice->Get_Enabled() )
                ControlDevice->sample( ActorID );
              ControlDevice = (TControlElem*) with0->DSSControls.Get_Next();
            }
          }
          catch( std::exception & E )
          {
          {
                  DoThreadSafeMsg("Error Sampling Control Device "+ ControlDevice->ParentClass->get_myClass_name() +
                      "." + ControlDevice->get_Name() + " Error = " + (std::string) E.what(), 484);
//            throw @EControlProblem ::( "Solution aborted." );
          }
          }
        }
      }
    }

    // =========================================================================================== =



    void TSolutionObj::Sample_DoControlActions( int ActorID )
    {
      if ( ControlMode == CONTROLSOFF )
        ControlActionsDone = true;
      else
      {
        SampleControlDevices( ActorID );
        DoControlActions( ActorID );

      /*This variable lets control devices know the bus list has changed*/
        ActiveCircuit[ActorID]->Control_BusNameRedefined = false;  // Reset until next change
      }
    }


    int TSolutionObj::Get_SolMode()
    {
        return DynaVars.SolutionMode;
    }

    void TSolutionObj::Set_Mode( const int Value )
    {
      DynaVars.intHour = 0;
      DynaVars.T = 0.0;
      Update_dblHour();
      ActiveCircuit[ActiveActor]->TrapezoidalIntegration = false;
      if ( ! OK_for_Dynamics( Value ) )
        return;
      if ( ! OK_for_Harmonics( Value ) )
        return;
      DynaVars.SolutionMode = Value;
      ControlMode = DefaultControlMode;   // Revert to default mode
      LoadModel = DefaultLoadModel;
      IsDynamicModel = false;
      IsHarmonicModel = false;
      SolutionInitialized = false;   // reinitialize solution when mode set (except dynamics)
      PreserveNodeVoltages = false;  // don't do this unless we have to
      SampleTheMeters = false;

       // Reset defaults for solution modes
      switch ( DynaVars.SolutionMode )
      {
        case PEAKDAY: case DAILYMODE:
        {
          DynaVars.h = 3600.0;
          NumberOfTimes = 24;
          SampleTheMeters = true;
        }
        break;
        case SNAPSHOT:
        {
          IntervalHrs = 1.0;
          NumberOfTimes = 1;
        }
        break;
        case YEARLYMODE:
        {
          IntervalHrs = 1.0;
          DynaVars.h = 3600.0;
          NumberOfTimes = 8760;
          SampleTheMeters = true;
        }
        break;
        case DUTYCYCLE:
        {
          DynaVars.h = 1.0;
          ControlMode = TIMEDRIVEN;
        }
        break;
        case DYNAMICMODE:
        {
          DynaVars.h = 0.001;
          ControlMode = TIMEDRIVEN;
          IsDynamicModel = true;
          PreserveNodeVoltages = true;  // need to do this in case Y changes during this mode
        }
        break;
        case EMPMODE:
        {
          DynaVars.h = 0.001;           // Integration time step
          ControlMode = TIMEDRIVEN;
          IsDynamicModel = true;
          PreserveNodeVoltages = true;  // need to do this in case Y changes during this mode
        }
        break;        
        case EMPDAILYMODE:
        {
          DynaVars.h = 3600.0;           // Integration time step
          NumberOfTimes = 24;
          SampleTheMeters = true;
          // IsDynamicModel = false;
          // PreserveNodeVoltages = true;  // need to do this in case Y changes during this mode
        }
        break;
        case GENERALTIME:
        {
          IntervalHrs = 1.0;
          DynaVars.h = 3600.0;
          NumberOfTimes = 1;  // just one time step per Solve call expected
        }
        break;
        case MONTECARLO1:
        {
          IntervalHrs = 1.0;
          SampleTheMeters = true;
        }
        break;
        case MONTECARLO2:
        {
          DynaVars.h = 3600.0;
          SampleTheMeters = true;
        }
        break;
        case MONTECARLO3:
        {
          IntervalHrs = 1.0;
          SampleTheMeters = true;
        }
        break;
        case MONTEFAULT:
        {
          IsDynamicModel = true;
        }
        break;
        case FAULTSTUDY:
        {
          IsDynamicModel = true;
        }
        break;
        case LOADDURATION1:
        {
          DynaVars.h = 3600.0;
          ActiveCircuit[ActiveActor]->TrapezoidalIntegration = true;
          SampleTheMeters = true;
        }
        break;
        case LOADDURATION2:
        {
          DynaVars.intHour = 1;
          ActiveCircuit[ActiveActor]->TrapezoidalIntegration = true;
          SampleTheMeters = true;
        }
        break;
        case AUTOADDFLAG:
        {
          IntervalHrs = 1.0;
          ActiveCircuit[ActiveActor]->AutoAddObj.ModeChanged = true;
        }
        break;
        case HARMONICMODE:
        {
          ControlMode = CONTROLSOFF;
          IsHarmonicModel = true;
          LoadModel = ADMITTANCE;
          PreserveNodeVoltages = true;  // need to do this in case Y changes during this mode
        }
        break;
        case HARMONICMODET:
        {
          IntervalHrs = 1.0;
          DynaVars.h = 3600.0;
          NumberOfTimes = 1;
          ControlMode = CONTROLSOFF;
          IsHarmonicModel = true;
          LoadModel = ADMITTANCE;
          PreserveNodeVoltages = true;  // need to do this in case Y changes during this mode
        }
        break;
      }

       /*Moved here 9-8-2007 so that mode is changed before reseting monitors, etc.*/
   
       // Reset Meters and Monitors
      MonitorClass[ActiveActor]->ResetAll( ActiveActor );
       /*by Dahei*/
      FMonitorClass[ActiveActor]->ResetAll( ActiveActor );
      EnergyMeterClass[ActiveActor]->ResetAll( ActiveActor );
      DoResetFaults();
      DoResetControls();
    }


    void TSolutionObj::AddInAuxCurrents( int SolveType, int ActorID )
    {
        /*FOR i := 1 to ActiveCircuit[ActiveActor].NumNodes Do Caccum(Currents^[i], AuxCurrents^[i]);*/
        // For Now, only AutoAdd Obj uses this
      if ( DynaVars.SolutionMode == AUTOADDFLAG )
        ActiveCircuit[ActorID]->AutoAddObj.AddCurrents( SolveType, ActorID );
    }

    void TSolutionObj::ZeroAuxCurrents( int ActorID )
    {
      int i = 0;
    //    FOR i := 1 to ActiveCircuit[ActorID].NumNodes Do AuxCurrents^[i] := CZERO;
    }


    void TSolutionObj::Check_Fault_Status( int ActorID )
    {
      TFaultObj* pFault;
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          pFault = (TFaultObj*) with0->Faults.Get_First() ;
          while ( pFault != NULL )
          {
            pFault->CheckStatus( ControlMode, ActorID );
            pFault = ( TFaultObj* ) with0->Faults.Get_Next() ;
          }
        }
      }  /*End With*/
    }


    /* This procedure is called for Solve Direct and any other solution method
      that does not get the injection currents for PC elements normally. In Dynamics mode,
      Generators are voltage sources ...

    Procedure TSolutionObj.GetMachineInjCurrents;

    Var
      pElem:TDSSCktElement;

    begin
         // do machines in Dynamics Mode
         IF   IsDynamicModel THEN
          With ActiveCircuit[ActiveActor] DO  Begin

             pElem := Generators.Get_First();
             WHILE pElem<>nil Do Begin
                 IF pElem.Enabled THEN pElem.InjCurrents; // uses NodeRef to add current into InjCurr Array;
                 pElem := Generators.Get_Next();
             End;

           End;

    end;
    */


    bool TSolutionObj::OK_for_Dynamics( const int Value )
    {
      bool result = false;
      bool ValueIsDynamic = false;
      result = true;
      switch ( Value )
      {
      case MONTEFAULT: case DYNAMICMODE: case FAULTSTUDY: case EMPMODE:
          ValueIsDynamic = true;
        break;
      default:
        ValueIsDynamic = false;
      }

       /*When we go in and out of Dynamics mode, we have to do some special things*/
      if ( IsDynamicModel && ! ValueIsDynamic )
          InvalidateAllPCElements();  // Force Recomp of YPrims when we leave Dynamics mode
      if ( ! IsDynamicModel && ValueIsDynamic )
      {   // see if conditions right for going into dynamics
        if ( ActiveCircuit[ActiveActor]->Issolved )
            switch (Value)
            {
            case EMPMODE:
                break;
                //  Don't initialize states.  All that is handled later, before the run is initiated
            default:    // This remains the same for all other modes: DYNAMICMODE, MONTEFAULT, FAULTSTUDY
                CalcInitialMachineStates();   // set state variables for machines (loads and generators)
            }
        else
        {
               /*Raise Error Message if not solved*/
          DoSimpleMsg( "Circuit must be solved in a non-dynamic mode before entering Dynamics or Fault study modes!" + CRLF + 
              "If you attempted to solve, then the solution has not yet converged.", 486 );
          if ( In_Redirect )
            Redirect_Abort = true;  // Get outta here
          result = false;
        }
      }
      return result;
    }

    bool TSolutionObj::OK_for_Harmonics( const int Value )

     /*When we go in and out of Harmonics mode, we have to do some special things*/
    {
      bool result = false;
      result = true;
      if ( IsHarmonicModel && ! ( ( Value == HARMONICMODE ) || ( Value == HARMONICMODET ) ) )
      {
        InvalidateAllPCElements();  // Force Recomp of YPrims when we leave Harmonics mode
        Set_Frequency(ActiveCircuit[ActiveActor]->Fundamental);   // Resets everything to norm
      }
      if ( ! IsHarmonicModel && ( ( Value == HARMONICMODE ) || ( Value == HARMONICMODET ) ) )
      {   // see if conditions right for going into Harmonics
        if ( ( ActiveCircuit[ActiveActor]->Issolved ) && ( get_FFrequency() == ActiveCircuit[ActiveActor]->Fundamental ) )
        {
          if   // set state variables for machines (loads and generators) and sources
          ( ! InitializeForHarmonics( ActiveActor ) )
          {
            result = false;
            if ( In_Redirect )
              Redirect_Abort = true;  // Get outta here
          }
        }
        else
        {
          DoSimpleMsg( "Circuit must be solved in a fundamental frequency power flow or direct mode before entering Harmonics mode!", 487 );
          if ( In_Redirect )
            Redirect_Abort = true;  // Get outta here
          result = false;
        }
      }
      return result;
    }


    void TSolutionObj::Set_Frequency( const double Value )
    {
      if ( FFrequency != Value )
      {
        FrequencyChanged = true;  // Force Rebuild of all Y Primitives
        SystemYChanged = true;  // Force rebuild of System Y
      }
      FFrequency = Value;
      if ( ActiveCircuit[ActiveActor] != NULL )
        Harmonic = FFrequency / ActiveCircuit[ActiveActor]->Fundamental;  // Make Sure Harmonic stays in synch
    }


    void TSolutionObj::Increment_time( )
    {
      /*# with DynaVars do */
      {
        auto& with0 = DynaVars;
        with0.T = with0.T + with0.h;
        while (with0.T >= 3600.0 )
        {
            with0.intHour++;
            with0.T = with0.T - 3600.0;
        }
        Update_dblHour();
      }
    }


    void TSolutionObj::InitPropertyValues( int ArrayOffset )
    {
      Set_PropertyValue(1, "");
      inherited::InitPropertyValues( NumPropsThisClass );
    }


    void TSolutionObj::Set_Year( const int Value )
    {
      if ( DIFilesAreOpen[ActiveActor] )
        EnergyMeterClass[ActiveActor]->CloseAllDIFiles( ActiveActor );
      Fyear = Value;
      DynaVars.intHour = 0;  /*Change year, start over*/
      DynaVars.T = 0.0;
      Update_dblHour();
      EnergyMeterClass[ActiveActor]->ResetAll( ActiveActor );  // force any previous year data to complete
    }


    void TSolutionObj::Set_Total_Time( const double Value )
    {
      Total_Time_Elapsed = Value;
    }



    void TSolutionObj::SaveVoltages()
    {
        TTextRec F;
        complex Volts;
        int i = 0, j = 0;
        String BusName;
        try
        {
            AssignFile(F, CircuitName_[ActiveActor] + "SavedVoltages.Txt");
            Rewrite(F);
            IOResultToException();
            /*# with ActiveCircuit[ActiveActor] do */
            {
                TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
                for (int stop = with0->NumBuses, i = 1; i <= stop; i++)
                {
                    BusName = with0->BusList.Get(i);
                    for (int stop = with0->Buses[i - 1]->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
                    {
                        Volts = NodeV[with0->Buses[i - 1]->GetRef(j)];
                        Write(F, BusName);
                        Write(F, ", ");
                        Write(F, with0->Buses[i - 1]->GetNum(j), 0);
                        WriteLn(F, Format(", %-.7g, %-.7g", cabs(Volts), cdang(Volts)));
                    }
                }
            }

            CloseFile(F);
            GlobalResult = CircuitName_[ActiveActor] + "SavedVoltages.Txt";
        }
        catch (std::exception& E)
        {
            DoSimpleMsg("Error opening Saved Voltages File: " + (std::string)E.what(), 488);
            return;
        }
        catch (...)
        {
            // to match with delphi
        }
    }


    /*  *************  MAIN SOLVER CALL  *************************/


    int TSolutionObj::SolveSystem( pNodeVarray V, int ActorID )
    {
      int result = 0;
      longInt RetCode = 0;
    //  iRes    : LongWord;
    //  dRes    : Double;

      String myMsg;

     /*Note: NodeV[0] = 0 + j0 always.  Therefore, pass the address of the element 1 of the array.
     */
      try
      {
        // new function to log KLUSolve.DLL function calls; same information as stepping through in Delphi debugger
        // SetLogFile ('KLU_Log.txt', 1);
        if ( ! ADiakoptics || ( ActorID == 1 ) )
          RetCode = SolveSparseSet( hY, &(V[1]), &(Currents[1]));  // Solve for present InjCurr
        else
          RetCode = SolveSparseSet( hY, &(V[LocalBusIdx[0]]), &(Currents[1]) );  // Solve for present InjCurr in Actor 1 context
    /**  Commented out because results are not logged currently -- but left in just in case
        // new information functions
        GetFlops(hY, @dRes);
        GetRGrowth(hY, @dRes);
        GetRCond(hY, @dRes);
        // GetCondEst (hY, @dRes); // this can be expensive
        GetSize(hY, @iRes);
        GetNNZ(hY, @iRes);
        GetSparseNNZ(hY, @iRes);
        GetSingularCol(hY, @iRes);
        **/
      }
      catch (std::exception & E)
      { //Raise
      {
              myMsg = "Error Solving System Y Matrix.  Sparse matrix solver reports numerical error: " + (std::string) E.what();
        DoThreadSafeMsg( myMsg, 0 );
        SolutionAbort = true;
      }
      }
      result = RetCode;
      return result;
    }


    void TSolutionObj::Update_dblHour( )
    {
      DynaVars.dblHour = DynaVars.intHour + double( DynaVars.T ) / 3600.0;
    }


    void TSolutionObj::UpdateLoopTime( )
    {

    // Update Loop time is called from end of time step cleanup
    // Timer is based on beginning of SolveSnap time
      QueryPerformanceCounter( &LoopEndtime );
#ifndef windows
      Step_Time_Elapsed = LoopEndtime - SolveStartTime;
#else
      Step_Time_Elapsed = ( double( ( LoopEndtime.QuadPart - SolveStartTime.QuadPart ) ) / CPU_Freq ) * 1000000;
#endif
    }


    void TSolutionObj::UpdateVBus( int ActorID )

    // Save present solution vector values to buses

    {
      int i = 0, j = 0;
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            /*# with Buses^[i] do */
            {
              auto& with1 = with0->Buses[i - 1];
              if (!( with1->VBus.empty() ) )
                for ( int stop = with1->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
                    (with1->VBus)[j - 1] = NodeV[with1->GetRef( j )];
            }
        }
      }
    }


    void TSolutionObj::RestoreNodeVfromVbus( )
    {
      int i = 0, j = 0;
      /*# with ActiveCircuit[ActiveActor] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActiveActor];
        for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
          /*# with Buses^[i] do */
          {
            auto& with1 = with0->Buses[i - 1];
            if (!( with1->VBus.empty() ) )
              for ( int stop = with1->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
                NodeV[with1->GetRef( j )] = (with1->VBus)[j - 1];
          }
      }
    }


    int TSolutionObj::SolveYDirect( int ActorID )

    /* Solves present Y matrix with no injection sources except voltage and current sources */
    {
      int result = 0;
      result = 0;
      if ( (!ADiakoptics) || (ActorID != 1) )
      {
        ZeroInjCurr( ActorID );   // Side Effect: Allocates InjCurr
        GetSourceInjCurrents( ActorID );
        if ( IsDynamicModel )
          GetPCInjCurr( ActorID );  // Need this in dynamics mode to pick up additional injections
        SolveSystem( &NodeV[0], ActorID); // Solve with Zero injection current
      }
      else
      {
        ADiak_PCInj = false;
        Solve_Diakoptics( );             // A-Diakoptics
      }
      return result;
    }


    void TSolutionObj::Get_Yiibus( )
    {
      unsigned int nNZ = 0;
      longInt result = 0;
      unsigned int i = 0, Row = 0, Col = 0;
      double re = 0.0, im = 0.0;
      std::vector <unsigned int> ColIdx;
      std::vector <unsigned int> RowIdx;
      std::vector <complex> cVals;
      int sf = 0;
      if ( ! hY )
      {
        DoSimpleMsg( "Y Matrix not Built.", 222 );
        return;
      }
      // print lower triangle of G and B using new functions
      // this compresses the entries if necessary - no extra work if already solved
      result = FactorSparseMatrix( hY );
      result = GetNNZ( hY, &nNZ );
      //GetSize (temphY, @nBus); // we should already know this
      try
      {
        ColIdx.resize(nNZ);
        RowIdx.resize(nNZ);
        cVals.resize(nNZ);
        nNZ_yii = nNZ;//how many lines of Y sparse
        if ( !pColIdx_Yii.empty() )
          pColIdx_Yii.clear();
        pColIdx_Yii.resize( nNZ_yii + 1 );
        if ( !pRowIdx_Yii.empty() )
          pRowIdx_Yii.clear();
        pRowIdx_Yii.resize( nNZ_yii + 1 );
        if ( !pcVals_Yii.empty() )
          pcVals_Yii.clear();
        pcVals_Yii.resize( nNZ_yii + 1 );
        sf = GetTripletMatrix( hY, nNZ, &(ColIdx[0]), &(RowIdx[0]), &(cVals[0]));
        // shows how to easily traverse the triplet format
        for ( int stop = nNZ - 1, i = 0; i <= stop; i++)
        {
          Col = ColIdx[i] + 1;
          Row = RowIdx[i] + 1;
          if ( Row == Col )
          { //diagnal
            re = cVals[i].re;
            im = cVals[i].im;
            NodeYii[Row] = cmplx( re, im );
          }
          // pColIdx_Yii,  pRowIdx_Yii,  pcVals_Yii
          pColIdx_Yii[i + 1] = Col;           // begin from 1
          pRowIdx_Yii[i + 1] = Row;
          pcVals_Yii[i + 1] = cVals[i];
        }
        NodeYiiEmpty = false;
        cVals.clear();
        ColIdx.clear();
        RowIdx.clear();
      }
      //__finally
      //{
      catch(...)
      {
          //To match with delphi (try)
      }
    }

    //----------------------------------------------------------------------------------------------------

    double TSolutionObj::get_FFrequency()
    {
        return FFrequency;
    }

    //----------------------------------------------------------------------------------------------------

    int TSolutionObj::get_Fyear()
    {
        return Fyear;
    }

    //----------------------------------------------------------------------------------------------------

    complex TSolutionObj::Get_Yij( int node_ref_i, int node_ref_j ) // get Gij + j Bij

    {
      complex result;
      int i = 0
           //nNZ_yii   :LongWord;       //how many lines in Yii
           //pColIdx_Yii, pRowIdx_Yii   :pLongIntArray;//array of LongWord;  //cols and rows
           //pcVals_Yii                 :pComplexArray;   //vals of yii
      , Col = 0, Row = 0;
      for ( int stop = nNZ_yii, i = 1; i <= stop; i++)
      {
        Col = (pColIdx_Yii)[i];
        Row = (pRowIdx_Yii)[i];
        if ( ( Row == node_ref_i ) && ( Col == node_ref_j ) )
          result = (pcVals_Yii)[i];
      }
      return result;
    }

    /********************************************************************************
    *             Used to create the OpenDSS Solver thread                         *
    ********************************************************************************
    */

    int TSolver::TMessageQueue::recv()
    {
        // wait_for should check every 100ms in case we miss the notify:
#if __cplusplus >= 201402L
        using namespace std::chrono_literals;
        const auto wait_timeout = 100ms; // C++14 permits a shorter syntax.
#else
        // C++11 is needed by a customer for older RHEL 7.x systems
        const auto wait_timeout = std::chrono::milliseconds(100);
#endif

        std::unique_lock<std::mutex> mlock(m_mutex);
        while(m_queue.empty())
        {
            // wait_for will unlock m_mutex until m_convar is notified or
            // wait_timeout expires.
            m_condVar.wait_for(mlock, wait_timeout);
        }

        int msg = m_queue.front();
        m_queue.pop();
        return msg; // m_mutex is unlocked here when mlock goes out of scope.
    }

    void TSolver::TMessageQueue::send(int msg)
    {
        std::unique_lock<std::mutex> mlock(m_mutex);
        m_queue.push(msg);
        m_condVar.notify_one();
    }

    TSolver::TSolver( int local_CPU, int ID )
     : ActorID(ID),
       MsgType(-1),
       AD_Init(false),
       ActorActive(true),
       Processing(false),
       ActorThread(&TSolver::Execute, this) // initialize this last; it starts the execution thread
    {
    }

    /*---------------------------------------------------------
    |               Send a message to the actor              |
    ----------------------------------------------------------*/


    void TSolver::Send_Message( int Msg )
    {
      MyMessages.send(Msg); // Notifies that there is a new simulation job
    }

    /*---------------------------------------------------------
    |      Checks if the actor has power injection Obj       |
    ----------------------------------------------------------*/


    bool TSolver::HasInjObj( )
    {
      bool result = false;
      int ListSize = 0, jj = 0;
      TVsourceObj* VSourceObj;
      TIsourceObj* ISourceObj;
      if ( ActorID == 2 )
        result = true;
      else
      {
        result = false;
        /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
        {
          TDSSCircuit* with0 = ActiveCircuit[ActorID];
          TSolutionObj* with1 = ActiveCircuit[ActorID]->Solution;
          {
          // Starts looking for VSource
            VSourceObj = (TVsourceObj*) VSourceClass[ActorID]->ElementList.Get_First();
            while ( VSourceObj != NULL )
            {
              if ( VSourceObj->Get_Enabled() )
              {
                result = true;
                break;
              }
              VSourceObj = (TVsourceObj *) VSourceClass[ActorID]->ElementList.Get_Next();
            }
            if ( ! result )
            {
            // Goes for ISources
              ISourceObj = (TIsourceObj*) IsourceClass[ActorID]->ElementList.Get_First();
              while ( ISourceObj != NULL )
              {
                if ( ISourceObj->Get_Enabled() )
                {
                  result = true;
                  break;
                }
                ISourceObj = (TIsourceObj*) IsourceClass[ActorID]->ElementList.Get_Next();
              }
            }
          }
        }
      }
      return result;
    }

    /*---------------------------------------------------------
    |    locates the local buses into actor 1's bus array    |
    ----------------------------------------------------------*/


    void TSolver::IndexBuses( )
    {
      bool Found = false;
      int k = 0, i = 0, j = 0;
      String myBUS;
      std::vector <string> LclBus;
      std::vector <string> SrcBus;
      String Ids [ 2/*# range 0..1*/ ];
      Ids[0] = "1_";
      Ids[1] = "2_";
      // Get_First(), get the list of buses in actor 1
      SrcBus.resize( 1 );
      if ( ActiveCircuit[1] != NULL )
      {
        /*# with ActiveCircuit[1] do */
        {
          TDSSCircuit* with0 = ActiveCircuit[1];
          {
            for ( int stop = with0->NumNodes, i = 1; i <= stop; i++)
            {
              /*# with MapNodeToBus^[i] do */
              {
                auto& with1 = with0->MapNodeToBus[i - 1];
                SrcBus[SrcBus.size() - 1] = ToUpperCaseStr(with0->BusList.Get(with1.BusRef)) + Format(".%-d", with1.NodeNum );
              }
              SrcBus.resize( SrcBus.size() + 1 );
            }
          }
        }
      }
      // rebuilds the Y matrix to relocate the local buses
      BuildYMatrix( WHOLEMATRIX, true, ActorID );   // Side Effect: Allocates V
      // Then, get the list of buses in my actor
      LclBus.resize( 1 );
      if ( ActiveCircuit[ActorID] != NULL )
      {
        /*# with ActiveCircuit[ActorID] do */
        {
          TDSSCircuit* with2 = ActiveCircuit[ActorID];
          {
            for ( int stop = with2->NumNodes, i = 1; i <= stop; i++)
            {
              /*# with MapNodeToBus^[i] do */
              {
                auto& with3 = with2->MapNodeToBus[i - 1];
                LclBus[LclBus.size() - 1] = ToUpperCaseStr(with2->BusList.Get(with3.BusRef)) + Format(".%-d", with3.NodeNum );
              }
              LclBus.resize( LclBus.size() + 1 );
            }
          }
        }
      }
      // Initializes the bus index vector
      /*# with ActiveCircuit[ActorID].Solution do */
      {
        auto& with4 = ActiveCircuit[ActorID]->Solution;

        with4->LocalBusIdx.resize( LclBus.size() - 1 );
        for ( int stop = (with4->LocalBusIdx.size() - 1 ), i = 0; i <= stop; i++)
        {
          for ( int stop = ( SrcBus.size() - 1 ), j = 0; j <= stop; j++)
            if ( LclBus[i] == SrcBus[j] )
              break;
          with4->LocalBusIdx[i] = j + 1;
        }

          // Initializes the list for easy accessing the ADiakoptics Isources
    //      if AD_IBus    = nil then AD_IBus    := TList<integer>.Create  ELSE AD_IBus.Clear;
    //      if AD_ISrcIdx = nil then AD_ISrcIdx := TList<integer>.Create  ELSE AD_ISrcIdx.Clear;
          // Locates the ISource used for the local ADiakoptics algorithm
        for ( int stop = ( ActiveCircuit[1]->Contours.NZero() - 1 ), j = 0; j <= stop; j++)
        {
          myBUS = SrcBus[ActiveCircuit[1]->Contours.CData[j].Row];
            // checks if the bus in in this circuit
          Found = false;
          for ( int stop = ( LclBus.size() - 1 ), k = 0; k <= stop; k++)
          {
            if ( LclBus[k] == myBUS )
            {
              Found = true;
              break;
            }
          }
          if ( Found )     // If found, add it to the indexed list
          {
//            with4->AD_IBus.Add( k + 1 );
//            with4->AD_ISrcIdx.Add( ActiveCircuit[1]->Contours.CData[j].Row );
          }
        }
      }
    }

    /*---------------------------------------------------------
    |                 Sets the local busy flag               |
    ----------------------------------------------------------*/


    void TSolver::Set_Processing( bool Nval )
    {
      Processing = Nval;
    }

    /*---------------------------------------------------------
    |                 Gets the local busy flag               |
    ----------------------------------------------------------*/


    bool TSolver::Get_Processing( )
    {
      bool result = false;
      result = Processing;
      return result;
    }

    /*---------------------------------------------------------
    |          Returns the CPU assigned to the actor         |
    ----------------------------------------------------------*/


    int TSolver::Get_CPU( )
    {
      int result = 0;
      result = ActorCPU[ActorID];
      return result;
    }

    /*---------------------------------------------------------
    |             Sets the CPU assigned to the actor         |
    ----------------------------------------------------------*/


    void TSolver::Set_CPU( int CPU )
    {
      ActorCPU[ActorID] = CPU;
    } 

    /*---------------------------------------------------------
    |             Zeroes the local voltage array             |
    ----------------------------------------------------------*/


    void TSolver::ZeroLocalV( )
    {
      int i = 0;
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          for ( int stop = with0->NumNodes, i = 1; i <= stop; i++)
            ActiveCircuit[ActorID]->Solution->NodeV[i] = CZero;
        }
      }
    }

    /*---------------------------------------------------------
    |     Uploads the local voltage array in the masters     |
    |     using the index map obtained in previous steps     |
    ----------------------------------------------------------*/


    void TSolutionObj::UploadV2Master( int ActorID )
    {
      int Idx = 0, i = 0;
    //  if ActorID = 2 then
    //  Begin
      /*# with ActiveCircuit[ActorID] do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        {
          for ( int stop = with0->NumNodes, i = 1; i <= stop; i++)
          {
            Idx = LocalBusIdx[i - 1];
            ActiveCircuit[1]->Solution->NodeV[Idx] = ActiveCircuit[ActorID]->Solution->NodeV[i];
          }
        }
      }

    //  End;
    }


    /*---------------------------------------------------------
    |   Retunrs the voltage at the node given at NodeIdx in  |
    |   context of actor 1  (A-Diakoptics)                   |
    ----------------------------------------------------------*/


    complex TSolutionObj::VoltInActor1( int NodeIdx )
    {
      complex result;
      if ( NodeIdx != 0 )
        NodeIdx = NodeIdx + ( LocalBusIdx[0] - 1 );
      // In the context of actor 1
      result = ActiveCircuit[1]->Solution->NodeV[NodeIdx];
      return result;
    }

    /*---------------------------------------------------------
    |   Updates the local ISources using the data obtained   |
    |   for Ic in actor 1                                    |
    ----------------------------------------------------------*/


    void TSolutionObj::UpdateISrc( int ActorID )
    {
      int LclIdx = 0, Idx = 0, i = 0;
      bool Found = false;
      complex myCmplx;
      /*
      # with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        TSolutionObj* with1 = ActiveCircuit[ActorID]->Solution;
        {
          for ( int stop = ( with1->AD_IBus.Count - 1 ), i = 0; i <= stop; i++)
          {
            for ( int stop = ( ActiveCircuit[1].Ic.NZero - 1 ), Idx = 0; Idx <= stop; Idx++)
            {
              if ( ActiveCircuit[1].Ic.CData[Idx].Row == AD_ISrcIdx.Items[i] )
              {
                Found = true;
                break;
              }
            }
            if ( Found )
            {
            // Adds the present current with the adjustment
              myCmplx = cmulreal( ActiveCircuit[1]->Ic.CData[Idx].Value, ( - 1.0 ) );
              Currents[AD_IBus.Items[i]] = cadd( myCmplx, Currents[AD_IBus.Items[i]] );
            }  // Otherwise is just another ISource in the zone
          }
        }
      }
      */
    }

    /********************************************************************************
    *             executes the selected solution algorithm                         *
    ********************************************************************************
    */


    void TSolver::Execute( )
    {
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          ActorActive = true;   // for now, sequential
          while ( ActorActive )
          {
            {
              MsgType = MyMessages.recv();
              Processing = true;
              switch ( MsgType )
              {
                case             // Evaluates the incomming message
                SIMULATE:               // Simulates the active ciruit on this actor
                  try
                  {
                      {                   // Checks if this is the coordinator actor in A-Diakoptics mode
                      // Normal solution routine
                        switch ( with1->DynaVars.SolutionMode )
                        {
                          case SNAPSHOT:
                            with1->SolveSnap( ActorID );
                          break;
                          case YEARLYMODE:
                            SolveYearly( ActorID );
                          break;
                          case DAILYMODE:
                            SolveDaily( ActorID );
                          break;
                          case DUTYCYCLE:
                            SolveDuty( ActorID );
                          break;
                          case DYNAMICMODE:
                            SolveDynamic( ActorID );
                          break;
                          case MONTECARLO1:
                            SolveMonte1( ActorID );
                          break;
                          case MONTECARLO2:
                            SolveMonte2( ActorID );
                          break;
                          case MONTECARLO3:
                            SolveMonte3( ActorID );
                          break;
                          case PEAKDAY:
                            SolvePeakDay( ActorID );
                          break;
                          case LOADDURATION1:
                            SolveLD1( ActorID );
                          break;
                          case LOADDURATION2:
                            SolveLD2( ActorID );
                          break;
                          case DIRECT:
                            with1->SolveDirect( ActorID );
                          break;
                          case MONTEFAULT:
                            SolveMonteFault( ActorID );
                          break;  // Monte Carlo Fault Cases

                          case FAULTSTUDY:
                            SolveFaultStudy( ActorID );
                          break;
                          case AUTOADDFLAG:
                            ActiveCircuit[ActorID]->AutoAddObj.Solve( ActorID );
                          break;
                          case HARMONICMODE:
                            SolveHarmonic( ActorID );
                          break;
                          case GENERALTIME:
                            SolveGeneralTime( ActorID );
                          break;
                          case HARMONICMODET:
                            SolveHarmonicT( ActorID );
                          break;  //Declares the Hsequential-time harmonics
                          case EMPMODE:
                              SolveEMP(ActorID);
                              break;
                          case EMPDAILYMODE:
                              SolveEMPDaily(ActorID);
                              break;
                        default:
                          DoThreadSafeMsg( "Unknown solution mode.", 481 );
                        }
                        QueryPerformanceCounter( &with1->Gendtime);
#ifndef windows
                        with1->Total_Solve_Time_Elapsed = with1->Gendtime - with1->GStartTime;
#else
                        with1->Total_Solve_Time_Elapsed = ( double( ( with1->Gendtime.QuadPart - with1->GStartTime.QuadPart ) ) / CPU_Freq ) * 1000000;
#endif
                        with1->Total_Time_Elapsed = with1->Total_Time_Elapsed + with1->Total_Solve_Time_Elapsed;
                        Processing = false;
                        FMessage = "1";
                      }
                      
                      if (!NoFormsAllowed)
                      {
                          CoutLn("Actor " + to_string(ActorID) + ": Job done");
                          cout << ">>";
                      }
                  }
                  catch( std::exception & E )
                  {
                      {
                        FMessage = "1";
                        ActorStatus[ActorID] = 1;                  // Global to indicate that the actor is ready
                        SolutionAbort = true;
                        if ( ! Parallel_enabled )
                          DoThreadSafeMsg( "Error Encountered in Solve: " + (std::string) E.what(), 482 );
                      }
                  }
                break;
                case INIT_ADIAKOPTICS:
                {
                  if ( ActorID > 2 )
                    Start_Diakoptics();               // Initializes the actor for Diakoptics (if needed)
                  IndexBuses();
                }
                break;
                case SOLVE_AD1:
                  with1->SolveAD( ActorID, true );
                break;               // Solves the model if the actor has PIE

                case SOLVE_AD2:
                    with1->SolveAD( ActorID, false );
                break;              // Complements the solution

                case ZEROIVECTOR:
                    with1->ZeroInjCurr( ActorID );
                break;
                case GETCURRINJ:
                    with1->GetSourceInjCurrents( ActorID );
                break;
                case CALC_INJ_CURR:
                    with1->GetPCInjCurr( ActorID );
                break;
                case DO_CTRL_ACTIONS:
                {
                    with1->ControlActionsDone = false;
                    with1->Sample_DoControlActions( ActorID );
                    MyMessages.send( CHECK_FAULT );
                    MyMessages.send( CHECKYBUS );
                }
                break;
                case CHECK_FAULT:
                    with1->Check_Fault_Status( ActorID );
                break;
                case CHECKYBUS:
                {
                  if (with1->SystemYChanged )
                  {
                    BuildYMatrix( WHOLEMATRIX, false, ActorID );  // Does not realloc V, I
                  }
                    /*by Dahei*/
                  if (with1->NodeYiiEmpty )
                      with1->Get_Yiibus();
                }
                break;
                case GETCTRLMODE:
                {
                    // Brings the control mode from actor 1
                    with1->ControlMode = ActiveCircuit[1]->Solution->ControlMode;
                    with1->DefaultControlMode = with1->ControlMode;
                    with1->MaxControlIterations = ActiveCircuit[1]->Solution->MaxControlIterations;
                }
                break;
                case EXIT_ACTOR:                // Terminates the thread
                {
                  ActorActive = false;
                  Doterminate();
                  
                  if (!NoFormsAllowed)
                  {
                      CoutLn("Actor " + to_string(ActorID) + " terminated");
                      cout << ">>";
                  }
                }
                break;
              default:                       // I don't know what the message is
                DoThreadSafeMsg( "Unknown message. ", 7010 );
              }
            }
            ActorStatus[ActorID] = 1;                  // Global to indicate that the actor is ready
          }
        }
      }
    }

    void TSolver::CallCallBack( )
    {
//      if (( FInfoProc != NULL ) )
//        FInfoProc( FMessage );
    }

    // Initializes the variables of the A-Diakoptics worker



    void TSolver::Start_Diakoptics( )
    {
      size_t jj = 0;
      TVsourceObj* VSourceObj;
      String BusName;
      DynStringArray myPDEList;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        TSolutionObj* with1 = ActiveCircuit[ActorID]->Solution;
        {
        // Select the main voltage source
          VSourceObj = (TVsourceObj*) VSourceClass[ActorID]->ElementList.Get_First();
        // Gets the name of the branch directly connected to the feeder head to remove it
        // (applies to all actors but actor 2 - first chunk of the system)
          BusName = VSourceObj->GetBus( 1 );
          jj = BusName.find( '.' );   // removes the dot
          if ( jj != String::npos )
            BusName = BusName.substr( 0, jj );
          SetActiveBus( BusName );                            // Activates the Bus
          myPDEList = with0->getPDEatBus( with0->BusList.Get( with0->ActiveBusIndex + 1 ) );
        // Disables the link branch
          DSSExecutive[ActorID]->Set_Command( myPDEList[0] + ".enabled=False");
        // Now disables all the VSources added artificially
          while ( VSourceObj != NULL )
          {
            BusName = ToLowerCaseStr( VSourceObj->get_Name() );
            if ( BusName == "source" )
              VSourceObj->Set_Enabled(false);                   // Disables the artificial VSource phase 1
            else
              if ( BusName == "vph_2" )
                VSourceObj->Set_Enabled(false);                 // Disables the artificial VSource phase 2
              else
                if ( BusName == "vph_3" )
                  VSourceObj->Set_Enabled(false);              // Disables the artificial VSource phase 3
            VSourceObj = (TVsourceObj*) VSourceClass[ActorID]->ElementList.Get_Next();
          }
        }
      }
    }


    void TSolver::Notify_Main( )
    {
      complex CNum;
      int i = 0, j = 0, Idx = 0;
      // Will do something
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        TDSSCircuit* with0 = ActiveCircuit[ActorID];
        TSolutionObj* with1 = ActiveCircuit[ActorID]->Solution;
        {
          i = with0->NumNodes;
          for ( int stop = i, Idx = 1; Idx <= stop; Idx++)
          {
          // if it doesn't includes any power injection element (Isource, VSource)
          // returns dV to correct the interconnection equation
            if ( ActorID > 2 )
              CNum = csub( with1->NodeV[Idx], with1->Node_dV[Idx] );
            else
              CNum = with1->NodeV[Idx];
            ActiveCircuit[1]->V_0.Insert( ( Idx + with0->VIndex - 1 ), 0, CNum );
          }
        }
      }
    }


    void TSolver::Doterminate( )        // Is the end of the thread
    {
      TObject* ex = NULL;
      ActorActive = false;
      Processing = false;
      ActorStatus[ActorID] = 1;      // Global to indicate that the actor is ready
    }


    TSolver::~TSolver( )
    {
      // todo check:  inherited::Destroy;
      Send_Message( EXIT_ACTOR );
      ActorThread.join();
    }

    void Solution_initialization()
    {
//      IsMultiThread = true;
    }

    class Solution_unit
    {
        public:
        Solution_unit()
        {
        Solution_initialization();
        }
    };

    Solution_unit _Solution_unit;


}// namespace Solution






