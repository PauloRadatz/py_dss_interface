
#pragma hdrstop

#include "SolutionAlgs.h"



#include "Arraydef.h"
#include "DSSGlobals.h"
#include "CmdForms.h"
#include "Utilities.h"
#include "Sysutils.h"
#include "System.h"
#include "mathutil.h"
#include <math.h>
#include "Fault.h"
#include "Ucomplex.h"
#include "YMatrix.h"
#include "PCElement.h"
#include "Spectrum.h"
#include "VSource.h"
#include "Isource.h"
#include "klusolve.h"



namespace SolutionAlgs
{

    int ProgressCount = 0;


    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void FinishTimeStep( int ActorID )
    /*
       Cample Cleanup and increment time

       For custom solutions.

    */
    {
      MonitorClass[ActorID]->SampleAll( ActorID );
      /*# with ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID]->Solution;
        if ( with0->SampleTheMeters )
          EnergyMeterClass[ActorID]->SampleAll( ActorID );   // Save Demand interval Files
        EndOfTimeStepCleanup( ActorID );
        with0->Increment_time();
      }
    }


    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void EndOfTimeStepCleanup( int ActorID )
    /*
       Put stuff in this procedure that needs to happen at the end of the time step
       in main solution loops (see below)
    */
    {
      StorageClass[ActorID]->UpdateAll( ActorID );
    //    Storage2Class[ActorID]->UpdateAll(ActorID);
      InvControlClass[ActorID]->UpdateAll( ActorID );
    //    InvControl2Class[ActorID]->UpdateAll(ActorID);
      ExpControlClass[ActorID]->UpdateAll( ActorID );

        // End of Time Step Timer
      ActiveCircuit[ActorID]->Solution->UpdateLoopTime();
      MonitorClass[ActorID]->SampleAllMode5( ActorID );  // sample all mode 5 monitors to get timings
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void Show10PctProgress( int i, int n, int ActorID )
    {
      if ( NoFormsAllowed )
        return;
      if ( ( ( i * 10 ) / n ) > ProgressCount )
      {
        ProgressCount++;
    //        ShowPctProgress( ProgressCount * 10, ActorID);
      }
    }


    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveYearly( int ActorID )
    {
      int result = 0;
      int n = 0, Twopct = 0, i = 0;
      result = 0;

    /* ProgressCaption( 'Solving Year '+ IntToStr(ActiveCircuit[ActorID].Solution.Year) + ' Actor: ' + IntToStr(ActorID) + ' CPU: ' + IntToStr(ActorCPU[ActorID]),ActorID);
     ActorProgressCount[ActorID] := 0;
     ShowPctProgress(ActorProgressCount[ActorID],ActorID);
    */
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          try
          {
            with1->IntervalHrs = double(with1->DynaVars.h ) / 3600.0;  // needed for energy meters and storage elements
            if ( ! DIFilesAreOpen[ActorID] )
              EnergyMeterClass[ActorID]->OpenAllDIFiles( ActorID );   // Open Demand Interval Files, if desired   Creates DI_Totals
            Twopct = max(with1->NumberOfTimes / 50, 1 );
            for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
              if ( ! SolutionAbort )
                /*# with DynaVars do */
                {
                  auto& with2 = with1->DynaVars;
                  with1->Increment_time();

                  if ( ADiakoptics && ( ActorID == 1 ) )
                    for ( int stop = NumOfActors, i = 2; i <= stop; i++)
                      ActiveCircuit[i]->Solution->Increment_time();
                  with0->DefaultHourMult = with0->DefaultYearlyShapeObj->GetMult( with2.dblHour );
                  if ( with0->PriceCurveObj != NULL )
                    with0->PriceSignal = with0->PriceCurveObj->GetPrice( with2.dblHour );
                  with1->SolveSnap( ActorID );
                  MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
                  if ( with1->SampleTheMeters )
                    EnergyMeterClass[ActorID]->SampleAll( ActorID ); // Make all Energy Meters take a sample
                  EndOfTimeStepCleanup( ActorID );
                  ActorPctProgress[ActorID] = ( n * 100 ) / with1->NumberOfTimes;
    //          If (N mod Twopct)=0 Then ShowPctProgress((N*100) div NumberofTimes,ActorID);
                }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
    //    EnergyMeterClass[ActorID]->CloseAllDIFiles(ActorID);   // Save Demand interval Files    See DIFilesAreOpen Logic
          }
          catch (...)
          {
              // To match delphi
          }
        }
      }
      return result;
    }


    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveDaily( int ActorID )

    /*
      Solves following the daily load curve.
      Stepsize defaults to 1 hr and number of times = 24.
      Load is modified by yearly growth, time of day, and global load multiplier.
    */
    {
      int result = 0;
      int n = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          // t:=0.0;
          // MonitorClass.ResetAll;
          // EnergyMeterClass.ResetAll;
          try
          {
            with1->IntervalHrs = double( with1->DynaVars.h ) / 3600.0;  // needed for energy meters
            if ( ! DIFilesAreOpen[ActorID] )
              EnergyMeterClass[ActorID]->OpenAllDIFiles( ActorID );   // Append Demand Interval Files, if desired
            for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
              if ( ! SolutionAbort )
                /*# with DynaVars do */
                {
                  auto& with2 = with1->DynaVars;
                  with1->Increment_time();
                  with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult( with2.dblHour );
                  if ( with0->PriceCurveObj != NULL )
                    with0->PriceSignal = with0->PriceCurveObj->GetPrice( with2.dblHour );
                  with1->SolveSnap( ActorID );
                  MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
                  if ( with1->SampleTheMeters )
                    EnergyMeterClass[ActorID]->SampleAll( ActorID ); // Make all Energy Meters take a sample
                  EndOfTimeStepCleanup( ActorID );
                  ActorPctProgress[ActorID] = ( n * 100 ) / with1->NumberOfTimes;
                }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
            if ( with1->SampleTheMeters )
              EnergyMeterClass[ActorID]->CloseAllDIFiles( ActorID );   // Save Demand interval Files
          } /*Try*/
          catch (...)
          {
              //matching with delphi
          }
        }
      }  /*WITH*/
      return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveEMPDaily(int ActorID)

        /*
          This is an identical routine to the the SolveDaily routine with a few differences. 
          The main difference is that the initial power output for all generating units is carried over from 
          the dynamic simulation perfromed in the EMP mode.  

          All other functionality, such as loadshape and generation shapes should work the same way.

          Solves following the daily load curve.
          Stepsize defaults to 1 hr and number of times = 24.
          Load is modified by yearly grBowth, time of day, and global load multiplier.
        */
    {
        int result = 0;
        int n = 0;
        result = 0;
        
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
            try
            {
                with1->IntervalHrs = double(with1->DynaVars.h) / 3600.0;  // needed for energy meters
                if (!DIFilesAreOpen[ActorID])
                    EnergyMeterClass[ActorID]->OpenAllDIFiles(ActorID);   // Append Demand Interval Files, if desired
                for (int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
                    if (!SolutionAbort)
                        /*# with DynaVars do */
                    {
                        auto& with2 = with1->DynaVars;
                        with1->Increment_time();
                        with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult(with2.dblHour);
                        if (with0->PriceCurveObj != NULL)
                            with0->PriceSignal = with0->PriceCurveObj->GetPrice(with2.dblHour);
                        
                        with1->SolveSnap(ActorID);
                        
                        MonitorClass[ActorID]->SampleAll(ActorID);  // Make all monitors take a sample
                        if (with1->SampleTheMeters)
                            EnergyMeterClass[ActorID]->SampleAll(ActorID); // Make all Energy Meters take a sample
                        EndOfTimeStepCleanup(ActorID);
                        ActorPctProgress[ActorID] = (n * 100) / with1->NumberOfTimes;
                    }
                MonitorClass[ActorID]->SaveAll(ActorID);
                if (with1->SampleTheMeters)
                    EnergyMeterClass[ActorID]->CloseAllDIFiles(ActorID);   // Save Demand interval Files
            } /*Try*/
            catch (...)
            {
                //matching with delphi
            }
        }
        return result;
    }

    //= = = =


    int SolvePeakDay( int ActorID )

    /*
     Solves peak day

        Takes the given load kW and assumes it represents the peak value.
        Load is modified by daily load curve and growth factor for the year.
        'h' defaults to 3600 (1 hr) but can be reset to anything.
        Differs from Daily mode in that the global load multiplier is ignored.
    */
    {
      int result = 0;
      int n = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          with1->DynaVars.T = 0.0;

            // MonitorClass.ResetAll;
            // EnergyMeterClass.ResetAll;
          try
          {
            with1->DynaVars.intHour = 0;
            with1->DynaVars.dblHour = 0.0;
            with1->IntervalHrs = double( with1->DynaVars.h ) / 3600.0;  // needed for energy meters and storage devices
            if ( ! DIFilesAreOpen[ActorID] )
              EnergyMeterClass[ActorID]->OpenAllDIFiles( ActorID );   // Open Demand Interval Files, if desired
            for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
              if ( ! SolutionAbort )
                /*# with DynaVars do */
                {
                  auto& with2 = with1->DynaVars;
                  with1->Increment_time();
                  with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult( with2.dblHour );
                  if ( with0->PriceCurveObj != NULL )
                    with0->PriceSignal = with0->PriceCurveObj->GetPrice( with2.dblHour );
                  with1->SolveSnap( ActorID );
                  MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
                  if ( with1->SampleTheMeters )
                    EnergyMeterClass[ActorID]->SampleAll( ActorID ); // Make all Energy Meters take a sample
                  EndOfTimeStepCleanup( ActorID );
                }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
            if ( with1->SampleTheMeters )
              EnergyMeterClass[ActorID]->CloseAllDIFiles( ActorID );   // Save Demand interval Files
          }
          catch (...)
          {
                // to match with the try requirements on C
          }
        }
      }  /*WITH*/
      return result;
    }


    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveDuty( int ActorID )
    {
      int result = 0;
      int n = 0, Twopct = 0, Temp0 = 0;
      bool Temp1 = false;
      result = 0;

    /*   ProgressCaption( 'Duty Cycle Solution', ActorID);
       ProgressCount := 0;
       ShowPctProgress(0, ActorID);       */
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
         //   t:=0.0;
            // MonitorClass.ResetAll;
          Twopct = max( 1, with1->NumberOfTimes / 50 );
          try
          {
            with1->IntervalHrs = double( with1->DynaVars.h ) / 3600.0;  // needed for energy meters and storage devices
            for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
              if ( ! SolutionAbort )
                /*# with DynaVars do */
                {
                  auto& with2 = with1->DynaVars;
                  with1->Increment_time();
                  with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult( with2.dblHour );
                // Assume pricesignal stays constant for dutycycle calcs
                  with1->SolveSnap( ActorID );
                  MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
                  if ( with1->SampleTheMeters )
                    EnergyMeterClass[ActorID]->SampleAll( ActorID ); // Make all Energy Meters take a sample
                  EndOfTimeStepCleanup( ActorID );
                  ActorPctProgress[ActorID] = ( n * 100 ) / with1->NumberOfTimes;
                  if ( with1->SampleTheMeters )
                    EnergyMeterClass[ActorID]->CloseAllDIFiles( ActorID );   // Save Demand interval Files
    //            If (N mod Twopct)=0 Then ShowPctProgress((N*100) div NumberofTimes, ActorID);
                }
//          }
//          __finally
 //         {
            MonitorClass[ActorID]->SaveAll( ActorID );
    //        ProgressHide(ActorID);
          }
          catch (...)
          {
              //to match delphi
          }
        }
      }
      return result;
    }

    int SolveGeneralTime( int ActorID )

    /*
       For Rolling your own solution modes
    */
    {
      int result = 0;
      int n = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          with1->IntervalHrs = double( with1->DynaVars.h ) / 3600.0;  // needed for energy meters and storage devices
          for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
            if ( ! SolutionAbort )
              /*# with DynaVars do */
              {
                auto& with2 = with1->DynaVars;
                  /*Compute basic multiplier from Default loadshape to use in generator dispatch, if any*/
                with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult( with2.dblHour );
                with1->SolveSnap( ActorID );
                FinishTimeStep( ActorID );
                ActorPctProgress[ActorID] = ( n * 100 ) / with1->NumberOfTimes;
              }
        }
      }
      return result;
    }




    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void IntegratePCStates( int ActorID )
     /*Integrate states in all PC Elements.  At present, only PC Elements
      can have dynamic states*/
    {
      TPCElement* PCelem;
      /*# with ActiveCircuit[ActorID] do */
      {
        auto with0 = ActiveCircuit[ActorID];
        {
          PCelem = (TPCElement*) with0->PCElements.Get_First();
          while ( PCelem != NULL )
          {
            PCelem->IntegrateStates( ActorID );
            PCelem = (TPCElement*) with0->PCElements.Get_Next();
          }
        }
      }
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

    void InitializePCStates(int ActorID)
        /* 
        This functino will initialize all states associated with dynamic models in each PC Element.  
        At present, only PC Elements can have dynamic states
        */
    {
        TPCElement* PCelem;
        {
            auto with0 = ActiveCircuit[ActorID];                            //  Get active circuit
            {
                PCelem = (TPCElement*)with0->PCElements.Get_First();              //  Get first PC element
                while (PCelem != NULL)
                {
                    PCelem->InitializeStates(ActorID);                       //  Call PC element InitializeState function 
                    PCelem = (TPCElement*)with0->PCElements.Get_Next();
                }
            }
        }
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    void CalculateStateDerivatives(int ActorID)
        /*
        This functino will calculate all state derivatives associated with dynamic models in each PC Element.
        At present, only PC Elements can have dynamic states
        */
    {
        TPCElement* PCelem;
        {
            auto with0 = ActiveCircuit[ActorID];                            //  Get active circuit
            {
                PCelem = (TPCElement*)with0->PCElements.Get_First();              //  Get first PC element
                while (PCelem != NULL)
                {
                    PCelem->CalculateRate(ActorID);                       //  Call PC element InitializeState function 
                    PCelem = (TPCElement*)with0->PCElements.Get_Next();
                }
            }
        }
    }


    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

    void StateIntegration(int ActorID)
        /*Integrate states in all PC Elements.  At present, only PC Elements
         can have dynamic states*/
    {
        TPCElement* PCelem;
        /*# with ActiveCircuit[ActorID] do */
        {
            auto with0 = ActiveCircuit[ActorID];
            {
                PCelem = (TPCElement*)with0->PCElements.Get_First();
                while (PCelem != NULL)
                {
                    PCelem->StateIntegration(ActorID);
                    PCelem = (TPCElement*)with0->PCElements.Get_Next();
                }
            }
        }
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

    void StateIntegration_correction(int ActorID)
        /*Integrate states in all PC Elements.  At present, only PC Elements
         can have dynamic states*/
    {
        TPCElement* PCelem;
        /*# with ActiveCircuit[ActorID] do */
        {
            auto with0 = ActiveCircuit[ActorID];
            {
                PCelem = (TPCElement*)with0->PCElements.Get_First();
                while (PCelem != NULL)
                {
                    PCelem->StateIntegration_correction(ActorID);
                    PCelem = (TPCElement*)with0->PCElements.Get_Next();
                }
            }
        }
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 


    int SolveDynamic( int ActorID )
    {
      int result = 0;
      int n = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = with0->Solution;
        {
          try
          {
            with1->SolutionInitialized = true; // If we're in dynamics mode, no need to re-initialize.
            with1->IntervalHrs = double(with1->DynaVars.h ) / 3600.0;  // needed for energy meters and storage devices
            for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
              if ( ! SolutionAbort )
                /*# with DynaVars do */
                {
                  auto& with2 = with1->DynaVars;
                  with1->Increment_time();
                  with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult( with2.dblHour );
              // Assume price signal stays constant for dynamic calcs
           /*Predictor*/
                  with2.IterationFlag = 0;
                  IntegratePCStates( ActorID );
                  with1->SolveSnap( ActorID );
           /*Corrector*/
                  with2.IterationFlag = 1;
                  IntegratePCStates( ActorID );
                  with1->SolveSnap( ActorID );
                  MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
                  FMonitorClass[ActorID]->update_sys_ld_info( ActorID );//get current value in INFO Broker -> UCF

              // attack and defense -> UCF
                  FMonitorClass[ActorID]->update_atks( ActorID );
                  FMonitorClass[ActorID]->update_defense_layer( ActorID );
              //-------------------------------
                  EndOfTimeStepCleanup( ActorID );
                }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
          }
          catch (...)
          {
              //
          }
        }
      }
      return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    //  SOLVE EMP MODE
    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

    int SolveEMP(int ActorID)
    {
    
        int n = 0;
        int nfinal;
        bool ret;
        int errcode;
        {
            auto with0 = ActiveCircuit[ActorID];
            auto with1 = with0->Solution;

            {
                try
                {
                    try {
                        //  Check if all generators have a dynamic model associated with them, if not, the throw exception
                        ret = GeneratorsHaveDynamicModel(ActorID);
                        if (!ret) {
                            errcode = 911;
                            throw errcode;
                        }
                    }
                    catch(...){
                        DoErrorMsg( "SolveEMP",
                                    "Simulation engine cannot find dynamic models for all generators in the case",
                                    "User must provide dynamic models for ALL generators in the case", errcode);
                        throw errcode;
                        return -1;
                    }
                    // This will initialize all state variables of all PCElements
                    if (!with1->DynamicsInitialized) {
                        InitializePCStates(ActorID);
                        with1->DynamicsInitialized = true; // If we're in dynamics mode, no need to re-initialize.
                    }
                    with1->IntervalHrs = double(with1->DynaVars.h) / 3600.0;  // needed for energy meters and storage devices
                    for (int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
                        if (!SolutionAbort)
                        {
                            //  This will calculate all state derivatives
                            CalculateStateDerivatives(ActorID);
                            auto& with2 = with1->DynaVars;
                            with1->Increment_time();
                            with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult(with2.dblHour);
                            with2.IterationFlag = 0;
                            
                            //  This will integrate states on all models - ab2 is the standard method
                            StateIntegration(ActorID); 
                            //  This will solve the network with the updated current injections from all models
                            
                            //  Add E1 + E3 impacts

                            //  Add Harmonics impacts

                            //  Add other weapons effects
                            ////  This will solve the network with the updated current injections from all models
                            with1->SolveSnap(ActorID);

                            /*
                            * 
                            *       This code can be enabled if we want to implement Heun's method (explicit trapezoidal)
                            *       To do so, we need to change the integraiton method used by StateIntegratino to Euler
                            *       since default is AB2
                            * 
                            * 
                            //  This will calculate all state derivatives
                            CalculateStateDerivatives(ActorID);
                            ////  This will integrate states on all models*/
                            //StateIntegration_correction(ActorID);
                            
                            ////  This will solve the network with the updated current injections from all models
                            //with1->SolveSnap(ActorID);
                            
                            //  Output variables from models
                            MonitorClass[ActorID]->SampleAll(ActorID);  // Make all monitors take a sample
                            FMonitorClass[ActorID]->update_sys_ld_info(ActorID);//get current value in INFO Broker -> UCF

                            // attack and defense -> UCF
                            FMonitorClass[ActorID]->update_atks(ActorID);
                            FMonitorClass[ActorID]->update_defense_layer(ActorID);
                            //-------------------------------
                            EndOfTimeStepCleanup(ActorID);
                            nfinal = n;
                        }
                    //          }
                    //          __finally
                    //          {
                    MonitorClass[ActorID]->SaveAll(ActorID);
                }
                catch (...)
                {
                    //
                }
            }
        }
        return 0;
    }


    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
    

    int SolveMonte1( int ActorID )
    {
      int result = 0;
      int n = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          try
          {
            with0->Set_LoadMultiplier(1.0);   // Always set with prop in case matrix must be rebuilt
            with1->IntervalHrs = 1.0;     // needed for energy meters and storage devices
            with1->DynaVars.intHour = 0;
            with1->DynaVars.dblHour = 0.0;// Use hour to denote Case number
            with1->DynaVars.T = 0.0;

            // MonitorClass.ResetAll;
            // EnergyMeterClass.ResetAll;

    /*        ProgressCaption( 'Monte Carlo Mode 1, ' + IntToStr(NumberofTimes) + ' Random Loads.', ActorID);
            ProgressCount := 0;    */
            for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
              if ( ! SolutionAbort )
              {
                with1->DynaVars.intHour++;
                with1->SolveSnap( ActorID );
                MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
                if (with1->SampleTheMeters )
                  EnergyMeterClass[ActorID]->SampleAll( ActorID );  // Make all meters take a sample
                ActorPctProgress[ActorID] = ( n * 100 ) / with1->NumberOfTimes;
    //            Show10PctProgress(N, NumberOfTimes, ActorID);
              }
              else
              {
                ErrorNumber = SOLUTION_ABORT;
                CmdResult = ErrorNumber;
                GlobalResult = "Solution Aborted";
                break;
              }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
            if (with1->SampleTheMeters )
              EnergyMeterClass[ActorID]->CloseAllDIFiles( ActorID );
    //        ProgressHide(ActorID);
          }
          catch (...)
          {
              //
          }
        }
      }
      return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveMonte2( int ActorID )

    // Do a daily load solution for several Random days

    {
      int result = 0;
      int i = 0, n = 0, Ndaily = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          try
          {
            with1->DynaVars.T = 0.0;
            with1->DynaVars.intHour = 0;
            with1->DynaVars.dblHour = 0.0;
            // MonitorClass.ResetAll;
            // EnergyMeterClass.ResetAll;
            with1->IntervalHrs = double(with1->DynaVars.h ) / 3600.0;  // needed for energy meters and storage devices
            Ndaily = Round( 24.0 / with1->IntervalHrs );
            if ( ! DIFilesAreOpen[ActorID] )
              EnergyMeterClass[ActorID]->OpenAllDIFiles( ActorID );   // Open Demand Interval Files, if desired

    /*        ProgressCaption('Monte Carlo Mode 2, ' + IntToStr(NumberofTimes) + ' Days.', ActorID);
            ProgressCount := 0;       */
            for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
              if ( ! SolutionAbort )
              {       // Number of Days

              // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
                switch (with1->RandomType )
                {
                  case UNIFORM:
                      with0->Set_LoadMultiplier(Random());
                  break;  // number between 0 and 1

                  case GAUSSIAN:
                      with0->Set_LoadMultiplier(Gauss(with0->DefaultDailyShapeObj->Get_Mean(), with0->DefaultDailyShapeObj->Get_StdDev() ));
                  break;
                }
                /*# with DynaVars do */
                auto& with2 = with1->DynaVars;
                for ( int stop = Ndaily, i = 1; i <= stop; i++)
                {
                  with1->Increment_time();
                  with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult( with2.dblHour );
                  with1->SolveSnap( ActorID );
                  MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
                  if (with1->SampleTheMeters )
                    EnergyMeterClass[ActorID]->SampleAll( ActorID );  // Make all meters take a sample
                  EndOfTimeStepCleanup( ActorID );
                }
                ActorPctProgress[ActorID] = ( n * 100 ) / with1->NumberOfTimes;
    //          Show10PctProgress(N, NumberOfTimes, ActorID);
              }
              else
              {
                ErrorNumber = SOLUTION_ABORT;
                CmdResult = ErrorNumber;
                GlobalResult = "Solution Aborted.";
                break;
              }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
            if (with1->SampleTheMeters )
              EnergyMeterClass[ActorID]->CloseAllDIFiles( ActorID );   // Save Demand interval Files
    //        ProgressHide(ActorID);
          }
          catch (...)
          {
              //
          }
        }
      }
      return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveMonte3( int ActorID )

    // Hold time fixed and just vary the global load multiplier

    {
      int result = 0;
      int n = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
        // Time must be set beFore entering this routine
          try
          {
            // MonitorClass.ResetAll;
            // EnergyMeterClass.ResetAll;
            with1->IntervalHrs = 1.0;  // just get per unit energy and multiply result as necessary
            if ( ! DIFilesAreOpen[ActorID] )
              EnergyMeterClass[ActorID]->OpenAllDIFiles( ActorID );   // Open Demand Interval Files, if desired

    /*        ProgressCaption( 'Monte Carlo Mode 3, ' + IntToStr(NumberofTimes) + ' Different Load Levels.', ActorID);
            ProgressCount := 0;    */
            with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult(with1->DynaVars.dblHour );
            if (with0->PriceCurveObj != NULL )
                with0->PriceSignal = with0->PriceCurveObj->GetPrice(with1->DynaVars.dblHour );
            for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
              if ( ! SolutionAbort )
              {

            // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
                switch (with1->RandomType )
                {
                  case UNIFORM:
                      with0->Set_LoadMultiplier(Random());
                  break;  // number between 0 and 1

                  case GAUSSIAN:
                      with0->Set_LoadMultiplier(Gauss(with0->DefaultDailyShapeObj->Get_Mean(), with0->DefaultDailyShapeObj->Get_StdDev() ));
                  break;
                  case LOGNORMAL:
                      with0->Set_LoadMultiplier(QuasiLogNormal(with0->DefaultDailyShapeObj->Get_Mean() ));
                  break;
                }
                with1->SolveSnap( ActorID );
                MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
                if (with1->SampleTheMeters )
                  EnergyMeterClass[ActorID]->SampleAll( ActorID );  // Make all meters take a sample
                ActorPctProgress[ActorID] = ( n * 100 ) / with1->NumberOfTimes;
    //            Show10PctProgress(N, NumberOfTimes, ActorID);
              }
              else
              {
                CmdResult = SOLUTION_ABORT;
                ErrorNumber = CmdResult;
                GlobalResult = "Solution Aborted";
                break;
              }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
            if (with1->SampleTheMeters )
              EnergyMeterClass[ActorID]->CloseAllDIFiles( ActorID );   // Save Demand interval Files
    //        ProgressHide(ActorID);
          }
          catch (...)
          {
              //
          }
        }
      } /*WITH*/
      return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveLD1( int ActorID )

    // Do a Daily Simulation based on a load duration curve

    {
      int result = 0;
      int n = 0, Ndaily = 0, i = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          try
          {
            if ( with0->LoadDurCurveObj == NULL )
            {
              DoSimpleMsg( "Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.", 470 );
              return result;
            }

      // Time must be set beFore entering this routine

          // MonitorClass.ResetAll;
          // EnergyMeterClass.ResetAll;
            Ndaily = Round( 24.0 / with1->DynaVars.h * 3600.0 );
            if ( ! DIFilesAreOpen[ActorID] )
              EnergyMeterClass[ActorID]->OpenAllDIFiles( ActorID );   // Open Demand Interval Files, if desired

    //      ProgressCaption( 'Load-Duration Mode 1 Solution. ', ActorID);

          // (set in Solve method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));
            with1->DynaVars.intHour = 0;
            /*# with DynaVars do */
            for ( int stop = Ndaily, i = 1; i <= stop; i++)
            {
              auto& with2 = with1->DynaVars;
          // Set the time
              with1->Increment_time();
              with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult( with2.dblHour );
              if ( ! SolutionAbort )
              {
                for ( int stop = with0->LoadDurCurveObj->get_FNumPoints(), n = 1; n <= stop; n++)
                {
                  with0->Set_LoadMultiplier(with0->LoadDurCurveObj->Mult( n ));  // Always set LoadMultiplier with prop in case matrix must be rebuilt
                  // Adjust meter interval to interval on value of present Load-Duration Curve
                  with1->IntervalHrs = with0->LoadDurCurveObj->Get_Interval();

              // Price curve must correspond to load-duration curve
                  if (with0->PriceCurveObj != NULL )
                      with0->PriceSignal = with0->PriceCurveObj->Price( n );
                  with1->SolveSnap( ActorID );
                  MonitorClass[ActorID]->SampleAll( ActorID );     // Make all monitors take a sample
                  if (with1->SampleTheMeters )
                    EnergyMeterClass[ActorID]->SampleAll( ActorID );  // Make all meters take a sample
                  EndOfTimeStepCleanup( ActorID );
                }
                ActorPctProgress[ActorID] = ( i * 100 ) / Ndaily;
    //           ShowPctProgress((i * 100) div NDaily, ActorID);
              }
              else
              {
                CmdResult = SOLUTION_ABORT;
                ErrorNumber = CmdResult;
                GlobalResult = "Solution Aborted";
                break;
              }
            }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
            if (with1->SampleTheMeters )
              EnergyMeterClass[ActorID]->CloseAllDIFiles( ActorID );   // Save Demand interval Files
    //      ProgressHide(ActorID);
          }
          catch (...)
          {
              //
          }
        }
      } /*WITH ActiveCircuit[ActiveActor]*/
      return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveLD2( int ActorID )

    // Hold time fixed and just vary the global load multiplier according to the global
    // Load-Duration Curve

    {
      int result = 0;
      int n = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          if ( with0->LoadDurCurveObj == NULL )
          {
            DoSimpleMsg( "Load Duration Curve Not Defined (Set LDCurve=... command). Cannot perForm solution.", 471 );
            return result;
          }

    // Time must be set beFore entering this routine


        // MonitorClass.ResetAll;
        // EnergyMeterClass.ResetAll;
          with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult( with1->DynaVars.dblHour );
          if ( ! DIFilesAreOpen[ActorID] )
            EnergyMeterClass[ActorID]->OpenAllDIFiles( ActorID );   // Open Demand Interval Files, if desired

        // (set in Solve Method) DefaultGrowthFactor :=  IntPower(DefaultGrowthRate, (Year-1));
          try
          {
            if ( SolutionAbort )
            {
              CmdResult = SOLUTION_ABORT;
              ErrorNumber = CmdResult;
              GlobalResult = "Solution Aborted.";
              return result;
            }
            for ( int stop = with0->LoadDurCurveObj->get_FNumPoints(), n = 1; n <= stop; n++)
            {

            // Adjust meter interval to interval on value of present Load-Duration Curve
                with0->Set_LoadMultiplier(with0->LoadDurCurveObj->Mult( n ));     // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
                with1->IntervalHrs = with0->LoadDurCurveObj->Get_Interval();

            // Price curve must correspond to load-duration curve
              if (with0->PriceCurveObj != NULL )
                  with0->PriceSignal = with0->PriceCurveObj->Price( n );
              with1->SolveSnap( ActorID );
              MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
              if (with1->SampleTheMeters )
                EnergyMeterClass[ActorID]->SampleAll( ActorID );  // Make all meters take a sample
              EndOfTimeStepCleanup( ActorID );
            }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
            if (with1->SampleTheMeters )
              EnergyMeterClass[ActorID]->CloseAllDIFiles( ActorID );   // Save Demand interval Files
          }
          catch (...)
          {
              //
          }
        }
      } /*WITH ActiveCircuit[ActiveActor]*/
      return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void PickAFault( int ActorID )
    // Enable one of the faults in the circuit.  Disable the rest

    {
      int NumFaults = 0, i = 0, Whichone = 0;
      TFaultObj* FaultObj;
      NumFaults = ActiveCircuit[ActorID]->Faults.get_myNumList();
      Whichone = trunc( Random() * NumFaults ) + 1;
      if ( Whichone > NumFaults )
        Whichone = NumFaults;
      for ( int stop = NumFaults, i = 1; i <= stop; i++)
      {
        FaultObj = (TFaultObj*) ActiveCircuit[ActorID]->Faults.Get( i );
        if ( i == Whichone )
        {
          ActiveFaultObj = FaultObj; // in Fault Unit
          FaultObj->Set_Enabled(true);
        }
        else
          FaultObj->Set_Enabled(false);
      }
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveMonteFault( int ActorID )
    {
      int result = 0;
      int n = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          try
          {
            with1->LoadModel = ADMITTANCE;   // All Direct solution
            with0->Set_LoadMultiplier(1.0);    // Always set LoadMultiplier WITH prop in case matrix must be rebuilt
            with1->DynaVars.intHour = 0;
            with1->DynaVars.dblHour = 0.0; // Use hour to denote Case number
            with1->DynaVars.T = 0.0;


          // MonitorClass.ResetAll;

    /*      ProgressCaption( 'Monte Carlo Fault Study: ' + IntToStr(NumberofTimes) + ' Different Faults.', ActorID);
          ProgressCount := 0;            */
            with1->SetGeneratorDispRef( ActorID );
            for ( int stop = with1->NumberOfTimes, n = 1; n <= stop; n++)
              if ( ! SolutionAbort )
              {
                  with1->DynaVars.intHour++;
                PickAFault( ActorID );  // Randomly enable one of the faults
                ActiveFaultObj->Randomize( ActorID );  // Randomize the fault resistance
                with1->SolveDirect( ActorID );
                MonitorClass[ActorID]->SampleAll( ActorID );  // Make all monitors take a sample
                ActorPctProgress[ActorID] = ( n * 100 ) / with1->NumberOfTimes;
    //          Show10PctProgress(N, NumberOfTimes, ActorID);
              }
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
    //      ProgressHide(ActorID);
          }
          catch (...)
          {
              //
          }
        }
      }
      return result;
    }

    /*--------------------------------------------------------------------------*/


    void AllocateAllSCParms( int ActorID )
    {
      int i = 0;
      /*# with ActiveCircuit[ActorID] do */
      {
        auto with0 = ActiveCircuit[ActorID];
        {
          for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
              with0->Buses[i - 1]->AllocateBusQuantities();
        }
      }
    }


    /*--------------------------------------------------------------------------*/


    void ComputeIsc( int ActorID )
    /* Compute Isc at all buses for current values of Voc and Ysc */
    {
      int i = 0;
      /*# with ActiveCircuit[ActorID] do */
      {
        auto with0 = ActiveCircuit[ActorID];
        {
          for ( int stop = with0->NumBuses, i = 1; i <= stop; i++)
            /*# with Buses^[i] do */
            {
              auto& with1 = with0->Buses[i - 1];
              {
                with1->Ysc.MVmult( &(with1->BusCurrent[0]), &(with1->VBus[0]));
              }
            }
        }
      }
    }


    /*--------------------------------------------------------------------------*/


    void ComputeYsc( int iB, int ActorID )

    /*Compute YSC for I-th bus*/
    /*Assume InjCurr is zeroed*/
    {
      int i = 0, j = 0, ref1 = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          /*# with Buses^[iB] do */
          {
            auto& with2 = with0->Buses[iB];
            {
              with2->Zsc.Clear();
              for ( int stop = with2->get_FNumNodesThisBus(), i = 1; i <= stop; i++)
              {
                ref1 = with2->GetRef( i );
                if ( ref1 > 0 )
                {
                  with1->Currents[ref1] = cONE;
              /*SparseSet expects 1st element of voltage array, not 0-th element*/
                  if (SolveSparseSet(with1->hYsystem, &(with1->NodeV[1]), &(with1->Currents[1])) < 1)
                  {
                      // throw EEsolv32Problem.Create( "Error Solving System Y Matrix in ComputeYsc. Problem with Sparse matrix solver." );
                  }
              /*Extract Voltage Vector = column of Zsc*/
                    for ( int stop = with2->get_FNumNodesThisBus(), j = 1; j <= stop; j++)
                    {
                        with2->Zsc.SetElement( j, i, with1->NodeV[with2->GetRef( j )] );
                    }
                  with1->Currents[ref1] = CZero;
                } /*IF ref...*/
              }
              with2->Ysc.CopyFrom( &( with2->Zsc ) );
              with2->Ysc.Invert(); /*Save as admittance*/
            }
          }
        }
      }
    }


    /*--------------------------------------------------------------------------*/


    void ComputeAllYsc( int ActorID )
    {
      int iB = 0, j = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          for ( int stop = with0->NumNodes, j = 1; j <= stop; j++)
            with1->Currents[j] = CZero;
          ActorProgressCount[ActorID] = 0;
          for ( int stop = with0->NumBuses, iB = 1; iB <= stop; iB++)
          {
            ComputeYsc( iB - 1, ActorID );  // Compute YSC for iB-th Bus
            if ( ( ( iB * 10 ) / with0->NumBuses ) > ProgressCount )
            {
              ActorProgressCount[ActorID]++;
    //            ShowPctProgress(30 + ActorProgressCount[ActorID] * 5, ActorID);
            }
          }
        }
      }
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void DisableAllFaults( int ActorID )
    {
      /*# with ActiveCircuit[ActorID] do */
      {
        auto with0 = ActiveCircuit[ActorID];
        {
          ActiveFaultObj = (TFaultObj*) with0->Faults.Get_First();
          while ( ActiveFaultObj != NULL )
          {
            ActiveFaultObj->Set_Enabled(false);
            ActiveFaultObj = (TFaultObj*) with0->Faults.Get_Next();
          }
        }
      }
    }


    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveFaultStudy( int ActorID )
    {
      int result = 0;
      result = 0;
      ActorPctProgress[ActorID] = 0;
    //   ShowPctProgress( 0, ActorID);
    //   ProgressCaption( 'Computing Open-Circuit Voltages', ActorID);
      /*# with ActiveCircuit[ActorID].Solution do */
      {
        auto with1 = ActiveCircuit[ActorID]->Solution;
        with1->LoadModel = ADMITTANCE;
        DisableAllFaults( ActorID );
        with1->SolveDirect( ActorID );   // This gets the open circuit voltages and bus lists corrected
        AllocateAllSCParms( ActorID );   // Reallocate bus quantities
        with1->UpdateVBus( ActorID );  // Put present solution Voc's in bus quantities
      }
      ActorPctProgress[ActorID] = 30;
      /*   ProgressCaption ('Computing Ysc Matrices for Each Bus', ActorID);
         ShowPctProgress (30, ActorID);*/
      ComputeAllYsc( ActorID );
      ActorPctProgress[ActorID] = 80;
      /*   ProgressCaption( 'Computing Short-circuit currents.', ActorID);
         ShowPctProgress (80, ActorID);*/
      ComputeIsc( ActorID );
      ActorPctProgress[ActorID] = 100;
      /*   ShowPctProgress ( 100, ActorID);
         ProgressCaption ('Done.', ActorID);*/
      //   ProgressHide(ActorID);
         // Now should have all we need to make a short circuit report
      return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void AddFrequency( pDoubleArray& FreqList, int& NumFreq, int& MaxFreq, double F )

    /*Add unique Frequency, F to list in ascending order, reallocating if necessary*/
    {
      int i = 0, j = 0;

         /*See if F is in List*/
      for ( int stop = NumFreq, i = 1; i <= stop; i++)
      {
             /*Allow a little tolerance (0.1 hz) for the Frequency for round off error*/
        if ( Abs( F - FreqList[i] ) < 0.1 )
          return; // Already in List, nothing to do
      }

         /*OK, it's not in list, so let's Add it*/
      NumFreq++;
      if ( NumFreq > MaxFreq )
      {  // Let's make a little more room
        std::vector <double> myTmp;
        int oldSize = MaxFreq;
        for (i = 0; i <= MaxFreq; i++)
            myTmp.push_back(FreqList[i]);
        MaxFreq += 20;
        FreqList = (pDoubleArray) realloc(FreqList, ( sizeof(double) * MaxFreq ) + 2);
        for (i = 0; i <= oldSize; i++)
            FreqList[i] = myTmp[i];
      }

         /*Let's add it in ascending order*/
      for ( int stop = NumFreq - 1, i = 1; i <= stop; i++)
      {
        if ( F < (FreqList)[i] )
        {
                 /*Push down array and insert it*/
          for ( int stop = i, j = NumFreq - 1; j >= stop; j--)
            FreqList[j + 1] = FreqList[j];
          (FreqList)[i] = F;
          return;  // We're done!
        }
      }

         /*If we fall through, tack it on to the end*/
      (FreqList)[NumFreq] = F;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    double GetSourceFrequency( TPCElement* pc ) // TODO - applicable to VCCS?

    {
      double result = 0.0;
      TVsourceObj* pVsrc;
      TIsourceObj* pIsrc;
      if ( CompareText( pc->Get_myPName(), "vsource" ) == 0 )
      {
        pVsrc = ( TVsourceObj* ) pc;
        result = pVsrc->SrcFrequency;
      }
      else
      {
        pIsrc = ( TIsourceObj* ) pc;
        result = pIsrc->SrcFrequency;
      }
      return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void CollectAllFrequencies( pDoubleArray& FreqList, int& NumFreq, int ActorID )
    {
      std::vector <longInt> SpectrumInUse;
      TPCElement* p;
      int MaxFreq = 0, i = 0, j = 0;
      TSpectrumObj* pSpectrum;
      double F = 0.0;
        /*Make a List of all frequencies in Use*/

        /*accumulate all unique Frequencies*/
      MaxFreq = 20;    // Initial List size
      NumFreq = 0;
      FreqList = (pDoubleArray) realloc(FreqList, ( sizeof(double) * MaxFreq ) + 2);
      /*# with ActiveCircuit[ActorID] do */
      {
        auto with0 = ActiveCircuit[ActorID];
        {
            /*Check Sources -- each could have a different base frequency*/
          p = (TPCElement*) with0->Sources.Get_First();
          while ( p != NULL )
          {
            if ( p->Get_Enabled() )
              if ( SpectrumClass[ActorID]->Find( p->Spectrum ) != NULL )
              {
                pSpectrum = (TSpectrumObj*) SpectrumClass[ActorID]->GetActiveObj();
                F = GetSourceFrequency( p );
                for ( int stop = pSpectrum->NumHarm, j = 1; j <= stop; j++)
                {
                  AddFrequency( FreqList, NumFreq, MaxFreq, (pSpectrum->HarmArray)[j - 1] * F );
                }
              }
            p = (TPCElement*) with0->Sources.Get_Next();
          }
        }
      }

        /*Mark Spectra being used*/
            /*Check loads and generators - these are assumed to be at fundamental frequency*/
      SpectrumInUse.resize( SpectrumClass[ActorID]->Get_ElementCount() + 1);  //Allocate and zero
      /*# with ActiveCircuit[ActorID] do */
      {
        auto with1 = ActiveCircuit[ActorID];
        {
          p = (TPCElement*) with1->PCElements.Get_First();
          while ( p != NULL )
          {
            if ( p->Get_Enabled() )
              if ( SpectrumClass[ActorID]->Find( p->Spectrum ) != NULL )
              {
                (SpectrumInUse)[SpectrumClass[ActorID]->get_ActiveElement()] = 1;
              }
            p = (TPCElement*) with1->PCElements.Get_Next();
          }
        }
      } /*With*/

        /*Add marked Spectra to list*/
      for ( int stop = SpectrumClass[ActorID]->Get_ElementCount(), i = 1; i <= stop; i++)
      {
        if ( (SpectrumInUse)[i] == 1 )
        {
          SpectrumClass[ActorID]->Set_Active(i);
          pSpectrum = (TSpectrumObj*) SpectrumClass[ActorID]->GetActiveObj();
          for ( int stop = pSpectrum->NumHarm, j = 1; j <= stop; j++)
          {
            AddFrequency( FreqList, NumFreq, MaxFreq, (pSpectrum->HarmArray)[j - 1] * ActiveCircuit[ActorID]->Fundamental );
          }
        }
      }
      SpectrumInUse.clear();
    }


    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    int SolveHarmonic( int ActorID )
    {
      int result = 0;
      pDoubleArray FrequencyList;
      int i = 0, NFreq = 0;
      result = 0;
      FrequencyList = NULL;   // Set up for Reallocmem
    /*   ShowPctProgress ( 0, ActorID);
       ProgressCaption( 'Performing Harmonic Solution', ActorID);    */
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          try
          {
            if ( with1->get_FFrequency() != with0->Fundamental )
            {     // Last solution was something other than fundamental
                with1->Set_Frequency(with0->Fundamental);
              if ( ! RetrieveSavedVoltages() )
                return result;  /*Get Saved fundamental frequency solution*/
            }
            MonitorClass[ActorID]->SampleAll( ActorID );   // Store the fundamental frequency in the monitors

           /* Get the list of Harmonic Frequencies to solve at*/
            if (with1->DoAllHarmonics )
              CollectAllFrequencies( FrequencyList, NFreq, ActorID );   // Allocates FrequencyList
            else
            {
              FrequencyList = (pDoubleArray) realloc(FrequencyList, sizeof(double) * with1->HarmonicListSize + 1);
              NFreq = with1->HarmonicListSize;
              for ( int stop = NFreq, i = 1; i <= stop; i++)
                (FrequencyList)[i] = with0->Fundamental * (with1->HarmonicList)[i];
            }

            for ( int stop = NFreq, i = 1; i <= stop; i++)
            {
              with1->Set_Frequency((FrequencyList)[i]);
              if ( Abs( with1->Harmonic - 1.0 ) > EPSILON )
              {    // Skip fundamental
    /*               ProgressCaption ( 'Solving at Frequency = ' + Format('%-g', [Frequency]), ActorID);
                   ShowPctProgress ( Round((100.0*i)/Nfreq), ActorID);*/
                ActorPctProgress[ActorID] = Round( ( 100.0 * i ) / NFreq );
                with1->SolveDirect( ActorID );
                MonitorClass[ActorID]->SampleAll( ActorID );
                   // Storage devices are assumed to stay the same since there is no time variation in this mode
              }
            } /*FOR*/

    /*       ShowPctProgress ( 100, ActorID);
           ProgressCaption ( 'Done.',ActorID);    */
//          }
//          __finally
//          {
    //       ProgressHide(ActorID);
            MonitorClass[ActorID]->SaveAll( ActorID );
            free(FrequencyList);
          }
          catch (...)
          {
              //
          }
         // Now should have all we need to make a short circuit report
        }
      }
      return result;
    }

    //========================================================================================



    int SolveHarmTime( int ActorID )     // It is based in SolveGeneralTime routine

    {
      int result = 0;
      result = 0;
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          with1->IntervalHrs = double( with1->DynaVars.h ) / 3600.0;  // needed for energy meters and storage devices
          if ( ! SolutionAbort )
            /*# with DynaVars do */
            {
                  /*Compute basic multiplier from Default loadshape to use in generator dispatch, if any*/
              with0->DefaultHourMult = with0->DefaultDailyShapeObj->GetMult( with1->DynaVars.dblHour );
              with1->SolveSnap( ActorID );
              //      Increment_time;  // This function is handeled from SolveHarmonics (04-10-2013)
            }
        }
      }
      return result;
    }
    //=============================================================================



    int SolveHarmonicT( int ActorID )
    {
      int result = 0;
      pDoubleArray FrequencyList;
      int i = 0, NFreq = 0;
      result = 0;
      FrequencyList = NULL;   // Set up for Reallocmem
      /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
      {
        auto with0 = ActiveCircuit[ActorID];
        auto with1 = ActiveCircuit[ActorID]->Solution;
        {
          with1->IntervalHrs = double(with1->DynaVars.h ) / 3600.0;  // needed for energy meters and storage devices
          try
          {
            if (with1->get_FFrequency() != with0->Fundamental )
            {     // Last solution was something other than fundamental
                with1->Set_Frequency(with0->Fundamental);
              if ( ! RetrieveSavedVoltages() )
                return result;  /*Get Saved fundamental frequency solution*/
            }
    //     DefaultHourMult := DefaultDailyShapeObj->GetMult(DynaVars.dblHour);
    //     IF Load_Changed THEN Begin    //Added to update the current sources of all frequencies any time
            InitializeForHarmonics( ActorID );  //the value of a load changes in a proportional way
    //            Load_Changed:=FALSE;     // Added 05 dec 2013 - D. Montenegro
    //     End;
            with1->SolveSnap( ActorID );
            MonitorClass[ActorID]->SampleAll( ActorID );   // Store the fundamental frequency in the monitors
           /* Get the list of Harmonic Frequencies to solve at*/
            if (with1->DoAllHarmonics )
              CollectAllFrequencies( FrequencyList, NFreq, ActorID );   // Allocates FrequencyList
            else
            {
              FrequencyList = (pDoubleArray) realloc(FrequencyList, ( sizeof(double) * with1->HarmonicListSize ) + 2);
              NFreq = with1->HarmonicListSize;
              for ( int stop = NFreq, i = 1; i <= stop; i++)
                (FrequencyList)[i] = with0->Fundamental * (with1->HarmonicList)[i];
            }

            std::vector <double> myTmp;
            for (i = 0; i <= NFreq; i++)
                myTmp.push_back(FrequencyList[i]);

            for ( int stop = NFreq, i = 1; i <= stop; i++)
            {
              with1->Set_Frequency((FrequencyList)[i]);
              if ( Abs(with1->Harmonic - 1.0 ) > EPSILON )
              {    // Skip fundamental
    //               DefaultHourMult := DefaultDailyShapeObj->GetMult(DynaVars.dblHour);
                SolveHarmTime( ActorID );
                MonitorClass[ActorID]->SampleAll( ActorID );
                EndOfTimeStepCleanup( ActorID );
                  // Storage devices are assumed to stay the same since there is no time variation in this mode  (Not necessarelly now)
              }
            } /*FOR*/
            with1->Increment_time();
//          }
//          __finally
//          {
            MonitorClass[ActorID]->SaveAll( ActorID );
            free(FrequencyList);
          }
          catch (...)
          {
              //
          }
        }
      }
      return result;
    }

    //================================================================================================================
    bool GeneratorsHaveDynamicModel(int ActorID)
        /*
        This function will loop over all PCElements that are of generator type 
        and check if they have a dynamic model associated with them.  If they do, it will allow
        simulation initialization, if not, then an error will be thrown directing the 
        user to correct the issue.
        */
    {
        bool is_gen = false;
        bool gen_exists = false;

        TPCElement* PCelem;
        {
            auto with0 = ActiveCircuit[ActorID];                            //  Get active circuit
            {
                PCelem = (TPCElement*)with0->PCElements.Get_First();              //  Get first PC element
                while (PCelem != NULL)
                {
                    //  First, check if the PC is a generator
                    is_gen = PCelem->IsGenerator();

                    //  If it is, then check for a dynamic model
                    if (is_gen)
                    {
                        gen_exists = PCelem->CheckForGeneratorModel();
                        //  If generator model doesn't exist, then throw an error and break loop
                        if (!gen_exists) {
                            return false;
                        }
                    }

                    //  Go to next element if current element is not a generator
                    PCelem = (TPCElement*)with0->PCElements.Get_Next();
                }
            }
        }
        return true;
    }

    //================================================================================================================

    void SolutionAlgs_initialization()
    {
      //IsMultiThread = true;
    }

    class SolutionAlgs_unit
    {
    public:
    SolutionAlgs_unit()
    {
      SolutionAlgs_initialization();
    }
    };
    SolutionAlgs_unit _SolutionAlgs_unit;

}

