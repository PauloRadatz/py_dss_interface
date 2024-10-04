
#pragma hdrstop

#include "AutoAdd.h"

#include "EnergyMeter.h"
#include "generator.h"
#include "DSSGlobals.h"
#include "PDElement.h"
#include "Utilities.h"
#include "Executive.h"
#include "CmdForms.h"  
#include "Solution.h"

namespace AutoAdd
{




    double SumSelectedRegisters(TEnergyMeterObj Mtr, pIntegerArray Regs, int count)
    {
        double result = 0.0;
        int i = 0;
        result = 0.0;
        /*# with Mtr do */
        auto with0 = Mtr;
        for (int stop = count, i = 1; i <= stop; i++)
        {
            result = result + with0.Registers[(Regs)[i - 1]] * with0.TotalsMask[(Regs)[i - 1]];
        }
        return result;
    }


    TAutoAdd::TAutoAdd()
        : BusIdxListSize(0),
        BusIdxListCreated(false),
        LastAddedGenerator(0),
        LastAddedCapacitor(0),
        BusIndex(0),
        Phases(0),
        Ycap(0.0),
        kWLosses(0.0),
        BaseLosses(0.0),
        puLossImprovement(0.0),
        kWEEN(0.0),
        BaseEEN(0.0),
        puEENImprovement(0.0),
        ProgressCount(0),
        GenkW(0.0),
        GenPF(0.0),
        Genkvar(0.0),
        Capkvar(0.0),
        AddType(0),
        ModeChanged(false)
    {
        CapacitorClass = (TCapacitor*) DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find("capacitor"));

        // AutoAdd defaults
        GenkW = 1000.0;
        GenPF = 1.0;
        Capkvar = 600.0;
        AddType = GENADD;
        LastAddedGenerator = 0;
        LastAddedCapacitor = 0;
        ModeChanged = true;
    }


    TAutoAdd::~TAutoAdd()
    {
        if (BusIdxListCreated)
            free(BusIdxList);
        // todo check:  inherited::Destroy();
    }


    void TAutoAdd::MakeBusList(int ActorID)
        // Make a list of unique busnames
        // IF AutoAddBusList in ActiveCircuit[ActorID] is not nil, use this list.
        // ELSE, Use the element lists in Energy Meters
        // IF no Energy Meters, use all the buses in the active circuit

    {
        TEnergyMeterObj* pMeter;
        int retval = 0;
        String Bname;
        int i = 0;
        TPDElement* PDElem;
        THashList FBusList;
        bool FBusListCreatedHere = false;
        if (BusIdxListCreated)
            free(BusIdxList);
        FBusListCreatedHere = false;
        BusIdxListCreated = false;

        // Autoaddbuslist exists in Active Circuit, use it  (see set Autobuslist=)
        if (ActiveCircuit[ActorID]->AutoAddBusList.Get_NumElements() > 0)
            FBusList = ActiveCircuit[ActorID]->AutoAddBusList;
        else
            if (ActiveCircuit[ActorID]->EnergyMeters.get_myNumList() == 0)
            {
                // No energymeters in circuit
                // Include all buses in the circuit
                BusIdxListSize = ActiveCircuit[ActorID]->BusList.Get_NumElements();
                BusIdxList = (pIntegerArray) realloc(BusIdxList, sizeof(long) * BusIdxListSize);
                for (int stop = BusIdxListSize, i = 1; i <= stop; i++)
                {
                    (BusIdxList)[i - 1] = i;
                }
                BusIdxListCreated = true;
                return;
            }
            else
            {
                /*Construct Bus List from Energy Meters Zone Lists*/
                // Include only buses in EnergyMeter lists
                    // Consider all meters
                FBusListCreatedHere = true;
                FBusList = THashList(ActiveCircuit[ActorID]->NumBuses);
                pMeter = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_First();
                while (pMeter != NULL)
                {
                    if (pMeter->BranchList != NULL)
                    {
                        PDElem = (TPDElement*) pMeter->BranchList->Get_First();
                        while (PDElem != NULL)
                        { // add only unique busnames
                            for (int stop = ( (TDSSCktElement*) PDElem )->Get_NTerms(), i = 1; i <= stop; i++)
                            {
                                Bname = StripExtension( ( (TDSSCktElement*) PDElem )->GetBus(i));
                                retval = FBusList.Find(Bname);
                                if (retval == 0)
                                {
                                    FBusList.Add(Bname);    // return value is index of bus
                                }
                            }
                            PDElem = (TPDElement*) pMeter->BranchList->Get_Forward();
                        }
                    }
                    pMeter = (TEnergyMeterObj*) ActiveCircuit[ActorID]->EnergyMeters.Get_Next();
                }
            }

        // Make busIdxList from FBusList
        BusIdxListSize = FBusList.Get_NumElements();
        BusIdxList = (pIntegerArray)realloc(BusIdxList, sizeof(long) * BusIdxListSize); 
        for (int stop = BusIdxListSize, i = 1; i <= stop; i++)
        {
            (BusIdxList)[i - 1] = ActiveCircuit[ActorID]->BusList.Find(FBusList.Get(i));
        }
        if (FBusListCreatedHere)
            FBusList.Clear();
        BusIdxListCreated = true;
    }


    double TAutoAdd::Get_WeightedLosses()

        // Returns losses in metered part of circuit +
        // weighted EEN values

        /*If no meters, returns just total losses in circuit*/

        /*Base everything on gen kW*/

    {
        double result = 0.0;
        ComputekWLosses_EEN(ActiveActor);
        if (ActiveCircuit[ActiveActor]->EnergyMeters.get_myNumList() == 0)
        {
            // No energymeters in circuit
            // Just go by total system losses
            puLossImprovement = (BaseLosses - kWLosses) / GenkW;
            puEENImprovement = 0.0;
            result = puLossImprovement;
        }
        else
            /*# with ActiveCircuit[ActiveActor] do */
        {
            auto with0 = ActiveCircuit[ActiveActor];
            {
                puLossImprovement = (BaseLosses - kWLosses) / GenkW;
                puEENImprovement = (BaseEEN - kWEEN) / GenkW;
                result = with0->LossWeight * puLossImprovement + with0->UEWeight * puEENImprovement;
            }
        }
        return result;
    }


    void TAutoAdd::AppendToFile(const String WhichFile, const String S)
    {
        TTextRec F;
        String Fname;
        try
        {
            Fname = GetOutputDirectory() + CircuitName_[ActiveActor] + "AutoAdded" + WhichFile + ".txt";
            AssignFile(F, Fname);
            if (FileExists(Fname))
                Append(F);
            else
                Rewrite(F);
            IOResultToException();
            WriteLn(F, S);
        }
        catch (std::exception &E)
        {
            DoSimpleMsg(String("Error TRYing to append to ") + Fname + CRLF + (std::string) E.what(), 438);
        }
        CloseFile(F);
    }




    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    String TAutoAdd::GetUniqueGenName()
    {
        String result;


        // TimeStmp:        TTimeStamp;

        String TrialName;
        bool Done = false;
        do
        {
            Done = true;
            LastAddedGenerator++;
            TrialName = "Gadd" + IntToStr(LastAddedGenerator);
            if ( ( (TDSSClass*) GeneratorClass )->Find(TrialName) != NULL)
                Done = false;
        } while (!(Done));
        result = TrialName;
        return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    String TAutoAdd::GetUniqueCapName()
    {
        String result;


        // TimeStmp:        TTimeStamp;

        String TrialName;
        bool Done = false;
        // TimeStmp := DateTimeToTimeStamp(Now);
        // Result := IntToStr(TimeStmp.date-730000)+'_'+IntToStr(TimeStmp.time);
        do
        {
            Done = true;
            LastAddedCapacitor++;
            TrialName = "Cadd" + IntToStr(LastAddedCapacitor);
            if ( ( (TDSSClass*) CapacitorClass )->Find(TrialName) != NULL)
                Done = false;
        } while (!(Done));
        result = TrialName;
        return result;
    }

    int TAutoAdd::Solve(int ActorID) // Automatically add caps or generators
    /*
     Automatically add a specified size of generator or capacitor at the location
     that results in the lowest losses in either metered part of circuit or
     total circuit, if no meters.

     If metered, EEN is also added in WITH a selected weighting factor (see
     set ueweight= ... command).

     Thus, this algorithm placed generators and capacitors to minimize losses and
     potential unserved energy.

    */

    {
        int result = 0;
        double LossImproveFactor = 0.0, MaxLossImproveFactor = 0.0;
        int MinLossBus = 0, MinBusPhases = 0;
        String Testbus;
        int i = 0;
        String CommandString;
        double kVrat = 0.0, TestGenkW = 0.0, TestCapkvar = 0.0;
        int ProgressMax = 0;

        /*  Algorithm:
             1) makes a list of buses to check, either
                a. Previously defined list
                b. Meter zone lists
                c. All buses, if neither of the above
             2) Inject a current corresponding to the generator
             3) Check test criteria
             4) Save result
             5) Add generator/capacitor to circuit

        */
        result = 0;
        /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
        {
            auto with0 = ActiveCircuit[ActorID];
            auto with1 = ActiveCircuit[ActorID]->Solution;
            {
                if (with1->LoadModel == ADMITTANCE)
                {
                    with1->LoadModel = POWERFLOW;
                    with1->SystemYChanged = true;  // Force rebuild of System Y without Loads
                }

                /*Do a preliminary snapshot solution to Force definition of meter zones
                 And set bus lists*/
                EnergyMeterClass[ActorID]->ResetAll(ActorID);
                if (with1->SystemYChanged || with0->get_FBusNameRedefined())
                {
                    with1->SolveSnap(ActorID);
                    ModeChanged = true;
                }
                EnergyMeterClass[ActorID]->SampleAll(ActorID);

                /* Check to see if bus base voltages have been defined */
                if (with0->Buses[with0->NumBuses - 1]->kVBase == 0.0)
                    with1->SetVoltageBases(ActorID);
                if (ModeChanged)
                {
                    MakeBusList(ActorID);  // Make list of buses to check
                    ModeChanged = false;  /*Keep same BusIdxList if no changes*/
                }
                with1->IntervalHrs = 1.0;

                /*Start up Log File*/
                AssignFile(FLog, GetOutputDirectory() + CircuitName_[ActiveActor] + "AutoAddLog.CSV");
                Rewrite(FLog);
                IOResultToException();
                WriteLn(FLog, "\"Bus\", \"Base kV\", \"kW Losses\", \"% Improvement\", \"kW UE\", \"% Improvement\", \"Weighted Total\", \"Iterations\"");
                CloseFile(FLog); // Close it now after clearing it out

              // for this solution mode, only the peak load condition is taken into account
              // load is adjusted for growth by year.
                with1->SetGeneratorDispRef(ActorID);

                /*Turn regulators and caps off while we are searching*/
                with1->ControlMode = CONTROLSOFF;
                SetBaseLosses();  /*Establish base values*/
                switch (AddType)
                {
                case GENADD:
                {
                    if (with0->PositiveSequence)
                        TestGenkW = GenkW / 3.0;
                    else
                        TestGenkW = GenkW;
                    if (GenPF != 0.0)
                    {
                        Genkvar = TestGenkW * sqrt(1.0 / sqr(GenPF) - 1.0);
                        if (GenPF < 0.0)
                            Genkvar = -Genkvar;
                    }
                    else
                    {   // Someone goofed and specified 0.0 PF
                        GenPF = 1.0;
                        Genkvar = 0.0;
                    }
                    MinLossBus = 0;   // null string
                    MaxLossImproveFactor = -1.0e50;  // Some very large neg number
                    MinBusPhases = 3;



                    /*Progress meter*/
 //                   {$IFDEF MSWINDOWS}
 //                   ProgressCaption( 'AutoAdding Generators', ActorID);
 //                   {$ENDIF}
                    ProgressMax = BusIdxListSize;
                    ProgressCount = 0;
                    //                   {$IFDEF MSWINDOWS}
                    //                   ProgressFormCaption( Format('Testing %d buses. Please Wait... ',[BusIdxListSize]), ActorID);
                    //                  {$ENDIF}
                    //                   ShowPctProgress(0, ActorID);
                    for (int stop = BusIdxListSize, i = 1; i <= stop; i++)
                    {
                        ProgressCount++;
                        BusIndex = (BusIdxList)[i - 1];
                        if (BusIndex > 0)
                        {
                            Testbus = with0->BusList.Get(BusIndex);
                            // ProgressFormCaption( 'Testing bus ' + TestBus);
                            if (((ProgressCount % 20) == 0) || (i == BusIdxListSize))
                            {

                                //                          ProgressFormCaption( Format('Testing bus %d/%d. ',[i,BusIdxListSize]), ActorID);
                              //                            ShowPctProgress (Round((100 * ProgressCount)/ProgressMax), ActorID);
                            }
                            EnergyMeterClass[ActorID]->ResetAll(ActorID);

                            /*Get the Number of Phases at this bus and the Node Ref and add into the Aux Current Array*/

                            /*Assume either a 3-phase or 1-phase generator*/
                            if (with0->Buses[BusIndex - 1]->get_FNumNodesThisBus() < 3)
                                Phases = 1;
                            else
                                Phases = 3;
                            GenVA = cmplx(1000.0 * TestGenkW / Phases, 1000.0 * Genkvar / Phases);

                            /* - -- - - - - - - Solution - - - - - - - - - - - - - - -*/
                            with0->Issolved = false;
                            with1->UseAuxCurrents = true;   // Calls InjCurrents on callback
                            with1->SolveSnap(ActorID);
                            if (with0->Issolved)
                            {
                                /*Only do this if solution converged ELSE something might break
                                 in meter sampling*/
                                EnergyMeterClass[ActorID]->SampleAll(ActorID);
                                LossImproveFactor = Get_WeightedLosses();
                                try
                                {
                                    Append(FLog);
                                    IOResultToException();
                                    Write(FLog, Format("\"%s\", %-g", Testbus.c_str(), with0->Buses[BusIndex - 1]->kVBase* SQRT3));
                                    Write(FLog, Format(", %-g, %-g", kWLosses, puLossImprovement * 100.0));
                                    Write(FLog, Format(", %-g, %-g", kWEEN, puEENImprovement * 100.0));
                                    WriteLn(FLog, Format(", %-g, %d", LossImproveFactor, with1->Iteration));
                            /* }
                                __finally
                                {*/
                                    CloseFile(FLog);
                                }
                                catch (...)
                                {
                                    //
                                }
                                if (LossImproveFactor > MaxLossImproveFactor)
                                {
                                    MaxLossImproveFactor = LossImproveFactor;
                                    MinLossBus = BusIndex;
                                    MinBusPhases = Phases;
                                }
                            }
                        }
                        if (SolutionAbort)
                            break;
                    }

                    /*Put Control mode back to default before inserting Generator for real*/
                    with1->ControlMode = CTRLSTATIC;
                    with1->UseAuxCurrents = false;
                    if (MinLossBus > 0)
                        /*# with DSSExecutive[ActorID] do */
                    {
                        auto with2 = DSSExecutive[ActorID];
                        {
                            if (MinBusPhases >= 3)
                                kVrat = with0->Buses[MinLossBus - 1]->kVBase * SQRT3;
                            else
                                kVrat = with0->Buses[MinLossBus - 1]->kVBase;
                            CommandString = String("New, generator.") + GetUniqueGenName() + ", bus1=\"" + with0->BusList.Get(MinLossBus) + "\", phases=" + IntToStr(MinBusPhases) + ", kv=" + Format("%-g", kVrat) + ", kw=" + Format("%-g", TestGenkW) + ", "
                                + Format("%5.2f", GenPF) + Format("! Factor =  %-g (%-.3g, %-.3g)", MaxLossImproveFactor, with0->LossWeight, with0->UEWeight);
                            with2->Set_Command(CommandString);    // Defines Generator

                                   // AppEnd this command to '...AutoAddedGenerators.Txt'
                            AppendToFile("Generators", CommandString);
                            with1->SolveSnap(ActorID);  // Force rebuilding of lists
                        }
                    }
                    // Return location of added generator so that it can
                    // be picked up through the result string of the COM interface
                    GlobalResult = with0->BusList.Get(MinLossBus) + Format(", %-g", MaxLossImproveFactor);
                    //{$IFDEF MSWINDOWS}
     //               ProgressHide(ActorID);
                    //{$ENDIF}

                    // note that the command that added the generator can be
                    // picked up from the Command property of the COM interface.
                }
                break;
                case CAPADD:
                {
                    MinLossBus = 0;   // null string
                    MaxLossImproveFactor = -1.0e50;  // Some very large number
                    MinBusPhases = 3;
                    if (with0->PositiveSequence)
                        TestCapkvar = Capkvar / 3.0;
                    else
                        TestCapkvar = Capkvar;

                    /*Progress meter*/
 //                   ProgressCaption ( 'AutoAdding Capacitors', ActorID);
                    ProgressMax = BusIdxListSize;
                    ProgressCount = 0;
                    for (int stop = BusIdxListSize, i = 1; i <= stop; i++)
                    {
                        ProgressCount++;
                        /*Make sure testbus is actually in the circuit*/
                        BusIndex = (BusIdxList)[i - 1];
                        if (BusIndex > 0)
                        {
                            Testbus = with0->BusList.Get(BusIndex);
                            //                         ProgressFormCaption('Testing bus ' + TestBus, ActorID);
                            //                         ShowPctProgress ( Round((100 * ProgressCount)/ProgressMax), ActorID);
                            EnergyMeterClass[ActorID]->ResetAll(ActorID);

                            /*Get the Number of Phases at this bus and the Node Ref and add into the Aux Current Array*/

                           /*Assume either a 3-phase or 1-phase Capacitor*/
                            if (with0->Buses[BusIndex - 1]->get_FNumNodesThisBus() < 3)
                                Phases = 1;
                            else
                                Phases = 3;

                            // Apply the capacitor at the bus rating
                            kVrat = with0->Buses[BusIndex - 1]->kVBase;  // L-N Base kV
                            Ycap = (TestCapkvar * 0.001 / Phases) / (kVrat * kVrat);


                            /* - -- - - - - - - Solution - - - - - - - - - - - - - - -*/
                            with0->Issolved = false;
                            with1->UseAuxCurrents = true;    // Calls InjCurrents on callback
                            with1->SolveSnap(ActorID);
                            if (with0->Issolved)
                            {
                                /*Only do this if solution converged ELSE something might break
                                 in meter sampling*/
                                EnergyMeterClass[ActorID]->SampleAll(ActorID);
                                LossImproveFactor = Get_WeightedLosses();
                                try
                                {
                                    Append(FLog);
                                    IOResultToException();
                                    Write(FLog, Format("\"%s\", %-g", Testbus.c_str(), with0->Buses[BusIndex - 1]->kVBase * SQRT3));
                                    Write(FLog, Format(", %-g, %-g", kWLosses, puLossImprovement * 100.0));
                                    Write(FLog, Format(", %-g, %-g", kWEEN, puEENImprovement * 100.0));
                                    WriteLn(FLog, Format(", %-g, %d", LossImproveFactor, with1->Iteration));
                             /* }
                                __finally
                                {*/
                                    CloseFile(FLog);
                                }
                                catch (...)
                                {
                                    //
                                }
                                if (LossImproveFactor > MaxLossImproveFactor)
                                {
                                    MaxLossImproveFactor = LossImproveFactor;
                                    MinLossBus = BusIndex;
                                    MinBusPhases = Phases;
                                }
                            }
                        }
                        if (SolutionAbort)
                            break;
                    }


                    /*Put Control mode back to default before inserting Capacitor for real*/
                    with1->ControlMode = CTRLSTATIC;
                    with1->UseAuxCurrents = false;
                    if (MinLossBus > 0)
                        /*# with DSSExecutive[ActorID] do */
                    {
                        auto with2 = DSSExecutive[ActorID];
                        {
                            if (MinBusPhases >= 3)
                                kVrat = with0->Buses[MinLossBus - 1]->kVBase * SQRT3;
                            else
                                kVrat = with0->Buses[MinLossBus - 1]->kVBase;
                            CommandString = String("New, Capacitor.") + GetUniqueCapName() + ", bus1=\"" + with0->BusList.Get(MinLossBus) + "\", phases=" + IntToStr(MinBusPhases) + ", kvar=" + Format("%-g", TestCapkvar) + ", kv=" + Format("%-g", kVrat);
                            with2->Set_Command(CommandString);     // Defines capacitor

                                   // AppEnd this command to 'DSSAutoAddedCapacitors.Txt'
                            AppendToFile("Capacitors", CommandString);
                            with1->SolveSnap(ActorID);  // for rebuilding of lists, etc.
                        }
                    }
                    // Return location of added generator so that it can
                    // be picked up through the result string of the COM interface
                    GlobalResult = with0->BusList.Get(MinLossBus);

                    // note that the command that added the generator can be
                    // picked up from the Command property of the COM interface.
                }
                break;
                }
            }
        }
        return result;
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void TAutoAdd::AddCurrents(int SolveType, int ActorID)

        /* Compute injection Currents for generator or capacitor and add into
          system Currents array
        */
    {
        complex BusV;
        int i = 0, Nref = 0;
        switch (AddType)
        {
        case GENADD:
            /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
        {
            auto with0 = ActiveCircuit[ActorID];
            auto with1 = ActiveCircuit[ActorID]->Solution;
            {
                /*For buses with voltage <> 0, add into aux current array*/
                for (int stop = Phases, i = 1; i <= stop; i++)
                {
                    Nref = with0->Buses[BusIndex - 1]->GetRef(i);
                    if (Nref > 0)
                    {   // add in only non-ground currents
                        if (!ADiakoptics | (ActorID == 1))
                            BusV = with1->NodeV[Nref];
                        else // In the context of actor 1
                            BusV = with1->VoltInActor1(Nref);
                        if ((BusV.re != 0.0) || (BusV.im != 0.0))
                            /*Current  INTO the system network*/
                            switch (SolveType)
                            {
                            case NEWTONSOLVE:
                                caccum(with1->Currents[Nref], cnegate(conjg(cdiv(GenVA, BusV))));
                                break;  // Terminal Current

                            case NORMALSOLVE:
                                caccum(with1->Currents[Nref], conjg(cdiv(GenVA, BusV)));
                                break;   // Injection Current
                            }
                    }
                }
            }
        }
        break;
        case CAPADD:
            /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
        {
            auto with0 = ActiveCircuit[ActorID];
            auto with1 = ActiveCircuit[ActorID]->Solution;
            {

                /*For buses with voltage <> 0, add into aux current array*/
                for (int stop = Phases, i = 1; i <= stop; i++)
                {
                    Nref = with0->Buses[BusIndex - 1]->GetRef(i);
                    if (Nref > 0)
                    {
                        if (!ADiakoptics || (ActorID == 1))
                            BusV = with1->NodeV[Nref];
                        else // In the context of actor 1
                            BusV = with1->VoltInActor1(Nref);
                        if ((BusV.re != 0.0) || (BusV.im != 0.0))
                            /*Current  INTO the system network*/
                            switch (SolveType)
                            {
                            case NEWTONSOLVE:
                                caccum(with1->Currents[Nref], cmul(cmplx(0.0, Ycap), BusV));
                                break; // Terminal Current

                            case NORMALSOLVE:
                                caccum(with1->Currents[Nref], cmul(cmplx(0.0, -Ycap), BusV));
                                break; // Injection Current
                            }  // Constant Y model
                    }
                }
            }
        }
        break;
        } /*CASE*/
    }


    void TAutoAdd::ComputekWLosses_EEN(int ActorID)
    {
        TEnergyMeterObj* pMeter;
        if (ActiveCircuit[ActorID]->EnergyMeters.get_myNumList() == 0)
        {

            // No energymeters in circuit
            // Just go by total system losses
            kWLosses = ActiveCircuit[ActorID]->Get_Losses(ActorID).re * 0.001;
            kWEEN = 0.0;
        }
        else
        {   // Sum losses in energy meters and add EEN
            kWLosses = 0.0;
            kWEEN = 0.0;
            /*# with ActiveCircuit[ActorID] do */
            {
                auto with0 = ActiveCircuit[ActorID];
                {
                    pMeter = (TEnergyMeterObj*) with0->EnergyMeters.Get_First();
                    while (pMeter != NULL)
                    {
                        kWLosses = kWLosses + SumSelectedRegisters(*pMeter, with0->LossRegs, with0->NumLossRegs);
                        kWEEN = kWEEN + SumSelectedRegisters(*pMeter, with0->UEregs, with0->NumUEregs);
                        pMeter = (TEnergyMeterObj*) with0->EnergyMeters.Get_Next();
                    }
                }
            }
        }
    }

    void TAutoAdd::SetBaseLosses()
    {
        ComputekWLosses_EEN(ActiveActor);
        BaseLosses = kWLosses;
        BaseEEN = kWEEN;
    }

}  // namespace AutoAdd







