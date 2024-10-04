

#pragma hdrstop

#include "YMatrix.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "CktElement.h"
#include "Utilities.h"
#include "klusolve.h"



namespace YMatrix
{
    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ReCalcAllYPrims(int ActorID)
    {
        TDSSCktElement* pElem;
        /*# with ActiveCircuit[ActorID] do */
        {
            TDSSCircuit* with0 = ActiveCircuit[ActorID];
            {
                if (with0->LogEvents)
                    LogThisEvent("Recalc All Yprims", ActorID);
                pElem = (TDSSCktElement*) with0->CktElements.Get_First();
                while (pElem != NULL)
                {
                    pElem->CalcYPrim(ActorID);
                    pElem = (TDSSCktElement*)with0->CktElements.Get_Next();
                }
            }
        }
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ReCalcInvalidYPrims(int ActorID)
        /*Recalc YPrims only for those circuit elements that have had changes since last
         solution*/
    {
        TDSSCktElement* pElem;
        /*# with ActiveCircuit[ActorID] do */
        {
            TDSSCircuit* with0 = ActiveCircuit[ActorID];
            {
                if (with0->LogEvents)
                    LogThisEvent("Recalc Invalid Yprims", ActorID);
                pElem = (TDSSCktElement*)with0->CktElements.Get_First();
                while (pElem != NULL)
                {
                    /*# with pElem do */
                    if (pElem->Get_YprimInvalid(ActorID,false))
                        pElem->CalcYPrim(ActorID);
                    pElem = (TDSSCktElement*)with0->CktElements.Get_Next();
                }
            }
        }
    }

    //= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =



    void ResetSparseMatrix(klusparseset_t* hY, int Size, int ActorID)
    {
        if (*hY != 0)
        {
            if  /*Get rid of existing one beFore making a new one*/ (DeleteSparseSet(*hY) < 1)
                DoSimpleMsg("Error Deleting System Y Matrix in ResetSparseMatrix. Problem with Sparse matrix solver.", 50001);
            *hY = 0;
        }

        // Make a new sparse set
        *hY = NewSparseSet(Size);
        if (! *hY)
        {   // Raise and exception
            DoSimpleMsg("Error Creating System Y Matrix. Problem WITH Sparse matrix solver.", 50000);
        }
    }


    void InitializeNodeVbase(int ActorID)
    {
        int i = 0;
        /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
        {
            TDSSCircuit* with0 = ActiveCircuit[ActorID];
            {
                for (int stop = with0->NumNodes, i = 1; i <= stop; i++)
                    /*# with MapNodeToBus^[i] do */
                {
                    TNodeBus with1 = with0->MapNodeToBus[i - 1];
                    {
                        with0->Solution->NodeVbase[i] = with0->Buses[with1.BusRef - 1]->kVBase * 1000.0;
                    }
                }
                with0->Solution->VoltageBaseChanged = false;
            }
        }
    }
    //=====================================================================================================================================
    void AddPDEShunt(int ActorID)
    /* Adds capacitors and rectors connected in shunt to the active Y Bus Matrix (expected to be series)
       This routine was created especifically to fill the needs of the NCIM solution algorithm */
    {
                
    }
    //=====================================================================================================================================
    void BuildYMatrix(int BuildOption, bool AllocateVI, int ActorID)

        /*Builds designated Y matrix for system and allocates solution arrays*/
    {
        int YMatrixsize = 0;
        //   CmatArray    :pComplexArray;   Replaced with a global array for thread safe operation

        TDSSCktElement* pElem;

        //{****} FTrace: TextFile;

       //{****} AssignFile(Ftrace, 'YmatrixTrace.txt');
       //{****} Rewrite(FTrace);
       //{****} IOResultToException();
        ActiveYPrim[ActorID] = pComplexArray();  //Replaces the previous local declaration CmatArray := Nil; for thread safe
        ActiveYPrim[ActorID] = NULL;
         // new function to log KLUSolve.DLL function calls
         // SetLogFile ('KLU_Log.txt', 1);
        /*# with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do */
        {
            auto with0 = ActiveCircuit[ActorID];
            {
                if (with0->Solution->PreserveNodeVoltages)
                    with0->Solution->UpdateVBus(ActorID); // Update voltage values stored with Bus object

                 // the following re counts the number of buses and resets meter zones and feeders
                 // If radial but systemNodeMap not set then init for radial got skipped due to script sequence
                if (with0->get_FBusNameRedefined())
                    with0->ReProcessBusDefs(ActorID);      // This changes the node references into the system Y matrix!!
                YMatrixsize = with0->NumNodes;
                if (AllocateVI)
                {
                    if (with0->LogEvents)
                        LogThisEvent("ReAllocating Solution Arrays", ActorID);
                    with0->Solution->NodeV.resize(with0->NumNodes + 1); // Allocate System Voltage array - allow for zero element
                    (with0->Solution->NodeV)[0] = CZero;
                    with0->Solution->Currents.resize(with0->NumNodes + 1); // Allocate System current array
                    with0->Solution->AuxCurrents = (pNodeVarray)realloc(with0->Solution->AuxCurrents, sizeof(complex) * (with0->NumNodes + 1)); // Allocate System current array

                    /*A-Diakoptics vectors memory allocation*/
                    with0->Solution->Node_dV.resize(with0->NumNodes + 1); // Allocate the partial solution voltage
                    with0->Solution->Ic_Local.resize(with0->NumNodes + 1); // Allocate the Complementary currents
                }
                switch (BuildOption)
                {
                case WHOLEMATRIX:
                {
                    ResetSparseMatrix(&with0->Solution->hYsystem, YMatrixsize, ActorID);
                    with0->Solution->hY = with0->Solution->hYsystem;
                }
                break;
                case SERIESONLY:
                {
                    ResetSparseMatrix(&with0->Solution->hYseries, YMatrixsize, ActorID);
                    with0->Solution->hY = with0->Solution->hYseries;
                }
                break;
                case PDE_ONLY:
                {
                    ResetSparseMatrix(&with0->Solution->hYseries, YMatrixsize, ActorID);
                    with0->Solution->hY = with0->Solution->hYseries;
                }
                break;
                }
                // tune up the Yprims if necessary
                if (with0->Solution->FrequencyChanged)
                    ReCalcAllYPrims(ActorID);
                else
                    ReCalcInvalidYPrims(ActorID);
                if (SolutionAbort)
                {
                    DoSimpleMsg("Y matrix build aborted due to error in primitive Y calculations.", 11001);
                    return;  // Some problem occured building Yprims
                }
                with0->Solution->FrequencyChanged = false;
                if (with0->LogEvents)
                    switch (BuildOption)
                    {
                    case WHOLEMATRIX:
                        LogThisEvent("Building Whole Y Matrix", ActorID);
                        break;
                    case SERIESONLY:
                        LogThisEvent("Building Series Y Matrix", ActorID);
                        break;
                    case PDE_ONLY:
                        LogThisEvent("Building PDE only Y Matrix", ActorID);
                        break;
                    }
                // Add in Yprims for all devices
                pElem = (TDSSCktElement*) with0->CktElements.Get_First();
                while (pElem != NULL)
                {
                    /*# with pElem do */
                    if (pElem->Get_Enabled())
                    {          // Add stuff only if enabled
                        switch (BuildOption)
                        {
                            case PDE_ONLY:
                            {
                                // First check if the element is PDE or source
                                bool ValidElm = ((pElem->ParentClass->DSSClassType & BaseClassMask) == PD_ELEMENT);
                                // If not PDE, check if it is a VSource
                                ValidElm = ValidElm || ((pElem->DSSObjType & CLASSMASK) == SOURCE);

                                if (ValidElm)
                                    ActiveYPrim[ActorID] = pElem->GetYPrimValues(ALL_YPRIM);
                                else
                                    ActiveYPrim[ActorID] = NULL;
                            }
                            break;
                            case SERIESONLY:
                                ActiveYPrim[ActorID] = pElem->GetYPrimValues(SERIES);
                                break;
                            default:   // Whole matrix
                                ActiveYPrim[ActorID] = pElem->GetYPrimValues(ALL_YPRIM);
                                break;
                        }
                        // new function adding primitive Y matrix to KLU system Y matrix
                        if (ActiveYPrim[ActorID] != NULL)
                        {
                            if ( AddPrimitiveMatrix( with0->Solution->hY, pElem->Yorder, &( pElem->NodeRef[0] ), &( ActiveYPrim[ActorID][0] ) ) < 1)
                                DoSimpleMsg("Node index out of range adding to System Y Matrix", 50002);
                        }
                    }   // If Enabled
                    pElem = (TDSSCktElement*) with0->CktElements.Get_Next();
                }
                //{****} CloseFile(Ftrace);
                //{****} FireOffEditor(  'YmatrixTrace.txt');

                // Allocate voltage and current vectors if requested
                if (AllocateVI)
                {
                    if (!with0->Solution->VmagSaved.empty())    with0->Solution->VmagSaved.clear();
                    if (!with0->Solution->ErrorSaved.empty())   with0->Solution->ErrorSaved.clear();
                    if (!with0->Solution->NodeVbase.empty())    with0->Solution->NodeVbase.clear();
                    if (!with0->Solution->NodeYii.empty())      with0->Solution->NodeYii.clear();             /*by Dahei -> UCF*/

                    with0->Solution->VmagSaved.resize(with0->NumNodes + 1);

                    with0->Solution->ErrorSaved.resize(with0->NumNodes + 1);

                    with0->Solution->NodeVbase.resize(with0->NumNodes + 1);

                    with0->Solution->NodeYii.resize(with0->NumNodes + 1);  // zero fill //Bii  {by Dahei -> UCF}
                    with0->Solution->NodeYiiEmpty = true;                                                          /*by Dahei -> UCF*/
                    InitializeNodeVbase(ActorID);
                }
                switch (BuildOption)
                {
                    case WHOLEMATRIX:
                    {
                        with0->Solution->SeriesYInvalid = true;  // Indicate that the Series matrix may not match
                        with0->Solution->SystemYChanged = false;
                    }
                    break;
                    case SERIESONLY:
                        with0->Solution->SeriesYInvalid = false;
                    break;  // SystemYChange unchanged
                    case PDE_ONLY:
                    {
                        with0->Solution->SeriesYInvalid = true; // Indicate that the Series matrix may not match
                        with0->Solution->SystemYChanged = false;
                    }
                    break;

                }
                // Deleted RCD only done now on mode change
                // SolutionInitialized := False;  //Require initialization of voltages if Y changed
                if (with0->Solution->PreserveNodeVoltages)
                    with0->Solution->RestoreNodeVfromVbus();
            }
        }
    }

    // leave the call to GetMatrixElement, but add more diagnostics



    String CheckYMatrixforZeroes(int ActorID)
    {
        String result;
        unsigned int i = 0;
        complex c;
        klusparseset_t hY;
        unsigned int sCol = 0;
        unsigned int nIslands = 0, iCount = 0, iFirst = 0, p = 0;
        vector < unsigned int > Cliques;
        result = "";
        /*# with ActiveCircuit[ActorID] do */
        {
            TDSSCircuit* with0 = ActiveCircuit[ActorID];
            {
                hY = with0->Solution->hY;
                for (int stop = with0->NumNodes, i = 1; i <= stop; i++)
                {
                    GetMatrixElement(hY, i, i, (pcomplex) &c);
                    if (cabs(c) == 0.0)
                        /*# with MapNodeToBus^[i] do */
                    {
                        TNodeBus with1 = with0->MapNodeToBus[i - 1];
                        {
                            result = result + Format("%sZero diagonal for bus %s, node %d", CRLF.c_str(), with0->BusList.Get(with1.BusRef).c_str(), with1.NodeNum);
                        }
                    }
                }

                // new diagnostics
                GetSingularCol(hY, &sCol); // returns a 1-based node number
                if (sCol > 0)
                    /*# with MapNodeToBus^[sCol] do */
                {
                    TNodeBus with1 = with0->MapNodeToBus[sCol - 1];
                    {
                        result = result + Format("%sMatrix singularity at bus %s, node %d", CRLF.c_str(), with0->BusList.Get(with1.BusRef).c_str(), sCol);
                    }
                }
                Cliques.resize(with0->NumNodes);
                nIslands = FindIslands(hY, with0->NumNodes, &Cliques[0]);
                if (nIslands > 1)
                {
                    result = result + Format("%sFound %d electrical islands:", CRLF.c_str(), nIslands);
                    for (int stop = nIslands, i = 1; i <= stop; i++)
                    {
                        iCount = 0;
                        iFirst = 0;
                        for (int stop = with0->NumNodes - 1, p = 0; p <= stop; p++)
                        {
                            if (Cliques[p] == i)
                            {
                                iCount += 1;
                                if (iFirst == 0)
                                    iFirst = p + 1;
                            }
                        }
                        /*# with MapNodeToBus^[iFirst] do */
                        {
                            TNodeBus with1 = with0->MapNodeToBus[iFirst - 1];
                            {
                                result = result + Format("%s  #%d has %d nodes, including bus %s (node %d)", CRLF.c_str(), i, iCount, with0->BusList.Get(with1.BusRef).c_str(), iFirst);
                            }
                        }
                    }
                }
            }
        }
        return result;
    }

} // namespace YMatrix








