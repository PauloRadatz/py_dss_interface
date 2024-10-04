

#pragma hdrstop

#include "ControlQueue.h"



#include "DSSGlobals.h"
#include "Sysutils.h"
#include "Utilities.h"
#include "System.h"
#include <string>
#include <algorithm>

/* TControlQueue */

namespace ControlQueue
{

    int TControlQueue::Push(const int Hour, const double Sec, const int Code, const int ProxyHdl, TControlElem* Owner, int ActorID)
    {
        int result = 0;
        result = Push(Hour, Sec, (EControlAction) Code, ProxyHdl, Owner, ActorID);
        return result;
    }


    int TControlQueue::Push(const int Hour, const double Sec, const EControlAction Code, const int ProxyHdl, TControlElem* Owner, int ActorID)

        /*Add a control action to the queue, sorted by lowest time first*/
        /*Returns handle to the action*/
    {
        int result = 0;
        int i = 0, Hr = 0;
        double ThisActionTime = 0.0, S = 0.0;
        TTimeRec Trec;
        pActionRecord pAction = NULL;
        bool ActionInserted = false;
        ctrlHandle++; // just a serial number

           /*Normalize the time */
        Hr = Hour;
        S = Sec;
        if (S > 3600.0){
            do
            {
                Hr = Hr + 1;
                S = S - 3600.0;
            } while (!(S < 3600.0));
		}
        Trec.Hour = Hr;
        Trec.Sec = S;
        ThisActionTime = TimeRecToTime(Trec);
        pAction = new TActionRecord;  // Make a new Action

               /*Insert the action in the list in order of time*/
            ActionInserted = false;
            for (int stop = ActionList.size() - 1, i = 0; i <= stop; i++)
            {
                if (ThisActionTime <= TimeRecToTime(((pActionRecord)ActionList[i])->ActionTime))
                {
                    TList::iterator it;
                    it = ActionList.begin();
                    it = ActionList.insert(it + i, pAction);
                    ActionInserted = true;
                    break;
                }
            }
            if (!ActionInserted)
                ActionList.push_back(pAction);
            /*# with pAction^ do */
            {
                pAction->ActionTime = Trec;
                pAction->ActionCode = Code;
                pAction->ActionHandle = ctrlHandle;
                pAction->ProxyHandle = ProxyHdl;
                pAction->ControlElement = Owner;
            }
            result = ctrlHandle;
            if (DebugTrace)
                WriteTraceRecord(((TDSSCktElement*)Owner)->get_Name(), Code, Owner->DblTraceParameter, Format("Handle %d Pushed onto Stack", ctrlHandle), ActorID);
            return result;
    }

    void TControlQueue::Clear()
    {
        int i = 0;
        /*# with ActionList do */  /*Free Allocated memory*/
        for (int stop = ActionList.size() - 1, i = 0; i <= stop; i++)
            free(ActionList[i]);
        ActionList.clear();
    }


    TControlQueue::TControlQueue()
        : ActionList(TList()),
        DebugTrace(false),
        ctrlHandle(0)
    {
        // inherited::Create();
        ActionList.clear();
        ctrlHandle = 0;
        DebugTrace = false;
    }


    TControlQueue::~TControlQueue()
    {
        Clear();
        ActionList.clear();
        // todo check:  inherited::Destroy;
    }


    void TControlQueue::DoAllActions(int ActorID)
    {
        int i = 0;
        /*# with ActionList do */
        for (int stop = ActionList.size() - 1, i = 0; i <= stop; i++)
            /*# with pActionRecord(Items[i])^ do */
        {
            TActionRecord* with0 = ((pActionRecord)ActionList[i]);
            with0->ControlElement->DoPendingAction(with0->ActionCode, with0->ProxyHandle, ActorID);
        }
        ActionList.clear();
    }


    bool TControlQueue::DoNearestActions(int& Hour, double& Sec, int ActorID)

        // Do only those actions with the same delay time as the first action time
        // Return time

    {
        bool result = false;
        TControlElem* pElem;
        TTimeRec t;
        int Code = 0, Hdl = 0, ProxyHdl = 0;
        result = false;
        /*# with ActionList do */
        if (ActionList.size() > 0)
        {
            t = ((pActionRecord)ActionList[0])->ActionTime;
            Hour = t.Hour;
            Sec = t.Sec;
            pElem = Pop(t, Code, ProxyHdl, Hdl, ActorID);
            while (pElem != NULL)
            {
                if (DebugTrace)
                    WriteTraceRecord(pElem->get_Name(), Code, pElem->DblTraceParameter, Format("Pop Handle %d Do Nearest Action", Hdl), ActorID);
                pElem->DoPendingAction(Code, ProxyHdl, ActorID);
                result = true;
                pElem = Pop(t, Code, ProxyHdl, Hdl, ActorID);
            }
        }
        return result;
    }


    bool TControlQueue::IsEmpty()
    {
        bool result = false;
        if (ActionList.empty())
            result = true;
        else
            result = false;
        return result;
    }


    TControlElem* TControlQueue::Pop(const TTimeRec& ActionTime, int& Code, int& ProxyHdl, int& Hdl, int ActorID)
        // pop off next control action with an action time <= ActionTime (sec)

    {
        TControlElem* result;
        int i = 0;
        double t = 0.0;
        result = NULL;
        t = TimeRecToTime(ActionTime);
        /*# with ActionList do */
        for (int stop = ActionList.size() - 1, i = 0; i <= stop; i++)
        {
            /*# with pActionRecord(Items[i])^ do */
            {
                TActionRecord* with0 = ((pActionRecord)ActionList[i]);
                if (TimeRecToTime(with0->ActionTime) <= t)
                {
                    result = with0->ControlElement;
                    Code = with0->ActionCode;
                    ProxyHdl = with0->ProxyHandle;
                    Hdl = with0->ActionHandle;
                    DeleteFromQueue(i, true, ActorID);
                    break;
                }
            }
        }
        return result;
    }

    TControlElem* TControlQueue::Pop_Time(const TTimeRec& ActionTime, int& Code, int& ProxyHdl, int& Hdl, double& ATime, bool KeepIn, int ActorID)  // Pop action from queue <= given time
     // pop off next control action with an action time <= ActionTime (sec)

    {
        TControlElem* result;
        int i = 0;
        double t = 0.0;
        result = NULL;
        t = TimeRecToTime(ActionTime);
        /*# with ActionList do */
        for (int stop = ActionList.size() - 1, i = 0; i <= stop; i++)
        {
            /*# with pActionRecord(Items[i])^ do */
            {
                TActionRecord* with0 = ((pActionRecord)ActionList[i]);
                if (TimeRecToTime(with0->ActionTime) <= t)
                {
                    result = with0->ControlElement;
                    Code = with0->ActionCode;
                    ProxyHdl = with0->ProxyHandle;
                    Hdl = with0->ActionHandle;
                    ATime = TimeRecToTime(with0->ActionTime);
                    if (!KeepIn)
                        DeleteFromQueue(i, true, ActorID);
                    break;
                }
            }
        }
        return result;
    }


    void TControlQueue::DeleteFromQueue(int i, bool popped, int ActorID)
        // Delete i-th element from the Queue

    {
        TControlElem* pElem;
        String S;
        /*# with pActionRecord(ActionList.Items[i])^ do */
        {
            TActionRecord* with0 = ((pActionRecord)ActionList[i]);
            {
                pElem = with0->ControlElement;
                if (DebugTrace)
                {
                    if (popped)
                        S = "by Pop function";
                    else
                        S = "by control device";
                    WriteTraceRecord(pElem->get_Name(), with0->ActionCode, pElem->DblTraceParameter, Format("Handle %d deleted from Queue ", with0->ActionHandle) + S, ActorID);
                }
            }
        }
        delete static_cast<TActionRecord*>(ActionList[i]);
        TList::iterator it2;
        it2 = ActionList.begin();
        it2 = ActionList.erase(it2 + i);
    }


    bool TControlQueue::DoActions(const int Hour, const double Sec, int ActorID)

        // Do all actions having an action time <= t

    {
        bool result = false;
        TControlElem* pElem;
        TTimeRec t;
        int Code = 0, Hdl = 0, ProxyHdl = 0;
        result = false;
        if (ActionList.size() > 0)
        {
            t.Hour = Hour;
            t.Sec = Sec;
            pElem = Pop(t, Code, ProxyHdl, Hdl, ActorID);
            while (pElem != NULL)
            {
                if (DebugTrace)
                    WriteTraceRecord(pElem->get_Name(), Code, pElem->DblTraceParameter, Format("Pop Handle %d Do Action", Hdl), ActorID);
                pElem->DoPendingAction(Code, ProxyHdl, ActorID);
                result = true;
                pElem = Pop(t, Code, ProxyHdl, Hdl, ActorID);
            }
        }
        return result;
    }

    //-----------------------------------------------------------------------------------------------------

    bool TControlQueue::get_DebugTrace()
    {
        return DebugTrace;
    }

    //-----------------------------------------------------------------------------------------------------

    bool TControlQueue::DoMultiRate(const int Hour, const double Sec, int ActorID)

        // Do all actions having an action time <= t and solves the circuit after each control action

    {
        bool result = false;
        TControlElem* pElem;
        int Code = 0, Hdl = 0, ProxyHdl = 0, idx = 0;
        result = false;
        for (int stop = 1, idx = 0; idx <= stop; idx++)
            Temp_Int[idx] = 0;    // Temporary register for hour
        for (int stop = 3, idx = 0; idx <= stop; idx++)
            Temp_dbl[idx] = 0.0;
        /*  Temp_dbl[0]  Temporary register for the secs
           Temp_dbl[1]  Temporary register for Time accumulator
           Temp_dbl[2]  Temporary register for Time upper boundary
           Temp_dbl[3]  Temporary register for the control action time */
        if (ActionList.size() > 0)
        {
            Ltimer.Hour = Hour;
            Ltimer.Sec = Sec;
            Temp_dbl[4] = ActiveCircuit[ActorID]->Solution->DynaVars.h;                        // Simulation step time (Time window size)
            Temp_dbl[6] = TimeRecToTime(Ltimer);                                    // Simulation step time incremental
            pElem = Pop_Time(Ltimer, Code, ProxyHdl, Hdl, Temp_dbl[3], false, ActorID);
            while (pElem != NULL)
            {
                if (DebugTrace)
                    WriteTraceRecord(pElem->get_Name(), Code, pElem->DblTraceParameter, Format("Pop Handle %d Do Action", Hdl), ActorID);
                pElem->DoPendingAction(Code, ProxyHdl, ActorID);
                result = true;
                pElem = Pop_Time(Ltimer, Code, ProxyHdl, Hdl, Temp_dbl[3], false, ActorID);
            }
            //**************After this point, the additional control actions are performed************
            Temp_dbl[7] = ActiveCircuit[ActorID]->Solution->DynaVars.T;                        // Saving the current time (secs)
            /*# with ActiveCircuit[ActorID]->Solution->DynaVars do */
            auto& with0 = ActiveCircuit[ActorID]->Solution->DynaVars;
            Temp_Int[2] = with0.intHour;          // Saving the current time (hour)
            Temp_dbl[2] = Temp_dbl[6];
            //*************** Simulation time is recalculated considering the next control action event ************
            Recalc_Time_Step(ActorID);
            pElem = Pop_Time(Ltimer, Code, ProxyHdl, Hdl, Temp_dbl[3], true, ActorID);         // Downloads the next CtrlAction without
            while (pElem != NULL)                                                      // removing it from the Queue
            {
                while (Temp_dbl[3] >= 3600.0)
                    Temp_dbl[3] = Temp_dbl[3] - 3600.0;    // CtrlAction Time is adjusted
                Temp_dbl[5] = (Temp_dbl[3] - Temp_dbl[6]) + Temp_dbl[1];              // Recalculates the CtrlAction occurrence time
                if (Temp_dbl[5] < Temp_dbl[4])                                       // Checks if the CtrlAction is within the
                {                                                                   // time window
                    pElem = Pop_Time(Ltimer, Code, ProxyHdl, Hdl, Temp_dbl[3], false, ActorID);  // Removes the CtrlAction from The Queue
                    if (DebugTrace)
                        WriteTraceRecord(pElem->get_Name(), Code, pElem->DblTraceParameter, Format("Pop Handle %d Do Action", Hdl), ActorID);
                    pElem->DoPendingAction(Code, ProxyHdl, ActorID);
                    pElem = Pop_Time(Ltimer, Code, ProxyHdl, Hdl, Temp_dbl[3], true, ActorID);   // Downloads the next CtrlAction without
                }                                                                     // removing it from the Queue
                else
                {
                    pElem->DoPendingAction(Code, ProxyHdl, ActorID);                               // Executes the CtrlAction
                    pElem = NULL;                                                  // The next CtrlAction is outside the time window
                    Temp_Int[1] = 1;                                                    // Preparing everything to exit
                }
                if ((pElem == NULL) && (Temp_Int[1] == 0))                             // The last CtrlAction was within the time
                {                                                                   // Time window, keep scanning
                  /*# with ActiveCircuit[ActorID]->Solution do */
                    auto with1 = ActiveCircuit[ActorID]->Solution;
                    {
                        Temp_dbl[1] = Temp_dbl[1] + (Temp_dbl[3] - Temp_dbl[6]);         // The Accumulated time is calculated
                        Temp_dbl[6] = Temp_dbl[6] + Temp_dbl[4];                         // Time reference moves forward
                        while (Temp_dbl[6] >= 3600.0)
                            Temp_dbl[6] = Temp_dbl[6] - 3600.0;// Time reference is adjusted
                //******************** Updates the circuit after applying the control actions **************************
                        with1->SolveCircuit(ActorID);
                        Restore_Time_Step(ActorID);                                                  // Restores Time for sampling devices
                        with1->SampleControlDevices(ActorID);
                        Recalc_Time_Step(ActorID);                                                   // Recalculating Time for next iteration
                        pElem = Pop_Time(Ltimer, Code, ProxyHdl, Hdl, Temp_dbl[3], true, ActorID);  // Downloads the next CtrlAction without
                    }                                                                  // removing it from the Queue
                }
            }
            Restore_Time_Step(ActorID);                                                         // Restores Time to keep going with the simulation
        }
        return result;
    }


    void TControlQueue::Recalc_Time_Step(int ActorID)
    {
        Temp_dbl[2] = Temp_dbl[2] + Temp_dbl[4];                                     // Time window moves forward
        while (Temp_dbl[2] >= 3600.0)                                                  // Adjusts the window
        {
            Temp_Int[0]++;
            Temp_dbl[2] = Temp_dbl[2] - 3600.0;
        }
        Ltimer.Hour = Temp_Int[0];
        Ltimer.Sec = Temp_dbl[2];
        /*# with ActiveCircuit[ActorID]->Solution->DynaVars do */
        auto& with0 = ActiveCircuit[ActorID]->Solution->DynaVars;
        with0.intHour = Temp_Int[0];               // Sets the simulation time
        ActiveCircuit[ActorID]->Solution->DynaVars.T = Temp_dbl[2];
        ActiveCircuit[ActorID]->Solution->Update_dblHour();
    }


    void TControlQueue::Restore_Time_Step(int ActorID)
    {
        /*# with ActiveCircuit[ActorID]->Solution->DynaVars do */
        auto& with0 = ActiveCircuit[ActorID]->Solution->DynaVars;
        with0.intHour = Temp_Int[2];
        ActiveCircuit[ActorID]->Solution->DynaVars.T = Temp_dbl[7];
        ActiveCircuit[ActorID]->Solution->Update_dblHour();
    }

    double TControlQueue::TimeRecToTime(TTimeRec Trec)
    {
        double result = 0.0;
        /*# with Trec do */
        result = Trec.Hour * 3600.0 + Trec.Sec;
        return result;
    }


    void TControlQueue::Set_Trace( bool Value)
    {
        DebugTrace = Value;
        if (DebugTrace)
        {
            AssignFile(Tracefile, GetOutputDirectory() + "Trace_ControlQueue.CSV");
            Rewrite(Tracefile);
            IOResultToException();
            WriteLn(Tracefile, "\"Hour\", \"sec\", \"Control Iteration\", \"Element\", \"Action Code\", \"Trace Parameter\", \"Description\"");
            CloseFile(Tracefile);
        }
    }


    void TControlQueue::ShowQueue(const String Filenm)
    {
        TTextRec F;
        int i = 0;
        pActionRecord pAction = NULL;
        try
        {
            AssignFile(F, Filenm);
            Rewrite(F);
            IOResultToException();
            WriteLn(F, "Handle, Hour, Sec, ActionCode, ProxyDevRef, Device");
            for (int stop = ActionList.size() - 1, i = 0; i <= stop; i++)
            {
                pAction = (pActionRecord) ActionList[i];
                if (pAction != NULL)
                    /*# with pAction^ do */
                {
                    WriteLn(F, Format("%d, %d, %-1.0f, %d, %d, ", 
                        pAction->ActionHandle, 
                            pAction->ActionTime.Hour, 
                            pAction->ActionTime.Sec, 
                            pAction->ActionCode, 
                            pAction->ProxyHandle) +
                        pAction->ControlElement->get_Name() + " ");
                }
            }
/* }
        __finally
        {*/
            CloseFile(F);
            if (AutoDisplayShowReport)
                FireOffEditor(Filenm);
        }
        catch (...)
        {
            //
        }
    }


    void TControlQueue::WriteTraceRecord(const String ElementName, const int Code, double TraceParameter, const String S, int ActorID)
    {
        try
        {
            if (!InShowResults)
            {
                Append(Tracefile);
                IOResultToException();
                WriteLn(Tracefile, Format("%d, %.6g, %d, ", 
                    ActiveCircuit[ActiveActor]->Solution->DynaVars.intHour, 
                        ActiveCircuit[ActiveActor]->Solution->DynaVars.T, 
                        ActiveCircuit[ActiveActor]->Solution->ControlIteration) + 
                        ElementName +
                        Format(" % d, % -.g, % s", Code, TraceParameter) +                        
                        S);
                CloseFile(Tracefile);
            }
        }
        catch (std::exception &E)
        {
            {
            }
        }
    }


    void TControlQueue::Delete(int Hdl, int ActorID)

        /*Delete an item by its Handle reference*/
    {
        int i = 0;
        /*# with ActionList do */
        for (int stop = ActionList.size() - 1, i = 0; i <= stop; i++)
        {
            if (((pActionRecord)ActionList[i])->ActionHandle == Hdl)
            {
                DeleteFromQueue(i, false, ActorID);
                return;
            }
        }
    }


    int TControlQueue::Get_QueueSize()
    {
        int result = 0;
        result = ActionList.size();
        return result;
    }


    String TControlQueue::QueueItem(int Qidx)
    {
        String result;
        pActionRecord pAction = NULL;
        pAction = (pActionRecord) ActionList[Qidx];
        if (pAction != NULL)
            /*# with pAction^ do */
        {
            result = Format("%d, %d, %f, %d, %d, ", 
                pAction->ActionHandle, 
                    pAction->ActionTime.Hour, 
                    pAction->ActionTime.Sec, 
                    pAction->ActionCode, 
                    pAction->ProxyHandle) + pAction->ControlElement->get_Name() + " ";
        }
        else
            result = "";
        return result;
    }


}// namespace ControlQueue






