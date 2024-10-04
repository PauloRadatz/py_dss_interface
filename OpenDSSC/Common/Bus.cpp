

#pragma hdrstop

#include "Bus.h"

#include "DSSGlobals.h"

namespace Bus
{

    TDSSBus::TDSSBus()
        : inherited("Bus"),
        FNumNodesThisBus(0),
        Allocation(3),
        lat(0.0),
        longitude(0.0),
        x(0.0),
        y(0.0),
        kVBase(0.0),
        DistFromMeter(0.0),
        GISCoordDefined(false),
        CoordDefined(false),
        BusChecked(false),
        Keep(false),
        IsRadialBus(false),
        BusFltRate(0.0),
        Bus_Num_Interrupt(0.0),
        Bus_Int_Duration(0.0),
        BusCustInterrupts(0.0),
        BusCustDurations(0.0),
        BusTotalNumCustomers(0),
        BusTotalMiles(0.0),
        BusSectionID(0)
    {
        Nodes.resize( Allocation );
        RefNo.resize( Allocation );
        FNumNodesThisBus = 0;
        Ysc = TcMatrix(0);
        Zsc = TcMatrix(0);
        Zsc012 = TcMatrix(0);
        VBus.clear();
        BusCurrent.clear();
        kVBase = 0.0;  // Signify that it has not been set
        x = 0.0;
        y = 0.0;
        DistFromMeter = 0.0;
        BusFltRate = 0.0;  // accummulated failure rate
        Bus_Int_Duration = 0.0;
        BusCustInterrupts = 0.0; // Accumulated Number of customer interruptions from this bus
        BusCustDurations = 0.0; // Accumulated Customer outage durations
        BusTotalNumCustomers = 0;
        BusTotalMiles = 0.0;  // total length of line downstream
        CoordDefined = false;
        Keep = false;
        IsRadialBus = false;
        // GIS data
        lat = 0;
        longitude = 0;
        GISCoordDefined = false;
    }


    TDSSBus::~TDSSBus()
    {
        if (Nodes.size() != 0)
            Nodes.clear();
//        if (RefNo != nullptr)
//            free(RefNo);
/*      if (VBus != NULL)
            free(VBus);
        if (BusCurrent != NULL)
            free(BusCurrent);*/
        // todo check:  inherited::Destroy;

        VBus.clear();
        BusCurrent.clear();
    }

    int TDSSBus::get_FNumNodesThisBus()
    {
        return FNumNodesThisBus;
    }

    void TDSSBus::AddANode()
    {
        FNumNodesThisBus++;
        if (FNumNodesThisBus > Allocation)
        {
            ++Allocation;
            Nodes.resize( Allocation );
            RefNo.resize( Allocation );
        }
    }


    int TDSSBus::Add(int NodeNum, int ActorID)
    {
        int result = 0;
        if (NodeNum == 0)
            result = 0;
        else
        {
            result = Find(NodeNum);
            if (result == 0)
            {
                // Add a node to the bus
                AddANode();
                Nodes[FNumNodesThisBus - 1] = NodeNum;
                /*# with ActiveCircuit[ActorID] do */
                {
                    auto with0 = ActiveCircuit[ActorID];
                    {
                        with0->NumNodes++;  // Global node number for circuit
                        RefNo[FNumNodesThisBus - 1] = with0->NumNodes;
                        result = with0->NumNodes;  // Return global node number
                    }
                }
            }
        }
        return result;
    }


    int TDSSBus::Find(int NodeNum)
        // Returns reference number

    {
        int result = 0;
        int i = 0;
        for (int stop = FNumNodesThisBus, i = 1; i <= stop; i++)
        {
            if (Nodes[i - 1] == NodeNum)
            {
                result = RefNo[i - 1];
                return result;
            }
        }
        result = 0;
        return result;
    }


    int TDSSBus::GetRef(int NodeIndex)
    {
        int result = 0;
        result = 0;
        if ((NodeIndex > 0) && (NodeIndex <= FNumNodesThisBus))
            result = RefNo[NodeIndex - 1];
        return result;
    }


    int TDSSBus::GetNum(int NodeIndex)
    {
        int result = 0;
        result = 0;
        if ((NodeIndex > 0) && (NodeIndex <= FNumNodesThisBus))
            result = Nodes[NodeIndex - 1];
        return result;
    }


    void TDSSBus::AllocateBusQuantities()
        // Have to perform a short circuit study to get this allocated

    {
        Ysc = TcMatrix(FNumNodesThisBus);
        Zsc = TcMatrix(FNumNodesThisBus);
        Zsc012 = TcMatrix(3); //  can only be 3x3  -- 0, 1, 2
        AllocateBusVoltages();
        AllocateBusCurrents();
    }

    complex TDSSBus::Get_Zsc0()
        // = Zs + 2 Zm

    {
        complex result;
        if ((Zsc.Norder != 0))
            result = cadd(Zsc.AvgDiagonal(), cmulreal(Zsc.AvgOffDiagonal(), 2.0));
        else
            result = CZero;
        return result;
    }


    complex TDSSBus::Get_Zsc1()
        // = Zs-Zm

    {
        complex result;
        if ((Zsc.Norder != 0))
            result = csub(Zsc.AvgDiagonal(), Zsc.AvgOffDiagonal());
        else
            result = CZero;
        return result;
    }


    int TDSSBus::FindIdx(int NodeNum)
        // Returns Index

    {
        int result = 0;
        int i = 0;
        for (int stop = FNumNodesThisBus, i = 1; i <= stop; i++)
        {
            if (Nodes[i - 1] == NodeNum)
            {
                result = i;
                return result;
            }
        }
        result = 0;
        return result;
    }

    void TDSSBus::AllocateBusVoltages()
    {
        int i = 0;
        VBus.resize(FNumNodesThisBus + 1);
        for (int stop = FNumNodesThisBus, i = 1; i <= stop; i++)
            VBus[i - 1] = CZero;
    }


    void TDSSBus::AllocateBusCurrents()
    {
        int i = 0;
        BusCurrent.resize(FNumNodesThisBus + 1);
        for (int stop = FNumNodesThisBus, i = 1; i <= stop; i++)
            BusCurrent[i - 1] = CZero;
    }

}










