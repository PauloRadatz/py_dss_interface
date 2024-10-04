#ifndef BusH
#define BusH
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
 2/4/03 added Zsc and Zsc1, Zsc0 properties
*/


#include "System.h"

#include "Arraydef.h"
#include "Ucomplex.h"
#include "Ucmatrix.h"
#include "NamedObject.h"




//class TDSSBus;
//struct TNodeBus;

namespace Bus
{


    class TDSSBus : public TNamedObject {
        typedef TNamedObject inherited;
    public:
//    private:
        int FNumNodesThisBus;
        std::vector <longInt> Nodes;
        int Allocation;
        std::vector <longInt> RefNo;
        void AddANode();
        complex Get_Zsc0();
        complex Get_Zsc1();
    public:
        vector <complex> VBus, BusCurrent;
        TcMatrix Zsc, Ysc, Zsc012;         // GIS coords
                     // coordinates
                  // Base kV for each node to ground (0)
        double lat, longitude, x, y, kVBase, DistFromMeter;
        bool GISCoordDefined, CoordDefined, BusChecked, Keep, IsRadialBus;  // Flag for general use in bus searches

             // ***** Reliability Variables
        double BusFltRate;  // Accumulated failure rate  downstream from this bus faults per year
        double Bus_Num_Interrupt;  // Number of interruptions this bus per year
        double Bus_Int_Duration; // Avg Annual Interruption duration for this bus
        double BusCustInterrupts; // Accumulated Number of customer interruptions from this bus
        double BusCustDurations; // Accumulated Customer outage durations
        int BusTotalNumCustomers;  // Total Number of customers served from this bus
        double BusTotalMiles;  // Total length of lines downstream from this bus for Duke siting algorithm
        int BusSectionID; // ID of the feeder section this bus belongs to
        TDSSBus();
        virtual ~TDSSBus();
        void AllocateBusQuantities();
        void AllocateBusVoltages();
        void AllocateBusCurrents();
        int Add(int NodeNum, int ActorID);
        int Find(int NodeNum); // Returns reference num for node by node number
        int FindIdx(int NodeNum); // Returns index of node by node number
        int GetRef(int NodeIndex); // Returns reference Num for node by node index
        int GetNum(int NodeIndex); // Returns ith node number designation
        int get_FNumNodesThisBus();

    private:
        // Private copy constructor and assignment, because 
        // copying leads to multiple ownership of VBus and BusCurrent arrays,
        // which leads to double-free when we try to delete them.
        TDSSBus(const TDSSBus&);
        TDSSBus& operator=(const TDSSBus&);
    };

    // Bus Collection



    typedef vector <TDSSBus*> pTBusArray;
    typedef TDSSBus* TBusArray[10/*# range 1..10*/];

    struct TNodeBus {
        int BusRef;   // Ref to Bus in circuit's BusList, 1-based
        int NodeNum;
    };


    typedef vector <TNodeBus> pTNodeBusArray;
    typedef TNodeBus TNodeBusArray[2/*# range 1..2*/];

}   // namespace Bus

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Bus;
#endif

#endif // BusH











