
#pragma hdrstop

#include "MeterElement.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"


using namespace std;
using namespace Arraydef;
using namespace CktElement;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace System;
using namespace Ucomplex;

namespace MeterElement
{

TMeterElement::TMeterElement(String ClassName) : inherited(ClassName) {}
TMeterElement::TMeterElement() {}



void TMeterElement::AllocateSensorArrays()
{
	if(ASSIGNED(MeteredElement))
		CalculatedCurrent.resize(MeteredElement->Yorder);
	if(ASSIGNED(MeteredElement))
		CalculatedVoltage.resize(MeteredElement->Yorder);
	SensorCurrent.resize(Fnphases);
	SensorVoltage.resize(Fnphases);
	PhsAllocationFactor.resize(Fnphases);
}

void TMeterElement::CalcAllocationFactors(int ActorID)
{
	int		IOffset = 0,
			i = 0,
			stop = 0;
	double	mag = 0.0;

	MeteredElement->GetCurrents(&CalculatedCurrent[0], ActorID);

    // The Phase Allocation Factor is the amount that the load must change to match the measured peak
	IOffset = (MeteredTerminal - 1) * MeteredElement->Get_NConds();
	AvgAllocFactor = 0.0;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		mag = cabs(CalculatedCurrent[i + IOffset - 1]);
		if(mag > 0.0)
			PhsAllocationFactor[i - 1] = SensorCurrent[i - 1] / mag;
		else
			PhsAllocationFactor[i - 1] = 1.0; // No change
		AvgAllocFactor = AvgAllocFactor + PhsAllocationFactor[i - 1];
	}
	AvgAllocFactor = AvgAllocFactor / Fnphases;   // Factor for 2- and 3-phase loads
}

TMeterElement::TMeterElement(TDSSClass* ParClass)
 : inherited(ParClass),
			ElementName(""),
			MeteredElement(nullptr),
			MeteredTerminal(1),
			MeteredElementChanged(false),
			AvgAllocFactor(0.0)
{
	DSSObjType = METER_ELEMENT;
	SensorCurrent.clear();
	SensorVoltage.clear();
	PhsAllocationFactor.clear();
	CalculatedCurrent.clear();
	CalculatedVoltage.clear();
}

TMeterElement::~TMeterElement()
{
	if(!SensorCurrent.empty())
		SensorCurrent.clear();
	if(!SensorVoltage.empty())
		SensorVoltage.clear();
	if(!CalculatedCurrent.empty())
		CalculatedCurrent.clear();
	if(!CalculatedVoltage.empty())
		CalculatedVoltage.clear();
	if(!PhsAllocationFactor.empty())
		PhsAllocationFactor.clear();
	// inherited::Destroy();
}


void TMeterElement::TakeSample(int ActorID)
{

  // virtual function - should be overridden
	DoSimpleMsg(String("Programming Error:  Reached base Meterelement class for TakeSample.") + CRLF
	           + "Device: "
	           + get_Name(), 723);
}

}  // namespace MeterElement

