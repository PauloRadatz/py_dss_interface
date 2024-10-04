#ifndef MeterElementH
#define MeterElementH

#include "System.h"
#include "Sysutils.h"

#include "CktElement.h"
#include "Bus.h"
#include "Ucomplex.h"
#include "DSSClass.h"
#include "Arraydef.h"

namespace MeterElement
{


 /*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TMeterElement : public TDSSCktElement
{
public:
	typedef TDSSCktElement inherited;	
	String ElementName;
	CktElement::TDSSCktElement* MeteredElement;  // Pointer to target circuit element
	int MeteredTerminal;
	bool MeteredElementChanged;
	std::vector <double> SensorCurrent;
	std::vector <double> SensorVoltage;
	std::vector <double> PhsAllocationFactor;
	std::vector <complex> CalculatedCurrent;
	std::vector <complex> CalculatedVoltage;
	double AvgAllocFactor; /*Average Allocation Factor*/
	TMeterElement(DSSClass::TDSSClass* ParClass);
	virtual ~TMeterElement();
	virtual void TakeSample(int ActorID);    // Sample control quantities and set action times in Control Queue
	void AllocateSensorArrays();
	void CalcAllocationFactors(int ActorID);
	TMeterElement(String ClassName);
	TMeterElement();
};


}  // namespace MeterElement

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace MeterElement;
#endif

#endif // MeterElementH





