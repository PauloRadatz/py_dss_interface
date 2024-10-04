
#pragma hdrstop

#include "ControlElem.h"


#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "PointerList.h"

using namespace std;
using namespace Bus;
using namespace CktElement;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace NamedObject;
using namespace PointerList;
using namespace System;


namespace ControlElem
{

TControlElem::TControlElem(String ClassName) : inherited(ClassName) {}
TControlElem::TControlElem() {}

int TControlElem::InitializeStates(int ActorID)
{
	// Nothing here, should default to individual model function
	return 0;
}
int TControlElem::CalculateRate(int ActorID)
{
	// Nothing here, should default to individual model function
	return 0;
}

int TControlElem::StateIntegration(int ActorID)
{
	// Nothing here, should default to individual model function
	return 0;
}

int TControlElem::StateIntegration_correction(int ActorID)
{
	return 0;
}

complex* TControlElem::CalculateIsorce(int ActorID, complex *pI012)
{
	complex* p = nullptr;

	return p;
}

complex TControlElem::GetIsorce1(int ActorID)
{
	return complex{};
}

complex TControlElem::GetIsorce2(int ActorID)
{
	return complex{};
}

complex TControlElem::dq2seq(complex vin, double angle)
{
	double voutr = 0.;
	double vouti = 0.;

	voutr = vin.re * sin(angle) + vin.im * cos(angle);
	vouti = -vin.re * cos(angle) + vin.im * sin(angle);
	return complex{voutr, vouti};
}

complex TControlElem::seq2dq(complex vin, double angle)
{
	double voutd = 0.;
	double voutq = 0.;

	voutd = vin.re * sin(angle) - vin.im * cos(angle);
	voutq = vin.re * cos(angle) + vin.im * sin(angle);

	return complex{ voutd, voutq };
}

double TControlElem::get_channel(int channel_num)
{
	return 0.0;
}

int64_t TControlElem::get_number_channels()
{
		// should never get here but rather direct to child class
	return 0;
}

String TControlElem::get_channel_header(int i)
{
	return String();
}

double TControlElem::get_efield()
{
	//	Should never get here
	return 0.0;
}

void TControlElem::set_efield(double efield)
{
	//	Should never get here
	return;
}

double TControlElem::get_pmech()
{
	//	Should never get here
	return 0.0;
}

void TControlElem::set_pmech(double pmech)
{
	//	Should never get here
	return;
}

double TControlElem::get_dspd()
{
	//	Should never get here
	return 0.0;
}

void TControlElem::set_dspd(double dspd)
{
	//	Should never get here
	return;
}

double TControlElem::get_plast()
{
	//	Should never get here
	return 0.0;
}

double TControlElem::get_qlast()
{
	//	Should never get here
	return 0.0;
}

TControlElem::TControlElem(TDSSClass* ParClass)
 : inherited(ParClass),
			FControlledElement(nullptr),
			FMonitoredElement(nullptr),
			ElementTerminal(0),
			ControlledBus(nullptr),
			MonitorVarIndex(0),
			TimeDelay(0.0),
			DblTraceParameter(0.0),
			ShowEventLog(false)
{
	DSSObjType = CTRL_ELEMENT;
	DblTraceParameter = 0.0;
	TimeDelay = 0.0;
	MonitorVariable = "";
	MonitorVarIndex = 0;
	ShowEventLog = EventLogDefault;
}

TControlElem::~TControlElem()
{
	// inherited::Destroy();
}

//---------------------------------------------------------------------------------------

TDSSCktElement* TControlElem::get_FControlledElement()
{
	return FControlledElement;
}

//---------------------------------------------------------------------------------------

TDSSCktElement* TControlElem::get_FMonitoredElement()
{
	return FMonitoredElement;
}

//---------------------------------------------------------------------------------------

void TControlElem::DoPendingAction(int Code, int ProxyHdl, int ActorID)
{

  // virtual function - should be overridden
	DoSimpleMsg(String("Programming Error:  Reached base class for DoPendingAction.") + CRLF
	           + "Device: "
	           + Get_myPName()
	           + "."
	           + get_Name(), 460);
}
/*Remove this control from the controlelementlist of the designated element*/

void TControlElem::RemoveSelfFromControlelementList(TDSSCktElement* CktElem)
{
	TControlElem* Ptr = nullptr;
	PointerList::TPointerList* TempList = nullptr;
	int i = 0;
	/*# with CktElem do */
	{
		auto with0 = CktElem;
         // Make a new copy of the control element list
		int stop = 0;
		TempList = new PointerList::TPointerList(1);
		for(stop = with0->ControlElementList.get_myNumList(), i = 1; i <= stop; i++)
		{
			Ptr = ((TControlElem*) with0->ControlElementList.Get(i));
			if(Ptr != this)
				TempList->Add(Ptr);  // skip Self in copying list
		}
		with0->ControlElementList.Clear();
		with0->ControlElementList = *TempList;
	}
}

void TControlElem::Reset(int ActorID)
{
	DoSimpleMsg(String("Programming Error: Reached base class for Reset.") + CRLF
	           + "Device: "
	           + Get_myPName()
	           + "."
	           + get_Name(), 461);
}

void TControlElem::sample(int ActorID)
{

  // virtual function - should be overridden
	DoSimpleMsg(String("Programming Error:  Reached base class for Sample.") + CRLF
	           + "Device: "
	           + Get_myPName()
	           + "."
	           + get_Name(), 462);
}

void TControlElem::Set_ControlledElement(TDSSCktElement* const Value)
{
	try

      // Check for reassignment of Controlled element and remove from list
	{
		if(ASSIGNED(FControlledElement))
			/*# with FControlledElement do */
			{
				auto with0 = FControlledElement;
				if(with0->ControlElementList.get_myNumList() == 1)
					with0->HasControl = false;
				RemoveSelfFromControlelementList(FControlledElement);
			}
/* }
	__finally
	{*/
		FControlledElement = const_cast<TDSSCktElement*>(Value);
		if(ASSIGNED(FControlledElement))
			/*# with FControlledElement do */
			{
				auto with1 = FControlledElement;
				with1->HasControl = true;
				with1->ControlElementList.Add(this);
			}
	}
	catch (...)
	{
		//
	}
}

void TControlElem::Set_MonitoredElement(TDSSCktElement* const Value)
{
	FMonitoredElement = const_cast<TDSSCktElement*>(Value);
	if(ASSIGNED(FMonitoredElement))
		FMonitoredElement->IsMonitored = true;
}






}  // namespace ControlElem





