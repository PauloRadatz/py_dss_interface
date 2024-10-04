#ifndef ControlElemH
#define ControlElemH

#include "System.h"
#include "Sysutils.h"

#include "CktElement.h"
#include "Bus.h"
#include "Ucomplex.h"
#include "DSSClass.h"


namespace ControlElem
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TControlElem : public CktElement::TDSSCktElement
{
public:
	typedef CktElement::TDSSCktElement inherited;	
//private:
	CktElement::TDSSCktElement* FControlledElement;
	CktElement::TDSSCktElement* FMonitoredElement;
	void Set_ControlledElement(CktElement::TDSSCktElement* const Value);  // Pointer to target circuit element
	void RemoveSelfFromControlelementList(CktElement::TDSSCktElement* CktElem);
	void Set_MonitoredElement(CktElement::TDSSCktElement* const Value);
	
public:
	String ElementName;
	int ElementTerminal;
	String ControlledBusName;  // If different than terminal
	Bus::TDSSBus* ControlledBus;
	String MonitorVariable;
	int MonitorVarIndex;
	double TimeDelay;
	double DblTraceParameter;
	bool ShowEventLog;
	TControlElem(DSSClass::TDSSClass* ParClass);
	virtual ~TControlElem();
	virtual void sample(int ActorID);    // Sample control quantities and set action times in Control Queue
	virtual void DoPendingAction(int Code, int ProxyHdl, int ActorID);   // Do the action that is pending from last sample
	virtual void Reset(int ActorID);
	TDSSCktElement* get_FControlledElement();
	TDSSCktElement* get_FMonitoredElement();

	TControlElem(String ClassName);
	TControlElem();


	//  Functions added to support new dynamics module
	int64_t nstates;
	int64_t	nchannels;
	virtual int InitializeStates(int ActorID);
	virtual int CalculateRate(int ActorID);
	virtual int StateIntegration(int ActorID);
	virtual int StateIntegration_correction(int ActorID);
	virtual complex* CalculateIsorce(int ActorID, complex *pI012);
	virtual complex GetIsorce1(int ActorID);
	virtual complex GetIsorce2(int ActorID);
	complex dq2seq(complex vin, double angle);
	complex seq2dq(complex vin, double angle);
	virtual double get_channel(int channel_num);
	virtual int64_t get_number_channels();
	virtual String get_channel_header(int channel_num);

	// Functions related to excitation systems
	virtual double get_efield();
	virtual void set_efield(double efield);

	// Function related to governor systems
	virtual double get_pmech();
	virtual void set_pmech(double pmech);
	virtual double get_dspd();
	virtual void set_dspd(double dspd);

	//	Functions to extract quantities from dynamic models
	virtual double get_plast();
	virtual double get_qlast();
	
};
const int USER_BASE_ACTION_CODE = 100;


}  // namespace ControlElem

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ControlElem;
#endif

#endif // ControlElemH





