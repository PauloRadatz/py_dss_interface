#ifndef PCElementH
#define PCElementH

#include "System.h"
#include "Sysutils.h"

#include "CktElement.h"
#include "Ucomplex.h"
#include "DSSClass.h"
#include "Arraydef.h"
#include "DSSClassDefs.h"
#include "fMonitor.h"
#include "Spectrum.h"
#include "DynamicExp.h"


namespace PCElement
{



/*$M+*/
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TPCElement : public TDSSCktElement
{
public:
	typedef TDSSCktElement inherited;	
//private:
	bool FIterminalUpdated;
//protected:
	virtual void GetTerminalCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual double Get_Variable(int i);
	virtual void Set_Variable(int i, double Value);
public:
	String Spectrum;
	Spectrum::TSpectrumObj* SpectrumObj;  /*Upline Energymeter*/
	MeterElement::TMeterElement* MeterObj;
	MeterElement::TMeterElement* SensorObj; // Upline Sensor for this element
	       /*by Dahei*/
	Fmonitor::TFMonitorObj* FMonObj;
	int Cluster_num;
	int NdNumInCluster;
	int nVLeaders;   // How many virtual leaders for this pcelement
	Fmonitor::TFMonitorObj* FMonObj2;
	int cluster_num2;
	int NdNumInCluster2;
	int NumStateVars;
	Ucomplex::pComplexArray InjCurrent;
	std::string DynamicEq;
	TDynamicExpObj* DynamicEqObj;
	std::vector <DynSlot> DynamicEqVals;
	std::vector <int> DynOut, DynamicEqPair;

	TPCElement(TDSSClass* ParClass);
	virtual ~TPCElement();
	void ZeroInjCurrent();
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present values of terminal
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present values of terminal
	virtual void ComputeIterminal(int ActorID);
	virtual int InjCurrents(int ActorID);
	void CalcYPrimContribution(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

      // Sweep solution removed  PROCEDURE BackwardSweep;Override;

      // For Harmonics Mode
	virtual void InitHarmonics(int ActorID);
	void set_ITerminalUpdated(bool Value, int ActorID);
	bool get_FITerminalUpdated(int ActorID, int value);
       // For Dynamics Mode and Control Devices
	virtual void InitStateVars(int ActorID);
	virtual void IntegrateStates(int ActorID);
	virtual int InitializeStates(int ActorID);
	virtual void CalculateRate(int ActorID);
	virtual void StateIntegration(int ActorID);
	virtual void StateIntegration_correction(int ActorID);
	virtual int NumVariables();
	virtual void GetAllVariables(Arraydef::pDoubleArray States);
	virtual String VariableName(int i);
	int LookupVariable(const String s);
	virtual	bool IsGenerator();
	virtual	bool CheckForGeneratorModel();

	int CheckIfDynVar(string myVar, int ActorID);
	void SetDynOutput(string myVar);
	string GetDynOutputStr();

	//__declspec ( property ( get = Get_Variable, put = Set_Variable ) )  double Variable[];
	////       Property ITerminalUpdated:Boolean read FITerminalUpdated write set_ITerminalUpdated;
	//__declspec ( property(get = get_FITerminalUpdated, put = set_ITerminalUpdated)) bool ITerminalUpdated[]DECLSPEC_2D;
//	TPCElement(TDSSClass* ParClass);
	TPCElement(String ClassName);
	TPCElement();
};


}  // namespace PCElement

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PCElement;
#endif

#endif // PCElementH




