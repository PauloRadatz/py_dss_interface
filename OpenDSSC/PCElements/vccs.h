#ifndef VCCSH
#define VCCSH

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "XYcurve.h"
#include "Arraydef.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "Command.h"




namespace VCCS
{


/*
  ----------------------------------------------------------
  Copyright (c) 2021, University of Pittsburgh
  Copyright (c) 2019-2021, Battelle Memorial Institute
  All rights reserved.
  ----------------------------------------------------------
*/

class TVCCS : public PCClass::TPCClass
{
	friend class TVCCSObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
	DSSClass::TDSSClass* XY_CurveClass;
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherSource);
public:
	TVCCS();
	virtual ~TVCCS();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TVCCSObj : public PCElement::TPCElement
{
	friend class TVCCS;
public:
	typedef PCElement::TPCElement inherited;	
private:
	XYCurve::TXYcurveObj* Fbp1;
	String Fbp1_name;
	XYCurve::TXYcurveObj* Fbp2;
	String Fbp2_name;
	XYCurve::TXYcurveObj* FFilter;
	String Ffilter_name;
	double BaseCurr; // line current at Ppct
	double BaseVolt; // line-to-neutral voltage at Vrated
	double FsampleFreq; // discretization frequency for Z filter
	int Fwinlen;
	int Ffiltlen;
	double Irated; // line current at full output
	double Fkv; // scale voltage to HW pu input
	double Fki; // scale HW pu output to current
	bool FrmsMode; // indicates a phasor-domain PLL simulation
	double FmaxIpu; // maximum RMS current in per-unit of rated
	double FvrmsTau; // LPF time constant sensing Vrms
	double FirmsTau; // LPF time constant producing Irms

        // Support for Dynamics Mode - PU of BaseVolt and BaseCurr
        // state variables for Dynamics Mode
	double S1; // Vwave(t), or Vrms in phasor mode
	double S2; // Iwave(t), or Ipwr in phasor mode
	double S3; // Irms,     or Hout in phasor mode
	double S4; // Ipeak,    or Irms in phasor mode
	double S5; // BP1out,   or NA in phasor mode
	double S6; // Hout,     or NA in phasor mode
	Ucomplex::complex sV1; // positive-sequence voltage; use to inject I1 only
	Ucomplex::complex vlast;
	Arraydef::pDoubleArray Y2;
	Arraydef::pDoubleArray Z;     // current digital filter history terms
	Arraydef::pDoubleArray whist;
	Arraydef::pDoubleArray zlast; // update only after the corrector step
	Arraydef::pDoubleArray wlast;
	int sIdxU; // ring buffer index for z and whist
	int sIdxY; // ring buffer index for y2 (rms current)
	double y2sum;
	void InitPhasorStates(int ActorID);
	void IntegratePhasorStates(int ActorID);
	void ShutoffInjections();
	void UpdateSequenceVoltage();
protected:
	virtual double Get_Variable(int i);
	virtual void Set_Variable(int i, double Value);
public:
	double Ppct;
	double Prated;
	double Vrated;
	TVCCSObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TVCCSObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

        // Support for Dynamics Mode
	virtual void InitStateVars(int ActorID);
	virtual void IntegrateStates(int ActorID);
	virtual int NumVariables();
	virtual void GetAllVariables(Arraydef::pDoubleArray States);
	virtual String VariableName(int i);
	TVCCSObj(DSSClass::TDSSClass* ParClass);
	TVCCSObj(String ClassName);
	TVCCSObj();
};
extern TVCCSObj* ActiveVCCSObj;
extern TVCCS* VCCSClass;


}  // namespace VCCS

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace VCCS;
#endif

#endif // VCCSH




