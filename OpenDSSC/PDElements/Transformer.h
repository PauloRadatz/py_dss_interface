#ifndef TransformerH
#define TransformerH

#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"

#include "Command.h"
#include "DSSClass.h"
#include "PDClass.h"
#include "PDElement.h"
#include "Ucomplex.h"
#include "Ucmatrix.h"
#include "ParserDel.h"
#include "Arraydef.h"
#include <math.h>

#include "DSSClassDefs.h"



namespace Transformer
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2023, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Change log
   1-28-00 Added tap properties so that regulator can control it.
   1-29-00 Added GetWindingVoltages
    2-1-00 Replaced TranParser with global AuxParser
    2-9-00 Fixed Set_PresentTap bug
   1-23-03 Added code to get 30 deg lag correct of y-delta transformers
   2-18-03 changed Rneut default to open (-1)
   2-21-03 changed automatic resetting of connection designator upon changing Rneut
   9-12-11 Fixed pctLoadLoss problem with sequence of definition with kVA property
*/

/* You can designate a transformer to be a substation by setting the sub=yes parameter*/

class TTransf : public TPDClass
{
	friend class TTransfObj;
public:
	typedef PDClass::TPDClass inherited;	
//private:
	void SetActiveWinding(int W);
	void InterpretConnection(const String s);
	void InterpretAllConns(const String s);
	void InterpretAllBuses(const String s);
	void InterpretAllTaps(const String s);
	void InterpretAllkVRatings(const String s);
	void InterpretAllkVARatings(const String s);
	void InterpretAllRs(const String s);
	double TrapZero(double Value, double DefaultValue);
	bool InterpretLeadLag(const String s);

       /*PROCEDURE MakeNewBusNameForNeutral(Var NewBusName:String; Nphases:Integer);*/
protected:
	void DefineProperties();
	virtual int MakeLike(const String TransfName);
public:
	TTransf();
	virtual ~TTransf();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TWinding
{
public:
	int Connection;      // on transformer MVABase  (1st winding)
    // for GIC solutions; default to 85% of Rpu
	double kVLL;
	double VBase;
	double kVA;
	double puTap;
	double Rpu;
	double Rdcpu;
	double RdcOhms;
	double Rneut;
	double Xneut;
	double Y_PPM;  // Anti Float reactance adder
	bool RdcSpecified;

        /*Tap Changer Data*/
	double TapIncrement;
	double MinTap;
	double MaxTap;
	int NumTaps;
	void ComputeAntiFloatAdder(double PPM_Factor, double VABase1ph);
	TWinding();
};

class TTransfObj : public PDELement::TPDElement
{
	friend class TTransf;
public:
	typedef PDELement::TPDElement inherited;	
//private:
	int DeltaDirection;
	double ppm_FloatFactor; //  parts per million winding float factor
	double pctImag;
	bool XRConst;
	double Get_PresentTap(int i, int ActorID) const;
	void Set_PresentTap(int i, int ActorID, double Value);
	double Get_MinTap(int i) const;
	double Get_MaxTap(int i) const;
	double Get_TapIncrement(int i) const;
	double Get_BaseVoltage(int i);
	double Get_BasekVLL(int i);
        // CIM accessors
	int Get_NumTaps(int i);
	double Get_WdgResistance(int i);
	double Get_WdgRdc(int i);
	int Get_WdgConnection(int i);
	double Get_WdgkVA(int i);
	double Get_Xsc(int i);
	double Get_WdgRneutral(int i);
	double Get_WdgXneutral(int i);
	double Get_WdgYPPM(int i);
	void CalcY_Terminal(double FreqMult);
	void GICBuildYTerminal();
	void BuildYPrimComponent(Ucmatrix::TcMatrix* YPrim_Component, Ucmatrix::TcMatrix* Y_Terminal);
	void AddNeutralToY(double FreqMultiplier);
	void FetchXfmrCode(const String Code);
protected:
	int NumWindings;
	int MaxWindings;
	std::vector <longInt> TermRef;  // keeps track of terminal connections
	double XHL;
	double XHT;
	double XLT;  // per unit
	double ZBase;
	std::vector <double> XSC;     // per unit SC measurements
	double VABase;    // FOR impedances
	Ucmatrix::TcMatrix* ZB;
	Ucmatrix::TcMatrix* Y_1Volt;
	Ucmatrix::TcMatrix* Y_Term;
	Ucmatrix::TcMatrix* Y_1Volt_NL;   // No Load Y's
	Ucmatrix::TcMatrix* Y_Term_NL;
	double Y_Terminal_Freqmult;
	double NormMaxHkVA;
	double EmergMaxHkVA;
	double ThermalTimeConst;  /*hr*/
	double n_thermal;
	double m_thermal;  /*Exponents*/
	double FLrise;
	double HSrise;
	bool HVLeadsLV;
	bool XHLChanged;
	Arraydef::TRatingsArray kVARatings;
	void SetTermRef();
public:
	double pctLoadLoss;
	double pctNoLoadLoss;
	int ActiveWinding;  // public for COM interface
	bool IsSubstation;
	String SubstationName;
	std::vector<TWinding> WINDING_;
	String XfmrBank;
	String XfmrCode;
	int CoreType; /*0=Shell; 1=1ph; 3-3leg; 5=5-leg*/
	String strCoreType;
	TTransfObj(DSSClass::TDSSClass* ParClass, const String TransfName);
	virtual ~TTransfObj();
	void SetNumWindings(int n);
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);

        /*GetLosses override for Transformer*/
	virtual void GetLosses(Ucomplex::complex& TotalLosses, Ucomplex::complex& LoadLosses, Ucomplex::complex& NoLoadLosses, int ActorID);
	int RotatePhases(int iPhs);
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual void SaveWrite(System::TTextRec& f);
	void GetWindingVoltages(int iWind, Ucomplex::pComplexArray VBuffer, int ActorID);
	void GetAllWindingCurrents(Ucomplex::pComplexArray CurrBuffer, int ActorID);  // All Winding currents in complex array
	String GetWindingCurrentsResult(int ActorID);  // All winding currents in string
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model

	int get_NumWindings();
	double get_XHL();
	double get_XHT();
	double get_XLT();
	double get_NormMaxHkVA();
	double get_EmergMaxHkVA();
	double get_ThermalTimeConst();
	double get_n_thermal();
	double get_m_thermal();
	double get_FLrise();
	double get_HSrise();
	double get_pctLoadLoss();
	double get_pctNoLoadLoss();
	double get_pctImag();
	double get_ppm_FloatFactor();
	double get_VABase();


        // CIM accessors
	TTransfObj(DSSClass::TDSSClass* ParClass);
	TTransfObj(String ClassName);
	TTransfObj();
};
extern TTransfObj* ActiveTransfObj;

int XSCSize(int NumWindings);

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


}  // namespace Transformer

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Transformer;
#endif

#endif // TransformerH




