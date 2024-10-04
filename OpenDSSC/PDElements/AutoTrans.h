#ifndef AutoTransH
#define AutoTransH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "PDClass.h"
#include "PDElement.h"
#include "Ucomplex.h"
#include "Ucmatrix.h"
#include "ParserDel.h"
#include "Arraydef.h"




namespace AutoTrans
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
   Change log
   7-14-2018  Created from Transformer
   9-19-2018  committed
   12-4-2018  Corrected indices for mapping into Yprim
   1-3-2019   Default last nphase nodes of X terminal (2) to same as first neutral node
   3-6-2021  Added code for readability
*/

/* You can designate a AutoTrans to be a substation by setting the sub=yes parameter*/

class TAutoTrans : public PDClass::TPDClass
{
	friend class TAutoTransObj;
public:
	typedef PDClass::TPDClass inherited;	
private:
	void SetActiveWinding(int W);
	void InterpretAutoConnection(const String s);
	void InterpretAllConns(const String s);
	void InterpretAllBuses(const String s, int ActorID);
	void InterpretAllTaps(const String s);
	void InterpretAllkVRatings(const String s);
	void InterpretAllkVARatings(const String s);
	void InterpretAllRs(const String s);
	double TrapZero(double Value, double DefaultValue);
	bool InterpretLeadLag(const String s);

       /*PROCEDURE MakeNewBusNameForNeutral(Var NewBusName:String; Nphases:Integer);*/
protected:
	void DefineProperties();
	virtual int MakeLike(const String AutoTransfName);
public:
	TAutoTrans();
	virtual ~TAutoTrans();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TAutoWinding
{
public:
	int Connection;
	double kVLL;
	double VBase;
	double kVA;
	double puTap;
	double Rpu;    // on AutoTrans MVABase  (H-X Rating)
	double Rdcpu;    // on AutoTrans MVABase  (H-X Rating)
	double RdcOhms;    // for GIC solutions; default to 85% of Rpu
	bool RdcSpecified;
	double Y_PPM;  // Anti Float reactance adder
	
        /*Tap Changer Data*/
	double TapIncrement;
	double MinTap;
	double MaxTap;
	int NumTaps;
	void ComputeAntiFloatAdder(double PPM_Factor, double VABase1ph);
	TAutoWinding(int iWinding);
	TAutoWinding();
};

class TAutoTransObj : public PDELement::TPDElement
{
	friend class TAutoTrans;
public:
	typedef PDELement::TPDElement inherited;	
//private:
	int DeltaDirection;
	double ppm_FloatFactor; //  parts per million winding float factor
	double pctImag;
	bool XRConst;
	double Get_PresentTap(int i, int ActorID);
	void Set_PresentTap(int i, int ActorID, double Value);
	double Get_MinTap(int i);
	double Get_MaxTap(int i);
	double Get_TapIncrement(int i);
	double Get_BaseVoltage(int i);
	double Get_BasekVLL(int i);
        // CIM accessors
	int Get_NumTaps(int i);
	double Get_WdgResistance(int i);
	int Get_WdgConnection(int i);
	double Get_WdgkVA(int i);
	double Get_Xsc(int i);
	double Get_WdgYPPM(int i);
	void CalcY_Terminal(double FreqMult, int ActorID);
	void GICBuildYTerminal();
	void BuildYPrimComponent(Ucmatrix::TcMatrix* YPrim_Component, Ucmatrix::TcMatrix* Y_Terminal);
	void FetchXfmrCode(const String Code);
	String GeTAutoWindingCurrentsResult(int ActorID);
	void SetBusAuto(int iwdg, const String s, int ActorID);
protected:
	int NumWindings;
	int MaxWindings;
	std::vector <longInt> TermRef;  // keeps track of terminal connections
	double puXHX;
	double puXHT;
	double puXXT;  // per unit
	double ZBase;
	std::vector <double> puXSC;     // per unit SC measurements
	double VABase;    // FOR impedances
	double kVSeries;   // Rating for Series winding
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
	bool XHXChanged;
	void SetTermRef();
public:
	double pctLoadLoss;
	double pctNoLoadLoss;
	int ActiveWinding;  // public for COM interface
	bool IsSubstation;
	String SubstationName;
	std::vector<TAutoWinding> WINDING_;
	String XfmrBank;
	String XfmrCode;
	int CoreType; /*0=Shell; 1=1ph; 3-3leg; 5=5-leg*/
	String strCoreType;
	TAutoTransObj(DSSClass::TDSSClass* ParClass, const String TransfName);
	virtual ~TAutoTransObj();
	void SetNumWindings(int n);
	virtual void RecalcElementData(int ActorID);
	virtual void SetNodeRef(int iTerm, Arraydef::pIntegerArray NodeRefArray);
	virtual void CalcYPrim(int ActorID);

        /*GetLosses override for AutoTrans*/
	virtual void GetLosses(Ucomplex::complex& TotalLosses, Ucomplex::complex& LoadLosses, Ucomplex::complex& NoLoadLosses, int ActorID);
        /*Getcurrents Override for AutoTrans*/
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID); // Get present values of terminal
	int RotatePhases(int iPhs);
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual void SaveWrite(System::TTextRec& f);
	void GetAutoWindingVoltages(int iWind, Ucomplex::pComplexArray VBuffer, int ActorID);
	void GetAllWindingCurrents(Ucomplex::pComplexArray CurrBuffer, int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model

	int get_NumWindings();

        // CIM accessors
	TAutoTransObj(DSSClass::TDSSClass* ParClass);
	TAutoTransObj(String ClassName);
	TAutoTransObj();
};
extern TAutoTransObj* ActiveAutoTransObj;
extern TAutoTrans* AutoTransClass;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


}  // namespace AutoTrans

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace AutoTrans;
#endif

#endif // AutoTransH




