#ifndef UPFCH
#define UPFCH

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Spectrum.h"
#include "Arraydef.h"
#include "LoadShape.h"
#include "XYcurve.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "Dynamics.h"
#include "Command.h"
#include "YMatrix.h"


namespace UPFC
{


/*
  ----------------------------------------------------------
  Copyright (c) 2022,  Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
 7-6-2015  Created from VSOURCE 

*/
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TUPFC : public PCClass::TPCClass
{
	friend class TUPFCObj;
public:
	typedef PCClass::TPCClass inherited;	
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherSource);
public:
	TUPFC();
	virtual ~TUPFC();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TUPFCObj : public PCElement::TPCElement
{
	friend class TUPFC;
public:
	typedef PCElement::TPCElement inherited;	
//private:
	double VRef; //Expected vooltage in the output (only magnitude)
	double PF; //Expected power factor (under revision)
	double Xs; //Impedance of the series Xfmr
	Ucomplex::pComplexArray Sr0; //Shift register for controller 1
	Ucomplex::pComplexArray Sr1; //Shift register for controller 2
	Ucomplex::complex Vbin; // Voltage at the input of the device
	Ucomplex::complex Vbout; // Voltage at the output of the device
	double Tol1;   //Tolerance (dead band) specified for the controller 1
	double ERR0[6/*# range 1..6*/]; //Error controller 1 for Dual mode
	double ZBase;
	double Freq;
	double VHLimit;   // High limit for the input voltage in volts (default 300V)
	double VLLimit;   // Low limit for the input voltage in volts (default 125V)
	double CLimit;   // Limit for the maximum current in amperes
	bool UPFCON;   // Flag to indicate when the UPFC operation is out of boundaries
	double VRef2;   // Value for deadband's upper limit, it is calculated if tolerance is specified
	double VRefD;   // Dynamic reference for control modes 4 and 5
	double KVARLim;   // kvar limit, defines the maximum amount of kvars that the UPFC can absorb
	String MonElm; // Name of the monitored element to perform PF compensation
	TDSSCktElement* myElm; // ref to the monitored element

        // some state vars for reporting
	double Losses;
	Ucomplex::complex IUPFC;
	Ucomplex::complex UPFC_Power;
	double QIdeal;
	int ModeUPFC;
	double Vpqmax;
	bool SyncFlag;   // Flag used to synchronize controllers in Dual mode
	bool SF2;   // Flag used to Synch control modes 4 and 5
	String LossCurve;      //Losses curve name
	XYCurve::TXYcurveObj* UPFCLossCurveObj; //Losses curve reference
	Ucomplex::complex GetinputCurr(int Cond, int ActorID);
	Ucomplex::complex GetOutputCurr(int Cond, int ActorID);
	Ucomplex::complex CalcUPFCPowers(int ModeUP, int Cond);
	double CalcUPFCLosses(double Vpu);
protected:
	virtual double Get_Variable(int i);
	virtual void Set_Variable(int i, double Value);
public:
	vector<Ucomplex::complex> InCurr;
	vector<Ucomplex::complex> OutCurr; // for storing the input and output currents
	Ucmatrix::TcMatrix* Z;  // Base Frequency Series Z matrix
	Ucmatrix::TcMatrix* Zinv;
	double Vmag;
	TUPFCObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TUPFCObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);

        // Uploads the input/output currents when commanded by the controller - 09/02/2021
	void UploadCurrents(int ActorID);
	bool CheckPFStatus(int ActorID);
	bool CheckStatus(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	virtual int NumVariables();
	virtual void GetAllVariables(Arraydef::pDoubleArray States);
	virtual String VariableName(int i);
	TUPFCObj(DSSClass::TDSSClass* ParClass);
	TUPFCObj(String ClassName);
	TUPFCObj();
};
extern TUPFCObj* ActiveUPFCObj;
extern TUPFC* UPFC_class;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace UPFC

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace UPFC;
#endif

#endif // UPFCH




