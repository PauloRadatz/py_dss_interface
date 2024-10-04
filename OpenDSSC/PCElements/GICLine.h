#ifndef GICLineH
#define GICLineH

#include "System.h"
#include "Sysutils.h"

#include "DSSClass.h"
#include "PCClass.h"
#include "PCElement.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Spectrum.h"
#include "d2c_structures.h"



namespace GICLine
{



/*
  ----------------------------------------------------------
  Copyright (c) 2011-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
 6-23-2011 Created from VSource object

 Simplified 2-terminal VSource with series impedance for GIC studies.
 For representing induced voltages in lines

 Contains blocking capacitor inherent in model.  Set C > 0.0 to activate.

 Blocking capacitors may also be added as separate items or the branch may be
 disconnected.

 Example:
    New GICline.Myline  Bus1=MyBus1  Bus2=MyBus2  Volts=1234   R=0.5

    This takes the following defaults:
      Angle=0
      X=0
      C=0
      Frequency=0.1 Hz
      Sequence = ZERO sequence
      ScanType = ZERO sequence


*/
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TGICLine : public PCClass::TPCClass
{
	friend class TGICLineObj;
public:
	typedef PCClass::TPCClass inherited;	
private:
	void GICLineSetBus1(const String s);
protected:
	void DefineProperties();
	virtual int MakeLike(const String OtherLine);
public:
	TGICLine();
	virtual ~TGICLine();
	virtual int Edit(int ActorID);
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

class TGICLineObj : public PCElement::TPCElement
{
	friend class TGICLine;
public:
	typedef PCElement::TPCElement inherited;	
private:
	double Angle;
	double Volts;
	double Vmag;  // Present voltage magnitude
	double SrcFrequency;
	double R;
	double X;
	double C;
	double ENorth;
	double EEast;
	double Lat1;
	double Lon1;
	double Lat2;
	double Lon2;
	double Vn;
	double VE;  // components of vmag
	int ScanType;
	int SequenceType;
	bool VoltsSpecified;
	void GetVterminalForSource();
	double Compute_VLine();
public:
	Ucmatrix::TcMatrix* Z;  // Base Frequency Series Z matrix
	Ucmatrix::TcMatrix* Zinv;
	TGICLineObj(DSSClass::TDSSClass* ParClass, const String SourceName);
	virtual ~TGICLineObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual int InjCurrents(int ActorID);
	virtual void GetInjCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void GetCurrents(Ucomplex::pComplexArray Curr, int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	TGICLineObj(DSSClass::TDSSClass* ParClass);
	TGICLineObj(String ClassName);
	TGICLineObj();
};
extern TGICLineObj* ActiveGICLineObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


}  // namespace GICLine

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace GICLine;
#endif

#endif // GICLineH




