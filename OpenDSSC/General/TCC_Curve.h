#ifndef TCC_CurveH
#define TCC_CurveH

#include "System.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Ucmatrix.h"
#include "Arraydef.h"


namespace TCC_Curve
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/* Created 8-25-00 */

/*
 Nominally, a time-current curve, but also used for volt-time curves.

 Collections of time points.  Return values can be interpolated either
 Log-Log as traditional TCC or as over- or under-voltage definite time.
*/

class TTCC_Curve : public DSSClass::TDSSClass
{
	friend class TTCC_CurveObj;
public:
	typedef DSSClass::TDSSClass inherited;	
//private:
	String Get_Code();  // Returns active TCC_Curve String
	void Set_Code(const String Value);  // sets the  active TCC_Curve
protected:
	void DefineProperties();
	virtual int MakeLike(const String ShapeName);
public:
	TTCC_Curve();
	virtual ~TTCC_Curve();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TTCC_CurveObj : public DSSObject::TDSSObject
{
	friend class TTCC_Curve;
public:
	typedef DSSObject::TDSSObject inherited;	
//private:
	int LastValueAccessed;
	int npts;  // Number of points in curve
        // Logarithms of t_values and c_values
          // Time values (hr) if Interval > 0.0  Else nil
	Arraydef::pDoubleArray Logt;
	Arraydef::pDoubleArray LogC;
	Arraydef::pDoubleArray t_values;
	Arraydef::pDoubleArray c_values;
public:
	TTCC_CurveObj(DSSClass::TDSSClass* ParClass, const String TCC_CurveName);
	virtual ~TTCC_CurveObj();
	double GetTCCTime(double C_Value);  // Return operating time for a particular time value
	double GetUVTime(double V_Value);  // Return operating time for undervoltage relay
	double GetOVTime(double V_Value);  // Return operating time for overvoltage relay
	double Value(int i);  // get C_Value by index
	double Time(int i);  // get time value (sec) corresponding to point index
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TTCC_CurveObj(DSSClass::TDSSClass* ParClass);
	TTCC_CurveObj(String ClassName);
	TTCC_CurveObj();
};
extern TTCC_CurveObj* ActiveTCC_CurveObj;


}  // namespace TCC_Curve

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace TCC_Curve;
#endif

#endif // TCC_CurveH





