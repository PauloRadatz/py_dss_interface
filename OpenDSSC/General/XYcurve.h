#ifndef XYCurveH
#define XYCurveH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Arraydef.h"
#include "d2c_structures.h"
#include <array>


namespace XYCurve
{



/*
  ----------------------------------------------------------
  Copyright (c) 2011-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*  2-15-2011 Converted from TempShape.

   General X-Y Curve Data Support Class

*/

/*

 The XYcurve object is a general DSS object used by all circuit elements
 as a reference for obtaining yearly, daily, and other Temperature shapes.

 The values are set by the normal New and Edit PROCEDUREs for any DSS object.

 The values may be retrieved by setting the Code Property in the XYCurve Class.
 This sets the active XYCurve object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveTXYcurveObj object and save the direct reference to the object.

 The user may place the curve data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, or white space
 one point to a line.

 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.



 */

// We're not allowed to return arrays such as if we used "typedef double TCoeff[2];",
// so instead, use C++ std::array:
typedef std::array<double,2> TCoeff;

class TXYcurve : public DSSClass::TDSSClass
{
	friend class TXYcurveObj;
public:
	typedef DSSClass::TDSSClass inherited;	
//private:
	std::vector <double> TempPointsBuffer;
	string Get_Code();  // Returns active TShape string
	void Set_Code(const String Value);  // sets the  active TShape
	void DoCSVFile(const String FileName);
	void DoSngFile(const String FileName);
	void DoDblFile(const String FileName);
protected:
	void DefineProperties();
	virtual int MakeLike(const String CurveName);
public:
	TXYcurve();
	virtual ~TXYcurve();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	virtual void* Find(const String ObjName);  // Find an obj of this class by name


       // Set this property to point ActiveTShapeObj to the right value
};

class TXYcurveObj : public TDSSObject
{
	friend class TXYcurve;
public:
	typedef TDSSObject inherited;	
//private:
	int LastValueAccessed;
	int FNumPoints;  // Number of points in curve
	int ArrayPropertyIndex;
	double FX;
	double FY;
	std::vector <double> XValues;
	std::vector <double> YValues;
	void Set_NumPoints(int Value);
	double InterpolatePoints(int i, int j, double X, Arraydef::pDoubleArray Xarray, Arraydef::pDoubleArray Yarray);
       // PROCEDURE SaveToDblFile;
       // PROCEDURE SaveToSngFile;
	double Get_YValue(int i);  // get Y Value by index
	double Get_XValue(int i);  // get X Value corresponding to point index
	void Set_XValue(int Index, double Value);
	void Set_YValue(int Index, double Value);
	double Get_X();
	double Get_Y();
	void Set_X(double Value);
	void Set_Y(double Value);
public:

       // Make these vars available to COM interface
	double FXshift;
	double FYshift;
	double FXscale;
	double FYscale;
	TXYcurveObj(DSSClass::TDSSClass* ParClass, const String XYCurveName);
	virtual ~TXYcurveObj();
	double GetYValue_(double X);  // Get Y value at specified X Value
	double GetXValue(double Y);  // Get X value at specified Y Value
	TCoeff GetCoefficients(double X);
	virtual string GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual void SaveWrite(System::TTextRec& f);
	int get_FNumPoints();

	//TXYcurveObj(DSSClass::TDSSClass* ParClass, const string XYCurveName);
	TXYcurveObj(string ClassName);
	TXYcurveObj();
};
extern TXYcurveObj* ActiveXYcurveObj;


}  // namespace XYCurve

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace XYCurve;
#endif

#endif // XYCurveH





