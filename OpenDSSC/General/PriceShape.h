#ifndef PriceShapeH
#define PriceShapeH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Arraydef.h"


namespace PriceShape
{



/*
  ----------------------------------------------------------
  Copyright (c) 2011-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*  2-16-2011 Converted from TempShape.
   Price shapes would generally be defined to correspond to loadshapes

*/

/*The PriceShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly, daily, and other Price shapes.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values may be retrieved by setting the Code Property in the PriceShape Class.
 This sets the active PriceShape object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActivePriceShapeObj object and save the direct reference to the object.

 PriceShapes default to fixed interval data (like Loadshapes).  If the Interval is specified to be 0.0,
 then both time and price data are expected.  If the Interval is  greater than 0.0,
 the user specifies only the prices.  The Hour command is ignored and the files are
 assumed to contain only the price data.

 The Interval may also be specified in seconds (sinterval) or minutes (minterval).

 The user may place the data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 For fixed interval data, only the price values are expected.  Therefore, the CSV format would
 contain only one number per line.  The two binary formats are packed.

 For variable interval data, (hour, price) pairs are expected in both formats.

 The Mean and Std Deviation are automatically computed upon demand when new series of points is entered.



 */

class TPriceShape : public DSSClass::TDSSClass
{
	friend class TPriceShapeObj;
public:
	typedef DSSClass::TDSSClass inherited;	
private:
	String Get_Code();  // Returns active PriceShape String
	void Set_Code(const String Value);  // sets the  active PriceShape
	void DoCSVFile(const String FileName);
	void DoSngFile(const String FileName);
	void DoDblFile(const String FileName);
protected:
	void DefineProperties();
	virtual int MakeLike(const String ShapeName);
public:
	TPriceShape();
	virtual ~TPriceShape();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	virtual void* Find(const String ObjName);  // Find an obj of this class by name
	void TOPExport(String ObjName); // can export this to top for plotting

       // Set this property to point ActivePriceShapeObj to the right value
};

class TPriceShapeObj : public DSSObject::TDSSObject
{
	friend class TPriceShape;
public:
	typedef DSSObject::TDSSObject inherited;	
private:
	int LastValueAccessed;
	int FNumPoints;  // Number of points in curve
	int ArrayPropertyIndex;
	bool FStdDevCalculated;
	double FMean;
	double FStdDev;
	double Get_Interval();
	void Set_NumPoints(int Value);
	void SaveToDblFile();
	void SaveToSngFile();
	void CalcMeanandStdDev();
	double Get_Mean();
	double Get_StdDev();
	void Set_Mean(double Value);
	void Set_StdDev(double Value);  // Normalize the curve presently in memory
public:
	double Interval;  //=0.0 then random interval     (hr)
          // Time values (hr) if Interval > 0.0  Else nil
	Arraydef::pDoubleArray Hours;
	Arraydef::pDoubleArray PriceValues;  // Prices
	TPriceShapeObj(DSSClass::TDSSClass* ParClass, const String PriceShapeName);
	virtual ~TPriceShapeObj();
	double GetPrice(double hr);  // Get Prices at specified time, hr
	double Price(int i);  // get Prices by index
	double Hour(int i);  // get hour corresponding to point index
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	int get_FNumPoints();

	TPriceShapeObj(DSSClass::TDSSClass* ParClass);
	TPriceShapeObj(String ClassName);
	TPriceShapeObj();
};
extern TPriceShapeObj* ActivePriceShapeObj;


}  // namespace PriceShape

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PriceShape;
#endif

#endif // PriceShapeH





