#ifndef GrowthShapeH
#define GrowthShapeH

#include "System.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Ucmatrix.h"
#include "Arraydef.h"


namespace GrowthShape
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*  8-18-00 Added call to InterpretDblArrayto allow File=Syntax */
/*The GrowthShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly growth curves.

 The values are set by the normal New and Edit procedures as for any DSS object.

 The values are retrieved by setting the Code Property in the GrowthShape Class.
 This sets the active GrowthShapeObj object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveGrowthShapeObj object and save the direct reference to the object.

 Growth shapes are entered as multipliers for the previous year's load.  If the
 load grows by 2.5% in a year, the multiplier is entered as 1.025.  You do not need
 to enter subsequent years if the multiplier remains the same.  You need only enter
 the years in which the growth rate is assumed to have changed.

 The user may place the data in CSV or binary files as well as passing through the
 command interface. The rules are the same as for LoadShapes except that the year
 is always entered.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 (Year, multiplier) pairs are expected in all formats.  Through the COM interface,
 supply separate arrays of Year and Mult.

 Edit growthshape.allisonsub npts=5
 ~   year="1999 2000 2001 2005 2010"
 ~   mult="1.10 1.07 1.05 1.025 1.01"

 This example describes a growth curve that start off relatively fast (10%) and after
 10 years tapers off to 1%

 */

class TGrowthShape : public DSSClass::TDSSClass
{
	friend class TGrowthShapeObj;
public:
	typedef DSSClass::TDSSClass inherited;	
//private:
	String Get_Code();  // Returns active GrowthShape String
	void Set_Code(const String Value);  // sets the  active GrowthShape
	void DoCSVFile(const String FileName);
	void DoSngFile(const String FileName);
	void DoDblFile(const String FileName);
protected:
	void DefineProperties();
	virtual int MakeLike(const String ShapeName);
public:
	TGrowthShape();
	virtual ~TGrowthShape();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);

       // Set this property to point ActiveGrowthShapeObj to the right value
};

class TGrowthShapeObj : public DSSObject::TDSSObject
{
	friend class TGrowthShape;
public:
	typedef DSSObject::TDSSObject inherited;	
//private:
	int npts;  // Number of points in curve
	int NYears;    // Number of years presently allocated in look up table
	int BaseYear;
	Arraydef::pIntegerArray Year;          // Year values
	Arraydef::pDoubleArray YearMult;
	Arraydef::pDoubleArray Multiplier;  // Multipliers
	void ReCalcYearMult();
public:
	TGrowthShapeObj(DSSClass::TDSSClass* ParClass, const String GrowthShapeName);
	virtual ~TGrowthShapeObj();
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	double GetMult(int Yr);  // Get multiplier for Specified Year
	TGrowthShapeObj(DSSClass::TDSSClass* ParClass);
	TGrowthShapeObj(String ClassName);
	TGrowthShapeObj();
};
extern TGrowthShapeObj* ActiveGrowthShapeObj;


}  // namespace GrowthShape

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace GrowthShape;
#endif

#endif // GrowthShapeH





