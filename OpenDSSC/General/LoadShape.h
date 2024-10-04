#ifndef LoadShapeH
#define LoadShapeH

#include <cstdint> // uint8_t
#include "System.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "Arraydef.h"
#include "d2c_structures.h"

namespace LoadShape
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*  8-18-00 Added call to InterpretDblArrayto allow File=Syntax */

/*The LoadShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly, daily, and other load shapes.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LoadShape Class.
 This sets the active LoadShape object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveLoadShapeObj object and save the direct reference to the object.

 Loadshapes default to fixed interval data.  If the Interval is specified to be 0.0,
 then both time and multiplier data are expected.  If the Interval is  greater than 0.0,
 the user specifies only the multipliers.  The Hour command is ignored and the files are
 assumed to contain only the multiplier data.

 The user may place the data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 For fixed interval data, only the multiplier is expected.  Therefore, the CSV format would
 contain only one number per line.  The two binary formats are packed.

 For variable interval data, (hour, multiplier) pairs are expected in both formats.

 The Mean and Std Deviation are automatically computed upon demand when new series of points is entered.

 The data may also be entered in unnormalized form.  The normalize=Yes command will force normalization.  That
 is, the multipliers are scaled so that the maximum value is 1.0.

 */

class TLoadShape : public TDSSClass
{
	friend class TLoadShapeObj;
public:
	typedef TDSSClass inherited;	
private:
	string Get_Code();  // Returns active LoadShape string
	void Set_Code(const String Value);  // sets the  active LoadShape
	void DoCSVFile(const String FileName);
	void Do2ColCSVFile(const String FileName);  // for P and Q pairs
	void DoSngFile(const String FileName);
	void DoDblFile(const String FileName);
protected:
	void DefineProperties();
	virtual int MakeLike(const String ShapeName);
public:
	TLoadShape();
	virtual ~TLoadShape();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	virtual void* Find(const String ObjName);  // Find an obj of this class by name
	void TOPExport(String ObjName);
	int CreateMMF(const String s, int Destination);
};

class TLoadShapeObj : public DSSObject::TDSSObject
{
	friend class TLoadShape;
public:
	typedef DSSObject::TDSSObject inherited;	
//private:
	int LastValueAccessed;
	int FNumPoints;  // Number of points in curve
	int ArrayPropertyIndex;
	int iMaxP;
	bool FStdDevCalculated;
	bool MaxQSpecified;
	double FMean;
	double FStdDev;

        // Function Get_FirstMult:Double;
        // Function Get_NextMult :Double;
	double Get_Interval();
	void Set_NumPoints(int Value);
	void SaveToDblFile();
	void SaveToSngFile();
	void CalcMeanandStdDev();
	double Get_Mean();
	double Get_StdDev();
	void Set_Mean(double Value);
	void Set_StdDev(double Value);  // Normalize the curve presently in memory
	void SetMaxPandQ();
public:
	double Interval;  //=0.0 then random interval     (hr)
          // Time values (hr) if Interval > 0.0  Else nil
	std::vector <double> Hours;
	std::vector <double> PMultipliers;
	std::vector <double> QMultipliers;  // Multipliers
	double MaxP;
	double MaxQ;
	double BaseP;
	double BaseQ;
	bool Enabled;
	bool UseActual;

	/************************************************************************
	*                    Memory mapping variables                          *
	*************************************************************************/
	bool UseMMF;            // Flag to indicated that the user wants to use MMF
#ifdef _WIN32
	HANDLE myMMF;              // Handle for the memory map (P)
	HANDLE myFile;             // Handle for the file to be mapped (P)
	HANDLE myQMMF;             // Handle for the memory map (Q)
	HANDLE myQFile;            // Handle for the file to be mapped (Q)
#else
	int myFile;             // Handle for the file to be mapped (P)
	int myQFile;            // Handle for the file to be mapped (Q)
#endif

	unsigned int myFileSizeQ;          // File size of the file opened (Q)
	unsigned int myFileSize;           // File size of the file opened (P)
	string myFileCmdQ;
	string myFileCmd;             // The file definition added by the user (for moving the data window)
                                  // Current view of the file mapped (Bytes - Q)
	uint8_t* myViewQ;
	uint8_t* myView;              // Current view of the file mapped (Bytes - P)
                               // The file type (P)
                              // The file type (Q)
                                 // The column to read (P)
                                // The column to read (Q)
                                // The size of the char line (P)
                               // The size of the char line (Q)
                               // The total data size expected (P)
                              // The total data size expected (Q)
                               // Memory View size in bytes (Q)
	int myFileType;
	int myFileTypeQ;
	int myColumn;
	int myColumnQ;
	int myLineLen;
	int myLineLenQ;
	int myDataSize;
	int myDataSizeQ;
	int MyViewLenQ;
	int MyViewLen;            // Memory View size in bytes (P)

        //**********************************************************************
	TLoadShapeObj(DSSClass::TDSSClass* ParClass, const string LoadShapeName);
	virtual ~TLoadShapeObj();
	Ucomplex::complex GetMult(double hr);           // Get multiplier at specified time
	double Mult(int i);               // get multiplier by index
	double Hour(int i);               // get hour corresponding to point index
	void Normalize();
        // Loads the current view of the MMF into memory for further use
	void LoadMMFView(const string Parmname, int Destination);
	void LoadFileFeatures(int ShapeType);
	virtual string GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	int get_FNumPoints();

        /*Property FirstMult :Double Read Get_FirstMult;*/
        /*Property NextMult  :Double Read Get_NextMult;*/
	TLoadShapeObj(DSSClass::TDSSClass* ParClass);
	TLoadShapeObj(string ClassName);
	TLoadShapeObj();
	string mode;
};
extern TLoadShapeObj* ActiveLoadShapeObj;


}  // namespace LoadShape

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace LoadShape;
#endif

#endif // LoadShapeH





