#ifndef LineH
#define LineH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "PDElement.h"
#include "Ucmatrix.h"
#include "LineCode.h"
#include "Arraydef.h"
#include "ConductorData.h"
#include "PDClass.h"
#include "Ucomplex.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "LineGeometry.h"
#include "LineSpacing.h"
#include "LineUnits.h"
#include "d2c_structures.h"


namespace Line
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*  3-1-00 Reactivated line dump
   3-13-03  Fixed bug where terminal quantities were not getting reallocated in FetchCondCode
   2018	    Added GIC stuff
   2022     Fixed bug on long-line correction to correct for frequencies other than base freq.
*/

class TLine : public PDClass::TPDClass
{
	friend class TLineObj;
public:
	typedef PDClass::TPDClass inherited;	
private:
	void DoRmatrix(int ActorID);
	void DoXmatrix(int ActorID);
	void DoCmatrix(int ActorID);
protected:
	void DefineProperties();  // Add Properties of this class to propName
	virtual int MakeLike(const String LineName);
public:
	TLine();
	virtual ~TLine();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TLineObj : public PDELement::TPDElement
{
	friend class TLine;
public:
	typedef PDELement::TPDElement inherited;	
public:
	double FZFrequency; // keep track of last frequency computed for geometry
	int FLineCodeUnits;
	double FUnitsConvert; // conversion factor
	LineGeometry::TLineGeometryObj* FLineGeometryObj;
	LineSpacing::TLineSpacingObj* FLineSpacingObj;
	ConductorData::pConductorDataArray FLineWireData;
	int FWireDataSize;
	ConductorData::ConductorChoice FPhaseChoice;
	bool FrhoSpecified;
	bool FLineCodeSpecified;
	int FEarthModel;
	bool FCapSpecified; // To make sure user specifies C in some form
	int FLineType; // Pointer to code for type of line
	int FUserLengthUnits; // keep track of the user's input length units
	bool FRatingsSpecified; // To track if ratings should be overriden from geometry + spacing spec
	void FMakeZFromGeometry(double f); // make new Z, Zinv, Yc, etc
	void KillGeometrySpecified();
	void FMakeZFromSpacing(double f); // make new Z, Zinv, Yc, etc
	void KillSpacingSpecified();
	void ClearYPrim();
	void ResetLengthUnits();
	void UpdatePDProperties();   // update inherited properties
	int NumConductorData();
	ConductorData::TConductorDataObj* FetchConductorData(int i);
	void ReallocZandYcMatrices();
	void DoLongLine(double Frequency);  // Long Line Correction for 1=phase
	void DoLongLine(double Frequency, double R, double X, double C, double& R_h, double& X_h, double& C_h, double& G_h);  // Long Line Correction for 1=phase
	void ConvertZinvToPosSeqR();  // for GIC analysis, primarily
protected:
	Ucmatrix::TcMatrix* Zinv;
public:     // Moved to make values available to the COM interface
	Ucmatrix::TcMatrix* Z;   // Base Frequency Series Z matrix  per unit length
	Ucmatrix::TcMatrix* YC;
	double R1;
	double X1;
	double R0;
	double X0;
	double C1;
	double C0;
	double Len;
	int LengthUnits;
	double Rg;
	double Xg;
	double KXg;
	double rho;
	double GeneralPlotQuantity;  // For general circuit plotting
	String CondCode;
	String GeometryCode;
	String SpacingCode;
	bool GeometrySpecified;
	bool SpacingSpecified;
	bool SymComponentsChanged;
	bool SymComponentsModel;
	bool IsSwitch;

//        NRatings             : Integer;           // See PDElement
//        ratings              : pDoubleArray;
	virtual void GetLosses(Ucomplex::complex& TotalLosses, Ucomplex::complex& LoadLosses, Ucomplex::complex& NoLoadLosses, int ActorID);
	virtual void GetSeqLosses(Ucomplex::complex& PosSeqLosses, Ucomplex::complex& NegSeqLosses, Ucomplex::complex& ZeroSeqLosses, int ActorID);
	TLineObj(TDSSClass* ParClass, const String LineName);
	virtual ~TLineObj();
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	bool MergeWith(TLineObj* OtherLine, bool SERIES);
	void UpdateControlElements(const String NewName, const String OldName);
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

        // Public for the COM Interface
	void FetchLineCode(const String Code);
	void FetchGeometryCode(const String Code);
	void FetchLineSpacing(const String Code);
	void FetchWireList(const String Code);
	void FetchCNCableList(const String Code);
	void FetchTSCableList(const String Code);

        // Reliability calcs
	virtual void CalcFltRate();  // Calc failure rates for section and buses

        // CIM XML access
	TLineObj(TDSSClass* ParClass);
	TLineObj(String ClassName);
	TLineObj();
};
extern TLineObj* ActiveLineObj;
extern LineGeometry::TLineGeometry* LineGeometryClass;  // public to show line constant results


}  // namespace Line

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Line;
#endif

#endif // LineH




