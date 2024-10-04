#ifndef LineGeometryH
#define LineGeometryH

#include "System.h"
#include "Sysutils.h"

#include "Arraydef.h"
#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "LineConstants.h"
#include "ConductorData.h"
#include "CNData.h"
#include "TSData.h"
#include "LineSpacing.h"
#include "Ucmatrix.h"
#include "d2c_structures.h"

namespace LineGeometry
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*The LineGeometry object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LineGeometry Class.
 This sets the active LineGeometry object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.

 */

class ELineGeometryProblem : public std::runtime_error
{
public:
	typedef std::runtime_error inherited;
	ELineGeometryProblem(const String &Msg);
};

class TLineGeometry : public DSSClass::TDSSClass
{
	friend class TLineGeometryObj;
public:
	typedef DSSClass::TDSSClass inherited;	
private:
	String Get_Code();  // Returns active line code String
	void Set_Code(const String Value);  // sets the  active LineGeometry
protected:
	void DefineProperties();
	virtual int MakeLike(const String LineName);
public:
	TLineGeometry();
	virtual ~TLineGeometry();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TLineGeometryObj : public DSSObject::TDSSObject
{
	friend class TLineGeometry;
public:
	typedef DSSObject::TDSSObject inherited;	
//private:
	ConductorData::pConductorChoiceArray FPhaseChoice;
	int Fnconds;
	int Fnphases;
	Arraydef::pStringArray FCondName;
	ConductorData::pConductorDataArray FWireData;
	std::vector <double> FX;
	std::vector <double> FY;
	std::vector <longInt> FUnits;
	int FLastUnit;
	bool DataChanged;
	bool FReduce;
	int FActiveCond;
	String FSpacingType;
	LineConstants::TLineConstants* FLineData;
	void ChangeLineConstantsType(ConductorData::ConductorChoice newPhaseChoice);
	void Set_Nconds(int Value);
	void Set_NPhases(int Value);
	void set_ActiveCond(int Value);
	Ucmatrix::TcMatrix* Get_YCmatrix(double f, double Lngth, int Units);
	Ucmatrix::TcMatrix* Get_Zmatrix(double f, double Lngth, int Units);
	double Get_RhoEarth();
	void Set_RhoEarth(double Value);
	int get_Nconds();
	void UpdateLineGeometryData(double f);   // call this before using the line data

        // CIM Accessors
	double Get_FX(int i);
	double Get_FY(int i);
	int Get_FUnits(int i);
	String Get_ConductorName(int i);
	ConductorData::TConductorDataObj* Get_ConductorData(int i);
	ConductorData::ConductorChoice Get_PhaseChoice(int i);
public:
	double NormAmps;
	double EmergAmps;
	int NumAmpRatings;
	Arraydef::TRatingsArray AmpRatings;
	int FLineType; // Pointer to code for type of line
	TLineGeometryObj(DSSClass::TDSSClass* ParClass, const String LineGeometryName);
	virtual ~TLineGeometryObj();
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual void SaveWrite(System::TTextRec& f);
	//------------------------functions for passing variables as properties------------------------------------
	int get_Fnphases();
	int get_FActiveCond();
	int get_Fnconds();

        // called from a Line object that has its own Spacing and Wires input
        // automatically sets reduce=y if the spacing has more wires than phases
	void LoadSpacingAndWires(LineSpacing::TLineSpacingObj* Spc, ConductorData::pConductorDataArray Wires);
        // CIM XML accessors
	TLineGeometryObj(DSSClass::TDSSClass* ParClass);
	TLineGeometryObj(String ClassName);
	TLineGeometryObj();
};
extern TLineGeometryObj* ActiveLineGeometryObj;

}  // namespace LineGeometry

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace LineGeometry;
#endif

#endif // LineGeometryH





