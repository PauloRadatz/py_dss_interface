#ifndef ConductorDataH
#define ConductorDataH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Arraydef.h"
#include "ParserDel.h"

#include "DSSClassDefs.h"
#include "Ucomplex.h"
#include "LineUnits.h"


namespace ConductorData
{

const int NumConductorClassProps = 13;

/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*The ConductorData object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the ConductorData Class.
 This sets the active ConductorData object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.
 */
enum ConductorChoice {Overhead,
                      ConcentricNeutral,
                      TapeShield,
                      unknown };
typedef ConductorChoice* ConductorChoiceArray;
typedef ConductorChoiceArray pConductorChoiceArray;

class TConductorData : public TDSSClass
{
	friend class TConductorDataObj;
public:
	typedef TDSSClass inherited;	
//private:
//protected:
	void CountProperties();
	void DefineProperties();
	int ClassEdit(const void* activeObj, int ParamPointer);
	void ClassMakeLike(const void* OtherObj);
public:
	TConductorData();
	virtual ~TConductorData();
};

class TConductorDataObj : public TDSSObject
{
	friend class TConductorData;
public:
	typedef TDSSObject inherited;	
//private:
	double FRDC;
	double FR60;
	double FGMR60;
	double Fcapradius60;  // in case it is different than radius for cap calcs
	double Fradius;
	int FGMRUnits;
	int FResistanceUnits;
	int FRadiusUnits;
public:
	double NormAmps;
	double EmergAmps;
	int NumAmpRatings;
	Arraydef::TRatingsArray AmpRatings;
	TConductorDataObj(TDSSClass* ParClass, const String ConductorDataName);
	virtual ~TConductorDataObj();

	double get_FRDC();
	double get_FR60();
	double get_FGMR60();
	double get_Fcapradius60();
	double get_Fradius();
	int get_FResistanceUnits();
	int get_FRadiusUnits();
	int get_FGMRUnits();

	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	virtual String GetPropertyValue(int Index);
	virtual int GetNumProperties(int ArrayOffset);
	TConductorDataObj(TDSSClass* ParClass);
	TConductorDataObj(String ClassName);
	TConductorDataObj();
};
typedef TConductorDataObj* TConductorDataArray;
typedef std::vector <TConductorDataArray> pConductorDataArray;
extern TConductorDataObj* ActiveConductorDataObj;


}  // namespace ConductorData

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace ConductorData;
#endif

#endif // ConductorDataH





