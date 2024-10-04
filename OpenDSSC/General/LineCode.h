#ifndef LineCodeH
#define LineCodeH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Ucmatrix.h"
#include "Arraydef.h"


namespace LineCode
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*The Linecode object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LineCode Class.
 This sets the active Linecode object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.

 */

class TLineCode : public DSSClass::TDSSClass
{
	friend class TLineCodeObj;
public:
	typedef DSSClass::TDSSClass inherited;	
private:
	bool SymComponentsChanged;
	bool MatrixChanged;
	string Get_Code();  // Returns active line code string
	void Set_Code(const String Value);  // sets the  active linecode
	void SetZ1Z0(int i, double Value);
	void SetUnits(const String s);  // decode units specification
	void DoMatrix(int i, int ActorID);  // set impedances as matrices
protected:
	void DefineProperties();
	virtual int MakeLike(const String LineName);
public:
	TLineCode();
	virtual ~TLineCode();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TLineCodeObj : public DSSObject::TDSSObject
{
	friend class TLineCode;
public:
	typedef DSSObject::TDSSObject inherited;	
//private:
	int FNeutralConductor;
	void Set_NPhases(int Value);
	void DoKronReduction();
	string get_Rmatrix();
	string get_Xmatrix();
	string get_CMatrix();
public:
	int NumAmpRatings;
	int Fnphases;
	bool SymComponentsModel;
	bool ReduceByKron;         // Base Frequency Series Z matrix
	Ucmatrix::TcMatrix* Z;
	Ucmatrix::TcMatrix* Zinv;
	Ucmatrix::TcMatrix* YC;  // Shunt capacitance matrix at Base frequency.
	double BaseFrequency;
	double R1;
	double X1;
	double R0;
	double X0;
	double C1;
	double C0;
	double NormAmps;
	double EmergAmps;
	double FaultRate;
	double PctPerm;
	double HrsToRepair;
	double Rg;
	double Xg;
	double rho;
	Arraydef::TRatingsArray AmpRatings;
	int FLineType; // Pointer to code for type of line
	int Units;  /*See LineUnits*/

	int get_Fnphases();

	TLineCodeObj(DSSClass::TDSSClass* ParClass, const String LineCodeName);
	virtual ~TLineCodeObj();
	void CalcMatricesFromZ1Z0();
	virtual string GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TLineCodeObj(DSSClass::TDSSClass* ParClass);
	TLineCodeObj(String ClassName);
	TLineCodeObj();
};
extern TLineCode* LineCodeClass;
extern TLineCodeObj* ActiveLineCodeObj;


}  // namespace LineCode

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace LineCode;
#endif

#endif // LineCodeH





