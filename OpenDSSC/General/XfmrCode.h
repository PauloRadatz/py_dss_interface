#ifndef XfmrCodeH
#define XfmrCodeH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Ucmatrix.h"
#include "Arraydef.h"
#include "Transformer.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"

#include "Ucomplex.h"



namespace XfmrCode
{


/*
  ----------------------------------------------------------
  Copyright (c) 2009-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
enum WdgParmChoice {Conn,
                    kV,
                    kVA,
                    R,
                    Tap };

class TXfmrCode : public TDSSClass
{
	friend class TXfmrCodeObj;
public:
	typedef TDSSClass inherited;	
private:
	String Get_Code();
	void SetActiveWinding(int W);
	void InterpretWindings(const String s, WdgParmChoice Which);
protected:
	void DefineProperties();
	virtual int MakeLike(const String Name);
public:
	TXfmrCode();
	virtual ~TXfmrCode();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
	void Set_Code(const String Value);

       // Set this property to point ActiveXfmrCodeObj to the right value
};

class TXfmrCodeObj : public DSSObject::TDSSObject
{
	friend class TXfmrCode;
public:
	typedef DSSObject::TDSSObject inherited;	
	int Fnphases;
	int ActiveWinding;
	int NumWindings;
	int MaxWindings;
	double XHL;
	double XHT;
	double XLT;  // per unit
	std::vector<double> XSC;     // per unit SC measurements
	double VABase;    // FOR impedances
	double NormMaxHkVA;
	double EmergMaxHkVA;
	double ThermalTimeConst;  /*hr*/
	double n_thermal;
	double m_thermal;  /*Exponents*/
	double FLrise;
	double HSrise;
	double pctLoadLoss;
	double pctNoLoadLoss;
	double ppm_FloatFactor; //  parts per million winding float factor
	double pctImag;
	std::vector<TWinding> WINDING_;
	int NumAmpRatings;
	Arraydef::TRatingsArray AmpRatings;
	void SetNumWindings(int n);
	void PullFromTransformer(Transformer::TTransfObj* Obj);
	TXfmrCodeObj(DSSClass::TDSSClass* ParClass, const String XfmrCodeName);
	virtual ~TXfmrCodeObj();
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TXfmrCodeObj(DSSClass::TDSSClass* ParClass);
	TXfmrCodeObj(String ClassName);
	TXfmrCodeObj();
};
extern TXfmrCodeObj* ActiveXfmrCodeObj;


}  // namespace XfmrCode

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace XfmrCode;
#endif

#endif // XfmrCodeH





