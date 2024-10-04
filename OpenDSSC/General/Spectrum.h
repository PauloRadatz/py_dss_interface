#ifndef SpectrumH
#define SpectrumH

#include "System.h"
#include "Sysutils.h"

#include "Command.h"
#include "DSSClass.h"
#include "DSSObject.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "d2c_structures.h"

namespace Spectrum
{


 /*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/* Created 10/25/00

   Harmonic Spectrum specified as Harmonic, pct magnitude and angle

   Spectrum is shifted by the fundamental angle and stored in MultArray
   so that the fundamental is at zero degrees phase shift

*/

class TSpectrum : public DSSClass::TDSSClass
{
	friend class TSpectrumObj;
public:
	typedef DSSClass::TDSSClass inherited;	
private:
	String Get_Code();  // Returns active spectrum code String
	void Set_Code(const String Value);  // sets the  active Spectrum
	void DoCSVFile(const String FileName);
protected:
	void DefineProperties();
	virtual int MakeLike(const String LineName);
public:
	TSpectrum();
	virtual ~TSpectrum();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int NewObject(const String ObjName);

       // Set this property to point ActiveSpectrumObj to the right value
};

class TSpectrumObj : public DSSObject::TDSSObject
{
	friend class TSpectrum;
public:
	typedef DSSObject::TDSSObject inherited;	
private:
	std::vector <double> puMagArray;
	std::vector <double> AngleArray;
	std::vector <complex> MultArray;
	void SetMultArray();
	bool HarmArrayHasaZero(int& zeropoint);
public:
	int NumHarm;          // Public so solution can get to it.
	std::vector <double> HarmArray;
	TSpectrumObj(DSSClass::TDSSClass* ParClass, const String SpectrumName);
	virtual ~TSpectrumObj();
	Ucomplex::complex GetMult(double h);
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);
	TSpectrumObj(DSSClass::TDSSClass* ParClass);
	TSpectrumObj(String ClassName);
	TSpectrumObj();
};
extern TSpectrumObj* ActiveSpectrumObj;


}  // namespace Spectrum

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Spectrum;
#endif

#endif // SpectrumH





