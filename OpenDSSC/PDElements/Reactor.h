#ifndef ReactorH
#define ReactorH

#include "System.h"

#include "Command.h"
#include "DSSClass.h"
#include "PDClass.h"
#include "PDElement.h"
#include "Ucomplex.h"
#include "Ucmatrix.h"
#include "Arraydef.h"
#include "d2c_structures.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "Sysutils.h"
#include "XYcurve.h"
#include "mathutil.h"



namespace Reactor
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*   10-26-00  Created from Capacitor  object
    3-2-06 Added Parallel Option and corrected frequency adjustments
           RMATRIX, Xmatrix untested
    2013   Added Symmetrical component specification and frequency-dependence for simplr
           R+jX model


Basic  Reactor

  Uses same rules as Capacitor and Fault for connections

  Implemented as a two-terminal constant impedance (Power Delivery Element)
  Defaults to a Shunt Reactor but can be connected as a two-terminal series reactor

  If Parallel=Yes, then the R and X components are treated as being in parallel

  Bus2 connection defaults to 0 node of Bus1 (if Bus2 has the default bus connection
  at the time Bus1 is defined.  Therefore, if only Bus1 is specified, a shunt Reactor results.
  If delta connected, Bus2 is set to node zero of Bus1 and nothing is returned in the lower
  half of YPrim - all zeroes.

  If an ungrounded wye is desired, explicitly set Bus2= and set all nodes the same,
    e.g. Bus1.4.4.4   (uses 4th node of Bus1 as neutral point)
        or BusNew.1.1.1  (makes a new bus for the neutral point)
  You must specify the nodes or you will get a series Reactor!

  A series Reactor is specified simply by setting bus2 and declaring the connection
  to be Wye.  If the connection is specified as delta, nothing will be connected to Bus2.
  In fact the number of terminals is set to 1.

  Reactance may be specified as:

     1.  kvar and kv ratings at base frequency.  impedance.  Specify kvar as total for
         all phases. For 1-phase, kV = Reactor coil kV rating.
         For 2 or 3-phase, kV is line-line three phase. For more than 3 phases, specify
         kV as actual coil voltage.
     2.  Series Resistance, R, and Reactance, X, in ohns at base frequency to be used in each phase.  If specified in this manner,
         the given value is always used whether wye or delta.  X may be specified as Inductance, LmH, in mH.
         The Z property may also be used to specify R and X in an array.
     3.  A R and X  matrices .
         If conn=wye then 2-terminal through device
         If conn=delta then 1-terminal.
         Ohms at base frequency
         Note that Rmatix may be in parallel with Xmatric (set parallel = Yes)
     4.  As symmetrical component values using Z1, Z2, and Z0 complex array properties.
         Z2 defaults to Z1, but can be set to a different value.

*/

class TReactor : public PDClass::TPDClass
{
	friend class TReactorObj;
public:
	typedef PDClass::TPDClass inherited;	
private:
	void DoMatrix(Arraydef::pDoubleArray Matrix, int ActorID);
	void InterpretConnection(const String s);
	void ReactorSetbus1(const String s);
protected:
	virtual int MakeLike(const String ReactorName);
	void DefineProperties();  // Add Properties of this class to propName
public:
	TReactor();
	virtual ~TReactor();
	virtual int Edit(int ActorID);     // uses global parser
	virtual int Init(int Handle, int ActorID);
	virtual int NewObject(const String ObjName);
};

class TReactorObj : public PDELement::TPDElement
{
	friend class TReactor;
public:
	typedef PDELement::TPDElement inherited;	
//private:
	double R;
	double Rp;
	double Gp;
	double X;
	double l;
	double kvarrating;
	double kvrating;
	Ucomplex::complex Z;
	Ucomplex::complex Z1;
	Ucomplex::complex Z2;
	Ucomplex::complex Z0;
	Arraydef::pDoubleArray Rmatrix;
	Arraydef::pDoubleArray Gmatrix;
	Arraydef::pDoubleArray XMatrix;
	Arraydef::pDoubleArray Bmatrix;  // If not nil then overrides C
	int Connection;   // 0 or 1 for wye (default) or delta, respectively
	int SpecType;   // 1=kvar, 2=R+jX, 3=R and X matrices, 4=sym components
	bool IsParallel;
	bool RpSpecified;
	bool Bus2Defined;
	bool Z2Specified;
	bool Z0Specified;

	bool FNormAmpsSpecified;
	bool FEmergAmpsSpecified;
public:
	String RCurve;
	XYCurve::TXYcurveObj* RCurveObj;
	String LCurve;
	XYCurve::TXYcurveObj* LCurveObj;
	TReactorObj(TDSSClass* ParClass, const String ReactorName);
	virtual ~TReactorObj();
	virtual void GetLosses(Ucomplex::complex& TotalLosses, Ucomplex::complex& LoadLosses, Ucomplex::complex& NoLoadLosses, int ActorID);
	virtual void MakePosSequence(int ActorID);  // Make a positive Sequence Model
	virtual void RecalcElementData(int ActorID);
	virtual void CalcYPrim(int ActorID);
	virtual String GetPropertyValue(int Index);
	virtual void InitPropertyValues(int ArrayOffset);
	virtual void DumpProperties(System::TTextRec& f, bool Complete);

				// CIM XML access - this is only tested for the IEEE 8500-node feeder
	TReactorObj(DSSClass::TDSSClass* ParClass);
	TReactorObj(String ClassName);
	TReactorObj();
};
extern TReactorObj* ActiveReactorObj;


}  // namespace Reactor

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Reactor;
#endif

#endif // ReactorH




