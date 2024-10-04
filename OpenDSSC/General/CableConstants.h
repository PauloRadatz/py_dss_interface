#ifndef CableConstantsH
#define CableConstantsH

#include "System.h"
#include "Sysutils.h"

#include "Arraydef.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LineUnits.h"
#include "LineConstants.h"
#include "d2c_structures.h"

namespace CableConstants
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TCableConstants : public LineConstants::TLineConstants
{
public:
	typedef LineConstants::TLineConstants inherited;	
//private:
	double Get_EpsR(int i);
	double Get_InsLayer(int i, int Units);
	double Get_DiaIns(int i, int Units);
	double Get_DiaCable(int i, int Units);
	void Set_EpsR(int i, double Value);
	void Set_InsLayer(int i, int Units, double Value);
	void Set_DiaIns(int i, int Units, double Value);
	void Set_DiaCable(int i, int Units, double Value);
//protected:
	Arraydef::pDoubleArray FEpsR;
	Arraydef::pDoubleArray FInsLayer;
	Arraydef::pDoubleArray FDiaIns;
	Arraydef::pDoubleArray FDiaCable;
public:
	virtual bool ConductorsInSameSpace(String& ErrorMessage);
	virtual void Kron(int Norder); // don't reduce Y, it has zero neutral capacitance
	TCableConstants(int NumConductors);
	virtual ~TCableConstants();
	TCableConstants();
};


}  // namespace CableConstants

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CableConstants;
#endif

#endif // CableConstantsH





