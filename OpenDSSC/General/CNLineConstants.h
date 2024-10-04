#ifndef CNLineConstantsH
#define CNLineConstantsH

#include "System.h"
#include "Sysutils.h"

#include "Arraydef.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LineUnits.h"
#include "LineConstants.h"
#include "CableConstants.h"

namespace CNLineConstants
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TCNLineConstants : public CableConstants::TCableConstants
{
public:
	typedef CableConstants::TCableConstants inherited;	
//private:
	Arraydef::pIntegerArray FkStrand;
	Arraydef::pDoubleArray FDiaStrand;
	Arraydef::pDoubleArray FGmrStrand;
	Arraydef::pDoubleArray FRStrand;
	int Get_kStrand(int i);
	double Get_DiaStrand(int i, int Units);
	double Get_GmrStrand(int i, int Units);
	double Get_RStrand(int i, int Units);
	void Set_kStrand(int i, int Value);
	void Set_DiaStrand(int i, int Units, double Value);
	void Set_GmrStrand(int i, int Units, double Value);
	void Set_RStrand(int i, int Units, double Value);
protected:
public:
	virtual void Calc(double f);
	TCNLineConstants(int NumConductors);
	virtual ~TCNLineConstants();
	TCNLineConstants();
};


}  // namespace CNLineConstants

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CNLineConstants;
#endif

#endif // CNLineConstantsH





