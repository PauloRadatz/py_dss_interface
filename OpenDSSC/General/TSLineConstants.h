#ifndef TSLineConstantsH
#define TSLineConstantsH

#include "System.h"
#include "Sysutils.h"
#include <math.h>

#include "Arraydef.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LineUnits.h"
#include "LineConstants.h"
#include "CableConstants.h"

namespace TSLineConstants
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TTSLineConstants : public CableConstants::TCableConstants
{
public:
	typedef CableConstants::TCableConstants inherited;	
//private:
	Arraydef::pDoubleArray FDiaShield;
	Arraydef::pDoubleArray FTapeLayer;
	Arraydef::pDoubleArray FTapeLap;
	double Get_DiaShield(int i, int Units);
	double Get_TapeLayer(int i, int Units);
	double Get_TapeLap(int i);
	void Set_DiaShield(int i, int Units, double Value);
	void Set_TapeLayer(int i, int Units, double Value);
	void Set_TapeLap(int i, double Value);
protected:
public:
	virtual void Calc(double f);
	TTSLineConstants(int NumConductors);
	virtual ~TTSLineConstants();
	TTSLineConstants();
};


}  // namespace TSLineConstants

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace TSLineConstants;
#endif

#endif // TSLineConstantsH





