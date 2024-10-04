#ifndef OHLineConstantsH
#define OHLineConstantsH

#include "System.h"

#include "Arraydef.h"
#include "Ucmatrix.h"
#include "Ucomplex.h"
#include "LineUnits.h"
#include "LineConstants.h"

namespace OHLineConstants
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*Manages the geometry data and calculates the impedance matrices for an overhead line*/

class TOHLineConstants : public LineConstants::TLineConstants
{
public:
	typedef LineConstants::TLineConstants inherited;	
private:
protected:
public:
	TOHLineConstants(int NumConductors);
	virtual ~TOHLineConstants();
	TOHLineConstants();
};


}  // namespace OHLineConstants

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace OHLineConstants;
#endif

#endif // OHLineConstantsH





