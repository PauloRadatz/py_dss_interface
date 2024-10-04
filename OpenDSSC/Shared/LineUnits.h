#ifndef LineUnitsH
#define LineUnitsH

#include "System.h"
#include "Sysutils.h"


namespace LineUnits
{


 /*
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
const int UNITS_MAXNUM = 9;
const int UNITS_NONE = 0;
const int UNITS_MILES = 1;
const int UNITS_KFT = 2;
const int UNITS_KM = 3;
const int UNITS_M = 4;
const int UNITS_FT = 5;
const int UNITS_IN = 6;
const int UNITS_CM = 7;
const int UNITS_MM = 8;
int GetUnitsCode(const String s);
String LineUnitsStr(int Units);

// Conversion to and from meters and per meter
double To_Meters(int Units);
double To_per_Meter(int Units);
double From_per_Meter(int Units);
double From_Meters(int Units);
double ConvertLineUnits(int FromUnits, int ToUnits);


}  // namespace LineUnits

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace LineUnits;
#endif

#endif // LineUnitsH




