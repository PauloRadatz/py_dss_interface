#ifndef ArraydefH
#define ArraydefH

#include "System.h"
#include <string>
#include <vector>


namespace Arraydef
{


 /*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

typedef std::vector <double> TRatingsArray;
/* Define arrays with dummy dimension of 100 so we can hard code
  constants for accessing small order elements;  Otherwise, always
  allocate arrays of these types before using*/
typedef std::vector < short int > SmallIntArray;
typedef short int* pSmallIntArray;
typedef int32_t longInt;
typedef std::vector < longInt > LongIntArray;
typedef longInt* pLongIntArray;
typedef longInt* pIntegerArray;
typedef std::vector < double > DoubleArray;
typedef double* pDoubleArray;
typedef std::vector < float > SingleArray;
typedef float* pSingleArray;
typedef void * PointerArray;
typedef PointerArray* PPointerArray;
typedef std::vector < std::string > StringArray;
typedef std::string* pStringArray;
typedef std::vector <std::string> DynStringArray;
typedef std::string* pDynStringArray;
typedef double* PDouble;
typedef float* PSingle;
typedef short int* PSmallInt;
typedef longInt* PLongInt;
typedef std::vector<double> DynSlot;
pStringArray AllocStringArray(int Size);
void FreeStringArray(pStringArray &ps, int Size);
/*--------------------------------------------------------------------------*/


}  // namespace Arraydef

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Arraydef;
#endif

#endif // ArraydefH




