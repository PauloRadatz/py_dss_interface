#ifndef MemoryMap_libH
#define MemoryMap_libH

#include "System.h"
#include "Sysutils.h"
#include "d2c_structures.h"

#include <math.h>


namespace MemoryMap_lib
{

/***************************Memory mapped files Library**************************
* This library was added to OpenDSS to handle the different functions related  *
* with mapping files into memory to accelerate the data manipulation process   *
* when including this device to the simulation                                 *
*                                                                              *
* last modification: 09-12-2022                                               *
********************************************************************************
*/
typedef double* DoubleArray1d;
typedef std::vector <double>* pDoubleArray1d;
typedef std::vector < vector <double> >* pDoubleArray2d;
typedef std::vector <String>* pStringArray1d;

TBytesStream* Create_Meter_Space(String Init_Str);
void WriteintoMemStr(TBytesStream* Mem_Space, String Content);
void WriteintoMem(TBytesStream* Mem_Space, double Content);
void CloseMHandler(TBytesStream* Mem_Space, const String Dest_Path, bool AppendFile);
void ReadMHandler(TBytesStream* Mem_Space, pDoubleArray2d X_axis, pStringArray1d Ylabels, pDoubleArray2d Y_axis);
void Write_String(TBytesStream* Mem_Space, const String Content);


}  // namespace MemoryMap_lib

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace MemoryMap_lib;
#endif

#endif // MemoryMap_libH





