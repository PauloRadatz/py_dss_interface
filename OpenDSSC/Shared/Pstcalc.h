#ifndef PstcalcH
#define PstcalcH

#include "System.h"
#include "Sysutils.h"
#include "Arraydef.h"
#include <math.h>


namespace Pstcalc
{



 /*
  ----------------------------------------------------------
  Copyright (c) 2011-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

// IEC868 FlickerMeter
// Adapted from Jeff Smith's original C++ code
// note: the scaling factor being used has been corrected.  It is scaling the output of Block 4 to
// 1.0 for the values given in Table 1 and Table 2. The last table in the std should be used for checking Pst
// 4/12/05 The meter is verified using Table 2 and Table 5 (Pinst and Pst tables)
// 7/28/05: This cpp is designed to receive a 6-cycle rms data stream from the dss and return Pst
// 8/3/05: Updated to receive single to 6-cycle rms data streams.  The rms window "DeltaT" determines which scaling factor to use
// 8/4/05: added back functionality to receive AC data.  Now these are 2 separate loops
// 8/31/11 Converted to Object Pascal
// 9/6/11  Removed PstAC code
// 1/22/2013 added RMS flickermeter implementation (temc)

// Note: allocates result array of doubles!!!
int PstRMS(Arraydef::pDoubleArray PstResult, Arraydef::pDoubleArray pVoltages, double Freqbase, int NcyclesperSample, int npts, int Lamp);
      // returns number of Pst elements computed  in PstResult array
      // That is the number of 10-minute intervals
      // will automatically clean up and reallocate PstStruct when this function is called
      // Init PstResult to Nil in calling routine.
      // Dispose of result in colling routine when done with it.

// input: N points of RMS voltage in pT, pRms
//        fBase (50 or 60) determines the weighting coefficients
//        vBase to normalize the RMS voltage to per-unit
//        pre-allocate pPst to hold the Pst results at 10-minute intervals
// output: pRms overwritten with Block 4 flicker output
//        pPst written with Block 5 Pst at 10-minute intervals
void FlickerMeter(int n, double FBase, double VBase, Arraydef::pSingleArray Pt, Arraydef::pSingleArray pRms, Arraydef::pSingleArray pPst);


}  // namespace Pstcalc

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Pstcalc;
#endif

#endif // PstcalcH




