#ifndef d2c_sysmathH
#define d2c_sysmathH

/*******************************************************************************

       Delphi2Cpp Library d2c_sysmath.h

       Copyright (c) 2011 Dr. Detlef Meyer-Eltz
       ALL RIGHTS RESERVED


       mathematical routines according to the Delphi system unit

	     Authorized for unlimited use in any Delphi2Cpp project.
	     
	     http://www.texttransformer.com/Delphi2Cpp_en.html


*******************************************************************************/

#include "d2c_systypes.h"


namespace System {


int64_t Round( long double d );
long double Frac( long double d );
int64_t Trunc( long double d );

/*
// from here on not contained in Delphi2Cpp trial
RandSeed;
Random
Randomize
Sqr
Pi
Sqr
Sqrt
ArcTan
Ln
Sin
Cos
Exp
Int
*/

} // namespace System

#endif