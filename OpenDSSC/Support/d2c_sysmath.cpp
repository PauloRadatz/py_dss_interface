//#include "stdafx.h"

/*******************************************************************************

       Delphi2Cpp Library d2c_sysmath.h

       Copyright (c) 2011 Dr. Detlef Meyer-Eltz
       ALL RIGHTS RESERVED


       mathematical routines according to the Delphi system unit

	     Authorized for unlimited use in any Delphi2Cpp project.
	     
	     http://www.texttransformer.com/Delphi2Cpp_en.html


*******************************************************************************/

#include "d2c_sysmath.h"
#include <math.h>
#include <stdlib.h>


namespace System
{

//---------------------------------------------------------------------------
int64_t Round( long double X )
{
  return (int64_t) ( X < 0 ? X -.5 : X + .5) ;  //floor(X + 0.5);
}
//---------------------------------------------------------------------------
long double Frac( long double X )
{
  return X - int(X);
}
//---------------------------------------------------------------------------
int64_t Trunc( long double X )
{
  long double ld;
  modfl(X, &ld);
  return (int64_t) ld;
}

}  // namespace System
