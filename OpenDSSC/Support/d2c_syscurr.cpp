//#include "stdafx.h"

/*
    Copyright of the basic file version
    -----------------------------------

    Explanation: in the following license "library" means
    the following files: 
    
    System.pas / d2c_system.pas
    System.h
    d2c_sysconst.h
    d2c_syscurr.h d2c_syscurr.cpp
    d2c_sysdate.h d2c_sysdate.cpp
    d2c_sysexcept.h d2c_sysexcept.cpp
    d2c_sysfile.h, d2c_sysfile.cpp 
    d2c_sysmath.h d2c_sysmath.cpp
    d2c_sysstring.h d2c_sysstring.cpp
    d2c_systypes.h
    d2c_varrec.h
    d2c_smallstring.h
    Windows.pas
    Sysutils.pas
    Sysutils.h Sysutils.cpp

    AS THEY ARE CONTAINED IN THE FREE TRIAL VERSION OF Delphi2Cpp.

    This library is derived from the FreePascal library:

    http://www.freepascal.org/

    FreePascal is published under the terms of GNU Lesser General
    Public License and the same terms apply to this library.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with d2c_sysfile.h/cpp; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA



    Copyright of the extended file version
    --------------------------------------


    The terms of the license above don't apply to extended versions
    of these files, which are distributed with commercial versions of 
    Delphi2Cpp. Individual licenses are applied to them. 
    The library doesn't depend on the commercial extensions and the 
    the commercial extensions only originates from the author
    Dr. Detlef Meyer-Eltz or might use code which has no copyright restrictions

    Copyright (C) <2011>  <Dr. Detlef Meyer-Eltz>

    The extended version of this file is authorized for unlimited use in any 
    Delphi2Cpp project.
	     
	  http://www.texttransformer.com/Delphi2Cpp_en.html

*/

#include "d2c_syscurr.h"
#include "d2c_sysconst.h"
#include "d2c_sysexcept.h"
#include "Sysutils.h"
#include "d2c_sysexcept.h"


using namespace System;

Currency MinCurrency = - 922337203685477.0000;
Currency MaxCurrency = 922337203685477.0000;

Currency::Currency() 
  : Val(0) {}
	  
Currency::Currency(double val)              
{
  Val = static_cast<int64_t>(10000 * val);
}
	  
Currency::Currency(int val)                 
{
  Val = 10000*(int64_t)val;
}
	  
Currency::Currency(const Currency& src)     
{
  Val = src.Val;
}
	  
Currency::Currency(const String& src)
{
  *this = StrToCurr( src );
}

Currency& Currency::operator =(double rhs)
{
	Val = static_cast<int64_t>(10000 * rhs);
  return *this;
}
	  
Currency& Currency::operator =(int rhs)
{
  Val = 10000*(int64_t) rhs; 
  return *this;
}
	  
Currency& Currency::operator =(const Currency& rhs)
{
  Val = rhs.Val; 
  return *this;
}

Currency Currency::operator -() const
{
  Currency tmp(*this); 
  tmp.Val = -(tmp.Val); 
  return tmp;
}

Currency::operator double() const 
{
  return ((double)Val) / 10000;
}

Currency::operator int() const    
{
  return (int) (Val / 10000);
}


Currency StrToCurr( const String& S )
{
  Currency result;
  if ( ! TextToFloat( (Char*) ( ((void*) S.c_str()) ), &result, fvCurrency ) ) 
//    throw EConvertError( _T(SysConst_SInvalidFloat), ARRAYOFCONST( S )); 
    throw EConvertError( ErrorString(_T(SysConst_SInvalidFloat),  S )); 
  return result;
}


bool TryStrToCurr( const String& S, Currency& Value )
{
  return TextToFloat( (Char*) ( ((void*) S.c_str()) ), &Value, fvCurrency );
}


Currency StrToCurrDef( const String& S, Currency deflt )
{
  Currency result;
  if ( ! TextToFloat( (Char*) ( ((void*) S.c_str()) ), &result, fvCurrency ) ) 
    result = deflt;
  return result;
}


String CurrToStr( Currency Value )
{
  return FloatToStrF( Value, ffGeneral, - 1, 0 );
}


