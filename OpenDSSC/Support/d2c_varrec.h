#ifndef d2c_varrecH
#define d2c_varrecH

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
    d2c_sysfile.h, d2c_sysfile.cpp 
    d2c_sysmath.h d2c_sysmath.cpp
    d2c_sysstring.h d2c_sysstring.cpp
    d2c_systypes.h
    d2c_varrec.h
    d2c_smallstring.h

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


#include "d2c_systypes.h"
#include "d2c_sysdate.h"
#include "d2c_syscurr.h"
#include <vector>


namespace System {
  class TObject;
  class TMetaClass;
  typedef const TMetaClass* TClass;
}

class Variant;
typedef Variant* PVariant;
typedef uint64_t* puint64_t;


const int vtInteger = 0;
const int vtBoolean = 1;
const int vtChar = 2;
const int vtExtended = 3;
const int vtString = 4;
const int vtPointer = 5;
const int vtPChar = 6;
const int vtObject = 7;
const int vtClass = 8;
const int vtWideChar = 9;
const int vtPWideChar = 10;
const int vtAnsiString = 11;
const int vtCurrency = 12;
const int vtVariant = 13;
const int vtInterface = 14;
const int vtWideString = 15;
const int vtInt64 = 16;
const int vtUInt64 = 17;



struct TVarRec {
  uint64_t VType;
  union 
  {

#if defined(ENDIAN_BIG) && defined(CPU64) 
    int32_t IntegerDummy; 
#endif
    int VInteger;
#if defined(ENDIAN_BIG) && defined(CPU64)  
    int32_t BoolDummy; 
#endif
    bool VBoolean;
#if defined(ENDIAN_BIG) && defined(CPU64)  
    int32_t CharDummy; 
#endif
    char VChar;
#if defined(ENDIAN_BIG) && defined(CPU64)  
    int32_t WCharDummy; 
#endif
    wchar_t VWideChar;
    PExtended VExtended;
    PShortString VString;
    void* VPointer;
    char* VPChar;
    System::TObject* VObject;
    System::TClass VClass;
    wchar_t* VPWideChar;
    void* VAnsiString;
    PCurrency VCurrency;
    PVariant VVariant;
    void* VInterface;
    void* VWideString;
    PInt64 VInt64;
    puint64_t VUInt64;
  }; //union

  TVarRec(): VType(vtInteger), VInteger(0) {} 
  TVarRec(int src): VType(vtInteger), VInteger(src) {}
  TVarRec(unsigned int src): VType(vtInteger), VInteger(static_cast<int>(src)) {}
  TVarRec(bool src): VType(vtBoolean), VBoolean(src) {}
  TVarRec(char src): VType(vtChar), VChar(src) {}
  TVarRec(wchar_t src): VType(vtWideChar), VWideChar(src) {}
  TVarRec(const long double& src): VType(vtExtended), VExtended(const_cast<long double*>(&src)) {}
  TVarRec(const double& src): VType(vtExtended), VExtended(reinterpret_cast<long double*>(const_cast<double*>(&src))) {}
  TVarRec(const float& src): VType(vtExtended), VExtended(reinterpret_cast<long double*>(const_cast<float*>(&src))) {}
  TVarRec(const ShortString& src): VType(vtString), VString(const_cast<PShortString>(&src)) {}
  TVarRec(const void* src): VType(vtPointer), VPointer(const_cast<void*>(src)) {}
  TVarRec(const char* src): VType(vtPChar), VPChar(const_cast<char*>(src)) {}
  TVarRec(const System::TObject& src): VType(vtObject), VObject(const_cast<System::TObject*>(&src)) {}
  TVarRec(System::TClass src): VType(vtClass), VClass(src) {}
  TVarRec(const wchar_t* src): VType(vtPWideChar), VPWideChar(const_cast<wchar_t*>(src)) {}
  TVarRec(const std::string& src): VType(vtAnsiString), VAnsiString(reinterpret_cast<void*>(const_cast<char*>(src.c_str()))) {}
  TVarRec(const std::wstring& src): VType(vtWideString), VWideString(reinterpret_cast<void*>(const_cast<wchar_t*>(src.c_str()))) {}
  TVarRec(const Currency& src): VType(vtCurrency), VCurrency(const_cast<PCurrency>(&src)) {}
  TVarRec(const int64_t& src): VType(vtInt64), VInt64(const_cast<PInt64>(&src)) {}
  TVarRec(const uint64_t& src): VType(vtUInt64), VUInt64(const_cast<puint64_t>(&src)) {}
  TVarRec(const Variant& src): VType(vtVariant), VVariant(const_cast<PVariant>(&src)) {}
  
  TVarRec& operator =(int src)
  {
    VType = vtInteger;
    VInteger = src;
    return *this;
  }

  TVarRec& operator =(bool src)
  {
    VType = vtBoolean;
    VBoolean = src;
    return *this;
  }

  TVarRec& operator =(char src)
  {
    VType = vtChar;
    VChar = src;
    return *this;
  }

  TVarRec& operator =(wchar_t src)
  {
    VType = vtWideChar;
    VWideChar = src;
    return *this;
  }

  TVarRec& operator =(const long double& src)
  {
    VType = vtExtended;
    VExtended = const_cast<PExtended>(&src);
    return *this;
  }

  TVarRec& operator =(const double& src)
  {
    VType = vtExtended;
    VExtended = reinterpret_cast<long double*>(const_cast<double*>(&src));
    return *this;
  }

  TVarRec& operator =(const float& src)
  {
    VType = vtExtended;
    VExtended = reinterpret_cast<long double*>(const_cast<float*>(&src));
    return *this;
  }

  TVarRec& operator =(const ShortString& src)
  {
    VType = vtString;
    VString = const_cast<PShortString>(&src);
    return *this;
  }

  TVarRec& operator =(const void* src)
  {
    VType = vtPointer;
    VPointer = const_cast<void*>(src);
    return *this;
  }

  TVarRec& operator =(const char* src)
  {
    VType = vtPChar;
    VPChar = const_cast<char*>(src);
    return *this;
  }

  TVarRec& operator =(const wchar_t* src)
  {
    VType = vtPWideChar;
    VPWideChar = const_cast<wchar_t*>(src);
    return *this;
  }

  TVarRec& operator =(const System::TObject& src)
  {
    VType = vtObject;
    VObject = const_cast<System::TObject*>(&src);
    return *this;
  }

  TVarRec& operator =(System::TClass src)
  {
    VType = vtClass;
    VClass = src;
    return *this;
  }


  TVarRec& operator =(const PWideChar src)
  {
    VType = vtPWideChar;
    VPWideChar = src;
    return *this;
  }

  TVarRec& operator =(const std::string& src)
  {
    VType = vtAnsiString;
    VAnsiString = reinterpret_cast<void*>(const_cast<char*>(src.c_str()));
    return *this;
  }

  TVarRec& operator =(const std::wstring& src)
  {
    VType = vtWideString;
    VWideString = reinterpret_cast<void*>(const_cast<wchar_t*>(src.c_str()));
    return *this;
  }

  TVarRec& operator =(const Currency& src)
  {
    VType = vtCurrency;
    VCurrency = const_cast<PCurrency>(&src);
    return *this;
  }

  TVarRec& operator =(const int64_t& src)
  {
    VType = vtInt64;
    VInt64 = const_cast<PInt64>(&src);
    return *this;
  }

  TVarRec& operator =(const uint64_t& src)
  {
    VType = vtUInt64;
    VUInt64 = const_cast<puint64_t>(&src);
    return *this;
  }

  TVarRec& operator =(const Variant& src)
  {
    VType = vtVariant;
    VVariant = const_cast<PVariant>(&src);
    return *this;
  }

};

typedef TVarRec *PVarRec;
typedef std::vector<TVarRec> VECTOROFCONST;

#endif