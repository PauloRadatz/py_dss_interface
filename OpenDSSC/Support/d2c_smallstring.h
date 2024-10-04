#ifndef d2c_smallstringH
#define d2c_smallstringH


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


/*******************************************************************************

       the class SmallString is constructed in analogy to
       the SmallString class of the Borland VCL as
       described in the CBuilder help file.

*******************************************************************************/

#include <string.h>
#include <string>

template <unsigned char sz> class SmallString
{
public:
  static unsigned char High() { return sz; }

  SmallString() : m_Len(0) {}
  
  SmallString(const SmallString& xsOther)
  {
    m_Len = xsOther.m_Len;
    strcpy(m_Data, xsOther.m_Data); 
  }

  SmallString& operator =(const SmallString& xsOther)
  {
    if (this != &xsOther)
    {
      m_Len = xsOther.m_Len;
      strcpy(m_Data, xsOther.m_Data);
    }
    return *this;
  }

  SmallString(const char* xpChar)
  {
    int len = strlen(xpChar);
    m_Len = (unsigned char)((len > sz) ? sz : len);
    strncpy(m_Data, xpChar, m_Len);
    m_Data[m_Len] = '\0';
  }

  SmallString& operator =(const char* xpChar)
  {
    int len = strlen(xpChar);
    m_Len = (unsigned char)((len > sz) ? sz : len);
    strncpy(m_Data, xpChar, m_Len);
    m_Data[m_Len] = '\0';
    return *this;
  }

  SmallString(char xChar)
  {
    m_Len = 1;
    m_Data[0] = xChar;
    m_Data[1] = '\0';
  }


  SmallString(const std::string& xsOther)
  {
    int len = xsOther.length();
    m_Len = (unsigned char)((len > sz) ? sz : len);
    strncpy(m_Data, xsOther.c_str(), m_Len);
    m_Data[m_Len] = '\0';
  }

  SmallString& operator =(const std::string& xsOther)
  {
    int len = xsOther.length();
    m_Len = (unsigned char)((len > sz) ? sz : len);
    strncpy(m_Data, xsOther.c_str(), m_Len);
    m_Data[m_Len] = '\0';
    return *this;
  }


  template <unsigned char sz2>
  bool operator ==(const SmallString<sz2>& xOther) const
  {
     return strcmp(m_Data, xOther.c_str()) == 0;
  }

  template <unsigned char sz2>
  bool operator !=(const SmallString<sz2>& xOther) const
  {
     return strcmp(m_Data, xOther.c_str()) != 0;
  }


  inline const char* c_str() const   
  { 
     return (m_Len > 0)? m_Data : "";
  }


  inline char operator [] (const unsigned char xiIndex) const
  {
  	return m_Data[xiIndex-1];
  }

  inline char& operator [](const unsigned char xiIndex)
  {
  	return m_Data[xiIndex-1];
  }

  inline operator std::string() const
  {
    return std::string(m_Data);
  }

  inline operator std::wstring() const
  {
    return std::wstring(m_Data, m_Data + m_Len);
  }

  inline int  Length()  const {return m_Len; }
  inline bool IsEmpty() const {return m_Len == 0; }
  inline void SetLength(unsigned char len)
  {
     m_Len = (unsigned char)((len > sz) ? sz : len);
     m_Data[m_Len] = '\0';
  }


private:
	
  unsigned char   m_Len;
  char            m_Data[sz + 1];

};

//---------------------------------------------------------------------------
template <unsigned char sz>
SmallString<sz> UpperCase( const SmallString<sz>& S )
{
  SmallString<sz> result;
  int i = 0;
  result[0] = S[0];
  for ( i = 1; i <= S[0]; i++)
    result[i] = toupper( S[i] );
  return result;
}


#endif