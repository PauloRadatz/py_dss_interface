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


#include "d2c_system.h"
#include "d2c_syscurr.h"

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <time.h>
#include <stdexcept>
#include <iomanip>

#if !defined(windows)
#include <unistd.h>
#include "windows2posix.h"
#endif

#if defined(windows) && defined (_WIDESTRING)
#include <ShellAPI.h> // CommandLineToArgvW
#endif
#if defined(windows)
#ifdef _MSC_VER
#include <atlstr.h>
#endif
#endif
#include <cstring>

using namespace std;



// #define _CRT_NON_CONFORMING_SWPRINTFS 1





namespace System
{

int Argc = 0;
PPChar Argv = NULL;
Char* CmdLine = NULL;
int CmdShow = 0;
bool IsLibrary = false;
bool IsConsole = false;
int HPrevInst = 0, MainInstance = 0;




void FillChar( void* X, int Count, unsignedchar Value )
{
  memset(X, Value, Count);
}

void FillChar( char* X, int Count, unsignedchar Value )
{
  memset(X, Value, Count);
}

void FillChar( wchar_t* X, int Count, unsignedchar Value )
{
  wchar_t* p = X;
  if(Count <= 0)
     return;
  while(Count--)
    *p++ = (wchar_t) Value;
}

void FillChar( string& X, int Count, unsignedchar Value )
{
  if(Count <= 0)
     return;
  string::iterator t = X.begin();
  while(Count--)
    *t++ =  Value;
}

void FillChar( wstring& X, int Count, unsignedchar Value )
{
  if(Count <= 0)
     return;
  wstring::iterator t = X.begin();
  while(Count--)
    *t++ = (wchar_t) Value;
}

void Str(long double xd, string& xs)
{ 
  ostringstream oss;
  oss << scientific << uppercase << setprecision(14) << xd;
  xs = oss.str(); 
}

void Str(double xd, string& xs)
{
  ostringstream oss;
  oss << scientific << uppercase << setprecision(14) << xd;
  xs = oss.str(); 
}

void Str(int xi, int xiMinWidth, string& xs)
{
  ostringstream oss;
  oss << scientific << uppercase << setw(xiMinWidth) << setprecision(14) << xi;
  xs = oss.str(); 
}

void Str(double xd, int xiMinWidth, string& xs)
{
  ostringstream oss;
  oss << scientific << uppercase << setw(xiMinWidth) << setprecision(14)  << xd;
  xs = oss.str(); 
}

void Str(long double xd, int xiMinWidth, string& xs)
{
  ostringstream oss;
  oss << scientific << uppercase << setw(xiMinWidth) << setprecision(14)  << xd;
  xs = oss.str(); 
}

void Str(double xd, int xiMinWidth, int xiDecPlaces, string& xs)
{
  ostringstream oss;
  if(xiMinWidth == 0)
  {
    if(xiDecPlaces == 0)
     oss << fixed << setprecision(xiDecPlaces) << xd;
    else
     oss << fixed << showpoint << setprecision(xiDecPlaces) << xd;
  }
  else
    oss << scientific << uppercase << setw(xiMinWidth) << setprecision(xiDecPlaces) << xd;
  xs = oss.str(); 
}

void Str(long double xd, int xiMinWidth, int xiDecPlaces, string& xs)
{
  ostringstream oss;
  if(xiMinWidth == 0)
  {
    if(xiDecPlaces == 0)
     oss << fixed << setprecision(xiDecPlaces) << xd;
    else
     oss << fixed << showpoint << setprecision(xiDecPlaces) << xd;
  }
  else
    oss << scientific << uppercase << setw(xiMinWidth) << setprecision(xiDecPlaces) << xd;
  xs = oss.str(); 
}

void Str(const Currency& xcr, int xiMinWidth, int xiDecPlaces, string& xs)
{
  ostringstream oss;
  if(xiMinWidth == 0)
  {
    if(xiDecPlaces == 0)
     oss << fixed << setprecision(xiDecPlaces) << (double) xcr;
    else
     oss << fixed << showpoint << setprecision(xiDecPlaces) << (double) xcr;
  }
  else
    oss << setw(xiMinWidth) << setprecision(xiDecPlaces) << (double) xcr;
  xs = oss.str(); 
}


void Str(long double xd, wstring& xs)
{ 
  wostringstream oss;
  oss << scientific << uppercase << setprecision(14) << xd;
  xs = oss.str(); 
}

void Str(double xd, wstring& xs)
{
  wostringstream oss;
  oss << scientific << uppercase << setprecision(14) << xd;
  xs = oss.str(); 
}


void Str(double xd, int xiMinWidth, wstring& xs)
{
  wostringstream oss;
  oss << scientific << uppercase << setw(xiMinWidth) << setprecision(14)  << xd;
  xs = oss.str(); 
}

void Str(int xi, int xiMinWidth, wstring& xs)
{
  wostringstream oss;
  oss << scientific << uppercase << setw(xiMinWidth) << setprecision(14)  << xi;
  xs = oss.str(); 
}


void Str(long double xd, int xiMinWidth, wstring& xs)
{ 
  wostringstream oss;
  oss << scientific << uppercase << setw(xiMinWidth) << setprecision(14) << xd;
  xs = oss.str(); 
}

void Str(double xd, int xiMinWidth, int xiDecPlaces, wstring& xs)
{
  wostringstream oss;
  if(xiMinWidth == 0)
  {
    if(xiDecPlaces == 0)
     oss << fixed << setprecision(xiDecPlaces) << xd;
    else
     oss << fixed << showpoint << setprecision(xiDecPlaces) << xd;
  }
  else
    oss << scientific << uppercase << setw(xiMinWidth) << setprecision(xiDecPlaces) << xd;
  xs = oss.str(); 
}

void Str(long double xd, int xiMinWidth, int xiDecPlaces, wstring& xs) 
{
  wostringstream oss;
  if(xiMinWidth == 0)
  {
    if(xiDecPlaces == 0)
     oss << fixed << setprecision(xiDecPlaces) << xd;
    else
     oss << fixed << showpoint << setprecision(xiDecPlaces) << xd;
  }
  else
    oss << scientific << uppercase << setw(xiMinWidth) << setprecision(xiDecPlaces) << xd;
  xs = oss.str(); 
}

void Str(const Currency& xcr, int xiMinWidth, int xiDecPlaces, wstring& xs)
{
  wostringstream oss;
  if(xiMinWidth == 0)
  {
    if(xiDecPlaces == 0)
     oss << fixed << setprecision(xiDecPlaces) << (double) xcr;
    else
     oss << fixed << showpoint << setprecision(xiDecPlaces) << (double) xcr;
  }
  else
    oss << setw(xiMinWidth) << setprecision(xiDecPlaces) << (double) xcr;
  xs = oss.str(); 
}

void Move(void* Source, void* Dest, int Count)
{
  memmove(Dest, Source, Count);
}

void Move(const string& Source, string& Dest, unsigned int Count)
{
   if(Count == Source.length())
     Dest = Source;
   else
     Dest = Source.substr(0, Count);
}

void Move(const wstring& Source, wstring& Dest, unsigned int Count)
{
   if(Count == Source.length())
     Dest = Source;
   else
     Dest = Source.substr(0, Count);
}

void Move(const char* Source, string& Dest, unsigned int Count)
{
   if(Count == strlen(Source))
     Dest = Source;
   else
     Dest = string(Source, Source + Count);
}

void Move(const wchar_t* Source, wstring& Dest, unsigned int Count)
{
   if(Count == wcslen(Source))
     Dest = Source;
   else
     Dest = wstring(Source, Source + Count);
} 

void Move(const std::string& Source, char* Dest, unsigned int Count)
{
  strncpy(Dest, Source.c_str(), Count); 
}

void Move(const std::wstring& Source, wchar_t* Dest, unsigned int Count)
{
   wcsncpy(Dest, Source.c_str(), Count);
}



////////

WORD Swap( WORD X )
{
  return ((WORD) ( ( X >> 8 ) | ( X << 8 ) ) );
}

int Swap( int X )
{
  return ( ( X & 0xFFFF ) << 16 ) + ( X >> 16 );
}

unsignedint Swap( unsignedint X )
{
  return ( ( X & 0xFFFF ) << 16 ) + ( X >> 16 );
}

int64_t Swap( int64_t X )
{
  return ( ( X & 0xFFFFFFFF ) << 32 ) + ( X >> 32 );
}

////////

void Assert( bool expr )
{
  if(!expr)
    throw runtime_error("assertion failed"); 
}

void Assert( bool expr, const string& Msg )
{
  if(!expr)
    throw runtime_error(Msg.c_str()); 
}


//  -> not contained in Delphi2Cpp trial 


//#ifdef WIN64
Char ModuleName[ 256/*# range 0..255*/ ];


Char* GetCommandFile( )
{
  Char* result = NULL;
  GetModuleFileName( 0, ModuleName, 255 );
  return ModuleName;
}
//#endif

void allocarg( int idx, int Len, int& argvlen )
{
  int oldargvlen = 0;
  if ( idx >= argvlen )
  {
    oldargvlen = argvlen;
    argvlen = ( idx + 8 ) & ( ~ 7 );
    ReallocMem( Argv, argvlen * sizeof( void* ) );
    FillChar( &Argv[oldargvlen], ( argvlen - oldargvlen ) * sizeof( void* ), 0 );
  }
      /* use Realloc to reuse Already Existing memory */
      /* always allocate, even if Length is zero, since */
      /* the arg. is still present!                     */
  ReallocMem( Argv[idx], Len + 1 );
}

#define System__39 ( TSet < wchar_t, 0, 255 >() \
                     << wchar_t ( 1 ) << wchar_t ( 2 ) << wchar_t ( 3 ) << wchar_t ( 4 ) << wchar_t ( 5 ) << wchar_t ( 6 ) << wchar_t ( 7 ) << wchar_t ( 8 ) << wchar_t ( L'\t' ) << wchar_t ( L'\n' ) \
                     << wchar_t ( 11 ) << wchar_t ( 12 ) << wchar_t ( L'\r' ) << wchar_t ( 14 ) << wchar_t ( 15 ) << wchar_t ( 16 ) << wchar_t ( 17 ) << wchar_t ( 18 ) << wchar_t ( 19 ) << wchar_t ( 20 ) \
                     << wchar_t ( 21 ) << wchar_t ( 22 ) << wchar_t ( 23 ) << wchar_t ( 24 ) << wchar_t ( 25 ) << wchar_t ( 26 ) << wchar_t ( 27 ) << wchar_t ( 28 ) << wchar_t ( 29 ) << wchar_t ( 30 ) \
                     << wchar_t ( 31 ) << wchar_t ( L' ' ) )

#ifdef windows
#ifdef WIN64
void setup_arguments( )
{
  int arglen = 0, Count = 0;
  char* argstart = NULL,* pc = NULL,* arg = NULL;
  char Quote = L'\0';
  int argvlen = 0;
  //SetupProcVars();
  /* Create commandLine, it starts with the executed Filename which is Argv[0] */
  /* Win32 passes the command not via the Args, but via GetModuleFileName*/
  Count = 0;
  Argv = NULL;
  argvlen = 0;
  pc = GetCommandFile();
  arglen = 0;
  do
  {
    arglen++;
  }
  while ( ! ( pc[arglen] == L'\x00' ) );
  allocarg( 0, arglen, argvlen );
  Move( (void*) pc, Argv[Count], arglen + 1 );
  /* setup CmdLine variable */
  CmdLine = GetCommandLine();
  /* process Arguments */
  pc = CmdLine;
  while ( *pc != L'\x00' )
  {
     /* Skip leading spaces */
    while ( System__39.Contains(*pc ) )
      pc++;
    if ( *pc == L'\x00' )
      break;
     /* Calc argument Length */
    Quote = L' ';
    argstart = pc;
    arglen = 0;
    bool bContinue = true;
    while ( bContinue && *pc != L'\x00' )
    {
      switch ( *pc )
      {
        case /*# L'\x01' .. L'\x20' */ 1 :
         case 2: case 3: case 4: case 5: case 6: case 7: case 8: case L'\t': case L'\n': case 11 :
         case 12: case L'\r': case 14: case 15: case 16: case 17: case 18: case 19: case 20: case 21 :
         case 22: case 23: case 24: case 25: case 26: case 27: case 28: case 29: case 30: case 31 :
         case L' ':
        {
          if ( Quote != L' ' )
            arglen++;
          else
            bContinue = false;
        }
        break;
        case L'\"':
        {
          if ( Quote != L'\'' )
          {
            if ( *(wchar_t*) ( pc + 1 ) != L'\"' )
            {
              if ( Quote == L'\"' )
                Quote = L' ';
              else
                Quote = L'\"';
            }
            else
              pc++;
          }
          else
            arglen++;
        }
        break;
        case L'\'':
        {
          if ( Quote != L'\"' )
          {
            if ( *(wchar_t*) ( pc + 1 ) != L'\'' )
            {
              if ( Quote == L'\'' )
                Quote = L' ';
              else
                Quote = L'\'';
            }
            else
              pc++;
          }
          else
            arglen++;
        }
        break;
      default:
        arglen++;
      }
      pc++;
    }
     /* copy argument */
     /* don'T copy the First one, it is Already there.*/
    if ( Count != 0 )
    {
      allocarg( Count, arglen, argvlen );
      Quote = L' ';
      pc = argstart;
      arg = Argv[Count];
      bContinue = true;
      while ( bContinue && *pc != L'\x00' )
      {
        switch ( *pc )
        {
          case /*# L'\x01' .. L'\x20' */ 1 :
           case 2: case 3: case 4: case 5: case 6: case 7: case 8: case L'\t': case L'\n': case 11 :
           case 12: case L'\r': case 14: case 15: case 16: case 17: case 18: case 19: case 20: case 21 :
           case 22: case 23: case 24: case 25: case 26: case 27: case 28: case 29: case 30: case 31 :
           case L' ':
          {
            if ( Quote != L' ' )
            {
              *arg = *pc;
              arg++;
            }
            else
              bContinue = false;
          }
          break;
          case L'\"':
          {
            if ( Quote != L'\'' )
            {
              if ( *(wchar_t*) ( pc + 1 ) != L'\"' )
              {
                if ( Quote == L'\"' )
                  Quote = L' ';
                else
                  Quote = L'\"';
              }
              else
                pc++;
            }
            else
            {
              *arg = *pc;
              arg++;
            }
          }
          break;
          case L'\'':
          {
            if ( Quote != L'\"' )
            {
              if ( *(wchar_t*) ( pc + 1 ) != L'\'' )
              {
                if ( Quote == L'\'' )
                  Quote = L' ';
                else
                  Quote = L'\'';
              }
              else
                pc++;
            }
            else
            {
              *arg = *pc;
              arg++;
            }
          }
          break;
        default:
        {
          *arg = *pc;
          arg++;
        }
        }
        pc++;
      }
      *arg = L'\x00';
    }
    Count++;
  }
  /* get Argc and Create an nil entry */
  Argc = Count;
  allocarg( Argc, arglen, argvlen );
  /* Free Unused memory */
  ReallocMem( Argv, ( Argc + 1 ) * sizeof( void* ) );
}
#else
#ifndef _WIDESTRING
void setup_arguments( )
{
  int arglen = 0, Count = 0;
  Char* argstart = NULL,* pc = NULL,* arg = NULL;
  bool Quote = false;
  int argvlen = 0;
  Char Buf[ 260 ];  // need MAX_PATH Bytes, not 256!
//  SetupProcVars();
  // Create commandLine, it starts with the executed Filename which is Argv[0] 
  // Win32 passes the command not via the Args, but via GetModuleFileName
  Count = 0;
  Argv = NULL;
  argvlen = 0;
  arglen = GetModuleFileName( 0, &Buf[0], sizeof( Buf ) );
  Buf[arglen] = L'\x00'; // be safe
  allocarg( 0, arglen, argvlen );
  Move( (void*) Buf, Argv[0], arglen + 1 );
  // setup CmdLine variable 
  CmdLine = GetCommandLine();
  // process Arguments 
  pc = CmdLine;
  while ( *pc != L'\x00' )
  {
     // Skip leading spaces 
    while ( System__39.Contains(*pc ) )
      pc++;
    if ( *pc == L'\x00' )
      break;
    // Calc argument Length 
    Quote = false;
    argstart = pc;
    arglen = 0;
    bool bContinue = true;
    while ( bContinue && *pc != L'\x00' )
    {
      switch ( *pc )
      {
        case  1 :
         case 2: case 3: case 4: case 5: case 6: case 7: case 8: case L'\t': case L'\n': case 11 :
         case 12: case L'\r': case 14: case 15: case 16: case 17: case 18: case 19: case 20: case 21 :
         case 22: case 23: case 24: case 25: case 26: case 27: case 28: case 29: case 30: case 31 :
         case L' ':
        {
          if ( Quote )
            arglen++;
          else
            bContinue = false;
            //break;
        }
        break;
        case L'\"':
          if ( pc[1] != L'\"' )
            Quote = ! Quote;
          else
            pc++;
        break;
      default:
        arglen++;
      }
      pc++;
    }
    // copy argument 
    // don'T copy the First one, it is Already there.
    if ( Count != 0 )
    {
      allocarg( Count, arglen, argvlen );
      Quote = false;
      pc = argstart;
      arg = Argv[Count];
      bContinue = true;
      while ( bContinue && *pc != L'\x00' )
      {
        switch ( *pc )
        {
          case 1 :
           case 2: case 3: case 4: case 5: case 6: case 7: case 8: case L'\t': case L'\n': case 11 :
           case 12: case L'\r': case 14: case 15: case 16: case 17: case 18: case 19: case 20: case 21 :
           case 22: case 23: case 24: case 25: case 26: case 27: case 28: case 29: case 30: case 31 :
           case L' ':
          {
            if ( Quote )
            {
              *arg = *pc;
              arg++;
            }
            else
              bContinue = false;
              //break;
          }
          break;
          case L'\"':
            if ( pc[1] != L'\"' )
              Quote = ! Quote;
            else
              pc++;
          break;
        default:
        {
          *arg = *pc;
          arg++;
        }
        }
        pc++;
      }
      *arg = L'\x00';
    }
    Count++;
  }
  // get Argc 
  Argc = Count;
  // Free Unused memory, leaving A nil entry at the end 
  ReallocMem( Argv, ( Count + 1 ) * sizeof( void* ) );
  Argv[Count] = NULL;
}
#else
// win32 / wchar_t
void setup_arguments( )
{
}
#endif // WIDESTRING

#endif // WIN32



int ParamCount( )
{
  //if(Argc == 0)
  //  setup_arguments();
    //Argv = CommandLineToArgvW(GetCommandLineW(), &Argc);
  return Argc - 1;
}

String ParamStr( int l )
{
  //if(Argc == 0)
  //  setup_arguments();
    //Argv = CommandLineToArgvW(GetCommandLineW(), &Argc);
  String result;
  if ( ( l >= 0 ) && ( l < Argc ) )
    result = Argv[l];
  else
    result = _T("");
  return result;
}

// looks up Module in list of all loaded modules
HMODULE FindResourceHInstance(unsigned int Module)
{ // not in free pascal and proprietary copyright in Borland sources
  if(Module == NULL)
     return ::GetModuleHandle(NULL);
  else
    return (HMODULE) Module;
}

class ResourceString
{
public:
    unsigned int* module;
    int identifier;
};

String LoadResourceString(const ResourceString* const rsrc)
{
  if ( rsrc == NULL )
    return String();

  // Resource strings are identified by a 16 bit unsigned integer and in Win32, the first 64KB of address space is permanently invalid
  if( rsrc->identifier < 64 * 1024 )  
  {
    String s;
    wchar_t* buf;
    HINSTANCE h;
    if(rsrc->module == NULL)
      h = ::GetModuleHandle(NULL);
    else
      h = FindResourceHInstance(*rsrc->module);
    String::size_type len = ::LoadStringW(h, rsrc->identifier, (wchar_t*) &buf, 0);
    if (len > 0)
#ifdef _WIDESTRING
      s.assign(buf, len);
#else
    {
       len *= sizeof(wchar_t) + 1;
       s.resize(len); // make room for trailing '\0' in worst case
       len = ::LoadStringA(h, rsrc->identifier, (char*) s.c_str(), len);
       s.resize(len);
    }
#endif
    return s;
  }
  else
    return (Char*) ( rsrc->identifier );
} 

String d2c_LoadResourceString(int Ident)
{
  ResourceString ResString;
  HMODULE h = ::GetModuleHandle(NULL);
  ResString.module = (unsigned int*) &h;
  ResString.identifier = (int) Ident;
  return LoadResourceString( &ResString );
}


#elif defined(linux)

int ParamCount( )
{
  int result = 0;
  result = Argc - 1;
  return result;
}

SmallString<255> execpathstr;

String ParamStr( int l )
{
   /* stricly conforming Posix Applications  */
   /* have the executing Filename as Argv[0] */
  if ( l == 0 )
    return execpathstr;
  else
  if ( l < Argc )
     return Argv[l];
   else
     return String();
}

cint FpReadLink( const char* Name, char* LinkName, size_t MaxLen )
{
  cint r = readlink(Name, LinkName, MaxLen);
  return r==-1 ? -errno : r;
}

void SysInitExecPath( )
{
  int i = 0;
  execpathstr[0] = '\x00';
  i = FpReadLink( "/proc/self/exe", &execpathstr[1], 255 /*# High(execpathstr) */ );
  /* it must also be an ABSOLUTE Filename, LINUX 2.0 Points to A memory
    location So this will Skip that */
  if ( ( i > 0 ) && ( execpathstr[1] == '/' ) )
    execpathstr[0] = ((Char) i );
}

#else
#error unknown platform
#endif


void System_initialization()
{
#ifdef _USRDLL  // todo use SHGFI_EXETYPE ?
  IsLibrary = true;
#endif

#ifdef _CONSOLE
  IsConsole = true;
#endif

#ifdef windows
  STARTUPINFO SI;
    GetStartupInfo( &SI );
  /* some Misc Win32 stuff */
  HPrevInst = 0;
  /*
  if ( ! IsLibrary )
    SysInstance = GetModuleHandle( NULL );
  MainInstance = HInstance();
  */

  if(!IsLibrary)
    CmdShow = SI.wShowWindow;

#ifdef _WIDESTRING
  CmdLine = GetCommandLine();
  Argv = CommandLineToArgvW(GetCommandLineW(), &Argc);
#else
  setup_arguments( );
#endif

#elif defined(linux)
  SysInitExecPath( );
#else
  #error system not defined
#endif

}

void System_finalization()
{
#ifdef _CONSOLE
#ifdef windows
  if(Argv != NULL)
    GlobalFree(Argv);
#endif
#endif
}

class System_unit
{
public:
System_unit()
{
  System_initialization();
}
~System_unit(){ System_finalization(); }
};
System_unit _System_unit;


}  // namespace System
