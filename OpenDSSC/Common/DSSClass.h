#ifndef DSSClassH
#define DSSClassH
/*
    ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*
 Base Class for all DSS collection classes.
 Keeps track of objects of each class, dispatches edits, etc
*/


#include "System.h"
#include "Sysutils.h"
#include "Command.h"
#include "Arraydef.h"
#include "HashList.h"
#include "PointerList.h"
#include "ParserDel.h"



//class TDSSClass;
//class TDSSClasses;

using namespace std;

namespace DSSClass
{ 

   // Collection of all DSS Classes

    class TDSSClasses 
    {

      friend class TDSSClass;
    public:
//      private:
      void Set_New(void* Value);
      public:
      TDSSClasses( );
      ~TDSSClasses( );
    };

       // Base for all collection classes



    class TDSSClass: public TObject{
      friend class TDSSClasses;
      //private:
      public:
      void Set_Active( int Value );
      int Get_ElementCount( );
      int Get_First( );
      int Get_Next( );
      void ResynchElementNameList( );
      //protected:
      std::string Class_Name;
      int ActiveElement;   // index of present ActiveElement
      TCommandList CommandList;
      int ActiveProperty;
      THashList ElementNameList;
      int AddObjectToList( void* Obj );  // Used by NewObject
      std::string Get_FirstPropertyName( );
      std::string Get_NextPropertyName( );
      virtual int MakeLike( std::string ObjName );
      void CountProperties( );  // Add no. of intrinsic properties
      void AllocatePropertyArrays( );
      void DefineProperties( );  // Add Properties of this class to propName
      int ClassEdit( const void* ActiveObj, const int ParamPointer );
      public:
      int NumProperties;
      std::string *PropertyName;
      std::string *PropertyHelp;
      int *PropertyIdxMap;
      int *RevPropertyIdxMap;    // maps property to internal command number
      int DSSClassType;
      TPointerList ElementList;
      bool ElementNamesOutOfSynch;     // When device gets renamed
      bool Saved;
      TDSSClass( );
      virtual ~TDSSClass();

             /*Helper routine for building Property strings*/;
      void AddProperty( std::string PropName, int CmdMapIndex, std::string HelpString );
      void ReallocateElementNameList( );
      virtual int Edit( int ActorID )      // uses global parser
      ;
      std::string get_myClass_name();
      virtual int Init( int Handle, int ActorID );
      virtual int NewObject( std::string ObjName );
      bool SetActive( std::string ObjName );
      void* GetActiveObj( ); // Get address of active obj of this class
      virtual void* Find( std::string ObjName )  // Find an obj of this class by name
      ;
      int PropertyIndex( std::string Prop );
      int get_ActiveElement();
    };

}
//VAR
//   DSSClasses         : TDSSClasses;
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace DSSClass;
#endif

#endif //  DSSClassH








