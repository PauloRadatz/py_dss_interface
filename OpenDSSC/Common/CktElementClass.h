#ifndef CktElementClassH
#define CktElementClassH


    /*
        ----------------------------------------------------------
      Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
      All rights reserved.
      ----------------------------------------------------------
    */
    /* Created 5/17/01 RCD to balance inheritance tree for Circuit Elements*/

    /*$M+*/


#include "System.h"
#include "DSSClass.h"



namespace CktElementClass
{


    class TCktElementClass : public TDSSClass {
        typedef TDSSClass inherited;
    private:
    protected:
        int virtual ClassEdit(const void* ActiveCktElemObj, const int ParamPointer);
        void ClassMakeLike(const void* OtherObj);
        void CountProperties();  // Add no. of intrinsic properties
        void DefineProperties();  // Add Properties of this class to propName
    public:
        int NumCktElemClassProps;
        TCktElementClass();
        virtual ~TCktElementClass();
        //__published:
    };

}  // namespace CktElementClass

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
    using namespace CktElementClass;
#endif

#endif //  CktElementClassH









