#ifndef MyDSSClassDefsH
#define MyDSSClassDefsH

#include "System.h"


namespace MyDSSClassDefs
{



/*
    Prototype unit for creating custom version of DSS

*/
const int MYCLASS_ELEMENT_CONST = 99 * 8;  // make unique constants for your classes
                                          // SEE DSSClassDefs.pas
     /*Assign (typically by adding) this constant to DSSClassType when objects of
      your custom class are instantiated. See Tline.Create in Line.Pas, for example*/
void CreateMyDSSClasses();  // Called in DSSClassDefs


}  // namespace MyDSSClassDefs

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace MyDSSClassDefs;
#endif

#endif // MyDSSClassDefsH




