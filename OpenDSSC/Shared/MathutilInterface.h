#ifndef MathutilInterfaceH
#define MathutilInterfaceH

#include <System.hpp>
#include "../Support/d2c_system.h"

#include "../Shared/Ucomplex.h"

namespace MathutilInterface
{



/*NOTE:   INCOMPLETE IMPLEMENTATION OF THE ZBEZJ ALGORITHM.
         CONFIRMED OPERATION OF I0 AND I1 BEFORE FINISHING IT. 8-24-06*/

  /*Define interface for VBA*/
void __stdcall I0(Ucomplex::complex& result, Ucomplex::complex& A);
void __stdcall I1(Ucomplex::complex& result, Ucomplex::complex& A);


}  // namespace MathutilInterface

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace MathutilInterface;
#endif

#endif // MathutilInterfaceH




