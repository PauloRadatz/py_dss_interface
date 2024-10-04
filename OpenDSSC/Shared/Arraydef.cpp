//#include <vcl.h>
#pragma hdrstop

#include "Arraydef.h"
//#include <System.SysUtils.hpp>

using namespace std;

namespace Arraydef
{



pStringArray AllocStringArray(int Size)
{
	pStringArray result = nullptr;
	result = new std::string[ sizeof((result)[0]) * Size] ;
	return result;
}
// Allocates a string array initialized with nil values

void FreeStringArray(pStringArray& ps, int Size)
{
	int i = 0;
	if(ps != nullptr)
	{
		int stop = 0;
		for(stop = Size, i = 1; i <= stop; i++)
		{
			ps[i - 1] = "";  // decrement counter in string
		}
		delete[] ps; // Deallocate the memory
		ps = nullptr;  // Throw it away and set it to NIL
	}
}


}  // namespace Arraydef




