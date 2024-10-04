#ifndef HashlistH
#define HashlistH

//#include "System.h"
//#include "d2c_system.h"
#include "Arraydef.h"
#include <math.h>
#include "System.h"
#include "Sysutils.h"
#include <string>
#include <d2c_structures.h>

namespace Hashlist
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

/*
  This Hash list module is designed to make searches on arrays of strings more
  efficient.  The list actually consists of several short linear lists.  When a string
  is added, it is hashed and placed at the end of one of the lists.

  The list may by searched by string or by index.  When by string, the string
  is hashed and the search is restricted to the resulting linear list.  When by
  index, it simply goes to that index in the array of pointers that points to the
  individual strings.

  All strings are saved in lower case and tested with case sensitivity.  This
  actually makes the search insensitive to case because everything is lower case.

  Modified 4/18/98 to allocate on demand.  That way, you can create it for a certain
  number of hash lists, without caring about how many actual there will be.

*/

/*$M+*/

   //pStringArray = ^StringArray;
  // StringArray = Array[1..100] of String;



struct TSubList
{
	std::vector <std::string> Str;
	std::vector <int> Idx;
	unsigned int Nelem;
	unsigned int NAllocated;
};
typedef TSubList SubListArray;
typedef std::vector <SubListArray> pSubListArray;

class THashList : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
	unsigned int NumElementsAllocated;
	unsigned int NumLists;
	unsigned int NumElements;
	std::vector <std::string> StringPtr;
	pSubListArray ListPtr;
	unsigned int AllocationInc;
	unsigned int LastFind;
	unsigned int LastHash;
	String LastSearchString;
	void ResizeSubList(TSubList& SubList);
	unsigned int Hash(const String s);
	void ResizeStrPtr();
	unsigned int Get_NumElements();
//protected:
public:
	unsigned int InitialAllocation;
	THashList(unsigned int Nelements);
	virtual ~THashList();
	int Add(const String s);
	int Find(const String s); // This is 1-based! For buses, e.g. ActiveBusIndex, remember to subtract 1.
	int FindNext();  //  repeat find for duplicate string in same hash list
	int FindAbbrev(const String s);
	String Get(unsigned int i); // This is 1-based! For buses, e.g. ActiveBusIndex, remember to sum 1.
	void Expand(unsigned int NewSize);   /*Expands number of elements*/
	void DumpToFile(const String FName);
	void Clear();
//__published:
public:
	THashList();
};


}  // namespace Hashlist

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace Hashlist;
#endif

#endif // HashlistH




