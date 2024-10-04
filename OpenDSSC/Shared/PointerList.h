#ifndef PointerListH
#define PointerListH

#include "System.h"
#include "Sysutils.h"
#include "Arraydef.h"



//class TPointerList;

namespace PointerList
{



/*$M+*/
/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TPointerList : public System::TObject
{
public:
	typedef TObject inherited;
//private:
	int NumInList;
	int MaxAllocated;
	int ActiveItem;
	std::vector <void*> List;
	int IncrementSize;
	void* Get_First();
	void* Get_Next();
	void* Get_Active();
	void Set_New(void* Value);
public:
	int get_myNumList();
	int get_myActiveItem();
	TPointerList(int Size);
	virtual ~TPointerList();
	void Clear();
	int Add(void* P);  // Returns index of item
	void* Get(int i);
//__published:
public:
	TPointerList();
};


}  // namespace PointerList

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace PointerList;
#endif

#endif // PointerListH




