//#include <vcl.h>
#pragma hdrstop

#include "PointerList.h"


using namespace std;
using namespace Arraydef;
using namespace System;

namespace PointerList
{
	PointerList::TPointerList::TPointerList(): TPointerList(0) {};

	//-------------------------------------------------------------------------
	PointerList::TPointerList::TPointerList(int Size)
	{
		NumInList = 0;
		MaxAllocated = 0;
		ActiveItem = 0;
		IncrementSize = 0;
		MaxAllocated = Size;
		if (MaxAllocated <= 0)
			MaxAllocated = 10;    // Default Size & Increment
		List.clear();
		List.resize(MaxAllocated);
		ActiveItem = 0;
		IncrementSize = MaxAllocated;  // Increment is equal to original allocation
	}

	//-------------------------------------------------------------------------

	PointerList::TPointerList::~TPointerList()
	{
//		for (int i = 0; i < List.size(); i++)
//			free(List[i]); //# FreeMemory accepts one parameter only;
		List.clear();
		// inherited::Destroy();
	}

	//-------------------------------------------------------------------------

	int PointerList::TPointerList::get_myNumList()
	{
		return NumInList;
	}

	//-------------------------------------------------------------------------

	int PointerList::TPointerList::get_myActiveItem()
	{
		return ActiveItem;
	}


	//-------------------------------------------------------------------------

	int PointerList::TPointerList::Add(void* P)
	{
		int result = 0;
		NumInList++;
		if (NumInList > MaxAllocated)
		{
			std::vector <void*> myTmp;
			MaxAllocated = MaxAllocated + IncrementSize;
			myTmp.resize(MaxAllocated);
			for (int idx = 0; idx < List.size(); idx++)
				myTmp[idx] = List[idx];
			List = myTmp;
		};

		List[NumInList - 1] = P;
		result = NumInList;
		ActiveItem = result;
		return result;
	}

	//-------------------------------------------------------------------------

	void PointerList::TPointerList::Set_New(void* Value)
	{
		Add(Value);
	}

	//-------------------------------------------------------------------------

	void* PointerList::TPointerList::Get_Active()
	{
		void* result = nullptr;
		if ((ActiveItem > 0) && (ActiveItem <= NumInList))
			result = Get(ActiveItem);
		else
			result = nullptr;
		return result;
	}

	//-------------------------------------------------------------------------

	void* PointerList::TPointerList::Get_First()
	{
		void* result = nullptr;
		if (NumInList > 0)
		{
			ActiveItem = 1;
			result = (void*)List[ActiveItem - 1];
		}
		else
		{
			ActiveItem = 0;
			result = nullptr;
		}
		return result;
	}

	//-------------------------------------------------------------------------

	void* PointerList::TPointerList::Get_Next()
	{
		void* result = nullptr;
		if (NumInList > 0)
		{
			++ActiveItem;
			if (ActiveItem > NumInList)
			{
				ActiveItem = NumInList;
				result = nullptr;
			}
			else
				result = (void*)List[ActiveItem - 1];
		}
		else
		{
			ActiveItem = 0;
			result = nullptr;
		}
		return result;
	}

	//-------------------------------------------------------------------------

	void* PointerList::TPointerList::Get(int i)
	{
		void* result = NULL;
		if ((i < 1) || (i > NumInList))
			result = NULL;
		else
		{
			result = List[i - 1];
			ActiveItem = i;
		}
		return result;
	}

	void PointerList::TPointerList::Clear()
	{
		ActiveItem = 0;
		NumInList = 0;
	}



}// namespace PointerList





