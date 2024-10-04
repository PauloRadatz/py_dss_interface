
#pragma hdrstop

#include "StackDef.h"


using namespace std;

namespace StackDef
{

TStackBase::TStackBase() {}
TPstack::TPstack() {}
TiStack::TiStack() {}



TStackBase::TStackBase(int initSize)
 : NumItems(0),
			Increment(0),
			MaxItems(0)
{
	;
	MaxItems = initSize;
	Increment = initSize;
}

TStackBase::~TStackBase()
{
	// inherited::Destroy();
}


void TStackBase::Clear()
{
	NumItems = 0;
}

int TStackBase::Size()
{
	int result = 0;
	result = NumItems;
	return result;
}

//-------------------------------------------------------

TPstack::TPstack(int initSize)
 : inherited(initSize),
			Items((PPointerArray) malloc(sizeof((Items)[1 - 1]) * MaxItems))
{
}

TPstack::~TPstack()
{
	ReallocMem(Items, 0);
	// inherited::Destroy();
}


void TPstack::Push(void* P)
{
	++NumItems;
	if(NumItems > MaxItems)
	{
		MaxItems += Increment;
		ReallocMem(Items, sizeof((Items)[1 - 1]) * MaxItems);
	}
	(Items)[NumItems - 1] = P;
}

void* TPstack::Pop()
{
	void* result = nullptr;
	if(NumItems > 0)
	{
		result = (Items)[NumItems - 1];
		--NumItems;
	}
	else
	result = nullptr;
	return result;
}

TiStack::TiStack(int initSize)
	: inherited(initSize),
	Items((pIntegerArray)malloc(sizeof((Items)[1 - 1])* MaxItems))
{
}

TiStack::~TiStack()
{
	ReallocMem(Items, 0);
	// inherited::Destroy();
}


void TiStack::Push(int P)
{
	++NumItems;
	if(NumItems > MaxItems)
	{
		MaxItems += Increment;
		ReallocMem(Items, sizeof(Items[1 - 1]) * MaxItems);
	}
	Items[NumItems - 1] = P;
}

int TiStack::Pop()
{
	int result = 0;
	if(NumItems > 0)
	{
		result = Items[NumItems - 1];
		--NumItems;
	}
	else
	result = 0;
	return result;
}




}  // namespace StackDef




