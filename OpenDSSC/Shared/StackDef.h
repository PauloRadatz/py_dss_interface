#ifndef StackDefH
#define StackDefH

#include "System.h"
#include "Sysutils.h"

#include "Arraydef.h"

namespace StackDef
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/

class TStackBase : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
protected:
	int NumItems;
	int Increment;
	int MaxItems;
public:
	TStackBase(int initSize);
	virtual ~TStackBase();
	void Clear();
	int Size();
	TStackBase();
};  // simple pointer stack

class TPstack : public TStackBase
{
public:
	typedef TStackBase inherited;	
//private:
	Arraydef::PPointerArray Items;
public:
	TPstack(int initSize);
	virtual ~TPstack();
	void Push(void* P);
	void* Pop();
	TPstack();
};  // simple integer stack

class TiStack : public TStackBase
{
public:
	typedef TStackBase inherited;	
//private:
	Arraydef::pIntegerArray Items;
public:
	TiStack(int initSize);
	virtual ~TiStack();
	void Push(int P);
	int Pop();
	TiStack();
};


}  // namespace StackDef

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace StackDef;
#endif

#endif // StackDefH




