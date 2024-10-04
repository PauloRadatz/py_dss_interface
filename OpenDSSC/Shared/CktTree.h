#ifndef CktTreeH
#define CktTreeH

#include "System.h"

#include "Arraydef.h"
#include "StackDef.h"
#include "PointerList.h"
#include "CktElement.h"
#include "d2c_structures.h"
#include "PDElement.h"
#include "PCElement.h"


typedef vector<TList> TAdjArray;

namespace CktTree
{


/*
  ----------------------------------------------------------
  Copyright (c) 2008-2023, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
*/
/*  Change Log

   8/12/99  Added level number to node
*/

class TCktTreeNode : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
	PointerList::TPointerList* FChildBranches;  // List of CktTreeNode pointers
	int NumToBuses;
	int ToBusPtr;
	vector<int> ToBusList;
	TCktTreeNode* Get_FirstChild();
	TCktTreeNode* Get_NextChild();
	TCktTreeNode* Get_Parent();
	void Set_AddChild(TCktTreeNode* const Value);
	void Set_AddObject(void* Value);
	int Get_NumChildren();
	int Get_NumObjects();
	int Get_ToBusReference();
	void Set_ToBusReference(int Value);
	void* Get_FirstObject();
	void* Get_NextObject();
//protected:
	bool ChildAdded;
	int LexicalLevel;
	TCktTreeNode* FParentBranch;
	PointerList::TPointerList* FShuntObjects;  // Generic objects attached to the tree at this node
public:
	void* CktObject;    // Pointer to the circuit object referenced
	int FromBusReference;
	int VoltBaseIndex;
	int FromTerminal;
	bool IsLoopedHere;
	bool IsParallel;
	bool IsDangling;
	void* LoopLineObj;
	TCktTreeNode(TCktTreeNode* const pParent, const void* pSelfObj);
	virtual ~TCktTreeNode();
	void ResetToBusList();
//__published:
public:
	TCktTreeNode();
};

class TZoneEndsList : public System::TObject
{
public:
	typedef TObject inherited;	
private:
	PointerList::TPointerList* EndNodeList;
	Arraydef::pIntegerArray EndBuses;
protected:
public:
	int NumEnds;
	TZoneEndsList();
	virtual ~TZoneEndsList();
	void Add(TCktTreeNode* const Node, int EndBusRef);
	int Get(int i, TCktTreeNode*& Node);
//__published:
};

class TCktTree : public System::TObject
{
public:
	typedef TObject inherited;	
//private:
	TCktTreeNode* FirstNode;
	StackDef::TPstack* ForwardStack;
	void* Get_Forward();
	void* Get_Backward();
	void* Get_First();
	void* Get_Parent();
	void* Get_FirstObject();
	void* Get_NextObject();
	void* Get_Active();
	int Get_Level();
	void Set_New(void* Value);
	void Set_NewObject(void* Value);
	void Set_Active(void* P);  // Set present node to this value
	void PushAllChildren();
//protected:
public:
	TCktTreeNode* PresentBranch;
	TZoneEndsList* ZoneEndsList;
	TCktTree();
	virtual ~TCktTree();
	void StartHere();   // Start Forward Search at the present location
                              // can also use active
	void AddNewChild(void* Value, int BusRef, int TerminalNo);
       //Property NewChild  :Pointer Write Set_NewChild; // Adds child to present, but doesn't change present
//__published:
};

   // build a tree of connected elements beginning at StartElement
   // Analyze = TRUE will check for loops, isolated components, and parallel lines (takes longer)
TCktTree* GetIsolatedSubArea(CktElement::TDSSCktElement* StartElement, bool Analyze = false);
void BuildActiveBusAdjacencyLists(TAdjArray& lstPD, TAdjArray& lstPC, int ActorID);
void FreeAndNilBusAdjacencyLists(TAdjArray& lstPD, TAdjArray& lstPC);


}  // namespace CktTree

#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE)
using namespace CktTree;
#endif

#endif // CktTreeH




