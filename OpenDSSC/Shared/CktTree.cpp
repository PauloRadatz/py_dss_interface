#pragma hdrstop

#include "CktTree.h"

#include "DSSGlobals.h"
#include "Circuit.h"

using namespace std;

namespace CktTree
{

	TCktTreeNode::TCktTreeNode() {}



	TCktTreeNode::TCktTreeNode(TCktTreeNode* const pParent, const void* pSelfObj)
	 : FChildBranches(new PointerList::TPointerList(2)),
				NumToBuses(0),
				ToBusPtr(0),
				ChildAdded(false),
				LexicalLevel(0),
				FParentBranch(nullptr),
				FShuntObjects(nullptr),
				CktObject(nullptr),
				FromBusReference(0),
				VoltBaseIndex(0),
				FromTerminal(0),
				IsLoopedHere(false),
				IsParallel(false),
				IsDangling(false),
				LoopLineObj(nullptr)
	{
		;
		CktObject = const_cast<void*>(pSelfObj);
		FParentBranch = const_cast<TCktTreeNode*>(pParent);
		if(FParentBranch != nullptr)
			LexicalLevel = FParentBranch->LexicalLevel + 1;
		else
			LexicalLevel = 0;
		FShuntObjects = new PointerList::TPointerList(1);
		FromBusReference = 0;
		VoltBaseIndex = 0; // Index to voltage base list used by energymeter and maybe others
		ToBusList.clear();
		ChildAdded = false;
		 // TEMc - initialize some topology variables, 10/2009
		IsDangling = true;
		IsLoopedHere = false;
		IsParallel = false;
		LoopLineObj = nullptr;
	}

	TCktTreeNode::~TCktTreeNode()
	{
		void* pChild = nullptr;
		void* pNext = nullptr;
		TCktTreeNode* TempNode = nullptr;
		pChild = FChildBranches->Get_First();
		while(pChild != nullptr)
		{
			pNext = FChildBranches->Get_Next();
			TempNode = ((TCktTreeNode*) pChild);
			delete TempNode;
			pChild = pNext;
		}
		ToBusList.clear();
		delete FChildBranches;
		delete FShuntObjects;
		// inherited::Destroy();
	}


	void TCktTreeNode::Set_AddChild(TCktTreeNode* const Value)
	{
		FChildBranches->Set_New((void*) Value);
		ChildAdded = true;
	}

	void TCktTreeNode::Set_AddObject(void* Value)
	{
		FShuntObjects->Set_New(Value);
	}

	TCktTreeNode* TCktTreeNode::Get_FirstChild()
	{
		TCktTreeNode* result = nullptr;
		result = ((TCktTreeNode*) FChildBranches->Get_First());
		return result;
	}

	TCktTreeNode* TCktTreeNode::Get_NextChild()
	{
		TCktTreeNode* result = nullptr;
		result = ((TCktTreeNode*) FChildBranches->Get_Next());
		return result;
	}

	TCktTreeNode* TCktTreeNode::Get_Parent()
	{
		TCktTreeNode* result = nullptr;
		result = FParentBranch;
		return result;
	}

	TCktTree::TCktTree()
	 : FirstNode(nullptr),
				ForwardStack(new TPstack(20)),
				PresentBranch(nullptr),
				ZoneEndsList(nullptr)
	{
		;
		PresentBranch = nullptr;
		ZoneEndsList = new TZoneEndsList();
	}

	TCktTree::~TCktTree()
	{
		delete ForwardStack;
		if(ZoneEndsList != nullptr)
			delete ZoneEndsList;
		if(FirstNode != nullptr)
			delete FirstNode;
		// inherited::Destroy();
	}


	void TCktTree::Set_New(void* Value)
	{
		PresentBranch = new TCktTreeNode(PresentBranch, Value);
		if(FirstNode == nullptr)
			FirstNode = PresentBranch;
	}

	void TCktTree::AddNewChild(void* Value, int BusRef, int TerminalNo)
	{
		TCktTreeNode* TempNode = nullptr;
		if(PresentBranch == nullptr)
		{
			Set_New(Value);
		}
		else
		{
			TempNode = new TCktTreeNode(PresentBranch, Value);
			/*# with TempNode do */
			{
				auto with0 = TempNode;
				with0->FromBusReference = BusRef;
				with0->FromTerminal = TerminalNo;
			}
			PresentBranch->Set_AddChild(TempNode);
		}
	}

	void TCktTree::Set_NewObject(void* Value)
	{
		if(PresentBranch != nullptr)
		{
			PresentBranch->Set_AddObject(Value);
		}
	}

	void TCktTree::PushAllChildren()
	{
		void* pChild = nullptr;
		if(PresentBranch != nullptr)
		 // Push all children of present node onto stack
		{
			pChild = PresentBranch->Get_FirstChild();
			while(pChild != nullptr)
			{
				ForwardStack->Push(pChild);
				pChild = PresentBranch->Get_NextChild();
			}
			PresentBranch->ChildAdded = false;
		}
	}

	void* TCktTree::Get_Forward()
	{
		void* result = nullptr;
	// MoveForward from Present node

	// If we have added children to the present node since we opened it push em on
		if(PresentBranch != nullptr)
		{
			if(PresentBranch->ChildAdded)
				PushAllChildren();

	  // If the forward stack is empty push stuff on it to get started
		}
		if(ForwardStack->Size() == 0)
			PushAllChildren();
		PresentBranch = ((TCktTreeNode*) ForwardStack->Pop());
		PushAllChildren();   // push all children of latest
		if(PresentBranch != nullptr)
			result = PresentBranch->CktObject;
		else
			result = nullptr;
		return result;
	}

	void* TCktTree::Get_Backward()
	{
		void* result = nullptr;

	// Move Backwardfrom Present node and reset forward stack
		PresentBranch = PresentBranch->Get_Parent();
		ForwardStack->Clear();
		if(PresentBranch != nullptr)
			result = PresentBranch->CktObject;
		else
			result = nullptr;
		return result;
	}

	void* TCktTree::Get_Parent()
	{
		void* result = nullptr;
		if(PresentBranch->FParentBranch != nullptr)
			result = PresentBranch->FParentBranch->CktObject;
		else
			result = nullptr;
		return result;
	}

	void* TCktTree::Get_First()
	{
		void* result = nullptr;
	// go to beginning and reset forward stack
		PresentBranch = FirstNode;
		ForwardStack->Clear();
		PushAllChildren();
		if(PresentBranch != nullptr)
			result = PresentBranch->CktObject;
		else
			result = nullptr;
		return result;
	}

	void* TCktTree::Get_FirstObject()
	{
		void* result = nullptr;
		if(PresentBranch != nullptr)
			result = PresentBranch->FShuntObjects->Get_First();
		else
			result = nullptr;
		return result;
	}

	void* TCktTree::Get_NextObject()
	{
		void* result = nullptr;
		if(PresentBranch != nullptr)
			result = PresentBranch->FShuntObjects->Get_Next();
		else
			result = nullptr;
		return result;
	}

	void* TCktTree::Get_Active()
	{
		void* result = nullptr;
		if(PresentBranch != nullptr)
			result = PresentBranch->CktObject;
		else
			result = nullptr;
		return result;
	}

	void TCktTree::Set_Active(void* P)
	{
		void* Temp = nullptr;
		Temp = Get_First();
		while(Temp != nullptr)
		{
			if(PresentBranch->CktObject == P)
				break;
			Temp = Get_Forward();
		}
		ForwardStack->Clear();
	}

	void TCktTree::StartHere()
	{
		ForwardStack->Clear();
		if(PresentBranch != nullptr)
			ForwardStack->Push(PresentBranch);
	}

	int TCktTree::Get_Level()
	{
		int result = 0;
		if(PresentBranch != nullptr)
			result = PresentBranch->LexicalLevel;
		else
			result = 0;
		return result;
	}

	int TCktTreeNode::Get_NumChildren()
	{
		int result = 0;
		result = FChildBranches->get_myNumList();
		return result;
	}

	int TCktTreeNode::Get_NumObjects()
	{
		int result = 0;
		result = FShuntObjects->get_myNumList();
		return result;
	}

	/* TZoneEndsList */

	void TZoneEndsList::Add(TCktTreeNode* const Node, int EndBusRef)
	{
		++NumEnds;
		EndNodeList->Set_New((void*) Node);
		EndBuses = (pIntegerArray) realloc(EndBuses, sizeof(EndBuses) * NumEnds);
		EndBuses[NumEnds - 1] = EndBusRef;
	}

	TZoneEndsList::TZoneEndsList()
	 : EndNodeList(new PointerList::TPointerList(10)),
				NumEnds(0)
	{
		EndBuses = NULL;
		NumEnds = 0;
	}

	TZoneEndsList::~TZoneEndsList()
	{
		delete EndNodeList;
		EndBuses = (pIntegerArray) realloc(EndBuses, 0);
		// inherited;
	}


	int TZoneEndsList::Get(int i, TCktTreeNode*& Node)
	{
		int result = 0;
		Node = ((TCktTreeNode*) EndNodeList->Get(i));
		result = EndBuses[i - 1];
		return result;
	}
	/*Sequentially access the To Bus list if more than one with each invocation of the property*/

	int TCktTreeNode::Get_ToBusReference()
	{
		int result = 0;
		if(NumToBuses == 1)
		{
			result = ToBusList[1 - 1];  // Always return the first
		}
		else
		{
			++ToBusPtr;
			if(ToBusPtr > NumToBuses)
			{
				result = -1;
				ToBusPtr = 0;  // Ready for next sequence of access
			}
			else
			result = ToBusList[ToBusPtr - 1];
		}
		return result;
	}

	void TCktTreeNode::Set_ToBusReference(int Value)
	{
		++NumToBuses;
//		ToBusList.resize(NumToBuses);
		ToBusList.push_back(Value);
	}

	void TCktTreeNode::ResetToBusList()
	{
		ToBusPtr = 0;
	}

	void* TCktTreeNode::Get_FirstObject()
	{
		void* result = nullptr;
		result = FShuntObjects->Get_First();
		return result;
	}

	void* TCktTreeNode::Get_NextObject()
	{
		void* result = nullptr;
		result = FShuntObjects->Get_Next();
		return result;
	}

	////////////////////////////////////////////////////////////////////////
	//
	// utility code for building a connected tree starting from a circuit element
	//
	////////////////////////////////////////////////////////////////////////

	// sources are excluded from the PC element list, so this is a brute-force search

	void GetSourcesConnectedToBus(int BusNum, TCktTree* BranchList, bool Analyze)
	{
		TPCElement* PSrc = nullptr;      // Sources are special PC elements
		/*# with ActiveCircuit[ActiveActor] do */
		{
		
			PSrc = ((TPCElement*) ActiveCircuit[ActiveActor]->Sources.Get_First());
			while(PSrc != nullptr)
			{
				if( ( (TDSSCktElement*) PSrc )->Get_Enabled())
				{
					if(Analyze || (!((TDSSCktElement*)PSrc)->Checked))
					{
						if((((TDSSCktElement*)PSrc)->Terminals)[0].BusRef == BusNum)  // ?Connected to this bus ?
						{
							if(Analyze)
							{
								((TDSSCktElement*)PSrc)->IsIsolated = false;
								BranchList->PresentBranch->IsDangling = false;
							}
							if(!((TDSSCktElement*)PSrc)->Checked)
							{
								BranchList->Set_NewObject(PSrc);
								((TDSSCktElement*)PSrc)->Checked = true;
							}
						}
					}
				}
				PSrc = ((TPCElement*) ActiveCircuit[ActiveActor]->Sources.Get_Next());
			}
		}/*With*/
	}

	void GetPCElementsConnectedToBus(TList* adjLst, TCktTree* BranchList, bool Analyze)
	{
		TDSSCktElement* P = nullptr;
		int i = 0;
		int stop = 0;
		for(stop = adjLst->size() - 1, i = 0; i <= stop; i++)
		{
			P = (TDSSCktElement*) (*adjLst)[i];
			if(P->Get_Enabled())
			{
				if(Analyze)
				{
					P->IsIsolated = false;
					BranchList->PresentBranch->IsDangling = false;
				}
				if(!P->Checked)
				{
					BranchList->Set_NewObject(P);
					P->Checked = true;
				}
			}
		}
	}

	void FindAllChildBranches(TList* adjLst, int BusNum, TCktTree* BranchList, bool Analyze, TDSSCktElement* ActiveBranch)
	{
		int i = 0;
		int j = 0;
		TDSSCktElement* P = nullptr;
		int stop = 0;

		for(stop = adjLst->size() - 1, i = 0; i <= stop; i++)
		{
			P = ((TDSSCktElement*)(*adjLst)[i]);
			if(P->Get_Enabled() && !(P == ActiveBranch))
			{
				if(Analyze || (!P->Checked))
				{
					if((!IsShuntElement(P)) && AllTerminalsClosed(P))
					{
						int stop1 = 0;
						for(stop1 = P->Get_NTerms(), j = 1; j <= stop1; j++)
						{
							if(BusNum == ((P->Terminals)[ j - 1 ].BusRef))
							{
								if(Analyze)
								{
									P->IsIsolated = false;
									BranchList->PresentBranch->IsDangling = false;
									if(P->Checked && (BranchList->Get_Level() > 0))
									{
										BranchList->PresentBranch->IsLoopedHere = true;
										BranchList->PresentBranch->LoopLineObj = P;
										if(IslineElement(P) && IslineElement(ActiveBranch))
										{
											if(CheckParallel(ActiveBranch, P))
												BranchList->PresentBranch->IsParallel = true;
										}
									}
								}
								if(!P->Checked)
								{
									BranchList->AddNewChild(P, BusNum, j);
									(P->Terminals)[ j - 1 ].Checked = true;
									P->Checked = true;
									break;
								}
							}
						}
					}
				}
			}
		}
	}

	void GetShuntPDElementsConnectedToBus(TList* adjLst, TCktTree* BranchList, bool Analyze)
	{
		TDSSCktElement* P = nullptr;
		int i = 0;
		int stop = 0;
	
		for(stop = adjLst->size() - 1, i = 0; i <= stop; i++)
		{
			P = ((TDSSCktElement*) (*adjLst)[i]);
			if(P->Get_Enabled() && IsShuntElement(P))
			{
				if(Analyze)
				{
					P->IsIsolated = false;
					BranchList->PresentBranch->IsDangling = false;
				}
				if(!P->Checked)
				{
					BranchList->Set_NewObject(P);
					P->Checked = true;
				}
			}
		}
	}

	TCktTree* GetIsolatedSubArea(TDSSCktElement* StartElement, bool Analyze/*# = false*/)
	{
		TCktTree*		result = nullptr;
		int				TestBusNum = 0;
		TCktTree*		BranchList = nullptr;
		int				iTerm = 0;
		TDSSCktElement* TestBranch = nullptr;
		TDSSCktElement* TestElement = nullptr;

		TAdjArray		lstPD;
		TAdjArray		lstPC;

		lstPD.resize(1);
		lstPC.resize(1);
		lstPD = ActiveCircuit[ActiveActor]->GetBusAdjacentPDLists(ActiveActor);
		lstPC = ActiveCircuit[ActiveActor]->GetBusAdjacentPCLists(ActiveActor);
		BranchList = new TCktTree();
		TestElement = StartElement;
		BranchList->Set_New((void*) TestElement);
		if(Analyze)
			TestElement->IsIsolated = false;
		TestElement->LastTerminalChecked = 0;  // We'll check things connected to both sides

	  // Check off this element so we don't use it again
		TestElement->Checked = true;

	  // Now start looking for other branches
	  // Finds any branch connected to the TestBranch and adds it to the list
	  // Goes until end of circuit, another energy meter, an open terminal, or disabled device.
		TestBranch = TestElement;
		while(TestBranch != nullptr)
		{
			int stop = 0;
			for(stop = TestBranch->Get_NTerms(), iTerm = 1; iTerm <= stop; iTerm++)
			{
				if(!((TestBranch->Terminals)[iTerm - 1].Checked))
			// Now find all pc Elements connected to the bus on this end of branch
			// attach them as generic objects to cktTree node.
				{
					TestBusNum = (TestBranch->Terminals)[iTerm - 1].BusRef;
					BranchList->PresentBranch->Set_ToBusReference(TestBusNum);   // Add this as a "to" bus reference
					if(TestBusNum > 0)
					{
						ActiveCircuit[ActiveActor]->Buses[TestBusNum - 1]->BusChecked = true;
						GetSourcesConnectedToBus(TestBusNum, BranchList, Analyze);
						GetPCElementsConnectedToBus(&(lstPC[TestBusNum]), BranchList, Analyze);
						GetShuntPDElementsConnectedToBus(&(lstPD[TestBusNum]), BranchList, Analyze);
						FindAllChildBranches(&(lstPD[TestBusNum]), TestBusNum, BranchList, Analyze, TestBranch);
					}
				}
			}   /*FOR iTerm*/
			TestBranch = ((TDSSCktElement*) BranchList->Get_Forward());
		} /*WHILE*/
		result = BranchList;
		return result;
	}

	void BuildActiveBusAdjacencyLists(TAdjArray& lstPD, TAdjArray& lstPC, int ActorID)
	{
		int		i = 0,
				j = 0,
				NBus = 0;
		TDSSCktElement* pCktElement = nullptr;
		int stop = 0;
		NBus = ActiveCircuit[ActorID]->NumBuses;
	  // Circuit.Buses is effectively 1-based; bus 0 is ground
		lstPD.resize(NBus + 1);
		lstPC.resize(NBus + 1);
		for(stop = NBus, i = 0; i <= stop; i++)
		{
			lstPD[i] = TList(); // default capacity should be enough
			lstPC[i] = TList();
		}
		pCktElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->PCElements.Get_First());
		while(pCktElement != nullptr)
		{
			if(pCktElement->Get_Enabled())
			{
				i = ((pCktElement->Terminals)[0].BusRef);
				lstPC[i].push_back(pCktElement);
			}
			pCktElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->PCElements.Get_Next());
		}
		pCktElement = ((TDSSCktElement*)ActiveCircuit[ActorID]->PDElements.Get_First());
	  //Put only eligible PDElements in the list
		while (pCktElement != nullptr)
		{
			if(pCktElement->Get_Enabled())
			{
				if(IsShuntElement(pCktElement))
				{
					i = (pCktElement->Terminals)[0].BusRef;
					lstPC[i].push_back(pCktElement);
				}
				else
				{
					if(AllTerminalsClosed(pCktElement))
					{
						int stop = 0;
						for(stop = pCktElement->Get_NTerms(), j = 1; j <= stop; j++)
						{
							i = (pCktElement->Terminals[ j - 1 ].BusRef);
							lstPD[i].push_back(pCktElement);
						}
					}
				}
			}
			pCktElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->PDElements.Get_Next());
		}
	}

	void FreeAndNilBusAdjacencyLists(TAdjArray& lstPD, TAdjArray& lstPC)
	{
		int i = 0;
		int stop = 0;
		for(stop = lstPD.size(), i = 0; i < stop; i++)
		{
			lstPD[i].clear();
			lstPC[i].clear();
		}
		lstPD.clear();
		lstPC.clear();
	}




}  // namespace CktTree




