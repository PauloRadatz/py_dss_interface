//#include <vcl.h>
#pragma hdrstop

#include "HashList.h"
#include "Utilities.h"

using namespace std;

using namespace Hashlist;



THashList::THashList()
	: NumElementsAllocated(0),
	NumElements(0),
	AllocationInc(0),
	LastFind(0),
	LastHash(0),
	InitialAllocation(0)
{
	NumLists = 1;
	StringPtr.resize(100);
	ListPtr.resize(100);
	for (int i = 0; i < ListPtr.size(); i++)
	{
		ListPtr[i].Str.clear();
		ListPtr[i].Idx.clear();
		ListPtr[i].NAllocated = 0;
		ListPtr[i].Nelem = 0;
	}
}



THashList::THashList(unsigned int Nelements)
 : NumElementsAllocated(0),
			NumLists(0),
			NumElements(0),
			AllocationInc(0),
			LastFind(0),
			LastHash(0),
			InitialAllocation(0)
{
	int i = 0;
	unsigned int Elementsperlist = 0;
	int stop = 0;
	;
	NumElements = 0;
	InitialAllocation = Nelements;
	StringPtr.clear();  // Allocmem(Sizeof(StringPtr^[1]) * Nelements);
	NumLists = (unsigned int) Round(sqrt((long double) Nelements));
	if(NumLists < 1)
		NumLists = 1;  // make sure at least one list
	Elementsperlist = (unsigned int)(Nelements / NumLists) + 1;
	AllocationInc = Elementsperlist;
	ListPtr.resize(NumLists);
	for(stop = NumLists, i = 1; i <= stop; i++)
	{
         /*Allocate initial Sublists to zero; alocated on demand*/
		/*# with ListPtr^[i] do */
		{
			
			ListPtr[i - 1].Str.clear();
			ListPtr[i - 1].Idx.clear();
			ListPtr[i - 1].NAllocated = 0;
			ListPtr[i - 1].Nelem = 0;
		}
	}
	LastFind = 0;
	LastHash = 0;
	LastSearchString = "";
}

THashList::~THashList()
{
	int i = 0;
	int j = 0;
	int stop = 0;
	for(stop = NumLists - 1, i = 1; i <= stop; i++)
	{
         /*DeAllocated  Sublists*/
		/*# with ListPtr^[i] do */
		try
		{
			if (ListPtr.size() != 0)
			{
				if ( (int) ListPtr[i - 1].Str.size() >= 0)
					ListPtr[i - 1].Str.clear(); //# FreeMemory accepts one parameter only;

				if (ListPtr[i - 1].Idx.size() != 0)
					ListPtr[i - 1].Idx.clear(); //# FreeMemory accepts one parameter only;
			}
		}
		catch (...)
		{
			//
		}
	}
	if (ListPtr.size() != 0)
		ListPtr.clear(); //# FreeMemory accepts one parameter only;
	if (((int)StringPtr.size() > 0))
		StringPtr.clear(); //# FreeMemory accepts one parameter only;

	// inherited::Destroy();
}


void ReallocStr(pStringArray(s), int OldSize, int NewSize)
{
	pStringArray X = nullptr;
	X = new std::string[NewSize];   // Zero fills new string pointer array (important!)
	if(OldSize > 0)
	{
		Move(s, X, OldSize);
		free(s); //# FreeMemory accepts one parameter only;
	}
	s = X;
}
// Make a bigger block to hold the pointers to the strings

void THashList::ResizeSubList(TSubList& SubList)
{
	unsigned int OldAllocation = 0;
    // resize by reasonable amount
	/*# with SubList do */
	{
		OldAllocation = SubList.NAllocated;
		SubList.NAllocated = OldAllocation + AllocationInc;
		SubList.Str.resize(SubList.NAllocated);
		SubList.Idx.resize(SubList.NAllocated);
	}
}

/*   This one was for AnsiStrings and just moved up to 8 bytes into an integer
Function THashList.Hash(Const S:String):Integer;

VAR
    Hashvalue:UInt64;

BEGIN
   HashValue := Uint64(0);
 // Only hash first 8 characters

   Move(S[1], HashValue, min(8, Length(S)));
   Result := (Hashvalue mod NumLists) + 1;
END;
*/

/* OLD HASH FUNCTION -- only hashes 1st 8 chars

Function THashList.Hash(Const S:String):Cardinal;

VAR
    Hashvalue:Word;
    i:Integer;

    {Use Delphi Math function: it is declared as inline}
    {Function Min(const a,b:Integer):Integer;
    BEGIN If a<b Then result := a else result := b; END;  }
BEGIN
   HashValue := 0;
 // Only hash first 8 characters
   FOR i := 1 to min(8,Length(S)) DO HashValue := HashValue*2 + ord(S[i]);
   Result := (Hashvalue mod NumLists) + 1;
END;
*/

/*   New supposedly fast hash method      */

unsigned int THashList::Get_NumElements()
{
	return NumElements;
}


unsigned int THashList::Hash(const String s)
{
	unsigned int result = 0;
	unsigned int Hashvalue = 0;
	int i = 0;

  /*per Stackoverflow.com*/
	int stop = 0;
	Hashvalue = 0;
	//for (stop = (int)s.length(), i = 1; i <= stop; i++)
	for (i = 1; i < static_cast<int>(s.length())-1; i++)
	{
		Hashvalue = (unsigned int) (((Hashvalue << 5) | (Hashvalue >> 27)) ^ ((unsigned int) s[i]));
	}
	result = (Hashvalue % NumLists) + 1;
	return result;
}

// make linear string list larger

void THashList::ResizeStrPtr()
{
	unsigned int OldAllocation = 0;
	std::vector <string> NewPointer;
	OldAllocation = NumElementsAllocated;
	NumElementsAllocated = OldAllocation + AllocationInc * NumLists;

   // Allocate and zero fill new string space (important!)
	NewPointer.resize(NumElementsAllocated);

   //move old stuff to new location then dispose of old stuff
	if(OldAllocation > 0)
	{
		for (int idx = 0 ; idx < OldAllocation; idx++)
			NewPointer[idx] = StringPtr[idx];
		//free(StringPtr); //# FreeMemory accepts one parameter only;
	}
	//StringPtr.resize(NumElementsAllocated);
	//for (int idx = 0; idx < NumElementsAllocated; idx++)
	//	StringPtr[idx] = NewPointer[idx];
	StringPtr = NewPointer;  // this is easier and faster
	NewPointer.clear();
}

int THashList::Add(const String s)
{
	int result = 0;
	unsigned int HashNum = 0;
	String Ss;
	Ss = ToLowerCaseStr(s);
	HashNum = Hash(Ss);
	NumElements++;
	if(NumElements > NumElementsAllocated)
		ResizeStrPtr();
	result = (int) NumElements;
	/*# with ListPtr^[HashNum] do */
	{
		
		ListPtr[HashNum - 1].Nelem++;
		if(ListPtr[HashNum - 1].Nelem > ListPtr[HashNum - 1].NAllocated)
			ResizeSubList(ListPtr[HashNum - 1]);
	}
	/*# with ListPtr^[HashNum] do */
	{
		
		ListPtr[HashNum - 1].Str[ListPtr[HashNum - 1].Nelem - 1] = Ss;   // make copy of whole string, lower case
		StringPtr[NumElements - 1] = Ss;   // increments count to string
		ListPtr[HashNum - 1].Idx[ListPtr[HashNum - 1].Nelem - 1] = NumElements;
	}
	return result;
}

int THashList::Find(const String s)
{
	int result = 0;
	int i = 0;
	LastSearchString = LowerCase(s);
	LastHash = Hash(LastSearchString);
	result = 0;
	LastFind = 0;
	/*# with ListPtr^[LastHash] do */
	{
		
		int stop = 0;
		for(stop = ListPtr[LastHash - 1].Nelem, i = 1; i <= stop; i++)
		{
			if(CompareStr(LastSearchString, (ListPtr[LastHash - 1].Str)[i - 1]) == 0)
			{
				result = ListPtr[LastHash - 1].Idx[i - 1];    // Return the index of this element
				LastFind = (unsigned int) i;
				break;
			}
		}
	}
	return result;
}

// Begin search in same list as last

int THashList::FindNext()
{
	int result = 0;
	int i = 0;
	result = 0;  // Default return
	++LastFind; // Start with next item in hash list
	if((LastHash > 0) && (LastHash <= NumLists))
		/*# with ListPtr^[LastHash] do */
		{
			
			int stop = 0;
			for(stop = ListPtr[LastHash - 1].Nelem, i = LastFind; i <= stop; i++)
			{
				if(CompareStr(LastSearchString, ListPtr[LastHash - 1].Str[i - 1]) == 0)
				{
					result = ListPtr[LastHash - 1].Idx[i - 1];    // Return the index of this element
					LastFind = (unsigned int) i;
					break;
				}
			}
		}
	return result;
}
/*Just make a linear search and test each string until a string is found that
 matches all the characters entered in S*/

int THashList::FindAbbrev(const String s)
{
	int result = 0;
	String Test1;
	String Test2;
	int i = 0;
	result = 0;
	if(s.length() > 0)
	{
		int stop = 0;
		Test1 = LowerCase(s);
		for (int stop = NumElements, j = 0; j < stop; j++)
		{
			if (StringPtr[j] == "")
			{
				StringPtr[j].assign("showreports");
			}
		}
		for(stop = NumElements, i = 1; i <= stop; i++)
		{
			
			Test2 = StringPtr[i - 1].substr(0, Test1.length());
			if(CompareStr(Test1, Test2) == 0)
			{
				result = i;
				break;
			}
		}
	}
	return result;
}

String THashList::Get(unsigned int i)
{
	String result;
	if((i > 0) && (i <= NumElements))
		result = StringPtr[i - 1];
	else
		result = "";
	return result;
}

/*
  This procedure creates a new set of string lists and copies the
  old strings into the new, hashing for the new number of lists.

*/

void THashList::Expand(unsigned int NewSize)
{
	std::vector < std::string > NewStringPtr;
	unsigned int NewNumLists = 0;
	unsigned int Elementsperlist = 0;
	pSubListArray NewListPtr;
	unsigned int HashNum = 0;
	String s;
	unsigned int OldNumLists = 0;
	NewListPtr.clear();
	int i = 0;
	int j = 0;
	if(NewSize > NumElementsAllocated)
	{
		int stop = 0;
		OldNumLists = NumLists;
		NewStringPtr.resize(NewSize);
		NewNumLists = (unsigned int) Round(sqrt((long double) NewSize));
		Elementsperlist = (unsigned int)(NewSize / NewNumLists) + 1;
		if(NewNumLists < 1)
			NewNumLists = 1;  // make sure at least one list
		NewListPtr.resize(NewNumLists);
		for(stop = NumLists, i = 1; i <= stop; i++)
		{
         /*Allocate initial Sublists*/
			/*# with NewListPtr^[i] do */
			{
				
				NewListPtr[i - 1].Str.resize(Elementsperlist);
				NewListPtr[i - 1].Idx.resize(Elementsperlist);
				NewListPtr[i - 1].NAllocated = Elementsperlist;
				NewListPtr[i - 1].Nelem = 0;
			}
		}
		NumLists = NewNumLists;  // Has to be set so Hash function will work

/*Add elements from old Hash List to New Hash List*/
		for(stop = NumElements, i = 1; i <= stop; i++)
		{
			s = StringPtr[i - 1];
			HashNum = Hash(s);
			/*# with NewListPtr^[HashNum] do */
			{
				
				NewListPtr[HashNum - 1].Nelem++;
				if(NewListPtr[HashNum - 1].Nelem > NewListPtr[HashNum - 1].NAllocated)
					ResizeSubList(NewListPtr[HashNum - 1]);
			}
			/*# with NewListPtr^[HashNum] do */
			{
				
				NewListPtr[HashNum - 1].Str[NewListPtr[HashNum - 1].Nelem - 1] = s;
				NewStringPtr[NumElements - 1] = NewListPtr[HashNum - 1].Str[NewListPtr[HashNum - 1].Nelem - 1];
				NewListPtr[HashNum - 1].Idx[NewListPtr[HashNum - 1].Nelem - 1] = (unsigned int) i;
			}
		}

/*Dump the old StringPtr and ListPtr storage*/
		for(stop = OldNumLists, i = 1; i <= stop; i++)
		{
         /*DeAllocate  Sublists*/
			/*# with ListPtr^[i] do */
			{
				
				ListPtr[i - 1].Str.clear(); //# FreeMemory accepts one parameter only;
				ListPtr[i - 1].Idx.clear(); //# FreeMemory accepts one parameter only;
			}
		}
		ListPtr.clear(); //# FreeMemory accepts one parameter only;
		StringPtr.clear(); //# FreeMemory accepts one parameter only;

/*Assign new String and List Pointers*/
		StringPtr = NewStringPtr;
		ListPtr = NewListPtr;
		NumElementsAllocated = NewSize;
	}
}

void THashList::DumpToFile(const String FName)
{
	System::TTextRec f = {};
	int i = 0;
	int j = 0;
	int stop = 0;
	AssignFile(f, FName);
	Rewrite(f);
	IOResultToException();
	WriteLn(f, "Number of Hash Lists = " + to_string(NumLists) + "Number of Elements = " + to_string(NumElements));
	WriteLn(f);
	WriteLn(f, L"Hash List Distribution");
	for(stop = NumLists, i = 1; i <= stop; i++)
	{
		/*# with ListPtr^[i] do */
		{
			
			WriteLn(f,"List = " + to_string(i) + ", Number of elements = " + to_string((int)ListPtr[i - 1].Nelem) );
		}
	}
	WriteLn(f);
	for(stop = NumLists, i = 1; i <= stop; i++)
	{
		/*# with ListPtr^[i] do */
		{
			
			int stop1 = 0;
			WriteLn(f, "List = " + to_string(i) + ", Number of elements = " + to_string(ListPtr[i - 1].Nelem));
			for(stop1 = ListPtr[i - 1].Nelem, j = 1; j <= stop1; j++)
			{
				{ Write(f, '\"'); Write(f, ListPtr[i - 1].Str[j - 1]); Write(f, L"\"  Idx= "); WriteLn(f, to_string(ListPtr[i - 1].Idx[j - 1]), 0); }
			}
		}
		WriteLn(f);
	}
	WriteLn(f, L"LINEAR LISTING...");
	for(stop = NumElements, i = 1; i <= stop; i++)
	{
		{ Write(f, i, 3); Write(f, L" = \""); Write(f, StringPtr[i - 1]); WriteLn(f, L'\"'); }
	}
	CloseFile(f);
}

void THashList::Clear()
{
	int i = 0;
	int j = 0;
	int stop = 0;
	for(stop = NumLists, i = 1; i <= stop; i++)
	{
		/*# with ListPtr^[i] do */
		{
			
			int stop1 = 0;
			ListPtr[i - 1].Nelem = 0;
			for(stop1 = ListPtr[i - 1].NAllocated, j = 1; j <= stop1; j++)
			{
				ListPtr[i - 1].Str[j - 1] = "";
			}
		}
	}
	for(stop = NumElementsAllocated, i = 1; i <= stop; i++)
	{
		StringPtr[i - 1] = "";
	}
	NumElements = 0;
}








