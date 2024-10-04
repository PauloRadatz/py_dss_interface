
#pragma hdrstop


#include "DSSObject.h"

#include "Utilities.h"


using namespace std;

namespace DSSObject
{

	TDSSObject::TDSSObject(TDSSClass* ParClass, string ClassName) : inherited(ParClass->get_myClass_name()) {}
	TDSSObject::TDSSObject(string ClassName) : inherited(ClassName) {}
	TDSSObject::TDSSObject() {}


	void TDSSObject::ClearPropSeqArray()
	{
		int i = 0;
		int stop = 0;
		PropSeqCount = 0;
		for (stop = ParentClass->NumProperties, i = 1; i <= stop; i++)
		{
			PrpSequence[i - 1] = 0;
		}
	}

	TDSSObject::TDSSObject(DSSClass::TDSSClass* ParClass)
		: inherited(ParClass->get_myClass_name()),
		PropSeqCount(0),
		DSSObjType(0),
		ParentClass(nullptr),
		ClassIndex(0),
		HasBeenSaved(false),
		Flag(false)
	{
		ParentClass = ParClass;
		FPropertyValue.resize(ParentClass->NumProperties);
		PrpSequence.resize(ParentClass->NumProperties);
		DSSObjType = 0;
		HasBeenSaved = false;
	}

	TDSSObject::~TDSSObject()
	{
		int i = 0;
		int stop = 0;
		/*for (stop = ParentClass->NumProperties, i = 1; i <= stop; i++)
		{
			FPropertyValue[i - 1] = "";
		}*/
		FPropertyValue.clear();
		PrpSequence.clear();
		//inherited::Destroy();
	}


	void TDSSObject::DumpProperties(TTextRec& f, bool Complete)
	{
		System::WriteLn(f);
		{ System::Write(f, "New "); System::Write(f, Get_myPName()); System::Write(f, "."); System::WriteLn(f, get_Name()); }
	}

	int TDSSObject::Edit(int ActorID)
	{
		int result = 0;
		ParentClass->Set_Active(ClassIndex);
		result = ParentClass->Edit(ActorID);
		return result;
	}

	string DSSObject::TDSSObject::GetPropertyValue(int Index)
	{
		string result;
		result = FPropertyValue[Index - 1];  // Default Behavior   for all DSS Objects
		return result;
	}

	String TDSSObject::Get_PropertyValue(int Index)
	{
		String result;
		result = GetPropertyValue(Index);  // This is virtual function that may call routine
		return result;
	}

	void TDSSObject::InitPropertyValues(int ArrayOffset)
	{
		Set_PropertyValue(ArrayOffset + 1,""); //Like   Property

		 // Clear propertySequence Array  after initialization
		ClearPropSeqArray();
	}

	void TDSSObject::SaveWrite(System::TTextRec& f)
	{
		int iProp = 0;
		String Str;
		bool LShpFlag = false;
		bool NptsRdy = false;    // Created to  know the that object is loadshape

	   /*Write only properties that were explicitly set in the
	   final order they were actually set*/
		LShpFlag = false;
		NptsRdy = false;
		/*# with ParentClass do */
		{
			auto with0 = ParentClass;
			if (ParentClass->get_myClass_name() == "LoadShape")
			{
				iProp = 1;
				LShpFlag = true;
			}
			else
				iProp = GetNextPropertySet(0); // Works on ActiveDSSObject
		}
		//  The part above was created to guarantee that the npts property will be the
		//  first to be declared when saving LoadShapes
		while (iProp > 0)
		{
			Str = Trim(String(Get_PropertyValue(iProp)));
			if (CompareText(Str, "----") == 0)
				Str = ""; // set to ignore this property
			if (Str.length() > 0)
			{
				/*# with ParentClass do */
				{
					auto with1 = ParentClass;
					{ Write(f, " "); Write(f, (with1->PropertyName)[(with1->RevPropertyIdxMap)[iProp - 1] - 1]); }
				}
				{ Write(f, "="); Write(f, CheckForBlanks(Str)); }
			}
			if (LShpFlag)
			{
				iProp = GetNextPropertySet(0);   // starts over
				LShpFlag = false;  // The npts is already processed
		// Flag to not repeat it again
				NptsRdy = true;
			}
			else
			{
				iProp = GetNextPropertySet(iProp);
				if (NptsRdy)
				{
					if (iProp == 1)
						iProp = GetNextPropertySet(iProp);
				}
			}
		}
	}
	// Find next larger property sequence number
	// return 0 if none found

	int TDSSObject::GetNextPropertySet(int Idx)
	{
		int result = 0;
		int i = 0;
		int smallest = 0;
		int stop = 0;
		smallest = 9999999; // some big number
		result = 0;
		if (Idx > 0)
			Idx = PrpSequence[Idx - 1];
		for (stop = ParentClass->NumProperties, i = 1; i <= stop; i++)
		{
			if (PrpSequence[i - 1] > Idx)
			{
				if (PrpSequence[i - 1] < smallest)
				{
					smallest = PrpSequence[i - 1];
					result = i;
				}
			}
		}
		return result;
	}

	void TDSSObject::Set_Name(const String Value)
	{

		// If renamed, then let someone know so hash list can be updated;
		if (Get_myLName().length() > 0)
			ParentClass->ElementNamesOutOfSynch = true;
		Set_myLName(Value);
	}

	string TDSSObject::get_Name()
	{
		String result;
		result = Get_myLName();
		return result;
	}

	void TDSSObject::Set_PropertyValue(int Index, const String Value)
	{
		FPropertyValue[Index - 1] = Value;

		// Keep track of the order in which this property was accessed for Save Command
		++PropSeqCount;
		PrpSequence[Index - 1] = PropSeqCount;
	}

}

