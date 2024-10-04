
#pragma hdrstop

#include "WireData.h"

#include "DSSGlobals.h"
#include <algorithm>

using namespace std;


namespace WireData
{

	TWireDataObj::TWireDataObj(TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
	TWireDataObj::TWireDataObj(String ClassName) : inherited(ClassName) {}
	TWireDataObj::TWireDataObj() {}


	const int NumPropsThisClass = 0; // because they were all moved to ConductorData
	  // Creates superstructure for all Line objects

	TWireData::TWireData()
	{
		;
		Class_Name = "WireData";
		DSSClassType = DSS_OBJECT;
		ActiveElement = 0;
		DefineProperties();
		std::string* slc = Slice(PropertyName, NumProperties);
		CommandList = TCommandList(slc, NumProperties);
		delete[] slc;
		CommandList.set_AbbrevAllowed(true);
	}

	TWireData::~TWireData()
	{
		// inherited::Destroy();
	}


	void TWireData::DefineProperties()
	{
		NumProperties = NumPropsThisClass;
		inherited::CountProperties();   // Get inherited property count
		AllocatePropertyArrays();
		ActiveProperty = NumPropsThisClass - 1;
		inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
	}

	int TWireData::NewObject(const String ObjName)
	{
		int result = 0;
		// create a new object of this class and add to list
		  /*# with ActiveCircuit[ActiveActor] do */
		{

			ActiveDSSObject[ActiveActor] = new TWireDataObj(this, ObjName);
			result = AddObjectToList(ActiveDSSObject[ActiveActor]);
		}
		return result;
	}

	int TWireData::Edit(int ActorID)
	{
		int result = 0;
		int ParamPointer = 0;
		String ParamName;
		String Param;
		result = 0;
		// continue parsing with contents of Parser
		ActiveConductorDataObj = (TConductorDataObj*)ElementList.Get_Active();
		ActiveDSSObject[ActorID] = ActiveConductorDataObj;
		/*# with ActiveConductorDataObj do */
		{
			auto with0 = ActiveConductorDataObj;
			ParamPointer = 0;
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
			while (Param.length() > 0)
			{
				if (ParamName.length() == 0)
					++ParamPointer;
				else
					ParamPointer = CommandList.Getcommand(ParamName);
				if ((ParamPointer > 0) && (ParamPointer <= NumProperties))
					with0->Set_PropertyValue(ParamPointer,Param);
				switch (ParamPointer)
				{
				case 	0:
					DoSimpleMsg(String("Unknown parameter \"") + ParamName
						+ "\" for Object \""
						+ Class_Name
						+ "."
						+ with0->get_Name()
						+ "\"", 101);
					break;
					// Inherited parameters
				default:
					inherited::ClassEdit(ActiveConductorDataObj, ParamPointer - NumPropsThisClass);
					break;
				}
				ParamName = Parser[ActorID]->GetNextParam();
				Param = Parser[ActorID]->MakeString_();
			}
		}
		return result;
	}

	int TWireData::MakeLike(const String WireName)
	{
		int result = 0;
		TWireDataObj* OtherWireData = nullptr;
		int i = 0;
		result = 0;
		OtherWireData = ((TWireDataObj*)Find(WireName));
		if (OtherWireData != nullptr)
			/*# with ActiveConductorDataObj do */
		{
			auto with0 = ActiveConductorDataObj;
			int stop = 0;
			ClassMakeLike(OtherWireData);
			for (stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherWireData->Get_PropertyValue(i));
			}
			result = 1;
		}
		else
			DoSimpleMsg(String("Error in Wire MakeLike: \"") + WireName + "\" Not Found.", 102);
		return result;
	}

	int TWireData::Init(int Handle, int ActorID)
	{
		int result = 0;
		DoSimpleMsg("Need to implement TWireData.Init", -1);
		result = 0;
		return result;
	}  // Returns active line code string

	String TWireData::Get_Code()
	{
		String result;
		result = ((TWireDataObj*)ElementList.Get_Active())->get_Name();
		return result;
	}  // sets the  active WireData

	void TWireData::Set_Code(const String Value)
	{
		TWireDataObj* WireDataObj = nullptr;
		ActiveConductorDataObj = nullptr;
		WireDataObj = (TWireDataObj*) ElementList.Get_First();
		while (WireDataObj != nullptr)
		{
			if (CompareText(WireDataObj->get_Name(), Value) == 0)
			{
				ActiveConductorDataObj = WireDataObj;
				return;
			}
			WireDataObj = (TWireDataObj*) ElementList.Get_Next();
		}
		DoSimpleMsg(String("WireData: \"") + Value + "\" not Found.", 103);
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	//      TWireData Obj
	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

	TWireDataObj::TWireDataObj(TDSSClass* ParClass, const String WireDataName)
		: inherited(ParClass, WireDataName)
	{
		Set_Name(LowerCase(WireDataName));
		DSSObjType = ParClass->DSSClassType;
		InitPropertyValues(0);
		/*ratings     :=  Nil;*/
	}

	TWireDataObj::~TWireDataObj()
	{
		// inherited::Destroy();
	}


	void TWireDataObj::DumpProperties(System::TTextRec& f, bool Complete)
	{
		inherited::DumpProperties(f, Complete);
	}

	String TWireDataObj::GetPropertyValue(int Index)
	{
		String result;
		result = inherited::GetPropertyValue(Index);
		return result;
	}

	int TWireDataObj::GetNumProperties(int ArrayOffset)
	{
		return NumPropsThisClass;
	}

	void TWireDataObj::InitPropertyValues(int ArrayOffset)
	{
		inherited::InitPropertyValues(ArrayOffset + NumPropsThisClass);
	}


}






