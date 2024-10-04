
#pragma hdrstop

#include "TSData.h"
#include "DSSGlobals.h"

using namespace std;


namespace TSData
{

TTSDataObj::TTSDataObj(TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TTSDataObj::TTSDataObj(String ClassName) : inherited(ClassName) {}
TTSDataObj::TTSDataObj() {}


const int NumPropsThisClass = 3;  // Creates superstructure for all Line objects

TTSData::TTSData()
{
	;
	Class_Name = "TSData";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}



TTSData::~TTSData()
{
	// inherited::Destroy();
}


void TTSData::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();
	(PropertyName)[1 - 1] = "DiaShield";
	(PropertyName)[2 - 1] = "TapeLayer";
	(PropertyName)[3 - 1] = "TapeLap";
	(PropertyHelp)[1 - 1] = "Diameter over tape shield; same units as radius; no default.";
	(PropertyHelp)[2 - 1] = "Tape shield thickness; same units as radius; no default.";
	(PropertyHelp)[3 - 1] = "Tape Lap in percent; default 20.0";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

int TTSData::NewObject(const String ObjName)
{
	int result = 0;
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TTSDataObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

int TTSData::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveConductorDataObj = (TConductorDataObj*) ElementList.Get_Active();
	ActiveDSSObject[ActorID] = ActiveConductorDataObj;
	/*# with TTSDataObj(ActiveConductorDataObj) do */
	{
		auto with0 = ((TTSDataObj*) ActiveConductorDataObj);
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while(Param.length() > 0)
		{
			if(ParamName.length() == 0)
				++ParamPointer;
			else
				ParamPointer = CommandList.Getcommand(ParamName);
			if((ParamPointer > 0) && (ParamPointer <= NumProperties))
				with0->Set_PropertyValue(ParamPointer,Param);
			switch(ParamPointer)
			{
				case 	0:
				DoSimpleMsg(String("Unknown parameter \"") + ParamName
	           + "\" for Object \""
	           + Class_Name
	           + "."
	           + with0->get_Name()
	           + "\"", 101);
				break;
				case 	1:
				with0->FDiaShield = Parser[ActorID]->MakeDouble_();
				break;
				case 	2:
				with0->FTapeLayer = Parser[ActorID]->MakeDouble_();
				break;
				case 	3:
				with0->FTapeLap = Parser[ActorID]->MakeDouble_();
				break;
        // Inherited parameters
				default:
				ClassEdit(ActiveConductorDataObj, ParamPointer - NumPropsThisClass);
				break;
			}

      /*Check for critical errors*/
			switch(ParamPointer)
			{
				case 	1:
				if(with0->FDiaShield <= 0.0)
					DoSimpleMsg(String("Error: Diameter over shield must be positive for TapeShieldData ") + with0->get_Name(), 999);
				break;
				case 	2:
				if(with0->FTapeLayer <= 0.0)
					DoSimpleMsg(String("Error: Tape shield thickness must be positive for TapeShieldData ") + with0->get_Name(), 999);
				break;
				case 	3:
				if((with0->FTapeLap < 0.0) || (with0->FTapeLap > 100.0))
					DoSimpleMsg(String("Error: Tap lap must range from 0 to 100 for TapeShieldData ") + with0->get_Name(), 999);
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
	}
	return result;
}

int TTSData::MakeLike(const String TSName)
{
	int result = 0;
	TTSDataObj* OtherData = nullptr;
	int i = 0;
	result = 0;
	OtherData = ((TTSDataObj*) Find(TSName));
	if(OtherData != nullptr)
		/*# with TTSDataObj(ActiveConductorDataObj) do */
		{
			auto with0 = ((TTSDataObj*) ActiveConductorDataObj);
			int stop = 0;
			with0->FDiaShield = OtherData->FDiaShield;
			with0->FTapeLayer = OtherData->FTapeLayer;
			with0->FTapeLap = OtherData->FTapeLap;
			ClassMakeLike(OtherData);
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherData->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in TapeShield MakeLike: \"") + TSName
	           + "\" Not Found.", 102);
	return result;
}

int TTSData::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TTSData.Init", -1);
	result = 0;
	return result;
}  // Returns active line code string

String TTSData::Get_Code()
{
	String result;
	result = ((TTSDataObj*) ElementList.Get_Active())->get_Name();
	return result;
}  // sets the  active TSData

void TTSData::Set_Code(const String Value)
{
	TTSDataObj* TSDataObj = nullptr;
	ActiveConductorDataObj = nullptr;
	TSDataObj = (TTSDataObj*) ElementList.Get_First();
	while(TSDataObj != nullptr)
	{
		if(CompareText(TSDataObj->get_Name(), Value) == 0)
		{
			ActiveConductorDataObj = TSDataObj;
			return;
		}
		TSDataObj = (TTSDataObj*) ElementList.Get_Next();
	}
	DoSimpleMsg(String("TSData: \"") + Value + "\" not Found.", 103);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTSData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TTSDataObj::TTSDataObj(TDSSClass* ParClass, const String TSDataName)
 : inherited(ParClass, TSDataName),
			FDiaShield(-1.0),
			FTapeLayer(-1.0),
			FTapeLap(20.0)
{
	Set_Name(LowerCase(TSDataName));
	DSSObjType = ParClass->DSSClassType;
	InitPropertyValues(0);
}

TTSDataObj::~TTSDataObj()
{
	// inherited::Destroy();
}

//---------------------------------------------------------------------------------

double TTSDataObj::get_FDiaShield()
{
	return FDiaShield;
}

//---------------------------------------------------------------------------------

double TTSDataObj::get_FTapeLayer()
{
	return FTapeLayer;
}

//---------------------------------------------------------------------------------

double TTSDataObj::get_FTapeLap()
{
	return FTapeLap;
}

//---------------------------------------------------------------------------------


void TTSDataObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = NumPropsThisClass, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); }
			switch(i)
			{
				case 	1:
				WriteLn(f, Format("%.6g",FDiaShield));
				break;
				case 	2:
				WriteLn(f, Format("%.6g",FTapeLayer));
				break;
				case 	3:
				WriteLn(f, Format("%.2g",FTapeLap));
				break;
				default:
				  ;
				break;
			}
		}
	}
}

String TTSDataObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	1:
		result = Format("%.6g", FDiaShield);
		break;
		case 	2:
		result = Format("%.6g", FTapeLayer);
		break;
		case 	3:
		result = Format("%.2g", FTapeLap);
		break;
		default:
		result = inherited::GetPropertyValue(Index - NumPropsThisClass);
		break;
	}
	return result;
}

int TTSDataObj::GetNumProperties(int ArrayOffset)
{
	return inherited::GetNumProperties(NumPropsThisClass + ArrayOffset);
}

void TTSDataObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"-1");
	Set_PropertyValue(2,"-1");
	Set_PropertyValue(3,"20.0");
	inherited::InitPropertyValues(ArrayOffset + NumPropsThisClass);
}




}  // namespace TSData





