
#pragma hdrstop

#include "GrowthShape.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Ucomplex.h"
#include "mathutil.h"
#include "Utilities.h"

using namespace std;
using namespace Arraydef;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace ParserDel;
using namespace System;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace GrowthShape
{

TGrowthShapeObj::TGrowthShapeObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TGrowthShapeObj::TGrowthShapeObj(String ClassName) : inherited(ClassName) {}
TGrowthShapeObj::TGrowthShapeObj() {}


TGrowthShapeObj* ActiveGrowthShapeObj = nullptr;
const int NumPropsThisClass = 6;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TGrowthShape::TGrowthShape()
{
	;
	Class_Name = "GrowthShape";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(false);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TGrowthShape::~TGrowthShape()
{


    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGrowthShape::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "npts";     // Number of points to expect
	PropertyName[2 - 1] = "year";     // vextor of year values
	PropertyName[3 - 1] = "mult";     // vector of multiplier values corresponding to years
	PropertyName[4 - 1] = "csvfile";   // Switch input to a csvfile                 (year, mult)
	PropertyName[5 - 1] = "sngfile";  // switch input to a binary file of singles  (year, mult)
	PropertyName[6 - 1] = "dblfile";   // switch input to a binary file of doubles (year, mult)
	PropertyHelp[1 - 1] = "Number of points to expect in subsequent vector.";
	PropertyHelp[2 - 1] = "Array of year values, or a text file spec, corresponding to the multipliers. "
	           "Enter only those years where the growth changes. "
	           "May be any integer sequence -- just so it is consistent. See help on Mult.";
	PropertyHelp[3 - 1] = String("Array of growth multiplier values, or a text file spec, corresponding to the year values. " "Enter the multiplier by which you would multiply the previous year's load to get the present year's.") + CRLF
	           + CRLF
	           + "Examples:"
	           + CRLF
	           + CRLF
	           + "  Year = [1, 2, 5]   Mult=[1.05, 1.025, 1.02]."
	           + CRLF
	           + "  Year= (File=years.txt) Mult= (file=mults.txt)."
	           + CRLF
	           + CRLF
	           + "Text files contain one value per line.";
	PropertyHelp[4 - 1] = "Switch input of growth curve data to a csv file containing (year, mult) points, one per line.";
	PropertyHelp[5 - 1] = "Switch input of growth curve data to a binary file of singles "
	           "containing (year, mult) points, packed one after another.";
	PropertyHelp[6 - 1] = "Switch input of growth curve data to a binary file of doubles "
	           "containing (year, mult) points, packed one after another.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGrowthShape::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TGrowthShapeObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGrowthShape::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	pDoubleArray YrBuffer = nullptr;
	int i = 0;
	result = 0;
  // continue parsing with contents of Parser
	ActiveGrowthShapeObj = ((TGrowthShapeObj*) ElementList.Get_Active());
	ActiveDSSObject[ActorID] = ActiveGrowthShapeObj;
	/*# with ActiveGrowthShapeObj do */
	{
		auto with0 = ActiveGrowthShapeObj;
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
	           + "\"", 600);
				break;
				case 	1:
				with0->npts = Parser[ActorID]->MakeInteger_();
				break;
				case 	2:
				{
					int stop = 0;
					with0->Year = (pIntegerArray) realloc(with0->Year, sizeof(long) * with0->npts);
					YrBuffer	= (pDoubleArray) realloc(YrBuffer, sizeof(double) * with0->npts);
					with0->npts = InterpretDblArray(Param, with0->npts, YrBuffer);  // Parser.ParseAsVector(Npts, Yrbuffer);
					for(stop = with0->npts, i = 1; i <= stop; i++)
					{
						(with0->Year)[i - 1] = (int) Round((YrBuffer)[i - 1]);
					}
					with0->BaseYear = (with0->Year)[1 - 1];
					free(YrBuffer); //# FreeMemory accepts one parameter only;
				}
				break;
				case 	3:
				{
					with0->Multiplier = (pDoubleArray) realloc(with0->Multiplier, sizeof(double) * with0->npts);
					with0->npts = InterpretDblArray(Param, with0->npts, with0->Multiplier);   //Parser.ParseAsVector(Npts, Multiplier);
				}
				break;
				case 	4:
				DoCSVFile(Param);
				break;
				case 	5:
				DoSngFile(Param);
				break;
				case 	6:
				DoDblFile(Param);
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveGrowthShapeObj, ParamPointer - NumPropsThisClass);
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		} /*WHILE*/
		with0->ReCalcYearMult();
	} /*WITH*/
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGrowthShape::MakeLike(const String ShapeName)
{
	int result = 0;
	TGrowthShapeObj* OtherGrowthShape = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line code in the present collection*/
	OtherGrowthShape = ((TGrowthShapeObj*) Find(ShapeName));
	if(OtherGrowthShape != nullptr)
		/*# with ActiveGrowthShapeObj do */
		{
			auto with0 = ActiveGrowthShapeObj;
			int stop = 0;
			with0->npts = OtherGrowthShape->npts;
			with0->Multiplier = (pDoubleArray) realloc(with0->Multiplier, sizeof(double) * with0->npts); 
			for(stop = with0->npts, i = 1; i <= stop; i++)
			{
				(with0->Multiplier)[i - 1] = (OtherGrowthShape->Multiplier)[i - 1];
			}
			with0->Year = (pIntegerArray) realloc(with0->Year, sizeof(long) * with0->npts);
			for(stop = with0->npts, i = 1; i <= stop; i++)
			{
				(with0->Year)[i - 1] = (OtherGrowthShape->Year)[i - 1];
			}
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherGrowthShape->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in GrowthShape MakeLike: \"") + ShapeName
	           + "\" Not Found.", 601);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TGrowthShape::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TGrowthShape.Init", -1);
	result = 0;
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Returns active line code string

String TGrowthShape::Get_Code()
{
	String result;
	TGrowthShapeObj* GrowthShapeObj = nullptr;
	GrowthShapeObj = ((TGrowthShapeObj*) ElementList.Get_Active());
	result = GrowthShapeObj->get_Name();
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // sets the  active GrowthShape

void TGrowthShape::Set_Code(const String Value)
{
	TGrowthShapeObj* GrowthShapeObj = nullptr;
	ActiveGrowthShapeObj = nullptr;
	GrowthShapeObj = ((TGrowthShapeObj*) ElementList.Get_First());
	while(GrowthShapeObj != nullptr)
	{
		if(CompareText(GrowthShapeObj->get_Name(), Value) == 0)
		{
			ActiveGrowthShapeObj = GrowthShapeObj;
			return;
		}
		GrowthShapeObj = ((TGrowthShapeObj*) ElementList.Get_Next());
	}
	DoSimpleMsg(String("GrowthShape: \"") + Value + "\" not Found.", 602);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGrowthShape::DoCSVFile(const String FileName)
{
	String dummy;
	System::TTextRec f = {};
	int i = 0;
	String s;
	try
	{
		AssignFile(f, FileName);
		Reset(f);
		IOResultToException();
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 603);
		CloseFile(f); System::InOutRes = 0;
		return;
	}
	try
	{
		/*# with ActiveGrowthShapeObj do */
		{
			auto with0 = ActiveGrowthShapeObj;
			i = 0;
			while((!Eof(f)) && (i < with0->npts))
			{
				++i;
				ReadLn(f, s);  /*Use AuxParser to allow flexible formats*/
				/*# with AuxParser[ActiveActor] do */
				{
					
             // Readln(F,Year^[i], Multiplier^[i]);
					AuxParser[ActiveActor]->SetCmdString(s);
					dummy = AuxParser[ActiveActor]->GetNextParam();
					with0->Year[i - 1] = AuxParser[ActiveActor]->MakeInteger_();
					dummy = AuxParser[ActiveActor]->GetNextParam();
					(with0->Multiplier)[i - 1] = AuxParser[ActiveActor]->MakeDouble_();
				}
			}
			CloseFile(f);
		}
	}
	catch(exception& e)
	{
		{
			DoSimpleMsg(String("Error Processing CSV File: \"") + FileName
	           + ". "
	           + (string) e.what(), 604);
			CloseFile(f);
			return;
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGrowthShape::DoSngFile(const String FileName)
{
	System::TTypedFile<float> f;
	float Y = 0.0F;
	float m = 0.0F;
	int i = 0;
	try
	{
		System::AssignFile(f, FileName);
		System::Reset(f);
		IOResultToException();
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 605);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	try
	{
		/*# with ActiveGrowthShapeObj do */
		{
			auto with0 = ActiveGrowthShapeObj;
			i = 0;
			while((!Eof(f)) && (i < with0->npts))
			{
				++i;
				{ System::Read(f, &Y); System::Read(f, &m); }
				with0->Year[i - 1] = (int) Round(Y);
				with0->Multiplier[i - 1] = m;
			}
			System::CloseFile(f);
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Processing GrowthShape File: \"") + FileName, 606);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGrowthShape::DoDblFile(const String FileName)
{
	System::TTypedFile<double> f;
	int i = 0;
	double Yr = 0.0;
	try
	{
		System::AssignFile(f, FileName);
		System::Reset(f);
		IOResultToException();
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 607);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	try
	{
		/*# with ActiveGrowthShapeObj do */
		{
			auto with0 = ActiveGrowthShapeObj;
			i = 0;
			while((!Eof(f)) && (i < with0->npts))
			{
				++i;
				{ System::Read(f, &Yr); System::Read(f, &(with0->Multiplier)[i - 1]); }
				with0->Year[i - 1] = (int) Round(Yr);
			}
			System::CloseFile(f);
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Processing GrowthShape File: \"") + FileName, 608);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TGrowthShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TGrowthShapeObj::TGrowthShapeObj(TDSSClass* ParClass, const String GrowthShapeName)
 : inherited(ParClass),
			npts(0),
			NYears(30),
			BaseYear(0),
			Year(nullptr),
			YearMult(nullptr),
			Multiplier(nullptr)
{
	Set_Name(LowerCase(GrowthShapeName));
	DSSObjType = ParClass->DSSClassType;
	Year = nullptr;
	Multiplier = nullptr;
	YearMult = new double[NYears];
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TGrowthShapeObj::~TGrowthShapeObj()
{
	free(Year);
	free(Multiplier);
	delete[] YearMult;
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This function returns the multiplier to use for a load in the given year.
// The first year specified in the curve is the base year.  The Base value
// is the beginning of the first year.

double TGrowthShapeObj::GetMult(int Yr)
{
	double result = 0.0;
	int Index = 0;
	result = 1.0;    // default return value if no points in curve
	if(npts > 0)         // Handle Exceptional cases
	{
		Index = Yr - BaseYear;
		if(Index > 0)     // Returns 1.0 for base year or any year previous
		{
			if(Index > NYears)  // Make some more space
			{
				NYears = Index + 10;
				YearMult = (pDoubleArray) realloc(YearMult, sizeof(double) * NYears);
				ReCalcYearMult();
			}
			result = (YearMult)[Index - 1];
		}
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TGrowthShapeObj::ReCalcYearMult()
{
	int i = 0;
	int DataPtr = 0;
	int Yr = 0;
	double Mult = 0.0;
	double MultInc = 0.0;
  // Fill up the YearMult array with total yearly multiplier from base year
	int stop = 0;
	Mult = (Multiplier)[1 - 1];
	MultInc = Mult;
	YearMult[1 - 1] = Mult;
	DataPtr = 1;
	Yr = BaseYear;
	for(stop = NYears, i = 2; i <= stop; i++)
	{
		++Yr;
		if(DataPtr < npts)
		{
			if((Year)[DataPtr + 1 - 1] == Yr)
			{
				++DataPtr;
				MultInc = (Multiplier)[DataPtr - 1];
			}
		}
		Mult = Mult * MultInc;
		YearMult[i - 1] = Mult;
	}
}

void TGrowthShapeObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			switch(i)
			{
				case 	2: case 3:
				{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[i - 1]); System::Write(f, "=("); System::Write(f, Get_PropertyValue(i)); System::WriteLn(f, L')'); }
				break;
				default:
				{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[i - 1]); System::Write(f, L'='); System::WriteLn(f, Get_PropertyValue(i)); }
				break;
			}
		}
	}
}

String TGrowthShapeObj::GetPropertyValue(int Index)
{
	String result;
	int i = 0;
	switch(Index)
	{
		case 	2:
		 case 3:
		result = "(";
		break;
		default:
		result = "";
		break;
	}
	switch(Index)
	{
		int stop;
		case 	2:
		for(stop = npts, i = 1; i <= stop; i++)
		{
			result = result + Format("%-d, ", (int)Year[i - 1]);
		}
		break;
		case 	3:
		for(stop = npts, i = 1; i <= stop; i++)
		{
			result = result + Format("%-g, ", Multiplier[i - 1]);
		}
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	switch(Index)
	{
		case 	2:
		 case 3:
		result = result + ")";
		break;
		default:
		  ;
		break;
	}
	return result;
}

void TGrowthShapeObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"0");     // Number of points to expect
	Set_PropertyValue(2,"");     // vextor of year values
	Set_PropertyValue(3,"");     // vector of multiplier values corresponding to years
	Set_PropertyValue(4,"");   // Switch input to a csvfile                 (year, mult)
	Set_PropertyValue(5,"");  // switch input to a binary file of singles  (year, mult)
	Set_PropertyValue(6,"");   // switch input to a binary file of doubles (year, mult)
	inherited::InitPropertyValues(NumPropsThisClass);
}




}  // namespace GrowthShape





