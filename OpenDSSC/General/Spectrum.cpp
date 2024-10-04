
#pragma hdrstop

#include "Spectrum.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
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
using namespace Utilities;

namespace Spectrum
{

TSpectrumObj::TSpectrumObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TSpectrumObj::TSpectrumObj(String ClassName) : inherited(ClassName) {}
TSpectrumObj::TSpectrumObj() {}


TSpectrumObj* ActiveSpectrumObj = nullptr;
const int NumPropsThisClass = 5;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TSpectrum::TSpectrum()
{
	;
	Class_Name = "Spectrum";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TSpectrum::~TSpectrum()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TSpectrum::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();
	PropertyName[0] = "NumHarm";
	PropertyName[1] = "harmonic";
	PropertyName[2] = "%mag";
	PropertyName[3] = "angle";
	PropertyName[4] = "CSVFile";
	PropertyHelp[0] = "Number of frequencies in this spectrum. (See CSVFile)";
	PropertyHelp[1] = String("Array of harmonic values. You can also use the syntax") + CRLF
	           + "harmonic = (file=filename)     !for text file one value per line"
	           + CRLF
	           + "harmonic = (dblfile=filename)  !for packed file of doubles"
	           + CRLF
	           + "harmonic = (sngfile=filename)  !for packed file of singles ";
	PropertyHelp[2] = String("Array of magnitude values, assumed to be in PERCENT. You can also use the syntax") + CRLF
	           + "%mag = (file=filename)     !for text file one value per line"
	           + CRLF
	           + "%mag = (dblfile=filename)  !for packed file of doubles"
	           + CRLF
	           + "%mag = (sngfile=filename)  !for packed file of singles ";
	PropertyHelp[3] = String("Array of phase angle values, degrees.You can also use the syntax") + CRLF
	           + "angle = (file=filename)     !for text file one value per line"
	           + CRLF
	           + "angle = (dblfile=filename)  !for packed file of doubles"
	           + CRLF
	           + "angle = (sngfile=filename)  !for packed file of singles ";
	PropertyHelp[4] = "File of spectrum points with (harmonic, magnitude-percent, angle-degrees) values, one set of 3 per line, in CSV format. "
	           "If fewer than NUMHARM frequencies found in the file, NUMHARM is set to the smaller value.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TSpectrum::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TSpectrumObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TSpectrum::Edit(int ActorID)
{
	int result = 0;
	int i = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	int iZeroPoint = 0;  // for error trapping
	result = 0;
  // continue parsing with contents of Parser
	ActiveSpectrumObj = ((TSpectrumObj*) ElementList.Get_Active());
	ActiveDSSObject[ActorID] = ActiveSpectrumObj;
	/*# with ActiveSpectrumObj do */
	{
		auto with0 = ActiveSpectrumObj;
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
	           + with0->get_Name()
	           + "\"", 650);
				break;
				case 	1:
				{
					int stop = 0;
					with0->NumHarm = Parser[ActorID]->MakeInteger_();
					with0->AngleArray.resize(with0->NumHarm); // Make a dummy Angle array
					for(stop = with0->NumHarm, i = 1; i <= stop; i++)
					{
						with0->AngleArray[i - 1] = 0.0;
					}
				}
				break;
				case 	2:
				{
					with0->HarmArray.resize(with0->NumHarm);
					with0->NumHarm = InterpretDblArray(Param, with0->NumHarm, &(with0->HarmArray[0]));
				}
				break;
				case 	3:
				{
					int stop = 0;
					with0->puMagArray.resize(with0->NumHarm);
					with0->NumHarm = InterpretDblArray(Param, with0->NumHarm, &(with0->puMagArray[0]));
					for(stop = with0->NumHarm, i = 1; i <= stop; i++)
					{
						with0->puMagArray[i - 1] = with0->puMagArray[i - 1] * 0.01;
					}  // convert to per unit
				}
				break;
				case 	4:
				{
					with0->AngleArray.resize(with0->NumHarm);
					with0->NumHarm = InterpretDblArray(Param, with0->NumHarm, &(with0->AngleArray[0]));
				}
				break;
				case 	5:
				DoCSVFile(Param);
				break;
          // Inherited parameters
				default:
				ClassEdit(ActiveSpectrumObj, ParamPointer - NumPropsThisClass);
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}       /*WHILE*/
		if(!with0->HarmArray.empty())   // Check this after HarmArray is allocated  2/20/2018
		{
			if(with0->HarmArrayHasaZero(iZeroPoint))
				DoSimpleMsg(Format("Error: Zero frequency detected in Spectrum.%s, point %d. Not allowed",with0->get_Name().c_str(), iZeroPoint), 65001);
			else
			{
				if((!with0->HarmArray.empty()) && (!with0->puMagArray.empty()) && (!with0->AngleArray.empty()))
					with0->SetMultArray();
			}
		}
	} /*WITH*/
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TSpectrum::MakeLike(const String LineName)
{
	int result = 0;
	TSpectrumObj* OtherSpectrum = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line code in the present collection*/
	OtherSpectrum = ((TSpectrumObj*) Find(LineName));
	if(OtherSpectrum != nullptr)
		/*# with ActiveSpectrumObj do */
		{
			auto with0 = ActiveSpectrumObj;
			int stop = 0;
			with0->NumHarm = OtherSpectrum->NumHarm;
			with0->HarmArray.resize(with0->NumHarm);
			with0->puMagArray.resize(with0->NumHarm);
			with0->AngleArray.resize(with0->NumHarm);
			for(stop = with0->NumHarm, i = 1; i <= stop; i++)
			{
				(with0->HarmArray)[i - 1]	= (OtherSpectrum->HarmArray)[i - 1];
				(with0->puMagArray)[i - 1]	= (OtherSpectrum->puMagArray)[i - 1];
				(with0->AngleArray)[i - 1]	= (OtherSpectrum->AngleArray)[i - 1];
			}
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherSpectrum->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Spectrum MakeLike: \"") + LineName
	           + "\" Not Found.", 651);
	return result;
}  // Returns active line code string

String TSpectrum::Get_Code()
{
	String result;
	TSpectrumObj* SpectrumObj = nullptr;
	SpectrumObj = ((TSpectrumObj*) ElementList.Get_Active());
	result = SpectrumObj->get_Name();
	return result;
}  // sets the  active Spectrum

void TSpectrum::Set_Code(const String Value)
{
	TSpectrumObj* SpectrumObj = nullptr;
	ActiveSpectrumObj = nullptr;
	SpectrumObj = ((TSpectrumObj*) ElementList.Get_First());
	while(SpectrumObj != nullptr)
	{
		if(CompareText(SpectrumObj->get_Name(), Value) == 0)
		{
			ActiveSpectrumObj = SpectrumObj;
			return;
		}
		SpectrumObj = ((TSpectrumObj*) ElementList.Get_Next());
	}
	DoSimpleMsg(String("Spectrum: \"") + Value + "\" not Found.", 652);
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TSpectrum Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TSpectrumObj::TSpectrumObj(TDSSClass* ParClass, const String SpectrumName)
 : inherited(ParClass),
			NumHarm(0)
{
	Set_Name(LowerCase(SpectrumName));
	DSSObjType = ParClass->DSSClassType;
	NumHarm = 0;
	puMagArray.clear();
	AngleArray.clear(); 
	MultArray.clear();
	HarmArray.clear();
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TSpectrumObj::~TSpectrumObj()
{
	puMagArray.clear();
	AngleArray.clear();
	MultArray.clear();
	HarmArray.clear();
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TSpectrum::DoCSVFile(const String FileName)
{
	String dummy;
	System::TTextRec f = {};
	int i = 0;
	String s;
	try
	{
		System::AssignFile(f, FileName);
		System::Reset(f);
		IOResultToException();
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Opening CSV File: \"") + FileName, 653);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	try
	{
		/*# with ActiveSpectrumObj do */
		{
			auto with0 = ActiveSpectrumObj;
			with0->HarmArray.resize(with0->NumHarm); 
			with0->puMagArray.resize(with0->NumHarm);
			with0->AngleArray.resize(with0->NumHarm);
			i = 0;
			while((!Eof(f)) && (i < with0->NumHarm))
			{
				++i;
				System::ReadLn(f, s);  // Use Auxparser, which allows for formats
				/*# with AuxParser[ActiveActor] do */
				{
					
					AuxParser[ActiveActor]->SetCmdString(s);
					dummy = AuxParser[ActiveActor]->GetNextParam();
					(with0->HarmArray)[i - 1] = AuxParser[ActiveActor]->MakeDouble_();
					dummy = AuxParser[ActiveActor]->GetNextParam();
					(with0->puMagArray)[i - 1] = AuxParser[ActiveActor]->MakeDouble_() * 0.01;
					dummy = AuxParser[ActiveActor]->GetNextParam();
					(with0->AngleArray)[i - 1] = AuxParser[ActiveActor]->MakeDouble_();
				}
			}
			System::CloseFile(f);
			if(i != with0->NumHarm)
				with0->NumHarm = i;   // reset number of points
		}
	}
	catch(exception& e)
	{
		{
			DoSimpleMsg(String("Error Processing CSV File: \"") + FileName
	           + ". "
	           + (string) e.what(), 654);
			System::CloseFile(f); System::InOutRes = 0;
			return;
		}
	}
}

void TSpectrumObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	int j = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			switch(i)
			{
				case 	2:
				{
					int stop1 = 0;
					{ 
						System::Write(f, "~ ");
						System::Write(f, with0->PropertyName[i - 1]); 
						System::Write(f, "=("); 
					}
					for(stop1 = NumHarm, j = 1; j <= stop1; j++)
					{
						System::Write(f, Format("%-g, ",HarmArray[j - 1]));
					}
					System::WriteLn(f, ")");
				}
				break;
				case 	3:
				{
					int stop1 = 0;
					{ 
						System::Write(f, "~ "); 
						System::Write(f, with0->PropertyName[i - 1]); 
						System::Write(f, "=("); 
					}
					for(stop1 = NumHarm, j = 1; j <= stop1; j++)
					{
						System::Write(f, Format("%-g, ",puMagArray[j - 1] * 100.0));
					}
					System::WriteLn(f, ")");
				}
				break;
				case 	4:
				{
					int stop1 = 0;
					{ 
						System::Write(f, "~ "); 
						System::Write(f, with0->PropertyName[i - 1]); 
						System::Write(f, "=("); 
					}
					for(stop1 = NumHarm, j = 1; j <= stop1; j++)
					{
						System::Write(f, Format("%-g, ",AngleArray[j - 1]));
					}
					System::WriteLn(f, ")");
				}
				break;
				default:
				{ 
					System::Write(f, "~ "); 
					System::Write(f, with0->PropertyName[i - 1]); 
					System::Write(f, "="); 
					System::WriteLn(f, Get_PropertyValue(i));
				}
				break;
			}
		}
	}
	if(Complete)
	{
		int stop = 0;
		WriteLn(f, "Multiplier Array:");
		WriteLn(f, "Harmonic, Mult.re, Mult.im, Mag,  Angle");
		for(stop = NumHarm, i = 1; i <= stop; i++)
		{
			{ Write(f, Format("%-g",HarmArray[i - 1])); Write(f, ", "); }
			Write(f, Format("%-g, %-g, ",MultArray[i - 1].re, MultArray[i - 1].im));
			Write(f, Format("%-g, %-g",cabs(MultArray[i - 1]), cdang(MultArray[i - 1])));
			WriteLn(f);
		}
	}
}

complex TSpectrumObj::GetMult(double h)
{
	complex result = {};
	int i = 0;

     /*Search List for  harmonic (nearest 0.01 harmonic) and return multiplier*/
	int stop = 0;
	for(stop = NumHarm, i = 1; i <= stop; i++)
	{
		if(Abs((h - HarmArray[i - 1])) < 0.01)
		{
			result = MultArray[i - 1];
			return result;
		} /*IF*/
	} /*For*/

     /*None Found, return zero*/
	result = CZero;
	return result;
}

String TSpectrumObj::GetPropertyValue(int Index)
{
	String result;
	int i = 0;
	switch(Index)
	{
		case 2: case 3: case 4:
		result = "(";
		break;
		default:
		result = "";
		break;
	}
	switch(Index)
	{
		int stop;

		case 	1:
		result = IntToStr(NumHarm);
		break;
		case 	2:
		for(stop = NumHarm, i = 1; i <= stop; i++)
		{
			result = result + Format("%-g, ",HarmArray[i - 1]);
		}
		break;
		case 	3:
		for(stop = NumHarm, i = 1; i <= stop; i++)
		{
			result = result + Format("%-g, ",puMagArray[i - 1] * 100.0);
		}
		break;
		case 	4:
		for(stop = NumHarm, i = 1; i <= stop; i++)
		{
			result = result + Format("%-g, ",AngleArray[i - 1]);
		}
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	switch(Index)
	{
		case 2: case 3: case 4:
		result = result + ")";
		break;
		default:
		  ;
		break;
	}
	return result;
}

bool TSpectrumObj::HarmArrayHasaZero(int& zeropoint)
{
	bool result = false;
	int i = 0;
	int stop = 0;
	result = false;
	zeropoint = 0;
	for(stop = NumHarm, i = 1; i <= stop; i++)
	{
		if(HarmArray[i - 1] == 0.0)
		{
			result = true;
			zeropoint = i;
			break;
		}
	}
	return result;
}

void TSpectrumObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"0");
	Set_PropertyValue(2,"");
	Set_PropertyValue(3,"");
	Set_PropertyValue(4,"");
	Set_PropertyValue(5,"");
	inherited::InitPropertyValues(NumPropsThisClass);
}

/*Rotate all phase angles so that the fundamental is at zero*/

void TSpectrumObj::SetMultArray()
{
	int i = 0;
	double FundAngle = 0.0;
	try
	{
		int stop = 0;
		FundAngle = 0.0;
		for(stop = NumHarm, i = 1; i <= stop; i++)
		{
			if(Round(HarmArray[i - 1]) == 1)
			{
				FundAngle = AngleArray[i - 1];
				break;
			}
		}
		MultArray.resize(NumHarm);
		for(stop = NumHarm, i = 1; i <= stop; i++)
		{
			MultArray[i - 1] = pdegtocomplex(puMagArray[i - 1], (AngleArray[i - 1] - HarmArray[i - 1] * FundAngle));
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Exception while computing Spectrum.") + get_Name()
	           + ". Check Definition. Aborting", 655);
		if(In_Redirect)
			Redirect_Abort = true;
	}
}




}  // namespace Spectrum





