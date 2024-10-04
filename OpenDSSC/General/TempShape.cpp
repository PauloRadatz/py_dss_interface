
#pragma hdrstop

#include "TempShape.h"

#include "DSSGlobals.h"
#include "Utilities.h"
#include "TOPExport.h"

using namespace std;
using namespace Arraydef;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace ParserDel;
using namespace PointerList;
using namespace System;
using namespace mathutil;
using namespace Utilities;

namespace TempShape
{

TTShapeObj::TTShapeObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TTShapeObj::TTShapeObj(string ClassName) : inherited(ClassName) {}
TTShapeObj::TTShapeObj() {}


TTShapeObj* ActiveTShapeObj = nullptr;
const int NumPropsThisClass = 12;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TTShape::TTShape()
{
	;
	Class_Name = "TShape";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TTShape::~TTShape()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TTShape::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	(PropertyName)[1 - 1] = "npts";     // Number of points to expect
	(PropertyName)[2 - 1] = "interval"; // default = 1.0;
	(PropertyName)[3 - 1] = "temp";     // vector of temperature values
	(PropertyName)[4 - 1] = "hour";     // vector of hour values
	(PropertyName)[5 - 1] = "mean";     // set the mean temp (otherwise computed)
	(PropertyName)[6 - 1] = "stddev";   // set the std dev of the temp (otherwise computed)
	(PropertyName)[7 - 1] = "csvfile";  // Switch input to a csvfile
	(PropertyName)[8 - 1] = "sngfile";  // switch input to a binary file of singles
	(PropertyName)[9 - 1] = "dblfile";    // switch input to a binary file of singles
	(PropertyName)[10 - 1] = "sinterval"; // Interval in seconds
	(PropertyName)[11 - 1] = "minterval"; // Interval in minutes
	(PropertyName)[12 - 1] = "action";    // Interval in minutes

     // define Property help values
	(PropertyHelp)[1 - 1] = "Max number of points to expect in temperature shape vectors. This gets reset to the number of Temperature values "
	           "found if less than specified.";     // Number of points to expect
	(PropertyHelp)[2 - 1] = String("Time interval for fixed interval data, hrs. Default = 1. " "If Interval = 0 then time data (in hours) may be at irregular intervals and time value must be specified using either the Hour property or input files. " "Then values are interpolated when Interval=0, but not for fixed interval data.  ") + CRLF
	           + CRLF
	           + "See also \"sinterval\" and \"minterval\"."; // default = 1.0;
	(PropertyHelp)[3 - 1] = String("Array of temperature values.  Units should be compatible with the object using the data. " "You can also use the syntax: ") + CRLF
	           + "Temp = (file=filename)     !for text file one value per line"
	           + CRLF
	           + "Temp = (dblfile=filename)  !for packed file of doubles"
	           + CRLF
	           + "Temp = (sngfile=filename)  !for packed file of singles "
	           + CRLF
	           + CRLF
	           + "Note: this property will reset Npts if the  number of values in the files are fewer.";     // vextor of hour values
	(PropertyHelp)[4 - 1] = String("Array of hour values. Only necessary to define this property for variable interval data." " If the data are fixed interval, do not use this property. " "You can also use the syntax: ") + CRLF
	           + "hour = (file=filename)     !for text file one value per line"
	           + CRLF
	           + "hour = (dblfile=filename)  !for packed file of doubles"
	           + CRLF
	           + "hour = (sngfile=filename)  !for packed file of singles ";     // vextor of hour values
	(PropertyHelp)[5 - 1] = "Mean of the temperature curve values.  This is computed on demand the first time a "
	           "value is needed.  However, you may set it to another value independently. "
	           "Used for Monte Carlo load simulations.";     // set the mean (otherwise computed)
	(PropertyHelp)[6 - 1] = String("Standard deviation of the temperatures.  This is computed on demand the first time a " "value is needed.  However, you may set it to another value independently." "Is overwritten if you subsequently read in a curve") + CRLF
	           + CRLF
	           + "Used for Monte Carlo load simulations.";   // set the std dev (otherwise computed)
	(PropertyHelp)[7 - 1] = "Switch input of  temperature curve data to a csv file "
	           "containing (hour, Temp) points, or simply (Temp) values for fixed time interval data, one per line. "
	           "NOTE: This action may reset the number of points to a lower value.";   // Switch input to a csvfile
	(PropertyHelp)[8 - 1] = "Switch input of  temperature curve data to a binary file of singles "
	           "containing (hour, Temp) points, or simply (Temp) values for fixed time interval data, packed one after another. "
	           "NOTE: This action may reset the number of points to a lower value.";  // switch input to a binary file of singles
	(PropertyHelp)[9 - 1] = "Switch input of  temperature curve data to a binary file of doubles "
	           "containing (hour, Temp) points, or simply (Temp) values for fixed time interval data, packed one after another. "
	           "NOTE: This action may reset the number of points to a lower value.";   // switch input to a binary file of singles
	(PropertyHelp)[10 - 1] = "Specify fixed interval in SECONDS. Alternate way to specify Interval property.";
	(PropertyHelp)[11 - 1] = "Specify fixed interval in MINUTES. Alternate way to specify Interval property.";
	(PropertyHelp)[12 - 1] = "{DblSave | SngSave} After defining temperature curve data... "
	           "Setting action=DblSave or SngSave will cause the present \"Temp\" values to be written to "
	           "either a packed file of double or single. The filename is the Tshape name. "; // Action
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TTShape::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TTShapeObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TTShape::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveTShapeObj = ((TTShapeObj*) ElementList.Get_Active());
	ActiveDSSObject[ActorID] = ActiveTShapeObj;
	/*# with ActiveTShapeObj do */
	{
		auto with0 = ActiveTShapeObj;
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
	           + "\"", 57610);
				break;
				case 	1:
				with0->Set_NumPoints(Parser[ActorID]->MakeInteger_());
				break;
				case 	2:
				with0->Interval = Parser[ActorID]->MakeDouble_();
				break;
				case 	3:
				{
					with0->TValues	= (pDoubleArray) realloc(with0->TValues, sizeof(double) * with0->get_FNumPoints());
                 // Allow possible Resetting (to a lower value) of num points when specifying temperatures not Hours
					with0->Set_NumPoints(InterpretDblArray(Param, with0->get_FNumPoints(), with0->TValues));   // Parser.ParseAsVector(Npts, Temperatures);
				}
				break;
				case 	4:
				{
					with0->Hours	= (pDoubleArray) realloc(with0->Hours, sizeof(double) * with0->get_FNumPoints());
					with0->Set_NumPoints( InterpretDblArray(Param, with0->get_FNumPoints(), with0->Hours));   // Parser.ParseAsVector(Npts, Hours);
				}
				break;
				case 	5:
				with0->Set_Mean(Parser[ActorID]->MakeDouble_());
				break;
				case 	6:
				with0->Set_StdDev(Parser[ActorID]->MakeDouble_());
				break;
				case 	7:
				DoCSVFile(Param);
				break;
				case 	8:
				DoSngFile(Param);
				break;
				case 	9:
				DoDblFile(Param);
				break;
				case 	10:
				with0->Interval = double(Parser[ActorID]->MakeDouble_()) / 3600.0;
				break;  // Convert seconds to hr
				case 	11:
				with0->Interval = double(Parser[ActorID]->MakeDouble_()) / 60.0;
				break;  // Convert minutes to hr
				case 	12:
				switch(LowerCase(Param)[0])
				{
					case 	L'd':
					with0->SaveToDblFile();
					break;
					case 	L's':
					with0->SaveToSngFile();
					break;
					default:
					  ;
					break;
				}
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveTShapeObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	3: case 7: case 8: case 9:
				{
					with0->FStdDevCalculated = false;   // now calculated on demand
					with0->ArrayPropertyIndex = ParamPointer;
					with0->Set_NumPoints(with0->FNumPoints);  // Keep Properties in order for save command
				}
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		} /*While*/
	} /*WITH*/
	return result;
}

void* TTShape::Find(const String ObjName)
{
	void* result = nullptr;
	if((ObjName.length() == 0) || (CompareText(ObjName, "none") == 0))
		result = nullptr;
	else
		result = inherited::Find(ObjName);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TTShape::MakeLike(const String ShapeName)
{
	int result = 0;
	TTShapeObj* OtherTShape = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line code in the present collection*/
	OtherTShape = ((TTShapeObj*) Find(ShapeName));
	if(OtherTShape != nullptr)
		/*# with ActiveTShapeObj do */
		{
			auto with0 = ActiveTShapeObj;
			int stop = 0;
			with0->Set_NumPoints(OtherTShape->get_FNumPoints());
			with0->Interval = OtherTShape->Interval;
			with0->TValues = (pDoubleArray) realloc(with0->TValues, sizeof(double) * with0->get_FNumPoints());
			for(stop = with0->get_FNumPoints(), i = 1; i <= stop; i++)
			{
				(with0->TValues)[i] = (OtherTShape->TValues)[i];
			}
			if(with0->Interval > 0.0)
				free(with0->Hours);
			else
			{
				int stop = 0;
				with0->Hours = (pDoubleArray) realloc(with0->Hours, sizeof(double) * with0->get_FNumPoints());
				for(stop = with0->get_FNumPoints(), i = 1; i <= stop; i++)
				{
					(with0->Hours)[i] = (OtherTShape->Hours)[i];
				}
			}
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherTShape->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in TShape MakeLike: \"") + ShapeName
	           + "\" Not Found.", 57611);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TTShape::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TTShape.Init", -1);
	result = 0;
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Returns active line code string

String TTShape::Get_Code()
{
	String result;
	TTShapeObj* TShapeObj = nullptr;
	TShapeObj = ((TTShapeObj*) ElementList.Get_Active());
	result = TShapeObj->get_Name();
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // sets the  active TShape

void TTShape::Set_Code(const string Value)
{
	TTShapeObj* TShapeObj = nullptr;
	ActiveTShapeObj = nullptr;
	TShapeObj = ((TTShapeObj*) ElementList.Get_First());
	while(TShapeObj != nullptr)
	{
		if(CompareText(TShapeObj->get_Name(), Value) == 0)
		{
			ActiveTShapeObj = TShapeObj;
			return;
		}
		TShapeObj = ((TTShapeObj*) ElementList.Get_Next());
	}
	DoSimpleMsg(String("TShape: \"") + Value + "\" not Found.", 57612);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TTShape::DoCSVFile(const string FileName)
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
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 57613);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	try
	{
		/*# with ActiveTShapeObj do */
		{
			auto with0 = ActiveTShapeObj;
			with0->TValues = (pDoubleArray) realloc(with0->TValues, sizeof(double) * with0->get_FNumPoints());
			if(with0->Interval == 0.0)
				with0->Hours = (pDoubleArray)realloc(with0->Hours, sizeof(double) * with0->get_FNumPoints());
			i = 0;
			while((!Eof(f)) && (i < with0->FNumPoints))
			{
				++i;
				System::ReadLn(f, s); // read entire line  and parse with AuxParser
            /*AuxParser allows commas or white space*/
				/*# with AuxParser[ActiveActor] do */
				auto with1 = AuxParser[ActiveActor];
				{
					
					with1->SetCmdString(s);
					if(with0->Interval == 0.0)
					{
						dummy = with1->GetNextParam();
						(with0->Hours)[i] = with1->MakeDouble_();
					}
					dummy = with1->GetNextParam();
					(with0->TValues)[i] = with1->MakeDouble_();
				}
			}
			CloseFile(f);
			if(i != with0->FNumPoints)
				with0->Set_NumPoints(i);
		}
	}
	catch(exception& e)
	{
		{
			DoSimpleMsg(String("Error Processing CSV File: \"") + FileName
	           + ". "
	           + (string) e.what(), 57614);
			CloseFile(f); System::InOutRes = 0;
			return;
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TTShape::DoSngFile(const string FileName)
{
	System::TTypedFile<float> f;
	float hr = 0.0F;
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
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 57615);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	try
	{
		/*# with ActiveTShapeObj do */
		{
			auto with0 = ActiveTShapeObj;
			with0->TValues = (pDoubleArray)realloc(with0->TValues, sizeof(double) * with0->get_FNumPoints());
			if(with0->Interval == 0.0)
				with0->Hours = (pDoubleArray)realloc(with0->Hours, sizeof(double) * with0->get_FNumPoints());
			i = 0;
			while((!Eof(f)) && (i < with0->FNumPoints))
			{
				++i;
				if(with0->Interval == 0.0)
				{
					System::Read(f, &hr);
					(with0->Hours)[i] = hr;
				}
				System::Read(f, &m);
				(with0->TValues)[i] = m;
			}
			System::CloseFile(f);
			if(i != with0->FNumPoints)
				with0->Set_NumPoints(i);
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Processing TShape File: \"") + FileName, 57616);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TTShape::DoDblFile(const string FileName)
{
	System::TTypedFile<double> f;
	int i = 0;
	try
	{
		System::AssignFile(f, FileName);
		System::Reset(f);
		IOResultToException();
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Opening File: \"") + FileName, 57617);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
	try
	{
		/*# with ActiveTShapeObj do */
		{
			auto with0 = ActiveTShapeObj;
			with0->TValues = (pDoubleArray) realloc(with0->TValues, sizeof(double) * with0->get_FNumPoints());
			if(with0->Interval == 0.0)
				with0->Hours = (pDoubleArray) realloc(with0->Hours, sizeof(double) * with0->get_FNumPoints());
			i = 0;
			while((!Eof(f)) && (i < with0->FNumPoints))
			{
				++i;
				if(with0->Interval == 0.0)
					System::Read(f, &(with0->Hours)[i]);
				System::Read(f, &(with0->TValues)[i]);
			}
			System::CloseFile(f);
			if(i != with0->FNumPoints)
				with0->Set_NumPoints(i);
		}
	}
	catch(...)
	{
		DoSimpleMsg(String("Error Processing Tshape File: \"") + FileName, 57618);
		System::CloseFile(f); System::InOutRes = 0;
		return;
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TTShapeObj::TTShapeObj(TDSSClass* ParClass, const string TShapeName)
 : inherited(ParClass),
			LastValueAccessed(1),
			FNumPoints(0),
			ArrayPropertyIndex(0),
			FStdDevCalculated(false),
			FMean(0.0),
			FStdDev(0.0),
			Interval(0.0),
			Hours(nullptr),
			TValues(nullptr)
{
	Set_Name(LowerCase(TShapeName));
	DSSObjType = ParClass->DSSClassType;
	Interval = 1.0;  // hr
	Hours = nullptr;
	TValues = nullptr;
	FStdDevCalculated = false;  // calculate on demand
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TTShapeObj::~TTShapeObj()
{
	free(Hours);
	if(TValues != NULL)
		free(TValues);
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This FUNCTION returns the Temperature for the given hour.
// If no points exist in the curve, the result is  0.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.

double TTShapeObj::GetTemperature(double hr)
{
	double result = 0.0;
	int Index = 0;
	int i = 0;
	result = 0.0;    // default return value if no points in curve
	if(FNumPoints > 0)
	{
		if(FNumPoints == 1)         // Handle Exceptional cases
		{
			result = TValues[1 - 1];
		}
		else
		{
			if(Interval > 0.0)
			{
				Index = (int) Round(hr / Interval);
				if(Index > FNumPoints)
					Index = Index % FNumPoints;  // Wrap around using remainder
				if(Index == 0)
					Index = FNumPoints;
				result = TValues[Index - 1];
			}
			else

          // For random interval
			
        /* Start with previous value accessed under the assumption that most
          of the time, this FUNCTION will be called sequentially*/

          /*Normalize Hr to max hour in curve to get wraparound*/
			{
				int stop = 0;
				if(hr > Hours[FNumPoints])
				{
					hr = hr - Trunc(hr / Hours[FNumPoints]) * Hours[FNumPoints];
				}
				if(Hours[LastValueAccessed] > hr)
					LastValueAccessed = 1;  // Start over from Beginning
				for(stop = FNumPoints, i = LastValueAccessed + 1; i <= stop; i++)
				{
					if(Abs((int) (Hours[i] - hr)) < 0.00001)  // If close to an actual point, just use it.
					{
						result = TValues[i - 1];
						LastValueAccessed = i;
						return result;
					}
					else
					{
						if(Hours[i] > hr)      // Interpolate for temperature
						{
							LastValueAccessed = i - 1;
							result = TValues[LastValueAccessed] + (hr - Hours[LastValueAccessed]) / (Hours[i - 1] - Hours[LastValueAccessed]) * (TValues[i - 1] - TValues[LastValueAccessed]);
							return result;
						}
					}
				}

           // If we fall through the loop, just use last value
				LastValueAccessed = FNumPoints - 1;
				result = TValues[FNumPoints];
			}
		}
	}
	return result;
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TTShapeObj::CalcMeanandStdDev()
{
	if(FNumPoints > 0)
	{
		if(Interval > 0.0)
			RCDMeanAndStdDev(TValues, FNumPoints, FMean, FStdDev);
		else
			CurveMeanAndStdDev(TValues, Hours, FNumPoints, FMean, FStdDev);
	}
	Set_PropertyValue(5,Format("%.8g", FMean));
	Set_PropertyValue(6,Format("%.8g", FStdDev));
	FStdDevCalculated = true;
}

double TTShapeObj::Get_Interval()
{
	double result = 0.0;
	if(Interval > 0.0)
		result = Interval;
	else
	{
		if(LastValueAccessed > 1)
			result = Hours[LastValueAccessed] - Hours[LastValueAccessed - 1];
		else
			result = 0.0;
	}
	return result;
}

double TTShapeObj::Get_Mean()
{
	double result = 0.0;
	if(!FStdDevCalculated)
		CalcMeanandStdDev();
	result = FMean;
	return result;
}

double TTShapeObj::Get_StdDev()
{
	double result = 0.0;
	if(!FStdDevCalculated)
		CalcMeanandStdDev();
	result = FStdDev;
	return result;
}

double TTShapeObj::Temperature(int i)
{
	double result = 0.0;
	if((i <= FNumPoints) && (i > 0))
	{
		result = TValues[i];
		LastValueAccessed = i;
	}
	else
	result = 0.0;
	return result;
}

double TTShapeObj::Hour(int i)
{
	double result = 0.0;
	if(Interval == 0)
	{
		if((i <= FNumPoints) && (i > 0))
		{
			result = Hours[i];
			LastValueAccessed = i;
		}
		else
		result = 0.0;
	}
	else
	{
		result = Hours[i] * Interval;
		LastValueAccessed = i;
	}
	return result;
}

void TTShapeObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[i - 1]); System::Write(f, "="); System::WriteLn(f, Get_PropertyValue(i)); }
		}
	}
}

String TTShapeObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	2:
		result = Format("%.8g", Interval);
		break;
		case 	3:
		result = GetDSSArray_Real(FNumPoints, TValues);
		break;
		case 	4:
		if(Hours != nullptr)
			result = GetDSSArray_Real(FNumPoints, Hours);
		break;
		case 	5:
		result = Format("%.8g", Get_Mean());
		break;
		case 	6:
		result = Format("%.8g", Get_StdDev());
		break;
		case 	10:
		result = Format("%.8g", Interval * 3600.0);
		break;
		case 	11:
		result = Format("%.8g", Interval * 60.0);
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TTShapeObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"0");     // Number of points to expect
	Set_PropertyValue(2,"1"); // default = 1.0 hr;
	Set_PropertyValue(3,"");     // vector of multiplier values
	Set_PropertyValue(4,"");     // vextor of hour values
	Set_PropertyValue(5,"0");     // set the mean (otherwise computed)
	Set_PropertyValue(6,"0");   // set the std dev (otherwise computed)
	Set_PropertyValue(7,"");   // Switch input to a csvfile
	Set_PropertyValue(8,"");  // switch input to a binary file of singles
	Set_PropertyValue(9,"");   // switch input to a binary file of singles
	Set_PropertyValue(10,"3600");   // seconds
	Set_PropertyValue(11,"60");     // minutes
	Set_PropertyValue(12,""); // action option .
	inherited::InitPropertyValues(NumPropsThisClass);
}

void TTShape::TOPExport(String ObjName)
{
	TStringList* NameList = nullptr;
	TStringList* cNames = nullptr;
	pDoubleArray Vbuf = nullptr;
	pDoubleArray CBuf = nullptr;
	TTShapeObj* Obj = nullptr;
	int MaxPts = 0;
	int i = 0;
	int j = 0;
	double MaxTime = 0.0;
	double MinInterval = 0.0;
	double Hr_Time = 0.0;
	PointerList::TPointerList* ObjList = nullptr;
	/*
	// removed for now, no under use currently
	TOPTransferFile->FileName = GetOutputDirectory() + "TOP_TShape.STO";
	try
	{
		TOPTransferFile->Open();
	}
	catch(Exception* e)
	{
		{
			DoSimpleMsg(String("TOP Transfer File Error: ") + e->Message, 57619);
			try
			{
				TOPTransferFile->Close();
              //OK if Error
			}
			catch(...)
			{
				;
			}
			return;
		}
	}

     //Send only fixed interval data
	ObjList = new PointerList::TPointerList(10);
	NameList = new TStringList();
	cNames = new TStringList();

     //Make a List of fixed interval data where the interval is greater than 1 minute
	if(CompareText(ObjName, "ALL") == 0)
	{
		Obj = ((TTShapeObj*) ElementList->Get_First());
		while(Obj != nullptr)
		{
			if(Obj->Interval > (1.0 / 60.0))
				ObjList->Add(Obj);
			Obj = ((TTShapeObj*) ElementList->Get_Next());
		}
	}
	else
	{
		Obj = ((TTShapeObj*) Find(ObjName));
		if(Obj != nullptr)
		{
			if(Obj->Interval > (1.0 / 60.0))
				ObjList->Add(Obj);
			else
				DoSimpleMsg(String("Tshape.") + ObjName + " is not hourly fixed interval.", 57620);
		}
		else
		{
			DoSimpleMsg(String("Tshape.") + ObjName + " not found.", 57621);
		}
	}

     //If none found, exit
	if(ObjList->get_myNumList() > 0)

       //Find Max number of points
	{
		int stop = 0;
		MaxTime = 0.0;
		MinInterval = 1.0;
		Obj = ((TTShapeObj*) ObjList->Get_First());
		while(Obj != nullptr)
		{
			MaxTime = Max(MaxTime, Obj->Get_FNumPoints() * Obj->Interval);
			MinInterval = Min(MinInterval, Obj->Interval);
			NameList->Add(Obj->get_Name());
			Obj = ((TTShapeObj*) ObjList->Get_Next());
		}
      // SetLength(Xarray, maxPts);
		MaxPts = (int) Round(MaxTime / MinInterval);
		TOPTransferFile->WriteHeader(0.0, MaxTime, MinInterval, ObjList->get_myNumList(), 0, 16, "DSS (TM), Electrotek Concepts (R)");
		TOPTransferFile->WriteNames(NameList, cNames);
		Hr_Time = 0.0;
		Vbuf = ((pDoubleArray) AllocMem(sizeof((*Vbuf)[1]) * ObjList->get_myNumList()));
		CBuf = ((pDoubleArray) AllocMem(sizeof((*Vbuf)[1]) * 1));   // just a dummy -- Cbuf is ignored here
		for(stop = MaxPts, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = ObjList->get_myNumList(), j = 1; j <= stop1; j++)
			{
				Obj = ((TTShapeObj*) ObjList->Get(j));
				(*Vbuf)[j] = Obj->GetTemperature(Hr_Time);
			}
			TOPTransferFile->WriteData(Hr_Time, Vbuf, CBuf);
			Hr_Time = Hr_Time + MinInterval;
		}
		TOPTransferFile->Close();
		TOPTransferFile->SendToTop();
		Vbuf = (pDoubleArray) realloc(Vbuf, 0);
		CBuf = (pDoubleArray) realloc(CBuf, 0);
	}
	delete ObjList;
	delete NameList;
	delete cNames;
	*/
}

//-----------------------------------------------------------

int TTShapeObj::get_FNumPoints()
{
	return FNumPoints;
}

//-----------------------------------------------------------

void TTShapeObj::SaveToDblFile()
{
	System::TTypedFile<double> f;
	int i = 0;
	String FName;
	if(TValues != NULL)
	{
		try
		{
			int stop = 0;
			FName = get_Name() + ".dbl";
			System::AssignFile(f, FName);
			System::Rewrite(f);
			IOResultToException();
			for(stop = get_FNumPoints(), i = 1; i <= stop; i++)
			{
				System::Write(f, &(TValues)[i]);
			}
			GlobalResult = String("Temp=[dblfile=") + FName + "]";
//		}
//		__finally
//		{
			System::CloseFile(f);
		}
		catch (exception& E)
		{
			// added to match with the try declaration in Delhpi
		}
	}
	else
	DoSimpleMsg(String("Tshape.") + get_Name() + " Temperatures not defined.", 57622);
}

void TTShapeObj::SaveToSngFile()
{
	System::TTypedFile<float> f;
	int i = 0;
	String FName;
	float Temp = 0.0F;
	if(TValues != NULL)
	{
		try
		{
			int stop = 0;
			FName = get_Name() + ".sng";
			System::AssignFile(f, FName);
			System::Rewrite(f);
			IOResultToException();
			for(stop = get_FNumPoints(), i = 1; i <= stop; i++)
			{
				Temp = (float) TValues[i];
				System::Write(f, &Temp);
			}
			GlobalResult = String("Temp=[sngfile=") + FName + "]";
//		}
//		__finally
//		{
			System::CloseFile(f);
		}
		catch (exception& E)
		{
			// Added to match the Delphi declaration
		}
	}
	else
	DoSimpleMsg(String("Tshape.") + get_Name() + " Temperatures not defined.", 57623);
}

void TTShapeObj::Set_Mean(double Value)
{
	FStdDevCalculated = true;
	FMean = Value;
}

void TTShapeObj::Set_NumPoints(int Value)
{
	Set_PropertyValue(1,IntToStr(Value));   // Update property list variable

      // Reset array property values to keep them in propoer order in Save
	if(ArrayPropertyIndex > 0)
		Set_PropertyValue(ArrayPropertyIndex,Get_PropertyValue(ArrayPropertyIndex));
	FNumPoints = Value;   // Now assign the value
}

void TTShapeObj::Set_StdDev(double Value)
{
	FStdDevCalculated = true;
	FStdDev = Value;
}




}  // namespace TempShape





