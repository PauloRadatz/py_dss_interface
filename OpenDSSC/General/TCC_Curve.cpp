
#pragma hdrstop

#include "TCC_Curve.h"
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

namespace TCC_Curve
{

TTCC_CurveObj::TTCC_CurveObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TTCC_CurveObj::TTCC_CurveObj(String ClassName) : inherited(ClassName) {}
TTCC_CurveObj::TTCC_CurveObj() {}


TTCC_CurveObj* ActiveTCC_CurveObj = nullptr;
const int NumPropsThisClass = 3;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TTCC_Curve::TTCC_Curve()
{
	;
	Class_Name = "TCC_Curve";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TTCC_Curve::~TTCC_Curve()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TTCC_Curve::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	(PropertyName)[0] = "npts";     // Number of points to expect
	(PropertyName)[1] = "C_array";     // vector of multiplier values
	(PropertyName)[2] = "T_array";     // vextor of time values , Sec

     // define Property help values
	(PropertyHelp)[0] = "Number of points to expect in time-current arrays.";     // Number of points to expect
	(PropertyHelp)[1] = "Array of current (or voltage) values corresponding to time values (see help on T_Array).";     // vector of multiplier values
	(PropertyHelp)[2] = String("Array of time values in sec. Typical array syntax: ") + CRLF
	           + "t_array = (1, 2, 3, 4, ...)"
	           + CRLF
	           + CRLF
	           + "Can also substitute a file designation: "
	           + CRLF
	           + "t_array =  (file=filename)"
	           + CRLF
	           + CRLF
	           + "The specified file has one value per line.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TTCC_Curve::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TTCC_CurveObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void CalcLogPoints(const pDoubleArray X, pDoubleArray LogX, int n)
{
	int i = 0;
	int stop = 0;
	for(stop = n, i = 1; i <= stop; i++)
	{
		if((X)[i - 1] > 0.0)
			(LogX)[i - 1] = log((X)[i - 1]);
		else
			(LogX)[i - 1] = log(0.001L);
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TTCC_Curve::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveTCC_CurveObj = ((TTCC_CurveObj*) ElementList.Get_Active());
	ActiveDSSObject[ActorID] = ActiveTCC_CurveObj;
	/*# with ActiveTCC_CurveObj do */
	{
		auto with0 = ActiveTCC_CurveObj;
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
	           + "\"", 420);
				break;
				case 	1:
				with0->npts = Parser[ActorID]->MakeInteger_();
				break;
				case 	2:
				with0->npts = InterpretDblArray(Param, with0->npts, with0->c_values);
				break;   // Parser.ParseAsVector(Npts, Multipliers);
				case 	3:
				with0->npts = InterpretDblArray(Param, with0->npts, with0->t_values);
				break;   // Parser.ParseAsVector(Npts, Hours);

           // Inherited parameters
				default:
				ClassEdit(ActiveTCC_CurveObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 	1:    // Reallocate arrays to corresponde to Npts
				{
					with0->c_values = (pDoubleArray) realloc(with0->c_values, sizeof(double) * with0->npts);
					with0->LogC		= (pDoubleArray) realloc(with0->LogC, sizeof(double) * with0->npts);
					with0->t_values = (pDoubleArray) realloc(with0->t_values, sizeof(double) * with0->npts);
					with0->Logt		= (pDoubleArray) realloc(with0->Logt, sizeof(double) * with0->npts);
				}
				break;
				case 	2:
				CalcLogPoints(with0->c_values, with0->LogC, with0->npts);
				break;
				case 	3:
				CalcLogPoints(with0->t_values, with0->Logt, with0->npts);
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		} /*WHILE*/
	} /*WITH*/
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TTCC_Curve::MakeLike(const String ShapeName)
{
	int result = 0;
	TTCC_CurveObj* OtherTCC_Curve = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line code in the present collection*/
	OtherTCC_Curve = ((TTCC_CurveObj*) Find(ShapeName));
	if(OtherTCC_Curve != nullptr)
		/*# with ActiveTCC_CurveObj do */
		{
			auto with0 = ActiveTCC_CurveObj;
			int stop = 0;
			with0->npts		= OtherTCC_Curve->npts;
			with0->c_values = new double[with0->npts];
			with0->LogC		= new double[with0->npts];
			with0->t_values = new double[with0->npts];
			with0->Logt		= new double[with0->npts];
			for(stop = with0->npts, i = 1; i <= stop; i++)
			{
				(with0->c_values)[i - 1] = (OtherTCC_Curve->c_values)[i - 1];
			}
			for(stop = with0->npts, i = 1; i <= stop; i++)
			{
				(with0->t_values)[i - 1] = (OtherTCC_Curve->t_values)[i - 1];
			}
			for(stop = with0->npts, i = 1; i <= stop; i++)
			{
				(with0->LogC)[i - 1] = (OtherTCC_Curve->LogC)[i - 1];
			}
			for(stop = with0->npts, i = 1; i <= stop; i++)
			{
				(with0->Logt)[i - 1] = (OtherTCC_Curve->Logt)[i - 1];
			}
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherTCC_Curve->Get_PropertyValue(i));
			}
		}
	else
		DoSimpleMsg(String("Error in TCC_Curve MakeLike: \"") + ShapeName
	           + "\" Not Found.", 421);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TTCC_Curve::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TTCC_Curve.Init", -1);
	result = 0;
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Returns active line code string

String TTCC_Curve::Get_Code()
{
	String result;
	TTCC_CurveObj* TCC_CurveObj = nullptr;
	TCC_CurveObj = ((TTCC_CurveObj*) ElementList.Get_Active());
	result = TCC_CurveObj->get_Name();
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // sets the  active TCC_Curve

void TTCC_Curve::Set_Code(const String Value)
{
	TTCC_CurveObj* TCC_CurveObj = nullptr;
	ActiveTCC_CurveObj = nullptr;
	TCC_CurveObj = ((TTCC_CurveObj*) ElementList.Get_First());
	while(TCC_CurveObj != nullptr)
	{
		if(CompareText(TCC_CurveObj->get_Name(), Value) == 0)
		{
			ActiveTCC_CurveObj = TCC_CurveObj;
			return;
		}
		TCC_CurveObj = ((TTCC_CurveObj*) ElementList.Get_Next());
	}
	DoSimpleMsg(String("TCC_Curve: \"") + Value + "\" not Found.", 422);
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TTCC_Curve Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TTCC_CurveObj::TTCC_CurveObj(TDSSClass* ParClass, const String TCC_CurveName)
 : inherited(ParClass),
			LastValueAccessed(1),
			npts(0),
			Logt(nullptr),
			LogC(nullptr),
			t_values(nullptr),
			c_values(nullptr)
{
	Set_Name(LowerCase(TCC_CurveName));
	DSSObjType = ParClass->DSSClassType;
	c_values = nullptr;
	t_values = nullptr;
	LogC = nullptr;
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TTCC_CurveObj::~TTCC_CurveObj()
{
	free(t_values);
	free(c_values);
	free(LogC);
	free(Logt);
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


// This function returns the operation time for the value given.
// If the value is less than the first entry, return = -1 for No operation.
// Log-Log  interpolation is used.

double TTCC_CurveObj::GetTCCTime(double C_Value)
{
	double result = 0.0;
	int i = 0;
	double Logtest = 0.0;
	result = -1.0;    // default return value

  /*If current is less than first point, just leave*/
	if(C_Value < c_values[1 - 1])
		return result;
	if(npts > 0)
	{
		if(npts == 1)         // Handle Exceptional cases
			result = t_values[1 - 1];
		else


      /* Start with previous value accessed under the assumption that most
        of the time, this function will be called sequentially*/
		{
			int stop = 0;
			if(c_values[LastValueAccessed - 1] > C_Value)
				LastValueAccessed = 1;  // Start over from beginning
			for(stop = npts, i = LastValueAccessed + 1; i <= stop; i++)
			{
				if(c_values[i - 1] == C_Value)
				{
					result = t_values[i - 1];        // direct hit!
					LastValueAccessed = i;
					return result;
				}
				else
				{
					if(c_values[i - 1] > C_Value)   // Log-Log interpolation
					{
						LastValueAccessed = i - 1;
						if(C_Value > 0.0)
							Logtest = log(C_Value);
						else
							Logtest = log(0.001L);
						result = exp(Logt[LastValueAccessed - 1] + (Logtest - LogC[LastValueAccessed - 1]) / (LogC[i - 1] - LogC[LastValueAccessed - 1]) * (Logt[i - 1] - Logt[LastValueAccessed - 1]));
						return result;
					}
				}
			}

       // If we fall through the loop, just use last value
			LastValueAccessed = npts - 1;
			result = t_values[npts - 1];
		}
	}
	return result;
}
// Over-voltage, definite time relay

double TTCC_CurveObj::GetOVTime(double V_Value)
{
	double result = 0.0;
	int i = 0;
	result = -1.0;  // No op return
	if(V_Value > c_values[1 - 1])
	{
		if(npts == 1)
			result = t_values[1 - 1];
		else
		{
			i = 1;
			while(c_values[i - 1] < V_Value)
			{
				++i;
				if(i > npts)
					break;
			}
			result = t_values[i - 1 - 1];
		}
	}
	return result;
}

// Under-voltage, definite time relay

double TTCC_CurveObj::GetUVTime(double V_Value)
{
	double result = 0.0;
	int i = 0;
	result = -1.0;  // No op return
	if(V_Value < c_values[npts - 1])
	{
		if(npts == 1)
			result = t_values[1 - 1];
		else
		{
			i = npts;
			while(c_values[i - 1] > V_Value)
			{
				--i;
				if(i == 0)
					break;
			}
			result = t_values[i + 1 - 1];
		}
	}
	return result;
}

double TTCC_CurveObj::Value(int i)
{
	double result = 0.0;
	if((i <= npts) && (i > 0))
	{
		result = c_values[i - 1];
		LastValueAccessed = i;
	}
	else
	result = 0.0;
	return result;
}

double TTCC_CurveObj::Time(int i)
{
	double result = 0.0;
	if((i <= npts) && (i > 0))
	{
		result = t_values[i - 1];
		LastValueAccessed = i;
	}
	else
	result = 0.0;
	return result;
}

void TTCC_CurveObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[i - 1]); System::Write(f, L'='); System::WriteLn(f, Get_PropertyValue(i)); }
		}
	}
}

String TTCC_CurveObj::GetPropertyValue(int Index)
{
	String result;
	result = "";
	switch(Index)
	{
		case 	2:
		result = GetDSSArray_Real(npts, c_values);
		break;
		case 	3:
		result = GetDSSArray_Real(npts, t_values);
		break;
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

void TTCC_CurveObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"0");     // Number of points to expect
	Set_PropertyValue(2,"");     // vector of multiplier values
	Set_PropertyValue(3,"");     // vextor of sec values
	inherited::InitPropertyValues(NumPropsThisClass);
}




}  // namespace TCC_Curve





