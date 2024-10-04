
#pragma hdrstop

#include "LineGeometry.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Ucomplex.h"
#include "Utilities.h"
#include "LineUnits.h"
#include "OHLineConstants.h"
#include "CNLineConstants.h"
#include "TSLineConstants.h"


using namespace std;
using namespace Arraydef;
using namespace CNData;
using namespace CNLineConstants;
using namespace Command;
using namespace ConductorData;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace LineConstants;
using namespace LineSpacing;
using namespace LineUnits;
using namespace OHLineConstants;
using namespace ParserDel;
using namespace System;
using namespace TSData;
using namespace TSLineConstants;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace Utilities;

namespace LineGeometry
{

ELineGeometryProblem::ELineGeometryProblem(const String &Msg) : inherited(Msg) {}

TLineGeometryObj::TLineGeometryObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TLineGeometryObj::TLineGeometryObj(String ClassName) : inherited(ClassName) {}
TLineGeometryObj::TLineGeometryObj() {}


TLineGeometryObj* ActiveLineGeometryObj = nullptr;

const int NumPropsThisClass = 19;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TLineGeometry::TLineGeometry()
{
	;
	Class_Name = "LineGeometry";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineGeometry::~TLineGeometry()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLineGeometry::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();
	PropertyName[1 - 1] = "nconds";
	PropertyName[2 - 1] = "nphases";
	PropertyName[3 - 1] = "cond";
	PropertyName[4 - 1] = "wire";
	PropertyName[5 - 1] = "x";
	PropertyName[6 - 1] = "h";
	PropertyName[7 - 1] = "units";
	PropertyName[8 - 1] = "normamps";
	PropertyName[9 - 1] = "emergamps";
	PropertyName[10 - 1] = "reduce";
	PropertyName[11 - 1] = "spacing";
	PropertyName[12 - 1] = "wires";
	PropertyName[13 - 1] = "cncable";
	PropertyName[14 - 1] = "tscable";
	PropertyName[15 - 1] = "cncables";
	PropertyName[16 - 1] = "tscables";
	PropertyName[17 - 1] = "Seasons";
	PropertyName[18 - 1] = "Ratings";
	PropertyName[19 - 1] = "LineType";
	PropertyHelp[1 - 1] = "Number of conductors in this geometry. Default is 3. Triggers memory allocations. Define first!";
	PropertyHelp[2 - 1] = "Number of phases. Default =3; All other conductors are considered neutrals and might be reduced out.";
	PropertyHelp[3 - 1] = "Set this = number of the conductor you wish to define. Default is 1.";
	PropertyHelp[4 - 1] = String("Code from WireData. MUST BE PREVIOUSLY DEFINED. no default.") + CRLF
	           + "Specifies use of Overhead Line parameter calculation,"
	           + CRLF
	           + "Unless Tape Shield cable previously assigned to phases, and this wire is a neutral.";
	PropertyHelp[5 - 1] = "x coordinate.";
	PropertyHelp[6 - 1] = "Height of conductor.";
	PropertyHelp[7 - 1] = "Units for x and h: {mi|kft|km|m|Ft|in|cm } Initial default is \"ft\", but defaults to last unit defined";
	PropertyHelp[8 - 1] = "Normal ampacity, amperes for the line. Defaults to first conductor if not specified.";
	PropertyHelp[9 - 1] = "Emergency ampacity, amperes. Defaults to first conductor if not specified.";
	PropertyHelp[10 - 1] = "{Yes | No} Default = no. Reduce to Nphases (Kron Reduction). Reduce out neutrals.";
	PropertyHelp[11 - 1] = String("Reference to a LineSpacing for use in a line constants calculation.") + CRLF
	           + "Alternative to x, h, and units. MUST BE PREVIOUSLY DEFINED."
	           + CRLF
	           + "Must match \"nconds\" as previously defined for this geometry."
	           + CRLF
	           + "Must be used in conjunction with the Wires property.";
	PropertyHelp[12 - 1] = String("Array of WireData names for use in a line constants calculation.") + CRLF
	           + "Alternative to individual wire inputs. ALL MUST BE PREVIOUSLY DEFINED."
	           + CRLF
	           + "Must match \"nconds\" as previously defined for this geometry,"
	           + CRLF
	           + "unless TSData or CNData were previously assigned to phases, and these wires are neutrals."
	           + CRLF
	           + "Must be used in conjunction with the Spacing property.";
	PropertyHelp[13 - 1] = String("Code from CNData. MUST BE PREVIOUSLY DEFINED. no default.") + CRLF
	           + "Specifies use of Concentric Neutral cable parameter calculation.";
	PropertyHelp[14 - 1] = String("Code from TSData. MUST BE PREVIOUSLY DEFINED. no default.") + CRLF
	           + "Specifies use of Tape Shield cable parameter calculation.";
	PropertyHelp[15 - 1] = String("Array of CNData names for cable parameter calculation.") + CRLF
	           + "All must be previously defined, and match \"nphases\" for this geometry."
	           + CRLF
	           + "You can later define \"nconds-nphases\" wires for bare neutral conductors.";
	PropertyHelp[16 - 1] = String("Array of TSData names for cable parameter calculation.") + CRLF
	           + "All must be previously defined, and match \"nphases\" for this geometry."
	           + CRLF
	           + "You can later define \"nconds-nphases\" wires for bare neutral conductors.";
	PropertyHelp[17 - 1] = "Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "
	           "\"Ratings\" property. Defaults to first conductor if not specified.";
	PropertyHelp[18 - 1] = String("An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert") + CRLF
	           + "multiple ratings to change during a QSTS simulation to evaluate different ratings in lines."
	           + "Defaults to first conductor if not specified.";
	PropertyHelp[19 - 1] = String("Code designating the type of line. ") + CRLF
	           + "One of: OH, UG, UG_TS, UG_CN, SWT_LDBRK, SWT_FUSE, SWT_SECT, SWT_REC, SWT_DISC, SWT_BRK, SWT_ELBOW"
	           + CRLF
	           + CRLF
	           + "OpenDSS currently does not use this internally. For whatever purpose the user defines. Default is OH.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineGeometry::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TLineGeometryObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineGeometry::Edit(int ActorID)
{
	int result = 0;
	int i = 0;
	int iStart = 0;
	int istop = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveLineGeometryObj = ((TLineGeometryObj*) ElementList.Get_Active());
	ActiveDSSObject[ActorID] = ActiveLineGeometryObj;
	/*# with ActiveLineGeometryObj do */
	{
		auto with0 = ActiveLineGeometryObj;
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
	           + "\"", 10101);
				break;
				case 	1:
				with0->Set_Nconds(Parser[ActorID]->MakeInteger_());
				break;  // Use property value to force reallocations
				case 	2:
				with0->Fnphases = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->set_ActiveCond(Parser[ActorID]->MakeInteger_());
				break;
				case 	4:
				{
					with0->FCondName[with0->get_FActiveCond() - 1] = Param;
					if((with0->FPhaseChoice[with0->get_FActiveCond() - 1] == unknown) || (with0->FPhaseChoice[with0->get_FActiveCond() - 1] == Overhead))
						with0->ChangeLineConstantsType(Overhead);
				}
				break;
				case 	5:
				with0->FX[with0->get_FActiveCond() - 1] = Parser[ActorID]->MakeDouble_();
				break;
				case 	6:
				with0->FY[with0->get_FActiveCond() - 1] = Parser[ActorID]->MakeDouble_();
				break;
				case 	7:
				{
					with0->FUnits[with0->get_FActiveCond() - 1] = GetUnitsCode(Param);
					with0->FLastUnit = (with0->FUnits)[with0->get_FActiveCond() - 1];
				}
				break;
				case 	8:
				with0->NormAmps = Parser[ActorID]->MakeDouble_();
				break;
				case 	9:
				with0->EmergAmps = Parser[ActorID]->MakeDouble_();
				break;
				case 	10:
				with0->FReduce = InterpretYesNo(Param);
				break;
				case 	11:
				{
					with0->FSpacingType = Parser[ActorID]->MakeString_();
					if(LineSpacingClass[ActorID]->SetActive(with0->FSpacingType))
					{
						ActiveLineSpacingObj = ((TLineSpacingObj*) LineSpacingClass[ActorID]->GetActiveObj());
						if(with0->Fnconds == ActiveLineSpacingObj->get_Fnconds())
						{
							int stop = 0;
							with0->FLastUnit = ActiveLineSpacingObj->get_FUnits();
							for(stop = with0->Fnconds, i = 1; i <= stop; i++)
							{
								with0->FX[i - 1] = ActiveLineSpacingObj->Get_FX(i);
								with0->FY[i - 1] = ActiveLineSpacingObj->Get_FY(i);
								with0->FUnits[i - 1] = with0->FLastUnit;
							}
						}
						else
						DoSimpleMsg(String("LineSpacing object ") + with0->FSpacingType
	           + " has the wrong number of wires.", 10103);
					}
					else
					DoSimpleMsg(String("LineSpacing object ") + with0->FSpacingType
	           + " has not been defined.", 10103);
				}
				break;
				case 	13:
				{
					(with0->FCondName)[with0->get_FActiveCond() - 1] = Param;
					with0->ChangeLineConstantsType(ConcentricNeutral);
				}
				break;
				case 	14:
				{
					(with0->FCondName)[with0->get_FActiveCond() - 1] = Param;
					with0->ChangeLineConstantsType(TapeShield);
				}
				break;
				case 	12: case 15: case 16:
				{
					int stop = 0;
					iStart = 1;
					istop = with0->Fnconds;
					if(ParamPointer == 15)
					{
						with0->ChangeLineConstantsType(ConcentricNeutral);
						istop = with0->Fnphases;
					}
					else
					{
						if(ParamPointer == 16)
						{
							with0->ChangeLineConstantsType(TapeShield);
							istop = with0->Fnphases;
						}
						else
						{
							if(ParamPointer == 12)
							{
								if(with0->FPhaseChoice[with0->get_FActiveCond() - 1] == unknown)
									with0->ChangeLineConstantsType(Overhead);
								else
								{
									if(with0->FPhaseChoice[with0->get_FActiveCond() - 1] != Overhead) // these are buried neutral wires
										iStart = with0->Fnphases + 1; // to fix the bug introduced with ActiveCond
								}
							}
						}
					}
					AuxParser[ActorID]->SetCmdString(Parser[ActorID]->MakeString_());
					for(stop = istop, i = iStart; i <= stop; i++)
					{
						String dummy = AuxParser[ActorID]->GetNextParam(); // ignore any parameter name  not expecting any
						(with0->FCondName)[i - 1] = AuxParser[ActorID]->MakeString_();
						if(ParamPointer == 15)
							CNDataClass[ActorID]->Set_Code((with0->FCondName)[i - 1]);
						else
						{
							if(ParamPointer == 16)
								TSDataClass[ActorID]->Set_Code((with0->FCondName)[i - 1]);
							else
								WireDataClass[ActorID]->Set_Code((with0->FCondName)[i - 1]);
						}
						if(ASSIGNED(ActiveConductorDataObj))
						{
							with0->FWireData[i - 1] = ActiveConductorDataObj;
							if(i == 1)
							{
								if((ActiveConductorDataObj->NormAmps > 0.0) && (with0->NormAmps == 0.0))
									with0->NormAmps = ActiveConductorDataObj->NormAmps;
								if((ActiveConductorDataObj->EmergAmps > 0.0) && (with0->EmergAmps == 0.0))
									with0->EmergAmps = ActiveConductorDataObj->EmergAmps;
								if((ActiveConductorDataObj->NumAmpRatings > 1) && (with0->NumAmpRatings == 1))
									with0->NumAmpRatings = ActiveConductorDataObj->NumAmpRatings;
								if((!ActiveConductorDataObj->AmpRatings.empty()) && (!with0->AmpRatings.empty()))
								{
									with0->AmpRatings.resize(with0->NumAmpRatings);
									with0->AmpRatings = ActiveConductorDataObj->AmpRatings;
								}
							}
						}
						else
						{
							if(ParamPointer == 15)
								DoSimpleMsg(String("CNData Object \"") + (with0->FCondName)[i]
	           + "\" not defined. Must be previously defined.", 10103);
							else
							{
								if(ParamPointer == 16)
									DoSimpleMsg(String("TSData Object \"") + (with0->FCondName)[i]
	           + "\" not defined. Must be previously defined.", 10103);
								else
									DoSimpleMsg(String("WireData Object \"") + (with0->FCondName)[i]
	           + "\" not defined. Must be previously defined.", 10103);
							}
						}
					}
				}
				break;
				case 	17:
				{
					with0->NumAmpRatings = Parser[ActorID]->MakeInteger_();
					with0->AmpRatings.resize(with0->NumAmpRatings);
				}
				break;
				case 	18:
				{
					with0->AmpRatings.resize(with0->NumAmpRatings); 
					Param = Parser[ActorID]->MakeString_();
					with0->NumAmpRatings = InterpretDblArray(Param, with0->NumAmpRatings, (pDoubleArray) &(with0->AmpRatings[0]));
				}
				break;
				case 	19:
				with0->FLineType = LineTypeList.Getcommand(Param);
				break;
           // Inherited parameters
				default:
				ClassEdit(ActiveLineGeometryObj, ParamPointer - NumPropsThisClass);
				break;
			}

         /*Set defaults*/
			switch(ParamPointer)
			{
				case 	2:
				if(with0->Fnphases > with0->Fnconds)
					with0->Fnphases = with0->Fnconds;
				break;
				case 	3:
				if((with0->get_FActiveCond() < 1) || (with0->get_FActiveCond() > with0->Fnconds))
					DoSimpleMsg(String("Illegal cond= specification in Line Geometry:") + CRLF
	           + Parser[ActorID]->get_CmdBuffer(), 10102);
				break;
				case 	4: case 13: case 14:
				{
					if(ParamPointer == 4)
						WireDataClass[ActorID]->Set_Code(Param);
					else
					{
						if(ParamPointer == 13)
							CNDataClass[ActorID]->Set_Code(Param);
						else
							TSDataClass[ActorID]->Set_Code(Param);
					}
					if(ASSIGNED(ActiveConductorDataObj))
					{
						with0->FWireData[with0->get_FActiveCond() - 1] = ActiveConductorDataObj;
                  /*Default the current ratings for this geometry to the rating of the first conductor*/
						if(with0->get_FActiveCond() == 1)
						{
							if((ActiveConductorDataObj->NormAmps > 0.0) && (with0->NormAmps == 0.0))
								with0->NormAmps = ActiveConductorDataObj->NormAmps;
							if((ActiveConductorDataObj->EmergAmps > 0.0) && (with0->EmergAmps == 0.0))
								with0->EmergAmps = ActiveConductorDataObj->EmergAmps;
							if((ActiveConductorDataObj->NumAmpRatings > 1) && (with0->NumAmpRatings == 1))
								with0->NumAmpRatings = ActiveConductorDataObj->NumAmpRatings;
							if((!ActiveConductorDataObj->AmpRatings.empty()) && (!with0->AmpRatings.empty()))
							{
								with0->AmpRatings.resize(with0->NumAmpRatings);
								with0->AmpRatings = ActiveConductorDataObj->AmpRatings;
							}
						}
					}
					else
					{
						if(ParamPointer == 4)
							DoSimpleMsg(String("WireData Object \"") + Param
	           + "\" not defined. Must be previously defined.", 10103);
						else
						{
							if(ParamPointer == 13)
								DoSimpleMsg(String("CNData Object \"") + Param
	           + "\" not defined. Must be previously defined.", 10103);
							else
								DoSimpleMsg(String("TSData Object \"") + Param
	           + "\" not defined. Must be previously defined.", 10103);
						}
					}
				}
				break;
				default:
				  ;
				break;
			}
			switch(ParamPointer)
			{
				case 	1: case 4: case 5: case 6: case 7: case 11: case 12: case 13: case 14: case 15: case 16:
				with0->DataChanged = true;
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

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineGeometry::MakeLike(const String LineName)
{
	int result = 0;
	TLineGeometryObj* OtherLineGeometry = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line code in the present collection*/
	OtherLineGeometry = ((TLineGeometryObj*) Find(LineName));
	if(OtherLineGeometry != nullptr)
		/*# with ActiveLineGeometryObj do */
		{
			auto with0 = ActiveLineGeometryObj;
			int stop = 0;
			with0->Set_Nconds(OtherLineGeometry->get_Fnconds());   // allocates
			with0->Fnphases = OtherLineGeometry->Fnphases;
			with0->FSpacingType = OtherLineGeometry->FSpacingType;
			for(stop = with0->Fnconds, i = 1; i <= stop; i++)
			{
				with0->FPhaseChoice[i - 1] = OtherLineGeometry->FPhaseChoice[i - 1];
			}
			for(stop = with0->Fnconds, i = 1; i <= stop; i++)
			{
				(with0->FCondName)[i - 1] = (OtherLineGeometry->FCondName)[i - 1];
			}
			for(stop = with0->Fnconds, i = 1; i <= stop; i++)
			{
				(with0->FWireData)[i - 1] = (OtherLineGeometry->FWireData)[i - 1];
			}
			for(stop = with0->Fnconds, i = 1; i <= stop; i++)
			{
				(with0->FX)[i - 1] = (OtherLineGeometry->FX)[i - 1];
			}
			for(stop = with0->Fnconds, i = 1; i <= stop; i++)
			{
				(with0->FY)[i - 1] = (OtherLineGeometry->FY)[i - 1];
			}
			for(stop = with0->Fnconds, i = 1; i <= stop; i++)
			{
				(with0->FUnits)[i - 1] = (OtherLineGeometry->FUnits)[i - 1];
			}
			with0->FReduce = OtherLineGeometry->FReduce;
			with0->DataChanged = true;
			with0->NormAmps = OtherLineGeometry->NormAmps;
			with0->EmergAmps = OtherLineGeometry->EmergAmps;
			with0->UpdateLineGeometryData(ActiveCircuit[ActiveActor]->Solution->get_FFrequency());
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherLineGeometry->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in LineGeometry MakeLike: \"") + LineName
	           + "\" Not Found.", 102);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineGeometry::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TLineGeometry.Init", -1);
	result = 0;
	return result;
}  // Returns active line code string

String TLineGeometry::Get_Code()
{
	String result;
	result = ((TLineGeometryObj*) ElementList.Get_Active())->get_Name();
	return result;
}  // sets the  active LineGeometry

void TLineGeometry::Set_Code(const String Value)
{
	TLineGeometryObj* LineGeometryObj = nullptr;
	ActiveLineGeometryObj = nullptr;
	LineGeometryObj = ((TLineGeometryObj*) ElementList.Get_First());
	while(LineGeometryObj != nullptr)
	{
		if(CompareText(LineGeometryObj->get_Name(), Value) == 0)
		{
			ActiveLineGeometryObj = LineGeometryObj;
			return;
		}
		LineGeometryObj = ((TLineGeometryObj*) ElementList.Get_Next());
	}
	DoSimpleMsg(String("LineGeometry: \"") + Value + "\" not Found.", 103);
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineGeometry Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineGeometryObj::TLineGeometryObj(TDSSClass* ParClass, const String LineGeometryName)
 : inherited(ParClass),
			Fnconds(0),
			Fnphases(0),
			FLastUnit(0),
			DataChanged(false),
			FReduce(false),
			FActiveCond(0),
			NormAmps(0.0),
			EmergAmps(0.0),
			NumAmpRatings(0),
			FLineType(0)
{
	Set_Name(LowerCase(LineGeometryName));

	FPhaseChoice = nullptr;
	DSSObjType = ParClass->DSSClassType;
	DataChanged = true;
	FCondName = nullptr;
	FWireData.clear();
	FX.clear();
	FY.clear();
	FUnits.clear();
	FLineData = nullptr;
	FSpacingType = "";

/* was causing unnecessary allocations (was leaving dangling memory)
      Nconds      := 3;  // Allocates terminals
      FNphases    := 3;
*/
	FActiveCond = 1;
	FLastUnit = UNITS_FT;
	NormAmps = 0.0;
	EmergAmps = 0.0;
	FLineType = 1;  // Default to OH  Line
	FReduce = false;
     /*Initialize dynamic array for ratings*/
	NumAmpRatings = 1;
	AmpRatings.resize(NumAmpRatings);
	AmpRatings[0] = NormAmps;
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineGeometryObj::~TLineGeometryObj()
{
	FLineData = nullptr;
	FreeStringArray(FCondName, Fnconds);
	FWireData.clear();
	FY.clear();
	FX.clear();
	FUnits.clear();
	// inherited::Destroy();
}


void TLineGeometryObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int i = 0;
	int j = 0;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = 2, i = 1; i <= stop; i++)
		{
			{ System::Write(f, "~ "); 
			System::Write(f, with0->PropertyName[i - 1]); 
			System::Write(f, "="); 
			System::WriteLn(f, GetPropertyValue(i)); }
		}
		for(stop = Fnconds, j = 1; j <= stop; j++)
		{
			set_ActiveCond(j);
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[3 - 1]); System::Write(f, "="); System::WriteLn(f, GetPropertyValue(3)); }
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[4 - 1]); System::Write(f, "="); System::WriteLn(f, GetPropertyValue(4)); }
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[5 - 1]); System::Write(f, "="); System::WriteLn(f, GetPropertyValue(5)); }
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[6 - 1]); System::Write(f, "="); System::WriteLn(f, GetPropertyValue(6)); }
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[7 - 1]); System::Write(f, "="); System::WriteLn(f, GetPropertyValue(7)); }
		}
		for(stop = with0->NumProperties, i = 8; i <= stop; i++)
		{
			{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[i - 1]); System::Write(f, "="); System::WriteLn(f, GetPropertyValue(i)); }
		}
	}
}

String TLineGeometryObj::GetPropertyValue(int Index)
{
	String result;
	int j = 0;
	int i = 0;
/*Return Property Value for Active index*/
	switch(Index)
	{
		case 	3:
		result = Format("%d", FActiveCond);
		break;
		case 	4:
		 case 13:
		 case 14:
		result = FCondName[FActiveCond - 1];
		break;
		case 	5:
		result = Format("%-g", FX[FActiveCond - 1]);
		break;
		case 	6:
		result = Format("%-g", FY[FActiveCond - 1]);
		break;
		case 	7:
		result = LineUnitsStr((FUnits)[FActiveCond - 1]);
		break;
		case 	8:
		result = Format("%-g", NormAmps);
		break;
		case 	9:
		result = Format("%-g", EmergAmps);
		break;
		case 	12:
		 case 15:
		 case 16:
		{
			int stop = 0;
			result = "[";
			for(stop = Fnconds, i = 1; i <= stop; i++)
			{
				result = result + FCondName[i - 1] + " ";
			}
			result = result + "]";
		}
		break;
		case 	17:
		result = IntToStr(NumAmpRatings);
		break;
		case 	18:
		{
			int stop = 0;
			result = "[";
			for(stop = NumAmpRatings, j = 1; j <= stop; j++)
			{
				result = result + FloatToStrF(AmpRatings[j - 1], ffGeneral, 8, 4) + ",";
			}
			result = result + "]";
		}
		break;
		case 	19:
		result = LineTypeList.Get(FLineType);
		break;
     // Inherited parameters
		default:
		result = inherited::GetPropertyValue(Index);
		break;
	}
	return result;
}

double TLineGeometryObj::Get_FX(int i)
{
	double result = 0.0;
	if(i <= Fnconds)
		result = FX[i - 1];
	else
		result = 0.0;
	return result;
}

double TLineGeometryObj::Get_FY(int i)
{
	double result = 0.0;
	if(i <= Fnconds)
		result = FY[i - 1];
	else
		result = 0.0;
	return result;
}

int TLineGeometryObj::Get_FUnits(int i)
{
	int result = 0;
	if(i <= Fnconds)
		result = FUnits[i - 1];
	else
		result = 0;
	return result;
}

String TLineGeometryObj::Get_ConductorName(int i)
{
	String result;
	if(i <= Fnconds)
		result = FCondName[i - 1];
	else
		result = "";
	return result;
}

TConductorDataObj* TLineGeometryObj::Get_ConductorData(int i)
{
	TConductorDataObj* result = nullptr;
	if(i <= Fnconds)
		result = FWireData[i - 1];
	else
		result = nullptr;
	return result;
}

int TLineGeometryObj::get_Nconds()
{
	int result = 0;
	if(FReduce)
		result = Fnphases;
	else
		result = Fnconds;
	return result;
}

ConductorChoice TLineGeometryObj::Get_PhaseChoice(int i)
{
	ConductorChoice result = Overhead;
	result = FPhaseChoice[i - 1];
	return result;
}

double TLineGeometryObj::Get_RhoEarth()
{
	double result = 0.0;
	result = FLineData->get_FrhoEarth();
	return result;
}

TcMatrix* TLineGeometryObj::Get_YCmatrix(double f, double Lngth, int Units)
{
	TcMatrix* result = nullptr;
	result = nullptr;
	if(DataChanged)
		UpdateLineGeometryData(f);
	if(!SolutionAbort)
		result = FLineData->Get_YCmatrix(f,Lngth,Units);
	return result;
}

TcMatrix* TLineGeometryObj::Get_Zmatrix(double f, double Lngth, int Units)
{
	TcMatrix* result = nullptr;
	result = nullptr;
	if(DataChanged)
		UpdateLineGeometryData(f);
	if(!SolutionAbort)
		result = FLineData->Get_Zmatrix(f,Lngth,Units);
	return result;
}

void TLineGeometryObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,"3");
	Set_PropertyValue(2,"3");
	Set_PropertyValue(3,"1");
	Set_PropertyValue(4,"");
	Set_PropertyValue(5,"0");
	Set_PropertyValue(6,"32");
	Set_PropertyValue(7,"ft");
	Set_PropertyValue(8,"0");
	Set_PropertyValue(9,"0");
	Set_PropertyValue(17,"1"); // 1 season
	Set_PropertyValue(18,"[400]"); // 1 rating
	Set_PropertyValue(19,"OH"); // 1 rating
	inherited::InitPropertyValues(NumPropsThisClass);
}
/* Override standard SaveWrite*/
/*Linegeometry structure not conducive to standard means of saving*/
void TLineGeometryObj::SaveWrite(System::TTextRec& f)
{
	String TempStr;
	String strPhaseChoice;
	int j = 0;
	int iProp = 0;
	int i = 0;
	/*Write only properties that were explicitly set in the
	final order they were actually set*/
	iProp = GetNextPropertySet(0); // Works on ActiveDSSObject
	if (iProp > 0)
		WriteLn(f);
	while (iProp > 0)
	{
		/*# with ParentClass do */
		{
			auto with0 = ParentClass;
			switch ((with0->RevPropertyIdxMap)[iProp - 1])
			{
			case 	3:
			case 11:
			case 12:   // if cond=, spacing, or wires were ever used write out arrays ...
			{
				int stop = 0;
				for (stop = Fnconds, i = 1; i <= stop; i++)
				{
					switch (Get_PhaseChoice(i))
					{
					case 	Overhead:
						strPhaseChoice = "wire";
						break;
					case 	ConcentricNeutral:
						strPhaseChoice = "cncable";
						break;
					case 	TapeShield:
						strPhaseChoice = "tscable";
						break;
					default:
						strPhaseChoice = "wire";
						break;
					}
					WriteLn(f, Format(("~ Cond=%d " + strPhaseChoice + "= " + FCondName[i - 1] + " X=%.7g h=%.7g units=" + LineUnitsStr((FUnits)[i - 1])).c_str(), i, FX[i - 1], FY[i - 1]));
				}
			}
			break; /*do Nothing*/
			case 4: case 5: case 6: case 7:
				;
				break; // Ignore these properties;
			case 	8:
				WriteLn(f, Format("~ normamps=%.4g", NormAmps));
				break;
			case 	9:
				WriteLn(f, Format("~ emergamps=%.4g", EmergAmps));
				break;
			case 	10:
				if (FReduce)
					WriteLn(f, "~ Reduce=Yes");
				break;
			case 13: case 14:
				;
				break;   /*do Nothing*/ // Ignore these properties;
			case 	18:
			{
				int stop = 0;
				TempStr = "[";
				for (stop = NumAmpRatings, j = 1; j <= stop; j++)
				{
					TempStr = TempStr + FloatToStrF(AmpRatings[j - 1], ffGeneral, 8, 4) + ",";
				}
				TempStr = TempStr + "]";
				WriteLn(f, String("ratings=") + TempStr);
			}
			break;
			case 	19:
				WriteLn(f, String("~ LineType=%.4g") + LineTypeList.Get(FLineType));
				break;
			default:
				String PropValue = "=" + CheckForBlanks(String(Get_PropertyValue(iProp)));
				WriteLn(f, "~ " + with0->PropertyName[(with0->RevPropertyIdxMap)[iProp - 1] - 1] + PropValue);
				break;
			}
		}
		iProp = GetNextPropertySet(iProp);
	}
}


void TLineGeometryObj::set_ActiveCond(int Value)
{
	if(Value > 0)
	{
		if(Value <= Fnconds)
		{
			FActiveCond = Value;
			if(FUnits[FActiveCond - 1] ==  - 1)
				FUnits[FActiveCond - 1] = FLastUnit;  // makes this a sticky value so you don't have to repeat it
		}
	}
}

void TLineGeometryObj::ChangeLineConstantsType(ConductorChoice newPhaseChoice)
{
	TLineConstants* newLineData = nullptr;
	bool needNew = false;
	newLineData = nullptr;
	needNew = false;
	if(newPhaseChoice != FPhaseChoice[get_FActiveCond() - 1])
		needNew = true;
	if(!ASSIGNED(FLineData))
		needNew = true;
	else
	{
		if(Fnconds != FLineData->get_FNumConds())
			needNew = true;
	}
	if(needNew)
		switch(newPhaseChoice)
		{
			case 	Overhead:
			newLineData = new TOHLineConstants(Fnconds);
			break;
			case 	ConcentricNeutral:
			newLineData = new TCNLineConstants(Fnconds);
			break;
			case 	TapeShield:
			newLineData = new TTSLineConstants(Fnconds);
			break;
			default:
			  ;
			break;
		}
	if(ASSIGNED(newLineData))
	{
		if (ASSIGNED(FLineData))
		{
			newLineData->Set_NPhases(FLineData->get_FNumPhases());
			newLineData->Set_Frhoearth(FLineData->get_FrhoEarth());
		}
		else
			delete FLineData;
		FLineData = newLineData;
	}
	FPhaseChoice[get_FActiveCond() - 1] = newPhaseChoice;
}

//-------------------------------------------------------------------------------------------------------

int TLineGeometryObj::get_Fnphases()
{
	return Fnphases;
}

//-------------------------------------------------------------------------------------------------------

int TLineGeometryObj::get_FActiveCond()
{
	return FActiveCond;
}

//-------------------------------------------------------------------------------------------------------

int TLineGeometryObj::get_Fnconds()
{
	return Fnconds;
}

//-------------------------------------------------------------------------------------------------------

void TLineGeometryObj::Set_Nconds(int Value)
{
	int i = 0;
	int stop = 0;
	if(ASSIGNED(FCondName))
		FreeStringArray(FCondName, Fnconds);  // dispose of old allocation
	Fnconds = Value;
	if(ASSIGNED(FLineData))
		free( FLineData );


  /*Allocations*/
	FWireData.resize(Fnconds);
	FX.resize(Fnconds);
	FY.resize(Fnconds);
	FUnits.resize(Fnconds);
	FPhaseChoice = (pConductorChoiceArray) realloc(FPhaseChoice, sizeof(FPhaseChoice[0]) * Fnconds);
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FPhaseChoice[i - 1] = unknown;
	}
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		set_ActiveCond(i);
		ChangeLineConstantsType(Overhead);    // works on activecond
	}
	FCondName = new string[Fnconds];

/*Initialize Allocations*/
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FWireData[i - 1] = nullptr;
	}
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FX[i - 1] = 0.0;
	}
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FY[i - 1] = 0.0;
	}
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FUnits[i - 1] = -1;
	}  // default to ft
	FLastUnit = UNITS_FT;
}

void TLineGeometryObj::Set_NPhases(int Value)
{
	Fnphases = Value;
	FLineData->Set_NPhases(Value);
}

void TLineGeometryObj::Set_RhoEarth(double Value)
{
	FLineData->Set_Frhoearth(Value);
}

void TLineGeometryObj::UpdateLineGeometryData(double f)
{
	int i = 0;
	String LineGeomErrMsg;
	TCNDataObj* cnd = nullptr;
	TTSDataObj* tsd = nullptr;
	int stop = 0;

	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FLineData->Set_X(i, FUnits[i - 1], FX[i - 1]);
		FLineData->Set_Y(i, FUnits[i - 1], FY[i - 1]);
		FLineData->Set_radius(i, FWireData[i - 1]->get_FRadiusUnits(), FWireData[i - 1]->get_Fcapradius60());
		FLineData->Set_Capradius(i, FWireData[i - 1]->get_FRadiusUnits(), FWireData[i - 1]->get_Fcapradius60());
		FLineData->Set_GMR(i, FWireData[i - 1]->get_FGMRUnits(), FWireData[i - 1]->get_FGMR60());
		FLineData->Set_Rdc(i, FWireData[i - 1]->get_FResistanceUnits(), FWireData[i - 1]->get_FRDC());
		FLineData->Set_Rac(i, FWireData[i - 1]->get_FResistanceUnits(), FWireData[i - 1]->get_FR60());
		if( dynamic_cast< TCNDataObj* > ( FWireData[i - 1] )  != nullptr ) 
		{
			/*# with (FLineData as TCNLineConstants) do */
			{
				auto with0 = ((TCNLineConstants*) FLineData);
				cnd = (TCNDataObj*) FWireData[i - 1];
				with0->Set_EpsR(i, cnd->get_FEpsR());
				with0->Set_InsLayer(i, cnd->get_FRadiusUnits(), cnd->get_FInsLayer());
				with0->Set_DiaIns(i, cnd->get_FRadiusUnits(), cnd->get_FDiaIns());
				with0->Set_DiaCable(i, cnd->get_FRadiusUnits(), cnd->get_FDiaCable());
				with0->Set_kStrand(i, cnd->get_FkStrand());
				with0->Set_DiaStrand(i, cnd->get_FRadiusUnits(), cnd->get_FDiaStrand());
				with0->Set_GmrStrand(i, cnd->get_FGMRUnits(), cnd->get_FGmrStrand());
				with0->Set_RStrand(i, cnd->get_FResistanceUnits(), cnd->get_FRStrand());

			}
		}
		else
		{
			if( ( dynamic_cast<TTSDataObj*> ( FWireData[i - 1] ) ) != nullptr)
			{
				/*# with (FLineData as TTSLineConstants) do */
				{
					auto with1 = ((TTSLineConstants*) FLineData);
					tsd = ((TTSDataObj*) FWireData[i - 1]);
					with1->Set_EpsR(i, tsd->get_FEpsR());
					with1->Set_InsLayer(i, tsd->get_FRadiusUnits(), tsd->get_FInsLayer());
					with1->Set_DiaIns(i, tsd->get_FRadiusUnits(), tsd->get_FDiaIns());
					with1->Set_DiaCable(i, tsd->get_FRadiusUnits(), tsd->get_FDiaCable());
					with1->Set_DiaShield(i, tsd->get_FRadiusUnits(), tsd->get_FDiaShield());
					with1->Set_TapeLayer(i, tsd->get_FRadiusUnits(), tsd->get_FTapeLayer());
					with1->Set_TapeLap(i, tsd->get_FTapeLap());
				}
			}
		}
	}
	FLineData->Set_NPhases(Fnphases);
	DataChanged = false;

  /*Before we calc, check for bad conductor definitions*/
	if(FLineData->ConductorsInSameSpace(LineGeomErrMsg))
	{
		SolutionAbort = true;
		throw ELineGeometryProblem(String("Error in LineGeometry.") + get_Name() + ": " + LineGeomErrMsg);
	}
	else
	{
		FLineData->Calc(f);
		if(FReduce)
			FLineData->Reduce(); // reduce out neutrals
	}
}

void TLineGeometryObj::LoadSpacingAndWires(TLineSpacingObj* Spc, pConductorDataArray Wires)
{
	int i = 0;
	ConductorChoice newPhaseChoice = Overhead;
	int stop = 0;
	Set_Nconds(Spc->get_Fnconds());   // allocates
	Fnphases = Spc->get_Fnphases();
	FSpacingType = Spc->get_Name();
	if(Fnconds > Fnphases)
		FReduce = true;
	newPhaseChoice = Overhead;
	for(i = 1; i < Fnconds; i++)
	{
		if(dynamic_cast<TCNDataObj*> (Wires[i - 1]) != nullptr )
			newPhaseChoice = ConcentricNeutral;
		if(dynamic_cast<TTSDataObj*>(Wires[i - 1]) != nullptr )
			newPhaseChoice = TapeShield;
	}
	ChangeLineConstantsType(newPhaseChoice);
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FCondName[i - 1] = Wires[i - 1]->get_Name();
	}
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FWireData[i - 1] = Wires[i - 1];
	}
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FX[i - 1] = Spc->Get_FX(i);
	}
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FY[i - 1] = Spc->Get_FY(i);
	}
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		FUnits[i - 1] = Spc->get_FUnits();
	}
	DataChanged = true;
	NormAmps = (Wires)[1 - 1]->NormAmps;
	EmergAmps = (Wires)[1 - 1]->EmergAmps;
	UpdateLineGeometryData(ActiveCircuit[ActiveActor]->Solution->get_FFrequency());
}




}  // namespace LineGeometry

