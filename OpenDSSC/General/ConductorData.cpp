
#pragma hdrstop

#include "ConductorData.h"

#include "DSSGlobals.h"
#include "Utilities.h"

using namespace std;


namespace ConductorData
{

TConductorDataObj::TConductorDataObj(TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TConductorDataObj::TConductorDataObj(String ClassName) : inherited(ClassName) {}
TConductorDataObj::TConductorDataObj() {}


TConductorDataObj* ActiveConductorDataObj;
const string LineUnitsHelp = "{mi|kft|km|m|Ft|in|cm|mm} Default=none.";

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TConductorData::TConductorData()
{
	;
	DSSClassType = DSS_OBJECT;
}

TConductorData::~TConductorData()
{
	// inherited::Destroy();
}


void TConductorData::CountProperties()
{
	NumProperties = NumProperties + NumConductorClassProps;
	inherited::CountProperties();
}

void TConductorData::DefineProperties()
{
	(PropertyName)[ActiveProperty + 1 + 0] = "Rdc";
	(PropertyName)[ActiveProperty + 1 + 1] = "Rac";
	(PropertyName)[ActiveProperty + 1 + 2] = "Runits";
	(PropertyName)[ActiveProperty + 1 + 3] = "GMRac";
	(PropertyName)[ActiveProperty + 1 + 4] = "GMRunits";
	(PropertyName)[ActiveProperty + 1 + 5] = "radius";
	(PropertyName)[ActiveProperty + 1 + 6] = "radunits";
	(PropertyName)[ActiveProperty + 1 + 7] = "normamps";
	(PropertyName)[ActiveProperty + 1 + 8] = "emergamps";
	(PropertyName)[ActiveProperty + 1 + 9] = "diam";
	(PropertyName)[ActiveProperty + 1 + 10] = "Seasons";
	(PropertyName)[ActiveProperty + 1 + 11] = "Ratings";
	(PropertyName)[ActiveProperty + 1 + 12] = "Capradius";
	(PropertyHelp)[ActiveProperty + 1 + 0] = "dc Resistance, ohms per unit length (see Runits). Defaults to Rac/1.02 if not specified.";
	(PropertyHelp)[ActiveProperty + 1 + 1] = "Resistance at 60 Hz per unit length. Defaults to 1.02*Rdc if not specified.";
	(PropertyHelp)[ActiveProperty + 1 + 2] = "Length units for resistance: ohms per " "{mi|kft|km|m|Ft|in|cm|mm} Default=none.";
	(PropertyHelp)[ActiveProperty + 1 + 3] = "GMR at 60 Hz. Defaults to .7788*radius if not specified.";
	(PropertyHelp)[ActiveProperty + 1 + 4] = "Units for GMR: " "{mi|kft|km|m|Ft|in|cm|mm} Default=none.";
	(PropertyHelp)[ActiveProperty + 1 + 5] = "Outside radius of conductor. Defaults to GMR/0.7788 if not specified.";
	(PropertyHelp)[ActiveProperty + 1 + 6] = "Units for outside radius: " "{mi|kft|km|m|Ft|in|cm|mm} Default=none.";
	(PropertyHelp)[ActiveProperty + 1 + 7] = "Normal ampacity, amperes. Defaults to Emergency amps/1.5 if not specified.";
	(PropertyHelp)[ActiveProperty + 1 + 8] = "Emergency ampacity, amperes. Defaults to 1.5 * Normal Amps if not specified.";
	(PropertyHelp)[ActiveProperty + 1 + 9] = "Diameter; Alternative method for entering radius.";
	(PropertyHelp)[ActiveProperty + 1 + 10] = "Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the \"Ratings\" property.";
	(PropertyHelp)[ActiveProperty + 1 + 11] = String("An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert") + CRLF
	           + "multiple ratings to change during a QSTS simulation to evaluate different ratings in lines.";
	(PropertyHelp)[ActiveProperty + 12] = "Equivalent conductor radius for capacitance calcs. Specify this for bundled conductors. Defaults to same value as radius. Define Diam or Radius property first.";
	ActiveProperty = ActiveProperty + NumConductorClassProps;
	inherited::DefineProperties();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TConductorData::ClassEdit(const void* activeObj, int ParamPointer)
{
	int result = 0;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	if(ParamPointer > 0)
		/*# with TConductorDataObj(activeObj) do */
		{
			auto with0 = ((TConductorDataObj*) activeObj);
			switch(ParamPointer)
			{
				case 	1:
				with0->FRDC = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	2:
				with0->FR60 = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	3:
				with0->FResistanceUnits = GetUnitsCode(Parser[ActiveActor]->MakeString_());
				break;
				case 	4:
				with0->FGMR60 = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	5:
				with0->FGMRUnits = GetUnitsCode(Parser[ActiveActor]->MakeString_());
				break;
				case 	6:
				with0->Fradius = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	7:
				with0->FRadiusUnits = GetUnitsCode(Parser[ActiveActor]->MakeString_());
				break;
				case 	8:
				with0->NormAmps = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	9:
				with0->EmergAmps = Parser[ActiveActor]->MakeDouble_();
				break;
				case 	10:
				with0->Fradius = Parser[ActiveActor]->MakeDouble_() / 2.0;
				break;
				case 	11:
				{
					with0->NumAmpRatings = Parser[ActiveActor]->MakeInteger_();
					with0->AmpRatings.resize(with0->NumAmpRatings);
				}
				break;
				case 	12:
				{
					with0->AmpRatings.resize(with0->NumAmpRatings); 
					Param = Parser[ActiveActor]->MakeString_();
					with0->NumAmpRatings = InterpretDblArray(Param, with0->NumAmpRatings, ((pDoubleArray) &(with0->AmpRatings[0])));
				}
				break;
				case 	13:
				with0->Fcapradius60 = Parser[ActiveActor]->MakeDouble_();
				break;
				default:
				inherited::ClassEdit(activeObj, ParamPointer - NumConductorClassProps);
				break;
			}
      /*Set defaults*/
			switch(ParamPointer)
			{
				case 	1:
				if(with0->FR60 < 0.0)
					with0->FR60 = 1.02 * with0->FRDC;
				break;
				case 	2:
				if(with0->FRDC < 0.0)
					with0->FRDC = with0->FR60 / 1.02;
				break;
				case 	4:
				{
					if(with0->Fradius < 0.0)
						with0->Fradius = with0->FGMR60 / 0.7788;  // Default to cylindrical conductor
				}
				break;
				case 	5:
				if(with0->FRadiusUnits == 0)
					with0->FRadiusUnits = with0->FGMRUnits;
				break;
				case 	6: case 10:
				{
					if(with0->FGMR60 < 0.0)
						with0->FGMR60 = 0.7788 * with0->Fradius;
					if(with0->Fcapradius60 < 0.0)
						with0->Fcapradius60 = with0->Fradius;    // default to radius
				}
				break;
				case 	7:
				if(with0->FGMRUnits == 0)
					with0->FGMRUnits = with0->FRadiusUnits;
				break;
				case 	8:
				if(with0->EmergAmps < 0.0)
					with0->EmergAmps = 1.5 * with0->NormAmps;
				break;
				case 	9:
				if(with0->NormAmps < 0.0)
					with0->NormAmps = with0->EmergAmps / 1.5;
				break;
				default:
				  ;
				break;
			}
      /*Check for critical errors*/
			switch(ParamPointer)
			{
				case 	4:
				if(with0->Fradius == 0.0)
					DoSimpleMsg(String("Error: Radius is specified as zero for ConductorData.") + with0->get_Name(), 999);
				break;
				case 	6:
				if(with0->FGMR60 == 0.0)
					DoSimpleMsg(String("Error: GMR is specified as zero for ConductorData.") + with0->get_Name(), 999);
				break;
				default:
				  ;
				break;
			}
		}
	return result;
}

//------------------------------------------------------------------------------------------------------

double TConductorDataObj::get_FRDC()
{
	return FRDC;
}

//------------------------------------------------------------------------------------------------------

double TConductorDataObj::get_FR60()
{
	return FR60;
}

//------------------------------------------------------------------------------------------------------

double TConductorDataObj::get_FGMR60()
{
	return FGMR60;
}

//------------------------------------------------------------------------------------------------------

double TConductorDataObj::get_Fcapradius60()
{
	return Fcapradius60;
}

//------------------------------------------------------------------------------------------------------
double TConductorDataObj::get_Fradius()
{
	return Fradius;
}

//------------------------------------------------------------------------------------------------------

int TConductorDataObj::get_FResistanceUnits()
{
	return FResistanceUnits;
}

//------------------------------------------------------------------------------------------------------

int TConductorDataObj::get_FRadiusUnits()
{
	return FRadiusUnits;
}

//------------------------------------------------------------------------------------------------------

int TConductorDataObj::get_FGMRUnits()
{
	return FGMRUnits;
}

//------------------------------------------------------------------------------------------------------

void TConductorData::ClassMakeLike(const void* OtherObj)
{
	TConductorDataObj* OtherConductorData = nullptr;
	OtherConductorData = ((TConductorDataObj*) OtherObj);
	/*# with TConductorDataObj(ActiveDSSObject[ActiveActor]) do */
	{
		auto with0 = ((TConductorDataObj*) ActiveDSSObject[ActiveActor]);
		with0->FRDC = OtherConductorData->FRDC;
		with0->FR60 = OtherConductorData->FR60;
		with0->FResistanceUnits = OtherConductorData->FResistanceUnits;
		with0->FGMR60 = OtherConductorData->FGMR60;
		with0->Fcapradius60 = OtherConductorData->Fcapradius60;
		with0->FGMRUnits = OtherConductorData->FGMRUnits;
		with0->Fradius = OtherConductorData->Fradius;
		with0->FRadiusUnits = OtherConductorData->FRadiusUnits;
		with0->NormAmps = OtherConductorData->NormAmps;
		with0->EmergAmps = OtherConductorData->EmergAmps;
	}
  // Inherited ClassMakeLike(OtherObj);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TConductorData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TConductorDataObj::TConductorDataObj(TDSSClass* ParClass, const String ConductorDataName)
 : inherited(ParClass),
			FRDC(-1.0),
			FR60(-1.0),
			FGMR60(-1.0),
			Fcapradius60(-1.0),
			Fradius(0.0),
			FGMRUnits(0),
			FResistanceUnits(0),
			FRadiusUnits(0),
			NormAmps(0.0),
			EmergAmps(0.0),
			NumAmpRatings(0)
{
	Set_Name(LowerCase(ConductorDataName));
	DSSObjType = ParClass->DSSClassType;
	Fradius = -1.0;
	FGMRUnits = 0;
	FResistanceUnits = 0;
	FRadiusUnits = 0;
	NormAmps = -1.0;
	EmergAmps = -1.0;
  /*Initialize dynamic array for ratings*/
	NumAmpRatings = 1;
	AmpRatings.resize(NumAmpRatings);
	AmpRatings[0] = NormAmps;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TConductorDataObj::~TConductorDataObj()
{
	// inherited::Destroy();
}


void TConductorDataObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int j = 0;
	int i = 0;
	String TempStr;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = NumConductorClassProps, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[GetNumProperties(0) + i - 1]); Write(f, L'='); }
			switch(i)
			{
				case 	1:
				WriteLn(f, Format("%.6g", FRDC));
				break;
				case 	2:
				WriteLn(f, Format("%.6g", FR60));
				break;
				case 	3:
				WriteLn(f, LineUnitsStr(FResistanceUnits));
				break;
				case 	4:
				WriteLn(f, Format("%.6g", FGMR60));
				break;
				case 	5:
				WriteLn(f, LineUnitsStr(FGMRUnits));
				break;
				case 	6:
				WriteLn(f, Format("%.6g", Fradius));
				break;
				case 	7:
				WriteLn(f, LineUnitsStr(FRadiusUnits));
				break;
				case 	8:
				WriteLn(f, Format("%.6g", NormAmps));
				break;
				case 	9:
				WriteLn(f, Format("%.6g", EmergAmps));
				break;
				case 	10:
				WriteLn(f, Format("%.6g", get_Fradius() * 2.0));
				break;
				case 	11:
				WriteLn(f, Format("%d", NumAmpRatings));
				break;
				case 	12:
				{
					int stop1 = 0;
					TempStr = "[";
					for(stop1 = NumAmpRatings, j = 1; j <= stop1; j++)
					{
						TempStr = TempStr + FloatToStrF(AmpRatings[j - 1], ffGeneral, 8, 4) + ",";
					}
					TempStr = TempStr + "]";
					WriteLn(f, TempStr);
				}
				break;
				case 	13:
				WriteLn(f, Format("%.6g", Fcapradius60));
				break;
				default:
				  ;
				break;
			}
		}
	}
}

String TConductorDataObj::GetPropertyValue(int Index)
{
	int j = 0;
	String TempStr;
	String result;
	result = "";
	switch(Index)
	{
		case 	1:
		result = Format("%.6g", FRDC);
		break;
		case 	2:
		result = Format("%.6g", FR60);
		break;
		case 	3:
		result = LineUnitsStr(FResistanceUnits);
		break;
		case 	4:
		result = Format("%.6g", FGMR60);
		break;
		case 	5:
		result = LineUnitsStr(FGMRUnits); 
		break;
		case 	6:
		result = Format("%.6g", Fradius);
		break;
		case 	7:
		result = LineUnitsStr(FRadiusUnits);
		break;
		case 	8:
		result = Format("%.6g", NormAmps);
		break;
		case 	9:
		result = Format("%.6g", EmergAmps);
		break;
		case 	10:
		result = Format("%.6g", Fradius * 2.0);
		break;
		case 	11:
		result = Format("%d", NumAmpRatings);
		break;
		case 	12:
		{
			int stop1 = 0;
			TempStr = "[";
			for(stop1 = NumAmpRatings, j = 1; j <= stop1; j++)
			{
				TempStr = TempStr + FloatToStrF(AmpRatings[j - 1], ffGeneral, 8, 4) + ",";
			}
			TempStr = TempStr + "]";
			result = TempStr;
		}
		break;
		case 	13:
		result = Format("%.6g", Fcapradius60);
		break;
		default:
		result = inherited::GetPropertyValue(GetNumProperties(0) + Index);
		break;
	}
	return result;
}

int TConductorDataObj::GetNumProperties(int ArrayOffset)
{
	DoErrorMsg(String("Something is Wrong.  Got to base Conductor GetNumProperties for Object:") + CRLF + 
		PName + "." + LName,
        "N/A",
        "Should not be able to get here. Probable Programming Error.", 400);

	return 0;
}

/*
function TConductorDataObj.GetPropertyValue(Index: Integer): String;
Var
        i, j:Integer;
        Tempstr : String;
begin

    Result := '';
    CASE Index of  // Special cases
        1 : Result := Format('%.6g',[FRDC]);
        2 : Result := Format('%.6g',[FR60]);
        3 : Result := Format('%s',[LineUnitsStr(FresistanceUnits)]);
        4 : Result := Format('%.6g',[FGMR60]);
        5 : Result := Format('%s',[LineUnitsStr(FGMRUnits)]);
        6 : Result := Format('%.6g',[Fradius]);
        7 : Result := Format('%s',[LineUnitsStr(FRadiusUnits)]);
        8 : Result := Format('%.6g',[NormAmps]);
        9 : Result := Format('%.6g',[EmergAmps]);
       10 : Result := Format('%.6g',[radius*2.0]);
       11 : Result := Format('%d',[NumAmpRatings]);
       12 : Begin
              TempStr   :=  '[';
              for  j:= 1 to NumAmpRatings do
                TempStr :=  TempStr + floattoStrf(AmpRatings[j-1],ffgeneral,8,4) + ',';
              TempStr   :=  TempStr + ']';
              Result := TempStr;
            End;
       13: Result := Format('%.6g',[Fcapradius60]);
    ELSE
       Result := Inherited GetPropertyValue(index);
    END;

end;
*/

void TConductorDataObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(ArrayOffset + 1,"-1");
	Set_PropertyValue(ArrayOffset + 2,"-1");
	Set_PropertyValue(ArrayOffset + 3,"none");
	Set_PropertyValue(ArrayOffset + 4,"-1");
	Set_PropertyValue(ArrayOffset + 5,"none");
	Set_PropertyValue(ArrayOffset + 6,"-1");
	Set_PropertyValue(ArrayOffset + 7,"none");
	Set_PropertyValue(ArrayOffset + 8,"-1");
	Set_PropertyValue(ArrayOffset + 9,"-1");
	Set_PropertyValue(ArrayOffset + 10,"-1");
	Set_PropertyValue(ArrayOffset + 11,"1");
	Set_PropertyValue(ArrayOffset + 12,"[-1]");
	Set_PropertyValue(ArrayOffset + 13,"-1");
	inherited::InitPropertyValues(ArrayOffset + 13);
}




}  // namespace ConductorData





