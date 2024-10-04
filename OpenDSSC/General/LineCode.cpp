
#pragma hdrstop

#include "LineCode.h"
#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Ucomplex.h"
#include "Utilities.h"
#include "LineUnits.h"

using namespace std;
using namespace Arraydef;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace LineUnits;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace Utilities;

namespace LineCode
{

TLineCodeObj::TLineCodeObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TLineCodeObj::TLineCodeObj(String ClassName) : inherited(ClassName) {}
TLineCodeObj::TLineCodeObj() {}


TLineCode* LineCodeClass = nullptr;
TLineCodeObj* ActiveLineCodeObj = nullptr;

const int NumPropsThisClass = 27;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // Creates superstructure for all Line objects

TLineCode::TLineCode()
 : SymComponentsChanged(false),
			MatrixChanged(false)
{
	;
	Class_Name = "LineCode";
	DSSClassType = DSS_OBJECT;
	ActiveElement = 0;
	DefineProperties();
	std::string* slc = Slice(PropertyName, NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
	LineCodeClass = this;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineCode::~TLineCode()
{

    // ElementList and  CommandList freed in inherited destroy
	// inherited::Destroy();
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLineCode::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();
	(PropertyName)[1 - 1] = "nphases";
	(PropertyName)[2 - 1] = "r1";
	(PropertyName)[3 - 1] = "x1";
	(PropertyName)[4 - 1] = "r0";
	(PropertyName)[5 - 1] = "x0";
	(PropertyName)[6 - 1] = "C1";
	(PropertyName)[7 - 1] = "C0";
	(PropertyName)[8 - 1] = "units";
	(PropertyName)[9 - 1] = "rmatrix";
	(PropertyName)[10 - 1] = "xmatrix";
	(PropertyName)[11 - 1] = "cmatrix";
	(PropertyName)[12 - 1] = "baseFreq";
	(PropertyName)[13 - 1] = "normamps";
	(PropertyName)[14 - 1] = "emergamps";
	(PropertyName)[15 - 1] = "faultrate";
	(PropertyName)[16 - 1] = "pctperm";
	(PropertyName)[17 - 1] = "repair";
	(PropertyName)[18 - 1] = "Kron";
	(PropertyName)[19 - 1] = "Rg";
	(PropertyName)[20 - 1] = "Xg";
	(PropertyName)[21 - 1] = "rho";
	(PropertyName)[22 - 1] = "neutral";
	(PropertyName)[23 - 1] = "B1";
	(PropertyName)[24 - 1] = "B0";
	(PropertyName)[25 - 1] = "Seasons";
	(PropertyName)[26 - 1] = "Ratings";
	(PropertyName)[27 - 1] = "LineType";
	(PropertyHelp)[1 - 1] = "Number of phases in the line this line code data represents.  Setting this property reinitializes the line code.  Impedance matrix is reset for default symmetrical component.";
	(PropertyHelp)[2 - 1] = "Positive-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition. See also Rmatrix.";
	(PropertyHelp)[3 - 1] = "Positive-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition. See also Xmatrix";
	(PropertyHelp)[4 - 1] = "Zero-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition.";
	(PropertyHelp)[5 - 1] = "Zero-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition.";
	(PropertyHelp)[6 - 1] = "Positive-sequence capacitance, nf per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition. See also Cmatrix and B1.";
	(PropertyHelp)[7 - 1] = "Zero-sequence capacitance, nf per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces "
	           "the program to use the symmetrical component line definition. See also B0.";
	(PropertyHelp)[8 - 1] = "One of (ohms per ...) {none|mi|km|kft|m|me|ft|in|cm}.  Default is none; assumes units agree with length units"
	           "given in Line object";
	(PropertyHelp)[9 - 1] = "Resistance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. "
	           "May be used to specify the impedance of any line configuration.  For balanced line models, you may "
	           "use the standard symmetrical component data definition instead.";
	(PropertyHelp)[10 - 1] = "Reactance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. "
	           "May be used to specify the impedance of any line configuration.  For balanced line models, you may "
	           "use the standard symmetrical component data definition instead.";
	(PropertyHelp)[11 - 1] = "Nodal Capacitance matrix, lower triangle, nf per unit length.Order of the matrix is the number of phases. "
	           "May be used to specify the shunt capacitance of any line configuration.  For balanced line models, you may "
	           "use the standard symmetrical component data definition instead.";
	(PropertyHelp)[12 - 1] = "Frequency at which impedances are specified.";
	(PropertyHelp)[13 - 1] = "Normal ampere limit on line.  This is the so-called Planning Limit. It may also be "
	           "the value above which load will have to be dropped in a contingency.  Usually about "
	           "75% - 80% of the emergency (one-hour) rating.";
	(PropertyHelp)[14 - 1] = "Emergency ampere limit on line (usually one-hour rating).";
	(PropertyHelp)[15 - 1] = "Number of faults per unit length per year.";
	(PropertyHelp)[16 - 1] = "Percentage of the faults that become permanent.";
	(PropertyHelp)[17 - 1] = "Hours to repair.";
	(PropertyHelp)[18 - 1] = "Kron = Y/N. Default=N.  Perform Kron reduction on the impedance matrix after it is formed, reducing order by 1. "
	           "Eliminates the conductor designated by the \"Neutral=\" property. "
	           "Do this after the R, X, and C matrices are defined. Ignored for symmetrical components. "
	           "May be issued more than once to eliminate more than one conductor by resetting the Neutral property after the previous "
	           "invoking of this property. Generally, you do not want to do a Kron reduction on the matrix if you intend to solve at a "
	           "frequency other than the base frequency and exploit the Rg and Xg values.";
	(PropertyHelp)[19 - 1] = "Carson earth return resistance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. "
	           "Default is 0.01805 = 60 Hz value in ohms per kft (matches default line impedances). "
	           "This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. "
	           "If not, set both Rg and Xg = 0.";
	(PropertyHelp)[20 - 1] = "Carson earth return reactance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. "
	           "Default value is 0.155081 = 60 Hz value in ohms per kft (matches default line impedances). "
	           "This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. "
	           "If not, set both Rg and Xg = 0.";
	(PropertyHelp)[21 - 1] = "Default=100 meter ohms.  Earth resitivity used to compute earth correction factor.";
	(PropertyHelp)[22 - 1] = "Designates which conductor is the \"neutral\" conductor that will be eliminated by Kron reduction. "
	           "Default is the last conductor (nphases value). After Kron reduction is set to 0. Subsequent issuing of Kron=Yes "
	           "will not do anything until this property is set to a legal value. Applies only to LineCodes defined by R, X, and C matrix.";
	(PropertyHelp)[23 - 1] = "Alternate way to specify C1. MicroS per unit length";
	(PropertyHelp)[24 - 1] = "Alternate way to specify C0. MicroS per unit length";
	(PropertyHelp)[25 - 1] = "Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the \"Ratings\" property.";
	(PropertyHelp)[26 - 1] = String("An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert") + CRLF
	           + "multiple ratings to change during a QSTS simulation to evaluate different ratings in lines.";
	(PropertyHelp)[27 - 1] = String("Code designating the type of line. ") + CRLF
	           + "One of: OH, UG, UG_TS, UG_CN, SWT_LDBRK, SWT_FUSE, SWT_SECT, SWT_REC, SWT_DISC, SWT_BRK, SWT_ELBOW"
	           + CRLF
	           + CRLF
	           + "OpenDSS currently does not use this internally. For whatever purpose the user defines. Default is OH.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineCode::NewObject(const String ObjName)
{
	int result = 0;
   // create a new object of this class and add to list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		ActiveDSSObject[ActiveActor] = new TLineCodeObj(this, ObjName);
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

String TLineCodeObj::get_Rmatrix()
{
	String result;
	int j = 0;
	int i = 0;
	int stop = 0;
	result = "[";
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Fnphases, j = 1; j <= stop1; j++)
		{
			result = result + Format("%12.8f ", Z->GetElement(i, j).re);
		}
		if(i < Fnphases)
			result = result + "|";
	}
	result = result + "]";
	return result;
}

String TLineCodeObj::get_Xmatrix()
{
	String result;
	int j = 0;
	int i = 0;
	int stop = 0;
	result = "[";
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Fnphases, j = 1; j <= stop1; j++)
		{
			result = result + Format("%12.8f ", Z->GetElement(i, j).im);
		}
		if(i < Fnphases)
			result = result + "|";
	}
	result = result + "]";
	return result;
}

String TLineCodeObj::get_CMatrix()
{
	String result;
	int i = 0;
	int j = 0;
	int stop = 0;
	result = "[";
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		for(stop1 = Fnphases, j = 1; j <= stop1; j++)
		{
			result = result
	           + Format("%12.8f ", double(YC->GetElement(i, j).im) / DSSGlobals::TwoPi / BaseFrequency * 1.0E9);
		}
		if(i < Fnphases)
			result = result + "|";
	}
	result = result + "]";
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// decodes the units string and sets the Units variable

void TLineCode::SetUnits(const String s)
{
	ActiveLineCodeObj->Units = GetUnitsCode(s);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// set symmetrical component impedances and a flag to indicate they were changed

void TLineCode::SetZ1Z0(int i, double Value)
{
	SymComponentsChanged = true;
	/*# with ActiveLineCodeObj do */
	{
		auto with0 = ActiveLineCodeObj;
		with0->SymComponentsModel = true;
		switch(i)
		{
			case 	1:
			with0->R1 = Value;
			break;
			case 	2:
			with0->X1 = Value;
			break;
			case 	3:
			with0->R0 = Value;
			break;
			case 	4:
			with0->X0 = Value;
			break;
			case 	5:
			with0->C1 = Value;
			break;
			case 	6:
			with0->C0 = Value;
			break;
			default:
			  ;
			break;
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLineCode::DoMatrix(int i, int ActorID)
{
	int OrderFound = 0;
	int Norder = 0;
	int j = 0;
	pDoubleArray MatBuffer;
	pComplexArray Zvalues;
	double Factor = 0.0;
	/*# with ActiveLineCodeObj do */
	{
		auto with0 = ActiveLineCodeObj;
		MatrixChanged = true;
		MatBuffer = new double[with0->Fnphases * with0->Fnphases];
		OrderFound = Parser[ActorID]->ParseAsSymMatrix(with0->Fnphases, MatBuffer);
		if(OrderFound > 0)
			switch(i)
			{    // Parse was successful
				case 	1:    /*R*/
				{
					Zvalues = with0->Z->GetValuesArrayPtr(Norder);
					if(Norder == with0->Fnphases)
					{
						int stop = 0;
						for(stop = with0->Fnphases * with0->Fnphases, j = 1; j <= stop; j++)
						{
							(Zvalues)[j - 1].re = (MatBuffer)[j - 1];
						}
					}
				}
				break;   /*X*/
				case 	2:
				{
					Zvalues = with0->Z->GetValuesArrayPtr(Norder);
					if(Norder == with0->Fnphases)
					{
						int stop = 0;
						for(stop = with0->Fnphases * with0->Fnphases, j = 1; j <= stop; j++)
						{
							(Zvalues)[j - 1].im = (MatBuffer)[j - 1];
						}
					}
				}
				break;    /*YC Matrix*/
				case 	3:
				{
					Factor = DSSGlobals::TwoPi * with0->BaseFrequency * 1.0e-9;
					Zvalues = with0->YC->GetValuesArrayPtr(Norder);
					if(Norder == with0->Fnphases)
					{
						int stop = 0;
						for(stop = with0->Fnphases * with0->Fnphases, j = 1; j <= stop; j++)
						{
							(Zvalues)[j - 1].im = Factor * (MatBuffer)[j - 1];
						}
					}
				}
				break;
				default:
				  ;
				break;
			}
		delete[] MatBuffer; //# free accepts one parameter only;
	}
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineCode::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	result = 0;
  // continue parsing with contents of Parser
	ActiveLineCodeObj = ((TLineCodeObj*) ElementList.Get_Active());
	ActiveDSSObject[ActorID] = ActiveLineCodeObj;
	SymComponentsChanged = false;
	MatrixChanged = false;
	ActiveLineCodeObj->ReduceByKron = false;  // Allow all matrices to be computed it raw form
	/*# with ActiveLineCodeObj do */
	{
		auto with0 = ActiveLineCodeObj;
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
				with0->Set_NPhases(Parser[ActorID]->MakeInteger_());
				break;  // Use property value to force reallocations
				case 	2:
				SetZ1Z0(1, Parser[ActorID]->MakeDouble_());
				break;  /*R1*/
				case 	3:
				SetZ1Z0(2, Parser[ActorID]->MakeDouble_());
				break;  /*X0*/
				case 	4:
				SetZ1Z0(3, Parser[ActorID]->MakeDouble_());
				break;  /*R1*/
				case 	5:
				SetZ1Z0(4, Parser[ActorID]->MakeDouble_());
				break;  /*X0*/
				case 	6:
				SetZ1Z0(5, Parser[ActorID]->MakeDouble_() * 1.0e-9);
				break; /*C1*/   // Convert from nano to farads
				case 	7:
				SetZ1Z0(6, Parser[ActorID]->MakeDouble_() * 1.0e-9);
				break; /*C0*/
				case 	8:
				SetUnits(Param);
				break; /*Rmatrix*/
				case 	9:
				DoMatrix(1, ActorID);
				break; /*Xmatrix*/
				case 	10:
				DoMatrix(2, ActorID);
				break; /*Cmatrix*/
				case 	11:
				DoMatrix(3, ActorID);
				break;
				case 	12:
				with0->BaseFrequency = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->NormAmps = Parser[ActorID]->MakeDouble_();
				break;
				case 	14:
				with0->EmergAmps = Parser[ActorID]->MakeDouble_();
				break;
				case 	15:
				with0->FaultRate = Parser[ActorID]->MakeDouble_();
				break;
				case 	16:
				with0->PctPerm = Parser[ActorID]->MakeDouble_();
				break;
				case 	17:
				with0->HrsToRepair = Parser[ActorID]->MakeDouble_();
				break;
				case 	18:
				with0->ReduceByKron = InterpretYesNo(Param);
				break;
				case 	19:
				with0->Rg = Parser[ActorID]->MakeDouble_();
				break;
				case 	20:
				with0->Xg = Parser[ActorID]->MakeDouble_();
				break;
				case 	21:
				with0->rho = Parser[ActorID]->MakeDouble_();
				break;
				case 	22:
				with0->FNeutralConductor = Parser[ActorID]->MakeInteger_();
				break;
				case 	23:
				SetZ1Z0(5, Parser[ActorID]->MakeDouble_() / (DSSGlobals::TwoPi * with0->BaseFrequency) * 1.0e-6);
				break; /*B1 -> C1*/
				case 	24:
				SetZ1Z0(6, Parser[ActorID]->MakeDouble_() / (DSSGlobals::TwoPi * with0->BaseFrequency) * 1.0e-6);
				break; /*B0 -> C0*/
				case 	25:
				{
					with0->NumAmpRatings = Parser[ActorID]->MakeInteger_();
					with0->AmpRatings.resize(with0->NumAmpRatings);
				}
				break;
				case 	26:
				{
					with0->AmpRatings.resize(with0->NumAmpRatings);
					Param = Parser[ActiveActor]->MakeString_();
					with0->NumAmpRatings = InterpretDblArray(Param, with0->NumAmpRatings, ((pDoubleArray) &(with0->AmpRatings[0])));
				}
				break;
				case 	27:
				with0->FLineType = LineTypeList.Getcommand(Param);
				break;
				default:
				ClassEdit(ActiveLineCodeObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 9: case 10: case 11:
				with0->SymComponentsModel = false;
				break;
				case 	18:
				if(with0->ReduceByKron && !with0->SymComponentsModel)
					with0->DoKronReduction();
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		if(with0->SymComponentsModel)
			with0->CalcMatricesFromZ1Z0();
		if(MatrixChanged)
		{
			with0->Zinv->CopyFrom(with0->Z);
			with0->Zinv->Invert();
		}
	}
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineCode::MakeLike(const String LineName)
{
	int result = 0;
	TLineCodeObj* OtherLineCode = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this line code in the present collection*/
	OtherLineCode = ((TLineCodeObj*) Find(LineName));
	if(OtherLineCode != nullptr)
		/*# with ActiveLineCodeObj do */
		{
			auto with0 = ActiveLineCodeObj;
			int stop = 0;
			if(with0->Fnphases != OtherLineCode->Fnphases)
			{
				with0->Fnphases = OtherLineCode->Fnphases;
				if(with0->Z != nullptr)
					delete with0->Z;
				if(with0->Zinv != nullptr)
					delete with0->Zinv;
				if(with0->YC != nullptr)
					delete with0->YC;
				with0->Z = new TcMatrix(with0->Fnphases);
				with0->Zinv = new TcMatrix(with0->Fnphases);
				with0->YC = new TcMatrix(with0->Fnphases);
			}
			with0->Z->CopyFrom(OtherLineCode->Z);
			with0->Zinv->CopyFrom(OtherLineCode->Zinv);
			with0->YC->CopyFrom(OtherLineCode->YC);
			with0->BaseFrequency = OtherLineCode->BaseFrequency;
			with0->R1 = OtherLineCode->R1;
			with0->X1 = OtherLineCode->X1;
			with0->R0 = OtherLineCode->R0;
			with0->X0 = OtherLineCode->X0;
			with0->C1 = OtherLineCode->C1;
			with0->C0 = OtherLineCode->C0;
			with0->Rg = OtherLineCode->Rg;
			with0->Xg = OtherLineCode->Xg;
			with0->rho = OtherLineCode->rho;
			with0->FNeutralConductor = OtherLineCode->FNeutralConductor;
			with0->NormAmps = OtherLineCode->NormAmps;
			with0->EmergAmps = OtherLineCode->EmergAmps;
			with0->FaultRate = OtherLineCode->FaultRate;
			with0->PctPerm = OtherLineCode->PctPerm;
			with0->HrsToRepair = OtherLineCode->HrsToRepair;
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherLineCode->Get_PropertyValue(i));
			}
			result = 1;
		}
	else
		DoSimpleMsg(String("Error in Line MakeLike: \"") + LineName + "\" Not Found.", 102);
	return result;
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

int TLineCode::Init(int Handle, int ActorID)
{
	int result = 0;
	DoSimpleMsg("Need to implement TLineCode.Init", -1);
	result = 0;
	return result;
}  // Returns active line code string

String TLineCode::Get_Code()
{
	String result;
	result = ((TLineCodeObj*) ElementList.Get_Active())->get_Name();
	return result;
}  // sets the  active linecode

void TLineCode::Set_Code(const String Value)
{
	TLineCodeObj* LineCodeObj = nullptr;
	ActiveLineCodeObj = nullptr;
	LineCodeObj = ((TLineCodeObj*) ElementList.Get_First());
	while(LineCodeObj != nullptr)
	{
		if(CompareText(LineCodeObj->get_Name(), Value) == 0)
		{
			ActiveLineCodeObj = LineCodeObj;
			return;
		}
		LineCodeObj = ((TLineCodeObj*) ElementList.Get_Next());
	}
	DoSimpleMsg(String("Linecode: \"") + Value + "\" not Found.", 103);
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineCode Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineCodeObj::TLineCodeObj(TDSSClass* ParClass, const String LineCodeName)
 : inherited(ParClass),
			SymComponentsModel(false),
			ReduceByKron(false),
			Z(nullptr),
			Zinv(nullptr),
			YC(nullptr),
			BaseFrequency(0.0),
			R1(0.0),
			X1(0.0),
			R0(0.0),
			X0(0.0),
			C1(0.0),
			C0(0.0),
			FaultRate(0.0),
			PctPerm(0.0),
			HrsToRepair(0.0),
			Rg(0.0),
			Xg(0.0),
			rho(0.0),
			FLineType(0),
			Units(0)
{
	Set_Name(LowerCase(LineCodeName));
	DSSObjType = ParClass->DSSClassType;
	Fnphases = 3;  // Directly set conds and phases
	FNeutralConductor = Fnphases;  // initialize to last conductor
	R1 = 0.0580;  //ohms per 1000 ft
	X1 = 0.1206;
	R0 = 0.1784;
	X0 = 0.4047;
	C1 = 3.4E-9;  // nf per 1000ft
	C0 = 1.6E-9;
	Z = nullptr;
	Zinv = nullptr;
	YC = nullptr;
	BaseFrequency = ActiveCircuit[ActiveActor]->Fundamental;
	Units = UNITS_NONE;  // default to none  (no conversion)
	NormAmps = 400.0;
	EmergAmps = 600.0;
	PctPerm = 20.0;
	FaultRate = 0.1;
	FLineType = 1;  // Default to OH  Line
	Rg = 0.01805;  // ohms per 1000'
	Xg = 0.155081;
	rho = 100.0;
	SymComponentsModel = true;
	ReduceByKron = false;
	CalcMatricesFromZ1Z0();  // put some reasonable values in

    /*Initialize dynamic array for ratings*/
	NumAmpRatings = 1;
	AmpRatings.resize(NumAmpRatings);
	AmpRatings[0] = NormAmps;
	InitPropertyValues(0);
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

TLineCodeObj::~TLineCodeObj()
{
	delete Z;
	delete Zinv;
	delete YC;
	// inherited::Destroy();
}


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

// Set the number of phases and reallocate phase-sensitive arrays
// Need to preserve values in Z matrices

void TLineCodeObj::Set_NPhases(int Value)
{
	if(Value > 0)
	{
		if(Fnphases != Value)    // If size is no different, we don't need to do anything
		{
			Fnphases = Value;
			FNeutralConductor = Fnphases;  // Init to last conductor
        // Put some reasonable values in these matrices
			CalcMatricesFromZ1Z0();  // reallocs matrices
		}
	}
}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

void TLineCodeObj::CalcMatricesFromZ1Z0()
{
	complex Zs = {};
	complex ZM = {};
	complex Ys = {};
	complex YM = {};
	complex Ztemp = {};
	int i = 0;
	int j = 0;
	double Yc1 = 0.0;
	double Yc0 = 0.0;
	double OneThird = 0.0;
	int stop = 0;
	if(Z != nullptr)
		delete Z;
	if(Zinv != nullptr)
		delete Zinv;
	if(YC != nullptr)
		delete YC;

    // For a line, nphases = ncond, for now
	Z = new TcMatrix(Fnphases);
	Zinv = new TcMatrix(Fnphases);
	YC = new TcMatrix(Fnphases);
	OneThird = 1.0 / 3.0;  // Do this to get more precision in next few statements
	Ztemp = cmulreal(cmplx(R1, X1), 2.0);
	Zs = cmulreal(cadd(Ztemp, cmplx(R0, X0)), OneThird);
	ZM = cmulreal(csub(cmplx(R0, X0), cmplx(R1, X1)), OneThird);
	Yc1 = DSSGlobals::TwoPi * BaseFrequency * C1;
	Yc0 = DSSGlobals::TwoPi * BaseFrequency * C0;
	Ys = cmulreal(cadd(cmulreal(cmplx(0.0, Yc1), 2.0), cmplx(0.0, Yc0)), OneThird);
	YM = cmulreal(csub(cmplx(0.0, Yc0), cmplx(0.0, Yc1)), OneThird);
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		int stop1 = 0;
		Z->SetElement(i, i, Zs);
		YC->SetElement(i, i, Ys);
		for(stop1 = i - 1, j = 1; j <= stop1; j++)
		{
			Z->SetElemsym(i, j, ZM);
			YC->SetElemsym(i, j, YM);
		}
	}
	Zinv->CopyFrom(Z);
	Zinv->Invert();
}

void TLineCodeObj::DumpProperties(System::TTextRec& f, bool Complete)
{
	int k = 0;
	int i = 0;
	int j = 0;
	char Buff[1000];
	String TempStr;
	inherited::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[1 - 1]); System::Write(f, "="); System::WriteLn(f, Fnphases, 0); }
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[2 - 1]); System::Write(f, "="); System::WriteLn(f, R1, 0, 5); }
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[3 - 1]); System::Write(f, "="); System::WriteLn(f, X1, 0, 5); }
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[4 - 1]); System::Write(f, "="); System::WriteLn(f, R0, 0, 5); }
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[5 - 1]); System::Write(f, "="); System::WriteLn(f, X0, 0, 5); }
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[6 - 1]); System::Write(f, "="); System::WriteLn(f, C1 * 1.0E9, 0, 5); }
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[7 - 1]); System::Write(f, "="); System::WriteLn(f, C0 * 1.0E9, 0, 5); }
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[8 - 1]); System::Write(f, "="); System::WriteLn(f, Get_PropertyValue(8)); }
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[9 - 1]); System::Write(f, "="); System::Write(f, L'\"'); }
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = Fnphases, j = 1; j <= stop1; j++)
			{
				{ System::Write(f, Z->GetElement(i, j).re, 0, 8); System::Write(f, ""); }
			}
			System::Write(f, L'|');
		}
		System::WriteLn(f, L'\"');
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[10 - 1]); System::Write(f, "="); Write(f, L'\"'); }
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = Fnphases, j = 1; j <= stop1; j++)
			{
				{ System::Write(f, Z->GetElement(i, j).im, 0, 8); System::Write(f, ""); }
			}
			Write(f, L'|');
		}
		System::WriteLn(f, L'\"');
		{ System::Write(f, "~ "); System::Write(f, with0->PropertyName[11 - 1]); System::Write(f, "="); Write(f, L'\"'); }
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			int stop1 = 0;
			for(stop1 = Fnphases, j = 1; j <= stop1; j++)
			{
				{ System::Write(f, (double(YC->GetElement(i, j).im) / DSSGlobals::TwoPi / BaseFrequency * 1.0E9), 0, 8); Write(f, ""); }
			}
			System::Write(f, L'|');
		}
		System::WriteLn(f, L'\"');
		for(stop = 21, i = 12; i <= stop; i++)
		{
			{ System::Write(f, "~ "); Write(f, (with0->PropertyName)[i - 1]); System::Write(f, "="); System::WriteLn(f, Get_PropertyValue(i)); }
		}
		sprintf(Buff, "~ %s=%d", with0->PropertyName[22 - 1].c_str(), FNeutralConductor);
		System::WriteLn(f, Buff);
		sprintf(Buff, "~ %s=%d", with0->PropertyName[25 - 1].c_str(), NumAmpRatings);
		System::WriteLn(f, Buff);
		TempStr = "[";
		for(stop = NumAmpRatings, k = 1; k <= stop; k++)
		{
			TempStr = TempStr + FloatToStrF(AmpRatings[k - 1], ffGeneral, 8, 4) + ",";
		}
		TempStr = TempStr + "]";
		sprintf(Buff, "~ %s=%s", with0->PropertyName[26 - 1].c_str(), TempStr.c_str());
		System::WriteLn(f, Buff);
	}
}

String TLineCodeObj::GetPropertyValue(int Index)
{
	String result;
	int j = 0;
	char Buff[1000];
	switch(Index)
	{
		case 	1:
		{
			sprintf(Buff, "%d", Fnphases);
			result = Buff;
		}
		break;
		case 	2:
			if (SymComponentsModel)
			{
				sprintf(Buff, "%.5g", R1);
				result = Buff;
			}
			else
				result = "----";
		break;
		case 	3:
			if (SymComponentsModel)
			{
				sprintf(Buff, "%.5g", X1);
				result = Buff;
			}
			else
				result = "----";
		break;
		case 	4:
			if (SymComponentsModel)
			{	
				sprintf(Buff, "%.5g", R0);
				result = Buff;
			}
			else
				result = "----";
		break;
		case 	5:
			if (SymComponentsModel)
			{
				sprintf(Buff, "%.5g", X0);
				result = Buff;
			}
			else
				result = "----";
		break;
		case 	6:
			if (SymComponentsModel)
			{
				sprintf(Buff, "%.5g", C1 * 1.0E9);
				result = Buff;
			}
			else
				result = "----";
		break;
		case 	7:
			if (SymComponentsModel)
			{
				sprintf(Buff, "%.5g", C0 * 1.0E9);
				result = Buff;
			}
			else
				result = "----";
		break;
		case 	8:
			result = LineUnitsStr(Units);
		break;
		case 	9:
			result = get_Rmatrix();
		break;
		case 	10:
			result = get_Xmatrix();
		break;
		case 	11:
			result = get_CMatrix();
		break;
		case 	12:
		{
			sprintf(Buff, "%.g", BaseFrequency);
			result = Buff;
		}
		break; //  was defaultbasefrequency ??? 'baseFreq';
		case 	18:
		if(ReduceByKron)
			result = "Y";
		else
			result = "N";
		break;
		case 	19:
		{
			sprintf(Buff, "%.5g", Rg);
			result = Buff;
		}
		break;
		case 	20:
		{
			sprintf(Buff, "%.5g", Xg);
			result = Buff;
		}
		break;
		case 	21:
		{
			sprintf(Buff, "%.5g", rho);
			result = Buff;
		}
		break;
		case 	22:
			result = IntToStr(FNeutralConductor);
		break;
		case 	23:
			if (SymComponentsModel)
			{
				sprintf(Buff, "%.5g", DSSGlobals::TwoPi * BaseFrequency * C1 * 1.0e6);
				result = Buff;
			}
			else
				result = "----";
		break;
		case 	24:
			if (SymComponentsModel)
			{
				sprintf(Buff, "%.5g", DSSGlobals::TwoPi * BaseFrequency * C0 * 1.0e6);
				result = Buff;
			}
			else
				result = "----";
		break;
		case 	25:
			result = IntToStr(NumAmpRatings);
		break;
		case 	26:
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
		case 	27:
			result = LineTypeList.Get(FLineType);
			break;
		default:
			result = inherited::GetPropertyValue(Index);
			break;
	}
	return result;
}

void TLineCodeObj::InitPropertyValues(int ArrayOffset)
{
	char Buff[300];

	Set_PropertyValue(1,"3"); // 'nphases';
	Set_PropertyValue(2,".058"); // 'r1';
	Set_PropertyValue(3,".1206"); // 'x1';
	Set_PropertyValue(4,"0.1784"); // 'r0';
	Set_PropertyValue(5,"0.4047"); // 'x0';
	Set_PropertyValue(6,"3.4"); // 'c1';
	Set_PropertyValue(7,"1.6"); // 'c0';
	Set_PropertyValue(8,"none"); // 'units';
	Set_PropertyValue(9,""); // 'rmatrix';
	Set_PropertyValue(10,""); // 'xmatrix';
	Set_PropertyValue(11,""); // 'cmatrix';
	sprintf(Buff, "%6.1f", DefaultBaseFreq);
	Set_PropertyValue(12,Buff); // 'baseFreq';
	Set_PropertyValue(13,"400"); // 'normamps';
	Set_PropertyValue(14,"600"); // 'emergamps';
	Set_PropertyValue(15,"0.1"); // 'faultrate';
	Set_PropertyValue(16,"20"); // 'pctperm';
	Set_PropertyValue(17,"3"); // 'Hrs to repair';
	Set_PropertyValue(18,"N"); // 'Kron';
	Set_PropertyValue(19,".01805"); // 'Rg';
	Set_PropertyValue(20,".155081"); // 'Xg';
	Set_PropertyValue(21,"100"); // 'rho';
	Set_PropertyValue(22,"3"); // 'Neutral';
	Set_PropertyValue(23,"1.2818"); // B1  microS
	Set_PropertyValue(24,"0.60319"); // B0  microS
	Set_PropertyValue(25,"1"); // 1 season
	Set_PropertyValue(26,"[400]"); // 1 rating
	Set_PropertyValue(27,"OH"); // 1 rating
	inherited::InitPropertyValues(NumPropsThisClass);
}

//----------------------------------------------------------------------------------------------------------------

int TLineCodeObj::get_Fnphases()
{
	return Fnphases;
}

//----------------------------------------------------------------------------------------------------------------

void TLineCodeObj::DoKronReduction()
{
	TcMatrix* NewZ = nullptr;
	TcMatrix* NewYC = nullptr;
	if(FNeutralConductor == 0)
		return;   // Do Nothing
	NewZ = nullptr;
	NewYC = nullptr;
	if(Fnphases > 1)
	{
		try
		{
			NewZ = Z->Kron(FNeutralConductor);       // Perform Kron Reductions into temp space
        /* Have to invert the Y matrix to eliminate properly*/
			YC->Invert();  // Vn = 0 not In
			NewYC = YC->Kron(FNeutralConductor);
		}
		catch (...)
		{
			DoSimpleMsg(Format("Kron Reduction failed: LineCode.%s. Attempting to eliminate Neutral Conductor %d.", get_Name().c_str(), FNeutralConductor), 103);
		}

        // Reallocate into smaller space   if Kron was successful
		if((NewZ != nullptr) && (NewYC != nullptr))
		{
			NewYC->Invert();  // Back to Y
			Set_NPhases(NewZ->get_Norder());

            // Get rid of Z and YC and replace
			delete Z;
			delete YC;
			Z = NewZ;
			YC = NewYC;
			FNeutralConductor = 0;
			ReduceByKron = false;

            /*Change Property values to reflect Kron reduction for save circuit function*/
			Set_PropertyValue(1,Format("%d", Fnphases));
			Set_PropertyValue(9,get_Rmatrix());
			Set_PropertyValue(10,get_Xmatrix());
			Set_PropertyValue(11,get_CMatrix());
		}
		else
		{
			DoSimpleMsg(Format("Kron Reduction failed: LineCode.%s. Attempting to eliminate Neutral Conductor %d.", get_Name().c_str(), FNeutralConductor), 103);
		}
	}
	else
	{
		DoSimpleMsg(String("Cannot perform Kron Reduction on a 1-phase LineCode: LineCode.") + get_Name(), 103);
	}
}




}  // namespace LineCode





