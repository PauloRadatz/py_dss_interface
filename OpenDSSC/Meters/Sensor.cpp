
#pragma hdrstop

#include "Sensor.h"

#include "ParserDel.h"
#include "DSSClassDefs.h"
#include "DSSGlobals.h"
#include "Circuit.h"
#include "CktElement.h"
#include "Transformer.h"
#include "PCElement.h"
#include "PDElement.h"
#include "Ucmatrix.h"
#include "ShowResults.h"
#include "mathutil.h"
#include "PointerList.h"
#include "Dynamics.h"
#include "Utilities.h"

using namespace std;
using namespace Arraydef;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace Dynamics;
using namespace MeterClass;
using namespace MeterElement;
using namespace PCElement;
using namespace PDELement;
using namespace ParserDel;
using namespace PointerList;
using namespace System;
using namespace Transformer;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;

namespace Sensor
{

TSensorObj::TSensorObj(DSSClass::TDSSClass* ParClass) : inherited(ParClass->get_myClass_name()) {}
TSensorObj::TSensorObj(String ClassName) : inherited(ClassName) {}
TSensorObj::TSensorObj() {}


TSensorObj* ActiveSensorObj = nullptr; /*TOPExport,*/
const int NumPropsThisClass = 13;

/*==============================================================================*/  // Creates superstructure for all Sensor objects

TSensor::TSensor()
{
	;
	Class_Name = "Sensor";
	DSSClassType = DSSClassType + SENSOR_ELEMENT;
	DefineProperties();
	std::string* slc = Slice((PropertyName), NumProperties);
	CommandList = TCommandList(slc, NumProperties);
	delete[] slc;
	CommandList.set_AbbrevAllowed(true);
}

/*==============================================================================*/

TSensor::~TSensor()
{
	// inherited::Destroy();
}


/*==============================================================================*/

void TSensor::DefineProperties()
{
	NumProperties = NumPropsThisClass;
	CountProperties();   // Get inherited property count
	AllocatePropertyArrays();


     // Define Property names
	PropertyName[1 - 1] = "element";
	PropertyName[2 - 1] = "terminal";
	PropertyName[3 - 1] = "kvbase";
	PropertyName[4 - 1] = "clear";
	PropertyName[5 - 1] = "kVs";
	PropertyName[6 - 1] = "currents";
	PropertyName[7 - 1] = "kWs";
	PropertyName[8 - 1] = "kvars";
	PropertyName[9 - 1] = "conn";  //  Sensor connection
	PropertyName[10 - 1] = "Deltadirection";  //  +/- 1
	PropertyName[11 - 1] = "%Error";  //  %Error of sensor
	PropertyName[12 - 1] = "Weight";  // for WLS calc
	PropertyName[13 - 1] = "action";
	PropertyHelp[1 - 1] = "Name (Full Object name) of element to which the Sensor is connected.";
	PropertyHelp[2 - 1] = "Number of the terminal of the circuit element to which the Sensor is connected. "
	           "1 or 2, typically. Default is 1.";
	PropertyHelp[3 - 1] = String("Voltage base for the sensor, in kV. If connected to a 2- or 3-phase terminal, ") + CRLF
	           + "specify L-L voltage. For 1-phase devices specify L-N or actual 1-phase voltage. "
	           + "Like many other DSS devices, default is 12.47kV.";
	PropertyHelp[4 - 1] = "{ Yes | No }. Clear=Yes clears sensor values. Should be issued before putting in a new set of measurements.";
	PropertyHelp[5 - 1] = "Array of Voltages (kV) measured by the voltage sensor. For Delta-connected "
	           "sensors, Line-Line voltages are expected. For Wye, Line-Neutral are expected.";
	PropertyHelp[6 - 1] = "Array of Currents (amps) measured by the current sensor. Specify this or power quantities; not both.";
	PropertyHelp[7 - 1] = String("Array of Active power (kW) measurements at the sensor. Is converted into Currents along with q=[...]") + CRLF
	           + "Will override any currents=[...] specification.";
	PropertyHelp[8 - 1] = "Array of Reactive power (kvar) measurements at the sensor. Is converted into Currents along with p=[...]";
	PropertyHelp[9 - 1] = String("Voltage sensor Connection: { wye | delta | LN | LL }.  Default is wye. Applies to voltage measurement only. ") + CRLF
	           + "Currents are always assumed to be line currents."
	           + CRLF
	           + "If wye or LN, voltage is assumed measured line-neutral; otherwise, line-line.";
	PropertyHelp[10 - 1] = "{1 or -1}  Default is 1:  1-2, 2-3, 3-1.  For reverse rotation, enter -1. Any positive or negative entry will suffice.";
	PropertyHelp[11 - 1] = "Assumed percent error in the measurement. Default is 1.";
	PropertyHelp[12 - 1] = "Weighting factor: Default is 1.";
	PropertyHelp[13 - 1] = String("NOT IMPLEMENTED.Action options: ") + CRLF
	           + "SQERROR: Show square error of the present value of the monitored terminal  "
	           + CRLF
	           + "quantity vs the sensor value. Actual values - convert to per unit in calling program.  "
	           + CRLF
	           + "Value reported in result window/result variable.";
	ActiveProperty = NumPropsThisClass - 1;
	inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
}

/*==============================================================================*/

int TSensor::NewObject(const String ObjName)
{
	int result = 0;
    // Make a new Sensor and add it to Sensor class list
	/*# with ActiveCircuit[ActiveActor] do */
	{
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TSensorObj(this, ObjName));


		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
	}
	return result;
}

/*==============================================================================*/

int TSensor::Edit(int ActorID)
{
	int result = 0;
	int ParamPointer = 0;
	String ParamName;
	String Param;
	bool DoRecalcElementData = false;

  // continue parsing with contents of Parser
	ActiveSensorObj = ((TSensorObj*) ElementList.Get_Active());
	ActiveCircuit[ActorID]->Set_ActiveCktElement((TDSSCktElement*) ActiveSensorObj);
	result = 0;
	DoRecalcElementData = false;
	/*# with ActiveSensorObj do */
	{
		auto with0 = ActiveSensorObj;
		ParamPointer = 0;
		ParamName = Parser[ActorID]->GetNextParam();
		Param = Parser[ActorID]->MakeString_();
		while(Param.size() > 0)
		{
			if(ParamName.size() == 0)
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
	           + "\"", 661);
				break;
				case 	1:
				with0->ElementName = LowerCase(Param);
				break;
				case 	2:
				with0->MeteredTerminal = Parser[ActorID]->MakeInteger_();
				break;
				case 	3:
				with0->kVBase = Parser[ActorID]->MakeDouble_();
				break;
				case 	4:
				with0->ClearSpecified = InterpretYesNo(Param);
				break;
				case 	5:
				Parser[ActorID]->ParseAsVector( ( (TDSSCktElement*) with0 )->Fnphases, &(with0->SensorVoltage[0]));
				break;  // Inits to zero
				case 	6:
				Parser[ActorID]->ParseAsVector(((TDSSCktElement*)with0)->Fnphases, &(with0->SensorCurrent[0]));
				break;  // Inits to zero
				case 	7:
				{
					Parser[ActorID]->ParseAsVector(((TDSSCktElement*)with0)->Fnphases, with0->SensorkW);
					with0->Pspecified = true;
					with0->UpdateCurrentVector();
				}
				break;
				case 	8:
				{
					Parser[ActorID]->ParseAsVector(((TDSSCktElement*)with0)->Fnphases, with0->Sensorkvar);
					with0->Qspecified = true;
					with0->UpdateCurrentVector();
				}
				break;
				case 	9:
				with0->Set_Conn(InterpretConnection(Param));
				break;
				case 	10:
				with0->FDeltaDirection = with0->LimitToPlusMinusOne(Parser[ActorID]->MakeInteger_());
				break;
				case 	11:
				with0->pctError = Parser[ActorID]->MakeDouble_();
				break;
				case 	12:
				with0->Weight = Parser[ActorID]->MakeDouble_();
				break;
				case 	13:
				with0->Set_Action(Param);
				break;  // Put sq error in Global Result

           // Inherited parameters
				default:
				inherited::ClassEdit(ActiveSensorObj, ParamPointer - NumPropsThisClass);
				break;
			}
			switch(ParamPointer)
			{
				case 1: case 2:
				{
					DoRecalcElementData = true;
					with0->MeteredElementChanged = true;
				}
				break;
				case 	3:
				DoRecalcElementData = true;
				break;

              /*Do not recalc element data for setting of sensor quantities*/
				case 	4:
				if(with0->ClearSpecified)
					with0->ClearSensor();
				break;
				case 	5:
				with0->Vspecified = true;
				break;
				case 	6:
				with0->Ispecified = true;
				break;
				case 	7:
				with0->Pspecified = true;
				break;
				case 	8:
				with0->Qspecified = true;
				break;
				case 	9:
				DoRecalcElementData = true;
				break;
				case 	10:
				DoRecalcElementData = true;
				break;
				default:
				  ;
				break;
			}
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
		}
		if(DoRecalcElementData)
			with0->RecalcElementData(ActorID);
	}
	return result;
}

/*==============================================================================*/  // Force all Sensors in the circuit to reset

void TSensor::ResetAll(int ActorID)
{
	TSensorObj* pSensor = nullptr;
	pSensor = ((TSensorObj*) ActiveCircuit[ActorID]->Sensors.Get_First());
	while(pSensor != nullptr)
	{
		if( ( (TDSSCktElement*) pSensor )->Get_Enabled())
			pSensor->ResetIt();
		pSensor = ((TSensorObj*) ActiveCircuit[ActiveActor]->Sensors.Get_Next());
	}
}

/*==============================================================================*/  // Force all Sensors in the circuit to take a sample

void TSensor::SampleAll(int ActorID)
{
	TSensorObj* pSensor = nullptr;
	pSensor = ((TSensorObj*) ActiveCircuit[ActorID]->Sensors.Get_First());
	while(pSensor != nullptr)
	{
		if(((TDSSCktElement*)pSensor)->Get_Enabled())
			pSensor->TakeSample(ActorID);
		pSensor = ((TSensorObj*) ActiveCircuit[ActorID]->Sensors.Get_Next());
	}
}

/*==============================================================================*/     // Force all Sensors in the circuit to save their buffers to disk

//VAR
//   Mon:TSensorObj;

void TSensor::SaveAll(int ActorID)
{

/*
   Mon := ActiveCircuit[ActiveActor].Sensors.Get_First();
   WHILE Mon<>Nil DO
   Begin
       If Mon.Enabled Then Mon.Save;
       Mon := ActiveCircuit[ActiveActor].Sensors.Get_Next();
   End;
*/
}

/*==============================================================================*/
// Set the HasSensorObj Flag for all cktElement;

void TSensor::SetHasSensorFlag()
{
	int i = 0;
	TSensorObj* ThisSensor = nullptr;
	TDSSCktElement* CktElem = nullptr;
   /*Initialize all to FALSE*/
	int stop = 0;
	/*# with ActiveCircuit[ActiveActor] do */
	{
		
		CktElem = ((TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_First());
		while(CktElem != nullptr)
		{
			CktElem->HasSensorObj = false;
			CktElem = ((TDSSCktElement*) ActiveCircuit[ActiveActor]->PDElements.Get_Next());
		}  /*WHILE*/
		CktElem = ((TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_First());
		while(CktElem != nullptr)
		{
			CktElem->HasSensorObj = false;
			CktElem = ((TDSSCktElement*) ActiveCircuit[ActiveActor]->PCElements.Get_Next());
		}  /*WHILE*/
	} /*WITH*/
	for(stop = ActiveCircuit[ActiveActor]->Sensors.get_myNumList(), i = 1; i <= stop; i++)
	{
		ThisSensor = ((TSensorObj*) ActiveCircuit[ActiveActor]->Sensors.Get(i));
		/*# with ThisSensor do */
		{
			auto with1 = ThisSensor;
			if(with1->MeteredElement != nullptr)
			{
				with1->MeteredElement->HasSensorObj = true;
				if(dynamic_cast<TPCElement*> (with1->MeteredElement))
					((TPCElement*) with1->MeteredElement)->SensorObj = ThisSensor;
				else
					((TPDElement*) with1->MeteredElement)->SensorObj = ThisSensor;
			}
		}
	}   /*FOR*/
}

/*==============================================================================*/

int TSensor::MakeLike(const String SensorName)
{
	int result = 0;
	TSensorObj* OtherSensor = nullptr;
	int i = 0;
	result = 0;
   /*See if we can find this Sensor name in the present collection*/
	OtherSensor = ((TSensorObj*) Find(SensorName));
	if(OtherSensor != nullptr)
		/*# with ActiveSensorObj do */
		{
			auto with0 = ActiveSensorObj;
			int stop = 0;
			((TDSSCktElement*)with0)->Set_NPhases(((TDSSCktElement*)OtherSensor)->Fnphases);
			((TDSSCktElement*)with0)->Set_Nconds(((TDSSCktElement*)OtherSensor)->Fnconds); // Force Reallocation of terminal stuff
			with0->ElementName = OtherSensor->ElementName;
			with0->MeteredElement = OtherSensor->MeteredElement;  // Pointer to target circuit element
			with0->MeteredTerminal = OtherSensor->MeteredTerminal;
/*==========================================================================*/







/*==========================================================================*/
			for(stop = with0->ParentClass->NumProperties, i = 1; i <= stop; i++)
			{
				with0->Set_PropertyValue(i,OtherSensor->Get_PropertyValue(i));
			}
			((TDSSCktElement*)with0)->BaseFrequency = ((TDSSCktElement*)OtherSensor)->BaseFrequency;
		}
	else
		DoSimpleMsg(String("Error in Sensor MakeLike: \"") + SensorName
	           + "\" Not Found.", 662);
	return result;
}

/*==============================================================================*/

int TSensor::Init(int Handle, int ActorID)
{
	int result = 0;
	TSensorObj* pSensor = nullptr;
	result = 0;
	if(Handle > 0)
	{
		pSensor = ((TSensorObj*) ElementList.Get(Handle));
		pSensor->ResetIt();
	}
	else
  // Do 'em all
	{
		pSensor = ((TSensorObj*) ElementList.Get_First());
		while(pSensor != nullptr)
		{
			pSensor->ResetIt();
			pSensor = ((TSensorObj*) ElementList.Get_Next());
		}
	}
	return result;
}


/*==========================================================================*/
/*                    TSensorObj                                           */
/*==========================================================================*/



/*==============================================================================*/

TSensorObj::TSensorObj(TDSSClass* ParClass, const String SensorName)
 : inherited(ParClass->get_myClass_name()),
			ValidSensor(false),
			SensorkW(nullptr),
			Sensorkvar(nullptr),
			kVBase(0.0),
			VBase(0.0),
			FConn(0),
			Vspecified(false),
			Ispecified(false),
			Pspecified(false),
			Qspecified(false),
			ClearSpecified(false),
			FDeltaDirection(0),
			pctError(0.0),
			Weight(0.0)
{
	Set_Name(LowerCase(SensorName));
	Set_NPhases(3);  // Directly set conds and phases
	Fnconds = 3;
	Set_NTerms(1);  // this forces allocation of terminals and conductors
                         // in base class
	SensorkW = nullptr;
	Sensorkvar = nullptr;
	kVBase = 12.47; // default 3-phase voltage
	Weight = 1.0;
	pctError = 1.0;
	Set_Conn(0);  // Wye
	ClearSensor();
	DSSObjType = ParClass->DSSClassType; //SENSOR_ELEMENT;
	InitPropertyValues(0);

   //  RecalcElementData;
}

/*==============================================================================*/

TSensorObj::~TSensorObj()
{
	ElementName = "";
	free(SensorkW);
	free(Sensorkvar);
	// inherited::Destroy();
}


/*==============================================================================*/

void TSensorObj::RecalcElementData(int ActorID)
{
	int DevIndex = 0;
	ValidSensor = false;
	DevIndex = GetCktElementIndex(ElementName); // Global function
	if(DevIndex > 0)  // Sensored element must already exist
	{
		MeteredElement = ((TDSSCktElement*) ActiveCircuit[ActorID]->CktElements.Get(DevIndex));
		if(MeteredTerminal > MeteredElement->Get_NTerms())
		{
			DoErrorMsg(String("Sensor: \"") + get_Name() + "\"", "Terminal no. \"" "\" does not exist.", "Respecify terminal no.", 665);
		}
		else
		{
			Set_NPhases(MeteredElement->Get_NPhases());
			Set_Nconds(MeteredElement->Get_NConds());

               // Sets name of i-th terminal's connected bus in Sensor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
			SetBus(1, MeteredElement->GetBus(MeteredTerminal));
			ClearSensor();
			ValidSensor = true;
			AllocateSensorObjArrays();
			ZeroSensorArrays();
			RecalcVbase();
		}
	}
	else
	{
		MeteredElement = nullptr;   // element not found
		DoErrorMsg(String("Sensor: \"") + this->get_Name() + "\"", String("Circuit Element \"") + ElementName + "\" Not Found.", " Element must be defined previously.", 666);
	}
}

void TSensorObj::MakePosSequence(int ActorID)
{
	if(MeteredElement != nullptr)
	{
		SetBus(1, MeteredElement->GetBus(MeteredTerminal));
		Set_NPhases(MeteredElement->Get_NPhases());
		Set_Nconds(MeteredElement->Get_NConds());
		ClearSensor();
		ValidSensor = true;
		AllocateSensorObjArrays();
		ZeroSensorArrays();
		RecalcVbase();
	}
	TDSSCktElement::MakePosSequence(ActorID);
}

/*==============================================================================*/

void TSensorObj::RecalcVbase()
{
	switch(FConn)
	{
		case 	0:
		if(Fnphases == 1)
			VBase = kVBase * 1000.0;
		else
			VBase = kVBase * 1000.0 / SQRT3;
		break;
		case 	1:
		VBase = kVBase * 1000.0;
		break;
		default:
		  ;
		break;
	}
}

/*==============================================================================*/

void TSensorObj::CalcYPrim(int ActorID)
{

  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
}

/*==============================================================================*/

/*What does it mean to reset a sensor?*/

void TSensorObj::ResetIt()
{
	ClearSensor();
}

/*==============================================================================*/
// For Delta connections or Line-Line voltages

int TSensorObj::RotatePhases(int j)
{
	int result = 0;
	result = j + FDeltaDirection;

     // make sure result is within limits
	if(Fnphases > 2)
         // Assumes 2 phase delta is open delta
	{
		if(result > Fnphases)
			result = 1;
		if(result < 1)
			result = Fnphases;
	}
	else
	{
		if(result < 1)
			result = 3;    // For 2-phase delta, next phase will be 3rd phase
	}
	return result;
}

/*==============================================================================*/

void TSensorObj::TakeSample(int ActorID)
{
	int i = 0;
	if(!(ValidSensor && Get_Enabled()))
		return;
	MeteredElement->GetCurrents(&(CalculatedCurrent[0]), ActorID);
	ComputeVterminal(ActorID);
	switch(FConn)
	{
		case 	1:
		for(int stop = Fnphases, i = 1; i <= stop; i++)
		{
			(CalculatedVoltage)[i - 1] = csub((Vterminal)[i - 1], (Vterminal)[RotatePhases(i) - 1]);
		}
		break;
		default:
		int stop = 0;
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			(CalculatedVoltage)[i - 1] = (Vterminal)[i - 1];
		}
		break;
	}

   /*NOTE: CalculatedVoltage is complex*/
}

/*==============================================================================*/  //Get present value of terminal Curr for reports

void TSensorObj::GetCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
/*
  Return array of zero
*/
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}

/*==============================================================================*/

int TSensorObj::get_FConn()
{
	return FConn;
}

/*==============================================================================*/

int TSensorObj::get_FDeltaDirection()
{
	return FDeltaDirection;
}

/*==============================================================================*/

double TSensorObj::get_kVBase()
{
	return kVBase;
}

/*==============================================================================*/

pDoubleArray TSensorObj::get_SensorkW()
{
	return SensorkW;
}

/*==============================================================================*/

pDoubleArray TSensorObj::get_Sensorkvar()
{
	return Sensorkvar;
}

/*==============================================================================*/

void TSensorObj::GetInjCurrents(pComplexArray Curr, int ActorID)
{
	int i = 0;
	int stop = 0;
	for(stop = Fnconds, i = 1; i <= stop; i++)
	{
		(Curr)[i - 1] = CZero;
	}
}
/*Updates the currentvector when P and Q are defined
 as the input vectors for the sensor*/

void TSensorObj::UpdateCurrentVector()
{
	double kVA = 0.0;
	int i = 0;
/*Convert P and Q specification to Currents*/
	if(Pspecified)    // compute currents assuming vbase
	{
		if(Qspecified)
		{
			int stop = 0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				kVA = cabs(cmplx((SensorkW)[i - 1], (Sensorkvar)[i - 1]));
				(SensorCurrent)[i - 1] = kVA * 1000.0 / VBase;
			}
		}
		else
    // No Q just use P
		{
			int stop = 0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				(SensorCurrent)[i - 1] = (SensorkW)[i - 1] * 1000.0 / VBase;
			}
		}
		Ispecified = true;    // Overrides current specification
	}
}
/*
  Return the WLS Error for Currents
  Get Square error and weight it

*/

double TSensorObj::Get_WLSCurrentError()
{
	double result = 0.0;
	double kVA = 0.0;
	int i = 0;
	result = 0.0;
/*Convert P and Q specification to Currents*/
	if(Pspecified)    // compute currents assuming vbase
	{
		if(Qspecified)
		{
			int stop = 0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				kVA = cabs(cmplx((SensorkW)[i - 1], (Sensorkvar)[i - 1]));
				(SensorCurrent)[i - 1] = kVA * 1000.0 / VBase;
			}
		}
		else
    // No Q just use P
		{
			int stop = 0;
			for(stop = Fnphases, i = 1; i <= stop; i++)
			{
				(SensorCurrent)[i - 1] = (SensorkW)[i - 1] * 1000.0 / VBase;
			}
		}
		Ispecified = true;    // Overrides current specification
	}
	if(Ispecified)
	{
		int stop = 0;
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			result = result + Sqr((CalculatedCurrent)[i - 1].re) + Sqr((CalculatedCurrent)[i - 1].im) - Sqr((SensorCurrent)[i - 1]);
		}
	}
	result = result * Weight;
	return result;
}

/*==============================================================================*/
// Get Square error and weight it

double TSensorObj::Get_WLSVoltageError()
{
	double result = 0.0;
	int i = 0;
	result = 0.0;
	if(Vspecified)
	{
		int stop = 0;
		for(stop = Fnphases, i = 1; i <= stop; i++)
		{
			result = result + Sqr((CalculatedVoltage)[i - 1].re) + Sqr((CalculatedVoltage)[i - 1].im) - Sqr((SensorVoltage)[i - 1]);
		}
	}
	result = result * Weight;
	return result;
}

/*==============================================================================*/

void TSensorObj::DumpProperties(TTextRec& f, bool Complete)
{
	int i = 0;
	TDSSCktElement::DumpProperties(f, Complete);
	/*# with ParentClass do */
	{
		auto with0 = ParentClass;
		int stop = 0;
		for(stop = with0->NumProperties, i = 1; i <= stop; i++)
		{
			{ Write(f, "~ "); Write(f, with0->PropertyName[i - 1]); Write(f, L'='); WriteLn(f, Get_PropertyValue(i)); }
		}
	}
	if(Complete)
	{
		WriteLn(f);
	}
}

/*==============================================================================*/

void TSensorObj::ClearSensor()
{
	Vspecified = false;
	Ispecified = false;
	Pspecified = false;
	Qspecified = false;
	ClearSpecified = false;
}

/*==============================================================================*/

void TSensorObj::AllocateSensorObjArrays()
{
	SensorkW = new double[Fnphases];
	Sensorkvar = new double[Fnphases];
	AllocateSensorArrays();
}

/*==============================================================================*/

void TSensorObj::ZeroSensorArrays()
{
	int i = 0;
	int stop = 0;
	for(stop = Fnphases, i = 1; i <= stop; i++)
	{
		(SensorCurrent)[i - 1] = 0.0;
		(SensorVoltage)[i - 1] = 0.0;
		(SensorkW)[i - 1] = 0.0;
		(Sensorkvar)[i - 1] = 0.0;
	}
}

/*==============================================================================*/

void TSensorObj::InitPropertyValues(int ArrayOffset)
{
	Set_PropertyValue(1,""); //'element'
	Set_PropertyValue(2,"1"); //'terminal'
	Set_PropertyValue(3,"12.47"); //'kVBase'
	Set_PropertyValue(4,"No"); // Must be set to yes to clear before setting quantities
	Set_PropertyValue(5,"[7.2, 7.2, 7.2]");
	Set_PropertyValue(6,"[0.0, 0.0, 0.0]");  // currents
	Set_PropertyValue(7,"[0.0, 0.0, 0.0]");  // P kW
	Set_PropertyValue(8,"[0.0, 0.0, 0.0]");  // Q kvar
	Set_PropertyValue(9,"wye");
	Set_PropertyValue(10,"1");
	Set_PropertyValue(11,"1");  // %Error
	Set_PropertyValue(12,"1");  // %Error
	Set_PropertyValue(13,"");   // Action
	TDSSCktElement::InitPropertyValues(NumPropsThisClass);
}


/*==============================================================================*/

int TSensorObj::LimitToPlusMinusOne(int i)
{
	int result = 0;
	if(i >= 0)
		result = 1;
	else
		result = -1;
	return result;
}

/*--------------------------------------------------------------------------*/

/* - function is not actually used
function TSensorObj.Get_FileName: String;
begin
        Result := GetOutputDirectory +  CircuitName_ + 'Sensor_' + Name + '.csv'
end;
*/

/*==============================================================================*/

void TSensorObj::Save()
{
}


/*==============================================================================*/
/*Interpret the Connection*/

void TSensorObj::Set_Conn(int Value)
{
	FConn = Value;
	RecalcVbase();
}

/*==============================================================================*/
/*Interpret Action Property*/

void TSensorObj::Set_Action(const String Value)
{
}

/*==============================================================================*/
  //WriteDLLDebugFile('Sensor');




}  // namespace Sensor





