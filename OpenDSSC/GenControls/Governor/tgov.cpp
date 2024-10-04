
#include "System.h"
#include "Sysutils.h"
#include "fstream"

#include "Command.h"
#include "ControlClass.h"
#include "ControlElem.h"
#include "CktElement.h"
#include "DSSClass.h"
#include "DSSGlobals.h"
#include "Arraydef.h"
#include "Ucomplex.h"
#include "PointerList.h"
#include "d2c_structures.h"
//#include "gencls.h" // May not need this
//#include "genrou.h"
#include "tgov.h"
#include <string>
#include "IntegrationMethods.h"

using namespace std;
using namespace Bus;
using namespace Circuit;
using namespace CktElement;
using namespace Command;
using namespace ControlClass;
using namespace ControlElem;
using namespace DSSClass;
using namespace DSSClassDefs;
using namespace DSSGlobals;
using namespace DSSObject;
using namespace ParserDel;
using namespace System;
using namespace Ucmatrix;
using namespace Ucomplex;
using namespace mathutil;
using namespace Utilities;


namespace Tgov
{

	TTgovObj* ActiveTgovObj = nullptr;      
	const int NumPropsThisClass = 9;		// Number of properties in this class

	void TTgov::DefineProperties()
	{
		NumProperties = NumPropsThisClass; // Parameter comes from DSSClass:TDSSClass
		CountProperties();   // Adds one to the NumProperties variable within DSSClass.cpp
		AllocatePropertyArrays(); // Adds one more column to property arrays (PropertyName, PropertyHelp, PropertyIdxMap) From DSSClass.cpp


		// Define Property names
		//PropertyName[0] = "bus_name";
		PropertyName[0] = "bus_name";
		PropertyName[1] = "gen_name";
		PropertyName[2] = "R";
		PropertyName[3] = "Dt";
		PropertyName[4] = "T1";
		PropertyName[5] = "T2";
		PropertyName[6] = "T3";
		PropertyName[7] = "Vmax";
		PropertyName[8] = "Vmin";

		PropertyHelp[0] = "Bus name where the generator model is connected to";
		PropertyHelp[1] = "Generator name where the model is connected to";
		PropertyHelp[2] = "Turbine governor droop (pu)";
		PropertyHelp[3] = "Turbine damping coefficient (pu)";
		PropertyHelp[4] = "Time constant for steam control valve (T1>0) (sec)";
		PropertyHelp[5] = "Lead/Lag Time Constant (sec)";
		PropertyHelp[6] = "Time constant for reheat (T3>0) (sec)";
		PropertyHelp[7] = "Maximum valve position (pu)";
		PropertyHelp[8] = "Minimum valve position (pu)";
	

		ActiveProperty = NumPropsThisClass - 1;
		inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
	}

	int TTgov::MakeLike(const String TgovName) 
	{
		int result = 0;
		TTgovObj* OtherTgov = nullptr;
		int i = 0;
		result = 0;
		
		/*See if we can find this TgovName in the present collection*/
		OtherTgov = ((TTgovObj*)Find(TgovName));
		if (OtherTgov != nullptr)
		{
			auto thisobj = ActiveTgovObj;
			int stop = 0;
			thisobj->bus_name = OtherTgov->bus_name; 

			for (i = 1; i <= thisobj->ParentClass->NumProperties; i++)
			{
				thisobj->Set_PropertyValue(i,OtherTgov->Get_PropertyValue(i));
			}
		}
		else
			DoSimpleMsg(String("Error in Tgov MakeLike: \"") + TgovName
				+ "\" Not Found.", 9001);
		return result;
	}

	//	Constructor //
	TTgov::TTgov() 
	{
		Class_Name = "Tgov";
		DSSClassType = DSSClassType + TGOV_ELEMENT; //Linked to DSSClassDefs.h (using 47)
		DefineProperties();
		CommandList = TCommandList(Slice((PropertyName), NumProperties), NumProperties);
		CommandList.set_AbbrevAllowed(true);
	}

	// Destructor //
	TTgov::~TTgov() 
	{
		// ENTER CODE HERE //
	}

	// Parser //
	int TTgov::Edit(int ActorID)
	{
		int result = 0;
		int ParamPointer = 0;
		String ParamName;
		String Param;
		int i = 0;
		int ret = -1;

		// Continue parsing WITH contents of Parser
		ActiveTgovObj = (TTgovObj*)ElementList.Get_Active();
		ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveTgovObj);
		result = 0;

		{
			auto thisobj = ActiveTgovObj;
			ParamPointer = 0;
			ParamName = Parser[ActorID]->GetNextParam();
			Param = Parser[ActorID]->MakeString_();
			while (Param.size() > 0)
			{
				if (ParamName.size() == 0)
					++ParamPointer;
				else
					ParamPointer = CommandList.Getcommand(ParamName);
				if ((ParamPointer > 0) && (ParamPointer <= NumProperties))
					thisobj->Set_PropertyValue(ParamPointer,Param);
				switch (ParamPointer)
				{
				case 	0:
					DoSimpleMsg(String("Unknown parameter \"") + ParamName
						+ "\" for Object \""
						+ Class_Name
						+ "."
						+ thisobj->get_Name()
						+ "\"", 9000);
					break;
				case 	1:
					thisobj->bus_name = Parser[ActorID]->MakeString_();
					break;
				case 	2:
					thisobj->gen_name = Parser[ActorID]->MakeString_();
					// Try to find generator in generator list
					ret = FindGenerator();
					break;
				case 	3:
					thisobj->R = Parser[ActorID]->MakeDouble_();
					break;
				case 	4:
					thisobj->Dt = Parser[ActorID]->MakeDouble_();
					break;
				case 	5:
					thisobj->T1 = Parser[ActorID]->MakeDouble_();
					break;
				case 	6:
					thisobj->T2 = Parser[ActorID]->MakeDouble_();
					break;
				case 	7:
					thisobj->T3 = Parser[ActorID]->MakeDouble_();
					break;
				case 	8:
					thisobj->Vmax = Parser[ActorID]->MakeDouble_();
					break;
				case 	9:
					thisobj->Vmin = Parser[ActorID]->MakeDouble_();
					break;
				default:
					ClassEdit(ActiveTgovObj, ParamPointer - NumPropsThisClass);
					break;
				}

				ParamName = Parser[ActorID]->GetNextParam();
				Param = Parser[ActorID]->MakeString_();
			}
			//thisobj->RecalcElementData(ActorID);
		}
		return result;
	}

	// Make New Tgov model and add to Tgov class list // 
	int TTgov::NewObject(const String ObjName) 
	{
		int result = 0;
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TTgovObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
		/*if(result)
			result = FindGenerator();
		*/
		return result;
	}

	int TTgov::FindGenerator()
	{
		auto thisobj = ActiveTgovObj;		// For the active Tgov model, set a pointer to its generator model
		TDSSClass* GenClass = nullptr;		// Create a Class called GenClass
		TGeneratorObj* Gen = nullptr;		// Create a Generator Object called Gen
		GenClass = (TDSSClass*)GetDSSClassPtr("generator");
		Gen = (TGeneratorObj*)GenClass->Find(thisobj->gen_name);
		if (ASSIGNED(Gen) && ((TDSSCktElement*)Gen)->Get_Enabled()) {		// If generator is found and is enabled, then set pointer
			Gen->turb_model = thisobj;										// C++ will allow this just fine since thisobj is a derived class from T
			thisobj->pGen = Gen;											// Set this object generator pointer for easy access
			if (Gen->turb_model != nullptr)
				return 0;
		}
		else {
			// If it gets to this point, generator was either not found, or casting was incorrect, or was not online/enabled
			DoSimpleMsg(String("Tgov model \"") + thisobj->get_Name()
				+ "\" could not find generator \"" + thisobj->gen_name
				+ "\". Generator could be missing or not enabled \"", 9002);
			return -1;
		}
		return -1;
	}

	// Determine if we need to determine that a generator model with the input PMech exists.


	//--------------------------------------------------------------//
	//																//
	//					TTgovObj Functions						//
	//																//
	//--------------------------------------------------------------//


	// THIS CONSTRUCTOR IS USED ONLY DURING TESTING
	//TTgovObj::TTgovObj() :
	//	ibase(1.0),
	//	mbase(1.0),
	//	vbase(1.0) //,

	//{
	//	nstates = 2;   // X1 and X2
	//	nchannels = 6; // WHAT IS THIS
	//	// States
	//	ps.reserve(nstates); // Is this for allocating memory
	//	s = new double(nstates);
	//	ss = new double(nstates);
	//	ds = new double(nstates);
	//	dss = new double(nstates);
	//	// Parameters
	//	Pmref = 0.0;
	//	R = 0.0;
	//	Dt = 0.0;
	//	T1 = 0.0;
	//	T2 = 0.0;
	//	T3 = 0.0;
	//	Vmin = 0.0;
	//	Vmax = 0.0;
	//	//	Variables
	//	pmech = 0.;
	//	dspd = 0.0;
	//	// Others
	//	pGen = nullptr; 
	//	FListSize = 0;
	//}

	TTgovObj::TTgovObj(DSSClass::TDSSClass* ParClass, const String TgovName) :
		inherited(ParClass)
	{
		Set_Name(LowerCase(TgovName));
		DSSObjType = ParClass->DSSClassType;
		cBuffer.clear();

		InitPropertyValues(0);

		//	Initialize members
		nstates = 2; 
		nchannels = 5; // This is the number of channels which will be monitored for Tgov
		
		// States
		s = new double[nstates];
		ss = new double[nstates];
		ds = new double[nstates];
		dss = new double[nstates];
		
		// Parameters
		R = 0.0;
		Dt = 0.0;
		T1 = 0.0;
		T2 = 0.0;
		T3 = 0.0;
		Vmin = 0.0;
		Vmax = 0.0;
		
		//	Variables
		pmech = 0.;
		dspd = 0.0;
		Y = 1.0;
		U = 1.0;
		
		// Others
		pGen = nullptr; 
		//FListSize = 0;
	}

	//------------------------------------------------------------
	// Deconstructor //
	TTgovObj::~TTgovObj() 
	{
		delete[] s;
		delete[] ss;
		delete[] ds;
		delete[] dss;
	}

	//------------------------------------------------------------
	// Default Parameters // 
	void TTgovObj::LoadDefaultParameters() 
	{
		// These are the default parameters for this model
		R = 0.05;
		Dt = 0.0;
		T1 = 0.5;
		T2 = 3.0;
		T3 = 10.0;
		Vmin = 0.0;
		Vmax = 1.0;
	}

	//------------------------------------------------------------
	// Initialization of the model states //
	int TTgovObj::InitializeStates(int ActorID)
	{
		// Get generator terminal current and terminal voltage

		/*VNeut,*/
		int i = 0;											

		//	Get voltage and MVA bases for generator.  All generator variables and parameters will be in pu and these are the bases used to 
		double mbase = pGen->GenVars.kVArating * 1000;					//	MVA base of generator, in VA

		/*Compute nominal Positive sequence voltage behind transient reactance*/
		if (pGen->GenON)
		{
			// Get pmech and dspd from generator model
			this->pmech = this->pGen->gen_model->get_pmech();
			this->dspd = this->pGen->gen_model->get_dspd();

			

			//calculate staste 1 before anti-windup
			Y = this->pmech + this->dspd * Dt;

			// Make sure the Y value is within limits
			if (Y > Vmax)
			{
				Y = Vmax;
				DoSimpleMsg(string("Vmax exceeded during intitialization of generator") + gen_name, 9005);
			}
			else if (Y < Vmin)
			{
				Y = Vmin;
				DoSimpleMsg(string("Vmin exceeded during intitialization of generator") + gen_name, 9005);
			}

			// Initialize first order lead/lag block
			this->s[1] = Y * (1 - T2 / T3);
			this->ss[1] = s[1];
			ds[1] = 0;


			// Initialize first order low-pass filter
			this->s[0] = Y;
			this->ss[0] = s[0];
			this->ds[0] = 0.0;

			// Caluculate pmref
			this->pmref = R * this->pmech + this->dspd * (1 + R * Dt);
			this->pmref = this->pmref;
		}
		else {
			DoSimpleMsg(Format(("Generator " + pGen->get_Name() + " is offline.").c_str()), 9004);
			return -1;
		}

		//int ret  = call_iterative_solver();

		return 0;
	}

	//------------------------------------------------------------
	// Calculate of the model states //
	int TTgovObj::CalculateRate(int ActorID)
	{
		// Hard Coding for Validation //
		int trigger = 1000;
		int trigger2 = 11000;
		stepCount = stepCount + 1;
		if (stepCount == trigger) {
			//this->pmref = 0.05;// Case 6
			//this->pmref = 0.10;// Case 7
			//this->pmref = 0.04;// Case 8
			//this->pmref = -0.1;// Case 9 and 10
		}
		if (stepCount == trigger2) {
			//this->pmref = 0.10;// Case 10
		}



		// Get the speed deviation of the generator model
		this->dspd = this->pGen->gen_model->get_dspd();

		U = (this -> pmref - this->dspd) / R;
		// Anti-Windup
		if (s[0] > Vmax)
		{
			s[0] = Vmax;
			ds[0] = (U - s[0]) / T1;
			if (ds[0] > 0)
			{
				ds[0] = 0;
			}
		}
		else if (s[0] < Vmin)
		{
			s[0] = Vmin;
			ds[0] = (U - s[0]) / T1;
			if (ds[0] < 0)
			{
				ds[0] = 0;
			}
		}
		else
		{
			ds[0] = (U - s[0]) / T1;
		}

		Y = s[1] + s[0]*(T2 / T3);
		ds[1] = (s[0] -Y ) / T3;
		this->pmech = Y + this->dspd * Dt;

		return 0;
	}

	//------------------------------------------------------------
	// Integration Method //
	int TTgovObj::StateIntegration(int ActorID)
	{
		////	Call global integration method of choice

		double h = ActiveCircuit[ActorID]->Solution->DynaVars.h;

		////	HEre we are calling Adams-Bashforth second order.  
		////	Other methods can be implemented and each model can call a specific method.  
		////	That is the beauty of this modular implementation
		//// IntegrationMethods::trapezoidal<TGenrouObj>(this, &h);
		IntegrationMethods::ab2order<TTgovObj>(this, &h);
		//// IntegrationMethods::euler<TGenrouObj>(this, &h);

		////this->nstates;
		return 0;
	}

	//------------------------------------------------------------
	// Integration Correction //
	int TTgovObj::StateIntegration_correction(int ActorID)
	{
		////	Call global integration method of choice

		double h = ActiveCircuit[ActorID]->Solution->DynaVars.h;

		////	HEre we are calling Adams-Bashforth second order.  
		////	Other methods can be implemented and each model can call a specific method.  
		////	That is the beauty of this modular implementation
		// IntegrationMethods::trapezoidal<TTgovObj>(this, &h);
		IntegrationMethods::euler<TTgovObj>(this, &h);
		//// IntegrationMethods::ab2order<TTgovObj>(this, &h);

		////this->nstates;
		return 0;
	}

	
	//------------------------------------------------------------
	// Send channels to monitor //
	double TTgovObj::get_channel(int i)
	{
		double result = 0.0;
		int n = 0;
		int k = 0;
		n = 0;
		result = -9999.99;				// error return value
		if (i < 1)
			return result;				// Someone goofed
		switch (i) {
		case 	1:
			result = this->pmech;		//	Mechanical Power (pu)
			break;
		case 	2:
			result = this->s[1];		//	first order lead-lag state
			break;
		case 	3:
			result = this->s[0];		//	first order low pass filter state
			break;
		case 	4:
			result = this->dspd;		//	Speed Deviation
			break;
		case	5:
			result = this->pmref;
			break;
		default:
			break;
		}
		return result;
	}

	//------------------------------------------------------------
	// Provide headers for channels to monitor //
	String TTgovObj::get_channel_header(int i)
	{
		String result;
		switch (i) {
		case 	1:
			result = " GOV_Pmech(pu)";
			break;
		case 	2:
			result = " GOV_lead-lag-state";
			break;
		case 	3:
			result = " GOV_lowpass-state";
			break;
		case 	4:
			result = " GOV_speed_deviation";
			break;
		case	5:
			result = " Reference Variable";
			break;
		default:
			break;
		}
		return result;
	}

	int64_t TTgovObj::get_number_channels()
	{
		return nchannels;
	}

	void TTgovObj::sample(int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	void TTgovObj::DoPendingAction(int Code, int ProxyHdl, int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	void TTgovObj::Reset(int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	void TTgovObj::InitPropertyValues(int ArrayOffset) {
		Set_PropertyValue(1,"");		// bus_name
		Set_PropertyValue(2,"");		// gen_name
		Set_PropertyValue(3,"0.05");	// R
		Set_PropertyValue(4,"0.0");		// Dt
		Set_PropertyValue(5,"0.5");		// T1
		Set_PropertyValue(6,"3.0");		// T2
		Set_PropertyValue(7,"10.0");	// T3
		Set_PropertyValue(8,"1.0");		// Vmax
		Set_PropertyValue(9,"0.0");		// Vmin
		inherited::InitPropertyValues(NumPropsThisClass);
	}

	// Provides calculated pmech when requested from gen_model
	double TTgovObj::get_pmech()
	{
		return this->pmech;
	}
	

	/*	void TTgovObj::DumpProperties(System::TTextRec& f, bool Complete) {

	}*/
}