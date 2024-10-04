
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
#include "ExcSexs.h"
#include <string>
#include "IntegrationMethods.h"


using namespace std;
//using namespace std;  // declared twice
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


namespace ExcSexs
{

	TExcSexsObj* ActiveExcSexsObj = nullptr;
	const int NumPropsThisClass = 8;		//	Number of properties in this class 
	

	void TExcSexs::DefineProperties()
	{
		NumProperties = NumPropsThisClass;
		CountProperties();   // Get inherited property count
		AllocatePropertyArrays();


		// Define Property names
		PropertyName[0] = "bus_name";
		PropertyName[1] = "gen_name";
		PropertyName[2] = "TaOverTb";
		PropertyName[3] = "Tb";
		PropertyName[4] = "K";
		PropertyName[5] = "Te";
		PropertyName[6] = "Emin";
		PropertyName[7] = "Emax";

		PropertyHelp[0] = "Bus name where the model is connected to";
		PropertyHelp[1] = "Generator name where model is connected to";
		PropertyHelp[2] = "Ratio between time constants Ta and Tb";
		PropertyHelp[3] = "Time constant Tb in seconds";
		PropertyHelp[4] = "Scaler value";
		PropertyHelp[5] = "Time constant Te in seconds";
		PropertyHelp[6] = "Per unit on EFD base";
		PropertyHelp[7] = "Per unit on EFD base";
		ActiveProperty = NumPropsThisClass - 1;
		inherited::DefineProperties();  // Add defs of inherited properties to bottom of list
	}

	int TExcSexs::MakeLike(const String ExcSexsName)
	{
		int result = 0;
		TExcSexsObj* OtherExcSexs = nullptr;
		int i = 0;
		result = 0;

		/*See if we can find this ExcSexsName in the present collection*/
		OtherExcSexs = ((TExcSexsObj*)Find(ExcSexsName));
		if (OtherExcSexs != nullptr)
		{
			auto thisobj = ActiveExcSexsObj;
			int stop = 0;
			thisobj->bus_name = OtherExcSexs->bus_name;

			for (i = 1; i <= thisobj->ParentClass->NumProperties; i++)
			{
				thisobj->Set_PropertyValue(i, OtherExcSexs->Get_PropertyValue(i));
			}
		}
		else
			DoSimpleMsg(String("Error in ExcSexs MakeLike: \"") + ExcSexsName
				+ "\" Not Found.", 9001);
		return result;
	}

	//	Constructor and Destructor
	TExcSexs::TExcSexs() {

		Class_Name = "ExcSexs";
		DSSClassType = DSSClassType + EXCSEXS_ELEMENT;
		DefineProperties();
		CommandList = TCommandList(Slice((PropertyName), NumProperties), NumProperties);
		CommandList.set_AbbrevAllowed(true);

	}

	TExcSexs::~TExcSexs() {

	}


	int TExcSexs::Edit(int ActorID) 
	{
		int result = 0;
		int ParamPointer = 0;
		String ParamName;
		String Param;
		int i = 0;
		int ret = -1;

		// continue parsing WITH contents of Parser
		ActiveExcSexsObj = (TExcSexsObj*)ElementList.Get_Active();
		ActiveCircuit[ActorID]->Set_ActiveCktElement(ActiveExcSexsObj);
		result = 0;

		{
			auto thisobj = ActiveExcSexsObj;
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
					thisobj->Set_PropertyValue(ParamPointer, Param);
				switch (ParamPointer)  // Ability to make this a search versus hardcoded order?
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
					thisobj->TaOverTb = Parser[ActorID]->MakeDouble_();
					break;
				case 	4:
					thisobj->Tb = Parser[ActorID]->MakeDouble_();
					break;
				case 	5:
					thisobj->K = Parser[ActorID]->MakeDouble_();
					break;
				case 	6:
					thisobj->Te = Parser[ActorID]->MakeDouble_();
					break;
				case 	7:
					thisobj->Emin = Parser[ActorID]->MakeDouble_();
					break;
				case 	8:
					thisobj->Emax = Parser[ActorID]->MakeDouble_();
					break;
				default:
					ClassEdit(ActiveExcSexsObj, ParamPointer - NumPropsThisClass);
					break;
				}

				ParamName = Parser[ActorID]->GetNextParam();
				Param = Parser[ActorID]->MakeString_();
			}
			//thisobj->RecalcElementData(ActorID);

		}

		return result;
	}

	int TExcSexs::NewObject(const String ObjName) {

		// Make a new ExcSexs model and add it to ExcSexs class list

		int result = 0;
		ActiveCircuit[ActiveActor]->Set_ActiveCktElement(new TExcSexsObj(this, ObjName));
		result = AddObjectToList(ActiveDSSObject[ActiveActor]);
		/*if(result)
			result = FindGenerator();
		*/
		return result;
	}


	// Find the generator then find generator model (as an extra step)????

	int TExcSexs::FindGenerator()
	{
		auto thisobj = ActiveExcSexsObj;		// For the active ExcSexs model, set a pointer to its generator model
		TDSSClass* GenClass = nullptr;
		TGeneratorObj* Gen = nullptr;
		GenClass = (TDSSClass*)GetDSSClassPtr("generator");
		Gen = (TGeneratorObj*)GenClass->Find(thisobj->gen_name);
		if (ASSIGNED(Gen) && ((TDSSCktElement*)Gen)->Get_Enabled()) {		// If generator is found and is enabled, then set pointer
			Gen->exc_model = thisobj;										// C++ will allow this just fine since thisobj is a derived class from T
			thisobj->pGen = Gen;											// Set this object generator pointer for easy access
			if (Gen->exc_model != nullptr)
				return 0;
		}
		else {
			// If it gets to this point, generator was either not found, or casting was incorrect, or was not online/enabled
			DoSimpleMsg(String("ExcSexs model \"") + thisobj->get_Name()
				+ "\" could not find generator \"" + thisobj->gen_name
				+ "\". Generator could be missing or not enabled \"", 9002);
			return -1;
		}
		return -1;
	}


	//--------------------------------------------------------------//
	//																//
	//					TExcSexsObj Functions						//
	//																//
	//--------------------------------------------------------------//
 

	// Default contstuctor for the ExcSexs object
	TExcSexsObj::TExcSexsObj()
	{
		nstates = 2;		// number of states contained in the model
		nchannels = 5;		// TBD by model developer
		
		// States
		ps.reserve(nstates);
		s = new double(nstates);
		ss = new double(nstates);
		ds = new double(nstates);
		dss = new double(nstates);
		
		// Parameters
		TaOverTb = 0.0;
		Tb = 0.0;
		K = 0.0;
		Te = 0.0;
		Emin = 0.0;
		Emax = 0.0;

		//	Variables
		efd=1.0;
		Y=1.0;
		U=1.0;
		Vs=1.0;
		Vref=1.0;
		Ec=1.0;
		
		// Others t
		pGen = nullptr;	// Pointer to generator associated with instance of the exicter
		FListSize = 0;	// ????????	

		ibase = 0;
		vbase = 0;
		mbase = 0;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Constructor
	TExcSexsObj::TExcSexsObj(DSSClass::TDSSClass* ParClass, const String ExcSexsName) :
		inherited(ParClass)
	{
		Set_Name(LowerCase(ExcSexsName));
		DSSObjType = ParClass->DSSClassType;
		cBuffer.clear();
		
		InitPropertyValues(0);


		//	Initialize members
		nstates = 2;		// number of states contained in the model
		nchannels = 5;		// Is this used? Value is selected based on?

		// States
		s = new double[nstates];
		ss = new double[nstates];
		ds = new double[nstates];
		dss = new double[nstates];

		// Parameters
		TaOverTb = 0.0;
		Tb = 0.0;
		K = 0.0;
		Te = 0.0;
		Emin = 0.0;
		Emax = 0.0;

		//	Variables
		efd=1.0;
		Y=1.0;
		U=1.0;
		Vs=1.0;
		Vref=1.0;
		Ec=1.0;

		// Others
		pGen = nullptr;
		FListSize = 0; // Needed ?
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Deconstructor, removes state information from the heap
	TExcSexsObj::~TExcSexsObj() {
		delete[] s;
		delete[] ss;
		delete[] ds;
		delete[] dss;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Specification of deafult parameters. Values here based on typical values listed
	// in PSSE PAG
	// The function is not called anywhere.....remove from code
	//void TExcSexsObj::LoadDefaultParameters() {
	//	TaOverTb = 0.1;
	//	Tb = 10.0;
	//	K = 100.0;
	//	Te = 0.1;
	//	Emin = 0.0;
	//	Emax = 3.0;
	//}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Initialization of the model states
	int TExcSexsObj::InitializeStates(int ActorID)
	{
		
		complex vterm;
		complex vtermpu;
		double vbase = (pGen->GenVars.kVGeneratorBase * 1000 / SQRT3);	//	Voltage base LL in Volts

		//	Check whether the "this->"is needed and where throughout this method
	
		// For validation
		stepCount = 0;


		if (pGen->GenON)
		{

			//Get field voltage (EDF) from the generator model 
			this->efd = this->pGen->gen_model->get_efield();
			
			// Initialize first order lag block
			this->s[1] = this->efd;
			this->ss[1] = this->s[1];
			this->ds[1] = 0.0;

			//Check for initialization of values beyond control limits 
			if (this->efd > Emax) {
				DoSimpleMsg(String("Initialization error of SEXS at generator \"") + this->gen_name
					+ "\". EFD > Emax \"", 9005);	// meaning behind the error number??
				return -1; // return negative value to main function call to stop initialization process?
			}else if (this->efd < Emin){
				DoSimpleMsg(String("Initialization error of SEXS at generator \"") + this->gen_name
					+ "\". EFD < Emin \"", 9005);	// meaning behind the error number??
				return -1;
			}else{}

			Y = this->s[1] / K;

			// Initialize Lead-lag block 
			this->s[0] = (1 - TaOverTb) * Y;
			this->ss[0] = this->s[0];
			this->ds[0] = 0.0;
			
			// U = Y,  no need to compute in the initialization
						
			//Compute the terminal voltage in per-unit
			vterm = pGen->GetVterminal(ActorID);		// Positive sequence voltage - LN
			vtermpu = cdivreal(vterm, vbase);


			// Assume no regulator or stabilizer
			Ec = cabs(vtermpu);		//Per-unit terminal voltage magnitude
			Vs = 0;					// assume no stabilizers initial
			this->Vref = Y + Ec - Vs;


		}
		else {
			DoSimpleMsg(Format(("Generator " + pGen->get_Name() + " is offline.").c_str()), 9004);
			return -1;
		}

		return 0;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	// Update state derivatives at each timestep
	// ---- Intentionally did not use the explicit this-> in the specifications to determine whether required.
	int TExcSexsObj::CalculateRate(int ActorID)
	{
		complex vterm;
		complex vtermpu;
		double vbase = (pGen->GenVars.kVGeneratorBase * 1000 / SQRT3);	//	Voltage base LL in Volts

		// Hard Coding for Validation //
		int trigger = 1000;
		int trigger2 = 11000;
		double Vref_inc = 0.1;
		stepCount = stepCount + 1;
		if (stepCount == trigger) {
			//this->Vref = 1.06;// Case 6
			//this->Vref = 2.00;// Case 7
			//this->Vref = 0.98;// Case 8
			//this->Vref = -2.0;// Case 9 and 10
		}
		if (stepCount == trigger2) {
			//this->Vref = 2.00;// Case 10
		}

		//Compute the terminal voltage in per-unit
		vterm = pGen->GetVterminal(ActorID);		// Positive sequence voltage - LN
		vtermpu = cdivreal(vterm, vbase);
		Ec = cabs(vtermpu);
		U = this->Vref - Ec + Vs; // confirm Vs remains at zero

		Y = s[0] + TaOverTb * U;
		ds[0] = (U - Y) / Tb;


		if (s[1] > Emax) {
			s[1] = Emax;
			ds[1] = (Y * K - s[1]) / Te;
			if (ds[1] > 0) {
				ds[1] = 0;
			}
		}
		else if (s[1] < Emin) {
			s[1] = Emin;
			ds[1] = (Y * K - s[1]) / Te;
			if (ds[1] < 0) {
				ds[1] = 0;
			}
		}
		else { 
			ds[1] = (Y * K - s[1]) / Te; 
		}

		this->efd = s[1];
		return 0;
	}


	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	int TExcSexsObj::StateIntegration(int ActorID)
	{
		//	Call global integration method of choice

		double h = ActiveCircuit[ActorID]->Solution->DynaVars.h;

		//	Here we are calling Adams-Bashforth second order.  
		//	Other methods can be implemented and each model can call a specific method.  
		//	That is the beauty of this modular implementation
		// IntegrationMethods::trapezoidal<TExcSexsObj>(this, &h);
		IntegrationMethods::ab2order<TExcSexsObj>(this, &h);

		return 0;
	}	

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	int TExcSexsObj::StateIntegration_correction(int ActorID)
	{
		//	Call global integration method of choice

		double h = ActiveCircuit[ActorID]->Solution->DynaVars.h;

		//	HEre we are calling Adams-Bashforth second order.  
		//	Other methods can be implemented and each model can call a specific method.  
		//	That is the beauty of this modular implementation
		// IntegrationMethods::trapezoidal<TExcSexsObj>(this, &h);
		IntegrationMethods::euler<TExcSexsObj>(this, &h);
		// IntegrationMethods::ab2order<TExcSexsObj>(this, &h);

		//this->nstates;
		return 0;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	//Provide variables to monitor 
	double TExcSexsObj::get_channel(int i)

	{
		double result = 0.0;
		int n = 0;
		int k = 0;
		n = 0;
		result = -9999.99;  // error return value
		if (i < 1)
			return result;  // Someone goofed
		switch (i) {
		case 	1:
			result = this->efd;		//	Efd
			break;
		case 	2:
			result = this->s[0];		//	lead-lag transfer function state
			break;
		case 	3:
			result = this->Vref;		//	Vref
			break;
		case 	4:
			result = this->Ec;			//	Ec
			break;
		case 	5:
			result = this->Vs;			//	Vs
			break;
		default:
			break;
		}
		return result;
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	//Provides headers for the monitor output file 
	String TExcSexsObj::get_channel_header(int i)
	{
		String result;
		switch (i) {
		case 	1:
			result = " Efd(pu)";
			break;
		case 	2:
			result = " lead-lag-state";
			break;
		case 	3:
			result = " Vref(pu)";
			break;
		case 	4:
			result = " Ec(pu)";
			break;
		case 	5:
			result = " Vs(pu)";
			break;
		default:
			break;
		}
		return result;
	}

	int64_t TExcSexsObj::get_number_channels()
	{
		return nchannels;
	}



	void TExcSexsObj::sample(int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	void TExcSexsObj::DoPendingAction(int Code, int ProxyHdl, int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	void TExcSexsObj::Reset(int ActorID) {
		// Empty for now, but need to see if anything needs to be added here
	}

	void TExcSexsObj::InitPropertyValues(int ArrayOffset) {
		Set_PropertyValue(1,"");		// bus_name;
		Set_PropertyValue(2,"");		// gen_name;
		Set_PropertyValue(3,"0.1");		// TaOverTb;
		Set_PropertyValue(4,"10.0");	// Tb
		Set_PropertyValue(5,"100.0");	// K;
		Set_PropertyValue(6,"0.1");		// Te;
		Set_PropertyValue(7,"0.0");		// Emin;
		Set_PropertyValue(8,"3.0");		// Emax;
		inherited::InitPropertyValues(NumPropsThisClass);
	}

	//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	//Provides calculated efield, primarily when requested from gen_model 
	//during the dynamic simulation 
	double TExcSexsObj::get_efield()
	{
		return this->efd;
	}


	/*
	void TExcSexsObj::DumpProperties(System::TTextRec& f, bool Complete) {

	}
	*/
}